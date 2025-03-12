use std::hash::{DefaultHasher, Hash, Hasher};

use super::context::{Context, Draft, PreContext, ResourceRef};
use super::schema::Schema;
use super::RetrieveWrapper;
use crate::{HashMap, HashSet};

use anyhow::{anyhow, bail, Result};
use serde_json::{Map, Number, Value};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Keyword<'a> {
    InPlaceApplicator(InPlaceApplicator<'a>),
    SubInstanceApplicator(SubInstanceApplicator<'a>),
    Assertion(Assertion<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InPlaceApplicator<'a> {
    // In-place
    AllOf(&'a Vec<Value>),
    AnyOf(&'a Vec<Value>),
    OneOf(&'a Vec<Value>),
    Ref(String),
    Const(&'a Value),
    Enum(&'a Vec<Value>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum SubInstanceApplicator<'a> {
    // Object
    Properties(&'a Map<String, Value>),
    AdditionalProperties {
        applicator: &'a Value,
        properties: Option<&'a Map<String, Value>>,
    },
    // Array
    PrefixItems(&'a Vec<Value>),
    Items {
        applicator: &'a Value,
        prefix_items: Option<&'a Vec<Value>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Assertion<'a> {
    // Core
    Type(&'a Value),
    // Number
    Minimum(&'a Number),
    Maximum(&'a Number),
    ExclusiveMinimum(&'a Number),
    ExclusiveMaximum(&'a Number),
    MultipleOf(&'a Number),
    // String
    MinLength(&'a Number),
    MaxLength(&'a Number),
    Pattern(String),
    Format(String),
    // Array
    MinItems(&'a Number),
    MaxItems(&'a Number),
    // Object
    Required(&'a Vec<Value>),
}

pub fn build_schema(
    contents: Value,
    retriever: Option<RetrieveWrapper>,
) -> Result<(Schema, HashMap<String, Schema>)> {
    if let Some(b) = contents.as_bool() {
        if b {
            return Ok((Schema::Any, HashMap::default()));
        } else {
            return Ok((Schema::false_schema(), HashMap::default()));
        }
    }

    let pre_ctx = PreContext::new(contents, retriever)?;
    let ctx = Context::new(&pre_ctx)?;

    let root_resource = ctx.lookup_resource(&pre_ctx.base_uri)?;
    let pre_schema = PreSchema::new(&ctx).with_resource(root_resource)?;
    let schema = pre_schema.compile()?;
    Ok((schema, ctx.take_defs()))
}

#[derive(Clone)]
struct PreSchema<'ctx> {
    ctx: &'ctx Context<'ctx>,
    inner: PreSchemaInner<'ctx>,
    seen_refs: HashSet<String>,
}
impl<'ctx> PreSchema<'ctx> {
    fn new(ctx: &'ctx Context) -> Self {
        PreSchema {
            ctx,
            inner: PreSchemaInner::Any,
            seen_refs: HashSet::default(),
        }
    }

    fn with_resource(mut self, resource: ResourceRef) -> Result<Self> {
        let schema = resource.contents();
        if let Some(b) = schema.as_bool() {
            if !b {
                self.inner = PreSchemaInner::False;
            }
            return Ok(self);
        }
        let schema = schema
            .as_object()
            .ok_or_else(|| anyhow!("schema must be an object or boolean"))?;
        let inner_ctx = self.ctx.in_subresource(resource)?;
        let keywords = get_keywords(&inner_ctx, schema)?;
        for kwd in keywords {
            self = self.with_keyword(kwd)?;
        }
        Ok(self)
    }

    fn with_keyword(mut self, kwd: Keyword) -> Result<Self> {
        match kwd {
            Keyword::Assertion(assertion) => {
                self.inner.assert(assertion);
                Ok(self)
            }
            Keyword::SubInstanceApplicator(applicator) => {
                self.inner.apply_to_subinstance(applicator);
                Ok(self)
            }
            Keyword::InPlaceApplicator(applicator) => match applicator {
                InPlaceApplicator::Ref(uri) => {
                    if self.seen_refs.contains(&uri) {
                        return Ok(self);
                    }
                    self.seen_refs.insert(uri.clone());
                    let resource = self.ctx.lookup_resource(&uri)?;
                    self.with_resource(resource)
                }
                InPlaceApplicator::AllOf(schemas) => {
                    for schema in schemas.iter() {
                        let resource = self.ctx.as_resource_ref(schema);
                        self = self.with_resource(resource)?;
                    }
                    Ok(self)
                }
                InPlaceApplicator::AnyOf(schemas) => {
                    if schemas.is_empty() {
                        self.inner = PreSchemaInner::False;
                        return Ok(self);
                    }
                    self.inner = PreSchemaInner::AnyOf(
                        schemas
                            .iter()
                            .map(|schema| {
                                let resource = self.ctx.as_resource_ref(schema);
                                self.clone().with_resource(resource)
                            })
                            .collect::<Result<Vec<_>>>()?,
                    );
                    Ok(self)
                }
                InPlaceApplicator::OneOf(schemas) => {
                    if schemas.is_empty() {
                        self.inner = PreSchemaInner::False;
                        return Ok(self);
                    }
                    self.inner = PreSchemaInner::OneOf(
                        schemas
                            .iter()
                            .map(|schema| {
                                let resource = self.ctx.as_resource_ref(schema);
                                self.clone().with_resource(resource)
                            })
                            .collect::<Result<Vec<_>>>()?,
                    );
                    Ok(self)
                }
                InPlaceApplicator::Const(v) => todo!(),
                InPlaceApplicator::Enum(v) => todo!(),
            },
        }
    }

    fn compile(self) -> Result<Schema> {
        match self.inner {
            PreSchemaInner::Any => Ok(Schema::Any),
            PreSchemaInner::False => Ok(Schema::false_schema()),
            PreSchemaInner::AnyOf(schemas) => {
                let schemas = schemas
                    .into_iter()
                    .map(|schema| schema.compile())
                    .collect::<Result<Vec<_>>>()?;
                Ok(Schema::AnyOf { options: schemas })
            }
            PreSchemaInner::OneOf(schemas) => {
                let schemas = schemas
                    .into_iter()
                    .map(|schema| schema.compile())
                    .collect::<Result<Vec<_>>>()?;
                Ok(Schema::OneOf { options: schemas })
            }
            PreSchemaInner::Simple(schema) => {
                // Build a fake URI using the hash of the schema
                // in order to avoid infinite recursion
                let mut hash = DefaultHasher::new();
                schema.hash(&mut hash);
                let hash = hash.finish();
                let uri = self.ctx.normalize_ref(&format!("#{}", hash))?;
                if !self.ctx.been_seen(&uri) {
                    self.ctx.mark_seen(&uri);
                    let compiled = schema.compile()?;
                    self.ctx.insert_ref(&uri, compiled.clone());
                }
                Ok(Schema::Ref { uri })
            }
        }
    }
}

#[derive(Clone)]
enum PreSchemaInner<'a> {
    Any,
    False,
    Simple(SimpleSchema<'a>),
    AnyOf(Vec<PreSchema<'a>>),
    OneOf(Vec<PreSchema<'a>>),
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct SimpleSchema<'a> {
    null: bool,
    boolean: bool,
    number: bool,
    integer: bool,
    string: bool,
    array: bool,
    object: bool,
    minimum: Option<Number>,
    maximum: Option<Number>,
    exclusive_minimum: Option<Number>,
    exclusive_maximum: Option<Number>,
    multiple_of: Option<Number>,
    min_length: Option<Number>,
    max_length: Option<Number>,
    pattern: Option<String>,
    format: Option<String>,
    min_items: Option<Number>,
    max_items: Option<Number>,
    required: Option<Vec<String>>,
    sub_instance_applicators: Vec<SubInstanceApplicator<'a>>,
}

impl SimpleSchema<'_> {
    fn compile(self) -> Result<Schema> {
        let mut options = Vec::new();
        if self.null {
            options.push(Schema::Null);
        }
        if self.boolean {
            // todo -- literal boolean
            options.push(Schema::Boolean);
        }
        if self.number || self.integer {
            todo!()
            // options.push(Schema::Number {
            //     minimum: self.minimum,
            //     maximum: self.maximum,
            //     exclusive_minimum: self.exclusive_minimum,
            //     exclusive_maximum: self.exclusive_maximum,
            //     multiple_of: self.multiple_of,
            //     integer: self.integer,
            // });
        }
        if self.string {
            todo!()
            // options.push(Schema::String {
            //     min_length: self.min_length,
            //     max_length: self.max_length,
            //     pattern: self.pattern,
            // });
        }
        if self.array {
            todo!()
        }
        if self.object {
            todo!()
        }
        if options.is_empty() {
            Ok(Schema::false_schema())
        } else if options.len() == 1 {
            Ok(options.into_iter().next().unwrap())
        } else {
            Ok(Schema::AnyOf { options })
        }
    }
}

impl PreSchemaInner<'_> {
    fn assert(&mut self, assertion: Assertion) {
        todo!()
    }

    fn apply_to_subinstance(&mut self, applicator: SubInstanceApplicator) {
        todo!()
    }
}

fn get_keywords<'a>(ctx: &Context, schema: &'a Map<String, Value>) -> Result<Vec<Keyword<'a>>> {
    let mut keywords = Vec::new();
    for (k, v) in schema.iter() {
        match k.as_str() {
            "type" => {
                keywords.push(Keyword::Assertion(Assertion::Type(v)));
            }
            "allOf" => {
                if let Some(v) = v.as_array() {
                    keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::AllOf(v)));
                } else {
                    bail!("allOf must be an array");
                }
            }
            "anyOf" => {
                if let Some(v) = v.as_array() {
                    keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::AnyOf(v)));
                } else {
                    bail!("anyOf must be an array");
                }
            }
            "oneOf" => {
                if let Some(v) = v.as_array() {
                    keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::OneOf(v)));
                } else {
                    bail!("oneOf must be an array");
                }
            }
            "ref" => {
                if let Some(v) = v.as_str() {
                    keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::Ref(
                        ctx.normalize_ref(v)?,
                    )));
                } else {
                    bail!("$ref must be a string");
                }
            }
            "const" => {
                keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::Const(v)));
            }
            "enum" => {
                if let Some(v) = v.as_array() {
                    keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::Enum(v)));
                } else {
                    bail!("enum must be an array");
                }
            }
            "properties" => {
                if let Some(v) = v.as_object() {
                    keywords.push(Keyword::SubInstanceApplicator(
                        SubInstanceApplicator::Properties(v),
                    ));
                } else {
                    bail!("properties must be an object");
                }
            }
            "additionalProperties" => {
                let properties = schema
                    .get("properties")
                    .map(|v| {
                        if let Some(v) = v.as_object() {
                            Ok(v)
                        } else {
                            Err(anyhow!("properties must be an object"))
                        }
                    })
                    .transpose()?;
                keywords.push(Keyword::SubInstanceApplicator(
                    SubInstanceApplicator::AdditionalProperties {
                        applicator: v,
                        properties,
                    },
                ));
            }
            "items" | "prefixItems" | "additionalItems" => {
                todo!("handle old draft semantics...")
            }
            "minItems" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MinItems(v)));
                } else {
                    bail!("minItems must be a number");
                }
            }
            "maxItems" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MaxItems(v)));
                } else {
                    bail!("maxItems must be a number");
                }
            }
            "minLength" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MinLength(v)));
                } else {
                    bail!("minLength must be a number");
                }
            }
            "maxLength" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MaxLength(v)));
                } else {
                    bail!("maxLength must be a number");
                }
            }
            "pattern" => {
                if let Some(v) = v.as_str() {
                    keywords.push(Keyword::Assertion(Assertion::Pattern(v.to_string())));
                } else {
                    bail!("pattern must be a string");
                }
            }
            "format" => {
                if let Some(v) = v.as_str() {
                    keywords.push(Keyword::Assertion(Assertion::Format(v.to_string())));
                } else {
                    bail!("format must be a string");
                }
            }
            "minimum" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::Minimum(v)));
                } else {
                    bail!("minimum must be a number");
                }
            }
            "maximum" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::Maximum(v)));
                } else {
                    bail!("maximum must be a number");
                }
            }
            "exclusiveMinimum" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::ExclusiveMinimum(v)));
                } else {
                    todo!("old draft booleans...")
                }
            }
            "exclusiveMaximum" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::ExclusiveMaximum(v)));
                } else {
                    todo!("old draft booleans...")
                }
            }
            "multipleOf" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MultipleOf(v)));
                } else {
                    bail!("multipleOf must be a number");
                }
            }
            "required" => {
                if let Some(v) = v.as_array() {
                    keywords.push(Keyword::Assertion(Assertion::Required(v)));
                } else {
                    bail!("required must be an array");
                }
            }
            "$anchor" | "$defs" | "definitions" | "$schema" | "$id" | "id" | "$comment"
            | "title" | "description" | "default" | "readOnly" | "writeOnly" | "examples"
            | "contentMediaType" | "contentEncoding" => {
                // Ignore these keywords -- they are annotations and metadata
            }
            _ => todo!(
                "Check if keyword is recognized using context. Bail if it is, ignore if it's not."
            ),
        }
    }
    Ok(keywords)
}
