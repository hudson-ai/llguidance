use std::any;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::{BitAndAssign, BitOrAssign};

use super::context::{Context, Draft, PreContext, ResourceRef};
use super::formats::lookup_format;
use super::numeric::Decimal;
use super::schema::Schema;
use super::RetrieveWrapper;
use crate::{HashMap, HashSet};

use anyhow::{anyhow, bail, Result};
use derivre::RegexAst;
use indexmap::{IndexMap, IndexSet};
use serde_json::{Map, Number, Value};

#[derive(Clone, Debug)]
enum Keyword<'a> {
    InPlaceApplicator(InPlaceApplicator<'a>),
    SubInstanceApplicator(SubInstanceApplicator<'a>),
    Assertion(Assertion),
}

#[derive(Clone, Debug)]
enum InPlaceApplicator<'a> {
    // In-place
    AllOf(&'a Vec<Value>),
    AnyOf(&'a Vec<Value>),
    OneOf(&'a Vec<Value>),
    Ref(String),
    Const(&'a Value),
    Enum(&'a Vec<Value>),
}

#[derive(Clone, Debug)]
enum SubInstanceApplicator<'a> {
    // Object
    Properties(IndexMap<&'a String, ResourceRef<'a>>),
    AdditionalProperties {
        applicator: ResourceRef<'a>,
        properties: HashSet<&'a String>,
    },
    // Array
    PrefixItems(Vec<ResourceRef<'a>>),
    Items {
        applicator: ResourceRef<'a>,
        prefix_items: usize,
    },
}

#[derive(Clone, Debug)]
enum Assertion {
    // Core
    Types(Types),
    // Number
    Minimum(f64),
    Maximum(f64),
    ExclusiveMinimum(f64),
    ExclusiveMaximum(f64),
    MultipleOf(Decimal),
    // String
    MinLength(u64),
    MaxLength(u64),
    Pattern(String),
    Format(String),
    // Array
    MinItems(u64),
    MaxItems(u64),
    // Object
    Required(IndexSet<String>),
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

    fn with_resource(mut self, resource: ResourceRef<'ctx>) -> Result<Self> {
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

    fn with_keyword(mut self, kwd: Keyword<'ctx>) -> Result<Self> {
        match kwd {
            Keyword::Assertion(assertion) => {
                self.inner.assert(assertion)?;
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
            PreSchemaInner::Simple(schema) => schema.compile(self.ctx),
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

#[derive(Clone, Debug)]
struct SimpleSchema<'a> {
    types: Types,
    // Number
    minimum: Option<f64>,
    maximum: Option<f64>,
    exclusive_minimum: Option<f64>,
    exclusive_maximum: Option<f64>,
    multiple_of: Option<Decimal>,
    // String
    min_length: u64,
    max_length: Option<u64>,
    pattern: Option<RegexAst>,
    // Array
    min_items: u64,
    max_items: Option<u64>,
    // Object
    required: IndexSet<String>,
    // Sub-instance
    sub_instance_applicators: Vec<SubInstanceApplicator<'a>>,
}

fn pattern_to_regex(pattern: &str) -> RegexAst {
    let left_anchored = pattern.starts_with('^');
    let right_anchored = pattern.ends_with('$');
    let trimmed = pattern.trim_start_matches('^').trim_end_matches('$');
    let mut result = String::new();
    if !left_anchored {
        result.push_str(".*");
    }
    // without parens, for a|b we would get .*a|b.* which is (.*a)|(b.*)
    result.push_str("(");
    result.push_str(trimmed);
    result.push_str(")");
    if !right_anchored {
        result.push_str(".*");
    }
    RegexAst::Regex(result)
}

impl SimpleSchema<'_> {
    fn assert(&mut self, assertion: Assertion) -> Result<()> {
        match assertion {
            Assertion::Types(v) => {
                self.types &= Types::try_from(v)?;
                Ok(())
            }
            Assertion::Minimum(v) => {
                match self.minimum {
                    Some(min) => {
                        self.minimum = Some(min.max(v));
                    }
                    None => {
                        self.minimum = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::Maximum(v) => {
                match self.maximum {
                    Some(max) => {
                        self.maximum = Some(max.min(v));
                    }
                    None => {
                        self.maximum = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::ExclusiveMinimum(v) => {
                match self.exclusive_minimum {
                    Some(min) => {
                        self.exclusive_minimum = Some(min.max(v));
                    }
                    None => {
                        self.exclusive_minimum = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::ExclusiveMaximum(v) => {
                match self.exclusive_maximum {
                    Some(max) => {
                        self.exclusive_maximum = Some(max.min(v));
                    }
                    None => {
                        self.exclusive_maximum = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::MultipleOf(v) => {
                match &self.multiple_of {
                    Some(m) => {
                        self.multiple_of = Some(m.lcm(&v));
                    }
                    None => {
                        self.multiple_of = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::MinLength(v) => {
                self.min_length = self.min_length.max(v);
                Ok(())
            }
            Assertion::MaxLength(v) => {
                match self.max_length {
                    Some(max) => {
                        self.max_length = Some(max.min(v));
                    }
                    None => {
                        self.max_length = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::Pattern(v) => {
                let p2 = pattern_to_regex(&v);
                match &self.pattern {
                    Some(p1) => {
                        self.pattern = Some(RegexAst::And(vec![p1.clone(), p2]));
                    }
                    None => {
                        self.pattern = Some(p2);
                    }
                }
                Ok(())
            }
            Assertion::Format(v) => {
                let fmt = lookup_format(&v).ok_or_else(|| anyhow!("Unknown format: {}", v))?;
                let p2 = pattern_to_regex(fmt);
                match &self.pattern {
                    Some(p1) => {
                        self.pattern = Some(RegexAst::And(vec![p1.clone(), p2]));
                    }
                    None => {
                        self.pattern = Some(p2);
                    }
                }
                Ok(())
            }
            Assertion::MinItems(v) => {
                self.min_items = self.min_items.max(v);
                Ok(())
            }
            Assertion::MaxItems(v) => {
                match self.max_items {
                    Some(max) => {
                        self.max_items = Some(max.min(v));
                    }
                    None => {
                        self.max_items = Some(v);
                    }
                }
                Ok(())
            }
            Assertion::Required(v) => {
                for k in v.into_iter() {
                    self.required.insert(k);
                }
                Ok(())
            }
        }
    }

    fn compile(self, ctx: &Context) -> Result<Schema> {
        let mut options = Vec::new();
        if self.types.contains(Types::NULL) {
            options.push(Schema::Null);
        }
        if self.types.contains(Types::BOOLEAN) {
            // todo -- literal boolean
            options.push(Schema::Boolean);
        }
        if self.types.contains(Types::NUMBER) {
            options.push(Schema::Number {
                minimum: self.minimum,
                maximum: self.maximum,
                exclusive_minimum: self.exclusive_minimum,
                exclusive_maximum: self.exclusive_maximum,
                multiple_of: self.multiple_of,
                integer: !self.types.contains(Types::NON_INTEGER),
            });
        }
        if self.types.contains(Types::STRING) {
            options.push(Schema::String {
                min_length: self.min_length,
                max_length: self.max_length,
                regex: self.pattern,
            });
        }
        if self.types.contains(Types::OBJECT) {
            let mut properties = IndexMap::<&String, SchemaPromise>::new();
            let mut additional = Vec::<(HashSet<&String>, ResourceRef)>::new();
            for applicator in self.sub_instance_applicators {
                match applicator {
                    SubInstanceApplicator::Properties(properties_to_apply) => {
                        for (k, v) in properties_to_apply {
                            let promise = if let Some(promise) = properties.get_mut(k) {
                                promise
                            } else {
                                let mut promise = SchemaPromise::new();
                                for (excluded, v) in additional.iter() {
                                    if !excluded.contains(k) {
                                        promise.push_resource(v.clone());
                                    }
                                }
                                properties.insert(k, promise);
                                properties.get_mut(k).unwrap()
                            };
                            promise.push_resource(v);
                        }
                    }
                    SubInstanceApplicator::AdditionalProperties {
                        applicator,
                        properties: excluded,
                    } => {
                        for (k, v) in properties.iter_mut() {
                            if !excluded.contains(k) {
                                v.push_resource(applicator.clone());
                            }
                        }
                        additional.push((excluded, applicator));
                    }
                    _ => todo!(),
                }
            }
            let properties = properties
                .into_iter()
                .map(|(k, promise)| {
                    let schema = promise.compile(ctx)?;
                    Ok((k.clone(), schema))
                })
                .collect::<Result<IndexMap<String, Schema>>>()?;

            let additional_properties = additional
                .into_iter()
                .fold(SchemaPromise::new(), |mut acc, (_, applicator)| {
                    acc.push_resource(applicator);
                    acc
                })
                .compile(ctx)?;
            options.push(Schema::Object {
                properties,
                additional_properties: Some(Box::new(additional_properties)),
                required: self.required,
            });
        }
        if self.types.contains(Types::ARRAY) {
            todo!();
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

impl Default for SimpleSchema<'_> {
    fn default() -> Self {
        SimpleSchema {
            types: Types { bits: Types::ALL },
            minimum: None,
            maximum: None,
            exclusive_minimum: None,
            exclusive_maximum: None,
            multiple_of: None,
            min_length: 0,
            max_length: None,
            pattern: None,
            min_items: 0,
            max_items: None,
            required: IndexSet::new(),
            sub_instance_applicators: Vec::new(),
        }
    }
}

impl<'a> PreSchemaInner<'a> {
    fn assert(&mut self, assertion: Assertion) -> Result<()> {
        match self {
            PreSchemaInner::Any => {
                let mut concrete = SimpleSchema::default();
                concrete.assert(assertion)?;
                *self = PreSchemaInner::Simple(concrete);
            }
            PreSchemaInner::False => {}
            PreSchemaInner::Simple(schema) => {
                schema.assert(assertion)?;
            }
            PreSchemaInner::AnyOf(schemas) => {
                for schema in schemas.iter_mut() {
                    schema.inner.assert(assertion.clone())?;
                }
            }
            PreSchemaInner::OneOf(schemas) => {
                for schema in schemas.iter_mut() {
                    schema.inner.assert(assertion.clone())?;
                }
            }
        };
        Ok(())
    }
    fn apply_to_subinstance(&mut self, applicator: SubInstanceApplicator<'a>) {
        match self {
            PreSchemaInner::Any => {
                let mut concrete = SimpleSchema::default();
                concrete.sub_instance_applicators.push(applicator);
                *self = PreSchemaInner::Simple(concrete);
            }
            PreSchemaInner::False => {}
            PreSchemaInner::Simple(schema) => {
                schema.sub_instance_applicators.push(applicator);
            }
            PreSchemaInner::AnyOf(schemas) | PreSchemaInner::OneOf(schemas) => {
                for schema in schemas.iter_mut() {
                    schema.inner.apply_to_subinstance(applicator.clone());
                }
            }
        };
    }
}

#[derive(Debug)]
struct SchemaPromise<'a> {
    resources: Vec<ResourceRef<'a>>,
}
impl Hash for SchemaPromise<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for resource in self.resources.iter() {
            resource.contents().hash(state);
            resource.draft().hash(state);
        }
    }
}
impl<'a> SchemaPromise<'a> {
    fn new() -> Self {
        SchemaPromise {
            resources: Vec::new(),
        }
    }
    fn push_resource(&mut self, resource: ResourceRef<'a>) {
        self.resources.push(resource);
    }
    fn compile(self, ctx: &Context) -> Result<Schema> {
        // Build a fake URI using the hash of the schema
        // in order to avoid infinite recursion
        let mut hash = DefaultHasher::new();
        self.hash(&mut hash);
        let hash = hash.finish();
        let uri = ctx.normalize_ref(&format!("#{}", hash))?;
        if !ctx.been_seen(&uri) {
            ctx.mark_seen(&uri);
            let compiled = self._compile(ctx)?;
            ctx.insert_ref(&uri, compiled.clone());
        }
        Ok(Schema::Ref { uri })
    }
    fn _compile(self, ctx: &Context) -> Result<Schema> {
        self.resources
            .into_iter()
            .fold(Ok(PreSchema::new(ctx)), |acc, resource| {
                acc.and_then(|pre_schema| pre_schema.with_resource(resource))
            })?
            .compile()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Types {
    bits: u8,
}
impl Types {
    const NULL: u8 = 1 << 0;
    const BOOLEAN: u8 = 1 << 1;
    const NON_INTEGER: u8 = 1 << 2;
    const INTEGER: u8 = 1 << 3;
    const STRING: u8 = 1 << 4;
    const ARRAY: u8 = 1 << 5;
    const OBJECT: u8 = 1 << 6;
    const NUMBER: u8 = Self::INTEGER | Self::NON_INTEGER;
    const ALL: u8 =
        Self::NULL | Self::BOOLEAN | Self::NUMBER | Self::STRING | Self::ARRAY | Self::OBJECT;

    fn contains(&self, flag: u8) -> bool {
        self.bits & flag != 0
    }
}

impl BitAndAssign for Types {
    fn bitand_assign(&mut self, rhs: Self) {
        self.bits &= rhs.bits;
    }
}

impl BitOrAssign for Types {
    fn bitor_assign(&mut self, rhs: Self) {
        self.bits |= rhs.bits;
    }
}

impl TryFrom<&str> for Types {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self> {
        let bits = match value {
            "null" => Types::NULL,
            "boolean" => Types::BOOLEAN,
            "integer" => Types::INTEGER,
            "string" => Types::STRING,
            "array" => Types::ARRAY,
            "object" => Types::OBJECT,
            "number" => Types::NUMBER,
            _ => bail!("unknown type: {}", value),
        };
        Ok(Types { bits })
    }
}

impl TryFrom<&Value> for Types {
    type Error = anyhow::Error;

    fn try_from(value: &Value) -> Result<Self> {
        if let Some(s) = value.as_str() {
            Types::try_from(s)
        } else if let Some(v) = value.as_array() {
            let mut types = Types { bits: 0 };
            for t in v.iter() {
                let s = t
                    .as_str()
                    .ok_or_else(|| anyhow!("type array must be array of strings"))?;
                types |= Types::try_from(s)?;
            }
            Ok(types)
        } else {
            bail!("type must be a string or array of strings")
        }
    }
}

fn get_keywords<'a>(ctx: &Context, schema: &'a Map<String, Value>) -> Result<Vec<Keyword<'a>>> {
    let mut keywords = Vec::new();
    let mut unimplemented = Vec::new();
    for (k, v) in schema.iter() {
        match k.as_str() {
            "type" => {
                keywords.push(Keyword::Assertion(Assertion::Types(Types::try_from(v)?)));
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
            "$ref" => {
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
                    let properties = v
                        .iter()
                        .map(|(k, v)| {
                            let resource = ctx.as_resource_ref(v);
                            Ok((k, resource))
                        })
                        .collect::<Result<IndexMap<&String, ResourceRef>>>()?;
                    keywords.push(Keyword::SubInstanceApplicator(
                        SubInstanceApplicator::Properties(properties),
                    ));
                } else {
                    bail!("properties must be an object");
                }
            }
            "additionalProperties" => {
                let properties = schema
                    .get("properties")
                    .map(|v| {
                        v.as_object()
                            .ok_or_else(|| anyhow!("properties must be an object"))?
                            .iter()
                            .map(|(k, _)| Ok(k))
                            .collect::<Result<HashSet<&String>>>()
                    })
                    .transpose()?
                    .unwrap_or_default();
                keywords.push(Keyword::SubInstanceApplicator(
                    SubInstanceApplicator::AdditionalProperties {
                        applicator: ctx.as_resource_ref(v),
                        properties,
                    },
                ));
            }
            "prefixItems" => {
                if ctx.draft <= Draft::Draft201909
                    || schema.get("additionalItems").is_some()
                    || schema.get("items").and_then(|v| v.as_array()).is_some()
                {
                    // Note that draft detection falls back to Draft202012 if the draft is unknown, so let's
                    // relax the draft constraint a bit and assume we're in an old draft if additionalItems
                    // is present or items is an array. In this case, prefixItems isn't used.
                    continue;
                }
                keywords.push(Keyword::SubInstanceApplicator(
                    SubInstanceApplicator::PrefixItems(
                        v.as_array()
                            .ok_or_else(|| anyhow!("prefixItems must be an array"))?
                            .iter()
                            .map(|v| ctx.as_resource_ref(v))
                            .collect(),
                    ),
                ));
            }
            "items" => {
                if ctx.draft <= Draft::Draft201909 || schema.get("additionalItems").is_some() {
                    // Note that draft detection falls back to Draft202012 if the draft is unknown, so let's
                    // relax the draft constraint a bit and assume we're in an old draft if additionalItems
                    // is present. In this case, we let items be an array and treat it as a prefixItems.
                    if let Some(items) = v.as_array() {
                        keywords.push(Keyword::SubInstanceApplicator(
                            SubInstanceApplicator::PrefixItems(
                                items.iter().map(|v| ctx.as_resource_ref(v)).collect(),
                            ),
                        ));
                        continue;
                    }
                }
                keywords.push(Keyword::SubInstanceApplicator(
                    SubInstanceApplicator::Items {
                        applicator: ctx.as_resource_ref(v),
                        prefix_items: schema
                            .get("prefixItems")
                            .and_then(|v| v.as_array())
                            .map_or(0, |v| v.len()),
                    },
                ));
            }
            "additionalItems" => {
                // Note that draft detection falls back to Draft202012 if the draft is unknown, so let's
                // relax the draft constraint a bit and assume we're in an old draft if additionalItems
                // is present. In this case, we treat it as items if and only if items is an array (which
                // we will treat as prefixItems). Otherwise, we ignore additionalItems.
                if let Some(prefix_items) = schema.get("items").and_then(|v| v.as_array()) {
                    keywords.push(Keyword::SubInstanceApplicator(
                        SubInstanceApplicator::Items {
                            applicator: ctx.as_resource_ref(v),
                            prefix_items: prefix_items.len(),
                        },
                    ));
                };
            }
            "minItems" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MinItems(
                        v.as_u64()
                            .ok_or_else(|| anyhow!("minItems must be a positive integer"))?,
                    )));
                } else {
                    bail!("minItems must be a number");
                }
            }
            "maxItems" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MaxItems(
                        v.as_u64()
                            .ok_or_else(|| anyhow!("maxItems must be a positive integer"))?,
                    )));
                } else {
                    bail!("maxItems must be a number");
                }
            }
            "minLength" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MinLength(
                        v.as_u64()
                            .ok_or_else(|| anyhow!("minLength must be a positive integer"))?,
                    )));
                } else {
                    bail!("minLength must be a number");
                }
            }
            "maxLength" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::MaxLength(
                        v.as_u64()
                            .ok_or_else(|| anyhow!("maxLength must be a positive integer"))?,
                    )));
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
                if let Some(v) = v.as_number().and_then(|v| v.as_f64()) {
                    keywords.push(Keyword::Assertion(Assertion::Minimum(v)));
                } else {
                    bail!("minimum must be a number");
                }
            }
            "maximum" => {
                if let Some(v) = v.as_number().and_then(|v| v.as_f64()) {
                    keywords.push(Keyword::Assertion(Assertion::Maximum(v)));
                } else {
                    bail!("maximum must be a number");
                }
            }
            "exclusiveMinimum" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::ExclusiveMinimum(
                        v.as_f64()
                            .ok_or_else(|| anyhow!("exclusiveMinimum must be a number"))?,
                    )));
                } else if let Some(b) = v.as_bool() {
                    // To handle old draft-4 style boolean
                    if b {
                        let v = schema.get("minimum").and_then(|v| v.as_f64()).ok_or_else(|| {
                            anyhow!("If exclusiveMinimum is true, minimum must be specified and a number")
                        })?;
                        keywords.push(Keyword::Assertion(Assertion::ExclusiveMinimum(v)));
                    }
                } else {
                    bail!("exclusiveMinimum must be a number or boolean");
                }
            }
            "exclusiveMaximum" => {
                if let Some(v) = v.as_number() {
                    keywords.push(Keyword::Assertion(Assertion::ExclusiveMaximum(
                        v.as_f64()
                            .ok_or_else(|| anyhow!("exclusiveMaximum must be a number"))?,
                    )));
                } else if let Some(b) = v.as_bool() {
                    // To handle old draft-4 style boolean
                    if b {
                        let v = schema.get("maximum").and_then(|v| v.as_f64()).ok_or_else(|| {
                            anyhow!("If exclusiveMaximum is true, maximum must be specified and a number")
                        })?;
                        keywords.push(Keyword::Assertion(Assertion::ExclusiveMaximum(v)));
                    }
                } else {
                    bail!("exclusiveMaximum must be a number or boolean");
                }
            }
            "multipleOf" => {
                if let Some(v) = v.as_number().and_then(|v| v.as_f64()) {
                    keywords.push(Keyword::Assertion(Assertion::MultipleOf(
                        Decimal::try_from(v)?,
                    )));
                } else {
                    bail!("multipleOf must be a number");
                }
            }
            "required" => {
                if let Some(v) = v.as_array() {
                    let mut required = IndexSet::new();
                    for item in v.iter() {
                        if let Some(s) = item.as_str() {
                            required.insert(s.to_string());
                        } else {
                            bail!("required must be an array of strings");
                        }
                    }
                    keywords.push(Keyword::Assertion(Assertion::Required(required)));
                } else {
                    bail!("required must be an array");
                }
            }
            "$anchor" | "$defs" | "definitions" | "$schema" | "$id" | "id" | "$comment"
            | "title" | "description" | "default" | "readOnly" | "writeOnly" | "examples"
            | "contentMediaType" | "contentEncoding" => {
                // Ignore these keywords -- they are annotations and metadata
            }
            k => {
                if ctx.draft.is_known_keyword(k) {
                    unimplemented.push(k.to_string());
                }
                // Otherwise we ignore
            }
        }
    }
    if !unimplemented.is_empty() {
        bail!("Unimplemented keywords: {:?}", unimplemented);
    }
    Ok(keywords)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_schema() {
        let schema = json!({
            "allOf": [{"$ref": "#/$defs/tree1"}, {"$ref": "#/$defs/tree2"}],
            "type": "object",
            "$defs": {
                "tree1": {
                    "$id": "https://example.com/tree1",
                    "type": ["object", "null"],
                    "properties": {
                        "left": { "$ref": "#/$defs/tree1" },
                        "right": { "$ref": "#/$defs/tree2" }
                    },
                    "required": ["center", "left", "right"]
                },
                "tree2": {
                    "$id": "https://example.com/tree2",
                    "type": ["object", "null"],
                    "properties": {
                        "left": { "$ref": "#/$defs/tree2" },
                        "right": { "$ref": "#/$defs/tree1" }
                    },
                    "required": ["left", "right", "under"]
                }
            }
        });
        let (compiled, defs) = build_schema(schema, None).unwrap();
        println!("{:?}", compiled);
        println!("{:?}", defs);
    }
}
