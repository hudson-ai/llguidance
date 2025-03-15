use super::context::{Context, Draft, PreContext, ResourceRef};
use super::numeric::Decimal;
use super::RetrieveWrapper;
use crate::{HashMap, HashSet};

use anyhow::{anyhow, bail, Result};
use indexmap::{IndexMap, IndexSet};
use serde_json::{Map, Value};

#[derive(Clone, Debug)]
enum RawSchema {
    True,
    False,
    Kwds(Vec<Keyword>),
}

#[derive(Clone, Debug)]
enum ParcookedSchema {
    Any,
    Unsatisfiable(String),
    Simple(Box<SimpleParcooked>),
    AnyOf(Vec<ParcookedSchema>),
    OneOf(Vec<ParcookedSchema>),
}

#[derive(Clone, Debug)]
struct SimpleParcooked {
    // This instance
    assertions: Vec<Assertion>,
    // Sub instances
    properties: IndexMap<String, RawSchema>,
    pending_properties: HashMap<String, RawSchema>,
    additional_properties: RawSchema,
    prefix_items: HashMap<usize, RawSchema>,
    pending_prefix_items: HashMap<usize, RawSchema>,
    items: RawSchema,
}

impl SimpleParcooked {
    fn new() -> Self {
        Self {
            assertions: Vec::new(),
            properties: IndexMap::new(),
            pending_properties: HashMap::default(),
            additional_properties: RawSchema::True,
            prefix_items: HashMap::default(),
            pending_prefix_items: HashMap::default(),
            items: RawSchema::True,
        }
    }
    fn assert(&mut self, assertion: Assertion) {
        self.assertions.push(assertion);
    }
    fn apply_to_subinstance(&mut self, applicator: SubInstanceApplicator) {
        match applicator {
            SubInstanceApplicator::Properties(properties) => {
                for (k, v) in properties {
                    if let Some(schema) = self.properties.get_mut(&k) {
                        schema.intersect(v)
                    } else if let Some(mut schema) = self.pending_properties.remove(&k) {
                        schema.intersect(v);
                        self.properties.insert(k, schema);
                    } else {
                        let mut schema = self.additional_properties.clone();
                        schema.intersect(v);
                        self.pending_properties.insert(k, schema);
                    }
                }
            }
            SubInstanceApplicator::AdditionalProperties {
                applicator,
                properties,
            } => {
                for (k, v) in self.properties.iter_mut() {
                    if !properties.contains(k) {
                        v.intersect(applicator.clone());
                    }
                }
                for (k, v) in self.pending_properties.iter_mut() {
                    if !properties.contains(k) {
                        v.intersect(applicator.clone());
                    }
                }
                for k in properties {
                    if !self.properties.contains_key(&k) {
                        self.pending_properties.insert(k, applicator.clone());
                    }
                }
                self.additional_properties.intersect(applicator);
            }
            _ => todo!(),
        }
    }
}

impl RawSchema {
    fn parcook(self, definitions: &HashMap<String, RawSchema>) -> ParcookedSchema {
        ParcookedSchema::Any.intersect(self, definitions)
    }
    fn intersect(&mut self, other: RawSchema) {
        // missed optimization opportunity since we're accumulating all keywords and not
        // sharing work for common prefixes
        todo!()
    }
}

impl ParcookedSchema {
    fn intersect(mut self, other: RawSchema, definitions: &HashMap<String, RawSchema>) -> Self {
        let kwds = match other {
            RawSchema::True => return self,
            RawSchema::False => return ParcookedSchema::Unsatisfiable("false schema".to_string()),
            RawSchema::Kwds(kwds) => kwds,
        };
        for kwd in kwds {
            match kwd {
                Keyword::Assertion(assertion) => {
                    self.assert(assertion);
                }
                Keyword::InPlaceApplicator(applicator) => match applicator {
                    InPlaceApplicator::AllOf(subschemas) => {
                        for schema in subschemas {
                            self = self.intersect(schema, definitions);
                        }
                    }
                    InPlaceApplicator::AnyOf(subschemas) => {
                        let options = subschemas
                            .into_iter()
                            .map(|schema| self.clone().intersect(schema, definitions))
                            .collect::<Vec<_>>();
                        self = ParcookedSchema::AnyOf(options);
                    }
                    InPlaceApplicator::OneOf(subschemas) => {
                        let options = subschemas
                            .into_iter()
                            .map(|schema| self.clone().intersect(schema, definitions))
                            .collect::<Vec<_>>();
                        self = ParcookedSchema::OneOf(options);
                    }
                    InPlaceApplicator::Const(value) => {
                        todo!()
                    }
                    InPlaceApplicator::Ref(uri) => {
                        if let Some(schema) = definitions.get(&uri) {
                            self = self.intersect(schema.clone(), definitions);
                        } else {
                            // TODO: Results
                            panic!("We should have already crawled this schema...");
                        }
                    }
                },
                Keyword::SubInstanceApplicator(applicator) => self.apply_to_subinstance(applicator),
            }
        }
        self
    }

    fn assert(&mut self, assertion: Assertion) {
        match self {
            ParcookedSchema::Any => {
                *self = ParcookedSchema::Simple(Box::new(SimpleParcooked::new()));
                self.assert(assertion);
            }
            ParcookedSchema::Unsatisfiable(_) => {
                // No-op
            }
            ParcookedSchema::AnyOf(subschemas) | ParcookedSchema::OneOf(subschemas) => {
                for schema in subschemas {
                    schema.assert(assertion.clone());
                }
            }
            ParcookedSchema::Simple(simple) => {
                // Base case
                simple.assert(assertion);
            }
        }
    }

    fn apply_to_subinstance(&mut self, applicator: SubInstanceApplicator) {
        match self {
            ParcookedSchema::Any => {
                *self = ParcookedSchema::Simple(Box::new(SimpleParcooked::new()));
                self.apply_to_subinstance(applicator);
            }
            ParcookedSchema::Unsatisfiable(_) => {
                // No-op
            }
            ParcookedSchema::AnyOf(subschemas) | ParcookedSchema::OneOf(subschemas) => {
                for schema in subschemas {
                    schema.apply_to_subinstance(applicator.clone());
                }
            }
            ParcookedSchema::Simple(simple) => simple.apply_to_subinstance(applicator),
        }
    }
}

#[derive(Clone, Debug)]
enum Keyword {
    InPlaceApplicator(InPlaceApplicator),
    SubInstanceApplicator(SubInstanceApplicator),
    Assertion(Assertion),
}

#[derive(Clone, Debug)]
enum InPlaceApplicator {
    AllOf(Vec<RawSchema>),
    AnyOf(Vec<RawSchema>),
    OneOf(Vec<RawSchema>),
    Const(Value),
    Ref(String),
}

#[derive(Clone, Debug)]
enum SubInstanceApplicator {
    // Object
    Properties(IndexMap<String, RawSchema>),
    AdditionalProperties {
        applicator: RawSchema,
        properties: HashSet<String>,
    },
    // Array
    PrefixItems(Vec<RawSchema>),
    Items {
        applicator: RawSchema,
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

#[derive(Clone, Debug)]
struct Types {
    // todo
}

struct SchemaBuilder {
    definitions: HashMap<String, RawSchema>,
    seen: HashSet<String>,
}

impl SchemaBuilder {
    fn new() -> Self {
        Self {
            definitions: HashMap::default(),
            seen: HashSet::default(),
        }
    }

    fn crawl(
        mut self,
        schema: Value,
        retriever: Option<RetrieveWrapper>,
    ) -> Result<(RawSchema, HashMap<String, RawSchema>)> {
        let pre_ctx = PreContext::new(schema, retriever)?;
        let ctx = Context::new(&pre_ctx)?;

        let root_resource = ctx.lookup_resource(&pre_ctx.base_uri)?;
        let psk = self.visit(&ctx, root_resource)?;
        Ok((psk, self.definitions))
    }

    fn visit(&mut self, ctx: &Context, resource: ResourceRef) -> Result<RawSchema> {
        let ctx = ctx.in_subresource(resource)?;
        let schema = resource.contents();
        if let Some(b) = schema.as_bool() {
            let psk = if b { RawSchema::True } else { RawSchema::False };
            return Ok(psk);
        }
        let schema = schema
            .as_object()
            .ok_or_else(|| anyhow!("schema must be an object or boolean"))?;
        let psk = RawSchema::Kwds(self.keywords(&ctx, schema)?);
        Ok(psk)
    }

    fn keywords(&mut self, ctx: &Context, schema: &Map<String, Value>) -> Result<Vec<Keyword>> {
        let mut keywords = Vec::new();
        let mut unimplemented = Vec::new();
        for (k, v) in schema.iter() {
            match k.as_str() {
                "type" => {
                    todo!();
                }
                "allOf" => {
                    if let Some(v) = v.as_array() {
                        let schemas = v
                            .iter()
                            .map(|v| ctx.as_resource_ref(v))
                            .map(|r| self.visit(ctx, r))
                            .collect::<Result<Vec<_>>>()?;
                        keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::AllOf(
                            schemas,
                        )));
                    } else {
                        bail!("allOf must be an array");
                    }
                }
                "anyOf" => {
                    if let Some(v) = v.as_array() {
                        let schemas = v
                            .iter()
                            .map(|v| ctx.as_resource_ref(v))
                            .map(|r| self.visit(ctx, r))
                            .collect::<Result<Vec<_>>>()?;
                        keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::AnyOf(
                            schemas,
                        )));
                    } else {
                        bail!("anyOf must be an array");
                    }
                }
                "oneOf" => {
                    if let Some(v) = v.as_array() {
                        let schemas = v
                            .iter()
                            .map(|v| ctx.as_resource_ref(v))
                            .map(|r| self.visit(ctx, r))
                            .collect::<Result<Vec<_>>>()?;
                        keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::OneOf(
                            schemas,
                        )));
                    } else {
                        bail!("oneOf must be an array");
                    }
                }
                "$ref" => {
                    if let Some(v) = v.as_str() {
                        let uri = ctx.normalize_ref(v)?;
                        if !self.seen.contains(&uri) {
                            self.seen.insert(uri.clone());
                            let resource = ctx.lookup_resource(&uri);
                            let psk = self.visit(ctx, resource?)?;
                            self.definitions.insert(uri.clone(), psk);
                        }
                        keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::Ref(uri)));
                    } else {
                        bail!("$ref must be a string");
                    }
                }
                "const" => {
                    keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::Const(
                        v.clone(),
                    )));
                }
                "enum" => {
                    if let Some(v) = v.as_array() {
                        let options = v
                            .iter()
                            .cloned()
                            .map(InPlaceApplicator::Const)
                            .map(Keyword::InPlaceApplicator)
                            .map(|kwd| RawSchema::Kwds(vec![kwd]))
                            .collect::<Vec<_>>();
                        keywords.push(Keyword::InPlaceApplicator(InPlaceApplicator::AnyOf(
                            options,
                        )));
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
                                let psk = self.visit(ctx, resource)?;
                                Ok((k.clone(), psk))
                            })
                            .collect::<Result<IndexMap<String, RawSchema>>>()?;
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
                                .map(|(k, _)| Ok(k.clone()))
                                .collect::<Result<HashSet<String>>>()
                        })
                        .transpose()?
                        .unwrap_or_default();
                    keywords.push(Keyword::SubInstanceApplicator(
                        SubInstanceApplicator::AdditionalProperties {
                            applicator: self.visit(ctx, ctx.as_resource_ref(v))?,
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
                                .map(|v| self.visit(ctx, ctx.as_resource_ref(v)))
                                .collect::<Result<Vec<_>>>()?,
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
                                    items
                                        .iter()
                                        .map(|v| self.visit(ctx, ctx.as_resource_ref(v)))
                                        .collect::<Result<Vec<_>>>()?,
                                ),
                            ));
                            continue;
                        }
                    }
                    keywords.push(Keyword::SubInstanceApplicator(
                        SubInstanceApplicator::Items {
                            applicator: self.visit(ctx, ctx.as_resource_ref(v))?,
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
                                applicator: self.visit(ctx, ctx.as_resource_ref(v))?,
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
}
