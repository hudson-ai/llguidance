#![allow(dead_code)] // Just for development

use super::context::{Context, Draft, PreContext, ResourceRef};
use super::formats::lookup_format;
use super::numeric::Decimal;
use super::RetrieveWrapper;
use crate::{HashMap, HashSet};

use anyhow::{anyhow, bail, Result};
use derivre::RegexAst;
use indexmap::{IndexMap, IndexSet};
use regex_syntax::escape;
use serde_json::{Map, Value};
use std::ops::{BitAndAssign, BitOrAssign};

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
    assertions: Assertions,
    // Sub instances
    properties: IndexMap<String, RawSchema>,
    pending_properties: HashMap<String, RawSchema>,
    additional_properties: RawSchema,
    prefix_items: HashMap<usize, RawSchema>,
    pending_prefix_items: HashMap<usize, RawSchema>,
    items: RawSchema,
}

#[derive(Clone, Debug)]
struct Assertions {
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
}

impl Assertions {
    fn assert(&mut self, assertion: Assertion) -> Result<()> {
        match assertion {
            Assertion::Types(v) => {
                self.types &= v;
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
}

impl Default for SimpleParcooked {
    fn default() -> Self {
        Self {
            assertions: Assertions::default(),
            properties: IndexMap::new(),
            pending_properties: HashMap::default(),
            additional_properties: RawSchema::True,
            prefix_items: HashMap::default(),
            pending_prefix_items: HashMap::default(),
            items: RawSchema::True,
        }
    }
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
    result.push('(');
    result.push_str(trimmed);
    result.push(')');
    if !right_anchored {
        result.push_str(".*");
    }
    RegexAst::Regex(result)
}

impl SimpleParcooked {
    fn assert(&mut self, assertion: Assertion) -> Result<()> {
        self.assertions.assert(assertion)
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
                        self.properties.insert(k, schema);
                    }
                }
            }
            SubInstanceApplicator::AdditionalProperties {
                applicator,
                property_names,
            } => {
                for (k, v) in self.properties.iter_mut() {
                    if !property_names.contains(k) {
                        v.intersect(applicator.clone());
                    }
                }
                for (k, v) in self.pending_properties.iter_mut() {
                    if !property_names.contains(k) {
                        v.intersect(applicator.clone());
                    }
                }
                for k in property_names {
                    if !self.properties.contains_key(&k) {
                        self.pending_properties
                            .insert(k, self.additional_properties.clone());
                    }
                }
                self.additional_properties.intersect(applicator);
            }
            SubInstanceApplicator::PrefixItems(items) => {
                for (i, v) in items.into_iter().enumerate() {
                    if let Some(schema) = self.prefix_items.get_mut(&i) {
                        schema.intersect(v);
                    } else if let Some(mut schema) = self.pending_prefix_items.remove(&i) {
                        schema.intersect(v);
                        self.prefix_items.insert(i, schema);
                    } else {
                        let mut schema = self.items.clone();
                        schema.intersect(v);
                        self.prefix_items.insert(i, schema);
                    }
                }
            }
            SubInstanceApplicator::Items {
                applicator,
                n_prefix_items,
            } => {
                for (i, v) in self.prefix_items.iter_mut() {
                    if *i >= n_prefix_items {
                        v.intersect(applicator.clone());
                    }
                }
                for (i, v) in self.pending_prefix_items.iter_mut() {
                    if *i >= n_prefix_items {
                        v.intersect(applicator.clone());
                    }
                }
                for i in 0..n_prefix_items {
                    if !self.prefix_items.contains_key(&i) {
                        self.pending_prefix_items.insert(i, self.items.clone());
                    }
                }
                self.items.intersect(applicator);
            }
        }
    }
}

impl RawSchema {
    fn parcook(self, definitions: &HashMap<String, RawSchema>) -> Result<ParcookedSchema> {
        ParcookedSchema::Any.intersect(self, definitions)
    }
    fn intersect(&mut self, other: RawSchema) {
        // missed optimization opportunity since we're accumulating all keywords and not
        // sharing work for common prefixes?
        if matches!(self, RawSchema::True) | matches!(other, RawSchema::False) {
            *self = other;
        } else if let (RawSchema::Kwds(kwds), RawSchema::Kwds(other_kwds)) = (self, other) {
            kwds.extend(other_kwds);
        }
        // Other case are no-ops
    }
}

impl ParcookedSchema {
    fn intersect(
        mut self,
        other: RawSchema,
        definitions: &HashMap<String, RawSchema>,
    ) -> Result<Self> {
        let kwds = match other {
            RawSchema::True => return Ok(self),
            RawSchema::False => {
                return Ok(ParcookedSchema::Unsatisfiable("false schema".to_string()))
            }
            RawSchema::Kwds(kwds) => kwds,
        };
        for kwd in kwds {
            match kwd {
                Keyword::Assertion(assertion) => {
                    self.assert(assertion)?;
                }
                Keyword::InPlaceApplicator(applicator) => match applicator {
                    InPlaceApplicator::AllOf(subschemas) => {
                        for schema in subschemas {
                            self = self.intersect(schema, definitions)?;
                        }
                    }
                    InPlaceApplicator::AnyOf(subschemas) => {
                        let options = subschemas
                            .into_iter()
                            .map(|schema| self.clone().intersect(schema, definitions))
                            .collect::<Result<Vec<_>>>()?;
                        self = ParcookedSchema::AnyOf(options);
                    }
                    InPlaceApplicator::OneOf(subschemas) => {
                        let options = subschemas
                            .into_iter()
                            .map(|schema| self.clone().intersect(schema, definitions))
                            .collect::<Result<Vec<_>>>()?;
                        self = ParcookedSchema::OneOf(options);
                    }
                    InPlaceApplicator::Const(value) => match value {
                        Value::Null => {
                            self.assert(Assertion::Types(Types::NULL.into()))?;
                        }
                        Value::Bool(_) => {
                            self.assert(Assertion::Types(Types::BOOLEAN.into()))?;
                        }
                        Value::Number(n) => {
                            let n = n.as_f64().ok_or_else(|| {
                                anyhow!("const number must be coercible to float")
                            })?;
                            self.assert(Assertion::Types(Types::NUMBER.into()))?;
                            self.assert(Assertion::Minimum(n))?;
                            self.assert(Assertion::Maximum(n))?;
                        }
                        Value::String(s) => {
                            self.assert(Assertion::Types(Types::STRING.into()))?;
                            self.assert(Assertion::Pattern(format!("^{}$", escape(&s))))?;
                        }
                        Value::Array(a) => {
                            self.assert(Assertion::Types(Types::ARRAY.into()))?;
                            self.assert(Assertion::MinItems(1))?;
                            self.assert(Assertion::MaxItems(1))?;
                            self.apply_to_subinstance(SubInstanceApplicator::Items {
                                applicator: RawSchema::False,
                                n_prefix_items: a.len(),
                            });
                            self.apply_to_subinstance(SubInstanceApplicator::PrefixItems(
                                a.into_iter()
                                    .map(|v| {
                                        RawSchema::Kwds(vec![Keyword::InPlaceApplicator(
                                            InPlaceApplicator::Const(v),
                                        )])
                                    })
                                    .collect::<Vec<_>>(),
                            ));
                        }
                        Value::Object(o) => {
                            self.assert(Assertion::Types(Types::OBJECT.into()))?;
                            self.assert(Assertion::Required(
                                o.keys()
                                    .map(|k| k.to_string())
                                    .collect::<IndexSet<String>>(),
                            ))?;
                            self.apply_to_subinstance(
                                SubInstanceApplicator::AdditionalProperties {
                                    applicator: RawSchema::False,
                                    property_names: o.keys().map(|k| k.to_string()).collect(),
                                },
                            );
                            self.apply_to_subinstance(SubInstanceApplicator::Properties(
                                o.iter()
                                    .map(|(k, v)| {
                                        let schema =
                                            RawSchema::Kwds(vec![Keyword::InPlaceApplicator(
                                                InPlaceApplicator::Const(v.clone()),
                                            )]);
                                        Ok((k.clone(), schema))
                                    })
                                    .collect::<Result<IndexMap<String, RawSchema>>>()?,
                            ));
                        }
                    },
                    InPlaceApplicator::Ref(uri) => {
                        if let Some(schema) = definitions.get(&uri) {
                            self = self.intersect(schema.clone(), definitions)?;
                        } else {
                            bail!("Reference to unknown schema: {}", uri);
                        }
                    }
                },
                Keyword::SubInstanceApplicator(applicator) => self.apply_to_subinstance(applicator),
            }
        }
        Ok(self)
    }

    fn assert(&mut self, assertion: Assertion) -> Result<()> {
        match self {
            ParcookedSchema::Any => {
                *self = ParcookedSchema::Simple(Box::default());
                self.assert(assertion)?;
            }
            ParcookedSchema::Unsatisfiable(_) => {
                // No-op
            }
            ParcookedSchema::AnyOf(subschemas) | ParcookedSchema::OneOf(subschemas) => {
                for schema in subschemas {
                    schema.assert(assertion.clone())?;
                }
            }
            ParcookedSchema::Simple(simple) => {
                // Base case
                simple.assert(assertion)?;
            }
        }
        Ok(())
    }

    fn apply_to_subinstance(&mut self, applicator: SubInstanceApplicator) {
        match self {
            ParcookedSchema::Any => {
                *self = ParcookedSchema::Simple(Box::default());
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
        property_names: HashSet<String>,
    },
    // Array
    PrefixItems(Vec<RawSchema>),
    Items {
        applicator: RawSchema,
        n_prefix_items: usize,
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

impl Default for Assertions {
    fn default() -> Self {
        Assertions {
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
        }
    }
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
                    keywords.push(Keyword::Assertion(Assertion::Types(Types::try_from(v)?)));
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
                            property_names: properties,
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
                            n_prefix_items: schema
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
                                n_prefix_items: prefix_items.len(),
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

#[derive(Clone, PartialEq, Eq, Hash)]
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

    fn new(bits: u8) -> Self {
        Types { bits }
    }

    fn contains(&self, flag: u8) -> bool {
        self.bits & flag != 0
    }
}

impl From<u8> for Types {
    fn from(bits: u8) -> Self {
        Types { bits }
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

impl std::fmt::Debug for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut types = Vec::new();
        if self.contains(Types::NULL) {
            types.push("null");
        }
        if self.contains(Types::BOOLEAN) {
            types.push("boolean");
        }
        if self.contains(Types::NUMBER) {
            if !self.contains(Types::NON_INTEGER) {
                types.push("integer");
            } else {
                types.push("number");
            }
        }
        if self.contains(Types::STRING) {
            types.push("string");
        }
        if self.contains(Types::ARRAY) {
            types.push("array");
        }
        if self.contains(Types::OBJECT) {
            types.push("object");
        }
        write!(f, "[{}]", types.join(", "))
    }
}
