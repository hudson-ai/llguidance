use crate::json::numeric::Decimal;
use anyhow::{anyhow, bail, Result};
use indexmap::{IndexMap, IndexSet};
use referencing::Draft;
use serde_json::Value;

// Keywords that are used for metadata or annotations, not directly driving validation.
// Note that some keywords like $id and $schema affect the behavior of other keywords, but
// they can safely be ignored if other keywords aren't present
const META_AND_ANNOTATIONS: [&str; 15] = [
    "$anchor",
    "$defs",
    "definitions",
    "$schema",
    "$id",
    "id",
    "$comment",
    "title",
    "description",
    "default",
    "readOnly",
    "writeOnly",
    "examples",
    "contentMediaType",
    "contentEncoding",
];

#[derive(Debug, Clone)]
pub enum PreSchema {
    Boolean(bool),
    Schema(Vec<Keyword>),
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Applicator(Applicator),
    Assertion(Assertion),
    // Annotation(String),
}

#[derive(Debug, Clone)]
pub enum Applicator {
    AnyOf(Vec<PreSchema>),
    OneOf(Vec<PreSchema>),
    AllOf(Vec<PreSchema>),
    Ref(String),
    Properties(IndexMap<String, PreSchema>),
    AdditionalProperties(PreSchema),
    Required(IndexSet<String>), // Not traditionally an applicator, but it's location dependent like one
    Items(PreSchema),
    PrefixItems(Vec<PreSchema>),
}

#[derive(Debug, Clone)]
pub enum Assertion {
    Type(TypeAssertion),
    Const(Value),
    Enum(Vec<Value>),
    Number(NumberAssertion),
    String(StringAssertion),
    Array(ArrayAssertion),
}

#[derive(Debug, Clone)]
pub enum NumberAssertion {
    Minimum(f64),
    Maximum(f64),
    ExclusiveMinimum(f64),
    ExclusiveMaximum(f64),
    MultipleOf(Decimal),
}

#[derive(Debug, Clone)]
pub enum StringAssertion {
    MinLength(u64),
    MaxLength(u64),
    Pattern(String),
    Format(String),
}

#[derive(Debug, Clone)]
pub enum ArrayAssertion {
    MinItems(u64),
    MaxItems(u64),
}

#[derive(Debug, Clone)]
pub enum TypeAssertion {
    Type(Type),
    Types(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null,
    Boolean,
    Number,
    Integer,
    String,
    Array,
    Object,
}

impl PreSchema {
    pub fn from_value(draft: Draft, value: Value) -> Result<PreSchema> {
        // TODO: handle additionalItems with funky draft preprocessing??
        match value {
            Value::Bool(b) => Ok(PreSchema::Boolean(b)),
            Value::Object(mapping) => {
                let keywords = mapping
                    .into_iter()
                    .filter_map(|(key, value)| {
                        Keyword::from_item(draft.clone(), key, value).transpose()
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(PreSchema::Schema(keywords))
            }
            // TODO: include location in error message?
            _ => bail!("schema must be a boolean or object"),
        }
    }
}

impl Keyword {
    fn from_item(draft: Draft, key: String, value: Value) -> Result<Option<Keyword>> {
        match key.as_str() {
            // Core
            "anyOf" => {
                let items = match value {
                    Value::Array(items) => items,
                    _ => bail!("anyOf must be an array"),
                };
                let schemas = items
                    .into_iter()
                    .map(|item| PreSchema::from_value(draft.clone(), item))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Some(Keyword::Applicator(Applicator::AnyOf(schemas))))
            }
            "oneOf" => {
                let items = match value {
                    Value::Array(items) => items,
                    _ => bail!("oneOf must be an array"),
                };
                let schemas = items
                    .into_iter()
                    .map(|item| PreSchema::from_value(draft.clone(), item))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Some(Keyword::Applicator(Applicator::OneOf(schemas))))
            }
            "allOf" => {
                let items = match value {
                    Value::Array(items) => items,
                    _ => bail!("allOf must be an array"),
                };
                let schemas = items
                    .into_iter()
                    .map(|item| PreSchema::from_value(draft.clone(), item))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Some(Keyword::Applicator(Applicator::AllOf(schemas))))
            }
            "$ref" => {
                let uri = match value {
                    Value::String(uri) => uri,
                    _ => bail!("$ref must be a string"),
                };
                Ok(Some(Keyword::Applicator(Applicator::Ref(uri))))
            }
            "const" => Ok(Some(Keyword::Assertion(Assertion::Const(value)))),
            "enum" => {
                let items = match value {
                    Value::Array(items) => items,
                    _ => bail!("enum must be an array"),
                };
                let instances = items.into_iter().collect::<Vec<Value>>();
                Ok(Some(Keyword::Assertion(Assertion::Enum(instances))))
            }
            "type" => match value {
                Value::String(tp) => Ok(Some(Keyword::Assertion(Assertion::Type(
                    TypeAssertion::Type(Type::from_str(&tp)?),
                )))),
                Value::Array(types) => {
                    let types = types
                        .into_iter()
                        .map(|tp| match tp {
                            Value::String(tp) => Ok(Type::from_str(&tp)?),
                            _ => bail!("type array must contain only strings"),
                        })
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Some(Keyword::Assertion(Assertion::Type(
                        TypeAssertion::Types(types),
                    ))))
                }
                _ => bail!("type must be a string or array of strings"),
            },
            // Array
            "items" => {
                let schema = PreSchema::from_value(draft.clone(), value)?;
                Ok(Some(Keyword::Applicator(Applicator::Items(schema))))
            }
            "additionalItems" => {
                todo!()
            }
            "prefixItems" => {
                let items = match value {
                    Value::Array(items) => items,
                    _ => bail!("prefixItems must be an array"),
                };
                let schemas = items
                    .into_iter()
                    .map(|item| PreSchema::from_value(draft.clone(), item))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Some(Keyword::Applicator(Applicator::PrefixItems(schemas))))
            }
            "minItems" => {
                let min_items = match value {
                    Value::Number(n) => n
                        .as_u64()
                        .ok_or_else(|| anyhow!("minItems must be a non-negative integer"))?,
                    _ => bail!("minItems must be a non-negative integer"),
                };
                Ok(Some(Keyword::Assertion(Assertion::Array(
                    ArrayAssertion::MinItems(min_items),
                ))))
            }
            "maxItems" => {
                let max_items = match value {
                    Value::Number(n) => n
                        .as_u64()
                        .ok_or_else(|| anyhow!("maxItems must be a non-negative integer"))?,
                    _ => bail!("maxItems must be a non-negative integer"),
                };
                Ok(Some(Keyword::Assertion(Assertion::Array(
                    ArrayAssertion::MaxItems(max_items),
                ))))
            }
            // Object
            "properties" => {
                let properties = match value {
                    Value::Object(mapping) => mapping,
                    _ => bail!("properties must be an object"),
                };
                let schemas = properties
                    .into_iter()
                    .map(|(key, value)| {
                        let schema = PreSchema::from_value(draft.clone(), value)?;
                        Ok((key, schema))
                    })
                    .collect::<Result<IndexMap<_, _>>>()?;
                Ok(Some(Keyword::Applicator(Applicator::Properties(schemas))))
            }
            "additionalProperties" => {
                let schema = PreSchema::from_value(draft.clone(), value)?;
                Ok(Some(Keyword::Applicator(Applicator::AdditionalProperties(
                    schema,
                ))))
            }
            "required" => {
                let required = match value {
                    Value::Array(items) => items,
                    _ => bail!("required must be an array"),
                };
                let keys = required
                    .into_iter()
                    .map(|item| match item {
                        Value::String(key) => Ok(key),
                        _ => bail!("required array must contain only strings"),
                    })
                    .collect::<Result<IndexSet<_>>>()?;
                Ok(Some(Keyword::Applicator(Applicator::Required(keys))))
            }
            // String
            "minLength" => {
                let min_length = match value {
                    Value::Number(n) => n
                        .as_u64()
                        .ok_or_else(|| anyhow!("minLength must be a non-negative integer"))?,
                    _ => bail!("minLength must be a non-negative integer"),
                };
                Ok(Some(Keyword::Assertion(Assertion::String(
                    StringAssertion::MinLength(min_length),
                ))))
            }
            "maxLength" => {
                let max_length = match value {
                    Value::Number(n) => n
                        .as_u64()
                        .ok_or_else(|| anyhow!("maxLength must be a non-negative integer"))?,
                    _ => bail!("maxLength must be a non-negative integer"),
                };
                Ok(Some(Keyword::Assertion(Assertion::String(
                    StringAssertion::MaxLength(max_length),
                ))))
            }
            "pattern" => {
                let pattern = match value {
                    Value::String(pattern) => pattern,
                    _ => bail!("pattern must be a string"),
                };
                Ok(Some(Keyword::Assertion(Assertion::String(
                    StringAssertion::Pattern(pattern),
                ))))
            }
            "format" => {
                let format = match value {
                    Value::String(format) => format,
                    _ => bail!("format must be a string"),
                };
                Ok(Some(Keyword::Assertion(Assertion::String(
                    StringAssertion::Format(format),
                ))))
            }
            // Number
            "minimum" => {
                let minimum = match value {
                    Value::Number(n) => n
                        .as_f64()
                        .ok_or_else(|| anyhow!("minimum must be a number"))?,
                    _ => bail!("minimum must be a number"),
                };
                Ok(Some(Keyword::Assertion(Assertion::Number(
                    NumberAssertion::Minimum(minimum),
                ))))
            }
            "maximum" => {
                let maximum = match value {
                    Value::Number(n) => n
                        .as_f64()
                        .ok_or_else(|| anyhow!("maximum must be a number"))?,
                    _ => bail!("maximum must be a number"),
                };
                Ok(Some(Keyword::Assertion(Assertion::Number(
                    NumberAssertion::Maximum(maximum),
                ))))
            }
            "exclusiveMinimum" => {
                // TODO: Handle bools via funky schema preprocessing??
                let exclusive_minimum = match value {
                    Value::Number(n) => n
                        .as_f64()
                        .ok_or_else(|| anyhow!("exclusiveMinimum must be a number"))?,
                    _ => bail!("exclusiveMinimum must be a number"),
                };
                Ok(Some(Keyword::Assertion(Assertion::Number(
                    NumberAssertion::ExclusiveMinimum(exclusive_minimum),
                ))))
            }
            "exclusiveMaximum" => {
                // TODO: Handle bools via funky schema preprocessing??
                let exclusive_maximum = match value {
                    Value::Number(n) => n
                        .as_f64()
                        .ok_or_else(|| anyhow!("exclusiveMaximum must be a number"))?,
                    _ => bail!("exclusiveMaximum must be a number"),
                };
                Ok(Some(Keyword::Assertion(Assertion::Number(
                    NumberAssertion::ExclusiveMaximum(exclusive_maximum),
                ))))
            }
            "multipleOf" => {
                let num = match value {
                    Value::Number(n) => n
                        .as_f64()
                        .ok_or_else(|| anyhow!("multipleOf must be a number"))?,
                    _ => bail!("multipleOf must be a number"),
                };
                let d = Decimal::try_from(num)?;
                Ok(Some(Keyword::Assertion(Assertion::Number(
                    NumberAssertion::MultipleOf(d),
                ))))
            }
            s => {
                if META_AND_ANNOTATIONS.contains(&s) {
                    // Ok(Some(Keyword::Annotation(s.to_string())))
                    Ok(None)
                } else if !draft.is_known_keyword(&key) {
                    Ok(None)
                } else {
                    Err(anyhow!("Unimplemented keyword: {}", key))
                }
            }
        }
    }
}

impl TypeAssertion {
    pub fn accept_type(&self, tp: &Type) -> Result<()> {
        match self {
            TypeAssertion::Type(t) => {
                if t == tp {
                    Ok(())
                } else {
                    bail!("Expected type {}, got {}", t.as_str(), tp.as_str())
                }
            }
            TypeAssertion::Types(types) => {
                if types.iter().any(|t| t == tp) {
                    Ok(())
                } else {
                    bail!(
                        "Expected one of types {:?}, got {}",
                        types.iter().map(Type::as_str).collect::<Vec<_>>(),
                        tp.as_str()
                    )
                }
            }
        }
    }
}

impl Default for TypeAssertion {
    fn default() -> Self {
        TypeAssertion::Types(vec![
            Type::Null,
            Type::Boolean,
            Type::Number,
            Type::String,
            Type::Array,
            Type::Object,
        ])
    }
}

impl Type {
    fn from_str(s: &str) -> Result<Type> {
        match s {
            "null" => Ok(Type::Null),
            "boolean" => Ok(Type::Boolean),
            "number" => Ok(Type::Number),
            "integer" => Ok(Type::Integer),
            "string" => Ok(Type::String),
            "array" => Ok(Type::Array),
            "object" => Ok(Type::Object),
            _ => bail!("Invalid type: {}", s),
        }
    }
    fn as_str(&self) -> &'static str {
        match self {
            Type::Null => "null",
            Type::Boolean => "boolean",
            Type::Number => "number",
            Type::Integer => "integer",
            Type::String => "string",
            Type::Array => "array",
            Type::Object => "object",
        }
    }
}
