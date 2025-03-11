use anyhow::{anyhow, bail, Result};
use indexmap::{IndexMap, IndexSet};
use serde_json::{Map, Value};
use std::collections::HashSet;

#[derive(Debug)]
enum PreSchema {
    Bool(bool),
    Definition(SchemaDefinition),
}

#[derive(Debug)]
struct SchemaDefinition {
    type_: Option<Type>,
    keywords: Vec<Keyword>,
}

#[derive(Debug)]
struct Type {
    null: bool,
    boolean: bool,
    number: bool,
    string: bool,
    array: bool,
    object: bool,
}

#[derive(Debug)]
enum Keyword {
    Assertion(Assertion),
    Applicator(Applicator),
}

#[derive(Debug)]
enum Assertion {
    String(StringAssertion),
    Number(NumberAssertion),
    Object(ObjectAssertion),
    Array(ArrayAssertion),
}

#[derive(Debug)]
enum StringAssertion {
    MinLength(u64),
    MaxLength(u64),
    Pattern(String),
    Format(String),
}

#[derive(Debug)]
enum NumberAssertion {
    Minimum(f64),
    Maximum(f64),
    ExclusiveMinimum(ExclusiveBound),
    ExclusiveMaximum(ExclusiveBound),
    MultipleOf(f64),
    // Integer,
}

#[derive(Debug)]
enum ExclusiveBound {
    Bool(bool), // For legacy Draft 4 support
    Number(f64),
}

#[derive(Debug)]
enum ObjectAssertion {
    Required(IndexSet<String>),
}

#[derive(Debug)]
enum ArrayAssertion {
    MinItems(u64),
    MaxItems(u64),
}

/// https://json-schema.org/draft/2020-12/json-schema-core#section-10
#[derive(Debug)]
enum Applicator {
    InPlace(InPlaceApplicator),
    ChildInstance(ChildInstanceApplicator),
}

/// https://json-schema.org/draft/2020-12/json-schema-core#section-10.2
#[derive(Debug)]
enum InPlaceApplicator {
    AllOf(Vec<PreSchema>),
    AnyOf(Vec<PreSchema>),
    OneOf(Vec<PreSchema>),
    Ref(String),
}

/// https://json-schema.org/draft/2020-12/json-schema-core#section-10.3
#[derive(Debug)]
enum ChildInstanceApplicator {
    Array(ArrayApplicator),
    Object(ObjectApplicator),
}

/// https://json-schema.org/draft/2020-12/json-schema-core#section-10.3.1
#[derive(Debug)]
enum ArrayApplicator {
    PrefixItems(Vec<PreSchema>),
    Items(ItemsApplicator),
}

#[derive(Debug)]
struct ItemsApplicator {
    items: PreSchema,
    // number of prefix items that don't need to validate against the items schema
    prefix_items: usize,
}

/// https://json-schema.org/draft/2020-12/json-schema-core#section-10.3.2
#[derive(Debug)]
enum ObjectApplicator {
    Properties(IndexMap<String, PreSchema>),
    AdditionalProperties(AdditionalPropertiesApplicator),
}

#[derive(Debug)]
struct AdditionalPropertiesApplicator {
    additional_properties: PreSchema,
    // properties that don't need to validate against the additional properties schema
    properties: HashSet<String>,
}

impl TryFrom<&Value> for PreSchema {
    type Error = anyhow::Error;

    fn try_from(value: &Value) -> Result<Self> {
        match value {
            Value::Bool(b) => Ok(PreSchema::Bool(*b)),
            Value::Object(map) => {
                let inner = SchemaDefinition::try_from(map)?;
                Ok(PreSchema::Definition(inner))
            }
            _ => bail!("Schema must be a boolean or an object"),
        }
    }
}

impl TryFrom<&Map<String, Value>> for SchemaDefinition {
    type Error = anyhow::Error;

    fn try_from(map: &Map<String, Value>) -> Result<Self> {
        let mut type_ = None;
        let mut keywords = vec![];

        for (key, value) in map.iter() {
            match key.as_str() {
                // Core
                "type" => {
                    type_ = Some(Type::try_from(value)?);
                }
                // Assertions
                // String
                "minLength" => {
                    let min_length = value.as_u64().ok_or_else(|| {
                        anyhow!("Expected minLength to be a non-negative integer")
                    })?;
                    keywords.push(Keyword::Assertion(Assertion::String(
                        StringAssertion::MinLength(min_length),
                    )));
                }
                "maxLength" => {
                    let max_length = value.as_u64().ok_or_else(|| {
                        anyhow!("Expected maxLength to be a non-negative integer")
                    })?;
                    keywords.push(Keyword::Assertion(Assertion::String(
                        StringAssertion::MaxLength(max_length),
                    )));
                }
                "pattern" => {
                    let pattern = value
                        .as_str()
                        .ok_or_else(|| anyhow!("Expected pattern to be a string"))?;
                    keywords.push(Keyword::Assertion(Assertion::String(
                        StringAssertion::Pattern(pattern.to_string()),
                    )));
                }
                "format" => {
                    let format = value
                        .as_str()
                        .ok_or_else(|| anyhow!("Expected format to be a string"))?;
                    keywords.push(Keyword::Assertion(Assertion::String(
                        StringAssertion::Format(format.to_string()),
                    )));
                }
                // Number
                "minimum" => {
                    let minimum = value
                        .as_f64()
                        .ok_or_else(|| anyhow!("Expected minimum to be a number"))?;
                    keywords.push(Keyword::Assertion(Assertion::Number(
                        NumberAssertion::Minimum(minimum),
                    )));
                }
                "maximum" => {
                    let maximum = value
                        .as_f64()
                        .ok_or_else(|| anyhow!("Expected maximum to be a number"))?;
                    keywords.push(Keyword::Assertion(Assertion::Number(
                        NumberAssertion::Maximum(maximum),
                    )));
                }
                "exclusiveMinimum" => match value {
                    Value::Bool(b) => {
                        keywords.push(Keyword::Assertion(Assertion::Number(
                            NumberAssertion::ExclusiveMinimum(ExclusiveBound::Bool(*b)),
                        )));
                    }
                    Value::Number(n) => {
                        let exclusive_minimum = n
                            .as_f64()
                            .ok_or_else(|| anyhow!("Expected exclusiveMinimum to be a number"))?;
                        keywords.push(Keyword::Assertion(Assertion::Number(
                            NumberAssertion::ExclusiveMinimum(ExclusiveBound::Number(
                                exclusive_minimum,
                            )),
                        )));
                    }
                    _ => bail!("Expected exclusiveMinimum to be a boolean or a number"),
                },
                "exclusiveMaximum" => match value {
                    Value::Bool(b) => {
                        keywords.push(Keyword::Assertion(Assertion::Number(
                            NumberAssertion::ExclusiveMaximum(ExclusiveBound::Bool(*b)),
                        )));
                    }
                    Value::Number(n) => {
                        let exclusive_maximum = n
                            .as_f64()
                            .ok_or_else(|| anyhow!("Expected exclusiveMaximum to be a number"))?;
                        keywords.push(Keyword::Assertion(Assertion::Number(
                            NumberAssertion::ExclusiveMaximum(ExclusiveBound::Number(
                                exclusive_maximum,
                            )),
                        )));
                    }
                    _ => bail!("Expected exclusiveMaximum to be a boolean or a number"),
                },
                "multipleOf" => todo!("decimal"),
                // Object
                "required" => {
                    let required = value
                        .as_array()
                        .ok_or_else(|| anyhow!("Expected required to be an array"))?
                        .iter()
                        .map(|v| {
                            v.as_str()
                                .ok_or_else(|| {
                                    anyhow!("Expected required to be an array of strings")
                                })
                                .and_then(|s| Ok(s.to_string()))
                        })
                        .collect::<Result<IndexSet<_>>>()?;
                    keywords.push(Keyword::Assertion(Assertion::Object(
                        ObjectAssertion::Required(required),
                    )));
                }
                // Array
                "minItems" => {
                    let min_items = value
                        .as_u64()
                        .ok_or_else(|| anyhow!("Expected minItems to be a non-negative integer"))?;
                    keywords.push(Keyword::Assertion(Assertion::Array(
                        ArrayAssertion::MinItems(min_items),
                    )));
                }
                "maxItems" => {
                    let max_items = value
                        .as_u64()
                        .ok_or_else(|| anyhow!("Expected maxItems to be a non-negative integer"))?;
                    keywords.push(Keyword::Assertion(Assertion::Array(
                        ArrayAssertion::MaxItems(max_items),
                    )));
                }
                // Applicators
                // In-place
                "allOf" => {
                    let all_of = value
                        .as_array()
                        .ok_or_else(|| anyhow!("Expected allOf to be an array"))?
                        .iter()
                        .map(|v| PreSchema::try_from(v))
                        .collect::<Result<Vec<_>>>()?;
                    keywords.push(Keyword::Applicator(Applicator::InPlace(
                        InPlaceApplicator::AllOf(all_of),
                    )));
                }
                "anyOf" => {
                    let any_of = value
                        .as_array()
                        .ok_or_else(|| anyhow!("Expected anyOf to be an array"))?
                        .iter()
                        .map(|v| PreSchema::try_from(v))
                        .collect::<Result<Vec<_>>>()?;
                    keywords.push(Keyword::Applicator(Applicator::InPlace(
                        InPlaceApplicator::AnyOf(any_of),
                    )));
                }
                "oneOf" => {
                    let one_of = value
                        .as_array()
                        .ok_or_else(|| anyhow!("Expected oneOf to be an array"))?
                        .iter()
                        .map(|v| PreSchema::try_from(v))
                        .collect::<Result<Vec<_>>>()?;
                    keywords.push(Keyword::Applicator(Applicator::InPlace(
                        InPlaceApplicator::OneOf(one_of),
                    )));
                }
                "ref" => {
                    let ref_ = value
                        .as_str()
                        .ok_or_else(|| anyhow!("Expected $ref to be a string"))?;
                    keywords.push(Keyword::Applicator(Applicator::InPlace(
                        InPlaceApplicator::Ref(ref_.to_string()),
                    )));
                }
                // Child instance
                // Object
                "properties" => {
                    let properties = value
                        .as_object()
                        .ok_or_else(|| anyhow!("Expected properties to be an object"))?
                        .iter()
                        .map(|(k, v)| {
                            Ok((
                                k.to_string(),
                                PreSchema::try_from(v).map_err(|e| {
                                    anyhow!("Failed to parse schema for property {}: {}", k, e)
                                })?,
                            ))
                        })
                        .collect::<Result<IndexMap<_, _>>>()?;
                    keywords.push(Keyword::Applicator(Applicator::ChildInstance(
                        ChildInstanceApplicator::Object(ObjectApplicator::Properties(properties)),
                    )));
                }
                "additionalProperties" => {
                    let additional_properties = PreSchema::try_from(value)?;
                    let properties = if let Some(properties) = map.get("properties") {
                        properties
                            .as_object()
                            .ok_or_else(|| anyhow!("Expected properties to be an object"))?
                            .keys()
                            .cloned()
                            .collect::<HashSet<_>>()
                    } else {
                        HashSet::new()
                    };
                    keywords.push(Keyword::Applicator(Applicator::ChildInstance(
                        ChildInstanceApplicator::Object(ObjectApplicator::AdditionalProperties(
                            AdditionalPropertiesApplicator {
                                additional_properties,
                                properties,
                            },
                        )),
                    )));
                }
                // Array
                "prefixItems" => {
                    // TODO: old-draft support
                    let prefix_items = value
                        .as_array()
                        .ok_or_else(|| anyhow!("Expected prefixItems to be an array"))?
                        .iter()
                        .map(|v| PreSchema::try_from(v))
                        .collect::<Result<Vec<_>>>()?;
                    keywords.push(Keyword::Applicator(Applicator::ChildInstance(
                        ChildInstanceApplicator::Array(ArrayApplicator::PrefixItems(prefix_items)),
                    )));
                }
                "items" => {
                    let items = PreSchema::try_from(value)?;
                    let prefix_items = if let Some(prefix_items) = map.get("prefixItems") {
                        prefix_items
                            .as_array()
                            .ok_or_else(|| anyhow!("Expected prefixItems to be an array"))?
                            .len()
                    } else {
                        0
                    };
                    keywords.push(Keyword::Applicator(Applicator::ChildInstance(
                        ChildInstanceApplicator::Array(ArrayApplicator::Items(ItemsApplicator {
                            items,
                            prefix_items,
                        })),
                    )));
                }
                _ => todo!(),
            }
        }

        Ok(SchemaDefinition { type_, keywords })
    }
}

impl TryFrom<&Value> for Type {
    type Error = anyhow::Error;

    fn try_from(value: &Value) -> Result<Self> {
        // TODO: bit flags?
        let mut type_ = Type {
            null: false,
            boolean: false,
            number: false,
            string: false,
            array: false,
            object: false,
        };
        let types = match value {
            Value::Array(arr) => arr
                .iter()
                .map(|v| {
                    v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                        anyhow!(
                            "Expected type array to be array of strings, found non-string value"
                        )
                    })
                })
                .collect::<Result<Vec<_>>>()?,
            Value::String(s) => vec![s.to_string()],
            _ => bail!("Expected type to be a string or an array of strings"),
        };
        for t in types {
            match t.as_str() {
                "null" => type_.null = true,
                "boolean" => type_.boolean = true,
                "number" => type_.number = true,
                "string" => type_.string = true,
                "array" => type_.array = true,
                "object" => type_.object = true,
                _ => bail!("Unknown type: {}", t),
            }
        }
        Ok(type_)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_parse_schema() {
        let schema = json!({
            "type": ["object", "null"],
            "properties": {
                "name": {
                    "type": "string",
                    "minLength": 1,
                    "maxLength": 100,
                    "pattern": "^\\w+$"
                },
                "age": {
                    "type": "number",
                    "minimum": 0,
                    "maximum": 120
                }
            },
            "required": ["name"],
            "additionalProperties": false
        });

        let pre_schema = PreSchema::try_from(&schema).unwrap();
        println!("{:#?}", pre_schema);
    }
}
