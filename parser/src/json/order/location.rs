use std::{array, collections::VecDeque, ops::Deref};

use anyhow::{anyhow, bail, Result};
use indexmap::{IndexMap, IndexSet};
use serde_json::{value::Index, Map, Value};

use crate::json::schema::Schema;

pub trait SchemaStatus: Clone {}

#[derive(Debug, Clone)]
pub struct Initial {}
impl SchemaStatus for Initial {}

#[derive(Debug, Clone)]
pub struct Applying {}
impl SchemaStatus for Applying {}

#[derive(Debug, Clone)]
pub struct Finalized {}
impl SchemaStatus for Finalized {}

#[derive(Debug, Clone)]
pub struct SchemaNode<S: SchemaStatus> {
    pub inner: SchemaInner<S>,
    pub state: S,
}

#[derive(Debug, Clone)]
pub enum SchemaInner<S: SchemaStatus> {
    True,
    False(String),
    Simple(Simple),
    Product(Box<Product<S>>),
    Collection(Box<Collection<S>>),
}

#[derive(Debug, Clone)]
pub enum Simple {
    Null,
    Boolean {
        literal: Option<bool>,
    },
    Number {
        minimum: Option<f64>,
        maximum: Option<f64>,
        exclusive_minimum: Option<f64>,
        exclusive_maximum: Option<f64>,
        multiple_of: Option<f64>,
        integer: bool,
    },
    String {
        min_length: usize,
        max_length: Option<usize>,
        pattern: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub enum Product<S: SchemaStatus> {
    AnyOf(Vec<SchemaNode<S>>),
    OneOf(Vec<SchemaNode<S>>),
}

#[derive(Debug, Clone)]
pub enum Collection<S: SchemaStatus> {
    Object {
        required: IndexSet<String>,
        properties: IndexMap<String, SchemaNode<S>>,
        additional_properties: SchemaNode<S>,
    },
    Array {
        min_items: usize,
        max_items: Option<usize>,
        prefix_items: Vec<SchemaNode<S>>,
        items: SchemaNode<S>,
    },
}

pub enum Assertion {
    String(StringAssertion),
    Number(NumberAssertion),
    // Type(TypeAssertion),
}

pub enum StringAssertion {
    MinLength(usize),
    MaxLength(usize),
    Pattern(String),
    Format(String),
}

pub enum NumberAssertion {
    Minimum(f64),
    Maximum(f64),
    ExclusiveMinimum(f64),
    ExclusiveMaximum(f64),
    MultipleOf(f64),
    Integer,
}

#[derive(Debug)]
pub struct TypeAssertion(Vec<Type>);

#[derive(PartialEq, Debug)]
pub enum Type {
    String,
    Number,
    Boolean,
    Null,
    Object,
    Array,
}

const ALL_TYPES: [Type; 6] = [
    Type::String,
    Type::Number,
    Type::Boolean,
    Type::Null,
    Type::Object,
    Type::Array,
];

impl Simple {
    fn assert(&mut self, assertion: &Assertion) {
        match (assertion, self) {
            (
                Assertion::Number(assertion),
                Simple::Number {
                    ref mut minimum,
                    ref mut maximum,
                    ref mut exclusive_minimum,
                    ref mut exclusive_maximum,
                    ref mut multiple_of,
                    ref mut integer,
                },
            ) => match assertion {
                NumberAssertion::Minimum(value) => {
                    *minimum = Some(minimum.map_or(*value, |m| m.max(*value)))
                }
                NumberAssertion::Maximum(value) => {
                    *maximum = Some(maximum.map_or(*value, |m| m.min(*value)))
                }
                NumberAssertion::ExclusiveMinimum(value) => {
                    *exclusive_minimum = Some(exclusive_minimum.map_or(*value, |m| m.min(*value)))
                }
                NumberAssertion::ExclusiveMaximum(value) => {
                    *exclusive_maximum = Some(exclusive_maximum.map_or(*value, |m| m.max(*value)))
                }
                NumberAssertion::MultipleOf(value) => todo!("Use decimal with lcm"),
                NumberAssertion::Integer => {
                    *integer = true;
                }
            },
            (
                Assertion::String(assertion),
                Simple::String {
                    ref mut min_length,
                    ref mut max_length,
                    ref mut pattern,
                },
            ) => match assertion {
                StringAssertion::MinLength(value) => {
                    *min_length = (*min_length).max(*value);
                }
                StringAssertion::MaxLength(value) => {
                    *max_length = max_length.map_or(Some(*value), |m| Some(m.min(*value)));
                }
                StringAssertion::Pattern(_) => todo!(),
                StringAssertion::Format(_) => todo!(),
            },
            _ => {
                // Non-applicable assertions!
            }
        }
    }
}

impl SchemaNode<Applying> {
    fn build(schema: &Value) -> Result<SchemaNode<Finalized>> {
        let mut initial = SchemaNode {
            inner: SchemaInner::True,
            state: Applying {},
        };
        initial.apply(schema)?;
        initial.finalize()
    }

    fn apply(&mut self, schema: &Value) -> Result<()> {
        if let Some(bool) = schema.as_bool() {
            if !bool {
                self.inner = SchemaInner::False("false schema".to_string());
            }
            return Ok(());
        }
        let schema_map = schema
            .as_object()
            .ok_or_else(|| anyhow!("Schema must be an object or boolean"))?;

        // Make early type assertion as it will short-circuit a lot of other assertions
        if let Some(value) = schema_map.get("type") {
            let type_strings = if let Some(array) = value.as_array() {
                array
                    .iter()
                    .map(|v| {
                        v.as_str()
                            .ok_or_else(|| anyhow!("type array must be strings"))
                    })
                    .collect::<Result<Vec<_>>>()?
            } else if let Some(string) = value.as_str() {
                vec![string]
            } else {
                bail!("type must be a string or array of strings")
            };
            let types = type_strings
                .iter()
                .map(|&t| match t {
                    "string" => Ok(Type::String),
                    "number" | "integer" => Ok(Type::Number),
                    "boolean" => Ok(Type::Boolean),
                    "null" => Ok(Type::Null),
                    "object" => Ok(Type::Object),
                    "array" => Ok(Type::Array),
                    _ => Err(anyhow!("Invalid type: {}", t)),
                })
                .collect::<Result<Vec<_>>>()?;
            self.assert_type(&types);
            if type_strings.contains(&"integer") {
                self.assert(&Assertion::Number(NumberAssertion::Integer));
            }
        }

        for (key, value) in schema_map.iter() {
            match key.as_str() {
                // Some assertions
                "type" => {
                    // We did this early (order doesn't matter for result)
                }
                "minimum" => self.assert(&Assertion::Number(NumberAssertion::Minimum(
                    value
                        .as_f64()
                        .ok_or_else(|| anyhow!("minimum must be a number"))?,
                ))),
                "maximum" => todo!("assert maximum"),
                "exclusiveMinimum" => todo!("assert exclusive minimum"),
                "exclusiveMaximum" => todo!("assert exclusive maximum"),
                "multipleOf" => todo!("assert multiple of"),
                "minLength" => todo!("assert min length"),
                "maxLength" => todo!("assert max length"),
                "pattern" => todo!("assert pattern"),
                "format" => todo!("assert format"),
                // Subschema applicators
                "properties" => todo!(),
                "additionalProperties" => todo!(),
                "items" => todo!(),
                "prefixItems" => todo!(),
                // In-place applicators
                "allOf" => {
                    let array = value
                        .as_array()
                        .ok_or_else(|| anyhow!("allOf must be an array"))?;
                    for schema in array {
                        self.apply(schema)?;
                    }
                }
                "anyOf" => {
                    let array = value
                        .as_array()
                        .ok_or_else(|| anyhow!("anyOf must be an array"))?;
                    let nodes = array
                        .iter()
                        .map(|schema| {
                            let mut node = self.clone();
                            node.apply(schema)?;
                            Ok(node)
                        })
                        .collect::<Result<Vec<_>>>()?;
                    self.inner = SchemaInner::Product(Box::new(Product::AnyOf(nodes)));
                }
                "oneOf" => {
                    let array = value
                        .as_array()
                        .ok_or_else(|| anyhow!("oneOf must be an array"))?;
                    let nodes = array
                        .iter()
                        .map(|schema| {
                            let mut node = self.clone();
                            node.apply(schema)?;
                            Ok(node)
                        })
                        .collect::<Result<Vec<_>>>()?;
                    self.inner = SchemaInner::Product(Box::new(Product::OneOf(nodes)));
                }
                "$ref" => {
                    todo!("Need to resolve reference and apply it in place *if it hasn't been applied yet (recursive)*")
                }
                _ => todo!(),
            }
        }
        Ok(())
        // self.state.applicators.push(schema);
    }

    fn assert(&mut self, assertion: &Assertion) {
        match self.inner {
            SchemaInner::False(_) => return,
            SchemaInner::True => {
                self.inner = concrete_any();
                self.assert(assertion);
            }
            SchemaInner::Simple(ref mut simple) => {
                simple.assert(assertion);
            }
            SchemaInner::Product(_) => todo!(),
            SchemaInner::Collection(_) => todo!(),
        }
    }

    fn assert_type(&mut self, types: &[Type]) {
        if types.is_empty() {
            self.inner = SchemaInner::False("No types".to_string());
            return;
        }
        match self.inner {
            SchemaInner::False(_) => return,
            SchemaInner::True => {
                if ALL_TYPES.iter().all(|t| types.contains(t)) {
                    return;
                }
                self.inner = concrete_any();
                self.assert_type(types);
            }
            SchemaInner::Simple(ref simple) => {
                for t in types {
                    match (t, simple) {
                        (Type::String, Simple::String { .. }) => return,
                        (Type::Number, Simple::Number { .. }) => return,
                        (Type::Boolean, Simple::Boolean { .. }) => return,
                        (Type::Null, Simple::Null) => return,
                        _ => {}
                    }
                }
                self.inner =
                    SchemaInner::False(format!("Expected one of {:?}, found {:?}", types, simple));
            }
            SchemaInner::Collection(ref collection) => {
                for t in types {
                    match (t, collection.deref()) {
                        (Type::Object, Collection::Object { .. }) => return,
                        (Type::Array, Collection::Array { .. }) => return,
                        _ => {}
                    }
                }
                self.inner = SchemaInner::False(format!(
                    "Expected one of {:?}, found {:?}",
                    types, collection
                ));
            }
            SchemaInner::Product(ref product) => {
                todo!()
            }
        }
    }

    fn finalize(self) -> Result<SchemaNode<Finalized>> {
        todo!()
    }
}

impl Product<Applying> {
    fn assert(&mut self, assertion: &Assertion) {
        match self {
            Product::AnyOf(options) | Product::OneOf(options) => {
                for option in options {
                    option.assert(assertion);
                }
            }
        }
    }
}

impl Collection<Applying> {
    fn assert(&mut self, assertion: &Assertion) {
        todo!()
    }
}

fn concrete_any() -> SchemaInner<Applying> {
    SchemaInner::Product(Box::new(Product::AnyOf(vec![
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Null),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Boolean { literal: None }),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Number {
                minimum: None,
                maximum: None,
                exclusive_minimum: None,
                exclusive_maximum: None,
                multiple_of: None,
                integer: false,
            }),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::String {
                min_length: 0,
                max_length: None,
                pattern: None,
            }),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Collection(Box::new(Collection::Object {
                required: IndexSet::new(),
                properties: IndexMap::new(),
                additional_properties: SchemaNode {
                    inner: SchemaInner::True,
                    state: Applying {},
                },
            })),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Collection(Box::new(Collection::Array {
                min_items: 0,
                max_items: None,
                prefix_items: vec![],
                items: SchemaNode {
                    inner: SchemaInner::True,
                    state: Applying {},
                },
            })),
            state: Applying {},
        },
    ])))
}
