use std::ops::Deref;

use anyhow::{anyhow, bail, Result};
use indexmap::{IndexMap, IndexSet};
use serde_json::Value;

pub trait SchemaStatus: Clone {}

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
    Simple(Simple<S>),
    Product(Product<S>),
}

#[derive(Debug, Clone)]
pub enum Simple<S: SchemaStatus> {
    Null,
    Boolean(Option<bool>),
    Number(NumberType),
    String(StringType),
    Object(ObjectType<S>),
    Array(ArrayType<S>),
}

#[derive(Debug, Clone)]
struct NumberType {
    minimum: Option<f64>,
    maximum: Option<f64>,
    exclusive_minimum: Option<f64>,
    exclusive_maximum: Option<f64>,
    multiple_of: Option<f64>,
    integer: bool,
}

#[derive(Debug, Clone)]
struct StringType {
    min_length: usize,
    max_length: Option<usize>,
    pattern: Option<String>,
}

#[derive(Debug, Clone)]
struct ObjectType<S: SchemaStatus> {
    required: IndexSet<String>,
    properties: IndexMap<String, SchemaNode<S>>,
    additional_properties: Box<SchemaNode<S>>,
}

#[derive(Debug, Clone)]
struct ArrayType<S: SchemaStatus> {
    min_items: usize,
    max_items: Option<usize>,
    prefix_items: Vec<SchemaNode<S>>,
    items: Box<SchemaNode<S>>,
}

#[derive(Debug, Clone)]
pub enum Product<S: SchemaStatus> {
    AnyOf(Vec<SchemaNode<S>>),
    OneOf(Vec<SchemaNode<S>>),
}

pub enum Assertion {
    String(StringAssertion),
    Number(NumberAssertion),
    Object(ObjectAssertion),
    Array(ArrayAssertion),
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

pub enum ObjectAssertion {
    // TODO
}

pub enum ArrayAssertion {
    // TODO
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

impl NumberType {
    fn assert(&mut self, assertion: &NumberAssertion) {
        match assertion {
            NumberAssertion::Minimum(value) => {
                self.minimum = Some(self.minimum.map_or(*value, |m| m.max(*value)))
            }
            NumberAssertion::Maximum(value) => {
                self.maximum = Some(self.maximum.map_or(*value, |m| m.min(*value)))
            }
            NumberAssertion::ExclusiveMinimum(value) => {
                self.exclusive_minimum =
                    Some(self.exclusive_minimum.map_or(*value, |m| m.min(*value)))
            }
            NumberAssertion::ExclusiveMaximum(value) => {
                self.exclusive_maximum =
                    Some(self.exclusive_maximum.map_or(*value, |m| m.max(*value)))
            }
            NumberAssertion::MultipleOf(value) => todo!("Use decimal with lcm"),
            NumberAssertion::Integer => {
                self.integer = true;
            }
        }
    }
}

impl StringType {
    fn assert(&mut self, assertion: &StringAssertion) {
        match assertion {
            StringAssertion::MinLength(value) => {
                self.min_length = self.min_length.max(*value);
            }
            StringAssertion::MaxLength(value) => {
                self.max_length = self
                    .max_length
                    .map_or(Some(*value), |m| Some(m.min(*value)));
            }
            StringAssertion::Pattern(_) => todo!(),
            StringAssertion::Format(_) => todo!(),
        }
    }
}

impl ObjectType<Applying> {
    fn assert(&mut self, assertion: &ObjectAssertion) {
        todo!()
    }
}

impl ArrayType<Applying> {
    fn assert(&mut self, assertion: &ArrayAssertion) {
        todo!()
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
                    self.inner = SchemaInner::Product(Product::AnyOf(nodes));
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
                    self.inner = SchemaInner::Product(Product::OneOf(nodes));
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
            SchemaInner::False(_) => {}
            SchemaInner::True => {
                self.inner = concrete_any();
                self.assert(assertion);
            }
            SchemaInner::Simple(ref mut simple) => match simple {
                Simple::Null => {}
                Simple::Boolean(_) => {}
                Simple::Number(ref mut number) => {
                    if let Assertion::Number(assertion) = assertion {
                        number.assert(assertion);
                    }
                }
                Simple::String(ref mut string) => {
                    if let Assertion::String(assertion) = assertion {
                        string.assert(assertion);
                    }
                }
                Simple::Object(ref mut object) => {
                    if let Assertion::Object(assertion) = assertion {
                        object.assert(assertion);
                    }
                }
                Simple::Array(ref mut array) => {
                    if let Assertion::Array(assertion) = assertion {
                        array.assert(assertion);
                    }
                }
            },
            SchemaInner::Product(ref mut product) => match product {
                Product::AnyOf(options) | Product::OneOf(options) => {
                    for option in options {
                        option.assert(assertion);
                    }
                }
            },
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
                        (Type::Null, Simple::Null) => return,
                        (Type::Boolean, Simple::Boolean { .. }) => return,
                        (Type::Number, Simple::Number { .. }) => return,
                        (Type::String, Simple::String { .. }) => return,
                        (Type::Object, Simple::Object { .. }) => return,
                        (Type::Array, Simple::Array { .. }) => return,
                        _ => {}
                    }
                }
                self.inner =
                    SchemaInner::False(format!("Expected one of {:?}, found {:?}", types, simple));
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

fn concrete_any() -> SchemaInner<Applying> {
    SchemaInner::Product(Product::AnyOf(vec![
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Null),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Boolean(None)),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Number(NumberType {
                minimum: None,
                maximum: None,
                exclusive_minimum: None,
                exclusive_maximum: None,
                multiple_of: None,
                integer: false,
            })),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::String(StringType {
                min_length: 0,
                max_length: None,
                pattern: None,
            })),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Object(ObjectType {
                required: IndexSet::new(),
                properties: IndexMap::new(),
                additional_properties: Box::new(SchemaNode {
                    inner: SchemaInner::True,
                    state: Applying {},
                }),
            })),
            state: Applying {},
        },
        SchemaNode {
            inner: SchemaInner::Simple(Simple::Array(ArrayType {
                min_items: 0,
                max_items: None,
                prefix_items: vec![],
                items: Box::new(SchemaNode {
                    inner: SchemaInner::True,
                    state: Applying {},
                }),
            })),
            state: Applying {},
        },
    ]))
}
