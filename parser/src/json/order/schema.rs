use indexmap::{IndexMap, IndexSet};

use super::keywords::PreSchema;

enum Schema {
    Any,
    Unsatisfiable(String),
    Simple(Simple),
    Product(Product),
    Reference(String),
}

enum Simple {
    Null,
    Boolean(Option<bool>),
    Number(NumberType),
    String(StringType),
    Object(ObjectType),
    Array(ArrayType),
}

enum Product {
    AnyOf(Vec<Schema>),
    OneOf(Vec<Schema>),
}

enum Concrete {
    Simple(Simple),
    Product(Product),
}

struct NumberType {
    minimum: Option<f64>,
    maximum: Option<f64>,
    exclusive_minimum: Option<bool>,
    exclusive_maximum: Option<bool>,
    multiple_of: Option<f64>,
    integer: bool,
}
impl Default for NumberType {
    fn default() -> Self {
        NumberType {
            minimum: None,
            maximum: None,
            exclusive_minimum: None,
            exclusive_maximum: None,
            multiple_of: None,
            integer: false,
        }
    }
}

struct StringType {
    min_length: usize,
    max_length: Option<usize>,
    pattern: Option<String>,
}
impl Default for StringType {
    fn default() -> Self {
        StringType {
            min_length: 0,
            max_length: None,
            pattern: None,
        }
    }
}

struct ObjectType {
    properties: IndexMap<String, Schema>,
    required: IndexSet<String>,
    additional_properties: Box<Schema>,
    min_properties: Option<usize>,
    max_properties: Option<usize>,
}
impl Default for ObjectType {
    fn default() -> Self {
        ObjectType {
            properties: IndexMap::new(),
            required: IndexSet::new(),
            additional_properties: Box::new(Schema::Any),
            min_properties: None,
            max_properties: None,
        }
    }
}

struct ArrayType {
    prefix_items: Vec<Schema>,
    items: Box<Schema>,
    min_items: Option<usize>,
    max_items: Option<usize>,
}
impl Default for ArrayType {
    fn default() -> Self {
        ArrayType {
            prefix_items: Vec::new(),
            items: Box::new(Schema::Any),
            min_items: None,
            max_items: None,
        }
    }
}

impl From<PreSchema> for Schema {
    fn from(pre_schema: PreSchema) -> Self {
        match pre_schema {
            PreSchema::Bool(true) => Schema::Any,
            PreSchema::Bool(false) => Schema::Unsatisfiable("false".to_string()),
            PreSchema::Definition(definition) => todo!(),
        }
    }
}
