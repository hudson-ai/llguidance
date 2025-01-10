use crate::json::numeric::Decimal;
use derivre::RegexAst;
use indexmap::{IndexMap, IndexSet};

use super::preschema::{
    Applicator, ArrayAssertion, Assertion, Keyword, NumberAssertion, PreSchema, StringAssertion,
    Type, TypeAssertion,
};

#[derive(Debug, Clone)]
pub enum Schema {
    Any,
    Unsatisfiable {
        reason: String,
    },
    Null,
    Boolean,
    Number {
        minimum: Option<f64>,
        maximum: Option<f64>,
        exclusive_minimum: Option<f64>,
        exclusive_maximum: Option<f64>,
        multiple_of: Option<Decimal>,
        integer: bool,
    },
    String {
        min_length: u64,
        max_length: Option<u64>,
        regex: Option<RegexAst>,
    },
    Array {
        min_items: u64,
        max_items: Option<u64>,
        prefix_items: Vec<Schema>,
        items: Option<Box<Schema>>,
    },
    Object {
        properties: IndexMap<String, Schema>,
        additional_properties: Option<Box<Schema>>,
        required: IndexSet<String>,
    },
    LiteralBool {
        value: bool,
    },
    AnyOf {
        options: Vec<Schema>,
    },
    OneOf {
        options: Vec<Schema>,
    },
    Ref {
        uri: String,
    },
}

impl Schema {
    fn from_preschema(ps: PreSchema) -> Self {
        Schema::Any.with_preschema(ps)
    }

    fn with_preschema(self, ps: PreSchema) -> Self {
        match ps {
            PreSchema::Boolean(true) => self,
            PreSchema::Boolean(false) => Schema::Unsatisfiable {
                reason: "false schema".to_string(),
            },
            PreSchema::Schema(kwds) => {
                // TODO/Hypothesis: asserting type first will speed things up quite a bit
                kwds.into_iter().fold(self, |schema, kwd| {
                    match kwd {
                        Keyword::Assertion(a) => {
                            println!("Asserting: {:?}", a);
                            let s = schema.assert(a);
                            println!("Result: {:?}", s);
                            s
                        }
                        Keyword::Applicator(a) => {
                            println!("Applying: {:?}", a);
                            let s = schema.apply(a);
                            println!("Result: {:?}", s);
                            s
                        }
                        // Keyword::Annotation(_) => schema,
                    }
                })
            }
        }
    }

    fn from_type(tp: &Type) -> Self {
        match tp {
            Type::Null => Schema::Null,
            Type::Boolean => Schema::Boolean,
            Type::Number => Schema::Number {
                minimum: None,
                maximum: None,
                exclusive_minimum: None,
                exclusive_maximum: None,
                multiple_of: None,
                integer: false,
            },
            Type::Integer => Schema::Number {
                minimum: None,
                maximum: None,
                exclusive_minimum: None,
                exclusive_maximum: None,
                multiple_of: None,
                integer: true,
            },
            Type::String => Schema::String {
                min_length: 0,
                max_length: None,
                regex: None,
            },
            Type::Array => Schema::Array {
                min_items: 0,
                max_items: None,
                prefix_items: Vec::new(),
                items: None,
            },
            Type::Object => Schema::Object {
                properties: IndexMap::new(),
                additional_properties: None,
                required: IndexSet::new(),
            },
        }
    }

    fn from_type_assertion(ta: &TypeAssertion) -> Self {
        match ta {
            TypeAssertion::Type(t) => Self::from_type(t),
            TypeAssertion::Types(t) => {
                if t.is_empty() {
                    Schema::Unsatisfiable {
                        reason: "Empty type array".to_string(),
                    }
                } else {
                    let options = t.into_iter().map(Self::from_type).collect();
                    Schema::AnyOf { options }
                }
            }
        }
    }

    fn assert_type(self, ta: &TypeAssertion) -> Self {
        let result = match self {
            Schema::Any => return Self::from_type_assertion(ta),
            Schema::Unsatisfiable { .. } => return self,
            Schema::Null => ta.accept_type(&Type::Null),
            Schema::Boolean => ta.accept_type(&Type::Boolean),
            Schema::LiteralBool { .. } => ta.accept_type(&Type::Boolean),
            Schema::Number {
                minimum,
                maximum,
                exclusive_minimum,
                exclusive_maximum,
                multiple_of,
                integer: false,
            } => {
                match (
                    ta.accept_type(&Type::Number),
                    ta.accept_type(&Type::Integer),
                ) {
                    (Err(e), Err(_)) => {
                        // Both failed
                        return Schema::Unsatisfiable {
                            reason: e.to_string(),
                        };
                    }
                    (Ok(()), _) => {
                        // Number OK
                        return Schema::Number {
                            minimum,
                            maximum,
                            exclusive_minimum,
                            exclusive_maximum,
                            multiple_of,
                            integer: false,
                        };
                    }
                    (Err(_), Ok(())) => {
                        // Downgrade to integer
                        return Schema::Number {
                            minimum,
                            maximum,
                            exclusive_minimum,
                            exclusive_maximum,
                            multiple_of,
                            integer: true,
                        };
                    }
                }
            }
            Schema::Number { integer: true, .. } => ta.accept_type(&Type::Integer),
            Schema::String { .. } => ta.accept_type(&Type::String),
            Schema::Array { .. } => ta.accept_type(&Type::Array),
            Schema::Object { .. } => ta.accept_type(&Type::Object),
            Schema::AnyOf { options } => {
                return Schema::AnyOf {
                    options: options
                        .into_iter()
                        .map(|s| s.assert_type(ta))
                        .collect(),
                }
            }
            Schema::OneOf { options } => {
                return Schema::OneOf {
                    options: options
                        .into_iter()
                        .map(|s| s.assert_type(ta))
                        .collect(),
                }
            }
            Schema::Ref { .. } => todo!(),
        };
        match result {
            Ok(()) => self,
            Err(e) => Schema::Unsatisfiable {
                reason: e.to_string(),
            },
        }
    }

    fn assert(self, assertion: Assertion) -> Self {
        match self {
            Schema::Any => {
                // Need to turn any into a concrete anyOf
                return Schema::from_type_assertion(&TypeAssertion::default()).assert(assertion)
            },
            Schema::AnyOf { options } => return Schema::AnyOf {
                options: options
                    .into_iter()
                    .map(|s| s.assert(assertion.clone()))
                    .collect(),
            },
            Schema::OneOf { options } => return Schema::OneOf {
                options: options
                    .into_iter()
                    .map(|s| s.assert(assertion.clone()))
                    .collect(),
            },
            _ => {}
        };
        match assertion {
            Assertion::Type(ta) => self.assert_type(&ta),
            Assertion::Const(_) => todo!(),
            Assertion::Enum(_) => todo!(),
            Assertion::Number(na) => match self {
                Schema::Number {
                    mut minimum,
                    mut maximum,
                    mut exclusive_minimum,
                    mut exclusive_maximum,
                    mut multiple_of,
                    integer,
                } => {
                    match na {
                        NumberAssertion::Minimum(m) => {
                            minimum = minimum.map_or(Some(m), |v| Some(v.max(m)));
                        }
                        NumberAssertion::Maximum(m) => {
                            maximum = maximum.map_or(Some(m), |v| Some(v.min(m)));
                        }
                        NumberAssertion::ExclusiveMinimum(m) => {
                            exclusive_minimum =
                                exclusive_minimum.map_or(Some(m), |v| Some(v.max(m)));
                        }
                        NumberAssertion::ExclusiveMaximum(m) => {
                            exclusive_maximum =
                                exclusive_maximum.map_or(Some(m), |v| Some(v.min(m)));
                        }
                        NumberAssertion::MultipleOf(m) => {
                            multiple_of = multiple_of.map_or(Some(m.clone()), |v| Some(v.lcm(&m)));
                        }
                    }
                    Schema::Number {
                        minimum,
                        maximum,
                        exclusive_minimum,
                        exclusive_maximum,
                        multiple_of,
                        integer,
                    }
                }
                _ => self,
            },
            Assertion::String(sa) => match self {
                Schema::String {
                    mut min_length,
                    mut max_length,
                    mut regex,
                } => {
                    match sa {
                        StringAssertion::MinLength(m) => {
                            min_length = min_length.max(m);
                        }
                        StringAssertion::MaxLength(m) => {
                            max_length = max_length.map_or(Some(m), |v| Some(v.min(m)));
                        }
                        StringAssertion::Pattern(p) => {
                            let r = RegexAst::Regex(p);
                            regex =
                                regex.map_or(Some(r.clone()), |v| Some(RegexAst::And(vec![v, r])));
                        }
                        StringAssertion::Format(_) => todo!(),
                    };
                    Schema::String {
                        min_length,
                        max_length,
                        regex,
                    }
                }
                _ => self,
            },
            Assertion::Array(aa) => match self {
                Schema::Array {
                    mut min_items,
                    mut max_items,
                    prefix_items,
                    items,
                } => {
                    match aa {
                        ArrayAssertion::MinItems(m) => {
                            min_items = min_items.max(m);
                        }
                        ArrayAssertion::MaxItems(m) => {
                            max_items = max_items.map_or(Some(m), |v| Some(v.min(m)));
                        }
                    };
                    Schema::Array {
                        min_items,
                        max_items,
                        prefix_items,
                        items,
                    }
                }
                _ => self,
            },
        }
    }

    fn apply(self, applicator: Applicator) -> Self {
        match self {
            Schema::Any => {
                // Need to turn any into a concrete anyOf
                return Schema::from_type_assertion(&TypeAssertion::default()).apply(applicator)
            },
            Schema::AnyOf { options } => return Schema::AnyOf {
                options: options
                    .into_iter()
                    .map(|s| s.apply(applicator.clone()))
                    .collect(),
            },
            Schema::OneOf { options } => return Schema::OneOf {
                options: options
                    .into_iter()
                    .map(|s| s.apply(applicator.clone()))
                    .collect(),
            },
            _ => {}
        };
        match applicator {
            Applicator::AllOf(schemas) => {
                schemas.into_iter().fold(self, |schema, ps| schema.with_preschema(ps))
            },
            Applicator::AnyOf(schemas) => Schema::AnyOf {
                options: schemas.into_iter().map(|ps| self.clone().with_preschema(ps)).collect::<Vec<_>>(),
            },
            Applicator::OneOf(schemas) => Schema::OneOf {
                options: schemas.into_iter().map(|ps| self.clone().with_preschema(ps)).collect::<Vec<_>>(),
            },
            Applicator::Required(applied_required) => match self {
                Schema::Object {
                    properties: props1,
                    additional_properties,
                    mut required,
                } => {
                    required.extend(applied_required);
                    Schema::Object {
                        properties: props1,
                        additional_properties,
                        required
                    }
                }
                _ => self,
            },
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Schema, PreSchema};
    use referencing::Draft;
    use serde_json::json;

    #[test]
    fn simple() {
        let draft = Draft::Draft202012;
        let schema = json!({
            "type": "integer",
            "minimum": 0,
            "maximum": 10,
        });
        let ps = PreSchema::from_value(draft, schema).unwrap();
        println!("PreSchema: {:?}", ps);
        let schema = Schema::from_preschema(ps);
        println!("Schema: {:?}", schema);
    }

    #[test]
    fn allOf() {
        let draft = Draft::Draft202012;
        let schema = json!({
            "allOf": [
                {
                    "type": ["string", "number"],
                    "minimum": 0,
                    "maximum": 10,
                },
                {
                    "type": "integer",
                    "minimum": 5,
                    "maximum": 15,
                },
            ]
        });
        let ps = PreSchema::from_value(draft, schema).unwrap();
        println!("PreSchema: {:?}", ps);
        let schema = Schema::from_preschema(ps);
        println!("Schema: {:?}", schema);
    }

    #[test]
    fn required() {
        let draft = Draft::Draft202012;
        let schema = json!({
            "allOf": [
                {
                    "type": "object",
                    "required": ["a", "b", "c"]
                },
                {
                    "type": "object",
                    "required": ["b", "a", "g", "h"]
                },
            ]
        });
        let ps = PreSchema::from_value(draft, schema).unwrap();
        println!("PreSchema: {:?}", ps);
        let schema = Schema::from_preschema(ps);
        println!("Schema: {:?}", schema);
    }
}