use lalrpop_util::lalrpop_mod;

mod error;
lalrpop_mod!(
    #[allow(clippy::just_underscores_and_digits, clippy::clone_on_copy)]
    parser,
    "/syntax/parser.rs"
);
pub mod tokens;

#[cfg(test)]
use crate::syntax::error::ParserError;
#[cfg(test)]
use crate::syntax::parser::*;
#[cfg(test)]
use crate::syntax::tokens::Lexer;
use codespan_reporting::diagnostic::Label;
use proptest_derive::Arbitrary;
use std::cmp::{max, min};
use std::fmt::Debug;
use std::ops::Range;

#[derive(Debug)]
pub struct Location {
    file_id: usize,
    span: Range<usize>,
}

impl Location {
    pub fn new(file_id: usize, span: Range<usize>) -> Self {
        Location { file_id, span }
    }

    pub fn extend_to(&self, other: &Location) -> Location {
        assert_eq!(self.file_id, other.file_id);
        Location {
            file_id: self.file_id,
            span: min(self.span.start, other.span.start)..max(self.span.end, other.span.end),
        }
    }

    pub fn primary_label(&self) -> Label<usize> {
        Label::primary(self.file_id, self.span.clone())
    }

    pub fn secondary_label(&self) -> Label<usize> {
        Label::secondary(self.file_id, self.span.clone())
    }
}

#[derive(Debug)]
pub struct Module {
    definitions: Vec<Definition>,
}

#[derive(Debug)]
pub struct Definition {
    location: Location,
    export: ExportClass,
    type_restrictions: TypeRestrictions,
    definition: Def,
}

#[derive(Debug)]
pub enum Def {
    Enumeration(EnumerationDef),
    Structure(StructureDef),
    Function(FunctionDef),
    Value(ValueDef),
}

impl Def {
    fn location(&self) -> &Location {
        match self {
            Def::Enumeration(def) => &def.location,
            Def::Structure(def) => &def.location,
            Def::Function(def) => &def.location,
            Def::Value(def) => &def.location,
        }
    }
}

#[derive(Debug)]
pub struct EnumerationDef {
    location: Location,
    options: Vec<EnumerationVariant>,
}

#[derive(Debug)]
pub struct EnumerationVariant {
    location: Location,
    name: String,
    arguments: Vec<Type>,
}

#[derive(Debug)]
pub struct StructureDef {
    name: String,
    location: Location,
    fields: Vec<StructureField>,
}

#[derive(Debug)]
pub struct StructureField {
    name: String,
    field_type: Type,
}

#[derive(Debug)]
pub struct FunctionDef {
    name: String,
    location: Location,
    arguments: Vec<FunctionArg>,
    return_type: Option<Type>,
    body: Vec<Statement>,
}

#[derive(Debug)]
pub struct FunctionArg {
    name: String,
    arg_type: Option<Type>,
}

#[derive(Debug)]
pub struct ValueDef {
    name: String,
    location: Location,
    value: Value,
}

#[derive(Debug)]
pub enum ExportClass {
    Public,
    Private,
}

#[derive(Debug)]
pub enum Statement {
    Binding(BindingStmt),
}

#[derive(Debug)]
pub struct BindingStmt {
    location: Location,
    mutable: bool,
    variable: String,
    value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Value(Value),
}

#[derive(Debug)]
pub struct TypeRestrictions {
    restrictions: Vec<TypeRestriction>,
}

impl TypeRestrictions {
    fn empty() -> Self {
        TypeRestrictions {
            restrictions: vec![],
        }
    }
}

#[derive(Debug)]
pub struct TypeRestriction {
    location: Location,
    class: String,
    variables: Vec<String>,
}

#[derive(Debug)]
pub enum Type {
    Constructor(Location, String),
    Variable(Location, String),
    Primitive(Location, String),
    Application(Box<Type>, Vec<Type>),
    Function(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub enum Value {
    Constant(ConstantValue),
}

#[derive(Debug)]
pub enum ConstantValue {
    Integer(Location, IntegerWithBase),
    Character(Location, char),
    String(Location, String),
}

#[derive(Clone, Debug, PartialEq, Eq, Arbitrary)]
pub struct IntegerWithBase {
    #[proptest(strategy = "proptest::prop_oneof![ \
        proptest::strategy::Just(None), \
        proptest::strategy::Just(Some(2)), \
        proptest::strategy::Just(Some(8)), \
        proptest::strategy::Just(Some(10)), \
        proptest::strategy::Just(Some(16)), \
    ]")]
    base: Option<u8>,
    value: u64,
}

#[test]
fn can_parse_constants() {
    let parse_constant = |str| {
        let lexer = Lexer::from(str).map(|item| {
            item.map_err(|e| ParserError::LexerError {
                file_id: 0,
                error: e,
            })
        });
        let result = ConstantValueParser::new().parse(0, lexer);
        result
    };

    assert!(matches!(
        parse_constant("16"),
        Ok(ConstantValue::Integer(
            _,
            IntegerWithBase {
                base: None,
                value: 16,
            }
        ))
    ));
    assert!(matches!(
        parse_constant("0x10"),
        Ok(ConstantValue::Integer(
            _,
            IntegerWithBase {
                base: Some(16),
                value: 16,
            }
        ))
    ));
    assert!(matches!(
        parse_constant("0o20"),
        Ok(ConstantValue::Integer(
            _,
            IntegerWithBase {
                base: Some(8),
                value: 16,
            }
        ))
    ));
    assert!(matches!(
        parse_constant("0b10000"),
        Ok(ConstantValue::Integer(
            _,
            IntegerWithBase {
                base: Some(2),
                value: 16,
            }
        ))
    ));
    assert!(
        matches!(parse_constant("\"foo\""), Ok(ConstantValue::String(_, x))
            if x == "foo")
    );
    assert!(matches!(
        parse_constant("'f'"),
        Ok(ConstantValue::Character(_, 'f'))
    ));
}

#[test]
fn can_parse_types() {
    let parse_type = |str| {
        let lexer = Lexer::from(str).map(|item| {
            item.map_err(|e| ParserError::LexerError {
                file_id: 0,
                error: e,
            })
        });
        let result = TypeParser::new().parse(0, lexer);
        result
    };

    println!("cons result: {:?}", parse_type("Cons"));
    assert!(matches!(
        parse_type("Cons"),
        Ok(Type::Application(cons, empty)) if 
            matches!(cons.as_ref(), Type::Constructor(_, c) if c == "Cons") &&
            empty.is_empty()
    ));
    assert!(matches!(
        parse_type("cons"),
        Ok(Type::Variable(_, c)) if c == "cons"
    ));
    assert!(matches!(
        parse_type("Cons a b"),
        Ok(Type::Application(a, b))
            if matches!(a.as_ref(), Type::Constructor(_, c) if c == "Cons") &&
               matches!(b.as_slice(), [Type::Variable(_, b1), Type::Variable(_, b2)]
                   if b1 == "a" && b2 == "b")
    ));
    assert!(matches!(
        parse_type("a -> z"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Variable(_, a1)] if a1 == "a") &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1 == "z")
    ));
    assert!(matches!(
        parse_type("a b -> z"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Variable(_, a1), Type::Variable(_, b1)]
                    if a1 == "a" && b1 == "b") &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1 == "z")
    ));
    assert!(matches!(
        parse_type("Cons a b -> z"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Application(cons, appargs)]
                if matches!(cons.as_ref(), Type::Constructor(_, c) if c == "Cons") &&
                   matches!(appargs.as_slice(), [Type::Variable(_, b1), Type::Variable(_, b2)]
                       if b1 == "a" && b2 == "b")) &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1 == "z")
    ));
}
