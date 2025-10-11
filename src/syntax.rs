mod error;
mod location;
mod name;
mod parse;
#[cfg(test)]
mod parser_tests;
pub mod tokens;

pub use crate::syntax::error::ParserError;
use crate::syntax::parse::Parser;
use crate::syntax::tokens::Lexer;
use internment::ArcIntern;
pub use location::{Located, Location};
use memmap2::Mmap;
pub use name::Name;
use proptest_derive::Arbitrary;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub struct Universe {
    pub files: HashMap<PathBuf, Mmap>,
    pub modules: HashMap<PathBuf, Module>,
}

impl Default for Universe {
    fn default() -> Self {
        Universe {
            files: HashMap::new(),
            modules: HashMap::new(),
        }
    }
}

impl Universe {
    pub fn add_file<P: AsRef<Path>>(&mut self, file: P) -> Result<(), ParserError> {
        let filename = file.as_ref().to_string_lossy().into_owned();

        let file_handle = std::fs::File::open(&file)
            .map_err(|e| ParserError::OpenError {
                file: filename.clone(),
                error: e,
            })?;
        let contents = unsafe { Mmap::map(&file_handle) }
            .map_err(|e| ParserError::ReadError {
                file: filename.clone(),
                error: e,
            })?;
        let string_contents = std::str::from_utf8(&contents)
            .map_err(|e| ParserError::Utf8Error {
                file: filename.clone(),
                error: e,
            })?;

        let lexer = Lexer::from(string_contents);
        let mut parser = Parser::new(&file, lexer);
        let module = parser.parse_module()?;
        self.modules.insert(file.as_ref().to_path_buf(), module);

        Ok(())
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

impl Located for Definition {
    fn location(&self) -> Location {
        self.location.clone()
    }
}

#[derive(Debug)]
pub enum Def {
    Enumeration(EnumerationDef),
    Structure(StructureDef),
    Function(FunctionDef),
    Value(ValueDef),
}

impl Located for Def {
    fn location(&self) -> Location {
        match self {
            Def::Enumeration(def) => def.location.clone(),
            Def::Structure(def) => def.location.clone(),
            Def::Function(def) => def.location.clone(),
            Def::Value(def) => def.location.clone(),
        }
    }
}

#[derive(Debug)]
pub struct EnumerationDef {
    name: String,
    location: Location,
    variants: Vec<EnumerationVariant>,
}

#[derive(Debug)]
pub struct EnumerationVariant {
    location: Location,
    name: String,
    argument: Option<Type>,
}

#[derive(Debug)]
pub struct StructureDef {
    name: String,
    location: Location,
    fields: Vec<StructureField>,
}

#[derive(Debug)]
pub struct StructureField {
    location: Location,
    export: ExportClass,
    name: String,
    field_type: Option<Type>,
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
    value: Expression,
}

#[derive(Debug)]
pub enum ExportClass {
    Public,
    Private,
}

#[derive(Debug)]
pub enum Statement {
    Binding(BindingStmt),
    Expression(Expression),
}

#[derive(Debug)]
pub struct BindingStmt {
    location: Location,
    mutable: bool,
    variable: Name,
    value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Value(ConstantValue),
    Reference(Name),
    EnumerationValue(Name, Name, Option<Box<Expression>>),
    StructureValue(Name, Vec<FieldValue>),
    Conditional(ConditionalExpr),
    Call(Box<Expression>, CallKind, Vec<Expression>),
    Block(Location, Vec<Statement>),
}

#[derive(Debug)]
pub struct ConditionalExpr {
    location: Location,
    test: Box<Expression>,
    consequent: Box<Expression>,
    alternative: Option<Box<Expression>>,
}

#[derive(Debug)]
pub enum CallKind {
    Infix,
    Normal,
    Postfix,
    Prefix,
}

#[derive(Debug)]
pub struct FieldValue {
    field: Name,
    value: Expression,
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
    constructor: Type,
    arguments: Vec<Type>,
}

#[derive(Debug)]
pub enum Type {
    Constructor(Location, String),
    Variable(Location, String),
    Primitive(Location, String),
    Application(Box<Type>, Vec<Type>),
    Function(Vec<Type>, Box<Type>),
}

impl Located for Type {
    fn location(&self) -> Location {
        match self {
            Type::Constructor(l, _) => l.clone(),
            Type::Variable(l, _) => l.clone(),
            Type::Primitive(l, _) => l.clone(),
            Type::Application(t1, ts) => {
                let mut result = t1.location();
                if let Some(last) = ts.last() {
                    result = result.extend_to(&last.location());
                }
                result
            }
            Type::Function(args, ret) => {
                if let Some(first) = args.first() {
                    first.location().extend_to(&ret.location())
                } else {
                    ret.location()
                }
            }
        }
    }
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
