use crate::syntax::location::{Located, Location};
use crate::syntax::name::Name;
use proptest_derive::Arbitrary;

#[derive(Debug)]
pub struct Module {
    pub definitions: Vec<Definition>,
}

#[derive(Debug)]
pub struct Definition {
    pub location: Location,
    pub export: ExportClass,
    pub type_restrictions: TypeRestrictions,
    pub definition: Def,
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
    Operator(OperatorDef),
}

impl Located for Def {
    fn location(&self) -> Location {
        match self {
            Def::Enumeration(def) => def.location.clone(),
            Def::Structure(def) => def.location.clone(),
            Def::Function(def) => def.location.clone(),
            Def::Value(def) => def.location.clone(),
            Def::Operator(def) => def.location.clone(),
        }
    }
}

#[derive(Debug)]
pub struct EnumerationDef {
    pub name: Name,
    pub location: Location,
    pub variants: Vec<EnumerationVariant>,
}

#[derive(Debug)]
pub struct EnumerationVariant {
    pub location: Location,
    pub name: Name,
    pub argument: Option<Type>,
}

#[derive(Debug)]
pub struct StructureDef {
    pub name: Name,
    pub location: Location,
    pub fields: Vec<StructureField>,
}

#[derive(Debug)]
pub struct StructureField {
    pub location: Location,
    pub export: ExportClass,
    pub name: Name,
    pub field_type: Option<Type>,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Name,
    pub location: Location,
    pub arguments: Vec<FunctionArg>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct FunctionArg {
    pub name: Name,
    pub arg_type: Option<Type>,
}

#[derive(Debug)]
pub struct ValueDef {
    pub name: Name,
    pub location: Location,
    pub mtype: Option<Type>,
    pub value: Expression,
}

#[derive(Debug)]
pub struct OperatorDef {
    pub operator_name: Name,
    pub location: Location,
    pub function_name: Name,
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
    pub location: Location,
    pub mutable: bool,
    pub variable: Name,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Value(ConstantValue),
    Reference(Location, Name),
    Enumeration(EnumerationExpr),
    Structure(StructureExpr),
    Conditional(ConditionalExpr),
    Match(MatchExpr),
    Call(Box<Expression>, CallKind, Vec<Expression>),
    Block(Location, Vec<Statement>),
}

impl Located for Expression {
    fn location(&self) -> Location {
        match self {
            Expression::Value(c) => c.location(),
            Expression::Reference(l, _) => l.clone(),
            Expression::Enumeration(ev) => ev.location.clone(),
            Expression::Structure(sv) => sv.location.clone(),
            Expression::Conditional(ce) => ce.location.clone(),
            Expression::Match(me) => me.location.clone(),
            Expression::Call(_, _, _) => unimplemented!(),
            Expression::Block(l, _) => l.clone(),
        }
    }
}

#[derive(Debug)]
pub struct EnumerationExpr {
    pub location: Location,
    pub type_name: Name,
    pub variant_name: Name,
    pub argument: Option<Box<Expression>>,
}

#[derive(Debug)]
pub struct StructureExpr {
    pub location: Location,
    pub type_name: Name,
    pub fields: Vec<FieldValue>,
}

#[derive(Debug)]
pub struct ConditionalExpr {
    pub location: Location,
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternative: Option<Box<Expression>>,
}

#[derive(Debug)]
pub struct MatchExpr {
    pub location: Location,
    pub value: Box<Expression>,
    pub cases: Vec<MatchCase>,
}

#[derive(Debug)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub consequent: Expression,
}

#[derive(Debug)]
pub enum Pattern {
    Constant(ConstantValue),
    Variable(Name),
    EnumerationValue(EnumerationPattern),
    Structure(StructurePattern),
}

#[derive(Debug)]
pub struct EnumerationPattern {
    pub location: Location,
    pub type_name: Name,
    pub variant_name: Name,
    pub argument: Option<Box<Pattern>>,
}

#[derive(Debug)]
pub struct StructurePattern {
    pub location: Location,
    pub type_name: Name,
    pub fields: Vec<(Name, Option<Pattern>)>,
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
    pub field: Name,
    pub value: Expression,
}

#[derive(Debug)]
pub struct TypeRestrictions {
    pub restrictions: Vec<TypeRestriction>,
}

impl TypeRestrictions {
    pub fn empty() -> Self {
        TypeRestrictions {
            restrictions: vec![],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.restrictions.is_empty()
    }
}

#[derive(Debug)]
pub struct TypeRestriction {
    pub constructor: Type,
    pub arguments: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Constructor(Location, Name),
    Variable(Location, Name),
    Primitive(Location, Name),
    Application(Box<Type>, Vec<Type>),
    Function(Box<Type>, Box<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        println!("self: {self:?}");
        println!("other: {other:?}");

        if matches!(other, Type::Application(con, args) if args.is_empty() && self == con.as_ref())
        {
            return true;
        }

        match self {
            Type::Constructor(_, x) => matches!(other, Type::Constructor(_, y) if x == y),
            Type::Variable(_, x) => matches!(other, Type::Variable(_, y) if x == y),
            Type::Primitive(_, x) => matches!(other, Type::Primitive(_, y) if x == y),
            Type::Application(con1, args1) => {
                matches!(other, Type::Application(con2, args2) if con1 == con2 && args1 == args2)
                    || (con1.as_ref() == other && args1.is_empty())
            }
            Type::Function(args1, ret1) => {
                matches!(other, Type::Function(args2, ret2) if args1 == args2 && ret1 == ret2)
            }
        }
    }
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
            Type::Function(arg, ret) => arg.location().extend_to(&ret.location()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ConstantValue {
    Integer(Location, IntegerWithBase),
    Character(Location, char),
    String(Location, String),
}

impl Located for ConstantValue {
    fn location(&self) -> Location {
        match self {
            ConstantValue::Integer(l, _) => l.clone(),
            ConstantValue::Character(l, _) => l.clone(),
            ConstantValue::String(l, _) => l.clone(),
        }
    }
}

impl PartialEq for ConstantValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ConstantValue::Character(_, x) => {
                matches!(other, ConstantValue::Character(_, y) if x == y)
            }
            ConstantValue::String(_, x) => matches!(other, ConstantValue::String(_, y) if x == y),
            ConstantValue::Integer(_, x) => matches!(other, ConstantValue::Integer(_, y) if x == y),
        }
    }
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
    pub base: Option<u8>,
    pub value: u64,
}
