use crate::syntax::ast::{ConstantValue, Type};
#[cfg(test)]
use crate::syntax::parse::Parser;
#[cfg(test)]
use crate::syntax::tokens::Lexer;
use pretty::{DocAllocator, Pretty};

impl<'a, D: ?Sized + DocAllocator<'a, A>, A: 'a> Pretty<'a, D, A> for Type {
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, A> {
        match self {
            Type::Constructor(_, n) => allocator.as_string(n),
            Type::Variable(_, n) => allocator.as_string(n),
            Type::Primitive(_, n) => allocator.text("prim%").append(allocator.as_string(n)),

            Type::Application(c, args) => c
                .pretty(allocator)
                .append(allocator.space())
                .append(allocator.intersperse(args, " ")),

            Type::Function(args, ret) => allocator
                .intersperse(args, " ")
                .append(allocator.space())
                .append(ret.pretty(allocator)),
        }
    }
}

impl<'a, D: ?Sized + DocAllocator<'a, A>, A: 'a> Pretty<'a, D, A> for ConstantValue {
    fn pretty(self, allocator: &'a D) -> pretty::DocBuilder<'a, D, A> {
        match self {
            ConstantValue::String(_, x) => allocator.text(format!("{x:?}")),
            ConstantValue::Character(_, c) => allocator.text(format!("{c:?}")),
            ConstantValue::Integer(_, iwb) => match iwb.base {
                None => allocator.as_string(iwb.value),
                Some(2) => allocator.text(format!("0b{:b}", iwb.value)),
                Some(8) => allocator.text(format!("0o{:o}", iwb.value)),
                Some(10) => allocator.text(format!("0d{}", iwb.value)),
                Some(16) => allocator.text(format!("0x{:x}", iwb.value)),
                Some(x) => panic!("Illegal base {x} for integer constant."),
            },
        }
    }
}

proptest::proptest! {
    #[test]
    fn constants(x: ConstantValue) {
        let allocator: pretty::Arena = pretty::Arena::new();
        let docbuilder = x.clone().pretty(&allocator);
        let mut string_version = String::new();
        docbuilder.render_fmt(80, &mut string_version).expect("can render to string");
        let lexer = Lexer::from(string_version.as_str());
        let mut parser = Parser::new("test", lexer);
        let roundtripped = parser.parse_constant().expect("can parse constant");
        proptest::prop_assert_eq!(x, roundtripped);
    }

//    #[test]
//    fn types(x: Type) {
//        let allocator: pretty::Arena = pretty::Arena::new();
//        let docbuilder = x.clone().pretty(&allocator);
//        let mut string_version = String::new();
//        docbuilder.render_fmt(80, &mut string_version).expect("can render to string");
//        println!("String version: {string_version:?}");
//        let lexer = Lexer::from(string_version.as_str());
//        let mut parser = Parser::new("test", lexer);
//        let roundtripped = parser.parse_type().expect("can parse constant");
//        proptest::prop_assert_eq!(x, roundtripped);
//    }
}
