use crate::syntax::error::ParserError;
use crate::syntax::parse::Parser;
use crate::syntax::tokens::{Lexer, Token};
use crate::syntax::*;

#[test]
fn constants() {
    let parse_constant = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_constant()
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
fn types() {
    let parse_type = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_type()
    };

    assert!(matches!(
        parse_type("Cons"),
        Ok(Type::Application(cons, empty)) if
            matches!(cons.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Cons") &&
            empty.is_empty()
    ));
    assert!(matches!(
        parse_type("cons"),
        Ok(Type::Variable(_, c)) if c.as_printed() == "cons"
    ));
    assert!(matches!(
        parse_type("Cons a b"),
        Ok(Type::Application(a, b))
            if matches!(a.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Cons") &&
               matches!(b.as_slice(), [Type::Variable(_, b1), Type::Variable(_, b2)]
                   if b1.as_printed() == "a" && b2.as_printed() == "b")
    ));
    assert!(matches!(
        parse_type("a -> z"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Variable(_, a1)] if a1.as_printed() == "a") &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1.as_printed() == "z")
    ));
    assert!(matches!(
        parse_type("(a -> z)"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Variable(_, a1)] if a1.as_printed() == "a") &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1.as_printed() == "z")
    ));
    assert!(matches!(
        parse_type("a b -> z"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Variable(_, a1), Type::Variable(_, b1)]
                    if a1.as_printed() == "a" && b1.as_printed() == "b") &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1.as_printed() == "z")
    ));
    assert!(matches!(
        parse_type("Cons a b -> z"),
        Ok(Type::Function(a, z))
            if matches!(a.as_slice(), [Type::Application(cons, appargs)]
                if matches!(cons.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Cons") &&
                   matches!(appargs.as_slice(), [Type::Variable(_, b1), Type::Variable(_, b2)]
                       if b1.as_printed() == "a" && b2.as_printed() == "b")) &&
               matches!(z.as_ref(), Type::Variable(_, z1) if z1.as_printed() == "z")
    ));
}

#[test]
fn type_restrictions() {
    let parse_tr = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_type_restrictions()
    };

    assert!(matches!(
        parse_tr("restrict()"),
        Ok(TypeRestrictions{ restrictions }) if restrictions.is_empty()
    ));

    assert!(matches!(
        parse_tr("restrict(Cons a b)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 1 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x.as_printed() == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x.as_printed() == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x.as_printed() == "b"))));

    assert!(matches!(
        parse_tr("restrict(Cons a b,)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 1 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x.as_printed() == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x.as_printed() == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x.as_printed() == "b"))));

    assert!(parse_tr("restrict(,Cons a b,)").is_err());

    assert!(matches!(
        parse_tr("restrict(Cons a b, Monad m)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 2 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x.as_printed() == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x.as_printed() == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x.as_printed() == "b")) &&
          matches!(&restrictions[1], TypeRestriction {
              constructor,
              arguments,
          } if matches!(constructor, Type::Constructor(_, x) if x.as_printed() == "Monad") &&
             arguments.len() == 1 &&
             matches!(&arguments[0], Type::Variable(_, x) if x.as_printed() == "m"))));

    assert!(matches!(
        parse_tr("restrict(Cons a b, Monad m,)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 2 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x.as_printed() == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x.as_printed() == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x.as_printed() == "b")) &&
          matches!(&restrictions[1], TypeRestriction {
              constructor,
              arguments,
          } if matches!(constructor, Type::Constructor(_, x) if x.as_printed() == "Monad") &&
             arguments.len() == 1 &&
             matches!(&arguments[0], Type::Variable(_, x) if x.as_printed() == "m"))));
}

#[test]
fn field_definition() {
    let parse_fd = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_field_definition()
    };

    assert!(parse_fd("foo").is_err());
    assert!(matches!(
        parse_fd("foo,"),
        Ok(Some(StructureField{ name, export: ExportClass::Private, field_type: None, .. }))
          if name.as_printed() == "foo"
    ));
    assert!(matches!(
        parse_fd("foo}"),
        Ok(Some(StructureField{ name, export: ExportClass::Private, field_type: None, .. }))
          if name.as_printed() == "foo"
    ));

    assert!(matches!(
        parse_fd("foo: Word8,"),
        Ok(Some(StructureField{ name, field_type, .. }))
          if name.as_printed() == "foo" &&
             matches!(&field_type, Some(Type::Application(c, args))
                 if matches!(c.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Word8") &&
                    args.is_empty())));

    assert!(matches!(
        parse_fd("foo: Cons a b,"),
        Ok(Some(StructureField{ name, field_type, .. }))
          if name.as_printed() == "foo" &&
             matches!(&field_type, Some(Type::Application(c, args))
                 if matches!(c.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Cons") &&
                    matches!(&args.as_slice(), &[Type::Variable(_, v1), Type::Variable(_, v2)]
                        if v1.as_printed() == "a" && v2.as_printed() == "b"))));

    assert!(matches!(
        parse_fd("foo: a -> b,"),
        Ok(Some(StructureField{ name, field_type, .. }))
          if name.as_printed() == "foo" &&
             matches!(&field_type, Some(Type::Function(args, ret))
                 if matches!(&args.as_slice(), &[Type::Variable(_, a)] if a.as_printed() == "a") &&
                    matches!(ret.as_ref(), Type::Variable(_, b) if b.as_printed() == "b"))));

    assert!(matches!(
        parse_fd("export foo: a -> b,"),
        Ok(Some(StructureField{ name, export: ExportClass::Public, field_type, .. }))
          if name.as_printed() == "foo" &&
             matches!(&field_type, Some(Type::Function(args, ret))
                 if matches!(&args.as_slice(), &[Type::Variable(_, a)] if a.as_printed() == "a") &&
                    matches!(ret.as_ref(), Type::Variable(_, b) if b.as_printed() == "b"))));
}

#[test]
fn structures() {
    let parse_st = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_structure()
    };

    assert!(parse_st("structure { }").is_err());
    assert!(parse_st("structure {").is_err());
    assert!(parse_st("structure foo {}").is_err());

    println!("result: {:?}", parse_st("structure Foo {}"));
    assert!(matches!(
        parse_st("structure Foo {}"),
        Ok(StructureDef { name, fields, .. })
          if name.as_printed() == "Foo" && fields.is_empty()));

    assert!(matches!(
        parse_st("structure Foo { bar }"),
        Ok(StructureDef { name, fields, .. })
          if name.as_printed() == "Foo" &&
             matches!(fields.as_slice(), &[StructureField { ref name, ref field_type, .. }]
                 if name.as_printed() == "bar" && field_type.is_none())));

    assert!(matches!(
        parse_st("structure Foo { bar: Word8 }"),
        Ok(StructureDef { name, fields, .. })
          if name.as_printed() == "Foo" &&
             matches!(fields.as_slice(), &[StructureField { ref name, ref field_type, .. }]
                 if name.as_printed() == "bar" &&
                    matches!(field_type, Some(Type::Application(c, args))
                      if matches!(c.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Word8") &&
                         args.is_empty()))));

    assert!(matches!(
        parse_st("structure Foo { bar: Word8, goo }"),
        Ok(StructureDef { name, fields, .. })
          if name.as_printed() == "Foo" &&
             matches!(fields.as_slice(),
               &[StructureField { ref name, ref field_type, .. },
                 StructureField { name: ref name2, field_type: None, .. }]
                 if name.as_printed() == "bar" &&
                    name2.as_printed() == "goo" &&
                    matches!(field_type, Some(Type::Application(c, args))
                      if matches!(c.as_ref(), Type::Constructor(_, c) if c.as_printed() == "Word8") &&
                         args.is_empty()))));

    assert!(matches!(
        parse_st("structure Foo { bar: b c -> a, goo }"),
        Ok(StructureDef { name, fields, .. })
          if name.as_printed() == "Foo" &&
             matches!(fields.as_slice(),
               &[StructureField { ref name, ref field_type, .. },
                 StructureField { name: ref name2, field_type: None, .. }]
                 if name.as_printed() == "bar" &&
                    name2.as_printed() == "goo" &&
                    matches!(field_type, Some(Type::Function(args, ret))
                      if matches!(&args.as_slice(), &[Type::Variable(_, b), Type::Variable(_, c)]
                           if b.as_printed() == "b" && c.as_printed() == "c") &&
                         matches!(ret.as_ref(), Type::Variable(_, a) if a.as_printed() == "a")))));

    assert!(matches!(
        parse_st("structure Foo { bar: b c -> a, goo, }"),
        Ok(StructureDef { name, fields, .. })
          if name.as_printed() == "Foo" &&
             matches!(fields.as_slice(),
               &[StructureField { ref name, ref field_type, .. },
                 StructureField { name: ref name2, field_type: None, .. }]
                 if name.as_printed() == "bar" &&
                    name2.as_printed() == "goo" &&
                    matches!(field_type, Some(Type::Function(args, ret))
                      if matches!(&args.as_slice(), &[Type::Variable(_, b), Type::Variable(_, c)]
                           if b.as_printed() == "b" && c.as_printed() == "c") &&
                         matches!(ret.as_ref(), Type::Variable(_, a) if a.as_printed() == "a")))));
}

#[test]
fn enum_variant() {
    let parse_ev = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_enum_variant()
    };

    assert!(matches!(parse_ev("foo"), Ok(None)));
    assert!(matches!(parse_ev("foo,"), Ok(None)));
    assert!(parse_ev("Cons foo,").is_err());
    assert!(matches!(parse_ev(""), Ok(None)));

    assert!(matches!(parse_ev("}"), Ok(None)));

    assert!(matches!(
      parse_ev("Cons,"),
      Ok(Some(EnumerationVariant { name, argument, .. }))
            if name.as_printed() == "Cons" && argument.is_none()));
    assert!(matches!(
      parse_ev("Cons }"),
      Ok(Some(EnumerationVariant { name, argument, .. }))
            if name.as_printed() == "Cons" && argument.is_none()));
    assert!(matches!(
      parse_ev("Cons, }"),
      Ok(Some(EnumerationVariant { name, argument, .. }))
            if name.as_printed() == "Cons" && argument.is_none()));

    assert!(matches!(
      parse_ev("Cons(Pair a),"),
      Ok(Some(EnumerationVariant { name, ref argument, .. }))
            if name.as_printed() == "Cons" &&
               matches!(argument, Some(Type::Application(typef, args))
                 if matches!(typef.as_ref(), Type::Constructor(_, name)
                       if name.as_printed() == "Pair") &&
                    matches!(&args.as_slice(), &[Type::Variable(_, argname)]
                       if argname.as_printed() == "a"))));
    assert!(matches!(
      parse_ev("Cons(Pair a) }"),
      Ok(Some(EnumerationVariant { name, ref argument, .. }))
            if name.as_printed() == "Cons" &&
               matches!(argument, Some(Type::Application(typef, args))
                 if matches!(typef.as_ref(), Type::Constructor(_, name)
                       if name.as_printed() == "Pair") &&
                    matches!(&args.as_slice(), &[Type::Variable(_, argname)]
                       if argname.as_printed() == "a"))));

    assert!(matches!(
      parse_ev("Cons(a b -> c) }"),
      Ok(Some(EnumerationVariant { name, ref argument, .. }))
            if name.as_printed() == "Cons" &&
               matches!(argument, Some(Type::Function(args, ret))
                 if matches!(&args.as_slice(), &[Type::Variable(_, a), Type::Variable(_, b)]
                      if a.as_printed() == "a" && b.as_printed() == "b") &&
                    matches!(ret.as_ref(), Type::Variable(_, c) if c.as_printed() == "c"))));
}

#[test]
fn enumerations() {
    let parse_en = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_enumeration()
    };

    assert!(parse_en("enumeration { }").is_err());
    assert!(parse_en("enumeration {").is_err());
    assert!(parse_en("enumeration").is_err());

    assert!(matches!(
      parse_en("enumeration Empty { }"),
      Ok(EnumerationDef { name, variants, .. })
        if name.as_printed() == "Empty" && variants.is_empty()));
    assert!(matches!(
      parse_en("enumeration Alternates { A, B }"),
      Ok(EnumerationDef { name, variants, .. })
        if name.as_printed() == "Alternates" &&
           matches!(&variants.as_slice(), &[
               EnumerationVariant { name: name1, argument: arg1, ..},
               EnumerationVariant { name: name2, argument: arg2, ..},
           ] if name1.as_printed() == "A" && arg1.is_none() &&
                name2.as_printed() == "B" && arg2.is_none())));
    assert!(matches!(
      parse_en("enumeration Alternates { A, B, }"),
      Ok(EnumerationDef { name, variants, .. })
        if name.as_printed() == "Alternates" &&
           matches!(&variants.as_slice(), &[
               EnumerationVariant { name: name1, argument: arg1, ..},
               EnumerationVariant { name: name2, argument: arg2, ..},
           ] if name1.as_printed() == "A" && arg1.is_none() &&
                name2.as_printed() == "B" && arg2.is_none())));
}

#[test]
fn expressions() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_expression()
    };

    assert!(parse_ex("").is_err());
    assert!(matches!(
      parse_ex("x"),
      Ok(Expression::Reference(_,n)) if n.as_printed() == "x"));
    assert!(matches!(
      parse_ex("(x)"),
      Ok(Expression::Reference(_,n)) if n.as_printed() == "x"));
    assert!(matches!(
        parse_ex("'c'"),
        Ok(Expression::Value(ConstantValue::Character(_, _)))
    ));
    assert!(matches!(
        parse_ex("\"c\""),
        Ok(Expression::Value(ConstantValue::String(_, _)))
    ));
    assert!(matches!(
        parse_ex("1"),
        Ok(Expression::Value(ConstantValue::Integer(_, _)))
    ));
    assert!(matches!(
        parse_ex("(1)"),
        Ok(Expression::Value(ConstantValue::Integer(_, _)))
    ));
}

#[test]
fn enumeration_values() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_expression()
    };

    assert!(parse_ex("Hello::world").is_err());
    assert!(matches!(
      parse_ex("Hello::World"),
      Ok(Expression::Enumeration(ev))
        if ev.type_name.as_printed() == "Hello" &&
           ev.variant_name.as_printed() == "World" &&
           ev.argument.is_none()));
    assert!(matches!(
      parse_ex("Hello::World(a)"),
      Ok(Expression::Enumeration(ev))
        if ev.type_name.as_printed() == "Hello" &&
           ev.variant_name.as_printed() == "World" &&
           ev.argument.is_some()));
}

#[test]
fn structure_value() {
    let parse_st = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_expression()
    };

    assert!(parse_st("Foo{ , }").is_err());
    assert!(parse_st("Foo{ foo, }").is_err());
    assert!(parse_st("Foo{ foo: , }").is_err());
    assert!(parse_st("Foo{ , foo: 1, }").is_err());
    println!("result: {:?}", parse_st("Foo{ foo: 1 }"));
    assert!(matches!(
      parse_st("Foo{ foo: 1 }"),
      Ok(Expression::Structure(sv))
        if sv.type_name.as_printed() == "Foo" &&
           matches!(sv.fields.as_slice(), [FieldValue{ field, value }]
             if field.as_printed() == "foo" &&
                matches!(value, Expression::Value(ConstantValue::Integer(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1, }"),
      Ok(Expression::Structure(sv))
        if sv.type_name.as_printed() == "Foo" &&
           matches!(sv.fields.as_slice(), [FieldValue{ field, value }]
             if field.as_printed() == "foo" &&
                matches!(value, Expression::Value(ConstantValue::Integer(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1, bar: \"foo\" }"),
      Ok(Expression::Structure(sv))
        if sv.type_name.as_printed() == "Foo" &&
           matches!(sv.fields.as_slice(), [FieldValue{ field: f1, value: v1 },
                                        FieldValue{ field: f2, value: v2 }]
             if f1.as_printed() == "foo" &&
                f2.as_printed() == "bar" &&
                matches!(v1, Expression::Value(ConstantValue::Integer(_,_))) &&
                matches!(v2, Expression::Value(ConstantValue::String(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1, bar: \"foo\", }"),
      Ok(Expression::Structure(sv))
        if sv.type_name.as_printed() == "Foo" &&
           matches!(sv.fields.as_slice(), [FieldValue{ field: f1, value: v1 },
                                        FieldValue{ field: f2, value: v2 }]
             if f1.as_printed() == "foo" &&
                f2.as_printed() == "bar" &&
                matches!(v1, Expression::Value(ConstantValue::Integer(_,_))) &&
                matches!(v2, Expression::Value(ConstantValue::String(_,_))))));
    assert!(parse_st("Foo{ foo: 1,, bar: \"foo\", }").is_err());
}

#[test]
fn infix_and_precedence() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.add_infix_precedence("+", parse::Associativity::Left, 6);
        result.add_infix_precedence("*", parse::Associativity::Right, 7);
        result.parse_expression()
    };

    assert!(matches!(
      parse_ex("0"),
      Ok(Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value, .. })))
          if value == 0));
    assert!(matches!(
      parse_ex("(0)"),
      Ok(Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value, .. })))
          if value == 0));
    assert!(matches!(
      parse_ex("((0))"),
      Ok(Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value, .. })))
          if value == 0));
    assert!(matches!(
      parse_ex("1 + 2"),
      Ok(Expression::Call(plus, CallKind::Infix, args))
        if matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
           matches!(args.as_slice(), [
             Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v1, .. })),
             Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v2, .. }))
           ] if *v1 == 1 && *v2 == 2)));
    assert!(matches!(
      parse_ex("1 + 2 + 3"),
      Ok(Expression::Call(plus, CallKind::Infix, args))
        if matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
           matches!(args.as_slice(), [
             Expression::Call(innerplus, CallKind::Infix, inner_args),
             Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v3, .. }))
           ] if *v3 == 3 &&
             matches!(innerplus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
             matches!(inner_args.as_slice(), [
               Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v1, .. })),
               Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v2, .. }))
             ] if *v1 == 1 && *v2 == 2))));
    assert!(matches!(
      parse_ex("1 * 2 * 3"),
      Ok(Expression::Call(times, CallKind::Infix, args))
        if matches!(times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
           matches!(args.as_slice(), [
             Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v1, .. })),
             Expression::Call(innertimes, CallKind::Infix, inner_args),
           ] if *v1 == 1 &&
             matches!(innertimes.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
             matches!(inner_args.as_slice(), [
               Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v2, .. })),
               Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: v3, .. }))
             ] if *v2 == 2 && *v3 == 3))));

    assert!(matches!(
      parse_ex("1 + 2 * 3 + 4"),
      Ok(Expression::Call(plus_right, CallKind::Infix, outer_args)) if
        matches!(plus_right.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
        matches!(outer_args.as_slice(), [
          Expression::Call(plus_left, CallKind::Infix, left_args),
          Expression::Value(ConstantValue::Integer(_, v4))
        ] if
           matches!(v4, IntegerWithBase{ value: 4, .. }) &&
           matches!(plus_left.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
           matches!(left_args.as_slice(), [
             Expression::Value(ConstantValue::Integer(_, v1)),
             Expression::Call(times, CallKind::Infix, times_args)
           ] if
              matches!(v1, IntegerWithBase{ value: 1, .. }) &&
              matches!(times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
              matches!(times_args.as_slice(), [
                Expression::Value(ConstantValue::Integer(_, v2)),
                Expression::Value(ConstantValue::Integer(_, v3))
              ] if
                matches!(v2, IntegerWithBase{ value: 2, .. }) &&
                matches!(v3, IntegerWithBase{ value: 3, .. }))))));

    assert!(matches!(
      parse_ex("1 * 2 + 3 * 4"),
      Ok(Expression::Call(plus, CallKind::Infix, outer_args)) if
        matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
        matches!(outer_args.as_slice(), [
          Expression::Call(left_times, CallKind::Infix, left_args),
          Expression::Call(right_times, CallKind::Infix, right_args)
        ] if
           matches!(left_times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
           matches!(right_times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
           matches!(left_args.as_slice(), [
             Expression::Value(ConstantValue::Integer(_, v1)),
             Expression::Value(ConstantValue::Integer(_, v2)),
           ] if
              matches!(v1, IntegerWithBase { value: 1, .. }) &&
              matches!(v2, IntegerWithBase { value: 2, .. })) &&
           matches!(right_args.as_slice(), [
             Expression::Value(ConstantValue::Integer(_, v3)),
             Expression::Value(ConstantValue::Integer(_, v4)),
           ] if
              matches!(v3, IntegerWithBase { value: 3, .. }) &&
              matches!(v4, IntegerWithBase { value: 4, .. })))));
}

#[test]
fn calls() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.add_infix_precedence("+", parse::Associativity::Left, 6);
        result.add_infix_precedence("*", parse::Associativity::Right, 7);
        result.parse_expression()
    };

    assert!(matches!(
      parse_ex("f()"),
      Ok(Expression::Call(f, CallKind::Normal, args)) if
        matches!(f.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
        args.is_empty()));
    assert!(matches!(
      parse_ex("f(a)"),
      Ok(Expression::Call(f, CallKind::Normal, args)) if
        matches!(f.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
        matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "a")));
    assert!(matches!(
      parse_ex("f(a,b)"),
      Ok(Expression::Call(f, CallKind::Normal, args)) if
        matches!(f.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
        matches!(args.as_slice(), [
          Expression::Reference(_,a),
          Expression::Reference(_,b),
        ] if a.as_printed() == "a" && b.as_printed() == "b")));
    assert!(matches!(
      parse_ex("f(a,b,)"),
      Ok(Expression::Call(f, CallKind::Normal, args)) if
        matches!(f.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
        matches!(args.as_slice(), [
          Expression::Reference(_,a),
          Expression::Reference(_,b),
        ] if a.as_printed() == "a" && b.as_printed() == "b")));
    assert!(parse_ex("f(,a,b,)").is_err());
    assert!(parse_ex("f(a,,b,)").is_err());
    assert!(parse_ex("f(a,b,,)").is_err());

    assert!(matches!(
      parse_ex("f()()"),
      Ok(Expression::Call(f, CallKind::Normal, args)) if
        matches!(f.as_ref(), Expression::Call(inner, CallKind::Normal, inner_args) if
         matches!(inner.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
         inner_args.is_empty()) &&
        args.is_empty()));

    assert!(matches!(
      parse_ex("f() + 1"),
      Ok(Expression::Call(plus, CallKind::Infix, args)) if
       matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
       matches!(args.as_slice(), [
        Expression::Call(subcall, CallKind::Normal, subargs),
        Expression::Value(ConstantValue::Integer(_, v1))
       ] if
        matches!(v1, IntegerWithBase{ value: 1, .. }) &&
        matches!(subcall.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
        subargs.is_empty())));

    assert!(matches!(
      parse_ex("f(a + b, c*d)"),
      Ok(Expression::Call(eff, CallKind::Normal, args)) if
       matches!(eff.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
       matches!(args.as_slice(), [
        Expression::Call(plus, CallKind::Infix, pargs),
        Expression::Call(times, CallKind::Infix, targs),
       ] if
        matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
        matches!(times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
        matches!(pargs.as_slice(), [ Expression::Reference(_,a), Expression::Reference(_,b) ] if
         a.as_printed() == "a" && b.as_printed() == "b") &&
        matches!(targs.as_slice(), [ Expression::Reference(_,c), Expression::Reference(_,d) ] if
         c.as_printed() == "c" && d.as_printed() == "d"))));

    assert!(matches!(
      parse_ex("f(a + b, c*d,)"),
      Ok(Expression::Call(eff, CallKind::Normal, args)) if
       matches!(eff.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
       matches!(args.as_slice(), [
        Expression::Call(plus, CallKind::Infix, pargs),
        Expression::Call(times, CallKind::Infix, targs),
       ] if
        matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
        matches!(times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
        matches!(pargs.as_slice(), [ Expression::Reference(_,a), Expression::Reference(_,b) ] if
         a.as_printed() == "a" && b.as_printed() == "b") &&
        matches!(targs.as_slice(), [ Expression::Reference(_,c), Expression::Reference(_,d) ] if
         c.as_printed() == "c" && d.as_printed() == "d"))));

    assert!(matches!(
      parse_ex("3 + f(1 + 2)"),
      Ok(Expression::Call(plus, CallKind::Infix, args)) if
       matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
       matches!(args.as_slice(), [
         Expression::Value(ConstantValue::Integer(_, v3)),
         Expression::Call(eff, CallKind::Normal, fargs)
       ] if
        matches!(v3, IntegerWithBase{ value: 3, .. }) &&
        matches!(eff.as_ref(), Expression::Reference(_,n) if n.as_printed() == "f") &&
        matches!(fargs.as_slice(), [Expression::Call(p, CallKind::Infix, pargs)] if
         matches!(p.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
         matches!(pargs.as_slice(), [Expression::Value(v1), Expression::Value(v2)] if
          matches!(v1, ConstantValue::Integer(_, IntegerWithBase { value: 1, .. })) &&
          matches!(v2, ConstantValue::Integer(_, IntegerWithBase { value: 2, .. })))))));

    assert!(matches!(
      parse_ex("(f . g)(1 + 2)"),
      Ok(Expression::Call(fg, CallKind::Normal, args)) if
       matches!(fg.as_ref(), Expression::Call(dot, CallKind::Infix, fgargs) if
         matches!(dot.as_ref(), Expression::Reference(_,n) if n.as_printed() == ".") &&
         matches!(fgargs.as_slice(), [Expression::Reference(_,f), Expression::Reference(_,g)] if
          f.as_printed() == "f" && g.as_printed() == "g")) &&
       matches!(args.as_slice(), [Expression::Call(plus, CallKind::Infix, pargs)] if
         matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
         matches!(pargs.as_slice(), [Expression::Value(v1), Expression::Value(v2)] if
           matches!(v1, ConstantValue::Integer(_, IntegerWithBase{ value: 1, .. })) &&
           matches!(v2, ConstantValue::Integer(_, IntegerWithBase{ value: 2, .. }))))));

    assert!(matches!(
    parse_ex("a + b(2 + 3) * c"),
    Ok(Expression::Call(plus, CallKind::Infix, pargs)) if
     matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
     matches!(pargs.as_slice(), [
      Expression::Reference(_,a),
      Expression::Call(times, CallKind::Infix, targs)
    ] if a.as_printed() == "a" &&
     matches!(times.as_ref(), Expression::Reference(_,n) if n.as_printed() == "*") &&
     matches!(targs.as_slice(), [
       Expression::Call(b, CallKind::Normal, bargs),
       Expression::Reference(_,c),
     ] if c.as_printed() == "c" &&
      matches!(b.as_ref(), Expression::Reference(_,n) if n.as_printed() == "b") &&
      matches!(bargs.as_slice(), [Expression::Call(plus, CallKind::Infix, pargs)] if
       matches!(plus.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
       matches!(pargs.as_slice(), [
         Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: 2, .. })),
         Expression::Value(ConstantValue::Integer(_, IntegerWithBase{ value: 3, .. }))
       ]))))));
}

#[test]
fn prefix_and_postfix() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.add_infix_precedence("+", parse::Associativity::Left, 4);
        result.add_infix_precedence("*", parse::Associativity::Left, 8);
        result.add_prefix_precedence("++", 6);
        result.add_postfix_precedence("++", 6);
        result.add_prefix_precedence("--", 7);
        result.add_postfix_precedence("--", 7);
        result.parse_expression()
    };

    assert!(matches!(
     parse_ex("++a"),
     Ok(Expression::Call(pp, CallKind::Prefix, args)) if
      matches!(pp.as_ref(), Expression::Reference(_,n) if n.as_printed() == "++") &&
      matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "a")));

    assert!(matches!(
     parse_ex("a--"),
     Ok(Expression::Call(pp, CallKind::Postfix, args)) if
      matches!(pp.as_ref(), Expression::Reference(_,n) if n.as_printed() == "--") &&
      matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "a")));

    // the prefix is weaker than the postfix, so it should be the outside
    // operatotr
    assert!(matches!(
     parse_ex("++a--"),
     Ok(Expression::Call(pp, CallKind::Prefix, args)) if
      matches!(pp.as_ref(), Expression::Reference(_,n) if n.as_printed() == "++") &&
      matches!(args.as_slice(), [Expression::Call(mm, CallKind::Postfix, args)] if
        matches!(mm.as_ref(), Expression::Reference(_,n) if n.as_printed() == "--") &&
        matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "a"))));

    // the prefix is stronger than the postfix, so it should be the inside
    // operator
    assert!(matches!(
     parse_ex("--a++"),
     Ok(Expression::Call(pp, CallKind::Postfix, args)) if
      matches!(pp.as_ref(), Expression::Reference(_,n) if n.as_printed() == "++") &&
      matches!(args.as_slice(), [Expression::Call(mm, CallKind::Prefix, args)] if
        matches!(mm.as_ref(), Expression::Reference(_,n) if n.as_printed() == "--") &&
        matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "a"))));

    assert!(matches!(
     parse_ex("a++ + b"),
     Ok(Expression::Call(p, CallKind::Infix, args)) if
      matches!(p.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
      matches!(args.as_slice(), [
        Expression::Call(mm, CallKind::Postfix, args),
        Expression::Reference(_,n)
      ] if n.as_printed() == "b" &&
        matches!(mm.as_ref(), Expression::Reference(_,n) if n.as_printed() == "++") &&
        matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "a"))));

    assert!(matches!(
     parse_ex("a + ++ b"),
     Ok(Expression::Call(p, CallKind::Infix, args)) if
      matches!(p.as_ref(), Expression::Reference(_,n) if n.as_printed() == "+") &&
      matches!(args.as_slice(), [
        Expression::Reference(_,n),
        Expression::Call(mm, CallKind::Prefix, args),
      ] if n.as_printed() == "a" &&
        matches!(mm.as_ref(), Expression::Reference(_,n) if n.as_printed() == "++") &&
        matches!(args.as_slice(), [Expression::Reference(_,n)] if n.as_printed() == "b"))));

    assert!(matches!(
     parse_ex("a * ++ b"),
     Err(ParserError::UnexpectedToken{ token: Token::OperatorName(pp), .. })
      if pp == "++"));
}

#[test]
fn blocks() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_expression()
    };

    assert!(matches!(
     parse_ex("{}"),
     Ok(Expression::Block(_, void)) if
      matches!(void.as_slice(), [Statement::Expression(call)] if
       matches!(call, Expression::Call(void, CallKind::Normal, vargs) if
        matches!(void.as_ref(), Expression::Reference(_,n) if
          n.as_printed() == "%prim%void") &&
        vargs.is_empty()))));
    assert!(matches!(
     parse_ex("{ x }"),
     Ok(Expression::Block(_, x)) if
      matches!(x.as_slice(), [Statement::Expression(Expression::Reference(_,n))] if
       n.as_printed() == "x")));
    assert!(matches!(
     parse_ex("{ x; }"),
     Ok(Expression::Block(_, x)) if
      matches!(x.as_slice(), [
        Statement::Expression(Expression::Reference(_,n)),
        Statement::Expression(Expression::Call(primv, CallKind::Normal, vargs)),
      ] if n.as_printed() == "x" && vargs.is_empty() &&
       matches!(primv.as_ref(), Expression::Reference(_,n) if
        n.as_printed() == "%prim%void"))));
    assert!(matches!(
     parse_ex("{ x;;; y }"),
     Ok(Expression::Block(_, x)) if
      matches!(x.as_slice(), [
        Statement::Expression(Expression::Reference(_,x)),
        Statement::Expression(Expression::Reference(_,y)),
      ] if x.as_printed() == "x" && y.as_printed() == "y")));
    assert!(matches!(
     parse_ex("{ x; y }"),
     Ok(Expression::Block(_, x)) if
      matches!(x.as_slice(), [
        Statement::Expression(Expression::Reference(_,x)),
        Statement::Expression(Expression::Reference(_,y)),
      ] if x.as_printed() == "x" && y.as_printed() == "y")));
}

#[test]
fn bindings() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_expression()
    };

    assert!(matches!(
     parse_ex("{ let x = y; }"),
     Ok(Expression::Block(_, x)) if
      matches!(x.as_slice(), [Statement::Binding(b), Statement::Expression(_)] if
       !b.mutable &&
       b.variable.as_printed() == "x" &&
       matches!(b.value, Expression::Reference(_,ref n) if n.as_printed() == "y"))));
}

#[test]
fn conditionals() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_expression()
    };

    assert!(matches!(
     parse_ex("if x { y } else { z }"),
     Ok(Expression::Conditional(cond)) if
      matches!(cond.test.as_ref(), Expression::Reference(_,n) if n.as_printed() == "x") &&
      matches!(cond.consequent.as_ref(), Expression::Block(_, cs) if
       matches!(cs.as_slice(), [Statement::Expression(Expression::Reference(_,n))] if
        n.as_printed() == "y")) &&
      matches!(cond.alternative.as_ref(), Some(expr) if
       matches!(expr.as_ref(), Expression::Block(_, ast) if
        matches!(ast.as_slice(), [Statement::Expression(Expression::Reference(_,n))] if
         n.as_printed() == "z")))));

    assert!(matches!(
     parse_ex("if x { y }"),
     Ok(Expression::Conditional(cond)) if
      matches!(cond.test.as_ref(), Expression::Reference(_,n) if n.as_printed() == "x") &&
      matches!(cond.consequent.as_ref(), Expression::Block(_, cs) if
       matches!(cs.as_slice(), [Statement::Expression(Expression::Reference(_,n))] if
        n.as_printed() == "y")) &&
       cond.alternative.is_none()));

    assert!(parse_ex("if x v { z }").is_err());

    assert!(matches!(
     parse_ex("if x + y { z }"),
     Ok(Expression::Conditional(cond)) if
      matches!(cond.test.as_ref(), Expression::Call(_, CallKind::Infix, _))));
}

#[test]
#[allow(clippy::get_first)]
fn patterns() {
    let parse_pat = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new("test", lexer);
        result.parse_pattern()
    };

    assert!(matches!(
     parse_pat("1"),
     Ok(Pattern::Constant(ConstantValue::Integer(_,
      IntegerWithBase { value, .. }))) if
      value == 1));
    assert!(matches!(
     parse_pat("x"),
     Ok(Pattern::Variable(n)) if n.as_printed() == "x"));
    assert!(matches!(
     parse_pat("Cons::Pair(pair)"),
     Ok(Pattern::EnumerationValue(EnumerationPattern{
         type_name, variant_name, argument: Some(subpat), ..
     })) if
       type_name.as_printed() == "Cons" &&
       variant_name.as_printed() == "Pair" &&
       matches!(subpat.as_ref(), Pattern::Variable(p) if
        p.as_printed() == "pair")));
    assert!(matches!(
     parse_pat("Structure{ field, other: something }"),
     Ok(Pattern::Structure(StructurePattern { type_name, fields, .. })) if
      type_name.as_printed() == "Structure" &&
      fields.len() == 2 &&
      matches!(fields.get(0), Some((n, None)) if n.as_printed() == "field") &&
      matches!(fields.get(1), Some((n, Some(Pattern::Variable(s)))) if
        n.as_printed() == "other" &&
        s.as_printed() == "something")));
    assert!(matches!(
     parse_pat("Enumeration::Value(Structure { field, })"),
     Ok(Pattern::EnumerationValue(EnumerationPattern {
         type_name, variant_name, argument: Some(subpat), ..
     })) if
      type_name.as_printed() == "Enumeration" &&
      variant_name.as_printed() == "Value" &&
      matches!(subpat.as_ref(), Pattern::Structure(StructurePattern {
       type_name, fields, ..
     }) if
       type_name.as_printed() == "Structure" &&
       fields.len() == 1 &&
       matches!(fields.first(), Some((f, None)) if
        f.as_printed() == "field"))));
    assert!(matches!(
     parse_pat("Structure { field: Enumeration::Value, }"),
     Ok(Pattern::Structure(StructurePattern {
      type_name, fields, ..
     })) if
       type_name.as_printed() == "Structure" &&
       fields.len() == 1 &&
       matches!(fields.first(), Some((f, Some(subpat))) if
         f.as_printed() == "field" &&
         matches!(subpat, Pattern::EnumerationValue(EnumerationPattern {
           type_name, variant_name, argument: None, ..
         }) if
          type_name.as_printed() == "Enumeration" &&
          variant_name.as_printed() == "Value"))));
}
