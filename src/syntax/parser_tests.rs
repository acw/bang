use crate::syntax::parse::Parser;
use crate::syntax::tokens::Lexer;
use crate::syntax::*;

#[test]
fn constants() {
    let parse_constant = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
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
        let mut result = Parser::new(0, lexer);
        result.parse_type()
    };

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
    println!("-------------");
    println!("{:?}", parse_type("(a -> z)"));
    println!("-------------");
    assert!(matches!(
        parse_type("(a -> z)"),
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

#[test]
fn type_restrictions() {
    let parse_tr = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
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
        } if matches!(constructor, Type::Constructor(_, x) if x == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x == "b"))));

    assert!(matches!(
        parse_tr("restrict(Cons a b,)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 1 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x == "b"))));

    assert!(matches!(parse_tr("restrict(,Cons a b,)"), Err(_)));

    assert!(matches!(
        parse_tr("restrict(Cons a b, Monad m)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 2 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x == "b")) &&
          matches!(&restrictions[1], TypeRestriction {
              constructor,
              arguments,
          } if matches!(constructor, Type::Constructor(_, x) if x == "Monad") &&
             arguments.len() == 1 &&
             matches!(&arguments[0], Type::Variable(_, x) if x == "m"))));

    assert!(matches!(
        parse_tr("restrict(Cons a b, Monad m,)"),
        Ok(TypeRestrictions { restrictions }) if restrictions.len() == 2 &&
          matches!(&restrictions[0], TypeRestriction {
              constructor,
              arguments,
        } if matches!(constructor, Type::Constructor(_, x) if x == "Cons") &&
             arguments.len() == 2 &&
             matches!(&arguments[0], Type::Variable(_, x) if x == "a") &&
             matches!(&arguments[1], Type::Variable(_, x) if x == "b")) &&
          matches!(&restrictions[1], TypeRestriction {
              constructor,
              arguments,
          } if matches!(constructor, Type::Constructor(_, x) if x == "Monad") &&
             arguments.len() == 1 &&
             matches!(&arguments[0], Type::Variable(_, x) if x == "m"))));
}

#[test]
fn field_definition() {
    let parse_fd = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
        result.parse_field_definition()
    };

    assert!(matches!(parse_fd("foo"), Err(_),));
    assert!(matches!(
        parse_fd("foo,"),
        Ok(Some(StructureField{ name, export: ExportClass::Private, field_type: None, .. }))
          if name == "foo"
    ));
    assert!(matches!(
        parse_fd("foo}"),
        Ok(Some(StructureField{ name, export: ExportClass::Private, field_type: None, .. }))
          if name == "foo"
    ));

    assert!(matches!(
        parse_fd("foo: Word8,"),
        Ok(Some(StructureField{ name, field_type, .. }))
          if name == "foo" &&
             matches!(&field_type, Some(Type::Application(c, args))
                 if matches!(c.as_ref(), Type::Constructor(_, c) if c == "Word8") &&
                    args.is_empty())));

    assert!(matches!(
        parse_fd("foo: Cons a b,"),
        Ok(Some(StructureField{ name, field_type, .. }))
          if name == "foo" &&
             matches!(&field_type, Some(Type::Application(c, args))
                 if matches!(c.as_ref(), Type::Constructor(_, c) if c == "Cons") &&
                    matches!(&args.as_slice(), &[Type::Variable(_, v1), Type::Variable(_, v2)]
                        if v1 == "a" && v2 == "b"))));

    assert!(matches!(
        parse_fd("foo: a -> b,"),
        Ok(Some(StructureField{ name, field_type, .. }))
          if name == "foo" &&
             matches!(&field_type, Some(Type::Function(args, ret))
                 if matches!(&args.as_slice(), &[Type::Variable(_, a)] if a == "a") &&
                    matches!(ret.as_ref(), Type::Variable(_, b) if b == "b"))));

    assert!(matches!(
        parse_fd("export foo: a -> b,"),
        Ok(Some(StructureField{ name, export: ExportClass::Public, field_type, .. }))
          if name == "foo" &&
             matches!(&field_type, Some(Type::Function(args, ret))
                 if matches!(&args.as_slice(), &[Type::Variable(_, a)] if a == "a") &&
                    matches!(ret.as_ref(), Type::Variable(_, b) if b == "b"))));
}

#[test]
fn structures() {
    let parse_st = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
        result.parse_structure()
    };

    assert!(matches!(parse_st("structure { }"), Err(_)));
    assert!(matches!(parse_st("structure {"), Err(_)));
    assert!(matches!(parse_st("structure foo {}"), Err(_)));

    assert!(matches!(
        parse_st("structure Foo {}"),
        Ok(StructureDef { name, fields, .. })
          if name == "Foo" && fields.is_empty()));

    assert!(matches!(
        parse_st("structure Foo { bar }"),
        Ok(StructureDef { name, fields, .. })
          if name == "Foo" &&
             matches!(fields.as_slice(), &[StructureField { ref name, ref field_type, .. }]
                 if name == "bar" && matches!(field_type, None))));

    assert!(matches!(
        parse_st("structure Foo { bar: Word8 }"),
        Ok(StructureDef { name, fields, .. })
          if name == "Foo" &&
             matches!(fields.as_slice(), &[StructureField { ref name, ref field_type, .. }]
                 if name == "bar" &&
                    matches!(field_type, Some(Type::Application(c, args))
                      if matches!(c.as_ref(), Type::Constructor(_, c) if c == "Word8") &&
                         args.is_empty()))));

    assert!(matches!(
        parse_st("structure Foo { bar: Word8, goo }"),
        Ok(StructureDef { name, fields, .. })
          if name == "Foo" &&
             matches!(fields.as_slice(),
               &[StructureField { ref name, ref field_type, .. },
                 StructureField { name: ref name2, field_type: None, .. }]
                 if name == "bar" &&
                    name2 == "goo" &&
                    matches!(field_type, Some(Type::Application(c, args))
                      if matches!(c.as_ref(), Type::Constructor(_, c) if c == "Word8") &&
                         args.is_empty()))));

    assert!(matches!(
        parse_st("structure Foo { bar: b c -> a, goo }"),
        Ok(StructureDef { name, fields, .. })
          if name == "Foo" &&
             matches!(fields.as_slice(),
               &[StructureField { ref name, ref field_type, .. },
                 StructureField { name: ref name2, field_type: None, .. }]
                 if name == "bar" &&
                    name2 == "goo" &&
                    matches!(field_type, Some(Type::Function(args, ret))
                      if matches!(&args.as_slice(), &[Type::Variable(_, b), Type::Variable(_, c)]
                           if b == "b" && c == "c") &&
                         matches!(ret.as_ref(), Type::Variable(_, a) if a == "a")))));

    assert!(matches!(
        parse_st("structure Foo { bar: b c -> a, goo, }"),
        Ok(StructureDef { name, fields, .. })
          if name == "Foo" &&
             matches!(fields.as_slice(),
               &[StructureField { ref name, ref field_type, .. },
                 StructureField { name: ref name2, field_type: None, .. }]
                 if name == "bar" &&
                    name2 == "goo" &&
                    matches!(field_type, Some(Type::Function(args, ret))
                      if matches!(&args.as_slice(), &[Type::Variable(_, b), Type::Variable(_, c)]
                           if b == "b" && c == "c") &&
                         matches!(ret.as_ref(), Type::Variable(_, a) if a == "a")))));
}

#[test]
fn enum_variant() {
    let parse_ev = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
        result.parse_enum_variant()
    };

    assert!(matches!(parse_ev("foo"), Err(_),));
    assert!(matches!(parse_ev("foo,"), Err(_),));
    assert!(matches!(parse_ev("Cons foo,"), Err(_),));
    assert!(matches!(parse_ev(""), Err(_)));

    assert!(matches!(parse_ev("}"), Ok(None)));

    assert!(matches!(
      parse_ev("Cons,"),
      Ok(Some(EnumerationVariant { name, argument, .. }))
            if name == "Cons" && argument.is_none()));
    assert!(matches!(
      parse_ev("Cons }"),
      Ok(Some(EnumerationVariant { name, argument, .. }))
            if name == "Cons" && argument.is_none()));
    assert!(matches!(
      parse_ev("Cons, }"),
      Ok(Some(EnumerationVariant { name, argument, .. }))
            if name == "Cons" && argument.is_none()));

    assert!(matches!(
      parse_ev("Cons(Pair a),"),
      Ok(Some(EnumerationVariant { name, ref argument, .. }))
            if name == "Cons" &&
               matches!(argument, Some(Type::Application(typef, args))
                 if matches!(typef.as_ref(), Type::Constructor(_, name)
                       if name == "Pair") &&
                    matches!(&args.as_slice(), &[Type::Variable(_, argname)]
                       if argname == "a"))));
    assert!(matches!(
      parse_ev("Cons(Pair a) }"),
      Ok(Some(EnumerationVariant { name, ref argument, .. }))
            if name == "Cons" &&
               matches!(argument, Some(Type::Application(typef, args))
                 if matches!(typef.as_ref(), Type::Constructor(_, name)
                       if name == "Pair") &&
                    matches!(&args.as_slice(), &[Type::Variable(_, argname)]
                       if argname == "a"))));

    assert!(matches!(
      parse_ev("Cons(a b -> c) }"),
      Ok(Some(EnumerationVariant { name, ref argument, .. }))
            if name == "Cons" &&
               matches!(argument, Some(Type::Function(args, ret))
                 if matches!(&args.as_slice(), &[Type::Variable(_, a), Type::Variable(_, b)]
                      if a == "a" && b == "b") &&
                    matches!(ret.as_ref(), Type::Variable(_, c) if c == "c"))));
}

#[test]
fn enumerations() {
    let parse_en = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
        result.parse_enumeration()
    };

    assert!(matches!(parse_en("enumeration { }"), Err(_)));
    assert!(matches!(parse_en("enumeration {"), Err(_)));
    assert!(matches!(parse_en("enumeration"), Err(_)));

    assert!(matches!(
      parse_en("enumeration Empty { }"),
      Ok(EnumerationDef { name, variants, .. })
        if name == "Empty" && variants.is_empty()));
    assert!(matches!(
      parse_en("enumeration Alternates { A, B }"),
      Ok(EnumerationDef { name, variants, .. })
        if name == "Alternates" &&
           matches!(&variants.as_slice(), &[
               EnumerationVariant { name: name1, argument: arg1, ..},
               EnumerationVariant { name: name2, argument: arg2, ..},
           ] if name1 == "A" && arg1.is_none() &&
                name2 == "B" && arg2.is_none())));
    assert!(matches!(
      parse_en("enumeration Alternates { A, B, }"),
      Ok(EnumerationDef { name, variants, .. })
        if name == "Alternates" &&
           matches!(&variants.as_slice(), &[
               EnumerationVariant { name: name1, argument: arg1, ..},
               EnumerationVariant { name: name2, argument: arg2, ..},
           ] if name1 == "A" && arg1.is_none() &&
                name2 == "B" && arg2.is_none())));
}

#[test]
fn expressions() {
    let parse_ex = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
        result.parse_expression()
    };

    assert!(matches!(parse_ex(""), Err(_)));
    assert!(matches!(
      parse_ex("x"),
      Ok(Expression::Reference(n)) if n.as_printed() == "x"));
    assert!(matches!(
      parse_ex("(x)"),
      Ok(Expression::Reference(n)) if n.as_printed() == "x"));
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
        let mut result = Parser::new(0, lexer);
        result.parse_expression()
    };

    assert!(matches!(parse_ex("Hello::world"), Err(_)));
    assert!(matches!(
      parse_ex("Hello::World"),
      Ok(Expression::EnumerationValue(t, v, None))
        if t.as_printed() == "Hello" &&
           v.as_printed() == "World"));
    assert!(matches!(
      parse_ex("Hello::World(a)"),
      Ok(Expression::EnumerationValue(t, v, Some(_)))
        if t.as_printed() == "Hello" &&
           v.as_printed() == "World"));
}

#[test]
fn structure_value() {
    let parse_st = |str| {
        let lexer = Lexer::from(str);
        let mut result = Parser::new(0, lexer);
        result.parse_expression()
    };

    assert!(matches!(parse_st("Foo{ , }"), Err(_)));
    assert!(matches!(parse_st("Foo{ foo, }"), Err(_)));
    assert!(matches!(parse_st("Foo{ foo: , }"), Err(_)));
    assert!(matches!(parse_st("Foo{ , foo: 1, }"), Err(_)));
    assert!(matches!(
      parse_st("Foo{ foo: 1 }"),
      Ok(Expression::StructureValue(sname, values))
        if sname.as_printed() == "Foo" &&
           matches!(values.as_slice(), [FieldValue{ field, value }]
             if field.as_printed() == "foo" &&
                matches!(value, Expression::Value(ConstantValue::Integer(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1, }"),
      Ok(Expression::StructureValue(sname, values))
        if sname.as_printed() == "Foo" &&
           matches!(values.as_slice(), [FieldValue{ field, value }]
             if field.as_printed() == "foo" &&
                matches!(value, Expression::Value(ConstantValue::Integer(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1, bar: \"foo\" }"),
      Ok(Expression::StructureValue(sname, values))
        if sname.as_printed() == "Foo" &&
           matches!(values.as_slice(), [FieldValue{ field: f1, value: v1 },
                                        FieldValue{ field: f2, value: v2 }]
             if f1.as_printed() == "foo" &&
                f2.as_printed() == "bar" &&
                matches!(v1, Expression::Value(ConstantValue::Integer(_,_))) &&
                matches!(v2, Expression::Value(ConstantValue::String(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1, bar: \"foo\", }"),
      Ok(Expression::StructureValue(sname, values))
        if sname.as_printed() == "Foo" &&
           matches!(values.as_slice(), [FieldValue{ field: f1, value: v1 },
                                        FieldValue{ field: f2, value: v2 }]
             if f1.as_printed() == "foo" &&
                f2.as_printed() == "bar" &&
                matches!(v1, Expression::Value(ConstantValue::Integer(_,_))) &&
                matches!(v2, Expression::Value(ConstantValue::String(_,_))))));
    assert!(matches!(
      parse_st("Foo{ foo: 1,, bar: \"foo\", }"),
      Err(_)));
}
