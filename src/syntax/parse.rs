use crate::syntax::error::ParserError;
use crate::syntax::tokens::{Lexer, LocatedToken, Token};
use crate::syntax::*;
use internment::ArcIntern;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub struct Parser<'lexer> {
    file: ArcIntern<PathBuf>,
    lexer: Lexer<'lexer>,
    known_tokens: Vec<LocatedToken>,
    prefix_precedence_table: HashMap<String, u8>,
    infix_precedence_table: HashMap<String, (u8, u8)>,
    postfix_precedence_table: HashMap<String, u8>,
}

pub enum Associativity {
    Left,
    Right,
    None,
}

impl<'lexer> Parser<'lexer> {
    /// Create a new parser from the given file index and lexer.
    ///
    /// The file index will be used for annotating locations and for
    /// error messages. If you don't care about either, you can use
    /// 0 with no loss of functionality. (Obviously, it will be harder
    /// to create quality error messages, but you already knew that.)
    pub fn new<P: AsRef<Path>>(file: P, lexer: Lexer<'lexer>) -> Parser<'lexer> {
        Parser {
            file: ArcIntern::new(file.as_ref().to_path_buf()),
            lexer,
            known_tokens: vec![],
            prefix_precedence_table: HashMap::new(),
            infix_precedence_table: HashMap::new(),
            postfix_precedence_table: HashMap::new(),
        }
    }

    /// Add the given operator to our precedence table, at the given
    /// precedence level and associativity.
    pub fn add_infix_precedence<S: ToString>(
        &mut self,
        operator: S,
        associativity: Associativity,
        level: u8,
    ) {
        let actual_associativity = match associativity {
            Associativity::Left => (level * 2, (level * 2) + 1),
            Associativity::Right => ((level * 2) + 1, level * 2),
            Associativity::None => (level * 2, level * 2),
        };

        self.infix_precedence_table
            .insert(operator.to_string(), actual_associativity);
    }

    pub fn add_prefix_precedence<S: ToString>(&mut self, operator: S, level: u8) {
        self.prefix_precedence_table
            .insert(operator.to_string(), level * 2);
    }

    pub fn add_postfix_precedence<S: ToString>(&mut self, operator: S, level: u8) {
        self.postfix_precedence_table
            .insert(operator.to_string(), level * 2);
    }

    fn get_precedence(&self, name: &String) -> (u8, u8) {
        match self.infix_precedence_table.get(name) {
            None => (19, 20),
            Some(x) => *x,
        }
    }

    /// Get the next token.
    pub fn next(&mut self) -> Result<Option<LocatedToken>, ParserError> {
        let result = self.known_tokens.pop();

        if result.is_some() {
            Ok(result)
        } else {
            self.lexer
                .next()
                .transpose()
                .map_err(|error| ParserError::LexerError {
                    file: self.file.clone(),
                    error,
                })
        }
    }

    fn save(&mut self, token: LocatedToken) {
        self.known_tokens.push(token)
    }

    fn bad_eof(&mut self, place: &'static str) -> ParserError {
        ParserError::UnacceptableEof {
            file: self.file.clone(),
            place,
        }
    }

    fn to_location(&self, span: Range<usize>) -> Location {
        Location::new(&self.file, span)
    }

    pub fn parse_module(&mut self) -> Result<Module, ParserError> {
        let mut definitions = vec![];

        loop {
            let next_token = self.next()?;

            if next_token.is_none() {
                return Ok(Module { definitions });
            }

            definitions.push(self.parse_definition()?);
        }
    }

    pub fn parse_definition(&mut self) -> Result<Definition, ParserError> {
        let (export, start) = self.parse_export_class()?;
        let type_restrictions = self.parse_type_restrictions()?;
        let definition = self.parse_def()?;
        let location = definition.location().merge_span(start);

        Ok(Definition {
            location,
            export,
            type_restrictions,
            definition,
        })
    }

    fn parse_export_class(&mut self) -> Result<(ExportClass, Range<usize>), ParserError> {
        let maybe_export = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for possible export"))?;

        if matches!(maybe_export.token, Token::ValueName(ref x) if x == "export") {
            Ok((ExportClass::Public, maybe_export.span))
        } else {
            let start = maybe_export.span.clone();
            self.save(maybe_export);
            Ok((ExportClass::Private, start))
        }
    }

    pub fn parse_type_restrictions(&mut self) -> Result<TypeRestrictions, ParserError> {
        let Some(maybe_restrict) = self.next()? else {
            return Ok(TypeRestrictions::empty());
        };

        if !matches!(maybe_restrict.token, Token::ValueName(ref x) if x == "restrict") {
            self.save(maybe_restrict);
            return Ok(TypeRestrictions::empty());
        }

        let maybe_paren = self
            .next()?
            .ok_or_else(|| self.bad_eof("Looking for open paren after restrict"))?;

        if !matches!(maybe_paren.token, Token::OpenParen) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: maybe_paren.span,
                token: maybe_paren.token,
                expected: "open parenthesis, following the restrict keyword",
            });
        }

        let mut restrictions = vec![];

        while let Some(type_restriction) = self.parse_type_restriction()? {
            restrictions.push(type_restriction);
        }

        let maybe_paren = self
            .next()?
            .ok_or_else(|| self.bad_eof("Looking for open paren after restrict"))?;
        if !matches!(maybe_paren.token, Token::CloseParen) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: maybe_paren.span,
                token: maybe_paren.token,
                expected: "close parenthesis following type restrictions",
            });
        }

        Ok(TypeRestrictions { restrictions })
    }

    fn parse_type_restriction(&mut self) -> Result<Option<TypeRestriction>, ParserError> {
        let maybe_constructor = self
            .next()?
            .ok_or_else(|| self.bad_eof("Looking for constructor for type restriction"))?;

        let constructor = match maybe_constructor.token {
            Token::TypeName(str) => {
                let name = Name::new(self.to_location(maybe_constructor.span.clone()), str);
                Type::Constructor(self.to_location(maybe_constructor.span), name)
            }
            Token::PrimitiveTypeName(str) => {
                let name = Name::new(self.to_location(maybe_constructor.span.clone()), str);
                Type::Primitive(self.to_location(maybe_constructor.span), name)
            }

            token @ Token::CloseParen | token @ Token::Comma => {
                self.save(LocatedToken {
                    token,
                    span: maybe_constructor.span,
                });
                return Ok(None);
            }

            weird => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: maybe_constructor.span,
                    token: weird,
                    expected: "Constructor name, comma, or close parenthesis in type restriction",
                });
            }
        };

        let mut arguments = vec![];

        while let Ok(t) = self.parse_base_type() {
            arguments.push(t);
        }

        let restriction = TypeRestriction {
            constructor,
            arguments,
        };

        let Some(maybe_comma) = self.next()? else {
            return Ok(Some(restriction));
        };

        match maybe_comma.token {
            Token::Comma => {}
            _ => self.save(maybe_comma),
        }

        Ok(Some(restriction))
    }

    fn parse_def(&mut self) -> Result<Def, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for definition body"))?;

        if let Ok(structure) = self.parse_structure() {
            return Ok(Def::Structure(structure));
        }

        if let Ok(enumeration) = self.parse_enumeration() {
            return Ok(Def::Enumeration(enumeration));
        }

        if let Ok(fun_or_val) = self.parse_function_or_value() {
            return Ok(fun_or_val);
        }

        Err(ParserError::UnexpectedToken {
            file: self.file.clone(),
            span: next.span,
            token: next.token,
            expected: "'structure', 'enumeration', or a value identifier",
        })
    }

    pub fn parse_structure(&mut self) -> Result<StructureDef, ParserError> {
        let structure_token = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for definition"))?;
        if !matches!(structure_token.token, Token::ValueName(ref s) if s == "structure") {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: structure_token.span,
                token: structure_token.token,
                expected: "the 'structure' keyword",
            });
        }

        let name = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for structure name"))?;
        let structure_name = match name.token {
            Token::TypeName(str) => Name::new(self.to_location(name.span), str),
            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: name.span,
                    token: name.token,
                    expected: "a structure name",
                });
            }
        };

        let brace = self
            .next()?
            .ok_or_else(|| self.bad_eof("the open brace after a structure name"))?;
        if !matches!(brace.token, Token::OpenBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: brace.span,
                token: brace.token,
                expected: "the brace after a structure name",
            });
        }

        let mut fields = vec![];

        while let Some(field_definition) = self.parse_field_definition()? {
            fields.push(field_definition);
        }

        let brace = self.next()?.ok_or_else(|| {
            self.bad_eof("the close brace after at the end of a structure definition")
        })?;
        if !matches!(brace.token, Token::CloseBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: brace.span,
                token: brace.token,
                expected: "the brace at the end of a structure definition",
            });
        }

        let location = self
            .to_location(structure_token.span)
            .extend_to(&self.to_location(brace.span));

        Ok(StructureDef {
            name: structure_name,
            location,
            fields,
        })
    }

    pub fn parse_field_value(&mut self) -> Result<Option<FieldValue>, ParserError> {
        let maybe_name = self
            .next()?
            .ok_or_else(|| self.bad_eof("parsing field definition"))?;

        let field = match maybe_name.token {
            Token::ValueName(x) => Name::new(self.to_location(maybe_name.span), x),
            _ => {
                self.save(maybe_name.clone());
                return Ok(None);
            }
        };

        let maybe_colon = self.next()?.ok_or_else(|| {
            self.bad_eof("looking for colon, comma, or close brace after field name")
        })?;
        if !matches!(maybe_colon.token, Token::Colon) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: maybe_colon.span,
                token: maybe_colon.token,
                expected: "colon after field name in constructor",
            });
        }

        let value = self.parse_expression()?;

        let end_token = self.next()?.ok_or_else(|| {
            self.bad_eof("looking for comma or close brace after field definition")
        })?;
        if !matches!(end_token.token, Token::Comma) {
            self.save(end_token);
        }

        Ok(Some(FieldValue { field, value }))
    }

    pub fn parse_field_definition(&mut self) -> Result<Option<StructureField>, ParserError> {
        let (export, start) = self.parse_export_class()?;
        let maybe_name = self
            .next()?
            .ok_or_else(|| self.bad_eof("parsing field definition"))?;

        let name = match maybe_name.token {
            Token::ValueName(x) => Name::new(self.to_location(maybe_name.span.clone()), x),
            _ => {
                self.save(maybe_name.clone());
                if matches!(export, ExportClass::Private) {
                    return Ok(None);
                } else {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: maybe_name.span,
                        token: maybe_name.token,
                        expected: "a field name",
                    });
                }
            }
        };
        let start_location = self.to_location(start);

        let maybe_colon = self.next()?.ok_or_else(|| {
            self.bad_eof("looking for colon, comma, or close brace after field name")
        })?;

        let field_type = match maybe_colon.token {
            Token::Comma | Token::CloseBrace => {
                self.save(maybe_colon);
                None
            }

            Token::Colon => Some(self.parse_type()?),

            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: maybe_colon.span,
                    token: maybe_colon.token,
                    expected: "colon, comma, or close brace after field name",
                });
            }
        };

        let end_token = self.next()?.ok_or_else(|| {
            self.bad_eof("looking for comma or close brace after field definition")
        })?;

        let maybe_end_location = match end_token.token {
            Token::Comma => Some(self.to_location(end_token.span)),
            Token::CloseBrace => {
                self.save(end_token);
                None
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: end_token.span,
                    token: end_token.token,
                    expected: "looking for comma or close brace after field definition",
                });
            }
        };

        let end_location = maybe_end_location
            .or_else(|| field_type.as_ref().map(|x| x.location()))
            .unwrap_or_else(|| self.to_location(maybe_name.span));
        let location = start_location.extend_to(&end_location);

        Ok(Some(StructureField {
            location,
            export,
            name,
            field_type,
        }))
    }

    pub fn parse_enumeration(&mut self) -> Result<EnumerationDef, ParserError> {
        let enumeration_token = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for definition"))?;
        if !matches!(enumeration_token.token, Token::ValueName(ref e) if e == "enumeration") {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: enumeration_token.span,
                token: enumeration_token.token,
                expected: "the 'enumeration' keyword",
            });
        }

        let name = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for enumeration name"))?;
        let enumeration_name = match name.token {
            Token::TypeName(str) => Name::new(self.to_location(name.span), str),
            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: name.span,
                    token: name.token,
                    expected: "an enumeration name",
                });
            }
        };

        let brace = self
            .next()?
            .ok_or_else(|| self.bad_eof("the open brace after an enumeration name"))?;
        if !matches!(brace.token, Token::OpenBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: brace.span,
                token: brace.token,
                expected: "the brace after an enumeration name",
            });
        }

        let mut variants = vec![];

        while let Some(variant_definition) = self.parse_enum_variant()? {
            variants.push(variant_definition);
        }

        let brace = self.next()?.ok_or_else(|| {
            self.bad_eof("the close brace after at the end of an enumeration definition")
        })?;
        if !matches!(brace.token, Token::CloseBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: brace.span,
                token: brace.token,
                expected: "the brace at the end of an enumeration definition",
            });
        }

        let location = self
            .to_location(enumeration_token.span)
            .extend_to(&self.to_location(brace.span));

        Ok(EnumerationDef {
            name: enumeration_name,
            location,
            variants,
        })
    }

    pub fn parse_enum_variant(&mut self) -> Result<Option<EnumerationVariant>, ParserError> {
        let maybe_name = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for enumeration name"))?;
        let name = match maybe_name.token {
            Token::TypeName(x) => Name::new(self.to_location(maybe_name.span.clone()), x),
            Token::CloseBrace => {
                self.save(maybe_name);
                return Ok(None);
            }
            _ => {
                self.save(maybe_name.clone());
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: maybe_name.span,
                    token: maybe_name.token,
                    expected: "variant name (identifier starting with a capital)",
                });
            }
        };
        let start_location = self.to_location(maybe_name.span);

        let maybe_paren = self
            .next()?
            .ok_or_else(|| self.bad_eof("trying to understand enumeration variant"))?;
        let (argument, arg_location) = if matches!(maybe_paren.token, Token::OpenParen) {
            let t = self.parse_type()?;

            let maybe_close = self
                .next()?
                .ok_or_else(|| self.bad_eof("trying to parse a enumeration variant's type"))?;
            if !matches!(maybe_close.token, Token::CloseParen) {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: maybe_close.span,
                    token: maybe_close.token,
                    expected: "close paren to end an enumeration variant's type argument",
                });
            }

            let location = t.location();
            (Some(t), location)
        } else {
            self.save(maybe_paren);
            (None, start_location.clone())
        };

        let ender = self.next()?.ok_or_else(|| {
            self.bad_eof("looking for comma or close brace after enumeration variant")
        })?;
        let end_location = match ender.token {
            Token::Comma => self.to_location(ender.span),
            Token::CloseBrace => {
                self.save(ender);
                arg_location
            }
            _ => {
                self.save(ender.clone());
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: ender.span,
                    token: ender.token,
                    expected: "comma or close brace after enumeration variant",
                });
            }
        };

        let location = start_location.extend_to(&end_location);

        Ok(Some(EnumerationVariant {
            name,
            location,
            argument,
        }))
    }

    fn parse_function_or_value(&mut self) -> Result<Def, ParserError> {
        unimplemented!()
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for an expression"))?;

        self.save(next.clone());
        match next.token {
            Token::ValueName(x) if x == "match" => {
                Ok(Expression::Match(self.parse_match_expression()?))
            }
            Token::ValueName(x) if x == "if" => {
                Ok(Expression::Conditional(self.parse_if_expression()?))
            }
            _ => self.parse_arithmetic(0),
        }
    }

    fn parse_match_expression(&mut self) -> Result<MatchExpr, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for a 'match' to open a pattern match"))?;

        if !matches!(next.token, Token::ValueName(ref x) if x == "match") {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "an 'match' to start a pattern match",
            });
        }
        let start = self.to_location(next.span);

        let value = Box::new(self.parse_arithmetic(0)?);

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for an open brace after 'match'"))?;
        if !matches!(next.token, Token::OpenBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "an open brace after the match expression",
            });
        }

        let mut cases = vec![];

        while let Some(case) = self.parse_match_case()? {
            cases.push(case);
        }

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for an open brace after 'match'"))?;
        if !matches!(next.token, Token::CloseBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "a close brace to end a match expression",
            });
        }
        let end = self.to_location(next.span);

        let location = start.extend_to(&end);

        Ok(MatchExpr {
            location,
            value,
            cases,
        })
    }

    fn parse_match_case(&mut self) -> Result<Option<MatchCase>, ParserError> {
        unimplemented!()
    }

    fn parse_if_expression(&mut self) -> Result<ConditionalExpr, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for an 'if' to start conditional"))?;
        if !matches!(next.token, Token::ValueName(ref x) if x == "if") {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "an 'if' to start a conditional",
            });
        }
        let start = self.to_location(next.span);

        let test = self.parse_arithmetic(0)?;
        let consequent = self.parse_block()?;

        let maybe_else = self.next()?;
        let (alternative, location) = match maybe_else {
            Some(LocatedToken {
                token: Token::ValueName(ref n),
                ..
            }) if n == "else" => {
                let expr = self.parse_block()?;
                let location = match expr {
                    Expression::Block(ref l, _) => l.clone(),
                    _ => panic!("How did parse_block not return a block?!"),
                };

                (Some(Box::new(expr)), location)
            }

            _ => {
                let location = match consequent {
                    Expression::Block(ref l, _) => l.clone(),
                    _ => panic!("How did parse_block not return a block?!"),
                };

                (None, location)
            }
        };

        Ok(ConditionalExpr {
            location: start.extend_to(&location),
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternative,
        })
    }

    pub fn parse_block(&mut self) -> Result<Expression, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for open brace to start block"))?;
        if !matches!(next.token, Token::OpenBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "an open brace to start a block",
            });
        }
        let start = self.to_location(next.span);

        let mut statements = vec![];
        let mut ended_with_expr = false;

        while let Some((stmt, terminal)) = self.parse_statement()? {
            statements.push(stmt);
            if terminal {
                ended_with_expr = true;
                break;
            }
        }

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for statement or block close"))?;
        if !matches!(next.token, Token::CloseBrace) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "a close brace to end a block",
            });
        }
        let end = self.to_location(next.span);

        if !ended_with_expr {
            let void_name = Name::new(end.clone(), "%prim%void");
            let void_ref = Expression::Reference(void_name);
            let void_call = Expression::Call(Box::new(void_ref), CallKind::Normal, vec![]);
            statements.push(Statement::Expression(void_call));
        }

        Ok(Expression::Block(start.extend_to(&end), statements))
    }

    pub fn parse_statement(&mut self) -> Result<Option<(Statement, bool)>, ParserError> {
        loop {
            let next = self
                .next()?
                .ok_or_else(|| self.bad_eof("looking for a statement or close brace"))?;

            match next.token {
                Token::CloseBrace => {
                    self.save(next);
                    return Ok(None);
                }

                Token::ValueName(ref l) if l == "let" => {
                    self.save(next);
                    return Ok(Some((Statement::Binding(self.parse_let()?), false)));
                }

                _ => {
                    self.save(next);
                    let expr = Statement::Expression(self.parse_expression()?);

                    let next = self
                        .next()?
                        .ok_or_else(|| self.bad_eof("looking for semicolon or close brace"))?;

                    if matches!(next.token, Token::Semi) {
                        return Ok(Some((expr, false)));
                    } else {
                        self.save(next);
                        return Ok(Some((expr, true)));
                    }
                }
            }
        }
    }

    pub fn parse_let(&mut self) -> Result<BindingStmt, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for a let for a binding statement"))?;
        if !matches!(next.token, Token::ValueName(ref n) if n == "let") {
            self.save(next.clone());
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "a 'let' to open a binding statement",
            });
        }
        let start = self.to_location(next.span);

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("'mut' or a variable name"))?;
        let mutable = matches!(next.token, Token::ValueName(ref n) if n == "mut");
        if !mutable {
            self.save(next);
        }

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("a variable name"))?;
        let variable = match next.token {
            Token::ValueName(v) => Name::new(self.to_location(next.span), v),
            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: next.span,
                    token: next.token,
                    expected: "a variable name for the let binding",
                });
            }
        };

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("an '=' after a variable name in a binding"))?;
        if !matches!(next.token, Token::OperatorName(ref x) if x == "=") {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "an '=' after the variable name in a let binding",
            });
        }

        let value = self.parse_expression()?;

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for terminal semicolon for let statement"))?;
        if !matches!(next.token, Token::Semi) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "a semicolon to finish a let statement",
            });
        }
        let end = self.to_location(next.span);

        Ok(BindingStmt {
            location: start.extend_to(&end),
            mutable,
            variable,
            value,
        })
    }

    pub fn parse_arithmetic(&mut self, level: u8) -> Result<Expression, ParserError> {
        // start by checking for prefix operators.
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for arithmetic expression"))?;

        let mut lhs = if let Token::OperatorName(ref n) = next.token {
            if let Some(pre_prec) = self.prefix_precedence_table.get(n) {
                if *pre_prec < level {
                    self.save(next.clone());
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "a base expression of a tighter-binding prefix operator",
                    });
                }

                let rhs = self.parse_arithmetic(*pre_prec)?;
                let opname = Name::new(self.to_location(next.span), n);
                let op_expr = Expression::Reference(opname);

                Expression::Call(Box::new(op_expr), CallKind::Prefix, vec![rhs])
            } else {
                self.save(next);
                self.parse_base_expression()?
            }
        } else {
            self.save(next);
            self.parse_base_expression()?
        };

        loop {
            let Some(next) = self.next()? else {
                return Ok(lhs);
            };

            match next.token {
                Token::OpenParen => {
                    self.save(next);
                    let args = self.parse_call_arguments()?;
                    lhs = Expression::Call(Box::new(lhs), CallKind::Normal, args);
                }

                Token::OperatorName(ref n) => {
                    if let Some(postprec) = self.postfix_precedence_table.get(n) {
                        if *postprec < level {
                            self.save(next);
                            break;
                        }

                        let opname = Name::new(self.to_location(next.span), n);
                        let op_expr = Expression::Reference(opname);

                        lhs = Expression::Call(Box::new(op_expr), CallKind::Postfix, vec![lhs]);
                        continue;
                    }

                    let (left_pr, right_pr) = self.get_precedence(&n);

                    if left_pr < level {
                        self.save(next);
                        break;
                    }

                    let rhs = self.parse_arithmetic(right_pr)?;
                    let name = Name::new(self.to_location(next.span), n);
                    let opref = Box::new(Expression::Reference(name));
                    let args = vec![lhs, rhs];

                    lhs = Expression::Call(opref, CallKind::Infix, args);
                }

                _ => {
                    self.save(next);
                    return Ok(lhs);
                }
            }
        }

        Ok(lhs)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for open paren for function arguments"))?;

        if !matches!(next.token, Token::OpenParen) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "open paren for call arguments",
            });
        }

        let mut args = vec![];

        loop {
            let next = self.next()?.ok_or_else(|| {
                self.bad_eof("looking for an expression or close paren in function arguments")
            })?;

            if matches!(next.token, Token::CloseParen) {
                break;
            }

            self.save(next);
            let argument = self.parse_arithmetic(0)?;
            args.push(argument);

            let next = self.next()?.ok_or_else(|| {
                self.bad_eof("looking for comma or close paren in function arguments")
            })?;
            match next.token {
                Token::Comma => continue,
                Token::CloseParen => break,
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "comma or close paren in function arguments",
                    });
                }
            }
        }

        Ok(args)
    }

    pub fn parse_base_expression(&mut self) -> Result<Expression, ParserError> {
        if let Ok(v) = self.parse_constant() {
            return Ok(Expression::Value(v));
        }

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for an expression"))?;

        match next.token {
            Token::OpenBrace => {
                self.save(next);
                return self.parse_block();
            }

            Token::OpenParen => {
                let inner = self.parse_expression()?;
                let hopefully_close = self
                    .next()?
                    .ok_or_else(|| self.bad_eof("looking for close paren to finish expression"))?;
                if matches!(hopefully_close.token, Token::CloseParen) {
                    Ok(inner)
                } else {
                    self.save(hopefully_close.clone());
                    Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: hopefully_close.span,
                        token: hopefully_close.token,
                        expected: "close paren after expression",
                    })
                }
            }

            Token::TypeName(n) | Token::PrimitiveTypeName(n) => {
                let type_name = Name::new(self.to_location(next.span), n);
                let after_type_name = self.next()?.ok_or_else(|| {
                    self.bad_eof("looking for colon, open brace, or open paren in constructor")
                })?;

                match after_type_name.token {
                    Token::OpenBrace => {
                        let mut fields = vec![];

                        while let Some(field) = self.parse_field_value()? {
                            fields.push(field);
                        }

                        let closer = self.next()?.ok_or_else(|| {
                            self.bad_eof("looking for close brace in structure value")
                        })?;
                        if !matches!(closer.token, Token::CloseBrace) {
                            return Err(ParserError::UnexpectedToken {
                                file: self.file.clone(),
                                span: closer.span,
                                token: closer.token,
                                expected: "close brace or comma after field value",
                            });
                        }

                        Ok(Expression::StructureValue(type_name, fields))
                    }

                    Token::Colon => {
                        let second_colon = self.next()?.ok_or_else(|| {
                            self.bad_eof("looking for second colon in enumeration value")
                        })?;
                        if !matches!(second_colon.token, Token::Colon) {
                            return Err(ParserError::UnexpectedToken {
                                file: self.file.clone(),
                                span: second_colon.span,
                                token: second_colon.token,
                                expected: "second colon in enumeration value",
                            });
                        }

                        let vname = self
                            .next()?
                            .ok_or_else(|| self.bad_eof("looking for enumeration value name"))?;

                        let value_name = match vname.token {
                            Token::TypeName(s) => {
                                let loc = self.to_location(vname.span);
                                Name::new(loc, s)
                            }

                            _ => {
                                return Err(ParserError::UnexpectedToken {
                                    file: self.file.clone(),
                                    span: vname.span,
                                    token: vname.token,
                                    expected: "enumeration value name",
                                });
                            }
                        };

                        let arg = if let Some(maybe_paren) = self.next()? {
                            let expr = self.parse_expression()?;

                            let tok = self.next()?.ok_or_else(|| {
                                self.bad_eof("looking for close paren after enum value argument")
                            })?;
                            if !matches!(tok.token, Token::CloseParen) {
                                return Err(ParserError::UnexpectedToken {
                                    file: self.file.clone(),
                                    span: tok.span,
                                    token: tok.token,
                                    expected: "close paren after enum value argument",
                                });
                            }

                            Some(Box::new(expr))
                        } else {
                            None
                        };

                        Ok(Expression::EnumerationValue(type_name, value_name, arg))
                    }

                    _ => Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: after_type_name.span,
                        token: after_type_name.token,
                        expected: "colon, open brace, or open paren in constructor",
                    }),
                }
            }

            Token::ValueName(n) | Token::PrimitiveValueName(n) => {
                let location = self.to_location(next.span);
                let name = Name::new(location, n);
                Ok(Expression::Reference(name))
            }

            _ => {
                self.save(next.clone());
                Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: next.span,
                    token: next.token,
                    expected: "some base expression or an open brace",
                })
            }
        }
    }

    pub fn parse_type(&mut self) -> Result<Type, ParserError> {
        self.parse_function_type()
    }

    fn parse_function_type(&mut self) -> Result<Type, ParserError> {
        let mut args = Vec::new();

        while let Ok(t) = self.parse_type_application() {
            args.push(t);
        }

        let Some(maybe_arrow) = self.next()? else {
            match args.pop() {
                None => {
                    return Err(ParserError::UnacceptableEof {
                        file: self.file.clone(),
                        place: "parsing function type or type",
                    });
                }

                Some(t) if args.len() == 0 => return Ok(t),

                Some(_) => {
                    return Err(ParserError::UnacceptableEof {
                        file: self.file.clone(),
                        place: "looking for '->' in function type",
                    });
                }
            }
        };

        if maybe_arrow.token == Token::Arrow {
            let right = self.parse_function_type()?;
            Ok(Type::Function(args, Box::new(right)))
        } else if args.len() == 1 {
            self.save(maybe_arrow);
            Ok(args.pop().expect("length = 1 works"))
        } else {
            self.save(maybe_arrow.clone());
            let LocatedToken { token, span } = maybe_arrow;

            Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span,
                token,
                expected: "'->' in function type",
            })
        }
    }

    fn parse_type_application(&mut self) -> Result<Type, ParserError> {
        let LocatedToken { token, span } =
            self.next()?.ok_or_else(|| self.bad_eof("parsing type"))?;

        let constructor = match token {
            Token::TypeName(x) => {
                let name = Name::new(self.to_location(span.clone()), x);
                Type::Constructor(self.to_location(span), name)
            }
            Token::PrimitiveTypeName(x) => {
                let name = Name::new(self.to_location(span.clone()), x);
                Type::Primitive(self.to_location(span), name)
            }
            _ => {
                self.save(LocatedToken { token, span });
                return self.parse_base_type();
            }
        };

        let mut args = vec![];

        while let Ok(next_arg) = self.parse_base_type() {
            args.push(next_arg);
        }

        Ok(Type::Application(Box::new(constructor), args))
    }

    fn parse_base_type(&mut self) -> Result<Type, ParserError> {
        let LocatedToken { token, span } =
            self.next()?.ok_or_else(|| self.bad_eof("parsing type"))?;

        match token {
            Token::TypeName(x) => {
                let name = Name::new(self.to_location(span.clone()), x);
                Ok(Type::Constructor(self.to_location(span), name))
            }
            Token::PrimitiveTypeName(x) => {
                let name = Name::new(self.to_location(span.clone()), x);
                Ok(Type::Primitive(self.to_location(span), name))
            }
            Token::ValueName(x) => {
                let name = Name::new(self.to_location(span.clone()), x);
                Ok(Type::Variable(self.to_location(span), name))
            }
            Token::OpenParen => {
                let t = self.parse_type()?;
                let closer = self
                    .next()?
                    .ok_or_else(|| self.bad_eof("close paren in type"))?;

                if !matches!(closer.token, Token::CloseParen) {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: closer.span,
                        token: closer.token,
                        expected: "close parenthesis to finish a type",
                    });
                }

                Ok(t)
            }
            token => {
                self.save(LocatedToken {
                    token: token.clone(),
                    span: span.clone(),
                });

                Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span,
                    token,
                    expected: "type constructor, type variable, or primitive type",
                })
            }
        }
    }

    pub fn parse_constant(&mut self) -> Result<ConstantValue, ParserError> {
        let maybe_constant = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for a constant"))?;

        match maybe_constant.token {
            Token::Integer(iwb) => Ok(ConstantValue::Integer(
                self.to_location(maybe_constant.span),
                iwb,
            )),
            Token::Character(c) => Ok(ConstantValue::Character(
                self.to_location(maybe_constant.span),
                c,
            )),
            Token::String(s) => Ok(ConstantValue::String(
                self.to_location(maybe_constant.span),
                s,
            )),
            _ => {
                self.save(maybe_constant.clone());
                Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: maybe_constant.span,
                    token: maybe_constant.token,
                    expected: "constant value",
                })
            }
        }
    }
}
