use crate::syntax::error::ParserError;
use crate::syntax::tokens::{Lexer, LocatedToken, Token};
use crate::syntax::*;
use internment::ArcIntern;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// A parser for a particular file.
///
/// This parser should be used for exactly one file, and its lifetime
/// must be tied to the underlying lexer. However, after the parser is
/// done, the resultant object should have no lifetime links to the
/// original file, so it can be thrown away.
///
/// The parser includes information about operator precedence that is
/// stateful.
pub struct Parser<'lexer> {
    file: ArcIntern<PathBuf>,
    lexer: Lexer<'lexer>,
    known_tokens: Vec<LocatedToken>,
    prefix_precedence_table: HashMap<String, u8>,
    infix_precedence_table: HashMap<String, (u8, u8)>,
    postfix_precedence_table: HashMap<String, u8>,
}

/// The directional associativity for an operator.
///
/// This directionality impacts whether (a + b + c) defaults to
/// ((a + b) + c) or (a + (b + c)). It does not effect situations
/// in which operator numeric precedence is different between
/// operators.
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
    ///
    /// This is used for infix operators, only.
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

    /// Add the given operator to our precedence table, at the given
    /// precedence level and associativity.
    ///
    /// This is used for prefix operators, only.
    pub fn add_prefix_precedence<S: ToString>(&mut self, operator: S, level: u8) {
        self.prefix_precedence_table
            .insert(operator.to_string(), level * 2);
    }

    /// Add the given operator to our precedence table, at the given
    /// precedence level and associativity.
    ///
    /// This is used for postfix operators, only.
    pub fn add_postfix_precedence<S: ToString>(&mut self, operator: S, level: u8) {
        self.postfix_precedence_table
            .insert(operator.to_string(), level * 2);
    }

    /// Get the precedence of the given operator.
    ///
    /// FIXME: This currently only functions on infix operators, not
    /// prefix and postfix. In general, this can all be cleaned up.
    fn get_precedence(&self, name: &String) -> (u8, u8) {
        match self.infix_precedence_table.get(name) {
            None => (19, 20),
            Some(x) => *x,
        }
    }

    /// Get the next token from the input stream, or None if we're at
    /// the end of a stream.
    ///
    /// Ok(None) represents "we have reached the end of the stream", while
    /// an Err(_) means that we ran into some sort of error (UTF-8 formatting,
    /// lexing, IO, etc.) in reading the stream.
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

    /// Save the given token back to the top of the stream.
    ///
    /// This is essentially an "undo" on next(), or an alternative path for
    /// peeking at the next token in the stream.
    fn save(&mut self, token: LocatedToken) {
        self.known_tokens.push(token)
    }

    /// Get the location of the next token in the stream.
    ///
    /// This will return an error if we're at the end of the file.
    fn current_location(&mut self) -> Result<Location, ParserError> {
        let current = self.next()?;
        match current {
            None => Err(self.bad_eof("trying to get current location")),
            Some(token) => {
                let retval = self.to_location(token.span.clone());
                self.save(token);
                Ok(retval)
            }
        }
    }

    /// Generate the parser error that should happen when we hit an EOF
    /// in a bad place.
    fn bad_eof<S: ToString>(&mut self, place: S) -> ParserError {
        ParserError::UnacceptableEof {
            file: self.file.clone(),
            place: place.to_string(),
        }
    }

    /// Convert an offset into a formal location that can be saved off
    /// into ASTs.
    fn to_location(&self, span: Range<usize>) -> Location {
        Location::new(&self.file, span)
    }

    /// See if the next token is the keyword, as expected.
    ///
    /// If it isn't, this routine will provide an error, but it will make
    /// sure to put the token back into the stream.
    fn require_keyword(&mut self, keyword: &'static str) -> Result<Location, ParserError> {
        match self.next()? {
            None => Err(self.bad_eof(format!("looking for keyword '{keyword}'"))),
            Some(ltoken) => match ltoken.token {
                Token::ValueName(s) if s.as_str() == keyword => Ok(self.to_location(ltoken.span)),
                _ => {
                    self.save(ltoken.clone());
                    Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: ltoken.span,
                        token: ltoken.token,
                        expected: format!("keyword {keyword}"),
                    })
                }
            },
        }
    }

    /// See if the next token is the given one, as expected.
    ///
    /// If it isn't, this routine will provide an error, but it will make
    /// sure to put the token back into the stream.
    fn require_token(
        &mut self,
        token: Token,
        place: &'static str,
    ) -> Result<Location, ParserError> {
        let message = || format!("looking for '{token}' in {place}");
        let next = self.next()?.ok_or_else(|| self.bad_eof(message()))?;

        if next.token != token {
            self.save(next.clone());
            Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: message(),
            })
        } else {
            Ok(self.to_location(next.span))
        }
    }

    /// Parse the top level file associated with a Bang module.
    ///
    /// This will expect to read until EOF, and will fail or stall
    /// forever if there is no EOF, or the EOF ends in the wrong
    /// place. So this should *not* be used for interactive sessions,
    /// because those are unlikely to have EOFs in the appropriate
    /// places.
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

    /// Parse a definition in a file (structure, enumeration, value, etc.).
    ///
    /// This will read a definition. If there's an error, it's very likely the
    /// input stream will be corrupted, so you probably don't want to try to
    /// recover. You can, obviously.
    pub fn parse_definition(&mut self) -> Result<Definition, ParserError> {
        let (export, start) = self.parse_export_class()?;
        let type_restrictions = self.parse_type_restrictions()?;
        let definition = self.parse_def()?;
        let location = definition.location().extend_to(&start);

        Ok(Definition {
            location,
            export,
            type_restrictions,
            definition,
        })
    }

    /// Parse the export class for the current definition.
    ///
    /// If there isn't an 'export' declaration, then this will return 'private',
    /// because if it hasn't been declared exported then it's private. But this
    /// does mean that a future parsing error will be assumed to be a private
    /// declaration.
    fn parse_export_class(&mut self) -> Result<(ExportClass, Location), ParserError> {
        if let Ok(span) = self.require_keyword("export") {
            Ok((ExportClass::Public, span))
        } else {
            let start = self.current_location()?;
            Ok((ExportClass::Private, start))
        }
    }

    /// Parse a type restriction and return it.
    ///
    /// Like the export class parsing, parsing type restrictions has a clear
    /// default (no restrictions) when the input doesn't lead with the appropriate
    /// keyword. As a result, this can generate a result even in cases in which
    /// the input is empty.
    pub fn parse_type_restrictions(&mut self) -> Result<TypeRestrictions, ParserError> {
        if self.require_keyword("restrict").is_err() {
            return Ok(TypeRestrictions::empty());
        }
        let _ = self.require_token(Token::OpenParen, "type restriction")?;

        let mut restrictions = vec![];

        while let Some(type_restriction) = self.parse_type_restriction()? {
            restrictions.push(type_restriction);
        }

        let _ = self.require_token(Token::CloseParen, "type restriction")?;
        Ok(TypeRestrictions { restrictions })
    }

    /// Parse a single type retriction.
    ///
    /// A type restriction should consist of a constructor token followed by
    /// some number of arguments. We parse this in the obvious way, stopping
    /// the input when we hit something that isn't a base type.
    ///
    /// Note that, because of this, we might end up in a situation in which
    /// we throw an error after consuming a bunch of input, meaning that it
    /// will be impossible to recover.
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
                    expected: "Constructor name, comma, or close parenthesis in type restriction"
                        .into(),
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

    /// Parse a definition.
    ///
    /// A definition can include a structure definition, the definition of an enumeration,
    /// the declaration of some sort of operator, or a value definition. (This statement
    /// assumes that you consider a function a value, which is reasonable.)
    ///
    /// If this returns an error, you should not presume that you can recover from it.
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

        if let Ok(operator) = self.parse_operator() {
            return Ok(Def::Operator(operator));
        }

        if let Ok(fun_or_val) = self.parse_function_or_value() {
            return Ok(fun_or_val);
        }

        Err(ParserError::UnexpectedToken {
            file: self.file.clone(),
            span: next.span,
            token: next.token,
            expected: "'structure', 'enumeration', or a value identifier".into(),
        })
    }

    /// Parse a structure definition.
    ///
    /// Structure definitions should start with the keyword "structure". If they
    /// don't, this will return, but it will do so in a way that is recoverable.
    /// Otherwise, we'll start eating tokens and who knows what state we'll end
    /// in.
    pub fn parse_structure(&mut self) -> Result<StructureDef, ParserError> {
        let start_location = self.require_keyword("structure")?;

        let structure_name = self.parse_type_name("structure definition")?;
        self.require_token(Token::OpenBrace, "after a structure name")?;

        let mut fields = vec![];
        while let Some(field_definition) = self.parse_field_definition()? {
            fields.push(field_definition);
        }

        let brace =
            self.require_token(Token::CloseBrace, "at the end of a structure definition")?;

        let location = start_location.extend_to(&brace);

        Ok(StructureDef {
            name: structure_name,
            location,
            fields,
        })
    }

    /// Parse a name and field value for a field inside a structure constructor.
    ///
    /// In this case, what we mean is the full "foo: bar" syntax that goes inside a structure
    /// expression to declare a value.
    pub fn parse_field_value(&mut self) -> Result<Option<FieldValue>, ParserError> {
        let Ok(field) = self.parse_name("structure value") else {
            return Ok(None);
        };
        self.require_token(Token::Colon, "after a field name")?;
        let value = self.parse_expression()?;

        if let Some(end_token) = self.next()?
            && !matches!(end_token.token, Token::Comma)
        {
            self.save(end_token);
        }

        Ok(Some(FieldValue { field, value }))
    }

    /// Parse a name and field definition for a field inside a structure definition.
    ///
    /// In this case, what we mean is the full "foo: Bar" syntax that goes inside a
    /// structure type definition. Note, though, that we allow the ": Bar" to be
    /// elided in the case that the user wants to try to infer the type. In addition,
    /// recall that structure types can declare their individual fields public or
    /// not, so that information gets parsed as well.
    pub fn parse_field_definition(&mut self) -> Result<Option<StructureField>, ParserError> {
        let (export, start_location) = self.parse_export_class()?;
        let Ok(name) = self.parse_name("field definition") else {
            return Ok(None);
        };

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
                    expected: "colon, comma, or close brace after field name".into(),
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
                    expected: "looking for comma or close brace after field definition".into(),
                });
            }
        };

        let end_location = maybe_end_location
            .or_else(|| field_type.as_ref().map(|x| x.location()))
            .unwrap_or_else(|| name.location().unwrap().clone());
        let location = start_location.extend_to(&end_location);

        Ok(Some(StructureField {
            location,
            export,
            name,
            field_type,
        }))
    }

    /// Parse an enumeration declaration from the input stream.
    ///
    /// As with structures, this will cleanly abort if the first token is wrong,
    /// but if it makes it past that token, all bets are off.
    pub fn parse_enumeration(&mut self) -> Result<EnumerationDef, ParserError> {
        let start_location = self.require_keyword("enumeration")?;
        let enumeration_name = self.parse_type_name("enumeration definition")?;

        self.require_token(Token::OpenBrace, "after enumeration name")?;

        let mut variants = vec![];
        while let Some(variant_definition) = self.parse_enum_variant()? {
            variants.push(variant_definition);
        }

        let brace = self.require_token(Token::CloseBrace, "after enumeration options")?;

        let location = start_location.extend_to(&brace);

        Ok(EnumerationDef {
            name: enumeration_name,
            location,
            variants,
        })
    }

    /// Parse a variant of an enumeration in the enumeration definition.
    ///
    /// At this point in bang's lifecycle, enumerations can have zero or one arguments,
    /// but no more, which simplified parsing a trace.
    pub fn parse_enum_variant(&mut self) -> Result<Option<EnumerationVariant>, ParserError> {
        let Ok(name) = self.parse_type_name("variant definition") else {
            return Ok(None);
        };
        let start_location = name.location().unwrap().clone();

        let maybe_paren = self
            .next()?
            .ok_or_else(|| self.bad_eof("trying to understand enumeration variant"))?;
        let (argument, arg_location) = if matches!(maybe_paren.token, Token::OpenParen) {
            let t = self.parse_type()?;
            self.require_token(Token::CloseParen, "variant's type argument")?;
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
                    expected: "comma or close brace after enumeration variant".into(),
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

    /// Parse an operator declaration.
    ///
    /// Operator declarations are the only thing where we immediately modify the state
    /// of the parser, allowing the operator to be used immediately after it is declared.
    /// Note that by "declare", we mean that the operator is given a variable that it maps
    /// to; that variable can be declared further on in the file or even in another module,
    /// as we won't try to resolve it until later.
    ///
    /// Like most definitions, we'll abort cleanly if the first token isn't the "operator"
    /// keyword, but all bets are off after that.
    pub fn parse_operator(&mut self) -> Result<OperatorDef, ParserError> {
        let _operator = self.require_keyword("operator")?;

        unimplemented!()
    }

    /// Parse a function or a value.
    ///
    /// Technically speaking, functions are values, so the name can feel a little silly.
    /// However, we have some nice syntax for functions that avoids the need to put lambdas
    /// everywhere, and so we sort of treat them differently.
    fn parse_function_or_value(&mut self) -> Result<Def, ParserError> {
        let name = self.parse_name("function or value definition")?;
        let start = name.location().unwrap().clone();

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("type or value for definition"))?;

        match next.token {
            // If we see an open parenthesis next, we're looking at a nicely-formatted
            // function definition, such as:
            //
            // factorial(x: Int) : Int {
            //    match x {
            //      1 => 1,
            //      x => x * fact(x - 1),
            //    }
            // }
            // 
            // Or any of many variations of that.
            Token::OpenParen => {
                unimplemented!()
            }

            // If we see a colon, then someone's giving us a type for what is probably
            // some form of simple constant, such as:
            //
            // foo : Int = 4
            //
            // But honestly, there's a lot of odd possibilities of complicated things
            // they could write there.
            Token::Colon => {
                unimplemented!()
            }

            // If we see an equal sign, we're jumping right to the value part of the
            // definition, and we're doing something like this:
            //
            // foo = 4
            //
            // Again, though, you could write all sorts of interesting things after
            // that.
            Token::OperatorName(eq) if eq == "=" => {
                let value = self.parse_expression()?;

                Ok(Def::Value(ValueDef {
                    name,
                    location: start.extend_to(&value.location()),
                    value,
                }))
            }

            // Those should be the only cases, so if we get here, something weird
            // is going on.
            _ => Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "open parenthesis, colon, or equals after variable in definition".into(),
            }),
        }
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
                expected: "an 'match' to start a pattern match".into(),
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
                expected: "an open brace after the match expression".into(),
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
                expected: "a close brace to end a match expression".into(),
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
        // skip over anything we can just skip
        loop {
            let peeked = self
                .next()?
                .ok_or_else(|| self.bad_eof("looking for match case"))?;

            if matches!(peeked.token, Token::Comma) {
                continue;
            }

            let stop = matches!(peeked.token, Token::CloseBrace);

            self.save(peeked);
            if stop {
                return Ok(None);
            }

            break;
        }

        let pattern = self.parse_pattern()?;

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for an open brace after 'match'"))?;
        if !matches!(next.token, Token::Arrow) {
            return Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "an arrow after a pattern, as part of a match case".into(),
            });
        }

        let consequent = self.parse_expression()?;

        Ok(Some(MatchCase {
            pattern,
            consequent,
        }))
    }

    pub fn parse_pattern(&mut self) -> Result<Pattern, ParserError> {
        if let Ok(constant) = self.parse_constant() {
            return Ok(Pattern::Constant(constant));
        }

        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for a pattern to match"))?;

        match next.token {
            Token::ValueName(x) => {
                let name = Name::new(self.to_location(next.span), x);
                Ok(Pattern::Variable(name))
            }

            Token::TypeName(x) => {
                let type_name = Name::new(self.to_location(next.span.clone()), x);
                let start = self.to_location(next.span);

                let next = self
                    .next()?
                    .ok_or_else(|| self.bad_eof("looking for a pattern to match"))?;
                match next.token {
                    Token::OpenBrace => {
                        let mut fields = vec![];

                        while let Some(field_pattern) = self.parse_field_pattern()? {
                            fields.push(field_pattern)
                        }

                        let final_brace = self.next()?.ok_or_else(|| {
                            self.bad_eof("looking for closing brace in structure pattern.")
                        })?;
                        if !matches!(final_brace.token, Token::CloseBrace) {
                            return Err(ParserError::UnexpectedToken {
                                file: self.file.clone(),
                                span: final_brace.span,
                                token: final_brace.token,
                                expected: "closing brace in structure pattern".into(),
                            });
                        }
                        let final_brace_location = self.to_location(final_brace.span);

                        let structure_pattern = StructurePattern {
                            location: start.extend_to(&final_brace_location),
                            type_name,
                            fields,
                        };

                        Ok(Pattern::Structure(structure_pattern))
                    }

                    Token::DoubleColon => {
                        let vname = self.next()?.ok_or_else(|| {
                            self.bad_eof("looking for enumeration value name in pattern")
                        })?;

                        let variant_name = match vname.token {
                            Token::TypeName(s) => {
                                let loc = self.to_location(vname.span.clone());
                                Name::new(loc, s)
                            }

                            _ => {
                                return Err(ParserError::UnexpectedToken {
                                    file: self.file.clone(),
                                    span: vname.span,
                                    token: vname.token,
                                    expected: "enumeration value name in pattern".into(),
                                });
                            }
                        };

                        let mut final_location = self.to_location(vname.span);

                        let argument = if let Some(maybe_paren) = self.next()? {
                            if matches!(maybe_paren.token, Token::OpenParen) {
                                let sub_pattern = self.parse_pattern()?;

                                let tok = self.next()?.ok_or_else(|| {
                                    self.bad_eof(
                                        "looking for close paren after enum value argument",
                                    )
                                })?;
                                if !matches!(tok.token, Token::CloseParen) {
                                    return Err(ParserError::UnexpectedToken {
                                        file: self.file.clone(),
                                        span: tok.span,
                                        token: tok.token,
                                        expected: "close paren after enum value argument".into(),
                                    });
                                }

                                final_location = self.to_location(tok.span);

                                Some(Box::new(sub_pattern))
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        let location = start.extend_to(&final_location);

                        let pattern = EnumerationPattern {
                            location,
                            type_name,
                            variant_name,
                            argument,
                        };

                        Ok(Pattern::EnumerationValue(pattern))
                    }

                    _ => Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "An '::' or '{' after a type name in a pattern".into(),
                    }),
                }
            }

            _ => Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "The start of a pattern: a variable name or type name".into(),
            }),
        }
    }

    fn parse_field_pattern(&mut self) -> Result<Option<(Name, Option<Pattern>)>, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for structure pattern field name"))?;
        let name = match next.token {
            Token::CloseBrace => {
                self.save(next);
                return Ok(None);
            }

            Token::ValueName(s) => Name::new(self.to_location(next.span), s),

            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: next.span,
                    token: next.token,
                    expected: "a field name in a structure pattern".into(),
                });
            }
        };

        let next = self.next()?.ok_or_else(|| {
            self.bad_eof("looking for colon, comma, or brace after structure field name in pattern")
        })?;
        let sub_pattern = match next.token {
            Token::Comma => None,

            Token::CloseBrace => {
                self.save(next);
                None
            }

            Token::Colon => {
                let subpattern = self.parse_pattern()?;
                let next = self.next()?.ok_or_else(|| {
                    self.bad_eof("looking for comma or close brace after structure field")
                })?;

                match next.token {
                    Token::Comma => {}
                    Token::CloseBrace => self.save(next),
                    _ => {
                        return Err(ParserError::UnexpectedToken {
                            file: self.file.clone(),
                            span: next.span,
                            token: next.token,
                            expected: "comma or close brace after structure field".into(),
                        });
                    }
                }

                Some(subpattern)
            }

            _ => {
                return Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: next.span,
                    token: next.token,
                    expected: "colon, comma, or brace after structure field name in pattern".into(),
                });
            }
        };

        Ok(Some((name, sub_pattern)))
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
                expected: "an 'if' to start a conditional".into(),
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
                expected: "an open brace to start a block".into(),
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
                expected: "a close brace to end a block".into(),
            });
        }
        let end = self.to_location(next.span);

        if !ended_with_expr {
            let void_name = Name::new(end.clone(), "%prim%void");
            let void_ref = Expression::Reference(end.clone(), void_name);
            let void_call = Expression::Call(Box::new(void_ref), CallKind::Normal, vec![]);
            statements.push(Statement::Expression(void_call));
        }

        Ok(Expression::Block(start.extend_to(&end), statements))
    }

    pub fn parse_statement(&mut self) -> Result<Option<(Statement, bool)>, ParserError> {
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for a statement or close brace"))?;

        match next.token {
            Token::CloseBrace => {
                self.save(next);
                Ok(None)
            }

            Token::ValueName(ref l) if l == "let" => {
                self.save(next);
                Ok(Some((Statement::Binding(self.parse_let()?), false)))
            }

            _ => {
                self.save(next);
                let expr = Statement::Expression(self.parse_expression()?);

                let next = self
                    .next()?
                    .ok_or_else(|| self.bad_eof("looking for semicolon or close brace"))?;

                if matches!(next.token, Token::Semi) {
                    Ok(Some((expr, false)))
                } else {
                    self.save(next);
                    Ok(Some((expr, true)))
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
                expected: "a 'let' to open a binding statement".into(),
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
                    expected: "a variable name for the let binding".into(),
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
                expected: "an '=' after the variable name in a let binding".into(),
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
                expected: "a semicolon to finish a let statement".into(),
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
                        expected: "a base expression of a tighter-binding prefix operator".into(),
                    });
                }

                let rhs = self.parse_arithmetic(*pre_prec)?;
                let location = self.to_location(next.span);
                let opname = Name::new(location.clone(), n);
                let op_expr = Expression::Reference(location, opname);

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

                        let location = self.to_location(next.span);
                        let opname = Name::new(location.clone(), n);
                        let op_expr = Expression::Reference(location, opname);

                        lhs = Expression::Call(Box::new(op_expr), CallKind::Postfix, vec![lhs]);
                        continue;
                    }

                    let (left_pr, right_pr) = self.get_precedence(n);

                    if left_pr < level {
                        self.save(next);
                        break;
                    }

                    let rhs = self.parse_arithmetic(right_pr)?;
                    let location = self.to_location(next.span);
                    let name = Name::new(location.clone(), n);
                    let opref = Box::new(Expression::Reference(location, name));
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
                expected: "open paren for call arguments".into(),
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
                        expected: "comma or close paren in function arguments".into(),
                    });
                }
            }
        }

        Ok(args)
    }

    /// Parse a base expression.
    ///
    /// A base expression can be any number of things:
    ///   * A constant, of any form
    ///   * A variable name
    ///   * A constructor, like a structure constructor or an enumeration value
    ///   * A parenthesized expression of some other form
    ///   * A block
    ///
    /// Most of these can be identified by the first token in the input
    /// stream. If we don't recognize a valid first token in the input
    /// stream, we return an error and restore the original input stream
    /// state. However, if the first token leads us to a valid next state,
    /// we may not be able to recover the original stream state on an error.
    ///
    /// As a result, this should only be called when you're very confident
    /// that the next thing is going to be an expression.
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
                self.parse_block()
            }

            Token::OpenParen => {
                let inner = self.parse_expression()?;
                self.require_token(Token::CloseParen, "the end of a parenthesized expression")?;
                Ok(inner)
            }

            Token::TypeName(n) | Token::PrimitiveTypeName(n) => {
                let type_name = Name::new(self.to_location(next.span.clone()), n);
                let after_type_name = self.next()?.ok_or_else(|| {
                    self.bad_eof("looking for colon, open brace, or open paren in constructor")
                })?;

                match after_type_name.token {
                    Token::OpenBrace => {
                        let mut fields = vec![];

                        while let Some(field) = self.parse_field_value()? {
                            fields.push(field);
                        }

                        let brace = self.require_token(Token::CloseBrace, "end of structure value")?;

                        let sv = StructureExpr {
                            location: self.to_location(next.span).extend_to(&brace),
                            type_name,
                            fields,
                        };

                        Ok(Expression::Structure(sv))
                    }

                    Token::DoubleColon => {
                        let vname = self
                            .next()?
                            .ok_or_else(|| self.bad_eof("looking for enumeration value name"))?;

                        let variant_name = match vname.token {
                            Token::TypeName(s) => {
                                let loc = self.to_location(vname.span.clone());
                                Name::new(loc, s)
                            }

                            _ => {
                                return Err(ParserError::UnexpectedToken {
                                    file: self.file.clone(),
                                    span: vname.span,
                                    token: vname.token,
                                    expected: "enumeration value name".into(),
                                });
                            }
                        };

                        let (argument, end_loc) = if let Some(maybe_paren) = self.next()? {
                            if matches!(maybe_paren.token, Token::OpenParen) {
                                let expr = self.parse_expression()?;
                                let closer = self.require_token(Token::CloseParen, "after variant argument")?;

                                (Some(Box::new(expr)), closer)
                            } else {
                                self.save(maybe_paren);
                                (None, self.to_location(vname.span))
                            }
                        } else {
                            (None, self.to_location(vname.span))
                        };

                        let ev = EnumerationExpr {
                            location: self.to_location(next.span).extend_to(&end_loc),
                            type_name,
                            variant_name,
                            argument,
                        };

                        Ok(Expression::Enumeration(ev))
                    }

                    _ => Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: after_type_name.span,
                        token: after_type_name.token,
                        expected: "colon, open brace, or open paren in constructor".into(),
                    }),
                }
            }

            Token::ValueName(n) | Token::PrimitiveValueName(n) => {
                let location = self.to_location(next.span);
                let name = Name::new(location.clone(), n);
                Ok(Expression::Reference(location, name))
            }

            _ => {
                self.save(next.clone());
                Err(ParserError::UnexpectedToken {
                    file: self.file.clone(),
                    span: next.span,
                    token: next.token,
                    expected: "some base expression or an open brace".into(),
                })
            }
        }
    }

    /// Parse a type from the input stream.
    ///
    /// Obviously, there are a lot of ways for things to not be a valid
    /// function type. As it can, this will try to leave things in the
    /// original state on an error, but that won't always be possible. So
    /// it's probably best to only try to call this when you're sure there
    /// should be a type sitting there.
    pub fn parse_type(&mut self) -> Result<Type, ParserError> {
        let mut args = Vec::new();

        while let Ok(t) = self.parse_type_application() {
            args.push(t);
        }

        let Some(maybe_arrow) = self.next()? else {
            match args.pop() {
                None => {
                    return Err(ParserError::UnacceptableEof {
                        file: self.file.clone(),
                        place: "parsing function type or type".into(),
                    });
                }

                Some(t) if args.is_empty() => return Ok(t),

                Some(_) => {
                    return Err(ParserError::UnacceptableEof {
                        file: self.file.clone(),
                        place: "looking for '->' in function type".into(),
                    });
                }
            }
        };

        if maybe_arrow.token == Token::Arrow {
            let right = self.parse_type()?;
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
                expected: "'->' in function type".into(),
            })
        }
    }

    /// Parse a type application.
    ///
    /// Type applications must start with a type name (a capitalized variable
    /// name). If we don't find one, we immediately error out. However if we
    /// do find one, we will then eat as many base types as we can until we
    /// run into an error.
    ///
    /// If we don't find a type name immediately, we will return an error but
    /// leave the parse stream unchanged. If we parse a bunch of base types
    /// correctly, the stream will be left at the start of the first non-base-type
    /// token. However, this function can leave things in a weird state if there
    /// is an open parenthesis that tries to enclose something that's not a type.
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

    /// Parse a base type from the input stream.
    ///
    /// A "base type" is a type variable, a primitive type name, a type name,
    /// or a parenthesized version of some other type. This function will return
    /// an error if it can't find one of these things, and will *attempt* to
    /// return the stream unmodified in the event of an error. However, if it
    /// sees a parenthesis and tries to parse a nested, complex type, it may
    /// not be possible to recover the state precisely.
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
                        expected: "close parenthesis to finish a type".into(),
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
                    expected: "type constructor, type variable, or primitive type".into(),
                })
            }
        }
    }

    /// Try to parse a constant value from the input stream.
    ///
    /// If we don't find a name, the stream should be returned in the same state
    /// at which it entered this function.
    pub(crate) fn parse_constant(&mut self) -> Result<ConstantValue, ParserError> {
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
                    expected: "constant value".into(),
                })
            }
        }
    }

    /// Try to parse a name from the input stream.
    ///
    /// If we don't find a name, the stream should be returned in the same state
    /// at which it entered this function.
    fn parse_name(&mut self, place: &'static str) -> Result<Name, ParserError> {
        let maybe_name = self
            .next()?
            .ok_or_else(|| self.bad_eof(format!("looking for a name in {place}")))?;

        if let Token::ValueName(x) = maybe_name.token {
            Ok(Name::new(self.to_location(maybe_name.span), x))
        } else {
            self.save(maybe_name.clone());
            Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: maybe_name.span,
                token: maybe_name.token,
                expected: format!("looking for a name in {place}"),
            })
        }
    }

    /// Try to parse a type name from the input stream.
    ///
    /// If we don't find a name, the stream should be returned in the same state
    /// at which it entered this function.
    fn parse_type_name(&mut self, place: &'static str) -> Result<Name, ParserError> {
        let maybe_name = self
            .next()?
            .ok_or_else(|| self.bad_eof(format!("looking for a type name in {place}")))?;

        if let Token::TypeName(x) = maybe_name.token {
            Ok(Name::new(self.to_location(maybe_name.span), x))
        } else {
            self.save(maybe_name.clone());
            Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: maybe_name.span,
                token: maybe_name.token,
                expected: format!("looking for a type name in {place}"),
            })
        }
    }
}
