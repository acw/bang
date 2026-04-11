use crate::syntax::error::ParserError;
use crate::syntax::precedence_table::{
    Associativity, OperatorClass, OperatorType, PrecedenceTable,
};
use crate::syntax::tokens::{Lexer, LocatedToken, Token};
use crate::syntax::*;
use internment::ArcIntern;
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
    precedence_table: PrecedenceTable,
    known_tokens: Vec<LocatedToken>,
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
            precedence_table: PrecedenceTable::default(),
            known_tokens: vec![],
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

    /// Return the current precedence table.
    ///
    #[cfg(test)]
    pub fn precedence_table(&mut self) -> &mut PrecedenceTable {
        &mut self.precedence_table
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

    /// See if the next token is an operator, as expected.
    ///
    /// If it isn't, this routine will provide an error, but it will make
    /// sure to put the token back into the stream.
    fn require_operator(&mut self, op: &'static str) -> Result<Location, ParserError> {
        match self.next()? {
            None => Err(self.bad_eof(format!("looking for symbol '{op}'"))),
            Some(ltoken) => match ltoken.token {
                Token::OperatorName(s) if s.as_str() == op => Ok(self.to_location(ltoken.span)),
                _ => {
                    self.save(ltoken.clone());
                    Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: ltoken.span,
                        token: ltoken.token,
                        expected: format!("symbol {op}"),
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
            if let Some(next_token) = self.next()? {
                self.save(next_token);
                definitions.push(self.parse_definition()?);
            } else {
                return Ok(Module { definitions });
            }
        }
    }

    #[allow(unused)]
    #[cfg(not(coverage))]
    fn print_next_token(&mut self, comment: &str) {
        let token = self.next().expect("can get token");
        println!(
            "[{comment}] next token will be {:?}",
            token.as_ref().map(|x| x.token.clone())
        );
        if let Some(token) = token {
            self.save(token);
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

        let _ = self.require_token(Token::Comma, "");

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

        match next.token {
            Token::ValueName(ref x) if x == "structure" => {
                self.save(next);
                Ok(Def::Structure(self.parse_structure()?))
            }

            Token::ValueName(ref x) if x == "enumeration" => {
                self.save(next);
                Ok(Def::Enumeration(self.parse_enumeration()?))
            }

            Token::ValueName(ref x)
                if x == "operator" || x == "prefix" || x == "infix" || x == "postfix" =>
            {
                self.save(next);
                Ok(Def::Operator(self.parse_operator()?))
            }

            Token::ValueName(_) => {
                self.save(next);
                self.parse_function_or_value()
            }

            _ => Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: next.span,
                token: next.token,
                expected: "'structure', 'enumeration', 'operator', or a value identifier".into(),
            }),
        }
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
    /// Like most definitions, we'll abort cleanly if the first token isn't "operator",
    /// "infix", "postfix", or "prefix" keywords, but all bets are off after that.
    pub fn parse_operator(&mut self) -> Result<OperatorDef, ParserError> {
        let (start, operator_type, associativity) = {
            let mut optype = OperatorType::Infix;
            let mut start = None;
            let mut assoc = Associativity::None;

            if let Ok(loc) = self.require_keyword("prefix") {
                optype = OperatorType::Prefix;
                start = Some(loc);
            } else if let Ok(loc) = self.require_keyword("postfix") {
                optype = OperatorType::Postfix;
                start = Some(loc);
            } else if let Ok(loc) = self.require_keyword("infix") {
                start = Some(loc);

                if self.require_keyword("right").is_ok() {
                    assoc = Associativity::Right;
                } else if self.require_keyword("left").is_ok() {
                    assoc = Associativity::Left;
                }
            }

            let oploc = self.require_keyword("operator")?;
            (start.unwrap_or(oploc), optype, assoc)
        };
        let operator_name = self.parse_operator_name("operator definition")?;

        let level = if self.require_keyword("at").is_ok() {
            let next = self
                .next()?
                .ok_or_else(|| self.bad_eof("precedence value in operator definition"))?;

            match next.token {
                Token::Integer(int_with_base) if int_with_base.value < 10 => {
                    int_with_base.value as u8
                }

                Token::Integer(ref int_with_base) => {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token.clone(),
                        expected: format!(
                            "number defining operator precedence ({} is too large",
                            int_with_base.value
                        ),
                    });
                }

                _ => {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "number defining operator precedence".into(),
                    });
                }
            }
        } else {
            5
        };

        let _ = self.require_token(Token::Arrow, "operator definition")?;

        let function_name = self.parse_name("operator function definition")?;
        let end = self.require_token(Token::Semi, "end of operator definition")?;

        self.precedence_table.add_operator(
            OperatorClass::Value,
            operator_type,
            associativity,
            operator_name.as_printed().to_string(),
            level,
        );

        Ok(OperatorDef {
            location: start.extend_to(&end),
            operator_name,
            function_name,
        })
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
                self.save(next);
                let arguments = self.parse_function_def_arguments()?;
                let mut return_type = None;

                if self.require_token(Token::Colon, "return type").is_ok() {
                    return_type = Some(self.parse_type()?);
                }

                let Expression::Block(end, body) = self.parse_block()? else {
                    panic!("parse_block returned something that wasn't a block.");
                };

                Ok(Def::Function(FunctionDef {
                    name,
                    location: start.extend_to(&end),
                    arguments,
                    return_type,
                    body,
                }))
            }

            // If we see a colon, then someone's giving us a type for what is probably
            // some form of simple constant, such as:
            //
            // foo : Int = 4;
            //
            // But honestly, there's a lot of odd possibilities of complicated things
            // they could write there.
            Token::Colon => {
                let value_type = self.parse_type()?;
                let _ = self.require_operator("=")?;
                let value = self.parse_expression()?;
                let end = self.require_token(Token::Semi, "at end of definition")?;

                Ok(Def::Value(ValueDef {
                    name,
                    location: start.extend_to(&end),
                    mtype: Some(value_type),
                    value,
                }))
            }

            // If we see an equal sign, we're jumping right to the value part of the
            // definition, and we're doing something like this:
            //
            // foo = 4;
            //
            // Again, though, you could write all sorts of interesting things after
            // that.
            Token::OperatorName(eq) if eq == "=" => {
                let value = self.parse_expression()?;
                let end = self.require_token(Token::Semi, "at end of definition")?;

                Ok(Def::Value(ValueDef {
                    name,
                    location: start.extend_to(&end),
                    mtype: None,
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

    /// Parse the arguments to a function declaration.
    ///
    /// Function arguments should have types, but don't have to. This function assumes
    /// that it's starting at the opening parenthesis, and will error (cleanly) if it
    /// isn't.
    fn parse_function_def_arguments(&mut self) -> Result<Vec<FunctionArg>, ParserError> {
        let _ = self.require_token(Token::OpenParen, "start of function argument definition")?;
        let mut result = vec![];
        let mut just_skipped_comma = false;

        loop {
            let next = self
                .next()?
                .ok_or_else(|| self.bad_eof("parsing function arguments"))?;

            if matches!(next.token, Token::CloseParen) {
                break;
            }

            if matches!(next.token, Token::Comma) {
                if just_skipped_comma {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "after another comma in function arguments".into(),
                    });
                }

                just_skipped_comma = true;
                continue;
            }

            self.save(next);
            just_skipped_comma = false;
            let name = self.parse_name("function argument name")?;
            let mut arg_type = None;

            if self.require_token(Token::Colon, "").is_ok() {
                arg_type = Some(self.parse_type()?);
            }

            result.push(FunctionArg { name, arg_type });
        }

        Ok(result)
    }

    /// Parse a single expression out of the input stream.
    ///
    /// Because expressions can start with so many possible tokens, it's very
    /// likely that if you call this, the input stream will be corrupted by any
    /// errors this function returns. So you should be careful to only call it
    /// in situations that don't require rollback.
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

    /// Parse a match expression.
    ///
    /// This function does assume that the next token in the input stream will
    /// be the "match" keyword, and will error immediately (albeit, saving the
    /// stream) if it isn't. So you *can* use this if you're not sure this is
    /// a match expression, and want to escape if it isn't.
    fn parse_match_expression(&mut self) -> Result<MatchExpr, ParserError> {
        let start = self.require_keyword("match")?;
        let value = Box::new(self.parse_arithmetic(0)?);
        self.require_token(Token::OpenBrace, "start of a match case list")?;

        let mut cases = vec![];
        while let Some(case) = self.parse_match_case()? {
            cases.push(case);
        }

        let end = self.require_token(Token::CloseBrace, "end of a match case list")?;
        Ok(MatchExpr {
            location: start.extend_to(&end),
            value,
            cases,
        })
    }

    /// Parse a single match case.
    ///
    /// A match case consists of a pattern, a double-arrow, and then an expression
    /// describing what to do if that pattern matches the expression. It may or may
    /// not conclude with a comma.
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
        self.require_token(Token::Arrow, "after pattern in match clause")?;

        let consequent = self.parse_expression()?;

        Ok(Some(MatchCase {
            pattern,
            consequent,
        }))
    }

    /// Parse a pattern from the input stream.
    ///
    /// Patterns are a recursive, complex structure without a clear opening token.
    /// So ... you better be sure that you want a pattern when you call this,
    /// because you're almost certainly not going to be able to recover and try
    /// something else if this breaks.
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

                        let end =
                            self.require_token(Token::CloseBrace, "after structure pattern")?;
                        let structure_pattern = StructurePattern {
                            location: start.extend_to(&end),
                            type_name,
                            fields,
                        };

                        Ok(Pattern::Structure(structure_pattern))
                    }

                    Token::DoubleColon => {
                        let variant_name =
                            self.parse_type_name("enumeration pattern variant name")?;

                        let mut final_location = variant_name.location().unwrap().clone();

                        let argument = if let Some(maybe_paren) = self.next()? {
                            if matches!(maybe_paren.token, Token::OpenParen) {
                                let sub_pattern = self.parse_pattern()?;
                                final_location = self.require_token(
                                    Token::CloseParen,
                                    "after enumeration pattern argument",
                                )?;

                                Some(Box::new(sub_pattern))
                            } else {
                                self.save(maybe_paren);
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

    /// Parse a field pattern.
    ///
    /// For reference, a field pattern is either just the name of a field, or a name of a
    /// field plus a colon and some form of subpattern. This can be used to either rename
    /// a field or to only match when a field has a particular value.
    ///
    /// Regardless, this should start with a name, and if it doesn't start with a name,
    /// we'll return Ok(None) to indicate that we're done parsing field patterns. If we
    /// do get a name and then reach some sort of error, though, who knows what state we'll
    /// end up in.
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

    /// Parse an if expression.
    ///
    /// Like many of these functions, there's a nice indicator immediately available to us
    /// so that we know whether or not this is an if statement. If we don't see it, we will
    /// return with an error but the input stream will be clean. However, if we do see one,
    /// and there's an error down the line, then there's nothing we can do.
    fn parse_if_expression(&mut self) -> Result<ConditionalExpr, ParserError> {
        let start = self.require_keyword("if")?;
        let test = self.parse_arithmetic(0)?;
        let consequent = self.parse_block()?;
        let mut alternative = None;

        if self.require_keyword("else").is_ok() {
            alternative = Some(Box::new(self.parse_block()?));
        }

        let end = alternative
            .as_ref()
            .map(|x| x.location())
            .unwrap_or_else(|| consequent.location());

        Ok(ConditionalExpr {
            location: start.extend_to(&end),
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternative,
        })
    }

    /// Parse a block.
    ///
    /// A block starts with an open brace -- so if we don't see one, we'll exit cleanly --
    /// but gets real complicated after that. So, once again, be thoughtful about how this
    /// is called.
    pub fn parse_block(&mut self) -> Result<Expression, ParserError> {
        let start = self.require_token(Token::OpenBrace, "start of a block")?;

        let mut statements = vec![];
        let mut ended_with_expr = false;

        while let Some((stmt, terminal)) = self.parse_statement()? {
            statements.push(stmt);
            if terminal {
                ended_with_expr = true;
                break;
            }
        }

        let end = self.require_token(Token::CloseBrace, "end of a block")?;

        if !ended_with_expr {
            let void_name = Name::new(end.clone(), "%prim%void");
            let void_ref = Expression::Reference(end.clone(), void_name);
            let void_call = Expression::Call(Box::new(void_ref), CallKind::Normal, vec![]);
            statements.push(Statement::Expression(void_call));
        }

        Ok(Expression::Block(start.extend_to(&end), statements))
    }

    /// Parse a statement, or return None if we're now done with parsing a block.
    ///
    /// We know we're done parsing a block when we hit a close brace, basically. We
    /// should ignore excess semicolons cleanly, and that sort of thing. Because
    /// statements vary pretty widely, you should not assume that the input is clean
    /// on any sort of error.
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

                Token::Semi => continue,

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

    /// Parse a let statement.
    ///
    /// This will assume that the first token in the stream is a "let", and be upset if
    /// it is not. However, it will be upset cleanly, which is nice.
    pub fn parse_let(&mut self) -> Result<BindingStmt, ParserError> {
        let start = self.require_keyword("let")?;
        let mutable = self.require_keyword("mut").is_ok();
        let variable = self.parse_name("let binding")?;
        let _ = self.require_operator("=")?;
        let value = self.parse_expression()?;
        let end = self.require_token(Token::Semi, "let statement")?;

        Ok(BindingStmt {
            location: start.extend_to(&end),
            mutable,
            variable,
            value,
        })
    }

    /// Parse an arithmetic expression, obeying the laws of precedence.
    ///
    /// This is an implementation of Pratt Parsing, although I've probably done it in
    /// a much more awkward way than necessary. I was heavily inspired and/or stole
    /// code directly from [this
    /// article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html),
    /// which was instrumental in its design. All errors mine.
    ///
    /// Note that because arithmetic expressions can start with so many tokens, you
    /// should only call this function if you are absolutely sure that there's an
    /// expression waiting for you, and it would be an error if there wasn't.
    pub fn parse_arithmetic(&mut self, level: u8) -> Result<Expression, ParserError> {
        // start by checking for prefix operators.
        let next = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for arithmetic expression"))?;

        let mut lhs = if let Token::OperatorName(ref n) = next.token {
            if let Some((pre_prec, _)) =
                self.precedence_table
                    .get_precedence(OperatorClass::Value, OperatorType::Prefix, n)
            {
                if pre_prec < level {
                    self.save(next.clone());
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "a base expression of a tighter-binding prefix operator".into(),
                    });
                }

                let rhs = self.parse_arithmetic(pre_prec)?;
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
                    if let Some((postprec, _)) = self.precedence_table.get_precedence(
                        OperatorClass::Value,
                        OperatorType::Postfix,
                        n,
                    ) {
                        if postprec < level {
                            self.save(next);
                            break;
                        }

                        let location = self.to_location(next.span);
                        let opname = Name::new(location.clone(), n);
                        let op_expr = Expression::Reference(location, opname);

                        lhs = Expression::Call(Box::new(op_expr), CallKind::Postfix, vec![lhs]);
                        continue;
                    }

                    let (left_pr, right_pr) = self
                        .precedence_table
                        .get_precedence(OperatorClass::Value, OperatorType::Infix, n)
                        .unwrap_or((19, 20));

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

    /// Parse the arguments to a function call.
    ///
    /// We assume that, at this point, you have eaten the thing you're calling out of
    /// the input stream, and are on the parenthesis that defines the arguments to the
    /// function. If you're not there, then this will error, but in a way that you can
    /// recover from.
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let _ = self.require_token(Token::OpenParen, "for function arguments")?;
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
                let Some(after_type_name) = self.next()? else {
                    return Ok(Expression::Reference(
                        type_name.location().unwrap().clone(),
                        type_name,
                    ));
                };

                match after_type_name.token {
                    Token::OpenBrace => {
                        let mut fields = vec![];

                        while let Some(field) = self.parse_field_value()? {
                            fields.push(field);
                        }

                        let brace =
                            self.require_token(Token::CloseBrace, "end of structure value")?;

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
                                let closer = self
                                    .require_token(Token::CloseParen, "after variant argument")?;

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

                    _ => {
                        self.save(after_type_name);
                        Ok(Expression::Reference(
                            type_name.location().unwrap().clone(),
                            type_name,
                        ))
                    }
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
        self.pratt_parser(
            OperatorClass::Type,
            &mut || self.parse_base_type(),
            Token::OpenBrace,
            Token::CloseBrace,
            &mut |loc,name| Type::Variable(loc, name),
            &mut |fun, arg| Type::Application(Box::new(fun), vec![arg]),
            &mut |fun, arg| Type::Application(Box::new(fun), vec![arg]),
            &mut |fun, arg1, arg2| Type::Application(Box::new(fun), vec![arg1,arg2]),
            &mut |fun, args| Type::Application(Box::new(fun), args),
            0,
        )
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

        if args.is_empty() {
            Ok(constructor)
        } else {
            Ok(Type::Application(Box::new(constructor), args))
        }
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

    /// Try to parse an operator from the input stream.
    ///
    /// If we don't find a name, the stream should be returned in the same state
    /// at which it entered this function.
    fn parse_operator_name(&mut self, place: &'static str) -> Result<Name, ParserError> {
        let maybe_name = self
            .next()?
            .ok_or_else(|| self.bad_eof(format!("looking for a type name in {place}")))?;

        if let Token::OperatorName(x) = maybe_name.token {
            Ok(Name::new(self.to_location(maybe_name.span), x))
        } else {
            self.save(maybe_name.clone());
            Err(ParserError::UnexpectedToken {
                file: self.file.clone(),
                span: maybe_name.span,
                token: maybe_name.token,
                expected: format!("looking for an operator name in {place}"),
            })
        }
    }

    /// Parse an expression, obeying the relevant laws of precedence.
    ///
    /// This is an implementation of Pratt Parsing, although I've probably done it in
    /// a much more awkward way than necessary. I was heavily inspired and/or stole
    /// code directly from [this
    /// article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html),
    /// which was instrumental in its design. All errors mine.
    ///
    /// Note that because arithmetic expressions can start with so many tokens, you
    /// should only call this function if you are absolutely sure that there's an
    /// expression waiting for you, and it would be an error if there wasn't.
    #[allow(clippy::too_many_arguments)]
    #[allow(dead_code)]
    pub fn pratt_parser<T>(
        &mut self,
        operator_class: OperatorClass,
        base_parser: &mut dyn FnMut() -> Result<T, ParserError>,
        call_open: Token,
        call_close: Token,
        ref_builder: &dyn Fn(Location, Name) -> T,
        prefix_builder: &dyn Fn(T, T) -> T,
        postfix_builder: &dyn Fn(T, T) -> T,
        infix_builder: &dyn Fn(T, T, T) -> T,
        call_builder: &dyn Fn(T, Vec<T>) -> T,
        level: u8,
    ) -> Result<T, ParserError>
    {
         // first we figure out what's on the left hand side of this thing. We have
         // to check for an operator here because we may be seeing a prefix expression.
         let mut lhs;
    
         // Check if the next token is a prefix operator. We need to extract this
         // information and then drop the borrow before calling base_parser().
         let prefix_op_info = {
             let next = self.next()?.ok_or_else(|| ParserError::UnacceptableEof {
                 file: self.file.clone(),
                 place: "parsing expression or type with operators".into(),
             })?;
    
             if let Token::OperatorName(ref operator_name) = next.token {
                 let precedence = self.precedence_table.get_precedence(
                     operator_class,
                     OperatorType::Prefix,
                     operator_name,
                 );
                 precedence
                     .map(|(p, _)| (p, operator_name.clone(), next.span.clone(), next.token.clone()))
             } else {
                 self.save(next);
                 None
             }
         };
    
         match prefix_op_info {
             None => {
                 lhs = base_parser()?;
             }
    
             Some((precedence_level, _, span, token)) if precedence_level < level => {
                 return Err(ParserError::UnexpectedToken {
                     file: self.file.clone(),
                     span,
                     token,
                     expected: "a base expression or type of a tighter-binding prefix operator".into(),
                 });
             }
    
             Some((_, operator_name, span, _)) => {
                 let subexpr = base_parser()?;
                 let location = self.to_location(span);
                 let opname = Name::new(location.clone(), &operator_name);
                 let opref = ref_builder(location, opname);
                 lhs = prefix_builder(opref, subexpr);
             }
         }
    
         loop {
             let Some(next) = self.next()? else {
                 return Ok(lhs);
             };
    
             if next.token == call_open {
                let mut arguments = vec![];
                let _ = self.lexer.next();

                loop {
                    let next = self.next()?.ok_or_else(||
                        self.bad_eof("parsing expression or type with operators"))?;

                    if next.token == call_close {
                        self.save(next);
                        break;
                    }

                    arguments.push(base_parser()?);

                    let next = self.next()?.ok_or_else(||
                        self.bad_eof("parsing expression or type with operators"))?;

                    if next.token == Token::Comma {
                        continue;
                    }

                    if next.token == call_close {
                        self.save(next);
                        break;
                    }

                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: next.span,
                        token: next.token,
                        expected: "comma or close".into(),
                    });
                }

                let enforced_close = self.next()?.ok_or_else(||
                    self.bad_eof("looking for close paren for call (expression or type)"))?;
                if enforced_close.token != call_close {
                    return Err(ParserError::UnexpectedToken {
                        file: self.file.clone(),
                        span: enforced_close.span,
                        token: enforced_close.token,
                        expected: "expected close".into(),
                    });
                }

                return Ok(call_builder(arguments));
             }
    
             let start_expr = if let Token::OperatorName(ref operator_name) = next.token {
                 // Clone immediately to release the borrow on `next` (the peeked reference),
                 // so we can call token_stream.next() after the precedence checks.
                 let operator_name = operator_name.clone();

                 // Check for postfix first. If '++' is both postfix and infix it will
                 // always be treated as postfix — an inherently ambiguous situation.
                 let postfix_precedence = self.precedence_table.get_precedence(
                     operator_class,
                     OperatorType::Postfix,
                     &operator_name,
                 );

                 if let Some((postfix_precedence, _)) = postfix_precedence {
                     if postfix_precedence < level {
                         self.save(next);
                         break;
                     }
                     let location = self.to_location(next.span);
                     let opname = Name::new(location.clone(), &operator_name);
                     let op_expr = ref_builder(location, opname);
                     lhs = postfix_builder(op_expr, lhs);
                     continue;
                 }

                 let (left_infix, right_infix) = self.precedence_table.get_precedence(
                     operator_class,
                     OperatorType::Infix,
                     &operator_name,
                 ).unwrap_or((19, 20));

                 if left_infix < level {
                     break;
                 }
                 // Consume only after deciding to use this operator as infix.
                 Some((right_infix, next.span.clone(), operator_name))
             } else {
                 None
             };
    
             if let Some((right_infix, span, operator_name)) = start_expr {
                 let rhs = self.pratt_parser(
                    operator_class,
                    base_parser,
                    call_open,
                    call_close,
                    ref_builder,
                    prefix_builder,
                    postfix_builder,
                    infix_builder,
                    call_builder,
                    right_infix,
                 )?;
    
                 let location = self.to_location(span);
                 let name = Name::new(location.clone(), operator_name);
                 let opref = ref_builder(location, name);
                 
                 lhs = infix_builder(opref, lhs, rhs);
            }
    
            break;
         }
    
         Ok(lhs)
    }
}
