use crate::syntax::error::ParserError;
use crate::syntax::tokens::{Lexer, LocatedToken, Token};
use crate::syntax::*;

pub struct Parser<'a> {
    file_id: usize,
    lexer: Lexer<'a>,
    known_tokens: Vec<LocatedToken>,
}

impl<'a> Parser<'a> {
    pub fn new(file_id: usize, lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            file_id,
            lexer,
            known_tokens: vec![],
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
                    file_id: self.file_id,
                    error,
                })
        }
    }

    fn save(&mut self, token: LocatedToken) {
        self.known_tokens.push(token)
    }

    fn bad_eof(&mut self, place: &'static str) -> ParserError {
        ParserError::UnacceptableEof {
            file_id: self.file_id,
            place,
        }
    }

    fn to_location(&self, span: Range<usize>) -> Location {
        Location {
            file_id: self.file_id,
            span,
        }
    }

    pub fn parse_type(&mut self) -> Result<Type, ParserError> {
        self.parse_function_type()
    }

    fn parse_function_type(&mut self) -> Result<Type, ParserError> {
        let mut args = Vec::new();

        while let Ok(t) = self.parse_type_application() {
            println!("got argument type: {t:?}");
            args.push(t);
        }

        let Some(maybe_arrow) = self.next()? else {
            println!("no arrow token");
            match args.pop() {
                None => {
                    return Err(ParserError::UnacceptableEof {
                        file_id: self.file_id,
                        place: "parsing function type or type",
                    });
                }

                Some(t) if args.len() == 0 => return Ok(t),

                Some(_) => {
                    return Err(ParserError::UnacceptableEof {
                        file_id: self.file_id,
                        place: "looking for '->' in function type",
                    });
                }
            }
        };

        if maybe_arrow.token == Token::Arrow {
            println!("found function arrow");
            let right = self.parse_function_type()?;
            Ok(Type::Function(args, Box::new(right)))
        } else if args.len() == 1 {
            println!("found non function arrow token {}", maybe_arrow.token);
            Ok(args.pop().expect("length = 1 works"))
        } else {
            self.save(maybe_arrow.clone());
            let LocatedToken { token, span } = maybe_arrow;

            Err(ParserError::UnexpectedToken {
                file_id: self.file_id,
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
            Token::TypeName(x) => Type::Constructor(self.to_location(span), x),
            Token::PrimitiveTypeName(x) => Type::Primitive(self.to_location(span), x),
            _ => {
                println!("saving {token}");
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
            Token::TypeName(x) => Ok(Type::Constructor(self.to_location(span), x)),
            Token::PrimitiveTypeName(x) => Ok(Type::Primitive(self.to_location(span), x)),
            Token::ValueName(x) => Ok(Type::Variable(self.to_location(span), x)),
            token => {
                self.save(LocatedToken {
                    token: token.clone(),
                    span: span.clone(),
                });

                Err(ParserError::UnexpectedToken {
                    file_id: self.file_id,
                    span,
                    token,
                    expected: "type constructor, type variable, or primitive type",
                })
            }
        }
    }

    pub fn parse_constant(&mut self) -> Result<ConstantValue, ParserError> {
        let LocatedToken { token, span } = self
            .next()?
            .ok_or_else(|| self.bad_eof("looking for a constant"))?;

        match token {
            Token::Integer(iwb) => Ok(ConstantValue::Integer(self.to_location(span), iwb)),
            Token::Character(c) => Ok(ConstantValue::Character(self.to_location(span), c)),
            Token::String(s) => Ok(ConstantValue::String(self.to_location(span), s)),
            _ => Err(ParserError::UnexpectedToken {
                file_id: self.file_id,
                span,
                token,
                expected: "constant value",
            }),
        }
    }
}
