use crate::syntax::IntegerWithBase;
use crate::syntax::error::LexerError;
use proptest_derive::Arbitrary;
use std::fmt;
use std::ops::Range;
use std::str::CharIndices;

#[derive(Clone)]
pub struct LocatedToken {
    pub token: Token,
    pub span: Range<usize>,
}

/// A single token of the input stream; used to help the parsing function over
/// more concrete things than bytes.
///
/// The [`std::fmt::Display`] implementation is designed to round-trip, so those
/// needing a more regular or descriptive option should consider using the
/// [`std::fmt::Debug`] implementation instead.
#[derive(Clone, Debug, PartialEq, Eq, Arbitrary)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenBrace,
    CloseBrace,
    Semi,
    Colon,
    Comma,
    BackTick,
    Arrow,
    Lambda(bool),

    TypeName(#[proptest(regex = r"[A-Z][a-zA-Z0-9_]*")] String),
    ValueName(#[proptest(regex = r"[a-z_][a-zA-Z0-9_]*")] String),
    OperatorName(
        #[proptest(
            regex = r"[\~\!\@\#\$\%\^\&\*\+\-\=\.<>\?\|][\~\!\@\#\$\%\^\&\*\+\-\=\.<>\?\|_]*",
            filter = "|x| x != \"->\""
        )]
        String,
    ),

    PrimitiveTypeName(#[proptest(regex = r"[A-Z][a-zA-Z0-9_]*")] String),
    PrimitiveValueName(#[proptest(regex = r"[a-z_][a-zA-Z0-9_]*")] String),

    Integer(IntegerWithBase),
    Character(char),
    String(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenSquare => write!(f, "["),
            Token::CloseSquare => write!(f, "]"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Semi => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::BackTick => write!(f, "`"),
            Token::Arrow => write!(f, "->"),
            Token::Lambda(false) => write!(f, "\\"),
            Token::Lambda(true) => write!(f, "λ"),
            Token::TypeName(str) => write!(f, "{str}"),
            Token::ValueName(str) => write!(f, "{str}"),
            Token::OperatorName(str) => write!(f, "{str}"),
            Token::PrimitiveTypeName(str) => write!(f, "prim%{str}"),
            Token::PrimitiveValueName(str) => write!(f, "prim%{str}"),
            Token::Integer(IntegerWithBase { base, value }) => match base {
                None => write!(f, "{value}"),
                Some(2) => write!(f, "0b{value:b}"),
                Some(8) => write!(f, "0o{value:o}"),
                Some(10) => write!(f, "0d{value}"),
                Some(16) => write!(f, "0x{value:x}"),
                Some(base) => write!(f, "<illegal number token base={base} value={value}>"),
            },
            Token::Character(c) => write!(f, "{c:?}"),
            Token::String(s) => write!(f, "{s:?}"),
        }
    }
}

#[allow(private_interfaces)]
pub enum Lexer<'a> {
    Working(LexerState<'a>),
    Errored(LexerError),
    Done(usize),
}

struct LexerState<'a> {
    stream: CharIndices<'a>,
    buffer: Option<(usize, char)>,
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(value: &'a str) -> Self {
        Lexer::Working(LexerState {
            stream: value.char_indices(),
            buffer: None,
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn new(stream: &'a str) -> Self {
        Lexer::Working(LexerState {
            stream: stream.char_indices(),
            buffer: None,
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<LocatedToken, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Lexer::Done(_) => None,
            Lexer::Errored(e) => Some(Err(e.clone())),
            Lexer::Working(state) => match state.next_token() {
                Err(e) => {
                    println!("ERROR: {e}");
                    *self = Lexer::Errored(e.clone());
                    Some(Err(e))
                }

                Ok(None) => {
                    *self = Lexer::Done(state.stream.offset());
                    None
                }

                Ok(Some(ltoken)) => Some(Ok(ltoken)),
            },
        }
    }
}

impl<'a> LexerState<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        let result = self.buffer.take().or_else(|| self.stream.next());
        result
    }

    fn stash_char(&mut self, idx: usize, c: char) {
        assert!(self.buffer.is_none());
        self.buffer = Some((idx, c));
    }

    fn next_token(&mut self) -> Result<Option<LocatedToken>, LexerError> {
        while let Some((token_start_offset, char)) = self.next_char() {
            if char.is_whitespace() {
                continue;
            }

            let simple_response = |token| {
                Ok(Some(LocatedToken {
                    token,
                    span: token_start_offset..self.stream.offset(),
                }))
            };

            match char {
                '(' => return simple_response(Token::OpenParen),
                ')' => return simple_response(Token::CloseParen),
                '[' => return simple_response(Token::OpenSquare),
                ']' => return simple_response(Token::CloseSquare),
                '{' => return simple_response(Token::OpenBrace),
                '}' => return simple_response(Token::CloseBrace),
                ';' => return simple_response(Token::Semi),
                ':' => return simple_response(Token::Colon),
                ',' => return simple_response(Token::Comma),
                '`' => return simple_response(Token::BackTick),
                '\\' => return simple_response(Token::Lambda(false)),
                'λ' => return simple_response(Token::Lambda(true)),

                '0' => return self.starts_with_zero(token_start_offset),
                '\'' => return self.starts_with_single(token_start_offset),
                '\"' => return self.starts_with_double(token_start_offset),
                '-' => return self.starts_with_dash(token_start_offset),
                _ => {}
            }

            if let Some(value) = char.to_digit(10) {
                return self.parse_integer(token_start_offset, 10, None, value as u64);
            }

            if char.is_uppercase() {
                return self.parse_identifier(
                    token_start_offset,
                    char.into(),
                    |c| c.is_alphanumeric() || c == '_',
                    Token::TypeName,
                );
            }

            if char.is_alphabetic() || char == '_' {
                return self.parse_identifier(
                    token_start_offset,
                    char.into(),
                    |c| c.is_alphanumeric() || c == '_',
                    Token::ValueName,
                );
            }

            if !char.is_alphanumeric() && !char.is_whitespace() && !char.is_control() {
                return self.parse_identifier(
                    token_start_offset,
                    char.into(),
                    |c| !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control(),
                    Token::OperatorName,
                );
            }
        }

        Ok(None)
    }

    fn starts_with_zero(
        &mut self,
        token_start_offset: usize,
    ) -> Result<Option<LocatedToken>, LexerError> {
        match self.next_char() {
            None => {
                let token = Token::Integer(IntegerWithBase {
                    base: None,
                    value: 0,
                });
                Ok(Some(LocatedToken {
                    token,
                    span: token_start_offset..self.stream.offset(),
                }))
            }

            Some((_, 'b')) => self.parse_integer(token_start_offset, 2, Some(2), 0),
            Some((_, 'o')) => self.parse_integer(token_start_offset, 8, Some(8), 0),
            Some((_, 'd')) => self.parse_integer(token_start_offset, 10, Some(10), 0),
            Some((_, 'x')) => self.parse_integer(token_start_offset, 16, Some(16), 0),

            Some((offset, c)) => {
                if let Some(value) = c.to_digit(10) {
                    self.parse_integer(token_start_offset, 10, None, value as u64)
                } else {
                    self.stash_char(offset, c);
                    let token = Token::Integer(IntegerWithBase {
                        base: None,
                        value: 0,
                    });
                    Ok(Some(LocatedToken {
                        token,
                        span: token_start_offset..offset,
                    }))
                }
            }
        }
    }

    fn parse_integer(
        &mut self,
        token_start_offset: usize,
        base: u32,
        provided_base: Option<u8>,
        mut value: u64,
    ) -> Result<Option<LocatedToken>, LexerError> {
        let mut end_offset = self.stream.offset();

        while let Some((offset, c)) = self.next_char() {
            end_offset = offset;
            if let Some(digit) = c.to_digit(base) {
                value = (value * (base as u64)) + (digit as u64);
            } else {
                self.stash_char(offset, c);
                break;
            }
        }

        let token = Token::Integer(IntegerWithBase {
            base: provided_base,
            value,
        });

        Ok(Some(LocatedToken {
            token,
            span: token_start_offset..end_offset,
        }))
    }

    fn parse_identifier(
        &mut self,
        token_start_offset: usize,
        mut identifier: String,
        mut allowed_character: fn(char) -> bool,
        mut builder: fn(String) -> Token,
    ) -> Result<Option<LocatedToken>, LexerError> {
        let mut end_offset = self.stream.offset();

        while let Some((offset, c)) = self.next_char() {
            end_offset = offset;

            if allowed_character(c) {
                identifier.push(c);
            } else if identifier == "prim" && c == '%' {
                identifier = String::new();
                allowed_character = |c| c.is_alphanumeric() || c == '_';
                match self.next_char() {
                    None => {
                        return Err(LexerError::IllegalPrimitive {
                            span: token_start_offset..end_offset,
                        });
                    }

                    Some((_, char)) => {
                        if char.is_uppercase() {
                            identifier.push(char);
                            builder = Token::PrimitiveTypeName;
                        } else if char.is_lowercase() || char == '_' {
                            identifier.push(char);
                            builder = Token::PrimitiveValueName;
                        } else {
                            return Err(LexerError::IllegalPrimitiveCharacter {
                                span: token_start_offset..end_offset,
                                char,
                            });
                        }
                    }
                }
            } else {
                self.stash_char(offset, c);
                break;
            }
        }

        Ok(Some(LocatedToken {
            token: builder(identifier),
            span: token_start_offset..end_offset,
        }))
    }

    fn starts_with_single(
        &mut self,
        token_start_offset: usize,
    ) -> Result<Option<LocatedToken>, LexerError> {
        let Some((_, mut char)) = self.next_char() else {
            return Err(LexerError::UnfinishedCharacter {
                span: token_start_offset..self.stream.offset(),
            });
        };

        if char == '\\' {
            char = self.get_escaped_character(token_start_offset)?;
        }

        let Some((idx, finish_char)) = self.next_char() else {
            return Err(LexerError::UnfinishedCharacter {
                span: token_start_offset..self.stream.offset(),
            });
        };

        if finish_char != '\'' {
            return Err(LexerError::OverlongCharacter {
                char,
                span: token_start_offset..self.stream.offset(),
            });
        }

        Ok(Some(LocatedToken {
            token: Token::Character(char),
            span: token_start_offset..idx,
        }))
    }

    fn get_escaped_character(&mut self, token_start_offset: usize) -> Result<char, LexerError> {
        let Some((idx, escaped_char)) = self.next_char() else {
            return Err(LexerError::UnfinishedCharacter {
                span: token_start_offset..self.stream.offset(),
            });
        };

        match escaped_char {
            '0' => Ok('\0'),
            'a' => Ok('\u{0007}'),
            'b' => Ok('\u{0008}'),
            'f' => Ok('\u{000C}'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            'u' => self.get_unicode_sequence(idx),
            'v' => Ok('\u{000B}'),
            '\'' => Ok('\''),
            '"' => Ok('"'),
            '\\' => Ok('\\'),
            _ => Err(LexerError::UnknownEscapeCharacter {
                escaped_char,
                span: idx..self.stream.offset(),
            }),
        }
    }

    fn get_unicode_sequence(&mut self, token_start_offset: usize) -> Result<char, LexerError> {
        let Some((_, char)) = self.next_char() else {
            return Err(LexerError::InvalidUnicode {
                span: token_start_offset..self.stream.offset(),
            });
        };

        if char != '{' {
            return Err(LexerError::InvalidUnicode {
                span: token_start_offset..self.stream.offset(),
            });
        }

        let mut value = 0;

        while let Some((idx, char)) = self.next_char() {
            if let Some(digit) = char.to_digit(16) {
                value = (value * 16) + digit;
                continue;
            }

            if char == '}' {
                if let Some(char) = char::from_u32(value) {
                    return Ok(char);
                } else {
                    return Err(LexerError::InvalidUnicode {
                        span: token_start_offset..idx,
                    });
                }
            }

            return Err(LexerError::InvalidUnicode {
                span: token_start_offset..self.stream.offset(),
            });
        }

        Err(LexerError::InvalidUnicode {
            span: token_start_offset..self.stream.offset(),
        })
    }

    fn starts_with_double(
        &mut self,
        token_start_offset: usize,
    ) -> Result<Option<LocatedToken>, LexerError> {
        let mut result = String::new();

        while let Some((idx, char)) = self.next_char() {
            match char {
                '"' => {
                    return Ok(Some(LocatedToken {
                        token: Token::String(result),
                        span: token_start_offset..idx,
                    }));
                }

                '\\' => result.push(self.get_escaped_character(idx)?),

                _ => result.push(char),
            }
        }

        Err(LexerError::UnfinishedString {
            span: token_start_offset..self.stream.offset(),
        })
    }

    fn starts_with_dash(
        &mut self,
        token_start_offset: usize,
    ) -> Result<Option<LocatedToken>, LexerError> {
        match self.next_char() {
            None => Ok(Some(LocatedToken {
                token: Token::OperatorName("-".into()),
                span: token_start_offset..token_start_offset + 1,
            })),
            Some((end, '>')) => Ok(Some(LocatedToken {
                token: Token::Arrow,
                span: token_start_offset..end,
            })),
            Some((_, c)) if !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control() => self
                .parse_identifier(
                    token_start_offset,
                    format!("-{c}"),
                    |c| !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control(),
                    Token::OperatorName,
                ),
            Some((idx, c)) => {
                self.stash_char(idx, c);
                Ok(Some(LocatedToken {
                    token: Token::OperatorName("-".into()),
                    span: token_start_offset..idx,
                }))
            }
        }
    }
}

proptest::proptest! {
    #[test]
    fn token_string_token(token: Token) {
        println!("Starting from {token:?}");
        let string = format!("{token}");
        let mut tokens = Lexer::from(string.as_str());
        let initial_token = tokens.next()
            .expect("Can get a token without an error.")
            .expect("Can get a valid token.")
            .token;

        proptest::prop_assert_eq!(token, initial_token);
        proptest::prop_assert!(tokens.next().is_none());
    }
}

#[cfg(test)]
fn parsed_single_token(s: &str) -> Token {
    let mut tokens = Lexer::from(s);
    let result = tokens
        .next()
        .expect(format!("Can get at least one token from {s:?}").as_str())
        .expect("Can get a valid token.")
        .token;

    assert!(
        tokens.next().is_none(),
        "Should only get one token from {s:?}"
    );

    result
}

#[test]
fn numbers_work_as_expected() {
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: None,
            value: 1
        }),
        parsed_single_token("1")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(2),
            value: 1
        }),
        parsed_single_token("0b1")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(8),
            value: 1
        }),
        parsed_single_token("0o1")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(10),
            value: 1
        }),
        parsed_single_token("0d1")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(16),
            value: 1
        }),
        parsed_single_token("0x1")
    );

    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: None,
            value: 10
        }),
        parsed_single_token("10")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(2),
            value: 2
        }),
        parsed_single_token("0b10")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(8),
            value: 8
        }),
        parsed_single_token("0o10")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(10),
            value: 10
        }),
        parsed_single_token("0d10")
    );
    assert_eq!(
        Token::Integer(IntegerWithBase {
            base: Some(16),
            value: 16
        }),
        parsed_single_token("0x10")
    );
}

#[test]
fn lambda_works() {
    assert_eq!(Token::Lambda(false), parsed_single_token("\\"));
    assert_eq!(Token::Lambda(true), parsed_single_token("λ"));
    assert_eq!(Token::TypeName("Λ".into()), parsed_single_token("Λ"));
}

#[test]
fn types_work_as_expected() {
    assert_eq!(Token::TypeName("Int".into()), parsed_single_token("Int"));
    assert_eq!(Token::TypeName("Int8".into()), parsed_single_token("Int8"));
    assert_eq!(Token::TypeName("Γ".into()), parsed_single_token("Γ"));
}

#[test]
fn values_work_as_expected() {
    assert_eq!(
        Token::ValueName("alpha".into()),
        parsed_single_token("alpha")
    );
    assert_eq!(Token::ValueName("ɑ".into()), parsed_single_token("ɑ"));
}

#[test]
fn operators_work_as_expected() {
    assert_eq!(Token::OperatorName("-".into()), parsed_single_token("-"));
    assert_eq!(Token::OperatorName("+".into()), parsed_single_token("+"));
    assert_eq!(Token::OperatorName("*".into()), parsed_single_token("*"));
    assert_eq!(Token::OperatorName("/".into()), parsed_single_token("/"));
    assert_eq!(Token::OperatorName("↣".into()), parsed_single_token("↣"));
}

#[test]
fn can_separate_pieces() {
    let mut lexer = Lexer::from("a-b");
    let mut next_token = move || lexer.next().map(|x| x.expect("Can read valid token").token);

    assert_eq!(Some(Token::ValueName("a".into())), next_token());
    assert_eq!(Some(Token::OperatorName("-".into())), next_token());
    assert_eq!(Some(Token::ValueName("b".into())), next_token());
    assert_eq!(None, next_token());

    let mut lexer = Lexer::from("a--b");
    let mut next_token = move || lexer.next().map(|x| x.expect("Can read valid token").token);

    assert_eq!(Some(Token::ValueName("a".into())), next_token());
    assert_eq!(Some(Token::OperatorName("--".into())), next_token());
    assert_eq!(Some(Token::ValueName("b".into())), next_token());
    assert_eq!(None, next_token());

    let mut lexer = Lexer::from("a - -b");
    let mut next_token = move || lexer.next().map(|x| x.expect("Can read valid token").token);

    assert_eq!(Some(Token::ValueName("a".into())), next_token());
    assert_eq!(Some(Token::OperatorName("-".into())), next_token());
    assert_eq!(Some(Token::OperatorName("-".into())), next_token());
    assert_eq!(Some(Token::ValueName("b".into())), next_token());
    assert_eq!(None, next_token());
}
