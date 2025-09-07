//use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::syntax::tokens::Token;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Lexer error at {file_id}: {error}")]
    LexerError { file_id: usize, error: LexerError },

    #[error("Unacceptable end of file at {file_id} while {place}")]
    UnacceptableEof { file_id: usize, place: &'static str },

    #[error("Unexpected token at {file_id}: expected {expected}, saw {token}")]
    UnexpectedToken {
        file_id: usize,
        span: Range<usize>,
        token: Token,
        expected: &'static str,
    },
}

#[derive(Clone, Debug, Error, PartialEq)]
pub enum LexerError {
    #[error("Illegal control character in input stream at offset {offset}")]
    IllegalControlCharacter { offset: usize },

    #[error("Illegal primitive value/type; it cut off before we could determine which at {span:?}")]
    IllegalPrimitive { span: Range<usize> },

    #[error("Illegal character in primitive ({char:?}) at {span:?}")]
    IllegalPrimitiveCharacter { span: Range<usize>, char: char },

    #[error("Unfinished character constant found at {span:?}")]
    UnfinishedCharacter { span: Range<usize> },

    #[error("Unfinished string constant found at {span:?}")]
    UnfinishedString { span: Range<usize> },

    #[error("Character {char:?} has some extra bits at the end at {span:?}")]
    OverlongCharacter { char: char, span: Range<usize> },

    #[error("Unknown escaped character {escaped_char:?} at {span:?}")]
    UnknownEscapeCharacter {
        escaped_char: char,
        span: Range<usize>,
    },

    #[error("Invalid unicode escape sequence at {span:?}")]
    InvalidUnicode { span: Range<usize> },
}

impl LexerError {
    pub fn to_triple(&self) -> (usize, Result<Token, LexerError>, usize) {
        match self {
            LexerError::IllegalControlCharacter { offset } => (*offset, Err(self.clone()), *offset),
            LexerError::IllegalPrimitive { span } => (span.start, Err(self.clone()), span.end),
            LexerError::IllegalPrimitiveCharacter { span, .. } => {
                (span.start, Err(self.clone()), span.end)
            }
            LexerError::UnfinishedCharacter { span, .. } => {
                (span.start, Err(self.clone()), span.end)
            }
            LexerError::UnfinishedString { span, .. } => (span.start, Err(self.clone()), span.end),
            LexerError::OverlongCharacter { span, .. } => (span.start, Err(self.clone()), span.end),
            LexerError::UnknownEscapeCharacter { span, .. } => {
                (span.start, Err(self.clone()), span.end)
            }
            LexerError::InvalidUnicode { span, .. } => (span.start, Err(self.clone()), span.end),
        }
    }
}

//impl<F> From<LexerError> for Diagnostic<F> {
//    fn from(value: LexerError) -> Self {
//        match value {
//            LexerError::IllegalControlCharacter { file, offset } => Diagnostic::error()
//                .with_code("E1001")
//                .with_message("Illegal control character in input stream")
//                .with_label(Label::primary(file, offset..offset).with_message("illegal character")),
//
//            LexerError::IllegalPrimitive { file, span } => Diagnostic::error()
//                .with_code("E1002")
//                .with_message("Illegal primitive; it cut off before it could finish")
//                .with_label(
//                    Label::primary(file, span)
//                        .with_message("should be at least one character after the %"),
//                ),
//
//            LexerError::IllegalPrimitiveCharacter { file, span, char } => Diagnostic::error()
//                .with_code("E1003")
//                .with_message(format!("Illegal character {char:?} in primitive"))
//                .with_label(Label::primary(file, span).with_message("illegal character")),
//
//            LexerError::UnfinishedCharacter { file, span } => Diagnostic::error()
//                .with_code("E1004")
//                .with_message("Unfinished character in input stream.")
//                .with_label(Label::primary(file, span).with_message("unfinished character")),
//
//            LexerError::UnfinishedString { file, span } => Diagnostic::error()
//                .with_code("E1005")
//                .with_message("Unfinished string in input stream.")
//                .with_label(Label::primary(file, span).with_message("unfinished string")),
//
//            LexerError::OverlongCharacter { file, char, span } => Diagnostic::error()
//                .with_code("E1006")
//                .with_message(format!(
//                    "Character {char:?} has some extra bits at the end of it."
//                ))
//                .with_label(Label::primary(file, span).with_message("overlong character")),
//
//            LexerError::UnknownEscapeCharacter {
//                file,
//                escaped_char,
//                span,
//            } => Diagnostic::error()
//                .with_code("E1007")
//                .with_message(format!("Unknown escape character {escaped_char:?}."))
//                .with_label(Label::primary(file, span).with_message("unknown character")),
//
//            LexerError::InvalidUnicode { file, span } => Diagnostic::error()
//                .with_code("E1008")
//                .with_message("Unknown or invalid unicode escape sequence.")
//                .with_label(Label::primary(file, span).with_message("escape sequence")),
//        }
//    }
//}
