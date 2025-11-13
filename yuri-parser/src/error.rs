use std::fmt::{Debug, Display};
use yuri_lexer::TokenKind;

use crate::TokenF;

#[derive(Debug, Clone)]
pub enum ParseHint {
    LeadingWhitespace,
    GrossIndentation,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Multiple(Vec<ParseError>),
    UnexpectedToken(TokenF),
    UnexpectedEof,
}

impl TokenF {
    pub fn expect(&self, kind: TokenKind) -> Result<Self, ParseError> {
        if self.kind == kind {
            Ok(*self)
        } else {
            Err(ParseError::UnexpectedToken(*self))
        }
    }
}

pub trait ParseTry: Sized {
    fn eof(&self) -> Result<TokenF, ParseError>;
}

impl ParseTry for Option<TokenF> {
    fn eof(&self) -> Result<TokenF, ParseError> {
        self.ok_or(ParseError::UnexpectedEof)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}
impl std::error::Error for ParseError {}
