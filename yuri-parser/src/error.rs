use std::fmt::{Debug, Display};

use yuri_lexer::TokenKind;

use crate::TokenF;

pub type ParseResult<T, E = ParseError> = Result<T, E>;

#[derive(Debug, Clone)]
pub enum ParseHint {
    LeadingWhitespace,
    GrossIndentation,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken { token: TokenKind, at: u32 },
    UnexpectedEof,
}

pub trait ParseTry {
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
