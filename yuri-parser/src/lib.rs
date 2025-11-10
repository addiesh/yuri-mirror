use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::ops::Deref;

use yuri_lexer::token::Token;

use crate::item::OuterDeclaration;

pub mod expression;
pub mod item;
pub mod parse;
pub mod types;

#[derive(Debug, Clone)]
pub struct ParseError;
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}
impl std::error::Error for ParseError {}

pub struct ParserStorage<'a> {
    ident_list: Vec<&'a str>,
    ident_set: HashMap<&'a str, usize>,
}

impl<'a> ParserStorage<'a> {
    pub fn get_ident(&'a mut self, string: &'a str) -> Ident {
        if let Some(ident) = Keyword::try_from_str(string) {
            Ident::Keyword(ident)
        } else if let Some(index) = self.ident_set.get(string) {
            Ident::Id(*index)
        } else {
            let index = self.ident_list.len();
            self.ident_list.push(string);
            self.ident_set.insert(string, index);
            Ident::Id(index)
        }
    }

    pub fn resolve(&'a self, ident: &Ident) -> Option<&'a str> {
        match ident {
            Ident::Id(index) => self.ident_list.get(*index).map(Deref::deref),
            Ident::Keyword(keyword) => Some(keyword.as_str()),
        }
    }
}

type ParseState<'a> = &'a [Token];

pub fn parse_all(
    // impl Iterator<Item = Token>
    tokens: &[Token],
) -> Result<Vec<OuterDeclaration>, ParseError> {
    // tokens.
    todo!()
}

pub enum Ident {
    Id(usize),
    Keyword(Keyword),
}

#[repr(transparent)]
pub struct Qpath(Box<[Ident]>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    Import,
    Export,
    Fn,
    Let,
    Type,
    Module,
    Break,
    Continue,
    Else,
    If,
    Return,
    And,
    Or,
    Xor,
    Loop,
    Fold,
    Reverse,
    Map,
    Flatten,
    Filter,
    Append,
    Prepend,
    Join,
}

impl Keyword {
    pub const fn as_str<'a>(self) -> &'a str {
        match self {
            Keyword::Import => "import",
            Keyword::Export => "export",
            Keyword::Fn => "fn",
            Keyword::Let => "let",
            Keyword::Type => "type",
            Keyword::Module => "module",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Else => "else",
            Keyword::If => "if",
            Keyword::Return => "return",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Xor => "xor",
            Keyword::Loop => "loop",
            Keyword::Fold => "fold",
            Keyword::Reverse => "reverse",
            Keyword::Map => "map",
            Keyword::Flatten => "flatten",
            Keyword::Filter => "filter",
            Keyword::Append => "append",
            Keyword::Prepend => "prepend",
            Keyword::Join => "join",
        }
    }

    // PartialEq isn't const :/
    pub fn try_from_str(value: &str) -> Option<Self> {
        Some(match value {
            "import" => Keyword::Import,
            "export" => Keyword::Export,
            "fn" => Keyword::Fn,
            "let" => Keyword::Let,
            "type" => Keyword::Type,
            "module" => Keyword::Module,
            "break" => Keyword::Break,
            "continue" => Keyword::Continue,
            "else" => Keyword::Else,
            "if" => Keyword::If,
            "return" => Keyword::Return,
            "and" => Keyword::And,
            "or" => Keyword::Or,
            "xor" => Keyword::Xor,
            "loop" => Keyword::Loop,
            "fold" => Keyword::Fold,
            "reverse" => Keyword::Reverse,
            "map" => Keyword::Map,
            "flatten" => Keyword::Flatten,
            "filter" => Keyword::Filter,
            "append" => Keyword::Append,
            "prepend" => Keyword::Prepend,
            "join" => Keyword::Join,
            _ => return None,
        })
    }
}

// pub static RESERVED_IDENTIFIERS: [&str; _] = ["not", "sampler"];
