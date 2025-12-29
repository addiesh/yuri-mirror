//! program = module_block;
//!
//! module_block = {
//!     function_item
//!     | type_alias_item
//!     | module_item
//!     | variable_item
//!     | import_item
//! };
//!
//! module_item =
//!     [ "export" ],
//!     "module",
//!     Ident,
//!     "{",
//!         module_block,
//!     "}";
//!
//! qpath = { Ident | (Ident, ".", qpath) };
//!
//! attribute = "@", qpath, [ "(", ( expression_list, [","] ), ")" ]
//!
//! type = qpath | array_type | compound_type;
//!
//! array_type = "[", type, ";", expression, "]";
//!
//! compound_type_field = { attribute }, Ident, ":", type;
//! compound_type_fields = compound_type_field, [",", compound_type_fields];
//! compound_type =
//!     "{{",
//!     ( compound_type_fields, [","] ),
//!     "}}";
//!
//! variable_item = ["export"], "let", Ident, "=", expression, ( ";" | Newline );
//!
//! import_item = "import", qpath, ( ";" | Newline );
//!
//! type_alias_item = ["export"], "type", Ident, "=", type, ( ";" | Newline );
//!
//! expression = expression_1;
//! expression_1 = expression_2, { "or", expression_2 };
//! expression_2 = expression_3, { "and", expression_3 };
//! expression_3 = expression_4, [ ("==" | "!=" | ">" | "<" | "<=" | ">="), expression_4 ];
//! expression_4 = expression_5, { "|", expression_5 };
//! expression_5 = expression_6, { "^", expression_6 };
//! expression_6 = expression_7, { "&", expression_7 };
//! expression_7 = expression_8, { ("<<" | ">>"), expression_8 };
//! expression_8 = expression_9, { ("+" | "-"), expression_9 };
//! expression_9 = expression_a, { ("*" | "/" | "%"), expression_a };
//! expression_a = expression_b, { "**", expression_b };
//! expression_b = ( ("!" | "~" | "+" | "-"), expression_b) | expression_c;
//!
//! # addie is tired. pretend field access and call expressions just work
//! expression_c =
//!     expression_last, { ".", qpath };
//!     | call_expression
//!     | ( "(", expression, ")" )

//! expression_last =
//!     | qpath
//!     | Literal
//!     | if_expression
//!     | array_expression
//!     | compound_expression
//!     | expression_block;
//!
//!
//! expression_list = expression, [",", expression_list];
//!
//! call_expression = Ident, "(", ( expression_list, [","] ), ")"
//!
//! compound_expression_field = Ident, "=", expression;
//! compound_expression_fields = compound_expression_field, [",", compound_expression_fields];
//! compound_expression =
//!     "{{",
//!     ( compound_expression_fields, [","] ),
//!     "}}";
//!
//! function_param = { attribute }, Ident, ":", type;
//! function_params = function_param, [",", function_params]
//! function_item =
//!     { attribute },
//!     [ "export" ],
//!     "fn",
//!     Ident,
//!     "(",
//!     ( function_params, [","] )
//!     ")",
//!     ":",
//!     type,
//!     expression_block;
//!
//!
//! expression_block = {
//!     function_item
//!     | type_alias_item
//!     | variable_item
//!     | import_item
//!     | expression
//! };

use smallvec::SmallVec;
use thin_vec::ThinVec;
use yuri_ast::{Ast, Ident, InStorage, Qpath};
use yuri_lexer::{Token, TokenKind};

use crate::error::{ParseError, ParseTry};

pub mod error;
pub mod expression;
pub mod item;
#[cfg(test)]
mod test;

/// "Full" token
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenF {
    pub kind: TokenKind,
    pub byte_offset: u32,
    pub len: u32,
}

pub fn parse_all<'src, 'storage>(
    source: &'src str,
    storage: &'storage mut InStorage,
    tokens: &'src [Token],
) -> (Ast, ParseState<'src, 'storage>) {
    let mut state = ParseState {
        storage,
        source,
        tokens,
        errors: Vec::new(),
        index: 0,
        byte_offset: 0,
    };

    let mut declarations = Vec::new();

    loop {
        if !state.has() {
            break;
        }
        if !state.take_whitespace(true) {
            break;
        }
        // TODO: use error recovery instead, this will error forever on an unexpected token.
        match state.outer_declaration() {
            Ok(decl) => declarations.push(decl),
            Err(err) => state.errors.push(err),
        }
    }

    (declarations, state)
}

pub struct ParseState<'src, 'storage> {
    storage: &'storage mut InStorage,
    source: &'src str,
    tokens: &'src [yuri_lexer::Token],
    byte_offset: u32,
    pub errors: Vec<ParseError>,
    // hints: Vec<ParseHint>
    index: usize,
}

impl<'src> ParseState<'src, '_> {
    pub fn str_from_token(&self, token: TokenF) -> &'src str {
        let start = token.byte_offset as usize;
        let end = token.byte_offset as usize + token.len as usize;
        &self.source[start..end]
    }

    pub fn str_to_ident<'a: 'src>(&mut self, str: &'a str) -> Ident {
        self.storage.to_ident(str)
    }

    pub fn token_to_ident(&mut self, token: TokenF) -> Ident {
        debug_assert_eq!(token.kind, TokenKind::Ident);
        self.storage.to_ident(self.str_from_token(token))
    }

    #[must_use]
    fn pos(&self) -> u32 {
        self.index as u32
    }

    #[must_use]
    fn peek(&self) -> Option<TokenF> {
        self.tokens.get(self.index).map(|tok| TokenF {
            kind: tok.kind,
            byte_offset: self.byte_offset,
            len: tok.len,
        })
    }

    #[must_use]
    fn peek_off(&self, offset: usize) -> Option<TokenF> {
        if offset == 0 {
            return self.peek();
        }

        let mut byte_offset = self.byte_offset;

        // consume tokens up until the offset
        // TODO: make sure that starting at 0 is correct here,
        // my "off-by-one" alarm is ringing
        for i in 0..(offset - 1) {
            let tok = self.tokens.get(self.index + i)?;
            byte_offset += tok.len;
        }

        self.tokens.get(self.index + offset).map(|tok| TokenF {
            byte_offset,
            kind: tok.kind,
            len: tok.len,
        })
    }

    #[must_use]
    fn take(&mut self) -> Option<TokenF> {
        let tok = self.peek()?;
        self.skip();
        Some(tok)
    }

    fn switch_seq<T: Copy>(&mut self, sequence: &[(T, &[TokenKind])]) -> Option<T> {
        for (res, seq) in sequence {
            if self.take_seq(seq) {
                return Some(*res);
            }
        }
        None
    }

    fn take_seq(&mut self, sequence: &[TokenKind]) -> bool {
        for (i, kind) in sequence.iter().enumerate() {
            let Some(other) = self.peek_off(i) else {
                return false;
            };
            let otherkind = other.kind;
            if otherkind != *kind {
                return false;
            }
        }
        self.skip_n(sequence.len());
        true
    }

    /// Returns false if EOF, true otherwise.
    fn skip(&mut self) -> bool {
        if self.has() {
            let tok = self.tokens[self.index];
            self.index += 1;
            self.byte_offset += tok.len;
            true
        } else {
            false
        }
    }

    fn skip_n(&mut self, n: usize) -> bool {
        for _ in 0..n {
            if !self.skip() {
                return false;
            }
        }
        true
    }

    fn has(&self) -> bool {
        self.index < self.tokens.len() // as isize
    }

    /// Skips over whitespace/newlines/line comments, leaving the peek cursor on a non-whitespace token.
    /// Returns false if EOF is reached, true otherwise.
    fn take_whitespace(&mut self, include_newline: bool) -> bool {
        loop {
            match self.peek().map(|tok| tok.kind) {
                Some(TokenKind::Whitespace | TokenKind::LineComment) => {
                    self.skip();
                }
                Some(TokenKind::Newline) if include_newline => {
                    self.skip();
                }
                None => return false,
                _ => return true,
            };
        }
    }

    // Returns the offset to the next non-whitespace token, and what kind of token it is.
    #[must_use]
    fn peek_whitespace(
        &mut self,
        mut offset: usize,
        include_newline: bool,
    ) -> Option<(usize, TokenF)> {
        loop {
            let Some(tok) = self.peek_off(offset) else {
                return None;
            };
            match tok.kind {
                TokenKind::Whitespace | TokenKind::LineComment => {
                    offset += 1;
                }
                TokenKind::Newline if include_newline => {
                    offset += 1;
                }
                _ => return Some((offset, tok)),
            };
        }
    }

    /// Eats tokens until encountering any one of the anchor sequences,
    /// in which it does not consume the sequence and returns the first character of the sequence encountered.
    /// If EOF is reached, None is returned.
    fn recover<'a>(&mut self, anchors: &[&'a [TokenKind]]) -> Option<&'a [TokenKind]> {
        loop {
            for seq in anchors {
                if self.take_seq(seq) {
                    return Some(seq);
                }
            }
            if !self.skip() {
                return None;
            }
        }
    }

    fn unexpected(&self) -> ParseError {
        ParseError::UnexpectedToken {
            token: self.peek().unwrap().kind,
            at: self.pos(),
        }
    }

    /// Like [take], but returns [ParseError::UnexpectedToken]
    /// and doesn't consume the token if the result is unexpected.
    /// Returns [ParseError::Eof] if there are no more tokens left.
    #[must_use]
    fn expect(&mut self, kind: TokenKind) -> Result<TokenF, ParseError> {
        let tok = self.peek().eof()?;
        if tok.kind == kind {
            Ok(self.take().unwrap())
        } else {
            Err(self.unexpected())
        }
    }

    /// Like [expect], but for the common cases of:
    /// - "I expect an ident for this token, tell me what it is"
    /// - "I expect a specific ident for this token"
    #[must_use]
    fn expect_ident(&mut self, ident: Option<Ident>) -> Result<Ident, ParseError> {
        let got = self.expect(TokenKind::Ident)?;
        let got = self.token_to_ident(got);
        if let Some(expected) = ident
            && got != expected
        {
            return Err(self.unexpected());
        }
        Ok(got)
    }

    // fn take_delimited<T>(&mut self, kind: TokenKind) -> Result<T, ParseError> {
    // }
}

// all the methods here expect the start of what they consume to be the PEEK cursor's token.
// TODO: error recovery w/ anchors and whatnot. that blog post was good.
impl<'src> ParseState<'src, '_> {
    /// Jumps to the next token, consuming a qualified path (a dot-separated identifier).
    /// On success, leaves the peek cursor on the next unrecognized token.
    fn qpath(&mut self) -> Result<Qpath, ParseError> {
        let mut parts = SmallVec::<[Ident; 4]>::new();
        loop {
            let tok = self.expect(TokenKind::Ident)?;
            let ident = self.token_to_ident(tok);
            parts.push(ident);
            // if the next token is a dot, add another part to the path
            if !self.take_whitespace(true) {
                break;
            }
            match self.peek().map(|tok| tok.kind) {
                Some(TokenKind::Dot) => self.skip(),
                _ => break,
            };
        }
        Ok(Qpath(ThinVec::from(parts.as_slice())))
    }
}
