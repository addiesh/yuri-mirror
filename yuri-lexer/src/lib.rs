// this module is basically a stripped-down rustc lexer.

use crate::cursor::Cursor;
use crate::token::{Base, LiteralKind, Token, TokenKind};

use LiteralKind::*;
use TokenKind::*;

mod cursor;
#[cfg(test)]
mod test;
pub mod token;

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || cursor.advance_token())
}

fn is_newline(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // End-of-line characters
        | '\u{000A}' // line feed (\n)
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // carriage return (\r)
        | '\u{0085}' // next line (from latin1)
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is considered a whitespace.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // `Default_Ignorable_Code_Point` characters
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Horizontal space characters
        | '\u{0009}' // tab (\t)
        | '\u{0020}' // space
    )
}

/// True if `c` is considered horizontal whitespace according to Rust language definition.
pub fn is_horizontal_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Horizontal space characters
        '\u{0009}'   // tab (\t)
        | '\u{0020}' // space
    )
}

/// True if `c` is valid as a first character of an identifier.
pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier.
pub fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

/// The passed string is lexically an identifier.
pub fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    pub fn advance_token(&mut self) -> Option<Token> {
        let first_char = self.bump()?;

        let token_kind = match first_char {
            // Line comment.
            '#' => self.line_comment(),

            c if is_newline(c) => {
                // a carriage return followed by a newline should be eaten together (for CRLF)
                if c == '\r' && self.first() == '\n' {
                    self.bump();
                }
                Newline
            }
            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident(),

            // Numeric literal.
            c @ '0'..='9' => Literal(self.number(c)),

            // Double-character tokens.
            '*' => match self.first() {
                '*' => {
                    self.bump();
                    DoubleStar
                }
                _ => Star,
            },
            '{' => match self.first() {
                '{' => {
                    self.bump();
                    OpenDoubleBrace
                }
                _ => OpenBrace,
            },
            '}' => match self.first() {
                '}' => {
                    self.bump();
                    CloseDoubleBrace
                }
                _ => CloseBrace,
            },

            // One-symbol tokens.
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '@' => At,
            '~' => Tilde,
            '?' => Question,
            ':' => Colon,
            '$' => Dollar,
            '=' => Eq,
            '!' => Bang,
            '<' => Lt,
            '>' => Gt,
            '-' => Minus,
            '+' => Plus,
            '&' => And,
            '|' => Or,
            '^' => Caret,
            '/' => Slash,
            '%' => Percent,

            _ => Unknown,
        };

        let res = Token::new(token_kind, self.pos_within_token());
        self.reset_pos_within_token();
        Some(res)
    }

    fn line_comment(&mut self) -> TokenKind {
        self.bump();

        self.eat_until2(b'\r', b'\n');
        LineComment
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        Whitespace
    }

    fn ident(&mut self) -> TokenKind {
        debug_assert!(is_id_start(self.prev()));
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(is_id_continue);
        Ident
    }

    fn number(&mut self, first_digit: char) -> LiteralKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        let mut base = Base::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.bump();
                    if !self.eat_binary_digits() {
                        return Int(base);
                    }
                }
                'o' => {
                    base = Base::Octal;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return Int(base);
                    }
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.bump();
                    if !self.eat_hexadecimal_digits() {
                        return Int(base);
                    }
                }

                // Not a base prefix; consume additional digits.
                '0'..='9' | '_' => {
                    self.eat_decimal_digits();
                }

                // Also not a base prefix; nothing more to do here.
                '.' => {}

                // Just a 0.
                _ => return Int(base),
            }
        } else {
            // No base prefix, parse number in the usual way.
            self.eat_decimal_digits();
        }

        match self.first() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access
            // (eg. `12.foo()`)
            '.' if base == Base::Decimal && !is_id_start(self.second()) => {
                // might have stuff after the dot, and if it does, it needs to start
                // with a number.

                // consume the dot
                self.bump();

                self.eat_decimal_digits();

                // must have digits before e notation or it looks like field access again.
                if self.first() == 'e' {
                    // consume the e
                    self.bump();
                    self.eat_float_exponent();
                }

                Float
            }
            // Explicit type suffixes for integer values.
            'i' | 'u' => {
                self.bump();
                Int(base)
            }
            // in Rust syntax, "1e+5" is valid; Yuri disagrees, and requires E notation be preceded by a decimal point and digit 0-9.
            _ => Int(base),
        }
    }

    fn eat_binary_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0' | '1' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert_eq!(self.prev(), 'e');
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }
}
