use std::fmt::{Display, Write};

pub mod error;
#[cfg(test)]
mod test;

// pub struct Span {
//     pub location: u32,
//     pub length: u16,
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DimensionCount {
    Two,
    Three,
    Four,
}

impl Display for DimensionCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            DimensionCount::Two => '2',
            DimensionCount::Three => '3',
            DimensionCount::Four => '4',
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBits {
    Int8,
    Int16,
    Int32,
    Int64,
}

impl Display for IntBits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            IntBits::Int8 => "8",
            IntBits::Int16 => "16",
            IntBits::Int32 => "32",
            IntBits::Int64 => "64",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatBits {
    Float16,
    Float32,
    Float64,
}

impl Display for FloatBits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            FloatBits::Float16 => "16",
            FloatBits::Float32 => "32",
            FloatBits::Float64 => "64",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScalarTy {
    Signed(IntBits),
    Unsigned(IntBits),
    Float(FloatBits),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    // ~
    BitwiseNot,
    // !
    LogicalNot,
    // -
    Negative,
    // +
    Positive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // "and"
    LogicAnd,
    // "or"
    LogicOr,
    // "xor"
    LogicXor,

    // &
    BitAnd,
    // |
    BitOr,
    // ^
    BitXor,

    // ==
    Eq,
    // !=
    NotEq,
    // <
    Lt,
    // <=
    LtEq,
    // >
    Gt,
    // >=
    GtEq,
    // <<
    ShiftLeft,
    // >>
    ShiftRight,
    // +
    Add,
    // -
    Sub,
    // *
    Multiply,
    // **
    Exponent,
    // /
    Divide,
    // %
    Remainder,
}

// Could be improved
#[must_use]
pub fn to_snake_case(ident: &str) -> String {
    let mut ident: String = ident
        .to_lowercase()
        .chars()
        .filter_map(|c| match c {
            '_' | '.' | '-' | '+' | '!' => Some('_'),
            '(' | ')' | '{' | '}' | '[' | ']' => None,
            c if c.is_whitespace() => Some('_'),
            c => {
                if c == '$' || unicode_xid::UnicodeXID::is_xid_continue(c) {
                    Some(c)
                } else {
                    None
                }
            }
        })
        .collect();
    {
        let mut last_char_was_underscore = false;
        ident.retain(|c| {
            if c == '_' {
                let keep = last_char_was_underscore;
                last_char_was_underscore = true;
                keep
            } else {
                last_char_was_underscore = false;
                true
            }
        });
        // TODO: handle camel case
    }
    ident
}
