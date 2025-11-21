//! Some types that are used in (almost) every stage of the compiler, and would be frustrating to relegate to just one.
//! Types used with parser storage (i.e. ident/qpath) are still relegated to the parser however.

#[cfg(test)]
mod test;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DimensionCount {
    Two,
    Three,
    Four,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBits {
    Int8,
    Int16,
    Int32,
    Int64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatBits {
    Float16,
    Float32,
    Float64,
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
