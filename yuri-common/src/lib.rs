use std::any::Any;
use std::fmt::{Debug, Display, Write};

pub mod error;
#[cfg(test)]
mod test;

// pub struct Span {
//     pub location: u32,
//     pub length: u16,
// }

/// If any types other than [D2], [D3], or [D4] implement this trait, you will get UB.
pub unsafe trait AnyDimension: Any + Debug {}
/// If any types other than [Bits8], [Bits16], [Bits32], or [Bits64] implement this trait, you will get UB.
pub unsafe trait AnyBits: Any + Debug {}
/// If any types other than [Bits16], [Bits32], or [Bits64] implement this trait, you will get UB.
pub unsafe trait AnyFloatBits: AnyBits {}

pub type Bits8 = nalgebra::U8;
unsafe impl AnyBits for Bits8 {}
pub type Bits16 = nalgebra::U16;
unsafe impl AnyBits for Bits16 {}
unsafe impl AnyFloatBits for Bits16 {}
pub type Bits32 = nalgebra::U32;
unsafe impl AnyBits for Bits32 {}
unsafe impl AnyFloatBits for Bits32 {}
pub type Bits64 = nalgebra::U64;
unsafe impl AnyBits for Bits64 {}
unsafe impl AnyFloatBits for Bits64 {}

pub type D2 = nalgebra::U2;
unsafe impl AnyDimension for D2 {}
pub type D3 = nalgebra::U3;
unsafe impl AnyDimension for D3 {}
pub type D4 = nalgebra::U4;
unsafe impl AnyDimension for D4 {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DimCount {
    Two,
    Three,
    Four,
}

impl AsRef<dyn AnyDimension> for DimCount {
    fn as_ref(&self) -> &dyn AnyDimension {
        match self {
            DimCount::Two => &nalgebra::Const::<2>,
            DimCount::Three => &nalgebra::Const::<3>,
            DimCount::Four => &nalgebra::Const::<4>,
        }
    }
}

impl Display for DimCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            DimCount::Two => '2',
            DimCount::Three => '3',
            DimCount::Four => '4',
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

impl AsRef<dyn AnyBits> for IntBits {
    fn as_ref(&self) -> &dyn AnyBits {
        match self {
            IntBits::Int8 => &nalgebra::Const::<8>,
            IntBits::Int16 => &nalgebra::Const::<16>,
            IntBits::Int32 => &nalgebra::Const::<32>,
            IntBits::Int64 => &nalgebra::Const::<64>,
        }
    }
}

// impl From<&dyn AnyBits> for IntBits {
//     fn from(value: &dyn AnyBits) -> Self {
//         let target = value.type_id();
//         if target == Bits8.type_id() {
//             Self::Int8
//         } else if target == Bits16.type_id() {
//             Self::Int16
//         } else if target == Bits32.type_id() {
//             Self::Int32
//         } else if target == Bits64.type_id() {
//             Self::Int64
//         } else {
//             unreachable!()
//         }
//     }
// }

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

impl AsRef<dyn AnyFloatBits> for FloatBits {
    fn as_ref(&self) -> &dyn AnyFloatBits {
        match self {
            FloatBits::Float16 => &nalgebra::Const::<16>,
            FloatBits::Float32 => &nalgebra::Const::<32>,
            FloatBits::Float64 => &nalgebra::Const::<64>,
        }
    }
}

// impl From<&dyn AnyFloatBits> for FloatBits {
//     fn from(value: &dyn AnyFloatBits) -> Self {
//         let target = value.type_id();
//         if target == Bits16.type_id() {
//             Self::Float16
//         } else if target == Bits32.type_id() {
//             Self::Float32
//         } else if target == Bits64.type_id() {
//             Self::Float64
//         } else {
//             unreachable!()
//         }
//     }
// }

impl Display for FloatBits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            FloatBits::Float16 => "16",
            FloatBits::Float32 => "32",
            FloatBits::Float64 => "64",
        })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
