//! Some types that are used in (almost) every stage of the compiler, and would be frustrating to relegate to just one.
//! Types used with parser storage (i.e. ident/qpath) are still relegated to the parser however.

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
