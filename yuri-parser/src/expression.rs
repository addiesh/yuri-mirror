use std::fmt::{Display, Formatter};

use crate::Ident;
use crate::item::{FunctionItem, TypeAliasItem, VariableItem};

pub enum Expression {
    /// Load the value of a given variable.
    Variable(Ident),
    /// A value to be evaluated at compile-time.
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Array(ArrayExpression),
    IfExpr(IfExpression),
    FunctionalCall(CallExpression),
    CompoundInit(CompoundExpression),
    Block(BlockExpression),
    Paren(Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "todo")
    }
}

// TODO: disambiguate between hex/binary/etc.
pub enum LiteralExpression {
    Bool(bool),
    Decimal(f32),
    /// An integer with no sign or decimal point.
    Integer(u32),
    /// An integer with an explicitly specified sign.
    SignedInt(i32),
}

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

pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

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
    Gr,
    // >=
    GrEq,
    // <<
    ShiftLeft,
    // >>
    ShiftRight,
    // +
    Add,
    // -
    Subtract,
    // *
    Multiply,
    // **
    Exponent,
    // /
    Divide,
    // %
    Remainder,
}

pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub a: Box<Expression>,
    pub b: Box<Expression>,
}

pub enum InnerDeclaration {
    Global(VariableItem),
    Function(FunctionItem),
    Alias(TypeAliasItem),
}

/// Local block element.
pub enum BlockStatement {
    LocalVariable(VariableItem),
    TypeAlias(TypeAliasItem),
    Function(FunctionItem),
    Return(Expression),
    Import(Ident),
}

pub struct BlockExpression {
    /// The scope that this block creates.
    pub statements: Vec<BlockStatement>,
    pub tail: Option<Box<Expression>>,
}

pub struct IfExpression {
    pub consequence: Box<BlockExpression>,
    pub condition: Box<Expression>,
    pub chained_else: Option<Box<ElseChain>>,
}

pub struct ElseChain {
    pub consequence: BlockExpression,
    pub condition: Option<Box<Expression>>,
    pub chained_else: Option<Box<ElseChain>>,
}

pub enum ArrayExpression {
    Elements(Vec<Expression>),
    Spread {
        element: Box<Expression>,
        length: Box<Expression>,
    },
}

pub struct CompoundExpression {
    pub fields: Vec<CompoundExpressionField>,
}

pub struct CompoundExpressionField {
    pub target_field: Ident,
    pub expression: Expression,
}

pub struct CallExpression {
    pub function: Ident,
    pub arguments: Vec<Expression>,
}
