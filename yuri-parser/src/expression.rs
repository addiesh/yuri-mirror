use crate::Ident;
use crate::item::{FunctionItem, TypeAliasItem, VariableItem};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
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
    #[cfg(debug_assertions)]
    Unimplemented,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "todo")
    }
}

// TODO: disambiguate between hex/binary/etc.
#[derive(Debug, Clone, Copy)]
pub enum LiteralExpression {
    Bool(bool),
    /// Known to be a float (i.e. decimal point)
    Decimal(f64),
    /// Known to be an integer (i.e. non-decimal base)
    Integer(i128),
    /// May be coerced to any possible type
    Number(i128),
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum InnerDeclaration {
    Global(VariableItem),
    Function(FunctionItem),
    Alias(TypeAliasItem),
}

/// Local block element.
#[derive(Debug, Clone)]
pub enum BlockStatement {
    LocalVariable(VariableItem),
    TypeAlias(TypeAliasItem),
    Function(FunctionItem),
    Return(Expression),
    Import(Ident),
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    /// The scope that this block creates.
    pub statements: Vec<BlockStatement>,
    pub tail: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub consequence: Box<BlockExpression>,
    pub condition: Box<Expression>,
    pub chained_else: Option<Box<ElseChain>>,
}

#[derive(Debug, Clone)]
pub struct ElseChain {
    pub consequence: BlockExpression,
    pub condition: Option<Box<Expression>>,
    pub chained_else: Option<Box<ElseChain>>,
}

#[derive(Debug, Clone)]
pub enum ArrayExpression {
    Elements(Vec<Expression>),
    Spread {
        element: Box<Expression>,
        length: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct CompoundExpression {
    pub fields: Vec<CompoundExpressionField>,
}

#[derive(Debug, Clone)]
pub struct CompoundExpressionField {
    pub target_field: Ident,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub receiver: Box<Expression>,
    pub arguments: Vec<Expression>,
}
