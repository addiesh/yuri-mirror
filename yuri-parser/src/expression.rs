use std::fmt::{Display, Formatter};

use yuri_common::{BinaryOperator, UnaryOperator};

use crate::item::{FunctionItem, TypeAliasItem, VariableItem};
use crate::{Ident, Qpath};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Load the value of a given variable.
    Variable(Qpath),
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

macro_rules! expression_from_helper {
    ($from:ty, $variant:ident) => {
        impl From<$from> for Expression {
            fn from(value: $from) -> Self {
                Expression::$variant(value)
            }
        }

        impl From<$from> for Box<Expression> {
            fn from(value: $from) -> Self {
                Box::new(Expression::$variant(value))
            }
        }
    };
}
expression_from_helper!(LiteralExpression, Literal);
expression_from_helper!(UnaryExpression, Unary);
expression_from_helper!(BinaryExpression, Binary);
expression_from_helper!(ArrayExpression, Array);
expression_from_helper!(IfExpression, IfExpr);
expression_from_helper!(CallExpression, FunctionalCall);
expression_from_helper!(CompoundExpression, CompoundInit);
expression_from_helper!(BlockExpression, Block);

impl Display for Expression {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

// TODO: disambiguate between hex/binary/etc.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LiteralExpression {
    Bool(bool),
    /// Known to be a float (i.e. decimal point)
    Decimal(f64),
    // TODO: improve this specificity
    /// Known to be an integer (i.e. non-decimal base)
    Integer(i128),
    /// May be coerced to any possible type
    Number(i128),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum InnerDeclaration {
    Global(VariableItem),
    Function(FunctionItem),
    Alias(TypeAliasItem),
}

/// Local block element.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockStatement {
    LocalVariable(VariableItem),
    TypeAlias(TypeAliasItem),
    Function(Box<FunctionItem>),
    Assign(Qpath, Box<Expression>),
    Return(Box<Expression>),
    Import(Ident),
    Value(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpression {
    /// The scope that this block creates.
    pub statements: Vec<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub consequence: Box<BlockExpression>,
    pub condition: Box<Expression>,
    pub chained_else: Option<Box<ElseChain>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseChain {
    pub consequence: BlockExpression,
    pub condition: Option<Box<Expression>>,
    pub chained_else: Option<Box<ElseChain>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayExpression {
    Elements(Vec<Expression>),
    Spread {
        element: Box<Expression>,
        length: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundExpression {
    pub fields: Vec<CompoundExpressionField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundExpressionField {
    pub target_field: Ident,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub receiver: Box<Expression>,
    pub arguments: Vec<Expression>,
}
