use std::borrow::Cow;
use std::sync::{Arc, Weak};

use crate::types::TypeValue;
use crate::types::primitive::Primitive;
use crate::{CompileError, FunctionItem, Resolution, Scope, TypeAliasItem, VariableItem};

mod sealed {
    use crate::{CompileError, TypeValue};

    pub trait ExpressionTrait<'a> {
        fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>>;
    }
}

/// Local block element.
pub enum BlockStatement<'a> {
    Variable(Weak<VariableItem<'a>>),
    TypeAlias(Weak<TypeAliasItem<'a>>),
    Function(Weak<FunctionItem<'a>>),
    Return(Expression<'a>),
    // Tail(Expression<'a>),
}

pub struct BlockExpression<'a> {
    /// The scope that this block creates.
    pub scope: Arc<Scope<'a>>,
    pub statements: Vec<BlockStatement<'a>>,
    pub tail: Option<Box<Expression<'a>>>,
}

impl<'a> sealed::ExpressionTrait<'a> for BlockExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        // figure out if the expression is unreachable

        // if let Some(tail) = &self.tail {
        //     tail
        // }

        todo!()
    }
}

pub struct IfExpression<'a> {
    pub consequence: BlockExpression<'a>,
    pub condition: Box<Expression<'a>>,
    pub chained_else: Option<ElseChain<'a>>,
}

impl<'a> sealed::ExpressionTrait<'a> for IfExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

pub struct ElseChain<'a> {
    pub consequence: BlockExpression<'a>,
    pub condition: Option<Box<Expression<'a>>>,
    pub chained_else: Option<Box<ElseChain<'a>>>,
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

pub struct UnaryExpression<'a> {
    operator: UnaryOperator,
    value: Box<Expression<'a>>,
}

impl<'a> sealed::ExpressionTrait<'a> for UnaryExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
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

pub struct BinaryExpression<'a> {
    pub operator: BinaryOperator,
    pub a: Box<Expression<'a>>,
    pub b: Box<Expression<'a>>,
}

impl<'a> sealed::ExpressionTrait<'a> for BinaryExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

pub struct CallExpression<'a> {
    pub function: Resolution<'a, Weak<FunctionItem<'a>>>,
    pub arguments: Vec<Expression<'a>>,
}

impl<'a> sealed::ExpressionTrait<'a> for CallExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

pub enum ArrayExpression<'a> {
    Elements(Vec<Expression<'a>>),
    Spread {
        element: Box<Expression<'a>>,
        length: Box<Expression<'a>>,
    },
}

impl<'a> sealed::ExpressionTrait<'a> for ArrayExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

// Because of duck-typing, compound expression types in codegen
pub struct CompoundExpression<'a> {
    pub fields: Vec<CompoundExpressionField<'a>>,
}

pub struct CompoundExpressionField<'a> {
    pub target_field: Cow<'a, str>,
    pub expression: Expression<'a>,
}

impl<'a> sealed::ExpressionTrait<'a> for CompoundExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

impl<'a> sealed::ExpressionTrait<'a> for Resolution<'a, Weak<VariableItem<'a>>> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

pub enum LiteralExpression {
    Bool(bool),
    Decimal(f32),
    /// An integer with no sign or decimal point.
    Integer(u32),
    /// An integer with an explicitly specified sign.
    SignedInt(i32),
}

impl<'a> sealed::ExpressionTrait<'a> for LiteralExpression {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        Ok(match self {
            LiteralExpression::Bool(b) => TypeValue::Primitive(Primitive::Bool(Some(*b))),
            LiteralExpression::Decimal(d) => TypeValue::Primitive(Primitive::Float1(Some(*d))),
            LiteralExpression::Integer(i) => TypeValue::Primitive(Primitive::AmbiguousInteger(*i)),
            LiteralExpression::SignedInt(s) => TypeValue::Primitive(Primitive::Signed1(Some(*s))),
        })
    }
}

pub enum Expression<'a> {
    Variable(Resolution<'a, Weak<VariableItem<'a>>>),
    Literal(LiteralExpression),
    Unary(UnaryExpression<'a>),
    Binary(BinaryExpression<'a>),
    Array(ArrayExpression<'a>),
    IfExpr(IfExpression<'a>),
    FunctionalCall(CallExpression<'a>),
    CompoundInit(CompoundExpression<'a>),
    Block(BlockExpression<'a>),
    Paren(Box<Expression<'a>>),
}

impl<'a> sealed::ExpressionTrait<'a> for Expression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        match self {
            Expression::Variable(e) => e.try_get_typeval(),
            Expression::Literal(e) => e.try_get_typeval(),
            Expression::Unary(e) => e.try_get_typeval(),
            Expression::Binary(e) => e.try_get_typeval(),
            Expression::Array(e) => e.try_get_typeval(),
            Expression::IfExpr(e) => e.try_get_typeval(),
            Expression::FunctionalCall(e) => e.try_get_typeval(),
            Expression::CompoundInit(e) => e.try_get_typeval(),
            Expression::Block(e) => e.try_get_typeval(),
            Expression::Paren(e) => e.try_get_typeval(),
        }
    }
}
