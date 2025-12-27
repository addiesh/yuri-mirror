use yuri_ast::{Ident, Qpath};
use yuri_common::{BinaryOperator, UnaryOperator};

use crate::resolution::Resolution;
use crate::scope::Scope;
use crate::{FunctionItem, TypeAliasItem, VariableItem, Yrc, Ywk};

/// Local block element.
#[derive(Clone, Debug)]
pub enum BlockStatement {
    /// Import a symbol into the scope.
    Import(Qpath),
    /// Declare a local variable
    VariableDecl(Ywk<VariableItem>),
    /// Declare a locally-available type alias.
    TypeAliasDecl(Ywk<TypeAliasItem>),
    /// Declare a locally-available function.
    FunctionDecl(Ywk<FunctionItem>),
    /// Assign a value to a local variable.
    Assign(Resolution<Ywk<VariableItem>>, Expression),
    /// Exit from the parent function early, returning the provided value.
    Return(Expression),
    /// Tail expression (or unused expression, but those only matter for control flow expressions.)
    Expression(Expression),

    // TODO
    Break(Option<Expression>),
    Continue(Option<Expression>),
}

#[derive(Clone, Debug)]
pub struct BlockExpression {
    /// The scope that this block creates.
    pub scope: Yrc<Scope>,
    pub statements: Vec<BlockStatement>,
}

#[derive(Clone, Debug)]
pub struct IfExpression {
    pub consequence: BlockExpression,
    pub condition: Box<Expression>,
    pub chained_else: Option<ElseChain>,
}

#[derive(Clone, Debug)]
pub struct ElseChain {
    pub consequence: BlockExpression,
    pub condition: Option<Box<Expression>>,
    pub chained_else: Option<Box<ElseChain>>,
}

#[derive(Clone, Debug)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

impl From<UnaryExpression> for Expression {
    fn from(value: UnaryExpression) -> Self {
        Expression::Unary(value)
    }
}

#[derive(Clone, Debug)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct CallExpression {
    /// The receiving expression, if this is a method call.
    /// For builtin constructors, module resolutions, or non-method functions, this is None.
    pub receiver: Option<Box<Expression>>,
    pub function: Resolution<Ywk<FunctionItem>>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub enum ArrayExpression {
    Elements(Vec<Expression>),
    Spread {
        element: Box<Expression>,
        length: Box<Expression>,
    },
}

// Because of duck-typing, compound expression types in codegen
#[derive(Clone, Debug)]
pub struct CompoundExpression {
    pub fields: Vec<CompoundExpressionField>,
}

#[derive(Clone, Debug)]
pub struct CompoundExpressionField {
    pub target_field: Ident,
    pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralExpr {
    Bool(bool),
    /// A numeric literal with either an explicit decimal point or using scientific notation.
    Decimal(f64),
    /// An integer with no sign, decimal point, or fancy notation.
    Integer(u64),
    BinInt(u64),
    HexInt(u64),
}

// impl sealed::ExpressionTrait for LiteralExpr {
//     fn try_get_typeval(&self) -> Result<TypeValue, TypeError> {
//         use ScalarTyVal as Scalar;
//         Ok(TypeValue::Primitive(match self {
//             LiteralExpr::Bool(b) => Primitive::Bool(Some(*b)),
//             LiteralExpr::Decimal(d) => Primitive::Scalar(Scalar::FloatX(Some(*d))),
//             LiteralExpr::Integer(i) => Primitive::Scalar(Scalar::UnsignedX(Some(*i))),
//             LiteralExpr::HexInt(u) | LiteralExpr::BinInt(u) => {
//                 Primitive::Scalar(Scalar::UnsignedX(Some(*u)))
//             }
//         }))
//     }
// }

#[derive(Clone, Debug)]
pub struct FieldExpr {
    pub receiver: Box<Expression>,
    pub field_name: Ident,
}

// impl sealed::ExpressionTrait for FieldExpr {
//     fn try_get_typeval(&self) -> Result<TypeValue, TypeError> {
//         let receiver_tv = self.receiver.try_get_typeval()?;
//         // TODO: we can't actually determine this without everything being lowered already.
//         // Plus, we need to know the scope we're in because a function might be hoisted here.
//         todo!()
//     }
// }

#[derive(Clone, Debug)]
pub enum Expression {
    /// A qualified variable.
    Variable(Resolution<Ywk<VariableItem>>),
    /// A typed-out primitive value.
    Literal(LiteralExpr),
    /// A unary expression. Currently, these are all prefixes.
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Array(ArrayExpression),
    IfExpr(IfExpression),
    /// Field access of a concrete type. Not to be confused
    Field(FieldExpr),
    /// Function/method calls and "constructor" builtins.
    FunctionalCall(CallExpression),
    CompoundInit(CompoundExpression),
    Block(BlockExpression),
    Invalid,
    #[cfg(debug_assertions)]
    Unimplemented {
        file: &'static str,
        line: u32,
        column: u32,
    },
}

// impl sealed::ExpressionTrait for Expression {
//     fn try_get_typeval(&self) -> Result<TypeValue, TypeError> {
//         match self {
//             Expression::Variable(e) => e.try_get_typeval(),
//             Expression::Literal(e) => e.try_get_typeval(),
//             Expression::Unary(e) => e.try_get_typeval(),
//             Expression::Binary(e) => e.try_get_typeval(),
//             Expression::Array(e) => e.try_get_typeval(),
//             Expression::IfExpr(e) => e.try_get_typeval(),
//             Expression::FunctionalCall(e) => e.try_get_typeval(),
//             Expression::CompoundInit(e) => e.try_get_typeval(),
//             Expression::Block(e) => e.try_get_typeval(),
//             Expression::Field(e) => e.try_get_typeval(),
//             #[cfg(debug_assertions)]
//             Expression::Unimplemented { file, line, column } => {
//                 panic!("unimplemented (source @ {file}:{line}:{column})")
//             }
//             Expression::Invalid => panic!("invalid type propagated too far"),
//         }
//     }
// }
