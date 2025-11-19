use yuri_common::{BinaryOperator, UnaryOperator};
use yuri_parser::{Ident, Qpath};

use crate::error::CompileError;
use crate::resolution::Resolution;
use crate::scope::Scope;
use crate::types::TypeValue;
use crate::types::primitive::{Primitive, ScalarTyVal};
use crate::{FunctionItem, TypeAliasItem, VariableItem, Yrc, Ywk};

mod sealed {
    use crate::TypeValue;
    use crate::error::CompileError;

    pub trait ExpressionTrait<'src> {
        fn try_get_typeval(&self) -> Result<TypeValue<'src>, CompileError<'src>>;
    }
}

/// Local block element.
pub enum BlockStatement<'src> {
    Import(Qpath),
    Variable(Ywk<VariableItem<'src>>),
    TypeAlias(Ywk<TypeAliasItem<'src>>),
    Function(Ywk<FunctionItem<'src>>),
    Return(Expression<'src>),
    Expression(Expression<'src>),
}

pub struct BlockExpression<'src> {
    /// The scope that this block creates.
    pub scope: Yrc<Scope<'src>>,
    pub statements: Vec<BlockStatement<'src>>,
}

impl<'src> From<&yuri_parser::expression::BlockExpression> for BlockExpression<'src> {
    fn from(value: &yuri_parser::expression::BlockExpression) -> Self {
        let scope = Yrc::new(Scope::new().into());

        BlockExpression {
            scope,
            statements: value
                .statements
                .iter()
                .map(|stmt| {
                    use yuri_parser::expression::BlockStatement;
                    match stmt {
                        BlockStatement::LocalVariable(variable_item) => todo!(),
                        BlockStatement::TypeAlias(type_alias_item) => todo!(),
                        BlockStatement::Function(function_item) => todo!(),
                        BlockStatement::Return(expression) => todo!(),
                        BlockStatement::Import(ident) => todo!(),
                        BlockStatement::Value(expression) => todo!(),
                    }
                })
                .collect(),
        }
    }
}

impl<'src> sealed::ExpressionTrait<'src> for BlockExpression<'src> {
    fn try_get_typeval(&self) -> Result<TypeValue<'src>, CompileError<'src>> {
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

pub struct UnaryExpression<'src> {
    pub operator: UnaryOperator,
    pub value: Box<Expression<'src>>,
}

impl<'src> sealed::ExpressionTrait<'src> for UnaryExpression<'src> {
    fn try_get_typeval(&self) -> Result<TypeValue<'src>, CompileError<'src>> {
        todo!()
    }
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
    pub function: Resolution<Ywk<FunctionItem<'a>>>,
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
    pub target_field: Ident,
    pub expression: Expression<'a>,
}

impl<'a> sealed::ExpressionTrait<'a> for CompoundExpression<'a> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

impl<'a> sealed::ExpressionTrait<'a> for Resolution<Ywk<VariableItem<'a>>> {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        todo!()
    }
}

pub enum LiteralExpression {
    Bool(bool),
    /// A numeric literal with an explicit decimal point
    Decimal(f64),
    /// An integer with no sign, decimal point, or fancy notation.
    Integer(u64),

    /// idk figure this out
    UnsignReqInt(u64),
    /// An integer with an explicitly specified sign.
    SignReqInt(i64),
}

impl<'a> sealed::ExpressionTrait<'a> for LiteralExpression {
    fn try_get_typeval(&self) -> Result<TypeValue<'a>, CompileError<'a>> {
        use ScalarTyVal as Scalar;
        Ok(TypeValue::Primitive(match self {
            LiteralExpression::Bool(b) => Primitive::Bool(Some(*b)),
            LiteralExpression::Decimal(d) => Primitive::Scalar(Scalar::FloatX(Some(*d))),
            LiteralExpression::Integer(i) => Primitive::Scalar(Scalar::UnsignedX(Some(*i))),
            LiteralExpression::UnsignReqInt(u) => Primitive::Scalar(Scalar::UnsignedX(Some(*u))),
            LiteralExpression::SignReqInt(s) => Primitive::Scalar(Scalar::SignedX(Some(*s))),
        }))
    }
}

pub enum Expression<'a> {
    Variable(Resolution<Ywk<VariableItem<'a>>>),
    Literal(LiteralExpression),
    Unary(UnaryExpression<'a>),
    Binary(BinaryExpression<'a>),
    Array(ArrayExpression<'a>),
    IfExpr(IfExpression<'a>),
    FunctionalCall(CallExpression<'a>),
    CompoundInit(CompoundExpression<'a>),
    Block(BlockExpression<'a>),
    Paren(Box<Expression<'a>>),
    Unimplemented,
    Invalid,
}

impl<'src> From<&yuri_parser::expression::Expression> for Expression<'src> {
    fn from(value: &yuri_parser::expression::Expression) -> Self {
        use yuri_parser::expression::Expression as Exp;
        use yuri_parser::expression::LiteralExpression as Lit;
        match value {
            Exp::Variable(qpath) => Expression::Variable(Resolution::Unresolved(qpath.clone())),
            Exp::Literal(lit) => Expression::Literal(match lit {
                Lit::Bool(b) => LiteralExpression::Bool(*b),
                Lit::Decimal(d) => LiteralExpression::Decimal(*d),
                // TODO: this sucks, held up by deciding how to represent numbers elsewhere
                Lit::Number(i) | Lit::Integer(i) => LiteralExpression::Integer(*i as _),
            }),
            Exp::Unary(unary) => Expression::Unary(UnaryExpression {
                operator: todo!(),
                value: todo!(),
            }),
            Exp::Binary(binary) => todo!(),
            Exp::Array(array) => todo!(),
            Exp::IfExpr(riffx) => todo!(),
            Exp::FunctionalCall(call) => todo!(),
            Exp::CompoundInit(compound) => todo!(),
            Exp::Block(expr) => Expression::Block(expr.into()),
            Exp::Paren(expression) => todo!(),
            Exp::Unimplemented => Expression::Unimplemented,
        }
    }
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
            Expression::Unimplemented => todo!("implement type"),
            Expression::Invalid => panic!("invalid type propagated too far"),
        }
    }
}
