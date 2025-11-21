use yuri_common::{BinaryOperator, UnaryOperator};
use yuri_parser::{Ident, Qpath};

use crate::error::CompileError;
use crate::resolution::Resolution;
use crate::scope::Scope;
use crate::types::TypeValue;
use crate::types::primitive::{Primitive, ScalarTyVal};
use crate::{FunctionItem, ParseLower, TypeAliasItem, VariableItem, Yrc, Ywk};

mod sealed {
    use crate::TypeValue;
    use crate::error::CompileError;

    pub trait ExpressionTrait<'src> {
        fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>>;
    }
}

/// Local block element.
pub enum BlockStatement {
    Import(Qpath),
    Variable(Ywk<VariableItem>),
    TypeAlias(Ywk<TypeAliasItem>),
    Function(Ywk<FunctionItem>),
    Assign(Qpath, Expression),
    Return(Expression),
    Expression(Expression),
}

pub struct BlockExpression {
    /// The scope that this block creates.
    pub scope: Yrc<Scope>,
    pub statements: Vec<BlockStatement>,
}

impl ParseLower<BlockExpression> for yuri_parser::expression::BlockExpression {
    fn lower(&self) -> BlockExpression {
        let scope = Yrc::new(Scope::new().into());

        BlockExpression {
            scope,
            statements: self
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
                        BlockStatement::Assign(qpath, expression) => todo!(),
                    }
                })
                .collect(),
        }
    }
}

impl<'src> sealed::ExpressionTrait<'src> for BlockExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        // figure out if the expression is unreachable

        // if let Some(tail) = &self.tail {
        //     tail
        // }

        todo!()
    }
}

pub struct IfExpression {
    pub consequence: BlockExpression,
    pub condition: Box<Expression>,
    pub chained_else: Option<ElseChain>,
}

impl<'a> sealed::ExpressionTrait<'a> for IfExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'a>> {
        todo!()
    }
}

pub struct ElseChain {
    pub consequence: BlockExpression,
    pub condition: Option<Box<Expression>>,
    pub chained_else: Option<Box<ElseChain>>,
}

pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

impl From<UnaryExpression> for Expression {
    fn from(value: UnaryExpression) -> Self {
        Expression::Unary(value)
    }
}

impl<'src> sealed::ExpressionTrait<'src> for UnaryExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

impl<'src> sealed::ExpressionTrait<'src> for BinaryExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

pub struct CallExpression {
    pub function: Resolution<Ywk<FunctionItem>>,
    pub arguments: Vec<Expression>,
}

impl<'src> sealed::ExpressionTrait<'src> for CallExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

pub enum ArrayExpression {
    Elements(Vec<Expression>),
    Spread {
        element: Box<Expression>,
        length: Box<Expression>,
    },
}

impl<'src> sealed::ExpressionTrait<'src> for ArrayExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

// Because of duck-typing, compound expression types in codegen
pub struct CompoundExpression {
    pub fields: Vec<CompoundExpressionField>,
}

pub struct CompoundExpressionField {
    pub target_field: Ident,
    pub expression: Expression,
}

impl<'src> sealed::ExpressionTrait<'src> for CompoundExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

impl<'src> sealed::ExpressionTrait<'src> for Resolution<Ywk<VariableItem>> {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
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
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'a>> {
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

pub enum Expression {
    Variable(Resolution<Ywk<VariableItem>>),
    Literal(LiteralExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Array(ArrayExpression),
    IfExpr(IfExpression),
    FunctionalCall(CallExpression),
    CompoundInit(CompoundExpression),
    Block(BlockExpression),
    Paren(Box<Expression>),
    Unimplemented,
    Invalid,
}

impl ParseLower<Expression> for yuri_parser::expression::Expression {
    fn lower(&self) -> Expression {
        use yuri_parser::expression::ArrayExpression as Arr;
        use yuri_parser::expression::Expression as Exp;
        use yuri_parser::expression::LiteralExpression as Lit;
        match self {
            Exp::Variable(qpath) => Expression::Variable(Resolution::Unresolved(qpath.clone())),
            Exp::Literal(lit) => Expression::Literal(match lit {
                Lit::Bool(b) => LiteralExpression::Bool(*b),
                Lit::Decimal(d) => LiteralExpression::Decimal(*d),
                // TODO: this sucks, held up by deciding how to represent numbers elsewhere
                Lit::Number(i) | Lit::Integer(i) => LiteralExpression::Integer(*i as _),
            }),
            Exp::Unary(unary) => Expression::Unary(UnaryExpression {
                operator: unary.operator,
                value: unary.value.lower().into(),
            }),
            Exp::Binary(binary) => Expression::Binary(BinaryExpression {
                operator: binary.operator,
                lhs: binary.lhs.lower().into(),
                rhs: binary.rhs.lower().into(),
            }),
            Exp::Array(Arr::Elements(elements)) => Expression::Array(ArrayExpression::Elements(
                elements.iter().map(ParseLower::lower).collect(),
            )),
            Exp::Array(Arr::Spread { element, length }) => {
                Expression::Array(ArrayExpression::Spread {
                    element: element.lower().into(),
                    length: length.lower().into(),
                })
            }
            Exp::IfExpr(riffx) => todo!(),
            Exp::FunctionalCall(call) => todo!(),
            Exp::CompoundInit(compound) => todo!(),
            Exp::Block(expr) => Expression::Block(expr.lower()),
            Exp::Paren(expression) => todo!(),
            Exp::Unimplemented => Expression::Unimplemented,
        }
    }
}

impl<'src> sealed::ExpressionTrait<'src> for Expression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
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
