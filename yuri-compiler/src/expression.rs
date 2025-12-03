use yuri_ast::{Ident, Qpath};
use yuri_common::{BinaryOperator, UnaryOperator};

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

impl ParseLower<BlockExpression> for yuri_ast::expression::BlockExpr {
    fn lower(&self) -> BlockExpression {
        let scope = Yrc::new(Scope::new().into());

        BlockExpression {
            scope,
            statements: self
                .statements
                .iter()
                .map(|stmt| {
                    use yuri_ast::expression::BlockStatement;
                    match stmt {
                        BlockStatement::LocalVariable(variable_item) => todo!(),
                        BlockStatement::TypeAlias(type_alias_item) => todo!(),
                        BlockStatement::Function(function_item) => todo!(),
                        BlockStatement::Return(expression) => todo!(),
                        BlockStatement::Import(ident) => todo!(),
                        BlockStatement::Expression(expression) => todo!(),
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

#[derive(Clone, Debug)]
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

impl<'src> sealed::ExpressionTrait<'src> for UnaryExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct CallExpression {
    /// The receiving expression, if this is a method call.
    /// For builtin constructors, module resolutions, or non-method functions, this is None.
    pub receiver: Option<Box<Expression>>,
    pub function: Resolution<Ywk<FunctionItem>>,
    pub arguments: Vec<Expression>,
}

impl<'src> sealed::ExpressionTrait<'src> for CallExpression {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'src>> {
        todo!()
    }
}

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub struct CompoundExpression {
    pub fields: Vec<CompoundExpressionField>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum LiteralExpr {
    Bool(bool),
    /// A numeric literal with either an explicit decimal point or using scientific notation.
    Decimal(f64),
    /// An integer with no sign, decimal point, or fancy notation.
    Integer(u64),
    BinInt(u64),
    HexInt(u64),
}

impl<'a> sealed::ExpressionTrait<'a> for LiteralExpr {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'a>> {
        use ScalarTyVal as Scalar;
        Ok(TypeValue::Primitive(match self {
            LiteralExpr::Bool(b) => Primitive::Bool(Some(*b)),
            LiteralExpr::Decimal(d) => Primitive::Scalar(Scalar::FloatX(Some(*d))),
            LiteralExpr::Integer(i) => Primitive::Scalar(Scalar::UnsignedX(Some(*i))),
            LiteralExpr::HexInt(u) | LiteralExpr::BinInt(u) => {
                Primitive::Scalar(Scalar::UnsignedX(Some(*u)))
            }
        }))
    }
}

#[derive(Clone, Debug)]
pub struct FieldExpr {
    pub receiver: Box<Expression>,
    pub field_name: Ident,
}

impl<'a> sealed::ExpressionTrait<'a> for FieldExpr {
    fn try_get_typeval(&self) -> Result<TypeValue, CompileError<'a>> {
        let receiver_tv = self.receiver.try_get_typeval()?;
        // TODO: we can't actually determine this without everything being lowered already.
        // Plus, we need to know the scope we're in because a function might be hoisted here.
        todo!()
    }
}

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

impl ParseLower<Expression> for yuri_ast::expression::Expression {
    fn lower(&self) -> Expression {
        use yuri_ast::expression::ArrayExpr as Arr;
        use yuri_ast::expression::Expression as Exp;
        use yuri_ast::expression::LiteralExpr as Lit;
        match self {
            Exp::Field(path_expr) => todo!(),
            Exp::Access(qpath) => {
                Expression::Variable(todo!("Resolution::Unresolved(qpath.clone())"))
            }
            Exp::Literal(lit) => Expression::Literal(match lit {
                Lit::Bool(b) => LiteralExpr::Bool(*b),
                Lit::Decimal(d) => LiteralExpr::Decimal(*d),
                Lit::Integer(i) => LiteralExpr::Integer(*i),
                Lit::BinInt(i) => LiteralExpr::BinInt(*i),
                Lit::HexInt(i) => LiteralExpr::HexInt(*i),
                Lit::Pi => todo!(),
                Lit::Tau => todo!(),
                Lit::Inf => todo!(),
                Lit::Nan => todo!(),
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
            Exp::Compound(compound) => todo!(),
            Exp::Block(expr) => Expression::Block(expr.lower()),
            Exp::Paren(expression) => todo!(),
            #[cfg(debug_assertions)]
            Exp::Unimplemented { file, line, column } => Expression::Unimplemented {
                file,
                line: *line,
                column: *column,
            },
            Exp::MatchExpr(match_expression) => todo!(),
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
            Expression::Field(e) => e.try_get_typeval(),
            #[cfg(debug_assertions)]
            Expression::Unimplemented { file, line, column } => {
                panic!("unimplemented (source @ {file}:{line}:{column})")
            }
            Expression::Invalid => panic!("invalid type propagated too far"),
        }
    }
}
