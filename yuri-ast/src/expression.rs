use std::fmt::{Display, Formatter};

use thin_vec::ThinVec;
use yuri_common::{BinaryOperator, UnaryOperator};

use crate::item::{FunctionItem, TypeAliasItem, VariableItem};
use crate::{Ident, Qpath};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Either a variable, a module, or a type.
    /// We can't really figure out which until resolution.
    Access(Ident),
    /// Field lookup.
    Path(PathExpr),
    /// A value to be evaluated at compile-time.
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Array(ArrayExpr),
    IfExpr(IfExpr),
    MatchExpr(MatchExpr),
    FunctionalCall(CallExpr),
    Compound(CompoundExpr),
    Block(BlockExpr),
    Paren(Box<Expression>),
    #[cfg(debug_assertions)]
    Unimplemented {
        file: &'static str,
        line: u32,
        column: u32,
    },
}

#[cfg(debug_assertions)]
#[macro_export]
macro_rules! expression_unimplemented {
    () => {
        Expression::Unimplemented {
            file: file!(),
            line: line!(),
            column: column!(),
        }
    };
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
expression_from_helper!(PathExpr, Path);
expression_from_helper!(LiteralExpr, Literal);
expression_from_helper!(UnaryExpr, Unary);
expression_from_helper!(BinaryExpr, Binary);
expression_from_helper!(ArrayExpr, Array);
expression_from_helper!(IfExpr, IfExpr);
expression_from_helper!(CallExpr, FunctionalCall);
expression_from_helper!(CompoundExpr, Compound);
expression_from_helper!(BlockExpr, Block);

impl Display for Expression {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

/// Path/dot/field access notation.
/// The receiver is the expression to look up the following fields on.
/// While PathExpr could be recursive, it's faster and more size-efficient to use an array.
#[derive(Debug, Clone, PartialEq)]
pub struct PathExpr {
    pub receiver: Box<Expression>,
    pub fields: ThinVec<Ident>,
}

impl PathExpr {
    #[inline]
    pub fn new_e(
        receiver: impl Into<Expression>,
        fields: impl IntoIterator<Item = Ident>,
    ) -> Expression {
        Self {
            receiver: Box::new(receiver.into()),
            fields: fields.into_iter().collect(),
        }
        .into()
    }
}

// TODO: disambiguate between hex/binary/etc.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LiteralExpr {
    Bool(bool),
    /// Known to be a float (i.e. decimal point)
    Decimal(f64),
    /// Math constant; the ratio between the circumference and the diameter of a circle.
    Pi,
    /// Math constant; the ratio between the circumference and the radius of a circle.
    Tau,
    /// Infinity.
    Inf,
    /// Not a number. Used instead of storing NAN in the decimal variant for clarity.
    Nan,
    // TODO: improve this specificity
    /// Known to be an integer (i.e. non-decimal base)
    Integer(i128),
    /// May be coerced to any possible type
    Number(i128),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

impl UnaryExpr {
    #[inline]
    pub fn new_e(operator: UnaryOperator, value: impl Into<Expression>) -> Expression {
        Self {
            operator,
            value: Box::new(value.into()),
        }
        .into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

impl BinaryExpr {
    #[inline]
    pub fn new_e(
        operator: BinaryOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Expression {
        Self {
            operator,
            lhs: Box::new(lhs.into()),
            rhs: Box::new(rhs.into()),
        }
        .into()
    }
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
pub struct BlockExpr {
    /// The scope that this block creates.
    pub statements: Vec<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    /// The boolean condition of the conditional expression.
    pub condition: Box<Expression>,
    /// If the condition is true, evaluate this block.
    pub consequence: Box<BlockExpr>,
    /// Optionally, an "else" chain.
    pub otherwise: Option<ElseChain>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseChain {
    /// An "else if" part
    Chain(Box<IfExpr>),
    /// An unconditional "else" block
    Otherwise(Box<BlockExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    /// The value to match on.
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayExpr {
    /// All elements of the array are listed one-by-one.
    Elements(Vec<Expression>),
    /// One value is copied N times.
    Spread {
        /// The expression whose value should be copied.
        element: Box<Expression>,
        /// The number of copied (and the length of the array)
        length: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundExpr {
    pub fields: Vec<CompoundExpressionField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundExpressionField {
    pub target_field: Ident,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    /// The function to call, usually a [VariableExpr], [PathExpr], [LiteralExpr], or another [CallExpr].
    pub receiver: Box<Expression>,
    pub arguments: ThinVec<Expression>,
}

impl CallExpr {
    #[inline]
    pub fn new_e(
        receiver: impl Into<Expression>,
        arguments: impl IntoIterator<Item = impl Into<Expression>>,
    ) -> Expression {
        Self {
            receiver: Box::new(receiver.into()),
            arguments: arguments.into_iter().map(Into::into).collect(),
        }
        .into()
    }
}
