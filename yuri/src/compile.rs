use std::{marker::PhantomData, sync::Arc};

use nalgebra::{Matrix2, Matrix3, Matrix4, Vector2, Vector3, Vector4};

mod sealed {
    // use crate::compile::TypeValue;
    pub trait TypeValueTrait<'a>: Send + Sync + 'a {}
    // pub trait Evaluable<'a>: Send + Sync + 'a {
    //     fn eval(&self) -> TypeValue;
    // }
}

pub enum TypeValue<'a> {
    Primitive(Primitive),
    Array(Array<'a, dyn sealed::TypeValueTrait<'a>>),
    Compound(CompoundType<'a>),
    Alias(Box<TypeAlias<'a>>),
}

impl<'a> sealed::TypeValueTrait<'a> for () {}
impl<'a> sealed::TypeValueTrait<'a> for Primitive {}
impl<'a> sealed::TypeValueTrait<'a> for CompoundType<'a> {}
impl<'a> sealed::TypeValueTrait<'a> for TypeValue<'a> {}
impl<'a> sealed::TypeValueTrait<'a> for TypeAlias<'a> {}
impl<'a, T: sealed::TypeValueTrait<'a>> sealed::TypeValueTrait<'a> for Array<'a, T> {}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct Module<'a> {
    pub name: String,
    pub submodules: Vec<Module<'a>>,
    pub scope: ScopeItems<'a>,
}

#[derive(Default)]
pub struct ScopeItems<'a> {
    pub variables: Vec<NamedItem<'a>>,
    pub functions: Vec<FunctionItem<'a>>,
    pub imports: Vec<Symbol<'a>>,
    pub type_aliases: Vec<TypeAlias<'a>>,
}

impl<'a> Module<'a> {
    pub fn empty(name: String) -> Self {
        Self {
            name,
            submodules: Default::default(),
            scope: Default::default(),
        }
    }
}

// Currently, the Yuri type system can only discriminate between **Known** and **Unknown** values.
// In future versions, the goal is to implement algebraic solving for partial unknowns.
#[derive(Clone, Copy)]
pub enum Primitive {
    Bool(Option<bool>),

    Float1(Option<f32>),
    Float2(Option<Vector2<f32>>),
    Float3(Option<Vector3<f32>>),
    Float4(Option<Vector4<f32>>),

    Signed1(Option<i32>),
    Signed2(Option<Vector2<i32>>),
    Signed3(Option<Vector3<i32>>),
    Signed4(Option<Vector4<i32>>),

    Unsigned1(Option<u32>),
    Unsigned2(Option<Vector2<u32>>),
    Unsigned3(Option<Vector3<u32>>),
    Unsigned4(Option<Vector4<u32>>),

    Mat2(Option<Matrix2<f32>>),
    Mat3(Option<Matrix3<f32>>),
    Mat4(Option<Matrix4<f32>>),
}

pub struct Symbol<'a> {
    path: Vec<&'a str>,
}

pub struct Attribute<'a> {
    // attributes will be weird.
    name: &'a str,
}

pub struct TypeAlias<'a> {
    pub name: &'a str,
    pub aliases: Option<TypeValue<'a>>,
}

/// A variable, function parameter, compound type field, or other named value storage.
pub struct NamedItem<'a> {
    attributes: Vec<Attribute<'a>>,
    name: &'a str,
    value_type: TypeValue<'a>,
}

pub struct Array<'a, T: sealed::TypeValueTrait<'a> + ?Sized> {
    // TODO: we can't really represent a homogenous array easily when the type of the array element is only known at runtime.
    // like, the size is known at runtime, but we can't do much with that.
    // the best we could do with type shamanry here is a generic on TypeValue (which would suck!)
    // vecs of boxed types suck.
    pub slots: Vec<Box<T>>,
    pub size: Option<u32>,
    pub _pd: PhantomData<&'a ()>,
}

pub struct CompoundType<'a> {
    pub fields: Vec<NamedItem<'a>>,
}

pub struct BlockExpression<'a> {
    // TODO: ohhh this is gonna require some brain power
    pub scope: Arc<ScopeItems<'a>>,
    pub expressions: Vec<Arc<Expression<'a>>>,
}

pub struct FunctionItem<'a> {
    pub attributes: Vec<Attribute<'a>>,
    pub name: &'a str,
    pub parameters: Vec<NamedItem<'a>>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: TypeValue<'a>,
    pub body: BlockExpression<'a>,
}

pub struct IfExpression<'a> {
    pub consequence: BlockExpression<'a>,
    pub condition: Box<Expression<'a>>,
    pub chained_else: Option<ElseChain<'a>>,
}

pub struct ElseChain<'a> {
    pub consequence: BlockExpression<'a>,
    pub condition: Option<Box<Expression<'a>>>,
    pub chained_else: Option<Box<ElseChain<'a>>>,
}

pub enum UnaryOperator {
    Invert,
    Negative,
    Positive,
}

pub struct UnaryExpression<'a> {
    operator: UnaryOperator,
    value: Box<Expression<'a>>,
}

pub enum BinaryOperator {
    LogicAnd,
    LogicOr,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gr,
    GrEq,
    ShiftLeft,
    ShiftRight,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
}

pub struct BinaryExpression<'a> {
    pub operator: BinaryOperator,
    pub a: Box<Expression<'a>>,
    pub b: Box<Expression<'a>>,
}

pub struct CallExpression<'a> {
    pub function: &'a str,
    pub arguments: Vec<(&'a str, Expression<'a>)>,
}

pub struct ArrayExpression<'a> {
    pub elements: Vec<Expression<'a>>,
}

pub struct CompoundExpression<'a> {
    pub fields: Vec<CompoundExpressionItem<'a>>,
}

/// A variable, function parameter, compound type field, or other named value storage.
pub struct CompoundExpressionItem<'a> {
    target_field: &'a str,
    expression: Expression<'a>,
}

pub enum Expression<'a> {
    Variable(Symbol<'a>),
    Literal(Primitive),
    Unary(UnaryExpression<'a>),
    Binary(BinaryExpression<'a>),
    Array(ArrayExpression<'a>),
    IfExpr(IfExpression<'a>),
    FunctionalCall(CallExpression<'a>),
    CompoundInit(CompoundExpression<'a>),
    Block(BlockExpression<'a>),
}
