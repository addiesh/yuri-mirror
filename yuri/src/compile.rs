use std::borrow::Cow;
use std::sync::{Arc, Weak};

use nalgebra::{Field, Matrix2, Matrix3, Matrix4, Vector2, Vector3, Vector4};

mod sealed {
    use super::TypeValue;
    // super Into, not From
    pub trait TypeValueTrait<'a>: Send + Sync + 'a {
        fn into_type_value(self) -> TypeValue<'a>;
    }
    // pub trait Evaluable<'a>: Send + Sync + 'a {
    //     fn eval(&self) -> TypeValue;
    // }
}

#[derive(Clone)]
pub enum DuckTypeError<'a> {
    /// We can't check whether the two values can be equal because we don't know what one of them actually is.
    Unresolved(Symbol<'a>),
    /// The two values are 100% known but are not equal themselves.
    ConcreteInequality,
    /// A is a subtype of B but not vice-versa.
    Subtype,
    /// A conversion that doesn't make sense, like Primitive to Compound.
    UnrelatedType,
    IncompatibleLength,
    IncompatibleFields,
}

pub trait Duck<'a>: Sized + 'a {
    /// Attempts to convert self into other.
    fn quack(&self, other: &Self) -> Result<Self, DuckTypeError<'a>>;
}

pub trait AttributeProvider<'a>: 'a + Send + Sync + Unpin {}

/// To the Yuri compiler, all "values" are actually types.
/// Whenver you would store a *value* in the Yuri AST, you store a type.
/// Whenever you would store a *type annotation* in the Yuri AST, you store a type.
#[derive(Clone)]
pub enum TypeValue<'a> {
    Primitive(Primitive),
    Array(Array<'a>),
    Compound(CompoundType<'a>),
    Alias(Resolution<'a, Weak<TypeAliasItem<'a>>>),
}

impl<'a> Duck<'a> for TypeValue<'a> {
    fn quack(&self, other: &Self) -> Result<Self, DuckTypeError<'a>> {
        use TypeValue::*;
        match (self, other) {
            (Primitive(a), Primitive(b)) => a.quack(b).map(|p| Primitive(p)),
            (Array(a), Array(b)) => a.quack(b).map(|a| Array(a)),
            (Compound(a), Compound(b)) => a.quack(b).map(|c| Compound(c)),

            // unresolved alias
            (Alias(Resolution::Unresolved(symbol)), _)
            | (_, Alias(Resolution::Unresolved(symbol))) => {
                Err(DuckTypeError::Unresolved(symbol.clone()))
            }

            (Alias(Resolution::Resolved { symbol, item }), other) => {
                if let Some(upgraded) = item.upgrade() {
                    upgraded.aliases.quack(other)
                } else {
                    Err(DuckTypeError::Unresolved(symbol.clone()))
                }
            }

            (other, Alias(Resolution::Resolved { symbol, item })) => {
                if let Some(upgraded) = item.upgrade() {
                    other.quack(&upgraded.aliases)
                } else {
                    Err(DuckTypeError::Unresolved(symbol.clone()))
                }
            }
            _ => Err(DuckTypeError::UnrelatedType),
        }
    }
}

impl<'a> sealed::TypeValueTrait<'a> for Primitive {
    fn into_type_value(self) -> TypeValue<'a> {
        TypeValue::Primitive(self)
    }
}
impl<'a> sealed::TypeValueTrait<'a> for Array<'a> {
    fn into_type_value(self) -> TypeValue<'a> {
        todo!("arrays cannot be coerced into `TypeValue` yet")
        // TypeValue::Array(*self as Array<'a, dyn sealed::TypeValueTrait<'a>>)
    }
}
impl<'a> sealed::TypeValueTrait<'a> for CompoundType<'a> {
    fn into_type_value(self) -> TypeValue<'a> {
        TypeValue::Compound(self)
    }
}
impl<'a> sealed::TypeValueTrait<'a> for Resolution<'a, Weak<TypeAliasItem<'a>>> {
    fn into_type_value(self) -> TypeValue<'a> {
        TypeValue::Alias(self)
    }
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct Module<'a> {
    pub parent: Option<Weak<Module<'a>>>,
    pub name: Cow<'a, str>,
    pub submodules: Vec<Arc<Module<'a>>>,
    pub scope: ScopeItems<'a>,
}

/// A "rule" in yuri is a guarantee that some condition will always
/// be true wherever the rule is in place. This allows for, among other things,
/// more advanced optimization. As such, this is a complicated idea to implement,
/// so we *probably* won't be implementing it for the V1 Yuri compiler.
pub struct ScopeRule {/* empty for now */}

#[derive(Default)]
pub struct ScopeItems<'a> {
    /// - For root modules, this is None.
    /// - For function blocks, this is the block/module scope in which they are defined.
    /// - For expression blocks, this is either the function scope
    ///   or the block scope (recursive) in which it is defined.
    pub parent: Option<Weak<ScopeItems<'a>>>,
    pub variables: Vec<Arc<Variable<'a>>>,
    pub functions: Vec<Arc<FunctionItem<'a>>>,
    pub imports: Vec<Arc<Symbol<'a>>>,
    pub type_aliases: Vec<Arc<TypeAliasItem<'a>>>,
    pub rules: Vec<ScopeRule>,
}

/// Despite the name, all variables are immutable.
pub struct Variable<'a> {
    pub name: &'a str,
    pub parent_scope: Weak<ScopeItems<'a>>,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: TypeValue<'a>,
    pub value_type: TypeValue<'a>,
}

impl<'a> Module<'a> {
    pub fn empty(name: Cow<'a, str>, parent: Option<Weak<Module<'a>>>) -> Self {
        Self {
            parent,
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

impl<'a> Duck<'a> for Primitive {
    #[rustfmt::skip]
    fn quack(&self, other: &Self) -> Result<Self, DuckTypeError<'a>> {
        use Primitive::*;
        // macros can't expand to match arms
        match (self, other) {
            // type -> type
            (Bool(None), Bool(None)) => Ok(Bool(None)),
            (Float1(None), Float1(None)) => Ok(Float1(None)),
            (Float2(None), Float2(None)) => Ok(Float2(None)),
            (Float3(None), Float3(None)) => Ok(Float3(None)),
            (Float4(None), Float4(None)) => Ok(Float4(None)),
            (Signed1(None), Signed1(None)) => Ok(Signed1(None)),
            (Signed2(None), Signed2(None)) => Ok(Signed2(None)),
            (Signed3(None), Signed3(None)) => Ok(Signed3(None)),
            (Signed4(None), Signed4(None)) => Ok(Signed4(None)),
            (Unsigned1(None), Unsigned1(None)) => Ok(Unsigned1(None)),
            (Unsigned2(None), Unsigned2(None)) => Ok(Unsigned2(None)),
            (Unsigned3(None), Unsigned3(None)) => Ok(Unsigned3(None)),
            (Unsigned4(None), Unsigned4(None)) => Ok(Unsigned4(None)),
            (Mat2(None), Mat2(None)) => Ok(Mat2(None)),
            (Mat3(None), Mat3(None)) => Ok(Mat3(None)),
            (Mat4(None), Mat4(None)) => Ok(Mat4(None)),

            // value -> value (needs equality relation)
            (Bool(Some(a)), Bool(Some(b))) => if a == b { Ok(Bool(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Float1(Some(a)), Float1(Some(b))) => if a == b { Ok(Float1(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Float2(Some(a)), Float2(Some(b))) => if a == b { Ok(Float2(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Float3(Some(a)), Float3(Some(b))) => if a == b { Ok(Float3(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Float4(Some(a)), Float4(Some(b))) => if a == b { Ok(Float4(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Signed1(Some(a)), Signed1(Some(b))) => if a == b { Ok(Signed1(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Signed2(Some(a)), Signed2(Some(b))) => if a == b { Ok(Signed2(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Signed3(Some(a)), Signed3(Some(b))) => if a == b { Ok(Signed3(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Signed4(Some(a)), Signed4(Some(b))) => if a == b { Ok(Signed4(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Unsigned1(Some(a)), Unsigned1(Some(b))) => if a == b { Ok(Unsigned1(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Unsigned2(Some(a)), Unsigned2(Some(b))) => if a == b { Ok(Unsigned2(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Unsigned3(Some(a)), Unsigned3(Some(b))) => if a == b { Ok(Unsigned3(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Unsigned4(Some(a)), Unsigned4(Some(b))) => if a == b { Ok(Unsigned4(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Mat2(Some(a)), Mat2(Some(b))) => if a == b { Ok(Mat2(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Mat3(Some(a)), Mat3(Some(b))) => if a == b { Ok(Mat3(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },
            (Mat4(Some(a)), Mat4(Some(b))) => if a == b { Ok(Mat4(Some(*a))) } else { Err(DuckTypeError::ConcreteInequality) },

            // value -> type
            (Bool(val), Bool(None)) => Ok(Bool(*val)),
            (Float1(val), Float1(None)) => Ok(Float1(*val)),
            (Float2(val), Float2(None)) => Ok(Float2(*val)),
            (Float3(val), Float3(None)) => Ok(Float3(*val)),
            (Float4(val), Float4(None)) => Ok(Float4(*val)),
            (Signed1(val), Signed1(None)) => Ok(Signed1(*val)),
            (Signed2(val), Signed2(None)) => Ok(Signed2(*val)),
            (Signed3(val), Signed3(None)) => Ok(Signed3(*val)),
            (Signed4(val), Signed4(None)) => Ok(Signed4(*val)),
            (Unsigned1(val), Unsigned1(None)) => Ok(Unsigned1(*val)),
            (Unsigned2(val), Unsigned2(None)) => Ok(Unsigned2(*val)),
            (Unsigned3(val), Unsigned3(None)) => Ok(Unsigned3(*val)),
            (Unsigned4(val), Unsigned4(None)) => Ok(Unsigned4(*val)),
            (Mat2(val), Mat2(None)) => Ok(Mat2(*val)),
            (Mat3(val), Mat3(None)) => Ok(Mat3(*val)),
            (Mat4(val), Mat4(None)) => Ok(Mat4(*val)),

            // type -> value (invalid)
            (Bool(None), Bool(Some(_)))
            | (Float1(None), Float1(Some(_)))
            | (Float2(None), Float2(Some(_)))
            | (Float3(None), Float3(Some(_)))
            | (Float4(None), Float4(Some(_)))
            | (Signed1(None), Signed1(Some(_)))
            | (Signed2(None), Signed2(Some(_)))
            | (Signed3(None), Signed3(Some(_)))
            | (Signed4(None), Signed4(Some(_)))
            | (Unsigned1(None), Unsigned1(Some(_)))
            | (Unsigned2(None), Unsigned2(Some(_)))
            | (Unsigned3(None), Unsigned3(Some(_)))
            | (Unsigned4(None), Unsigned4(Some(_)))
            | (Mat2(None), Mat2(Some(_)))
            | (Mat3(None), Mat3(Some(_)))
            | (Mat4(None), Mat4(Some(_))) => Err(DuckTypeError::Subtype),

            _ => Err(DuckTypeError::UnrelatedType),
        }
    }
}

#[derive(Clone)]
pub enum Resolution<'a, T> {
    Unresolved(Symbol<'a>),
    Resolved { symbol: Symbol<'a>, item: T },
}

#[derive(Clone)]
pub struct Symbol<'a> {
    path: Cow<'a, [&'a str]>,
}

#[derive(Clone)]
pub struct Attribute<'a> {
    // attributes will be weird.
    pub name: Resolution<'a, &'a dyn AttributeProvider<'a>>,
}

pub struct TypeAliasItem<'a> {
    pub name: &'a str,
    pub aliases: TypeValue<'a>,
}

#[derive(Clone)]
pub struct ArrayType<'a> {
    /// If this is Some, we know all elements of the array must be this type.
    /// If this is None, we don't know what the type of each element is.
    pub explicit_element_type: Box<TypeValue<'a>>,

    /// If this is Some, we know the size of the array at compile-time.
    /// If this is None, the array is unsized.
    pub explicit_len: Option<u32>,
}

#[derive(Clone)]
pub struct Array<'a> {
    // TODO: we can't really represent a homogenous array easily when the type of the array element is only known at runtime.
    // like, the size is known at runtime, but we can't do much with that.
    // the best we could do with type shamanry here is a generic on TypeValue (which would suck!)
    // vecs of boxed types suck.
    /// If this is Some, this can be a VALUE or a TYPE.
    /// If `explicit_len` is also Some, the number of slots must be equal to the `explicit_len`.
    /// If this is None, this can only be a TYPE.
    pub slots: Option<Vec<TypeValue<'a>>>,

    pub big_type: Option<ArrayType<'a>>,
}

impl<'a> Array<'a> {
    /// Whether this array is "valid," ensuring homogeny and matching type annotations.
    pub fn is_valid(&self) -> bool {
        // TODO: this is so complicated...
        todo!("validity checking for arrays is unimplemented")
        // match (self.slots, self.explicit_element_type, self.explicit_len) {
        //     (Some(slots), Some(e_len)) => {}
        //     (None, Some(e_len)) => {}
        //     // must have at least one field in order to be valid:
        //     // - No slots means no value
        //     // -
        //     (None, None, None) => false,
        // }
        // if self.slots.len() >= 2 {
        //     let mut accumulated_type = None;
        //     for slot in &self.slots {
        //         if let Some(slot) = slot {
        //             match accumulated_type {
        //                 None => {
        //                     accumulated_type = Some(slot);
        //                     continue;
        //                 }
        //                 Some(acc) => {
        //                     let quack = slot.quack(acc);
        //                     if quack.is_ok()
        //                 }
        //             }
        //         }
        //     }
        // }
        // true
    }
}

impl<'a> Duck<'a> for Array<'a> {
    fn quack(&self, other: &Self) -> Result<Self, DuckTypeError<'a>> {
        todo!("arrays cannot quack yet")
        // if self.len.is_some() && other.len.is_none() {}
        // match (self, other) {}
    }
}

#[derive(Clone)]
pub struct CompoundType<'a> {
    pub fields: Vec<CompoundTypeField<'a>>,
}

#[derive(Clone)]
pub struct CompoundTypeField<'a> {
    // pub parent: Weak<CompoundType<'a>>,
    pub attributes: Vec<Attribute<'a>>,
    pub name: &'a str,
    pub field_type: TypeValue<'a>,
}

// impl Clone for CompoundType {
//     fn clone(&self) -> Self {
//         Self {
//             fields: self.fields.iter().map(|field| CompoundTypeField).collect(),
//         }
//     }
// }

impl<'a> Duck<'a> for CompoundType<'a> {
    fn quack(&self, other: &Self) -> Result<Self, DuckTypeError<'a>> {
        if self.fields.len() != other.fields.len() {
            return Err(DuckTypeError::IncompatibleLength);
        }
        // should we quack based on field order, field name, or both?
        let results: Vec<Result<_, DuckTypeError>> = self
            .fields
            .iter()
            .zip(&other.fields)
            .map(|(a, b)| {
                // bias towards target
                Ok(CompoundTypeField {
                    field_type: a.field_type.quack(&b.field_type)?,
                    // parent: b.parent.clone(),
                    attributes: b.attributes.clone(),
                    name: b.name,
                })
            })
            .collect();

        results
            .iter()
            .filter_map(|res| if let Err(err) = res { Some(err) } else { None });

        todo!("compound types cannot quack yet")
    }
}

pub struct BlockExpression<'a> {
    // TODO: ohhh this is gonna require some brain power
    pub scope: Arc<ScopeItems<'a>>,
    pub statements: Vec<Expression<'a>>,
}

pub struct FunctionItem<'a> {
    pub attributes: Vec<Attribute<'a>>,
    pub name: &'a str,
    pub parameters: Vec<Variable<'a>>,
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
    pub function: Resolution<'a, Weak<FunctionItem<'a>>>,
    pub arguments: Vec<(&'a str, Expression<'a>)>,
}

pub struct ArrayExpression<'a> {
    pub elements: Vec<Expression<'a>>,
}

// Because of duck-typing, compound expression types in codegen
pub struct CompoundExpression<'a> {
    pub fields: Vec<CompoundExpressionItem<'a>>,
}

pub struct CompoundExpressionItem<'a> {
    pub target_field: &'a str,
    pub expression: Expression<'a>,
}

pub enum Expression<'a> {
    Variable(Resolution<'a, Weak<Variable<'a>>>),
    Literal(Primitive),
    Unary(UnaryExpression<'a>),
    Binary(BinaryExpression<'a>),
    Array(ArrayExpression<'a>),
    IfExpr(IfExpression<'a>),
    FunctionalCall(CallExpression<'a>),
    CompoundInit(CompoundExpression<'a>),
    Block(BlockExpression<'a>),
}
