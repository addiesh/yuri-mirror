use nalgebra::{DMatrix, Vector2, Vector3, Vector4};
use yuri_common::{DimensionCount, FloatBits, IntBits, ScalarTy};

use crate::error::TypeError;
use crate::types::{TypeValue, Typeable};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MatrixTyVal {
    pub columns: DimensionCount,
    pub rows: DimensionCount,
    pub bitsize: Option<FloatBits>,
    // TODO: store the data in a way that makes sense here
    // pub data: DMatrix,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VectorTyVal {
    pub size: DimensionCount,
    pub repr: ScalarTy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScalarTyVal {
    Signed8(Option<i8>),
    Signed16(Option<i16>),
    Signed32(Option<i32>),
    Signed64(Option<i64>),
    /// May be any signed number
    SignedX(Option<i64>),
    /// May be any number, but has a sign.
    SignedNum(Option<i64>),
    Unsigned8(Option<u8>),
    Unsigned16(Option<u16>),
    Unsigned32(Option<u32>),
    Unsigned64(Option<u64>),
    /// May be any unsigned number.
    UnsignedX(Option<u64>),
    /// May be any number, but cannot have a sign.
    UnsignedNum(Option<u64>),
    Float16(Option<f32>),
    Float32(Option<f32>),
    Float64(Option<f64>),
    /// May be any float number.
    FloatX(Option<f64>),
    /// May be any number, but has a decimal point.
    FloatNum(Option<u64>),
}

// Currently, the Yuri type system can only discriminate between **Known** and **Unknown** values.
// In future versions, the goal is to implement algebraic solving for partial unknowns.
#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
    Bool(Option<bool>),
    Scalar(ScalarTyVal),
    Vector(VectorTyVal),
    Matrix(MatrixTyVal),
}

fn primitive_union<'a>(this: &Primitive, other: &Primitive) -> Result<Primitive, TypeError<'a>> {
    use Primitive::*;
    // TODO: technically the equality here has errors because of floating-point NaN.
    // fix if it becomes an issue.

    Ok(match (this, other) {
        // concrete equality
        (Bool(Some(a)), Bool(Some(b))) if a == b => Bool(Some(*a)),

        // (Mat2(Some(a)), Mat2(Some(b))) if a.clone() == b.clone() => Mat2(Some(a.clone())),
        // (Mat3(Some(a)), Mat3(Some(b))) if a.clone() == b.clone() => Mat3(Some(a.clone())),
        // (Mat4(Some(a)), Mat4(Some(b))) if a.clone() == b.clone() => Mat4(Some(a.clone())),

        // (AmbiguousInteger(a), AmbiguousInteger(b)) if a == b => AmbiguousInteger(*a),
        // (AmbiguousInteger(a), Signed1(b)) => match (i32::try_from(*a), *b) {
        //     (Err(_), _) => {
        //         return Err(TypeError::IntegerBounds(
        //             *a as _,
        //             TypeValue::Primitive(other.clone()),
        //         ));
        //     }
        //     (Ok(a), Some(b)) if a == b => Primitive::Signed1(Some(a)),
        //     (Ok(_), _) => Primitive::Signed1(None),
        // },
        // (AmbiguousInteger(a), Unsigned1(b)) => match (*a, *b) {
        //     (a, Some(b)) if a == b => Primitive::Unsigned1(Some(a)),
        //     (_, _) => Primitive::Unsigned1(None),
        // },

        // if the values aren't known or aren't equal, then broaden to the type.
        (Bool(_), Bool(_)) => Bool(None),

        _ => todo!("fix this whole awful code"),
        // _ => {
        //     return Err(TypeError::UnrelatedType(
        //         TypeValue::Primitive(this.clone()),
        //         TypeValue::Primitive(other.clone()),
        //     ));
        // }
    })
}

impl<'a> Typeable<'a> for Primitive {
    #[rustfmt::skip]
    fn intersect_with(&self, other: &Self) -> Result<Self, TypeError<'a>> {
        use Primitive::*;
        // macros can't expand to match arms
        match (self, other) {
            // ambiguous literals
            // (AmbiguousInteger(_), Float1(_)) => Err(TypeError::IntegerAsFloat(
            //     TypeValue::Primitive(self.clone()),
            //     TypeValue::Primitive(other.clone()),
            // )),
            // (AmbiguousInteger(a), Signed1(b)) => {
            //     match (i32::try_from(*a), *b) {
            //         (Err(_), _) => Err(TypeError::IntegerBounds(*a as _, TypeValue::Primitive(other.clone()))),
            //         (Ok(a), Some(b)) if a != b => {
            //             Err(TypeError::ConcreteInequality(
            //                 TypeValue::Primitive(self.clone()),
            //                 TypeValue::Primitive(other.clone()),
            //             ))
            //         }
            //         (Ok(a), _) => Ok(Self::Signed1(Some(a)))
            //     }
            // },
            // (AmbiguousInteger(a), Unsigned1(b)) => {
            //     match (*a, *b) {
            //         (a, Some(b)) if a != b => {
            //             Err(TypeError::ConcreteInequality(
            //                 TypeValue::Primitive(self.clone()),
            //                 TypeValue::Primitive(other.clone()),
            //             ))
            //         }
            //         (a, _) => Ok(Self::Unsigned1(Some(a)))
            //     }
            // },

            // type -> type
            (Bool(None), Bool(None)) => Ok(Bool(None)),

            // value -> value (needs equality relation)
            (Bool(Some(a)), Bool(Some(b))) => if a == b { Ok(Bool(Some(*a))) } else { Err(TypeError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },

            // value -> type
            (Bool(val), Bool(None)) => Ok(Bool(*val)),

            // type -> value (invalid)
            (Bool(None), Bool(Some(_)))
            => Err(TypeError::UnknownSubtype(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))),

            _ => todo!()
            // _ => Err(TypeError::UnrelatedType(
            //     TypeValue::Primitive(self.clone()),
            //     TypeValue::Primitive(other.clone())
            // )),
        }
    }

    fn union(this: &Self, other: &Self) -> Result<Self, TypeError<'a>> {
        let result = primitive_union(this, other);

        // validate commutativity
        #[cfg(debug_assertions)]
        {
            let check = primitive_union(other, this);
            match (&result, &check) {
                (Ok(result), Ok(check)) => debug_assert_eq!(result, check),
                // at the very least, it commutatively errors
                (Err(_result), Err(_check)) => {}
                _ => panic!(
                    "The commutativity guarantees of `TypeValue::union` were not upheld with type {this:?} and {other:?} (caused by primitive union)"
                ),
            }
        }
        result
    }

    fn is_resolved(&self) -> bool {
        true
    }
}
