use std::fmt::Debug;

use nalgebra::{DMatrix, DVector};
use yuri_common::DimCount;

use crate::error::TypeError;
use crate::types::{TyVal, Typeable};

#[derive(Debug, Clone, PartialEq)]
pub struct MatrixTyVal {
    pub columns: DimCount,
    pub rows: DimCount,
    pub data: MatrixStorage,
}

// TODO: this sucks. please rework

#[derive(Debug, Clone, PartialEq)]
pub enum MatrixStorage {
    Signed8(Option<DMatrix<i8>>),
    Signed16(Option<DMatrix<i16>>),
    Signed32(Option<DMatrix<i32>>),
    Signed64(Option<DMatrix<i64>>),
    SignedAny(Option<DMatrix<i64>>),

    Unsigned8(Option<DMatrix<u8>>),
    Unsigned16(Option<DMatrix<u16>>),
    Unsigned32(Option<DMatrix<u32>>),
    Unsigned64(Option<DMatrix<u64>>),
    UnsignedAny(Option<DMatrix<u64>>),

    Float16(Option<DMatrix<f32>>),
    Float32(Option<DMatrix<f32>>),
    Float64(Option<DMatrix<f64>>),
    FloatAny(Option<DMatrix<f64>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VectorTyVal {
    pub size: DimCount,
    pub data: VectorStorage,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VectorStorage {
    Signed8(Option<DVector<i8>>),
    Signed16(Option<DVector<i16>>),
    Signed32(Option<DVector<i32>>),
    Signed64(Option<DVector<i64>>),
    SignedAny(Option<DVector<i64>>),

    Unsigned8(Option<DVector<u8>>),
    Unsigned16(Option<DVector<u16>>),
    Unsigned32(Option<DVector<u32>>),
    Unsigned64(Option<DVector<u64>>),
    UnsignedAny(Option<DVector<u64>>),

    Float16(Option<DVector<f32>>),
    Float32(Option<DVector<f32>>),
    Float64(Option<DVector<f64>>),
    FloatAny(Option<DVector<f64>>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScalarTyVal {
    Signed8(Option<i8>),
    Signed16(Option<i16>),
    Signed32(Option<i32>),
    Signed64(Option<i64>),
    SignedAny(Option<i64>),

    Unsigned8(Option<u8>),
    Unsigned16(Option<u16>),
    Unsigned32(Option<u32>),
    Unsigned64(Option<u64>),
    UnsignedAny(Option<u64>),

    Float16(Option<f32>),
    Float32(Option<f32>),
    Float64(Option<f64>),
    FloatAny(Option<f64>),

    NaN,
    Pi,
    Tau,
}

// Currently, the Yuri type system can only discriminate between **Known** and **Unknown** values.
// In future versions, the goal is to implement algebraic solving for partial unknowns.
#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveTyVal {
    Bool(Option<bool>),
    Scalar(ScalarTyVal),
    Vector(VectorTyVal),
    Matrix(MatrixTyVal),
}

fn primitive_union(
    this: &PrimitiveTyVal,
    other: &PrimitiveTyVal,
) -> Result<PrimitiveTyVal, TypeError> {
    use PrimitiveTyVal::*;
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

impl Typeable for PrimitiveTyVal {
    #[rustfmt::skip]
    fn intersect_with(&self, other: &Self) -> Result<Self, TypeError> {
        use PrimitiveTyVal::*;
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
            (Bool(Some(a)), Bool(Some(b))) => if a == b { Ok(Bool(Some(*a))) } else { Err(TypeError::ConcreteInequality(TyVal::Primitive(self.clone()), TyVal::Primitive(other.clone()))) },

            // value -> type
            (Bool(val), Bool(None)) => Ok(Bool(*val)),

            // type -> value (invalid)
            (Bool(None), Bool(Some(_)))
            => Err(TypeError::UnknownSubtype(TyVal::Primitive(self.clone()), TyVal::Primitive(other.clone()))),

            _ => todo!()
            // _ => Err(TypeError::UnrelatedType(
            //     TypeValue::Primitive(self.clone()),
            //     TypeValue::Primitive(other.clone())
            // )),
        }
    }

    fn union(this: &Self, other: &Self) -> Result<Self, TypeError> {
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
