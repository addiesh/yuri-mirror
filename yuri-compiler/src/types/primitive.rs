use nalgebra::{Matrix2, Matrix3, Matrix4, Vector2, Vector3, Vector4};

use crate::CompileError;
use crate::types::{TypeValue, Typeable};

// Currently, the Yuri type system can only discriminate between **Known** and **Unknown** values.
// In future versions, the goal is to implement algebraic solving for partial unknowns.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Primitive {
    Bool(Option<bool>),

    Float1(Option<f32>),
    Float2(Option<Box<Vector2<f32>>>),
    Float3(Option<Box<Vector3<f32>>>),
    Float4(Option<Box<Vector4<f32>>>),

    AmbiguousInteger(u32),

    Signed1(Option<i32>),
    Signed2(Option<Box<Vector2<i32>>>),
    Signed3(Option<Box<Vector3<i32>>>),
    Signed4(Option<Box<Vector4<i32>>>),

    Unsigned1(Option<u32>),
    Unsigned2(Option<Box<Vector2<u32>>>),
    Unsigned3(Option<Box<Vector3<u32>>>),
    Unsigned4(Option<Box<Vector4<u32>>>),

    Mat2(Option<Box<Matrix2<f32>>>),
    Mat3(Option<Box<Matrix3<f32>>>),
    Mat4(Option<Box<Matrix4<f32>>>),
}

fn primitive_union<'a>(this: &Primitive, other: &Primitive) -> Result<Primitive, CompileError<'a>> {
    use Primitive::*;
    // TODO: technically the equality here has errors because of floating-point NaN.
    // fix if it becomes an issue.

    Ok(match (this, other) {
        // concrete equality
        (Bool(Some(a)), Bool(Some(b))) if a == b => Bool(Some(*a)),

        (Float1(Some(a)), Float1(Some(b))) if a == b => Float1(Some(*a)),
        (Float2(Some(a)), Float2(Some(b))) if a.clone() == b.clone() => Float2(Some(a.clone())),
        (Float3(Some(a)), Float3(Some(b))) if a.clone() == b.clone() => Float3(Some(a.clone())),
        (Float4(Some(a)), Float4(Some(b))) if a.clone() == b.clone() => Float4(Some(a.clone())),

        (Signed1(Some(a)), Signed1(Some(b))) if a == b => Signed1(Some(*a)),
        (Signed2(Some(a)), Signed2(Some(b))) if a.clone() == b.clone() => Signed2(Some(a.clone())),
        (Signed3(Some(a)), Signed3(Some(b))) if a.clone() == b.clone() => Signed3(Some(a.clone())),
        (Signed4(Some(a)), Signed4(Some(b))) if a.clone() == b.clone() => Signed4(Some(a.clone())),

        (Unsigned1(Some(a)), Unsigned1(Some(b))) if a == b => Unsigned1(Some(*a)),
        (Unsigned2(Some(a)), Unsigned2(Some(b))) if a.clone() == b.clone() => {
            Unsigned2(Some(a.clone()))
        }
        (Unsigned3(Some(a)), Unsigned3(Some(b))) if a.clone() == b.clone() => {
            Unsigned3(Some(a.clone()))
        }
        (Unsigned4(Some(a)), Unsigned4(Some(b))) if a.clone() == b.clone() => {
            Unsigned4(Some(a.clone()))
        }

        (Mat2(Some(a)), Mat2(Some(b))) if a.clone() == b.clone() => Mat2(Some(a.clone())),
        (Mat3(Some(a)), Mat3(Some(b))) if a.clone() == b.clone() => Mat3(Some(a.clone())),
        (Mat4(Some(a)), Mat4(Some(b))) if a.clone() == b.clone() => Mat4(Some(a.clone())),

        (AmbiguousInteger(a), AmbiguousInteger(b)) if a == b => AmbiguousInteger(*a),
        (AmbiguousInteger(a), Signed1(b)) => match (i32::try_from(*a), *b) {
            (Err(_), _) => {
                return Err(CompileError::IntegerBounds(
                    *a as _,
                    TypeValue::Primitive(other.clone()),
                ));
            }
            (Ok(a), Some(b)) if a == b => Primitive::Signed1(Some(a)),
            (Ok(_), _) => Primitive::Signed1(None),
        },
        (AmbiguousInteger(a), Unsigned1(b)) => match (*a, *b) {
            (a, Some(b)) if a == b => Primitive::Unsigned1(Some(a)),
            (_, _) => Primitive::Unsigned1(None),
        },

        // if the values aren't known or aren't equal, then broaden to the type.
        (Bool(_), Bool(_)) => Bool(None),
        (Float1(_), Float1(_)) => Float1(None),
        (Float2(_), Float2(_)) => Float2(None),
        (Float3(_), Float3(_)) => Float3(None),
        (Float4(_), Float4(_)) => Float4(None),
        (Signed1(_), Signed1(_)) => Signed1(None),
        (Signed2(_), Signed2(_)) => Signed2(None),
        (Signed3(_), Signed3(_)) => Signed3(None),
        (Signed4(_), Signed4(_)) => Signed4(None),
        (Unsigned1(_), Unsigned1(_)) => Unsigned1(None),
        (Unsigned2(_), Unsigned2(_)) => Unsigned2(None),
        (Unsigned3(_), Unsigned3(_)) => Unsigned3(None),
        (Unsigned4(_), Unsigned4(_)) => Unsigned4(None),
        (Mat2(_), Mat2(_)) => Mat2(None),
        (Mat3(_), Mat3(_)) => Mat3(None),
        (Mat4(_), Mat4(_)) => Mat4(None),

        _ => {
            return Err(CompileError::UnrelatedType(
                TypeValue::Primitive(this.clone()),
                TypeValue::Primitive(other.clone()),
            ));
        }
    })
}

impl<'a> Typeable<'a> for Primitive {
    #[rustfmt::skip]
    fn intersect_with(&self, other: &Self) -> Result<Self, CompileError<'a>> {
        use Primitive::*;
        // macros can't expand to match arms
        match (self, other) {
            // ambiguous literals
            (AmbiguousInteger(_), Float1(_)) => Err(CompileError::IntegerAsFloat(
                TypeValue::Primitive(self.clone()),
                TypeValue::Primitive(other.clone()),
            )),
            (AmbiguousInteger(a), Signed1(b)) => {
                match (i32::try_from(*a), *b) {
                    (Err(_), _) => Err(CompileError::IntegerBounds(*a as _, TypeValue::Primitive(other.clone()))),
                    (Ok(a), Some(b)) if a != b => {
                        Err(CompileError::ConcreteInequality(
                            TypeValue::Primitive(self.clone()),
                            TypeValue::Primitive(other.clone()),
                        ))
                    }
                    (Ok(a), _) => Ok(Self::Signed1(Some(a)))
                }
            },
            (AmbiguousInteger(a), Unsigned1(b)) => {
                match (*a, *b) {
                    (a, Some(b)) if a != b => {
                        Err(CompileError::ConcreteInequality(
                            TypeValue::Primitive(self.clone()),
                            TypeValue::Primitive(other.clone()),
                        ))
                    }
                    (a, _) => Ok(Self::Unsigned1(Some(a)))
                }
            },

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
            (Bool(Some(a)), Bool(Some(b))) => if a == b { Ok(Bool(Some(*a))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Float1(Some(a)), Float1(Some(b))) => if a == b { Ok(Float1(Some(*a))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Float2(Some(a)), Float2(Some(b))) => if a == b { Ok(Float2(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Float3(Some(a)), Float3(Some(b))) => if a == b { Ok(Float3(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Float4(Some(a)), Float4(Some(b))) => if a == b { Ok(Float4(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Signed1(Some(a)), Signed1(Some(b))) => if a == b { Ok(Signed1(Some(*a))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Signed2(Some(a)), Signed2(Some(b))) => if a == b { Ok(Signed2(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Signed3(Some(a)), Signed3(Some(b))) => if a == b { Ok(Signed3(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Signed4(Some(a)), Signed4(Some(b))) => if a == b { Ok(Signed4(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Unsigned1(Some(a)), Unsigned1(Some(b))) => if a == b { Ok(Unsigned1(Some(*a))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Unsigned2(Some(a)), Unsigned2(Some(b))) => if a == b { Ok(Unsigned2(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Unsigned3(Some(a)), Unsigned3(Some(b))) => if a == b { Ok(Unsigned3(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Unsigned4(Some(a)), Unsigned4(Some(b))) => if a == b { Ok(Unsigned4(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Mat2(Some(a)), Mat2(Some(b))) => if a == b { Ok(Mat2(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Mat3(Some(a)), Mat3(Some(b))) => if a == b { Ok(Mat3(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },
            (Mat4(Some(a)), Mat4(Some(b))) => if a == b { Ok(Mat4(Some(a.clone()))) } else { Err(CompileError::ConcreteInequality(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))) },

            // value -> type
            (Bool(val), Bool(None)) => Ok(Bool(*val)),
            (Float1(val), Float1(None)) => Ok(Float1(*val)),
            (Float2(val), Float2(None)) => Ok(Float2(val.clone())),
            (Float3(val), Float3(None)) => Ok(Float3(val.clone())),
            (Float4(val), Float4(None)) => Ok(Float4(val.clone())),
            (Signed1(val), Signed1(None)) => Ok(Signed1(*val)),
            (Signed2(val), Signed2(None)) => Ok(Signed2(val.clone())),
            (Signed3(val), Signed3(None)) => Ok(Signed3(val.clone())),
            (Signed4(val), Signed4(None)) => Ok(Signed4(val.clone())),
            (Unsigned1(val), Unsigned1(None)) => Ok(Unsigned1(*val)),
            (Unsigned2(val), Unsigned2(None)) => Ok(Unsigned2(val.clone())),
            (Unsigned3(val), Unsigned3(None)) => Ok(Unsigned3(val.clone())),
            (Unsigned4(val), Unsigned4(None)) => Ok(Unsigned4(val.clone())),
            (Mat2(val), Mat2(None)) => Ok(Mat2(val.clone())),
            (Mat3(val), Mat3(None)) => Ok(Mat3(val.clone())),
            (Mat4(val), Mat4(None)) => Ok(Mat4(val.clone())),

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
            | (Mat4(None), Mat4(Some(_))) => Err(CompileError::UnknownSubtype(TypeValue::Primitive(self.clone()), TypeValue::Primitive(other.clone()))),

            _ => Err(CompileError::UnrelatedType(
                TypeValue::Primitive(self.clone()),
                TypeValue::Primitive(other.clone())
            )),
        }
    }

    fn union(this: &Self, other: &Self) -> Result<Self, CompileError<'a>> {
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

    fn try_resolve(&self) -> Result<Self, CompileError<'a>> {
        Ok(self.clone())
    }
}
