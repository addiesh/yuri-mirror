use std::fmt::Debug;

use yuri_ast::Qpath;

use crate::types::TyVal;

#[derive(Clone, Debug)]
pub struct ResolutionError(pub Qpath);

// TODO: improve error reporting
#[derive(Debug)]
pub enum TypeError {
    Multiple(Vec<TypeError>),
    /// We can't check whether the two values can be equal because we don't know what one of them actually is.
    Unresolved(ResolutionError),
    /// The typeval is valid at a Rust type level, but not at a Yuri behavior level.
    /// This includes arrays with values that don't match their annotations
    SemanticallyInvalid(TyVal),
    /// The two values are 100% known but are not equal themselves.
    ConcreteInequality(TyVal, TyVal),
    /// A is a subtype of B but not vice-versa.
    /// You cannot cast from an unknown value to a concrete one.
    UnknownSubtype(TyVal, TyVal),
    /// A conversion that doesn't make sense, like Primitive to Compound.
    /// Types A and B are categorically separate; no type-level conversion exists from A to B
    UnrelatedType(TyVal, TyVal),
    IncompatibleArrayLength(Option<u32>, u32),
    /// Two compound types with two different counts of fields cannot be cast to one another ({l1} != {l2})
    IncompatibleFieldCount(u32, u32),
    /// The provided integer (originating from a literal) is out of bounds for the target type.
    IntegerBounds(i64, TyVal),
    IntegerAsFloat(TyVal, TyVal),
}

impl Clone for TypeError {
    fn clone(&self) -> Self {
        match self {
            Self::Multiple(arg0) => Self::Multiple(arg0.clone()),

            Self::Unresolved(arg0) => Self::Unresolved(arg0.clone()),
            Self::SemanticallyInvalid(arg0) => Self::SemanticallyInvalid(arg0.clone()),
            Self::ConcreteInequality(arg0, arg1) => {
                Self::ConcreteInequality(arg0.clone(), arg1.clone())
            }
            Self::UnknownSubtype(arg0, arg1) => Self::UnknownSubtype(arg0.clone(), arg1.clone()),
            Self::UnrelatedType(arg0, arg1) => Self::UnrelatedType(arg0.clone(), arg1.clone()),
            Self::IncompatibleArrayLength(arg0, arg1) => {
                Self::IncompatibleArrayLength(*arg0, *arg1)
            }
            Self::IncompatibleFieldCount(arg0, arg1) => Self::IncompatibleFieldCount(*arg0, *arg1),
            Self::IntegerBounds(arg0, arg1) => Self::IntegerBounds(*arg0, arg1.clone()),
            Self::IntegerAsFloat(arg0, arg1) => Self::IntegerAsFloat(arg0.clone(), arg1.clone()),
        }
    }
}

impl From<ResolutionError> for TypeError {
    fn from(value: ResolutionError) -> Self {
        Self::Unresolved(value)
    }
}
