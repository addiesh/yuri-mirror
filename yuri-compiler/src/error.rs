use std::error::Error;
use std::fmt::{Debug, Display};

use yuri_parser::Qpath;
use yuri_parser::error::ParseError;

use crate::types::TypeValue;

#[derive(Clone, Debug)]
pub struct ResolutionError(pub Qpath);

// TODO: improve error reporting
#[derive(Debug)]
pub enum TypeError<'a> {
    Multiple(Vec<TypeError<'a>>),
    /// We can't check whether the two values can be equal because we don't know what one of them actually is.
    Unresolved(ResolutionError),
    /// The typeval is valid at a Rust type level, but not at a Yuri behavior level.
    /// This includes arrays with values that don't match their annotations
    SemanticallyInvalid(TypeValue<'a>),
    /// The two values are 100% known but are not equal themselves.
    ConcreteInequality(TypeValue<'a>, TypeValue<'a>),
    /// A is a subtype of B but not vice-versa.
    /// You cannot cast from an unknown value to a concrete one.
    UnknownSubtype(TypeValue<'a>, TypeValue<'a>),
    /// A conversion that doesn't make sense, like Primitive to Compound.
    /// Types A and B are categorically separate; no type-level conversion exists from A to B
    UnrelatedType(TypeValue<'a>, TypeValue<'a>),
    IncompatibleArrayLength(Option<u32>, u32),
    /// Two compound types with two different counts of fields cannot be cast to one another ({l1} != {l2})
    IncompatibleFieldCount(u32, u32),
    /// The provided integer (originating from a literal) is out of bounds for the target type.
    IntegerBounds(i64, TypeValue<'a>),
    IntegerAsFloat(TypeValue<'a>, TypeValue<'a>),
}

impl<'a> From<ResolutionError> for TypeError<'a> {
    fn from(value: ResolutionError) -> Self {
        Self::Unresolved(value)
    }
}
impl<'a> From<ResolutionError> for CompileError<'a> {
    fn from(value: ResolutionError) -> Self {
        Self::Unresolved(value)
    }
}

// TODO: improve error reporting
#[derive(Debug)]
pub enum CompileError<'a> {
    Multiple(Vec<CompileError<'a>>),
    Parse(ParseError),
    NotOurFault(Box<dyn Error + Send + Sync>),
    Unresolved(ResolutionError),
    TypeCheck(Box<TypeError<'a>>),
}

impl<'a> From<ParseError> for CompileError<'a> {
    fn from(value: ParseError) -> Self {
        CompileError::Parse(value)
    }
}

impl<'a> Clone for CompileError<'a> {
    fn clone(&self) -> Self {
        match self {
            Self::Multiple(arg0) => Self::Multiple(arg0.clone()),
            Self::Parse(arg0) => Self::Parse(arg0.clone()),
            Self::NotOurFault(arg0) => {
                Self::NotOurFault(format!("(CLONED MESSAGE): {arg0:?}").into())
            }
            Self::TypeCheck(arg0) => Self::TypeCheck(arg0.clone()),
            Self::Unresolved(arg0) => Self::Unresolved(arg0.clone()),
        }
    }
}

impl<'a> Clone for TypeError<'a> {
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

impl<'a> Display for CompileError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // just use debug
        Debug::fmt(&self, f)
    }
}

impl<'a> Error for CompileError<'a> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
