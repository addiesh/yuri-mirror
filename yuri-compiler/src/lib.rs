use std::error::Error;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::sync::{Arc, Weak};

use yuri_parser::error::ParseError;

use crate::item::{FunctionItem, TypeAliasItem, VariableItem};
use crate::types::TypeValue;

mod columbo;
mod expression;
mod item;
#[cfg(test)]
mod test;
mod types;

// TODO: improve error reporting
#[derive(Debug)]
pub enum CompileError<'a> {
    Multiple(Vec<CompileError<'a>>),
    Parse(ParseError),
    NotOurFault(Box<dyn Error + Send + Sync>),
    /// We can't check whether the two values can be equal because we don't know what one of them actually is.
    Unresolved(ItemPath<'a>),
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
            Self::Unresolved(arg0) => Self::Unresolved(arg0.clone()),
            Self::SemanticallyInvalid(arg0) => Self::SemanticallyInvalid(arg0.clone()),
            Self::ConcreteInequality(arg0, arg1) => {
                Self::ConcreteInequality(arg0.clone(), arg1.clone())
            }
            Self::UnknownSubtype(arg0, arg1) => Self::UnknownSubtype(arg0.clone(), arg1.clone()),
            Self::UnrelatedType(arg0, arg1) => Self::UnrelatedType(arg0.clone(), arg1.clone()),
            Self::IncompatibleArrayLength(arg0, arg1) => {
                Self::IncompatibleArrayLength(arg0.clone(), arg1.clone())
            }
            Self::IncompatibleFieldCount(arg0, arg1) => {
                Self::IncompatibleFieldCount(arg0.clone(), arg1.clone())
            }
            Self::IntegerBounds(arg0, arg1) => Self::IntegerBounds(arg0.clone(), arg1.clone()),
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

/// Provides a "compiler plugin" that works on attribute semantics.
pub trait AttributeProvider<'a>: 'a + Send + Sync + Unpin {
    // TODO: implement methods
    fn process_hl(&self) -> ();
    fn process_ll(&self) -> ();
}

/// A "rule" in yuri is a guarantee that some condition will always
/// be true wherever the rule is in place. This allows for, among other things,
/// more advanced optimization. As such, this is a complicated idea to implement,
/// so we *probably* won't be implementing it for the V1 Yuri compiler.
pub struct ScopeRule {/* empty for now */}

#[derive(Default)]
pub struct Scope<'a> {
    /// - For root modules, this is None.
    /// - For function blocks, this is the block/module scope in which they are defined.
    /// - For expression blocks (including if/else/loop blocks), this is either the function scope
    ///   or the parent block scope (recursive) in which it is contained.
    pub parent: Option<Weak<Scope<'a>>>,
    pub variables: Vec<Arc<VariableItem<'a>>>,
    pub parameters: Vec<Arc<VariableItem<'a>>>,
    pub functions: Vec<Arc<FunctionItem<'a>>>,
    pub imports: Vec<Arc<ItemPath<'a>>>,
    pub type_aliases: Vec<Arc<TypeAliasItem<'a>>>,
    /// Currently unused. In the future, this may be used to create assertions on values and types
    /// based on checked predicates.
    pub _rules: Vec<ScopeRule>,
}

#[derive(Clone)]
pub enum Resolution<'a, T: 'a> {
    Unresolved(ItemPath<'a>),
    Resolved { item_path: ItemPath<'a>, item: T },
}

impl<'a, T> Resolution<'a, T> {
    pub fn try_inline(&self) -> Result<T, CompileError<'a>>
    where
        T: Clone,
    {
        match self {
            Resolution::Unresolved(item_path) => Err(CompileError::Unresolved(item_path.clone())),
            Resolution::Resolved { item, .. } => Ok(item.clone()),
        }
    }
}

impl<'a, T> Resolution<'a, Weak<T>> {
    pub fn try_upgrade(&self) -> Result<Arc<T>, CompileError<'a>> {
        match self {
            Resolution::Unresolved(item_path) => Err(CompileError::Unresolved(item_path.clone())),
            Resolution::Resolved { item_path, item } => {
                if let Some(arc) = item.upgrade() {
                    Ok(arc)
                } else {
                    Err(CompileError::Unresolved(item_path.clone()))
                }
            }
        }
    }
}

impl<'a, T> PartialEq for Resolution<'a, T> {
    // TODO: i don't like this
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Resolution::Resolved { item_path: l0, .. },
                Resolution::Resolved { item_path: r0, .. },
            ) => l0 == r0,
            _ => false,
        }
    }
}

// impl<'a, T: PartialEq> PartialEq for Resolution<'a, T> {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Resolution::Resolved { item: l0, .. }, Resolution::Resolved { item: r0, .. }) => {
//                 l0 == r0
//             }
//             _ => false,
//         }
//     }
// }

impl<'a, T> std::fmt::Debug for Resolution<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved(identifier) => f.debug_tuple("Unresolved").field(identifier).finish(),
            Self::Resolved {
                item_path: identifier,
                item: _item,
            } => f.debug_tuple("Resolved").field(identifier).finish(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ItemPath<'a> {
    pub path: String,
    _pd: PhantomData<&'a ()>,
}

#[derive(Clone, Debug)]
pub struct Attribute<'a> {
    // attributes will be weird.
    pub name: Resolution<'a, &'a dyn AttributeProvider<'a>>,
}

impl<'a> PartialEq for Attribute<'a> {
    fn eq(&self, other: &Self) -> bool {
        // TODO: I don't like this.
        let a = &self.name;
        let b = &other.name;
        match (a, b) {
            (Resolution::Unresolved(l0), Resolution::Unresolved(r0))
            | (Resolution::Unresolved(l0), Resolution::Resolved { item_path: r0, .. })
            | (Resolution::Resolved { item_path: l0, .. }, Resolution::Unresolved(r0))
            | (
                Resolution::Resolved { item_path: l0, .. },
                Resolution::Resolved { item_path: r0, .. },
            ) => l0 == r0,
        }
    }
}

#[inline]
fn split_results<T, E>(iterator: impl Iterator<Item = Result<T, E>>) -> (Vec<T>, Vec<E>) {
    let (elements, errors): (Vec<_>, Vec<_>) = iterator.partition(Result::is_err);

    (
        elements
            .into_iter()
            .filter_map(Result::ok)
            .collect::<Vec<_>>(),
        errors
            .into_iter()
            .filter_map(Result::err)
            .collect::<Vec<_>>(),
    )
}
