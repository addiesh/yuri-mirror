use std::borrow::Cow;
use std::sync::{Arc, Weak};

use crate::expression::{BlockExpression, Expression};
use crate::types::TypeValue;
use crate::{Attribute, Scope};

#[derive(Debug, PartialEq)]
pub struct TypeAliasItem<'a> {
    pub name: Cow<'a, str>,
    pub aliases: TypeValue<'a>,
}

/// Despite the name, all variables are immutable.
pub struct VariableItem<'a> {
    pub name: Cow<'a, str>,
    pub parent_scope: Weak<Scope<'a>>,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: Option<TypeValue<'a>>,
    pub value: Expression<'a>,
}

pub struct ParameterItem<'a> {
    pub attributes: Vec<Attribute<'a>>,
    pub name: Cow<'a, str>,
    pub function: Weak<FunctionItem<'a>>,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: TypeValue<'a>,
}

pub struct FunctionItem<'a> {
    pub attributes: Vec<Attribute<'a>>,
    pub name: Cow<'a, str>,
    pub parameters: Vec<Arc<ParameterItem<'a>>>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: TypeValue<'a>,
    pub body: BlockExpression<'a>,
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct Module<'a> {
    pub parent: Option<Weak<Module<'a>>>,
    pub name: Cow<'a, str>,
    pub submodules: Vec<Arc<Module<'a>>>,
    pub scope: Arc<Scope<'a>>,
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
