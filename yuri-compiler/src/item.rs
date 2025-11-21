use yuri_parser::Ident;

use crate::attribute::Attribute;
use crate::expression::{BlockExpression, Expression};
use crate::scope::Scope;
use crate::types::TypeValue;
use crate::{Yrc, Ywk};

#[derive(Debug)]
pub struct TypeAliasItem {
    pub parent_scope: Ywk<Scope>,
    pub name: Ident,
    pub aliases: TypeValue,
}

/// Despite the name, all variables are immutable.
pub struct VariableItem {
    pub parent_scope: Ywk<Scope>,
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: Option<TypeValue>,
    pub value: Expression,
}

pub struct ParameterItem {
    pub parent_function: Ywk<FunctionItem>,
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: TypeValue,
}

pub struct FunctionItem {
    pub parent_scope: Ywk<Scope>,
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    pub parameters: Vec<Yrc<ParameterItem>>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: TypeValue,
    pub body: BlockExpression,
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct Module {
    pub parent: Option<Ywk<Module>>,
    pub name: Ident,
    pub submodules: Vec<Yrc<Module>>,
    pub scope: Yrc<Scope>,
}

impl Module {
    pub fn empty(name: Ident, parent: Option<Ywk<Module>>) -> Self {
        Self {
            parent,
            name,
            submodules: Default::default(),
            scope: Default::default(),
        }
    }
}
