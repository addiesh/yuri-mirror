use yuri_ast::Ident;

use crate::attribute::Attribute;
use crate::expression::{BlockExpression, Expression};
use crate::scope::Scope;
use crate::types::TyVal;
use crate::{Yrc, Ywk};

#[derive(Debug)]
pub struct TypeAliasItem {
    pub parent_scope: Ywk<Scope>,
    pub name: Ident,
    pub aliases: TyVal,
}

/// Despite the name, all variables are immutable.
#[derive(Clone, Debug)]
pub struct VariableItem {
    pub parent_scope: Ywk<Scope>,
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: Option<TyVal>,
    pub value: Expression,
}

#[derive(Clone, Debug)]
pub struct ParameterItem {
    pub parent_function: Ywk<FunctionItem>,
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: TyVal,
}

#[derive(Clone, Debug)]
pub struct FunctionItem {
    pub parent_scope: Ywk<Scope>,
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    pub parameters: Vec<Yrc<ParameterItem>>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: TyVal,
    pub body: BlockExpression,
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
#[derive(Clone, Debug)]
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
