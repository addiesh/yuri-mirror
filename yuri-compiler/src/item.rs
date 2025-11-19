use yuri_parser::Ident;

use crate::attribute::Attribute;
use crate::expression::{BlockExpression, Expression};
use crate::scope::Scope;
use crate::types::TypeValue;
use crate::{Yrc, Ywk};

#[derive(Debug)]
pub struct TypeAliasItem<'src> {
    pub parent_scope: Ywk<Scope<'src>>,
    pub name: Ident,
    pub aliases: TypeValue<'src>,
}

/// Despite the name, all variables are immutable.
pub struct VariableItem<'src> {
    pub parent_scope: Ywk<Scope<'src>>,
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: Option<TypeValue<'src>>,
    pub value: Expression<'src>,
}

pub struct ParameterItem<'src> {
    pub parent_function: Ywk<FunctionItem<'src>>,
    pub attributes: Vec<Attribute<'src>>,
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: TypeValue<'src>,
}

pub struct FunctionItem<'src> {
    pub parent_scope: Ywk<Scope<'src>>,
    pub attributes: Vec<Attribute<'src>>,
    pub name: Ident,
    pub parameters: Vec<Yrc<ParameterItem<'src>>>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: TypeValue<'src>,
    pub body: BlockExpression<'src>,
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct Module<'a> {
    pub parent: Option<Ywk<Module<'a>>>,
    pub name: Ident,
    pub submodules: Vec<Yrc<Module<'a>>>,
    pub scope: Yrc<Scope<'a>>,
}

impl<'a> Module<'a> {
    pub fn empty(name: Ident, parent: Option<Ywk<Module<'a>>>) -> Self {
        Self {
            parent,
            name,
            submodules: Default::default(),
            scope: Default::default(),
        }
    }
}
