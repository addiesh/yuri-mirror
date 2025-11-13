use crate::expression::{BlockExpression, Expression};
use crate::types::Type;
use crate::{Ident, Qpath};

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Qpath,
    pub params: Option<Vec<()>>,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeAliasItem {
    pub export: bool,
    pub name: Ident,
    pub aliases: Type,
}

#[derive(Debug, Clone)]
pub struct VariableItem {
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub explicit_type: Option<Type>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ParameterItem {
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    pub explicit_type: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
    pub attributes: Vec<Attribute>,
    pub export: bool,
    pub name: Ident,
    pub parameters: Vec<ParameterItem>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: Type,
    pub body: BlockExpression,
}

pub enum OuterDeclaration {
    GlobalVariable(VariableItem),
    Function(FunctionItem),
    Alias(TypeAliasItem),
    Import(Ident),
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct ModuleItem {
    pub name: Ident,
    pub contents: Vec<OuterDeclaration>,
}
