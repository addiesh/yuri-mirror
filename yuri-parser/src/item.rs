use crate::expression::{BlockExpression, Expression};
use crate::types::WrittenTy;
use crate::{Ident, Qpath};

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub path: Qpath,
    pub params: Option<Vec<()>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasItem {
    pub export: bool,
    pub name: Ident,
    pub aliases: WrittenTy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableItem {
    pub name: Ident,
    /// The target type of this variable, as specified by the programmer.
    pub written_ty: Option<WrittenTy>,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterItem {
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    pub explicit_type: WrittenTy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionItem {
    pub attributes: Vec<Attribute>,
    pub export: bool,
    pub name: Ident,
    pub parameters: Vec<ParameterItem>,
    // of note, the *actual* return type is derived from the function body.
    pub return_type: WrittenTy,
    pub body: BlockExpression,
}

pub enum OuterDeclaration {
    Submodule(Box<ModuleItem>),
    GlobalVariable(Box<VariableItem>),
    Function(Box<FunctionItem>),
    Alias(Box<TypeAliasItem>),
    Import(Ident),
}

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
pub struct ModuleItem {
    pub name: Ident,
    pub contents: Vec<OuterDeclaration>,
}
