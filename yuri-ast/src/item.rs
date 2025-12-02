use crate::expression::{BlockExpr, Expression};
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
    pub return_type: WrittenTy,
    pub body: BlockExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OuterDeclaration {
    Submodule(Box<ModuleItem>),
    GlobalVariable(Box<VariableItem>),
    Function(Box<FunctionItem>),
    TypeAlias(Box<TypeAliasItem>),
    Import(Ident),
}

macro_rules! outer_from_helper {
    ($from:ty, $variant:ident) => {
        impl From<$from> for OuterDeclaration {
            fn from(value: $from) -> Self {
                OuterDeclaration::$variant(Box::new(value))
            }
        }

        impl From<Box<$from>> for OuterDeclaration {
            fn from(value: Box<$from>) -> Self {
                OuterDeclaration::$variant(value)
            }
        }
    };
}

outer_from_helper!(ModuleItem, Submodule);
outer_from_helper!(VariableItem, GlobalVariable);
outer_from_helper!(FunctionItem, Function);
outer_from_helper!(TypeAliasItem, TypeAlias);

/// Represents a series of submodules, functions, global variables, and type aliases/definitions.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleItem {
    pub name: Ident,
    pub contents: Vec<OuterDeclaration>,
}
