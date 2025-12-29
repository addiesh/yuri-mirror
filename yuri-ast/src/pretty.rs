use std::fmt::Debug;

use yuri_common::ScalarTy;

use crate::expression::{
    ArrayExpr, BinaryExpr, BlockExpr, CallExpr, CompoundExpr, Expression, FieldExpr, IfExpr,
    LiteralExpr, MatchExpr, UnaryExpr,
};
use crate::item::{
    Attribute, FunctionItem, ModuleItem, OuterDeclaration, TypeAliasItem, VariableItem,
};
use crate::types::{ArrayTy, CompoundTy, CompoundTyField, MatrixTy, VectorTy, WrittenTy};
use crate::{Ident, InStorage};

pub trait AstPretty: Debug {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        let _ = (storage, depth);
        format!("debug {self:?}")
    }
}

impl AstPretty for Expression {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        match self {
            Expression::Access(ident) => ident.pretty_print_ast(storage, depth),
            Expression::Field(field_expr) => field_expr.pretty_print_ast(storage, depth),
            Expression::Literal(literal_expr) => literal_expr.pretty_print_ast(storage, depth),
            Expression::Unary(unary_expr) => unary_expr.pretty_print_ast(storage, depth),
            Expression::Binary(binary_expr) => binary_expr.pretty_print_ast(storage, depth),
            Expression::Array(array_expr) => array_expr.pretty_print_ast(storage, depth),
            Expression::IfExpr(if_expr) => if_expr.pretty_print_ast(storage, depth),
            Expression::MatchExpr(match_expr) => match_expr.pretty_print_ast(storage, depth),
            Expression::FunctionalCall(call_expr) => call_expr.pretty_print_ast(storage, depth),
            Expression::Compound(compound_expr) => compound_expr.pretty_print_ast(storage, depth),
            Expression::Block(block_expr) => block_expr.pretty_print_ast(storage, depth),
            Expression::Paren(expression) => {
                format!("Paren ( {} )", expression.pretty_print_ast(storage, depth))
            }
            Expression::Unimplemented { file, line, column } => {
                format!("!! Unimplemented {file}:{line}:{column} !!")
            }
        }
    }
}

impl AstPretty for Ident {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        let ident = storage.resolve(self).expect("ident needs valid ID");
        format!("\"{ident}\"")
    }
}
impl AstPretty for FieldExpr {}
impl AstPretty for LiteralExpr {}
impl AstPretty for UnaryExpr {}
impl AstPretty for BinaryExpr {}
impl AstPretty for ArrayExpr {}
impl AstPretty for IfExpr {}
impl AstPretty for MatchExpr {}
impl AstPretty for CallExpr {}
impl AstPretty for CompoundExpr {}
impl AstPretty for BlockExpr {}

impl AstPretty for WrittenTy {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        match self {
            WrittenTy::Alias(qpath) => qpath.pretty_print(storage),
            WrittenTy::Bool => "bool".to_owned(),
            WrittenTy::Scalar(scalar_ty) => scalar_ty.pretty_print_ast(storage, depth),
            WrittenTy::Vector(vector_ty) => vector_ty.pretty_print_ast(storage, depth),
            WrittenTy::Matrix(matrix_ty) => matrix_ty.pretty_print_ast(storage, depth),
            WrittenTy::Array(array_ty) => array_ty.pretty_print_ast(storage, depth),
            WrittenTy::Compound(compound_ty) => compound_ty.pretty_print_ast(storage, depth),
            WrittenTy::Enum => todo!(),
        }
    }
}
impl AstPretty for ScalarTy {}
impl AstPretty for VectorTy {}
impl AstPretty for MatrixTy {}
impl AstPretty for ArrayTy {}
impl AstPretty for CompoundTy {
    fn pretty_print_ast(&self, _storage: &InStorage, depth: usize) -> String {
        let output = self
            .fields
            .iter()
            .map(|field| {
                let tabs = "\t".repeat(depth);
                format!("{tabs}{field:?},")
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!("{{{{\n{output}\n}}}}")
    }
}
// impl AstPretty for CompoundTyField {}

impl AstPretty for Attribute {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        let qp = self.path.pretty_print(storage);
        if let Some(args) = &self.args {
            let args = args
                .iter()
                .map(|expr| expr.pretty_print_ast(storage, depth + 1))
                .collect::<Vec<_>>()
                .join(", ");

            format!("@{qp}({args})")
        } else {
            format!("@{qp}")
        }
    }
}

impl AstPretty for OuterDeclaration {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        match self {
            OuterDeclaration::Submodule(module_item) => {
                module_item.pretty_print_ast(storage, depth)
            }
            OuterDeclaration::GlobalVariable(variable_item) => {
                variable_item.pretty_print_ast(storage, depth)
            }
            OuterDeclaration::Function(function_item) => {
                function_item.pretty_print_ast(storage, depth)
            }
            OuterDeclaration::TypeAlias(type_alias_item) => {
                type_alias_item.pretty_print_ast(storage, depth)
            }
            OuterDeclaration::Import(qpath) => {
                let parts = qpath.pretty_print(storage);
                format!("import: {parts:?}")
            }
        }
    }
}

impl AstPretty for ModuleItem {}
impl AstPretty for VariableItem {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        let name = storage.resolve(&self.name).expect("ident needs valid ID");
        let wty = if let Some(wty) = &self.written_ty {
            wty.pretty_print_ast(storage, depth + 1)
        } else {
            "[infer]".to_string()
        };
        let value = self.value.pretty_print_ast(storage, depth + 1);
        format!("variable: let {name}: {wty} = {value}")
    }
}
impl AstPretty for FunctionItem {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        let export = self.export;
        let attribs = self
            .attributes
            .iter()
            .map(|a| a.pretty_print_ast(storage, depth))
            .collect::<Vec<_>>()
            .join(" ");
        let name = storage.resolve(&self.name).expect("ident needs valid ID");
        let params = self
            .parameters
            .iter()
            .map(|p| {
                format!(
                    "{}: {:?}",
                    storage.resolve(&p.name).expect("ident needs valid ID"),
                    p.explicit_type
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        let body = self.body.pretty_print_ast(storage, depth + 1);
        let return_ty = self.return_type.pretty_print_ast(storage, depth + 1);
        format!("export={export} {attribs} fn {name} ({params}): {return_ty} {{\n{body}\n}}")
    }
}
impl AstPretty for TypeAliasItem {
    fn pretty_print_ast(&self, storage: &InStorage, depth: usize) -> String {
        let export = self.export;
        let attribs = &self.type_attributes;
        let name = storage.resolve(&self.name).expect("ident needs valid ID");
        let wty = self.aliases.pretty_print_ast(storage, depth + 1);
        format!("export={export} {attribs:?} type {name} = {wty}")
    }
}
