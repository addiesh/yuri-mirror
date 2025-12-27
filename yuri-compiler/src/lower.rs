use std::sync::MutexGuard;

use yuri_ast::{Ast, Ident, InStorage, Qpath, expression_unimplemented};
use yuri_hir::attribute::Attribute;
use yuri_hir::expression::{
    ArrayExpression, BinaryExpression, BlockExpression, Expression, LiteralExpr, UnaryExpression,
};
use yuri_hir::item::{FunctionItem, Module, ParameterItem, TypeAliasItem, VariableItem};
use yuri_hir::resolution::Resolution;
use yuri_hir::scope::{Scope, ScopeItem};
use yuri_hir::types::TyVal;
use yuri_hir::types::compound::{CompoundTyVal, CompoundTypeField};
use yuri_hir::types::primitive::PrimitiveTyVal;
use yuri_hir::{Yrc, Ywk};

use crate::error::CompileError;

/// Trait for cheap conversions between syntax tree and compiler node.
/// The node may be incomplete, and may have to be resolved/lowered further.
pub trait ParseLower<Output> {
    fn lower(&self) -> Output;
}

impl ParseLower<BlockExpression> for yuri_ast::expression::BlockExpr {
    fn lower(&self) -> BlockExpression {
        let scope = Yrc::new(Scope::new().into());

        BlockExpression {
            scope,
            statements: self
                .statements
                .iter()
                .map(|stmt| {
                    use yuri_ast::expression::BlockStatement;
                    match stmt {
                        BlockStatement::LocalVariable(variable_item) => todo!(),
                        BlockStatement::TypeAlias(type_alias_item) => todo!(),
                        BlockStatement::Function(function_item) => todo!(),
                        BlockStatement::Return(expression) => todo!(),
                        BlockStatement::Import(ident) => todo!(),
                        BlockStatement::Expression(expression) => todo!(),
                        BlockStatement::Assign(qpath, expression) => todo!(),
                    }
                })
                .collect(),
        }
    }
}

impl ParseLower<Expression> for yuri_ast::expression::Expression {
    fn lower(&self) -> Expression {
        use yuri_ast::expression::ArrayExpr as Arr;
        use yuri_ast::expression::Expression as Exp;
        use yuri_ast::expression::LiteralExpr as Lit;
        match self {
            Exp::Field(path_expr) => todo!(),
            Exp::Access(qpath) => {
                Expression::Variable(todo!("Resolution::Unresolved(qpath.clone())"))
            }
            Exp::Literal(lit) => Expression::Literal(match lit {
                Lit::Bool(b) => LiteralExpr::Bool(*b),
                Lit::Decimal(d) => LiteralExpr::Decimal(*d),
                Lit::Integer(i) => LiteralExpr::Integer(*i),
                Lit::BinInt(i) => LiteralExpr::BinInt(*i),
                Lit::HexInt(i) => LiteralExpr::HexInt(*i),
                Lit::Pi => todo!(),
                Lit::Tau => todo!(),
                Lit::Inf => todo!(),
                Lit::Nan => todo!(),
            }),
            Exp::Unary(unary) => Expression::Unary(UnaryExpression {
                operator: unary.operator,
                value: unary.value.lower().into(),
            }),
            Exp::Binary(binary) => Expression::Binary(BinaryExpression {
                operator: binary.operator,
                lhs: binary.lhs.lower().into(),
                rhs: binary.rhs.lower().into(),
            }),
            Exp::Array(Arr::Elements(elements)) => Expression::Array(ArrayExpression::Elements(
                elements.iter().map(ParseLower::lower).collect(),
            )),
            Exp::Array(Arr::Spread { element, length }) => {
                Expression::Array(ArrayExpression::Spread {
                    element: element.lower().into(),
                    length: length.lower().into(),
                })
            }
            Exp::IfExpr(riffx) => todo!(),
            Exp::FunctionalCall(call) => todo!(),
            Exp::Compound(compound) => todo!(),
            Exp::Block(expr) => Expression::Block(expr.lower()),
            Exp::Paren(expression) => todo!(),
            #[cfg(debug_assertions)]
            Exp::Unimplemented { file, line, column } => Expression::Unimplemented {
                file,
                line: *line,
                column: *column,
            },
            Exp::MatchExpr(match_expression) => todo!(),
        }
    }
}

impl ParseLower<TyVal> for yuri_ast::types::WrittenTy {
    fn lower(&self) -> TyVal {
        use yuri_ast::types::WrittenTy;
        match self {
            WrittenTy::Bool => TyVal::Primitive(PrimitiveTyVal::Bool(None)),
            WrittenTy::Scalar(scalar_ty) => todo!(),
            WrittenTy::Vector(vector_ty) => todo!(),
            WrittenTy::Matrix(matrix_ty) => todo!(),
            WrittenTy::Array(array_ty) => todo!(),
            WrittenTy::Compound(compound_ty) => TyVal::Compound(Box::new(CompoundTyVal {
                fields: compound_ty
                    .fields
                    .iter()
                    .map(|field| CompoundTypeField {
                        name: field.name,
                        attributes: field
                            .attributes
                            .iter()
                            .map(|attrib| Attribute {
                                path: attrib.path.clone(),
                                args: Default::default(),
                            })
                            .collect(),
                        field_type: field.field_ty.lower(),
                    })
                    .collect(),
            })),
            WrittenTy::Alias(qpath) => TyVal::Alias(Resolution::Unresolved(qpath.clone())),
            WrittenTy::Enum => todo!("lower enum type"),
        }
    }
}

struct Lowerer<'src, 'storage, 'at> {
    storage: &'storage mut InStorage,
    source: &'src str,
    ast: &'at Ast,
    modules: Vec<Yrc<Module>>,
    scopes: Vec<Yrc<Scope>>,
}

pub fn lower<'src>(
    source: &'src str,
    storage: &mut InStorage,
    ast: &Ast,
    root_module_name: Ident,
) -> Result<Yrc<Module>, CompileError<'src>> {
    // walk the tree!

    let mut lowerer = Lowerer {
        storage,
        source,
        ast,
        scopes: Vec::with_capacity(8),
        modules: Vec::with_capacity(2),
    };
    lowerer.scopes.push(Yrc::new(Scope::new().into()));

    lowerer.module(root_module_name)
}

impl<'src, 'storage, 'at> Lowerer<'src, 'storage, 'at> {
    pub fn scope(&mut self) -> Option<Yrc<Scope>> {
        self.scopes.last().map(Yrc::clone)
    }

    pub fn weak_scope(&mut self) -> Option<Ywk<Scope>> {
        self.scopes.last().map(Yrc::downgrade)
    }

    pub fn focus(&mut self) -> Option<MutexGuard<'_, Scope>> {
        Some(self.scopes.last()?.lock().unwrap())
    }

    // pub fn strong_focus(&self) -> Yrc<Scope<'src>> {
    //     self.scope.last().unwrap().clone()
    // }

    pub fn weak_focus(&self) -> Ywk<Scope> {
        Yrc::downgrade(self.scopes.last().unwrap())
    }

    pub fn module<'a>(&'a mut self, name: Ident) -> Result<Yrc<Module>, CompileError<'src>> {
        // 1. depth-first pass

        let this = Yrc::new_cyclic(|module_weak| {
            let module_parent = self.modules.last().map(Yrc::downgrade);
            let mut submodules = Vec::new();

            let scope = Yrc::new_cyclic(|scope| {
                let mut scope = Scope {
                    parent: self.weak_scope(),
                    items: vec![],
                    _rules: Default::default(),
                };

                for outer in self.ast {
                    use yuri_ast::item::OuterDeclaration;
                    match outer {
                        OuterDeclaration::Submodule(module_item) => {
                            todo!("submodule tail recursion")
                            // submodules.push();
                        }
                        OuterDeclaration::GlobalVariable(variable_item) => {
                            let mut variable = Yrc::new(
                                VariableItem {
                                    parent_scope: self.weak_focus(),
                                    name: variable_item.name,
                                    explicit_type: variable_item
                                        .written_ty
                                        .as_ref()
                                        .map(ParseLower::lower),
                                    value: expression_unimplemented!(),
                                }
                                .into(),
                            );

                            // variable_item

                            scope.items.push(ScopeItem::Variable(variable));
                        }
                        OuterDeclaration::Function(function_item) => {
                            let mut function = Yrc::new_cyclic(|function| {
                                let parameters = function_item
                                    .parameters
                                    .iter()
                                    .map(|param| {
                                        Yrc::new(
                                            ParameterItem {
                                                parent_function: function.clone(),
                                                name: param.name,
                                                attributes: self.shallow_convert_attributes(
                                                    &function_item.attributes,
                                                ),
                                                explicit_type: param.explicit_type.lower(),
                                            }
                                            .into(),
                                        )
                                    })
                                    .collect();

                                FunctionItem {
                                    parent_scope: self.weak_focus(),
                                    name: function_item.name,
                                    attributes: self
                                        .shallow_convert_attributes(&function_item.attributes),
                                    parameters,
                                    return_type: function_item.return_type.lower(),
                                    body: function_item.body.lower(),
                                }
                                .into()
                            });

                            scope.items.push(ScopeItem::Function(function));
                        }
                        OuterDeclaration::TypeAlias(type_alias_item) => {
                            scope.items.push(ScopeItem::TypeAlias(Yrc::new(
                                TypeAliasItem {
                                    parent_scope: self.weak_focus(),
                                    name: type_alias_item.name,
                                    aliases: type_alias_item.aliases.lower(),
                                }
                                .into(),
                            )));
                        }
                        OuterDeclaration::Import(qpath) => {
                            scope.items.push(ScopeItem::Import(qpath.clone()));
                        }
                    }
                }

                scope.into()
            });

            let this = Module {
                parent: module_parent,
                name,
                submodules,
                scope,
            };

            this.into()
        });

        Ok(this)
    }

    pub fn shallow_convert_attributes(
        &self,
        attribs: &[yuri_ast::item::Attribute],
    ) -> Vec<Attribute> {
        attribs
            .iter()
            .map(|attrib| Attribute {
                path: attrib.path.clone(),
                args: Default::default(),
            })
            .collect()
    }

    pub fn resolve_type(
        &self,
        current_scope: Yrc<Scope>,
        qpath: &Qpath,
    ) -> Resolution<Ywk<TypeAliasItem>> {
        let mut arc = current_scope;
        let mut guard = arc.lock().unwrap();
        loop {
            for item in &guard.items {
                if let ScopeItem::TypeAlias(alias) = item {
                    return Resolution::Resolved {
                        item_path: qpath.clone(),
                        item: Yrc::downgrade(alias),
                    };
                }
            }

            if let Some(parent) = &guard.parent {
                let t_arc = parent.upgrade().unwrap();
                drop(guard);
                drop(arc);
                arc = t_arc;
                guard = arc.lock().unwrap();
            } else {
                break;
            }
        }
        Resolution::Unresolved(qpath.clone())
    }
}
