use std::sync::MutexGuard;

use yuri_ast::Ast;
use yuri_ast::Ident;
use yuri_ast::InStorage;
use yuri_ast::Qpath;
use yuri_ast::expression_unimplemented;

use crate::ParseLower;
use crate::Yrc;
use crate::Ywk;
use crate::attribute::Attribute;
use crate::error::CompileError;
use crate::expression::Expression;
use crate::item::FunctionItem;
use crate::item::Module;
use crate::item::ParameterItem;
use crate::item::TypeAliasItem;
use crate::item::VariableItem;
use crate::resolution::Resolution;
use crate::scope::Scope;
use crate::scope::ScopeItem;

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
                path: Resolution::Unresolved(attrib.path.clone()),
                params: Default::default(),
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
