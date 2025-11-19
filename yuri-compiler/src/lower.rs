use std::sync::MutexGuard;

use yuri_parser::Ast;
use yuri_parser::ParseStorage;
use yuri_parser::Qpath;

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

struct Lower<'src> {
    storage: &'src mut ParseStorage<'src>,
    source: &'src str,
    ast: &'src Ast,
    scope: Vec<Yrc<Scope<'src>>>,
}

pub fn lower<'src>(
    storage: &'src mut ParseStorage<'src>,
    source: &'src str,
    ast: &'src Ast,
) -> Result<Module<'src>, CompileError<'src>> {
    // walk the tree!

    let mut lowerer = Lower {
        storage,
        source,
        ast,
        scope: Vec::with_capacity(8),
    };
    lowerer.scope.push(Yrc::new(Scope::new().into()));

    lowerer.module()
}

impl<'src> Lower<'src> {
    pub fn focus(&mut self) -> MutexGuard<'_, Scope<'src>> {
        self.scope.last().unwrap().lock().unwrap()
    }

    // pub fn strong_focus(&self) -> Yrc<Scope<'src>> {
    //     self.scope.last().unwrap().clone()
    // }

    pub fn weak_focus(&self) -> Ywk<Scope<'src>> {
        Yrc::downgrade(self.scope.last().unwrap())
    }

    pub fn module<'a>(&'a mut self) -> Result<Module<'src>, CompileError<'src>> {
        // 1. depth-first pass

        for outer in self.ast {
            use yuri_parser::item::OuterDeclaration;
            match outer {
                OuterDeclaration::GlobalVariable(variable_item) => {
                    let mut variable = Yrc::new(
                        VariableItem {
                            parent_scope: self.weak_focus(),
                            name: variable_item.name,
                            explicit_type: variable_item.written_ty.as_ref().map(Into::into),
                            value: Expression::Unimplemented,
                        }
                        .into(),
                    );

                    // variable_item

                    self.focus().items.push(ScopeItem::Variable(variable));
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
                                        attributes: self
                                            .shallow_convert_attributes(&function_item.attributes),
                                        explicit_type: Into::into(&param.explicit_type),
                                    }
                                    .into(),
                                )
                            })
                            .collect();

                        FunctionItem {
                            parent_scope: self.weak_focus(),
                            name: function_item.name,
                            attributes: self.shallow_convert_attributes(&function_item.attributes),
                            parameters,
                            return_type: Into::into(&function_item.return_type),
                            body: Into::into(&function_item.body),
                        }
                        .into()
                    });

                    self.focus().items.push(ScopeItem::Function(function));
                }
                OuterDeclaration::Alias(type_alias_item) => todo!(),
                OuterDeclaration::Import(ident) => todo!(),
            }
        }

        todo!()
    }

    pub fn shallow_convert_attributes(
        &self,
        attribs: &[yuri_parser::item::Attribute],
    ) -> Vec<Attribute<'src>> {
        attribs
            .iter()
            .map(|attrib| Attribute {
                path: Resolution::Unresolved(attrib.path.clone()),
                _todo_params: Default::default(),
            })
            .collect()
    }

    pub fn resolve_type(&self, qpath: &Qpath) -> Resolution<Ywk<TypeAliasItem<'src>>> {
        // Resolution::Unresolved(qpath)
        todo!()
    }
}
