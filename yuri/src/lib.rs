use crate::compile::Module;

mod codegen;
mod compile;
mod parse;

use std::error::Error;

pub type YuriError = Box<dyn Error + Send + Sync + 'static>;

/// Represents one "compilation context" of Yuri.
pub struct YuriContext<'a> {
    modules: Vec<Module<'a>>,
}

impl<'a> YuriContext<'a> {
    pub fn new(initial_capacity: usize) -> Self {
        Self {
            modules: Vec::with_capacity(initial_capacity),
        }
    }

    pub fn parse_module(
        &self,
        name: impl Into<String>,
        code: &str,
    ) -> Result<Module<'a>, YuriError> {
        todo!()
        // parse::parse_module(name.into(), code)
    }
}
