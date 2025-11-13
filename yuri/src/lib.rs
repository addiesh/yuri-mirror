use std::{error::Error, marker::PhantomData};

use yuri_compiler::CompileError;

pub type YuriError = Box<dyn Error + Send + Sync + 'static>;

/// Represents one "compilation context" of Yuri.
pub struct YuriContext<'a> {
    _pd: PhantomData<&'a ()>, // modules: Vec<Module<'a>>,
}

impl<'a> YuriContext<'a> {
    pub fn new(initial_capacity: usize) -> Self {
        Self {
            _pd: Default::default(),
            // modules: Vec::with_capacity(initial_capacity),
        }
    }

    // pub fn parse_module(
    //     &self,
    //     name: impl Into<String>,
    //     code: &str,
    // ) -> Result<Module<'a>, YuriError> {
    //     parse::parse_module(name.into(), code)
    // }
}

pub fn _test_compile<'a>(fname: &'a str, input: &'a str) -> Result<(), CompileError<'a>> {
    let tokens: Vec<_> = yuri_lexer::tokenize(input).collect();
    for token in &tokens {
        println!("{token:?}");
    }
    let ast = yuri_parser::parse_all(input, &tokens)?;
    // let ir = yuri_compiler::compile_
    Ok(())
}
