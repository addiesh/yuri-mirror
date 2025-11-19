use std::error::Error;
use std::marker::PhantomData;

use yuri_compiler::error::CompileError;
use yuri_parser::ParseStorage;

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
    let mut storage = ParseStorage::default();
    let ast = yuri_parser::parse_all(&mut storage, input, &tokens)?;
    // let ir = yuri_compiler::compile_
    Ok(())
}
