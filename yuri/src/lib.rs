use std::error::Error;
use std::path::PathBuf;

use yuri_ast::InStorage;
use yuri_common::to_snake_case;
use yuri_compiler::Yrc;
use yuri_compiler::error::CompileError;
use yuri_compiler::item::Module;
use yuri_lexer::Token;

pub type YuriError = Box<dyn Error + Send + Sync + 'static>;

pub fn _test_compile<'src>(
    file_path: &'src str,
    source: &'src str,
) -> Result<Yrc<Module>, CompileError<'src>> {
    let tokens: Vec<Token> = yuri_lexer::tokenize(source).collect();
    for token in &tokens {
        println!("{token:?}");
    }
    let mut storage = InStorage::default();

    let module_name = to_snake_case(
        &PathBuf::from(file_path)
            .file_stem()
            .ok_or(Box::from("Failed to extract file stem from file path!"))?
            .to_string_lossy(),
    );

    let module_name_ident = storage.to_ident(&module_name);
    let (ast, parse_errors, _state) = yuri_parser::parse_all(source, &mut storage, &tokens);
    let module = yuri_compiler::lower::lower(source, &mut storage, &ast, module_name_ident)?;

    if !parse_errors.is_empty() {
        return Err(CompileError::Multiple(
            parse_errors.into_iter().map(CompileError::Parse).collect(),
        ));
    }

    Ok(module)
    // Ok(todo!("figure out how to return the stuff to the caller"))
}
