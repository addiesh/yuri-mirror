use std::error::Error;
use std::{env, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Box<[String]> = env::args().collect();
    let code = fs::read_to_string(&args[1])?;
    yuri::_print_syntax_tree(&code)?;
    Ok(())
}
