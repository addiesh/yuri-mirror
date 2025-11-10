use std::error::Error;
use std::fs;
use std::{env, path::PathBuf};

use rayon::prelude::*;

fn cli() -> clap::Command {
    use clap::{Arg, ArgAction, Command};
    Command::new("yuri")
        .about("Yuri shader compiler")
        .version(env!("CARGO_PKG_VERSION"))
        .subcommand_required(true)
        .subcommand(
            Command::new("compile")
                .about("Compiles a given set of source files.")
                .arg(Arg::new("sources").action(ArgAction::Append).required(true))
                .arg(
                    Arg::new("output")
                        .short('o')
                        .long("output")
                        .action(ArgAction::Set)
                        .required(true),
                ),
        )
}

// yuri compile
// yuri

fn compile(source_files: &[PathBuf]) -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    let mut yuri_ctx = yuri::YuriContext::new(source_files.len());

    let source_files = source_files
        .into_par_iter()
        .map(|fname| {
            fs::read_to_string(fname).map(|code| (fname.to_string_lossy().to_string(), code))
        })
        .collect::<Result<Box<[(String, String)]>, _>>()?;

    {
        let x = source_files
            .iter()
            .map(|(fname, code)| {
                // yuri_ctx.parse_module(fname, &code)
                yuri::_test_compile(&fname, code.as_ref())
            })
            .collect::<Result<Box<[_]>, _>>()
            .unwrap();
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Trace)
        .init();

    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("compile", sub_m)) => {
            let source_files = sub_m.get_one::<Vec<PathBuf>>("sources").unwrap();
            compile(source_files)?;
        }
        _ => println!("weh"),
    };

    Ok(())
}
