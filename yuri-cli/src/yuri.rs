use std::env;
use std::error::Error;
use std::fs;

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
                .arg(
                    Arg::new("sources")
                        .action(ArgAction::Append)
                        .required(true)
                        .help("The paths to one or more files of Yuri shader code to compile."),
                )
                .arg(
                    Arg::new("output")
                        .short('o')
                        .long("output")
                        .action(ArgAction::Set)
                        .required(true)
                        .help("The name of the file(s) to output the compiled shader binary to."),
                ),
        )
}

// yuri compile
// yuri

fn compile(source_files: &[String]) -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    let source_files = source_files
        .into_par_iter()
        .map(|fname| fs::read_to_string(fname).map(|code| (fname, code)))
        .collect::<Result<Box<[(&String, String)]>, _>>()?;

    {
        let _x = source_files
            .iter()
            .map(|(fname, code)| {
                // yuri_ctx.parse_module(fname, &code)
                yuri::_test_compile(fname, code.as_ref())
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
            let source_files: Vec<String> = sub_m
                .get_many::<String>("sources")
                .unwrap()
                .cloned()
                .collect();
            compile(&source_files)?;
        }
        _ => println!("weh"),
    };

    Ok(())
}
