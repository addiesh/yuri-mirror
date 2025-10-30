use log::debug;
use rayon::prelude::*;
use std::error::Error;
use std::{env, fs};

fn main() -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Trace)
        .init();

    let args: Box<[String]> = env::args().collect();

    // TODO: use clap for CLI

    let source_files = &args[1..];
    debug!("input files: {source_files:?}");

    let mut yuri_ctx = yuri::YuriContext::new(source_files.len());

    source_files
        .into_par_iter()
        .map(|fname| fs::read_to_string(fname).map(|code| (fname, code)))
        .collect::<Result<Box<[_]>, _>>()?
        .into_par_iter()
        .map(|(fname, code)| yuri_ctx.parse_module(fname, &code))
        .collect::<Result<Box<[_]>, _>>()?;

    Ok(())
}
