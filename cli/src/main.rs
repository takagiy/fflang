use std::path::PathBuf;

use anyhow::Result;
use clap::{crate_version, Parser};

use crate::runner::Runner;

mod runner;

#[derive(Parser)]
#[clap(version = crate_version!(), author = "Yuki Takagi <takagiy.4dev@gmail.com>")]
pub struct Opts {
    input_file: PathBuf,
    output_file: PathBuf,
}

fn main() -> Result<()> {
    let opts = Opts::parse();
    let runner = Runner::new().unwrap();
    runner.compile_exe(&opts)
}
