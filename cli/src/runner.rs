use anyhow::{anyhow, Result};
use fflang::{
    asm_write::AsmWriter,
    hir_check::HirChecker,
    hir_gen::HirGenerator,
    lex::Lexer,
    llir_gen::{llvm::Context, LLIRGen},
    parse::Parser,
};
use std::{
    env::temp_dir,
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
    process::{self, Command},
};

use crate::Opts;

pub struct Runner {
    temp_dir: PathBuf,
}

impl Runner {
    pub fn new() -> Result<Self> {
        let mut tmp = temp_dir();
        let mut subdir = "ffc".to_string();
        subdir.push_str(&process::id().to_string());
        tmp.push(&subdir);
        fs::create_dir_all(&tmp)?;
        Ok(Runner { temp_dir: tmp })
    }

    pub fn compile_exe(&self, opts: &Opts) -> Result<()> {
        self.compile_obj(&opts.input_file, &self.temp_dir.join("out.o"))?;
        self.link(&opts.output_file, &[&self.temp_dir.join("out.o")])
    }

    fn link(&self, output: &Path, objs: &[&Path]) -> Result<()> {
        let exit_code = Command::new("cc")
            .args(objs.iter())
            .args(&[Path::new("-o"), output])
            .status()?;
        if exit_code.success() {
            Ok(())
        } else {
            Err(anyhow!("Linker cc exited with code failed"))
        }
    }

    fn compile_obj(&self, input: &Path, output: &Path) -> Result<()> {
        let input_file = self.read(input)?;
        let lexer = Lexer::new(&input_file);
        let parser = Parser::new(lexer.map(|r| r.map(|(_, tok)| tok)));
        let hir_gen = HirGenerator::new(parser);
        let hir: Vec<_> = hir_gen.collect();
        let mut hir_check = HirChecker::new(&hir);
        hir_check.collect_entities()?;
        hir_check.check_type()?;
        let context = Context::create();
        let mut llir_gen = LLIRGen::new(&context, hir.iter(), hir_check.env);
        llir_gen.generate()?;
        let asm_write = AsmWriter::new();
        asm_write
            .write(&llir_gen.inner.module, Path::new(output))
            .unwrap();
        Ok(())
    }

    fn read(&self, path: &Path) -> Result<String> {
        let mut file = File::open(path)?;
        let mut buffer = String::new();
        file.read_to_string(&mut buffer)?;
        Ok(buffer)
    }
}

impl Drop for Runner {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.temp_dir).unwrap();
    }
}
