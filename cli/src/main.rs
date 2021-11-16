use std::{env::args, fs::File, io::Read, path::Path};

use fflang::{
    asm_write::AsmWriter, hir_check::HirChecker, hir_gen::HirGenerator, lex::Lexer,
    llir_gen::{LLIRGen, llvm::Context}, parse::Parser,
};

fn main() {
    println!("Hello, world!");
    let args: Vec<_> = args().collect();
    let input = &args[1];
    let output = &args[2];
    println!("{:?}", &input);
    println!("{:?}", &output);
    let mut input = File::open(&input).expect("Input file was not found");
    let mut src = String::new();
    input.read_to_string(&mut src).unwrap();
    let lexer = Lexer::new(&src);
    let parser = Parser::new(lexer.map(|r| r.map(|(_, tok)| tok)));
    let hir_gen = HirGenerator::new(parser);
    let hir: Vec<_> = hir_gen.collect();
    let mut hir_check = HirChecker::new(&hir);
    hir_check
        .collect_entities()
        .expect("Failed to collect entities");
    hir_check.check_type().expect("Failed to check types");
    let context = Context::create();
    let mut llir_gen = LLIRGen::new(&context, hir.iter(), hir_check.env);
    llir_gen.generate().expect("Failed to generate LLVM IR");
    let asm_write = AsmWriter::new();
    asm_write
        .write(&llir_gen.inner.module, Path::new(&output))
        .expect("Failed to write object");
}
