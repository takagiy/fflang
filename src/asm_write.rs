use thiserror::Error;

use std::{ops::Deref, path::Path};

use inkwell::{OptimizationLevel, module::Module, support::LLVMString, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}};

#[derive(Debug, Error, PartialEq)]
pub enum AsmWriteError {
    #[error("Failed to write")]
    WriteFailed(LLVMString),
}

pub struct AsmWriter {
    pub machine: TargetMachine,
}

impl AsmWriter {
    pub fn new() -> Self {
        Target::initialize_all(&InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: true,
            info: true,
            machine_code: true,
        });
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();
        let opt = OptimizationLevel::Aggressive;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let machine = target
            .create_target_machine(
                &triple,
                cpu.deref().to_str().unwrap(),
                features.deref().to_str().unwrap(),
                opt,
                reloc,
                model,
            )
            .unwrap();
        AsmWriter { machine }
    }

    pub fn write(&self, module: &Module, path: &Path) -> Result<(), AsmWriteError> {
        self.machine
            .write_to_file(module, FileType::Object, path)
            .map_err(|msg| AsmWriteError::WriteFailed(msg))
    }
}
