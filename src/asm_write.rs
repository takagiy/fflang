use thiserror::Error;

use std::{ops::Deref, path::Path};

use inkwell::{
    module::Module,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum AsmWriteError {
    #[error("Failed to write")]
    WriteFailed,
}

pub struct AsmWriter {
    pub machine: TargetMachine,
}

impl AsmWriter {
    fn new() -> Self {
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

    fn write(&self, module: &Module, path: &Path) -> Result<(), AsmWriteError> {
        self.machine
            .write_to_file(module, FileType::Object, path)
            .map_err(|_| AsmWriteError::WriteFailed)
    }
}
