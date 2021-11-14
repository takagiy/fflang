use inkwell::{
    builder::Builder, context::Context, module::Module, passes::PassManager, values::FunctionValue,
};

use crate::{
    hir_check::Environment,
    hir_gen::{FnDef, HirGenError},
};

pub struct LLIRGen<'hir, 'ctx> {
    pub hir: Vec<Result<FnDef, HirGenError>>,
    pub env: Environment<'hir>,
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub module: Module<'ctx>,
}
