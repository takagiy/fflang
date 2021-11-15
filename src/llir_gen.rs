use itertools::Itertools;
use thiserror::Error;

use inkwell::{
    builder::{self, Builder},
    context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    hir_check::{self, HirCheckError, Type},
    hir_gen::{Expr, ExprKind, FnDef, HirGenError, Literal},
    lex::Sign,
};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum LLIRGenError {
    #[error("Undeclared function was found")]
    UndeclaredFunction,
    #[error("Unexpected non-functional type were found")]
    NonFunctionalType,
    #[error("Unexpected functional type were found")]
    FunctionalType,
    #[error(transparent)]
    HirCheckError(#[from] HirCheckError),
}

pub struct Environment<'ctx> {
    pub functions: Vec<Option<FunctionValue<'ctx>>>,
    pub vars: Vec<PointerValue<'ctx>>,
}

pub struct Context<'ctx> {
    raw: &'ctx context::Context,
}

pub struct LLIRGen<'hir, 'ctx, I>
where
    I: Iterator<Item = &'hir Result<FnDef, HirGenError>>,
{
    pub hir: I,
    pub ck_env: hir_check::Environment<'hir>,
    pub env: Environment<'ctx>,
    pub context: Context<'ctx>,
    pub builder: Builder<'ctx>,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub module: Module<'ctx>,
    pub current_fn: Option<FunctionValue<'ctx>>,
}

impl<'hir, 'ctx, I> LLIRGen<'hir, 'ctx, I>
where
    I: Iterator<Item = &'hir Result<FnDef, HirGenError>>,
{
    pub fn generate(&mut self, fn_def: &'hir FnDef) -> Result<(), LLIRGenError> {
        let ty = self.ck_env.get_ty(fn_def.let_id)?;
        let ty = self.context.fn_ty(ty)?;
        let f = self.module.add_function(&fn_def.name, ty, None);
        self.env.functions[self.ck_env.refs[&fn_def.let_id]] = Some(f);
        self.current_fn = Some(f);
        let entry = self.context.raw.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);
        for (param_val, param_def) in f.get_param_iter().zip(&fn_def.params) {
            param_val.set_name(&param_def.name);
            let var = self
                .builder
                .build_alloca(param_val.get_type(), &param_def.name);
            self.builder.build_store(var, param_val);
        }
        let ret = self.gen_expr(&fn_def.body)?;
        self.builder.build_return(Some(&ret));
        Ok(())
    }

    pub fn gen_expr(&mut self, expr: &'hir Expr) -> Result<BasicValueEnum<'ctx>, LLIRGenError> {
        Ok(match &expr.kind {
            ExprKind::VarRefExpr(_) => self.builder.build_load(
                self.env.vars[self.ck_env.refs[&expr.id]],
                &expr.id.to_string(),
            ),
            ExprKind::LiteralExpr(ex) => match ex {
                Literal::Int(sign, value) => {
                    let positive = self.context.raw.i32_type().const_int(*value, true);
                    if *sign == Sign::Positive {
                        positive.into()
                    } else {
                        positive.const_neg().into()
                    }
                }
                Literal::Float(value) => self.context.raw.f64_type().const_float(*value).into(),
                Literal::Bool(value) => self
                    .context
                    .raw
                    .i8_type()
                    .const_int(*value as u64, false)
                    .into(),
                Literal::Str(_) => unimplemented!("Str literal is not implemented yet"),
            },
            ExprKind::VarLetExpr(ex) => {
                let var_ty = self.context.basic_ty(self.ck_env.get_ty(ex.def.id)?)?;
                let var = self.builder.build_alloca(var_ty, &ex.let_id.to_string());
                let def = self.gen_expr(&ex.def)?;
                self.builder.build_store(var, def);
                self.env.vars[self.ck_env.refs[&ex.let_id]] = var;
                return self.gen_expr(&ex.body);
            }
            ExprKind::FnAppExpr(ex) => {
                let f = self.env.functions[self.ck_env.refs[&ex.op.id]]
                    .ok_or(LLIRGenError::UndeclaredFunction)?;
                let args: Vec<BasicMetadataValueEnum<'ctx>> = ex
                    .args
                    .iter()
                    .map(|a| self.gen_expr(a).map(Into::into))
                    .try_collect()?;
                self.builder
                    .build_call(f, &args, &expr.id.to_string())
                    .try_as_basic_value()
                    .left()
                    .unwrap_or(self.context.raw.i8_type().const_zero().into())
            }
            ExprKind::IfExpr(ex) => {
                let cur_f = self.current_fn.unwrap();
                let cond = self.gen_expr(&ex.cond)?.into_int_value();
                let conseq_bb = self.context.raw.append_basic_block(cur_f, "ifconseq");
                let alter_bb = self.context.raw.append_basic_block(cur_f, "ifalter");
                let merge_bb = self.context.raw.append_basic_block(cur_f, "ifend");
                self.builder
                    .build_conditional_branch(cond, conseq_bb, alter_bb);
                self.builder.position_at_end(conseq_bb);
                let conseq = self.gen_expr(&ex.conseq)?;
                self.builder.build_unconditional_branch(merge_bb);
                self.builder.position_at_end(alter_bb);
                let alter = self.gen_expr(&ex.alter)?;
                self.builder.build_unconditional_branch(merge_bb);
                let ty = self.ck_env.get_ty(expr.id)?;
                let ty = self.context.basic_ty(ty)?;
                let phi = self.builder.build_phi(ty, &expr.id.to_string());
                phi.add_incoming(&[(&conseq, conseq_bb), (&alter, alter_bb)]);
                phi.as_basic_value()
            }
        })
    }
}

impl<'ctx> Context<'ctx> {
    pub fn basic_ty(&mut self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, LLIRGenError> {
        Ok(match ty {
            Type::Int => self.raw.i32_type().into(),
            Type::Float => self.raw.f64_type().into(),
            Type::Bool => self.raw.i8_type().into(),
            Type::Str => unimplemented!("Type Str is not implemented yet"),
            Type::Fn(_, _) => return Err(LLIRGenError::FunctionalType),
        })
    }

    pub fn fn_ty(&mut self, ty: &Type) -> Result<FunctionType<'ctx>, LLIRGenError> {
        match ty {
            Type::Fn(ret, param) => {
                let param: Vec<BasicMetadataTypeEnum<'ctx>> = param
                    .iter()
                    .map(|ty| self.basic_ty(&ty).map(Into::into))
                    .try_collect()?;
                Ok(self.basic_ty(&ret)?.fn_type(&param, false))
            }
            _ => Err(LLIRGenError::NonFunctionalType),
        }
    }
}
