use itertools::Itertools;
use thiserror::Error;

use std::{collections::HashMap, str::FromStr};

use crate::hir_gen::{Expr, ExprKind, FnDef, HirGenError, Id, Literal};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum HirCheckError {
    #[error("Unknown identifier were found")]
    UnknownName(String),
    #[error("Variable were not recognized by typechecker")]
    UnknownRef,
    #[error("Unexpected types were found")]
    TypeMismatch,
    #[error("Type of expression is unknown")]
    TypeUnknown,
    #[error(transparent)]
    HirGenError(#[from] HirGenError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entity {
    orig_id: Id,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Fn(Box<Type>, Vec<Type>),
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseTypeError {
    #[error("Unknown typename was found")]
    Unknown,
}

pub struct Environment<'hir> {
    pub entities: Vec<Entity>,
    pub types: HashMap<Id, Type>,
    pub refs: HashMap<Id, usize>,
    var_env: HashMap<&'hir str, Vec<usize>>,
}

pub struct HirChecker<'hir> {
    pub env: Environment<'hir>,
    pub hir: &'hir [Result<FnDef, HirGenError>],
}

impl FromStr for Type {
    type Err = ParseTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "Int" {
            Ok(Type::Int)
        } else if s == "Bool" {
            Ok(Type::Bool)
        } else if s == "Str" {
            Ok(Type::Str)
        } else {
            Err(ParseTypeError::Unknown)
        }
    }
}

impl<'hir> Default for Environment<'hir> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'hir> Environment<'hir> {
    pub fn new() -> Self {
        Environment {
            entities: Vec::new(),
            types: HashMap::new(),
            refs: HashMap::new(),
            var_env: HashMap::new(),
        }
    }
}

impl<'hir> HirChecker<'hir> {
    pub fn new(hir: &'hir [Result<FnDef, HirGenError>]) -> Self {
        HirChecker {
            env: Environment::new(),
            hir,
        }
    }

    pub fn check_type(&mut self) -> Result<(), HirCheckError> {
        for decl in self.hir {
            match decl {
                Ok(fn_def) => self.env.type_fn(fn_def)?,
                Err(e) => return Err(HirCheckError::HirGenError(e.clone())),
            }
        }
        Ok(())
    }

    pub fn collect_entities(&mut self) -> Result<(), HirCheckError> {
        for decl in self.hir {
            match decl {
                Ok(fn_def) => self.env.collect_fn_entities(fn_def)?,
                Err(e) => return Err(HirCheckError::HirGenError(e.clone())),
            }
        }
        Ok(())
    }
}

impl<'hir> Environment<'hir> {
    fn type_fn(&mut self, fn_def: &'hir FnDef) -> Result<(), HirCheckError> {
        let mut par_ty = Vec::new();
        for param in &fn_def.params {
            self.set_ty(param.id, param.ty.clone());
            par_ty.push(param.ty.clone());
        }
        let ret_ty = self.type_expr(&fn_def.body)?.clone();
        self.set_ty(fn_def.let_id, Type::Fn(Box::new(ret_ty), par_ty));
        Ok(())
    }

    fn type_expr(&mut self, expr: &'hir Expr) -> Result<&Type, HirCheckError> {
        let ty = match &expr.kind {
            ExprKind::VarRefExpr(_) => {
                return self.get_ty(expr.id);
            }
            ExprKind::LiteralExpr(ex) => match ex {
                Literal::Int(_, _) => Type::Int,
                Literal::Float(_) => Type::Float,
                Literal::Bool(_) => Type::Bool,
                Literal::Str(_) => Type::Str,
            },
            ExprKind::VarLetExpr(ex) => {
                let def_ty = self.type_expr(&ex.def)?.clone();
                self.set_ty(ex.let_id, def_ty);
                self.type_expr(&ex.body)?.clone()
            }
            ExprKind::FnAppExpr(ex) => {
                let fn_ty = self.get_ty(ex.op.id)?.clone();
                match fn_ty {
                    Type::Fn(ret_ty, par_ty) => {
                        for (par_ty, arg) in par_ty.iter().zip(ex.args.iter()) {
                            let arg_ty = self.type_expr(arg)?.clone();
                            self.unify(par_ty, &arg_ty)?;
                        }
                        ret_ty.as_ref().clone()
                    }
                    _ => return Err(HirCheckError::TypeMismatch),
                }
            }
            ExprKind::IfExpr(ex) => {
                let cond_ty = self.type_expr(&ex.cond)?.clone();
                self.unify(&Type::Bool, &cond_ty)?;
                let conseq_ty = self.type_expr(&ex.conseq)?.clone();
                let alter_ty = self.type_expr(&ex.alter)?.clone();
                self.unify(&conseq_ty, &alter_ty)?;
                conseq_ty
            }
        };
        self.set_ty(expr.id, ty);
        self.get_ty(expr.id)
    }

    fn unify(&mut self, t: &Type, u: &Type) -> Result<(), HirCheckError> {
        if t != u {
            Err(HirCheckError::TypeMismatch)
        } else {
            Ok(())
        }
    }

    pub fn get_ty(&self, id: Id) -> Result<&Type, HirCheckError> {
        self.refs
            .get(&id)
            .and_then(|idx| self.types.get(&self.entities[*idx].orig_id))
            .or_else(|| self.types.get(&id)).ok_or(HirCheckError::TypeUnknown)
    }

    fn set_ty(&mut self, id: Id, ty: Type) {
        self.types.entry(id).or_insert(ty);
    }

    fn collect_fn_entities(&mut self, fn_def: &'hir FnDef) -> Result<(), HirCheckError> {
        self.add_entity(&fn_def.name, fn_def.let_id);
        for param in &fn_def.params {
            self.add_entity(&param.name, param.id);
        }
        self.collect_expr_entities(&fn_def.body)?;
        Ok(())
    }

    fn collect_expr_entities(&mut self, expr: &'hir Expr) -> Result<(), HirCheckError> {
        match &expr.kind {
            ExprKind::VarRefExpr(ex) => {
                self.resolve_id(expr.id, &ex.name)?;
            }
            ExprKind::LiteralExpr(_) => (),
            ExprKind::VarLetExpr(ex) => {
                self.collect_expr_entities(&ex.def)?;
                self.add_entity(&ex.name, ex.let_id);
                self.collect_expr_entities(&ex.body)?;
                self.remove_name(&ex.name);
            }
            ExprKind::IfExpr(ex) => {
                self.collect_expr_entities(&ex.cond)?;
                self.collect_expr_entities(&ex.conseq)?;
                self.collect_expr_entities(&ex.alter)?;
            }
            ExprKind::FnAppExpr(ex) => {
                self.collect_expr_entities(&ex.op)?;
                ex.args
                    .iter()
                    .map(|arg| self.collect_expr_entities(arg))
                    .try_collect()?;
            }
        };
        Ok(())
    }

    fn remove_name(&mut self, name: &'hir str) {
        if let Some(ent_vec) = self.var_env.get_mut(name) {
            ent_vec.pop();
            if ent_vec.is_empty() {
                self.var_env.remove(name);
            }
        }
    }

    fn resolve_id(&mut self, id: Id, name: &'hir str) -> Result<(), HirCheckError> {
        let ent = self
            .var_env
            .get(name)
            .and_then(|ent_vec| ent_vec.last())
            .ok_or_else(|| HirCheckError::UnknownName(name.to_owned()))?;
        self.refs.insert(id, *ent);
        Ok(())
    }

    fn add_entity(&mut self, name: &'hir str, id: Id) {
        let idx = self.entities.len();
        self.entities.push(Entity { orig_id: id });
        self.var_env.entry(name).or_default().push(idx);
        self.refs.insert(id, idx);
    }
}

#[test]
fn test_check_type() {
    let hir = test_hir();
    let mut checker = HirChecker::new(&hir);
    assert_eq!(checker.collect_entities(), Ok(()));
    assert_eq!(checker.check_type(), Ok(()));
    let env = &checker.env;
    assert_eq!(
        env.types.get(&1),
        Some(&Type::Fn(
            Box::new(Type::Int),
            vec![
                Type::Fn(Box::new(Type::Int), vec![Type::Int, Type::Int]),
                Type::Int,
                Type::Int,
                Type::Int,
            ]
        ))
    );
}

#[test]
fn test_collect_entities() {
    let hir = test_hir();
    let mut checker = HirChecker::new(&hir);
    assert_eq!(checker.collect_entities(), Ok(()));
    let env = &checker.env;
    assert_eq!(env.entities.len(), 6);
    let ref1 = env.refs.get(&3);
    let ref2 = env.refs.get(&10);
    let ref3 = env.refs.get(&7);
    let ref4 = env.refs.get(&16);
    assert_ne!(ref1, None);
    assert_eq!(ref1, ref2);
    assert_ne!(ref1, ref3);
    assert_eq!(ref3, ref4);
}

#[cfg(test)]
fn test_hir() -> Vec<Result<FnDef, HirGenError>> {
    use crate::hir_gen::*;
    use crate::lex::Sign;
    vec![Ok(FnDef {
        id: 0,
        let_id: 1,
        name: "sum".to_owned(),
        params: vec![
            FnParam {
                id: 2,
                name: "add".to_owned(),
                ty: Type::Fn(Box::new(Type::Int), vec![Type::Int, Type::Int]),
            },
            FnParam {
                id: 3,
                name: "x".to_owned(),
                ty: Type::Int,
            },
            FnParam {
                id: 4,
                name: "y".to_owned(),
                ty: Type::Int,
            },
            FnParam {
                id: 5,
                name: "z".to_owned(),
                ty: Type::Int,
            },
        ],
        body: Box::new(Expr {
            id: 6,
            kind: ExprKind::VarLetExpr(VarLet {
                let_id: 7,
                name: "x".to_owned(),
                def: Box::new(Expr {
                    id: 8,
                    kind: ExprKind::FnAppExpr(FnApp {
                        op: Box::new(Expr {
                            id: 9,
                            kind: ExprKind::VarRefExpr(VarRef {
                                name: "add".to_owned(),
                            }),
                        }),
                        args: vec![
                            Expr {
                                id: 10,
                                kind: ExprKind::VarRefExpr(VarRef {
                                    name: "x".to_owned(),
                                }),
                            },
                            Expr {
                                id: 11,
                                kind: ExprKind::VarRefExpr(VarRef {
                                    name: "y".to_owned(),
                                }),
                            },
                        ],
                    }),
                }),
                body: Box::new(Expr {
                    id: 12,
                    kind: ExprKind::IfExpr(If {
                        cond: Box::new(Expr {
                            id: 13,
                            kind: ExprKind::LiteralExpr(Literal::Bool(true)),
                        }),
                        conseq: Box::new(Expr {
                            id: 14,
                            kind: ExprKind::FnAppExpr(FnApp {
                                op: Box::new(Expr {
                                    id: 15,
                                    kind: ExprKind::VarRefExpr(VarRef {
                                        name: "add".to_owned(),
                                    }),
                                }),
                                args: vec![
                                    Expr {
                                        id: 16,
                                        kind: ExprKind::VarRefExpr(VarRef {
                                            name: "x".to_owned(),
                                        }),
                                    },
                                    Expr {
                                        id: 17,
                                        kind: ExprKind::VarRefExpr(VarRef {
                                            name: "z".to_owned(),
                                        }),
                                    },
                                ],
                            }),
                        }),
                        alter: Box::new(Expr {
                            id: 18,
                            kind: ExprKind::LiteralExpr(Literal::Int(Sign::Positive, 0)),
                        }),
                    }),
                }),
            }),
        }),
    })]
}
