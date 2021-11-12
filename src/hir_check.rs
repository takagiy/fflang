use itertools::Itertools;
use thiserror::Error;

use std::collections::HashMap;

use crate::hir_gen::{Decl, Expr, ExprKind, FnDef, HirGenError, Id, Literal};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum HirCheckError {
    #[error("Unknown identifier were found")]
    UnknownName(String),
    #[error(transparent)]
    HirGenError(#[from] HirGenError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entity {}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(usize),
    Int,
    Float,
    Str,
}

pub struct Environment<'hir> {
    pub entities: Vec<Entity>,
    pub types: Vec<Type>,
    pub refs: HashMap<Id, usize>,
    var_env: HashMap<&'hir str, Vec<usize>>,
}

pub struct HirChecker<'hir, I>
where
    I: Iterator<Item = &'hir Result<Decl, HirGenError>>,
{
    pub env: Environment<'hir>,
    pub hir: I,
}

impl<'hir> Environment<'hir> {
    pub fn new() -> Self {
        Environment {
            entities: Vec::new(),
            types: Vec::new(),
            refs: HashMap::new(),
            var_env: HashMap::new(),
        }
    }
}

impl<'hir, I> HirChecker<'hir, I>
where
    I: Iterator<Item = &'hir Result<Decl, HirGenError>>,
{
    pub fn new(hir: I) -> Self {
        HirChecker {
            env: Environment::new(),
            hir,
        }
    }

    pub fn check_type(&mut self) -> Result<(), HirCheckError> {
        for decl in self.hir.by_ref() {
            match decl {
                Ok(decl) => match decl {
                    Decl::FnDefDecl(fn_def) => self.env.type_fn(fn_def)?,
                },
                Err(e) => return Err(HirCheckError::HirGenError(e.clone())),
            }
        }
        Ok(())
    }

    pub fn collect_entities(&mut self) -> Result<(), HirCheckError> {
        for decl in self.hir.by_ref() {
            match decl {
                Ok(decl) => match decl {
                    Decl::FnDefDecl(fn_def) => self.env.collect_fn_entities(fn_def)?,
                },
                Err(e) => return Err(HirCheckError::HirGenError(e.clone())),
            }
        }
        Ok(())
    }
}

impl<'hir> Environment<'hir> {
    fn type_fn(&mut self, fn_def: &'hir FnDef) -> Result<(), HirCheckError> {
        let ret_type = self.type_expr(&fn_def.body)?;
        Ok(())
    }

    fn type_expr(&mut self, expr: &'hir Expr) -> Result<Type, HirCheckError> {
        panic!("")
    }

    fn collect_fn_entities(&mut self, fn_def: &'hir FnDef) -> Result<(), HirCheckError> {
        self.add_entity(&fn_def.name, fn_def.let_id, Entity {});
        for param in &fn_def.params {
            self.add_entity(&param.name, param.id, Entity {});
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
                self.add_entity(&ex.name, ex.let_id, Entity {});
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
            .ok_or(HirCheckError::UnknownName(name.to_owned()))?;
        self.refs.insert(id, *ent);
        Ok(())
    }

    fn add_entity(&mut self, name: &'hir str, id: Id, entity: Entity) {
        let idx = self.entities.len();
        self.entities.push(entity);
        self.var_env.entry(name).or_default().push(idx);
        self.refs.insert(id, idx);
    }
}

#[test]
fn test_collect_entities() {
    use crate::hir_gen::*;
    use crate::lex::Sign;
    let hir = [Ok(Decl::FnDefDecl(FnDef {
        id: 0,
        let_id: 1,
        name: "sum".to_owned(),
        params: vec![
            FnParam {
                id: 2,
                name: "add".to_owned(),
            },
            FnParam {
                id: 3,
                name: "x".to_owned(),
            },
            FnParam {
                id: 4,
                name: "y".to_owned(),
            },
            FnParam {
                id: 5,
                name: "z".to_owned(),
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
                            kind: ExprKind::LiteralExpr(Literal::Int(Sign::Positive, 1)),
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
    }))];
    let mut checker = HirChecker::new(hir.iter());
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
