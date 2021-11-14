use itertools::Itertools;
use thiserror::Error;

use std::iter::Peekable;

use crate::{
    hir_check::Type,
    lex::Sign,
    parse::{self, ParseError},
};

pub type Id = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub id: Id,
    pub let_id: Id,
    pub name: String,
    pub params: Vec<FnParam>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    pub id: Id,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub id: Id,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    VarRefExpr(VarRef),
    LiteralExpr(Literal),
    VarLetExpr(VarLet),
    FnAppExpr(FnApp),
    IfExpr(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarRef {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(Sign, u64),
    Float(f64),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarLet {
    pub let_id: Id,
    pub name: String,
    pub def: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnApp {
    pub op: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub conseq: Box<Expr>,
    pub alter: Box<Expr>,
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum HirGenError {
    #[error(transparent)]
    ParseError(#[from] ParseError),
}

pub struct HirGenerator<I>
where
    I: Iterator<Item = Result<parse::Decl, ParseError>>,
{
    asts: Peekable<I>,
    id: Id,
}

impl<I> HirGenerator<I>
where
    I: Iterator<Item = Result<parse::Decl, ParseError>>,
{
    pub fn generate(&mut self, ast: parse::Decl) -> Result<FnDef, HirGenError> {
        match ast {
            parse::Decl::FnDefDecl(fn_def) => self.gen_fn_def(fn_def),
        }
    }

    fn gen_fn_def(&mut self, ast: parse::FnDef) -> Result<FnDef, HirGenError> {
        let params = ast
            .params
            .into_iter()
            .map(|p| self.gen_param(p))
            .try_collect()?;
        Ok(FnDef {
            id: self.uniq_id(),
            let_id: self.uniq_id(),
            name: ast.name,
            params,
            body: Box::new(self.gen_expr(*ast.body)?),
        })
    }

    fn gen_param(&mut self, ast: parse::FnParam) -> Result<FnParam, HirGenError> {
        Ok(FnParam {
            id: self.uniq_id(),
            name: ast.name,
            ty: ast.ty,
        })
    }

    fn gen_expr(&mut self, ast: parse::Expr) -> Result<Expr, HirGenError> {
        Ok(Expr {
            id: self.uniq_id(),
            kind: match ast {
                parse::Expr::VarRefExpr(ex) => ExprKind::VarRefExpr(VarRef { name: ex.name }),
                parse::Expr::LiteralExpr(ex) => ExprKind::LiteralExpr(match ex {
                    parse::Literal::Int(sign, value) => Literal::Int(sign, value),
                    parse::Literal::Float(value) => Literal::Float(value),
                    parse::Literal::Str(value) => Literal::Str(value),
                }),
                parse::Expr::VarLetExpr(ex) => ExprKind::VarLetExpr(VarLet {
                    let_id: self.uniq_id(),
                    name: ex.name,
                    def: Box::new(self.gen_expr(*ex.def)?),
                    body: Box::new(self.gen_expr(*ex.body)?),
                }),
                parse::Expr::FnAppExpr(ex) => ExprKind::FnAppExpr(FnApp {
                    op: Box::new(Expr {
                        id: self.uniq_id(),
                        kind: ExprKind::VarRefExpr(VarRef { name: ex.fn_name }),
                    }),
                    args: ex
                        .args
                        .into_iter()
                        .map(|p| self.gen_expr(p))
                        .try_collect()?,
                }),
                parse::Expr::IfExpr(ex) => ExprKind::IfExpr(If {
                    cond: Box::new(self.gen_expr(*ex.cond)?),
                    conseq: Box::new(self.gen_expr(*ex.conseq)?),
                    alter: Box::new(self.gen_expr(*ex.alter)?),
                }),
            },
        })
    }

    fn uniq_id(&mut self) -> Id {
        self.id += 1;
        self.id
    }
}

impl<I> Iterator for HirGenerator<I>
where
    I: Iterator<Item = Result<parse::Decl, ParseError>>,
{
    type Item = Result<FnDef, HirGenError>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.asts.next()? {
            Ok(ast) => self.generate(ast),
            Err(e) => Err(HirGenError::ParseError(e)),
        })
    }
}
