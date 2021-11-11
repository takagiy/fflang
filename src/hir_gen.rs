use itertools::Itertools;
use thiserror::Error;

use std::iter::Peekable;

use crate::{
    lex::Sign,
    parse::{self, ParseError},
};

pub type Id = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Name(TypeName),
    Fn(FnType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeName {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    pub ret: Box<Type>,
    pub params: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    FnDefDecl(FnDef),
}

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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    VarRefExpr(VarRef),
    LiteralExpr(Literal),
    VarLetExpr(VarLet),
    FnAppExpr(FnApp),
    IfExpr(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarRef {
    pub id: Id,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: Id,
    pub kind: LitKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitKind {
    Int(Sign, u64),
    Float(f64),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarLet {
    pub id: Id,
    pub let_id: Id,
    pub name: String,
    pub def: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnApp {
    pub id: Id,
    pub op: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub id: Id,
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
    pub fn generate(&mut self, ast: parse::Decl) -> Result<Decl, HirGenError> {
        match ast {
            parse::Decl::FnDefDecl(fn_def) => self.gen_fn_def(fn_def),
        }
    }

    fn gen_fn_def(&mut self, ast: parse::FnDef) -> Result<Decl, HirGenError> {
        let params = ast
            .params
            .into_iter()
            .map(|p| self.gen_param(p))
            .try_collect()?;
        Ok(Decl::FnDefDecl(FnDef {
            id: self.uniq_id(),
            let_id: self.uniq_id(),
            name: ast.name,
            params,
            body: Box::new(self.gen_expr(*ast.body)?),
        }))
    }

    fn gen_param(&mut self, ast: parse::FnParam) -> Result<FnParam, HirGenError> {
        Ok(FnParam {
            id: self.uniq_id(),
            name: ast.name,
        })
    }

    fn gen_expr(&mut self, ast: parse::Expr) -> Result<Expr, HirGenError> {
        Ok(match ast {
            parse::Expr::VarRefExpr(ex) => Expr::VarRefExpr(VarRef {
                id: self.uniq_id(),
                name: ex.name,
            }),
            parse::Expr::LiteralExpr(ex) => Expr::LiteralExpr(Literal {
                id: self.uniq_id(),
                kind: match ex {
                    parse::Literal::Int(sign, value) => LitKind::Int(sign, value),
                    parse::Literal::Float(value) => LitKind::Float(value),
                    parse::Literal::Str(value) => LitKind::Str(value),
                },
            }),
            parse::Expr::VarLetExpr(ex) => Expr::VarLetExpr(VarLet {
                id: self.uniq_id(),
                let_id: self.uniq_id(),
                name: ex.name,
                def: Box::new(self.gen_expr(*ex.def)?),
                body: Box::new(self.gen_expr(*ex.body)?),
            }),
            parse::Expr::FnAppExpr(ex) => Expr::FnAppExpr(FnApp {
                id: self.uniq_id(),
                op: Box::new(Expr::VarRefExpr(VarRef {
                    id: self.uniq_id(),
                    name: ex.fn_name,
                })),
                args: ex
                    .args
                    .into_iter()
                    .map(|p| self.gen_expr(p))
                    .try_collect()?,
            }),
            parse::Expr::IfExpr(ex) => Expr::IfExpr(If {
                id: self.uniq_id(),
                cond: Box::new(self.gen_expr(*ex.cond)?),
                conseq: Box::new(self.gen_expr(*ex.conseq)?),
                alter: Box::new(self.gen_expr(*ex.alter)?),
            }),
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
    type Item = Result<Decl, HirGenError>;
    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.asts.next()? {
            Ok(ast) => self.generate(ast),
            Err(e) => Err(HirGenError::ParseError(e)),
        })
    }
}
