use thiserror::Error;

use std::iter::Peekable;

use crate::lex::{
    LexError, Sign, Token, TokenKind,
    TokenSpec::{IsOp, IsWord},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    FnDefDecl(FnDef),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<FnParam>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    TypeName(String),
    Generic(String, Vec<Type>),
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
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(Sign, u64),
    Float(f64),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarLet {
    pub name: String,
    pub def: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnApp {
    pub fn_name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub conseq: Box<Expr>,
    pub alter: Box<Expr>,
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("Unexpected token were found")]
    UnexpectedToken,
    #[error("Unexpected EOF were found")]
    UnexpectedEOF,
    #[error(transparent)]
    LexError(#[from] LexError),
}

pub struct Parser<I: Iterator<Item = Result<Token, LexError>>> {
    tokens: Peekable<I>,
    prev_tokens: Vec<Token>,
}

impl<I: Iterator<Item = Result<Token, LexError>>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            prev_tokens: Vec::new(),
        }
    }

    pub fn next_tree(&mut self) -> Result<Decl, ParseError> {
        Ok(Decl::FnDefDecl(self.next_fn_def()?))
    }

    fn next_fn_def(&mut self) -> Result<FnDef, ParseError> {
        let name = self.expect_word()?;
        self.expect(&[IsOp("=")])?;
        let params = self.next_fn_params()?;
        let body = Box::new(self.next_expr(true)?);
        Ok(FnDef { name, params, body })
    }

    fn next_fn_params(&mut self) -> Result<Vec<FnParam>, ParseError> {
        self.expect(&[IsWord("with")])?;
        let mut result = Vec::new();
        loop {
            let name = self.expect_word()?;
            self.expect(&[IsOp(":")])?;
            let type_ = self.next_type()?;
            result.push(FnParam { name, type_ });
            let delim = self.expect(&[IsOp(","), IsWord("do")])?;
            if delim == IsWord("do") {
                break;
            }
        }
        Ok(result)
    }

    fn next_type(&mut self) -> Result<Type, ParseError> {
        let name = self.expect_word()?;
        Ok(Type::TypeName(name))
    }

    fn next_expr(&mut self, consume_args: bool) -> Result<Expr, ParseError> {
        Ok(match self.next_token()? {
            Token::Word(id) => {
                if id == "if" {
                    Expr::IfExpr(self.next_if()?)
                } else if self.peeked_is(&[IsOp("=")]) {
                    Expr::VarLetExpr(self.next_let(id)?)
                } else {
                    self.next_ref_or_app(id, consume_args)?
                }
            }
            Token::LtInt(sign, value) => Expr::LiteralExpr(Literal::Int(sign, value)),
            Token::LtFloat(value) => Expr::LiteralExpr(Literal::Float(value)),
            Token::LtStr(value) => Expr::LiteralExpr(Literal::Str(value)),
            _ => return Err(ParseError::UnexpectedToken),
        })
    }

    fn next_if(&mut self) -> Result<If, ParseError> {
        let cond = Box::new(self.next_expr(true)?);
        self.expect(&[IsWord("then")])?;
        let conseq = Box::new(self.next_expr(true)?);
        self.expect(&[IsWord("else")])?;
        let alter = Box::new(self.next_expr(true)?);
        Ok(If {
            cond,
            conseq,
            alter,
        })
    }

    fn next_let(&mut self, name: String) -> Result<VarLet, ParseError> {
        self.expect(&[IsOp("=")])?;
        let def = Box::new(self.next_expr(true)?);
        self.expect(&[IsWord("in")])?;
        let body = Box::new(self.next_expr(true)?);
        Ok(VarLet { name, def, body })
    }

    fn next_ref_or_app(&mut self, name: String, consume_args: bool) -> Result<Expr, ParseError> {
        let mut args = Vec::new();
        if consume_args {
            loop {
                if self.tokens.peek().is_none()
                    || self.peeked_is(&[IsWord("then"), IsWord("else"), IsWord("in")])
                {
                    break;
                }
                args.push(self.next_expr(false)?);
            }
        }
        Ok(if args.is_empty() {
            Expr::VarRefExpr(VarRef { name })
        } else {
            Expr::FnAppExpr(FnApp {
                fn_name: name,
                args,
            })
        })
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        match self.tokens.next() {
            None => return Err(ParseError::UnexpectedEOF),
            Some(Err(e)) => return Err(ParseError::LexError(e)),
            Some(Ok(t)) => Ok(t),
        }
    }

    fn expect_word(&mut self) -> Result<String, ParseError> {
        match self.next_token()? {
            Token::Word(w) => Ok(w),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn expect_kind(&mut self, expected: &[TokenKind]) -> Result<Token, ParseError> {
        let t = self.next_token()?;
        if expected.contains(&t.kind()) {
            Ok(t)
        } else {
            Err(ParseError::UnexpectedToken)
        }
    }

    fn expect<T>(&mut self, expected: &[T]) -> Result<Token, ParseError>
    where
        Token: PartialEq<T>,
    {
        let t = self.next_token()?;
        if expected.iter().any(|ex| &t == ex) {
            Ok(t)
        } else {
            Err(ParseError::UnexpectedToken)
        }
    }

    fn peeked_is<T>(&mut self, expected: &[T]) -> bool
    where
        Token: PartialEq<T>,
    {
        self.tokens.peek().map_or(false, |next| {
            next.as_ref()
                .map_or(false, |next| expected.iter().any(|ex| next == ex))
        })
    }
}

impl<I> Iterator for Parser<I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    type Item = Result<Decl, ParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.peek().is_none() {
            None
        } else {
            Some(self.next_tree())
        }
    }
}

#[test]
fn test_parser() {
    let tokens = [
        Ok(Token::Word("fizzbuzz".to_owned())),
        Ok(Token::Op("=".to_owned())),
        Ok(Token::Word("with".to_owned())),
        Ok(Token::Word("i".to_owned())),
        Ok(Token::Op(":".to_owned())),
        Ok(Token::Word("Int".to_owned())),
        Ok(Token::Word("do".to_owned())),
        Ok(Token::Word("pln".to_owned())),
        Ok(Token::Word("if".to_owned())),
        Ok(Token::Word("mod".to_owned())),
        Ok(Token::LtInt(Sign::Positive, 15)),
        Ok(Token::Word("i".to_owned())),
        Ok(Token::Word("then".to_owned())),
        Ok(Token::LtStr("fizzbuzz".to_owned())),
        Ok(Token::Word("else".to_owned())),
        Ok(Token::Word("if".to_owned())),
        Ok(Token::Word("mod".to_owned())),
        Ok(Token::LtInt(Sign::Positive, 3)),
        Ok(Token::Word("i".to_owned())),
        Ok(Token::Word("then".to_owned())),
        Ok(Token::LtStr("fizz".to_owned())),
        Ok(Token::Word("else".to_owned())),
        Ok(Token::Word("if".to_owned())),
        Ok(Token::Word("mod".to_owned())),
        Ok(Token::LtInt(Sign::Positive, 5)),
        Ok(Token::Word("i".to_owned())),
        Ok(Token::Word("then".to_owned())),
        Ok(Token::LtStr("buzz".to_owned())),
        Ok(Token::Word("else".to_owned())),
        Ok(Token::Word("to_string".to_owned())),
        Ok(Token::Word("i".to_owned())),
    ];
    let mut parser = Parser::new(tokens.into_iter());
    assert_eq!(
        parser.next_tree().unwrap(),
        Decl::FnDefDecl(FnDef {
            name: "fizzbuzz".to_owned(),
            params: vec![FnParam {
                name: "i".to_owned(),
                type_: Type::TypeName("Int".to_owned())
            }],
            body: Box::new(Expr::FnAppExpr(FnApp {
                fn_name: "pln".to_owned(),
                args: vec![Expr::IfExpr(If {
                    cond: Box::new(Expr::FnAppExpr(FnApp {
                        fn_name: "mod".to_owned(),
                        args: vec![
                            Expr::LiteralExpr(Literal::Int(Sign::Positive, 15)),
                            Expr::VarRefExpr(VarRef {
                                name: "i".to_owned()
                            })
                        ]
                    })),
                    conseq: Box::new(Expr::LiteralExpr(Literal::Str("fizzbuzz".to_owned()))),
                    alter: Box::new(Expr::IfExpr(If {
                        cond: Box::new(Expr::FnAppExpr(FnApp {
                            fn_name: "mod".to_owned(),
                            args: vec![
                                Expr::LiteralExpr(Literal::Int(Sign::Positive, 3)),
                                Expr::VarRefExpr(VarRef {
                                    name: "i".to_owned()
                                })
                            ]
                        })),
                        conseq: Box::new(Expr::LiteralExpr(Literal::Str("fizz".to_owned()))),
                        alter: Box::new(Expr::IfExpr(If {
                            cond: Box::new(Expr::FnAppExpr(FnApp {
                                fn_name: "mod".to_owned(),
                                args: vec![
                                    Expr::LiteralExpr(Literal::Int(Sign::Positive, 5)),
                                    Expr::VarRefExpr(VarRef {
                                        name: "i".to_owned()
                                    })
                                ]
                            })),
                            conseq: Box::new(Expr::LiteralExpr(Literal::Str("buzz".to_owned()))),
                            alter: Box::new(Expr::FnAppExpr(FnApp {
                                fn_name: "to_string".to_owned(),
                                args: vec![Expr::VarRefExpr(VarRef {
                                    name: "i".to_owned()
                                })]
                            }))
                        }))
                    }))
                })]
            })),
        })
    );
}
