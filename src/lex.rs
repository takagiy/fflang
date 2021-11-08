use std::{
    iter::{Enumerate, Peekable},
    str::{Chars, Lines},
};

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Op(String),
    Word(String),
    LtStr(String),
    LtInt(Sign, u64),
    LtFloat(f64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

pub struct Lexer<'a> {
    input: &'a str,
    lines: Peekable<Enumerate<Lines<'a>>>,
    chars: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            lines: input.lines().enumerate().peekable(),
            chars: "".chars().enumerate().peekable(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    row: usize,
    letter: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Location, Token);
    fn next(&mut self) -> Option<Self::Item> {
        let mut row = 0;

        loop {
            match self.chars.peek() {
                None => match self.lines.next() {
                    None => return None,
                    Some(line) => {
                        row = line.0;
                        self.chars = line.1.chars().enumerate().peekable();
                    }
                },
                Some((_, c)) if !c.is_whitespace() => break,
                _ => { self.chars.next(); },
            }
        }

        let letter = self.chars.peek().unwrap().0;

        let token = match self.chars.peek().unwrap().1 {
            '(' => Token::LParen,
            ')' => Token::RParen,
            'a'..='z' | 'A'..='Z' | '_' => Token::Word(
                self.chars
                    .take_while_ref(|c| c.1.is_ascii_alphabetic() || c.1 == '_')
                    .map(|c| c.1)
                    .collect(),
            ),
            '"' => {
                self.chars.next();
                Token::LtStr(
                    self.chars
                        .by_ref()
                        .take_while(|c| c.1 == '"')
                        .map(|c| c.1)
                        .collect(),
                )
            }
            '0'..='9' => Token::LtInt(
                Sign::Positive,
                self.chars
                    .take_while_ref(|c| c.1.is_ascii_digit())
                    .map(|c| c.1)
                    .collect::<String>()
                    .parse()
                    .unwrap(),
            ),
            c if c.is_ascii_punctuation() => Token::Op(
                self.chars
                    .take_while_ref(|c| c.1.is_ascii_punctuation())
                    .map(|c| c.1)
                    .collect(),
            ),
            c => panic!("unrecognized symbol \"{}\"", c),
        };

        return Some((Location { row, letter }, token));
    }
}

#[test]
fn lexer_test() {
    let mut lx = Lexer::new(&"Hello, World!
    let x = 90 in
        x_**2
    ");
    assert_eq!(lx.next().unwrap().1, Token::Word("Hello".into()));
    assert_eq!(lx.next().unwrap().1, Token::Op(",".into()));
    assert_eq!(lx.next().unwrap().1, Token::Word("World".into()));
    assert_eq!(lx.next().unwrap().1, Token::Op("!".into()));
    assert_eq!(lx.next().unwrap().1, Token::Word("let".into()));
    assert_eq!(lx.next().unwrap().1, Token::Word("x".into()));
    assert_eq!(lx.next().unwrap().1, Token::Op("=".into()));
    assert_eq!(lx.next().unwrap().1, Token::LtInt(Sign::Positive, 90));
    assert_eq!(lx.next().unwrap().1, Token::Word("in".into()));
    assert_eq!(lx.next().unwrap().1, Token::Word("x_".into()));
    assert_eq!(lx.next().unwrap().1, Token::Op("**".into()));
    assert_eq!(lx.next().unwrap().1, Token::LtInt(Sign::Positive, 2));
    assert_eq!(lx.next(), None);
}
