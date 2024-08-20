use crate::{
    parsing::{
        ast::{
            If, Ast, Decl, Expr, Item, Items, Job, Operation
        },
        lexer::{
            LinizedTokens, Token, TokenType, Tokens
        }
    },
};

use std::{
    fmt,
    slice::Iter,
    iter::Peekable
};

pub type LinizedTokensIterator<'a> = Peekable::<Iter::<'a, (usize, Tokens<'a>)>>;

const IFS: &'static [&'static str] = &[
    "ifeq", "ifneq"
];

pub enum ErrorType {
    NoClosingEndif,
    UnexpectedToken,
    JobWithoutTarget,
    ExpectedOnlyOneTokenOnTheLeftSide,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        match self {
            NoClosingEndif => write!(f, "No closing endif"),
            UnexpectedToken => write!(f, "Unexpected token"),
            JobWithoutTarget => write!(f, "Job without a target"),
            ExpectedOnlyOneTokenOnTheLeftSide => write!(f, "Expected only one token on the left side")
        }
    }
}

pub struct Error {
    ty: ErrorType,
    note: Option::<&'static str>,
}

impl Error {
    #[inline]
    pub fn new(ty: ErrorType,
               note: Option::<&'static str>)
       -> Self
    {
        Self { ty, note }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = &self.ty;
        if let Some(note) = self.note {
            write!(f, "{ty}\n\tNOTE: {note}")
        } else {
            write!(f, "{ty}")
        }
    }
}

pub struct Parser<'a> {
    pub ast: Ast<'a>,
    pub items: Items<'a>,
    iter: LinizedTokensIterator<'a>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(ts: &'a LinizedTokens<'a>) -> Self {
        Self {
            ast: Ast::default(),
            items: Items::new(),
            iter: ts.into_iter().peekable(),
        }
    }

    #[track_caller]
    fn report_err(err_token: Option::<&Token>, err: Error) -> ! {
        if let Some(errt) = err_token {
            panic!("{errt}: [ERROR] {err}")
        } else {
            panic!("[ERROR] {err}")
        }
    }

    // Unexpected First Token error
    #[inline]
    #[track_caller]
    fn uft_err(line: &'a Tokens) -> ! {
        Self::report_err(line.get(0), Error::new(ErrorType::UnexpectedToken, None))
    }

    // To check token that we have only one token on the left side in these kinda situations:
    // ```
    // FLAGS=-f 69
    // ```
    // or here:
    // ```
    // $OUT: main.c
    //     $CC -o $t $FLAGS
    // ```
    #[inline]
    fn check_token_pos(pos: usize, token: Option::<&'a Token<'a>>) {
        if pos > 1 {
            Self::report_err(token, Error::new(ErrorType::ExpectedOnlyOneTokenOnTheLeftSide, None))
        }
    }

    fn parse_eq(first: &'a Token, line: &'a Tokens, eq_idx: usize) -> Item<'a> {
        if let Some(token) = line.get(eq_idx - 1) {
            if token.str.eq("+") {
                let Some(right_side) = line.get(eq_idx + 1) else {
                    panic!("Expected right side after expression")
                };

                let left_side = line.get(eq_idx - 2).unwrap();
                let expr = Expr::new(left_side, Operation::PlusEqual, right_side);
                return Item::Expr(expr)
            } else if token.str.ends_with("+") {
                let Some(right_side) = line.get(eq_idx + 1) else {
                    panic!("Expected right side after expression")
                };

                let expr = Expr::new(token, Operation::PlusEqual, right_side);
                return Item::Expr(expr)
            } else if token.str.eq("-") {
                let Some(right_side) = line.get(eq_idx + 2) else {
                    panic!("Expected right side after expression")
                };

                let left_side = line.get(eq_idx - 2).unwrap();
                let expr = Expr::new(left_side, Operation::MinusEqual, right_side);
                return Item::Expr(expr)
            } else if token.str.ends_with("-") {
                let Some(right_side) = line.get(eq_idx + 1) else {
                    panic!("Expected right side after expression")
                };

                let expr = Expr::new(token, Operation::MinusEqual, right_side);
                return Item::Expr(expr)
            }
        }

        Self::check_token_pos(eq_idx, Some(first));

        let left_side = first;
        let right_side = line[eq_idx + 1..].into_iter().collect::<Vec::<_>>();
        let decl = Decl::new(left_side, right_side);
        Item::Decl(decl)
    }

    fn parse_if(first: &Token, iter: &mut Peekable<Iter<'a, Token>>, iter2: &mut LinizedTokensIterator<'a>) -> Item<'a> {
        use ErrorType::*;

        let mut endif = false;
        let mut body = Vec::new();
        let (mut else_body, mut else_flag) = (Vec::new(), false);

        while let Some((_, line)) = iter2.next() {
            if line.iter().find(|t| t.str.eq("else")).is_some() {
                else_flag = true;
            } else {
                if line.iter().any(|t| t.str.eq("endif")) {
                    endif = true;
                    break
                }

                let Some(first) = line.first() else { continue };
                let item = if IFS.contains(&first.str) {
                    let mut iter = line.into_iter().peekable();
                    Self::parse_if(first, &mut iter, iter2)
                } else {
                    let Some(eq_idx) = line.iter().position(|x| matches!(x.typ, TokenType::Equal)) else { continue };
                    let Some(first) = line.get(eq_idx - 1) else { continue };
                    Self::parse_eq(first, line, eq_idx)
                };

                if else_flag {
                    else_body.push(item);
                } else {
                    body.push(item);
                }
            }
        }

        let keyword = iter.next().unwrap();
        if !endif {
            Self::report_err(Some(keyword), Error::new(NoClosingEndif, None));
        }

        let rev = if first.str.eq("ifeq") { false } else { true };

        let Some(left_side) = iter.next() else {
            panic!("No left_side")
        };
        let Some(right_side) = iter.next() else {
            panic!("No right_side")
        };

        let r#if = If::new(rev, left_side, right_side, body, else_body);
        Item::If(r#if)
    }

    fn parse_line(&mut self, _: &usize, line: &'a Tokens) {
        use {
            ErrorType::*,
            TokenType::*
        };

        let mut iter = line.into_iter().peekable();
        let Some(first) = iter.peek() else { return };
        if first.str.eq("endif") { return };
        match first.typ {
            Literal => if IFS.contains(&first.str) {
                let item = Self::parse_if(first, &mut iter, &mut self.iter);
                self.items.push(item);
            } else if let Some(eq_idx) = line.iter().position(|x| matches!(x.typ, Equal)) {
                let item = Self::parse_eq(first, line, eq_idx);
                self.items.push(item);
            } else if let Some(colon_idx) = line.iter().position(|x| matches!(x.typ, Colon)) {
                Self::check_token_pos(colon_idx, Some(first));

                let target = first;
                let dependencies = &line[colon_idx + 1..];
                let mut body = Vec::with_capacity(line.len());
                while let Some((wc, line)) = self.iter.peek() {
                    if wc.eq(&0) { break }
                    body.push(line);
                    self.iter.next();
                }

                let job = Job::new(target, dependencies, body);
                self.items.push(Item::Job(job));
            } else {
                Self::uft_err(line);
            },
            Colon => {
                let err = Error::new(JobWithoutTarget, Some("Jobs without targets are not allowed here!"));
                Self::report_err(Some(first), err);
            }
            _ => Self::uft_err(line)
        };
    }

    pub fn parse(&mut self) {
        while let Some((wc, line)) = self.iter.next() {
            self.parse_line(wc, line);
        }
    }
}
