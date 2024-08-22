use crate::{
    parsing::{
        ast::{
            If, Ast, Decl, Expr,
            Item, Items, Job,
            Operation, IfKind
        },
        lexer::{
            LinizedTokens, Token,
            TokenType, Tokens
        }
    },
};

use std::{
    fmt,
    slice::Iter,
    iter::Peekable
};

pub type LinizedTokensIterator<'a> = Peekable::<Iter::<'a, (usize, Tokens<'a>)>>;

pub const IFEQ:   &'static str = "ifeq";
pub const IFNEQ:  &'static str = "ifneq";
pub const IFDEF:  &'static str = "ifdef";
pub const IFNDEF: &'static str = "ifndef";

pub const IFS: &'static [&'static str] = &[
    IFEQ, IFNEQ, IFDEF, IFNDEF,
];

pub const EXPORT:   &'static str = "export";
pub const UNEXPORT: &'static str = "unexport";

pub const EXPORTS: &'static [&'static str] = &[
    EXPORT, UNEXPORT
];

pub enum ErrorType {
    NoLeftSide,
    NoRightSide,
    NoClosingEndif,
    UnexpectedToken,
    JobWithoutTarget,
    ExportWithNoArgs,
    UnexportWithNoArgs,
    ExpectedOnlyOneTokenOnTheLeftSide,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        match self {
            NoLeftSide => write!(f, "No left side"),
            NoRightSide => write!(f, "No ligh side"),
            NoClosingEndif => write!(f, "No closing endif"),
            UnexpectedToken => write!(f, "Unexpected token"),
            ExportWithNoArgs => write!(f, "Export with no args"),
            JobWithoutTarget => write!(f, "Job without a target"),
            UnexportWithNoArgs => write!(f, "Unexport with no args"),
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

macro_rules! collect_exports {
    ($self: expr, $first: expr, $line: expr, $vty: tt, $errty: expr) => {{
        let item = &$line[1..];
        if item.is_empty() {
            Self::report_err(Some($first), Error::new($errty, None));
        }
        $self.items.push(Item::$vty(item));
    }};
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

    //                                          is token joint or not
    //                                                    ^^
    fn check_for_math(str: &str) -> Option::<(Operation, bool)> {
        if        str.eq("+") {
            Some((Operation::PlusEqual,  true))
        } else if str.ends_with("+") {
            Some((Operation::PlusEqual,  false))
        } else if str.eq("-") {
            Some((Operation::MinusEqual, true))
        } else if str.ends_with("-") {
            Some((Operation::MinusEqual, false))
        } else { None }
    }

    fn parse_eq(first: &'a Token, line: &'a Tokens, eq_idx: usize) -> Item<'a> {
        if let Some(token) = line.get(eq_idx - 1) {
            if eq_idx + 1 >= line.len() {
                panic!("Expected right side after expression")
            }

            'blk: {
                let Some(check) = Self::check_for_math(token.str) else { break 'blk };
                let right_side = &line[eq_idx + 1..];
                let left_side = if check.1 {
                    line.get(eq_idx - 2).unwrap()
                } else {
                    token
                };

                let expr = Expr::new(left_side, check.0, right_side);
                return Item::Expr(expr)
            }
        }

        Self::check_token_pos(eq_idx, Some(first));

        let left_side = first;
        let right_side = line[eq_idx + 1..].into_iter().collect::<Vec::<_>>();
        let decl = Decl::new(left_side, right_side);
        Item::Decl(decl)
    }

    fn parse_export_unexport(first: &'a Token, line: &'a Tokens) -> Item<'a> {
        let exports = &line[1..];
        if first.str.eq(EXPORT) {
            Item::Export(exports)
        } else {
            Item::Unexport(exports)
        }
    }

    fn parse_if(first: &Token, iter: &mut Peekable<Iter<'a, Token>>, iter2: &mut LinizedTokensIterator<'a>) -> Item<'a> {
        use {
            ErrorType::*,
            TokenType::*
        };

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
                } else if let Some(eq_idx) = line.iter().position(|x| matches!(x.typ, Equal)) {
                    let Some(first) = line.get(eq_idx - 1) else { continue };
                    Self::parse_eq(first, line, eq_idx)
                } else if EXPORTS.contains(&first.str) {
                    Self::parse_export_unexport(first, line)
                } else if let Some(colon_idx) = line.iter().position(|x| matches!(x.typ, Colon)) {
                    Self::parse_job(first, line, iter2, colon_idx)
                } else {
                    todo!()
                };

                if else_flag {
                    else_body.push(item);
                } else {
                    body.push(item);
                }
            }
        }

        let keyword = iter.next();
        if !endif {
            Self::report_err(keyword, Error::new(NoClosingEndif, None));
        }

        let ifkind = IfKind::try_from(first.str).unwrap();
        let Some(left_side) = iter.next() else {
            Self::report_err(keyword, Error::new(NoLeftSide, None));
        };

        let right_side = if matches!(ifkind, IfKind::Eq | IfKind::Neq) {
            let v = iter.next().unwrap_or_else(|| {
                Self::report_err(keyword, Error::new(NoRightSide, None))
            });
            Some(v)
        } else { None };

        let r#if = If::new(ifkind, left_side, right_side, body, else_body);
        Item::If(r#if)
    }

    fn parse_job(first: &'a Token, line: &'a Tokens, iter: &mut LinizedTokensIterator<'a>, colon_idx: usize) -> Item<'a> {
        Self::check_token_pos(colon_idx, Some(first));

        let target = first;
        let dependencies = &line[colon_idx + 1..];
        let mut body = Vec::with_capacity(line.len());
        while let Some((wc, line)) = iter.peek() {
            // If the amount of spaces before the token is zero, it means that body of the job is ended.
            if wc.eq(&0) { break }

            body.push(line);
            iter.next();
        }

        let job = Job::new(target, dependencies, body);
        Item::Job(job)
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
                let item = Self::parse_if(&first, &mut iter, &mut self.iter);
                self.items.push(item);
            } else if first.str.eq(EXPORT) {
                collect_exports!(self, first, line, Export, ExportWithNoArgs);
            } else if first.str.eq(UNEXPORT) {
                collect_exports!(self, first, line, Unexport, UnexportWithNoArgs);
            } else if let Some(eq_idx) = line.iter().position(|x| matches!(x.typ, Equal)) {
                let item = Self::parse_eq(first, line, eq_idx);
                self.items.push(item);
            } else if let Some(colon_idx) = line.iter().position(|x| matches!(x.typ, Colon)) {
                let item = Self::parse_job(first, line, &mut self.iter, colon_idx);
                self.items.push(item);
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
