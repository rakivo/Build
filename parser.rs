use crate::{
    ast::{
        Ast,
        Job,
    },
    lexer::{
        Token,
        Tokens,
        TokenType,
        LinizedTokens,
    }
};

use std::{
    fmt,
    slice::Iter,
    iter::Peekable
};

pub type LinizedTokensIterator<'a> = Peekable::<Iter::<'a, (usize, Tokens<'a>)>>;

#[allow(unused)]
const KEYWORDS: &'static [&'static str] = &[
];

pub enum ErrorType {
    UnexpectedToken,
    JobWithoutTarget,
    ExpectedOnlyOneTokenOnTheLeftSide,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        match self {
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
    ast: Ast<'a>,
    iter: LinizedTokensIterator<'a>,
    err_token: Option::<&'a Token<'a>>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(ts: &'a LinizedTokens<'a>) -> Self {
        Self {
            ast: Ast::default(),
            iter: ts.into_iter().peekable(),
            err_token: None
        }
    }

    #[track_caller]
    fn report_err(&mut self, err: Error) -> ! {
        if let Some(errt) = self.err_token {
            panic!("{errt}: [ERROR] {err}")
        } else {
            panic!("[ERROR] {err}")
        }
    }

    // Unexpected First Token error
    #[inline]
    #[track_caller]
    fn uft_err(&mut self, line: &'a Tokens) -> ! {
        self.err_token = line.get(0);
        let err = Error::new(ErrorType::UnexpectedToken, None);
        self.report_err(err)
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
    fn check_token_pos(&mut self, pos: usize, token: Option::<&'a Token<'a>>) {
        if pos > 1 {
            self.err_token = token;
            let err = Error::new(ErrorType::ExpectedOnlyOneTokenOnTheLeftSide, None);
            self.report_err(err);
        }
    }

    fn parse_line(&mut self, _: &usize, line: &'a Tokens) {
        use TokenType::*;

        let mut iter = line.into_iter().peekable();
        let Some(first) = iter.peek() else { return };
        match first.typ {
            Literal => if let Some(eq_idx) = line.iter().position(|x| matches!(x.typ, Equal)) {
                self.check_token_pos(eq_idx, Some(first));

            // Found a job
            } else if let Some(colon_idx) = line.iter().position(|x| matches!(x.typ, Colon)) {
                self.check_token_pos(colon_idx, Some(first));

                let target = first;
                let dependencies = &line[colon_idx + 1..];
                let mut body = Vec::with_capacity(line.len());
                while let Some((wc, line)) = self.iter.peek() {
                    if wc.eq(&0) { break }
                    body.push(line);
                    self.iter.next();
                }

                let job = Job::new(target, dependencies, body);

                println!("{job}");
                self.ast.jobs.push(job);
            } else {
                self.uft_err(line);
            },
            Colon => {
                self.err_token = Some(first);
                let err = Error::new(ErrorType::JobWithoutTarget, Some("Jobs without targets are not allowed in here!"));
                self.report_err(err);
            }
            _ => self.uft_err(line)
        };
    }

    pub fn parse(&mut self) {
        while let Some((wc, line)) = self.iter.next() {
            self.parse_line(wc, line);
        }
    }
}
