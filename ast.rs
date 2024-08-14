use crate::{
    lexer::{
        Token,
        Tokens,
    }
};

use std::fmt;

pub struct Decl<'a> {
    left_side: &'a Token<'a>,
    right_side: Tokens<'a>,
}

pub struct Job<'a> {
    target: &'a Token<'a>,
    dependencies: &'a [Token<'a>],
    body: Vec::<&'a Vec::<Token<'a>>>
}

impl<'a> Job<'a> {
    pub fn new(target: &'a Token<'a>,
               dependencies: &'a [Token<'a>],
               body: Vec::<&'a Vec::<Token<'a>>>)
        -> Self
    {
        Self { target, dependencies, body }
    }
}

impl fmt::Display for Job<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{t}: ", t = self.target)?;
        for t in self.dependencies {
            write!(f, "{t} ")?;
        }

        writeln!(f)?;
        for line in self.body.iter() {
            write!(f, "   ")?;
            for t in line.iter() {
                write!(f, " {t}")?
            }
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct Ast<'a> {
    pub decls: Vec::<Decl<'a>>,
    pub jobs: Vec::<Job<'a>>,
}
