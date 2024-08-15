use crate::{
    execution::cmd::{
        Jobs,
        Job as CmdJob,
    },
    parsing::{
        lexer::Token,
    }
};

use std::{
    env,
    fmt
};

pub struct Decl<'a> {
    left_side: &'a Token<'a>,
    right_side: &'a [Token<'a>],
}

impl<'a> Decl<'a> {
    pub fn new(left_side: &'a Token<'a>,
               right_side: &'a [Token<'a>])
        -> Self
    {
        Self { left_side, right_side }
    }
}

impl fmt::Display for Decl<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{l}=", l = self.left_side)?;
        for t in self.right_side.iter() {
            write!(f, "{s} ", s = t.str)?
        }
        Ok(())
    }
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

pub enum ErrorType {
    UndefinedVariable,
    UndefinedEnviromentVariable,
    UnexpectedDependencySpecialSymbolNotInBody,
    UnexpectedTargetSpecialSymbolInTargetSection,
    UnexpectedDependencySpecialSymbolWhileNoDependencies,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        match self {
            UndefinedVariable => write!(f, "Undefined variable"),
            UndefinedEnviromentVariable => write!(f, "Undefined enviromen
t variable"),
            UnexpectedDependencySpecialSymbolNotInBody => write!(f, "Unexpected dependency special symbol in dependencies section"),
            UnexpectedTargetSpecialSymbolInTargetSection => write!(f, "Unexpected target special symbol in target section"),
            UnexpectedDependencySpecialSymbolWhileNoDependencies => write!(f, "Unexpected dependency special symbol while no dependencies"),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
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

#[derive(Default)]
pub struct Ast<'a> {
    pub decls: Vec::<Decl<'a>>,
    pub jobs: Vec::<Job<'a>>,
}

enum ParsingSection {
    Target,
    Dependencies,
    Body,
}

#[derive(Default)]
struct CurrJob<'a> {
    target: Option::<&'a String>,
    dependencies: Option::<&'a Vec::<String>>,
}

impl<'a> Ast<'a> {
    const VARIABLE_SYMBOL: char = '#';
    const SPECIAL_NAMES: &'static [&'static str] = &[
        "<", "@", "t", "d",
    ];

    #[track_caller]
    fn report_err(&self, err: Error, token: Option::<&'a Token<'a>>) -> ! {
        if let Some(errt) = token {
            panic!("{errt}: [ERROR] {err}")
        } else {
            panic!("[ERROR] {err}")
        }
    }

    fn get_value(&self, token: &'a Token<'a>, section: ParsingSection, curr_job: &CurrJob) -> String {
        use {
            ErrorType::*,
            ParsingSection::*,
        };

        if token.str.starts_with(Self::VARIABLE_SYMBOL) {
            let name = &token.str[1..];
            if name.is_empty() {
                self.report_err(Error::new(UndefinedVariable, None), Some(token));
            }

            if let Some(decl) = self.decls.iter().find(|x| x.left_side.str.eq(name)) {
                decl.right_side.iter().map(|x| x.str).collect::<Vec::<_>>().join(" ")
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(token));
            }
        } else if token.str.starts_with('$') {
            let name = &token.str[1..];
            if name.is_empty() {
                self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(token));
            }

            if Self::SPECIAL_NAMES.contains(&name) {
                match name.as_bytes()[0] {
                    b'd' | b'<' => if matches!(section, Dependencies | Target) {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody,
                                                   Some("You can use \"$d\" and \"$<\" ONLY in body of job")), Some(token));
                    } else if let Some(dep) = curr_job.dependencies.unwrap().first() {
                        dep.to_owned()
                    } else {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolWhileNoDependencies, None), Some(token));
                    },

                    b't' | b'@' => if matches!(section, Target) {
                        self.report_err(Error::new(UnexpectedTargetSpecialSymbolInTargetSection,
                                                   Some("You can use \"$t\" and \"$@\" either in body of a job, or in its dependencies")), Some(token));
                    } else if let Some(target) = curr_job.target {
                        target.to_owned()
                    } else {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolWhileNoDependencies, None), Some(token));
                    },
                    _ => unreachable!()
                }
            } else if let Ok(value) = env::var(name) {
                value
            } else {
                self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(token))
            }
        } else {
            token.str.to_owned()
        }
    }

    pub fn parse(&mut self) -> Result::<Jobs, Error> {
        use ParsingSection::*;

        let jobs = self.jobs.iter().map(|job| {
            let mut curr_job = CurrJob::default();
            let target = self.get_value(job.target, Target, &curr_job);
            curr_job.target = Some(&target);

            let deps = job.dependencies.iter()
                .map(|t| self.get_value(t, Dependencies, &curr_job))
                .collect::<Vec::<_>>();

            curr_job.dependencies = Some(&deps);
            let body = job.body.iter().map(|line| {
                line.iter().map(|t| self.get_value(t, Body, &curr_job)).collect::<Vec::<_>>()
            }).collect::<Vec::<_>>();

            CmdJob::new(target, deps, body)
        }).collect::<Vec::<_>>();

        Ok(jobs)
    }
}
