use crate::{
    lexer::{
        Token,
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

#[derive(Debug)]
pub struct CmdJob {
    target: String,
    dependencies: Vec::<String>,
    body: Vec::<Vec::<String>>,
}

impl CmdJob {
    pub fn new(target: String,
               dependencies: Vec::<String>,
               body: Vec::<Vec::<String>>)
        -> Self
    {
        Self { target, dependencies, body }
    }
}

pub type Jobs = Vec::<CmdJob>;

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
struct CurrJob<'a> {
    target: Option::<&'a String>,
    dependencies: Option::<&'a Vec::<String>>,
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
                                                   Some("You can use \"$d\" and \"$<\" ONLY in body of job")), Some(token));
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

        let mut jobs = Jobs::new();
        for job in self.jobs.iter() {
            let mut curr_job = CurrJob::default();
            let target = self.get_value(job.target, Target, &curr_job);
            curr_job.target = Some(&target);

            let mut deps = Vec::new();
            for token in job.dependencies.iter() {
                deps.push(self.get_value(token, Dependencies, &curr_job));
            }
            curr_job.dependencies = Some(&deps);

            let mut body = Vec::new();
            for line in job.body.iter() {
                let mut line_strs = Vec::new();
                for token in line.iter() {
                    line_strs.push(self.get_value(token, Body, &curr_job));
                }
                body.push(line_strs);
            }

            let job = CmdJob::new(target, deps, body);
            jobs.push(job);
        }

        Ok(jobs)
    }
}
