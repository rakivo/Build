use crate::{
    parsing::lexer::{
        Token,
        Tokens
    },
    execution::cmd::{
        Jobs,
        Job as CmdJob,
    }
};

use std::{
    env,
    fmt,
    process::exit,
    collections::{HashMap, HashSet}
};

pub struct Decl<'a> {
    pub left_side: &'a Token<'a>,
    pub right_side: &'a [Token<'a>],
}

impl<'a> Decl<'a> {
    #[inline]
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

#[derive(Debug)]
pub enum Operation {
    Plus,
    PlusEqual,

    Minus,
    MinusEqual,
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub left_side: String,
    pub operation: Operation,
    pub right_side: Token<'a>,
}

impl<'a> Expr<'a> {
    #[inline]
    pub fn new(left_side: String,
               operation: Operation,
               right_side: Token<'a>)
        -> Self
    {
        Self { left_side, operation, right_side }
    }
}

pub struct Job<'a> {
    target: &'a Token<'a>,
    dependencies: &'a [Token<'a>],
    body: Vec::<&'a Tokens<'a>>
}

impl<'a> Job<'a> {
    #[inline]
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
    JobDependsOnItself,
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
            JobDependsOnItself => write!(f, "Job depends on itself"),
            UndefinedEnviromentVariable => write!(f, "Undefined enviroment variable"),
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
    pub exprs: Vec::<Expr<'a>>,
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

    fn detect_cycle(jobs: &Jobs) -> bool {
        let mut visited = HashSet::with_capacity(jobs.len());
        let mut in_rec_stack = HashSet::with_capacity(jobs.len());
        let job_map = HashMap::from_iter(jobs.iter().map(|job| (job.target.as_str(), job)).into_iter());

        fn dfs<'a>(job_target: &'a str,
                   job_map: &HashMap::<&'a str, &'a CmdJob>,
                   visited: &mut HashSet::<&'a str>,
                   in_rec_stack: &mut HashSet::<&'a str>)
                   -> bool
        {
            if in_rec_stack.contains(job_target) { return true }
            if visited.contains(job_target) { return false }

            visited.insert(job_target);
            in_rec_stack.insert(job_target);

            if let Some(job) = job_map.get(job_target) {
                if job.dependencies.iter().any(|dep| dfs(dep, job_map, visited, in_rec_stack)) {
                    return true
                }
            }

            in_rec_stack.remove(job_target);
            false
        }

        for job in job_map.values() {
            if !visited.contains(job.target.as_str())
            && dfs(job.target.as_str(), &job_map, &mut visited, &mut in_rec_stack)
            {
                return true
            }
        }

        false
    }

    pub fn parse(&mut self) -> Result::<Jobs, Error> {
        use {
            ErrorType::*,
            ParsingSection::*
        };

        let jobs = self.jobs.iter().map(|job| {
            let mut curr_job = CurrJob::default();
            let target = self.get_value(job.target, Target, &curr_job);
            curr_job.target = Some(&target);

            let deps = job.dependencies.iter().fold(Vec::with_capacity(job.dependencies.len()),
                |mut deps, t|
            {
                let value = self.get_value(t, Dependencies, &curr_job);
                if target.eq(&value) {
                    let err = Error::new(JobDependsOnItself, None);
                    eprintln!("{t}: [ERROR] {err}\n\tNOTE: Job \"{target}\" depends on itself, so, infinite recursion detected");
                    exit(1)
                }

                deps.push(value);
                deps
            });

            curr_job.dependencies = Some(&deps);
            let body = job.body.iter().map(|line| {
                line.iter().map(|t| self.get_value(t, Body, &curr_job)).collect::<Vec::<_>>()
            }).collect::<Vec::<_>>();

            CmdJob::new(target, deps, body)
        }).collect::<Vec::<_>>();

        if Self::detect_cycle(&jobs) {
            eprintln!("[ERROR] Infinite dependency cycle detected, aborting..");
            exit(1);
        }

        Ok(jobs)
    }
}
