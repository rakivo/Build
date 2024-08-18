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
    mem::take,
    process::exit,
    collections::{HashMap, HashSet}
};

#[derive(Debug)]
pub struct Decl<'a> {
    pub left_side: &'a Token<'a>,
    pub right_side: Vec::<&'a Token<'a>>,
}

impl<'a> Decl<'a> {
    #[inline]
    pub fn new(left_side: &'a Token<'a>,
               right_side: Vec::<&'a Token<'a>>)
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
    #[allow(unused)]
    Plus,
    PlusEqual,

    #[allow(unused)]
    Minus,
    MinusEqual,
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub left_side: &'a Token<'a>,
    pub operation: Operation,
    pub right_side: &'a Token<'a>,
}

impl<'a> Expr<'a> {
    #[inline]
    pub fn new(left_side: &'a Token<'a>,
               operation: Operation,
               right_side: &'a Token<'a>)
        -> Self
    {
        Self { left_side, operation, right_side }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Item<'a> {
    If(If<'a>),
    Decl(Decl<'a>),
    Expr(Expr<'a>),
    Job(Job<'a>)
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

#[derive(Debug)]
pub struct If<'a> {
    rev: bool,
    left_side: &'a Token<'a>,
    right_side: &'a Token<'a>,
    body: Vec::<Item<'a>>,
    else_body: Vec::<Item<'a>>
}

impl<'a> If<'a> {
    pub fn new(rev: bool,
               left_side: &'a Token<'a>,
               right_side: &'a Token<'a>,
               body: Vec::<Item<'a>>,
               else_body: Vec::<Item<'a>>)
       -> Self
    {
        Self { rev, left_side, right_side, body, else_body }
    }
}

#[derive(Default)]
pub struct Ast<'a> {
    pub items: Vec::<Item<'a>>,
    vars: HashMap::<&'a str, Vec::<&'a Token<'a>>>,
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

            if let Some(value) = self.vars.get(name) {
                value.iter().map(|x| x.str).collect::<Vec::<_>>().join(" ")
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(token))
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

    fn get_value_(&self, token: &'a Token) -> String {
        use ErrorType::*;

        if token.str.starts_with(Self::VARIABLE_SYMBOL) {
            let name = if token.str.ends_with(",") {
                &token.str[1..token.str.len() - 1]
            } else {
                &token.str[1..]
            };

            if name.is_empty() {
                self.report_err(Error::new(UndefinedVariable, None), Some(token));
            }

            if let Some(value) = self.vars.get(name) {
                value.iter().map(|x| x.str).collect::<Vec::<_>>().join(" ")
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(token))
            }
        } else {
            token.str.to_owned()
        }
    }

    fn eval_decl(&mut self, decl: &Decl<'a>) {
        use ErrorType::*;

        if self.vars.contains_key(&decl.left_side.str) {
            println!("{l} redeclared", l = decl.left_side);
        }

        if let Some(token) = decl.right_side.first() {
            if !token.str.starts_with("#") {
                self.vars.insert(decl.left_side.str, decl.right_side.to_owned());
                return
            }

            if let Some(value) = self.vars.get(&token.str[1..]) {
                let mut v = decl.right_side.to_owned();
                v.extend(value.iter());
                self.vars.insert(decl.left_side.str, v);
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(token))
            }
        } else {
            self.vars.insert(decl.left_side.str, decl.right_side.to_owned());
        }
    }

    fn parse_expr(&mut self, expr: &Expr<'a>) {
        use ErrorType::*;

        let key = if expr.left_side.str.ends_with("+") || expr.left_side.str.ends_with("-") {
            &expr.left_side.str[1..expr.left_side.str.len() - 1]
        } else {
            &expr.left_side.str[1..]
        };

        let mut value = Vec::new();
        if expr.right_side.str.starts_with("#") {
            if let Some(v) = self.vars.get(&expr.right_side.str[1..]) {
                value.extend(v);
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(&expr.right_side))
            }
        } else {
            value.push(expr.right_side);
        };

        let Some(decl) = self.vars.get_mut(&key) else {
            self.report_err(Error::new(UndefinedVariable, None), Some(&expr.left_side))
        };

        match expr.operation {
            Operation::PlusEqual => {
                decl.extend(value);
            }
            Operation::MinusEqual => {
                *decl = take(decl).into_iter().filter(|token| {
                    token.str != expr.right_side.str
                }).collect::<Vec::<_>>();
            }
            _ => todo!()
        }
    }

    pub fn parse(&mut self) -> Result::<Jobs, Error> {
        use {
            ErrorType::*,
            ParsingSection::*
        };

        let mut jobs = Vec::with_capacity(self.items.len() / 2);
        for item in std::mem::take(&mut self.items).into_iter() {
            match item {
                Item::If(if_) => {
                    let lv = self.get_value_(if_.left_side);
                    let rv = self.get_value_(if_.right_side);

                    let mut bool = lv.eq(&rv);
                    if if_.rev { bool = !bool; };
                    let tokens = if bool {
                        if_.body
                    } else {
                        if_.else_body
                    };

                    for item in tokens {
                        match item {
                            Item::Decl(decl) => {
                                self.eval_decl(&decl);
                            },
                            Item::Expr(expr) => if expr.left_side.str.starts_with("#") {
                                self.parse_expr(&expr);
                            } else {
                                println!("{:#?}", expr.left_side);
                                panic!("In-place math expressions are not supported yet BRUH")
                            },
                            _ => todo!()
                        }
                    }
                }
                Item::Decl(decl) => {
                    self.eval_decl(&decl);
                },
                Item::Expr(expr) => if expr.left_side.str.starts_with("#") {
                    self.parse_expr(&expr);
                } else {
                    println!("{:#?}", expr.left_side);
                    panic!("In-place math expressions are not supported yet BRUH")
                },
                Item::Job(job) => {
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

                    let job = CmdJob::new(target, deps, body);
                    jobs.push(job);
                }
            }
        }

        if Self::detect_cycle(&jobs) {
            eprintln!("[ERROR] Infinite dependency cycle detected, aborting..");
            exit(1);
        }

        Ok(jobs)
    }
}
