use crate::{
    parsing::{
        parser::{
            IFEQ, IFNEQ,
            IFDEF, IFNDEF,
        },
        lexer::{
            Token,
            Tokens
        },
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

impl fmt::Debug for Decl<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Decl<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{l} =", l = self.left_side.str)?;
        for t in self.right_side.iter() {
            write!(f, " {s}", s = t.str)?
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

pub struct Expr<'a> {
    pub left_side: &'a Token<'a>,
    pub operation: Operation,
    pub right_side: &'a [Token<'a>],
}

impl<'a> Expr<'a> {
    #[inline]
    pub fn new(left_side: &'a Token<'a>,
               operation: Operation,
               right_side: &'a [Token<'a>])
        -> Self
    {
        Self { left_side, operation, right_side }
    }
}

impl fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{l} {o:?} {r:?}", l = self.left_side.str, o = self.operation, r = self.right_side)
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
        write!(f, "{t}: ", t = self.target.str)?;
        for t in self.dependencies {
            write!(f, "{s} ", s = t.str)?;
        }

        writeln!(f)?;
        for line in self.body.iter() {
            write!(f, "   ")?;
            for t in line.iter() {
                write!(f, " {s}", s = t.str)?
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

impl fmt::Debug for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

pub struct Error<'a> {
    ty: ErrorType,
    note: Option::<&'a str>,
}

impl<'a> Error<'a> {
    #[inline]
    pub fn new(ty: ErrorType,
               note: Option::<&'a str>)
       -> Self
    {
        Self { ty, note }
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = &self.ty;
        if let Some(note) = self.note {
            write!(f, "{ty}\n\tNOTE: {note}")
        } else {
            write!(f, "{ty}")
        }
    }
}

pub type Export<'a> = &'a [Token<'a>];
pub type Unexport<'a> = &'a [Token<'a>];

#[derive(Debug)]
pub enum Item<'a> {
    If(If<'a>),
    Job(Job<'a>),
    Decl(Decl<'a>),
    Expr(Expr<'a>),
    Export(Export<'a>),
    Unexport(Unexport<'a>)
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

pub enum IfKind {
    Eq, Neq, Def, Ndef
}

impl TryFrom::<&str> for IfKind {
    type Error = ();

    fn try_from(s: &str) -> Result::<Self, Self::Error> {
        match s {
            IFEQ   => Ok(Self::Eq),
            IFNEQ  => Ok(Self::Neq),
            IFDEF  => Ok(Self::Def),
            IFNDEF => Ok(Self::Ndef),
            _      => Err(())
        }
    }
}

impl ToString for IfKind {
    fn to_string(&self) -> String {
        let s = match self {
            Self::Eq   => IFEQ,
            Self::Neq  => IFNEQ,
            Self::Def  => IFDEF,
            Self::Ndef => IFNDEF,
        };
        s.to_owned()
    }
}

pub struct If<'a> {
    kind: IfKind,
    left_side: &'a Token<'a>,
    right_side: Option::<&'a Token<'a>>,
    body: Vec::<Item<'a>>,
    else_body: Vec::<Item<'a>>
}

impl<'a> If<'a> {
    pub fn new(kind: IfKind,
               left_side: &'a Token<'a>,
               right_side: Option::<&'a Token<'a>>,
               body: Vec::<Item<'a>>,
               else_body: Vec::<Item<'a>>)
       -> Self
    {
        Self { kind, left_side, right_side, body, else_body }
    }
}

impl fmt::Debug for If<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for If<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r#if = self.kind.to_string();
        match self.kind {
            IfKind::Eq | IfKind::Neq   => writeln!(f, "{if} {l} {r}", l = self.left_side.str, r = self.right_side.unwrap().str)?,
            IfKind::Def | IfKind::Ndef => writeln!(f, "{if} {l}", l = self.left_side.str)?,
        };

        for item in self.body.iter() {
            write!(f, "\t{item:?}")?;
        }

        writeln!(f)?;
        if !self.else_body.is_empty() {
            writeln!(f, "else")?;
            for item in self.else_body.iter() {
                write!(f, "\t{item:?}")?;
            }
        }

        write!(f, "\nendif")
    }
}

pub type Items<'a> = Vec::<Item<'a>>;

#[derive(Default)]
pub struct Ast<'a> {
    pub jobs: Jobs,
    vars: HashMap::<&'a str, Vec::<&'a Token<'a>>>,
}

impl<'a> Ast<'a> {
    const VARIABLE_SYMBOL: char = '#';
    const SPECIAL_NAMES: &'static [&'static str] = &[
        "<", "d",
        "@", "t",
        "^", "ds"
    ];

    #[track_caller]
    fn report_err(&self, err: Error, token: Option::<&'a Token<'a>>) -> ! {
        if let Some(errt) = token {
            panic!("{errt}: [ERROR] {err}")
        } else {
            panic!("[ERROR] {err}")
        }
    }

    fn detect_cycle(&self) -> bool {
        let mut visited = HashSet::with_capacity(self.jobs.len());
        let mut in_rec_stack = HashSet::with_capacity(self.jobs.len());
        let job_map = HashMap::from_iter(self.jobs.iter().map(|job| (job.target.as_str(), job)).into_iter());

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

        let trimmed = if token.str.ends_with(",") {
            &token.str[..token.str.len() - 1]
        } else {
            &token.str
        };

        if token.str.starts_with(Self::VARIABLE_SYMBOL) {
            if trimmed[1..].is_empty() {
                self.report_err(Error::new(UndefinedVariable, None), Some(token));
            }

            if let Some(value) = self.vars.get(&trimmed[1..]) {
                value.iter().map(|x| x.str).collect::<Vec::<_>>().join(" ")
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(token))
            }
        } else if token.str.starts_with("$") {
            if trimmed[1..].is_empty() {
                self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(token));
            }

            if let Ok(value) = env::var(&trimmed[1..]) {
                value
            } else {
                self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(token))
            }
        } else {
            trimmed.to_owned()
        }
    }

    fn eval_decl(&mut self, decl: Decl<'a>) {
        use ErrorType::*;

        if self.vars.contains_key(&decl.left_side.str) {
            println!("{l} redeclared", l = decl.left_side);
        }

        if let Some(token) = decl.right_side.first() {
            if !token.str.starts_with("#") {
                self.vars.insert(decl.left_side.str, decl.right_side);
                return
            }

            if let Some(value) = self.vars.get(&token.str[1..]) {
                let mut v = decl.right_side;
                v.extend(value);
                self.vars.insert(decl.left_side.str, v);
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(token))
            }
        } else {
            self.vars.insert(decl.left_side.str, decl.right_side);
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
        for token in expr.right_side {
            if token.str.starts_with("#") {
                if let Some(v) = self.vars.get(&token.str[1..]) {
                    value.extend(v);
                } else {
                    self.report_err(Error::new(UndefinedVariable, None), Some(&token))
                }
            } else {
                value.push(token);
            };
        }

        let Some(decl) = self.vars.get_mut(&key) else {
            self.report_err(Error::new(UndefinedVariable, None), Some(&expr.left_side))
        };

        match expr.operation {
            Operation::PlusEqual  => decl.extend(value),
            Operation::MinusEqual => *decl = take(decl).into_iter().filter(|token| {
                expr.right_side.iter().any(|t| t.str != token.str)
            }).collect::<Vec::<_>>(),
            _ => todo!()
        }
    }

    fn export_unexport(&self, exports: Export, un: bool) {
        use ErrorType::*;

        for export in exports {
            if let Some(v) = self.vars.get(export.str) {
                if un {
                    env::remove_var(export.str);
                } else {
                    env::set_var(export.str, v.iter().map(|t| t.str).collect::<Vec::<_>>().join(" "));
                }
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(export));
            }
        }
    }

    fn process_item(&mut self, item: Item<'a>) {
        match item {
            Item::If(r#if)            => self.process_if_tokens(r#if),
            Item::Decl(decl)          => self.eval_decl(decl),
            Item::Job(job)            => self.parse_job(job),
            Item::Export(exports)     => self.export_unexport(exports, false),
            Item::Unexport(unexports) => self.export_unexport(unexports, true),
            Item::Expr(expr)          => if expr.left_side.str.starts_with("#") {
                self.parse_expr(&expr);
            } else {
                panic!("In-place math expressions are not supported yet BRUH")
            }
        }
    }

    fn process_if_tokens(&mut self, r#if: If<'a>) {
        for item in self.parse_if(r#if).into_iter() {
            self.process_item(item);
        }
    }

    fn parse_if(&self, r#if: If<'a>) -> Vec::<Item<'a>> {
        let lv = self.get_value_(r#if.left_side);
        match r#if.kind {
            IfKind::Eq | IfKind::Neq => {
                let rv = self.get_value_(r#if.right_side.unwrap());
                let mut bool = lv.trim().eq(rv.trim());
                if matches!(r#if.kind, IfKind::Neq) { bool = !bool; };
                if bool {
                    r#if.body
                } else {
                    r#if.else_body
                }
            }
            IfKind::Def | IfKind::Ndef => {
                let mut bool = self.vars.contains_key(&lv.as_str());
                if matches!(r#if.kind, IfKind::Ndef) { bool = !bool; };
                if bool {
                    r#if.body
                } else {
                    r#if.else_body
                }
            }
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
                match name {
                    "d" | "<" => if matches!(section, Dependencies | Target) {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody,
                                                   Some("You can use \"$d\" and \"$<\" ONLY in body of job")), Some(token));
                    } else if let Some(dep) = curr_job.dependencies.unwrap().first() {
                        dep.to_owned()
                    } else {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolWhileNoDependencies, None), Some(token));
                    },

                    "ds" | "^" => if matches!(section, Dependencies | Target) {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody,
                                                   Some("You can use \"$d\" and \"$<\" ONLY in body of job")), Some(token));
                    } else if !curr_job.dependencies.unwrap().is_empty() {
                        curr_job.dependencies.unwrap().join(" ")
                    } else {
                        self.report_err(Error::new(UnexpectedDependencySpecialSymbolWhileNoDependencies, None), Some(token));
                    },

                    "t" | "@" => if matches!(section, Target) {
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

    fn parse_job(&mut self, job: Job) {
        use {
            ErrorType::*,
            ParsingSection::*
        };

        let mut curr_job = CurrJob::default();
        let target = self.get_value(job.target, Target, &curr_job);
        curr_job.target = Some(&target);

        let deps = job.dependencies.iter().fold(Vec::with_capacity(job.dependencies.len()),
            |mut deps, t|
        {
            let value = self.get_value(t, Dependencies, &curr_job);
            if target.eq(&value) {
                let msg = format!("Job \"{target}\" depends on itself, so, infinite recursion detected");
                let err = Error::new(JobDependsOnItself, Some(&msg));
                self.report_err(err, Some(t));
            }
            deps.push(value);
            deps
        });

        curr_job.dependencies = Some(&deps);
        let body = job.body.iter().map(|line| {
            line.iter().map(|t| self.get_value(t, Body, &curr_job)).collect::<Vec::<_>>()
        }).collect::<Vec::<_>>();

        let job = CmdJob::new(target, deps, body);
        self.jobs.push(job);
    }

    pub fn parse(&mut self, items: Items<'a>) {
        for item in items.into_iter() {
            self.process_item(item);
        }

        if self.detect_cycle() {
            eprintln!("[ERROR] Infinite dependency cycle detected, aborting..");
            exit(1);
        }
    }
}
