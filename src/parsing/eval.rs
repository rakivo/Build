use crate::{
    panic,
    execution::cmd::{Jobs, Job as CmdJob, Execute},
    parsing::{
        parser::{IFEQ,  IFNEQ, IFDEF, IFNDEF},
        lexer::{Loc, Token, Tokens, TokenType},
    },
};

use std::{
    env,
    fmt,
    slice::Iter,
    process::exit,
    collections::{HashMap, HashSet},
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
    Plus,
    PlusEqual,

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
    #[inline]
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
    target: &'a [Token<'a>],
    dependencies: &'a [Token<'a>],
    body: Vec::<&'a Tokens<'a>>
}

impl<'a> Job<'a> {
    #[inline]
    pub fn new(target: &'a [Token<'a>],
               dependencies: &'a [Token<'a>],
               body: Vec::<&'a Vec::<Token<'a>>>)
        -> Self
    {
        Self { target, dependencies, body }
    }
}

impl fmt::Display for Job<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: ", self.target)?;
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
    #[inline]
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

#[derive(Default)]
struct CurrJob<'a> {
    target: Option::<&'a String>,
    dependencies: Option::<&'a Vec::<String>>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct If<'a> {
    kind: IfKind,
    left_side: &'a Token<'a>,
    right_side: Option::<&'a Token<'a>>,
    body: Vec::<Item<'a>>,
    else_body: Vec::<Item<'a>>
}

impl<'a> If<'a> {
    #[inline]
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

pub type Items<'a> = Vec::<Item<'a>>;

#[derive(Default)]
pub struct Eval<'a> {
    pub jobs: Jobs,
    vars: HashMap::<&'a str, Vec::<String>>,
}

macro_rules! get_values {
    ($self: expr, $tokens: expr; $names: tt $(,)?; $str: expr, $loc: expr) => {{
        let len = $tokens.len();
        $tokens.into_iter().fold(Vec::with_capacity(len), |mut value, $names| {
            if !$str.starts_with("#") {
                value.push($str.to_owned());
                return value
            }
            value.extend($self.get_value__(&$str, &$loc));
            value
        })
    }};
}

impl<'a> Eval<'a> {
    const VARIABLE_SYMBOL: char = '#';
    const KEYWORDS: &'static [&'static str] = &[
        "shell",
        "addprefix",
        "vaddprefix"
    ];
    const PATTERNS: &'static [&'static str] = &[
        "<", "d",
        "@", "t",
        "^", "ds"
    ];

    #[track_caller]
    fn report_err(&self, err: Error, loc: Option::<&Loc>) -> ! {
        if let Some(loc) = loc {
            panic!("{loc}: [ERROR] {err}\n")
        } else {
            panic!("[ERROR] {err}\n")
        }
    }

    #[inline]
    fn keyword_check(str: &str) -> Option::<&&str> {
        Self::KEYWORDS.into_iter().find(|s| s == &&str)
    }

    #[inline]
    #[track_caller]
    fn handle_keyword_here() -> ! {
        panic!("HANDLE KEYWORD HERE\n")
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

    #[inline]
    fn get_name(token: &str) -> String {
        token.chars()
            .take_while(|c| c.is_alphabetic() || c.eq(&'_'))
            .collect::<String>()
    }

    fn get_parens_from_tokens(err_loc: &Loc, tokens: &[&Token]) -> (usize, usize) {
        let Some(lparen_idx) = tokens.iter().position(|token| matches!(token.typ, TokenType::LParen)) else {
            panic!("{err_loc} Expected lparen after `shell` keyword\n");
        };

        let Some(rparen_idx) = tokens.iter().position(|token| matches!(token.typ, TokenType::RParen)) else {
            panic!("{err_loc} Expected closing rparen after the lparen\n", err_loc = tokens[lparen_idx].loc);
        };

        if lparen_idx + 1 == rparen_idx {
            panic!("{err_loc} `shell` keyword with no commands inside\n");
        }

        (lparen_idx, rparen_idx)
    }

    fn eval_shell(&self, shell_loc: &Loc, tokens: &Vec::<&Token>, paren_idxs: Option::<(usize, usize)>) -> String {
        let (lparen_idx, rparen_idx) = paren_idxs.unwrap_or(Self::get_parens_from_tokens(shell_loc, tokens.as_slice()));
        let cmd = tokens[lparen_idx + 1..rparen_idx].into_iter().map(|t| {
            if t.str.starts_with(Self::VARIABLE_SYMBOL) {
                self.get_value__(t.str, &t.loc).join(" ")
            } else {
                t.str.to_owned()
            }
        }).collect::<Vec::<_>>().join(" ");

        Execute::execute_cmd(&cmd, true, false).unwrap_or_else(|_| {
            panic!("{shell_loc} Process exited with non-zero code\n")
        })
    }

    fn eval_addprefix(&self, addprefix_loc: &Loc, tokens: &Vec::<&Token>, paren_idxs: Option::<(usize, usize)>, vpref: bool) -> Vec::<String> {
        let (lparen_idx, rparen_idx) = paren_idxs.unwrap_or(Self::get_parens_from_tokens(addprefix_loc, tokens.as_slice()));
        let Some(prefix) = tokens.get(lparen_idx + 1) else {
            panic!("{addprefix_loc} `addprefix` without prefix\n")
        };

        let prefix_str = &prefix.str;
        tokens[lparen_idx + 1 + 1..rparen_idx].into_iter().map(|t| {
            if t.str.starts_with(Self::VARIABLE_SYMBOL) {
                if vpref {
                    format!("{prefix_str}{s}", s = self.get_value__(t.str, &t.loc).join(" "))
                } else {
                    self.get_value__(t.str, &t.loc).into_iter().map(|s| {
                        format!("{prefix_str}{s}")
                    }).collect::<Vec::<_>>().join(" ")
                }
            } else {
                format!("{prefix_str}{s}", s = t.str)
            }
        }).collect()
    }

    fn parse_decl(&mut self, decl: Decl<'a>) {
        use TokenType::{Plus, Minus};

        if self.vars.contains_key(&decl.left_side.str) {
            println!("{l} got redeclared", l = decl.left_side);
        }

        let Some(first) = decl.right_side.first() else {
            self.vars.insert(decl.left_side.str, Vec::new());
            return
        };

        match Self::keyword_check(first.str) {
            Some(&"shell") => {
                let value = self.eval_shell(&first.loc, &decl.right_side, None).lines()
                    .map(ToOwned::to_owned)
                    .collect();

                self.vars.insert(decl.left_side.str, value);
                return
            },
            Some(&"addprefix") | Some(&"vaddprefix") => {
                let value = self.eval_addprefix(&first.loc, &decl.right_side, None, first.str.eq("vaddprefix"));
                self.vars.insert(decl.left_side.str, value);
                return
            }
            Some(..) => Self::handle_keyword_here(),
            None => {}
        };

        let value = if decl.right_side.iter()
            .enumerate()
            .any(|(i, t)| {
                 matches!(t.typ, Plus | Minus)
              && matches!(decl.right_side.get(i + 1), Some(t) if t.str.starts_with("#"))
            })
        {
            let mut i = 0;
            let mut tokens = Vec::new();
            while i < decl.right_side.len() {
                let token = decl.right_side[i];
                let mut s = token.str.to_owned();
                if !token.str.starts_with("#") {
                    i += 1;
                    tokens.push((s, &token.loc));
                    continue
                }

                if let Some(sign) = decl.right_side.get(i + 1) {
                    match sign.typ {
                        Plus | Minus => if let Some(right_side) = decl.right_side.get(i + 2) {
                            s.push(if matches!(sign.typ, Plus) { '+' } else { '-' });
                            s.push_str(right_side.str);
                            i += 2;
                        } else {
                            panic!("No right side\n")
                        }
                        _ => {}
                    }
                }

                i += 1;
                tokens.push((s, &token.loc));
            }

            get_values!(self, tokens; (str, loc); str, loc)
        } else {
            get_values!(self, decl.right_side; token; token.str, token.loc)
        };

        self.vars.insert(decl.left_side.str, value);
    }

    fn parse_expr(&mut self, expr: &Expr<'a>) {
        use ErrorType::*;

        let key = if expr.left_side.str.ends_with("+") || expr.left_side.str.ends_with("-") {
            &expr.left_side.str[1..expr.left_side.str.len() - 1]
        } else {
            &expr.left_side.str[1..]
        };

        if !self.vars.contains_key(&key) {
            self.report_err(Error::new(UndefinedVariable, None), Some(&expr.left_side.loc))
        }

        let value = expr.right_side.iter()
            .fold(Vec::with_capacity(expr.right_side.len()),
                |mut value, token|
        {
            if token.str.starts_with("#") {
                value.extend(self.get_value__(token.str, &token.loc));
            } else {
                value.push(token.str.to_owned());
            } value
        });

        let decl = unsafe { self.vars.get_mut(&key).unwrap_unchecked() };
        match expr.operation {
            Operation::PlusEqual  => decl.extend(value),
            Operation::MinusEqual => decl.retain(|s| !expr.right_side.iter().any(|t| t.str.eq(s))),
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
                    env::set_var(export.str, v.join(" ").replace(" = ", "="));
                }
            } else {
                self.report_err(Error::new(UndefinedVariable, None), Some(&export.loc));
            }
        }
    }

    fn process_item(&mut self, item: Item<'a>) {
        match item {
            Item::If(r#if)            => self.process_if_tokens(r#if),
            Item::Decl(decl)          => self.parse_decl(decl),
            Item::Job(job)            => self.parse_job(job),
            Item::Export(exports)     => self.export_unexport(exports, false),
            Item::Unexport(unexports) => self.export_unexport(unexports, true),
            Item::Expr(expr)          => if expr.left_side.str.starts_with("#") {
                self.parse_expr(&expr);
            } else {
                panic!("In-place math expressions are not supported yet BRUH, or probably you forgot to put a `#` thingy\n")
            }
        }
    }

    fn process_if_tokens(&mut self, r#if: If<'a>) {
        for item in self.parse_if(r#if).into_iter() {
            self.process_item(item);
        }
    }

    #[inline]
    #[track_caller]
    fn unwrap_value<T>(&self, v: Option::<T>, loc: &Loc) -> T {
        v.unwrap_or_else(|| {
            self.report_err(Error::new(ErrorType::UndefinedVariable, None), Some(loc))
        })
    }

    #[inline]
    fn get_value_for_if_panic(&self, token: &'a Token) -> (Vec::<String>, bool) {
        use ErrorType::*;
        match self.get_value_for_if(token) {
            (Some(v), false) => (v, false),
            (Some(v), true)  => (v, true),

            (None,    false) => self.report_err(Error::new(UndefinedVariable, None), Some(&token.loc)),
            (None,    true)  => self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(&token.loc)),
        }
    }

    fn get_value_for_if(&self, token: &'a Token) -> (Option::<Vec::<String>>, bool) {
        use ErrorType::*;

        let name = if token.str.ends_with(",") {
            &token.str[..token.str.len() - 1]
        } else {
            &token.str
        };

        if token.str.starts_with(Self::VARIABLE_SYMBOL) {
            (Some(self.get_value__(name, &token.loc)), false)
        } else if token.str.starts_with("$") {
            if name[1..].is_empty() {
                self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(&token.loc));
            }

            if let Ok(value) = env::var(&name[1..]) {
                (Some(vec![value]), true)
            } else {
                (None, true)
            }
        } else {
            (Some(vec![name.to_owned()]), false)
        }
    }

    fn parse_if(&self, r#if: If<'a>) -> Vec::<Item<'a>> {
        let (lv, is_env) = self.get_value_for_if(r#if.left_side);
        let bool = match r#if.kind {
            IfKind::Eq | IfKind::Neq => {
                let lv = self.unwrap_value(lv, &r#if.left_side.loc);
                let rv = self.get_value_for_if_panic(unsafe { r#if.right_side.unwrap_unchecked() }).0;
                lv.eq(&rv)
            }
            IfKind::Def | IfKind::Ndef => {
                if lv.is_none() {
                    false
                } else if r#if.left_side.str.starts_with(Self::VARIABLE_SYMBOL) {
                    let lv = self.unwrap_value(lv, &r#if.left_side.loc);
                    !lv.into_iter().any(|x| !self.vars.contains_key(x.as_str()))
                } else {
                    is_env
                }
            }
        };

        let bool = if matches!(r#if.kind, IfKind::Neq | IfKind::Ndef) { !bool } else { bool };
        if bool {
            r#if.body
        } else {
            r#if.else_body
        }
    }

    fn get_value__(&self, str: &'a str, loc: &Loc) -> Vec::<String> {
        let mut last_pos = 0;
        let mut ret = str.split('#')
            .into_iter()
            .fold((
                Vec::<String>::new(),
                0,
                None::<Vec::<String>>,
                None::<Vec::<String>>,
            ), |(mut ret, count, add, sub), name_|
        // I have this count here to keep track of whether I am processing a variable or concatenating tokens.
        // In cases like: `echo #SRC_DIR##BIN_FILE#concat`, this function will output you the values of the two variables, which are `SRC_DIR` and `BIN_FILE`, the first `#` indicates the end of the first variable's name, and the second one indicates the start of another variable. If you didn't have the second `#`, the tokens following it would be treated as regular tokens, rather than a name of the variable.
        // The counter works the following way: If the current string is empty or it is a variable, you increment the counter, this lets you know whether to treat the tokens as tokens or as variable name.
        {
            if name_.is_empty() {
                last_pos += 1;
                return (ret, count + 1, None, None)
            }

            let name = Self::get_name(name_);
            last_pos += name.len() + 1;
            if count & 1 == 0 {
                unsafe { ret.last_mut().unwrap_unchecked() }.push_str(&name);
                return (ret, count, None, None)
            }

            fn op_check(s: &str) -> Option::<Operation> {
                if s.ends_with('+') {
                    Some(Operation::Plus)
                } else if s.ends_with('-') {
                    Some(Operation::Minus)
                } else { None }
            }

            let value = self.vars.get(name.as_str()).unwrap_or_else(|| {
                panic!("{loc}: [ERROR] Undefined variable\n")
            });

            let tokens = value.iter().map(ToString::to_string).collect::<Vec::<_>>();
            if let Some(mut add_ts) = add {
                let last = add_ts.last_mut().unwrap();
                last.push(' ');
                last.extend(tokens);
                ret.extend(add_ts);
                return (ret, count, None, None)
            } else if let Some(mut sub_ts) = sub {
                sub_ts.retain(|s| !tokens.contains(s));
                ret.extend(sub_ts);
                return (ret, count, None, None)
            }

            match op_check(name_) {
                Some(Operation::Plus)  => return (ret, count, Some(tokens), None),
                Some(Operation::Minus) => return (ret, count, None, Some(tokens)),
                _ => {}
            };

            if let Some(last) = ret.last_mut() {
                last.push_str(&tokens.join(" "));
            } else {
                ret.extend(tokens);
            }

            (ret, count + 1, None, None)
        }).0;

        if last_pos + 1 < str.len() {
            let s = &str[last_pos - 1..];
            if let Some(last) = ret.last_mut() {
                last.push_str(s);
            } else {
                ret.push(s.to_owned());
            }
        }

        ret
    }

    fn target_check_for_keyword(&self, target: &'a [Token]) -> Option::<String> {
        match Self::keyword_check(target[0].str) {
            Some(&"shell") => {
                Some(self.eval_shell(&target[0].loc, &target.iter().collect(), None))
            },
            Some(&"addprefix") | Some(&"vaddprefix") => {
                Some(self.eval_addprefix(&target[0].loc, &target.iter().collect(), None, target[0].str.eq("vaddprefix")).join(" "))
            }
            Some(..) => Self::handle_keyword_here(),
            None => None
        }
    }

    fn get_tokens_until_rparen_from_iter(t: &'a Token, iter: &mut Iter::<'a, Token>) -> Vec::<&'a Token<'a>> {
        let mut tokens = vec![t];
        while let Some(t) = iter.next() {
            tokens.push(t);
            if matches!(t.typ, TokenType::RParen) { break }
        } tokens
    }

    fn dependency_iter_check_for_keyword(&self, t: &'a Token, iter: &mut Iter::<'a, Token>) -> Option::<Vec::<String>> {
        match Self::keyword_check(t.str) {
            Some(&"shell") => {
                let tokens = Self::get_tokens_until_rparen_from_iter(t, iter);
                Some(self.eval_shell(&t.loc, &tokens, Some((0, tokens.len() - 1))).lines().map(ToOwned::to_owned).collect())
            },
            Some(&"addprefix") | Some(&"vaddprefix") => {
                let tokens = Self::get_tokens_until_rparen_from_iter(t, iter);
                let ret = self.eval_addprefix(&t.loc, &tokens, Some((0, tokens.len() - 1)), t.str.eq(""));
                Some(ret)
            }
            Some(..) => Self::handle_keyword_here(),
            None => None
        }
    }

    fn parse_job(&mut self, job: Job) {
        use ErrorType::*;

        const INVALID_DEP_PATTERN_NOTE: &'static str = "You can use \"$d\" and \"$<\" ONLY in body of job";
        const INVALID_TARGET_PATTERN_NOTE: &'static str = "You can use \"$t\" and \"$@\" either in body of a job, or in its dependencies";

        // Temp `Job` instance to keep track of target and dependencies to expand patterns like `$t`/`$@`, `$d`/`$<` etc...
        let mut curr_job = CurrJob::default();

        let target = if let Some(keyword_tokens) = self.target_check_for_keyword(job.target) {
            keyword_tokens
        } else if job.target[0].str.starts_with("$") {
            let name = &job.target[0].str[1..];
            let token_loc = &job.target[0].loc;

            if Self::PATTERNS.contains(&name) {
                match name {
                    "d"  | "<" => self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody, Some(INVALID_DEP_PATTERN_NOTE)), Some(&token_loc)),
                    "t"  | "@" => self.report_err(Error::new(UnexpectedTargetSpecialSymbolInTargetSection, Some(INVALID_TARGET_PATTERN_NOTE)), Some(&token_loc)),
                    "ds" | "^" => self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody, Some(INVALID_DEP_PATTERN_NOTE)), Some(&token_loc)),
                    _ => unreachable!()
                }
            } else if let Ok(value) = env::var(name) {
                value
            } else {
                self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(&token_loc))
            }
        } else if job.target[0].str.starts_with(Self::VARIABLE_SYMBOL) {
            let str = job.target.iter().map(|t| t.str).collect::<Vec::<_>>().join(" ");
            self.get_value__(&str, &job.target[0].loc).join(" ")
        } else {
            job.target.iter().map(|t| t.str).collect::<Vec::<_>>().join(" ")
        };

        curr_job.target = Some(&target);

        let mut iter = job.dependencies.into_iter();
        let mut deps = Vec::with_capacity(job.dependencies.len() / 2);
        while let Some(t) = iter.next() {
            let value = if let Some(keyword_tokens) = self.dependency_iter_check_for_keyword(t, &mut iter) {
                keyword_tokens
            } else if job.target[0].str.starts_with("$") {
                let name = &job.target[0].str[1..];
                let token_loc = &job.target[0].loc;

                if Self::PATTERNS.contains(&name) {
                    match name {
                        "t"  | "@" => vec![unsafe { curr_job.target.unwrap_unchecked() }.to_owned()],
                        "d"  | "<" => self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody, Some(INVALID_DEP_PATTERN_NOTE)), Some(&token_loc)),
                        "ds" | "^" => self.report_err(Error::new(UnexpectedDependencySpecialSymbolNotInBody, Some(INVALID_DEP_PATTERN_NOTE)), Some(&token_loc)),
                        _ => unreachable!()
                    }
                } else if let Ok(value) = env::var(name) {
                    vec![value]
                } else {
                    self.report_err(Error::new(UndefinedEnviromentVariable, None), Some(&token_loc))
                }
            } else {
                self.get_value__(t.str, &t.loc)
            };

            if value.iter().any(|s| target.eq(s)) {
                let msg = format!("Job \"{target}\" depends on itself, so, infinite recursion detected");
                let err = Error::new(JobDependsOnItself, Some(&msg));
                self.report_err(err, Some(&t.loc));
            }

            deps.extend(value);
        }

        curr_job.dependencies = Some(&deps);

        fn is_line_silent(line: &Vec::<String>) -> bool {
            matches!(line.first(), Some(t) if t.starts_with("@"))
        }

        let body = job.body.into_iter().fold(Vec::new(), |mut body, line| {
            let mut line = line.into_iter().map(|t| {
                if t.str.starts_with(Self::VARIABLE_SYMBOL) {
                    self.get_value__(t.str, &t.loc).join(" ")
                } else {
                    t.str.to_owned()
                }
            }).map(|mut s| {
                s = s.replace("$t", unsafe { curr_job.target.unwrap_unchecked() });
                if s.contains("$d") {
                    s.replace("$d", unsafe { curr_job.dependencies.unwrap_unchecked() }.first().unwrap_or_else(|| self.report_err(Error::new(UnexpectedDependencySpecialSymbolWhileNoDependencies, None), Some(&job.target[0].loc))))
                } else { s }
            }).collect::<Vec::<_>>();

            // let mut line = line.iter().map(|t| self.get_value_for_job(t.str, &t.loc, Body, &curr_job).join(" ")).collect();
            let silent = is_line_silent(&line);
            if silent { line[0].remove(0); }
            body.push((silent, line));
            body
        });

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
