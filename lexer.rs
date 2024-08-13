use std::{
    fmt,
    str,
    vec::IntoIter,
    iter::{
        Peekable,
        Enumerate
    },
};

pub type ColStr<'b> = (usize, &'b str);
pub type Tokens<'a> = Vec::<Token<'a>>;
pub type LinizedTokens<'a> = Vec::<Tokens<'a>>;
pub type Lines<'a> = Peekable<Enumerate<str::Lines<'a>>>;

#[derive(Debug)]
pub enum TokenType {
    Char,
    String,
    Literal,

    At,
    Equal,

    Plus,
    PlusEqual,
    Minus,
    MinusEqual,

    LCurly,
    RCurly,

    Colon,
}

pub struct Loc<'a>(pub &'a str, pub usize, pub usize);

impl fmt::Display for Loc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{f}:{r}:{c}", f = self.0, r = self.1 + 1, c = self.2 + 1)
    }
}

pub struct Token<'a> {
    pub typ: TokenType,
    pub loc: Loc<'a>,
    pub str: &'a str,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{l}: {t:?}: {s}", l = self.loc, t = self.typ, s = self.str)
    }
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'a> Token<'a> {
    pub fn new(typ: TokenType, loc: Loc<'a>, str: &'a str) -> Self {
        Self { typ, loc, str }
    }
}

pub struct Lexer<'a> {
    row: usize,
    iter: Lines<'a>,
    file_path: &'a str
}

impl<'a> Lexer<'a> {
    pub fn new(file_path: &'a str, content: &'a str) -> Self {
        Self { file_path, row: 0, iter: content.lines().enumerate().peekable() }
    }

    pub fn lex_line(&mut self, line: ColStr<'a>, ts: &mut LinizedTokens<'a>) {
        use TokenType::*;

        let strs = Self::get_strs(line.1);
        let len = strs.len();
        if len < 1 {
            self.row += 1;
            return
        }

        let mut iter = strs.into_iter().peekable();
        if matches!(iter.peek(), Some((_, s)) if s.bytes().next().eq(&Some(b'#'))) {
            self.row += 1;
            return
        }

        let mut line_ts = Tokens::with_capacity(len);
        while let Some((col, s)) = iter.next() {
            let mut bytes = s.bytes();
            let Some(first) = bytes.next() else { continue };
            let second = bytes.next();
            let tt = match first {
                b'#'  => {
                    ts.push(line_ts);
                    self.row += 1;
                    return
                },
                b'\'' => Char,
                b'"'  => String,
                b'-'  => if second.eq(&Some(b'=')) {
                    MinusEqual
                } else { Minus },
                b'+'  => if second.eq(&Some(b'=')) {
                    PlusEqual
                } else { Plus },
                b'{'  => LCurly,
                b'}'  => RCurly,
                b'@'  => At,
                b':'  => Colon,
                b'='  => Equal,
                _     => Literal,
            };

            let loc = Loc(self.file_path, self.row, col);
            let t = Token::new(tt, loc, s);
            line_ts.push(t);
        }

        ts.push(line_ts);
        self.row += 1;
    }

    pub fn lex(&mut self) -> std::io::Result::<LinizedTokens> {
        let mut ts = LinizedTokens::new();
        while let Some(line) = self.iter.next() {
            self.lex_line(line, &mut ts);
        }
        Ok(ts)
    }

    const CHAR_LIST: &'static [char] = &[
        '=', '"', '\'', '#', '+', '-', '{', '}', ':', '@'
    ];

    fn get_strs(input: &str) -> IntoIter::<ColStr> {
        let (s, e, mut ret) = input.char_indices().fold((0, 0, Vec::with_capacity(input.len())),
            |(s, e, mut ret), (i, c)|
        {
            let isw = c.is_whitespace();
            if isw || Self::CHAR_LIST.contains(&c) {
                if s != i {
                    ret.push((s, &input[s..i]));
                }
                if isw {
                    (i + c.len_utf8(), e, ret)
                } else {
                    ret.push((i, &input[i..i + c.len_utf8()]));
                    (i + c.len_utf8(), e, ret)
                }
            } else {
                (s, i + c.len_utf8(), ret)
            }
        });

        if s != e {
            ret.push((s, &input[s..]));
        }

        ret.into_iter()
    }
}
