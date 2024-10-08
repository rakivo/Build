use std::{
    fmt,
    str,
    path::PathBuf,
    vec::IntoIter,
    iter::{Peekable, Enumerate},
};

pub type ColStr<'a> = (usize, &'a str);
pub type Tokens<'a> = Vec::<Token<'a>>;
pub type LinizedTokens<'a> = Vec::<(usize, Tokens<'a>)>;
pub type Lines<'a> = Peekable<Enumerate<str::Lines<'a>>>;

#[derive(Debug, Clone)]
pub enum TokenType {
    Char,
    String,
    Literal,

    Equal,

    Plus,
    PlusEqual,
    Minus,
    MinusEqual,

    LParen,
    RParen,

    Colon,
}

#[derive(Clone)]
pub struct Loc<'a>(pub &'a PathBuf, pub usize, pub usize);

impl fmt::Display for Loc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{f}:{r}:{c}", f = self.0.display(), r = self.1 + 1, c = self.2 + 1)
    }
}

impl fmt::Debug for Loc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub struct Token<'a> {
    pub wc:  usize,
    pub typ: TokenType,
    pub loc: Loc<'a>,
    pub str: &'a str,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{l}: {t:?}: \"{s}\"", l = self.loc, t = self.typ, s = self.str)
    }
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'a> Token<'a> {
    #[inline]
    pub fn new(wc: usize, typ: TokenType, loc: Loc<'a>, str: &'a str) -> Self {
        Self { wc, typ, loc, str }
    }
}

pub struct Lexer<'a> {
    row: usize,
    iter: Lines<'a>,
    file_path: &'a PathBuf,
}

impl<'a> Lexer<'a> {
    const COMMENT_SYMBOL: u8 = b';';

    #[inline]
    pub fn new(file_path: &'a PathBuf, content: &'a str) -> Self {
        Self { file_path, row: 0, iter: content.lines().enumerate().peekable() }
    }

    pub fn lex_line(&mut self, line: ColStr<'a>, ts: &mut LinizedTokens<'a>) {
        use TokenType::*;

        let (wc, strs) = Self::split_whitespace_preserve_indices(line.1);

        let len = strs.len();
        if len < 1 {
            self.row += 1;
            return
        }

        let mut iter = strs.into_iter().peekable();

        // The whole line is a comment
        if matches!(iter.peek(), Some((.., s)) if s.bytes().next().eq(&Some(Self::COMMENT_SYMBOL))) {
            self.row += 1;
            return
        }

        let mut line_ts = Tokens::with_capacity(len);
        while let Some((col, s)) = iter.next() {
            let mut bytes = s.bytes();
            let Some(first) = bytes.next() else { continue };
            let second = bytes.next();
            let tt = match first {
                b'\'' => Char,
                b'"'  => String,
                b'('  => LParen,
                b')'  => RParen,
                b':'  => Colon,
                b'='  => Equal,
                b'-'  => if second.eq(&Some(b'=')) {
                    MinusEqual
                } else { Minus },
                b'+'  => if second.eq(&Some(b'=')) {
                    PlusEqual
                } else { Plus },
                Self::COMMENT_SYMBOL => {
                    ts.push((wc, line_ts));
                    self.row += 1;
                    return
                },
                _ => Literal,
            };

            let loc = Loc(self.file_path, self.row, col);
            line_ts.push(Token::new(wc, tt, loc, s));
        }

        ts.push((wc, line_ts));
        self.row += 1;
    }

    pub fn lex(&mut self) -> std::io::Result::<LinizedTokens> {
        let mut ts = LinizedTokens::new();
        while let Some(line) = self.iter.next() {
            self.lex_line(line, &mut ts);
        }
        Ok(ts)
    }

    const SPLIT_CHARS: &'static [char] = &['(', ')', '=', ':'];

    fn split_whitespace_preserve_indices(input: &str) -> (usize, IntoIter::<ColStr>) {
        let (s, e, mut ret) = input.char_indices().fold((0, 0, Vec::with_capacity(input.len())),
            |(s, e, mut ret), (i, c)|
        {
            let iss = Self::SPLIT_CHARS.contains(&c);
            if c.is_whitespace() || iss {
                if s != i {
                    ret.push((s, &input[s..i]));
                }
                if iss {
                    ret.push((s + 1, &input[i..=i]));
                }
                (i + c.len_utf8(), e, ret)
            } else {
                (s, i + c.len_utf8(), ret)
            }
        });

        if s != e {
            ret.push((s, &input[s..]));
        }

        let wc = input.chars()
            .take_while(|x| x.is_whitespace())
            .count();

        (wc, ret.into_iter())
    }
}
