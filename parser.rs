use crate::{
    lexer::{
        Token,
        Tokens,
        TokenType,
        LinizedTokens,
    }
};

use std::slice::Iter;

pub type LinizedTokensIterator<'a> = Iter::<'a, Tokens<'a>>;

pub struct Parser<'a> {
    iter: LinizedTokensIterator<'a>,
    err_token: Option::<&'a Token<'a>>,
}

#[allow(unused)]
const KEYWORDS: &'static [&'static str] = &[
];

impl<'a> Parser<'a> {
    pub fn new(ts: &'a LinizedTokens<'a>) -> Self {
        Self {
            iter: ts.into_iter(),
            err_token: None
        }
    }

    fn report_err(&mut self, fmt: &str) -> ! {
        if let Some(errt) = self.err_token {
            panic!("{errt}: [ERROR] {fmt}")
        } else {
            panic!("[ERROR] {fmt}")
        }
    }

    // Unexpected First Token error
    #[inline]
    fn uft_err(&mut self, line: &'a Tokens) -> ! {
        self.err_token = line.get(0);
        self.report_err("UNEXPECTED TOKEN")
    }

    fn parse_line(&mut self, line: &'a Tokens) {
        use TokenType::*;

        let mut iter = line.into_iter().peekable();
        let Some(ref first) = iter.peek() else { return };
        match first.typ {
            Literal => if let Some(eq_idx) = line.iter().position(|x| matches!(x.typ, Equal)) {
                if eq_idx > 1 {
                    self.err_token = Some(first);
                    self.report_err("EXPECTED ONLY ONE TOKEN ON THE LEFT SIDE");
                }
            } else if let Some(colon_idx) = line.iter().position(|x| matches!(x.typ, Colon)) {
                if colon_idx > 1 {
                    self.err_token = Some(first);
                    self.report_err("EXPECTED ONLY ONE TOKEN ON THE LEFT SIDE");
                }
            } else {
                self.uft_err(line);
            },
            _ => todo!()
            // String     => {},
            // At         => {},
            // Equal      => {},
            // Plus       => {},
            // PlusEqual  => {},
            // Minus      => {},
            // MinusEqual => {},
            // LCurly     => {},
            // RCurly     => {},
            // Colon      => {},
        };
    }

    pub fn parse(&mut self) {
        while let Some(line) = self.iter.next() {
            self.parse_line(line);
        }
    }
}
