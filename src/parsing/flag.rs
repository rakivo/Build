// This is a modified version of crate that is available on github, so, you can probably take a closer look into it: <https://github.com/rakivo/flag>

use std::{
    env,
    ops::Range,
    str::FromStr,
    path::PathBuf,
    process::exit
};

pub trait TryParse
where
    Self: Sized
{
    fn parse(_: &Parser, f: &Flag::<Self>) -> Option::<Self>;
    fn parse_many(_: &Parser, f: &Flag::<Self>, nargs: NArgs) -> Option::<Vec::<Self>>;
}

#[inline]
fn parse<T>(parser: &Parser, flag: &Flag::<T>) -> Option::<String> {
    parser.splitted.iter().skip_while(|x| x != &flag.short && x != &flag.long).skip(1).next().cloned()
}

fn parse_many<T>(parser: &Parser, flag: &Flag::<T>, nargs: NArgs) -> Option::<Vec::<String>> {
    let mut iter = parser.splitted.iter().cloned().skip_while(|x| x != &flag.short && x != &flag.long).skip(1);
    if let Some(v) = iter.next() {
        let mut ret = vec![v];
        match nargs {
            NArgs::Count(count)   => ret.extend(iter.take(count - 1)),
            NArgs::Remainder      => ret.extend(iter),
            NArgs::SmartRemainder => ret.extend(iter.take_while(|x| !x.starts_with("-")))
        }
        Some(ret)
    } else {
        None
    }
}

impl TryParse for () {
    #[inline(always)]
    fn parse(_: &Parser, _: &Flag::<()>) -> Option::<Self> {
        println!("[WARN] Calling `parse()` on `()` type");
        None
    }

    #[inline(always)]
    fn parse_many(_: &Parser, _: &Flag::<()>, _: NArgs) -> Option::<Vec::<Self>> {
        println!("[WARN] Calling `parse()` on `()` type");
        None
    }
}

impl TryParse for String {
    #[inline(always)]
    fn parse(parser: &Parser, flag: &Flag::<String>) -> Option::<Self> {
        parse(parser, flag)
    }

    #[inline(always)]
    fn parse_many(parser: &Parser, flag: &Flag::<String>, nargs: NArgs) -> Option::<Vec::<Self>> {
        parse_many(parser, flag, nargs)
    }
}

impl TryParse for PathBuf {
    #[inline(always)]
    fn parse(parser: &Parser, flag: &Flag::<PathBuf>) -> Option::<Self> {
        parse(parser, flag).map(PathBuf::from)
    }

    #[inline(always)]
    fn parse_many(parser: &Parser, flag: &Flag::<PathBuf>, nargs: NArgs) -> Option::<Vec::<Self>> {
        parse_many(parser, flag, nargs).map(|v| v.into_iter().map(PathBuf::from).collect())
    }
}


impl TryParse for bool {
    #[inline(always)]
    fn parse(parser: &Parser, flag: &Flag::<bool>) -> Option::<Self> {
        Some(parser.passed(flag))
    }

    #[inline(always)]
    fn parse_many(parser: &Parser, flag: &Flag::<bool>, _: NArgs) -> Option::<Vec::<Self>> {
        Some(vec![parser.passed(flag)])
    }
}

fn parse_int<T>(x: &str) -> T
where
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display,
{
    x.parse::<_>().map_err(|err| {
        eprintln!("ERROR: Failed to convert `{x}` to integer: {err}");
        exit(1)
    }).unwrap()
}

fn parse_range<T>(x: &str) -> Range::<T>
where
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display,
{
    let splitted = x.split("..").collect::<Vec::<_>>();
    if splitted.len() != 2 {
        eprintln!("ERROR: Failed to convert `{x}` to range, expected value like: `69..69`");
        exit(1)
    }
    let start = parse_int(splitted[0]);
    let end = parse_int(splitted[1]);
    start..end
}

impl TryParse for isize {
    #[inline]
    fn parse(parser: &Parser, flag: &Flag::<isize>) -> Option::<Self> {
        parse(parser, flag).map(|x| parse_int(&x))
    }

    #[inline]
    fn parse_many(parser: &Parser, flag: &Flag::<isize>, nargs: NArgs) -> Option::<Vec::<Self>> {
        parse_many(parser, flag, nargs).map(|v| v.into_iter().map(|x| parse_int(&x)).collect())
    }
}

impl TryParse for Range::<isize> {
    #[inline]
    fn parse(parser: &Parser, flag: &Flag::<Range::<isize>>) -> Option::<Self> {
        parse(parser, flag).map(|x| parse_range(&x))
    }

    #[inline]
    fn parse_many(parser: &Parser, flag: &Flag::<Range::<isize>>, nargs: NArgs) -> Option::<Vec::<Self>> {
        parse_many(parser, flag, nargs).map(|v| v.into_iter().map(|x| parse_range(&x)).collect())
    }
}

macro_rules! impl_try_parse {
    ($($t: ty) *) => {
        $(
            impl TryParse for $t {
                #[inline]
                fn parse(parser: &Parser, flag: &Flag::<$t>) -> Option::<Self> {
                    isize::parse(parser, &Flag::<_> {
                        short: flag.short,
                        long: flag.long,
                        help: flag.help,
                        mandatory: flag.mandatory,
                        default: flag.default.map(|x| x as _),
                        description: flag.description
                    }).map(|x| x as _)
                }

                #[inline]
                fn parse_many(parser: &Parser, flag: &Flag::<$t>, nargs: NArgs) -> Option::<Vec::<Self>> {
                    isize::parse_many(parser, &Flag::<_> {
                        short: flag.short,
                        long: flag.long,
                        help: flag.help,
                        mandatory: flag.mandatory,
                        default: flag.default.map(|x| x as _),
                        description: flag.description
                    }, nargs).map(|x| x.into_iter().map(|x| x as _).collect())
                }
            }

            impl TryParse for Range::<$t> {
                #[inline]
                fn parse(parser: &Parser, flag: &Flag::<Range::<$t>>) -> Option::<Self> {
                    Range::<isize>::parse(parser, &Flag::<_> {
                        short: flag.short,
                        long: flag.long,
                        help: flag.help,
                        mandatory: flag.mandatory,
                        default: flag.default.as_ref().map(|x| Range {
                            start: x.start as _,
                            end: x.end as _,
                        }),
                        description: flag.description
                    }).map(|x| Range {
                        start: x.start as _,
                        end: x.end as _,
                    })
                }

                #[inline]
                fn parse_many(parser: &Parser, flag: &Flag::<Range::<$t>>, nargs: NArgs) -> Option::<Vec::<Self>> {
                    Range::<isize>::parse_many(parser, &Flag::<_> {
                        short: flag.short,
                        long: flag.long,
                        help: flag.help,
                        mandatory: flag.mandatory,
                        default: flag.default.as_ref().map(|x| Range {
                            start: x.start as _,
                            end: x.end as _,
                        }),
                        description: flag.description
                    }, nargs).map(|x| x.into_iter().map(|x| Range {
                        start: x.start as _,
                        end: x.end as _,
                    }).collect())
                }
            }
        ) *
    }
}

impl_try_parse! { i8 i16 i32 i128 u8 u16 u32 u64 u128 usize }

#[allow(unused)]
#[derive(Clone)]
pub enum NArgs {
    Remainder,
    Count(usize),
    SmartRemainder,
}

pub struct Flag<T = ()> {
    short: &'static str,
    long: &'static str,
    help: Option::<&'static str>,
    mandatory: bool,
    default: Option::<T>,
    description: &'static str
}

#[macro_export]
macro_rules! new_flag {
    ($short: literal, $long: literal, $description: literal) => {
        Flag::new($short, $long, None, $description)
    };
    ($short: literal, $long: literal, $def: expr, $description: literal) => {
        Flag::new($short, $long, Option::Some($def), $description)
    };
}

impl<T> std::fmt::Display for Flag<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const DPAD: usize = 20;
        let flags = format!("[{s}, {l}]", s = self.short, l = self.long);
        write!(f, "{flags} {}{}", " ".repeat(DPAD - (flags.len())), self.description)
    }
}

/// I separated moving and borrowing methods to conveniently create flags in one line, e.g.
/// ```
/// let flag = Flag::<i32>::new("-f", "--flag").mandatory().help("test");
/// ```
/// If these functions did not move the flag into them, you would encounter a CE: error[E0716]: temporary value dropped while borrowed.
#[allow(unused)]
impl<T> Flag<T>
where
    T: TryParse
{
    pub const fn new(short: &'static str, long: &'static str, default: Option::<T>, description: &'static str) -> Self {
        Self {
            short,
            long,
            help: None,
            mandatory: false,
            default,
            description
        }
    }

    #[inline(always)]
    pub const fn help(mut self, help: &'static str) -> Self {
        self.help = Some(help); self
    }

    #[inline(always)]
    pub fn help_borrow(&mut self, help: &'static str) -> &mut Self {
        self.help = Some(help); self
    }

    #[inline(always)]
    pub const fn mandatory(mut self) -> Self {
        self.mandatory = true; self
    }

    #[inline(always)]
    pub fn mandatory_borrow(&mut self) -> &mut Self {
        self.mandatory = true; self
    }

    #[inline(always)]
    pub fn default(mut self, v: T) -> Self {
        self.default = Some(v); self
    }

    #[inline(always)]
    pub fn default_borrow(&mut self, v: T) -> &mut Self {
        self.default = Some(v); self
    }
}

pub struct Parser {
    #[allow(unused)]
    string: String,
    splitted: Vec::<String>,
}

#[allow(unused)]
impl Parser {
    #[inline]
    pub fn new() -> Self {
        let splitted = env::args().collect::<Vec::<_>>();
        Self {
            string: splitted.join(" "),
            splitted
        }
    }

    /// Panics when flag is mandatory and it's not provided.
    #[inline]
    fn mandatory_check<T>(&self, f: &Flag::<T>) {
        if f.mandatory && !self.passed(f) {
            eprintln!("ERROR: neither `{s}` nor `{l}` flag is passed", s = f.short, l = f.long);
            exit(1)
        }
    }

    /// Tries to find specific flag in `std::env::args()` and convert it to T
    ///
    /// Panics when flag is mandatory and it's not provided or failed to convert passed argument to T.
    #[inline]
    pub fn parse<T>(&self, f: &Flag::<T>) -> Option::<T>
    where
        T: TryParse
    {
        self.mandatory_check(f);
        T::parse(self, f)
    }

    #[inline]
    pub fn parse_many<T>(&self, f: &Flag::<T>, nargs: NArgs) -> Option::<Vec::<T>>
    where
        T: TryParse
    {
        self.mandatory_check(f);
        T::parse_many(self, f, nargs)
    }

    /// Returns result of `T::parse()` or flag.default in case if `T::parse()` failed.
    /// If flag.default is None, returns T::default.
    ///
    /// Panics when flag is mandatory and it's not provided.
    #[inline]
    pub fn parse_or_default<T>(&self, f: &Flag::<T>) -> T
    where
        T: TryParse + Default + Clone
    {
        self.mandatory_check(f);
        T::parse(self, f).unwrap_or(f.default.to_owned().unwrap_or_default())
    }

    /// Checks if specific flag is provided
    #[inline]
    pub fn passed<T>(&self, f: &Flag::<T>) -> bool {
        self.splitted.iter().any(|x| x == f.short || x == f.long)
    }
}
