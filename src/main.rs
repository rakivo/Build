mod parsing;
use parsing::{flags::parse_flags, lexer::Lexer, parser::Parser};

mod execution;
use execution::{
    cmd::{find_buildfile, Execute},
};

#[cfg(feature = "dbg")]
use std::time::Instant;
use std::{env, fs::read_to_string, process::exit};

#[macro_export]
macro_rules! panic {
    ($($tt: tt)*) => {
        if cfg!(debug_assertions) {
            std::panic!($($tt)*)
        } else {
            print!($($tt)*);
            std::process::exit(1);
        }
    };
}

fn main() -> std::io::Result<()> {
    #[cfg(feature = "dbg")]
    let parser_start = Instant::now();

    let flags = parse_flags();
    if let Some(dir) = flags.env_dir.as_ref() {
        println!("Entering directory \"{dir}\"");
        if let Err(err) = env::set_current_dir(dir) {
            eprintln!("Failed to cd to \"{dir}\": {err}");
            exit(1);
        }
    }

    let file_path = find_buildfile()?;
    let content = read_to_string(&file_path)?;
    let mut lexer = Lexer::new(&file_path, &content);
    let tokens = lexer.lex()?;

    let mut parser = Parser::new(&tokens);
    parser.parse();

    #[cfg(feature = "dbg")]
    {
        let end_time = parser_start.elapsed().as_micros();
        println!("Parsing done in {end_time}us");
    }

    #[cfg(feature = "dbg")]
    let eval_start = Instant::now();

    parser.eval.parse(parser.items);
    let jobs = parser.eval.jobs;

    #[cfg(feature = "dbg")]
    {
        let end_time = eval_start.elapsed().as_micros();
        println!("Evaling done in {end_time}us");
    }

    let mut execute = Execute::new(jobs, flags);
    execute.execute()?;

    Ok(())
}
