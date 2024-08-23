mod parsing;
use parsing::{lexer::Lexer, parser::Parser};

mod execution;
use execution::{
    flags::parse_flags,
    cmd::{find_buildfile, Execute},
};

#[cfg(feature = "dbg")]
use std::time::Instant;
use std::{env, fs::read_to_string, process::exit};

fn main() -> std::io::Result<()> {
    #[cfg(feature = "dbg")]
    let start_time = Instant::now();

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
    parser.ast.parse(parser.items);
    let jobs = parser.ast.jobs;

    #[cfg(feature = "dbg")]
    {
        let end_time = start_time.elapsed().as_micros();
        println!("Parsing done in {end_time}us");
    }

    let mut execute = Execute::new(jobs, flags);
    execute.execute()?;

    Ok(())
}
