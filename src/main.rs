mod parsing;
mod execution;

use parsing::{
    lexer::Lexer,
    parser::Parser
};

use execution::{
    flags::parse_flags,
    cmd::{
        Execute,
        find_rakefile
    },
};

use std::{
    env,
    process::exit,
    fs::read_to_string
};

fn main() -> std::io::Result::<()> {
    let flags = parse_flags();
    if let Some(dir) = flags.env_dir.as_ref() {
        println!("Entering directory \"{dir}\"");
        if let Err(err) = env::set_current_dir(dir) {
            eprintln!("Failed to cd to \"{dir}\": {err}");
            exit(1);
        }
    }

    let file_path = find_rakefile()?;
    let content = read_to_string(&file_path)?;
    let mut lexer = Lexer::new(&file_path, &content);
    let tokens = lexer.lex()?;

    let mut parser = Parser::new(&tokens);
    let jobs = parser.parse().unwrap();

    let mut execute = Execute::new(jobs, flags);
    execute.execute()?;

    Ok(())
}
