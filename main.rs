mod parser;
mod lexer;

use lexer::*;
use parser::*;

use std::{
    env,
    fs::read_to_string
};

fn main() -> std::io::Result::<()> {
    let args = env::args().collect::<Vec::<_>>();
    if args.len() < 2 {
        panic!("Usage: `{program}` <file_path>", program = args[0]);
    }

    let file_path = &args[1];
    let content = read_to_string(file_path)?;
    let mut lexer = Lexer::new(file_path, &content);
    let tokens = lexer.lex()?;

    // println!("{tokens:#?}");
    // tokens.iter().for_each(|line| {
    //     line.iter().for_each(|t| println!("{t}"));
    //     println!();
    // });

    let mut parser = Parser::new(&tokens);
    parser.parse();

    Ok(())
}
