use crate::{
    new_flag,
    parsing::flag::*
};

use std::process::exit;

pub struct Flags {
    pub job: Option::<String>,
    pub env_dir: Option::<String>,
    pub phony: bool,
    pub silent: bool,
    pub keepgoing: bool
}

pub fn parse_flags() -> Flags {
    let parser = Parser::new();

    let help_flag: Flag::<bool> = new_flag!("-h", "--help", "help flag");

    let job_flag:       Flag::<String> = new_flag!("-j", "--job", "name of a specific job to execute");
    let env_dir_flag:   Flag::<String> = new_flag!("-C", "--directory", "path to a specific directory to `cd` into before executing jobs");
    let phony_flag:     Flag::<bool>   = new_flag!("-B", "--always-make", "always rebuild the job, regardless of whether it needs to be rebuilt");
    let silent_flag:    Flag::<bool>   = new_flag!("-s", "--silent", "disable cmd echo");
    let keepgoing_flag: Flag::<bool>   = new_flag!("-k", "--keep-going", "don't exit if process exited with non-zero code");

    if parser.passed(&help_flag) {
        println!("Usage: build [options] ...");
        println!("Options:");
        println!("  {job_flag}");
        println!("  {env_dir_flag}");
        println!("  {phony_flag}");
        println!("  {silent_flag}");
        println!("  {keepgoing_flag}");
        exit(0);
    }

    Flags {
        job:       parser.parse(&job_flag),
        env_dir:   parser.parse(&env_dir_flag),
        phony:     parser.passed(&phony_flag),
        silent:    parser.passed(&silent_flag),
        keepgoing: parser.passed(&keepgoing_flag),
    }
}
