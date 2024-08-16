use crate::{
    new_flag,
    execution::flag::*,
};

pub struct Flags {
    // <Buildfile> [-j, --job] <name_of_job>
    pub job: Option::<String>,

    // <Buildfile> [-C, --directory] <path_to_env_dir>
    pub env_dir: Option::<String>,

    // <Buildfile> [-B, --always-make]
    pub phony: bool
}

pub fn parse_flags() -> Flags {
    let parser = Parser::new();

    let job_flag: Flag::<String> = new_flag!("-j", "--job");
    let env_dir_flag: Flag::<String> = new_flag!("-C", "--directory");
    let phony_flag: Flag::<bool> = new_flag!("-B", "--always-make");

    Flags {
        job: parser.parse(&job_flag),
        env_dir: parser.parse(&env_dir_flag),
        phony: parser.passed(&phony_flag),
    }
}
