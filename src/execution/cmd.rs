use std::{
    fs::metadata,
    path::PathBuf,
    time::SystemTime,
    process::{
        exit,
        Command
    },
};

#[derive(Debug)]
pub struct Job {
    target: String,
    dependencies: Vec::<String>,
    #[allow(unused)]
    body: Vec::<Vec::<String>>,
}

impl Job {
    pub fn new(target: String,
               dependencies: Vec::<String>,
               body: Vec::<Vec::<String>>)
        -> Self
    {
        Self { target, dependencies, body }
    }
}

pub type Jobs = Vec::<Job>;

pub struct Execute {
    jobs: Jobs,
}

impl Execute {
    #[inline]
    pub fn new(jobs: Jobs) -> Self {
        Self { jobs }
    }

    #[inline]
    fn path_exists(p: &str) -> bool {
        let p: PathBuf = p.into();
        p.exists()
    }

    #[inline]
    fn get_last_modification_time(s: &str) -> std::io::Result::<SystemTime> {
        metadata::<PathBuf>(s.into()).map_err(|err| {
            eprintln!("[ERROR] Failed to get last modification time of \"{s}\", apparently it does not exist");
            err
        })?.modified()
    }

    #[inline]
    fn nothing_to_do_for(what: &str) {
        println!("Nothing to do for \"{what}\"");
    }

    fn needs_rebuild_many(&self, bin: &str, srcs: &Vec::<String>) -> bool {
        let mut times = Vec::with_capacity(srcs.len());
        for src in srcs.iter() {
            if let Some(job) = self.jobs.iter().find(|j| j.target.eq(src)) {
                self.execute_job_if_needed(job);
            } else {
                times.push(Self::get_last_modification_time(src).unwrap());
            }
        }

        if !Self::path_exists(bin) { return true }

        let bin_mod_time = Self::get_last_modification_time(bin).unwrap();
        times.into_iter().any(|src_mod_time| src_mod_time > bin_mod_time)
    }

    #[inline]
    fn render_cmd(cmd: &Vec::<String>) -> String {
        cmd.join(" ")
    }

    pub const CMD_ARG:  &'static str = if cfg!(windows) {"cmd"} else {"sh"};
    pub const CMD_ARG2: &'static str = if cfg!(windows) {"/C"} else {"-c"};

    fn execute_job_if_needed(&self, job: &Job) {
        if self.needs_rebuild_many(&job.target, &job.dependencies) {
            for line in job.body.iter() {
                let rendered = Self::render_cmd(line);
                println!("{rendered}");

                let out = Command::new(Self::CMD_ARG).arg(Self::CMD_ARG2)
                    .arg(rendered)
                    .output()
                    .expect("Failed to execute process");

                if let Some(code) = out.status.code() {
                    if code != 0 {
                        if !out.stderr.is_empty() {
                            eprint!("{stderr}", stderr = String::from_utf8_lossy(&out.stderr));
                        }

                        eprintln!("Process exited abnormally with code {code}");
                        exit(1);
                    }
                }

                if !out.stdout.is_empty() {
                    eprint!("{stdout}", stdout = String::from_utf8_lossy(&out.stdout));
                }
            }
        } else {
            Self::nothing_to_do_for(&job.target);
        }
    }

    pub fn execute(&mut self) -> std::io::Result::<()> {
        let job = self.jobs.first().unwrap_or_else(|| exit(0));
        self.execute_job_if_needed(job);
        Ok(())
    }
}
