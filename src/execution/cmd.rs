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
    fn path_exists<P>(p: P) -> bool
    where
        P: Into::<PathBuf>
    {
        p.into().exists()
    }

    #[inline]
    fn get_last_modification_time<'a>(s: &str) -> std::io::Result::<SystemTime> {
        metadata::<PathBuf>(s.into()).map_err(|err| {
            eprintln!("[ERROR] Failed to get last modification time of \"{s}\", apparently it does not exist");
            err
        })?.modified()
    }

    fn needs_rebuild_many(&self, bin: &str, srcs: &Vec::<String>) -> bool {
        let mut times = Vec::with_capacity(srcs.len());
        for src in srcs.iter() {
            if let Some(job) = self.jobs.iter().find(|j| j.target.eq(src)) {
                if self.needs_rebuild_many(&job.target, &job.dependencies) {
                    self.execute_job(job);
                } else {
                    println!("Nothing to do for \"{job}\"", job = job.target);
                }
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

    fn execute_job(&self, job: &Job) {
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
    }

    pub fn execute(&mut self) -> std::io::Result::<()> {
        let job = if let Some(j) = self.jobs.iter().find(|job| job.target.eq("all")) {
            j
        } else if let Some(j) = self.jobs.first() {
            j
        } else { return Ok(()) };

        if self.needs_rebuild_many(&job.target, &job.dependencies) {
            self.execute_job(job);
        } else {
            println!("Nothing to do for \"{job}\"", job = job.target);
        }

        Ok(())
    }
}
