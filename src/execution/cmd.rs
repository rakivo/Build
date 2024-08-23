use std::{
    env,
    path::PathBuf,
    time::SystemTime,
    fs::{
        read_dir,
        metadata,
    },
    process::{
        exit,
        Command
    },
};

const BUILD_FILE_NAME: &'static str = "Buildfile";

use crate::execution::flags::Flags;

pub type Body = Vec::<(bool, Vec::<String>)>;

#[derive(Debug)]
pub struct Job {
    pub target: String,
    pub dependencies: Vec::<String>,
    body: Body
}

impl Job {
    #[inline]
    pub fn new(target: String,
               dependencies: Vec::<String>,
               body: Body)
        -> Self
    {
        Self { target, dependencies, body }
    }
}

#[derive(Debug)]
pub struct Dir {
    paths: Vec::<PathBuf>,
}

impl Dir {
    pub fn new(root: &PathBuf) -> Dir {
        Dir {
            paths: if let Ok(entries) = read_dir::<&PathBuf>(root) {
                entries.into_iter()
                    .filter_map(Result::ok)
                    .filter(|e| e.path().is_file())
                    .map(|e| e.path())
                    .collect()
            } else {
                Vec::new()
            }
        }
    }
}

impl IntoIterator for Dir {
    type Item = PathBuf;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.paths.into_iter()
    }
}

pub fn find_buildfile() -> std::io::Result::<PathBuf> {
    let dir_path = env::current_dir()?;
    let buildfile = Dir::new(&dir_path).into_iter()
        .find(|f| matches!(f.file_name(), Some(name) if name == BUILD_FILE_NAME))
        .unwrap_or_else(|| {
            println!("No `{BUILD_FILE_NAME}` found in {dir}", dir = dir_path.display());
            exit(1);
        });

    Ok(buildfile)
}

pub type Jobs = Vec::<Job>;

pub struct Execute {
    jobs: Jobs,
    flags: Flags,
}

impl Execute {
    #[inline]
    pub fn new(jobs: Jobs, flags: Flags) -> Self {
        Self { jobs, flags }
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

    fn needs_rebuild(&self, job: &Job) -> bool {
        // If phony flag: `-B` is passed, we need to rebuild everything and only after that return true
        if self.flags.phony {
            job.dependencies.iter().for_each(|dep| {
                if let Some(job) = self.jobs.iter().find(|j| j.target.eq(dep)) {
                    self.execute_job_if_needed(job);
                }
            });

            return true
        }

        let times = job.dependencies.iter().fold(Vec::with_capacity(job.dependencies.len()),
            |mut times, dep|
        {
            // If current job depends on other job, the other job will be executed, recursively.
            if let Some(job) = self.jobs.iter().find(|j| j.target.eq(dep)) {
                self.execute_job_if_needed(job);
            } else {
                let time = Self::get_last_modification_time(dep).unwrap();
                times.push(time);
            } times
        });

        if !Self::path_exists(&job.target) { return true }

        let target_mod_time = unsafe { Self::get_last_modification_time(&job.target).unwrap_unchecked() };
        times.into_iter().any(|dep_mod_time| dep_mod_time > target_mod_time)
    }

    #[inline]
    fn render_cmd(cmds: &[String]) -> String {
        cmds.iter().fold(String::new(), |mut ret, cmd| {
            ret.push_str(&cmd.replace(" = ", "="));
            ret.push(' ');
            ret
        })
    }

    pub const CMD_ARG:  &'static str = if cfg!(windows) {"cmd"} else {"sh"};
    pub const CMD_ARG2: &'static str = if cfg!(windows) {"/C"} else {"-c"};

    fn execute_job_if_needed(&self, job: &Job) {
        if !self.needs_rebuild(&job) {
            return Self::nothing_to_do_for(&job.target)
        }

        for (silent, line) in job.body.iter() {
            let rendered = Self::render_cmd(&line);
            if !self.flags.silent && !silent {
                println!("{rendered}");
            }

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
                    if !self.flags.keepgoing { exit(1) }
                }
            }

            if !out.stdout.is_empty() {
                eprint!("{stdout}", stdout = String::from_utf8_lossy(&out.stdout));
            }
        }
    }

    pub fn execute(&mut self) -> std::io::Result::<()> {
        let job = if let Some(job_target) = self.flags.job.as_ref() {
            self.jobs.iter().find(|j| j.target.eq(job_target)).unwrap_or_else(|| {
                eprintln!("No job with target \"{job_target}\" found");
                exit(1)
            })
        } else {
            self.jobs.first().unwrap_or_else(|| exit(0))
        };

        self.execute_job_if_needed(job);
        Ok(())
    }
}
