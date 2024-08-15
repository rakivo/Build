use std::{
    fs::{
        metadata,
    },
    path::PathBuf,
    time::SystemTime,
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

    fn needs_rebuild_many(bin: &str, srcs: &Vec::<String>) -> std::io::Result::<bool> {
        // I collect times on purpose to check if all of the src files exist,
        // to catch unexisting dependencies at `compile time` whether the bin path exists or not.

        let mut times = Vec::new();
        for src in srcs.iter() {
            let time = Self::get_last_modification_time(src)?;
            times.push(time)
        }

        if !Self::path_exists(bin) { return Ok(true) }

        let bin_mod_time = Self::get_last_modification_time(bin)?;
        Ok(times.into_iter().any(|src_mod_time| src_mod_time > bin_mod_time))
    }

    pub fn execute(&mut self) -> std::io::Result::<()> {
        for job in self.jobs.iter() {
            if Self::needs_rebuild_many(&job.target, &job.dependencies)? {
                println!("{}", job.target);
            }
        }

        Ok(())
    }
}
