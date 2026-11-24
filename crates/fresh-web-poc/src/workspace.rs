//! A workspace: a root folder on a host ("authority") that serves its files,
//! directories, processes and terminals.
//!
//! Only the local host is implemented. Everything that touches the host goes
//! through [`Authority`], so an SSH or container host is another
//! implementation, not a change to the callers.

use anyhow::{anyhow, Result};
use serde_json::{json, Value};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::UNIX_EPOCH;

pub trait Authority: Send + Sync {
    /// A short name for the host, shown to the user.
    fn name(&self) -> String;
    fn list_dir(&self, path: &Path) -> Result<Vec<Value>>;
    fn stat(&self, path: &Path) -> Value;
    fn read_file(&self, path: &Path, max: u64) -> Result<String>;
    fn write_file(&self, path: &Path, text: &str) -> Result<()>;
    /// A command to run on the host, before it is started.
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command;
    /// The interactive shell a terminal runs, to be started on a PTY. A
    /// remote host returns e.g. `ssh -t host ...`, a devcontainer
    /// `docker exec -it <container> ...`; the PTY and its byte stream are the
    /// same either way.
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder;
}

pub struct Local;

fn mtime(m: &std::fs::Metadata) -> u64 {
    m.modified()
        .ok()
        .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

impl Authority for Local {
    fn name(&self) -> String {
        "local".into()
    }

    fn list_dir(&self, path: &Path) -> Result<Vec<Value>> {
        let mut out = Vec::new();
        for e in std::fs::read_dir(path)? {
            let e = e?;
            let p = e.path();
            let meta = std::fs::metadata(&p).ok();
            out.push(json!({
                "name": e.file_name().to_string_lossy(),
                "path": p.to_string_lossy(),
                "dir": meta.as_ref().is_some_and(|m| m.is_dir()),
                "size": meta.as_ref().map(|m| m.len()).unwrap_or(0),
            }));
        }
        out.sort_by(|a, b| {
            let ad = a["dir"].as_bool().unwrap_or(false);
            let bd = b["dir"].as_bool().unwrap_or(false);
            bd.cmp(&ad).then_with(|| {
                a["name"]
                    .as_str()
                    .unwrap_or("")
                    .to_lowercase()
                    .cmp(&b["name"].as_str().unwrap_or("").to_lowercase())
            })
        });
        Ok(out)
    }

    fn stat(&self, path: &Path) -> Value {
        match std::fs::metadata(path) {
            Ok(m) => json!({
                "exists": true,
                "dir": m.is_dir(),
                "size": m.len(),
                "mtime": mtime(&m),
            }),
            Err(_) => json!({ "exists": false }),
        }
    }

    fn read_file(&self, path: &Path, max: u64) -> Result<String> {
        let m = std::fs::metadata(path)?;
        if m.len() > max {
            return Err(anyhow!("file too large to read whole; open it as a document"));
        }
        Ok(String::from_utf8_lossy(&std::fs::read(path)?).into_owned())
    }

    fn write_file(&self, path: &Path, text: &str) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(path, text)?;
        Ok(())
    }

    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command {
        let mut c = Command::new(program);
        c.args(args).current_dir(cwd);
        c
    }

    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder {
        let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".into());
        let mut c = portable_pty::CommandBuilder::new(shell);
        c.cwd(cwd);
        c.env("TERM", "xterm-256color");
        c.env("COLORTERM", "truecolor");
        c
    }
}

pub struct Workspace {
    pub root: PathBuf,
    pub authority: Box<dyn Authority>,
}

impl Workspace {
    /// Resolve a client-supplied path: absolute as given, relative to the
    /// root otherwise.
    pub fn resolve(&self, p: &str) -> PathBuf {
        let p = Path::new(p);
        if p.is_absolute() {
            p.to_path_buf()
        } else {
            self.root.join(p)
        }
    }

    /// Files under the root whose relative path contains every character of
    /// `query` in order, best (shortest) first. Skips VCS and build folders.
    pub fn find_files(&self, query: &str, limit: usize) -> Vec<String> {
        const SKIP: &[&str] = &[".git", "target", "node_modules", ".hg", ".svn"];
        let q: Vec<char> = query.to_lowercase().chars().filter(|c| !c.is_whitespace()).collect();
        let mut hits: Vec<(usize, String)> = Vec::new();
        let mut stack = vec![self.root.clone()];
        let mut seen = 0usize;
        while let Some(dir) = stack.pop() {
            let Ok(rd) = std::fs::read_dir(&dir) else { continue };
            for e in rd.flatten() {
                let name = e.file_name().to_string_lossy().into_owned();
                let p = e.path();
                let Ok(ft) = e.file_type() else { continue };
                if ft.is_dir() {
                    if !SKIP.contains(&name.as_str()) {
                        stack.push(p);
                    }
                    continue;
                }
                seen += 1;
                if seen > 200_000 {
                    break;
                }
                let rel = p
                    .strip_prefix(&self.root)
                    .unwrap_or(&p)
                    .to_string_lossy()
                    .into_owned();
                let lower = rel.to_lowercase();
                let mut it = lower.chars();
                if q.iter().all(|c| it.any(|x| x == *c)) {
                    hits.push((rel.len(), rel));
                }
            }
        }
        hits.sort();
        hits.into_iter().take(limit).map(|(_, r)| r).collect()
    }
}
