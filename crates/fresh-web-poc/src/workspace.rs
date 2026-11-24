//! A workspace: a root folder on a host ("authority") that serves its files,
//! directories, processes and terminals.
//!
//! Every file operation goes through the authority's [`FileSystem`] — the
//! editor's own filesystem abstraction — whether the host is this machine
//! (`StdFileSystem`) or another one reached over SSH (`RemoteFileSystem`, the
//! editor's agent-based remote filesystem). Documents read through it lazily,
//! a range at a time, so a 100 MB file over a slow link costs the windows
//! actually viewed. Listing, finding and searching run through it too (on the
//! host's side for a remote one), so there is one code path for every host.

use anyhow::{anyhow, Result};
use fresh_editor_core::model::filesystem::{
    EntryType, FileSearchOptions, FileSystem, StdFileSystem, WalkOptions,
};
use fresh_editor_core::model::host_path::HostPathBuf;
use serde_json::{json, Value};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::UNIX_EPOCH;

pub type Fs = Arc<dyn FileSystem + Send + Sync>;

/// Folders never walked: version control internals and build output.
const SKIP_DIRS: &[&str] = &[".git", "target", "node_modules", ".hg", ".svn"];

pub trait Authority: Send + Sync {
    /// A short name for the host, shown to the user.
    fn name(&self) -> String;
    /// The host's filesystem.
    fn fs(&self) -> Fs;
    /// A command to run on the host, before it is started.
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command;
    /// The interactive shell a terminal runs, to be started on a PTY.
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder;
}

/// This machine.
pub struct Local {
    pub fs: Fs,
}

impl Local {
    pub fn new() -> Self {
        Local {
            fs: Arc::new(StdFileSystem),
        }
    }
}

fn login_shell() -> String {
    std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".into())
}

impl Authority for Local {
    fn name(&self) -> String {
        "local".into()
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command {
        let mut c = Command::new(program);
        c.args(args).current_dir(cwd);
        c
    }
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder {
        let mut c = portable_pty::CommandBuilder::new(login_shell());
        c.cwd(cwd);
        c.env("TERM", "xterm-256color");
        c.env("COLORTERM", "truecolor");
        c
    }
}

/// Quote a word for a POSIX shell.
fn sh_quote(s: &str) -> String {
    if !s.is_empty()
        && s.bytes()
            .all(|b| b.is_ascii_alphanumeric() || b"-_./=:@%+,".contains(&b))
    {
        s.to_string()
    } else {
        format!("'{}'", s.replace('\'', "'\\''"))
    }
}

/// Another machine over SSH: files through the editor's remote agent (one
/// persistent connection, ranged reads, newline counting and searching on
/// the remote side); processes and terminals through `ssh`.
pub struct Ssh {
    pub target: String,
    pub extra: Vec<String>,
    pub fs: Fs,
    /// Keeps the connection's async tasks alive.
    pub _runtime: tokio::runtime::Runtime,
    pub _conn: fresh::services::remote::SshConnection,
}

impl Authority for Ssh {
    fn name(&self) -> String {
        format!("ssh {}", self.target)
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command {
        let mut line = format!(
            "cd {} && exec {}",
            sh_quote(&cwd.to_string_lossy()),
            sh_quote(program)
        );
        for a in args {
            line.push(' ');
            line.push_str(&sh_quote(a));
        }
        let mut c = Command::new("ssh");
        c.args(&self.extra)
            .arg("-o")
            .arg("BatchMode=yes")
            .arg(&self.target)
            .arg(line);
        c
    }
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder {
        let mut c = portable_pty::CommandBuilder::new("ssh");
        c.args(&self.extra);
        c.arg("-t");
        c.arg(&self.target);
        c.arg(format!(
            "cd {} && exec ${{SHELL:-/bin/sh}} -l",
            sh_quote(&cwd.to_string_lossy())
        ));
        c.env("TERM", "xterm-256color");
        c
    }
}

/// The editor's remote agent run on this machine, without ssh: the whole
/// remote path (agent protocol, ranged reads, remote-side counting and
/// searching) with only the transport left out. Processes and terminals run
/// locally.
pub struct Agent {
    pub fs: Fs,
    pub _runtime: tokio::runtime::Runtime,
}

impl Authority for Agent {
    fn name(&self) -> String {
        "agent".into()
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command {
        Local::new().command(program, args, cwd)
    }
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder {
        Local::new().shell_command(cwd)
    }
}

pub struct Workspace {
    pub root: PathBuf,
    pub authority: Arc<dyn Authority>,
}

pub fn host(p: &Path) -> HostPathBuf {
    HostPathBuf::from_path_buf(p.to_path_buf())
}

fn millis(t: Option<std::time::SystemTime>) -> u64 {
    t.and_then(|t| t.duration_since(UNIX_EPOCH).ok())
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

impl Workspace {
    pub fn fs(&self) -> Fs {
        self.authority.fs()
    }

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

    pub fn list_dir(&self, path: &Path) -> Result<Vec<Value>> {
        let mut out: Vec<Value> = self
            .fs()
            .read_dir(host(path).as_host_path())?
            .into_iter()
            .map(|e| {
                let dir = matches!(e.entry_type, EntryType::Directory) || e.symlink_target_is_dir;
                json!({
                    "name": e.name,
                    "path": PathBuf::from(e.path).to_string_lossy(),
                    "dir": dir,
                    "size": e.metadata.as_ref().map(|m| m.size).unwrap_or(0),
                })
            })
            .collect();
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

    pub fn stat(&self, path: &Path) -> Value {
        let fs = self.fs();
        let p = host(path);
        match fs.metadata(p.as_host_path()) {
            Ok(m) => json!({
                "exists": true,
                "dir": fs.is_dir(p.as_host_path()).unwrap_or(false),
                "size": m.size,
                "mtime": millis(m.modified),
                "readonly": m.is_readonly,
            }),
            Err(_) => json!({ "exists": false }),
        }
    }

    pub fn read_file(&self, path: &Path, max: u64) -> Result<String> {
        let fs = self.fs();
        let p = host(path);
        if fs.metadata(p.as_host_path())?.size > max {
            return Err(anyhow!(
                "file too large to read whole; open it as a document"
            ));
        }
        Ok(String::from_utf8_lossy(&fs.read_file(p.as_host_path())?).into_owned())
    }

    pub fn write_file(&self, path: &Path, text: &str) -> Result<()> {
        let fs = self.fs();
        if let Some(parent) = path.parent() {
            fs.create_dir_all(host(parent).as_host_path())?;
        }
        fs.write_file(host(path).as_host_path(), text.as_bytes())?;
        Ok(())
    }

    pub fn exists(&self, p: &Path) -> bool {
        self.fs().exists(host(p).as_host_path())
    }

    pub fn is_dir(&self, p: &Path) -> bool {
        self.fs().is_dir(host(p).as_host_path()).unwrap_or(false)
    }

    /// A new empty file, or a folder (with its parents).
    pub fn create(&self, p: &Path, dir: bool) -> Result<()> {
        let fs = self.fs();
        if dir {
            fs.create_dir_all(host(p).as_host_path())?;
        } else {
            if self.exists(p) {
                return Err(anyhow!("{} exists", p.display()));
            }
            self.write_file(p, "")?;
        }
        Ok(())
    }

    pub fn rename(&self, from: &Path, to: &Path) -> Result<()> {
        if self.exists(to) {
            return Err(anyhow!("{} exists", to.display()));
        }
        self.fs()
            .rename(host(from).as_host_path(), host(to).as_host_path())?;
        Ok(())
    }

    /// Delete a file, or a folder and everything in it.
    pub fn remove(&self, p: &Path) -> Result<()> {
        let fs = self.fs();
        if self.is_dir(p) {
            for e in fs.read_dir(host(p).as_host_path())? {
                self.remove(&PathBuf::from(e.path))?;
            }
            fs.remove_dir(host(p).as_host_path())?;
        } else {
            fs.remove_file(host(p).as_host_path())?;
        }
        Ok(())
    }

    /// Copy a file or folder into `to` (a folder: into it), never over an
    /// existing name. Returns where it went.
    pub fn copy(&self, from: &Path, to: &Path) -> Result<PathBuf> {
        let fs = self.fs();
        let mut to = if self.is_dir(to) {
            to.join(from.file_name().unwrap_or_default())
        } else {
            to.to_path_buf()
        };
        while self.exists(&to) {
            let stem = to
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned();
            let ext = to
                .extension()
                .map(|e| format!(".{}", e.to_string_lossy()))
                .unwrap_or_default();
            to = to.with_file_name(format!("{stem} copy{ext}"));
        }
        if self.is_dir(from) {
            fs.copy_dir_all(host(from).as_host_path(), host(&to).as_host_path())?;
        } else {
            fs.copy(host(from).as_host_path(), host(&to).as_host_path())?;
        }
        Ok(to)
    }

    /// Where the server keeps a document's recovery data, on the host.
    pub fn recovery_dir(&self) -> PathBuf {
        let fs = self.fs();
        let base: PathBuf = fs
            .home_dir()
            .map(PathBuf::from)
            .unwrap_or_else(|_| self.root.clone());
        let d = base.join(".local/share/fresh-poc/recovery");
        let _ = fs.create_dir_all(host(&d).as_host_path());
        d
    }

    /// The modification signature (size, mtime) of a path, for watching it.
    pub fn signature(&self, p: &Path) -> Option<(u64, u64)> {
        let m = self.fs().metadata(host(p).as_host_path()).ok()?;
        Some((m.size, millis(m.modified)))
    }

    /// The root's ignore rules (`.gitignore`, `.git/info/exclude`), read
    /// through the filesystem so a remote workspace's rules apply too.
    fn ignore_rules(&self) -> ignore::gitignore::Gitignore {
        let mut b = ignore::gitignore::GitignoreBuilder::new(&self.root);
        let fs = self.fs();
        for f in [".gitignore", ".git/info/exclude"] {
            if let Ok(bytes) = fs.read_file(host(&self.root.join(f)).as_host_path()) {
                for line in String::from_utf8_lossy(&bytes).lines() {
                    let _ = b.add_line(None, line);
                }
            }
        }
        b.build()
            .unwrap_or_else(|_| ignore::gitignore::Gitignore::empty())
    }

    /// Every file under the root as (absolute path, relative path): hidden
    /// and ignored files left out. The walk runs on the host's side.
    fn files(&self, max: usize, mut f: impl FnMut(&Path, &str) -> bool) -> Result<()> {
        let rules = self.ignore_rules();
        let cancel = AtomicBool::new(false);
        let opts = WalkOptions {
            skip_dirs: SKIP_DIRS,
            max_entries: max,
            ..WalkOptions::default()
        };
        self.fs()
            .walk(host(&self.root).as_host_path(), &opts, &cancel, &mut |e| {
                if !matches!(e.entry_type, EntryType::File) {
                    return true;
                }
                if rules.matched_path_or_any_parents(e.rel, false).is_ignore() {
                    return true;
                }
                let abs: PathBuf = e.path.to_host_path_buf().into();
                f(&abs, e.rel)
            })?;
        Ok(())
    }

    /// Files whose relative path matches `query`: every space-separated term
    /// a subsequence of it, best (contiguous, then shortest) first.
    pub fn find_files(&self, query: &str, limit: usize) -> Result<Vec<String>> {
        let terms: Vec<Vec<char>> = query
            .to_lowercase()
            .split_whitespace()
            .map(|t| t.chars().collect())
            .collect();
        let mut hits: Vec<(usize, usize, String)> = Vec::new();
        self.files(500_000, |_, rel| {
            let lower = rel.to_lowercase();
            let mut contiguous = 0;
            for t in &terms {
                let mut it = lower.chars();
                if !t.iter().all(|c| it.any(|x| x == *c)) {
                    return true;
                }
                if lower.contains(&t.iter().collect::<String>()) {
                    contiguous += 1;
                }
            }
            hits.push((terms.len() - contiguous, rel.len(), rel.to_string()));
            true
        })?;
        hits.sort();
        Ok(hits.into_iter().take(limit).map(|(_, _, r)| r).collect())
    }

    /// Matches of `pattern` across the workspace: each file searched by the
    /// filesystem (on the host's side for a remote one).
    pub fn search(
        &self,
        pattern: &str,
        is_regex: bool,
        case: bool,
        whole: bool,
        max: usize,
    ) -> Result<Value> {
        if is_regex {
            regex::Regex::new(pattern)?;
        }
        let fs = self.fs();
        let mut paths = Vec::new();
        self.files(200_000, |abs, rel| {
            paths.push((abs.to_path_buf(), rel.to_string()));
            true
        })?;
        let mut out = Vec::new();
        let mut files = 0usize;
        let mut truncated = false;
        for (abs, rel) in paths {
            let left = max.saturating_sub(out.len());
            if left == 0 {
                truncated = true;
                break;
            }
            let opts = FileSearchOptions {
                fixed_string: !is_regex,
                case_sensitive: case,
                whole_word: whole,
                max_matches: left,
            };
            let Ok(found) = fs.search_file(host(&abs).as_host_path(), pattern, &opts) else {
                continue;
            };
            if !found.is_empty() {
                files += 1;
            }
            for m in found {
                out.push(json!({
                    "path": rel, "line": m.line.saturating_sub(1), "col": m.column.saturating_sub(1), "len": m.length,
                    "text": m.context,
                }));
            }
        }
        Ok(json!({ "matches": out, "files": files, "truncated": truncated }))
    }
}
