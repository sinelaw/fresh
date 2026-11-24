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
//!
//! Every process — a client's `proc.run`, a terminal's shell, a language
//! server and its availability probe — is built by the authority too
//! ([`Authority::command`], [`Authority::shell_command`]): directly here, `ssh`
//! to a remote host, `docker exec` into a container.

use anyhow::Result;
use fresh::services::remote::ConnectionParams;
use fresh_editor_core::model::filesystem::{
    EntryType, FileSearchOptions, FileSystem, StdFileSystem, WalkOptions,
};
use fresh_editor_core::model::host_path::HostPathBuf;
use ignore::gitignore::{Gitignore, GitignoreBuilder};
use serde_json::{json, Value};
use std::collections::HashMap;
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
/// the remote side); processes and terminals through `ssh`, with the port,
/// identity and options of the agent's connection.
pub struct Ssh {
    pub params: ConnectionParams,
    pub fs: Fs,
    /// Keeps the connection's async tasks alive.
    pub _runtime: tokio::runtime::Runtime,
    pub _conn: fresh::services::remote::SshConnection,
}

/// `ssh … <target> 'cd <cwd> && exec <program> <args…>'`: a non-interactive
/// command on the host of `params`.
fn ssh_command(params: &ConnectionParams, program: &str, args: &[String], cwd: &Path) -> Command {
    let mut c = Command::new("ssh");
    c.args([
        "-o",
        "BatchMode=yes",
        "-o",
        "StrictHostKeyChecking=accept-new",
    ]);
    if let Some(port) = params.port {
        c.arg("-p").arg(port.to_string());
    }
    if let Some(id) = &params.identity_file {
        c.arg("-i").arg(id);
    }
    c.args(&params.extra_args).arg(params.ssh_target());
    let mut line = format!(
        "cd {} && exec {}",
        sh_quote(&cwd.to_string_lossy()),
        sh_quote(program)
    );
    for a in args {
        line.push(' ');
        line.push_str(&sh_quote(a));
    }
    c.arg(line);
    c
}

/// The editor's own SSH terminal command: a login shell in `cwd`, with auth
/// prompts allowed to surface in the terminal.
fn ssh_shell_command(params: &ConnectionParams, cwd: &Path) -> portable_pty::CommandBuilder {
    let mut c = portable_pty::CommandBuilder::new("ssh");
    c.args(fresh::services::remote::build_ssh_terminal_args(
        params,
        Some(&cwd.to_string_lossy()),
    ));
    c.env("TERM", "xterm-256color");
    c
}

impl Authority for Ssh {
    fn name(&self) -> String {
        format!("ssh {}", self.params)
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command {
        ssh_command(&self.params, program, args, cwd)
    }
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder {
        ssh_shell_command(&self.params, cwd)
    }
}

/// A container whose workspace is bind-mounted at the same path, as in a
/// devcontainer: files stay on this machine; processes, terminals and
/// language servers run in the container through `docker exec`.
pub struct Container {
    /// The container's name or id.
    pub id: String,
    pub fs: Fs,
}

impl Authority for Container {
    fn name(&self) -> String {
        format!("container {}", self.id)
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> Command {
        // `-i` keeps stdin open: a language server is spoken to on it.
        let mut c = Command::new("docker");
        c.args(["exec", "-i", "-w"])
            .arg(cwd)
            .arg(&self.id)
            .arg(program)
            .args(args);
        c
    }
    fn shell_command(&self, cwd: &Path) -> portable_pty::CommandBuilder {
        let mut c = portable_pty::CommandBuilder::new("docker");
        c.args(["exec", "-it", "-w"]);
        c.arg(cwd);
        c.args([
            "-e",
            "TERM=xterm-256color",
            "-e",
            "COLORTERM=truecolor",
            &self.id,
            "sh",
            "-c",
            "exec \"${SHELL:-/bin/sh}\" -l",
        ]);
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

/// Ignore rules as git applies them: `.git/info/exclude`, then the root's
/// `.gitignore`, then each folder's own `.gitignore`, a deeper file's rules
/// deciding over a shallower one's. Everything in an ignored folder is
/// ignored, whatever rules lie inside it (git never reads them). Each
/// `.gitignore` is read through the workspace's filesystem, once, when a
/// path under its folder is first asked about.
pub struct Ignores {
    root: PathBuf,
    fs: Fs,
    exclude: Gitignore,
    /// Each folder's own rules (by path relative to the root, `""` for the
    /// root), `None` when it has none.
    rules: HashMap<String, Option<Gitignore>>,
    /// Whether each folder asked about is ignored.
    dirs: HashMap<String, bool>,
}

impl Ignores {
    pub fn new(root: &Path, fs: Fs) -> Self {
        let exclude =
            Self::read(&fs, root, &root.join(".git/info/exclude")).unwrap_or_else(Gitignore::empty);
        Ignores {
            root: root.to_path_buf(),
            fs,
            exclude,
            rules: HashMap::new(),
            dirs: HashMap::new(),
        }
    }

    /// The rules in `file`, relative to the folder `base`.
    fn read(fs: &Fs, base: &Path, file: &Path) -> Option<Gitignore> {
        let bytes = fs.read_file(host(file).as_host_path()).ok()?;
        let mut b = GitignoreBuilder::new(base);
        for line in String::from_utf8_lossy(&bytes).lines() {
            // A malformed pattern is skipped, as git skips it.
            let _ = b.add_line(None, line);
        }
        b.build().ok()
    }

    fn rules_of(&mut self, dir: &str) -> Option<&Gitignore> {
        if !self.rules.contains_key(dir) {
            let base = if dir.is_empty() {
                self.root.clone()
            } else {
                self.root.join(dir)
            };
            let rules = Self::read(&self.fs, &base, &base.join(".gitignore"));
            self.rules.insert(dir.to_string(), rules);
        }
        self.rules.get(dir).and_then(Option::as_ref)
    }

    /// Whether `rel` (relative to the root, `/` between components) is
    /// ignored; `dir` when it is a folder.
    pub fn ignored(&mut self, rel: &str, dir: bool) -> bool {
        if let Some((parent, _)) = rel.rsplit_once('/') {
            if self.dir_ignored(parent) {
                return true;
            }
        }
        self.matched(rel, dir)
    }

    fn dir_ignored(&mut self, rel: &str) -> bool {
        if let Some(&ignored) = self.dirs.get(rel) {
            return ignored;
        }
        let ignored = self.ignored(rel, true);
        self.dirs.insert(rel.to_string(), ignored);
        ignored
    }

    /// The verdict on `rel` itself of the rules of every folder holding it,
    /// the deepest that has one deciding.
    fn matched(&mut self, rel: &str, dir: bool) -> bool {
        let abs = self.root.join(rel);
        let mut ignored = self.exclude.matched(&abs, dir).is_ignore();
        let parts: Vec<&str> = rel.split('/').collect();
        for depth in 0..parts.len() {
            let folder = parts[..depth].join("/");
            if let Some(rules) = self.rules_of(&folder) {
                match rules.matched(&abs, dir) {
                    ignore::Match::Ignore(_) => ignored = true,
                    ignore::Match::Whitelist(_) => ignored = false,
                    ignore::Match::None => {}
                }
            }
        }
        ignored
    }
}

pub fn host(p: &Path) -> HostPathBuf {
    HostPathBuf::from_path_buf(p.to_path_buf())
}

fn millis(t: Option<std::time::SystemTime>) -> u64 {
    t.and_then(|t| t.duration_since(UNIX_EPOCH).ok())
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

/// A path's modification signature (size, mtime in ms) through `fs`, or
/// `None` when it does not exist.
pub fn signature(fs: &Fs, p: &Path) -> Option<(u64, u64)> {
    let m = fs.metadata(host(p).as_host_path()).ok()?;
    Some((m.size, millis(m.modified)))
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

    /// A folder's entries, folders first, each marked `ignored` by the
    /// workspace's ignore rules (nested `.gitignore` files included).
    pub fn list_dir(&self, path: &Path) -> Result<Vec<Value>> {
        // Relative to the root, `/`-separated; `None` outside the workspace.
        let rel = path.strip_prefix(&self.root).ok().map(|r| {
            r.components()
                .map(|c| c.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/")
        });
        let mut ignores = Ignores::new(&self.root, self.fs());
        let mut out: Vec<Value> = self
            .fs()
            .read_dir(host(path).as_host_path())?
            .into_iter()
            .map(|e| {
                let dir = matches!(e.entry_type, EntryType::Directory) || e.symlink_target_is_dir;
                let ignored = rel.as_ref().is_some_and(|r| {
                    let me = if r.is_empty() {
                        e.name.clone()
                    } else {
                        format!("{r}/{}", e.name)
                    };
                    ignores.ignored(&me, dir)
                });
                json!({
                    "name": e.name,
                    "path": PathBuf::from(e.path).to_string_lossy(),
                    "dir": dir,
                    "size": e.metadata.as_ref().map(|m| m.size).unwrap_or(0),
                    "ignored": ignored,
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
            fail!(
                TooLarge,
                "file too large to read whole; open it as a document"
            );
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
                fail!(Exists, "{} exists", p.display());
            }
            self.write_file(p, "")?;
        }
        Ok(())
    }

    pub fn rename(&self, from: &Path, to: &Path) -> Result<()> {
        if self.exists(to) {
            fail!(Exists, "{} exists", to.display());
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

    /// The modification signature of a path, for watching it.
    pub fn signature(&self, p: &Path) -> Option<(u64, u64)> {
        signature(&self.fs(), p)
    }

    /// Every file under the root as (absolute path, relative path): hidden
    /// and ignored files left out. The walk runs on the host's side; the
    /// ignore rules are read after it, through the filesystem too.
    fn files(&self, max: usize, mut f: impl FnMut(&Path, &str) -> bool) -> Result<()> {
        let cancel = AtomicBool::new(false);
        let opts = WalkOptions {
            skip_dirs: SKIP_DIRS,
            max_entries: max,
            ..WalkOptions::default()
        };
        let mut all: Vec<(PathBuf, String)> = Vec::new();
        self.fs()
            .walk(host(&self.root).as_host_path(), &opts, &cancel, &mut |e| {
                if matches!(e.entry_type, EntryType::File) {
                    all.push((e.path.to_host_path_buf().into(), e.rel.to_string()));
                }
                true
            })?;
        let mut ignores = Ignores::new(&self.root, self.fs());
        for (abs, rel) in all {
            if !ignores.ignored(&rel, false) && !f(&abs, &rel) {
                break;
            }
        }
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
            if let Err(e) = regex::Regex::new(pattern) {
                fail!(BadRequest, "{e}");
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(c: &Command) -> Vec<String> {
        std::iter::once(c.get_program())
            .chain(c.get_args())
            .map(|a| a.to_string_lossy().into_owned())
            .collect()
    }

    fn pty_argv(c: &portable_pty::CommandBuilder) -> Vec<String> {
        c.get_argv()
            .iter()
            .map(|a| a.to_string_lossy().into_owned())
            .collect()
    }

    fn local_ws() -> (tempfile::TempDir, Workspace) {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        let ws = Workspace {
            root,
            authority: Arc::new(Local::new()),
        };
        (dir, ws)
    }

    #[test]
    fn container_runs_processes_with_docker_exec() {
        let c = Container {
            id: "dev-1".into(),
            fs: Arc::new(StdFileSystem),
        };
        let cmd = c.command("rust-analyzer", &["--stdio".into()], Path::new("/w s/p"));
        assert_eq!(
            argv(&cmd),
            [
                "docker",
                "exec",
                "-i",
                "-w",
                "/w s/p",
                "dev-1",
                "rust-analyzer",
                "--stdio"
            ]
        );
        // No local working directory: the path is the container's.
        assert_eq!(cmd.get_current_dir(), None);
        let sh = c.shell_command(Path::new("/w"));
        assert_eq!(
            pty_argv(&sh),
            [
                "docker",
                "exec",
                "-it",
                "-w",
                "/w",
                "-e",
                "TERM=xterm-256color",
                "-e",
                "COLORTERM=truecolor",
                "dev-1",
                "sh",
                "-c",
                "exec \"${SHELL:-/bin/sh}\" -l",
            ]
        );
    }

    #[test]
    fn ssh_commands_carry_port_identity_and_options() {
        let mut p = ConnectionParams::parse("me@box:2222").unwrap();
        p.identity_file = Some("/k/id".into());
        p.extra_args = vec!["-J".into(), "jump".into()];
        let cmd = ssh_command(&p, "git", &["log".into(), "a b".into()], Path::new("/p q"));
        assert_eq!(
            argv(&cmd),
            [
                "ssh",
                "-o",
                "BatchMode=yes",
                "-o",
                "StrictHostKeyChecking=accept-new",
                "-p",
                "2222",
                "-i",
                "/k/id",
                "-J",
                "jump",
                "me@box",
                "cd '/p q' && exec git log 'a b'",
            ]
        );
        let sh = pty_argv(&ssh_shell_command(&p, Path::new("/p")));
        assert_eq!(sh[0], "ssh");
        assert!(sh.windows(2).any(|w| w == ["-p", "2222"]), "{sh:?}");
        assert!(sh.windows(2).any(|w| w == ["-i", "/k/id"]), "{sh:?}");
        assert!(sh.iter().any(|a| a == "-t"), "{sh:?}");
        assert!(sh.iter().any(|a| a == "me@box"), "{sh:?}");
    }

    #[test]
    fn shell_quoting() {
        assert_eq!(sh_quote("plain/path-1.rs"), "plain/path-1.rs");
        assert_eq!(sh_quote(""), "''");
        assert_eq!(sh_quote("it's"), "'it'\\''s'");
        assert_eq!(sh_quote("$HOME"), "'$HOME'");
    }

    #[test]
    fn file_operations() {
        let (_d, ws) = local_ws();
        let r = &ws.root;
        ws.create(&r.join("a/b"), true).unwrap();
        ws.create(&r.join("a/b/f.txt"), false).unwrap();
        assert!(
            ws.create(&r.join("a/b/f.txt"), false).is_err(),
            "never over a file"
        );
        ws.write_file(&r.join("a/b/f.txt"), "text").unwrap();
        ws.rename(&r.join("a/b/f.txt"), &r.join("a/g.txt")).unwrap();
        assert!(!ws.exists(&r.join("a/b/f.txt")));
        assert_eq!(ws.read_file(&r.join("a/g.txt"), 100).unwrap(), "text");
        assert!(ws.read_file(&r.join("a/g.txt"), 2).is_err(), "over the cap");
        ws.create(&r.join("a/h.txt"), false).unwrap();
        assert!(ws.rename(&r.join("a/g.txt"), &r.join("a/h.txt")).is_err());

        // Copy into a folder, then again: never over an existing name.
        let c1 = ws.copy(&r.join("a/g.txt"), &r.join("a/b")).unwrap();
        assert_eq!(c1, r.join("a/b/g.txt"));
        let c2 = ws.copy(&r.join("a/g.txt"), &r.join("a/b")).unwrap();
        assert_eq!(c2, r.join("a/b/g copy.txt"));
        let c3 = ws.copy(&r.join("a/b"), r).unwrap();
        assert_eq!(c3, r.join("b"));
        assert_eq!(ws.read_file(&r.join("b/g copy.txt"), 100).unwrap(), "text");

        let names: Vec<String> = ws
            .list_dir(&r.join("a"))
            .unwrap()
            .iter()
            .map(|e| e["name"].as_str().unwrap().to_string())
            .collect();
        assert_eq!(
            names,
            ["b", "g.txt", "h.txt"],
            "folders first, then by name"
        );

        ws.remove(&r.join("a")).unwrap();
        assert!(!ws.exists(&r.join("a")));
        assert_eq!(ws.stat(&r.join("b"))["dir"], true);
        assert_eq!(ws.stat(&r.join("a"))["exists"], false);
    }

    #[test]
    fn nested_gitignore_files_apply_below_their_folder() {
        let (_d, ws) = local_ws();
        let r = &ws.root;
        for (p, t) in [
            (".gitignore", "*.tmp\n"),
            ("app/.gitignore", "dist/\n/local.rs\n!keep.tmp\n"),
            ("app/src/main.rs", "needle\n"),
            ("app/local.rs", "needle\n"),
            ("app/sub/local.rs", "needle\n"),
            ("app/dist/out.rs", "needle\n"),
            ("app/keep.tmp", "needle\n"),
            ("app/drop.tmp", "needle\n"),
            ("other/x.tmp", "needle\n"),
            ("other/dist/kept.rs", "needle\n"),
            // An ignored folder's own rules are never read.
            ("app/dist/.gitignore", "!*\n"),
        ] {
            ws.write_file(&r.join(p), t).unwrap();
        }
        let mut found = ws.find_files("", 100).unwrap();
        found.sort();
        assert_eq!(
            found,
            [
                "app/keep.tmp",
                "app/src/main.rs",
                "app/sub/local.rs",
                "other/dist/kept.rs"
            ]
        );
        let res = ws.search("needle", false, false, false, 100).unwrap();
        let mut hits: Vec<&str> = res["matches"]
            .as_array()
            .unwrap()
            .iter()
            .map(|m| m["path"].as_str().unwrap())
            .collect();
        hits.sort();
        assert_eq!(hits, found);

        let listed: Vec<(String, bool)> = ws
            .list_dir(&r.join("app"))
            .unwrap()
            .iter()
            .map(|e| {
                (
                    e["name"].as_str().unwrap().to_string(),
                    e["ignored"].as_bool().unwrap(),
                )
            })
            .collect();
        assert_eq!(
            listed,
            [
                ("dist".to_string(), true),
                ("src".to_string(), false),
                ("sub".to_string(), false),
                (".gitignore".to_string(), false),
                ("drop.tmp".to_string(), true),
                ("keep.tmp".to_string(), false),
                ("local.rs".to_string(), true),
            ]
        );
    }

    #[test]
    fn find_and_search_follow_ignore_rules() {
        let (_d, ws) = local_ws();
        let r = &ws.root;
        for (p, t) in [
            (".gitignore", "build/\n*.log\n!keep.log\n"),
            (".git/info/exclude", "secret.txt\n"),
            ("src/main.rs", "fn main() { needle(); }\n"),
            ("src/lib.rs", "pub fn needle() {}\nconst NEEDLES: u8 = 0;\n"),
            ("build/out.rs", "needle\n"),
            ("debug.log", "needle\n"),
            ("keep.log", "needle\n"),
            ("secret.txt", "needle\n"),
            (".hidden/x.rs", "needle\n"),
            ("target/y.rs", "needle\n"),
        ] {
            ws.write_file(&r.join(p), t).unwrap();
        }
        assert_eq!(
            ws.find_files("rs", 10).unwrap(),
            ["src/lib.rs", "src/main.rs"]
        );
        assert_eq!(ws.find_files("smn", 10).unwrap(), ["src/main.rs"]);

        let res = ws.search("needle", false, false, false, 100).unwrap();
        let mut hits: Vec<(String, u64)> = res["matches"]
            .as_array()
            .unwrap()
            .iter()
            .map(|m| {
                (
                    m["path"].as_str().unwrap().to_string(),
                    m["line"].as_u64().unwrap(),
                )
            })
            .collect();
        hits.sort();
        assert_eq!(
            hits,
            [
                ("keep.log".to_string(), 0),
                ("src/lib.rs".to_string(), 0),
                ("src/lib.rs".to_string(), 1),
                ("src/main.rs".to_string(), 0),
            ]
        );
        assert_eq!(res["files"], 3);
        let whole = ws.search("needle", false, false, true, 100).unwrap();
        assert_eq!(whole["matches"].as_array().unwrap().len(), 3);
        let case = ws.search("NEEDLE", false, true, false, 100).unwrap();
        assert_eq!(case["matches"].as_array().unwrap().len(), 1);
        let re = ws.search(r"fn \w+\(", true, false, false, 100).unwrap();
        assert_eq!(re["matches"].as_array().unwrap().len(), 2);
        assert!(ws.search("(", true, false, false, 100).is_err());
        let capped = ws.search("needle", false, false, false, 2).unwrap();
        assert_eq!(capped["matches"].as_array().unwrap().len(), 2);
        assert_eq!(capped["truncated"], true);
    }
}
