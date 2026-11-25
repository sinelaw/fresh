//! The orchestrator: one server, many workspaces.
//!
//! The server holds a registry of open workspaces ([`Session`]s), each a root
//! on a host with its own authority, trust, documents, terminals, processes,
//! watches and language servers. The command line's folder is the first;
//! clients open more (`workspace.open`) and close them (`workspace.close`).
//!
//! A client views one workspace: it names it in `hello` (or gets the first
//! open one), and every operation it sends after that is the workspace's.
//! Viewing another one is a new client (the web client reloads on
//! `?ws=<id>`), so a client never mixes two workspaces' documents. Every
//! client hears about every workspace — `workspace.added`,
//! `workspace.removed`, `workspace.changed`, `workspace.status`,
//! `orchestrator.folders` — for the dock.
//!
//! Each workspace also has what the user gave it: a name, a folder in the
//! dock (folders nest, `A/B`), a pin, when it was created and last worked
//! in (typed into one of its files or terminals), and, for a workspace made
//! on a new git worktree, that worktree. The registry — workspaces and
//! folders — is kept in a file on this machine (one per server port)
//! through a local authority
//! ([`Store`]) and opened again when the server starts; a workspace that
//! cannot be opened again (its host unreachable, its folder gone) stays
//! listed with why, to retry or dismiss.

use crate::authority::{self, Authority, HostSpec};
use crate::error::{err, Code};
use crate::hub::Hub;
use crate::session::{s, u, Session};
use crate::workspace::{self, Fs, Workspace};
use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

/// Milliseconds since the epoch.
pub fn now_ms() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

/// A git worktree a workspace was made on: the repository it belongs to and
/// its branch (the worktree is the workspace's root).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Worktree {
    pub repo: String,
    pub branch: String,
}

/// What the user gave a workspace, and when it was made and last worked in.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Meta {
    pub host: HostSpec,
    /// The user's name for it; without one, it is named after its agent's
    /// terminal title, or its folder.
    #[serde(default)]
    pub name: Option<String>,
    /// Its folder in the dock, `""` for the top level.
    #[serde(default)]
    pub folder: String,
    #[serde(default)]
    pub pinned: bool,
    #[serde(default)]
    pub created: u64,
    #[serde(default)]
    pub worked: u64,
    #[serde(default)]
    pub worktree: Option<Worktree>,
    /// The agents running in it when the registry was last written, as the
    /// commands that take each up again (`claude --continue`, …): started
    /// again when the workspace is.
    #[serde(default)]
    pub resume: Vec<String>,
}

/// A workspace as the registry keeps it.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Record {
    pub id: u64,
    pub root: String,
    pub trusted: bool,
    #[serde(flatten)]
    pub meta: Meta,
}

/// A machine the user declared: a name and how to reach it. This machine
/// is always there, undeclared; the hosts in the user's `~/.ssh/config`
/// are there too (`ssh_config`), without being declared.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Machine {
    #[serde(default)]
    pub name: String,
    pub host: HostSpec,
    /// Found in `~/.ssh/config` (not kept in the registry).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub ssh_config: bool,
}

/// The hosts `~/.ssh/config` (on this machine, whose `ssh` reaches them)
/// names: each `Host` alias that is not a pattern, in order, following
/// `Include` lines (relative to `~/.ssh`, without wildcards). Each is
/// reached by its alias, so `ssh` applies what the config says of it.
pub fn ssh_config_hosts(fs: &Fs, home: &Path) -> Vec<Machine> {
    fn read(fs: &Fs, path: &Path, home: &Path, depth: usize, out: &mut Vec<String>) {
        let Ok(bytes) = fs.read_file(workspace::host(path).as_host_path()) else {
            return;
        };
        for line in String::from_utf8_lossy(&bytes).lines() {
            let line = line.trim();
            let (key, rest) = line
                .split_once(|c: char| c.is_whitespace() || c == '=')
                .unwrap_or((line, ""));
            let words = rest
                .trim_start_matches(|c: char| c.is_whitespace() || c == '=')
                .split_whitespace();
            match key.to_ascii_lowercase().as_str() {
                "host" => out.extend(
                    words
                        .filter(|w| !w.contains(['*', '?', '!']))
                        .map(str::to_string),
                ),
                "include" if depth < 4 => {
                    for w in words.filter(|w| !w.contains(['*', '?'])) {
                        let p = match w.strip_prefix("~/") {
                            Some(r) => home.join(r),
                            None if w.starts_with('/') => PathBuf::from(w),
                            None => home.join(".ssh").join(w),
                        };
                        read(fs, &p, home, depth + 1, out);
                    }
                }
                _ => {}
            }
        }
    }
    let mut aliases = Vec::new();
    read(fs, &home.join(".ssh/config"), home, 0, &mut aliases);
    let mut seen = std::collections::BTreeSet::new();
    aliases
        .into_iter()
        .filter(|a| seen.insert(a.clone()))
        .map(|alias| Machine {
            name: alias.clone(),
            host: HostSpec::Ssh {
                target: alias,
                identity: None,
                options: Vec::new(),
            },
            ssh_config: true,
        })
        .collect()
}

/// The registry's file.
#[derive(Default, Serialize, Deserialize)]
struct Saved {
    #[serde(default)]
    folders: Vec<String>,
    #[serde(default)]
    machines: Vec<Machine>,
    #[serde(default)]
    workspaces: Vec<Record>,
}

/// Where the registry is kept: a file read and written through a
/// filesystem (this machine's, through a local authority).
pub struct Store {
    pub fs: Fs,
    pub path: PathBuf,
}

impl Store {
    /// `~/.local/share/fresh-poc/orchestrator-<port>.json` on this machine:
    /// a server is known by its port, so two servers keep two registries.
    pub fn local(port: u16) -> Result<Store> {
        let fs = authority::Local::new().fs();
        let home = PathBuf::from(fs.home_dir()?);
        Ok(Store {
            fs,
            path: home.join(format!(".local/share/fresh-poc/orchestrator-{port}.json")),
        })
    }

    fn read(&self) -> Saved {
        self.fs
            .read_file(workspace::host(&self.path).as_host_path())
            .ok()
            .and_then(|b| serde_json::from_slice(&b).ok())
            .unwrap_or_default()
    }

    fn write(&self, saved: &Saved) {
        let write = || -> Result<()> {
            if let Some(dir) = self.path.parent() {
                self.fs
                    .create_dir_all(workspace::host(dir).as_host_path())?;
            }
            self.fs.write_file(
                workspace::host(&self.path).as_host_path(),
                &serde_json::to_vec_pretty(&json!({
                    "folders": saved.folders, "machines": saved.machines,
                    "workspaces": saved.workspaces,
                }))?,
            )?;
            Ok(())
        };
        if let Err(e) = write() {
            eprintln!("{}: not saved: {e}", self.path.display());
        }
    }
}

pub struct Orchestrator {
    pub hub: Arc<Hub>,
    sessions: Mutex<BTreeMap<u64, Arc<Session>>>,
    /// Workspaces that could not be opened again, with why.
    failed: Mutex<BTreeMap<u64, (Record, String)>>,
    /// The dock's folders, in order.
    folders: Mutex<Vec<String>>,
    /// The machines the user declared, in order.
    machines: Mutex<Vec<Machine>>,
    /// Hosts reached to browse their folders, by label: a connection is
    /// made once and kept.
    reached: Mutex<BTreeMap<String, Arc<dyn Authority>>>,
    next: AtomicU64,
    /// Network-like delays on every workspace's filesystem.
    slow: bool,
    store: Option<Store>,
}

/// How a new workspace's folder is made: a new git worktree, on a new
/// branch cut from `base` (default: the repository's default branch).
pub struct WorktreePlan<'a> {
    /// The worktree's name, and its branch's (default: `<repo>-N`).
    pub branch: Option<&'a str>,
    pub base: Option<&'a str>,
}

/// What `workspace.open` asks for.
pub struct Open<'a> {
    pub host: HostSpec,
    pub root: &'a str,
    pub trusted: bool,
    /// A command to run in a terminal once it is open: an agent session.
    pub agent: Option<&'a str>,
    /// The command that takes that agent up again after a restart.
    pub agent_resume: Option<&'a str>,
    /// A new git worktree of the repository `root` is in, instead of
    /// `root` itself.
    pub worktree: Option<WorktreePlan<'a>>,
    pub name: Option<&'a str>,
    pub folder: &'a str,
}

impl<'a> Open<'a> {
    /// The folder `root` on `host` as it is: no agent, no worktree, no
    /// name, at the dock's top level.
    pub fn new(host: HostSpec, root: &'a str, trusted: bool) -> Self {
        Open {
            host,
            root,
            trusted,
            agent: None,
            agent_resume: None,
            worktree: None,
            name: None,
            folder: "",
        }
    }
}

/// Run `git` with `args` in `cwd` on the host; its output, or its last
/// line of complaint.
fn git(authority: &dyn Authority, args: &[&str], cwd: &Path) -> Result<String> {
    let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    let out = authority
        .command("git", &args, cwd)
        .0
        .stdin(std::process::Stdio::null())
        .output()
        .map_err(|e| err(Code::Failed, format!("cannot run git: {e}")))?;
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        let line = stderr
            .lines()
            .rfind(|l| !l.trim().is_empty())
            .unwrap_or("git failed");
        let code = if line.contains("already exists") {
            Code::Exists
        } else {
            Code::Failed
        };
        return Err(err(code, line.trim().to_string()));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

impl Orchestrator {
    pub fn new(slow: bool, store: Option<Store>) -> Self {
        Orchestrator {
            hub: Arc::default(),
            sessions: Mutex::default(),
            failed: Mutex::default(),
            folders: Mutex::default(),
            machines: Mutex::default(),
            reached: Mutex::default(),
            next: AtomicU64::new(0),
            slow,
            store,
        }
    }

    pub fn session(&self, id: u64) -> Option<Arc<Session>> {
        self.sessions.lock().unwrap().get(&id).cloned()
    }

    /// Every workspace, in the order opened: the open ones and the ones
    /// that could not be opened again (`error`, no `status`).
    pub fn list(&self) -> Vec<Value> {
        let sessions: Vec<Arc<Session>> = self.sessions.lock().unwrap().values().cloned().collect();
        let mut out: Vec<(u64, Value)> = sessions.iter().map(|s| (s.id, s.entry())).collect();
        for (id, (r, why)) in self.failed.lock().unwrap().iter() {
            out.push((*id, failed_entry(r, why)));
        }
        out.sort_by_key(|(id, _)| *id);
        out.into_iter().map(|(_, e)| e).collect()
    }

    pub fn folders(&self) -> Vec<String> {
        self.folders.lock().unwrap().clone()
    }

    /// The machines the user declared, as kept.
    pub fn machines(&self) -> Vec<Machine> {
        self.machines.lock().unwrap().clone()
    }

    /// Every machine to choose from: the declared ones, then the hosts in
    /// `~/.ssh/config` not declared under the same name.
    pub fn all_machines(&self) -> Vec<Machine> {
        let mut out = self.machines();
        let fs = authority::Local::new().fs();
        if let Ok(home) = fs.home_dir() {
            for m in ssh_config_hosts(&fs, &PathBuf::from(home)) {
                if !out.iter().any(|d| d.name == m.name) {
                    out.push(m);
                }
            }
        }
        out
    }

    /// `host`'s authority: this machine's, or a connection made once and
    /// kept.
    fn reach(&self, host: &HostSpec) -> Result<Arc<dyn Authority>> {
        if *host == HostSpec::Local {
            return Ok(Arc::new(authority::Local::new()));
        }
        let key = host.label();
        if let Some(a) = self.reached.lock().unwrap().get(&key) {
            return Ok(a.clone());
        }
        let a = authority::connect(host, self.slow)
            .map_err(|e| err(Code::Failed, format!("cannot reach {key}: {e}")))?;
        self.reached.lock().unwrap().insert(key, a.clone());
        Ok(a)
    }

    /// Declare a machine (it is reached first, so one that cannot be is
    /// refused), or redeclare one of the same name.
    fn add_machine(&self, mut machine: Machine) -> Result<Vec<Machine>> {
        // Unnamed, a machine is called by its host.
        if machine.name.trim().is_empty() {
            machine.name = match &machine.host {
                HostSpec::Ssh { target, .. } => target.clone(),
                h => h.label(),
            };
        }
        machine.ssh_config = false;
        if machine.host == HostSpec::Local {
            fail!(BadRequest, "this machine is always there");
        }
        self.reached.lock().unwrap().remove(&machine.host.label());
        self.reach(&machine.host)?;
        {
            let mut ms = self.machines.lock().unwrap();
            ms.retain(|m| m.name != machine.name);
            ms.push(machine);
        }
        self.save();
        Ok(self.all_machines())
    }

    fn remove_machine(&self, name: &str) -> Vec<Machine> {
        self.machines.lock().unwrap().retain(|m| m.name != name);
        self.save();
        self.all_machines()
    }

    /// The folders in `path` on `host` (`~` and a path under it start at
    /// the home folder there), for choosing a workspace's root.
    fn browse(&self, host: &HostSpec, path: &str) -> Result<Value> {
        let fs = self.reach(host)?.fs();
        let home = PathBuf::from(fs.home_dir()?);
        let dir = match path.trim() {
            "" | "~" => home.clone(),
            p => match p.strip_prefix("~/") {
                Some(rest) => home.join(rest),
                None => PathBuf::from(p),
            },
        };
        let mut dirs: Vec<Value> = fs
            .read_dir(workspace::host(&dir).as_host_path())
            .map_err(|e| anyhow::Error::new(e).context(dir.display().to_string()))?
            .into_iter()
            .filter(|e| {
                matches!(
                    e.entry_type,
                    fresh_editor_core::model::filesystem::EntryType::Directory
                ) || e.symlink_target_is_dir
            })
            .map(|e| json!({ "name": e.name, "path": dir.join(&e.name).to_string_lossy() }))
            .collect();
        dirs.sort_by(|a, b| {
            let key = |v: &Value| v["name"].as_str().unwrap_or("").to_lowercase();
            key(a).cmp(&key(b))
        });
        Ok(json!({
            "path": dir.to_string_lossy(),
            "parent": dir.parent().map(|p| p.to_string_lossy().into_owned()),
            "home": home.to_string_lossy(),
            "dirs": dirs,
        }))
    }

    /// Write the registry down (when there is a store).
    fn save(&self) {
        let Some(store) = &self.store else { return };
        let sessions: Vec<Arc<Session>> = self.sessions.lock().unwrap().values().cloned().collect();
        let mut workspaces: Vec<Record> = sessions.iter().map(|s| s.record()).collect();
        workspaces.extend(self.failed.lock().unwrap().values().map(|(r, _)| r.clone()));
        workspaces.sort_by_key(|r| r.id);
        store.write(&Saved {
            folders: self.folders(),
            machines: self.machines(),
            workspaces,
        });
    }

    /// Open the workspaces the registry kept, as they were (ids, names,
    /// folders, trust); one that cannot be opened stays listed with why.
    pub fn restore(&self) {
        let Some(store) = &self.store else { return };
        let saved = store.read();
        *self.folders.lock().unwrap() = saved.folders;
        *self.machines.lock().unwrap() = saved.machines;
        for r in saved.workspaces {
            self.next.fetch_max(r.id, Ordering::SeqCst);
            if let Err(e) = self.start(r.clone()) {
                self.failed.lock().unwrap().insert(r.id, (r, e.to_string()));
            }
        }
    }

    /// Reach a record's host, find its root there and start following it.
    fn start(&self, r: Record) -> Result<Arc<Session>> {
        let authority = authority::connect(&r.meta.host, self.slow)
            .map_err(|e| err(Code::Failed, format!("cannot reach the host: {e}")))?;
        let root = PathBuf::from(&r.root);
        if !authority
            .fs()
            .is_dir(workspace::host(&root).as_host_path())
            .unwrap_or(false)
        {
            fail!(
                NotFound,
                "{} is not a folder on {}",
                r.root,
                r.meta.host.label()
            );
        }
        if matches!(r.meta.host, HostSpec::Container { .. }) {
            check_mounted(&*authority, &root)?;
        }
        let ws = Arc::new(Workspace { root, authority });
        let resume = r.meta.resume.clone();
        let s = Session::new(r.id, ws, r.trusted, r.meta, self.hub.clone());
        self.sessions.lock().unwrap().insert(r.id, s.clone());
        // Its agents take up where they left off (a restricted workspace
        // runs none).
        if r.trusted {
            for cmd in &resume {
                if let Err(e) = s.open_terminal((120, 30), None, Some(cmd), Some(cmd)) {
                    eprintln!("workspace {}: {cmd}: {e}", r.id);
                }
            }
        }
        Ok(s)
    }

    /// Open a workspace: reach its host, find its root there (`~` is the
    /// home folder there; a relative path is resolved by the host's
    /// filesystem, from where the server, or the remote agent, runs), make
    /// its git worktree when asked, and start following it. A folder already
    /// open on the same host is that workspace (as it is: its trust is not
    /// changed). With an agent, a terminal running it is opened in the
    /// workspace, which must be trusted.
    pub fn open(&self, o: &Open) -> Result<(Arc<Session>, Option<u64>)> {
        if o.agent.is_some() && !o.trusted {
            fail!(
                Untrusted,
                "an agent runs in a trusted workspace: trust it to start one"
            );
        }
        if o.worktree.is_some() && !o.trusted {
            fail!(
                Untrusted,
                "making a worktree runs git: trust the workspace to make one"
            );
        }
        self.check_folder(o.folder)?;
        let authority = authority::connect(&o.host, self.slow)
            .map_err(|e| err(Code::Failed, format!("cannot reach the host: {e}")))?;
        let fs = authority.fs();
        let given = match o.root.strip_prefix("~") {
            Some(rest) if rest.is_empty() || rest.starts_with('/') => {
                PathBuf::from(fs.home_dir()?).join(rest.trim_start_matches('/'))
            }
            _ => PathBuf::from(o.root),
        };
        let mut root: PathBuf = fs
            .canonicalize(workspace::host(&given).as_host_path())
            .map(PathBuf::from)
            .map_err(|e| err(Code::NotFound, format!("{}: {e}", given.display())))?;
        if !fs
            .is_dir(workspace::host(&root).as_host_path())
            .unwrap_or(false)
        {
            fail!(BadRequest, "{} is not a folder", root.display());
        }
        let worktree = match &o.worktree {
            Some(plan) => {
                let (dir, wt) = make_worktree(&*authority, &fs, &root, plan)?;
                root = dir;
                Some(wt)
            }
            None => None,
        };
        if matches!(o.host, HostSpec::Container { .. }) {
            check_mounted(&*authority, &root)?;
        }
        let ws = Arc::new(Workspace { root, authority });
        let (session, added) = {
            let mut sessions = self.sessions.lock().unwrap();
            let same = sessions
                .values()
                .find(|s| s.ws.root == ws.root && s.ws.authority.name() == ws.authority.name());
            match same {
                Some(s) => (s.clone(), false),
                None => {
                    let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
                    let now = now_ms();
                    let meta = Meta {
                        host: o.host.clone(),
                        name: o.name.filter(|n| !n.trim().is_empty()).map(str::to_string),
                        folder: o.folder.to_string(),
                        pinned: false,
                        created: now,
                        worked: now,
                        worktree,
                        resume: Vec::new(),
                    };
                    let s = Session::new(id, ws, o.trusted, meta, self.hub.clone());
                    sessions.insert(id, s.clone());
                    (s, true)
                }
            }
        };
        if added {
            self.hub
                .broadcast(&json!({"ev": "workspace.added", "workspace": session.entry()}));
            self.save();
        }
        let agent = match o.agent {
            Some(cmd) => Some(session.open_terminal((120, 30), None, Some(cmd), o.agent_resume)?),
            None => None,
        };
        Ok((session, agent))
    }

    /// Close a workspace: stop everything it runs on its host and forget
    /// its documents. Refused while it holds unsaved documents, unless
    /// `discard`. With `remove_worktree`, its git worktree is removed too
    /// (its branch stays). Clients viewing it are told
    /// (`workspace.removed`) and can do nothing more there. A workspace that
    /// could not be opened again is dismissed.
    pub fn close(&self, id: u64, discard: bool, remove_worktree: bool) -> Result<()> {
        if self.failed.lock().unwrap().remove(&id).is_some() {
            self.hub
                .broadcast(&json!({"ev": "workspace.removed", "workspace": id}));
            self.save();
            return Ok(());
        }
        let session = self
            .session(id)
            .ok_or_else(|| err(Code::NotFound, format!("no workspace {id}")))?;
        let unsaved = session.modified();
        if unsaved > 0 && !discard {
            fail!(
                Unsaved,
                "{} has {unsaved} unsaved document{}",
                session.name(),
                if unsaved == 1 { "" } else { "s" }
            );
        }
        let worktree = session.meta().worktree;
        if remove_worktree && worktree.is_none() {
            fail!(BadRequest, "{} is not a worktree", session.name());
        }
        self.sessions.lock().unwrap().remove(&id);
        session.shutdown();
        self.hub
            .broadcast(&json!({"ev": "workspace.removed", "workspace": id}));
        self.save();
        if let (true, Some(wt)) = (remove_worktree, worktree) {
            let root = session.ws.root.to_string_lossy().into_owned();
            git(
                &*session.ws.authority,
                &["-C", &wt.repo, "worktree", "remove", "--force", &root],
                Path::new(&wt.repo),
            )?;
        }
        Ok(())
    }

    /// Open again a workspace that could not be.
    fn retry(&self, id: u64) -> Result<Value> {
        let (r, _) = self
            .failed
            .lock()
            .unwrap()
            .remove(&id)
            .ok_or_else(|| err(Code::NotFound, format!("no workspace {id} to retry")))?;
        match self.start(r.clone()) {
            Ok(s) => {
                let entry = s.entry();
                self.hub
                    .broadcast(&json!({"ev": "workspace.changed", "workspace": entry}));
                self.save();
                Ok(entry)
            }
            Err(e) => {
                let entry = failed_entry(&r, &e.to_string());
                self.failed.lock().unwrap().insert(id, (r, e.to_string()));
                self.hub
                    .broadcast(&json!({"ev": "workspace.changed", "workspace": entry}));
                Err(e)
            }
        }
    }

    /// A workspace's name, folder or pin.
    fn set(&self, id: u64, m: &Value) -> Result<Value> {
        if let Some(folder) = m.get("folder").and_then(Value::as_str) {
            self.check_folder(folder)?;
        }
        let edit = |meta: &mut Meta| {
            if let Some(n) = m.get("name") {
                meta.name = n
                    .as_str()
                    .map(str::trim)
                    .filter(|n| !n.is_empty())
                    .map(str::to_string);
            }
            if let Some(f) = m.get("folder").and_then(Value::as_str) {
                meta.folder = f.to_string();
            }
            if let Some(p) = m.get("pinned").and_then(Value::as_bool) {
                meta.pinned = p;
            }
        };
        let entry = match self.session(id) {
            Some(s) => {
                s.edit_meta(edit);
                s.entry()
            }
            None => {
                let mut failed = self.failed.lock().unwrap();
                let (r, why) = failed
                    .get_mut(&id)
                    .ok_or_else(|| err(Code::NotFound, format!("no workspace {id}")))?;
                edit(&mut r.meta);
                failed_entry(r, why)
            }
        };
        self.hub
            .broadcast(&json!({"ev": "workspace.changed", "workspace": entry}));
        self.save();
        Ok(entry)
    }

    fn check_folder(&self, folder: &str) -> Result<()> {
        if !folder.is_empty() && !self.folders.lock().unwrap().iter().any(|f| f == folder) {
            fail!(NotFound, "no folder {folder}");
        }
        Ok(())
    }

    /// Change the folders: `create` one (and its parents), `rename` one
    /// (its subfolders and workspaces follow), `delete` one (its
    /// subfolders and workspaces move up to its parent).
    fn folder(&self, op: &str, m: &Value) -> Result<Value> {
        let clean = |p: &str| -> Result<String> {
            let parts: Vec<&str> = p.split('/').map(str::trim).collect();
            if parts.iter().any(|x| x.is_empty()) {
                fail!(BadRequest, "a folder's name can't be empty");
            }
            Ok(parts.join("/"))
        };
        // Where a folder path lands when `from` becomes `to`.
        let moved = |p: &str, from: &str, to: &str| -> Option<String> {
            if p == from {
                Some(to.to_string())
            } else {
                p.strip_prefix(&format!("{from}/")).map(|rest| {
                    if to.is_empty() {
                        rest.to_string()
                    } else {
                        format!("{to}/{rest}")
                    }
                })
            }
        };
        let (from, to) = {
            let mut folders = self.folders.lock().unwrap();
            match op {
                "folder.create" => {
                    let path = clean(s(m, "path")?)?;
                    let mut at = String::new();
                    for part in path.split('/') {
                        at = if at.is_empty() {
                            part.into()
                        } else {
                            format!("{at}/{part}")
                        };
                        if !folders.contains(&at) {
                            folders.push(at.clone());
                        }
                    }
                    (None, None)
                }
                "folder.rename" => {
                    let (from, to) = (s(m, "from")?.to_string(), clean(s(m, "to")?)?);
                    if !folders.contains(&from) {
                        fail!(NotFound, "no folder {from}");
                    }
                    if folders.contains(&to) {
                        fail!(Exists, "{to} exists");
                    }
                    for f in folders.iter_mut() {
                        if let Some(n) = moved(f, &from, &to) {
                            *f = n;
                        }
                    }
                    (Some(from), Some(to))
                }
                _ => {
                    let from = s(m, "path")?.to_string();
                    if !folders.contains(&from) {
                        fail!(NotFound, "no folder {from}");
                    }
                    let parent = from
                        .rsplit_once('/')
                        .map(|(p, _)| p)
                        .unwrap_or("")
                        .to_string();
                    folders.retain(|f| f != &from);
                    for f in folders.iter_mut() {
                        if let Some(n) = moved(f, &from, &parent) {
                            *f = n;
                        }
                    }
                    let mut seen = std::collections::HashSet::new();
                    folders.retain(|f| seen.insert(f.clone()));
                    (Some(from), Some(parent))
                }
            }
        };
        if let (Some(from), Some(to)) = (from, to) {
            let sessions: Vec<Arc<Session>> =
                self.sessions.lock().unwrap().values().cloned().collect();
            for s in sessions {
                let f = s.meta().folder;
                if let Some(n) = moved(&f, &from, &to) {
                    s.edit_meta(|meta| meta.folder = n);
                    self.hub
                        .broadcast(&json!({"ev": "workspace.changed", "workspace": s.entry()}));
                }
            }
            for (r, _) in self.failed.lock().unwrap().values_mut() {
                if let Some(n) = moved(&r.meta.folder, &from, &to) {
                    r.meta.folder = n;
                }
            }
        }
        let folders = self.folders();
        self.hub
            .broadcast(&json!({"ev": "orchestrator.folders", "folders": folders}));
        self.save();
        Ok(json!({ "folders": folders }))
    }

    /// The session `client` views, for an operation on it.
    fn viewed(&self, client: u64) -> Result<Arc<Session>> {
        match self.hub.workspace_of(client) {
            None => fail!(UnknownClient, "unknown client {client}: say hello first"),
            Some(None) => fail!(NotFound, "no workspace: open one"),
            Some(Some(id)) => self
                .session(id)
                .ok_or_else(|| err(Code::NotFound, format!("workspace {id} was closed"))),
        }
    }

    pub fn dispatch(&self, client: u64, op: &str, m: &Value) -> Result<Value> {
        let flag = |k: &str| m.get(k).and_then(Value::as_bool).unwrap_or(false);
        let text = |k: &str| {
            m.get(k)
                .and_then(Value::as_str)
                .filter(|c| !c.trim().is_empty())
        };
        Ok(match op {
            "hello" => {
                // The workspace asked for, or the first one; none when it
                // was closed (the client offers the others).
                let session = match m.get("workspace").and_then(Value::as_u64) {
                    Some(id) => self.session(id),
                    None => self.sessions.lock().unwrap().values().next().cloned(),
                };
                let id = self.hub.register(session.as_ref().map(|s| s.id));
                json!({
                    "client": id,
                    "workspace": session.map(|s| s.hello()),
                    "workspaces": self.list(),
                    "folders": self.folders(),
                    "machines": self.all_machines(),
                })
            }
            "workspace.list" => json!({ "workspaces": self.list(), "folders": self.folders() }),
            "machine.list" => json!({ "machines": self.all_machines() }),
            "machine.add" => {
                self.known(client)?;
                let machine: Machine = serde_json::from_value(m.clone())
                    .map_err(|e| err(Code::BadRequest, format!("machine: {e}")))?;
                json!({ "machines": self.add_machine(machine)? })
            }
            "machine.remove" => {
                self.known(client)?;
                json!({ "machines": self.remove_machine(s(m, "name")?) })
            }
            "machine.browse" => {
                self.known(client)?;
                let host: HostSpec = match m.get("host") {
                    Some(h) => serde_json::from_value(h.clone())
                        .map_err(|e| err(Code::BadRequest, format!("host: {e}")))?,
                    None => HostSpec::Local,
                };
                self.browse(&host, m.get("path").and_then(Value::as_str).unwrap_or(""))?
            }
            "workspace.open" => {
                self.known(client)?;
                let host: HostSpec = match m.get("host") {
                    Some(h) => serde_json::from_value(h.clone())
                        .map_err(|e| err(Code::BadRequest, format!("host: {e}")))?,
                    None => HostSpec::Local,
                };
                let worktree = m
                    .get("worktree")
                    .filter(|w| w.is_object())
                    .map(|w| WorktreePlan {
                        branch: w
                            .get("branch")
                            .and_then(Value::as_str)
                            .filter(|b| !b.trim().is_empty()),
                        base: w
                            .get("base")
                            .and_then(Value::as_str)
                            .filter(|b| !b.trim().is_empty()),
                    });
                let (session, agent) = self.open(&Open {
                    host,
                    root: s(m, "root")?,
                    trusted: flag("trusted"),
                    agent: text("agent"),
                    agent_resume: text("agent_resume"),
                    worktree,
                    name: text("name"),
                    folder: m.get("folder").and_then(Value::as_str).unwrap_or(""),
                })?;
                json!({ "workspace": session.entry(), "agent": agent })
            }
            "workspace.close" => {
                self.known(client)?;
                self.close(u(m, "workspace")?, flag("discard"), flag("remove_worktree"))?;
                json!({})
            }
            "workspace.retry" => {
                self.known(client)?;
                json!({ "workspace": self.retry(u(m, "workspace")?)? })
            }
            "workspace.set" => {
                self.known(client)?;
                json!({ "workspace": self.set(u(m, "workspace")?, m)? })
            }
            "agents.scan" => {
                // Agent sessions run outside the editor on a host
                // (default: this machine), to import as workspaces.
                self.known(client)?;
                let host: HostSpec = match m.get("host") {
                    Some(h) => serde_json::from_value(h.clone())
                        .map_err(|e| err(Code::BadRequest, format!("host: {e}")))?,
                    None => HostSpec::Local,
                };
                let authority = authority::connect(&host, self.slow)
                    .map_err(|e| err(Code::Failed, format!("cannot reach the host: {e}")))?;
                let fs = authority.fs();
                let home = PathBuf::from(fs.home_dir()?);
                json!({ "sessions": crate::agents::scan(&fs, &home) })
            }
            "folder.create" | "folder.rename" | "folder.delete" => {
                self.known(client)?;
                self.folder(op, m)?
            }
            // Skins and themes: a user's own come from the viewed
            // workspace's host; without one, the built-in ones.
            "skin.list" | "theme.list" | "theme.get" => {
                self.known(client)?;
                let session = self.viewed(client).ok();
                let ws = session.as_ref().map(|s| &*s.ws);
                match op {
                    "skin.list" => crate::skins::list(ws),
                    "theme.list" => crate::themes::list(ws),
                    _ => crate::themes::get(ws, s(m, "key")?)?,
                }
            }
            _ => self.viewed(client)?.dispatch(client, op, m)?,
        })
    }

    fn known(&self, client: u64) -> Result<()> {
        if self.hub.workspace_of(client).is_none() {
            fail!(UnknownClient, "unknown client {client}: say hello first");
        }
        Ok(())
    }

    /// A client's event stream ended: what it opened in its workspace is
    /// released, and it is forgotten.
    pub fn drop_client(&self, client: u64) {
        if let Some(Some(id)) = self.hub.workspace_of(client) {
            if let Some(s) = self.session(id) {
                s.drop_client(client);
            }
        }
        self.hub.forget(client);
    }

    /// The server is exiting: the registry is written down (with when each
    /// workspace was last worked in), and every workspace stops what it
    /// started.
    pub fn shutdown(&self) {
        self.save();
        let sessions: Vec<Arc<Session>> = std::mem::take(&mut *self.sessions.lock().unwrap())
            .into_values()
            .collect();
        for s in sessions {
            s.shutdown();
        }
    }
}

/// A workspace that could not be opened again, as the dock lists it.
fn failed_entry(r: &Record, why: &str) -> Value {
    let name = Path::new(&r.root)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| r.root.clone());
    json!({
        "id": r.id, "name": name, "label": r.meta.name, "root": r.root, "host": r.meta.host.label(),
        "folder": r.meta.folder, "pinned": r.meta.pinned, "created": r.meta.created,
        "worked": r.meta.worked, "worktree": r.meta.worktree, "status": null, "error": why,
    })
}

/// Make a new git worktree of the repository `project` is in: on a new
/// branch `plan.branch` (default: `<repo>-N`, the first free) cut from
/// `plan.base` (default: the remote's default branch, else `HEAD`), in
/// `~/.local/share/fresh-poc/worktrees/<repo>/<name>` on the host. Returns
/// the worktree's folder.
fn make_worktree(
    authority: &dyn Authority,
    fs: &Fs,
    project: &Path,
    plan: &WorktreePlan,
) -> Result<(PathBuf, Worktree)> {
    let top = git(authority, &["rev-parse", "--show-toplevel"], project).map_err(|_| {
        err(
            Code::BadRequest,
            format!("{} is not in a git repository", project.display()),
        )
    })?;
    // The main repository, also when `project` is itself a worktree.
    let common = git(
        authority,
        &["rev-parse", "--path-format=absolute", "--git-common-dir"],
        project,
    )?;
    let repo = match common.strip_suffix("/.git") {
        Some(r) => r.to_string(),
        None => top,
    };
    let repo_name = Path::new(&repo)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "repo".into());
    let dir = PathBuf::from(fs.home_dir()?)
        .join(".local/share/fresh-poc/worktrees")
        .join(&repo_name);
    let exists = |p: &Path| fs.exists(workspace::host(p).as_host_path());
    let branch = match plan.branch {
        Some(b) => b.trim().to_string(),
        None => (1..)
            .map(|n| format!("{repo_name}-{n}"))
            .find(|n| {
                !exists(&dir.join(n))
                    && git(
                        authority,
                        &[
                            "rev-parse",
                            "--verify",
                            "--quiet",
                            &format!("refs/heads/{n}"),
                        ],
                        Path::new(&repo),
                    )
                    .is_err()
            })
            .unwrap_or_default(),
    };
    let base = match plan.base {
        Some(b) => b.to_string(),
        None => git(
            authority,
            &[
                "symbolic-ref",
                "--quiet",
                "--short",
                "refs/remotes/origin/HEAD",
            ],
            Path::new(&repo),
        )
        .unwrap_or_else(|_| "HEAD".into()),
    };
    let path = dir.join(&branch);
    if exists(&path) {
        fail!(Exists, "{} exists", path.display());
    }
    fs.create_dir_all(workspace::host(&dir).as_host_path())?;
    let target = path.to_string_lossy().into_owned();
    git(
        authority,
        &[
            "-C", &repo, "worktree", "add", &target, "-b", &branch, &base,
        ],
        Path::new(&repo),
    )?;
    let root = fs
        .canonicalize(workspace::host(&path).as_host_path())
        .map(PathBuf::from)?;
    Ok((root, Worktree { repo, branch }))
}

/// A container workspace's files are read here and its processes run in the
/// container, against the same paths: its root must be bind-mounted at its
/// own path there.
fn check_mounted(authority: &dyn Authority, root: &Path) -> Result<()> {
    let seen = authority
        .command(
            "sh",
            &[
                "-c".into(),
                "if test -d \"$1\"; then echo folder; else echo absent; fi".into(),
                "sh".into(),
                root.to_string_lossy().into(),
            ],
            Path::new("/"),
        )
        .0
        .stdin(std::process::Stdio::null())
        .output()
        .map_err(|e| anyhow!("{}: cannot run docker: {e}", authority.name()))?;
    match String::from_utf8_lossy(&seen.stdout).trim() {
        "folder" => Ok(()),
        "absent" => fail!(
            BadRequest,
            "{}: {} is not a folder in the container; bind-mount it at the same path",
            authority.name(),
            root.display()
        ),
        _ => fail!(
            Failed,
            "{}: {}",
            authority.name(),
            String::from_utf8_lossy(&seen.stderr).trim()
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::code_of;
    use std::time::{Duration, Instant};
    use tokio::sync::mpsc::UnboundedReceiver;

    fn open(orch: &Orchestrator, root: &Path, trusted: bool) -> Arc<Session> {
        orch.open(&Open::new(HostSpec::Local, root.to_str().unwrap(), trusted))
            .unwrap()
            .0
    }

    /// A client viewing workspace `ws`, with its event stream open.
    fn client(orch: &Orchestrator, ws: u64) -> (u64, UnboundedReceiver<String>) {
        let hello = orch
            .dispatch(0, "hello", &json!({ "workspace": ws }))
            .unwrap();
        let id = hello["client"].as_u64().unwrap();
        (id, orch.hub.take_stream(id).unwrap())
    }

    fn events(rx: &mut UnboundedReceiver<String>) -> Vec<Value> {
        let mut out = Vec::new();
        while let Ok(m) = rx.try_recv() {
            out.push(serde_json::from_str(&m).unwrap());
        }
        out
    }

    fn until(mut f: impl FnMut() -> bool) -> bool {
        let end = Instant::now() + Duration::from_secs(10);
        while Instant::now() < end {
            if f() {
                return true;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        false
    }

    #[test]
    fn ssh_config_hosts_are_its_aliases() {
        let home = tempfile::tempdir().unwrap();
        let ssh = home.path().join(".ssh");
        std::fs::create_dir_all(&ssh).unwrap();
        std::fs::write(
            ssh.join("config"),
            "# mine\nHost devbox build-*\n  HostName 10.0.0.5\n  User me\nHost *\n  ServerAliveInterval 30\n\
             Include extra.conf\nhost=gpu !nothere\nHost devbox\n",
        )
        .unwrap();
        std::fs::write(ssh.join("extra.conf"), "Host jump\n  Port 2222\n").unwrap();
        let fs = authority::Local::new().fs();
        let names: Vec<String> = ssh_config_hosts(&fs, home.path())
            .into_iter()
            .map(|m| {
                assert!(m.ssh_config);
                assert_eq!(
                    m.host,
                    HostSpec::Ssh {
                        target: m.name.clone(),
                        identity: None,
                        options: Vec::new()
                    }
                );
                m.name
            })
            .collect();
        assert_eq!(names, ["devbox", "jump", "gpu"]);
        // No config: no hosts.
        let empty = tempfile::tempdir().unwrap();
        assert!(ssh_config_hosts(&fs, empty.path()).is_empty());
    }

    #[test]
    fn workspaces_are_isolated_from_each_other() {
        let (d1, d2) = (tempfile::tempdir().unwrap(), tempfile::tempdir().unwrap());
        std::fs::write(d1.path().join("a.txt"), "one\n").unwrap();
        std::fs::write(d2.path().join("a.txt"), "two\n").unwrap();
        let orch = Orchestrator::new(false, None);
        let w1 = open(&orch, d1.path(), true);
        let w2 = open(&orch, d2.path(), true);
        assert_ne!(w1.id, w2.id);
        let ids: Vec<u64> = orch
            .list()
            .iter()
            .map(|e| e["id"].as_u64().unwrap())
            .collect();
        assert_eq!(ids, [w1.id, w2.id], "in the order opened");

        let (c1, mut rx1) = client(&orch, w1.id);
        let (c2, mut rx2) = client(&orch, w2.id);
        // The same relative path is each one's own file.
        let text = |c: u64| {
            let doc = orch
                .dispatch(c, "doc.open", &json!({"path": "a.txt"}))
                .unwrap()["doc"]
                .clone();
            orch.dispatch(c, "doc.text", &json!({"doc": doc, "start": 0, "end": 4}))
                .unwrap()["text"]
                .clone()
        };
        assert_eq!((text(c1), text(c2)), (json!("one\n"), json!("two\n")));

        // A terminal's output goes to its workspace's clients only.
        let t = orch
            .dispatch(c2, "term.open", &json!({"command": "echo hi from two"}))
            .unwrap()["term"]
            .clone();
        assert!(until(|| events(&mut rx2)
            .iter()
            .any(|e| e["ev"] == "term.exit" && e["term"] == t)));
        assert!(events(&mut rx1)
            .iter()
            .all(|e| !e["ev"].as_str().unwrap().starts_with("term.")));
        let attach = orch.dispatch(c1, "term.attach", &json!({ "term": t }));
        assert_eq!(code_of(&attach.unwrap_err()), Code::NotFound);

        // Status: each its own; every client hears every workspace's.
        let st = w1.status();
        assert_eq!(
            (st["docs"].clone(), st["viewers"].clone()),
            (json!(1), json!(1))
        );
        assert_eq!(w2.status()["terminals"][0]["state"], "exited");
        w2.publish_status();
        assert!(events(&mut rx1)
            .iter()
            .any(|e| e["ev"] == "workspace.status" && e["workspace"] == w2.id));
    }

    #[test]
    fn opening_an_open_folder_gives_its_workspace() {
        let d = tempfile::tempdir().unwrap();
        let orch = Orchestrator::new(false, None);
        let w = open(&orch, d.path(), false);
        let (c, mut rx) = client(&orch, w.id);
        let again = orch
            .dispatch(
                c,
                "workspace.open",
                &json!({"root": d.path(), "host": {"kind": "local"}, "trusted": true}),
            )
            .unwrap();
        assert_eq!(again["workspace"]["id"], w.id);
        assert_eq!(again["workspace"]["status"]["trusted"], false, "as it was");
        assert_eq!(orch.list().len(), 1);
        assert!(events(&mut rx).iter().all(|e| e["ev"] != "workspace.added"));

        let missing = orch
            .dispatch(c, "workspace.open", &json!({"root": d.path().join("nope")}))
            .unwrap_err();
        assert_eq!(code_of(&missing), Code::NotFound);
        std::fs::write(d.path().join("f"), "").unwrap();
        let file = orch
            .dispatch(c, "workspace.open", &json!({"root": d.path().join("f")}))
            .unwrap_err();
        assert_eq!(code_of(&file), Code::BadRequest);
        let agent = orch
            .dispatch(
                c,
                "workspace.open",
                &json!({"root": d.path(), "agent": "sh"}),
            )
            .unwrap_err();
        assert_eq!(code_of(&agent), Code::Untrusted, "an agent needs trust");
    }

    #[test]
    fn closing_a_workspace_stops_it_and_tells_every_client() {
        let (d1, d2) = (tempfile::tempdir().unwrap(), tempfile::tempdir().unwrap());
        std::fs::write(d1.path().join("a.txt"), "a\n").unwrap();
        let orch = Orchestrator::new(false, None);
        let w1 = open(&orch, d1.path(), true);
        let w2 = open(&orch, d2.path(), true);
        let (c1, mut rx1) = client(&orch, w1.id);
        let (c2, mut rx2) = client(&orch, w2.id);
        let doc = orch
            .dispatch(c1, "doc.open", &json!({"path": "a.txt"}))
            .unwrap()["doc"]
            .clone();
        orch.dispatch(
            c1,
            "doc.edit",
            &json!({"doc": doc, "base": 0, "ops": [{"t": "ins", "pos": 0, "text": "x"}]}),
        )
        .unwrap();
        orch.dispatch(c1, "term.open", &json!({"command": "sleep 7303"}))
            .unwrap();

        let close = json!({ "workspace": w1.id });
        let e = orch.dispatch(c2, "workspace.close", &close).unwrap_err();
        assert_eq!(code_of(&e), Code::Unsaved);
        orch.dispatch(
            c2,
            "workspace.close",
            &json!({"workspace": w1.id, "discard": true}),
        )
        .unwrap();
        assert_eq!(
            w1.status()["terminals"],
            json!([]),
            "its terminals are closed"
        );
        for rx in [&mut rx1, &mut rx2] {
            assert!(events(rx)
                .iter()
                .any(|e| e["ev"] == "workspace.removed" && e["workspace"] == w1.id));
        }
        let gone = orch.dispatch(c1, "fs.list", &json!({})).unwrap_err();
        assert_eq!(code_of(&gone), Code::NotFound);
        assert_eq!(orch.list().len(), 1);

        // A client asking for a closed workspace views none: it has the
        // list, and the built-in skins.
        let hello = orch
            .dispatch(0, "hello", &json!({ "workspace": w1.id }))
            .unwrap();
        assert!(hello["workspace"].is_null());
        assert_eq!(hello["workspaces"].as_array().unwrap().len(), 1);
        let c = hello["client"].as_u64().unwrap();
        assert!(orch.dispatch(c, "skin.list", &json!({})).unwrap()["skins"]
            .as_array()
            .is_some_and(|s| !s.is_empty()));
        assert_eq!(
            code_of(&orch.dispatch(c, "fs.list", &json!({})).unwrap_err()),
            Code::NotFound
        );
        assert_eq!(
            code_of(&orch.dispatch(999, "fs.list", &json!({})).unwrap_err()),
            Code::UnknownClient
        );
    }

    #[test]
    fn an_agent_session_reports_its_state_and_asks_for_attention() {
        let d = tempfile::tempdir().unwrap();
        let orch = Orchestrator::new(false, None);
        let (w, agent) = orch
            .open(&Open {
                agent: Some(
                    "echo working; sleep 1; printf 'Apply the change? [y/n] '; read a; echo got $a; printf '\\a'; sleep 30",
                ),
                ..Open::new(HostSpec::Local, d.path().to_str().unwrap(), true)
            })
            .unwrap();
        let agent = agent.unwrap();
        // Named after its command line, cut short.
        let name = "echo working; sleep 1; printf '…";
        let state = |w: &Session| {
            w.publish_status();
            let st = w.status();
            let t = st["terminals"][0].clone();
            assert_eq!(
                (t["id"].as_u64(), t["agent"].clone(), t["name"].clone()),
                (Some(agent), json!(true), json!(name))
            );
            (
                t["state"].as_str().unwrap().to_string(),
                st["attention"].clone(),
            )
        };
        assert!(until(|| state(&w).0 == "working"));
        assert!(until(|| state(&w).0 == "waiting"), "a question on screen");
        assert_eq!(state(&w).1, json!(format!("{name} is waiting for you")));
        w.seen();
        assert!(state(&w).1.is_null());
        // Answered: it works, then rings the bell.
        let (c, _rx) = client(&orch, w.id);
        orch.dispatch(c, "term.input", &json!({"term": agent, "data": "y\n"}))
            .unwrap();
        assert!(until(
            || state(&w).1 == json!(format!("{name} rang the bell"))
        ));
        orch.dispatch(c, "workspace.seen", &json!({})).unwrap();
        assert!(state(&w).1.is_null());
        orch.shutdown();
    }

    fn store(dir: &Path) -> Option<Store> {
        Some(Store {
            fs: Arc::new(fresh_editor_core::model::filesystem::StdFileSystem),
            path: dir.join("orchestrator.json"),
        })
    }

    #[test]
    fn the_registry_comes_back_after_a_restart() {
        let (state, d1, d2) = (
            tempfile::tempdir().unwrap(),
            tempfile::tempdir().unwrap(),
            tempfile::tempdir().unwrap(),
        );
        let (w1, w2) = {
            let orch = Orchestrator::new(false, store(state.path()));
            let w1 = open(&orch, d1.path(), true).id;
            let w2 = open(&orch, d2.path(), false).id;
            let (c, mut rx) = client(&orch, w1);
            orch.dispatch(c, "folder.create", &json!({"path": "Features/Login"}))
                .unwrap();
            assert_eq!(orch.folders(), ["Features", "Features/Login"]);
            let set = json!({"workspace": w1, "name": "login fix", "folder": "Features/Login", "pinned": true});
            let e = orch.dispatch(c, "workspace.set", &set).unwrap()["workspace"].clone();
            assert_eq!(
                (e["label"].clone(), e["folder"].clone(), e["pinned"].clone()),
                (json!("login fix"), json!("Features/Login"), json!(true))
            );
            let evs = events(&mut rx);
            assert!(evs.iter().any(|e| e["ev"] == "orchestrator.folders"));
            assert!(evs
                .iter()
                .any(|e| e["ev"] == "workspace.changed" && e["workspace"]["id"] == w1));
            let nowhere = json!({"workspace": w1, "folder": "Nope"});
            assert_eq!(
                code_of(&orch.dispatch(c, "workspace.set", &nowhere).unwrap_err()),
                Code::NotFound
            );
            // A folder renamed carries its subfolders and workspaces.
            orch.dispatch(
                c,
                "folder.rename",
                &json!({"from": "Features", "to": "Work"}),
            )
            .unwrap();
            assert_eq!(orch.folders(), ["Work", "Work/Login"]);
            assert_eq!(orch.session(w1).unwrap().meta().folder, "Work/Login");
            orch.shutdown();
            (w1, w2)
        };
        // d2 is gone by the next start: listed with why, retried once back.
        let d2_path = d2.path().to_path_buf();
        drop(d2);
        let orch = Orchestrator::new(false, store(state.path()));
        orch.restore();
        assert_eq!(orch.folders(), ["Work", "Work/Login"]);
        let list = orch.list();
        assert_eq!(list.len(), 2);
        assert_eq!(
            (
                list[0]["id"].as_u64(),
                list[0]["label"].clone(),
                list[0]["folder"].clone()
            ),
            (Some(w1), json!("login fix"), json!("Work/Login"))
        );
        assert_eq!(list[0]["status"]["trusted"], true);
        assert_eq!(list[1]["id"].as_u64(), Some(w2));
        assert!(list[1]["status"].is_null());
        assert!(list[1]["error"].as_str().unwrap().contains("not a folder"));
        let (c, _rx) = client(&orch, w1);
        let retry = json!({ "workspace": w2 });
        assert!(orch.dispatch(c, "workspace.retry", &retry).is_err());
        std::fs::create_dir_all(&d2_path).unwrap();
        let back = orch.dispatch(c, "workspace.retry", &retry).unwrap();
        assert_eq!(back["workspace"]["status"]["trusted"], false, "as it was");
        // A new workspace takes the next id; deleting a folder moves its
        // workspaces up.
        let d3 = tempfile::tempdir().unwrap();
        assert!(open(&orch, d3.path(), false).id > w2);
        orch.dispatch(c, "folder.delete", &json!({"path": "Work"}))
            .unwrap();
        assert_eq!(orch.folders(), ["Login"]);
        assert_eq!(orch.session(w1).unwrap().meta().folder, "Login");
        std::fs::remove_dir_all(&d2_path).unwrap();
    }

    #[test]
    fn a_workspace_on_a_new_worktree_and_its_removal() {
        let tmp = tempfile::tempdir().unwrap();
        // A name of its own: the worktrees go under the host's home.
        let name = format!(
            "p{}",
            tmp.path()
                .file_name()
                .unwrap()
                .to_string_lossy()
                .trim_start_matches('.')
        );
        let repo = tmp.path().join(&name);
        std::fs::create_dir(&repo).unwrap();
        let git = |args: &[&str]| {
            let out = std::process::Command::new("git")
                .args(args)
                .current_dir(&repo)
                .output()
                .unwrap();
            assert!(
                out.status.success(),
                "{}",
                String::from_utf8_lossy(&out.stderr)
            );
        };
        git(&["init", "-q", "-b", "main"]);
        std::fs::write(repo.join("a.txt"), "a\n").unwrap();
        git(&["add", "."]);
        git(&[
            "-c",
            "user.name=t",
            "-c",
            "user.email=t@t",
            "commit",
            "-q",
            "-m",
            "a",
        ]);

        let orch = Orchestrator::new(false, None);
        let root = repo.to_str().unwrap();
        let plan = || {
            Some(WorktreePlan {
                branch: None,
                base: None,
            })
        };
        let restricted = Open {
            worktree: plan(),
            ..Open::new(HostSpec::Local, root, false)
        };
        assert_eq!(
            code_of(&orch.open(&restricted).err().unwrap()),
            Code::Untrusted
        );
        let (w, _) = orch
            .open(&Open {
                worktree: plan(),
                ..Open::new(HostSpec::Local, root, true)
            })
            .unwrap();
        let wt = w.meta().worktree.unwrap();
        assert_eq!(wt.branch, format!("{name}-1"));
        assert_eq!(Path::new(&wt.repo), repo.canonicalize().unwrap());
        let base = w.ws.root.parent().unwrap().to_path_buf();
        assert!(base.ends_with(format!(".local/share/fresh-poc/worktrees/{name}")));
        assert!(w.ws.root.join("a.txt").exists());
        // The next one, from inside the first worktree: the same repository.
        let inside = w.ws.root.to_string_lossy().into_owned();
        let second = Open {
            worktree: Some(WorktreePlan {
                branch: Some("fix"),
                base: Some("main"),
            }),
            ..Open::new(HostSpec::Local, &inside, true)
        };
        let (w2, _) = orch.open(&second).unwrap();
        assert_eq!(w2.meta().worktree.unwrap().branch, "fix");
        assert_eq!(w2.meta().worktree.unwrap().repo, wt.repo);
        let again = Open {
            worktree: Some(WorktreePlan {
                branch: Some("fix"),
                base: None,
            }),
            ..Open::new(HostSpec::Local, root, true)
        };
        assert_eq!(code_of(&orch.open(&again).err().unwrap()), Code::Exists);

        // Closing with the worktree removes it; its branch stays.
        let dir = w.ws.root.clone();
        orch.close(w.id, false, true).unwrap();
        assert!(!dir.exists());
        git(&["rev-parse", "--verify", "--quiet", &format!("{name}-1")]);
        orch.close(w2.id, false, true).unwrap();
        let plain = open(&orch, &repo, true);
        assert_eq!(
            code_of(&orch.close(plain.id, false, true).unwrap_err()),
            Code::BadRequest,
            "not a worktree"
        );
        std::fs::remove_dir_all(base).unwrap();
    }

    #[test]
    fn running_agents_are_taken_up_again_after_a_restart() {
        let (state, d) = (tempfile::tempdir().unwrap(), tempfile::tempdir().unwrap());
        let root = d.path().to_str().unwrap();
        let id = {
            let orch = Orchestrator::new(false, store(state.path()));
            let (w, _) = orch
                .open(&Open {
                    agent: Some("sleep 7304"),
                    agent_resume: Some("echo resumed; sleep 7305"),
                    ..Open::new(HostSpec::Local, root, true)
                })
                .unwrap();
            // An agent that ended is not taken up again.
            w.open_terminal((80, 24), None, Some("true"), Some("echo never"))
                .unwrap();
            assert!(until(|| w.status()["terminals"][1]["state"] == "exited"));
            orch.shutdown();
            w.id
        };
        let orch = Orchestrator::new(false, store(state.path()));
        orch.restore();
        let w = orch.session(id).unwrap();
        let terms = w.status()["terminals"].clone();
        let commands: Vec<&str> = terms
            .as_array()
            .unwrap()
            .iter()
            .map(|t| t["command"].as_str().unwrap())
            .collect();
        assert_eq!(commands, ["echo resumed; sleep 7305"]);
        assert_eq!(terms[0]["resume"], "echo resumed; sleep 7305");
        orch.shutdown();
    }
}
