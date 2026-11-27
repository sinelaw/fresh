//! An open workspace ("session"): a root on a host, and everything the
//! server holds for it — its documents and their edit logs, terminals,
//! processes, watches and language servers, and whether it is trusted.
//!
//! Every session is independent: its clients (the browser tabs viewing it)
//! see only its events, and closing it stops what it started on its host.
//! The orchestrator (`src/orchestrator.rs`) keeps the sessions and routes
//! each client's operations to the one it views.
//!
//! A session also reports its **status**, for the orchestrator's overview:
//! open and unsaved documents, terminals and what each is doing, processes,
//! diagnostics, the git branch, how many clients view it, and whether it
//! needs the user's attention — a terminal rang the bell, or an agent
//! session stopped working (it finished, exited, or waits on a question).
//! Attention stays until a client viewing the workspace has seen it
//! (`workspace.seen`).

use crate::docs::{self, Docs};
use crate::error::{err, Code};
use crate::hub::{Hub, Room};
use crate::languages;
use crate::orchestrator::{now_ms, Meta, Record};
use crate::workspace::{self, Workspace};
use crate::{lsp, lsp_shapes, ot, process, terminal};
use anyhow::Result;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// How often a session's status is looked at.
const STATUS_EVERY: Duration = Duration::from_millis(1000);

pub fn arg<'a>(m: &'a Value, k: &str) -> Result<&'a Value> {
    m.get(k)
        .filter(|v| !v.is_null())
        .ok_or_else(|| err(Code::BadRequest, format!("missing {k}")))
}
pub fn u(m: &Value, k: &str) -> Result<u64> {
    arg(m, k)?
        .as_u64()
        .ok_or_else(|| err(Code::BadRequest, format!("{k} is not a number")))
}
pub fn s<'a>(m: &'a Value, k: &str) -> Result<&'a str> {
    arg(m, k)?
        .as_str()
        .ok_or_else(|| err(Code::BadRequest, format!("{k} is not a string")))
}

/// Each document's diagnostics counts (errors, warnings), from its latest
/// report.
type DiagCounts = Arc<Mutex<HashMap<u64, (usize, usize)>>>;

/// What the status looked at last time: each terminal's state and bells
/// (to see what changed), and the status last sent.
#[derive(Default)]
struct Seen {
    /// Each terminal's state and bells when last looked at.
    terms: HashMap<u64, (terminal::State, u64)>,
    sent: Option<Value>,
}

pub struct Session {
    pub id: u64,
    pub ws: Arc<Workspace>,
    hub: Arc<Hub>,
    room: Room,
    docs: Arc<Mutex<Docs>>,
    terms: terminal::Terms,
    procs: process::Procs,
    watches: process::Watches,
    lsp: lsp::Lsp,
    trusted: AtomicBool,
    diags: DiagCounts,
    /// Why the workspace needs the user, until a viewer has seen it.
    attention: Mutex<Option<String>>,
    seen: Mutex<Seen>,
    /// What the user gave it (name, folder, pin), its host, when it was
    /// made and last worked in, its worktree.
    meta: Mutex<Meta>,
    /// Its host's CPU counters at the last `host.stats` (none before the
    /// first), which the next one's CPU use is measured from.
    cpu: crate::skins::CpuSample,
}

impl Session {
    pub fn new(
        id: u64,
        ws: Arc<Workspace>,
        trusted: bool,
        meta: Meta,
        hub: Arc<Hub>,
    ) -> Arc<Session> {
        let room = Room::new(hub.clone(), id);
        let docs: Arc<Mutex<Docs>> = Arc::default();
        let diags: DiagCounts = Arc::default();
        let on_notify: lsp::OnNotify = {
            let room = room.clone();
            let docs = docs.clone();
            let diags = diags.clone();
            Arc::new(move |lang, event| match event {
                lsp::ServerEvent::Notification("textDocument/publishDiagnostics", params) => {
                    publish_diagnostics(&room, &docs, &diags, params)
                }
                lsp::ServerEvent::Notification(..) => {}
                // Its viewers stop asking it (it stays failed until
                // restarted; `lsp.status` says why).
                lsp::ServerEvent::Ended => room.broadcast(&json!({
                    "ev": "lsp.ended", "language": lang,
                    "why": format!("the {lang} language server ended"),
                })),
            })
        };
        let session = Arc::new(Session {
            id,
            lsp: lsp::Lsp::new(ws.clone(), on_notify),
            ws: ws.clone(),
            hub,
            room: room.clone(),
            docs,
            terms: terminal::Terms::default(),
            procs: process::Procs::default(),
            watches: process::Watches::default(),
            trusted: AtomicBool::new(trusted),
            diags,
            attention: Mutex::new(None),
            seen: Mutex::default(),
            meta: Mutex::new(meta),
            cpu: crate::skins::CpuSample::default(),
        });
        session.watches.start(room, ws);
        Session::follow_disk(&session);
        Session::follow_status(&session);
        session
    }

    pub fn trusted(&self) -> bool {
        self.trusted.load(Ordering::SeqCst)
    }

    /// The workspace's name: its root folder's.
    pub fn name(&self) -> String {
        self.ws
            .root
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| self.ws.root.to_string_lossy().into_owned())
    }

    pub fn meta(&self) -> Meta {
        self.meta.lock().unwrap().clone()
    }

    pub fn edit_meta(&self, f: impl FnOnce(&mut Meta)) {
        f(&mut self.meta.lock().unwrap());
    }

    /// The workspace as the registry keeps it, with how to take up again
    /// the agents running in it now.
    pub fn record(&self) -> Record {
        let mut meta = self.meta();
        meta.resume = self
            .terms
            .list()
            .iter()
            .filter(|t| t["state"] != "exited")
            .filter_map(|t| t["resume"].as_str().map(str::to_string))
            .collect();
        Record {
            id: self.id,
            root: self.ws.root.to_string_lossy().into_owned(),
            trusted: self.trusted(),
            meta,
        }
    }

    /// The workspace as the orchestrator lists it: `name` is its folder's,
    /// `label` the user's name for it.
    pub fn entry(&self) -> Value {
        let m = self.meta();
        json!({
            "id": self.id, "name": self.name(), "label": m.name, "root": self.ws.root.to_string_lossy(),
            "host": m.host, "folder": m.folder, "pinned": m.pinned,
            "created": m.created, "worktree": m.worktree, "status": self.status(), "error": null,
        })
    }

    /// What a client viewing the workspace starts from.
    pub fn hello(&self) -> Value {
        let trusted = self.trusted();
        json!({
            "id": self.id, "name": self.name(), "root": self.ws.root.to_string_lossy(),
            "host": self.meta().host, "trusted": trusted,
            "terminals": self.terms.list(),
            // Finding the servers runs a probe on the host: only once
            // trusted (then sent with `workspace.trusted`).
            "lsp": if trusted { self.lsp.languages() } else { vec![] },
        })
    }

    /// Unsaved documents.
    pub fn modified(&self) -> usize {
        self.docs
            .lock()
            .unwrap()
            .docs
            .values()
            .filter(|d| d.buffer.is_modified())
            .count()
    }

    /// The workspace's live status (see the module's documentation).
    pub fn status(&self) -> Value {
        let (open, modified, ids): (usize, usize, Vec<u64>) = {
            let docs = self.docs.lock().unwrap();
            (
                docs.docs.len(),
                docs.docs
                    .values()
                    .filter(|d| d.buffer.is_modified())
                    .count(),
                docs.docs.keys().copied().collect(),
            )
        };
        let (errors, warnings) = {
            let mut diags = self.diags.lock().unwrap();
            diags.retain(|id, _| ids.contains(id));
            diags
                .values()
                .fold((0, 0), |(e, w), (de, dw)| (e + de, w + dw))
        };
        json!({
            "trusted": self.trusted(),
            "docs": open, "modified": modified,
            "terminals": self.terms.list(),
            "procs": self.procs.count(),
            "errors": errors, "warnings": warnings,
            "branch": git_branch(&self.ws),
            "viewers": self.room.viewers(),
            "worked": self.meta.lock().unwrap().worked,
            "attention": *self.attention.lock().unwrap(),
        })
    }

    /// Look at the status: raise attention for what changed in the
    /// terminals, and tell every client (`workspace.status`) when the status
    /// differs from the last one sent.
    pub fn publish_status(&self) {
        let mut seen = self.seen.lock().unwrap();
        let terms = self.terms.snapshots();
        for t in &terms {
            let name = &t.name;
            let reason = match seen.terms.get(&t.id).copied() {
                Some((_, b)) if t.bells > b => Some(format!("{name} rang the bell")),
                // An agent stopped working: it asks, it finished work, or
                // it ended.
                Some((s, _)) if t.agent() && s != t.state => match t.state {
                    terminal::State::Waiting => Some(format!("{name} is waiting for you")),
                    terminal::State::Done => Some(format!("{name} finished")),
                    terminal::State::Exited => Some(format!("{name} exited")),
                    terminal::State::Working | terminal::State::Idle | terminal::State::Unknown => {
                        None
                    }
                },
                _ => None,
            };
            if let Some(r) = reason {
                *self.attention.lock().unwrap() = Some(r);
            }
            seen.terms.insert(t.id, (t.state, t.bells));
        }
        seen.terms.retain(|id, _| terms.iter().any(|t| t.id == *id));
        let status = self.status();
        if seen.sent.as_ref() != Some(&status) {
            self.hub.broadcast(
                &json!({"ev": "workspace.status", "workspace": self.id, "status": status}),
            );
            seen.sent = Some(status);
        }
    }

    /// A viewer has seen what needed attention.
    pub fn seen(&self) {
        *self.attention.lock().unwrap() = None;
        self.terms.seen();
        self.publish_status();
    }

    /// Look at the status every [`STATUS_EVERY`] while the session lives.
    fn follow_status(session: &Arc<Session>) {
        let weak = Arc::downgrade(session);
        std::thread::spawn(move || loop {
            std::thread::sleep(STATUS_EVERY);
            let Some(s) = weak.upgrade() else { return };
            s.publish_status();
        });
    }

    pub fn dispatch(&self, client: u64, op: &str, m: &Value) -> Result<Value> {
        let ws = &self.ws;
        // An untrusted workspace runs nothing of its own: no processes, no
        // terminals, no language servers, until the user trusts it.
        let trusted = self.trusted();
        if !trusted && (op.starts_with("proc.") || op.starts_with("lsp.")) {
            fail!(
                Untrusted,
                "workspace is restricted: trust it to run processes, terminals and language servers"
            );
        }
        Ok(match op {
            "workspace.trust" => {
                if !self.trusted.swap(true, Ordering::SeqCst) {
                    // Documents opened while restricted get their language
                    // servers now.
                    let mut docs = self.docs.lock().unwrap();
                    let ids: Vec<u64> = docs.docs.keys().copied().collect();
                    for id in ids {
                        if let Ok(d) = docs.get(id) {
                            self.lsp_open(d)?;
                        }
                    }
                }
                self.room
                    .broadcast(&json!({"ev": "workspace.trusted", "lsp": self.lsp.languages()}));
                // Every client's list says so now, not at the next look.
                self.publish_status();
                json!({})
            }
            "workspace.seen" => {
                self.seen();
                json!({})
            }
            "host.stats" => crate::skins::host_stats(ws, &self.cpu),
            "fs.list" => {
                let p = m
                    .get("path")
                    .and_then(Value::as_str)
                    .map(|p| ws.resolve(p))
                    .unwrap_or(ws.root.clone());
                json!({ "entries": ws.list_dir(&p)? })
            }
            "fs.stat" => ws.stat(&ws.resolve(s(m, "path")?)),
            "fs.read" => json!({ "text": ws.read_file(&ws.resolve(s(m, "path")?), 16 << 20)? }),
            "fs.write" => {
                ws.write_file(&ws.resolve(s(m, "path")?), s(m, "text")?)?;
                json!({})
            }
            "fs.find" => json!({ "files": ws.find_files(s(m, "query")?, 100)? }),
            "fs.create" => {
                ws.create(
                    &ws.resolve(s(m, "path")?),
                    m.get("dir").and_then(Value::as_bool).unwrap_or(false),
                )?;
                json!({})
            }
            "fs.rename" => {
                let (from, to) = (ws.resolve(s(m, "from")?), ws.resolve(s(m, "to")?));
                self.rename(&from, &to)?;
                json!({})
            }
            "fs.delete" => {
                self.delete(&ws.resolve(s(m, "path")?))?;
                json!({})
            }
            "fs.copy" => {
                let to = ws.copy(&ws.resolve(s(m, "from")?), &ws.resolve(s(m, "to")?))?;
                json!({ "path": to.to_string_lossy() })
            }
            "search.project" => ws.search(
                s(m, "query")?,
                m.get("regex").and_then(Value::as_bool).unwrap_or(false),
                m.get("case").and_then(Value::as_bool).unwrap_or(false),
                m.get("whole").and_then(Value::as_bool).unwrap_or(false),
                m.get("max").and_then(Value::as_u64).unwrap_or(5000) as usize,
            )?,
            "fs.watch" => {
                json!({ "watch": self.watches.add(ws, client, ws.resolve(s(m, "path")?)) })
            }
            "fs.unwatch" => {
                self.watches.remove(u(m, "watch")?);
                json!({})
            }

            "doc.new" => {
                let mut docs = self.docs.lock().unwrap();
                let id = docs.create(client, ws.fs());
                docs.get(id)?.info()
            }
            "doc.open" => {
                let mut docs = self.docs.lock().unwrap();
                // An untitled document kept for its unsaved text.
                if let Some(n) = m.get("untitled").and_then(Value::as_u64) {
                    let id = docs.join_untitled(client, n)?;
                    return docs.get(id).map(|d| d.info());
                }
                let path = ws.resolve(s(m, "path")?);
                let (id, new) = docs.open(client, &path, ws.fs())?;
                let d = docs.get(id)?;
                if new && d.buffer.line_count().is_none() {
                    index_lines(self.docs.clone(), self.room.clone(), id);
                }
                if new && trusted {
                    self.lsp_open(d)?;
                }
                d.info()
            }
            "doc.close" => {
                // `discard`: give up unsaved changes, unless other clients
                // still hold the document (`others` says how many).
                let id = u(m, "doc")?;
                let discard = m.get("discard").and_then(Value::as_bool).unwrap_or(false);
                let mut docs = self.docs.lock().unwrap();
                match docs.close(client, id, discard) {
                    Some(d) => {
                        self.lsp_close(&d);
                        json!({ "dropped": true, "others": 0 })
                    }
                    None => {
                        let others = docs.get(id).map(|d| d.clients.len()).unwrap_or(0);
                        json!({ "dropped": false, "others": others })
                    }
                }
            }
            "doc.reload" => {
                // Give up unsaved edits (every client's) for the file as it
                // is on disk now; with `encoding`, read in that encoding
                // instead of the detected one.
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                let fs = ws.fs();
                let Some(path) = d.origin.path().map(Path::to_path_buf) else {
                    fail!(NotFound, "{} has no file", d.display());
                };
                let now = workspace::signature(&fs, &path);
                if now.is_none() {
                    fail!(NotFound, "{} is not on disk", path.display());
                }
                let fresh = match m.get("encoding").and_then(Value::as_str) {
                    Some(name) => {
                        let Some(enc) = docs::encoding_named(name) else {
                            fail!(BadRequest, "no encoding {name}");
                        };
                        if d.buffer.is_large_file() {
                            fail!(BadRequest, "{} is a large file, read as it is viewed; it is read in the detected encoding", path.display());
                        }
                        docs::Buffer::load_from_file_with_encoding(
                            &path,
                            enc,
                            fs,
                            docs::BufferConfig::default(),
                        )?
                    }
                    None => docs::Buffer::load_from_file(&path, docs::LARGE_FILE_THRESHOLD, fs)?,
                };
                if fresh.is_binary() {
                    fail!(Binary, "{} is now a binary file", path.display());
                }
                self.reload_with(d, fresh, now)?;
                d.info()
            }
            "doc.info" => self.docs.lock().unwrap().get(u(m, "doc")?)?.info(),
            "doc.formats" => json!({
                "languages": languages::catalog(),
                "encodings": docs::Encoding::all().iter().map(|e| json!({"name": e.display_name(), "description": e.description()})).collect::<Vec<_>>(),
                "line_endings": ["LF", "CRLF", "CR"],
            }),
            "doc.set" => self.set_format(m)?,
            "doc.lines" => {
                let count = m.get("count").and_then(Value::as_u64).unwrap_or(200) as usize;
                self.docs
                    .lock()
                    .unwrap()
                    .get(u(m, "doc")?)?
                    .lines_forward(u(m, "from")? as usize, count)?
            }
            "doc.lines_before" => {
                let count = m.get("count").and_then(Value::as_u64).unwrap_or(200) as usize;
                self.docs
                    .lock()
                    .unwrap()
                    .get(u(m, "doc")?)?
                    .lines_backward(u(m, "before")? as usize, count)?
            }
            "doc.window" => {
                // Lines before and after `around` in one read, so both halves
                // are of one version, at most `bytes` on either side.
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                let before = m.get("before").and_then(Value::as_u64).unwrap_or(100) as usize;
                let after = m.get("after").and_then(Value::as_u64).unwrap_or(400) as usize;
                // No cap asked for: as much as one read returns.
                let bytes = m.get("bytes").and_then(Value::as_u64).unwrap_or(u64::MAX);
                d.window(
                    u(m, "around")? as usize,
                    before,
                    after,
                    usize::try_from(bytes).unwrap_or(usize::MAX),
                )?
            }
            "doc.text" => {
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                json!({ "text": d.text(u(m, "start")? as usize, u(m, "end")? as usize)?, "version": d.version })
            }
            "doc.search" => {
                let backward = m.get("backward").and_then(Value::as_bool).unwrap_or(false);
                let at = self.docs.lock().unwrap().get(u(m, "doc")?)?.search(
                    s(m, "query")?,
                    u(m, "from")? as usize,
                    backward,
                )?;
                json!({ "pos": at })
            }
            "doc.line_start" => {
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                // Reads the one chunk holding the line when it is not loaded.
                json!({ "pos": d.buffer.resolve_line_byte_offset(u(m, "line")? as usize) })
            }
            "doc.edit" => {
                self.worked();
                self.edit(client, m)?
            }
            "doc.save" => {
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                let force = m.get("force").and_then(Value::as_bool).unwrap_or(false);
                d.save(&ws.recovery_dir(), force)?;
                let info = d.info();
                if let Some(server) = d.language_id().and_then(|l| self.lsp.running(l)) {
                    server.notify(
                        "textDocument/didSave",
                        json!({"textDocument": {"uri": d.uri()}}),
                    );
                }
                let ev = json!({"ev": "doc.saved", "doc": d.id, "info": info});
                for c in &d.clients {
                    self.room.send(*c, &ev);
                }
                info
            }
            "doc.save_as" => self.save_as(m)?,

            "term.open" => {
                let cols = m.get("cols").and_then(Value::as_u64).unwrap_or(80) as u16;
                let rows = m.get("rows").and_then(Value::as_u64).unwrap_or(24) as u16;
                let name = m.get("name").and_then(Value::as_str);
                let command = m
                    .get("command")
                    .and_then(Value::as_str)
                    .filter(|c| !c.trim().is_empty());
                let resume = m
                    .get("resume")
                    .and_then(Value::as_str)
                    .filter(|c| !c.trim().is_empty());
                json!({ "term": self.open_terminal((cols, rows), name, command, resume)? })
            }
            "term.input" => {
                self.worked();
                self.terms.input(u(m, "term")?, s(m, "data")?)?;
                json!({})
            }
            "term.resize" => {
                let (c, r) = (u(m, "cols")? as u16, u(m, "rows")? as u16);
                self.terms
                    .resize(&self.room, u(m, "term")?, c.max(2), r.max(2))?;
                json!({})
            }
            "term.attach" => self.terms.attach(u(m, "term")?)?,
            "term.close" => {
                self.terms.close(u(m, "term")?);
                json!({})
            }

            "proc.run" => {
                let args: Vec<String> = m
                    .get("args")
                    .and_then(Value::as_array)
                    .map(|a| {
                        a.iter()
                            .filter_map(|v| v.as_str().map(String::from))
                            .collect()
                    })
                    .unwrap_or_default();
                let cwd = m.get("cwd").and_then(Value::as_str);
                json!({ "proc": self.procs.run(self.room.clone(), ws, client, s(m, "program")?, &args, cwd)? })
            }
            "proc.kill" => json!({ "killed": self.procs.kill(u(m, "proc")?) }),

            "lsp.hover" | "lsp.definition" | "lsp.completion" => self.lsp_request(op, m)?,
            "lsp.call" => self.lsp_call(m)?,
            "lsp.status" => self.lsp.status(),
            "lsp.restart" => self.lsp_restart(s(m, "language")?)?,
            "lsp.stop" => self.lsp_stop(s(m, "language")?),

            _ => fail!(UnknownOp, "unknown op {op}"),
        })
    }

    /// The user typed into one of its files or terminals.
    fn worked(&self) {
        self.meta.lock().unwrap().worked = now_ms();
    }

    /// A terminal in the workspace: the user's shell, or `command` in it
    /// (an agent session). Refused while the workspace is restricted.
    pub fn open_terminal(
        &self,
        size: (u16, u16),
        name: Option<&str>,
        command: Option<&str>,
        resume: Option<&str>,
    ) -> Result<u64> {
        if !self.trusted() {
            fail!(
                Untrusted,
                "workspace is restricted: trust it to run terminals"
            );
        }
        let id = self
            .terms
            .open(self.room.clone(), &self.ws, size, name, command, resume)?;
        self.publish_status();
        Ok(id)
    }

    fn edit(&self, client: u64, m: &Value) -> Result<Value> {
        let id = u(m, "doc")?;
        let base = u(m, "base")?;
        let prims: Vec<ot::Prim> = serde_json::from_value(arg(m, "ops")?.clone())?;
        let mut docs = self.docs.lock().unwrap();
        let d = docs.get(id)?;
        let ops = self.sequence(d, base, prims)?;
        self.broadcast_ops(d, client, ops);
        Ok(json!({ "version": d.version }))
    }

    /// Sequence an edit against log version `base` and tell the language
    /// server; returns the primitives as applied, for [`Self::broadcast_ops`].
    fn sequence(&self, d: &mut docs::Doc, base: u64, prims: Vec<ot::Prim>) -> Result<Vec<Value>> {
        let server = (!d.buffer.is_large_file())
            .then(|| d.language_id().and_then(|l| self.lsp.running(l)))
            .flatten();
        let applied = d.edit(base, prims, server.is_some())?;
        if let Some(server) = server {
            let changes: Vec<Value> = applied
                .iter()
                .filter_map(|a| a.lsp.as_ref())
                .map(|((sl, sc), (el, ec), text)| {
                    json!({"range": {"start": {"line": sl, "character": sc}, "end": {"line": el, "character": ec}}, "text": text})
                })
                .collect();
            server.notify(
                "textDocument/didChange",
                json!({"textDocument": {"uri": d.uri(), "version": d.version}, "contentChanges": changes}),
            );
        }
        Ok(applied
            .iter()
            .map(|a| {
                let mut v = serde_json::to_value(&a.prim).unwrap_or(Value::Null);
                v["nl"] = json!(a.newlines);
                v
            })
            .collect())
    }

    /// The latest log entry to the document's clients, as made by `client`
    /// (0: the server itself, following the file on disk). Sent under the
    /// documents lock, so every client sees every document's log in order.
    /// The editing client takes the event about its own edit as the
    /// acknowledgement.
    fn broadcast_ops(&self, d: &docs::Doc, client: u64, ops: Vec<Value>) {
        let ev = json!({
            "ev": "doc.ops", "doc": d.id, "version": d.version, "client": client,
            "ops": ops, "len": d.len(), "lines": d.buffer.line_count(), "modified": d.buffer.is_modified(),
        });
        for c in &d.clients {
            self.room.send(*c, &ev);
        }
    }

    /// Follow every open document's file on disk: its (size, mtime) is
    /// polled through the workspace's filesystem, and a change the editor
    /// did not make itself (its own saves record what they leave) is
    /// handled by [`Self::disk_changed`]. Stops when the session goes.
    fn follow_disk(session: &Arc<Session>) {
        let weak = Arc::downgrade(session);
        std::thread::spawn(move || loop {
            std::thread::sleep(Duration::from_millis(500));
            let Some(session) = weak.upgrade() else {
                return;
            };
            session.check_disk();
        });
    }

    /// One look at every open document's file.
    fn check_disk(&self) {
        // Read without the lock: a remote host's answers take time.
        // In id order, so documents are followed in the order they opened.
        let mut seen: Vec<_> = {
            let docs = self.docs.lock().unwrap();
            docs.docs
                .values()
                .filter_map(|d| {
                    Some((
                        d.id,
                        d.origin.path()?.to_path_buf(),
                        d.disk,
                        d.buffer.is_large_file() || !d.buffer.is_modified(),
                    ))
                })
                .collect()
        };
        seen.sort_by_key(|x| x.0);
        let fs = self.ws.fs();
        for (id, path, disk, would_reload) in seen {
            let now = workspace::signature(&fs, &path);
            if now == disk {
                continue;
            }
            // The new text, when it may replace the document's.
            let fresh = (now.is_some() && would_reload).then(|| {
                docs::Buffer::load_from_file(&path, docs::LARGE_FILE_THRESHOLD, fs.clone())
            });
            let mut docs = self.docs.lock().unwrap();
            let Ok(d) = docs.get(id) else { continue };
            // Renamed or saved meanwhile: look again next time.
            if d.origin.path() != Some(path.as_path()) || d.disk != disk {
                continue;
            }
            self.disk_changed(d, now, fresh);
        }
    }

    /// The file of `d` changed on disk to signature `now`:
    /// - gone: the document keeps its text, unsaved (`doc.deleted`);
    /// - a huge document reads its unedited text from the file, so it is
    ///   reloaded whatever it holds (`doc.reloaded`, with `lost` when it had
    ///   unsaved edits);
    /// - with no unsaved edits: the change is applied as an edit from the
    ///   server, a minimal diff through the log, so every replica, cursor
    ///   and undo entry carries through it;
    /// - with unsaved edits: its clients are told once (`doc.changed_on_disk`)
    ///   and choose to reload (`doc.reload`) or keep theirs (a forced save).
    fn disk_changed(
        &self,
        d: &mut docs::Doc,
        now: Option<(u64, u64)>,
        fresh: Option<Result<docs::Buffer>>,
    ) {
        if now.is_none() {
            d.disk = None;
            d.reported = None;
            d.buffer.set_modified(true);
            self.send_to(
                d,
                &json!({"ev": "doc.deleted", "doc": d.id, "info": d.info()}),
            );
            return;
        }
        let modified = d.buffer.is_modified();
        match fresh {
            Some(Ok(b)) if !b.is_binary() && (d.buffer.is_large_file() || !modified) => {
                if let Err(e) = self.reload_with(d, b, now) {
                    eprintln!("{}: not followed: {e}", d.display());
                }
            }
            _ if d.reported != Some(now) => {
                d.reported = Some(now);
                self.send_to(
                    d,
                    &json!({"ev": "doc.changed_on_disk", "doc": d.id, "info": d.info()}),
                );
            }
            _ => {}
        }
    }

    /// Make `d` hold `fresh`, the file as it is now with signature `now`.
    fn reload_with(
        &self,
        d: &mut docs::Doc,
        mut fresh: docs::Buffer,
        now: Option<(u64, u64)>,
    ) -> Result<()> {
        if d.buffer.is_large_file() || fresh.is_large_file() {
            // A huge file: a new buffer and a new log; clients fetch their
            // windows again.
            let lost = d.buffer.is_modified();
            self.lsp_close(d);
            d.buffer = fresh;
            d.version += 1;
            d.log.clear();
            d.disk = now;
            d.reported = None;
            if self.trusted() {
                self.lsp_open(d)?;
            }
            if d.buffer.line_count().is_none() {
                index_lines(self.docs.clone(), self.room.clone(), d.id);
            }
            self.send_to(
                d,
                &json!({"ev": "doc.reloaded", "doc": d.id, "lost": lost, "info": d.info()}),
            );
            return Ok(());
        }
        let n = fresh.len();
        let new = String::from_utf8_lossy(&fresh.get_text_range_mut(0, n)?).into_owned();
        let old = d.full_text()?;
        let prims = docs::diff_prims(&old, &new);
        let ops = if prims.is_empty() {
            Vec::new()
        } else {
            self.sequence(d, d.version, prims)?
        };
        // The file's format as read now (another encoding asked for, or the
        // file's own changed).
        d.buffer.set_default_encoding(fresh.encoding());
        d.buffer.set_default_line_ending(fresh.line_ending());
        d.buffer.set_modified(false);
        d.disk = now;
        d.reported = None;
        if !ops.is_empty() {
            self.broadcast_ops(d, 0, ops);
        }
        let ev = json!({"ev": "doc.saved", "doc": d.id, "info": d.info()});
        self.send_to(d, &ev);
        Ok(())
    }

    /// Change what a document is, not its text: its `language` (the
    /// language server of the old one gets `didClose`, the new one's
    /// `didOpen`), and the `line_ending` and `encoding` the next save
    /// writes (the document is then modified). Every client holding it gets
    /// `doc.info`.
    fn set_format(&self, m: &Value) -> Result<Value> {
        let mut docs = self.docs.lock().unwrap();
        let d = docs.get(u(m, "doc")?)?;
        if let Some(name) = m.get("line_ending").and_then(Value::as_str) {
            let Some(le) = docs::line_ending_named(name) else {
                fail!(BadRequest, "no line ending {name}");
            };
            if le != d.buffer.line_ending() {
                // The text follows at once, as an edit from the server, so
                // every client's copy is what the next save writes (a CR
                // file's line breaks are `\n` in the text). A huge file's
                // text is converted as it is saved.
                if !d.buffer.is_large_file() {
                    let old = d.full_text()?;
                    let lf = old.replace("\r\n", "\n");
                    let new = if le == docs::LineEnding::CRLF {
                        lf.replace('\n', "\r\n")
                    } else {
                        lf
                    };
                    let prims = docs::diff_prims(&old, &new);
                    if !prims.is_empty() {
                        let ops = self.sequence(d, d.version, prims)?;
                        self.broadcast_ops(d, 0, ops);
                    }
                }
                d.buffer.set_line_ending(le);
            }
        }
        if let Some(name) = m.get("encoding").and_then(Value::as_str) {
            let Some(enc) = docs::encoding_named(name) else {
                fail!(BadRequest, "no encoding {name}");
            };
            if enc != d.buffer.encoding() {
                d.buffer.set_encoding(enc);
            }
        }
        if let Some(lang) = m.get("language") {
            let lang = lang
                .as_str()
                .map(|id| {
                    languages::by_id(id, d.origin.path())
                        .ok_or_else(|| err(Code::BadRequest, format!("no language {id}")))
                })
                .transpose()?;
            if lang != d.language {
                self.lsp_close(d);
                d.language = lang;
                if self.trusted() {
                    self.lsp_open(d)?;
                }
            }
        }
        let info = d.info();
        self.send_to(d, &json!({"ev": "doc.info", "doc": d.id, "info": info}));
        Ok(info)
    }

    /// Save a document as a file and bind it there (`doc.save_as {doc,
    /// path, overwrite}`): an untitled document gets its file, a file's
    /// document moves to the new one (the old file stays as last saved).
    /// The language server sees the old URI closed and the new one opened;
    /// the document's clients get `doc.renamed` (`from`: the old path, or
    /// null) and `doc.saved`.
    fn save_as(&self, m: &Value) -> Result<Value> {
        let path = self.ws.resolve(s(m, "path")?);
        let overwrite = m.get("overwrite").and_then(Value::as_bool).unwrap_or(false);
        let id = u(m, "doc")?;
        let mut docs = self.docs.lock().unwrap();
        if docs.get(id)?.origin.path() == Some(path.as_path()) {
            fail!(
                BadRequest,
                "{} is the document's own file: save it",
                path.display()
            );
        }
        self.lsp_close(docs.get(id)?);
        let saved = docs.save_as(id, &path, &self.ws.recovery_dir(), overwrite);
        let d = docs.get(id)?;
        if self.trusted() {
            self.lsp_open(d)?;
        }
        let from = saved?;
        let info = d.info();
        self.send_to(d, &json!({"ev": "doc.renamed", "doc": id, "from": from.map(|p| p.to_string_lossy().into_owned()), "info": info}));
        self.send_to(d, &json!({"ev": "doc.saved", "doc": id, "info": info}));
        Ok(info)
    }

    fn send_to(&self, d: &docs::Doc, ev: &Value) {
        for c in &d.clients {
            self.room.send(*c, ev);
        }
    }

    /// Rename or move a file or folder. Documents open on it (or in it)
    /// follow: their path and language change, their language servers are
    /// told (`didClose` of the old name, `didOpen` of the new), and their
    /// clients get `doc.renamed`. Held under the documents lock, so no
    /// document opens or saves meanwhile.
    fn rename(&self, from: &Path, to: &Path) -> Result<()> {
        let mut docs = self.docs.lock().unwrap();
        Self::refuse_lazy(&mut docs, from, "moved")?;
        if !docs.under(to).is_empty() {
            fail!(Exists, "{} is open as an unsaved document", to.display());
        }
        self.ws.rename(from, to)?;
        let trusted = self.trusted();
        for moved in docs.renamed(from, to) {
            let d = docs.get(moved.id)?;
            if let Some(server) = moved.language.as_deref().and_then(|l| self.lsp.running(l)) {
                server.notify(
                    "textDocument/didClose",
                    json!({"textDocument": {"uri": lsp::uri_of(&moved.path)}}),
                );
            }
            if trusted {
                self.lsp_open(d)?;
            }
            let ev = json!({"ev": "doc.renamed", "doc": d.id, "from": moved.path.to_string_lossy(), "info": d.info()});
            for c in &d.clients {
                self.room.send(*c, &ev);
            }
        }
        Ok(())
    }

    /// Delete a file or folder. A document open on a deleted file keeps
    /// its text, unsaved (saving writes the file again); its clients get
    /// `doc.deleted`. Documents are updated by what is gone, even when the
    /// deletion stopped part-way.
    fn delete(&self, path: &Path) -> Result<()> {
        let mut docs = self.docs.lock().unwrap();
        Self::refuse_lazy(&mut docs, path, "deleted")?;
        let removed = self.ws.remove(path);
        for id in docs.deleted(path) {
            let d = docs.get(id)?;
            let ev = json!({"ev": "doc.deleted", "doc": id, "info": d.info()});
            for c in &d.clients {
                self.room.send(*c, &ev);
            }
        }
        removed
    }

    /// A huge file's document reads its text from the file as it is
    /// viewed, so the file cannot go while the document is open.
    fn refuse_lazy(docs: &mut Docs, path: &Path, what: &str) -> Result<()> {
        for id in docs.under(path) {
            let d = docs.get(id)?;
            if d.buffer.is_large_file() {
                fail!(
                    InUse,
                    "{} is open as a large file, read from disk as it is viewed; close it before it is {what}",
                    d.display()
                );
            }
        }
        Ok(())
    }

    /// Hand a newly opened document of an ordinary size to its language's
    /// server, starting the server if it is installed.
    fn lsp_open(&self, d: &mut docs::Doc) -> Result<()> {
        if d.buffer.is_large_file() {
            return Ok(());
        }
        let Some(lang) = d.language_id().map(String::from) else {
            return Ok(());
        };
        if let Some(server) = self.lsp.server(&lang) {
            let text = d.full_text()?;
            server.notify(
                "textDocument/didOpen",
                json!({"textDocument": {
                    "uri": d.uri(), "languageId": lang,
                    "version": d.version, "text": text,
                }}),
            );
        }
        Ok(())
    }

    fn lsp_close(&self, d: &docs::Doc) {
        if let Some(server) = d.language_id().and_then(|l| self.lsp.running(l)) {
            server.notify(
                "textDocument/didClose",
                json!({"textDocument": {"uri": d.uri()}}),
            );
        }
    }

    /// Start `language`'s server again (`lsp.restart`): the running one is
    /// stopped, a new one starts and is handed every open document of the
    /// language. Every client gets `lsp.status`.
    fn lsp_restart(&self, language: &str) -> Result<Value> {
        if !self.lsp.available(language) {
            fail!(
                NoLanguageServer,
                "no {language} language server on the workspace's host"
            );
        }
        let started = self.lsp.restart(language).is_some();
        {
            let mut docs = self.docs.lock().unwrap();
            let ids: Vec<u64> = docs
                .docs
                .values()
                .filter(|d| d.language_id() == Some(language))
                .map(|d| d.id)
                .collect();
            for id in ids {
                self.lsp_open(docs.get(id)?)?;
            }
        }
        if !started {
            fail!(
                LanguageServer,
                "the {language} language server did not start"
            );
        }
        self.lsp_changed();
        Ok(self.lsp.status())
    }

    /// Stop `language`'s server (`lsp.stop`) until it is restarted: its
    /// documents' diagnostics are cleared, and every client gets
    /// `lsp.status`.
    fn lsp_stop(&self, language: &str) -> Value {
        self.lsp.stop(language);
        {
            let docs = self.docs.lock().unwrap();
            let mut diags = self.diags.lock().unwrap();
            for d in docs.docs.values() {
                if d.language_id() == Some(language) {
                    diags.remove(&d.id);
                    self.send_to(
                        d,
                        &json!({"ev": "lsp.diagnostics", "doc": d.id, "version": d.version, "items": []}),
                    );
                }
            }
        }
        self.lsp_changed();
        self.lsp.status()
    }

    /// Which servers run changed: every client is told, and the
    /// workspace's status follows (its diagnostics).
    fn lsp_changed(&self) {
        self.room
            .broadcast(&json!({"ev": "lsp.status", "status": self.lsp.status()}));
        self.publish_status();
    }

    /// Any request about a document, by method: the server fills in the
    /// document (and the position, given `pos` as a byte offset) and returns
    /// the language server's result as it came. Positions in it are the
    /// server's (line, UTF-16 column); the client converts them with its copy
    /// of the text. A request about the workspace (`workspace/symbol`)
    /// names a `language` instead of a document, and goes to that
    /// language's running server.
    fn lsp_call(&self, m: &Value) -> Result<Value> {
        let method = s(m, "method")?.to_string();
        let mut params = m.get("params").cloned().unwrap_or(json!({}));
        if m.get("doc").is_none() {
            let language = s(m, "language")?;
            let server = self.lsp.running(language).ok_or_else(|| {
                err(
                    Code::NoLanguageServer,
                    format!("no running {language} language server"),
                )
            })?;
            let r = server.request(&method, params, Duration::from_secs(20))?;
            return Ok(lsp_shapes::plain(&method, r));
        }
        let server = {
            let mut docs = self.docs.lock().unwrap();
            let d = docs.get(u(m, "doc")?)?;
            let server = d
                .language_id()
                .and_then(|l| self.lsp.running(l))
                .ok_or_else(|| {
                    err(
                        Code::NoLanguageServer,
                        "no language server for this document",
                    )
                })?;
            params["textDocument"] = json!({"uri": d.uri()});
            if let Some(p) = m.get("pos").and_then(Value::as_u64) {
                let (l, c) = d.buffer.position_to_lsp_position(p as usize);
                params["position"] = json!({"line": l, "character": c});
            }
            server
        };
        let r = server.request(&method, params, Duration::from_secs(20))?;
        Ok(lsp_shapes::plain(&method, r))
    }

    fn lsp_request(&self, op: &str, m: &Value) -> Result<Value> {
        let (server, uri, line, character) = {
            let mut docs = self.docs.lock().unwrap();
            let d = docs.get(u(m, "doc")?)?;
            let server = d
                .language_id()
                .and_then(|l| self.lsp.running(l))
                .ok_or_else(|| {
                    err(
                        Code::NoLanguageServer,
                        "no language server for this document",
                    )
                })?;
            let (l, c) = d.buffer.position_to_lsp_position(u(m, "pos")? as usize);
            (server, d.uri(), l, c)
        };
        let params = json!({"textDocument": {"uri": uri}, "position": {"line": line, "character": character}});
        let method = match op {
            "lsp.hover" => "textDocument/hover",
            "lsp.definition" => "textDocument/definition",
            _ => "textDocument/completion",
        };
        let r = server.request(method, params, Duration::from_secs(15))?;
        Ok(match op {
            "lsp.hover" => {
                let c = &r["contents"];
                let text = if let Some(s) = c.as_str() {
                    s.to_string()
                } else if let Some(v) = c.get("value").and_then(Value::as_str) {
                    v.to_string()
                } else if let Some(a) = c.as_array() {
                    a.iter()
                        .map(|x| x.as_str().or(x["value"].as_str()).unwrap_or("").to_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                } else {
                    String::new()
                };
                json!({ "text": text })
            }
            "lsp.definition" => {
                let list = match &r {
                    Value::Array(a) => a.clone(),
                    Value::Null => vec![],
                    other => vec![other.clone()],
                };
                let locs: Vec<Value> = list
                    .iter()
                    .filter_map(|l| {
                        let uri = l["uri"].as_str().or(l["targetUri"].as_str())?;
                        let range = if l["range"].is_object() { &l["range"] } else { &l["targetSelectionRange"] };
                        Some(json!({
                            "path": lsp::path_of(uri)?.to_string_lossy(),
                            "line": range["start"]["line"], "character": range["start"]["character"],
                        }))
                    })
                    .collect();
                json!({ "locations": locs })
            }
            _ => {
                let items = r.get("items").cloned().unwrap_or(r.clone());
                let items: Vec<Value> = items
                    .as_array()
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .take(100)
                    .map(|i| {
                        let insert = i["textEdit"]["newText"]
                            .as_str()
                            .or(i["insertText"].as_str())
                            .or(i["label"].as_str())
                            .unwrap_or("")
                            .to_string();
                        json!({"label": i["label"], "detail": i["detail"], "insert": insert})
                    })
                    .collect();
                json!({ "items": items })
            }
        })
    }

    /// The workspace is closed, or the server is exiting: stop every
    /// process, terminal and language server it started on the host.
    pub fn shutdown(&self) {
        self.procs.stop_all();
        self.terms.close_all();
        self.lsp.stop_all();
    }

    /// A client's event stream ended: what it opened is released.
    pub fn drop_client(&self, client: u64) {
        self.watches.drop_client(client);
        self.procs.drop_client(client);
        let closed = self.docs.lock().unwrap().drop_client(client);
        for d in closed {
            self.lsp_close(&d);
        }
    }
}

/// The git branch the workspace's root is on, read from `.git/HEAD`
/// through the workspace's filesystem (in a worktree, `.git` is a file
/// naming the git folder); a detached head is its short hash. `None` outside
/// a repository.
fn git_branch(ws: &Workspace) -> Option<String> {
    let fs = ws.fs();
    let read = |p: &Path| {
        fs.read_file(workspace::host(p).as_host_path())
            .ok()
            .map(|b| String::from_utf8_lossy(&b).trim().to_string())
    };
    let dot = ws.root.join(".git");
    let head = read(&dot.join("HEAD")).or_else(|| {
        let link = read(&dot)?;
        let gitdir = ws.root.join(link.strip_prefix("gitdir:")?.trim());
        read(&gitdir.join("HEAD"))
    })?;
    Some(match head.strip_prefix("ref: refs/heads/") {
        Some(branch) => branch.to_string(),
        None => head.chars().take(7).collect(),
    })
}

/// Diagnostics as data, in byte offsets, tagged with the version they
/// describe; a report for an older version than the document's is dropped
/// (the server will send a fresh one).
/// Their counts are kept for the workspace's status.
fn publish_diagnostics(room: &Room, docs: &Mutex<Docs>, diags: &DiagCounts, params: &Value) {
    let Some(uri) = params["uri"].as_str() else {
        return;
    };
    let mut docs = docs.lock().unwrap();
    let Some(id) = docs.by_uri(uri) else {
        return;
    };
    let Ok(d) = docs.get(id) else { return };
    if let Some(v) = params["version"].as_u64() {
        if v != d.version {
            return;
        }
    }
    let items: Vec<Value> = params["diagnostics"]
        .as_array()
        .cloned()
        .unwrap_or_default()
        .iter()
        .map(|g| {
            let p = |k: &str| {
                let l = g["range"][k]["line"].as_u64().unwrap_or(0) as usize;
                let c = g["range"][k]["character"].as_u64().unwrap_or(0) as usize;
                d.buffer.lsp_position_to_byte(l, c)
            };
            json!({
                "start": p("start"), "end": p("end"),
                "severity": g["severity"].as_u64().unwrap_or(1),
                "message": g["message"], "source": g["source"],
            })
        })
        .collect();
    let count = |sev: u64| items.iter().filter(|i| i["severity"] == sev).count();
    diags.lock().unwrap().insert(id, (count(1), count(2)));
    let ev = json!({"ev": "lsp.diagnostics", "doc": id, "version": d.version, "items": items});
    for c in &d.clients {
        room.send(*c, &ev);
    }
}

/// Build a document's line index in the background: every document gets
/// one; a huge file's is counted here, a chunk at a time, through its
/// filesystem (a remote one counts on the remote side, so the bytes never
/// cross the link). Counts are kept by file range, which edits made meanwhile
/// cannot change; regions edited meanwhile are in memory and counted there.
fn index_lines(docs: Arc<Mutex<Docs>>, room: Room, id: u64) {
    std::thread::spawn(move || build_line_index(&docs, &room, id));
}

pub(crate) fn build_line_index(docs: &Mutex<Docs>, room: &Room, id: u64) {
    if let Err(e) = count_lines(docs, room, id) {
        // Its lines stay without numbers: its viewers are told why.
        let clients: Vec<u64> = match docs.lock().unwrap().get(id) {
            Ok(d) => d.clients.iter().copied().collect(),
            Err(_) => Vec::new(), // closed meanwhile: no one to tell
        };
        let ev = json!({"ev": "doc.index_failed", "doc": id, "why": e.to_string()});
        for c in clients {
            room.send(c, &ev);
        }
    }
}

/// Count document `id`'s lines and tell its viewers (nothing to do when it
/// was closed meanwhile).
fn count_lines(docs: &Mutex<Docs>, room: &Room, id: u64) -> Result<()> {
    let (fs, work, total) = {
        let mut g = docs.lock().unwrap();
        let Ok(d) = g.get(id) else { return Ok(()) };
        let (chunks, total) = d.buffer.prepare_line_scan();
        let leaves = d.buffer.piece_tree_leaves();
        let work: Vec<(PathBuf, u64, usize)> = chunks
            .iter()
            .filter(|c| !c.already_known)
            .filter_map(|c| d.buffer.leaf_io_params(&leaves[c.leaf_index]))
            .collect();
        (d.buffer.filesystem().clone(), work, total)
    };
    let mut counts = std::collections::HashMap::new();
    let mut done = 0usize;
    let mut last = std::time::Instant::now();
    for (path, off, len) in work {
        let n = fs
            .count_line_feeds_in_range(workspace::host(&path).as_host_path(), off, len)
            .map_err(|e| anyhow::anyhow!("{}: {e}", path.display()))?;
        counts.insert((path, off, len), n);
        done += len;
        if last.elapsed() > Duration::from_millis(250) {
            last = std::time::Instant::now();
            room.broadcast(&json!({"ev": "doc.indexing", "doc": id, "done": done, "total": total}));
        }
    }
    let mut g = docs.lock().unwrap();
    let Ok(d) = g.get(id) else { return Ok(()) };
    let (chunks, _) = d.buffer.prepare_line_scan();
    let leaves = d.buffer.piece_tree_leaves();
    let mut updates = Vec::new();
    for c in chunks.iter().filter(|c| !c.already_known) {
        let leaf = &leaves[c.leaf_index];
        let n = match d.buffer.leaf_io_params(leaf) {
            Some(k) => match counts.get(&k) {
                Some(n) => *n,
                None => fs
                    .count_line_feeds_in_range(workspace::host(&k.0).as_host_path(), k.1, k.2)
                    .map_err(|e| anyhow::anyhow!("{}: {e}", k.0.display()))?,
            },
            None => d.buffer.scan_leaf(leaf)?,
        };
        updates.push((c.leaf_index, n));
    }
    d.buffer.apply_scan_updates(&updates);
    let ev = json!({"ev": "doc.indexed", "doc": id, "lines": d.buffer.line_count(), "version": d.version});
    for c in &d.clients {
        room.send(*c, &ev);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::authority::HostSpec;
    use crate::error::code_of;
    use crate::orchestrator::{Open, Orchestrator};
    use docs::Docs;
    use fresh_editor_core::model::filesystem::StdFileSystem;
    use std::time::Instant;

    /// A server with one local workspace in a new folder.
    fn app(trusted: bool) -> (tempfile::TempDir, Orchestrator, Arc<Session>) {
        let dir = tempfile::tempdir().unwrap();
        let orch = Orchestrator::new(false, None);
        let (s, _) = orch
            .open(&Open::new(
                HostSpec::Local,
                dir.path().to_str().unwrap(),
                trusted,
            ))
            .unwrap();
        (dir, orch, s)
    }

    /// A client of the first workspace with its event stream open, as after
    /// `GET /events`.
    pub fn client(orch: &Orchestrator) -> (u64, tokio::sync::mpsc::UnboundedReceiver<String>) {
        let id = orch.dispatch(0, "hello", &json!({})).unwrap()["client"]
            .as_u64()
            .unwrap();
        (id, orch.hub.take_stream(id).unwrap())
    }

    pub fn event(
        rx: &mut tokio::sync::mpsc::UnboundedReceiver<String>,
        ev: &str,
        within: Duration,
    ) -> Option<Value> {
        let end = Instant::now() + within;
        while Instant::now() < end {
            match rx.try_recv() {
                Ok(m) => {
                    let v: Value = serde_json::from_str(&m).unwrap();
                    if v["ev"] == ev {
                        return Some(v);
                    }
                }
                Err(_) => std::thread::sleep(Duration::from_millis(10)),
            }
        }
        None
    }

    #[test]
    fn a_restricted_workspace_runs_nothing_until_trusted() {
        let (dir, orch, s) = app(false);
        std::fs::write(dir.path().join("a.py"), "x = 1\n").unwrap();
        let (c, mut rx) = client(&orch);
        let hello = orch.dispatch(0, "hello", &json!({})).unwrap();
        assert_eq!(hello["workspace"]["trusted"], false);
        assert_eq!(
            hello["workspace"]["lsp"],
            json!([]),
            "no probe while restricted"
        );
        assert!(!s.lsp.probed(), "nothing was run");

        let run = json!({"program": "sh", "args": ["-c", "echo ran"]});
        for (op, m) in [
            ("proc.run", run.clone()),
            ("proc.kill", json!({"proc": 1})),
            ("term.open", json!({})),
            ("lsp.status", json!({})),
            (
                "lsp.call",
                json!({"doc": 1, "method": "textDocument/hover"}),
            ),
            ("lsp.hover", json!({"doc": 1, "pos": 0})),
        ] {
            let e = orch.dispatch(c, op, &m).unwrap_err();
            assert_eq!(code_of(&e), Code::Untrusted, "{op}: {e}");
        }
        // Files and documents work as ever, without a language server.
        orch.dispatch(c, "doc.open", &json!({"path": "a.py"}))
            .unwrap();
        assert!(!s.lsp.probed());
        assert!(s.terms.list().is_empty());

        orch.dispatch(c, "workspace.trust", &json!({})).unwrap();
        let ev = event(&mut rx, "workspace.trusted", Duration::from_secs(5)).unwrap();
        assert!(ev["lsp"].is_array());
        let p = orch.dispatch(c, "proc.run", &run).unwrap()["proc"].clone();
        let exit = event(&mut rx, "proc.exit", Duration::from_secs(10)).unwrap();
        assert_eq!(
            (exit["proc"].clone(), exit["stdout"].clone()),
            (p, json!("ran\n"))
        );
    }

    #[test]
    fn a_documents_line_ending_encoding_and_language_can_be_set() {
        let (dir, orch, _s) = app(false);
        std::fs::write(dir.path().join("a.txt"), "a\r\nb\r\n").unwrap();
        let (c, mut rx) = client(&orch);
        let info = orch
            .dispatch(c, "doc.open", &json!({"path": "a.txt"}))
            .unwrap();
        assert_eq!(info["line_ending"], "CRLF");
        let doc = info["doc"].as_u64().unwrap();
        let set = |m: Value| {
            let mut m = m;
            m["doc"] = json!(doc);
            orch.dispatch(c, "doc.set", &m)
        };
        let info = set(json!({"line_ending": "LF", "language": "python"})).unwrap();
        assert_eq!(info["line_ending"], "LF");
        assert_eq!(info["language"], "python");
        assert_eq!(info["modified"], true, "the next save converts the file");
        let ev = event(&mut rx, "doc.info", Duration::from_secs(1)).expect("doc.info");
        assert_eq!(ev["info"]["line_ending"], "LF");
        orch.dispatch(c, "doc.save", &json!({"doc": doc})).unwrap();
        assert_eq!(std::fs::read(dir.path().join("a.txt")).unwrap(), b"a\nb\n");

        // Saved in another encoding; reopened in one.
        set(json!({"encoding": "UTF-16 LE"})).unwrap();
        orch.dispatch(c, "doc.save", &json!({"doc": doc})).unwrap();
        assert_eq!(
            std::fs::read(dir.path().join("a.txt")).unwrap(),
            b"\xff\xfea\0\n\0b\0\n\0"
        );
        std::fs::write(dir.path().join("a.txt"), b"caf\xe9\n").unwrap();
        let info = orch
            .dispatch(c, "doc.reload", &json!({"doc": doc, "encoding": "Latin-1"}))
            .unwrap();
        assert_eq!(info["encoding"], "Latin-1");
        let text = orch
            .dispatch(c, "doc.text", &json!({"doc": doc, "start": 0, "end": 6}))
            .unwrap();
        assert_eq!(text["text"], "café\n");

        for bad in [
            json!({"line_ending": "NL"}),
            json!({"encoding": "EBCDIC"}),
            json!({"language": "cobol"}),
        ] {
            let e = set(bad).unwrap_err();
            assert_eq!(code_of(&e), Code::BadRequest, "{e}");
        }
    }

    #[test]
    fn a_disconnected_client_releases_what_it_opened() {
        let (dir, orch, s) = app(true);
        std::fs::write(dir.path().join("a.txt"), "a\n").unwrap();
        std::fs::write(dir.path().join("b.txt"), "b\n").unwrap();
        let (c, _rx) = client(&orch);
        let (other, _orx) = client(&orch);
        let open = |client, p: &str| {
            orch.dispatch(client, "doc.open", &json!({ "path": p }))
                .unwrap()["doc"]
                .as_u64()
                .unwrap()
        };
        let a = open(c, "a.txt");
        let b = open(c, "b.txt");
        let shared = open(other, "a.txt");
        assert_eq!(shared, a);
        orch.dispatch(
            c,
            "doc.edit",
            &json!({"doc": b, "base": 0, "ops": [{"t": "ins", "pos": 0, "text": "x"}]}),
        )
        .unwrap();
        orch.dispatch(c, "fs.watch", &json!({"path": "a.txt"}))
            .unwrap();
        orch.dispatch(c, "proc.run", &json!({"program": "sleep", "args": ["30"]}))
            .unwrap();
        assert_eq!((s.watches.count(), s.procs.count()), (1, 1));

        orch.drop_client(c);
        assert_eq!(s.watches.count(), 0);
        let end = Instant::now() + Duration::from_secs(5);
        while s.procs.count() > 0 && Instant::now() < end {
            std::thread::sleep(Duration::from_millis(10));
        }
        assert_eq!(s.procs.count(), 0, "its process was killed");
        {
            let mut docs = s.docs.lock().unwrap();
            // Still open by the other client; unsaved edits are kept.
            assert_eq!(docs.get(a).unwrap().clients, [other].into());
            assert!(docs.get(b).unwrap().clients.is_empty());
        }
        let e = orch
            .dispatch(c, "doc.open", &json!({"path": "a.txt"}))
            .unwrap_err();
        assert_eq!(code_of(&e), Code::UnknownClient, "{e}");

        orch.drop_client(other);
        let mut docs = s.docs.lock().unwrap();
        assert!(docs.get(a).is_err(), "closed by every client: dropped");
        assert!(docs.get(b).is_ok());
    }

    #[test]
    fn a_language_server_is_stopped_until_restarted() {
        let (_dir, orch, s) = app(true);
        let (c, mut rx) = client(&orch);
        let status = orch
            .dispatch(c, "lsp.stop", &json!({"language": "python"}))
            .unwrap();
        assert_eq!(status["python"], false);
        let ev = event(&mut rx, "lsp.status", Duration::from_secs(1)).unwrap();
        assert_eq!(ev["status"]["python"], false);
        // Stopped: no file of the language starts it, and requests about
        // the workspace find none.
        assert!(s.lsp.server("python").is_none());
        let e = orch
            .dispatch(
                c,
                "lsp.call",
                &json!({"language": "python", "method": "workspace/symbol", "params": {"query": "x"}}),
            )
            .unwrap_err();
        assert_eq!(code_of(&e), Code::NoLanguageServer);
        // A language with no server on the host cannot be restarted.
        let e = orch
            .dispatch(c, "lsp.restart", &json!({"language": "cobol"}))
            .unwrap_err();
        assert_eq!(code_of(&e), Code::NoLanguageServer);
        let e = orch
            .dispatch(c, "lsp.call", &json!({"method": "workspace/symbol"}))
            .unwrap_err();
        assert_eq!(code_of(&e), Code::BadRequest, "a document or a language");
    }

    #[test]
    fn an_untitled_document_is_kept_while_unsaved_and_saved_as_a_file() {
        let (dir, orch, _s) = app(true);
        let root = dir.path().canonicalize().unwrap();
        let (c, rx) = client(&orch);
        let new = |c: u64| orch.dispatch(c, "doc.new", &json!({})).unwrap();
        let a = new(c);
        assert_eq!(
            (a["path"].clone(), a["untitled"].clone()),
            (Value::Null, json!(1))
        );
        let b = new(c);
        assert_eq!(b["untitled"], 2, "the smallest number not taken");
        let (a, b) = (a["doc"].as_u64().unwrap(), b["doc"].as_u64().unwrap());
        let e = orch
            .dispatch(c, "doc.save", &json!({"doc": a}))
            .unwrap_err();
        assert_eq!(code_of(&e), Code::BadRequest, "no file to save to");

        // Closed empty, it goes; its number is free again.
        orch.dispatch(c, "doc.close", &json!({"doc": b})).unwrap();
        assert_eq!(new(c)["untitled"], 2);

        // With text, it outlives its client, and the next one joins it.
        orch.dispatch(
            c,
            "doc.edit",
            &json!({"doc": a, "base": 0, "ops": [{"t": "ins", "pos": 0, "text": "hello\n"}]}),
        )
        .unwrap();
        orch.drop_client(c);
        drop(rx);
        let (c, mut rx) = client(&orch);
        let info = orch
            .dispatch(c, "doc.open", &json!({"untitled": 1}))
            .unwrap();
        assert_eq!(
            (info["doc"].as_u64(), info["modified"].clone()),
            (Some(a), json!(true))
        );
        let e = orch
            .dispatch(c, "doc.open", &json!({"untitled": 9}))
            .unwrap_err();
        assert_eq!(code_of(&e), Code::NotFound);

        // Saved as a file: bound to it, its language detected; a file that
        // is there is overwritten only when asked.
        std::fs::write(root.join("taken.py"), "x\n").unwrap();
        let e = orch
            .dispatch(c, "doc.save_as", &json!({"doc": a, "path": "taken.py"}))
            .unwrap_err();
        assert_eq!(code_of(&e), Code::Exists);
        let info = orch
            .dispatch(c, "doc.save_as", &json!({"doc": a, "path": "sub/n.py"}))
            .unwrap();
        assert_eq!(info["path"], json!(root.join("sub/n.py")));
        assert_eq!(
            (
                info["untitled"].clone(),
                info["language"].clone(),
                info["modified"].clone()
            ),
            (Value::Null, json!("python"), json!(false))
        );
        assert_eq!(
            std::fs::read_to_string(root.join("sub/n.py")).unwrap(),
            "hello\n"
        );
        let ev = event(&mut rx, "doc.renamed", Duration::from_secs(1)).unwrap();
        assert_eq!(ev["from"], Value::Null);
        let again = orch
            .dispatch(c, "doc.open", &json!({"path": "sub/n.py"}))
            .unwrap();
        assert_eq!(again["doc"], a, "found by its file now");

        // A file's document saved as another: it moves there, the old file
        // stays as it was last saved.
        orch.dispatch(
            c,
            "doc.edit",
            &json!({"doc": a, "base": 1, "ops": [{"t": "ins", "pos": 0, "text": "# "}]}),
        )
        .unwrap();
        orch.dispatch(
            c,
            "doc.save_as",
            &json!({"doc": a, "path": "taken.py", "overwrite": true}),
        )
        .unwrap();
        let ev = event(&mut rx, "doc.renamed", Duration::from_secs(1)).unwrap();
        assert_eq!(ev["from"], json!(root.join("sub/n.py")));
        assert_eq!(
            std::fs::read_to_string(root.join("taken.py")).unwrap(),
            "# hello\n"
        );
        assert_eq!(
            std::fs::read_to_string(root.join("sub/n.py")).unwrap(),
            "hello\n"
        );
        let other = orch
            .dispatch(c, "doc.open", &json!({"path": "sub/n.py"}))
            .unwrap()["doc"]
            .as_u64()
            .unwrap();
        let e = orch
            .dispatch(
                c,
                "doc.save_as",
                &json!({"doc": other, "path": "taken.py", "overwrite": true}),
            )
            .unwrap_err();
        assert_eq!(code_of(&e), Code::Exists, "open as another document");
    }

    #[test]
    fn open_documents_follow_a_rename_and_survive_a_delete() {
        let (dir, orch, _s) = app(true);
        let root = dir.path().canonicalize().unwrap();
        std::fs::create_dir(root.join("src")).unwrap();
        std::fs::write(root.join("src/a.txt"), "a\n").unwrap();
        let (c, mut rx) = client(&orch);
        let a = orch
            .dispatch(c, "doc.open", &json!({"path": "src/a.txt"}))
            .unwrap()["doc"]
            .as_u64()
            .unwrap();
        let edit = |doc: u64, base: u64| {
            orch.dispatch(
                c,
                "doc.edit",
                &json!({"doc": doc, "base": base, "ops": [{"t": "ins", "pos": 0, "text": "x"}]}),
            )
            .unwrap();
        };
        edit(a, 0);

        // A file: the document follows, with the language of its new name.
        orch.dispatch(
            c,
            "fs.rename",
            &json!({"from": "src/a.txt", "to": "src/b.py"}),
        )
        .unwrap();
        let ev = event(&mut rx, "doc.renamed", Duration::from_secs(1)).unwrap();
        assert_eq!(ev["doc"], a);
        assert_eq!(ev["from"], json!(root.join("src/a.txt")));
        assert_eq!(ev["info"]["path"], json!(root.join("src/b.py")));
        assert_eq!(ev["info"]["language"], "python");
        orch.dispatch(c, "doc.save", &json!({"doc": a})).unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("src/b.py")).unwrap(),
            "xa\n"
        );
        assert!(!root.join("src/a.txt").exists(), "the old name stays gone");

        // A folder holding it: the same.
        orch.dispatch(c, "fs.rename", &json!({"from": "src", "to": "lib"}))
            .unwrap();
        let ev = event(&mut rx, "doc.renamed", Duration::from_secs(1)).unwrap();
        assert_eq!(ev["info"]["path"], json!(root.join("lib/b.py")));
        let again = orch
            .dispatch(c, "doc.open", &json!({"path": "lib/b.py"}))
            .unwrap();
        assert_eq!(again["doc"], a, "found under its new path");

        // Never onto a name an unsaved document holds.
        let n = orch
            .dispatch(c, "doc.open", &json!({"path": "new.txt"}))
            .unwrap()["doc"]
            .as_u64()
            .unwrap();
        edit(n, 0);
        let e = orch
            .dispatch(
                c,
                "fs.rename",
                &json!({"from": "lib/b.py", "to": "new.txt"}),
            )
            .unwrap_err();
        assert_eq!(code_of(&e), Code::Exists);

        // Deleted: the document keeps its text, unsaved; saving writes the
        // file again.
        orch.dispatch(c, "fs.delete", &json!({"path": "lib"}))
            .unwrap();
        let ev = event(&mut rx, "doc.deleted", Duration::from_secs(1)).unwrap();
        assert_eq!(
            (ev["doc"].clone(), ev["info"]["modified"].clone()),
            (json!(a), json!(true))
        );
        assert!(!root.join("lib").exists());
        orch.dispatch(c, "doc.save", &json!({"doc": a})).unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("lib/b.py")).unwrap(),
            "xa\n"
        );
    }

    #[test]
    fn open_documents_follow_their_files_on_disk() {
        let (dir, orch, s) = app(true);
        let p = dir.path().join("a.txt");
        std::fs::write(&p, "one\ntwo\nthree\n").unwrap();
        let (c, mut rx) = client(&orch);
        let a = orch
            .dispatch(c, "doc.open", &json!({"path": "a.txt"}))
            .unwrap()["doc"]
            .as_u64()
            .unwrap();
        let text = |s: &Session| s.docs.lock().unwrap().get(a).unwrap().full_text().unwrap();

        // The editor's own save is not a change on disk.
        orch.dispatch(
            c,
            "doc.edit",
            &json!({"doc": a, "base": 0, "ops": [{"t": "ins", "pos": 0, "text": "zero\n"}]}),
        )
        .unwrap();
        orch.dispatch(c, "doc.save", &json!({"doc": a})).unwrap();
        while rx.try_recv().is_ok() {}
        s.check_disk();
        assert!(rx.try_recv().is_err(), "nothing to follow after a save");

        // Unmodified: the change arrives as edits from the server.
        std::fs::write(&p, "zero\none\n2\nthree\nfour\n").unwrap();
        s.check_disk();
        let ev = event(&mut rx, "doc.ops", Duration::from_secs(1)).unwrap();
        assert_eq!(
            (ev["client"].clone(), ev["modified"].clone()),
            (json!(0), json!(false))
        );
        assert_eq!(text(&s), "zero\none\n2\nthree\nfour\n");
        s.check_disk();
        assert!(event(&mut rx, "doc.ops", Duration::from_millis(50)).is_none());

        // Unsaved edits: reported once; a reload takes the disk's text.
        orch.dispatch(
            c,
            "doc.edit",
            &json!({"doc": a, "base": 2, "ops": [{"t": "ins", "pos": 0, "text": "mine "}]}),
        )
        .unwrap();
        std::fs::write(&p, "theirs\n").unwrap();
        s.check_disk();
        assert!(event(&mut rx, "doc.changed_on_disk", Duration::from_secs(1)).is_some());
        s.check_disk();
        assert!(event(&mut rx, "doc.changed_on_disk", Duration::from_millis(50)).is_none());
        assert_eq!(text(&s), "mine zero\none\n2\nthree\nfour\n");
        let info = orch.dispatch(c, "doc.reload", &json!({"doc": a})).unwrap();
        assert_eq!(info["modified"], false);
        assert_eq!(text(&s), "theirs\n");

        // Deleted outside: kept, unsaved.
        std::fs::remove_file(&p).unwrap();
        s.check_disk();
        let ev = event(&mut rx, "doc.deleted", Duration::from_secs(1)).unwrap();
        assert_eq!(ev["info"]["modified"], true);
    }

    #[test]
    fn a_large_file_is_indexed_in_the_background() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("big.txt");
        // Fixed-width lines: line i starts at byte 14 * i.
        let n = docs::LARGE_FILE_THRESHOLD / 14 + 50_000;
        let text: String = (0..n).map(|i| format!("line {i:08}\n")).collect();
        std::fs::write(&p, &text).unwrap();
        let docs = Mutex::new(Docs::default());
        let fs: workspace::Fs = Arc::new(StdFileSystem);
        let (id, _) = docs.lock().unwrap().open(1, &p, fs).unwrap();
        {
            let mut g = docs.lock().unwrap();
            let d = g.get(id).unwrap();
            assert_eq!(d.buffer.line_count(), None);
            // An edit before the index is built: two more lines, mid-file.
            d.edit(
                0,
                vec![ot::Prim::Ins {
                    pos: 14 * 500_000,
                    text: "x\ny\n".into(),
                }],
                false,
            )
            .unwrap();
        }
        build_line_index(&docs, &Room::new(Arc::default(), 1), id);
        let mut g = docs.lock().unwrap();
        let d = g.get(id).unwrap();
        assert_eq!(d.buffer.line_count(), Some(n + 3));
        let w = d.lines_forward(14 * 100 + 3, 1).unwrap();
        assert_eq!(
            (w["first_line"].clone(), w["text"].clone()),
            (json!(100), json!("line 00000100\n"))
        );
        let w = d.lines_forward(14 * 700_000 + 4, 1).unwrap();
        assert_eq!(
            (w["first_line"].clone(), w["text"].clone()),
            (json!(700_002), json!("line 00700000\n"))
        );
        assert_eq!(d.info()["lines"], json!(n + 3));
    }
}
