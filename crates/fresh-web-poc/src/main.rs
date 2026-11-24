//! `fresh-poc`: Fresh split into a data server and a thick web client.
//!
//! The server is responsible for data only: a workspace on a host, its files
//! and directories (with open documents served through a windowed API over
//! the editor's lazy piece tree, so huge files work), the ordered edit log of
//! each document, terminals, processes, and one connection per language
//! server. Everything about the experience — layout, editing, undo, plugins,
//! and every marker and decoration — lives in the client (`web/`).
//!
//! Usage: `fresh-poc [--port N] [DIR]`, then open the printed URL.

#[macro_use]
mod error;
mod docs;
mod hub;
mod lsp;
mod ot;
mod process;
mod terminal;
mod workspace;

use anyhow::{anyhow, Result};
use docs::Docs;
use error::{err, Code};
use fresh_editor_core::model::filesystem::StdFileSystem;
use hub::Hub;
use serde_json::{json, Value};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use workspace::{Agent, Authority, Container, Local, Ssh, Workspace};

mod assets {
    include!(concat!(env!("OUT_DIR"), "/assets.rs"));
}

pub struct App {
    ws: Arc<Workspace>,
    hub: Arc<Hub>,
    docs: Arc<Mutex<Docs>>,
    terms: terminal::Terms,
    procs: process::Procs,
    watches: process::Watches,
    lsp: lsp::Lsp,
    trusted: std::sync::atomic::AtomicBool,
}

fn arg<'a>(m: &'a Value, k: &str) -> Result<&'a Value> {
    m.get(k)
        .filter(|v| !v.is_null())
        .ok_or_else(|| err(Code::BadRequest, format!("missing {k}")))
}
fn u(m: &Value, k: &str) -> Result<u64> {
    arg(m, k)?
        .as_u64()
        .ok_or_else(|| err(Code::BadRequest, format!("{k} is not a number")))
}
fn s<'a>(m: &'a Value, k: &str) -> Result<&'a str> {
    arg(m, k)?
        .as_str()
        .ok_or_else(|| err(Code::BadRequest, format!("{k} is not a string")))
}

impl App {
    fn new(ws: Arc<Workspace>, trusted: bool) -> Arc<App> {
        let hub = Arc::new(Hub::default());
        let docs: Arc<Mutex<Docs>> = Arc::default();
        let on_notify: lsp::OnNotify = {
            let hub = hub.clone();
            let docs = docs.clone();
            Arc::new(move |_lang, method, params| {
                if method == "textDocument/publishDiagnostics" {
                    publish_diagnostics(&hub, &docs, params);
                }
            })
        };
        let app = Arc::new(App {
            lsp: lsp::Lsp::new(ws.clone(), on_notify),
            ws: ws.clone(),
            hub: hub.clone(),
            docs,
            terms: terminal::Terms::default(),
            procs: process::Procs::default(),
            watches: process::Watches::default(),
            trusted: std::sync::atomic::AtomicBool::new(trusted),
        });
        app.watches.start(hub, ws);
        app
    }

    fn dispatch(&self, client: u64, op: &str, m: &Value) -> Result<Value> {
        let ws = &self.ws;
        // An untrusted workspace runs nothing of its own: no processes, no
        // terminals, no language servers, until the user trusts it.
        let trusted = self.trusted.load(std::sync::atomic::Ordering::SeqCst);
        if !trusted && (op.starts_with("proc.") || op == "term.open" || op.starts_with("lsp.")) {
            fail!(
                Untrusted,
                "workspace is restricted: trust it to run processes, terminals and language servers"
            );
        }
        // What a client opens is released when its event stream ends, so
        // only a client with a live stream can open anything.
        if matches!(op, "doc.open" | "fs.watch" | "proc.run") && !self.hub.has(client) {
            fail!(UnknownClient, "unknown client {client}: say hello first");
        }
        Ok(match op {
            "hello" => {
                let id = self.hub.register();
                json!({
                    "client": id,
                    "root": ws.root.to_string_lossy(),
                    "name": ws.root.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default(),
                    "host": ws.authority.name(),
                    "terminals": self.terms.list(),
                    "trusted": trusted,
                    // Finding the servers runs a probe on the host: only
                    // once trusted (then sent with `workspace.trusted`).
                    "lsp": if trusted { self.lsp.languages() } else { vec![] },
                })
            }
            "workspace.trust" => {
                if !self.trusted.swap(true, std::sync::atomic::Ordering::SeqCst) {
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
                self.hub.broadcast(
                    &json!({"ev": "workspace.trusted", "lsp": self.lsp.languages()}),
                    None,
                );
                json!({})
            }
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
                ws.rename(&from, &to)?;
                self.docs.lock().unwrap().renamed(&from, &to);
                json!({})
            }
            "fs.delete" => {
                ws.remove(&ws.resolve(s(m, "path")?))?;
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

            "doc.open" => {
                let path = ws.resolve(s(m, "path")?);
                let mut docs = self.docs.lock().unwrap();
                let (id, new) = docs.open(client, &path, ws.fs())?;
                let d = docs.get(id)?;
                if new && d.buffer.line_count().is_none() {
                    index_lines(self.docs.clone(), self.hub.clone(), id);
                }
                if new && trusted {
                    self.lsp_open(d)?;
                }
                d.info()
            }
            "doc.close" => {
                let closed = self.docs.lock().unwrap().close(client, u(m, "doc")?);
                if let Some(d) = closed {
                    self.lsp_close(&d);
                }
                json!({})
            }
            "doc.info" => self.docs.lock().unwrap().get(u(m, "doc")?)?.info(),
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
                // are of one version.
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                let before = m.get("before").and_then(Value::as_u64).unwrap_or(100) as usize;
                let after = m.get("after").and_then(Value::as_u64).unwrap_or(400) as usize;
                let at = d.line_start(u(m, "around")? as usize)?;
                let back = if before > 0 {
                    Some(d.lines_backward(at, before)?)
                } else {
                    None
                };
                let mut fwd = d.lines_forward(at, after)?;
                if let Some(b) = back {
                    let text = format!(
                        "{}{}",
                        b["text"].as_str().unwrap_or(""),
                        fwd["text"].as_str().unwrap_or("")
                    );
                    fwd["text"] = json!(text);
                    fwd["start"] = b["start"].clone();
                    fwd["first_line"] = b["first_line"].clone();
                    fwd["bof"] = b["bof"].clone();
                }
                fwd
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
            "doc.edit" => self.edit(client, m)?,
            "doc.save" => {
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                let force = m.get("force").and_then(Value::as_bool).unwrap_or(false);
                d.save(&ws.recovery_dir(), force)?;
                let info = d.info();
                if let Some(server) = d.language.as_deref().and_then(|l| self.lsp.running(l)) {
                    server.notify(
                        "textDocument/didSave",
                        json!({"textDocument": {"uri": lsp::uri_of(&d.path)}}),
                    );
                }
                let ev = json!({"ev": "doc.saved", "doc": d.id, "info": info});
                for c in &d.clients {
                    self.hub.send(*c, &ev);
                }
                info
            }

            "term.open" => {
                let cols = m.get("cols").and_then(Value::as_u64).unwrap_or(80) as u16;
                let rows = m.get("rows").and_then(Value::as_u64).unwrap_or(24) as u16;
                json!({ "term": self.terms.open(self.hub.clone(), ws, cols, rows)? })
            }
            "term.input" => {
                self.terms.input(u(m, "term")?, s(m, "data")?)?;
                json!({})
            }
            "term.resize" => {
                let (c, r) = (u(m, "cols")? as u16, u(m, "rows")? as u16);
                self.terms
                    .resize(&self.hub, u(m, "term")?, c.max(2), r.max(2))?;
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
                json!({ "proc": self.procs.run(self.hub.clone(), ws, client, s(m, "program")?, &args, cwd)? })
            }
            "proc.kill" => json!({ "killed": self.procs.kill(u(m, "proc")?) }),

            "lsp.hover" | "lsp.definition" | "lsp.completion" => self.lsp_request(op, m)?,
            "lsp.call" => self.lsp_call(m)?,
            "lsp.status" => self.lsp.status(),

            _ => fail!(UnknownOp, "unknown op {op}"),
        })
    }

    fn edit(&self, client: u64, m: &Value) -> Result<Value> {
        let id = u(m, "doc")?;
        let base = u(m, "base")?;
        let prims: Vec<ot::Prim> = serde_json::from_value(arg(m, "ops")?.clone())?;
        let mut docs = self.docs.lock().unwrap();
        let d = docs.get(id)?;
        let server = (!d.buffer.is_large_file())
            .then(|| d.language.as_deref().and_then(|l| self.lsp.running(l)))
            .flatten();
        let applied = d.edit(base, prims, server.is_some())?;
        let ops: Vec<Value> = applied
            .iter()
            .map(|a| {
                let mut v = serde_json::to_value(&a.prim).unwrap_or(Value::Null);
                v["nl"] = json!(a.newlines);
                v
            })
            .collect();
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
                json!({"textDocument": {"uri": lsp::uri_of(&d.path), "version": d.version}, "contentChanges": changes}),
            );
        }
        // Sent under the documents lock, so every client sees every
        // document's log in order. The editing client takes the event about
        // its own edit as the acknowledgement.
        let ev = json!({
            "ev": "doc.ops", "doc": id, "version": d.version, "client": client,
            "ops": ops, "len": d.len(), "modified": d.buffer.is_modified(),
        });
        for c in &d.clients {
            self.hub.send(*c, &ev);
        }
        Ok(json!({ "version": d.version }))
    }

    /// Hand a newly opened document of an ordinary size to its language's
    /// server, starting the server if it is installed.
    fn lsp_open(&self, d: &mut docs::Doc) -> Result<()> {
        if d.buffer.is_large_file() {
            return Ok(());
        }
        let Some(lang) = d.language.clone() else {
            return Ok(());
        };
        if let Some(server) = self.lsp.server(&lang) {
            let text = d.full_text()?;
            server.notify(
                "textDocument/didOpen",
                json!({"textDocument": {
                    "uri": lsp::uri_of(&d.path), "languageId": lang,
                    "version": d.version, "text": text,
                }}),
            );
        }
        Ok(())
    }

    fn lsp_close(&self, d: &docs::Doc) {
        if let Some(server) = d.language.as_deref().and_then(|l| self.lsp.running(l)) {
            server.notify(
                "textDocument/didClose",
                json!({"textDocument": {"uri": lsp::uri_of(&d.path)}}),
            );
        }
    }

    /// Any request about a document, by method: the server fills in the
    /// document (and the position, given `pos` as a byte offset) and returns
    /// the language server's result as it came. Positions in it are the
    /// server's (line, UTF-16 column); the client converts them with its copy
    /// of the text.
    fn lsp_call(&self, m: &Value) -> Result<Value> {
        let method = s(m, "method")?.to_string();
        let mut params = m.get("params").cloned().unwrap_or(json!({}));
        let server = {
            let mut docs = self.docs.lock().unwrap();
            let d = docs.get(u(m, "doc")?)?;
            let server = d
                .language
                .as_deref()
                .and_then(|l| self.lsp.running(l))
                .ok_or_else(|| {
                    err(
                        Code::NoLanguageServer,
                        "no language server for this document",
                    )
                })?;
            params["textDocument"] = json!({"uri": lsp::uri_of(&d.path)});
            if let Some(p) = m.get("pos").and_then(Value::as_u64) {
                let (l, c) = d.buffer.position_to_lsp_position(p as usize);
                params["position"] = json!({"line": l, "character": c});
            }
            server
        };
        server.request(&method, params, Duration::from_secs(20))
    }

    fn lsp_request(&self, op: &str, m: &Value) -> Result<Value> {
        let (server, uri, line, character) = {
            let mut docs = self.docs.lock().unwrap();
            let d = docs.get(u(m, "doc")?)?;
            let server = d
                .language
                .as_deref()
                .and_then(|l| self.lsp.running(l))
                .ok_or_else(|| {
                    err(
                        Code::NoLanguageServer,
                        "no language server for this document",
                    )
                })?;
            let (l, c) = d.buffer.position_to_lsp_position(u(m, "pos")? as usize);
            (server, lsp::uri_of(&d.path), l, c)
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

    fn drop_client(&self, client: u64) {
        self.hub.forget(client);
        self.watches.drop_client(client);
        self.procs.drop_client(client);
        let closed = self.docs.lock().unwrap().drop_client(client);
        for d in closed {
            self.lsp_close(&d);
        }
    }
}

/// Diagnostics as data, in byte offsets, tagged with the version they
/// describe; a report for an older version than the document's is dropped
/// (the server will send a fresh one).
fn publish_diagnostics(hub: &Hub, docs: &Mutex<Docs>, params: &Value) {
    let Some(path) = params["uri"].as_str().and_then(lsp::path_of) else {
        return;
    };
    let mut docs = docs.lock().unwrap();
    let Some(id) = docs.by_path(&path) else {
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
    let ev = json!({"ev": "lsp.diagnostics", "doc": id, "version": d.version, "items": items});
    for c in &d.clients {
        hub.send(*c, &ev);
    }
}

/// Build a document's line index in the background: every document gets
/// one; a huge file's is counted here, a chunk at a time, through its
/// filesystem (a remote one counts on the remote side, so the bytes never
/// cross the link). Counts are kept by file range, which edits made meanwhile
/// cannot change; regions edited meanwhile are in memory and counted there.
fn index_lines(docs: Arc<Mutex<Docs>>, hub: Arc<Hub>, id: u64) {
    std::thread::spawn(move || build_line_index(&docs, &hub, id));
}

fn build_line_index(docs: &Mutex<Docs>, hub: &Hub, id: u64) {
    let (fs, work, total) = {
        let mut g = docs.lock().unwrap();
        let Ok(d) = g.get(id) else { return };
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
        let Ok(n) = fs.count_line_feeds_in_range(workspace::host(&path).as_host_path(), off, len)
        else {
            return;
        };
        counts.insert((path, off, len), n);
        done += len;
        if last.elapsed() > Duration::from_millis(250) {
            last = std::time::Instant::now();
            hub.broadcast(
                &json!({"ev": "doc.indexing", "doc": id, "done": done, "total": total}),
                None,
            );
        }
    }
    let mut g = docs.lock().unwrap();
    let Ok(d) = g.get(id) else { return };
    let (chunks, _) = d.buffer.prepare_line_scan();
    let leaves = d.buffer.piece_tree_leaves();
    let mut updates = Vec::new();
    for c in chunks.iter().filter(|c| !c.already_known) {
        let leaf = &leaves[c.leaf_index];
        let n = match d.buffer.leaf_io_params(leaf) {
            Some(k) => counts.get(&k).copied().or_else(|| {
                fs.count_line_feeds_in_range(workspace::host(&k.0).as_host_path(), k.1, k.2)
                    .ok()
            }),
            None => d.buffer.scan_leaf(leaf).ok(),
        };
        let Some(n) = n else { return };
        updates.push((c.leaf_index, n));
    }
    d.buffer.apply_scan_updates(&updates);
    let ev = json!({"ev": "doc.indexed", "doc": id, "lines": d.buffer.line_count(), "version": d.version});
    for c in &d.clients {
        hub.send(*c, &ev);
    }
}

fn content_type(path: &str) -> &'static str {
    match path.rsplit('.').next() {
        Some("html") => "text/html; charset=utf-8",
        Some("js") => "text/javascript; charset=utf-8",
        Some("css") => "text/css; charset=utf-8",
        Some("json") => "application/json",
        Some("svg") => "image/svg+xml",
        _ => "application/octet-stream",
    }
}

/// One of the web client's own files: embedded in the binary, or read from
/// `FRESH_POC_WEB` while developing the client. These are this program's
/// files, on this machine, not the workspace's, so they are the one read that
/// does not go through the authority.
fn asset(path: &str) -> Option<Vec<u8>> {
    if let Some(dir) = std::env::var_os("FRESH_POC_WEB") {
        let p = PathBuf::from(dir).join(path);
        if !p.components().any(|c| c.as_os_str() == "..") {
            return std::fs::read(p).ok();
        }
    }
    assets::ASSETS
        .iter()
        .find(|(p, _)| *p == path)
        .map(|(_, b)| b.to_vec())
}

fn main() -> Result<()> {
    let mut port = 8090u16;
    let mut trusted = true;
    let mut ssh: Option<String> = None;
    let mut container: Option<String> = None;
    let mut agent = false;
    let mut slow = false;
    let mut root: Option<String> = None;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--port" => {
                port = args
                    .next()
                    .and_then(|p| p.parse().ok())
                    .ok_or_else(|| anyhow!("--port N"))?
            }
            "--restricted" => trusted = false,
            "--ssh" => {
                ssh = Some(
                    args.next()
                        .ok_or_else(|| anyhow!("--ssh [user@]host[:port]"))?,
                )
            }
            "--container" => {
                container = Some(
                    args.next()
                        .ok_or_else(|| anyhow!("--container NAME_OR_ID"))?,
                )
            }
            "--agent" => agent = true,
            "--slow-network" => slow = true,
            "-h" | "--help" => {
                println!(
                    "usage: fresh-poc [--port N] [--restricted] [--ssh [user@]host[:port] | --container NAME | --agent] [--slow-network] [DIR]\n\n\
                     --ssh            the workspace is on another machine (files through the editor's remote agent)\n\
                     --container      processes, terminals and language servers run in a running container\n\
                     \x20                (docker exec); DIR must be bind-mounted at the same path in it\n\
                     --agent          the remote agent on this machine, without ssh (the remote code path, locally)\n\
                     --slow-network   add network-like delays to every filesystem call\n\
                     --restricted     start untrusted: no processes, terminals or language servers until trusted"
                );
                return Ok(());
            }
            _ => root = Some(a),
        }
    }
    if [ssh.is_some(), container.is_some(), agent]
        .iter()
        .filter(|&&b| b)
        .count()
        > 1
    {
        return Err(anyhow!("--ssh, --container and --agent exclude each other"));
    }
    let in_container = container.is_some();
    let authority: Arc<dyn Authority> = if let Some(target) = ssh {
        let params = fresh::services::remote::ConnectionParams::parse(&target)
            .ok_or_else(|| anyhow!("bad --ssh target"))?;
        let runtime = tokio::runtime::Runtime::new()?;
        let conn = runtime.block_on(fresh::services::remote::SshConnection::connect(
            params.clone(),
        ))?;
        let rfs = fresh::services::remote::RemoteFileSystem::new(
            conn.channel(),
            conn.connection_string(),
        );
        runtime.block_on(rfs.prime_sys_info())?;
        Arc::new(Ssh {
            params,
            fs: maybe_slow(Arc::new(rfs), slow),
            _runtime: runtime,
            _conn: conn,
        })
    } else if let Some(id) = container {
        Arc::new(Container {
            id,
            fs: maybe_slow(Arc::new(StdFileSystem), slow),
        })
    } else if agent {
        let runtime = tokio::runtime::Runtime::new()?;
        let channel = runtime.block_on(fresh::services::remote::spawn_local_agent())?;
        let rfs = fresh::services::remote::RemoteFileSystem::new(channel, "agent".into());
        runtime.block_on(rfs.prime_sys_info())?;
        Arc::new(Agent {
            fs: maybe_slow(Arc::new(rfs), slow),
            _runtime: runtime,
        })
    } else {
        let mut l = Local::new();
        l.fs = maybe_slow(l.fs, slow);
        Arc::new(l)
    };

    let given = PathBuf::from(root.unwrap_or_else(|| ".".into()));
    let fs = authority.fs();
    let root: PathBuf = fs
        .canonicalize(workspace::host(&given).as_host_path())
        .map(PathBuf::from)
        .map_err(|e| anyhow!("{}: {e}", given.display()))?;
    if in_container {
        // Files are read here and processes run in the container, against
        // the same paths: the root must be bind-mounted at its own path.
        let seen = authority
            .command(
                "test",
                &["-d".into(), root.to_string_lossy().into()],
                Path::new("/"),
            )
            .stdin(std::process::Stdio::null())
            .output()
            .map_err(|e| anyhow!("{}: cannot run docker: {e}", authority.name()))?;
        let err = String::from_utf8_lossy(&seen.stderr);
        if !err.trim().is_empty() {
            return Err(anyhow!("{}: {}", authority.name(), err.trim()));
        }
        if !seen.status.success() {
            return Err(anyhow!(
                "{}: {} is not a folder in the container; bind-mount it at the same path",
                authority.name(),
                root.display(),
            ));
        }
    }
    let ws = Arc::new(Workspace {
        root: root.clone(),
        authority,
    });
    let app = App::new(ws.clone(), trusted);
    println!(
        "fresh-poc serving {} ({}) on http://127.0.0.1:{port}/",
        root.display(),
        ws.authority.name()
    );
    tokio::runtime::Runtime::new()?.block_on(serve(app, port))
}

/// Requests from clients: `POST /api` (one JSON operation, run on the
/// blocking pool, since it may wait on a lock or a language server),
/// `GET /events` (the client's Server-Sent Events stream), and the client's
/// own files.
async fn serve(app: Arc<App>, port: u16) -> Result<()> {
    use axum::routing::{get, post};
    let router = axum::Router::new()
        .route("/api", post(api))
        .route("/events", get(events))
        .fallback(get(static_file))
        .with_state(app);
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", port)).await?;
    axum::serve(listener, router).await?;
    Ok(())
}

async fn api(
    axum::extract::State(app): axum::extract::State<Arc<App>>,
    body: axum::body::Bytes,
) -> axum::response::Response {
    use axum::response::IntoResponse;
    let reply = tokio::task::spawn_blocking(move || {
        let msg: Value = serde_json::from_slice(&body).unwrap_or(Value::Null);
        let client = msg.get("client").and_then(Value::as_u64).unwrap_or(0);
        let op = msg
            .get("op")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_string();
        match app.dispatch(client, &op, &msg) {
            Ok(v) => json!({ "ok": v }),
            Err(e) => json!({ "error": error::to_json(&e) }),
        }
    })
    .await
    .unwrap_or_else(|e| json!({ "error": {"code": "failed", "message": e.to_string()} }));
    (
        [(axum::http::header::CACHE_CONTROL, "no-store")],
        axum::Json(reply),
    )
        .into_response()
}

#[derive(serde::Deserialize)]
struct EventsQuery {
    client: u64,
}

/// Forgets the client when its event stream goes away.
struct ClientGuard(Arc<App>, u64);
impl Drop for ClientGuard {
    fn drop(&mut self) {
        self.0.drop_client(self.1);
    }
}

async fn events(
    axum::extract::State(app): axum::extract::State<Arc<App>>,
    axum::extract::Query(q): axum::extract::Query<EventsQuery>,
) -> axum::response::Response {
    use axum::response::sse::{Event, KeepAlive, Sse};
    use axum::response::IntoResponse;
    let Some(rx) = app.hub.take_stream(q.client) else {
        return (axum::http::StatusCode::NOT_FOUND, "no such client").into_response();
    };
    let guard = ClientGuard(app, q.client);
    let stream = futures_util::stream::unfold((rx, guard), |(mut rx, guard)| async move {
        let msg = rx.recv().await?;
        Some((
            Ok::<_, std::convert::Infallible>(Event::default().data(msg)),
            (rx, guard),
        ))
    });
    Sse::new(stream)
        .keep_alive(KeepAlive::default())
        .into_response()
}

async fn static_file(uri: axum::http::Uri) -> axum::response::Response {
    use axum::response::IntoResponse;
    let p = uri.path().trim_start_matches('/');
    let p = if p.is_empty() { "index.html" } else { p };
    match asset(p) {
        Some(b) => (
            [
                (axum::http::header::CONTENT_TYPE, content_type(p)),
                (axum::http::header::CACHE_CONTROL, "no-store"),
            ],
            b,
        )
            .into_response(),
        None => (axum::http::StatusCode::NOT_FOUND, "not found").into_response(),
    }
}

/// The filesystem, with network-like delays when asked for.
fn maybe_slow(fs: workspace::Fs, slow: bool) -> workspace::Fs {
    if slow {
        Arc::new(fresh::services::fs::slow::SlowFileSystem::new(
            fs,
            fresh::services::fs::slow::SlowFsConfig::slow_network(),
        ))
    } else {
        fs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    fn app(trusted: bool) -> (tempfile::TempDir, Arc<App>) {
        let dir = tempfile::tempdir().unwrap();
        let ws = Arc::new(Workspace {
            root: dir.path().canonicalize().unwrap(),
            authority: Arc::new(Local::new()),
        });
        (dir, App::new(ws, trusted))
    }

    /// A client with its event stream open, as after `GET /events`.
    fn client(app: &App) -> (u64, tokio::sync::mpsc::UnboundedReceiver<String>) {
        let id = app.dispatch(0, "hello", &json!({})).unwrap()["client"]
            .as_u64()
            .unwrap();
        (id, app.hub.take_stream(id).unwrap())
    }

    fn event(
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
        let (dir, app) = app(false);
        std::fs::write(dir.path().join("a.py"), "x = 1\n").unwrap();
        let (c, mut rx) = client(&app);
        let hello = app.dispatch(0, "hello", &json!({})).unwrap();
        assert_eq!(hello["trusted"], false);
        assert_eq!(hello["lsp"], json!([]), "no probe while restricted");
        assert!(!app.lsp.probed(), "nothing was run");

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
            let e = app.dispatch(c, op, &m).unwrap_err();
            assert_eq!(error::code_of(&e), Code::Untrusted, "{op}: {e}");
        }
        // Files and documents work as ever, without a language server.
        app.dispatch(c, "doc.open", &json!({"path": "a.py"}))
            .unwrap();
        assert!(!app.lsp.probed());
        assert!(app.terms.list().is_empty());

        app.dispatch(c, "workspace.trust", &json!({})).unwrap();
        let ev = event(&mut rx, "workspace.trusted", Duration::from_secs(5)).unwrap();
        assert!(ev["lsp"].is_array());
        let p = app.dispatch(c, "proc.run", &run).unwrap()["proc"].clone();
        let exit = event(&mut rx, "proc.exit", Duration::from_secs(10)).unwrap();
        assert_eq!(
            (exit["proc"].clone(), exit["stdout"].clone()),
            (p, json!("ran\n"))
        );
    }

    #[test]
    fn a_disconnected_client_releases_what_it_opened() {
        let (dir, app) = app(true);
        std::fs::write(dir.path().join("a.txt"), "a\n").unwrap();
        std::fs::write(dir.path().join("b.txt"), "b\n").unwrap();
        let (c, _rx) = client(&app);
        let (other, _orx) = client(&app);
        let open = |client, p: &str| {
            app.dispatch(client, "doc.open", &json!({ "path": p }))
                .unwrap()["doc"]
                .as_u64()
                .unwrap()
        };
        let a = open(c, "a.txt");
        let b = open(c, "b.txt");
        let shared = open(other, "a.txt");
        assert_eq!(shared, a);
        app.dispatch(
            c,
            "doc.edit",
            &json!({"doc": b, "base": 0, "ops": [{"t": "ins", "pos": 0, "text": "x"}]}),
        )
        .unwrap();
        app.dispatch(c, "fs.watch", &json!({"path": "a.txt"}))
            .unwrap();
        app.dispatch(c, "proc.run", &json!({"program": "sleep", "args": ["30"]}))
            .unwrap();
        assert_eq!((app.watches.count(), app.procs.count()), (1, 1));

        app.drop_client(c);
        assert_eq!(app.watches.count(), 0);
        let end = Instant::now() + Duration::from_secs(5);
        while app.procs.count() > 0 && Instant::now() < end {
            std::thread::sleep(Duration::from_millis(10));
        }
        assert_eq!(app.procs.count(), 0, "its process was killed");
        {
            let mut docs = app.docs.lock().unwrap();
            // Still open by the other client; unsaved edits are kept.
            assert_eq!(docs.get(a).unwrap().clients, [other].into());
            assert!(docs.get(b).unwrap().clients.is_empty());
        }
        let e = app
            .dispatch(c, "doc.open", &json!({"path": "a.txt"}))
            .unwrap_err();
        assert_eq!(error::code_of(&e), Code::UnknownClient, "{e}");

        app.drop_client(other);
        let mut docs = app.docs.lock().unwrap();
        assert!(docs.get(a).is_err(), "closed by every client: dropped");
        assert!(docs.get(b).is_ok());
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
        build_line_index(&docs, &Hub::default(), id);
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
