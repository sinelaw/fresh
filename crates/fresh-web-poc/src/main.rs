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

mod docs;
mod hub;
mod lsp;
mod ot;
mod plugins;
mod process;
mod terminal;
mod workspace;

use anyhow::{anyhow, Result};
use docs::Docs;
use hub::Hub;
use serde_json::{json, Value};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use workspace::{Local, Workspace};

mod assets {
    include!(concat!(env!("OUT_DIR"), "/assets.rs"));
}

pub struct App {
    ws: Workspace,
    hub: Arc<Hub>,
    docs: Arc<Mutex<Docs>>,
    terms: terminal::Terms,
    procs: process::Procs,
    watches: process::Watches,
    lsp: lsp::Lsp,
    plugins: Value,
}

fn arg<'a>(m: &'a Value, k: &str) -> Result<&'a Value> {
    m.get(k).filter(|v| !v.is_null()).ok_or_else(|| anyhow!("missing {k}"))
}
fn u(m: &Value, k: &str) -> Result<u64> {
    arg(m, k)?.as_u64().ok_or_else(|| anyhow!("{k} is not a number"))
}
fn s<'a>(m: &'a Value, k: &str) -> Result<&'a str> {
    arg(m, k)?.as_str().ok_or_else(|| anyhow!("{k} is not a string"))
}

impl App {
    fn new(root: PathBuf) -> Arc<App> {
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
            lsp: lsp::Lsp::new(root.clone(), on_notify),
            ws: Workspace { root, authority: Box::new(Local) },
            hub: hub.clone(),
            docs,
            terms: terminal::Terms::default(),
            procs: process::Procs::default(),
            watches: process::Watches::default(),
            plugins: plugins::compile_all(),
        });
        app.watches.start(hub);
        app
    }

    fn dispatch(&self, client: u64, op: &str, m: &Value) -> Result<Value> {
        let ws = &self.ws;
        Ok(match op {
            "hello" => {
                let id = self.hub.register();
                json!({
                    "client": id,
                    "root": ws.root.to_string_lossy(),
                    "name": ws.root.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default(),
                    "host": ws.authority.name(),
                    "terminals": self.terms.list(),
                })
            }
            "fs.list" => {
                let p = m.get("path").and_then(Value::as_str).map(|p| ws.resolve(p)).unwrap_or(ws.root.clone());
                json!({ "entries": ws.authority.list_dir(&p)? })
            }
            "fs.stat" => ws.authority.stat(&ws.resolve(s(m, "path")?)),
            "fs.read" => json!({ "text": ws.authority.read_file(&ws.resolve(s(m, "path")?), 16 << 20)? }),
            "fs.write" => {
                ws.authority.write_file(&ws.resolve(s(m, "path")?), s(m, "text")?)?;
                json!({})
            }
            "fs.find" => json!({ "files": ws.find_files(s(m, "query")?, 50) }),
            "fs.watch" => json!({ "watch": self.watches.add(client, ws.resolve(s(m, "path")?)) }),
            "fs.unwatch" => {
                self.watches.remove(u(m, "watch")?);
                json!({})
            }

            "doc.open" => {
                let path = ws.resolve(s(m, "path")?);
                let mut docs = self.docs.lock().unwrap();
                let (id, new) = docs.open(client, &path)?;
                let d = docs.get(id)?;
                if new && !d.buffer.is_large_file() {
                    if let Some(lang) = d.language.clone() {
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
                    }
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
                self.docs.lock().unwrap().get(u(m, "doc")?)?.lines_forward(u(m, "from")? as usize, count)?
            }
            "doc.lines_before" => {
                let count = m.get("count").and_then(Value::as_u64).unwrap_or(200) as usize;
                self.docs.lock().unwrap().get(u(m, "doc")?)?.lines_backward(u(m, "before")? as usize, count)?
            }
            "doc.window" => {
                // Lines before and after `around` in one read, so both halves
                // are of one version.
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                let before = m.get("before").and_then(Value::as_u64).unwrap_or(100) as usize;
                let after = m.get("after").and_then(Value::as_u64).unwrap_or(400) as usize;
                let at = d.line_start(u(m, "around")? as usize)?;
                let back = if before > 0 { Some(d.lines_backward(at, before)?) } else { None };
                let mut fwd = d.lines_forward(at, after)?;
                if let Some(b) = back {
                    let text = format!("{}{}", b["text"].as_str().unwrap_or(""), fwd["text"].as_str().unwrap_or(""));
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
                json!({ "pos": d.buffer.line_start_offset(u(m, "line")? as usize) })
            }
            "doc.edit" => self.edit(client, m)?,
            "doc.save" => {
                let mut docs = self.docs.lock().unwrap();
                let d = docs.get(u(m, "doc")?)?;
                d.save()?;
                let info = d.info();
                if let Some(server) = d.language.as_deref().and_then(|l| self.lsp.running(l)) {
                    server.notify("textDocument/didSave", json!({"textDocument": {"uri": lsp::uri_of(&d.path)}}));
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
                self.terms.resize(&self.hub, u(m, "term")?, c.max(2), r.max(2))?;
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
                    .map(|a| a.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                    .unwrap_or_default();
                let cwd = m.get("cwd").and_then(Value::as_str);
                json!({ "proc": self.procs.run(self.hub.clone(), ws, client, s(m, "program")?, &args, cwd)? })
            }
            "proc.kill" => json!({ "killed": self.procs.kill(u(m, "proc")?) }),

            "lsp.hover" | "lsp.definition" | "lsp.completion" => self.lsp_request(op, m)?,
            "lsp.status" => self.lsp.status(),

            _ => return Err(anyhow!("unknown op {op}")),
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
        let applied = d.edit(client, base, prims, server.is_some())?;
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

    fn lsp_close(&self, d: &docs::Doc) {
        if let Some(server) = d.language.as_deref().and_then(|l| self.lsp.running(l)) {
            server.notify("textDocument/didClose", json!({"textDocument": {"uri": lsp::uri_of(&d.path)}}));
        }
    }

    fn lsp_request(&self, op: &str, m: &Value) -> Result<Value> {
        let (server, uri, line, character) = {
            let mut docs = self.docs.lock().unwrap();
            let d = docs.get(u(m, "doc")?)?;
            let server = d
                .language
                .as_deref()
                .and_then(|l| self.lsp.running(l))
                .ok_or_else(|| anyhow!("no language server for this document"))?;
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
                        let insert = i["textEdit"]["newText"].as_str().or(i["insertText"].as_str()).or(i["label"].as_str()).unwrap_or("").to_string();
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
    let Some(path) = params["uri"].as_str().and_then(lsp::path_of) else { return };
    let mut docs = docs.lock().unwrap();
    let Some(id) = docs.by_path(&path) else { return };
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

fn asset(path: &str) -> Option<Vec<u8>> {
    if let Some(dir) = std::env::var_os("FRESH_POC_WEB") {
        let p = PathBuf::from(dir).join(path);
        if !p.components().any(|c| c.as_os_str() == "..") {
            return std::fs::read(p).ok();
        }
    }
    assets::ASSETS.iter().find(|(p, _)| *p == path).map(|(_, b)| b.to_vec())
}

fn respond(req: tiny_http::Request, status: u16, ctype: &str, body: Vec<u8>) {
    let h = tiny_http::Header::from_bytes(&b"Content-Type"[..], ctype.as_bytes()).unwrap();
    let nc = tiny_http::Header::from_bytes(&b"Cache-Control"[..], &b"no-store"[..]).unwrap();
    let _ = req.respond(tiny_http::Response::from_data(body).with_status_code(status).with_header(h).with_header(nc));
}

fn handle(app: Arc<App>, mut req: tiny_http::Request) {
    let url = req.url().to_string();
    let (path, query) = url.split_once('?').unwrap_or((&url, ""));
    match (req.method(), path) {
        (tiny_http::Method::Post, "/api") => {
            let mut body = String::new();
            let _ = req.as_reader().read_to_string(&mut body);
            let msg: Value = serde_json::from_str(&body).unwrap_or(Value::Null);
            let client = msg.get("client").and_then(Value::as_u64).unwrap_or(0);
            let op = msg.get("op").and_then(Value::as_str).unwrap_or("");
            let reply = match app.dispatch(client, op, &msg) {
                Ok(v) => json!({ "ok": v }),
                Err(e) => json!({ "error": e.to_string() }),
            };
            respond(req, 200, "application/json", reply.to_string().into_bytes());
        }
        (tiny_http::Method::Get, "/events") => {
            let client: u64 = query
                .split('&')
                .find_map(|kv| kv.strip_prefix("client="))
                .and_then(|v| v.parse().ok())
                .unwrap_or(0);
            let Some(rx) = app.hub.take_stream(client) else {
                respond(req, 404, "text/plain", b"no such client".to_vec());
                return;
            };
            let mut w = req.into_writer();
            let head = "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-store\r\nConnection: keep-alive\r\n\r\n";
            if w.write_all(head.as_bytes()).and_then(|_| w.flush()).is_err() {
                app.drop_client(client);
                return;
            }
            loop {
                let msg = match rx.recv_timeout(Duration::from_secs(15)) {
                    Ok(m) => format!("data: {m}\n\n"),
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout) => ": ping\n\n".to_string(),
                    Err(_) => break,
                };
                if w.write_all(msg.as_bytes()).and_then(|_| w.flush()).is_err() {
                    break;
                }
            }
            app.drop_client(client);
        }
        (tiny_http::Method::Get, "/plugins") => {
            respond(req, 200, "application/json", app.plugins.to_string().into_bytes());
        }
        (tiny_http::Method::Get, p) => {
            let p = p.trim_start_matches('/');
            let p = if p.is_empty() { "index.html" } else { p };
            match asset(p) {
                Some(b) => respond(req, 200, content_type(p), b),
                None => respond(req, 404, "text/plain", b"not found".to_vec()),
            }
        }
        _ => respond(req, 405, "text/plain", b"method not allowed".to_vec()),
    }
}

fn main() -> Result<()> {
    let mut port = 8090u16;
    let mut root: Option<PathBuf> = None;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--port" => port = args.next().and_then(|p| p.parse().ok()).ok_or_else(|| anyhow!("--port N"))?,
            "-h" | "--help" => {
                println!("usage: fresh-poc [--port N] [DIR]");
                return Ok(());
            }
            _ => root = Some(PathBuf::from(a)),
        }
    }
    let root = root.unwrap_or(std::env::current_dir()?).canonicalize()?;
    let app = App::new(root.clone());
    let server = tiny_http::Server::http(("127.0.0.1", port)).map_err(|e| anyhow!("{e}"))?;
    println!("fresh-poc serving {} on http://127.0.0.1:{port}/", root.display());
    for req in server.incoming_requests() {
        let app = app.clone();
        std::thread::spawn(move || handle(app, req));
    }
    Ok(())
}
