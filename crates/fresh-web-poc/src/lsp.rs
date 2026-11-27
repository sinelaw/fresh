//! One language-server connection per language, shared by every client.
//!
//! A language server expects exactly one client keeping its documents in
//! sync, so the server owns that connection and drives the sync from its own
//! edit log: `didOpen` when a document of the language opens, an incremental
//! `didChange` for every primitive the log sequences. Requests from clients
//! (hover, definition, completion) are routed in and their replies routed
//! back to whoever asked. Diagnostics are broadcast as plain data tagged with
//! the document version they describe; each client anchors them in its own
//! marker trees. The server keeps no annotations itself.

use crate::authority::{Authority, HostProc};
use crate::workspace::Workspace;
use anyhow::{anyhow, Result};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Stdio};
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;

/// Called with every notification a server sends (`method`, `params`).
pub type OnNotify = Arc<dyn Fn(&str, ServerEvent) + Send + Sync>;

/// What a language's server tells the workspace unasked: a notification
/// (its method and params), or that its output ended (it exited, or its
/// link went).
pub enum ServerEvent<'a> {
    Notification(&'a str, &'a Value),
    Ended,
}

/// The command for a language's server.
pub fn command_for(language: &str) -> Option<Vec<String>> {
    let cmd: &[&str] = match language {
        "python" => &["pyright-langserver", "--stdio"],
        "rust" => &["rust-analyzer"],
        "typescript" | "javascript" => &["typescript-language-server", "--stdio"],
        "c" | "cpp" => &["clangd"],
        "go" => &["gopls"],
        _ => return None,
    };
    Some(cmd.iter().map(|s| s.to_string()).collect())
}

pub fn uri_of(path: &Path) -> String {
    let mut s = String::from("file://");
    for b in path.to_string_lossy().bytes() {
        if b.is_ascii_alphanumeric() || b"/-_.~".contains(&b) {
            s.push(b as char);
        } else {
            s.push_str(&format!("%{b:02X}"));
        }
    }
    s
}

pub fn path_of(uri: &str) -> Option<PathBuf> {
    let rest = uri.strip_prefix("file://")?;
    let bytes = rest.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(v) = u8::from_str_radix(&rest[i + 1..i + 3], 16) {
                out.push(v);
                i += 3;
                continue;
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    Some(PathBuf::from(String::from_utf8_lossy(&out).into_owned()))
}

pub struct Server {
    pub language: String,
    queue: Sender<Value>,
    pending: Arc<Mutex<HashMap<i64, Sender<Value>>>>,
    /// Its output ended (it exited, or its link went): nothing it was
    /// asked will be answered.
    ended: Arc<AtomicBool>,
    next_id: AtomicI64,
    child: Mutex<Child>,
    authority: Arc<dyn Authority>,
    host: HostProc,
}

fn write_msg(w: &mut ChildStdin, msg: &Value) -> std::io::Result<()> {
    let body = msg.to_string();
    write!(w, "Content-Length: {}\r\n\r\n{}", body.len(), body)?;
    w.flush()
}

fn read_msg(r: &mut BufReader<impl Read>) -> Option<Value> {
    let mut len = 0usize;
    loop {
        let mut line = String::new();
        if r.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        if let Some(v) = line.strip_prefix("Content-Length:") {
            len = v.trim().parse().ok()?;
        }
    }
    let mut body = vec![0u8; len];
    r.read_exact(&mut body).ok()?;
    serde_json::from_slice(&body).ok()
}

impl Server {
    fn start(
        language: &str,
        cmd: &[String],
        ws: &Workspace,
        on_notify: OnNotify,
    ) -> Result<Arc<Self>> {
        let (mut command, host) = ws.authority.command(&cmd[0], &cmd[1..], &ws.root);
        let mut child = command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()?;
        let mut stdin = child.stdin.take().ok_or_else(|| anyhow!("no stdin"))?;
        let stdout = child.stdout.take().ok_or_else(|| anyhow!("no stdout"))?;
        let (queue, rx): (Sender<Value>, Receiver<Value>) = channel();
        let pending: Arc<Mutex<HashMap<i64, Sender<Value>>>> = Arc::default();
        let ended = Arc::new(AtomicBool::new(false));
        let server = Arc::new(Server {
            language: language.to_string(),
            queue,
            pending: pending.clone(),
            ended: ended.clone(),
            next_id: AtomicI64::new(1),
            child: Mutex::new(child),
            authority: ws.authority.clone(),
            host,
        });

        // Replies to the server's own requests go out through the writer
        // thread too, so all writes are ordered.
        let replies = server.queue.clone();
        let lang = language.to_string();
        let pending_r = pending.clone();
        std::thread::spawn(move || {
            let mut r = BufReader::new(stdout);
            while let Some(msg) = read_msg(&mut r) {
                let method = msg.get("method").and_then(Value::as_str);
                match (method, msg.get("id")) {
                    (None, Some(id)) => {
                        if let Some(tx) = id
                            .as_i64()
                            .and_then(|i| pending_r.lock().unwrap().remove(&i))
                        {
                            let _ = tx.send(msg);
                        }
                    }
                    (Some(m), Some(id)) => {
                        // A request from the server: answer with defaults.
                        let result = if m == "workspace/configuration" {
                            let n = msg["params"]["items"]
                                .as_array()
                                .map(|a| a.len())
                                .unwrap_or(0);
                            Value::Array(vec![Value::Null; n])
                        } else {
                            Value::Null
                        };
                        let _ = replies.send(json!({"jsonrpc": "2.0", "id": id, "result": result}));
                    }
                    (Some(m), None) => {
                        on_notify(&lang, ServerEvent::Notification(m, &msg["params"]))
                    }
                    _ => {}
                }
            }
            // Its output ended: what it was asked fails now (the waits'
            // senders dropped), not when each times out.
            ended.store(true, Ordering::SeqCst);
            pending_r.lock().unwrap().clear();
            on_notify(&lang, ServerEvent::Ended);
        });

        // The writer: initialize first, then everything queued, in order.
        let root_uri = uri_of(&ws.root);
        let pending_w = pending;
        std::thread::spawn(move || {
            let (tx, init_rx) = channel();
            pending_w.lock().unwrap().insert(0, tx);
            let init = json!({
                "jsonrpc": "2.0", "id": 0, "method": "initialize",
                "params": {
                    // The server runs on the workspace's host, where this
                    // process's id means nothing; it exits when its stdin
                    // closes.
                    "processId": null,
                    "rootUri": root_uri,
                    "workspaceFolders": [{"uri": root_uri, "name": "workspace"}],
                    "capabilities": {
                        "textDocument": {
                            "synchronization": {"didSave": true},
                            "publishDiagnostics": {"versionSupport": true},
                            "hover": {"contentFormat": ["markdown", "plaintext"]},
                            "completion": {"completionItem": {"snippetSupport": false}},
                            "definition": {},
                        },
                        "workspace": {"configuration": true, "workspaceFolders": true},
                    },
                },
            });
            if write_msg(&mut stdin, &init).is_err() {
                return;
            }
            if init_rx.recv_timeout(Duration::from_secs(60)).is_err() {
                return;
            }
            let _ = write_msg(
                &mut stdin,
                &json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}),
            );
            while let Ok(msg) = rx.recv() {
                if write_msg(&mut stdin, &msg).is_err() {
                    break;
                }
            }
        });
        Ok(server)
    }

    pub fn notify(&self, method: &str, params: Value) {
        let _ = self
            .queue
            .send(json!({"jsonrpc": "2.0", "method": method, "params": params}));
    }

    /// Whether it ended (see `ended`).
    pub fn has_ended(&self) -> bool {
        self.ended.load(Ordering::SeqCst)
    }

    /// Why it ended, as its process says.
    fn ending(&self) -> String {
        match self.child.lock().unwrap().try_wait() {
            Ok(Some(status)) => format!("the {} language server exited ({status})", self.language),
            _ => format!("the {} language server stopped answering", self.language),
        }
    }

    /// Send a request and wait for its reply (failing at once once it
    /// ended).
    pub fn request(&self, method: &str, params: Value, timeout: Duration) -> Result<Value> {
        if self.has_ended() {
            fail!(LanguageServer, "{method}: {}", self.ending());
        }
        let id = self.next_id.fetch_add(1, Ordering::SeqCst);
        let (tx, rx) = channel();
        self.pending.lock().unwrap().insert(id, tx);
        let _ = self
            .queue
            .send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
        let reply = rx.recv_timeout(timeout).map_err(|_| {
            self.pending.lock().unwrap().remove(&id);
            crate::error::err(
                crate::error::Code::LanguageServer,
                format!("{method}: no reply from {} server", self.language),
            )
        })?;
        if let Some(e) = reply.get("error") {
            fail!(LanguageServer, "{method}: {e}");
        }
        Ok(reply["result"].clone())
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        // The local child lives as long as the server on the host (or the
        // link to it): only a running one is stopped there.
        let mut child = self.child.lock().unwrap();
        let running = matches!(child.try_wait(), Ok(None));
        if running {
            self.authority.stop(&self.host);
        }
        let _ = child.kill();
        let _ = child.wait();
        if running {
            self.authority.release(&self.host);
        }
    }
}

/// A language's server in a workspace, once first asked for: running,
/// stopped by the user, or failed to start (why). A stopped or failed one
/// stays so (no file of the language starts it) until restarted.
#[derive(Clone)]
enum Slot {
    Running(Arc<Server>),
    Stopped,
    Failed(String),
}

impl Slot {
    /// Its server, while it runs (one that ended is failed, until
    /// restarted).
    fn running(&self) -> Option<Arc<Server>> {
        match self {
            Slot::Running(s) if !s.has_ended() => Some(s.clone()),
            Slot::Running(_) | Slot::Stopped | Slot::Failed(_) => None,
        }
    }

    /// As `lsp.status` reports it (`LspState`, types/fresh.d.js).
    fn state(&self) -> Value {
        match self {
            Slot::Running(s) if s.has_ended() => json!({"kind": "failed", "why": s.ending()}),
            Slot::Running(_) => json!({"kind": "running"}),
            Slot::Stopped => json!({"kind": "stopped"}),
            Slot::Failed(why) => json!({"kind": "failed", "why": why}),
        }
    }
}

/// The servers, one per language, started on first use, on the
/// workspace's host.
pub struct Lsp {
    ws: Arc<Workspace>,
    servers: Mutex<HashMap<String, Slot>>,
    /// The programs of [`LANGUAGES`]' servers found on the host: probed once,
    /// with one command, the first time it is asked.
    installed: OnceLock<HashSet<String>>,
    on_notify: OnNotify,
}

/// The languages a server is configured for, in the order reported.
pub const LANGUAGES: &[&str] = &[
    "python",
    "rust",
    "javascript",
    "typescript",
    "c",
    "cpp",
    "go",
];

/// A shell script printing, one per line, each of `programs` found on the
/// `PATH`.
fn probe_script(programs: &[String]) -> String {
    let words: Vec<String> = programs
        .iter()
        .map(|p| format!("'{}'", p.replace('\'', "'\\''")))
        .collect();
    format!(
        "for p in {}; do command -v \"$p\" >/dev/null 2>&1 && echo \"$p\"; done; true",
        words.join(" ")
    )
}

impl Lsp {
    pub fn new(ws: Arc<Workspace>, on_notify: OnNotify) -> Self {
        Lsp {
            ws,
            servers: Mutex::default(),
            installed: OnceLock::new(),
            on_notify,
        }
    }

    fn installed(&self) -> &HashSet<String> {
        self.installed.get_or_init(|| {
            let mut programs: Vec<String> = LANGUAGES
                .iter()
                .filter_map(|l| command_for(l).map(|c| c[0].clone()))
                .collect();
            programs.sort();
            programs.dedup();
            self.ws
                .authority
                .command("sh", &["-c".into(), probe_script(&programs)], &self.ws.root)
                .0
                .stdin(Stdio::null())
                .stderr(Stdio::null())
                .output()
                .map(|o| {
                    String::from_utf8_lossy(&o.stdout)
                        .lines()
                        .map(str::to_string)
                        .collect()
                })
                .unwrap_or_else(|e| {
                    // No server can be found: say why, where it is seen.
                    eprintln!(
                        "{}: could not look for language servers: {e}",
                        self.ws.root.display()
                    );
                    HashSet::new()
                })
        })
    }

    /// Whether the host was probed for servers yet.
    #[cfg(test)]
    pub fn probed(&self) -> bool {
        self.installed.get().is_some()
    }

    /// The languages whose server is installed on the host.
    pub fn languages(&self) -> Vec<&'static str> {
        LANGUAGES
            .iter()
            .copied()
            .filter(|l| self.available(l))
            .collect()
    }

    /// Whether `language`'s server is installed on the host.
    pub fn available(&self, language: &str) -> bool {
        command_for(language).is_some_and(|cmd| self.installed().contains(&cmd[0]))
    }

    /// The server for `language`, starting it if it is installed.
    pub fn server(&self, language: &str) -> Option<Arc<Server>> {
        if !self.available(language) {
            return None;
        }
        let mut m = self.servers.lock().unwrap();
        if let Some(s) = m.get(language) {
            return s.running();
        }
        let cmd = command_for(language)?;
        let slot = match Server::start(language, &cmd, &self.ws, self.on_notify.clone()) {
            Ok(s) => Slot::Running(s),
            Err(e) => Slot::Failed(format!("{}: {e}", cmd.join(" "))),
        };
        let s = slot.running();
        m.insert(language.to_string(), slot);
        s
    }

    /// Stop `language`'s server; it stays stopped (no file of the
    /// language starts it) until [`Self::restart`].
    pub fn stop(&self, language: &str) {
        let old = self
            .servers
            .lock()
            .unwrap()
            .insert(language.to_string(), Slot::Stopped);
        // Stopped on the host as it is dropped, outside the lock.
        drop(old);
    }

    /// Start `language`'s server again (stopping the one running): the
    /// new one, or `None` when it is not installed or did not start (its
    /// status then says why).
    pub fn restart(&self, language: &str) -> Option<Arc<Server>> {
        let old = self.servers.lock().unwrap().remove(language);
        drop(old);
        self.server(language)
    }

    /// Stop every server (a server is stopped on the host when dropped).
    pub fn stop_all(&self) {
        let servers: Vec<_> = self.servers.lock().unwrap().drain().collect();
        drop(servers);
    }

    /// The server for `language` only if it is already running.
    pub fn running(&self, language: &str) -> Option<Arc<Server>> {
        self.servers
            .lock()
            .unwrap()
            .get(language)
            .and_then(Slot::running)
    }

    /// Each language's server asked for so far: running, stopped or failed.
    pub fn status(&self) -> Value {
        let m = self.servers.lock().unwrap();
        json!(m
            .iter()
            .map(|(k, v)| (k.clone(), v.state()))
            .collect::<serde_json::Map<_, _>>())
    }
}
