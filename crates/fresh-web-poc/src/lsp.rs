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

use crate::workspace::Workspace;
use anyhow::{anyhow, Result};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Stdio};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// Called with every notification a server sends (`method`, `params`).
pub type OnNotify = Arc<dyn Fn(&str, &str, &Value) + Send + Sync>;

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
    next_id: AtomicI64,
    child: Mutex<Child>,
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
        let mut child = ws
            .authority
            .command(&cmd[0], &cmd[1..], &ws.root)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()?;
        let mut stdin = child.stdin.take().ok_or_else(|| anyhow!("no stdin"))?;
        let stdout = child.stdout.take().ok_or_else(|| anyhow!("no stdout"))?;
        let (queue, rx): (Sender<Value>, Receiver<Value>) = channel();
        let pending: Arc<Mutex<HashMap<i64, Sender<Value>>>> = Arc::default();
        let server = Arc::new(Server {
            language: language.to_string(),
            queue,
            pending: pending.clone(),
            next_id: AtomicI64::new(1),
            child: Mutex::new(child),
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
                    (Some(m), None) => on_notify(&lang, m, &msg["params"]),
                    _ => {}
                }
            }
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
                    "processId": std::process::id(),
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

    /// Send a request and wait for its reply.
    pub fn request(&self, method: &str, params: Value, timeout: Duration) -> Result<Value> {
        let id = self.next_id.fetch_add(1, Ordering::SeqCst);
        let (tx, rx) = channel();
        self.pending.lock().unwrap().insert(id, tx);
        let _ = self
            .queue
            .send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
        let reply = rx.recv_timeout(timeout).map_err(|_| {
            self.pending.lock().unwrap().remove(&id);
            anyhow!("{method}: no reply from {} server", self.language)
        })?;
        if let Some(e) = reply.get("error") {
            return Err(anyhow!("{method}: {e}"));
        }
        Ok(reply["result"].clone())
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        let _ = self.child.lock().unwrap().kill();
    }
}

/// The servers, one per language, started on first use, on the
/// workspace's host.
pub struct Lsp {
    ws: Arc<Workspace>,
    servers: Mutex<HashMap<String, Option<Arc<Server>>>>,
    installed: Mutex<HashMap<String, bool>>,
    on_notify: OnNotify,
}

impl Lsp {
    pub fn new(ws: Arc<Workspace>, on_notify: OnNotify) -> Self {
        Lsp {
            ws,
            servers: Mutex::default(),
            installed: Mutex::default(),
            on_notify,
        }
    }

    /// Whether `language`'s server is installed on the host (asked once).
    pub fn available(&self, language: &str) -> bool {
        let Some(cmd) = command_for(language) else {
            return false;
        };
        if let Some(&v) = self.installed.lock().unwrap().get(language) {
            return v;
        }
        let probe = format!("command -v {}", cmd[0]);
        let ok = self
            .ws
            .authority
            .command("sh", &["-c".into(), probe], &self.ws.root)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|s| s.success());
        self.installed
            .lock()
            .unwrap()
            .insert(language.to_string(), ok);
        ok
    }

    /// The server for `language`, starting it if it is installed.
    pub fn server(&self, language: &str) -> Option<Arc<Server>> {
        if !self.available(language) {
            return None;
        }
        let mut m = self.servers.lock().unwrap();
        if let Some(s) = m.get(language) {
            return s.clone();
        }
        let s = command_for(language)
            .and_then(|cmd| Server::start(language, &cmd, &self.ws, self.on_notify.clone()).ok());
        m.insert(language.to_string(), s.clone());
        s
    }

    /// The server for `language` only if it is already running.
    pub fn running(&self, language: &str) -> Option<Arc<Server>> {
        self.servers
            .lock()
            .unwrap()
            .get(language)
            .cloned()
            .flatten()
    }

    pub fn status(&self) -> Value {
        let m = self.servers.lock().unwrap();
        json!(m
            .iter()
            .map(|(k, v)| (k.clone(), json!(v.is_some())))
            .collect::<serde_json::Map<_, _>>())
    }
}
