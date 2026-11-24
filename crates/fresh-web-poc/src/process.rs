//! Processes run on the workspace's host for a client (a plugin's
//! `runOnTarget`, `spawnProcess`), and paths a client watches.

use crate::hub::Hub;
use crate::workspace::Workspace;
use anyhow::Result;
use serde_json::json;
use std::collections::HashMap;
use std::io::Read;
use std::path::PathBuf;
use std::process::{Child, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

#[derive(Default)]
pub struct Procs {
    running: Arc<Mutex<HashMap<u64, Child>>>,
    next: AtomicU64,
}

impl Procs {
    /// Start `program`; its result is sent to `client` as `proc.exit`.
    pub fn run(
        &self,
        hub: Arc<Hub>,
        ws: &Workspace,
        client: u64,
        program: &str,
        args: &[String],
        cwd: Option<&str>,
    ) -> Result<u64> {
        let cwd = cwd
            .map(|c| ws.resolve(c))
            .unwrap_or_else(|| ws.root.clone());
        let mut cmd = ws.authority.command(program, args, &cwd);
        cmd.stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => {
                hub.send(
                    client,
                    &json!({"ev": "proc.exit", "proc": id, "code": -1, "stdout": "", "stderr": e.to_string()}),
                );
                return Ok(id);
            }
        };
        let mut out = child.stdout.take();
        let mut err = child.stderr.take();
        let out_t = std::thread::spawn(move || {
            let mut s = Vec::new();
            if let Some(o) = out.as_mut() {
                let _ = o.read_to_end(&mut s);
            }
            String::from_utf8_lossy(&s).into_owned()
        });
        let err_t = std::thread::spawn(move || {
            let mut s = Vec::new();
            if let Some(o) = err.as_mut() {
                let _ = o.read_to_end(&mut s);
            }
            String::from_utf8_lossy(&s).into_owned()
        });
        self.running.lock().unwrap().insert(id, child);
        let running = self.running.clone();
        std::thread::spawn(move || {
            let code = loop {
                let status = {
                    let mut m = running.lock().unwrap();
                    match m.get_mut(&id) {
                        Some(c) => c.try_wait().ok().flatten(),
                        None => break -1,
                    }
                };
                if let Some(s) = status {
                    break s.code().unwrap_or(-1);
                }
                std::thread::sleep(Duration::from_millis(10));
            };
            running.lock().unwrap().remove(&id);
            let stdout = out_t.join().unwrap_or_default();
            let stderr = err_t.join().unwrap_or_default();
            hub.send(
                client,
                &json!({"ev": "proc.exit", "proc": id, "code": code, "stdout": stdout, "stderr": stderr}),
            );
        });
        Ok(id)
    }

    pub fn kill(&self, id: u64) -> bool {
        match self.running.lock().unwrap().get_mut(&id) {
            Some(c) => c.kill().is_ok(),
            None => false,
        }
    }
}

struct Watch {
    client: u64,
    path: PathBuf,
    last: Option<(u64, u64)>,
}

/// Paths watched by polling their size and modification time.
#[derive(Default)]
pub struct Watches {
    map: Arc<Mutex<HashMap<u64, Watch>>>,
    next: AtomicU64,
}

impl Watches {
    /// Poll every watched path's (size, mtime) through the workspace's
    /// filesystem, so a remote host's paths are watched the same way.
    pub fn start(&self, hub: Arc<Hub>, ws: Arc<Workspace>) {
        let map = self.map.clone();
        std::thread::spawn(move || loop {
            std::thread::sleep(Duration::from_millis(500));
            let paths: Vec<(u64, PathBuf)> = map
                .lock()
                .unwrap()
                .iter()
                .map(|(id, w)| (*id, w.path.clone()))
                .collect();
            let sigs: Vec<(u64, Option<(u64, u64)>)> =
                paths.iter().map(|(id, p)| (*id, ws.signature(p))).collect();
            let mut m = map.lock().unwrap();
            for (id, now) in sigs {
                let Some(w) = m.get_mut(&id) else { continue };
                if now != w.last {
                    w.last = now;
                    hub.send(
                        w.client,
                        &json!({"ev": "fs.changed", "watch": id, "path": w.path.to_string_lossy()}),
                    );
                }
            }
        });
    }

    pub fn add(&self, ws: &Workspace, client: u64, path: PathBuf) -> u64 {
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        let last = ws.signature(&path);
        self.map
            .lock()
            .unwrap()
            .insert(id, Watch { client, path, last });
        id
    }

    pub fn remove(&self, id: u64) {
        self.map.lock().unwrap().remove(&id);
    }

    pub fn drop_client(&self, client: u64) {
        self.map.lock().unwrap().retain(|_, w| w.client != client);
    }
}
