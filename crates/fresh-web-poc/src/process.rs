//! Processes run on the workspace's host for a client (a plugin's
//! `runOnTarget`, `spawnProcess`), and paths a client watches.

use crate::authority::{Authority, HostProc};
use crate::hub::Room;
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

/// A running process: the local child (the program here, or the `ssh` or
/// `docker` client reaching it) and the process on the host.
struct Running {
    client: u64,
    child: Child,
    authority: Arc<dyn Authority>,
    host: HostProc,
    stopped: bool,
}

type Table = Arc<Mutex<HashMap<u64, Running>>>;

#[derive(Default)]
pub struct Procs {
    running: Table,
    next: AtomicU64,
}

/// Stop process `id` on its host, then its local child. The local child
/// lives as long as the host process (or the link to it), so only one still
/// running is stopped. Returns whether it was running.
fn stop(running: &Table, id: u64) -> bool {
    let target = {
        let mut m = running.lock().unwrap();
        m.get_mut(&id).and_then(|r| {
            let alive = matches!(r.child.try_wait(), Ok(None));
            r.stopped |= alive;
            alive.then(|| (r.authority.clone(), r.host.clone()))
        })
    };
    let Some((authority, host)) = target else {
        return false;
    };
    authority.stop(&host);
    if let Some(r) = running.lock().unwrap().get_mut(&id) {
        let _ = r.child.kill();
    }
    true
}

impl Procs {
    /// Start `program`; its result is sent to `client` as `proc.exit`.
    pub fn run(
        &self,
        room: Room,
        ws: &Workspace,
        client: u64,
        program: &str,
        args: &[String],
        cwd: Option<&str>,
    ) -> Result<u64> {
        let cwd = cwd
            .map(|c| ws.resolve(c))
            .unwrap_or_else(|| ws.root.clone());
        let (mut cmd, host) = ws.authority.command(program, args, &cwd);
        cmd.stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        // A program that can't be started fails the request itself.
        let mut child = cmd.spawn().map_err(|e| anyhow::anyhow!("{program}: {e}"))?;
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
        self.running.lock().unwrap().insert(
            id,
            Running {
                client,
                child,
                authority: ws.authority.clone(),
                host,
                stopped: false,
            },
        );
        let running = self.running.clone();
        std::thread::spawn(move || {
            // Its exit code; none when a signal ended it, or it was stopped.
            let code: Option<i32> = loop {
                let status = {
                    let mut m = running.lock().unwrap();
                    match m.get_mut(&id) {
                        Some(r) => r.child.try_wait().ok().flatten(),
                        None => break None,
                    }
                };
                if let Some(s) = status {
                    break s.code();
                }
                std::thread::sleep(Duration::from_millis(10));
            };
            let ended = running.lock().unwrap().remove(&id);
            // A stop that raced the process's own end leaves its marker.
            if let Some(r) = ended.filter(|r| r.stopped) {
                r.authority.release(&r.host);
            }
            let stdout = out_t.join().unwrap_or_default();
            let stderr = err_t.join().unwrap_or_default();
            room.send(
                client,
                &json!({"ev": "proc.exit", "proc": id, "code": code, "stdout": stdout, "stderr": stderr}),
            );
        });
        Ok(id)
    }

    /// Kill process `id` and its process group on the host. Returns whether
    /// it was running.
    pub fn kill(&self, id: u64) -> bool {
        stop(&self.running, id)
    }

    /// A client disconnected: nobody is left to report its processes to,
    /// so they are killed (and reaped by their waiting threads), in the
    /// background, since a stop may take a round trip to the host.
    pub fn drop_client(&self, client: u64) {
        let ids: Vec<u64> = self
            .running
            .lock()
            .unwrap()
            .iter()
            .filter(|(_, r)| r.client == client)
            .map(|(id, _)| *id)
            .collect();
        let running = self.running.clone();
        std::thread::spawn(move || {
            for id in ids {
                stop(&running, id);
            }
        });
    }

    /// Kill every process, and wait for it.
    pub fn stop_all(&self) {
        let ids: Vec<u64> = self.running.lock().unwrap().keys().copied().collect();
        for id in ids {
            stop(&self.running, id);
        }
    }

    /// How many processes are running.
    pub fn count(&self) -> usize {
        self.running.lock().unwrap().len()
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
    /// Stops when the watches go (their workspace was closed).
    pub fn start(&self, room: Room, ws: Arc<Workspace>) {
        let weak = Arc::downgrade(&self.map);
        std::thread::spawn(move || loop {
            std::thread::sleep(Duration::from_millis(500));
            let Some(map) = weak.upgrade() else { return };
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
                    room.send(
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

    /// How many paths are watched.
    #[cfg(test)]
    pub fn count(&self) -> usize {
        self.map.lock().unwrap().len()
    }
}
