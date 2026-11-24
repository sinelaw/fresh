//! Terminals: a PTY on the workspace's host, and its raw output.
//!
//! The server does not emulate a terminal. It spawns the host's shell on a
//! PTY (through the workspace's [`Authority`](crate::authority::Authority), so
//! the same code serves a local shell, an SSH session or a devcontainer) and
//! streams the bytes. Each client runs its own terminal emulator (xterm.js)
//! on them. A replay buffer of recent output lets a client that attaches
//! late, or reconnects, rebuild the screen: every chunk carries its offset in
//! the stream, and the replay says where it ends, so nothing is shown twice.

use crate::authority::{Authority, HostProc};
use crate::hub::Hub;
use crate::workspace::Workspace;
use anyhow::{anyhow, Result};
use base64::Engine;
use portable_pty::{native_pty_system, Child, MasterPty, PtySize};
use serde_json::{json, Value};
use std::collections::{HashMap, VecDeque};
use std::io::{Read, Write};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

/// How much recent output is kept for replay.
const REPLAY_BYTES: usize = 1 << 20;

struct Output {
    /// The last [`REPLAY_BYTES`] bytes, ending at stream offset `end`.
    replay: VecDeque<u8>,
    end: u64,
    exited: bool,
}

struct Term {
    out: Arc<Mutex<Output>>,
    writer: Box<dyn Write + Send>,
    master: Box<dyn MasterPty + Send>,
    child: Box<dyn Child + Send + Sync>,
    size: (u16, u16),
    authority: Arc<dyn Authority>,
    /// The shell on the host.
    host: HostProc,
}

#[derive(Default)]
pub struct Terms {
    map: Mutex<HashMap<u64, Term>>,
    next: AtomicU64,
}

fn b64(bytes: &[u8]) -> String {
    base64::engine::general_purpose::STANDARD.encode(bytes)
}

impl Terms {
    pub fn open(&self, hub: Arc<Hub>, ws: &Workspace, cols: u16, rows: u16) -> Result<u64> {
        let pair = native_pty_system()
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .map_err(|e| anyhow!("{e}"))?;
        let (command, host) = ws.authority.shell_command(&ws.root);
        let child = pair
            .slave
            .spawn_command(command)
            .map_err(|e| anyhow!("{e}"))?;
        drop(pair.slave);
        let mut reader = pair.master.try_clone_reader().map_err(|e| anyhow!("{e}"))?;
        let writer = pair.master.take_writer().map_err(|e| anyhow!("{e}"))?;
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        let out = Arc::new(Mutex::new(Output {
            replay: VecDeque::new(),
            end: 0,
            exited: false,
        }));
        {
            let out = out.clone();
            let (authority, host) = (ws.authority.clone(), host.clone());
            std::thread::spawn(move || {
                let mut buf = [0u8; 32768];
                loop {
                    match reader.read(&mut buf) {
                        Ok(0) | Err(_) => break,
                        Ok(n) => {
                            // Recorded and sent under one lock, so the offsets
                            // the clients see are in order.
                            let mut o = out.lock().unwrap();
                            let at = o.end;
                            o.replay.extend(&buf[..n]);
                            let over = o.replay.len().saturating_sub(REPLAY_BYTES);
                            o.replay.drain(..over);
                            o.end += n as u64;
                            hub.broadcast(
                                &json!({"ev": "term.data", "term": id, "at": at, "data": b64(&buf[..n])}),
                                None,
                            );
                        }
                    }
                }
                out.lock().unwrap().exited = true;
                hub.broadcast(&json!({"ev": "term.exit", "term": id}), None);
                // The shell's pid file outlives it (the shell replaced the
                // script that would have removed it).
                authority.release(&host);
            });
        }
        self.map.lock().unwrap().insert(
            id,
            Term {
                out,
                writer,
                master: pair.master,
                child,
                size: (cols, rows),
                authority: ws.authority.clone(),
                host,
            },
        );
        Ok(id)
    }

    /// The replay buffer, and the stream offset it ends at.
    pub fn attach(&self, id: u64) -> Result<Value> {
        let m = self.map.lock().unwrap();
        let t = m.get(&id).ok_or_else(|| {
            crate::error::err(crate::error::Code::NotFound, format!("no terminal {id}"))
        })?;
        let o = t.out.lock().unwrap();
        let (a, b) = o.replay.as_slices();
        let mut bytes = a.to_vec();
        bytes.extend_from_slice(b);
        Ok(json!({
            "data": b64(&bytes), "end": o.end, "exited": o.exited,
            "cols": t.size.0, "rows": t.size.1,
        }))
    }

    pub fn input(&self, id: u64, data: &str) -> Result<()> {
        let mut m = self.map.lock().unwrap();
        let t = m.get_mut(&id).ok_or_else(|| {
            crate::error::err(crate::error::Code::NotFound, format!("no terminal {id}"))
        })?;
        t.writer.write_all(data.as_bytes())?;
        t.writer.flush()?;
        Ok(())
    }

    /// Size the PTY. With several clients attached, the last to resize wins,
    /// as with a shared tmux window.
    pub fn resize(&self, hub: &Hub, id: u64, cols: u16, rows: u16) -> Result<()> {
        let mut m = self.map.lock().unwrap();
        let t = m.get_mut(&id).ok_or_else(|| {
            crate::error::err(crate::error::Code::NotFound, format!("no terminal {id}"))
        })?;
        if t.size != (cols, rows) {
            t.master
                .resize(PtySize {
                    rows,
                    cols,
                    pixel_width: 0,
                    pixel_height: 0,
                })
                .map_err(|e| anyhow!("{e}"))?;
            t.size = (cols, rows);
            hub.broadcast(
                &json!({"ev": "term.size", "term": id, "cols": cols, "rows": rows}),
                None,
            );
        }
        Ok(())
    }

    pub fn close(&self, id: u64) {
        let term = self.map.lock().unwrap().remove(&id);
        if let Some(mut t) = term {
            // A hang-up to the shell on the host (it passes it on to its
            // jobs), then the local end. The local child lives as long as
            // the shell (or the link to it): only a running one is stopped.
            let running = matches!(t.child.try_wait(), Ok(None));
            if running {
                t.authority.stop(&t.host);
            }
            let _ = t.child.kill();
            let _ = t.child.wait();
            if running {
                t.authority.release(&t.host);
            }
        }
    }

    pub fn close_all(&self) {
        for id in self.list() {
            self.close(id);
        }
    }

    pub fn list(&self) -> Vec<u64> {
        let mut v: Vec<u64> = self.map.lock().unwrap().keys().copied().collect();
        v.sort();
        v
    }
}
