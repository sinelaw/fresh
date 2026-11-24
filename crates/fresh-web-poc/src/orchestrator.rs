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
//! `workspace.removed`, `workspace.status` — for the overview.

use crate::authority::{self, HostSpec};
use crate::error::{err, Code};
use crate::hub::Hub;
use crate::session::{s, u, Session};
use crate::workspace::{self, Workspace};
use anyhow::{anyhow, Result};
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

pub struct Orchestrator {
    pub hub: Arc<Hub>,
    sessions: Mutex<BTreeMap<u64, Arc<Session>>>,
    next: AtomicU64,
    /// Network-like delays on every workspace's filesystem.
    slow: bool,
}

/// What `workspace.open` asks for.
pub struct Open<'a> {
    pub host: HostSpec,
    pub root: &'a str,
    pub trusted: bool,
    /// A command to run in a terminal once it is open: an agent session.
    pub agent: Option<&'a str>,
}

impl Orchestrator {
    pub fn new(slow: bool) -> Self {
        Orchestrator {
            hub: Arc::default(),
            sessions: Mutex::default(),
            next: AtomicU64::new(0),
            slow,
        }
    }

    pub fn session(&self, id: u64) -> Option<Arc<Session>> {
        self.sessions.lock().unwrap().get(&id).cloned()
    }

    /// Every open workspace, in the order opened.
    pub fn list(&self) -> Vec<Value> {
        let sessions: Vec<Arc<Session>> = self.sessions.lock().unwrap().values().cloned().collect();
        sessions.iter().map(|s| s.entry()).collect()
    }

    /// Open a workspace: reach its host, find its root there (`~` is the
    /// home folder there; a relative path is resolved by the host's
    /// filesystem, from where the server, or the remote agent, runs), and
    /// start following it. A folder already open on the same host is that
    /// workspace (as it is: its trust is not changed). With an agent, a
    /// terminal running it is opened in the workspace, which must be
    /// trusted.
    pub fn open(&self, o: &Open) -> Result<(Arc<Session>, Option<u64>)> {
        if o.agent.is_some() && !o.trusted {
            fail!(
                Untrusted,
                "an agent runs in a trusted workspace: trust it to start one"
            );
        }
        let authority = authority::connect(&o.host, self.slow)
            .map_err(|e| err(Code::Failed, format!("cannot reach the host: {e}")))?;
        let fs = authority.fs();
        let given = match o.root.strip_prefix("~") {
            Some(rest) if rest.is_empty() || rest.starts_with('/') => {
                PathBuf::from(fs.home_dir()?).join(rest.trim_start_matches('/'))
            }
            _ => PathBuf::from(o.root),
        };
        let root: PathBuf = fs
            .canonicalize(workspace::host(&given).as_host_path())
            .map(PathBuf::from)
            .map_err(|e| err(Code::NotFound, format!("{}: {e}", given.display())))?;
        if !fs
            .is_dir(workspace::host(&root).as_host_path())
            .unwrap_or(false)
        {
            fail!(BadRequest, "{} is not a folder", root.display());
        }
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
                    let s = Session::new(id, ws, o.trusted, self.hub.clone());
                    sessions.insert(id, s.clone());
                    (s, true)
                }
            }
        };
        if added {
            self.hub
                .broadcast(&json!({"ev": "workspace.added", "workspace": session.entry()}));
        }
        let agent = match o.agent {
            Some(cmd) => Some(session.open_terminal((120, 30), None, Some(cmd))?),
            None => None,
        };
        Ok((session, agent))
    }

    /// Close a workspace: stop everything it runs on its host and forget
    /// its documents. Refused while it holds unsaved documents, unless
    /// `discard`. Clients viewing it are told (`workspace.removed`) and
    /// can do nothing more there.
    pub fn close(&self, id: u64, discard: bool) -> Result<()> {
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
        self.sessions.lock().unwrap().remove(&id);
        session.shutdown();
        self.hub
            .broadcast(&json!({"ev": "workspace.removed", "workspace": id}));
        Ok(())
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
                })
            }
            "workspace.list" => json!({ "workspaces": self.list() }),
            "workspace.open" => {
                self.known(client)?;
                let host: HostSpec = match m.get("host") {
                    Some(h) => serde_json::from_value(h.clone())
                        .map_err(|e| err(Code::BadRequest, format!("host: {e}")))?,
                    None => HostSpec::Local,
                };
                let (session, agent) = self.open(&Open {
                    host,
                    root: s(m, "root")?,
                    trusted: m.get("trusted").and_then(Value::as_bool).unwrap_or(false),
                    agent: m
                        .get("agent")
                        .and_then(Value::as_str)
                        .filter(|c| !c.trim().is_empty()),
                })?;
                json!({ "workspace": session.entry(), "agent": agent })
            }
            "workspace.close" => {
                self.known(client)?;
                let discard = m.get("discard").and_then(Value::as_bool).unwrap_or(false);
                self.close(u(m, "workspace")?, discard)?;
                json!({})
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

    /// The server is exiting: every workspace stops what it started.
    pub fn shutdown(&self) {
        let sessions: Vec<Arc<Session>> = std::mem::take(&mut *self.sessions.lock().unwrap())
            .into_values()
            .collect();
        for s in sessions {
            s.shutdown();
        }
    }
}

/// A container workspace's files are read here and its processes run in the
/// container, against the same paths: its root must be bind-mounted at its
/// own path there.
fn check_mounted(authority: &dyn authority::Authority, root: &Path) -> Result<()> {
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
        orch.open(&Open {
            host: HostSpec::Local,
            root: root.to_str().unwrap(),
            trusted,
            agent: None,
        })
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
    fn workspaces_are_isolated_from_each_other() {
        let (d1, d2) = (tempfile::tempdir().unwrap(), tempfile::tempdir().unwrap());
        std::fs::write(d1.path().join("a.txt"), "one\n").unwrap();
        std::fs::write(d2.path().join("a.txt"), "two\n").unwrap();
        let orch = Orchestrator::new(false);
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
        let orch = Orchestrator::new(false);
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
        let orch = Orchestrator::new(false);
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
        let orch = Orchestrator::new(false);
        let (w, agent) = orch
            .open(&Open {
                host: HostSpec::Local,
                root: d.path().to_str().unwrap(),
                trusted: true,
                agent: Some(
                    "echo working; sleep 1; printf 'Apply the change? [y/n] '; read a; echo got $a; printf '\\a'; sleep 30",
                ),
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
}
