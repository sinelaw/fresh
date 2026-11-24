//! Clients and the events pushed to them.
//!
//! A client gets an id from `hello` and then holds one Server-Sent Events
//! stream (`GET /events?client=ID`). Events queued before the stream opens
//! wait in its channel. A client whose stream drops is forgotten, and what it
//! had open is released.
//!
//! Each client views one workspace (or none, when there is none to view):
//! its workspace's events go to it through that workspace's [`Room`], and
//! the orchestrator's (workspaces opened, closed, their status) go to every
//! client.

use serde_json::Value;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc::{
    unbounded_channel as channel, UnboundedReceiver as Receiver, UnboundedSender as Sender,
};

struct Slot {
    tx: Sender<String>,
    rx: Option<Receiver<String>>,
    /// The workspace the client views.
    workspace: Option<u64>,
}

#[derive(Default)]
pub struct Hub {
    clients: Mutex<HashMap<u64, Slot>>,
    next: AtomicU64,
}

impl Hub {
    /// A new client viewing `workspace`.
    pub fn register(&self, workspace: Option<u64>) -> u64 {
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        let (tx, rx) = channel();
        self.clients.lock().unwrap().insert(
            id,
            Slot {
                tx,
                rx: Some(rx),
                workspace,
            },
        );
        id
    }

    /// The receiving end of `client`'s events, once: the stream takes it.
    pub fn take_stream(&self, client: u64) -> Option<Receiver<String>> {
        self.clients.lock().unwrap().get_mut(&client)?.rx.take()
    }

    /// The workspace `client` views: `None` for an unknown client,
    /// `Some(None)` for one viewing none.
    pub fn workspace_of(&self, client: u64) -> Option<Option<u64>> {
        self.clients
            .lock()
            .unwrap()
            .get(&client)
            .map(|s| s.workspace)
    }

    /// How many clients view `workspace`.
    pub fn viewers(&self, workspace: u64) -> usize {
        self.clients
            .lock()
            .unwrap()
            .values()
            .filter(|s| s.workspace == Some(workspace))
            .count()
    }

    pub fn forget(&self, client: u64) {
        self.clients.lock().unwrap().remove(&client);
    }

    pub fn send(&self, client: u64, ev: &Value) {
        if let Some(s) = self.clients.lock().unwrap().get(&client) {
            let _ = s.tx.send(ev.to_string());
        }
    }

    /// To every client (`workspace` `None`), or every one viewing
    /// `workspace`.
    fn broadcast_to(&self, workspace: Option<u64>, ev: &Value) {
        let msg = ev.to_string();
        for s in self.clients.lock().unwrap().values() {
            if workspace.is_none() || s.workspace == workspace {
                let _ = s.tx.send(msg.clone());
            }
        }
    }

    /// To every client, whatever it views.
    pub fn broadcast(&self, ev: &Value) {
        self.broadcast_to(None, ev);
    }
}

/// One workspace's clients: the hub, narrowed to the clients viewing it.
#[derive(Clone)]
pub struct Room {
    hub: Arc<Hub>,
    workspace: u64,
}

impl Room {
    pub fn new(hub: Arc<Hub>, workspace: u64) -> Self {
        Room { hub, workspace }
    }

    /// Whether `client` is registered and views this workspace.
    pub fn has(&self, client: u64) -> bool {
        self.hub.workspace_of(client) == Some(Some(self.workspace))
    }

    pub fn send(&self, client: u64, ev: &Value) {
        if self.has(client) {
            self.hub.send(client, ev);
        }
    }

    /// To every client viewing the workspace.
    pub fn broadcast(&self, ev: &Value) {
        self.hub.broadcast_to(Some(self.workspace), ev);
    }

    pub fn viewers(&self) -> usize {
        self.hub.viewers(self.workspace)
    }
}
