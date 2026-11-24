//! Clients and the events pushed to them.
//!
//! A client gets an id from `hello` and then holds one Server-Sent Events
//! stream (`GET /events?client=ID`). Events queued before the stream opens
//! wait in its channel. A client whose stream drops is forgotten, and what it
//! had open is released.

use serde_json::Value;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Mutex;

struct Slot {
    tx: Sender<String>,
    rx: Option<Receiver<String>>,
}

#[derive(Default)]
pub struct Hub {
    clients: Mutex<HashMap<u64, Slot>>,
    next: AtomicU64,
}

impl Hub {
    pub fn register(&self) -> u64 {
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        let (tx, rx) = channel();
        self.clients
            .lock()
            .unwrap()
            .insert(id, Slot { tx, rx: Some(rx) });
        id
    }

    /// The receiving end of `client`'s events, once: the stream takes it.
    pub fn take_stream(&self, client: u64) -> Option<Receiver<String>> {
        self.clients.lock().unwrap().get_mut(&client)?.rx.take()
    }

    pub fn forget(&self, client: u64) {
        self.clients.lock().unwrap().remove(&client);
    }

    pub fn send(&self, client: u64, ev: &Value) {
        if let Some(s) = self.clients.lock().unwrap().get(&client) {
            let _ = s.tx.send(ev.to_string());
        }
    }

    /// To every client, or every one but `except`.
    pub fn broadcast(&self, ev: &Value, except: Option<u64>) {
        let msg = ev.to_string();
        for (&id, s) in self.clients.lock().unwrap().iter() {
            if Some(id) != except {
                let _ = s.tx.send(msg.clone());
            }
        }
    }

    pub fn ids(&self) -> Vec<u64> {
        self.clients.lock().unwrap().keys().copied().collect()
    }
}
