//! `Tasks`: async ownership, without the library choosing a runtime.
//!
//! The application starts the work — a thread, an executor, a channel from
//! somewhere else. What this provides is the two guarantees that remove the
//! need for an `is_mounted` flag:
//!
//! - results are delivered **on the UI scheduler between frames**, never
//!   concurrently with build, layout or paint;
//! - delivery **does not occur after teardown**.
//!
//! `launch_replacing(tag, ..)` additionally cancels any prior launch under the
//! same tag: an older result is dropped rather than racing the newer one. Every
//! other race is the caller's to resolve, and is resolved at delivery time.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;

use super::Behavior;

/// The half that crosses to whatever is doing the work. `Send`, so it can be
/// moved into a thread or a future.
pub struct TaskHandle<T: Send + 'static> {
    tx: Sender<(u64, T)>,
    generation: u64,
    alive: Arc<AtomicBool>,
    current: Arc<AtomicU64>,
}

impl<T: Send + 'static> TaskHandle<T> {
    /// Hand back a result. Returns whether it will be delivered: `false` once
    /// the owning element has been torn down, or once a newer launch under the
    /// same tag has replaced this one.
    pub fn deliver(&self, value: T) -> bool {
        if !self.alive.load(Ordering::Acquire) {
            return false;
        }
        if self.current.load(Ordering::Acquire) != self.generation {
            return false;
        }
        self.tx.send((self.generation, value)).is_ok()
    }

    /// Whether this launch is still the current one for its tag and its owner
    /// is still mounted.
    pub fn is_live(&self) -> bool {
        self.alive.load(Ordering::Acquire)
            && self.current.load(Ordering::Acquire) == self.generation
    }
}

struct Inner<T: Send + 'static> {
    tx: Sender<(u64, T)>,
    rx: Receiver<(u64, T)>,
    alive: Arc<AtomicBool>,
    tags: RefCell<HashMap<&'static str, Arc<AtomicU64>>>,
    next: Cell<u64>,
    #[allow(clippy::type_complexity)]
    on_result: RefCell<Option<Rc<dyn Fn(T)>>>,
    services: RefCell<Option<crate::services::Services>>,
}

/// Register one of these per component that starts asynchronous work.
pub struct Tasks<T: Send + 'static> {
    inner: Inner<T>,
}

impl<T: Send + 'static> Default for Tasks<T> {
    fn default() -> Self {
        Tasks::new()
    }
}

impl<T: Send + 'static> Tasks<T> {
    pub fn new() -> Self {
        let (tx, rx) = channel();
        Tasks {
            inner: Inner {
                tx,
                rx,
                alive: Arc::new(AtomicBool::new(true)),
                tags: RefCell::new(HashMap::new()),
                next: Cell::new(1),
                on_result: RefCell::new(None),
                services: RefCell::new(None),
            },
        }
    }

    /// What to do with each result. Called on the UI thread, between frames.
    pub fn on_result(&self, f: impl Fn(T) + 'static) {
        *self.inner.on_result.borrow_mut() = Some(Rc::new(f));
    }

    /// Start work under `tag`, superseding any earlier launch with the same
    /// tag: the older result is dropped rather than racing the newer one.
    ///
    /// The library does not name a runtime. `work` runs wherever the host's
    /// spawner puts it, and hands its results back through the handle it is
    /// given. Delivery still happens on the UI scheduler between frames, and
    /// still stops once the owning element is torn down.
    pub fn launch_replacing<F>(&self, tag: &'static str, work: F)
    where
        F: FnOnce(TaskHandle<T>) + Send + 'static,
    {
        let handle = self.handle(tag);
        let spawn = self.inner.services.borrow().clone();
        match spawn {
            Some(s) => s.spawn(Box::new(move || work(handle))),
            // Not registered, so there is nowhere to put the work: run it here
            // rather than drop it.
            None => work(handle),
        }
    }

    /// A launch whose work the caller drives itself — a channel it already
    /// owns, an executor the library never sees. Same supersession rule.
    pub fn handle(&self, tag: &'static str) -> TaskHandle<T> {
        let generation = self.inner.next.get();
        self.inner.next.set(generation + 1);
        let mut tags = self.inner.tags.borrow_mut();
        let current = tags
            .entry(tag)
            .or_insert_with(|| Arc::new(AtomicU64::new(0)))
            .clone();
        current.store(generation, Ordering::Release);
        TaskHandle {
            tx: self.inner.tx.clone(),
            generation,
            alive: self.inner.alive.clone(),
            current,
        }
    }

    /// Deliver everything that has arrived. Called by the scheduler; returns
    /// how many results were handed over.
    pub fn drain(&self) -> usize {
        if !self.inner.alive.load(Ordering::Acquire) {
            return 0;
        }
        let handler = self.inner.on_result.borrow().clone();
        let mut n = 0;
        while let Ok((generation, value)) = self.inner.rx.try_recv() {
            let superseded = self
                .inner
                .tags
                .borrow()
                .values()
                .all(|c| c.load(Ordering::Acquire) != generation);
            if superseded {
                continue;
            }
            if let Some(h) = &handler {
                h(value);
                n += 1;
            }
        }
        n
    }
}

impl<T: Send + 'static> Behavior for Tasks<T> {
    fn attach(&self, services: &crate::services::Services) {
        *self.inner.services.borrow_mut() = Some(services.clone());
    }

    fn teardown(&self) {
        self.inner.alive.store(false, Ordering::Release);
        *self.inner.on_result.borrow_mut() = None;
    }

    fn behavior_name(&self) -> &'static str {
        "Tasks"
    }

    fn pump(&self) -> usize {
        self.drain()
    }
}
