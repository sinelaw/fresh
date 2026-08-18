//! `Ticker`, `Cache`, `Controller` and `Persisted`.

use std::cell::RefCell;
use std::rc::Rc;

use super::Behavior;

// -- Ticker ------------------------------------------------------------------

/// A callback once per frame, torn down with its element.
pub struct Ticker {
    #[allow(clippy::type_complexity)]
    f: RefCell<Option<Rc<dyn Fn()>>>,
    running: std::cell::Cell<bool>,
}

impl Ticker {
    pub fn new(f: impl Fn() + 'static) -> Self {
        Ticker {
            f: RefCell::new(Some(Rc::new(f))),
            running: std::cell::Cell::new(true),
        }
    }

    pub fn pause(&self) {
        self.running.set(false);
    }

    pub fn resume(&self) {
        self.running.set(true);
    }
}

impl Behavior for Ticker {
    fn frame(&self) {
        if !self.running.get() {
            return;
        }
        let f = self.f.borrow().clone();
        if let Some(f) = f {
            f();
        }
    }

    fn has_pending(&self) -> bool {
        // A running ticker wants a frame every frame; that is what it is for.
        self.running.get()
    }

    fn teardown(&self) {
        *self.f.borrow_mut() = None;
    }

    fn behavior_name(&self) -> &'static str {
        "Ticker"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

// -- Cache -------------------------------------------------------------------

/// Local memoization, and the one field a component may write during `build`.
///
/// The design document exempts `Cache` from a purity check on the other fields.
/// Here that check is the type system: `build` receives `&Self::State`, so no
/// other field is writable during a build at all. What `Cache` adds is the
/// sanctioned interior mutability, under the contract the document states —
/// **writes are idempotent functions of the build inputs**, so the framework
/// may call `build` any number of times without committing the result and the
/// component cannot tell.
pub struct Cache<K, V> {
    slot: RefCell<Option<(K, V)>>,
}

impl<K, V> Default for Cache<K, V> {
    fn default() -> Self {
        Cache {
            slot: RefCell::new(None),
        }
    }
}

impl<K: PartialEq + Clone, V: Clone> Cache<K, V> {
    pub fn new() -> Self {
        Cache::default()
    }

    /// The value for these inputs, computing it only when the inputs changed.
    pub fn get_or(&self, key: K, f: impl FnOnce() -> V) -> V {
        if let Some((k, v)) = self.slot.borrow().as_ref() {
            if *k == key {
                return v.clone();
            }
        }
        let v = f();
        *self.slot.borrow_mut() = Some((key, v.clone()));
        v
    }

    pub fn clear(&self) {
        *self.slot.borrow_mut() = None;
    }
}

impl<K: 'static, V: 'static> Behavior for Cache<K, V> {
    fn teardown(&self) {
        *self.slot.borrow_mut() = None;
    }

    fn behavior_name(&self) -> &'static str {
        "Cache"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

// -- Controller --------------------------------------------------------------

/// An imperative command path from an owner to a child.
///
/// The owner constructs it, passes it down in a description, and the child
/// binds it during construction. Scope is restricted: **a command may touch
/// only the target's local state.** A command that changes controlled state
/// violates the data-flow rule, and the binding is the place to enforce that —
/// the sink a child installs closes over its own `Updater` and nothing else.
///
/// Not an id side table: binding is explicit and the state stays on the
/// element.
pub struct Controller<C: 'static> {
    #[allow(clippy::type_complexity)]
    sink: RefCell<Option<Rc<dyn Fn(C)>>>,
    pending: RefCell<Vec<C>>,
}

impl<C: 'static> Default for Controller<C> {
    fn default() -> Self {
        Controller {
            sink: RefCell::new(None),
            pending: RefCell::new(Vec::new()),
        }
    }
}

impl<C: 'static> Controller<C> {
    pub fn new() -> Rc<Controller<C>> {
        Rc::new(Controller::default())
    }

    /// Called by the child, once, during construction.
    pub fn bind(&self, f: impl Fn(C) + 'static) {
        *self.sink.borrow_mut() = Some(Rc::new(f));
        let queued = std::mem::take(&mut *self.pending.borrow_mut());
        let sink = self.sink.borrow().clone();
        if let Some(sink) = sink {
            for c in queued {
                sink(c);
            }
        }
    }

    pub fn is_bound(&self) -> bool {
        self.sink.borrow().is_some()
    }

    /// Send a command. It reaches the target's state through that target's own
    /// `set_state`, so it lands between frames like any other update.
    pub fn send(&self, c: C) {
        let sink = self.sink.borrow().clone();
        match sink {
            Some(f) => f(c),
            // Not mounted yet: hold it until the child binds.
            None => self.pending.borrow_mut().push(c),
        }
    }
}

impl<C: 'static> Behavior for Controller<C> {
    fn teardown(&self) {
        *self.sink.borrow_mut() = None;
    }

    fn behavior_name(&self) -> &'static str {
        "Controller"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

// -- Persisted ---------------------------------------------------------------

/// Where persisted values are read and written. The library never interprets
/// them; the host decides what a scope means and where the bytes live.
pub trait Store {
    fn get(&self, key: &str) -> Option<Rc<dyn std::any::Any>>;
    fn set(&self, key: &str, value: Rc<dyn std::any::Any>);
}

/// A value read from the host store at construction and written back at
/// teardown.
///
/// Unmount still destroys the state object; the value is restored at the next
/// construction. Keys are anchored to the enclosing `PersistenceScope` rather
/// than to tree position, so moving a widget does not lose its value and two
/// widgets at the same position under different documents do not share one.
///
/// Scope: **new incidental state only.** Existing serialized view state stays
/// application state.
pub struct Persisted<T: Clone + 'static> {
    key: String,
    value: RefCell<T>,
    store: RefCell<Option<Rc<dyn Store>>>,
}

impl<T: Clone + 'static> Persisted<T> {
    /// `scope` is the value of the enclosing `PersistenceScope` ambient, read
    /// by the caller — the framework does not guess it.
    pub fn new(scope: &str, key: &str, default: T) -> Self {
        Persisted {
            key: format!("{scope}/{key}"),
            value: RefCell::new(default),
            store: RefCell::new(None),
        }
    }

    pub fn get(&self) -> T {
        self.value.borrow().clone()
    }

    pub fn set(&self, v: T) {
        *self.value.borrow_mut() = v;
    }
}

impl<T: Clone + 'static> Behavior for Persisted<T> {
    fn attach(&self, services: &crate::services::Services) {
        let store = services.store.clone();
        if let Some(s) = &store {
            if let Some(v) = s.get(&self.key) {
                if let Ok(v) = v.downcast::<T>() {
                    *self.value.borrow_mut() = (*v).clone();
                }
            }
        }
        *self.store.borrow_mut() = store;
    }

    fn teardown(&self) {
        let store = self.store.borrow().clone();
        if let Some(s) = store {
            s.set(&self.key, Rc::new(self.value.borrow().clone()));
        }
    }

    fn behavior_name(&self) -> &'static str {
        "Persisted"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// The ambient that scopes persisted keys to a document or a route rather than
/// to a position in the tree.
pub static PERSISTENCE_SCOPE: crate::ambient::Ambient<String> =
    crate::ambient::Ambient::new("persistence-scope");
