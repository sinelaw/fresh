//! Scheduling: the dirty set, the flush, and the invariants around them.
//!
//! Updates mark; they do not propagate. `set_state` records a mutation and adds
//! the element to a dirty set, and nothing else happens synchronously. All
//! mounting, unmounting and reordering happens inside [`Ui::flush`] — the tree
//! is structurally frozen between flushes.

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::panic::AssertUnwindSafe;
use std::rc::Rc;

use crate::desc::{Event, Handler, Node};
use crate::element::{Arena, ElementId, Txn};

// ---------------------------------------------------------------------------
// Renderer
// ---------------------------------------------------------------------------

/// The seam between the element tree and whatever owns geometry.
///
/// At this layer it reports element lifecycle only. The layout phase replaces
/// the calls for primitives with real render objects; the shape of the seam is
/// what later phases build on, and what the test recorder observes.
pub trait Renderer {
    fn create(&mut self, id: ElementId, ty: crate::desc::ElemType, name: &'static str) {
        let _ = (id, ty, name);
    }
    fn update(&mut self, id: ElementId, ty: crate::desc::ElemType, name: &'static str) {
        let _ = (id, ty, name);
    }
    fn dispose(&mut self, id: ElementId, ty: crate::desc::ElemType, name: &'static str) {
        let _ = (id, ty, name);
    }
}

/// Discards everything. The default.
pub struct NullRenderer;
impl Renderer for NullRenderer {}

// ---------------------------------------------------------------------------
// Scheduler state
// ---------------------------------------------------------------------------

type Mutation = Box<dyn FnOnce(&mut dyn Any)>;

/// Shared between the tree and every handler that can mark it dirty.
pub struct Sched {
    building: Option<(ElementId, &'static str)>,
    dirty: HashSet<ElementId>,
    mutations: Vec<(ElementId, Mutation)>,
}

pub(crate) type SchedRef = Rc<RefCell<Sched>>;

impl Sched {
    fn new() -> Self {
        Sched {
            building: None,
            dirty: HashSet::new(),
            mutations: Vec::new(),
        }
    }

    pub(crate) fn enter_build(&mut self, id: ElementId, name: &'static str) {
        self.building = Some((id, name));
    }

    pub(crate) fn clear_building(&mut self) {
        self.building = None;
    }

    pub(crate) fn forget(&mut self, id: ElementId) {
        self.dirty.remove(&id);
        self.mutations.retain(|(i, _)| *i != id);
    }

    fn guard_not_building(&self, what: &str) {
        if let Some((id, name)) = self.building {
            panic!("{what} during build at {name} ({id:?})");
        }
    }

    fn mark(&mut self, id: ElementId) {
        self.dirty.insert(id);
    }
}

/// A capturable handle that mutates one element's state.
///
/// Handlers hold this; calling it during a build is an error, because the build
/// that would read the change has already read the old value.
pub struct Updater<S: 'static> {
    sched: SchedRef,
    id: ElementId,
    _p: PhantomData<fn(&mut S)>,
}

impl<S: 'static> Clone for Updater<S> {
    fn clone(&self) -> Self {
        Updater {
            sched: self.sched.clone(),
            id: self.id,
            _p: PhantomData,
        }
    }
}

impl<S: 'static> Updater<S> {
    pub fn id(&self) -> ElementId {
        self.id
    }

    /// Queue a mutation and mark the element dirty. Applied at the start of the
    /// next flush, in the order queued.
    pub fn set(&self, f: impl FnOnce(&mut S) + 'static) {
        let mut s = self.sched.borrow_mut();
        s.guard_not_building("set_state");
        let id = self.id;
        s.mutations.push((
            id,
            Box::new(move |any: &mut dyn Any| {
                let st = any
                    .downcast_mut::<S>()
                    .expect("set_state state type does not match the component");
                f(st);
            }),
        ));
        s.mark(id);
    }
}

// ---------------------------------------------------------------------------
// BuildCx
// ---------------------------------------------------------------------------

/// What a component sees while building.
pub struct BuildCx<'a, M> {
    id: ElementId,
    sched: SchedRef,
    children: &'a [Node<M>],
}

impl<'a, M: 'static> BuildCx<'a, M> {
    pub(crate) fn new(id: ElementId, sched: SchedRef, children: &'a [Node<M>]) -> Self {
        BuildCx {
            id,
            sched,
            children,
        }
    }

    pub fn id(&self) -> ElementId {
        self.id
    }

    /// The children the parent passed to this component as props.
    pub fn children(&self) -> &'a [Node<M>] {
        self.children
    }

    /// A handle to this element's state, for capture by handlers.
    pub fn updater<S: 'static>(&self) -> Updater<S> {
        Updater {
            sched: self.sched.clone(),
            id: self.id,
            _p: PhantomData,
        }
    }

    /// A handler that mutates this element's state and marks it dirty.
    pub fn set_state<S: 'static>(&self, f: impl Fn(&mut S) + 'static) -> Handler<M> {
        let up = self.updater::<S>();
        let f = Rc::new(f);
        Rc::new(move |_ev: &Event| {
            let f = f.clone();
            up.set(move |s| f(s));
            None
        })
    }

    /// A handler that produces a message for the application.
    pub fn emit(&self, f: impl Fn(&Event) -> M + 'static) -> Handler<M> {
        Rc::new(move |ev| Some(f(ev)))
    }

    /// A handler that may or may not produce a message.
    pub fn handler(&self, f: impl Fn(&Event) -> Option<M> + 'static) -> Handler<M> {
        Rc::new(f)
    }
}

// ---------------------------------------------------------------------------
// Ui
// ---------------------------------------------------------------------------

/// The mounted tree.
pub struct Ui<M> {
    pub(crate) arena: Arena<M>,
    pub(crate) root: Option<ElementId>,
    pub(crate) sched: SchedRef,
    pub(crate) renderer: Box<dyn Renderer>,
    pub(crate) pending_dispose: Vec<ElementId>,
    pub(crate) txn: Option<Txn<M>>,
    pub(crate) trace: bool,
    pub(crate) build_log: Vec<ElementId>,
}

impl<M: 'static> Default for Ui<M> {
    fn default() -> Self {
        Ui::new()
    }
}

impl<M: 'static> Ui<M> {
    pub fn new() -> Self {
        Ui::with_renderer(Box::new(NullRenderer))
    }

    pub fn with_renderer(renderer: Box<dyn Renderer>) -> Self {
        Ui {
            arena: Arena::default(),
            root: None,
            sched: Rc::new(RefCell::new(Sched::new())),
            renderer,
            pending_dispose: Vec::new(),
            txn: None,
            trace: false,
            build_log: Vec::new(),
        }
    }

    /// Hand over a freshly built root description and reconcile it.
    pub fn frame(&mut self, root: Node<M>) {
        self.run_flush(Some(root));
    }

    /// Rebuild whatever is dirty, without a new root description.
    pub fn flush(&mut self) {
        self.run_flush(None);
    }

    fn run_flush(&mut self, root: Option<Node<M>>) {
        self.sched.borrow().guard_not_building("flush");

        // 1. Apply queued state mutations, then mark their elements for build.
        //    A mutation aimed at an element that has since been disposed is
        //    dropped: the mark is silently lost, which is what makes deferred
        //    disposal safe for handlers.
        let mutations = std::mem::take(&mut self.sched.borrow_mut().mutations);
        for (id, f) in mutations {
            let Some(el) = self.arena.get_mut(id) else {
                continue;
            };
            if el.disposed {
                continue;
            }
            if let Some(st) = el.state.as_mut() {
                f(&mut **st);
            }
        }
        let marked: Vec<ElementId> = self.sched.borrow().dirty.iter().copied().collect();
        for id in marked {
            let disposed = self.arena.get(id).map(|e| e.disposed).unwrap_or(true);
            if disposed {
                self.sched.borrow_mut().forget(id);
            } else {
                self.set_needs_build_untracked(id, true);
            }
        }

        // 2. Reconcile the root description, if one was supplied.
        if let Some(node) = root {
            self.in_txn(|ui| ui.reconcile_root(node));
        }

        // 3. Drain the dirty set, shallowest first, with depth read at flush
        //    time. A parent rebuild reconciles its children, which clears their
        //    marks; a subtree skipped by the identity short-circuit keeps its
        //    marks and is rebuilt later in the same drain.
        let mut rounds = 0;
        loop {
            let mut ids: Vec<ElementId> = self.sched.borrow_mut().dirty.drain().collect();
            if ids.is_empty() {
                break;
            }
            ids.sort_by_key(|&e| (self.arena.get(e).map(|el| el.depth).unwrap_or(0), e.0));
            for e in ids {
                let Some(el) = self.arena.get(e) else {
                    continue;
                };
                if el.disposed || !el.needs_build {
                    continue;
                }
                self.in_txn(|ui| ui.rebuild(e));
            }
            rounds += 1;
            assert!(
                rounds < 64,
                "flush did not converge: an element keeps marking itself dirty"
            );
        }

        // 4. Deferred disposal, children before parents.
        self.process_disposals();
    }

    /// Run one reconcile transaction. On a panic the tree is rolled back to the
    /// last committed content and the panic continues; the caller sees a tree
    /// that is intact, not one that is half-updated.
    fn in_txn<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.begin_txn();
        match std::panic::catch_unwind(AssertUnwindSafe(|| f(self))) {
            Ok(v) => {
                self.commit_txn();
                v
            }
            Err(payload) => {
                self.abort_txn();
                std::panic::resume_unwind(payload)
            }
        }
    }

    fn set_needs_build_untracked(&mut self, id: ElementId, v: bool) {
        if let Some(el) = self.arena.get_mut(id) {
            el.needs_build = v;
        }
    }

    // -- application-facing state access -----------------------------------

    /// Queue a state mutation from outside a handler. Same path as
    /// [`Updater::set`]: applied at the start of the next flush.
    pub fn set_state<S: 'static>(&mut self, id: ElementId, f: impl FnOnce(&mut S) + 'static) {
        let up: Updater<S> = Updater {
            sched: self.sched.clone(),
            id,
            _p: PhantomData,
        };
        up.set(f);
    }

    /// Mark an element for rebuild without changing its state.
    pub fn mark(&mut self, id: ElementId) {
        self.sched.borrow_mut().mark(id);
    }

    pub fn state<S: 'static>(&self, id: ElementId) -> Option<&S> {
        self.arena
            .get(id)?
            .state
            .as_ref()
            .and_then(|s| s.downcast_ref::<S>())
    }

    // -- tree inspection ---------------------------------------------------

    pub fn root(&self) -> Option<ElementId> {
        self.root
    }

    pub fn children(&self, id: ElementId) -> Vec<ElementId> {
        self.arena
            .get(id)
            .map(|e| e.children.clone())
            .unwrap_or_default()
    }

    pub fn parent(&self, id: ElementId) -> Option<ElementId> {
        self.arena.get(id).and_then(|e| e.parent)
    }

    pub fn depth(&self, id: ElementId) -> Option<u32> {
        self.arena.get(id).map(|e| e.depth)
    }

    pub fn key_of(&self, id: ElementId) -> Option<crate::key::Key> {
        self.arena.get(id).and_then(|e| e.key.clone())
    }

    pub fn type_of(&self, id: ElementId) -> Option<crate::desc::ElemType> {
        self.arena.get(id).map(|e| e.ty)
    }

    pub fn name_of(&self, id: ElementId) -> Option<&'static str> {
        self.arena.get(id).map(|e| e.name)
    }

    pub fn is_live(&self, id: ElementId) -> bool {
        self.arena.get(id).is_some()
    }

    pub fn live_count(&self) -> usize {
        self.arena.live()
    }

    /// Walk from the root by child index: `ui.at(&[0, 2])`.
    pub fn at(&self, path: &[usize]) -> Option<ElementId> {
        let mut cur = self.root?;
        for &i in path {
            cur = *self.arena.get(cur)?.children.get(i)?;
        }
        Some(cur)
    }

    // -- diagnostics -------------------------------------------------------

    /// Record every component build. Off by default; the log is unbounded.
    pub fn trace(&mut self, on: bool) {
        self.trace = on;
        if !on {
            self.build_log.clear();
        }
    }

    pub fn take_build_log(&mut self) -> Vec<ElementId> {
        std::mem::take(&mut self.build_log)
    }
}
