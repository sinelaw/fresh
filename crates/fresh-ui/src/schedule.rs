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

use crate::ambient::{Ambient, AmbientNode};
use crate::behavior::Behavior;
use crate::desc::{Event, Handler, Node};
use crate::element::{Arena, ElementId, Txn};
use crate::render::geom::{Point, Rect, Size};
use crate::render::spec::LayoutSpec;

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

/// Why an element was marked for rebuild. Recorded so that a tree dump answers
/// the question a failing test actually asks.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DirtyCause {
    /// First build, at mount.
    Mount,
    /// A `set_state`, at this source location.
    SetState(&'static std::panic::Location<'static>),
    /// The named ambient changed.
    Ambient(&'static str),
    /// Reconciled as part of this parent's rebuild.
    Parent(ElementId),
    /// Marked explicitly, at this source location.
    Marked(&'static std::panic::Location<'static>),
    /// Focus entered or left this element's subtree.
    Focus,
}

impl std::fmt::Display for DirtyCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirtyCause::Mount => write!(f, "mount"),
            DirtyCause::SetState(l) => write!(f, "set_state {}:{}", l.file(), l.line()),
            DirtyCause::Ambient(n) => write!(f, "ambient @{n}"),
            DirtyCause::Parent(p) => write!(f, "parent {p:?}"),
            DirtyCause::Marked(l) => write!(f, "mark {}:{}", l.file(), l.line()),
            DirtyCause::Focus => write!(f, "focus"),
        }
    }
}

type Mutation = Box<dyn FnOnce(&mut dyn Any)>;

/// Shared between the tree and every handler that can mark it dirty.
pub struct Sched {
    pub(crate) building: Option<(ElementId, &'static str)>,
    dirty: HashSet<ElementId>,
    mutations: Vec<(ElementId, Mutation)>,
    /// Causes recorded by handlers, which cannot reach the element themselves.
    /// Applied at the start of the flush.
    causes: Vec<(ElementId, DirtyCause)>,
}

pub(crate) type SchedRef = Rc<RefCell<Sched>>;

impl Sched {
    fn new() -> Self {
        Sched {
            building: None,
            dirty: HashSet::new(),
            mutations: Vec::new(),
            causes: Vec::new(),
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
        self.causes.retain(|(i, _)| *i != id);
    }

    fn guard_not_building(&self, what: &str) {
        if let Some((id, name)) = self.building {
            panic!("{what} during build at {name} ({id:?})");
        }
    }

    pub(crate) fn mark(&mut self, id: ElementId) {
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
    #[track_caller]
    pub fn set(&self, f: impl FnOnce(&mut S) + 'static) {
        self.set_at(std::panic::Location::caller(), f);
    }

    /// As [`set`], with the reporting site supplied explicitly. A handler built
    /// during a build records the site where the handler was created, which is
    /// the source location that explains the rebuild.
    ///
    /// [`set`]: Updater::set
    pub fn set_at(
        &self,
        site: &'static std::panic::Location<'static>,
        f: impl FnOnce(&mut S) + 'static,
    ) {
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
        s.causes.push((id, DirtyCause::SetState(site)));
        s.mark(id);
    }
}

// ---------------------------------------------------------------------------
// BuildCx
// ---------------------------------------------------------------------------

/// What a component sees while constructing its state.
///
/// This runs once per mount. Behaviors are registered here, and an ambient read
/// here is a **snapshot**: `init` does not re-run, so a value cached in a field
/// from here will not track later changes to that ambient.
pub struct InitCx<'a, M> {
    id: ElementId,
    sched: SchedRef,
    scope: Option<Rc<AmbientNode>>,
    children: &'a [Node<M>],
    behaviors: Vec<Rc<dyn Behavior>>,
    init_reads: Vec<ElementId>,
}

impl<'a, M: 'static> InitCx<'a, M> {
    pub(crate) fn new(
        id: ElementId,
        sched: SchedRef,
        scope: Option<Rc<AmbientNode>>,
        children: &'a [Node<M>],
    ) -> Self {
        InitCx {
            id,
            sched,
            scope,
            children,
            behaviors: Vec::new(),
            init_reads: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> (Vec<Rc<dyn Behavior>>, Vec<ElementId>) {
        (self.behaviors, self.init_reads)
    }

    pub fn id(&self) -> ElementId {
        self.id
    }

    pub fn children(&self) -> &'a [Node<M>] {
        self.children
    }

    pub fn updater<S: 'static>(&self) -> Updater<S> {
        Updater {
            sched: self.sched.clone(),
            id: self.id,
            _p: PhantomData,
        }
    }

    /// Enrol a behavior for teardown. The returned handle is stored in a named
    /// field of the state; the element holds the other half. Teardown runs in
    /// reverse registration order when the element is disposed.
    pub fn register<B: Behavior + 'static>(&mut self, b: B) -> Rc<B> {
        let rc = Rc::new(b);
        self.behaviors.push(rc.clone() as Rc<dyn Behavior>);
        rc
    }

    /// Read an ambient as a snapshot. This does **not** create a dependency:
    /// caching the result in a field and using it later is the stale-output
    /// case, and it is reported by a debug assertion if the value ever changes.
    pub fn read<T: 'static>(&mut self, ambient: &Ambient<T>) -> Option<Rc<T>> {
        let (provider, value) = self.scope.as_ref()?.lookup(ambient.key())?;
        if !self.init_reads.contains(&provider) {
            self.init_reads.push(provider);
        }
        value.downcast::<T>().ok()
    }
}

/// What a component sees while building.
pub struct BuildCx<'a, M> {
    id: ElementId,
    sched: SchedRef,
    scope: Option<Rc<AmbientNode>>,
    children: &'a [Node<M>],
    reads: Vec<ElementId>,
}

impl<'a, M: 'static> BuildCx<'a, M> {
    pub(crate) fn new(
        id: ElementId,
        sched: SchedRef,
        scope: Option<Rc<AmbientNode>>,
        children: &'a [Node<M>],
    ) -> Self {
        BuildCx {
            id,
            sched,
            scope,
            children,
            reads: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> Vec<ElementId> {
        self.reads
    }

    /// Read an ambient and register this element as a dependent of the nearest
    /// provider. One explicit hop, a visible dependent list, no tracking.
    ///
    /// Rebuild-and-re-read is the whole protocol: when the provider's value
    /// changes, every dependent is marked, regardless of which part of the
    /// value it actually read.
    pub fn read<T: 'static>(&mut self, ambient: &Ambient<T>) -> Option<Rc<T>> {
        let (provider, value) = self.scope.as_ref()?.lookup(ambient.key())?;
        if !self.reads.contains(&provider) {
            self.reads.push(provider);
        }
        value.downcast::<T>().ok()
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
    #[track_caller]
    pub fn set_state<S: 'static>(&self, f: impl Fn(&mut S) + 'static) -> Handler<M> {
        let up = self.updater::<S>();
        let site = std::panic::Location::caller();
        let f = Rc::new(f);
        Rc::new(move |_ev: &Event| {
            let f = f.clone();
            up.set_at(site, move |s| f(s));
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

    /// Geometry state.
    pub(crate) frame_size: Size,
    /// Relayout boundaries awaiting a measure pass.
    pub(crate) layout_dirty: Vec<ElementId>,
    /// Out-of-flow layers found by the last arrange walk, with their parents.
    pub(crate) pending_layers: Vec<(ElementId, ElementId)>,
    pub(crate) spec: LayoutSpec,

    /// Pointer state.
    pub(crate) hover: Vec<ElementId>,
    pub(crate) captured: Option<ElementId>,
    pub(crate) press: Option<(ElementId, crate::event::Button)>,

    /// Focus state. Neither the application nor the component declares it.
    pub(crate) focus: Option<ElementId>,
    pub(crate) focus_selection: crate::event::SelectionOnFocus,
    pub(crate) traversal: Box<dyn crate::focus::TraversalPolicy>,
    pub(crate) shortcuts: Vec<crate::focus::Shortcut>,
    /// Messages produced outside a dispatch call, delivered with the next one.
    pub(crate) pending_messages: Vec<M>,
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
            frame_size: Size::ZERO,
            layout_dirty: Vec::new(),
            pending_layers: Vec::new(),
            spec: LayoutSpec::default(),
            hover: Vec::new(),
            captured: None,
            press: None,
            focus: None,
            focus_selection: crate::event::SelectionOnFocus::None,
            traversal: Box::new(crate::focus::ReadingOrder),
            shortcuts: crate::focus::default_shortcuts(),
            pending_messages: Vec::new(),
        }
    }

    /// One frame: take a freshly built root description, reconcile it, lay it
    /// out for a terminal of `size`, and return the display list.
    pub fn frame(&mut self, root: Node<M>, size: Size) -> &LayoutSpec {
        self.run_flush(Some(root));
        self.flush_layout(size);
        self.apply_autofocus();
        self.flush_paint(size);
        &self.spec
    }

    /// Rebuild what is dirty and re-render at the last frame size. This is the
    /// path an event takes: nothing new is supplied from the application.
    pub fn tick(&mut self) -> &LayoutSpec {
        let size = self.frame_size;
        self.run_flush(None);
        self.flush_layout(size);
        self.apply_autofocus();
        self.flush_paint(size);
        &self.spec
    }

    /// Reconcile a new root description without laying it out. For callers
    /// that only care about tree structure.
    pub fn reconcile(&mut self, root: Node<M>) {
        self.run_flush(Some(root));
    }

    /// Rebuild whatever is dirty, without a new root description and without
    /// laying anything out.
    pub fn flush(&mut self) {
        self.run_flush(None);
    }

    /// The display list from the last `frame` or `tick`.
    pub fn spec(&self) -> &LayoutSpec {
        &self.spec
    }

    /// Messages produced by framework-initiated activity — a focus change asked
    /// for imperatively, for instance — since the last time they were taken.
    pub fn take_messages(&mut self) -> Vec<M> {
        std::mem::take(&mut self.pending_messages)
    }

    // -- geometry ----------------------------------------------------------

    /// Absolute position and size, valid after the first layout. Never during
    /// `build`: build is a function of the description, state and ambients, and
    /// reading geometry from it would make build depend on layout, which
    /// depends on build.
    #[track_caller]
    pub fn rect(&self, id: ElementId) -> Rect {
        debug_assert!(
            self.sched.borrow().building.is_none(),
            "geometry is not readable during build"
        );
        self.arena
            .get(id)
            .map(|e| e.layout.rect)
            .unwrap_or_default()
    }

    #[track_caller]
    pub fn size_of(&self, id: ElementId) -> Size {
        debug_assert!(
            self.sched.borrow().building.is_none(),
            "geometry is not readable during build"
        );
        self.arena
            .get(id)
            .map(|e| e.layout.size)
            .unwrap_or_default()
    }

    /// The clip this element inherited from its ancestors.
    pub fn clip(&self, id: ElementId) -> Rect {
        self.arena
            .get(id)
            .map(|e| e.layout.clip)
            .unwrap_or_default()
    }

    /// How many times this element has been measured. A change that stops at a
    /// relayout boundary leaves the counters above it untouched.
    pub fn layouts(&self, id: ElementId) -> u32 {
        self.arena.get(id).map(|e| e.layout.layouts).unwrap_or(0)
    }

    /// A viewport's scroll offset and the size of the content behind it.
    pub fn scroll(&self, id: ElementId) -> (Point, Size) {
        self.arena
            .get(id)
            .map(|e| (e.layout.scroll, e.layout.content))
            .unwrap_or_default()
    }

    /// Move a viewport's window. Framework-owned state: it survives rebuilds
    /// and is not declared by the component.
    pub fn scroll_to(&mut self, id: ElementId, offset: Point) {
        let changed = match self.arena.get_mut(id) {
            Some(el) => {
                let old = el.layout.scroll;
                el.layout.scroll = offset;
                old != offset
            }
            None => false,
        };
        if changed {
            self.mark_needs_layout(id);
        }
    }

    pub fn frame_size(&self) -> Size {
        self.frame_size
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
        let causes = std::mem::take(&mut self.sched.borrow_mut().causes);
        for (id, cause) in causes {
            if let Some(el) = self.arena.get_mut(id) {
                if !el.disposed {
                    el.last_dirty = Some(cause);
                }
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
    #[track_caller]
    pub fn set_state<S: 'static>(&mut self, id: ElementId, f: impl FnOnce(&mut S) + 'static) {
        let up: Updater<S> = Updater {
            sched: self.sched.clone(),
            id,
            _p: PhantomData,
        };
        up.set_at(std::panic::Location::caller(), f);
    }

    /// Mark an element for rebuild without changing its state.
    #[track_caller]
    pub fn mark(&mut self, id: ElementId) {
        self.mark_dirty(id, DirtyCause::Marked(std::panic::Location::caller()));
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
