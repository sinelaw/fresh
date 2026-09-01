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

    /// Whether a flush would do anything.
    pub(crate) fn has_pending(&self) -> bool {
        !self.dirty.is_empty() || !self.mutations.is_empty()
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
    services: crate::services::Services,
    geom_store: Rc<RefCell<crate::services::GeomStore>>,
    /// A focus registration this component asked for, applied by the framework
    /// once construction finishes.
    pub(crate) focus_request: Option<crate::focus::tree::FocusNodeData>,
}

impl<'a, M: 'static> InitCx<'a, M> {
    pub(crate) fn new(
        id: ElementId,
        sched: SchedRef,
        scope: Option<Rc<AmbientNode>>,
        children: &'a [Node<M>],
        services: crate::services::Services,
        geom_store: Rc<RefCell<crate::services::GeomStore>>,
    ) -> Self {
        InitCx {
            id,
            sched,
            scope,
            children,
            behaviors: Vec::new(),
            init_reads: Vec::new(),
            services,
            geom_store,
            focus_request: None,
        }
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn finish(
        self,
    ) -> (
        Vec<Rc<dyn Behavior>>,
        Vec<ElementId>,
        Option<crate::focus::tree::FocusNodeData>,
    ) {
        (self.behaviors, self.init_reads, self.focus_request)
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
        rc.attach(&self.services);
        self.behaviors.push(rc.clone() as Rc<dyn Behavior>);
        rc
    }

    /// Register this component in the focus tree without wrapping it in a
    /// `Focusable` description.
    ///
    /// Everything a `Focusable` primitive provides — traversal, key routing,
    /// focus transitions — applies. The registration lives as long as the
    /// element, so focus survives reconciliation for the same reason.
    pub fn focusable(&mut self, f: crate::focus::Focusable<M>) -> Rc<crate::focus::Focusable<M>> {
        self.focus_request = Some(crate::focus::tree::FocusNodeData::new(
            self.id,
            crate::render::object::FocusReg {
                ordinal: f.ordinal,
                skip: f.skip,
                scope: f.scope,
                focus_within: f.focus_within,
                autofocus: f.autofocus,
            },
        ));
        self.register(f)
    }

    /// What is valid from construction: the scheduler, the spawner, the
    /// registries. Deliberately free of geometry.
    pub fn services(&self) -> &crate::services::Services {
        &self.services
    }

    /// A handle to this element's geometry, valid from the first layout on.
    ///
    /// Taken here, at construction, and read from an event handler, a ticker or
    /// a task callback — all of which run while the tree is borrowed and so
    /// cannot hold a reference into it. Reading one during `build` is rejected:
    /// that is the same validity window `Geometry` carries, checked at the read
    /// rather than by the borrow.
    pub fn geometry(&mut self) -> crate::services::GeomHandle {
        crate::services::GeomHandle::new(self.geom_store.clone(), self.sched.clone(), self.id)
    }

    /// A handle to another element's geometry, addressed by the element id an
    /// owner already holds.
    pub fn geometry_of(&mut self, id: ElementId) -> crate::services::GeomHandle {
        crate::services::GeomHandle::new(self.geom_store.clone(), self.sched.clone(), id)
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
    services: crate::services::Services,
}

impl<'a, M: 'static> BuildCx<'a, M> {
    pub(crate) fn new(
        id: ElementId,
        sched: SchedRef,
        scope: Option<Rc<AmbientNode>>,
        children: &'a [Node<M>],
        services: crate::services::Services,
    ) -> Self {
        BuildCx {
            id,
            sched,
            scope,
            children,
            reads: Vec::new(),
            services,
        }
    }

    /// What is valid from construction. There is deliberately no
    /// `cx.geometry` here: build is a function of the description, the state
    /// and the ambients, and reading layout from it would make build depend on
    /// itself.
    pub fn services(&self) -> &crate::services::Services {
        &self.services
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

    /// The render tree: computed, retained geometry.
    pub(crate) render: crate::render::object::RenderArena,
    pub(crate) render_root: Option<crate::render::object::RenderId>,
    /// The focus tree: registrations, and the scopes that group them.
    pub(crate) focus_tree: crate::focus::tree::FocusTree,
    /// Focus registrations with no focus ancestor.
    pub(crate) focus_roots: Vec<crate::focus::FocusId>,
    /// Set while a render object is being measured, so a second measurement of
    /// the same subtree in one frame can be counted.
    pub(crate) measuring: bool,
    /// Set while a [`Ui::layout_only`] pass runs, so the passes below can tell
    /// "compute this layout" from "a frame happened".
    ///
    /// It changes nothing about the geometry — the same build, the same
    /// measure, the same arrange. What it changes is that the things a frame
    /// *owes the application* are not owed to a question: an arrival handed
    /// over, a queued scroll applied. Those are reactions to a frame having
    /// happened, and a caller that fires them to ask where something is
    /// changes the thing it is asking about.
    pub(crate) geometry_pass: bool,
    pub(crate) frame_no: u64,
    pub(crate) frame_size: Size,
    /// Relayout boundaries awaiting a measure pass.
    pub(crate) layout_dirty: Vec<crate::render::object::RenderId>,
    /// Rectangles the host supplied for keys no element carries.
    ///
    /// An [`Anchor::Node`] names a thing; usually that thing is an element and
    /// the tree knows where it is. Sometimes it is a point *inside* a host leaf
    /// — a text caret, a terminal's cursor — and the tree hands that leaf a
    /// rectangle and knows nothing about what is in it. The owner of the space
    /// supplies the rectangle instead, and the anchor is still a name rather
    /// than a number in a description.
    ///
    /// The web platform calls this an anchor element with a virtual reference;
    /// the rule here is the same one: whoever owns the space answers where the
    /// thing is. Cleared at the start of every frame, because a stale caret is
    /// worse than none.
    pub(crate) host_anchors: std::collections::HashMap<crate::key::Key, Rect>,
    /// Out-of-flow layers found by the last arrange walk, with their parents.
    pub(crate) pending_layers: Vec<(
        crate::render::object::RenderId,
        crate::render::object::RenderId,
    )>,
    pub(crate) spec: LayoutSpec,

    /// Pointer state.
    pub(crate) hover: Vec<ElementId>,
    pub(crate) captured: Option<ElementId>,
    /// The elements a press landed on, which button it was, and which press
    /// of a run it was — the last so the `Click` it completes can report it.
    pub(crate) press: Option<(Vec<ElementId>, crate::event::MouseButton, u8)>,

    /// Focus state. Neither the application nor the component declares it.
    pub(crate) focus: Option<ElementId>,
    pub(crate) focus_selection: crate::event::SelectionOnFocus,
    /// Where focus was before a modal took it.
    pub(crate) focus_restore: Option<ElementId>,
    pub(crate) traversal: Box<dyn crate::focus::TraversalPolicy>,
    pub(crate) shortcuts: Vec<crate::focus::Shortcut>,
    /// Messages produced outside a dispatch call, delivered with the next one.
    pub(crate) pending_messages: Vec<M>,
    pub(crate) services: crate::services::Services,
    /// Elements that registered behaviors, so the scheduler can pump them
    /// without walking the tree.
    pub(crate) behaviour_hosts: Vec<ElementId>,
    /// Elements an owner holds a handle to.
    pub(crate) anchored: Vec<ElementId>,
    /// Geometry for the elements somebody holds a handle to, refreshed once a
    /// frame. A handler runs while the tree is borrowed, so it reads here.
    pub(crate) geom_store: Rc<RefCell<crate::services::GeomStore>>,
    /// The viewport whose scrollbar is being dragged, between press and release.
    pub(crate) scrollbar_drag: Option<crate::render::object::RenderId>,
    /// Where in the thumb the drag was grabbed, in rows from its top.
    ///
    /// **A press on the thumb picks it up; a press on the track jumps to it.**
    /// Without this every press put the thumb's *top* under the pointer, so
    /// grabbing a thumb anywhere but its first row shifted it up by however
    /// far down it had been grabbed — a press that moved the viewport without
    /// the pointer moving at all. Zero for a press on the bare track, which is
    /// the jump.
    pub(crate) scrollbar_grab: i32,
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
            render: Default::default(),
            render_root: None,
            focus_tree: Default::default(),
            focus_roots: Vec::new(),
            measuring: false,
            geometry_pass: false,
            frame_no: 0,
            frame_size: Size::ZERO,
            layout_dirty: Vec::new(),
            host_anchors: std::collections::HashMap::new(),
            pending_layers: Vec::new(),
            spec: LayoutSpec::default(),
            hover: Vec::new(),
            captured: None,
            press: None,
            focus: None,
            focus_selection: crate::event::SelectionOnFocus::None,
            focus_restore: None,
            traversal: Box::new(crate::focus::ReadingOrder),
            shortcuts: crate::focus::default_shortcuts(),
            pending_messages: Vec::new(),
            services: Default::default(),
            behaviour_hosts: Vec::new(),
            anchored: Vec::new(),
            geom_store: Rc::new(RefCell::new(crate::services::GeomStore::default())),
            scrollbar_drag: None,
            scrollbar_grab: 0,
        }
    }

    /// One frame: take a freshly built root description, reconcile it, lay it
    /// out for a terminal of `size`, and return the display list.
    pub fn frame(&mut self, root: Node<M>, size: Size) -> &LayoutSpec {
        self.host_anchors.clear();
        self.run_flush(Some(root));
        self.flush_layout(size);
        self.settle(size);
        self.flush_paint(size);
        &self.spec
    }

    /// The geometry a frame would produce, without the frame.
    ///
    /// A host that needs a rectangle out of a description it has just built —
    /// how tall a region came out, where a row landed — needs the build and
    /// the layout: a description *states* its geometry, and measuring it is
    /// the only thing that answers. It does not need what a frame additionally
    /// *does*. Focus moving to an autofocused element and the application
    /// being told so, a queued reveal scrolling a viewport, a ticker
    /// advancing, a display list nothing will draw: those happen because a
    /// frame was shown. A caller that fires them in order to ask a question
    /// changes the thing it is measuring — once per question asked.
    ///
    /// Reconciliation is *not* one of them, and still runs: an element that
    /// does not exist has no rectangle, so a new description has to be mounted
    /// before it can be measured. That is also why this is a query against the
    /// live tree rather than a scratch copy of it — a description carries
    /// handles ([`Anchor`](crate::behavior::Anchor)) that bind to whichever
    /// tree reconciles them, so laying the same description out in a second
    /// tree would take the binding away from the first.
    ///
    /// Read the answer with [`Ui::rect_of`] and its neighbours. There is no
    /// display list to return, because nothing was painted; the one from the
    /// last real frame is left alone, which is what is on the screen. The next
    /// [`Ui::frame`] is unaffected — it reconciles whatever description it is
    /// given, over this one.
    pub fn layout_only(&mut self, root: Node<M>, size: Size) {
        // Host anchors are neither cleared nor published here: they are the
        // host's answer for what lives inside its own leaves, `frame` clears
        // them because it is about to collect a fresh set, and a query has no
        // fresh set to collect.
        let was = std::mem::replace(&mut self.geometry_pass, true);
        self.run_flush(Some(root));
        self.flush_layout(size);
        // The half of `settle` that is part of computing a layout rather than
        // a reaction to one. A builder that runs *during* the layout pass — a
        // `layout_reader`, or the `Component` inside one that fills a windowed
        // list's rows — raises its dirt after the layout drain has finished
        // its own loop, and geometry read without draining it is the geometry
        // of the description *before* that build. Autofocus is the other half,
        // and is the reaction.
        if self.sched.borrow().has_pending() || !self.layout_dirty.is_empty() {
            self.run_flush(None);
            self.flush_layout(size);
        }
        self.geometry_pass = was;
    }

    /// Tell the tree where something inside a host leaf is, so an
    /// [`Anchor::Node`] naming it can resolve.
    ///
    /// The tree gives a host a rectangle and knows nothing about its interior,
    /// so a layer anchored to a text caret names something only the host can
    /// locate. This is how the host answers. See [`Ui::host_anchors`].
    ///
    /// An element carrying the key always wins: this fills a gap, it cannot
    /// shadow a real node. Anchors are per-frame and [`Ui::frame`] clears them.
    /// Call [`Ui::place_layers`] afterwards to put the layers where the new
    /// anchors say.
    pub fn set_host_anchor(&mut self, key: crate::key::Key, rect: Rect) {
        self.host_anchors.insert(key, rect);
    }

    /// Place every layer again, against the anchors as they now stand, and
    /// repaint.
    ///
    /// For a host whose own pipeline is interleaved with the tree's: it needs
    /// the frame laid out before it can work out where its caret is, and the
    /// caret before a layer hanging off it can be placed. Re-running the frame
    /// would reconcile a description that has not changed and pump every
    /// behaviour a second time; this re-walks the arrangement and repaints,
    /// which is what actually has to happen.
    pub fn place_layers(&mut self, size: Size) -> &LayoutSpec {
        self.replace_layers(size);
        self.flush_paint(size);
        &self.spec
    }

    /// Whether a `frame` or `tick` would change anything: a dirty element, a
    /// state mutation waiting to apply, a message produced out of band, or a
    /// behavior with something to deliver or a ticker running.
    ///
    /// This surfaces the mark-and-flush state the scheduler already keeps; it
    /// is a whole-frame yes/no, not per-cell damage tracking. A host loop reads
    /// it to skip quiet frames entirely — compute and paint — instead of
    /// redrawing at a fixed rate.
    pub fn needs_frame(&self) -> bool {
        if self.sched.borrow().has_pending() || !self.pending_messages.is_empty() {
            return true;
        }
        self.behaviour_hosts.iter().any(|id| {
            self.arena
                .get(*id)
                .is_some_and(|el| el.behaviors.iter().any(|b| b.has_pending()))
        })
    }

    /// Rebuild what is dirty and re-render at the last frame size. This is the
    /// path an event takes: nothing new is supplied from the application.
    pub fn tick(&mut self) -> &LayoutSpec {
        let size = self.frame_size;
        self.run_flush(None);
        self.flush_layout(size);
        self.settle(size);
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

    /// Install the host's executor. Work a component starts through `Tasks`
    /// runs there instead of on a thread of the library's choosing.
    pub fn set_spawner(&mut self, f: impl Fn(crate::services::Job) + 'static) {
        self.services.spawn = Rc::new(f);
    }

    /// The services this tree hands to behaviors. Exposed so a test can attach
    /// a behavior without mounting one.
    pub fn services_for_test(&self) -> crate::services::Services {
        self.services.clone()
    }

    /// Install the host's persistence store. `Persisted` values are read from
    /// it at construction and written back at teardown.
    pub fn set_store(&mut self, s: Rc<dyn crate::behavior::Store>) {
        self.services.store = Some(s);
    }

    /// Whether a host leaf should receive raw input this frame.
    ///
    /// Derived rather than declared: an exclusive layer makes everything
    /// outside it inert, and a leaf that is inert takes no raw input. This is
    /// what replaces a `blocks_terminal_input` flag.
    pub fn raw_input(&self) -> bool {
        self.raw_input_leaves().next().is_some()
    }

    /// Which host leaves are taking raw input this frame.
    ///
    /// Derived rather than declared, and answered per element rather than for
    /// the tree as a whole: an exclusive layer makes everything outside it
    /// inert, and a leaf that is inert takes no raw input. A leaf *inside* the
    /// exclusive layer still does. This is what replaces a
    /// `blocks_terminal_input` flag.
    pub fn raw_input_leaves(&self) -> impl Iterator<Item = ElementId> + '_ {
        let exclusive: Vec<ElementId> = self
            .pending_layers
            .iter()
            .filter(|(l, _)| {
                self.layer_geom(*l)
                    .is_some_and(|g| g.modality == crate::desc::Modality::Exclusive)
            })
            .filter_map(|(l, _)| self.element_of(*l))
            .collect();
        self.render
            .live_ids()
            .filter_map(move |r| {
                let n = self.render.get(r)?;
                n.raw_input.then_some(n.element)
            })
            .filter(move |e| {
                exclusive.is_empty() || exclusive.iter().any(|x| self.is_ancestor(*x, *e))
            })
    }

    /// Whether `anc` is `e` or one of its ancestors.
    pub(crate) fn is_ancestor(&self, anc: ElementId, e: ElementId) -> bool {
        let mut cur = Some(e);
        while let Some(c) = cur {
            if c == anc {
                return true;
            }
            cur = self.arena.get(c).and_then(|x| x.parent);
        }
        false
    }

    /// Geometry for one element, valid after the first layout. The type is what
    /// keeps it out of `build`.
    pub fn geometry(&self, id: ElementId) -> crate::services::Geometry<'_, M> {
        crate::services::Geometry { ui: self, id }
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
        self.rect_of(id)
    }

    /// As `rect`, without the build-time check. For framework use.
    pub fn rect_of(&self, id: ElementId) -> Rect {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .map(|n| n.data.rect)
            .unwrap_or_default()
    }

    #[track_caller]
    pub fn size_of(&self, id: ElementId) -> Size {
        debug_assert!(
            self.sched.borrow().building.is_none(),
            "geometry is not readable during build"
        );
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .map(|n| n.data.size)
            .unwrap_or_default()
    }

    /// The clip this element inherited from its ancestors.
    pub fn clip(&self, id: ElementId) -> Rect {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .map(|n| n.data.clip)
            .unwrap_or_default()
    }

    /// How many times this element has been measured. A change that stops at a
    /// relayout boundary leaves the counters above it untouched.
    pub fn layouts(&self, id: ElementId) -> u32 {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .map(|n| n.data.layouts)
            .unwrap_or(0)
    }

    /// How many of those were a second look at the same subtree in one frame.
    /// `Auto` is the ergonomic default, so its cost is reported rather than
    /// inferred.
    pub fn remeasures(&self, id: ElementId) -> u32 {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .map(|n| n.data.remeasures)
            .unwrap_or(0)
    }

    /// A viewport's scroll offset and the size of the content behind it.
    ///
    /// Both are in the unit that viewport's offset counts, which only
    /// [`Ui::band`] says. See [`Ui::window`].
    pub fn scroll(&self, id: ElementId) -> (Point, Size) {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .map(|n| (n.data.scroll, n.data.content))
            .unwrap_or_default()
    }

    /// A viewport's window, **in the unit its offset counts**. `None` for an
    /// element that is not scrolling anything.
    ///
    /// The unit is not the rectangle's. A cell-scrolling window's offset is a
    /// row and its window is its own height, so `window.h == rect_of(id).h`;
    /// an item-scrolling one's offset is an index and `window.h` is *how many
    /// items* fit, which is that height divided by the band — four, not
    /// twelve, for three-row cards in a twelve-row box. [`Ui::band`] is what
    /// tells them apart: `Some(_)` means items, `None` means cells. Reading
    /// the height for both put a list of three-row cards eleven items down
    /// inside a "fifteen-row" window, and drew a thumb that filled its own
    /// track.
    ///
    /// The horizontal axis is cells either way: an item-scrolled window counts
    /// items down and cells across.
    pub fn window(&self, id: ElementId) -> Option<Rect> {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .and_then(|n| n.data.window)
    }

    /// The band a viewport's items sit in, when its offset counts items.
    /// `None` where it counts cells — there an item is not a thing.
    ///
    /// This is the unit tag for [`Ui::window`] and [`Ui::scroll`], and for
    /// [`Band::Cells`](crate::render::object::Band::Cells) it is also the
    /// number that converts between them: one item is that many cells tall.
    pub fn band(&self, id: ElementId) -> Option<crate::render::object::Band> {
        self.render_for(id)
            .and_then(|r| self.render.get(r))
            .and_then(|n| n.data.band)
    }

    /// How many items a keyed widget is showing, and which — the window of the
    /// nearest item-scrolling viewport at or under the element carrying `key`,
    /// **in items**: `window.y` is the first item on screen and `window.h` is
    /// how many of them fit. (Across, it is still cells: an item-scrolled
    /// window scrolls only down.)
    ///
    /// **The descend is the contract, not an implementation detail.** A key
    /// belongs to whoever wrote it, and what they wrote it on is a widget: a
    /// [`List`](crate::widgets::List) keyed by its owner carries that key on
    /// the component element, while the viewport that owns the window is one
    /// or two elements inside it — under the focus wrapper, when the list
    /// takes focus. So this searches at and under the key rather than only at
    /// it, nearest first, and the first window found is the answer.
    ///
    /// `None` where the key names nothing, where nothing under it scrolls, or
    /// where what scrolls counts *cells* — a text area has no items, and
    /// answering with its height in rows would be the units conflated again,
    /// under a name that promises they are not. [`Ui::window`] is the way to
    /// ask that one.
    ///
    /// Same standing as [`Ui::rect_of`]: an outside caller reading what the
    /// last layout decided, from the tree that decided it.
    ///
    /// This searches the whole frame, so it is right only where the key is
    /// unique frame-wide; [`Ui::item_window_in`] scopes the search to one
    /// subtree, and also reports the window's height in cells.
    pub fn item_window(&self, key: &crate::key::Key) -> Option<Rect> {
        let root = self.root?;
        Some(self.item_window_in(root, key)?.0)
    }

    /// [`Ui::item_window`] searched inside one subtree, with the window's own
    /// height **in cells** beside it.
    ///
    /// Two answers because they are two units and neither derives the other
    /// without the band. `Rect::h` is items; the `u16` is how many cells those
    /// items occupy, which is the viewport's own laid-out height. A caller
    /// that needs both — a widget whose selection steps items while its scroll
    /// offset counts rows — would otherwise have to multiply by a band it
    /// cannot see, and §6i of the retained-mode plan is the record of what
    /// guessing that number costs.
    ///
    /// The root is the scope, for the reason [`Ui::find_by_key_in`] states: a
    /// key is unique only where its owner says it is, and two panels in one
    /// frame can each key a list `"items"`. `item_window` is this from the
    /// tree's root, which is right only when the caller knows the key is
    /// unique frame-wide.
    pub fn item_window_in(&self, root: ElementId, key: &crate::key::Key) -> Option<(Rect, u16)> {
        let el = self.find_by_key_in(root, key)?;
        let r = self.viewport_at_or_under(el)?;
        let n = self.render.get(r)?;
        let window = n.data.band.is_some().then_some(n.data.window).flatten()?;
        Some((window, n.data.rect.h))
    }

    /// The nearest scrolling render node at or under `el`, breadth-first, so
    /// that a viewport nested inside one of the key's own rows never wins over
    /// the widget's own.
    fn viewport_at_or_under(&self, el: ElementId) -> Option<crate::render::object::RenderId> {
        let mut queue = std::collections::VecDeque::from([el]);
        while let Some(e) = queue.pop_front() {
            let Some(node) = self.arena.get(e) else {
                continue;
            };
            if let Some(r) = node.render {
                if self.render.get(r).is_some_and(|n| n.data.window.is_some()) {
                    return Some(r);
                }
            }
            queue.extend(node.children.iter().copied());
        }
        None
    }

    /// Move a viewport's window. Framework-owned state: it survives rebuilds
    /// and is not declared by the component.
    pub fn scroll_to(&mut self, id: ElementId, offset: Point) {
        let Some(r) = self.render_for(id) else { return };
        let changed = match self.render.get_mut(r) {
            Some(n) => {
                let old = n.data.scroll;
                n.data.scroll = offset;
                old != offset
            }
            None => false,
        };
        if changed {
            self.mark_render_dirty(r);
        }
    }

    pub fn frame_size(&self) -> Size {
        self.frame_size
    }

    fn run_flush(&mut self, root: Option<Node<M>>) {
        self.sched.borrow().guard_not_building("flush");

        // 0. Hand over anything that arrived from elsewhere since the last
        //    frame. Between frames, never during build, layout or paint.
        //
        //    Not on a geometry pass. That is between frames too, but it is not
        //    one: delivering here would put an arrival into the tree earlier
        //    for having been asked a question, and would advance every ticker
        //    once per question — making how often the host asks where
        //    something is visible in what the tree does.
        if !self.geometry_pass {
            self.pump_behaviors();
        }

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
            ids.sort_by_key(|&e| (self.arena.get(e).map(|el| el.depth).unwrap_or(0), e.idx));
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

    /// Settle focus and layout, and rebuild once more if either moved, so
    /// paint sees a tree that has finished moving.
    ///
    /// Autofocus happens after layout, because which scope is active depends on
    /// which layers resolved. A widget that mirrors focus reacts by marking
    /// itself, so without this second pass a focus ring would appear one frame
    /// late.
    ///
    /// **There are two kinds of dirt here and both have to be asked about.**
    /// The scheduler knows what still needs *building*; the render tree knows
    /// what still needs *measuring*, and that list can be non-empty while the
    /// scheduler's is clear. A `layout_reader` rebuilds during the layout pass
    /// — and so does a `Component` inside one, which is how a windowed
    /// [`List`](crate::widgets::List) fills its rows — so the dirt its
    /// reconcile raises arrives after `drain_layout` has finished its own
    /// loop. Asking only the scheduler carried that dirt into the next frame
    /// and painted the subtree one frame behind the description it was built
    /// from: the editor's settings dialog kept the previous category selected
    /// until some later, unrelated event drew again.
    fn settle(&mut self, size: Size) {
        self.apply_autofocus();
        if self.sched.borrow().has_pending() || !self.layout_dirty.is_empty() {
            self.run_flush(None);
            self.flush_layout(size);
        }
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

    /// Give every registered behavior a chance to deliver what it received
    /// from outside the UI thread. A behavior that delivers something marks its
    /// element, so the rebuild happens in this same flush.
    fn pump_behaviors(&mut self) {
        let ids: Vec<ElementId> = self.behaviour_hosts.clone();
        for id in ids {
            let Some(el) = self.arena.get(id) else {
                continue;
            };
            if el.disposed {
                continue;
            }
            let behaviors = el.behaviors.clone();
            for b in behaviors {
                b.frame();
                b.pump();
            }
        }
        self.behaviour_hosts.retain(|id| {
            self.arena
                .get(*id)
                .is_some_and(|e| !e.disposed && !e.behaviors.is_empty())
        });
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
