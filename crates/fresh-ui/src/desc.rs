//! Descriptions: the immutable input to reconciliation.
//!
//! A description tree is a pure function of state. Constructing one has no side
//! effects: no platform resources, no registration, no mutation of anything
//! outside itself. Descriptions are values with constructor arguments only, and
//! they never hand a durable reference back to the caller.
//!
//! ## Naming deviation from the design document
//!
//! The design document spells the container constructor `Box::col()`. A type
//! named `Box` in scope shadows `std::boxed::Box` for every user of the crate,
//! so the constructors here are the free functions [`col`], [`row`] and
//! [`stack`]. The description variant keeps the name [`Desc::Box`].

use std::any::TypeId;
use std::rc::Rc;

use crate::ambient::{AmbientKey, ProvideProps};
use crate::component::{AnyComponent, Component};
use crate::key::Key;

pub use crate::event::{Event, GestureKind};

/// A listener. Returns a message for the application, or nothing.
pub type Handler<M> = Rc<dyn Fn(&Event) -> Option<M>>;

/// One registration on a `Gesture`.
pub struct Listener<M> {
    pub kind: GestureKind,
    /// Run on the way down (root to target) rather than on the way up.
    pub capture: bool,
    pub handler: Handler<M>,
}

impl<M> Clone for Listener<M> {
    fn clone(&self) -> Self {
        Listener {
            kind: self.kind,
            capture: self.capture,
            handler: self.handler.clone(),
        }
    }
}

// ---------------------------------------------------------------------------
// Node
// ---------------------------------------------------------------------------

/// One description. Immutable once built.
pub struct Node<M> {
    pub key: Option<Key>,
    /// Cross-axis and main-axis sizing requested by the *parent* of this node.
    /// Sizing is a node-level attribute rather than a `Box` property because
    /// any node can be given an extent by its container, including a
    /// `Component` that does not itself lay anything out.
    pub w: Sizing,
    pub h: Sizing,
    /// Per-item provenance for the display list. Inherited by descendants that
    /// do not set their own. The library never interprets it.
    pub theme: Option<Rc<str>>,
    pub desc: Desc<M>,
    pub children: Vec<Node<M>>,
}

/// The description variants.
pub enum Desc<M> {
    /// Constraint layout: row / column / stack, padding, gap, border.
    Box(BoxProps),
    /// Leaf paint.
    TextRun(TextProps),
    /// Clip plus scroll offset.
    Viewport(ViewportProps),
    /// Pointer region with listeners.
    Gesture(GestureProps<M>),
    /// Focus registration with key listeners.
    Focusable(FocusProps<M>),
    /// Out-of-flow content and a stacking context.
    Layer(LayerProps<M>),
    /// Foreign content owned by the host: a buffer split, a PTY grid.
    Host(HostId),
    /// Makes an ambient value visible to everything below it (§ambient). Not a
    /// primitive: it has no render object and contributes no geometry.
    Provide(ProvideProps),
    /// Structure that depends on the incoming constraints. Its builder runs
    /// during the layout pass, with the constraints as an argument, so the
    /// dependency is scoped to this node and evaluated in the right pass
    /// instead of becoming a build/layout cycle or a one-frame lag.
    LayoutReader(LayoutReaderProps<M>),
    /// A subtree the reconciler skips when the instance is unchanged.
    Shared(Rc<Node<M>>),
    /// Composition: builds a subtree from props and state.
    Component(Rc<dyn AnyComponent<M>>),
}

/// What the reconciler matches on, together with the key.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum ElemType {
    Box,
    TextRun,
    Viewport,
    Gesture,
    Focusable,
    Layer,
    Host,
    /// The ambient's identity is part of the type, so swapping one ambient for
    /// another at the same position remounts instead of aliasing.
    Provide(AmbientKey),
    LayoutReader,
    Component(TypeId),
}

// ---------------------------------------------------------------------------
// Props
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Dir {
    Row,
    #[default]
    Col,
}

/// How a node asks to be sized along one axis.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Sizing {
    /// Whatever the content needs, within the incoming constraint.
    #[default]
    Auto,
    /// An exact extent in terminal cells.
    Cells(u16),
    /// A share of the remainder left after fixed children are placed.
    Flex(u16),
    /// A percentage of the incoming constraint, 0..=100.
    Pct(u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Pad {
    pub x: u16,
    pub y: u16,
}

/// Cross-axis placement of a container's children.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Align {
    /// Fill the cross axis, when the container's cross extent is definite.
    /// Where it is not, this behaves as `Start`.
    #[default]
    Stretch,
    Start,
    Center,
    End,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct BoxProps {
    pub dir: Dir,
    /// Children share the whole rect instead of being placed in sequence.
    pub stack: bool,
    pub pad: Pad,
    pub gap: u16,
    pub border: bool,
    pub align: Align,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct TextProps {
    pub text: Rc<str>,
    pub wrap: bool,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct ViewportProps {
    /// Framework-owned once mounted; this is the initial value only.
    pub scroll: (u16, u16),
    pub selectable: bool,
    pub max_h: Option<u16>,
    /// Emit a scrollbar item when the content exceeds the window.
    pub scrollbar: bool,
}

/// Whether a gesture region absorbs pointer hits that land on it.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum PointerMode {
    /// Hits stop here. The default: a region that draws is a region that hits.
    #[default]
    Opaque,
    /// This node's own area does not hit; its children still do, so a click
    /// that lands on the node itself falls through to whatever is behind.
    Transparent,
    /// Neither this node nor anything below it is hittable.
    Ignore,
}

pub struct GestureProps<M> {
    pub mode: PointerMode,
    pub listeners: Vec<Listener<M>>,
}

impl<M> Default for GestureProps<M> {
    fn default() -> Self {
        GestureProps {
            mode: PointerMode::default(),
            listeners: Vec::new(),
        }
    }
}

pub struct FocusProps<M> {
    pub autofocus: bool,
    /// Opens a focus scope: traversal is confined inside while it is active.
    pub scope: bool,
    /// Explicit traversal position; `None` means reading order.
    pub ordinal: Option<i32>,
    /// Reachable by pointer but skipped by traversal.
    pub skip: bool,
    /// Raw key listeners, offered before intents are resolved.
    pub on_key: Vec<Handler<M>>,
    /// Key chords this subtree reads differently from the global map.
    pub shortcuts: Vec<crate::focus::Shortcut>,
    /// How *this* part of the interface carries out an intent.
    pub actions: Vec<(crate::focus::Intent, Handler<M>)>,
    /// Called with `FocusGained` / `FocusLost`. A component that draws a focus
    /// indicator mirrors focus through this; focus itself is not component
    /// state.
    pub on_focus_change: Option<Handler<M>>,
    /// Rebuild this element when focus enters or leaves its subtree.
    pub focus_within: bool,
}

impl<M> Default for FocusProps<M> {
    fn default() -> Self {
        FocusProps {
            autofocus: false,
            scope: false,
            ordinal: None,
            skip: false,
            on_key: Vec::new(),
            shortcuts: Vec::new(),
            actions: Vec::new(),
            on_focus_change: None,
            focus_within: false,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub enum Anchor {
    #[default]
    Parent,
    Node(Key),
    Point(u16, u16),
    Screen(Align),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Place {
    Below,
    Above,
    RightOf,
    LeftOf,
    #[default]
    Over,
    Fill,
}

/// What to do when the placed rect does not fit the frame.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Fit {
    /// Try the opposite side.
    pub flip: bool,
    /// Pull back inside the frame.
    pub clamp: bool,
    /// Slide along the cross axis.
    pub shift: bool,
}

impl Fit {
    pub const FLIP: Fit = Fit {
        flip: true,
        clamp: false,
        shift: false,
    };
    pub const CLAMP: Fit = Fit {
        flip: false,
        clamp: true,
        shift: false,
    };
    pub const SHIFT: Fit = Fit {
        flip: false,
        clamp: false,
        shift: true,
    };

    pub const fn or(self, o: Fit) -> Fit {
        Fit {
            flip: self.flip | o.flip,
            clamp: self.clamp | o.clamp,
            shift: self.shift | o.shift,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Modality {
    /// Content underneath keeps pointer and keyboard.
    #[default]
    None,
    /// Content underneath is visible but takes no input.
    Inert,
    /// Content underneath takes no input and traversal cannot leave.
    Exclusive,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Scrim {
    Dim,
    Opaque,
}

/// What dismisses a layer.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Dismiss {
    pub outside_pointer: bool,
    pub escape: bool,
    pub any_key: bool,
    pub any_input: bool,
}

impl Dismiss {
    pub const NONE: Dismiss = Dismiss {
        outside_pointer: false,
        escape: false,
        any_key: false,
        any_input: false,
    };
    pub const OUTSIDE_POINTER: Dismiss = Dismiss {
        outside_pointer: true,
        ..Dismiss::NONE
    };
    pub const ESCAPE: Dismiss = Dismiss {
        escape: true,
        ..Dismiss::NONE
    };
    pub const ANY_KEY: Dismiss = Dismiss {
        any_key: true,
        ..Dismiss::NONE
    };
    pub const ANY_INPUT: Dismiss = Dismiss {
        any_input: true,
        ..Dismiss::NONE
    };

    pub const fn or(self, o: Dismiss) -> Dismiss {
        Dismiss {
            outside_pointer: self.outside_pointer | o.outside_pointer,
            escape: self.escape | o.escape,
            any_key: self.any_key | o.any_key,
            any_input: self.any_input | o.any_input,
        }
    }
}

pub struct LayerProps<M> {
    pub anchor: Anchor,
    pub place: Place,
    pub fit: Fit,
    pub modality: Modality,
    pub scrim: Option<Scrim>,
    pub dismiss: Dismiss,
    /// Fired when a declared dismissal condition is met. The layer is described
    /// by the application, so closing it is the application's move.
    pub on_dismiss: Option<Handler<M>>,
}

impl<M> Default for LayerProps<M> {
    fn default() -> Self {
        LayerProps {
            anchor: Anchor::default(),
            place: Place::default(),
            fit: Fit::default(),
            modality: Modality::default(),
            scrim: None,
            dismiss: Dismiss::default(),
            on_dismiss: None,
        }
    }
}

impl<M> Clone for LayerProps<M> {
    fn clone(&self) -> Self {
        LayerProps {
            anchor: self.anchor.clone(),
            place: self.place,
            fit: self.fit,
            modality: self.modality,
            scrim: self.scrim,
            dismiss: self.dismiss,
            on_dismiss: self.on_dismiss.clone(),
        }
    }
}

impl<M> LayerProps<M> {
    /// Whether two layer descriptions place their content differently. Only
    /// these fields matter to layout; the handler does not.
    pub fn geom_eq(&self, o: &Self) -> bool {
        self.anchor == o.anchor && self.place == o.place && self.fit == o.fit
    }
}

/// A builder that receives the constraints its node was given.
pub struct LayoutReaderProps<M> {
    #[allow(clippy::type_complexity)]
    pub build: Rc<dyn Fn(crate::render::geom::Constraints) -> Node<M>>,
}

impl<M> Clone for LayoutReaderProps<M> {
    fn clone(&self) -> Self {
        LayoutReaderProps {
            build: self.build.clone(),
        }
    }
}

/// An opaque handle to host-owned content. The library never interprets it;
/// the embedding application maps it to a buffer, a terminal grid, or whatever
/// else it renders itself.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct HostId(pub u64);

impl From<u64> for HostId {
    fn from(v: u64) -> Self {
        HostId(v)
    }
}

// ---------------------------------------------------------------------------
// Clone (manual: M is never required to be Clone)
// ---------------------------------------------------------------------------

impl<M> Clone for Node<M> {
    fn clone(&self) -> Self {
        Node {
            key: self.key.clone(),
            w: self.w,
            h: self.h,
            theme: self.theme.clone(),
            desc: self.desc.clone(),
            children: self.children.clone(),
        }
    }
}

impl<M> Clone for Desc<M> {
    fn clone(&self) -> Self {
        match self {
            Desc::Box(p) => Desc::Box(p.clone()),
            Desc::TextRun(p) => Desc::TextRun(p.clone()),
            Desc::Viewport(p) => Desc::Viewport(p.clone()),
            Desc::Gesture(p) => Desc::Gesture(p.clone()),
            Desc::Focusable(p) => Desc::Focusable(p.clone()),
            Desc::Layer(p) => Desc::Layer(p.clone()),
            Desc::Host(h) => Desc::Host(*h),
            Desc::Provide(p) => Desc::Provide(p.clone()),
            Desc::LayoutReader(p) => Desc::LayoutReader(p.clone()),
            Desc::Shared(n) => Desc::Shared(n.clone()),
            Desc::Component(c) => Desc::Component(c.clone()),
        }
    }
}

impl<M> Clone for GestureProps<M> {
    fn clone(&self) -> Self {
        GestureProps {
            mode: self.mode,
            listeners: self.listeners.clone(),
        }
    }
}

impl<M> Clone for FocusProps<M> {
    fn clone(&self) -> Self {
        FocusProps {
            autofocus: self.autofocus,
            scope: self.scope,
            ordinal: self.ordinal,
            skip: self.skip,
            on_key: self.on_key.clone(),
            shortcuts: self.shortcuts.clone(),
            actions: self.actions.clone(),
            on_focus_change: self.on_focus_change.clone(),
            focus_within: self.focus_within,
        }
    }
}

// ---------------------------------------------------------------------------
// Inspection
// ---------------------------------------------------------------------------

/// Follow `Shared` wrappers to the node that actually describes the element.
pub fn resolve<M>(n: &Node<M>) -> &Node<M> {
    let mut cur = n;
    while let Desc::Shared(inner) = &cur.desc {
        cur = &**inner;
    }
    cur
}

/// The `(type, name)` pair the reconciler matches on. `Shared` is transparent.
pub fn node_type<M>(n: &Node<M>) -> (ElemType, &'static str) {
    match &resolve(n).desc {
        Desc::Box(_) => (ElemType::Box, "Box"),
        Desc::TextRun(_) => (ElemType::TextRun, "TextRun"),
        Desc::Viewport(_) => (ElemType::Viewport, "Viewport"),
        Desc::Gesture(_) => (ElemType::Gesture, "Gesture"),
        Desc::Focusable(_) => (ElemType::Focusable, "Focusable"),
        Desc::Layer(_) => (ElemType::Layer, "Layer"),
        Desc::Host(_) => (ElemType::Host, "Host"),
        Desc::Provide(p) => (ElemType::Provide(p.key), "Provide"),
        Desc::LayoutReader(_) => (ElemType::LayoutReader, "LayoutReader"),
        Desc::Component(c) => (ElemType::Component(c.comp_type_id()), c.comp_name()),
        Desc::Shared(_) => unreachable!("resolve() removes Shared"),
    }
}

/// The key this node reconciles under: its own, or the wrapped node's.
pub fn node_key<M>(n: &Node<M>) -> Option<Key> {
    n.key.clone().or_else(|| resolve(n).key.clone())
}

pub(crate) fn component_of<M>(n: &Node<M>) -> Option<Rc<dyn AnyComponent<M>>> {
    match &resolve(n).desc {
        Desc::Component(c) => Some(c.clone()),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

impl<M> Node<M> {
    pub fn new(desc: Desc<M>) -> Node<M> {
        Node {
            key: None,
            w: Sizing::Auto,
            h: Sizing::Auto,
            theme: None,
            desc,
            children: Vec::new(),
        }
    }

    /// A node that occupies nothing. Used by `if_` when the condition is false.
    pub fn nil() -> Node<M> {
        Node {
            key: None,
            w: Sizing::Cells(0),
            h: Sizing::Cells(0),
            theme: None,
            desc: Desc::Box(BoxProps::default()),
            children: Vec::new(),
        }
    }

    pub fn component<C: Component<M>>(c: C) -> Node<M> {
        Node::new(Desc::Component(Rc::new(c)))
    }

    /// Wrap in `Shared` so the reconciler can skip the subtree while the same
    /// instance is handed back. Hold the `Rc` in a field and hand out
    /// [`shared_rc`] clones of it; a fresh `Rc` each frame skips nothing.
    pub fn shared(self) -> Node<M> {
        Node::new(Desc::Shared(Rc::new(self)))
    }
}

/// Re-use an existing shared subtree. The reconciler skips it while the
/// instance is unchanged; reference identity is the only skip rule.
pub fn shared_rc<M>(rc: Rc<Node<M>>) -> Node<M> {
    Node::new(Desc::Shared(rc))
}

/// A column: children placed top to bottom.
pub fn col<M>() -> Node<M> {
    Node::new(Desc::Box(BoxProps {
        dir: Dir::Col,
        ..BoxProps::default()
    }))
}

/// A row: children placed left to right.
pub fn row<M>() -> Node<M> {
    Node::new(Desc::Box(BoxProps {
        dir: Dir::Row,
        ..BoxProps::default()
    }))
}

/// A stack: every child gets the whole rect, painted in order.
pub fn stack<M>() -> Node<M> {
    Node::new(Desc::Box(BoxProps {
        stack: true,
        ..BoxProps::default()
    }))
}

pub fn text<M>(s: impl AsRef<str>) -> Node<M> {
    Node::new(Desc::TextRun(TextProps {
        text: Rc::from(s.as_ref()),
        wrap: false,
    }))
}

pub fn viewport<M>(child: Node<M>) -> Node<M> {
    let mut n = Node::new(Desc::Viewport(ViewportProps::default()));
    n.children.push(child);
    n
}

pub fn gesture<M>(child: Node<M>) -> Node<M> {
    let mut n = Node::new(Desc::Gesture(GestureProps::default()));
    n.children.push(child);
    n
}

pub fn focusable<M>(child: Node<M>) -> Node<M> {
    let mut n = Node::new(Desc::Focusable(FocusProps::default()));
    n.children.push(child);
    n
}

/// Build a subtree from the constraints this node is given.
///
/// The builder runs during layout, may run more than once per frame under
/// intrinsic sizing, and may not call `set_state`.
pub fn layout_reader<M: 'static>(
    f: impl Fn(crate::render::geom::Constraints) -> Node<M> + 'static,
) -> Node<M> {
    Node::new(Desc::LayoutReader(LayoutReaderProps { build: Rc::new(f) }))
}

pub fn layer<M>() -> Node<M> {
    Node::new(Desc::Layer(LayerProps::default()))
}

pub fn host<M>(id: impl Into<HostId>) -> Node<M> {
    Node::new(Desc::Host(id.into()))
}

// ---------------------------------------------------------------------------
// Builder methods
// ---------------------------------------------------------------------------

impl<M> Node<M> {
    pub fn key(mut self, k: impl Into<Key>) -> Self {
        self.key = Some(k.into());
        self
    }

    pub fn child(mut self, c: impl Into<Node<M>>) -> Self {
        self.children.push(c.into());
        self
    }

    pub fn children<I, N>(mut self, it: I) -> Self
    where
        I: IntoIterator<Item = N>,
        N: Into<Node<M>>,
    {
        self.children.extend(it.into_iter().map(Into::into));
        self
    }

    pub fn child_if(self, cond: bool, f: impl FnOnce() -> Node<M>) -> Self {
        if cond {
            self.child(f())
        } else {
            self
        }
    }

    pub fn child_if_some<T>(self, v: Option<T>, f: impl FnOnce(T) -> Node<M>) -> Self {
        match v {
            Some(v) => self.child(f(v)),
            None => self,
        }
    }

    /// Keep this node when `cond`, otherwise collapse it to [`Node::nil`].
    pub fn if_(self, cond: bool) -> Self {
        if cond {
            self
        } else {
            Node::nil()
        }
    }

    /// Tag this node and its descendants with a provenance name, which appears
    /// on every display-list item they produce.
    pub fn theme(mut self, name: impl AsRef<str>) -> Self {
        self.theme = Some(Rc::from(name.as_ref()));
        self
    }

    pub fn scrollbar(mut self) -> Self {
        match &mut self.desc {
            Desc::Viewport(p) => p.scrollbar = true,
            _ => panic!("scrollbar() applies to Viewport nodes only"),
        }
        self
    }

    pub fn w(mut self, s: Sizing) -> Self {
        self.w = s;
        self
    }

    pub fn h(mut self, s: Sizing) -> Self {
        self.h = s;
        self
    }

    /// Take a share of the parent's remaining main-axis extent.
    pub fn flex(mut self, n: u16) -> Self {
        self.w = Sizing::Flex(n);
        self.h = Sizing::Flex(n);
        self
    }

    fn box_props(&mut self) -> &mut BoxProps {
        match &mut self.desc {
            Desc::Box(p) => p,
            _ => panic!("this property applies to Box nodes only"),
        }
    }

    pub fn pad(mut self, x: u16, y: u16) -> Self {
        self.box_props().pad = Pad { x, y };
        self
    }

    pub fn gap(mut self, g: u16) -> Self {
        self.box_props().gap = g;
        self
    }

    pub fn border(mut self) -> Self {
        self.box_props().border = true;
        self
    }

    pub fn align(mut self, a: Align) -> Self {
        self.box_props().align = a;
        self
    }

    pub fn wrap(mut self) -> Self {
        match &mut self.desc {
            Desc::TextRun(p) => p.wrap = true,
            _ => panic!("wrap() applies to TextRun nodes only"),
        }
        self
    }

    fn gesture_props(&mut self) -> &mut GestureProps<M> {
        match &mut self.desc {
            Desc::Gesture(p) => p,
            _ => panic!("listeners apply to Gesture nodes only"),
        }
    }

    /// Listen on the way up: target first, then each ancestor.
    pub fn on(mut self, kind: GestureKind, h: Handler<M>) -> Self {
        self.gesture_props().listeners.push(Listener {
            kind,
            capture: false,
            handler: h,
        });
        self
    }

    /// Listen on the way down: each ancestor before the target.
    pub fn on_capture(mut self, kind: GestureKind, h: Handler<M>) -> Self {
        self.gesture_props().listeners.push(Listener {
            kind,
            capture: true,
            handler: h,
        });
        self
    }

    pub fn on_click(self, f: impl Fn(&Event) -> M + 'static) -> Self {
        self.on(GestureKind::Click, Rc::new(move |e| Some(f(e))))
    }

    pub fn on_secondary_click(self, f: impl Fn(&Event) -> M + 'static) -> Self {
        self.on(GestureKind::SecondaryClick, Rc::new(move |e| Some(f(e))))
    }

    pub fn pointer_mode(mut self, m: PointerMode) -> Self {
        self.gesture_props().mode = m;
        self
    }

    fn focus_props(&mut self) -> &mut FocusProps<M> {
        match &mut self.desc {
            Desc::Focusable(p) => p,
            _ => panic!("focus properties apply to Focusable nodes only"),
        }
    }

    pub fn autofocus(mut self) -> Self {
        self.focus_props().autofocus = true;
        self
    }

    pub fn focus_scope(mut self) -> Self {
        self.focus_props().scope = true;
        self
    }

    pub fn ordinal(mut self, n: i32) -> Self {
        self.focus_props().ordinal = Some(n);
        self
    }

    /// Reachable by pointer, skipped by traversal.
    pub fn skip_traversal(mut self) -> Self {
        self.focus_props().skip = true;
        self
    }

    /// Rebuild when focus enters or leaves this subtree.
    pub fn focus_within(mut self) -> Self {
        self.focus_props().focus_within = true;
        self
    }

    pub fn on_key(mut self, f: impl Fn(&Event) -> Option<M> + 'static) -> Self {
        self.focus_props().on_key.push(Rc::new(f));
        self
    }

    pub fn on_focus_change(mut self, f: impl Fn(&Event) -> Option<M> + 'static) -> Self {
        self.focus_props().on_focus_change = Some(Rc::new(f));
        self
    }

    /// Read a chord as an intent, for this subtree only.
    pub fn shortcut(mut self, key: crate::event::KeyPress, intent: crate::focus::Intent) -> Self {
        self.focus_props()
            .shortcuts
            .push(crate::focus::Shortcut::new(key, intent));
        self
    }

    /// How this part of the interface carries out an intent.
    pub fn action(
        mut self,
        intent: crate::focus::Intent,
        f: impl Fn(&Event) -> M + 'static,
    ) -> Self {
        self.focus_props()
            .actions
            .push((intent, Rc::new(move |e| Some(f(e)))));
        self
    }

    /// As `action`, for a handler that may decline to produce a message.
    pub fn action_handler(mut self, intent: crate::focus::Intent, h: Handler<M>) -> Self {
        self.focus_props().actions.push((intent, h));
        self
    }

    pub fn on_key_handler(mut self, h: Handler<M>) -> Self {
        self.focus_props().on_key.push(h);
        self
    }

    pub fn on_focus_handler(mut self, h: Handler<M>) -> Self {
        self.focus_props().on_focus_change = Some(h);
        self
    }

    fn layer_props(&mut self) -> &mut LayerProps<M> {
        match &mut self.desc {
            Desc::Layer(p) => p,
            _ => panic!("layer properties apply to Layer nodes only"),
        }
    }

    pub fn anchor(mut self, a: Anchor) -> Self {
        self.layer_props().anchor = a;
        self
    }

    pub fn place(mut self, p: Place) -> Self {
        self.layer_props().place = p;
        self
    }

    pub fn fit(mut self, f: Fit) -> Self {
        self.layer_props().fit = f;
        self
    }

    pub fn modality(mut self, m: Modality) -> Self {
        self.layer_props().modality = m;
        self
    }

    pub fn scrim(mut self, s: Option<Scrim>) -> Self {
        self.layer_props().scrim = s;
        self
    }

    pub fn dismiss(mut self, d: Dismiss) -> Self {
        self.layer_props().dismiss = d;
        self
    }

    pub fn on_dismiss(mut self, f: impl Fn(&Event) -> M + 'static) -> Self {
        self.layer_props().on_dismiss = Some(Rc::new(move |e| Some(f(e))));
        self
    }

    /// As `on_dismiss`, for a handler that may decline to produce a message.
    pub fn on_dismiss_handler(mut self, h: Handler<M>) -> Self {
        self.layer_props().on_dismiss = Some(h);
        self
    }
}

/// A build context accepts any component as a node.
pub trait ComponentExt<M>: Component<M> + Sized {
    fn node(self) -> Node<M> {
        Node::component(self)
    }
}

impl<M, C: Component<M>> ComponentExt<M> for C {}
