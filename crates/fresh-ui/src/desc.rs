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

/// Payload delivered to a handler.
///
/// The fields are defined by the pointer phase (L5) and the focus phase (L6).
/// The type exists now so that handler signatures do not change when they land.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[non_exhaustive]
pub struct Event {}

/// A listener. Returns a message for the application, or nothing.
pub type Handler<M> = Rc<dyn Fn(&Event) -> Option<M>>;

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
    Layer(LayerProps),
    /// Foreign content owned by the host: a buffer split, a PTY grid.
    Host(HostId),
    /// Makes an ambient value visible to everything below it (§ambient). Not a
    /// primitive: it has no render object and contributes no geometry.
    Provide(ProvideProps),
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Align {
    #[default]
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
}

/// Whether a gesture region absorbs pointer hits that land on it.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum PointerMode {
    /// Hits stop here. The default: a region that draws is a region that hits.
    #[default]
    Opaque,
    /// Hits fall through to whatever is behind.
    Transparent,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum GestureKind {
    Click,
    SecondaryClick,
    Press,
    Release,
    Move,
    Enter,
    Leave,
    Wheel,
}

pub struct GestureProps<M> {
    pub mode: PointerMode,
    pub listeners: Vec<(GestureKind, Handler<M>)>,
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
    /// Opens a focus scope: traversal is trapped inside while it is active.
    pub scope: bool,
    /// Explicit traversal position; `None` means reading order.
    pub ordinal: Option<i32>,
    /// Reachable by pointer but skipped by traversal.
    pub skip: bool,
    pub on_key: Vec<Handler<M>>,
}

impl<M> Default for FocusProps<M> {
    fn default() -> Self {
        FocusProps {
            autofocus: false,
            scope: false,
            ordinal: None,
            skip: false,
            on_key: Vec::new(),
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

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct LayerProps {
    pub anchor: Anchor,
    pub place: Place,
    pub fit: Fit,
    pub modality: Modality,
    pub scrim: Option<Scrim>,
    pub dismiss: Dismiss,
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

    pub fn on(mut self, kind: GestureKind, h: Handler<M>) -> Self {
        self.gesture_props().listeners.push((kind, h));
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

    fn layer_props(&mut self) -> &mut LayerProps {
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
}

/// A build context accepts any component as a node.
pub trait ComponentExt<M>: Component<M> + Sized {
    fn node(self) -> Node<M> {
        Node::component(self)
    }
}

impl<M, C: Component<M>> ComponentExt<M> for C {}
