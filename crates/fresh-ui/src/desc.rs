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
    /// Floors on the resolved extent, in cells. `0` is "no floor".
    ///
    /// A node-level attribute for the same reason [`Node::w`] is: the extent a
    /// container hands a child is negotiated between them, and the floor is the
    /// child's half of that negotiation. It is what makes a *gap that must not
    /// vanish* expressible — `Sizing::Flex` says "take what is left", which is
    /// nothing when nothing is left, and a separator or a padding column
    /// usually means "take what is left, but never less than this".
    pub min_w: u16,
    pub min_h: u16,
    /// Who keeps its size when there is not enough room; higher yields last.
    /// See [`Node::priority`]. `0` for everything that does not say.
    pub priority: u8,
    /// Whether pointer hits stop at this node. `None` leaves it to the render
    /// object, which is [`PointerMode::Opaque`] for everything that has one.
    ///
    /// Node-level, again for the same reason: any node can stand in front of
    /// another, so any node can need to say it is not what the pointer meant —
    /// an overlay strip carrying a title, a spacer inside one, a decorative
    /// frame drawn over a list. Before this, only a `Gesture` could say it, so
    /// a transparent overlay had to be a gesture with no listeners wrapped
    /// around every container inside it, and even that did not work: the
    /// wrapper became transparent while the container it wrapped stayed
    /// opaque.
    pub pointer: Option<PointerMode>,
    /// Per-item provenance for the display list. Inherited by descendants that
    /// do not set their own. The library never interprets it.
    pub theme: Option<Rc<str>>,
    /// An owner's handle to this element, bound when it mounts.
    pub anchor: Option<Rc<crate::behavior::Anchor>>,
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
    Host(HostSpec),
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
    /// Bound what descendants may paint and hit to this box's *content* rect —
    /// inside the border ring and the padding.
    ///
    /// Off by default: a plain `row()` or `col()` is a layout grouping, and
    /// clipping every one of them would cost a bound per node for no gain.
    /// [`border`](Node::border) turns it on, because a frame its own content
    /// can paint over is not a frame.
    pub clip: bool,
}

/// How a run gives up cells it was not given.
///
/// Non-wrapping text measures at its natural width and is clipped to whatever
/// the parent allowed, which loses the fact that anything was cut. That was
/// tolerable while a host truncated its own strings first — but a host cannot,
/// once [`Node::priority`] lets layout decide a column's width: the width is
/// not known until measurement is over. So the run says how it yields, and the
/// end that survives is part of what it says.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Elide {
    /// Clip, silently. The default, and right for text whose overflow the
    /// enclosing box already explains.
    #[default]
    None,
    /// Keep the head, mark the cut at the end: a command name, a label, a
    /// message.
    Tail,
    /// Keep the tail, mark the cut at the start: a file path, whose filename
    /// is the part worth seeing.
    Head,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct TextProps {
    /// The run's content, in pieces. One piece for ordinary text; several when
    /// parts of it are styled differently.
    ///
    /// The pieces are one logical string: measurement, wrapping and truncation
    /// treat them as a unit, and a wrap point may fall inside a piece. That is
    /// the difference between this and laying separate `TextRun`s side by side,
    /// which wrap and truncate independently.
    pub runs: Rc<[Run]>,
    pub wrap: bool,
    /// Which end survives when the run is given less than it measured at.
    /// Ignored when `wrap` is set: wrapped text has no overflow to mark.
    pub elide: Elide,
    /// Where the text cursor sits within this run, in columns. Set by whatever
    /// is being edited; the library places it and the backend shows it.
    pub cursor: Option<u16>,
}

/// One piece of a text run, and the theme it paints in.
///
/// A piece with no theme of its own inherits the node's — so an unstyled run is
/// a single piece with `theme: None`, and adding styling means splitting the
/// content into more pieces rather than switching to a different mechanism.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Run {
    pub text: Rc<str>,
    pub theme: Option<crate::render::spec::ThemeKey>,
}

impl Run {
    /// A piece that paints in the node's own theme.
    pub fn plain(text: impl AsRef<str>) -> Run {
        Run {
            text: Rc::from(text.as_ref()),
            theme: None,
        }
    }

    /// A piece that paints in a theme of its own.
    pub fn themed(text: impl AsRef<str>, theme: impl AsRef<str>) -> Run {
        Run {
            text: Rc::from(text.as_ref()),
            theme: Some(crate::render::spec::ThemeKey(Some(Rc::from(
                theme.as_ref(),
            )))),
        }
    }
}

impl TextProps {
    /// The whole content as one string — what measurement and wrapping work on.
    pub fn plain(&self) -> String {
        self.runs.iter().map(|r| &*r.text).collect()
    }
}

/// What a viewport's scroll offset counts.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum ScrollMode {
    /// Cells of content. The viewport translates its children.
    #[default]
    Cells,
    /// Items. The child renders only the window, so nothing is translated; the
    /// offset is an index. This is what lets a window onto a million rows exist
    /// at all: a cell extent that large does not fit a coordinate.
    Items(u32),
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct ViewportProps {
    /// Framework-owned once mounted; this is the initial value only.
    pub scroll: (u16, u16),
    /// Mark the region as text-selectable in the display list. The library
    /// never interprets it — a backend that supports selection reads it, the
    /// same way it reads a theme name.
    pub selectable: bool,
    /// An upper bound on the window's height, whatever the constraints allow.
    pub max_h: Option<u16>,
    /// Emit a scrollbar item when the content exceeds the window.
    pub scrollbar: bool,
    pub mode: ScrollMode,
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
    pub listeners: Vec<Listener<M>>,
}

impl<M> Default for GestureProps<M> {
    fn default() -> Self {
        GestureProps {
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
    /// A position with no extent. A click happened *at* a coordinate, and a
    /// menu placed `Over` it starts there.
    Point(u16, u16),
    /// A single cell, which is what a caret or a hovered cell is.
    ///
    /// The distinction from [`Anchor::Point`] is the whole of it: a point
    /// resolves to a zero-size rect, so `Place::Below` a point is the point's
    /// own row. Below a *cell* is the row after it, and a flip clears the cell
    /// rather than landing on it. Both are correct for what they name — a
    /// click position has no extent, a caret occupies one — and a completion
    /// popup that opens on top of the character it is completing is the bug
    /// that comes of having only the first.
    Cell(u16, u16),
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
    /// How this layer sits against its anchor on the axis the placement does
    /// not use.
    ///
    /// A dropdown is as wide as the button it hangs off; a notification hangs
    /// off the *right* edge of the row above it. Both are a relationship to
    /// the anchor rather than a number, and neither is expressible otherwise:
    /// a layer measures against the whole frame, so `flex` or `Sizing::Grow`
    /// inside it reach the frame's edge, and a cell count is the caller
    /// measuring the anchor itself — the arithmetic anchoring exists to
    /// remove.
    ///
    /// `Align::Stretch` takes the anchor's whole extent, which is what
    /// [`Node::stretch_to_anchor`] spells; the others place a
    /// naturally-sized layer within it. `None` leaves the layer where the
    /// placement put it, which is the default and what every layer did before
    /// this existed.
    ///
    /// `Place::Fill` is the all-axes form and ignores this. `Above` and
    /// `Below` free the width; `LeftOf` and `RightOf` the height; `Over` has
    /// no free axis and is unaffected.
    pub align: Option<Align>,
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
            align: None,
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
            align: self.align,
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
        self.anchor == o.anchor
            && self.place == o.place
            && self.fit == o.fit
            && self.align == o.align
    }
}

/// A builder that receives the constraints its node was given.
pub struct LayoutReaderProps<M> {
    #[allow(clippy::type_complexity)]
    pub build: Rc<dyn Fn(crate::render::object::LayoutInfo) -> Node<M>>,
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

/// What a `Host` description carries.
///
/// The escape hatch of design goal 6, and it is an ordinary render object: a
/// host leaf has exactly the capabilities a built-in primitive has and no
/// others. `Plain` is the convenience for a host that only needs a rectangle.
#[derive(Clone)]
pub enum HostSpec {
    Plain(HostId),
    #[allow(clippy::type_complexity)]
    Leaf(Rc<dyn Fn() -> Box<dyn crate::render::object::HostLeaf>>),
}

impl HostSpec {
    pub(crate) fn make(&self) -> Box<dyn crate::render::object::RenderObject> {
        match self {
            HostSpec::Plain(id) => Box::new(crate::render::object::PlainHost { id: *id }),
            HostSpec::Leaf(f) => f(),
        }
    }
}

impl PartialEq for HostSpec {
    fn eq(&self, o: &Self) -> bool {
        match (self, o) {
            (HostSpec::Plain(a), HostSpec::Plain(b)) => a == b,
            (HostSpec::Leaf(a), HostSpec::Leaf(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for HostSpec {}

impl std::fmt::Debug for HostSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HostSpec::Plain(id) => write!(f, "Host({})", id.0),
            HostSpec::Leaf(_) => write!(f, "Host(leaf)"),
        }
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
            min_w: self.min_w,
            min_h: self.min_h,
            priority: self.priority,
            pointer: self.pointer,
            theme: self.theme.clone(),
            anchor: self.anchor.clone(),
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
            Desc::Host(h) => Desc::Host(h.clone()),
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
            min_w: 0,
            priority: 0,
            min_h: 0,
            pointer: None,
            theme: None,
            anchor: None,
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
            min_w: 0,
            priority: 0,
            min_h: 0,
            pointer: None,
            theme: None,
            anchor: None,
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
        runs: Rc::from(vec![Run::plain(s)]),
        wrap: false,
        elide: Elide::None,
        cursor: None,
    }))
}

/// A text run whose pieces are styled independently.
///
/// One logical string: it wraps and measures as a whole, and a wrap point may
/// fall inside a piece. Use this when the styling is *inside* the text —
/// a match highlight, a mnemonic, inline code in a sentence. When the pieces
/// are independently positioned instead, they are separate nodes in a `row()`,
/// which is layout rather than styling.
///
/// ```ignore
/// text_runs([Run::plain("Op"), Run::themed("e", "mnemonic"), Run::plain("n")])
/// ```
pub fn text_runs<M>(runs: impl IntoIterator<Item = Run>) -> Node<M> {
    Node::new(Desc::TextRun(TextProps {
        runs: Rc::from(runs.into_iter().collect::<Vec<_>>()),
        wrap: false,
        elide: Elide::None,
        cursor: None,
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
    f: impl Fn(crate::render::object::LayoutInfo) -> Node<M> + 'static,
) -> Node<M> {
    Node::new(Desc::LayoutReader(LayoutReaderProps { build: Rc::new(f) }))
}

pub fn layer<M>() -> Node<M> {
    Node::new(Desc::Layer(LayerProps::default()))
}

/// Content the backend draws, given a rectangle.
pub fn host<M>(id: impl Into<HostId>) -> Node<M> {
    Node::new(Desc::Host(HostSpec::Plain(id.into())))
}

/// Content the application measures, paints and hit-tests itself.
///
/// The factory runs once per mount, so the leaf may hold state for as long as
/// the element lives — the same terms every built-in primitive has.
pub fn host_leaf<M>(f: impl Fn() -> Box<dyn crate::render::object::HostLeaf> + 'static) -> Node<M> {
    Node::new(Desc::Host(HostSpec::Leaf(Rc::new(f))))
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

    /// Mark the region as text-selectable in the display list. The library
    /// holds no selection model; a backend that supports one reads this.
    pub fn selectable(mut self) -> Self {
        match &mut self.desc {
            Desc::Viewport(p) => p.selectable = true,
            _ => panic!("selectable() applies to Viewport nodes only"),
        }
        self
    }

    /// Bound the window's height, whatever the constraints allow.
    pub fn max_h(mut self, cells: u16) -> Self {
        match &mut self.desc {
            Desc::Viewport(p) => p.max_h = Some(cells),
            _ => panic!("max_h() applies to Viewport nodes only"),
        }
        self
    }

    /// Where the window starts. The initial value only: from the first layout
    /// on, the offset is framework-owned.
    pub fn scroll_at(mut self, x: u16, y: u16) -> Self {
        match &mut self.desc {
            Desc::Viewport(p) => p.scroll = (x, y),
            _ => panic!("scroll_at() applies to Viewport nodes only"),
        }
        self
    }

    /// Scroll by item index rather than by cell, over `count` items.
    pub fn items(mut self, count: u32) -> Self {
        match &mut self.desc {
            Desc::Viewport(p) => p.mode = ScrollMode::Items(count),
            _ => panic!("items() applies to Viewport nodes only"),
        }
        self
    }

    /// Let the owner address this element's window by handle.
    pub fn anchor_to(mut self, a: std::rc::Rc<crate::behavior::Anchor>) -> Self {
        self.anchor = Some(a);
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

    /// Never narrower than this, whatever the sizing resolves to.
    pub fn min_w(mut self, cells: u16) -> Self {
        self.min_w = cells;
        self
    }

    /// Who keeps its size when there is not enough room: **higher yields last.**
    ///
    /// A row resolves its non-flex children against the space that is left, in
    /// declaration order, so the first-declared child gets its full ask and the
    /// last one gets the remainder. That is *placement*, and placement is not
    /// precedence — there was no way to say "size the right-hand side first,
    /// and let the left take what remains" without doing the arithmetic outside
    /// layout and passing in the answer.
    ///
    /// Two surfaces had already done exactly that (the status bar's reserved
    /// right side, the suggestion row's four columns, where the name is sized
    /// before the description absorbs the squeeze), which is what says this is
    /// a missing concept rather than two special cases.
    ///
    /// Default `0`. Equal priorities keep declaration order, so a tree that
    /// never sets this lays out exactly as before.
    pub fn priority(mut self, p: u8) -> Self {
        self.priority = p;
        self
    }

    /// Never shorter than this.
    pub fn min_h(mut self, cells: u16) -> Self {
        self.min_h = cells;
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

    /// Draw a box outline just inside this node's rectangle, and bound what is
    /// inside it to the remaining content rect.
    ///
    /// The clip comes with the border on purpose. Content laid out in a
    /// bordered box can still be given a rectangle that reaches the ring — an
    /// unsatisfiable [`min_w`](Node::min_w) floor is the usual way — and
    /// without a bound it paints over the frame, turning a corner into a
    /// letter. Call [`clip(false)`](Node::clip) to keep the border and drop the
    /// bound.
    pub fn border(mut self) -> Self {
        let p = self.box_props();
        p.border = true;
        p.clip = true;
        self
    }

    /// Bound what descendants may paint and hit to this box's content rect.
    ///
    /// Implied by [`border`](Node::border); set it explicitly for an unbordered
    /// box whose children must not escape — a pane in a split grid, a cell in a
    /// table.
    pub fn clip(mut self, on: bool) -> Self {
        self.box_props().clip = on;
        self
    }

    pub fn align(mut self, a: Align) -> Self {
        self.box_props().align = a;
        self
    }

    /// Place the text cursor at this column within the run.
    pub fn cursor_at(mut self, col: u16) -> Self {
        match &mut self.desc {
            Desc::TextRun(p) => p.cursor = Some(col),
            _ => panic!("cursor_at() applies to TextRun nodes only"),
        }
        self
    }

    /// Which end of this run survives a width it did not ask for.
    ///
    /// See [`Elide`]. A no-op on anything but a text run, and on a wrapped one.
    pub fn elide(mut self, e: Elide) -> Self {
        if let Desc::TextRun(t) = &mut self.desc {
            t.elide = e;
        }
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

    /// The pointer entered this node. Fired on the node itself, not propagated —
    /// hover is a statement about one node. A component that draws a hover
    /// indicator mirrors this into its own state, the way a focus indicator
    /// mirrors `on_focus_change`.
    pub fn on_enter(self, h: Handler<M>) -> Self {
        self.on(GestureKind::Enter, h)
    }

    /// The pointer left this node. The partner of `on_enter`.
    pub fn on_leave(self, h: Handler<M>) -> Self {
        self.on(GestureKind::Leave, h)
    }

    /// Whether pointer hits stop here.
    ///
    /// Applies to any node, not only a `Gesture`: see [`Node::pointer`].
    pub fn pointer_mode(mut self, m: PointerMode) -> Self {
        self.pointer = Some(m);
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

    /// Match the anchor on the axis the placement leaves free.
    ///
    /// The `Align::Stretch` spelling of [`Node::align_to_anchor`], kept
    /// because "as wide as the thing it hangs off" reads better at the call
    /// site than an alignment does.
    pub fn stretch_to_anchor(self) -> Self {
        self.align_to_anchor(Align::Stretch)
    }

    /// How this layer sits against its anchor on the free axis. See
    /// [`LayerProps::align`].
    pub fn align_to_anchor(mut self, a: Align) -> Self {
        self.layer_props().align = Some(a);
        self
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

// ---------------------------------------------------------------------------
// Descriptions become render objects
// ---------------------------------------------------------------------------

/// Create if there is nothing there yet, otherwise push the changed props into
/// what is already there. Updating in place is what preserves retained state: a
/// viewport's scroll offset, a text run's wrapped rows, a host leaf's handle.
fn sync<T: crate::render::object::RenderObject + 'static>(
    obj: Option<&mut dyn crate::render::object::RenderObject>,
    make: impl FnOnce() -> T,
    update: impl FnOnce(&mut T),
) -> Option<Box<dyn crate::render::object::RenderObject>> {
    match obj {
        Some(o) => {
            if let Some(t) = o.as_any_mut().downcast_mut::<T>() {
                update(t);
            }
            None
        }
        None => Some(Box::new(make())),
    }
}

impl<M: 'static> Desc<M> {
    /// The one place in the library that knows which description produces which
    /// render object.
    ///
    /// Everything downstream — layout, paint, hit-testing, focus, input routing
    /// — asks the object rather than the description, so adding a primitive
    /// means adding a variant, a render object, and one arm here.
    pub(crate) fn sync_render(
        &self,
        obj: Option<&mut dyn crate::render::object::RenderObject>,
    ) -> Option<Box<dyn crate::render::object::RenderObject>> {
        use crate::render::object::FocusReg;
        use crate::render::prim::{
            BoxRender, FocusRender, GestureRender, LayerRender, ReaderRender, TextRender,
            ViewportRender,
        };
        match self {
            Desc::Box(p) => sync(
                obj,
                || BoxRender { props: p.clone() },
                |o| o.props = p.clone(),
            ),
            Desc::TextRun(p) => sync(obj, || TextRender::new(p.clone()), |o| o.props = p.clone()),
            Desc::Viewport(p) => sync(
                obj,
                || ViewportRender::new(p.clone()),
                |o| o.props = p.clone(),
            ),
            // A gesture is a pointer region and a listener list; whether it
            // absorbs a hit is `Node::pointer`, which any node carries.
            Desc::Gesture(_) => sync(obj, || GestureRender, |_| {}),
            Desc::Focusable(f) => {
                let reg = FocusReg {
                    ordinal: f.ordinal,
                    skip: f.skip,
                    scope: f.scope,
                    focus_within: f.focus_within,
                    autofocus: f.autofocus,
                };
                sync(obj, || FocusRender { reg }, |o| o.reg = reg)
            }
            Desc::Layer(l) => sync(
                obj,
                || LayerRender::from_props(l),
                |o| *o = LayerRender::from_props(l),
            ),
            // A host leaf is opaque to the library: it owns its own state and
            // there are no props to push into it.
            Desc::Host(h) => match obj {
                Some(_) => None,
                None => Some(h.make()),
            },
            // The builder is a new closure over new state every build, so a
            // description change has to re-run it even when the constraints are
            // unchanged. Only the framework knows the description changed.
            Desc::LayoutReader(_) => sync(obj, ReaderRender::default, |o: &mut ReaderRender| {
                o.invalidate()
            }),
            // Identity or data, but no geometry: the render tree skips these.
            Desc::Provide(_) | Desc::Shared(_) | Desc::Component(_) => None,
        }
    }
}

/// A node's own size request, looking through a `Shared` wrapper.
pub(crate) fn node_sizing<M>(n: &Node<M>) -> (Sizing, Sizing) {
    let inner = resolve(n);
    (
        if n.w == Sizing::Auto { inner.w } else { n.w },
        if n.h == Sizing::Auto { inner.h } else { n.h },
    )
}

/// Whether a description change can move anything.
///
/// Not "did the description change": a pointer mode, a key listener or a
/// dismissal rule changes what a node *does* without changing where anything
/// sits. Those reach the render object regardless; this decides only whether
/// the layout pass has to run again.
pub(crate) fn layout_relevant_changed<M>(old: &Node<M>, new: &Node<M>) -> bool {
    if node_sizing(old) != node_sizing(new) {
        return true;
    }
    match (&resolve(old).desc, &resolve(new).desc) {
        (Desc::Box(a), Desc::Box(b)) => a != b,
        (Desc::TextRun(a), Desc::TextRun(b)) => a != b,
        (Desc::Viewport(a), Desc::Viewport(b)) => a != b,
        (Desc::Layer(a), Desc::Layer(b)) => !a.geom_eq(b),
        (Desc::Host(a), Desc::Host(b)) => a != b,
        // Gesture, Focusable, Provide, LayoutReader and Component have no
        // geometry of their own; theirs comes from their children.
        _ => false,
    }
}
