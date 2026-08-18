//! The third tree: render objects.
//!
//! Descriptions are cheap and discarded; elements hold identity; render objects
//! hold the expensive, computed, retained data — geometry, cached measurements,
//! scroll offsets, focus registration, host handles. They are created rarely,
//! mutated constantly, and disposed when the component genuinely goes away.
//!
//! The render tree **skips** nodes that have no geometry of their own: a
//! `Component`, a `Provide` and a `Shared` wrapper contribute identity or data
//! but not rectangles, so a render node's children are the nearest render
//! descendants of its element, not that element's children.
//!
//! `RenderObject` is not generic over the message type. Layout, paint and
//! hit-testing never see a message; that is what lets a host supply one.

use std::rc::Rc;

use crate::element::ElementId;

use super::geom::{Constraints, Point, Rect, Size};
use super::spec::DrawList;
use super::Sizing;

/// A handle into the render arena.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RenderId(pub(crate) u32);

impl std::fmt::Debug for RenderId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R{}", self.0)
    }
}

/// Where a render object ended up: its absolute rectangle and the clip it
/// inherited. Handed to `paint` so an implementation never reads back from the
/// arena.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Geom {
    pub rect: Rect,
    pub clip: Rect,
}

/// What a render object does with a point inside its own rectangle.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum Hit {
    /// The hit stops here.
    #[default]
    Opaque,
    /// This node is not hit, but it takes part in the path and the search
    /// continues behind it.
    Transparent,
    /// Neither this node nor anything below it is hittable.
    Ignore,
}

/// What a container may do to its children while laying itself out.
///
/// Deliberately not generic: an implementation can measure and place its
/// children and nothing else. It cannot reach the element tree, read another
/// subtree's geometry, or mark anything dirty.
pub trait LayoutCx {
    /// This node's render children, in tree order.
    fn children(&self) -> Vec<RenderId>;
    /// The size request a child carries, resolved through nodes with no
    /// geometry of their own.
    fn sizing(&self, child: RenderId) -> (Sizing, Sizing);
    /// Measure a child. Returns its size, honouring the layout cache.
    fn measure(&mut self, child: RenderId, c: Constraints) -> Size;
    /// Position a child relative to this node's content origin.
    fn place(&mut self, child: RenderId, at: Point);
    /// Fill in the window of the nearest enclosing `Viewport`, if any.
    fn enclosing_window(&self, info: LayoutInfo) -> LayoutInfo;
    /// This node's scroll offset. Framework-owned: neither the application nor
    /// the component declares it.
    fn scroll(&self) -> Point;
    /// Declare the window this node shows onto its content. The framework
    /// clamps the offset against it, chains the wheel off it, and a
    /// constraint-dependent builder below reads it.
    fn set_scroll(&mut self, info: ScrollInfo);
    /// Run a structure builder that depends on the constraints, reconcile what
    /// it produced, and return the render children that resulted. The one place
    /// a build happens inside layout.
    fn rebuild(&mut self, info: LayoutInfo);
    /// The element this render object belongs to, for diagnostics.
    fn element(&self) -> ElementId;
}

/// What a scrolling node tells the framework about its window.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScrollInfo {
    /// The window, in the content's coordinates.
    pub window: Rect,
    /// The content behind it, in cells.
    pub content: Size,
    /// The furthest the offset may travel.
    pub max: Point,
    /// Whether children are moved by the offset. An index-scrolled window
    /// renders only what is inside it, so nothing is moved.
    pub translate: bool,
}

/// What a constraint-dependent builder is told.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LayoutInfo {
    pub constraints: Constraints,
    /// The window the nearest enclosing `Viewport` is showing, in its content's
    /// coordinates. `None` outside one.
    pub scroll_window: Option<Rect>,
}

/// Computed, retained geometry and the behaviour that produces it.
pub trait RenderObject {
    /// Constraints down, size up. Position children with `cx.place`.
    fn layout(&mut self, c: Constraints, cx: &mut dyn LayoutCx) -> Size;

    /// Emit display-list items. Never reads geometry back: everything this
    /// needs arrives in `g`.
    fn paint(&self, g: Geom, out: &mut DrawList) {
        let _ = (g, out);
    }

    /// What happens to a point inside this node's rectangle.
    fn hit(&self, local: Point) -> Hit {
        let _ = local;
        Hit::Opaque
    }

    /// A node whose size cannot change as a result of anything below it. The
    /// framework also treats tight constraints as a boundary; this is for
    /// implementations that know more than the constraints say.
    fn relayout_boundary(&self) -> bool {
        false
    }

    /// Whether this node bounds what its descendants may paint and hit.
    fn clips(&self) -> bool {
        false
    }

    /// Whether this node takes part in its parent's flow. A layer does not: it
    /// needs its anchor's rectangle, which does not exist until the main walk
    /// is done, so the framework holds it back for the second stage.
    fn out_of_flow(&self) -> bool {
        false
    }

    /// For the element dump.
    fn render_name(&self) -> &'static str {
        "RenderObject"
    }

    /// So the framework can push changed props into a live object rather than
    /// replacing it, which would discard its retained state.
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any;

    /// Whether this object consumes raw host input — a PTY grid does. Answered
    /// by host leaves; every built-in primitive says no.
    fn takes_raw_input(&self) -> bool {
        false
    }
}

/// Host content: application-owned drawing with its own measurement and
/// hit-testing.
///
/// This is the escape hatch, and it is an ordinary `RenderObject`: a host leaf
/// has exactly the capabilities a built-in primitive has, and no others.
pub trait HostLeaf: RenderObject {}

/// One node of the render tree.
pub(crate) struct RenderNode {
    /// `None` only while the object is checked out for `layout`.
    pub obj: Option<Box<dyn RenderObject>>,
    pub element: ElementId,
    pub parent: Option<RenderId>,
    pub children: Vec<RenderId>,
    /// The size request from the description chain above this node.
    pub w: Sizing,
    pub h: Sizing,
    /// Cached from the object so the framework can ask while the object itself
    /// is checked out for `layout`.
    pub clips: bool,
    pub out_of_flow: bool,
    /// Provenance and identity, resolved through the elements with no geometry
    /// between this node and its render parent.
    pub theme: Option<Rc<str>>,
    pub key: Option<crate::key::Key>,
    pub data: RenderData,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct RenderData {
    /// Offset from the parent's content origin. Written by the parent.
    pub offset: Point,
    /// Absolute position and size, after `arrange`.
    pub rect: Rect,
    /// All ancestor clips intersected.
    pub clip: Rect,
    pub size: Size,

    /// This node's own geometry is stale.
    pub needs_layout: bool,
    /// Something below is stale. Path-marked, so the top-down walk skips any
    /// subtree carrying neither bit without visiting it.
    pub child_needs_layout: bool,
    /// The constraints of the last layout and its result. A clean node handed
    /// equal constraints returns this without visiting its subtree.
    pub cached: Option<(Constraints, Size)>,
    /// The last layout ran with tight constraints, so nothing below can change
    /// this node's size.
    pub boundary: bool,
    /// The window this node shows onto its content, if it shows one.
    pub scroll: Point,
    pub content: Size,
    pub window: Option<Rect>,
    pub scroll_max: Point,
    pub translate: bool,

    /// How many times this node has been measured, and how many of those were a
    /// second measurement of the same subtree in one frame. Intrinsic sizing is
    /// the ergonomic default, so its cost has to be visible.
    pub layouts: u32,
    pub remeasures: u32,
    /// The frame this node was last measured in.
    pub measured_in: u64,
}

impl RenderData {
    pub fn fresh() -> Self {
        RenderData {
            needs_layout: true,
            ..RenderData::default()
        }
    }
}

#[derive(Default)]
pub(crate) struct RenderArena {
    slots: Vec<Option<RenderNode>>,
    free: Vec<u32>,
}

impl RenderArena {
    pub fn alloc(&mut self, node: RenderNode) -> RenderId {
        match self.free.pop() {
            Some(i) => {
                self.slots[i as usize] = Some(node);
                RenderId(i)
            }
            None => {
                self.slots.push(Some(node));
                RenderId(self.slots.len() as u32 - 1)
            }
        }
    }

    pub fn release(&mut self, id: RenderId) -> Option<RenderNode> {
        let n = self.slots.get_mut(id.0 as usize)?.take();
        if n.is_some() {
            self.free.push(id.0);
        }
        n
    }

    pub fn get(&self, id: RenderId) -> Option<&RenderNode> {
        self.slots.get(id.0 as usize).and_then(|s| s.as_ref())
    }

    pub fn get_mut(&mut self, id: RenderId) -> Option<&mut RenderNode> {
        self.slots.get_mut(id.0 as usize).and_then(|s| s.as_mut())
    }

    pub fn capacity(&self) -> usize {
        self.slots.len()
    }
}

impl std::ops::Index<RenderId> for RenderArena {
    type Output = RenderNode;
    fn index(&self, id: RenderId) -> &RenderNode {
        self.get(id).expect("render id is not live")
    }
}

/// A host leaf that only needs a rectangle: it fills what it is given and the
/// backend draws it. The general form is [`HostLeaf`]; this is the convenience.
pub struct PlainHost {
    pub id: crate::desc::HostId,
}

impl RenderObject for PlainHost {
    fn layout(&mut self, c: Constraints, _cx: &mut dyn LayoutCx) -> Size {
        c.constrain(c.max())
    }

    fn paint(&self, g: Geom, out: &mut DrawList) {
        out.push(super::spec::Draw::Host(self.id), g);
    }

    fn render_name(&self) -> &'static str {
        "Host"
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl HostLeaf for PlainHost {}

/// So a description can carry either form.
pub type HostObject = Rc<dyn Fn() -> Box<dyn HostLeaf>>;
