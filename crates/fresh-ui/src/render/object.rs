//! Per-element geometry.
//!
//! The design describes render objects as a third tree. They are held here as a
//! field on the element instead: the correspondence is one-to-one and their
//! lifetimes are identical, and keeping one arena makes the two walks that need
//! both — retargeting at component boundaries, and focus registration — direct
//! rather than a join across two id spaces.

use super::geom::{Constraints, Point, Rect, Size};

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
    /// this node's size: a change stops here instead of walking to the root.
    pub boundary: bool,

    /// `Viewport` only: the scroll offset, and the size of the content behind
    /// the window. Framework-owned — neither the application nor the component
    /// declares it.
    pub scroll: Point,
    pub content: Size,

    /// How many times this node has been measured. Diagnostics: it is how a
    /// test shows that a change stopped at a relayout boundary.
    pub layouts: u32,
}

impl RenderData {
    pub fn fresh() -> Self {
        RenderData {
            needs_layout: true,
            ..RenderData::default()
        }
    }
}
