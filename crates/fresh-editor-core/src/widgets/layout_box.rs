//! The per-panel layout-box tree — the geometry substrate for hit-tested
//! event dispatch (widget-framework v2, phase 3).
//!
//! Collection (`kinds::*::collect`) already flattens every widget subtree
//! to rows plus half a dozen side channels (`hits`, `scroll_regions`,
//! `overlays`, …), each of which re-derives a slice of geometry because
//! the renderer had no coordinate space of its own. This module gives the
//! renderer that coordinate space: every widget contributes one
//! [`LayoutBox`] carrying its panel-relative rectangle, stacking level,
//! and dispatch-relevant flags, and containers shift child boxes exactly
//! as they shift the existing column-addressed side channels
//! (`EmbedRect`/`ScrollRegion`).
//!
//! The tree is renderer-internal: rows remain the paint wire format, and
//! `HitArea` remains the click wire format for the web bridge. What the
//! tree adds is *structure* — parent links, document order, z — so mouse
//! dispatch can hit-test topmost-then-deepest and bubble along real
//! ancestor chains instead of hand-ordered ladders, and so the focus
//! ring can be derived instead of separately collected.
//!
//! Arena convention: boxes live in a flat `Vec` where **children precede
//! their parent** (each subtree pushes its children first, then its own
//! root — "root-last"). `parent` indices point into the same `Vec`.
//! Within one parent, children appear in document order. Use
//! [`document_order`] / [`hit_path`] rather than relying on raw indices.
//!
//! Coordinate space: `row`/`col` are panel-inner coordinates in rendered
//! rows and **display columns** (matching `EmbedRect::col_in_row`, not
//! `HitArea` byte offsets — byte offsets vary per row, columns do not).
//! The inline Row collapse path shifts columns by accumulated DISPLAY
//! width (the phase-3 column-justify work landed display-width-correct
//! shifting in `containers.rs`; the historical byte-length
//! approximation is gone).
//!
//! Tie-break note: this file carries TWO stacking scans with OPPOSITE
//! document-order tie-breaks, each right for its consumer —
//! [`hit_path`] (panel-local) breaks z/depth ties toward the LATER
//! sibling (later paints over earlier within a panel row), while
//! [`hit_stack`] (chrome arena) breaks toward the EARLIER box (within
//! a band, registry/push order IS precedence: specific targets are
//! pushed before their guards). Moving a surface between the two trees
//! means re-checking which rule it renders under.

/// Scroll state a scrollable box carries out of the render: the item
/// totals and window the scrollbar paints from and the wheel/drag
/// handlers clamp against. This used to be a separate `ScrollRegion`
/// side channel duplicating the box's rectangle; the payload now rides
/// the box itself — one geometry, one shift path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoxScroll {
    /// Total items (list/tree) or content lines (multi-line text).
    pub total: usize,
    /// Items/lines visible in the window.
    pub visible: usize,
    /// First visible item/line index.
    pub offset: usize,
}

/// One node in the panel's layout-box tree.
#[derive(Debug, Clone, PartialEq)]
pub struct LayoutBox {
    /// The widget's `key`, when the spec declared one. Keyed boxes are
    /// addressable by the runtime (focus, instance state, events).
    pub key: Option<String>,
    /// The spec kind tag (`"list"`, `"text"`, `"col"`, …) — the same
    /// vocabulary `HitArea::widget_kind` uses, plus container tags and
    /// the popup pseudo-kinds `"text_completions"` / `"dropdown_popup"`.
    pub kind: &'static str,
    /// Index of the parent box in the same arena; `None` for the root.
    pub parent: Option<usize>,
    /// Panel-inner top row of the box's rectangle.
    pub row: u32,
    /// Panel-inner left edge, in display columns.
    pub col: u32,
    /// Width in display columns.
    pub width: u32,
    /// Height in rendered rows.
    pub height: u32,
    /// Stacking level. 0 = base surface; overlay-promoted content is 1;
    /// the dropdown pop-over is 2. Hit-testing picks the highest `z`
    /// first, then the deepest box.
    pub z: u8,
    /// True when the box's final rectangle is resolved at paint time in
    /// *screen* coordinates (the dropdown pop-over, which flips above
    /// its anchor near the frame edge). `row`/`col` then hold the
    /// panel-relative anchor, and pointer dispatch must consult the
    /// paint-recorded rect instead of this box's extent.
    pub screen_space: bool,
    /// Participates in the Tab ring. Mirrors `collect_tabbable`'s rules
    /// (keyed, non-disabled, `focusable` where the spec has the flag).
    pub focusable: bool,
    /// Scroll container: a wheel event bubbling through this box is
    /// consumed here when the widget is not already at its bound.
    pub scrollable: bool,
    /// An opaque surface: a pointer event inside this box that no
    /// descendant consumes stops here rather than falling through to
    /// content beneath (today: overlay-promoted popup rows).
    pub pointer_opaque: bool,
    /// A focus scope boundary: Tab cycles among focusables inside the
    /// nearest enclosing trap instead of the whole panel.
    pub focus_trap: bool,
    /// Scroll window state, present on a scrollable box whose content
    /// overflows bookkeeping (always written by List/Tree/multi-line
    /// Text renders). The scrollbar painter and drag handlers read it
    /// with the box's own rectangle.
    pub scroll: Option<BoxScroll>,
}

impl LayoutBox {
    /// A box with the given tag and rectangle and every flag off.
    pub fn plain(kind: &'static str, row: u32, col: u32, width: u32, height: u32) -> Self {
        LayoutBox {
            key: None,
            kind,
            parent: None,
            row,
            col,
            width,
            height,
            z: 0,
            screen_space: false,
            focusable: false,
            scrollable: false,
            pointer_opaque: false,
            focus_trap: false,
            scroll: None,
        }
    }

    /// Whether the panel-space point sits inside this box's rectangle.
    /// Always false for `screen_space` boxes — their rectangle is not
    /// knowable until paint.
    pub fn contains(&self, row: u32, col: u32) -> bool {
        !self.screen_space
            && row >= self.row
            && row < self.row + self.height
            && col >= self.col
            && col < self.col + self.width
    }
}

/// Depth of a box (root = 0), by walking parent links.
fn depth(boxes: &[LayoutBox], mut idx: usize) -> usize {
    let mut d = 0;
    while let Some(p) = boxes[idx].parent {
        d += 1;
        idx = p;
        debug_assert!(d <= boxes.len(), "parent cycle in layout-box arena");
    }
    d
}

/// The ancestor chain of `idx`, root first, `idx` last.
pub fn ancestor_path(boxes: &[LayoutBox], idx: usize) -> Vec<usize> {
    let mut path = vec![idx];
    let mut cur = idx;
    while let Some(p) = boxes[cur].parent {
        path.push(p);
        cur = p;
    }
    path.reverse();
    path
}

/// Hit-test the tree at a panel-space point: the target is the box
/// containing the point with the highest `z`, ties broken by depth
/// (deepest wins), then by later document position (a later sibling
/// paints over an earlier one). Returns the ancestor chain root→target,
/// or an empty Vec on a miss.
///
/// `screen_space` boxes never match here — a dropdown pop-over floats past
/// the panel's own rectangle, so it is not addressable in panel space. The
/// floating panel's click path used to resolve one against a paint-recorded
/// screen rect before reaching this; that path is deleted (S7) and the
/// remaining caller is the wheel routing over a *buffer-mounted* panel, where
/// the pop-over is the tree's `layer()` and the hit path resolves it.
pub fn hit_path(boxes: &[LayoutBox], row: u32, col: u32) -> Vec<usize> {
    let mut best: Option<(u8, usize, usize)> = None; // (z, depth, idx)
    for (idx, b) in boxes.iter().enumerate() {
        if !b.contains(row, col) {
            continue;
        }
        let cand = (b.z, depth(boxes, idx), idx);
        if best.is_none_or(|cur| cand >= cur) {
            best = Some(cand);
        }
    }
    match best {
        Some((_, _, idx)) => ancestor_path(boxes, idx),
        None => Vec::new(),
    }
}

/// Indices of all boxes in document order (pre-order over the tree,
/// siblings in insertion order). This is the order the focus ring uses.
pub fn document_order(boxes: &[LayoutBox]) -> Vec<usize> {
    // Children of each parent, in arena (insertion) order.
    let mut roots: Vec<usize> = Vec::new();
    let mut children: Vec<Vec<usize>> = vec![Vec::new(); boxes.len()];
    for (idx, b) in boxes.iter().enumerate() {
        match b.parent {
            Some(p) => children[p].push(idx),
            None => roots.push(idx),
        }
    }
    let mut order = Vec::with_capacity(boxes.len());
    let mut stack: Vec<usize> = roots.into_iter().rev().collect();
    while let Some(idx) = stack.pop() {
        order.push(idx);
        for &c in children[idx].iter().rev() {
            stack.push(c);
        }
    }
    order
}

/// Keys of focusable boxes in document order — the derived Tab ring.
pub fn focus_ring(boxes: &[LayoutBox]) -> Vec<String> {
    document_order(boxes)
        .into_iter()
        .filter(|&i| boxes[i].focusable)
        .filter_map(|i| boxes[i].key.clone())
        .collect()
}

// `focus_ring_scoped` was here — the Tab ring scoped to the nearest
// `focus_trap` ancestor of the focused box. It recovered two facts from the
// rectangles a paint left behind, `focusable` and `focus_trap`, and both are
// `box_meta`'s and so functions of the spec. Its one caller
// (`Editor::handle_widget_focus_advance`) now reads the ring off the tree's
// own registrations (`Ui::next_in`), which a panel the tree describes — and
// which therefore has no boxes at all — answers directly.

#[cfg(test)]
mod tests {
    use super::*;

    /// col(button, row(list, list)) shaped arena, root-last.
    fn sample() -> Vec<LayoutBox> {
        let mut button = LayoutBox::plain("button", 0, 0, 40, 1);
        button.key = Some("b".into());
        button.focusable = true;
        let mut list_a = LayoutBox::plain("list", 0, 0, 20, 5);
        list_a.key = Some("la".into());
        list_a.focusable = true;
        list_a.scrollable = true;
        let mut list_b = LayoutBox::plain("list", 0, 20, 20, 5);
        list_b.key = Some("lb".into());
        list_b.focusable = true;
        list_b.scrollable = true;
        let row = LayoutBox::plain("row", 1, 0, 40, 5);
        let col = LayoutBox::plain("col", 0, 0, 40, 6);
        // Arena: children before parent; parent links by index.
        // 0=button 1=list_a 2=list_b 3=row 4=col
        let mut v = vec![button, list_a, list_b, row, col];
        v[0].parent = Some(4);
        v[1].parent = Some(3);
        v[2].parent = Some(3);
        v[3].parent = Some(4);
        // Shift the row's children into panel space (the row starts at
        // row 1), the way collect_col's row_offset shift does.
        v[1].row = 1;
        v[2].row = 1;
        v
    }

    #[test]
    fn hit_path_finds_deepest_box() {
        let boxes = sample();
        // Point inside list_b: path is col -> row -> list_b.
        let path = hit_path(&boxes, 3, 25);
        assert_eq!(path, vec![4, 3, 2]);
        // Point on the button row.
        assert_eq!(hit_path(&boxes, 0, 5), vec![4, 0]);
        // Miss: below everything.
        assert!(hit_path(&boxes, 9, 5).is_empty());
    }

    #[test]
    fn hit_path_prefers_higher_z() {
        let mut boxes = sample();
        // An overlay row covering the lists' area at z=1.
        let mut popup = LayoutBox::plain("text_completions", 1, 0, 40, 3);
        popup.z = 1;
        popup.pointer_opaque = true;
        popup.parent = Some(0); // child of the button's text field, say
        boxes.push(popup);
        let path = hit_path(&boxes, 2, 25);
        assert_eq!(*path.last().unwrap(), 5, "overlay box wins over list_b");
    }

    #[test]
    fn screen_space_boxes_never_hit() {
        let mut boxes = sample();
        let mut pop = LayoutBox::plain("dropdown_popup", 0, 0, 40, 6);
        pop.screen_space = true;
        pop.z = 2;
        boxes.push(pop);
        assert_eq!(hit_path(&boxes, 0, 5), vec![4, 0], "anchor rect ignored");
    }

    #[test]
    fn document_order_is_preorder() {
        let boxes = sample();
        let order = document_order(&boxes);
        assert_eq!(order, vec![4, 0, 3, 1, 2]);
    }

    #[test]
    fn focus_ring_is_focusables_in_document_order() {
        let boxes = sample();
        assert_eq!(focus_ring(&boxes), vec!["b", "la", "lb"]);
    }
}
