//! The layout pass, over the render tree.
//!
//! Box constraints in integer cells: constraints propagate down, sizes
//! propagate up, parents position children. The algorithms live in the render
//! objects (`prim`); what is here is the framework around them — the tree, the
//! cache, the dirty bits, and the two stages.
//!
//! Two mechanisms bound the work. **Relayout boundaries** stop a change
//! propagating up: a node laid out under tight constraints cannot change size,
//! so a dirty node marks the path only as far as the nearest one, and the pass
//! re-enters there rather than at the root. The **constraint-keyed cache** stops
//! recomputation propagating down: a clean node handed constraints equal to its
//! last ones returns the stored size without visiting its subtree.

use std::rc::Rc;

use crate::desc::{node_sizing, resolve, Align, Anchor, Fit, Place, Sizing};
use crate::element::ElementId;
use crate::render::object::{LayoutCx, LayoutInfo, RenderId};
use crate::render::prim;
use crate::schedule::Ui;

use super::geom::{Constraints, Point, Rect, Size};

/// What relinking carries down from an element with no geometry of its own.
#[derive(Default, Clone, PartialEq)]
pub(crate) struct Carry {
    parent: Option<RenderId>,
    w: Sizing,
    h: Sizing,
    min_w: u16,
    min_h: u16,
    priority: u8,
    pointer: Option<crate::desc::PointerMode>,
    theme: Option<Rc<str>>,
    key: Option<crate::key::Key>,
    focus_parent: Option<crate::focus::FocusId>,
}

pub use prim::wrap_text;

// ---------------------------------------------------------------------------
// The layout context handed to a render object
// ---------------------------------------------------------------------------

pub(crate) struct UiLayoutCx<'a, M: 'static> {
    pub ui: &'a mut Ui<M>,
    pub node: RenderId,
}

impl<M: 'static> LayoutCx for UiLayoutCx<'_, M> {
    fn children(&self) -> Vec<RenderId> {
        self.ui.render[self.node]
            .children
            .iter()
            .copied()
            .filter(|c| !self.ui.render[*c].out_of_flow)
            .collect()
    }

    fn sizing(&self, child: RenderId) -> (Sizing, Sizing) {
        let n = &self.ui.render[child];
        (n.w, n.h)
    }

    fn floor(&self, child: RenderId) -> (u16, u16) {
        let n = &self.ui.render[child];
        (n.min_w, n.min_h)
    }

    fn priority(&self, child: RenderId) -> u8 {
        self.ui.render[child].priority
    }

    fn measure(&mut self, child: RenderId, c: Constraints) -> Size {
        self.ui.measure(child, c)
    }

    fn place(&mut self, child: RenderId, at: Point) {
        if let Some(n) = self.ui.render.get_mut(child) {
            n.data.offset = at;
        }
    }

    fn enclosing_window(&self, mut info: LayoutInfo) -> LayoutInfo {
        let mut cur = Some(self.node);
        while let Some(r) = cur {
            let n = &self.ui.render[r];
            if let Some(w) = n.data.window {
                info.scroll_window = Some(w);
                return info;
            }
            cur = n.parent;
        }
        info
    }

    fn scroll(&self) -> Point {
        self.ui.render[self.node].data.scroll
    }

    fn set_offset(&mut self, at: Point) {
        if let Some(n) = self.ui.render.get_mut(self.node) {
            n.data.scroll = at;
        }
    }

    fn set_scroll(&mut self, info: crate::render::object::ScrollInfo) {
        let moved = {
            let Some(n) = self.ui.render.get_mut(self.node) else {
                return;
            };
            let moved = n.data.window != Some(info.window);
            n.data.window = Some(info.window);
            n.data.content = info.content;
            n.data.scroll_max = info.max;
            n.data.translate = info.translate;
            moved
        };
        if moved {
            // The constraint cache is keyed by constraints, and a window is not
            // one: a builder that reads this window has to be told the window
            // moved, or it returns its last answer.
            self.ui.invalidate_window_readers(self.node);
        }
    }

    fn rebuild(&mut self, info: LayoutInfo) {
        self.ui.run_reader(self.node, info);
    }

    fn element(&self) -> ElementId {
        self.ui.render[self.node].element
    }
}

// ---------------------------------------------------------------------------
// The pass
// ---------------------------------------------------------------------------

impl<M: 'static> Ui<M> {
    /// A node whose geometry is stale. The mark travels up only as far as the
    /// nearest relayout boundary, which is where the next pass re-enters.
    pub(crate) fn mark_needs_layout(&mut self, element: ElementId) {
        let Some(r) = self.nearest_render(element) else {
            return;
        };
        self.mark_render_dirty(r);
    }

    pub(crate) fn mark_render_dirty(&mut self, r: RenderId) {
        let Some(n) = self.render.get_mut(r) else {
            return;
        };
        n.data.needs_layout = true;
        n.data.cached = None;

        let mut cur = n.parent;
        let mut boundary = r;
        while let Some(p) = cur {
            let Some(pn) = self.render.get_mut(p) else {
                break;
            };
            boundary = p;
            pn.data.child_needs_layout = true;
            if pn.data.boundary {
                break;
            }
            cur = pn.parent;
        }
        if !self.layout_dirty.contains(&boundary) {
            self.layout_dirty.push(boundary);
        }
    }

    /// The render node this element owns, or the nearest one above it.
    pub(crate) fn nearest_render(&self, element: ElementId) -> Option<RenderId> {
        let mut cur = Some(element);
        while let Some(e) = cur {
            let el = self.arena.get(e)?;
            if let Some(r) = el.render {
                return Some(r);
            }
            cur = el.parent;
        }
        None
    }

    /// The render node this element owns, or the first one below it. This is
    /// what a geometry query on a `Component` element answers with.
    pub(crate) fn render_for(&self, element: ElementId) -> Option<RenderId> {
        let el = self.arena.get(element)?;
        if let Some(r) = el.render {
            return Some(r);
        }
        el.children.iter().find_map(|c| self.render_for(*c))
    }

    /// The element a render node belongs to.
    pub(crate) fn element_of(&self, r: RenderId) -> Option<ElementId> {
        self.render.get(r).map(|n| n.element)
    }

    /// How an out-of-flow node places itself and what it does to the input
    /// behind it, asked of the render object. The second layout stage, paint,
    /// hit-testing and focus all go through this rather than matching on a
    /// description.
    pub(crate) fn layer_geom(&self, r: RenderId) -> Option<crate::render::object::LayerGeom> {
        self.render.get(r).and_then(|n| n.obj.as_ref())?.layer()
    }

    /// Rebuild the render and focus trees' links from the element tree.
    ///
    /// Render objects and focus registrations are created and disposed with
    /// their elements; only the links, the inherited size request and
    /// provenance are recomputed. A child list that actually changed marks the
    /// node for layout.
    pub(crate) fn relink(&mut self) {
        let Some(root) = self.root else {
            self.render_root = None;
            self.focus_roots.clear();
            return;
        };
        let mut roots = Vec::new();
        let mut focus_roots = Vec::new();
        self.relink_from(root, Carry::default(), &mut roots, &mut focus_roots);
        self.render_root = roots.first().copied();
        self.focus_roots = focus_roots;
    }

    pub(crate) fn relink_from_pub(
        &mut self,
        e: ElementId,
        parent: Option<RenderId>,
        theme: Option<Rc<str>>,
        out: &mut Vec<RenderId>,
    ) {
        let focus_parent = parent
            .and_then(|p| self.element_of(p))
            .and_then(|pe| self.nearest_focus(pe));
        let mut fout = Vec::new();
        self.relink_from(
            e,
            Carry {
                parent,
                theme,
                focus_parent,
                ..Carry::default()
            },
            out,
            &mut fout,
        );
        if let Some(fp) = focus_parent {
            if let Some(n) = self.focus_tree.get_mut(fp) {
                n.children = fout;
            }
        }
    }

    /// The focus registration at or above this element.
    fn nearest_focus(&self, e: ElementId) -> Option<crate::focus::FocusId> {
        let mut cur = Some(e);
        while let Some(x) = cur {
            let el = self.arena.get(x)?;
            if let Some(f) = el.focus {
                return Some(f);
            }
            cur = el.parent;
        }
        None
    }

    fn relink_from(
        &mut self,
        e: ElementId,
        carry: Carry,
        out: &mut Vec<RenderId>,
        fout: &mut Vec<crate::focus::FocusId>,
    ) {
        let Some(el) = self.arena.get(e) else { return };
        // Nothing in this subtree changed and it is being handed exactly what
        // it was handed last time, so it would recompute the links it already
        // has. What it contributed is replayed instead.
        if !el.link_dirty && el.last_carry.as_ref() == Some(&carry) {
            out.extend(el.last_out.iter().copied());
            fout.extend(el.last_fout.iter().copied());
            return;
        }
        let (o0, f0) = (out.len(), fout.len());
        self.relink_node(e, carry.clone(), out, fout);
        let (delta_out, delta_fout) = (out[o0..].to_vec(), fout[f0..].to_vec());
        if let Some(el) = self.arena.get_mut(e) {
            el.link_dirty = false;
            el.last_carry = Some(carry);
            el.last_out = delta_out;
            el.last_fout = delta_fout;
        }
    }

    fn relink_node(
        &mut self,
        e: ElementId,
        carry: Carry,
        out: &mut Vec<RenderId>,
        fout: &mut Vec<crate::focus::FocusId>,
    ) {
        let Some(el) = self.arena.get(e) else { return };
        let (nw, nh) = node_sizing(&el.desc);
        let w = if carry.w != Sizing::Auto { carry.w } else { nw };
        let h = if carry.h != Sizing::Auto { carry.h } else { nh };
        // Floors and pointer mode travel the same way sizing does: down from a
        // description that has no geometry of its own, onto the first node that
        // has. The outer one wins, because it is the one the caller wrote on
        // the wrapper it handed over.
        let min_w = carry.min_w.max(el.desc.min_w);
        let min_h = carry.min_h.max(el.desc.min_h);
        let priority = carry.priority.max(el.desc.priority);
        let pointer = carry.pointer.or(el.desc.pointer);
        let theme = el
            .desc
            .theme
            .clone()
            .or_else(|| resolve(&el.desc).theme.clone())
            .or(carry.theme);
        let key = el.key.clone().or(carry.key);
        let render = el.render;
        let focus = el.focus;
        let kids = el.children.clone();

        // The focus tree mirrors the render tree but is not identical to it:
        // only registrations and scopes appear.
        let focus_parent = match focus {
            Some(f) => {
                if let Some(n) = self.focus_tree.get_mut(f) {
                    n.parent = carry.focus_parent;
                }
                fout.push(f);
                Some(f)
            }
            None => carry.focus_parent,
        };

        match render {
            Some(r) => {
                {
                    let n = self.render.get_mut(r).expect("live render node");
                    n.parent = carry.parent;
                    n.w = w;
                    n.h = h;
                    n.min_w = min_w;
                    n.min_h = min_h;
                    n.priority = priority;
                    n.pointer = pointer;
                    n.theme = theme;
                    n.key = key;
                }
                let carried = self.render[r].theme.clone();
                let mut inner = Vec::new();
                let mut finner = Vec::new();
                for k in kids {
                    // Provenance is inherited; identity is not. A render child
                    // uses its own key, or that of a transparent element
                    // between it and this node.
                    self.relink_from(
                        k,
                        Carry {
                            parent: Some(r),
                            theme: carried.clone(),
                            focus_parent,
                            ..Carry::default()
                        },
                        &mut inner,
                        &mut finner,
                    );
                }
                if self.render[r].children != inner {
                    self.render.get_mut(r).expect("live").children = inner;
                    self.mark_render_dirty(r);
                }
                if let Some(f) = focus {
                    if let Some(n) = self.focus_tree.get_mut(f) {
                        n.children = finner;
                    }
                } else {
                    fout.extend(finner);
                }
                out.push(r);
            }
            None => {
                let mut finner = Vec::new();
                for k in kids {
                    self.relink_from(
                        k,
                        Carry {
                            parent: carry.parent,
                            w,
                            h,
                            min_w,
                            min_h,
                            priority,
                            pointer,
                            theme: theme.clone(),
                            key: key.clone(),
                            focus_parent,
                        },
                        out,
                        &mut finner,
                    );
                }
                if let Some(f) = focus {
                    if let Some(n) = self.focus_tree.get_mut(f) {
                        n.children = finner;
                    }
                } else {
                    fout.extend(finner);
                }
            }
        }
    }

    /// Tell the builders below this node that its window moved. Stops at a
    /// nested scrolling node, which publishes a window of its own.
    pub(crate) fn invalidate_window_readers(&mut self, r: RenderId) {
        let kids = match self.render.get(r) {
            Some(n) => n.children.clone(),
            None => return,
        };
        for k in kids {
            let (reads, clips) = match self.render.get(k) {
                Some(n) => (n.reads_window, n.clips),
                None => continue,
            };
            if reads {
                self.mark_render_dirty(k);
            }
            if !clips {
                self.invalidate_window_readers(k);
            }
        }
    }

    /// Measure one render node, honouring the cache.
    pub(crate) fn measure(&mut self, r: RenderId, c: Constraints) -> Size {
        {
            let n = &self.render[r];
            if !n.data.needs_layout && !n.data.child_needs_layout {
                if let Some((cc, sz)) = n.data.cached {
                    if cc == c {
                        return sz;
                    }
                }
            }
        }
        // A second measurement of the same node within one frame is the
        // intrinsic-sizing cost, and is counted rather than hidden.
        let again = self.render[r].data.measured_in == self.frame_no;
        let mut obj = self
            .render
            .get_mut(r)
            .expect("live render node")
            .obj
            .take()
            .expect("render object measured re-entrantly");

        let was_measuring = std::mem::replace(&mut self.measuring, true);
        let size = {
            let mut cx = UiLayoutCx { ui: self, node: r };
            obj.layout(c, &mut cx)
        };
        self.measuring = was_measuring;

        let boundary = c.is_tight() || obj.relayout_boundary();
        let n = self.render.get_mut(r).expect("live render node");
        n.obj = Some(obj);
        n.data.size = size;
        if n.clips {
            // Framework-owned: keep the window inside what the node said its
            // content is.
            n.data.scroll.x = n.data.scroll.x.clamp(0, n.data.scroll_max.x.max(0));
            n.data.scroll.y = n.data.scroll.y.clamp(0, n.data.scroll_max.y.max(0));
        }
        n.data.cached = Some((c, size));
        n.data.needs_layout = false;
        n.data.child_needs_layout = false;
        n.data.boundary = boundary;
        n.data.layouts += 1;
        n.data.measured_in = self.frame_no;
        if again {
            n.data.remeasures += 1;
        }
        // A node that was laid out may have changed its own size and moved
        // every child it placed, so its rectangles are computed again. Nothing
        // else in the tree needs to be.
        self.mark_arrange(r);
        size
    }

    /// Path-mark for the positioning walk, the same shape as the layout mark
    /// but without a boundary to stop at: a rectangle is absolute, so a node
    /// that moved moves everything below it.
    fn mark_arrange(&mut self, r: RenderId) {
        if let Some(n) = self.render.get_mut(r) {
            n.data.arrange_dirty = true;
        }
        let mut cur = self.render.get(r).and_then(|n| n.parent);
        while let Some(p) = cur {
            let Some(pn) = self.render.get_mut(p) else {
                break;
            };
            if pn.data.child_arrange_dirty {
                break;
            }
            pn.data.child_arrange_dirty = true;
            cur = pn.parent;
        }
    }

    /// Run layout, position everything, and resolve out-of-flow layers.
    pub(crate) fn flush_layout(&mut self, frame: Size) {
        self.frame_no = self.frame_no.wrapping_add(1);
        self.relink();
        let Some(root) = self.render_root else { return };

        let root_c = self.root_constraints(root, frame);
        let full = self.frame_size != frame || self.render[root].data.cached.is_none();
        if full {
            // The dirty list is deliberately *not* cleared here. A root
            // measure is not a whole-tree measure: path-marking stops at the
            // nearest relayout boundary, so the root walk short-circuits above
            // every boundary that was marked from below and leaves it holding
            // last frame's measurement — which for text is the shaped rows
            // paint reads. Dropping the list here painted a stale status bar
            // for the whole frame in which a layer was open, because the layer
            // dirtied the root. Re-entering the root from the drain afterwards
            // is a cache hit.
            self.measure(root, root_c);
        }

        self.drain_layout(root, root_c);

        self.frame_size = frame;

        // A constraint-dependent builder may have replaced part of its subtree.
        self.process_disposals();

        self.replace_layers(frame);

        // Commands an owner queued through a handle, applied once geometry
        // exists and before anything reads it.
        if self.apply_anchors() {
            // The layers found by the first walk are stale the moment anything
            // moves: re-arranging without clearing them would resolve, paint
            // and dismiss every one of them twice.
            self.replace_layers(frame);
        }

        self.refresh_geometry();
    }

    /// Arrange the tree and place every layer against the anchors as they
    /// stand.
    ///
    /// Re-runnable by construction, and run more than once already: a scroll
    /// command applied after the first walk moves content, which moves anything
    /// anchored to it. The layer list is rebuilt rather than reused because
    /// `arrange` appends to it — resolving the accumulated list would place,
    /// paint and dismiss every layer twice.
    pub(crate) fn replace_layers(&mut self, frame: Size) {
        let Some(root) = self.render_root else { return };
        self.pending_layers.clear();
        self.arrange(root, Point::ZERO, Rect::from_size(frame));
        self.resolve_layers(frame);
        self.process_disposals();
    }

    /// Update the geometry of every element somebody holds a handle to. The
    /// cost is the number of handles, not the size of the tree.
    fn refresh_geometry(&mut self) {
        let store = self.geom_store.clone();
        let ids: Vec<ElementId> = store.borrow().entries.keys().copied().collect();
        for id in ids {
            let g = self
                .render_for(id)
                .and_then(|r| self.render.get(r))
                .map(|n| crate::services::GeomSnapshot {
                    rect: n.data.rect,
                    clip: n.data.clip,
                    scroll: n.data.scroll,
                    content: n.data.content,
                })
                .unwrap_or_default();
            store.borrow_mut().entries.insert(id, g);
        }
    }

    /// Returns whether anything moved.
    fn apply_anchors(&mut self) -> bool {
        use crate::behavior::anchor::Command;
        let ids = self.anchored.clone();
        let mut moved = false;
        for id in ids {
            let Some(a) = self.arena.get(id).and_then(|e| e.desc.anchor.clone()) else {
                continue;
            };
            for cmd in a.take() {
                let Some(r) = self.render_for(id) else {
                    continue;
                };
                let (scroll, max, rows) = {
                    let n = &self.render[r];
                    (n.data.scroll, n.data.scroll_max, n.data.size.h as i32)
                };
                let next = match cmd {
                    Command::ScrollTo(p) => p,
                    Command::Reveal(i) => {
                        let i = i as i32;
                        // The shortest move that puts the index inside the
                        // window; nothing at all if it already is.
                        let y = if i < scroll.y {
                            i
                        } else if i >= scroll.y + rows {
                            i - rows + 1
                        } else {
                            scroll.y
                        };
                        Point::new(scroll.x, y)
                    }
                };
                let next = Point::new(next.x.clamp(0, max.x.max(0)), next.y.clamp(0, max.y.max(0)));
                if next != scroll {
                    if let Some(n) = self.render.get_mut(r) {
                        n.data.scroll = next;
                    }
                    self.mark_render_dirty(r);
                    moved = true;
                }
            }
        }
        if moved {
            // One more pass so the window the builders read is the one the
            // commands asked for.
            if let Some(root) = self.render_root {
                let root_c = self.root_constraints(root, self.frame_size);
                self.drain_layout(root, root_c);
            }
            self.process_disposals();
        }
        moved
    }

    /// Drain the dirty boundaries to a fixpoint.
    ///
    /// A constraint-dependent builder reconciles during the pass, and what it
    /// produces can dirty nodes the drain has already gone past; without the
    /// loop they would be measured one frame late.
    ///
    /// Every dirty boundary is re-entered, including one below another:
    /// path-marking stops at a boundary, so an ancestor's re-measure does not
    /// reach the boundaries beneath it. The constraint cache is what makes the
    /// overlapping case cheap.
    fn drain_layout(&mut self, root: RenderId, root_c: Constraints) {
        let mut rounds = 0;
        while !self.layout_dirty.is_empty() {
            let mut dirty = std::mem::take(&mut self.layout_dirty);
            dirty.sort_by_key(|&r| self.render_depth(r));
            for b in dirty {
                if self.render.get(b).is_none() {
                    continue;
                }
                let Some((c, _)) = self.render[b].data.cached else {
                    // No cache means no constraints to re-enter this boundary
                    // on, so the pass starts again from the root. That does
                    // *not* stand in for the rest of the list: path-marking
                    // stops at a boundary, so the root walk short-circuits
                    // above every other dirty boundary and leaves it stale.
                    // Each one is still visited — by then the root pass has
                    // usually refilled its cache, and if it has not, this
                    // measure is a cache hit that costs nothing.
                    self.measure(root, root_c);
                    continue;
                };
                self.measure(b, c);
            }
            rounds += 1;
            assert!(
                rounds < 16,
                "layout did not settle: a builder keeps dirtying what it produced"
            );
        }
    }

    fn render_depth(&self, mut r: RenderId) -> u32 {
        let mut d = 0;
        while let Some(p) = self.render.get(r).and_then(|n| n.parent) {
            d += 1;
            r = p;
        }
        d
    }

    /// The root is a child of the frame. `Auto` there means "fill", and an
    /// explicit request is honoured so a subtree can be measured on its own
    /// terms.
    fn root_constraints(&self, root: RenderId, frame: Size) -> Constraints {
        let n = &self.render[root];
        let axis = |s: Sizing, extent: u16| -> (u16, u16) {
            match s {
                Sizing::Cells(v) => {
                    let v = v.min(extent);
                    (v, v)
                }
                Sizing::Pct(p) => {
                    let v = prim::pct(extent, p);
                    (v, v)
                }
                Sizing::Auto | Sizing::Flex(_) => (extent, extent),
            }
        };
        let (min_w, max_w) = axis(n.w, frame.w);
        let (min_h, max_h) = axis(n.h, frame.h);
        Constraints::new(min_w, max_w, min_h, max_h)
    }

    // -- positioning ---------------------------------------------------------

    pub(crate) fn arrange(&mut self, r: RenderId, origin: Point, clip: Rect) {
        let (size, clips, clip_inset, translate, scroll, kids, dirty, was, cached) = {
            let Some(n) = self.render.get(r) else { return };
            (
                n.data.size,
                n.clips,
                n.clip_inset,
                n.data.translate,
                n.data.scroll,
                n.children.clone(),
                n.data.arrange_dirty || n.data.child_arrange_dirty,
                (n.data.rect, n.data.clip),
                n.data.layers.clone(),
            )
        };
        let rect = Rect::at(origin, size);
        // Nothing below was laid out and this node lands where it already is:
        // every rectangle in the subtree is the one it already has. The layers
        // it published are re-published so paint order does not depend on how
        // much work the pass did.
        if !dirty && was == (rect, clip) {
            self.pending_layers.extend(cached);
            return;
        }
        {
            let n = self.render.get_mut(r).expect("live");
            n.data.rect = rect;
            n.data.clip = clip;
            n.data.arrange_dirty = false;
            n.data.child_arrange_dirty = false;
        }
        let start = self.pending_layers.len();
        // A clipping node bounds its descendants at its *content* rect, not at
        // its outer edge: a bordered box owns the ring it drew, so content that
        // reaches it has escaped rather than arrived.
        let child_clip = if clips {
            clip.intersect(rect.deflate(clip_inset.0, clip_inset.1))
        } else {
            clip
        };
        let sc = if clips && translate {
            scroll
        } else {
            Point::ZERO
        };
        for k in kids {
            if self.render.get(k).map(|n| n.out_of_flow).unwrap_or(false) {
                self.pending_layers.push((k, r));
                continue;
            }
            let off = self.render[k].data.offset;
            self.arrange(
                k,
                Point::new(origin.x + off.x - sc.x, origin.y + off.y - sc.y),
                child_clip,
            );
        }
        let found = self.pending_layers[start..].to_vec();
        if let Some(n) = self.render.get_mut(r) {
            n.data.layers = found;
        }
    }

    /// Layers resolve after the main walk: one anchored to a node needs that
    /// node's rectangle, which does not exist until its subtree has laid out.
    /// `fit` is applied here, against the frame.
    fn resolve_layers(&mut self, frame: Size) {
        let mut i = 0;
        while i < self.pending_layers.len() {
            let (lr, parent) = self.pending_layers[i];
            i += 1;
            let Some(props) = self.layer_geom(lr) else {
                continue;
            };
            // Where this layer may go. The frame unless it named a region —
            // resolved the same way an `Anchor::Node` is, so a region the tree
            // does not contain yet can still be named.
            let bounds = props
                .within
                .as_ref()
                .and_then(|k| {
                    self.find_by_key(k)
                        .and_then(|e| self.render_for(e))
                        .map(|r| self.render[r].data.rect)
                        .or_else(|| self.host_anchors.get(k).copied())
                })
                .unwrap_or(Rect::from_size(frame));
            let anchor = match &props.anchor {
                Anchor::Parent => self.render[parent].data.rect,
                // An element carrying the key first; then a rectangle the
                // host published for it, for a thing that lives inside a host
                // leaf and has no element of its own; then the parent, which is
                // what a name nobody answers has always fallen back to.
                Anchor::Node(k) => self
                    .find_by_key(k)
                    .and_then(|e| self.render_for(e))
                    .map(|r| self.render[r].data.rect)
                    .or_else(|| self.host_anchors.get(k).copied())
                    .unwrap_or(self.render[parent].data.rect),
                // In `bounds`' space, origin included — the frame when the
                // layer named no region. A region "moves the origin as well as
                // the limit" for a screen anchor already; a point was the one
                // that did not, so a caller holding coordinates inside a panel
                // had to add the panel's origin itself, and that addition is
                // the second source of geometry naming a region removes.
                Anchor::Point(x, y) => Rect::new(bounds.x + *x as i32, bounds.y + *y as i32, 0, 0),
                // One cell, which is the whole difference: `Place::Below` a
                // cell is the row after it, while below a point is the point's
                // own row.
                Anchor::Cell(x, y) => Rect::new(bounds.x + *x as i32, bounds.y + *y as i32, 1, 1),
                Anchor::Screen(_) => bounds,
            };
            let c = if props.place == Place::Fill {
                Constraints::tight(anchor.size())
            } else if props.align == Some(Align::Stretch) {
                // Free axis pinned to the anchor, the other still free. What
                // makes a dropdown the width of its button without the caller
                // measuring the button.
                let a = anchor.size();
                match props.place {
                    Place::Above | Place::Below => Constraints::new(a.w, a.w, 0, bounds.h),
                    Place::LeftOf | Place::RightOf => Constraints::new(0, bounds.w, a.h, a.h),
                    Place::Over | Place::Fill => Constraints::loose(bounds.size()),
                }
            } else {
                Constraints::loose(bounds.size())
            };
            self.render.get_mut(lr).expect("live").data.needs_layout = true;
            let size = self.measure(lr, c);
            let origin = place(
                &props.anchor,
                props.place,
                props.fit,
                props.align,
                anchor,
                size,
                bounds,
            );
            self.arrange(lr, origin, Rect::from_size(frame));
        }
    }

    /// The first element carrying this key, in tree order. What an `Anchor`
    /// addressed by key resolves through, and what a test uses to name a node.
    pub fn find_by_key(&self, k: &crate::key::Key) -> Option<ElementId> {
        let root = self.root?;
        self.find_key_from(root, k)
    }

    fn find_key_from(&self, id: ElementId, k: &crate::key::Key) -> Option<ElementId> {
        let el = self.arena.get(id)?;
        if el.key.as_ref() == Some(k) {
            return Some(id);
        }
        for c in &el.children {
            if let Some(f) = self.find_key_from(*c, k) {
                return Some(f);
            }
        }
        None
    }
}

#[allow(clippy::too_many_arguments)]
fn place(
    anchor_kind: &Anchor,
    p: Place,
    fit: Fit,
    align: Option<Align>,
    anchor: Rect,
    size: Size,
    // Where the layer may go: the frame, or the region it named. Every edge
    // below is this rectangle's, not the screen's.
    bounds: Rect,
) -> Point {
    let (left, top) = (bounds.x, bounds.y);
    let right = bounds.right();
    let bottom = bounds.bottom();
    let sw = size.w as i32;
    let sh = size.h as i32;

    if let Anchor::Screen(a) = anchor_kind {
        let x = match a {
            Align::Stretch | Align::Start => left,
            Align::Center => left + (bounds.w as i32 - sw) / 2,
            Align::End => right - sw,
        };
        let y = match a {
            Align::Stretch | Align::Start => top,
            Align::Center => top + (bounds.h as i32 - sh) / 2,
            Align::End => bottom - sh,
        };
        return Point::new(x.max(left), y.max(top));
    }

    let (mut x, mut y) = match p {
        Place::Below => (anchor.x, anchor.bottom()),
        Place::Above => (anchor.x, anchor.y - sh),
        Place::RightOf => (anchor.right(), anchor.y),
        Place::LeftOf => (anchor.x - sw, anchor.y),
        Place::Over | Place::Fill => (anchor.x, anchor.y),
    };

    // Where the layer sits against the anchor on the axis the placement did
    // not use. `Stretch` was already applied as a constraint and needs no
    // origin of its own; the rest slide within the anchor's extent.
    match (align, p) {
        (Some(a), Place::Above | Place::Below) => {
            x = match a {
                Align::Stretch | Align::Start => anchor.x,
                Align::Center => anchor.x + (anchor.w as i32 - sw) / 2,
                Align::End => anchor.right() - sw,
            }
        }
        (Some(a), Place::LeftOf | Place::RightOf) => {
            y = match a {
                Align::Stretch | Align::Start => anchor.y,
                Align::Center => anchor.y + (anchor.h as i32 - sh) / 2,
                Align::End => anchor.bottom() - sh,
            }
        }
        _ => {}
    }

    if fit.flip {
        match p {
            Place::Below if y + sh > bottom && anchor.y - sh >= top => y = anchor.y - sh,
            Place::Above if y < top && anchor.bottom() + sh <= bottom => y = anchor.bottom(),
            Place::RightOf if x + sw > right && anchor.x - sw >= left => x = anchor.x - sw,
            Place::LeftOf if x < left && anchor.right() + sw <= right => x = anchor.right(),
            _ => {}
        }
    }
    if fit.shift || fit.clamp {
        x = x.min(right - sw).max(left);
    }
    if fit.clamp {
        y = y.min(bottom - sh).max(top);
    }
    Point::new(x, y)
}
