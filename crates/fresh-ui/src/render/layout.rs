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

use crate::desc::{resolve, Align, Anchor, Desc, Fit, Node, Place, Sizing};
use crate::element::ElementId;
use crate::render::object::{LayoutCx, LayoutInfo, RenderId, RenderObject};
use crate::render::prim;
use crate::schedule::Ui;

use super::geom::{Constraints, Point, Rect, Size};

pub use prim::wrap_text;

pub(crate) fn wrapped_lines(text: &str, width: u16) -> u16 {
    wrap_text(text, width).len().min(u16::MAX as usize) as u16
}

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

    fn set_window(&mut self, w: Rect) {
        if let Some(n) = self.ui.render.get_mut(self.node) {
            n.data.window = Some(w);
        }
    }

    fn set_content(&mut self, s: Size) {
        if let Some(n) = self.ui.render.get_mut(self.node) {
            n.data.content = s;
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

    /// Relink one element subtree under an existing render parent. Used by a
    /// constraint-dependent builder, which reconciles mid-layout.
    pub(crate) fn relink_from_pub(
        &mut self,
        e: ElementId,
        parent: Option<RenderId>,
        out: &mut Vec<RenderId>,
    ) {
        self.relink_from(e, parent, Sizing::Auto, Sizing::Auto, None, None, out);
    }

    /// The element a render node belongs to.
    pub(crate) fn element_of(&self, r: RenderId) -> Option<ElementId> {
        self.render.get(r).map(|n| n.element)
    }

    /// Rebuild the render tree's links from the element tree.
    ///
    /// Render objects are created and disposed with their elements; only the
    /// links, the inherited size request, and provenance are recomputed. A
    /// child list that actually changed marks the node for layout.
    pub(crate) fn relink(&mut self) {
        let Some(root) = self.root else {
            self.render_root = None;
            return;
        };
        let mut roots = Vec::new();
        self.relink_from(
            root,
            None,
            Sizing::Auto,
            Sizing::Auto,
            None,
            None,
            &mut roots,
        );
        self.render_root = roots.first().copied();
    }

    #[allow(clippy::too_many_arguments)]
    fn relink_from(
        &mut self,
        e: ElementId,
        parent: Option<RenderId>,
        cw: Sizing,
        ch: Sizing,
        theme: Option<Rc<str>>,
        key: Option<crate::key::Key>,
        out: &mut Vec<RenderId>,
    ) {
        let Some(el) = self.arena.get(e) else { return };
        let (nw, nh) = node_sizing(&el.desc);
        let w = if cw != Sizing::Auto { cw } else { nw };
        let h = if ch != Sizing::Auto { ch } else { nh };
        let theme = el
            .desc
            .theme
            .clone()
            .or_else(|| resolve(&el.desc).theme.clone())
            .or(theme);
        let key = el.key.clone().or(key);
        let render = el.render;
        let kids = el.children.clone();

        match render {
            Some(r) => {
                {
                    let n = self.render.get_mut(r).expect("live render node");
                    n.parent = parent;
                    n.w = w;
                    n.h = h;
                    n.theme = theme;
                    n.key = key;
                }
                let mut inner = Vec::new();
                for k in kids {
                    // Provenance is inherited; identity is not. A render child
                    // uses its own key, or that of a transparent element
                    // between it and this node.
                    let carried = self.render[r].theme.clone();
                    self.relink_from(
                        k,
                        Some(r),
                        Sizing::Auto,
                        Sizing::Auto,
                        carried,
                        None,
                        &mut inner,
                    );
                }
                if self.render[r].children != inner {
                    self.render.get_mut(r).expect("live").children = inner;
                    self.mark_render_dirty(r);
                }
                out.push(r);
            }
            None => {
                for k in kids {
                    self.relink_from(k, parent, w, h, theme.clone(), key.clone(), out);
                }
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
        let already = self.render[r].data.cached.is_some() && self.measuring;
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
            // Framework-owned: keep the window inside the content.
            let max_x = n.data.content.w.saturating_sub(size.w) as i32;
            let max_y = n.data.content.h.saturating_sub(size.h) as i32;
            n.data.scroll.x = n.data.scroll.x.clamp(0, max_x.max(0));
            n.data.scroll.y = n.data.scroll.y.clamp(0, max_y.max(0));
            n.data.window = Some(Rect::at(n.data.scroll, size));
        }
        n.data.cached = Some((c, size));
        n.data.needs_layout = false;
        n.data.child_needs_layout = false;
        n.data.boundary = boundary;
        n.data.layouts += 1;
        if already {
            // A second measurement of the same subtree in one frame. `Auto` is
            // the ergonomic default, so its cost has to be visible.
            n.data.remeasures += 1;
        }
        size
    }

    /// Run layout, position everything, and resolve out-of-flow layers.
    pub(crate) fn flush_layout(&mut self, frame: Size) {
        self.relink();
        let Some(root) = self.render_root else { return };

        let root_c = self.root_constraints(root, frame);
        let full = self.frame_size != frame || self.render[root].data.cached.is_none();
        if full {
            self.layout_dirty.clear();
            self.measure(root, root_c);
        } else {
            let mut dirty = std::mem::take(&mut self.layout_dirty);
            dirty.sort_by_key(|&r| self.render_depth(r));
            // Every dirty boundary is re-entered, including one below another:
            // path-marking stops at a boundary, so an ancestor's re-measure
            // does not reach the boundaries beneath it. The constraint cache is
            // what makes the overlapping case cheap.
            for b in dirty {
                if self.render.get(b).is_none() {
                    continue;
                }
                let Some((c, _)) = self.render[b].data.cached else {
                    self.measure(root, root_c);
                    break;
                };
                self.measure(b, c);
            }
        }
        self.frame_size = frame;

        // A constraint-dependent builder may have replaced part of its subtree.
        self.process_disposals();

        self.pending_layers.clear();
        self.arrange(root, Point::ZERO, Rect::from_size(frame));
        self.resolve_layers(frame);
        self.process_disposals();
    }

    fn render_depth(&self, mut r: RenderId) -> u32 {
        let mut d = 0;
        while let Some(p) = self.render.get(r).and_then(|n| n.parent) {
            d += 1;
            r = p;
        }
        d
    }

    fn is_render_ancestor(&self, a: RenderId, mut b: RenderId) -> bool {
        while let Some(p) = self.render.get(b).and_then(|n| n.parent) {
            if p == a {
                return true;
            }
            b = p;
        }
        false
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
        let (size, clips, scroll, kids) = {
            let Some(n) = self.render.get(r) else { return };
            (n.data.size, n.clips, n.data.scroll, n.children.clone())
        };
        let rect = Rect::at(origin, size);
        {
            let n = self.render.get_mut(r).expect("live");
            n.data.rect = rect;
            n.data.clip = clip;
        }
        let child_clip = if clips { clip.intersect(rect) } else { clip };
        let sc = if clips { scroll } else { Point::ZERO };
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
    }

    /// Layers resolve after the main walk: one anchored to a node needs that
    /// node's rectangle, which does not exist until its subtree has laid out.
    /// `fit` is applied here, against the frame.
    fn resolve_layers(&mut self, frame: Size) {
        let mut i = 0;
        while i < self.pending_layers.len() {
            let (lr, parent) = self.pending_layers[i];
            i += 1;
            let element = self.render[lr].element;
            let props = match &resolve(&self.arena[element].desc).desc {
                Desc::Layer(p) => p.clone(),
                _ => continue,
            };
            let anchor = match &props.anchor {
                Anchor::Parent => self.render[parent].data.rect,
                Anchor::Node(k) => self
                    .find_by_key(k)
                    .and_then(|e| self.render_for(e))
                    .map(|r| self.render[r].data.rect)
                    .unwrap_or(self.render[parent].data.rect),
                Anchor::Point(x, y) => Rect::new(*x as i32, *y as i32, 0, 0),
                Anchor::Screen(_) => Rect::from_size(frame),
            };
            let c = if props.place == Place::Fill {
                Constraints::tight(anchor.size())
            } else {
                Constraints::loose(frame)
            };
            self.render.get_mut(lr).expect("live").data.needs_layout = true;
            let size = self.measure(lr, c);
            let origin = place(&props.anchor, props.place, props.fit, anchor, size, frame);
            self.arrange(lr, origin, Rect::from_size(frame));
        }
    }

    pub(crate) fn find_by_key(&self, k: &crate::key::Key) -> Option<ElementId> {
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

fn place(anchor_kind: &Anchor, p: Place, fit: Fit, anchor: Rect, size: Size, frame: Size) -> Point {
    let fw = frame.w as i32;
    let fh = frame.h as i32;
    let sw = size.w as i32;
    let sh = size.h as i32;

    if let Anchor::Screen(a) = anchor_kind {
        let x = match a {
            Align::Stretch | Align::Start => 0,
            Align::Center => (fw - sw) / 2,
            Align::End => fw - sw,
        };
        let y = match a {
            Align::Stretch | Align::Start => 0,
            Align::Center => (fh - sh) / 2,
            Align::End => fh - sh,
        };
        return Point::new(x.max(0), y.max(0));
    }

    let (mut x, mut y) = match p {
        Place::Below => (anchor.x, anchor.bottom()),
        Place::Above => (anchor.x, anchor.y - sh),
        Place::RightOf => (anchor.right(), anchor.y),
        Place::LeftOf => (anchor.x - sw, anchor.y),
        Place::Over | Place::Fill => (anchor.x, anchor.y),
    };

    if fit.flip {
        match p {
            Place::Below if y + sh > fh && anchor.y - sh >= 0 => y = anchor.y - sh,
            Place::Above if y < 0 && anchor.bottom() + sh <= fh => y = anchor.bottom(),
            Place::RightOf if x + sw > fw && anchor.x - sw >= 0 => x = anchor.x - sw,
            Place::LeftOf if x < 0 && anchor.right() + sw <= fw => x = anchor.right(),
            _ => {}
        }
    }
    if fit.shift || fit.clamp {
        x = x.min(fw - sw).max(0);
    }
    if fit.clamp {
        y = y.min(fh - sh).max(0);
    }
    Point::new(x, y)
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
