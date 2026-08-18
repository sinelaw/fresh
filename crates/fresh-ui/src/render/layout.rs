//! The layout pass.
//!
//! Box constraints in integer cells: constraints propagate down, sizes
//! propagate up, parents position children. One pass, no solver, no sub-cell
//! arithmetic.
//!
//! Two mechanisms bound the work. **Relayout boundaries** stop a change
//! propagating up: a node laid out under tight constraints cannot change size,
//! so a dirty node marks the path only as far as the nearest one, and the pass
//! re-enters there rather than at the root. The **constraint-keyed cache** stops
//! recomputation propagating down: a clean node handed constraints equal to its
//! last ones returns the stored size without visiting its subtree.

use unicode_width::UnicodeWidthStr;

use crate::desc::{
    resolve, Align, Anchor, BoxProps, Desc, Dir, ElemType, Fit, Node, Place, Sizing,
};
use crate::element::ElementId;
use crate::schedule::Ui;

use super::geom::{distribute, Constraints, Point, Rect, Size};

// -- axis helpers ------------------------------------------------------------

fn main_of(d: Dir, s: Size) -> u16 {
    match d {
        Dir::Row => s.w,
        Dir::Col => s.h,
    }
}

fn cross_of(d: Dir, s: Size) -> u16 {
    match d {
        Dir::Row => s.h,
        Dir::Col => s.w,
    }
}

fn size_of(d: Dir, main: u16, cross: u16) -> Size {
    match d {
        Dir::Row => Size::new(main, cross),
        Dir::Col => Size::new(cross, main),
    }
}

fn point_of(d: Dir, main: i32, cross: i32) -> Point {
    match d {
        Dir::Row => Point::new(main, cross),
        Dir::Col => Point::new(cross, main),
    }
}

fn axes(d: Dir, main: (u16, u16), cross: (u16, u16)) -> Constraints {
    match d {
        Dir::Row => Constraints::new(main.0, main.1, cross.0, cross.1),
        Dir::Col => Constraints::new(cross.0, cross.1, main.0, main.1),
    }
}

fn pct(extent: u16, p: u8) -> u16 {
    ((extent as u32 * p as u32) / 100) as u16
}

/// Resolve a cross-axis request into a constraint range.
fn cross_range(s: Sizing, extent: u16, definite: bool, align: Align) -> (u16, u16) {
    match s {
        Sizing::Cells(v) => {
            let v = v.min(extent);
            (v, v)
        }
        Sizing::Pct(p) => {
            let v = pct(extent, p);
            (v, v)
        }
        // A cross-axis flex request means "fill".
        Sizing::Flex(_) => (extent, extent),
        Sizing::Auto => {
            if definite && align == Align::Stretch {
                (extent, extent)
            } else {
                (0, extent)
            }
        }
    }
}

fn align_offset(align: Align, extent: u16, size: u16) -> i32 {
    let slack = extent.saturating_sub(size);
    match align {
        Align::Stretch | Align::Start => 0,
        Align::Center => (slack / 2) as i32,
        Align::End => slack as i32,
    }
}

/// Greedy word wrap, breaking a word that cannot fit on a line of its own.
///
/// Measure and paint call the same function, so the height layout reserves is
/// always exactly the number of rows paint emits.
pub(crate) fn wrap_text(text: &str, width: u16) -> Vec<String> {
    if width == 0 {
        return Vec::new();
    }
    let width = width as usize;
    let mut out: Vec<String> = Vec::new();
    for para in text.split('\n') {
        let mut line = String::new();
        for word in para.split(' ') {
            if line.is_empty() {
                line.push_str(word);
            } else if UnicodeWidthStr::width(line.as_str()) + 1 + UnicodeWidthStr::width(word)
                <= width
            {
                line.push(' ');
                line.push_str(word);
            } else {
                out.push(std::mem::take(&mut line));
                line.push_str(word);
            }
            while UnicodeWidthStr::width(line.as_str()) > width {
                let head: String = line.chars().take(width).collect();
                let tail: String = line.chars().skip(width).collect();
                out.push(head);
                line = tail;
            }
        }
        out.push(line);
    }
    if out.is_empty() {
        out.push(String::new());
    }
    out
}

pub(crate) fn wrapped_lines(text: &str, width: u16) -> u16 {
    wrap_text(text, width).len().min(u16::MAX as usize) as u16
}

// -- the pass ----------------------------------------------------------------

impl<M: 'static> Ui<M> {
    /// A node whose geometry is stale. The mark travels up only as far as the
    /// nearest relayout boundary, which is where the next pass re-enters.
    pub(crate) fn mark_needs_layout(&mut self, id: ElementId) {
        let Some(el) = self.arena.get_mut(id) else {
            return;
        };
        if el.layout.needs_layout {
            return;
        }
        el.layout.needs_layout = true;
        el.layout.cached = None;

        let mut cur = el.parent;
        let mut boundary = id;
        while let Some(p) = cur {
            let Some(pel) = self.arena.get_mut(p) else {
                break;
            };
            boundary = p;
            pel.layout.child_needs_layout = true;
            if pel.layout.boundary {
                break;
            }
            cur = pel.parent;
        }
        if !self.layout_dirty.contains(&boundary) {
            self.layout_dirty.push(boundary);
        }
    }

    /// Run layout, then position everything and resolve out-of-flow layers.
    pub(crate) fn flush_layout(&mut self, frame: Size) {
        let Some(root) = self.root else { return };

        let root_c = self.root_constraints(root, frame);
        let full = self.frame_size != frame || self.arena[root].layout.cached.is_none();
        if full {
            self.layout_dirty.clear();
            self.layout_node(root, root_c);
        } else {
            let mut dirty = std::mem::take(&mut self.layout_dirty);
            dirty.sort_by_key(|&e| self.arena.get(e).map(|x| x.depth).unwrap_or(0));
            let mut done: Vec<ElementId> = Vec::new();
            for b in dirty {
                if self.arena.get(b).is_none() {
                    continue;
                }
                if done.iter().any(|&a| self.is_ancestor(a, b)) {
                    continue;
                }
                let Some((c, _)) = self.arena[b].layout.cached else {
                    // Never laid out under known constraints: fall back to the
                    // root rather than guess.
                    self.layout_node(root, root_c);
                    done.clear();
                    break;
                };
                self.layout_node(b, c);
                done.push(b);
            }
        }
        self.frame_size = frame;

        // A LayoutReader may have replaced part of its subtree during the pass.
        self.process_disposals();

        self.pending_layers.clear();
        self.arrange(root, Point::ZERO, Rect::from_size(frame));
        self.resolve_layers(frame);
        self.process_disposals();
    }

    /// The root is a child of the frame. `Auto` there means "fill the frame",
    /// which is what an application root wants; an explicit request is honoured
    /// so that a subtree can be measured on its own terms.
    fn root_constraints(&self, root: ElementId, frame: Size) -> Constraints {
        let (sw, sh) = self.sizing_of(root);
        let axis = |s: Sizing, extent: u16| -> (u16, u16) {
            match s {
                Sizing::Cells(v) => {
                    let v = v.min(extent);
                    (v, v)
                }
                Sizing::Pct(p) => {
                    let v = pct(extent, p);
                    (v, v)
                }
                Sizing::Auto | Sizing::Flex(_) => (extent, extent),
            }
        };
        let (min_w, max_w) = axis(sw, frame.w);
        let (min_h, max_h) = axis(sh, frame.h);
        Constraints::new(min_w, max_w, min_h, max_h)
    }

    fn is_ancestor(&self, a: ElementId, mut b: ElementId) -> bool {
        while let Some(p) = self.arena.get(b).and_then(|e| e.parent) {
            if p == a {
                return true;
            }
            b = p;
        }
        false
    }

    fn layout_node(&mut self, id: ElementId, c: Constraints) -> Size {
        {
            let el = &self.arena[id];
            if !el.layout.needs_layout && !el.layout.child_needs_layout {
                if let Some((cc, sz)) = el.layout.cached {
                    if cc == c {
                        return sz;
                    }
                }
            }
        }
        let size = self.layout_dispatch(id, c);
        let el = self.arena.get_mut(id).expect("live");
        el.layout.size = size;
        el.layout.cached = Some((c, size));
        el.layout.needs_layout = false;
        el.layout.child_needs_layout = false;
        el.layout.boundary = c.is_tight();
        el.layout.layouts += 1;
        size
    }

    fn layout_dispatch(&mut self, id: ElementId, c: Constraints) -> Size {
        match self.arena[id].ty {
            ElemType::Box => self.layout_box(id, c),
            ElemType::TextRun => {
                let (text, wrap) = match &resolve(&self.arena[id].desc).desc {
                    Desc::TextRun(p) => (p.text.clone(), p.wrap),
                    _ => unreachable!(),
                };
                let natural = UnicodeWidthStr::width(&*text) as u16;
                if wrap {
                    let w = c.max_w.min(natural.max(1));
                    let w = if c.min_w > 0 { c.max_w } else { w };
                    c.constrain(Size::new(w, wrapped_lines(&text, w)))
                } else {
                    c.constrain(Size::new(natural, 1))
                }
            }
            ElemType::Viewport => self.layout_viewport(id, c),
            // Out of flow: a layer contributes nothing to its parent's size and
            // is resolved after the main walk, once its anchor has a rectangle.
            ElemType::Layer => Size::ZERO,
            ElemType::Host => c.constrain(c.max()),
            ElemType::LayoutReader => self.layout_reader(id, c),
            // No geometry of their own: a stack of their children.
            ElemType::Gesture
            | ElemType::Focusable
            | ElemType::Provide(_)
            | ElemType::Component(_) => self.layout_passthrough(id, c),
        }
    }

    fn flow_children(&self, id: ElementId) -> Vec<ElementId> {
        self.arena[id]
            .children
            .iter()
            .copied()
            .filter(|&c| self.arena.get(c).map(|e| e.ty) != Some(ElemType::Layer))
            .collect()
    }

    /// The one place a build runs during layout. `set_state` is rejected for
    /// the duration, and the subtree is reconciled transactionally before it is
    /// measured.
    fn layout_reader(&mut self, id: ElementId, c: Constraints) -> Size {
        let f = match &resolve(&self.arena[id].desc).desc {
            Desc::LayoutReader(p) => p.build.clone(),
            _ => unreachable!(),
        };
        let name = self.arena[id].name;
        let sched = self.sched.clone();
        sched.borrow_mut().enter_build(id, name);
        let node = f(c);
        sched.borrow_mut().clear_building();
        self.begin_txn();
        self.reconcile_children(id, vec![node]);
        self.commit_txn();
        self.layout_passthrough(id, c)
    }

    fn layout_passthrough(&mut self, id: ElementId, c: Constraints) -> Size {
        let kids = self.flow_children(id);
        let mut size = c.min();
        for k in kids {
            let s = self.layout_node(k, c);
            self.arena.get_mut(k).expect("live").layout.offset = Point::ZERO;
            size = Size::new(size.w.max(s.w), size.h.max(s.h));
        }
        c.constrain(size)
    }

    fn layout_viewport(&mut self, id: ElementId, c: Constraints) -> Size {
        // A viewport takes the space it is given; its content does not affect
        // it, which is what makes it a relayout boundary.
        let own = c.constrain(c.max());
        let inner = Constraints::new(own.w, own.w, 0, u16::MAX);
        let kids = self.flow_children(id);
        let mut content = Size::ZERO;
        for k in kids {
            let s = self.layout_node(k, inner);
            self.arena.get_mut(k).expect("live").layout.offset = Point::ZERO;
            content = Size::new(content.w.max(s.w), content.h.max(s.h));
        }
        let el = self.arena.get_mut(id).expect("live");
        el.layout.content = content;
        // Keep the offset inside the content.
        let max_y = content.h.saturating_sub(own.h) as i32;
        let max_x = content.w.saturating_sub(own.w) as i32;
        el.layout.scroll.x = el.layout.scroll.x.clamp(0, max_x.max(0));
        el.layout.scroll.y = el.layout.scroll.y.clamp(0, max_y.max(0));
        own
    }

    fn layout_box(&mut self, id: ElementId, c: Constraints) -> Size {
        let props: BoxProps = match &resolve(&self.arena[id].desc).desc {
            Desc::Box(p) => p.clone(),
            _ => unreachable!(),
        };
        let border = u16::from(props.border);
        let ins_x = props.pad.x.saturating_add(border);
        let ins_y = props.pad.y.saturating_add(border);
        let inner = Constraints::new(
            c.min_w.saturating_sub(2 * ins_x),
            c.max_w.saturating_sub(2 * ins_x),
            c.min_h.saturating_sub(2 * ins_y),
            c.max_h.saturating_sub(2 * ins_y),
        );
        let kids = self.flow_children(id);

        if props.stack {
            return self.layout_stack(id, c, inner, &kids, &props, ins_x, ins_y);
        }

        let dir = props.dir;
        let n = kids.len();
        let gaps = props.gap.saturating_mul(n.saturating_sub(1) as u16);
        let avail = main_of(dir, inner.max()).saturating_sub(gaps);
        let cross_extent = cross_of(dir, inner.max());
        let cross_definite = match dir {
            Dir::Row => inner.min_h == inner.max_h,
            Dir::Col => inner.min_w == inner.max_w,
        };

        let mut mains = vec![0u16; n];
        let mut crosses = vec![0u16; n];
        let mut weights = vec![0u16; n];
        let mut fixed_used: u16 = 0;

        // Everything that is not flex resolves first: flex divides what is left.
        for i in 0..n {
            let (sw, sh) = self.sizing_of(kids[i]);
            let (s_main, s_cross) = match dir {
                Dir::Row => (sw, sh),
                Dir::Col => (sh, sw),
            };
            if let Sizing::Flex(w) = s_main {
                weights[i] = w.max(1);
                continue;
            }
            let room = avail.saturating_sub(fixed_used);
            let main = match s_main {
                Sizing::Cells(v) => (v.min(room), v.min(room)),
                Sizing::Pct(p) => {
                    let v = pct(avail, p).min(room);
                    (v, v)
                }
                Sizing::Auto => (0, room),
                Sizing::Flex(_) => unreachable!(),
            };
            let cross = cross_range(s_cross, cross_extent, cross_definite, props.align);
            let s = self.layout_node(kids[i], axes(dir, main, cross));
            mains[i] = main_of(dir, s);
            crosses[i] = cross_of(dir, s);
            fixed_used = fixed_used.saturating_add(mains[i]);
        }

        let remaining = avail.saturating_sub(fixed_used);
        let shares = distribute(remaining, &weights);
        for i in 0..n {
            if weights[i] == 0 {
                continue;
            }
            let (sw, sh) = self.sizing_of(kids[i]);
            let s_cross = match dir {
                Dir::Row => sh,
                Dir::Col => sw,
            };
            let cross = cross_range(s_cross, cross_extent, cross_definite, props.align);
            let s = self.layout_node(kids[i], axes(dir, (shares[i], shares[i]), cross));
            mains[i] = main_of(dir, s);
            crosses[i] = cross_of(dir, s);
        }

        let content_main = mains
            .iter()
            .fold(0u16, |a, b| a.saturating_add(*b))
            .saturating_add(gaps);
        let content_cross = crosses.iter().copied().max().unwrap_or(0);
        let own = c.constrain(Size::new(
            size_of(dir, content_main, content_cross)
                .w
                .saturating_add(2 * ins_x),
            size_of(dir, content_main, content_cross)
                .h
                .saturating_add(2 * ins_y),
        ));

        let (ins_main, ins_cross) = match dir {
            Dir::Row => (ins_x, ins_y),
            Dir::Col => (ins_y, ins_x),
        };
        let inner_cross = cross_of(dir, own).saturating_sub(2 * ins_cross);
        let mut pos = ins_main as i32;
        for i in 0..n {
            let off = align_offset(props.align, inner_cross, crosses[i]);
            self.arena.get_mut(kids[i]).expect("live").layout.offset =
                point_of(dir, pos, ins_cross as i32 + off);
            pos += mains[i] as i32 + props.gap as i32;
        }
        own
    }

    #[allow(clippy::too_many_arguments)]
    fn layout_stack(
        &mut self,
        _id: ElementId,
        c: Constraints,
        inner: Constraints,
        kids: &[ElementId],
        props: &BoxProps,
        ins_x: u16,
        ins_y: u16,
    ) -> Size {
        let mut widest = 0u16;
        let mut tallest = 0u16;
        let w_definite = inner.min_w == inner.max_w;
        let h_definite = inner.min_h == inner.max_h;
        let mut sizes = Vec::with_capacity(kids.len());
        for &k in kids {
            let (sw, sh) = self.sizing_of(k);
            let wc = cross_range(sw, inner.max_w, w_definite, props.align);
            let hc = cross_range(sh, inner.max_h, h_definite, props.align);
            let s = self.layout_node(k, Constraints::new(wc.0, wc.1, hc.0, hc.1));
            widest = widest.max(s.w);
            tallest = tallest.max(s.h);
            sizes.push(s);
        }
        let own = c.constrain(Size::new(
            widest.saturating_add(2 * ins_x),
            tallest.saturating_add(2 * ins_y),
        ));
        let iw = own.w.saturating_sub(2 * ins_x);
        let ih = own.h.saturating_sub(2 * ins_y);
        for (i, &k) in kids.iter().enumerate() {
            self.arena.get_mut(k).expect("live").layout.offset = Point::new(
                ins_x as i32 + align_offset(props.align, iw, sizes[i].w),
                ins_y as i32 + align_offset(props.align, ih, sizes[i].h),
            );
        }
        own
    }

    /// The size request that applies to this element, looking through nodes
    /// that have no geometry of their own.
    fn sizing_of(&self, id: ElementId) -> (Sizing, Sizing) {
        let mut w = Sizing::Auto;
        let mut h = Sizing::Auto;
        let mut cur = Some(id);
        while let Some(c) = cur {
            let Some(el) = self.arena.get(c) else { break };
            let (nw, nh) = node_sizing(&el.desc);
            if w == Sizing::Auto {
                w = nw;
            }
            if h == Sizing::Auto {
                h = nh;
            }
            if w != Sizing::Auto && h != Sizing::Auto {
                break;
            }
            cur = match el.ty {
                ElemType::Component(_) | ElemType::Provide(_) => el.children.first().copied(),
                _ => None,
            };
        }
        (w, h)
    }

    // -- positioning ---------------------------------------------------------

    fn arrange(&mut self, id: ElementId, origin: Point, clip: Rect) {
        let (size, ty, scroll, kids) = {
            let Some(el) = self.arena.get(id) else { return };
            (el.layout.size, el.ty, el.layout.scroll, el.children.clone())
        };
        let rect = Rect::at(origin, size);
        {
            let el = self.arena.get_mut(id).expect("live");
            el.layout.rect = rect;
            el.layout.clip = clip;
        }
        let child_clip = if ty == ElemType::Viewport {
            clip.intersect(rect)
        } else {
            clip
        };
        let sc = if ty == ElemType::Viewport {
            scroll
        } else {
            Point::ZERO
        };
        for k in kids {
            if self.arena.get(k).map(|e| e.ty) == Some(ElemType::Layer) {
                self.pending_layers.push((k, id));
                continue;
            }
            let off = self.arena[k].layout.offset;
            self.arrange(
                k,
                Point::new(origin.x + off.x - sc.x, origin.y + off.y - sc.y),
                child_clip,
            );
        }
    }

    /// Layers resolve after the main walk: a layer anchored to a node needs
    /// that node's rectangle, which does not exist until its subtree has laid
    /// out. `fit` is applied here, against the frame.
    fn resolve_layers(&mut self, frame: Size) {
        let mut i = 0;
        while i < self.pending_layers.len() {
            let (lid, parent) = self.pending_layers[i];
            i += 1;
            let props = match &resolve(&self.arena[lid].desc).desc {
                Desc::Layer(p) => p.clone(),
                _ => continue,
            };
            let anchor = match &props.anchor {
                Anchor::Parent => self.arena[parent].layout.rect,
                Anchor::Node(k) => self
                    .find_by_key(k)
                    .map(|e| self.arena[e].layout.rect)
                    .unwrap_or(self.arena[parent].layout.rect),
                Anchor::Point(x, y) => Rect::new(*x as i32, *y as i32, 0, 0),
                Anchor::Screen(_) => Rect::from_size(frame),
            };
            let c = if props.place == Place::Fill {
                Constraints::tight(anchor.size())
            } else {
                Constraints::loose(frame)
            };
            // A layer's subtree is laid out in its own right, not as part of
            // its parent's flow.
            self.arena.get_mut(lid).expect("live").layout.needs_layout = true;
            let size = self.layout_layer_content(lid, c);
            let origin = place(&props.anchor, props.place, props.fit, anchor, size, frame);
            self.arrange(lid, origin, Rect::from_size(frame));
        }
    }

    fn layout_layer_content(&mut self, lid: ElementId, c: Constraints) -> Size {
        let kids = self.flow_children(lid);
        let mut size = Size::ZERO;
        for k in kids {
            let s = self.layout_node(k, c);
            self.arena.get_mut(k).expect("live").layout.offset = Point::ZERO;
            size = Size::new(size.w.max(s.w), size.h.max(s.h));
        }
        let el = self.arena.get_mut(lid).expect("live");
        el.layout.size = size;
        el.layout.needs_layout = false;
        el.layout.child_needs_layout = false;
        el.layout.cached = Some((c, size));
        el.layout.layouts += 1;
        size
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
        (Desc::Layer(a), Desc::Layer(b)) => a != b,
        (Desc::Host(a), Desc::Host(b)) => a != b,
        // Gesture, Focusable, Provide and Component have no geometry of their
        // own; theirs comes from their children.
        _ => false,
    }
}
