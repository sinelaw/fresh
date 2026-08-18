//! Hit-testing and propagation.
//!
//! The path is derived from the **retained geometry**, not from the
//! descriptions, so it accounts for overlapping content, clipping and
//! out-of-flow layers — none of which a walk over descriptions can see.
//!
//! ```text
//! path   = hit_test(point)          // root -> target
//! capture: root   -> target         // each node may claim
//! target
//! bubble : target -> root           // each node may claim
//! ```

use std::rc::Rc;

use crate::desc::{resolve, Desc, ElemType};
use crate::element::ElementId;
use crate::event::{
    Ctl, Event, Flow, GestureKind, Input, KeyPress, Mods, MouseButton, Phase, SelectionOnFocus,
};
use crate::render::geom::Point;
use crate::render::object::{Hit, RenderId};
use crate::schedule::Ui;

impl<M: 'static> Ui<M> {
    /// Route one host input into the tree and collect the messages handlers
    /// produced. Handlers run during this call; the tree is not rebuilt by it.
    pub fn dispatch(&mut self, input: Input) -> Vec<M> {
        let mut out = Vec::new();
        match input {
            Input::Key(k) => self.dispatch_key(k, &mut out),
            Input::Move { pos, mods } => {
                let paths = self.route(pos);
                self.update_hover(&paths, pos, mods, &mut out);
                self.propagate_all(
                    &paths,
                    GestureKind::Move,
                    pos,
                    MouseButton::Left,
                    mods,
                    0,
                    &mut out,
                );
            }
            Input::Press { pos, button, mods } => {
                self.dismiss_for_pointer(pos, &mut out);
                let paths = self.route(pos);
                // Every stacked path's target, so a click is derived per path:
                // a transparent overlay and what is behind it were both
                // pressed, and both are clicked.
                let targets: Vec<ElementId> =
                    paths.iter().filter_map(|p| p.last().copied()).collect();
                self.press = Some((targets, button));
                self.propagate_all(&paths, GestureKind::Press, pos, button, mods, 0, &mut out);
            }
            Input::Release { pos, button, mods } => {
                let paths = self.route(pos);
                self.propagate_all(&paths, GestureKind::Release, pos, button, mods, 0, &mut out);
                // A click is a press and a release over the same element, one
                // per stacked path.
                if let Some((pressed, b)) = self.press.take() {
                    if b == button {
                        let kind = match button {
                            MouseButton::Right => GestureKind::SecondaryClick,
                            _ => GestureKind::Click,
                        };
                        let click_paths: Vec<Vec<ElementId>> = paths
                            .iter()
                            .filter(|p| p.last().is_some_and(|t| pressed.contains(t)))
                            .cloned()
                            .collect();
                        self.propagate_all(&click_paths, kind, pos, button, mods, 0, &mut out);
                    }
                }
                self.captured = None;
            }
            Input::Wheel { pos, delta, mods } => {
                let paths = self.route(pos);
                let (claimed, prevented) = self.propagate_all(
                    &paths,
                    GestureKind::Wheel,
                    pos,
                    MouseButton::Left,
                    mods,
                    delta,
                    &mut out,
                );
                if !claimed && !prevented {
                    // Scroll chaining: the first viewport along the path whose
                    // offset can actually move takes it. One at its bound does
                    // not claim, so the wheel continues outward.
                    if let Some(p) = paths.first() {
                        let p = p.clone();
                        self.scroll_chain(&p, delta);
                    }
                }
            }
        }
        out
    }

    /// Run the stacked paths in order until one claims the event. A
    /// transparent region is why there is more than one.
    #[allow(clippy::too_many_arguments)]
    fn propagate_all(
        &mut self,
        paths: &[Vec<ElementId>],
        kind: GestureKind,
        pos: Point,
        button: MouseButton,
        mods: Mods,
        delta: i32,
        out: &mut Vec<M>,
    ) -> (bool, bool) {
        let mut prevented = false;
        for path in paths {
            let (claimed, p) = self.propagate(path, kind, pos, button, mods, delta, None, out);
            prevented |= p;
            if claimed {
                return (true, prevented);
            }
        }
        (false, prevented)
    }

    // -- hit-testing ---------------------------------------------------------

    /// The element path under a point, outermost first. The topmost one when
    /// several are stacked.
    pub fn hit_test(&self, p: Point) -> Vec<ElementId> {
        self.hit_paths(p).into_iter().next().unwrap_or_default()
    }

    /// Every path under a point, topmost first.
    ///
    /// There is more than one when a region declares itself transparent: its
    /// own handlers run, and then the hit continues to whatever is behind. The
    /// walk is over the **render tree**, so it accounts for overlapping
    /// content, clipping and out-of-flow layers — none of which a walk over
    /// descriptions can see.
    pub fn hit_paths(&self, p: Point) -> Vec<Vec<ElementId>> {
        // A modal layer makes everything outside its subtree non-interactive,
        // so the search starts there.
        if let Some(e) = self.topmost_modal() {
            if let Some(r) = self.render_for(e) {
                return self.paths_in(r, p);
            }
        }
        // Otherwise: layers first, topmost declared last, then the tree.
        for &(lr, _) in self.pending_layers.iter().rev() {
            let paths = self.paths_in(lr, p);
            if !paths.is_empty() {
                return paths;
            }
        }
        match self.render_root {
            Some(r) => self.paths_in(r, p),
            None => Vec::new(),
        }
    }

    /// Hit a subtree, returning full element paths from the tree root: a layer
    /// is an ordinary child of the description tree, so propagation continues
    /// through its ancestors and it keeps its owner's identity.
    fn paths_in(&self, r: RenderId, p: Point) -> Vec<Vec<ElementId>> {
        let mut deep: Vec<Vec<RenderId>> = Vec::new();
        self.collect_paths(r, p, &mut deep);
        let Some(root_el) = self.element_of(r) else {
            return Vec::new();
        };
        let above = self.ancestors_of(root_el);
        deep.into_iter()
            .map(|mut path| {
                path.reverse();
                let mut out = above.clone();
                out.extend(path.into_iter().filter_map(|n| self.element_of(n)));
                out
            })
            .collect()
    }

    /// Depth-first, topmost sibling first, pushing deepest-first paths.
    /// Returns whether an opaque hit occurred, which is what stops the search
    /// reaching anything behind.
    fn collect_paths(&self, r: RenderId, p: Point, out: &mut Vec<Vec<RenderId>>) -> bool {
        let Some(n) = self.render.get(r) else {
            return false;
        };
        let rect = n.data.rect;
        let local = Point::new(p.x - rect.x, p.y - rect.y);
        let disposition = n.obj.as_ref().map(|o| o.hit(local)).unwrap_or(Hit::Opaque);
        if disposition == Hit::Ignore {
            return false;
        }

        let mut blocked = false;
        let mut below: Vec<Vec<RenderId>> = Vec::new();
        for &c in n.children.iter().rev() {
            // Layers are hit-tested as their own stacking contexts.
            if self.render.get(c).map(|x| x.out_of_flow).unwrap_or(false) {
                continue;
            }
            if self.collect_paths(c, p, &mut below) {
                blocked = true;
                break;
            }
        }
        for path in below.iter_mut() {
            path.push(r);
        }
        out.append(&mut below);

        if !blocked && rect.intersect(n.data.clip).contains(p) {
            match disposition {
                Hit::Opaque => {
                    out.push(vec![r]);
                    blocked = true;
                }
                // Its own handlers run, and then the hit continues behind it.
                Hit::Transparent => out.push(vec![r]),
                Hit::Ignore => {}
            }
        }
        blocked
    }

    fn ancestors_of(&self, id: ElementId) -> Vec<ElementId> {
        let mut up = Vec::new();
        let mut cur = self.arena.get(id).and_then(|e| e.parent);
        while let Some(p) = cur {
            up.push(p);
            cur = self.arena.get(p).and_then(|e| e.parent);
        }
        up.reverse();
        up
    }

    /// Where the event should go: the captured element if there is one,
    /// otherwise whatever is under the pointer.
    fn route(&self, p: Point) -> Vec<Vec<ElementId>> {
        match self.captured {
            Some(c) if self.arena.get(c).is_some() => vec![self.path_to(c)],
            _ => self.hit_paths(p),
        }
    }

    /// Root-to-element path.
    pub fn path_to(&self, id: ElementId) -> Vec<ElementId> {
        let mut path = self.ancestors_of(id);
        path.push(id);
        path
    }

    // -- propagation ---------------------------------------------------------

    /// The target as a listener on `at` should see it: rewritten to the root of
    /// the outermost component between the real target and the listener, so
    /// composition structure does not leak through events.
    fn retarget(&self, target: ElementId, at: ElementId) -> ElementId {
        let mut best = target;
        let mut cur = target;
        while let Some(p) = self.arena.get(cur).and_then(|e| e.parent) {
            if p == at {
                break;
            }
            if matches!(
                self.arena.get(p).map(|e| e.ty),
                Some(ElemType::Component(_))
            ) {
                best = p;
            }
            cur = p;
        }
        best
    }

    /// Returns `(claimed, default_prevented)`.
    #[allow(clippy::too_many_arguments)]
    fn propagate(
        &mut self,
        path: &[ElementId],
        kind: GestureKind,
        pos: Point,
        button: MouseButton,
        mods: Mods,
        delta: i32,
        key: Option<KeyPress>,
        out: &mut Vec<M>,
    ) -> (bool, bool) {
        let Some(&target) = path.last() else {
            return (false, false);
        };
        let ctl = Rc::new(Ctl::default());

        for capture in [true, false] {
            let order: Vec<ElementId> = if capture {
                path.to_vec()
            } else {
                path.iter().rev().copied().collect()
            };
            for n in order {
                let handlers = self.listeners(n, kind, capture);
                if handlers.is_empty() {
                    continue;
                }
                let rect = self.rect_of(n);
                let ev = Event {
                    kind,
                    pos,
                    local: Point::new(pos.x - rect.x, pos.y - rect.y),
                    button,
                    mods,
                    delta,
                    key,
                    selection: SelectionOnFocus::None,
                    target: self.retarget(target, n),
                    current: n,
                    phase: if n == target {
                        Phase::Target
                    } else if capture {
                        Phase::Capture
                    } else {
                        Phase::Bubble
                    },
                    ctl: ctl.clone(),
                };
                for h in handlers {
                    if let Some(m) = h(&ev) {
                        out.push(m);
                    }
                    if ctl.flow.get() == Flow::Stop {
                        break;
                    }
                }
                self.apply_controls(&ctl, out);
                if ctl.flow.get() == Flow::Stop {
                    return (true, ctl.default_prevented.get());
                }
            }
        }
        self.apply_controls(&ctl, out);
        (false, ctl.default_prevented.get())
    }

    fn apply_controls(&mut self, ctl: &Ctl, out: &mut Vec<M>) {
        if let Some(c) = ctl.capture_request.take() {
            self.captured = Some(c);
        }
        if ctl.release_request.take() {
            self.captured = None;
        }
        if let Some((id, sel)) = ctl.focus_request.take() {
            self.focus_element(id, sel, out);
        }
    }

    fn listeners(
        &self,
        id: ElementId,
        kind: GestureKind,
        capture: bool,
    ) -> Vec<crate::desc::Handler<M>> {
        let Some(el) = self.arena.get(id) else {
            return Vec::new();
        };
        match &resolve(&el.desc).desc {
            Desc::Gesture(g) => g
                .listeners
                .iter()
                .filter(|l| l.kind == kind && l.capture == capture)
                .map(|l| l.handler.clone())
                .collect(),
            Desc::Focusable(f) if kind == GestureKind::Key && !capture => f.on_key.clone(),
            _ => Vec::new(),
        }
    }

    // -- hover ---------------------------------------------------------------

    fn update_hover(&mut self, paths: &[Vec<ElementId>], pos: Point, mods: Mods, out: &mut Vec<M>) {
        let mut now: Vec<ElementId> = Vec::new();
        for p in paths {
            for e in p {
                if !now.contains(e) {
                    now.push(*e);
                }
            }
        }
        let old = std::mem::take(&mut self.hover);
        let left: Vec<ElementId> = old.iter().copied().filter(|e| !now.contains(e)).collect();
        let entered: Vec<ElementId> = now.iter().copied().filter(|e| !old.contains(e)).collect();
        self.hover = now;
        // Enter and Leave do not propagate: they are statements about one node.
        for n in left.into_iter().rev() {
            self.fire_at(n, GestureKind::Leave, pos, mods, out);
        }
        for n in entered {
            self.fire_at(n, GestureKind::Enter, pos, mods, out);
        }
    }

    fn fire_at(
        &mut self,
        n: ElementId,
        kind: GestureKind,
        pos: Point,
        mods: Mods,
        out: &mut Vec<M>,
    ) {
        let handlers = self.listeners(n, kind, false);
        if handlers.is_empty() {
            return;
        }
        let rect = self.rect_of(n);
        let ctl = Rc::new(Ctl::default());
        let ev = Event {
            kind,
            pos,
            local: Point::new(pos.x - rect.x, pos.y - rect.y),
            button: MouseButton::Left,
            mods,
            delta: 0,
            key: None,
            selection: SelectionOnFocus::None,
            target: n,
            current: n,
            phase: Phase::Target,
            ctl: ctl.clone(),
        };
        for h in handlers {
            if let Some(m) = h(&ev) {
                out.push(m);
            }
        }
        self.apply_controls(&ctl, out);
    }

    /// What is under the pointer right now, outermost first.
    pub fn hovered(&self) -> &[ElementId] {
        &self.hover
    }

    pub fn captured(&self) -> Option<ElementId> {
        self.captured
    }

    // -- defaults ------------------------------------------------------------

    fn scroll_chain(&mut self, path: &[ElementId], delta: i32) {
        for &n in path.iter().rev() {
            let Some(r) = self.render_for(n) else {
                continue;
            };
            let (scroll, max, clips) = {
                let Some(node) = self.render.get(r) else {
                    continue;
                };
                (node.data.scroll, node.data.scroll_max, node.clips)
            };
            if !clips {
                continue;
            }
            let next = (scroll.y + delta).clamp(0, max.y.max(0));
            if next != scroll.y {
                if let Some(node) = self.render.get_mut(r) {
                    node.data.scroll.y = next;
                }
                self.mark_render_dirty(r);
                return;
            }
        }
    }

    fn dismiss_for_pointer(&mut self, pos: Point, out: &mut Vec<M>) {
        let path = self.hit_test(pos);
        let layers: Vec<ElementId> = self
            .pending_layers
            .iter()
            .filter_map(|(l, _)| self.element_of(*l))
            .collect();
        for lid in layers {
            let props = match self.arena.get(lid).map(|e| &e.desc) {
                Some(d) => match &resolve(d).desc {
                    Desc::Layer(l) => l.clone(),
                    _ => continue,
                },
                None => continue,
            };
            if !props.dismiss.outside_pointer {
                continue;
            }
            // An ancestor test over the existing tree: inside the layer's
            // subtree, or not.
            if path.contains(&lid) {
                continue;
            }
            if let Some(h) = &props.on_dismiss {
                let ctl = Rc::new(Ctl::default());
                let ev = Event {
                    kind: GestureKind::Press,
                    pos,
                    local: pos,
                    button: MouseButton::Left,
                    mods: Mods::NONE,
                    delta: 0,
                    key: None,
                    selection: SelectionOnFocus::None,
                    target: lid,
                    current: lid,
                    phase: Phase::Target,
                    ctl,
                };
                if let Some(m) = h(&ev) {
                    out.push(m);
                }
            }
        }
    }

    pub(crate) fn dismiss_for_key(&mut self, k: KeyPress, out: &mut Vec<M>) -> bool {
        use crate::event::KeyCode;
        let layers: Vec<ElementId> = self
            .pending_layers
            .iter()
            .filter_map(|(l, _)| self.element_of(*l))
            .collect();
        let mut any = false;
        for lid in layers {
            let props = match self.arena.get(lid).map(|e| &e.desc) {
                Some(d) => match &resolve(d).desc {
                    Desc::Layer(l) => l.clone(),
                    _ => continue,
                },
                None => continue,
            };
            let matched = (props.dismiss.escape && k.code == KeyCode::Esc)
                || props.dismiss.any_key
                || props.dismiss.any_input;
            if !matched {
                continue;
            }
            if let Some(h) = &props.on_dismiss {
                let ctl = Rc::new(Ctl::default());
                let ev = Event {
                    kind: GestureKind::Key,
                    pos: Point::ZERO,
                    local: Point::ZERO,
                    button: MouseButton::Left,
                    mods: k.mods,
                    delta: 0,
                    key: Some(k),
                    selection: SelectionOnFocus::None,
                    target: lid,
                    current: lid,
                    phase: Phase::Target,
                    ctl,
                };
                if let Some(m) = h(&ev) {
                    out.push(m);
                }
                any = true;
            }
        }
        any
    }
}
