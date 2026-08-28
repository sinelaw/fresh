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
    Axis, Ctl, Event, Flow, GestureKind, Input, KeyPress, Mods, MouseButton, Phase,
    SelectionOnFocus,
};
use crate::render::geom::Point;
use crate::render::object::{Hit, RenderId};
use crate::schedule::Ui;

/// What one dispatch did.
///
/// Messages and *claim* are different answers. A handler claims an event with
/// [`Event::stop`] and may return no message at all; another produces a message
/// without claiming anything. A host routing between this tree and its own
/// older pipeline needs to know which happened, and cannot infer it from the
/// messages — so it is reported.
pub struct Dispatch<M> {
    /// What the handlers returned, in the order they ran.
    pub msgs: Vec<M>,
    /// Whether a node claimed the event, so nothing behind this tree should
    /// also act on it.
    pub claimed: bool,
}

impl<M> std::ops::Deref for Dispatch<M> {
    type Target = [M];
    fn deref(&self) -> &[M] {
        &self.msgs
    }
}

impl<M> IntoIterator for Dispatch<M> {
    type Item = M;
    type IntoIter = std::vec::IntoIter<M>;
    fn into_iter(self) -> Self::IntoIter {
        self.msgs.into_iter()
    }
}

impl<M: PartialEq> PartialEq<Vec<M>> for Dispatch<M> {
    fn eq(&self, other: &Vec<M>) -> bool {
        self.msgs == *other
    }
}

impl<M: std::fmt::Debug> std::fmt::Debug for Dispatch<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Dispatch")
            .field("msgs", &self.msgs)
            .field("claimed", &self.claimed)
            .finish()
    }
}

impl<M: 'static> Ui<M> {
    /// Route one host input into the tree, reporting the messages handlers
    /// produced and whether the event was claimed. Handlers run during this
    /// call; the tree is not rebuilt by it.
    pub fn dispatch(&mut self, input: Input) -> Dispatch<M> {
        let mut out = Vec::new();
        let claimed = self.route_input(input, &mut out);
        Dispatch { msgs: out, claimed }
    }

    /// The routing itself, reporting whether anything claimed the event.
    fn route_input(&mut self, input: Input, out: &mut Vec<M>) -> bool {
        match input {
            Input::Key(k) => self.dispatch_key(k, out),
            Input::Move { pos, mods } => {
                if let Some(r) = self.scrollbar_drag {
                    // A drag in progress owns the pointer.
                    self.scroll_to_pointer(r, pos.y);
                    return true;
                }
                let paths = self.route(pos);
                self.update_hover(&paths, pos, mods, out);
                let (claimed, _) = self.propagate_all(
                    &paths,
                    GestureKind::Move,
                    pos,
                    MouseButton::Left,
                    mods,
                    Wheel::NONE,
                    1,
                    out,
                );
                claimed
            }
            Input::Press {
                pos,
                button,
                mods,
                clicks,
            } => {
                // A press on a viewport's scrollbar gutter drives its scroll
                // directly — click to jump, then drag to follow. Scroll is
                // framework-owned, so this produces no application message.
                if button == MouseButton::Left {
                    if let Some(r) = self.scrollbar_hit(pos) {
                        self.scrollbar_drag = Some(r);
                        self.scroll_to_pointer(r, pos.y);
                        return true;
                    }
                }
                // Dismissal happens for any button; it *claims* only for the
                // primary one.
                //
                // A left click outside a menu is spent closing it — that is
                // the whole gesture. A right click outside is not: every
                // platform closes the open menu and opens the new one from
                // that same press, so consuming it would cost the user a
                // click. Same rule a viewport applies to the wheel: act, and
                // claim only when the act was the whole of it.
                let dismissed = self.dismiss_for_pointer(pos, out);
                let dismiss_claims = dismissed && button == MouseButton::Left;
                let paths = self.route(pos);
                // Every stacked path's target, so a click is derived per path:
                // a transparent overlay and what is behind it were both
                // pressed, and both are clicked.
                let targets: Vec<ElementId> =
                    paths.iter().filter_map(|p| p.last().copied()).collect();
                // The run count travels with the press so the `Click` this
                // completes can carry it too.
                self.press = Some((targets, button, clicks));
                let (claimed, _) = self.propagate_all(
                    &paths,
                    GestureKind::Press,
                    pos,
                    button,
                    mods,
                    Wheel::NONE,
                    clicks,
                    out,
                );
                claimed || dismiss_claims
            }
            Input::Release { pos, button, mods } => {
                if self.scrollbar_drag.take().is_some() {
                    return true;
                }
                let paths = self.route(pos);
                let (mut claimed, _) = self.propagate_all(
                    &paths,
                    GestureKind::Release,
                    pos,
                    button,
                    mods,
                    Wheel::NONE,
                    1,
                    out,
                );
                // A click is a press and a release over the same element, one
                // per stacked path.
                if let Some((pressed, b, clicks)) = self.press.take() {
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
                        let (c, _) = self.propagate_all(
                            &click_paths,
                            kind,
                            pos,
                            button,
                            mods,
                            Wheel::NONE,
                            clicks,
                            out,
                        );
                        claimed |= c;
                    }
                }
                self.captured = None;
                claimed
            }
            Input::Wheel {
                pos,
                delta,
                axis,
                mods,
            } => {
                let wheel = Wheel { delta, axis };
                let paths = self.route(pos);
                let (claimed, prevented) = self.propagate_all(
                    &paths,
                    GestureKind::Wheel,
                    pos,
                    MouseButton::Left,
                    mods,
                    wheel,
                    1,
                    out,
                );
                if !claimed && !prevented {
                    // Scroll chaining: the first viewport along the path whose
                    // offset can actually move takes it. One at its bound does
                    // not claim, so the wheel continues outward.
                    if let Some(p) = paths.first() {
                        let p = p.clone();
                        self.scroll_chain(&p, wheel);
                    }
                }
                claimed
            }
        }
    }

    /// How far a wheel turned and along which axis, as one parameter — so the
    /// gestures that are not wheels pass [`Wheel::NONE`] rather than a bare zero.
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
        wheel: Wheel,
        clicks: u8,
        out: &mut Vec<M>,
    ) -> (bool, bool) {
        let mut prevented = false;
        for path in paths {
            let (claimed, p) =
                self.propagate(path, kind, pos, button, mods, wheel, None, clicks, out);
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
        // A modal layer makes everything *below* it non-interactive. Not
        // everything outside its subtree: a layer resolved after it — a
        // dropdown opened by one of its own fields — is above it, and stays
        // hittable. Layers are resolved in the order they were found, so
        // "after" is an index.
        let floor = self.topmost_modal_index().unwrap_or(0);
        for i in (floor..self.pending_layers.len()).rev() {
            let paths = self.paths_in(self.pending_layers[i].0, p);
            if !paths.is_empty() {
                return paths;
            }
        }
        // Nothing above took it. Inside a modal the search stops at its
        // subtree; outside one it reaches the whole tree.
        let root = match self.topmost_modal().and_then(|e| self.render_for(e)) {
            Some(r) => Some(r),
            None => self.render_root,
        };
        match root {
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
        // What the description says, if it says anything; otherwise the render
        // object's own answer. A node that draws nothing still hits by default
        // — a plain container is a surface until it says it is not — so saying
        // so is what `pointer_mode` is for.
        let disposition = match n.pointer {
            Some(crate::desc::PointerMode::Opaque) => Hit::Opaque,
            Some(crate::desc::PointerMode::Transparent) => Hit::Transparent,
            Some(crate::desc::PointerMode::Ignore) => Hit::Ignore,
            None => n.obj.as_ref().map(|o| o.hit(local)).unwrap_or(Hit::Opaque),
        };
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
        wheel: Wheel,
        key: Option<KeyPress>,
        clicks: u8,
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
                    delta: wheel.delta,
                    axis: wheel.axis,
                    key,
                    clicks,
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
        if kind == GestureKind::Key {
            return if capture {
                Vec::new()
            } else {
                self.focus_config(id).map(|c| c.on_key).unwrap_or_default()
            };
        }
        match &resolve(&el.desc).desc {
            Desc::Gesture(g) => g
                .listeners
                .iter()
                .filter(|l| l.kind == kind && l.capture == capture)
                .map(|l| l.handler.clone())
                .collect(),
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
            axis: Axis::Vertical,
            key: None,
            clicks: 1,
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

    /// The viewport whose scrollbar gutter is under a point, if any. The gutter
    /// is the node's last column, which its content does not cover, so a hit
    /// there is unambiguous.
    fn scrollbar_hit(&self, pos: Point) -> Option<RenderId> {
        let mut found = None;
        for e in self.hit_test(pos) {
            let Some(r) = self.arena.get(e).and_then(|el| el.render) else {
                continue;
            };
            let Some(n) = self.render.get(r) else {
                continue;
            };
            if n.scrollbar && n.clips && n.data.scroll_max.y > 0 {
                let rect = n.data.rect;
                if pos.x == rect.right() - 1 && rect.y <= pos.y && pos.y < rect.bottom() {
                    found = Some(r);
                }
            }
        }
        found
    }

    /// Map a pointer row on the scrollbar track to a scroll offset and apply it.
    ///
    /// **The thumb's top lands on the row pointed at.** Its travel is the part
    /// of the track it can reach — `track - len`, not the whole track — and
    /// dividing by the track instead leaves the thumb short of the row by
    /// `len` cells at the bottom and the last row of the track unable to reach
    /// the end of the content. So the same `len` [`Draw::scrollbar_thumb`]
    /// paints with is what this divides by; press and drag both come here, so
    /// they agree by construction.
    fn scroll_to_pointer(&mut self, r: RenderId, y: i32) {
        let (rect, max) = {
            let Some(n) = self.render.get(r) else { return };
            (n.data.rect, n.data.scroll_max.y)
        };
        use crate::render::spec::Draw;
        let track = rect.h.max(1);
        let content = max.max(0) as u32 + track as u32;
        let top_of = |off: i32| Draw::scrollbar_thumb(off.max(0) as u32, content, track).0 as i32;
        let (_, len) = Draw::scrollbar_thumb(0, content, track);
        let travel = (track as i32 - len as i32).max(0);
        let off = if travel == 0 || max <= 0 {
            0
        } else {
            let rel = (y - rect.y).clamp(0, travel);
            // `scrollbar_thumb` *floors* offset -> row, so dividing back the
            // same way lands the thumb a row above the one pointed at whenever
            // the quotient has a fraction. Take the smallest offset that
            // reaches the row instead, and keep the one below it as a
            // candidate: with fewer scroll positions than track rows not every
            // row is reachable, and there the nearer of the two wins.
            let hi = ((rel * max + travel - 1) / travel).min(max);
            let lo = (hi - 1).max(0);
            if (top_of(lo) - rel).abs() < (top_of(hi) - rel).abs() {
                lo
            } else {
                hi
            }
        };
        if let Some(n) = self.render.get_mut(r) {
            n.data.scroll.y = off.clamp(0, max);
        }
        self.mark_render_dirty(r);
    }

    fn scroll_chain(&mut self, path: &[ElementId], wheel: Wheel) {
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
            let (at, limit) = match wheel.axis {
                Axis::Vertical => (scroll.y, max.y),
                Axis::Horizontal => (scroll.x, max.x),
            };
            let next = (at + wheel.delta).clamp(0, limit.max(0));
            if next != at {
                if let Some(node) = self.render.get_mut(r) {
                    match wheel.axis {
                        Axis::Vertical => node.data.scroll.y = next,
                        Axis::Horizontal => node.data.scroll.x = next,
                    }
                }
                self.mark_render_dirty(r);
                return;
            }
        }
    }

    /// The one part of a layer that cannot live on the render object: a
    /// handler is typed by the message, and a render object never sees one.
    fn dismiss_handler(&self, lid: ElementId) -> Option<crate::desc::Handler<M>> {
        match &resolve(&self.arena.get(lid)?.desc).desc {
            Desc::Layer(l) => l.on_dismiss.clone(),
            _ => None,
        }
    }

    /// Reports whether any layer was dismissed.
    fn dismiss_for_pointer(&mut self, pos: Point, out: &mut Vec<M>) -> bool {
        let mut dismissed = false;
        let path = self.hit_test(pos);
        let layers: Vec<ElementId> = self
            .pending_layers
            .iter()
            .filter_map(|(l, _)| self.element_of(*l))
            .collect();
        for lid in layers {
            let Some(geom) = self.render_for(lid).and_then(|r| self.layer_geom(r)) else {
                continue;
            };
            if !geom.dismiss.outside_pointer {
                continue;
            }
            // An ancestor test over the existing tree: inside the layer's
            // subtree, or not.
            if path.contains(&lid) {
                continue;
            }
            if let Some(h) = self.dismiss_handler(lid) {
                let ctl = Rc::new(Ctl::default());
                let ev = Event {
                    kind: GestureKind::Press,
                    pos,
                    local: pos,
                    button: MouseButton::Left,
                    mods: Mods::NONE,
                    delta: 0,
                    axis: Axis::Vertical,
                    key: None,
                    clicks: 1,
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
            // A layer that declared the dismissal was dismissed, whether or
            // not it also had something to say about it.
            dismissed = true;
        }
        dismissed
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
            let Some(geom) = self.render_for(lid).and_then(|r| self.layer_geom(r)) else {
                continue;
            };
            let matched = (geom.dismiss.escape && k.code == KeyCode::Esc)
                || geom.dismiss.any_key
                || geom.dismiss.any_input;
            if !matched {
                continue;
            }
            if let Some(h) = self.dismiss_handler(lid) {
                let ctl = Rc::new(Ctl::default());
                let ev = Event {
                    kind: GestureKind::Key,
                    pos: Point::ZERO,
                    local: Point::ZERO,
                    button: MouseButton::Left,
                    mods: k.mods,
                    clicks: 1,
                    delta: 0,
                    axis: Axis::Vertical,
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

/// How far a wheel turned, and along which axis.
///
/// One parameter rather than two, so the gestures that are not wheels pass
/// [`Wheel::NONE`] instead of a bare zero whose meaning has to be inferred.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct Wheel {
    pub delta: i32,
    pub axis: Axis,
}

impl Wheel {
    /// No wheel movement: what a press, release or click carries.
    pub const NONE: Wheel = Wheel {
        delta: 0,
        axis: Axis::Vertical,
    };
}
