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

use crate::desc::{resolve, Desc, ElemType, Modality, PointerMode};
use crate::element::ElementId;
use crate::event::{
    Ctl, Event, Flow, GestureKind, Input, KeyPress, Mods, MouseButton, Phase, SelectionOnFocus,
};
use crate::render::geom::Point;
use crate::schedule::Ui;

impl<M: 'static> Ui<M> {
    /// Route one host input into the tree and collect the messages handlers
    /// produced. Handlers run during this call; the tree is not rebuilt by it.
    pub fn dispatch(&mut self, input: Input) -> Vec<M> {
        let mut out = Vec::new();
        match input {
            Input::Key(k) => self.dispatch_key(k, &mut out),
            Input::Move { pos, mods } => {
                let path = self.route(pos);
                self.update_hover(&path, pos, mods, &mut out);
                self.propagate(
                    &path,
                    GestureKind::Move,
                    pos,
                    MouseButton::Left,
                    mods,
                    0,
                    None,
                    &mut out,
                );
            }
            Input::Press { pos, button, mods } => {
                self.dismiss_for_pointer(pos, &mut out);
                let path = self.route(pos);
                self.press = path.last().copied().map(|t| (t, button));
                self.propagate(
                    &path,
                    GestureKind::Press,
                    pos,
                    button,
                    mods,
                    0,
                    None,
                    &mut out,
                );
            }
            Input::Release { pos, button, mods } => {
                let path = self.route(pos);
                self.propagate(
                    &path,
                    GestureKind::Release,
                    pos,
                    button,
                    mods,
                    0,
                    None,
                    &mut out,
                );
                // A click is a press and a release over the same element.
                if let Some((pressed, b)) = self.press.take() {
                    if b == button && path.contains(&pressed) {
                        let kind = match button {
                            MouseButton::Right => GestureKind::SecondaryClick,
                            _ => GestureKind::Click,
                        };
                        let click_path = self.path_to(pressed);
                        self.propagate(&click_path, kind, pos, button, mods, 0, None, &mut out);
                    }
                }
                self.captured = None;
            }
            Input::Wheel { pos, delta, mods } => {
                let path = self.route(pos);
                let (claimed, prevented) = self.propagate(
                    &path,
                    GestureKind::Wheel,
                    pos,
                    MouseButton::Left,
                    mods,
                    delta,
                    None,
                    &mut out,
                );
                if !claimed && !prevented {
                    // Scroll chaining: the first viewport along the path whose
                    // offset can actually move takes it. One that is already at
                    // its bound does not claim, so the wheel continues outward.
                    self.scroll_chain(&path, delta);
                }
            }
        }
        out
    }

    // -- hit-testing ---------------------------------------------------------

    /// The element path under a point, outermost first.
    pub fn hit_test(&self, p: Point) -> Vec<ElementId> {
        // A modal layer makes everything outside its subtree non-interactive,
        // so the search starts there.
        if let Some(&(lid, _)) = self
            .pending_layers
            .iter()
            .rev()
            .find(|(l, _)| self.layer_modality(*l) != Modality::None)
        {
            return self.hit_subtree(lid, p).unwrap_or_default();
        }
        // Otherwise: layers first, topmost declared last, then the tree.
        for &(lid, _) in self.pending_layers.iter().rev() {
            if let Some(path) = self.hit_subtree(lid, p) {
                return path;
            }
        }
        match self.root {
            Some(r) => self.hit_subtree(r, p).unwrap_or_default(),
            None => Vec::new(),
        }
    }

    /// Hit a subtree, returning the full path from the tree root: a layer is an
    /// ordinary child of the description tree, so propagation continues through
    /// its ancestors and it keeps its owner's identity.
    fn hit_subtree(&self, id: ElementId, p: Point) -> Option<Vec<ElementId>> {
        let mut deep = Vec::new();
        if !self.hit_node(id, p, &mut deep) {
            return None;
        }
        deep.reverse();
        let mut path = self.ancestors_of(id);
        path.extend(deep);
        Some(path)
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

    /// Depth-first, topmost sibling first. Pushes deepest-first.
    fn hit_node(&self, id: ElementId, p: Point, out: &mut Vec<ElementId>) -> bool {
        let Some(el) = self.arena.get(id) else {
            return false;
        };
        let mode = self.pointer_mode(id);
        if mode == PointerMode::Ignore {
            return false;
        }
        for &c in el.children.iter().rev() {
            // Layers are hit-tested as their own stacking context.
            if self.arena.get(c).map(|e| e.ty) == Some(ElemType::Layer) {
                continue;
            }
            if self.hit_node(c, p, out) {
                out.push(id);
                return true;
            }
        }
        let vis = el.layout.rect.intersect(el.layout.clip);
        if mode == PointerMode::Opaque && vis.contains(p) {
            out.push(id);
            return true;
        }
        false
    }

    fn pointer_mode(&self, id: ElementId) -> PointerMode {
        match self.arena.get(id).map(|e| &e.desc) {
            Some(d) => match &resolve(d).desc {
                Desc::Gesture(g) => g.mode,
                _ => PointerMode::Opaque,
            },
            None => PointerMode::Opaque,
        }
    }

    fn layer_modality(&self, id: ElementId) -> Modality {
        match self.arena.get(id).map(|e| &e.desc) {
            Some(d) => match &resolve(d).desc {
                Desc::Layer(l) => l.modality,
                _ => Modality::None,
            },
            None => Modality::None,
        }
    }

    /// Where the event should go: the captured element if there is one,
    /// otherwise whatever is under the pointer.
    fn route(&self, p: Point) -> Vec<ElementId> {
        match self.captured {
            Some(c) if self.arena.get(c).is_some() => self.path_to(c),
            _ => self.hit_test(p),
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
                let rect = self.arena.get(n).map(|e| e.layout.rect).unwrap_or_default();
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

    fn update_hover(&mut self, path: &[ElementId], pos: Point, mods: Mods, out: &mut Vec<M>) {
        let old = std::mem::take(&mut self.hover);
        let left: Vec<ElementId> = old.iter().copied().filter(|e| !path.contains(e)).collect();
        let entered: Vec<ElementId> = path.iter().copied().filter(|e| !old.contains(e)).collect();
        self.hover = path.to_vec();
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
        let rect = self.arena.get(n).map(|e| e.layout.rect).unwrap_or_default();
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
            if self.arena.get(n).map(|e| e.ty) != Some(ElemType::Viewport) {
                continue;
            }
            let (scroll, content) = self.scroll(n);
            let window = self.size_of(n);
            let max = content.h.saturating_sub(window.h) as i32;
            let next = (scroll.y + delta).clamp(0, max);
            if next != scroll.y {
                self.scroll_to(n, Point::new(scroll.x, next));
                return;
            }
        }
    }

    fn dismiss_for_pointer(&mut self, pos: Point, out: &mut Vec<M>) {
        let path = self.hit_test(pos);
        let layers: Vec<ElementId> = self.pending_layers.iter().map(|(l, _)| *l).collect();
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
        let layers: Vec<ElementId> = self.pending_layers.iter().map(|(l, _)| *l).collect();
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
