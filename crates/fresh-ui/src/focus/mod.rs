//! Focus: a tree that mirrors the render tree but is not identical to it.
//!
//! Keyboard events travel the **focus chain**, not the hit chain. A key pressed
//! in a text field is offered to the field, then to its focus ancestors, up to
//! the root — which is why there is no central context enum deciding who gets a
//! key.
//!
//! Focus survives reconciliation for free: it names an element, and
//! reconciliation preserves matched elements.

pub mod intent;
pub mod policy;

pub use intent::{default_shortcuts, Intent, Shortcut};
pub use policy::{
    Directional, FocusDir, FocusEntry, FocusId, FocusScope, ReadingOrder, TraversalPolicy,
};

use std::rc::Rc;

use crate::desc::{resolve, Desc, ElemType, Modality};
use crate::element::ElementId;
use crate::event::{
    Ctl, Event, Flow, GestureKind, KeyPress, Mods, MouseButton, Phase, SelectionOnFocus,
};
use crate::render::geom::Point;
use crate::schedule::{DirtyCause, Ui};

impl<M: 'static> Ui<M> {
    /// The element that currently has focus.
    pub fn focused(&self) -> Option<ElementId> {
        self.focus
    }

    /// What the last focus acquisition asked the target to do with its
    /// selection. Clicking a field, tabbing to it and restoring it are
    /// different operations, and the difference is carried by the request.
    pub fn focus_selection(&self) -> SelectionOnFocus {
        self.focus_selection
    }

    /// Whether this element has focus, or contains the element that does. A
    /// node that renders differently because a *descendant* is focused — an
    /// active split's border — reads this.
    pub fn has_focus_within(&self, id: ElementId) -> bool {
        let Some(f) = self.focus else { return false };
        let mut cur = Some(f);
        while let Some(c) = cur {
            if c == id {
                return true;
            }
            cur = self.arena.get(c).and_then(|e| e.parent);
        }
        false
    }

    pub fn request_focus(&mut self, id: ElementId, sel: SelectionOnFocus) {
        let mut out = Vec::new();
        self.focus_element(id, sel, &mut out);
        self.pending_messages.extend(out);
    }

    /// Move focus. The order is fixed: the old element is told it lost focus
    /// before the new one is told it gained it.
    pub(crate) fn focus_element(&mut self, id: ElementId, sel: SelectionOnFocus, out: &mut Vec<M>) {
        let old = self.focus;
        if old == Some(id) {
            self.focus_selection = sel;
            return;
        }
        self.focus = Some(id);
        self.focus_selection = sel;
        if let Some(o) = old {
            self.fire_focus_change(o, false, out);
        }
        self.fire_focus_change(id, true, out);
        self.invalidate_focus_within(old, Some(id));
    }

    pub fn blur(&mut self) {
        let mut out = Vec::new();
        if let Some(o) = self.focus.take() {
            self.fire_focus_change(o, false, &mut out);
            self.invalidate_focus_within(Some(o), None);
        }
        self.pending_messages.extend(out);
    }

    fn fire_focus_change(&mut self, id: ElementId, gained: bool, out: &mut Vec<M>) {
        let handler = match self.arena.get(id).map(|e| &e.desc) {
            Some(d) => match &resolve(d).desc {
                Desc::Focusable(f) => f.on_focus_change.clone(),
                _ => None,
            },
            None => None,
        };
        let Some(h) = handler else { return };
        let mut ev = self.synth_event(
            id,
            if gained {
                GestureKind::FocusGained
            } else {
                GestureKind::FocusLost
            },
            None,
            Rc::new(Ctl::default()),
        );
        ev.selection = self.focus_selection;
        if let Some(m) = h(&ev) {
            out.push(m);
        }
    }

    fn synth_event(
        &self,
        id: ElementId,
        kind: GestureKind,
        key: Option<KeyPress>,
        ctl: Rc<Ctl>,
    ) -> Event {
        Event {
            kind,
            pos: Point::ZERO,
            local: Point::ZERO,
            button: MouseButton::Left,
            mods: key.map(|k| k.mods).unwrap_or(Mods::NONE),
            delta: 0,
            key,
            selection: SelectionOnFocus::None,
            target: id,
            current: id,
            phase: Phase::Target,
            ctl,
        }
    }

    /// Only registrants below the common ancestor of the old and new positions
    /// are invalidated: for the common ancestor and everything above it, the
    /// answer to "is focus inside me" did not change.
    fn invalidate_focus_within(&mut self, old: Option<ElementId>, new: Option<ElementId>) {
        let ca = match (old, new) {
            (Some(a), Some(b)) => self.common_ancestor(a, b),
            _ => None,
        };
        for end in [old, new].into_iter().flatten() {
            let mut cur = Some(end);
            while let Some(c) = cur {
                if Some(c) == ca {
                    break;
                }
                if self.registers_focus_within(c) {
                    self.mark_dirty(c, DirtyCause::Focus);
                }
                cur = self.arena.get(c).and_then(|e| e.parent);
            }
        }
    }

    fn registers_focus_within(&self, id: ElementId) -> bool {
        match self.arena.get(id).map(|e| &e.desc) {
            Some(d) => matches!(&resolve(d).desc, Desc::Focusable(f) if f.focus_within),
            None => false,
        }
    }

    fn common_ancestor(&self, a: ElementId, b: ElementId) -> Option<ElementId> {
        let pa = self.path_to(a);
        let pb = self.path_to(b);
        let mut best = None;
        for (x, y) in pa.iter().zip(pb.iter()) {
            if x == y {
                best = Some(*x);
            } else {
                break;
            }
        }
        best
    }

    // -- the focusable set ---------------------------------------------------

    /// The focusables traversal can reach right now.
    ///
    /// A modal layer confines traversal to itself; otherwise the nearest
    /// enclosing focus scope applies; otherwise the whole tree.
    pub fn focus_scope(&self) -> FocusScope {
        let mut nodes = Vec::new();
        if let Some(r) = self.scope_root() {
            self.collect_focusables(r, &mut nodes);
        }
        FocusScope { nodes }
    }

    fn scope_root(&self) -> Option<ElementId> {
        if let Some(&(lid, _)) = self
            .pending_layers
            .iter()
            .rev()
            .find(|(l, _)| self.is_modal(*l))
        {
            return Some(lid);
        }
        if let Some(f) = self.focus {
            let mut cur = Some(f);
            while let Some(c) = cur {
                if self.opens_scope(c) {
                    return Some(c);
                }
                cur = self.arena.get(c).and_then(|e| e.parent);
            }
        }
        self.root
    }

    fn is_modal(&self, id: ElementId) -> bool {
        match self.arena.get(id).map(|e| &e.desc) {
            Some(d) => matches!(&resolve(d).desc, Desc::Layer(l) if l.modality != Modality::None),
            None => false,
        }
    }

    fn opens_scope(&self, id: ElementId) -> bool {
        match self.arena.get(id).map(|e| &e.desc) {
            Some(d) => matches!(&resolve(d).desc, Desc::Focusable(f) if f.scope),
            None => false,
        }
    }

    fn collect_focusables(&self, id: ElementId, out: &mut Vec<FocusEntry>) {
        let Some(el) = self.arena.get(id) else { return };
        if el.ty == ElemType::Focusable {
            if let Desc::Focusable(f) = &resolve(&el.desc).desc {
                if !f.skip {
                    out.push(FocusEntry {
                        id,
                        ordinal: f.ordinal,
                        rect: el.layout.rect,
                    });
                }
            }
        }
        for c in el.children.clone() {
            self.collect_focusables(c, out);
        }
    }

    /// Move focus in a direction, using the installed traversal policy.
    pub fn move_focus(&mut self, dir: FocusDir) -> bool {
        let scope = self.focus_scope();
        match self.traversal.next(&scope, self.focus, dir) {
            Some(n) => {
                let mut out = Vec::new();
                self.focus_element(n, SelectionOnFocus::SelectAll, &mut out);
                self.pending_messages.extend(out);
                true
            }
            None => false,
        }
    }

    /// Settle focus after a frame.
    ///
    /// Three cases, in order: focus is already inside the active scope and
    /// nothing happens; a modal has just opened and focus moves into it,
    /// remembering where it was; the modal has closed and focus goes back.
    pub(crate) fn apply_autofocus(&mut self) {
        let modal = self
            .pending_layers
            .iter()
            .rev()
            .find(|(l, _)| self.is_modal(*l))
            .map(|(l, _)| *l);
        let Some(root) = modal.or(self.root) else {
            return;
        };

        let inside = self
            .focus
            .is_some_and(|f| self.arena.get(f).is_some() && self.is_within(f, root));
        if inside {
            return;
        }

        if modal.is_some() {
            // Entering a scope: remember where focus was so it can come back.
            if let Some(f) = self.focus.filter(|f| self.arena.get(*f).is_some()) {
                self.focus_restore = Some(f);
            }
        } else if let Some(prev) = self.focus_restore.take() {
            if self.arena.get(prev).is_some() {
                self.focus = None;
                let mut out = Vec::new();
                self.focus_element(prev, SelectionOnFocus::Preserve, &mut out);
                self.pending_messages.extend(out);
                return;
            }
        }

        self.focus = None;
        let mut nodes = Vec::new();
        self.collect_focusables(root, &mut nodes);
        let wanted = nodes
            .iter()
            .find(|e| {
                matches!(self.arena.get(e.id).map(|x| &x.desc), Some(d)
                    if matches!(&resolve(d).desc, Desc::Focusable(f) if f.autofocus))
            })
            // A scope with nothing marked still needs somewhere for traversal
            // to start, or Tab inside a modal would do nothing.
            .or_else(|| modal.and(nodes.first()));
        if let Some(e) = wanted {
            let id = e.id;
            let mut out = Vec::new();
            self.focus_element(id, SelectionOnFocus::SelectAll, &mut out);
            self.pending_messages.extend(out);
        }
    }

    fn is_within(&self, mut id: ElementId, root: ElementId) -> bool {
        loop {
            if id == root {
                return true;
            }
            match self.arena.get(id).and_then(|e| e.parent) {
                Some(p) => id = p,
                None => return false,
            }
        }
    }

    // -- key routing ---------------------------------------------------------

    /// Keys travel the focus chain: the focused element, then each of its
    /// ancestors. Raw listeners run first; whatever they decline is resolved to
    /// an intent and offered to the same chain as an action.
    pub(crate) fn dispatch_key(&mut self, k: KeyPress, out: &mut Vec<M>) {
        let chain: Vec<ElementId> = match self.focus {
            Some(f) => self.path_to(f),
            None => self.root.map(|r| vec![r]).unwrap_or_default(),
        };
        if self.propagate_key(&chain, k, out) {
            return;
        }
        if let Some(intent) = self.resolve_intent(&chain, k) {
            if self.run_action(&chain, intent, k, out) {
                return;
            }
            if self.default_for_intent(intent) {
                return;
            }
        }
        self.dismiss_for_key(k, out);
    }

    fn propagate_key(&mut self, chain: &[ElementId], k: KeyPress, out: &mut Vec<M>) -> bool {
        let Some(&target) = chain.last() else {
            return false;
        };
        let ctl = Rc::new(Ctl::default());
        for &n in chain.iter().rev() {
            let handlers = match self.arena.get(n).map(|e| &e.desc) {
                Some(d) => match &resolve(d).desc {
                    Desc::Focusable(f) => f.on_key.clone(),
                    _ => Vec::new(),
                },
                None => Vec::new(),
            };
            if handlers.is_empty() {
                continue;
            }
            let mut ev = self.synth_event(n, GestureKind::Key, Some(k), ctl.clone());
            ev.target = target;
            ev.phase = if n == target {
                Phase::Target
            } else {
                Phase::Bubble
            };
            for h in handlers {
                if let Some(m) = h(&ev) {
                    out.push(m);
                }
                if ctl.flow.get() == Flow::Stop {
                    break;
                }
            }
            if ctl.flow.get() == Flow::Stop {
                self.apply_focus_controls(&ctl, out);
                return true;
            }
        }
        self.apply_focus_controls(&ctl, out);
        false
    }

    fn apply_focus_controls(&mut self, ctl: &Ctl, out: &mut Vec<M>) {
        if let Some((id, sel)) = ctl.focus_request.take() {
            self.focus_element(id, sel, out);
        }
    }

    fn resolve_intent(&self, chain: &[ElementId], k: KeyPress) -> Option<Intent> {
        for &n in chain.iter().rev() {
            if let Some(d) = self.arena.get(n).map(|e| &e.desc) {
                if let Desc::Focusable(f) = &resolve(d).desc {
                    if let Some(s) = f.shortcuts.iter().find(|s| s.key == k) {
                        return Some(s.intent);
                    }
                }
            }
        }
        self.shortcuts.iter().find(|s| s.key == k).map(|s| s.intent)
    }

    /// The same intent resolves to a different action depending on where focus
    /// is. Nearest wins.
    fn run_action(
        &mut self,
        chain: &[ElementId],
        intent: Intent,
        k: KeyPress,
        out: &mut Vec<M>,
    ) -> bool {
        for &n in chain.iter().rev() {
            let handler = match self.arena.get(n).map(|e| &e.desc) {
                Some(d) => match &resolve(d).desc {
                    Desc::Focusable(f) => f
                        .actions
                        .iter()
                        .find(|(i, _)| *i == intent)
                        .map(|(_, h)| h.clone()),
                    _ => None,
                },
                None => None,
            };
            let Some(h) = handler else { continue };
            let ctl = Rc::new(Ctl::default());
            let ev = self.synth_event(n, GestureKind::Key, Some(k), ctl.clone());
            if let Some(m) = h(&ev) {
                out.push(m);
            }
            self.apply_focus_controls(&ctl, out);
            return true;
        }
        false
    }

    fn default_for_intent(&mut self, intent: Intent) -> bool {
        match intent {
            Intent::Next => self.move_focus(FocusDir::Next),
            Intent::Prev => self.move_focus(FocusDir::Prev),
            Intent::Up => self.move_focus(FocusDir::Up),
            Intent::Down => self.move_focus(FocusDir::Down),
            Intent::Left => self.move_focus(FocusDir::Left),
            Intent::Right => self.move_focus(FocusDir::Right),
            _ => false,
        }
    }

    /// Replace the global shortcut map. Per-`Focusable` shortcuts still win.
    pub fn set_shortcuts(&mut self, s: Vec<Shortcut>) {
        self.shortcuts = s;
    }

    pub fn set_traversal_policy(&mut self, p: Box<dyn TraversalPolicy>) {
        self.traversal = p;
    }
}
