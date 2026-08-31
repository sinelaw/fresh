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
pub mod tree;

pub use intent::{default_shortcuts, Intent, Shortcut};
pub use policy::{
    Directional, FocusDir, FocusEntry, FocusScope, FocusTarget, ReadingOrder, TraversalPolicy,
};
pub use tree::FocusId;

/// Focus registration as a behavior.
///
/// A component that becomes focusable this way needs no `Focusable`
/// description around it — build order step 3's rule that every later
/// primitive is a behavior. Everything the description form offers is offered
/// here: traversal position, key listeners, shortcuts, actions, focus
/// transitions and focus-within invalidation. The routing code never learns
/// which of the two forms is in play.
pub struct Focusable<M: 'static> {
    pub(crate) on_change: Option<crate::desc::Handler<M>>,
    pub(crate) on_key: Vec<crate::desc::Handler<M>>,
    pub(crate) shortcuts: Vec<Shortcut>,
    pub(crate) actions: Vec<(Intent, crate::desc::Handler<M>)>,
    pub(crate) ordinal: Option<i32>,
    pub(crate) skip: bool,
    pub(crate) scope: bool,
    pub(crate) focus_within: bool,
    pub(crate) autofocus: bool,
}

impl<M: 'static> Focusable<M> {
    pub fn new(on_change: crate::desc::Handler<M>) -> Self {
        Focusable {
            on_change: Some(on_change),
            ..Focusable::bare()
        }
    }

    /// Focusable, with no transition handler.
    pub fn bare() -> Self {
        Focusable {
            on_change: None,
            on_key: Vec::new(),
            shortcuts: Vec::new(),
            actions: Vec::new(),
            ordinal: None,
            skip: false,
            scope: false,
            focus_within: false,
            autofocus: false,
        }
    }

    /// A scope that groups the focusables below it without being one itself.
    pub fn scope() -> Self {
        Focusable {
            skip: true,
            scope: true,
            ..Focusable::bare()
        }
    }

    pub fn ordinal(mut self, n: i32) -> Self {
        self.ordinal = Some(n);
        self
    }

    pub fn skip(mut self) -> Self {
        self.skip = true;
        self
    }

    /// Take focus when the enclosing scope opens.
    pub fn autofocus(mut self) -> Self {
        self.autofocus = true;
        self
    }

    /// Rebuild the owning element when focus enters or leaves its subtree.
    pub fn focus_within(mut self) -> Self {
        self.focus_within = true;
        self
    }

    /// A raw key listener, offered before intents are resolved.
    pub fn on_key(mut self, h: crate::desc::Handler<M>) -> Self {
        self.on_key.push(h);
        self
    }

    /// A chord this subtree reads differently from the global map.
    pub fn shortcut(mut self, s: Shortcut) -> Self {
        self.shortcuts.push(s);
        self
    }

    /// How this part of the interface carries out an intent.
    pub fn action(mut self, i: Intent, h: crate::desc::Handler<M>) -> Self {
        self.actions.push((i, h));
        self
    }
}

impl<M: 'static> crate::behavior::Behavior for Focusable<M> {
    fn behavior_name(&self) -> &'static str {
        "Focusable"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

use std::rc::Rc;

use crate::desc::{resolve, Desc, Modality};
use crate::element::ElementId;
use crate::event::{
    Axis, Ctl, Event, Flow, GestureKind, KeyPress, Mods, MouseButton, Phase, SelectionOnFocus,
};
use crate::render::geom::Point;
use crate::schedule::{DirtyCause, Ui};

/// An element's focus behaviour, wherever it was declared: on a `Focusable`
/// description, or on a `Focusable` behavior a component registered during
/// `init`. Key routing reads this and never the description, so the two forms
/// are the same thing to everything downstream.
pub(crate) struct FocusConfig<M: 'static> {
    pub on_key: Vec<crate::desc::Handler<M>>,
    pub shortcuts: Vec<Shortcut>,
    pub actions: Vec<(Intent, crate::desc::Handler<M>)>,
    pub on_change: Option<crate::desc::Handler<M>>,
}

impl<M: 'static> Ui<M> {
    pub(crate) fn focus_config(&self, id: ElementId) -> Option<FocusConfig<M>> {
        let el = self.arena.get(id)?;
        if let Desc::Focusable(f) = &resolve(&el.desc).desc {
            return Some(FocusConfig {
                on_key: f.on_key.clone(),
                shortcuts: f.shortcuts.clone(),
                actions: f.actions.clone(),
                on_change: f.on_focus_change.clone(),
            });
        }
        el.behaviors.iter().find_map(|b| {
            b.as_any()
                .downcast_ref::<Focusable<M>>()
                .map(|f| FocusConfig {
                    on_key: f.on_key.clone(),
                    shortcuts: f.shortcuts.clone(),
                    actions: f.actions.clone(),
                    on_change: f.on_change.clone(),
                })
        })
    }
}

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

    #[track_caller]
    pub fn request_focus(&mut self, id: ElementId, sel: SelectionOnFocus) {
        debug_assert!(
            self.sched.borrow().building.is_none(),
            "request_focus during build: focus is not a function of the description"
        );
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

    pub(crate) fn fire_focus_change(&mut self, id: ElementId, gained: bool, out: &mut Vec<M>) {
        let Some(h) = self.focus_config(id).and_then(|c| c.on_change) else {
            return;
        };
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
            clicks: 1,
            delta: 0,
            axis: Axis::Vertical,
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
        self.arena
            .get(id)
            .and_then(|e| e.focus)
            .and_then(|f| self.focus_tree.get(f))
            .is_some_and(|n| n.reg.focus_within)
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
    /// Read from the focus tree, so the cost is the size of the focusable set
    /// rather than the size of the element tree. A modal layer confines
    /// traversal to itself; otherwise the nearest enclosing scope applies;
    /// otherwise the whole tree.
    pub fn focus_scope(&self) -> FocusScope {
        let mut nodes = Vec::new();
        match self.active_scope() {
            Some(f) => {
                let kids = self
                    .focus_tree
                    .get(f)
                    .map(|n| n.children.clone())
                    .unwrap_or_default();
                for c in kids {
                    self.collect_focus(c, &mut nodes);
                }
                // A scope that is itself focusable is reachable inside itself.
                if self.focus_tree.get(f).is_some_and(|n| !n.reg.skip) {
                    self.push_focus(f, &mut nodes);
                }
            }
            None => {
                for r in self.focus_roots.clone() {
                    self.collect_focus(r, &mut nodes);
                }
            }
        }
        // **The layer confines the set, not a registration inside it.**
        //
        // `active_scope` asks the topmost keyboard-owning layer for a focus
        // registration — and a `layer()` has none, ever, because a layer is
        // not focusable. So the confinement this function's own doc promises
        // silently did not happen unless something in the layer *also*
        // declared `focus_scope()`, and traversal walked straight out of a
        // modal into the frame behind it.
        //
        // That is worse than a stray highlight. `move_focus` returning true
        // *claims the key*, so Tab and the arrows a modal declines were spent
        // moving focus out of it: a completion popup's pass-through dismissal
        // never ran (Left, Right and Shift+Tab were swallowed), and Tab in a
        // dialog left the dialog instead of reaching its next button.
        //
        // Filtering here rather than in `active_scope` because both answers
        // are wanted: a scope *inside* a modal is narrower still, and this
        // leaves it alone.
        if let Some(m) = self.topmost_modal() {
            nodes.retain(|n| self.is_within(n.id, m));
        }
        FocusScope { nodes }
    }

    /// The topmost layer that owns the keyboard, as an element — which is
    /// what confines focus traversal.
    pub(crate) fn topmost_modal(&self) -> Option<ElementId> {
        let i = self.topmost_modal_index(Modality::owns_keyboard)?;
        self.element_of(self.pending_layers[i].0)
    }

    /// Where the topmost layer answering `channel` sits in resolution order.
    /// Everything resolved after it is above it and stays live; everything
    /// before it is inert *on that channel*.
    ///
    /// **The channel is a parameter because the answer differs by channel.**
    /// A menu owns every key while the bar it hangs from stays clickable, so
    /// asking one question for both put the pointer floor under a layer that
    /// had only claimed the keyboard.
    pub(crate) fn topmost_modal_index(&self, channel: fn(Modality) -> bool) -> Option<usize> {
        (0..self.pending_layers.len()).rev().find(|&i| {
            self.layer_geom(self.pending_layers[i].0)
                .is_some_and(|g| channel(g.modality))
        })
    }

    /// The topmost layer that takes the pointer away from what is behind it.
    pub(crate) fn topmost_pointer_modal(&self) -> Option<ElementId> {
        let i = self.topmost_modal_index(Modality::blocks_pointer)?;
        self.element_of(self.pending_layers[i].0)
    }

    /// The registration that groups what traversal may currently reach.
    pub(crate) fn active_scope(&self) -> Option<crate::focus::FocusId> {
        if let Some(e) = self.topmost_modal() {
            if let Some(f) = self.arena.get(e).and_then(|el| el.focus) {
                return Some(f);
            }
        }
        // Otherwise the nearest scope above where focus is.
        let mut cur = self
            .focus
            .and_then(|e| self.arena.get(e))
            .and_then(|el| el.focus);
        while let Some(f) = cur {
            let n = self.focus_tree.get(f)?;
            if n.reg.scope {
                return Some(f);
            }
            cur = n.parent;
        }
        None
    }

    fn collect_focus(&self, f: crate::focus::FocusId, out: &mut Vec<FocusEntry>) {
        let Some(n) = self.focus_tree.get(f) else {
            return;
        };
        if !n.reg.skip {
            self.push_focus(f, out);
        }
        for c in n.children.clone() {
            self.collect_focus(c, out);
        }
    }

    fn push_focus(&self, f: crate::focus::FocusId, out: &mut Vec<FocusEntry>) {
        let Some(n) = self.focus_tree.get(f) else {
            return;
        };
        out.push(FocusEntry {
            id: n.element,
            ordinal: n.reg.ordinal,
            rect: self.rect_of(n.element),
        });
    }

    /// Whether an element is inside the scope traversal is currently confined
    /// to.
    fn in_active_scope(&self, e: ElementId) -> bool {
        // The same two confinements `focus_scope` applies, in the same order.
        if let Some(m) = self.topmost_modal() {
            if !self.is_within(e, m) {
                return false;
            }
        }
        match self.active_scope() {
            None => true,
            Some(scope) => {
                let root = match self.focus_tree.get(scope) {
                    Some(n) => n.element,
                    None => return true,
                };
                self.is_within(e, root)
            }
        }
    }

    /// Move focus in a direction, using the installed traversal policy.
    pub fn move_focus(&mut self, dir: FocusDir) -> bool {
        let scope = self.focus_scope();
        match self.traversal.next(&scope, self.focus, dir) {
            // **Landing where you started is not a move.**
            //
            // Reading order wraps, so a scope holding one focusable answers
            // every direction with that same element. Returning true for it
            // *claims the key* — `dispatch_key` stops at
            // `default_for_intent` — so a modal with a single focusable in it
            // swallowed every arrow and every Tab, and none of them reached
            // the layer's `Dismiss` behind. The editor's completion popup is
            // exactly one focusable inside one layer, which is why Left,
            // Right and Shift+Tab there did nothing at all rather than
            // closing it.
            Some(n) if Some(n) != self.focus => {
                let mut out = Vec::new();
                self.focus_element(n, SelectionOnFocus::SelectAll, &mut out);
                self.pending_messages.extend(out);
                true
            }
            _ => false,
        }
    }

    /// Settle focus after a frame.
    ///
    /// Three cases, in order: focus is already inside the active scope and
    /// nothing happens; a scope has just opened and focus moves into it,
    /// remembering where it was; the scope has closed and focus goes back.
    pub(crate) fn apply_autofocus(&mut self) {
        let alive = self.focus.is_some_and(|f| self.arena.get(f).is_some());
        if alive && self.focus.is_some_and(|f| self.in_active_scope(f)) {
            return;
        }
        let modal = self.topmost_modal();

        if modal.is_some() {
            // Entering a scope: remember where focus was so it can come back.
            if let Some(f) = self.focus.filter(|f| self.arena.get(*f).is_some()) {
                self.focus_restore = Some(f);
            }
        } else if let Some(prev) = self.focus_restore.take() {
            if self.arena.get(prev).is_some() {
                let mut out = Vec::new();
                self.focus_element(prev, SelectionOnFocus::Preserve, &mut out);
                self.pending_messages.extend(out);
                return;
            }
        }

        let scope = self.focus_scope();
        let wanted = scope
            .nodes
            .iter()
            .find(|e| {
                self.arena
                    .get(e.id)
                    .and_then(|x| x.focus)
                    .and_then(|f| self.focus_tree.get(f))
                    .is_some_and(|n| n.reg.autofocus)
            })
            // A scope with nothing marked still needs somewhere for traversal
            // to start, or Tab inside a modal would do nothing.
            .or_else(|| modal.and(scope.nodes.first()));
        let mut out = Vec::new();
        match wanted {
            Some(e) => {
                let id = e.id;
                self.focus_element(id, SelectionOnFocus::SelectAll, &mut out);
            }
            // Focus was somewhere the active scope cannot reach and there is
            // nowhere to move it to. Losing focus is still a transition, and
            // the element that had it is told so.
            None => {
                if let Some(o) = self.focus.take() {
                    self.fire_focus_change(o, false, &mut out);
                    self.invalidate_focus_within(Some(o), None);
                }
            }
        }
        self.pending_messages.extend(out);
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
    /// Reports whether anything claimed the key, so a host with its own
    /// pipeline behind this tree knows not to act on it as well.
    pub(crate) fn dispatch_key(&mut self, k: KeyPress, out: &mut Vec<M>) -> bool {
        let chain: Vec<ElementId> = match self.focus {
            Some(f) => self.path_to(f),
            None => self.root.map(|r| vec![r]).unwrap_or_default(),
        };
        if self.propagate_key(&chain, k, out) {
            return true;
        }
        if let Some(intent) = self.resolve_intent(&chain, k) {
            if self.run_action(&chain, intent, k, out) {
                return true;
            }
            if self.default_for_intent(intent) {
                return true;
            }
        }
        // A key that dismisses a layer is answered by that layer: Escape
        // closing a menu is the menu's reply, not a key that also belongs to
        // whatever is behind it.
        let (dismissed, spent) = self.dismiss_for_key(k, out);
        if spent {
            return true;
        }
        // **A layer that dismissed itself passing through is out of the way.**
        // It said so: `Dismiss::passing_through` is "close, and let the input
        // reach what it was aimed at". A completion list is the shape — Enter
        // there means "close this and insert a newline", and the newline is
        // the user's, not the popup's — so the modal claim below must not put
        // the layer back in front of a key it has just stepped out of.
        if dismissed {
            return false;
        }
        // **A modal layer owns the keyboard, including the keys it declines.**
        // Nothing above acted on this one, and focus is inside a layer that
        // took the keyboard away from what is behind it — so the key stops
        // here. That is what modal means to a keyboard, and it is the last
        // thing a host with its own pipeline behind this tree needs told.
        self.key_stops_at_modal()
    }

    /// Whether focus sits inside a layer that *swallows* the keys it does not
    /// act on.
    ///
    /// A host with its own input pipeline behind this tree asks this about the
    /// keys the tree has no vocabulary for: it cannot route them, and letting
    /// them past a modal surface would reach what the modal is covering. A
    /// `Modality::Focus` layer answers `false` — confining focus is not the
    /// same as taking the key away from the host, and that layer's whole
    /// point is that what it declines is still the host's to resolve.
    pub fn keyboard_owned(&self) -> bool {
        self.key_stops_at_modal()
    }

    /// Whether focus sits inside a layer that owns the keyboard.
    ///
    /// The chain is the focused element's ancestors, and a layer's element is
    /// one of them, so this asks the same containment question traversal does
    /// — no separate stack of "who owns the keyboard" and no ranking of
    /// surfaces. With nothing focused there is no chain and nothing owns it.
    ///
    /// **Confining and swallowing are different questions**, and this is the
    /// swallowing one: a `Modality::Focus` layer is on the chain and got the
    /// key first, and still lets what it declined carry on to the host.
    fn key_stops_at_modal(&self) -> bool {
        self.focus_in_layer(crate::desc::Modality::swallows_keys)
    }

    /// Whether focus sits inside a layer that *confines* it — the other half
    /// of [`Modality`](crate::desc::Modality), and a different question from
    /// [`keyboard_owned`](Self::keyboard_owned).
    ///
    /// **A surface can own the keyboard and still hand back what it declines.**
    /// That is exactly a `Modality::Focus` layer: an open prompt is
    /// unambiguously where the keyboard is, and the keys it does not bind are
    /// still the host's. `keyboard_owned` answers the second half and says
    /// *no* for it; this answers the first and says *yes*.
    ///
    /// A host with surfaces of its own behind the tree needs both, because
    /// "may I intercept this key" and "may I resolve this key" are not the
    /// same permission. An unfocused popup asks the first: while anything
    /// above it owns the keyboard it must not intercept, whether or not that
    /// thing swallows. Answering it by containment is what replaces asking a
    /// ranked list of surfaces where the popup sits in it.
    pub fn focus_confined(&self) -> bool {
        self.focus_in_layer(crate::desc::Modality::owns_keyboard)
    }

    /// The containment question both halves are asking, with the half as an
    /// argument: is any layer on the focused element's ancestor chain one this
    /// predicate accepts.
    fn focus_in_layer(&self, half: fn(crate::desc::Modality) -> bool) -> bool {
        let Some(f) = self.focus else { return false };
        self.path_to(f).iter().any(|&n| {
            self.render_for(n)
                .and_then(|r| self.layer_geom(r))
                .is_some_and(|g| half(g.modality))
        })
    }

    fn propagate_key(&mut self, chain: &[ElementId], k: KeyPress, out: &mut Vec<M>) -> bool {
        let Some(&target) = chain.last() else {
            return false;
        };
        let ctl = Rc::new(Ctl::default());
        for &n in chain.iter().rev() {
            let handlers = self.focus_config(n).map(|c| c.on_key).unwrap_or_default();
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
            if let Some(c) = self.focus_config(n) {
                if let Some(s) = c.shortcuts.iter().find(|s| s.key == k) {
                    return Some(s.intent);
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
            let handler = self.focus_config(n).and_then(|c| {
                c.actions
                    .iter()
                    .find(|(i, _)| *i == intent)
                    .map(|(_, h)| h.clone())
            });
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
        // **A direction needs somewhere to move from.**
        //
        // Tab enters the interface from nowhere — that is what Tab is for, and
        // `move_focus` picking a first node is the right answer for it. An
        // arrow key is not that gesture. With nothing focused it belongs to
        // whoever else is listening, and in a host that offers its keys to this
        // tree before its own handlers, "whoever else" is the application.
        //
        // The editor found this the expensive way: adding one focusable to the
        // frame — a suggestion list that is driven by the app and has no
        // keyboard of its own — gave directional traversal somewhere to go, so
        // `Right` in a command palette started moving focus instead of the
        // text cursor. `Home` was unaffected, which is the tell: it has no
        // default here.
        let directional = matches!(
            intent,
            Intent::Up | Intent::Down | Intent::Left | Intent::Right
        );
        if directional && self.focus.is_none() {
            return false;
        }
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
