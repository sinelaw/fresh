//! Chrome surfaces as REGISTERED components.
//!
//! This is slice 0 of `docs/internal/chrome-event-model-plan.md`: the
//! central `chrome_boxes()` enumeration is dissolved into one
//! [`ChromeComponent`] per surface, each contributing its own boxes to
//! the per-event tree from the live state and paint caches it owns.
//! The registry below ([`components`]) is the ONE list — the chrome
//! analogue of `widgets::kinds::behavior()`. Later slices grow the
//! trait (per-gesture handlers, `hit_stack` dispatch, parent links,
//! keyboard) without moving this code again.
//!
//! The tree is DERIVED from live state — never hand-maintained (derivation
//! is what keeps stale-geometry races impossible; see the plan's "what NOT
//! to do"). It IS memoized, though: [`chrome_tree`] caches the last build
//! and reuses it only when a VALIDATED claim holds — the coarse `ui_gen`
//! epoch matches AND a fresh (cheap, never-memoized) `overlay_stack` build
//! equals the snapshot the cached tree was built from. Staleness is
//! checked, not trusted: surface changes from any Editor API show up in
//! the stack comparison without a hand-maintained bump roster, and debug
//! builds oracle-check every hit against a full rebuild besides.

mod base;
mod context_menu;
mod dock;
mod file_explorer;
mod floating_modal;
mod menu;
mod modals;
mod popups;
mod prompt;
mod splits;
mod status_bar;

use super::types::HoverTarget;
use super::Editor;

/// Overlay-layer ranks, DESCENDING = higher keyboard/modal precedence.
/// Deliberately independent of both registry order and box z: the menu
/// LAYER outranks the prompt's and the popups' (an open menu owns the
/// keyboard over them) while its boxes sit in a lower pointer band,
/// and the native context menus rank BELOW the popup layer (the
/// unfocused-popup guard's `take_while` must not see them) while their
/// boxes sit at the very top. Event-debug (1000) is hardcoded in
/// `overlay_layers` — a debugging instrument, not a component.
pub(crate) mod layer_rank {
    pub(crate) const SETTINGS: u16 = 900;
    pub(crate) const KEYBINDING_EDITOR: u16 = 890;
    pub(crate) const CALIBRATION_WIZARD: u16 = 880;
    pub(crate) const WORKSPACE_TRUST: u16 = 870;
    pub(crate) const MENU: u16 = 860;
    pub(crate) const PROMPT: u16 = 850;
    pub(crate) const POPUP: u16 = 840;
    pub(crate) const CONTEXT_MENU: u16 = 830;
    pub(crate) const FLOATING_MODAL: u16 = 820;
    pub(crate) const DOCK: u16 = 810;
    pub(crate) const EDITOR_BASE: u16 = 0;
}
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

/// Shared cell-in-rect test for component geometry.
pub(crate) fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

/// One box in the per-event chrome tree: the geometry (the SAME
/// `LayoutBox` type the panel model uses, so hit math and flags are
/// shared) plus which registered component owns it — dispatch calls
/// the owner's handlers instead of matching kind strings.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ChromeBox {
    pub lb: LayoutBox,
    /// Index into [`components`].
    pub owner: usize,
}

impl std::borrow::Borrow<LayoutBox> for ChromeBox {
    fn borrow(&self) -> &LayoutBox {
        &self.lb
    }
}

/// What a component did with a pointer gesture on one of its boxes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Disposition {
    /// Handled — the walk stops.
    Consumed,
    /// Side effects applied, but the event must keep routing — the
    /// act-then-continue guards (transient dismiss, explorer menu
    /// clear). NOT interchangeable with `Pass`: on a `pointer_opaque`
    /// box, `Pass` STOPS the walk (the opacity gate absorbs the
    /// event) while `PassAfter` keeps walking — an observer's
    /// continue must not be blocked by its own box's opacity. Today
    /// every PassAfter producer is a non-opaque guard, so the two
    /// only diverge if an opaque surface adopts observer semantics.
    PassAfter,
    /// Not this surface's event — the walk continues to the next box.
    Pass,
}

/// Which press gesture a pointer event carries. Triple-click ROUTES
/// through the tree like every other press (overlay swallow, popup
/// block/dismiss, then the split line-select arm); only the selection
/// semantics themselves are a buffer concern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerPress {
    Left,
    Right,
    Double,
    Triple,
}

/// One pointer gesture offered to a component's box, press kind
/// included — per-gesture behavior lives in the handler (the way
/// `WidgetImpl::on_pointer` takes `event_type`), not in one trait
/// method per press. `modifiers` matter only to the editor surface
/// (Ctrl+click follows links); guards and chrome controls ignore
/// them.
pub(crate) struct ChromePointer {
    pub press: PointerPress,
    pub col: u16,
    pub row: u16,
    pub modifiers: crossterm::event::KeyModifiers,
}

/// Per-event sink the components push their boxes into. Wraps the
/// frame dimensions so full-frame guards don't each re-read them, and
/// stamps each box with the collecting component's registry index.
///
/// ## RULING — the chrome "tree" is currently FLAT
///
/// No chrome component sets `LayoutBox.parent`: every box is a root,
/// so `hit_stack`'s structural rules (effective z = max along the
/// ancestor chain, children-above-parents) are inert at chrome level
/// — ordering is purely the boxes' own z bands plus push order within
/// a band. Nesting a box under another does NOT work here yet; the
/// full-frame guard boxes (close guards, scrims, observers) are the
/// deliberate flat-world encoding of containment ("anything outside
/// my rect"), NOT a legacy pattern to copy around — a new surface
/// should push its rects + at most one guard, and must not expect a
/// parent link to clip or lift it. `LayoutBox.parent`, `focusable`,
/// `focus_trap` and `scroll` are reserved-but-unset at chrome level
/// (they are live in the panel-local tree); parent links and the
/// chrome focus ring are the forward-design arc's work
/// (sinelaw/fresh#3024), where the guard boxes dissolve into real
/// containment.
pub(crate) struct ChromeTreeBuilder {
    frame_width: u32,
    frame_height: u32,
    current_owner: usize,
    boxes: Vec<ChromeBox>,
}

impl ChromeTreeBuilder {
    fn new(frame_width: u32, frame_height: u32) -> Self {
        ChromeTreeBuilder {
            frame_width,
            frame_height,
            current_owner: 0,
            boxes: Vec::new(),
        }
    }

    pub(crate) fn push(&mut self, lb: LayoutBox) {
        self.boxes.push(ChromeBox {
            lb,
            owner: self.current_owner,
        });
    }

    /// Frame height in rows — for components whose box spans the
    /// full column height (the dock column and its resize border).

    /// A full-frame surface — a guard/capture whose semantics ARE
    /// full-screen (close guards, absorb/dismiss, modal scrims,
    /// position-blind wheel capture), never a geometry proxy.
    pub(crate) fn full(&mut self, kind: &'static str, z: u8) {
        let mut b = LayoutBox::plain(kind, 0, 0, self.frame_width, self.frame_height);
        b.z = z;
        self.push(b);
    }

    /// A surface at its painted rectangle.
    pub(crate) fn rect(&mut self, kind: &'static str, z: u8, r: ratatui::layout::Rect) {
        let mut b = LayoutBox::plain(
            kind,
            r.y as u32,
            r.x as u32,
            r.width as u32,
            r.height as u32,
        );
        b.z = z;
        self.push(b);
    }
}

/// What one press-walk step does to the walk, given the component's
/// disposition and the box's opacity — the [`Disposition`] contract
/// as CODE (pinned by the unit tests below) instead of prose:
/// `PassAfter` continues even on an opaque box (an observer's
/// continue must not be blocked by its own box's opacity), while a
/// declined (`Pass`) opaque box absorbs the event.
pub(crate) fn pointer_walk_step(disp: Disposition, pointer_opaque: bool) -> PointerWalkStep {
    match disp {
        Disposition::Consumed => PointerWalkStep::Stop,
        Disposition::PassAfter => PointerWalkStep::Continue,
        Disposition::Pass => {
            if pointer_opaque {
                PointerWalkStep::Stop
            } else {
                PointerWalkStep::Continue
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerWalkStep {
    Stop,
    Continue,
}

/// The active pointer GRAB, if any: press-established routing that
/// owns the pointer until release. Grabs are NOT bubble dispatch — a
/// drag must keep routing to its owner even when the pointer crosses
/// an alternate-screen terminal or any other surface (the btop-resize
/// bug). The FULL press-to-release roster lives here: the terminal
/// forward sink suppresses forwarding for every grab, and
/// `handle_mouse_drag` dispatches on the grab instead of a
/// hand-ordered flag ladder. Derived from live drag state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerGrab {
    /// Dock width resize from its right border.
    DockResize,
    /// Drag-to-select in a widget markdown/text document.
    WidgetText,
    /// A floating/dock panel's list scrollbar drag.
    WidgetScrollbar,
    /// A split's vertical scrollbar (thumb-relative or track-jump).
    VScrollbar,
    /// A split's horizontal scrollbar.
    HScrollbar,
    /// Split-separator resize.
    SplitSeparator,
    /// File-explorer width resize from its border.
    ExplorerWidth,
    /// A press on a live terminal grid whose first motion converts to
    /// scrollback text selection (selection intent).
    TerminalSelectPending,
    /// Buffer text selection (the press placed the caret; drags
    /// extend the selection).
    TextSelection,
    /// A tab being dragged toward a drop zone.
    TabDrag,
}

/// The grab in effect for the current event, if any. The terminal
/// forward sink consults this instead of a hand-listed field check,
/// and the Drag arm dispatches on it. Checked in the old drag
/// ladder's order so precedence is unchanged when (rarely) two flags
/// coexist.
pub(crate) fn pointer_grab(ed: &Editor) -> Option<PointerGrab> {
    if ed.dock_resizing {
        return Some(PointerGrab::DockResize);
    }
    if ed.widget_text_drag.is_some() {
        return Some(PointerGrab::WidgetText);
    }
    if ed
        .dock
        .as_ref()
        .is_some_and(|p| p.scrollbar_drag_key.is_some())
        || ed
            .floating_widget_panel
            .as_ref()
            .is_some_and(|p| p.scrollbar_drag_key.is_some())
    {
        return Some(PointerGrab::WidgetScrollbar);
    }
    // Same grab for a scrollbar on a buffer-mounted widget panel; its
    // tracks live on the editor rather than on a panel struct.
    if ed.split_widget_scrollbar_drag.is_some() {
        return Some(PointerGrab::WidgetScrollbar);
    }
    let ms = &ed.active_window().mouse_state;
    if ms.dragging_scrollbar.is_some() {
        return Some(PointerGrab::VScrollbar);
    }
    if ms.dragging_horizontal_scrollbar.is_some() {
        return Some(PointerGrab::HScrollbar);
    }
    if ms.dragging_separator.is_some() {
        return Some(PointerGrab::SplitSeparator);
    }
    if ms.dragging_file_explorer || ms.drag_start_explorer_width.is_some() {
        return Some(PointerGrab::ExplorerWidth);
    }
    if ms.terminal_drag_pending.is_some() {
        return Some(PointerGrab::TerminalSelectPending);
    }
    if ms.dragging_text_selection {
        return Some(PointerGrab::TextSelection);
    }
    if ms.dragging_tab.is_some() {
        return Some(PointerGrab::TabDrag);
    }
    None
}

/// One registered chrome surface: `collect` contributes its boxes for
/// THIS event from the live state/caches the surface owns, and the
/// per-gesture handlers own its behavior (slice 1: hover, right-click,
/// double-click — left-click and wheel still ride the central kind
/// arrays until slice 2). Handlers receive the box they were offered
/// and decline (`None`/`Pass`) where its geometry is coarser than
/// their real target, exactly as the central match arms did.
pub(crate) trait ChromeComponent: Sync {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder);

    /// Name the hover target under the pointer, or decline so the walk
    /// falls through to the next surface down. Takes `&mut Editor` like
    /// the pointer handlers (not `&Editor` like `collect`): live-state
    /// geometry derivation can lazily load buffer chunks (the status
    /// bar's cursor-column segment), which needs the mutable borrow.
    fn hover(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
    ) -> Option<HoverTarget> {
        None
    }

    /// React to a hover-target transition (enter / leave / move),
    /// offered to EVERY component after the hover walk names the new
    /// target — the reaction half of hover, living with the surface
    /// it drives (the menu's auto-switch/submenu machine, the context
    /// menu's highlight, the explorer's status tooltip). Components
    /// key off the target variants they own; reactions are
    /// independent — one surface reacting never suppresses another's
    /// leave-reaction (the old central ladder's early returns did).
    /// Return true to request a re-render beyond the target diff
    /// itself.
    fn on_hover_change(
        &self,
        _ed: &mut Editor,
        _old: Option<&HoverTarget>,
        _new: Option<&HoverTarget>,
        _col: u16,
        _row: u16,
    ) -> bool {
        false
    }

    /// A pointer press (left / right / double, per `ev.press`) on one
    /// of this component's boxes.
    fn on_pointer(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        Ok(Disposition::Pass)
    }

    /// This component's overlay-layer contributions, from live state:
    /// `(rank, Layer)` pairs pushed into `out` (see [`layer_rank`]).
    /// `Editor::overlay_layers()` concatenates every component's
    /// contributions and sorts by rank descending — the layer stack is
    /// DERIVED from the registry, so a surface's presence, keyboard
    /// ownership, `KeyContext`, and PTY blocking are declared by its
    /// component instead of a central conditional ladder.
    fn layers(&self, _ed: &Editor, _out: &mut Vec<(u16, crate::app::overlay::Layer)>) {}

    /// Layer-targeted keyboard dispatch — THE key walk. After the
    /// pre-band (event-debug, terminal input, getNextKey capture),
    /// `Editor::dispatch_layer_keyboard` walks the
    /// owner-stamped `overlay_stack()` top-down, offering the key to
    /// each layer's declaring component through this method — the
    /// keyboard analogue of `dispatch_pointer` walking `hit_stack`
    /// over owner-stamped boxes. `None` = this layer declines and the
    /// walk continues to the next layer down; `Some` = this layer
    /// dealt with the key, with the handler's result — including
    /// `Some(Ok(Ignored))`, which still stops the walk (the
    /// query-replace confirm prompt consumes every key that way).
    /// The error channel carries `handle_action` failures up through
    /// `handle_key`, matching the old staged pipeline's `?`s.
    /// Interiors stay bespoke, per the modal-mouse ruling: the
    /// component is the dispatch slot, only ROUTING is derived.
    fn on_layer_key(
        &self,
        _ed: &mut Editor,
        _layer: &crate::app::overlay::Layer,
        _event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        None
    }

    /// A wheel delta over one of this component's boxes. `Consumed`
    /// stops the walk; a scroll surface already at its bound (or a
    /// box whose real target the pointer missed) returns `Pass` so
    /// the wheel keeps falling — scroll chaining.
    fn on_wheel(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> AnyhowResult<Disposition> {
        Ok(Disposition::Pass)
    }

    /// A HORIZONTAL wheel delta (Shift+wheel, or a native
    /// ScrollLeft/ScrollRight) over one of this component's boxes —
    /// same walk and chaining contract as [`Self::on_wheel`].
    /// Surfaces with no horizontal axis simply decline.
    fn on_hwheel(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> AnyhowResult<Disposition> {
        Ok(Disposition::Pass)
    }
}

/// The ONE chrome registry — every routable surface, once.
///
/// Ordering: every gesture scans the tree with
/// [`crate::widgets::layout_box::hit_stack`] — effective-z bands,
/// children above parents, and document order within a band. z bands
/// ride a x10 scale (180 context menu … 10 editor, 0 base) so
/// per-gesture guard/capture surfaces occupy their own slots
/// (transient dismiss at 175 above the z170 prompt/popup targets, the
/// overlay prompt's wheel modal at 160 above its position-blind
/// suggestion capture at 155, its click scrim down at 15 just above
/// the editor band). Within a band, registry order IS precedence:
/// components push specific targets before guards.
pub(crate) fn components() -> &'static [&'static dyn ChromeComponent] {
    &[
        // The modal band: whole-channel mouse capture, first-active
        // wins, ranked as `overlay_layers()` ranks their layers.
        // (FloatingModal captures too — it sits lower, after the
        // surfaces that render above it.)
        &modals::Settings,
        &modals::KeybindingEditor,
        &modals::CalibrationWizard,
        &modals::WorkspaceTrust,
        &context_menu::ContextMenu,
        &prompt::Prompt,
        &popups::Popups,
        &floating_modal::FloatingModal,
        &dock::Dock,
        &splits::Splits,
        &menu::Menu,
        &file_explorer::FileExplorer,
        &status_bar::StatusBar,
        &base::Base,
    ]
}

/// Build the chrome surface tree for one event: every component
/// contributes its live boxes, each stamped with its owner. Replaces
/// the monolithic enumeration.
pub(crate) fn chrome_tree(ed: &Editor) -> Vec<ChromeBox> {
    // VALIDATED MEMO — a hit is an equality claim that is CHECKED, not
    // trusted. Two keys, covering the tree's two input classes:
    //
    // 1. `ui_gen` match — the geometry epoch. `collect` reads paint
    //    caches (`last_frame`, per-component layout mirrors) that only
    //    move under `render`/`relayout`, and both bump. The event
    //    funnels bump too (see `bump_ui_gen`), coarsely and cheaply.
    // 2. Overlay-stack equality — the presence epoch, DERIVED instead
    //    of enumerated. `overlay_stack()` is a cheap always-fresh build
    //    of every component's activity predicates and claims; comparing
    //    it to the snapshot taken when the cached tree was built
    //    catches surface open/close/focus changes from ANY path —
    //    plugin dispatch, tests driving Editor APIs directly — without
    //    a hand-maintained bump roster (which CI's oracle proved
    //    incomplete when a counter alone was tried).
    //
    // A rebuild advances `ui_tree_seq`, giving downstream memos (the
    // hover-cell skip) a "tree provably unchanged" token. Debug builds
    // still oracle-check every hit against a fresh build, so any input
    // this two-key scheme fails to cover dies as an assertion instead
    // of routing an event through a stale tree.
    let stack = ed.overlay_stack();
    if let Some((gen, built_from, cached)) = ed.chrome_tree_memo.borrow().as_ref() {
        if *gen == ed.ui_gen && *built_from == stack {
            debug_assert_eq!(
                cached,
                &chrome_tree_uncached(ed),
                "chrome_tree memo hit diverges from live state — an input \
                 changed under an unchanged ui_gen + overlay stack"
            );
            return cached.clone();
        }
    }
    let fresh = chrome_tree_uncached(ed);
    *ed.chrome_tree_memo.borrow_mut() = Some((ed.ui_gen, stack, fresh.clone()));
    ed.ui_tree_seq.set(ed.ui_tree_seq.get().wrapping_add(1));
    fresh
}

fn chrome_tree_uncached(ed: &Editor) -> Vec<ChromeBox> {
    let frame = ed.active_chrome().last_frame;
    let mut t = ChromeTreeBuilder::new(frame.width as u32, frame.height as u32);
    for (i, c) in components().iter().enumerate() {
        t.current_owner = i;
        c.collect(ed, &mut t);
    }
    t.boxes
}

#[cfg(test)]
mod tests {
    use super::layer_rank::*;

    /// The rank block is the ONE precedence source for the keyboard
    /// walk, the PTY gate, `get_key_context`,
    /// and `popup_blocked_by_higher_modal` — a one-character edit here
    /// changes behavior in five places, so the deliberate relations
    /// are pinned. Each assert names the behavior that regresses if it
    /// flips.
    #[test]
    fn deliberate_rank_relations_are_pinned() {
        // The capture-all modal band outranks everything routable.
        for modal in [SETTINGS, KEYBINDING_EDITOR, CALIBRATION_WIZARD] {
            for below in [WORKSPACE_TRUST, MENU, PROMPT, POPUP] {
                assert!(modal > below, "modal band must own the keyboard first");
            }
        }
        // Workspace-trust keys beat an open prompt — the deliberate
        // convergence fix of the K arc (dispatch now agrees with
        // `get_key_context`, which always ranked WT higher).
        assert!(WORKSPACE_TRUST > PROMPT);
        // An open menu owns the keyboard over the prompt and popups.
        assert!(MENU > PROMPT && MENU > POPUP);
        // The prompt outranks the popup band (block order of the old
        // dispatch_modal_input, preserved as ranks).
        assert!(PROMPT > POPUP);
        // Context menus rank BELOW the popup layer — the
        // `popup_blocked_by_higher_modal` take_while must not see
        // them (their keyboard precedence is the pre-band grab, by
        // ruling; the rank is deliberately NOT it).
        assert!(CONTEXT_MENU < POPUP);
        // A focused centered modal takes keys over the dock beneath
        // it (the New-Session form on top of the sessions dock).
        assert!(FLOATING_MODAL > DOCK);
        // Prompt/popup/menu take keys before a focused dock or
        // centered modal — the R1 rank-inversion fix.
        assert!(POPUP > FLOATING_MODAL);
        // The editor base is the floor.
        for r in [
            SETTINGS,
            KEYBINDING_EDITOR,
            CALIBRATION_WIZARD,
            WORKSPACE_TRUST,
            MENU,
            PROMPT,
            POPUP,
            CONTEXT_MENU,
            FLOATING_MODAL,
            DOCK,
        ] {
            assert!(r > EDITOR_BASE);
        }
    }

    /// Every rank is distinct: intra-rank ordering falls back to the
    /// stable sort's declaration order, and nothing today relies on
    /// that — keep it that way by construction.
    #[test]
    fn ranks_are_distinct() {
        let ranks = [
            SETTINGS,
            KEYBINDING_EDITOR,
            CALIBRATION_WIZARD,
            WORKSPACE_TRUST,
            MENU,
            PROMPT,
            POPUP,
            CONTEXT_MENU,
            FLOATING_MODAL,
            DOCK,
            EDITOR_BASE,
        ];
        let set: std::collections::HashSet<_> = ranks.iter().collect();
        assert_eq!(set.len(), ranks.len(), "two layers share a rank");
    }

    /// The `Disposition` contract as behavior, not prose: `PassAfter`
    /// and `Pass` are NOT interchangeable on an opaque box — an
    /// observer's continue survives its own box's opacity, a decline
    /// does not. (Today every PassAfter producer is a non-opaque
    /// guard; this pins the rule for the first opaque surface that
    /// adopts observer semantics.)
    #[test]
    fn pass_after_is_not_pass_on_an_opaque_box() {
        use super::{pointer_walk_step, Disposition, PointerWalkStep};
        assert_eq!(
            pointer_walk_step(Disposition::PassAfter, true),
            PointerWalkStep::Continue,
        );
        assert_eq!(
            pointer_walk_step(Disposition::Pass, true),
            PointerWalkStep::Stop,
        );
        assert_eq!(
            pointer_walk_step(Disposition::Pass, false),
            PointerWalkStep::Continue,
        );
        assert_eq!(
            pointer_walk_step(Disposition::Consumed, false),
            PointerWalkStep::Stop,
        );
    }

    /// The base-layer contract `handle_key` degrades on (and
    /// `dispatch_layer_keyboard` terminates through): the stack of a
    /// live editor ALWAYS ends with the editor base layer, owned by a
    /// registered component, owning the keyboard. `Base::layers` must
    /// never grow a state gate.
    #[test]
    fn overlay_stack_always_ends_with_an_owning_base_layer() {
        let temp = tempfile::tempdir().unwrap();
        let dir_context = crate::config_io::DirectoryContext::for_testing(temp.path());
        let ed = crate::app::Editor::for_test(
            crate::config::Config::default(),
            80,
            24,
            None,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            std::sync::Arc::new(crate::model::filesystem::StdFileSystem),
            None,
            None,
            false,
            false,
        )
        .unwrap();
        let stack = ed.overlay_stack();
        let last = stack.last().expect("stack never empty");
        assert!(
            matches!(last.layer.kind, crate::app::overlay::LayerKind::Editor),
            "the editor base terminates the stack"
        );
        assert!(
            last.layer.owns_keyboard,
            "the base always owns the keyboard"
        );
        assert!(
            last.owner.is_some(),
            "the base is a registered component (the walk can dispatch to it)"
        );
    }
}
