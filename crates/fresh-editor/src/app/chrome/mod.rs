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
//! epoch matches AND a fresh (cheap, never-memoized) `overlay_layers` build
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
    /// Drag-to-select in a widget markdown/text document.
    WidgetText,
    /// A floating/dock panel's list scrollbar drag.
    WidgetScrollbar,
    /// A split's vertical scrollbar (thumb-relative or track-jump).
    VScrollbar,
    /// A split's horizontal scrollbar.
    HScrollbar,
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
///
/// **Three grabs have left, and they left by becoming what this imitates.**
/// The dock's width, a split separator and the file explorer's width are all
/// dragged by a *node*, and a node that calls `capture_pointer` on its press
/// keeps every move and the release wherever the pointer goes — so there is
/// nothing to rank and nothing to keep in sync. What is left below are the
/// drags whose press is not a node's: they retire with their surfaces, the
/// same way these did (`view::shell::grip`).
pub(crate) fn pointer_grab(ed: &Editor) -> Option<PointerGrab> {
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
/// Whether a cell is inside a rectangle.
///
/// A plain geometry helper, in the argument order the pointer handlers here
/// read in — cell first, rectangle second. The predicate itself is
/// [`crate::view::ui::layout::point_in_rect`]; this was a third copy of it.
///
/// It outlived the box walk it was written for: the surfaces that hit-test
/// rectangles their own painters recorded — a modal's interior, a plugin
/// panel's widgets — still ask it.
pub(crate) fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    crate::view::ui::layout::point_in_rect(rect, col, row)
}

pub(crate) trait ChromeComponent: Sync {
    /// React to the pointer being at a cell, whatever is under it.
    ///
    /// The reactions keyed on the *position* rather than on the hover target:
    /// the dock's overlay scrollbar, which reveals itself while the pointer is
    /// over the sessions list. Offered on every motion event; return true to
    /// ask for a re-render, which a reaction should do only on the transition
    /// it cares about rather than on every step.
    fn on_pointer_moved(&self, _ed: &mut Editor, _col: u16, _row: u16) -> bool {
        false
    }

    /// React to a hover-target transition (enter / leave / move),
    /// offered to EVERY component after the tree names the new target —
    /// the reaction half of hover, living with the surface it drives
    /// (the menu's auto-switch/submenu machine, the context menu's
    /// highlight, the explorer's status tooltip). Components key off
    /// the target variants they own; reactions are independent — one
    /// surface reacting never suppresses another's leave-reaction (the
    /// old central ladder's early returns did).
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

    /// This component's overlay-layer contributions, from live state:
    /// `(rank, Layer)` pairs pushed into `out` (see [`layer_rank`]).
    /// `Editor::overlay_layers()` concatenates every component's
    /// contributions and sorts by rank descending — the layer stack is
    /// DERIVED from the registry, so a surface's presence, keyboard
    /// ownership, `KeyContext`, and PTY blocking are declared by its
    /// component instead of a central conditional ladder.
    fn layers(&self, _ed: &Editor, _out: &mut Vec<(u16, crate::app::overlay::Layer)>) {}

    // **No keyboard dispatch.** This was `on_layer_key`: the key walk,
    // offered down `layer_rank`'s ordering, the keyboard analogue of
    // `dispatch_pointer` over owner-stamped boxes. Every member has crossed
    // — by containment for the modals and the context menu, by their own
    // layers for the menu and the popups, by `Modality::Focus` for the prompt
    // and the two plugin panels — and the editor base is a direct call from
    // `handle_key`. What a component still declares is where its layer sits
    // and what that layer means, which is `layers` above.
}

/// The ONE chrome registry — every surface with keyboard behaviour, once.
///
/// **Precedence is not this list's**, and no longer anything else's here
/// either: it is the order the frame declares its layers in. Order here
/// decides nothing. It used to: every gesture scanned a z-ordered list of
/// rectangles, and within a band the registry order *was* precedence, so
/// components pushed specific targets before guards. Then it was the ranked
/// key walk's. Both are gone — the pointer and the keyboard are the shell
/// tree's, and a `ChromeComponent` is what is left of a surface once both its
/// input halves have migrated: a declaration of where its layer sits in the
/// stack `get_key_context` and the PTY gate still read.
///
/// The order below is the one the pointer walk left behind, kept because it
/// still reads as "outermost first" and nothing gains by shuffling it.
pub(crate) fn components() -> &'static [&'static dyn ChromeComponent] {
    &[
        &modals::Settings,
        &modals::KeybindingEditor,
        &modals::CalibrationWizard,
        &modals::WorkspaceTrust,
        &context_menu::ContextMenu,
        &prompt::Prompt,
        &popups::Popups,
        &floating_modal::FloatingModal,
        &dock::Dock,
        &menu::Menu,
        &file_explorer::FileExplorer,
        &base::Base,
    ]
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

    /// The base-layer contract: the stack of a live editor ALWAYS ends with
    /// the editor base layer, owning the keyboard. `Base::layers` must never
    /// grow a state gate — `get_key_context` resolves against the first
    /// owning layer with a context and `expect`s one, so a gate here would
    /// panic the input path.
    #[test]
    fn the_stack_always_ends_with_an_owning_base_layer() {
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
        let stack = ed.overlay_layers();
        let last = stack.last().expect("stack never empty");
        assert!(
            matches!(last.kind, crate::app::overlay::LayerKind::Editor),
            "the editor base terminates the stack"
        );
        assert!(last.owns_keyboard, "the base always owns the keyboard");
        assert!(
            last.key_context.is_some(),
            "the base names a context, which `get_key_context` expects to find"
        );
    }
}
