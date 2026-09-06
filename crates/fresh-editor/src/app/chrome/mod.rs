//! Chrome surfaces as REGISTERED components.
//!
//! What a [`ChromeComponent`] declares today is two things: the overlay
//! LAYER its surface contributes (`layers`, ranked by [`layer_rank`]) and
//! how the surface reacts to a hover-target change (`on_hover_change`).
//! Everything else this registry once carried has crossed to the shell
//! tree — the per-event box tree and its validated memo, the pointer walk
//! over those boxes, and the ranked keyboard walk. Their retirement is
//! recorded in `docs/internal/retained-mode-ui.md` §3.1, which deletes this module; nothing here
//! memoizes, and no cache is consulted on the way to a layer.
//!
//! What keeps the registry alive is that the layer stack must stay DERIVED
//! from live state. A surface's presence, keyboard ownership, `KeyContext`
//! and PTY blocking are declared next to the surface that owns them, so
//! they cannot drift the way the central conditional ladders they replaced
//! did (`app::overlay`'s header names the three that had already gone out
//! of sync with each other). [`components`] is the ONE list; its order
//! decides nothing — see the note on that function.

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
///
/// **A keyboard table, and nothing else.** It is not paint order and not
/// the frame's declaration order: `MENU` outranks `CONTEXT_MENU` here
/// because an open menu owns the keyboard, while the frame declares the
/// context menu *after* the menu-bar dropdowns so it paints on top of
/// them; and `CONTEXT_MENU` sits below `POPUP` so the unfocused-popup
/// guard's `take_while` cannot see it, while a context menu paints above
/// every popup. Reading the two orders as one has already produced a wrong
/// comment in `view::shell::frame` — when the question is "what is drawn
/// over what", the answer is that frame's declaration order, never this.
///
/// Only the two ordered readers of `Editor::overlay_layers` consult these
/// ranks (`resolve_focus_context` and `popup_blocked_by_higher_modal`); a
/// gate that merely asks whether a layer is present reads the unordered
/// `Editor::overlay_layer_set` instead. Event-debug (1000) is hardcoded in
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
/// **Five grabs have left, and they left by becoming what this imitates.**
/// The dock's width, a split separator, the file explorer's width and both of
/// a pane's scrollbars are dragged by a *node*, and a node that calls
/// `capture_pointer` on its press keeps every move and the release wherever
/// the pointer goes — so there is nothing to rank and nothing to keep in
/// sync. The two scrollbars are the ones that show why the ranking existed at
/// all: a thumb drag leaves the bar's own column on its first step, which is
/// precisely the case a re-hit-test would get wrong and the flag ladder was
/// built to survive.
///
/// What is left below are the drags whose press is not a node's: they retire
/// with their surfaces, the same way these did (`view::shell::grip`,
/// `view::shell::splits::scrollbar`).
pub(crate) fn pointer_grab(ed: &Editor) -> Option<PointerGrab> {
    if ed.widget_text_drag.is_some() {
        return Some(PointerGrab::WidgetText);
    }
    // A scrollbar on a buffer-mounted widget panel; its tracks live on the
    // editor rather than on a panel struct.
    //
    // The dock's and the floating panel's own drag flags were tested here
    // first. Nothing set them: the press that armed one was resolved against
    // tracks the interior painter recorded, and that painter is deleted, so
    // the two clauses answered `false` on every path (S7).
    if ed.split_widget_scrollbar_drag.is_some() {
        return Some(PointerGrab::WidgetScrollbar);
    }
    let ms = &ed.active_window().mouse_state;
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

/// Whether a cell is inside a rectangle.
///
/// A plain geometry helper, cell first and rectangle second — its callers'
/// order, and the reverse of the predicate it forwards to,
/// [`crate::view::ui::layout::point_in_rect`], of which this was a third
/// copy.
///
/// It outlived the box walk it was written for because three probes still
/// test a rectangle some *other* writer published, none of them a node's
/// hit-test: the widget runtime, against the hit list and popup rect its own
/// painter recorded; the transient-popup probe in `mouse_input`, against
/// `active_chrome().popup_areas`; and `chrome::splits`, against the pane and
/// tab-strip rectangles the split layout and the shell tree report. The
/// modals no longer ask — their interiors answer their own presses.
pub(crate) fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    crate::view::ui::layout::point_in_rect(rect, col, row)
}

pub(crate) trait ChromeComponent: Sync {
    // **`on_pointer_moved` is gone.** It existed for one reaction — the
    // dock's overlay scrollbar, revealed while the pointer was over the
    // column — and that reaction was keyed on the pointer's *cell* because
    // the only thing that knew where the column was, was a rectangle the
    // painter had recorded. The column is a node now and reports its own
    // Enter and Leave (`UiFact::DockHover`), so there is nothing left that
    // needs every motion event offered to every component.

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
    /// `Editor::layer_contributions` concatenates every component's, and
    /// only its ordered reader sorts by rank descending — the layer stack
    /// is DERIVED from the registry, so a surface's presence, keyboard
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

    /// The rank block is the ONE precedence source for the two readers
    /// that consume `Editor::overlay_layers` as an ORDER — the
    /// `resolve_focus_context` walk behind `get_key_context`, and
    /// `popup_blocked_by_higher_modal`'s `take_while` behind
    /// `resolve_unfocused_popup_action`. Everything else that consults the
    /// stack (the PTY gate, the LSP-hover suppressor, the chrome-caret
    /// gate) asks a membership question and reads the unordered
    /// `Editor::overlay_layer_set`, so a one-character edit here cannot
    /// reach it. Two places, then — but neither has any other statement of
    /// its precedence, so the deliberate relations are pinned. Each assert
    /// names the behavior that regresses if it flips.
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
