//! Chrome surfaces' hover reactions, and the pointer grabs.
//!
//! What a [`ChromeComponent`] declares today is one thing: how its surface
//! reacts to a hover-target change (`on_hover_change`). Everything else this
//! registry once carried has crossed to the shell tree — the per-event box
//! tree and its validated memo, the pointer walk over those boxes, the ranked
//! keyboard walk, and last the overlay-layer stack that told `get_key_context`
//! and the PTY gate which surface was up (both read the tree now: `app::
//! overlay`). The modules below keep the `Editor` methods the tree's facts
//! land in for each surface. `docs/internal/retained-mode-ui.md` §3.1 moves
//! the two hover reactions beside their surfaces and deletes this module.

mod base;
mod context_menu;
mod dock;
mod file_explorer;
mod menu;
mod modals;
mod popups;
mod prompt;
mod splits;
mod status_bar;

use super::types::HoverTarget;
use super::Editor;

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
    // A panel's list scrollbar was a grab here, for the dock and the floating
    // panel first and then for a buffer-mounted panel. A described panel's
    // list is a viewport whose bar captures the pointer itself, and every
    // mounted panel is described, so no panel scrollbar is a grab of this
    // walk's any more.
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
}

/// The surfaces with a hover reaction, offered every hover-target change.
pub(crate) fn components() -> &'static [&'static dyn ChromeComponent] {
    &[&menu::Menu, &file_explorer::FileExplorer]
}
