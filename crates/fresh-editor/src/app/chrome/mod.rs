//! Chrome surfaces' hover reactions.
//!
//! What a [`ChromeComponent`] declares today is one thing: how its surface
//! reacts to a hover-target change (`on_hover_change`). Everything else this
//! registry once carried has crossed to the shell tree — the per-event box
//! tree and its validated memo, the pointer walk over those boxes, the ranked
//! keyboard walk, and last the overlay-layer stack that told `get_key_context`
//! and the PTY gate which surface was up (both read the tree now: `app::
//! overlay`), and last the pointer grab — its final member was the markdown
//! document's drag-to-select, and that is the run's own captured gesture now.
//! The modules below keep the `Editor` methods the tree's facts land in for
//! each surface. Moving the two hover reactions beside their surfaces deletes
//! this module.

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
