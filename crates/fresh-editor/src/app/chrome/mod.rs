//! Where each chrome surface's facts land.
//!
//! The modules below hold the `Editor` methods the tree's messages call for
//! each surface, and nothing else: the registry that used to sit here is
//! gone. It carried the per-event box tree and its validated memo, the
//! pointer walk over those boxes, the ranked keyboard walk, the overlay-layer
//! stack that told `get_key_context` and the PTY gate which surface was up
//! (both read the tree now: `app::overlay`), and the pointer grab, whose last
//! member — the markdown document's drag-to-select — is the run's own
//! captured gesture.
//!
//! **The last thing it carried was two hover reactions, and a default that
//! lost them.** `ChromeComponent::on_hover_change` had a `false` default
//! body, so a surface that was registered but had not written one took the
//! default silently: the menu bar did exactly that, and hovering a submenu
//! parent opened nothing while `menu_hover_reaction` sat with no callers at
//! all. Two surfaces react; `UiFact::Hover` calls both by name, and a
//! reaction that is not called is now a name that does not resolve.

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

use super::Editor;

/// Whether a cell is inside a rectangle.
///
/// A plain geometry helper, cell first and rectangle second — its callers'
/// order, and the reverse of the predicate it forwards to,
/// [`crate::view::ui::layout::point_in_rect`], of which this was a third
/// copy.
///
/// It outlived the box walk it was written for because a few probes still
/// test a rectangle against a point rather than letting a node answer: the
/// widget runtime, against the hit list and popup rect its own painter
/// recorded; `chrome::splits`, against the pane and tab-strip rectangles the
/// shell tree reports; and the transient-popup probe in `mouse_input`, which
/// now asks the tree for the boxes but still compares them itself. The modals
/// no longer ask — their interiors answer their own presses.
pub(crate) fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    crate::view::ui::layout::point_in_rect(rect, col, row)
}
