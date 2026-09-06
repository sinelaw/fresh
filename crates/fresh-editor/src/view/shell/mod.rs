//! The outside-in migration shell.
//!
//! Two pieces, both described in
//! `docs/internal/retained-mode-ui.md`:
//!
//! - [`frame`] — the editor frame as a `fresh-ui` description, with one
//!   `Host` region per area the old painters own. Pinned cell-for-cell against
//!   the ratatui rectangles in `tests/ui_shell_frame_parity.rs`. This line said
//!   "proven" and it overstates what that test can do now: S1b deleted the
//!   production copy of the second computation, so the test's own `reference()`
//!   is the only one left. It is a golden of the layout the editor *had*, not a
//!   live cross-check against one it still runs — enough to stop the shell's
//!   geometry drifting away from the behaviour users have, and unable by
//!   construction to catch the two sides being wrong together, because there is
//!   no longer a second side. Read that file's header before leaning on it.
//! - [`fold`] — the backend: a walk over `LayoutSpec::items` that writes cells
//!   into a `ratatui::Buffer`, calling back into the host for `Draw::Host`.
//!
//! Both are on the render path. The frame's geometry is the shell's, and the
//! fold paints everything the tree owns outright; regions that have not
//! migrated are `Host` leaves, painted by the code that always painted them,
//! into the rectangles this same layout produced.

pub mod calibration;
pub mod content;
pub mod context_menu;
pub mod dock;
pub mod entry;
pub mod event_debug;
pub mod file_browser;
pub mod file_explorer;
pub mod fold;
pub mod frame;
pub mod geometry;
pub mod grip;
pub mod input;
pub mod keybinding;
pub mod menu;
pub mod modal;
pub mod msg;
pub mod overlay_prompt;
pub mod panel;
pub mod popup;
pub mod prompt;
pub mod search_options;
pub mod settings;
pub mod sidebar;
pub mod splits;
pub mod status_bar;
pub mod theme_info;
pub mod trust;
pub mod widgets;

// ── layout read-back ────────────────────────────────────────────────────────

/// A laid-out rectangle in screen coordinates.
///
/// Layout works in frame-local `i32`; the terminal wants `u16` offset by the
/// area the frame was given. That conversion was written out at four call
/// sites, which is three too many — but the *guard* around it differs by
/// caller and belongs to them, not here: [`rect_of`] drops zero-size
/// elements, `search_options` drops zero-width ones while a toggle
/// reconciles, and `frame::regions_of` deliberately keeps a region that
/// paints nothing. So this converts and nothing else.
pub(crate) fn screen_rect(r: fresh_ui::Rect, size: ratatui::layout::Rect) -> ratatui::layout::Rect {
    ratatui::layout::Rect {
        x: size.x.saturating_add(r.x.max(0) as u16),
        y: size.y.saturating_add(r.y.max(0) as u16),
        width: r.w,
        height: r.h,
    }
}

/// Where layout put the element with this key, or `None` if it has no area.
///
/// The common case: a keyed element that is only interesting when it occupies
/// cells. Callers that need a zero-size element to survive should read the
/// rectangle themselves and call [`screen_rect`].
pub(crate) fn rect_of(
    ui: &fresh_ui::Ui<msg::UiMsg>,
    key: &fresh_ui::Key,
    size: ratatui::layout::Rect,
) -> Option<ratatui::layout::Rect> {
    let e = ui.find_by_key(key)?;
    let r = ui.rect_of(e);
    (r.w > 0 && r.h > 0).then(|| screen_rect(r, size))
}

/// The cell a keyed element starts at, **whether or not it occupies any**.
///
/// [`rect_of`]'s zero-size filter is right for the surfaces that ask "is this
/// on screen": a bar-less pane places a zero-size scrollbar node and `None`
/// there means "no bar". It is exactly wrong for a marker whose *only* job is
/// to be a position — the caret's cell is a zero-width node between two runs,
/// so a filter on width answers `None` for every caret there has ever been.
pub(crate) fn cell_of(
    ui: &fresh_ui::Ui<msg::UiMsg>,
    key: &fresh_ui::Key,
    size: ratatui::layout::Rect,
) -> Option<(u16, u16)> {
    let e = ui.find_by_key(key)?;
    let r = screen_rect(ui.rect_of(e), size);
    Some((r.x, r.y))
}
