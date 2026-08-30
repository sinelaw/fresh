//! The outside-in migration shell.
//!
//! Two pieces, both described in
//! `docs/internal/fresh-editor-ui-migration.md`:
//!
//! - [`frame`] — the editor frame as a `fresh-ui` description, with one
//!   `Host` region per area the old painters own. Proven to reproduce the
//!   ratatui rectangles exactly in `tests/ui_shell_frame_parity.rs`.
//! - [`fold`] — the backend: a walk over `LayoutSpec::items` that writes cells
//!   into a `ratatui::Buffer`, calling back into the host for `Draw::Host`.
//!
//! Both are on the render path. The frame's geometry is the shell's, and the
//! fold paints everything the tree owns outright; regions that have not
//! migrated are `Host` leaves, painted by the code that always painted them,
//! into the rectangles this same layout produced.

pub mod context_menu;
pub mod dock;
pub mod file_browser;
pub mod file_explorer;
pub mod fold;
pub mod frame;
pub mod input;
pub mod menu;
pub mod modal;
pub mod msg;
pub mod overlay_prompt;
pub mod panel;
pub mod popup;
pub mod prompt;
pub mod search_options;
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
