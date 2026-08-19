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
//! Neither is on the render path yet. `Editor::render` still paints the frame
//! itself; this module is the seam that will replace it, built and tested
//! first so the mechanism is proven before any surface depends on it.

pub mod fold;
pub mod frame;
pub mod input;
pub mod msg;
pub mod status_bar;
