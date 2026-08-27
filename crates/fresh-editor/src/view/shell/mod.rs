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
pub mod file_explorer;
pub mod fold;
pub mod frame;
pub mod input;
pub mod menu;
pub mod msg;
pub mod search_options;
pub mod status_bar;
