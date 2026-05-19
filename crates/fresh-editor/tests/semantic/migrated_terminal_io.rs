//! Shallow vt100 / ANSI round-trip smoke tests.
//!
//! These prove the render → ANSI-emit → vt100-parse pipeline
//! transports buffer text end-to-end. They do NOT cover the
//! specific regressions in the cited e2e files:
//!   - tests/e2e/ansi_cursor.rs — files starting with ANSI
//!     escape sequences must not place the hardware cursor at
//!     (0,0). Tracked as an orphan in #2058.
//!   - tests/e2e/redraw_screen.rs — Action::RedrawScreen must
//!     force a full repaint. Tracked as an orphan in #2058.
//!   - tests/e2e/rendering.rs — cursor position, line numbers,
//!     current-line highlight, ANSI RGB color. Tracked as an
//!     orphan in #2058.
//!
//! The dropped tautological grid_dimensions_match_terminal test
//! just compared `grid.height` to the constructed terminal
//! height — same source, vacuously true.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::observable::{Observable, RoundTripGrid};
use fresh::test_api::Action;

#[test]
fn migrated_buffer_text_round_trips_through_ansi_emit() {
    let mut h = EditorTestHarness::with_temp_project(60, 12).unwrap();
    let _f = h.load_buffer_from_text("hello world").unwrap();
    let grid = RoundTripGrid::extract(&mut h);
    assert!(
        grid.rows.iter().any(|r| r.contains("hello world")),
        "vt100 grid lacks 'hello world'; rows: {:#?}",
        grid.rows
    );
}

#[test]
fn migrated_typing_appears_in_grid_after_render_real() {
    let mut h = EditorTestHarness::with_temp_project(60, 12).unwrap();
    let _f = h.load_buffer_from_text("").unwrap();
    h.api_mut().dispatch(Action::InsertChar('A'));
    h.api_mut().dispatch(Action::InsertChar('B'));
    h.api_mut().dispatch(Action::InsertChar('C'));
    let grid = RoundTripGrid::extract(&mut h);
    assert!(
        grid.rows.iter().any(|r| r.contains("ABC")),
        "vt100 grid lacks typed 'ABC'; rows: {:#?}",
        grid.rows
    );
}
