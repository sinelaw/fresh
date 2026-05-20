//! DECLARATIVE: shallow vt100 / ANSI round-trip smoke tests
//! migrated from `tests/e2e/terminal_io_smoke.rs`.
//!
//! Scenarios are pure data: each test is a single `LayoutScenario`
//! literal whose `row_checks` assert the per-row text produced by
//! the real `render_real()` → ANSI → vt100 round-trip. No
//! direct harness usage; no `let grid = RoundTripGrid::...`
//! imperative extraction.
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

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

#[test]
fn migrated_buffer_text_round_trips_through_ansi_emit() {
    assert_layout_scenario(LayoutScenario {
        description: "buffer text round-trips through ANSI emit + vt100 parse".into(),
        initial_text: "hello world".into(),
        width: 60,
        height: 12,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("hello world".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_typing_appears_in_grid_after_render_real() {
    assert_layout_scenario(LayoutScenario {
        description: "typed chars appear in the vt100 round-trip grid".into(),
        initial_text: String::new(),
        width: 60,
        height: 12,
        actions: vec![
            Action::InsertChar('A'),
            Action::InsertChar('B'),
            Action::InsertChar('C'),
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("ABC".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drops the InsertChar dispatches from
/// `migrated_typing_appears_in_grid_after_render_real`. Without
/// them the buffer stays empty, so no grid row can contain "ABC".
/// Proves the InsertChar actions are what produce the round-tripped
/// text in the vt100 grid.
#[test]
fn anti_terminal_io_dropping_insert_char_yields_no_abc_in_grid() {
    let scenario = LayoutScenario {
        description: "anti: without InsertChar, the vt100 grid must NOT contain 'ABC'".into(),
        initial_text: String::new(),
        width: 60,
        height: 12,
        actions: vec![],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("ABC".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: empty buffer should not contain 'ABC'"
    );
}
