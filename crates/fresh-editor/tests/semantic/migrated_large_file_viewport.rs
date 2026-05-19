//! First user of `RenderSnapshot.row_checks` (per #2058 step 5).
//!
//! Demonstrates the per-row text inspection pattern. Migrates
//! `test_large_file_viewport` from the old `tests/e2e/scrolling.rs`
//! cluster: a long buffer opened in a small viewport must show
//! the file's first lines and NOT show its last lines.
//!
//! The earlier `migrated_layout::migrated_load_long_buffer_keeps_
//! viewport_at_top` (deleted in commit 7bd7f1b as fake) just
//! asserted `top_byte == 0`, which is trivially true for any
//! 1-line buffer. This file actually inspects rendered rows and
//! pins what the user sees.
//!
//! Pattern for the ~50 e2e files now unblocked by the
//! `rendered_rows` extension: build a `LayoutScenario` (or use
//! the harness-direct path) that calls
//! `RenderSnapshot::extract_with_rendered_rows` and add
//! `row_checks` to the `RenderSnapshotExpect`.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};

#[test]
fn migrated_large_file_first_lines_visible_last_lines_not() {
    // Original: `test_large_file_viewport` (tests/e2e/scrolling.rs).
    // 100-line buffer in a 10-row terminal; opening must scroll
    // the viewport to the top, so the first lines render and the
    // last lines don't.
    let content: String = (0..100).map(|i| format!("Line {i:03}\n")).collect();
    let mut h = EditorTestHarness::with_temp_project(40, 10).unwrap();
    let _f = h.load_buffer_from_text(&content).unwrap();
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut h);

    let expect = RenderSnapshotExpect {
        viewport_top_byte: Some(0),
        row_checks: vec![
            RowMatch::AnyRowContains("Line 000".into()),
            RowMatch::AnyRowContains("Line 001".into()),
            // Last line of the buffer must not be on screen.
            RowMatch::NoRowContains("Line 099".into()),
            // A row deep into the buffer also must not appear.
            RowMatch::NoRowContains("Line 050".into()),
        ],
        ..Default::default()
    };
    if let Some((field, expected, actual)) = expect.check_against(&snap) {
        panic!("RenderSnapshot mismatch: {field} expected {expected}; actual {actual}\nrows={:?}", snap.rendered_rows);
    }
}

/// Anti-test: drops `extract_with_rendered_rows` in favor of the
/// cheap `Observable::extract`. The resulting snapshot has empty
/// `rendered_rows`, so any non-empty `row_checks` fails with
/// "rendered_rows not populated". Pins that the per-row claims
/// are load-bearing on the extract method used.
#[test]
fn anti_large_file_viewport_using_default_extract_yields_unpopulated_rows() {
    use crate::common::scenario::observable::Observable;
    let content: String = (0..100).map(|i| format!("Line {i:03}\n")).collect();
    let mut h = EditorTestHarness::with_temp_project(40, 10).unwrap();
    let _f = h.load_buffer_from_text(&content).unwrap();
    // Cheap extract — rendered_rows stays empty.
    let snap = RenderSnapshot::extract(&mut h);
    assert!(
        snap.rendered_rows.is_empty(),
        "anti: default extract must NOT populate rendered_rows"
    );

    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains("Line 000".into())],
        ..Default::default()
    };
    let res = expect.check_against(&snap);
    assert!(
        res.is_some_and(|(f, _, _)| f == "rendered_rows"),
        "anti: row_checks against an unpopulated snapshot must fail with rendered_rows field"
    );
}
