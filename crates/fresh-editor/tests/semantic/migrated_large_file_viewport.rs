//! DECLARATIVE: migrates `test_large_file_viewport` from the old
//! `tests/e2e/scrolling.rs` cluster. A long buffer in a small
//! viewport must show the file's first lines and NOT show its
//! last lines.
//!
//! Pure `LayoutScenario` data; runner builds the harness.

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

#[test]
fn migrated_large_file_first_lines_visible_last_lines_not() {
    // Original: `test_large_file_viewport` (tests/e2e/scrolling.rs).
    // 80×24 terminal, 100-line "Line {i}" file: first lines visible,
    // "Line 50" not. We keep the original geometry and labels.
    //
    // NOTE: with plain (non-zero-padded) labels "Line {i}", "Line 5"
    // is a prefix of "Line 50".."Line 59", so a NoRowContains("Line 5")
    // would be unsound. We assert NoRowContains("Line 50") (the exact
    // label the original used) and NoRowContains("Line 99"), which are
    // distinct strings that only ever match those specific lines.
    let content: String = (0..100).map(|i| format!("Line {i}\n")).collect();
    assert_layout_scenario(LayoutScenario {
        description: "100-line buffer in 80x24 viewport: head visible, tail not".into(),
        initial_text: content,
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("Line 0".into()),
                RowMatch::AnyRowContains("Line 1".into()),
                RowMatch::NoRowContains("Line 99".into()),
                RowMatch::NoRowContains("Line 50".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: shrink the buffer to 5 lines. With only 5 lines,
/// the "Line 99" / "Line 50" `NoRowContains` checks pass
/// trivially (those rows don't exist in any buffer), so the
/// scenario's positive claim "lines 0..1 visible AND 50+99
/// absent" no longer carries the long-buffer-scroll invariant.
/// We flip it to a contradictory expectation: the
/// `RowMatch::AnyRowContains("Line 99")` must NOT match a 5-line
/// buffer, so `check_layout_scenario` returns Err.
#[test]
fn anti_large_file_viewport_short_buffer_lacks_line_99() {
    let scenario = LayoutScenario {
        description: "anti: 5-line buffer cannot contain 'Line 99'".into(),
        initial_text: (0..5).map(|i| format!("Line {i}\n")).collect(),
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 99".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: a 5-line buffer must NOT contain 'Line 99'"
    );
}
