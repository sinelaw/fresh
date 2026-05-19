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
    let content: String = (0..100).map(|i| format!("Line {i:03}\n")).collect();
    assert_layout_scenario(LayoutScenario {
        description: "100-line buffer in 10-row viewport: head visible, tail not".into(),
        initial_text: content,
        width: 40,
        height: 10,
        actions: vec![],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("Line 000".into()),
                RowMatch::AnyRowContains("Line 001".into()),
                RowMatch::NoRowContains("Line 099".into()),
                RowMatch::NoRowContains("Line 050".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: shrink the buffer to 5 lines. With only 5 lines,
/// the "Line 099" / "Line 050" `NoRowContains` checks pass
/// trivially (those rows don't exist in any buffer), so the
/// scenario's positive claim "lines 000..001 visible AND
/// 050+099 absent" no longer carries the long-buffer-scroll
/// invariant. We flip it to a contradictory expectation: the
/// `RowMatch::AnyRowContains("Line 099")` must NOT match a
/// 5-line buffer, so `check_layout_scenario` returns Err.
#[test]
fn anti_large_file_viewport_short_buffer_lacks_line_099() {
    let scenario = LayoutScenario {
        description: "anti: 5-line buffer cannot contain 'Line 099'".into(),
        initial_text: (0..5).map(|i| format!("Line {i:03}\n")).collect(),
        width: 40,
        height: 10,
        actions: vec![],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 099".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: a 5-line buffer must NOT contain 'Line 099'"
    );
}
