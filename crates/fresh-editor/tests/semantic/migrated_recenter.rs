//! DECLARATIVE: Migration of `Action::Recenter` (Ctrl+L) from
//! `tests/e2e/emacs_actions.rs::test_recenter_basic`.
//!
//! Scenario is data; runner executes. Load-bearing claim: after
//! Recenter the cursor's screen row lands in the middle band of
//! the viewport.

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario,
};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;
use fresh::test_api::Action;

fn long_buffer(line_count: usize) -> String {
    (0..line_count)
        .map(|i| format!("line {i:02}\n"))
        .collect::<String>()
}

fn move_down_n_then_recenter(n: usize) -> Vec<Action> {
    let mut actions: Vec<Action> = (0..n).map(|_| Action::MoveDown).collect();
    actions.push(Action::Recenter);
    actions
}

#[test]
fn migrated_recenter_lands_cursor_in_middle_band_of_viewport() {
    assert_layout_scenario(LayoutScenario {
        description: "Recenter centres cursor in 10-row viewport".into(),
        initial_text: long_buffer(50),
        width: 40,
        height: 10,
        actions: move_down_n_then_recenter(25),
        expected_top_byte: None,
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor_row_in: Some((2, 7)),
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn anti_recenter_dropped_leaves_cursor_at_viewport_bottom() {
    let scenario = LayoutScenario {
        description: "anti: Recenter dropped — cursor lands at viewport bottom, not middle".into(),
        initial_text: long_buffer(50),
        width: 40,
        height: 10,
        actions: (0..25).map(|_| Action::MoveDown).collect(),
        expected_top_byte: None,
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor_row_in: Some((2, 7)),
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without Recenter the cursor lands outside the middle band"
    );
}
