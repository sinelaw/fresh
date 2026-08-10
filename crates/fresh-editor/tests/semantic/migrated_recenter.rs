//! DECLARATIVE: Migration of `Action::Recenter` (Ctrl+L) from
//! `tests/e2e/emacs_actions.rs::test_recenter_basic`.
//!
//! Scenario is data; runner executes. Load-bearing claim: after
//! Recenter the cursor's screen row lands in the middle band of
//! the viewport, somewhere natural scrolling would not have put it.
//!
//! The scenario has to be one where those two differ, and the
//! original was not. It used a 10-row terminal, whose text area is
//! 7 rows; with a scroll margin of 3, minimal downward placement
//! rests the cursor at `height - 1 - margin` = row 3 — which is
//! also the middle. The anti-test only detected anything because
//! the placement pass of the day overshot by one row and left the
//! cursor at row 2, `margin + 1` rows from the bottom rather than
//! `margin`. It was pinned to a defect, so removing the defect
//! (see `wrap_model/viewport.py::ensure_visible`, whose minimality
//! property this branch made explicit) broke it.
//!
//! A taller viewport separates them: minimal placement rests near
//! the bottom while the middle band stays in the middle, so
//! Recenter has somewhere to move the cursor *to* and dropping it
//! is detectable again.

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
        description: "Recenter centres cursor in 20-row viewport".into(),
        initial_text: long_buffer(50),
        width: 40,
        height: 20,
        actions: move_down_n_then_recenter(25),
        expected_top_byte: None,
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor_row_in: Some((7, 9)),
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn anti_recenter_dropped_leaves_cursor_at_viewport_bottom() {
    // Without Recenter the cursor stays at the natural-scroll row
    // (row 2 in this layout), which is OUTSIDE the [3,4] middle
    // band, so `check_layout_scenario` must return Err.
    let scenario = LayoutScenario {
        description: "anti: Recenter dropped — cursor stays above the middle band".into(),
        initial_text: long_buffer(50),
        width: 40,
        height: 20,
        actions: (0..25).map(|_| Action::MoveDown).collect(),
        expected_top_byte: None,
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor_row_in: Some((7, 9)),
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without Recenter the cursor is above the middle band"
    );
}
