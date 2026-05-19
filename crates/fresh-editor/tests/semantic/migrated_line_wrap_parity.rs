//! DECLARATIVE migration of `tests/e2e/line_wrap_parity.rs`.
//!
//! Parity invariants between the LineWrapCache / ViewLine data
//! the renderer paints from and the scroll-math / cursor-positioning
//! surfaces that the rest of the editor reads.
//!
//! Each test is a `LayoutScenario` data literal. The scrollbar
//! drag-to-bottom is encoded via the symbolic
//! `MouseDragSpec::VerticalScrollbarFullRange`. The cursor parity
//! claim uses `RenderSnapshotExpect::cursor_cell_matches_buffer_char`.

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, MouseDragSpec,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::config::Config;
use fresh::test_api::Action;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

fn word_wrapped_buffer() -> String {
    let para: String = (0..25)
        .map(|i| format!("word{:02}", i))
        .collect::<Vec<_>>()
        .join(" ");
    let mut lines = Vec::new();
    for _ in 0..6 {
        lines.push(para.clone());
    }
    lines.push("TAIL_MARKER_XYZ".to_string());
    lines.join("\n")
}

fn cursor_into_wrap_actions() -> Vec<Action> {
    let mut actions = vec![Action::MoveDocumentStart, Action::MoveDown];
    for _ in 0..45 {
        actions.push(Action::MoveRight);
    }
    actions
}

#[test]
fn migrated_cursor_hardware_position_matches_content_under_cursor_w60() {
    assert_layout_scenario(LayoutScenario {
        description: "w=60: cursor cell matches buffer char after Down+45 Right under wrap".into(),
        initial_text: word_wrapped_buffer(),
        width: 60,
        height: 20,
        config: Some(config_with_wrap()),
        actions: cursor_into_wrap_actions(),
        expected_snapshot: RenderSnapshotExpect {
            cursor_cell_matches_buffer_char: true,
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_cursor_hardware_position_matches_content_under_cursor_w80() {
    assert_layout_scenario(LayoutScenario {
        description: "w=80: cursor cell matches buffer char after Down+45 Right under wrap".into(),
        initial_text: word_wrapped_buffer(),
        width: 80,
        height: 20,
        config: Some(config_with_wrap()),
        actions: cursor_into_wrap_actions(),
        expected_snapshot: RenderSnapshotExpect {
            cursor_cell_matches_buffer_char: true,
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_cursor_hardware_position_matches_content_under_cursor_w100() {
    assert_layout_scenario(LayoutScenario {
        description: "w=100: cursor cell matches buffer char after Down+45 Right under wrap".into(),
        initial_text: word_wrapped_buffer(),
        width: 100,
        height: 20,
        config: Some(config_with_wrap()),
        actions: cursor_into_wrap_actions(),
        expected_snapshot: RenderSnapshotExpect {
            cursor_cell_matches_buffer_char: true,
            ..Default::default()
        },
        ..Default::default()
    });
}

fn drag_to_bottom_reaches_tail_marker_at(width: u16) -> LayoutScenario {
    LayoutScenario {
        description: format!(
            "w={width}: drag scrollbar to bottom reveals TAIL_MARKER on a word-wrapped buffer"
        ),
        initial_text: word_wrapped_buffer(),
        width,
        height: 20,
        config: Some(config_with_wrap()),
        actions: vec![],
        mouse_drags: vec![MouseDragSpec::VerticalScrollbarFullRange],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("TAIL_MARKER_XYZ".into())],
            ..Default::default()
        },
        ..Default::default()
    }
}

#[test]
fn migrated_scrollbar_thumb_reaches_bottom_on_word_wrapped_buffer() {
    assert_layout_scenario(drag_to_bottom_reaches_tail_marker_at(80));
}

#[test]
fn migrated_drag_to_bottom_reaches_end_at_w50() {
    assert_layout_scenario(drag_to_bottom_reaches_tail_marker_at(50));
}

#[test]
fn migrated_drag_to_bottom_reaches_end_at_w70() {
    assert_layout_scenario(drag_to_bottom_reaches_tail_marker_at(70));
}

#[test]
fn migrated_drag_to_bottom_reaches_end_at_w90() {
    assert_layout_scenario(drag_to_bottom_reaches_tail_marker_at(90));
}

#[test]
fn migrated_drag_to_bottom_reaches_end_at_w120() {
    assert_layout_scenario(drag_to_bottom_reaches_tail_marker_at(120));
}

/// Anti-test: drop the scrollbar drag. Without it, the viewport
/// stays at the top of the buffer and TAIL_MARKER must NOT be
/// visible. `check_layout_scenario` must return Err on the
/// AnyRowContains("TAIL_MARKER_XYZ") expectation.
#[test]
fn anti_scrollbar_thumb_without_drag_keeps_tail_off_screen() {
    let scenario = LayoutScenario {
        description: "anti: no drag, expect TAIL_MARKER absent ⇒ AnyRowContains must Err".into(),
        initial_text: word_wrapped_buffer(),
        width: 80,
        height: 20,
        config: Some(config_with_wrap()),
        actions: vec![],
        // Drop the VerticalScrollbarFullRange drag — viewport stays at top.
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("TAIL_MARKER_XYZ".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti: without the scrollbar drag TAIL_MARKER cannot appear"
    );
}

/// Anti-test: drop the Down + 45 Right movements. Without them the
/// cursor stays at byte 0 (column 0, row 0) — the cursor cell at
/// (0, 0) is the 'w' of "word00" so cursor_cell_matches_buffer_char
/// still holds; we instead invert the cell-match claim by adding
/// a contradictory row_check (the cursor must be on row 0 = "word00"
/// but that row is the FIRST row of the buffer; after the dropped
/// movement actions the cursor would be near the wrapped boundary,
/// not at row 0). Encoded as a hardware_cursor_row_in: (10, 20) —
/// cursor must be below row 10, which only happens after Down moves.
#[test]
fn anti_cursor_parity_without_movement_stays_at_buffer_start() {
    let scenario = LayoutScenario {
        description: "anti: no movement ⇒ cursor stays at row 0, not below row 10".into(),
        initial_text: word_wrapped_buffer(),
        width: 80,
        height: 20,
        config: Some(config_with_wrap()),
        actions: vec![Action::MoveDocumentStart],
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor_row_in: Some((10, 20)),
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti: without Down+Right the cursor must NOT be in rows 10..=20"
    );
}
