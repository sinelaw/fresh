//! DECLARATIVE migration of `tests/e2e/line_wrap_parity.rs`.
//!
//! Parity invariants between the LineWrapCache / ViewLine data
//! the renderer paints from and the scroll-math / cursor-positioning
//! surfaces that the rest of the editor reads (see
//! `docs/internal/line-wrap-cache-plan.md`).
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Cursor-on-screen parity.** On a word-wrapped buffer, the
//!      char at the cursor's hardware position must match the char
//!      the cursor logically points to. Pre-refactor, char-wrap
//!      inside `cursor_screen_position` disagreed with the
//!      renderer's word-wrap and could put the hardware cursor a
//!      row off.
//!
//!   2. **Thumb-vs-content parity.** Dragging the scrollbar thumb
//!      to the bottom of the track must show the buffer's
//!      TAIL_MARKER. If the thumb's reported total visual rows
//!      underreported the buffer (as pre-refactor `wrap_line` did
//!      vs. the renderer's word-aware wrap), the drag would stop
//!      short.
//!
//!   3. **Thumb consistency across widths.** Drag-to-bottom must
//!      reach the tail marker at multiple widths.
//!
//! Each test is a `LayoutScenario` data literal. The scrollbar
//! drag-to-bottom is encoded via the symbolic
//! `MouseDragSpec::VerticalScrollbarFullRange` (the runner resolves
//! the column / first / last rows from the harness's content area
//! at runtime). The cursor parity claim uses
//! `RenderSnapshotExpect::cursor_cell_matches_buffer_char` — the
//! matcher derives the expected printable char from
//! `cursor_byte` + `buffer_text` on the snapshot and compares it
//! against the rendered cell at the hardware cursor position.
//!
//! Source: `tests/e2e/line_wrap_parity.rs` (3 tests migrated; no
//! tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, MouseDragSpec,
    ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

fn wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

/// Realistic word-wrapped buffer — the kind of text where
/// `wrap_line`'s char-wrap and the renderer's word-boundary wrap
/// disagreed before the refactor. TAIL_MARKER lets the drag-to-end
/// tests confirm the bottom of the buffer is reachable.
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

/// Ctrl+Home → Down → 45×Right. Lands the cursor mid-line near a
/// wrap boundary on several widths — where the old char-wrap /
/// word-wrap drift surfaced.
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
        description: "w=60: cursor cell == buffer char after Down + 45 Right under wrap".into(),
        initial_text: word_wrapped_buffer(),
        width: 60,
        height: 20,
        config_overrides: wrap_overrides(),
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
        description: "w=80: cursor cell == buffer char after Down + 45 Right under wrap".into(),
        initial_text: word_wrapped_buffer(),
        width: 80,
        height: 20,
        config_overrides: wrap_overrides(),
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
        description: "w=100: cursor cell == buffer char after Down + 45 Right under wrap".into(),
        initial_text: word_wrapped_buffer(),
        width: 100,
        height: 20,
        config_overrides: wrap_overrides(),
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
            "w={width}: scrollbar drag-to-bottom reveals TAIL_MARKER on word-wrapped buffer"
        ),
        initial_text: word_wrapped_buffer(),
        width,
        height: 20,
        config_overrides: wrap_overrides(),
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
/// `AnyRowContains("TAIL_MARKER_XYZ")` expectation.
#[test]
fn anti_scrollbar_thumb_without_drag_keeps_tail_off_screen() {
    let scenario = LayoutScenario {
        description: "anti: no drag, expect TAIL_MARKER ⇒ AnyRowContains must Err".into(),
        initial_text: word_wrapped_buffer(),
        width: 80,
        height: 20,
        config_overrides: wrap_overrides(),
        actions: vec![],
        // Deliberately no mouse_drags — the load-bearing step we drop.
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

/// Anti-test: drop the Down + 45×Right movements. Without them the
/// cursor stays at byte 0 (the very first 'w' of `word00`). The
/// cell-match still trivially holds there, so the parity matcher
/// can't catch the drop. Instead we pin the cursor row to a band
/// that's only reachable AFTER Down — `hardware_cursor_row_in:
/// (10, 20)`. With no movement the cursor stays in the first
/// visible rows, so the band check fails and `check_layout_scenario`
/// returns Err.
#[test]
fn anti_cursor_parity_without_movement_stays_at_buffer_start() {
    let scenario = LayoutScenario {
        description: "anti: no movement ⇒ cursor stays near row 0, not in rows 10..=20".into(),
        initial_text: word_wrapped_buffer(),
        width: 80,
        height: 20,
        config_overrides: wrap_overrides(),
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
