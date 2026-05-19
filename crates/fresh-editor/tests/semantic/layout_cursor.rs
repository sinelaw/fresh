//! LayoutScenarios that pin the hardware cursor position after
//! action sequences. These are the cleanest demonstrations that
//! the editor's layout pass projects the buffer cursor to the
//! right screen cell.
//!
//! All scenarios use 80×24 default terminal so the gutter width
//! is the standard `1 + max(2,digits) + 3 = 6` cells; the cursor
//! is reported in *text-area* coordinates by `cursor_screen_position`,
//! which means column 0 is "first text column", not "left edge".

use crate::common::scenario::layout_scenario::{assert_layout_scenario, LayoutScenario};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;
use fresh::test_api::Action;

#[test]
fn layout_cursor_at_origin_after_load() {
    assert_layout_scenario(LayoutScenario {
        description: "fresh load: hardware cursor at text (0,0)".into(),
        initial_text: "abc".into(),
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor: Some((0, 0)),
            ..Default::default()
        },
    });
}

#[test]
fn layout_cursor_after_three_right_arrows() {
    assert_layout_scenario(LayoutScenario {
        description: "MoveRight ×3 lands cursor at text (3,0)".into(),
        initial_text: "abcdef".into(),
        width: 80,
        height: 24,
        actions: vec![Action::MoveRight, Action::MoveRight, Action::MoveRight],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor: Some((3, 0)),
            ..Default::default()
        },
    });
}

#[test]
fn layout_cursor_after_move_down() {
    assert_layout_scenario(LayoutScenario {
        description: "MoveDown lands cursor at text (0, 1)".into(),
        initial_text: "alpha\nbravo".into(),
        width: 80,
        height: 24,
        actions: vec![Action::MoveDown],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor: Some((0, 1)),
            ..Default::default()
        },
    });
}

#[test]
fn layout_gutter_width_grows_at_100_lines() {
    // ≥100 lines ⇒ 3-digit line numbers ⇒ gutter_width = 1+3+3 = 7.
    let text: String = (0..120).map(|i| format!("line {i}\n")).collect();
    assert_layout_scenario(LayoutScenario {
        description: "120-line buffer pads gutter to 3 digits + frame = 7 cells".into(),
        initial_text: text,
        width: 100,
        height: 30,
        actions: vec![],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            gutter_width: Some(7),
            ..Default::default()
        },
    });
}

#[test]
fn layout_cursor_after_move_line_end_lands_one_past_last_char() {
    // FINDING: `cursor_screen_position` for a cursor sitting *at*
    // the end-of-line newline byte reports column N-1 (the last
    // text cell), not N. Pinning that down — text length 5, col 4.
    assert_layout_scenario(LayoutScenario {
        description: "MoveLineEnd on 'hello' lands cursor at text col 4 (last text cell)".into(),
        initial_text: "hello".into(),
        width: 80,
        height: 24,
        actions: vec![Action::MoveLineEnd],
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor: Some((4, 0)),
            ..Default::default()
        },
    });
}
