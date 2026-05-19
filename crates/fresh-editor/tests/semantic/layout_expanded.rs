//! Phase 2 expansion: LayoutScenarios that exercise the richer
//! `RenderSnapshot` fields (gutter, hardware cursor) on top of the
//! original `viewport_top_byte` shortcut.
//!
//! These scenarios prove the new accessors on `EditorTestApi`
//! (`gutter_width`, `hardware_cursor_position`, `terminal_width`,
//! `terminal_height`) project through the runner correctly. They
//! land alongside the layout_shadow differential.

use crate::common::scenario::layout_scenario::{assert_layout_scenario, LayoutScenario};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;

#[test]
fn theorem_gutter_width_uses_minimum_two_digits() {
    // `Viewport::gutter_width` formula: `1 + max(digits, 2) + 3`.
    // For ≤ 99-line buffers, that's 1 + 2 + 3 = 6 cells.
    assert_layout_scenario(LayoutScenario {
        description: "short buffer pads gutter to 2-digit minimum (= 6 cells)".into(),
        initial_text: "hello".into(),
        width: 80,
        height: 24,
        actions: vec![],
        config: None,
        expected_top_byte: Some(0),
        expected_snapshot: RenderSnapshotExpect {
            gutter_width: Some(6),
            ..Default::default()
        },
    });
}

#[test]
fn theorem_hardware_cursor_after_load_is_text_origin() {
    // `cursor_screen_position` returns *text-area* coordinates,
    // not absolute screen cells — gutter offset is applied at draw
    // time. With no actions, the cursor sits at byte 0, which is
    // text col 0, row 0.
    assert_layout_scenario(LayoutScenario {
        description: "fresh load parks hardware cursor at text origin (0, 0)".into(),
        initial_text: "alpha\nbravo\n".into(),
        width: 80,
        height: 24,
        actions: vec![],
        expected_snapshot: RenderSnapshotExpect {
            hardware_cursor: Some((0, 0)),
            ..Default::default()
        },
        ..Default::default()
    });
}
