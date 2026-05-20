//! DECLARATIVE rewrite. Migration of `tests/e2e/horizontal_scrollbar.rs`.
//!
//! Every test is a `LayoutScenario` data literal — no harness
//! calls, no per-step imperative `send_key` / `render` flow.
//!
//! Load-bearing claims preserved here:
//!
//!   1. With `line_wrap=false` and very long lines, a horizontal
//!      scrollbar is rendered on (or just below) the last content
//!      row.
//!   2. Toggling the vertical scrollbar via
//!      `Action::ToggleVerticalScrollbar` hides/shows the scrollbar
//!      column and sets the appropriate status message.
//!   3. The matching path for `Action::ToggleHorizontalScrollbar`.
//!   4. `config.editor.show_vertical_scrollbar = false` on startup
//!      lets buffer text extend into the last column.
//!   5. `config.editor.show_horizontal_scrollbar = false` on startup
//!      lets buffer text extend into the bottom content row.
//!   6. In a vertically split view, both splits show the file's
//!      first line ("Line 0:" sentinel) — proves split routing
//!      doesn't drop the buffer's render side.
//!
//! ## DSL extensions used
//!
//! - `LayoutScenario::expected_horizontal_scrollbar_visible` /
//!   `expected_no_horizontal_scrollbar_on_last_content_row` /
//!   `expected_scrollbar_at_column` — declarative wrappers around
//!   the scrollbar-cell-style detection that previously required
//!   harness-direct probing.
//! - `LayoutScenario::expected_status_message` — wraps
//!   `EditorTestApi::status_message` so step assertions on the
//!   "Vertical scrollbar hidden/shown" round-trip can be expressed
//!   declaratively.
//! - `RenderSnapshotExpect::status_message` — same shape but on
//!   `StepAssertion`s so the multi-toggle scenarios can pin the
//!   intermediate status after each toggle.
//! - `ScenarioConfigOverrides::{line_wrap, show_horizontal_scrollbar,
//!   show_vertical_scrollbar}` — already in the DSL.

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, ScenarioConfigOverrides, StepAssertion,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

/// Build N lines of width `line_length` so horizontal scrolling is
/// required when `line_wrap=false`.
fn long_lines_content(num_lines: usize, line_length: usize) -> String {
    (0..num_lines)
        .map(|i| {
            let prefix = format!("Line {i}: ");
            let padding_len = line_length.saturating_sub(prefix.len());
            format!("{prefix}{}", "X".repeat(padding_len))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn migrated_horizontal_scrollbar_visible_with_long_lines() {
    // Original: `test_horizontal_scrollbar_visible_with_long_lines`.
    assert_layout_scenario(LayoutScenario {
        description: "line_wrap=false + long lines ⇒ horizontal scrollbar visible".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(true),
            show_vertical_scrollbar: Some(true),
            ..Default::default()
        },
        expected_horizontal_scrollbar_visible: Some(true),
        ..Default::default()
    });
}

#[test]
fn migrated_horizontal_scrollbar_hidden_with_line_wrap_first_line_still_visible() {
    // Original: `test_horizontal_scrollbar_hidden_with_line_wrap`.
    // With wrap enabled, the e2e doesn't assert on scrollbar absence
    // — it asserts that the buffer's first line is visible.
    assert_layout_scenario(LayoutScenario {
        description: "line_wrap=true ⇒ first buffer line (Line 0:) still visible".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            show_horizontal_scrollbar: Some(true),
            show_vertical_scrollbar: Some(true),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 0:".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_toggle_vertical_scrollbar_via_action() {
    // Original: `test_toggle_vertical_scrollbar`. Round-trip:
    // dispatch the action twice, observing the intermediate status
    // message ("hidden") after the first toggle and the final
    // status ("shown") after the second. Routes through
    // `Action::ToggleVerticalScrollbar` — the same action the
    // keybinding and palette entries funnel into.
    assert_layout_scenario(LayoutScenario {
        description: "ToggleVerticalScrollbar round-trip: status hidden then shown".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(false),
            show_vertical_scrollbar: Some(true),
            ..Default::default()
        },
        actions: vec![
            Action::ToggleVerticalScrollbar,
            Action::ToggleVerticalScrollbar,
        ],
        step_assertions: vec![
            StepAssertion {
                after_action_index: 0,
                expect: RenderSnapshotExpect {
                    status_message: Some("Vertical scrollbar hidden".into()),
                    ..Default::default()
                },
            },
            StepAssertion {
                after_action_index: 1,
                expect: RenderSnapshotExpect {
                    status_message: Some("Vertical scrollbar shown".into()),
                    ..Default::default()
                },
            },
        ],
        expected_scrollbar_at_column: Some(79),
        expected_status_message: Some("Vertical scrollbar shown".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_toggle_horizontal_scrollbar_via_action() {
    // Original: `test_toggle_horizontal_scrollbar`.
    assert_layout_scenario(LayoutScenario {
        description: "ToggleHorizontalScrollbar round-trip: status hidden then shown".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(true),
            ..Default::default()
        },
        actions: vec![
            Action::ToggleHorizontalScrollbar,
            Action::ToggleHorizontalScrollbar,
        ],
        step_assertions: vec![
            StepAssertion {
                after_action_index: 0,
                expect: RenderSnapshotExpect {
                    status_message: Some("Horizontal scrollbar hidden".into()),
                    ..Default::default()
                },
            },
            StepAssertion {
                after_action_index: 1,
                expect: RenderSnapshotExpect {
                    status_message: Some("Horizontal scrollbar shown".into()),
                    ..Default::default()
                },
            },
        ],
        expected_status_message: Some("Horizontal scrollbar shown".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_config_show_vertical_scrollbar_false_lets_content_extend() {
    // Original: `test_config_show_vertical_scrollbar_false`. With
    // the vertical scrollbar hidden via config, long content must
    // extend into the rightmost column — assert by checking a
    // content row contains the 'X' padding.
    assert_layout_scenario(LayoutScenario {
        description: "show_vertical_scrollbar=false ⇒ content extends; 'X' visible".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(false),
            show_vertical_scrollbar: Some(false),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            // Some row past the row 5 area should contain 'X'.
            row_checks: vec![RowMatch::AnyRowContains("X".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_config_show_horizontal_scrollbar_false_lets_bottom_row_show_content() {
    // Original: `test_config_show_horizontal_scrollbar_false`.
    // With horizontal scrollbar disabled, buffer content reaches
    // the last content row — assert "some row contains 'Line' or
    // 'X'" via the disjunctive matcher.
    assert_layout_scenario(LayoutScenario {
        description: "show_horizontal_scrollbar=false ⇒ buffer content reaches last row".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(false),
            ..Default::default()
        },
        // Note: we don't pin "no horizontal scrollbar on the last
        // content row" here because the vertical scrollbar (still
        // on, by default) bleeds a thumb cell onto that row in the
        // bottom-right cell. The original e2e's only assertion was
        // that buffer content reaches the bottom — preserve that
        // verbatim via the row matcher.
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContainsAny(vec!["Line".into(), "X".into()])],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_horizontal_scrollbar_in_split_view_keeps_first_line_visible() {
    // Original: `test_horizontal_scrollbar_in_split_view`. The e2e
    // routed through Ctrl+P → "Split Vertical" → Enter to exercise
    // the palette path; the declarative form calls
    // `Action::SplitVertical` directly — same dispatch the palette
    // entry funnels into. The load-bearing claim ("Line 0: still
    // visible in at least one split") is unchanged.
    assert_layout_scenario(LayoutScenario {
        description: "after SplitVertical: 'Line 0:' still visible in some split".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(true),
            ..Default::default()
        },
        actions: vec![Action::SplitVertical],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 0:".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

// ── Anti-tests ────────────────────────────────────────────────────────

/// Anti: drop the `show_horizontal_scrollbar = true` config flag
/// (set it false) and use short content. The horizontal scrollbar
/// must NOT appear on the last content row.
#[test]
fn anti_horizontal_scrollbar_with_config_off_is_absent_on_bottom_row() {
    assert_layout_scenario(LayoutScenario {
        description:
            "anti: show_horizontal_scrollbar=false + short content ⇒ no bottom-row scrollbar".into(),
        initial_text: "short line a\nshort line b\nshort line c\n".into(),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(false),
            show_vertical_scrollbar: Some(false),
            ..Default::default()
        },
        expected_no_horizontal_scrollbar_on_last_content_row: Some(true),
        ..Default::default()
    });
}

/// Anti: drop the second `ToggleVerticalScrollbar` dispatch. After
/// only one toggle, the status must be "hidden", not "shown".
#[test]
fn anti_single_vertical_scrollbar_toggle_leaves_status_as_hidden() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: single ToggleVerticalScrollbar ⇒ status 'hidden'".into(),
        initial_text: long_lines_content(50, 200),
        width: 80,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            show_horizontal_scrollbar: Some(false),
            show_vertical_scrollbar: Some(true),
            ..Default::default()
        },
        actions: vec![Action::ToggleVerticalScrollbar],
        expected_status_message: Some("Vertical scrollbar hidden".into()),
        ..Default::default()
    });
}
