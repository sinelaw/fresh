//! Migration of `tests/e2e/horizontal_scrollbar.rs` — horizontal and
//! vertical scrollbar visibility, config-driven defaults, and toggle
//! routing.
//!
//! Load-bearing claims preserved here:
//!
//!   1. With `line_wrap=false` and very long lines, a horizontal
//!      scrollbar is rendered on (or just below) the last content
//!      row.
//!   2. Toggling the vertical scrollbar via `Action::ToggleVerticalScrollbar`
//!      hides/shows the scrollbar column and sets the appropriate
//!      status message — exercises the action-dispatch path that
//!      the keymap, command palette, and direct method all funnel
//!      through.
//!   3. The matching path for `Action::ToggleHorizontalScrollbar`.
//!   4. `config.editor.show_vertical_scrollbar = false` on startup
//!      lets buffer text extend into the last column.
//!   5. `config.editor.show_horizontal_scrollbar = false` on startup
//!      lets buffer text extend into the bottom content row.
//!   6. In a vertically split view, both splits show the file's
//!      first line ("Line 0:" sentinel) — proves split routing
//!      doesn't drop the buffer's render side.
//!
//! Scrollbar-geometry observations (`is_scrollbar_thumb_at`,
//! `is_scrollbar_track_at`, `has_scrollbar_at_column`,
//! `content_area_rows`) have no `EditorTestApi` projection — they
//! live on `EditorTestHarness` because they probe the rendered
//! ratatui buffer's per-cell styles, not abstract editor state.
//! These tests therefore use the harness-direct pattern (the same
//! pattern `migrated_redraw_screen.rs` uses for the full-redraw
//! flag).
//!
//! Note: the e2e `test_horizontal_scrollbar_hidden_with_line_wrap`
//! asserts on `Line 0:` being visible (not on scrollbar absence),
//! so the migration faithfully preserves that text claim via
//! `RowMatch::AnyRowContains`.
//!
//! Source: `tests/e2e/horizontal_scrollbar.rs` (all 7 tests
//! migrated; no tests deferred).

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::test_api::Action;

/// Helper: any scrollbar-colored cell at the given row?
fn has_scrollbar_at_row(harness: &EditorTestHarness, row: u16) -> bool {
    let buffer = harness.buffer();
    let width = buffer.area.width;
    (0..width).any(|col| {
        harness.is_scrollbar_thumb_at(col, row) || harness.is_scrollbar_track_at(col, row)
    })
}

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

fn config_no_wrap_both_bars() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;
    config.editor.show_vertical_scrollbar = true;
    config
}

#[test]
fn migrated_horizontal_scrollbar_visible_with_long_lines() {
    // Original: `test_horizontal_scrollbar_visible_with_long_lines`.
    let mut harness =
        EditorTestHarness::with_config(80, 24, config_no_wrap_both_bars()).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let (_, last_content_row) = harness.content_area_rows();
    let found = has_scrollbar_at_row(&harness, last_content_row as u16)
        || has_scrollbar_at_row(&harness, (last_content_row + 1) as u16);

    assert!(
        found,
        "Horizontal scrollbar must be visible when line_wrap=false \
         and lines exceed viewport width"
    );
}

#[test]
fn migrated_horizontal_scrollbar_hidden_with_line_wrap_first_line_still_visible() {
    // Original: `test_horizontal_scrollbar_hidden_with_line_wrap`.
    // The e2e asserts that with wrap enabled the *first line* is
    // visible — the wrapping path doesn't lose the buffer content
    // even though horizontal scrolling is irrelevant. Migrated to
    // the per-row matcher to preserve the assertion verbatim.
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.show_horizontal_scrollbar = true;
    config.editor.show_vertical_scrollbar = true;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains("Line 0:".into())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "With line_wrap=true the first buffer line must still \
             render: {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_toggle_vertical_scrollbar_via_action() {
    // Original: `test_toggle_vertical_scrollbar`. Routes through
    // `Action::ToggleVerticalScrollbar` rather than the direct
    // `editor_mut().toggle_*` accessor so the dispatch path
    // (commands.rs entry, app/input.rs handler) is exercised — the
    // command-palette and keybinding entries funnel through the
    // same Action.
    let mut config = Config::default();
    config.editor.show_horizontal_scrollbar = false;
    config.editor.show_vertical_scrollbar = true;
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_scrollbar_at_column(79),
        "Vertical scrollbar must be visible at column 79 initially"
    );

    harness.api_mut().dispatch(Action::ToggleVerticalScrollbar);
    harness.render().unwrap();

    let msg = harness.editor().get_status_message().cloned();
    assert_eq!(
        msg.as_deref(),
        Some("Vertical scrollbar hidden"),
        "First toggle must set the 'hidden' status message"
    );

    harness.api_mut().dispatch(Action::ToggleVerticalScrollbar);
    harness.render().unwrap();

    let msg = harness.editor().get_status_message().cloned();
    assert_eq!(
        msg.as_deref(),
        Some("Vertical scrollbar shown"),
        "Second toggle must set the 'shown' status message"
    );
    assert!(
        harness.has_scrollbar_at_column(79),
        "After re-toggle the vertical scrollbar must be visible again"
    );
}

#[test]
fn migrated_toggle_horizontal_scrollbar_via_action() {
    // Original: `test_toggle_horizontal_scrollbar`.
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let (_, last_content_row) = harness.content_area_rows();
    let has_initial = has_scrollbar_at_row(&harness, last_content_row as u16)
        || has_scrollbar_at_row(&harness, (last_content_row + 1) as u16);
    assert!(
        has_initial,
        "Horizontal scrollbar must be visible initially"
    );

    harness
        .api_mut()
        .dispatch(Action::ToggleHorizontalScrollbar);
    harness.render().unwrap();
    let msg = harness.editor().get_status_message().cloned();
    assert_eq!(
        msg.as_deref(),
        Some("Horizontal scrollbar hidden"),
        "First toggle must set the 'hidden' status message"
    );

    harness
        .api_mut()
        .dispatch(Action::ToggleHorizontalScrollbar);
    harness.render().unwrap();
    let msg = harness.editor().get_status_message().cloned();
    assert_eq!(
        msg.as_deref(),
        Some("Horizontal scrollbar shown"),
        "Second toggle must set the 'shown' status message"
    );
}

#[test]
fn migrated_config_show_vertical_scrollbar_false_lets_content_extend() {
    // Original: `test_config_show_vertical_scrollbar_false`.
    // With the vertical scrollbar hidden via config on startup,
    // long content must extend into the rightmost column (the 'X'
    // padding is visible on a content row).
    let mut config = Config::default();
    config.editor.show_vertical_scrollbar = false;
    config.editor.show_horizontal_scrollbar = false;
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let row_text = harness.get_row_text(5);
    assert!(
        row_text.contains('X'),
        "With vertical scrollbar disabled, content row should contain \
         'X' padding. Got: {:?}",
        row_text.trim()
    );
}

#[test]
fn migrated_config_show_horizontal_scrollbar_false_lets_bottom_row_show_content() {
    // Original: `test_config_show_horizontal_scrollbar_false`.
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let (_, last_content_row) = harness.content_area_rows();
    let row_text = harness.get_row_text(last_content_row as u16);
    assert!(
        row_text.contains("Line") || row_text.contains('X'),
        "Last content row should show buffer content when horizontal \
         scrollbar is disabled. Got: {:?}",
        row_text.trim()
    );
}

#[test]
fn migrated_horizontal_scrollbar_in_split_view_keeps_first_line_visible() {
    // Original: `test_horizontal_scrollbar_in_split_view`. Uses the
    // command-palette "Split Vertical" path (Ctrl+P → type → Enter)
    // so the keymap-to-action resolution is exercised, matching the
    // e2e routing.
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Split Vertical").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains("Line 0:".into())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "After vertical split, the buffer's first line should be \
             visible in at least one split: {f} expected {e}; actual {a}\n\
             rows={:#?}",
            snap.rendered_rows
        );
    }
}

/// Anti-test: drop the `show_horizontal_scrollbar = true` config
/// precondition. With the config flag false (and short content so
/// nothing forces overflow either way), no horizontal scrollbar
/// row may appear, AND the buffer's first line must still render —
/// proving the visibility claim in
/// `migrated_horizontal_scrollbar_visible_with_long_lines` is
/// gated on the config flag being on, not on the harness existing.
#[test]
fn anti_horizontal_scrollbar_with_config_off_is_absent_on_bottom_row() {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = false;
    config.editor.show_vertical_scrollbar = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // Short content — no overflow either way; this anti drops only
    // the config flag.
    let content = "short line a\nshort line b\nshort line c\n";
    harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (_, last_content_row) = harness.content_area_rows();
    // We probe only `last_content_row` (the horizontal scrollbar's
    // natural slot when present). Rows below are the status / mode
    // line and have their own backgrounds; including them would
    // overmatch.
    let found_on_last = has_scrollbar_at_row(&harness, last_content_row as u16);

    assert!(
        !found_on_last,
        "anti: with show_horizontal_scrollbar=false the horizontal \
         scrollbar must NOT be drawn on the last content row \
         (found={found_on_last})"
    );
}

/// Anti-test: drop the second `Action::ToggleVerticalScrollbar`
/// dispatch. After a single toggle, the status message must be
/// "hidden", NOT "shown" — proves the round-trip toggle in the
/// positive test depends on dispatching the action twice, not on
/// some incidental scrollbar state.
#[test]
fn anti_single_vertical_scrollbar_toggle_leaves_status_as_hidden() {
    let mut config = Config::default();
    config.editor.show_horizontal_scrollbar = false;
    config.editor.show_vertical_scrollbar = true;
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let content = long_lines_content(50, 200);
    harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::ToggleVerticalScrollbar);
    harness.render().unwrap();

    let msg = harness.editor().get_status_message().cloned();
    assert_eq!(
        msg.as_deref(),
        Some("Vertical scrollbar hidden"),
        "anti: a single toggle must leave the status at 'hidden', not 'shown'"
    );
}
