//! Regression test for issue #1790:
//! Markdown Compose: current-line highlight does not follow cursor across
//! wrapped sub-rows.
//!
//! When a logical line in markdown compose mode is soft-wrapped onto multiple
//! visual rows (the plugin uses `addSoftBreak` with indent=0 for plain
//! paragraphs), the current-line background was only drawn on the *first*
//! visual sub-row of that logical line.  Subsequent sub-rows — including the
//! one where the cursor actually rests after pressing End — were rendered with
//! the default editor background.
//!
//! The fix makes `is_on_cursor_line` recognise any view sub-row whose source
//! byte falls within the cursor's logical-line byte range, so the highlight
//! covers the whole wrapped paragraph (matching native line-wrap behaviour).
//!
//! Before the fix, the cell at the cursor's hardware position had the editor
//! background; with the fix it has `current_line_bg`.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;

#[test]
fn test_compose_wrapped_paragraph_highlight_follows_cursor() {
    init_tracing_from_env();

    // Build a single very long paragraph that, in compose mode at the default
    // compose width, will soft-wrap to several visual rows.
    let long_paragraph = "A piece tree data structure represents this file. Some of the data may be in memory while the rest is pointed at the disk. The piece tree provides an iterator that walks in linear offset order over nodes of the tree, yielding chunks of contiguous content longer.";
    let md_content = format!("# Test\n\n{}\n", long_paragraph);

    // Set up project with the markdown_compose plugin
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("test.md");
    std::fs::write(&md_path, &md_content).unwrap();

    // 110×30 to match the issue's reproduction width. Use the dark theme
    // explicitly so we can compare against its `current_line_bg` (40, 40, 40).
    let config = Config {
        theme: "dark".into(),
        ..Default::default()
    };
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(110, 30, config, project_root).unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    // Enable compose mode via command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for compose mode (and its soft-wrap pass) to settle: the long word
    // "represents" appears in the source as part of the paragraph, but at
    // 110-col compose width the paragraph wraps so the substring "longer."
    // ends up on a continuation row.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("longer.")
        })
        .unwrap();

    // Move into the paragraph (source line 3: 1=#, 2=blank, 3=paragraph) and
    // press End to land at the end of the *first* visual sub-row, then Down
    // once more to step onto a wrapped sub-row.  (Issue #1790: pressing End
    // on a wrapped line lands on the current visual row's end, not the
    // logical-line end.  To reach a wrapped sub-row we navigate visually
    // with Down.  One Down is enough: any non-first sub-row of the paragraph
    // exercises the bug, and stepping further risks overshooting onto the
    // blank line below when the paragraph wraps to only two visual rows.)
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 2)
        .unwrap();
    // Semantic wait: the cursor must land on the paragraph's first visual
    // sub-row before we press End.  Anchor on the first paragraph words.
    harness
        .wait_until(|h| {
            let Some((_x, y)) = h.vt100_cursor_position() else {
                return false;
            };
            let screen = h.screen_to_string();
            screen
                .lines()
                .nth(y as usize)
                .is_some_and(|l| l.contains("piece tree"))
        })
        .unwrap();
    let (_x_before_end, y_before_end) = harness.screen_cursor_position();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    // Semantic wait: End advanced the cursor far to the right on the same
    // visual row.  (We only require the column to be past the leftmost
    // possible compose indent; the exact value depends on the wrap point.)
    harness
        .wait_until(|h| {
            let Some((x, y)) = h.vt100_cursor_position() else {
                return false;
            };
            y == y_before_end && x > 10
        })
        .unwrap();
    let (x_after_end, _y) = harness.screen_cursor_position();

    // Step down one visual row to reach the second (wrapped) sub-row.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Semantic wait: cursor advanced to the next visual row, and that row
    // still belongs to the wrapped paragraph (not the blank line below or
    // anywhere off-paragraph).  We assert paragraph membership by requiring
    // the row to be non-blank, since every wrapped sub-row of the paragraph
    // contains text whereas the line immediately below is empty.
    harness
        .wait_until(|h| {
            let Some((_x, y)) = h.vt100_cursor_position() else {
                return false;
            };
            if y <= y_before_end {
                return false;
            }
            let screen = h.screen_to_string();
            screen
                .lines()
                .nth(y as usize)
                .is_some_and(|l| !l.trim().is_empty())
        })
        .unwrap();
    // Sanity: End set us past the compose indent and Down preserved a
    // similarly-deep column (clamped to the sub-row's end if shorter).
    let (cursor_x_now, _y) = harness.screen_cursor_position();
    assert!(
        cursor_x_now > 0,
        "After End+Down the cursor should sit inside the wrapped sub-row, \
         not at column 0 (would mean we overshot onto a blank line). \
         x_after_end={}, cursor_x={}",
        x_after_end,
        cursor_x_now,
    );

    // Hardware cursor must be visible somewhere in the content area.
    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    assert!(
        cursor_y > 0,
        "Hardware cursor should land inside the content area, got y={}",
        cursor_y
    );

    // Sanity: the wrapped paragraph must actually span multiple visual rows
    // — otherwise this test isn't exercising the bug.  We check that "longer."
    // (the last word) is on a different row from "piece tree" (start of the
    // paragraph).
    let screen = harness.screen_to_string();
    let row_of =
        |needle: &str| -> Option<usize> { screen.lines().position(|l| l.contains(needle)) };
    let first_row = row_of("piece tree").expect("paragraph start should be visible");
    let last_row = row_of("longer.").expect("paragraph end should be visible");
    assert!(
        last_row > first_row,
        "Expected the paragraph to wrap (start row {} should be above end row {})",
        first_row,
        last_row
    );

    // Default dark theme `current_line_bg` (matches existing tests in
    // rendering.rs).
    let current_line_bg = Color::Rgb(40, 40, 40);

    // The cell directly under the hardware cursor must have current_line_bg.
    // Before the fix this assertion failed: the cursor's wrapped sub-row was
    // rendered with the default editor bg.
    let style_at_cursor = harness
        .get_cell_style(cursor_x, cursor_y)
        .expect("cell at cursor position should exist");
    assert_eq!(
        style_at_cursor.bg,
        Some(current_line_bg),
        "Issue #1790: cell at the cursor's wrapped sub-row should have \
         current_line_bg ({:?}), got {:?} (cursor at {},{})",
        current_line_bg,
        style_at_cursor.bg,
        cursor_x,
        cursor_y
    );
}
