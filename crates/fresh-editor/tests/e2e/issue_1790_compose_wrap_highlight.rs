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
    // twice more to step through visual sub-rows so we end up on a later one.
    // (Issue #1790: pressing End on a wrapped line lands on the current visual
    // row's end, not the logical-line end.  To reach a wrapped sub-row we
    // navigate visually with Down.)
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 2)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    // Step down two visual rows to reach the third sub-row of the paragraph.
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 2)
        .unwrap();

    // Let the post-movement view settle.
    let mut prev = String::new();
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            let stable = s == prev;
            prev = s;
            stable
        })
        .unwrap();

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
