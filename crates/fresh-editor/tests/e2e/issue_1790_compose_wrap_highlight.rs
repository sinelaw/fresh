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

    // Move into the paragraph and onto a wrapped sub-row.  Counting Down /
    // End keypresses to land on the wrapped portion is fragile in compose
    // mode (visual layout is plugin-driven and can shift between runs), so
    // instead navigate by mouse to the row that contains "longer." — by
    // construction that's the *last* visual sub-row of the wrapped paragraph
    // and therefore a wrapped (non-first) sub-row, which is exactly what
    // issue #1790 covers.  Locate it from the same rendered screen the
    // assertion will read, click on a column that holds real text, and wait
    // semantically for the cursor to land on that row before asserting.
    let screen_pre = harness.screen_to_string();
    let first_para_row = screen_pre
        .lines()
        .position(|l| l.contains("piece tree"))
        .expect("paragraph start should be visible") as u16;
    let last_para_row = screen_pre
        .lines()
        .position(|l| l.contains("longer."))
        .expect("paragraph end should be visible") as u16;
    assert!(
        last_para_row > first_para_row,
        "Expected the paragraph to wrap (start row {} should be above end row {})",
        first_para_row,
        last_para_row
    );
    // Click on a column known to hold paragraph text on the last sub-row.
    // "longer." sits near the start of the last sub-row, so column equal to
    // its offset on that row is a safe click target.
    let last_line = screen_pre
        .lines()
        .nth(last_para_row as usize)
        .expect("last paragraph row must be readable");
    let click_col = last_line
        .find("longer")
        .expect("'longer' substring must be locatable on the last sub-row")
        as u16;
    harness.mouse_click(click_col, last_para_row).unwrap();

    // Semantic wait: the hardware cursor lands on the clicked row, and the
    // cell at the cursor position has been re-styled by the renderer
    // (i.e. the highlight pipeline has caught up to the cursor move).  We
    // verify the latter by waiting until two consecutive ticks report the
    // same bg at the cursor position — this is a true per-cell stability
    // check rather than a screen-symbol equality, so it doesn't miss
    // highlight-only updates (which `screen_to_string` filters out).
    harness
        .wait_until(|h| {
            let Some((_x, y)) = h.vt100_cursor_position() else {
                return false;
            };
            y == last_para_row
        })
        .unwrap();
    let mut prev_bg: Option<Option<ratatui::style::Color>> = None;
    harness
        .wait_until(|h| {
            let Some((x, y)) = h.vt100_cursor_position() else {
                return false;
            };
            if y != last_para_row {
                return false;
            }
            let cur_bg = h.get_cell_style(x, y).map(|s| s.bg).unwrap_or(None);
            let stable = prev_bg == Some(cur_bg);
            prev_bg = Some(cur_bg);
            stable
        })
        .unwrap();

    // Read the hardware cursor position now that the highlight pipeline
    // has settled.  Must sit on `last_para_row` (the wrapped sub-row we
    // clicked into) — the wait above guarantees that.
    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    assert_eq!(
        cursor_y, last_para_row,
        "Cursor should be on the wrapped sub-row we clicked ({}), not {}",
        last_para_row, cursor_y,
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
