//! `editor.scroll_offset` keeps its rows of context with soft wrap **off**.
//!
//! The setting is a reading margin: the cursor stops that many rows short of
//! the window's edge and the file scrolls under it instead. With wrap off the
//! margin went missing — the cursor walked to the very last row of the pane and
//! only then did the view move, one row per key press, whatever
//! `scroll_offset` said.
//!
//! It looked like a per-file-type bug because the wrap index that owns
//! placement has size ceilings: past them no index is built, the byte-oriented
//! pass stays the vertical authority, and it always applied the margin. So a
//! 200-line source file rode the edge while a 6000-line one in the same window,
//! with the same config, kept its context rows.
//!
//! The test drives `Down` and `Up` and reads only rendered output — the
//! hardware cursor's row against the pane's own first and last row.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Deliberately not the default (3), so the assertions below can only pass if
/// the configured value reached the placement pass.
const SCROLL_OFFSET: usize = 5;

/// Comfortably more than the walk below covers, so the document never runs out
/// underneath the cursor and the margin is owed at every step.
const LINES: usize = 300;

/// Small enough to stay well inside the wrap index's ceilings, which is the
/// case that lost the margin.
fn wrap_off_config() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.scroll_offset = SCROLL_OFFSET;
    config
}

#[test]
fn scroll_offset_holds_the_cursor_off_the_edge_with_wrap_off() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(80, 24, wrap_off_config()).unwrap();

    let path = harness.project_dir().unwrap().join("lines.ts");
    let text = (1..=LINES)
        .map(|i| format!("const v{i} = {i};"))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::write(&path, text).unwrap();

    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("const v1 = 1;"))
        .unwrap();

    let (first_row, last_row) = harness.content_area_rows();

    // Down through several screenfuls. Every line the cursor lands on has 100+
    // more below it, so the view owes it `SCROLL_OFFSET` rows of what is
    // coming; a cursor closer than that to the bottom means the view stopped
    // scrolling ahead of it.
    for step in 1..=120 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let (_, cursor_row) = harness.screen_cursor_position();
        assert!(
            cursor_row as usize + SCROLL_OFFSET <= last_row,
            "Down #{step}: the cursor is on screen row {cursor_row}, {} rows from \
             the pane's last row ({last_row}), with the rest of the file still \
             below it. `scroll_offset` is {SCROLL_OFFSET}, so the view must \
             scroll while the cursor is still that far from the edge.\n{}",
            last_row.saturating_sub(cursor_row as usize),
            harness.screen_to_string(),
        );
    }

    // And the same margin above the cursor on the way back up, stopping short
    // of the first line so there is always more document overhead.
    for step in 1..=80 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let (_, cursor_row) = harness.screen_cursor_position();
        assert!(
            cursor_row as usize >= first_row + SCROLL_OFFSET,
            "Up #{step}: the cursor is on screen row {cursor_row}, {} rows from \
             the pane's first row ({first_row}), with more of the file above \
             it. `scroll_offset` is {SCROLL_OFFSET}.\n{}",
            (cursor_row as usize).saturating_sub(first_row),
            harness.screen_to_string(),
        );
    }
}
