//! `editor.scroll_offset` keeps its rows of context with soft wrap **off**.
//!
//! The margin went missing in the hand-off between the two placement passes,
//! which no single pass can see — hence an e2e test rather than a unit one.
//! It read as a per-file-type bug because the wrap index that owns placement
//! has size ceilings, and past them the byte pass (which kept the margin) is
//! the authority.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Not the default (3), so this can only pass if the configured value arrived.
const SCROLL_OFFSET: usize = 5;

/// More than the walk below covers, so the margin is owed at every step.
const LINES: usize = 300;

fn wrap_off_config() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.scroll_offset = SCROLL_OFFSET;
    config
}

#[test]
fn scroll_offset_holds_the_cursor_off_the_edge_with_wrap_off() {
    // Small enough to be indexed, which is the case that lost the margin.
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

    // Back up, stopping short of the first line so there is always more above.
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
