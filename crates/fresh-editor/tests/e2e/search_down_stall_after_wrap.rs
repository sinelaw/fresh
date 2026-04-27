//! E2E test for the Down-arrow viewport-stall bug after find-selection wrap.
//!
//! Companion to `search_viewport_stall_after_wrap` (#1689): there the bug
//! showed up across F3/Alt+N navigation steps. Here it manifests for plain
//! cursor motion *after* the wrap-around: the wrap-around recenter hands
//! control back to the editor, the user presses Down to keep reading, but
//! the viewport stays pinned. The cursor advances off the bottom of the
//! visible area and stays there for many keypresses before the viewport
//! finally catches up.
//!
//! Repro: file with the word "TARGET" repeated several times, search for
//! it, cycle through every match (the last Alt+N wraps back to match 1),
//! then press Down 20 times. The cursor's expected line must be on
//! screen at the end.
//!
//! On a buggy build the viewport stalls around the recentered position
//! and the line corresponding to "first match line + 20" never enters
//! the visible area within a reasonable number of presses.
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::path::Path;

const NUM_MATCHES: usize = 5;
const FIRST_MATCH_LINE: usize = 18;
const LINE_SPACING: usize = 30;
const TOTAL_LINES: usize = 200;

fn write_target_file(path: &Path) {
    let mut content = String::new();
    for i in 0..TOTAL_LINES {
        let m = (i.saturating_sub(FIRST_MATCH_LINE)) / LINE_SPACING;
        let is_match_line =
            i >= FIRST_MATCH_LINE && m < NUM_MATCHES && i == FIRST_MATCH_LINE + m * LINE_SPACING;
        if is_match_line {
            content.push_str(&format!("line {i:03} TARGET here\n"));
        } else {
            content.push_str(&format!("line {i:03} filler row\n"));
        }
    }
    std::fs::write(path, &content).unwrap();
}

/// After Alt+N cycles around to the first match, plain Down presses must
/// keep the cursor on screen — the viewport must follow.
#[test]
fn test_down_after_alt_n_wraparound_keeps_cursor_visible() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    write_target_file(&file_path);

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open Find prompt and search.
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("TARGET").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Cycle through every match, ending with one extra Alt+N that wraps
    // back to the first match. After this the editor is parked at the
    // first match again, but the viewport has been recentered around it
    // and (on the buggy build) is in the state that suppresses the next
    // few Down-arrow scrolls.
    for _ in 0..NUM_MATCHES {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::ALT)
            .unwrap();
        harness.process_async_and_render().unwrap();
    }

    // Press Down 20 times. From the first-match line that's well into the
    // filler region between matches: cursor should now be at
    // `FIRST_MATCH_LINE + 20`. The viewport (24 rows tall, ~22 visible
    // content rows) cannot show both the first match and a line 20 rows
    // below it, so the viewport must scroll.
    let down_presses = 20usize;
    for _ in 0..down_presses {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let target_line = FIRST_MATCH_LINE + down_presses;
    let needle = format!("line {target_line:03} filler row");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&needle),
        "After {down_presses} Down presses following Alt+N wrap-around, the cursor's \
         line ({target_line}, expected text {needle:?}) must be visible — the \
         viewport stalled instead of following the cursor.\nRendered screen:\n{screen}"
    );
}
