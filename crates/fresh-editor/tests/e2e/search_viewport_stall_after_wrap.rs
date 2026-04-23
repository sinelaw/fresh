//! E2E test for the viewport-stall-after-wrap bug in Find Next.
//!
//! Reproduces <https://github.com/sinelaw/fresh/issues/1689>:
//! When cycling through matches with F3, the viewport scrolls correctly on
//! the first pass but stalls after the first wrap-around. The cursor keeps
//! moving to subsequent matches (status bar updates), but the viewport no
//! longer scrolls to follow it, so matches in other parts of the file end
//! up completely off-screen.
//!
//! Shape of the bug-inducing input: several match clusters, each cluster
//! small enough to fit in one viewport but clusters separated by enough
//! lines that no two clusters are ever visible together. This mirrors the
//! real-world repro in `plugins/lib/fresh.d.ts` searching for `getPluginApi`
//! (7 matches in 3 clusters at lines 75/90/93, 957/972, 1920/1926).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::path::Path;

/// Build a buffer with 7 "NEEDLE" matches in 3 clusters: lines 5/6/7, 55/56,
/// 105/106. Clusters are >40 lines apart so they can never share a 24-row
/// viewport.
const MATCH_LINES: &[usize] = &[5, 6, 7, 55, 56, 105, 106];

fn write_clustered_needle_file(path: &Path) {
    let mut content = String::new();
    for i in 0..200 {
        if MATCH_LINES.contains(&i) {
            content.push_str(&format!("line {i} NEEDLE here\n"));
        } else {
            content.push_str(&format!("line {i} filler text\n"));
        }
    }
    std::fs::write(path, &content).unwrap();
}

/// Drive the search prompt, returning a harness parked on the first match.
fn harness_searched_for_needle() -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    write_clustered_needle_file(&file_path);

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("NEEDLE").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    (harness, temp_dir)
}

#[test]
fn test_find_next_viewport_does_not_stall_after_wrap() {
    let (mut harness, _temp_dir) = harness_searched_for_needle();

    // Find Next order: initial lands on match 1 (line 5).
    // Each F3 press then advances through match 2, 3, 4, 5, 6, 7, wrap to 1,
    // 2, 3, 4 … keeping one-to-one with MATCH_LINES cycled.
    //
    // After 10 F3 presses we should be on match 4 again (line 55) — this is
    // in cluster B, far from cluster A. That means *on this press* the
    // viewport must have scrolled; the cursor sits at line 55 which is
    // outside any viewport that was showing cluster A.
    //
    // The bug stalls the viewport around cluster A after the first wrap
    // (press #7). Thereafter every F3 moves the cursor but not the
    // viewport — so `line 55` is never rendered.
    let presses = 10usize;
    for _ in 0..presses {
        harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
    }

    // Sequence after `presses` F3s starting from match 1:
    //   idx = presses % 7  (0-indexed match index)
    // 10 % 7 = 3, which is MATCH_LINES[3] = 55.
    let expected_match_line = MATCH_LINES[presses % MATCH_LINES.len()];
    assert_eq!(
        expected_match_line, 55,
        "sanity: after 10 F3 presses the cursor should be on the line-55 match"
    );

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("line {expected_match_line} NEEDLE here")),
        "After {presses} F3 presses the current match (line {expected_match_line}) \
         must be visible on screen, but the viewport stalled on an earlier cluster. \
         Rendered screen:\n{screen}"
    );
}

/// Same viewport-stall manifests via Alt+N (`find_selection_next`) once a
/// search is active — Alt+N delegates to `find_next` when the cursor is on
/// a match, so any fix to `move_cursor_to_match` must keep this path healthy
/// too.
#[test]
fn test_find_selection_next_viewport_does_not_stall_after_wrap() {
    let (mut harness, _temp_dir) = harness_searched_for_needle();

    let presses = 10usize;
    for _ in 0..presses {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::ALT)
            .unwrap();
        harness.process_async_and_render().unwrap();
    }

    let expected_match_line = MATCH_LINES[presses % MATCH_LINES.len()];
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("line {expected_match_line} NEEDLE here")),
        "After {presses} Alt+N presses the current match (line {expected_match_line}) \
         must be visible on screen, but the viewport stalled on an earlier cluster. \
         Rendered screen:\n{screen}"
    );
}
