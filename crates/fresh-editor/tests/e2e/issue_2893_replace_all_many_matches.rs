//! Regression test for <https://github.com/sinelaw/fresh/issues/2893>:
//!
//! Replace-all over a file with tens of thousands of matches never finished.
//! Four parts of the flow scaled with (matches × matches): collecting the
//! matches re-read a 64KB chunk per occurrence, the piece tree scanned every
//! edit for every leaf, each marker owner was adjusted once per edit — and a
//! deletion has to walk every marker at or after it — and every edit event
//! re-summed the whole edit list to shift the cursor.
//!
//! Without those fixes this test does not finish; with them the replace lands
//! in a few seconds even in a debug build.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// One match per line, as in the issue report.
const LINES: usize = 60_000;

#[test]
fn test_replace_all_completes_with_60k_matches() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("big.txt");
    let content: String = (0..LINES)
        .map(|i| format!("line {i:06}: some source code here\n"))
        .collect();
    std::fs::write(&file_path, &content).unwrap();

    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Ctrl+R: replace in the current buffer.
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("Replace: ").unwrap();

    harness.type_text("source").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Replace 'source' with: ")
        .unwrap();

    harness.type_text("SRC").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The replace completes: the status line reports every occurrence, and the
    // text on screen carries the replacement with no match left behind.
    harness
        .wait_for_screen_contains(&format!("Replaced {LINES} occurrence"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("some SRC code here"),
        "replaced text should be on screen, got:\n{screen}"
    );
    assert!(
        !screen.contains("some source code here"),
        "no occurrence should be left on screen, got:\n{screen}"
    );
}
