//! Regression test for <https://github.com/sinelaw/fresh/issues/2111>:
//!
//! F3 / Shift+F3 must step through search matches while the search bar is
//! still open — the bar keeps the keyboard and the query stays editable,
//! matching VS Code, Sublime and browser find. Before the fix the prompt
//! swallowed the key as an unhandled modal key, so F3 did nothing at all
//! until the bar had been closed with Enter.
//!
//! Everything here is asserted from the rendered screen: the status bar's
//! `Ln N, Col M` for where the cursor landed, its `Match N of M` for the
//! step count, the buffer rows for what scrolled into view, and the
//! `Search: …` prompt line for the bar still being open.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Four lines, `alpha` on the last three — never on line 1, so the very
/// first jump is visible in the status bar instead of coinciding with the
/// cursor's starting `Ln 1, Col 1`.
const CONTENT: &str = "one two three\nbeta alpha two\ngamma alpha three\ndelta alpha four\n";

/// Where each `alpha` sits, as the status bar renders it.
const MATCH_1: &str = "Ln 2, Col 6";
const MATCH_2: &str = "Ln 3, Col 7";
const MATCH_3: &str = "Ln 4, Col 7";

fn harness_with(content: &str, dir: &TempDir) -> EditorTestHarness {
    let file_path = dir.path().join("test.txt");
    std::fs::write(&file_path, content).unwrap();

    // Wide enough that the status bar renders `Match N of M` in full rather
    // than eliding it.
    let mut harness =
        EditorTestHarness::create(120, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
}

/// Open the search bar and type `query` — without confirming, so the bar
/// stays open exactly as it is when the user reaches for F3.
fn open_search_bar(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("Search: ").unwrap();
    harness.type_text(query).unwrap();
    harness
        .wait_for_screen_contains(&format!("Search: {query}"))
        .unwrap();
}

#[test]
fn test_f3_advances_matches_and_keeps_search_bar_open() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &temp_dir);

    open_search_bar(&mut harness, "alpha");
    harness.assert_screen_contains("Ln 1, Col 1");

    // The first press commits the typed query and lands on the first match
    // at/after the cursor — the same spot Enter would pick, minus the closing.
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.wait_for_screen_contains(MATCH_1).unwrap();
    harness.assert_screen_contains("Search: alpha");

    // Each further press steps one match forward, wrapping at the end. The
    // status bar counts them off, and the bar stays on screen throughout.
    for (press, expected) in [(2, MATCH_2), (3, MATCH_3), (1, MATCH_1)] {
        harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
        harness
            .wait_for_screen_contains(&format!("Match {press} of 3"))
            .unwrap();
        harness.assert_screen_contains(expected);
        harness.assert_screen_contains("Search: alpha");
    }
}

#[test]
fn test_shift_f3_steps_backwards_with_search_bar_open() {
    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &temp_dir);

    open_search_bar(&mut harness, "alpha");

    // The cursor starts at the top of the file, so a backward search wraps to
    // the last match.
    harness
        .send_key(KeyCode::F(3), KeyModifiers::SHIFT)
        .unwrap();
    harness.wait_for_screen_contains(MATCH_3).unwrap();
    harness.assert_screen_contains("Search: alpha");

    for (press, expected) in [(2, MATCH_2), (1, MATCH_1)] {
        harness
            .send_key(KeyCode::F(3), KeyModifiers::SHIFT)
            .unwrap();
        harness
            .wait_for_screen_contains(&format!("Match {press} of 3"))
            .unwrap();
        harness.assert_screen_contains(expected);
        harness.assert_screen_contains("Search: alpha");
    }
}

#[test]
fn test_f3_with_search_bar_open_reaches_matches_below_the_viewport() {
    // A match every tenth line, far past the 24-row viewport: incremental
    // highlighting only paints the visible rows, so stepping must consult the
    // committed match list rather than the on-screen overlays — otherwise it
    // wraps inside the screen and never reaches line 60.
    let mut content = String::new();
    for line in 1..=120 {
        if line % 10 == 0 {
            content.push_str(&format!("needle here {line}\n"));
        } else {
            content.push_str(&format!("filler {line}\n"));
        }
    }

    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(&content, &temp_dir);

    open_search_bar(&mut harness, "needle");
    // Line 60 is nowhere near the opening viewport.
    harness.assert_screen_not_contains("needle here 60");

    for _ in 0..6 {
        harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    harness.wait_for_screen_contains("Match 6 of 12").unwrap();
    harness.assert_screen_contains("Ln 60, Col 1");
    // The viewport followed the cursor, so the sixth match is on screen.
    harness.assert_screen_contains("needle here 60");
    harness.assert_screen_contains("Search: needle");
}

#[test]
fn test_editing_the_query_after_f3_re_runs_the_search() {
    // Same shape as CONTENT, but the last line holds a different word.
    let content = "one two three\nbeta alpha two\ngamma alpha three\ndelta needle four\n";
    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(content, &temp_dir);

    open_search_bar(&mut harness, "alpha");
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.wait_for_screen_contains(MATCH_1).unwrap();

    // The bar still owns the keyboard, so the query can be rewritten in place
    // (Ctrl+A selects the whole query) and the next F3 searches the new text.
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("needle").unwrap();
    harness.wait_for_screen_contains("Search: needle").unwrap();

    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    // "needle" only appears on line 4, at the column `alpha` used there.
    harness.wait_for_screen_contains(MATCH_3).unwrap();
    harness.assert_screen_contains("Search: needle");
}
