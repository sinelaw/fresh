//! Regression test for <https://github.com/sinelaw/fresh/issues/2111>:
//!
//! F3 / Shift+F3 must step through search matches while the search bar is
//! still open — the bar keeps the keyboard and the query stays editable,
//! matching VS Code, Sublime and browser find. Before the fix the prompt
//! swallowed the key as an unhandled modal key, so F3 did nothing at all
//! until the bar had been closed with Enter.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Byte offsets of every occurrence of `needle` in `haystack`.
fn all_matches(haystack: &str, needle: &str) -> Vec<usize> {
    haystack.match_indices(needle).map(|(pos, _)| pos).collect()
}

/// Open the search bar and type `query` — without confirming, so the bar
/// stays open exactly as it is when the user reaches for F3.
fn open_search_bar(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(query).unwrap();
    harness.render().unwrap();
}

fn harness_with(content: &str, dir: &TempDir) -> EditorTestHarness {
    let file_path = dir.path().join("test.txt");
    std::fs::write(&file_path, content).unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
}

#[test]
fn test_f3_advances_matches_and_keeps_search_bar_open() {
    let content = "alpha one\nbeta alpha two\ngamma alpha three\ndelta alpha four\n";
    let matches = all_matches(content, "alpha");
    assert_eq!(matches.len(), 4, "fixture should hold four matches");

    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(content, &temp_dir);

    open_search_bar(&mut harness, "alpha");

    // First F3 commits the typed query and lands on the first match at/after
    // the cursor — the same spot Enter would pick, minus the closing.
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        matches[0],
        "first F3 should land on the first match"
    );
    assert!(
        harness.editor().is_prompting(),
        "the search bar must stay open after F3"
    );

    // Each further press steps one match forward, wrapping at the end.
    for expected in [matches[1], matches[2], matches[3], matches[0]] {
        harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        assert_eq!(
            harness.cursor_position(),
            expected,
            "F3 should advance to the next match"
        );
        assert!(
            harness.editor().is_prompting(),
            "the search bar must stay open while stepping with F3"
        );
    }

    // The query is still in the bar and still editable.
    assert!(
        harness.get_prompt_line().contains("alpha"),
        "the query should still be shown in the search bar, got {:?}",
        harness.get_prompt_line()
    );
}

#[test]
fn test_shift_f3_steps_backwards_with_search_bar_open() {
    let content = "alpha one\nbeta alpha two\ngamma alpha three\ndelta alpha four\n";
    let matches = all_matches(content, "alpha");

    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(content, &temp_dir);

    open_search_bar(&mut harness, "alpha");

    // The cursor starts at the top of the file, so a backward search wraps to
    // the last match.
    harness
        .send_key(KeyCode::F(3), KeyModifiers::SHIFT)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        matches[3],
        "Shift+F3 from the top of the file should wrap to the last match"
    );
    assert!(
        harness.editor().is_prompting(),
        "the search bar must stay open after Shift+F3"
    );

    for expected in [matches[2], matches[1], matches[0]] {
        harness
            .send_key(KeyCode::F(3), KeyModifiers::SHIFT)
            .unwrap();
        harness.process_async_and_render().unwrap();
        assert_eq!(
            harness.cursor_position(),
            expected,
            "Shift+F3 should step to the previous match"
        );
    }
}

#[test]
fn test_f3_with_search_bar_open_reaches_matches_below_the_viewport() {
    // Matches every tenth line, well past the 24-row viewport: incremental
    // highlighting only paints the visible rows, so stepping must consult the
    // committed match list rather than the on-screen overlays.
    let mut content = String::new();
    for line in 0..120 {
        if line % 10 == 0 {
            content.push_str(&format!("needle on line {line}\n"));
        } else {
            content.push_str(&format!("filler {line}\n"));
        }
    }
    let matches = all_matches(&content, "needle");
    assert_eq!(matches.len(), 12);

    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(&content, &temp_dir);

    open_search_bar(&mut harness, "needle");

    for expected in &matches {
        harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
        harness.process_async_and_render().unwrap();
        assert_eq!(
            harness.cursor_position(),
            *expected,
            "F3 should keep walking matches past the bottom of the viewport"
        );
    }
}

#[test]
fn test_editing_the_query_after_f3_re_runs_the_search() {
    let content = "alpha one\nbeta alpha two\ngamma alpha three\ndelta needle four\n";
    let temp_dir = TempDir::new().unwrap();
    let mut harness = harness_with(content, &temp_dir);

    open_search_bar(&mut harness, "alpha");
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(harness.cursor_position(), content.find("alpha").unwrap());

    // The bar still owns the keyboard, so the query can be rewritten in place
    // and the next F3 must search for the *new* text.
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("needle").unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        content.find("needle").unwrap(),
        "F3 after editing the query should search for the edited query"
    );
    assert!(
        harness.editor().is_prompting(),
        "the search bar must stay open throughout"
    );
}
