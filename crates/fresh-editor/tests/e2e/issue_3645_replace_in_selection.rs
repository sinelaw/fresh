//! Replace with a selection over several lines replaces only inside it, as
//! Find in Selection searches only inside it (issue #3645). A selection
//! within one line is what to replace, not where: it pre-fills the search
//! and the replace covers the whole buffer, as before.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

const TEXT: &str = "a1\na2\na3\na4\n";

fn harness_with(text: &str) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let file_path = harness.project_dir().unwrap().join("test.txt");
    std::fs::write(&file_path, text).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
}

fn key(harness: &mut EditorTestHarness, code: KeyCode, modifiers: KeyModifiers) {
    harness.send_key(code, modifiers).unwrap();
    harness.render().unwrap();
}

/// Select lines `first..=last` (1-based) from the start of the buffer.
fn select_lines(harness: &mut EditorTestHarness, first: usize, last: usize) {
    for _ in 1..first {
        key(harness, KeyCode::Down, KeyModifiers::NONE);
    }
    for _ in first..=last {
        key(harness, KeyCode::Down, KeyModifiers::SHIFT);
    }
}

/// Answer the replace prompts: search for `search`, replace with
/// `replacement`.
fn answer(harness: &mut EditorTestHarness, search: &str, replacement: &str) {
    harness.type_text(search).unwrap();
    key(harness, KeyCode::Enter, KeyModifiers::NONE);
    harness.type_text(replacement).unwrap();
    key(harness, KeyCode::Enter, KeyModifiers::NONE);
}

#[test]
fn replace_all_stays_inside_a_multi_line_selection() {
    let mut harness = harness_with(TEXT);
    select_lines(&mut harness, 2, 3);
    key(&mut harness, KeyCode::Char('r'), KeyModifiers::CONTROL);
    harness.assert_screen_contains("Replace: ");
    answer(&mut harness, "a", "@@@");
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "a1\n@@@2\n@@@3\na4\n"
    );
}

#[test]
fn replace_all_with_no_match_in_the_selection_replaces_nothing() {
    let mut harness = harness_with("b1\nb2\na3\na4\n");
    select_lines(&mut harness, 1, 2);
    key(&mut harness, KeyCode::Char('r'), KeyModifiers::CONTROL);
    answer(&mut harness, "a", "@");
    assert_eq!(harness.get_buffer_content().unwrap(), "b1\nb2\na3\na4\n");
}

#[test]
fn replace_with_a_selection_within_a_line_replaces_everywhere() {
    let mut harness = harness_with(TEXT);
    // Select the `a` of the first line: it becomes the search
    key(&mut harness, KeyCode::Right, KeyModifiers::SHIFT);
    key(&mut harness, KeyCode::Char('r'), KeyModifiers::CONTROL);
    key(&mut harness, KeyCode::Enter, KeyModifiers::NONE);
    harness.type_text("@").unwrap();
    key(&mut harness, KeyCode::Enter, KeyModifiers::NONE);
    assert_eq!(harness.get_buffer_content().unwrap(), "@1\n@2\n@3\n@4\n");
}

/// Query-replace asks about the matches in the selection only, follows the
/// selection's end as replacements change its length, and does not wrap
/// out of it.
#[test]
fn query_replace_asks_only_inside_a_multi_line_selection() {
    let mut harness = harness_with(TEXT);
    select_lines(&mut harness, 2, 3);
    key(
        &mut harness,
        KeyCode::Char('r'),
        KeyModifiers::CONTROL | KeyModifiers::ALT,
    );
    harness.assert_screen_contains("Query replace: ");
    answer(&mut harness, "a", "@@@");
    harness.assert_screen_contains("Replace? (y)es (n)o (a)ll (c)ancel");
    harness.type_text("y").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace? (y)es (n)o (a)ll (c)ancel");
    harness.type_text("y").unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Replace? (y)es");
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "a1\n@@@2\n@@@3\na4\n"
    );
}

#[test]
fn query_replace_all_stays_inside_a_multi_line_selection() {
    let mut harness = harness_with(TEXT);
    select_lines(&mut harness, 2, 3);
    key(
        &mut harness,
        KeyCode::Char('r'),
        KeyModifiers::CONTROL | KeyModifiers::ALT,
    );
    answer(&mut harness, "a", "@");
    harness.type_text("a").unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "a1\n@2\n@3\na4\n");
}
