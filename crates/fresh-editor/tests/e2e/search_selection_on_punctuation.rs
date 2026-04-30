//! Regression test for <https://github.com/sinelaw/fresh/issues/1537>:
//! after `goto_matching_bracket` (or any cursor jump that lands on a
//! non-word character), `find_selection_next` / `find_selection_previous`
//! must not synthesize a search query out of adjacent words by extending
//! across the bracket/whitespace.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// After Ctrl+F searches for "sub" and the cursor is moved to a
/// `}` (a non-word character that is not a search match), pressing
/// Ctrl+F3 (`find_selection_next`) must not replace the active search
/// query with a multi-line "word" stitched together from the surrounding
/// text. Either the original "sub" search is kept and we move to the
/// next "sub", or no new search is started — but we never start a search
/// for `}` (or for `three\n}\nsub`).
#[test]
fn test_find_selection_next_on_punctuation_does_not_hijack_query() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("issue1537.txt");
    let content = "sub one\nsub two { (\nsub three\n}\nsub four\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for "sub" so a search_state is active with 4 matches.
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("sub").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // The lone `}` is at byte offset 30 (line 4, col 1). Move the cursor there.
    let brace_pos = content.find('}').unwrap();
    harness
        .editor_mut()
        .active_cursors_mut()
        .primary_mut()
        .position = brace_pos;
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), brace_pos);

    // Ctrl+F3 — find_selection_next — must not start a search for the
    // bracket-and-surrounding-words "word".
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // The cursor must land on one of the existing "sub" matches (or stay
    // put), never on a position whose match contains the closing brace.
    let cursor = harness.cursor_position();
    let sub_positions: Vec<usize> = (0..content.len())
        .filter(|&i| content[i..].starts_with("sub"))
        .collect();
    assert!(
        cursor == brace_pos || sub_positions.contains(&cursor),
        "After Ctrl+F3 on '}}', cursor moved to {} which is neither the \
         brace position {} nor any 'sub' match {:?}. The search query \
         was hijacked by punctuation/whitespace word extension.",
        cursor,
        brace_pos,
        sub_positions
    );
}

/// Same as the bracket case, but for whitespace: cursor on a space
/// between words must not produce a synthesized "word1 word2" query.
#[test]
fn test_find_selection_next_on_whitespace_does_not_hijack_query() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("ws.txt");
    // "sub" appears on multiple lines so a Ctrl+F search is meaningful.
    let content = "sub one\nsub two\nsub three\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Active search for "sub".
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("sub").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Park the cursor on the space between "sub" and "two" on line 2.
    let space_pos = "sub one\nsub".len();
    assert_eq!(&content[space_pos..space_pos + 1], " ");
    harness
        .editor_mut()
        .active_cursors_mut()
        .primary_mut()
        .position = space_pos;
    harness.render().unwrap();

    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // The buggy behavior synthesizes "sub two" as the search query and
    // moves to the unique "sub two" match (which happens to start with
    // "sub"). With the fix, no new query is synthesized — the cursor
    // continues navigating the existing "sub" search and lands on the
    // next "sub" (the one on line 3, "sub three").
    let expected = "sub one\nsub two\n".len();
    assert_eq!(&content[expected..expected + 3], "sub");
    assert_eq!(
        harness.cursor_position(),
        expected,
        "Ctrl+F3 on whitespace should not synthesize a query — it should \
         continue the existing 'sub' search to the next match."
    );
}
