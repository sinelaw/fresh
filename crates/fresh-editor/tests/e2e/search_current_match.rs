//! E2E tests for the current search match.
//!
//! Find Next / Find Previous select the whole match they land on, so a regex
//! match shows exactly how far it reaches and `Delete` removes all of it. The
//! current match, in Find Next and in Query Replace alike, is drawn in its own
//! color so it stands out from the other highlighted matches.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::{Color, Modifier};
use tempfile::TempDir;

fn open_with(content: &str) -> (TempDir, EditorTestHarness) {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.xhtml");
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    (temp_dir, harness)
}

/// Open the search bar, optionally switch on regex mode, type `query` and
/// confirm with Enter.
fn search(harness: &mut EditorTestHarness, query: &str, regex: bool) {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    if regex {
        harness
            .send_key(KeyCode::Char('r'), KeyModifiers::ALT)
            .unwrap();
    }
    harness.type_text(query).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
}

fn find_next(harness: &mut EditorTestHarness) {
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
}

/// Background color of the screen cell `offset` columns right of where
/// `anchor_text` starts on screen.
fn bg_at(harness: &EditorTestHarness, anchor_text: &str, offset: u16) -> Option<Color> {
    let (x, y) = harness
        .find_text_on_screen(anchor_text)
        .unwrap_or_else(|| panic!("'{anchor_text}' not on screen"));
    harness.get_cell_style(x + offset, y).and_then(|s| s.bg)
}

fn is_bold_at(harness: &EditorTestHarness, anchor_text: &str, offset: u16) -> bool {
    let (x, y) = harness
        .find_text_on_screen(anchor_text)
        .unwrap_or_else(|| panic!("'{anchor_text}' not on screen"));
    harness
        .get_cell_style(x + offset, y)
        .is_some_and(|s| s.add_modifier.contains(Modifier::BOLD))
}

fn current_match_bg(harness: &EditorTestHarness) -> Color {
    harness.editor().theme().search_current_match_bg
}

fn match_bg(harness: &EditorTestHarness) -> Color {
    harness.editor().theme().search_match_bg
}

#[test]
fn test_find_next_selects_whole_regex_match() {
    let (_dir, mut harness) = open_with("keep <b>one</b> keep <b>two</b> end\n");

    search(&mut harness, "<b>.*?</b>", true);

    assert_eq!(harness.cursor_position(), 5, "caret at the match start");
    assert_eq!(harness.get_selected_text(), "<b>one</b>");

    find_next(&mut harness);

    assert_eq!(
        harness.cursor_position(),
        21,
        "caret at the next match start"
    );
    assert_eq!(harness.get_selected_text(), "<b>two</b>");
}

#[test]
fn test_greedy_regex_match_is_visible_as_selection() {
    let (_dir, mut harness) = open_with("keep <b>one</b> keep <b>two</b> end\n");

    search(&mut harness, "<b>.*</b>", true);

    assert_eq!(
        harness.get_selected_text(),
        "<b>one</b> keep <b>two</b>",
        "a greedy match should show its full extent"
    );
}

#[test]
fn test_find_previous_selects_match() {
    let (_dir, mut harness) = open_with("aa foo bb foo cc\n");

    search(&mut harness, "foo", false);
    find_next(&mut harness);
    assert_eq!(harness.cursor_position(), 10);

    harness
        .send_key(KeyCode::F(3), KeyModifiers::SHIFT)
        .unwrap();
    harness.process_async_and_render().unwrap();

    assert_eq!(harness.cursor_position(), 3);
    assert_eq!(harness.get_selected_text(), "foo");
}

/// The user's workflow for removing elements one by one: land on a match,
/// check it, delete it, go to the next. A match that slides into the deleted
/// one's place must not be skipped.
#[test]
fn test_delete_selected_match_then_find_next_lands_on_following_match() {
    let (_dir, mut harness) = open_with("<br/><br/>x<br/>\n");

    search(&mut harness, "<br/>", false);
    assert_eq!(harness.get_selected_text(), "<br/>");

    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "<br/>x<br/>\n",
        "Delete should remove the whole selected match"
    );

    find_next(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        0,
        "the match that slid up to the cursor is the next one"
    );
    assert_eq!(harness.get_selected_text(), "<br/>");

    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    find_next(&mut harness);
    assert_eq!(harness.cursor_position(), 1);
    assert_eq!(harness.get_selected_text(), "<br/>");

    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "x\n");
}

#[test]
fn test_current_match_has_its_own_color_over_the_selection() {
    let (_dir, mut harness) = open_with("aa foo bb foo cc\n");
    assert_ne!(
        current_match_bg(&harness),
        match_bg(&harness),
        "theme must tell the current match apart"
    );

    search(&mut harness, "foo", false);

    // Offset 4 is the 'o' after the first "foo"'s caret cell; 11 is inside
    // the second "foo".
    assert_eq!(
        bg_at(&harness, "aa foo bb", 4),
        Some(current_match_bg(&harness)),
        "the selected current match keeps the current-match color"
    );
    assert!(
        is_bold_at(&harness, "aa foo bb", 4),
        "the current match is bold"
    );
    assert!(
        !is_bold_at(&harness, "aa foo bb", 11),
        "other matches are not bold"
    );
    assert_eq!(bg_at(&harness, "aa foo bb", 11), Some(match_bg(&harness)));

    find_next(&mut harness);

    assert_eq!(bg_at(&harness, "aa foo bb", 4), Some(match_bg(&harness)));
    assert_eq!(
        bg_at(&harness, "aa foo bb", 11),
        Some(current_match_bg(&harness))
    );
}

#[test]
fn test_query_replace_marks_the_match_it_asks_about() {
    let (_dir, mut harness) = open_with("aa foo bb foo cc\n");

    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.type_text("foo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Empty replacement: delete each confirmed match.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace?");

    assert_eq!(
        bg_at(&harness, "aa foo bb", 4),
        Some(current_match_bg(&harness))
    );
    assert_eq!(bg_at(&harness, "aa foo bb", 11), Some(match_bg(&harness)));

    // Skip the first match; the mark moves to the second.
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    assert_ne!(
        bg_at(&harness, "aa foo bb", 4),
        Some(current_match_bg(&harness))
    );
    assert_eq!(
        bg_at(&harness, "aa foo bb", 11),
        Some(current_match_bg(&harness))
    );

    // Delete the second match; that ends the session and clears the mark.
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "aa foo bb  cc\n");
    let (x, y) = harness.find_text_on_screen("aa foo bb").unwrap();
    for offset in 0..14 {
        assert_ne!(
            harness.get_cell_style(x + offset, y).and_then(|s| s.bg),
            Some(current_match_bg(&harness)),
            "no current-match mark should remain after Query Replace ends"
        );
    }
}

/// Reopening the search bar while the current match is selected brings back
/// the query that found it (here a regex), not the literal matched text.
#[test]
fn test_reopening_search_on_current_match_prefills_query() {
    let (_dir, mut harness) = open_with("keep <b>one</b> keep <b>two</b> end\n");

    search(&mut harness, "<b>.*?</b>", true);
    assert_eq!(harness.get_selected_text(), "<b>one</b>");

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Search: <b>.*?</b>");
}

/// A selection the user made themselves still pre-fills the search bar.
#[test]
fn test_user_selection_still_prefills_search() {
    let (_dir, mut harness) = open_with("alpha word beta\n");

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.get_selected_text(), "word");

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Search: word");
}

/// Esc ends Query Replace like `c` does: the current-match mark goes with it.
#[test]
fn test_query_replace_esc_clears_the_current_match_mark() {
    let (_dir, mut harness) = open_with("aa foo bb foo cc\n");

    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.type_text("foo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        bg_at(&harness, "aa foo bb", 4),
        Some(current_match_bg(&harness))
    );

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        !harness.screen_to_string().contains("Replace?"),
        "Esc should end Query Replace"
    );
    let (x, y) = harness.find_text_on_screen("aa foo bb").unwrap();
    for offset in 0..14 {
        assert_ne!(
            harness.get_cell_style(x + offset, y).and_then(|s| s.bg),
            Some(current_match_bg(&harness)),
            "no current-match mark should remain after Esc"
        );
    }
}

/// After deleting the current match, moving the caret away and back onto a
/// match start does not make F3 re-select that match: stepping works from the
/// caret as usual once it has moved.
#[test]
fn test_find_next_after_deleting_and_moving_steps_past_the_caret() {
    let (_dir, mut harness) = open_with("xfoo foo foo\n");

    search(&mut harness, "foo", false);
    assert_eq!(harness.cursor_position(), 1);
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "x foo foo\n");

    // Move the caret onto the start of the next match (" foo" -> 2).
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(harness.cursor_position(), 2);

    find_next(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        6,
        "F3 moves past the match at the caret once the caret has moved"
    );
}

/// Ctrl+F3 (find selection next) on the selected current match of a regex
/// search continues that search rather than searching for the matched text.
#[test]
fn test_find_selection_next_on_current_match_continues_regex_search() {
    let (_dir, mut harness) = open_with("<b>one</b> <b>two</b> <b>one</b>\n");

    search(&mut harness, "<b>.*?</b>", true);
    assert_eq!(harness.get_selected_text(), "<b>one</b>");

    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.get_selected_text(),
        "<b>two</b>",
        "Ctrl+F3 steps to the next regex match, not the next literal '<b>one</b>'"
    );

    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(harness.cursor_position(), 0);
    assert_eq!(harness.get_selected_text(), "<b>one</b>");
}

/// The same after an edit has shifted the matches: the selected current match
/// is still recognized, so Ctrl+F3 continues the regex search.
#[test]
fn test_find_selection_next_after_an_edit_continues_regex_search() {
    let (_dir, mut harness) = open_with("<b>1</b> <b>22</b> <b>333</b> <b>4444</b>\n");

    search(&mut harness, "<b>.*?</b>", true);
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    find_next(&mut harness);
    assert_eq!(harness.get_selected_text(), "<b>22</b>");

    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.get_selected_text(),
        "<b>333</b>",
        "Ctrl+F3 steps to the next regex match, not a literal '<b>22</b>' search"
    );
}

/// Typing over the selected current match replaces it; the typed text is not
/// a match, so it does not keep the current-match look.
#[test]
fn test_typing_over_the_current_match_drops_the_mark() {
    let (_dir, mut harness) = open_with("aa foo bb foo cc\n");

    search(&mut harness, "foo", false);
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "aa X bb foo cc\n");
    assert_ne!(
        bg_at(&harness, "aa X bb", 3),
        Some(current_match_bg(&harness)),
        "the typed text must not keep the current-match color"
    );
    // The remaining match is still an ordinary highlighted match.
    assert_eq!(bg_at(&harness, "aa X bb", 9), Some(match_bg(&harness)));
}
