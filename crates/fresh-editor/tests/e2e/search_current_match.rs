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

// ---------------------------------------------------------------------------
// Edits invalidate the captured match set (issue #3444)
// ---------------------------------------------------------------------------

/// Select all and delete: the buffer holds nothing, so Find Next has nowhere
/// to go. The match set captured when the search ran used to survive the
/// delete and send F3 to byte offsets that no longer existed, reporting a
/// match in a completely empty buffer.
#[test]
fn test_find_next_reports_no_matches_after_deleting_the_whole_buffer() {
    let (_dir, mut harness) = open_with("<p>one</p>\n<p>two</p>\n<p>three</p>\n");

    search(&mut harness, "<p>", false);
    find_next(&mut harness);
    assert!(
        harness.get_status_bar().contains("Match 2 of 3"),
        "status bar before the edit: {}",
        harness.get_status_bar()
    );

    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "");

    find_next(&mut harness);
    let status = harness.get_status_bar();
    assert!(
        !status.contains("Match "),
        "an empty buffer has no match to report, got: {status}"
    );
    assert_eq!(harness.get_selected_text(), "", "nothing got selected");
}

/// Deleting some of the matched lines leaves the rest navigable, and the
/// reported total counts only what is still in the buffer.
#[test]
fn test_find_next_count_tracks_partially_deleted_matches() {
    let (_dir, mut harness) = open_with("<p>a</p>\nfill\n<p>b</p>\nfill\n<p>c</p>\nfill\n");

    search(&mut harness, "<p>", false);
    assert!(
        harness.get_status_bar().contains("Found 3 matches"),
        "status bar after the search: {}",
        harness.get_status_bar()
    );

    // Delete the first matched line, leaving two matches behind.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "fill\n<p>b</p>\nfill\n<p>c</p>\nfill\n"
    );

    find_next(&mut harness);
    let status = harness.get_status_bar();
    assert!(
        status.contains("of 2"),
        "the deleted match must drop out of the total, got: {status}"
    );

    // Both survivors are still reachable, and neither is a stale offset.
    assert_eq!(harness.get_selected_text(), "<p>");
    find_next(&mut harness);
    assert_eq!(harness.get_selected_text(), "<p>");
    assert!(
        harness.get_status_bar().contains("of 2"),
        "status bar on the second survivor: {}",
        harness.get_status_bar()
    );
}

/// Deleting every match one at a time ends with nothing to find — the case
/// from the issue report, where F3 kept "finding" elements already removed.
#[test]
fn test_find_next_after_deleting_every_match_reports_no_matches() {
    let (_dir, mut harness) = open_with("a <br/> b <br/> c <br/> d\n");

    search(&mut harness, "<br/>", false);
    for _ in 0..3 {
        assert_eq!(harness.get_selected_text(), "<br/>");
        harness
            .send_key(KeyCode::Delete, KeyModifiers::NONE)
            .unwrap();
        find_next(&mut harness);
    }
    harness.render().unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "a  b  c  d\n");

    let status = harness.get_status_bar();
    assert!(
        !status.contains("Match "),
        "every match is gone, got: {status}"
    );

    // Shift+F3 shares the path and must agree.
    harness
        .send_key(KeyCode::F(3), KeyModifiers::SHIFT)
        .unwrap();
    harness.process_async_and_render().unwrap();
    let status = harness.get_status_bar();
    assert!(
        !status.contains("Match "),
        "Find Previous must agree there is nothing left, got: {status}"
    );
}

/// A match that spans a line break reaches outside the line(s) an edit
/// touches. Re-evaluating only those lines removes its overlay and cannot
/// re-find it, which used to drop it from the live match set — and, once
/// the overlays became the authority for "no matches left", killed the
/// search outright while the match was still sitting in the buffer.
#[test]
fn test_find_next_keeps_a_multiline_match_after_an_edit_on_its_last_line() {
    let (_dir, mut harness) = open_with("aaa\nfoo\nbarZ\nzzz\n");

    search(&mut harness, "foo\\nbar", true);
    assert_eq!(harness.get_selected_text(), "foo\nbar");

    // Edit the match's last line, past the match itself.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("Q").unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "aaa\nfoo\nbarZQ\nzzz\n"
    );

    find_next(&mut harness);
    assert_eq!(
        harness.get_selected_text(),
        "foo\nbar",
        "the match is still there, so Find Next must still reach it"
    );
    assert!(
        harness.get_status_bar().contains("Match 1 of 1"),
        "status bar: {}",
        harness.get_status_bar()
    );
}

/// Dropping the overlays while leaving the search navigable - what
/// `finish_interactive_replace` does when a query-replace is quit - must
/// hand Find Next back to the stored match set rather than read the empty
/// namespace as "every match is gone".
#[test]
fn test_find_next_still_works_after_query_replace_drops_the_overlays() {
    let (_dir, mut harness) = open_with("one TARGET two TARGET three TARGET\n");

    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.type_text("TARGET").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace?");

    // Quit without replacing: the overlays go, the search stays navigable.
    harness.type_text("q").unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "one TARGET two TARGET three TARGET\n",
        "quitting replaces nothing"
    );
    assert_eq!(
        harness.count_search_highlights(),
        0,
        "finishing the replace drops the search overlays"
    );

    find_next(&mut harness);
    assert_eq!(
        harness.get_selected_text(),
        "TARGET",
        "the matches are still in the buffer, so Find Next must reach them: {}",
        harness.get_status_bar()
    );
}
