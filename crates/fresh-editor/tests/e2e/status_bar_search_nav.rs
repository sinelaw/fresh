//! The status bar's `{search_nav}` arrows: while a search is active, a `↓`
//! and a `↑` that step to the next and previous match on a left click, the
//! same as F3 / Shift+F3. Which match the cursor is on is told by the
//! existing "Match X of Y" message.
//!
//! Everything is asserted from the rendered screen: the arrows, the message
//! and the `Ln N` cursor readout on the status bar row, the prompt line for
//! whether the search bar is still open, and the buffer rows for what a
//! replace wrote.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// `alpha` on lines 2, 4 and 6, never on line 1: the first jump moves the
/// cursor, so every step shows on the `Ln N` readout.
const CONTENT: &str = "zero\nalpha one\nbeta\nalpha two\ngamma\nalpha three\n";

const NEXT: &str = "\u{2193}";
const PREVIOUS: &str = "\u{2191}";

fn harness_with(content: &str, dir: &TempDir) -> EditorTestHarness {
    let file_path = dir.path().join("test.txt");
    std::fs::write(&file_path, content).unwrap();
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
}

/// The status bar row: the one carrying the `Ln N, Col M` readout.
fn status_row(harness: &EditorTestHarness) -> String {
    let (_, row) = harness
        .find_text_on_screen("Ln ")
        .unwrap_or_else(|| panic!("no status bar:\n{}", harness.screen_to_string()));
    harness.screen_row_text(row)
}

/// The arrows as the status bar draws them: joined, no separator between.
const ARROWS: &str = " \u{2193}  \u{2191} ";

/// The cursor is on line `line` and the status bar carries the arrows.
fn assert_arrows_at_line(harness: &EditorTestHarness, line: usize) {
    let row = status_row(harness);
    assert!(
        row.contains(ARROWS),
        "status bar should show the arrows, got {row:?}\nscreen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        row.contains(&format!("Ln {line}, ")),
        "cursor should be on line {line}, got {row:?}"
    );
}

fn assert_no_arrows(harness: &EditorTestHarness) {
    let row = status_row(harness);
    assert!(
        !row.contains(NEXT) && !row.contains(PREVIOUS),
        "status bar should carry no search arrows, got {row:?}\nscreen:\n{}",
        harness.screen_to_string()
    );
}

/// Click `glyph`, then wait for the step's "Match X of 3" message.
fn click_to(harness: &mut EditorTestHarness, glyph: &str, nth: usize, line: usize) {
    click_status(harness, glyph);
    harness
        .wait_for_screen_contains(&format!("Match {nth} of 3"))
        .unwrap();
    assert_arrows_at_line(harness, line);
}

/// Left-click the cell holding `glyph` on the status bar row.
fn click_status(harness: &mut EditorTestHarness, glyph: &str) {
    let (_, status) = harness.find_text_on_screen("Ln ").expect("a status bar");
    let (col, row) = harness
        .find_text_on_screen(glyph)
        .unwrap_or_else(|| panic!("no {glyph} on screen:\n{}", harness.screen_to_string()));
    assert_eq!(row, status, "{glyph} should be on the status bar row");
    harness.mouse_click(col, row).unwrap();
}

fn search(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("Search: ").unwrap();
    harness.type_text(query).unwrap();
    harness
        .wait_for_screen_contains(&format!("Search: {query}"))
        .unwrap();
}

/// Ctrl+F, `alpha`, Enter: the search the other tests start from.
fn committed_search(harness: &mut EditorTestHarness) {
    search(harness, "alpha");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.process_async_and_render().unwrap();
}

#[test]
fn no_arrows_before_a_search() {
    let dir = TempDir::new().unwrap();
    let harness = harness_with(CONTENT, &dir);
    assert_no_arrows(&harness);
}

#[test]
fn a_committed_search_shows_the_arrows_on_the_status_bar() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    committed_search(&mut harness);

    assert_arrows_at_line(&harness, 2);
    // No count of its own: the position is the message's.
    assert!(!status_row(&harness).contains("1/3"));
    // The status bar is the row just above the prompt line at the bottom.
    let (_, row) = harness.find_text_on_screen(NEXT).expect("an arrow");
    assert_eq!(
        row as usize,
        harness.terminal_height() - 2,
        "the arrows are on the status bar row\nscreen:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn clicking_the_arrows_steps_through_the_matches_and_wraps() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    committed_search(&mut harness);
    assert_arrows_at_line(&harness, 2);

    click_to(&mut harness, NEXT, 2, 4);
    click_to(&mut harness, NEXT, 3, 6);
    // Past the last match, ↓ wraps to the first.
    click_to(&mut harness, NEXT, 1, 2);

    // ↑ goes back, wrapping the other way from the first.
    click_to(&mut harness, PREVIOUS, 3, 6);
    click_to(&mut harness, PREVIOUS, 2, 4);
}

/// ↓ steps from the cursor, as F3 does.
#[test]
fn the_arrows_step_from_the_cursor() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    committed_search(&mut harness);

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_arrows_at_line(&harness, 3);

    click_to(&mut harness, NEXT, 2, 4);
}

/// With the search bar still open (F3 previews matches, issue #2111),
/// clicking an arrow steps too, and the bar stays open.
#[test]
fn clicking_with_the_search_bar_open_keeps_it_open() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    search(&mut harness, "alpha");
    // Typed but not run: nothing to step through yet.
    assert_no_arrows(&harness);

    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_arrows_at_line(&harness, 2);

    click_to(&mut harness, NEXT, 2, 4);
    harness.assert_screen_contains("Search: alpha");

    click_to(&mut harness, PREVIOUS, 1, 2);
    harness.assert_screen_contains("Search: alpha");
}

/// The search step of Replace shows the arrows while the replacement is
/// typed; the replace-all that follows ends the search, so they go.
#[test]
fn replace_shows_the_arrows_and_drops_them_after_replacing() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("alpha").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("with: ").unwrap();
    assert_arrows_at_line(&harness, 2);

    harness.type_text("omega").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.wait_for_screen_contains("omega three").unwrap();
    assert_no_arrows(&harness);
}

/// Query replace: the arrows are hidden while the y/n/a/c prompt walks the
/// matches (a click would move off the match it asks about), and the
/// finished replace ends the search instead of leaving the old positions
/// for the arrows to step through.
#[test]
fn query_replace_hides_the_arrows_and_ends_the_search() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.wait_for_screen_contains("Query replace: ").unwrap();
    harness.type_text("alpha").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Query replace 'alpha' with: ")
        .unwrap();
    assert_arrows_at_line(&harness, 2);

    harness.type_text("omega").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Replace? (y)es (n)o (a)ll (c)ancel")
        .unwrap();
    assert_no_arrows(&harness);

    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("omega one").unwrap();
    assert_no_arrows(&harness);

    // Cancel after one replacement: the search ended with it.
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.assert_screen_contains("alpha two");
    assert_no_arrows(&harness);
}

/// Escape on a search bar whose query F3 ran cancels the search with it.
#[test]
fn cancelling_the_search_removes_the_arrows() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    search(&mut harness, "alpha");
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_arrows_at_line(&harness, 2);

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt_closed().unwrap();
    assert_no_arrows(&harness);
}

/// Clear Search from the command palette removes the arrows.
#[test]
fn clearing_the_search_removes_the_arrows() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    committed_search(&mut harness);
    assert_arrows_at_line(&harness, 2);

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Clear Search").unwrap();
    harness.wait_for_screen_contains("Clear Search").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    assert_no_arrows(&harness);
}

/// The search belongs to the buffer it ran on: another tab shows no arrows
/// for it, and switching back shows them again.
#[test]
fn another_buffer_does_not_show_the_arrows() {
    let dir = TempDir::new().unwrap();
    let mut harness = harness_with(CONTENT, &dir);
    committed_search(&mut harness);
    assert_arrows_at_line(&harness, 2);

    let other = dir.path().join("other.txt");
    std::fs::write(&other, "alpha\nalpha\nalpha\nalpha\n").unwrap();
    harness.open_file(&other).unwrap();
    harness.wait_for_screen_contains("other.txt").unwrap();
    assert_no_arrows(&harness);

    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_arrows_at_line(&harness, 2);
}

/// Leaving a query replace part-way by switching tabs ends its search, as
/// finishing it does: the replacements already made left the search's
/// positions stale, and the arrows would step through them.
#[test]
fn switching_tabs_during_query_replace_ends_the_search() {
    let dir = TempDir::new().unwrap();
    let other = dir.path().join("other.txt");
    std::fs::write(&other, "nothing here\n").unwrap();
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&other).unwrap();
    let file_path = dir.path().join("test.txt");
    std::fs::write(&file_path, CONTENT).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.wait_for_screen_contains("Query replace: ").unwrap();
    harness.type_text("alpha").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Query replace 'alpha' with: ")
        .unwrap();
    harness.type_text("omega").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Replace? (y)es (n)o (a)ll (c)ancel")
        .unwrap();
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("omega one").unwrap();

    // Over to the other tab, which drops the replace prompt, and back.
    let (col, row) = harness.find_text_on_screen("other.txt").unwrap();
    harness.mouse_click(col, row).unwrap();
    harness.wait_for_screen_contains("nothing here").unwrap();
    let (col, row) = harness.find_text_on_screen("test.txt").unwrap();
    harness.mouse_click(col, row).unwrap();
    harness.wait_for_screen_contains("omega one").unwrap();
    harness.assert_screen_not_contains("Replace? (y)es");
    assert_no_arrows(&harness);
}
