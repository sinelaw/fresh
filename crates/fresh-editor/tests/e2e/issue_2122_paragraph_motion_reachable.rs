//! Regression test for issue #2122: `move_to_paragraph_up` /
//! `move_to_paragraph_down` existed but nothing reached them — no default
//! key, no command-palette entry, no menu item.
//!
//! They are now palette commands ("Go to Next Paragraph" / "Go to Previous
//! Paragraph") and entries in the Go menu. No default key is added: the
//! obvious candidate, Ctrl+Up/Down, means "scroll one line" in other editors.
//!
//! Observable: the status bar's `Ln N, Col 1` lands on the blank line that
//! ends the paragraph, and the text it moved over is not painted as selected.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Blank lines at 3 and 6.
const TEXT: &str = "one\ntwo\n\nthree\nfour\n\nfive\n";

fn run_palette_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(command).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// The motion moves the cursor without selecting: the line it passed over
/// keeps the ordinary background.
fn assert_not_selected(harness: &EditorTestHarness, word: &str) {
    let (x, y) = harness
        .find_text_on_screen(word)
        .unwrap_or_else(|| panic!("{word:?} should be on screen"));
    let bg = harness.get_cell_style(x, y).and_then(|s| s.bg);
    assert_ne!(
        bg,
        Some(harness.editor().theme().selection_bg),
        "{word:?} should not be painted as selected"
    );
}

#[test]
fn palette_moves_to_next_and_previous_paragraph() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    let _fixture = harness.load_buffer_from_text(TEXT).unwrap();
    harness.assert_screen_contains("Ln 1, Col 1");

    run_palette_command(&mut harness, "Go to Next Paragraph");
    harness.assert_screen_contains("Ln 3, Col 1");
    assert_not_selected(&harness, "two");

    run_palette_command(&mut harness, "Go to Next Paragraph");
    harness.assert_screen_contains("Ln 6, Col 1");

    run_palette_command(&mut harness, "Go to Previous Paragraph");
    harness.assert_screen_contains("Ln 3, Col 1");
    assert_not_selected(&harness, "four");
}

#[test]
fn go_menu_lists_and_runs_the_paragraph_motions() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    let _fixture = harness.load_buffer_from_text(TEXT).unwrap();
    harness.assert_screen_not_contains("Next Paragraph");

    // The entries appear only once the Go menu is open, so they belong to it.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Go to Line");
    harness.assert_screen_contains("Previous Paragraph");

    let (x, y) = harness
        .find_text_on_screen("Next Paragraph")
        .expect("Go menu should list Next Paragraph");
    harness.mouse_click(x, y).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Next Paragraph");
    harness.assert_screen_contains("Ln 3, Col 1");
}
