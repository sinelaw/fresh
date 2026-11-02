//! Regression test for issue #2122: `move_to_paragraph_up` /
//! `move_to_paragraph_down` existed but nothing reached them — no default
//! key, no command-palette entry, no menu item.
//!
//! They are now palette commands ("Go to Next Paragraph" / "Go to Previous
//! Paragraph") and entries in the Go menu. No default key is added: the
//! obvious candidate, Ctrl+Up/Down, means "scroll one line" in other editors.
//!
//! Observable: the cursor's byte offset lands on the blank line that ends
//! the paragraph, with no selection.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Byte offsets of the two blank lines in [`TEXT`].
const TEXT: &str = "one\ntwo\n\nthree\nfour\n\nfive\n";
const FIRST_BLANK: usize = 8;
const SECOND_BLANK: usize = 20;

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

#[test]
fn palette_moves_to_next_and_previous_paragraph() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    let _fixture = harness.load_buffer_from_text(TEXT).unwrap();
    assert_eq!(harness.cursor_position(), 0);

    run_palette_command(&mut harness, "Go to Next Paragraph");
    assert_eq!(harness.cursor_position(), FIRST_BLANK);

    run_palette_command(&mut harness, "Go to Next Paragraph");
    assert_eq!(harness.cursor_position(), SECOND_BLANK);

    run_palette_command(&mut harness, "Go to Previous Paragraph");
    assert_eq!(harness.cursor_position(), FIRST_BLANK);

    // A motion, not a selection.
    assert!(harness.get_selected_text().is_empty());
}

#[test]
fn go_menu_lists_and_runs_the_paragraph_motions() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    let _fixture = harness.load_buffer_from_text(TEXT).unwrap();

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Next Paragraph");
    harness.assert_screen_contains("Previous Paragraph");

    // Go to Line, Go to Definition, Find References, Go to Implementation,
    // then (past the separator) Next Paragraph.
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), FIRST_BLANK);
}
