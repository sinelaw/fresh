//! The explorer says why it did not move (sinelaw/fresh#3326, H).
//!
//! Opening or focusing the explorer reveals the active file in the tree.
//! When it cannot — the buffer has no file, or the file is outside the
//! project — the tree used to sit on its root row while the status said only
//! that the explorer was focused. Now the status says which, and the two
//! paths (column hidden, column shown) use one wording.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// The status message as set, not as painted: the status bar truncates a
/// long message to the room it has, which at this width is not enough.
fn status_line(h: &EditorTestHarness) -> String {
    h.editor().get_status_message().cloned().unwrap_or_default()
}

#[test]
fn focusing_the_explorer_says_why_it_could_not_reveal() {
    let mut h = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = h.project_dir().unwrap();
    fs::write(project.join("a.txt"), "hello\n").unwrap();

    // An unnamed buffer: nothing to reveal, and the wording says so — on
    // the hidden→shown path, which used to say "opened".
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_file_explorer().unwrap();
    h.wait_until(|h| status_line(h).contains("no file to reveal"))
        .unwrap_or_else(|e| panic!("{e}\n{}", h.screen_to_string()));
    assert!(
        status_line(&h).starts_with("File explorer focused;"),
        "one wording for opening and focusing: {:?}",
        status_line(&h)
    );

    // A file outside the project: the tree cannot show it, and says so.
    let outside = tempfile::TempDir::new().unwrap();
    let elsewhere = outside.path().join("elsewhere.txt");
    fs::write(&elsewhere, "far away\n").unwrap();
    h.editor_mut().open_file(&elsewhere).unwrap();
    h.render().unwrap();
    // Back to the editor, then focus the explorer again (the shown path).
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|h| status_line(h).contains("elsewhere.txt is outside the project"))
        .unwrap_or_else(|e| panic!("{e}\n{}", h.screen_to_string()));

    // A file in the project reveals, and the plain wording is back.
    h.editor_mut().open_file(&project.join("a.txt")).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|h| {
        let s = status_line(h);
        s.contains("File explorer focused") && !s.contains(';')
    })
    .unwrap_or_else(|e| panic!("{e}\n{}", h.screen_to_string()));
}
