//! E2E tests for keyboard shortcuts with CapsLock active.
//!
//! When CapsLock is on, terminals with the kitty keyboard protocol send
//! uppercase characters (e.g. `Char('A')`) WITHOUT the SHIFT modifier.
//! The editor must still resolve shortcuts like Ctrl+A (select all),
//! Ctrl+C (copy), Ctrl+V (paste), Ctrl+X (cut), Ctrl+Z (undo), etc.
//!
//! Reproduces: CapsLock breaks Ctrl+A, Ctrl+C, Ctrl+V and other shortcuts

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Simulate CapsLock by sending an uppercase Char with CONTROL but no SHIFT.
/// This is what terminals with kitty keyboard protocol report when CapsLock
/// is on and the user presses Ctrl+<letter>.
fn capslock_ctrl(c: char) -> (KeyCode, KeyModifiers) {
    (KeyCode::Char(c.to_ascii_uppercase()), KeyModifiers::CONTROL)
}

#[test]
fn test_capslock_ctrl_a_selects_all() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();
    harness.assert_buffer_content("hello world");

    // CapsLock + Ctrl+A should select all text
    let (code, mods) = capslock_ctrl('a');
    harness.send_key(code, mods).unwrap();

    // Typing should replace the selection
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("X");
}

#[test]
fn test_capslock_ctrl_c_copies() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello").unwrap();

    // Select all with normal Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();

    // CapsLock + Ctrl+C should copy
    let (code, mods) = capslock_ctrl('c');
    harness.send_key(code, mods).unwrap();

    // Move to end and paste with normal Ctrl+V
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();

    harness.assert_buffer_content("hellohello");
}

#[test]
fn test_capslock_ctrl_v_pastes() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello").unwrap();

    // Select all and copy normally
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Move to end
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // CapsLock + Ctrl+V should paste
    let (code, mods) = capslock_ctrl('v');
    harness.send_key(code, mods).unwrap();

    harness.assert_buffer_content("hellohello");
}

#[test]
fn test_capslock_ctrl_x_cuts() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello").unwrap();

    // Select all with normal Ctrl+A
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();

    // CapsLock + Ctrl+X should cut
    let (code, mods) = capslock_ctrl('x');
    harness.send_key(code, mods).unwrap();

    // Buffer should be empty after cut
    harness.assert_buffer_content("");

    // Paste should bring back the text
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("hello");
}

#[test]
fn test_capslock_ctrl_z_undoes() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // CapsLock + Ctrl+Z should undo (at least one character)
    let (code, mods) = capslock_ctrl('z');
    harness.send_key(code, mods).unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.len() < 5,
        "Undo should have removed at least one character, got: {:?}",
        content
    );
}

#[test]
fn test_capslock_ctrl_f_opens_search() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("hello world").unwrap();

    // CapsLock + Ctrl+F should open search
    let (code, mods) = capslock_ctrl('f');
    harness.send_key(code, mods).unwrap();

    // Search bar should be visible
    harness.assert_screen_contains("Search:");
}
