use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Regression test: Shift+Backspace should delete a character backward,
/// just like plain Backspace.
///
/// Users sometimes hold Shift accidentally (e.g. after typing a capital
/// letter) when pressing Backspace. The editor should treat that identically
/// to Backspace rather than ignoring the key.
///
/// Reproduces: https://github.com/sinelaw/fresh/issues/1588
#[test]
fn test_shift_backspace_deletes_character() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Type some text.
    harness.type_text("hello").unwrap();
    harness.assert_buffer_content("hello");

    // Shift+Backspace should delete the last character, same as Backspace.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::SHIFT)
        .unwrap();
    harness.assert_buffer_content("hell");

    // Pressing plain Backspace should also work.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("hel");

    // Another Shift+Backspace to confirm repeated behavior.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::SHIFT)
        .unwrap();
    harness.assert_buffer_content("he");
}
