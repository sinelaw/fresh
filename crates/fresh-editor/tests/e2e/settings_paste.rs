use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

fn send_text(harness: &mut EditorTestHarness, text: &str) {
    for c in text.chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
}

#[test]
fn test_settings_paste() {
    let mut harness = EditorTestHarness::new(100, 40).unwrap();

    // Isolate from the host clipboard: the Ctrl+C below writes the copied
    // text to the internal clipboard and the later Ctrl+V reads it back from
    // there, so the copy/paste round-trip can't be corrupted by (or corrupt)
    // a parallel test sharing the real OS clipboard.
    harness.editor_mut().set_clipboard_for_test(String::new());

    // Set clipboard content to "rust"
    send_text(&mut harness, "rust");
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Open settings
    harness.open_settings().unwrap();

    // Search for "languages"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "languages");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // Confirm search
    harness.render().unwrap();

    // Enter to open "Add Language" dialog (since it's a Map and we are on "Add new")
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the Edit Value dialog opened on the first language entry.
    // Don't assert which language that is — the list is alphabetical and
    // breaks every time a language sorting earlier is added.
    harness.assert_screen_contains("Edit Value");
    harness.assert_screen_contains("Key");

    // Navigate down to Comment Prefix field (Key is read-only for existing entries)
    loop {
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        if screen.contains(">  Comment Prefix") || screen.contains(">● Comment Prefix") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    // Enter to start editing the "Comment Prefix" field
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Paste "rust"
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify content is pasted into Comment Prefix field
    harness.assert_screen_contains("rust");
}
