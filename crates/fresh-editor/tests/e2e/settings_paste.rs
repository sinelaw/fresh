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

    // Verify Edit Value dialog shows first language alphabetically (astro)
    harness.assert_screen_contains("Key");
    harness.assert_screen_contains("astro");

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
    // Note: astro has no comment_prefix set, so the field starts empty
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
