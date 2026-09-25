//! Ctrl+J from a legacy terminal (a raw LF byte) acts as Enter in the editor.
//!
//! The parser reports LF as Ctrl+J so a program in the integrated terminal
//! can tell it from Enter's CR (sinelaw/fresh#3169, covered by
//! `terminal_child_keys::ctrl_j_reaches_the_child_as_lf_and_enter_as_cr`).
//! Before that, LF was parsed as Enter, which is what the editor itself
//! still wants: nothing binds Ctrl+J, so after the parser change it did
//! nothing at all — no newline in a buffer, no confirm in a prompt.
//!
//! These feed the raw bytes a terminal sends through `InputParser` and assert
//! on what ends up rendered.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::server::input_parser::{Event, InputParser};
use tempfile::TempDir;

/// Feed raw terminal bytes through the parser into the editor, exactly as
/// session mode does on the server side.
fn send_bytes(harness: &mut EditorTestHarness, bytes: &[u8]) {
    for event in InputParser::new().parse(bytes) {
        if let Event::Key(press) = event {
            harness.send_key_press(press).unwrap();
        }
    }
}

fn harness_with_file(content: &str) -> (TempDir, EditorTestHarness) {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("notes.txt");
    std::fs::write(&file_path, content).unwrap();
    let mut config = Config::default();
    // Pin the "default" keymap; `Config::default()` picks `macos` on macOS.
    config.active_keybinding_map = fresh::config::KeybindingMapName("default".to_string());
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    (temp_dir, harness)
}

/// LF in a buffer breaks the line, as Enter does.
#[test]
fn ctrl_j_inserts_a_newline_in_a_buffer() {
    let (_temp_dir, mut harness) = harness_with_file("onetwo\n");
    harness.assert_screen_contains("onetwo");

    // After "one", then LF.
    send_bytes(&mut harness, b"\x1b[C\x1b[C\x1b[C");
    send_bytes(&mut harness, b"\n");

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("onetwo") && screen.contains("Ln 2, Col 1"),
        "Ctrl+J must split the line at the cursor:\n{screen}"
    );
    harness.assert_screen_contains("two");
}

/// LF in a prompt confirms it, as Enter does: Go to Line jumps.
#[test]
fn ctrl_j_confirms_a_prompt() {
    let (_temp_dir, mut harness) = harness_with_file("a\nb\nc\nd\n");

    // Ctrl+G (0x07) opens Go to Line.
    send_bytes(&mut harness, &[0x07]);
    harness.wait_for_prompt().unwrap();
    send_bytes(&mut harness, b"3");
    send_bytes(&mut harness, b"\n");

    harness.wait_for_prompt_closed().unwrap();
    harness.assert_screen_contains("Ln 3, Col 1");
}
