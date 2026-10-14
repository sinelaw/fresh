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
use fresh::config::{Config, Keybinding};
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
    harness_with_bindings(content, &[])
}

/// The same, with the user's own `(chord, action)` bindings added.
fn harness_with_bindings(content: &str, bindings: &[(&str, &str)]) -> (TempDir, EditorTestHarness) {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("notes.txt");
    std::fs::write(&file_path, content).unwrap();
    let mut config = Config::default();
    // Pin the "default" keymap; `Config::default()` picks `macos` on macOS.
    config.active_keybinding_map = fresh::config::KeybindingMapName("default".to_string());
    for (chord, action) in bindings {
        config.keybindings.push(Keybinding {
            key: String::new(),
            modifiers: Vec::new(),
            keys: Vec::new(),
            chord: chord.to_string(),
            action: action.to_string(),
            args: Default::default(),
            when: None,
        });
    }
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

/// The buffer's only line once the keys have run: `X` alone means Select
/// All ran and the `X` replaced everything.
fn assert_select_all_ran(harness: &EditorTestHarness) {
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("one") && !screen.contains("two") && screen.contains("X"),
        "the bound key must run Select All, which `X` then replaces:\n{screen}"
    );
}

/// A user's chord that starts with Ctrl+J runs: LF must reach the chord
/// resolver as Ctrl+J, not as the Enter an unbound Ctrl+J reads as.
#[test]
fn a_ctrl_j_chord_runs_from_a_legacy_terminal() {
    let (_temp_dir, mut harness) =
        harness_with_bindings("one\ntwo\n", &[("C-j C-k", "select_all")]);

    // LF, then Ctrl+K (0x0B), then an `X` to replace the selection.
    send_bytes(&mut harness, b"\n");
    send_bytes(&mut harness, &[0x0b]);
    send_bytes(&mut harness, b"X");
    harness.render().unwrap();

    assert_select_all_ran(&harness);
}

/// A `noop` on Ctrl+J disables the key; it must not turn into Enter.
#[test]
fn a_noop_ctrl_j_does_nothing() {
    let (_temp_dir, mut harness) = harness_with_bindings("onetwo\n", &[("C-j", "noop")]);

    send_bytes(&mut harness, b"\x1b[C\x1b[C\x1b[C");
    send_bytes(&mut harness, b"\n");
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("onetwo") && screen.contains("Ln 1, Col 4"),
        "a noop Ctrl+J must leave the line whole:\n{screen}"
    );
}

/// ESC LF, which some terminals send for Alt+Enter, parses as Ctrl+Alt+J;
/// unbound, it is Alt+Enter, as it was when LF parsed as Enter. On Windows
/// too, where the keymap types Ctrl+Alt+<char> as AltGr text: from the
/// parser, a Ctrl+Alt+J is ESC LF, never AltGr (see `router::ctrl_j_reading`).
#[test]
fn esc_lf_is_alt_enter() {
    let (_temp_dir, mut harness) =
        harness_with_bindings("one\ntwo\n", &[("M-Enter", "select_all")]);

    send_bytes(&mut harness, b"\x1b\n");
    send_bytes(&mut harness, b"X");
    harness.render().unwrap();

    assert_select_all_ran(&harness);
}

/// A mode that binds Ctrl+J holds it only where that mode resolves keys:
/// the buffer (merge_conflict's `merge-result` binds `C-j` to the next
/// conflict). A prompt never resolves the buffer's mode, so there LF still
/// confirms (sinelaw/fresh#3384).
#[cfg(feature = "plugins")]
#[test]
fn a_modes_ctrl_j_does_not_stop_it_confirming_a_prompt() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::input::keybindings::{Action, KeyContext};
    let (_temp_dir, mut harness) = harness_with_file("a\nb\nc\nd\n");
    {
        let kb = harness.editor().keybindings_for_tests();
        kb.write().unwrap().load_plugin_default(
            KeyContext::Mode("merge-result".to_string()),
            KeyCode::Char('j'),
            KeyModifiers::CONTROL,
            Action::PluginAction("merge_next_conflict".to_string()),
        );
    }
    harness
        .editor_mut()
        .handle_plugin_command(fresh_core::api::PluginCommand::SetEditorMode {
            mode: Some("merge-result".to_string()),
        })
        .unwrap();

    // Ctrl+G (0x07) opens Go to Line.
    send_bytes(&mut harness, &[0x07]);
    harness.wait_for_prompt().unwrap();
    send_bytes(&mut harness, b"3");
    send_bytes(&mut harness, b"\n");

    harness.render().unwrap();
    assert!(
        !harness.editor().is_prompting(),
        "Ctrl+J must confirm the prompt, the buffer's mode notwithstanding"
    );
    harness.assert_screen_contains("Ln 3, Col 1");
}
