//! Regression tests for issue #3214: a confirmation prompt wider than the
//! terminal was cut off at the last column, so its last answers (like
//! "(C)ancel") were simply gone, with nothing to show they were missing.
//!
//! These bring up the real quit prompt in a narrow terminal and read the
//! rendered screen, which is what the user actually sees.

use crate::common::global_state::pin_config_globals;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, LocaleName};

const HEIGHT: u16 = 24;

/// Opens a file, edits it and presses Ctrl+Q, so the quit prompt is up. Hot
/// exit is on, which adds the "(recoverable)" answer and makes the English
/// message 99 columns wide.
fn quit_prompt_up(width: u16, mut config: Config) -> EditorTestHarness {
    config.editor.hot_exit = true;
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(width, HEIGHT, config).unwrap();
    let file = harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(&file, "hello\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.type_text("unsaved").unwrap();
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
}

/// At 80 columns the English quit prompt wraps onto a second row, so every
/// answer is on screen. Typing an answer keeps it that way: the prompt takes
/// the answer as typed input, and it used to snap back to one clipped row as
/// soon as the input wasn't empty.
#[test]
fn quit_prompt_keeps_cancel_visible_at_80_columns() {
    let mut harness = quit_prompt_up(80, Config::default());

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("(recoverable)") && screen.contains("(C)ancel"),
        "every answer should be visible:\n{screen}"
    );

    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("(C)ancel? d"),
        "cancel should still be visible after typing an answer, with the answer after it:\n{screen}"
    );
    assert!(!harness.editor().should_quit(), "nothing was confirmed yet");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    assert!(
        !harness.editor().should_quit(),
        "cancelling the prompt must not quit"
    );
}

/// The German message needs five rows at 30 columns. The prompt stops at
/// three, ends the last one with an ellipsis, and leaves the rest of the
/// screen to the buffer.
#[test]
fn german_quit_prompt_on_a_tiny_terminal_stops_at_three_rows() {
    // Declared first so it is dropped last, which resets the locale for the
    // next test.
    let _locale = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("de".to_string())),
        ..Default::default()
    };
    let harness = quit_prompt_up(30, config);

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let first = lines
        .iter()
        .position(|l| l.contains("1 Buffer hat"))
        .unwrap_or_else(|| panic!("the German prompt should be up:\n{screen}"));
    let last = lines
        .iter()
        .position(|l| l.trim_end().ends_with('…'))
        .unwrap_or_else(|| panic!("the cut should end in an ellipsis:\n{screen}"));
    assert_eq!(
        last - first + 1,
        3,
        "the prompt should take exactly three rows:\n{screen}"
    );
    assert!(
        screen.contains("unsaved"),
        "the buffer should still be on screen above the prompt:\n{screen}"
    );
}
