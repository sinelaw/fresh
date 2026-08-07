//! Regression test for #2810: in session (daemon, `fresh -a`) mode the Escape
//! key did nothing until the *next* keypress, which was then swallowed along
//! with it.
//!
//! ## The wedge the user reported
//! `Alt+A` opens the project Search/Replace panel. `Esc` — advertised by the
//! panel's own hint line as "Esc close" — leaves it open and focused. The
//! following `Ctrl+P` never opens the command palette, and the characters typed
//! for the palette are inserted into the open file instead, marking it dirty.
//!
//! ## Root cause
//! A session client sends raw terminal bytes; the server decodes them with
//! `InputParser`. A lone trailing `ESC` is ambiguous — the Escape key, or the
//! head of an escape sequence — so the parser buffers it until the next byte
//! decides. The direct (tty) reader resolves that with a poll timeout; the
//! server's socket read is non-blocking and nothing ever resolved it, so the
//! `ESC` waited indefinitely. The next keypress then completed it as an Alt
//! chord: `ESC` + `Ctrl+P` (`0x10`) became one Alt+Ctrl+P event, consuming both
//! keys. (`InputParser` compounded it by dropping the Control bit, so that
//! chord matched the bound `Alt+P` and ran "find previous match".)
//!
//! ## What this test does
//! It drives the real session decode path — raw bytes through
//! `ClientInputParser`, its events into the editor — and asserts only on
//! rendered output: the panel closes on Escape, `Ctrl+P` opens the palette, and
//! the keystrokes meant for the palette never reach the file.
//!
//! Without the fix `flush_idle` yields nothing, the panel stays on screen and
//! the test fails at the first wait.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::Event;
use fresh::server::input_parser::ClientInputParser;
use std::fs;
use std::time::{Duration, Instant};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 30;

/// Comfortably longer than the parser's escape grace window, so `flush_idle`
/// resolves a buffered `ESC` the way an idle socket does on the server. Passed
/// as an explicit `Instant` rather than slept, so the test stays
/// time-insensitive.
///
/// Derived from the grace rather than hard-coded: it sat at exactly the default
/// window, so raising that default (as #2793 did) left this test passing only on
/// the `>=` boundary.
const PAST_ESC_GRACE: Duration =
    fresh::server::input_parser::DEFAULT_ESC_GRACE.saturating_add(Duration::from_millis(10));

/// Raw bytes for the keys the report uses. A session client forwards exactly
/// these; `Alt+A` is `ESC a`, Escape is a lone `ESC`, `Ctrl+P` is `0x10`.
const ALT_A: &[u8] = b"\x1ba";
const ESCAPE: &[u8] = b"\x1b";
const CTRL_P: &[u8] = b"\x10";

/// Project with the search_replace plugin and one file to search in.
fn setup() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    fs::write(
        project_root.join("alpha_one.rs"),
        "fn main() {\n    println!(\"alpha one\");\n}\n",
    )
    .unwrap();

    (temp_dir, project_root)
}

/// Feed one chunk of client bytes through the session decode path and dispatch
/// whatever it produces into the editor, then let the socket go idle so a
/// buffered lone `ESC` resolves — exactly what the server loop does per tick.
fn send_bytes(harness: &mut EditorTestHarness, parser: &mut ClientInputParser, bytes: &[u8]) {
    let mut events = parser.parse(bytes);
    events.extend(parser.flush_idle(Instant::now() + PAST_ESC_GRACE));
    for event in events {
        if let Event::Key(ke) = event {
            harness.send_key(ke.code, ke.modifiers).unwrap();
        }
    }
    harness.render().unwrap();
}

/// Esc closes the Search/Replace panel and the next Ctrl+P opens the palette,
/// so the characters typed for it never land in the file.
#[test]
fn session_escape_closes_search_replace_and_frees_the_palette() {
    let (_temp_dir, project_root) = setup();
    let start_file = project_root.join("alpha_one.rs");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Default::default(),
        project_root,
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    let mut parser = ClientInputParser::new();

    // Alt+A → the project Search/Replace panel.
    send_bytes(&mut harness, &mut parser, ALT_A);
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();

    // Escape must close it — the panel's own hint line says "Esc close".
    send_bytes(&mut harness, &mut parser, ESCAPE);
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Search/Replace*"))
        .unwrap();

    // Ctrl+P must reach the palette rather than being absorbed into the
    // still-pending escape.
    send_bytes(&mut harness, &mut parser, CTRL_P);
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(">command"),
        "Ctrl+P after Escape should open the command palette. Screen:\n{}",
        screen
    );

    // …so the characters typed for the palette go to the palette, not the file.
    send_bytes(&mut harness, &mut parser, b"Open Terminal");
    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        !content.contains("Open Terminal"),
        "palette keystrokes were inserted into the buffer: {:?}",
        content
    );
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("alpha_one.rs*"),
        "the buffer must not be marked dirty by palette keystrokes. Screen:\n{}",
        screen
    );
}

/// The plain-editor half of the same wedge: Escape then Ctrl+P must not
/// collapse into one Alt chord that runs an unrelated command and drops both
/// keypresses.
#[test]
fn session_escape_then_ctrl_p_is_two_keys_not_an_alt_chord() {
    let (_temp_dir, project_root) = setup();
    let start_file = project_root.join("alpha_one.rs");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Default::default(),
        project_root,
    )
    .unwrap();
    harness.open_file(&start_file).unwrap();
    harness.render().unwrap();

    let mut parser = ClientInputParser::new();
    send_bytes(&mut harness, &mut parser, ESCAPE);
    send_bytes(&mut harness, &mut parser, CTRL_P);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(">command"),
        "Escape followed by Ctrl+P should still open the palette. Screen:\n{}",
        screen
    );
    // The mis-decoded chord used to resolve to Alt+P (find previous match),
    // which reports its own status message.
    assert!(
        !screen.contains("Match 1 of"),
        "Escape + Ctrl+P must not run the Alt+P binding. Screen:\n{}",
        screen
    );
}
