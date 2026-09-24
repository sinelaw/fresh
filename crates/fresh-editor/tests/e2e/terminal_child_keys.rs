//! What a child program in the integrated terminal actually receives.
//!
//! Each test runs a tiny raw-mode byte dumper as the terminal's shell: it
//! prints every byte it reads from the PTY as ` xx` hex, so the screen shows
//! exactly what the editor forwarded. A sentinel key (`z`, ` 7a`) typed after
//! the key under test is the semantic wait — it lands under either behaviour,
//! and what precedes it says whether the key reached the child.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, Keybinding, TerminalShellConfig};
use fresh::server::input_parser::{Event, InputParser};
use portable_pty::{native_pty_system, PtySize};

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

/// A config whose terminal runs the byte dumper, on the default keymap
/// (`Config::default()` picks `macos` on macOS).
fn dumper_config() -> Config {
    let mut config = Config::default();
    config.active_keybinding_map = fresh::config::KeybindingMapName("default".to_string());
    config.terminal.shell = Some(TerminalShellConfig {
        command: "/bin/sh".into(),
        args: vec![
            "-c".into(),
            "stty raw -echo; printf 'READY\\r\\n'; \
             while :; do \
               b=$(dd bs=1 count=1 2>/dev/null | od -An -tx1 | tr -d ' \\n'); \
               printf ' %s' \"$b\"; \
             done"
                .into(),
        ],
    });
    config
}

/// Open the dumper terminal and wait until it is reading.
fn open_dumper(config: Config) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 30, config).unwrap();
    harness.editor_mut().open_terminal();
    harness.wait_for_screen_contains("READY").unwrap();
    harness
}

/// Feed raw bytes, as the host terminal sends them, through the input parser
/// into the editor — the path a real keystroke takes.
fn send_bytes(harness: &mut EditorTestHarness, bytes: &[u8]) {
    for event in InputParser::new().parse(bytes) {
        if let Event::Key(press) = event {
            harness.send_key_press(press).unwrap();
        }
    }
}

/// Press the sentinel and wait for the child to report it.
fn send_sentinel(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains(" 7a").unwrap();
}

/// Issue #3270: a user `noop` binding for Ctrl+Q in the `terminal` context
/// must stop the editor's own Ctrl+Q (quit) and hand the key to the child,
/// so a non-modal editor running in the terminal can use it.
#[test]
#[cfg(unix)]
fn terminal_noop_binding_sends_ctrl_q_to_the_child_instead_of_quitting() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }
    let mut config = dumper_config();
    config.keybindings.push(Keybinding {
        key: "q".into(),
        modifiers: vec!["ctrl".into()],
        keys: Vec::new(),
        chord: String::new(),
        action: "noop".into(),
        args: Default::default(),
        when: Some("terminal".into()),
    });
    let mut harness = open_dumper(config);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    send_sentinel(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !harness.editor().should_quit(),
        "Ctrl+Q bound to noop in the terminal context must not quit the editor"
    );
    assert!(
        screen.contains(" 11 7a"),
        "Ctrl+Q (0x11) must reach the child before the sentinel.\nScreen:\n{screen}"
    );
}

/// Issue #3169: Ctrl+J arrives from the host terminal as LF (0x0A) and must
/// reach the child as LF, not as Enter's CR — programs such as coding agents
/// insert a newline on Ctrl+J and submit on Enter.
#[test]
#[cfg(unix)]
fn ctrl_j_reaches_the_child_as_lf_and_enter_as_cr() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }
    let mut harness = open_dumper(dumper_config());

    send_bytes(&mut harness, b"\n");
    send_bytes(&mut harness, b"\r");
    send_sentinel(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(" 0a 0d 7a"),
        "Ctrl+J must reach the child as LF (0a) and Enter as CR (0d).\nScreen:\n{screen}"
    );
}
