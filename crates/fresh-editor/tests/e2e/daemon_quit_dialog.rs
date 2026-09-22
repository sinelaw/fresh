//! Ctrl+Q in a daemon session asks whether to detach or quit.
//!
//! Quitting an attached client used to stop the daemon outright — and with
//! it every terminal and agent the session hosted — when what the user
//! usually wants is their shell back. The dialog offers Detach (the default),
//! Quit (stop everything), and Cancel.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn daemon_harness() -> EditorTestHarness {
    let mut h =
        EditorTestHarness::with_temp_project_and_config(120, 40, Config::default()).unwrap();
    h.editor_mut().set_session_mode(true);
    h.render().unwrap();
    h
}

fn press_quit(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("Detach") && screen.contains("Quit and Stop All"),
        "Ctrl+Q in a daemon session must ask Detach / Quit.\nScreen:\n{screen}"
    );
    assert!(!h.editor().should_quit() && !h.editor().should_detach());
}

#[test]
fn daemon_quit_enter_detaches_by_default() {
    let mut h = daemon_harness();
    press_quit(&mut h);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    assert!(
        h.editor().should_detach(),
        "Enter must pick the default, Detach"
    );
    assert!(!h.editor().should_quit(), "Detach must not stop the daemon");
}

#[test]
fn daemon_quit_choosing_quit_stops_the_daemon() {
    let mut h = daemon_harness();
    press_quit(&mut h);
    h.send_key(KeyCode::Char('q'), KeyModifiers::NONE).unwrap();
    assert!(h.editor().should_quit(), "the Quit button must quit");
    assert!(!h.editor().should_detach());
}

#[test]
fn daemon_quit_esc_cancels() {
    let mut h = daemon_harness();
    press_quit(&mut h);
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    assert!(!h.editor().should_quit() && !h.editor().should_detach());
    assert!(!h.screen_to_string().contains("Quit and Stop All"));
}

/// Outside a daemon session Ctrl+Q keeps quitting straight away.
#[test]
fn non_daemon_quit_is_unchanged() {
    let mut h =
        EditorTestHarness::with_temp_project_and_config(120, 40, Config::default()).unwrap();
    h.send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(h.editor().should_quit());
}
