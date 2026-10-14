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

/// Every row of the question starts on the same column, wrapped or not. The
/// body used to be indented by a leading space, which moved only its first
/// row: the rows it wrapped onto sat hard against the border.
#[test]
fn daemon_quit_body_rows_share_one_left_edge() {
    // Narrow enough that the body's lines wrap.
    let mut h = EditorTestHarness::with_temp_project_and_config(44, 30, Config::default()).unwrap();
    h.editor_mut().set_session_mode(true);
    h.render().unwrap();
    h.send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();

    let screen = h.screen_to_string();
    let lines: Vec<Vec<char>> = screen.lines().map(|l| l.chars().collect()).collect();
    let title_row = lines
        .iter()
        .position(|l| l.iter().collect::<String>().contains("Quit or Detach"))
        .unwrap_or_else(|| panic!("no dialog title.\nScreen:\n{screen}"));
    let border = lines[title_row]
        .iter()
        .position(|&c| c == '│')
        .expect("the title row has the card's left border");
    // Title, rule, blank; then the body until the next rule.
    let mut body_rows = 0;
    for l in &lines[title_row + 3..] {
        let inner = &l[border + 1..];
        if inner.first() == Some(&'─') {
            break;
        }
        if inner.iter().take_while(|&&c| c != '│').all(|&c| c == ' ') {
            continue;
        }
        body_rows += 1;
        assert!(
            inner[0] == ' ' && inner[1] != ' ',
            "every body row must start one column inside the border.\nScreen:\n{screen}"
        );
    }
    assert!(
        body_rows > 3,
        "the body should have wrapped.\nScreen:\n{screen}"
    );
}
