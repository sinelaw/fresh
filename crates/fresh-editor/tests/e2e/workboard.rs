//! E2E coverage for the Workboard plugin: the full-screen board over the
//! Orchestrator's workspaces. One local workspace (the project itself) is
//! enough to drive the wiring — the palette command, the lens keys, pinning
//! a card on the Board, the pin and lens surviving a close and reopen, the
//! mouse (a click selects, a double click opens), and the jump command with
//! nothing to jump to. Sorting and layout are covered
//! by the plugin's own unit tests (`plugins/tests/workboard_*.test.ts`).
//!
//! Driven through the keyboard, asserted on rendered output.
#![cfg(unix)]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

/// A git project with the Orchestrator and the Workboard installed.
fn setup() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("homeproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    copy_plugin(&plugins_dir, "workboard");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    assert!(std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success());
    (temp_dir, root)
}

fn run_command(h: &mut EditorTestHarness, name: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(name).unwrap();
    h.wait_until(|h| h.screen_to_string().contains(name))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
}

fn open_board(h: &mut EditorTestHarness) {
    run_command(h, "Workboard: Open");
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Workboard")
            && s.contains("Inbox")
            && s.contains("Folders")
            && s.contains("homeproj")
    })
    .unwrap();
}

fn press(h: &mut EditorTestHarness, ch: char) {
    h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
}

/// The board opens on the Inbox, where an idle workspace is parked; `4`
/// switches to the Board, where a clean idle workspace is in Backlog.
#[test]
fn board_opens_on_the_inbox_and_switches_lens() {
    let (_tmp, root) = setup();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 30, Config::default(), root).unwrap();
    h.render().unwrap();
    open_board(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("PARKED"))
        .unwrap();

    press(&mut h, '4');
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Backlog  1") && s.contains("Needs me  0") && s.contains("Shipped  0")
    })
    .unwrap();

    // Projects groups the workspace under its repository's directory.
    press(&mut h, '2');
    h.wait_until(|h| !h.screen_to_string().contains("Backlog  1"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("homeproj  1"),
        "Projects lists the repo with its count:\n{screen}"
    );
}

/// `>` pins the card one column right; the pin and the lens survive closing
/// and reopening the board, and `p` unpins it.
#[test]
fn a_pinned_card_stays_put_across_a_reopen() {
    let (_tmp, root) = setup();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 30, Config::default(), root).unwrap();
    h.render().unwrap();
    open_board(&mut h);
    press(&mut h, '4');
    h.wait_until(|h| h.screen_to_string().contains("Backlog  1"))
        .unwrap();

    press(&mut h, '>');
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Backlog  0") && s.contains("Needs me  1") && s.contains("⊙")
    })
    .unwrap();

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Needs me  1"))
        .unwrap();

    open_board(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Needs me  1") && s.contains("⊙")
    })
    .unwrap();

    press(&mut h, 'p');
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Backlog  1") && !s.contains("⊙")
    })
    .unwrap();
}

/// With no agent waiting and no unseen finished work, the jump command says
/// so rather than moving anywhere.
#[test]
fn next_waiting_agent_with_nothing_waiting_says_so() {
    let (_tmp, root) = setup();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 30, Config::default(), root).unwrap();
    h.render().unwrap();
    run_command(&mut h, "Workboard: Next Waiting Agent");
    h.wait_until(|h| h.screen_to_string().contains("nothing needs you"))
        .unwrap();
}

/// The screen cell (col, row) where `needle` starts.
fn pos_of(h: &EditorTestHarness, needle: &str) -> (u16, u16) {
    let screen = h.screen_to_string();
    screen
        .lines()
        .enumerate()
        .find_map(|(r, l)| {
            l.find(needle)
                .map(|b| (l[..b].chars().count() as u16, r as u16))
        })
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{screen}"))
}

/// Past the double-click window, so the next press starts a fresh count.
fn past_double_click(h: &EditorTestHarness) {
    let window = h.config().editor.double_click_time_ms.saturating_mul(2);
    h.advance_time(std::time::Duration::from_millis(window));
}

/// A click on a card only selects it — the board stays up — and a double
/// click opens its workspace, as Enter does, which closes the board.
#[test]
fn a_click_selects_a_card_and_a_double_click_opens_it() {
    let (_tmp, root) = setup();
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 30, Config::default(), root).unwrap();
    h.render().unwrap();
    open_board(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("PARKED"))
        .unwrap();
    let (col, row) = pos_of(&h, "homeproj");

    past_double_click(&h);
    h.mouse_click(col, row).unwrap();
    past_double_click(&h);
    h.render().unwrap();
    assert!(
        h.screen_to_string().contains("1 Inbox"),
        "a single click leaves the board open:\n{}",
        h.screen_to_string()
    );

    h.mouse_click(col, row).unwrap();
    h.mouse_click(col, row).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("1 Inbox"))
        .unwrap();
}
