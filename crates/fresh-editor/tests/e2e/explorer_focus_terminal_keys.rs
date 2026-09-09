//! The file explorer and a focused terminal each keep their own keys.
//!
//! Assertions are on rendered output only (CONTRIBUTING.md §2).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use std::path::PathBuf;

/// Rows are cropped to this before searching, so the panes beside the
/// explorer cannot match.
const EXPLORER_COLS: usize = 40;

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

/// The `Explorer` menu is offered only while the explorer owns the keyboard.
/// Its selection glyph is not usable for this — a shell cursor paints the same
/// block character.
fn explorer_has_focus(harness: &EditorTestHarness) -> bool {
    harness.get_menu_bar().contains("Explorer")
}

fn explorer_text(harness: &EditorTestHarness) -> String {
    harness
        .screen_to_string()
        .lines()
        .map(|line| line.chars().take(EXPLORER_COLS).collect::<String>())
        .collect::<Vec<_>>()
        .join("\n")
}

fn selected_explorer_row(harness: &EditorTestHarness) -> Option<String> {
    explorer_text(harness)
        .lines()
        .find(|left| left.contains('▌'))
        .map(|left| left.to_string())
}

fn setup_project() -> (tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().unwrap();
    // On macOS a tempdir is a symlink into `/private/var`; the editor stores
    // the resolved path.
    let root = fs::canonicalize(temp.path()).unwrap();
    fs::create_dir(root.join("nested")).unwrap();
    fs::write(root.join("nested/inner_file.txt"), "inner\n").unwrap();
    fs::write(root.join("alpha.txt"), "alpha\n").unwrap();
    (temp, root)
}

/// Leaves the terminal focused, with no file open so the explorer's selection
/// starts on the root row and `nested` sits directly below it.
fn harness_with_explorer_and_terminal(root: &PathBuf) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_working_dir(140, 36, root.clone()).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer_item("nested").unwrap();

    harness
        .run_palette_command("Open Terminal to the Right")
        .unwrap();
    // Stable, not just observed: the shell's first prompt lands after the
    // command returns, and a focus handover racing it pulled the keyboard back
    // off the explorer mid-test.
    harness
        .wait_until_stable(|h| !explorer_has_focus(h))
        .unwrap();
    harness
}

/// Down doubles as the focus check — the explorer only moves its selection
/// while it owns the keyboard, and arrow keys are unaffected by the bug.
/// Expanding is synchronous, so neither step waits and the broken case fails
/// rather than blocking. Focus after Enter proves nothing: acting on a
/// directory row hands the keyboard back either way.
fn assert_enter_expands_nested(harness: &mut EditorTestHarness) {
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let before = selected_explorer_row(harness).unwrap_or_else(|| {
        panic!(
            "the explorer should have moved its selection down onto `nested`, \
             which it only does while it owns the keyboard.\nExplorer:\n{}",
            explorer_text(harness)
        )
    });
    assert!(
        before.contains("nested") && before.contains('>'),
        "expected the selection on a collapsed `nested`.\nRow: {before:?}\nExplorer:\n{}",
        explorer_text(harness)
    );

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        explorer_text(harness).contains("inner_file.txt"),
        "Enter must reach the file explorer and expand `nested` — a terminal \
         being the active buffer behind the explorer is not a reason to send \
         the explorer's keys to the shell.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Without the dispatch gate, Enter reaches the shell and `nested` stays
/// collapsed.
#[test]
#[cfg(not(windows))] // spawns a PTY-backed shell
fn enter_acts_on_the_explorer_after_clicking_back_into_it() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }
    let (_temp, root) = setup_project();
    let mut harness = harness_with_explorer_and_terminal(&root);

    // Empty space below the tree, so the click itself activates nothing.
    harness.mouse_click(20, 12).unwrap();
    harness.wait_until(explorer_has_focus).unwrap();

    assert_enter_expands_nested(&mut harness);
}

/// `/bin/cat` as the shell keeps this off the platform's line editor: with no
/// readline in the way the tty echoes the control bytes as `^B` / `^E`, which
/// is what proves they reached the PTY.
///
/// Only the Ctrl+B half fails without the fix; Ctrl+E is asserted as a guard
/// against putting `FocusFileExplorer` back on the terminal's allowlist.
#[test]
#[cfg(not(windows))] // spawns a PTY and asserts on tty control-char echo
fn ctrl_b_and_ctrl_e_reach_the_shell_when_the_terminal_is_focused() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }
    let shell = "/bin/cat";
    if !std::path::Path::new(shell).exists() {
        eprintln!("Skipping: {shell} not available");
        return;
    }

    let (_temp, root) = setup_project();
    let mut config = fresh::config::Config::default();
    config.terminal.shell = Some(fresh::config::TerminalShellConfig {
        command: shell.to_string(),
        args: Vec::new(),
    });
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 36, config, root.clone()).unwrap();
    harness.render().unwrap();

    // The explorer stays closed, so its panel appearing means a key was stolen.
    harness
        .run_palette_command("Open Terminal to the Right")
        .unwrap();
    harness.type_text("abc").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("abc"))
        .unwrap();

    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    // Resolves under either behaviour, so it gates rather than only ending
    // when the test passes.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("^B") || screen.contains("File Explorer")
        })
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("File Explorer"),
        "Ctrl+B must not open the file explorer while a terminal is focused — \
         it is the shell's backward-char.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("^B"),
        "Ctrl+B must reach the terminal's PTY.\nScreen:\n{screen}"
    );

    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("^E") || explorer_has_focus(h))
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !explorer_has_focus(&harness),
        "Ctrl+E must not move focus to the file explorer while a terminal is \
         focused — it is the shell's end-of-line.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("^E"),
        "Ctrl+E must reach the terminal's PTY.\nScreen:\n{screen}"
    );
}
