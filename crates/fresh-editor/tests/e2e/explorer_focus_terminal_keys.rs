//! The file explorer keeps its keys when a terminal is the active buffer.
//!
//! `dispatch_terminal_input`'s scroll-back branch used to forward any plain
//! key — Enter, Tab, Backspace, every character — to the PTY whenever the
//! *active buffer* was a terminal, without asking who actually owns the
//! keyboard. The active buffer stays the terminal while the user is off in
//! the file explorer, so pressing Enter there dived into the shell (which
//! then took focus back) instead of acting on the selected entry. Issue #2029
//! fixed the same class for the *live* branch of that dispatch; this is the
//! scroll-back branch, which the gate never reached.
//!
//! Per CONTRIBUTING.md §2 every assertion is on rendered output: the menu bar,
//! the explorer's selection glyph, its expand/collapse arrows, and the tree.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use std::path::PathBuf;

/// How many columns the explorer panel occupies at the width these tests use.
/// Screen rows are truncated to this before being searched, so only the
/// explorer's own cells can match — the editor and terminal panes beside it
/// draw block glyphs and file names of their own.
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

/// Whether the file explorer owns the keyboard, read off the menu bar: the
/// `Explorer` menu is offered only while it does.
///
/// The explorer's selection glyph is deliberately *not* the signal here — it
/// is a block character a shell cursor also paints, so a whole-screen search
/// for it can match the terminal pane instead.
fn explorer_has_focus(harness: &EditorTestHarness) -> bool {
    harness.get_menu_bar().contains("Explorer")
}

/// Everything the explorer panel itself has drawn, with the panes beside it
/// cropped away.
fn explorer_text(harness: &EditorTestHarness) -> String {
    harness
        .screen_to_string()
        .lines()
        .map(|line| line.chars().take(EXPLORER_COLS).collect::<String>())
        .collect::<Vec<_>>()
        .join("\n")
}

/// The explorer's selected row — the one carrying its selection glyph.
fn selected_explorer_row(harness: &EditorTestHarness) -> Option<String> {
    explorer_text(harness)
        .lines()
        .find(|left| left.contains('▌'))
        .map(|left| left.to_string())
}

/// A project whose root holds a directory (`nested`, itself holding
/// `inner_file.txt`) and a loose file, under a per-test temp workdir
/// (CONTRIBUTING.md §4).
fn setup_project() -> (tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().unwrap();
    // Canonicalize: on macOS a tempdir is a symlink into `/private/var`, and
    // the editor stores the resolved path.
    let root = fs::canonicalize(temp.path()).unwrap();
    fs::create_dir(root.join("nested")).unwrap();
    fs::write(root.join("nested/inner_file.txt"), "inner\n").unwrap();
    fs::write(root.join("alpha.txt"), "alpha\n").unwrap();
    (temp, root)
}

/// Open the explorer, then a terminal to the right of it. Leaves the terminal
/// focused with the terminal buffer active, which is the state both tests
/// start their real work from.
///
/// No file is opened, so the explorer's selection sits where it starts: on
/// the project root, the top row, with `nested` directly below it.
fn harness_with_explorer_and_terminal(root: &PathBuf) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_working_dir(140, 36, root.clone()).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer_item("nested").unwrap();

    // The terminal takes focus, so from here on the active buffer is a
    // terminal even once focus moves elsewhere — the whole point of these
    // regressions. Losing the `Explorer` menu is the rendered signal that the
    // handover happened.
    harness
        .run_palette_command("Open Terminal to the Right")
        .unwrap();
    // Settle, not just observe: the shell's first prompt and the split's
    // first paints land asynchronously after the palette command returns, and
    // a focus handover racing them was enough to pull the keyboard back off
    // the explorer mid-test. Waiting for the screen to stop changing as well
    // as for the handover makes the tests' starting state deterministic.
    harness
        .wait_until_stable(|h| !explorer_has_focus(h))
        .unwrap();
    harness
}

/// With the explorer focused, step the selection onto the collapsed `nested`
/// directory and press Enter: the explorer must expand it.
///
/// Both steps are checked on the frame they happen in, with no waiting, so
/// the broken case fails outright instead of blocking:
///
/// * The Down step doubles as the focus check. The explorer only moves its
///   selection while it owns the keyboard, so a rendered selection on
///   `nested` proves focus is there — and Down is not a key either bug
///   affects (neither is "plain"), so it cannot mask what Enter does next.
/// * Expanding is synchronous, so `inner_file.txt` is on screen in the same
///   frame as the Enter. If the key had gone to the shell, `nested` would
///   still be collapsed.
///
/// Focus is deliberately not asserted *after* Enter: acting on a directory
/// row hands the keyboard back to the editor either way, which is existing
/// behaviour and says nothing about where the key went.
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

/// Enter in a focused file explorer acts on the explorer, even when the buffer
/// behind it is a live terminal. Focus is handed back with the mouse here.
///
/// Fails without the dispatch gate: Enter reaches the shell instead, the
/// explorer loses focus in the same frame, and `nested` stays collapsed.
#[test]
#[cfg(not(windows))] // spawns a PTY-backed shell
fn enter_acts_on_the_explorer_after_clicking_back_into_it() {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return;
    }
    let (_temp, root) = setup_project();
    let mut harness = harness_with_explorer_and_terminal(&root);

    // Hand focus back to the explorer with the mouse, clicking empty space
    // below the tree so the click itself activates nothing.
    harness.mouse_click(20, 12).unwrap();
    harness.wait_until(explorer_has_focus).unwrap();

    assert_enter_expands_nested(&mut harness);
}
