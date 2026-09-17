//! Regression: a workspace created in *this* session must be able to reach
//! the dock's `done` badge — the green `✓` that means "worked while you were
//! elsewhere, and you haven't looked yet".
//!
//! It could not. `sessionState` reads `workedSinceSeen`, which the
//! `terminal_output` handler arms only once the current output burst has
//! lasted `WORK_MIN_MS`; the burst clock starts on `lastOutputAt === null`.
//! Two of the session constructors left `lastOutputAt` absent instead of
//! `null`, and `undefined` matches neither that test nor the
//! `>= IDLE_AFTER_MS` one — so the clock never started, `workedSinceSeen`
//! never armed, and a row that had plainly been working fell from `working`
//! straight to the dim `·`. Every other badge (`*`, `●`, `·`) still
//! rendered, which is why it read as "quiet", not "broken".
//!
//! The timings here are the plugin's own constants (ACTIVATION_GRACE_MS
//! 1.5s, WORK_MIN_MS 1.5s, IDLE_AFTER_MS 5s) and the test spends real
//! wall-clock time, because the thing under test *is* the clock.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use std::path::PathBuf;
use std::time::{Duration, Instant};

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

/// A **non-git** project with the orchestrator plugin installed: the New
/// Workspace form unchecks "create a new git worktree" for a non-git path,
/// so the create is just "open a workspace here with a terminal" — no
/// `git worktree add` to slow the test down.
fn setup_project(name: &str) -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join(name);
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    (temp_dir, root)
}

fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("+ New") && h.editor().is_dock_focused())
        .unwrap();
}

fn pos_of(h: &EditorTestHarness, needle: &str) -> (u16, u16) {
    h.find_text_on_screen(needle)
        .unwrap_or_else(|| panic!("screen missing '{needle}':\n{}", h.screen_to_string()))
}

/// The dock line carrying `label`, badge glyph and all — cut at the dock's
/// right-hand wall, so the editor beside it cannot supply the glyph.
fn dock_row(h: &EditorTestHarness, label: &str) -> Option<String> {
    h.screen_to_string()
        .lines()
        .map(|l| l.split('│').next().unwrap_or(l).to_string())
        .find(|l| l.contains(label))
}

/// Let real time pass while the editor keeps servicing its PTYs — the dock's
/// state is a function of wall-clock gaps between terminal reads, so this
/// cannot be simulated by ticking alone.
fn run_for(h: &mut EditorTestHarness, secs: f64) {
    let until = Instant::now() + Duration::from_secs_f64(secs);
    while Instant::now() < until {
        std::thread::sleep(Duration::from_millis(100));
        h.tick_and_render().unwrap();
    }
}

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn a_workspace_created_this_session_reaches_the_done_badge() {
    if !pty_available() {
        eprintln!("Skipping done-badge test: PTY not available");
        return;
    }
    fresh::i18n::set_locale("en");
    let (_tmp, root) = setup_project("alphaproj");
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), root.clone())
            .unwrap();
    h.render().unwrap();
    let launch_label = h.editor().active_window().label.clone();
    open_dock(&mut h);

    // `[ + New ]` → the form → "Create Workspace" (create *and* visit).
    let (ncol, nrow) = pos_of(&h, "+ New");
    h.mouse_click(ncol + 1, nrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();
    let (ccol, crow) = pos_of(&h, "Create Workspace");
    h.mouse_click(ccol + 1, crow).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        !s.contains("New Workspace") && s.contains("Terminal 0")
    })
    .unwrap();
    let work_label = h.editor().active_window().label.clone();
    assert_ne!(work_label, launch_label, "the create should have moved us");

    // Give the agent something to do, starting after we have left: the
    // opening `sleep` covers both the walk back to the launch window and the
    // plugin's 1.5s activation grace, so every line of the burst lands while
    // another window is active and none of it is written off as an
    // activation redraw.
    h.type_text("sleep 3; for i in 1 2 3 4 5 6 7 8 9 10 11 12; do echo work $i; sleep 0.25; done")
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Back to where we were, before the burst starts: `Next Window` cycles
    // the active window, and the palette is reachable from the workspace's
    // terminal (e2e::orchestrator_dock leans on the same thing).
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Next Window").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Next Window"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.editor().active_window().label == launch_label)
        .unwrap();

    // 3s of waiting + 3s of output + the 5s idle window, plus margin.
    run_for(&mut h, 13.0);

    let row = dock_row(&h, &work_label).unwrap_or_else(|| {
        panic!(
            "dock lost the row for '{work_label}':\n{}",
            h.screen_to_string()
        )
    });
    assert!(
        row.contains('✓'),
        "a workspace that worked unseen and went quiet must read `done` (✓), \
         got:\n  {row}\nfull screen:\n{}",
        h.screen_to_string()
    );
}
