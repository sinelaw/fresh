//! Regression test for issue #2031: missing next/prev window
//! commands.
//!
//! Fresh has an internal "window" concept (`fresh_core::WindowId`,
//! one per project / working directory — see
//! `crates/fresh-editor/src/app/mod.rs:587-594`), but until this
//! change the only way to move between windows was the heavyweight
//! `Switch Project` command (which opens a fuzzy picker prompt).
//! Users asked for a one-keystroke cycle analogous to
//! `Next Buffer` / `Prev Buffer` for tabs and
//! `Next Split` / `Prev Split` for split panes.
//!
//! New commands:
//!   - `Action::NextWindow`  — cycle to the next window (wrap-around)
//!   - `Action::PrevWindow`  — cycle to the previous window
//!
//! Observability per CONTRIBUTING §Testing: each test opens a
//! distinct file in each window (`alpha.txt` in window 1,
//! `beta.txt` in window 2). The active window's file's content is
//! what's rendered on screen, so cycling between windows flips the
//! visible text between `ALPHA_CONTENT` and `BETA_CONTENT`.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Helper: set up two windows, each viewing a distinct file. Leaves
/// the harness with window 2 active (it's the one we just created
/// and dove into). The two files' contents serve as the
/// screen-observable signal for which window is active.
///
/// Returns the `TempDir` so the caller keeps it alive for the
/// duration of the test — dropping it would delete window 2's root
/// out from under the harness.
fn two_windows_with_distinct_files(harness: &mut EditorTestHarness) -> tempfile::TempDir {
    let first_root = harness.project_dir().unwrap();
    std::fs::write(first_root.join("alpha.txt"), "ALPHA_CONTENT\n").unwrap();
    harness
        .editor_mut()
        .open_file(&first_root.join("alpha.txt"))
        .unwrap();
    harness.render().unwrap();

    let extra_temp = tempfile::tempdir().unwrap();
    let second_id = harness
        .editor_mut()
        .create_window_at(extra_temp.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second_id);
    std::fs::write(extra_temp.path().join("beta.txt"), "BETA_CONTENT\n").unwrap();
    harness
        .editor_mut()
        .open_file(&extra_temp.path().join("beta.txt"))
        .unwrap();
    harness.render().unwrap();
    extra_temp
}

/// Invoke a command from the command palette by name + Enter.
fn run_palette_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(command).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// `Next Window` cycles forward through the open windows. Starting
/// on window 2 (beta.txt), `Next Window` must wrap around to window
/// 1 — observed as the screen flipping from `BETA_CONTENT` to
/// `ALPHA_CONTENT`.
#[test]
fn next_window_command_cycles_to_the_other_open_window() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let _extra_temp = two_windows_with_distinct_files(&mut harness);

    // Precondition: window 2 is active, so beta.txt is on screen.
    harness.wait_for_screen_contains("BETA_CONTENT").unwrap();

    run_palette_command(&mut harness, "Next Window");

    // Cycle wraps to window 1 — alpha.txt must now be visible.
    harness.wait_for_screen_contains("ALPHA_CONTENT").unwrap();
}

/// `Previous Window` cycles backward. From window 2 (beta.txt),
/// `Previous Window` lands on window 1 (alpha.txt).
#[test]
fn prev_window_command_cycles_backward() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let _extra_temp = two_windows_with_distinct_files(&mut harness);

    harness.wait_for_screen_contains("BETA_CONTENT").unwrap();

    run_palette_command(&mut harness, "Previous Window");

    harness.wait_for_screen_contains("ALPHA_CONTENT").unwrap();
}

/// Running `Next Window` twice in a row must return to the starting
/// window — confirms the cycle wraps cleanly with exactly two
/// windows open.
#[test]
fn two_next_window_calls_return_to_the_starting_window() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let _extra_temp = two_windows_with_distinct_files(&mut harness);

    harness.wait_for_screen_contains("BETA_CONTENT").unwrap();

    run_palette_command(&mut harness, "Next Window");
    harness.wait_for_screen_contains("ALPHA_CONTENT").unwrap();

    run_palette_command(&mut harness, "Next Window");
    harness.wait_for_screen_contains("BETA_CONTENT").unwrap();
}
