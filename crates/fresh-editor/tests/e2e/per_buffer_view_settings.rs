//! E2E tests for per-buffer view-setting command-palette variants.
//!
//! Covers "Toggle Line Numbers (Current Buffer)" and "Toggle Line Wrap
//! (Current Buffer)": they must affect only the current buffer (not others)
//! and persist across a session restart. All assertions observe rendered
//! screen output.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Run a command-palette entry by fuzzy-typing its full name and pressing Enter.
fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Toggling line numbers for the current buffer via the command palette must
/// not change line-number visibility in other buffers.
#[test]
fn test_line_numbers_current_buffer_scopes_to_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(120, 24).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.txt"), "alpha\nbeta\n").unwrap();
    std::fs::write(dir.join("b.txt"), "delta\nepsilon\n").unwrap();

    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();

    // b.txt is active and shows the line-number gutter.
    harness.assert_screen_contains("│ delta");

    run_command(&mut harness, "Toggle Line Numbers (Current Buffer)");

    // b.txt now renders without a gutter, but its content is still shown.
    harness.assert_screen_not_contains("│ delta");
    harness.assert_screen_contains("delta");

    // a.txt is untouched: switching to it still shows the gutter.
    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("│ alpha");

    // Returning to b.txt keeps its per-buffer override.
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("│ delta");
    harness.assert_screen_contains("delta");
}

/// A per-buffer line-number override must survive a session restart.
#[test]
fn test_line_numbers_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, "alpha\nbeta\n").unwrap();

    // Session 1: turn line numbers off for this buffer, then save the workspace.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("│ alpha");

        harness.editor_mut().toggle_line_numbers_current_buffer();
        harness.render().unwrap();
        harness.assert_screen_not_contains("│ alpha");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore the workspace; line numbers stay off for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            120,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        harness.assert_screen_contains("alpha");
        harness.assert_screen_not_contains("│ alpha");
    }
}

/// Toggling line wrap for the current buffer must not change wrapping in other
/// buffers. With wrap on, the overflow tail of a long line is visible on a
/// wrapped row; with wrap off it is truncated off-screen.
#[test]
fn test_line_wrap_current_buffer_scopes_to_buffer() {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    let mut harness = EditorTestHarness::with_temp_project_and_config(60, 24, config).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.txt"), format!("{}TAILAAA\n", "A".repeat(80))).unwrap();
    std::fs::write(dir.join("b.txt"), format!("{}TAILBBB\n", "B".repeat(80))).unwrap();

    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();

    // b.txt active: wrap on, so the overflow tail wraps onto a visible row.
    harness.assert_screen_contains("TAILBBB");

    run_command(&mut harness, "Toggle Line Wrap (Current Buffer)");

    // b.txt now has wrap off: the tail is truncated off-screen.
    harness.assert_screen_not_contains("TAILBBB");

    // a.txt is untouched: it still wraps and shows its tail.
    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("TAILAAA");

    // b.txt keeps its per-buffer override.
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("TAILBBB");
}

/// A per-buffer line-wrap override must survive a session restart.
#[test]
fn test_line_wrap_current_buffer_persists_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, format!("{}TAILAAA\n", "A".repeat(80))).unwrap();

    let mk_config = || {
        let mut c = Config::default();
        c.editor.line_wrap = true;
        c
    };

    // Session 1: wrap is on (tail visible); turn it off for this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            60,
            24,
            mk_config(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("TAILAAA");

        harness.editor_mut().toggle_line_wrap_current_buffer();
        harness.render().unwrap();
        harness.assert_screen_not_contains("TAILAAA");

        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore; wrap stays off for this buffer (tail truncated).
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            60,
            24,
            mk_config(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should have been restored");
        harness.render().unwrap();

        harness.assert_screen_not_contains("TAILAAA");
    }
}
