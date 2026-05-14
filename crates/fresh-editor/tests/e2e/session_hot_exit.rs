// End-to-end tests for hot exit / recovery in session mode (fresh -a)
//
// Issue #1233: Named sessions don't preserve hot exit state because
// EditorServer::run() exits without calling end_recovery_session()
// or save_workspace().
//
// These tests verify that the session shutdown path (which mirrors
// EditorServer::run's shutdown) properly preserves state.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

/// Test that hot exit state is preserved when using session mode.
///
/// This test verifies that `fresh -a mysession` preserves unsaved changes
/// across server restarts via end_recovery_session() and save_workspace().
#[test]
fn test_session_hot_exit_preserves_unsaved_changes() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("hello.txt");
    std::fs::write(&file1, "original content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: open file, modify it, then shutdown like the server does
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        // Simulate session mode
        harness.editor_mut().set_session_mode(true);

        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("original content");

        // Modify the file
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" EDITED").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("EDITED");

        // Clean shutdown — mirrors what EditorServer::run() now does
        harness.shutdown(true).unwrap();
    }

    // Verify file on disk is unchanged
    let on_disk = std::fs::read_to_string(&file1).unwrap();
    assert_eq!(
        on_disk, "original content",
        "File on disk should be unchanged"
    );

    // Second session: restore — should find hot exit state
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session should have been restored");
        harness.assert_screen_contains("EDITED");
    }
}

/// Test that unnamed buffer content is preserved across session server restarts.
#[test]
fn test_session_unnamed_buffer_preserved_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: create unnamed buffer with content
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);

        harness.new_buffer().unwrap();
        harness.type_text("scratch notes here").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("scratch notes here");

        // Clean shutdown — mirrors what EditorServer::run() now does
        harness.shutdown(true).unwrap();
    }

    // Second session: restore and verify unnamed buffer content
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session should have been restored");
        harness.assert_screen_contains("scratch notes here");
    }
}

/// Test that the workspace (open tabs, active tab) is preserved across session restarts.
#[test]
fn test_session_workspace_preserved_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("a.txt");
    let file2 = project_dir.join("b.txt");
    std::fs::write(&file1, "Content of file A").unwrap();
    std::fs::write(&file2, "Content of file B").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: open two files
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);

        harness.open_file(&file1).unwrap();
        harness.open_file(&file2).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("Content of file B");

        // Clean shutdown — mirrors what EditorServer::run() now does
        harness.shutdown(true).unwrap();
    }

    // Second session: restore and verify tabs are back
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session should have been restored");
        harness.assert_screen_contains("Content of file B");
    }
}

/// Regression: session restore must refresh `primary_cursor_line_number`.
///
/// The restore path writes `view_state.cursors.primary().position` directly
/// (no `Event::MoveCursor`), so the per-buffer `primary_cursor_line_number`
/// cache stays at `EditorState::new`'s default `Absolute(0)`. Status bar
/// and plugin-side `getCursorLine` both read that cache — without this fix,
/// a Git Blame invoked right after restore reads 0 and lands on Ln 1 even
/// though the cursor's byte position is on line 100.
#[test]
fn test_session_restore_refreshes_cursor_line_cache() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Enough lines that the target line is clearly distinct from line 1.
    let file = project_dir.join("big.txt");
    let mut content = String::new();
    for i in 1..=200 {
        content.push_str(&format!("Line {i}\n"));
    }
    std::fs::write(&file, &content).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: open the file, navigate to line 100, shutdown to save.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file).unwrap();
        harness
            .wait_until(|h| h.get_buffer_content().unwrap().contains("Line 200"))
            .unwrap();

        // Navigate to line 100 via Ctrl-G to exercise the real keybinding path.
        harness
            .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
            .unwrap();
        harness.wait_for_prompt().unwrap();
        harness.type_text("100").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("Ln 100"))
            .unwrap();

        harness.shutdown(true).unwrap();
    }

    // Second session: restore and assert status bar shows Ln 100, not Ln 1.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session should have been restored");

        let screen = harness.screen_to_string();
        assert!(
            screen.contains("Ln 100"),
            "Status bar should report Ln 100 after restore, not the stale \
             default. Screen:\n{screen}",
        );
    }
}
