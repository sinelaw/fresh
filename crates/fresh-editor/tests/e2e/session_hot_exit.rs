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
        harness
            .send_key(KeyCode::End, KeyModifiers::NONE)
            .unwrap();
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
        config.editor.persist_unnamed_buffers = true;

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
        config.editor.persist_unnamed_buffers = true;

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
