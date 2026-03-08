// End-to-end tests for unnamed buffer persistence across sessions (#1148)

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

/// Test that an unnamed buffer with content survives save/restore cycle
#[test]
fn test_unnamed_buffer_survives_save_restore_cycle() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: create unnamed buffer, type content, save workspace + flush
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

        harness.new_buffer().unwrap();
        harness.type_text("scratch notes here").unwrap();
        harness.render().unwrap();

        // Verify content appears on screen
        harness.assert_screen_contains("scratch notes here");

        // End recovery session (flushes dirty buffers + assigns recovery IDs),
        // then save workspace (captures those IDs).
        harness.editor_mut().end_recovery_session().unwrap();
        harness.editor_mut().save_workspace().unwrap();
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

        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should have been restored");

        // The unnamed buffer content should be visible on screen
        harness.render().unwrap();
        harness.assert_screen_contains("scratch notes here");
    }
}

/// Test that unnamed buffers and file-backed buffers are restored together
#[test]
fn test_unnamed_and_file_buffers_restored_together() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("a.txt");
    std::fs::write(&file1, "file A content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: open file + create unnamed buffer
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

        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("file A content");

        harness.new_buffer().unwrap();
        harness.type_text("unnamed content").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("unnamed content");

        harness.editor_mut().save_workspace().unwrap();
        harness.editor_mut().end_recovery_session().unwrap();
    }

    // Second session: restore both
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

        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should have been restored");

        // Switch to the file buffer and verify it's visible
        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("file A content");
    }
}

/// Test that quit with only unnamed modified buffers skips the discard prompt
#[test]
fn test_quit_unnamed_only_skips_prompt() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let mut config = Config::default();
    config.editor.persist_unnamed_buffers = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_dir)
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Create unnamed buffer with content
    harness.new_buffer().unwrap();
    harness.type_text("some scratch text").unwrap();
    harness.render().unwrap();

    // Send Ctrl+Q to quit - should quit immediately without prompt
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // No "unsaved changes" prompt should appear; editor should quit
    harness.assert_screen_not_contains("unsaved changes");
    assert!(
        harness.should_quit(),
        "Editor should quit without prompt when only unnamed buffers are modified"
    );
}

/// Test that persist_unnamed_buffers=false preserves old behavior
#[test]
fn test_persist_disabled_shows_quit_prompt() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let mut config = Config::default();
    config.editor.persist_unnamed_buffers = false;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_dir)
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Create unnamed buffer with content
    harness.new_buffer().unwrap();
    harness.type_text("some scratch text").unwrap();
    harness.render().unwrap();

    // Send Ctrl+Q to quit - should show prompt
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should see the discard prompt on screen
    harness.assert_screen_contains("unsaved changes");
    assert!(
        !harness.should_quit(),
        "Editor should show prompt when persist_unnamed_buffers is disabled"
    );
}

/// Test that quit with mixed modified buffers still prompts when hot_exit is off
#[test]
fn test_quit_mixed_modified_still_prompts() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("test.txt");
    std::fs::write(&file1, "original").unwrap();

    let mut config = Config::default();
    config.editor.persist_unnamed_buffers = true;
    config.editor.hot_exit = false;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_dir)
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Open and modify a file-backed buffer
    harness.open_file(&file1).unwrap();
    harness.type_text("edit").unwrap();
    harness.render().unwrap();

    // Also create an unnamed buffer
    harness.new_buffer().unwrap();
    harness.type_text("scratch").unwrap();
    harness.render().unwrap();

    // Send Ctrl+Q to quit - should show prompt because of file-backed modification
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should see the discard prompt
    harness.assert_screen_contains("unsaved changes");
    assert!(
        !harness.should_quit(),
        "Editor should show prompt when file-backed buffers are modified"
    );
}

/// Test that hot exit preserves unsaved changes in file-backed buffers across sessions
#[test]
fn test_hot_exit_restores_unsaved_file_changes() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("hello.txt");
    std::fs::write(&file1, "original content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: open file, modify it, then flush+save
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

        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("original content");

        // Move to end and add text
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" EDITED").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("EDITED");

        // Save workspace and end recovery session (simulating clean exit)
        harness.editor_mut().save_workspace().unwrap();
        harness.editor_mut().end_recovery_session().unwrap();
    }

    // Verify file on disk is unchanged
    let on_disk = std::fs::read_to_string(&file1).unwrap();
    assert_eq!(
        on_disk, "original content",
        "File on disk should be unchanged"
    );

    // Second session: restore and verify unsaved changes are back
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

        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should have been restored");

        // The unsaved changes should be visible on screen
        harness.render().unwrap();
        harness.assert_screen_contains("EDITED");
    }
}

/// Test that hot exit restores unsaved changes when file is opened via CLI
/// (i.e. without workspace restore - simulates `fresh file.txt` workflow)
#[test]
fn test_hot_exit_restores_without_workspace() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("hello.txt");
    std::fs::write(&file1, "original content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // First session: open file, modify it, end recovery (simulating quit without workspace save)
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

        harness.open_file(&file1).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("original content");

        // Move to end and add text
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" EDITED").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("EDITED");

        // Only end recovery session (no workspace save - simulates CLI file workflow)
        harness.editor_mut().end_recovery_session().unwrap();
    }

    // Verify file on disk is unchanged
    let on_disk = std::fs::read_to_string(&file1).unwrap();
    assert_eq!(
        on_disk, "original content",
        "File on disk should be unchanged"
    );

    // Second session: open file directly (as CLI would), then apply hot exit recovery
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

        // Open file directly (as CLI would, without workspace restore)
        harness.open_file(&file1).unwrap();

        // Apply hot exit recovery for open buffers
        let recovered = harness
            .editor_mut()
            .apply_hot_exit_recovery()
            .unwrap();
        assert!(recovered > 0, "Should have recovered at least one buffer");

        // The unsaved changes should be visible on screen
        harness.render().unwrap();
        harness.assert_screen_contains("EDITED");
    }
}

/// Test that hot exit quit skips the prompt for modified file-backed buffers
#[test]
fn test_hot_exit_skips_quit_prompt() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("test.txt");
    std::fs::write(&file1, "original").unwrap();

    let mut config = Config::default();
    config.editor.hot_exit = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_dir)
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Open and modify the file
    harness.open_file(&file1).unwrap();
    harness.type_text("changes").unwrap();
    harness.render().unwrap();

    // Send Ctrl+Q - should quit without prompt (hot exit backs up changes)
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("unsaved changes");
    assert!(
        harness.should_quit(),
        "Editor should quit without prompt when hot_exit is enabled"
    );
}
