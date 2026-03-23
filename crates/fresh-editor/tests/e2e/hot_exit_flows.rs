// End-to-end tests for hot exit UX flows from the hot-exit-improvements-plan.
//
// These tests cover the behavioral flows described in the PRD:
//   Flow A: Launch with no arguments, no session (#1231)
//   Flow B: Launch with file arguments, no session (#1232)
//   Flow C: Session-scoped recovery isolation (#1233)
//   Flow D: CLI files in restored session (#1237)
//   Flow E: Tab order preserved across restart (#1234)
//   Flow F: File changed on disk since hot exit (mtime mismatch)

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

// =========================================================================
// Flow A: Launch with no arguments, no session
// Issue #1231 — workspace restore should NOT leave an extra unnamed buffer
// =========================================================================

/// After workspace restore with no CLI files, there should be no extra
/// "[No Name]" tab alongside the restored file tabs.
#[test]
fn test_flow_a_no_extra_unnamed_buffer_on_workspace_restore() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("main.rs");
    std::fs::write(&file1, "fn main() {}").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open a file, then shut down cleanly
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
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
        harness.render().unwrap();
        harness.assert_screen_contains("main.rs");

        harness.shutdown(true).unwrap();
    }

    // Session 2: restore with no CLI files
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Workspace should be restored");
        harness.render().unwrap();

        // The restored file should be present
        harness.assert_screen_contains("main.rs");

        // There should be NO extra unnamed buffer tab
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            !tab_bar.contains("[No Name]"),
            "Workspace restore with no CLI files should not create extra unnamed buffer.\nTab bar: {tab_bar}"
        );
    }
}

/// When workspace restore succeeds, the content of the active buffer
/// should be visible — not an empty unnamed buffer.
#[test]
fn test_flow_a_restored_buffer_content_visible() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("greeting.txt");
    std::fs::write(&file1, "Hello from workspace restore").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open file, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
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
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Session 2: verify content is shown
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.startup(true, &[]).unwrap();
        harness.assert_screen_contains("Hello from workspace restore");
    }
}

// =========================================================================
// Flow B: Launch with file arguments + workspace restore
// Issue #1232 — CLI files should be additive, not replace workspace
// =========================================================================

/// When launching with CLI file args and a prior workspace exists,
/// the workspace tabs should be restored AND the CLI file should be opened.
#[test]
fn test_flow_b_cli_files_additive_to_workspace() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_ws = project_dir.join("workspace_file.txt");
    let file_cli = project_dir.join("cli_file.txt");
    std::fs::write(&file_ws, "I was open in the workspace").unwrap();
    std::fs::write(&file_cli, "I was specified on the CLI").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open workspace_file, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file_ws).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("workspace_file.txt");

        harness.shutdown(true).unwrap();
    }

    // Session 2: restore workspace, but also pass cli_file on command line
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[file_cli.clone()]).unwrap();
        assert!(restored, "Workspace should be restored");
        harness.render().unwrap();

        // Both tabs should be visible: the workspace file AND the CLI file
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains("workspace_file.txt"),
            "Workspace tab should still be present after CLI file open.\nTab bar: {tab_bar}"
        );
        assert!(
            tab_bar.contains("cli_file.txt"),
            "CLI file should be added to tab bar.\nTab bar: {tab_bar}"
        );
    }
}

/// CLI file that is already in the workspace should not create a duplicate tab.
#[test]
fn test_flow_b_cli_file_already_in_workspace_no_duplicate() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("shared.txt");
    std::fs::write(&file1, "Content of shared file").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open the file, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
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
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Session 2: restore workspace AND pass same file as CLI arg
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.startup(true, &[file1.clone()]).unwrap();
        harness.render().unwrap();

        // There should be exactly one tab for shared.txt, no duplicates.
        // Duplicate tabs would show "shared.txt 1" and "shared.txt 2".
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains("shared.txt"),
            "shared.txt should be in tab bar.\nTab bar: {tab_bar}"
        );
        assert!(
            !tab_bar.contains("shared.txt 2"),
            "File already in workspace should not create a duplicate tab.\nTab bar: {tab_bar}"
        );
    }
}

/// Hot exit recovery of unsaved changes should still work when CLI files are specified.
#[test]
fn test_flow_b_hot_exit_recovery_with_cli_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_modified = project_dir.join("modified.txt");
    let file_cli = project_dir.join("extra.txt");
    std::fs::write(&file_modified, "original").unwrap();
    std::fs::write(&file_cli, "extra file content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: modify a file, shut down (hot exit saves the changes)
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file_modified).unwrap();
        harness.render().unwrap();

        // Modify the buffer
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" UNSAVED").unwrap();
        harness.render().unwrap();

        harness.shutdown(true).unwrap();
    }

    // Verify original file on disk is unchanged
    assert_eq!(std::fs::read_to_string(&file_modified).unwrap(), "original");

    // Session 2: restore with CLI file — unsaved changes should still be recovered
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.startup(true, &[file_cli.clone()]).unwrap();
        harness.render().unwrap();

        // The unsaved change should have been recovered
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains("modified.txt"),
            "Modified file should be restored from workspace.\nTab bar: {tab_bar}"
        );
        assert!(
            tab_bar.contains("extra.txt"),
            "CLI file should also be present.\nTab bar: {tab_bar}"
        );
    }
}

// =========================================================================
// Flow C: Session-scoped recovery isolation
// Issue #1233 — different sessions should have independent recovery state
// =========================================================================

/// Two separate sessions should not interfere with each other's hot exit state.
/// Session A's dirty buffers should not appear in session B, and vice versa.
///
/// Currently fails because recovery storage is global, not session-scoped (#1233).
/// Will pass after implementing session-scoped recovery (Phase 2 of the plan).
#[test]
fn test_flow_c_session_scoped_recovery_isolation() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_a = project_dir.join("session_a_file.txt");
    let file_b = project_dir.join("session_b_file.txt");
    std::fs::write(&file_a, "Session A content").unwrap();
    std::fs::write(&file_b, "Session B content").unwrap();

    // Use separate dir contexts to simulate separate sessions
    let dir_context_a = DirectoryContext::for_testing(&temp_dir.path().join("session_a"));
    let dir_context_b = DirectoryContext::for_testing(&temp_dir.path().join("session_b"));

    // Session A: open and modify a file, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context_a.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness
            .editor_mut()
            .set_session_name(Some("session_a".into()));
        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" MODIFIED_A").unwrap();
        harness.render().unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session B: open a different file, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context_b.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness
            .editor_mut()
            .set_session_name(Some("session_b".into()));
        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" MODIFIED_B").unwrap();
        harness.render().unwrap();

        harness.shutdown(true).unwrap();
    }

    // Restore session A: should see A's file, NOT B's
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context_a.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness
            .editor_mut()
            .set_session_name(Some("session_a".into()));
        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session A workspace should be restored");
        harness.render().unwrap();

        harness.assert_screen_contains("MODIFIED_A");
        harness.assert_screen_not_contains("MODIFIED_B");
        harness.assert_screen_not_contains("session_b_file.txt");
    }

    // Restore session B: should see B's file, NOT A's
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context_b.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness
            .editor_mut()
            .set_session_name(Some("session_b".into()));
        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session B workspace should be restored");
        harness.render().unwrap();

        harness.assert_screen_contains("MODIFIED_B");
        harness.assert_screen_not_contains("MODIFIED_A");
        harness.assert_screen_not_contains("session_a_file.txt");
    }
}

// =========================================================================
// Flow D: CLI files in a restored session
// Issue #1237 — `fresh -a session file.txt` should add to restored session
// =========================================================================

/// When a named session is restored with CLI file args, the session workspace
/// tabs should remain and the CLI file should be added and focused.
#[test]
fn test_flow_d_cli_files_added_to_restored_session() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_session = project_dir.join("session_file.txt");
    let file_cli = project_dir.join("cli_opened.txt");
    std::fs::write(&file_session, "Part of the session").unwrap();
    std::fs::write(&file_cli, "Opened via CLI").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open file in session, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file_session).unwrap();
        harness.render().unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session 2: restore session but also open a CLI file (simulates `fresh -a mysession file.txt`)
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);
        let restored = harness.startup(true, &[file_cli.clone()]).unwrap();
        assert!(restored, "Session workspace should be restored");
        harness.render().unwrap();

        // Both tabs should exist
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains("session_file.txt"),
            "Session file should be present after restore.\nTab bar: {tab_bar}"
        );
        assert!(
            tab_bar.contains("cli_opened.txt"),
            "CLI file should be opened in the session.\nTab bar: {tab_bar}"
        );

        // The CLI file content should be visible (it should be the active buffer)
        harness.assert_screen_contains("Opened via CLI");
    }
}

// =========================================================================
// Flow E: Tab order preserved across restart
// Issue #1234 — tabs should appear in the same order after restore
// =========================================================================

/// Open multiple files, verify their order in the tab bar survives a
/// shutdown → restore cycle.
#[test]
fn test_flow_e_tab_order_preserved_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Create files with alphabetically-unrelated names so we can detect ordering
    let file_alpha = project_dir.join("alpha.txt");
    let file_beta = project_dir.join("beta.txt");
    let file_gamma = project_dir.join("gamma.txt");
    std::fs::write(&file_alpha, "A").unwrap();
    std::fs::write(&file_beta, "B").unwrap();
    std::fs::write(&file_gamma, "C").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open files in a specific order: beta, gamma, alpha
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);

        harness.open_file(&file_beta).unwrap();
        harness.open_file(&file_gamma).unwrap();
        harness.open_file(&file_alpha).unwrap();
        harness.render().unwrap();

        // Verify initial order
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        let pos_beta = tab_bar
            .find("beta.txt")
            .expect("beta.txt should be in tab bar");
        let pos_gamma = tab_bar
            .find("gamma.txt")
            .expect("gamma.txt should be in tab bar");
        let pos_alpha = tab_bar
            .find("alpha.txt")
            .expect("alpha.txt should be in tab bar");
        assert!(
            pos_beta < pos_gamma && pos_gamma < pos_alpha,
            "Initial order should be beta, gamma, alpha.\nTab bar: {tab_bar}"
        );

        harness.shutdown(true).unwrap();
    }

    // Session 2: verify the same order is restored
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            120,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Workspace should be restored");
        harness.render().unwrap();

        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        let pos_beta = tab_bar
            .find("beta.txt")
            .expect("beta.txt should be in restored tab bar");
        let pos_gamma = tab_bar
            .find("gamma.txt")
            .expect("gamma.txt should be in restored tab bar");
        let pos_alpha = tab_bar
            .find("alpha.txt")
            .expect("alpha.txt should be in restored tab bar");
        assert!(
            pos_beta < pos_gamma && pos_gamma < pos_alpha,
            "Restored tab order should match original: beta, gamma, alpha.\nTab bar: {tab_bar}"
        );
    }
}

/// Active tab should be preserved across restart.
#[test]
fn test_flow_e_active_tab_preserved_across_restart() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("first.txt");
    let file2 = project_dir.join("second.txt");
    std::fs::write(&file1, "First file content").unwrap();
    std::fs::write(&file2, "Second file content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open two files, switch to the first one, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
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

        // Switch back to first tab (Ctrl+Shift+Tab or similar)
        // Use direct open_file to focus it
        harness.open_file(&file1).unwrap();
        harness.render().unwrap();

        // Verify first file is active (its content should be visible)
        harness.assert_screen_contains("First file content");

        harness.shutdown(true).unwrap();
    }

    // Session 2: the first file should still be active
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Workspace should be restored");
        harness.render().unwrap();

        // The active buffer should be the first file
        harness.assert_screen_contains("First file content");
    }
}

// =========================================================================
// Flow F: File changed on disk since hot exit (mtime mismatch)
// =========================================================================

/// When a file is modified on disk between shutdown and restore, the recovery
/// should be skipped and the user should be warned (not silently discarded).
///
/// Currently fails because recovery is applied regardless of mtime changes,
/// silently overwriting external edits. Will pass after implementing mtime
/// check with user warning (Phase 4, Task 4.2 of the plan).
#[test]
fn test_flow_f_mtime_mismatch_skips_recovery() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("changing.txt");
    std::fs::write(&file1, "original on disk").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open file, make unsaved edits, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
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
        harness.render().unwrap();

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" UNSAVED_EDIT").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("UNSAVED_EDIT");

        harness.shutdown(true).unwrap();
    }

    // Modify the file on disk AFTER shutdown (simulate external edit)
    // Sleep >1s to ensure mtime (stored as whole seconds) changes
    std::thread::sleep(std::time::Duration::from_millis(1100));
    std::fs::write(&file1, "externally modified on disk").unwrap();

    // Session 2: restore — the unsaved edit should NOT be applied
    // because the file changed on disk
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.startup(true, &[]).unwrap();
        harness.render().unwrap();

        // The buffer should show the new on-disk content, not the stale recovery
        harness.assert_screen_contains("externally modified on disk");
        harness.assert_screen_not_contains("UNSAVED_EDIT");
    }
}

// =========================================================================
// Additional edge case tests
// =========================================================================

/// Multiple unnamed buffers should all be preserved across hot exit.
#[test]
fn test_multiple_unnamed_buffers_preserved() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: create two unnamed buffers with content
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.editor_mut().set_session_mode(true);

        // First unnamed buffer
        harness.new_buffer().unwrap();
        harness.type_text("Scratch buffer one").unwrap();
        harness.render().unwrap();

        // Second unnamed buffer
        harness.new_buffer().unwrap();
        harness.type_text("Scratch buffer two").unwrap();
        harness.render().unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session 2: both unnamed buffers should be restored
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Workspace should be restored");
        harness.render().unwrap();

        // At least one unnamed buffer should be visible
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains("[No Name]"),
            "Unnamed buffers should be restored.\nTab bar: {tab_bar}"
        );

        // Check one of the buffer contents is visible
        let screen = harness.screen_to_string();
        let has_one = screen.contains("Scratch buffer one");
        let has_two = screen.contains("Scratch buffer two");
        assert!(
            has_one || has_two,
            "At least one unnamed buffer's content should be visible.\nScreen:\n{screen}"
        );
    }
}

/// Hot exit disabled: modified buffers should NOT be recovered.
#[test]
fn test_hot_exit_disabled_no_recovery() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("no_recover.txt");
    std::fs::write(&file1, "original").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: modify file with hot_exit DISABLED, shut down
    {
        let mut config = Config::default();
        config.editor.hot_exit = false;
        // hot_exit = false also disables unnamed buffer persistence

        let mut harness = EditorTestHarness::create(
            100,
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
        harness.render().unwrap();

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" SHOULD_NOT_RECOVER").unwrap();
        harness.render().unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session 2: restore — with hot_exit off, no recovery data should exist
    {
        let mut config = Config::default();
        config.editor.hot_exit = false;
        // hot_exit = false also disables unnamed buffer persistence

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.startup(true, &[]).unwrap();
        harness.render().unwrap();

        // The unsaved edit should NOT have been recovered
        harness.assert_screen_not_contains("SHOULD_NOT_RECOVER");
    }
}

// =========================================================================
// Undo after hot exit recovery should not clear modified flag
// =========================================================================

/// After hot exit recovery, the buffer's modified flag must remain set even
/// after pressing undo. Previously, the event log's saved_at_index was left
/// at 0 after recovery, causing undo to incorrectly clear the modified flag.
#[test]
fn test_undo_after_hot_exit_recovery_keeps_modified_flag() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("test.txt");
    std::fs::write(&file1, "original content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open file, make an edit, then hot-exit
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
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
        harness.render().unwrap();

        // Type some text to create a modification
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness.type_text(" EDITED").unwrap();
        harness.render().unwrap();

        // Tab bar should show modified indicator
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains('*'),
            "Tab should show modified indicator after editing.\nTab bar: {tab_bar}"
        );

        // Shutdown with hot exit (preserves unsaved changes)
        harness.shutdown(true).unwrap();
    }

    // Session 2: restore workspace, then press undo
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Workspace should be restored");
        harness.render().unwrap();

        // Tab bar should show modified indicator after recovery
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains('*'),
            "Tab should show modified indicator after hot exit recovery.\nTab bar: {tab_bar}"
        );

        // Verify recovered content is present
        harness.assert_screen_contains("EDITED");

        // Press undo — this should NOT clear the modified indicator,
        // because the recovered content still differs from the on-disk file
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        // The tab bar should STILL show the modified indicator
        let tab_bar = harness.screen_row_text(layout::TAB_BAR_ROW as u16);
        assert!(
            tab_bar.contains('*'),
            "Tab should still show modified indicator after undo following hot exit recovery.\nTab bar: {tab_bar}"
        );

        // The recovered content should still be visible (undo had nothing to undo)
        harness.assert_screen_contains("EDITED");
    }
}
