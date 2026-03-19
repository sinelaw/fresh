// Regression test: saving an unnamed buffer to a path outside the workspace root
// and then restoring the session should show a tab for the external file.
//
// Bug: serialize_split_view_state only emits SerializedTabRef::File for paths
// inside working_dir (via strip_prefix). External files are stored in
// `external_files` but not in `open_tabs`, so on restore the file is opened
// and focused but has no tab in the tab bar.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

#[test]
fn test_save_as_external_file_has_tab_after_restore() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    let external_dir = temp_dir.path().join("external");
    std::fs::create_dir(&project_dir).unwrap();
    std::fs::create_dir(&external_dir).unwrap();

    let file_a = project_dir.join("a.txt");
    let file_b = project_dir.join("b.txt");
    std::fs::write(&file_a, "file A content").unwrap();
    std::fs::write(&file_b, "file B content").unwrap();

    let external_path = external_dir.join("scratch.txt");

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Session 1: open files, create unnamed buffer, type content, save-as to
    // external path, then shutdown with workspace save.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        harness.open_file(&file_a).unwrap();
        harness.open_file(&file_b).unwrap();

        // Create unnamed buffer with content
        harness.new_buffer().unwrap();
        harness.type_text("scratch notes").unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("scratch notes");

        // Ctrl+S on unnamed buffer triggers save-as prompt
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("Save as:");

        // Type the external path and press Enter
        harness.type_text(&external_path.to_string_lossy()).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // The tab should now show "scratch.txt"
        harness.assert_screen_contains("scratch.txt");

        // Verify the file was written
        assert_eq!(
            std::fs::read_to_string(&external_path).unwrap(),
            "scratch notes"
        );

        harness.shutdown(true).unwrap();
    }

    // Session 2: restore and verify the external file has a tab
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            100,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "Session should have been restored");

        // The external file should be visible in the content area
        harness.assert_screen_contains("scratch notes");

        // The tab bar is on row 1 (0-indexed, row 0 is the menu bar).
        // Check that "scratch.txt" appears in the tab bar row, not just
        // in the status bar at the bottom.
        let tab_bar_row = harness.screen_row_text(1);
        eprintln!("Tab bar row: {}", tab_bar_row);
        let screen = harness.screen_to_string();
        eprintln!("Full screen:\n{}", screen);

        // BUG: the tab for "scratch.txt" is missing from the tab bar even
        // though the file is open and focused.
        assert!(
            tab_bar_row.contains("scratch.txt"),
            "Tab for external file 'scratch.txt' should be in the tab bar row. Tab bar: '{}'\nFull screen:\n{}",
            tab_bar_row,
            screen
        );

        // Verify all three tabs are present in the tab bar
        assert!(
            tab_bar_row.contains("a.txt") && tab_bar_row.contains("b.txt"),
            "Other file tabs should also be present in tab bar. Tab bar: '{}'",
            tab_bar_row,
        );
    }
}
