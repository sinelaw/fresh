mod common;

use common::harness::EditorTestHarness;
use fresh::config::Config;
use std::fs;
use std::time::Duration;

#[test]
fn test_persistent_auto_save() -> anyhow::Result<()> {
    // 1. Setup harness with auto-save enabled
    let mut config = Config::default();
    config.editor.auto_save_enabled = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config)?;
    let temp_dir = harness.project_dir().unwrap();
    let file_path = temp_dir.join("test_auto_save.txt");
    fs::write(&file_path, "Initial content")?;

    // 2. Open the file
    harness.open_file(&file_path)?;

    // 3. Modify the buffer
    harness.type_text("!")?;

    // Verify buffer is modified
    assert!(harness.editor().active_state().buffer.is_modified());

    // 4. Advance time
    harness.advance_time(Duration::from_millis(2100)); // Fixed 2s interval

    // 5. Trigger auto-save (simulating main loop)
    let saved_count = harness.editor_mut().auto_save_persistent_buffers()?;
    assert_eq!(saved_count, 1, "Should have auto-saved 1 buffer");

    // 6. Verify file on disk
    let content = fs::read_to_string(&file_path)?;
    assert!(
        content.contains("!"),
        "File should contain the modification. Content: {}",
        content
    );

    // 7. Verify buffer is no longer modified
    assert!(!harness.editor().active_state().buffer.is_modified());

    Ok(())
}
