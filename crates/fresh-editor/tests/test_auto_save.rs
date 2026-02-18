mod common;

use common::harness::EditorTestHarness;
use fresh::config::Config;
use std::fs;
use std::time::Duration;

/// Helper to create an auto-save-enabled config with a short interval for testing
fn auto_save_config(interval_ms: u64) -> Config {
    let mut config = Config::default();
    config.editor.auto_save_enabled = true;
    config.editor.auto_save_interval_ms = interval_ms;
    config
}

#[test]
fn test_persistent_auto_save_basic() -> anyhow::Result<()> {
    let config = auto_save_config(2000);

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config)?;
    let temp_dir = harness.project_dir().unwrap();
    let file_path = temp_dir.join("test_auto_save.txt");
    fs::write(&file_path, "Initial content")?;

    harness.open_file(&file_path)?;
    harness.type_text("!")?;
    assert!(harness.editor().active_state().buffer.is_modified());

    // Advance past the interval
    harness.advance_time(Duration::from_millis(2100));

    let saved_count = harness.editor_mut().auto_save_persistent_buffers()?;
    assert_eq!(saved_count, 1, "Should have auto-saved 1 buffer");

    let content = fs::read_to_string(&file_path)?;
    assert!(
        content.contains("!"),
        "File should contain the modification. Content: {}",
        content
    );
    assert!(!harness.editor().active_state().buffer.is_modified());

    Ok(())
}

#[test]
fn test_persistent_auto_save_throttled_before_interval() -> anyhow::Result<()> {
    let config = auto_save_config(5000);

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config)?;
    let temp_dir = harness.project_dir().unwrap();
    let file_path = temp_dir.join("test_throttle.txt");
    fs::write(&file_path, "Original")?;

    harness.open_file(&file_path)?;
    harness.type_text("X")?;
    assert!(harness.editor().active_state().buffer.is_modified());

    // Advance less than the 5s interval
    harness.advance_time(Duration::from_millis(3000));

    let saved_count = harness.editor_mut().auto_save_persistent_buffers()?;
    assert_eq!(saved_count, 0, "Should NOT save before interval elapses");

    // File on disk should be unchanged
    let content = fs::read_to_string(&file_path)?;
    assert_eq!(
        content, "Original",
        "File should not have been modified yet"
    );
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should still be modified"
    );

    Ok(())
}

#[test]
fn test_persistent_auto_save_fires_after_interval() -> anyhow::Result<()> {
    let config = auto_save_config(5000);

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config)?;
    let temp_dir = harness.project_dir().unwrap();
    let file_path = temp_dir.join("test_fires_after.txt");
    fs::write(&file_path, "Original")?;

    harness.open_file(&file_path)?;
    harness.type_text("Y")?;

    // Not enough time yet
    harness.advance_time(Duration::from_millis(3000));
    let saved = harness.editor_mut().auto_save_persistent_buffers()?;
    assert_eq!(saved, 0, "Should not save at 3s");

    // Now advance past the interval (total 5.1s)
    harness.advance_time(Duration::from_millis(2100));
    let saved = harness.editor_mut().auto_save_persistent_buffers()?;
    assert_eq!(saved, 1, "Should save after 5.1s");

    let content = fs::read_to_string(&file_path)?;
    assert!(content.contains("Y"), "File should contain the edit");

    Ok(())
}

#[test]
fn test_auto_recovery_save_throttled_before_interval() -> anyhow::Result<()> {
    let mut config = Config::default();
    config.editor.auto_recovery_save_interval_ms = 5000;

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config)?;
    let temp_dir = harness.project_dir().unwrap();
    let file_path = temp_dir.join("test_recovery_throttle.txt");
    fs::write(&file_path, "Original")?;

    harness.open_file(&file_path)?;
    harness.type_text("Z")?;

    // Before interval: should not save
    harness.advance_time(Duration::from_millis(3000));
    let saved = harness.editor_mut().auto_recovery_save_dirty_buffers()?;
    assert_eq!(saved, 0, "Should NOT recovery-save before interval");

    // After interval: should save
    harness.advance_time(Duration::from_millis(2100));
    let saved = harness.editor_mut().auto_recovery_save_dirty_buffers()?;
    assert!(saved > 0, "Should recovery-save after interval elapses");

    Ok(())
}
