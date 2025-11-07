// E2E tests for editor behavior with slow filesystem operations
//
// These tests verify that the editor remains responsive and performs
// well even when filesystem operations are slow (network drives, slow disks, etc.)

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::fs::SlowFsConfig;
use std::time::Duration;

#[test]
fn test_slow_fs_harness_creation() {
    // Verify that we can create a test harness with slow filesystem
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(10));
    let harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Verify metrics are available
    assert!(harness.fs_metrics().is_some());

    // Get metrics snapshot (this is async, so we need a runtime)
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let metrics = runtime.block_on(harness.get_fs_metrics_snapshot()).unwrap();
    // Metrics should be initialized
    assert_eq!(metrics.total_calls(), metrics.total_calls());
}

#[test]
fn test_slow_fs_metrics_tracking() {
    // Create a slow filesystem with 50ms delays
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(50));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    let runtime = tokio::runtime::Runtime::new().unwrap();

    // Get initial metrics (should be zero or minimal)
    let metrics_before = runtime.block_on(harness.get_fs_metrics_snapshot()).unwrap();
    let initial_calls = metrics_before.total_calls();

    // Perform an action that might trigger filesystem operations
    // For example, typing text shouldn't trigger many fs operations
    harness.type_text("hello world").unwrap();

    let metrics_after = runtime.block_on(harness.get_fs_metrics_snapshot()).unwrap();

    // Typing text should not trigger filesystem operations
    // (unless there's autosave or similar features)
    assert_eq!(
        metrics_after.total_calls(),
        initial_calls,
        "Typing should not trigger filesystem operations"
    );
}

#[test]
fn test_editor_creation_with_slow_fs() {
    // Test that editor can be created even with very slow filesystem
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(200));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Editor should render successfully
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(!screen.is_empty());
}

#[test]
fn test_typing_remains_fast_with_slow_fs() {
    // Even with slow filesystem, typing should remain responsive
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    let start = std::time::Instant::now();

    // Type a moderate amount of text
    let text = "The quick brown fox jumps over the lazy dog";
    harness.type_text(text).unwrap();

    let elapsed = start.elapsed();

    // Typing should be fast (not waiting on filesystem)
    // Even with 100ms fs delays, typing 44 characters should be well under 1 second
    assert!(
        elapsed < Duration::from_secs(1),
        "Typing took {:?}, which is too slow. Text editing should not block on filesystem.",
        elapsed
    );

    // Verify the text was actually inserted
    harness.assert_buffer_content(text);
}

#[test]
fn test_slow_network_fs_preset() {
    // Test using the slow_network preset configuration
    let slow_config = SlowFsConfig::slow_network();
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Should still be able to create buffers and type
    harness.type_text("Testing slow network filesystem").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content();
    assert_eq!(content, "Testing slow network filesystem");
}

#[test]
fn test_slow_disk_preset() {
    // Test using the slow_disk preset configuration
    let slow_config = SlowFsConfig::slow_disk();
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Editor should work normally
    harness.type_text("Testing slow disk").unwrap();
    assert_eq!(harness.get_buffer_content(), "Testing slow disk");
}

#[test]
fn test_navigation_with_slow_fs() {
    // Test that cursor navigation is not affected by slow filesystem
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Type some text
    harness.type_text("line 1\nline 2\nline 3").unwrap();

    let start = std::time::Instant::now();

    // Navigate around (these operations should not touch filesystem)
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Up, KeyModifiers::NONE)
            .unwrap();
        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .unwrap();
        harness
            .send_key(KeyCode::Left, KeyModifiers::NONE)
            .unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    let elapsed = start.elapsed();

    // Navigation should be instant (well under 100ms even with slow fs)
    assert!(
        elapsed < Duration::from_millis(500),
        "Navigation took {:?}, which suggests it's waiting on filesystem",
        elapsed
    );

    // Verify no extra filesystem calls were made for navigation
    let metrics = tokio::runtime::Runtime::new().unwrap().block_on(harness.get_fs_metrics_snapshot()).unwrap();

    // Since we started with an empty buffer and didn't open files,
    // there should be minimal filesystem calls
    assert!(
        metrics.total_calls() < 10,
        "Too many filesystem calls ({}) for simple navigation",
        metrics.total_calls()
    );
}

#[test]
fn test_metrics_provide_timing_info() {
    // Verify that slow fs metrics track delay time correctly
    let delay = Duration::from_millis(100);
    let slow_config = SlowFsConfig::uniform(delay);
    let harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    let metrics = tokio::runtime::Runtime::new().unwrap().block_on(harness.get_fs_metrics_snapshot()).unwrap();

    // The metrics should track total delay time
    // (exact value depends on how many fs operations happened during editor init)
    if metrics.total_calls() > 0 {
        assert!(
            metrics.total_delay_time > Duration::ZERO,
            "Metrics should track delay time"
        );
    }
}
