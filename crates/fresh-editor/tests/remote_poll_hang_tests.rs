//! Tests for poll_file_changes blocking the main event loop
//!
//! These tests verify that the editor's file change polling does not block
//! the main event loop when filesystem operations are slow or hanging.
//! This reproduces the bug where a remote filesystem going down causes
//! the editor to deadlock because poll_file_changes calls metadata()
//! synchronously from the event loop.
//!
//! Current status: These tests FAIL (hang forever) because poll_file_changes
//! calls filesystem.metadata() synchronously with no timeout or async handling.
//! They will pass once polling is made non-blocking for slow/remote filesystems.

mod common;

use common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::services::fs::{SlowFileSystem, SlowFsConfig};
use std::sync::Arc;
use std::time::Duration;

/// Test: poll_file_changes must not block the event loop when metadata() hangs.
///
/// Scenario: A file is open in the editor. The filesystem's metadata() call
/// takes a very long time (simulating a remote server that stopped responding).
/// When poll_file_changes fires, it should not block the event loop.
///
/// BUG: Currently hangs because poll_file_changes calls filesystem.metadata()
/// synchronously in a loop over all open buffers. If metadata() blocks (as it
/// does with RemoteFileSystem when the SSH connection drops), the entire editor
/// freezes.
///
/// After the fix, process_async_messages() should return quickly even when the
/// filesystem is slow/hanging.
#[test]
fn test_poll_file_changes_does_not_hang_with_slow_metadata() {
    // Create a filesystem where metadata() blocks for a very long time
    // (simulating a dead remote connection)
    let slow_config = SlowFsConfig {
        metadata_delay: Duration::from_secs(999),
        ..SlowFsConfig::none()
    };
    let inner = Arc::new(fresh::model::filesystem::StdFileSystem);
    let slow_fs = Arc::new(SlowFileSystem::new(inner, slow_config));

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(Config {
                editor: fresh::config::EditorConfig {
                    auto_revert_poll_interval_ms: 100,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(slow_fs),
    )
    .unwrap();

    // Create a file and open it in the editor
    let temp_dir = harness.project_dir().expect("harness should have a temp dir");
    let file_path = temp_dir.join("test_file.txt");
    std::fs::write(&file_path, "hello").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Advance time past the poll interval so poll_file_changes will fire
    harness.advance_time(Duration::from_millis(200));

    // This call should return quickly, not block for 999 seconds.
    // BUG: Currently hangs here because metadata() sleeps for 999s.
    let _ = harness.editor_mut().process_async_messages();

    // If we get here, the fix is working — polling didn't block.
}

/// Test: poll_file_tree_changes must not block the event loop when metadata() hangs.
///
/// Same issue as poll_file_changes but for directory mtime polling.
#[test]
fn test_poll_file_tree_changes_does_not_hang_with_slow_metadata() {
    let slow_config = SlowFsConfig {
        metadata_delay: Duration::from_secs(999),
        ..SlowFsConfig::none()
    };
    let inner = Arc::new(fresh::model::filesystem::StdFileSystem);
    let slow_fs = Arc::new(SlowFileSystem::new(inner, slow_config));

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(Config {
                editor: fresh::config::EditorConfig {
                    auto_revert_poll_interval_ms: 100,
                    file_tree_poll_interval_ms: 100,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_filesystem(slow_fs),
    )
    .unwrap();

    // Create a file and open it (this triggers file tree population)
    let temp_dir = harness.project_dir().expect("harness should have a temp dir");
    let file_path = temp_dir.join("test_file.txt");
    std::fs::write(&file_path, "hello").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Advance time past the poll interval
    harness.advance_time(Duration::from_millis(200));

    // This should return quickly, not block.
    let _ = harness.editor_mut().process_async_messages();
}
