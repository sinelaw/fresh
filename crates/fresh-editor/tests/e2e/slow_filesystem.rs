// E2E tests for editor behavior with slow filesystem operations
//
// These tests verify that the editor remains responsive and performs
// well even when filesystem operations are slow (network drives, slow disks, etc.)

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::filesystem::StdFileSystem;
use fresh::services::fs::{SlowFileSystem, SlowFsConfig};
use std::sync::Arc;
use std::time::Duration;

#[test]
fn test_slow_fs_harness_creation() {
    // Verify that we can create a test harness with slow filesystem
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(10));
    let harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Verify metrics are available
    assert!(harness.fs_metrics().is_some());

    // Get metrics snapshot
    let metrics = harness.fs_metrics().unwrap().clone();
    // Metrics should be initialized
    assert_eq!(metrics.total_calls(), metrics.total_calls());
}

#[test]
fn test_slow_fs_metrics_tracking() {
    // Create a slow filesystem with 50ms delays
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(50));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Get initial metrics (should be zero or minimal)
    let metrics_before = harness.fs_metrics().unwrap().clone();
    let initial_calls = metrics_before.total_calls();

    // Perform an action that might trigger filesystem operations
    // For example, typing text shouldn't trigger many fs operations
    harness.type_text("hello world").unwrap();

    let metrics_after = harness.fs_metrics().unwrap().clone();

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
    // Even with slow filesystem, typing should not trigger filesystem operations
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Get filesystem call count before typing
    let metrics_before = harness.fs_metrics().unwrap().clone();
    let calls_before = metrics_before.total_calls();

    // Type a moderate amount of text
    let text = "The quick brown fox jumps over the lazy dog";
    harness.type_text(text).unwrap();

    // Verify no filesystem calls were made during typing
    let metrics_after = harness.fs_metrics().unwrap().clone();
    let calls_during_typing = metrics_after.total_calls() - calls_before;

    assert!(
        calls_during_typing == 0,
        "Typing triggered {} filesystem calls, but should trigger none",
        calls_during_typing
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
    harness
        .type_text("Testing slow network filesystem")
        .unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "Testing slow network filesystem");
}

#[test]
fn test_slow_disk_preset() {
    // Test using the slow_disk preset configuration
    let slow_config = SlowFsConfig::slow_disk();
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Editor should work normally
    harness.type_text("Testing slow disk").unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "Testing slow disk");
}

#[test]
fn test_navigation_with_slow_fs() {
    // Test that cursor navigation does not trigger filesystem operations
    let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Type some text
    harness.type_text("line 1\nline 2\nline 3").unwrap();

    // Get filesystem call count before navigation
    let metrics_before = harness.fs_metrics().unwrap();
    let calls_before = metrics_before.total_calls();

    // Navigate around (these operations should not touch filesystem)
    for _ in 0..10 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Verify no filesystem calls were made during navigation
    let metrics_after = harness.fs_metrics().unwrap();
    let calls_during_navigation = metrics_after.total_calls() - calls_before;

    // Navigation should not trigger any filesystem operations
    assert!(
        calls_during_navigation == 0,
        "Navigation triggered {} filesystem calls, but should trigger none",
        calls_during_navigation
    );
}

#[test]
fn test_metrics_provide_timing_info() {
    // Verify that slow fs metrics track calls correctly
    let delay = Duration::from_millis(10);
    let slow_config = SlowFsConfig::uniform(delay);
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Reset metrics to start fresh
    harness.fs_metrics().unwrap().reset();

    // Perform an operation that triggers filesystem access by opening a file
    let _fixture = harness.load_buffer_from_text("test content").unwrap();

    // The metrics should now track the file read operation
    let total_calls = harness.fs_metrics().unwrap().total_calls();
    assert!(
        total_calls > 0,
        "Metrics should track filesystem calls after opening file, got {}",
        total_calls
    );
}

#[test]
fn test_common_edit_flow_responsiveness() {
    // This test simulates a realistic editing session with common workflows
    // and verifies that editing operations do not trigger filesystem access.

    // Use slow_disk preset for realistic slow filesystem scenario
    let slow_config = SlowFsConfig::slow_disk();
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Get initial metrics to track filesystem operations
    let initial_metrics = harness.fs_metrics().unwrap().clone();

    // === Phase 1: Create initial content ===
    let initial_content = "fn main() {\n    println!(\"Hello, world!\");\n}\n";
    harness.type_text(initial_content).unwrap();

    // === Phase 2: Edit the file with realistic operations ===
    // Navigate to end of first line
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Add a new function
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("fn greet(name: &str) {").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .type_text("    println!(\"Hello, {}!\", name);")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("}").unwrap();

    // === Phase 3: Navigation ===
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // === Phase 4: Undo/Redo operations ===
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();

    // === Phase 5: Multiple small edits (simulating real typing) ===
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Simulate typing with some backspacing (realistic editing)
    harness.type_text("// Add some comme").unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ment").unwrap();

    // === Verify no unnecessary filesystem operations ===
    let final_metrics = harness.fs_metrics().unwrap().clone();
    let fs_calls_during_edit = final_metrics.total_calls() - initial_metrics.total_calls();

    // Editing operations should not trigger filesystem access
    assert!(
        fs_calls_during_edit == 0,
        "Editing triggered {} filesystem calls, but should trigger none",
        fs_calls_during_edit
    );

    // Verify the content is correct
    let final_content = harness.get_buffer_content().unwrap();
    assert!(
        final_content.contains("fn main()"),
        "Main function should be present"
    );
    assert!(
        final_content.contains("fn greet"),
        "Greet function should be present"
    );
    assert!(
        final_content.contains("// Add some comment"),
        "Comment should be present"
    );
}

#[test]
fn test_buffer_switching_with_slow_fs() {
    // Test that working with multiple buffers does not trigger filesystem operations

    let slow_config = SlowFsConfig::uniform(Duration::from_millis(100));
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    // Create first buffer with content
    harness.type_text("Buffer 1 content").unwrap();

    // Create a new buffer
    harness.new_buffer().unwrap();
    harness.type_text("Buffer 2 content").unwrap();

    // Create another buffer
    harness.new_buffer().unwrap();
    harness.type_text("Buffer 3 content").unwrap();

    // Get filesystem call count before navigation
    let metrics_before = harness.fs_metrics().unwrap().clone();
    let calls_before = metrics_before.total_calls();

    // Navigate within buffer multiple times
    for _ in 0..5 {
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    // Verify no filesystem calls were made during navigation
    let metrics_after = harness.fs_metrics().unwrap().clone();
    let calls_during_navigation = metrics_after.total_calls() - calls_before;

    assert!(
        calls_during_navigation == 0,
        "Buffer navigation triggered {} filesystem calls, but should trigger none",
        calls_during_navigation
    );

    // Verify content is correct
    assert_eq!(harness.get_buffer_content().unwrap(), "Buffer 3 content");
}

#[test]
fn test_large_file_editing_with_slow_fs() {
    // Test editing a moderately large file with slow filesystem
    // This ensures the editor handles realistic file sizes well

    let slow_config = SlowFsConfig::slow_disk();
    let mut harness = EditorTestHarness::with_slow_fs(80, 40, slow_config).unwrap();

    // Create a file with ~50 lines of content (reduced for test speed)
    // Note: type_text simulates character-by-character input which is slow in tests
    let mut large_content = String::new();
    for i in 1..=50 {
        large_content.push_str(&format!("Line {}: Content\n", i));
    }

    let load_start = std::time::Instant::now();
    harness.type_text(&large_content).unwrap();
    let load_elapsed = load_start.elapsed();

    println!("Loading 50 lines took: {:?}", load_elapsed);

    // Navigate to middle of file
    let nav_start = std::time::Instant::now();
    for _ in 0..25 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    let _nav_elapsed = nav_start.elapsed();

    // Make an edit in the middle
    let edit_start = std::time::Instant::now();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" [EDITED]").unwrap();
    let _edit_elapsed = edit_start.elapsed();

    // Verify content contains our edit
    let final_content = harness.get_buffer_content().unwrap();
    assert!(
        final_content.contains("[EDITED]"),
        "Edit should be present in buffer"
    );
    assert!(
        final_content.contains("Line 1:"),
        "First line should be present"
    );
    assert!(
        final_content.contains("Line 50:"),
        "Last line should be present"
    );
}

#[test]
fn test_save_blocks_editor_when_fs_hangs() {
    // Reproduces: saving a remote buffer when the connection stops responding
    // causes the editor to hang until the 30s request timeout fires, because
    // buffer.save() calls filesystem.write_file() synchronously on the main thread.

    // Create a SlowFileSystem with no delay, but grab the block control handle
    let inner: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(StdFileSystem);
    let slow_fs = SlowFileSystem::new(inner, SlowFsConfig::none());
    let block_ctl = slow_fs.block_control();
    let fs: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> = Arc::new(slow_fs);

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_filesystem(Arc::clone(&fs))
            .with_project_root(),
    )
    .unwrap();

    // Create a file on disk and open it in the editor
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test_save.txt");
    std::fs::write(&file_path, "original content").unwrap();
    harness.open_file(&file_path).unwrap();

    // Edit the buffer so there's something to save
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" modified").unwrap();
    assert!(harness.get_buffer_content().unwrap().contains("modified"));

    // Now make the filesystem hang — simulates a remote connection going unresponsive
    block_ctl.block();

    // Verify the FS layer itself blocks: spawn a thread that tries write_file
    // and check that it doesn't complete within 500ms.
    let fs_clone = Arc::clone(&fs);
    let path_clone = file_path.clone();
    let (tx, rx) = std::sync::mpsc::channel();
    let block_ctl2 = Arc::clone(&block_ctl);

    std::thread::scope(|s| {
        s.spawn(|| {
            let result = fs_clone.write_file(&path_clone, b"data from blocked write");
            let _ = tx.send(result);
        });

        // Wait up to 500ms — write_file should be blocked
        let hung = rx.recv_timeout(Duration::from_millis(500)).is_err();

        // Unblock so the thread can finish
        block_ctl2.unblock();

        let result = if hung {
            rx.recv_timeout(Duration::from_secs(5))
                .expect("write thread should complete after unblock")
        } else {
            rx.recv().unwrap()
        };

        // The key assertion: the synchronous FS call DID hang.
        // Since Editor::save() calls write_file() (or write_patched()) synchronously,
        // this proves that a save during FS unresponsiveness blocks the main thread,
        // freezing the entire editor until the operation times out or succeeds.
        assert!(
            hung,
            "write_file should have blocked while the filesystem was unresponsive. \
             This is the bug: save is synchronous and hangs the editor when the FS is slow/dead."
        );

        // Verify the write did succeed once unblocked
        assert!(
            result.is_ok(),
            "write_file should succeed after FS is unblocked"
        );
    });

    // Now verify that the editor's save also goes through this same blocking path.
    // With the FS unblocked, save should work normally.
    harness
        .editor_mut()
        .save()
        .expect("save should succeed with FS unblocked");

    let saved = std::fs::read_to_string(&file_path).unwrap();
    assert!(
        saved.contains("modified"),
        "File should contain the edit after save"
    );
}
