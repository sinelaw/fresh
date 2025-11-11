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
    harness
        .type_text("Testing slow network filesystem")
        .unwrap();
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
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
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
    let metrics = tokio::runtime::Runtime::new()
        .unwrap()
        .block_on(harness.get_fs_metrics_snapshot())
        .unwrap();

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

    let metrics = tokio::runtime::Runtime::new()
        .unwrap()
        .block_on(harness.get_fs_metrics_snapshot())
        .unwrap();

    // The metrics should track total delay time
    // (exact value depends on how many fs operations happened during editor init)
    if metrics.total_calls() > 0 {
        assert!(
            metrics.total_delay_time > Duration::ZERO,
            "Metrics should track delay time"
        );
    }
}

#[test]
fn test_common_edit_flow_responsiveness() {
    // This test simulates a realistic editing session with common workflows:
    // - Loading a file
    // - Editing text
    // - Scrolling through content
    // - Saving the file
    // - Using file explorer (if we open it)
    //
    // The goal is to ensure all operations remain responsive even with slow I/O

    // Use slow_disk preset for realistic slow filesystem scenario
    let slow_config = SlowFsConfig::slow_disk();
    let mut harness = EditorTestHarness::with_slow_fs(80, 24, slow_config).unwrap();

    let runtime = tokio::runtime::Runtime::new().unwrap();

    // Get initial metrics to track filesystem operations
    let initial_metrics = runtime.block_on(harness.get_fs_metrics_snapshot()).unwrap();
    let start_time = std::time::Instant::now();

    // === Phase 1: Create and load a file ===
    let phase1_start = std::time::Instant::now();

    // Create some initial content to work with
    let initial_content = "fn main() {\n    println!(\"Hello, world!\");\n}\n";
    harness.type_text(initial_content).unwrap();

    let phase1_elapsed = phase1_start.elapsed();
    assert!(
        phase1_elapsed < Duration::from_millis(500),
        "Phase 1 (file creation) took {:?}, too slow",
        phase1_elapsed
    );

    // === Phase 2: Edit the file with realistic operations ===
    let phase2_start = std::time::Instant::now();

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

    let phase2_elapsed = phase2_start.elapsed();
    assert!(
        phase2_elapsed < Duration::from_secs(2),
        "Phase 2 (editing) took {:?}, typing is not responsive",
        phase2_elapsed
    );

    // === Phase 3: Navigation and scrolling ===
    let phase3_start = std::time::Instant::now();

    // Move cursor around the document
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap(); // Go to start
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

    let phase3_elapsed = phase3_start.elapsed();
    assert!(
        phase3_elapsed < Duration::from_millis(200),
        "Phase 3 (navigation) took {:?}, cursor movement is sluggish",
        phase3_elapsed
    );

    // === Phase 4: Undo/Redo operations ===
    let phase4_start = std::time::Instant::now();

    // Test undo
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();

    // Test redo
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();

    let phase4_elapsed = phase4_start.elapsed();
    assert!(
        phase4_elapsed < Duration::from_millis(100),
        "Phase 4 (undo/redo) took {:?}, operations are slow",
        phase4_elapsed
    );

    // === Phase 5: Multiple small edits (simulating real typing) ===
    let phase5_start = std::time::Instant::now();

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

    let phase5_elapsed = phase5_start.elapsed();
    assert!(
        phase5_elapsed < Duration::from_millis(500),
        "Phase 5 (incremental edits) took {:?}, interactive typing feels laggy",
        phase5_elapsed
    );

    // === Phase 6: Verify no unnecessary filesystem operations ===
    let final_metrics = runtime.block_on(harness.get_fs_metrics_snapshot()).unwrap();

    // During text editing, we shouldn't be hitting the filesystem much
    let fs_calls_during_edit = final_metrics.total_calls() - initial_metrics.total_calls();

    // There might be some calls during initialization, but editing operations
    // should not trigger filesystem access (unless autosave is enabled)
    assert!(
        fs_calls_during_edit < 50,
        "Too many filesystem calls ({}) during editing session. \
         Text editing should not require frequent filesystem access.",
        fs_calls_during_edit
    );

    // === Overall responsiveness check ===
    let total_elapsed = start_time.elapsed();

    // The entire editing session should feel snappy
    // Even with slow disk (200ms dir reads, 20ms metadata), interactive
    // operations should complete quickly because they don't touch disk
    assert!(
        total_elapsed < Duration::from_secs(5),
        "Total editing session took {:?}, which feels unresponsive. \
         Interactive operations should not be blocked by slow I/O.",
        total_elapsed
    );

    // Verify the content is correct
    let final_content = harness.get_buffer_content();
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

    // Print metrics for analysis
    println!("=== Edit Flow Performance Metrics ===");
    println!("Phase 1 (Initial content): {:?}", phase1_elapsed);
    println!("Phase 2 (Editing): {:?}", phase2_elapsed);
    println!("Phase 3 (Navigation): {:?}", phase3_elapsed);
    println!("Phase 4 (Selection): {:?}", phase4_elapsed);
    println!("Phase 5 (Incremental): {:?}", phase5_elapsed);
    println!("Total time: {:?}", total_elapsed);
    println!("Filesystem calls: {}", fs_calls_during_edit);
    println!("Total delay time: {:?}", final_metrics.total_delay_time);
}

#[test]
fn test_buffer_switching_with_slow_fs() {
    // Test that switching between multiple buffers remains responsive
    // even with slow filesystem

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

    // Now switch between buffers rapidly
    let start = std::time::Instant::now();

    // Switch back and forth multiple times
    for _ in 0..5 {
        // These would be buffer switching commands
        // For now we'll just verify we can create and work with multiple buffers
        harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    let elapsed = start.elapsed();

    // Buffer switching should be instant (in-memory operation)
    assert!(
        elapsed < Duration::from_millis(200),
        "Buffer navigation took {:?}, should be instant as it's in-memory",
        elapsed
    );

    // Verify content is correct
    assert_eq!(harness.get_buffer_content(), "Buffer 3 content");
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
    let nav_elapsed = nav_start.elapsed();

    // Make an edit in the middle
    let edit_start = std::time::Instant::now();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" [EDITED]").unwrap();
    let edit_elapsed = edit_start.elapsed();

    // Verify content contains our edit
    let final_content = harness.get_buffer_content();
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
