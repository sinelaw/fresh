use crate::common::harness::EditorTestHarness;
use fresh::services::stdin_spool::StdinSpool;
use std::sync::Arc;

/// A spool already holding `content`, as if stdin had been fully drained.
///
/// The real thing has no name, so these tests go through the same nameless
/// spool the editor uses rather than standing in a temp file for it.
fn stdin_spool(content: &str) -> Arc<StdinSpool> {
    Arc::new(StdinSpool::with_contents(content.as_bytes()).unwrap())
}

/// Test opening a buffer from stdin temp file
#[test]
fn test_open_stdin_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create temp file with stdin content
    let content = "Hello from stdin!\nLine 2\nLine 3";
    let spool = stdin_spool(content);

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    // Verify the buffer contains the stdin content
    harness.assert_buffer_content(content);

    // Render and check the display shows [stdin]
    harness.render().unwrap();
    harness.assert_screen_contains("[stdin]");
}

/// Test that stdin buffer is not initially marked as modified
#[test]
fn test_stdin_buffer_not_modified_initially() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "Some content from stdin";
    let spool = stdin_spool(content);

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    // The buffer should not be modified (it's fresh from stdin)
    harness.render().unwrap();
    // Modified buffers show a dot after the name, so [stdin] should not have a dot
    // The status bar should show just [stdin] not [stdin] •
    let screen = harness.screen_to_string();
    assert!(screen.contains("[stdin]"), "Expected [stdin] in status bar");
}

/// Test opening empty stdin content
#[test]
fn test_open_empty_stdin_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let spool = stdin_spool("");

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    // Empty buffer should still work
    harness.assert_buffer_content("");
    harness.render().unwrap();
    harness.assert_screen_contains("[stdin]");
}

/// Test stdin buffer replaces empty initial buffer
#[test]
fn test_stdin_replaces_empty_buffer() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Initially we have an empty [No Name] buffer
    harness.assert_buffer_content("");

    // Open stdin buffer - it should replace the empty initial buffer
    let content = "Stdin content";
    let spool = stdin_spool(content);

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    // Verify the content is there
    harness.assert_buffer_content(content);

    // Should only have one buffer (stdin replaced the initial one)
    harness.render().unwrap();
    harness.assert_screen_contains("[stdin]");
    // Should NOT contain [No Name] since it was replaced
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("[No Name]"),
        "Empty buffer should have been replaced"
    );
}

/// Test stdin buffer with multiline content
#[test]
fn test_stdin_multiline_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5";
    let spool = stdin_spool(content);

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    harness.assert_buffer_content(content);

    // Render and verify content is visible
    harness.render().unwrap();
    harness.assert_screen_contains("Line 1");
    harness.assert_screen_contains("Line 2");
}

/// Test stdin buffer with special characters
#[test]
fn test_stdin_special_characters() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "Tab:\there\nUnicode: 你好 🎉\nSpecial: <>&\"'";
    let spool = stdin_spool(content);

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    harness.assert_buffer_content(content);
}

/// Test stdin streaming with background thread updates buffer progressively
#[test]
fn test_stdin_streaming_progress() {
    use std::io::Write;
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    };
    use std::thread;
    use std::time::Duration;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // The spool plus its write side, exactly as `start_stdin_streaming`
    // hands them out: the thread below stands in for the pipe drain.
    let (spool, mut file) = StdinSpool::create().unwrap();
    let spool = Arc::new(spool);

    // Signal to stop the streaming thread
    let stop_signal = Arc::new(AtomicBool::new(false));
    let stop_signal_clone = stop_signal.clone();

    // Spawn background thread that writes data progressively
    let thread_handle = thread::spawn(move || -> anyhow::Result<()> {
        let mut count = 0;
        while !stop_signal_clone.load(Ordering::Relaxed) && count < 10 {
            writeln!(file, "Line {}", count)?;
            file.flush()?;
            count += 1;
            thread::sleep(Duration::from_millis(10));
        }
        Ok(())
    });

    // Open stdin buffer with the background thread handle
    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), Some(thread_handle))
        .unwrap();

    // Initially empty or nearly empty
    let initial_len = harness.editor().active_state().buffer.len();

    // Poll a few times to let data stream in
    for _ in 0..50 {
        harness.editor_mut().poll_stdin_streaming();
        thread::sleep(Duration::from_millis(5));
    }

    // Buffer should have grown
    let after_poll_len = harness.editor().active_state().buffer.len();
    assert!(
        after_poll_len > initial_len,
        "Buffer should have grown from {} to more, but got {}",
        initial_len,
        after_poll_len
    );

    // Signal thread to stop
    stop_signal.store(true, Ordering::Relaxed);

    // Wait for completion
    for _ in 0..100 {
        if !harness.editor().is_stdin_streaming() {
            break;
        }
        harness.editor_mut().poll_stdin_streaming();
        thread::sleep(Duration::from_millis(10));
    }

    // Streaming should be complete
    assert!(
        !harness.editor().is_stdin_streaming(),
        "Streaming should be complete"
    );

    // Buffer should contain some lines
    harness.render().unwrap();
    harness.assert_screen_contains("[stdin]");
}

/// Test stdin with large file triggers lazy loading mode
#[test]
fn test_stdin_large_file_lazy_loading() {
    use crate::common::harness::HarnessOptions;
    use fresh::config::Config;

    // Create config with very low large file threshold (1KB) for testing
    let mut config = Config::default();
    config.editor.large_file_threshold_bytes = 1024; // 1KB threshold

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();

    // Create content larger than the threshold
    let content = "X".repeat(2048); // 2KB of X's
    let spool = stdin_spool(&content);

    harness
        .editor_mut()
        .open_stdin_buffer(Arc::clone(&spool), None)
        .unwrap();

    // Verify the buffer is in large file mode
    let state = harness.editor().active_state();
    assert!(
        state.buffer.is_large_file(),
        "Large stdin should trigger lazy loading mode"
    );

    // Status should mention lazy loading
    harness.render().unwrap();
    harness.assert_screen_contains("[stdin]");
}
