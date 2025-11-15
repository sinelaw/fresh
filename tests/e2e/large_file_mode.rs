use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::visual_testing::VisualFlow;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test cursor positioning when moving down in large file mode
/// This test catches a bug where cursor movement with Down arrow key
/// doesn't work correctly after the first few lines in large file mode
#[test]
fn test_large_file_cursor_down_movement() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_down_movement").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let mut flow = VisualFlow::new(
        "Large File Cursor Movement",
        "Large File Mode",
        "Testing cursor movement with Down arrow in large file mode",
    );

    // Step 1: Initial state at top of file
    harness
        .capture_visual_step(&mut flow, "initial", "File opened at top")
        .unwrap();

    let initial_pos = harness.cursor_position();
    assert_eq!(initial_pos, 0, "Should start at position 0");

    // Step 2: Move down line by line and verify cursor keeps moving forward
    let mut prev_pos = initial_pos;

    for i in 1..=50 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

        let cursor_pos = harness.cursor_position();
        let screen_pos = harness.screen_cursor_position();

        // The key check: cursor position should keep advancing
        assert!(
            cursor_pos > prev_pos,
            "After {} Down presses, cursor should advance from {} but is at {}",
            i,
            prev_pos,
            cursor_pos
        );

        // Screen cursor Y should increase or stay same (if scrolling)
        // but should definitely be visible
        assert!(
            screen_pos.1 < 24,
            "Screen cursor Y should be within terminal bounds at iteration {}",
            i
        );

        // Visual checkpoints at key moments
        if i == 5 {
            harness
                .capture_visual_step(&mut flow, "after_5_down", "After pressing Down 5 times")
                .unwrap();
        } else if i == 25 {
            harness
                .capture_visual_step(&mut flow, "after_25_down", "After pressing Down 25 times")
                .unwrap();
        } else if i == 50 {
            harness
                .capture_visual_step(&mut flow, "after_50_down", "After pressing Down 50 times")
                .unwrap();
        }

        prev_pos = cursor_pos;
    }

    flow.finalize();
}

/// Test typing characters in large file mode
/// This test catches a bug where typed characters don't appear at the
/// cursor position in large file mode
#[test]
fn test_large_file_typing() {
    let big_txt_path = TestFixture::big_txt_for_test("typing").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let mut flow = VisualFlow::new(
        "Large File Typing",
        "Large File Mode",
        "Testing that typed characters appear at cursor position in large file mode",
    );

    // Step 1: Initial state
    harness
        .capture_visual_step(&mut flow, "initial", "File opened at top")
        .unwrap();

    // Step 2: Move down several lines to test typing deeper in the file
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let cursor_pos_before_typing = harness.cursor_position();
    harness
        .capture_visual_step(
            &mut flow,
            "before_typing",
            "Positioned at line 10 before typing",
        )
        .unwrap();

    // Step 3: Type some characters
    let text_to_type = "HELLO";
    harness.type_text(text_to_type).unwrap();

    let cursor_pos_after_typing = harness.cursor_position();
    let screen_pos_after = harness.screen_cursor_position();

    // Verify cursor moved forward by the number of characters typed
    assert_eq!(
        cursor_pos_after_typing,
        cursor_pos_before_typing + text_to_type.len(),
        "Cursor should have moved forward by {} bytes after typing '{}', but moved from {} to {}",
        text_to_type.len(),
        text_to_type,
        cursor_pos_before_typing,
        cursor_pos_after_typing
    );

    // Verify screen cursor is visible and in a reasonable position
    assert!(
        screen_pos_after.0 < 80,
        "Screen cursor X position should be within terminal width"
    );
    assert!(
        screen_pos_after.1 < 24,
        "Screen cursor Y position should be within terminal height"
    );

    harness
        .capture_visual_step(&mut flow, "after_typing", "After typing 'HELLO'")
        .unwrap();

    // Step 4: Continue to move down and type more to verify consistency throughout the file
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let cursor_pos_before_second = harness.cursor_position();

    harness.type_text("WORLD").unwrap();

    let cursor_pos_after_second = harness.cursor_position();

    // Verify cursor still moves correctly after typing deeper in the file
    assert_eq!(
        cursor_pos_after_second,
        cursor_pos_before_second + 5,
        "After typing 'WORLD' deeper in file, cursor should advance by 5 bytes"
    );

    harness
        .capture_visual_step(
            &mut flow,
            "after_second_typing",
            "After typing more text deeper in file",
        )
        .unwrap();

    flow.finalize();
}

/// Test cursor positioning when rapidly moving down in large file
/// This stress tests the cursor tracking to ensure it stays in sync
#[test]
fn test_large_file_rapid_cursor_movement() {
    let big_txt_path = TestFixture::big_txt_for_test("rapid_cursor_movement").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    // Rapidly move down 100 lines
    let target_line = 100;
    for i in 1..=target_line {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

        let cursor_pos = harness.cursor_position();
        let expected_byte_pos = i * 80; // Each line is 80 bytes

        assert_eq!(
            cursor_pos, expected_byte_pos,
            "After {} Down presses, cursor should be at byte position {}, but is at {}",
            i, expected_byte_pos, cursor_pos
        );
    }

    // Verify final position
    let final_pos = harness.cursor_position();
    assert_eq!(
        final_pos,
        target_line * 80,
        "Final cursor position should be at line {} (byte {})",
        target_line,
        target_line * 80
    );

    // Verify screen cursor is visible
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < 80 && screen_pos.1 < 24,
        "Screen cursor should be within terminal bounds, but is at {:?}",
        screen_pos
    );
}

/// Test cursor and typing interaction in large file mode
/// This combines cursor movement and typing to catch interaction bugs
#[test]
fn test_large_file_cursor_movement_and_typing() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_and_typing").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    // Move down several lines
    let moves_down = 10;
    for _ in 0..moves_down {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    let pos_after_movement = harness.cursor_position();
    assert_eq!(
        pos_after_movement,
        moves_down * 80,
        "After moving down {} times, should be at byte {}",
        moves_down,
        moves_down * 80
    );

    // Type at this position
    let text = "TEST";
    harness.type_text(text).unwrap();

    let pos_after_typing = harness.cursor_position();
    assert_eq!(
        pos_after_typing,
        pos_after_movement + text.len(),
        "After typing, cursor should advance by text length"
    );

    // Move down again
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    let pos_after_second_movement = harness.cursor_position();
    // After editing, line boundaries may have shifted, but cursor should still advance
    // The important thing is that the cursor moves and doesn't get stuck
    assert!(
        pos_after_second_movement > pos_after_typing,
        "Cursor should advance when pressing Down after typing"
    );

    // Verify screen cursor is visible throughout
    let screen_pos = harness.screen_cursor_position();
    assert!(
        screen_pos.0 < 80 && screen_pos.1 < 24,
        "Screen cursor should remain visible"
    );
}

/// Test that cursor screen position matches logical position in large files
/// This is a visual regression test to ensure the cursor is rendered at the right location
#[test]
fn test_large_file_cursor_screen_position_accuracy() {
    let big_txt_path = TestFixture::big_txt_for_test("cursor_screen_position").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&big_txt_path).unwrap();
    harness.render().unwrap();

    let mut flow = VisualFlow::new(
        "Large File Cursor Screen Position",
        "Large File Mode",
        "Verifying cursor screen position matches logical position",
    );

    // Capture initial state
    harness
        .capture_visual_step(&mut flow, "line_0", "Cursor at line 0")
        .unwrap();
    let initial_screen_y = harness.screen_cursor_position().1;

    // Move down and capture screen positions
    for i in 1..=10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

        let screen_pos = harness.screen_cursor_position();
        let cursor_pos = harness.cursor_position();

        // Verify logical position
        assert_eq!(
            cursor_pos,
            i * 80,
            "Logical cursor position should match line number * 80"
        );

        // The screen Y position should increase (until we hit viewport scrolling)
        // but we're within the first 10 lines so it should be directly visible
        if i < 20 {
            // Well within viewport
            assert!(
                screen_pos.1 > initial_screen_y,
                "Screen cursor Y should increase when moving down within viewport"
            );
        }

        // Take a visual snapshot every 5 lines
        if i % 5 == 0 {
            harness
                .capture_visual_step(
                    &mut flow,
                    &format!("line_{}", i),
                    &format!("Cursor at line {}", i),
                )
                .unwrap();
        }
    }

    flow.finalize();
}
