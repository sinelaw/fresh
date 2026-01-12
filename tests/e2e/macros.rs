use crate::common::harness::EditorTestHarness;

/// Test that recording a macro and playing it back with "Play Last Macro" works
/// This also verifies that PlayLastMacro doesn't cause infinite recursion
#[test]
fn test_macro_record_and_play_last() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Open command palette and start recording macro on register 0
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Record Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show prompt for register
    harness.assert_screen_contains("Record macro (0-9):");

    // Type '0' to select register 0
    harness.type_text("0").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show recording indicator
    harness.assert_screen_contains("Recording");

    // Type some text that will be recorded
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Verify text was inserted
    harness.assert_screen_contains("hello");

    // Stop recording via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Stop Recording").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show macro saved message
    harness.assert_screen_contains("Macro");

    // Move to a new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now play the last recorded macro via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Play Last Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The macro should have played and inserted "hello" again
    // We should now have "hello" on line 1 and "hello" on line 2
    let screen = harness.screen_to_string();
    let hello_count = screen.matches("hello").count();
    assert!(
        hello_count >= 2,
        "Expected 'hello' to appear at least twice after playing macro, but found {} occurrences. Screen:\n{}",
        hello_count,
        screen
    );

    // Verify no stack overflow or error occurred
    harness.assert_screen_not_contains("error");
    harness.assert_screen_not_contains("overflow");
}

/// Test that recording a macro with multiple cursors and playing it back doesn't cause stack overflow
/// This reproduces a bug where recording with multiple cursors active, then playing via "Play Last Macro"
/// caused infinite recursion
#[test]
fn test_macro_with_multiple_cursors_no_overflow() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Add some initial lines of text
    harness.type_text("line 1").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("line 2").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("line 3").unwrap();
    harness.render().unwrap();

    // Move cursor up to line 2
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Start recording macro on register 0 via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Record Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("0").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be recording
    harness.assert_screen_contains("Recording");

    // Add multiple cursors via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Add Cursor Above").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type some text (with multiple cursors, should appear on multiple lines)
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Stop recording WITHOUT clearing cursors first - this was the bug trigger
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Stop Recording").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show macro saved
    harness.assert_screen_contains("Macro");

    // Debug: Show what's in the macro
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("List Macros").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let macro_list = harness.screen_to_string();
    println!("=== MACRO LIST ===\n{}\n==================", macro_list);

    // Close buffer and clear cursors
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Clear cursors by pressing Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now try to play the last macro - this used to cause stack overflow
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Play Last Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // If we got here without crashing, the test passed
    // The macro should have executed (adding cursor and typing X)
    let screen = harness.screen_to_string();
    println!("Screen after playing macro:\n{}", screen);

    // Verify we see the X's that were typed
    assert!(
        screen.contains("X"),
        "Macro should have typed 'X'. Screen:\n{}",
        screen
    );
}

/// Test that playing last macro when no macro was recorded shows appropriate message
#[test]
fn test_play_last_macro_when_none_recorded() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Try to play last macro when none has been recorded
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Play Last Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show message that no macro was recorded
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("No macro") || screen.contains("no macro"),
        "Expected message about no macro recorded. Screen:\n{}",
        screen
    );
}

/// Test that macro playback is undoable as a single operation
#[test]
fn test_macro_playback_is_undoable() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Start recording macro on register 0
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Record Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("0").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Type some text
    harness.type_text("abc").unwrap();
    harness.render().unwrap();

    // Stop recording
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Stop Recording").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Move to new line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify initial state - should have "abc" on first line
    harness.assert_screen_contains("abc");

    // Play the macro
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Play Last Macro").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now have "abc" twice
    let screen = harness.screen_to_string();
    let abc_count = screen.matches("abc").count();
    assert!(
        abc_count >= 2,
        "Expected 'abc' twice after macro playback, found {}. Screen:\n{}",
        abc_count,
        screen
    );

    // Now undo - should undo the entire macro playback
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // After one undo, the second "abc" should be gone
    // (If macro playback is properly grouped, one undo removes all macro actions)
    let screen_after_undo = harness.screen_to_string();
    let abc_count_after = screen_after_undo.matches("abc").count();

    // We expect at most 1 "abc" after undo (the original one typed during recording)
    // Note: The first "abc" was typed during recording, so it has its own undo entry
    assert!(
        abc_count_after < abc_count,
        "Undo should have removed macro playback. Before: {} 'abc', after: {}. Screen:\n{}",
        abc_count,
        abc_count_after,
        screen_after_undo
    );
}
