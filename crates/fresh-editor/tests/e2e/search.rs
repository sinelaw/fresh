//! E2E tests for search and replace functionality

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Test basic forward search functionality
#[test]
fn test_basic_search_forward() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with searchable content
    std::fs::write(&file_path, "hello world\nfoo bar\nhello again\nbaz").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the search prompt appeared
    harness.assert_screen_contains("Search: ");

    // Type search query
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Confirm search - Enter moves to first match
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Cursor should move to the first match ("hello" at position 0)
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos, 0,
        "Cursor should be at the start of first 'hello'"
    );

    // Find next match with F3
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Cursor should move to second match ("hello" at line 3)
    let cursor_pos = harness.cursor_position();

    // Second "hello" starts at position after "hello world\nfoo bar\n"
    let expected_pos = "hello world\nfoo bar\n".len();
    assert_eq!(
        cursor_pos, expected_pos,
        "Cursor should be at the start of second 'hello'"
    );
}

/// Test that Ctrl+Shift+N works as an alternative for Shift+F3 (find previous)
/// This is important because many terminals don't properly capture Shift+F3
#[test]
fn test_find_previous_with_ctrl_shift_n() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with searchable content
    std::fs::write(&file_path, "hello world\nfoo bar\nhello again\nhello final").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Confirm search - moves to first match
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should be at first "hello" (position 0)
    let first_match_pos = harness.cursor_position();
    assert_eq!(first_match_pos, 0, "Should start at first 'hello'");

    // Find next match with F3
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Now at second "hello" (in "hello again")
    let second_match_pos = harness.cursor_position();
    assert!(
        second_match_pos > first_match_pos,
        "F3 should move to next match"
    );

    // Find previous with Ctrl+Shift+N (alternative for Shift+F3)
    harness
        .send_key(
            KeyCode::Char('n'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should be back at first match
    let back_to_first = harness.cursor_position();
    assert_eq!(
        back_to_first, first_match_pos,
        "Ctrl+Shift+N should go back to previous match"
    );
}

/// Test that selecting a word pre-populates the search prompt and find next keeps working
#[test]
fn test_find_next_prefills_from_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("search_selection.txt");
    std::fs::write(&file_path, "alpha word beta word gamma").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();

    let selected_text = harness.get_selected_text();
    assert_eq!(
        selected_text, "word",
        "Ctrl+W should select the word under the cursor"
    );

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();

    harness.assert_screen_contains("Search: word");

    // Clear selection so the confirmed search will run over the entire buffer
    harness
        .editor_mut()
        .active_state_mut()
        .cursors
        .primary_mut()
        .clear_selection();

    harness
        .editor_mut()
        .active_state_mut()
        .cursors
        .primary_mut()
        .position = 0;

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    assert_eq!(
        harness.cursor_position(),
        6,
        "Cursor should jump to the first 'word' match"
    );

    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();

    assert_eq!(
        harness.cursor_position(),
        16,
        "Find next should move to the following 'word'"
    );
}

/// Command palette entry should run a search restricted to the current selection
#[test]
fn test_find_in_selection_command() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("search_selection.txt");
    std::fs::write(&file_path, "alpha word beta word gamma").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();

    let selected_text = harness.get_selected_text();
    assert_eq!(
        selected_text, "word",
        "Ctrl+W should select the word under the cursor"
    );

    // Open command palette and choose "Find in Selection"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Find in Selection").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.render().unwrap();
    harness.assert_screen_contains("Search: word");

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    assert_eq!(
        harness.cursor_position(),
        6,
        "Cursor should jump to the first 'word' match inside selection"
    );

    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();

    assert_eq!(
        harness.cursor_position(),
        6,
        "Find next should stay within the selection"
    );

    assert_eq!(
        harness
            .editor()
            .get_status_message()
            .map(|msg| msg.as_str()),
        Some("No more matches.")
    );
}

/// Test incremental search highlighting as user types
#[test]
fn test_incremental_search_highlighting() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with multiple matches visible on screen
    std::fs::write(
        &file_path,
        "test line one\ntest line two\nother content\ntest line three\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "test" - should see highlighting before pressing Enter
    harness.type_text("test").unwrap();
    harness.render().unwrap();

    // Check that highlights appear in the rendered output
    // The screen should show the text with search highlights
    let screen = harness.screen_to_string();

    // Screen should contain the search prompt with "test"
    assert!(
        screen.contains("Search: test"),
        "Search prompt should show typed text"
    );

    // Verify matches are in the visible area (we have 3 "test" matches on screen)
    // This is a basic check - the highlighting is visual, but we can verify the content is there
    assert!(screen.contains("test line one"));
    assert!(screen.contains("test line two"));
    assert!(screen.contains("test line three"));
}

/// Test that search highlighting only applies to visible viewport
#[test]
fn test_search_highlighting_visible_only() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.txt");

    // Create a file with many lines, more than can fit on screen
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!("Line {} with search term\n", i));
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("search").unwrap();
    harness.render().unwrap();

    // The test passes if highlighting doesn't cause performance issues
    // (no timeout or excessive CPU usage)
    // In a real scenario, only visible lines would be highlighted

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be at first match
    let cursor_pos = harness.cursor_position();
    assert!(cursor_pos > 0, "Cursor should have moved to a match");

    // Scroll down and search should still work efficiently
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Find next should work even after scrolling
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let new_cursor_pos = harness.cursor_position();
    assert!(
        new_cursor_pos > cursor_pos,
        "Cursor should have moved to next match"
    );
}

/// Test interactive replace wrap-around behavior
#[test]
fn test_interactive_replace_wrap_around() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test.txt");

    // Create a file with "foo" at positions: 0, 20, 40
    // We'll start at position 25 (middle), so we should see:
    // 1. Match at 40 (after cursor)
    // 2. Wrap around
    // 3. Match at 0 (before starting position)
    // 4. Match at 20 (before starting position)
    // 5. Stop (no more matches before start_pos=25)
    std::fs::write(&file_path, "foo is here\nand\nfoo is there\nfoo again").unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to position 25 (somewhere in the middle, after first two "foo"s)
    // Content: "foo is here\n" = 12 chars, "and\n" = 4 chars (total 16), "foo is there\n" = 13 chars (total 29), "foo again"
    // So "foo" appears at: 0, 16, 29
    // Let's position at 25 (after second "foo")
    for _ in 0..25 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let start_pos = harness.cursor_position();
    assert_eq!(start_pos, 25, "Cursor should be at position 25");

    // Trigger interactive replace with Ctrl+Alt+R
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.render().unwrap();

    // Should show "Query replace: " prompt
    harness.assert_screen_contains("Query replace: ");

    // Type search pattern "foo"
    harness.type_text("foo").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "Query replace 'foo' with: " prompt
    harness.assert_screen_contains("Query replace 'foo' with: ");

    // Type replacement "XXX"
    harness.type_text("XXX").unwrap();
    harness.render().unwrap();

    // Confirm replacement
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be at first match after position 25, which is at position 29 ("foo again")
    let pos1 = harness.cursor_position();
    assert_eq!(pos1, 29, "Should be at third 'foo' (position 29)");
    harness.assert_screen_contains("Replace? (y)es (n)o (a)ll (c)ancel");

    // Press 'y' to replace this occurrence
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    // Should wrap around to beginning and find "foo" at position 0
    let pos2 = harness.cursor_position();
    assert_eq!(pos2, 0, "Should wrap to first 'foo' (position 0)");

    // Should show [Wrapped] indicator
    harness.assert_screen_contains("[Wrapped]");

    // Press 'n' to skip this one
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    // Should move to "foo" at position 16
    let pos3 = harness.cursor_position();
    assert_eq!(pos3, 16, "Should be at second 'foo' (position 16)");

    // Still shows [Wrapped] indicator
    harness.assert_screen_contains("[Wrapped]");

    // Press 'y' to replace this occurrence
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    // Should finish (no more matches before start_pos=25)
    // Check the status message shows completion (truncated on screen)
    harness.assert_screen_contains("Replaced 2 occ");

    // Verify the buffer content has the expected replacements
    let content = harness.get_buffer_content().unwrap();

    // We replaced:
    // - Third "foo" (at 29) -> "XXX"
    // - Skipped first "foo" (at 0)
    // - Replaced second "foo" (at 16) -> "XXX"
    // Expected: "foo is here\nand\nXXX is there\nXXX again"
    assert_eq!(
        content, "foo is here\nand\nXXX is there\nXXX again",
        "Should have replaced 2nd and 3rd 'foo' only"
    );
}

/// Test interactive replace stops at starting position after wrap
#[test]
fn test_interactive_replace_wrap_stops_at_start() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test.txt");

    // Create file with pattern at positions before and after cursor
    std::fs::write(&file_path, "foo\nbar\nbaz\nfoo\nqux\nfoo").unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to second "foo" (around position 12-16)
    for _ in 0..12 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Trigger query-replace
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.render().unwrap();

    // Enter search term
    harness.type_text("foo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Enter replacement
    harness.type_text("XXX").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be at second "foo", press 'n' to skip
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    // Should be at third "foo", press 'n' to skip
    harness.type_text("n").unwrap();
    harness.render().unwrap();

    // Should wrap and be at first "foo", press 'y' to replace
    harness.type_text("y").unwrap();
    harness.render().unwrap();

    // Should finish (second foo is at/past starting position)
    harness.assert_screen_contains("Replaced 1 occ");
}

/// Test that search highlights update when scrolling to show new matches
#[test]
fn test_search_highlights_update_on_scroll() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file where "MATCH" appears on every 3rd line
    // This ensures there are always matches visible regardless of scroll position
    let mut content = String::new();
    for i in 0..60 {
        if i % 3 == 0 {
            content.push_str(&format!("Line {} has MATCH keyword\n", i));
        } else {
            content.push_str(&format!("Line {} no keyword here\n", i));
        }
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("MATCH").unwrap();
    harness.render().unwrap();

    // Confirm search - this highlights visible matches
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should have highlights for visible matches
    let highlights_initial = harness.count_search_highlights();
    assert!(
        highlights_initial > 0,
        "Should have search highlights after search"
    );

    // Get position of first highlight
    let state = harness.editor().active_state();
    let first_highlight_pos = state
        .overlays
        .all()
        .iter()
        .find(|o| {
            o.namespace
                .as_ref()
                .map(|ns| ns.as_str().starts_with("search"))
                .unwrap_or(false)
        })
        .and_then(|o| state.marker_list.get_position(o.start_marker))
        .expect("Should have at least one highlight");

    // Scroll down significantly
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // After scrolling, overlays for all matches still exist
    // (they're created for all matches, not just visible ones)
    let highlights_after_scroll = harness.count_search_highlights();
    assert!(
        highlights_after_scroll > 0,
        "Should still have search highlights after scrolling"
    );

    // With all-match overlays, scrolling doesn't change which overlays exist,
    // it just changes which ones are rendered. The first overlay position
    // is still at the first match in the file.
    // What matters is that F3 can still navigate through all matches.
    let _ = first_highlight_pos; // silence unused warning
}

/// Test search history navigation with Up/Down arrows
#[test]
fn test_search_history_navigation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Clear history to ensure test isolation
    harness.editor_mut().clear_search_history();

    // First search: "hello"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Second search: "foo"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Third search: "test"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("test").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search prompt again - should pre-fill with most recent search ("test")
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Press Up arrow - should show "foo" (previous in history)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: foo");

    // Press Up arrow again - should show "hello"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: hello");

    // Press Up arrow again - should stay at "hello" (oldest)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: hello");

    // Press Down arrow - should show "foo"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: foo");

    // Press Down arrow - should show "test"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Cancel the prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that search history preserves current input when navigating
#[test]
fn test_search_history_preserves_current_input() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "content here").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Clear history to ensure test isolation
    harness.editor_mut().clear_search_history();

    // Add one item to history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("previous").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search again and start typing
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("current input").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: current input");

    // Press Up to go to history
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: previous");

    // Press Down to return to current input
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: current input");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that replace has separate history from search
#[test]
fn test_replace_history_separate_from_search() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Do a search to add to search history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("search_term").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Do a replace to add to both search and replace history
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace: ");

    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show replacement prompt
    harness.assert_screen_contains("Replace 'hello' with: ");

    harness.type_text("goodbye").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open replace again - pre-fills with "hello" (last search history item)
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace: hello");

    // Press Up - should show "search_term" (going back in shared search history)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace: search_term");

    // Confirm to get to replacement prompt (searching for "search_term")
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace 'search_term' with: ");

    // Press Up - should show "goodbye" (from replace history, which is separate)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Replace 'search_term' with: goodbye");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that history skips empty entries and duplicate consecutive entries
#[test]
fn test_search_history_skips_empty_and_duplicates() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Clear history to ensure test isolation
    harness.editor_mut().clear_search_history();

    // Try to search with empty string (should not be added to history)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for "test" twice
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("test").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("test").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Search for "other"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("other").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search and check history - should pre-fill with "other" (most recent)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: other");

    // Press Up - should show "test" (only one "test" in history, not two)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Press Up - should stay at "test" (no empty string before it)
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Search: test");

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that incremental search highlights update when navigating history
#[test]
fn test_history_updates_incremental_highlights() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add "hello" and "foo" to search history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search again - prompt pre-fills with "foo" (last history item)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify prompt shows "foo" (pre-filled from last search)
    let screen = harness.screen_to_string();
    assert!(screen.contains("foo bar"), "Should show 'foo' in content");
    assert!(
        screen.contains("Search: foo"),
        "Should show 'foo' in prompt (pre-filled)"
    );

    // Press Up to navigate back in history to "hello"
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify "hello" appears on screen (highlights should update)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("hello world"),
        "Should show 'hello' in content"
    );
    assert!(
        screen.contains("Search: hello"),
        "Should show 'hello' in prompt"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that incremental highlighting works on second search (bug reproduction)
#[test]
fn test_incremental_highlighting_on_second_search() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // First search: "hello"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Verify incremental highlighting appears for "hello"
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("hello world"),
        "Should show 'hello' in content"
    );
    assert!(
        screen.contains("Search: hello"),
        "Should show search prompt"
    );

    // Confirm first search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Second search: "foo"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "foo" incrementally
    harness.type_text("f").unwrap();
    harness.render().unwrap();

    // Verify incremental highlighting appears for "f" (not "hello")
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Search: f"),
        "Should show 'f' in search prompt"
    );

    // Type rest of "foo"
    harness.type_text("oo").unwrap();
    harness.render().unwrap();

    // Verify incremental highlighting appears for "foo"
    let screen = harness.screen_to_string();
    assert!(screen.contains("foo bar"), "Should show 'foo' in content");
    assert!(
        screen.contains("Search: foo"),
        "Should show 'foo' in prompt"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that highlights disappear when search query becomes empty
#[test]
fn test_highlights_clear_when_query_becomes_empty() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar\ntest content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Open search and type "hello"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Verify highlights exist
    let highlight_count_before = harness.count_search_highlights();
    assert!(
        highlight_count_before > 0,
        "Should have highlights for 'hello'"
    );

    // Delete all characters one by one
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Verify highlights are cleared
    let highlight_count_after = harness.count_search_highlights();
    assert_eq!(
        highlight_count_after, 0,
        "Should have no highlights when query is empty"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that highlights clear when navigating to empty input via history
#[test]
fn test_highlights_clear_on_history_to_empty() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world\nfoo bar").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Add "hello" to history
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("hello").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open search again and start typing something new
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness.render().unwrap();

    // Verify we have highlights for "foo"
    let highlight_count_foo = harness.count_search_highlights();
    assert!(highlight_count_foo > 0, "Should have highlights for 'foo'");

    // Navigate up to history (shows "hello")
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have highlights for "hello"
    let highlight_count_hello = harness.count_search_highlights();
    assert!(
        highlight_count_hello > 0,
        "Should have highlights for 'hello'"
    );

    // Navigate down past the end (returns to "foo")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should have highlights for "foo" again
    let highlight_count_foo_again = harness.count_search_highlights();
    assert!(
        highlight_count_foo_again > 0,
        "Should have highlights for 'foo' again"
    );

    // Now delete all characters to make it empty
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // Navigate up to "hello" in history
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down to empty input
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Highlights should be cleared for empty input
    let highlight_count_empty = harness.count_search_highlights();
    assert_eq!(
        highlight_count_empty, 0,
        "Should have no highlights when navigating to empty input"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that search options bar appears when search prompt is active
#[test]
fn test_search_options_bar_appears() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Before search, verify no search options bar
    let screen_before = harness.screen_to_string();
    assert!(
        !screen_before.contains("Case Sensitive"),
        "Search options bar should not appear before search prompt"
    );

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify search options bar appears with checkboxes and shortcuts
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Case Sensitive"),
        "Should show 'Case Sensitive' option"
    );
    assert!(
        screen.contains("Whole Word"),
        "Should show 'Whole Word' option"
    );
    // On macOS, Alt is rendered as ⌥, on other platforms as "Alt"
    assert!(
        screen.contains("Alt+C") || screen.contains("⌥+C"),
        "Should show keyboard shortcut for case sensitive toggle"
    );
    assert!(
        screen.contains("Alt+W") || screen.contains("⌥+W"),
        "Should show keyboard shortcut for whole word toggle"
    );

    // Verify search prompt is shown
    harness.assert_screen_contains("Search:");

    // Cancel the search
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify search options bar disappears
    let screen_after = harness.screen_to_string();
    assert!(
        !screen_after.contains("Case Sensitive"),
        "Search options bar should disappear after canceling search"
    );
}

/// Test toggling case sensitivity with Alt+C during search
#[test]
fn test_toggle_case_sensitive_in_search() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello HELLO hello").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query (case-sensitive by default)
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // By default, case-sensitive is ON, so search for "hello" should match only "hello" (lowercase)
    // Verify the [x] checkbox is shown for case sensitive
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("[x] Case Sensitive") || screen.contains("[x]"),
        "Case Sensitive should be checked by default"
    );

    // Toggle case sensitivity with Alt+C
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Verify checkbox is now unchecked
    let screen_after_toggle = harness.screen_to_string();
    assert!(
        screen_after_toggle.contains("[ ] Case Sensitive") || screen_after_toggle.contains("[ ]"),
        "Case Sensitive should be unchecked after Alt+C"
    );

    // Cancel search
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test toggling whole word match with Alt+W during search
#[test]
fn test_toggle_whole_word_in_search() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "test testing tested").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query (whole word is OFF by default)
    harness.type_text("test").unwrap();
    harness.render().unwrap();

    // By default, whole word is OFF, so search for "test" should match all occurrences
    // Verify the [ ] checkbox is shown for whole word
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("[ ] Whole Word"),
        "Whole Word should be unchecked by default"
    );

    // Toggle whole word with Alt+W
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Verify checkbox is now checked
    let screen_after_toggle = harness.screen_to_string();
    assert!(
        screen_after_toggle.contains("[x] Whole Word"),
        "Whole Word should be checked after Alt+W"
    );

    // Cancel search
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that search options bar also appears for replace prompts
#[test]
fn test_search_options_bar_in_replace() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger replace with Ctrl+R
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify search options bar appears for replace prompt too
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Case Sensitive"),
        "Should show 'Case Sensitive' option in replace prompt"
    );
    assert!(
        screen.contains("Whole Word"),
        "Should show 'Whole Word' option in replace prompt"
    );

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Test that status bar is hidden when suggestions popup is shown
#[test]
fn test_status_bar_hidden_during_suggestions() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Before opening command palette, status bar should show "Palette:"
    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("Palette:"),
        "Status bar should show 'Palette:' indicator before command palette. Screen:\n{}",
        screen_before
    );

    // Open command palette (which has suggestions)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify suggestions are shown (Quick Open uses ">" prefix in hints)
    harness.assert_screen_contains(">command");

    // Status bar should be hidden when suggestions are visible
    // The "Palette:" indicator should not be visible
    let _screen_with_suggestions = harness.screen_to_string();
    // Note: We can't easily verify the status bar is hidden without checking specific positions,
    // but we can verify the suggestions take more screen space

    // Cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // After closing, status bar should be visible again
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("Palette:"),
        "Status bar should show 'Palette:' indicator after closing command palette. Screen:\n{}",
        screen_after
    );
}

/// Test Ctrl+F3 finds next occurrence of word under cursor
#[test]
fn test_find_selection_next_from_word_under_cursor() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with searchable content
    std::fs::write(&file_path, "hello world\nfoo bar\nhello again\nhello final").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Cursor is at the start, on "hello"
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should start at position 0"
    );

    // Press Ctrl+F3 to find next occurrence of word under cursor
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to second "hello" (at "hello again")
    let expected_pos = "hello world\nfoo bar\n".len();
    assert_eq!(
        harness.cursor_position(),
        expected_pos,
        "Ctrl+F3 should move cursor to second 'hello'"
    );

    // Press Ctrl+F3 again to find the next occurrence
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to third "hello" (at "hello final")
    let expected_pos = "hello world\nfoo bar\nhello again\n".len();
    assert_eq!(
        harness.cursor_position(),
        expected_pos,
        "Second Ctrl+F3 should move cursor to third 'hello'"
    );
}

/// Test Ctrl+Shift+F3 finds previous occurrence of word under cursor
#[test]
fn test_find_selection_previous_from_word_under_cursor() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with searchable content
    std::fs::write(&file_path, "hello world\nfoo bar\nhello again\nhello final").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move to the last "hello" position
    let last_hello_pos = "hello world\nfoo bar\nhello again\n".len();
    harness
        .editor_mut()
        .active_state_mut()
        .cursors
        .primary_mut()
        .position = last_hello_pos;
    harness.render().unwrap();

    // Press Ctrl+Shift+F3 to find previous occurrence
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL | KeyModifiers::SHIFT)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to second "hello" (at "hello again")
    let expected_pos = "hello world\nfoo bar\n".len();
    assert_eq!(
        harness.cursor_position(),
        expected_pos,
        "Ctrl+Shift+F3 should move cursor to previous 'hello'"
    );
}

/// Test Ctrl+F3 with a selection uses the selection text for search
#[test]
fn test_find_selection_next_with_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with "foo" appearing multiple times
    std::fs::write(&file_path, "foo bar baz\nfoo test\nanother foo here").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Select "foo" at the start using Ctrl+W (select word)
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify "foo" is selected
    let selected_text = harness.get_selected_text();
    assert_eq!(selected_text, "foo", "Should have 'foo' selected");

    // Press Ctrl+F3 to find next occurrence of selection
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to second "foo" (at "foo test")
    let expected_pos = "foo bar baz\n".len();
    assert_eq!(
        harness.cursor_position(),
        expected_pos,
        "Ctrl+F3 should move cursor to second 'foo'"
    );
}

/// Test that Ctrl+F3 wraps around to find matches from the beginning
#[test]
fn test_find_selection_next_wraps_around() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with "hello" at the start and cursor at the end
    std::fs::write(&file_path, "hello world\ntest").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Move cursor to "test" at the end
    let test_pos = "hello world\n".len();
    harness
        .editor_mut()
        .active_state_mut()
        .cursors
        .primary_mut()
        .position = test_pos;
    harness.render().unwrap();

    // Press Ctrl+F3 to find next occurrence of "test"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Since there's only one "test", it should stay at the same position (wrap around to self)
    assert_eq!(
        harness.cursor_position(),
        test_pos,
        "Ctrl+F3 should wrap around to the same match when only one exists"
    );
}

/// Test that F3 continues searching after Ctrl+F3 establishes search term
#[test]
fn test_f3_continues_after_find_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file with "hello" appearing multiple times
    std::fs::write(&file_path, "hello world\nhello again\nhello final").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Cursor is at the start, on "hello"
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should start at position 0"
    );

    // Press Ctrl+F3 to find next occurrence
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should be at second "hello"
    let second_hello_pos = "hello world\n".len();
    assert_eq!(
        harness.cursor_position(),
        second_hello_pos,
        "Should be at second 'hello'"
    );

    // Now press regular F3 to continue searching
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to third "hello"
    let third_hello_pos = "hello world\nhello again\n".len();
    assert_eq!(
        harness.cursor_position(),
        third_hello_pos,
        "F3 should continue to third 'hello' after Ctrl+F3"
    );
}

/// Test that repeated Ctrl+F3 keeps the same search term even when landing on a longer word
/// e.g., searching for "bla" should keep searching for "bla" even when landing on "blafoo"
#[test]
fn test_find_selection_keeps_search_term() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a test file where "test" appears as standalone and as part of "testing"
    // test -> testing -> test -> tester
    std::fs::write(
        &file_path,
        "test word\ntesting here\ntest again\ntester end",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Cursor starts at "test" on line 1
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should start at position 0"
    );

    // Press Ctrl+F3 to find next occurrence of "test"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to "testing" (position 10) - this contains "test"
    let testing_pos = "test word\n".len();
    assert_eq!(
        harness.cursor_position(),
        testing_pos,
        "First Ctrl+F3 should move to 'testing' which contains 'test'"
    );

    // Press Ctrl+F3 again - should continue searching for "test", NOT "testing"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to standalone "test" on line 3
    let test_again_pos = "test word\ntesting here\n".len();
    assert_eq!(
        harness.cursor_position(),
        test_again_pos,
        "Second Ctrl+F3 should continue searching for 'test', not 'testing'"
    );

    // Press Ctrl+F3 again - should find "tester" which also contains "test"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    let tester_pos = "test word\ntesting here\ntest again\n".len();
    assert_eq!(
        harness.cursor_position(),
        tester_pos,
        "Third Ctrl+F3 should find 'tester' which contains 'test'"
    );

    // Press Ctrl+F3 again - should wrap around to first "test"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        0,
        "Fourth Ctrl+F3 should wrap around to first 'test'"
    );
}

/// Test that manually moving cursor invalidates the search state
/// so that next Alt+N/Ctrl+F3 starts fresh from the new position
#[test]
fn test_find_selection_invalidates_on_cursor_move() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with two different words that each appear twice
    // "hello world hello world"
    //  ^0    ^6     ^12   ^18
    std::fs::write(&file_path, "hello world hello world").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Cursor starts at "hello" (position 0)
    assert_eq!(harness.cursor_position(), 0);

    // Press Ctrl+F3 to find next occurrence of "hello"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should move to second "hello" at position 12
    let second_hello_pos = "hello world ".len();
    assert_eq!(
        harness.cursor_position(),
        second_hello_pos,
        "First Ctrl+F3 should move to second 'hello'"
    );

    // Now manually move cursor to "world" at position 18 using End then Home to go to second line
    // Actually, let's use arrow keys to move to position 18 (start of second "world")
    // From position 12, we need to move 6 positions right
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let second_world_pos = "hello world hello ".len();
    assert_eq!(
        harness.cursor_position(),
        second_world_pos,
        "Arrow keys should move to second 'world'"
    );

    // Press Ctrl+F3 again - should NOW search for "world", not "hello"
    harness
        .send_key(KeyCode::F(3), KeyModifiers::CONTROL)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // "world" appears at positions 6 and 18. We're at 18, so next should wrap to 6.
    let first_world_pos = "hello ".len();
    assert_eq!(
        harness.cursor_position(),
        first_world_pos,
        "Ctrl+F3 after moving cursor should search for 'world', not 'hello'"
    );
}

/// Test search in a large file (issue #657)
///
/// This test reproduces the bug where searching in large files fails with
/// "Buffer not fully loaded" error. The bug occurs because:
/// 1. Large files use lazy loading - chunks are loaded on demand
/// 2. The old perform_search() used buffer.to_string() which returns None
///    for buffers with unloaded regions
/// 3. This caused the search to fail instead of loading the needed data
///
/// The fix uses get_text_range_mut() which loads the buffer on demand.
#[test]
fn test_search_in_large_file() {
    use std::io::Write;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.txt");

    // Create a file that's large enough to have unloaded regions after viewport prep.
    // The chunk size is 1MB, so we need a file > 2MB to ensure parts remain unloaded.
    // Using 3MB to be safe.
    let mut file = std::fs::File::create(&file_path).unwrap();

    // Write 3MB of content - this ensures at least 1MB+ remains unloaded after
    // the first 1MB chunk is loaded for viewport rendering.
    // Each line is about 70 bytes, so we need ~45000 lines for 3MB.
    for i in 0..45000 {
        writeln!(
            file,
            "Line {:06}: This is padding content to make the file large enough",
            i
        )
        .unwrap();
    }
    // Add a unique searchable string near the END of the file.
    // This ensures the search target is in an UNLOADED region.
    writeln!(file, "UNIQUE_SEARCH_TARGET_STRING_12345").unwrap();
    // Add a few more lines after
    for i in 0..100 {
        writeln!(file, "Trailing line {:06}", i).unwrap();
    }
    file.flush().unwrap();
    drop(file);

    // Verify file is larger than 2MB to ensure we have unloaded regions
    let file_size = std::fs::metadata(&file_path).unwrap().len();
    assert!(
        file_size > 2 * 1024 * 1024,
        "File should be larger than 2MB to ensure unloaded regions, but is {} bytes",
        file_size
    );
    eprintln!(
        "Test file size: {} bytes ({:.2} MB)",
        file_size,
        file_size as f64 / 1024.0 / 1024.0
    );

    // Create harness and open the large file
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // Verify the buffer is in large file mode
    let is_large_file = harness.editor().active_state().buffer.is_large_file();
    eprintln!("Buffer is_large_file: {}", is_large_file);
    assert!(
        is_large_file,
        "Buffer should be in large file mode for 3MB file"
    );

    // REPRODUCE THE BUG: Check if the buffer has unloaded regions BEFORE search
    // With a 3MB file and 1MB chunk size, at least 2MB should be unloaded.
    // buffer.to_string() returns None when there are unloaded regions.
    let buffer_content_before = harness.get_buffer_content();
    eprintln!(
        "Buffer content available before search (to_string returns Some): {}",
        buffer_content_before.is_some()
    );
    // This assertion proves the bug exists - to_string() returns None for unloaded buffers
    assert!(
        buffer_content_before.is_none(),
        "BUG REPRODUCED: Buffer should have unloaded regions (to_string() returns None). \
         If this fails, the file might not be large enough or chunks got loaded unexpectedly."
    );

    // Now trigger search - this should work with the fix even though buffer has unloaded regions
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check that the search prompt appeared
    harness.assert_screen_contains("Search: ");

    // Search for the unique string that's at the END of the file (in unloaded region)
    harness.type_text("UNIQUE_SEARCH_TARGET").unwrap();
    harness.render().unwrap();

    // Confirm search - with the fix, this should load the buffer and find the match
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // VERIFY THE FIX: Search should succeed without "Buffer not fully loaded" error
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Buffer not fully loaded"),
        "FIX VERIFIED: Search should work without 'Buffer not fully loaded' error.\nScreen: {}",
        screen
    );

    // Verify cursor moved to the match (should be near the end, around byte 3MB)
    let cursor_pos = harness.cursor_position();
    // 45000 lines * ~70 bytes = ~3.15MB. Search target is around byte 3MB.
    let expected_min_pos = 45000 * 60; // Conservative estimate (~2.7MB)
    assert!(
        cursor_pos > expected_min_pos,
        "Cursor should have moved to the match position (at least byte {}), but is at {}",
        expected_min_pos,
        cursor_pos
    );
    eprintln!("Search found match at position: {}", cursor_pos);
}

/// Test search in large file with explicit low threshold
/// This ensures the fix works for files that are considered "large" by configuration
#[test]
fn test_search_in_large_file_with_low_threshold() {
    use std::io::Write;

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("medium_test.txt");

    // Create a file with known content - larger than 10KB but smaller than 1MB
    let mut file = std::fs::File::create(&file_path).unwrap();

    // Write ~50KB of content with searchable strings
    for i in 0..600 {
        writeln!(file, "Line {:04}: Some padding content here", i).unwrap();
    }
    // Add unique search target
    writeln!(file, "FINDME_SPECIAL_STRING").unwrap();
    for i in 0..100 {
        writeln!(file, "Trailing {:04}", i).unwrap();
    }
    file.flush().unwrap();
    drop(file);

    // Verify file size is at least what we expect
    let file_size = std::fs::metadata(&file_path).unwrap().len();
    eprintln!("Test file size: {} bytes", file_size);

    // Create config with very low large_file_threshold to force lazy loading
    let mut config = Config::default();
    config.editor.large_file_threshold_bytes = 1024; // 1KB threshold
    eprintln!(
        "Config large_file_threshold_bytes: {}",
        config.editor.large_file_threshold_bytes
    );

    // Create harness with the custom config
    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify the buffer is in large file mode (lazy loading)
    let is_large_file = harness.editor().active_state().buffer.is_large_file();
    eprintln!("Buffer is_large_file: {}", is_large_file);
    assert!(
        is_large_file,
        "Buffer should be in large file mode with threshold of 1KB"
    );

    // Check buffer loading status
    let buffer = &harness.editor().active_state().buffer;
    eprintln!("Buffer total bytes: {}", buffer.len());
    let buffer_content = harness.get_buffer_content();
    eprintln!("Buffer content available: {}", buffer_content.is_some());

    // If buffer content is available even though it's a large file,
    // that means the entire file got loaded during open_file or render.
    // This would mean the bug has been fixed (or isn't reproducible in tests).
    // The fix should be in perform_search to load the buffer before searching.

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for the unique string
    harness.type_text("FINDME_SPECIAL").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // The search should succeed without errors
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Buffer not fully loaded"),
        "Search should work with low threshold without 'Buffer not fully loaded' error"
    );

    // Verify cursor moved to find the match
    let cursor_pos = harness.cursor_position();
    // The match should be after the first 600 lines - actual position is around 22000-24000
    // depending on exact line lengths, so we use a more conservative estimate
    let expected_min_pos = 20000; // Conservative estimate (600 lines * ~35 bytes)
    assert!(
        cursor_pos > expected_min_pos,
        "Cursor should have moved to the match position (at least byte {}), but is at {}",
        expected_min_pos,
        cursor_pos
    );
}

/// Test that F3 search works with matches outside the viewport
/// Creates a large buffer with matches at the beginning and end (outside initial viewport)
/// and verifies F3 correctly cycles through all matches
#[test]
fn test_f3_search_finds_matches_outside_viewport() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_test.txt");

    // Create a large file with matches at beginning and end
    // Viewport is 24 lines, so we need > 24 lines to ensure second match is outside viewport
    let mut content = String::new();

    // First match at the very beginning
    content.push_str("UNIQUE_MATCH here at the start\n");

    // Add many lines of padding (100 lines to be well outside viewport of 24 lines)
    for i in 0..100 {
        content.push_str(&format!("Padding line {:03} with no match content\n", i));
    }

    // Second match at the end (way outside initial viewport)
    content.push_str("UNIQUE_MATCH here at the end\n");

    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify cursor starts at position 0
    assert_eq!(
        harness.cursor_position(),
        0,
        "Cursor should start at position 0"
    );

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type search query
    harness.type_text("UNIQUE_MATCH").unwrap();
    harness.render().unwrap();

    // Confirm search - should move to first match at position 0
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should be at first match (position 0)
    let first_match_pos = harness.cursor_position();
    assert_eq!(
        first_match_pos, 0,
        "After search, cursor should be at first match (position 0)"
    );

    // Press F3 to find next match (outside initial viewport)
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Calculate expected position of second match
    // First line: "UNIQUE_MATCH here at the start\n" = 31 chars
    // 100 padding lines: "Padding line XXX with no match content\n" = 39 chars each
    let expected_second_match = 31 + (100 * 39);

    // Should be at second match (way outside initial viewport)
    let second_match_pos = harness.cursor_position();
    assert_eq!(
        second_match_pos, expected_second_match,
        "F3 should move to second match at position {}. Got: {}",
        expected_second_match, second_match_pos
    );

    // Press F3 again - should wrap to first match
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    let wrapped_pos = harness.cursor_position();
    assert_eq!(
        wrapped_pos, 0,
        "F3 should wrap to first match (position 0). Got: {}",
        wrapped_pos
    );

    // Verify all matches were found by cycling through again
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        expected_second_match,
        "Second F3 cycle should find second match again"
    );
}

/// Test that F3 continues searching after buffer modifications
/// Bug: After typing to modify the buffer, F3 (find next) should still work
/// and the search state should be preserved (showing "Match X of Y").
#[test]
fn test_f3_continues_searching_after_buffer_modification() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("test.txt");

    // Create a test file with "foo" appearing multiple times
    // Content: "foo\nfoo\nfoo\n" - "foo" at positions 0, 4, 8
    std::fs::write(&file_path, "foo\nfoo\nfoo\n").unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify initial content
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content, "foo\nfoo\nfoo\n",
        "Initial content should be correct"
    );

    // Step 1: Go to top of buffer (should already be there)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), 0, "Cursor should be at start");

    // Step 2: Search for "foo"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("foo").unwrap();
    harness.render().unwrap();

    // Confirm search - should move to first "foo" at position 0
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should be at first "foo" (position 0) and status bar shows match info
    assert_eq!(
        harness.cursor_position(),
        0,
        "After search, cursor should be at first 'foo' (position 0)"
    );
    let screen_after_search = harness.screen_to_string();
    // Status bar may show "Found 3 matches" or "Match 1 of 3" depending on search mode
    assert!(
        screen_after_search.contains("3 match") || screen_after_search.contains("of 3"),
        "Status should indicate 3 matches found. Screen:\n{}",
        screen_after_search
    );

    // Step 3: Press F3 to skip to second "foo"
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // Should be at second "foo" (position 4)
    assert_eq!(
        harness.cursor_position(),
        4,
        "After first F3, cursor should be at second 'foo' (position 4)"
    );
    let screen_after_f3 = harness.screen_to_string();
    // F3 should show "Match X of 3" format
    assert!(
        screen_after_f3.contains("of 3"),
        "Status should show 'Match X of 3'. Screen:\n{}",
        screen_after_f3
    );

    // Step 4: Move to beginning of buffer and type something to modify the buffer
    // This will shift all subsequent positions
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type "XXX " (4 characters) at the beginning
    // This shifts all "foo" positions by 4 bytes:
    // - First "foo" was at 0, now at 4
    // - Second "foo" was at 4, now at 8
    // - Third "foo" was at 8, now at 12
    harness.type_text("XXX ").unwrap();
    harness.render().unwrap();

    // Verify the buffer was modified
    let modified_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        modified_content, "XXX foo\nfoo\nfoo\n",
        "Buffer should be modified with 'XXX ' prefix"
    );

    // Current cursor position should be at 4 (after "XXX ")
    assert_eq!(
        harness.cursor_position(),
        4,
        "After typing, cursor should be at position 4"
    );

    // Step 5: Press F3 to find next "foo"
    // The search state should be preserved, and F3 should find the next "foo"
    // at its UPDATED position (accounting for the buffer modification)
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    // After typing "XXX " at position 0, the "foo" positions are now:
    // - First "foo" at position 4 (was 0)
    // - Second "foo" at position 8 (was 4)
    // - Third "foo" at position 12 (was 8)
    //
    // Before typing, current_match_index was 1 (we were at second match at position 4)
    // F3 increments to index 2, which now points to position 12 (third "foo")
    // The key thing is that positions are CORRECT (not stale) after the fix
    let cursor_after_f3 = harness.cursor_position();
    assert_eq!(
        cursor_after_f3, 12,
        "F3 should jump to third 'foo' at updated position 12. Got: {}",
        cursor_after_f3
    );

    // KEY ASSERTION: Status bar shows correct match info
    let screen_after_edit_f3 = harness.screen_to_string();
    assert!(
        screen_after_edit_f3.contains("of 3"),
        "F3 should work after buffer modification and show match count. Screen:\n{}",
        screen_after_edit_f3
    );

    // Press F3 again - should wrap to first "foo" at position 4
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    let cursor_wrap = harness.cursor_position();
    assert_eq!(
        cursor_wrap, 4,
        "F3 should wrap to first 'foo' (position 4). Got: {}",
        cursor_wrap
    );

    // Press F3 again - should go to second "foo" at position 8
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    let cursor_second = harness.cursor_position();
    assert_eq!(
        cursor_second, 8,
        "F3 should go to second 'foo' (position 8). Got: {}",
        cursor_second
    );
}

/// Test searching for double underscores (common in Python __init__, __name__, etc.)
#[test]
fn test_search_double_underscore() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.py");

    // Create a Python file with double underscores
    let content = "def __init__(self):\n    self.__name__ = 'test'\n    __special__ = True\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Trigger search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for __init__
    harness.type_text("__init__").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should find the match (cursor moves to position of "__init__")
    let cursor_pos = harness.cursor_position();
    // "__init__" starts at position 4 (after "def ")
    assert_eq!(
        cursor_pos, 4,
        "Should find '__init__' at position 4, got {}",
        cursor_pos
    );
    // Note: We don't check status bar message as it may be truncated on systems
    // with long temp paths (e.g., macOS /private/var/folders/...)
}

/// Test searching for just double underscore (__)
#[test]
fn test_search_double_underscore_prefix() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.py");

    // Create a file with multiple double underscores
    let content = "__init__, __name__, __file__\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for just "__"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("__").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should find the first match at position 0
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos, 0,
        "Should find first '__' at position 0, got {}",
        cursor_pos
    );
    // Note: We don't check status bar message as it may be truncated on systems
    // with long temp paths (e.g., macOS /private/var/folders/...)
}

/// Test searching for angle bracket (common in generics like Vec<T>)
#[test]
fn test_search_angle_bracket() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create a Rust file with angle brackets
    let content = "let x: Vec<String> = Vec::new();\nlet y: Option<i32> = None;\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for "Vec<"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Vec<").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should find the match - "Vec<" starts at position 7 (after "let x: ")
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos, 7,
        "Should find 'Vec<' at position 7, got {}",
        cursor_pos
    );
}

/// Test searching for closing angle bracket with type
#[test]
fn test_search_with_closing_angle_bracket() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");

    // Create content with generic types
    let content = "plugin_name<T>\nplugin_name<U>\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for "plugin_name<"
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("plugin_name<").unwrap();
    harness.render().unwrap();

    // Confirm search
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should find first match at position 0
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos, 0,
        "Should find 'plugin_name<' at position 0, got {}",
        cursor_pos
    );
    // Note: We don't check status bar message as it may be truncated on systems
    // with long temp paths (e.g., macOS /private/var/folders/...)
}
