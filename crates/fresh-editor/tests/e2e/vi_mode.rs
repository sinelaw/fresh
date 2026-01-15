//! End-to-end tests for Vi mode
//!
//! Tests the vi mode plugin functionality including:
//! - Basic navigation (h, j, k, l, w, b)
//! - Mode switching (i, Escape)
//! - Operators with motions (dw, dd, etc.)
//!
//! Note: These tests require the vi mode plugin to be loaded.

use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::input::keybindings::Action::PluginAction;
use std::fs;

/// Create a harness with vi mode plugin loaded (uses real plugins/vi_mode.ts)
fn vi_mode_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy vi_mode plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    copy_plugin_lib(&plugins_dir);

    // Create harness with the project directory (so plugins load)
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Enable internal-only clipboard to isolate tests from each other
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Return temp_dir to keep it alive during the test
    (harness, temp_dir)
}

/// Helper to enable vi mode via command palette
fn enable_vi_mode(harness: &mut EditorTestHarness) {
    // Wait for vi_mode plugin command to be registered (check by action name, which is stable)
    harness
        .wait_until(|h| {
            let commands = h.editor().command_registry().read().unwrap().get_all();
            commands
                .iter()
                .any(|c| c.action == PluginAction("vi_mode_toggle".to_string()))
        })
        .unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type the localized command name
    harness.type_text("Toggle Vi mode").unwrap();

    // Wait for command to appear in palette
    harness.wait_for_screen_contains("Toggle Vi mode").unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for vi mode to be enabled (semantic: editor_mode is set to vi-normal)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

// =============================================================================
// Basic Navigation Tests
// =============================================================================

/// Test h, j, k, l navigation in vi normal mode
#[test]
fn test_vi_hjkl_navigation() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    // Create a multi-line test file
    let fixture = TestFixture::new("test.txt", "abc\ndef\nghi\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable vi mode
    enable_vi_mode(&mut harness);

    // Verify vi mode is enabled (semantic check)
    assert_eq!(
        harness.editor().editor_mode(),
        Some("vi-normal".to_string())
    );

    // Get initial cursor position (should be 0)
    let initial_pos = harness.cursor_position();
    assert_eq!(initial_pos, 0, "Expected initial cursor at position 0");

    // Test 'l' (move right) - wait for cursor to move (semantic waiting)
    harness
        .send_key(KeyCode::Char('l'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > initial_pos)
        .unwrap();

    // Test 'j' (move down) - wait for cursor to move to next line
    let pos_before_j = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_j)
        .unwrap();

    // Test 'k' (move up) - wait for cursor to move back
    let pos_before_k = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() < pos_before_k)
        .unwrap();

    // Test 'h' (move left) - wait for cursor to return to start
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() == initial_pos)
        .unwrap();

    // Verify file content is unchanged (no 'l', 'j', 'k', 'h' characters inserted)
    harness.assert_buffer_content("abc\ndef\nghi\n");
}

/// Test w and b word navigation
#[test]
fn test_vi_word_navigation() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello world test\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Test 'w' (move to next word) - wait for cursor to move (semantic waiting)
    let pos0 = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.wait_until(|h| h.cursor_position() > pos0).unwrap();

    let pos1 = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.wait_until(|h| h.cursor_position() > pos1).unwrap();

    // Test 'b' (move to previous word) - wait for cursor to move back
    let pos2 = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.wait_until(|h| h.cursor_position() < pos2).unwrap();

    // Content should be unchanged
    harness.assert_buffer_content("hello world test\n");
}

// =============================================================================
// Mode Switching Tests
// =============================================================================

/// Test switching from normal to insert mode with 'i'
#[test]
fn test_vi_insert_mode() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Verify we're in normal mode (semantic check)
    assert_eq!(
        harness.editor().editor_mode(),
        Some("vi-normal".to_string())
    );

    // Enter insert mode with 'i'
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for insert mode (semantic check)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    // Type some text
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Return to normal mode with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for normal mode (semantic check)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Verify text was inserted (semantic waiting)
    harness.wait_for_buffer_content("Xhello\n").unwrap();
}

/// Test 'a' inserts after cursor
#[test]
fn test_vi_insert_after() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "abc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Press 'a' to insert after cursor
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for insert mode (semantic check)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    // Type some text
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Return to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for normal mode (semantic check)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // 'X' should be inserted after 'a' (semantic waiting)
    harness.wait_for_buffer_content("aXbc\n").unwrap();
}

/// Test 'o' opens line below
#[test]
fn test_vi_open_below() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "line1\nline2\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Press 'o' to open line below
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for insert mode (semantic check)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    // Type some text
    harness.type_text("new line").unwrap();
    harness.render().unwrap();

    // Return to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for normal mode (semantic check)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Verify new line was inserted (semantic waiting)
    harness
        .wait_for_buffer_content("line1\nnew line\nline2\n")
        .unwrap();
}

// =============================================================================
// Operator Tests
// =============================================================================

/// Test 'x' deletes character under cursor
#[test]
fn test_vi_delete_char() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "abc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Delete first character with 'x'
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for delete to complete (semantic waiting)
    harness.wait_for_buffer_content("bc\n").unwrap();
}

/// Test 'dd' deletes entire line
#[test]
fn test_vi_delete_line() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "line1\nline2\nline3\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Delete line with 'dd'
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    // Wait for operator-pending mode before sending second key
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for delete to complete (semantic waiting)
    harness.wait_for_buffer_content("line2\nline3\n").unwrap();
}

/// Test 'dw' deletes to next word (operator + motion composability)
#[test]
fn test_vi_delete_word() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello world test\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Delete word with 'dw'
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    // Wait for operator-pending mode before sending motion
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "hello " should be deleted (semantic waiting)
    harness.wait_for_buffer_content("world test\n").unwrap();
}

// =============================================================================
// Undo/Redo Tests
// =============================================================================

/// Test 'u' undoes last change
#[test]
fn test_vi_undo() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "abc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Delete a character (semantic waiting)
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.wait_for_buffer_content("bc\n").unwrap();

    // Undo with 'u' - use semantic wait since undo is async
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for undo to complete (semantic waiting per README guidelines)
    harness.wait_for_buffer_content("abc\n").unwrap();
}

// =============================================================================
// Yank/Paste Tests
// =============================================================================

/// Test 'yy' yanks line and 'p' pastes it below
#[test]
fn test_vi_yank_paste_line() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "AAA\nBBB\nCCC\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // First y enters operator-pending mode
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for operator-pending mode
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();

    // Second y completes the yy command (yank line)
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait to return to normal mode (yy is complete)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Paste below with 'p'
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // AAA should be duplicated on line 2 (semantic waiting)
    harness
        .wait_for_buffer_content("AAA\nAAA\nBBB\nCCC\n")
        .unwrap();
}

/// Test 'P' pastes line above current line
#[test]
fn test_vi_paste_before_line() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "AAA\nBBB\nCCC\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Move to line 2 (BBB) - wait for cursor to move (semantic waiting)
    let pos_before_j = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_j)
        .unwrap();

    // Yank line with 'yy'
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    // Wait for operator-pending mode before sending second key
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    // Wait for mode to return to normal after yy completes
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Paste above with 'P'
    harness
        .send_key(KeyCode::Char('P'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // BBB should be inserted above the current line (semantic waiting)
    harness
        .wait_for_buffer_content("AAA\nBBB\nBBB\nCCC\n")
        .unwrap();
}

/// Test 'v' enters visual mode and 'd' deletes selection
#[test]
fn test_vi_visual_delete() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Enter visual mode with 'v' - wait for mode change (semantic waiting)
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-visual".to_string()))
        .unwrap();

    // Extend selection with 'w' (select word) - wait for cursor to move
    let pos_before_w = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_w)
        .unwrap();

    // Delete with 'd'
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "hello " should be deleted, leaving "world" (semantic waiting)
    harness.wait_for_buffer_content("world\n").unwrap();
}

/// Test 'V' enters visual line mode and 'd' deletes line
#[test]
fn test_vi_visual_line_delete() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "AAA\nBBB\nCCC\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Move to line 2 (BBB) - wait for cursor to move (semantic waiting)
    let pos_before_j = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_j)
        .unwrap();

    // Enter visual line mode with 'V' - wait for mode change
    harness
        .send_key(KeyCode::Char('V'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-visual-line".to_string()))
        .unwrap();

    // Delete with 'd'
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // BBB line should be deleted (semantic waiting)
    harness.wait_for_buffer_content("AAA\nCCC\n").unwrap();
}

/// Test visual mode yank and paste
#[test]
fn test_vi_visual_yank() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Enter visual mode with 'v' - wait for mode change (semantic waiting)
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-visual".to_string()))
        .unwrap();

    // Extend selection with 'e' (to end of word) - wait for cursor to move
    let pos_before_e = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_e)
        .unwrap();

    // Yank with 'y' - wait for mode to return to normal
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Move to start of "world" - wait for cursor to move
    let pos_before_w = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_w)
        .unwrap();

    // Paste with 'p' (inserts after cursor)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    // "hello" should be pasted after 'w' in "world" (semantic waiting)
    harness
        .wait_for_buffer_content("hello whelloorld\n")
        .unwrap();
}

/// Test 'diw' deletes inner word
#[test]
fn test_vi_delete_inner_word() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello world test\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Move to "world" (w moves to start of next word) - semantic waiting
    let pos_before_w = harness.cursor_position();
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.cursor_position() > pos_before_w)
        .unwrap();

    // diw = delete inner word
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    // Wait for operator-pending mode
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    // Wait for text-object mode
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-text-object".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "world" should be deleted, leaving "hello  test"
    // Use wait_for_buffer_content since text object operations are async
    harness.wait_for_buffer_content("hello  test\n").unwrap();
}

/// Test 'ci"' changes inside quotes
#[test]
fn test_vi_change_inner_quotes() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "say \"hello world\" here\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Store initial position
    let initial_pos = harness.cursor_position();

    // Move into the quoted string with fh (find 'h')
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for find-char mode
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-find-char".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for cursor to move from initial position (semantic waiting)
    harness
        .wait_until(|h| h.cursor_position() > initial_pos)
        .unwrap();

    // ci" = change inner quotes
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for operator-pending mode
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for text-object mode
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-text-object".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('"'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for insert mode (ci" deletes content and enters insert)
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    // Now in insert mode, type replacement
    harness.type_text("Hi").unwrap();
    harness.render().unwrap();

    // Escape back to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Content inside quotes replaced with "Hi" (semantic waiting)
    harness
        .wait_for_buffer_content("say \"Hi\" here\n")
        .unwrap();
}

// =============================================================================
// Colon Command Tests
// =============================================================================

/// Test ':w' saves the file
#[test]
fn test_vi_colon_write() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Enter insert mode and make a change
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Return to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Verify file is modified
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "Buffer should be modified"
    );

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear (semantic waiting)
    harness.wait_for_prompt().unwrap();

    // Type 'w' and press Enter
    harness.type_text("w").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for file to be saved (buffer no longer modified) - semantic waiting
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();
}

/// Test ':q' closes buffer (via status message confirmation)
#[test]
fn test_vi_colon_quit() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    // Open two files so we can close one and stay in editor
    let fixture1 = TestFixture::new("test1.txt", "file1\n").unwrap();
    let fixture2 = TestFixture::new("test2.txt", "file2\n").unwrap();
    harness.open_file(&fixture1.path).unwrap();
    harness.open_file(&fixture2.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Verify we're on test2.txt
    harness.wait_for_screen_contains("test2.txt").unwrap();

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear (semantic waiting)
    harness.wait_for_prompt().unwrap();

    // Type 'q' and press Enter
    harness.type_text("q").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for buffer to close - we should now see test1.txt
    harness.wait_for_screen_contains("test1.txt").unwrap();
}

/// Test ':q!' force quits even with unsaved changes
#[test]
fn test_vi_colon_force_quit() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    // Open two files so we can close one and stay in editor
    let fixture1 = TestFixture::new("test1.txt", "file1\n").unwrap();
    let fixture2 = TestFixture::new("test2.txt", "file2\n").unwrap();
    harness.open_file(&fixture1.path).unwrap();
    harness.open_file(&fixture2.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Make a change to create unsaved modifications
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Return to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Verify we're on test2.txt
    harness.wait_for_screen_contains("test2.txt").unwrap();

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear
    harness.wait_for_prompt().unwrap();

    // Type 'q!' and press Enter (force quit)
    harness.type_text("q!").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for buffer to close - we should now see test1.txt
    harness.wait_for_screen_contains("test1.txt").unwrap();
}

/// Test ':wq' saves and quits
#[test]
fn test_vi_colon_write_quit() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    // Open two files so we can close one and stay in editor
    let fixture1 = TestFixture::new("test1.txt", "file1\n").unwrap();
    let fixture2 = TestFixture::new("test2.txt", "file2\n").unwrap();
    harness.open_file(&fixture1.path).unwrap();
    harness.open_file(&fixture2.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Make a change
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Return to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Verify we're on test2.txt
    harness.wait_for_screen_contains("test2.txt").unwrap();

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear
    harness.wait_for_prompt().unwrap();

    // Type 'wq' and press Enter
    harness.type_text("wq").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for buffer to close - we should now see test1.txt
    harness.wait_for_screen_contains("test1.txt").unwrap();

    // Wait for file to be written to disk using semantic waiting
    // The file write is asynchronous, so we poll until the content appears
    let fixture2_path = fixture2.path.clone();
    harness
        .wait_until(move |_h| {
            fs::read_to_string(&fixture2_path)
                .map(|content| content.contains("X"))
                .unwrap_or(false)
        })
        .expect("Saved file should contain the change");
}

/// Test ':35' goes to line 35 and edits happen at the correct position
#[test]
fn test_vi_colon_goto_line() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    // Create a file with 50 lines - line 35 won't be visible initially
    let content = (1..=50)
        .map(|i| format!("line_{:02}_content\n", i))
        .collect::<String>();
    let fixture = TestFixture::new("test.txt", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear
    harness.wait_for_prompt().unwrap();

    // Type '35' and press Enter to go to line 35
    harness.type_text("35").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Enter insert mode and type "INSERTED_" at the beginning of line 35
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-insert".to_string()))
        .unwrap();

    harness.type_text("INSERTED_").unwrap();
    harness.render().unwrap();

    // Return to normal mode
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();

    // Verify the complete modified line is visible - proves goto AND edit worked
    harness
        .wait_for_screen_contains("INSERTED_line_35_content")
        .unwrap();
}

/// Test ':bn' goes to next buffer
#[test]
fn test_vi_colon_buffer_next() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    // Open two files
    let fixture1 = TestFixture::new("test1.txt", "file1\n").unwrap();
    let fixture2 = TestFixture::new("test2.txt", "file2\n").unwrap();
    harness.open_file(&fixture1.path).unwrap();
    harness.open_file(&fixture2.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Wait to see test2.txt is the active buffer
    harness.wait_for_screen_contains("test2.txt").unwrap();

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear
    harness.wait_for_prompt().unwrap();

    // Type 'bn' and press Enter
    harness.type_text("bn").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for buffer to change to test1.txt
    harness.wait_for_screen_contains("test1.txt").unwrap();
}

/// Test ':sp' creates horizontal split
#[test]
fn test_vi_colon_split() {
    let (mut harness, _temp_dir) = vi_mode_harness(80, 24);

    let fixture = TestFixture::new("test.txt", "hello\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    enable_vi_mode(&mut harness);

    // Press ':' to enter command mode
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for prompt to appear
    harness.wait_for_prompt().unwrap();

    // Type 'sp' and press Enter
    harness.type_text("sp").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for split to be created - we should see "hello" twice (semantic waiting)
    // Split creates a divider, so content should appear in both panes
    harness
        .wait_until(|h| {
            // Count occurrences of "hello" across all screen rows
            let mut count = 0;
            for row in 0..h.terminal_height() {
                let line = h.get_screen_row(row);
                if line.contains("hello") {
                    count += 1;
                }
            }
            count >= 2
        })
        .unwrap();
}
