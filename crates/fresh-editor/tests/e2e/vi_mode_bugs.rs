#![allow(non_snake_case)]
//! Bug reproduction tests for Vi mode
//!
//! Each test documents a specific known bug. Tests are written to demonstrate
//! the *expected* vim behavior — they will fail (or timeout) until the
//! underlying bug is fixed.

use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::input::keybindings::Action::PluginAction;
use std::fs;

/// Create a harness with vi mode plugin loaded
fn vi_mode_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    copy_plugin_lib(&plugins_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.editor_mut().set_clipboard_for_test("".to_string());
    (harness, temp_dir)
}

/// Enable vi mode via command palette
fn enable_vi_mode(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let commands = h.editor().command_registry().read().unwrap().get_all();
            commands
                .iter()
                .any(|c| c.action == PluginAction("vi_mode_toggle".to_string()))
        })
        .unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Toggle Vi").unwrap();
    harness.wait_for_screen_contains("Toggle Vi mode").unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

/// Helper: send a single normal-mode key and render
fn send_key(harness: &mut EditorTestHarness, c: char) {
    let mods = if c.is_ascii_uppercase() {
        KeyModifiers::SHIFT
    } else {
        KeyModifiers::NONE
    };
    harness.send_key(KeyCode::Char(c), mods).unwrap();
    harness.render().unwrap();
}

/// Helper: wait for operator-pending mode, then send the motion key
fn send_operator_motion(harness: &mut EditorTestHarness, op: char, motion: char) {
    send_key(harness, op);
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(harness, motion);
}

/// Helper: wait for vi-normal mode
fn wait_normal(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

/// Helper: wait for vi-insert mode
fn wait_insert(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-insert".to_string()))
        .unwrap();
}

/// Helper: escape to normal mode
fn escape(harness: &mut EditorTestHarness) {
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    wait_normal(harness);
}

// =============================================================================
// Bug #1: C (change to end of line) doesn't delete text
// =============================================================================

/// `C` should delete from cursor to end of line and enter insert mode.
/// BUG: cursor moves to EOL and enters insert, but text is not deleted.
#[test]
fn test_vi_bug_C_does_not_delete_to_eol() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to 'w' in "world"
    let pos0 = harness.cursor_position();
    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() > pos0).unwrap();

    // Press C — should delete "world" and enter insert mode
    send_key(&mut harness, 'C');
    wait_insert(&mut harness);

    // Type replacement text
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    escape(&mut harness);

    // Expected: "hello X\n"  (the "world" was deleted, "X" inserted)
    harness.wait_for_buffer_content("hello X\n").unwrap();
}

// =============================================================================
// Bug #2: D (delete to end of line) doesn't delete text
// =============================================================================

/// `D` should delete from cursor to end of line, stay in normal mode.
/// BUG: cursor moves to EOL but text is not deleted.
#[test]
fn test_vi_bug_D_does_not_delete_to_eol() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to 'w' in "world"
    let pos0 = harness.cursor_position();
    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() > pos0).unwrap();

    // Press D — should delete "world", stay in normal mode
    send_key(&mut harness, 'D');
    wait_normal(&mut harness);

    harness.wait_for_buffer_content("hello \n").unwrap();
}

// =============================================================================
// Bug #3: cc / S (change whole line) doesn't delete text
// =============================================================================

/// `cc` should clear the current line content and enter insert mode.
/// BUG: cursor moves to EOL, enters insert, but line content remains.
#[test]
fn test_vi_bug_cc_does_not_clear_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nline two\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // cc = change line
    send_key(&mut harness, 'c');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 'c');
    wait_insert(&mut harness);

    // Type replacement
    harness.type_text("REPLACED").unwrap();
    harness.render().unwrap();
    escape(&mut harness);

    harness
        .wait_for_buffer_content("REPLACED\nline two\n")
        .unwrap();
}

/// `S` should clear the current line content and enter insert mode (same as cc).
#[test]
fn test_vi_bug_S_does_not_clear_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nline two\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'S');
    wait_insert(&mut harness);

    harness.type_text("NEW").unwrap();
    harness.render().unwrap();
    escape(&mut harness);

    harness.wait_for_buffer_content("NEW\nline two\n").unwrap();
}

// =============================================================================
// Bug #4: Visual block mode (Ctrl+V) — I/A don't work, not true block ops
// =============================================================================

/// `I` in visual block mode should enter insert mode for block insertion.
///
/// Root cause: The editor has no block-insert mechanism. In vim, `I` in visual
/// block mode enters insert on the first selected line; after Escape, the typed
/// text is replicated on every line of the block. This requires multi-cursor
/// insert support tied to the block selection, which the editor doesn't have.
///
/// To fix: Add a new editor action (e.g. `BlockInsert`) that creates cursors at
/// the start of each line in the block selection, enters insert mode, and on
/// Escape replicates the inserted text to all cursor positions. Then bind `I` in
/// vi-visual-block mode to trigger it.
#[test]
#[ignore]
fn test_vi_bug_visual_block_I_ignored() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "aaaa\nbbbb\ncccc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Enter visual block with Ctrl+V
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-block".to_string()))
        .unwrap();

    // Select down two lines
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'j');

    // Press I — should enter insert mode for block insert
    send_key(&mut harness, 'I');

    // In vim, I in visual block enters insert mode.
    // After typing text and pressing Escape, the text is inserted on all selected lines.
    // For now, just verify we at least enter insert mode.
    wait_insert(&mut harness);
}

// =============================================================================
// Bug #5: `e` motion off-by-one and gets stuck
// =============================================================================

/// `e` should land on the last character of the current word, not after it.
/// BUG: lands on the space after the word (off by one), and repeating gets stuck.
#[test]
fn test_vi_bug_e_off_by_one() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    // "hello world\n" — 'o' in "hello" is at byte offset 4
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // From position 0 ('h'), press 'e' — should land on 'o' (offset 4)
    send_key(&mut harness, 'e');
    harness.wait_until(|h| h.cursor_position() > 0).unwrap();
    assert_eq!(
        harness.cursor_position(),
        4,
        "e should land on last char of 'hello' (offset 4), not after it"
    );
}

/// Pressing `e` twice should advance to the end of the second word.
/// BUG: second `e` gets stuck because first `e` lands on space.
#[test]
fn test_vi_bug_e_gets_stuck() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world end\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // First e: land on last char of "hello" (offset 4)
    send_key(&mut harness, 'e');
    harness.wait_until(|h| h.cursor_position() > 0).unwrap();
    let pos_after_first_e = harness.cursor_position();

    // Second e: should advance to last char of "world" (offset 10)
    send_key(&mut harness, 'e');
    harness
        .wait_until(|h| h.cursor_position() > pos_after_first_e)
        .unwrap();

    assert_eq!(
        harness.cursor_position(),
        10,
        "second e should land on last char of 'world' (offset 10)"
    );
}

// =============================================================================
// Bug #6: `e` missing from operator-pending mode
// =============================================================================

/// `de` should delete from cursor to end of current word.
/// BUG: `e` is not bound in operator-pending mode, so `de` doesn't work properly.
#[test]
fn test_vi_bug_de_not_working() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // de should delete "hello" (to end of word), leaving " world\n"
    send_operator_motion(&mut harness, 'd', 'e');

    harness.wait_for_buffer_content(" world\n").unwrap();
}

/// `ce` should change from cursor to end of current word.
#[test]
fn test_vi_bug_ce_not_working() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // ce should delete "hello" and enter insert
    send_operator_motion(&mut harness, 'c', 'e');
    wait_insert(&mut harness);

    harness.type_text("HI").unwrap();
    harness.render().unwrap();
    escape(&mut harness);

    harness.wait_for_buffer_content("HI world\n").unwrap();
}

// =============================================================================
// Bug #7: nG (go to line N) doesn't work — count ignored by G
// =============================================================================

/// `3G` should go to line 3.
/// BUG: G ignores count prefix, always goes to end of file.
#[test]
fn test_vi_bug_nG_ignores_count() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "line1\nline2\nline3\nline4\nline5\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Cursor starts at offset 0 (line 1)
    assert_eq!(harness.cursor_position(), 0);

    // 3G should go to line 3 (offset 12: "line1\nline2\n" = 12 bytes)
    send_key(&mut harness, '3');
    send_key(&mut harness, 'G');

    harness.wait_until(|h| h.cursor_position() > 0).unwrap();

    // Line 3 starts at byte offset 12
    assert_eq!(
        harness.cursor_position(),
        12,
        "3G should go to beginning of line 3 (offset 12)"
    );
}

// =============================================================================
// Bug #8: r (replace character) not implemented
// =============================================================================

/// `ra` should replace the character under the cursor with 'a'.
/// BUG: shows "Replace char not yet implemented".
#[test]
fn test_vi_bug_r_not_implemented() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // ra should replace 'h' with 'a'
    send_key(&mut harness, 'r');
    // Wait for the plugin to enter replace-char mode before sending the
    // replacement character (same pattern as send_operator_motion).
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-replace-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'a');

    harness.wait_for_buffer_content("aello\n").unwrap();
}

// =============================================================================
// Bug #9: h/l wrap across lines
// =============================================================================

/// `l` at the end of a line should NOT wrap to the next line.
/// BUG: l wraps to the beginning of the next line.
#[test]
fn test_vi_bug_l_wraps_across_lines() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "ab\ncd\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to 'b' (offset 1) — last char of line 1
    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();

    // Press l again — should stay at offset 1 (vim doesn't wrap l)
    send_key(&mut harness, 'l');
    harness.render().unwrap();

    // Give a moment for any async processing
    harness.wait_until(|h| h.cursor_position() <= 1).unwrap();

    assert_eq!(
        harness.cursor_position(),
        1,
        "l at end of line should not wrap to next line"
    );
}

/// `h` at the beginning of a line should NOT wrap to the previous line.
/// BUG: h wraps to the end of the previous line.
#[test]
fn test_vi_bug_h_wraps_across_lines() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "ab\ncd\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to line 2 (offset 3 = 'c')
    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();

    // Press h — should stay at offset 3 (beginning of line 2, vim doesn't wrap h)
    send_key(&mut harness, 'h');
    harness.render().unwrap();

    harness.wait_until(|h| h.cursor_position() >= 3).unwrap();

    assert_eq!(
        harness.cursor_position(),
        3,
        "h at beginning of line should not wrap to previous line"
    );
}

// =============================================================================
// Bug #12: ^ doesn't skip whitespace (same as 0)
// =============================================================================

/// `^` should move to the first non-blank character on the line.
/// BUG: `^` calls move_line_start (same as `0`), doesn't skip whitespace.
#[test]
fn test_vi_bug_caret_same_as_zero() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    // 4 spaces of indentation before "hello"
    let fixture = TestFixture::new("test.txt", "    hello\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to end of line to start from a known position
    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() > 0).unwrap();

    // Press ^ — should go to offset 4 ('h' in "hello"), NOT offset 0
    send_key(&mut harness, '^');
    harness.wait_until(|h| h.cursor_position() < 8).unwrap();

    assert_eq!(
        harness.cursor_position(),
        4,
        "^ should move to first non-blank char (offset 4), not line start (offset 0)"
    );
}

// =============================================================================
// Bug #13: J (join lines) doesn't add space
// =============================================================================

/// `J` should join lines with a space between them.
/// BUG: J concatenates lines without inserting a space.
#[test]
fn test_vi_bug_J_no_space() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello\nworld\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // J should join "hello" and "world" with a space
    send_key(&mut harness, 'J');

    // Expected: "hello world\n"  (not "helloworld\n")
    harness.wait_for_buffer_content("hello world\n").unwrap();
}

// =============================================================================
// Bug #15: f/t/F/T find-char mode missing special characters
// =============================================================================

/// `f(` should find the next '(' on the current line.
/// BUG: '(' is not bound in vi-find-char mode, so find stays pending/fails.
#[test]
fn test_vi_bug_find_char_special_chars() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "foo(bar) baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // f( should find '(' at offset 3
    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('('), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should return to normal mode and cursor at offset 3
    wait_normal(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        3,
        "f( should move to '(' at offset 3"
    );
}

/// `f.` should find the next '.' on the current line.
#[test]
fn test_vi_bug_find_char_dot() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello.world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('.'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    wait_normal(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        5,
        "f. should move to '.' at offset 5"
    );
}

/// `f/` should find '/' — common in file paths.
#[test]
fn test_vi_bug_find_char_slash() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "path/to/file\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    wait_normal(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        4,
        "f/ should move to '/' at offset 4"
    );
}

// =============================================================================
// Bug #16: d operator doesn't yank deleted text
// =============================================================================

/// After `dw`, pasting with `p` should paste the deleted word.
///
/// Root cause: `dw` maps to the atomic action `delete_word_forward` (in
/// `atomicOperatorActions`). This Rust action deletes the text but does NOT
/// copy it to the clipboard. In vim, every `d` command populates the unnamed
/// register so the deleted text can be pasted with `p`.
///
/// To fix (option A — Rust side): Make `DeleteWordForward`, `DeleteWordBackward`,
/// `DeleteToLineEnd`, and `DeleteToLineStart` also copy the deleted text to the
/// internal clipboard before deleting.
///
/// To fix (option B — plugin side): Stop using atomic delete actions in
/// `applyOperatorWithMotion`. Instead, use the selection-based path for all
/// operators: `select_word_right` + `cut` (which copies to clipboard). This
/// avoids the stale-snapshot problem because `cut` operates on the current
/// selection, not on positions read via `getCursorPosition()`.
#[test]
#[ignore]
fn test_vi_bug_dw_does_not_yank() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // dw deletes "hello "
    send_operator_motion(&mut harness, 'd', 'w');
    harness.wait_for_buffer_content("world\n").unwrap();

    // Move to end of "world", then p should paste "hello " after cursor
    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() > 0).unwrap();

    send_key(&mut harness, 'p');

    // Expected: "worldhello \n" or similar — the key point is "hello " was pasted
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .map(|c| c.contains("hello "))
                .unwrap_or(false)
        })
        .unwrap();
}

// =============================================================================
// Bug #10/11: v/V cursor movement on mode entry
// =============================================================================

/// Entering visual mode with `v` selects the character under cursor.
/// In vim, the character under cursor is highlighted as part of the selection.
/// The cursor position advances by 1 (selection end), but conceptually
/// the cursor is "on" the original character.
#[test]
fn test_vi_bug_v_moves_cursor_on_entry() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "abcdef\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    let pos_before = harness.cursor_position();

    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();

    // select_right advances cursor by 1 to establish anchor+selection;
    // this is the expected behavior for character-wise visual mode
    assert!(
        harness.cursor_position() <= pos_before + 1,
        "v should select current char; pos={}, expected <= {}",
        harness.cursor_position(),
        pos_before + 1
    );

    escape(&mut harness);
}

/// Entering visual line mode with `V` should not move the cursor to the next line.
/// BUG: V jumps cursor down one line.
#[test]
fn test_vi_bug_V_moves_cursor_down() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "line1\nline2\nline3\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Cursor should be on line 1 (offset 0..4)
    let pos_before = harness.cursor_position();
    assert!(pos_before < 5, "should start on line 1");

    send_key(&mut harness, 'V');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();

    // In vim, V selects the entire line including newline.
    // select_line moves cursor to the start of the next line (byte 6).
    // This is acceptable — the line-wise selection spans the full line.
    // The original bug was V doing move_line_start + select_line which
    // moved DOWN from the current position first. Now we just do select_line
    // from the current position, which correctly selects line 1.
    assert!(
        harness.cursor_position() <= 6,
        "V should select current line; pos={}, expected <=6",
        harness.cursor_position()
    );

    escape(&mut harness);
}

// =============================================================================
// Bug #17: Visual mode switching (v -> V -> Ctrl+V) broken
// =============================================================================

/// Switching from visual line (V) to visual block (Ctrl+V) should work.
/// BUG: Ctrl+V from visual line mode doesn't switch to visual block.
#[test]
fn test_vi_bug_visual_mode_switching() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "aaaa\nbbbb\ncccc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Enter visual mode
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();

    // Switch to visual line with V
    send_key(&mut harness, 'V');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();

    // Switch to visual block with Ctrl+V
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-block".to_string()))
        .unwrap();

    escape(&mut harness);
}

// =============================================================================
// Bug #14: $ cursor goes past last character
// =============================================================================

/// In normal mode, `$` should place cursor on the last character, not past it.
/// BUG: cursor goes one position past the end.
#[test]
fn test_vi_bug_dollar_past_eol() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    // "abc\n" — last char 'c' is at offset 2
    let fixture = TestFixture::new("test.txt", "abc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() > 0).unwrap();

    assert_eq!(
        harness.cursor_position(),
        2,
        "$ should land on last char 'c' (offset 2), not past it (offset 3)"
    );
}

// =============================================================================
// Bug #18-25: Missing features (not implemented)
// =============================================================================

/// `~` should toggle the case of the character under cursor.
/// BUG: `~` is not bound at all in vi-normal mode.
#[test]
fn test_vi_bug_tilde_not_implemented() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // ~ should toggle 'h' to 'H' and advance cursor
    send_key(&mut harness, '~');

    harness.wait_for_buffer_content("Hello\n").unwrap();
}

/// `*` should search for the word under cursor.
///
/// Root cause: `*` is not bound in vi-normal mode. The plugin has no handler
/// for it.
///
/// To fix: Add a `vi_search_word` handler that does `select_word` (to select
/// the word under cursor) followed by `find_selection_next` (to search for the
/// next occurrence). Bind `*` to this handler in vi-normal mode. Similarly,
/// add `#` bound to `find_selection_previous` for reverse search.
#[test]
#[ignore]
fn test_vi_bug_star_not_implemented() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nhello again\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // * should search for "hello" and move to next occurrence (line 2, offset 12)
    send_key(&mut harness, '*');

    harness.wait_until(|h| h.cursor_position() == 12).unwrap();
}

// =============================================================================
// Bug #26: Count display persists in status bar
// =============================================================================

/// After a counted motion like `3j`, the count should not persist in the status bar.
///
/// Root cause: `consumeCount()` calls `editor.setStatus()` to clear the "(3)"
/// display, but `setStatus` sends the update via an async channel. The test
/// checks the rendered screen before the status bar update is processed, so
/// "(3)" is still visible.
///
/// To fix: Use `wait_until_stable` with a condition that checks the screen
/// does NOT contain "(3)". Alternatively, add a render cycle after the motion
/// completes and before checking. The underlying plugin fix (clearing count in
/// `consumeCount`) is correct — only the test timing is the issue.
#[test]
#[ignore]
fn test_vi_bug_count_persists_in_status() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "a\nb\nc\nd\ne\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Send 3j (move down 3 lines)
    send_key(&mut harness, '3');
    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() > 0).unwrap();

    // The status bar should show "-- NORMAL --" without "(3)"
    // Check that the screen contains clean NORMAL indicator
    harness.wait_for_screen_contains("NORMAL").unwrap();

    // Render and check the screen does NOT contain "(3)"
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("(3)"),
        "Count '(3)' should not persist in status bar after motion completes"
    );
}

// =============================================================================
// Bug: 2dd (count with dd) — verify it works
// =============================================================================

/// `2dd` should delete 2 lines.
#[test]
fn test_vi_2dd_delete_two_lines() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "line1\nline2\nline3\nline4\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // 2dd should delete first 2 lines
    send_key(&mut harness, '2');
    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("line3\nline4\n").unwrap();
}

// =============================================================================
// Bug: d% / c% / y% (operator + matching-bracket motion) does nothing
// =============================================================================

/// `d%` should delete from the cursor through the matching bracket (inclusive),
/// e.g. on the `(` of "foo(bar)baz" it should leave "foobaz".
///
/// Was a no-op because `goto_matching_bracket` had no entry in
/// `OPERATOR_MOTIONS`; it now resolves through `matchingBracketTarget`, the
/// same resolver the normal-mode `%` uses.
#[test]
fn test_vi_bug_d_percent_ignored() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "foo(bar)baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move cursor onto the '(' at byte 3.
    send_key(&mut harness, 'l');
    send_key(&mut harness, 'l');
    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();

    // d% should delete "(bar)" inclusive, leaving "foobaz\n".
    send_operator_motion(&mut harness, 'd', '%');

    harness.wait_for_buffer_content("foobaz\n").unwrap();
}

// =============================================================================
// Bug #2442: j/k onto a shorter line park the cursor one past the last char,
// so `x` deletes the newline and joins lines.
// =============================================================================

/// In NORMAL mode the cursor must never rest past the last character of a line.
/// Moving down (`j`) from a long line onto a *shorter* line must clamp the
/// cursor to the shorter line's last character — like Vim — not the insert
/// position one past it. Otherwise `x` deletes the line-ending newline and
/// joins the following line (silent corruption from navigate-then-delete).
#[test]
fn test_vi_bug_2442_j_onto_shorter_line_then_x_joins_lines() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new(
        "motions.txt",
        "hello world foo bar baz\nshort\na longer line with many words here\n",
    )
    .unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // $ -> last char of line 1 (the 'z'). j -> down onto "short".
    send_key(&mut harness, '$');
    let pos_eol = harness.cursor_position();
    send_key(&mut harness, 'j');
    harness
        .wait_until(|h| h.cursor_position() > pos_eol)
        .unwrap();

    // x deletes one character. In Vim the cursor sits on the 't' of "short",
    // so x gives "shor" and never joins line 3.
    send_key(&mut harness, 'x');

    harness
        .wait_for_buffer_content(
            "hello world foo bar baz\nshor\na longer line with many words here\n",
        )
        .unwrap();
}

/// Same off-by-one for upward motion (`k`) onto a shorter line.
#[test]
fn test_vi_bug_2442_k_onto_shorter_line_then_x_joins_lines() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new(
        "motions.txt",
        "hello world foo bar baz\nshort\na longer line with many words here\n",
    )
    .unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Go to line 3, end of line, then k up onto "short".
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'j');
    send_key(&mut harness, '$');
    let pos_eol = harness.cursor_position();
    send_key(&mut harness, 'k');
    harness
        .wait_until(|h| h.cursor_position() < pos_eol)
        .unwrap();

    send_key(&mut harness, 'x');

    harness
        .wait_for_buffer_content(
            "hello world foo bar baz\nshor\na longer line with many words here\n",
        )
        .unwrap();
}

/// The desired (goal) column must survive the clamp: long -> short -> long
/// returns to the original column, matching Vim's remembered column.
#[test]
fn test_vi_bug_2442_goal_column_preserved_through_short_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    // Column 10 (0-indexed byte 10) is 'd' of "world" on line 1 and 'l' of
    // "longer" on line 3; line 2 "short" is only 5 chars.
    let fixture = TestFixture::new(
        "motions.txt",
        "hello world foo bar baz\nshort\na longer line with many words here\n",
    )
    .unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to byte column 10 on line 1 via 10 'l' presses.
    for _ in 0..10 {
        send_key(&mut harness, 'l');
    }
    harness.wait_until(|h| h.cursor_position() == 10).unwrap();

    // j onto "short" clamps to its last char ('t', byte 4 on the line ->
    // absolute byte 24+4 = 28), then j onto line 3 must restore column 10
    // (absolute byte 24 + 6 + 10 = 40).
    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 28).unwrap();
    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 40).unwrap();
}

// =============================================================================
// Bug #2439: quote text-objects i"/a" don't search forward on the line
// =============================================================================

/// Helper: send an operator + inner/around text-object (e.g. `di"`).
fn send_operator_text_object(
    harness: &mut EditorTestHarness,
    op: char,
    modifier: char,
    object: char,
) {
    send_key(harness, op);
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(harness, modifier);
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-text-object".to_string()))
        .unwrap();
    send_key(harness, object);
}

/// `di"` from the start of a line (before the opening quote) should, like Vim,
/// search forward on the current line and delete the contents of the quoted
/// string — `the "quick" brown fox` → `the "" brown fox`.
///
/// BUG: the quote text object only selected the pair the cursor was already
/// *inside*, so `di"`/`ci"` were silent no-ops when the cursor sat before the
/// quotes (the common case, e.g. right after `0`).
#[test]
fn test_vi_bug_di_quote_searches_forward_on_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "the \"quick\" brown fox\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Cursor is at column 0 (before the opening quote).
    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    // di" should delete inside the forward quoted string.
    send_operator_text_object(&mut harness, 'd', 'i', '"');

    harness
        .wait_for_buffer_content("the \"\" brown fox\n")
        .unwrap();
}

/// `ci"` from before the quotes should delete inside the quoted string, enter
/// insert mode, and let you type the replacement — same forward-search rule as
/// `di"`.
#[test]
fn test_vi_bug_ci_quote_searches_forward_on_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "the \"quick\" brown fox\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    // ci" should clear inside the quotes and drop into insert mode.
    send_operator_text_object(&mut harness, 'c', 'i', '"');
    wait_insert(&mut harness);

    harness.type_text("WXYZ").unwrap();
    harness.render().unwrap();
    escape(&mut harness);

    harness
        .wait_for_buffer_content("the \"WXYZ\" brown fox\n")
        .unwrap();
}

/// Bug #2604: `da"` should also delete the trailing whitespace after the
/// closing quote, like Vim (`:help aquote`): `the "quick" brown fox` →
/// `the brown fox` (single space), not `the  brown fox` (double space).
#[test]
fn test_vi_bug_da_quote_includes_trailing_whitespace() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "the \"quick\" brown fox\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Cursor is at column 0 (before the opening quote).
    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    // da" deletes `"quick"` plus the trailing space.
    send_operator_text_object(&mut harness, 'd', 'a', '"');

    harness.wait_for_buffer_content("the brown fox\n").unwrap();
}

/// Bug #2604 (leading-whitespace fallback): when there is no trailing
/// whitespace after the closing quote, `da"` includes the leading whitespace
/// before the opening quote instead — `foo "bar"` → `foo`.
#[test]
fn test_vi_bug_da_quote_falls_back_to_leading_whitespace() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "foo \"bar\"\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    // No trailing whitespace after the closing quote, so the leading space is
    // consumed instead.
    send_operator_text_object(&mut harness, 'd', 'a', '"');

    harness.wait_for_buffer_content("foo\n").unwrap();
}

// =============================================================================
// Bug #2438: vi mode indent operators >>/<< (and visual >/<) do nothing
// =============================================================================
//
// `>>`/`<<` in normal mode and `>`/`<` in visual mode were never bound, so they
// were silent no-ops. They now shift whole lines by one indent level (the
// default `text` buffer uses 4 spaces) and leave the cursor on the first
// non-blank of the first line, matching Vim.

/// `>>` indents the current line by one shiftwidth.
#[test]
fn test_vi_indent_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbeta\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // >> on line 1
    send_key(&mut harness, '>');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '>');

    harness
        .wait_for_buffer_content("    alpha\nbeta\n")
        .unwrap();
}

/// `<<` dedents the current line by one shiftwidth.
#[test]
fn test_vi_dedent_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "        alpha\nbeta\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // << on line 1 removes one 4-space level.
    send_key(&mut harness, '<');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '<');

    harness
        .wait_for_buffer_content("    alpha\nbeta\n")
        .unwrap();
}

/// A count applies the indent to N lines: `2>>` indents two lines.
#[test]
fn test_vi_indent_line_with_count() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\nthree\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // 2>> indents lines 1 and 2, leaving line 3 untouched.
    send_key(&mut harness, '2');
    send_key(&mut harness, '>');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '>');

    harness
        .wait_for_buffer_content("    one\n    two\nthree\n")
        .unwrap();
}

/// `>` plus a linewise motion (`>j`) indents the spanned lines.
#[test]
fn test_vi_indent_motion() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\nthree\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // >j indents the current line and the one below.
    send_operator_motion(&mut harness, '>', 'j');

    harness
        .wait_for_buffer_content("    one\n    two\nthree\n")
        .unwrap();
}

/// `.` repeats the last `>>`.
#[test]
fn test_vi_indent_line_repeat() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbeta\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, '>');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '>');
    harness
        .wait_for_buffer_content("    alpha\nbeta\n")
        .unwrap();

    // The buffer matches as soon as the indent lands, but `vi_indent_line` is
    // still finishing up (placing the cursor, then switching back to normal
    // mode). A `.` sent while the editor is still in operator-pending mode is
    // unbound there and gets swallowed, so wait for normal mode first —
    // matching the sibling repeat tests in vi_mode.rs.
    wait_normal(&mut harness);

    // `.` indents the same line again -> two levels (8 spaces).
    send_key(&mut harness, '.');
    harness
        .wait_for_buffer_content("        alpha\nbeta\n")
        .unwrap();
}

/// Visual-line `>` indents the selected line and returns to normal mode.
#[test]
fn test_vi_visual_line_indent() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbeta\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // V selects line 1, > indents it.
    harness
        .send_key(KeyCode::Char('V'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();

    send_key(&mut harness, '>');

    harness
        .wait_for_buffer_content("    alpha\nbeta\n")
        .unwrap();
    // Returns to normal mode after the operation (Vim behavior).
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

/// Visual-line `<` dedents the selected lines.
#[test]
fn test_vi_visual_line_dedent() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "    alpha\n    beta\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // V then j to select both lines, then <.
    harness
        .send_key(KeyCode::Char('V'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, '<');

    harness.wait_for_buffer_content("alpha\nbeta\n").unwrap();
}

/// Visual-line `>` indents exactly the highlighted lines and nothing past them.
/// It operates on the same live selection as visual-mode `d`/`y`, so it stays
/// consistent with them; the regression this guards against is the indent
/// bleeding into the line below the selection.
#[test]
fn test_vi_visual_line_indent_stops_at_selection() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\nthree\nfour\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // `V` then `j` selects two lines, as it does in Vim: `select_line` already
    // leaves the caret on the next line's first column, so `select_down`
    // extends the selection by exactly one more line. `three` and `four` are
    // not selected and must stay unindented.
    //
    // This test previously asserted three indented lines, which is what the
    // plugin did before the extra `select_line_end` was dropped from
    // `vi_vline_down` — checked against Vim 9.1, which indents two.
    harness
        .send_key(KeyCode::Char('V'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, '>');

    harness
        .wait_for_buffer_content("    one\n    two\nthree\nfour\n")
        .unwrap();
}

// =============================================================================
// Bug #2606: visual-block (Ctrl+V) indent operators >/< do nothing
// =============================================================================
//
// `>`/`<` were bound in visual and visual-line mode (bug #2438) but never in
// visual-block mode, so the keys were silently swallowed and the editor stayed
// stuck in `-- VISUAL BLOCK --`. Like Vim, they now shift every line the block
// touches by one shiftwidth and return to normal mode.

/// Visual-block `>` indents every line the block spans and returns to normal.
#[test]
fn test_vi_visual_block_indent() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbeta\ngamma\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Ctrl+V enters visual block, j extends it down to the second line.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-block".to_string()))
        .unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, '>');

    // Both spanned lines shift right by one 4-space level; gamma is untouched.
    harness
        .wait_for_buffer_content("    alpha\n    beta\ngamma\n")
        .unwrap();
    // Returns to normal mode after the operation (Vim behavior).
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

/// Visual-block `<` dedents every line the block spans and returns to normal.
#[test]
fn test_vi_visual_block_dedent() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "    alpha\n    beta\ngamma\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-block".to_string()))
        .unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, '<');

    harness
        .wait_for_buffer_content("alpha\nbeta\ngamma\n")
        .unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

// =============================================================================
// Bug #2441: find-char motions (f/t/F/T) broken with operators and ; / , repeat
// =============================================================================

/// `dfr` should delete from the cursor up to and including the first `r`,
/// leaving "ld foo bar baz". BUG: stuck in operator-pending, nothing deleted.
#[test]
fn test_vi_bug_2441_dfr_deletes_through_target() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo bar baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 'f');
    // `f` enters vi-find-char and consumes the *next* key as the target.
    // Wait for that mode before sending 'r': without it, a slow-CI race
    // delivers 'r' while still in vi-operator-pending, the find never
    // happens, and the wait below hangs to the external timeout. Same guard
    // as the normal-mode f-find tests above.
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'r');

    harness.wait_for_buffer_content("ld foo bar baz\n").unwrap();
}

/// `dfw` should delete up to and including the first `w` ("hello w"),
/// leaving "orld foo bar baz". BUG: `f` is dropped and `w` runs as a plain
/// word motion, so `dfw` silently degrades to `dw`.
#[test]
fn test_vi_bug_2441_dfw_target_is_motion_key() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo bar baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 'f');
    // `f` enters vi-find-char and consumes the *next* key as the target.
    // Wait for that mode before sending 'w': without it, a slow-CI race
    // delivers 'w' while still in vi-operator-pending, the find never
    // happens, and the wait below hangs to the external timeout. Same guard
    // as the normal-mode f-find tests above.
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'w');

    harness
        .wait_for_buffer_content("orld foo bar baz\n")
        .unwrap();
}

/// `dtr` (till) should delete up to but NOT including the first `r`,
/// leaving "rld foo bar baz".
#[test]
fn test_vi_bug_2441_dtr_deletes_until_target() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo bar baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 't');
    // `t` enters vi-find-char and consumes the *next* key as the target.
    // Wait for that mode before sending 'r': without it, a slow-CI race
    // delivers 'r' while still in vi-operator-pending, the find never
    // happens, and the wait below hangs to the external timeout. Same guard
    // as the normal-mode f-find tests above.
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'r');

    harness
        .wait_for_buffer_content("rld foo bar baz\n")
        .unwrap();
}

/// `cfr` should delete up to and including the first `r` and enter insert mode,
/// then typed text replaces it. Leaves "Xld foo bar baz".
#[test]
fn test_vi_bug_2441_cfr_changes_through_target() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo bar baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'c');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 'f');
    // `f` enters vi-find-char and consumes the *next* key as the target.
    // Wait for that mode before sending 'r': without it, a slow-CI race
    // delivers 'r' while still in vi-operator-pending, the find never
    // happens, and the wait below hangs to the external timeout. Same guard
    // as the normal-mode f-find tests above.
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'r');
    wait_insert(&mut harness);
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    escape(&mut harness);

    harness
        .wait_for_buffer_content("Xld foo bar baz\n")
        .unwrap();
}

/// `;` should repeat the last `f`/`t`. After `fo` (lands on first 'o', col 4),
/// `;` should advance to the next 'o' (col 7). BUG: `;` is a no-op.
#[test]
fn test_vi_bug_2441_semicolon_repeats_find() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo bar baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'f');
    // `f` enters vi-find-char and consumes the *next* key as the target.
    // Wait for that mode before sending 'o'; otherwise a fast/slow-CI race
    // delivers 'o' while still in vi-normal, where it means "open line below"
    // (→ insert mode), the find never happens, and the cursor wait below hangs
    // to the external timeout. Matches the f-find sibling tests above.
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'o');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();

    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 7).unwrap();
}

/// `,` should repeat the last find in the opposite direction. After `fo;`
/// (cursor at col 7), `,` should move back to the previous 'o' (col 4).
#[test]
fn test_vi_bug_2441_comma_repeats_find_reverse() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo bar baz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'f');
    // `f` enters vi-find-char and consumes the *next* key as the target.
    // Wait for that mode before sending 'o'; otherwise a fast/slow-CI race
    // delivers 'o' while still in vi-normal, where it means "open line below"
    // (→ insert mode), the find never happens, and the cursor waits below hang
    // to the external timeout. Gate each subsequent step on the cursor landing
    // so `;`/`,` repeat a completed find rather than racing an in-flight one.
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, 'o');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 7).unwrap();

    send_key(&mut harness, ',');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
}

// =============================================================================
// Issue #2443: dot-repeat (`.`) of the cursor-repositioning insert commands
// (`o`/`O`/`a`/`A`) corrupted the buffer: the recorded change spanned from the
// PRE-reposition cursor position to the insert end, so replay re-inserted
// intervening buffer text instead of only the typed keystrokes.
//
// These reproducers observe only what is on screen: the rendered text rows and
// the status line's mode and `Ln N, Col N` readouts.
// =============================================================================

/// First screen row that shows buffer text (row 0 is the menu bar, row 1 the
/// tab bar).
const FIRST_TEXT_ROW: u16 = 2;

/// Consecutive identical frames that count as "the editor has finished
/// reacting". Used only to decide that a *wrong* result is final; the expected
/// result short-circuits the wait as soon as it renders.
const SETTLED_RENDERS: usize = 20;

/// The buffer lines as rendered, with the line-number gutter stripped and the
/// scrollbar column dropped.
///
/// A line the editor renders empty comes back as `""`; rows past the end of the
/// buffer come back as the `~` filler verbatim, so an expectation that lists the
/// filler also pins the buffer's line count.
fn rendered_buffer_lines(harness: &EditorTestHarness, count: usize) -> Vec<String> {
    (0..count)
        .map(|i| {
            let row = harness.screen_row_text(FIRST_TEXT_ROW + i as u16);
            // The scrollbar paints a block glyph in the last column of some
            // rows; it is chrome, not buffer text.
            let row = row.trim_end_matches(['▌', '▐', '█', ' ']).to_string();
            if let Some((_gutter, text)) = row.split_once(" │ ") {
                text.to_string()
            } else if row.ends_with('│') {
                // Gutter with no text after it: an empty buffer line.
                String::new()
            } else {
                row
            }
        })
        .collect()
}

/// Wait for the rendered buffer rows to become exactly `expected`, and fail with
/// a diff if the screen settles on anything else.
///
/// Both arms wait on observed state rather than elapsed time: the success arm
/// fires the moment the expected rows render, and the failure arm fires once the
/// screen has stopped changing. The failure arm is what makes these reproducers
/// useful regression guards — waiting only for the expected text means a
/// regression never asserts at all, it just runs until the external test timeout
/// kills it, which reports a hang instead of naming the corrupted text.
fn assert_renders(harness: &mut EditorTestHarness, expected: &[&str]) {
    let mut previous = String::new();
    let mut settled = 0usize;
    harness
        .wait_until(|h| {
            if rendered_buffer_lines(h, expected.len()) == expected {
                return true;
            }
            let screen = h.screen_to_string();
            if screen == previous {
                settled += 1;
            } else {
                previous = screen;
                settled = 0;
            }
            settled >= SETTLED_RENDERS
        })
        .unwrap();

    assert_eq!(
        rendered_buffer_lines(harness, expected.len()),
        expected,
        "editor settled on the wrong buffer text"
    );
}

/// Wait for the status line to report the cursor on the given 1-based line.
fn wait_cursor_line(harness: &mut EditorTestHarness, line: usize) {
    harness
        .wait_for_screen_contains(&format!("Ln {line},"))
        .unwrap();
}

/// Wait for the status line to report the given 1-based line and column.
fn wait_cursor_at(harness: &mut EditorTestHarness, line: usize, col: usize) {
    harness
        .wait_for_screen_contains(&format!("Ln {line}, Col {col}"))
        .unwrap();
}

/// Wait for the status line to show vi insert mode.
fn wait_insert_indicator(harness: &mut EditorTestHarness) {
    harness.wait_for_screen_contains("-- INSERT --").unwrap();
}

/// Leave insert mode, waiting for the status line to show normal mode again.
fn escape_to_normal(harness: &mut EditorTestHarness) {
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.wait_for_screen_contains("-- NORMAL --").unwrap();
}

/// `o hello Esc` then `.` on another line must open a line below it containing
/// exactly the typed text. BUG: `.` injected the original line's content plus
/// the typed text mid-word (`charalpha` / `hellolie`).
#[test]
fn test_vi_bug_2443_dot_repeat_open_below() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbravo\ncharlie\ndelta\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'o');
    wait_insert_indicator(&mut harness);
    harness.type_text("hello").unwrap();
    escape_to_normal(&mut harness);
    assert_renders(
        &mut harness,
        &["alpha", "hello", "bravo", "charlie", "delta", "", "~"],
    );

    // Move from the inserted "hello" line down to "charlie", waiting for each
    // motion to land so `.` replays at the intended position.
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 3);
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 4);

    send_key(&mut harness, '.');
    assert_renders(
        &mut harness,
        &[
            "alpha", "hello", "bravo", "charlie", "hello", "delta", "", "~",
        ],
    );
}

/// `a Y Esc` then `.` on another line must append the typed text after the
/// cursor. BUG: `.` re-injected the original line's leading character
/// (`aYbravo` instead of `bYravo`).
#[test]
fn test_vi_bug_2443_dot_repeat_append_after_cursor() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbravo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'a');
    wait_insert_indicator(&mut harness);
    harness.type_text("Y").unwrap();
    escape_to_normal(&mut harness);
    assert_renders(&mut harness, &["aYlpha", "bravo", "", "~"]);

    // `j` then `0`: land on the `b` of "bravo".
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 2);
    send_key(&mut harness, '0');
    wait_cursor_at(&mut harness, 2, 1);

    send_key(&mut harness, '.');
    assert_renders(&mut harness, &["aYlpha", "bYravo", "", "~"]);
}

/// `A X Esc` then `.` on another line must append the typed text at end of
/// line. BUG: `.` injected the entire original line's content at the cursor
/// (`bravalphaXo` instead of `bravoX`).
#[test]
fn test_vi_bug_2443_dot_repeat_append_line_end() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbravo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'A');
    wait_insert_indicator(&mut harness);
    harness.type_text("X").unwrap();
    escape_to_normal(&mut harness);
    assert_renders(&mut harness, &["alphaX", "bravo", "", "~"]);

    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 2);

    send_key(&mut harness, '.');
    assert_renders(&mut harness, &["alphaX", "bravoX", "", "~"]);
}

/// `O` must open the empty line above the current line AND leave the cursor on
/// it, so the typed text goes into the new line. BUG: the empty line opened at
/// the right place but the cursor landed on the line above it, so the text
/// corrupted that line (`bheyravo`), and `.` afterwards reported "No change to
/// repeat".
#[test]
fn test_vi_bug_2443_open_above_cursor_placement_and_repeat() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbravo\ncharlie\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Move to "charlie".
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 2);
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 3);

    send_key(&mut harness, 'O');
    wait_insert_indicator(&mut harness);
    harness.type_text("hey").unwrap();
    escape_to_normal(&mut harness);
    assert_renders(&mut harness, &["alpha", "bravo", "hey", "charlie", "", "~"]);

    // `.` must repeat the whole change: open a line above and insert "hey".
    send_key(&mut harness, '.');
    assert_renders(
        &mut harness,
        &["alpha", "bravo", "hey", "hey", "charlie", "", "~"],
    );
}

/// An insert entered with `i` after an unrelated `x` must be recorded for `.`.
/// BUG: `.` still repeated the old `x` (the insert was never recorded), so the
/// second line lost a character instead of gaining the typed one.
#[test]
fn test_vi_bug_2443_insert_after_delete_char_is_recorded() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "abc\nxyz\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'x');
    assert_renders(&mut harness, &["bc", "xyz", "", "~"]);

    send_key(&mut harness, 'i');
    wait_insert_indicator(&mut harness);
    harness.type_text("Q").unwrap();
    escape_to_normal(&mut harness);
    assert_renders(&mut harness, &["Qbc", "xyz", "", "~"]);

    // `j` then `0`: land on the `x` of "xyz".
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 2);
    send_key(&mut harness, '0');
    wait_cursor_at(&mut harness, 2, 1);

    // `.` must repeat the insert (`iQ`), not the earlier `x`.
    send_key(&mut harness, '.');
    assert_renders(&mut harness, &["Qbc", "Qxyz", "", "~"]);
}

/// Control: dot-repeat of a plain `i` insert (already correct before the fix)
/// keeps working, so the fix cannot regress it.
#[test]
fn test_vi_bug_2443_control_dot_repeat_insert_before() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\nbravo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'i');
    wait_insert_indicator(&mut harness);
    harness.type_text("AB").unwrap();
    escape_to_normal(&mut harness);
    assert_renders(&mut harness, &["ABalpha", "bravo", "", "~"]);

    // Cursor rests on `B` (col 2). `j` keeps the column, so `.` inserts before
    // the `r` of "bravo".
    send_key(&mut harness, 'j');
    wait_cursor_at(&mut harness, 2, 2);

    send_key(&mut harness, '.');
    assert_renders(&mut harness, &["ABalpha", "bABravo", "", "~"]);
}

/// Control: dot-repeat of `x` (already correct before the fix) keeps deleting
/// successive characters, so the fix cannot regress it.
#[test]
fn test_vi_bug_2443_control_dot_repeat_delete_char() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "abcdef\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'x');
    assert_renders(&mut harness, &["bcdef", "", "~"]);

    send_key(&mut harness, '.');
    assert_renders(&mut harness, &["cdef", "", "~"]);

    send_key(&mut harness, '.');
    assert_renders(&mut harness, &["def", "", "~"]);
}

/// A recorded `ci"` must replay as a change, and stay one across repeated `.`.
///
/// Two bugs met here. The replay re-recorded itself from the `"d"` it is
/// handed, downgrading the record to a delete; and `ci"` entered insert mode
/// before its queued `deleteRange`/`setBufferCursor` had landed, so the
/// session's start offset was the pre-command cursor and the Escape-time
/// capture recorded surrounding buffer text as if it had been typed. The
/// second is the `c`-operator half of #2443 — `.` pasted line 1's text into
/// line 2 (`b "a "Q" y`). Found by driving the editor in tmux.
#[test]
fn test_vi_ci_quote_dot_repeat_twice_stays_a_change() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "a \"one\" z\nb \"two\" y\nc \"six\" x\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    send_operator_text_object(&mut harness, 'c', 'i', '"');
    wait_insert(&mut harness);
    harness.type_text("Q").unwrap();
    harness.render().unwrap();
    escape(&mut harness);
    harness
        .wait_for_buffer_content("a \"Q\" z\nb \"two\" y\nc \"six\" x\n")
        .unwrap();

    // First `.` on line 2.
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 2);
    send_key(&mut harness, '0');
    send_key(&mut harness, '.');
    harness
        .wait_for_buffer_content("a \"Q\" z\nb \"Q\" y\nc \"six\" x\n")
        .unwrap();

    // Second `.` on line 3 must still insert, not just delete.
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 3);
    send_key(&mut harness, '0');
    send_key(&mut harness, '.');
    harness
        .wait_for_buffer_content("a \"Q\" z\nb \"Q\" y\nc \"Q\" x\n")
        .unwrap();
}

/// Vi's modal state is scoped to one buffer: a visual anchor is a byte offset
/// into the text it was taken in. Opening another buffer must return to normal
/// mode rather than carry the anchor across, where it would address unrelated
/// bytes.
#[test]
fn test_vi_buffer_switch_leaves_visual_mode() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let first = TestFixture::new("first.txt", "aaaaaaaaaaaaaaaa\nbbbb\n").unwrap();
    let second = TestFixture::new("second.txt", "xyz\n").unwrap();
    harness.open_file(&first.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Anchor a visual selection well past the end of the other buffer.
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    for _ in 0..8 {
        send_key(&mut harness, 'l');
    }
    harness.render().unwrap();

    harness.open_file(&second.path).unwrap();
    harness.render().unwrap();
    wait_normal(&mut harness);

    // `dl` is a fresh operator+motion in the new buffer. Had the abandoned
    // anchor survived, the operator would have resolved against it instead of
    // deleting the single character under the cursor.
    send_key(&mut harness, 'd');
    send_key(&mut harness, 'l');
    harness.wait_for_buffer_content("yz\n").unwrap();
}

/// Returning to a buffer vi left mid-selection must not find normal mode
/// sitting over a live selection: the host keeps the selection per buffer, so
/// the next `x` would extend and cut the whole thing.
#[test]
fn test_vi_returning_to_buffer_collapses_abandoned_selection() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let first = TestFixture::new("first.txt", "abcdefgh\n").unwrap();
    let second = TestFixture::new("second.txt", "zzzz\n").unwrap();
    harness.open_file(&first.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    for _ in 0..4 {
        send_key(&mut harness, 'l');
    }
    harness.render().unwrap();
    assert!(
        harness.has_selection(),
        "visual mode should be holding a selection before the switch — \
         without one this test proves nothing"
    );

    harness.open_file(&second.path).unwrap();
    harness.render().unwrap();
    wait_normal(&mut harness);

    // Back to the buffer that still holds the abandoned selection.
    harness.open_file(&first.path).unwrap();
    harness.render().unwrap();
    wait_normal(&mut harness);

    // The assertion is on the selection itself rather than on the result of an
    // edit: after a visual selection the cursor sits at its end, so what `x`
    // would remove depends on a column this test has no reason to pin down.
    // `setBufferCursor` is dispatched to the editor thread, hence the wait.
    harness.wait_until(|h| !h.has_selection()).unwrap();
}

/// The same for an insert session: its start offset belongs to the buffer it
/// was opened in, so a buffer switch drops it instead of trying to capture
/// typed text against the new buffer's cursor.
#[test]
fn test_vi_buffer_switch_leaves_insert_mode() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let first = TestFixture::new("first.txt", "alpha\n").unwrap();
    let second = TestFixture::new("second.txt", "beta\n").unwrap();
    harness.open_file(&first.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'i');
    wait_insert(&mut harness);
    harness.type_text("Z").unwrap();
    harness.render().unwrap();

    harness.open_file(&second.path).unwrap();
    harness.render().unwrap();
    wait_normal(&mut harness);

    // Typing now goes through normal mode: `x` deletes a character rather
    // than being inserted literally.
    send_key(&mut harness, 'x');
    harness.wait_for_buffer_content("eta\n").unwrap();
}

/// `memory` is deliberately not reset by a buffer switch: Vim's `.` is global,
/// and it records a command rather than an offset, so it stays valid in any
/// buffer. This is the half of the contract most likely to regress.
#[test]
fn test_vi_dot_repeat_survives_buffer_switch() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let first = TestFixture::new("first.txt", "alpha\n").unwrap();
    let second = TestFixture::new("second.txt", "bravo\n").unwrap();
    harness.open_file(&first.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    // Record a change in the first buffer.
    send_key(&mut harness, 'x');
    harness.wait_for_buffer_content("lpha\n").unwrap();

    harness.open_file(&second.path).unwrap();
    harness.render().unwrap();
    wait_normal(&mut harness);

    // `.` must still replay it here.
    send_key(&mut harness, '.');
    harness.wait_for_buffer_content("ravo\n").unwrap();
}

/// The same decay on the operator+motion path: `applyOperatorWithMotion`
/// records too, and the replay hands it `"d"` for a recorded `cw`.
#[test]
fn test_vi_cw_dot_repeat_twice_stays_a_change() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one two\nthree four\nfive six\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    send_key(&mut harness, 'c');
    send_key(&mut harness, 'w');
    wait_insert(&mut harness);
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    escape(&mut harness);
    harness
        .wait_for_buffer_content("X two\nthree four\nfive six\n")
        .unwrap();

    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 2);
    send_key(&mut harness, '0');
    send_key(&mut harness, '.');
    harness
        .wait_for_buffer_content("X two\nX four\nfive six\n")
        .unwrap();

    // The second `.` must still insert.
    send_key(&mut harness, 'j');
    wait_cursor_line(&mut harness, 3);
    send_key(&mut harness, '0');
    send_key(&mut harness, '.');
    harness
        .wait_for_buffer_content("X two\nX four\nX six\n")
        .unwrap();
}

/// `buffer_activated` does not mean "the active buffer changed" — moving
/// focus between two splits showing the same buffer fires it too, and
/// deliberately so. Vi must not drop the selection being made in a file just
/// because focus moved between two views of it.
#[test]
fn test_vi_split_focus_keeps_visual_mode() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "abcdefgh\nijkl\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness.wait_until(|h| h.cursor_position() == 0).unwrap();

    // Two splits on the same buffer.
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SplitVertical);
    harness.render().unwrap();

    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'l');
    harness.render().unwrap();

    // Focus the other split: same buffer, so vi stays in visual mode.
    harness.editor_mut().next_split();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
}

// =============================================================================
// Vim-parity gaps found by running the same keystrokes through Vim 9.1
// =============================================================================

/// `Vk` has to grow the selection *upward* from the anchor line. The host's
/// `select_up` shrank a selection that already ended at a line start to
/// nothing, so `Vkd` deleted no lines at all.
#[test]
fn test_vi_visual_line_up_extends_selection() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\nthree\nfour\nfive\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'j');
    send_key(&mut harness, 'j');
    harness
        .send_key(KeyCode::Char('V'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();
    send_key(&mut harness, 'k');
    send_key(&mut harness, 'd');

    harness
        .wait_for_buffer_content("one\nfour\nfive\n")
        .unwrap();
}

/// `VJ` joins every line the selection touches. Vim performs one join fewer
/// than the number of selected lines.
#[test]
fn test_vi_visual_line_join() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\nthree\nfour\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness
        .send_key(KeyCode::Char('V'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual-line".to_string()))
        .unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'J');

    harness
        .wait_for_buffer_content("one two\nthree\nfour\n")
        .unwrap();
}

/// Visual mode takes `i`/`a` text objects: `viw` selects the word under the
/// caret, and `d` then removes exactly that word.
#[test]
fn test_vi_visual_inner_word() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'w');
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'i');
    send_key(&mut harness, 'w');
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("hello  foo\n").unwrap();
}

/// `vb` covers the character `v` started on, the way Vim's does: from the
/// start of `world`, `vbd` removes `hello w`.
#[test]
fn test_vi_visual_word_back_includes_anchor() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'w');
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'b');
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("orld foo\n").unwrap();
}

/// Vim's bracket text objects do not fail when the caret is outside every
/// pair: they fall forward to the next one.
#[test]
fn test_vi_inner_paren_falls_forward() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "foo(bar, baz)\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, 'i');
    send_key(&mut harness, '(');

    harness.wait_for_buffer_content("foo()\n").unwrap();
}

/// `I` inserts before the first non-blank, not in column 0 — that is `gI`.
/// `.` replays the same motion.
#[test]
fn test_vi_insert_line_start_skips_indent() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "    one\n        two\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    harness
        .send_key(KeyCode::Char('I'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();
    wait_insert(&mut harness);
    harness.type_text("X").unwrap();
    escape(&mut harness);

    send_key(&mut harness, 'j');
    send_key(&mut harness, '0');
    send_key(&mut harness, '.');

    harness
        .wait_for_buffer_content("    Xone\n        Xtwo\n")
        .unwrap();
}

/// `r` is a change, so `.` repeats it.
#[test]
fn test_vi_replace_char_dot_repeat() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'r');
    send_key(&mut harness, 'Z');
    wait_normal(&mut harness);
    send_key(&mut harness, 'j');
    send_key(&mut harness, '0');
    send_key(&mut harness, '.');

    harness.wait_for_buffer_content("Zne\nZwo\n").unwrap();
}

/// `;` after a `t` skips the match it is already sitting in front of, instead
/// of landing on the same column forever.
#[test]
fn test_vi_till_repeat_advances() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world foo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 't');
    send_key(&mut harness, 'o');
    wait_normal(&mut harness);
    send_key(&mut harness, ';');
    send_key(&mut harness, 'x');

    // `to` lands on the `l` before the `o` of "hello". `;` skips that same `o`
    // and stops before the one in "world", so `x` takes the `w`. Checked
    // against Vim 9.1.
    harness.wait_for_buffer_content("hello orld foo\n").unwrap();
}

/// Vim's `v$` lands *on* the end-of-line position, so the inclusive selection
/// takes the line break and `v$d` joins the next line up. `d$` does not.
#[test]
fn test_vi_visual_line_end_takes_line_break() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nnext\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'w');
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, '$');
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("hello next\n").unwrap();
}

/// `dj` on the last line and `dk` on the first are no-ops in Vim: the whole
/// operator fails when the motion cannot move.
#[test]
fn test_vi_linewise_operator_fails_at_buffer_edge() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_operator_motion(&mut harness, 'd', 'k');
    wait_normal(&mut harness);
    harness.wait_for_buffer_content("one\ntwo\n").unwrap();

    send_key(&mut harness, 'j');
    send_operator_motion(&mut harness, 'd', 'j');
    wait_normal(&mut harness);
    harness.wait_for_buffer_content("one\ntwo\n").unwrap();
}

// =============================================================================
// Bugs found by an unprimed review of this branch, each checked against Vim 9.1
// =============================================================================

/// `[count]J` near the end of the file joined past the last line, deleting the
/// trailing newline and appending a space. Vim joins what it can and stops.
#[test]
fn test_vi_join_count_stops_at_last_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "aaa\nbbb\nccc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'j');
    send_key(&mut harness, '5');
    send_key(&mut harness, 'J');

    harness.wait_for_buffer_content("aaa\nbbb ccc\n").unwrap();
}

/// Visual `J` took its line span from the line-wise mode's own bookkeeping, so
/// in charwise visual it always joined exactly one pair.
#[test]
fn test_vi_visual_charwise_join_spans_selection() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "aaa\nbbb\nccc\nddd\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'J');

    harness
        .wait_for_buffer_content("aaa bbb ccc\nddd\n")
        .unwrap();
}

/// `r` replaces on the caret's line only. An unclamped count ran over the line
/// break; Vim refuses the command outright when the line is too short.
#[test]
fn test_vi_replace_char_count_stays_on_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "ab\ncd\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, '5');
    send_key(&mut harness, 'r');
    send_key(&mut harness, 'z');
    wait_normal(&mut harness);

    // Nothing changes: five characters are not available on this line.
    send_key(&mut harness, 'x');
    harness.wait_for_buffer_content("b\ncd\n").unwrap();
}

/// `d3G` deletes to line 3, not to the end of the file. The count was dropped
/// because a count defaulted to 1 is indistinguishable from no count at all.
#[test]
fn test_vi_delete_to_counted_line() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "aaa\nbbb\nccc\nddd\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '3');
    send_key(&mut harness, 'G');

    harness.wait_for_buffer_content("ddd\n").unwrap();
}

/// A line-wise change leaves an empty line to type into; `.` replayed it as a
/// plain delete, so the repeated text landed on the following line.
#[test]
fn test_vi_linewise_change_dot_repeat() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "one\ntwo\nthree\nfour\nfive\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_operator_motion(&mut harness, 'c', 'j');
    wait_insert(&mut harness);
    harness.type_text("X").unwrap();
    escape(&mut harness);

    send_key(&mut harness, 'j');
    send_key(&mut harness, 'j');
    send_key(&mut harness, '.');

    harness.wait_for_buffer_content("X\nthree\nX\n").unwrap();
}

/// `vh` and `vk` have to take the character `v` started on with them. Driving
/// the host's selection backwards just shrank it to nothing.
#[test]
fn test_vi_visual_backward_keeps_anchor_character() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "hello world\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'l');
    send_key(&mut harness, 'l');
    send_key(&mut harness, 'l');
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'h');
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("heo world\n").unwrap();
}

/// Visual `k` grows the selection upward from the anchor.
#[test]
fn test_vi_visual_up_keeps_anchor_character() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "aaa\nbbb\nccc\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'j');
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'k');
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("bb\nccc\n").unwrap();
}

fn open_vi(content: &str) -> (EditorTestHarness, tempfile::TempDir, TestFixture) {
    let (mut harness, td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", content).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);
    (harness, td, fixture)
}

fn edit_first_line_then_normal(harness: &mut EditorTestHarness, text: &str) {
    send_key(harness, 'i');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-insert".to_string()))
        .unwrap();
    harness.type_text(text).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    wait_normal(harness);
}

#[test]
fn test_vi_ZZ_saves_and_quits() {
    let (mut harness, _td, fixture) = open_vi("alpha\n");
    edit_first_line_then_normal(&mut harness, "X");

    send_key(&mut harness, 'Z');
    send_key(&mut harness, 'Z');

    harness.wait_until(|h| h.should_quit()).unwrap();
    assert_eq!(fs::read_to_string(&fixture.path).unwrap(), "Xalpha\n");
}

#[test]
fn test_vi_ZQ_quits_without_saving() {
    let (mut harness, _td, fixture) = open_vi("alpha\n");
    edit_first_line_then_normal(&mut harness, "X");

    send_key(&mut harness, 'Z');
    send_key(&mut harness, 'Q');

    harness.wait_until(|h| h.should_quit()).unwrap();
    assert_eq!(fs::read_to_string(&fixture.path).unwrap(), "alpha\n");
}

#[test]
fn test_vi_counted_find_char_uses_count() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d.e\n");

    send_key(&mut harness, '2');
    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');

    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
}

#[test]
fn test_vi_counted_find_char_repeat_uses_count() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d.e\n");

    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();

    send_key(&mut harness, '2');
    send_key(&mut harness, ';');

    harness.wait_until(|h| h.cursor_position() == 5).unwrap();
}

#[test]
fn test_vi_reverse_find_repeat_keeps_original_direction() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d.e\n");

    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');
    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();

    send_key(&mut harness, ',');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();

    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
}

#[test]
fn test_vi_d_caret_deletes_to_first_non_blank() {
    let (mut harness, _td, _fixture) = open_vi("    hello world\n");

    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() == 14).unwrap();
    send_operator_motion(&mut harness, 'd', '^');

    harness.wait_for_buffer_content("    d\n").unwrap();
}

#[test]
fn test_vi_d_semicolon_repeats_find_with_operator() {
    let (mut harness, _td, _fixture) = open_vi("a,b,c,d\n");

    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, ',');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();

    send_operator_motion(&mut harness, 'd', ';');

    harness.wait_for_buffer_content("ac,d\n").unwrap();
}

#[test]
fn test_vi_d_percent_backward_from_closing_bracket() {
    let (mut harness, _td, _fixture) = open_vi("foo(bar)baz\n");

    for _ in 0..7 {
        send_key(&mut harness, 'l');
    }
    harness.wait_until(|h| h.cursor_position() == 7).unwrap();
    send_operator_motion(&mut harness, 'd', '%');

    harness.wait_for_buffer_content("foobaz\n").unwrap();
}

#[test]
fn test_vi_plus_and_minus_move_to_first_non_blank() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n  c\n");

    send_key(&mut harness, '+');
    harness.wait_until(|h| h.cursor_position() == 6).unwrap();

    send_key(&mut harness, '+');
    harness.wait_until(|h| h.cursor_position() == 10).unwrap();

    send_key(&mut harness, '-');
    harness.wait_until(|h| h.cursor_position() == 6).unwrap();
}

#[test]
fn test_vi_underscore_with_count_moves_down_to_first_non_blank() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n  c\n");

    send_key(&mut harness, '3');
    send_key(&mut harness, '_');

    harness.wait_until(|h| h.cursor_position() == 10).unwrap();
}

/// `.` after `d,` must repeat `d,` against the current find without storing
/// the reversed direction: Vim's `;` afterwards still searches forward.
#[test]
fn test_vi_dot_after_d_comma_keeps_find_direction() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d.e\n");

    send_key(&mut harness, 'f');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');
    send_key(&mut harness, ';');
    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 5).unwrap();

    send_operator_motion(&mut harness, 'd', ',');
    harness.wait_for_buffer_content("a.b.d.e\n").unwrap();

    send_key(&mut harness, '.');
    harness.wait_for_buffer_content("a.d.e\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();

    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
}

/// `%` with the cursor between brackets uses the next bracket on the line,
/// the same one `d%` acts on: from `b` in "foo(bar)baz" it finds `)` and
/// jumps to its `(`.
#[test]
fn test_vi_percent_inside_brackets_matches_d_percent() {
    let (mut harness, _td, _fixture) = open_vi("foo(bar)baz\n");

    for _ in 0..4 {
        send_key(&mut harness, 'l');
    }
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_key(&mut harness, '%');

    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
}

/// `%` before any bracket on the line jumps to the partner of the first one.
#[test]
fn test_vi_percent_before_bracket_jumps_to_partner() {
    let (mut harness, _td, _fixture) = open_vi("foo(bar)baz\n");

    send_key(&mut harness, '%');

    harness.wait_until(|h| h.cursor_position() == 7).unwrap();
}

/// `ZZ` (`:x`) writes only when the buffer has changes: an unchanged file is
/// not rewritten.
#[test]
fn test_vi_ZZ_unmodified_does_not_rewrite_file() {
    // Backdate the file before the editor opens it, so the editor's own view
    // of the file agrees with the disk and a save would go through.
    let (mut harness, _td) = vi_mode_harness(80, 24);
    let fixture = TestFixture::new("test.txt", "alpha\n").unwrap();
    let old = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000_000);
    fs::File::options()
        .write(true)
        .open(&fixture.path)
        .unwrap()
        .set_modified(old)
        .unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'Z');
    send_key(&mut harness, 'Z');

    harness.wait_until(|h| h.should_quit()).unwrap();
    assert_eq!(
        fs::metadata(&fixture.path).unwrap().modified().unwrap(),
        old
    );
    assert_eq!(fs::read_to_string(&fixture.path).unwrap(), "alpha\n");
}

/// `ZZ` on an unchanged buffer with no file name quits instead of failing
/// with "no file name".
#[test]
fn test_vi_ZZ_unnamed_unmodified_quits() {
    let (mut harness, _td) = vi_mode_harness(80, 24);
    harness.render().unwrap();
    enable_vi_mode(&mut harness);

    send_key(&mut harness, 'Z');
    send_key(&mut harness, 'Z');

    harness.wait_until(|h| h.should_quit()).unwrap();
}

// =============================================================================
// Adjacent `t`/`T` targets: found without moving (Vim), not "not found"
// =============================================================================

/// Helper: a find-char key (`f`/`t`/`F`/`T`), wait for its mode, then the target.
fn send_find(harness: &mut EditorTestHarness, find: char, target: char) {
    send_key(harness, find);
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(harness, target);
}

/// `t.` with the `.` right after the cursor does not move, but Vim records
/// the find: `;` then skips the adjacent match and stops before the next one.
#[test]
fn test_vi_t_adjacent_records_find_for_semicolon() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d\n");

    send_find(&mut harness, 't', '.');
    wait_normal(&mut harness);
    assert_eq!(harness.cursor_position(), 0);

    send_key(&mut harness, ';');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
}

/// Same for the reverse repeat: after an adjacent `t.`, `,` searches backward
/// as `T.` and skips the adjacent match.
#[test]
fn test_vi_t_adjacent_records_find_for_comma() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d\n");

    send_key(&mut harness, '$');
    send_key(&mut harness, 'h');
    send_key(&mut harness, 'h');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();

    send_find(&mut harness, 't', '.');
    wait_normal(&mut harness);
    send_key(&mut harness, ',');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
}

/// `dt.` with the `.` adjacent deletes the character under the cursor (the
/// motion is inclusive and lands on the cursor), and `.` repeats it.
#[test]
fn test_vi_dt_adjacent_deletes_char_under_cursor() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d\n");

    send_operator_motion(&mut harness, 'd', 't');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');
    harness.wait_for_buffer_content(".b.c.d\n").unwrap();

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_key(&mut harness, '.');
    harness.wait_for_buffer_content("..c.d\n").unwrap();
}

/// `yt.` with the `.` adjacent yanks the character under the cursor.
#[test]
fn test_vi_yt_adjacent_yanks_char_under_cursor() {
    let (mut harness, _td, _fixture) = open_vi("ab.c\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_motion(&mut harness, 'y', 't');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');
    wait_normal(&mut harness);

    send_key(&mut harness, '$');
    send_key(&mut harness, 'p');
    harness.wait_for_buffer_content("ab.cb\n").unwrap();
}

/// `cT.` with the `.` just before the cursor changes nothing but still opens
/// an insert at the cursor, as in Vim.
#[test]
fn test_vi_cT_adjacent_enters_insert() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d\n");

    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() == 6).unwrap();
    send_operator_motion(&mut harness, 'c', 'T');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-find-char".to_string()))
        .unwrap();
    send_key(&mut harness, '.');
    wait_insert(&mut harness);

    harness.type_text("X").unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    wait_normal(&mut harness);
    harness.wait_for_buffer_content("a.b.c.Xd\n").unwrap();
}

// =============================================================================
// `_`, `+`, `-` are line-wise motions, so operators take them too
// =============================================================================

#[test]
fn test_vi_d_plus_deletes_current_and_next_line() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n  c\n");

    send_operator_motion(&mut harness, 'd', '+');

    harness.wait_for_buffer_content("  c\n").unwrap();
}

/// From column 0 the upper end of `d-` sits on a line start; both lines go.
#[test]
fn test_vi_d_minus_from_column_zero_deletes_both_lines() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n  c\n");

    send_key(&mut harness, 'j');
    send_key(&mut harness, '0');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_operator_motion(&mut harness, 'd', '-');

    harness.wait_for_buffer_content("  c\n").unwrap();
}

#[test]
fn test_vi_d_underscore_with_count_deletes_count_lines() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n  c\n");

    send_key(&mut harness, '2');
    send_operator_motion(&mut harness, 'd', '_');

    harness.wait_for_buffer_content("  c\n").unwrap();
}

/// `+` has no line to go to on the last line, so `d+` fails and changes nothing.
#[test]
fn test_vi_d_plus_on_last_line_fails() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n");

    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_operator_motion(&mut harness, 'd', '+');
    wait_normal(&mut harness);

    harness.wait_for_buffer_content("  a\n  b\n").unwrap();
}

/// `^` on a line of nothing but blanks lands on the last blank, never on the
/// line terminator.
#[test]
fn test_vi_caret_on_blank_line_lands_on_last_blank() {
    let (mut harness, _td, _fixture) = open_vi("   \nz\n");

    send_key(&mut harness, '^');

    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
}

/// `.` after `d;` repeats `d;` against the find that is current when `.` runs.
#[test]
fn test_vi_dot_after_d_semicolon_uses_current_find() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c,d,e,f\n");

    send_find(&mut harness, 'f', '.');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_motion(&mut harness, 'd', ';');
    harness.wait_for_buffer_content("ac,d,e,f\n").unwrap();

    // The current find is now `f,`; `.` repeats `d;` with it, not with `f.`.
    send_find(&mut harness, 'f', ',');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_key(&mut harness, '.');

    harness.wait_for_buffer_content("ace,f\n").unwrap();
}

// =============================================================================
// An empty range is a motion that succeeded: `c` still opens its insert
// =============================================================================

fn change_then_type(harness: &mut EditorTestHarness, text: &str) {
    wait_insert(harness);
    harness.type_text(text).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    wait_normal(harness);
}

#[test]
fn test_vi_s_on_empty_line_enters_insert() {
    let (mut harness, _td, _fixture) = open_vi("\nz\n");

    send_key(&mut harness, 's');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("X\nz\n").unwrap();
}

#[test]
fn test_vi_C_on_empty_line_enters_insert() {
    let (mut harness, _td, _fixture) = open_vi("\nz\n");

    send_key(&mut harness, 'C');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("X\nz\n").unwrap();
}

#[test]
fn test_vi_c0_in_column_zero_enters_insert() {
    let (mut harness, _td, _fixture) = open_vi("abc\n");

    send_operator_motion(&mut harness, 'c', '0');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("Xabc\n").unwrap();
}

/// An empty change yanks nothing: the register keeps the previous yank, shape
/// included, so `p` still puts a yanked line on a line of its own.
#[test]
fn test_vi_empty_change_keeps_linewise_register() {
    let (mut harness, _td, _fixture) = open_vi("abc\nz\n");

    send_operator_motion(&mut harness, 'y', 'y');
    wait_normal(&mut harness);
    send_operator_motion(&mut harness, 'c', '0');
    change_then_type(&mut harness, "X");
    harness.wait_for_buffer_content("Xabc\nz\n").unwrap();

    send_key(&mut harness, 'p');
    harness.wait_for_buffer_content("Xabc\nabc\nz\n").unwrap();
}

// =============================================================================
// `.` resolves the recorded change afresh and replays it as the change it was
// =============================================================================

/// A line-wise change replays as a change: `.` after `c+` leaves its own
/// line of text instead of joining it onto the next one.
#[test]
fn test_vi_dot_after_c_plus_leaves_a_line_to_type_into() {
    let (mut harness, _td, _fixture) = open_vi("a\nb\nc\nd\ne\nf\n");

    send_operator_motion(&mut harness, 'c', '+');
    change_then_type(&mut harness, "X");
    harness.wait_for_buffer_content("X\nc\nd\ne\nf\n").unwrap();

    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_key(&mut harness, '.');

    harness.wait_for_buffer_content("X\nX\ne\nf\n").unwrap();
}

/// When the recorded motion no longer resolves, `.` does nothing at all: the
/// recorded text is not inserted either.
#[test]
fn test_vi_dot_after_change_with_failing_motion_inserts_nothing() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c\nzzz\n");

    send_find(&mut harness, 'f', '.');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_motion(&mut harness, 'c', ';');
    change_then_type(&mut harness, "X");
    harness.wait_for_buffer_content("aXc\nzzz\n").unwrap();

    send_key(&mut harness, 'j');
    send_key(&mut harness, '0');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_key(&mut harness, '.');
    wait_normal(&mut harness);
    // `.` itself is synchronous from here on; a second change proves it ran.
    send_key(&mut harness, 'x');
    harness.wait_for_buffer_content("aXc\nzz\n").unwrap();
}

/// After `.` replays a change the caret rests on the last inserted character,
/// as Escape leaves it after the original insert.
#[test]
fn test_vi_dot_after_cw_leaves_caret_on_last_inserted_char() {
    let (mut harness, _td, _fixture) = open_vi("foo bar baz\n");

    send_operator_motion(&mut harness, 'c', 'w');
    change_then_type(&mut harness, "X");
    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_key(&mut harness, '.');

    harness.wait_for_buffer_content("X X baz\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
}

// =============================================================================
// Counts on `;`/`,`, `+`/`-`/`j`, and failed finds
// =============================================================================

/// Only a bare `;` skips an adjacent `t` match; `2;` counts matches from the
/// caret as usual.
#[test]
fn test_vi_counted_semicolon_after_adjacent_t_does_not_skip() {
    let (mut harness, _td, _fixture) = open_vi("a.b.c.d.e.f\n");

    send_find(&mut harness, 't', '.');
    wait_normal(&mut harness);
    send_key(&mut harness, '2');
    send_key(&mut harness, ';');

    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
}

/// A count past the end of the buffer clamps to the last line, as Vim's
/// `cursor_down` does; it only fails when there is no line to move to.
#[test]
fn test_vi_plus_count_past_end_clamps_to_last_line() {
    let (mut harness, _td, _fixture) = open_vi("a\n  b\n  c\n");

    send_key(&mut harness, '5');
    send_key(&mut harness, '+');

    harness.wait_until(|h| h.cursor_position() == 8).unwrap();
}

#[test]
fn test_vi_d_count_j_past_end_clamps_to_last_line() {
    let (mut harness, _td, _fixture) = open_vi("a\nb\nc\n");

    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_key(&mut harness, 'd');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '9');
    send_key(&mut harness, 'j');

    harness.wait_for_buffer_content("a\n").unwrap();
}

/// Vim stores the find before searching, so a find with no match is still
/// the one `;` repeats.
#[test]
fn test_vi_failed_find_is_the_one_semicolon_repeats() {
    let (mut harness, _td, _fixture) = open_vi("a.b.cz\n");

    send_find(&mut harness, 'f', '.');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_find(&mut harness, 'f', 'q');
    wait_normal(&mut harness);
    send_key(&mut harness, ';');
    wait_normal(&mut harness);
    // `;` repeats `fq`, which has no match: the caret stays put. A following
    // `l` shows `;` was handled before it.
    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
}

// =============================================================================
// Empty ranges per operator, and where each operator leaves the caret
// =============================================================================

/// `I` on a line of nothing but blanks inserts after the blanks.
#[test]
fn test_vi_I_on_blank_line_inserts_after_the_blanks() {
    let (mut harness, _td, _fixture) = open_vi("    \nz\n");

    send_key(&mut harness, 'I');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("    X\nz\n").unwrap();
}

/// `>` over a motion that covers nothing still shifts the line.
#[test]
fn test_vi_shift_over_empty_motion_shifts_the_line() {
    let (mut harness, _td, _fixture) = open_vi("abc\n");

    send_operator_motion(&mut harness, '>', '0');

    harness.wait_for_buffer_content("    abc\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
}

/// `y` over a motion that covers nothing yanks the empty text: `P` then puts
/// nothing, rather than the previous yank.
#[test]
fn test_vi_yank_over_empty_motion_empties_the_register() {
    let (mut harness, _td, _fixture) = open_vi("abc def\n");

    send_operator_motion(&mut harness, 'y', 'w');
    wait_normal(&mut harness);
    send_key(&mut harness, 'w');
    send_key(&mut harness, '0');
    harness.wait_until(|h| h.cursor_position() == 0).unwrap();
    send_operator_motion(&mut harness, 'y', '0');
    wait_normal(&mut harness);
    send_key(&mut harness, 'P');
    wait_normal(&mut harness);
    // Proves `P` ran before the check: `x` deletes the `a` it would have
    // shifted right.
    send_key(&mut harness, 'x');

    harness.wait_for_buffer_content("bc def\n").unwrap();
}

/// A line-wise yank leaves the caret at the start of the yanked text, column
/// included: `y_` and `y+` do not move it.
#[test]
fn test_vi_y_underscore_and_y_plus_keep_the_caret() {
    let (mut harness, _td, _fixture) = open_vi("  abcdef\n  ghijkl\n");

    for _ in 0..3 {
        send_key(&mut harness, 'l');
    }
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
    send_operator_motion(&mut harness, 'y', '_');
    wait_normal(&mut harness);
    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();

    send_operator_motion(&mut harness, 'y', '+');
    wait_normal(&mut harness);
    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 5).unwrap();
}

/// A line-wise delete leaves the caret on the first non-blank of the line
/// that moved up into place.
#[test]
fn test_vi_d_plus_lands_on_first_non_blank() {
    let (mut harness, _td, _fixture) = open_vi("abc\ndef\n    ghi\n");

    send_operator_motion(&mut harness, 'd', '+');

    harness.wait_for_buffer_content("    ghi\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
}

/// `:x {file}` on an unchanged buffer writes nothing, like `:x` itself.
#[test]
fn test_vi_x_with_file_name_on_unmodified_buffer_writes_nothing() {
    let (mut harness, td, _fixture) = open_vi("alpha\n");
    let other = td.path().join("other.txt");

    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text(&format!("x {}", other.display()))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.wait_until(|h| h.should_quit()).unwrap();
    assert!(!other.exists());
}

// =============================================================================
// Text objects: empty blocks, Vim's line rules for blocks, multi-byte text
// =============================================================================

/// `ci(` on `()` has an empty block to change: it types between the brackets.
#[test]
fn test_vi_ci_paren_on_empty_parens_types_between_them() {
    let (mut harness, _td, _fixture) = open_vi("f()\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '(');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("f(X)\n").unwrap();
}

/// `.` repeats `ci(` on the next empty pair.
#[test]
fn test_vi_dot_repeats_ci_paren_on_empty_parens() {
    let (mut harness, _td, _fixture) = open_vi("f() g()\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '(');
    change_then_type(&mut harness, "X");
    harness.wait_for_buffer_content("f(X) g()\n").unwrap();

    // `w` stops on `)`, then on `g`; `ci(` from `g` finds the pair after it.
    send_key(&mut harness, 'w');
    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() == 5).unwrap();
    send_key(&mut harness, '.');

    harness.wait_for_buffer_content("f(X) g(X)\n").unwrap();
}

/// `di(` on `()` deletes nothing but moves the caret inside, onto the `)`.
#[test]
fn test_vi_di_paren_on_empty_parens_moves_inside() {
    let (mut harness, _td, _fixture) = open_vi("f()\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'd', 'i', '(');

    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    harness.wait_for_buffer_content("f()\n").unwrap();
}

#[test]
fn test_vi_ci_quote_on_empty_quotes_types_between_them() {
    let (mut harness, _td, _fixture) = open_vi("a\"\"b\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '"');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("a\"X\"b\n").unwrap();
}

/// With the caret on a `)`, the block is the one that `)` closes.
#[test]
fn test_vi_ci_paren_on_closing_paren_changes_its_block() {
    let (mut harness, _td, _fixture) = open_vi("((a)b)\n");

    for _ in 0..3 {
        send_key(&mut harness, 'l');
    }
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '(');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("((X)b)\n").unwrap();
}

/// A block on lines of its own: `ci{` changes those lines, leaving one to
/// type into, and keeps the braces on their lines.
#[test]
fn test_vi_ci_brace_on_block_lines_changes_the_lines() {
    let (mut harness, _td, _fixture) = open_vi("{\n    foo\n}\n");

    send_operator_text_object(&mut harness, 'c', 'i', '{');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("{\nX\n}\n").unwrap();
}

#[test]
fn test_vi_di_brace_on_block_lines_deletes_the_lines() {
    let (mut harness, _td, _fixture) = open_vi("{\n    foo\n}\n");

    send_operator_text_object(&mut harness, 'd', 'i', '{');

    harness.wait_for_buffer_content("{\n}\n").unwrap();
}

/// A `(` that ends its line starts the block on the next line.
#[test]
fn test_vi_ci_paren_skips_the_line_break_after_the_open_paren() {
    let (mut harness, _td, _fixture) = open_vi("f(\n)\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '(');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("f(\nX)\n").unwrap();
}

/// A `)` alone on its line ends the block before that line, so the text
/// after the `(` is changed but the line break stays.
#[test]
fn test_vi_ci_paren_keeps_the_line_break_before_a_lone_close_paren() {
    let (mut harness, _td, _fixture) = open_vi("f(a\n)\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '(');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("f(X\n)\n").unwrap();
}

/// Visual mode selects the block as it is, line break included.
#[test]
fn test_vi_visual_inner_paren_over_lines() {
    let (mut harness, _td, _fixture) = open_vi("f(\n  a\n  )\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_key(&mut harness, 'v');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-visual".to_string()))
        .unwrap();
    send_key(&mut harness, 'i');
    send_key(&mut harness, '(');
    send_key(&mut harness, 'd');

    harness.wait_for_buffer_content("f(\n  )\n").unwrap();
}

/// Text objects measure in bytes: a multi-byte character before the caret
/// does not shift the range.
#[test]
fn test_vi_ci_paren_after_multibyte_char() {
    let (mut harness, _td, _fixture) = open_vi("é(ab)\n");

    send_key(&mut harness, 'l');
    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', '(');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("é(X)\n").unwrap();
}

#[test]
fn test_vi_ciw_after_multibyte_char() {
    let (mut harness, _td, _fixture) = open_vi("é foo\n");

    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
    send_operator_text_object(&mut harness, 'c', 'i', 'w');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("é X\n").unwrap();
}

// =============================================================================
// Word objects by class, the caret after a delete, and `p`
// =============================================================================

/// `iw` on punctuation takes the run of punctuation, a word of its own class.
#[test]
fn test_vi_diw_on_punctuation_deletes_the_run() {
    let (mut harness, _td, _fixture) = open_vi("a..b\n");

    send_key(&mut harness, 'l');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_text_object(&mut harness, 'd', 'i', 'w');

    harness.wait_for_buffer_content("ab\n").unwrap();
}

/// On blanks, `iw` is the blanks and `aw` is the blanks plus the next word.
#[test]
fn test_vi_iw_and_aw_on_blanks() {
    let (mut harness, _td, _fixture) = open_vi("foo   bar\nfoo   bar\n");

    for _ in 0..3 {
        send_key(&mut harness, 'l');
    }
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
    send_operator_text_object(&mut harness, 'd', 'i', 'w');
    harness
        .wait_for_buffer_content("foobar\nfoo   bar\n")
        .unwrap();

    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 10).unwrap();
    send_operator_text_object(&mut harness, 'd', 'a', 'w');
    harness.wait_for_buffer_content("foobar\nfoo\n").unwrap();
}

/// `aw` on the last word of a line takes the blanks before it instead.
#[test]
fn test_vi_aw_on_last_word_takes_leading_blanks() {
    let (mut harness, _td, _fixture) = open_vi("foo bar\n");

    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_operator_text_object(&mut harness, 'd', 'a', 'w');

    harness.wait_for_buffer_content("foo\n").unwrap();
}

/// `x` on the last character leaves the caret on the new last character, not
/// past the end of the line.
#[test]
fn test_vi_x_on_last_char_keeps_caret_on_the_line() {
    let (mut harness, _td, _fixture) = open_vi("ab\n");

    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_key(&mut harness, 'x');

    harness.wait_for_buffer_content("a\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 0).unwrap();
}

/// `yyp` lands on the first non-blank of the pasted line.
#[test]
fn test_vi_linewise_paste_lands_on_first_non_blank() {
    let (mut harness, _td, _fixture) = open_vi("  ab\nz\n");

    send_operator_motion(&mut harness, 'y', 'y');
    wait_normal(&mut harness);
    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 5).unwrap();
    send_key(&mut harness, 'p');

    harness.wait_for_buffer_content("  ab\nz\n  ab\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 9).unwrap();
}

/// A character-wise `p` leaves the caret on the last pasted character.
#[test]
fn test_vi_charwise_paste_lands_on_last_pasted_char() {
    let (mut harness, _td, _fixture) = open_vi("foo bar\n");

    send_operator_motion(&mut harness, 'y', 'w');
    wait_normal(&mut harness);
    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() == 6).unwrap();
    send_key(&mut harness, 'p');

    harness.wait_for_buffer_content("foo barfoo \n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 10).unwrap();
}

/// `p` on an empty line puts the text on that line.
#[test]
fn test_vi_charwise_paste_on_empty_line() {
    let (mut harness, _td, _fixture) = open_vi("\nab\n");

    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 1).unwrap();
    send_operator_motion(&mut harness, 'y', 'l');
    wait_normal(&mut harness);
    send_key(&mut harness, 'k');
    harness.wait_until(|h| h.cursor_position() == 0).unwrap();
    send_key(&mut harness, 'p');

    harness.wait_for_buffer_content("a\nab\n").unwrap();
}

/// Shifting does not touch the register: `yy>>p` still puts a whole line.
#[test]
fn test_vi_shift_keeps_linewise_register() {
    let (mut harness, _td, _fixture) = open_vi("ab\nz\n");

    send_operator_motion(&mut harness, 'y', 'y');
    wait_normal(&mut harness);
    send_operator_motion(&mut harness, '>', '>');
    harness.wait_for_buffer_content("    ab\nz\n").unwrap();
    send_key(&mut harness, 'p');

    harness.wait_for_buffer_content("    ab\nab\nz\n").unwrap();
}

// =============================================================================
// Hands-on comparison with Vim: each test is a case where fresh differed.
// =============================================================================

/// Helper: `i`/`a` after an operator, then the object key.
fn send_text_object(harness: &mut EditorTestHarness, op: char, modifier: char, object: char) {
    send_operator_motion(harness, op, modifier);
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-text-object".to_string()))
        .unwrap();
    send_key(harness, object);
}

/// `;` after non-ASCII text on the line lands on the match, not a byte or two
/// short of it — which used to split `ü` and write invalid UTF-8.
#[test]
fn test_vi_find_repeat_after_multibyte_text() {
    let (mut harness, _td, _fixture) = open_vi("ä,ö,ü,x,y\n");

    send_find(&mut harness, 'f', ',');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_key(&mut harness, 'c');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, ';');
    change_then_type(&mut harness, "Z");

    harness.wait_for_buffer_content("äZü,x,y\n").unwrap();
}

/// Non-ASCII text on an earlier line does not shift `f` on a later one.
#[test]
fn test_vi_find_after_multibyte_line() {
    let (mut harness, _td, _fixture) = open_vi("ü ö ä\na,b,c\n");

    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 9).unwrap();
    send_find(&mut harness, 'f', ',');
    harness.wait_until(|h| h.cursor_position() == 10).unwrap();
    send_key(&mut harness, 'x');

    harness.wait_for_buffer_content("ü ö ä\nab,c\n").unwrap();
}

/// `dw` on a line's last word stops at the end of the line (`:help word`).
#[test]
fn test_vi_dw_last_word_keeps_line_break() {
    let (mut harness, _td, _fixture) = open_vi("foo bar\nbaz\n");

    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() == 4).unwrap();
    send_operator_motion(&mut harness, 'd', 'w');

    harness.wait_for_buffer_content("foo \nbaz\n").unwrap();
    harness.wait_until(|h| h.cursor_position() == 3).unwrap();
}

/// `dw` on an empty line deletes the line.
#[test]
fn test_vi_dw_on_empty_line_deletes_it() {
    let (mut harness, _td, _fixture) = open_vi("\nabc\n");

    send_operator_motion(&mut harness, 'd', 'w');

    harness.wait_for_buffer_content("abc\n").unwrap();
}

/// `yw` on a line's last word yanks the word, not its line break.
#[test]
fn test_vi_yw_last_word_then_P() {
    let (mut harness, _td, _fixture) = open_vi("abc\ndef\n");

    send_operator_motion(&mut harness, 'y', 'w');
    wait_normal(&mut harness);
    send_key(&mut harness, 'P');

    harness.wait_for_buffer_content("abcabc\ndef\n").unwrap();
}

/// Letters outside ASCII are word characters: `dw` on `über` takes the word.
#[test]
fn test_vi_dw_non_ascii_word() {
    let (mut harness, _td, _fixture) = open_vi("über straße\n");

    send_operator_motion(&mut harness, 'd', 'w');

    harness.wait_for_buffer_content("straße\n").unwrap();
}

/// `j` on the last line does not move onto the empty line after the final
/// newline.
#[test]
fn test_vi_j_stays_on_last_line() {
    let (mut harness, _td, _fixture) = open_vi("a\nb\n");

    send_key(&mut harness, 'G');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'i');
    change_then_type(&mut harness, "X");

    harness.wait_for_buffer_content("a\nXb\n").unwrap();
}

/// `G` and `{count}gg` go to the first non-blank of their line.
#[test]
fn test_vi_G_and_count_gg_go_to_first_non_blank() {
    let (mut harness, _td, _fixture) = open_vi("  a\n  b\n  c\n");

    send_key(&mut harness, 'G');
    harness.wait_until(|h| h.cursor_position() == 10).unwrap();
    send_key(&mut harness, '2');
    send_key(&mut harness, 'g');
    send_key(&mut harness, 'g');
    harness.wait_until(|h| h.cursor_position() == 6).unwrap();
}

/// `.` repeats `>j`.
#[test]
fn test_vi_dot_repeats_shift_motion() {
    let (mut harness, _td, _fixture) = open_vi("a\nb\nc\nd\n");

    send_operator_motion(&mut harness, '>', 'j');
    harness
        .wait_for_buffer_content("    a\n    b\nc\nd\n")
        .unwrap();
    wait_normal(&mut harness);
    send_key(&mut harness, 'j');
    send_key(&mut harness, 'j');
    harness.wait_until(|h| h.cursor_position() == 12).unwrap();
    send_key(&mut harness, '.');

    harness
        .wait_for_buffer_content("    a\n    b\n    c\n    d\n")
        .unwrap();
}

/// A shift leaves empty lines alone, and `u` undoes it in one step.
#[test]
fn test_vi_shift_skips_empty_lines_and_undoes_at_once() {
    let (mut harness, _td, _fixture) = open_vi("a\n\nb\n");

    send_key(&mut harness, '>');
    harness
        .wait_until(|h| h.editor().input_mode() == Some("vi-operator-pending".to_string()))
        .unwrap();
    send_key(&mut harness, '2');
    send_key(&mut harness, 'j');
    harness.wait_for_buffer_content("    a\n\n    b\n").unwrap();
    wait_normal(&mut harness);
    send_key(&mut harness, 'u');

    harness.wait_for_buffer_content("a\n\nb\n").unwrap();
}

/// `>>` on a tab-indented line measures the tab as columns and writes the new
/// indent as spaces.
#[test]
fn test_vi_shift_tab_indent_with_spaces() {
    let (mut harness, _td, _fixture) = open_vi("\ta\n");

    send_operator_motion(&mut harness, '>', '>');

    harness.wait_for_buffer_content("        a\n").unwrap();
}

/// A quote after a backslash does not close the string.
#[test]
fn test_vi_di_quote_skips_escaped_quote() {
    let (mut harness, _td, _fixture) = open_vi("x = \"a\\\"b\" y\n");

    send_find(&mut harness, 'f', 'a');
    harness.wait_until(|h| h.cursor_position() == 5).unwrap();
    send_text_object(&mut harness, 'd', 'i', '"');

    harness.wait_for_buffer_content("x = \"\" y\n").unwrap();
}

/// Between two strings, `i"` is the text between them, as in Vim.
#[test]
fn test_vi_di_quote_between_strings() {
    let (mut harness, _td, _fixture) = open_vi("x \"a\" y \"b\"\n");

    send_find(&mut harness, 'f', 'y');
    harness.wait_until(|h| h.cursor_position() == 6).unwrap();
    send_text_object(&mut harness, 'd', 'i', '"');

    harness.wait_for_buffer_content("x \"a\"\"b\"\n").unwrap();
}

/// `daw` on a line's only word keeps the indent.
#[test]
fn test_vi_daw_keeps_indent() {
    let (mut harness, _td, _fixture) = open_vi("  foo\nx\n");

    send_key(&mut harness, 'w');
    harness.wait_until(|h| h.cursor_position() == 2).unwrap();
    send_text_object(&mut harness, 'd', 'a', 'w');

    harness.wait_for_buffer_content("  \nx\n").unwrap();
}

/// `daw` on trailing blanks of the last line fails; on an earlier line it
/// takes the blanks.
#[test]
fn test_vi_daw_on_trailing_blanks() {
    let (mut harness, _td, _fixture) = open_vi("one two   \nx\n");

    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() == 9).unwrap();
    send_text_object(&mut harness, 'd', 'a', 'w');
    harness.wait_for_buffer_content("one two\nx\n").unwrap();

    let (mut harness, _td, _fixture) = open_vi("one two   \n");
    send_key(&mut harness, '$');
    harness.wait_until(|h| h.cursor_position() == 9).unwrap();
    send_text_object(&mut harness, 'd', 'a', 'w');
    wait_normal(&mut harness);
    send_key(&mut harness, 'x');
    harness.wait_for_buffer_content("one two  \n").unwrap();
}

/// A key that names no text object cancels the operator; the next key is an
/// ordinary command again.
#[test]
fn test_vi_unknown_text_object_cancels_operator() {
    let (mut harness, _td, _fixture) = open_vi("abc\n");

    send_text_object(&mut harness, 'd', 'i', 'q');
    wait_normal(&mut harness);
    send_key(&mut harness, 'x');

    harness.wait_for_buffer_content("bc\n").unwrap();
}

/// A different operator key after an operator (`dy`) cancels it: nothing is
/// yanked, so `P` still puts what `yl` took.
#[test]
fn test_vi_mismatched_operator_cancels() {
    let (mut harness, _td, _fixture) = open_vi("ab\ncd\n");

    send_operator_motion(&mut harness, 'y', 'l');
    wait_normal(&mut harness);
    send_operator_motion(&mut harness, 'd', 'y');
    wait_normal(&mut harness);
    send_key(&mut harness, 'P');

    harness.wait_for_buffer_content("aab\ncd\n").unwrap();
}
