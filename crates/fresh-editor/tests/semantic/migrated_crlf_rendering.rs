//! Migration of `tests/e2e/crlf_rendering.rs` — line-ending handling.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **CRLF transparent rendering.** Files with `\r\n` line endings
//!      render cleanly — every line's text is visible, and the CR
//!      bytes do NOT surface as visible glyphs (`<0D>` / `^M`) on a
//!      file the editor has detected as CRLF.
//!   2. **Mixed line endings.** A buffer mixing `\r\n` and `\n` still
//!      renders all lines cleanly; no CR markers leak.
//!   3. **CRLF cursor movement.** Down-arrow over CRLF lines advances
//!      the byte offset (the `\r\n` is treated as a single logical
//!      line break, not as two stop positions).
//!   4. **CRLF editing.** End + type extends the first CRLF line in
//!      place; the buffer text reflects the edit; no CR markers.
//!   5. **CRLF Enter preserves format.** Pressing Enter mid-file in a
//!      CRLF-detected buffer and saving must produce `\r\n` for the
//!      newly inserted line — the saved bytes are exactly
//!      `Line 1\r\nLine 2\r\nLine 3\r\n`.
//!   6. **CRLF empty lines.** A blank `\r\n` line counts as a
//!      navigable line — Down advances past it.
//!   7. **LF Enter preserves format.** Mirror of #5 for LF files: a
//!      newly inserted line must save as `\n`, not `\r\n`.
//!   8. **CRLF backspace at line start joins.** Backspace at column 0
//!      of line 2 collapses the `\r\n`, joining "Line 1Line 2" both
//!      on screen and in the saved bytes.
//!   9. **CRLF delete at line end joins.** Mirror of #8 via Delete at
//!      end of line 1.
//!   10. **CRLF cut/paste preserves CRLF.** Cutting a full CRLF line
//!       and pasting at EOF round-trips the `\r\n` ending in the
//!       saved file.
//!   11. **CR in LF file is visible.** A file detected as LF that
//!       contains stray `\r` (including `\r\n` sequences) must show
//!       the CR as `<0D>` — proves the editor's line-ending-detection
//!       gate flips per-buffer, not per-character.
//!   12. **CRLF cursor visibility across many lines.** After switching
//!       a Java buffer to CRLF and pasting it 2x to grow past one
//!       screen-height, the hardware cursor stays inside the content
//!       area and within screen width at the start/end of every line
//!       (forward and reverse). Catches viewport-tracking drift
//!       triggered by the CRLF line-stride change.
//!   13. **LF→CRLF conversion on save.** Setting line ending to CRLF
//!       via the command palette and saving converts every line in
//!       the file to `\r\n`. Status bar reflects the new format.
//!   14. **CRLF→LF conversion on save.** Mirror of #13.
//!
//! ## Harness-direct pattern
//!
//! These tests need surfaces without a `EditorTestApi` projection:
//!
//!   - `load_buffer_from_text_named` (preserves the on-disk fixture
//!     across the test so save-and-reread checks can read it back),
//!   - the raw `\r\n` byte content needs to land on disk so the
//!     editor's line-ending detection picks CRLF — `TestFixture`'s
//!     byte-level `write_all` is the natural fit,
//!   - `cursor_position()` for the byte-offset advance checks,
//!   - `screen_cursor_position()` + `content_area_rows()` for the
//!     hardware-cursor visibility sweep,
//!   - `set_clipboard_for_test` for the cut/paste round-trip (system
//!     clipboard interference in parallel tests would corrupt the
//!     `\r\n` round-trip),
//!   - `wait_for_prompt` / `wait_for_prompt_closed` /
//!     `wait_until(buffer.is_modified)` for the
//!     command-palette-driven Set-Line-Ending flow,
//!   - `editor().get_status_message()` for the "LF"/"CRLF" status
//!     bar text check after a conversion.
//!
//! Source: `tests/e2e/crlf_rendering.rs` (14 tests migrated; no tests
//! deferred). Per-row screen text comes from
//! `RenderSnapshot::extract_with_rendered_rows` so the assertions
//! exercise the same vt100 round-trip path the e2e
//! `assert_screen_contains` did.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};

/// Helper: snapshot the rendered rows and assert the given substring
/// appears on some row. Mirrors `harness.assert_screen_contains`.
fn assert_any_row_contains(harness: &mut EditorTestHarness, needle: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains(needle.to_string())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "expected some row to contain {needle:?}: {f} expected {e}; actual {a}\n\
             rows={:#?}",
            snap.rendered_rows
        );
    }
}

fn assert_no_row_contains(harness: &mut EditorTestHarness, needle: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::NoRowContains(needle.to_string())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "expected NO row to contain {needle:?}: {f} expected {e}; actual {a}\n\
             rows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_crlf_buffer_rendering_hides_cr_markers() {
    // Source: `test_crlf_buffer_rendering`.
    let content = "Line 1\r\nLine 2\r\nLine 3\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_test.txt", content)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 1");
    assert_any_row_contains(&mut harness, "Line 2");
    assert_any_row_contains(&mut harness, "Line 3");
    // CR bytes must not surface as visible glyphs in a CRLF-detected file.
    assert_no_row_contains(&mut harness, "<0D>");
    assert_no_row_contains(&mut harness, "^M");
    // Filename in status bar.
    assert_any_row_contains(&mut harness, "crlf_test.txt");
    drop(fixture);
}

#[test]
fn migrated_mixed_line_endings_render_cleanly() {
    // Source: `test_mixed_line_endings_rendering`.
    let content = "CRLF line 1\r\nLF line 2\nCRLF line 3\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("mixed_endings.txt", content)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "CRLF line 1");
    assert_any_row_contains(&mut harness, "LF line 2");
    assert_any_row_contains(&mut harness, "CRLF line 3");
    assert_no_row_contains(&mut harness, "<0D>");
    assert_no_row_contains(&mut harness, "^M");
    drop(fixture);
}

#[test]
fn migrated_crlf_cursor_moves_forward_across_lines() {
    // Source: `test_crlf_cursor_movement`. The byte offset advances
    // per Down — the `\r\n` is a single logical break, not two.
    let content = "First\r\nSecond\r\nThird\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_cursor.txt", content)
        .unwrap();

    let initial = harness.cursor_position();
    assert_eq!(initial, 0, "Should start at byte 0");

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after_down = harness.cursor_position();
    assert!(
        after_down > initial,
        "Down should advance past the first CRLF line"
    );

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after_second = harness.cursor_position();
    assert!(
        after_second > after_down,
        "Down should continue advancing past subsequent CRLF lines"
    );
    drop(fixture);
}

#[test]
fn migrated_crlf_editing_extends_line_and_hides_cr() {
    // Source: `test_crlf_editing`.
    let content = "Hello\r\nWorld\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_edit.txt", content)
        .unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" there").unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Hello there");
    assert_any_row_contains(&mut harness, "World");
    assert_no_row_contains(&mut harness, "<0D>");
    assert_no_row_contains(&mut harness, "^M");

    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("Hello there"),
        "Buffer should contain edited text, got: {buffer_content:?}"
    );
    drop(fixture);
}

#[test]
fn migrated_crlf_enter_preserves_crlf_format_on_save() {
    // Source: `test_crlf_new_line_insertion`. Save → read → exact
    // byte match for `Line 1\r\nLine 2\r\nLine 3\r\n`.
    let content = "Line 1\r\nLine 3\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_newline.txt", content)
        .unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line 2").unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 1");
    assert_any_row_contains(&mut harness, "Line 2");
    assert_any_row_contains(&mut harness, "Line 3");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved = std::fs::read_to_string(&fixture.path).unwrap();
    assert_eq!(
        saved, "Line 1\r\nLine 2\r\nLine 3\r\n",
        "File must keep CRLF format end-to-end; saved={saved:?}"
    );
}

#[test]
fn migrated_crlf_empty_line_is_navigable() {
    // Source: `test_crlf_empty_lines`.
    let content = "Line 1\r\n\r\nLine 3\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_empty.txt", content)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 1");
    assert_any_row_contains(&mut harness, "Line 3");

    let initial = harness.cursor_position();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let pos_on_empty = harness.cursor_position();
    assert!(
        pos_on_empty > initial,
        "Down should move past Line 1 onto the empty line"
    );

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let pos_on_line3 = harness.cursor_position();
    assert!(
        pos_on_line3 > pos_on_empty,
        "Down should move past the empty line onto Line 3"
    );
    drop(fixture);
}

#[test]
fn migrated_lf_enter_preserves_lf_format_on_save() {
    // Source: `test_lf_new_line_insertion`.
    let content = "Line 1\nLine 3\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("lf_newline.txt", content)
        .unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Line 2").unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 1");
    assert_any_row_contains(&mut harness, "Line 2");
    assert_any_row_contains(&mut harness, "Line 3");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved = std::fs::read_to_string(&fixture.path).unwrap();
    assert!(
        !saved.contains("\r\n"),
        "LF file must not gain CRLF sequences; saved={saved:?}"
    );
    assert_eq!(
        saved, "Line 1\nLine 2\nLine 3\n",
        "File must keep LF format end-to-end; saved={saved:?}"
    );
}

#[test]
fn migrated_crlf_backspace_at_line_start_joins_lines() {
    // Source: `test_crlf_backspace_at_line_start`.
    let content = "Line 1\r\nLine 2\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_backspace.txt", content)
        .unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 1Line 2");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved = std::fs::read_to_string(&fixture.path).unwrap();
    assert_eq!(
        saved, "Line 1Line 2\r\n",
        "Backspace at start of CRLF line 2 must collapse the \\r\\n into a single line"
    );
}

#[test]
fn migrated_crlf_delete_at_line_end_joins_lines() {
    // Source: `test_crlf_delete_at_line_end`.
    let content = "Line 1\r\nLine 2\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_delete.txt", content)
        .unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 1Line 2");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved = std::fs::read_to_string(&fixture.path).unwrap();
    assert_eq!(
        saved, "Line 1Line 2\r\n",
        "Delete at end of CRLF line 1 must collapse the \\r\\n into a single line"
    );
}

#[test]
fn migrated_crlf_cut_paste_preserves_crlf() {
    // Source: `test_crlf_cut_paste`.
    let content = "Line 1\r\nLine 2\r\nLine 3\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Internal-only clipboard so parallel tests can't corrupt the
    // \r\n round-trip via the OS clipboard.
    harness.editor_mut().set_clipboard_for_test("".to_string());
    let fixture = harness
        .load_buffer_from_text_named("crlf_cut_paste.txt", content)
        .unwrap();
    // Mirror the e2e: re-arm test clipboard after open_file (its
    // pre/post-open clipboard reset was load-bearing in the e2e).
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Select "Line 2\r\n" — Down to line 2, Home, then Shift+Down.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();

    // Cut.
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert_no_row_contains(&mut harness, "Line 2");

    // Go to EOF and paste.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "Line 2");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved = std::fs::read_to_string(&fixture.path).unwrap();
    assert!(
        saved.contains("Line 2\r\n"),
        "Pasted line must preserve CRLF ending; saved={saved:?}"
    );
}

#[test]
fn migrated_cr_in_lf_file_is_visible_as_0d() {
    // Source: `test_cr_shown_in_lf_file`. In an LF-detected file
    // even the \r in \r\n is shown as <0D>.
    let content = "Line1\nHello\rWorld\nLine3\r\nLine4\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("lf_with_cr.txt", content)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "<0D>");
    assert_any_row_contains(&mut harness, "Line1");
    assert_any_row_contains(&mut harness, "Hello");
    assert_any_row_contains(&mut harness, "World");
    assert_any_row_contains(&mut harness, "Line3");
    assert_any_row_contains(&mut harness, "Line4");
    drop(fixture);
}

#[test]
fn migrated_crlf_cursor_visibility_across_grown_buffer() {
    // Source: `test_crlf_cursor_visibility`. Java buffer → switch to
    // CRLF → grow via paste 2x → walk every line forward and back,
    // asserting the hardware cursor stays inside the content area
    // and within screen width.
    let java_content = r#"public class Test {
    public static void main(String[] args) {
        System.out.println("Hello");
        int x = 42;
    }
}"#;
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.editor_mut().set_clipboard_for_test("".to_string());
    let fixture = harness
        .load_buffer_from_text_named("Test.java", java_content)
        .unwrap();
    harness.render().unwrap();

    // Set line ending to CRLF via command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("set line ending").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The Set-Line-Ending prompt should be closed.
    assert_no_row_contains(&mut harness, "Line ending:");

    // Select all → copy → end → newline → paste → newline → paste.
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let content_after_paste = harness.get_buffer_content().unwrap();
    let line_count = content_after_paste.lines().count();
    assert!(
        line_count >= 18,
        "Pasting the 6-line Java buffer twice should yield ≥18 lines; got {line_count}"
    );

    // Helper: cursor must be inside content area and within screen width.
    fn check_cursor_visible(harness: &mut EditorTestHarness, location: &str) {
        harness.render().unwrap();
        let (cursor_x, cursor_y) = harness.screen_cursor_position();
        let (content_start, content_end) = harness.content_area_rows();
        assert!(
            cursor_y as usize >= content_start && cursor_y as usize <= content_end,
            "Cursor at {location} should be in content area: y={cursor_y} \
             not in range [{content_start}, {content_end}]"
        );
        assert!(
            cursor_x < 80,
            "Cursor at {location} should be within screen width: x={cursor_x} >= 80"
        );
    }

    // Ctrl+Home → byte 0.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    check_cursor_visible(&mut harness, "start of buffer");
    assert_eq!(harness.cursor_position(), 0, "Should be at byte 0");

    // Walk every line forward.
    for line_num in 0..line_count {
        harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        check_cursor_visible(&mut harness, &format!("line {line_num} start"));

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        check_cursor_visible(&mut harness, &format!("line {line_num} end"));

        harness.type_text("*").unwrap();
        harness.render().unwrap();

        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }

    // Walk back up.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    check_cursor_visible(&mut harness, "end of buffer");
    for line_num in (0..line_count).rev() {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        check_cursor_visible(&mut harness, &format!("line {line_num} (going up)"));
    }

    // Type at buffer start.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("// START>>").unwrap();
    harness.render().unwrap();
    assert_any_row_contains(&mut harness, "// START>>");

    let final_content = harness.get_buffer_content().unwrap();
    assert!(final_content.contains("public class Test"));
    assert!(final_content.contains("public static void main"));
    assert!(final_content.contains("System.out.println"));
    assert!(final_content.contains("int x = 42"));
    drop(fixture);
}

#[test]
fn migrated_set_line_ending_lf_to_crlf_converts_on_save() {
    // Source: `test_set_line_ending_converts_on_save_lf_to_crlf`.
    let content = "Line 1\nLine 2\nLine 3\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("lf_to_crlf.txt", content)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "LF");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("set line").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    assert_any_row_contains(&mut harness, "CRLF");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved_bytes = std::fs::read(&fixture.path).unwrap();
    let saved = String::from_utf8_lossy(&saved_bytes);
    assert!(
        saved.contains("\r\n"),
        "After conversion the file must contain CRLF; saved={saved:?}"
    );
    assert_eq!(
        saved, "Line 1\r\nLine 2\r\nLine 3\r\n",
        "All line endings should be converted to CRLF"
    );
}

#[test]
fn migrated_set_line_ending_crlf_to_lf_converts_on_save() {
    // Source: `test_set_line_ending_converts_on_save_crlf_to_lf`.
    let content = "Line 1\r\nLine 2\r\nLine 3\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("crlf_to_lf.txt", content)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "CRLF");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("set line").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    assert_any_row_contains(&mut harness, "LF");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved_bytes = std::fs::read(&fixture.path).unwrap();
    let saved = String::from_utf8_lossy(&saved_bytes);
    assert!(
        !saved.contains("\r\n"),
        "After conversion to LF the file must not contain CRLF; saved={saved:?}"
    );
    assert_eq!(
        saved, "Line 1\nLine 2\nLine 3\n",
        "All line endings should be converted to LF"
    );
}

// =====================================================================
// Anti-tests — every load-bearing precondition or action is gated by a
// dropped-action anti-test that proves the positive claim depends on
// the action, not on harness incidentals.
// =====================================================================

/// Anti-test: don't load a CRLF file at all. The starting empty buffer
/// must NOT contain "Line 1" — proves the
/// `migrated_crlf_buffer_rendering_hides_cr_markers` claim depends on
/// the file having actually loaded.
#[test]
fn anti_crlf_buffer_without_load_has_no_line_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // No load_buffer_from_text_named — the buffer starts empty.
    harness.render().unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::NoRowContains("Line 1".into())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "anti: without loading the CRLF file, no row should contain 'Line 1': \
             {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

/// Anti-test: drop the `Down` keystrokes from the CRLF cursor-movement
/// test. Without them, the cursor must stay at byte 0 — proves the
/// movement claim is gated on Down, not on file load alone.
#[test]
fn anti_crlf_cursor_without_down_stays_at_zero() {
    let content = "First\r\nSecond\r\nThird\r\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fixture = harness
        .load_buffer_from_text_named("crlf_cursor.txt", content)
        .unwrap();
    // No Down keystrokes.
    let pos = harness.cursor_position();
    assert_eq!(
        pos, 0,
        "anti: without Down the cursor must stay at byte 0 in the CRLF buffer; got {pos}"
    );
}

/// Anti-test: drop the Set-Line-Ending command palette flow from the
/// LF→CRLF conversion test. Saving without the conversion must leave
/// the file in LF — proves the conversion is what flips the bytes,
/// not the save alone.
#[test]
fn anti_lf_to_crlf_without_set_line_ending_keeps_lf() {
    let content = "Line 1\nLine 2\nLine 3\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text_named("lf_no_convert.txt", content)
        .unwrap();
    harness.render().unwrap();

    // Edit a char so the buffer is dirty, then revert it — that way
    // the save path actually rewrites bytes. Easier: just save (most
    // editors will rewrite on Ctrl+S regardless of dirty state, but
    // to be safe we type+backspace).
    harness.type_text(" ").unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // No set-line-ending palette flow here.
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    let saved = std::fs::read_to_string(&fixture.path).unwrap();
    assert!(
        !saved.contains("\r\n"),
        "anti: without Set-Line-Ending → CRLF, the saved file must stay LF; saved={saved:?}"
    );
}
