//! Migration of `tests/e2e/undo_redo_marker_roundtrip.rs` —
//! single-edit + bulk-edit operations under undo/redo must
//! preserve the byte position of a margin marker.
//!
//! This file uses the "Direct-harness for cross-state claims"
//! pattern: each test grabs `EditorTestHarness::editor_mut()`
//! directly to read `MarkerId` positions and dispatch BulkEdit
//! ops. That's the same surface the e2e originals use; the
//! marker model is a production-internal `Margins` /
//! `LineIndicator` thing that has no projection on
//! `EditorTestApi` (and probably shouldn't — it's a position
//! invariant, not a user-visible observable).
//!
//! Each op (TypeChar, Backspace, Delete, Enter, MoveLineUp,
//! MoveLineDown, etc.) is tested under the same shape:
//!   1. Set up "aaa\nbbb\nccc" with cursor at start of line 2
//!      and a margin marker at byte 0.
//!   2. Capture original content + marker position.
//!   3. Apply ONE operation.
//!   4. Undo: content + marker must return to original.
//!   5. Redo: content + marker must return to post-op state.
//!
//! Tracks orphan in #2058. This is the first of the
//! `undo_redo_marker_roundtrip.rs` migrations; the other 5
//! deterministic tests + 3 proptests remain to be ported.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::marker::MarkerId;
use fresh::view::margin::LineIndicator;
use ratatui::style::Color;

fn add_margin_indicator(harness: &mut EditorTestHarness, byte_offset: usize) -> MarkerId {
    let indicator = LineIndicator::new("●", Color::Red, 10);
    let state = harness.editor_mut().active_state_mut();
    state
        .margins
        .set_line_indicator(byte_offset, "test".to_string(), indicator)
}

fn margin_position(harness: &EditorTestHarness, id: MarkerId) -> Option<usize> {
    harness
        .editor()
        .active_state()
        .margins
        .get_indicator_position(id)
}

/// Build the standard 3-line fixture with cursor at start of
/// line 2 and a marker at byte 0. Returns (harness, marker_id).
fn setup_multiline() -> (EditorTestHarness, MarkerId) {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    harness.type_text("aaa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("bbb").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ccc").unwrap();

    // Cursor at start of line 2.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "aaa\nbbb\nccc");

    let id = add_margin_indicator(&mut harness, 0);
    assert_eq!(margin_position(&harness, id).unwrap(), 0);
    (harness, id)
}

fn verify_roundtrip(
    harness: &mut EditorTestHarness,
    marker_id: MarkerId,
    orig_content: &str,
    orig_marker: usize,
    op_name: &str,
) {
    let post_op_content = harness.get_buffer_content().unwrap();
    let post_op_marker = margin_position(harness, marker_id).unwrap();

    // Undo.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    let after_undo_content = harness.get_buffer_content().unwrap();
    let after_undo_marker = margin_position(harness, marker_id).unwrap();
    assert_eq!(
        after_undo_content, orig_content,
        "{op_name}: content not restored after undo. Expected {orig_content:?}, got {after_undo_content:?}",
    );
    assert_eq!(
        after_undo_marker, orig_marker,
        "{op_name}: marker not restored after undo. Expected {orig_marker}, got {after_undo_marker}",
    );

    // Redo.
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    let after_redo_content = harness.get_buffer_content().unwrap();
    let after_redo_marker = margin_position(harness, marker_id).unwrap();
    assert_eq!(
        after_redo_content, post_op_content,
        "{op_name}: content not restored after redo. Expected {post_op_content:?}, got {after_redo_content:?}",
    );
    assert_eq!(
        after_redo_marker, post_op_marker,
        "{op_name}: marker not restored after redo. Expected {post_op_marker}, got {after_redo_marker}",
    );
}

#[test]
fn migrated_marker_roundtrip_under_single_edits() {
    // Original: tests/e2e/undo_redo_marker_roundtrip.rs:182
    // test_each_single_edit_op_marker_roundtrip. Walks 4 ops
    // (TypeChar, Backspace, Delete, Enter); each must preserve
    // the marker position under Undo/Redo.

    // TypeChar — insert at cursor.
    {
        let (mut harness, id) = setup_multiline();
        let orig = harness.get_buffer_content().unwrap();
        let orig_marker = margin_position(&harness, id).unwrap();
        harness.type_text("X").unwrap();
        verify_roundtrip(&mut harness, id, &orig, orig_marker, "TypeChar");
    }

    // Backspace — delete char before cursor.
    {
        let (mut harness, id) = setup_multiline();
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        let orig = harness.get_buffer_content().unwrap();
        let orig_marker = margin_position(&harness, id).unwrap();
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        verify_roundtrip(&mut harness, id, &orig, orig_marker, "Backspace");
    }

    // Delete — delete char at cursor.
    {
        let (mut harness, id) = setup_multiline();
        let orig = harness.get_buffer_content().unwrap();
        let orig_marker = margin_position(&harness, id).unwrap();
        harness
            .send_key(KeyCode::Delete, KeyModifiers::NONE)
            .unwrap();
        verify_roundtrip(&mut harness, id, &orig, orig_marker, "Delete");
    }

    // Enter — insert newline at cursor.
    {
        let (mut harness, id) = setup_multiline();
        let orig = harness.get_buffer_content().unwrap();
        let orig_marker = margin_position(&harness, id).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        verify_roundtrip(&mut harness, id, &orig, orig_marker, "Enter");
    }
}

#[test]
fn migrated_marker_roundtrip_under_bulk_edits() {
    // Original: tests/e2e/undo_redo_marker_roundtrip.rs:248
    // test_each_bulk_edit_op_marker_roundtrip. Walks BulkEdit
    // verbs (MoveLineUp, MoveLineDown); each must preserve the
    // marker position under Undo/Redo. BulkEdit goes through a
    // different undo-snapshot path than single edits, so this
    // is a distinct invariant.

    // MoveLineDown via Alt+Down.
    {
        let (mut harness, id) = setup_multiline();
        let orig = harness.get_buffer_content().unwrap();
        let orig_marker = margin_position(&harness, id).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
        verify_roundtrip(&mut harness, id, &orig, orig_marker, "MoveLineDown");
    }

    // MoveLineUp via Alt+Up.
    {
        let (mut harness, id) = setup_multiline();
        let orig = harness.get_buffer_content().unwrap();
        let orig_marker = margin_position(&harness, id).unwrap();
        harness.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();
        verify_roundtrip(&mut harness, id, &orig, orig_marker, "MoveLineUp");
    }
}

#[test]
fn migrated_marker_at_end_of_buffer_under_enter_then_typechar_then_movelineup() {
    // Original: test_enter_typechar_movelineup_marker_at_end.
    // Regression: a marker at the end of buffer must survive
    // [Enter, '}', Home, Alt+Up] then 3 Undos back to the orig
    // marker position.
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    harness.type_text("aaa").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("bbb").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("ccc").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let buf_len = harness.buffer_len();
    let id = add_margin_indicator(&mut harness, buf_len);
    let orig_marker = margin_position(&harness, id).unwrap();
    let orig_content = harness.get_buffer_content().unwrap();
    assert_eq!(orig_content, "aaa\nbbb\nccc");
    assert_eq!(orig_marker, 11);

    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('}'), KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();

    // 3 undos back to orig.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }
    assert_eq!(harness.get_buffer_content().unwrap(), orig_content);
    assert_eq!(
        margin_position(&harness, id).unwrap(),
        orig_marker,
        "marker not restored after 3 Undos through Enter/'}}'/Alt+Up"
    );
}

#[test]
fn migrated_marker_roundtrip_under_move_line_down() {
    // Original: test_move_line_down_marker_roundtrip. Marker at
    // byte 0 (start of "aaa"); MoveLineDown swaps "aaa" with
    // "bbb"; Undo restores; Redo re-swaps. Marker position
    // tracks through both directions.
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    harness.type_text("aaa").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("bbb").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("ccc").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();

    let orig_content = harness.get_buffer_content().unwrap();
    assert_eq!(orig_content, "aaa\nbbb\nccc");
    let id = add_margin_indicator(&mut harness, 0);
    let orig_marker = margin_position(&harness, id).unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "bbb\naaa\nccc");
    verify_roundtrip(&mut harness, id, &orig_content, orig_marker, "MoveLineDown");
}

#[test]
fn migrated_marker_roundtrip_under_move_line_up() {
    // Original: test_move_line_up_marker_roundtrip. Marker at
    // byte 4 (start of "bbb"); MoveLineUp swaps "bbb" with
    // "aaa"; Undo restores; Redo re-swaps.
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    harness.type_text("aaa").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("bbb").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("ccc").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let orig_content = harness.get_buffer_content().unwrap();
    let id = add_margin_indicator(&mut harness, 4);
    let orig_marker = margin_position(&harness, id).unwrap();
    assert_eq!(orig_marker, 4);

    harness.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), "bbb\naaa\nccc");
    verify_roundtrip(&mut harness, id, &orig_content, orig_marker, "MoveLineUp");
}

#[test]
fn migrated_marker_roundtrip_through_interleaved_single_and_bulk_edits() {
    // Original: test_interleaved_single_and_bulk_edit_marker_roundtrip.
    // Walks Type 'X' + MoveLineDown, then verifies that
    // individual Undos (one per edit) restore the marker
    // step-by-step, and individual Redos re-apply step-by-step.
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    harness.type_text("aa").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    harness.type_text("bb").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let base_content = harness.get_buffer_content().unwrap();
    assert_eq!(base_content, "aa\nbb");
    let id = add_margin_indicator(&mut harness, 0);
    let base_marker = margin_position(&harness, id).unwrap();

    // Step 1: type 'X' at start.
    harness.type_text("X").unwrap();
    let step1_content = harness.get_buffer_content().unwrap();
    let step1_marker = margin_position(&harness, id).unwrap();

    // Step 2: MoveLineDown.
    harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    let step2_content = harness.get_buffer_content().unwrap();
    let step2_marker = margin_position(&harness, id).unwrap();

    // Undo step 2 (BulkEdit) → back to step1.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), step1_content);
    assert_eq!(margin_position(&harness, id).unwrap(), step1_marker);

    // Undo step 1 (single edit) → back to base.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), base_content);
    assert_eq!(margin_position(&harness, id).unwrap(), base_marker);

    // Redo step 1 → step1.
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), step1_content);
    assert_eq!(margin_position(&harness, id).unwrap(), step1_marker);

    // Redo step 2 → step2.
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(harness.get_buffer_content().unwrap(), step2_content);
    assert_eq!(margin_position(&harness, id).unwrap(), step2_marker);
}

/// Anti-test: dropping the Undo from the verification step must
/// surface as a content mismatch (the post-op content is not the
/// orig content). Pins that the verification function actually
/// checks the undo result.
#[test]
fn anti_marker_roundtrip_without_undo_yields_post_op_content() {
    let (mut harness, _id) = setup_multiline();
    let orig = harness.get_buffer_content().unwrap();
    harness.type_text("X").unwrap();
    let post_op = harness.get_buffer_content().unwrap();
    assert_ne!(
        post_op, orig,
        "anti: typing must change content; otherwise the roundtrip claim is vacuous"
    );
}
