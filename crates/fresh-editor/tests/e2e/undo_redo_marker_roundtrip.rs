// E2E property-based tests for undo/redo marker position roundtrip.
//
// Core property: after applying a random sequence of operations (including
// BulkEdit operations like MoveLineUp/Down), then undoing ALL write operations,
// marker positions must return to their original values.
//
// This uses EditorTestHarness to exercise the full pipeline including
// apply_events_as_bulk_edit, event logging, and undo/redo machinery.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::marker::MarkerId;
use fresh::view::margin::LineIndicator;
use proptest::prelude::*;
use ratatui::style::Color;

/// Operations that can be performed on the editor, including BulkEdit sources.
#[derive(Debug, Clone)]
enum Op {
    TypeChar(char),
    Backspace,
    Delete,
    Enter,
    Left,
    Right,
    Home,
    End,
    MoveLineUp,
    MoveLineDown,
}

impl Op {
    fn apply(&self, harness: &mut EditorTestHarness) -> anyhow::Result<()> {
        match self {
            Self::TypeChar(ch) => harness.send_key(KeyCode::Char(*ch), KeyModifiers::NONE),
            Self::Backspace => harness.send_key(KeyCode::Backspace, KeyModifiers::NONE),
            Self::Delete => harness.send_key(KeyCode::Delete, KeyModifiers::NONE),
            Self::Enter => harness.send_key(KeyCode::Enter, KeyModifiers::NONE),
            Self::Left => harness.send_key(KeyCode::Left, KeyModifiers::NONE),
            Self::Right => harness.send_key(KeyCode::Right, KeyModifiers::NONE),
            Self::Home => harness.send_key(KeyCode::Home, KeyModifiers::NONE),
            Self::End => harness.send_key(KeyCode::End, KeyModifiers::NONE),
            Self::MoveLineUp => harness.send_key(KeyCode::Up, KeyModifiers::ALT),
            Self::MoveLineDown => harness.send_key(KeyCode::Down, KeyModifiers::ALT),
        }
    }
}

fn op_strategy() -> impl Strategy<Value = Op> {
    prop_oneof![
        5 => any::<char>()
            .prop_filter("printable ASCII", |c| c.is_ascii() && !c.is_ascii_control())
            .prop_map(Op::TypeChar),
        2 => Just(Op::Backspace),
        1 => Just(Op::Delete),
        2 => Just(Op::Enter),
        2 => Just(Op::Left),
        2 => Just(Op::Right),
        1 => Just(Op::Home),
        1 => Just(Op::End),
        2 => Just(Op::MoveLineUp),
        2 => Just(Op::MoveLineDown),
    ]
}

/// Add a margin indicator at a byte offset via direct state mutation.
fn add_margin_indicator(harness: &mut EditorTestHarness, byte_offset: usize) -> MarkerId {
    let state = harness.editor_mut().active_state_mut();
    let indicator = LineIndicator::new("●", Color::Red, 10);
    state
        .margins
        .set_line_indicator(byte_offset, "test".to_string(), indicator)
}

/// Get the current byte position of a margin indicator.
fn get_margin_position(harness: &EditorTestHarness, marker_id: MarkerId) -> Option<usize> {
    harness
        .editor()
        .active_state()
        .margins
        .get_indicator_position(marker_id)
}

/// Send Ctrl+Z (undo).
fn undo(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
}

/// Send Ctrl+Y (redo).
fn redo(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::CONTROL)
        .unwrap();
}

// ============================================================================
// Deterministic tests: one operation at a time
// ============================================================================

/// Helper: set up a harness with "aaa\nbbb\nccc", cursor at start of line 2,
/// and a margin indicator at byte 0. Returns (harness, margin_id).
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

    // Move cursor to start of line 2
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), "aaa\nbbb\nccc");

    // Add margin at byte 0
    let margin_id = add_margin_indicator(&mut harness, 0);
    assert_eq!(get_margin_position(&harness, margin_id).unwrap(), 0);

    (harness, margin_id)
}

/// Helper: verify undo → redo roundtrip after an operation has been applied.
///
/// Call this AFTER the operation. Pass the content/margin from BEFORE the operation.
fn verify_roundtrip_after_op(
    harness: &mut EditorTestHarness,
    margin_id: MarkerId,
    orig_content: &str,
    orig_margin: usize,
    op_name: &str,
) {
    let post_op_content = harness.get_buffer_content().unwrap();
    let post_op_margin = get_margin_position(harness, margin_id).unwrap();

    // Undo
    undo(harness);
    let after_undo_content = harness.get_buffer_content().unwrap();
    let after_undo_margin = get_margin_position(harness, margin_id).unwrap();

    assert_eq!(
        after_undo_content, orig_content,
        "{}: content not restored after undo.\nExpected: {:?}\nGot: {:?}",
        op_name, orig_content, after_undo_content
    );
    assert_eq!(
        after_undo_margin, orig_margin,
        "{}: margin not restored after undo. Expected {}, got {}",
        op_name, orig_margin, after_undo_margin
    );

    // Redo
    redo(harness);
    let after_redo_content = harness.get_buffer_content().unwrap();
    let after_redo_margin = get_margin_position(harness, margin_id).unwrap();

    assert_eq!(
        after_redo_content, post_op_content,
        "{}: content not restored after redo.\nExpected: {:?}\nGot: {:?}",
        op_name, post_op_content, after_redo_content
    );
    assert_eq!(
        after_redo_margin, post_op_margin,
        "{}: margin not restored after redo. Expected {}, got {}",
        op_name, post_op_margin, after_redo_margin
    );
}

/// Walk through each single-edit operation type and verify marker roundtrip.
#[test]
fn test_each_single_edit_op_marker_roundtrip() {
    // TypeChar: insert at cursor position (start of line 2, byte 4)
    {
        let (mut harness, margin_id) = setup_multiline();
        let orig_content = harness.get_buffer_content().unwrap();
        let orig_margin = get_margin_position(&harness, margin_id).unwrap();
        harness.type_text("X").unwrap();
        verify_roundtrip_after_op(
            &mut harness,
            margin_id,
            &orig_content,
            orig_margin,
            "TypeChar",
        );
    }

    // Backspace: delete char before cursor
    {
        let (mut harness, margin_id) = setup_multiline();
        // Move to end of line 2 so there's something to backspace
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        let orig_content = harness.get_buffer_content().unwrap();
        let orig_margin = get_margin_position(&harness, margin_id).unwrap();
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        verify_roundtrip_after_op(
            &mut harness,
            margin_id,
            &orig_content,
            orig_margin,
            "Backspace",
        );
    }

    // Delete: delete char at cursor
    {
        let (mut harness, margin_id) = setup_multiline();
        let orig_content = harness.get_buffer_content().unwrap();
        let orig_margin = get_margin_position(&harness, margin_id).unwrap();
        harness
            .send_key(KeyCode::Delete, KeyModifiers::NONE)
            .unwrap();
        verify_roundtrip_after_op(
            &mut harness,
            margin_id,
            &orig_content,
            orig_margin,
            "Delete",
        );
    }

    // Enter: insert newline at cursor
    {
        let (mut harness, margin_id) = setup_multiline();
        let orig_content = harness.get_buffer_content().unwrap();
        let orig_margin = get_margin_position(&harness, margin_id).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        verify_roundtrip_after_op(&mut harness, margin_id, &orig_content, orig_margin, "Enter");
    }
}

/// Walk through each BulkEdit operation type and verify marker roundtrip.
#[test]
fn test_each_bulk_edit_op_marker_roundtrip() {
    // MoveLineDown: cursor on line 2, move it down
    {
        let (mut harness, margin_id) = setup_multiline();
        let orig_content = harness.get_buffer_content().unwrap();
        let orig_margin = get_margin_position(&harness, margin_id).unwrap();
        harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
        verify_roundtrip_after_op(
            &mut harness,
            margin_id,
            &orig_content,
            orig_margin,
            "MoveLineDown",
        );
    }

    // MoveLineUp: cursor on line 2, move it up
    {
        let (mut harness, margin_id) = setup_multiline();
        let orig_content = harness.get_buffer_content().unwrap();
        let orig_margin = get_margin_position(&harness, margin_id).unwrap();
        harness.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();
        verify_roundtrip_after_op(
            &mut harness,
            margin_id,
            &orig_content,
            orig_margin,
            "MoveLineUp",
        );
    }
}

// ============================================================================
// Deterministic tests: specific scenarios
// ============================================================================

/// Regression test: [Enter, TypeChar('}'), Home, MoveLineUp] with marker at end.
#[test]
fn test_enter_typechar_movelineup_marker_at_end() {
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
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let buf_len = harness.buffer_len();
    let margin_id = add_margin_indicator(&mut harness, buf_len);
    let orig_margin = get_margin_position(&harness, margin_id).unwrap();
    let orig_content = harness.get_buffer_content().unwrap();
    assert_eq!(orig_content, "aaa\nbbb\nccc");
    assert_eq!(orig_margin, 11);

    // Apply ops
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('}'), KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();

    // Undo all three operations
    undo(&mut harness);
    undo(&mut harness);
    undo(&mut harness);
    let u3 = get_margin_position(&harness, margin_id).unwrap();

    assert_eq!(harness.get_buffer_content().unwrap(), orig_content);
    assert_eq!(
        u3, orig_margin,
        "Margin should be restored to {}, got {}",
        orig_margin, u3
    );
}

/// MoveLineDown (BulkEdit) with margin indicator: forward/undo/redo roundtrip.
#[test]
fn test_move_line_down_marker_roundtrip() {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Type multi-line content
    harness.type_text("aaa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("bbb").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ccc").unwrap();

    // Move cursor to line 1 (Home, Home to get to start, then we're on last line — go up)
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();

    let original_content = harness.get_buffer_content().unwrap();
    assert_eq!(original_content, "aaa\nbbb\nccc");

    // Add margin at byte 0 (start of "aaa")
    let margin_id = add_margin_indicator(&mut harness, 0);
    let orig_margin_pos = get_margin_position(&harness, margin_id).unwrap();
    assert_eq!(orig_margin_pos, 0);

    // MoveLineDown: "aaa" moves to line 2 → "bbb\naaa\nccc"
    harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    let after_move = harness.get_buffer_content().unwrap();
    assert_eq!(after_move, "bbb\naaa\nccc");
    let moved_margin_pos = get_margin_position(&harness, margin_id).unwrap();

    // Undo
    undo(&mut harness);
    let after_undo = harness.get_buffer_content().unwrap();
    assert_eq!(
        after_undo, original_content,
        "Content not restored after undo"
    );
    let undo_margin_pos = get_margin_position(&harness, margin_id).unwrap();
    assert_eq!(
        undo_margin_pos, orig_margin_pos,
        "Margin not restored after undo. Expected {}, got {}",
        orig_margin_pos, undo_margin_pos
    );

    // Redo
    redo(&mut harness);
    let after_redo = harness.get_buffer_content().unwrap();
    assert_eq!(after_redo, after_move, "Content not restored after redo");
    let redo_margin_pos = get_margin_position(&harness, margin_id).unwrap();
    assert_eq!(
        redo_margin_pos, moved_margin_pos,
        "Margin not restored after redo. Expected {}, got {}",
        moved_margin_pos, redo_margin_pos
    );
}

/// MoveLineUp (BulkEdit) with margin indicator: forward/undo/redo roundtrip.
#[test]
fn test_move_line_up_marker_roundtrip() {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Type multi-line content
    harness.type_text("aaa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("bbb").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ccc").unwrap();

    // Cursor is at end of "ccc" (last line). Move to start of line 2.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let original_content = harness.get_buffer_content().unwrap();

    // Add margin at byte 4 (start of "bbb")
    let margin_id = add_margin_indicator(&mut harness, 4);
    let orig_margin_pos = get_margin_position(&harness, margin_id).unwrap();
    assert_eq!(orig_margin_pos, 4);

    // MoveLineUp: "bbb" moves to line 1 → "bbb\naaa\nccc"
    harness.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();
    let after_move = harness.get_buffer_content().unwrap();
    assert_eq!(after_move, "bbb\naaa\nccc");
    let moved_margin_pos = get_margin_position(&harness, margin_id).unwrap();

    // Undo
    undo(&mut harness);
    let after_undo = harness.get_buffer_content().unwrap();
    assert_eq!(
        after_undo, original_content,
        "Content not restored after undo"
    );
    let undo_margin_pos = get_margin_position(&harness, margin_id).unwrap();
    assert_eq!(
        undo_margin_pos, orig_margin_pos,
        "Margin not restored after undo. Expected {}, got {}",
        orig_margin_pos, undo_margin_pos
    );

    // Redo
    redo(&mut harness);
    let after_redo = harness.get_buffer_content().unwrap();
    assert_eq!(after_redo, after_move, "Content not restored after redo");
    let redo_margin_pos = get_margin_position(&harness, margin_id).unwrap();
    assert_eq!(
        redo_margin_pos, moved_margin_pos,
        "Margin not restored after redo. Expected {}, got {}",
        moved_margin_pos, redo_margin_pos
    );
}

/// Type + MoveLineDown + Type: interleaved single-edit and BulkEdit with marker.
#[test]
fn test_interleaved_single_and_bulk_edit_marker_roundtrip() {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Setup: "aa\nbb"
    harness.type_text("aa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("bb").unwrap();

    // Go to start of line 1
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    let base_content = harness.get_buffer_content().unwrap();
    assert_eq!(base_content, "aa\nbb");

    // Add margin at byte 0
    let margin_id = add_margin_indicator(&mut harness, 0);
    let base_margin = get_margin_position(&harness, margin_id).unwrap();

    // Step 1: Type "X" at start → "Xaa\nbb"
    harness.type_text("X").unwrap();
    let step1_content = harness.get_buffer_content().unwrap();
    let step1_margin = get_margin_position(&harness, margin_id).unwrap();

    // Step 2: MoveLineDown (BulkEdit) → should swap lines
    harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    let step2_content = harness.get_buffer_content().unwrap();
    let step2_margin = get_margin_position(&harness, margin_id).unwrap();

    // Undo step 2 (BulkEdit)
    undo(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), step1_content);
    assert_eq!(
        get_margin_position(&harness, margin_id).unwrap(),
        step1_margin,
        "Margin not restored after undoing BulkEdit"
    );

    // Undo step 1 (single edit "X")
    undo(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), base_content);
    assert_eq!(
        get_margin_position(&harness, margin_id).unwrap(),
        base_margin,
        "Margin not restored after undoing all edits"
    );

    // Redo step 1
    redo(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), step1_content);
    assert_eq!(
        get_margin_position(&harness, margin_id).unwrap(),
        step1_margin,
        "Margin not restored after redo step 1"
    );

    // Redo step 2
    redo(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), step2_content);
    assert_eq!(
        get_margin_position(&harness, margin_id).unwrap(),
        step2_margin,
        "Margin not restored after redo step 2"
    );
}

// ============================================================================
// Property-based tests
// ============================================================================

/// Run a sequence of ops, then undo all writes, verify marker position restored,
/// then redo all, verify marker position matches post-edit.
fn run_marker_roundtrip(ops: &[Op]) -> Result<(), proptest::test_runner::TestCaseError> {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // Type initial multi-line content so MoveLineUp/Down have something to work with
    harness.type_text("aaa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("bbb").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("ccc").unwrap();
    // Move to start
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();

    // Add margin at end of buffer to avoid the known affinity limitation
    // where insert-at-marker-position pushes the marker right.
    let buf_len = harness.buffer_len();
    let margin_id = add_margin_indicator(&mut harness, buf_len);

    let orig_content = harness.get_buffer_content().unwrap();
    let orig_margin_pos = get_margin_position(&harness, margin_id).unwrap();

    // Apply all ops
    for op in ops {
        op.apply(&mut harness).unwrap();
    }

    let post_edit_content = harness.get_buffer_content().unwrap();
    let post_edit_margin_pos = get_margin_position(&harness, margin_id).unwrap();

    // Undo until we're back at the original content (max 50 undos)
    let mut undo_count = 0;
    for _ in 0..50 {
        if harness.get_buffer_content().unwrap() == orig_content {
            break;
        }
        undo(&mut harness);
        undo_count += 1;
    }

    let after_undo_content = harness.get_buffer_content().unwrap();
    let after_undo_margin_pos = get_margin_position(&harness, margin_id).unwrap();

    prop_assert_eq!(
        &after_undo_content,
        &orig_content,
        "Buffer content not restored after undo.\nOps: {:?}",
        ops
    );
    prop_assert_eq!(
        after_undo_margin_pos,
        orig_margin_pos,
        "Margin position not restored after undo.\n\
         Expected {}, got {}.\n\
         Original: {:?}\nPost-edit: {:?}\nAfter-undo: {:?}\nOps: {:?}",
        orig_margin_pos,
        after_undo_margin_pos,
        orig_content,
        post_edit_content,
        after_undo_content,
        ops
    );

    // Redo the same number of times we undid
    for _ in 0..undo_count {
        redo(&mut harness);
    }

    let after_redo_content = harness.get_buffer_content().unwrap();
    let after_redo_margin_pos = get_margin_position(&harness, margin_id).unwrap();

    prop_assert_eq!(
        &after_redo_content,
        &post_edit_content,
        "Buffer content not restored after redo.\nOps: {:?}",
        ops
    );
    prop_assert_eq!(
        after_redo_margin_pos,
        post_edit_margin_pos,
        "Margin position not restored after redo.\n\
         Expected {}, got {}.\n\
         Post-edit: {:?}\nAfter-redo: {:?}\nOps: {:?}",
        post_edit_margin_pos,
        after_redo_margin_pos,
        post_edit_content,
        after_redo_content,
        ops
    );

    Ok(())
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 50,
        max_shrink_iters: 5000,
        ..ProptestConfig::default()
    })]

    /// Single op (including BulkEdit): marker roundtrip through undo/redo.

    #[test]

    fn prop_single_op_marker_roundtrip(
        op in op_strategy(),
    ) {
        run_marker_roundtrip(&[op])?;
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 50,
        max_shrink_iters: 5000,
        ..ProptestConfig::default()
    })]

    /// Two ops: marker roundtrip through undo/redo.

    #[test]

    fn prop_two_op_marker_roundtrip(
        op1 in op_strategy(),
        op2 in op_strategy(),
    ) {
        run_marker_roundtrip(&[op1, op2])?;
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 30,
        max_shrink_iters: 5000,
        ..ProptestConfig::default()
    })]

    /// Long sequence with BulkEdit ops: marker roundtrip through undo/redo.

    #[test]

    fn prop_long_sequence_marker_roundtrip(
        ops in prop::collection::vec(op_strategy(), 1..15),
    ) {
        run_marker_roundtrip(&ops)?;
    }
}
