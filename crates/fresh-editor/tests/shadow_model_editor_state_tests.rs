// Shadow model property-based tests for EditorState with undo/redo
//
// These tests exercise the full editor pipeline via EditorTestHarness::send_key(),
// including undo/redo and bulk edit operations (MoveLineUp/Down).

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use proptest::prelude::*;

/// Operations that can be performed on the editor
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
    Undo,
    Redo,
}

impl Op {
    /// Apply this operation to the test harness via send_key
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
            Self::Undo => harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL),
            Self::Redo => harness.send_key(KeyCode::Char('y'), KeyModifiers::CONTROL),
        }
    }
}

/// Strategy for generating random operations with weighted distribution
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
        1 => Just(Op::MoveLineUp),
        1 => Just(Op::MoveLineDown),
        3 => Just(Op::Undo),
        2 => Just(Op::Redo),
    ]
}

/// Run a sequence of ops and verify shadow model matches editor state
fn run_and_verify(ops: &[Op]) -> Result<(), proptest::test_runner::TestCaseError> {
    // Disable auto_indent and auto_close so the shadow model (which is a simple
    // string that doesn't simulate either) stays in sync with the editor buffer.
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.enable_shadow_validation();

    for op in ops {
        op.apply(&mut harness).unwrap();
    }

    let buffer_content = harness.get_buffer_content().unwrap();
    let shadow_string = harness.get_shadow_string().to_string();
    let cursor_pos = harness.cursor_position();
    let shadow_cursor = harness.get_shadow_cursor();
    let buffer_len = harness.buffer_len();

    prop_assert_eq!(
        &buffer_content,
        &shadow_string,
        "Buffer content diverged from shadow after {} ops\nOps: {:?}",
        ops.len(),
        ops
    );

    prop_assert_eq!(
        cursor_pos,
        shadow_cursor,
        "Cursor position diverged from shadow after {} ops\n\
         Editor cursor: {}, Shadow cursor: {}\n\
         Buffer content: {:?}\nOps: {:?}",
        ops.len(),
        cursor_pos,
        shadow_cursor,
        &buffer_content,
        ops
    );

    prop_assert!(
        cursor_pos <= buffer_len,
        "Cursor position {} exceeds buffer length {} after {} ops\nOps: {:?}",
        cursor_pos,
        buffer_len,
        ops.len(),
        ops
    );

    Ok(())
}

/// Deterministic test: apply each Op variant individually to catch basic mismatches
#[test]
fn test_single_op_each() {
    let ops: Vec<Op> = vec![
        Op::TypeChar('a'),
        Op::TypeChar('\n'),
        Op::Backspace,
        Op::Delete,
        Op::Enter,
        Op::Left,
        Op::Right,
        Op::Home,
        Op::End,
        Op::MoveLineUp,
        Op::MoveLineDown,
        Op::Undo,
        Op::Redo,
    ];

    for op in &ops {
        let mut config = fresh::config::Config::default();
        config.editor.auto_indent = false;
        config.editor.auto_close = false;
        let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
        harness.enable_shadow_validation();

        op.apply(&mut harness).unwrap();

        let buffer_content = harness.get_buffer_content().unwrap();
        let shadow_string = harness.get_shadow_string().to_string();
        let cursor_pos = harness.cursor_position();
        let shadow_cursor = harness.get_shadow_cursor();
        let buffer_len = harness.buffer_len();

        assert_eq!(
            buffer_content, shadow_string,
            "Content mismatch for single op {:?}",
            op
        );
        assert_eq!(
            cursor_pos, shadow_cursor,
            "Cursor mismatch for single op {:?}: editor={}, shadow={}",
            op, cursor_pos, shadow_cursor
        );
        assert!(
            cursor_pos <= buffer_len,
            "Cursor {} exceeds buffer len {} for single op {:?}",
            cursor_pos,
            buffer_len,
            op
        );
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 500,
        max_shrink_iters: 5000,
        ..ProptestConfig::default()
    })]

    /// Property test: 2-op sequences to test all pairwise interactions
    #[test]
    fn prop_shadow_model_2_ops(
        op1 in op_strategy(),
        op2 in op_strategy(),
    ) {
        run_and_verify(&[op1, op2])?;
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 200,
        max_shrink_iters: 5000,
        ..ProptestConfig::default()
    })]

    /// Property test: long random sequences to stress undo/redo chains
    #[test]
    fn prop_shadow_model_long_sequence(
        ops in prop::collection::vec(op_strategy(), 1..50),
    ) {
        run_and_verify(&ops)?;
    }
}
