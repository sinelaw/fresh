// Property-based tests using proptest
// These tests generate random sequences of operations and verify invariants

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use proptest::prelude::*;

/// Generate random edit operations
#[derive(Debug, Clone)]
enum EditOp {
    TypeChar(char),
    TypeString(String),
    Backspace,
    Delete,
    Enter,
    Left,
    Right,
    Home,
    End,
}

impl EditOp {
    /// Apply this operation to the test harness
    fn apply(&self, harness: &mut EditorTestHarness) -> std::io::Result<()> {
        match self {
            EditOp::TypeChar(ch) => harness.type_text(&ch.to_string()),
            EditOp::TypeString(s) => harness.type_text(s),
            EditOp::Backspace => harness.send_key(KeyCode::Backspace, KeyModifiers::NONE),
            EditOp::Delete => harness.send_key(KeyCode::Delete, KeyModifiers::NONE),
            EditOp::Enter => harness.send_key(KeyCode::Enter, KeyModifiers::NONE),
            EditOp::Left => harness.send_key(KeyCode::Left, KeyModifiers::NONE),
            EditOp::Right => harness.send_key(KeyCode::Right, KeyModifiers::NONE),
            EditOp::Home => harness.send_key(KeyCode::Home, KeyModifiers::NONE),
            EditOp::End => harness.send_key(KeyCode::End, KeyModifiers::NONE),
        }
    }
}

/// Strategy for generating random edit operations
fn edit_op_strategy() -> impl Strategy<Value = EditOp> {
    prop_oneof![
        // Typing operations (more common)
        3 => any::<char>()
            .prop_filter("printable ASCII", |c| c.is_ascii() && !c.is_ascii_control())
            .prop_map(EditOp::TypeChar),
        2 => "[a-zA-Z0-9 ]{1,10}"
            .prop_map(|s| EditOp::TypeString(s)),
        // Navigation operations
        1 => Just(EditOp::Left),
        1 => Just(EditOp::Right),
        1 => Just(EditOp::Home),
        1 => Just(EditOp::End),
        // Editing operations
        2 => Just(EditOp::Backspace),
        2 => Just(EditOp::Delete),
        1 => Just(EditOp::Enter),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 100,
        max_shrink_iters: 1000,
        ..ProptestConfig::default()
    })]

    /// Property test: piece tree should always match shadow string after any sequence of edits
    #[test]
    fn prop_piece_tree_matches_shadow(ops in prop::collection::vec(edit_op_strategy(), 1..50)) {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.enable_shadow_validation();

        // Apply all operations
        for op in &ops {
            op.apply(&mut harness).unwrap();
        }

        // Get final state
        let buffer_content = harness.get_buffer_content();
        let shadow_content = harness.get_shadow_string();

        // They should match!
        prop_assert_eq!(
            &buffer_content,
            shadow_content,
            "Piece tree diverged from shadow string after {} operations\nOperations: {:#?}",
            ops.len(),
            ops
        );
    }

    /// Property test: cursor position should always be valid
    #[test]
    fn prop_cursor_position_valid(ops in prop::collection::vec(edit_op_strategy(), 1..50)) {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();

        for op in &ops {
            op.apply(&mut harness).unwrap();

            let cursor_pos = harness.cursor_position();
            let buffer_len = harness.buffer_len();

            prop_assert!(
                cursor_pos <= buffer_len,
                "Cursor position {} exceeds buffer length {} after operation {:?}",
                cursor_pos,
                buffer_len,
                op
            );
        }
    }

    /// Property test: buffer length should match shadow length
    #[test]
    fn prop_buffer_length_matches_shadow(ops in prop::collection::vec(edit_op_strategy(), 1..50)) {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.enable_shadow_validation();

        for op in &ops {
            op.apply(&mut harness).unwrap();
        }

        let buffer_len = harness.buffer_len();
        let shadow_len = harness.get_shadow_string().len();

        prop_assert_eq!(
            buffer_len,
            shadow_len,
            "Buffer length {} doesn't match shadow length {} after {} operations\nOperations: {:#?}",
            buffer_len,
            shadow_len,
            ops.len(),
            ops
        );
    }
}
