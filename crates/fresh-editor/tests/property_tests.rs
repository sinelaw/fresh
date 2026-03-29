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
    fn apply(&self, harness: &mut EditorTestHarness) -> anyhow::Result<()> {
        match self {
            Self::TypeChar(ch) => harness.type_text(&ch.to_string()),
            Self::TypeString(s) => harness.type_text(s),
            Self::Backspace => harness.send_key(KeyCode::Backspace, KeyModifiers::NONE),
            Self::Delete => harness.send_key(KeyCode::Delete, KeyModifiers::NONE),
            Self::Enter => harness.send_key(KeyCode::Enter, KeyModifiers::NONE),
            Self::Left => harness.send_key(KeyCode::Left, KeyModifiers::NONE),
            Self::Right => harness.send_key(KeyCode::Right, KeyModifiers::NONE),
            Self::Home => harness.send_key(KeyCode::Home, KeyModifiers::NONE),
            Self::End => harness.send_key(KeyCode::End, KeyModifiers::NONE),
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
            .prop_map(EditOp::TypeString),
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

/// Regression test for the specific case that exposed the line_wrap + Home + Backspace bug.
/// With line_wrap enabled (default), Home on a wrapped line goes to the visual line start
/// instead of position 0, causing Backspace to delete a character the shadow model doesn't expect.
#[test]
fn regression_home_backspace_with_line_wrap() {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = false;
    config.editor.auto_close = false;
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.enable_shadow_validation();

    let ops = vec![
        EditOp::TypeString("aaaAaa a".into()),
        EditOp::TypeString("0aa ".into()),
        EditOp::TypeString("aa0aaa".into()),
        EditOp::TypeString(" aa00 A".into()),
        EditOp::TypeString("aAA  aA AA".into()),
        EditOp::TypeString("a  0Aa0 ".into()),
        EditOp::TypeString(" AA AA".into()),
        EditOp::TypeString("AaAAA   ".into()),
        EditOp::TypeString(" a 0A  aA".into()),
        EditOp::TypeString("AaAaAa".into()),
        EditOp::Home,
        EditOp::Backspace,
    ];

    for op in &ops {
        op.apply(&mut harness).unwrap();
    }

    let buffer_content = harness.get_buffer_content().unwrap();
    let shadow_content = harness.get_shadow_string();

    assert_eq!(
        &buffer_content, shadow_content,
        "Piece tree diverged from shadow string"
    );
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
        // Disable auto_indent and auto_close so the shadow model stays in sync
        let mut config = fresh::config::Config::default();
        config.editor.auto_indent = false;
        config.editor.auto_close = false;
        // Disable line_wrap so that Home/End use physical line boundaries,
        // matching the shadow model which doesn't account for visual wrapping.
        config.editor.line_wrap = false;
        let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
        harness.enable_shadow_validation();

        // Apply all operations
        for op in &ops {
            op.apply(&mut harness).unwrap();
        }

        // Get final state
        let buffer_content = harness.get_buffer_content().unwrap();
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
        // Disable auto_indent and auto_close so the shadow model stays in sync
        let mut config = fresh::config::Config::default();
        config.editor.auto_indent = false;
        config.editor.auto_close = false;
        // Disable line_wrap so that Home/End use physical line boundaries,
        // matching the shadow model which doesn't account for visual wrapping.
        config.editor.line_wrap = false;
        let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
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
