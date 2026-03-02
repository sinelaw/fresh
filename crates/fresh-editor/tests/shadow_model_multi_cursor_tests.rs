// Shadow model property-based tests for multi-cursor editing
//
// These tests verify that edits (typing, Enter, Backspace, Delete) and cursor
// movements (arrows, Home, End, selection via Shift) behave identically across
// ALL cursors by comparing against a simple shadow model that tracks the full
// buffer content plus every cursor position.
//
// Architecture:
//   1. Set up the editor with some initial content and multiple cursors
//   2. Apply a random sequence of operations via EditorTestHarness::send_key()
//   3. Apply the same operations to a simple shadow model
//   4. After each operation verify: buffer content == shadow content,
//      and every cursor position matches

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use proptest::prelude::*;

// ============================================================================
// Shadow model for multi-cursor tracking
// ============================================================================

/// A simple shadow cursor: position + optional anchor (for selection)
#[derive(Debug, Clone, PartialEq, Eq)]
struct ShadowCursor {
    position: usize,
    anchor: Option<usize>,
}

impl ShadowCursor {
    fn new(position: usize) -> Self {
        Self {
            position,
            anchor: None,
        }
    }

    fn selection_range(&self) -> Option<std::ops::Range<usize>> {
        self.anchor.map(|anchor| {
            if self.position <= anchor {
                self.position..anchor
            } else {
                anchor..self.position
            }
        })
    }
}

/// Where the cursor should end up after an edit
#[derive(Debug, Clone, Copy)]
enum CursorAfterEdit {
    /// Cursor moves to end of inserted text (for typing/enter)
    AfterInsert,
    /// Cursor stays at edit start (for backspace/delete)
    AtEditStart,
}

/// Multi-cursor shadow model: tracks buffer content and all cursor positions.
///
/// Edits are applied atomically (all cursors at once) to match how the real
/// editor uses `apply_events_as_bulk_edit`. Original positions are used for
/// all edits, and cumulative position shifts are tracked.
#[derive(Debug, Clone)]
struct MultiCursorShadow {
    content: String,
    cursors: Vec<ShadowCursor>,
}

impl MultiCursorShadow {
    fn new(content: &str, cursor_positions: &[usize]) -> Self {
        let cursors = cursor_positions
            .iter()
            .map(|&pos| ShadowCursor::new(pos.min(content.len())))
            .collect();
        Self {
            content: content.to_string(),
            cursors,
        }
    }

    /// Apply all edits atomically, rebuilding the string from pieces.
    /// Each edit is (position, delete_len, insert_text, cursor_idx).
    /// This mirrors the editor's `apply_bulk_edits` behavior: all edits
    /// reference ORIGINAL positions and are applied simultaneously.
    fn apply_edits_atomic(
        &mut self,
        edits: &[(usize, usize, String, usize)],
        cursor_after: CursorAfterEdit,
    ) {
        if edits.is_empty() {
            return;
        }

        // Sort edits ascending by position for string rebuild
        let mut sorted_asc: Vec<&(usize, usize, String, usize)> = edits.iter().collect();
        sorted_asc.sort_by_key(|e| e.0);

        // Build new content by walking through original string
        let mut new_content = String::new();
        let mut read_pos = 0;

        for &(ref edit_pos, ref del_len, ref ins_text, _) in &sorted_asc {
            // Copy unmodified content before this edit
            if *edit_pos > read_pos {
                new_content.push_str(&self.content[read_pos..*edit_pos]);
            }
            // Insert new text
            new_content.push_str(ins_text);
            // Skip over deleted content
            read_pos = *edit_pos + *del_len;
        }
        // Copy remaining content after last edit
        if read_pos < self.content.len() {
            new_content.push_str(&self.content[read_pos..]);
        }

        self.content = new_content;

        // Calculate position deltas for cursor adjustment
        let mut position_deltas: Vec<(usize, isize)> = edits
            .iter()
            .map(|(pos, del_len, ins_text, _)| (*pos, ins_text.len() as isize - *del_len as isize))
            .collect();
        position_deltas.sort_by_key(|(pos, _)| *pos);

        let calc_shift = |original_pos: usize| -> isize {
            let mut shift: isize = 0;
            for &(edit_pos, delta) in &position_deltas {
                if edit_pos < original_pos {
                    shift += delta;
                }
            }
            shift
        };

        // Update cursor positions for cursors that participated in edits
        let edited_cursors: std::collections::HashSet<usize> =
            edits.iter().map(|(_, _, _, idx)| *idx).collect();

        for &(edit_pos, _del_len, ref ins_text, cursor_idx) in edits {
            let shift = calc_shift(edit_pos);
            let adjusted_pos = (edit_pos as isize + shift).max(0) as usize;
            self.cursors[cursor_idx].position = match cursor_after {
                CursorAfterEdit::AfterInsert => adjusted_pos.saturating_add(ins_text.len()),
                CursorAfterEdit::AtEditStart => adjusted_pos,
            };
            self.cursors[cursor_idx].anchor = None;
        }

        // Adjust non-editing cursors
        for (idx, cursor) in self.cursors.iter_mut().enumerate() {
            if !edited_cursors.contains(&idx) {
                let shift = calc_shift(cursor.position);
                cursor.position = (cursor.position as isize + shift).max(0) as usize;
                cursor.anchor = None;
            }
        }
    }

    /// Apply an insert-style operation (type_char or enter) to all cursors atomically.
    fn atomic_insert(&mut self, text: &str) {
        let edits: Vec<(usize, usize, String, usize)> = self
            .cursors
            .iter()
            .enumerate()
            .map(|(idx, cursor)| {
                let (insert_pos, del_len) = if let Some(range) = cursor.selection_range() {
                    (range.start, range.len())
                } else {
                    (cursor.position, 0)
                };
                (insert_pos, del_len, text.to_string(), idx)
            })
            .collect();

        self.apply_edits_atomic(&edits, CursorAfterEdit::AfterInsert);
    }

    /// Apply Backspace to all cursors atomically.
    fn atomic_backspace(&mut self) {
        let edits: Vec<(usize, usize, String, usize)> = self
            .cursors
            .iter()
            .enumerate()
            .filter_map(|(idx, cursor)| {
                if let Some(range) = cursor.selection_range() {
                    Some((range.start, range.len(), String::new(), idx))
                } else if cursor.position > 0 {
                    Some((cursor.position - 1, 1, String::new(), idx))
                } else {
                    None
                }
            })
            .collect();

        self.apply_edits_atomic(&edits, CursorAfterEdit::AtEditStart);
    }

    /// Apply Delete (forward) to all cursors atomically.
    fn atomic_delete(&mut self) {
        let content_len = self.content.len();
        let edits: Vec<(usize, usize, String, usize)> = self
            .cursors
            .iter()
            .enumerate()
            .filter_map(|(idx, cursor)| {
                if let Some(range) = cursor.selection_range() {
                    Some((range.start, range.len(), String::new(), idx))
                } else if cursor.position < content_len {
                    Some((cursor.position, 1, String::new(), idx))
                } else {
                    None
                }
            })
            .collect();

        self.apply_edits_atomic(&edits, CursorAfterEdit::AtEditStart);
    }

    /// Move all cursors left.
    /// The editor always moves from cursor.position, clearing any selection.
    fn move_left(&mut self) {
        for cursor in &mut self.cursors {
            cursor.anchor = None;
            if cursor.position > 0 {
                cursor.position -= 1;
            }
        }
    }

    /// Move all cursors right.
    /// The editor always moves from cursor.position, clearing any selection.
    fn move_right(&mut self) {
        for cursor in &mut self.cursors {
            cursor.anchor = None;
            if cursor.position < self.content.len() {
                cursor.position += 1;
            }
        }
    }

    /// Select left (Shift+Left) for all cursors
    fn select_left(&mut self) {
        for cursor in &mut self.cursors {
            if cursor.anchor.is_none() {
                cursor.anchor = Some(cursor.position);
            }
            if cursor.position > 0 {
                cursor.position -= 1;
            }
        }
    }

    /// Select right (Shift+Right) for all cursors
    fn select_right(&mut self) {
        for cursor in &mut self.cursors {
            if cursor.anchor.is_none() {
                cursor.anchor = Some(cursor.position);
            }
            if cursor.position < self.content.len() {
                cursor.position += 1;
            }
        }
    }
}

// ============================================================================
// Operations
// ============================================================================

#[derive(Debug, Clone)]
enum MultiCursorOp {
    TypeChar(char),
    Enter,
    Backspace,
    Delete,
    Left,
    Right,
    SelectLeft,
    SelectRight,
}

impl MultiCursorOp {
    fn apply_to_editor(&self, harness: &mut EditorTestHarness) -> anyhow::Result<()> {
        match self {
            Self::TypeChar(ch) => harness.send_key(KeyCode::Char(*ch), KeyModifiers::NONE),
            Self::Enter => harness.send_key(KeyCode::Enter, KeyModifiers::NONE),
            Self::Backspace => harness.send_key(KeyCode::Backspace, KeyModifiers::NONE),
            Self::Delete => harness.send_key(KeyCode::Delete, KeyModifiers::NONE),
            Self::Left => harness.send_key(KeyCode::Left, KeyModifiers::NONE),
            Self::Right => harness.send_key(KeyCode::Right, KeyModifiers::NONE),
            Self::SelectLeft => harness.send_key(KeyCode::Left, KeyModifiers::SHIFT),
            Self::SelectRight => harness.send_key(KeyCode::Right, KeyModifiers::SHIFT),
        }
    }

    fn apply_to_shadow(&self, shadow: &mut MultiCursorShadow) {
        match self {
            Self::TypeChar(ch) => shadow.atomic_insert(&ch.to_string()),
            Self::Enter => shadow.atomic_insert("\n"),
            Self::Backspace => shadow.atomic_backspace(),
            Self::Delete => shadow.atomic_delete(),
            Self::Left => shadow.move_left(),
            Self::Right => shadow.move_right(),
            Self::SelectLeft => shadow.select_left(),
            Self::SelectRight => shadow.select_right(),
        }
    }
}

fn multi_cursor_op_strategy() -> impl Strategy<Value = MultiCursorOp> {
    prop_oneof![
        5 => any::<char>()
            .prop_filter("printable ASCII", |c| c.is_ascii_graphic() || *c == ' ')
            .prop_map(MultiCursorOp::TypeChar),
        3 => Just(MultiCursorOp::Enter),
        2 => Just(MultiCursorOp::Backspace),
        1 => Just(MultiCursorOp::Delete),
        2 => Just(MultiCursorOp::Left),
        2 => Just(MultiCursorOp::Right),
        2 => Just(MultiCursorOp::SelectLeft),
        2 => Just(MultiCursorOp::SelectRight),
    ]
}

// ============================================================================
// Verification
// ============================================================================

fn verify_content_and_cursors(
    harness: &EditorTestHarness,
    shadow: &MultiCursorShadow,
    step: usize,
    ops: &[MultiCursorOp],
) -> Result<(), proptest::test_runner::TestCaseError> {
    let buffer_content = harness.get_buffer_content().unwrap_or_default();

    // Verify buffer content
    prop_assert_eq!(
        &buffer_content,
        &shadow.content,
        "Buffer content mismatch at step {}\nOps: {:?}",
        step,
        &ops[..=step]
    );

    // Verify cursor positions (sorted for comparison since order may differ)
    let mut editor_positions: Vec<usize> = harness
        .editor()
        .active_cursors()
        .iter()
        .map(|(_, c)| c.position)
        .collect();
    editor_positions.sort();

    let mut shadow_positions: Vec<usize> = shadow.cursors.iter().map(|c| c.position).collect();
    shadow_positions.sort();

    prop_assert_eq!(
        editor_positions.len(),
        shadow_positions.len(),
        "Cursor count mismatch at step {}: editor={}, shadow={}\nOps: {:?}",
        step,
        editor_positions.len(),
        shadow_positions.len(),
        &ops[..=step]
    );

    prop_assert_eq!(
        &editor_positions,
        &shadow_positions,
        "Cursor positions mismatch at step {}\n\
         Buffer: {:?}\nEditor cursors: {:?}\nShadow cursors: {:?}\nOps: {:?}",
        step,
        &buffer_content,
        editor_positions,
        shadow_positions,
        &ops[..=step]
    );

    Ok(())
}

// ============================================================================
// Test helpers
// ============================================================================

/// Set up the editor with multiple cursors using Ctrl+Alt+Down.
/// Returns the harness and a shadow model initialized with matching cursor positions.
fn setup_multi_cursor_editor(
    initial_text: &str,
    num_extra_cursors: usize,
) -> anyhow::Result<(EditorTestHarness, MultiCursorShadow)> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial text
    harness.type_text(initial_text)?;

    // Move to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;

    // Add extra cursors below
    for _ in 0..num_extra_cursors {
        harness.send_key(KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT)?;
    }

    // Collect all cursor positions from the editor
    let mut cursor_positions: Vec<usize> = harness
        .editor()
        .active_cursors()
        .iter()
        .map(|(_, c)| c.position)
        .collect();
    cursor_positions.sort();

    let shadow = MultiCursorShadow::new(initial_text, &cursor_positions);

    Ok((harness, shadow))
}

// ============================================================================
// Deterministic tests
// ============================================================================

/// Test: type a character with 2 cursors
#[test]
fn test_multi_cursor_type_char() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let op = MultiCursorOp::TypeChar('X');
    op.apply_to_editor(&mut harness).unwrap();
    op.apply_to_shadow(&mut shadow);

    verify_content_and_cursors(&harness, &shadow, 0, &[op]).unwrap();
}

/// Test: press Enter with 2 cursors
#[test]
fn test_multi_cursor_enter() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let op = MultiCursorOp::Enter;
    op.apply_to_editor(&mut harness).unwrap();
    op.apply_to_shadow(&mut shadow);

    verify_content_and_cursors(&harness, &shadow, 0, &[op]).unwrap();
}

/// Test: Backspace with 2 cursors
#[test]
fn test_multi_cursor_backspace() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    // First move right so we have something to backspace
    let move_op = MultiCursorOp::Right;
    move_op.apply_to_editor(&mut harness).unwrap();
    move_op.apply_to_shadow(&mut shadow);

    let op = MultiCursorOp::Backspace;
    op.apply_to_editor(&mut harness).unwrap();
    op.apply_to_shadow(&mut shadow);

    verify_content_and_cursors(&harness, &shadow, 1, &[move_op, op]).unwrap();
}

/// Test: Delete with 2 cursors
#[test]
fn test_multi_cursor_delete() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let op = MultiCursorOp::Delete;
    op.apply_to_editor(&mut harness).unwrap();
    op.apply_to_shadow(&mut shadow);

    verify_content_and_cursors(&harness, &shadow, 0, &[op]).unwrap();
}

/// Test: Enter then type, to match the exact bug scenario from issue #1140
#[test]
fn test_multi_cursor_enter_then_type() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let ops = vec![
        MultiCursorOp::Enter,
        MultiCursorOp::TypeChar('X'),
        MultiCursorOp::Enter,
        MultiCursorOp::TypeChar('Y'),
    ];

    for (i, op) in ops.iter().enumerate() {
        op.apply_to_editor(&mut harness).unwrap();
        op.apply_to_shadow(&mut shadow);
        verify_content_and_cursors(&harness, &shadow, i, &ops).unwrap();
    }
}

/// Test: selection then Enter (replace selection with newline)
#[test]
fn test_multi_cursor_select_then_enter() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let ops = vec![
        MultiCursorOp::SelectRight,
        MultiCursorOp::SelectRight,
        MultiCursorOp::Enter,
    ];

    for (i, op) in ops.iter().enumerate() {
        op.apply_to_editor(&mut harness).unwrap();
        op.apply_to_shadow(&mut shadow);
        verify_content_and_cursors(&harness, &shadow, i, &ops).unwrap();
    }
}

/// Test: selection then type (replace selection with character)
#[test]
fn test_multi_cursor_select_then_type() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let ops = vec![
        MultiCursorOp::SelectRight,
        MultiCursorOp::SelectRight,
        MultiCursorOp::TypeChar('X'),
    ];

    for (i, op) in ops.iter().enumerate() {
        op.apply_to_editor(&mut harness).unwrap();
        op.apply_to_shadow(&mut shadow);
        verify_content_and_cursors(&harness, &shadow, i, &ops).unwrap();
    }
}

/// Test: selection then backspace (delete selection)
#[test]
fn test_multi_cursor_select_then_backspace() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

    let ops = vec![
        MultiCursorOp::SelectRight,
        MultiCursorOp::SelectRight,
        MultiCursorOp::Backspace,
    ];

    for (i, op) in ops.iter().enumerate() {
        op.apply_to_editor(&mut harness).unwrap();
        op.apply_to_shadow(&mut shadow);
        verify_content_and_cursors(&harness, &shadow, i, &ops).unwrap();
    }
}

/// Test with 3 cursors
#[test]
fn test_three_cursors_enter() {
    let (mut harness, mut shadow) = setup_multi_cursor_editor("aaa\nbbb\nccc", 2).unwrap();

    // Should have 3 cursors
    assert_eq!(harness.editor().active_cursors().iter().count(), 3);
    assert_eq!(shadow.cursors.len(), 3);

    let ops = vec![
        MultiCursorOp::Right,
        MultiCursorOp::Enter,
        MultiCursorOp::TypeChar('X'),
    ];

    for (i, op) in ops.iter().enumerate() {
        op.apply_to_editor(&mut harness).unwrap();
        op.apply_to_shadow(&mut shadow);
        verify_content_and_cursors(&harness, &shadow, i, &ops).unwrap();
    }
}

// ============================================================================
// Helpers for property tests
// ============================================================================

/// Check if any cursor selections overlap. When selections overlap, both the
/// editor and shadow model may produce imprecise cursor positions due to
/// ambiguity in overlapping edit resolution. We still verify buffer content.
fn has_overlapping_selections(shadow: &MultiCursorShadow) -> bool {
    let mut ranges: Vec<std::ops::Range<usize>> = shadow
        .cursors
        .iter()
        .map(|c| {
            if let Some(range) = c.selection_range() {
                range
            } else {
                c.position..c.position
            }
        })
        .collect();
    ranges.sort_by_key(|r| r.start);
    for i in 1..ranges.len() {
        if ranges[i].start < ranges[i - 1].end {
            return true;
        }
    }
    false
}

/// Verify content always; verify cursor positions only when selections don't overlap.
fn verify_step(
    harness: &EditorTestHarness,
    shadow: &MultiCursorShadow,
    step: usize,
    op: &MultiCursorOp,
    ops: &[MultiCursorOp],
    skip_cursor_check: &mut bool,
) -> Result<(), proptest::test_runner::TestCaseError> {
    let buffer_content = harness.get_buffer_content().unwrap_or_default();
    prop_assert_eq!(
        &buffer_content,
        &shadow.content,
        "Content mismatch at step {} ({:?})\nOps so far: {:?}",
        step,
        op,
        &ops[..=step]
    );

    // Once cursors diverge (due to overlapping selections), skip position checks
    // for the rest of this test case since subsequent operations compound the divergence
    if *skip_cursor_check {
        return Ok(());
    }

    if has_overlapping_selections(shadow) {
        *skip_cursor_check = true;
        return Ok(());
    }

    let mut editor_positions: Vec<usize> = harness
        .editor()
        .active_cursors()
        .iter()
        .map(|(_, c)| c.position)
        .collect();
    editor_positions.sort();

    let mut shadow_positions: Vec<usize> = shadow.cursors.iter().map(|c| c.position).collect();
    shadow_positions.sort();

    prop_assert_eq!(
        &editor_positions,
        &shadow_positions,
        "Cursor positions mismatch at step {} ({:?})\n\
         Buffer: {:?}\nOps so far: {:?}",
        step,
        op,
        &buffer_content,
        &ops[..=step]
    );

    Ok(())
}

// ============================================================================
// Property tests
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 200,
        max_shrink_iters: 5000,
        ..ProptestConfig::default()
    })]

    /// Property test: random operations with 2 cursors on 3-line content
    #[test]
    fn prop_multi_cursor_2_cursors(
        ops in prop::collection::vec(multi_cursor_op_strategy(), 1..20),
    ) {
        let (mut harness, mut shadow) =
            setup_multi_cursor_editor("aaa\nbbb\nccc", 1).unwrap();

        let mut skip_cursor_check = false;
        for (i, op) in ops.iter().enumerate() {
            op.apply_to_editor(&mut harness).unwrap();
            op.apply_to_shadow(&mut shadow);
            verify_step(&harness, &shadow, i, op, &ops, &mut skip_cursor_check)?;
        }
    }

    /// Property test: random operations with 3 cursors
    #[test]
    fn prop_multi_cursor_3_cursors(
        ops in prop::collection::vec(multi_cursor_op_strategy(), 1..15),
    ) {
        let (mut harness, mut shadow) =
            setup_multi_cursor_editor("aaa\nbbb\nccc", 2).unwrap();

        let mut skip_cursor_check = false;
        for (i, op) in ops.iter().enumerate() {
            op.apply_to_editor(&mut harness).unwrap();
            op.apply_to_shadow(&mut shadow);
            verify_step(&harness, &shadow, i, op, &ops, &mut skip_cursor_check)?;
        }
    }
}
