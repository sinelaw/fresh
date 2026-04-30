//! Track B migration: pure-action subset of `tests/e2e/unicode_cursor.rs`.
//!
//! These theorems pin down the editor's UTF-8 / grapheme-cluster
//! invariants — the kind of behavior that breaks subtly when a
//! refactor mistakes byte boundaries for character boundaries:
//!
//!   * a Right/Left arrow advances by one grapheme cluster, not by
//!     one byte (so multi-byte UTF-8 chars and Thai clusters are
//!     atomic);
//!   * Backspace deletes one *code point*, allowing layer-by-layer
//!     removal of Thai combining marks;
//!   * Delete (forward) removes a full grapheme cluster (because
//!     deleting the base would orphan the marks);
//!   * selection-delete and selection-replace operate on the byte
//!     range under the selection, leaving surrounding multi-byte
//!     chars intact.
//!
//! Skipped (deferred):
//!   * `test_cursor_sync_with_non_ascii_box_drawing_chars` and
//!     `test_thai_file_open_and_movement` — assert on
//!     `screen_cursor_position` (visual column after gutter), which
//!     is a layout observable and would be the natural home for a
//!     `RenderSnapshot`-style theorem.
//!   * `test_mouse_click_on_non_ascii_text` — mouse input, not
//!     action-level.
//!   * `test_backspace_utf8_file_save_roundtrip` — full file save
//!     round-trip, beyond the pure-state contract.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use fresh::test_api::Action;

// ─────────────────────────────────────────────────────────────────────────
// Cursor movement through multi-byte UTF-8 characters
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_right_arrow_steps_over_4_byte_emoji_as_one_unit() {
    // Replaces test_cursor_sync_with_emoji.
    // "Hello 😀 World 🌍" — '😀' is 4 bytes (U+1F600). Seven Right
    // arrows from byte 0: H, e, l, l, o, space, 😀 → byte 10
    // (6 ASCII + 4 emoji).
    assert_buffer_theorem(BufferTheorem {
        description: "Right arrow ×7 from byte 0 traverses 'Hello ' + emoji = byte 10",
        initial_text: "Hello 😀 World 🌍",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "Hello 😀 World 🌍",
        expected_primary: CursorExpect::at(10),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_typing_after_emoji_inserts_at_correct_byte_offset() {
    // Replaces the second half of test_cursor_sync_with_emoji.
    // After traversing past the emoji, typing 'X' must insert at the
    // byte position immediately after it — not somewhere inside the
    // multi-byte sequence.
    assert_buffer_theorem(BufferTheorem {
        description: "Typing after an emoji inserts past it without splitting the byte sequence",
        initial_text: "Hello 😀 World 🌍",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::InsertChar('X'),
        ],
        expected_text: "Hello 😀X World 🌍",
        expected_primary: CursorExpect::at(11),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// DeleteBackward (Backspace) removes whole UTF-8 chars, not bytes
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_backspace_removes_entire_3_byte_euro_sign() {
    // Replaces test 1 of test_backspace_deletes_entire_utf8_character.
    // '€' is 3 bytes (0xE2 0x82 0xAC). Backspace from byte 3 must
    // collapse to empty — not leave a half-character behind.
    assert_buffer_theorem(BufferTheorem {
        description: "Backspace deletes the whole € (3 bytes) atomically",
        initial_text: "€",
        actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
        expected_text: "",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_backspace_removes_norwegian_chars_one_at_a_time() {
    // Replaces test 2 of test_backspace_deletes_entire_utf8_character.
    // 'æ', 'ø', 'å' are each 2 bytes. Three backspaces from end of
    // "æøå" yields "æø", "æ", "" — one char each, not bytes.
    assert_buffer_theorem(BufferTheorem {
        description: "Three backspaces over æøå remove one 2-byte char each",
        initial_text: "æøå",
        actions: vec![
            Action::MoveDocumentEnd,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
        ],
        expected_text: "",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_backspace_removes_4_byte_emoji_atomically() {
    // Replaces test 3 of test_backspace_deletes_entire_utf8_character.
    // "a😀b" → backspace 'b' → "a😀" → backspace emoji → "a".
    assert_buffer_theorem(BufferTheorem {
        description: "Backspace deletes a 4-byte emoji atomically (not just one byte)",
        initial_text: "a😀b",
        actions: vec![
            Action::MoveDocumentEnd,
            Action::DeleteBackward,
            Action::DeleteBackward,
        ],
        expected_text: "a",
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// DeleteForward (Delete key) removes whole UTF-8 chars
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_delete_forward_removes_entire_3_byte_euro() {
    // Replaces test_delete_forward_removes_entire_utf8_character.
    // From byte 0 of "a€b": Delete 'a' → "€b"; Delete '€' (3 bytes)
    // → "b" — must skip the whole euro sign, not just one byte.
    assert_buffer_theorem(BufferTheorem {
        description: "DeleteForward removes a 3-byte euro sign atomically",
        initial_text: "a€b",
        actions: vec![
            Action::MoveDocumentStart,
            Action::DeleteForward,
            Action::DeleteForward,
        ],
        expected_text: "b",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// Selection-delete and selection-replace over UTF-8 ranges
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_selection_delete_collapses_multibyte_run() {
    // Replaces test_selection_delete_with_utf8_characters.
    // "aæøåb" — select the three Norwegian chars (3 graphemes,
    // 6 bytes), backspace deletes the selection cleanly: "ab".
    assert_buffer_theorem(BufferTheorem {
        description:
            "Selection-delete over a 3-grapheme / 6-byte UTF-8 run leaves surrounding ASCII intact",
        initial_text: "aæøåb",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::DeleteBackward,
        ],
        expected_text: "ab",
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_selection_replace_swaps_emoji_with_ascii() {
    // Replaces test_selection_replace_with_utf8_characters.
    // "hello😀world" — select the emoji (1 grapheme / 4 bytes) and
    // type 'X' to replace it; surrounding ASCII unchanged.
    assert_buffer_theorem(BufferTheorem {
        description: "Selection-replace swaps a 4-byte emoji for ASCII without splitting neighbors",
        initial_text: "hello😀world",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectRight,
            Action::InsertChar('X'),
        ],
        expected_text: "helloXworld",
        expected_primary: CursorExpect::at(6),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// Thai: grapheme-cluster movement, layer-by-layer backspace, atomic delete
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_right_arrow_skips_thai_grapheme_cluster() {
    // Replaces test_thai_grapheme_cluster_movement (movement subset).
    // "aที่b" — 'ที่' is 1 grapheme cluster across 3 code points (9
    // bytes). Three Right arrows from byte 0: 'a' → byte 1, cluster
    // → byte 10, 'b' → byte 11.
    assert_buffer_theorem(BufferTheorem {
        description: "Right arrow x3 from byte 0 of 'aที่b' visits bytes 1, 10, 11 (cluster atomic)",
        initial_text: "aที่b",
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
        ],
        expected_text: "aที่b",
        expected_primary: CursorExpect::at(11),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_left_arrow_skips_thai_grapheme_cluster_backwards() {
    // Replaces test_thai_grapheme_cluster_movement (reverse subset).
    // From end of "aที่b" (byte 11), three Left arrows: 'b' → 10,
    // cluster → 1, 'a' → 0.
    assert_buffer_theorem(BufferTheorem {
        description: "Left arrow x3 from end of 'aที่b' visits bytes 10, 1, 0 (cluster atomic)",
        initial_text: "aที่b",
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::MoveLeft,
            Action::MoveLeft,
        ],
        expected_text: "aที่b",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_backspace_peels_thai_combining_marks_layer_by_layer() {
    // Replaces test_thai_backspace_layer_by_layer.
    // Backspace deletes ONE code point, not one grapheme — this is
    // the "fix a typo in a tone mark without retyping the whole
    // character" behavior. Three backspaces over "ที่" yield
    // "ที", "ท", "".
    assert_buffer_theorem(BufferTheorem {
        description: "Three backspaces over ที่ peel one code point each (tone, vowel, base)",
        initial_text: "ที่",
        actions: vec![
            Action::MoveDocumentEnd,
            Action::DeleteBackward,
            Action::DeleteBackward,
            Action::DeleteBackward,
        ],
        expected_text: "",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
fn theorem_delete_forward_removes_thai_grapheme_atomically() {
    // Replaces test_thai_delete_entire_cluster.
    // From byte 0 of "ที่นี่" (two grapheme clusters), DeleteForward
    // removes the *whole* first cluster — not one code point —
    // because deleting just the base would orphan the marks.
    assert_buffer_theorem(BufferTheorem {
        description: "DeleteForward removes a whole Thai grapheme cluster, not one code point",
        initial_text: "ที่นี่",
        actions: vec![Action::MoveDocumentStart, Action::DeleteForward],
        expected_text: "นี่",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}
