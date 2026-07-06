//! Virtual space: cursor columns past the end of a line.
//!
//! Fresh cursors are byte offsets, so a cursor can never *store* a position
//! beyond a line's content. Virtual space instead derives it: a collapsed
//! cursor sitting at its line's content end whose `sticky_column` (the
//! desired visual column, carried by every `MoveCursor` event and preserved
//! by vertical movement) exceeds the line's visual width is *virtually* at
//! that sticky column. Rendering draws it there, mouse clicks past EOL set
//! it, and typing materializes the gap with spaces.
//!
//! This is the single source of truth for that derivation
//! (`cursor_virtual_columns`); movement, rendering, editing, and mouse code
//! all call it rather than re-deriving the rule. The
//! [`VirtualSpaceMode`](crate::config::VirtualSpaceMode) gate is part of the
//! signature so a disabled config can never leak a virtual position.
//!
//! Invariants:
//! - The buffer is never mutated by movement; spaces appear only when an
//!   edit happens at a virtual position.
//! - Byte positions given to LSP, plugins, and selections are always the
//!   clipped position (the line content end); the virtual columns are a
//!   view/editing concept only.
//! - A cursor with a selection is never virtual (linear selections stay
//!   byte-based; block selections carry their own column geometry).

use crate::config::VirtualSpaceMode;
use crate::model::buffer::Buffer;
use crate::model::cursor::{Cursor, SelectionMode};
use crate::primitives::display_width::visual_column_at_byte;

/// How many visual columns past its line's content end the cursor sits.
///
/// Returns 0 unless all of these hold:
/// - `mode` allows the cursor beyond EOL,
/// - the cursor is collapsed (no linear or block selection),
/// - its byte position is exactly at the line's content end,
/// - its `sticky_column` exceeds the line content's visual width.
pub fn cursor_virtual_columns(mode: VirtualSpaceMode, buffer: &Buffer, cursor: &Cursor) -> usize {
    if !mode.cursor_beyond_eol() {
        return 0;
    }
    if cursor.anchor.is_some() || cursor.selection_mode == SelectionMode::Block {
        return 0;
    }
    let Some(sticky) = cursor.sticky_column else {
        return 0;
    };

    let line = buffer.get_line_number(cursor.position);
    let Some(line_start) = buffer.line_start_offset(line) else {
        return 0;
    };
    let content = buffer.get_line(line).unwrap_or_default();
    let text = String::from_utf8_lossy(&content);
    let content_len = text.trim_end_matches(['\r', '\n']).len();
    if cursor.position != line_start + content_len {
        return 0;
    }

    let width = visual_column_at_byte(&text, content_len);
    sticky.saturating_sub(width)
}

/// The sticky column that places a cursor `virtual_columns` past the end of
/// the line containing `line_content_end` (a byte position at a line's
/// content end). Inverse of [`cursor_virtual_columns`].
pub fn sticky_for_virtual_position(
    buffer: &Buffer,
    line_content_end: usize,
    virtual_columns: usize,
) -> usize {
    let width =
        crate::primitives::display_width::visual_column_of(buffer, line_content_end).unwrap_or(0);
    width + virtual_columns
}

#[cfg(test)]
mod tests {
    use super::*;

    fn buffer(s: &str) -> Buffer {
        Buffer::from_str_test(s)
    }

    fn cursor_at(position: usize, sticky: Option<usize>) -> Cursor {
        let mut c = Cursor::new(position);
        c.sticky_column = sticky;
        c
    }

    #[test]
    fn zero_when_mode_off() {
        let buf = buffer("ab\nxyz");
        let c = cursor_at(2, Some(10));
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::Off, &buf, &c), 0);
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::Block, &buf, &c), 0);
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &buf, &c), 8);
    }

    #[test]
    fn zero_without_sticky_or_mid_line() {
        let buf = buffer("ab\nxyz");
        // No sticky column → not virtual.
        assert_eq!(
            cursor_virtual_columns(VirtualSpaceMode::On, &buf, &cursor_at(2, None)),
            0
        );
        // Mid-line position → not virtual even with a large sticky.
        assert_eq!(
            cursor_virtual_columns(VirtualSpaceMode::On, &buf, &cursor_at(1, Some(10))),
            0
        );
        // Sticky within the line width → not virtual.
        assert_eq!(
            cursor_virtual_columns(VirtualSpaceMode::On, &buf, &cursor_at(2, Some(1))),
            0
        );
        // Sticky exactly at the line width → not virtual.
        assert_eq!(
            cursor_virtual_columns(VirtualSpaceMode::On, &buf, &cursor_at(2, Some(2))),
            0
        );
    }

    #[test]
    fn zero_with_selection() {
        let buf = buffer("ab\nxyz");
        let mut c = cursor_at(2, Some(10));
        c.anchor = Some(0);
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &buf, &c), 0);
    }

    #[test]
    fn wide_chars_measure_visually() {
        // 你好 is 6 bytes but 4 visual columns wide.
        let buf = buffer("你好\nxyz");
        let c = cursor_at(6, Some(9));
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &buf, &c), 5);
    }

    #[test]
    fn empty_line_and_empty_buffer() {
        let buf = buffer("ab\n\nxyz");
        // Cursor on the empty middle line (byte 3).
        let c = cursor_at(3, Some(7));
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &buf, &c), 7);

        let empty = buffer("");
        let c = cursor_at(0, Some(5));
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &empty, &c), 5);
    }

    #[test]
    fn crlf_line_content_end() {
        let buf = buffer("ab\r\nxyz");
        // Content end of line 0 is byte 2 (before \r\n).
        let c = cursor_at(2, Some(6));
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &buf, &c), 4);
    }

    #[test]
    fn sticky_roundtrip() {
        let buf = buffer("你好\nxyz");
        let sticky = sticky_for_virtual_position(&buf, 6, 5);
        assert_eq!(sticky, 9);
        let c = cursor_at(6, Some(sticky));
        assert_eq!(cursor_virtual_columns(VirtualSpaceMode::On, &buf, &c), 5);
    }
}
