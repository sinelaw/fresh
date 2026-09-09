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
    let Some(width) = line_width_at_content_end(buffer, cursor.position) else {
        return 0;
    };
    sticky.saturating_sub(width)
}

/// How many virtual lines below the end of the buffer the cursor sits on
/// (vertical virtual space, set by clicking below the last line).
///
/// Returns 0 unless `mode` allows the cursor beyond EOL, the cursor is
/// collapsed, and it sits exactly at the buffer end (the only position
/// where `virtual_lines_below` is meaningful).
pub fn cursor_virtual_lines(mode: VirtualSpaceMode, buffer: &Buffer, cursor: &Cursor) -> usize {
    if !mode.cursor_beyond_eol()
        || cursor.anchor.is_some()
        || cursor.selection_mode == SelectionMode::Block
        || cursor.position != buffer.len()
    {
        return 0;
    }
    cursor.virtual_lines_below
}

/// The text that materializes the cursor's virtual position when an edit
/// lands there, to be inserted at the cursor's (clipped) byte position
/// before the edited text: line endings down to the cursor's virtual line,
/// then spaces out to its column — or just the column padding when the
/// cursor is virtual only horizontally. Empty when the cursor isn't in
/// virtual space.
pub fn virtual_gap_text(
    mode: VirtualSpaceMode,
    buffer: &Buffer,
    cursor: &Cursor,
    line_ending: &str,
) -> String {
    let vlines = cursor_virtual_lines(mode, buffer, cursor);
    if vlines > 0 {
        // Virtual lines are empty, so the column padding is the full sticky
        // column (one space per column from the line start).
        let cols = cursor.sticky_column.unwrap_or(0);
        format!("{}{}", line_ending.repeat(vlines), " ".repeat(cols))
    } else {
        " ".repeat(cursor_virtual_columns(mode, buffer, cursor))
    }
}

/// If `position` sits exactly at its line's content end, the visual width
/// of that line's content; `None` otherwise. This is the precondition for a
/// cursor at `position` to be virtual, and the base the sticky column is
/// measured against.
pub fn line_width_at_content_end(buffer: &Buffer, position: usize) -> Option<usize> {
    let line = buffer.get_line_number(position);
    let line_start = buffer.line_start_offset(line)?;
    let content = buffer.get_line(line).unwrap_or_default();
    let text = String::from_utf8_lossy(&content);
    let content_len = text.trim_end_matches(['\r', '\n']).len();
    if position != line_start + content_len {
        return None;
    }
    Some(visual_column_at_byte(&text, content_len))
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
    fn virtual_lines_only_at_buffer_end() {
        let buf = buffer("ab\nxyz");
        let mut c = cursor_at(6, Some(3));
        c.virtual_lines_below = 2;
        assert_eq!(cursor_virtual_lines(VirtualSpaceMode::On, &buf, &c), 2);
        assert_eq!(cursor_virtual_lines(VirtualSpaceMode::Off, &buf, &c), 0);
        assert_eq!(cursor_virtual_lines(VirtualSpaceMode::Block, &buf, &c), 0);

        // Not at the buffer end → not on a virtual line.
        c.position = 2;
        assert_eq!(cursor_virtual_lines(VirtualSpaceMode::On, &buf, &c), 0);

        // A selection disables it.
        c.position = 6;
        c.anchor = Some(0);
        assert_eq!(cursor_virtual_lines(VirtualSpaceMode::On, &buf, &c), 0);
    }

    #[test]
    fn gap_text_covers_lines_then_columns() {
        let buf = buffer("ab");
        // Cursor two virtual lines below the end, column 3.
        let mut c = cursor_at(2, Some(3));
        c.virtual_lines_below = 2;
        assert_eq!(
            virtual_gap_text(VirtualSpaceMode::On, &buf, &c, "\n"),
            "\n\n   "
        );
        // CRLF buffers materialize CRLF line endings.
        assert_eq!(
            virtual_gap_text(VirtualSpaceMode::On, &buf, &c, "\r\n"),
            "\r\n\r\n   "
        );

        // Horizontal-only virtual position: spaces past the line width.
        let c = cursor_at(2, Some(5));
        assert_eq!(
            virtual_gap_text(VirtualSpaceMode::On, &buf, &c, "\n"),
            "   "
        );

        // Not virtual → empty.
        let c = cursor_at(1, None);
        assert_eq!(virtual_gap_text(VirtualSpaceMode::On, &buf, &c, "\n"), "");
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
