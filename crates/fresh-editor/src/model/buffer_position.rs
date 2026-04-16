//! Pure conversions between byte offsets and 2D positions on a `Buffer`.
//!
//! These helpers consolidate the `byte_to_2d` / `pos_2d_to_byte` functions
//! that previously existed as duplicates in `app/clipboard.rs` and
//! `input/actions.rs`. They are free functions, not methods, to avoid
//! growing `TextBuffer`'s API surface — both callers already have a
//! `&Buffer` in hand.

use crate::model::buffer::Buffer;
use crate::model::cursor::Position2D;

/// Convert a byte offset into a (line, column) position.
///
/// The column is measured in *bytes from the start of the line*, not
/// visual columns — this matches the cursor position model used by the
/// editing core.
pub fn byte_to_2d(buffer: &Buffer, byte_pos: usize) -> Position2D {
    let line = buffer.get_line_number(byte_pos);
    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let column = byte_pos.saturating_sub(line_start);
    Position2D { line, column }
}

/// Convert a 2D position into a byte offset, clamping the column to the
/// line's byte length (excluding the trailing newline, if any).
pub fn pos_2d_to_byte(buffer: &Buffer, pos: Position2D) -> usize {
    let line_start = buffer.line_start_offset(pos.line).unwrap_or(0);
    let line_content = buffer.get_line(pos.line).unwrap_or_default();
    let line_len = if line_content.last() == Some(&b'\n') {
        line_content.len().saturating_sub(1)
    } else {
        line_content.len()
    };
    let clamped_col = pos.column.min(line_len);
    line_start + clamped_col
}
