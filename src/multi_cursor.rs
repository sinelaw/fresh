//! Multi-cursor operations for adding cursors at various positions

use crate::cursor::Cursor;
use crate::state::EditorState;

/// Result of attempting to add a cursor
pub enum AddCursorResult {
    /// Cursor was added successfully
    Success {
        cursor: Cursor,
        total_cursors: usize,
    },
    /// Operation failed with a message
    Failed { message: String },
}

/// Add a cursor at the next occurrence of the selected text
/// If no selection, returns Failed
pub fn add_cursor_at_next_match(state: &mut EditorState) -> AddCursorResult {
    // Get the selected text from the primary cursor
    let primary = state.cursors.primary();
    let selection_range = match primary.selection_range() {
        Some(range) => range,
        None => {
            return AddCursorResult::Failed {
                message: "No selection to match".to_string(),
            }
        }
    };

    // Extract the selected text
    let pattern = state.get_text_range(selection_range.start, selection_range.end);

    // Find the next occurrence after the current selection
    let search_start = selection_range.end;
    let match_pos = match state.buffer.find_next(&pattern, search_start) {
        Some(pos) => pos,
        None => {
            return AddCursorResult::Failed {
                message: "No more matches".to_string(),
            }
        }
    };

    // Create a new cursor at the match position with selection
    let new_cursor = Cursor::with_selection(match_pos, match_pos + pattern.len());

    AddCursorResult::Success {
        cursor: new_cursor,
        total_cursors: state.cursors.iter().count() + 1,
    }
}

/// Add a cursor above the primary cursor at the same column
pub fn add_cursor_above(state: &mut EditorState) -> AddCursorResult {
    let primary = state.cursors.primary();

    // Find the start of the current line using iterator
    let mut iter = state.buffer.line_iterator(primary.position, 80);
    let Some((line_start, _line_content)) = iter.next() else {
        return AddCursorResult::Failed {
            message: "Unable to find current line".to_string(),
        };
    };

    // Check if we're on the first line
    if line_start == 0 {
        return AddCursorResult::Failed {
            message: "Already at first line".to_string(),
        };
    }

    // Calculate column offset from line start
    let col_offset = primary.position - line_start;

    // After next(), iterator is positioned after current line
    // Call prev() twice: once to get back to current line, once more to get previous line
    iter.prev(); // Move back to current line

    // Get the previous line
    if let Some((prev_line_start, prev_line_content)) = iter.prev() {
        // Calculate new position on previous line, capping at line length
        let prev_line_len = prev_line_content.len();
        let new_pos = prev_line_start + col_offset.min(prev_line_len);

        let new_cursor = Cursor::new(new_pos);

        AddCursorResult::Success {
            cursor: new_cursor,
            total_cursors: state.cursors.iter().count() + 1,
        }
    } else {
        AddCursorResult::Failed {
            message: "Already at first line".to_string(),
        }
    }
}

/// Add a cursor below the primary cursor at the same column
pub fn add_cursor_below(state: &mut EditorState) -> AddCursorResult {
    let primary = state.cursors.primary();

    // Find the start of the current line using iterator
    let mut iter = state.buffer.line_iterator(primary.position, 80);
    let Some((line_start, _)) = iter.next() else {
        return AddCursorResult::Failed {
            message: "Unable to find current line".to_string(),
        };
    };

    // Calculate column offset from line start
    let col_offset = primary.position - line_start;

    // Get next line (we already consumed current line with first iter.next())
    if let Some((next_line_start, next_line_content)) = iter.next() {
        // Calculate new position on next line, capping at line length
        let next_line_len = next_line_content.len();
        let new_pos = next_line_start + col_offset.min(next_line_len);
        let new_cursor = Cursor::new(new_pos);

        AddCursorResult::Success {
            cursor: new_cursor,
            total_cursors: state.cursors.iter().count() + 1,
        }
    } else {
        AddCursorResult::Failed {
            message: "Already at last line".to_string(),
        }
    }
}
