//! Multi-cursor operations for adding cursors at various positions

use crate::model::cursor::Cursor;
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

/// Information about a cursor's position within its line
struct CursorLineInfo {
    /// Byte offset of the line start
    line_start: usize,
    /// Column offset from line start
    col_offset: usize,
}

/// Get line info for a cursor position
fn get_cursor_line_info(state: &mut EditorState, position: usize) -> Option<CursorLineInfo> {
    let mut iter = state.buffer.line_iterator(position, 80);
    let (line_start, _) = iter.next_line()?;
    Some(CursorLineInfo {
        line_start,
        col_offset: position.saturating_sub(line_start),
    })
}

/// Calculate cursor position on a line, clamping to line length (excluding newline)
fn cursor_position_on_line(line_start: usize, line_content: &str, target_col: usize) -> usize {
    let line_len = line_content.trim_end_matches('\n').len();
    line_start + target_col.min(line_len)
}

/// Create a successful AddCursorResult
fn success_result(cursor: Cursor, state: &EditorState) -> AddCursorResult {
    AddCursorResult::Success {
        cursor,
        total_cursors: state.cursors.iter().count() + 1,
    }
}

/// Adjust cursor position if it's on a newline character
/// Returns position + 1 if cursor is at a newline, otherwise returns position unchanged
fn adjust_position_for_newline(state: &mut EditorState, position: usize) -> usize {
    if position < state.buffer.len() {
        if let Ok(byte_at_cursor) = state.buffer.get_text_range_mut(position, 1) {
            if byte_at_cursor.first() == Some(&b'\n') {
                return position + 1;
            }
        }
    }
    position
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

    // Determine if the original selection is "backward" (cursor at start of selection)
    let cursor_at_start = primary.position == selection_range.start;

    // Extract the selected text
    let pattern = state.get_text_range(selection_range.start, selection_range.end);
    let pattern_len = pattern.len();

    // Start searching from the end of the current selection
    let mut search_start = selection_range.end;
    let _ign = search_start; // To prevent infinite loops (unused now)

    // Loop until we find a match that isn't already occupied by a cursor
    loop {
        let match_pos = match state.buffer.find_next(&pattern, search_start) {
            Some(pos) => pos,
            None => {
                // If finding next failed even with wrap-around (implied by buffer.find_next usually),
                // then truly no matches exist.
                return AddCursorResult::Failed {
                    message: "No more matches".to_string(),
                };
            }
        };

        // Calculate the range of the found match
        let match_range = match_pos..(match_pos + pattern_len);

        // Check if any existing cursor overlaps with this match
        let is_occupied = state.cursors.iter().any(|(_, c)| {
            if let Some(r) = c.selection_range() {
                r == match_range
            } else {
                false
            }
        });

        if !is_occupied {
            // Found a free match!
            let match_start = match_pos;
            let match_end = match_pos + pattern_len;
            let new_cursor = if cursor_at_start {
                let mut cursor = Cursor::new(match_start);
                cursor.set_anchor(match_end);
                cursor
            } else {
                Cursor::with_selection(match_start, match_end)
            };
            return success_result(new_cursor, state);
        }

        // If we wrapped around and came back to where we started searching (or past it), stop to avoid infinite loop
        // We need to handle the case where find_next wraps around.
        // Assuming buffer.find_next does wrap around:
        // If match_pos <= search_start and we haven't wrapped explicitly, it means we wrapped.

        // Let's refine the search start. We want to search *after* this occupied match.
        // If match_pos is behind us, we wrapped.

        let next_start = match_pos + pattern_len;

        // Simple cycle detection: if we are stuck on the same spot or have cycled through the whole buffer
        // Ideally we check if we've visited this match_pos before, but checking if we passed initial_start again is a decent proxy
        // provided we handle the wrap-around logic correctly.

        // If find_next scans the whole buffer, it might return the same spot if it's the only match.
        // If it's occupied, we are done.

        // To be safe against infinite loops if all matches are occupied:
        if match_pos == selection_range.start {
            // We wrapped all the way back to the primary cursor without finding a free spot
            return AddCursorResult::Failed {
                message: "All matches are already selected".to_string(),
            };
        }

        search_start = next_start;
    }
}

/// Add a cursor above the primary cursor at the same column
pub fn add_cursor_above(state: &mut EditorState) -> AddCursorResult {
    let position = state.cursors.primary().position;

    // Adjust position if cursor is at a newline character
    // This handles cases where add_cursor_above/below places cursor at same column
    let adjusted_position = adjust_position_for_newline(state, position);

    // Get current line info
    let Some(info) = get_cursor_line_info(state, adjusted_position) else {
        return AddCursorResult::Failed {
            message: "Unable to find current line".to_string(),
        };
    };

    // Check if we're on the first line
    if info.line_start == 0 {
        return AddCursorResult::Failed {
            message: "Already at first line".to_string(),
        };
    }

    // Navigate to previous line using iterator
    let mut iter = state.buffer.line_iterator(adjusted_position, 80);
    iter.next_line(); // Consume current line
    iter.prev(); // Move back to current line

    // Get the previous line
    if let Some((prev_line_start, prev_line_content)) = iter.prev() {
        let new_pos = cursor_position_on_line(prev_line_start, &prev_line_content, info.col_offset);
        success_result(Cursor::new(new_pos), state)
    } else {
        AddCursorResult::Failed {
            message: "Already at first line".to_string(),
        }
    }
}

/// Add a cursor below the primary cursor at the same column
pub fn add_cursor_below(state: &mut EditorState) -> AddCursorResult {
    let position = state.cursors.primary().position;

    // Get current line info
    let Some(info) = get_cursor_line_info(state, position) else {
        return AddCursorResult::Failed {
            message: "Unable to find current line".to_string(),
        };
    };

    // Navigate to next line using iterator
    let mut iter = state.buffer.line_iterator(position, 80);
    iter.next_line(); // Consume current line

    // Get next line
    if let Some((next_line_start, next_line_content)) = iter.next_line() {
        let new_pos = cursor_position_on_line(next_line_start, &next_line_content, info.col_offset);
        success_result(Cursor::new(new_pos), state)
    } else {
        AddCursorResult::Failed {
            message: "Already at last line".to_string(),
        }
    }
}
