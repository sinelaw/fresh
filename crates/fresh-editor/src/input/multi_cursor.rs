//! Multi-cursor operations for adding cursors at various positions

use crate::model::cursor::{Cursor, Cursors};
use crate::primitives::word_navigation::{find_word_end, find_word_start};
use crate::state::EditorState;

/// Result of attempting to add a cursor
pub enum AddCursorResult {
    /// Cursor was added successfully
    Success {
        cursor: Cursor,
        total_cursors: usize,
    },
    /// Word was selected (no new cursor added, but primary cursor selection changed)
    /// This happens when Ctrl+D is pressed with no selection - it selects the current word
    WordSelected { word_start: usize, word_end: usize },
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
fn success_result(cursor: Cursor, cursors: &Cursors) -> AddCursorResult {
    AddCursorResult::Success {
        cursor,
        total_cursors: cursors.iter().count() + 1,
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

/// Where the next match of `query` starts at or after `from`, wrapping to
/// the start of the buffer.
fn next_match(
    buffer: &mut crate::model::buffer::TextBuffer,
    query: &crate::model::buffer::SearchQuery,
    from: usize,
) -> Option<usize> {
    let len = buffer.len();
    [from.min(len)..len, 0..len].into_iter().find_map(|range| {
        let found = buffer.find_regex_matches(query, range, 1).ok()?;
        found.first().map(|&(pos, _)| pos)
    })
}

/// Add a cursor at the next occurrence of the selected text
/// If no selection, selects the entire word at cursor position first
pub fn add_cursor_at_next_match(state: &mut EditorState, cursors: &Cursors) -> AddCursorResult {
    // Get the selected text from the primary cursor
    let primary = cursors.primary();
    let selection_range = match primary.selection_range() {
        Some(range) => range,
        None => {
            // No selection - select the entire word at cursor position
            let cursor_pos = primary.position;
            let word_start = find_word_start(&state.buffer, cursor_pos);

            // Determine word_end: if we're just past a word (at a non-word char but
            // word_start < cursor_pos), use cursor_pos as the end. This handles the
            // case where cursor is at the space right after a word.
            let word_end = if word_start < cursor_pos {
                // Check if we're at a word character
                let at_word_char = if cursor_pos < state.buffer.len() {
                    if let Ok(bytes) = state.buffer.get_text_range_mut(cursor_pos, 1) {
                        bytes
                            .first()
                            .map(|&b| crate::primitives::word_navigation::is_word_char(b))
                            .unwrap_or(false)
                    } else {
                        false
                    }
                } else {
                    false
                };

                if at_word_char {
                    // We're in the middle of a word, find the actual end
                    find_word_end(&state.buffer, cursor_pos)
                } else {
                    // We're just past a word, use cursor position as end
                    cursor_pos
                }
            } else {
                // word_start == cursor_pos, find the end normally
                find_word_end(&state.buffer, cursor_pos)
            };

            // If cursor is on whitespace or punctuation (word_start == word_end), fail
            if word_start == word_end {
                return AddCursorResult::Failed {
                    message: "No word at cursor position".to_string(),
                };
            }

            // Return WordSelected so caller can update the cursor's selection
            return AddCursorResult::WordSelected {
                word_start,
                word_end,
            };
        }
    };

    // Determine if the original selection is "backward" (cursor at start of selection)
    let cursor_at_start = primary.position == selection_range.start;

    // Extract the selected text
    let pattern = state.get_text_range(selection_range.start, selection_range.end);
    let pattern_len = pattern.len();
    let Ok(query) = crate::model::buffer::SearchQuery::new(&pattern, false, false, true) else {
        return AddCursorResult::Failed {
            message: "No more matches".to_string(),
        };
    };

    // Step through the matches after the selection, wrapping at the end,
    // until one no cursor holds. Every match position seen is remembered, so
    // when they are all held the search stops once it comes round again.
    let mut search_start = selection_range.end;
    let mut seen = std::collections::HashSet::new();
    loop {
        let Some(match_pos) = next_match(&mut state.buffer, &query, search_start) else {
            return AddCursorResult::Failed {
                message: "No more matches".to_string(),
            };
        };
        if !seen.insert(match_pos) {
            return AddCursorResult::Failed {
                message: "All matches are already selected".to_string(),
            };
        }

        let match_range = match_pos..(match_pos + pattern_len);
        let is_occupied = cursors
            .iter()
            .any(|(_, c)| c.selection_range() == Some(match_range.clone()));
        if !is_occupied {
            let new_cursor = if cursor_at_start {
                let mut cursor = Cursor::new(match_range.start);
                cursor.set_anchor(match_range.end);
                cursor
            } else {
                Cursor::with_selection(match_range.start, match_range.end)
            };
            return success_result(new_cursor, cursors);
        }
        search_start = match_range.end;
    }
}

/// Add a cursor above the primary cursor at the same column
pub fn add_cursor_above(state: &mut EditorState, cursors: &Cursors) -> AddCursorResult {
    let position = cursors.primary().position;

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
        success_result(Cursor::new(new_pos), cursors)
    } else {
        AddCursorResult::Failed {
            message: "Already at first line".to_string(),
        }
    }
}

/// Compute end-of-line byte positions for every line covered by ANY existing
/// cursor's selection (or the cursor's current line when there is no
/// selection). Used to place cursors at the end of each line, matching
/// VSCode's "Add Cursor to Line Ends" / Sublime's "Split Selection into Lines"
/// semantics where every existing cursor contributes, not just the primary.
///
/// "End of line" is the byte offset immediately before the trailing `\n`
/// (or `\r\n`), or the position past the last byte for the final line of a
/// buffer without a trailing newline.
///
/// Returned positions are sorted in document order and deduplicated, so two
/// cursors on the same line collapse to a single line-end.
pub fn line_end_positions_in_selection(state: &mut EditorState, cursors: &Cursors) -> Vec<usize> {
    let mut positions = Vec::new();
    for (_id, cursor) in cursors.iter() {
        let (range_start, range_end) = match cursor.selection_range() {
            Some(range) => (range.start, range.end),
            None => (cursor.position, cursor.position),
        };
        let mut iter = state.buffer.line_iterator(range_start, 80);
        while let Some((line_start, line_content)) = iter.next_line() {
            // Stop when we've moved past the selection's end line.
            // The line we want to include is the one containing range_end;
            // a line strictly after that has line_start > range_end.
            if line_start > range_end {
                break;
            }
            let trimmed = line_content
                .trim_end_matches('\n')
                .trim_end_matches('\r')
                .len();
            positions.push(line_start + trimmed);
        }
    }
    positions.sort_unstable();
    positions.dedup();
    positions
}

/// Add a cursor below the primary cursor at the same column
pub fn add_cursor_below(state: &mut EditorState, cursors: &Cursors) -> AddCursorResult {
    let position = cursors.primary().position;

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
        success_result(Cursor::new(new_pos), cursors)
    } else {
        AddCursorResult::Failed {
            message: "Already at last line".to_string(),
        }
    }
}
