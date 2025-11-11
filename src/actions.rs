//! Action to event conversion - translates high-level actions into buffer events

use crate::buffer::Buffer;
use crate::event::Event;
use crate::keybindings::Action;
use crate::state::EditorState;
use crate::word_navigation::{
    find_word_end, find_word_start, find_word_start_left, find_word_start_right,
};

/// Calculate the maximum valid cursor position in the buffer.
/// This is the end of the last line (excluding trailing newline).
/// For empty buffers, returns 0.
fn max_cursor_position(buffer: &Buffer) -> usize {
    if buffer.is_empty() {
        return 0;
    }

    // Check if buffer ends with newline (creating an implicit empty last line)
    let buffer_ends_with_newline = {
        let last_char = buffer.slice(buffer.len() - 1..buffer.len());
        last_char == "\n"
    };

    // If buffer ends with newline, cursor can go to buffer.len() (the empty last line)
    if buffer_ends_with_newline {
        return buffer.len();
    }

    // Find the last line by iterating from the end
    let mut iter = buffer.line_iterator(buffer.len().saturating_sub(1));

    // Get the current line (which should be the last line)
    if let Some((line_start, line_content)) = iter.next() {
        // Calculate end position excluding the trailing newline
        let line_len = line_content.trim_end_matches('\n').len();
        line_start + line_len
    } else {
        // Fallback to 0 if we can't find a line
        0
    }
}

/// Convert an action into a sequence of events that can be applied to the editor state
///
/// # Parameters
/// * `state` - The current editor state
/// * `action` - The action to convert
/// * `tab_size` - Number of spaces per tab
/// * `auto_indent` - Whether auto-indent is enabled
///
/// # Returns
/// * `Some(Vec<Event>)` - Events to apply for this action
/// * `None` - If the action doesn't generate events (like Quit, Save, etc.)
pub fn action_to_events(
    state: &EditorState,
    action: Action,
    tab_size: usize,
    auto_indent: bool,
) -> Option<Vec<Event>> {
    let mut events = Vec::new();

    match action {
        // Character input - insert at each cursor
        Action::InsertChar(ch) => {
            // Collect cursors and sort by position (reverse order)
            // This ensures insertions at later positions happen first,
            // avoiding position shifts that would affect earlier insertions
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Check if this is a closing delimiter that should trigger auto-dedent
            let is_closing_delimiter = matches!(ch, '}' | ')' | ']');

            for (cursor_id, cursor) in cursor_vec {
                let insert_position = if let Some(range) = cursor.selection_range() {
                    let start = range.start;
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                    start
                } else {
                    cursor.position
                };

                // Auto-dedent logic for closing delimiters
                if is_closing_delimiter && auto_indent {
                    // Find the start of the current line
                    let mut line_start = insert_position;
                    while line_start > 0 {
                        let prev = line_start - 1;
                        if state.buffer.slice_bytes(prev..prev + 1).first() == Some(&b'\n') {
                            break;
                        }
                        line_start = prev;
                    }

                    // Check if line only contains spaces before cursor
                    let line_before_cursor = state.buffer.slice_bytes(line_start..insert_position);
                    let only_spaces = line_before_cursor.iter().all(|&b| b == b' ' || b == b'\t');

                    if only_spaces && insert_position > line_start {
                        // Calculate correct indent for the closing delimiter using tree-sitter
                        // This temporarily inserts the delimiter and uses @dedent captures to find correct position
                        let correct_indent = if let Some(highlighter) = &state.highlighter {
                            let language = highlighter.language();
                            let result = state
                                .indent_calculator
                                .borrow_mut()
                                .calculate_dedent_for_delimiter(
                                    &state.buffer,
                                    insert_position,
                                    ch,
                                    language,
                                    tab_size,
                                )
                                .unwrap_or(0);
                            result
                        } else {
                            0
                        };

                        // Delete the incorrect spacing
                        let spaces_to_delete = insert_position - line_start;
                        if spaces_to_delete > 0 {
                            events.push(Event::Delete {
                                range: line_start..insert_position,
                                deleted_text: state.buffer.slice(line_start..insert_position),
                                cursor_id,
                            });
                        }

                        // Insert correct spacing + the closing delimiter
                        let mut text = " ".repeat(correct_indent);
                        text.push(ch);
                        events.push(Event::Insert {
                            position: line_start,
                            text,
                            cursor_id,
                        });
                        continue;
                    }
                }

                // Normal character insertion
                events.push(Event::Insert {
                    position: insert_position,
                    text: ch.to_string(),
                    cursor_id,
                });
            }
        }

        Action::InsertNewline => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            for (cursor_id, cursor) in cursor_vec {
                // When there's a selection, the cursor will be at the start of the range after deletion
                let indent_position = if let Some(range) = cursor.selection_range() {
                    let start = range.start; // Extract start before moving range
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                    start // Use start of selection for indent calculation
                } else {
                    cursor.position // No selection, use cursor position
                };

                // Calculate indent for new line
                let mut text = "\n".to_string();

                if auto_indent {
                    if let Some(highlighter) = &state.highlighter {
                        // Use tree-sitter-based indent when we have a highlighter
                        let language = highlighter.language();
                        if let Some(indent_spaces) = state
                            .indent_calculator
                            .borrow_mut()
                            .calculate_indent(&state.buffer, indent_position, language, tab_size)
                        {
                            text.push_str(&" ".repeat(indent_spaces));
                        }
                    } else {
                        // Fallback for files without syntax highlighting (e.g., .txt)
                        let indent_spaces =
                            crate::indent::IndentCalculator::calculate_indent_no_language(
                                &state.buffer,
                                indent_position,
                                tab_size,
                            );
                        text.push_str(&" ".repeat(indent_spaces));
                    }
                }

                events.push(Event::Insert {
                    position: indent_position,
                    text,
                    cursor_id,
                });
            }
        }

        Action::InsertTab => {
            let tab_str = " ".repeat(tab_size);
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            for (cursor_id, cursor) in cursor_vec {
                if let Some(range) = cursor.selection_range() {
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                }

                events.push(Event::Insert {
                    position: cursor.position,
                    text: tab_str.clone(),
                    cursor_id,
                });
            }
        }

        // Basic movement - move each cursor
        Action::MoveLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use prev_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state.buffer.prev_char_boundary(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None, // No selection
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column on horizontal movement
                });
            }
        }

        Action::MoveRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                // Use next_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state
                    .buffer
                    .next_char_boundary(cursor.position)
                    .min(max_pos);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column on horizontal movement
                });
            }
        }

        Action::MoveUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use iterator to navigate to previous line
                // line_iterator positions us at the start of the current line
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Get previous line
                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    // Calculate length without trailing newline
                    let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                    let new_pos = prev_line_start + goal_column.min(prev_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::MoveDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Skip current line, then get next line
                iter.next();
                if let Some((next_line_start, next_line_content)) = iter.next() {
                    // Calculate length without trailing newline
                    let next_line_len = next_line_content.trim_end_matches('\n').len();
                    let new_pos = next_line_start + goal_column.min(next_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::MoveLineStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                if let Some((line_start, _)) = iter.next() {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_start,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::MoveLineEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                if let Some((line_start, line_content)) = iter.next() {
                    // Calculate end position (exclude newline)
                    let line_len = line_content.trim_end_matches('\n').len();
                    let line_end = line_start + line_len;

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor: None,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::MoveWordLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_left(&state.buffer, cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveWordRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveDocumentStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: 0,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveDocumentEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: max_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MovePageUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Move up by viewport height
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.prev() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        new_pos = 0;
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::MovePageDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Move down by viewport height
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Consume current line
                iter.next();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        // Reached end of buffer - clamp to last valid position
                        new_pos = max_cursor_position(&state.buffer);
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        // Selection movement - same as regular movement but keeps anchor
        Action::SelectLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use prev_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state.buffer.prev_char_boundary(cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                // Use next_char_boundary to ensure we land on a valid UTF-8 character boundary
                let new_pos = state
                    .buffer
                    .next_char_boundary(cursor.position)
                    .min(max_pos);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                    let new_pos = prev_line_start + goal_column.min(prev_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::SelectDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Skip current line, then get next line
                iter.next();
                if let Some((next_line_start, next_line_content)) = iter.next() {
                    let next_line_len = next_line_content.trim_end_matches('\n').len();
                    let new_pos = next_line_start + goal_column.min(next_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_column, // Preserve the goal column
                    });
                }
            }
        }

        Action::SelectLineStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((line_start, _)) = iter.next() {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_start,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::SelectLineEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((line_start, line_content)) = iter.next() {
                    // Calculate end position (exclude newline)
                    let line_len = line_content.trim_end_matches('\n').len();
                    let line_end = line_start + line_len;

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::SelectWordLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_left(&state.buffer, cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectWordRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectDocumentStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: 0,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectDocumentEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: max_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::SelectPageUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.prev() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        new_pos = 0;
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::SelectPageDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Consume current line
                iter.next();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        // Reached end of buffer - clamp to last valid position
                        new_pos = max_cursor_position(&state.buffer);
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::SelectAll => {
            // Select entire buffer for primary cursor only
            let primary_id = state.cursors.primary_id();
            let primary_cursor = state.cursors.primary();
            let max_pos = max_cursor_position(&state.buffer);
            events.push(Event::MoveCursor {
                cursor_id: primary_id,
                old_position: primary_cursor.position,
                new_position: max_pos,
                old_anchor: primary_cursor.anchor,
                new_anchor: Some(0),
                old_sticky_column: primary_cursor.sticky_column,
                new_sticky_column: 0, // Reset sticky column
            });
            // Note: RemoveSecondaryCursors is handled in handle_key, not as an event
        }

        Action::SelectWord => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Find word boundaries at current position
                // First find the start of the word we're in/adjacent to
                let word_start = find_word_start(&state.buffer, cursor.position);
                // Then find the end of that word (from the start, not from cursor)
                // This ensures we select the current word, not the next one
                let word_end = find_word_end(&state.buffer, word_start);

                if word_start < word_end {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: word_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(word_start),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::DeleteBackward => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            for (cursor_id, cursor) in cursor_vec {
                if let Some(range) = cursor.selection_range() {
                    // Delete the selection
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                } else if cursor.position > 0 {
                    // Delete character before cursor
                    let delete_from = cursor.position.saturating_sub(1);
                    let range = delete_from..cursor.position;
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                }
            }
        }

        Action::DeleteForward => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = state.cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            for (cursor_id, cursor) in cursor_vec {
                if let Some(range) = cursor.selection_range() {
                    // Delete the selection
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                } else if cursor.position < state.buffer.len() {
                    // Delete character after cursor
                    let range = cursor.position..(cursor.position + 1);
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                }
            }
        }

        Action::DeleteWordBackward => {
            for (cursor_id, cursor) in state.cursors.iter() {
                if let Some(range) = cursor.selection_range() {
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                } else {
                    let word_start = find_word_start_left(&state.buffer, cursor.position);
                    if word_start < cursor.position {
                        let range = word_start..cursor.position;
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }
                }
            }
        }

        Action::DeleteWordForward => {
            for (cursor_id, cursor) in state.cursors.iter() {
                if let Some(range) = cursor.selection_range() {
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                } else {
                    let word_end = find_word_start_right(&state.buffer, cursor.position);
                    if cursor.position < word_end {
                        let range = cursor.position..word_end;
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: state.buffer.slice(range),
                            cursor_id,
                        });
                    }
                }
            }
        }

        Action::DeleteLine => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let line_start = iter.current_position();

                if let Some((_start, content)) = iter.next() {
                    let line_end = line_start + content.len();
                    let range = line_start..line_end;
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                }
            }
        }

        Action::RemoveSecondaryCursors => {
            // Generate RemoveCursor events for all cursors except the first (original) one
            // Find the first cursor ID (lowest ID = original cursor)
            let first_id = state
                .cursors
                .iter()
                .map(|(id, _)| id)
                .min_by_key(|id| id.0)
                .expect("Should have at least one cursor");

            for (cursor_id, cursor) in state.cursors.iter() {
                if cursor_id != first_id {
                    events.push(Event::RemoveCursor {
                        cursor_id,
                        position: cursor.position,
                        anchor: cursor.anchor,
                    });
                }
            }
        }

        Action::ScrollUp => {
            events.push(Event::Scroll { line_offset: -1 });
        }

        Action::ScrollDown => {
            events.push(Event::Scroll { line_offset: 1 });
        }

        // Actions that don't generate events
        Action::Quit
        | Action::Save
        | Action::SaveAs
        | Action::Open
        | Action::New
        | Action::Close
        | Action::NextBuffer
        | Action::PrevBuffer
        | Action::NavigateBack
        | Action::NavigateForward
        | Action::SplitHorizontal
        | Action::SplitVertical
        | Action::CloseSplit
        | Action::NextSplit
        | Action::PrevSplit
        | Action::Copy
        | Action::Cut
        | Action::Paste
        | Action::AddCursorNextMatch
        | Action::AddCursorAbove
        | Action::AddCursorBelow
        | Action::CommandPalette
        | Action::ShowHelp
        | Action::ToggleLineWrap
        | Action::IncreaseSplitSize
        | Action::DecreaseSplitSize
        | Action::Undo
        | Action::Redo
        | Action::HelpToggle
        | Action::HelpScrollUp
        | Action::HelpScrollDown
        | Action::HelpPageUp
        | Action::HelpPageDown
        | Action::PromptConfirm
        | Action::PromptCancel
        | Action::PromptBackspace
        | Action::PromptDelete
        | Action::PromptMoveLeft
        | Action::PromptMoveRight
        | Action::PromptMoveStart
        | Action::PromptMoveEnd
        | Action::PromptSelectPrev
        | Action::PromptSelectNext
        | Action::PromptPageUp
        | Action::PromptPageDown
        | Action::PromptAcceptSuggestion
        | Action::PromptMoveWordLeft
        | Action::PromptMoveWordRight
        | Action::PromptDeleteWordForward
        | Action::PromptDeleteWordBackward
        | Action::PromptCopy
        | Action::PromptCut
        | Action::PromptPaste
        | Action::PromptMoveLeftSelecting
        | Action::PromptMoveRightSelecting
        | Action::PromptMoveHomeSelecting
        | Action::PromptMoveEndSelecting
        | Action::PromptSelectWordLeft
        | Action::PromptSelectWordRight
        | Action::PromptSelectAll
        | Action::PopupSelectNext
        | Action::PopupSelectPrev
        | Action::PopupPageUp
        | Action::PopupPageDown
        | Action::PopupConfirm
        | Action::PopupCancel
        | Action::ToggleFileExplorer
        | Action::FocusFileExplorer
        | Action::FocusEditor
        | Action::FileExplorerUp
        | Action::FileExplorerDown
        | Action::FileExplorerExpand
        | Action::FileExplorerCollapse
        | Action::FileExplorerOpen
        | Action::FileExplorerRefresh
        | Action::FileExplorerNewFile
        | Action::FileExplorerNewDirectory
        | Action::FileExplorerDelete
        | Action::FileExplorerRename
        | Action::FileExplorerToggleHidden
        | Action::FileExplorerToggleGitignored
        | Action::LspCompletion
        | Action::LspGotoDefinition
        | Action::LspRename
        | Action::RenameConfirm
        | Action::RenameCancel
        | Action::RenameMoveLeft
        | Action::RenameMoveRight
        | Action::RenameMoveHome
        | Action::RenameMoveEnd
        | Action::GitGrep
        | Action::GitFindFile
        | Action::Search
        | Action::FindNext
        | Action::FindPrevious
        | Action::Replace
        | Action::QueryReplace
        | Action::PluginAction(_)
        | Action::None => return None,

        Action::SelectLine => {
            // Select the entire line for each cursor
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use iterator to get line bounds
                let mut iter = state.buffer.line_iterator(cursor.position);
                if let Some((line_start, line_content)) = iter.next() {
                    // Include newline if present
                    let line_end = line_start + line_content.len();

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(line_start),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::ExpandSelection => {
            // Expand selection for each cursor
            for (cursor_id, cursor) in state.cursors.iter() {
                if let Some(anchor) = cursor.anchor {
                    // Already have a selection - expand by one word to the right
                    // First move to the start of the next word, then to its end
                    let next_word_start = find_word_start_right(&state.buffer, cursor.position);
                    let new_end = find_word_end(&state.buffer, next_word_start);
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(anchor),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                } else {
                    // No selection - select from cursor to end of current word
                    let word_start = find_word_start(&state.buffer, cursor.position);
                    let word_end = find_word_end(&state.buffer, cursor.position);

                    // If cursor is on non-word char OR at the end of a word,
                    // select from current position to end of next word
                    let (final_start, final_end) =
                        if word_start == word_end || cursor.position == word_end {
                            // Find the next word (skip non-word characters to find it)
                            let next_start = find_word_start_right(&state.buffer, cursor.position);
                            let next_end = find_word_end(&state.buffer, next_start);
                            // Select FROM cursor position TO the end of next word
                            (cursor.position, next_end)
                        } else {
                            // On a word char - select from cursor to end of current word
                            (cursor.position, word_end)
                        };

                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: final_end,
                        old_anchor: cursor.anchor,
                        new_anchor: Some(final_start),
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }
    }

    Some(events)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event::{CursorId, Event};
    use crate::state::EditorState;

    #[test]
    fn test_backspace_deletes_newline() {
        let mut state = EditorState::new(80, 24);

        // Insert "Hello\nWorld"
        state.apply(&Event::Insert {
            position: 0,
            text: "Hello\nWorld".to_string(),
            cursor_id: CursorId(0),
        });

        assert_eq!(state.buffer.to_string(), "Hello\nWorld");
        assert_eq!(state.cursors.primary().position, 11);

        // Move cursor to position 6 (beginning of "World")
        state.apply(&Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 0,
            new_position: 6,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });

        assert_eq!(state.cursors.primary().position, 6);

        // Press Backspace - should delete the newline at position 5
        let events = action_to_events(&state, Action::DeleteBackward, 4, false).unwrap();
        println!("Generated events: {:?}", events);

        for event in events {
            state.apply(&event);
        }

        assert_eq!(state.buffer.to_string(), "HelloWorld");
        assert_eq!(state.cursors.primary().position, 5);
    }
}
