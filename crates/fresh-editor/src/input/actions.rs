//! Action to event conversion - translates high-level actions into buffer events

use crate::input::keybindings::Action;
use crate::input::line_move::{move_lines, LineMoveDirection};
use crate::model::buffer::{Buffer, LineEnding};
use crate::model::cursor::{Cursors, Position2D, SelectionMode};
use crate::model::event::{CursorId, Event};
use crate::primitives::display_width::{byte_offset_at_visual_column, str_width};
use crate::primitives::word_navigation::{
    find_word_end, find_word_end_right, find_word_start, find_word_start_left,
    find_word_start_right,
};
use crate::state::EditorState;
use std::ops::Range;

/// Direction for block selection movement
#[derive(Debug, Clone, Copy)]
enum BlockDirection {
    Left,
    Right,
    Up,
    Down,
}

/// Convert byte offset to 2D position (line, column)
fn byte_to_2d(buffer: &Buffer, byte_pos: usize) -> Position2D {
    let line = buffer.get_line_number(byte_pos);
    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let column = byte_pos.saturating_sub(line_start);
    Position2D { line, column }
}

/// Convert 2D position to byte offset
fn pos_2d_to_byte(buffer: &Buffer, pos: Position2D) -> usize {
    let line_start = buffer.line_start_offset(pos.line).unwrap_or(0);
    // Get line content to check bounds
    let line_content = buffer.get_line(pos.line).unwrap_or_default();
    // Clamp column to line length (excluding newline)
    let line_len = if line_content.last() == Some(&b'\n') {
        line_content.len().saturating_sub(1)
    } else {
        line_content.len()
    };
    let clamped_col = pos.column.min(line_len);
    line_start + clamped_col
}

/// Calculate the visual column (display width) at the cursor position.
/// Returns (visual_column, byte_column_within_line).
fn calculate_visual_column(
    buffer: &mut Buffer,
    cursor_position: usize,
    estimated_line_length: usize,
) -> (usize, usize) {
    let mut iter = buffer.line_iterator(cursor_position, estimated_line_length);
    let current_line_start = iter.current_position();
    let byte_column = cursor_position.saturating_sub(current_line_start);

    if let Some((_, line_content)) = iter.next_line() {
        if byte_column > 0 && byte_column <= line_content.len() {
            (str_width(&line_content[..byte_column]), byte_column)
        } else {
            (byte_column, byte_column) // Fallback for edge cases
        }
    } else {
        (byte_column, byte_column) // Fallback
    }
}

/// Pattern for matching line ending characters (\r and \n)
const LINE_ENDING_CHARS: &[char] = &['\r', '\n'];

/// Get the length of line content excluding line ending characters (\r and \n).
/// Handles CRLF, LF, and CR line endings.
fn content_len_without_line_ending(content: &str) -> usize {
    content.trim_end_matches(LINE_ENDING_CHARS).len()
}

/// Adjust position after moving left in CRLF mode.
/// If we land on \n that's preceded by \r, skip back to the \r.
/// This ensures the cursor never sits between \r and \n.
fn adjust_position_for_crlf_left(buffer: &Buffer, pos: usize) -> usize {
    if buffer.line_ending() != LineEnding::CRLF || pos == 0 {
        return pos;
    }

    let byte_at_pos = buffer.slice_bytes(pos..pos + 1);
    if byte_at_pos.first() == Some(&b'\n') {
        let prev_byte = buffer.slice_bytes(pos.saturating_sub(1)..pos);
        if prev_byte.first() == Some(&b'\r') {
            return pos - 1; // Skip back to \r
        }
    }
    pos
}

/// Calculate next position when moving right, treating CRLF as a single unit.
/// If cursor is on \r followed by \n, skip over both.
/// Uses grapheme cluster boundaries for proper handling of combining characters.
fn next_position_for_crlf(buffer: &Buffer, pos: usize, max_pos: usize) -> usize {
    if buffer.line_ending() == LineEnding::CRLF {
        let cur_byte = buffer.slice_bytes(pos..pos + 1);
        let next_byte = buffer.slice_bytes(pos + 1..pos + 2);
        if cur_byte.first() == Some(&b'\r') && next_byte.first() == Some(&b'\n') {
            return (pos + 2).min(max_pos); // Skip both \r and \n
        }
    }
    buffer.next_grapheme_boundary(pos).min(max_pos)
}

/// Convert deletion ranges to Delete events
///
/// This is a common pattern used across many deletion actions.
/// It reads the text from each range and creates Delete events.
fn apply_deletions(
    state: &mut EditorState,
    deletions: Vec<(CursorId, Range<usize>)>,
    events: &mut Vec<Event>,
) {
    for (cursor_id, range) in deletions {
        let deleted_text = state.get_text_range(range.start, range.end);
        events.push(Event::Delete {
            range,
            deleted_text,
            cursor_id,
        });
    }
}

/// Collect all line start positions in a given byte range
///
/// This is used for indent/dedent operations to find all lines that need
/// to be indented or dedented within a selection.
fn collect_line_starts(
    buffer: &mut Buffer,
    start_pos: usize,
    end_pos: usize,
    estimated_line_length: usize,
) -> Vec<usize> {
    let buffer_len = buffer.len();
    let mut line_starts = Vec::new();
    let mut iter = buffer.line_iterator(start_pos, estimated_line_length);

    // Collect all line starts by iterating through lines using a single iterator
    // The iterator naturally handles the trailing empty line case without infinite loops
    while let Some((line_start, _)) = iter.next_line() {
        if line_start > end_pos || line_start > buffer_len {
            break;
        }
        line_starts.push(line_start);
    }

    line_starts
}

/// Calculate how much leading whitespace to remove from a line for dedent
///
/// Returns (chars_to_remove, deleted_text) where chars_to_remove is the number
/// of characters to delete, and deleted_text is the string being deleted.
fn calculate_leading_whitespace_removal(
    buffer: &Buffer,
    line_start: usize,
    tab_size: usize,
) -> (usize, String) {
    let buffer_len = buffer.len();
    let line_bytes = buffer.slice_bytes(line_start..buffer_len.min(line_start + tab_size + 1));

    if !line_bytes.is_empty() && line_bytes[0] == b'\t' {
        (1, "\t".to_string())
    } else {
        let spaces_to_remove = line_bytes
            .iter()
            .take(tab_size)
            .take_while(|&&b| b == b' ')
            .count();
        (spaces_to_remove, " ".repeat(spaces_to_remove))
    }
}

/// Add a MoveCursor event to restore cursor position after indent/dedent
fn add_move_cursor_event(
    events: &mut Vec<Event>,
    cursor_id: CursorId,
    old_position: usize,
    new_position: usize,
    old_anchor: Option<usize>,
    new_anchor: Option<usize>,
    old_sticky_column: usize,
) {
    events.push(Event::MoveCursor {
        cursor_id,
        old_position,
        new_position,
        old_anchor,
        new_anchor,
        old_sticky_column,
        new_sticky_column: 0,
    });
}

/// Handle block selection movement
fn block_select_action(
    state: &mut EditorState,
    cursors: &mut Cursors,
    events: &mut Vec<Event>,
    direction: BlockDirection,
) {
    // Get line count for bounds checking
    let total_lines = {
        let len = state.buffer.len();
        if len == 0 {
            1
        } else {
            state.buffer.get_line_number(len.saturating_sub(1)) + 1
        }
    };

    for (cursor_id, cursor) in cursors.iter() {
        let current_2d = byte_to_2d(&state.buffer, cursor.position);

        // If not in block mode, start block selection
        let block_anchor =
            if cursor.selection_mode != SelectionMode::Block || cursor.block_anchor.is_none() {
                current_2d
            } else {
                cursor.block_anchor.unwrap()
            };

        // Calculate new 2D position based on direction
        let new_2d = match direction {
            BlockDirection::Left => Position2D {
                line: current_2d.line,
                column: current_2d.column.saturating_sub(1),
            },
            BlockDirection::Right => {
                // Get current line length to bound the column
                let line_content = state.buffer.get_line(current_2d.line).unwrap_or_default();
                let line_len = if line_content.last() == Some(&b'\n') {
                    line_content.len().saturating_sub(1)
                } else {
                    line_content.len()
                };
                Position2D {
                    line: current_2d.line,
                    column: (current_2d.column + 1).min(line_len),
                }
            }
            BlockDirection::Up => {
                if current_2d.line > 0 {
                    Position2D {
                        line: current_2d.line - 1,
                        column: current_2d.column,
                    }
                } else {
                    current_2d
                }
            }
            BlockDirection::Down => {
                if current_2d.line + 1 < total_lines {
                    Position2D {
                        line: current_2d.line + 1,
                        column: current_2d.column,
                    }
                } else {
                    current_2d
                }
            }
        };

        // Convert new 2D position back to byte offset
        let new_byte_pos = pos_2d_to_byte(&state.buffer, new_2d);

        // Store the byte anchor for the event system (for undo/redo compatibility)
        let byte_anchor = pos_2d_to_byte(&state.buffer, block_anchor);

        events.push(Event::MoveCursor {
            cursor_id,
            old_position: cursor.position,
            new_position: new_byte_pos,
            old_anchor: cursor.anchor,
            new_anchor: Some(byte_anchor),
            old_sticky_column: cursor.sticky_column,
            new_sticky_column: new_2d.column,
        });

        // Note: We need to set block selection mode after the event is processed
        // This will be done in a separate step
    }

    // Update selection mode for all cursors to Block mode
    // We need to do this directly since Event::MoveCursor doesn't support selection mode changes
    // Note: We update the cursors here to set block_anchor BEFORE the events are applied
    // This way the events will move the cursor, but the anchor remains fixed
    let buffer_ref = &state.buffer;
    cursors.map(|cursor| {
        if cursor.selection_mode != SelectionMode::Block || cursor.block_anchor.is_none() {
            let current_2d = byte_to_2d(buffer_ref, cursor.position);
            cursor.start_block_selection(current_2d.line, current_2d.column);
        }
    });
}

/// Clear block selection when performing normal operations
/// This should be called when the user performs a non-block action
pub fn clear_block_selection_if_active(cursors: &mut Cursors) {
    cursors.map(|cursor| {
        if cursor.selection_mode == SelectionMode::Block {
            cursor.clear_block_selection();
        }
    });
}

/// Convert block selection to multiple cursors with normal selections.
/// Each cursor will have a selection covering that line's portion of the block.
/// This should be called before action processing so normal multi-cursor logic applies.
/// Returns events to add the new cursors (if any).
fn convert_block_selection_to_cursors(
    state: &mut EditorState,
    cursors: &mut Cursors,
) -> Vec<Event> {
    let mut events = Vec::new();

    // Check if any cursor has a block selection
    let block_info: Option<(CursorId, Position2D, Position2D)> =
        cursors.iter().find_map(|(cursor_id, cursor)| {
            if cursor.has_block_selection() {
                let block_anchor = cursor.block_anchor?;
                let cursor_2d = byte_to_2d(&state.buffer, cursor.position);
                Some((cursor_id, block_anchor, cursor_2d))
            } else {
                None
            }
        });

    let Some((primary_cursor_id, block_anchor, cursor_2d)) = block_info else {
        return events;
    };

    // Calculate block rectangle bounds
    let min_line = block_anchor.line.min(cursor_2d.line);
    let max_line = block_anchor.line.max(cursor_2d.line);
    let min_col = block_anchor.column.min(cursor_2d.column);
    let max_col = block_anchor.column.max(cursor_2d.column);

    // Calculate cursor positions for each line
    let mut cursor_positions: Vec<(usize, usize)> = Vec::new(); // (position, anchor)

    for line in min_line..=max_line {
        let line_start = state.buffer.line_start_offset(line).unwrap_or(0);
        let line_content = state.buffer.get_line(line).unwrap_or_default();

        // Calculate line length excluding newline
        let line_len = if line_content.last() == Some(&b'\n') {
            line_content.len().saturating_sub(1)
        } else {
            line_content.len()
        };

        // Clamp columns to actual line length
        let actual_min_col = min_col.min(line_len);
        let actual_max_col = max_col.min(line_len);

        let anchor = line_start + actual_min_col;
        let position = line_start + actual_max_col;

        cursor_positions.push((position, anchor));
    }

    // Update the primary cursor to have a normal selection on the first line
    if let Some((position, anchor)) = cursor_positions.first().copied() {
        if let Some(cursor) = cursors.get_mut(primary_cursor_id) {
            cursor.position = position;
            cursor.anchor = if position != anchor {
                Some(anchor)
            } else {
                None
            };
            cursor.clear_block_selection();
        }
    }

    // Add new cursors for remaining lines
    let mut next_cursor_id = cursors.count();
    for (position, anchor) in cursor_positions.into_iter().skip(1) {
        let cursor_id = CursorId(next_cursor_id);
        next_cursor_id += 1;

        events.push(Event::AddCursor {
            cursor_id,
            position,
            anchor: if position != anchor {
                Some(anchor)
            } else {
                None
            },
        });
    }

    events
}

/// Get the matching close character for auto-pairing.
pub fn get_auto_close_char(ch: char, auto_indent: bool, language: &str) -> Option<char> {
    if !auto_indent {
        return None;
    }
    // Disable auto-closing quotes in plain text files
    if language == "text" && matches!(ch, '"' | '\'' | '`') {
        return None;
    }
    match ch {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        '"' => Some('"'),
        '\'' => Some('\''),
        '`' => Some('`'),
        _ => None,
    }
}

/// Calculate the correct indent for a closing delimiter using tree-sitter.
fn calculate_closing_delimiter_indent(
    state: &mut EditorState,
    insert_position: usize,
    ch: char,
    tab_size: usize,
) -> usize {
    if let Some(language) = state.highlighter.language() {
        state
            .indent_calculator
            .borrow_mut()
            .calculate_dedent_for_delimiter(&state.buffer, insert_position, ch, language, tab_size)
            .unwrap_or(0)
    } else {
        0
    }
}

/// Convert a visual indent width to actual indent characters.
/// When `use_tabs` is true, uses tab characters; otherwise uses spaces.
/// The `indent_width` is the visual width in columns, and `tab_size` is
/// how many columns a tab character represents.
fn indent_to_string(indent_width: usize, use_tabs: bool, tab_size: usize) -> String {
    if use_tabs && tab_size > 0 {
        let num_tabs = indent_width / tab_size;
        let remaining_spaces = indent_width % tab_size;
        let mut result = "\t".repeat(num_tabs);
        if remaining_spaces > 0 {
            result.push_str(&" ".repeat(remaining_spaces));
        }
        result
    } else {
        " ".repeat(indent_width)
    }
}

/// Handle skip-over with dedent: when typing a closing delimiter that exists after cursor,
/// and the line has incorrect indentation, fix the indent and skip over.
/// Returns true if handled (caller should continue to next cursor).
fn handle_skip_over_with_dedent(
    state: &mut EditorState,
    events: &mut Vec<Event>,
    cursor_id: CursorId,
    ch: char,
    insert_position: usize,
    line_start: usize,
    tab_size: usize,
) -> bool {
    let correct_indent = calculate_closing_delimiter_indent(state, insert_position, ch, tab_size);
    let current_indent = insert_position - line_start;

    if current_indent != correct_indent {
        // Delete incorrect spacing
        let deleted_text = state.get_text_range(line_start, insert_position);
        events.push(Event::Delete {
            range: line_start..insert_position,
            deleted_text,
            cursor_id,
        });

        // Insert correct spacing
        if correct_indent > 0 {
            events.push(Event::Insert {
                position: line_start,
                text: " ".repeat(correct_indent),
                cursor_id,
            });
        }

        // Move cursor to after the closing delimiter
        events.push(Event::MoveCursor {
            cursor_id,
            old_position: line_start + correct_indent,
            new_position: line_start + correct_indent + 1,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: 0,
            new_sticky_column: 0,
        });
        return true;
    }
    false
}

/// Handle simple skip-over: move cursor past existing closing bracket/quote.
fn handle_skip_over(events: &mut Vec<Event>, cursor_id: CursorId, insert_position: usize) {
    events.push(Event::MoveCursor {
        cursor_id,
        old_position: insert_position,
        new_position: insert_position + 1,
        old_anchor: None,
        new_anchor: None,
        old_sticky_column: 0,
        new_sticky_column: 0,
    });
}

/// Handle auto-dedent: when typing a closing delimiter on a line with only spaces,
/// fix the indentation and insert the delimiter.
fn handle_auto_dedent(
    state: &mut EditorState,
    events: &mut Vec<Event>,
    cursor_id: CursorId,
    ch: char,
    insert_position: usize,
    line_start: usize,
    tab_size: usize,
) {
    let correct_indent = calculate_closing_delimiter_indent(state, insert_position, ch, tab_size);

    // Delete the incorrect spacing
    let spaces_to_delete = insert_position - line_start;
    if spaces_to_delete > 0 {
        let deleted_text = state.get_text_range(line_start, insert_position);
        events.push(Event::Delete {
            range: line_start..insert_position,
            deleted_text,
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
}

/// Check if auto-close should happen based on character after cursor.
fn should_auto_close(char_after: Option<u8>) -> bool {
    let is_alphanumeric_after = char_after
        .map(|b| b.is_ascii_alphanumeric() || b == b'_')
        .unwrap_or(false);
    !is_alphanumeric_after
}

/// Handle auto-close: insert both opening and closing bracket/quote.
fn handle_auto_close(
    events: &mut Vec<Event>,
    cursor_id: CursorId,
    ch: char,
    close_char: char,
    insert_position: usize,
) {
    // Insert opening + closing character
    let text = format!("{}{}", ch, close_char);
    events.push(Event::Insert {
        position: insert_position,
        text,
        cursor_id,
    });
    // Move cursor between the brackets
    events.push(Event::MoveCursor {
        cursor_id,
        old_position: insert_position + 2,
        new_position: insert_position + 1,
        old_anchor: None,
        new_anchor: None,
        old_sticky_column: 0,
        new_sticky_column: 0,
    });
}

/// Cursor context data collected before processing insertions.
struct InsertCursorData {
    cursor_id: CursorId,
    selection: Option<Range<usize>>,
    insert_position: usize,
    line_start: usize,
    only_spaces: bool,
    char_after: Option<u8>,
    deleted_text: Option<String>,
}

/// Collect cursor data needed for character insertion.
fn collect_insert_cursor_data(state: &mut EditorState, cursors: &Cursors) -> Vec<InsertCursorData> {
    // Collect cursors and sort by the effective insert position (reverse order)
    let mut cursor_vec: Vec<_> = cursors.iter().collect();
    cursor_vec.sort_by_key(|(_, c)| {
        let insert_pos = c.selection_range().map(|r| r.start).unwrap_or(c.position);
        std::cmp::Reverse(insert_pos)
    });

    // Collect cursor IDs and positions
    let cursor_info: Vec<_> = cursor_vec
        .iter()
        .map(|(cursor_id, cursor)| {
            let selection = cursor.selection_range();
            let insert_position = selection
                .as_ref()
                .map(|r| r.start)
                .unwrap_or(cursor.position);
            (*cursor_id, selection, insert_position)
        })
        .collect();

    drop(cursor_vec);

    // Collect all cursor data with buffer access
    cursor_info
        .into_iter()
        .map(|(cursor_id, selection, insert_position)| {
            // Calculate line start for auto-dedent
            let mut line_start = insert_position;
            while line_start > 0 {
                let prev = line_start - 1;
                if state.buffer.slice_bytes(prev..prev + 1).first() == Some(&b'\n') {
                    break;
                }
                line_start = prev;
            }

            let line_before_cursor = state.buffer.slice_bytes(line_start..insert_position);
            let only_spaces = line_before_cursor.iter().all(|&b| b == b' ' || b == b'\t');

            let check_pos = selection.as_ref().map(|r| r.end).unwrap_or(insert_position);
            let char_after = if check_pos < state.buffer.len() {
                state
                    .buffer
                    .slice_bytes(check_pos..check_pos + 1)
                    .first()
                    .copied()
            } else {
                None
            };

            let deleted_text = selection
                .as_ref()
                .map(|r| state.get_text_range(r.start, r.end));

            InsertCursorData {
                cursor_id,
                selection,
                insert_position,
                line_start,
                only_spaces,
                char_after,
                deleted_text,
            }
        })
        .collect()
}

/// Handle InsertChar action - insert character at each cursor position.
fn insert_char_events(
    state: &mut EditorState,
    cursors: &Cursors,
    events: &mut Vec<Event>,
    ch: char,
    tab_size: usize,
    auto_indent: bool,
) {
    let is_closing_delimiter = matches!(ch, '}' | ')' | ']');
    let auto_close_char = get_auto_close_char(ch, auto_indent, &state.language);
    let cursor_data = collect_insert_cursor_data(state, cursors);

    for data in cursor_data {
        // Delete selection if present
        if let (Some(range), Some(text)) = (data.selection, data.deleted_text) {
            events.push(Event::Delete {
                range,
                deleted_text: text,
                cursor_id: data.cursor_id,
            });
        }

        // Try skip-over logic for closing brackets/quotes
        if auto_indent && matches!(ch, ')' | ']' | '}' | '"' | '\'' | '`') {
            if let Some(next_byte) = data.char_after {
                if next_byte == ch as u8 {
                    // Try skip-over with dedent for closing delimiters
                    if is_closing_delimiter
                        && data.only_spaces
                        && data.insert_position > data.line_start
                        && handle_skip_over_with_dedent(
                            state,
                            events,
                            data.cursor_id,
                            ch,
                            data.insert_position,
                            data.line_start,
                            tab_size,
                        )
                    {
                        continue;
                    }
                    // Simple skip-over
                    handle_skip_over(events, data.cursor_id, data.insert_position);
                    continue;
                }
            }
        }

        // Try auto-dedent for closing delimiters
        if is_closing_delimiter
            && auto_indent
            && data.only_spaces
            && data.insert_position > data.line_start
        {
            handle_auto_dedent(
                state,
                events,
                data.cursor_id,
                ch,
                data.insert_position,
                data.line_start,
                tab_size,
            );
            continue;
        }

        // Try auto-close
        if let Some(close_char) = auto_close_char {
            if should_auto_close(data.char_after) {
                handle_auto_close(events, data.cursor_id, ch, close_char, data.insert_position);
                continue;
            }
        }

        // Normal character insertion
        events.push(Event::Insert {
            position: data.insert_position,
            text: ch.to_string(),
            cursor_id: data.cursor_id,
        });
    }
}

/// Calculate the maximum valid cursor position in the buffer.
/// This is the end of the last line (excluding trailing newline).
/// For empty buffers, returns 0.
fn max_cursor_position(buffer: &Buffer) -> usize {
    // The maximum cursor position is simply the end of the buffer
    // No need to use line iterator or calculate line positions
    buffer.len()
}

/// Transform selected text (or current word if no selection) using the given transform function.
/// Processes cursors in reverse order to avoid position shifts.
fn transform_case<F>(
    state: &mut EditorState,
    cursors: &mut Cursors,
    events: &mut Vec<Event>,
    transform: F,
) where
    F: Fn(&str) -> String,
{
    let mut selections: Vec<_> = cursors
        .iter()
        .map(|(cursor_id, cursor)| {
            if let Some(range) = cursor.selection_range() {
                (cursor_id, range.start, range.end)
            } else {
                // No selection - use current word
                let word_start = find_word_start(&state.buffer, cursor.position);
                let word_end = find_word_end(&state.buffer, word_start);
                (cursor_id, word_start, word_end)
            }
        })
        .filter(|(_, start, end)| start < end)
        .collect();
    selections.sort_by_key(|(_, start, _)| std::cmp::Reverse(*start));

    for (cursor_id, start, end) in selections {
        let text = state.get_text_range(start, end);
        let transformed = transform(&text);
        if transformed != text {
            events.push(Event::Delete {
                range: start..end,
                deleted_text: text,
                cursor_id,
            });
            events.push(Event::Insert {
                position: start,
                text: transformed,
                cursor_id,
            });
        }
    }
}

/// Convert an action into a sequence of events that can be applied to the editor state
///
/// # Parameters
/// * `state` - The current editor state
/// * `action` - The action to convert
/// * `tab_size` - Number of spaces per tab
/// * `auto_indent` - Whether auto-indent is enabled
/// * `estimated_line_length` - Estimated bytes per line for large files
/// * `viewport_height` - Height of the viewport in lines (for PageUp/PageDown)
///
/// # Returns
/// * `Some(Vec<Event>)` - Events to apply for this action
/// * `None` - If the action doesn't generate events (like Quit, Save, etc.)
pub fn action_to_events(
    state: &mut EditorState,
    cursors: &mut Cursors,
    action: Action,
    tab_size: usize,
    auto_indent: bool,
    estimated_line_length: usize,
    viewport_height: u16,
) -> Option<Vec<Event>> {
    // For virtual buffers with hidden cursors, ignore movement and editing actions
    if !state.show_cursors && action.is_movement_or_editing() {
        return None;
    }

    let mut events = Vec::new();

    // Convert block selection to multi-cursor before processing editing actions
    // This allows normal multi-cursor logic to handle typing, deletion, etc.
    if action.is_editing() {
        let cursor_events = convert_block_selection_to_cursors(state, cursors);
        for event in &cursor_events {
            state.apply(cursors, event);
        }
        events.extend(cursor_events);
    }

    match action {
        // Character input - insert at each cursor
        Action::InsertChar(ch) => {
            insert_char_events(state, cursors, &mut events, ch, tab_size, auto_indent);
        }

        Action::InsertNewline => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Collect deletions and positions for indentation
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    cursor
                        .selection_range()
                        .map(|range| (*cursor_id, range.clone(), range.start))
                })
                .collect();

            let indent_positions: Vec<_> = cursor_vec
                .iter()
                .map(|(cursor_id, cursor)| {
                    let indent_position = cursor
                        .selection_range()
                        .map(|r| r.start)
                        .unwrap_or(cursor.position);
                    (*cursor_id, indent_position)
                })
                .collect();

            // Get text for deletions and build delete events
            for (cursor_id, range, _start) in deletions {
                let deleted_text = state.get_text_range(range.start, range.end);
                events.push(Event::Delete {
                    range,
                    deleted_text,
                    cursor_id,
                });
            }

            // Now process insertions
            let line_ending = state.buffer.line_ending().as_str();
            for (cursor_id, indent_position) in indent_positions {
                // Calculate indent for new line
                let mut text = line_ending.to_string();

                // Check for bracket expansion: cursor between matching brackets like {|}
                // Only applies to braces, brackets, and parentheses (not quotes)
                let bracket_expansion = if auto_indent && indent_position > 0 {
                    let char_before = state
                        .buffer
                        .slice_bytes(indent_position.saturating_sub(1)..indent_position)
                        .first()
                        .copied();
                    let char_after = if indent_position < state.buffer.len() {
                        state
                            .buffer
                            .slice_bytes(indent_position..indent_position + 1)
                            .first()
                            .copied()
                    } else {
                        None
                    };

                    // Check if we're between matching brackets (not quotes)
                    matches!(
                        (char_before, char_after),
                        (Some(b'('), Some(b')'))
                            | (Some(b'['), Some(b']'))
                            | (Some(b'{'), Some(b'}'))
                    )
                } else {
                    false
                };

                // Track cursor line position for bracket expansion
                // After bracket expansion, cursor should be at end of cursor line, not at end of closing bracket line
                let mut cursor_line_end_position: Option<usize> = None;

                if auto_indent {
                    let use_tabs = state.buffer_settings.use_tabs;
                    if let Some(language) = state.highlighter.language() {
                        // Use tree-sitter-based indent when we have a highlighter
                        if let Some(indent_width) = state
                            .indent_calculator
                            .borrow_mut()
                            .calculate_indent(&state.buffer, indent_position, language, tab_size)
                        {
                            let indent_str = indent_to_string(indent_width, use_tabs, tab_size);
                            text.push_str(&indent_str);

                            // For bracket expansion, add another newline with dedented closing bracket
                            if bracket_expansion {
                                // Record where cursor should end up (end of cursor line)
                                cursor_line_end_position =
                                    Some(indent_position + line_ending.len() + indent_str.len());

                                // Calculate the dedent for the closing bracket line
                                // It should match the indent of the line containing the opening bracket
                                let opening_bracket_indent =
                                    crate::primitives::indent::IndentCalculator::get_line_indent_at_position(
                                        &state.buffer,
                                        indent_position.saturating_sub(1),
                                        tab_size,
                                    );
                                text.push_str(line_ending);
                                text.push_str(&indent_to_string(
                                    opening_bracket_indent,
                                    use_tabs,
                                    tab_size,
                                ));
                            }
                        }
                    } else {
                        // Fallback for files without syntax highlighting (e.g., .txt)
                        let indent_width =
                            crate::primitives::indent::IndentCalculator::calculate_indent_no_language(
                                &state.buffer,
                                indent_position,
                                tab_size,
                            );
                        let indent_str = indent_to_string(indent_width, use_tabs, tab_size);
                        text.push_str(&indent_str);

                        // For bracket expansion in non-language files
                        if bracket_expansion {
                            // Record where cursor should end up (end of cursor line)
                            cursor_line_end_position =
                                Some(indent_position + line_ending.len() + indent_str.len());

                            let opening_bracket_indent =
                                crate::primitives::indent::IndentCalculator::get_line_indent_at_position(
                                    &state.buffer,
                                    indent_position.saturating_sub(1),
                                    tab_size,
                                );
                            text.push_str(line_ending);
                            text.push_str(&indent_to_string(
                                opening_bracket_indent,
                                use_tabs,
                                tab_size,
                            ));
                        }
                    }
                }

                // Calculate where cursor will end up after insert
                let cursor_after_insert = indent_position + text.len();

                events.push(Event::Insert {
                    position: indent_position,
                    text,
                    cursor_id,
                });

                // For bracket expansion, move cursor back to the cursor line
                // (not the closing bracket line where it ends up after insert)
                if let Some(cursor_line_end) = cursor_line_end_position {
                    // Get current cursor state to build the MoveCursor event
                    if let Some(cursor) = cursors.get(cursor_id) {
                        events.push(Event::MoveCursor {
                            cursor_id,
                            old_position: cursor_after_insert,
                            new_position: cursor_line_end,
                            old_anchor: None, // No selection after bracket expansion
                            new_anchor: None,
                            old_sticky_column: cursor.sticky_column,
                            new_sticky_column: cursor_line_end, // Reset sticky column
                        });
                    }
                }
            }
        }

        Action::DedentSelection => {
            // Dedent selected lines and preserve selections
            // Collect all line starts from all cursors first to avoid position shifts
            use std::collections::BTreeMap;
            let mut all_line_deletions: BTreeMap<usize, (usize, String)> = BTreeMap::new();
            let mut cursor_info = Vec::new();

            for (cursor_id, cursor) in cursors.iter() {
                let has_selection = cursor.selection_range().is_some();

                let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
                    (range.start, range.end)
                } else {
                    // No selection - dedent current line
                    let iter = state
                        .buffer
                        .line_iterator(cursor.position, estimated_line_length);
                    let line_start = iter.current_position();
                    (line_start, cursor.position)
                };

                // Find all line starts in the range using helper function
                let line_starts = collect_line_starts(
                    &mut state.buffer,
                    start_pos,
                    end_pos,
                    estimated_line_length,
                );

                // For each line start, calculate what to delete
                for &line_start in &line_starts {
                    if let std::collections::btree_map::Entry::Vacant(e) =
                        all_line_deletions.entry(line_start)
                    {
                        let (chars_to_remove, deleted_text) = calculate_leading_whitespace_removal(
                            &state.buffer,
                            line_start,
                            tab_size,
                        );

                        if chars_to_remove > 0 {
                            e.insert((chars_to_remove, deleted_text));
                        }
                    }
                }

                // Store cursor info for later restoration
                cursor_info.push((
                    cursor_id,
                    cursor.position,
                    cursor.anchor,
                    cursor.sticky_column,
                    has_selection,
                    start_pos,
                    end_pos,
                ));
            }

            // Create delete events in reverse order to avoid position shifts
            let first_cursor_id = cursors.iter().next().unwrap().0;
            for (&line_start, (chars_to_remove, deleted_text)) in all_line_deletions.iter().rev() {
                events.push(Event::Delete {
                    range: line_start..line_start + chars_to_remove,
                    deleted_text: deleted_text.clone(),
                    cursor_id: first_cursor_id,
                });
            }

            // Calculate new cursor/selection positions and add MoveCursor events
            for (
                cursor_id,
                old_position,
                old_anchor,
                old_sticky_column,
                has_selection,
                start_pos,
                end_pos,
            ) in cursor_info
            {
                // Calculate how many chars were removed before start_pos and end_pos
                let mut removed_before_start = 0;
                let mut removed_before_end = 0;
                let mut removed_before_position = 0;

                for (&line_start, &(chars_to_remove, _)) in &all_line_deletions {
                    if line_start < start_pos {
                        removed_before_start += chars_to_remove;
                    }
                    if line_start <= end_pos {
                        removed_before_end += chars_to_remove;
                    }
                    if line_start < old_position {
                        removed_before_position += chars_to_remove;
                    }
                }

                if has_selection {
                    // Had selection - restore it with adjusted positions
                    let new_anchor = start_pos.saturating_sub(removed_before_start);
                    let new_position = end_pos.saturating_sub(removed_before_end);
                    add_move_cursor_event(
                        &mut events,
                        cursor_id,
                        old_position,
                        new_position,
                        old_anchor,
                        Some(new_anchor),
                        old_sticky_column,
                    );
                } else {
                    // No selection - just move cursor back by amount removed before it
                    let new_position = old_position.saturating_sub(removed_before_position);
                    add_move_cursor_event(
                        &mut events,
                        cursor_id,
                        old_position,
                        new_position,
                        old_anchor,
                        None,
                        old_sticky_column,
                    );
                }
            }
        }

        Action::InsertTab => {
            // Insert a tab character or spaces based on language config
            let tab_str = if state.buffer_settings.use_tabs {
                "\t".to_string()
            } else {
                " ".repeat(tab_size)
            };

            // Check if any cursor has a selection
            let has_selection = cursors
                .iter()
                .any(|(_, cursor)| cursor.selection_range().is_some());

            if has_selection {
                // Indent selected lines and preserve selections
                // Collect all line starts from all cursors first to avoid position shifts
                use std::collections::BTreeSet;
                let mut all_line_starts = BTreeSet::new();
                let mut cursor_info = Vec::new();

                for (cursor_id, cursor) in cursors.iter() {
                    if let Some(range) = cursor.selection_range() {
                        let (start_pos, end_pos) = (range.start, range.end);

                        // Find all line starts in the range using helper function
                        let line_starts = collect_line_starts(
                            &mut state.buffer,
                            start_pos,
                            end_pos,
                            estimated_line_length,
                        );

                        // Add to global set (automatically deduplicates and sorts)
                        all_line_starts.extend(line_starts.iter());

                        // Store cursor info for later restoration
                        cursor_info.push((
                            cursor_id,
                            cursor.position,
                            cursor.anchor,
                            cursor.sticky_column,
                            start_pos,
                            end_pos,
                        ));
                    }
                }

                // Create insert events for all line starts in reverse order
                // This ensures later positions aren't shifted by earlier insertions
                let first_cursor_id = cursors.iter().next().unwrap().0;
                for &line_start in all_line_starts.iter().rev() {
                    events.push(Event::Insert {
                        position: line_start,
                        text: tab_str.clone(),
                        cursor_id: first_cursor_id,
                    });
                }

                // Calculate new selection positions and add MoveCursor events
                let indent_len = tab_str.len();
                for (cursor_id, old_position, old_anchor, old_sticky_column, start_pos, end_pos) in
                    cursor_info
                {
                    // Count how many indents were inserted at or before each position
                    // Use <= for anchor because we insert at line starts, and positions >= line_start shift
                    // Use < for position to avoid double-counting the indent at position itself
                    let indents_at_or_before_anchor = all_line_starts
                        .iter()
                        .filter(|&&pos| pos <= start_pos)
                        .count();
                    let indents_before_position =
                        all_line_starts.iter().filter(|&&pos| pos < end_pos).count();

                    let new_anchor = start_pos + (indents_at_or_before_anchor * indent_len);
                    let new_position = end_pos + (indents_before_position * indent_len);

                    add_move_cursor_event(
                        &mut events,
                        cursor_id,
                        old_position,
                        new_position,
                        old_anchor,
                        Some(new_anchor),
                        old_sticky_column,
                    );
                }
            } else {
                // No selection - insert tab character at cursor position
                // Sort cursors by position (reverse order) to avoid position shifts
                let mut cursor_vec: Vec<_> = cursors.iter().collect();
                cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

                // Insert tabs
                for (cursor_id, cursor) in cursor_vec {
                    events.push(Event::Insert {
                        position: cursor.position,
                        text: tab_str.clone(),
                        cursor_id,
                    });
                }
            }
        }

        // Basic movement - move each cursor
        // Uses grapheme cluster boundaries for proper handling of combining characters
        Action::MoveLeft => {
            for (cursor_id, cursor) in cursors.iter() {
                let new_pos = state.buffer.prev_grapheme_boundary(cursor.position);
                let new_pos = adjust_position_for_crlf_left(&state.buffer, new_pos);

                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column on horizontal movement
                });
            }
        }

        Action::MoveRight => {
            for (cursor_id, cursor) in cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                let new_pos = next_position_for_crlf(&state.buffer, cursor.position, max_pos);

                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column on horizontal movement
                });
            }
        }

        Action::MoveUp => {
            for (cursor_id, cursor) in cursors.iter() {
                // Calculate visual column first (iterator is dropped after this call)
                let (current_visual_column, _) = calculate_visual_column(
                    &mut state.buffer,
                    cursor.position,
                    estimated_line_length,
                );

                // Use sticky_column if set (now stores visual column), otherwise use current visual column
                let goal_visual_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_visual_column
                };

                // Now create iterator for navigation
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);

                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    // Calculate byte offset from visual column, ensuring valid character boundary
                    let prev_line_text = prev_line_content.trim_end_matches('\n');
                    let byte_offset =
                        byte_offset_at_visual_column(prev_line_text, goal_visual_column);
                    let new_pos = prev_line_start + byte_offset;

                    // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                    let new_anchor = if cursor.deselect_on_move {
                        None
                    } else {
                        cursor.anchor
                    };
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_visual_column, // Preserve the goal visual column
                    });
                }
            }
        }

        Action::MoveDown => {
            for (cursor_id, cursor) in cursors.iter() {
                // Calculate visual column first (iterator is dropped after this call)
                let (current_visual_column, _) = calculate_visual_column(
                    &mut state.buffer,
                    cursor.position,
                    estimated_line_length,
                );

                // Use sticky_column if set (now stores visual column), otherwise use current visual column
                let goal_visual_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_visual_column
                };

                // Now create iterator for navigation
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);

                // Consume current line
                iter.next_line();

                if let Some((next_line_start, next_line_content)) = iter.next_line() {
                    // Calculate byte offset from visual column, ensuring valid character boundary
                    let next_line_text = next_line_content.trim_end_matches('\n');
                    let byte_offset =
                        byte_offset_at_visual_column(next_line_text, goal_visual_column);
                    let new_pos = next_line_start + byte_offset;

                    // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                    let new_anchor = if cursor.deselect_on_move {
                        None
                    } else {
                        cursor.anchor
                    };
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: new_pos,
                        old_anchor: cursor.anchor,
                        new_anchor,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: goal_visual_column, // Preserve the goal visual column
                    });
                }
            }
        }

        Action::MoveLineStart => {
            for (cursor_id, cursor) in cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                if let Some((line_start, _)) = iter.next_line() {
                    // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                    let new_anchor = if cursor.deselect_on_move {
                        None
                    } else {
                        cursor.anchor
                    };
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_start,
                        old_anchor: cursor.anchor,
                        new_anchor,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::MoveLineEnd => {
            for (cursor_id, cursor) in cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                if let Some((line_start, line_content)) = iter.next_line() {
                    // In both LF and CRLF mode, cursor lands at the first byte of line ending
                    // For LF: cursor on \n. For CRLF: cursor on \r (before both \r\n)
                    let line_end = line_start + content_len_without_line_ending(&line_content);

                    // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                    let new_anchor = if cursor.deselect_on_move {
                        None
                    } else {
                        cursor.anchor
                    };
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position: cursor.position,
                        new_position: line_end,
                        old_anchor: cursor.anchor,
                        new_anchor,
                        old_sticky_column: cursor.sticky_column,
                        new_sticky_column: 0, // Reset sticky column
                    });
                }
            }
        }

        Action::MoveWordLeft => {
            for (cursor_id, cursor) in cursors.iter() {
                let new_pos = find_word_start_left(&state.buffer, cursor.position);
                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveWordRight => {
            for (cursor_id, cursor) in cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveWordEnd => {
            for (cursor_id, cursor) in cursors.iter() {
                let new_pos = find_word_end_right(&state.buffer, cursor.position);
                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveDocumentStart => {
            for (cursor_id, cursor) in cursors.iter() {
                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: 0,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MoveDocumentEnd => {
            for (cursor_id, cursor) in cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: max_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0, // Reset sticky column
                });
            }
        }

        Action::MovePageUp => {
            for (cursor_id, cursor) in cursors.iter() {
                // Move up by viewport height
                let lines_to_move = viewport_height.saturating_sub(1) as usize;
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
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

                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        Action::MovePageDown => {
            for (cursor_id, cursor) in cursors.iter() {
                // Move down by viewport height
                let lines_to_move = viewport_height.saturating_sub(1) as usize;
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let current_line_start = iter.current_position();
                let current_column = cursor.position - current_line_start;

                // Use sticky_column if set, otherwise use current column
                let goal_column = if cursor.sticky_column > 0 {
                    cursor.sticky_column
                } else {
                    current_column
                };

                // Consume current line
                iter.next_line();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next_line() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + goal_column.min(line_len);
                    } else {
                        // Reached end of buffer - clamp to last valid position
                        new_pos = max_cursor_position(&state.buffer);
                        break;
                    }
                }

                // Preserve anchor if deselect_on_move is false (Emacs mark mode)
                let new_anchor = if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: goal_column, // Preserve the goal column
                });
            }
        }

        // Selection movement - same as regular movement but keeps anchor
        // Uses grapheme cluster boundaries for proper handling of combining characters
        Action::SelectLeft => {
            for (cursor_id, cursor) in cursors.iter() {
                let new_pos = state.buffer.prev_grapheme_boundary(cursor.position);
                let new_pos = adjust_position_for_crlf_left(&state.buffer, new_pos);

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
            for (cursor_id, cursor) in cursors.iter() {
                let max_pos = max_cursor_position(&state.buffer);
                let new_pos = next_position_for_crlf(&state.buffer, cursor.position, max_pos);

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
            for (cursor_id, cursor) in cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
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
            for (cursor_id, cursor) in cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
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
                iter.next_line();
                if let Some((next_line_start, next_line_content)) = iter.next_line() {
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

        Action::SelectToParagraphUp => {
            // Jump to previous empty line while extending selection
            for (cursor_id, cursor) in cursors.iter() {
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);

                // Move to previous line first
                let mut found_pos = None;
                while let Some((line_start, line_content)) = iter.prev() {
                    // Check if this is an empty line (only whitespace/newline)
                    let trimmed = line_content.trim_end_matches(|c| c == '\n' || c == '\r');
                    if trimmed.is_empty() || trimmed.chars().all(char::is_whitespace) {
                        found_pos = Some(line_start);
                        break;
                    }
                }

                // If no empty line found, go to start of buffer
                let new_pos = found_pos.unwrap_or(0);

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0,
                });
            }
        }

        Action::SelectToParagraphDown => {
            // Jump to next empty line while extending selection
            for (cursor_id, cursor) in cursors.iter() {
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);

                // Skip current line
                iter.next_line();

                // Find next empty line
                let mut found_pos = None;
                while let Some((line_start, line_content)) = iter.next_line() {
                    // Check if this is an empty line (only whitespace/newline)
                    let trimmed = line_content.trim_end_matches(|c| c == '\n' || c == '\r');
                    if trimmed.is_empty() || trimmed.chars().all(char::is_whitespace) {
                        found_pos = Some(line_start);
                        break;
                    }
                }

                // If no empty line found, go to end of buffer
                let new_pos = found_pos.unwrap_or(state.buffer.len());

                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: Some(anchor),
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0,
                });
            }
        }

        Action::SelectLineStart => {
            for (cursor_id, cursor) in cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((line_start, _)) = iter.next_line() {
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
            for (cursor_id, cursor) in cursors.iter() {
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((line_start, line_content)) = iter.next_line() {
                    // In both LF and CRLF mode, cursor lands at the first byte of line ending
                    // For LF: cursor on \n. For CRLF: cursor on \r (before both \r\n)
                    let line_end = line_start + content_len_without_line_ending(&line_content);

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
            for (cursor_id, cursor) in cursors.iter() {
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
            for (cursor_id, cursor) in cursors.iter() {
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

        Action::SelectWordEnd => {
            for (cursor_id, cursor) in cursors.iter() {
                let new_pos = find_word_end_right(&state.buffer, cursor.position);
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
            for (cursor_id, cursor) in cursors.iter() {
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
            for (cursor_id, cursor) in cursors.iter() {
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
            for (cursor_id, cursor) in cursors.iter() {
                let lines_to_move = viewport_height.saturating_sub(1) as usize;
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
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
            for (cursor_id, cursor) in cursors.iter() {
                let lines_to_move = viewport_height.saturating_sub(1) as usize;
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
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
                iter.next_line();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next_line() {
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
            let primary_id = cursors.primary_id();
            let primary_cursor = cursors.primary();
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
            for (cursor_id, cursor) in cursors.iter() {
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
            let mut cursor_vec: Vec<_> = cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            // Collect all deletions first, checking for auto-pair deletion
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((*cursor_id, range))
                    } else if cursor.position > 0 {
                        // Use prev_char_boundary to delete one code point at a time
                        // This allows "layer-by-layer" deletion of Thai combining marks
                        // In CRLF files, this also ensures we delete \r\n as a unit
                        let delete_from = state.buffer.prev_char_boundary(cursor.position);
                        let delete_from = adjust_position_for_crlf_left(&state.buffer, delete_from);

                        // Check for auto-pair deletion when auto_indent is enabled
                        // Note: Auto-pairs are ASCII-only, so we can safely check single bytes
                        if auto_indent && cursor.position < state.buffer.len() {
                            let char_before = state
                                .buffer
                                .slice_bytes(delete_from..cursor.position)
                                .first()
                                .copied();
                            let char_after = state
                                .buffer
                                .slice_bytes(cursor.position..cursor.position + 1)
                                .first()
                                .copied();

                            // Check if we're between matching brackets/quotes
                            let is_matching_pair = matches!(
                                (char_before, char_after),
                                (Some(b'('), Some(b')'))
                                    | (Some(b'['), Some(b']'))
                                    | (Some(b'{'), Some(b'}'))
                                    | (Some(b'"'), Some(b'"'))
                                    | (Some(b'\''), Some(b'\''))
                                    | (Some(b'`'), Some(b'`'))
                            );

                            if is_matching_pair {
                                // Delete both opening and closing characters
                                Some((*cursor_id, delete_from..cursor.position + 1))
                            } else {
                                Some((*cursor_id, delete_from..cursor.position))
                            }
                        } else {
                            Some((*cursor_id, delete_from..cursor.position))
                        }
                    } else {
                        None
                    }
                })
                .collect();

            // Get text and create delete events
            apply_deletions(state, deletions, &mut events);
        }

        Action::DeleteForward => {
            // Sort cursors by position (reverse order) to avoid position shifts
            let mut cursor_vec: Vec<_> = cursors.iter().collect();
            cursor_vec.sort_by_key(|(_, c)| std::cmp::Reverse(c.position));

            let buffer_len = state.buffer.len();

            // Collect all deletions first
            let deletions: Vec<_> = cursor_vec
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((*cursor_id, range))
                    } else if cursor.position < buffer_len {
                        // Use next_char_boundary to properly handle multi-byte UTF-8 characters
                        // In CRLF files, this also ensures we delete \r\n as a unit
                        let delete_to =
                            next_position_for_crlf(&state.buffer, cursor.position, buffer_len);

                        Some((*cursor_id, cursor.position..delete_to))
                    } else {
                        None
                    }
                })
                .collect();

            // Get text and create delete events
            apply_deletions(state, deletions, &mut events);
        }

        Action::DeleteWordBackward => {
            // Collect ranges first to avoid borrow checker issues
            let deletions: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((cursor_id, range))
                    } else {
                        let word_start = find_word_start_left(&state.buffer, cursor.position);
                        if word_start < cursor.position {
                            Some((cursor_id, word_start..cursor.position))
                        } else {
                            None
                        }
                    }
                })
                .collect();

            // Now get text and create events
            apply_deletions(state, deletions, &mut events);
        }

        Action::DeleteWordForward => {
            // Collect ranges first to avoid borrow checker issues
            let deletions: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        Some((cursor_id, range))
                    } else {
                        let word_end = find_word_start_right(&state.buffer, cursor.position);
                        if cursor.position < word_end {
                            Some((cursor_id, cursor.position..word_end))
                        } else {
                            None
                        }
                    }
                })
                .collect();

            // Now get text and create events
            apply_deletions(state, deletions, &mut events);
        }

        Action::DeleteLine => {
            // Collect line ranges first to avoid borrow checker issues
            let deletions: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    let mut iter = state
                        .buffer
                        .line_iterator(cursor.position, estimated_line_length);
                    let line_start = iter.current_position();
                    iter.next_line().map(|(_start, content)| {
                        let line_end = line_start + content.len();
                        (cursor_id, line_start..line_end)
                    })
                })
                .collect();

            // Now get text and create events
            apply_deletions(state, deletions, &mut events);
        }

        Action::DeleteToLineEnd => {
            // Delete from cursor to end of line (like Ctrl+K in emacs/bash)
            let deletions: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    let mut iter = state
                        .buffer
                        .line_iterator(cursor.position, estimated_line_length);
                    let line_start = iter.current_position();
                    iter.next_line().map(|(_start, content)| {
                        let line_end = line_start + content_len_without_line_ending(&content);
                        if cursor.position < line_end {
                            Some((cursor_id, cursor.position..line_end))
                        } else {
                            // If cursor is at end of line content, delete the newline instead
                            let full_line_end = line_start + content.len();
                            if cursor.position < full_line_end {
                                Some((cursor_id, cursor.position..full_line_end))
                            } else {
                                None
                            }
                        }
                    })?
                })
                .collect();

            apply_deletions(state, deletions, &mut events);
        }

        Action::DeleteToLineStart => {
            // Delete from start of line to cursor (like Ctrl+U in bash)
            let deletions: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    let iter = state
                        .buffer
                        .line_iterator(cursor.position, estimated_line_length);
                    let line_start = iter.current_position();
                    if cursor.position > line_start {
                        Some((cursor_id, line_start..cursor.position))
                    } else {
                        None
                    }
                })
                .collect();

            apply_deletions(state, deletions, &mut events);
        }

        Action::MoveLineUp => {
            move_lines(
                state,
                cursors,
                &mut events,
                LineMoveDirection::Up,
                estimated_line_length,
            );
        }

        Action::MoveLineDown => {
            move_lines(
                state,
                cursors,
                &mut events,
                LineMoveDirection::Down,
                estimated_line_length,
            );
        }

        Action::TransposeChars => {
            // Transpose the character before the cursor with the one at the cursor
            // Collect cursor positions first to avoid borrow issues
            let cursor_positions: Vec<_> = cursors.iter().map(|(id, c)| (id, c.position)).collect();

            for (cursor_id, pos) in cursor_positions {
                // Need at least 2 characters: one before and one at cursor
                if pos > 0 && pos < state.buffer.len() {
                    // Get the two characters as a string
                    let text = state.get_text_range(pos - 1, pos + 1);
                    let chars: Vec<char> = text.chars().collect();
                    if chars.len() >= 2 {
                        // Delete both characters and insert them swapped
                        events.push(Event::Delete {
                            range: (pos - 1)..(pos + 1),
                            deleted_text: text,
                            cursor_id,
                        });
                        let swapped = format!("{}{}", chars[1], chars[0]);
                        events.push(Event::Insert {
                            position: pos - 1,
                            text: swapped,
                            cursor_id,
                        });
                    }
                }
            }
        }

        Action::ToUpperCase => {
            transform_case(state, cursors, &mut events, |s| s.to_uppercase());
        }

        Action::ToLowerCase => {
            transform_case(state, cursors, &mut events, |s| s.to_lowercase());
        }

        Action::SortLines => {
            // Sort selected lines alphabetically
            // Process cursors in reverse order to avoid position shifts
            let line_ending = state.buffer.line_ending().as_str();
            let mut selections: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    cursor.selection_range().map(|range| (cursor_id, range))
                })
                .collect();
            selections.sort_by_key(|(_, range)| std::cmp::Reverse(range.start));

            for (cursor_id, range) in selections {
                let text = state.get_text_range(range.start, range.end);
                // Split into lines, preserving the original line ending style
                let mut lines: Vec<&str> = text.lines().collect();
                // Check if original text ends with a newline
                let ends_with_newline = text.ends_with('\n') || text.ends_with("\r\n");

                if lines.len() > 1 {
                    lines.sort();
                    let mut sorted_text = lines.join(line_ending);
                    if ends_with_newline {
                        sorted_text.push_str(line_ending);
                    }

                    if sorted_text != text {
                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text: text,
                            cursor_id,
                        });
                        events.push(Event::Insert {
                            position: range.start,
                            text: sorted_text,
                            cursor_id,
                        });
                    }
                }
            }
        }

        Action::OpenLine => {
            // Insert a newline at cursor position but don't move cursor
            // (like pressing Enter but staying on current line)
            let line_ending = state.buffer.line_ending().as_str();
            for (cursor_id, cursor) in cursors.iter() {
                events.push(Event::Insert {
                    position: cursor.position,
                    text: line_ending.to_string(),
                    cursor_id,
                });
            }
        }

        Action::DuplicateLine => {
            // Duplicate the current line (or selected lines) below
            // Process cursors in reverse order to avoid position shifts
            let mut cursor_data: Vec<_> = cursors
                .iter()
                .filter_map(|(cursor_id, cursor)| {
                    if let Some(range) = cursor.selection_range() {
                        // Has selection: duplicate selected lines
                        let start_line = state.buffer.get_line_number(range.start);
                        let end_line = state
                            .buffer
                            .get_line_number(range.end.saturating_sub(1).max(range.start));
                        let line_start = state.buffer.line_start_offset(start_line)?;
                        // Get end of last line
                        let mut iter = state.buffer.line_iterator(
                            state.buffer.line_start_offset(end_line)?,
                            estimated_line_length,
                        );
                        let end_line_start = iter.current_position();
                        iter.next_line().map(|(_, content)| {
                            let line_end = end_line_start + content.len();
                            (cursor_id, line_start, line_end)
                        })
                    } else {
                        // No selection: duplicate current line
                        let mut iter = state
                            .buffer
                            .line_iterator(cursor.position, estimated_line_length);
                        let line_start = iter.current_position();
                        iter.next_line().map(|(_, content)| {
                            let line_end = line_start + content.len();
                            (cursor_id, line_start, line_end)
                        })
                    }
                })
                .collect();
            cursor_data.sort_by_key(|(_, start, _)| std::cmp::Reverse(*start));

            for (cursor_id, line_start, line_end) in cursor_data {
                let line_text = state.get_text_range(line_start, line_end);
                let line_ending = state.buffer.line_ending().as_str();
                // If the line doesn't end with a newline, prepend one
                let has_trailing_newline = line_text.ends_with('\n') || line_text.ends_with("\r\n");
                let insert_text = if has_trailing_newline {
                    line_text
                } else {
                    format!("{}{}", line_ending, line_text)
                };
                let insert_len = insert_text.len();
                events.push(Event::Insert {
                    position: line_end,
                    text: insert_text,
                    cursor_id,
                });

                // Move cursor to start of the newly duplicated line.
                // After the Insert, apply_insert places cursor at line_end + insert_len.
                // The new line starts at line_end (if original had trailing newline)
                // or line_end + line_ending.len() (if we prepended a newline).
                let new_line_start = if has_trailing_newline {
                    line_end
                } else {
                    line_end + line_ending.len()
                };
                let cursor = cursors.get(cursor_id);
                let old_sticky = cursor.map(|c| c.sticky_column).unwrap_or(0);
                events.push(Event::MoveCursor {
                    cursor_id,
                    old_position: line_end + insert_len,
                    new_position: new_line_start,
                    old_anchor: None,
                    new_anchor: None,
                    old_sticky_column: old_sticky,
                    new_sticky_column: 0,
                });
            }
        }

        Action::Recenter => {
            // Scroll so that the cursor is centered in the view
            // This is handled specially - we emit a Recenter event
            events.push(Event::Recenter);
        }

        Action::SetMark => {
            // Set the selection anchor at the current cursor position
            // This starts a selection that extends as the cursor moves
            for (cursor_id, cursor) in cursors.iter() {
                events.push(Event::SetAnchor {
                    cursor_id,
                    position: cursor.position,
                });
            }
        }

        Action::RemoveSecondaryCursors => {
            // Generate RemoveCursor events for all cursors except the first (original) one
            // Also clear anchor and reset deselect_on_move on all cursors (cancels Emacs mark mode)
            // Find the first cursor ID (lowest ID = original cursor)
            let first_id = cursors
                .iter()
                .map(|(id, _)| id)
                .min_by_key(|id| id.0)
                .expect("Should have at least one cursor");

            for (cursor_id, cursor) in cursors.iter() {
                if cursor_id != first_id {
                    events.push(Event::RemoveCursor {
                        cursor_id,
                        position: cursor.position,
                        anchor: cursor.anchor,
                    });
                }
                // Clear anchor and reset deselect_on_move for all cursors (including the first one)
                events.push(Event::ClearAnchor { cursor_id });
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
        | Action::ForceQuit
        | Action::Detach
        | Action::Save
        | Action::SaveAs
        | Action::Open
        | Action::SwitchProject
        | Action::New
        | Action::Close
        | Action::CloseTab
        | Action::GotoLine
        | Action::NextBuffer
        | Action::PrevBuffer
        | Action::SwitchToPreviousTab
        | Action::SwitchToTabByName
        | Action::NavigateBack
        | Action::NavigateForward
        | Action::SplitHorizontal
        | Action::SplitVertical
        | Action::CloseSplit
        | Action::NextSplit
        | Action::PrevSplit
        | Action::Copy
        | Action::CopyWithTheme(_)
        | Action::Cut
        | Action::Paste
        | Action::YankWordForward
        | Action::YankWordBackward
        | Action::YankToLineEnd
        | Action::YankToLineStart
        | Action::AddCursorNextMatch
        | Action::AddCursorAbove
        | Action::AddCursorBelow
        | Action::CommandPalette
        | Action::QuickOpen
        | Action::ShowHelp
        | Action::ToggleLineWrap
        | Action::ToggleComposeMode
        | Action::SetComposeWidth
        | Action::IncreaseSplitSize
        | Action::DecreaseSplitSize
        | Action::ToggleMaximizeSplit
        | Action::Undo
        | Action::Redo
        | Action::GoToMatchingBracket
        | Action::JumpToNextError
        | Action::JumpToPreviousError
        | Action::ShowKeyboardShortcuts
        | Action::ShowWarnings
        | Action::ShowStatusLog
        | Action::ShowLspStatus
        | Action::ClearWarnings
        | Action::SmartHome
        | Action::ToggleComment
        | Action::SetBookmark(_)
        | Action::JumpToBookmark(_)
        | Action::ClearBookmark(_)
        | Action::ListBookmarks
        | Action::ToggleSearchCaseSensitive
        | Action::ToggleSearchWholeWord
        | Action::ToggleSearchRegex
        | Action::ToggleSearchConfirmEach
        | Action::StartMacroRecording
        | Action::StopMacroRecording
        | Action::PlayMacro(_)
        | Action::ToggleMacroRecording(_)
        | Action::ShowMacro(_)
        | Action::ListMacros
        | Action::PromptRecordMacro
        | Action::PromptPlayMacro
        | Action::PlayLastMacro
        | Action::PromptSetBookmark
        | Action::PromptJumpToBookmark
        | Action::PromptConfirm
        | Action::PromptConfirmWithText(_)
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
        | Action::PromptDeleteToLineEnd
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
        | Action::FileBrowserToggleHidden
        | Action::FileBrowserToggleDetectEncoding
        | Action::PopupSelectNext
        | Action::PopupSelectPrev
        | Action::PopupPageUp
        | Action::PopupPageDown
        | Action::PopupConfirm
        | Action::PopupCancel
        | Action::ToggleFileExplorer
        | Action::ToggleMenuBar
        | Action::ToggleTabBar
        | Action::ToggleVerticalScrollbar
        | Action::ToggleHorizontalScrollbar
        | Action::FocusFileExplorer
        | Action::FocusEditor
        | Action::SetBackground
        | Action::SetBackgroundBlend
        | Action::FileExplorerUp
        | Action::FileExplorerDown
        | Action::FileExplorerPageUp
        | Action::FileExplorerPageDown
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
        | Action::FileExplorerSearchClear
        | Action::FileExplorerSearchBackspace
        | Action::LspCompletion
        | Action::LspGotoDefinition
        | Action::LspReferences
        | Action::LspRename
        | Action::LspHover
        | Action::LspSignatureHelp
        | Action::LspCodeActions
        | Action::LspRestart
        | Action::LspStop
        | Action::LspToggleForBuffer
        | Action::ToggleInlayHints
        | Action::ToggleMouseHover
        | Action::ToggleLineNumbers
        | Action::ToggleScrollSync
        | Action::ToggleMouseCapture
        | Action::DumpConfig
        | Action::Search
        | Action::FindInSelection
        | Action::FindNext
        | Action::FindPrevious
        | Action::FindSelectionNext
        | Action::FindSelectionPrevious
        | Action::Replace
        | Action::QueryReplace
        | Action::MenuActivate
        | Action::MenuClose
        | Action::MenuLeft
        | Action::MenuRight
        | Action::MenuUp
        | Action::MenuDown
        | Action::MenuExecute
        | Action::MenuOpen(_)
        | Action::SwitchKeybindingMap(_)
        | Action::PluginAction(_)
        | Action::None
        | Action::ScrollTabsLeft
        | Action::ScrollTabsRight
        | Action::SelectTheme
        | Action::SelectKeybindingMap
        | Action::SelectCursorStyle
        | Action::SelectLocale
        | Action::Revert
        | Action::ToggleAutoRevert
        | Action::FormatBuffer
        | Action::TrimTrailingWhitespace
        | Action::EnsureFinalNewline
        | Action::OpenTerminal
        | Action::CloseTerminal
        | Action::FocusTerminal
        | Action::TerminalEscape
        | Action::ToggleKeyboardCapture
        | Action::TerminalPaste
        | Action::OpenSettings
        | Action::CloseSettings
        | Action::SettingsSave
        | Action::SettingsReset
        | Action::SettingsToggleFocus
        | Action::SettingsActivate
        | Action::SettingsSearch
        | Action::SettingsHelp
        | Action::SettingsIncrement
        | Action::SettingsDecrement
        | Action::SetTabSize
        | Action::SetLineEnding
        | Action::SetEncoding
        | Action::ReloadWithEncoding
        | Action::SetLanguage
        | Action::ToggleIndentationStyle
        | Action::ToggleTabIndicators
        | Action::ToggleDebugHighlights
        | Action::ResetBufferSettings
        | Action::ShellCommand
        | Action::ShellCommandReplace
        | Action::CalibrateInput
        | Action::EventDebug
        | Action::OpenKeybindingEditor
        | Action::AddRuler
        | Action::RemoveRuler => return None,

        // Block/rectangular selection actions
        Action::BlockSelectLeft => {
            block_select_action(state, cursors, &mut events, BlockDirection::Left);
        }

        Action::BlockSelectRight => {
            block_select_action(state, cursors, &mut events, BlockDirection::Right);
        }

        Action::BlockSelectUp => {
            block_select_action(state, cursors, &mut events, BlockDirection::Up);
        }

        Action::BlockSelectDown => {
            block_select_action(state, cursors, &mut events, BlockDirection::Down);
        }

        Action::SelectLine => {
            // Select the entire line for each cursor
            for (cursor_id, cursor) in cursors.iter() {
                // Use iterator to get line bounds
                let mut iter = state
                    .buffer
                    .line_iterator(cursor.position, estimated_line_length);
                if let Some((line_start, line_content)) = iter.next_line() {
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
            for (cursor_id, cursor) in cursors.iter() {
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
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::model::cursor::Cursors;
    use crate::model::event::{CursorId, Event};
    use crate::state::EditorState;

    #[test]
    fn test_backspace_deletes_newline() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "Hello\nWorld"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "Hello\nWorld".to_string(),
                cursor_id: CursorId(0),
            },
        );

        assert_eq!(state.buffer.to_string().unwrap(), "Hello\nWorld");
        assert_eq!(cursors.primary().position, 11);

        // Move cursor to position 6 (beginning of "World")
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 0,
                new_position: 6,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(cursors.primary().position, 6);

        // Press Backspace - should delete the newline at position 5
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        println!("Generated events: {:?}", events);

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "HelloWorld");
        assert_eq!(cursors.primary().position, 5);
    }

    #[test]
    fn test_move_down_basic() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert three lines
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "Line1\nLine2\nLine3".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to start of file
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 17,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(cursors.primary().position, 0);

        // Move down - should go to position 6 (start of Line2)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 6, "Cursor should move to start of Line2");
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&mut cursors, &events[0]);
        assert_eq!(cursors.primary().position, 6);

        // Move down again - should go to position 12 (start of Line3)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 12, "Cursor should move to start of Line3");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_line_up_without_trailing_newline() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "A\nB".to_string(),
                cursor_id: CursorId(0),
            },
        );

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: 2, // "B"
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineUp,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "B\nA");
        assert_eq!(cursors.primary().position, 0);
    }

    #[test]
    fn test_move_line_down_without_trailing_newline() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "A\nB".to_string(),
                cursor_id: CursorId(0),
            },
        );

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: 0, // "A"
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineDown,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "B\nA");
        assert_eq!(cursors.primary().position, 2);
    }

    #[test]
    fn test_move_line_up_first_line_noop() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "A\nB".to_string(),
                cursor_id: CursorId(0),
            },
        );

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineUp,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        assert!(events.is_empty());
        assert_eq!(state.buffer.to_string().unwrap(), "A\nB");
        assert_eq!(cursors.primary().position, 0);
    }

    #[test]
    fn test_move_line_down_last_line_noop() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "A\nB".to_string(),
                cursor_id: CursorId(0),
            },
        );

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: 2, // "B"
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineDown,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        assert!(events.is_empty());
        assert_eq!(state.buffer.to_string().unwrap(), "A\nB");
        assert_eq!(cursors.primary().position, 2);
    }

    #[test]
    fn test_move_line_up_multi_cursor_separate_lines() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "A\nB\nC\nD".to_string(),
                cursor_id: CursorId(0),
            },
        );

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: 2, // "B"
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        state.apply(
            &mut cursors,
            &Event::AddCursor {
                position: 6, // "D"
                cursor_id: CursorId(1),
                anchor: None,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineUp,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "B\nA\nD\nC");
        assert_eq!(cursors.get(CursorId(0)).unwrap().position, 0);
        assert_eq!(cursors.get(CursorId(1)).unwrap().position, 4);
    }

    #[test]
    fn test_move_line_up_large_file_unloaded_chunks() {
        use crate::model::buffer::TextBuffer;
        use std::fs;
        use tempfile::tempdir;

        let temp_dir = tempdir().unwrap();
        let test_file = temp_dir.path().join("move_line_up_large_file.txt");

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("Line {i:04}\n"));
        }
        fs::write(&test_file, &content).unwrap();

        let large_file_threshold = 500;
        let buffer =
            TextBuffer::load_from_file(&test_file, large_file_threshold, test_fs()).unwrap();

        let mut state = EditorState::new(80, 24, large_file_threshold, test_fs());
        let mut cursors = Cursors::new();
        state.buffer = buffer;

        let line_len = "Line 0000\n".len();
        let target_line = 120usize;
        let target_start = line_len * target_line;

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: target_start,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineUp,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        let line_119_start = line_len * (target_line - 1);
        let line_120_start = line_len * target_line;
        let line_119 = state.get_text_range(line_119_start, line_119_start + line_len);
        let line_120 = state.get_text_range(line_120_start, line_120_start + line_len);

        assert_eq!(line_119, "Line 0120\n");
        assert_eq!(line_120, "Line 0119\n");
    }

    #[test]
    fn test_move_line_down_large_file_selection_block() {
        use crate::model::buffer::TextBuffer;
        use std::fs;
        use tempfile::tempdir;

        let temp_dir = tempdir().unwrap();
        let test_file = temp_dir.path().join("move_line_down_large_file.txt");

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("Line {i:04}\n"));
        }
        fs::write(&test_file, &content).unwrap();

        let large_file_threshold = 500;
        let buffer =
            TextBuffer::load_from_file(&test_file, large_file_threshold, test_fs()).unwrap();

        let mut state = EditorState::new(80, 24, large_file_threshold, test_fs());
        let mut cursors = Cursors::new();
        state.buffer = buffer;

        let line_len = "Line 0000\n".len();
        let start_line = 50usize;
        let end_line_exclusive = 53usize; // selects lines 50..=52
        let selection_start = line_len * start_line;
        let selection_end = line_len * end_line_exclusive;

        let pos = cursors.primary().position;
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: pos,
                new_position: selection_end,
                old_anchor: None,
                new_anchor: Some(selection_start),
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineDown,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        let line_50 = state.get_text_range(selection_start, selection_start + line_len);
        let line_51 =
            state.get_text_range(selection_start + line_len, selection_start + line_len * 2);
        let line_52 = state.get_text_range(
            selection_start + line_len * 2,
            selection_start + line_len * 3,
        );
        let line_53 = state.get_text_range(
            selection_start + line_len * 3,
            selection_start + line_len * 4,
        );

        assert_eq!(line_50, "Line 0053\n");
        assert_eq!(line_51, "Line 0050\n");
        assert_eq!(line_52, "Line 0051\n");
        assert_eq!(line_53, "Line 0052\n");
    }

    #[test]
    fn test_move_up_basic() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert three lines
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "Line1\nLine2\nLine3".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Cursor is at end (position 17)
        // Text structure: "Line1\nLine2\nLine3"
        // Positions: 0-4 (Line1), 5 (\n), 6-10 (Line2), 11 (\n), 12-16 (Line3)
        assert_eq!(cursors.primary().position, 17);
        assert_eq!(state.buffer.to_string().unwrap(), "Line1\nLine2\nLine3");

        // Move up - cursor is at end of Line3 (position 17, column 5)
        // Should go to end of Line2 (position 11, which is the newline, BUT we want column 5 which is position 11)
        // Wait, Line2 has content "Line2" (5 chars), so column 5 is position 6+5=11 (the newline)
        // This is technically correct but weird - we're on the newline
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            // The current behavior puts us at position 11 (the newline after Line2)
            // This happens because Line2 without newline has length 5, and we preserve column 5
            // Position 6 (start of Line2) + 5 = 11 (the newline)
            assert_eq!(
                *new_position, 11,
                "Cursor should move to column 5 of Line2 (which is the newline)"
            );
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&mut cursors, &events[0]);

        // Move up again - from position 11 (newline after Line2)
        // Current line is Line2 (starts at 6), column is 11-6=5
        // Previous line is Line1 (starts at 0), content "Line1" has length 5
        // So we go to position 0 + min(5, 5) = 5 (the newline after Line1)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(
                *new_position, 5,
                "Cursor should move to column 5 of Line1 (the newline)"
            );
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_down_preserves_column() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert lines with different lengths
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "12345\n123\n12345".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to position 3 (column 3 of first line)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 15,
                new_position: 3,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(cursors.primary().position, 3);

        // Move down - should go to position 9 (column 3 of second line, which is end of "123")
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(
                *new_position, 9,
                "Cursor should move to end of shorter line"
            );
            assert_eq!(
                *new_sticky_column, 3,
                "Sticky column should preserve original column"
            );
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&mut cursors, &events[0]);

        // Move down again - should go to position 13 (column 3 of third line)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(*new_position, 13, "Cursor should move back to column 3");
            assert_eq!(*new_sticky_column, 3, "Sticky column should be preserved");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_up_preserves_column() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert lines with different lengths
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "12345\n123\n12345".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to position 13 (column 3 of third line)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 15,
                new_position: 13,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(cursors.primary().position, 13);

        // Move up - should go to position 9 (column 3 of second line, which is end of "123")
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(
                *new_position, 9,
                "Cursor should move to end of shorter line"
            );
            assert_eq!(
                *new_sticky_column, 3,
                "Sticky column should preserve original column"
            );
        } else {
            panic!("Expected MoveCursor event");
        }

        state.apply(&mut cursors, &events[0]);

        // Move up again - should go to position 3 (column 3 of first line)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor {
            new_position,
            new_sticky_column,
            ..
        } = &events[0]
        {
            assert_eq!(*new_position, 3, "Cursor should move back to column 3");
            assert_eq!(*new_sticky_column, 3, "Sticky column should be preserved");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_down_at_line_start() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert two lines
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "First\nSecond".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to start (position 0)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 12,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Move down - should go to position 6 (start of second line)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 6, "Cursor should move to start of next line");
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_up_at_line_start() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert two lines
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "First\nSecond".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to start of second line (position 6)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 12,
                new_position: 6,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Move up - should go to position 0 (start of first line)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        assert_eq!(events.len(), 1);

        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(
                *new_position, 0,
                "Cursor should move to start of previous line"
            );
        } else {
            panic!("Expected MoveCursor event");
        }
    }

    #[test]
    fn test_move_down_with_empty_lines() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert lines with empty line in middle
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "Line1\n\nLine3".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to start
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 12,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Move down - should go to position 6 (empty line)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 6, "Cursor should move to empty line");
        }

        state.apply(&mut cursors, &events[0]);

        // Move down again - should go to position 7 (start of Line3)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        if let Event::MoveCursor { new_position, .. } = &events[0] {
            assert_eq!(*new_position, 7, "Cursor should move to Line3");
        }
    }

    #[test]
    fn test_column_calculation_doesnt_underflow() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert a single line
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "Hello".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Set cursor at end (position 5)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 5,
                new_position: 5,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Try to move up (no previous line exists)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        assert_eq!(
            events.len(),
            0,
            "Should not generate event when at first line"
        );

        // Try to move down (no next line exists)
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        assert_eq!(
            events.len(),
            0,
            "Should not generate event when at last line"
        );
    }

    #[test]
    fn test_line_iterator_positioning_for_cursor_movement() {
        // This test verifies the behavior of line_iterator when positioning at different offsets
        // to understand how column calculation works
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "Line1\nLine2\nLine3".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // First, let's verify what offset_to_position returns for key positions
        // Text structure: "Line1\nLine2\nLine3"
        // Positions: 0-4 (Line1), 5 (\n), 6-10 (Line2), 11 (\n), 12-16 (Line3), 17 (end)

        // Position 11 is the newline after "Line2"
        if let Some(pos) = state.buffer.offset_to_position(11) {
            println!(
                "offset_to_position(11) = line={}, column={}",
                pos.line, pos.column
            );
            // The newline is the 6th character of line 1 (0-indexed): "Line2\n"
            // So column should be 5 (0-indexed)
        }

        // Position 17 is after "Line3"
        if let Some(pos) = state.buffer.offset_to_position(17) {
            println!(
                "offset_to_position(17) = line={}, column={}",
                pos.line, pos.column
            );
            // This is the 6th character of line 2 (after "Line3")
            // So column should be 5
        }

        // Test 1: Position at end of Line3 (position 17)
        // line_iterator(17) should position at start of Line3 (position 12)
        let iter = state.buffer.line_iterator(17, 80);
        assert_eq!(
            iter.current_position(),
            12,
            "Iterator at position 17 should be at line start 12"
        );

        // Test 2: Position in middle of Line2 (position 9, which is 'n' in "Line2")
        let iter = state.buffer.line_iterator(9, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Iterator at position 9 should be at line start 6"
        );

        // Test 3: Position at newline after Line2 (position 11)
        let iter = state.buffer.line_iterator(11, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Iterator at position 11 (newline) should be at line start 6"
        );

        // Test 4: Position at start of Line2 (position 6)
        let iter = state.buffer.line_iterator(6, 80);
        assert_eq!(
            iter.current_position(),
            6,
            "Iterator at position 6 should stay at 6"
        );
    }

    #[test]
    fn test_move_line_end_positioning() {
        // Test where MoveLineEnd actually puts the cursor
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "HelloNew Line\nWorld!".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Start at position 0
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 20,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Move to line end
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineEnd,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            println!("MoveLineEnd event: {:?}", event);
            state.apply(&mut cursors, &event);
        }

        println!(
            "After MoveLineEnd: cursor at {}",
            cursors.primary().position
        );
        // "HelloNew Line\n" - the visible part is 13 chars (0-12)
        // MoveLineEnd should put cursor at position 13 (after the visible text, before/on the newline)
        assert_eq!(
            cursors.primary().position,
            13,
            "MoveLineEnd should position at end of visible text"
        );
    }

    #[test]
    fn test_move_line_start_from_eof() {
        // Test MoveLineStart when cursor is at EOF (beyond last character)
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "HelloNew Line\nWorld!".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Cursor is at EOF (position 20)
        assert_eq!(cursors.primary().position, 20);
        println!("Starting at EOF: position 20");

        // Check what line_iterator does at EOF
        let iter = state.buffer.line_iterator(20, 80);
        println!(
            "line_iterator(20).current_position() = {}",
            iter.current_position()
        );

        // Move to line start
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineStart,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            println!("MoveLineStart event from EOF: {:?}", event);
            state.apply(&mut cursors, &event);
        }

        println!(
            "After MoveLineStart from EOF: cursor at {}",
            cursors.primary().position
        );
        // Should move to position 14 (start of "World!" line)
        assert_eq!(
            cursors.primary().position,
            14,
            "MoveLineStart from EOF should go to start of last line"
        );
    }

    #[test]
    fn test_move_up_with_unloaded_chunks() {
        // Test MoveUp when the chunk containing the cursor hasn't been loaded yet
        // This simulates large file behavior where not all chunks are in memory
        use crate::model::buffer::TextBuffer;
        use std::fs;

        // Create a temp file with multiple lines
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test_large_file_move_up.txt");

        // Write 100 lines to simulate a larger file (each line ~25 bytes)
        let mut content = String::new();
        for i in 0..100 {
            content.push_str(&format!("This is line number {}\n", i));
        }
        fs::write(&test_file, &content).unwrap();

        // Use a VERY SMALL threshold (500 bytes) to force lazy loading behavior
        // This ensures chunks won't all be loaded at once
        let large_file_threshold = 500;
        let buffer =
            TextBuffer::load_from_file(&test_file, large_file_threshold, test_fs()).unwrap();

        // Create editor state with the loaded buffer
        let mut state = EditorState::new(80, 24, large_file_threshold, test_fs());
        let mut cursors = Cursors::new();
        state.buffer = buffer;

        // Move cursor to near the end (line 90)
        let target_line_start: usize = content.lines().take(90).map(|l| l.len() + 1).sum();
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 0,
                new_position: target_line_start,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        println!(
            "Cursor at line 90, position: {}",
            cursors.primary().position
        );

        // Try to move up - this should work even if chunks aren't loaded
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        println!("MoveUp events: {:?}", events);

        assert!(
            !events.is_empty(),
            "MoveUp should generate events even with unloaded chunks"
        );

        for event in events {
            state.apply(&mut cursors, &event);
        }

        println!("After MoveUp: cursor at {}", cursors.primary().position);
        assert!(
            cursors.primary().position < target_line_start,
            "Cursor should have moved up"
        );

        // Clean up
        fs::remove_file(&test_file).ok();
    }

    #[test]
    fn test_move_down_from_newline_position() {
        // Test moving down when cursor is ON a newline character
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "HelloNew Line\nWorld!"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "HelloNew Line\nWorld!".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Text structure: "HelloNew Line\nWorld!"
        // Positions: 0-12 (HelloNew Line), 13 (\n), 14-19 (World!)
        assert_eq!(state.buffer.to_string().unwrap(), "HelloNew Line\nWorld!");

        // Move cursor to position 13 (the newline after "HelloNew Line")
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 20,
                new_position: 13,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(cursors.primary().position, 13);
        println!("Starting position: 13 (on the newline)");

        // line_iterator(13) should position at...?
        let iter = state.buffer.line_iterator(13, 80);
        println!(
            "line_iterator(13).current_position() = {}",
            iter.current_position()
        );

        // Move down to second line
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        println!("MoveDown events: {:?}", events);

        if events.is_empty() {
            panic!("MoveDown from position 13 generated no events!");
        }

        for event in events {
            state.apply(&mut cursors, &event);
        }
        println!(
            "After MoveDown from position 13: cursor at {}",
            cursors.primary().position
        );

        // We expect to be at position 14 (start of "World!" line) or somewhere on that line
        // NOT at position 20 (EOF)
        assert!(
            cursors.primary().position >= 14 && cursors.primary().position <= 20,
            "After MoveDown from newline, cursor should be on the next line, not at EOF"
        );
    }

    #[test]
    fn test_move_down_then_home_backspace() {
        // Reproduce the e2e test failure scenario
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "HelloNew Line\nWorld!"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "HelloNew Line\nWorld!".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Text structure: "HelloNew Line\nWorld!"
        // Positions: 0-12 (HelloNew Line), 13 (\n), 14-19 (World!)
        assert_eq!(state.buffer.to_string().unwrap(), "HelloNew Line\nWorld!");
        assert_eq!(cursors.primary().position, 20); // End of text

        // Move up to first line
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveUp, 4, false, 80, 24).unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }
        println!("After MoveUp: cursor at {}", cursors.primary().position);

        // Move to end of first line
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineEnd,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }
        assert_eq!(
            cursors.primary().position,
            13,
            "Should be at end of first line (position 13, the newline)"
        );

        // Move down to second line
        let events =
            action_to_events(&mut state, &mut cursors, Action::MoveDown, 4, false, 80, 24).unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }
        println!("After MoveDown: cursor at {}", cursors.primary().position);

        // Move to start of line (Home)
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::MoveLineStart,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }
        println!("After Home: cursor at {}", cursors.primary().position);
        assert_eq!(
            cursors.primary().position,
            14,
            "Should be at start of second line (position 14)"
        );

        // Delete backward (should delete the newline)
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            false,
            80,
            24,
        )
        .unwrap();
        for event in events.iter() {
            println!("Event: {:?}", event);
            state.apply(&mut cursors, event);
        }

        println!(
            "After backspace: buffer = {:?}",
            state.buffer.to_string().unwrap()
        );
        println!("After backspace: cursor at {}", cursors.primary().position);
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "HelloNew LineWorld!",
            "Lines should be joined"
        );
        assert_eq!(
            cursors.primary().position,
            13,
            "Cursor should be at join point"
        );
    }

    #[test]
    fn test_bracket_auto_close_parenthesis() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Cursor is at position 0 initially
        assert_eq!(cursors.primary().position, 0);

        // Insert opening parenthesis with auto_indent=true
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('('),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        println!("Events: {:?}", events);

        // Should have Insert event for "()" and MoveCursor to position between them
        assert_eq!(events.len(), 2, "Should have Insert and MoveCursor events");

        // Apply events
        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "()");
        assert_eq!(
            cursors.primary().position,
            1,
            "Cursor should be between brackets"
        );
    }

    #[test]
    fn test_bracket_auto_close_curly_brace() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert opening curly brace with auto_indent=true
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('{'),
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "{}");
        assert_eq!(
            cursors.primary().position,
            1,
            "Cursor should be between braces"
        );
    }

    #[test]
    fn test_bracket_auto_close_square_bracket() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert opening square bracket
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('['),
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "[]");
        assert_eq!(cursors.primary().position, 1);
    }

    #[test]
    fn test_bracket_auto_close_double_quote() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();
        state.language = "rust".to_string();

        // Insert double quote
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('"'),
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "\"\"");
        assert_eq!(cursors.primary().position, 1);
    }

    #[test]
    fn test_bracket_auto_close_disabled_when_auto_indent_false() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert opening parenthesis with auto_indent=false
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('('),
            4,
            false,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Should only insert the opening character, no auto-close
        assert_eq!(state.buffer.to_string().unwrap(), "(");
        assert_eq!(cursors.primary().position, 1);
    }

    #[test]
    fn test_bracket_auto_close_not_before_alphanumeric() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "abc"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "abc".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor to start
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 3,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Insert opening parenthesis before 'abc'
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('('),
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Should NOT auto-close because 'a' is alphanumeric
        assert_eq!(state.buffer.to_string().unwrap(), "(abc");
        assert_eq!(cursors.primary().position, 1);
    }

    #[test]
    fn test_bracket_auto_close_multiple_cursors() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert some text
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "foo\nbar".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Add a second cursor
        state.apply(
            &mut cursors,
            &Event::AddCursor {
                position: 0,
                cursor_id: CursorId(1),
                anchor: None,
            },
        );

        // Move both cursors to end of their respective lines
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 7,
                new_position: 7, // end of "bar"
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(1),
                old_position: 0,
                new_position: 3, // end of "foo"
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Insert opening parenthesis at both cursors
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('('),
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Both cursors should have auto-closed brackets
        assert_eq!(state.buffer.to_string().unwrap(), "foo()\nbar()");
    }

    #[test]
    fn test_bracket_auto_close_multiple_cursors_with_skip_over() {
        // Test case: type 'foo()' with multiple cursors - the closing paren should skip over
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Start with two empty lines
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "\n".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Primary cursor at position 0 (start of first line)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 1,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Add a second cursor at position 1 (start of second line)
        state.apply(
            &mut cursors,
            &Event::AddCursor {
                position: 1,
                cursor_id: CursorId(1),
                anchor: None,
            },
        );

        // Type 'f'
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('f'),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Type 'o'
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('o'),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Type 'o'
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('o'),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Verify we have "foo\nfoo" before typing '('
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "foo\nfoo",
            "Before typing '(' we should have just 'foo' on each line"
        );

        // Type '(' - should auto-close to '()'
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('('),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Verify auto-close happened: we typed '(' but got '()' on each line
        // This confirms the auto-close feature is working with multiple cursors
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "foo()\nfoo()",
            "Auto-close should add closing paren: typing '(' should produce '()'"
        );

        // Verify cursors are positioned between ( and ) for skip-over to work
        // Buffer is "foo()\nfoo()" - positions: f(0)o(1)o(2)((3))(4)\n(5)f(6)o(7)o(8)((9))(10)
        // After auto-close, cursor should be at position 4 (after '(' at 3, before ')' at 4)
        // and at position 10 (after '(' at 9, before ')' at 10)
        let cursor_positions: Vec<_> = cursors.iter().map(|(_, c)| c.position).collect();
        assert!(
            cursor_positions.contains(&4) && cursor_positions.contains(&10),
            "Cursors should be between parens at positions 4 and 10, got: {:?}",
            cursor_positions
        );

        // Type ')' - should skip over the existing ')', not add another
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar(')'),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Should still be "foo()\nfoo()" - the ')' should have skipped over, not doubled
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "foo()\nfoo()",
            "Closing paren should skip over existing paren, not create 'foo())'"
        );
    }

    #[test]
    fn test_bracket_auto_close_three_cursors_with_skip_over() {
        // Test case: type 'foo()' with THREE cursors - the closing paren should skip over
        // This tests the bug where skip-over fails with 3+ cursors
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Start with three empty lines
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "\n\n".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Primary cursor at position 0 (start of first line)
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 2,
                new_position: 0,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Add a second cursor at position 1 (start of second line)
        state.apply(
            &mut cursors,
            &Event::AddCursor {
                position: 1,
                cursor_id: CursorId(1),
                anchor: None,
            },
        );

        // Add a third cursor at position 2 (start of third line)
        state.apply(
            &mut cursors,
            &Event::AddCursor {
                position: 2,
                cursor_id: CursorId(2),
                anchor: None,
            },
        );

        // Type 'foo'
        for ch in ['f', 'o', 'o'] {
            let events = action_to_events(
                &mut state,
                &mut cursors,
                Action::InsertChar(ch),
                4,
                true,
                80,
                24,
            )
            .unwrap();
            for event in events {
                state.apply(&mut cursors, &event);
            }
        }

        // Verify we have "foo\nfoo\nfoo" before typing '('
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "foo\nfoo\nfoo",
            "Before typing '(' we should have 'foo' on each line"
        );

        // Type '(' - should auto-close to '()'
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar('('),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Verify auto-close happened
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "foo()\nfoo()\nfoo()",
            "Auto-close should add closing paren on all three lines"
        );

        // Verify cursor positions - all should be between ( and )
        let cursor_positions: Vec<_> = cursors.iter().map(|(_, c)| c.position).collect();
        // Buffer is "foo()\nfoo()\nfoo()" - positions:
        // f(0)o(1)o(2)((3))(4)\n(5)f(6)o(7)o(8)((9))(10)\n(11)f(12)o(13)o(14)((15))(16)
        // Cursors should be at 4, 10, 16 (between each ( and ))
        assert!(
            cursor_positions.contains(&4)
                && cursor_positions.contains(&10)
                && cursor_positions.contains(&16),
            "Cursors should be between parens at positions 4, 10, and 16, got: {:?}",
            cursor_positions
        );

        // Type ')' - should skip over the existing ')', not add another
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::InsertChar(')'),
            4,
            true,
            80,
            24,
        )
        .unwrap();
        for event in events {
            state.apply(&mut cursors, &event);
        }

        // Should still be "foo()\nfoo()\nfoo()" - the ')' should have skipped over
        assert_eq!(
            state.buffer.to_string().unwrap(),
            "foo()\nfoo()\nfoo()",
            "Closing paren should skip over existing paren on ALL THREE lines"
        );
    }

    #[test]
    fn test_auto_pair_deletion_parenthesis() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "()"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "()".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor between the brackets
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 2,
                new_position: 1,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        assert_eq!(state.buffer.to_string().unwrap(), "()");
        assert_eq!(cursors.primary().position, 1);

        // Delete backward with auto_indent=true - should delete both characters
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "");
        assert_eq!(cursors.primary().position, 0);
    }

    #[test]
    fn test_auto_pair_deletion_curly_brace() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "{}"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "{}".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor between the braces
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 2,
                new_position: 1,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Delete backward - should delete both
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "");
    }

    #[test]
    fn test_auto_pair_deletion_double_quote() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert empty string literal
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "\"\"".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor between the quotes
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 2,
                new_position: 1,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Delete backward - should delete both quotes
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "");
    }

    #[test]
    fn test_auto_pair_deletion_disabled_when_auto_indent_false() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "()"
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "()".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor between the brackets
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 2,
                new_position: 1,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Delete backward with auto_indent=false - should only delete opening bracket
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            false,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), ")");
        assert_eq!(cursors.primary().position, 0);
    }

    #[test]
    fn test_auto_pair_deletion_not_matching() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "(]" - not a matching pair
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "(]".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor between
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 2,
                new_position: 1,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Delete backward - should only delete opening bracket since they don't match
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "]");
        assert_eq!(cursors.primary().position, 0);
    }

    #[test]
    fn test_auto_pair_deletion_with_content() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        let mut cursors = Cursors::new();

        // Insert "(abc)" - has content between brackets
        state.apply(
            &mut cursors,
            &Event::Insert {
                position: 0,
                text: "(abc)".to_string(),
                cursor_id: CursorId(0),
            },
        );

        // Move cursor after 'a'
        state.apply(
            &mut cursors,
            &Event::MoveCursor {
                cursor_id: CursorId(0),
                old_position: 5,
                new_position: 2,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            },
        );

        // Delete backward - should only delete 'a', not both brackets
        let events = action_to_events(
            &mut state,
            &mut cursors,
            Action::DeleteBackward,
            4,
            true,
            80,
            24,
        )
        .unwrap();

        for event in events {
            state.apply(&mut cursors, &event);
        }

        assert_eq!(state.buffer.to_string().unwrap(), "(bc)");
    }
}

#[cfg(test)]
mod property_tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }

    use super::*;
    use proptest::prelude::*;

    // Generate text with some newlines
    fn text_with_newlines() -> impl Strategy<Value = Vec<u8>> {
        prop::collection::vec(
            prop_oneof![(b'a'..=b'z').prop_map(|c| c), Just(b'\n'),],
            0..200,
        )
    }

    proptest! {
        /// Test that collect_line_starts returns valid line start positions
        #[test]
        fn prop_collect_line_starts_returns_valid_positions(
            text in text_with_newlines(),
            start_frac in 0.0f64..=1.0,
            end_frac in 0.0f64..=1.0,
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut buffer = Buffer::from_bytes(text.clone(), test_fs());
            let buffer_len = buffer.len();

            // Convert fractions to positions, ensuring start <= end
            let start_pos = (start_frac * buffer_len as f64) as usize;
            let end_pos = (end_frac * buffer_len as f64) as usize;
            let (start_pos, end_pos) = if start_pos <= end_pos {
                (start_pos, end_pos)
            } else {
                (end_pos, start_pos)
            };

            let line_starts = collect_line_starts(&mut buffer, start_pos, end_pos, 80);

            // Property 1: All positions should be <= end_pos and <= buffer_len
            for &pos in &line_starts {
                prop_assert!(pos <= end_pos, "Position {} exceeds end_pos {}", pos, end_pos);
                prop_assert!(pos <= buffer_len, "Position {} exceeds buffer_len {}", pos, buffer_len);
            }

            // Property 2: All positions should be valid line starts
            // (either position 0, or the byte before is a newline)
            for &pos in &line_starts {
                if pos == 0 {
                    continue; // Position 0 is always a valid line start
                }
                let prev_byte = buffer.get_text_range_mut(pos - 1, 1).unwrap();
                prop_assert_eq!(
                    prev_byte[0], b'\n',
                    "Position {} is not a valid line start (preceded by {:?})",
                    pos, prev_byte
                );
            }

            // Property 3: Positions should be sorted and have no duplicates
            for window in line_starts.windows(2) {
                prop_assert!(
                    window[0] < window[1],
                    "Positions not strictly increasing: {} >= {}",
                    window[0], window[1]
                );
            }

            // Property 4: Should include all line starts in range
            // Find all actual line starts in the text
            let mut expected_line_starts: Vec<usize> = vec![0];
            for (i, &byte) in text.iter().enumerate() {
                if byte == b'\n' && i < buffer_len {
                    expected_line_starts.push(i + 1);
                }
            }
            // Filter to those in range, considering that we start from the line containing start_pos
            let first_line_start = expected_line_starts.iter()
                .filter(|&&pos| pos <= start_pos)
                .max()
                .copied()
                .unwrap_or(0);
            let expected_in_range: Vec<usize> = expected_line_starts.iter()
                .filter(|&&pos| pos >= first_line_start && pos <= end_pos)
                .copied()
                .collect();

            prop_assert_eq!(
                line_starts, expected_in_range,
                "Line starts mismatch for text {:?} with start={} end={}",
                String::from_utf8_lossy(&text), start_pos, end_pos
            );
        }

        /// Test that collect_line_starts handles edge cases correctly
        #[test]
        fn prop_collect_line_starts_edge_cases(
            text in text_with_newlines(),
        ) {
            if text.is_empty() {
                return Ok(());
            }

            let mut buffer = Buffer::from_bytes(text.clone(), test_fs());
            let buffer_len = buffer.len();

            // Edge case 1: start_pos == end_pos (single position range)
            let mid = buffer_len / 2;
            let line_starts = collect_line_starts(&mut buffer, mid, mid, 80);
            // Should return exactly one line start (the line containing mid)
            prop_assert!(line_starts.len() <= 1, "Single position range should have at most 1 line start");

            // Edge case 2: Full buffer range
            let line_starts = collect_line_starts(&mut buffer, 0, buffer_len, 80);
            // Should return at least position 0
            prop_assert!(!line_starts.is_empty(), "Full range should have at least one line start");
            prop_assert_eq!(line_starts[0], 0, "First line start should be 0 for full range starting at 0");

            // Edge case 3: Range at the very end
            if buffer_len > 0 {
                let line_starts = collect_line_starts(&mut buffer, buffer_len - 1, buffer_len, 80);
                // Should return the line start for the last line
                prop_assert!(!line_starts.is_empty(), "End range should have at least one line start");
            }
        }

        /// Test that trailing newlines produce the correct number of line starts
        #[test]
        fn prop_collect_line_starts_trailing_newline(
            prefix in "[a-z]{0,20}",
            num_trailing_newlines in 0usize..5,
        ) {
            let text = format!("{}{}", prefix, "\n".repeat(num_trailing_newlines));
            if text.is_empty() {
                return Ok(());
            }

            let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
            let buffer_len = buffer.len();

            let line_starts = collect_line_starts(&mut buffer, 0, buffer_len, 80);

            // Expected: 1 (for position 0) + num_trailing_newlines (one for each \n creates a new line start)
            // But we only count line starts that are <= end_pos
            // If prefix is empty and we have N newlines, we should have positions: 0, 1, 2, ..., N
            // But the last one at position N would be > buffer_len - 1 only if it's the synthetic empty line
            let expected_count = if prefix.is_empty() {
                // Just newlines: positions 0, 1, 2, ..., up to buffer_len
                num_trailing_newlines.min(buffer_len) + 1
            } else {
                // prefix + newlines
                1 + num_trailing_newlines
            };

            prop_assert_eq!(
                line_starts.len(), expected_count,
                "Text {:?} (len={}) should have {} line starts, got {:?}",
                text, buffer_len, expected_count, line_starts
            );
        }
    }
}
