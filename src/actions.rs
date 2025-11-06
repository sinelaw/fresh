//! Action to event conversion - translates high-level actions into buffer events

use crate::event::Event;
use crate::keybindings::Action;
use crate::state::EditorState;
use crate::word_navigation::{
    find_word_end, find_word_start, find_word_start_left, find_word_start_right,
};

/// Convert an action into a sequence of events that can be applied to the editor state
///
/// # Parameters
/// * `state` - The current editor state
/// * `action` - The action to convert
/// * `tab_size` - Number of spaces per tab
///
/// # Returns
/// * `Some(Vec<Event>)` - Events to apply for this action
/// * `None` - If the action doesn't generate events (like Quit, Save, etc.)
pub fn action_to_events(
    state: &EditorState,
    action: Action,
    tab_size: usize,
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

            for (cursor_id, cursor) in cursor_vec {
                // If there's a selection, delete it first
                if let Some(range) = cursor.selection_range() {
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                }

                // Insert the character
                events.push(Event::Insert {
                    position: cursor.position,
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
                if let Some(range) = cursor.selection_range() {
                    events.push(Event::Delete {
                        range: range.clone(),
                        deleted_text: state.buffer.slice(range),
                        cursor_id,
                    });
                }

                events.push(Event::Insert {
                    position: cursor.position,
                    text: "\n".to_string(),
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
                // Check if we're at the start of a line
                let mut iter = state.buffer.line_iterator(cursor.position);
                let line_start = iter.current_position();

                let new_pos = if cursor.position > 0 && cursor.position == line_start {
                    // At start of line - move to end of previous line (before its newline)
                    if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                        // Position at end of previous line content, not at the newline
                        let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                        prev_line_start + prev_line_len
                    } else {
                        // No previous line, just move left by 1
                        cursor.position.saturating_sub(1)
                    }
                } else {
                    // Normal left movement
                    cursor.position.saturating_sub(1)
                };

                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: None, // No selection
                });
            }
        }

        Action::MoveRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Check if we're at the end of a line (before the newline)
                let mut iter = state.buffer.line_iterator(cursor.position);
                if let Some((line_start, line_content)) = iter.next() {
                    let line_len_without_newline = line_content.trim_end_matches('\n').len();
                    let line_end_pos = line_start + line_len_without_newline;

                    // If we're at or past the end of visible content, and there's a newline
                    // skip directly to the start of the next line
                    let new_pos = if cursor.position >= line_end_pos && line_content.ends_with('\n') {
                        // Skip the newline - move to start of next line
                        line_start + line_content.len()
                    } else {
                        // Normal right movement
                        (cursor.position + 1).min(state.buffer.len())
                    };

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos.min(state.buffer.len()),
                        anchor: None,
                    });
                } else {
                    // Fallback: normal right movement
                    let new_pos = (cursor.position + 1).min(state.buffer.len());
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }
        }

        Action::MoveUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Use iterator to navigate to previous line
                // line_iterator positions us at the start of the current line
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;

                // Get previous line
                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    // Calculate length without trailing newline
                    let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                    let new_pos = prev_line_start + col_offset.min(prev_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
                    });
                }
            }
        }

        Action::MoveDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;

                // Skip current line, then get next line
                iter.next();
                if let Some((next_line_start, next_line_content)) = iter.next() {
                    // Calculate length without trailing newline
                    let next_line_len = next_line_content.trim_end_matches('\n').len();
                    let new_pos = next_line_start + col_offset.min(next_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: None,
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
                        position: line_start,
                        anchor: None,
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
                        position: line_end,
                        anchor: None,
                    });
                }
            }
        }

        Action::MoveWordLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_left(&state.buffer, cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: None,
                });
            }
        }

        Action::MoveWordRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: None,
                });
            }
        }

        Action::MoveDocumentStart => {
            for (cursor_id, _) in state.cursors.iter() {
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: 0,
                    anchor: None,
                });
            }
        }

        Action::MoveDocumentEnd => {
            for (cursor_id, _) in state.cursors.iter() {
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: state.buffer.len(),
                    anchor: None,
                });
            }
        }

        Action::MovePageUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Move up by viewport height
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.prev() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + col_offset.min(line_len);
                    } else {
                        new_pos = 0;
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: None,
                });
            }
        }

        Action::MovePageDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Move down by viewport height
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;

                // Consume current line
                iter.next();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + col_offset.min(line_len);
                    } else {
                        new_pos = state.buffer.len();
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: None,
                });
            }
        }

        // Selection movement - same as regular movement but keeps anchor
        Action::SelectLeft => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = cursor.position.saturating_sub(1);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = (cursor.position + 1).min(state.buffer.len());
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                if let Some((prev_line_start, prev_line_content)) = iter.prev() {
                    let prev_line_len = prev_line_content.trim_end_matches('\n').len();
                    let new_pos = prev_line_start + col_offset.min(prev_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
                    });
                }
            }
        }

        Action::SelectDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Skip current line, then get next line
                iter.next();
                if let Some((next_line_start, next_line_content)) = iter.next() {
                    let next_line_len = next_line_content.trim_end_matches('\n').len();
                    let new_pos = next_line_start + col_offset.min(next_line_len);

                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: new_pos,
                        anchor: Some(anchor),
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
                        position: line_start,
                        anchor: Some(anchor),
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
                        position: line_end,
                        anchor: Some(anchor),
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
                    position: new_pos,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectWordRight => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let new_pos = find_word_start_right(&state.buffer, cursor.position);
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectDocumentStart => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: 0,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectDocumentEnd => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let anchor = cursor.anchor.unwrap_or(cursor.position);
                events.push(Event::MoveCursor {
                    cursor_id,
                    position: state.buffer.len(),
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectPageUp => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.prev() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + col_offset.min(line_len);
                    } else {
                        new_pos = 0;
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectPageDown => {
            for (cursor_id, cursor) in state.cursors.iter() {
                let lines_to_move = state.viewport.height.saturating_sub(1);
                let mut iter = state.buffer.line_iterator(cursor.position);
                let current_line_start = iter.current_position();
                let col_offset = cursor.position - current_line_start;
                let anchor = cursor.anchor.unwrap_or(cursor.position);

                // Consume current line
                iter.next();

                let mut new_pos = cursor.position;
                for _ in 0..lines_to_move {
                    if let Some((line_start, line_content)) = iter.next() {
                        let line_len = line_content.trim_end_matches('\n').len();
                        new_pos = line_start + col_offset.min(line_len);
                    } else {
                        new_pos = state.buffer.len();
                        break;
                    }
                }

                events.push(Event::MoveCursor {
                    cursor_id,
                    position: new_pos,
                    anchor: Some(anchor),
                });
            }
        }

        Action::SelectAll => {
            // Select entire buffer for primary cursor only
            let primary_id = state.cursors.primary_id();
            events.push(Event::MoveCursor {
                cursor_id: primary_id,
                position: state.buffer.len(),
                anchor: Some(0),
            });
            // Note: RemoveSecondaryCursors is handled in handle_key, not as an event
        }

        Action::SelectWord => {
            for (cursor_id, cursor) in state.cursors.iter() {
                // Find word boundaries at current position
                let word_start = find_word_start(&state.buffer, cursor.position);
                let word_end = find_word_end(&state.buffer, cursor.position);

                if word_start < word_end {
                    events.push(Event::MoveCursor {
                        cursor_id,
                        position: word_end,
                        anchor: Some(word_start),
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
        | Action::PromptMoveLeft
        | Action::PromptMoveRight
        | Action::PromptMoveStart
        | Action::PromptMoveEnd
        | Action::PromptSelectPrev
        | Action::PromptSelectNext
        | Action::PromptPageUp
        | Action::PromptPageDown
        | Action::PromptAcceptSuggestion
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
                        position: line_end,
                        anchor: Some(line_start),
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
                        position: new_end,
                        anchor: Some(anchor),
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
                        position: final_end,
                        anchor: Some(final_start),
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
            position: 6,
            anchor: None,
        });

        assert_eq!(state.cursors.primary().position, 6);

        // Press Backspace - should delete the newline at position 5
        let events = action_to_events(&state, Action::DeleteBackward, 4).unwrap();
        println!("Generated events: {:?}", events);

        for event in events {
            state.apply(&event);
        }

        assert_eq!(state.buffer.to_string(), "HelloWorld");
        assert_eq!(state.cursors.primary().position, 5);
    }
}
