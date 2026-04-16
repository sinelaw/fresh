//! Text-manipulation orchestrators on `Editor`.
//!
//! Smart-home, comment toggling, bracket matching — operations that read
//! cursor + buffer state, compute a target position or edit, and apply
//! events to the active buffer. Pure decision logic for smart-home lives
//! in `super::smart_home`; these methods are the cross-cutting drivers.

use rust_i18n::t;

use crate::model::event::{Event, LeafId};

use super::Editor;

impl Editor {
    /// Smart home: toggle between line start and first non-whitespace character
    pub(super) fn smart_home(&mut self) {
        let estimated_line_length = self.config.editor.estimated_line_length;
        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();

        // When line wrap is on, use the visual (soft-wrapped) line boundaries
        if self.config.editor.line_wrap {
            let split_id = self.split_manager.active_split();
            if let Some(new_pos) =
                self.smart_home_visual_line(split_id, cursor.position, estimated_line_length)
            {
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_pos,
                    old_anchor: cursor.anchor,
                    new_anchor: None,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: 0,
                };
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);
                return;
            }
            // Fall through to physical line logic if visual lookup fails
        }

        let state = self.active_state_mut();

        // Get physical line information
        let mut iter = state
            .buffer
            .line_iterator(cursor.position, estimated_line_length);
        if let Some((line_start, line_content)) = iter.next_line() {
            // Find first non-whitespace character
            let first_non_ws = line_content
                .chars()
                .take_while(|c| *c != '\n')
                .position(|c| !c.is_whitespace())
                .map(|offset| line_start + offset)
                .unwrap_or(line_start);

            // Toggle: if at first non-ws, go to line start; otherwise go to first non-ws
            let new_pos = if cursor.position == first_non_ws {
                line_start
            } else {
                first_non_ws
            };

            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };

            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        }
    }

    /// Compute the smart-home target for a visual (soft-wrapped) line.
    ///
    /// Adapter around [`super::smart_home::smart_home_target`]: fetches the
    /// visual-row boundaries and the first non-whitespace offset, then
    /// delegates the decision. When the pure helper returns
    /// [`SmartHomeTarget::PreviousVisualRowStart`] the caller issues the
    /// extra layout lookup here.
    fn smart_home_visual_line(
        &mut self,
        split_id: LeafId,
        cursor_pos: usize,
        estimated_line_length: usize,
    ) -> Option<usize> {
        use super::smart_home::{smart_home_target, SmartHomeTarget};

        let visual_start = self
            .cached_layout
            .visual_line_start(split_id, cursor_pos, false)?;

        // Determine the physical line start to tell first-row from continuation.
        let buffer_id = self.split_manager.active_buffer_id()?;
        let state = self.buffers.get_mut(&buffer_id)?;
        let mut iter = state
            .buffer
            .line_iterator(visual_start, estimated_line_length);
        let (phys_line_start, content) = iter.next_line()?;

        let is_first_visual_row = visual_start == phys_line_start;

        // The first_non_ws offset is only meaningful on the first visual row;
        // compute it eagerly anyway so the pure helper stays unconditional.
        let first_non_ws = if is_first_visual_row {
            let visual_end = self
                .cached_layout
                .visual_line_end(split_id, cursor_pos, false)
                .unwrap_or(visual_start);
            let visual_len = visual_end.saturating_sub(visual_start);
            content
                .chars()
                .take(visual_len)
                .take_while(|c| *c != '\n')
                .position(|c| !c.is_whitespace())
                .map(|offset| visual_start + offset)
                .unwrap_or(visual_start)
        } else {
            visual_start
        };

        match smart_home_target(cursor_pos, visual_start, is_first_visual_row, first_non_ws) {
            SmartHomeTarget::At(pos) => Some(pos),
            SmartHomeTarget::PreviousVisualRowStart => self
                .cached_layout
                .visual_line_start(split_id, cursor_pos, true),
        }
    }

    /// Toggle comment on the current line or selection
    pub(super) fn toggle_comment(&mut self) {
        // Determine comment prefix from language config
        // If no language detected or no comment prefix configured, do nothing
        let language = &self.active_state().language;
        let comment_prefix = self
            .config
            .languages
            .get(language)
            .and_then(|lang_config| lang_config.comment_prefix.clone());

        let comment_prefix: String = match comment_prefix {
            Some(prefix) => {
                // Ensure there's a trailing space for consistent formatting
                if prefix.ends_with(' ') {
                    prefix
                } else {
                    format!("{} ", prefix)
                }
            }
            None => return, // No comment prefix for this language, do nothing
        };

        let estimated_line_length = self.config.editor.estimated_line_length;

        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();
        let state = self.active_state_mut();

        // Save original selection info to restore after edit
        let original_anchor = cursor.anchor;
        let original_position = cursor.position;
        let had_selection = original_anchor.is_some();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            let iter = state
                .buffer
                .line_iterator(cursor.position, estimated_line_length);
            let line_start = iter.current_position();
            (line_start, cursor.position)
        };

        // Find all line starts in the range
        let buffer_len = state.buffer.len();
        let mut line_starts = Vec::new();
        let mut iter = state.buffer.line_iterator(start_pos, estimated_line_length);
        let mut current_pos = iter.current_position();
        line_starts.push(current_pos);

        while let Some((_, content)) = iter.next_line() {
            current_pos += content.len();
            if current_pos >= end_pos || current_pos >= buffer_len {
                break;
            }
            let next_iter = state
                .buffer
                .line_iterator(current_pos, estimated_line_length);
            let next_start = next_iter.current_position();
            if next_start != *line_starts.last().unwrap() {
                line_starts.push(next_start);
            }
            iter = state
                .buffer
                .line_iterator(current_pos, estimated_line_length);
        }

        // Determine if we should comment or uncomment
        // If all lines are commented, uncomment; otherwise comment
        let all_commented = line_starts.iter().all(|&line_start| {
            let line_bytes = state
                .buffer
                .slice_bytes(line_start..buffer_len.min(line_start + comment_prefix.len() + 10));
            let line_str = String::from_utf8_lossy(&line_bytes);
            let trimmed = line_str.trim_start();
            trimmed.starts_with(comment_prefix.trim())
        });

        let mut events = Vec::new();
        // Track (edit_position, byte_delta) for calculating new cursor positions
        // delta is positive for insertions, negative for deletions
        let mut position_deltas: Vec<(usize, isize)> = Vec::new();

        if all_commented {
            // Uncomment: remove comment prefix from each line
            for &line_start in line_starts.iter().rev() {
                let line_bytes = state
                    .buffer
                    .slice_bytes(line_start..buffer_len.min(line_start + 100));
                let line_str = String::from_utf8_lossy(&line_bytes);

                // Find where the comment prefix starts (after leading whitespace)
                let leading_ws: usize = line_str
                    .chars()
                    .take_while(|c| c.is_whitespace() && *c != '\n')
                    .map(|c| c.len_utf8())
                    .sum();
                let rest = &line_str[leading_ws..];

                if rest.starts_with(comment_prefix.trim()) {
                    let remove_len = if rest.starts_with(&comment_prefix) {
                        comment_prefix.len()
                    } else {
                        comment_prefix.trim().len()
                    };
                    let deleted_text = String::from_utf8_lossy(&state.buffer.slice_bytes(
                        line_start + leading_ws..line_start + leading_ws + remove_len,
                    ))
                    .to_string();
                    events.push(Event::Delete {
                        range: (line_start + leading_ws)..(line_start + leading_ws + remove_len),
                        deleted_text,
                        cursor_id,
                    });
                    position_deltas.push((line_start, -(remove_len as isize)));
                }
            }
        } else {
            // Comment: add comment prefix to each line
            let prefix_len = comment_prefix.len();
            for &line_start in line_starts.iter().rev() {
                events.push(Event::Insert {
                    position: line_start,
                    text: comment_prefix.to_string(),
                    cursor_id,
                });
                position_deltas.push((line_start, prefix_len as isize));
            }
        }

        if events.is_empty() {
            return;
        }

        let action_desc = if all_commented {
            "Uncomment"
        } else {
            "Comment"
        };

        // If there was a selection, add a MoveCursor event to restore it
        if had_selection {
            // Sort deltas by position ascending for calculation
            position_deltas.sort_by_key(|(pos, _)| *pos);

            // Calculate cumulative shift for a position based on edits at or before that position
            let calc_shift = |original_pos: usize| -> isize {
                let mut shift: isize = 0;
                for (edit_pos, delta) in &position_deltas {
                    if *edit_pos < original_pos {
                        shift += delta;
                    }
                }
                shift
            };

            let anchor_shift = calc_shift(original_anchor.unwrap_or(0));
            let position_shift = calc_shift(original_position);

            let new_anchor = (original_anchor.unwrap_or(0) as isize + anchor_shift).max(0) as usize;
            let new_position = (original_position as isize + position_shift).max(0) as usize;

            events.push(Event::MoveCursor {
                cursor_id,
                old_position: original_position,
                new_position,
                old_anchor: original_anchor,
                new_anchor: Some(new_anchor),
                old_sticky_column: 0,
                new_sticky_column: 0,
            });
        }

        // Use optimized bulk edit for multi-line comment toggle
        let description = format!("{} lines", action_desc);
        if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
            self.active_event_log_mut().append(bulk_edit);
        }

        self.set_status_message(
            t!(
                "lines.action",
                action = action_desc,
                count = line_starts.len()
            )
            .to_string(),
        );
    }

    /// Go to matching bracket
    pub(super) fn goto_matching_bracket(&mut self) {
        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();
        let state = self.active_state_mut();

        let pos = cursor.position;
        if pos >= state.buffer.len() {
            self.set_status_message(t!("diagnostics.bracket_none").to_string());
            return;
        }

        let bytes = state.buffer.slice_bytes(pos..pos + 1);
        if bytes.is_empty() {
            self.set_status_message(t!("diagnostics.bracket_none").to_string());
            return;
        }

        let ch = bytes[0] as char;

        // All supported bracket pairs
        const BRACKET_PAIRS: &[(char, char)] = &[('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')];

        let bracket_info = match ch {
            '(' => Some(('(', ')', true)),
            ')' => Some(('(', ')', false)),
            '[' => Some(('[', ']', true)),
            ']' => Some(('[', ']', false)),
            '{' => Some(('{', '}', true)),
            '}' => Some(('{', '}', false)),
            '<' => Some(('<', '>', true)),
            '>' => Some(('<', '>', false)),
            _ => None,
        };

        // Limit searches to avoid O(n) scans on huge files.
        use crate::view::bracket_highlight_overlay::MAX_BRACKET_SEARCH_BYTES;

        // If cursor is not on a bracket, search backward for the nearest
        // enclosing opening bracket, then jump to its matching close.
        let (opening, closing, search_start, forward) =
            if let Some((opening, closing, forward)) = bracket_info {
                (opening, closing, pos, forward)
            } else {
                // Search backward from cursor to find enclosing opening bracket.
                // Track depth per bracket type to handle nesting correctly.
                let mut depths: Vec<i32> = vec![0; BRACKET_PAIRS.len()];
                let mut found = None;
                let search_limit = pos.saturating_sub(MAX_BRACKET_SEARCH_BYTES);
                let mut search_pos = pos.saturating_sub(1);
                loop {
                    let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                    if !b.is_empty() {
                        let c = b[0] as char;
                        for (i, &(open, close)) in BRACKET_PAIRS.iter().enumerate() {
                            if c == close {
                                depths[i] += 1;
                            } else if c == open {
                                if depths[i] > 0 {
                                    depths[i] -= 1;
                                } else {
                                    // Found an unmatched opening bracket — this encloses us
                                    found = Some((open, close, search_pos));
                                    break;
                                }
                            }
                        }
                        if found.is_some() {
                            break;
                        }
                    }
                    if search_pos <= search_limit {
                        break;
                    }
                    search_pos -= 1;
                }

                if let Some((opening, closing, bracket_pos)) = found {
                    // Jump forward from the enclosing opening bracket to its match
                    (opening, closing, bracket_pos, true)
                } else {
                    self.set_status_message(t!("diagnostics.bracket_none").to_string());
                    return;
                }
            };

        // Find matching bracket (bounded to MAX_BRACKET_SEARCH_BYTES)
        let buffer_len = state.buffer.len();
        let mut depth = 1;
        let matching_pos = if forward {
            let search_limit = (search_start + 1 + MAX_BRACKET_SEARCH_BYTES).min(buffer_len);
            let mut search_pos = search_start + 1;
            let mut found = None;
            while search_pos < search_limit && depth > 0 {
                let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                if !b.is_empty() {
                    let c = b[0] as char;
                    if c == opening {
                        depth += 1;
                    } else if c == closing {
                        depth -= 1;
                        if depth == 0 {
                            found = Some(search_pos);
                        }
                    }
                }
                search_pos += 1;
            }
            found
        } else {
            let search_limit = search_start.saturating_sub(MAX_BRACKET_SEARCH_BYTES);
            let mut search_pos = search_start.saturating_sub(1);
            let mut found = None;
            loop {
                let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                if !b.is_empty() {
                    let c = b[0] as char;
                    if c == closing {
                        depth += 1;
                    } else if c == opening {
                        depth -= 1;
                        if depth == 0 {
                            found = Some(search_pos);
                            break;
                        }
                    }
                }
                if search_pos <= search_limit {
                    break;
                }
                search_pos -= 1;
            }
            found
        };

        if let Some(new_pos) = matching_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        } else {
            self.set_status_message(t!("diagnostics.bracket_no_match").to_string());
        }
    }

}
