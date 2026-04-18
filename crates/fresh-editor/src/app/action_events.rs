//! Action -> Event conversion on `Editor`.
//!
//! `action_to_events` is the bridge between the Action enum (what a key
//! press *means* in editor terms) and the Event stream (what actually
//! gets applied to the active buffer). For movement actions on
//! soft-wrapped lines it routes through `handle_visual_line_movement`,
//! which walks the cached layout to translate visual-row movement into
//! the right buffer byte offset.

use crate::input::actions::action_to_events as convert_action_to_events;
use crate::input::keybindings::Action;
use crate::model::event::{Event, LeafId};

use super::Editor;

impl Editor {
    /// Convert an action into a list of events to apply to the active buffer
    /// Returns None for actions that don't generate events (like Quit)
    pub fn action_to_events(&mut self, action: Action) -> Option<Vec<Event>> {
        let auto_indent = self.config.editor.auto_indent;
        let estimated_line_length = self.config.editor.estimated_line_length;

        // Use the *effective* active split: when the user is focused on an
        // inner panel of a grouped buffer (e.g. a magit-style review panel),
        // its leaf id lives in `split_view_states` but is not in the main
        // split tree. `effective_active_split` returns that inner leaf, so
        // motion targets the panel's own buffer/cursors instead of the
        // group host's.
        let active_split = self.effective_active_split();
        let viewport_height = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.viewport.height)
            .unwrap_or(24);

        // Always try visual line movement first — it uses the cached layout to
        // move through soft-wrapped rows.  Returns None when the layout can't
        // resolve the movement, falling through to logical movement below.
        if let Some(events) =
            self.handle_visual_line_movement(&action, active_split, estimated_line_length)
        {
            return Some(events);
        }

        // Page motion: drive the viewport via the same view-line-aware
        // scroll primitive the ScrollUp/ScrollDown actions use, then land
        // the cursor at the new viewport top so it stays visible.
        //
        // `Viewport::scroll_down` / `scroll_up` already walk view rows in
        // wrap mode and logical lines in no-wrap mode, so one code path
        // handles both: the cursor advance matches whatever the viewport
        // just did, page-for-page.  Doing this here (rather than appending
        // an `Event::Scroll` to the normal MovePageDown MoveCursor event)
        // avoids a stall where the logical-line cursor move lands inside
        // the current viewport and `ensure_visible` has no reason to
        // scroll.
        if let Some(events) = self.handle_page_motion(&action, active_split, viewport_height) {
            return Some(events);
        }

        let buffer_id = self.active_buffer();
        let state = self.buffers.get_mut(&buffer_id).unwrap();

        // Use per-buffer settings which respect language overrides and user changes
        let tab_size = state.buffer_settings.tab_size;
        let auto_close = state.buffer_settings.auto_close;
        let auto_surround = state.buffer_settings.auto_surround;

        let cursors = &mut self
            .split_view_states
            .get_mut(&active_split)
            .unwrap()
            .cursors;
        convert_action_to_events(
            state,
            cursors,
            action,
            tab_size,
            auto_indent,
            auto_close,
            auto_surround,
            estimated_line_length,
            viewport_height,
        )
    }

    /// Handle PageUp/PageDown (and their select variants) by scrolling the
    /// viewport a page of view rows and landing the cursor at the new top.
    ///
    /// Returns `None` for non-page actions, or when the viewport couldn't
    /// scroll at all (buffer shorter than a page and already at the edge) —
    /// in that case the caller falls through to the default handler which
    /// still moves the cursor to the buffer boundary.
    fn handle_page_motion(
        &mut self,
        action: &Action,
        split_id: LeafId,
        viewport_height: u16,
    ) -> Option<Vec<Event>> {
        let (direction, is_select) = match action {
            Action::MovePageDown => (1isize, false),
            Action::MovePageUp => (-1isize, false),
            Action::SelectPageDown => (1isize, true),
            Action::SelectPageUp => (-1isize, true),
            _ => return None,
        };

        let delta = (viewport_height as isize).saturating_sub(1).max(1) * direction;

        let old_top_byte = self.split_view_states.get(&split_id)?.viewport.top_byte;
        self.handle_scroll_event(delta);
        let new_top_byte = self.split_view_states.get(&split_id)?.viewport.top_byte;

        if new_top_byte == old_top_byte {
            // Viewport couldn't move (already at top/bottom of buffer).  Fall
            // back to the default cursor-only handler so PageDown at EOF
            // still clamps the cursor to the last line (and PageUp at BOF
            // clamps to byte 0), matching the historical behaviour.
            return None;
        }

        // Emit a MoveCursor event placing each cursor at the new viewport
        // top.  The cursor is guaranteed visible (it's at row 0 of the new
        // viewport) and each press advances by exactly a full page of view
        // rows — the same way it does when line wrap is off.
        let cursors = &self.split_view_states.get(&split_id)?.cursors;
        let events: Vec<Event> = cursors
            .iter()
            .map(|(cursor_id, cursor)| {
                let new_anchor = if is_select {
                    Some(cursor.anchor.unwrap_or(cursor.position))
                } else if cursor.deselect_on_move {
                    None
                } else {
                    cursor.anchor
                };
                Event::MoveCursor {
                    cursor_id,
                    old_position: cursor.position,
                    new_position: new_top_byte,
                    old_anchor: cursor.anchor,
                    new_anchor,
                    old_sticky_column: cursor.sticky_column,
                    new_sticky_column: cursor.sticky_column,
                }
            })
            .collect();

        Some(events)
    }

    /// Handle visual line movement actions using the cached layout
    /// Returns Some(events) if the action was handled, None if it should fall through
    fn handle_visual_line_movement(
        &mut self,
        action: &Action,
        split_id: LeafId,
        _estimated_line_length: usize,
    ) -> Option<Vec<Event>> {
        // Classify the action
        enum VisualAction {
            UpDown { direction: i8, is_select: bool },
            LineEnd { is_select: bool },
            LineStart { is_select: bool },
        }

        // Note: We don't intercept BlockSelectUp/Down because block selection has
        // special semantics (setting block_anchor) that require the default handler
        let visual_action = match action {
            Action::MoveUp => VisualAction::UpDown {
                direction: -1,
                is_select: false,
            },
            Action::MoveDown => VisualAction::UpDown {
                direction: 1,
                is_select: false,
            },
            Action::SelectUp => VisualAction::UpDown {
                direction: -1,
                is_select: true,
            },
            Action::SelectDown => VisualAction::UpDown {
                direction: 1,
                is_select: true,
            },
            // When line wrapping is off, Home/End should move to the physical line
            // start/end, not the visual (horizontally-scrolled) row boundary.
            // Fall through to the standard handler which uses line_iterator.
            Action::MoveLineEnd if self.config.editor.line_wrap => {
                VisualAction::LineEnd { is_select: false }
            }
            Action::SelectLineEnd if self.config.editor.line_wrap => {
                VisualAction::LineEnd { is_select: true }
            }
            Action::MoveLineStart if self.config.editor.line_wrap => {
                VisualAction::LineStart { is_select: false }
            }
            Action::SelectLineStart if self.config.editor.line_wrap => {
                VisualAction::LineStart { is_select: true }
            }
            _ => return None, // Not a visual line action
        };

        // First, collect cursor data we need (to avoid borrow conflicts).
        // Use the *effective* active split + buffer so that cursor motion in
        // a focused buffer-group panel reads the panel's own cursors and
        // buffer instead of the group host's.
        let cursor_data: Vec<_> = {
            let active_split = self.effective_active_split();
            let active_buffer = self.active_buffer();
            let cursors = &self.split_view_states.get(&active_split).unwrap().cursors;
            let state = self.buffers.get(&active_buffer).unwrap();
            cursors
                .iter()
                .map(|(cursor_id, cursor)| {
                    // Check if cursor is at a physical line boundary:
                    // - at_line_ending: byte at cursor position is a newline or at buffer end
                    // - at_line_start: cursor is at position 0 or preceded by a newline
                    let at_line_ending = if cursor.position < state.buffer.len() {
                        let bytes = state
                            .buffer
                            .slice_bytes(cursor.position..cursor.position + 1);
                        bytes.first() == Some(&b'\n') || bytes.first() == Some(&b'\r')
                    } else {
                        true // end of buffer is a boundary
                    };
                    let at_line_start = if cursor.position == 0 {
                        true
                    } else {
                        let prev = state
                            .buffer
                            .slice_bytes(cursor.position - 1..cursor.position);
                        prev.first() == Some(&b'\n')
                    };
                    (
                        cursor_id,
                        cursor.position,
                        cursor.anchor,
                        cursor.sticky_column,
                        cursor.deselect_on_move,
                        at_line_ending,
                        at_line_start,
                    )
                })
                .collect()
        };

        let mut events = Vec::new();

        for (
            cursor_id,
            position,
            anchor,
            sticky_column,
            deselect_on_move,
            at_line_ending,
            at_line_start,
        ) in cursor_data
        {
            let (new_pos, new_sticky) = match &visual_action {
                VisualAction::UpDown {
                    direction,
                    is_select,
                } => {
                    // When a selection is active, plain (non-selecting) vertical
                    // motion starts from the selection's edge closest to the
                    // motion direction (top edge for Up, bottom edge for Down),
                    // matching VSCode/Sublime/browser behavior (issue #1566).
                    // Emacs mark-mode (`deselect_on_move == false`) is unaffected.
                    let from_pos = if deselect_on_move && !*is_select {
                        if let Some(anchor) = anchor {
                            if *direction < 0 {
                                position.min(anchor)
                            } else {
                                position.max(anchor)
                            }
                        } else {
                            position
                        }
                    } else {
                        position
                    };

                    // Calculate current visual column from cached layout
                    let current_visual_col = self
                        .cached_layout
                        .byte_to_visual_column(split_id, from_pos)?;

                    let goal_visual_col = if sticky_column > 0 {
                        sticky_column
                    } else {
                        current_visual_col
                    };

                    match self.cached_layout.move_visual_line(
                        split_id,
                        from_pos,
                        goal_visual_col,
                        *direction,
                    ) {
                        Some(result) => result,
                        None => {
                            // Target visual row is past the cached view-line
                            // mappings — the destination row isn't in the
                            // currently-rendered viewport slice.  In wrap mode
                            // that means the next visual row belongs to a
                            // logical line (or wrapped segment) that is
                            // off-screen.  Compute its position directly from
                            // the buffer + wrap config so we don't fall
                            // through to the byte-based MoveDown handler,
                            // which would treat `goal_visual_col` as a
                            // *logical* column on the whole next logical
                            // line and teleport the cursor deep into a
                            // wrapped paragraph (issue #1574, jump variant).
                            match self.compute_wrap_aware_visual_move_fallback(
                                from_pos,
                                goal_visual_col,
                                *direction,
                                _estimated_line_length,
                            ) {
                                Some(result) => result,
                                None => continue, // Genuinely at buffer boundary
                            }
                        }
                    }
                }
                VisualAction::LineEnd { .. } => {
                    // Allow advancing to next visual segment only if not at a physical line ending
                    let allow_advance = !at_line_ending;
                    match self
                        .cached_layout
                        .visual_line_end(split_id, position, allow_advance)
                    {
                        Some(end_pos) => (end_pos, 0),
                        None => return None,
                    }
                }
                VisualAction::LineStart { .. } => {
                    // Allow advancing to previous visual segment only if not at a physical line start
                    let allow_advance = !at_line_start;
                    match self
                        .cached_layout
                        .visual_line_start(split_id, position, allow_advance)
                    {
                        Some(start_pos) => (start_pos, 0),
                        None => return None,
                    }
                }
            };

            let is_select = match &visual_action {
                VisualAction::UpDown { is_select, .. } => *is_select,
                VisualAction::LineEnd { is_select } => *is_select,
                VisualAction::LineStart { is_select } => *is_select,
            };

            let new_anchor = if is_select {
                Some(anchor.unwrap_or(position))
            } else if deselect_on_move {
                None
            } else {
                anchor
            };

            events.push(Event::MoveCursor {
                cursor_id,
                old_position: position,
                new_position: new_pos,
                old_anchor: anchor,
                new_anchor,
                old_sticky_column: sticky_column,
                new_sticky_column: new_sticky,
            });
        }

        if events.is_empty() {
            None // Let the default handler deal with it
        } else {
            Some(events)
        }
    }

    /// Compute a wrap-aware target position when the cached view-line
    /// mappings don't cover the requested direction.
    ///
    /// `move_visual_line` returns `None` when the target visual row is
    /// past the currently-rendered viewport — typically because the
    /// destination line wraps off-screen below (for Down) or above (for
    /// Up).  The generic MoveDown/MoveUp fallback that normally kicks in
    /// when the intercept returns None treats `goal_visual_col` as a
    /// column on the whole next logical line, which is wrong for wrap
    /// mode: if the next logical line is a long wrapped paragraph, the
    /// cursor lands several visual rows deep (issue #1574, jump variant).
    ///
    /// This helper uses the current row's `line_end_byte` (which the
    /// cached layout does know) to find the byte position just past the
    /// current visual row, and lands the cursor at the *start* of the
    /// next visual row.  That's conservative (the sticky visual column
    /// from the previous row isn't preserved across an off-screen jump)
    /// but it reliably places the cursor on the first visual row of
    /// the next logical line / wrapped segment instead of somewhere
    /// deep inside it.  Preserving sticky precisely when the target row
    /// is off-screen would require re-running the full token-based
    /// wrapping pipeline for the target line, which the editor doesn't
    /// currently expose outside of the render pipeline.
    ///
    /// Returns `Some((new_position, new_sticky))` on success, or `None`
    /// if wrap mode is off (delegate to caller default) or we're at a
    /// genuine buffer boundary.
    fn compute_wrap_aware_visual_move_fallback(
        &mut self,
        from_pos: usize,
        goal_visual_col: usize,
        direction: i8,
        estimated_line_length: usize,
    ) -> Option<(usize, usize)> {
        if !self.config.editor.line_wrap {
            // Non-wrap mode: the byte-based fallback is correct, let it run.
            return None;
        }

        let active_split = self.effective_active_split();
        let active_buffer = self.active_buffer();

        if direction > 0 {
            // Find current row's end byte via cached layout — this is the
            // authoritative "end of current visual row" position that the
            // renderer itself uses.
            let cur_row_line_end = {
                let mappings = self.cached_layout.view_line_mappings.get(&active_split)?;
                let row_idx = self.cached_layout.find_visual_row(active_split, from_pos)?;
                mappings.get(row_idx)?.line_end_byte
            };

            let state = self.buffers.get_mut(&active_buffer)?;
            let buffer = &mut state.buffer;
            let buffer_len = buffer.len();
            if cur_row_line_end >= buffer_len {
                return None; // Genuine end of buffer
            }

            // Step past the newline at `cur_row_line_end`, mirroring the
            // tokenization logic in `build_base_tokens`: CRLF (`\r\n`) is a
            // SINGLE logical line break and the next logical line starts two
            // bytes past the `\r`, not one.  Falling back to `+ 1` lands the
            // cursor on the `\n` inside the CRLF pair, which
            // `find_view_line_for_byte` resolves back to the SAME row — so
            // pressing Down from an empty separator line on a CRLF file
            // appears to jump the cursor to the wrong visual row (issue
            // #1574, Windows-CRLF variant).  When `cur_row_line_end` isn't a
            // newline the current row is a wrapped continuation and the
            // next visual row starts at the same byte position.
            let target_pos = step_past_line_break(buffer, cur_row_line_end, buffer_len);
            if target_pos > buffer_len {
                return None;
            }

            // Preserve goal_visual_col as the new sticky column so if the
            // user keeps pressing Down the normal cached-layout path will
            // honor it once the target row is rendered.
            let _ = estimated_line_length;
            Some((target_pos, goal_visual_col))
        } else {
            // Up-direction fallback: mirror the Down logic.  Use the
            // cached layout to locate the current visual row's "anchor"
            // byte (the row start for rows with visible content, or
            // `line_end_byte` for empty rows which have no source
            // mapping), then step back one byte so the cursor lands on
            // the *end* of the preceding visual row.
            //
            // For a row whose start is a logical-line-start, stepping
            // back one byte lands on the trailing newline of the
            // previous logical line — the renderer shows this as the
            // end of the last visual row of that line, which is exactly
            // where the cursor should land when walking Up.
            //
            // For a wrapped continuation row, the "start" is already a
            // byte within the same logical line; stepping back one byte
            // keeps us inside the line on the previous wrapped segment.
            //
            // For empty rows (no char_source_bytes, common at paragraph
            // separators), `line_end_byte` is the empty line's newline;
            // stepping back one byte lands on the previous line's
            // trailing newline — again the end of its last visual row.
            let (cur_row_anchor, row_is_empty) = {
                let mappings = self.cached_layout.view_line_mappings.get(&active_split)?;
                let row_idx = self.cached_layout.find_visual_row(active_split, from_pos)?;
                let row = mappings.get(row_idx)?;
                match row.char_source_bytes.iter().find_map(|b| *b) {
                    Some(start) => (start, false),
                    None => (row.line_end_byte, true),
                }
            };

            if cur_row_anchor == 0 {
                return None; // At the very beginning of the buffer
            }

            // Step back across the newline preceding `cur_row_anchor`,
            // mirroring the tokenization logic in `build_base_tokens`:
            // CRLF is a SINGLE logical line break so we must step back
            // two bytes over it, not one.  Blindly subtracting 1 on a
            // CRLF file lands the cursor on the `\n` INSIDE the CRLF
            // pair, which `find_view_line_for_byte` resolves to a row
            // the user wouldn't expect (issue #1574, Windows-CRLF
            // variant).  For LF or a lone CR the byte arithmetic falls
            // through to a one-byte step.
            let state = self.buffers.get_mut(&active_buffer)?;
            let buffer = &mut state.buffer;
            let _ = row_is_empty;
            let target_pos = step_before_line_break(buffer, cur_row_anchor);
            let _ = estimated_line_length;
            Some((target_pos, goal_visual_col))
        }
    }
}

/// Advance past the line break at `pos`, matching the CRLF handling in
/// `build_base_tokens` (where `\r\n` is a single logical line break
/// represented by one `Newline` token at the `\r`).  When `pos` is on a
/// `\r` immediately followed by `\n` we step two bytes; on a lone `\n`
/// or `\r` we step one; otherwise (`pos` isn't on a newline, i.e. a
/// wrapped-continuation boundary) we return `pos` unchanged so the next
/// visual row starts at the same byte.  Without this, pressing Down
/// across a CRLF newline lands the cursor on the `\n` inside the pair,
/// which `find_view_line_for_byte` resolves back to the *same* row
/// (issue #1574, Windows-CRLF variant).
fn step_past_line_break(
    buffer: &crate::model::buffer::Buffer,
    pos: usize,
    buffer_len: usize,
) -> usize {
    if pos >= buffer_len {
        return pos;
    }
    let end = (pos + 2).min(buffer_len);
    let bytes = buffer.slice_bytes(pos..end);
    match (bytes.first(), bytes.get(1)) {
        (Some(b'\r'), Some(b'\n')) => pos + 2,
        (Some(b'\r'), _) | (Some(b'\n'), _) => pos + 1,
        _ => pos,
    }
}

/// Step back across the line break immediately preceding `pos`, mirror
/// of [`step_past_line_break`].  Two bytes for CRLF (`\r\n`), one for
/// LF or a lone CR, zero if `pos == 0`.  Callers use this to land the
/// cursor at the *end* of the previous visual row when moving Up across
/// a newline — landing mid-CRLF would place the cursor on the `\n` and
/// re-resolve to the same row (issue #1574, Windows-CRLF variant).
fn step_before_line_break(buffer: &crate::model::buffer::Buffer, pos: usize) -> usize {
    if pos == 0 {
        return pos;
    }
    if pos >= 2 {
        let bytes = buffer.slice_bytes((pos - 2)..pos);
        if bytes.first() == Some(&b'\r') && bytes.get(1) == Some(&b'\n') {
            return pos - 2;
        }
    }
    pos - 1
}
