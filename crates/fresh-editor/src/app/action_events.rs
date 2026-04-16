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
                        None => continue, // At boundary, skip this cursor
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
}
