//! Event application orchestrators on `Editor`.
//!
//! Every buffer mutation in this editor flows through one of:
//!
//! - `log_and_apply_event` — the canonical single-event path that logs
//!   to the EventLog and applies the event.
//! - `apply_event_to_active_buffer` — apply without logging, used by
//!   replay paths.
//! - `apply_events_as_bulk_edit` — batched multi-event application
//!   under one undo boundary, used by replace-all, format-on-save, etc.
//! - `trigger_plugin_hooks_for_event` — broadcast hook notifications
//!   to plugins after an event applies.
//!
//! Plus three "scroll/viewport event" handlers that bypass the buffer
//! entirely: handle_scroll_event, handle_set_viewport_event,
//! handle_recenter_event. And a small invalidate_layouts_for_buffer
//! helper.

use lsp_types::TextDocumentContentChangeEvent;

use crate::model::event::{BufferId, Event, LeafId};

use super::types::EventLineInfo;
use super::Editor;

impl Editor {
    /// All event applications MUST go through this method to ensure consistency.
    /// Log an event and apply it to the active buffer.
    /// For Delete events, captures displaced marker positions before applying
    /// so undo can restore them to their exact original positions.
    pub fn log_and_apply_event(&mut self, event: &Event) {
        // Capture displaced markers before the event is applied
        if let Event::Delete { range, .. } = event {
            let displaced = self.active_state().capture_displaced_markers(range);
            self.active_event_log_mut().append(event.clone());
            if !displaced.is_empty() {
                self.active_event_log_mut()
                    .set_displaced_markers_on_last(displaced);
            }
        } else {
            self.active_event_log_mut().append(event.clone());
        }
        self.apply_event_to_active_buffer(event);
    }

    pub fn apply_event_to_active_buffer(&mut self, event: &Event) {
        // Handle View events at Editor level - View events go to SplitViewState, not EditorState
        // This properly separates Buffer state from View state
        match event {
            Event::Scroll { line_offset } => {
                self.handle_scroll_event(*line_offset);
                return;
            }
            Event::SetViewport { top_line } => {
                self.handle_set_viewport_event(*top_line);
                return;
            }
            Event::Recenter => {
                self.handle_recenter_event();
                return;
            }
            _ => {}
        }

        // Any buffer-modifying event commits the user to this file, so promote
        // it out of preview mode. Cursor moves and view-only events don't
        // count — only real edits (Insert / Delete / BulkEdit, or a Batch
        // containing any of those) flip the bit. Placed here (rather than
        // in `log_and_apply_event`) because several edit paths bypass
        // logging and call `apply_event_to_active_buffer` directly — notably
        // `InsertChar` (single-character typing).
        if event.modifies_buffer() {
            self.promote_active_buffer_from_preview();
        }

        // IMPORTANT: Calculate LSP changes and line info BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer,
        // so we must convert them to LSP positions before modifying the buffer.
        let lsp_changes = self.collect_lsp_changes(event);

        // Calculate line info for plugin hooks (using same pre-modification buffer state)
        let line_info = self.calculate_event_line_info(event);

        // 1. Apply the event to the buffer
        // Borrow cursors from SplitViewState (sole source of truth) and state from buffers.
        //
        // Use the *effective* active split so events targeting a focused
        // buffer-group panel land in the panel's own split view state, not
        // the group host's. Without this, MoveCursor events for a focused
        // panel would try to look up the panel buffer's keyed state in the
        // host split (which doesn't have it) and panic on unwrap.
        {
            let split_id = self.effective_active_split();
            let active_buf = self.active_buffer();
            let cursors = &mut self
                .split_view_states
                .get_mut(&split_id)
                .unwrap()
                .keyed_states
                .get_mut(&active_buf)
                .unwrap()
                .cursors;
            let state = self.buffers.get_mut(&active_buf).unwrap();
            state.apply(cursors, event);
        }

        // 1c. Invalidate layouts for all views of this buffer after content changes
        // Note: recovery_pending is set automatically by the buffer on edits
        match event {
            Event::Insert { .. } | Event::Delete { .. } | Event::BulkEdit { .. } => {
                self.invalidate_layouts_for_buffer(self.active_buffer());
                self.schedule_semantic_tokens_full_refresh(self.active_buffer());
                self.schedule_folding_ranges_refresh(self.active_buffer());
            }
            Event::Batch { events, .. } => {
                let has_edits = events
                    .iter()
                    .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));
                if has_edits {
                    self.invalidate_layouts_for_buffer(self.active_buffer());
                    self.schedule_semantic_tokens_full_refresh(self.active_buffer());
                    self.schedule_folding_ranges_refresh(self.active_buffer());
                }
            }
            _ => {}
        }

        // 2. Adjust cursors in other splits that share the same buffer
        self.adjust_other_split_cursors_for_event(event);

        // 3. Clear search highlights on edit (Insert/Delete events)
        // This preserves highlights while navigating but clears them when modifying text
        // EXCEPT during interactive replace where we want to keep highlights visible
        let in_interactive_replace = self.interactive_replace_state.is_some();

        // Note: We intentionally do NOT clear search overlays on buffer modification.
        // Overlays have markers that automatically track position changes through edits,
        // which allows F3/Shift+F3 to find matches at their updated positions.
        // The visual highlights may be on text that no longer matches the query,
        // but that's acceptable - user can see where original matches were.
        let _ = in_interactive_replace; // silence unused warning

        // 3. Trigger plugin hooks for this event (with pre-calculated line info)
        self.trigger_plugin_hooks_for_event(event, line_info);

        // 4. Notify LSP of the change using pre-calculated positions
        // For BulkEdit events (undo/redo of code actions, renames, etc.),
        // collect_lsp_changes returns empty because there are no incremental byte
        // positions to convert — BulkEdit restores a tree snapshot.  Send a
        // full-document replacement so the LSP server stays in sync.
        if lsp_changes.is_empty() && event.modifies_buffer() {
            if let Some(full_text) = self.active_state().buffer.to_string() {
                let full_change = vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: full_text,
                }];
                self.send_lsp_changes_for_buffer(self.active_buffer(), full_change);
            }
        } else {
            self.send_lsp_changes_for_buffer(self.active_buffer(), lsp_changes);
        }
    }

    /// Apply multiple Insert/Delete events efficiently using bulk edit optimization.
    ///
    /// This avoids O(n²) complexity by:
    /// 1. Converting events to (position, delete_len, insert_text) tuples
    /// 2. Applying all edits in a single tree pass via apply_bulk_edits
    /// 3. Creating a BulkEdit event for undo (stores tree snapshot via Arc clone = O(1))
    ///
    /// # Arguments
    /// * `events` - Vec of Insert/Delete events (sorted by position descending for correct application)
    /// * `description` - Description for the undo log
    ///
    /// # Returns
    /// The BulkEdit event that was applied, for tracking purposes
    pub fn apply_events_as_bulk_edit(
        &mut self,
        events: Vec<Event>,
        description: String,
    ) -> Option<Event> {
        use crate::model::event::CursorId;

        // Check if any events modify the buffer
        let has_buffer_mods = events
            .iter()
            .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));

        if !has_buffer_mods {
            // No buffer modifications - use regular Batch
            return None;
        }

        // Multi-cursor edits and code-action rewrites go through this path
        // (not `apply_event_to_active_buffer`). Promote any preview tab
        // here too so the invariant "edited buffer is never preview"
        // holds regardless of which edit path runs.
        self.promote_active_buffer_from_preview();

        let active_buf = self.active_buffer();
        let split_id = self.split_manager.active_split();

        // Capture old cursor states from SplitViewState (sole source of truth)
        let old_cursors: Vec<(CursorId, usize, Option<usize>)> = self
            .split_view_states
            .get(&split_id)
            .unwrap()
            .keyed_states
            .get(&active_buf)
            .unwrap()
            .cursors
            .iter()
            .map(|(id, c)| (id, c.position, c.anchor))
            .collect();

        let state = self.buffers.get_mut(&active_buf).unwrap();

        // Snapshot buffer state for undo (piece tree + buffers)
        let old_snapshot = state.buffer.snapshot_buffer_state();

        // Convert events to edit tuples: (position, delete_len, insert_text)
        // Events must be sorted by position descending (later positions first)
        // This ensures earlier edits don't shift positions of later edits
        let mut edits: Vec<(usize, usize, String)> = Vec::new();

        for event in &events {
            match event {
                Event::Insert { position, text, .. } => {
                    edits.push((*position, 0, text.clone()));
                }
                Event::Delete { range, .. } => {
                    edits.push((range.start, range.len(), String::new()));
                }
                _ => {}
            }
        }

        // Sort edits by position descending (required by apply_bulk_edits)
        edits.sort_by(|a, b| b.0.cmp(&a.0));

        // Convert to references for apply_bulk_edits
        let edit_refs: Vec<(usize, usize, &str)> = edits
            .iter()
            .map(|(pos, del, text)| (*pos, *del, text.as_str()))
            .collect();

        // Snapshot displaced markers before edits so undo can restore them exactly.
        let displaced_markers = state.capture_displaced_markers_bulk(&edits);

        // Apply bulk edits
        let _delta = state.buffer.apply_bulk_edits(&edit_refs);

        // Convert edit list to lengths-only for marker replay.
        // Merge edits at the same position into a single (pos, del_len, ins_len)
        // tuple. This is necessary because delete+insert at the same position
        // (e.g., line move: delete block, insert rearranged block) should be
        // treated as a replacement, not two independent adjustments.
        let edit_lengths: Vec<(usize, usize, usize)> = {
            let mut lengths: Vec<(usize, usize, usize)> = Vec::new();
            for (pos, del_len, text) in &edits {
                if let Some(last) = lengths.last_mut() {
                    if last.0 == *pos {
                        // Same position: merge del and ins lengths
                        last.1 += del_len;
                        last.2 += text.len();
                        continue;
                    }
                }
                lengths.push((*pos, *del_len, text.len()));
            }
            lengths
        };

        // Adjust markers and margins using the merged edit lengths.
        // Using merged edits (net delta for same-position replacements) avoids
        // the marker-at-boundary problem where sequential delete+insert at the
        // same position pushes markers incorrectly.
        for &(pos, del_len, ins_len) in &edit_lengths {
            if del_len > 0 && ins_len > 0 {
                // Replacement: adjust by net delta only
                if ins_len > del_len {
                    state.marker_list.adjust_for_insert(pos, ins_len - del_len);
                    state.margins.adjust_for_insert(pos, ins_len - del_len);
                } else if del_len > ins_len {
                    state.marker_list.adjust_for_delete(pos, del_len - ins_len);
                    state.margins.adjust_for_delete(pos, del_len - ins_len);
                }
                // Equal: net delta 0, no adjustment needed
            } else if del_len > 0 {
                state.marker_list.adjust_for_delete(pos, del_len);
                state.margins.adjust_for_delete(pos, del_len);
            } else if ins_len > 0 {
                state.marker_list.adjust_for_insert(pos, ins_len);
                state.margins.adjust_for_insert(pos, ins_len);
            }
        }

        // Snapshot buffer state after edits (for redo)
        let new_snapshot = state.buffer.snapshot_buffer_state();

        // Calculate new cursor positions based on events
        // Process cursor movements from the original events
        let mut new_cursors: Vec<(CursorId, usize, Option<usize>)> = old_cursors.clone();

        // Calculate position adjustments from edits (sorted ascending by position)
        // Each entry is (edit_position, delta) where delta = insert_len - delete_len
        let mut position_deltas: Vec<(usize, isize)> = Vec::new();
        for (pos, del_len, text) in &edits {
            let delta = text.len() as isize - *del_len as isize;
            position_deltas.push((*pos, delta));
        }
        position_deltas.sort_by_key(|(pos, _)| *pos);

        // Helper: calculate cumulative shift for a position based on edits at lower positions
        let calc_shift = |original_pos: usize| -> isize {
            let mut shift: isize = 0;
            for (edit_pos, delta) in &position_deltas {
                if *edit_pos < original_pos {
                    shift += delta;
                }
            }
            shift
        };

        // Apply adjustments to cursor positions
        // First check for explicit MoveCursor events (e.g., from indent operations)
        // These take precedence over implicit cursor updates from Insert/Delete
        for (cursor_id, ref mut pos, ref mut anchor) in &mut new_cursors {
            let mut found_move_cursor = false;
            // Save original position before any modifications - needed for shift calculation
            let original_pos = *pos;

            // Check if this cursor has an Insert at its original position (auto-close pattern).
            // For auto-close, Insert is at cursor position and MoveCursor is relative to original state.
            // For other operations (like indent), Insert is elsewhere and MoveCursor already accounts for shifts.
            let insert_at_cursor_pos = events.iter().any(|e| {
                matches!(e, Event::Insert { position, cursor_id: c, .. }
                    if *c == *cursor_id && *position == original_pos)
            });

            // First pass: look for explicit MoveCursor events for this cursor
            for event in &events {
                if let Event::MoveCursor {
                    cursor_id: event_cursor,
                    new_position,
                    new_anchor,
                    ..
                } = event
                {
                    if event_cursor == cursor_id {
                        // Only adjust for shifts if the Insert was at the cursor's original position
                        // (like auto-close). For other operations (like indent where Insert is at
                        // line start), the MoveCursor already accounts for the shift.
                        let shift = if insert_at_cursor_pos {
                            calc_shift(original_pos)
                        } else {
                            0
                        };
                        *pos = (*new_position as isize + shift).max(0) as usize;
                        *anchor = *new_anchor;
                        found_move_cursor = true;
                    }
                }
            }

            // If no explicit MoveCursor, derive position from Insert/Delete
            if !found_move_cursor {
                let mut found_edit = false;
                for event in &events {
                    match event {
                        Event::Insert {
                            position,
                            text,
                            cursor_id: event_cursor,
                        } if event_cursor == cursor_id => {
                            // For insert, cursor moves to end of inserted text
                            // Account for shifts from edits at lower positions
                            let shift = calc_shift(*position);
                            let adjusted_pos = (*position as isize + shift).max(0) as usize;
                            *pos = adjusted_pos.saturating_add(text.len());
                            *anchor = None;
                            found_edit = true;
                        }
                        Event::Delete {
                            range,
                            cursor_id: event_cursor,
                            ..
                        } if event_cursor == cursor_id => {
                            // For delete, cursor moves to start of deleted range
                            // Account for shifts from edits at lower positions
                            let shift = calc_shift(range.start);
                            *pos = (range.start as isize + shift).max(0) as usize;
                            *anchor = None;
                            found_edit = true;
                        }
                        _ => {}
                    }
                }

                // If this cursor had no events at all (e.g., cursor at end of buffer
                // during Delete, or at start during Backspace), still adjust its position
                // for shifts caused by other cursors' edits.
                if !found_edit {
                    let shift = calc_shift(original_pos);
                    *pos = (original_pos as isize + shift).max(0) as usize;
                }
            }
        }

        // Update cursors in SplitViewState (sole source of truth)
        {
            let cursors = &mut self
                .split_view_states
                .get_mut(&split_id)
                .unwrap()
                .keyed_states
                .get_mut(&active_buf)
                .unwrap()
                .cursors;
            for (cursor_id, position, anchor) in &new_cursors {
                if let Some(cursor) = cursors.get_mut(*cursor_id) {
                    cursor.position = *position;
                    cursor.anchor = *anchor;
                }
            }
        }

        // Invalidate highlighter
        self.buffers
            .get_mut(&active_buf)
            .unwrap()
            .highlighter
            .invalidate_all();

        // Create BulkEdit event with both buffer snapshots
        let bulk_edit = Event::BulkEdit {
            old_snapshot: Some(old_snapshot),
            new_snapshot: Some(new_snapshot),
            old_cursors,
            new_cursors,
            description,
            edits: edit_lengths,
            displaced_markers,
        };

        // Post-processing (layout invalidation, split cursor sync, etc.)
        self.invalidate_layouts_for_buffer(self.active_buffer());
        self.adjust_other_split_cursors_for_event(&bulk_edit);
        // Note: Do NOT clear search overlays - markers track through edits for F3/Shift+F3

        // Notify LSP of the change using full document replacement.
        // Bulk edits combine multiple Delete+Insert operations into a single tree pass,
        // so computing individual incremental LSP changes is not feasible. Instead,
        // send the full document content which is always correct.
        let buffer_id = self.active_buffer();
        let full_content_change = self
            .buffers
            .get(&buffer_id)
            .and_then(|s| s.buffer.to_string())
            .map(|text| {
                vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text,
                }]
            })
            .unwrap_or_default();
        if !full_content_change.is_empty() {
            self.send_lsp_changes_for_buffer(buffer_id, full_content_change);
        }

        Some(bulk_edit)
    }

    /// Trigger plugin hooks for an event (if any)
    /// line_info contains pre-calculated line numbers from BEFORE buffer modification
    fn trigger_plugin_hooks_for_event(&mut self, event: &Event, line_info: EventLineInfo) {
        let buffer_id = self.active_buffer();

        // Convert event to hook args and fire the appropriate hook
        let mut cursor_changed_lines = false;
        let hook_args = match event {
            Event::Insert { position, text, .. } => {
                let insert_position = *position;
                let insert_len = text.len();

                // Adjust byte ranges for the insertion
                if let Some(seen) = self.seen_byte_ranges.get_mut(&buffer_id) {
                    // Collect adjusted ranges:
                    // - Ranges ending before insert: keep unchanged
                    // - Ranges containing insert point: remove (content changed)
                    // - Ranges starting after insert: shift by insert_len
                    let adjusted: std::collections::HashSet<(usize, usize)> = seen
                        .iter()
                        .filter_map(|&(start, end)| {
                            if end <= insert_position {
                                // Range ends before insert - unchanged
                                Some((start, end))
                            } else if start >= insert_position {
                                // Range starts at or after insert - shift forward
                                Some((start + insert_len, end + insert_len))
                            } else {
                                // Range contains insert point - invalidate
                                None
                            }
                        })
                        .collect();
                    *seen = adjusted;
                }

                Some((
                    "after_insert",
                    crate::services::plugins::hooks::HookArgs::AfterInsert {
                        buffer_id,
                        position: *position,
                        text: text.clone(),
                        // Byte range of the affected area
                        affected_start: insert_position,
                        affected_end: insert_position + insert_len,
                        // Line info from pre-modification buffer
                        start_line: line_info.start_line,
                        end_line: line_info.end_line,
                        lines_added: line_info.line_delta.max(0) as usize,
                    },
                ))
            }
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                let delete_start = range.start;

                // Adjust byte ranges for the deletion
                let delete_end = range.end;
                let delete_len = delete_end - delete_start;
                if let Some(seen) = self.seen_byte_ranges.get_mut(&buffer_id) {
                    // Collect adjusted ranges:
                    // - Ranges ending before delete start: keep unchanged
                    // - Ranges overlapping deletion: remove (content changed)
                    // - Ranges starting after delete end: shift backward by delete_len
                    let adjusted: std::collections::HashSet<(usize, usize)> = seen
                        .iter()
                        .filter_map(|&(start, end)| {
                            if end <= delete_start {
                                // Range ends before delete - unchanged
                                Some((start, end))
                            } else if start >= delete_end {
                                // Range starts after delete - shift backward
                                Some((start - delete_len, end - delete_len))
                            } else {
                                // Range overlaps deletion - invalidate
                                None
                            }
                        })
                        .collect();
                    *seen = adjusted;
                }

                Some((
                    "after_delete",
                    crate::services::plugins::hooks::HookArgs::AfterDelete {
                        buffer_id,
                        range: range.clone(),
                        deleted_text: deleted_text.clone(),
                        // Byte position and length of deleted content
                        affected_start: delete_start,
                        deleted_len: deleted_text.len(),
                        // Line info from pre-modification buffer
                        start_line: line_info.start_line,
                        end_line: line_info.end_line,
                        lines_removed: (-line_info.line_delta).max(0) as usize,
                    },
                ))
            }
            Event::Batch { events, .. } => {
                // Fire hooks for each event in the batch
                // Note: For batches, line info is approximate since buffer already modified
                // Individual events will use the passed line_info which covers the whole batch
                for e in events {
                    // Use default line info for sub-events - they share the batch's line_info
                    // This is a simplification; proper tracking would need per-event pre-calculation
                    let sub_line_info = self.calculate_event_line_info(e);
                    self.trigger_plugin_hooks_for_event(e, sub_line_info);
                }
                None
            }
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                ..
            } => {
                // Get line numbers for old and new positions (1-indexed for plugins)
                let old_line = self.active_state().buffer.get_line_number(*old_position) + 1;
                let line = self.active_state().buffer.get_line_number(*new_position) + 1;
                cursor_changed_lines = old_line != line;
                let text_props = self
                    .active_state()
                    .text_properties
                    .get_at(*new_position)
                    .into_iter()
                    .map(|tp| tp.properties.clone())
                    .collect();
                Some((
                    "cursor_moved",
                    crate::services::plugins::hooks::HookArgs::CursorMoved {
                        buffer_id,
                        cursor_id: *cursor_id,
                        old_position: *old_position,
                        new_position: *new_position,
                        line,
                        text_properties: text_props,
                    },
                ))
            }
            _ => None,
        };

        // Fire the hook to TypeScript plugins
        if let Some((hook_name, ref args)) = hook_args {
            // Update the full plugin state snapshot BEFORE firing the hook
            // This ensures the plugin can read up-to-date state (diff, cursors, viewport, etc.)
            // Without this, there's a race condition where the async hook might read stale data
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();

            self.plugin_manager.run_hook(hook_name, args.clone());
        }

        // After inter-line cursor_moved, proactively refresh lines so
        // cursor-dependent conceals (e.g. emphasis auto-expose in compose
        // mode tables) update in the same frame. Without this, there's a
        // one-frame lag: the cursor_moved hook fires async to the plugin
        // which calls refreshLines() back, but that round-trip means the
        // first render after the cursor move still shows stale conceals.
        //
        // Only refresh on inter-line movement: intra-line moves (e.g.
        // Left/Right within a row) don't change which row is auto-exposed,
        // and the plugin's async refreshLines() handles span-level changes.
        if cursor_changed_lines {
            self.handle_refresh_lines(buffer_id);
        }
    }

    /// Handle scroll events using the SplitViewState's viewport
    ///
    /// View events (like Scroll) go to SplitViewState, not EditorState.
    /// This correctly handles scroll limits when view transforms inject headers.
    /// Also syncs to EditorState.viewport for the active split (used in rendering).
    pub(super) fn handle_scroll_event(&mut self, line_offset: isize) {
        use crate::view::ui::view_pipeline::ViewLineIterator;

        let active_split = self.split_manager.active_split();

        // Check if this split is in a scroll sync group (anchor-based sync for diffs)
        // Mark both splits to skip ensure_visible so cursor doesn't override scroll
        // The sync_scroll_groups() at render time will sync the other split
        if let Some(group) = self
            .scroll_sync_manager
            .find_group_for_split(active_split.into())
        {
            let left = group.left_split;
            let right = group.right_split;
            if let Some(vs) = self.split_view_states.get_mut(&LeafId(left)) {
                vs.viewport.set_skip_ensure_visible();
            }
            if let Some(vs) = self.split_view_states.get_mut(&LeafId(right)) {
                vs.viewport.set_skip_ensure_visible();
            }
            // Continue to scroll the active split normally below
        }

        // Fall back to simple sync_group (same delta to all splits)
        let sync_group = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.sync_group);
        let splits_to_scroll = if let Some(group_id) = sync_group {
            self.split_manager
                .get_splits_in_group(group_id, &self.split_view_states)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_scroll {
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
                id
            } else {
                continue;
            };
            let tab_size = self.config.editor.tab_size;

            // Get view_transform tokens from SplitViewState (if any)
            let view_transform_tokens = self
                .split_view_states
                .get(&split_id)
                .and_then(|vs| vs.view_transform.as_ref())
                .map(|vt| vt.tokens.clone());

            // Get mutable references to both buffer and view state
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                // Collect plugin soft-break positions BEFORE re-borrowing the
                // buffer so the viewport's visual-row math matches the renderer
                // (avoids the wheel-absorbed / empty-bottom mouse-scroll bugs
                // for compose-mode markdown — see scroll_down_visual).
                let soft_breaks = state.collect_soft_break_positions();
                let buffer = &mut state.buffer;
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    if let Some(tokens) = view_transform_tokens {
                        // Use view-aware scrolling with the transform's tokens
                        let view_lines: Vec<_> =
                            ViewLineIterator::new(&tokens, false, false, tab_size, false).collect();
                        view_state
                            .viewport
                            .scroll_view_lines(&view_lines, line_offset);
                    } else {
                        // No view transform - use traditional buffer-based scrolling
                        if line_offset > 0 {
                            view_state.viewport.scroll_down(
                                buffer,
                                &soft_breaks,
                                line_offset as usize,
                            );
                        } else {
                            view_state.viewport.scroll_up(
                                buffer,
                                &soft_breaks,
                                line_offset.unsigned_abs(),
                            );
                        }
                    }
                    // Mark to skip ensure_visible on next render so the scroll isn't undone
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }

    /// Handle SetViewport event using SplitViewState's viewport
    fn handle_set_viewport_event(&mut self, top_line: usize) {
        let active_split = self.split_manager.active_split();

        // Check if this split is in a scroll sync group (anchor-based sync for diffs)
        // If so, set the group's scroll_line and let render sync the viewports
        if self
            .scroll_sync_manager
            .is_split_synced(active_split.into())
        {
            if let Some(group) = self
                .scroll_sync_manager
                .find_group_for_split_mut(active_split.into())
            {
                // Convert line to left buffer space if coming from right split
                let scroll_line = if group.is_left_split(active_split.into()) {
                    top_line
                } else {
                    group.right_to_left_line(top_line)
                };
                group.set_scroll_line(scroll_line);
            }

            // Mark both splits to skip ensure_visible
            if let Some(group) = self
                .scroll_sync_manager
                .find_group_for_split(active_split.into())
            {
                let left = group.left_split;
                let right = group.right_split;
                if let Some(vs) = self.split_view_states.get_mut(&LeafId(left)) {
                    vs.viewport.set_skip_ensure_visible();
                }
                if let Some(vs) = self.split_view_states.get_mut(&LeafId(right)) {
                    vs.viewport.set_skip_ensure_visible();
                }
            }
            return;
        }

        // Fall back to simple sync_group (same line to all splits)
        let sync_group = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.sync_group);
        let splits_to_scroll = if let Some(group_id) = sync_group {
            self.split_manager
                .get_splits_in_group(group_id, &self.split_view_states)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_scroll {
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
                id
            } else {
                continue;
            };

            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                let buffer = &mut state.buffer;
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    view_state.viewport.scroll_to(buffer, top_line);
                    // Mark to skip ensure_visible on next render so the scroll isn't undone
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }

    /// Handle Recenter event using SplitViewState's viewport
    fn handle_recenter_event(&mut self) {
        let active_split = self.split_manager.active_split();

        // Find other splits in the same sync group if any
        let sync_group = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.sync_group);
        let splits_to_recenter = if let Some(group_id) = sync_group {
            self.split_manager
                .get_splits_in_group(group_id, &self.split_view_states)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_recenter {
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
                id
            } else {
                continue;
            };

            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                let buffer = &mut state.buffer;
                let view_state = self.split_view_states.get_mut(&split_id);

                if let Some(view_state) = view_state {
                    // Recenter viewport on cursor
                    let cursor = *view_state.cursors.primary();
                    let viewport_height = view_state.viewport.visible_line_count();
                    let target_rows_from_top = viewport_height / 2;

                    // Move backwards from cursor position target_rows_from_top lines
                    let mut iter = buffer.line_iterator(cursor.position, 80);
                    for _ in 0..target_rows_from_top {
                        if iter.prev().is_none() {
                            break;
                        }
                    }
                    let new_top_byte = iter.current_position();
                    view_state.viewport.top_byte = new_top_byte;
                    // Mark to skip ensure_visible on next render so the scroll isn't undone
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }

    /// Invalidate layouts for all splits viewing a specific buffer
    ///
    /// Called after buffer content changes (Insert/Delete) to mark
    /// layouts as dirty, forcing rebuild on next access.
    /// Also clears any cached view transform since its token source_offsets
    /// become stale after buffer edits.
    pub(super) fn invalidate_layouts_for_buffer(&mut self, buffer_id: BufferId) {
        // Find all splits that display this buffer
        let splits_for_buffer = self.split_manager.splits_for_buffer(buffer_id);

        // Invalidate layout and clear stale view transform for each split
        for split_id in splits_for_buffer {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.invalidate_layout();
                // Clear cached view transform — its token source_offsets are from
                // before the edit and would cause conceals to be applied at wrong positions.
                // The view_transform_request hook will fire on the next render to rebuild it.
                view_state.view_transform = None;
                // Mark as stale so that any pending SubmitViewTransform commands
                // (from a previous view_transform_request) are rejected.
                view_state.view_transform_stale = true;
            }
        }
    }
}
