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
//! The "scroll/viewport event" handlers (handle_scroll_event,
//! handle_set_viewport_event, handle_recenter_event) and the small
//! `invalidate_layouts_for_buffer` helper now live on `impl Window`
//! since they're entirely per-window concerns.

use lsp_types::TextDocumentContentChangeEvent;

use crate::model::event::Event;

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

    /// Apply an event to the active buffer with all cross-cutting concerns.
    /// This is the centralized method that automatically handles:
    /// - Event application to buffer
    /// - Plugin hooks (after-insert, after-delete, etc.)
    /// - LSP notifications
    /// - Any other cross-cutting concerns
    pub fn apply_event_to_active_buffer(&mut self, event: &Event) {
        // Handle View events at Editor level - View events go to SplitViewState, not EditorState
        // This properly separates Buffer state from View state
        match event {
            Event::Scroll { line_offset } => {
                self.active_window_mut().handle_scroll_event(*line_offset);
                return;
            }
            Event::SetViewport { top_line } => {
                self.active_window_mut()
                    .handle_set_viewport_event(*top_line);
                return;
            }
            Event::Recenter => {
                self.active_window_mut().handle_recenter_event();
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
            self.active_window_mut()
                .promote_active_buffer_from_preview();
        }

        // IMPORTANT: Calculate LSP changes and line info BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer,
        // so we must convert them to LSP positions before modifying the buffer.
        let lsp_changes = self.active_window().collect_lsp_changes(event);

        // Calculate line info for plugin hooks (using same pre-modification buffer state)
        let line_info = self.active_window().calculate_event_line_info(event);

        // 1. Apply the event to the buffer
        // Borrow cursors from SplitViewState (sole source of truth) and state from buffers.
        //
        // Use the *effective* active split so events targeting a focused
        // buffer-group panel land in the panel's own split view state, not
        // the group host's. Without this, MoveCursor events for a focused
        // panel would try to look up the panel buffer's keyed state in the
        // host split (which doesn't have it) and panic on unwrap.
        //
        // Debug-only check: verify the pane-buffer invariant before
        // dereferencing. Any mismatch means a write path skipped
        // `Editor::set_pane_buffer` (see `active_focus.rs`); we want
        // that to fail with a clear message in tests rather than
        // surfacing as a bare `Option::unwrap` panic in production
        // (issue #1620).
        {
            let split_id = self.effective_active_split();
            let active_buf = self.active_buffer();
            debug_assert!(
                self.windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .is_some_and(|vs| vs.keyed_states.contains_key(&active_buf)),
                "pane-buffer invariant violated: split {:?} resolves to buffer {:?} \
                 but that split's keyed_states has no entry for it. Some write path \
                 bypassed Editor::set_pane_buffer; see active_focus.rs / issue #1620.",
                split_id,
                active_buf,
            );
            self.active_window_mut()
                .apply_event_to_keyed_buffer(active_buf, split_id, event);
        }

        // 1c. Invalidate layouts for all views of this buffer after content changes
        // Note: recovery_pending is set automatically by the buffer on edits
        match event {
            Event::Insert { .. } | Event::Delete { .. } | Event::BulkEdit { .. } => {
                let buf = self.active_buffer();
                let win = self.active_window_mut();
                win.invalidate_layouts_for_buffer(buf);
                win.schedule_semantic_tokens_full_refresh(buf);
                win.schedule_folding_ranges_refresh(buf);
            }
            Event::Batch { events, .. } => {
                let has_edits = events
                    .iter()
                    .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));
                if has_edits {
                    let buf = self.active_buffer();
                    let win = self.active_window_mut();
                    win.invalidate_layouts_for_buffer(buf);
                    win.schedule_semantic_tokens_full_refresh(buf);
                    win.schedule_folding_ranges_refresh(buf);
                }
            }
            _ => {}
        }

        // 2. Adjust cursors in other splits that share the same buffer
        self.active_window_mut()
            .adjust_other_split_cursors_for_event(event);

        // 3. Re-evaluate search highlights around the edited region.
        // Overlays have markers that automatically track position changes
        // through edits (so F3/Shift+F3 find matches at their updated
        // positions), but the markers never re-check whether the covered
        // text still matches the query. Without this, editing inside a
        // highlighted match — or typing against its boundary so a
        // whole-word `\b` rule no longer holds — would leave a stale
        // highlight on text that no longer matches. We skip during
        // interactive replace, which manages its own highlight lifecycle.
        if self.active_window().interactive_replace_state.is_none() {
            let search_bg = self.theme.read().unwrap().search_match_bg;
            let search_fg = self.theme.read().unwrap().search_match_fg;
            match event {
                Event::Insert { position, text, .. } => {
                    self.active_window_mut().reevaluate_search_overlays_around(
                        *position,
                        text.len(),
                        search_fg,
                        search_bg,
                    );
                }
                Event::Delete { range, .. } => {
                    self.active_window_mut().reevaluate_search_overlays_around(
                        range.start,
                        0,
                        search_fg,
                        search_bg,
                    );
                }
                Event::Batch { events, .. } => {
                    for e in events {
                        match e {
                            Event::Insert { position, text, .. } => {
                                self.active_window_mut().reevaluate_search_overlays_around(
                                    *position,
                                    text.len(),
                                    search_fg,
                                    search_bg,
                                );
                            }
                            Event::Delete { range, .. } => {
                                self.active_window_mut().reevaluate_search_overlays_around(
                                    range.start,
                                    0,
                                    search_fg,
                                    search_bg,
                                );
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }

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
                let buf = self.active_buffer();
                self.active_window_mut()
                    .send_lsp_changes_for_buffer(buf, full_change);
            }
        } else {
            let buf = self.active_buffer();
            self.active_window_mut()
                .send_lsp_changes_for_buffer(buf, lsp_changes);
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
        self.active_window_mut()
            .promote_active_buffer_from_preview();

        let active_buf = self.active_buffer();
        // Use `effective_active_split` rather than `split_manager.active_split()`
        // so we get the leaf whose `SplitViewState` actually owns the active
        // buffer's keyed_states. They diverge whenever a buffer-group panel
        // is focused (e.g. the Theme Editor): `active_buffer()` resolves to
        // the inner panel's buffer via `effective_active_pair`, while the
        // outer split has no entry for it. Without this, a paste with >1
        // event in the Theme Editor unwraps `None` and panics.
        let split_id = self.effective_active_split();

        // Capture old cursor states from SplitViewState (sole source of truth)
        let old_cursors: Vec<(CursorId, usize, Option<usize>)> = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .unwrap()
            .keyed_states
            .get(&active_buf)
            .unwrap()
            .cursors
            .iter()
            .map(|(id, c)| (id, c.position, c.anchor))
            .collect();

        let state = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&active_buf)
            .unwrap();

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
        edits.sort_by_key(|b| std::cmp::Reverse(b.0));

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

        // Adjust markers and margins using the merged edit lengths, AND record
        // them into the coordinate-map ring. Routing through the same method the
        // undo/redo bulk path uses (`replay_bulk_marker_adjustments`) keeps this
        // forward path's ring coverage equal to its marker coverage — without it
        // a paste / multi-cursor / query-replace / code-action bumped the buffer
        // version with no ring delta, so `map_plugin_coord` under-shifted stale
        // plugin coordinates (the exact corruption coord_map exists to prevent).
        // Merged edits give the net delta for same-position replacements, which
        // also avoids the marker-at-boundary problem.
        state.replay_bulk_marker_adjustments(&edit_lengths);

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
        //
        // Sticky (goal) columns ride along: an explicit MoveCursor carries the
        // authoritative new goal column, and a cursor that edited must
        // re-derive its goal from the new position (`None`). Cursors that had
        // no events keep their goal. Without this, the goal column set before
        // a bulk edit (e.g. by a mouse click) survives the edit and sends the
        // next vertical move to a stale column.
        let mut sticky_updates: Vec<(CursorId, Option<usize>)> = Vec::new();
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
                    new_sticky_column,
                    ..
                } = event
                {
                    if event_cursor == cursor_id {
                        sticky_updates.retain(|(id, _)| id != cursor_id);
                        sticky_updates.push((*cursor_id, *new_sticky_column));
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
                            if !found_edit {
                                sticky_updates.push((*cursor_id, None));
                            }
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
                            if !found_edit {
                                sticky_updates.push((*cursor_id, None));
                            }
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
                .split_view_states_mut()
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
            for (cursor_id, sticky) in &sticky_updates {
                if let Some(cursor) = cursors.get_mut(*cursor_id) {
                    cursor.sticky_column = *sticky;
                }
            }
        }

        // Notify the highlighter of each edit so the cache can take the
        // partial-update path on the next render. Throwing the whole cache
        // away here (the previous behaviour) wiped every checkpoint as well,
        // forcing a cold reparse from byte zero on the next keystroke — see
        // https://github.com/sinelaw/fresh/issues/1958.
        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&active_buf)
            .unwrap()
            .highlighter
            .notify_edits(&edit_lengths);

        // Bulk edits (multi-cursor typing, paste, code-action rewrites) apply
        // here instead of through the Insert/Delete hook arms, so shift plugin
        // interval markers for each edit too — mirroring the `marker_list`
        // adjustments above — or plugin-tracked decorations keep stale coords.
        #[cfg(feature = "plugins")]
        for &(pos, del_len, ins_len) in &edit_lengths {
            self.shift_plugin_markers_for_edit(active_buf, pos, del_len, ins_len);
        }

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
        let buf = self.active_buffer();
        let win = self.active_window_mut();
        win.invalidate_layouts_for_buffer(buf);
        win.adjust_other_split_cursors_for_event(&bulk_edit);
        // Note: Do NOT clear search overlays - markers track through edits for F3/Shift+F3

        // Notify LSP of the change using full document replacement.
        // Bulk edits combine multiple Delete+Insert operations into a single tree pass,
        // so computing individual incremental LSP changes is not feasible. Instead,
        // send the full document content which is always correct.
        let buffer_id = self.active_buffer();
        let full_content_change = self
            .buffers()
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
            self.active_window_mut()
                .send_lsp_changes_for_buffer(buffer_id, full_content_change);
        }

        Some(bulk_edit)
    }

    /// Trigger plugin hooks for an event (if any)
    /// line_info contains pre-calculated line numbers from BEFORE buffer modification
    fn trigger_plugin_hooks_for_event(&mut self, event: &Event, line_info: EventLineInfo) {
        let buffer_id = self.active_buffer();

        // Convert event to hook args and fire the appropriate hook
        let hook_args = match event {
            Event::Insert { position, text, .. } => {
                let insert_position = *position;
                let insert_len = text.len();

                // Adjust byte ranges for the insertion
                if let Some(seen) = self
                    .active_window_mut()
                    .seen_byte_ranges
                    .get_mut(&buffer_id)
                {
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

                // Shift plugin interval markers so they ride the inserted text
                // (the editor owns shifting; plugins never track byte offsets).
                #[cfg(feature = "plugins")]
                self.shift_plugin_markers_for_edit(buffer_id, insert_position, 0, insert_len);

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
                if let Some(seen) = self
                    .active_window_mut()
                    .seen_byte_ranges
                    .get_mut(&buffer_id)
                {
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

                // Shift plugin interval markers for the deletion (editor owns
                // shifting; the plugin re-discovers markers whose interior was hit).
                #[cfg(feature = "plugins")]
                self.shift_plugin_markers_for_edit(buffer_id, delete_start, delete_len, 0);

                Some((
                    "after_delete",
                    crate::services::plugins::hooks::HookArgs::AfterDelete {
                        buffer_id,
                        start: range.start,
                        end: range.end,
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
                    let sub_line_info = self.active_window().calculate_event_line_info(e);
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
                let line = self.active_state().buffer.get_line_number(*new_position) + 1;
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

            self.plugin_manager
                .read()
                .unwrap()
                .run_hook(hook_name, args.clone());
        }

        // Cursor movement needs NO refresh here: cursor-dependent conceals
        // and soft breaks carry activation scopes evaluated at render time
        // (see view/activation.rs), so the very next frame picks up the
        // reveal without any marker churn or plugin round-trip. Refreshing
        // the viewport on every inter-line move used to clear
        // `seen_byte_ranges`, re-fire `lines_changed` for all visible
        // lines, and rebuild every conceal/soft-break marker — bumping the
        // manager versions and invalidating the whole `LineWrapCache` and
        // `VisualRowIndex` per keypress (the compose-mode arrow-key lag).
        //
        // A structure-changing edit (a newline inserted or deleted) still
        // needs a full refresh, for a subtler reason: it renumbers every row
        // below it, but the per-edit `seen_byte_ranges` invalidation only
        // drops the single line containing the edit point — every other row
        // merely shifts and stays "seen", so it never re-fires
        // `lines_changed`. A per-line decoration plugin (markdown_compose's
        // table borders / conceals) then leaves the prior frame's decorations
        // in place, scattered across the buffer versions they were last
        // emitted at. During an Insert/Delete storm (which emits no
        // MoveCursor events at all) nothing ever forces a complete re-fire,
        // so the stale decorations pile up and persist. Clearing seen here
        // gives every such edit one consistent, whole-viewport re-fire.
        // Intra-line edits (no line-count change) stay on the cheap path:
        // the edited line's own invalidation re-fires it, which is
        // sufficient.
        let edit_changed_line_count = matches!(event, Event::Insert { .. } | Event::Delete { .. })
            && line_info.line_delta != 0;
        if edit_changed_line_count {
            self.handle_refresh_lines(buffer_id);
        }
    }
}
