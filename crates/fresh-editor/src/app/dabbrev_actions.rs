//! Dabbrev expand action: Emacs-style sequential cycling completion.
//!
//! Unlike popup-based completion, dabbrev directly inserts the best match
//! and cycles through alternatives on repeated Alt+/ presses. The session
//! resets when any other action is taken (typing, moving, etc.).

use super::{DabbrevCycleState, Editor};
use crate::model::event::Event;
use crate::services::completion::dabbrev::DabbrevProvider;
use crate::services::completion::provider::{
    CompletionContext, CompletionProvider, OtherBufferSlice, ProviderResult,
};

/// Scan radius for other-buffer slices during dabbrev.
const OTHER_BUFFER_SCAN_RADIUS: usize = 64 * 1024; // 64 KB

impl Editor {
    /// Handle the DabbrevExpand action (Alt+/).
    ///
    /// First invocation: compute candidates from the active buffer (and
    /// other open buffers), insert the top match. Subsequent invocations:
    /// undo the previous insertion and insert the next candidate.
    pub(crate) fn dabbrev_expand(&mut self) {
        if self.dabbrev_state.is_some() {
            self.dabbrev_cycle();
        } else {
            self.dabbrev_expand_first();
        }
    }

    /// Cycle to the next dabbrev candidate.
    fn dabbrev_cycle(&mut self) {
        // Take state temporarily to satisfy borrow checker.
        let mut state = match self.dabbrev_state.take() {
            Some(s) => s,
            None => return,
        };

        let cursor_id = self.active_cursors().primary_id();
        let cursor_pos = self.active_cursors().primary().position;
        let word_start = state.word_start;

        // Delete the previously inserted text.
        let prev_text = &state.candidates[state.index];
        let prev_end = word_start + prev_text.len();
        if cursor_pos == prev_end && prev_end <= self.active_state().buffer.len() {
            let deleted_text = self.active_state_mut().get_text_range(word_start, prev_end);
            let delete_event = Event::Delete {
                range: word_start..prev_end,
                deleted_text,
                cursor_id,
            };
            self.log_and_apply_event(&delete_event);
        }

        // Advance index. Wrap → restore original prefix and end session.
        state.index += 1;
        if state.index >= state.candidates.len() {
            // Restore the original prefix the user typed.
            let insert_event = Event::Insert {
                position: word_start,
                text: state.original_prefix.clone(),
                cursor_id,
            };
            self.log_and_apply_event(&insert_event);
            // Session over — don't re-store state.
        } else {
            // Insert the next candidate.
            let next = state.candidates[state.index].clone();
            let insert_event = Event::Insert {
                position: word_start,
                text: next,
                cursor_id,
            };
            self.log_and_apply_event(&insert_event);
            self.dabbrev_state = Some(state);
        }
    }

    /// First Alt+/ press: scan buffers and insert the best match.
    fn dabbrev_expand_first(&mut self) {
        use crate::primitives::word_navigation::find_completion_word_start;

        let cursor_id = self.active_cursors().primary_id();
        let cursor_pos = self.active_cursors().primary().position;
        let word_start = find_completion_word_start(&self.active_state().buffer, cursor_pos);

        if word_start >= cursor_pos {
            return; // No prefix typed
        }

        let prefix = self
            .active_state_mut()
            .get_text_range(word_start, cursor_pos);
        if prefix.is_empty() {
            return;
        }

        let buffer_len = self.active_state().buffer.len();
        let is_large = self.active_state().buffer.is_large_file();
        let scan_range = CompletionContext::compute_scan_range(cursor_pos, buffer_len, is_large);
        let buffer_window = self.active_state().buffer.slice_bytes(scan_range.clone());

        // Get language-specific word chars from resolved config.
        let word_chars_extra = self.active_state().buffer_settings.word_characters.clone();

        // Build other-buffer slices (MRU order).
        let active_buf_id = self.active_buffer();
        let other_buffers = self.collect_other_buffer_slices(active_buf_id);

        let prefix_has_upper = prefix.chars().any(|c| c.is_uppercase());

        let ctx = CompletionContext {
            prefix: prefix.clone(),
            cursor_byte: cursor_pos,
            word_start_byte: word_start,
            buffer_len,
            is_large_file: is_large,
            scan_range,
            viewport_top_byte: 0,
            viewport_bottom_byte: buffer_len.min(512 * 1024),
            language_id: None,
            word_chars_extra,
            prefix_has_uppercase: prefix_has_upper,
            other_buffers,
        };

        let provider = DabbrevProvider::new();
        let result = provider.provide(&ctx, &buffer_window);

        let candidates: Vec<String> = match result {
            ProviderResult::Ready(c) => c.into_iter().map(|c| c.label).collect(),
            _ => return,
        };

        if candidates.is_empty() {
            return;
        }

        // Delete the prefix and insert the first candidate.
        let deleted_text = prefix.clone();
        let delete_event = Event::Delete {
            range: word_start..cursor_pos,
            deleted_text,
            cursor_id,
        };
        self.log_and_apply_event(&delete_event);

        let first = candidates[0].clone();
        let insert_event = Event::Insert {
            position: word_start,
            text: first,
            cursor_id,
        };
        self.log_and_apply_event(&insert_event);

        self.dabbrev_state = Some(DabbrevCycleState {
            original_prefix: prefix,
            word_start,
            candidates,
            index: 0,
        });
    }

    /// Collect small byte-windows from other open buffers for multi-buffer scanning.
    pub(crate) fn collect_other_buffer_slices(
        &self,
        exclude_buffer_id: crate::model::event::BufferId,
    ) -> Vec<OtherBufferSlice> {
        let mut slices = Vec::new();

        for (&buf_id, state) in self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
        {
            if buf_id == exclude_buffer_id {
                continue;
            }
            let buf = &state.buffer;
            let buf_len = buf.len();
            if buf_len == 0 {
                continue;
            }
            // Take a window from the middle (or full if small enough).
            let radius = OTHER_BUFFER_SCAN_RADIUS;
            let mid = buf_len / 2;
            let start = mid.saturating_sub(radius);
            let end = (mid + radius).min(buf_len);
            let bytes = buf.slice_bytes(start..end);
            let label = buf
                .file_path()
                .and_then(|p| p.file_name())
                .map(|f| f.to_string_lossy().to_string())
                .unwrap_or_else(|| "untitled".to_string());
            slices.push(OtherBufferSlice {
                buffer_id: buf_id.0 as u64,
                bytes,
                label,
            });
        }

        slices
    }

    /// Reset the dabbrev cycling session. Called when any non-dabbrev action
    /// is taken (typing, moving cursor, etc.).
    pub(crate) fn reset_dabbrev_state(&mut self) {
        self.dabbrev_state = None;
    }

    /// Run the `CompletionService` (buffer-words + dabbrev providers) and
    /// return results as `PopupListItemData` items suitable for the
    /// completion popup. The items use icon `"w"` to visually distinguish
    /// them from LSP results.
    ///
    /// Returns an empty vec if the prefix is empty or no candidates match.
    pub(crate) fn get_buffer_completion_popup_items(
        &mut self,
    ) -> Vec<crate::model::event::PopupListItemData> {
        use crate::model::event::PopupListItemData;
        use crate::primitives::word_navigation::find_completion_word_start;

        let cursor_pos = self.active_cursors().primary().position;
        let word_start = find_completion_word_start(&self.active_state().buffer, cursor_pos);

        if word_start >= cursor_pos {
            return Vec::new();
        }

        let prefix = self
            .active_state_mut()
            .get_text_range(word_start, cursor_pos);
        if prefix.is_empty() {
            return Vec::new();
        }

        let buffer_len = self.active_state().buffer.len();
        let is_large = self.active_state().buffer.is_large_file();
        let scan_range = CompletionContext::compute_scan_range(cursor_pos, buffer_len, is_large);
        let buffer_window = self.active_state().buffer.slice_bytes(scan_range.clone());
        let word_chars_extra = self.active_state().buffer_settings.word_characters.clone();

        let active_buf_id = self.active_buffer();
        let other_buffers = self.collect_other_buffer_slices(active_buf_id);

        // Get viewport bounds for proximity scoring.
        let split_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.splits.as_ref())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        let viewport_top_byte = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.splits.as_ref())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .map(|sv| sv.viewport.top_byte)
            .unwrap_or(0);
        // Estimate bottom by adding a generous window.
        let viewport_bottom_byte = (viewport_top_byte + 8192).min(buffer_len);

        let prefix_has_upper = prefix.chars().any(|c| c.is_uppercase());

        let ctx = CompletionContext {
            prefix,
            cursor_byte: cursor_pos,
            word_start_byte: word_start,
            buffer_len,
            is_large_file: is_large,
            scan_range,
            viewport_top_byte,
            viewport_bottom_byte,
            language_id: None,
            word_chars_extra,
            prefix_has_uppercase: prefix_has_upper,
            other_buffers,
        };

        let candidates = self.completion_service.request(&ctx, &buffer_window);

        candidates
            .into_iter()
            .map(|c| PopupListItemData {
                text: c.label.clone(),
                detail: c.detail.clone(),
                icon: Some("w".to_string()),
                data: c.insert_text.or(Some(c.label)),
            })
            .collect()
    }
}
