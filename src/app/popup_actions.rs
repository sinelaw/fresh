//! Popup action handlers.
//!
//! This module contains handlers for popup-related actions like confirmation and cancellation.

use super::Editor;
use crate::model::event::Event;
use crate::primitives::word_navigation::find_completion_word_start;

/// Result of handling a popup confirmation.
pub enum PopupConfirmResult {
    /// Popup handled, continue normally
    Done,
    /// Popup handled, should return early from handle_action
    EarlyReturn,
}

impl Editor {
    /// Handle PopupConfirm action.
    ///
    /// Returns `PopupConfirmResult` indicating what the caller should do next.
    pub fn handle_popup_confirm(&mut self) -> PopupConfirmResult {
        // Check if this is an LSP confirmation popup
        let lsp_confirmation_action = if let Some(popup) = self.active_state().popups.top() {
            if let Some(title) = &popup.title {
                if title.starts_with("Start LSP Server:") {
                    popup.selected_item().and_then(|item| item.data.clone())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        // Handle LSP confirmation if present
        if let Some(action) = lsp_confirmation_action {
            self.hide_popup();
            self.handle_lsp_confirmation_response(&action);
            return PopupConfirmResult::EarlyReturn;
        }

        // If it's a completion popup, insert the selected item
        let completion_text = if let Some(popup) = self.active_state().popups.top() {
            if let Some(title) = &popup.title {
                if title == "Completion" {
                    popup.selected_item().and_then(|item| item.data.clone())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        // Perform the completion if we have text
        if let Some(text) = completion_text {
            self.insert_completion_text(text);
        }

        self.hide_popup();
        PopupConfirmResult::Done
    }

    /// Insert completion text, replacing the word prefix at cursor.
    fn insert_completion_text(&mut self, text: String) {
        let (cursor_id, cursor_pos, word_start) = {
            let state = self.active_state();
            let cursor_id = state.cursors.primary_id();
            let cursor_pos = state.cursors.primary().position;
            let word_start = find_completion_word_start(&state.buffer, cursor_pos);
            (cursor_id, cursor_pos, word_start)
        };

        let deleted_text = if word_start < cursor_pos {
            self.active_state_mut()
                .get_text_range(word_start, cursor_pos)
        } else {
            String::new()
        };

        if word_start < cursor_pos {
            let delete_event = Event::Delete {
                range: word_start..cursor_pos,
                deleted_text,
                cursor_id,
            };

            self.active_event_log_mut().append(delete_event.clone());
            self.apply_event_to_active_buffer(&delete_event);

            let buffer_len = self.active_state().buffer.len();
            let insert_pos = word_start.min(buffer_len);

            let insert_event = Event::Insert {
                position: insert_pos,
                text,
                cursor_id,
            };

            self.active_event_log_mut().append(insert_event.clone());
            self.apply_event_to_active_buffer(&insert_event);
        } else {
            let insert_event = Event::Insert {
                position: cursor_pos,
                text,
                cursor_id,
            };

            self.active_event_log_mut().append(insert_event.clone());
            self.apply_event_to_active_buffer(&insert_event);
        }
    }

    /// Handle PopupCancel action.
    pub fn handle_popup_cancel(&mut self) {
        if self.pending_lsp_confirmation.is_some() {
            self.pending_lsp_confirmation = None;
            self.set_status_message("LSP server startup cancelled".to_string());
        }
        self.hide_popup();
    }
}
