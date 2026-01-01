//! Popup action handlers.
//!
//! This module contains handlers for popup-related actions like confirmation and cancellation.

use super::Editor;
use crate::model::event::Event;
use crate::primitives::snippet::{expand_snippet, is_snippet};
use crate::primitives::word_navigation::find_completion_word_start;
use rust_i18n::t;

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
        // Check if this is an action popup (from plugin showActionPopup)
        if let Some((popup_id, _actions)) = &self.active_action_popup {
            let popup_id = popup_id.clone();
            let action_id = self
                .active_state()
                .popups
                .top()
                .and_then(|p| p.selected_item())
                .and_then(|item| item.data.clone())
                .unwrap_or_else(|| "dismissed".to_string());

            self.hide_popup();
            self.active_action_popup = None;

            // Fire the ActionPopupResult hook
            self.plugin_manager.run_hook(
                "action_popup_result",
                crate::services::plugins::hooks::HookArgs::ActionPopupResult {
                    popup_id,
                    action_id,
                },
            );

            return PopupConfirmResult::EarlyReturn;
        }

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
    /// If the text contains LSP snippet syntax, it will be expanded.
    fn insert_completion_text(&mut self, text: String) {
        // Check if this is a snippet and expand it
        let (insert_text, cursor_offset) = if is_snippet(&text) {
            let expanded = expand_snippet(&text);
            (expanded.text, Some(expanded.cursor_offset))
        } else {
            (text, None)
        };

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

        let insert_pos = if word_start < cursor_pos {
            let delete_event = Event::Delete {
                range: word_start..cursor_pos,
                deleted_text,
                cursor_id,
            };

            self.active_event_log_mut().append(delete_event.clone());
            self.apply_event_to_active_buffer(&delete_event);

            let buffer_len = self.active_state().buffer.len();
            word_start.min(buffer_len)
        } else {
            cursor_pos
        };

        let insert_event = Event::Insert {
            position: insert_pos,
            text: insert_text.clone(),
            cursor_id,
        };

        self.active_event_log_mut().append(insert_event.clone());
        self.apply_event_to_active_buffer(&insert_event);

        // If this was a snippet, position cursor at the snippet's $0 location
        if let Some(offset) = cursor_offset {
            let new_cursor_pos = insert_pos + offset;
            // Get current cursor position after the insert
            let current_pos = self.active_state().cursors.primary().position;
            if current_pos != new_cursor_pos {
                let move_event = Event::MoveCursor {
                    cursor_id,
                    old_position: current_pos,
                    new_position: new_cursor_pos,
                    old_anchor: None,
                    new_anchor: None,
                    old_sticky_column: 0,
                    new_sticky_column: 0,
                };
                self.active_state_mut().apply(&move_event);
            }
        }
    }

    /// Handle PopupCancel action.
    pub fn handle_popup_cancel(&mut self) {
        tracing::info!(
            "handle_popup_cancel: active_action_popup={:?}",
            self.active_action_popup.as_ref().map(|(id, _)| id)
        );

        // Check if this is an action popup (from plugin showActionPopup)
        if let Some((popup_id, _actions)) = self.active_action_popup.take() {
            tracing::info!(
                "handle_popup_cancel: dismissing action popup id={}",
                popup_id
            );
            self.hide_popup();

            // Fire the ActionPopupResult hook with "dismissed"
            self.plugin_manager.run_hook(
                "action_popup_result",
                crate::services::plugins::hooks::HookArgs::ActionPopupResult {
                    popup_id,
                    action_id: "dismissed".to_string(),
                },
            );
            tracing::info!("handle_popup_cancel: action_popup_result hook fired");
            return;
        }

        if self.pending_lsp_confirmation.is_some() {
            self.pending_lsp_confirmation = None;
            self.set_status_message(t!("lsp.startup_cancelled_msg").to_string());
        }
        self.hide_popup();
        // Clear completion items when popup is closed
        self.completion_items = None;
    }

    /// Handle typing a character while completion popup is open.
    /// Inserts the character into the buffer and re-filters the completion list.
    pub fn handle_popup_type_char(&mut self, c: char) {
        // First, insert the character into the buffer
        let (cursor_id, cursor_pos) = {
            let state = self.active_state();
            (state.cursors.primary_id(), state.cursors.primary().position)
        };

        let insert_event = Event::Insert {
            position: cursor_pos,
            text: c.to_string(),
            cursor_id,
        };

        self.active_event_log_mut().append(insert_event.clone());
        self.apply_event_to_active_buffer(&insert_event);

        // Now re-filter the completion list
        self.refilter_completion_popup();
    }

    /// Handle backspace while completion popup is open.
    /// Deletes a character and re-filters the completion list.
    pub fn handle_popup_backspace(&mut self) {
        let (cursor_id, cursor_pos) = {
            let state = self.active_state();
            (state.cursors.primary_id(), state.cursors.primary().position)
        };

        // Don't do anything if at start of buffer
        if cursor_pos == 0 {
            return;
        }

        // Find the previous character boundary
        let prev_pos = {
            let state = self.active_state();
            let text = match state.buffer.to_string() {
                Some(t) => t,
                None => return,
            };
            // Find the previous character
            text[..cursor_pos]
                .char_indices()
                .last()
                .map(|(i, _)| i)
                .unwrap_or(0)
        };

        let deleted_text = self.active_state_mut().get_text_range(prev_pos, cursor_pos);

        let delete_event = Event::Delete {
            range: prev_pos..cursor_pos,
            deleted_text,
            cursor_id,
        };

        self.active_event_log_mut().append(delete_event.clone());
        self.apply_event_to_active_buffer(&delete_event);

        // Now re-filter the completion list
        self.refilter_completion_popup();
    }

    /// Re-filter the completion popup based on current prefix.
    /// If no items match, dismiss the popup.
    fn refilter_completion_popup(&mut self) {
        // Get stored completion items
        let items = match &self.completion_items {
            Some(items) if !items.is_empty() => items.clone(),
            _ => {
                self.hide_popup();
                return;
            }
        };

        // Get current prefix
        let (word_start, cursor_pos) = {
            let state = self.active_state();
            let cursor_pos = state.cursors.primary().position;
            let word_start = find_completion_word_start(&state.buffer, cursor_pos);
            (word_start, cursor_pos)
        };

        let prefix = if word_start < cursor_pos {
            self.active_state_mut()
                .get_text_range(word_start, cursor_pos)
                .to_lowercase()
        } else {
            String::new()
        };

        // Filter items
        let filtered_items: Vec<&lsp_types::CompletionItem> = if prefix.is_empty() {
            items.iter().collect()
        } else {
            items
                .iter()
                .filter(|item| {
                    item.label.to_lowercase().starts_with(&prefix)
                        || item
                            .filter_text
                            .as_ref()
                            .map(|ft| ft.to_lowercase().starts_with(&prefix))
                            .unwrap_or(false)
                })
                .collect()
        };

        // If no items match, dismiss popup
        if filtered_items.is_empty() {
            self.hide_popup();
            self.completion_items = None;
            return;
        }

        // Get current selection to try preserving it
        let current_selection = self
            .active_state()
            .popups
            .top()
            .and_then(|p| p.selected_item())
            .map(|item| item.text.clone());

        // Convert to PopupListItem
        use crate::view::popup::PopupListItem;

        let popup_items: Vec<PopupListItem> = filtered_items
            .iter()
            .map(|item| {
                let text = item.label.clone();
                let detail = item.detail.clone();
                let icon = match item.kind {
                    Some(lsp_types::CompletionItemKind::FUNCTION)
                    | Some(lsp_types::CompletionItemKind::METHOD) => Some("λ".to_string()),
                    Some(lsp_types::CompletionItemKind::VARIABLE) => Some("v".to_string()),
                    Some(lsp_types::CompletionItemKind::STRUCT)
                    | Some(lsp_types::CompletionItemKind::CLASS) => Some("S".to_string()),
                    Some(lsp_types::CompletionItemKind::CONSTANT) => Some("c".to_string()),
                    Some(lsp_types::CompletionItemKind::KEYWORD) => Some("k".to_string()),
                    _ => None,
                };

                let mut list_item = PopupListItem::new(text);
                if let Some(detail) = detail {
                    list_item = list_item.with_detail(detail);
                }
                if let Some(icon) = icon {
                    list_item = list_item.with_icon(icon);
                }
                let data = item
                    .insert_text
                    .clone()
                    .or_else(|| Some(item.label.clone()));
                if let Some(data) = data {
                    list_item = list_item.with_data(data);
                }
                list_item
            })
            .collect();

        // Try to preserve selection
        let selected = current_selection
            .and_then(|sel| popup_items.iter().position(|item| item.text == sel))
            .unwrap_or(0);

        // Update the popup with filtered items
        use crate::model::event::{
            PopupContentData, PopupData, PopupListItemData, PopupPositionData,
        };

        let popup_data = PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: popup_items
                    .into_iter()
                    .map(|item| PopupListItemData {
                        text: item.text,
                        detail: item.detail,
                        icon: item.icon,
                        data: item.data,
                    })
                    .collect(),
                selected,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        };

        // Close old popup and show new one
        self.hide_popup();
        self.active_state_mut()
            .apply(&crate::model::event::Event::ShowPopup { popup: popup_data });
    }
}
