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

        // Check if this is a code action popup
        if self.pending_code_actions.is_some() {
            let selected_index = self
                .active_state()
                .popups
                .top()
                .and_then(|p| p.selected_item())
                .and_then(|item| item.data.as_ref())
                .and_then(|data| data.parse::<usize>().ok());

            self.hide_popup();
            if let Some(index) = selected_index {
                self.execute_code_action(index);
            }
            self.pending_code_actions = None;
            return PopupConfirmResult::EarlyReturn;
        }

        // Check if this is an LSP confirmation popup
        if self.pending_lsp_confirmation.is_some() {
            let action = self
                .active_state()
                .popups
                .top()
                .and_then(|p| p.selected_item())
                .and_then(|item| item.data.clone());
            if let Some(action) = action {
                self.hide_popup();
                self.handle_lsp_confirmation_response(&action);
                return PopupConfirmResult::EarlyReturn;
            }
        }

        // If it's a completion popup, insert the selected item
        let completion_info = self
            .active_state()
            .popups
            .top()
            .filter(|p| p.kind == crate::view::popup::PopupKind::Completion)
            .and_then(|p| p.selected_item())
            .map(|item| (item.text.clone(), item.data.clone()));

        // Perform the completion if we have text
        if let Some((label, insert_text)) = completion_info {
            if let Some(text) = insert_text {
                self.insert_completion_text(text);
            }

            // Apply additional_text_edits (e.g., auto-imports) from the matching CompletionItem
            self.apply_completion_additional_edits(&label);
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
            let cursors = self.active_cursors();
            let cursor_id = cursors.primary_id();
            let cursor_pos = cursors.primary().position;
            let state = self.active_state();
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

            self.log_and_apply_event(&delete_event);

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

        self.log_and_apply_event(&insert_event);

        // If this was a snippet, position cursor at the snippet's $0 location
        if let Some(offset) = cursor_offset {
            let new_cursor_pos = insert_pos + offset;
            // Get current cursor position after the insert
            let current_pos = self.active_cursors().primary().position;
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
                let split_id = self.split_manager.active_split();
                let buffer_id = self.active_buffer();
                let state = self.buffers.get_mut(&buffer_id).unwrap();
                let cursors = &mut self.split_view_states.get_mut(&split_id).unwrap().cursors;
                state.apply(cursors, &move_event);
            }
        }
    }

    /// Apply additional_text_edits from the accepted completion item (e.g. auto-imports).
    /// Finds the matching CompletionItem by label from the stored completion_items.
    fn apply_completion_additional_edits(&mut self, label: &str) {
        // Find the matching CompletionItem from stored items
        let additional_edits = self
            .completion_items
            .as_ref()
            .and_then(|items| items.iter().find(|item| item.label == label))
            .and_then(|item| item.additional_text_edits.clone());

        if let Some(edits) = additional_edits {
            if !edits.is_empty() {
                tracing::info!(
                    "Applying {} additional text edits from completion '{}'",
                    edits.len(),
                    label
                );
                let buffer_id = self.active_buffer();
                if let Err(e) = self.apply_lsp_text_edits(buffer_id, edits) {
                    tracing::error!("Failed to apply completion additional_text_edits: {}", e);
                }
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

        if self.pending_code_actions.is_some() {
            self.pending_code_actions = None;
            self.hide_popup();
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
            let cursors = self.active_cursors();
            (cursors.primary_id(), cursors.primary().position)
        };

        let insert_event = Event::Insert {
            position: cursor_pos,
            text: c.to_string(),
            cursor_id,
        };

        self.log_and_apply_event(&insert_event);

        // Now re-filter the completion list
        self.refilter_completion_popup();
    }

    /// Handle backspace while completion popup is open.
    /// Deletes a character and re-filters the completion list.
    pub fn handle_popup_backspace(&mut self) {
        let (cursor_id, cursor_pos) = {
            let cursors = self.active_cursors();
            (cursors.primary_id(), cursors.primary().position)
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

        self.log_and_apply_event(&delete_event);

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
            let cursor_pos = self.active_cursors().primary().position;
            let state = self.active_state();
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

        // Try to preserve selection
        let selected = current_selection
            .and_then(|sel| filtered_items.iter().position(|item| item.label == sel))
            .unwrap_or(0);

        let popup_data = build_completion_popup(&filtered_items, selected);

        // Close old popup and show new one
        self.hide_popup();
        let split_id = self.split_manager.active_split();
        let buffer_id = self.active_buffer();
        let state = self.buffers.get_mut(&buffer_id).unwrap();
        let cursors = &mut self.split_view_states.get_mut(&split_id).unwrap().cursors;
        state.apply(
            cursors,
            &crate::model::event::Event::ShowPopup { popup: popup_data },
        );
    }
}

/// Build a completion `PopupData` from a list of LSP `CompletionItem`s.
///
/// This is the single code path for creating completion popups, used both for
/// the initial LSP completion response and for re-filtering during type-to-filter.
pub(crate) fn build_completion_popup(
    items: &[&lsp_types::CompletionItem],
    selected: usize,
) -> crate::model::event::PopupData {
    use crate::model::event::{
        PopupContentData, PopupKindHint, PopupListItemData, PopupPositionData,
    };

    let list_items: Vec<PopupListItemData> = items
        .iter()
        .map(|item| {
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

            PopupListItemData {
                text: item.label.clone(),
                detail: item.detail.clone(),
                icon,
                data: item
                    .insert_text
                    .clone()
                    .or_else(|| Some(item.label.clone())),
            }
        })
        .collect();

    crate::model::event::PopupData {
        kind: PopupKindHint::Completion,
        title: None,
        description: None,
        transient: false,
        content: PopupContentData::List {
            items: list_items,
            selected,
        },
        position: PopupPositionData::BelowCursor,
        width: 50,
        max_height: 15,
        bordered: true,
    }
}
