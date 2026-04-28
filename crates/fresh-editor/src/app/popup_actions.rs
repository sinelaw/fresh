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
    /// Dispatches by reading the currently-focused popup's `PopupResolver`
    /// — the popup itself carries its own "how do I confirm?" identity.
    /// This eliminates the old side-channel cascade where `pending_X:
    /// Option<...>` flags competed for precedence: two popups coexisting
    /// (e.g. plugin action popup on the global stack + LSP auto-prompt
    /// on the buffer stack) would race on whose flag the cascade hit
    /// first, and the wrong branch would claim the key.
    ///
    /// Global popups shadow buffer popups for keyboard focus (see
    /// `input_dispatch::dispatch_modal_input`), so the confirm path
    /// picks the same popup: global first, then the active buffer.
    pub fn handle_popup_confirm(&mut self) -> PopupConfirmResult {
        use crate::view::popup::PopupResolver;

        // Clone the top popup's resolver so we can `match` on it without
        // keeping a borrow on `self.global_popups` / `self.buffers`
        // while the handler mutates the editor.
        let resolver = if self.global_popups.is_visible() {
            self.global_popups.top().map(|p| p.resolver.clone())
        } else {
            self.active_state().popups.top().map(|p| p.resolver.clone())
        };

        match resolver {
            Some(PopupResolver::PluginAction { popup_id }) => {
                let action_id = self
                    .global_popups
                    .top()
                    .or_else(|| self.active_state().popups.top())
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone())
                    .unwrap_or_else(|| "dismissed".to_string());
                self.hide_popup();
                self.plugin_manager.run_hook(
                    "action_popup_result",
                    crate::services::plugins::hooks::HookArgs::ActionPopupResult {
                        popup_id,
                        action_id,
                    },
                );
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::LspStatus) => {
                let action_key = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone());
                self.hide_popup();
                // User picked a row → end the auto-prompt cycle for
                // this language.
                let active = self.active_buffer();
                if let Some(language) = self.buffers.get(&active).map(|s| s.language.clone()) {
                    self.pending_auto_start_prompts.remove(&language);
                    self.auto_start_prompted_languages.insert(language);
                }
                if let Some(key) = action_key {
                    self.handle_lsp_status_action(&key);
                }
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::CodeAction) => {
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
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::LspConfirm { language }) => {
                let action = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone());
                if let Some(action) = action {
                    self.hide_popup();
                    self.handle_lsp_confirmation_response(&language, &action);
                    PopupConfirmResult::EarlyReturn
                } else {
                    self.hide_popup();
                    PopupConfirmResult::EarlyReturn
                }
            }

            Some(PopupResolver::RemoteIndicator) => {
                let action_key = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone());
                self.hide_popup();
                if let Some(key) = action_key {
                    self.handle_remote_indicator_action(&key);
                }
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::Completion) => {
                // Grab the selected item's label + insert-text before we
                // mutate the popup stack — insert_completion_text edits
                // the buffer, which invalidates the borrow.
                let completion_info = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.selected_item())
                    .map(|item| (item.text.clone(), item.data.clone()));
                if let Some((label, insert_text)) = completion_info {
                    if let Some(text) = insert_text {
                        self.insert_completion_text(text);
                    }
                    self.apply_completion_additional_edits(&label);
                }
                self.hide_popup();
                PopupConfirmResult::Done
            }

            Some(PopupResolver::None) | None => {
                self.hide_popup();
                PopupConfirmResult::Done
            }
        }
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
    /// If the item already has additional_text_edits, apply them directly.
    /// If not and the server supports completionItem/resolve, send a resolve request
    /// so the server can fill them in (the response is handled asynchronously).
    fn apply_completion_additional_edits(&mut self, label: &str) {
        // Find the matching CompletionItem from stored items
        let item = self
            .completion_items
            .as_ref()
            .and_then(|items| items.iter().find(|item| item.label == label).cloned());

        let Some(item) = item else { return };

        if let Some(edits) = &item.additional_text_edits {
            if !edits.is_empty() {
                tracing::info!(
                    "Applying {} additional text edits from completion '{}'",
                    edits.len(),
                    label
                );
                let buffer_id = self.active_buffer();
                if let Err(e) = self.apply_lsp_text_edits(buffer_id, edits.clone()) {
                    tracing::error!("Failed to apply completion additional_text_edits: {}", e);
                }
                return;
            }
        }

        // No additional_text_edits present — try resolve if server supports it
        if self.server_supports_completion_resolve() {
            tracing::info!(
                "Completion '{}' has no additional_text_edits, sending completionItem/resolve",
                label
            );
            self.send_completion_resolve(item);
        }
    }

    /// Handle PopupCancel action.
    ///
    /// Mirrors `handle_popup_confirm`: dispatch on the focused popup's
    /// `PopupResolver`. Each flavour does its own cleanup; no
    /// precedence between unrelated popup types.
    pub fn handle_popup_cancel(&mut self) {
        use crate::view::popup::PopupResolver;

        let resolver = if self.global_popups.is_visible() {
            self.global_popups.top().map(|p| p.resolver.clone())
        } else {
            self.active_state().popups.top().map(|p| p.resolver.clone())
        };

        match resolver {
            Some(PopupResolver::PluginAction { popup_id }) => {
                tracing::info!(
                    "handle_popup_cancel: dismissing action popup id={}",
                    popup_id
                );
                self.hide_popup();
                self.plugin_manager.run_hook(
                    "action_popup_result",
                    crate::services::plugins::hooks::HookArgs::ActionPopupResult {
                        popup_id,
                        action_id: "dismissed".to_string(),
                    },
                );
            }

            Some(PopupResolver::LspStatus) => {
                // End the auto-prompt cycle for the active buffer's
                // language so re-focusing another file of the same
                // language doesn't re-pop it.
                let active = self.active_buffer();
                if let Some(language) = self.buffers.get(&active).map(|s| s.language.clone()) {
                    self.pending_auto_start_prompts.remove(&language);
                    self.auto_start_prompted_languages.insert(language);
                }
                self.hide_popup();
            }

            Some(PopupResolver::CodeAction) => {
                self.pending_code_actions = None;
                self.hide_popup();
            }

            Some(PopupResolver::LspConfirm { language: _ }) => {
                self.set_status_message(t!("lsp.startup_cancelled_msg").to_string());
                self.hide_popup();
            }

            Some(PopupResolver::Completion) => {
                self.hide_popup();
                self.completion_items = None;
            }

            Some(PopupResolver::RemoteIndicator) => {
                self.hide_popup();
            }

            Some(PopupResolver::None) | None => {
                self.hide_popup();
                self.completion_items = None;
            }
        }
    }

    /// Get the formatted key hint for the completion accept action (e.g. "Tab").
    /// Looks up the keybinding for the ConfirmPopup/Tab action in completion context.
    pub(crate) fn completion_accept_key_hint(&self) -> Option<String> {
        // Tab is hardcoded in the completion input handler, so default to "Tab"
        Some("Tab".to_string())
    }

    /// Format the keybinding currently bound to `Action::PopupFocus`,
    /// rendered into popup titles when the popup is unfocused so the
    /// user can see how to grab the keyboard. Falls back to `Alt+T`
    /// (the default) when no binding is registered.
    pub(crate) fn popup_focus_key_hint(&self) -> Option<String> {
        let kb = self.keybindings.read().ok()?;
        // The action is meant to fire in any context the user is in
        // when an unfocused popup is on screen — i.e. Normal,
        // FileExplorer, Terminal, etc. The keymap registers it under
        // `KeyContext::Global` so it applies uniformly; look it up
        // there first, then fall through to `Normal` for users who
        // override the binding without specifying `when`.
        kb.get_keybinding_for_action(
            &crate::input::keybindings::Action::PopupFocus,
            crate::input::keybindings::KeyContext::Global,
        )
        .or_else(|| {
            kb.get_keybinding_for_action(
                &crate::input::keybindings::Action::PopupFocus,
                crate::input::keybindings::KeyContext::Normal,
            )
        })
        .or_else(|| Some("Alt+T".to_string()))
    }

    /// Mark the topmost visible popup as focused so subsequent key
    /// events route into the popup's input handler.
    ///
    /// Editor-level (global) popups shadow buffer popups for keyboard
    /// focus, mirroring the priority encoded in `dispatch_modal_input`,
    /// so we focus whichever popup the user actually sees.
    ///
    /// No-op when no popup is visible — the user pressing the
    /// focus-popup key with nothing to focus shouldn't error or steal
    /// the keystroke from the buffer.
    pub fn handle_popup_focus(&mut self) {
        if let Some(popup) = self.global_popups.top_mut() {
            popup.focused = true;
            return;
        }
        if let Some(popup) = self.active_state_mut().popups.top_mut() {
            popup.focused = true;
        }
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
        // Get stored LSP completion items (may be empty if no LSP).
        let lsp_items = self.completion_items.clone().unwrap_or_default();

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

        // Filter LSP items
        let filtered_lsp: Vec<&lsp_types::CompletionItem> = if prefix.is_empty() {
            lsp_items.iter().collect()
        } else {
            lsp_items
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

        // Build combined items: LSP first, then buffer-word results.
        let mut all_popup_items = lsp_items_to_popup_items(&filtered_lsp);
        let buffer_word_items = self.get_buffer_completion_popup_items();
        let lsp_labels: std::collections::HashSet<String> = all_popup_items
            .iter()
            .map(|i| i.text.to_lowercase())
            .collect();
        all_popup_items.extend(
            buffer_word_items
                .into_iter()
                .filter(|item| !lsp_labels.contains(&item.text.to_lowercase())),
        );

        // If no items match from either source, dismiss popup.
        if all_popup_items.is_empty() {
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
            .and_then(|sel| all_popup_items.iter().position(|item| item.text == sel))
            .unwrap_or(0);

        let popup_data = build_completion_popup_from_items(all_popup_items, selected);
        let accept_hint = self.completion_accept_key_hint();

        // Close old popup and show new one
        self.hide_popup();
        let buffer_id = self.active_buffer();
        let state = self.buffers.get_mut(&buffer_id).unwrap();
        let mut popup_obj = crate::state::convert_popup_data_to_popup(&popup_data);
        popup_obj.accept_key_hint = accept_hint;
        popup_obj.resolver = crate::view::popup::PopupResolver::Completion;
        state.popups.show_or_replace(popup_obj);
    }
}

/// Build a completion popup from a combined list of already-converted items.
///
/// Used when merging LSP results + buffer-word results into a single popup.
pub(crate) fn build_completion_popup_from_items(
    items: Vec<crate::model::event::PopupListItemData>,
    selected: usize,
) -> crate::model::event::PopupData {
    use crate::model::event::{PopupContentData, PopupKindHint, PopupPositionData};

    crate::model::event::PopupData {
        kind: PopupKindHint::Completion,
        title: None,
        description: None,
        transient: false,
        content: PopupContentData::List { items, selected },
        position: PopupPositionData::BelowCursor,
        width: 50,
        max_height: 15,
        bordered: true,
    }
}

/// Convert LSP `CompletionItem`s to `PopupListItemData`s.
pub(crate) fn lsp_items_to_popup_items(
    items: &[&lsp_types::CompletionItem],
) -> Vec<crate::model::event::PopupListItemData> {
    use crate::model::event::PopupListItemData;

    items
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
        .collect()
}
