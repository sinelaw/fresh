//! Popup action handlers.
//!
//! This module contains handlers for popup-related actions like confirmation and cancellation.

use super::Editor;
use crate::app::window::LspCompletionCandidate;
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
                self.plugin_manager.read().unwrap().run_hook(
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
                self.active_window_mut().pending_code_actions = None;
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

            Some(PopupResolver::ReadOnly) => {
                let action_key = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone());
                self.hide_popup();
                if let Some(key) = action_key {
                    self.handle_read_only_menu_action(&key);
                }
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::Update) => {
                let action_key = self
                    .active_state()
                    .popups
                    .top()
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone());
                self.hide_popup();
                if let Some(key) = action_key {
                    self.handle_update_menu_action(&key);
                }
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::WorkspaceTrust) => {
                // The trust prompt lives on the global stack; read its
                // selection there (global-first, matching the resolver lookup).
                let action_key = self
                    .global_popups
                    .top()
                    .or_else(|| self.active_state().popups.top())
                    .and_then(|p| p.selected_item())
                    .and_then(|item| item.data.clone());
                self.hide_popup();
                if let Some(key) = action_key {
                    self.handle_workspace_trust_action(&key);
                }
                PopupConfirmResult::EarlyReturn
            }

            Some(PopupResolver::Completion) => {
                // Grab the selected *row* — its insert-text and its index —
                // before we mutate the popup stack: insert_completion_text
                // edits the buffer, which invalidates the borrow.
                //
                // The row index is what identifies the accepted candidate.
                // Labels don't: an auto-import list offers one `HashMap`
                // row per crate exporting one, and looking the item back
                // up by label applied the *first* such row's import
                // regardless of which one was selected (#2952).
                let selection = self.active_state().popups.top().and_then(|p| {
                    p.selected_index()
                        .zip(p.selected_item().map(|item| item.data.clone()))
                });
                if let Some((row, insert_text)) = selection {
                    if let Some(text) = insert_text {
                        self.insert_completion_text(text);
                    }
                    self.apply_completion_additional_edits(row);
                }
                self.hide_popup();
                PopupConfirmResult::Done
            }

            Some(PopupResolver::SettingsSaveError { layer }) => {
                // Acknowledging the save-failed popup opens the offending
                // config file so the user can fix the syntax error.
                self.hide_popup();
                if let Err(e) = self.open_config_file(layer) {
                    tracing::warn!("Failed to open config file after save error: {}", e);
                }
                PopupConfirmResult::Done
            }

            Some(PopupResolver::None) | None => {
                self.hide_popup();
                PopupConfirmResult::Done
            }
        }
    }

    /// Insert completion text, replacing the word prefix at *every* cursor.
    /// If the text contains LSP snippet syntax, it will be expanded.
    ///
    /// Multi-cursor: each cursor's own word prefix is replaced, so cursors
    /// stay in lock-step after the accept (issue #1901, accept path). All
    /// per-cursor edits go through `apply_events_as_bulk_edit` so undo is
    /// atomic.
    fn insert_completion_text(&mut self, text: String) {
        use crate::model::event::CursorId;

        // Check if this is a snippet and expand it
        let (insert_text, cursor_offset) = if is_snippet(&text) {
            let expanded = expand_snippet(&text);
            (expanded.text, Some(expanded.cursor_offset))
        } else {
            (text, None)
        };

        // Collect per-cursor data: id, current position, word_start, prefix text.
        let cursor_data: Vec<(CursorId, usize, usize, String)> = {
            let positions: Vec<(CursorId, usize)> = self
                .active_cursors()
                .iter()
                .map(|(id, c)| (id, c.position))
                .collect();
            positions
                .into_iter()
                .map(|(id, pos)| {
                    let word_start = {
                        let state = self.active_state();
                        find_completion_word_start(&state.buffer, pos)
                    };
                    let prefix = if word_start < pos {
                        self.active_state_mut().get_text_range(word_start, pos)
                    } else {
                        String::new()
                    };
                    (id, pos, word_start, prefix)
                })
                .collect()
        };

        // Build delete+insert events. `apply_events_as_bulk_edit` sorts by
        // descending position internally, so emission order doesn't matter.
        let mut events: Vec<Event> = Vec::new();
        for (cursor_id, pos, word_start, prefix) in &cursor_data {
            if *word_start < *pos {
                events.push(Event::Delete {
                    range: *word_start..*pos,
                    deleted_text: prefix.clone(),
                    cursor_id: *cursor_id,
                });
            }
            events.push(Event::Insert {
                position: *word_start,
                text: insert_text.clone(),
                cursor_id: *cursor_id,
            });
        }

        if events.is_empty() {
            return;
        }

        let description = "Accept completion".to_string();
        if cursor_data.len() > 1 || events.len() > 1 {
            // Multi-cursor (or replacement = delete+insert): one atomic bulk edit.
            if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
                self.active_event_log_mut().append(bulk_edit);
            }
        } else {
            for event in events {
                self.log_and_apply_event(&event);
            }
        }

        // Snippet placement: after the bulk edit, each cursor sits at the end
        // of its own inserted text; the snippet's $0 sits `cursor_offset` bytes
        // into that text. Walk each cursor back to its $0 placeholder.
        if let Some(offset) = cursor_offset {
            if offset != insert_text.len() {
                let move_events: Vec<Event> = self
                    .active_cursors()
                    .iter()
                    .map(|(cursor_id, cursor)| {
                        let current = cursor.position;
                        let target = current.saturating_sub(insert_text.len()) + offset;
                        Event::MoveCursor {
                            cursor_id,
                            old_position: current,
                            new_position: target,
                            old_anchor: cursor.anchor,
                            new_anchor: None,
                            old_sticky_column: cursor.sticky_column,
                            new_sticky_column: None,
                        }
                    })
                    .collect();
                for event in move_events {
                    self.log_and_apply_event(&event);
                }
            }
        }
    }

    /// Apply additional_text_edits from the accepted completion item (e.g. auto-imports).
    /// If the item already has additional_text_edits, apply them directly.
    /// If not and the server supports completionItem/resolve, send a resolve request
    /// so the server can fill them in (the response is handled asynchronously).
    ///
    /// `popup_row` is the index of the accepted row in the completion popup;
    /// `Window::completion_popup_lsp_items` maps it back to the exact
    /// candidate the row was built from. Rows past the end of that mapping
    /// are buffer-word candidates and carry no LSP edits.
    fn apply_completion_additional_edits(&mut self, popup_row: usize) {
        let candidate = self
            .active_window()
            .completion_popup_lsp_items
            .get(popup_row)
            .cloned();

        let Some(candidate) = candidate else { return };
        let item = &candidate.item;
        let label = item.label.clone();

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

        // No additional_text_edits present — ask the server that offered
        // this candidate to fill them in. `send_completion_resolve` gates
        // on *that* server's capability; a sibling server for the same
        // language advertising resolve is not an answer.
        tracing::info!(
            "Completion '{}' has no additional_text_edits, sending completionItem/resolve",
            label
        );
        self.active_window_mut().send_completion_resolve(&candidate);
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
                self.plugin_manager.read().unwrap().run_hook(
                    "action_popup_result",
                    crate::services::plugins::hooks::HookArgs::ActionPopupResult {
                        popup_id,
                        action_id: "dismissed".to_string(),
                    },
                );
            }

            Some(PopupResolver::LspStatus) => {
                self.hide_popup();
            }

            Some(PopupResolver::CodeAction) => {
                self.active_window_mut().pending_code_actions = None;
                self.hide_popup();
            }

            Some(PopupResolver::LspConfirm { language: _ }) => {
                self.set_status_message(t!("lsp.startup_cancelled_msg").to_string());
                self.hide_popup();
            }

            Some(PopupResolver::Completion) => {
                self.hide_popup();
                self.active_window_mut().clear_completion_items();
            }

            Some(PopupResolver::RemoteIndicator) => {
                self.hide_popup();
            }

            Some(PopupResolver::ReadOnly) => {
                self.hide_popup();
            }

            Some(PopupResolver::Update) => {
                self.hide_popup();
            }

            Some(PopupResolver::WorkspaceTrust) => {
                // The trust prompt is a forced choice: there is no "undecided"
                // outcome, so Escape does nothing. The user must pick Trust /
                // Restricted / Blocked (each records a concrete decision).
            }

            Some(PopupResolver::SettingsSaveError { layer }) => {
                // Acknowledging the save-failed popup opens the offending
                // config file so the user can fix the syntax error.
                self.hide_popup();
                if let Err(e) = self.open_config_file(layer) {
                    tracing::warn!("Failed to open config file after save error: {}", e);
                }
            }

            Some(PopupResolver::None) | None => {
                self.hide_popup();
                self.active_window_mut().clear_completion_items();
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
        // The keymap registers `popup_focus` in the `Normal` and
        // `FileExplorer` contexts (not `Global`) so a user's own
        // `alt+t` rebinding in those same contexts wins at the same
        // precedence level — a Global default would shadow the
        // override and silently swallow the user's keystroke. Look up
        // Normal first (the most likely place a user is when the
        // popup pops up), then fall through to FileExplorer, and
        // finally to a hard-coded `Alt+T` so the title is never an
        // empty parenthetical.
        kb.get_keybinding_for_action(
            &crate::input::keybindings::Action::PopupFocus,
            crate::input::keybindings::KeyContext::Normal,
        )
        .or_else(|| {
            kb.get_keybinding_for_action(
                &crate::input::keybindings::Action::PopupFocus,
                crate::input::keybindings::KeyContext::FileExplorer,
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
    /// Inserts the character at every cursor and re-filters the completion list.
    ///
    /// Routes through `Action::InsertChar` so multi-cursor edits land in lock-
    /// step with normal typing: secondary cursors stay in sync with the
    /// primary one (issue #1901) and a single bulk-edit goes into the undo log.
    pub fn handle_popup_type_char(&mut self, c: char) {
        use crate::input::keybindings::Action;

        if let Some(events) = self
            .active_window_mut()
            .action_to_events(Action::InsertChar(c))
        {
            if events.len() > 1 {
                let description = format!("Insert '{}'", c);
                if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
                    self.active_event_log_mut().append(bulk_edit);
                }
            } else {
                for event in events {
                    self.log_and_apply_event(&event);
                }
            }
        }

        self.refilter_completion_popup();
    }

    /// Handle backspace while completion popup is open.
    /// Deletes one character behind every cursor and re-filters the
    /// completion list.
    ///
    /// Routes through `Action::DeleteBackward` so multi-cursor edits stay in
    /// sync (issue #1901). The action handler already no-ops cursors at the
    /// start of the buffer.
    pub fn handle_popup_backspace(&mut self) {
        use crate::input::keybindings::Action;

        if let Some(events) = self
            .active_window_mut()
            .action_to_events(Action::DeleteBackward)
        {
            if events.len() > 1 {
                if let Some(bulk_edit) =
                    self.apply_events_as_bulk_edit(events, "Backspace".to_string())
                {
                    self.active_event_log_mut().append(bulk_edit);
                }
            } else {
                for event in events {
                    self.log_and_apply_event(&event);
                }
            }
        }

        self.refilter_completion_popup();
    }

    /// Lowercased word prefix at the primary cursor — what completion
    /// candidates are filtered against.
    pub(crate) fn completion_word_prefix(&mut self) -> String {
        let (word_start, cursor_pos) = {
            let cursor_pos = self.active_cursors().primary().position;
            let state = self.active_state();
            let word_start = find_completion_word_start(&state.buffer, cursor_pos);
            (word_start, cursor_pos)
        };

        if word_start < cursor_pos {
            self.active_state_mut()
                .get_text_range(word_start, cursor_pos)
                .to_lowercase()
        } else {
            String::new()
        }
    }

    /// Build the rows of the completion popup for the word prefix at the
    /// cursor: the stored LSP candidates that still match, followed by
    /// buffer-word candidates the server didn't already offer.
    ///
    /// This is the only writer of `Window::completion_popup_lsp_items`, and
    /// it fills it from the very list it converts into rows — so the accept
    /// path can turn "the selected row" back into "the candidate that row
    /// was built from" without guessing. Recovering the item by label
    /// instead is what shipped the wrong auto-import in #2952: labels
    /// repeat across auto-import candidates.
    pub(crate) fn build_completion_popup_rows(
        &mut self,
    ) -> Vec<crate::model::event::PopupListItemData> {
        let prefix = self.completion_word_prefix();

        let matching: Vec<LspCompletionCandidate> = self
            .active_window()
            .completion_items
            .iter()
            .flatten()
            .filter(|candidate| completion_matches_prefix(&candidate.item, &prefix))
            .cloned()
            .collect();

        let mut rows =
            lsp_items_to_popup_items(&matching.iter().map(|c| &c.item).collect::<Vec<_>>());
        self.active_window_mut().completion_popup_lsp_items = matching;

        // Buffer-word candidates go below, minus anything the server
        // already offers under the same label.
        let lsp_labels: std::collections::HashSet<String> =
            rows.iter().map(|i| i.text.to_lowercase()).collect();
        let buffer_word_items = self.get_buffer_completion_popup_items();
        rows.extend(
            buffer_word_items
                .into_iter()
                .filter(|item| !lsp_labels.contains(&item.text.to_lowercase())),
        );
        rows
    }

    /// The candidate behind the highlighted completion row, described in
    /// terms that survive rebuilding the rows.
    ///
    /// Read *before* `build_completion_popup_rows` runs, since that call
    /// overwrites the row → item mapping this reads.
    fn selected_completion_row(&self) -> Option<SelectedCompletionRow> {
        let popup = self.active_state().popups.top()?;
        let row = popup.selected_index()?;
        match self.active_window().completion_popup_lsp_items.get(row) {
            Some(item) => Some(SelectedCompletionRow::Lsp(Box::new(item.clone()))),
            // Past the LSP rows: a buffer-word row.
            None => popup
                .selected_item()
                .map(|item| SelectedCompletionRow::BufferWord(item.text.clone())),
        }
    }

    /// Where the previously selected candidate ended up among the freshly
    /// built `rows`, or `None` if the new prefix filtered it out.
    fn completion_row_of(
        &self,
        previous: &SelectedCompletionRow,
        rows: &[crate::model::event::PopupListItemData],
    ) -> Option<usize> {
        let lsp_rows = &self.active_window().completion_popup_lsp_items;
        match previous {
            SelectedCompletionRow::Lsp(item) => lsp_rows.iter().position(|i| i == item.as_ref()),
            // Buffer-word rows carry no payload beyond their text, so two
            // of them with the same text are interchangeable and the text
            // is identity enough — but only *among the buffer-word rows*:
            // an LSP row sharing that label is a different candidate, with
            // its own import.
            SelectedCompletionRow::BufferWord(text) => rows
                .iter()
                .enumerate()
                .skip(lsp_rows.len())
                .find(|(_, row)| row.text == *text)
                .map(|(index, _)| index),
        }
    }

    /// Re-filter the completion popup based on current prefix.
    /// If no items match, dismiss the popup.
    fn refilter_completion_popup(&mut self) {
        // What the user has highlighted, captured before the rows (and the
        // mapping behind them) are rebuilt.
        let previous_selection = self.selected_completion_row();

        let all_popup_items = self.build_completion_popup_rows();

        // If no items match from either source, dismiss popup.
        if all_popup_items.is_empty() {
            self.hide_popup();
            self.active_window_mut().clear_completion_items();
            return;
        }

        // Keep the highlight on the candidate the user picked, identified
        // by the *item* behind the row rather than by its label: an
        // auto-import list offers one `HashMap` row per crate exporting
        // one, and matching on the label snapped the highlight back to the
        // first of them on every further keystroke (#2952). A candidate
        // the new prefix filtered out falls back to the first row.
        let selected = previous_selection
            .and_then(|previous| self.completion_row_of(&previous, &all_popup_items))
            .unwrap_or(0);

        let popup_data = build_completion_popup_from_items(all_popup_items, selected);
        let accept_hint = self.completion_accept_key_hint();
        let (popup_bg, popup_border_fg) = {
            let theme = self.theme();
            (theme.popup_bg, theme.popup_border_fg)
        };

        // Close old popup and show new one
        self.hide_popup();
        let buffer_id = self.active_buffer();
        let state = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
            .unwrap();
        let mut popup_obj =
            crate::state::convert_popup_data_to_popup(&popup_data, popup_bg, popup_border_fg);
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

/// The highlighted completion row, in terms that outlive the rows
/// themselves — so typing another character can put the highlight back on
/// the same candidate rather than on the first row that happens to share
/// its label.
enum SelectedCompletionRow {
    /// An LSP candidate, identified by the candidate itself. Boxed because
    /// a `CompletionItem` is large next to the other variant.
    Lsp(Box<LspCompletionCandidate>),
    /// A buffer-word row, identified by its text (it has no other payload).
    BufferWord(String),
}

/// Whether a completion candidate survives the word prefix at the cursor.
///
/// `prefix` must already be lowercased (see `Editor::completion_word_prefix`).
pub(crate) fn completion_matches_prefix(item: &lsp_types::CompletionItem, prefix: &str) -> bool {
    prefix.is_empty()
        || item.label.to_lowercase().starts_with(prefix)
        || item
            .filter_text
            .as_ref()
            .map(|ft| ft.to_lowercase().starts_with(prefix))
            .unwrap_or(false)
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

            // Prefer `labelDetails` for the dimmed text next to the label:
            // we advertise `labelDetailsSupport`, so servers put the
            // import path of an auto-import candidate there (rust-analyzer:
            // "(use std::collections::HashMap)") while the label stays the
            // bare identifier the accept path inserts. Fall back to `detail`
            // when a server doesn't use labelDetails.
            let detail = item
                .label_details
                .as_ref()
                .and_then(|ld| {
                    let text = match (&ld.detail, &ld.description) {
                        (Some(d), Some(desc)) => format!("{} {}", d.trim_start(), desc),
                        (Some(d), None) => d.trim_start().to_string(),
                        (None, Some(desc)) => desc.clone(),
                        (None, None) => return None,
                    };
                    Some(text)
                })
                .or_else(|| item.detail.clone());

            PopupListItemData {
                text: item.label.clone(),
                detail,
                icon,
                data: item
                    .insert_text
                    .clone()
                    .or_else(|| Some(item.label.clone())),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Auto-import candidates carry their import path in `labelDetails`
    /// (we advertise `labelDetailsSupport`, sinelaw/fresh#2603). The popup
    /// item must show that path as the dimmed detail while keeping the
    /// bare identifier as both the visible label and the inserted text.
    #[test]
    fn lsp_items_to_popup_items_renders_label_details() {
        let item = lsp_types::CompletionItem {
            label: "HashMap".to_string(),
            label_details: Some(lsp_types::CompletionItemLabelDetails {
                detail: Some(" (use std::collections::HashMap)".to_string()),
                description: None,
            }),
            kind: Some(lsp_types::CompletionItemKind::STRUCT),
            ..Default::default()
        };
        let popup_items = lsp_items_to_popup_items(&[&item]);
        assert_eq!(popup_items.len(), 1);
        assert_eq!(popup_items[0].text, "HashMap");
        assert_eq!(
            popup_items[0].detail.as_deref(),
            Some("(use std::collections::HashMap)")
        );
        // The accepted insert text must stay the bare identifier.
        assert_eq!(popup_items[0].data.as_deref(), Some("HashMap"));
    }

    /// Servers that don't use `labelDetails` keep their `detail` rendering.
    #[test]
    fn lsp_items_to_popup_items_falls_back_to_detail() {
        let item = lsp_types::CompletionItem {
            label: "test_function".to_string(),
            detail: Some("fn test_function()".to_string()),
            ..Default::default()
        };
        let popup_items = lsp_items_to_popup_items(&[&item]);
        assert_eq!(popup_items[0].detail.as_deref(), Some("fn test_function()"));
    }
}
