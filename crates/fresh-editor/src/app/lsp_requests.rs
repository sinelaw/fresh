//! LSP (Language Server Protocol) request handling for the Editor.
//!
//! This module contains all methods related to LSP operations including:
//! - Completion requests and response handling
//! - Go-to-definition
//! - Hover documentation
//! - Find references
//! - Signature help
//! - Code actions
//! - Rename operations
//! - Inlay hints

use anyhow::Result as AnyhowResult;
use rust_i18n::t;
use std::io;
use std::time::{Duration, Instant};

use lsp_types::TextDocumentContentChangeEvent;

use crate::model::event::{BufferId, Event};
use crate::primitives::word_navigation::{find_word_end, find_word_start};
use crate::view::prompt::{Prompt, PromptType};

use crate::services::lsp::async_handler::LspHandle;
use crate::types::LspFeature;

use super::{uri_to_path, Editor, SemanticTokenRangeRequest};

/// Ensure every line in a docstring is separated by a blank line.
///
/// LSP documentation (e.g. from pyright) often uses single newlines between
/// lines, which markdown treats as soft breaks within one paragraph. This
/// doubles all single newlines so each line becomes its own paragraph with
/// spacing between them.
fn space_doc_paragraphs(text: &str) -> String {
    text.replace("\n\n", "\x00").replace(['\n', '\x00'], "\n\n")
}

/// Whether an LSP range (half-open end, like `[start, end)`) contains the given
/// `(line, character)` LSP position. Zero-length ranges (start == end) are
/// treated as containing their single anchor point so point-style diagnostics
/// still match a hover that lands exactly on them.
fn lsp_range_contains(range: &lsp_types::Range, line: u32, character: u32) -> bool {
    let start = range.start;
    let end = range.end;
    // Before start?
    if line < start.line || (line == start.line && character < start.character) {
        return false;
    }
    // Zero-length range: accept exact anchor match.
    if start.line == end.line && start.character == end.character {
        return line == start.line && character == start.character;
    }
    // After end? (half-open)
    if line > end.line || (line == end.line && character >= end.character) {
        return false;
    }
    true
}

const SEMANTIC_TOKENS_FULL_DEBOUNCE_MS: u64 = 500;
const SEMANTIC_TOKENS_RANGE_DEBOUNCE_MS: u64 = 50;
const SEMANTIC_TOKENS_RANGE_PADDING_LINES: usize = 10;
const FOLDING_RANGES_DEBOUNCE_MS: u64 = 300;

impl Editor {
    /// Handle LSP completion response.
    /// Supports merging from multiple servers: first response creates the menu,
    /// subsequent responses extend it.
    pub(crate) fn handle_completion_response(
        &mut self,
        request_id: u64,
        items: Vec<lsp_types::CompletionItem>,
    ) -> AnyhowResult<()> {
        // Check if this is one of the pending completion requests
        if !self.pending_completion_requests.remove(&request_id) {
            tracing::debug!(
                "Ignoring completion response for outdated request {}",
                request_id
            );
            return Ok(());
        }

        // Update status when all completion responses have arrived
        if self.pending_completion_requests.is_empty() {}

        if items.is_empty() {
            tracing::debug!("No completion items received");
            return Ok(());
        }

        // Get the partial word at cursor to filter completions
        use crate::primitives::word_navigation::find_completion_word_start;
        let cursor_pos = self.active_cursors().primary().position;
        let (word_start, cursor_pos) = {
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

        // Filter completions to match the typed prefix
        let filtered_items: Vec<&lsp_types::CompletionItem> = if prefix.is_empty() {
            // No prefix - show all completions
            items.iter().collect()
        } else {
            // Filter to items that start with the prefix (case-insensitive)
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

        if filtered_items.is_empty() && self.completion_items.is_none() {
            tracing::debug!("No completion items match prefix '{}'", prefix);
            return Ok(());
        }

        // Store/extend original items for type-to-filter (merge from multiple servers)
        match &mut self.completion_items {
            Some(existing) => {
                existing.extend(items);
                tracing::debug!("Extended completion items, now {} total", existing.len());
            }
            None => {
                self.completion_items = Some(items);
            }
        }

        // Rebuild popup from ALL merged items (not just the new batch)
        let all_items = self.completion_items.as_ref().unwrap();
        let all_filtered: Vec<&lsp_types::CompletionItem> = if prefix.is_empty() {
            all_items.iter().collect()
        } else {
            all_items
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

        if all_filtered.is_empty() {
            tracing::debug!("No completion items match prefix '{}'", prefix);
            return Ok(());
        }

        // Build LSP popup items, then append buffer-word items below.
        let mut all_popup_items =
            crate::app::popup_actions::lsp_items_to_popup_items(&all_filtered);
        let buffer_word_items = self.get_buffer_completion_popup_items();
        // Deduplicate: skip buffer-word items whose label already appears in LSP results.
        let lsp_labels: std::collections::HashSet<String> = all_popup_items
            .iter()
            .map(|i| i.text.to_lowercase())
            .collect();
        all_popup_items.extend(
            buffer_word_items
                .into_iter()
                .filter(|item| !lsp_labels.contains(&item.text.to_lowercase())),
        );

        let popup_data =
            crate::app::popup_actions::build_completion_popup_from_items(all_popup_items, 0);
        let accept_hint = self.completion_accept_key_hint();

        {
            let buffer_id = self.active_buffer();
            let state = self.buffers.get_mut(&buffer_id).unwrap();
            // Convert PopupData to Popup and use show_or_replace to avoid stacking
            let mut popup_obj = crate::state::convert_popup_data_to_popup(&popup_data);
            popup_obj.accept_key_hint = accept_hint;
            state.popups.show_or_replace(popup_obj);
        }

        tracing::info!(
            "Showing completion popup with {} items",
            self.completion_items.as_ref().map_or(0, |i| i.len())
        );

        Ok(())
    }

    /// Handle LSP go-to-definition response
    pub(crate) fn handle_goto_definition_response(
        &mut self,
        request_id: u64,
        locations: Vec<lsp_types::Location>,
    ) -> AnyhowResult<()> {
        // Check if this is the pending request
        if self.pending_goto_definition_request != Some(request_id) {
            tracing::debug!(
                "Ignoring go-to-definition response for outdated request {}",
                request_id
            );
            return Ok(());
        }

        self.pending_goto_definition_request = None;

        if locations.is_empty() {
            self.status_message = Some(t!("lsp.no_definition").to_string());
            return Ok(());
        }

        // For now, just jump to the first location
        let location = &locations[0];

        // Convert URI to file path
        if let Ok(path) = uri_to_path(&location.uri) {
            // Open the file
            let buffer_id = match self.open_file(&path) {
                Ok(id) => id,
                Err(e) => {
                    // Check if this is a large file encoding confirmation error
                    if let Some(confirmation) =
                        e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                    {
                        self.start_large_file_encoding_confirmation(confirmation);
                    } else {
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                    return Ok(());
                }
            };

            // Move cursor to the definition position
            let line = location.range.start.line as usize;
            let character = location.range.start.character as usize;

            // Calculate byte position from line and character
            let position = self
                .buffers
                .get(&buffer_id)
                .map(|state| state.buffer.line_col_to_position(line, character));

            if let Some(position) = position {
                // Move cursor - read cursor info from split view state
                let (cursor_id, old_position, old_anchor, old_sticky_column) = {
                    let cursors = self.active_cursors();
                    let primary = cursors.primary();
                    (
                        cursors.primary_id(),
                        primary.position,
                        primary.anchor,
                        primary.sticky_column,
                    )
                };
                let event = crate::model::event::Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: position,
                    old_anchor,
                    new_anchor: None,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for goto definition
                };

                let split_id = self.split_manager.active_split();
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let cursors = &mut self.split_view_states.get_mut(&split_id).unwrap().cursors;
                    state.apply(cursors, &event);
                }
            }

            self.status_message = Some(
                t!(
                    "lsp.jumped_to_definition",
                    path = path.display().to_string(),
                    line = line + 1
                )
                .to_string(),
            );
        } else {
            self.status_message = Some(t!("lsp.cannot_open_definition").to_string());
        }

        Ok(())
    }

    /// Check if there are any pending LSP requests
    pub fn has_pending_lsp_requests(&self) -> bool {
        !self.pending_completion_requests.is_empty()
            || self.pending_goto_definition_request.is_some()
    }

    /// Cancel any pending LSP requests
    /// This should be called when the user performs an action that would make
    /// the pending request's results stale (e.g., cursor movement, text editing)
    pub(crate) fn cancel_pending_lsp_requests(&mut self) {
        // Cancel scheduled (not yet sent) completion trigger
        self.scheduled_completion_trigger = None;
        if !self.pending_completion_requests.is_empty() {
            let ids: Vec<u64> = self.pending_completion_requests.drain().collect();
            for request_id in ids {
                tracing::debug!("Canceling pending LSP completion request {}", request_id);
                self.send_lsp_cancel_request(request_id);
            }
        }
        if let Some(request_id) = self.pending_goto_definition_request.take() {
            tracing::debug!(
                "Canceling pending LSP goto-definition request {}",
                request_id
            );
            // Send cancellation to the LSP server
            self.send_lsp_cancel_request(request_id);
        }
    }

    /// Send a cancel request to the LSP server for a specific request ID
    fn send_lsp_cancel_request(&mut self, request_id: u64) {
        // Get language from buffer state
        let buffer_id = self.active_buffer();
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };

        if let Some(lsp) = self.lsp.as_mut() {
            // Only send cancel if LSP is already running (no need to spawn just to cancel)
            if let Some(handle) = lsp.get_handle_mut(&language) {
                if let Err(e) = handle.cancel_request(request_id) {
                    tracing::warn!("Failed to send LSP cancel request: {}", e);
                } else {
                    tracing::debug!("Sent $/cancelRequest for request_id={}", request_id);
                }
            }
        }
    }

    /// Dispatch an exclusive LSP feature request to the first handle that allows the feature.
    ///
    /// Ensures all handles receive didOpen first, then calls the closure with the first
    /// handle matching the feature filter. For features like hover, definition, rename, etc.
    pub(crate) fn with_lsp_for_buffer<F, R>(
        &mut self,
        buffer_id: BufferId,
        feature: LspFeature,
        f: F,
    ) -> Option<R>
    where
        F: FnOnce(&LspHandle, &lsp_types::Uri, &str) -> R,
    {
        use crate::services::lsp::manager::LspSpawnResult;

        let (uri, language, file_path) = {
            let metadata = self.buffer_metadata.get(&buffer_id)?;
            if !metadata.lsp_enabled {
                return None;
            }
            let uri = metadata.file_uri()?.clone();
            let file_path = metadata.file_path().cloned();
            let language = self.buffers.get(&buffer_id)?.language.clone();
            (uri, language, file_path)
        };

        let lsp = self.lsp.as_mut()?;
        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            return None;
        }

        // Ensure didOpen is sent to all handles
        self.ensure_did_open_all(buffer_id, &uri, &language)?;

        // Dispatch to the first handle that allows this feature
        let lsp = self.lsp.as_mut()?;
        let sh = lsp.handle_for_feature_mut(&language, feature)?;
        Some(f(&sh.handle, &uri, &language))
    }

    /// Dispatch a merged LSP feature request to all handles that allow the feature.
    ///
    /// Ensures all handles receive didOpen first, then calls the closure for each
    /// handle matching the feature filter, collecting all results. For features like
    /// completion, code actions, diagnostics, etc.
    pub(crate) fn with_all_lsp_for_buffer_feature<F, R>(
        &mut self,
        buffer_id: BufferId,
        feature: LspFeature,
        f: F,
    ) -> Vec<R>
    where
        F: Fn(&LspHandle, &lsp_types::Uri, &str) -> R,
    {
        use crate::services::lsp::manager::LspSpawnResult;

        let (uri, language, file_path) = match (|| {
            let metadata = self.buffer_metadata.get(&buffer_id)?;
            if !metadata.lsp_enabled {
                return None;
            }
            let uri = metadata.file_uri()?.clone();
            let file_path = metadata.file_path().cloned();
            let language = self.buffers.get(&buffer_id)?.language.clone();
            Some((uri, language, file_path))
        })() {
            Some(v) => v,
            None => return Vec::new(),
        };

        let lsp = match self.lsp.as_mut() {
            Some(l) => l,
            None => return Vec::new(),
        };
        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            return Vec::new();
        }

        // Ensure didOpen is sent to all handles
        if self
            .ensure_did_open_all(buffer_id, &uri, &language)
            .is_none()
        {
            return Vec::new();
        }

        // Dispatch to all handles that allow this feature
        let lsp = match self.lsp.as_mut() {
            Some(l) => l,
            None => return Vec::new(),
        };
        lsp.handles_for_feature_mut(&language, feature)
            .into_iter()
            .map(|sh| f(&sh.handle, &uri, &language))
            .collect()
    }

    /// Like `with_all_lsp_for_buffer_feature`, but also passes the server name
    /// to the closure for attribution purposes.
    pub(crate) fn with_all_lsp_for_buffer_feature_named<F, R>(
        &mut self,
        buffer_id: BufferId,
        feature: LspFeature,
        f: F,
    ) -> Vec<R>
    where
        F: Fn(&LspHandle, &lsp_types::Uri, &str, &str) -> R,
    {
        use crate::services::lsp::manager::LspSpawnResult;

        let (uri, language, file_path) = match (|| {
            let metadata = self.buffer_metadata.get(&buffer_id)?;
            if !metadata.lsp_enabled {
                return None;
            }
            let uri = metadata.file_uri()?.clone();
            let file_path = metadata.file_path().cloned();
            let language = self.buffers.get(&buffer_id)?.language.clone();
            Some((uri, language, file_path))
        })() {
            Some(v) => v,
            None => return Vec::new(),
        };

        let lsp = match self.lsp.as_mut() {
            Some(l) => l,
            None => return Vec::new(),
        };
        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            return Vec::new();
        }

        if self
            .ensure_did_open_all(buffer_id, &uri, &language)
            .is_none()
        {
            return Vec::new();
        }

        let lsp = match self.lsp.as_mut() {
            Some(l) => l,
            None => return Vec::new(),
        };
        lsp.handles_for_feature_mut(&language, feature)
            .into_iter()
            .map(|sh| f(&sh.handle, &uri, &language, &sh.name))
            .collect()
    }

    /// Ensure didOpen has been sent to all handles for the given buffer's language.
    /// Returns Some(()) on success, None if we can't access required state.
    fn ensure_did_open_all(
        &mut self,
        buffer_id: BufferId,
        uri: &lsp_types::Uri,
        language: &str,
    ) -> Option<()> {
        let lsp = self.lsp.as_mut()?;
        let handle_ids: Vec<u64> = lsp
            .get_handles(language)
            .iter()
            .map(|sh| sh.handle.id())
            .collect();

        let needs_open: Vec<u64> = {
            let metadata = self.buffer_metadata.get(&buffer_id)?;
            handle_ids
                .iter()
                .filter(|id| !metadata.lsp_opened_with.contains(id))
                .copied()
                .collect()
        };

        if !needs_open.is_empty() {
            let text = self.buffers.get(&buffer_id)?.buffer.to_string()?;
            let lsp = self.lsp.as_mut()?;
            for sh in lsp.get_handles_mut(language) {
                if needs_open.contains(&sh.handle.id()) {
                    if let Err(e) =
                        sh.handle
                            .did_open(uri.clone(), text.clone(), language.to_string())
                    {
                        tracing::warn!("Failed to send didOpen to '{}': {}", sh.name, e);
                        continue;
                    }
                    let metadata = self.buffer_metadata.get_mut(&buffer_id)?;
                    metadata.lsp_opened_with.insert(sh.handle.id());
                    tracing::debug!(
                        "Sent didOpen for {} to LSP handle '{}' (language: {})",
                        uri.as_str(),
                        sh.name,
                        language
                    );
                }
            }
        }

        Some(())
    }

    /// Request LSP completion at current cursor position.
    /// Sends completion requests to all eligible servers for merged results.
    pub(crate) fn request_completion(&mut self) {
        // A new completion request starts a fresh batch. Cancel any
        // previous in-flight completion requests so their late responses
        // are ignored (handle_completion_response drops responses whose
        // request_id isn't in pending_completion_requests), and drop any
        // leftover items from a previous popup that was closed via the
        // "pass-through" path (hide_popup() without handle_popup_cancel,
        // e.g. Enter or a non-word character while the popup was open).
        // Without this, the new response would be merged into the stale
        // items by `handle_completion_response`'s extend branch, leading
        // to duplicate / stale entries in the rendered popup — see the
        // regression test in
        // crates/fresh-editor/tests/e2e/lsp_completion_duplicate_entries_1514.rs
        // and sinelaw/fresh#1514.
        if !self.pending_completion_requests.is_empty() {
            let ids: Vec<u64> = self.pending_completion_requests.drain().collect();
            for request_id in ids {
                tracing::debug!(
                    "Canceling previous pending LSP completion request {}",
                    request_id
                );
                self.send_lsp_cancel_request(request_id);
            }
        }
        self.completion_items = None;

        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();

        // Pre-allocate request IDs for all eligible servers
        let base_request_id = self.next_lsp_request_id;
        // Use an atomic counter in the closure
        let counter = std::sync::atomic::AtomicU64::new(0);

        let results = self.with_all_lsp_for_buffer_feature(
            buffer_id,
            LspFeature::Completion,
            |handle, uri, _language| {
                let idx = counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                let request_id = base_request_id + idx;
                let result =
                    handle.completion(request_id, uri.clone(), line as u32, character as u32);
                if result.is_ok() {
                    tracing::info!(
                        "Requested completion at {}:{}:{} (request_id={})",
                        uri.as_str(),
                        line,
                        character,
                        request_id
                    );
                }
                (request_id, result.is_ok())
            },
        );

        let mut sent_ids = Vec::new();
        for (request_id, ok) in &results {
            if *ok {
                sent_ids.push(*request_id);
            }
        }
        // Advance the ID counter past all allocated IDs
        self.next_lsp_request_id = base_request_id + results.len() as u64;

        if !sent_ids.is_empty() {
            self.pending_completion_requests.extend(sent_ids);
        } else {
            // No LSP servers available — show buffer-word completions as popup.
            self.show_buffer_word_completion_popup();
        }
    }

    /// Show a completion popup with buffer-word results only (no LSP).
    ///
    /// Called when no LSP servers are available for the current buffer.
    fn show_buffer_word_completion_popup(&mut self) {
        let items = self.get_buffer_completion_popup_items();
        if items.is_empty() {
            return;
        }

        let popup_data = crate::app::popup_actions::build_completion_popup_from_items(items, 0);
        let accept_hint = self.completion_accept_key_hint();

        let buffer_id = self.active_buffer();
        let state = self.buffers.get_mut(&buffer_id).unwrap();
        let mut popup_obj = crate::state::convert_popup_data_to_popup(&popup_data);
        popup_obj.accept_key_hint = accept_hint;
        state.popups.show_or_replace(popup_obj);
    }

    /// Check if the inserted character should trigger completion
    /// and if so, request completion automatically (possibly after a delay).
    ///
    /// Only triggers when `completion_popup_auto_show` is enabled. Then:
    /// 1. Trigger characters (like `.`, `::`, etc.): immediate if suggest_on_trigger_characters is enabled
    /// 2. Word characters: delayed by quick_suggestions_delay_ms if quick_suggestions is enabled
    ///
    /// This provides VS Code-like behavior where suggestions appear while typing,
    /// with debouncing to avoid spamming the LSP server.
    pub(crate) fn maybe_trigger_completion(&mut self, c: char) {
        // Auto-show must be enabled for any automatic triggering
        if !self.config.editor.completion_popup_auto_show {
            return;
        }

        // Get the active buffer's language
        let language = self.active_state().language.clone();

        // Check if this character is a trigger character for this language
        let is_lsp_trigger = self
            .lsp
            .as_ref()
            .map(|lsp| lsp.is_completion_trigger_char(c, &language))
            .unwrap_or(false);

        // Check if quick suggestions is enabled and this is a word character
        let quick_suggestions_enabled = self.config.editor.quick_suggestions;
        let suggest_on_trigger_chars = self.config.editor.suggest_on_trigger_characters;
        let is_word_char = c.is_alphanumeric() || c == '_';

        // Case 1: Trigger character - immediate trigger (bypasses delay)
        if is_lsp_trigger && suggest_on_trigger_chars {
            tracing::debug!(
                "Trigger character '{}' immediately triggers completion for language {}",
                c,
                language
            );
            // Cancel any pending scheduled trigger
            self.scheduled_completion_trigger = None;
            self.request_completion();
            return;
        }

        // Case 2: Word character with quick suggestions - schedule delayed trigger
        if quick_suggestions_enabled && is_word_char {
            let delay_ms = self.config.editor.quick_suggestions_delay_ms;
            let trigger_time = Instant::now() + Duration::from_millis(delay_ms);

            tracing::debug!(
                "Scheduling completion trigger in {}ms for language {} (char '{}')",
                delay_ms,
                language,
                c
            );

            // Schedule (or reschedule) the completion trigger
            // This effectively debounces - each keystroke resets the timer
            self.scheduled_completion_trigger = Some(trigger_time);
        } else {
            // Non-word, non-trigger character (space, punctuation, etc.) —
            // cancel any pending scheduled trigger so a stale timer from the
            // previous word doesn't fire at the wrong cursor position.
            self.scheduled_completion_trigger = None;
        }
    }

    /// Request LSP go-to-definition at current cursor position
    pub(crate) fn request_goto_definition(&mut self) -> AnyhowResult<()> {
        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(
                buffer_id,
                LspFeature::Definition,
                |handle, uri, _language| {
                    let result = handle.goto_definition(
                        request_id,
                        uri.clone(),
                        line as u32,
                        character as u32,
                    );
                    if result.is_ok() {
                        tracing::info!(
                            "Requested go-to-definition at {}:{}:{}",
                            uri.as_str(),
                            line,
                            character
                        );
                    }
                    result.is_ok()
                },
            )
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_goto_definition_request = Some(request_id);
        }

        Ok(())
    }

    /// Request LSP hover documentation at current cursor position
    pub fn request_hover(&mut self) -> AnyhowResult<()> {
        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Debug: Log the position conversion details
        if let Some(pos) = state.buffer.offset_to_position(cursor_pos) {
            tracing::debug!(
                "Hover request: cursor_byte={}, line={}, byte_col={}, utf16_col={}",
                cursor_pos,
                pos.line,
                pos.column,
                character
            );
        }

        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, LspFeature::Hover, |handle, uri, _language| {
                let result = handle.hover(request_id, uri.clone(), line as u32, character as u32);
                if result.is_ok() {
                    tracing::info!(
                        "Requested hover at {}:{}:{} (byte_pos={})",
                        uri.as_str(),
                        line,
                        character,
                        cursor_pos
                    );
                }
                result.is_ok()
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_hover_request = Some(request_id);
            self.pending_hover_position = Some((line as u32, character as u32));
        }

        Ok(())
    }

    /// Request LSP hover documentation at a specific byte position
    /// Used for mouse-triggered hover
    /// Returns `Ok(true)` if the request was dispatched, `Ok(false)` if no
    /// eligible server was available (e.g. not yet initialized).
    pub(crate) fn request_hover_at_position(&mut self, byte_pos: usize) -> AnyhowResult<bool> {
        // Get the current buffer
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(byte_pos);

        // Debug: Log the position conversion details
        if let Some(pos) = state.buffer.offset_to_position(byte_pos) {
            tracing::trace!(
                "Mouse hover request: byte_pos={}, line={}, byte_col={}, utf16_col={}",
                byte_pos,
                pos.line,
                pos.column,
                character
            );
        }

        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, LspFeature::Hover, |handle, uri, _language| {
                let result = handle.hover(request_id, uri.clone(), line as u32, character as u32);
                if result.is_ok() {
                    tracing::trace!(
                        "Mouse hover requested at {}:{}:{} (byte_pos={})",
                        uri.as_str(),
                        line,
                        character,
                        byte_pos
                    );
                }
                result.is_ok()
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_hover_request = Some(request_id);
            self.pending_hover_position = Some((line as u32, character as u32));
        }

        Ok(sent)
    }

    /// Handle hover response from LSP
    pub(crate) fn handle_hover_response(
        &mut self,
        request_id: u64,
        contents: String,
        is_markdown: bool,
        range: Option<((u32, u32), (u32, u32))>,
    ) {
        // Check if this response is for the current pending request
        if self.pending_hover_request != Some(request_id) {
            tracing::debug!("Ignoring stale hover response: {}", request_id);
            return;
        }

        self.pending_hover_request = None;
        let hover_lsp_position = self.pending_hover_position.take();

        // Gather any diagnostics whose range overlaps the hover position so
        // they can be fused into the top of the hover card. Without this the
        // user has to leave hover and go chase the error elsewhere in the UI
        // even though the cursor is already on the offending symbol.
        let diagnostic_lines = hover_lsp_position
            .map(|pos| self.compose_hover_diagnostic_lines(pos))
            .unwrap_or_default();

        if contents.is_empty() && diagnostic_lines.is_empty() {
            self.set_status_message(t!("lsp.no_hover").to_string());
            self.hover_symbol_range = None;
            return;
        }

        // Debug: log raw hover content to diagnose formatting issues
        tracing::debug!(
            "LSP hover content (markdown={}):\n{}",
            is_markdown,
            contents
        );

        // Convert LSP range to byte offsets for highlighting
        if let Some(((start_line, start_char), (end_line, end_char))) = range {
            let state = self.active_state();
            let start_byte = state
                .buffer
                .lsp_position_to_byte(start_line as usize, start_char as usize);
            let end_byte = state
                .buffer
                .lsp_position_to_byte(end_line as usize, end_char as usize);
            self.hover_symbol_range = Some((start_byte, end_byte));
            tracing::debug!(
                "Hover symbol range: {}..{} (LSP {}:{}..{}:{})",
                start_byte,
                end_byte,
                start_line,
                start_char,
                end_line,
                end_char
            );

            // Remove previous hover overlay if any
            if let Some(old_handle) = self.hover_symbol_overlay.take() {
                let remove_event = crate::model::event::Event::RemoveOverlay { handle: old_handle };
                self.apply_event_to_active_buffer(&remove_event);
            }

            // Add overlay to highlight the hovered symbol
            let event = crate::model::event::Event::AddOverlay {
                namespace: None,
                range: start_byte..end_byte,
                face: crate::model::event::OverlayFace::Background {
                    color: (80, 80, 120), // Subtle highlight for hovered symbol
                },
                priority: 90, // Below rename (100) but above syntax (lower)
                message: None,
                extend_to_line_end: false,
                url: None,
            };
            self.apply_event_to_active_buffer(&event);
            // Store the handle for later removal
            if let Some(state) = self.buffers.get(&self.active_buffer()) {
                self.hover_symbol_overlay = state.overlays.all().last().map(|o| o.handle.clone());
            }
        } else {
            // No range provided by LSP - compute word boundaries at hover position
            // This prevents the popup from following the mouse within the same word
            if let Some((hover_byte_pos, _, _, _)) = self.mouse_state.lsp_hover_state {
                let state = self.active_state();
                let start_byte = find_word_start(&state.buffer, hover_byte_pos);
                let end_byte = find_word_end(&state.buffer, hover_byte_pos);
                if start_byte < end_byte {
                    self.hover_symbol_range = Some((start_byte, end_byte));
                    tracing::debug!(
                        "Hover symbol range (computed from word boundaries): {}..{}",
                        start_byte,
                        end_byte
                    );
                } else {
                    self.hover_symbol_range = None;
                }
            } else {
                self.hover_symbol_range = None;
            }
        }

        // Create a popup with the hover contents.
        //
        // When a diagnostic overlaps the hover position, we pre-style its
        // lines (severity-colored header + plain message) and concatenate
        // with the parsed hover body into a single `PopupContent::Markdown`
        // vector. This avoids the previous approach of injecting a
        // `**bold**` heading and a `---` horizontal rule into the markdown
        // input — which rendered as uncolored bold text + a thick 40-cell
        // divider with blank-line padding, wasting vertical space and
        // losing the "this is an error" visual signal.
        use crate::view::markdown::{parse_markdown, StyledLine};
        use crate::view::popup::{Popup, PopupContent, PopupPosition};
        use ratatui::style::Style;
        use unicode_width::UnicodeWidthStr;

        let hover_lines: Vec<StyledLine> = if contents.is_empty() {
            Vec::new()
        } else if is_markdown {
            parse_markdown(&contents, &self.theme, Some(&self.grammar_registry))
        } else {
            contents
                .lines()
                .map(|s| {
                    let mut sl = StyledLine::new();
                    sl.push(s.to_string(), Style::default().fg(self.theme.popup_text_fg));
                    sl
                })
                .collect()
        };

        let has_diagnostic = !diagnostic_lines.is_empty();
        let mut all_lines: Vec<StyledLine> = Vec::new();
        all_lines.extend(diagnostic_lines);
        if has_diagnostic && !hover_lines.is_empty() {
            // Compact single-line separator — no blank padding, no 40-cell
            // dash run. One row of dashes the width of the content, in the
            // popup border color so it reads as "same card, new section."
            let mut sep = StyledLine::new();
            sep.push(
                "─".repeat(12),
                Style::default().fg(self.theme.popup_border_fg),
            );
            all_lines.push(sep);
        }
        all_lines.extend(hover_lines);

        // Drop trailing empty lines that some markdown payloads carry.
        while all_lines
            .last()
            .map(|l| l.spans.iter().all(|s| s.text.trim().is_empty()))
            .unwrap_or(false)
        {
            all_lines.pop();
        }

        // Fit width to content so short hovers stop rendering in an 80-col
        // card with half the width empty. Measured as the widest styled
        // line (display cells, not bytes), plus 4 for borders + padding,
        // clamped to [30, 80]. Height stays dynamic on terminal size.
        let content_width: usize = all_lines
            .iter()
            .map(|l| {
                l.spans
                    .iter()
                    .map(|s| UnicodeWidthStr::width(s.text.as_str()))
                    .sum::<usize>()
            })
            .max()
            .unwrap_or(0);
        let popup_width = (content_width as u16 + 4).clamp(30, 80);
        let dynamic_height = (self.terminal_height * 60 / 100).clamp(15, 40);

        // Construct the popup with the fused content.
        let mut popup = Popup::text(Vec::new(), &self.theme);
        popup.content = PopupContent::Markdown(all_lines);
        popup.title = Some(t!("lsp.popup_hover").to_string());
        popup.transient = true;
        popup.position = if let Some((x, y)) = self.mouse_hover_screen_position.take() {
            PopupPosition::Fixed { x, y: y + 1 }
        } else {
            PopupPosition::BelowCursor
        };
        popup.width = popup_width;
        popup.max_height = dynamic_height;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.show(popup);
            tracing::info!("Showing hover popup (markdown={})", is_markdown);
        }

        // Mark hover request as sent to prevent duplicate popups during race conditions
        // (e.g., when mouse moves while a hover response is pending)
        self.mouse_state.lsp_hover_request_sent = true;
    }

    /// Pre-style any diagnostics overlapping the hover position into lines
    /// ready to stack into the hover popup. Each diagnostic yields two or
    /// more styled lines:
    ///   1. severity marker + label in `diagnostic_*_fg`, followed by
    ///      `  (source)` dimmed — italic on theme-default foreground,
    ///   2. one styled line per message line, in `popup_text_fg`.
    ///
    /// Multiple overlapping diagnostics are separated by a blank line.
    /// Returns an empty vec when there are no overlapping diagnostics,
    /// or no buffer/URI resolves.
    fn compose_hover_diagnostic_lines(
        &self,
        lsp_pos: (u32, u32),
    ) -> Vec<crate::view::markdown::StyledLine> {
        use crate::view::markdown::StyledLine;
        use lsp_types::DiagnosticSeverity;
        use ratatui::style::{Modifier, Style};

        let buffer_id = self.active_buffer();
        let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
            return Vec::new();
        };
        let Some(uri) = metadata.file_uri() else {
            return Vec::new();
        };
        let Some(diagnostics) = self.get_stored_diagnostics().get(uri.as_str()) else {
            return Vec::new();
        };

        let (hover_line, hover_char) = lsp_pos;
        let overlapping: Vec<&lsp_types::Diagnostic> = diagnostics
            .iter()
            .filter(|d| lsp_range_contains(&d.range, hover_line, hover_char))
            .collect();

        if overlapping.is_empty() {
            return Vec::new();
        }

        let mut out: Vec<StyledLine> = Vec::new();
        for (idx, diag) in overlapping.iter().enumerate() {
            if idx > 0 {
                out.push(StyledLine::new());
            }

            let (label, marker, severity_color) = match diag.severity {
                Some(DiagnosticSeverity::ERROR) => ("Error", "✖", self.theme.diagnostic_error_fg),
                Some(DiagnosticSeverity::WARNING) => {
                    ("Warning", "⚠", self.theme.diagnostic_warning_fg)
                }
                Some(DiagnosticSeverity::INFORMATION) => {
                    ("Info", "ℹ", self.theme.diagnostic_info_fg)
                }
                Some(DiagnosticSeverity::HINT) => ("Hint", "ℹ", self.theme.diagnostic_hint_fg),
                _ => ("Diagnostic", "•", self.theme.popup_text_fg),
            };

            let header_style = Style::default()
                .fg(severity_color)
                .add_modifier(Modifier::BOLD);
            let mut header = StyledLine::new();
            header.push(format!("{} {}", marker, label), header_style);
            if let Some(source) = diag.source.as_deref().filter(|s| !s.is_empty()) {
                // Dim italic source tag — reads as metadata, not as part
                // of the diagnostic text.
                header.push(
                    format!("  ({})", source),
                    Style::default()
                        .fg(self.theme.tab_inactive_fg)
                        .add_modifier(Modifier::ITALIC),
                );
            }
            out.push(header);

            // Message verbatim: one styled line per message line. Using
            // `popup_text_fg` lets themes override the body color; the
            // severity information is already conveyed by the header.
            for message_line in diag.message.lines() {
                let mut line = StyledLine::new();
                line.push(
                    message_line.to_string(),
                    Style::default().fg(self.theme.popup_text_fg),
                );
                out.push(line);
            }
        }
        out
    }

    /// Apply inlay hints to editor state as virtual text
    #[doc(hidden)]
    pub fn apply_inlay_hints_to_state(
        state: &mut crate::state::EditorState,
        hints: &[lsp_types::InlayHint],
    ) {
        use crate::view::virtual_text::VirtualTextPosition;
        use ratatui::style::{Color, Style};

        // Clear existing inlay hints
        state.virtual_texts.clear(&mut state.marker_list);

        if hints.is_empty() {
            return;
        }

        // Style for inlay hints - dimmed to not distract from actual code
        let hint_style = Style::default().fg(Color::Rgb(128, 128, 128));

        for hint in hints {
            // Convert LSP position to byte offset
            let byte_offset = state.buffer.lsp_position_to_byte(
                hint.position.line as usize,
                hint.position.character as usize,
            );

            // Extract text from hint label
            let text = match &hint.label {
                lsp_types::InlayHintLabel::String(s) => s.clone(),
                lsp_types::InlayHintLabel::LabelParts(parts) => {
                    parts.iter().map(|p| p.value.as_str()).collect::<String>()
                }
            };

            // LSP inlay hint positions are insertion points between characters.
            // For positions within the buffer, render hints before the character at the
            // byte offset so they appear at the correct location (e.g., before punctuation
            // or newline). Hints at or beyond EOF are anchored to the last character and
            // rendered after it.
            if state.buffer.is_empty() {
                continue;
            }

            // Pick the anchor character for this hint. If the LSP-computed
            // byte lies on a line terminator (\n or the \r of a CRLF), the
            // "following character" is the first byte of the next line.
            // Anchoring to it would make the hint drift one line down on
            // any whitespace edit adjacent to the brace (issue #1572), so
            // instead anchor to the *preceding* non-newline character with
            // `AfterChar`. That keeps the hint stuck to the glyph the LSP
            // intended to annotate even as edits shift bytes around it.
            let buf_len = state.buffer.len();
            let byte_here = if byte_offset < buf_len {
                state
                    .buffer
                    .slice_bytes(byte_offset..byte_offset + 1)
                    .first()
                    .copied()
            } else {
                None
            };
            let at_line_break = matches!(byte_here, Some(b'\n' | b'\r'));

            let (byte_offset, position) = if byte_offset >= buf_len {
                // Hint is at EOF: anchor to last character and render
                // after it.
                (buf_len.saturating_sub(1), VirtualTextPosition::AfterChar)
            } else if at_line_break && byte_offset > 0 {
                // Hint points past the last glyph on a line: anchor to
                // that glyph with AfterChar so the marker cannot drift
                // onto a subsequent line when whitespace is edited.
                (byte_offset - 1, VirtualTextPosition::AfterChar)
            } else {
                (byte_offset, VirtualTextPosition::BeforeChar)
            };

            // Use the hint text as-is - spacing is handled during rendering
            let display_text = text;

            state.virtual_texts.add(
                &mut state.marker_list,
                byte_offset,
                display_text,
                hint_style,
                position,
                0, // Default priority
            );
        }

        tracing::debug!("Applied {} inlay hints as virtual text", hints.len());
    }

    /// Request LSP find references at current cursor position
    pub(crate) fn request_references(&mut self) -> AnyhowResult<()> {
        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let state = self.active_state();

        // Extract the word under cursor for display
        let symbol = {
            let text = match state.buffer.to_string() {
                Some(t) => t,
                None => {
                    self.set_status_message(t!("error.buffer_not_loaded").to_string());
                    return Ok(());
                }
            };
            let bytes = text.as_bytes();
            let buf_len = bytes.len();

            if cursor_pos <= buf_len {
                // Find word boundaries
                let is_word_char = |c: char| c.is_alphanumeric() || c == '_';

                // Find start of word
                let mut start = cursor_pos;
                while start > 0 {
                    // Move to previous byte
                    start -= 1;
                    // Skip continuation bytes (UTF-8)
                    while start > 0 && (bytes[start] & 0xC0) == 0x80 {
                        start -= 1;
                    }
                    // Get the character at this position
                    if let Some(ch) = text[start..].chars().next() {
                        if !is_word_char(ch) {
                            start += ch.len_utf8();
                            break;
                        }
                    } else {
                        break;
                    }
                }

                // Find end of word
                let mut end = cursor_pos;
                while end < buf_len {
                    if let Some(ch) = text[end..].chars().next() {
                        if is_word_char(ch) {
                            end += ch.len_utf8();
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if start < end {
                    text[start..end].to_string()
                } else {
                    String::new()
                }
            } else {
                String::new()
            }
        };

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(
                buffer_id,
                LspFeature::References,
                |handle, uri, _language| {
                    let result =
                        handle.references(request_id, uri.clone(), line as u32, character as u32);
                    if result.is_ok() {
                        tracing::info!(
                            "Requested find references at {}:{}:{} (byte_pos={})",
                            uri.as_str(),
                            line,
                            character,
                            cursor_pos
                        );
                    }
                    result.is_ok()
                },
            )
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_references_request = Some(request_id);
            self.pending_references_symbol = symbol;
        }

        Ok(())
    }

    /// Request LSP signature help at current cursor position
    pub(crate) fn request_signature_help(&mut self) {
        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(
                buffer_id,
                LspFeature::SignatureHelp,
                |handle, uri, _language| {
                    let result = handle.signature_help(
                        request_id,
                        uri.clone(),
                        line as u32,
                        character as u32,
                    );
                    if result.is_ok() {
                        tracing::info!(
                            "Requested signature help at {}:{}:{} (byte_pos={})",
                            uri.as_str(),
                            line,
                            character,
                            cursor_pos
                        );
                    }
                    result.is_ok()
                },
            )
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_signature_help_request = Some(request_id);
        }
    }

    /// Handle signature help response from LSP
    pub(crate) fn handle_signature_help_response(
        &mut self,
        request_id: u64,
        signature_help: Option<lsp_types::SignatureHelp>,
    ) {
        // Check if this response is for the current pending request
        if self.pending_signature_help_request != Some(request_id) {
            tracing::debug!("Ignoring stale signature help response: {}", request_id);
            return;
        }

        self.pending_signature_help_request = None;
        let signature_help = match signature_help {
            Some(help) if !help.signatures.is_empty() => help,
            _ => {
                tracing::debug!("No signature help available");
                return;
            }
        };

        // Get the active signature
        let active_signature_idx = signature_help.active_signature.unwrap_or(0) as usize;
        let signature = match signature_help.signatures.get(active_signature_idx) {
            Some(sig) => sig,
            None => return,
        };

        // Build the display content as markdown
        let mut content = String::new();

        // Add the signature label (function signature)
        content.push_str(&signature.label);
        content.push('\n');

        // Add parameter highlighting info
        let active_param = signature_help
            .active_parameter
            .or(signature.active_parameter)
            .unwrap_or(0) as usize;

        // If there are parameters, highlight the active one
        if let Some(params) = &signature.parameters {
            if let Some(param) = params.get(active_param) {
                // Get parameter label
                let param_label = match &param.label {
                    lsp_types::ParameterLabel::Simple(s) => s.clone(),
                    lsp_types::ParameterLabel::LabelOffsets(offsets) => {
                        // Extract substring from signature label
                        let start = offsets[0] as usize;
                        let end = offsets[1] as usize;
                        if end <= signature.label.len() {
                            signature.label[start..end].to_string()
                        } else {
                            String::new()
                        }
                    }
                };

                if !param_label.is_empty() {
                    content.push_str(&format!("\n> {}\n", param_label));
                }

                // Add parameter documentation if available
                if let Some(doc) = &param.documentation {
                    let doc_text = match doc {
                        lsp_types::Documentation::String(s) => s.clone(),
                        lsp_types::Documentation::MarkupContent(m) => m.value.clone(),
                    };
                    if !doc_text.is_empty() {
                        content.push('\n');
                        content.push_str(&doc_text);
                        content.push('\n');
                    }
                }
            }
        }

        // Add function documentation if available
        if let Some(doc) = &signature.documentation {
            let doc_text = match doc {
                lsp_types::Documentation::String(s) => s.clone(),
                lsp_types::Documentation::MarkupContent(m) => m.value.clone(),
            };
            if !doc_text.is_empty() {
                content.push_str("\n---\n\n");
                content.push_str(&space_doc_paragraphs(&doc_text));
            }
        }

        // Create a popup with markdown rendering (like hover popup)
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let mut popup = Popup::markdown(&content, &self.theme, Some(&self.grammar_registry));
        popup.title = Some(t!("lsp.popup_signature").to_string());
        popup.transient = true;
        popup.position = PopupPosition::BelowCursor;
        popup.width = 60;
        popup.max_height = 20;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.show(popup);
            tracing::info!(
                "Showing signature help popup for {} signatures",
                signature_help.signatures.len()
            );
        }
    }

    /// Request LSP code actions at current cursor position.
    /// Sends code action requests to all eligible servers for merged results.
    pub(crate) fn request_code_actions(&mut self) -> AnyhowResult<()> {
        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let selection_range = self.active_cursors().primary().selection_range();
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get selection range (if any) or use cursor position
        let (start_line, start_char, end_line, end_char) = if let Some(range) = selection_range {
            let (s_line, s_char) = state.buffer.position_to_lsp_position(range.start);
            let (e_line, e_char) = state.buffer.position_to_lsp_position(range.end);
            (s_line as u32, s_char as u32, e_line as u32, e_char as u32)
        } else {
            (line as u32, character as u32, line as u32, character as u32)
        };

        // Get diagnostics at cursor position for context
        // TODO: Implement diagnostic retrieval when needed
        let diagnostics: Vec<lsp_types::Diagnostic> = Vec::new();
        let buffer_id = self.active_buffer();

        // Pre-allocate request IDs for all eligible servers
        let base_request_id = self.next_lsp_request_id;
        let counter = std::sync::atomic::AtomicU64::new(0);

        let results = self.with_all_lsp_for_buffer_feature_named(
            buffer_id,
            LspFeature::CodeAction,
            |handle, uri, _language, server_name| {
                let idx = counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                let request_id = base_request_id + idx;
                let result = handle.code_actions(
                    request_id,
                    uri.clone(),
                    start_line,
                    start_char,
                    end_line,
                    end_char,
                    diagnostics.clone(),
                );
                if result.is_ok() {
                    tracing::info!(
                        "Requested code actions at {}:{}:{}-{}:{} (byte_pos={}, request_id={}, server={})",
                        uri.as_str(),
                        start_line,
                        start_char,
                        end_line,
                        end_char,
                        cursor_pos,
                        request_id,
                        server_name
                    );
                }
                (request_id, result.is_ok(), server_name.to_string())
            },
        );

        let mut sent_ids = Vec::new();
        for (request_id, ok, server_name) in &results {
            if *ok {
                sent_ids.push(*request_id);
                self.pending_code_actions_server_names
                    .insert(*request_id, server_name.clone());
            }
        }
        // Advance the ID counter past all allocated IDs
        self.next_lsp_request_id = base_request_id + results.len() as u64;

        if !sent_ids.is_empty() {
            // Clear any previously accumulated actions for a fresh merge
            self.pending_code_actions = None;
            self.pending_code_actions_requests.extend(sent_ids);
        }

        Ok(())
    }

    /// Handle code actions response from LSP.
    /// Supports merging from multiple servers: each response extends the action
    /// list, and the popup is shown/updated with each arriving response.
    pub(crate) fn handle_code_actions_response(
        &mut self,
        request_id: u64,
        actions: Vec<lsp_types::CodeActionOrCommand>,
    ) {
        // Check if this response is for one of the pending requests
        if !self.pending_code_actions_requests.remove(&request_id) {
            tracing::debug!("Ignoring stale code actions response: {}", request_id);
            return;
        }

        // Update status when all code action responses have arrived
        if self.pending_code_actions_requests.is_empty() {}

        // Look up the server name for this request
        let server_name = self
            .pending_code_actions_server_names
            .remove(&request_id)
            .unwrap_or_default();

        if actions.is_empty() {
            // Only show "no code actions" if all responses are in and we have nothing
            if self.pending_code_actions_requests.is_empty()
                && self
                    .pending_code_actions
                    .as_ref()
                    .is_none_or(|a| a.is_empty())
            {
                self.set_status_message(t!("lsp.no_code_actions").to_string());
            }
            return;
        }

        // Tag each action with its server name and store/extend for merging
        let tagged_actions: Vec<(String, lsp_types::CodeActionOrCommand)> = actions
            .into_iter()
            .map(|a| (server_name.clone(), a))
            .collect();

        match &mut self.pending_code_actions {
            Some(existing) => {
                existing.extend(tagged_actions);
                tracing::debug!("Extended code actions, now {} total", existing.len());
            }
            None => {
                self.pending_code_actions = Some(tagged_actions);
            }
        }

        // Build list items from all accumulated code actions
        use crate::view::popup::{Popup, PopupListItem, PopupPosition};
        use ratatui::style::Style;

        // Check if actions come from multiple servers
        let all_actions = self.pending_code_actions.as_ref().unwrap();
        let multiple_servers = {
            let mut names = std::collections::HashSet::new();
            for (name, _) in all_actions {
                names.insert(name.as_str());
            }
            names.len() > 1
        };

        let items: Vec<PopupListItem> = all_actions
            .iter()
            .enumerate()
            .map(|(i, (srv_name, action))| {
                let title = match action {
                    lsp_types::CodeActionOrCommand::Command(cmd) => &cmd.title,
                    lsp_types::CodeActionOrCommand::CodeAction(ca) => &ca.title,
                };
                let kind = match action {
                    lsp_types::CodeActionOrCommand::CodeAction(ca) => {
                        ca.kind.as_ref().map(|k| k.as_str().to_string())
                    }
                    _ => None,
                };
                // Show server name in detail when multiple servers contribute
                let detail = if multiple_servers && !srv_name.is_empty() {
                    match kind {
                        Some(k) => Some(format!("[{}] {}", srv_name, k)),
                        None => Some(format!("[{}]", srv_name)),
                    }
                } else {
                    kind
                };
                PopupListItem {
                    text: format!("{}. {}", i + 1, title),
                    detail,
                    icon: None,
                    data: Some(i.to_string()),
                    disabled: false,
                }
            })
            .collect();

        let mut popup = Popup::list(items, &self.theme);
        popup.kind = crate::view::popup::PopupKind::Action;
        popup.title = Some(t!("lsp.popup_code_actions").to_string());
        popup.position = PopupPosition::BelowCursor;
        popup.width = 60;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup, replacing any existing action popup to avoid stacking
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.show_or_replace(popup);
            tracing::info!(
                "Showing code actions popup with {} actions",
                all_actions.len()
            );
        }
    }

    /// Execute a code action by index from the stored pending_code_actions.
    pub(crate) fn execute_code_action(&mut self, index: usize) {
        let action = match &self.pending_code_actions {
            Some(actions) => actions.get(index).map(|(_, a)| a.clone()),
            None => None,
        };

        let Some(action) = action else {
            tracing::warn!("Code action index {} out of range", index);
            return;
        };

        match action {
            lsp_types::CodeActionOrCommand::CodeAction(ca) => {
                // If the action has no edit and no command, it may need resolve first.
                // Only resolve if the action has `data` and the server supports resolveProvider.
                if ca.edit.is_none()
                    && ca.command.is_none()
                    && ca.data.is_some()
                    && self.server_supports_code_action_resolve()
                {
                    tracing::info!(
                        "Code action '{}' needs resolve, sending codeAction/resolve",
                        ca.title
                    );
                    self.send_code_action_resolve(ca);
                    return;
                }
                self.execute_resolved_code_action(ca);
            }
            lsp_types::CodeActionOrCommand::Command(cmd) => {
                self.send_execute_command(cmd);
            }
        }
    }

    /// Execute a code action that has been fully resolved (has edit and/or command).
    pub(crate) fn execute_resolved_code_action(&mut self, ca: lsp_types::CodeAction) {
        let title = ca.title.clone();

        // Apply workspace edit if present
        if let Some(edit) = ca.edit {
            match self.apply_workspace_edit(edit) {
                Ok(n) => {
                    self.set_status_message(
                        t!("lsp.code_action_applied", title = &title, count = n).to_string(),
                    );
                }
                Err(e) => {
                    self.set_status_message(format!("Code action failed: {e}"));
                    return;
                }
            }
        }

        // Execute command if present (may trigger workspace/applyEdit from server)
        if let Some(cmd) = ca.command {
            self.send_execute_command(cmd);
        }
    }

    /// Send workspace/executeCommand to the LSP server
    fn send_execute_command(&mut self, cmd: lsp_types::Command) {
        tracing::info!("Executing LSP command: {} ({})", cmd.title, cmd.command);
        self.set_status_message(
            t!(
                "lsp.code_action_applied",
                title = &cmd.title,
                count = 0_usize
            )
            .to_string(),
        );

        // Get the language for this buffer to find the right LSP handle
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => return,
        };

        if let Some(lsp) = &mut self.lsp {
            for sh in lsp.get_handles_mut(&language) {
                if let Err(e) = sh
                    .handle
                    .execute_command(cmd.command.clone(), cmd.arguments.clone())
                {
                    tracing::warn!("Failed to send executeCommand to '{}': {}", sh.name, e);
                }
            }
        }
    }

    /// Send codeAction/resolve to the LSP server
    fn send_code_action_resolve(&mut self, action: lsp_types::CodeAction) {
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => return,
        };

        self.next_lsp_request_id += 1;
        let request_id = self.next_lsp_request_id;

        if let Some(lsp) = &mut self.lsp {
            for sh in lsp.get_handles_mut(&language) {
                if let Err(e) = sh.handle.code_action_resolve(request_id, action.clone()) {
                    tracing::warn!("Failed to send codeAction/resolve to '{}': {}", sh.name, e);
                }
            }
        }
    }

    /// Check if any LSP server for the current buffer supports codeAction/resolve
    fn server_supports_code_action_resolve(&self) -> bool {
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => return false,
        };

        if let Some(lsp) = &self.lsp {
            for sh in lsp.get_handles(&language) {
                if sh.capabilities.code_action_resolve {
                    return true;
                }
            }
        }
        false
    }

    /// Check if any LSP server for the current buffer supports completionItem/resolve
    pub(crate) fn server_supports_completion_resolve(&self) -> bool {
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => return false,
        };

        if let Some(lsp) = &self.lsp {
            for sh in lsp.get_handles(&language) {
                if sh.capabilities.completion_resolve {
                    return true;
                }
            }
        }
        false
    }

    /// Send completionItem/resolve to the LSP server
    pub(crate) fn send_completion_resolve(&mut self, item: lsp_types::CompletionItem) {
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => return,
        };

        self.next_lsp_request_id += 1;
        let request_id = self.next_lsp_request_id;

        if let Some(lsp) = &mut self.lsp {
            for sh in lsp.get_handles_mut(&language) {
                if sh.capabilities.completion_resolve {
                    if let Err(e) = sh.handle.completion_resolve(request_id, item.clone()) {
                        tracing::warn!(
                            "Failed to send completionItem/resolve to '{}': {}",
                            sh.name,
                            e
                        );
                    }
                    return;
                }
            }
        }
    }

    /// Handle a resolved completion item — apply additional_text_edits (e.g. auto-imports).
    pub(crate) fn handle_completion_resolved(&mut self, item: lsp_types::CompletionItem) {
        if let Some(additional_edits) = item.additional_text_edits {
            if !additional_edits.is_empty() {
                tracing::info!(
                    "Applying {} additional text edits from completion resolve",
                    additional_edits.len()
                );
                let buffer_id = self.active_buffer();
                if let Err(e) = self.apply_lsp_text_edits(buffer_id, additional_edits) {
                    tracing::error!("Failed to apply completion additional_text_edits: {}", e);
                }
            }
        }
    }

    /// Apply formatting edits from textDocument/formatting response.
    pub(crate) fn apply_formatting_edits(
        &mut self,
        uri: &str,
        edits: Vec<lsp_types::TextEdit>,
    ) -> AnyhowResult<usize> {
        // Find the buffer for this URI
        let buffer_id = self
            .buffer_metadata
            .iter()
            .find(|(_, meta)| meta.file_uri().map(|u| u.as_str() == uri).unwrap_or(false))
            .map(|(id, _)| *id);

        if let Some(buffer_id) = buffer_id {
            let count = self.apply_lsp_text_edits(buffer_id, edits)?;
            self.set_status_message(format!("Formatted ({} edits)", count));
            Ok(count)
        } else {
            tracing::warn!("Cannot apply formatting: no buffer for URI {}", uri);
            Ok(0)
        }
    }

    /// Request document formatting from LSP.
    pub(crate) fn request_formatting(&mut self) {
        let buffer_id = self.active_buffer();
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) if m.lsp_enabled => m,
            _ => {
                self.set_status_message("LSP not available for this buffer".to_string());
                return;
            }
        };

        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => return,
        };

        let language = match self.buffers.get(&buffer_id).map(|s| s.language.clone()) {
            Some(l) => l,
            None => return,
        };

        let tab_size = self.config.editor.tab_size as u32;
        let insert_spaces = !self.config.editor.use_tabs;

        self.next_lsp_request_id += 1;
        let request_id = self.next_lsp_request_id;

        if let Some(lsp) = &mut self.lsp {
            if let Some(sh) = lsp.handle_for_feature_mut(&language, LspFeature::Format) {
                if let Err(e) =
                    sh.handle
                        .document_formatting(request_id, uri, tab_size, insert_spaces)
                {
                    tracing::warn!("Failed to request formatting: {}", e);
                }
            } else {
                self.set_status_message("Formatting not supported by LSP server".to_string());
            }
        }
    }

    /// Handle find references response from LSP
    pub(crate) fn handle_references_response(
        &mut self,
        request_id: u64,
        locations: Vec<lsp_types::Location>,
    ) -> AnyhowResult<()> {
        tracing::info!(
            "handle_references_response: received {} locations for request_id={}",
            locations.len(),
            request_id
        );

        // Check if this response is for the current pending request
        if self.pending_references_request != Some(request_id) {
            tracing::debug!("Ignoring stale references response: {}", request_id);
            return Ok(());
        }

        self.pending_references_request = None;
        if locations.is_empty() {
            self.set_status_message(t!("lsp.no_references").to_string());
            return Ok(());
        }

        // Convert locations to hook args format
        let lsp_locations: Vec<crate::services::plugins::hooks::LspLocation> = locations
            .iter()
            .map(|loc| {
                // Convert URI to file path
                let file = if loc.uri.scheme().map(|s| s.as_str()) == Some("file") {
                    // Extract path from file:// URI
                    loc.uri.path().as_str().to_string()
                } else {
                    loc.uri.as_str().to_string()
                };

                crate::services::plugins::hooks::LspLocation {
                    file,
                    line: loc.range.start.line + 1, // LSP is 0-based, convert to 1-based
                    column: loc.range.start.character + 1, // LSP is 0-based
                }
            })
            .collect();

        let count = lsp_locations.len();
        let symbol = std::mem::take(&mut self.pending_references_symbol);
        self.set_status_message(
            t!("lsp.found_references", count = count, symbol = &symbol).to_string(),
        );

        // Fire the lsp_references hook so plugins can display the results
        self.plugin_manager.run_hook(
            "lsp_references",
            crate::services::plugins::hooks::HookArgs::LspReferences {
                symbol: symbol.clone(),
                locations: lsp_locations,
            },
        );

        tracing::info!(
            "Fired lsp_references hook with {} locations for symbol '{}'",
            count,
            symbol
        );

        Ok(())
    }

    /// Apply LSP text edits to a buffer and return the number of changes made.
    /// Edits are sorted in reverse order and applied as a batch.
    pub(crate) fn apply_lsp_text_edits(
        &mut self,
        buffer_id: BufferId,
        mut edits: Vec<lsp_types::TextEdit>,
    ) -> AnyhowResult<usize> {
        if edits.is_empty() {
            return Ok(0);
        }

        // Sort edits by position (reverse order to avoid offset issues)
        edits.sort_by(|a, b| {
            b.range
                .start
                .line
                .cmp(&a.range.start.line)
                .then(b.range.start.character.cmp(&a.range.start.character))
        });

        // Collect all events for this buffer into a batch
        let mut batch_events = Vec::new();
        let mut changes = 0;

        // Get cursor_id for this buffer from split view state
        let cursor_id = {
            let split_id = self
                .split_manager
                .splits_for_buffer(buffer_id)
                .into_iter()
                .next()
                .unwrap_or_else(|| self.split_manager.active_split());
            self.split_view_states
                .get(&split_id)
                .map(|vs| vs.cursors.primary_id())
                .unwrap_or_else(|| self.active_cursors().primary_id())
        };

        // Create events for all edits
        for edit in edits {
            let state = self
                .buffers
                .get_mut(&buffer_id)
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;

            // Convert LSP range to byte positions
            let start_line = edit.range.start.line as usize;
            let start_char = edit.range.start.character as usize;
            let end_line = edit.range.end.line as usize;
            let end_char = edit.range.end.character as usize;

            let start_pos = state.buffer.lsp_position_to_byte(start_line, start_char);
            let end_pos = state.buffer.lsp_position_to_byte(end_line, end_char);
            let buffer_len = state.buffer.len();

            // Log the conversion for debugging
            let old_text = if start_pos < end_pos && end_pos <= buffer_len {
                state.get_text_range(start_pos, end_pos)
            } else {
                format!(
                    "<invalid range: start={}, end={}, buffer_len={}>",
                    start_pos, end_pos, buffer_len
                )
            };
            tracing::debug!(
                "  Converting LSP range line {}:{}-{}:{} to bytes {}..{} (replacing {:?} with {:?})",
                start_line, start_char, end_line, end_char,
                start_pos, end_pos, old_text, edit.new_text
            );

            // Delete old text
            if start_pos < end_pos {
                let deleted_text = state.get_text_range(start_pos, end_pos);
                let delete_event = Event::Delete {
                    range: start_pos..end_pos,
                    deleted_text,
                    cursor_id,
                };
                batch_events.push(delete_event);
            }

            // Insert new text
            if !edit.new_text.is_empty() {
                let insert_event = Event::Insert {
                    position: start_pos,
                    text: edit.new_text.clone(),
                    cursor_id,
                };
                batch_events.push(insert_event);
            }

            changes += 1;
        }

        // Apply all rename changes using bulk edit for O(n) performance
        if !batch_events.is_empty() {
            self.apply_events_to_buffer_as_bulk_edit(
                buffer_id,
                batch_events,
                "LSP Rename".to_string(),
            )?;
        }

        Ok(changes)
    }

    /// Apply a single TextDocumentEdit from a workspace edit.
    ///
    /// Per LSP spec: if `text_document.version` is non-null, it must match the
    /// version we last sent via didOpen/didChange. On mismatch the edit is stale
    /// and we skip it to avoid corrupting the buffer.
    fn apply_text_document_edit(
        &mut self,
        text_doc_edit: lsp_types::TextDocumentEdit,
    ) -> AnyhowResult<usize> {
        let uri = text_doc_edit.text_document.uri;

        // Version check: if the server specifies a version, verify it matches
        // what we sent. A mismatch means the edit was computed against stale content.
        if let Some(expected_version) = text_doc_edit.text_document.version {
            if let Ok(path) = uri_to_path(&uri) {
                if let Some(lsp) = &self.lsp {
                    let language = self
                        .buffers
                        .get(&self.active_buffer())
                        .map(|s| s.language.clone())
                        .unwrap_or_default();
                    for sh in lsp.get_handles(&language) {
                        if let Some(current_version) = sh.handle.document_version(&path) {
                            if (expected_version as i64) != current_version {
                                tracing::warn!(
                                    "Rejecting stale TextDocumentEdit for {:?}: \
                                     server version {} != our version {}",
                                    path,
                                    expected_version,
                                    current_version,
                                );
                                return Ok(0);
                            }
                        }
                    }
                }
            }
        }

        if let Ok(path) = uri_to_path(&uri) {
            let buffer_id = match self.open_file(&path) {
                Ok(id) => id,
                Err(e) => {
                    if let Some(confirmation) =
                        e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                    {
                        self.start_large_file_encoding_confirmation(confirmation);
                    } else {
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                    return Ok(0);
                }
            };

            let edits: Vec<lsp_types::TextEdit> = text_doc_edit
                .edits
                .into_iter()
                .map(|one_of| match one_of {
                    lsp_types::OneOf::Left(text_edit) => text_edit,
                    lsp_types::OneOf::Right(annotated) => annotated.text_edit,
                })
                .collect();

            tracing::info!("Applying {} edits for {:?}:", edits.len(), path);
            for (i, edit) in edits.iter().enumerate() {
                tracing::info!(
                    "  Edit {}: line {}:{}-{}:{} -> {:?}",
                    i,
                    edit.range.start.line,
                    edit.range.start.character,
                    edit.range.end.line,
                    edit.range.end.character,
                    edit.new_text
                );
            }

            self.apply_lsp_text_edits(buffer_id, edits)
        } else {
            Ok(0)
        }
    }

    /// Apply a resource operation (CreateFile, RenameFile, DeleteFile) from a workspace edit.
    fn apply_resource_operation(&mut self, op: lsp_types::ResourceOp) -> AnyhowResult<()> {
        match op {
            lsp_types::ResourceOp::Create(create) => {
                let path = std::path::PathBuf::from(create.uri.path().as_str());
                let overwrite = create
                    .options
                    .as_ref()
                    .and_then(|o| o.overwrite)
                    .unwrap_or(false);
                let ignore_if_exists = create
                    .options
                    .as_ref()
                    .and_then(|o| o.ignore_if_exists)
                    .unwrap_or(false);

                if path.exists() {
                    if ignore_if_exists {
                        tracing::debug!("CreateFile: {:?} already exists, ignoring", path);
                        return Ok(());
                    }
                    if !overwrite {
                        tracing::warn!("CreateFile: {:?} already exists and overwrite=false", path);
                        return Ok(());
                    }
                }

                // Create parent directories if needed
                if let Some(parent) = path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::write(&path, "")?;
                tracing::info!("CreateFile: created {:?}", path);

                // Open the new file as a buffer
                if let Err(e) = self.open_file(&path) {
                    tracing::warn!("CreateFile: failed to open created file {:?}: {}", path, e);
                }
            }
            lsp_types::ResourceOp::Rename(rename) => {
                let old_path = std::path::PathBuf::from(rename.old_uri.path().as_str());
                let new_path = std::path::PathBuf::from(rename.new_uri.path().as_str());
                let overwrite = rename
                    .options
                    .as_ref()
                    .and_then(|o| o.overwrite)
                    .unwrap_or(false);
                let ignore_if_exists = rename
                    .options
                    .as_ref()
                    .and_then(|o| o.ignore_if_exists)
                    .unwrap_or(false);

                if new_path.exists() {
                    if ignore_if_exists {
                        tracing::debug!("RenameFile: {:?} already exists, ignoring", new_path);
                        return Ok(());
                    }
                    if !overwrite {
                        tracing::warn!(
                            "RenameFile: {:?} already exists and overwrite=false",
                            new_path
                        );
                        return Ok(());
                    }
                }

                // Create parent directories if needed
                if let Some(parent) = new_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::rename(&old_path, &new_path)?;
                tracing::info!("RenameFile: {:?} -> {:?}", old_path, new_path);
            }
            lsp_types::ResourceOp::Delete(delete) => {
                let path = std::path::PathBuf::from(delete.uri.path().as_str());
                let recursive = delete
                    .options
                    .as_ref()
                    .and_then(|o| o.recursive)
                    .unwrap_or(false);
                let ignore_if_not_exists = delete
                    .options
                    .as_ref()
                    .and_then(|o| o.ignore_if_not_exists)
                    .unwrap_or(false);

                if !path.exists() {
                    if ignore_if_not_exists {
                        tracing::debug!("DeleteFile: {:?} does not exist, ignoring", path);
                        return Ok(());
                    }
                    tracing::warn!("DeleteFile: {:?} does not exist", path);
                    return Ok(());
                }

                if path.is_dir() && recursive {
                    std::fs::remove_dir_all(&path)?;
                } else if path.is_file() {
                    std::fs::remove_file(&path)?;
                }
                tracing::info!("DeleteFile: deleted {:?}", path);
            }
        }
        Ok(())
    }

    /// Apply an LSP WorkspaceEdit (used by rename, code actions, etc.).
    ///
    /// Returns the total number of text changes applied.
    pub(crate) fn apply_workspace_edit(
        &mut self,
        workspace_edit: lsp_types::WorkspaceEdit,
    ) -> AnyhowResult<usize> {
        tracing::debug!(
            "Applying WorkspaceEdit: changes={:?}, document_changes={:?}",
            workspace_edit.changes.as_ref().map(|c| c.len()),
            workspace_edit.document_changes.as_ref().map(|dc| match dc {
                lsp_types::DocumentChanges::Edits(e) => format!("{} edits", e.len()),
                lsp_types::DocumentChanges::Operations(o) => format!("{} operations", o.len()),
            })
        );

        let mut total_changes = 0;

        // Handle changes (map of URI -> Vec<TextEdit>)
        if let Some(changes) = workspace_edit.changes {
            for (uri, edits) in changes {
                if let Ok(path) = uri_to_path(&uri) {
                    let buffer_id = match self.open_file(&path) {
                        Ok(id) => id,
                        Err(e) => {
                            if let Some(confirmation) = e.downcast_ref::<
                                crate::model::buffer::LargeFileEncodingConfirmation,
                            >() {
                                self.start_large_file_encoding_confirmation(confirmation);
                            } else {
                                self.set_status_message(
                                    t!("file.error_opening", error = e.to_string())
                                        .to_string(),
                                );
                            }
                            return Ok(0);
                        }
                    };
                    total_changes += self.apply_lsp_text_edits(buffer_id, edits)?;
                }
            }
        }

        // Handle document_changes (TextDocumentEdit[] or DocumentChangeOperation[])
        if let Some(document_changes) = workspace_edit.document_changes {
            use lsp_types::DocumentChanges;

            match document_changes {
                DocumentChanges::Edits(edits) => {
                    for text_doc_edit in edits {
                        total_changes += self.apply_text_document_edit(text_doc_edit)?;
                    }
                }
                DocumentChanges::Operations(ops) => {
                    // Process operations in order — resource ops (create/rename/delete)
                    // must be applied before text edits on the created/renamed files.
                    for op in ops {
                        match op {
                            lsp_types::DocumentChangeOperation::Edit(text_doc_edit) => {
                                total_changes += self.apply_text_document_edit(text_doc_edit)?;
                            }
                            lsp_types::DocumentChangeOperation::Op(resource_op) => {
                                self.apply_resource_operation(resource_op)?;
                                total_changes += 1;
                            }
                        }
                    }
                }
            }
        }

        Ok(total_changes)
    }

    /// Handle rename response from LSP
    pub fn handle_rename_response(
        &mut self,
        _request_id: u64,
        result: Result<lsp_types::WorkspaceEdit, String>,
    ) -> AnyhowResult<()> {
        match result {
            Ok(workspace_edit) => {
                let total_changes = self.apply_workspace_edit(workspace_edit)?;
                self.status_message = Some(t!("lsp.renamed", count = total_changes).to_string());
            }
            Err(error) => {
                // Per LSP spec: ContentModified errors (-32801) should NOT be shown to user
                if error.contains("content modified") || error.contains("-32801") {
                    tracing::debug!(
                        "LSP rename: ContentModified error (expected, ignoring): {}",
                        error
                    );
                    self.status_message = Some(t!("lsp.rename_cancelled").to_string());
                } else {
                    self.status_message = Some(t!("lsp.rename_failed", error = &error).to_string());
                }
            }
        }

        Ok(())
    }

    /// Apply events to a specific buffer using bulk edit optimization (O(n) vs O(n²))
    ///
    /// This is similar to `apply_events_as_bulk_edit` but works on a specific buffer
    /// (which may not be the active buffer) and handles LSP notifications correctly.
    pub(crate) fn apply_events_to_buffer_as_bulk_edit(
        &mut self,
        buffer_id: BufferId,
        events: Vec<Event>,
        description: String,
    ) -> AnyhowResult<()> {
        use crate::model::event::CursorId;

        if events.is_empty() {
            return Ok(());
        }

        // Create a temporary batch for collecting LSP changes (before applying)
        let batch_for_lsp = Event::Batch {
            events: events.clone(),
            description: description.clone(),
        };

        // IMPORTANT: Calculate LSP changes BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer.
        let original_active = self.active_buffer();
        self.split_manager.set_active_buffer_id(buffer_id);
        let lsp_changes = self.collect_lsp_changes(&batch_for_lsp);
        self.split_manager.set_active_buffer_id(original_active);

        // Capture old cursor states from split view state
        // Find a split that has this buffer in its keyed_states
        let split_id_for_cursors = self
            .split_manager
            .splits_for_buffer(buffer_id)
            .into_iter()
            .next()
            .unwrap_or_else(|| self.split_manager.active_split());
        let old_cursors: Vec<(CursorId, usize, Option<usize>)> = self
            .split_view_states
            .get(&split_id_for_cursors)
            .and_then(|vs| vs.keyed_states.get(&buffer_id))
            .map(|bvs| {
                bvs.cursors
                    .iter()
                    .map(|(id, c)| (id, c.position, c.anchor))
                    .collect()
            })
            .unwrap_or_default();

        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;

        // Snapshot buffer state for undo (piece tree + buffers)
        let old_snapshot = state.buffer.snapshot_buffer_state();

        // Convert events to edit tuples: (position, delete_len, insert_text)
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

        // Apply bulk edits - O(n) instead of O(n²)
        let _delta = state.buffer.apply_bulk_edits(&edit_refs);

        // Calculate new cursor positions based on edits
        let mut position_deltas: Vec<(usize, isize)> = Vec::new();
        for (pos, del_len, text) in &edits {
            let delta = text.len() as isize - *del_len as isize;
            position_deltas.push((*pos, delta));
        }
        position_deltas.sort_by_key(|(pos, _)| *pos);

        let calc_shift = |original_pos: usize| -> isize {
            let mut shift: isize = 0;
            for (edit_pos, delta) in &position_deltas {
                if *edit_pos < original_pos {
                    shift += delta;
                }
            }
            shift
        };

        // Calculate new cursor positions
        let buffer_len = state.buffer.len();
        let new_cursors: Vec<(CursorId, usize, Option<usize>)> = old_cursors
            .iter()
            .map(|(id, pos, anchor)| {
                let shift = calc_shift(*pos);
                let new_pos = ((*pos as isize + shift).max(0) as usize).min(buffer_len);
                let new_anchor = anchor.map(|a| {
                    let anchor_shift = calc_shift(a);
                    ((a as isize + anchor_shift).max(0) as usize).min(buffer_len)
                });
                (*id, new_pos, new_anchor)
            })
            .collect();

        // Snapshot buffer state after edits (for redo)
        let new_snapshot = state.buffer.snapshot_buffer_state();

        // Invalidate syntax highlighting
        state.highlighter.invalidate_all();

        // Apply new cursor positions to split view state
        if let Some(vs) = self.split_view_states.get_mut(&split_id_for_cursors) {
            if let Some(bvs) = vs.keyed_states.get_mut(&buffer_id) {
                for (cursor_id, new_pos, new_anchor) in &new_cursors {
                    if let Some(cursor) = bvs.cursors.get_mut(*cursor_id) {
                        cursor.position = *new_pos;
                        cursor.anchor = *new_anchor;
                    }
                }
            }
        }

        // Convert edit list to lengths-only for undo/redo marker replay.
        // Merge edits at the same position into a single replacement.
        let edit_lengths: Vec<(usize, usize, usize)> = {
            let mut lengths: Vec<(usize, usize, usize)> = Vec::new();
            for (pos, del_len, text) in &edits {
                if let Some(last) = lengths.last_mut() {
                    if last.0 == *pos {
                        last.1 += del_len;
                        last.2 += text.len();
                        continue;
                    }
                }
                lengths.push((*pos, *del_len, text.len()));
            }
            lengths
        };

        // Adjust markers using merged net-delta (same logic as apply_events_as_bulk_edit)
        for &(pos, del_len, ins_len) in &edit_lengths {
            if del_len > 0 && ins_len > 0 {
                if ins_len > del_len {
                    state.marker_list.adjust_for_insert(pos, ins_len - del_len);
                    state.margins.adjust_for_insert(pos, ins_len - del_len);
                } else if del_len > ins_len {
                    state.marker_list.adjust_for_delete(pos, del_len - ins_len);
                    state.margins.adjust_for_delete(pos, del_len - ins_len);
                }
            } else if del_len > 0 {
                state.marker_list.adjust_for_delete(pos, del_len);
                state.margins.adjust_for_delete(pos, del_len);
            } else if ins_len > 0 {
                state.marker_list.adjust_for_insert(pos, ins_len);
                state.margins.adjust_for_insert(pos, ins_len);
            }
        }

        // Create BulkEdit event for undo log
        let bulk_edit = Event::BulkEdit {
            old_snapshot: Some(old_snapshot),
            new_snapshot: Some(new_snapshot),
            old_cursors,
            new_cursors,
            description,
            edits: edit_lengths,
            displaced_markers,
        };

        // Add to event log
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            event_log.append(bulk_edit);
        }

        // Notify LSP about the changes using pre-calculated positions
        self.send_lsp_changes_for_buffer(buffer_id, lsp_changes);

        Ok(())
    }

    /// Send pre-calculated LSP changes for a specific buffer
    pub(crate) fn send_lsp_changes_for_buffer(
        &mut self,
        buffer_id: BufferId,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) {
        if changes.is_empty() {
            return;
        }

        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no metadata for buffer {:?}",
                    buffer_id
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("send_lsp_changes_for_buffer: LSP disabled for this buffer");
            return;
        }

        // Get the URI
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };
        let file_path = metadata.file_path().cloned();

        // Get language from buffer state
        let language = match self.buffers.get(&buffer_id).map(|s| s.language.clone()) {
            Some(l) => l,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no buffer state for {:?}",
                    buffer_id
                );
                return;
            }
        };

        tracing::trace!(
            "send_lsp_changes_for_buffer: sending {} changes to {} in single didChange notification",
            changes.len(),
            uri.as_str()
        );

        // Check if we can use LSP (respects auto_start setting)
        use crate::services::lsp::manager::LspSpawnResult;
        let Some(lsp) = self.lsp.as_mut() else {
            tracing::debug!("send_lsp_changes_for_buffer: no LSP manager available");
            return;
        };

        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            tracing::debug!(
                "send_lsp_changes_for_buffer: LSP not running for {} (auto_start disabled)",
                language
            );
            return;
        }

        // Check which handles need didOpen first
        let handles_needing_open: Vec<_> = {
            let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
                return;
            };
            lsp.get_handles(&language)
                .into_iter()
                .filter(|sh| !metadata.lsp_opened_with.contains(&sh.handle.id()))
                .map(|sh| (sh.name.clone(), sh.handle.id()))
                .collect()
        };

        if !handles_needing_open.is_empty() {
            // Get text for didOpen
            let text = match self
                .buffers
                .get(&buffer_id)
                .and_then(|s| s.buffer.to_string())
            {
                Some(t) => t,
                None => {
                    tracing::debug!(
                        "send_lsp_changes_for_buffer: buffer text not available for didOpen"
                    );
                    return;
                }
            };

            // Send didOpen to all handles that haven't been opened yet
            let Some(lsp) = self.lsp.as_mut() else { return };
            for sh in lsp.get_handles_mut(&language) {
                if handles_needing_open
                    .iter()
                    .any(|(_, id)| *id == sh.handle.id())
                {
                    if let Err(e) = sh
                        .handle
                        .did_open(uri.clone(), text.clone(), language.clone())
                    {
                        tracing::warn!(
                            "Failed to send didOpen to '{}' before didChange: {}",
                            sh.name,
                            e
                        );
                    } else {
                        tracing::debug!(
                            "Sent didOpen for {} to LSP handle '{}' before didChange",
                            uri.as_str(),
                            sh.name
                        );
                    }
                }
            }

            // Mark all as opened
            if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                for (_, handle_id) in &handles_needing_open {
                    metadata.lsp_opened_with.insert(*handle_id);
                }
            }

            // didOpen already contains the full current buffer content, so we must
            // NOT also send didChange (which carries pre-edit incremental changes).
            // Sending both would corrupt the server's view of the document.
            return;
        }

        // Now send didChange to all handles for this language
        let Some(lsp) = self.lsp.as_mut() else { return };
        let mut any_sent = false;
        for sh in lsp.get_handles_mut(&language) {
            if let Err(e) = sh.handle.did_change(uri.clone(), changes.clone()) {
                tracing::warn!("Failed to send didChange to '{}': {}", sh.name, e);
            } else {
                any_sent = true;
            }
        }
        if any_sent {
            tracing::trace!("Successfully sent batched didChange to LSP");

            // Invalidate diagnostic cache so the next diagnostic apply recomputes
            // overlay positions from fresh byte offsets (the buffer content changed)
            if let Some(state) = self.buffers.get(&buffer_id) {
                if let Some(path) = state.buffer.file_path() {
                    crate::services::lsp::diagnostics::invalidate_cache_for_file(
                        &path.to_string_lossy(),
                    );
                }
            }

            // Schedule debounced diagnostic re-pull (1000ms after last edit)
            self.scheduled_diagnostic_pull = Some((
                buffer_id,
                std::time::Instant::now() + std::time::Duration::from_millis(1000),
            ));
        }
    }

    /// Start rename mode - select the symbol at cursor and allow inline editing
    pub(crate) fn start_rename(&mut self) -> AnyhowResult<()> {
        // If server supports prepareRename, validate first
        if self.server_supports_prepare_rename() {
            self.send_prepare_rename();
            return Ok(());
        }

        self.show_rename_prompt()
    }

    /// Handle prepareRename response — if valid, show rename prompt; if error, show message.
    pub(crate) fn handle_prepare_rename_response(
        &mut self,
        result: Result<serde_json::Value, String>,
    ) {
        match result {
            Ok(value) if !value.is_null() => {
                // prepareRename succeeded — show the rename prompt
                if let Err(e) = self.show_rename_prompt() {
                    self.set_status_message(format!("Rename failed: {e}"));
                }
            }
            Ok(_) => {
                self.set_status_message("Cannot rename at this position".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Cannot rename: {e}"));
            }
        }
    }

    /// Check if any LSP server for the current buffer supports prepareRename
    fn server_supports_prepare_rename(&self) -> bool {
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => return false,
        };

        if let Some(lsp) = &self.lsp {
            for sh in lsp.get_handles(&language) {
                if sh.capabilities.rename {
                    // prepareRename is advertised via prepare_support in client caps
                    // and supported if server has rename capability
                    return true;
                }
            }
        }
        false
    }

    /// Send textDocument/prepareRename to the LSP server
    fn send_prepare_rename(&mut self) {
        let cursor_pos = self.active_cursors().primary().position;
        let (line, character) = self
            .active_state()
            .buffer
            .position_to_lsp_position(cursor_pos);

        let buffer_id = self.active_buffer();
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) if m.lsp_enabled => m,
            _ => return,
        };
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => return,
        };
        let language = match self.buffers.get(&buffer_id).map(|s| s.language.clone()) {
            Some(l) => l,
            None => return,
        };

        self.next_lsp_request_id += 1;
        let request_id = self.next_lsp_request_id;

        if let Some(lsp) = &mut self.lsp {
            if let Some(sh) = lsp.handle_for_feature_mut(&language, LspFeature::Rename) {
                if let Err(e) =
                    sh.handle
                        .prepare_rename(request_id, uri, line as u32, character as u32)
                {
                    tracing::warn!("Failed to send prepareRename: {}", e);
                }
            }
        }
    }

    /// Show the rename prompt (called directly or after prepareRename succeeds).
    fn show_rename_prompt(&mut self) -> AnyhowResult<()> {
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

        // Get the current buffer and cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let (word_start, word_end) = {
            let state = self.active_state();

            // Find the word boundaries
            let word_start = find_word_start(&state.buffer, cursor_pos);
            let word_end = find_word_end(&state.buffer, cursor_pos);

            // Check if we're on a word
            if word_start >= word_end {
                self.status_message = Some(t!("lsp.no_symbol_at_cursor").to_string());
                return Ok(());
            }

            (word_start, word_end)
        };

        // Get the word text
        let word_text = self.active_state_mut().get_text_range(word_start, word_end);

        // Create an overlay to highlight the symbol being renamed
        let overlay_handle = self.add_overlay(
            None,
            word_start..word_end,
            crate::model::event::OverlayFace::Background {
                color: (50, 100, 200), // Blue background for rename
            },
            100,
            Some(t!("lsp.popup_renaming").to_string()),
        );

        // Enter rename mode using the Prompt system
        // Store the rename metadata in the PromptType and pre-fill the input with the current name
        let mut prompt = Prompt::new(
            "Rename to: ".to_string(),
            PromptType::LspRename {
                original_text: word_text.clone(),
                start_pos: word_start,
                end_pos: word_end,
                overlay_handle,
            },
        );
        // Pre-fill the input with the current name and position cursor at the end
        prompt.set_input(word_text);

        self.prompt = Some(prompt);
        Ok(())
    }

    /// Cancel rename mode - removes overlay if the prompt was for LSP rename
    pub(crate) fn cancel_rename_overlay(&mut self, handle: &crate::view::overlay::OverlayHandle) {
        self.remove_overlay(handle.clone());
    }

    /// Perform the actual LSP rename request
    pub(crate) fn perform_lsp_rename(
        &mut self,
        new_name: String,
        original_text: String,
        start_pos: usize,
        overlay_handle: crate::view::overlay::OverlayHandle,
    ) {
        // Remove the overlay first
        self.cancel_rename_overlay(&overlay_handle);

        // Check if the name actually changed
        if new_name == original_text {
            self.status_message = Some(t!("lsp.name_unchanged").to_string());
            return;
        }

        // Use the position from when we entered rename mode, NOT the current cursor position
        // This ensures we send the rename request for the correct symbol even if cursor moved
        let rename_pos = start_pos;

        // Convert byte position to LSP position (line, UTF-16 code units)
        // LSP uses UTF-16 code units for character offsets, not byte offsets
        let state = self.active_state();
        let (line, character) = state.buffer.position_to_lsp_position(rename_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, LspFeature::Rename, |handle, uri, _language| {
                let result = handle.rename(
                    request_id,
                    uri.clone(),
                    line as u32,
                    character as u32,
                    new_name.clone(),
                );
                if result.is_ok() {
                    tracing::info!(
                        "Requested rename at {}:{}:{} to '{}'",
                        uri.as_str(),
                        line,
                        character,
                        new_name
                    );
                }
                result.is_ok()
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
        } else if self
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|m| m.file_path())
            .is_none()
        {
            self.status_message = Some(t!("lsp.cannot_rename_unsaved").to_string());
        }
    }

    /// Request inlay hints for the active buffer (if enabled and LSP available)
    pub(crate) fn request_inlay_hints_for_active_buffer(&mut self) {
        if !self.config.editor.enable_inlay_hints {
            return;
        }

        let buffer_id = self.active_buffer();

        // Get line count from buffer state
        let line_count = if let Some(state) = self.buffers.get(&buffer_id) {
            state.buffer.line_count().unwrap_or(1000)
        } else {
            return;
        };
        let last_line = line_count.saturating_sub(1) as u32;
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(
                buffer_id,
                LspFeature::InlayHints,
                |handle, uri, _language| {
                    let result =
                        handle.inlay_hints(request_id, uri.clone(), 0, 0, last_line, 10000);
                    if result.is_ok() {
                        tracing::info!(
                            "Requested inlay hints for {} (request_id={})",
                            uri.as_str(),
                            request_id
                        );
                    } else if let Err(e) = &result {
                        tracing::debug!("Failed to request inlay hints: {}", e);
                    }
                    result.is_ok()
                },
            )
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_inlay_hints_request = Some(request_id);
        }
    }

    /// Schedule a folding range refresh for a buffer (debounced).
    pub(crate) fn schedule_folding_ranges_refresh(&mut self, buffer_id: BufferId) {
        let next_time = Instant::now() + Duration::from_millis(FOLDING_RANGES_DEBOUNCE_MS);
        self.folding_ranges_debounce.insert(buffer_id, next_time);
    }

    /// Issue a debounced folding range request if the timer has elapsed.
    pub(crate) fn maybe_request_folding_ranges_debounced(&mut self, buffer_id: BufferId) {
        let Some(ready_at) = self.folding_ranges_debounce.get(&buffer_id).copied() else {
            return;
        };
        if Instant::now() < ready_at {
            return;
        }

        self.folding_ranges_debounce.remove(&buffer_id);
        self.request_folding_ranges_for_buffer(buffer_id);
    }

    /// Request folding ranges for a buffer if supported and needed.
    pub(crate) fn request_folding_ranges_for_buffer(&mut self, buffer_id: BufferId) {
        if self.folding_ranges_in_flight.contains_key(&buffer_id) {
            return;
        }

        let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
            return;
        };
        if !metadata.lsp_enabled {
            return;
        }
        let Some(uri) = metadata.file_uri().cloned() else {
            return;
        };
        let file_path = metadata.file_path().cloned();

        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };

        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        if !lsp.folding_ranges_supported(&language) {
            return;
        }

        // Ensure there is a running server
        use crate::services::lsp::manager::LspSpawnResult;
        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            return;
        }

        let Some(sh) = lsp.handle_for_feature_mut(&language, LspFeature::FoldingRange) else {
            return;
        };
        let handle = &mut sh.handle;

        let request_id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;
        let buffer_version = self
            .buffers
            .get(&buffer_id)
            .map(|s| s.buffer.version())
            .unwrap_or(0);

        match handle.folding_ranges(request_id, uri) {
            Ok(()) => {
                self.pending_folding_range_requests.insert(
                    request_id,
                    super::FoldingRangeRequest {
                        buffer_id,
                        version: buffer_version,
                    },
                );
                self.folding_ranges_in_flight
                    .insert(buffer_id, (request_id, buffer_version));
            }
            Err(e) => {
                tracing::debug!("Failed to request folding ranges: {}", e);
            }
        }
    }

    /// Request semantic tokens for a specific buffer if supported and needed.
    pub(crate) fn maybe_request_semantic_tokens(&mut self, buffer_id: BufferId) {
        if !self.config.editor.enable_semantic_tokens_full {
            return;
        }

        // Avoid duplicate in-flight requests per buffer
        if self.semantic_tokens_in_flight.contains_key(&buffer_id) {
            return;
        }

        let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
            return;
        };
        if !metadata.lsp_enabled {
            return;
        }
        let Some(uri) = metadata.file_uri().cloned() else {
            return;
        };
        let file_path_for_spawn = metadata.file_path().cloned();
        // Get language from buffer state
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };

        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        // Ensure there is a running server
        use crate::services::lsp::manager::LspSpawnResult;
        if lsp.try_spawn(&language, file_path_for_spawn.as_deref()) != LspSpawnResult::Spawned {
            return;
        }

        // Check that a server actually supports full semantic tokens
        if !lsp.semantic_tokens_full_supported(&language) {
            return;
        }
        if lsp.semantic_tokens_legend(&language).is_none() {
            return;
        }

        let Some(state) = self.buffers.get(&buffer_id) else {
            return;
        };
        let buffer_version = state.buffer.version();
        if let Some(store) = state.semantic_tokens.as_ref() {
            if store.version == buffer_version {
                return; // Already up to date
            }
        }

        let previous_result_id = state
            .semantic_tokens
            .as_ref()
            .and_then(|store| store.result_id.clone());

        let Some(sh) = lsp.handle_for_feature_mut(&language, LspFeature::SemanticTokens) else {
            return;
        };
        // Check capabilities on the specific server we'll send to
        let supports_delta = sh.capabilities.semantic_tokens_full_delta;
        let use_delta = previous_result_id.is_some() && supports_delta;
        let handle = &mut sh.handle;

        let request_id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;

        let request_kind = if use_delta {
            super::SemanticTokensFullRequestKind::FullDelta
        } else {
            super::SemanticTokensFullRequestKind::Full
        };

        let request_result = if use_delta {
            handle.semantic_tokens_full_delta(request_id, uri, previous_result_id.unwrap())
        } else {
            handle.semantic_tokens_full(request_id, uri)
        };

        match request_result {
            Ok(_) => {
                self.pending_semantic_token_requests.insert(
                    request_id,
                    super::SemanticTokenFullRequest {
                        buffer_id,
                        version: buffer_version,
                        kind: request_kind,
                    },
                );
                self.semantic_tokens_in_flight
                    .insert(buffer_id, (request_id, buffer_version, request_kind));
            }
            Err(e) => {
                tracing::debug!("Failed to request semantic tokens: {}", e);
            }
        }
    }

    /// Schedule a full semantic token refresh for a buffer (debounced).
    pub(crate) fn schedule_semantic_tokens_full_refresh(&mut self, buffer_id: BufferId) {
        if !self.config.editor.enable_semantic_tokens_full {
            return;
        }

        let next_time = Instant::now() + Duration::from_millis(SEMANTIC_TOKENS_FULL_DEBOUNCE_MS);
        self.semantic_tokens_full_debounce
            .insert(buffer_id, next_time);
    }

    /// Issue a debounced full semantic token request if the timer has elapsed.
    pub(crate) fn maybe_request_semantic_tokens_full_debounced(&mut self, buffer_id: BufferId) {
        if !self.config.editor.enable_semantic_tokens_full {
            self.semantic_tokens_full_debounce.remove(&buffer_id);
            return;
        }

        let Some(ready_at) = self.semantic_tokens_full_debounce.get(&buffer_id).copied() else {
            return;
        };
        if Instant::now() < ready_at {
            return;
        }

        self.semantic_tokens_full_debounce.remove(&buffer_id);
        self.maybe_request_semantic_tokens(buffer_id);
    }

    /// Request semantic tokens for a viewport range (with padding).
    pub(crate) fn maybe_request_semantic_tokens_range(
        &mut self,
        buffer_id: BufferId,
        start_line: usize,
        end_line: usize,
    ) {
        let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
            return;
        };
        if !metadata.lsp_enabled {
            return;
        }
        let Some(uri) = metadata.file_uri().cloned() else {
            return;
        };
        let file_path = metadata.file_path().cloned();
        // Get language from buffer state
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };

        let Some(lsp) = self.lsp.as_mut() else {
            return;
        };

        // Ensure there is a running server
        use crate::services::lsp::manager::LspSpawnResult;
        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            return;
        }

        if !lsp.semantic_tokens_range_supported(&language) {
            // Fall back to full document tokens if no server supports range.
            self.maybe_request_semantic_tokens(buffer_id);
            return;
        }
        if lsp.semantic_tokens_legend(&language).is_none() {
            return;
        }

        let Some(sh) = lsp.handle_for_feature_mut(&language, LspFeature::SemanticTokens) else {
            return;
        };
        // The handle_for_feature_mut check ensures has_capability(SemanticTokens) which is
        // full || range. Double-check this specific server supports range.
        if !sh.capabilities.semantic_tokens_range {
            return;
        }
        let handle = &mut sh.handle;
        let Some(state) = self.buffers.get(&buffer_id) else {
            return;
        };

        let buffer_version = state.buffer.version();
        let mut padded_start = start_line.saturating_sub(SEMANTIC_TOKENS_RANGE_PADDING_LINES);
        let mut padded_end = end_line.saturating_add(SEMANTIC_TOKENS_RANGE_PADDING_LINES);

        if let Some(line_count) = state.buffer.line_count() {
            if line_count == 0 {
                return;
            }
            let max_line = line_count.saturating_sub(1);
            padded_start = padded_start.min(max_line);
            padded_end = padded_end.min(max_line);
        }

        let start_byte = state.buffer.line_start_offset(padded_start).unwrap_or(0);
        let end_char = state
            .buffer
            .get_line(padded_end)
            .map(|line| String::from_utf8_lossy(&line).encode_utf16().count())
            .unwrap_or(0);
        let end_byte = if state.buffer.line_start_offset(padded_end).is_some() {
            state.buffer.lsp_position_to_byte(padded_end, end_char)
        } else {
            state.buffer.len()
        };

        if start_byte >= end_byte {
            return;
        }

        let range = start_byte..end_byte;
        if let Some((in_flight_id, in_flight_start, in_flight_end, in_flight_version)) =
            self.semantic_tokens_range_in_flight.get(&buffer_id)
        {
            if *in_flight_start == padded_start
                && *in_flight_end == padded_end
                && *in_flight_version == buffer_version
            {
                return;
            }
            if let Err(e) = handle.cancel_request(*in_flight_id) {
                tracing::debug!("Failed to cancel semantic token range request: {}", e);
            }
            self.pending_semantic_token_range_requests
                .remove(in_flight_id);
            self.semantic_tokens_range_in_flight.remove(&buffer_id);
        }

        if let Some((applied_start, applied_end, applied_version)) =
            self.semantic_tokens_range_applied.get(&buffer_id)
        {
            if *applied_start == padded_start
                && *applied_end == padded_end
                && *applied_version == buffer_version
            {
                return;
            }
        }

        let now = Instant::now();
        if let Some((last_start, last_end, last_version, last_time)) =
            self.semantic_tokens_range_last_request.get(&buffer_id)
        {
            if *last_start == padded_start
                && *last_end == padded_end
                && *last_version == buffer_version
                && now.duration_since(*last_time)
                    < Duration::from_millis(SEMANTIC_TOKENS_RANGE_DEBOUNCE_MS)
            {
                return;
            }
        }

        let lsp_range = lsp_types::Range {
            start: lsp_types::Position {
                line: padded_start as u32,
                character: 0,
            },
            end: lsp_types::Position {
                line: padded_end as u32,
                character: end_char as u32,
            },
        };

        let request_id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;

        match handle.semantic_tokens_range(request_id, uri, lsp_range) {
            Ok(_) => {
                self.pending_semantic_token_range_requests.insert(
                    request_id,
                    SemanticTokenRangeRequest {
                        buffer_id,
                        version: buffer_version,
                        range: range.clone(),
                        start_line: padded_start,
                        end_line: padded_end,
                    },
                );
                self.semantic_tokens_range_in_flight.insert(
                    buffer_id,
                    (request_id, padded_start, padded_end, buffer_version),
                );
                self.semantic_tokens_range_last_request
                    .insert(buffer_id, (padded_start, padded_end, buffer_version, now));
            }
            Err(e) => {
                tracing::debug!("Failed to request semantic token range: {}", e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::{lsp_range_contains, Editor};

    fn range(sl: u32, sc: u32, el: u32, ec: u32) -> lsp_types::Range {
        lsp_types::Range {
            start: lsp_types::Position {
                line: sl,
                character: sc,
            },
            end: lsp_types::Position {
                line: el,
                character: ec,
            },
        }
    }

    #[test]
    fn test_lsp_range_contains_inclusive_start_exclusive_end() {
        let r = range(3, 10, 3, 20);
        // Before start
        assert!(!lsp_range_contains(&r, 3, 9));
        assert!(!lsp_range_contains(&r, 2, 50));
        // At start (inclusive)
        assert!(lsp_range_contains(&r, 3, 10));
        // Inside
        assert!(lsp_range_contains(&r, 3, 15));
        // Just before end (inclusive)
        assert!(lsp_range_contains(&r, 3, 19));
        // At end (exclusive)
        assert!(!lsp_range_contains(&r, 3, 20));
        // After end
        assert!(!lsp_range_contains(&r, 3, 21));
        assert!(!lsp_range_contains(&r, 4, 0));
    }

    #[test]
    fn test_lsp_range_contains_multiline() {
        let r = range(2, 5, 4, 3);
        // Line before start
        assert!(!lsp_range_contains(&r, 1, 100));
        // On start line, before start character
        assert!(!lsp_range_contains(&r, 2, 4));
        // On start line, at start character (inclusive)
        assert!(lsp_range_contains(&r, 2, 5));
        // Interior line — any character is inside.
        assert!(lsp_range_contains(&r, 3, 0));
        assert!(lsp_range_contains(&r, 3, 9999));
        // End line, before end character (inclusive)
        assert!(lsp_range_contains(&r, 4, 2));
        // End line, at end character (exclusive)
        assert!(!lsp_range_contains(&r, 4, 3));
        // Line after end
        assert!(!lsp_range_contains(&r, 5, 0));
    }

    #[test]
    fn test_lsp_range_contains_zero_length_matches_anchor_only() {
        // Point diagnostic: start == end.
        let r = range(7, 4, 7, 4);
        assert!(lsp_range_contains(&r, 7, 4));
        assert!(!lsp_range_contains(&r, 7, 3));
        assert!(!lsp_range_contains(&r, 7, 5));
        assert!(!lsp_range_contains(&r, 6, 4));
        assert!(!lsp_range_contains(&r, 8, 4));
    }
    use crate::model::buffer::Buffer;
    use crate::state::EditorState;
    use crate::view::virtual_text::VirtualTextPosition;
    use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position};

    fn make_hint(line: u32, character: u32, label: &str, kind: Option<InlayHintKind>) -> InlayHint {
        InlayHint {
            position: Position { line, character },
            label: InlayHintLabel::String(label.to_string()),
            kind,
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }
    }

    #[test]
    fn test_inlay_hint_inserts_before_character() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        state.buffer = Buffer::from_str_test("ab");

        if !state.buffer.is_empty() {
            state.marker_list.adjust_for_insert(0, state.buffer.len());
        }

        let hints = vec![make_hint(0, 1, ": i32", Some(InlayHintKind::TYPE))];
        Editor::apply_inlay_hints_to_state(&mut state, &hints);

        let lookup = state
            .virtual_texts
            .build_lookup(&state.marker_list, 0, state.buffer.len());
        let vtexts = lookup.get(&1).expect("expected hint at byte offset 1");
        assert_eq!(vtexts.len(), 1);
        assert_eq!(vtexts[0].text, ": i32");
        assert_eq!(vtexts[0].position, VirtualTextPosition::BeforeChar);
    }

    #[test]
    fn test_inlay_hint_at_eof_renders_after_last_char() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        state.buffer = Buffer::from_str_test("ab");

        if !state.buffer.is_empty() {
            state.marker_list.adjust_for_insert(0, state.buffer.len());
        }

        let hints = vec![make_hint(0, 2, ": i32", Some(InlayHintKind::TYPE))];
        Editor::apply_inlay_hints_to_state(&mut state, &hints);

        let lookup = state
            .virtual_texts
            .build_lookup(&state.marker_list, 0, state.buffer.len());
        let vtexts = lookup.get(&1).expect("expected hint anchored to last byte");
        assert_eq!(vtexts.len(), 1);
        assert_eq!(vtexts[0].text, ": i32");
        assert_eq!(vtexts[0].position, VirtualTextPosition::AfterChar);
    }

    #[test]
    fn test_inlay_hint_empty_buffer_is_ignored() {
        let mut state = EditorState::new(
            80,
            24,
            crate::config::LARGE_FILE_THRESHOLD_BYTES as usize,
            test_fs(),
        );
        state.buffer = Buffer::from_str_test("");

        let hints = vec![make_hint(0, 0, ": i32", Some(InlayHintKind::TYPE))];
        Editor::apply_inlay_hints_to_state(&mut state, &hints);

        assert!(state.virtual_texts.is_empty());
    }

    #[test]
    fn test_space_doc_paragraphs_inserts_blank_lines() {
        use super::space_doc_paragraphs;

        // Single newlines become double newlines
        let input = "sep\n  description.\nend\n  another.";
        let result = space_doc_paragraphs(input);
        assert_eq!(result, "sep\n\n  description.\n\nend\n\n  another.");
    }

    #[test]
    fn test_space_doc_paragraphs_preserves_existing_blank_lines() {
        use super::space_doc_paragraphs;

        // Already-double newlines stay double (not quadrupled)
        let input = "First paragraph.\n\nSecond paragraph.";
        let result = space_doc_paragraphs(input);
        assert_eq!(result, "First paragraph.\n\nSecond paragraph.");
    }

    #[test]
    fn test_space_doc_paragraphs_plain_text() {
        use super::space_doc_paragraphs;

        let input = "Just a single line of docs.";
        let result = space_doc_paragraphs(input);
        assert_eq!(result, "Just a single line of docs.");
    }
}
