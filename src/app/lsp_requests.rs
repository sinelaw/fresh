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

use std::io;

use lsp_types::TextDocumentContentChangeEvent;

use crate::model::event::{BufferId, Event};
use crate::services::lsp::manager::detect_language;
use crate::view::prompt::{Prompt, PromptType};

use super::{uri_to_path, Editor};

impl Editor {
    /// Handle LSP completion response
    pub(crate) fn handle_completion_response(
        &mut self,
        request_id: u64,
        items: Vec<lsp_types::CompletionItem>,
    ) -> io::Result<()> {
        // Check if this is the pending completion request
        if self.pending_completion_request != Some(request_id) {
            tracing::debug!(
                "Ignoring completion response for outdated request {}",
                request_id
            );
            return Ok(());
        }

        self.pending_completion_request = None;
        self.lsp_status.clear();

        if items.is_empty() {
            tracing::debug!("No completion items received");
            return Ok(());
        }

        // Get the partial word at cursor to filter completions
        use crate::primitives::word_navigation::find_completion_word_start;
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

        if filtered_items.is_empty() {
            tracing::debug!("No completion items match prefix '{}'", prefix);
            return Ok(());
        }

        // Convert CompletionItem to PopupListItem
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
                // Store the insert_text or label as data
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

        // Show the popup
        use crate::model::event::{
            PopupContentData, PopupData, PopupListItemData, PopupPositionData,
        };
        let popup_data = PopupData {
            title: Some("Completion".to_string()),
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
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        };

        self.active_state_mut()
            .apply(&crate::model::event::Event::ShowPopup { popup: popup_data });

        tracing::info!("Showing completion popup with {} items", items.len());

        Ok(())
    }

    /// Handle LSP go-to-definition response
    pub(crate) fn handle_goto_definition_response(
        &mut self,
        request_id: u64,
        locations: Vec<lsp_types::Location>,
    ) -> io::Result<()> {
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
            self.status_message = Some("No definition found".to_string());
            return Ok(());
        }

        // For now, just jump to the first location
        let location = &locations[0];

        // Convert URI to file path
        if let Ok(path) = uri_to_path(&location.uri) {
            // Open the file
            let buffer_id = self.open_file(&path)?;

            // Move cursor to the definition position
            let line = location.range.start.line as usize;
            let character = location.range.start.character as usize;

            // Calculate byte position from line and character
            if let Some(state) = self.buffers.get(&buffer_id) {
                let position = state.buffer.line_col_to_position(line, character);

                // Move cursor
                let cursor_id = state.cursors.primary_id();
                let old_position = state.cursors.primary().position;
                let old_anchor = state.cursors.primary().anchor;
                let old_sticky_column = state.cursors.primary().sticky_column;
                let event = crate::model::event::Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: position,
                    old_anchor,
                    new_anchor: None,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for goto definition
                };

                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state.apply(&event);
                }
            }

            self.status_message = Some(format!(
                "Jumped to definition at {}:{}",
                path.display(),
                line + 1
            ));
        } else {
            self.status_message = Some("Could not open definition location".to_string());
        }

        Ok(())
    }

    /// Check if there are any pending LSP requests
    pub fn has_pending_lsp_requests(&self) -> bool {
        self.pending_completion_request.is_some() || self.pending_goto_definition_request.is_some()
    }

    /// Cancel any pending LSP requests
    /// This should be called when the user performs an action that would make
    /// the pending request's results stale (e.g., cursor movement, text editing)
    pub(crate) fn cancel_pending_lsp_requests(&mut self) {
        if let Some(request_id) = self.pending_completion_request.take() {
            tracing::debug!("Canceling pending LSP completion request {}", request_id);
            // Send cancellation to the LSP server
            self.send_lsp_cancel_request(request_id);
            self.lsp_status.clear();
        }
        if let Some(request_id) = self.pending_goto_definition_request.take() {
            tracing::debug!(
                "Canceling pending LSP goto-definition request {}",
                request_id
            );
            // Send cancellation to the LSP server
            self.send_lsp_cancel_request(request_id);
            self.lsp_status.clear();
        }
    }

    /// Send a cancel request to the LSP server for a specific request ID
    fn send_lsp_cancel_request(&mut self, request_id: u64) {
        // Get the current file path to determine language
        let metadata = self.buffer_metadata.get(&self.active_buffer());
        let file_path = metadata.and_then(|meta| meta.file_path());

        if let Some(path) = file_path {
            if let Some(language) = detect_language(path, &self.config.languages) {
                if let Some(lsp) = self.lsp.as_mut() {
                    if let Some(handle) = lsp.get_or_spawn(&language) {
                        if let Err(e) = handle.cancel_request(request_id) {
                            tracing::warn!("Failed to send LSP cancel request: {}", e);
                        } else {
                            tracing::debug!("Sent $/cancelRequest for request_id={}", request_id);
                        }
                    }
                }
            }
        }
    }

    /// Execute a closure with LSP handle, ensuring didOpen was sent first.
    ///
    /// This helper centralizes the logic for:
    /// 1. Getting buffer metadata, URI, and language
    /// 2. Getting or spawning the LSP handle
    /// 3. Ensuring didOpen was sent to this server instance (lazy - only gets text if needed)
    /// 4. Calling the provided closure with the handle
    ///
    /// Returns None if any step fails (no file, no language, LSP disabled, etc.)
    pub(crate) fn with_lsp_for_buffer<F, R>(&mut self, buffer_id: BufferId, f: F) -> Option<R>
    where
        F: FnOnce(&crate::services::lsp::async_handler::LspHandle, &lsp_types::Uri, &str) -> R,
    {
        // Get metadata (immutable borrow first to extract what we need)
        let (uri, _path, language) = {
            let metadata = self.buffer_metadata.get(&buffer_id)?;
            if !metadata.lsp_enabled {
                return None;
            }
            let uri = metadata.file_uri()?.clone();
            let path = metadata.file_path()?.to_path_buf();
            let language = detect_language(&path, &self.config.languages)?;
            (uri, path, language)
        };

        // Get handle ID (spawning if needed)
        let handle_id = {
            let lsp = self.lsp.as_mut()?;
            let handle = lsp.get_or_spawn(&language)?;
            handle.id()
        };

        // Check if didOpen is needed
        let needs_open = {
            let metadata = self.buffer_metadata.get(&buffer_id)?;
            !metadata.lsp_opened_with.contains(&handle_id)
        };

        if needs_open {
            // Only now get the text (can be expensive for large buffers)
            let text = self.buffers.get(&buffer_id)?.buffer.to_string()?;

            // Send didOpen
            {
                let lsp = self.lsp.as_mut()?;
                let handle = lsp.get_or_spawn(&language)?;
                if let Err(e) = handle.did_open(uri.clone(), text, language.clone()) {
                    tracing::warn!("Failed to send didOpen: {}", e);
                    return None;
                }
            }

            // Mark as opened with this server instance
            let metadata = self.buffer_metadata.get_mut(&buffer_id)?;
            metadata.lsp_opened_with.insert(handle_id);

            tracing::debug!(
                "Sent didOpen for {} to LSP handle {} (language: {})",
                uri.as_str(),
                handle_id,
                language
            );
        }

        // Call the closure with the handle
        let lsp = self.lsp.as_mut()?;
        let handle = lsp.get_or_spawn(&language)?;
        Some(f(handle, &uri, &language))
    }

    /// Request LSP completion at current cursor position
    pub(crate) fn request_completion(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
                let result =
                    handle.completion(request_id, uri.clone(), line as u32, character as u32);
                if result.is_ok() {
                    tracing::info!(
                        "Requested completion at {}:{}:{}",
                        uri.as_str(),
                        line,
                        character
                    );
                }
                result.is_ok()
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_completion_request = Some(request_id);
            self.lsp_status = "LSP: completion...".to_string();
        }

        Ok(())
    }

    /// Request LSP go-to-definition at current cursor position
    pub(crate) fn request_goto_definition(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
                let result =
                    handle.goto_definition(request_id, uri.clone(), line as u32, character as u32);
                if result.is_ok() {
                    tracing::info!(
                        "Requested go-to-definition at {}:{}:{}",
                        uri.as_str(),
                        line,
                        character
                    );
                }
                result.is_ok()
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_goto_definition_request = Some(request_id);
        }

        Ok(())
    }

    /// Request LSP hover documentation at current cursor position
    pub(crate) fn request_hover(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

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
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
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
            self.lsp_status = "LSP: hover...".to_string();
        }

        Ok(())
    }

    /// Request LSP hover documentation at a specific byte position
    /// Used for mouse-triggered hover
    pub(crate) fn request_hover_at_position(&mut self, byte_pos: usize) -> io::Result<()> {
        // Get the current buffer
        let state = self.active_state();

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(byte_pos);

        // Debug: Log the position conversion details
        if let Some(pos) = state.buffer.offset_to_position(byte_pos) {
            tracing::debug!(
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
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
                let result = handle.hover(request_id, uri.clone(), line as u32, character as u32);
                if result.is_ok() {
                    tracing::debug!(
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
            self.lsp_status = "LSP: hover...".to_string();
        }

        Ok(())
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
        self.lsp_status.clear();

        if contents.is_empty() {
            self.set_status_message("No hover information available".to_string());
            self.hover_symbol_range = None;
            return;
        }

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
            };
            self.apply_event_to_active_buffer(&event);
            // Store the handle for later removal
            if let Some(state) = self.buffers.get(&self.active_buffer()) {
                self.hover_symbol_overlay = state.overlays.all().last().map(|o| o.handle.clone());
            }
        } else {
            // No range provided by LSP, clear any previous highlight
            self.hover_symbol_range = None;
        }

        // Create a popup with the hover contents
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        // Use markdown rendering if the content is markdown
        let mut popup = if is_markdown {
            Popup::markdown(&contents, &self.theme)
        } else {
            // Plain text - split by lines
            let lines: Vec<String> = contents.lines().map(|s| s.to_string()).collect();
            Popup::text(lines, &self.theme)
        };

        // Configure popup properties
        popup.title = Some("Hover".to_string());
        popup.transient = true;
        // Use mouse position if this was a mouse-triggered hover, otherwise use cursor position
        popup.position = if let Some((x, y)) = self.mouse_hover_screen_position.take() {
            // Position below the mouse, offset by 1 row
            PopupPosition::Fixed { x, y: y + 1 }
        } else {
            PopupPosition::BelowCursor
        };
        popup.width = 80;
        popup.max_height = 20;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.show(popup);
            tracing::info!("Showing hover popup (markdown={})", is_markdown);
        }
    }

    /// Apply inlay hints to editor state as virtual text
    pub(crate) fn apply_inlay_hints_to_state(
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

            // Determine position based on hint kind
            // Type hints go after, parameter hints go before
            let position = match hint.kind {
                Some(lsp_types::InlayHintKind::TYPE) => VirtualTextPosition::AfterChar,
                Some(lsp_types::InlayHintKind::PARAMETER) => VirtualTextPosition::BeforeChar,
                _ => VirtualTextPosition::AfterChar, // Default to after
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
    pub(crate) fn request_references(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Extract the word under cursor for display
        let symbol = {
            let text = match state.buffer.to_string() {
                Some(t) => t,
                None => {
                    self.set_status_message("Buffer not fully loaded".to_string());
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
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
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
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_references_request = Some(request_id);
            self.pending_references_symbol = symbol;
            self.lsp_status = "LSP: finding references...".to_string();
        }

        Ok(())
    }

    /// Request LSP signature help at current cursor position
    pub(crate) fn request_signature_help(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);
        let buffer_id = self.active_buffer();
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
                let result =
                    handle.signature_help(request_id, uri.clone(), line as u32, character as u32);
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
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_signature_help_request = Some(request_id);
            self.lsp_status = "LSP: signature help...".to_string();
        }

        Ok(())
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
        self.lsp_status.clear();

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

        // Build the display content
        let mut lines: Vec<String> = Vec::new();

        // Add the signature label (function signature)
        lines.push(signature.label.clone());

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
                    lines.push(format!("> {}", param_label));
                }

                // Add parameter documentation if available
                if let Some(doc) = &param.documentation {
                    let doc_text = match doc {
                        lsp_types::Documentation::String(s) => s.clone(),
                        lsp_types::Documentation::MarkupContent(m) => m.value.clone(),
                    };
                    if !doc_text.is_empty() {
                        lines.push(String::new());
                        lines.push(doc_text);
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
                if lines.len() > 1 {
                    lines.push(String::new());
                    lines.push("---".to_string());
                }
                lines.push(doc_text);
            }
        }

        // Create a popup with the signature help
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let mut popup = Popup::text(lines, &self.theme);
        popup.title = Some("Signature Help".to_string());
        popup.transient = true;
        popup.position = PopupPosition::BelowCursor;
        popup.width = 60;
        popup.max_height = 10;
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

    /// Request LSP code actions at current cursor position
    pub(crate) fn request_code_actions(&mut self) -> io::Result<()> {
        // Get the current buffer and cursor position
        let state = self.active_state();
        let cursor_pos = state.cursors.primary().position;

        // Convert byte position to LSP position (line, UTF-16 code units)
        let (line, character) = state.buffer.position_to_lsp_position(cursor_pos);

        // Get selection range (if any) or use cursor position
        let (start_line, start_char, end_line, end_char) =
            if let Some(range) = state.cursors.primary().selection_range() {
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
        let request_id = self.next_lsp_request_id;

        // Use helper to ensure didOpen is sent before the request
        let sent = self
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
                let result = handle.code_actions(
                    request_id,
                    uri.clone(),
                    start_line,
                    start_char,
                    end_line,
                    end_char,
                    diagnostics,
                );
                if result.is_ok() {
                    tracing::info!(
                        "Requested code actions at {}:{}:{}-{}:{} (byte_pos={})",
                        uri.as_str(),
                        start_line,
                        start_char,
                        end_line,
                        end_char,
                        cursor_pos
                    );
                }
                result.is_ok()
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_code_actions_request = Some(request_id);
            self.lsp_status = "LSP: code actions...".to_string();
        }

        Ok(())
    }

    /// Handle code actions response from LSP
    pub(crate) fn handle_code_actions_response(
        &mut self,
        request_id: u64,
        actions: Vec<lsp_types::CodeActionOrCommand>,
    ) {
        // Check if this response is for the current pending request
        if self.pending_code_actions_request != Some(request_id) {
            tracing::debug!("Ignoring stale code actions response: {}", request_id);
            return;
        }

        self.pending_code_actions_request = None;
        self.lsp_status.clear();

        if actions.is_empty() {
            self.set_status_message("No code actions available".to_string());
            return;
        }

        // Build the display content
        let mut lines: Vec<String> = Vec::new();
        lines.push(format!("Code Actions ({}):", actions.len()));
        lines.push(String::new());

        for (i, action) in actions.iter().enumerate() {
            let title = match action {
                lsp_types::CodeActionOrCommand::Command(cmd) => &cmd.title,
                lsp_types::CodeActionOrCommand::CodeAction(ca) => &ca.title,
            };
            lines.push(format!("  {}. {}", i + 1, title));
        }

        lines.push(String::new());
        lines.push("Press number to select, Esc to cancel".to_string());

        // Create a popup with the code actions
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let mut popup = Popup::text(lines, &self.theme);
        popup.title = Some("Code Actions".to_string());
        popup.position = PopupPosition::BelowCursor;
        popup.width = 60;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.show(popup);
            tracing::info!("Showing code actions popup with {} actions", actions.len());
        }

        // Note: Executing code actions would require storing the actions and handling
        // key presses to select and apply them. This is left for future enhancement.
        self.set_status_message(format!(
            "Found {} code action(s) - selection not yet implemented",
            actions.len()
        ));
    }

    /// Handle find references response from LSP
    pub(crate) fn handle_references_response(
        &mut self,
        request_id: u64,
        locations: Vec<lsp_types::Location>,
    ) -> io::Result<()> {
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
        self.lsp_status.clear();

        if locations.is_empty() {
            self.set_status_message("No references found".to_string());
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
        self.set_status_message(format!("Found {} reference(s) for '{}'", count, symbol));

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
    ) -> io::Result<usize> {
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
                let cursor_id = state.cursors.primary_id();
                let delete_event = Event::Delete {
                    range: start_pos..end_pos,
                    deleted_text,
                    cursor_id,
                };
                batch_events.push(delete_event);
            }

            // Insert new text
            if !edit.new_text.is_empty() {
                let state = self
                    .buffers
                    .get(&buffer_id)
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;
                let cursor_id = state.cursors.primary_id();
                let insert_event = Event::Insert {
                    position: start_pos,
                    text: edit.new_text.clone(),
                    cursor_id,
                };
                batch_events.push(insert_event);
            }

            changes += 1;
        }

        // Create a batch event for all rename changes
        if !batch_events.is_empty() {
            let batch = Event::Batch {
                events: batch_events,
                description: "LSP Rename".to_string(),
            };
            self.apply_rename_batch_to_buffer(buffer_id, batch)?;
        }

        Ok(changes)
    }

    /// Handle rename response from LSP
    pub fn handle_rename_response(
        &mut self,
        _request_id: u64,
        result: Result<lsp_types::WorkspaceEdit, String>,
    ) -> io::Result<()> {
        self.lsp_status.clear();

        match result {
            Ok(workspace_edit) => {
                // Log the full workspace edit for debugging
                tracing::debug!(
                    "Received WorkspaceEdit: changes={:?}, document_changes={:?}",
                    workspace_edit.changes.as_ref().map(|c| c.len()),
                    workspace_edit.document_changes.as_ref().map(|dc| match dc {
                        lsp_types::DocumentChanges::Edits(e) => format!("{} edits", e.len()),
                        lsp_types::DocumentChanges::Operations(o) =>
                            format!("{} operations", o.len()),
                    })
                );

                // Apply the workspace edit
                let mut total_changes = 0;

                // Handle changes (map of URI -> Vec<TextEdit>)
                if let Some(changes) = workspace_edit.changes {
                    for (uri, edits) in changes {
                        if let Ok(path) = uri_to_path(&uri) {
                            let buffer_id = self.open_file(&path)?;
                            total_changes += self.apply_lsp_text_edits(buffer_id, edits)?;
                        }
                    }
                }

                // Handle document_changes (TextDocumentEdit[])
                // This is what rust-analyzer sends instead of changes
                if let Some(document_changes) = workspace_edit.document_changes {
                    use lsp_types::DocumentChanges;

                    let text_edits = match document_changes {
                        DocumentChanges::Edits(edits) => edits,
                        DocumentChanges::Operations(ops) => {
                            // Extract TextDocumentEdit from operations
                            ops.into_iter()
                                .filter_map(|op| {
                                    if let lsp_types::DocumentChangeOperation::Edit(edit) = op {
                                        Some(edit)
                                    } else {
                                        None
                                    }
                                })
                                .collect()
                        }
                    };

                    for text_doc_edit in text_edits {
                        let uri = text_doc_edit.text_document.uri;

                        if let Ok(path) = uri_to_path(&uri) {
                            let buffer_id = self.open_file(&path)?;

                            // Extract TextEdit from OneOf<TextEdit, AnnotatedTextEdit>
                            let edits: Vec<lsp_types::TextEdit> = text_doc_edit
                                .edits
                                .into_iter()
                                .map(|one_of| match one_of {
                                    lsp_types::OneOf::Left(text_edit) => text_edit,
                                    lsp_types::OneOf::Right(annotated) => annotated.text_edit,
                                })
                                .collect();

                            // Log the edits for debugging
                            tracing::info!(
                                "Applying {} edits from rust-analyzer for {:?}:",
                                edits.len(),
                                path
                            );
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

                            total_changes += self.apply_lsp_text_edits(buffer_id, edits)?;
                        }
                    }
                }

                self.status_message =
                    Some(format!("Renamed successfully ({} changes)", total_changes));
            }
            Err(error) => {
                // Per LSP spec: ContentModified errors (-32801) should NOT be shown to user
                // These are expected when document changes during LSP operations
                // Reference: https://github.com/neovim/neovim/issues/16900
                if error.contains("content modified") || error.contains("-32801") {
                    tracing::debug!(
                        "LSP rename: ContentModified error (expected, ignoring): {}",
                        error
                    );
                    self.status_message =
                        Some("Rename cancelled (document was modified)".to_string());
                } else {
                    // Show other errors to user
                    self.status_message = Some(format!("Rename failed: {}", error));
                }
            }
        }

        Ok(())
    }

    /// Helper to apply a batch of rename events to a specific buffer and notify LSP
    pub(crate) fn apply_rename_batch_to_buffer(
        &mut self,
        buffer_id: BufferId,
        batch: Event,
    ) -> io::Result<()> {
        // Add to event log
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            event_log.append(batch.clone());
        }

        // IMPORTANT: Calculate LSP changes BEFORE applying to buffer!
        // The byte positions in the events are relative to the ORIGINAL buffer,
        // so we must convert them to LSP positions before modifying the buffer.
        // Otherwise, the LSP server will receive incorrect position information.
        let original_active = self.active_buffer();
        // Temporarily switch buffer for LSP change collection (no side effects needed)
        self.split_manager.set_active_buffer_id(buffer_id);
        let lsp_changes = self.collect_lsp_changes(&batch);
        self.split_manager.set_active_buffer_id(original_active);

        // Save cursor position before applying batch
        // The batch will move the cursor to each edit location, but we want to
        // preserve the cursor position (adjusted for edits before it)
        let state = self
            .buffers
            .get(&buffer_id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;
        let original_cursor_pos = state.cursors.primary().position;
        let original_cursor_anchor = state.cursors.primary().anchor;

        // Calculate cursor position adjustment based on edits
        // Edits are applied in reverse order (end of file to start), but we need
        // to calculate the cumulative delta for all edits before the cursor
        let mut cursor_delta: isize = 0;
        if let Event::Batch { events, .. } = &batch {
            for event in events {
                match event {
                    Event::Delete { range, .. } => {
                        if range.end <= original_cursor_pos {
                            // Delete entirely before cursor - cursor moves back
                            cursor_delta -= range.len() as isize;
                        } else if range.start < original_cursor_pos {
                            // Delete crosses cursor - cursor moves to start of delete
                            cursor_delta = range.start as isize - original_cursor_pos as isize;
                        }
                        // Delete entirely after cursor - no effect
                    }
                    Event::Insert { position, text, .. } => {
                        // Only move cursor if insert is STRICTLY BEFORE cursor position
                        // If insert is AT cursor, cursor should stay at start of new text
                        let adjusted_cursor =
                            (original_cursor_pos as isize + cursor_delta) as usize;
                        if *position < adjusted_cursor {
                            // Insert before cursor - cursor moves forward
                            cursor_delta += text.len() as isize;
                        }
                        // Insert at or after cursor - no effect on cursor position
                    }
                    _ => {}
                }
            }
        }

        // Apply to buffer state
        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Buffer not found"))?;
        state.apply(&batch);

        // Restore cursor to adjusted position
        let buffer_len = state.buffer.len();
        let new_cursor_pos =
            ((original_cursor_pos as isize + cursor_delta).max(0) as usize).min(buffer_len);
        state.cursors.primary_mut().position = new_cursor_pos;

        // Adjust anchor if there was a selection
        if let Some(anchor) = original_cursor_anchor {
            let new_anchor = ((anchor as isize + cursor_delta).max(0) as usize).min(buffer_len);
            state.cursors.primary_mut().anchor = Some(new_anchor);
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

        // Get the file path for language detection
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("send_lsp_changes_for_buffer: no file path for buffer");
                return;
            }
        };

        let language = match detect_language(path, &self.config.languages) {
            Some(l) => l,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no language detected for {:?}",
                    path
                );
                return;
            }
        };

        tracing::trace!(
            "send_lsp_changes_for_buffer: sending {} changes to {} in single didChange notification",
            changes.len(),
            uri.as_str()
        );

        // Get handle ID first
        let handle_id = {
            let Some(lsp) = self.lsp.as_mut() else {
                tracing::debug!("send_lsp_changes_for_buffer: no LSP manager available");
                return;
            };
            let Some(handle) = lsp.get_or_spawn(&language) else {
                tracing::warn!(
                    "send_lsp_changes_for_buffer: failed to get or spawn LSP client for {}",
                    language
                );
                return;
            };
            handle.id()
        };

        // Check if didOpen needs to be sent first
        let needs_open = {
            let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
                return;
            };
            !metadata.lsp_opened_with.contains(&handle_id)
        };

        if needs_open {
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

            // Send didOpen first
            if let Some(lsp) = self.lsp.as_mut() {
                if let Some(handle) = lsp.get_or_spawn(&language) {
                    if let Err(e) = handle.did_open(uri.clone(), text, language.clone()) {
                        tracing::warn!("Failed to send didOpen before didChange: {}", e);
                        return;
                    }
                    tracing::debug!(
                        "Sent didOpen for {} to LSP handle {} before didChange",
                        uri.as_str(),
                        handle_id
                    );
                }
            }

            // Mark as opened
            if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                metadata.lsp_opened_with.insert(handle_id);
            }
        }

        // Now send didChange
        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                if let Err(e) = client.did_change(uri, changes) {
                    tracing::warn!("Failed to send didChange to LSP: {}", e);
                } else {
                    tracing::trace!("Successfully sent batched didChange to LSP");
                }
            }
        }
    }

    /// Start rename mode - select the symbol at cursor and allow inline editing
    pub(crate) fn start_rename(&mut self) -> io::Result<()> {
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

        // Get the current buffer and cursor position
        let (word_start, word_end) = {
            let state = self.active_state();
            let cursor_pos = state.cursors.primary().position;

            // Find the word boundaries
            let word_start = find_word_start(&state.buffer, cursor_pos);
            let word_end = find_word_end(&state.buffer, cursor_pos);

            // Check if we're on a word
            if word_start >= word_end {
                self.status_message = Some("No symbol at cursor".to_string());
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
            Some("Renaming".to_string()),
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
            self.status_message = Some("Name unchanged".to_string());
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
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
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
            self.lsp_status = "LSP: rename...".to_string();
        } else if self
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|m| m.file_path())
            .is_none()
        {
            self.status_message = Some("Cannot rename in unsaved buffer".to_string());
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
            .with_lsp_for_buffer(buffer_id, |handle, uri, _language| {
                let result = handle.inlay_hints(request_id, uri.clone(), 0, 0, last_line, 10000);
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
            })
            .unwrap_or(false);

        if sent {
            self.next_lsp_request_id += 1;
            self.pending_inlay_hints_request = Some(request_id);
        }
    }
}
