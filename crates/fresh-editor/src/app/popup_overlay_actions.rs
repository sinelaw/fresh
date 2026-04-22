//! Popup, overlay, and LSP-confirmation orchestrators on `Editor`.
//!
//! Three loosely-related clusters that all manipulate the active buffer's
//! popup stack and overlay list via Event dispatch:
//!
//!   - Overlay management (add_overlay, remove_overlay,
//!     remove_overlays_in_range, clear_overlays)
//!   - Popup lifecycle (show_popup, hide_popup, dismiss_transient_popups,
//!     scroll_popup, on_editor_focus_lost, clear_popups, popup nav)
//!   - LSP confirmation popup (show_lsp_confirmation_popup,
//!     handle_lsp_confirmation_response, notify_lsp_current_file_opened,
//!     has_pending_lsp_confirmation)

use std::ops::Range;

use rust_i18n::t;

use crate::model::event::Event;

use super::Editor;

impl Editor {
    // === Overlay Management (Event-Driven) ===

    /// Add an overlay for decorations (underlines, highlights, etc.)
    pub fn add_overlay(
        &mut self,
        namespace: Option<crate::view::overlay::OverlayNamespace>,
        range: Range<usize>,
        face: crate::model::event::OverlayFace,
        priority: i32,
        message: Option<String>,
    ) -> crate::view::overlay::OverlayHandle {
        let event = Event::AddOverlay {
            namespace,
            range,
            face,
            priority,
            message,
            extend_to_line_end: false,
            url: None,
        };
        self.apply_event_to_active_buffer(&event);
        // Return the handle of the last added overlay
        let state = self.active_state();
        state
            .overlays
            .all()
            .last()
            .map(|o| o.handle.clone())
            .unwrap_or_default()
    }

    /// Remove an overlay by handle
    pub fn remove_overlay(&mut self, handle: crate::view::overlay::OverlayHandle) {
        let event = Event::RemoveOverlay { handle };
        self.apply_event_to_active_buffer(&event);
    }

    /// Remove all overlays in a range
    pub fn remove_overlays_in_range(&mut self, range: Range<usize>) {
        let event = Event::RemoveOverlaysInRange { range };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Clear all overlays
    pub fn clear_overlays(&mut self) {
        let event = Event::ClearOverlays;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === Popup Management (Event-Driven) ===

    /// Show a popup window
    pub fn show_popup(&mut self, popup: crate::model::event::PopupData) {
        let event = Event::ShowPopup { popup };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Show a popup and attach a confirm/cancel resolver to it. The
    /// `PopupData` event doesn't carry the resolver (it's a view-layer
    /// concern that doesn't need event-log replay); we set it on the
    /// resulting `Popup` immediately after `show_popup` pushes it.
    pub fn show_popup_with_resolver(
        &mut self,
        popup: crate::model::event::PopupData,
        resolver: crate::view::popup::PopupResolver,
    ) {
        self.show_popup(popup);
        if let Some(top) = self.active_state_mut().popups.top_mut() {
            top.resolver = resolver;
        }
    }

    /// Hide the topmost popup
    pub fn hide_popup(&mut self) {
        // Editor-level popups take precedence: dismiss them first if any are
        // visible. This avoids leaking a popup-stack pop event into the
        // active buffer's event log when the popup we're closing is global.
        if self.global_popups.is_visible() {
            self.global_popups.hide();

            // Clear hover symbol highlight if present (kept for parity with
            // the buffer-popup branch even though global popups don't use it
            // today — cheap no-op when nothing is set).
            if let Some(handle) = self.hover.take_symbol_overlay() {
                let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
                self.apply_event_to_active_buffer(&remove_overlay_event);
            }
            self.hover.set_symbol_range(None);
            return;
        }

        let event = Event::HidePopup;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);

        // Complete --wait tracking if this buffer had a popup-based wait
        let active = self.active_buffer();
        if let Some((wait_id, true)) = self.wait_tracking.remove(&active) {
            self.completed_waits.push(wait_id);
        }

        // Clear hover symbol highlight if present
        if let Some(handle) = self.hover.take_symbol_overlay() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.hover.set_symbol_range(None);
    }

    /// Dismiss transient popups if present
    /// These popups should be dismissed on scroll or other user actions
    pub(super) fn dismiss_transient_popups(&mut self) {
        // Action popups are persistent by design — only buffer-level transient
        // popups (Hover, Signature Help) get auto-dismissed here.
        let is_transient_popup = self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| p.transient);

        if is_transient_popup {
            self.hide_popup();
            tracing::trace!("Dismissed transient popup");
        }
    }

    /// Scroll any popup content by delta lines
    /// Positive delta scrolls down, negative scrolls up
    pub(super) fn scroll_popup(&mut self, delta: i32) {
        if let Some(popup) = self.global_popups.top_mut() {
            popup.scroll_by(delta);
            return;
        }
        if let Some(popup) = self.active_state_mut().popups.top_mut() {
            popup.scroll_by(delta);
            tracing::debug!(
                "Scrolled popup by {}, new offset: {}",
                delta,
                popup.scroll_offset
            );
        }
    }

    /// Called when the editor buffer loses focus (e.g., switching buffers,
    /// opening prompts/menus, focusing file explorer, etc.)
    ///
    /// This is the central handler for focus loss that:
    /// - Dismisses transient popups (Hover, Signature Help)
    /// - Clears LSP hover state and pending requests
    /// - Removes hover symbol highlighting
    pub(super) fn on_editor_focus_lost(&mut self) {
        // Dismiss transient popups via EditorState
        self.active_state_mut().on_focus_lost();

        // Clear hover state
        self.mouse_state.lsp_hover_state = None;
        self.mouse_state.lsp_hover_request_sent = false;
        self.hover.clear_pending();

        // Clear hover symbol highlight if present
        if let Some(handle) = self.hover.take_symbol_overlay() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.hover.set_symbol_range(None);

        // Any focus change (buffer switch, file explorer, menus, …) ends the
        // goto-line preview flow. Drop the snapshot so a later Esc cannot
        // rubber-band the cursor over state the user has moved past.
        self.goto_line_preview = None;
    }

    /// Clear all popups
    pub fn clear_popups(&mut self) {
        let event = Event::ClearPopups;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === LSP Confirmation Popup ===

    /// Show the LSP confirmation popup for a language server
    ///
    /// This displays a centered popup asking the user to confirm whether
    /// they want to start the LSP server for the given language.
    pub fn show_lsp_confirmation_popup(&mut self, language: &str) {
        use crate::model::event::{
            PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
        };

        // Get the server command for display
        let server_info = if let Some(lsp) = &self.lsp {
            if let Some(config) = lsp.get_config(language) {
                if !config.command.is_empty() {
                    format!("{} ({})", language, config.command)
                } else {
                    language.to_string()
                }
            } else {
                language.to_string()
            }
        } else {
            language.to_string()
        };

        let popup = PopupData {
            kind: PopupKindHint::List,
            title: Some(format!("Start LSP Server: {}?", server_info)),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "Allow this time".to_string(),
                        detail: Some("Start the LSP server for this session".to_string()),
                        icon: None,
                        data: Some("allow_once".to_string()),
                    },
                    PopupListItemData {
                        text: "Always allow".to_string(),
                        detail: Some("Always start this LSP server automatically".to_string()),
                        icon: None,
                        data: Some("allow_always".to_string()),
                    },
                    PopupListItemData {
                        text: "Don't start".to_string(),
                        detail: Some("Cancel LSP server startup".to_string()),
                        icon: None,
                        data: Some("deny".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::Centered,
            width: 50,
            max_height: 8,
            bordered: true,
        };

        // The language travels with the popup via its resolver so
        // confirm time reads it from the popup itself — no side-channel
        // Editor field needed, and no coupling between popups.
        self.show_popup_with_resolver(
            popup,
            crate::view::popup::PopupResolver::LspConfirm {
                language: language.to_string(),
            },
        );
    }

    /// Handle the LSP confirmation popup response
    ///
    /// This is called when the user confirms their selection in the LSP
    /// confirmation popup. It processes the response and starts the LSP
    /// server if approved.
    ///
    /// `language` is read from the confirming popup's `PopupResolver`
    /// (no side-channel), so `handle_popup_confirm`'s resolver match
    /// can call us directly with what it destructured out of the popup.
    pub fn handle_lsp_confirmation_response(&mut self, language: &str, action: &str) -> bool {
        let language = language.to_string();

        // Get file path from active buffer for workspace root detection
        let file_path = self
            .buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.file_path().cloned());

        match action {
            "allow_once" => {
                // Spawn the LSP server just this once (don't add to always-allowed)
                if let Some(lsp) = &mut self.lsp {
                    // Temporarily allow this language for spawning
                    lsp.allow_language(&language);
                    // Use force_spawn since user explicitly confirmed
                    if lsp.force_spawn(&language, file_path.as_deref()).is_some() {
                        tracing::info!("LSP server for {} started (allowed once)", language);
                        self.set_status_message(
                            t!("lsp.server_started", language = language).to_string(),
                        );
                    } else {
                        self.set_status_message(
                            t!("lsp.failed_to_start", language = language).to_string(),
                        );
                    }
                }
                // Notify LSP about the current file
                self.notify_lsp_current_file_opened(&language);
            }
            "allow_always" => {
                // Spawn the LSP server and remember the preference
                if let Some(lsp) = &mut self.lsp {
                    lsp.allow_language(&language);
                    // Use force_spawn since user explicitly confirmed
                    if lsp.force_spawn(&language, file_path.as_deref()).is_some() {
                        tracing::info!("LSP server for {} started (always allowed)", language);
                        self.set_status_message(
                            t!("lsp.server_started_auto", language = language).to_string(),
                        );
                    } else {
                        self.set_status_message(
                            t!("lsp.failed_to_start", language = language).to_string(),
                        );
                    }
                }
                // Notify LSP about the current file
                self.notify_lsp_current_file_opened(&language);
            }
            _ => {
                // User declined - don't start the server
                tracing::info!("LSP server for {} startup declined by user", language);
                self.set_status_message(
                    t!("lsp.startup_cancelled", language = language).to_string(),
                );
            }
        }

        true
    }

    /// Notify LSP about the currently open file
    ///
    /// This is called after an LSP server is started to notify it about
    /// the current file so it can provide features like diagnostics.
    fn notify_lsp_current_file_opened(&mut self, language: &str) {
        // Get buffer metadata for the active buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer()) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_current_file_opened: no metadata for buffer {:?}",
                    self.active_buffer()
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("notify_lsp_current_file_opened: LSP disabled for this buffer");
            return;
        }

        // Get file path for LSP spawn
        let file_path = metadata.file_path().cloned();

        // Get the URI (computed once in with_file)
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "notify_lsp_current_file_opened: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };

        // Get the buffer text and line count before borrowing lsp
        let active_buffer = self.active_buffer();

        // Use buffer's stored language to verify it matches the LSP server
        let file_language = match self.buffers.get(&active_buffer).map(|s| s.language.clone()) {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_current_file_opened: no buffer state");
                return;
            }
        };

        // Only notify if the file's language matches the LSP server we just started
        if file_language != language {
            tracing::debug!(
                "notify_lsp_current_file_opened: file language {} doesn't match server {}",
                file_language,
                language
            );
            return;
        }
        let (text, line_count, buffer_version) =
            if let Some(state) = self.buffers.get(&active_buffer) {
                let text = match state.buffer.to_string() {
                    Some(t) => t,
                    None => {
                        tracing::debug!("notify_lsp_current_file_opened: buffer not fully loaded");
                        return;
                    }
                };
                let line_count = state.buffer.line_count().unwrap_or(1000);
                (text, line_count, state.buffer.version())
            } else {
                tracing::debug!("notify_lsp_current_file_opened: no buffer state");
                return;
            };

        // Send didOpen to all LSP handles (use force_spawn to ensure they're started)
        if let Some(lsp) = &mut self.lsp {
            // force_spawn starts all servers for this language
            if lsp.force_spawn(language, file_path.as_deref()).is_some() {
                tracing::info!("Sending didOpen to LSP servers for: {}", uri.as_str());
                let mut any_opened = false;
                for sh in lsp.get_handles_mut(language) {
                    if let Err(e) =
                        sh.handle
                            .did_open(uri.clone(), text.clone(), file_language.clone())
                    {
                        tracing::warn!("Failed to send didOpen to '{}': {}", sh.name, e);
                    } else {
                        any_opened = true;
                    }
                }

                if any_opened {
                    tracing::info!("Successfully sent didOpen to LSP after confirmation");

                    // Request pull diagnostics from primary handle
                    if let Some(handle) = lsp.get_handle_mut(language) {
                        let previous_result_id =
                            self.diagnostic_result_ids.get(uri.as_str()).cloned();
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;

                        if let Err(e) =
                            handle.document_diagnostic(request_id, uri.clone(), previous_result_id)
                        {
                            tracing::debug!(
                                "Failed to request pull diagnostics (server may not support): {}",
                                e
                            );
                        }

                        // Request inlay hints if enabled
                        if self.config.editor.enable_inlay_hints {
                            let request_id = self.next_lsp_request_id;
                            self.next_lsp_request_id += 1;

                            let last_line = line_count.saturating_sub(1) as u32;
                            let last_char = 10000u32;

                            if let Err(e) = handle.inlay_hints(
                                request_id,
                                uri.clone(),
                                0,
                                0,
                                last_line,
                                last_char,
                            ) {
                                tracing::debug!(
                                    "Failed to request inlay hints (server may not support): {}",
                                    e
                                );
                            } else {
                                self.pending_inlay_hints_requests.insert(
                                    request_id,
                                    super::InlayHintsRequest {
                                        buffer_id: active_buffer,
                                        version: buffer_version,
                                    },
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// Check if the topmost visible popup is the LSP confirmation
    /// popup. Used by callers that need to know "is an LSP confirm
    /// prompt currently in front of the user?" — e.g. the file-open
    /// queue waits on this instead of racing past the prompt.
    pub fn has_pending_lsp_confirmation(&self) -> bool {
        use crate::view::popup::PopupResolver;
        let matches_lsp_confirm = |p: &crate::view::popup::Popup| -> bool {
            matches!(p.resolver, PopupResolver::LspConfirm { .. })
        };
        self.global_popups.top().is_some_and(matches_lsp_confirm)
            || self
                .active_state()
                .popups
                .top()
                .is_some_and(matches_lsp_confirm)
    }

    /// Navigate popup selection (next item)
    pub fn popup_select_next(&mut self) {
        if let Some(popup) = self.global_popups.top_mut() {
            popup.select_next();
            return;
        }
        let event = Event::PopupSelectNext;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup selection (previous item)
    pub fn popup_select_prev(&mut self) {
        if let Some(popup) = self.global_popups.top_mut() {
            popup.select_prev();
            return;
        }
        let event = Event::PopupSelectPrev;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup (page down)
    pub fn popup_page_down(&mut self) {
        if let Some(popup) = self.global_popups.top_mut() {
            popup.page_down();
            return;
        }
        let event = Event::PopupPageDown;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup (page up)
    pub fn popup_page_up(&mut self) {
        if let Some(popup) = self.global_popups.top_mut() {
            popup.page_up();
            return;
        }
        let event = Event::PopupPageUp;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }
}
