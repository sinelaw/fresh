//! Popup and overlay orchestrators on `Editor`.
//!
//! Two loosely-related clusters that both manipulate the active buffer's
//! popup stack and overlay list via Event dispatch:
//!
//!   - Overlay management (add_overlay, remove_overlay,
//!     remove_overlays_in_range, clear_overlays)
//!   - Popup lifecycle (show_popup, hide_popup, dismiss_transient_popups,
//!     scroll_popup, on_editor_focus_lost, clear_popups, popup nav)

use std::ops::Range;

use crate::model::event::Event;

use super::window::Window;
use super::Editor;

impl Editor {
    // === Overlay Management (Event-Driven) ===

    /// Add a decoration overlay (underline, highlight, etc.) to the active
    /// buffer and return its handle for later removal.
    ///
    /// Decoration overlays are ephemeral and not part of the undo history, and
    /// `AddOverlay` fires no plugin hook, so this adds directly to the buffer's
    /// overlay manager rather than round-tripping through an `AddOverlay` event
    /// — which would discard the handle. The handle comes straight from the add
    /// (via [`EditorState::add_overlay`]); recovering it from
    /// `overlays.all().last()` would be wrong — the set is unordered and a
    /// removal swaps entries around, so `.last()` is some other producer's
    /// overlay (e.g. an error diagnostic), not the one just added.
    pub fn add_overlay(
        &mut self,
        namespace: Option<crate::view::overlay::OverlayNamespace>,
        range: Range<usize>,
        face: crate::model::event::OverlayFace,
        priority: i32,
        message: Option<String>,
    ) -> crate::view::overlay::OverlayHandle {
        self.active_state_mut()
            .add_overlay(namespace, range, face, priority, message, false, None)
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
        // Stamp the freshly-pushed popup with the user's actual
        // focus-popup keybinding so the title hint reflects the
        // configured key (default `Alt+T`). The PopupData event itself
        // doesn't carry this — it's a view-layer concern set after the
        // converter pushes the Popup onto the active buffer's stack.
        //
        // Background / border: the `convert_popup_data_to_popup` shim is
        // called from `EditorState::apply` without a theme handle, so its
        // replay path uses theme *defaults*. Re-stamp here with the
        // *live* theme's `popup_bg` / `popup_border_fg` so the popup
        // tracks the user's active theme (e.g. an ANSI-16 dark theme
        // overrides the default `Rgb(30, 30, 30)` here).
        let hint = self.popup_focus_key_hint();
        let (popup_bg, popup_border_fg) = {
            let theme = self.theme();
            (theme.popup_bg, theme.popup_border_fg)
        };
        if let Some(top) = self.active_state_mut().popups.top_mut() {
            top.focus_key_hint = hint;
            top.background_style = ratatui::style::Style::default().bg(popup_bg);
            top.border_style = ratatui::style::Style::default().fg(popup_border_fg);
        }
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
            if let Some(handle) = self.active_window_mut().hover.take_symbol_overlay() {
                let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
                self.apply_event_to_active_buffer(&remove_overlay_event);
            }
            self.active_window_mut().hover.set_symbol_range(None);
            return;
        }

        let event = Event::HidePopup;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);

        // Complete --wait tracking if this buffer had a popup-based wait
        let active = self.active_buffer();
        if let Some((wait_id, true)) = self.active_window_mut().wait_tracking.remove(&active) {
            self.active_window_mut().completed_waits.push(wait_id);
        }

        // Clear hover symbol highlight if present
        if let Some(handle) = self.active_window_mut().hover.take_symbol_overlay() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.active_window_mut().hover.set_symbol_range(None);
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

    /// Clear all popups
    pub fn clear_popups(&mut self) {
        let event = Event::ClearPopups;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Dismiss popups that overlap with a new modal UI (a prompt
    /// opening, a status-bar indicator switching to a picker, etc.).
    ///
    /// Targets menu-style popups (`List` / `Action`) on both the
    /// buffer-local and editor-wide stacks. `Completion` and `Hover`
    /// popups are intentionally left alone: a Completion popup is
    /// driven by typing into the very prompt that may have just
    /// opened (e.g. type-to-filter), and a `Hover` popup is a
    /// transient documentation overlay that the existing transient-
    /// dismiss logic already handles.
    ///
    /// Use this before opening any prompt or other top-level picker
    /// so a previously-open LSP-Servers popup (or plugin action
    /// popup) doesn't keep overlapping the new UI. The user-reported
    /// flow that motivated this is: LSP indicator popup open →
    /// click the language indicator → language picker prompt opens,
    /// LSP popup stays overlapping it. (#1941 follow-up)
    pub fn dismiss_menu_popups_for_prompt(&mut self) {
        use crate::view::popup::PopupKind;

        // Buffer-local popup stack — drop menu/action popups.
        // ClearPopups is a single event that nukes the whole stack;
        // since menu/action popups dominate the stack and we don't
        // expect a mixed stack of completion-under-menu, this is a
        // pragmatic over-approximation. If a future caller stacks a
        // Completion under a List on the same buffer, we'd need to
        // selectively pop instead — there's no current callsite that
        // does that.
        let buffer_local_has_menu_or_action = self
            .active_state()
            .popups
            .all()
            .iter()
            .any(|p| matches!(p.kind, PopupKind::List | PopupKind::Action));
        if buffer_local_has_menu_or_action {
            self.clear_popups();
        }

        // Editor-wide popup stack: pop popups while the top is a
        // List/Action menu popup. Skip if the top is a Completion or
        // Hover popup (the rule above).
        while self
            .global_popups
            .top()
            .is_some_and(|p| matches!(p.kind, PopupKind::List | PopupKind::Action))
        {
            self.global_popups.hide();
        }
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

impl Window {
    /// Called when the editor buffer loses focus (e.g., switching buffers,
    /// opening prompts/menus, focusing file explorer, etc.)
    ///
    /// Dismisses transient popups, clears LSP hover state and pending requests,
    /// and removes hover symbol highlighting.
    pub(crate) fn on_editor_focus_lost(&mut self) {
        // Dismiss transient popups via EditorState
        self.active_state_mut().on_focus_lost();

        // Clear hover state
        self.mouse_state.lsp_hover_state = None;
        self.mouse_state.lsp_hover_request_sent = false;
        self.hover.clear_pending();

        // Clear hover symbol highlight if present. Inlined from
        // `Event::RemoveOverlay` handling (state.rs) so we don't have to
        // reach back through `Editor::apply_event_to_active_buffer`.
        if let Some(handle) = self.hover.take_symbol_overlay() {
            let state = self.active_state_mut();
            state
                .overlays
                .remove_by_handle(&handle, &mut state.marker_list);
        }
        self.hover.set_symbol_range(None);

        // Any focus change (buffer switch, file explorer, menus, …) ends the
        // goto-line preview flow. Drop the snapshot so a later Esc cannot
        // rubber-band the cursor over state the user has moved past.
        self.goto_line_preview = None;
    }
}
