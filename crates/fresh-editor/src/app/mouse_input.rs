//! Mouse input handling.
//!
//! This module contains all mouse event handling logic including:
//! - Click, double-click, and drag handling
//! - Scrollbar interaction
//! - Hover target computation
//! - Split separator dragging
//! - Text selection via mouse

use super::*;
use crate::input::keybindings::Action;
use crate::model::event::{ContainerId, CursorId, LeafId, SplitDirection};
use crate::services::plugins::hooks::HookArgs;
use crate::view::popup_mouse::{popup_areas_to_layout_info, PopupHitTester};
use crate::view::prompt::PromptType;
use crate::view::ui::tabs::TabHit;
use anyhow::Result as AnyhowResult;
use ratatui::layout::Rect;
use rust_i18n::t;

/// Returns true if (col, row) falls inside `rect`.
fn in_rect(col: u16, row: u16, rect: Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

impl Editor {
    /// If any overlay layer captures mouse events, dispatch to its
    /// dedicated handler and return its result; otherwise return `None`
    /// so the caller continues with the normal click/wheel pipeline.
    ///
    /// This is the mouse counterpart of `resolve_focus_context` /
    /// `presents_blocking_overlay`: precedence is the order of
    /// `overlay_layers()`, top-first. Only the kinds whose modal
    /// handlers exist need an arm here — non-capturing layers fall
    /// through.
    fn dispatch_modal_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
        is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        use crate::app::overlay::LayerKind;

        // Snapshot the capturing kinds first so the borrow ends before
        // any `&mut self` handler runs.
        let capturing_kind = self.overlay_layers().iter().find_map(|l| match l.kind {
            LayerKind::Settings
            | LayerKind::KeybindingEditor
            | LayerKind::CalibrationWizard
            | LayerKind::WorkspaceTrust
            | LayerKind::FloatingModal => Some(l.kind),
            _ => None,
        })?;
        Some(match capturing_kind {
            LayerKind::KeybindingEditor => self.handle_keybinding_editor_mouse(mouse_event),
            LayerKind::Settings => self.handle_settings_mouse(mouse_event, is_double_click),
            // The calibration wizard owns the modal z-band but ignores
            // every mouse event (its UI is keyboard-driven). Swallowing
            // here matches the previous explicit `return Ok(false)`.
            LayerKind::CalibrationWizard => Ok(false),
            LayerKind::WorkspaceTrust => self.handle_workspace_trust_mouse(mouse_event),
            // The centered widget modal (orchestrator control room /
            // New-Session form) captures the whole mouse channel here —
            // before the terminal-forward and the editor's buffer paths —
            // so a click/double-click/scroll over the dialog never leaks to
            // an alternate-screen terminal or the buffer it covers. Clicks
            // route to the panel's own hit-test (focusing the clicked
            // widget); everything else is swallowed.
            LayerKind::FloatingModal => self.handle_floating_modal_mouse(mouse_event),
            _ => unreachable!("find_map only returns capturing kinds"),
        })
    }

    /// Mouse handler for the centered widget modal (`floating_widget_panel`).
    /// The dialog is fully modal: presses hit-test the panel (focusing the
    /// clicked widget / placing the text cursor), wheel scrolls it, and a
    /// drag drives only its scrollbar. Every other event — and every press
    /// that lands outside the panel box — is swallowed, so nothing reaches
    /// the buffer, terminal, or dock beneath. Always returns
    /// `Ok(true)` (a render is cheap and the modal just consumed an event).
    fn handle_floating_modal_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};
        let (col, row) = (mouse_event.column, mouse_event.row);
        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                // An anchored popup (right-click context menu) dismisses when
                // the press lands outside its box — standard menu behaviour.
                // The centered modal instead swallows outside-clicks (it has
                // explicit Cancel / Esc).
                if self.floating_panel_is_anchored()
                    && !self.point_in_floating_panel(super::PanelSlot::Floating, col, row)
                {
                    self.dismiss_floating_panel_with_cancel(super::PanelSlot::Floating);
                    return Ok(true);
                }
                // Single / double / triple clicks all map to one panel
                // hit-test — never the buffer's word/line select beneath.
                self.handle_floating_widget_click(super::PanelSlot::Floating, col, row);
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                // Only a scrollbar drag is meaningful; other drags are
                // swallowed rather than starting a buffer text-selection.
                self.try_widget_scrollbar_drag(super::PanelSlot::Floating, row);
            }
            MouseEventKind::Up(MouseButton::Left) => {
                self.release_widget_scrollbar();
            }
            MouseEventKind::ScrollUp => {
                self.handle_floating_widget_panel_wheel(super::PanelSlot::Floating, col, row, -3);
            }
            MouseEventKind::ScrollDown => {
                self.handle_floating_widget_panel_wheel(super::PanelSlot::Floating, col, row, 3);
            }
            // Right-click, horizontal scroll, motion, other-button releases:
            // swallowed — the modal eats them all.
            _ => {}
        }
        Ok(true)
    }

    /// Handle a mouse event.
    /// Returns true if a re-render is needed.
    pub fn handle_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        let (is_double_click, is_triple_click) = self.detect_multi_click(&mouse_event, col, row);

        // Modal mouse-capture: walk the overlay stack top-down (the same
        // list `get_key_context` / `dispatch_terminal_input` consult) and
        // dispatch to the first layer that captures mouse. This replaces
        // a hand-listed ladder that had drifted out of order with the
        // keyboard dispatcher.
        if let Some(result) = self.dispatch_modal_mouse(mouse_event, is_double_click) {
            return result;
        }

        // Cancel LSP rename prompt on any mouse interaction
        let mut needs_render = false;
        if let Some(ref prompt) = self.active_window_mut().prompt {
            if matches!(prompt.prompt_type, PromptType::LspRename { .. }) {
                self.cancel_prompt();
                needs_render = true;
            }
        }

        // Update mouse cursor position for software cursor rendering (used by GPM)
        // When GPM is active, we always need to re-render to update the cursor position
        let cursor_moved = self.active_window_mut().mouse_cursor_position != Some((col, row));
        self.active_window_mut().mouse_cursor_position = Some((col, row));
        if self.active_window_mut().gpm_active && cursor_moved {
            needs_render = true;
        }

        tracing::trace!(
            "handle_mouse: kind={:?}, col={}, row={}",
            mouse_event.kind,
            col,
            row
        );

        // Check if we should forward mouse events to the terminal
        // Forward if: in terminal mode, mouse is over terminal buffer, and terminal is in alternate screen mode
        //
        // ...unless a chrome drag is in progress (dock-border resize, split
        // separator, or file-explorer width). That drag owns the mouse until
        // release, so don't let an alternate-screen terminal swallow the
        // motion once the pointer crosses over it — *growing* the dock drags
        // the cursor rightward across a full-screen `btop`, and forwarding
        // there both stalls the resize and eats the mouse-up that ends it,
        // leaving the drag stuck. Shrinking happened to work only because the
        // pointer stays left of the terminal the whole time.
        let chrome_drag_active = self.dock_resizing || {
            let ms = &self.active_window().mouse_state;
            ms.dragging_separator.is_some() || ms.drag_start_explorer_width.is_some()
        };
        if !chrome_drag_active {
            if let Some(result) =
                self.active_window_mut()
                    .try_forward_mouse_to_terminal(col, row, mouse_event)
            {
                return result;
            }
        }

        // Ctrl+Click on a file path printed in the live terminal opens it in
        // Fresh (jumping to any :line:col it encodes). Handled before normal
        // click routing so it doesn't disturb cursor/selection state.
        if let Some(result) = self.try_open_terminal_link(col, row, mouse_event) {
            return result;
        }

        // Dismiss theme info popup on any left-click; check if click is on the button first
        if self.active_window_mut().theme_info_popup.is_some() {
            if let MouseEventKind::Down(MouseButton::Left) = mouse_event.kind {
                if let Some((popup_rect, button_row_offset)) = self.theme_info_popup_rect() {
                    if in_rect(col, row, popup_rect) {
                        // Check if click is on the button row (last content row
                        // before border). `button_row_offset` is `None` when the
                        // popup has no theme keys (no button to open).
                        if let Some(offset) = button_row_offset {
                            let actual_button_row = popup_rect.y + offset;
                            if row == actual_button_row {
                                let key =
                                    self.active_window_mut().theme_info_popup.as_ref().and_then(
                                        |p| p.info.fg_key.clone().or_else(|| p.info.bg_key.clone()),
                                    );
                                self.active_window_mut().theme_info_popup = None;
                                if let Some(key) = key {
                                    self.fire_theme_inspect_hook(key);
                                }
                                return Ok(true);
                            }
                        }
                        // Click inside popup but not on an actionable button - ignore
                        return Ok(true);
                    }
                }
                // Click outside popup - dismiss
                self.active_window_mut().theme_info_popup = None;
                needs_render = true;
            }
        }

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                if is_double_click || is_triple_click {
                    if let Some((buffer_id, byte_pos)) =
                        self.fold_toggle_line_at_screen_position(col, row)
                    {
                        self.active_window_mut()
                            .toggle_fold_at_byte(buffer_id, byte_pos);
                        needs_render = true;
                        return Ok(needs_render);
                    }
                }
                if is_triple_click {
                    // Triple click detected - select entire line
                    self.handle_mouse_triple_click(col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                if is_double_click {
                    // Double click detected - both clicks within time threshold AND at same position
                    self.handle_mouse_double_click(col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                self.handle_mouse_click(col, row, mouse_event.modifiers)?;
                needs_render = true;
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                self.handle_mouse_drag(col, row)?;
                needs_render = true;
            }
            MouseEventKind::Up(MouseButton::Left) => {
                // End a dock-resize drag and persist the chosen width so
                // it survives toggling the dock off/on.
                if self.dock_resizing {
                    self.dock_resizing = false;
                    if let Some(super::PanelPlacement::LeftDock { width_cols }) =
                        self.dock.as_ref().map(|f| f.placement)
                    {
                        self.dock_width = Some(width_cols);
                    }
                    return Ok(true);
                }
                // Check if we were dragging a separator to trigger terminal resize
                let was_dragging_separator = self
                    .active_window_mut()
                    .mouse_state
                    .dragging_separator
                    .is_some();

                // Check if we were dragging a tab and complete the drop
                if let Some(drag_state) = self.active_window_mut().mouse_state.dragging_tab.take() {
                    if drag_state.is_dragging() {
                        if let Some(drop_zone) = drag_state.drop_zone {
                            self.execute_tab_drop(
                                drag_state.buffer_id,
                                drag_state.source_split_id,
                                drop_zone,
                            );
                        }
                    }
                }

                // Stop dragging and clear drag state
                self.release_widget_scrollbar();
                self.clear_active_window_drag_state();

                // If we finished dragging a split separator, the split
                // ratios changed: reflow through the single layout funnel.
                if was_dragging_separator {
                    self.relayout();
                }

                needs_render = true;
            }
            MouseEventKind::Moved => {
                // Dispatch MouseMove hook to plugins (fire-and-forget, no blocking check)
                {
                    // Find content rect for the split under the mouse
                    let content_rect = self
                        .active_layout()
                        .split_areas
                        .iter()
                        .find(|(_, _, content_rect, _, _, _)| in_rect(col, row, *content_rect))
                        .map(|(_, _, rect, _, _, _)| *rect);

                    let (content_x, content_y) = content_rect.map(|r| (r.x, r.y)).unwrap_or((0, 0));

                    self.plugin_manager.read().unwrap().run_hook(
                        "mouse_move",
                        HookArgs::MouseMove {
                            column: col,
                            row,
                            content_x,
                            content_y,
                        },
                    );
                }

                // Only re-render if hover target actually changed
                // (preserve needs_render if already set, e.g., for GPM cursor updates)
                let hover_changed = self.update_hover_target(col, row);
                needs_render = needs_render || hover_changed;

                // Ctrl+hover over a resolvable path in the live terminal
                // underlines it to signal it's clickable.
                let term_link_changed =
                    self.update_terminal_link_hover(col, row, mouse_event.modifiers);
                needs_render = needs_render || term_link_changed;

                // Update theme info popup button highlight on hover (only when
                // the popup actually has a button — the keyless message variant
                // returns `None` and never highlights).
                if let Some((popup_rect, Some(button_row_offset))) = self.theme_info_popup_rect() {
                    let button_row = popup_rect.y + button_row_offset;
                    let new_highlighted = row == button_row
                        && col >= popup_rect.x
                        && col < popup_rect.x + popup_rect.width;
                    if let Some(ref mut popup) = self.active_window_mut().theme_info_popup {
                        if popup.button_highlighted != new_highlighted {
                            popup.button_highlighted = new_highlighted;
                            needs_render = true;
                        }
                    }
                }

                // Track LSP hover state for mouse-triggered hover popups
                self.update_lsp_hover_state(col, row);

                // The dock's overlay scrollbar follows the pointer: reveal it
                // while the mouse is over the sessions list, hide it otherwise.
                // Tracked off the actual motion events we receive (not gated on
                // `mouse_hover_enabled`, which only governs terminal-level mode
                // 1003 — and is off by default on Windows): if a Moved event
                // arrives, use it. Re-render only on the enter/leave transition
                // (not every motion) so it fades in/out without churn.
                let now_over = self
                    .dock
                    .as_ref()
                    .map(|d| {
                        d.scrollbar_hover_zones.iter().any(|z| {
                            col >= z.x && col < z.x + z.width && row >= z.y && row < z.y + z.height
                        })
                    })
                    .unwrap_or(false);
                if let Some(d) = self.dock.as_mut() {
                    if d.scrollbar_zone_hovered != now_over {
                        d.scrollbar_zone_hovered = now_over;
                        needs_render = true;
                    }
                }
            }
            MouseEventKind::ScrollUp => {
                self.handle_vertical_scroll(col, row, mouse_event.modifiers, -3)?;
                needs_render = true;
            }
            MouseEventKind::ScrollDown => {
                self.handle_vertical_scroll(col, row, mouse_event.modifiers, 3)?;
                needs_render = true;
            }
            MouseEventKind::ScrollLeft => {
                // Native horizontal scroll left
                self.active_window_mut()
                    .handle_horizontal_scroll(col, row, -3)?;
                needs_render = true;
            }
            MouseEventKind::ScrollRight => {
                // Native horizontal scroll right
                self.active_window_mut()
                    .handle_horizontal_scroll(col, row, 3)?;
                needs_render = true;
            }
            MouseEventKind::Down(MouseButton::Right) => {
                // Mouse-modal overlay: swallow right-click / Ctrl+right-click
                // so neither the tab context menu nor the theme-info popup
                // fires, and the buffer below is untouched.
                if self.overlay_prompt_active() {
                    needs_render = true;
                } else if mouse_event
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::CONTROL)
                {
                    // Ctrl+Right-Click → theme info popup
                    self.show_theme_info_popup(col, row)?;
                    needs_render = true;
                } else {
                    // Normal right-click → tab context menu
                    self.handle_right_click(col, row)?;
                    needs_render = true;
                }
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

        self.active_window_mut().mouse_state.last_position = Some((col, row));
        Ok(needs_render)
    }

    /// Detect double/triple clicks and update click-tracking state.
    fn detect_multi_click(
        &mut self,
        mouse_event: &crossterm::event::MouseEvent,
        col: u16,
        row: u16,
    ) -> (bool, bool) {
        use crossterm::event::{MouseButton, MouseEventKind};
        if !matches!(mouse_event.kind, MouseEventKind::Down(MouseButton::Left)) {
            return (false, false);
        }
        let now = self.time_source.now();
        let threshold = std::time::Duration::from_millis(self.config.editor.double_click_time_ms);
        let is_consecutive = if let (Some(prev_time), Some(prev_pos)) = (
            self.active_window_mut().previous_click_time,
            self.active_window_mut().previous_click_position,
        ) {
            now.duration_since(prev_time) < threshold && prev_pos == (col, row)
        } else {
            false
        };
        if is_consecutive {
            self.active_window_mut().click_count += 1;
        } else {
            self.active_window_mut().click_count = 1;
        }
        self.active_window_mut().previous_click_time = Some(now);
        self.active_window_mut().previous_click_position = Some((col, row));
        let is_triple = self.active_window_mut().click_count >= 3;
        let is_double = self.active_window_mut().click_count == 2;
        if is_triple {
            self.active_window_mut().click_count = 0;
            self.active_window_mut().previous_click_time = None;
            self.active_window_mut().previous_click_position = None;
        }
        (is_double, is_triple)
    }

    /// Dispatch a vertical scroll event (ScrollUp/ScrollDown) through the priority chain:
    /// Shift → horizontal scroll, prompt, file browser, popup, editor/terminal.
    fn handle_vertical_scroll(
        &mut self,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
        delta: i32,
    ) -> AnyhowResult<()> {
        if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
            self.active_window_mut()
                .handle_horizontal_scroll(col, row, delta)?;
        } else if self.handle_overlay_prompt_scroll(col, row, delta) {
            // Floating-overlay prompt (Live Grep): the wheel scrolls the pane
            // under the pointer — the preview when over it, otherwise the
            // result list (without moving the selection). See issue #2119.
        } else if self.handle_prompt_scroll(delta) {
            // bottom-anchored prompt consumed the scroll (moves selection)
        } else if self.is_file_open_active()
            && self.is_mouse_over_file_browser(col, row)
            && self.handle_file_open_scroll(delta)
        {
            // file browser consumed the scroll
        } else if self.is_mouse_over_any_popup(col, row) {
            self.scroll_popup(delta);
        } else if self.floating_widget_panel.is_some() {
            // A centered modal (orchestrator picker, New-Session form,
            // ...) is modal and takes precedence: scroll it when the
            // pointer is over it, otherwise swallow the wheel so it
            // can't leak through to the buffer (or the dock) behind it.
            // Either way the event is consumed here — never falls through.
            self.handle_floating_widget_panel_wheel(super::PanelSlot::Floating, col, row, delta);
        } else if self.dock.is_some()
            && self.handle_floating_widget_panel_wheel(super::PanelSlot::Dock, col, row, delta)
        {
            // The dock swallows the wheel whenever the pointer is over
            // its column (never leaks to the window beneath).
        } else if self
            .active_window()
            .split_at_position(col, row)
            .map(|(_, buffer_id)| self.handle_widget_panel_wheel(buffer_id, delta))
            .unwrap_or(false)
        {
            // a mounted widget panel consumed the scroll
        } else {
            if self.active_window().terminal_mode
                && self
                    .active_window()
                    .is_terminal_buffer(self.active_buffer())
            {
                // Scrolling up drops the terminal to read-only scrollback:
                // remember that mode so re-focusing keeps it in scrollback.
                let __b = self.active_buffer();
                self.active_window_mut().set_terminal_interaction_mode(
                    __b,
                    crate::app::window::TerminalInteractionMode::Scrollback,
                );
                self.active_window_mut().sync_terminal_to_buffer(__b);
                self.active_window_mut().terminal_mode = false;
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
            }
            self.dismiss_transient_popups();
            self.active_window_mut()
                .handle_mouse_scroll(col, row, delta)?;
        }
        Ok(())
    }

    /// Route a wheel event inside the floating-overlay prompt (Live Grep).
    ///
    /// The overlay is mouse-modal, so it always consumes the wheel (returns
    /// true) when active — the event must never leak to the buffer below.
    /// * Over the preview pane → scroll the preview.
    /// * Anywhere else (result list, input, toolbar, frame) → scroll the
    ///   result list *without* moving the selection.
    ///
    /// Bottom-anchored prompts (command palette, file finder) are left to
    /// `handle_prompt_scroll`, which keeps their wheel-moves-selection UX.
    fn handle_overlay_prompt_scroll(&mut self, col: u16, row: u16, delta: i32) -> bool {
        if !self.overlay_prompt_active() {
            return false;
        }
        let preview_area = self.active_chrome().prompt_preview_area;
        let results_visible = self
            .active_chrome()
            .prompt_results_area
            .map(|r| r.height as usize)
            .unwrap_or(0);
        if let Some(preview) = preview_area {
            if in_rect(col, row, preview) {
                self.active_window_mut()
                    .scroll_overlay_preview_by_lines(delta);
                return true;
            }
        }
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.scroll_results(delta, results_visible);
        }
        true
    }

    /// Update the current hover target based on mouse position
    /// Returns true if the hover target changed (requiring a re-render)
    pub(super) fn update_hover_target(&mut self, col: u16, row: u16) -> bool {
        let old_target = self.active_window_mut().mouse_state.hover_target.clone();
        let new_target = self.compute_hover_target(col, row);
        let changed = old_target != new_target;
        self.active_window_mut().mouse_state.hover_target = new_target.clone();

        // If a menu is currently open and we're hovering over a different menu bar item,
        // switch to that menu automatically
        if let Some(active_menu_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();
            if let Some(HoverTarget::MenuBarItem(hovered_menu_idx)) = new_target.clone() {
                if hovered_menu_idx != active_menu_idx {
                    self.menu_state.open_menu(hovered_menu_idx);
                    return true; // Force re-render since menu changed
                }
            }

            // If hovering over a menu dropdown item, check if it's a submenu and open it
            if let Some(HoverTarget::MenuDropdownItem(_, item_idx)) = new_target.clone() {
                // If this item is the parent of the currently open submenu, keep it open.
                // This prevents blinking when hovering over the parent item of an open submenu.
                if self.menu_state.submenu_path.first() == Some(&item_idx) {
                    tracing::trace!(
                        "menu hover: staying on submenu parent item_idx={}, submenu_path={:?}",
                        item_idx,
                        self.menu_state.submenu_path
                    );
                    return changed;
                }

                // Clear any open submenus since we're at a different item in the main dropdown
                if !self.menu_state.submenu_path.is_empty() {
                    tracing::trace!(
                        "menu hover: clearing submenu_path={:?} for different item_idx={}",
                        self.menu_state.submenu_path,
                        item_idx
                    );
                    self.menu_state.submenu_path.clear();
                    self.menu_state.highlighted_item = Some(item_idx);
                    return true;
                }

                // Check if the hovered item is a submenu
                if let Some(menu) = all_menus.get(active_menu_idx) {
                    if let Some(crate::config::MenuItem::Submenu { items, .. }) =
                        menu.items.get(item_idx)
                    {
                        if !items.is_empty() {
                            tracing::trace!("menu hover: opening submenu at item_idx={}", item_idx);
                            self.menu_state.submenu_path.push(item_idx);
                            self.menu_state.highlighted_item = Some(0);
                            return true;
                        }
                    }
                }
                // Update highlighted item for non-submenu items too
                if self.menu_state.highlighted_item != Some(item_idx) {
                    self.menu_state.highlighted_item = Some(item_idx);
                    return true;
                }
            }

            // If hovering over a submenu item, handle submenu navigation
            if let Some(HoverTarget::SubmenuItem(depth, item_idx)) = new_target {
                // If this item is the parent of a currently open nested submenu, keep it open.
                // This prevents blinking when hovering over the parent item of an open nested submenu.
                // submenu_path[depth] stores the index of the nested submenu opened from this level.
                if self.menu_state.submenu_path.len() > depth
                    && self.menu_state.submenu_path.get(depth) == Some(&item_idx)
                {
                    tracing::trace!(
                        "menu hover: staying on nested submenu parent depth={}, item_idx={}, submenu_path={:?}",
                        depth,
                        item_idx,
                        self.menu_state.submenu_path
                    );
                    return changed;
                }

                // Truncate submenu path to this depth (close any deeper submenus)
                if self.menu_state.submenu_path.len() > depth {
                    tracing::trace!(
                        "menu hover: truncating submenu_path={:?} to depth={} for item_idx={}",
                        self.menu_state.submenu_path,
                        depth,
                        item_idx
                    );
                    self.menu_state.submenu_path.truncate(depth);
                }

                // Get the items at this depth
                if let Some(items) = self
                    .menu_state
                    .get_current_items(&all_menus, active_menu_idx)
                {
                    // Check if hovered item is a submenu - if so, open it
                    if let Some(crate::config::MenuItem::Submenu {
                        items: sub_items, ..
                    }) = items.get(item_idx)
                    {
                        if !sub_items.is_empty()
                            && !self.menu_state.submenu_path.contains(&item_idx)
                        {
                            tracing::trace!(
                                "menu hover: opening nested submenu at depth={}, item_idx={}",
                                depth,
                                item_idx
                            );
                            self.menu_state.submenu_path.push(item_idx);
                            self.menu_state.highlighted_item = Some(0);
                            return true;
                        }
                    }
                    // Update highlighted item
                    if self.menu_state.highlighted_item != Some(item_idx) {
                        self.menu_state.highlighted_item = Some(item_idx);
                        return true;
                    }
                }
            }
        }

        // Handle tab context menu hover - update highlighted item
        if let Some(HoverTarget::TabContextMenuItem(item_idx)) = new_target.clone() {
            if let Some(ref mut menu) = self.active_window_mut().tab_context_menu {
                if menu.highlighted != item_idx {
                    menu.highlighted = item_idx;
                    return true;
                }
            }
        }

        if let Some(&HoverTarget::FileExplorerContextMenuItem(item_idx)) = new_target.as_ref() {
            if let Some(ref mut menu) = self.active_window_mut().file_explorer_context_menu {
                if menu.highlighted != item_idx {
                    menu.highlighted = item_idx;
                    return true;
                }
            }
        }

        // Handle "+" new-tab popup menu hover - update highlighted item
        if let Some(HoverTarget::NewTabMenuItem(item_idx)) = new_target.clone() {
            if let Some(ref mut menu) = self.active_window_mut().new_tab_menu {
                if menu.highlighted != item_idx {
                    menu.highlighted = item_idx;
                    return true;
                }
            }
        }

        // Handle file explorer status indicator hover - show tooltip
        // Always dismiss existing tooltip first when target changes
        if old_target != new_target
            && matches!(
                old_target,
                Some(HoverTarget::FileExplorerStatusIndicator(_))
            )
        {
            self.dismiss_file_explorer_status_tooltip();
        }

        if let Some(HoverTarget::FileExplorerStatusIndicator(ref path)) = new_target {
            // Only show tooltip if this is a new hover (not already showing for this path)
            if old_target != new_target {
                self.show_file_explorer_status_tooltip(path.clone(), col, row);
                return true;
            }
        }

        changed
    }

    /// Update LSP hover state based on mouse position
    /// Tracks position for debounced hover requests
    ///
    /// Hover popup stays visible when:
    /// - Mouse is over the hover popup itself
    /// - Mouse is within the hovered symbol range
    ///
    /// Hover is dismissed when mouse leaves the editor area entirely.
    fn update_lsp_hover_state(&mut self, col: u16, row: u16) {
        tracing::trace!(col, row, "update_lsp_hover_state: raw mouse position");

        // Suppress LSP hover when a popup is already visible (e.g. theme info popup,
        // tab context menu, or the status-bar LSP status popup) to avoid hover
        // tooltips overlapping other popups.
        if self.active_window_mut().theme_info_popup.is_some()
            || self.active_window_mut().tab_context_menu.is_some()
            || self.active_window_mut().new_tab_menu.is_some()
            || self
                .active_window_mut()
                .file_explorer_context_menu
                .is_some()
            || self.is_lsp_status_popup_open()
        {
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        }

        // Check if mouse is over a transient popup - if so, keep hover active
        if self.is_mouse_over_transient_popup(col, row) {
            return;
        }

        // Find which split the mouse is over
        let split_info = self
            .active_layout()
            .split_areas
            .iter()
            .find(|(_, _, content_rect, _, _, _)| in_rect(col, row, *content_rect))
            .map(|(split_id, buffer_id, content_rect, _, _, _)| {
                (*split_id, *buffer_id, *content_rect)
            });

        let Some((split_id, buffer_id, content_rect)) = split_info else {
            // Mouse is not over editor content - clear hover state and dismiss popup
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        };

        // Get cached mappings and gutter width for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();
        let gutter_width = self
            .buffers()
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);
        let fallback = self
            .buffers()
            .get(&buffer_id)
            .map(|s| s.buffer.len())
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .and_then(|vs| vs.compose_width);

        // Convert screen position to buffer byte position
        let Some(byte_pos) = super::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            false, // Don't include gutter
            compose_width,
        ) else {
            // Mouse is in the gutter — stop tracking a pending request but keep
            // any existing popup visible. The popup is only dismissed when the
            // mouse leaves the editor area entirely (see docstring).
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
            }
            return;
        };

        // Check if mouse is past the end of line content - don't trigger hover for empty space
        let content_col = col.saturating_sub(content_rect.x);
        let text_col = content_col.saturating_sub(gutter_width) as usize;
        let visual_row = row.saturating_sub(content_rect.y) as usize;

        let line_info = cached_mappings
            .as_ref()
            .and_then(|mappings| mappings.get(visual_row))
            .map(|line_mapping| {
                (
                    line_mapping.visual_to_char.len(),
                    line_mapping.line_end_byte,
                )
            });

        let is_past_line_end_or_empty = line_info
            .map(|(line_len, _)| {
                // Empty lines (just newline) should not trigger hover
                if line_len <= 1 {
                    return true;
                }
                text_col >= line_len
            })
            // If mouse is below all mapped lines (no mapping), don't trigger hover
            .unwrap_or(true);

        tracing::trace!(
            col,
            row,
            content_col,
            text_col,
            visual_row,
            gutter_width,
            byte_pos,
            ?line_info,
            is_past_line_end_or_empty,
            "update_lsp_hover_state: position check"
        );

        if is_past_line_end_or_empty {
            tracing::trace!(
                "update_lsp_hover_state: mouse past line end or empty line, clearing hover"
            );
            // Mouse is past end of line content — stop tracking a pending
            // request but keep any existing popup visible. The popup is only
            // dismissed when the mouse leaves the editor area entirely
            // (see docstring).
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
            }
            return;
        }

        // Check if mouse is within the hovered symbol range - if so, keep hover active
        if let Some((start, end)) = self.active_window_mut().hover.symbol_range() {
            if byte_pos >= start && byte_pos < end {
                // Mouse is still over the hovered symbol - keep hover state
                return;
            }
        }

        // Check if we're still hovering the same position
        if let Some((old_pos, _, _, _)) = self.active_window_mut().mouse_state.lsp_hover_state {
            if old_pos == byte_pos {
                // Same position - keep existing state
                return;
            }
            // Position changed outside the hovered symbol range. Don't dismiss
            // the popup here: a new hover request will fire after the debounce
            // and replace the popup naturally if the mouse settles on another
            // symbol. Dismissing eagerly tore the popup down whenever the
            // mouse passed through whitespace between two words (issue #692).
        }

        // Start tracking new hover position
        self.active_window_mut().mouse_state.lsp_hover_state =
            Some((byte_pos, std::time::Instant::now(), col, row));
        self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
    }

    /// Check if mouse position is over a transient popup (hover, signature help)
    fn is_mouse_over_transient_popup(&self, col: u16, row: u16) -> bool {
        let layouts = popup_areas_to_layout_info(&self.active_chrome().popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_transient_popup(col, row)
    }

    /// Check if mouse position is over any popup (including non-transient ones like completion)
    fn is_mouse_over_any_popup(&self, col: u16, row: u16) -> bool {
        // Editor-level popup overlays absorb every click within their outer
        // rect so the buffer below doesn't receive a stray cursor placement.
        for (_, popup_area, _, _, _) in &self.active_chrome().global_popup_areas {
            if in_rect(col, row, *popup_area) {
                return true;
            }
        }
        // The prompt's suggestions popup also absorbs clicks across its full
        // outer rect (border + items): clicking the chrome must not move the
        // buffer cursor below.
        if let Some(outer) = self.active_chrome().suggestions_outer_area {
            if in_rect(col, row, outer) {
                return true;
            }
        }
        let layouts = popup_areas_to_layout_info(&self.active_chrome().popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_popup(col, row)
    }

    /// Check if mouse position is over the file browser popup
    fn is_mouse_over_file_browser(&self, col: u16, row: u16) -> bool {
        self.active_window()
            .file_browser_layout
            .as_ref()
            .is_some_and(|layout| layout.contains(col, row))
    }

    // `split_at_position` lives on `impl Window` — call it via
    // `self.active_window().split_at_position(col, row)`.

    /// Compute what hover target is at the given position
    fn compute_hover_target(&self, col: u16, row: u16) -> Option<HoverTarget> {
        self.hover_target_in_floating_overlays(col, row)
            .or_else(|| self.hover_target_in_chrome(col, row))
    }

    /// Hit-test floating overlay layers: context menus, command palette,
    /// popup lists, and the file-browser dialog. These always render on
    /// top of the chrome and must be checked first.
    fn hover_target_in_floating_overlays(&self, col: u16, row: u16) -> Option<HoverTarget> {
        if let Some(ref menu) = self.active_window().file_explorer_context_menu {
            let (menu_x, menu_y) = menu.clamped_position(
                self.active_chrome().last_frame.width,
                self.active_chrome().last_frame.height,
            );
            let menu_width = super::types::FILE_EXPLORER_CONTEXT_MENU_WIDTH;
            let menu_height = menu.height();

            if col >= menu_x
                && col < menu_x + menu_width
                && row > menu_y
                && row < menu_y + menu_height - 1
            {
                let item_idx = (row - menu_y - 1) as usize;
                if item_idx < menu.items().len() {
                    return Some(HoverTarget::FileExplorerContextMenuItem(item_idx));
                }
            }
        }

        // Check the "+" new-tab popup menu (rendered on top)
        if let Some(ref menu) = self.active_window().new_tab_menu {
            let menu_x = menu.position.0;
            let menu_y = menu.position.1;
            let menu_width = super::types::NEW_TAB_MENU_WIDTH;
            let items = super::types::NewTabMenuItem::all();
            let menu_height = items.len() as u16 + 2;

            if col >= menu_x
                && col < menu_x + menu_width
                && row > menu_y
                && row < menu_y + menu_height - 1
            {
                let item_idx = (row - menu_y - 1) as usize;
                if item_idx < items.len() {
                    return Some(HoverTarget::NewTabMenuItem(item_idx));
                }
            }
        }

        // Check tab context menu first (it's rendered on top)
        if let Some(ref menu) = self.active_window().tab_context_menu {
            let menu_x = menu.position.0;
            let menu_y = menu.position.1;
            let menu_width = 22u16;
            let items = super::types::TabContextMenuItem::all();
            let menu_height = items.len() as u16 + 2;

            if col >= menu_x
                && col < menu_x + menu_width
                && row > menu_y
                && row < menu_y + menu_height - 1
            {
                let item_idx = (row - menu_y - 1) as usize;
                if item_idx < items.len() {
                    return Some(HoverTarget::TabContextMenuItem(item_idx));
                }
            }
        }

        // Check suggestions area first (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.active_chrome().suggestions_area
        {
            if in_rect(col, row, *inner_rect) {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    return Some(HoverTarget::SuggestionItem(item_idx));
                }
            }
        }

        // Check popups (they're rendered on top)
        // Check from top to bottom (reverse order since last popup is on top)
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            self.active_chrome().popup_areas.iter().rev()
        {
            if in_rect(col, row, *inner_rect) && *num_items > 0 {
                // Calculate which item is being hovered
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;

                if item_idx < *num_items {
                    return Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
                }
            }
        }

        // Check file browser popup
        if self.is_file_open_active() {
            if let Some(hover) = self.compute_file_browser_hover(col, row) {
                return Some(hover);
            }
        }

        None
    }

    /// Hit-test the permanent chrome: menu bar, file explorer panel,
    /// split separators, tabs, scrollbars, status bar, and search
    /// options. Called only after floating overlays have been ruled out.
    fn hover_target_in_chrome(&self, col: u16, row: u16) -> Option<HoverTarget> {
        // Check menu bar (row 0, only when visible)
        // Check menu bar using cached layout from previous render
        if self.active_window().menu_bar_visible {
            if let Some(ref menu_layout) = self.active_chrome().menu_layout {
                if let Some(menu_idx) = menu_layout.menu_at(col, row) {
                    return Some(HoverTarget::MenuBarItem(menu_idx));
                }
            }
        }

        // Check menu dropdown items if a menu is open (including submenus)
        if let Some(active_idx) = self.menu_state.active_menu {
            if let Some(hover) = self.compute_menu_dropdown_hover(col, row, active_idx) {
                return Some(hover);
            }
        }

        // Check file explorer close button and border (for resize)
        if let Some(explorer_area) = self.active_layout().file_explorer_area {
            // Close button is at position: explorer_area.x + explorer_area.width - 3 to -1
            let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
            if row == explorer_area.y
                && col >= close_button_x
                && col < explorer_area.x + explorer_area.width
            {
                return Some(HoverTarget::FileExplorerCloseButton);
            }

            // Check if hovering over a status indicator in the file explorer content area
            let content_start_y = explorer_area.y + 1; // +1 for title bar
            let content_end_y = explorer_area.y + explorer_area.height.saturating_sub(1); // -1 for bottom border
            let content_width = explorer_area.width.saturating_sub(3) as usize;

            if row >= content_start_y && row < content_end_y {
                // Determine which item is at this row
                if let Some(explorer) = self.file_explorer().as_ref() {
                    let relative_row = row.saturating_sub(content_start_y) as usize;
                    let scroll_offset = explorer.get_scroll_offset();
                    let item_index = relative_row + scroll_offset;
                    let display_nodes = explorer.get_display_nodes();

                    if item_index < display_nodes.len() {
                        let (node_id, indent) = display_nodes[item_index];
                        if let Some(node) = explorer.tree().get_node(node_id) {
                            let theme = self.theme.read().unwrap();
                            let neutral_fg = if node
                                .entry
                                .metadata
                                .as_ref()
                                .map(|m| m.is_hidden)
                                .unwrap_or(false)
                            {
                                theme.line_number_fg
                            } else if node.entry.is_symlink() {
                                theme.syntax_type
                            } else if node.is_dir() {
                                theme.syntax_keyword
                            } else {
                                theme.editor_fg
                            };
                            let slot_resolver = self.file_explorer_slot_resolver();
                            let slot_context = crate::view::file_tree::ExplorerSlotContext {
                                path: &node.entry.path,
                                is_dir: node.is_dir(),
                                has_unsaved: self.file_explorer_node_has_unsaved_changes(
                                    &node.entry.path,
                                    node.is_dir(),
                                ),
                                is_symlink: node.entry.is_symlink(),
                                is_hidden: node
                                    .entry
                                    .metadata
                                    .as_ref()
                                    .map(|m| m.is_hidden)
                                    .unwrap_or(false),
                                decorations: &self.active_window().file_explorer_decoration_cache,
                                slot_overrides: &self
                                    .active_window()
                                    .file_explorer_slot_override_cache,
                                theme: &theme,
                                neutral_fg,
                            };
                            let slot_resolution = slot_resolver.resolve(&slot_context);
                            if let Some((slot_start, slot_end)) = crate::view::ui::file_explorer::FileExplorerRenderer::trailing_slot_screen_bounds(
                                crate::view::ui::file_explorer::TrailingSlotBoundsCtx {
                                    view: explorer,
                                    node_id,
                                    indent,
                                    content_width,
                                    slot_resolution: &slot_resolution,
                                    tree_indicator_collapsed: &self.config.file_explorer.tree_indicator_collapsed,
                                    tree_indicator_expanded: &self.config.file_explorer.tree_indicator_expanded,
                                    explorer_area,
                                },
                            ) {
                                if col >= slot_start && col < slot_end {
                                    return Some(HoverTarget::FileExplorerStatusIndicator(
                                        node.entry.path.clone(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            // The border is at the rightmost column of the file explorer area
            // (the drawn border character), not one past it.
            let border_x = explorer_area.x + explorer_area.width.saturating_sub(1);
            if col == border_x
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                return Some(HoverTarget::FileExplorerBorder);
            }
        }

        // Check split separators
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.active_layout().separator_areas
        {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                return Some(HoverTarget::SplitSeparator(*split_id, *direction));
            }
        }

        // Check tab areas using cached hit regions (computed during rendering)
        // Check split control buttons first (they're on top of the tab row)
        for (split_id, btn_row, start_col, end_col) in &self.active_layout().close_split_areas {
            if row == *btn_row && col >= *start_col && col < *end_col {
                return Some(HoverTarget::CloseSplitButton(*split_id));
            }
        }

        for (split_id, btn_row, start_col, end_col) in &self.active_layout().maximize_split_areas {
            if row == *btn_row && col >= *start_col && col < *end_col {
                return Some(HoverTarget::MaximizeSplitButton(*split_id));
            }
        }

        for (split_id, tab_layout) in &self.active_layout().tab_layouts {
            match tab_layout.hit_test(col, row) {
                Some(TabHit::CloseButton(target)) => {
                    return Some(HoverTarget::TabCloseButton(target, *split_id));
                }
                Some(TabHit::TabName(target)) => {
                    return Some(HoverTarget::TabName(target, *split_id));
                }
                Some(TabHit::ScrollLeft)
                | Some(TabHit::ScrollRight)
                | Some(TabHit::BarBackground)
                | Some(TabHit::NewTabButton)
                | None => {}
            }
        }

        // Check scrollbars
        for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
            &self.active_layout().split_areas
        {
            if in_rect(col, row, *scrollbar_rect) {
                let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;

                if is_on_thumb {
                    return Some(HoverTarget::ScrollbarThumb(*split_id));
                } else {
                    return Some(HoverTarget::ScrollbarTrack(*split_id, relative_row as u16));
                }
            }
        }

        // Check status bar indicators — one generic hit-test over every
        // clickable segment recorded last frame (encoding, LSP, remote, …).
        if let Some((status_row, _status_x, _status_width)) = self.active_chrome().status_bar.area {
            if row == status_row {
                for (id, indicator_row, start, end) in &self.active_chrome().status_bar.clickable {
                    if row == *indicator_row && col >= *start && col < *end {
                        return Some(HoverTarget::StatusBarClickable(*id));
                    }
                }
            }
        }

        // Check search options bar checkboxes
        if let Some(ref layout) = self.active_chrome().search_options_layout {
            use crate::view::ui::status_bar::SearchOptionsHover;
            if let Some(hover) = layout.checkbox_at(col, row) {
                return Some(match hover {
                    SearchOptionsHover::CaseSensitive => HoverTarget::SearchOptionCaseSensitive,
                    SearchOptionsHover::WholeWord => HoverTarget::SearchOptionWholeWord,
                    SearchOptionsHover::Regex => HoverTarget::SearchOptionRegex,
                    SearchOptionsHover::ConfirmEach => HoverTarget::SearchOptionConfirmEach,
                    SearchOptionsHover::None => return None,
                });
            }
        }

        None
    }

    /// Handle mouse double click (down event)
    /// Double-click in editor area selects the word under the cursor.
    pub(super) fn handle_mouse_double_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        tracing::debug!("handle_mouse_double_click at col={}, row={}", col, row);

        // Double-click on a suggestion item commits the choice — even for
        // prompts whose first click only previews. The first click already
        // selected the row; the second confirms (#1660).
        if let Some(r) = self.handle_click_suggestions_confirm(col, row) {
            return r;
        }

        // Mouse-modal overlay: swallow any double-click that wasn't on a
        // result row so it can't reach (and word-select in) the buffer below.
        if self.overlay_prompt_active() {
            return Ok(());
        }

        // Handle popups: dismiss if clicking outside, block if clicking inside
        if self.is_mouse_over_any_popup(col, row) {
            // Double-click inside popup - block from reaching editor
            return Ok(());
        } else {
            // Double-click outside popup - dismiss transient popups
            self.dismiss_transient_popups();
        }

        // Is it in the file open dialog?
        if self.handle_file_open_double_click(col, row) {
            return Ok(());
        }

        // Is it in the file explorer? Double-click opens file AND focuses editor
        if let Some(explorer_area) = self.active_layout().file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row > explorer_area.y // Skip title bar
                && row < explorer_area.y + explorer_area.height
            {
                // Open file and focus editor (via file_explorer_open_file which calls focus_editor)
                self.file_explorer_open_file()?;
                return Ok(());
            }
        }

        // Find which split/buffer was clicked and handle double-click
        let split_areas = self.active_layout().split_areas.clone();
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &split_areas
        {
            if in_rect(col, row, *content_rect) {
                // Double-clicked on an editor split
                if self.active_window().is_terminal_buffer(*buffer_id) {
                    self.active_window_mut().key_context =
                        crate::input::keybindings::KeyContext::Terminal;
                    // Don't select word in terminal buffers
                    return Ok(());
                }

                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;

                // Position cursor at click location and select word
                self.handle_editor_double_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Handle double-click in editor content area - selects the word under cursor
    fn handle_editor_double_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        use crate::model::event::Event;

        // Fixed panels (toolbars, headers) are inert — no click focus,
        // no selection. Scrollable group panels still accept clicks even
        // when their cursor is hidden.
        if self.active_window().is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Focus this split
        self.focus_split(split_id, buffer_id);

        // Get cached view line mappings for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let leaf_id = split_id;
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

        // Pull the bits we need out of the active window separately;
        // the per-step helper methods (`apply_event_to_buffer` etc.)
        // hide the disjoint sub-field borrowing.
        let gutter_width = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);

        let Some(target_position) = super::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            true, // Allow gutter clicks
            compose_width,
        ) else {
            return Ok(());
        };

        let primary_cursor_id = self
            .active_window()
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&leaf_id))
            .map(|vs| vs.cursors.primary_id())
            .unwrap_or(CursorId(0));
        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position: 0,
            new_position: target_position,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        };

        if let Some(event_log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            event_log.append(event.clone());
        }
        self.active_window_mut()
            .apply_event_to_buffer(buffer_id, leaf_id, &event);

        // Now select the word under cursor
        self.handle_action(Action::SelectWord)?;

        // Set up drag state so subsequent drag events extend selection word-by-word
        if let Some(cursor) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.cursors.primary())
        {
            // Store both edges of the selected word so we can use the appropriate
            // anchor when dragging forward (use word start) vs backward (use word end).
            let sel_start = cursor.selection_start();
            let sel_end = cursor.selection_end();
            self.active_window_mut().mouse_state.dragging_text_selection = true;
            self.active_window_mut().mouse_state.drag_selection_split = Some(split_id);
            self.active_window_mut().mouse_state.drag_selection_anchor = Some(sel_start);
            self.active_window_mut().mouse_state.drag_selection_by_words = true;
            self.active_window_mut().mouse_state.drag_selection_word_end = Some(sel_end);
        }

        Ok(())
    }
    /// Handle mouse triple click (down event)
    /// Triple-click in editor area selects the entire line under the cursor.
    pub(super) fn handle_mouse_triple_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        tracing::debug!("handle_mouse_triple_click at col={}, row={}", col, row);

        // Mouse-modal overlay: never let a triple-click line-select in the
        // buffer below the overlay.
        if self.overlay_prompt_active() {
            return Ok(());
        }

        // Handle popups: dismiss if clicking outside, block if clicking inside
        if self.is_mouse_over_any_popup(col, row) {
            return Ok(());
        } else {
            self.dismiss_transient_popups();
        }

        // Find which split/buffer was clicked
        let split_areas = self.active_layout().split_areas.clone();
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &split_areas
        {
            if in_rect(col, row, *content_rect) {
                if self.active_window().is_terminal_buffer(*buffer_id) {
                    return Ok(());
                }

                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;

                // Use the same pattern as handle_editor_double_click:
                // first focus and position cursor, then select line
                self.handle_editor_triple_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Handle triple-click in editor content area - selects the entire line under cursor
    fn handle_editor_triple_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        use crate::model::event::Event;

        if self.active_window().is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Focus this split
        self.focus_split(split_id, buffer_id);

        // Get cached view line mappings for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        let leaf_id = split_id;
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

        // Pull the bits we need out of the active window separately;
        // the per-step helper methods (`apply_event_to_buffer` etc.)
        // hide the disjoint sub-field borrowing.
        let gutter_width = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);

        let Some(target_position) = super::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            true,
            compose_width,
        ) else {
            return Ok(());
        };

        let primary_cursor_id = self
            .active_window()
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&leaf_id))
            .map(|vs| vs.cursors.primary_id())
            .unwrap_or(CursorId(0));
        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position: 0,
            new_position: target_position,
            old_anchor: None,
            new_anchor: None,
            old_sticky_column: None,
            new_sticky_column: None,
        };

        if let Some(event_log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            event_log.append(event.clone());
        }
        self.active_window_mut()
            .apply_event_to_buffer(buffer_id, leaf_id, &event);

        // Now select the entire line
        self.handle_action(Action::SelectLine)?;

        Ok(())
    }

    /// Handle mouse click (down event)
    /// True while a floating-overlay prompt (e.g. Live Grep / Universal
    /// Search) owns the screen. Such overlays are **mouse-modal**: their own
    /// targets (result list, scrollbar, and — once wired — toolbar controls)
    /// are handled, but every other click is swallowed so it never lands in
    /// the buffer below and moves its cursor. Bottom-anchored (non-overlay)
    /// prompts are unaffected.
    pub(super) fn overlay_prompt_active(&self) -> bool {
        self.active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.overlay)
    }

    pub(super) fn handle_mouse_click(
        &mut self,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        // A centered modal takes click precedence over the dock: while
        // the New-Session form is up over the dock, clicks hit-test the
        // modal (clicks outside it are swallowed — it has Cancel / Esc).
        if self.floating_widget_panel.is_some() {
            self.handle_floating_widget_click(super::PanelSlot::Floating, col, row);
            return Ok(());
        }
        // Dock resize: a press on the dock's right border (its rightmost
        // column) starts a drag that resizes the dock width. Checked
        // before the click-routing below so the border column is a
        // resize handle, not a widget hit.
        if let Some(super::PanelPlacement::LeftDock { width_cols }) =
            self.dock.as_ref().map(|f| f.placement)
        {
            if col == width_cols.saturating_sub(1) {
                self.dock_resizing = true;
                return Ok(());
            }
        }
        // Dock click routing (non-modal): clicks inside its column
        // hit-test (and re-focus it if blurred); clicks in the editor
        // blur the dock and fall through to normal editor handling.
        if let Some((super::PanelPlacement::LeftDock { width_cols }, focused)) =
            self.dock.as_ref().map(|f| (f.placement, f.focused))
        {
            if col < width_cols {
                tracing::debug!(
                    target: "fresh::dock",
                    col,
                    row,
                    width_cols,
                    focused,
                    "handle_mouse_click: click in dock column"
                );
                if !focused {
                    // Symmetric with `blur_floating_panel`: the un-blur
                    // must notify the plugin via a `focus` widget_event
                    // so any mirror of dock-focus state updates before
                    // the click's row-select event fires its scheduling
                    // logic. Without this, the orchestrator's
                    // `dockBlurred` mirror stayed `true` and its
                    // debounced live-switch aborted on the first
                    // un-dive click.
                    self.refocus_floating_panel(super::PanelSlot::Dock);
                }
                self.handle_floating_widget_click(super::PanelSlot::Dock, col, row);
                return Ok(());
            }
            if focused {
                tracing::debug!(
                    target: "fresh::dock",
                    col,
                    row,
                    width_cols,
                    "handle_mouse_click: click outside dock — blurring"
                );
                self.blur_floating_panel(super::PanelSlot::Dock);
            }
        }
        if let Some(r) = self.handle_click_context_menus(col, row) {
            return r;
        }
        if !self.is_mouse_over_any_popup(col, row) {
            self.dismiss_transient_popups();
        }
        if let Some(r) = self.handle_click_suggestions(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_prompt_scrollbar(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_popup_scrollbar(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_global_popups(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_buffer_popups(col, row) {
            return r;
        }
        if self.is_mouse_over_any_popup(col, row) {
            return Ok(());
        }
        if self.is_file_open_active() && self.handle_file_open_click(col, row) {
            return Ok(());
        }
        if let Some(r) = self.handle_click_menu_bar(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_file_explorer_area(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_scrollbar(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_horizontal_scrollbar(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_status_bar(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_search_options(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_split_separator(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_split_controls(col, row) {
            return r;
        }
        if let Some(r) = self.handle_click_tab_bar(col, row) {
            return r;
        }

        // A floating-overlay prompt is mouse-modal: its own targets (result
        // list, scrollbar) were handled above. A click on a toolbar control
        // toggles it through the host (which emits a widget_event); anything
        // else — the input row, separator, preview pane, empty space, or a
        // click outside the frame — is swallowed here so it never reaches the
        // buffer and moves its cursor.
        if self.overlay_prompt_active() {
            let hit = self
                .active_chrome()
                .prompt_toolbar_hits
                .iter()
                .find(|(_, r)| in_rect(col, row, *r))
                .map(|(k, _)| k.clone());
            if let Some(widget_key) = hit {
                // Move keyboard focus to the clicked control so Tab continues
                // from here, then flip it through the host (which emits a
                // widget_event for the plugin).
                if let Some(p) = self.active_window_mut().prompt.as_mut() {
                    p.toolbar_focus = Some(widget_key.clone());
                }
                self.toggle_overlay_toolbar_widget(&widget_key);
            }
            return Ok(());
        }

        // Check if click is in editor content area
        tracing::debug!(
            "handle_mouse_click: checking {} split_areas for click at ({}, {})",
            self.active_layout().split_areas.len(),
            col,
            row
        );
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.active_layout().split_areas
        {
            tracing::debug!(
                "  split_id={:?}, content_rect=({}, {}, {}x{})",
                split_id,
                content_rect.x,
                content_rect.y,
                content_rect.width,
                content_rect.height
            );
            if in_rect(col, row, *content_rect) {
                // Click in editor - focus split and position cursor
                tracing::debug!("  -> HIT! calling handle_editor_click");
                self.handle_editor_click(
                    col,
                    row,
                    *split_id,
                    *buffer_id,
                    *content_rect,
                    modifiers,
                )?;
                return Ok(());
            }
        }
        tracing::debug!("  -> No split area hit");

        Ok(())
    }

    // ── handle_mouse_click helpers ──────────────────────────────────────────
    // Each returns Some(result) if the click was consumed, None to fall through.

    fn handle_click_context_menus(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        if self
            .active_window_mut()
            .file_explorer_context_menu
            .is_some()
        {
            if let Some(result) = self.handle_file_explorer_context_menu_click(col, row) {
                return Some(result);
            }
        }
        if self.active_window_mut().tab_context_menu.is_some() {
            if let Some(result) = self.handle_tab_context_menu_click(col, row) {
                return Some(result);
            }
        }
        if self.active_window_mut().new_tab_menu.is_some() {
            if let Some(result) = self.handle_new_tab_menu_click(col, row) {
                return Some(result);
            }
        }
        None
    }

    /// Hit-test (col, row) against the suggestions popup. Returns the index
    /// of the suggestion under the click, or `None` if the click is outside
    /// the inner item area or no suggestions are visible.
    fn suggestion_at(&self, col: u16, row: u16) -> Option<usize> {
        let (inner_rect, start_idx, _visible_count, total_count) =
            self.active_chrome().suggestions_area?;
        if col < inner_rect.x
            || col >= inner_rect.x + inner_rect.width
            || row < inner_rect.y
            || row >= inner_rect.y + inner_rect.height
        {
            return None;
        }
        let relative_row = (row - inner_rect.y) as usize;
        let item_idx = start_idx + relative_row;
        if item_idx < total_count {
            Some(item_idx)
        } else {
            None
        }
    }

    fn handle_click_suggestions(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let item_idx = self.suggestion_at(col, row)?;
        let prompt = self.active_window_mut().prompt.as_mut()?;
        prompt.selected_suggestion = Some(item_idx);
        let confirms = prompt.prompt_type.click_confirms();
        if !confirms {
            // Mirror keyboard navigation / scroll: sync the input
            // to the selected suggestion so the prompt reflects
            // what Enter would commit.
            if let Some(suggestion) = prompt.suggestions.get(item_idx) {
                prompt.input = suggestion.get_value().to_string();
                prompt.cursor_pos = prompt.input.len();
            }
        }
        if confirms {
            return Some(self.handle_action(Action::PromptConfirm));
        }
        Some(Ok(()))
    }

    /// Click handler that always commits the suggestion under the cursor,
    /// regardless of `click_confirms`. Used for double-clicks so that
    /// preview-on-click prompts still have a mouse-only commit path.
    fn handle_click_suggestions_confirm(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let item_idx = self.suggestion_at(col, row)?;
        let prompt = self.active_window_mut().prompt.as_mut()?;
        prompt.selected_suggestion = Some(item_idx);
        if let Some(suggestion) = prompt.suggestions.get(item_idx) {
            prompt.input = suggestion.get_value().to_string();
            prompt.cursor_pos = prompt.input.len();
        }
        Some(self.handle_action(Action::PromptConfirm))
    }

    /// Click/drag on the floating-overlay prompt's scrollbar
    /// (issue #1796). Reuses
    /// `view::ui::scrollbar::ScrollbarState::click_to_offset` for
    /// the same math the popup-scrollbar handler uses, so thumb
    /// behaviour is consistent across the editor.
    fn handle_click_prompt_scrollbar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        use crate::view::ui::scrollbar::ScrollbarState;
        let sb_rect = self.active_chrome().suggestions_scrollbar_rect?;
        if col < sb_rect.x
            || col >= sb_rect.x + sb_rect.width
            || row < sb_rect.y
            || row >= sb_rect.y + sb_rect.height
        {
            return None;
        }
        // Read what the renderer drew so the drag math matches what
        // the user sees. `suggestions_area` carries
        // (inner_rect, scroll_start_idx, visible_count, total_count).
        // Snapshot suggestions_area before borrowing the window's
        // prompt — `active_window_mut()` is a method call so the
        // compiler can't see that `prompt` and `chrome_layout` are
        // disjoint sub-fields.
        let suggestions_area_visible = self.active_chrome().suggestions_area.map(|(_, _, v, _)| v);
        let active_window_id = self.active_window;
        let prompt = self
            .windows
            .get_mut(&active_window_id)
            .and_then(|w| w.prompt.as_mut())?;
        let visible = suggestions_area_visible.unwrap_or(prompt.suggestions.len().min(10));
        let total = prompt.suggestions.len();
        let track_height = sb_rect.height as usize;
        let click_row = row.saturating_sub(sb_rect.y) as usize;
        let state = ScrollbarState::new(total, visible, prompt.scroll_offset);
        prompt.scroll_offset = state.click_to_offset(track_height, click_row);
        // Hand off to the drag follow-up so subsequent mouse moves
        // keep tracking the thumb.
        self.active_window_mut()
            .mouse_state
            .dragging_prompt_scrollbar = true;
        Some(Ok(()))
    }

    fn handle_click_popup_scrollbar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        // Collect all needed data before mutating self.
        let scrollbar_info: Option<(usize, i32)> =
            self.active_chrome().popup_areas.iter().rev().find_map(
                |(popup_idx, _popup_rect, inner_rect, _scroll, _n, scrollbar_rect, total_lines)| {
                    let sb_rect = scrollbar_rect.as_ref()?;
                    if col >= sb_rect.x
                        && col < sb_rect.x + sb_rect.width
                        && row >= sb_rect.y
                        && row < sb_rect.y + sb_rect.height
                    {
                        let relative_row = (row - sb_rect.y) as usize;
                        let track_height = sb_rect.height as usize;
                        let visible_lines = inner_rect.height as usize;
                        if track_height > 0 && *total_lines > visible_lines {
                            let max_scroll = total_lines.saturating_sub(visible_lines);
                            let target = if track_height > 1 {
                                (relative_row * max_scroll) / (track_height.saturating_sub(1))
                            } else {
                                0
                            };
                            Some((*popup_idx, target as i32))
                        } else {
                            Some((*popup_idx, 0))
                        }
                    } else {
                        None
                    }
                },
            );
        let (popup_idx, target_scroll) = scrollbar_info?;
        self.active_window_mut()
            .mouse_state
            .dragging_popup_scrollbar = Some(popup_idx);
        self.active_window_mut().mouse_state.drag_start_row = Some(row);
        let current_scroll = self
            .active_state()
            .popups
            .get(popup_idx)
            .map(|p| p.scroll_offset)
            .unwrap_or(0);
        self.active_window_mut().mouse_state.drag_start_popup_scroll = Some(current_scroll);
        let state = self.active_state_mut();
        if let Some(popup) = state.popups.get_mut(popup_idx) {
            popup.scroll_by(target_scroll - current_scroll as i32);
        }
        Some(Ok(()))
    }

    /// Handle every mouse event while the workspace-trust modal is up. Left
    /// clicks act on its controls (radio rows select + confirm; [ OK ] confirms
    /// the current selection; the secondary button cancels or quits); the wheel
    /// scrolls an overflowing dialog. Everything else is absorbed so nothing
    /// reaches the buffer behind the modal.
    fn handle_workspace_trust_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};
        let col = mouse_event.column;
        let row = mouse_event.row;
        let layout = self.active_chrome().workspace_trust_dialog.clone();

        match mouse_event.kind {
            MouseEventKind::ScrollUp => {
                self.workspace_trust_scroll = self.workspace_trust_scroll.saturating_sub(2);
            }
            MouseEventKind::ScrollDown => {
                let max = layout.as_ref().map(|l| l.max_scroll).unwrap_or(0);
                self.workspace_trust_scroll = (self.workspace_trust_scroll + 2).min(max);
            }
            MouseEventKind::Down(MouseButton::Left) => {
                if let Some(layout) = layout {
                    let hit = |r: ratatui::layout::Rect| in_rect(col, row, r);
                    if hit(layout.ok) {
                        let idx = self.current_workspace_trust_selection();
                        self.confirm_workspace_trust(idx);
                    } else if hit(layout.quit) {
                        // Secondary: Cancel (close) when voluntarily opened,
                        // Quit (exit the editor) for the mandatory open-time gate.
                        self.hide_popup();
                        if !self.workspace_trust_prompt_cancellable {
                            self.should_quit = true;
                        }
                    } else if let Some(i) = layout.radios.iter().position(|r| hit(*r)) {
                        self.confirm_workspace_trust(i);
                    }
                    // else: click on the dialog body or dimmed backdrop — absorb.
                }
            }
            // Drag / move / release / right-click / horizontal scroll: absorb.
            _ => {}
        }
        Ok(true)
    }

    fn handle_click_global_popups(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        for (popup_idx, popup_rect, inner_rect, scroll_offset, num_items) in self
            .active_chrome()
            .global_popup_areas
            .clone()
            .into_iter()
            .rev()
        {
            if popup_rect.width >= 5 {
                let cb_x = popup_rect.x + popup_rect.width - 4;
                if row == popup_rect.y && col >= cb_x && col < cb_x + 3 {
                    return Some(self.handle_action(Action::PopupCancel));
                }
            }
            if in_rect(col, row, inner_rect) && num_items > 0 {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;
                if item_idx < num_items {
                    if let Some(popup) = self.global_popups.get_mut(popup_idx) {
                        if let crate::view::popup::PopupContent::List { items: _, selected } =
                            &mut popup.content
                        {
                            *selected = item_idx;
                        }
                    }
                    return Some(self.handle_action(Action::PopupConfirm));
                }
            }
        }
        None
    }

    fn handle_click_buffer_popups(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        // Check close-button overlay ("[×]") on each popup.
        let close_hit = self.active_chrome().popup_areas.iter().rev().find_map(
            |(_idx, popup_rect, _inner, _scroll, _n, _sb, _tl)| {
                if popup_rect.width < 5 {
                    return None;
                }
                let cb_x = popup_rect.x + popup_rect.width - 4;
                if row == popup_rect.y && col >= cb_x && col < cb_x + 3 {
                    Some(())
                } else {
                    None
                }
            },
        );
        if close_hit.is_some() {
            return Some(self.handle_action(Action::PopupCancel));
        }

        // Content area clicks — clone to allow &mut self calls inside the loop.
        let popup_areas = self.active_chrome().popup_areas.clone();
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            popup_areas.iter().rev()
        {
            if !in_rect(col, row, *inner_rect) {
                continue;
            }
            let relative_col = (col - inner_rect.x) as usize;
            let relative_row = (row - inner_rect.y) as usize;

            let link_url = {
                let state = self.active_state();
                state
                    .popups
                    .top()
                    .and_then(|p| p.link_at_position(relative_col, relative_row))
            };
            if let Some(url) = link_url {
                #[cfg(feature = "runtime")]
                if let Err(e) = open::that(&url) {
                    self.set_status_message(format!("Failed to open URL: {}", e));
                } else {
                    self.set_status_message(format!("Opening: {}", url));
                }
                return Some(Ok(()));
            }

            if *num_items > 0 {
                let item_idx = scroll_offset + relative_row;
                if item_idx < *num_items {
                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.top_mut() {
                        if let crate::view::popup::PopupContent::List { items: _, selected } =
                            &mut popup.content
                        {
                            *selected = item_idx;
                        }
                    }
                    return Some(self.handle_action(Action::PopupConfirm));
                }
            }

            let is_text_popup = {
                let state = self.active_state();
                state.popups.top().is_some_and(|p| {
                    matches!(
                        p.content,
                        crate::view::popup::PopupContent::Text(_)
                            | crate::view::popup::PopupContent::Markdown(_)
                    )
                })
            };
            if is_text_popup {
                let line = scroll_offset + relative_row;
                let popup_idx_copy = *popup_idx;
                let state = self.active_state_mut();
                if let Some(popup) = state.popups.top_mut() {
                    popup.start_selection(line, relative_col);
                }
                self.active_window_mut().mouse_state.selecting_in_popup = Some(popup_idx_copy);
                return Some(Ok(()));
            }
        }
        None
    }

    fn handle_click_menu_bar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        if self.active_window_mut().menu_bar_visible {
            // Resolve the hit before any &mut operations to avoid borrow conflicts.
            let hit = self
                .active_chrome()
                .menu_layout
                .as_ref()
                .and_then(|ml| ml.menu_at(col, row));
            let layout_exists = self.active_chrome().menu_layout.is_some();
            if layout_exists {
                if let Some(menu_idx) = hit {
                    if self.menu_state.active_menu == Some(menu_idx) {
                        self.close_menu_with_auto_hide();
                    } else {
                        self.active_window_mut().on_editor_focus_lost();
                        self.menu_state.open_menu(menu_idx);
                    }
                    return Some(Ok(()));
                } else if row == 0 {
                    self.close_menu_with_auto_hide();
                    return Some(Ok(()));
                }
            }
        }

        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();
            if let Some(menu) = all_menus.get(active_idx) {
                match self.handle_menu_dropdown_click(col, row, menu) {
                    Ok(Some(click_result)) => return Some(click_result),
                    Ok(None) => {}
                    Err(e) => return Some(Err(e)),
                }
            }
            self.close_menu_with_auto_hide();
            return Some(Ok(()));
        }

        None
    }

    fn handle_click_file_explorer_area(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let explorer_area = self.active_layout().file_explorer_area?;
        let border_x = explorer_area.x + explorer_area.width.saturating_sub(1);
        if col == border_x && row >= explorer_area.y && row < explorer_area.y + explorer_area.height
        {
            self.active_window_mut().mouse_state.dragging_file_explorer = true;
            self.active_window_mut().mouse_state.drag_start_position = Some((col, row));
            self.active_window_mut()
                .mouse_state
                .drag_start_explorer_width = Some(self.active_window().file_explorer_width);
            return Some(Ok(()));
        }
        if in_rect(col, row, explorer_area) {
            return Some(self.handle_file_explorer_click(col, row, explorer_area));
        }
        None
    }

    fn handle_click_scrollbar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let (split_id, buffer_id, scrollbar_rect, is_on_thumb) =
            self.active_layout().split_areas.iter().find_map(
                |(split_id, buffer_id, _content, scrollbar_rect, thumb_start, thumb_end)| {
                    if in_rect(col, row, *scrollbar_rect) {
                        let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                        let on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
                        Some((*split_id, *buffer_id, *scrollbar_rect, on_thumb))
                    } else {
                        None
                    }
                },
            )?;

        self.focus_split(split_id, buffer_id);
        if is_on_thumb {
            self.active_window_mut().mouse_state.dragging_scrollbar = Some(split_id);
            self.active_window_mut().mouse_state.drag_start_row = Some(row);
            if self.active_window().is_composite_buffer(buffer_id) {
                if let Some(vs) = self
                    .active_window()
                    .composite_view_states
                    .get(&(split_id, buffer_id))
                {
                    self.active_window_mut()
                        .mouse_state
                        .drag_start_composite_scroll_row = Some(vs.scroll_row);
                }
            } else {
                let snap = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .map(|vs| (vs.viewport.top_byte, vs.viewport.top_view_line_offset));
                if let Some((top_byte, top_view_line_offset)) = snap {
                    let ms = &mut self.active_window_mut().mouse_state;
                    ms.drag_start_top_byte = Some(top_byte);
                    ms.drag_start_view_line_offset = Some(top_view_line_offset);
                }
            }
        } else {
            self.active_window_mut().mouse_state.dragging_scrollbar = Some(split_id);
            if let Err(e) = self.active_window_mut().handle_scrollbar_jump(
                col,
                row,
                split_id,
                buffer_id,
                scrollbar_rect,
            ) {
                return Some(Err(e));
            }
            self.active_window_mut().mouse_state.hover_target =
                Some(HoverTarget::ScrollbarThumb(split_id));
        }
        Some(Ok(()))
    }

    fn handle_click_horizontal_scrollbar(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let (split_id, buffer_id, hscrollbar_rect, max_content_width, is_on_thumb) = self
            .active_layout()
            .horizontal_scrollbar_areas
            .iter()
            .find_map(
                |(
                    split_id,
                    buffer_id,
                    hscrollbar_rect,
                    max_content_width,
                    thumb_start,
                    thumb_end,
                )| {
                    if col >= hscrollbar_rect.x
                        && col < hscrollbar_rect.x + hscrollbar_rect.width
                        && row >= hscrollbar_rect.y
                        && row < hscrollbar_rect.y + hscrollbar_rect.height
                    {
                        let relative_col = col.saturating_sub(hscrollbar_rect.x) as usize;
                        let on_thumb = relative_col >= *thumb_start && relative_col < *thumb_end;
                        Some((
                            *split_id,
                            *buffer_id,
                            *hscrollbar_rect,
                            *max_content_width,
                            on_thumb,
                        ))
                    } else {
                        None
                    }
                },
            )?;

        self.focus_split(split_id, buffer_id);
        self.active_window_mut()
            .mouse_state
            .dragging_horizontal_scrollbar = Some(split_id);
        if is_on_thumb {
            self.active_window_mut().mouse_state.drag_start_hcol = Some(col);
            if let Some(vs) = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&split_id)
            {
                self.active_window_mut().mouse_state.drag_start_left_column =
                    Some(vs.viewport.left_column);
            }
        } else {
            self.active_window_mut().mouse_state.drag_start_hcol = None;
            self.active_window_mut().mouse_state.drag_start_left_column = None;
            let relative_col = col.saturating_sub(hscrollbar_rect.x) as f64;
            let track_width = hscrollbar_rect.width as f64;
            let ratio = if track_width > 1.0 {
                (relative_col / (track_width - 1.0)).clamp(0.0, 1.0)
            } else {
                0.0
            };
            if let Some(vs) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .get_mut(&split_id)
            {
                let visible_width = vs.viewport.width as usize;
                let max_scroll = max_content_width.saturating_sub(visible_width);
                let target_col = (ratio * max_scroll as f64).round() as usize;
                vs.viewport.left_column = target_col.min(max_scroll);
                vs.viewport.set_skip_ensure_visible();
            }
        }
        Some(Ok(()))
    }

    /// Map a click on a status-bar segment to its editor `Action`. This is the
    /// single id→action table for the generic click rail; adding a clickable
    /// element means adding one arm here (plus listing it in
    /// `StatusBarRenderer::clickable_for_kind`).
    ///
    /// Most segments dismiss any open menu-style popup first (the #1941
    /// follow-up: otherwise a stale popup overlaps the new prompt). The LSP,
    /// remote, and read-only menus are the exceptions — each owns a toggle
    /// (a second click closes it), so dismissing first would defeat the toggle;
    /// they clear other popups themselves after their toggle check.
    fn dispatch_status_bar_click(
        &mut self,
        id: crate::view::ui::status_bar::StatusBarClickable,
    ) -> AnyhowResult<()> {
        use crate::view::ui::status_bar::StatusBarClickable as C;
        match id {
            C::LineEnding => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::SetLineEnding)
            }
            C::Encoding => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::SetEncoding)
            }
            C::Language => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::SetLanguage)
            }
            // Owns its own toggle (second click closes the popup).
            C::Lsp => self.handle_action(Action::ShowLspStatus),
            // Owns its own toggle; clears other popups itself after the check.
            C::RemoteIndicator => self.handle_action(Action::ShowRemoteIndicatorMenu),
            C::WorkspaceTrust => {
                // Opens the (cancellable) workspace-trust prompt.
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::WorkspaceTrustPrompt)
            }
            C::Warnings => {
                self.dismiss_menu_popups_for_prompt();
                self.handle_action(Action::ShowWarnings)
            }
            C::Messages => self.handle_action(Action::ShowStatusLog),
            // Owns its own toggle (second click closes the read-only menu).
            C::ReadOnly => self.handle_action(Action::ShowReadOnlyMenu),
        }
    }

    fn handle_click_status_bar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let (status_row, _status_x, _status_width) = self.active_chrome().status_bar.area?;
        if row != status_row {
            return None;
        }
        // Generic click rail: one hit-test over every clickable segment drawn
        // last frame. The id→Action mapping (and each element's popup-dismiss
        // nuance) lives in `dispatch_status_bar_click`.
        let clickables = self.active_chrome().status_bar.clickable.clone();
        for (id, r, s, e) in clickables {
            if row == r && col >= s && col < e {
                return Some(self.dispatch_status_bar_click(id));
            }
        }
        // Plugin-registered tokens. Walk the per-frame map produced by
        // `render_status_bar`; on a hit, fire `status_bar_token_clicked`
        // so the registering plugin can react. We split the registry key
        // (`"<plugin>:<token>"`) on the first colon — that's how
        // `register_status_bar_element` builds it.
        let plugin_areas = self.active_chrome().status_bar.plugin_token_areas.clone();
        for (key, (r, s, e)) in plugin_areas {
            if row == r && col >= s && col < e {
                let (plugin_name, token_name) = match key.split_once(':') {
                    Some((p, t)) => (p.to_string(), t.to_string()),
                    None => (String::new(), key.clone()),
                };
                self.dismiss_menu_popups_for_prompt();
                self.plugin_manager.read().unwrap().run_hook(
                    "status_bar_token_clicked",
                    crate::services::plugins::hooks::HookArgs::StatusBarTokenClicked {
                        plugin_name,
                        token_name,
                    },
                );
                return Some(Ok(()));
            }
        }
        None
    }

    fn handle_click_search_options(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        use crate::view::ui::status_bar::SearchOptionsHover;
        let layout = self.active_chrome().search_options_layout.clone()?;
        match layout.checkbox_at(col, row)? {
            SearchOptionsHover::CaseSensitive => {
                Some(self.handle_action(Action::ToggleSearchCaseSensitive))
            }
            SearchOptionsHover::WholeWord => {
                Some(self.handle_action(Action::ToggleSearchWholeWord))
            }
            SearchOptionsHover::Regex => Some(self.handle_action(Action::ToggleSearchRegex)),
            SearchOptionsHover::ConfirmEach => {
                Some(self.handle_action(Action::ToggleSearchConfirmEach))
            }
            SearchOptionsHover::None => None,
        }
    }

    fn handle_click_split_separator(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let separator_areas = self.active_layout().separator_areas.clone();
        for (split_id, direction, sep_x, sep_y, sep_length) in &separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };
            if is_on_separator {
                self.active_window_mut().mouse_state.dragging_separator =
                    Some((*split_id, *direction));
                self.active_window_mut().mouse_state.drag_start_position = Some((col, row));
                let ratio = self
                    .split_manager_mut()
                    .get_ratio((*split_id).into())
                    .or_else(|| self.grouped_split_ratio(*split_id));
                if let Some(ratio) = ratio {
                    self.active_window_mut().mouse_state.drag_start_ratio = Some(ratio);
                }
                return Some(Ok(()));
            }
        }
        None
    }

    fn handle_click_split_controls(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        let close_split_id = self
            .active_layout()
            .close_split_areas
            .iter()
            .find(|(_, btn_row, start_col, end_col)| {
                row == *btn_row && col >= *start_col && col < *end_col
            })
            .map(|(split_id, _, _, _)| *split_id);
        if let Some(split_id) = close_split_id {
            if let Err(e) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .close_split(split_id)
            {
                self.set_status_message(
                    t!("error.cannot_close_split", error = e.to_string()).to_string(),
                );
            } else {
                let new_active = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                if let Some(buffer_id) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .buffer_for_split(new_active)
                {
                    self.set_active_buffer(buffer_id);
                }
                self.set_status_message(t!("split.closed").to_string());
            }
            return Some(Ok(()));
        }

        let maximize_target = self
            .active_layout()
            .maximize_split_areas
            .iter()
            .find(|(_, btn_row, start_col, end_col)| {
                row == *btn_row && col >= *start_col && col < *end_col
            })
            .map(|(split_id, _, _, _)| *split_id);
        if let Some(target) = maximize_target {
            // Move focus to the clicked split before maximizing. Otherwise
            // a click on a non-active split's button leaves the active
            // split (now hidden by the maximize) silently capturing
            // keystrokes. Skip when already maximized: the unmaximize
            // click can only land on the maximized split, which is
            // already the active one.
            let already_maximized = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr.is_maximized())
                .unwrap_or(false);
            if !already_maximized {
                if let Some(buffer_id) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .buffer_for_split(target)
                {
                    self.focus_split(target, buffer_id);
                }
            }
            match self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .toggle_maximize_for(target)
            {
                Ok(maximized) => {
                    let msg = if maximized {
                        t!("split.maximized").to_string()
                    } else {
                        t!("split.restored").to_string()
                    };
                    self.set_status_message(msg);
                }
                Err(e) => self.set_status_message(e),
            }
            return Some(Ok(()));
        }

        None
    }

    fn handle_click_tab_bar(&mut self, col: u16, row: u16) -> Option<AnyhowResult<()>> {
        for (split_id, tab_layout) in &self.active_layout().tab_layouts {
            tracing::debug!(
                "Tab layout for split {:?}: bar_area={:?}, left_scroll={:?}, right_scroll={:?}",
                split_id,
                tab_layout.bar_area,
                tab_layout.left_scroll_area,
                tab_layout.right_scroll_area
            );
        }
        let tab_hit = self
            .active_layout()
            .tab_layouts
            .iter()
            .find_map(|(split_id, tab_layout)| {
                let hit = tab_layout.hit_test(col, row);
                tracing::debug!(
                    "Tab hit_test at ({}, {}) for split {:?} returned {:?}",
                    col,
                    row,
                    split_id,
                    hit
                );
                hit.map(|h| (*split_id, h))
            });
        let (split_id, hit) = tab_hit?;
        match hit {
            TabHit::CloseButton(target) => {
                match target {
                    crate::view::split::TabTarget::Buffer(buffer_id) => {
                        self.focus_split(split_id, buffer_id);
                        self.close_tab_in_split(buffer_id, split_id);
                    }
                    crate::view::split::TabTarget::Group(group_leaf) => {
                        self.close_buffer_group_by_leaf(group_leaf);
                    }
                }
                Some(Ok(()))
            }
            TabHit::TabName(target) => {
                let direction = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .map(|vs| {
                        let open = &vs.open_buffers;
                        let cur = vs.active_target();
                        let cur_idx = open.iter().position(|t| *t == cur);
                        let new_idx = open.iter().position(|t| *t == target);
                        match (cur_idx, new_idx) {
                            (Some(c), Some(n)) if n > c => 1,
                            (Some(c), Some(n)) if n < c => -1,
                            _ => 0,
                        }
                    })
                    .unwrap_or(0);
                self.active_window_mut()
                    .animate_tab_switch(split_id, direction);
                match target {
                    crate::view::split::TabTarget::Buffer(buffer_id) => {
                        self.focus_split(split_id, buffer_id);
                        self.active_window_mut()
                            .promote_buffer_from_preview(buffer_id);
                        self.active_window_mut().mouse_state.dragging_tab = Some(
                            super::types::TabDragState::new(buffer_id, split_id, (col, row)),
                        );
                    }
                    crate::view::split::TabTarget::Group(group_leaf) => {
                        self.activate_group_tab(split_id, group_leaf);
                    }
                }
                Some(Ok(()))
            }
            TabHit::ScrollLeft => {
                self.set_status_message("ScrollLeft clicked!".to_string());
                if let Some(vs) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&split_id)
                {
                    vs.tab_scroll_offset = vs.tab_scroll_offset.saturating_sub(10);
                }
                Some(Ok(()))
            }
            TabHit::ScrollRight => {
                self.set_status_message("ScrollRight clicked!".to_string());
                if let Some(vs) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&split_id)
                {
                    vs.tab_scroll_offset = vs.tab_scroll_offset.saturating_add(10);
                }
                Some(Ok(()))
            }
            TabHit::NewTabButton => {
                // Open the "+" popup just below the button. Close any tab
                // context menu first so only one popup is visible.
                self.active_window_mut().tab_context_menu = None;
                self.active_window_mut().new_tab_menu =
                    Some(super::types::NewTabMenu::new(split_id, col, row + 1));
                Some(Ok(()))
            }
            TabHit::BarBackground => None,
        }
    }

    /// Handle mouse drag event
    pub(super) fn handle_mouse_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        // Dock resize drag: track the pointer column as the new dock
        // width (the right border follows the cursor), clamped so it
        // can't swallow the chrome.
        if self.dock_resizing {
            let max_cols = self.terminal_width.max(20).saturating_sub(20).max(10);
            let new_w = col.saturating_add(1).clamp(10, max_cols);
            let mut changed = false;
            if let Some(fwp) = self.dock.as_mut() {
                if let super::PanelPlacement::LeftDock { width_cols } = &mut fwp.placement {
                    changed = *width_cols != new_w;
                    *width_cols = new_w;
                }
            }
            if changed {
                // Persist the live width *before* relaying out. `relayout`
                // fires the `resize` hook, and the orchestrator answers it
                // by re-issuing the dock's responsive `dock_width`, which
                // `handle_floating_panel_control` clamps against the
                // persisted `dock_width` override. Updating that override
                // here (not only on mouse-up) lets the user's dragged width
                // win the round-trip — otherwise the responsive re-issue
                // snaps the dock straight back and the drag does nothing.
                self.dock_width = Some(new_w);
                // The dock got wider/narrower: reflow the chrome (terminals,
                // viewports, panels) to the new dock width via the funnel.
                self.relayout();
            }
            return Ok(());
        }
        // Floating-panel list scrollbar drag takes precedence — the
        // modal panel owns the input channel while it's up.
        if self.try_widget_scrollbar_drag(super::PanelSlot::Dock, row)
            || self.try_widget_scrollbar_drag(super::PanelSlot::Floating, row)
        {
            let _ = col;
            return Ok(());
        }
        // Mouse-modal overlay: the only legitimate drag is on the overlay's
        // own result-list scrollbar (its drag flag was set on mouse-down).
        // Anything else — text-selection drag in the buffer, a buffer
        // scrollbar behind the overlay — is swallowed so the buffer stays put.
        if self.overlay_prompt_active()
            && !self.active_window().mouse_state.dragging_prompt_scrollbar
        {
            return Ok(());
        }

        // If dragging scrollbar, update scroll position
        if let Some(dragging_split_id) = self.active_window_mut().mouse_state.dragging_scrollbar {
            // Snapshot split_areas so we don't borrow `self.active_layout()` and
            // `self.active_window_mut()` simultaneously below.
            let split_areas = self.active_layout().split_areas.clone();
            for (split_id, buffer_id, _content_rect, scrollbar_rect, _thumb_start, _thumb_end) in
                &split_areas
            {
                if *split_id == dragging_split_id {
                    // Check if we started dragging from the thumb (have drag_start_row)
                    if self.active_window().mouse_state.drag_start_row.is_some() {
                        // Relative drag from thumb
                        self.active_window_mut().handle_scrollbar_drag_relative(
                            row,
                            *split_id,
                            *buffer_id,
                            *scrollbar_rect,
                        )?;
                    } else {
                        // Jump drag (started from track)
                        self.active_window_mut().handle_scrollbar_jump(
                            col,
                            row,
                            *split_id,
                            *buffer_id,
                            *scrollbar_rect,
                        )?;
                    }
                    return Ok(());
                }
            }
        }

        // If dragging horizontal scrollbar, update horizontal scroll position
        if let Some(dragging_split_id) = self
            .active_window_mut()
            .mouse_state
            .dragging_horizontal_scrollbar
        {
            // Clone the scrollbar layout so the loop doesn't hold an
            // immutable borrow on `self` while it mutates
            // `self.split_view_states`. The active window's layout cache
            // is repopulated each frame, so a one-frame snapshot is fine.
            let hscrollbar_areas = self.active_layout().horizontal_scrollbar_areas.clone();
            for (
                split_id,
                _buffer_id,
                hscrollbar_rect,
                max_content_width,
                thumb_start,
                thumb_end,
            ) in &hscrollbar_areas
            {
                if *split_id == dragging_split_id {
                    let track_width = hscrollbar_rect.width as f64;
                    if track_width <= 1.0 {
                        break;
                    }

                    if let (Some(drag_start_hcol), Some(drag_start_left_column)) = (
                        self.active_window_mut().mouse_state.drag_start_hcol,
                        self.active_window_mut().mouse_state.drag_start_left_column,
                    ) {
                        // Relative drag from thumb - move proportionally to mouse offset
                        // Use thumb size to compute the correct ratio so thumb tracks with mouse
                        let col_offset = (col as i32) - (drag_start_hcol as i32);
                        if let Some(view_state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_view_states_mut())
                            .expect("active window must have a populated split layout")
                            .get_mut(&dragging_split_id)
                        {
                            let visible_width = view_state.viewport.width as usize;
                            let max_scroll = max_content_width.saturating_sub(visible_width);
                            if max_scroll > 0 {
                                let thumb_size = thumb_end.saturating_sub(*thumb_start).max(1);
                                let track_travel = (track_width - thumb_size as f64).max(1.0);
                                let scroll_per_pixel = max_scroll as f64 / track_travel;
                                let scroll_offset =
                                    (col_offset as f64 * scroll_per_pixel).round() as i64;
                                let new_left =
                                    (drag_start_left_column as i64 + scroll_offset).max(0) as usize;
                                view_state.viewport.left_column = new_left.min(max_scroll);
                                view_state.viewport.set_skip_ensure_visible();
                            }
                        }
                    } else {
                        // Jump drag (started from track) - jump to absolute position
                        let relative_col = col.saturating_sub(hscrollbar_rect.x) as f64;
                        let ratio = (relative_col / (track_width - 1.0)).clamp(0.0, 1.0);

                        if let Some(view_state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_view_states_mut())
                            .expect("active window must have a populated split layout")
                            .get_mut(&dragging_split_id)
                        {
                            let visible_width = view_state.viewport.width as usize;
                            let max_scroll = max_content_width.saturating_sub(visible_width);
                            let target_col = (ratio * max_scroll as f64).round() as usize;
                            view_state.viewport.left_column = target_col.min(max_scroll);
                            view_state.viewport.set_skip_ensure_visible();
                        }
                    }

                    return Ok(());
                }
            }
        }

        // If selecting text in popup, extend selection
        if let Some(popup_idx) = self.active_window_mut().mouse_state.selecting_in_popup {
            // Find the popup area from cached layout
            if let Some((_, _, inner_rect, scroll_offset, _, _, _)) = self
                .active_chrome()
                .popup_areas
                .iter()
                .find(|(idx, _, _, _, _, _, _)| *idx == popup_idx)
            {
                // Check if mouse is within the popup inner area
                if col >= inner_rect.x
                    && col < inner_rect.x + inner_rect.width
                    && row >= inner_rect.y
                    && row < inner_rect.y + inner_rect.height
                {
                    let relative_col = (col - inner_rect.x) as usize;
                    let relative_row = (row - inner_rect.y) as usize;
                    let line = scroll_offset + relative_row;

                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.get_mut(popup_idx) {
                        popup.extend_selection(line, relative_col);
                    }
                }
            }
            return Ok(());
        }

        // If dragging the floating-overlay prompt's scrollbar
        // (issue #1796), update its scroll_offset using the same
        // math as the click handler. Same shared-widget logic the
        // popup-scrollbar drag uses below.
        if self
            .active_window_mut()
            .mouse_state
            .dragging_prompt_scrollbar
        {
            use crate::view::ui::scrollbar::ScrollbarState;
            // Snapshot chrome rects up front so the prompt borrow on
            // active_window_mut() doesn't conflict.
            let sb_rect = self.active_chrome().suggestions_scrollbar_rect;
            let suggestions_area_visible =
                self.active_chrome().suggestions_area.map(|(_, _, v, _)| v);
            let active_window_id = self.active_window;
            if let (Some(sb_rect), Some(prompt)) = (
                sb_rect,
                self.windows
                    .get_mut(&active_window_id)
                    .and_then(|w| w.prompt.as_mut()),
            ) {
                let visible = suggestions_area_visible.unwrap_or(prompt.suggestions.len().min(10));
                let total = prompt.suggestions.len();
                let track_height = sb_rect.height as usize;
                // Allow dragging slightly past the top/bottom; clamp
                // here rather than rejecting so the thumb keeps up
                // with a fast mouse.
                let clamped_row =
                    row.clamp(sb_rect.y, sb_rect.y + sb_rect.height.saturating_sub(1));
                let click_row = clamped_row.saturating_sub(sb_rect.y) as usize;
                let state = ScrollbarState::new(total, visible, prompt.scroll_offset);
                prompt.scroll_offset = state.click_to_offset(track_height, click_row);
            }
            return Ok(());
        }

        // If dragging popup scrollbar, update popup scroll position
        if let Some(popup_idx) = self
            .active_window_mut()
            .mouse_state
            .dragging_popup_scrollbar
        {
            // Find the popup's scrollbar rect from cached layout
            if let Some((_, _, inner_rect, _, _, Some(sb_rect), total_lines)) = self
                .active_chrome()
                .popup_areas
                .iter()
                .find(|(idx, _, _, _, _, _, _)| *idx == popup_idx)
            {
                let track_height = sb_rect.height as usize;
                let visible_lines = inner_rect.height as usize;

                if track_height > 0 && *total_lines > visible_lines {
                    let relative_row = row.saturating_sub(sb_rect.y) as usize;
                    let max_scroll = total_lines.saturating_sub(visible_lines);
                    let target_scroll = if track_height > 1 {
                        (relative_row * max_scroll) / (track_height.saturating_sub(1))
                    } else {
                        0
                    };

                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.get_mut(popup_idx) {
                        let current_scroll = popup.scroll_offset as i32;
                        let delta = target_scroll as i32 - current_scroll;
                        popup.scroll_by(delta);
                    }
                }
            }
            return Ok(());
        }

        // If dragging separator, update split ratio
        if let Some((split_id, direction)) = self.active_window_mut().mouse_state.dragging_separator
        {
            self.handle_separator_drag(col, row, split_id, direction)?;
            return Ok(());
        }

        // If dragging file explorer border, update width
        if self.active_window_mut().mouse_state.dragging_file_explorer {
            self.handle_file_explorer_border_drag(col)?;
            return Ok(());
        }

        // If dragging to select text
        if self.active_window_mut().mouse_state.dragging_text_selection {
            self.handle_text_selection_drag(col, row)?;
            return Ok(());
        }

        // If dragging a tab, update position and compute drop zone
        if self.active_window_mut().mouse_state.dragging_tab.is_some() {
            self.handle_tab_drag(col, row)?;
            return Ok(());
        }

        Ok(())
    }

    /// Handle text selection drag - extends selection from anchor to current position
    fn handle_text_selection_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        use crate::model::event::Event;
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

        let Some(split_id) = self.active_window_mut().mouse_state.drag_selection_split else {
            return Ok(());
        };
        let Some(anchor_position) = self.active_window_mut().mouse_state.drag_selection_anchor
        else {
            return Ok(());
        };

        // Find the buffer and content rect for this split in one pass
        let Some((buffer_id, content_rect)) = self
            .active_layout()
            .split_areas
            .iter()
            .find(|(sid, _, _, _, _, _)| *sid == split_id)
            .map(|(_, bid, rect, _, _, _)| (*bid, *rect))
        else {
            return Ok(());
        };

        // Get cached view line mappings for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();

        let leaf_id = split_id;

        // Get fallback from SplitViewState viewport
        let fallback = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

        // Calculate the target position and selection geometry by
        // reading buffer state directly, then dispatch the move via
        // Window helpers.
        let drag_by_words = self.active_window_mut().mouse_state.drag_selection_by_words;
        let drag_word_end = self.active_window_mut().mouse_state.drag_selection_word_end;

        let Some((target_position, new_position, anchor_position, new_sticky_column)) = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .and_then(|state| {
                let gutter_width = state.margins.left_total_width() as u16;
                let target_position = super::click_geometry::screen_to_buffer_position(
                    col,
                    row,
                    content_rect,
                    gutter_width,
                    &cached_mappings,
                    fallback,
                    true, // Allow gutter clicks for drag selection
                    compose_width,
                )?;
                let (new_position, anchor_pos) = if drag_by_words {
                    if target_position >= anchor_position {
                        (
                            find_word_end(&state.buffer, target_position),
                            anchor_position,
                        )
                    } else {
                        let word_end = drag_word_end.unwrap_or(anchor_position);
                        (find_word_start(&state.buffer, target_position), word_end)
                    }
                } else {
                    (target_position, anchor_position)
                };
                // Visual column, not byte column — see `visual_column_of`.
                let new_sticky_column =
                    crate::primitives::display_width::visual_column_of(&state.buffer, new_position);
                Some((target_position, new_position, anchor_pos, new_sticky_column))
            })
        else {
            return Ok(());
        };
        let _ = target_position;

        let (primary_cursor_id, old_position, old_anchor, old_sticky_column) = self
            .active_window()
            .buffers
            .splits()
            .and_then(|(_, vs)| vs.get(&leaf_id))
            .map(|vs| {
                let cursor = vs.cursors.primary();
                (
                    vs.cursors.primary_id(),
                    cursor.position,
                    cursor.anchor,
                    cursor.sticky_column,
                )
            })
            .unwrap_or((CursorId(0), 0, None, None));

        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position,
            new_position,
            old_anchor,
            new_anchor: Some(anchor_position),
            old_sticky_column,
            new_sticky_column: new_sticky_column.or(old_sticky_column),
        };

        if let Some(event_log) = self.active_window_mut().event_logs.get_mut(&buffer_id) {
            event_log.append(event.clone());
        }
        self.active_window_mut()
            .apply_event_to_buffer(buffer_id, leaf_id, &event);

        Ok(())
    }

    /// Handle file explorer border drag for resizing
    pub(super) fn handle_file_explorer_border_drag(&mut self, col: u16) -> AnyhowResult<()> {
        let Some((start_col, _start_row)) =
            self.active_window_mut().mouse_state.drag_start_position
        else {
            return Ok(());
        };
        let Some(start_width) = self
            .active_window_mut()
            .mouse_state
            .drag_start_explorer_width
        else {
            return Ok(());
        };

        let delta = col as i32 - start_col as i32;
        let total_width = self.terminal_width as i32;

        // Drag preserves the variant the user chose. A user editing
        // columns doesn't want their mode silently flipped to percent
        // just because they grabbed the divider.
        if total_width > 0 {
            use crate::config::ExplorerWidth;
            self.active_window_mut().file_explorer_width = match start_width {
                ExplorerWidth::Percent(start_pct) => {
                    let percent_delta = (delta * 100) / total_width;
                    let new_pct = (start_pct as i32 + percent_delta).clamp(0, 100) as u8;
                    ExplorerWidth::Percent(new_pct)
                }
                ExplorerWidth::Columns(start_cols) => {
                    let new_cols = (start_cols as i32 + delta).clamp(0, total_width) as u16;
                    ExplorerWidth::Columns(new_cols)
                }
            };
            // The sidebar width changed: reflow terminals/viewports/panels
            // through the single layout funnel.
            self.relayout();
        }

        Ok(())
    }

    /// Handle separator drag for split resizing
    pub(super) fn handle_separator_drag(
        &mut self,
        col: u16,
        row: u16,
        split_id: ContainerId,
        direction: SplitDirection,
    ) -> AnyhowResult<()> {
        let Some((start_col, start_row)) = self.active_window_mut().mouse_state.drag_start_position
        else {
            return Ok(());
        };
        let Some(start_ratio) = self.active_window_mut().mouse_state.drag_start_ratio else {
            return Ok(());
        };
        let Some(editor_area) = self.active_layout().editor_content_area else {
            return Ok(());
        };

        // Calculate the delta in screen space
        let (delta, total_size) = match direction {
            SplitDirection::Horizontal => {
                // For horizontal splits, we move the separator up/down (row changes)
                let delta = row as i32 - start_row as i32;
                let total = editor_area.height as i32;
                (delta, total)
            }
            SplitDirection::Vertical => {
                // For vertical splits, we move the separator left/right (col changes)
                let delta = col as i32 - start_col as i32;
                let total = editor_area.width as i32;
                (delta, total)
            }
        };

        // Convert screen delta to ratio delta
        // The ratio represents the fraction of space the first split gets
        if total_size > 0 {
            let ratio_delta = delta as f32 / total_size as f32;
            let new_ratio = (start_ratio + ratio_delta).clamp(0.1, 0.9);

            // Update the split ratio. The container may live in the main
            // split tree or inside a stashed Grouped subtree (buffer group
            // panels like the theme editor); try the main tree first and
            // fall back to the grouped subtrees.
            if self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .get_ratio(split_id.into())
                .is_some()
            {
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_ratio(split_id, new_ratio);
            } else {
                self.set_grouped_split_ratio(split_id, new_ratio);
            }
            // Reflow live as the separator moves so terminals track the
            // split sizes during the drag, not just on release.
            self.relayout();
        }

        Ok(())
    }

    /// Handle right-click event
    pub(super) fn handle_right_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        // A right-click anywhere dismisses the "+" new-tab popup (it's a
        // left-click-only menu).
        self.active_window_mut().new_tab_menu = None;

        // Right-click inside the orchestrator dock column → let the plugin
        // raise a per-session context menu. Mirrors the left-click path:
        // re-focus the dock first (so the menu acts against a focused dock)
        // and swallow the event so it never falls through to the editor or
        // the file-explorer menu below.
        if let Some(super::PanelPlacement::LeftDock { width_cols }) =
            self.dock.as_ref().map(|f| f.placement)
        {
            if col < width_cols {
                if self.dock.as_ref().map(|f| !f.focused).unwrap_or(false) {
                    self.refocus_floating_panel(super::PanelSlot::Dock);
                }
                self.handle_floating_widget_context_click(super::PanelSlot::Dock, col, row);
                return Ok(());
            }
        }

        let frame_w = self.active_chrome().last_frame.width;
        let frame_h = self.active_chrome().last_frame.height;
        if let Some(ref menu) = self.active_window().file_explorer_context_menu {
            let (menu_x, menu_y) = menu.clamped_position(frame_w, frame_h);
            let menu_width = super::types::FILE_EXPLORER_CONTEXT_MENU_WIDTH;
            let menu_height = menu.height();
            if col >= menu_x
                && col < menu_x + menu_width
                && row >= menu_y
                && row < menu_y + menu_height
            {
                return Ok(());
            }
        }

        // First check if a tab context menu is open and the click is on a menu item
        if let Some(ref menu) = self.active_window_mut().tab_context_menu {
            let menu_x = menu.position.0;
            let menu_y = menu.position.1;
            let menu_width = 22u16; // "Close to the Right" + padding
            let menu_height = super::types::TabContextMenuItem::all().len() as u16 + 2; // items + borders

            // Check if click is inside the menu
            if col >= menu_x
                && col < menu_x + menu_width
                && row >= menu_y
                && row < menu_y + menu_height
            {
                // Click inside menu - let left-click handler deal with it
                return Ok(());
            }
        }

        if let Some(explorer_area) = self.active_layout().file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row < explorer_area.y + explorer_area.height
                && row > explorer_area.y
            // skip title row
            {
                let relative_row = row.saturating_sub(explorer_area.y + 1);
                let (is_multi, is_root_selected) =
                    if let Some(explorer) = self.file_explorer_mut().as_mut() {
                        let display_nodes = explorer.get_display_nodes();
                        let scroll_offset = explorer.get_scroll_offset();
                        let clicked_index = (relative_row as usize) + scroll_offset;
                        let mut clicked_is_root = false;
                        if clicked_index < display_nodes.len() {
                            let (node_id, _) = display_nodes[clicked_index];
                            explorer.set_selected(Some(node_id));
                            clicked_is_root = node_id == explorer.tree().root_id();
                        }
                        (explorer.has_multi_selection(), clicked_is_root)
                    } else {
                        (false, false)
                    };
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::FileExplorer;
                self.active_window_mut().tab_context_menu = None;
                self.active_window_mut().file_explorer_context_menu =
                    Some(super::types::FileExplorerContextMenu::new(
                        col,
                        row + 1,
                        is_multi,
                        is_root_selected,
                    ));
                return Ok(());
            }
        }

        self.active_window_mut().file_explorer_context_menu = None;

        // Check if right-click is on a tab
        let tab_hit = self
            .active_layout()
            .tab_layouts
            .iter()
            .find_map(
                |(split_id, tab_layout)| match tab_layout.hit_test(col, row) {
                    Some(TabHit::TabName(target) | TabHit::CloseButton(target)) => {
                        // Context menu only makes sense for buffer tabs; groups are
                        // plugin-managed and closed via the close button.
                        target.as_buffer().map(|bid| (*split_id, bid))
                    }
                    _ => None,
                },
            );

        if let Some((split_id, buffer_id)) = tab_hit {
            // Open tab context menu
            self.active_window_mut().tab_context_menu =
                Some(TabContextMenu::new(buffer_id, split_id, col, row + 1));
        } else {
            // Click outside tab - close context menu if open
            self.active_window_mut().tab_context_menu = None;
        }

        Ok(())
    }

    /// Handle left-click on tab context menu
    pub(super) fn handle_tab_context_menu_click(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let menu = self.active_window_mut().tab_context_menu.as_ref()?;
        let menu_x = menu.position.0;
        let menu_y = menu.position.1;
        let menu_width = 22u16;
        let items = super::types::TabContextMenuItem::all();
        let menu_height = items.len() as u16 + 2; // items + borders

        // Check if click is inside the menu area
        if col < menu_x || col >= menu_x + menu_width || row < menu_y || row >= menu_y + menu_height
        {
            // Click outside menu - close it
            self.active_window_mut().tab_context_menu = None;
            return Some(Ok(()));
        }

        // Check if click is on the border (first or last row)
        if row == menu_y || row == menu_y + menu_height - 1 {
            return Some(Ok(()));
        }

        // Calculate which item was clicked (accounting for border)
        let item_idx = (row - menu_y - 1) as usize;
        if item_idx >= items.len() {
            return Some(Ok(()));
        }

        // Get the menu state before closing it
        let buffer_id = menu.buffer_id;
        let split_id = menu.split_id;
        let item = items[item_idx];

        // Close the menu
        self.active_window_mut().tab_context_menu = None;

        // Execute the action
        Some(self.execute_tab_context_menu_action(item, buffer_id, split_id))
    }

    /// Handle left-click on the "+" new-tab popup menu.
    pub(super) fn handle_new_tab_menu_click(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let menu = self.active_window_mut().new_tab_menu.as_ref()?;
        let (menu_x, menu_y) = menu.position;
        let items = super::types::NewTabMenuItem::all();
        let menu_width = super::types::NEW_TAB_MENU_WIDTH;
        let menu_height = items.len() as u16 + 2; // items + borders

        // Click outside the menu closes it.
        if col < menu_x || col >= menu_x + menu_width || row < menu_y || row >= menu_y + menu_height
        {
            self.active_window_mut().new_tab_menu = None;
            return Some(Ok(()));
        }

        // Border rows (first/last) are inert.
        if row == menu_y || row == menu_y + menu_height - 1 {
            return Some(Ok(()));
        }

        let item_idx = (row - menu_y - 1) as usize;
        if item_idx >= items.len() {
            return Some(Ok(()));
        }

        let split_id = menu.split_id;
        let item = items[item_idx];

        // Close the menu before running the action.
        self.active_window_mut().new_tab_menu = None;

        Some(self.execute_new_tab_menu_action(item, split_id))
    }

    /// Execute a "+" new-tab popup menu action.
    fn execute_new_tab_menu_action(
        &mut self,
        item: super::types::NewTabMenuItem,
        split_id: LeafId,
    ) -> AnyhowResult<()> {
        use super::types::NewTabMenuItem;
        // Ensure the new buffer/terminal lands in the split whose "+" was
        // clicked: `open_terminal`/`new_buffer` act on the active split, so
        // focus that split first (via the buffer it currently shows).
        if let Some(buffer_id) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .and_then(|(mgr, _)| mgr.buffer_for_split(split_id))
        {
            self.focus_split(split_id, buffer_id);
        }
        match item {
            NewTabMenuItem::NewTerminal => {
                self.open_terminal();
            }
            NewTabMenuItem::NewFile => {
                self.new_buffer();
            }
        }
        Ok(())
    }

    /// Execute a tab context menu action
    fn execute_tab_context_menu_action(
        &mut self,
        item: super::types::TabContextMenuItem,
        buffer_id: BufferId,
        leaf_id: LeafId,
    ) -> AnyhowResult<()> {
        use super::types::TabContextMenuItem;
        match item {
            TabContextMenuItem::Close => {
                self.close_tab_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseOthers => {
                self.close_other_tabs_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseToRight => {
                self.close_tabs_to_right_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseToLeft => {
                self.close_tabs_to_left_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseAll => {
                self.close_all_tabs_in_split(leaf_id);
            }
            TabContextMenuItem::CopyRelativePath => {
                self.copy_buffer_path(buffer_id, true);
            }
            TabContextMenuItem::CopyFullPath => {
                self.copy_buffer_path(buffer_id, false);
            }
        }

        Ok(())
    }

    /// Handle keyboard input for the "+" new-tab popup menu.
    ///
    /// While the popup is open it owns the keyboard: Up/Down move the
    /// highlight, Enter activates the highlighted item, Esc dismisses it,
    /// and *every other key is swallowed* so nothing leaks into the buffer
    /// underneath. Returns `Some` when a popup is open (the key is always
    /// consumed in that case), `None` when there is no popup.
    pub(super) fn handle_new_tab_menu_key(
        &mut self,
        code: crossterm::event::KeyCode,
    ) -> Option<AnyhowResult<()>> {
        use crossterm::event::KeyCode;

        self.active_window().new_tab_menu.as_ref()?;

        match code {
            KeyCode::Up => {
                if let Some(ref mut menu) = self.active_window_mut().new_tab_menu {
                    menu.prev_item();
                }
            }
            KeyCode::Down => {
                if let Some(ref mut menu) = self.active_window_mut().new_tab_menu {
                    menu.next_item();
                }
            }
            KeyCode::Enter => {
                let selected = self.active_window().new_tab_menu.as_ref().map(|menu| {
                    (
                        super::types::NewTabMenuItem::all()[menu.highlighted],
                        menu.split_id,
                    )
                });
                self.active_window_mut().new_tab_menu = None;
                if let Some((item, split_id)) = selected {
                    return Some(self.execute_new_tab_menu_action(item, split_id));
                }
            }
            KeyCode::Esc => {
                self.active_window_mut().new_tab_menu = None;
            }
            // Filter out everything else while the popup is focused.
            _ => {}
        }
        Some(Ok(()))
    }

    /// Handle keyboard input for the tab right-click context menu.
    ///
    /// Modal like [`handle_new_tab_menu_key`]: Up/Down navigate, Enter
    /// activates the highlighted item, Esc dismisses, and all other keys are
    /// swallowed so they can't reach the buffer underneath.
    pub(super) fn handle_tab_context_menu_key(
        &mut self,
        code: crossterm::event::KeyCode,
    ) -> Option<AnyhowResult<()>> {
        use crossterm::event::KeyCode;

        self.active_window().tab_context_menu.as_ref()?;

        match code {
            KeyCode::Up => {
                if let Some(ref mut menu) = self.active_window_mut().tab_context_menu {
                    menu.prev_item();
                }
            }
            KeyCode::Down => {
                if let Some(ref mut menu) = self.active_window_mut().tab_context_menu {
                    menu.next_item();
                }
            }
            KeyCode::Enter => {
                let selected = self
                    .active_window()
                    .tab_context_menu
                    .as_ref()
                    .map(|menu| (menu.highlighted_item(), menu.buffer_id, menu.split_id));
                self.active_window_mut().tab_context_menu = None;
                if let Some((item, buffer_id, split_id)) = selected {
                    return Some(self.execute_tab_context_menu_action(item, buffer_id, split_id));
                }
            }
            KeyCode::Esc => {
                self.active_window_mut().tab_context_menu = None;
            }
            // Filter out everything else while the popup is focused.
            _ => {}
        }
        Some(Ok(()))
    }

    /// Handle keyboard navigation for the file explorer context menu.
    /// Returns `Some` if the key was consumed, `None` to let normal dispatch continue.
    pub(super) fn handle_file_explorer_context_menu_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyModifiers;

        if modifiers != KeyModifiers::NONE {
            return None;
        }

        match code {
            KeyCode::Up => {
                if let Some(ref mut menu) = self.active_window_mut().file_explorer_context_menu {
                    menu.prev_item();
                }
                Some(Ok(()))
            }
            KeyCode::Down => {
                if let Some(ref mut menu) = self.active_window_mut().file_explorer_context_menu {
                    menu.next_item();
                }
                Some(Ok(()))
            }
            KeyCode::Enter => {
                let item = {
                    let menu = self
                        .active_window_mut()
                        .file_explorer_context_menu
                        .as_ref()?;
                    menu.items()[menu.highlighted]
                };
                self.active_window_mut().file_explorer_context_menu = None;
                self.execute_file_explorer_context_menu_action(item);
                Some(Ok(()))
            }
            KeyCode::Esc => {
                self.active_window_mut().file_explorer_context_menu = None;
                Some(Ok(()))
            }
            _ => None,
        }
    }

    /// Handle left-click on the file explorer context menu
    pub(super) fn handle_file_explorer_context_menu_click(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        // Extract all needed values while the immutable borrow is live, then mutate.
        let frame_w = self.active_chrome().last_frame.width;
        let frame_h = self.active_chrome().last_frame.height;
        let clicked_item: Option<super::types::FileExplorerContextMenuItem> = {
            let menu = self.active_window().file_explorer_context_menu.as_ref()?;
            let (menu_x, menu_y) = menu.clamped_position(frame_w, frame_h);
            let menu_width = super::types::FILE_EXPLORER_CONTEXT_MENU_WIDTH;
            let menu_height = menu.height();

            if col < menu_x
                || col >= menu_x + menu_width
                || row < menu_y
                || row >= menu_y + menu_height
            {
                self.active_window_mut().file_explorer_context_menu = None;
                return Some(Ok(()));
            }

            if row == menu_y || row == menu_y + menu_height - 1 {
                return Some(Ok(()));
            }

            let item_idx = (row - menu_y - 1) as usize;
            menu.items().get(item_idx).copied()
        };

        self.active_window_mut().file_explorer_context_menu = None;
        if let Some(item) = clicked_item {
            self.execute_file_explorer_context_menu_action(item);
        }
        Some(Ok(()))
    }

    fn execute_file_explorer_context_menu_action(
        &mut self,
        item: super::types::FileExplorerContextMenuItem,
    ) {
        use super::types::FileExplorerContextMenuItem;
        match item {
            FileExplorerContextMenuItem::NewFile => self.file_explorer_new_file(),
            FileExplorerContextMenuItem::NewDirectory => self.file_explorer_new_directory(),
            FileExplorerContextMenuItem::Rename => self.file_explorer_rename(),
            FileExplorerContextMenuItem::Cut => self.active_window_mut().file_explorer_cut(),
            FileExplorerContextMenuItem::Copy => self.active_window_mut().file_explorer_copy(),
            FileExplorerContextMenuItem::Paste => self.file_explorer_paste(),
            FileExplorerContextMenuItem::Duplicate => self.file_explorer_duplicate(),
            FileExplorerContextMenuItem::Delete => self.file_explorer_delete(),
            FileExplorerContextMenuItem::CopyFullPath => self.file_explorer_copy_path(false),
            FileExplorerContextMenuItem::CopyRelativePath => self.file_explorer_copy_path(true),
        }
    }

    /// Show a tooltip for a file explorer status indicator
    fn show_file_explorer_status_tooltip(&mut self, path: std::path::PathBuf, col: u16, row: u16) {
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let is_directory = path.is_dir();
        let has_unsaved_changes = self.file_explorer_node_has_unsaved_changes(&path, is_directory);

        let node_metadata = self
            .file_explorer()
            .and_then(|explorer| explorer.tree().get_node_by_path(&path))
            .and_then(|node| node.entry.metadata.as_ref());
        let is_hidden = node_metadata.map(|m| m.is_hidden).unwrap_or(false);
        let is_symlink = path.is_symlink();
        let theme = self.theme.read().unwrap();
        let neutral_fg = if is_hidden {
            theme.line_number_fg
        } else if is_symlink {
            theme.syntax_type
        } else if is_directory {
            theme.syntax_keyword
        } else {
            theme.editor_fg
        };
        let slot_resolver = self.file_explorer_slot_resolver();
        let slot_context = crate::view::file_tree::ExplorerSlotContext {
            path: &path,
            is_dir: is_directory,
            has_unsaved: has_unsaved_changes,
            is_symlink,
            is_hidden,
            decorations: &self.active_window().file_explorer_decoration_cache,
            slot_overrides: &self.active_window().file_explorer_slot_override_cache,
            theme: &theme,
            neutral_fg,
        };
        let slot_resolution = slot_resolver.resolve(&slot_context);

        // Build tooltip content
        let Some(summary) = slot_resolution.trailing.and_then(|slot| slot.tooltip) else {
            return; // No status to show
        };
        let mut lines = summary.lines;
        let has_custom_trailing_override = self
            .active_window()
            .file_explorer_slot_override_cache
            .has_trailing_override_for_path(&path);

        if !has_custom_trailing_override {
            // Compatibility tooltips enrich native git/status content with
            // directory child summaries and file diff stats. Explicit slot
            // overrides own their hover content end-to-end.
            if is_directory {
                if let Some(modified_files) = self.get_modified_files_in_directory(&path) {
                    lines.push(String::new()); // Empty line separator
                    lines.push("Modified files:".to_string());
                    const MAX_FILES: usize = 8;
                    for (i, file) in modified_files.iter().take(MAX_FILES).enumerate() {
                        // Show relative path from the directory
                        let display_name = file
                            .strip_prefix(&path)
                            .unwrap_or(file)
                            .to_string_lossy()
                            .to_string();
                        lines.push(format!("  {}", display_name));
                        if i == MAX_FILES - 1 && modified_files.len() > MAX_FILES {
                            lines.push(format!(
                                "  ... and {} more",
                                modified_files.len() - MAX_FILES
                            ));
                            break;
                        }
                    }
                }
            } else if let Some(stats) = self.get_git_diff_stats(&path) {
                // For files, try to get git diff stats
                lines.push(String::new()); // Empty line separator
                lines.push(stats);
            }
        }

        if lines.is_empty() {
            return;
        }

        // Create popup
        let mut popup = Popup::text(lines, &self.theme.read().unwrap());
        popup.title = Some(summary.title);
        popup.transient = true;
        popup.position = PopupPosition::Fixed { x: col, y: row + 1 };
        popup.width = 50;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.read().unwrap().popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.read().unwrap().popup_bg);

        // Show the popup
        let __buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&__buffer_id)
        {
            state.popups.show(popup);
        }
    }

    fn file_explorer_node_has_unsaved_changes(
        &self,
        path: &std::path::Path,
        is_directory: bool,
    ) -> bool {
        if is_directory {
            self.windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .iter()
                .any(|(buffer_id, state)| {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.active_window().buffer_metadata.get(buffer_id)
                        {
                            if let Some(file_path) = metadata.file_path() {
                                return file_path.starts_with(path);
                            }
                        }
                    }
                    false
                })
        } else {
            self.windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .expect("active window present")
                .iter()
                .any(|(buffer_id, state)| {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.active_window().buffer_metadata.get(buffer_id)
                        {
                            return metadata.file_path().map(|p| p.as_path()) == Some(path);
                        }
                    }
                    false
                })
        }
    }

    /// Dismiss the file explorer status tooltip
    fn dismiss_file_explorer_status_tooltip(&mut self) {
        // Dismiss any transient popups
        let __buffer_id = self.active_buffer();
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&__buffer_id)
        {
            state.popups.dismiss_transient();
        }
    }

    /// Get git diff stats for a file (insertions/deletions)
    fn get_git_diff_stats(&self, path: &std::path::Path) -> Option<String> {
        use crate::services::process_hidden::HideWindow;
        use std::process::Command;

        // Run git diff --numstat for the file
        let output = Command::new("git")
            .args(["diff", "--numstat", "--"])
            .arg(path)
            .current_dir(self.working_dir())
            .hide_window()
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let line = stdout.lines().next()?;
        let parts: Vec<&str> = line.split('\t').collect();

        if parts.len() >= 2 {
            let insertions = parts[0];
            let deletions = parts[1];

            // Handle binary files (shows as -)
            if insertions == "-" && deletions == "-" {
                return Some("Binary file changed".to_string());
            }

            let ins: i32 = insertions.parse().unwrap_or(0);
            let del: i32 = deletions.parse().unwrap_or(0);

            if ins > 0 || del > 0 {
                return Some(format!("+{} -{} lines", ins, del));
            }
        }

        // Also check staged changes
        let staged_output = Command::new("git")
            .args(["diff", "--numstat", "--cached", "--"])
            .arg(path)
            .current_dir(self.working_dir())
            .hide_window()
            .output()
            .ok()?;

        if staged_output.status.success() {
            let staged_stdout = String::from_utf8_lossy(&staged_output.stdout);
            if let Some(line) = staged_stdout.lines().next() {
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() >= 2 {
                    let insertions = parts[0];
                    let deletions = parts[1];

                    if insertions == "-" && deletions == "-" {
                        return Some("Binary file staged".to_string());
                    }

                    let ins: i32 = insertions.parse().unwrap_or(0);
                    let del: i32 = deletions.parse().unwrap_or(0);

                    if ins > 0 || del > 0 {
                        return Some(format!("+{} -{} lines (staged)", ins, del));
                    }
                }
            }
        }

        None
    }

    /// Get list of modified files in a directory
    fn get_modified_files_in_directory(
        &self,
        dir_path: &std::path::Path,
    ) -> Option<Vec<std::path::PathBuf>> {
        let modified_files = self
            .active_window()
            .file_explorer_decoration_cache
            .direct_paths_under(dir_path);

        (!modified_files.is_empty()).then_some(modified_files)
    }

    /// Hit-test a click against the floating widget panel. Clicks
    /// inside the panel's inner rect resolve to a widget row/byte
    /// and fire `widget_event` via the same path
    /// Forward a vertical-wheel scroll to the active floating
    /// widget panel — same plumbing the orchestrator's
    /// embedded-widget panels use, but the floating panel
    /// doesn't show up in `split_at_position` so it needs its
    /// own dispatch entry point. Returns `true` when the panel
    /// is active AND the mouse is inside its inner rect (so the
    /// caller knows the wheel was consumed and shouldn't fall
    /// through to buffer scrolling).
    fn handle_floating_widget_panel_wheel(
        &mut self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
        delta: i32,
    ) -> bool {
        let inner = match self.panel(slot) {
            Some(fwp) => match fwp.last_inner_rect {
                Some(rect) => rect,
                None => return false,
            },
            None => return false,
        };
        if col < inner.x || col >= inner.x + inner.width {
            return false;
        }
        if row < inner.y || row >= inner.y + inner.height {
            return false;
        }
        let scrolled = self.handle_widget_panel_wheel(slot.buffer_id(), delta);
        // The non-modal dock must swallow the wheel whenever the pointer
        // is over it, even when the list is too short to scroll — the
        // scroll must never leak through to the active window beneath.
        let is_dock = matches!(
            self.panel(slot).map(|f| f.placement),
            Some(super::PanelPlacement::LeftDock { .. })
        );
        scrolled || is_dock
    }

    /// Try to start a floating-panel list scrollbar drag. Returns
    /// true if the press landed on a scrollbar track (so the caller
    /// skips row hit-testing — the bar overlaps the list's rightmost
    /// column). Reuses the canonical `ScrollbarMouse`/`ScrollbarState`.
    fn try_widget_scrollbar_press(&mut self, slot: super::PanelSlot, col: u16, row: u16) -> bool {
        use crate::view::ui::scrollbar::ScrollbarState;
        let (panel_key, tracks) = match self.panel(slot) {
            Some(fwp) => (fwp.panel_key.clone(), fwp.scrollbar_tracks.clone()),
            None => return false,
        };
        for t in &tracks {
            let state = ScrollbarState::new(t.total, t.visible, t.scroll);
            let pressed = self
                .panel_mut(slot)
                .and_then(|fwp| fwp.scrollbar_mouse.press(state, t.rect, col, row));
            if let Some(new_offset) = pressed {
                if let Some(fwp) = self.panel_mut(slot) {
                    fwp.scrollbar_drag_key = Some(t.list_key.clone());
                }
                self.apply_widget_scroll(&panel_key, &t.list_key, new_offset, t.visible);
                return true;
            }
        }
        false
    }

    /// Continue an in-flight floating-panel scrollbar drag. Returns
    /// true if a drag is active (the press captured a `list_key`).
    fn try_widget_scrollbar_drag(&mut self, slot: super::PanelSlot, row: u16) -> bool {
        use crate::view::ui::scrollbar::ScrollbarState;
        let (panel_key, key) = match self.panel(slot) {
            Some(fwp) => match &fwp.scrollbar_drag_key {
                Some(k) => (fwp.panel_key.clone(), k.clone()),
                None => return false,
            },
            None => return false,
        };
        // The track geometry for the dragged list (its rect may have
        // shifted if the panel re-rendered between events).
        let track = self.panel(slot).and_then(|fwp| {
            fwp.scrollbar_tracks
                .iter()
                .find(|t| t.list_key == key)
                .cloned()
        });
        let Some(t) = track else {
            return false;
        };
        let state = ScrollbarState::new(t.total, t.visible, t.scroll);
        let new_offset = self
            .panel_mut(slot)
            .and_then(|fwp| fwp.scrollbar_mouse.drag(state, t.rect, row));
        if let Some(off) = new_offset {
            self.apply_widget_scroll(&panel_key, &key, off, t.visible);
        }
        true
    }

    /// End any in-flight floating-panel scrollbar drag.
    pub(super) fn release_widget_scrollbar(&mut self) {
        for fwp in [self.dock.as_mut(), self.floating_widget_panel.as_mut()]
            .into_iter()
            .flatten()
        {
            fwp.scrollbar_mouse.release();
            fwp.scrollbar_drag_key = None;
        }
    }

    /// Apply a host-driven scroll to a panel list (scrollbar press /
    /// drag): update the registry's instance state, re-render, and —
    /// when the list has a live selection that moved into the new
    /// window — notify the plugin so its own selection mirror +
    /// preview stay in sync with the thumb.
    fn apply_widget_scroll(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        list_key: &str,
        new_offset: usize,
        visible: usize,
    ) {
        let moved_sel = self.widget_registry.set_list_scroll(
            panel_key,
            list_key,
            new_offset as u32,
            visible as u32,
        );
        self.rerender_widget_panel(panel_key);
        if let Some(sel) = moved_sel {
            self.fire_widget_event(
                panel_key,
                list_key.to_string(),
                "select".to_string(),
                serde_json::json!({ "index": sel as i64 }),
            );
        }
    }

    /// Right-click hit-test against a floating widget panel. Resolves the
    /// cell under the cursor to a widget and — only when it lands on a
    /// `list` row — fires a `widget_event` with `event_type: "context"`
    /// (carrying the same `{ index, key, list_key }` payload a left-click
    /// "select" would). Plugins use this to raise a context menu for the
    /// right-clicked row. Returns `true` when a context event fired (so the
    /// caller swallows the click). Clicks on non-list widgets, padding, or
    /// outside the inner rect return `false`.
    fn handle_floating_widget_context_click(
        &mut self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
    ) -> bool {
        let (panel_key, inner) = match self.panel(slot) {
            Some(fwp) => match fwp.last_inner_rect {
                Some(rect) => (fwp.panel_key.clone(), rect),
                None => return false,
            },
            None => return false,
        };
        if col < inner.x || col >= inner.x + inner.width {
            return false;
        }
        if row < inner.y || row >= inner.y + inner.height {
            return false;
        }
        let brow = (row - inner.y) as u32;
        let entries = self
            .panel(slot)
            .map(|f| f.entries.clone())
            .unwrap_or_default();
        let local_screen_col = (col - inner.x) as usize;
        let bcol = match entries.get(brow as usize) {
            Some(entry) => screen_col_to_byte(&entry.text, local_screen_col),
            None => return false,
        };
        let (mut payload, key, kind) =
            match self
                .widget_registry
                .hit_test(slot.buffer_id(), brow, bcol as u32)
            {
                Some((_, hit)) => (hit.payload.clone(), hit.widget_key.clone(), hit.widget_kind),
                None => return false,
            };
        // A context menu only makes sense over a real list row.
        if kind != "list" {
            return false;
        }
        // Carry the screen cell so the plugin can anchor its popup at the
        // click (the list `select` payload only has the row index).
        if let Some(obj) = payload.as_object_mut() {
            obj.insert("col".to_string(), serde_json::json!(col));
            obj.insert("row".to_string(), serde_json::json!(row));
        }
        if !self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            return false;
        }
        self.fire_widget_event(&panel_key, key, "context".to_string(), payload);
        true
    }

    /// True when the centered (`Floating`) slot currently holds an
    /// anchored context-menu popup rather than a centered modal.
    fn floating_panel_is_anchored(&self) -> bool {
        matches!(
            self.floating_widget_panel.as_ref().map(|f| f.placement),
            Some(super::PanelPlacement::Anchored { .. })
        )
    }

    /// True when `(col, row)` falls within the panel's drawn box — the
    /// last-rendered inner rect grown by its 1-cell border. False when the
    /// panel or its rect is absent.
    fn point_in_floating_panel(&self, slot: super::PanelSlot, col: u16, row: u16) -> bool {
        let Some(inner) = self.panel(slot).and_then(|f| f.last_inner_rect) else {
            return false;
        };
        let x0 = inner.x.saturating_sub(1);
        let y0 = inner.y.saturating_sub(1);
        // inner.{x,y} + {width,height} already lands on the far border cell.
        col >= x0 && col <= inner.x + inner.width && row >= y0 && row <= inner.y + inner.height
    }

    /// Unmount the floating panel and fire a `cancel` widget_event so the
    /// owning plugin clears its state — the click-outside analogue of the
    /// Esc dismissal in `dispatch_floating_widget_key`.
    fn dismiss_floating_panel_with_cancel(&mut self, slot: super::PanelSlot) {
        let panel_key = match self.panel(slot) {
            Some(f) => f.panel_key.clone(),
            None => return,
        };
        let widget_key = self
            .widget_registry
            .get(&panel_key)
            .map(|p| p.focus_key.clone())
            .unwrap_or_default();
        self.fire_widget_event(
            &panel_key,
            widget_key,
            "cancel".to_string(),
            serde_json::json!({}),
        );
        *self.panel_opt_mut(slot) = None;
        let _ = self.widget_registry.unmount(&panel_key);
    }

    /// `handle_editor_click` uses; clicks outside the rect are
    /// swallowed without dismissing the panel.
    fn handle_floating_widget_click(&mut self, slot: super::PanelSlot, col: u16, row: u16) {
        // Scrollbar press wins over row hit-testing (the bar overlaps
        // the list's rightmost column).
        if self.try_widget_scrollbar_press(slot, col, row) {
            return;
        }
        let (panel_key, inner) = match self.panel(slot) {
            Some(fwp) => match fwp.last_inner_rect {
                Some(rect) => (fwp.panel_key.clone(), rect),
                None => return,
            },
            None => return,
        };
        if col < inner.x || col >= inner.x + inner.width {
            return;
        }
        if row < inner.y || row >= inner.y + inner.height {
            return;
        }
        let brow = (row - inner.y) as u32;
        let entries = self
            .panel(slot)
            .map(|f| f.entries.clone())
            .unwrap_or_default();
        let local_screen_col = (col - inner.x) as usize;
        let bcol = match entries.get(brow as usize) {
            Some(entry) => screen_col_to_byte(&entry.text, local_screen_col),
            None => return,
        };
        let (mut hit_payload, hit_event, hit_key, hit_kind) =
            match self
                .widget_registry
                .hit_test(slot.buffer_id(), brow, bcol as u32)
            {
                Some((_, hit)) => (
                    hit.payload.clone(),
                    hit.event_type.to_string(),
                    hit.widget_key.clone(),
                    hit.widget_kind,
                ),
                None => {
                    tracing::debug!(
                        target: "fresh::dock",
                        ?slot, col, row, brow, bcol,
                        "handle_floating_widget_click: hit_test found no widget"
                    );
                    return;
                }
            };
        if !hit_key.is_empty() {
            let tabbable = self
                .widget_registry
                .get(&panel_key)
                .map(|p| p.tabbable.iter().any(|k| k == &hit_key))
                .unwrap_or(false);
            tracing::debug!(
                target: "fresh::dock",
                hit_key = %hit_key,
                hit_kind,
                hit_event = %hit_event,
                tabbable,
                "handle_floating_widget_click: hit"
            );
            if tabbable {
                self.set_panel_focus_and_notify(&panel_key, hit_key.clone());
            }
            self.rerender_widget_panel(&panel_key);
        } else {
            tracing::debug!(
                target: "fresh::dock",
                hit_kind,
                hit_event = %hit_event,
                "handle_floating_widget_click: hit with empty key (not focusable)"
            );
        }
        let handled_specially = if hit_kind == "tree" && hit_event == "expand" {
            if let Some(item_key) = hit_payload.get("key").and_then(|v| v.as_str()) {
                self.handle_widget_tree_expand_toggle(&panel_key, &hit_key, item_key);
                true
            } else {
                false
            }
        } else {
            false
        };
        if !handled_specially {
            // Tag the event as mouse-originated. Keyboard nav (arrows)
            // fires `select` through `handle_widget_command` *without* this
            // marker, so a plugin can tell a click apart from an arrow-move
            // that happens to emit the same event/payload — e.g. the
            // orchestrator dock opens an inactive on-disk worktree on
            // *click* but not when you merely arrow past it.
            if let Some(obj) = hit_payload.as_object_mut() {
                obj.insert("via".to_string(), serde_json::json!("click"));
            }
            self.fire_widget_event(&panel_key, hit_key, hit_event, hit_payload);
        }
    }

    /// Clear all in-progress drag state on the active window's mouse state.
    /// The active text/popup selection is intentionally preserved — only the
    /// drag bookkeeping fields are reset.
    fn clear_active_window_drag_state(&mut self) {
        let ms = &mut self.active_window_mut().mouse_state;
        ms.dragging_scrollbar = None;
        ms.drag_start_row = None;
        ms.drag_start_top_byte = None;
        ms.dragging_horizontal_scrollbar = None;
        ms.drag_start_hcol = None;
        ms.drag_start_left_column = None;
        ms.dragging_separator = None;
        ms.drag_start_position = None;
        ms.drag_start_ratio = None;
        ms.dragging_file_explorer = false;
        ms.drag_start_explorer_width = None;
        ms.dragging_text_selection = false;
        ms.drag_selection_split = None;
        ms.drag_selection_anchor = None;
        ms.drag_selection_by_words = false;
        ms.drag_selection_word_end = None;
        ms.dragging_popup_scrollbar = None;
        ms.drag_start_popup_scroll = None;
        ms.dragging_prompt_scrollbar = false;
        ms.selecting_in_popup = None;
    }
}

/// Translate a display-column offset within a rendered line into a
/// UTF-8 byte offset inside the entry's text. Walks the string
/// codepoint-by-codepoint using `unicode-width` so wide glyphs
/// (CJK, emoji) advance by their actual screen width.
fn screen_col_to_byte(text: &str, target_col: usize) -> usize {
    use unicode_width::UnicodeWidthChar;
    let mut byte = 0;
    let mut col = 0usize;
    for ch in text.chars() {
        let w = UnicodeWidthChar::width(ch).unwrap_or(0);
        if col + w > target_col {
            return byte;
        }
        col += w;
        byte += ch.len_utf8();
    }
    byte
}
