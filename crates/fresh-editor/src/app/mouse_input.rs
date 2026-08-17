//! Mouse input handling.
//!
//! This module contains all mouse event handling logic including:
//! - Click, double-click, and drag handling
//! - Scrollbar interaction
//! - Hover target computation
//! - Split separator dragging
//! - Text selection via mouse

use super::chrome::in_rect;
use super::*;
use crate::model::event::{ContainerId, CursorId, SplitDirection};
use crate::services::plugins::hooks::HookArgs;
use crate::view::popup_mouse::{popup_areas_to_layout_info, PopupHitTester};
use crate::view::prompt::{PromptType, MAX_VISIBLE_SUGGESTIONS};
use anyhow::Result as AnyhowResult;
use ratatui::layout::Rect;

/// Map a screen row on a suggestion list's scrollbar track to the prompt
/// scroll offset that puts the thumb's top on exactly that row.
///
/// Shared by the press and the drag-follow-up so the thumb tracks the cursor
/// identically in both. [`ScrollbarState::offset_for_thumb_top`] is the real
/// inverse of the thumb geometry the renderer draws — the ONE track mapping
/// (its off-by-a-row `click_to_offset` sibling is deleted).
///
/// Rows above/below the track clamp to its ends rather than being rejected,
/// so a fast drag doesn't drop the thumb.
pub(super) fn prompt_scrollbar_offset_for_row(
    total: usize,
    visible: usize,
    scroll_offset: usize,
    sb_rect: Rect,
    row: u16,
) -> usize {
    use crate::view::ui::scrollbar::ScrollbarState;
    let clamped_row = row.clamp(sb_rect.y, sb_rect.y + sb_rect.height.saturating_sub(1));
    let track_row = clamped_row.saturating_sub(sb_rect.y) as usize;
    ScrollbarState::new(total, visible, scroll_offset)
        .offset_for_thumb_top(sb_rect.height as usize, track_row)
}

impl Editor {
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

        // Modal mouse-capture, offered in RANK order over the derived
        // overlay stack: the first component whose modal surface is up
        // claims the whole mouse channel. Every capturing component
        // declares a layer from the same activity predicate its
        // capture gates on, so walking the owner-stamped stack visits
        // exactly the capturing candidates — and deletes the old
        // registry-order duplicate of the precedence (two hand-synced
        // encodings, comment-only sync). Rank IS the one source now,
        // for the keyboard walk and the capture band alike.
        {
            let stack = self.overlay_stack();
            let mut seen = std::collections::HashSet::new();
            for entry in &stack {
                // The hardcoded event-debug head has no owner; a
                // component contributing several layers is offered
                // the capture once, at its highest rank.
                let Some(owner) = entry.owner else { continue };
                if !seen.insert(owner) {
                    continue;
                }
                if let Some(result) = super::chrome::components()[owner].capture_mouse(
                    self,
                    mouse_event,
                    is_double_click,
                ) {
                    return result;
                }
            }
        }

        // Cancel the LSP-rename prompt on ANY mouse interaction.
        // RULING — pre-band whole-channel observer, the mouse analogue
        // of the keyboard's transient-popup dismissal: it must fire on
        // every event kind (click, wheel, even bare motion) wherever it
        // lands, which no box on the walk can express (a box fires only
        // when hit, and only for gestures with arms). It acts then
        // continues — routing proceeds to capture/walk as if it weren't
        // here. The `prompt_type` match is the observer's own gate, not
        // surface routing.
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
        let chrome_drag_active = super::chrome::pointer_grab(self).is_some();
        // An open native context menu (tab / "+" new-tab / file-explorer)
        // takes mouse precedence over terminal forwarding. These menus render
        // on top of — and frequently overlap — an alternate-screen terminal
        // that has captured the mouse (e.g. right-clicking a terminal's tab
        // opens the tab menu directly over the terminal's content). Without
        // this gate the terminal-forward path below would swallow clicks/moves
        // aimed at the menu, so menu items couldn't be selected (they'd inject
        // mouse escape codes into the PTY instead). Skipping forwarding lets
        // the event fall through to the normal pipeline, where
        // `handle_click_context_menus` (select / dismiss) and the hover
        // hit-test (highlight-follows-pointer) already handle it. The menu's
        // precedence itself lives in the chrome walk/capture ordering (its
        // boxes ride the top routable band); this fork only keeps the PTY
        // from swallowing the events first.
        let context_menu_open = self.active_window().context_menu_core().is_some();
        // DERIVED suppression for everything with opaque geometry: a
        // pointer-opaque chrome box over the cell (an info popup, the
        // suggestions dropdown, the theme-info popup) must take the
        // event in the walk — forwarding it would inject mouse codes
        // into the PTY *through* the popup. This replaces growing the
        // hand list one surface at a time; the context-menu check
        // above stays NAMED by ruling because its boxes are
        // deliberately not opaque (its close-guard backdrop owns
        // outside clicks), so opacity cannot express it.
        let opaque_chrome_over_point = {
            let tree = super::chrome::chrome_tree(self);
            crate::widgets::layout_box::hit_stack(&tree, row as u32, col as u32)
                .into_iter()
                .any(|i| tree[i].lb.pointer_opaque)
        };
        if !chrome_drag_active && !context_menu_open && !opaque_chrome_over_point {
            let forwarding = self.config.terminal.mouse_forwarding;
            if let Some(result) = self.active_window_mut().try_forward_mouse_to_terminal(
                col,
                row,
                mouse_event,
                forwarding,
            ) {
                return result;
            }
        }

        // Ctrl+Click on a file path printed in the live terminal opens it in
        // Fresh (jumping to any :line:col it encodes). Handled before normal
        // click routing so it doesn't disturb cursor/selection state.
        if let Some(result) = self.try_open_terminal_link(col, row, mouse_event) {
            return result;
        }

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                // NOTE: the fold-toggle double/triple check lives in
                // `Splits::on_pointer`'s Double/Triple arm — inside the
                // walk, so a popup's opaque box or the overlay prompt's
                // swallow blocks it by construction (it used to sit
                // here pre-walk, hit-testing `split_areas` directly and
                // bypassing every guard the walk enforces).
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
                // Release is GRAB-KEYED like the Drag arm: the derived
                // `pointer_grab` names which press-to-release routing is
                // ending, and its arm runs that grab's finalizer — no
                // more per-surface field-poke ladder that had to be kept
                // in sync with the grab roster by hand. Grabs without a
                // finalizer just fall to the blanket clear below.
                let grab = super::chrome::pointer_grab(self);
                match grab {
                    // End a dock-resize drag and persist the chosen
                    // width so it survives toggling the dock off/on.
                    Some(super::chrome::PointerGrab::DockResize) => {
                        self.dock_resizing = false;
                        if let Some(super::PanelPlacement::LeftDock { width_cols }) =
                            self.dock.as_ref().map(|f| f.placement)
                        {
                            self.dock_width = Some(width_cols);
                        }
                        return Ok(true);
                    }
                    // Complete a tab drop before the drag state clears.
                    Some(super::chrome::PointerGrab::TabDrag) => {
                        if let Some(drag_state) =
                            self.active_window_mut().mouse_state.dragging_tab.take()
                        {
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
                    }
                    _ => {}
                }

                // Blanket sweep: every remaining drag flag drops here,
                // so no grab can outlive its release even if its
                // finalizer above was skipped.
                self.release_widget_scrollbar();
                self.widget_text_drag = None;
                self.clear_active_window_drag_state();

                // A finished split-separator drag changed the ratios:
                // reflow through the single layout funnel (after the
                // sweep, as before).
                if matches!(grab, Some(super::chrome::PointerGrab::SplitSeparator)) {
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
                // underlines it to signal it's clickable. RULING: stays
                // beside (not inside) the `HoverTarget` walk, like its
                // click half stays pre-walk — the tracker is a
                // modifier-keyed regex probe over terminal-grid CONTENT,
                // not a surface-naming question; the walk names chrome,
                // content trackers own their reactions (the same seam as
                // `update_lsp_hover_state` below).
                let term_link_changed =
                    self.update_terminal_link_hover(col, row, mouse_event.modifiers);
                needs_render = needs_render || term_link_changed;

                // Track LSP hover state for mouse-triggered hover popups
                self.update_lsp_hover_state(col, row);

                // Bare icon buttons inside a panel (the dock's `×`) light up
                // under the pointer, the way the tab and file explorer `×`
                // do. Tracked off the same motion events as the dock's
                // scrollbar reveal (`Dock::on_hover_change`), and likewise
                // re-rendering only on the enter/leave transition.
                needs_render = self.update_widget_hover(col, row, None) || needs_render;
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
                self.handle_horizontal_scroll(col, row, -3)?;
                needs_render = true;
            }
            MouseEventKind::ScrollRight => {
                // Native horizontal scroll right
                self.handle_horizontal_scroll(col, row, 3)?;
                needs_render = true;
            }
            MouseEventKind::Down(MouseButton::Right) => {
                // One walk for every right-click flavor: the overlay
                // prompt's guard box swallows (mouse-modal), the theme
                // inspector's trigger claims Ctrl+Right-Click, and the
                // routable surfaces below take plain right-clicks.
                self.handle_right_click(col, row, mouse_event.modifiers)?;
                needs_render = true;
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

    /// Dispatch a vertical scroll event (ScrollUp/ScrollDown): Shift
    /// pans horizontally; otherwise the wheel scans the chrome tree
    /// top-down (`hit_stack`), offering each box to its owning
    /// component until one consumes — surfaces with no wheel handler
    /// decline, so the wheel keeps falling (scroll chaining) down to
    /// the `chrome:base` fallback.
    /// THE wheel dispatch engine — one walk for both axes. Build the
    /// per-event chrome tree, scan the boxes under the point top-down
    /// (`hit_stack`), offer the delta to each box's owning component
    /// (`on_wheel` / `on_hwheel` by axis) until one consumes.
    /// Deliberately NO opacity gate: wheel chains through declining
    /// surfaces (scroll chaining). Adding a scroll surface never
    /// touches this — write a component, register it, contribute
    /// boxes.
    fn dispatch_wheel(
        &mut self,
        horizontal: bool,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        let tree = super::chrome::chrome_tree(self);
        for i in crate::widgets::layout_box::hit_stack(&tree, row as u32, col as u32) {
            let b = &tree[i];
            let c = super::chrome::components()[b.owner];
            let disp = if horizontal {
                c.on_hwheel(self, &b.lb, col, row, delta)?
            } else {
                c.on_wheel(self, &b.lb, col, row, delta)?
            };
            match disp {
                super::chrome::Disposition::Consumed => return Ok(()),
                super::chrome::Disposition::PassAfter | super::chrome::Disposition::Pass => {}
            }
        }
        Ok(())
    }

    fn handle_vertical_scroll(
        &mut self,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
        delta: i32,
    ) -> AnyhowResult<()> {
        // Shift turns the wheel horizontal (same engine, other axis).
        let horizontal = modifiers.contains(crossterm::event::KeyModifiers::SHIFT);
        self.dispatch_wheel(horizontal, col, row, delta)
    }

    /// Route a horizontal scroll (Shift+wheel, native ScrollLeft /
    /// ScrollRight) through the SAME engine as every other gesture —
    /// surfaces with a horizontal axis (split panes, tab strips)
    /// claim their boxes; everything else declines and the base
    /// drops it.
    pub(super) fn handle_horizontal_scroll(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        self.dispatch_wheel(true, col, row, delta)
    }

    /// Update the current hover target based on mouse position.
    /// Returns true if a re-render is needed. This is the generic
    /// engine only: walk the tree for the new target, diff, store,
    /// then offer the transition to every registered component
    /// (`on_hover_change`) — the per-surface hover REACTIONS (menu
    /// auto-switch/submenu machine, context-menu highlight, explorer
    /// tooltip) live with their components, not here.
    pub(super) fn update_hover_target(&mut self, col: u16, row: u16) -> bool {
        let old_target = self.active_window_mut().mouse_state.hover_target.clone();
        let new_target = self.compute_hover_target(col, row);
        let mut needs_render = old_target != new_target;
        self.active_window_mut().mouse_state.hover_target = new_target.clone();
        for c in super::chrome::components() {
            needs_render |=
                c.on_hover_change(self, old_target.as_ref(), new_target.as_ref(), col, row);
        }
        needs_render
    }

    /// Update LSP hover state based on mouse position
    /// Tracks position for debounced hover requests
    ///
    /// Hover popup stays visible when:
    /// - Mouse is over the hover popup itself
    /// - Mouse is within the hovered symbol range
    ///
    /// Hover is dismissed when mouse leaves the editor area entirely.
    ///
    /// RULING — this pipeline stays OUTSIDE the `HoverTarget` walk: it
    /// is not a "name the surface under the pointer" question but a
    /// debounced request state machine over BUFFER content (symbol
    /// ranges, popup keep-alive, request dedup) whose transitions the
    /// walk's enter/leave diff cannot express. It composes with the
    /// walk the same way `update_terminal_link_hover` does: the walk
    /// names chrome, these trackers own editor-content reactions.
    /// Folding it in is recorded in the plan doc as part of the
    /// mounted-panel/hover unification arc, not chrome registration.
    fn update_lsp_hover_state(&mut self, col: u16, row: u16) {
        tracing::trace!(col, row, "update_lsp_hover_state: raw mouse position");

        // Suppress LSP hover when a popup is already visible (the theme
        // info popup or the status-bar LSP status popup — both hand
        // -listed because neither declares an overlay layer) to avoid
        // hover tooltips overlapping other popups. Same for any modal
        // overlay (Open File dialog, command palette, menu, native
        // context menus, …), all DERIVED from `modal_overlay_active`:
        // mouse positions over the overlay map to the buffer *behind*
        // it, so tracking them would fire hover requests for invisible
        // content and render the popup on top of the dialog
        // (sinelaw/fresh#2912). (An open context menu used to be a
        // third hand-listed check here; its ContextMenu layer already
        // makes `modal_overlay_active` true, so the check was a
        // redundant second encoding.)
        if self.active_window_mut().theme_info_popup.is_some()
            || self.is_lsp_status_popup_open()
            || self.modal_overlay_active()
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

        // Check if we're still hovering the same position in the same buffer
        if let Some((old_pos, _, _, _, old_buf)) =
            self.active_window_mut().mouse_state.lsp_hover_state
        {
            if old_pos == byte_pos && old_buf == buffer_id {
                // Same position - keep existing state
                return;
            }
            // Position changed outside the hovered symbol range. Don't dismiss
            // the popup here: a new hover request will fire after the debounce
            // and replace the popup naturally if the mouse settles on another
            // symbol. Dismissing eagerly tore the popup down whenever the
            // mouse passed through whitespace between two words (issue #692).
        }

        // Start tracking new hover position (remembering which buffer the
        // pointer is over, so the request targets that buffer — not the
        // active one — see `lsp_hover_state`).
        self.active_window_mut().mouse_state.lsp_hover_state =
            Some((byte_pos, std::time::Instant::now(), col, row, buffer_id));
        self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
    }

    /// Check if mouse position is over a transient popup (hover, signature help)
    fn is_mouse_over_transient_popup(&self, col: u16, row: u16) -> bool {
        let layouts = popup_areas_to_layout_info(&self.active_chrome().popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_transient_popup(col, row)
    }

    /// Check if mouse position is over any popup (including non-transient ones like completion)
    pub(super) fn is_mouse_over_any_popup(&self, col: u16, row: u16) -> bool {
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
    pub(super) fn is_mouse_over_file_browser(&self, col: u16, row: u16) -> bool {
        self.active_window()
            .file_browser_layout
            .as_ref()
            .is_some_and(|layout| layout.contains(col, row))
    }

    // `split_at_position` lives on `impl Window` — call it via
    // `self.active_window().split_at_position(col, row)`.

    /// Compute what hover target is at the given position
    fn compute_hover_target(&mut self, col: u16, row: u16) -> Option<HoverTarget> {
        // The hover surfaces, as chrome boxes — the same geometric walk
        // as wheel/click/right-click/double-click, in query form: the
        // highest-z box whose handler names a target wins, and handlers
        // whose geometry is finer than their rectangle (context-menu
        // borders, tab-bar background) decline so the point falls
        // through to the boxes below.
        let tree = super::chrome::chrome_tree(self);
        for i in crate::widgets::layout_box::hit_stack(&tree, row as u32, col as u32) {
            let b = &tree[i];
            if let Some(t) = super::chrome::components()[b.owner].hover(self, &b.lb, col, row) {
                return Some(t);
            }
            // Opacity gate: a declining opaque surface (a popup) stops
            // the scan — nothing beneath it is hoverable through it.
            if b.lb.pointer_opaque {
                return None;
            }
        }
        None
    }

    /// Handle mouse double click (down event)
    /// Double-click in editor area selects the word under the cursor:
    /// the suggestion-confirm (#1660), overlay swallow, popup
    /// block/dismiss guard, file-open dialog, explorer body, and the
    /// split word-select arm are all component arms in the engine's
    /// one scan — no post-walk special cases.
    pub(super) fn handle_mouse_double_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        self.dispatch_pointer(
            super::chrome::PointerPress::Double,
            col,
            row,
            crossterm::event::KeyModifiers::empty(),
        )
    }

    /// Handle mouse triple click (down event)
    /// Triple-click in editor area selects the entire line under the
    /// cursor — same engine, same arms (the Splits line-select arm
    /// takes what the overlay/popup guards let through).
    pub(super) fn handle_mouse_triple_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        self.dispatch_pointer(
            super::chrome::PointerPress::Triple,
            col,
            row,
            crossterm::event::KeyModifiers::empty(),
        )
    }

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

    /// THE pointer dispatch engine — ONE walk for every press kind
    /// (left, right, double, triple). Build the per-event chrome
    /// tree, scan the boxes under the point top-down (`hit_stack`),
    /// offer the press to each box's owning component, and honor the
    /// dispositions: `Consumed` stops the walk, `PassAfter` acts then
    /// continues (guards), and a DECLINED opaque box absorbs the
    /// press (nothing routes through a popup). Multi-box surfaces
    /// (one box per popup / dropdown level) are dispatched once per
    /// surface kind — their handlers resolve by position over the
    /// whole collection. Adding a chrome surface never touches this
    /// engine: write a component, register it, contribute boxes.
    fn dispatch_pointer(
        &mut self,
        press: super::chrome::PointerPress,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        let tree = super::chrome::chrome_tree(self);
        let mut seen = std::collections::HashSet::new();
        for i in crate::widgets::layout_box::hit_stack(&tree, row as u32, col as u32) {
            let b = &tree[i];
            if !seen.insert(b.lb.kind) {
                continue;
            }
            let ev = super::chrome::ChromePointer {
                press,
                col,
                row,
                modifiers,
            };
            match super::chrome::components()[b.owner].on_pointer(self, &b.lb, &ev)? {
                super::chrome::Disposition::Consumed => return Ok(()),
                super::chrome::Disposition::PassAfter => {}
                super::chrome::Disposition::Pass => {
                    if b.lb.pointer_opaque {
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn handle_mouse_click(
        &mut self,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        // (The centered modal's precedence over everything here is the
        // FloatingModal component's whole-channel capture — this path
        // is unreachable while it is up. Dock routing — column clicks,
        // the resize-border grab, blur-on-outside — is the Dock
        // component's boxes and arms in the engine's scan.)
        self.dispatch_pointer(super::chrome::PointerPress::Left, col, row, modifiers)
    }

    /// Handle mouse drag event
    pub(super) fn handle_mouse_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        use super::chrome::PointerGrab;
        // THE grab slot: the press-to-release owner derived from live
        // drag state (`chrome::pointer_grab`) routes every motion —
        // no re-hit-testing mid-drag (the btop-resize ruling), no
        // hand-ordered flag ladder. `pointer_grab`'s check order
        // preserves the old ladder's precedence.
        let Some(grab) = super::chrome::pointer_grab(self) else {
            return Ok(());
        };
        // Mouse-modal overlay: the only legitimate drags are the
        // overlay's own result-list scrollbar and the grabs the old
        // ladder ran ahead of the swallow (dock resize, widget text,
        // widget scrollbar). Anything else — text selection in the
        // buffer, a buffer scrollbar behind the overlay — is
        // swallowed so the buffer stays put.
        if self.overlay_prompt_active()
            && !matches!(
                grab,
                PointerGrab::PromptScrollbar
                    | PointerGrab::DockResize
                    | PointerGrab::WidgetText
                    | PointerGrab::WidgetScrollbar
            )
        {
            return Ok(());
        }
        match grab {
            // Dock resize drag: track the pointer column as the new dock
            // width (the right border follows the cursor), clamped so it
            // can't swallow the chrome.
            PointerGrab::DockResize => {
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
            }
            // Drag-to-select on a widget markdown/text document: armed by the
            // press that placed the caret; every Drag extends the selection to
            // the pointer.
            PointerGrab::WidgetText => {
                self.handle_widget_text_selection_drag(col, row);
            }
            // Floating-panel list scrollbar drag — the modal panel
            // owns the input channel while it's up.
            PointerGrab::WidgetScrollbar => {
                let _ = self.try_widget_scrollbar_drag(super::PanelSlot::Dock, row)
                    || self.try_widget_scrollbar_drag(super::PanelSlot::Floating, row);
            }
            // Vertical scrollbar drag: update scroll position.
            PointerGrab::VScrollbar => {
                if let Some(dragging_split_id) =
                    self.active_window_mut().mouse_state.dragging_scrollbar
                {
                    // Snapshot split_areas so we don't borrow `self.active_layout()` and
                    // `self.active_window_mut()` simultaneously below.
                    let split_areas = self.active_layout().split_areas.clone();
                    for (
                        split_id,
                        buffer_id,
                        _content_rect,
                        scrollbar_rect,
                        _thumb_start,
                        _thumb_end,
                    ) in &split_areas
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
            }
            // Horizontal scrollbar drag: update horizontal scroll position.
            PointerGrab::HScrollbar => {
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
                                    let max_scroll =
                                        max_content_width.saturating_sub(visible_width);
                                    if max_scroll > 0 {
                                        let thumb_size =
                                            thumb_end.saturating_sub(*thumb_start).max(1);
                                        let track_travel =
                                            (track_width - thumb_size as f64).max(1.0);
                                        let scroll_per_pixel = max_scroll as f64 / track_travel;
                                        let scroll_offset =
                                            (col_offset as f64 * scroll_per_pixel).round() as i64;
                                        let new_left =
                                            (drag_start_left_column as i64 + scroll_offset).max(0)
                                                as usize;
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
                                    let max_scroll =
                                        max_content_width.saturating_sub(visible_width);
                                    let target_col = (ratio * max_scroll as f64).round() as usize;
                                    view_state.viewport.left_column = target_col.min(max_scroll);
                                    view_state.viewport.set_skip_ensure_visible();
                                }
                            }

                            return Ok(());
                        }
                    }
                }
            }
            // Selecting text in an info popup: extend the selection.
            PointerGrab::PopupSelect => {
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
                }
            }
            // The floating-overlay prompt's scrollbar (issue #1796):
            // update its scroll_offset using the same math as the
            // click handler. Same shared-widget logic the
            // popup-scrollbar drag uses below.
            PointerGrab::PromptScrollbar => {
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
                    let visible = suggestions_area_visible
                        .unwrap_or_else(|| prompt.suggestions.len().min(MAX_VISIBLE_SUGGESTIONS));
                    prompt.scroll_offset = prompt_scrollbar_offset_for_row(
                        prompt.suggestions.len(),
                        visible,
                        prompt.scroll_offset,
                        sb_rect,
                        row,
                    );
                    // Keep the manual-scroll latch through the drag so the
                    // renderer doesn't pull the offset back to the selection.
                    prompt.manual_scroll = true;
                }
            }
            // A buffer popup's scrollbar: update its scroll position.
            PointerGrab::PopupScrollbar => {
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
                }
            }
            // Split-separator drag: update the split ratio.
            PointerGrab::SplitSeparator => {
                if let Some((split_id, direction)) =
                    self.active_window_mut().mouse_state.dragging_separator
                {
                    self.handle_separator_drag(col, row, split_id, direction)?;
                }
            }
            // File-explorer border drag: update its width.
            PointerGrab::ExplorerWidth => {
                self.handle_file_explorer_border_drag(col)?;
            }
            // A drag whose press landed on a live terminal grid: this is
            // selection intent (a bare click only focuses — see
            // `handle_editor_click`). Drop the split into read-only scrollback
            // and start a normal text-selection drag anchored at the press.
            PointerGrab::TerminalSelectPending => {
                if let Some((split_id, buffer_id, ocol, orow)) =
                    self.active_window().mouse_state.terminal_drag_pending
                {
                    self.begin_terminal_grid_selection(split_id, buffer_id, ocol, orow, col, row)?;
                }
            }
            // Text-selection drag: extend from the anchor.
            PointerGrab::TextSelection => {
                self.handle_text_selection_drag(col, row)?;
            }
            // Tab drag: update position and compute the drop zone.
            PointerGrab::TabDrag => {
                self.handle_tab_drag(col, row)?;
            }
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
            .map(|vs| vs.viewport.top_byte())
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

        // Terminal scrollback views are unwrapped and gutter-free, so a
        // screen cell maps to a byte exactly (viewport top line + row).
        // Resolve directly instead of through the render-cached view-line
        // mappings: drag events processed after the live-grid→scrollback
        // flip but before the next render would otherwise resolve against
        // mappings cached from a *previous* buffer view of this split and
        // land the selection head far from the pointer.
        let terminal_grid_target = if self.active_window().is_terminal_buffer(buffer_id)
            && self
                .active_window()
                .split_terminal_scrollback(leaf_id, buffer_id)
        {
            self.terminal_grid_byte_at(leaf_id, buffer_id, content_rect, col, row)
        } else {
            None
        };

        let Some((target_position, new_position, anchor_position, new_sticky_column)) = self
            .active_window()
            .buffers
            .get(&buffer_id)
            .and_then(|state| {
                let gutter_width = state.margins.left_total_width() as u16;
                let target_position = match terminal_grid_target {
                    Some(pos) => pos,
                    None => super::click_geometry::screen_to_buffer_position(
                        col,
                        row,
                        content_rect,
                        gutter_width,
                        &cached_mappings,
                        fallback,
                        true, // Allow gutter clicks for drag selection
                        compose_width,
                    )?,
                };
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
            // Store the raw fraction; the absolute minimum-pane-size guard is
            // enforced at layout time, so dragging the separator toward the
            // edge stops when the sibling would drop below the minimum size
            // rather than at a fixed 10%/90%.
            let new_ratio = (start_ratio + ratio_delta).clamp(0.0, 1.0);

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
                // Guarded by the `get_ratio(..).is_some()` check above, so
                // this id resolves to a resizable Split; the bool result is
                // not actionable here (the drag can only target a container).
                let _resized = self
                    .windows
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

    /// Handle right-click event — same engine. Ordering rides z; the
    /// anywhere-clears (tab "+" menu, close-split confirm) are the
    /// Splits component's top-band PassAfter guard, the overlay
    /// prompt's swallow and the theme inspector's Ctrl+Right trigger
    /// are boxes above the routable surfaces.
    pub(super) fn handle_right_click(
        &mut self,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        self.dispatch_pointer(super::chrome::PointerPress::Right, col, row, modifiers)
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
        ms.terminal_drag_pending = None;
        ms.dragging_popup_scrollbar = None;
        ms.dragging_prompt_scrollbar = false;
        ms.selecting_in_popup = None;
    }
}
