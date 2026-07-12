//! Mouse input handling for the Settings dialog.
//!
//! This module contains all mouse event handling for the settings modal,
//! including clicks, scrolling, and drag operations.

use crate::app::Editor;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use super::items::SettingControl;
use super::render::ControlLayoutInfo;
use super::{FocusPanel, SettingsHit, SettingsLayout};
use crate::view::controls::DualListColumn;

/// Computed layout for entry dialog hit testing
struct EntryDialogLayout {
    dialog_x: u16,
    dialog_y: u16,
    dialog_width: u16,
    dialog_height: u16,
    inner_x: u16,
    inner_y: u16,
    inner_width: u16,
    inner_height: u16,
    button_y: u16,
    scrollbar_x: u16,
}

impl EntryDialogLayout {
    /// Compute entry dialog layout from modal area
    fn from_modal(modal: ratatui::layout::Rect) -> Option<Self> {
        if modal.width == 0 || modal.height == 0 {
            return None;
        }

        let dialog_width = (modal.width * 85 / 100).clamp(50, 90);
        let dialog_height = (modal.height * 90 / 100).max(15);
        let dialog_x = modal.x + (modal.width.saturating_sub(dialog_width)) / 2;
        let dialog_y = modal.y + (modal.height.saturating_sub(dialog_height)) / 2;

        Some(Self {
            dialog_x,
            dialog_y,
            dialog_width,
            dialog_height,
            inner_x: dialog_x + 2,
            inner_y: dialog_y + 1,
            inner_width: dialog_width.saturating_sub(4),
            inner_height: dialog_height.saturating_sub(5),
            button_y: dialog_y + dialog_height - 2,
            scrollbar_x: dialog_x + dialog_width - 3,
        })
    }

    fn contains(&self, col: u16, row: u16) -> bool {
        col >= self.dialog_x
            && col < self.dialog_x + self.dialog_width
            && row >= self.dialog_y
            && row < self.dialog_y + self.dialog_height
    }

    fn in_content_area(&self, col: u16, row: u16) -> bool {
        col >= self.inner_x
            && col < self.inner_x + self.inner_width
            && row >= self.inner_y
            && row < self.inner_y + self.inner_height
    }

    fn near_scrollbar(&self, col: u16) -> bool {
        col >= self.scrollbar_x.saturating_sub(2) && col <= self.dialog_x + self.dialog_width
    }
}

impl Editor {
    /// Handle mouse events when settings modal is open.
    pub(crate) fn handle_settings_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
        is_double_click: bool,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        // When help overlay is open, consume all mouse events
        if let Some(ref state) = self.settings_state {
            if state.showing_help {
                return Ok(false);
            }
        }

        // Handle confirm dialog mouse events
        let showing_confirm = self
            .settings_state
            .as_ref()
            .map(|s| s.showing_confirm_dialog)
            .unwrap_or(false);
        if showing_confirm {
            match mouse_event.kind {
                MouseEventKind::Moved => {
                    let hover = self.get_confirm_dialog_button_at(col, row);
                    if let Some(ref mut state) = self.settings_state {
                        state.confirm_dialog_hover = hover;
                    }
                    return Ok(hover.is_some());
                }
                MouseEventKind::Down(MouseButton::Left) => {
                    return self.handle_confirm_dialog_click(col, row);
                }
                _ => {}
            }
            return Ok(false);
        }

        // Handle mouse events for entry dialog
        if let Some(ref mut state) = self.settings_state {
            if state.showing_entry_dialog() {
                match mouse_event.kind {
                    MouseEventKind::Moved => {
                        return Ok(self.entry_dialog_update_hover(col, row));
                    }
                    MouseEventKind::ScrollUp => {
                        if let Some(dialog) = state.entry_dialog_mut() {
                            dialog.scroll_up();
                            return Ok(true);
                        }
                    }
                    MouseEventKind::ScrollDown => {
                        if let Some(dialog) = state.entry_dialog_mut() {
                            dialog.scroll_down(20);
                            return Ok(true);
                        }
                    }
                    MouseEventKind::Drag(MouseButton::Left) => {
                        return Ok(self.entry_dialog_scrollbar_drag(col, row));
                    }
                    MouseEventKind::Down(MouseButton::Left) => {
                        return self.handle_entry_dialog_click(col, row, is_double_click);
                    }
                    _ => {}
                }
                return Ok(false);
            }
        }

        // Track hover position and compute hover hit for visual feedback
        match mouse_event.kind {
            MouseEventKind::Moved => {
                let hover_hit = self
                    .active_chrome()
                    .settings_layout
                    .as_ref()
                    .and_then(|layout: &SettingsLayout| layout.hit_test(col, row));

                if let Some(ref mut state) = self.settings_state {
                    let old_hit = state.hover_hit;
                    state.hover_position = Some((col, row));
                    state.hover_hit = hover_hit;

                    // Update dropdown hover index when hovering over options
                    let new_hover_idx = match hover_hit {
                        Some(SettingsHit::ControlDropdownOption(_, opt_idx)) => Some(opt_idx),
                        _ => None,
                    };
                    let hover_changed = state.set_dropdown_hover(new_hover_idx);

                    return Ok(old_hit != hover_hit || hover_changed);
                }
                return Ok(false);
            }
            MouseEventKind::ScrollUp => {
                // If a dropdown is open, forward scroll to it
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_scroll(-3);
                        return Ok(true);
                    }
                    // If search is active and we have results, scroll search results
                    if state.search_active && !state.search_results.is_empty() {
                        return Ok(state.search_scroll_up(3));
                    }
                }
                // Wheel over the categories panel scrolls it independently.
                if self.over_categories_panel(col, row) {
                    if let Some(ref mut state) = self.settings_state {
                        state.categories_scroll.scroll.scroll_by(-3);
                        return Ok(true);
                    }
                }
                return Ok(self.settings_scroll_up(3));
            }
            MouseEventKind::ScrollDown => {
                // If a dropdown is open, forward scroll to it
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_scroll(3);
                        return Ok(true);
                    }
                    // If search is active and we have results, scroll search results
                    if state.search_active && !state.search_results.is_empty() {
                        return Ok(state.search_scroll_down(3));
                    }
                }
                if self.over_categories_panel(col, row) {
                    if let Some(ref mut state) = self.settings_state {
                        state.categories_scroll.scroll.scroll_by(3);
                        return Ok(true);
                    }
                }
                return Ok(self.settings_scroll_down(3));
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                // Check if dragging on search scrollbar
                if let Some(ref mut state) = self.settings_state {
                    if state.search_active && !state.search_results.is_empty() {
                        if let Some(scrolled) = self.search_scrollbar_drag(col, row) {
                            return Ok(scrolled);
                        }
                    }
                }
                return Ok(self.settings_scrollbar_drag(col, row));
            }
            MouseEventKind::Down(MouseButton::Left) => {}
            _ => return Ok(false),
        }

        // Use cached settings layout for hit testing
        let Some(hit) = self
            .active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|layout: &SettingsLayout| layout.hit_test(col, row))
        else {
            return Ok(false);
        };

        // A click on a main-panel text field enters editing (via dispatch)
        // AND positions the caret where the click landed (#2573). The
        // geometry was stamped into the layout at render time; read it
        // here, where the screen `col` is still available (the web-shared
        // `dispatch_settings_hit` carries no column). Grab it before
        // dispatch, since dispatch re-borrows the layout.
        let text_click_geometry = match hit {
            SettingsHit::ControlText(idx) => self
                .active_chrome()
                .settings_layout
                .as_ref()
                .and_then(|l| l.items.iter().find(|it| it.index == idx))
                .and_then(|it| match &it.control {
                    ControlLayoutInfo::Text { geometry, .. } => geometry.clone(),
                    _ => None,
                }),
            _ => None,
        };

        self.dispatch_settings_hit(hit, row, is_double_click);

        // Position the caret after dispatch, which ran `start_editing` and
        // seeded the caret at end-of-value; the click position wins.
        if let Some(geometry) = text_click_geometry {
            let byte = geometry.value_byte_at(col);
            if let Some(state) = self.settings_state.as_mut() {
                state.position_text_cursor(byte);
            }
        }
        Ok(true)
    }

    /// Perform the action for a resolved `SettingsHit` — shared by the TUI mouse
    /// hit-test above and the web `/settings` route, which sends the hit it
    /// rendered natively. So a click does the same thing in both frontends.
    /// `row` is used only by the scrollbar hits (TUI-only; the web never sends
    /// them).
    pub(crate) fn dispatch_settings_hit(
        &mut self,
        hit: SettingsHit,
        row: u16,
        is_double_click: bool,
    ) {
        // If a dropdown is open and the click is outside it, cancel and stop.
        if let Some(ref mut state) = self.settings_state {
            if state.is_dropdown_open() {
                let is_click_on_open_dropdown = matches!(
                    hit,
                    SettingsHit::ControlDropdown(idx) | SettingsHit::ControlDropdownOption(idx, _)
                        if idx == state.selected_item
                );
                if !is_click_on_open_dropdown {
                    state.dropdown_cancel();
                    return;
                }
            }
        }

        match hit {
            SettingsHit::Outside | SettingsHit::Background | SettingsHit::SettingsPanel => {}
            SettingsHit::Category(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Categories);
                    state.selected_category = idx;
                    state.selected_item = 0;
                    state.scroll_panel = crate::view::ui::ScrollablePanel::new();
                    state.sub_focus = None;
                    // Click lands the cursor on the category row itself
                    // (not on a section), even after auto-expand reveals
                    // child sections — matches keyboard Up/Down arriving
                    // here.
                    state.tree_cursor_section = None;
                    // Deliberate click on a category — auto-expand so the
                    // user immediately sees its sections.
                    state.auto_expand_current_category();
                }
            }
            SettingsHit::CategoryDisclosure(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.toggle_category_expanded(idx);
                }
            }
            SettingsHit::CategorySection(cat_idx, section_idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.jump_to_section(cat_idx, section_idx);
                    // Mouse clicks in the left tree should keep the tree as
                    // the focused panel, just like clicking a top-level
                    // category. `jump_to_section` is also used by search and
                    // keyboard flows where moving focus to the settings body
                    // is correct, so restore the mouse-specific focus here.
                    state.focus.set(FocusPanel::Categories);
                }
            }
            SettingsHit::CategoriesPanel | SettingsHit::CategoriesScrollbar => {
                // Click without scroll wheel — no-op for now (the panel-area
                // hit only fires when no other category/section row was hit).
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Categories);
                }
            }
            SettingsHit::SearchResult(idx) => {
                // Click on search result - select it and jump to it (same as Enter)
                if let Some(ref mut state) = self.settings_state {
                    state.selected_search_result = idx;
                    state.jump_to_search_result();
                }
            }
            SettingsHit::Item(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
            }
            SettingsHit::ControlToggle(idx) | SettingsHit::ControlDropdown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_activate_current();
            }
            SettingsHit::ControlDropdownOption(idx, option_idx) => {
                // Click on a dropdown option - select it and close dropdown
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.dropdown_select(option_idx);
                }
            }
            SettingsHit::ControlDecrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_decrement_current();
            }
            SettingsHit::ControlIncrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_increment_current();
            }
            SettingsHit::ControlNumberValue(idx) => {
                // Click on the value between the brackets — focus the item
                // and enter inline editing mode (matches the Enter-key flow).
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.start_number_editing();
                }
            }
            SettingsHit::ControlText(idx) | SettingsHit::ControlTextListRow(idx, _) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.start_editing();
                }
            }
            SettingsHit::ControlMapRow(idx, row_idx) => {
                let is_add_new_row = if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;

                    let mut is_add_new = false;
                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                is_add_new = row_idx >= map_state.entries.len();
                                map_state.focused_entry = if row_idx < map_state.entries.len() {
                                    Some(row_idx)
                                } else {
                                    None
                                };
                            }
                        }
                    }
                    is_add_new
                } else {
                    false
                };
                // "Add new" row activates on single click (#604), entries require double-click
                if is_add_new_row || is_double_click {
                    self.settings_activate_current();
                }
            }
            SettingsHit::ControlMapAddNew(idx) => {
                // Click on map add-new row - focus it and activate immediately
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;

                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                map_state.focused_entry = None; // Focus add-new row
                            }
                        }
                    }
                }
                // Single click on add-new activates immediately
                self.settings_activate_current();
            }
            SettingsHit::ControlDualListAvailable(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| {
                        dl.active_column = DualListColumn::Available;
                        dl.available_cursor = row;
                    });
                    state.start_editing();
                }
            }
            SettingsHit::ControlDualListIncluded(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| {
                        dl.active_column = DualListColumn::Included;
                        dl.included_cursor = row;
                    });
                    state.start_editing();
                }
            }
            SettingsHit::ControlDualListAdd(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.add_selected());
                    state.on_value_changed();
                    state.refresh_dual_list_sibling();
                }
            }
            SettingsHit::ControlDualListRemove(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.remove_selected());
                    state.on_value_changed();
                    state.refresh_dual_list_sibling();
                }
            }
            SettingsHit::ControlDualListMoveUp(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.move_up());
                    state.on_value_changed();
                }
            }
            SettingsHit::ControlDualListMoveDown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.move_down());
                    state.on_value_changed();
                }
            }
            SettingsHit::ControlInherit(idx) => {
                // Click on [Inherit] button - set value to null (inherited)
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.set_current_to_null();
                }
            }
            SettingsHit::LayerButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.cycle_target_layer();
                }
            }
            SettingsHit::SaveButton => self.close_settings(true),
            SettingsHit::CancelButton => {
                if let Some(ref mut state) = self.settings_state {
                    if state.has_changes() {
                        state.showing_confirm_dialog = true;
                        state.confirm_dialog_selection = 0;
                    } else {
                        state.visible = false;
                    }
                }
            }
            SettingsHit::ResetButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.reset_current_to_default();
                }
            }
            SettingsHit::ClearCategoryButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.clear_current_category();
                }
            }
            SettingsHit::EditButton => {
                // Open config file for the selected layer
                if let Some(ref state) = self.settings_state {
                    let layer = state.target_layer;
                    // Best-effort: open may fail if file doesn't exist yet
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.open_config_file(layer);
                }
            }
            SettingsHit::Scrollbar => self.settings_scrollbar_click(row),
            SettingsHit::SearchScrollbar => self.search_scrollbar_click(row),
            SettingsHit::SearchResultsPanel => {
                // Clicking on search results panel background - no action needed
            }
        }
    }

    /// Select an entry-dialog item by index and begin editing it — the
    /// semantic equivalent of a TUI click on that row inside the add/edit
    /// dialog (`handle_entry_dialog_item_click`, non-TextList path). Used by
    /// the web `/settings` route so the entry dialog is clickable instead of
    /// keyboard-only.
    #[cfg(feature = "web")]
    pub(crate) fn entry_dialog_select_item(&mut self, idx: usize) {
        if let Some(state) = self.settings_state.as_mut() {
            if let Some(dialog) = state.entry_dialog_mut() {
                if idx >= dialog.items.len() || dialog.items[idx].read_only {
                    return;
                }
                dialog.focus_on_buttons = false;
                dialog.selected_item = idx;
                dialog.update_focus_states();
                if !dialog.editing_text {
                    dialog.start_editing();
                }
            }
        }
    }

    /// Activate an entry-dialog button by semantic name ("save" | "cancel" |
    /// "delete"). Routing by name rather than index keeps the web and TUI in
    /// agreement even though they lay the buttons out in a different order.
    #[cfg(feature = "web")]
    pub(crate) fn entry_dialog_activate_button(&mut self, kind: &str) {
        let Some(state) = self.settings_state.as_mut() else {
            return;
        };
        let has_delete = state
            .entry_dialog()
            .map(|d| !d.is_new && !d.no_delete)
            .unwrap_or(false);
        match kind {
            "save" => state.save_entry_dialog(),
            "delete" if has_delete => state.request_entry_delete_confirm(),
            _ => state.close_entry_dialog(),
        }
    }

    /// Whether the given screen coords sit inside the categories panel —
    /// used to route mouse-wheel events to the correct scroll target.
    fn over_categories_panel(&self, col: u16, row: u16) -> bool {
        self.active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|layout| layout.categories_panel_area)
            .is_some_and(|area| {
                col >= area.x
                    && col < area.x.saturating_add(area.width)
                    && row >= area.y
                    && row < area.y.saturating_add(area.height)
            })
    }

    fn settings_scroll_up(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_up(delta))
            .unwrap_or(false)
    }

    fn settings_scroll_down(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_down(delta))
            .unwrap_or(false)
    }

    fn settings_scrollbar_click(&mut self, row: u16) {
        if let Some(ref scrollbar_area) = self
            .active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|l| l.scrollbar_area)
        {
            if scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    state.scroll_to_ratio(ratio);
                }
            }
        }
    }

    fn settings_scrollbar_drag(&mut self, col: u16, row: u16) -> bool {
        if let Some(ref scrollbar_area) = self
            .active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|l| l.scrollbar_area)
        {
            let in_scrollbar_x = col >= scrollbar_area.x.saturating_sub(1)
                && col <= scrollbar_area.x + scrollbar_area.width;
            if in_scrollbar_x && scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    return state.scroll_to_ratio(ratio);
                }
            }
        }
        false
    }

    fn search_scrollbar_click(&mut self, row: u16) {
        if let Some(ref scrollbar_area) = self
            .active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|l| l.search_scrollbar_area)
        {
            if scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    state.search_scroll_to_ratio(ratio);
                }
            }
        }
    }

    fn search_scrollbar_drag(&mut self, col: u16, row: u16) -> Option<bool> {
        if let Some(ref scrollbar_area) = self
            .active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|l| l.search_scrollbar_area)
        {
            let in_scrollbar_x = col >= scrollbar_area.x.saturating_sub(1)
                && col <= scrollbar_area.x + scrollbar_area.width;
            if in_scrollbar_x && scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    return Some(state.search_scroll_to_ratio(ratio));
                }
            }
        }
        None // Not on search scrollbar
    }

    fn entry_dialog_layout(&self) -> Option<EntryDialogLayout> {
        self.active_chrome()
            .settings_layout
            .as_ref()
            .and_then(|l| EntryDialogLayout::from_modal(l.modal_area))
    }

    fn entry_dialog_scrollbar_drag(&mut self, col: u16, row: u16) -> bool {
        let Some(layout) = self.entry_dialog_layout() else {
            return false;
        };

        if layout.near_scrollbar(col) && layout.inner_height > 0 {
            let relative_y = row.saturating_sub(layout.inner_y);
            let ratio = (relative_y as f32 / layout.inner_height as f32).clamp(0.0, 1.0);

            if let Some(ref mut state) = self.settings_state {
                if let Some(dialog) = state.entry_dialog_mut() {
                    dialog.scroll_to_ratio(ratio);
                    return true;
                }
            }
        }
        false
    }

    fn entry_dialog_update_hover(&mut self, col: u16, row: u16) -> bool {
        let Some(layout) = self.entry_dialog_layout() else {
            return false;
        };

        let Some(ref mut state) = self.settings_state else {
            return false;
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return false;
        };

        let old_item = dialog.hover_item;
        let old_button = dialog.hover_button;

        // Reset hover state
        dialog.hover_item = None;
        dialog.hover_button = None;

        if !layout.contains(col, row) {
            return old_item.is_some() || old_button.is_some();
        }

        // Check button hover. Layout matches `render_entry_dialog`:
        // [Save, Cancel] or [Save, Cancel, Delete] with a wider gap
        // before Delete so the destructive button stands apart.
        if row == layout.button_y {
            let has_delete = !dialog.is_new && !dialog.no_delete;
            let buttons: &[&str] = if has_delete {
                &["[ Save ]", "[ Cancel ]", "[ Delete ]"]
            } else {
                &["[ Save ]", "[ Cancel ]"]
            };
            let delete_idx = if has_delete {
                Some(buttons.len() - 1)
            } else {
                None
            };
            const BUTTON_GAP: u16 = 2;
            const DELETE_GAP: u16 = 6;
            let total_width: u16 = buttons
                .iter()
                .enumerate()
                .map(|(i, b)| {
                    let gap = if Some(i) == delete_idx {
                        DELETE_GAP
                    } else if i == 0 {
                        0
                    } else {
                        BUTTON_GAP
                    };
                    b.len() as u16 + gap
                })
                .sum();
            let mut x = layout.dialog_x + (layout.dialog_width.saturating_sub(total_width)) / 2;

            for (idx, label) in buttons.iter().enumerate() {
                if idx > 0 {
                    let gap = if Some(idx) == delete_idx {
                        DELETE_GAP
                    } else {
                        BUTTON_GAP
                    };
                    x += gap;
                }
                let width = label.len() as u16;
                if col >= x && col < x + width {
                    dialog.hover_button = Some(idx);
                    break;
                }
                x += width;
            }
        }

        // Check item hover (only for editable items)
        if layout.in_content_area(col, row) {
            let click_y = (row - layout.inner_y) as usize + dialog.scroll_offset;
            let mut content_y: usize = 0;

            // Check if we have a separator between read-only and editable items
            let first_editable = dialog.first_editable_index;
            let has_separator = first_editable > 0 && first_editable < dialog.items.len();

            for (idx, item) in dialog.items.iter().enumerate() {
                // Account for separator before first editable item
                if has_separator && idx == first_editable {
                    content_y += 1; // separator height
                }

                let item_end = content_y + item.control.control_height() as usize;
                if click_y >= content_y && click_y < item_end {
                    // Only hover on editable items
                    if !item.read_only {
                        dialog.hover_item = Some(idx);
                    }
                    break;
                }
                content_y = item_end;
            }
        }

        old_item != dialog.hover_item || old_button != dialog.hover_button
    }

    fn handle_entry_dialog_click(
        &mut self,
        col: u16,
        row: u16,
        _is_double_click: bool,
    ) -> AnyhowResult<bool> {
        let Some(layout) = self.entry_dialog_layout() else {
            return Ok(false);
        };

        if !layout.contains(col, row) {
            return Ok(false);
        }

        // Button click
        if row == layout.button_y {
            return self.handle_entry_dialog_button_click(col, &layout);
        }

        // Item click
        if layout.in_content_area(col, row) {
            return self.handle_entry_dialog_item_click(col, row, &layout);
        }

        Ok(false)
    }

    fn handle_entry_dialog_button_click(
        &mut self,
        col: u16,
        layout: &EntryDialogLayout,
    ) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return Ok(false);
        };

        let has_delete = !dialog.is_new && !dialog.no_delete;
        let buttons: &[&str] = if has_delete {
            &["[ Save ]", "[ Cancel ]", "[ Delete ]"]
        } else {
            &["[ Save ]", "[ Cancel ]"]
        };
        let delete_idx = if has_delete {
            Some(buttons.len() - 1)
        } else {
            None
        };
        const BUTTON_GAP: u16 = 2;
        const DELETE_GAP: u16 = 6;
        let total_width: u16 = buttons
            .iter()
            .enumerate()
            .map(|(i, b)| {
                let gap = if Some(i) == delete_idx {
                    DELETE_GAP
                } else if i == 0 {
                    0
                } else {
                    BUTTON_GAP
                };
                b.len() as u16 + gap
            })
            .sum();
        let mut x = layout.dialog_x + (layout.dialog_width.saturating_sub(total_width)) / 2;

        for (idx, label) in buttons.iter().enumerate() {
            if idx > 0 {
                let gap = if Some(idx) == delete_idx {
                    DELETE_GAP
                } else {
                    BUTTON_GAP
                };
                x += gap;
            }
            let width = label.len() as u16;
            if col >= x && col < x + width {
                dialog.focus_on_buttons = true;
                dialog.focused_button = idx;
                return self.settings_entry_dialog_activate_button();
            }
            x += width;
        }
        Ok(false)
    }

    /// Map a click on a single-line `Text` control to a byte offset in
    /// its value, so the caret lands where the user clicked (#2573).
    ///
    /// The entry dialog is a modal that recomputes its own item positions
    /// on click, so rather than store per-item geometry it reconstructs it:
    /// it replays the exact widget render the dialog drew
    /// (`setting_control_to_widget_aligned` → `render_spec_no_autofocus`)
    /// and reads the value-layout breadcrumbs back off the field's `focus`
    /// hit area via [`WidgetTextClickGeometry`] — the same type the main
    /// settings panel stamps at render time, and the same value-byte
    /// mapping the widget-panel click path uses. Returns `None` for
    /// non-text controls or when the render produced no text hit (e.g. an
    /// empty item name → no key).
    fn entry_text_click_to_value_byte(
        control: &SettingControl,
        item_name: &str,
        label_col_width: u16,
        control_area_x: u16,
        control_area_width: u16,
        click_col: u16,
    ) -> Option<usize> {
        if !matches!(control, SettingControl::Text(_)) {
            return None;
        }
        // Mirrors `render_entry_items`: the control is indented by the
        // 3-col focus-indicator gutter, and the widget's own label column
        // is the dialog-wide width minus that gutter.
        const FOCUS_INDICATOR_WIDTH: u16 = 3;
        let spec = crate::view::settings::widget_map::setting_control_to_widget_aligned(
            item_name,
            control,
            Some(label_col_width.saturating_sub(FOCUS_INDICATOR_WIDTH)),
        );
        let out = crate::widgets::render_spec_no_autofocus(
            &spec,
            &std::collections::HashMap::new(),
            "",
            control_area_width.max(1) as u32,
        );
        let geometry =
            crate::widgets::WidgetTextClickGeometry::from_render_output(&out, control_area_x)?;
        Some(geometry.value_byte_at(click_col))
    }

    fn handle_entry_dialog_item_click(
        &mut self,
        col: u16,
        row: u16,
        layout: &EntryDialogLayout,
    ) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return Ok(false);
        };

        let click_y = (row - layout.inner_y) as usize + dialog.scroll_offset;
        let mut content_y: usize = 0;

        // Check if we have a separator between read-only and editable items
        let first_editable = dialog.first_editable_index;
        let has_separator = first_editable > 0 && first_editable < dialog.items.len();

        for (idx, item) in dialog.items.iter().enumerate() {
            // Account for separator before first editable item
            if has_separator && idx == first_editable {
                content_y += 1; // separator height
            }

            let item_end = content_y + item.control.control_height() as usize;
            if click_y >= content_y && click_y < item_end {
                // Skip clicks on read-only items
                if item.read_only {
                    return Ok(false);
                }
                let sub_row = click_y - content_y;

                // Per-field action buttons ([Reset]/[Inherit]) rendered on the
                // control's first row at the right edge (mirror of the renderer
                // in `render_entry_items`). Resolve which one was clicked using
                // `item`/`dialog` immutably first, then mutate — keeps the
                // borrows disjoint.
                let clicked_action = if sub_row == 0 {
                    let buttons = dialog.field_action_buttons(idx);
                    let right = layout.inner_x + layout.inner_width;
                    super::entry_dialog::layout_field_action_buttons(&buttons, right)
                        .into_iter()
                        .find(|(_, x, w)| col >= *x && col < x.saturating_add(*w))
                        .map(|(action, _, _)| action)
                } else {
                    None
                };
                if let Some(action) = clicked_action {
                    match action {
                        super::entry_dialog::FieldAction::Reset => {
                            dialog.reset_field(idx);
                        }
                        super::entry_dialog::FieldAction::Inherit => {
                            dialog.inherit_field(idx);
                        }
                    }
                    dialog.focus_on_buttons = false;
                    dialog.field_button_focus = None;
                    dialog.selected_item = idx;
                    dialog.update_focus_states();
                    return Ok(true);
                }

                // TextList rows render as: label (sub_row 0), one row
                // per committed item, then the trailing add-new row.
                // Map the click to either remove (`[x]`), focus +
                // edit (text area), or activate-pending (`[+] Add new`
                // / input `[+]`). Without this, every click in a
                // TextList row just opened the input, ignoring `[x]`
                // and the row-text targets entirely.
                if matches!(item.control, SettingControl::TextList(_)) {
                    return self.handle_text_list_click(idx, sub_row, col, layout);
                }
                // Click-to-position: resolve the clicked column to a value
                // byte before mutating the dialog (the immutable `item`
                // borrow is still live here). `label_col_width` mirrors
                // `render_settings_entry_dialog`'s dialog-wide computation.
                let click_byte = if matches!(item.control, SettingControl::Text(_)) {
                    const FOCUS_INDICATOR_WIDTH: u16 = 3;
                    let max_label_width = (layout.inner_width / 2).max(20);
                    let label_col_width = dialog
                        .items
                        .iter()
                        .map(|it| it.name.len() as u16 + 2)
                        .filter(|&w| w <= max_label_width)
                        .max()
                        .unwrap_or(20)
                        .min(max_label_width);
                    Self::entry_text_click_to_value_byte(
                        &item.control,
                        &item.name,
                        label_col_width,
                        layout.inner_x + FOCUS_INDICATOR_WIDTH,
                        layout.inner_width.saturating_sub(FOCUS_INDICATOR_WIDTH),
                        col,
                    )
                } else {
                    None
                };
                dialog.focus_on_buttons = false;
                dialog.field_button_focus = None;
                dialog.selected_item = idx;
                dialog.update_focus_states();
                if !dialog.editing_text {
                    dialog.start_editing();
                }
                // Place the caret after `start_editing` (which seeds the
                // cursor at end-of-value), so the click position wins.
                if let Some(byte) = click_byte {
                    if let SettingControl::Text(ts) = &mut dialog.items[idx].control {
                        ts.set_cursor_from_flat(byte);
                    }
                }
                return Ok(true);
            }
            content_y = item_end;
        }
        Ok(false)
    }

    fn handle_text_list_click(
        &mut self,
        item_idx: usize,
        sub_row: usize,
        col: u16,
        _layout: &EntryDialogLayout,
    ) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return Ok(false);
        };
        let item = match dialog.items.get_mut(item_idx) {
            Some(it) => it,
            None => return Ok(false),
        };
        let tl = match &mut item.control {
            SettingControl::TextList(s) => s,
            _ => return Ok(false),
        };

        // sub_row 0 is the label row.
        if sub_row == 0 {
            // Focus the control on a generic label click; user
            // can keyboard from there.
            dialog.focus_on_buttons = false;
            dialog.selected_item = item_idx;
            dialog.update_focus_states();
            return Ok(true);
        }

        let n_items = tl.items.len();
        // Compute which TextList row was clicked: existing item rows
        // are sub_row 1..=n_items, the add-new row is sub_row n_items+1.
        let on_add_row = sub_row == n_items + 1;
        let item_row_idx = if !on_add_row { Some(sub_row - 1) } else { None };

        // Hit-test the trailing button (`[x]` or `[+]`). The renderer
        // uses indent=2 then `[xxx]` for actual_field_width chars, then
        // ` ` then `[x]` or `[+]` (3 chars). The dialog inner area starts
        // at `inner_x + focus_indicator_width` (3 cols). Computing the
        // exact column would need the actual_field_width, which we
        // don't carry here; treat anything in the rightmost ~5 chars
        // of the rendered row that the user is likely to click as the
        // trailing button. With the dialog defaulting to 30-char fields
        // this lands cleanly on `[x]` / `[+]`.
        let dialog_inner_right = _layout.dialog_x + _layout.dialog_width.saturating_sub(2);
        let in_trailing_button = col + 5 >= dialog_inner_right
            || {
                // Fallback: if the row text is shorter than the field
                // (common — items rarely fill 30 chars), the user clicks
                // the [x] which is right after `]`. Use a hand-rolled
                // approximation: text_indent + field_width < col.
                let text_indent = _layout.dialog_x + 2 + 3 /* focus indicator */ + 2 /* TextList indent */ + 1 /* `[` */;
                let estimated_field_width = 28u16;
                col > text_indent + estimated_field_width
            };

        match (on_add_row, item_row_idx, in_trailing_button) {
            // Click on `[+]` of an active input: commit pending.
            (true, _, true) if tl.pending_active || !tl.new_item_text.is_empty() => {
                tl.add_item();
                dialog.user_edited = true;
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                tl.focused_item = None;
                tl.pending_active = false;
                dialog.update_focus_states();
                Ok(true)
            }
            // Click anywhere on the (collapsed) `[+] Add new` row, or
            // on the input text area: focus the trailing slot and
            // activate input mode so the user can start typing.
            (true, _, _) => {
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                tl.activate_pending();
                dialog.editing_text = true;
                dialog.update_focus_states();
                Ok(true)
            }
            // Click on `[x]` of a committed row: remove it.
            (false, Some(row_idx), true) if row_idx < tl.items.len() => {
                tl.remove_item(row_idx);
                dialog.user_edited = true;
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                dialog.update_focus_states();
                Ok(true)
            }
            // Click on a committed row's text area: focus that item.
            (false, Some(row_idx), false) if row_idx < tl.items.len() => {
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                tl.focused_item = Some(row_idx);
                tl.pending_active = false;
                dialog.update_focus_states();
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn settings_entry_dialog_activate_button(&mut self) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };

        let (btn, has_delete) = {
            let Some(dialog) = state.entry_dialog() else {
                return Ok(false);
            };
            let has_delete = !dialog.is_new && !dialog.no_delete;
            (dialog.focused_button, has_delete)
        };

        // Button order: [Save, Cancel] or [Save, Cancel, Delete].
        match (btn, has_delete) {
            (0, _) => state.save_entry_dialog(),
            (1, _) => state.close_entry_dialog(),
            (2, true) => state.request_entry_delete_confirm(),
            _ => state.close_entry_dialog(),
        }
        Ok(true)
    }

    fn handle_confirm_dialog_click(&mut self, col: u16, row: u16) -> AnyhowResult<bool> {
        if let Some(idx) = self.get_confirm_dialog_button_at(col, row) {
            match idx {
                0 => self.save_settings_and_close(),
                1 => self.discard_settings_and_close(),
                2 => {
                    if let Some(ref mut state) = self.settings_state {
                        state.showing_confirm_dialog = false;
                    }
                }
                _ => {}
            }
            return Ok(true);
        }
        Ok(false)
    }

    /// Returns which confirm dialog button (0-2) is at the given position, if any
    fn get_confirm_dialog_button_at(&self, col: u16, row: u16) -> Option<usize> {
        let modal_area = self
            .active_chrome()
            .settings_layout
            .as_ref()
            .map(|l| l.modal_area)?;

        let changes_count = self
            .settings_state
            .as_ref()
            .map(|s| s.get_change_descriptions().len())
            .unwrap_or(0);

        // Calculate dialog dimensions (same as in render_confirm_dialog)
        let dialog_width = 50u16.min(modal_area.width.saturating_sub(4));
        let dialog_height = (7 + changes_count as u16)
            .min(20)
            .min(modal_area.height.saturating_sub(4));
        let dialog_x = modal_area.x + (modal_area.width.saturating_sub(dialog_width)) / 2;
        let dialog_y = modal_area.y + (modal_area.height.saturating_sub(dialog_height)) / 2;

        let inner_x = dialog_x + 2;
        let inner_width = dialog_width.saturating_sub(4);
        let button_y = dialog_y + dialog_height - 3;

        // Check if on button row
        if row != button_y {
            return None;
        }

        // Button labels (must match render_confirm_dialog)
        let options = [
            t!("confirm.save_and_exit").to_string(),
            t!("confirm.discard").to_string(),
            t!("confirm.cancel").to_string(),
        ];
        let total_width: u16 = options.iter().map(|o| o.len() as u16 + 4).sum::<u16>() + 4;
        let mut x = inner_x + (inner_width.saturating_sub(total_width)) / 2;

        for (idx, label) in options.iter().enumerate() {
            let button_width = label.len() as u16 + 4;
            if col >= x && col < x + button_width + 1 {
                return Some(idx);
            }
            x += button_width + 3;
        }

        None
    }

    fn save_settings_and_close(&mut self) {
        self.save_settings();
        if let Some(ref mut state) = self.settings_state {
            state.visible = false;
            state.showing_confirm_dialog = false;
        }
    }

    fn discard_settings_and_close(&mut self) {
        if let Some(ref mut state) = self.settings_state {
            state.visible = false;
            state.showing_confirm_dialog = false;
        }
    }
}
