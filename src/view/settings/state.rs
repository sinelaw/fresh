//! Settings state management
//!
//! Tracks the current state of the settings UI, pending changes,
//! and provides methods for reading/writing config values.

use super::entry_dialog::EntryDialogState;
use super::items::{control_to_value, SettingControl, SettingItem, SettingsPage};
use super::layout::SettingsHit;
use super::schema::{parse_schema, SettingCategory};
use super::search::{search_settings, SearchResult};
use crate::config::Config;
use crate::view::controls::FocusState;
use crate::view::ui::ScrollablePanel;
use std::collections::HashMap;

/// Which panel currently has keyboard focus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FocusPanel {
    /// Category list (left panel)
    #[default]
    Categories,
    /// Settings items (right panel)
    Settings,
    /// Footer buttons (Reset/Save/Cancel)
    Footer,
}

/// The state of the settings UI
#[derive(Debug)]
pub struct SettingsState {
    /// Parsed schema categories
    categories: Vec<SettingCategory>,
    /// Pages built from categories
    pub pages: Vec<SettingsPage>,
    /// Currently selected category index
    pub selected_category: usize,
    /// Currently selected item index within the category
    pub selected_item: usize,
    /// Which panel currently has keyboard focus
    pub focus_panel: FocusPanel,
    /// Selected footer button index (0=Reset, 1=Save, 2=Cancel)
    pub footer_button_index: usize,
    /// Pending changes (path -> new value)
    pub pending_changes: HashMap<String, serde_json::Value>,
    /// The original config value (for detecting changes)
    original_config: serde_json::Value,
    /// Whether the settings panel is visible
    pub visible: bool,
    /// Current search query
    pub search_query: String,
    /// Whether search is active
    pub search_active: bool,
    /// Current search results
    pub search_results: Vec<SearchResult>,
    /// Selected search result index
    pub selected_search_result: usize,
    /// Whether the unsaved changes confirmation dialog is showing
    pub showing_confirm_dialog: bool,
    /// Selected option in confirmation dialog (0=Save, 1=Discard, 2=Cancel)
    pub confirm_dialog_selection: usize,
    /// Whether the help overlay is showing
    pub showing_help: bool,
    /// Scrollable panel for settings items
    pub scroll_panel: ScrollablePanel,
    /// Sub-focus index within the selected item (for TextList/Map navigation)
    pub sub_focus: Option<usize>,
    /// Whether we're in text editing mode (for TextList controls)
    pub editing_text: bool,
    /// Current mouse hover position (for hover feedback)
    pub hover_position: Option<(u16, u16)>,
    /// Current hover hit result (computed from hover_position and cached layout)
    pub hover_hit: Option<SettingsHit>,
    /// Entry detail dialog state (for editing Language/LSP/Keybinding entries)
    pub entry_dialog: Option<EntryDialogState>,
}

impl SettingsState {
    /// Create a new settings state from schema and current config
    pub fn new(schema_json: &str, config: &Config) -> Result<Self, serde_json::Error> {
        let categories = parse_schema(schema_json)?;
        let config_value = serde_json::to_value(config)?;
        let pages = super::items::build_pages(&categories, &config_value);

        Ok(Self {
            categories,
            pages,
            selected_category: 0,
            selected_item: 0,
            focus_panel: FocusPanel::Categories,
            footer_button_index: 1, // Default to Save button
            pending_changes: HashMap::new(),
            original_config: config_value,
            visible: false,
            search_query: String::new(),
            search_active: false,
            search_results: Vec::new(),
            selected_search_result: 0,
            showing_confirm_dialog: false,
            confirm_dialog_selection: 0,
            showing_help: false,
            scroll_panel: ScrollablePanel::new(),
            sub_focus: None,
            editing_text: false,
            hover_position: None,
            hover_hit: None,
            entry_dialog: None,
        })
    }

    /// Show the settings panel
    pub fn show(&mut self) {
        self.visible = true;
        self.focus_panel = FocusPanel::Categories;
        self.footer_button_index = 1; // Default to Save button
        self.selected_category = 0;
        self.selected_item = 0;
        self.scroll_panel = ScrollablePanel::new();
        self.sub_focus = None;
    }

    /// Hide the settings panel
    pub fn hide(&mut self) {
        self.visible = false;
        self.search_active = false;
        self.search_query.clear();
    }

    /// Get the currently selected page
    pub fn current_page(&self) -> Option<&SettingsPage> {
        self.pages.get(self.selected_category)
    }

    /// Get the currently selected page mutably
    pub fn current_page_mut(&mut self) -> Option<&mut SettingsPage> {
        self.pages.get_mut(self.selected_category)
    }

    /// Get the currently selected item
    pub fn current_item(&self) -> Option<&SettingItem> {
        self.current_page()
            .and_then(|page| page.items.get(self.selected_item))
    }

    /// Get the currently selected item mutably
    pub fn current_item_mut(&mut self) -> Option<&mut SettingItem> {
        self.pages
            .get_mut(self.selected_category)
            .and_then(|page| page.items.get_mut(self.selected_item))
    }

    /// Check if the current text field can be exited (valid JSON if required)
    pub fn can_exit_text_editing(&self) -> bool {
        self.current_item()
            .map(|item| {
                if let SettingControl::Text(state) = &item.control {
                    state.is_valid()
                } else {
                    true
                }
            })
            .unwrap_or(true)
    }

    /// Check if entry dialog's current text field can be exited (valid JSON if required)
    pub fn entry_dialog_can_exit_text_editing(&self) -> bool {
        self.entry_dialog
            .as_ref()
            .and_then(|dialog| dialog.current_item())
            .map(|item| {
                if let SettingControl::Text(state) = &item.control {
                    state.is_valid()
                } else {
                    true
                }
            })
            .unwrap_or(true)
    }

    /// Initialize map focus when entering a Map control.
    /// `from_above`: true = start at first entry, false = start at add-new field
    fn init_map_focus(&mut self, from_above: bool) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Map(ref mut map_state) = item.control {
                map_state.init_focus(from_above);
            }
        }
    }

    /// Move selection up
    pub fn select_prev(&mut self) {
        match self.focus_panel {
            FocusPanel::Categories => {
                if self.selected_category > 0 {
                    self.selected_category -= 1;
                    self.selected_item = 0;
                    self.scroll_panel = ScrollablePanel::new();
                    self.sub_focus = None;
                }
            }
            FocusPanel::Settings => {
                // Try to navigate within current Map control first
                let handled = self
                    .current_item_mut()
                    .and_then(|item| match &mut item.control {
                        SettingControl::Map(map_state) => Some(map_state.focus_prev()),
                        _ => None,
                    })
                    .unwrap_or(false);

                if !handled && self.selected_item > 0 {
                    self.selected_item -= 1;
                    self.sub_focus = None;
                    self.init_map_focus(false); // entering from below
                }
                self.ensure_visible();
            }
            FocusPanel::Footer => {
                // Navigate between footer buttons (left)
                if self.footer_button_index > 0 {
                    self.footer_button_index -= 1;
                }
            }
        }
    }

    /// Move selection down
    pub fn select_next(&mut self) {
        match self.focus_panel {
            FocusPanel::Categories => {
                if self.selected_category + 1 < self.pages.len() {
                    self.selected_category += 1;
                    self.selected_item = 0;
                    self.scroll_panel = ScrollablePanel::new();
                    self.sub_focus = None;
                }
            }
            FocusPanel::Settings => {
                // Try to navigate within current Map control first
                let handled = self
                    .current_item_mut()
                    .and_then(|item| match &mut item.control {
                        SettingControl::Map(map_state) => Some(map_state.focus_next()),
                        _ => None,
                    })
                    .unwrap_or(false);

                if !handled {
                    let can_move = self
                        .current_page()
                        .map_or(false, |page| self.selected_item + 1 < page.items.len());
                    if can_move {
                        self.selected_item += 1;
                        self.sub_focus = None;
                        self.init_map_focus(true); // entering from above
                    }
                }
                self.ensure_visible();
            }
            FocusPanel::Footer => {
                // Navigate between footer buttons (right)
                if self.footer_button_index < 2 {
                    self.footer_button_index += 1;
                }
            }
        }
    }

    /// Switch focus between panels: Categories -> Settings -> Footer -> Categories
    pub fn toggle_focus(&mut self) {
        self.focus_panel = match self.focus_panel {
            FocusPanel::Categories => FocusPanel::Settings,
            FocusPanel::Settings => FocusPanel::Footer,
            FocusPanel::Footer => FocusPanel::Categories,
        };

        // Reset item selection when switching to settings
        if self.focus_panel == FocusPanel::Settings
            && self.selected_item >= self.current_page().map_or(0, |p| p.items.len())
        {
            self.selected_item = 0;
        }
        self.sub_focus = None;

        if self.focus_panel == FocusPanel::Settings {
            self.init_map_focus(true); // entering from above
        }

        self.ensure_visible();
    }

    /// Ensure the selected item is visible in the viewport
    pub fn ensure_visible(&mut self) {
        if self.focus_panel != FocusPanel::Settings {
            return;
        }

        // Need to avoid borrowing self for both page and scroll_panel
        let selected_item = self.selected_item;
        let sub_focus = self.sub_focus;
        if let Some(page) = self.pages.get(self.selected_category) {
            self.scroll_panel
                .ensure_focused_visible(&page.items, selected_item, sub_focus);
        }
    }

    /// Record a pending change for a setting
    pub fn set_pending_change(&mut self, path: &str, value: serde_json::Value) {
        // Check if this is the same as the original value
        let original = self.original_config.pointer(path);
        if original == Some(&value) {
            self.pending_changes.remove(path);
        } else {
            self.pending_changes.insert(path.to_string(), value);
        }
    }

    /// Check if there are unsaved changes
    pub fn has_changes(&self) -> bool {
        !self.pending_changes.is_empty()
    }

    /// Apply pending changes to a config
    pub fn apply_changes(&self, config: &Config) -> Result<Config, serde_json::Error> {
        let mut config_value = serde_json::to_value(config)?;

        for (path, value) in &self.pending_changes {
            if let Some(target) = config_value.pointer_mut(path) {
                *target = value.clone();
            }
        }

        serde_json::from_value(config_value)
    }

    /// Discard all pending changes
    pub fn discard_changes(&mut self) {
        self.pending_changes.clear();
        // Rebuild pages from original config
        self.pages = super::items::build_pages(&self.categories, &self.original_config);
    }

    /// Reset the current item to its default value
    pub fn reset_current_to_default(&mut self) {
        // Get the info we need first, then release the borrow
        let reset_info = self.current_item().and_then(|item| {
            item.default
                .as_ref()
                .map(|default| (item.path.clone(), default.clone()))
        });

        if let Some((path, default)) = reset_info {
            self.set_pending_change(&path, default.clone());

            // Now update the control state
            if let Some(item) = self.current_item_mut() {
                update_control_from_value(&mut item.control, &default);
                item.modified = false;
            }
        }
    }

    /// Handle a value change from user interaction
    pub fn on_value_changed(&mut self) {
        // Get value and path first, then release borrow
        let change_info = self.current_item().map(|item| {
            let value = control_to_value(&item.control);
            let modified = match &item.default {
                Some(default) => &value != default,
                None => true,
            };
            (item.path.clone(), value, modified)
        });

        if let Some((path, value, modified)) = change_info {
            // Update modified flag
            if let Some(item) = self.current_item_mut() {
                item.modified = modified;
            }
            self.set_pending_change(&path, value);
        }
    }

    /// Update focus states for rendering
    pub fn update_focus_states(&mut self) {
        for (page_idx, page) in self.pages.iter_mut().enumerate() {
            for (item_idx, item) in page.items.iter_mut().enumerate() {
                let is_focused = self.focus_panel == FocusPanel::Settings
                    && page_idx == self.selected_category
                    && item_idx == self.selected_item;

                let focus = if is_focused {
                    FocusState::Focused
                } else {
                    FocusState::Normal
                };

                match &mut item.control {
                    SettingControl::Toggle(state) => state.focus = focus,
                    SettingControl::Number(state) => state.focus = focus,
                    SettingControl::Dropdown(state) => state.focus = focus,
                    SettingControl::Text(state) => state.focus = focus,
                    SettingControl::TextList(state) => state.focus = focus,
                    SettingControl::Map(state) => state.focus = focus,
                    SettingControl::KeybindingList(state) => state.focus = focus,
                    SettingControl::Json(state) => state.focus = focus,
                    SettingControl::Complex { .. } => {}
                }
            }
        }
    }

    /// Start search mode
    pub fn start_search(&mut self) {
        self.search_active = true;
        self.search_query.clear();
        self.search_results.clear();
        self.selected_search_result = 0;
    }

    /// Cancel search mode
    pub fn cancel_search(&mut self) {
        self.search_active = false;
        self.search_query.clear();
        self.search_results.clear();
        self.selected_search_result = 0;
    }

    /// Update search query and refresh results
    pub fn set_search_query(&mut self, query: String) {
        self.search_query = query;
        self.search_results = search_settings(&self.pages, &self.search_query);
        self.selected_search_result = 0;
    }

    /// Add a character to the search query
    pub fn search_push_char(&mut self, c: char) {
        self.search_query.push(c);
        self.search_results = search_settings(&self.pages, &self.search_query);
        self.selected_search_result = 0;
    }

    /// Remove the last character from the search query
    pub fn search_pop_char(&mut self) {
        self.search_query.pop();
        self.search_results = search_settings(&self.pages, &self.search_query);
        self.selected_search_result = 0;
    }

    /// Navigate to previous search result
    pub fn search_prev(&mut self) {
        if !self.search_results.is_empty() && self.selected_search_result > 0 {
            self.selected_search_result -= 1;
        }
    }

    /// Navigate to next search result
    pub fn search_next(&mut self) {
        if !self.search_results.is_empty()
            && self.selected_search_result + 1 < self.search_results.len()
        {
            self.selected_search_result += 1;
        }
    }

    /// Jump to the currently selected search result
    pub fn jump_to_search_result(&mut self) {
        if let Some(result) = self.search_results.get(self.selected_search_result) {
            self.selected_category = result.page_index;
            self.selected_item = result.item_index;
            self.focus_panel = FocusPanel::Settings;
            // Reset scroll offset but preserve viewport for ensure_visible
            self.scroll_panel.scroll.offset = 0;
            // Update content height for the new category's items
            if let Some(page) = self.pages.get(self.selected_category) {
                self.scroll_panel.update_content_height(&page.items);
            }
            self.sub_focus = None;
            self.init_map_focus(true);
            self.ensure_visible();
            self.cancel_search();
        }
    }

    /// Get the currently selected search result
    pub fn current_search_result(&self) -> Option<&SearchResult> {
        self.search_results.get(self.selected_search_result)
    }

    /// Show the unsaved changes confirmation dialog
    pub fn show_confirm_dialog(&mut self) {
        self.showing_confirm_dialog = true;
        self.confirm_dialog_selection = 0; // Default to "Save and Exit"
    }

    /// Hide the confirmation dialog
    pub fn hide_confirm_dialog(&mut self) {
        self.showing_confirm_dialog = false;
        self.confirm_dialog_selection = 0;
    }

    /// Move to next option in confirmation dialog
    pub fn confirm_dialog_next(&mut self) {
        self.confirm_dialog_selection = (self.confirm_dialog_selection + 1) % 3;
    }

    /// Move to previous option in confirmation dialog
    pub fn confirm_dialog_prev(&mut self) {
        self.confirm_dialog_selection = if self.confirm_dialog_selection == 0 {
            2
        } else {
            self.confirm_dialog_selection - 1
        };
    }

    /// Toggle the help overlay
    pub fn toggle_help(&mut self) {
        self.showing_help = !self.showing_help;
    }

    /// Hide the help overlay
    pub fn hide_help(&mut self) {
        self.showing_help = false;
    }

    /// Check if the entry dialog is showing
    pub fn showing_entry_dialog(&self) -> bool {
        self.entry_dialog.is_some()
    }

    /// Open the entry dialog for the currently focused map entry
    pub fn open_entry_dialog(&mut self) {
        let Some(item) = self.current_item() else {
            return;
        };

        // Determine what type of entry we're editing based on the path
        let path = item.path.as_str();
        let SettingControl::Map(map_state) = &item.control else {
            return;
        };

        // Get the focused entry
        let Some(entry_idx) = map_state.focused_entry else {
            return;
        };
        let Some((key, value)) = map_state.entries.get(entry_idx) else {
            return;
        };

        // Get the value schema for this map
        let Some(schema) = map_state.value_schema.as_ref() else {
            return; // No schema available, can't create dialog
        };

        // Create dialog from schema
        let dialog = EntryDialogState::from_schema(key.clone(), value, schema, &path, false);
        self.entry_dialog = Some(dialog);
    }

    /// Open entry dialog for adding a new entry (with empty key)
    pub fn open_add_entry_dialog(&mut self) {
        let Some(item) = self.current_item() else {
            return;
        };
        let SettingControl::Map(map_state) = &item.control else {
            return;
        };
        let Some(schema) = map_state.value_schema.as_ref() else {
            return;
        };
        let path = item.path.clone();

        // Create dialog with empty key - user will fill it in
        let dialog = EntryDialogState::from_schema(
            String::new(),
            &serde_json::json!({}),
            schema,
            &path,
            true,
        );
        self.entry_dialog = Some(dialog);
    }

    /// Close the entry dialog without saving
    pub fn close_entry_dialog(&mut self) {
        self.entry_dialog = None;
    }

    /// Save the entry dialog and apply changes
    pub fn save_entry_dialog(&mut self) {
        let Some(dialog) = self.entry_dialog.take() else {
            return;
        };

        // Get key from the dialog's key field (may have been edited)
        let key = dialog.get_key();
        if key.is_empty() {
            return; // Can't save with empty key
        }

        let value = dialog.to_value();
        let map_path = dialog.map_path.clone();
        let original_key = dialog.entry_key.clone();
        let is_new = dialog.is_new;
        let key_changed = !is_new && key != original_key;

        // Update the map control with the new value
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Map(map_state) = &mut item.control {
                // If key was changed, remove old entry first
                if key_changed {
                    if let Some(idx) = map_state
                        .entries
                        .iter()
                        .position(|(k, _)| k == &original_key)
                    {
                        map_state.entries.remove(idx);
                    }
                }

                // Find or add the entry with the (possibly new) key
                if let Some(entry) = map_state.entries.iter_mut().find(|(k, _)| k == &key) {
                    entry.1 = value.clone();
                } else {
                    map_state.entries.push((key.clone(), value.clone()));
                    map_state.entries.sort_by(|a, b| a.0.cmp(&b.0));
                }
            }
        }

        // Record deletion of old key if key was changed
        if key_changed {
            let old_path = format!("{}/{}", map_path, original_key);
            self.pending_changes
                .insert(old_path, serde_json::Value::Null);
        }

        // Record the pending change
        let path = format!("{}/{}", map_path, key);
        self.set_pending_change(&path, value);
    }

    /// Delete the entry from the map and close the dialog
    pub fn delete_entry_dialog(&mut self) {
        let Some(dialog) = self.entry_dialog.take() else {
            return;
        };

        let path = format!("{}/{}", dialog.map_path, dialog.entry_key);

        // Remove from the map control
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Map(map_state) = &mut item.control {
                if let Some(idx) = map_state
                    .entries
                    .iter()
                    .position(|(k, _)| k == &dialog.entry_key)
                {
                    map_state.remove_entry(idx);
                }
            }
        }

        // Record the pending change (null value signals deletion)
        self.set_pending_change(&path, serde_json::Value::Null);
    }

    /// Get the maximum scroll offset for the current page (in rows)
    pub fn max_scroll(&self) -> u16 {
        self.scroll_panel.scroll.max_offset()
    }

    /// Scroll up by a given number of rows
    /// Returns true if the scroll offset changed
    pub fn scroll_up(&mut self, delta: usize) -> bool {
        let old = self.scroll_panel.scroll.offset;
        self.scroll_panel.scroll_up(delta as u16);
        old != self.scroll_panel.scroll.offset
    }

    /// Scroll down by a given number of rows
    /// Returns true if the scroll offset changed
    pub fn scroll_down(&mut self, delta: usize) -> bool {
        let old = self.scroll_panel.scroll.offset;
        self.scroll_panel.scroll_down(delta as u16);
        old != self.scroll_panel.scroll.offset
    }

    /// Scroll to a position based on a ratio (0.0 to 1.0)
    /// Returns true if the scroll offset changed
    pub fn scroll_to_ratio(&mut self, ratio: f32) -> bool {
        let old = self.scroll_panel.scroll.offset;
        self.scroll_panel.scroll_to_ratio(ratio);
        old != self.scroll_panel.scroll.offset
    }

    /// Start text editing mode for TextList, Text, or Map controls
    pub fn start_editing(&mut self) {
        if let Some(item) = self.current_item() {
            if matches!(
                item.control,
                SettingControl::TextList(_) | SettingControl::Text(_) | SettingControl::Map(_)
            ) {
                self.editing_text = true;
            }
        }
    }

    /// Stop text editing mode
    pub fn stop_editing(&mut self) {
        self.editing_text = false;
    }

    /// Check if the current item is editable (TextList, Text, or Map)
    pub fn is_editable_control(&self) -> bool {
        self.current_item().map_or(false, |item| {
            matches!(
                item.control,
                SettingControl::TextList(_) | SettingControl::Text(_) | SettingControl::Map(_)
            )
        })
    }

    /// Insert a character into the current editable control
    pub fn text_insert(&mut self, c: char) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.insert(c),
                SettingControl::Text(state) => {
                    state.value.insert(state.cursor, c);
                    state.cursor += c.len_utf8();
                }
                SettingControl::Map(state) => {
                    state.new_key_text.insert(state.cursor, c);
                    state.cursor += c.len_utf8();
                }
                _ => {}
            }
        }
    }

    /// Backspace in the current editable control
    pub fn text_backspace(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.backspace(),
                SettingControl::Text(state) => {
                    if state.cursor > 0 {
                        let mut char_start = state.cursor - 1;
                        while char_start > 0 && !state.value.is_char_boundary(char_start) {
                            char_start -= 1;
                        }
                        state.value.remove(char_start);
                        state.cursor = char_start;
                    }
                }
                SettingControl::Map(state) => {
                    if state.cursor > 0 {
                        let mut char_start = state.cursor - 1;
                        while char_start > 0 && !state.new_key_text.is_char_boundary(char_start) {
                            char_start -= 1;
                        }
                        state.new_key_text.remove(char_start);
                        state.cursor = char_start;
                    }
                }
                _ => {}
            }
        }
    }

    /// Move cursor left in the current editable control
    pub fn text_move_left(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.move_left(),
                SettingControl::Text(state) => {
                    if state.cursor > 0 {
                        let mut new_pos = state.cursor - 1;
                        while new_pos > 0 && !state.value.is_char_boundary(new_pos) {
                            new_pos -= 1;
                        }
                        state.cursor = new_pos;
                    }
                }
                SettingControl::Map(state) => {
                    if state.cursor > 0 {
                        let mut new_pos = state.cursor - 1;
                        while new_pos > 0 && !state.new_key_text.is_char_boundary(new_pos) {
                            new_pos -= 1;
                        }
                        state.cursor = new_pos;
                    }
                }
                _ => {}
            }
        }
    }

    /// Move cursor right in the current editable control
    pub fn text_move_right(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.move_right(),
                SettingControl::Text(state) => {
                    if state.cursor < state.value.len() {
                        let mut new_pos = state.cursor + 1;
                        while new_pos < state.value.len() && !state.value.is_char_boundary(new_pos)
                        {
                            new_pos += 1;
                        }
                        state.cursor = new_pos;
                    }
                }
                SettingControl::Map(state) => {
                    if state.cursor < state.new_key_text.len() {
                        let mut new_pos = state.cursor + 1;
                        while new_pos < state.new_key_text.len()
                            && !state.new_key_text.is_char_boundary(new_pos)
                        {
                            new_pos += 1;
                        }
                        state.cursor = new_pos;
                    }
                }
                _ => {}
            }
        }
    }

    /// Move focus to previous item in TextList/Map (wraps within control)
    pub fn text_focus_prev(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.focus_prev(),
                SettingControl::Map(state) => {
                    state.focus_prev();
                }
                _ => {}
            }
        }
    }

    /// Move focus to next item in TextList/Map (wraps within control)
    pub fn text_focus_next(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.focus_next(),
                SettingControl::Map(state) => {
                    state.focus_next();
                }
                _ => {}
            }
        }
    }

    /// Add new item in TextList/Map (from the new item field)
    pub fn text_add_item(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => state.add_item(),
                SettingControl::Map(state) => state.add_entry_from_input(),
                _ => {}
            }
        }
        // Record the change
        self.on_value_changed();
    }

    /// Remove the currently focused item in TextList/Map
    pub fn text_remove_focused(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::TextList(state) => {
                    if let Some(idx) = state.focused_item {
                        state.remove_item(idx);
                    }
                }
                SettingControl::Map(state) => {
                    if let Some(idx) = state.focused_entry {
                        state.remove_entry(idx);
                    }
                }
                _ => {}
            }
        }
        // Record the change
        self.on_value_changed();
    }

    // =========== Dropdown methods ===========

    /// Check if current item is a dropdown with menu open
    pub fn is_dropdown_open(&self) -> bool {
        self.current_item().map_or(false, |item| {
            if let SettingControl::Dropdown(ref d) = item.control {
                d.open
            } else {
                false
            }
        })
    }

    /// Toggle dropdown open/closed
    pub fn dropdown_toggle(&mut self) {
        let mut opened = false;
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(ref mut d) = item.control {
                d.toggle_open();
                opened = d.open;
            }
        }

        // When dropdown opens, update content height and ensure it's visible
        if opened {
            // Update content height since item is now taller
            let selected_item = self.selected_item;
            if let Some(page) = self.pages.get(self.selected_category) {
                self.scroll_panel.update_content_height(&page.items);
                // Ensure the dropdown item is visible with its new expanded height
                self.scroll_panel
                    .ensure_focused_visible(&page.items, selected_item, None);
            }
        }
    }

    /// Select previous option in dropdown
    pub fn dropdown_prev(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(ref mut d) = item.control {
                d.select_prev();
            }
        }
    }

    /// Select next option in dropdown
    pub fn dropdown_next(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(ref mut d) = item.control {
                d.select_next();
            }
        }
    }

    /// Confirm dropdown selection (close and record change)
    pub fn dropdown_confirm(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(ref mut d) = item.control {
                d.confirm();
            }
        }
        self.on_value_changed();
    }

    /// Cancel dropdown (restore original value and close)
    pub fn dropdown_cancel(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(ref mut d) = item.control {
                d.cancel();
            }
        }
    }

    // =========== Number editing methods ===========

    /// Check if current item is a number input being edited
    pub fn is_number_editing(&self) -> bool {
        self.current_item().map_or(false, |item| {
            if let SettingControl::Number(ref n) = item.control {
                n.editing
            } else {
                false
            }
        })
    }

    /// Start number editing mode
    pub fn start_number_editing(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Number(ref mut n) = item.control {
                n.start_editing();
            }
        }
    }

    /// Insert a character into number input
    pub fn number_insert(&mut self, c: char) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Number(ref mut n) = item.control {
                n.insert_char(c);
            }
        }
    }

    /// Backspace in number input
    pub fn number_backspace(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Number(ref mut n) = item.control {
                n.backspace();
            }
        }
    }

    /// Confirm number editing
    pub fn number_confirm(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Number(ref mut n) = item.control {
                n.confirm_editing();
            }
        }
        self.on_value_changed();
    }

    /// Cancel number editing
    pub fn number_cancel(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Number(ref mut n) = item.control {
                n.cancel_editing();
            }
        }
    }

    /// Get list of pending changes for display
    pub fn get_change_descriptions(&self) -> Vec<String> {
        self.pending_changes
            .iter()
            .map(|(path, value)| {
                let value_str = match value {
                    serde_json::Value::Bool(b) => b.to_string(),
                    serde_json::Value::Number(n) => n.to_string(),
                    serde_json::Value::String(s) => format!("\"{}\"", s),
                    _ => value.to_string(),
                };
                format!("{}: {}", path, value_str)
            })
            .collect()
    }
}

/// Update a control's state from a JSON value
fn update_control_from_value(control: &mut SettingControl, value: &serde_json::Value) {
    match control {
        SettingControl::Toggle(state) => {
            if let Some(b) = value.as_bool() {
                state.checked = b;
            }
        }
        SettingControl::Number(state) => {
            if let Some(n) = value.as_i64() {
                state.value = n;
            }
        }
        SettingControl::Dropdown(state) => {
            if let Some(s) = value.as_str() {
                if let Some(idx) = state.options.iter().position(|o| o == s) {
                    state.selected = idx;
                }
            }
        }
        SettingControl::Text(state) => {
            if let Some(s) = value.as_str() {
                state.value = s.to_string();
                state.cursor = state.value.len();
            }
        }
        SettingControl::TextList(state) => {
            if let Some(arr) = value.as_array() {
                state.items = arr
                    .iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect();
            }
        }
        SettingControl::Map(state) => {
            if let Some(obj) = value.as_object() {
                state.entries = obj.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                state.entries.sort_by(|a, b| a.0.cmp(&b.0));
            }
        }
        SettingControl::KeybindingList(state) => {
            if let Some(arr) = value.as_array() {
                state.bindings = arr.clone();
            }
        }
        SettingControl::Json(state) => {
            // Re-create from value with pretty printing
            let json_str =
                serde_json::to_string_pretty(value).unwrap_or_else(|_| "null".to_string());
            let json_str = if json_str.is_empty() {
                "null".to_string()
            } else {
                json_str
            };
            state.original_text = json_str.clone();
            state.editor.set_value(&json_str);
            state.scroll_offset = 0;
        }
        SettingControl::Complex { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_SCHEMA: &str = r#"
{
  "type": "object",
  "properties": {
    "theme": {
      "type": "string",
      "default": "dark"
    },
    "line_numbers": {
      "type": "boolean",
      "default": true
    }
  },
  "$defs": {}
}
"#;

    fn test_config() -> Config {
        Config::default()
    }

    #[test]
    fn test_settings_state_creation() {
        let config = test_config();
        let state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        assert!(!state.visible);
        assert_eq!(state.selected_category, 0);
        assert!(!state.has_changes());
    }

    #[test]
    fn test_navigation() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        // Start in category focus
        assert_eq!(state.focus_panel, FocusPanel::Categories);

        // Toggle to settings
        state.toggle_focus();
        assert_eq!(state.focus_panel, FocusPanel::Settings);

        // Navigate items
        state.select_next();
        assert_eq!(state.selected_item, 1);

        state.select_prev();
        assert_eq!(state.selected_item, 0);
    }

    #[test]
    fn test_pending_changes() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        assert!(!state.has_changes());

        state.set_pending_change("/theme", serde_json::Value::String("light".to_string()));
        assert!(state.has_changes());

        state.discard_changes();
        assert!(!state.has_changes());
    }

    #[test]
    fn test_show_hide() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        assert!(!state.visible);

        state.show();
        assert!(state.visible);
        assert_eq!(state.focus_panel, FocusPanel::Categories);

        state.hide();
        assert!(!state.visible);
    }

    // Schema with dropdown (enum) and number controls for testing
    const TEST_SCHEMA_CONTROLS: &str = r#"
{
  "type": "object",
  "properties": {
    "theme": {
      "type": "string",
      "enum": ["dark", "light", "high-contrast"],
      "default": "dark"
    },
    "tab_size": {
      "type": "integer",
      "minimum": 1,
      "maximum": 8,
      "default": 4
    },
    "line_numbers": {
      "type": "boolean",
      "default": true
    }
  },
  "$defs": {}
}
"#;

    #[test]
    fn test_dropdown_toggle() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        state.show();
        state.toggle_focus(); // Move to settings

        // Items are sorted alphabetically: line_numbers, tab_size, theme
        // Navigate to theme (dropdown) at index 2
        state.select_next();
        state.select_next();
        assert!(!state.is_dropdown_open());

        state.dropdown_toggle();
        assert!(state.is_dropdown_open());

        state.dropdown_toggle();
        assert!(!state.is_dropdown_open());
    }

    #[test]
    fn test_dropdown_cancel_restores() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        state.show();
        state.toggle_focus();

        // Items are sorted alphabetically: line_numbers, tab_size, theme
        // Navigate to theme (dropdown) at index 2
        state.select_next();
        state.select_next();

        // Open dropdown
        state.dropdown_toggle();
        assert!(state.is_dropdown_open());

        // Get initial selection
        let initial = state.current_item().and_then(|item| {
            if let SettingControl::Dropdown(ref d) = item.control {
                Some(d.selected)
            } else {
                None
            }
        });

        // Change selection
        state.dropdown_next();
        let after_change = state.current_item().and_then(|item| {
            if let SettingControl::Dropdown(ref d) = item.control {
                Some(d.selected)
            } else {
                None
            }
        });
        assert_ne!(initial, after_change);

        // Cancel - should restore
        state.dropdown_cancel();
        assert!(!state.is_dropdown_open());

        let after_cancel = state.current_item().and_then(|item| {
            if let SettingControl::Dropdown(ref d) = item.control {
                Some(d.selected)
            } else {
                None
            }
        });
        assert_eq!(initial, after_cancel);
    }

    #[test]
    fn test_dropdown_confirm_keeps_selection() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        state.show();
        state.toggle_focus();

        // Open dropdown
        state.dropdown_toggle();

        // Change selection
        state.dropdown_next();
        let after_change = state.current_item().and_then(|item| {
            if let SettingControl::Dropdown(ref d) = item.control {
                Some(d.selected)
            } else {
                None
            }
        });

        // Confirm - should keep new selection
        state.dropdown_confirm();
        assert!(!state.is_dropdown_open());

        let after_confirm = state.current_item().and_then(|item| {
            if let SettingControl::Dropdown(ref d) = item.control {
                Some(d.selected)
            } else {
                None
            }
        });
        assert_eq!(after_change, after_confirm);
    }

    #[test]
    fn test_number_editing() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        state.show();
        state.toggle_focus();

        // Navigate to tab_size (second item)
        state.select_next();

        // Should not be editing yet
        assert!(!state.is_number_editing());

        // Start editing
        state.start_number_editing();
        assert!(state.is_number_editing());

        // Insert characters
        state.number_insert('8');

        // Confirm
        state.number_confirm();
        assert!(!state.is_number_editing());
    }

    #[test]
    fn test_number_cancel_editing() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        state.show();
        state.toggle_focus();

        // Navigate to tab_size
        state.select_next();

        // Get initial value
        let initial_value = state.current_item().and_then(|item| {
            if let SettingControl::Number(ref n) = item.control {
                Some(n.value)
            } else {
                None
            }
        });

        // Start editing and make changes
        state.start_number_editing();
        state.number_backspace();
        state.number_insert('9');
        state.number_insert('9');

        // Cancel
        state.number_cancel();
        assert!(!state.is_number_editing());

        // Value should be unchanged (edit text was just cleared)
        let after_cancel = state.current_item().and_then(|item| {
            if let SettingControl::Number(ref n) = item.control {
                Some(n.value)
            } else {
                None
            }
        });
        assert_eq!(initial_value, after_cancel);
    }

    #[test]
    fn test_number_backspace() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        state.show();
        state.toggle_focus();
        state.select_next();

        state.start_number_editing();
        state.number_backspace();

        // Check edit text was modified
        let edit_text = state.current_item().and_then(|item| {
            if let SettingControl::Number(ref n) = item.control {
                Some(n.edit_text.clone())
            } else {
                None
            }
        });
        // Original "4" should have last char removed, leaving ""
        assert_eq!(edit_text, Some(String::new()));

        state.number_cancel();
    }
}
