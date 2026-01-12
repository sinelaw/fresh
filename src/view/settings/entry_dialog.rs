//! Entry detail dialog for editing complex map entries
//!
//! Provides a modal dialog for editing complex map entries using the same
//! SettingItem/SettingControl infrastructure as the main settings UI.

use super::items::{build_item_from_value, control_to_value, SettingControl, SettingItem};
use super::schema::{SettingSchema, SettingType};
use crate::view::controls::{FocusState, TextInputState};
use serde_json::Value;

/// State for the entry detail dialog
#[derive(Debug, Clone)]
pub struct EntryDialogState {
    /// The entry key (e.g., "rust" for language)
    pub entry_key: String,
    /// The map path this entry belongs to (e.g., "/languages", "/lsp")
    pub map_path: String,
    /// Human-readable title for the dialog
    pub title: String,
    /// Whether this is a new entry (vs editing existing)
    pub is_new: bool,
    /// Items in the dialog (using same SettingItem structure as main settings)
    pub items: Vec<SettingItem>,
    /// Currently selected item index
    pub selected_item: usize,
    /// Sub-focus index within the selected item (for TextList/Map navigation)
    pub sub_focus: Option<usize>,
    /// Whether we're in text editing mode
    pub editing_text: bool,
    /// Currently focused button (0=Save, 1=Delete, 2=Cancel for existing; 0=Save, 1=Cancel for new)
    pub focused_button: usize,
    /// Whether focus is on buttons (true) or items (false)
    pub focus_on_buttons: bool,
    /// Whether deletion was requested
    pub delete_requested: bool,
    /// Scroll offset for the items area
    pub scroll_offset: usize,
    /// Last known viewport height (updated during render)
    pub viewport_height: usize,
    /// Hovered item index (for mouse hover feedback)
    pub hover_item: Option<usize>,
    /// Hovered button index (for mouse hover feedback)
    pub hover_button: Option<usize>,
    /// Original value when dialog was opened (for Cancel to restore)
    pub original_value: Value,
    /// Index of first editable item (items before this are read-only)
    /// Used for rendering separator and focus navigation
    pub first_editable_index: usize,
    /// Whether deletion is disabled (for auto-managed entries like plugins)
    pub no_delete: bool,
}

impl EntryDialogState {
    /// Create a dialog from a schema definition
    ///
    /// This is the primary, schema-driven constructor. It builds items
    /// dynamically from the SettingSchema's properties using the same
    /// build logic as the main settings UI.
    pub fn from_schema(
        key: String,
        value: &Value,
        schema: &SettingSchema,
        map_path: &str,
        is_new: bool,
        no_delete: bool,
    ) -> Self {
        let mut items = Vec::new();

        // Add key field as first item (read-only for existing entries)
        let key_item = SettingItem {
            path: "__key__".to_string(),
            name: "Key".to_string(),
            description: Some("unique identifier for this entry".to_string()),
            control: SettingControl::Text(TextInputState::new("Key").with_value(&key)),
            default: None,
            modified: false,
            layer_source: crate::config_io::ConfigLayer::System,
            read_only: !is_new, // Key is editable only for new entries
            is_auto_managed: false,
        };
        items.push(key_item);

        // Add schema-driven items from object properties
        if let SettingType::Object { properties } = &schema.setting_type {
            for prop in properties {
                let field_name = prop.path.trim_start_matches('/');
                let field_value = value.get(field_name);
                let item = build_item_from_value(prop, field_value);
                items.push(item);
            }
        }

        // Sort items: read-only first, then editable
        items.sort_by_key(|item| !item.read_only);

        // Find the first editable item index
        let first_editable_index = items
            .iter()
            .position(|item| !item.read_only)
            .unwrap_or(items.len());

        // If all items are read-only, start with focus on buttons
        let focus_on_buttons = first_editable_index >= items.len();
        let selected_item = if focus_on_buttons {
            0
        } else {
            first_editable_index
        };

        let title = if is_new {
            format!("Add {}", schema.name)
        } else {
            format!("Edit {}", schema.name)
        };

        Self {
            entry_key: key,
            map_path: map_path.to_string(),
            title,
            is_new,
            items,
            selected_item,
            sub_focus: None,
            editing_text: false,
            focused_button: 0,
            focus_on_buttons,
            delete_requested: false,
            scroll_offset: 0,
            viewport_height: 20, // Default, updated during render
            hover_item: None,
            hover_button: None,
            original_value: value.clone(),
            first_editable_index,
            no_delete,
        }
    }

    /// Create a dialog for an array item (no key field)
    ///
    /// Used for ObjectArray controls where items are identified by index, not key.
    pub fn for_array_item(
        index: Option<usize>,
        value: &Value,
        schema: &SettingSchema,
        array_path: &str,
        is_new: bool,
    ) -> Self {
        let mut items = Vec::new();

        // Add schema-driven items from object properties (no key field for arrays)
        if let SettingType::Object { properties } = &schema.setting_type {
            for prop in properties {
                let field_name = prop.path.trim_start_matches('/');
                let field_value = value.get(field_name);
                let item = build_item_from_value(prop, field_value);
                items.push(item);
            }
        }

        // Sort items: read-only first, then editable
        items.sort_by_key(|item| !item.read_only);

        // Find the first editable item index
        let first_editable_index = items
            .iter()
            .position(|item| !item.read_only)
            .unwrap_or(items.len());

        // If all items are read-only, start with focus on buttons
        let focus_on_buttons = first_editable_index >= items.len();
        let selected_item = if focus_on_buttons {
            0
        } else {
            first_editable_index
        };

        let title = if is_new {
            format!("Add {}", schema.name)
        } else {
            format!("Edit {}", schema.name)
        };

        Self {
            entry_key: index.map_or(String::new(), |i| i.to_string()),
            map_path: array_path.to_string(),
            title,
            is_new,
            items,
            selected_item,
            sub_focus: None,
            editing_text: false,
            focused_button: 0,
            focus_on_buttons,
            delete_requested: false,
            scroll_offset: 0,
            viewport_height: 20,
            hover_item: None,
            hover_button: None,
            original_value: value.clone(),
            first_editable_index,
            no_delete: false, // Arrays typically allow deletion
        }
    }

    /// Get the current key value from the key item
    pub fn get_key(&self) -> String {
        // Find the key item by path (may not be first after sorting)
        for item in &self.items {
            if item.path == "__key__" {
                if let SettingControl::Text(state) = &item.control {
                    return state.value.clone();
                }
            }
        }
        self.entry_key.clone()
    }

    /// Get button count (3 for existing entries with Delete, 2 for new/no_delete entries)
    pub fn button_count(&self) -> usize {
        if self.is_new || self.no_delete {
            2 // Save, Cancel (no Delete for new entries or when no_delete is set)
        } else {
            3
        }
    }

    /// Convert dialog state back to JSON value (excludes the __key__ item)
    pub fn to_value(&self) -> Value {
        let mut obj = serde_json::Map::new();

        for item in &self.items {
            // Skip the special key item - it's stored separately
            if item.path == "__key__" {
                continue;
            }

            let field_name = item.path.trim_start_matches('/');
            let value = control_to_value(&item.control);
            obj.insert(field_name.to_string(), value);
        }

        Value::Object(obj)
    }

    /// Get currently selected item
    pub fn current_item(&self) -> Option<&SettingItem> {
        if self.focus_on_buttons {
            None
        } else {
            self.items.get(self.selected_item)
        }
    }

    /// Get currently selected item mutably
    pub fn current_item_mut(&mut self) -> Option<&mut SettingItem> {
        if self.focus_on_buttons {
            None
        } else {
            self.items.get_mut(self.selected_item)
        }
    }

    /// Move focus to next item or button
    pub fn focus_next(&mut self) {
        if self.editing_text {
            return; // Don't change focus while editing
        }

        if self.focus_on_buttons {
            if self.focused_button + 1 < self.button_count() {
                // Move to next button
                self.focused_button += 1;
            } else {
                // Wrap to first editable item (skip read-only items)
                if self.first_editable_index < self.items.len() {
                    self.focus_on_buttons = false;
                    self.selected_item = self.first_editable_index;
                }
                // If all items are read-only, stay on buttons (don't wrap)
            }
        } else {
            // Check if current item is an ObjectArray that can navigate internally
            let array_nav_result = self
                .items
                .get(self.selected_item)
                .and_then(|item| {
                    if let SettingControl::ObjectArray(state) = &item.control {
                        // Navigation order: entries -> add-new -> exit
                        match state.focused_index {
                            Some(idx) if idx + 1 < state.bindings.len() => {
                                // On entry, can go to next entry
                                Some(true)
                            }
                            Some(_) => {
                                // On last entry, can go to add-new
                                Some(true)
                            }
                            None => {
                                // On add-new, exit to next dialog item
                                Some(false)
                            }
                        }
                    } else {
                        None
                    }
                });

            match array_nav_result {
                Some(true) => {
                    // Navigate within the ObjectArray
                    if let Some(item) = self.items.get_mut(self.selected_item) {
                        if let SettingControl::ObjectArray(state) = &mut item.control {
                            state.focus_next();
                        }
                    }
                }
                Some(false) => {
                    // Exit ObjectArray, go to next item
                    if self.selected_item + 1 < self.items.len() {
                        self.selected_item += 1;
                        self.sub_focus = None;
                        // Initialize next item's ObjectArray if it has entries
                        self.init_object_array_focus();
                    } else {
                        self.focus_on_buttons = true;
                        self.focused_button = 0;
                    }
                }
                None => {
                    // Not an ObjectArray, normal navigation
                    // All items after first_editable_index are editable (sorted)
                    if self.selected_item + 1 < self.items.len() {
                        self.selected_item += 1;
                        self.sub_focus = None;
                        // Initialize next item's ObjectArray if it has entries
                        self.init_object_array_focus();
                    } else {
                        self.focus_on_buttons = true;
                        self.focused_button = 0;
                    }
                }
            }
        }

        self.update_focus_states();
        self.ensure_selected_visible(self.viewport_height);
    }

    /// Move focus to previous item or button
    pub fn focus_prev(&mut self) {
        if self.editing_text {
            return; // Don't change focus while editing
        }

        if self.focus_on_buttons {
            if self.focused_button > 0 {
                self.focused_button -= 1;
            } else {
                // Move back to last editable item
                if self.first_editable_index < self.items.len() {
                    self.focus_on_buttons = false;
                    self.selected_item = self.items.len().saturating_sub(1);
                }
                // If all items are read-only, stay on buttons (don't wrap)
            }
        } else {
            // Check if current item is an ObjectArray that can navigate internally
            let array_nav_result = self
                .items
                .get(self.selected_item)
                .and_then(|item| {
                    if let SettingControl::ObjectArray(state) = &item.control {
                        // Navigation order (reverse): exit <- entries <- add-new
                        match state.focused_index {
                            None => {
                                // On add-new, can go back to last entry (if any)
                                if !state.bindings.is_empty() {
                                    Some(true)
                                } else {
                                    Some(false) // No entries, exit
                                }
                            }
                            Some(0) => {
                                // On first entry, exit to previous dialog item
                                Some(false)
                            }
                            Some(_) => {
                                // On entry, can go to previous entry
                                Some(true)
                            }
                        }
                    } else {
                        None
                    }
                });

            match array_nav_result {
                Some(true) => {
                    // Navigate within the ObjectArray
                    if let Some(item) = self.items.get_mut(self.selected_item) {
                        if let SettingControl::ObjectArray(state) = &mut item.control {
                            state.focus_prev();
                        }
                    }
                }
                Some(false) => {
                    // Exit ObjectArray, go to previous editable item (not into read-only)
                    if self.selected_item > self.first_editable_index {
                        self.selected_item -= 1;
                        self.sub_focus = None;
                        // Initialize previous item's ObjectArray to add-new (end)
                        self.init_object_array_focus_end();
                    } else {
                        // At first editable item, go to buttons
                        self.focus_on_buttons = true;
                        self.focused_button = self.button_count().saturating_sub(1);
                    }
                }
                None => {
                    // Not an ObjectArray, normal navigation
                    // Don't go below first_editable_index (read-only items)
                    if self.selected_item > self.first_editable_index {
                        self.selected_item -= 1;
                        self.sub_focus = None;
                        // Initialize previous item's ObjectArray to add-new (end)
                        self.init_object_array_focus_end();
                    } else {
                        // At first editable item, go to buttons
                        self.focus_on_buttons = true;
                        self.focused_button = self.button_count().saturating_sub(1);
                    }
                }
            }
        }

        self.update_focus_states();
        self.ensure_selected_visible(self.viewport_height);
    }

    /// Initialize ObjectArray focus to first entry (when arriving from above)
    fn init_object_array_focus(&mut self) {
        if let Some(item) = self.items.get_mut(self.selected_item) {
            if let SettingControl::ObjectArray(state) = &mut item.control {
                // Start at first entry if there are any, otherwise stay on add-new
                if !state.bindings.is_empty() {
                    state.focused_index = Some(0);
                }
            }
        }
    }

    /// Initialize ObjectArray focus to add-new (when arriving from below)
    fn init_object_array_focus_end(&mut self) {
        if let Some(item) = self.items.get_mut(self.selected_item) {
            if let SettingControl::ObjectArray(state) = &mut item.control {
                // Start at add-new row (None)
                state.focused_index = None;
            }
        }
    }

    /// Move to next sub-item within current control (for TextList, Map)
    pub fn sub_focus_next(&mut self) {
        if let Some(item) = self.items.get(self.selected_item) {
            let max_sub = match &item.control {
                SettingControl::TextList(state) => state.items.len(), // +1 for add-new
                SettingControl::Map(state) => state.entries.len(),    // +1 for add-new
                _ => 0,
            };

            if max_sub > 0 {
                let current = self.sub_focus.unwrap_or(0);
                if current < max_sub {
                    self.sub_focus = Some(current + 1);
                } else {
                    // Move to next item
                    self.sub_focus = None;
                    self.focus_next();
                }
            } else {
                self.focus_next();
            }
        } else {
            self.focus_next();
        }
    }

    /// Move to previous sub-item within current control
    pub fn sub_focus_prev(&mut self) {
        if let Some(sub) = self.sub_focus {
            if sub > 0 {
                self.sub_focus = Some(sub - 1);
            } else {
                self.sub_focus = None;
            }
        } else {
            self.focus_prev();
        }
    }

    /// Update focus states for all items
    pub fn update_focus_states(&mut self) {
        for (idx, item) in self.items.iter_mut().enumerate() {
            let state = if !self.focus_on_buttons && idx == self.selected_item {
                FocusState::Focused
            } else {
                FocusState::Normal
            };

            match &mut item.control {
                SettingControl::Toggle(s) => s.focus = state,
                SettingControl::Number(s) => s.focus = state,
                SettingControl::Dropdown(s) => s.focus = state,
                SettingControl::Text(s) => s.focus = state,
                SettingControl::TextList(s) => s.focus = state,
                SettingControl::Map(s) => s.focus = state,
                SettingControl::ObjectArray(s) => s.focus = state,
                SettingControl::Json(s) => s.focus = state,
                SettingControl::Complex { .. } => {}
            }
        }
    }

    /// Calculate total content height for all items (including separator)
    pub fn total_content_height(&self) -> usize {
        let items_height: usize = self
            .items
            .iter()
            .map(|item| item.control.control_height() as usize)
            .sum();
        // Add 1 for separator if we have both read-only and editable items
        let separator_height =
            if self.first_editable_index > 0 && self.first_editable_index < self.items.len() {
                1
            } else {
                0
            };
        items_height + separator_height
    }

    /// Calculate the Y offset of the selected item (including separator)
    pub fn selected_item_offset(&self) -> usize {
        let items_offset: usize = self
            .items
            .iter()
            .take(self.selected_item)
            .map(|item| item.control.control_height() as usize)
            .sum();
        // Add 1 for separator if selected item is after it
        let separator_offset = if self.first_editable_index > 0
            && self.first_editable_index < self.items.len()
            && self.selected_item >= self.first_editable_index
        {
            1
        } else {
            0
        };
        items_offset + separator_offset
    }

    /// Calculate the height of the selected item
    pub fn selected_item_height(&self) -> usize {
        self.items
            .get(self.selected_item)
            .map(|item| item.control.control_height() as usize)
            .unwrap_or(1)
    }

    /// Ensure the selected item is visible within the viewport
    pub fn ensure_selected_visible(&mut self, viewport_height: usize) {
        if self.focus_on_buttons {
            // Scroll to bottom when buttons are focused
            let total = self.total_content_height();
            if total > viewport_height {
                self.scroll_offset = total.saturating_sub(viewport_height);
            }
            return;
        }

        let item_start = self.selected_item_offset();
        let item_end = item_start + self.selected_item_height();

        // If item starts before viewport, scroll up
        if item_start < self.scroll_offset {
            self.scroll_offset = item_start;
        }
        // If item ends after viewport, scroll down
        else if item_end > self.scroll_offset + viewport_height {
            self.scroll_offset = item_end.saturating_sub(viewport_height);
        }
    }

    /// Ensure the cursor within a JSON editor is visible
    ///
    /// When editing a multiline JSON control, this adjusts scroll_offset
    /// to keep the cursor row visible within the viewport.
    pub fn ensure_cursor_visible(&mut self) {
        if !self.editing_text || self.focus_on_buttons {
            return;
        }

        // Get cursor row from current item (if it's a JSON editor)
        let cursor_row = if let Some(item) = self.items.get(self.selected_item) {
            if let SettingControl::Json(state) = &item.control {
                state.cursor_pos().0
            } else {
                return; // Not a JSON editor
            }
        } else {
            return;
        };

        // Calculate absolute position of cursor row in content:
        // item_offset + 1 (for label row) + cursor_row
        let item_offset = self.selected_item_offset();
        let cursor_content_row = item_offset + 1 + cursor_row;

        let viewport_height = self.viewport_height;

        // If cursor is above viewport, scroll up
        if cursor_content_row < self.scroll_offset {
            self.scroll_offset = cursor_content_row;
        }
        // If cursor is below viewport, scroll down
        else if cursor_content_row >= self.scroll_offset + viewport_height {
            self.scroll_offset = cursor_content_row.saturating_sub(viewport_height) + 1;
        }
    }

    /// Scroll up by one line
    pub fn scroll_up(&mut self) {
        self.scroll_offset = self.scroll_offset.saturating_sub(1);
    }

    /// Scroll down by one line
    pub fn scroll_down(&mut self, viewport_height: usize) {
        let max_scroll = self.total_content_height().saturating_sub(viewport_height);
        if self.scroll_offset < max_scroll {
            self.scroll_offset += 1;
        }
    }

    /// Scroll to a position based on ratio (0.0 = top, 1.0 = bottom)
    ///
    /// Used for scrollbar drag operations.
    pub fn scroll_to_ratio(&mut self, ratio: f32) {
        let max_scroll = self
            .total_content_height()
            .saturating_sub(self.viewport_height);
        let new_offset = (ratio * max_scroll as f32).round() as usize;
        self.scroll_offset = new_offset.min(max_scroll);
    }

    /// Start text editing mode for the current control
    pub fn start_editing(&mut self) {
        if let Some(item) = self.current_item_mut() {
            // Don't allow editing read-only fields
            if item.read_only {
                return;
            }
            match &mut item.control {
                SettingControl::Text(state) => {
                    // TextInputState uses focus state, cursor is already at end from with_value
                    state.cursor = state.value.len();
                    self.editing_text = true;
                }
                SettingControl::TextList(state) => {
                    // Focus on the new item input by default
                    state.focus_new_item();
                    self.editing_text = true;
                }
                SettingControl::Number(state) => {
                    state.start_editing();
                    self.editing_text = true;
                }
                SettingControl::Json(_) => {
                    // JSON editor is always ready to edit, just set the flag
                    self.editing_text = true;
                }
                _ => {}
            }
        }
    }

    /// Stop text editing mode
    pub fn stop_editing(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Number(state) = &mut item.control {
                state.cancel_editing();
            }
        }
        self.editing_text = false;
    }

    /// Handle character input
    pub fn insert_char(&mut self, c: char) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.insert(c);
                }
                SettingControl::TextList(state) => {
                    state.insert(c);
                }
                SettingControl::Number(state) => {
                    state.insert_char(c);
                }
                SettingControl::Json(state) => {
                    state.insert(c);
                }
                _ => {}
            }
        }
    }

    pub fn insert_str(&mut self, s: &str) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.insert_str(s);
                }
                SettingControl::TextList(state) => {
                    state.insert_str(s);
                }
                SettingControl::Number(state) => {
                    for c in s.chars() {
                        state.insert_char(c);
                    }
                }
                SettingControl::Json(state) => {
                    state.insert_str(s);
                }
                _ => {}
            }
        }
    }

    /// Handle backspace
    pub fn backspace(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.backspace();
                }
                SettingControl::TextList(state) => {
                    state.backspace();
                }
                SettingControl::Number(state) => {
                    state.backspace();
                }
                SettingControl::Json(state) => {
                    state.backspace();
                }
                _ => {}
            }
        }
    }

    /// Handle cursor left
    pub fn cursor_left(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.move_left();
                }
                SettingControl::TextList(state) => {
                    state.move_left();
                }
                SettingControl::Json(state) => {
                    state.move_left();
                }
                _ => {}
            }
        }
    }

    /// Handle cursor left with selection (Shift+Left)
    pub fn cursor_left_selecting(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.editor.move_left_selecting();
            }
        }
    }

    /// Handle cursor right
    pub fn cursor_right(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.move_right();
                }
                SettingControl::TextList(state) => {
                    state.move_right();
                }
                SettingControl::Json(state) => {
                    state.move_right();
                }
                _ => {}
            }
        }
    }

    /// Handle cursor right with selection (Shift+Right)
    pub fn cursor_right_selecting(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.editor.move_right_selecting();
            }
        }
    }

    /// Handle cursor up (for multiline controls)
    pub fn cursor_up(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.move_up();
            }
        }
        self.ensure_cursor_visible();
    }

    /// Handle cursor up with selection (Shift+Up)
    pub fn cursor_up_selecting(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.editor.move_up_selecting();
            }
        }
        self.ensure_cursor_visible();
    }

    /// Handle cursor down (for multiline controls)
    pub fn cursor_down(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.move_down();
            }
        }
        self.ensure_cursor_visible();
    }

    /// Handle cursor down with selection (Shift+Down)
    pub fn cursor_down_selecting(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.editor.move_down_selecting();
            }
        }
        self.ensure_cursor_visible();
    }

    /// Insert newline in JSON editor
    pub fn insert_newline(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.insert('\n');
            }
        }
    }

    /// Revert JSON changes to original and stop editing
    pub fn revert_json_and_stop(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.revert();
            }
        }
        self.editing_text = false;
    }

    /// Check if current control is a JSON editor
    pub fn is_editing_json(&self) -> bool {
        if !self.editing_text {
            return false;
        }
        self.current_item()
            .map(|item| matches!(&item.control, SettingControl::Json(_)))
            .unwrap_or(false)
    }

    /// Toggle boolean value
    pub fn toggle_bool(&mut self) {
        if let Some(item) = self.current_item_mut() {
            // Don't allow toggling read-only fields
            if item.read_only {
                return;
            }
            if let SettingControl::Toggle(state) = &mut item.control {
                state.checked = !state.checked;
            }
        }
    }

    /// Toggle dropdown open state
    pub fn toggle_dropdown(&mut self) {
        if let Some(item) = self.current_item_mut() {
            // Don't allow editing read-only fields
            if item.read_only {
                return;
            }
            if let SettingControl::Dropdown(state) = &mut item.control {
                state.open = !state.open;
            }
        }
    }

    /// Move dropdown selection up
    pub fn dropdown_prev(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(state) = &mut item.control {
                if state.open {
                    state.select_prev();
                }
            }
        }
    }

    /// Move dropdown selection down
    pub fn dropdown_next(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(state) = &mut item.control {
                if state.open {
                    state.select_next();
                }
            }
        }
    }

    /// Confirm dropdown selection
    pub fn dropdown_confirm(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Dropdown(state) = &mut item.control {
                state.open = false;
            }
        }
    }

    /// Increment number value
    pub fn increment_number(&mut self) {
        if let Some(item) = self.current_item_mut() {
            // Don't allow editing read-only fields
            if item.read_only {
                return;
            }
            if let SettingControl::Number(state) = &mut item.control {
                state.increment();
            }
        }
    }

    /// Decrement number value
    pub fn decrement_number(&mut self) {
        if let Some(item) = self.current_item_mut() {
            // Don't allow editing read-only fields
            if item.read_only {
                return;
            }
            if let SettingControl::Number(state) = &mut item.control {
                state.decrement();
            }
        }
    }

    /// Delete the currently focused item from a TextList control
    pub fn delete_list_item(&mut self) {
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::TextList(state) = &mut item.control {
                // Remove the currently focused item if any
                if let Some(idx) = state.focused_item {
                    state.remove_item(idx);
                }
            }
        }
    }

    /// Delete character at cursor (forward delete)
    pub fn delete(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.delete();
                }
                SettingControl::TextList(state) => {
                    state.delete();
                }
                SettingControl::Json(state) => {
                    state.delete();
                }
                _ => {}
            }
        }
    }

    /// Move cursor to beginning of line
    pub fn cursor_home(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.move_home();
                }
                SettingControl::TextList(state) => {
                    state.move_home();
                }
                SettingControl::Json(state) => {
                    state.move_home();
                }
                _ => {}
            }
        }
    }

    /// Move cursor to end of line
    pub fn cursor_end(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Text(state) => {
                    state.move_end();
                }
                SettingControl::TextList(state) => {
                    state.move_end();
                }
                SettingControl::Json(state) => {
                    state.move_end();
                }
                _ => {}
            }
        }
    }

    /// Select all text in current control
    pub fn select_all(&mut self) {
        if !self.editing_text {
            return;
        }
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Json(state) = &mut item.control {
                state.select_all();
            }
            // Note: Text and TextList don't have select_all implemented
        }
    }

    /// Check if any field is currently in edit mode
    pub fn is_editing(&self) -> bool {
        self.editing_text
            || self
                .current_item()
                .map(|item| {
                    matches!(
                        &item.control,
                        SettingControl::Dropdown(s) if s.open
                    )
                })
                .unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_schema() -> SettingSchema {
        SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: Some("Test schema".to_string()),
            setting_type: SettingType::Object {
                properties: vec![
                    SettingSchema {
                        path: "/enabled".to_string(),
                        name: "Enabled".to_string(),
                        description: Some("Enable this".to_string()),
                        setting_type: SettingType::Boolean,
                        default: Some(serde_json::json!(true)),
                        read_only: false,
                    },
                    SettingSchema {
                        path: "/command".to_string(),
                        name: "Command".to_string(),
                        description: Some("Command to run".to_string()),
                        setting_type: SettingType::String,
                        default: Some(serde_json::json!("")),
                        read_only: false,
                    },
                ],
            },
            default: None,
            read_only: false,
        }
    }

    #[test]
    fn from_schema_creates_key_item_first() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            false,
        );

        assert!(!dialog.items.is_empty());
        assert_eq!(dialog.items[0].path, "__key__");
        assert_eq!(dialog.items[0].name, "Key");
    }

    #[test]
    fn from_schema_creates_items_from_properties() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({"enabled": true, "command": "test-cmd"}),
            &schema,
            "/test",
            false,
            false,
        );

        // Key + 2 properties = 3 items
        assert_eq!(dialog.items.len(), 3);
        assert_eq!(dialog.items[1].name, "Enabled");
        assert_eq!(dialog.items[2].name, "Command");
    }

    #[test]
    fn get_key_returns_key_value() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "mykey".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            false,
        );

        assert_eq!(dialog.get_key(), "mykey");
    }

    #[test]
    fn to_value_excludes_key() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({"enabled": true, "command": "cmd"}),
            &schema,
            "/test",
            false,
            false,
        );

        let value = dialog.to_value();
        assert!(value.get("__key__").is_none());
        assert!(value.get("enabled").is_some());
    }

    #[test]
    fn focus_navigation_works() {
        let schema = create_test_schema();
        let mut dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false, // existing entry - Key is read-only
            false, // allow delete
        );

        // With is_new=false, Key is read-only and sorted first
        // Items: [Key (read-only), Enabled, Command]
        // Focus starts at first editable item (index 1)
        assert_eq!(dialog.first_editable_index, 1);
        assert_eq!(dialog.selected_item, 1); // First editable (Enabled)
        assert!(!dialog.focus_on_buttons);

        dialog.focus_next();
        assert_eq!(dialog.selected_item, 2); // Command

        dialog.focus_next();
        assert!(dialog.focus_on_buttons); // No more editable items
        assert_eq!(dialog.focused_button, 0);

        // Going back should skip read-only Key
        dialog.focus_prev();
        assert!(!dialog.focus_on_buttons);
        assert_eq!(dialog.selected_item, 2); // Last editable (Command)

        dialog.focus_prev();
        assert_eq!(dialog.selected_item, 1); // First editable (Enabled)

        dialog.focus_prev();
        assert!(dialog.focus_on_buttons); // Wraps to buttons, not to read-only Key
    }

    #[test]
    fn button_count_differs_for_new_vs_existing() {
        let schema = create_test_schema();

        let new_dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            true,
            false,
        );
        assert_eq!(new_dialog.button_count(), 2); // Save, Cancel

        let existing_dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            false, // allow delete
        );
        assert_eq!(existing_dialog.button_count(), 3); // Save, Delete, Cancel

        // no_delete hides the Delete button even for existing entries
        let no_delete_dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            true, // no delete (auto-managed entries like plugins)
        );
        assert_eq!(no_delete_dialog.button_count(), 2); // Save, Cancel (no Delete)
    }
}
