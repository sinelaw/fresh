//! Setting items for the UI
//!
//! Converts schema information into renderable setting items.

use super::schema::{SettingCategory, SettingSchema, SettingType};
use crate::view::controls::{
    DropdownState, FocusState, KeybindingListState, MapState, NumberInputState, TextInputState,
    TextListState, ToggleState,
};
use crate::view::ui::{FocusRegion, ScrollItem, TextEdit};
use std::collections::HashSet;

/// State for multiline JSON editing
#[derive(Debug, Clone)]
pub struct JsonEditState {
    /// The text editor state
    pub editor: TextEdit,
    /// Original text (for revert on Escape)
    pub original_text: String,
    /// Label for the control
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Scroll offset for viewing (used by entry dialog)
    pub scroll_offset: usize,
    /// Maximum visible lines (for main settings panel)
    pub max_visible_lines: usize,
}

impl JsonEditState {
    /// Create a new JSON edit state with pretty-printed JSON
    pub fn new(label: impl Into<String>, value: Option<&serde_json::Value>) -> Self {
        let json_str = value
            .map(|v| serde_json::to_string_pretty(v).unwrap_or_else(|_| "null".to_string()))
            .unwrap_or_else(|| "null".to_string());

        Self {
            original_text: json_str.clone(),
            editor: TextEdit::with_text(&json_str),
            label: label.into(),
            focus: FocusState::Normal,
            scroll_offset: 0,
            max_visible_lines: 6,
        }
    }

    /// Revert to original value (for Escape key)
    pub fn revert(&mut self) {
        self.editor.set_value(&self.original_text);
        self.scroll_offset = 0;
    }

    /// Commit current value as the new original (after saving)
    pub fn commit(&mut self) {
        self.original_text = self.editor.value();
    }

    /// Get the full text value
    pub fn value(&self) -> String {
        self.editor.value()
    }

    /// Check if the JSON is valid
    pub fn is_valid(&self) -> bool {
        serde_json::from_str::<serde_json::Value>(&self.value()).is_ok()
    }

    /// Get number of lines to display (all lines)
    pub fn display_height(&self) -> usize {
        self.editor.line_count()
    }

    /// Get number of lines for constrained view (e.g., main settings panel)
    pub fn display_height_capped(&self) -> usize {
        self.editor.line_count().min(self.max_visible_lines)
    }

    /// Get lines for rendering
    pub fn lines(&self) -> &[String] {
        &self.editor.lines
    }

    /// Get cursor position (row, col)
    pub fn cursor_pos(&self) -> (usize, usize) {
        (self.editor.cursor_row, self.editor.cursor_col)
    }

    // Delegate editing methods to TextEdit
    pub fn insert(&mut self, c: char) {
        self.editor.insert_char(c);
    }

    pub fn insert_str(&mut self, s: &str) {
        self.editor.insert_str(s);
    }

    pub fn backspace(&mut self) {
        self.editor.backspace();
    }

    pub fn delete(&mut self) {
        self.editor.delete();
    }

    pub fn move_left(&mut self) {
        self.editor.move_left();
    }

    pub fn move_right(&mut self) {
        self.editor.move_right();
    }

    pub fn move_up(&mut self) {
        self.editor.move_up();
    }

    pub fn move_down(&mut self) {
        self.editor.move_down();
    }

    pub fn move_home(&mut self) {
        self.editor.move_home();
    }

    pub fn move_end(&mut self) {
        self.editor.move_end();
    }

    pub fn move_word_left(&mut self) {
        self.editor.move_word_left();
    }

    pub fn move_word_right(&mut self) {
        self.editor.move_word_right();
    }

    // Selection methods
    pub fn has_selection(&self) -> bool {
        self.editor.has_selection()
    }

    pub fn selection_range(&self) -> Option<((usize, usize), (usize, usize))> {
        self.editor.selection_range()
    }

    pub fn selected_text(&self) -> Option<String> {
        self.editor.selected_text()
    }

    pub fn delete_selection(&mut self) -> Option<String> {
        self.editor.delete_selection()
    }

    pub fn clear_selection(&mut self) {
        self.editor.clear_selection();
    }

    pub fn move_left_selecting(&mut self) {
        self.editor.move_left_selecting();
    }

    pub fn move_right_selecting(&mut self) {
        self.editor.move_right_selecting();
    }

    pub fn move_up_selecting(&mut self) {
        self.editor.move_up_selecting();
    }

    pub fn move_down_selecting(&mut self) {
        self.editor.move_down_selecting();
    }

    pub fn move_home_selecting(&mut self) {
        self.editor.move_home_selecting();
    }

    pub fn move_end_selecting(&mut self) {
        self.editor.move_end_selecting();
    }

    pub fn move_word_left_selecting(&mut self) {
        self.editor.move_word_left_selecting();
    }

    pub fn move_word_right_selecting(&mut self) {
        self.editor.move_word_right_selecting();
    }

    pub fn select_all(&mut self) {
        self.editor.select_all();
    }

    pub fn delete_word_forward(&mut self) {
        self.editor.delete_word_forward();
    }

    pub fn delete_word_backward(&mut self) {
        self.editor.delete_word_backward();
    }

    pub fn delete_to_end(&mut self) {
        self.editor.delete_to_end();
    }
}

/// Create a JSON control for editing arbitrary JSON values (multiline)
fn json_control(
    name: &str,
    current_value: Option<&serde_json::Value>,
    default: Option<&serde_json::Value>,
) -> SettingControl {
    let value = current_value.or(default);
    SettingControl::Json(JsonEditState::new(name, value))
}

/// A renderable setting item
#[derive(Debug, Clone)]
pub struct SettingItem {
    /// JSON pointer path
    pub path: String,
    /// Display name
    pub name: String,
    /// Description
    pub description: Option<String>,
    /// The control for this setting
    pub control: SettingControl,
    /// Default value (for reset)
    pub default: Option<serde_json::Value>,
    /// Whether this setting has been modified from default
    pub modified: bool,
    /// Whether this field is read-only (cannot be edited)
    pub read_only: bool,
}

/// The type of control to render for a setting
#[derive(Debug, Clone)]
pub enum SettingControl {
    Toggle(ToggleState),
    Number(NumberInputState),
    Dropdown(DropdownState),
    Text(TextInputState),
    TextList(TextListState),
    /// Map/dictionary control for key-value pairs
    Map(MapState),
    /// Array of objects control (for keybindings, etc.)
    ObjectArray(KeybindingListState),
    /// Multiline JSON editor
    Json(JsonEditState),
    /// Complex settings that can't be edited inline
    Complex {
        type_name: String,
    },
}

impl SettingControl {
    /// Calculate the height needed for this control (in lines)
    pub fn control_height(&self) -> u16 {
        match self {
            // TextList needs: 1 label line + items + 1 "add new" row
            Self::TextList(state) => {
                // 1 for label + items count + 1 for add-new row
                (state.items.len() + 2) as u16
            }
            // Map needs: 1 label + entries + expanded content + 1 add-new row
            Self::Map(state) => {
                let base = 1 + state.entries.len() + 1; // label + entries + add-new
                                                        // Add extra height for expanded entries (up to 6 lines each)
                let expanded_height: usize = state
                    .expanded
                    .iter()
                    .filter_map(|&idx| state.entries.get(idx))
                    .map(|(_, v)| {
                        if let Some(obj) = v.as_object() {
                            obj.len().min(5) + if obj.len() > 5 { 1 } else { 0 }
                        } else {
                            0
                        }
                    })
                    .sum();
                (base + expanded_height) as u16
            }
            // Dropdown needs extra height when open to show options
            Self::Dropdown(state) => {
                if state.open {
                    // 1 for label/button + number of options (max 8 visible)
                    1 + state.options.len().min(8) as u16
                } else {
                    1
                }
            }
            // KeybindingList needs: 1 label + bindings + 1 add-new row
            Self::ObjectArray(state) => {
                // 1 for label + bindings count + 1 for add-new row
                (state.bindings.len() + 2) as u16
            }
            // Json needs: 1 label + visible lines
            Self::Json(state) => {
                // 1 for label + displayed lines
                1 + state.display_height() as u16
            }
            // All other controls fit in 1 line
            _ => 1,
        }
    }
}

impl SettingItem {
    /// Calculate the total height needed for this item (control + description + spacing)
    pub fn item_height(&self) -> u16 {
        // Height = control + description (if any) + spacing
        let description_height = if self.description.is_some() { 1 } else { 0 };
        self.control.control_height() + description_height + 1
    }

    /// Calculate height with expanded description when focused
    pub fn item_height_expanded(&self, width: u16) -> u16 {
        let description_height = self.description_height_expanded(width);
        self.control.control_height() + description_height + 1
    }

    /// Calculate description height when expanded (wrapped to width)
    pub fn description_height_expanded(&self, width: u16) -> u16 {
        if let Some(ref desc) = self.description {
            if desc.is_empty() || width == 0 {
                return 0;
            }
            // Calculate number of lines needed for wrapped description
            let chars_per_line = width.saturating_sub(2) as usize; // Leave some margin
            if chars_per_line == 0 {
                return 1;
            }
            ((desc.len() + chars_per_line - 1) / chars_per_line) as u16
        } else {
            0
        }
    }

    /// Calculate the content height (control + description, excluding spacing)
    pub fn content_height(&self) -> u16 {
        let description_height = if self.description.is_some() { 1 } else { 0 };
        self.control.control_height() + description_height
    }

    /// Calculate content height with expanded description
    pub fn content_height_expanded(&self, width: u16) -> u16 {
        let description_height = self.description_height_expanded(width);
        self.control.control_height() + description_height
    }
}

/// Clean a description to remove redundancy with the name.
/// Returns None if the description is empty or essentially just repeats the name.
pub fn clean_description(name: &str, description: Option<&str>) -> Option<String> {
    let desc = description?;
    if desc.is_empty() {
        return None;
    }

    // Build a set of significant words from the name (lowercase for comparison)
    let name_words: HashSet<String> = name
        .to_lowercase()
        .split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty() && w.len() > 2)
        .map(String::from)
        .collect();

    // Common filler words to ignore when checking for new info
    let filler_words: HashSet<&str> = [
        "the", "a", "an", "to", "for", "of", "in", "on", "is", "are", "be", "and", "or", "when",
        "whether", "if", "this", "that", "with", "from", "by", "as", "at", "show", "enable",
        "disable", "set", "use", "allow", "default", "true", "false",
    ]
    .into_iter()
    .collect();

    // Split description into words
    let desc_words: Vec<&str> = desc
        .split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty())
        .collect();

    // Check if description has any meaningful new information
    let has_new_info = desc_words.iter().any(|word| {
        let lower = word.to_lowercase();
        lower.len() > 2 && !name_words.contains(&lower) && !filler_words.contains(lower.as_str())
    });

    if !has_new_info {
        return None;
    }

    Some(desc.to_string())
}

impl ScrollItem for SettingItem {
    fn height(&self) -> u16 {
        self.item_height()
    }

    fn focus_regions(&self) -> Vec<FocusRegion> {
        match &self.control {
            // TextList: each row is a focus region
            SettingControl::TextList(state) => {
                let mut regions = Vec::new();
                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: 0,
                    height: 1,
                });
                // Each item row (id = 1 + row_index)
                for i in 0..state.items.len() {
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: 1 + i as u16,
                        height: 1,
                    });
                }
                // Add-new row
                regions.push(FocusRegion {
                    id: 1 + state.items.len(),
                    y_offset: 1 + state.items.len() as u16,
                    height: 1,
                });
                regions
            }
            // Map: each entry row is a focus region
            SettingControl::Map(state) => {
                let mut regions = Vec::new();
                let mut y = 0u16;

                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: y,
                    height: 1,
                });
                y += 1;

                // Each entry (id = 1 + entry_index)
                for (i, (_, v)) in state.entries.iter().enumerate() {
                    let mut entry_height = 1u16;
                    // Add expanded content height if expanded
                    if state.expanded.contains(&i) {
                        if let Some(obj) = v.as_object() {
                            entry_height += obj.len().min(5) as u16;
                            if obj.len() > 5 {
                                entry_height += 1;
                            }
                        }
                    }
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: y,
                        height: entry_height,
                    });
                    y += entry_height;
                }

                // Add-new row
                regions.push(FocusRegion {
                    id: 1 + state.entries.len(),
                    y_offset: y,
                    height: 1,
                });
                regions
            }
            // KeybindingList: each entry row is a focus region
            SettingControl::ObjectArray(state) => {
                let mut regions = Vec::new();
                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: 0,
                    height: 1,
                });
                // Each binding (id = 1 + index)
                for i in 0..state.bindings.len() {
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: 1 + i as u16,
                        height: 1,
                    });
                }
                // Add-new row
                regions.push(FocusRegion {
                    id: 1 + state.bindings.len(),
                    y_offset: 1 + state.bindings.len() as u16,
                    height: 1,
                });
                regions
            }
            // Other controls: single region covering the whole item
            _ => {
                vec![FocusRegion {
                    id: 0,
                    y_offset: 0,
                    height: self.item_height().saturating_sub(1), // Exclude spacing
                }]
            }
        }
    }
}

/// A page of settings (corresponds to a category)
#[derive(Debug, Clone)]
pub struct SettingsPage {
    /// Page name
    pub name: String,
    /// JSON path prefix
    pub path: String,
    /// Description
    pub description: Option<String>,
    /// Settings on this page
    pub items: Vec<SettingItem>,
    /// Subpages
    pub subpages: Vec<SettingsPage>,
}

/// Convert a category tree into pages with control states
pub fn build_pages(
    categories: &[SettingCategory],
    config_value: &serde_json::Value,
) -> Vec<SettingsPage> {
    categories
        .iter()
        .map(|cat| build_page(cat, config_value))
        .collect()
}

/// Build a single page from a category
fn build_page(category: &SettingCategory, config_value: &serde_json::Value) -> SettingsPage {
    let items = category
        .settings
        .iter()
        .map(|s| build_item(s, config_value))
        .collect();

    let subpages = category
        .subcategories
        .iter()
        .map(|sub| build_page(sub, config_value))
        .collect();

    SettingsPage {
        name: category.name.clone(),
        path: category.path.clone(),
        description: category.description.clone(),
        items,
        subpages,
    }
}

/// Build a setting item with its control state initialized from current config
pub fn build_item(schema: &SettingSchema, config_value: &serde_json::Value) -> SettingItem {
    // Get current value from config
    let current_value = config_value.pointer(&schema.path);

    // Create control based on type
    let control = match &schema.setting_type {
        SettingType::Boolean => {
            let checked = current_value
                .and_then(|v| v.as_bool())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_bool()))
                .unwrap_or(false);
            SettingControl::Toggle(ToggleState::new(checked, &schema.name))
        }

        SettingType::Integer { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_i64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);

            let mut state = NumberInputState::new(value, &schema.name);
            if let Some(min) = minimum {
                state = state.with_min(*min);
            }
            if let Some(max) = maximum {
                state = state.with_max(*max);
            }
            SettingControl::Number(state)
        }

        SettingType::Number { minimum, maximum } => {
            // For floats, we display as integers (multiply by 100 for percentages)
            let value = current_value
                .and_then(|v| v.as_f64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_f64()))
                .unwrap_or(0.0);

            // Convert to integer representation
            let int_value = (value * 100.0).round() as i64;
            let mut state = NumberInputState::new(int_value, &schema.name).with_percentage();
            if let Some(min) = minimum {
                state = state.with_min((*min * 100.0) as i64);
            }
            if let Some(max) = maximum {
                state = state.with_max((*max * 100.0) as i64);
            }
            SettingControl::Number(state)
        }

        SettingType::String => {
            let value = current_value
                .and_then(|v| v.as_str())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");

            let state = TextInputState::new(&schema.name).with_value(value);
            SettingControl::Text(state)
        }

        SettingType::Enum { options } => {
            // Handle null values in enums (represented as empty string in dropdown values)
            let current = if current_value.map(|v| v.is_null()).unwrap_or(false) {
                "" // null maps to empty string (Auto-detect option)
            } else {
                current_value
                    .and_then(|v| v.as_str())
                    .or_else(|| {
                        let default = schema.default.as_ref()?;
                        if default.is_null() {
                            Some("")
                        } else {
                            default.as_str()
                        }
                    })
                    .unwrap_or("")
            };

            let display_names: Vec<String> = options.iter().map(|o| o.name.clone()).collect();
            let values: Vec<String> = options.iter().map(|o| o.value.clone()).collect();
            let selected = values.iter().position(|v| v == current).unwrap_or(0);
            let state = DropdownState::with_values(display_names, values, &schema.name)
                .with_selected(selected);
            SettingControl::Dropdown(state)
        }

        SettingType::StringArray => {
            let items: Vec<String> = current_value
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect()
                })
                .or_else(|| {
                    schema.default.as_ref().and_then(|d| {
                        d.as_array().map(|arr| {
                            arr.iter()
                                .filter_map(|v| v.as_str().map(String::from))
                                .collect()
                        })
                    })
                })
                .unwrap_or_default();

            let state = TextListState::new(&schema.name).with_items(items);
            SettingControl::TextList(state)
        }

        SettingType::Object { .. } => {
            json_control(&schema.name, current_value, schema.default.as_ref())
        }

        SettingType::Map {
            value_schema,
            display_field,
            no_add,
        } => {
            // Get current map value or default
            let map_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!({}));

            let mut state = MapState::new(&schema.name).with_entries(&map_value);
            state = state.with_value_schema((**value_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            if *no_add {
                state = state.with_no_add(true);
            }
            SettingControl::Map(state)
        }

        SettingType::ObjectArray {
            item_schema,
            display_field,
        } => {
            // Get current array or default
            let array_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!([]));

            let mut state = KeybindingListState::new(&schema.name).with_bindings(&array_value);
            state = state.with_item_schema((**item_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            SettingControl::ObjectArray(state)
        }

        SettingType::Complex => json_control(&schema.name, current_value, schema.default.as_ref()),
    };

    // Check if modified from default
    let modified = match (&current_value, &schema.default) {
        (Some(current), Some(default)) => *current != default,
        (Some(_), None) => true,
        _ => false,
    };

    // Clean description to remove redundancy with name
    let cleaned_description = clean_description(&schema.name, schema.description.as_deref());

    SettingItem {
        path: schema.path.clone(),
        name: schema.name.clone(),
        description: cleaned_description,
        control,
        default: schema.default.clone(),
        modified,
        read_only: schema.read_only,
    }
}

/// Build a setting item with a value provided directly (for dialogs)
pub fn build_item_from_value(
    schema: &SettingSchema,
    current_value: Option<&serde_json::Value>,
) -> SettingItem {
    // Create control based on type
    let control = match &schema.setting_type {
        SettingType::Boolean => {
            let checked = current_value
                .and_then(|v| v.as_bool())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_bool()))
                .unwrap_or(false);
            SettingControl::Toggle(ToggleState::new(checked, &schema.name))
        }

        SettingType::Integer { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_i64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);

            let mut state = NumberInputState::new(value, &schema.name);
            if let Some(min) = minimum {
                state = state.with_min(*min);
            }
            if let Some(max) = maximum {
                state = state.with_max(*max);
            }
            SettingControl::Number(state)
        }

        SettingType::Number { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_f64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_f64()))
                .unwrap_or(0.0);

            let int_value = (value * 100.0).round() as i64;
            let mut state = NumberInputState::new(int_value, &schema.name).with_percentage();
            if let Some(min) = minimum {
                state = state.with_min((*min * 100.0) as i64);
            }
            if let Some(max) = maximum {
                state = state.with_max((*max * 100.0) as i64);
            }
            SettingControl::Number(state)
        }

        SettingType::String => {
            let value = current_value
                .and_then(|v| v.as_str())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");

            let state = TextInputState::new(&schema.name).with_value(value);
            SettingControl::Text(state)
        }

        SettingType::Enum { options } => {
            // Handle null values in enums (represented as empty string in dropdown values)
            let current = if current_value.map(|v| v.is_null()).unwrap_or(false) {
                "" // null maps to empty string (Auto-detect option)
            } else {
                current_value
                    .and_then(|v| v.as_str())
                    .or_else(|| {
                        let default = schema.default.as_ref()?;
                        if default.is_null() {
                            Some("")
                        } else {
                            default.as_str()
                        }
                    })
                    .unwrap_or("")
            };

            let display_names: Vec<String> = options.iter().map(|o| o.name.clone()).collect();
            let values: Vec<String> = options.iter().map(|o| o.value.clone()).collect();
            let selected = values.iter().position(|v| v == current).unwrap_or(0);
            let state = DropdownState::with_values(display_names, values, &schema.name)
                .with_selected(selected);
            SettingControl::Dropdown(state)
        }

        SettingType::StringArray => {
            let items: Vec<String> = current_value
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect()
                })
                .or_else(|| {
                    schema.default.as_ref().and_then(|d| {
                        d.as_array().map(|arr| {
                            arr.iter()
                                .filter_map(|v| v.as_str().map(String::from))
                                .collect()
                        })
                    })
                })
                .unwrap_or_default();

            let state = TextListState::new(&schema.name).with_items(items);
            SettingControl::TextList(state)
        }

        SettingType::Object { .. } => {
            json_control(&schema.name, current_value, schema.default.as_ref())
        }

        SettingType::Map {
            value_schema,
            display_field,
            no_add,
        } => {
            let map_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!({}));

            let mut state = MapState::new(&schema.name).with_entries(&map_value);
            state = state.with_value_schema((**value_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            if *no_add {
                state = state.with_no_add(true);
            }
            SettingControl::Map(state)
        }

        SettingType::ObjectArray {
            item_schema,
            display_field,
        } => {
            let array_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!([]));

            let mut state = KeybindingListState::new(&schema.name).with_bindings(&array_value);
            state = state.with_item_schema((**item_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            SettingControl::ObjectArray(state)
        }

        SettingType::Complex => json_control(&schema.name, current_value, schema.default.as_ref()),
    };

    // Check if modified from default
    let modified = match (&current_value, &schema.default) {
        (Some(current), Some(default)) => *current != default,
        (Some(_), None) => true,
        _ => false,
    };

    SettingItem {
        path: schema.path.clone(),
        name: schema.name.clone(),
        description: schema.description.clone(),
        control,
        default: schema.default.clone(),
        modified,
        read_only: schema.read_only,
    }
}

/// Extract the current value from a control
pub fn control_to_value(control: &SettingControl) -> serde_json::Value {
    match control {
        SettingControl::Toggle(state) => serde_json::Value::Bool(state.checked),

        SettingControl::Number(state) => {
            if state.is_percentage {
                // Convert back to float (divide by 100)
                let float_value = state.value as f64 / 100.0;
                serde_json::Number::from_f64(float_value)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Number(state.value.into()))
            } else {
                serde_json::Value::Number(state.value.into())
            }
        }

        SettingControl::Dropdown(state) => state
            .selected_value()
            .map(|s| {
                if s.is_empty() {
                    // Empty string represents null in nullable enums
                    serde_json::Value::Null
                } else {
                    serde_json::Value::String(s.to_string())
                }
            })
            .unwrap_or(serde_json::Value::Null),

        SettingControl::Text(state) => serde_json::Value::String(state.value.clone()),

        SettingControl::TextList(state) => {
            let arr: Vec<serde_json::Value> = state
                .items
                .iter()
                .map(|s| serde_json::Value::String(s.clone()))
                .collect();
            serde_json::Value::Array(arr)
        }

        SettingControl::Map(state) => state.to_value(),

        SettingControl::ObjectArray(state) => state.to_value(),

        SettingControl::Json(state) => {
            // Parse the JSON string back to a value
            serde_json::from_str(&state.value()).unwrap_or(serde_json::Value::Null)
        }

        SettingControl::Complex { .. } => serde_json::Value::Null,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_config() -> serde_json::Value {
        serde_json::json!({
            "theme": "monokai",
            "check_for_updates": false,
            "editor": {
                "tab_size": 2,
                "line_numbers": true
            }
        })
    }

    #[test]
    fn test_build_toggle_item() {
        let schema = SettingSchema {
            path: "/check_for_updates".to_string(),
            name: "Check For Updates".to_string(),
            description: Some("Check for updates".to_string()),
            setting_type: SettingType::Boolean,
            default: Some(serde_json::Value::Bool(true)),
            read_only: false,
        };

        let config = sample_config();
        let item = build_item(&schema, &config);

        assert_eq!(item.path, "/check_for_updates");
        assert!(item.modified); // false != true (default)

        if let SettingControl::Toggle(state) = &item.control {
            assert!(!state.checked); // Current value is false
        } else {
            panic!("Expected toggle control");
        }
    }

    #[test]
    fn test_build_number_item() {
        let schema = SettingSchema {
            path: "/editor/tab_size".to_string(),
            name: "Tab Size".to_string(),
            description: None,
            setting_type: SettingType::Integer {
                minimum: Some(1),
                maximum: Some(16),
            },
            default: Some(serde_json::Value::Number(4.into())),
            read_only: false,
        };

        let config = sample_config();
        let item = build_item(&schema, &config);

        assert!(item.modified); // 2 != 4 (default)

        if let SettingControl::Number(state) = &item.control {
            assert_eq!(state.value, 2);
            assert_eq!(state.min, Some(1));
            assert_eq!(state.max, Some(16));
        } else {
            panic!("Expected number control");
        }
    }

    #[test]
    fn test_build_text_item() {
        let schema = SettingSchema {
            path: "/theme".to_string(),
            name: "Theme".to_string(),
            description: None,
            setting_type: SettingType::String,
            default: Some(serde_json::Value::String("high-contrast".to_string())),
            read_only: false,
        };

        let config = sample_config();
        let item = build_item(&schema, &config);

        assert!(item.modified);

        if let SettingControl::Text(state) = &item.control {
            assert_eq!(state.value, "monokai");
        } else {
            panic!("Expected text control");
        }
    }

    #[test]
    fn test_clean_description_keeps_full_desc_with_new_info() {
        // "Tab Size" + "Number of spaces per tab character" -> keeps full desc (has "spaces", "character")
        let result = clean_description("Tab Size", Some("Number of spaces per tab character"));
        assert!(result.is_some());
        let cleaned = result.unwrap();
        // Should preserve original casing and contain the full info
        assert!(cleaned.starts_with('N')); // uppercase 'N' from "Number"
        assert!(cleaned.contains("spaces"));
        assert!(cleaned.contains("character"));
    }

    #[test]
    fn test_clean_description_keeps_extra_info() {
        // "Line Numbers" + "Show line numbers in the gutter" -> should keep full desc with "gutter"
        let result = clean_description("Line Numbers", Some("Show line numbers in the gutter"));
        assert!(result.is_some());
        let cleaned = result.unwrap();
        assert!(cleaned.contains("gutter"));
    }

    #[test]
    fn test_clean_description_returns_none_for_pure_redundancy() {
        // If description is just the name repeated, return None
        let result = clean_description("Theme", Some("Theme"));
        assert!(result.is_none());

        // Or only filler words around the name
        let result = clean_description("Theme", Some("The theme to use"));
        assert!(result.is_none());
    }

    #[test]
    fn test_clean_description_returns_none_for_empty() {
        let result = clean_description("Theme", Some(""));
        assert!(result.is_none());

        let result = clean_description("Theme", None);
        assert!(result.is_none());
    }

    #[test]
    fn test_control_to_value() {
        let toggle = SettingControl::Toggle(ToggleState::new(true, "Test"));
        assert_eq!(control_to_value(&toggle), serde_json::Value::Bool(true));

        let number = SettingControl::Number(NumberInputState::new(42, "Test"));
        assert_eq!(control_to_value(&number), serde_json::json!(42));

        let text = SettingControl::Text(TextInputState::new("Test").with_value("hello"));
        assert_eq!(
            control_to_value(&text),
            serde_json::Value::String("hello".to_string())
        );
    }
}
