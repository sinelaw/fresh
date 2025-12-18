//! Entry detail dialog for editing complex map entries
//!
//! Provides a modal dialog for editing complex map entries with proper controls.
//! Fields are built dynamically from the JSON Schema.

use crate::view::controls::FocusState;
use crate::view::settings::schema::{SettingSchema, SettingType};
use serde_json::Value;

/// A field in the entry dialog
#[derive(Debug, Clone)]
pub struct DialogField {
    /// Field name/key
    pub name: String,
    /// Display label
    pub label: String,
    /// Current value
    pub value: FieldValue,
    /// Whether this field is required
    pub required: bool,
    /// Description/help text
    pub description: Option<String>,
}

/// Possible values for dialog fields
#[derive(Debug, Clone)]
pub enum FieldValue {
    /// Boolean toggle
    Bool(bool),
    /// Single-line text
    Text {
        value: String,
        cursor: usize,
        editing: bool,
    },
    /// Optional text (can be null)
    OptionalText {
        value: Option<String>,
        cursor: usize,
        editing: bool,
    },
    /// String array
    StringList {
        items: Vec<String>,
        focused_index: Option<usize>,
        new_text: String,
        cursor: usize,
        editing: bool,
    },
    /// Integer number
    Integer {
        value: i64,
        min: Option<i64>,
        max: Option<i64>,
        editing: bool,
        text: String,
    },
    /// Dropdown selection
    Dropdown {
        options: Vec<String>,
        selected: usize,
        open: bool,
    },
    /// Nested object (show field count, click to expand)
    Object {
        /// JSON representation
        json: Value,
        /// Expanded state
        expanded: bool,
    },
}

impl FieldValue {
    /// Check if the field is currently in edit mode
    pub fn is_editing(&self) -> bool {
        match self {
            FieldValue::Bool(_) => false,
            FieldValue::Text { editing, .. } => *editing,
            FieldValue::OptionalText { editing, .. } => *editing,
            FieldValue::StringList { editing, .. } => *editing,
            FieldValue::Integer { editing, .. } => *editing,
            FieldValue::Dropdown { open, .. } => *open,
            FieldValue::Object { .. } => false,
        }
    }
}

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
    /// Fields in the dialog
    pub fields: Vec<DialogField>,
    /// Currently focused field index
    pub focused_field: usize,
    /// Currently focused button (0=Save, 1=Cancel)
    pub focused_button: usize,
    /// Whether focus is on buttons (true) or fields (false)
    pub focus_on_buttons: bool,
}

impl EntryDialogState {
    /// Create a dialog from a schema definition
    ///
    /// This is the primary, schema-driven constructor. It builds fields
    /// dynamically from the SettingSchema's properties.
    pub fn from_schema(
        key: String,
        value: &Value,
        schema: &SettingSchema,
        map_path: &str,
        is_new: bool,
    ) -> Self {
        let fields = build_fields_from_schema(schema, value);
        let title = if is_new {
            format!("New {}", schema.name)
        } else {
            format!("Edit {}: {}", schema.name, key)
        };

        Self {
            entry_key: key,
            map_path: map_path.to_string(),
            title,
            is_new,
            fields,
            focused_field: 0,
            focused_button: 0,
            focus_on_buttons: false,
        }
    }

    /// Convert dialog state back to JSON value
    pub fn to_value(&self) -> Value {
        let mut obj = serde_json::Map::new();

        for field in &self.fields {
            // Handle nested paths like "process_limits.enabled"
            let parts: Vec<&str> = field.name.split('.').collect();
            let value = field_to_value(&field.value);

            if parts.len() == 1 {
                obj.insert(parts[0].to_string(), value);
            } else if parts.len() == 2 {
                // Nested field
                let parent = obj
                    .entry(parts[0].to_string())
                    .or_insert_with(|| Value::Object(serde_json::Map::new()));
                if let Value::Object(ref mut parent_obj) = parent {
                    parent_obj.insert(parts[1].to_string(), value);
                }
            }
        }

        Value::Object(obj)
    }

    /// Move focus to previous field
    pub fn focus_prev(&mut self) {
        if self.focus_on_buttons {
            if self.focused_button > 0 {
                self.focused_button -= 1;
            } else {
                self.focus_on_buttons = false;
                self.focused_field = self.fields.len().saturating_sub(1);
            }
        } else if self.focused_field > 0 {
            self.focused_field -= 1;
        }
    }

    /// Move focus to next field
    pub fn focus_next(&mut self) {
        if self.focus_on_buttons {
            if self.focused_button < 1 {
                self.focused_button += 1;
            }
        } else if self.focused_field + 1 < self.fields.len() {
            self.focused_field += 1;
        } else {
            self.focus_on_buttons = true;
            self.focused_button = 0;
        }
    }

    /// Get the currently focused field
    pub fn current_field(&self) -> Option<&DialogField> {
        self.fields.get(self.focused_field)
    }

    /// Get the currently focused field mutably
    pub fn current_field_mut(&mut self) -> Option<&mut DialogField> {
        self.fields.get_mut(self.focused_field)
    }

    /// Toggle a boolean field or dropdown
    pub fn toggle_current(&mut self) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Bool(b) => *b = !*b,
                FieldValue::Dropdown { open, .. } => *open = !*open,
                _ => {}
            }
        }
    }

    /// Start editing the current text field
    pub fn start_editing(&mut self) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Text {
                    editing,
                    cursor,
                    value,
                } => {
                    *editing = true;
                    *cursor = value.len();
                }
                FieldValue::OptionalText {
                    editing,
                    cursor,
                    value,
                } => {
                    *editing = true;
                    *cursor = value.as_ref().map_or(0, |s| s.len());
                }
                FieldValue::StringList {
                    editing, cursor, ..
                } => {
                    *editing = true;
                    *cursor = 0;
                }
                FieldValue::Integer {
                    editing,
                    text,
                    value,
                    ..
                } => {
                    *editing = true;
                    *text = value.to_string();
                }
                _ => {}
            }
        }
    }

    /// Stop editing and confirm changes
    pub fn stop_editing(&mut self) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Text { editing, .. } => *editing = false,
                FieldValue::OptionalText { editing, .. } => *editing = false,
                FieldValue::StringList { editing, .. } => *editing = false,
                FieldValue::Integer {
                    editing,
                    text,
                    value,
                    ..
                } => {
                    *editing = false;
                    if let Ok(n) = text.parse::<i64>() {
                        *value = n;
                    }
                }
                FieldValue::Dropdown { open, .. } => *open = false,
                _ => {}
            }
        }
    }

    /// Check if any field is being edited
    pub fn is_editing(&self) -> bool {
        self.fields.iter().any(|f| f.value.is_editing())
    }

    /// Get the focus state for a field
    pub fn field_focus_state(&self, index: usize) -> FocusState {
        if self.focus_on_buttons {
            FocusState::Normal
        } else if index == self.focused_field {
            FocusState::Focused
        } else {
            FocusState::Normal
        }
    }

    /// Insert a character into the current editable field
    pub fn insert_char(&mut self, c: char) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Text {
                    value,
                    cursor,
                    editing,
                } if *editing => {
                    value.insert(*cursor, c);
                    *cursor += c.len_utf8();
                }
                FieldValue::OptionalText {
                    value,
                    cursor,
                    editing,
                } if *editing => {
                    if value.is_none() {
                        *value = Some(String::new());
                    }
                    if let Some(ref mut s) = value {
                        s.insert(*cursor, c);
                        *cursor += c.len_utf8();
                    }
                }
                FieldValue::StringList {
                    new_text,
                    cursor,
                    editing,
                    ..
                } if *editing => {
                    new_text.insert(*cursor, c);
                    *cursor += c.len_utf8();
                }
                FieldValue::Integer { text, editing, .. } if *editing => {
                    if c.is_ascii_digit() || (c == '-' && text.is_empty()) {
                        text.push(c);
                    }
                }
                _ => {}
            }
        }
    }

    /// Handle backspace
    pub fn backspace(&mut self) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Text {
                    value,
                    cursor,
                    editing,
                } if *editing && *cursor > 0 => {
                    *cursor -= 1;
                    value.remove(*cursor);
                }
                FieldValue::OptionalText {
                    value,
                    cursor,
                    editing,
                } if *editing && *cursor > 0 => {
                    if let Some(ref mut s) = value {
                        *cursor -= 1;
                        s.remove(*cursor);
                        if s.is_empty() {
                            *value = None;
                        }
                    }
                }
                FieldValue::StringList {
                    new_text,
                    cursor,
                    editing,
                    ..
                } if *editing && *cursor > 0 => {
                    *cursor -= 1;
                    new_text.remove(*cursor);
                }
                FieldValue::Integer { text, editing, .. } if *editing => {
                    text.pop();
                }
                _ => {}
            }
        }
    }

    /// Move cursor left
    pub fn cursor_left(&mut self) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Text {
                    cursor, editing, ..
                }
                | FieldValue::OptionalText {
                    cursor, editing, ..
                }
                | FieldValue::StringList {
                    cursor, editing, ..
                } if *editing && *cursor > 0 => {
                    *cursor -= 1;
                }
                _ => {}
            }
        }
    }

    /// Move cursor right
    pub fn cursor_right(&mut self) {
        if let Some(field) = self.current_field_mut() {
            match &mut field.value {
                FieldValue::Text {
                    value,
                    cursor,
                    editing,
                } if *editing && *cursor < value.len() => {
                    *cursor += 1;
                }
                FieldValue::OptionalText {
                    value,
                    cursor,
                    editing,
                } if *editing => {
                    let max = value.as_ref().map_or(0, |s| s.len());
                    if *cursor < max {
                        *cursor += 1;
                    }
                }
                FieldValue::StringList {
                    new_text,
                    cursor,
                    editing,
                    ..
                } if *editing && *cursor < new_text.len() => {
                    *cursor += 1;
                }
                _ => {}
            }
        }
    }

    /// Navigate within dropdown
    pub fn dropdown_prev(&mut self) {
        if let Some(field) = self.current_field_mut() {
            if let FieldValue::Dropdown {
                options, selected, ..
            } = &mut field.value
            {
                if *selected > 0 {
                    *selected -= 1;
                } else {
                    *selected = options.len().saturating_sub(1);
                }
            }
        }
    }

    /// Navigate within dropdown
    pub fn dropdown_next(&mut self) {
        if let Some(field) = self.current_field_mut() {
            if let FieldValue::Dropdown {
                options, selected, ..
            } = &mut field.value
            {
                if *selected + 1 < options.len() {
                    *selected += 1;
                } else {
                    *selected = 0;
                }
            }
        }
    }

    /// Add item to string list and clear input
    pub fn add_list_item(&mut self) {
        if let Some(field) = self.current_field_mut() {
            if let FieldValue::StringList {
                items,
                new_text,
                cursor,
                ..
            } = &mut field.value
            {
                if !new_text.is_empty() {
                    items.push(std::mem::take(new_text));
                    *cursor = 0;
                }
            }
        }
    }

    /// Delete focused item from string list
    pub fn delete_list_item(&mut self) {
        if let Some(field) = self.current_field_mut() {
            if let FieldValue::StringList {
                items,
                focused_index,
                ..
            } = &mut field.value
            {
                if let Some(idx) = *focused_index {
                    if idx < items.len() {
                        items.remove(idx);
                        if items.is_empty() {
                            *focused_index = None;
                        } else if idx >= items.len() {
                            *focused_index = Some(items.len() - 1);
                        }
                    }
                }
            }
        }
    }

    /// Navigate within string list
    pub fn list_prev(&mut self) {
        if let Some(field) = self.current_field_mut() {
            if let FieldValue::StringList {
                items,
                focused_index,
                editing,
                ..
            } = &mut field.value
            {
                if *editing {
                    return;
                }
                match *focused_index {
                    None if !items.is_empty() => *focused_index = Some(items.len() - 1),
                    Some(0) => *focused_index = None,
                    Some(idx) => *focused_index = Some(idx - 1),
                    _ => {}
                }
            }
        }
    }

    /// Navigate within string list
    pub fn list_next(&mut self) {
        if let Some(field) = self.current_field_mut() {
            if let FieldValue::StringList {
                items,
                focused_index,
                editing,
                ..
            } = &mut field.value
            {
                if *editing {
                    return;
                }
                match *focused_index {
                    Some(idx) if idx + 1 < items.len() => *focused_index = Some(idx + 1),
                    Some(_) => *focused_index = None,
                    None if !items.is_empty() => *focused_index = Some(0),
                    _ => {}
                }
            }
        }
    }
}

/// Convert field value to JSON
fn field_to_value(field: &FieldValue) -> Value {
    match field {
        FieldValue::Bool(b) => Value::Bool(*b),
        FieldValue::Text { value, .. } => Value::String(value.clone()),
        FieldValue::OptionalText { value, .. } => value.clone().map_or(Value::Null, Value::String),
        FieldValue::StringList { items, .. } => {
            Value::Array(items.iter().map(|s| Value::String(s.clone())).collect())
        }
        FieldValue::Integer { value, .. } => Value::Number((*value).into()),
        FieldValue::Dropdown {
            options, selected, ..
        } => options
            .get(*selected)
            .map(|s| Value::String(s.clone()))
            .unwrap_or(Value::Null),
        FieldValue::Object { json, .. } => json.clone(),
    }
}

/// Build dialog fields from a schema definition
fn build_fields_from_schema(schema: &SettingSchema, value: &Value) -> Vec<DialogField> {
    let mut fields = Vec::new();

    // Extract properties from schema if it's an Object type
    let properties = match &schema.setting_type {
        SettingType::Object { properties } => properties,
        _ => return fields, // Not an object schema, return empty
    };

    for prop in properties {
        let field_value = value.get(&prop.path.trim_start_matches('/'));
        let field = build_field_from_property(prop, field_value);
        fields.push(field);
    }

    fields
}

/// Build a single dialog field from a schema property
fn build_field_from_property(prop: &SettingSchema, value: Option<&Value>) -> DialogField {
    let field_value = match &prop.setting_type {
        SettingType::Boolean => {
            let checked = value
                .and_then(|v| v.as_bool())
                .or_else(|| prop.default.as_ref().and_then(|d| d.as_bool()))
                .unwrap_or(false);
            FieldValue::Bool(checked)
        }

        SettingType::Integer { minimum, maximum } => {
            let val = value
                .and_then(|v| v.as_i64())
                .or_else(|| prop.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);
            FieldValue::Integer {
                value: val,
                min: *minimum,
                max: *maximum,
                editing: false,
                text: String::new(),
            }
        }

        SettingType::Number { .. } => {
            // Treat as integer for simplicity (could be extended)
            let val = value
                .and_then(|v| v.as_f64())
                .or_else(|| prop.default.as_ref().and_then(|d| d.as_f64()))
                .map(|f| f as i64)
                .unwrap_or(0);
            FieldValue::Integer {
                value: val,
                min: None,
                max: None,
                editing: false,
                text: String::new(),
            }
        }

        SettingType::String => {
            // Check if the value can be null (nullable string)
            let is_nullable = value.map_or(false, |v| v.is_null())
                || prop.default.as_ref().map_or(false, |d| d.is_null());

            if is_nullable {
                FieldValue::OptionalText {
                    value: value.and_then(|v| v.as_str()).map(String::from),
                    cursor: 0,
                    editing: false,
                }
            } else {
                let text = value
                    .and_then(|v| v.as_str())
                    .or_else(|| prop.default.as_ref().and_then(|d| d.as_str()))
                    .unwrap_or("")
                    .to_string();
                FieldValue::Text {
                    value: text,
                    cursor: 0,
                    editing: false,
                }
            }
        }

        SettingType::Enum { options } => {
            let current = value
                .and_then(|v| v.as_str())
                .or_else(|| prop.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");
            let option_values: Vec<String> = options.iter().map(|o| o.value.clone()).collect();
            let selected = option_values.iter().position(|v| v == current).unwrap_or(0);
            FieldValue::Dropdown {
                options: options.iter().map(|o| o.name.clone()).collect(),
                selected,
                open: false,
            }
        }

        SettingType::StringArray => {
            let items: Vec<String> = value
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect()
                })
                .or_else(|| {
                    prop.default.as_ref().and_then(|d| {
                        d.as_array().map(|arr| {
                            arr.iter()
                                .filter_map(|v| v.as_str().map(String::from))
                                .collect()
                        })
                    })
                })
                .unwrap_or_default();
            FieldValue::StringList {
                items,
                focused_index: None,
                new_text: String::new(),
                cursor: 0,
                editing: false,
            }
        }

        SettingType::Object { .. } | SettingType::Map { .. } | SettingType::Complex => {
            // For complex nested objects, store as JSON for now
            let json = value
                .cloned()
                .unwrap_or_else(|| prop.default.clone().unwrap_or(serde_json::json!({})));
            FieldValue::Object {
                json,
                expanded: false,
            }
        }

        SettingType::KeybindingArray => {
            // Treat as a complex object for now
            let json = value.cloned().unwrap_or_else(|| serde_json::json!([]));
            FieldValue::Object {
                json,
                expanded: false,
            }
        }
    };

    // Extract property name from path (e.g., "/extensions" -> "extensions")
    let name = prop.path.trim_start_matches('/').to_string();

    DialogField {
        name,
        label: prop.name.clone(),
        value: field_value,
        required: false, // Could be derived from schema if we had "required" info
        description: prop.description.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::settings::schema::EnumOption;
    use serde_json::json;

    /// Helper to create a simple property schema
    fn prop(name: &str, setting_type: SettingType) -> SettingSchema {
        SettingSchema {
            path: format!("/{}", name),
            name: name.to_string(),
            description: None,
            setting_type,
            default: None,
        }
    }

    /// Helper to create a property schema with default
    fn prop_with_default(name: &str, setting_type: SettingType, default: Value) -> SettingSchema {
        SettingSchema {
            path: format!("/{}", name),
            name: name.to_string(),
            description: Some(format!("{} description", name)),
            setting_type,
            default: Some(default),
        }
    }

    // === build_fields_from_schema tests ===

    #[test]
    fn non_object_schema_returns_empty_fields() {
        let schema = prop("test", SettingType::Boolean);
        let fields = build_fields_from_schema(&schema, &json!({}));
        assert!(fields.is_empty());
    }

    #[test]
    fn object_schema_creates_fields_for_each_property() {
        let schema = SettingSchema {
            path: "/root".to_string(),
            name: "Root".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![
                    prop("enabled", SettingType::Boolean),
                    prop("name", SettingType::String),
                ],
            },
            default: None,
        };

        let value = json!({"enabled": true, "name": "test"});
        let fields = build_fields_from_schema(&schema, &value);

        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "enabled");
        assert_eq!(fields[1].name, "name");
    }

    // === Boolean field tests ===

    #[test]
    fn boolean_uses_value_over_default() {
        let schema = prop_with_default("flag", SettingType::Boolean, json!(true));
        let field = build_field_from_property(&schema, Some(&json!(false)));
        assert!(matches!(field.value, FieldValue::Bool(false)));
    }

    #[test]
    fn boolean_falls_back_to_default() {
        let schema = prop_with_default("flag", SettingType::Boolean, json!(true));
        let field = build_field_from_property(&schema, None);
        assert!(matches!(field.value, FieldValue::Bool(true)));
    }

    #[test]
    fn boolean_defaults_to_false_when_no_value_or_default() {
        let schema = prop("flag", SettingType::Boolean);
        let field = build_field_from_property(&schema, None);
        assert!(matches!(field.value, FieldValue::Bool(false)));
    }

    // === Integer field tests ===

    #[test]
    fn integer_preserves_min_max_constraints() {
        let schema = prop(
            "count",
            SettingType::Integer {
                minimum: Some(1),
                maximum: Some(100),
            },
        );
        let field = build_field_from_property(&schema, Some(&json!(50)));

        match field.value {
            FieldValue::Integer {
                value, min, max, ..
            } => {
                assert_eq!(value, 50);
                assert_eq!(min, Some(1));
                assert_eq!(max, Some(100));
            }
            _ => panic!("Expected Integer field"),
        }
    }

    #[test]
    fn integer_with_no_constraints() {
        let schema = prop(
            "count",
            SettingType::Integer {
                minimum: None,
                maximum: None,
            },
        );
        let field = build_field_from_property(&schema, None);

        match field.value {
            FieldValue::Integer { min, max, .. } => {
                assert_eq!(min, None);
                assert_eq!(max, None);
            }
            _ => panic!("Expected Integer field"),
        }
    }

    // === String field tests ===

    #[test]
    fn string_with_null_value_becomes_optional() {
        let schema = prop("comment", SettingType::String);
        let field = build_field_from_property(&schema, Some(&Value::Null));

        assert!(matches!(
            field.value,
            FieldValue::OptionalText { value: None, .. }
        ));
    }

    #[test]
    fn string_with_null_default_becomes_optional() {
        let schema = prop_with_default("comment", SettingType::String, Value::Null);
        let field = build_field_from_property(&schema, None);

        assert!(matches!(
            field.value,
            FieldValue::OptionalText { value: None, .. }
        ));
    }

    #[test]
    fn nullable_string_preserves_actual_value() {
        let schema = prop_with_default("comment", SettingType::String, Value::Null);
        let field = build_field_from_property(&schema, Some(&json!("hello")));

        match field.value {
            FieldValue::OptionalText { value, .. } => {
                assert_eq!(value, Some("hello".to_string()));
            }
            _ => panic!("Expected OptionalText field"),
        }
    }

    #[test]
    fn regular_string_uses_text_field() {
        let schema = prop_with_default("name", SettingType::String, json!("default"));
        let field = build_field_from_property(&schema, Some(&json!("actual")));

        match field.value {
            FieldValue::Text { value, .. } => assert_eq!(value, "actual"),
            _ => panic!("Expected Text field"),
        }
    }

    // === Enum field tests ===

    #[test]
    fn enum_selects_matching_option() {
        let schema = prop(
            "mode",
            SettingType::Enum {
                options: vec![
                    EnumOption {
                        name: "Auto".to_string(),
                        value: "auto".to_string(),
                    },
                    EnumOption {
                        name: "Manual".to_string(),
                        value: "manual".to_string(),
                    },
                    EnumOption {
                        name: "Off".to_string(),
                        value: "off".to_string(),
                    },
                ],
            },
        );
        let field = build_field_from_property(&schema, Some(&json!("manual")));

        match field.value {
            FieldValue::Dropdown {
                selected, options, ..
            } => {
                assert_eq!(selected, 1);
                assert_eq!(options, vec!["Auto", "Manual", "Off"]);
            }
            _ => panic!("Expected Dropdown field"),
        }
    }

    #[test]
    fn enum_unknown_value_defaults_to_first() {
        let schema = prop(
            "mode",
            SettingType::Enum {
                options: vec![
                    EnumOption {
                        name: "A".to_string(),
                        value: "a".to_string(),
                    },
                    EnumOption {
                        name: "B".to_string(),
                        value: "b".to_string(),
                    },
                ],
            },
        );
        let field = build_field_from_property(&schema, Some(&json!("unknown")));

        match field.value {
            FieldValue::Dropdown { selected, .. } => assert_eq!(selected, 0),
            _ => panic!("Expected Dropdown field"),
        }
    }

    // === StringArray field tests ===

    #[test]
    fn string_array_from_value() {
        let schema = prop("extensions", SettingType::StringArray);
        let field = build_field_from_property(&schema, Some(&json!(["rs", "toml"])));

        match field.value {
            FieldValue::StringList { items, .. } => {
                assert_eq!(items, vec!["rs", "toml"]);
            }
            _ => panic!("Expected StringList field"),
        }
    }

    #[test]
    fn string_array_falls_back_to_default() {
        let schema = prop_with_default("tags", SettingType::StringArray, json!(["default"]));
        let field = build_field_from_property(&schema, None);

        match field.value {
            FieldValue::StringList { items, .. } => {
                assert_eq!(items, vec!["default"]);
            }
            _ => panic!("Expected StringList field"),
        }
    }

    #[test]
    fn string_array_filters_non_strings() {
        let schema = prop("mixed", SettingType::StringArray);
        let field = build_field_from_property(&schema, Some(&json!(["a", 123, "b", null])));

        match field.value {
            FieldValue::StringList { items, .. } => {
                assert_eq!(items, vec!["a", "b"]);
            }
            _ => panic!("Expected StringList field"),
        }
    }

    // === Complex type tests ===

    #[test]
    fn nested_object_stored_as_json() {
        let schema = prop(
            "limits",
            SettingType::Object {
                properties: vec![prop(
                    "max",
                    SettingType::Integer {
                        minimum: None,
                        maximum: None,
                    },
                )],
            },
        );
        let value = json!({"max": 100, "extra": "data"});
        let field = build_field_from_property(&schema, Some(&value));

        match field.value {
            FieldValue::Object { json, expanded } => {
                assert_eq!(json, value);
                assert!(!expanded);
            }
            _ => panic!("Expected Object field"),
        }
    }

    #[test]
    fn map_type_stored_as_json() {
        let inner = prop("value", SettingType::String);
        let schema = prop(
            "configs",
            SettingType::Map {
                value_schema: Box::new(inner),
                display_field: None,
            },
        );
        let field = build_field_from_property(&schema, Some(&json!({"key": "val"})));

        assert!(matches!(field.value, FieldValue::Object { .. }));
    }

    #[test]
    fn complex_type_uses_default_when_no_value() {
        let schema = prop_with_default("data", SettingType::Complex, json!({"preset": true}));
        let field = build_field_from_property(&schema, None);

        match field.value {
            FieldValue::Object { json, .. } => {
                assert_eq!(json, json!({"preset": true}));
            }
            _ => panic!("Expected Object field"),
        }
    }

    // === Number type test ===

    #[test]
    fn number_converted_to_integer() {
        let schema = prop(
            "ratio",
            SettingType::Number {
                minimum: None,
                maximum: None,
            },
        );
        let field = build_field_from_property(&schema, Some(&json!(3.7)));

        match field.value {
            FieldValue::Integer { value, .. } => assert_eq!(value, 3),
            _ => panic!("Expected Integer field"),
        }
    }

    // === from_schema constructor tests ===

    #[test]
    fn from_schema_generates_edit_title() {
        let schema = SettingSchema {
            path: "/value".to_string(),
            name: "LanguageConfig".to_string(),
            description: None,
            setting_type: SettingType::Object { properties: vec![] },
            default: None,
        };

        let dialog = EntryDialogState::from_schema(
            "rust".to_string(),
            &json!({}),
            &schema,
            "/languages",
            false,
        );

        assert_eq!(dialog.title, "Edit LanguageConfig: rust");
        assert_eq!(dialog.entry_key, "rust");
        assert_eq!(dialog.map_path, "/languages");
        assert!(!dialog.is_new);
    }

    #[test]
    fn from_schema_generates_new_title() {
        let schema = SettingSchema {
            path: "/value".to_string(),
            name: "LspConfig".to_string(),
            description: None,
            setting_type: SettingType::Object { properties: vec![] },
            default: None,
        };

        let dialog =
            EntryDialogState::from_schema("python".to_string(), &json!({}), &schema, "/lsp", true);

        assert_eq!(dialog.title, "New LspConfig");
        assert!(dialog.is_new);
    }

    #[test]
    fn from_schema_populates_fields_from_value() {
        let schema = SettingSchema {
            path: "/value".to_string(),
            name: "Config".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![
                    prop("enabled", SettingType::Boolean),
                    prop_with_default(
                        "count",
                        SettingType::Integer {
                            minimum: None,
                            maximum: None,
                        },
                        json!(10),
                    ),
                ],
            },
            default: None,
        };

        let value = json!({"enabled": true});
        let dialog =
            EntryDialogState::from_schema("test".to_string(), &value, &schema, "/test", false);

        assert_eq!(dialog.fields.len(), 2);

        // First field uses provided value
        assert!(matches!(dialog.fields[0].value, FieldValue::Bool(true)));

        // Second field falls back to default (value not provided)
        match &dialog.fields[1].value {
            FieldValue::Integer { value, .. } => assert_eq!(*value, 10),
            _ => panic!("Expected Integer field"),
        }
    }

    // === Field metadata tests ===

    #[test]
    fn field_name_extracted_from_path() {
        let schema = SettingSchema {
            path: "/deeply/nested/property".to_string(),
            name: "Property".to_string(),
            description: None,
            setting_type: SettingType::Boolean,
            default: None,
        };
        let field = build_field_from_property(&schema, None);

        // Name is the full path minus leading slash
        assert_eq!(field.name, "deeply/nested/property");
        assert_eq!(field.label, "Property");
    }

    #[test]
    fn field_preserves_description() {
        let schema = SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: Some("Help text".to_string()),
            setting_type: SettingType::Boolean,
            default: None,
        };
        let field = build_field_from_property(&schema, None);

        assert_eq!(field.description, Some("Help text".to_string()));
    }
}
