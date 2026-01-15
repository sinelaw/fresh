//! JSON Schema parsing for settings UI
//!
//! Parses the config JSON Schema to build the settings UI structure.
//!
//! # Extensible Enums with `x-enum-values`
//!
//! This module supports a custom JSON Schema extension called `x-enum-values` that allows
//! enum values to be defined separately from type definitions. This enables extensibility -
//! new enum values can be added without modifying the type schema.
//!
//! ## How it works
//!
//! 1. Define a type in `$defs` (without hardcoded enum values):
//!    ```json
//!    "$defs": {
//!      "ThemeOptions": {
//!        "type": "string"
//!      }
//!    }
//!    ```
//!
//! 2. Reference the type in properties:
//!    ```json
//!    "properties": {
//!      "theme": {
//!        "$ref": "#/$defs/ThemeOptions",
//!        "default": "dark"
//!      }
//!    }
//!    ```
//!
//! 3. Define enum values separately - each value declares which type it extends:
//!    ```json
//!    "x-enum-values": [
//!      { "ref": "#/$defs/ThemeOptions", "name": "Dark", "value": "dark" },
//!      { "ref": "#/$defs/ThemeOptions", "name": "Light", "value": "light" },
//!      { "ref": "#/$defs/ThemeOptions", "name": "High Contrast", "value": "high-contrast" }
//!    ]
//!    ```
//!
//! ## Entry structure
//!
//! Each entry in `x-enum-values` has:
//! - `ref` (required): JSON pointer to the type being extended (e.g., `#/$defs/ThemeOptions`)
//! - `value` (required): The actual value, must match the referenced type
//! - `name` (optional): Human-friendly display name, defaults to `value` if not provided
//!
//! ## Benefits
//!
//! - **Extensibility**: Add new values without changing the schema structure
//! - **Self-describing**: Values declare which type they belong to
//! - **Plugin-friendly**: External sources can contribute enum values
//! - **Type-safe**: Values are validated against their referenced type

use serde::Deserialize;
use std::collections::HashMap;

/// A property/setting from the schema
#[derive(Debug, Clone)]
pub struct SettingSchema {
    /// JSON pointer path (e.g., "/editor/tab_size")
    pub path: String,
    /// Human-readable name derived from property name
    pub name: String,
    /// Description from schema
    pub description: Option<String>,
    /// The type of this setting
    pub setting_type: SettingType,
    /// Default value (as JSON)
    pub default: Option<serde_json::Value>,
    /// Whether this field is read-only (cannot be edited by user)
    pub read_only: bool,
}

/// Type of a setting, determines which control to render
#[derive(Debug, Clone)]
pub enum SettingType {
    /// Boolean toggle
    Boolean,
    /// Integer number with optional min/max
    Integer {
        minimum: Option<i64>,
        maximum: Option<i64>,
    },
    /// Floating point number
    Number {
        minimum: Option<f64>,
        maximum: Option<f64>,
    },
    /// Free-form string
    String,
    /// String with enumerated options (display name, value)
    Enum { options: Vec<EnumOption> },
    /// Array of strings
    StringArray,
    /// Array of objects with a schema (for keybindings, etc.)
    ObjectArray {
        item_schema: Box<SettingSchema>,
        /// JSON pointer to field within item to display as preview (e.g., "/action")
        display_field: Option<String>,
    },
    /// Nested object (category)
    Object { properties: Vec<SettingSchema> },
    /// Map with string keys (for languages, lsp configs)
    Map {
        value_schema: Box<SettingSchema>,
        /// JSON pointer to field within value to display as preview (e.g., "/command")
        display_field: Option<String>,
        /// Whether to disallow adding new entries (entries are auto-managed)
        no_add: bool,
    },
    /// Complex type we can't edit directly
    Complex,
}

/// An option in an enum type
#[derive(Debug, Clone)]
pub struct EnumOption {
    /// Display name shown in UI
    pub name: String,
    /// Actual value stored in config
    pub value: String,
}

/// A category in the settings tree
#[derive(Debug, Clone)]
pub struct SettingCategory {
    /// Category name (e.g., "Editor", "File Explorer")
    pub name: String,
    /// JSON path prefix for this category
    pub path: String,
    /// Description of this category
    pub description: Option<String>,
    /// Settings in this category
    pub settings: Vec<SettingSchema>,
    /// Subcategories
    pub subcategories: Vec<SettingCategory>,
}

/// Raw JSON Schema structure for deserialization
#[derive(Debug, Deserialize)]
struct RawSchema {
    #[serde(rename = "type")]
    schema_type: Option<SchemaType>,
    description: Option<String>,
    default: Option<serde_json::Value>,
    properties: Option<HashMap<String, RawSchema>>,
    items: Option<Box<RawSchema>>,
    #[serde(rename = "enum")]
    enum_values: Option<Vec<serde_json::Value>>,
    minimum: Option<serde_json::Number>,
    maximum: Option<serde_json::Number>,
    #[serde(rename = "$ref")]
    ref_path: Option<String>,
    #[serde(rename = "$defs")]
    defs: Option<HashMap<String, RawSchema>>,
    #[serde(rename = "additionalProperties")]
    additional_properties: Option<AdditionalProperties>,
    /// Extensible enum values - see module docs for details
    #[serde(rename = "x-enum-values", default)]
    extensible_enum_values: Vec<EnumValueEntry>,
    /// Custom extension: field to display as preview in Map/ObjectArray entries
    /// e.g., "/command" for OnSaveAction, "/action" for Keybinding
    #[serde(rename = "x-display-field")]
    display_field: Option<String>,
    /// Whether this field is read-only
    #[serde(rename = "readOnly", default)]
    read_only: bool,
    /// Whether this Map-type property should be rendered as its own category
    #[serde(rename = "x-standalone-category", default)]
    standalone_category: bool,
    /// Whether this Map should disallow adding new entries (entries are auto-managed)
    #[serde(rename = "x-no-add", default)]
    no_add: bool,
}

/// An entry in the x-enum-values array
#[derive(Debug, Deserialize)]
struct EnumValueEntry {
    /// JSON pointer to the type being extended (e.g., "#/$defs/ThemeOptions")
    #[serde(rename = "ref")]
    ref_path: String,
    /// Human-friendly display name (optional, defaults to value)
    name: Option<String>,
    /// The actual value (must match the referenced type)
    value: serde_json::Value,
}

/// additionalProperties can be a boolean or a schema object
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum AdditionalProperties {
    Bool(bool),
    Schema(Box<RawSchema>),
}

/// JSON Schema type can be a single string or an array of strings
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum SchemaType {
    Single(String),
    Multiple(Vec<String>),
}

impl SchemaType {
    /// Get the primary type (first type if array, or the single type)
    fn primary(&self) -> Option<&str> {
        match self {
            Self::Single(s) => Some(s.as_str()),
            Self::Multiple(v) => v.first().map(|s| s.as_str()),
        }
    }
}

/// Map from $ref paths to their enum options
type EnumValuesMap = HashMap<String, Vec<EnumOption>>;

/// Parse the JSON Schema and build the category tree
pub fn parse_schema(schema_json: &str) -> Result<Vec<SettingCategory>, serde_json::Error> {
    let raw: RawSchema = serde_json::from_str(schema_json)?;

    let defs = raw.defs.unwrap_or_default();
    let properties = raw.properties.unwrap_or_default();

    // Build enum values map from x-enum-values entries
    let enum_values_map = build_enum_values_map(&raw.extensible_enum_values);

    let mut categories = Vec::new();
    let mut top_level_settings = Vec::new();

    // Process each top-level property (sorted for deterministic output)
    let mut sorted_props: Vec<_> = properties.into_iter().collect();
    sorted_props.sort_by(|a, b| a.0.cmp(&b.0));
    for (name, prop) in sorted_props {
        let path = format!("/{}", name);
        let display_name = humanize_name(&name);

        // Resolve references
        let resolved = resolve_ref(&prop, &defs);

        // Check if this property should be a standalone category (for Map types)
        if prop.standalone_category {
            // Create a category with the Map setting as its only content
            let setting = parse_setting(&name, &path, &prop, &defs, &enum_values_map);
            categories.push(SettingCategory {
                name: display_name,
                path: path.clone(),
                description: prop.description.clone().or(resolved.description.clone()),
                settings: vec![setting],
                subcategories: Vec::new(),
            });
        } else if let Some(ref inner_props) = resolved.properties {
            // This is a category with nested settings
            let settings = parse_properties(inner_props, &path, &defs, &enum_values_map);
            categories.push(SettingCategory {
                name: display_name,
                path: path.clone(),
                description: resolved.description.clone(),
                settings,
                subcategories: Vec::new(),
            });
        } else {
            // This is a top-level setting
            let setting = parse_setting(&name, &path, &prop, &defs, &enum_values_map);
            top_level_settings.push(setting);
        }
    }

    // If there are top-level settings, create a "General" category for them
    if !top_level_settings.is_empty() {
        // Sort top-level settings alphabetically
        top_level_settings.sort_by(|a, b| a.name.cmp(&b.name));
        categories.insert(
            0,
            SettingCategory {
                name: "General".to_string(),
                path: String::new(),
                description: Some("General settings".to_string()),
                settings: top_level_settings,
                subcategories: Vec::new(),
            },
        );
    }

    // Sort categories alphabetically, but keep General first
    categories.sort_by(|a, b| match (a.name.as_str(), b.name.as_str()) {
        ("General", _) => std::cmp::Ordering::Less,
        (_, "General") => std::cmp::Ordering::Greater,
        (a, b) => a.cmp(b),
    });

    Ok(categories)
}

/// Build a map from $ref paths to their enum options
fn build_enum_values_map(entries: &[EnumValueEntry]) -> EnumValuesMap {
    let mut map: EnumValuesMap = HashMap::new();

    for entry in entries {
        let value_str = match &entry.value {
            serde_json::Value::String(s) => s.clone(),
            other => other.to_string(),
        };

        let option = EnumOption {
            name: entry.name.clone().unwrap_or_else(|| value_str.clone()),
            value: value_str,
        };

        map.entry(entry.ref_path.clone()).or_default().push(option);
    }

    map
}

/// Parse properties into settings
fn parse_properties(
    properties: &HashMap<String, RawSchema>,
    parent_path: &str,
    defs: &HashMap<String, RawSchema>,
    enum_values_map: &EnumValuesMap,
) -> Vec<SettingSchema> {
    let mut settings = Vec::new();

    for (name, prop) in properties {
        let path = format!("{}/{}", parent_path, name);
        let setting = parse_setting(name, &path, prop, defs, enum_values_map);
        settings.push(setting);
    }

    // Sort settings alphabetically by name
    settings.sort_by(|a, b| a.name.cmp(&b.name));

    settings
}

/// Parse a single setting from its schema
fn parse_setting(
    name: &str,
    path: &str,
    schema: &RawSchema,
    defs: &HashMap<String, RawSchema>,
    enum_values_map: &EnumValuesMap,
) -> SettingSchema {
    let setting_type = determine_type(schema, defs, enum_values_map);

    // Get description from resolved ref if not present on schema
    let resolved = resolve_ref(schema, defs);
    let description = schema
        .description
        .clone()
        .or_else(|| resolved.description.clone());

    // Check for readOnly flag on schema or resolved ref
    let read_only = schema.read_only || resolved.read_only;

    SettingSchema {
        path: path.to_string(),
        name: humanize_name(name),
        description,
        setting_type,
        default: schema.default.clone(),
        read_only,
    }
}

/// Determine the SettingType from a schema
fn determine_type(
    schema: &RawSchema,
    defs: &HashMap<String, RawSchema>,
    enum_values_map: &EnumValuesMap,
) -> SettingType {
    // Check for extensible enum values via $ref
    if let Some(ref ref_path) = schema.ref_path {
        if let Some(options) = enum_values_map.get(ref_path) {
            if !options.is_empty() {
                return SettingType::Enum {
                    options: options.clone(),
                };
            }
        }
    }

    // Resolve ref for type checking
    let resolved = resolve_ref(schema, defs);

    // Check for inline enum values (on original schema or resolved ref)
    let enum_values = schema
        .enum_values
        .as_ref()
        .or(resolved.enum_values.as_ref());
    if let Some(values) = enum_values {
        let options: Vec<EnumOption> = values
            .iter()
            .filter_map(|v| {
                if v.is_null() {
                    // null in enum represents "auto-detect" or "default"
                    Some(EnumOption {
                        name: "Auto-detect".to_string(),
                        value: String::new(), // Empty string represents null
                    })
                } else {
                    v.as_str().map(|s| EnumOption {
                        name: s.to_string(),
                        value: s.to_string(),
                    })
                }
            })
            .collect();
        if !options.is_empty() {
            return SettingType::Enum { options };
        }
    }

    // Check type field
    match resolved.schema_type.as_ref().and_then(|t| t.primary()) {
        Some("boolean") => SettingType::Boolean,
        Some("integer") => {
            let minimum = resolved.minimum.as_ref().and_then(|n| n.as_i64());
            let maximum = resolved.maximum.as_ref().and_then(|n| n.as_i64());
            SettingType::Integer { minimum, maximum }
        }
        Some("number") => {
            let minimum = resolved.minimum.as_ref().and_then(|n| n.as_f64());
            let maximum = resolved.maximum.as_ref().and_then(|n| n.as_f64());
            SettingType::Number { minimum, maximum }
        }
        Some("string") => SettingType::String,
        Some("array") => {
            // Check if it's an array of strings or objects
            if let Some(ref items) = resolved.items {
                let item_resolved = resolve_ref(items, defs);
                if item_resolved.schema_type.as_ref().and_then(|t| t.primary()) == Some("string") {
                    return SettingType::StringArray;
                }
                // Check if items reference an object type
                if items.ref_path.is_some() {
                    // Parse the item schema from the referenced definition
                    let item_schema =
                        parse_setting("item", "", item_resolved, defs, enum_values_map);

                    // Only create ObjectArray if the item is an object with properties
                    if matches!(item_schema.setting_type, SettingType::Object { .. }) {
                        // Get display_field from x-display-field in the referenced schema
                        let display_field = item_resolved.display_field.clone();
                        return SettingType::ObjectArray {
                            item_schema: Box::new(item_schema),
                            display_field,
                        };
                    }
                }
            }
            SettingType::Complex
        }
        Some("object") => {
            // Check for additionalProperties (map type)
            if let Some(ref add_props) = resolved.additional_properties {
                match add_props {
                    AdditionalProperties::Schema(schema_box) => {
                        let inner_resolved = resolve_ref(schema_box, defs);
                        let value_schema =
                            parse_setting("value", "", inner_resolved, defs, enum_values_map);

                        // Get display_field from x-display-field in the referenced schema
                        let display_field = inner_resolved.display_field.clone();

                        // Get no_add from the parent schema (resolved)
                        let no_add = resolved.no_add;

                        return SettingType::Map {
                            value_schema: Box::new(value_schema),
                            display_field,
                            no_add,
                        };
                    }
                    AdditionalProperties::Bool(true) => {
                        // additionalProperties: true means any value is allowed
                        return SettingType::Complex;
                    }
                    AdditionalProperties::Bool(false) => {
                        // additionalProperties: false means no additional properties
                        // Fall through to check for fixed properties
                    }
                }
            }
            // Regular object with fixed properties
            if let Some(ref props) = resolved.properties {
                let properties = parse_properties(props, "", defs, enum_values_map);
                return SettingType::Object { properties };
            }
            SettingType::Complex
        }
        _ => SettingType::Complex,
    }
}

/// Resolve a $ref to its definition
fn resolve_ref<'a>(schema: &'a RawSchema, defs: &'a HashMap<String, RawSchema>) -> &'a RawSchema {
    if let Some(ref ref_path) = schema.ref_path {
        // Parse ref path like "#/$defs/EditorConfig"
        if let Some(def_name) = ref_path.strip_prefix("#/$defs/") {
            if let Some(def) = defs.get(def_name) {
                return def;
            }
        }
    }
    schema
}

/// Convert snake_case to Title Case
fn humanize_name(name: &str) -> String {
    name.split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().chain(chars).collect(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_SCHEMA: &str = r##"
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Config",
  "type": "object",
  "properties": {
    "theme": {
      "description": "Color theme name",
      "type": "string",
      "default": "high-contrast"
    },
    "check_for_updates": {
      "description": "Check for new versions on quit",
      "type": "boolean",
      "default": true
    },
    "editor": {
      "description": "Editor settings",
      "$ref": "#/$defs/EditorConfig"
    }
  },
  "$defs": {
    "EditorConfig": {
      "description": "Editor behavior configuration",
      "type": "object",
      "properties": {
        "tab_size": {
          "description": "Number of spaces per tab",
          "type": "integer",
          "minimum": 1,
          "maximum": 16,
          "default": 4
        },
        "line_numbers": {
          "description": "Show line numbers",
          "type": "boolean",
          "default": true
        }
      }
    }
  }
}
"##;

    #[test]
    fn test_parse_schema() {
        let categories = parse_schema(SAMPLE_SCHEMA).unwrap();

        // Should have General and Editor categories
        assert_eq!(categories.len(), 2);
        assert_eq!(categories[0].name, "General");
        assert_eq!(categories[1].name, "Editor");
    }

    #[test]
    fn test_general_category() {
        let categories = parse_schema(SAMPLE_SCHEMA).unwrap();
        let general = &categories[0];

        // General should have theme and check_for_updates
        assert_eq!(general.settings.len(), 2);

        let theme = general
            .settings
            .iter()
            .find(|s| s.path == "/theme")
            .unwrap();
        assert!(matches!(theme.setting_type, SettingType::String));

        let updates = general
            .settings
            .iter()
            .find(|s| s.path == "/check_for_updates")
            .unwrap();
        assert!(matches!(updates.setting_type, SettingType::Boolean));
    }

    #[test]
    fn test_editor_category() {
        let categories = parse_schema(SAMPLE_SCHEMA).unwrap();
        let editor = &categories[1];

        assert_eq!(editor.path, "/editor");
        assert_eq!(editor.settings.len(), 2);

        let tab_size = editor
            .settings
            .iter()
            .find(|s| s.name == "Tab Size")
            .unwrap();
        if let SettingType::Integer { minimum, maximum } = &tab_size.setting_type {
            assert_eq!(*minimum, Some(1));
            assert_eq!(*maximum, Some(16));
        } else {
            panic!("Expected integer type");
        }
    }

    #[test]
    fn test_humanize_name() {
        assert_eq!(humanize_name("tab_size"), "Tab Size");
        assert_eq!(humanize_name("line_numbers"), "Line Numbers");
        assert_eq!(humanize_name("check_for_updates"), "Check For Updates");
        assert_eq!(humanize_name("lsp"), "Lsp");
    }
}
