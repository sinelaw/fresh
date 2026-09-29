//! Settings labels from the config schema, translated through a catalog of
//! the test's own, so every kind of key is covered whatever the shipped
//! catalogs translate.

use fresh::view::settings::schema::{
    parse_schema, section_display_name, SettingCategory, SettingSchema, SettingType,
};

/// A locale no shipped catalog uses.
const LOCALE: &str = "zz";

const CATALOG: &str = r#"{
  "settings.field.alpha": "Zulu alpha",
  "settings.field.alpha_desc": "Translated alpha.",
  "settings.field.page.nested.inner": "Translated inner",
  "settings.field.page.nested.inner_desc": "Translated inner.",
  "settings.field.page.table.*.grammar": "Translated map grammar",
  "settings.field.page.list.*.grammar_desc": "Translated list grammar.",
  "settings.category.page": "Translated page",
  "settings.category.page_desc": "Translated page.",
  "settings.section.odd_bits": "Translated odd bits"
}"#;

const SCHEMA: &str = r##"{
  "type": "object",
  "properties": {
    "alpha": { "description": "Alpha.", "type": "boolean" },
    "beta": { "description": "Beta.", "type": "boolean", "x-section": "Plain" },
    "page": { "description": "Page.", "$ref": "#/$defs/Page" }
  },
  "$defs": {
    "Page": {
      "type": "object",
      "properties": {
        "nested": {
          "type": "object",
          "properties": {
            "inner": { "description": "Inner.", "type": "boolean", "x-section": "Odd Bits" }
          }
        },
        "table": {
          "type": "object",
          "additionalProperties": { "$ref": "#/$defs/Entry" }
        },
        "list": {
          "type": "array",
          "items": { "$ref": "#/$defs/Entry" }
        }
      }
    },
    "Entry": {
      "type": "object",
      "properties": {
        "grammar": { "description": "Grammar.", "type": "string" }
      }
    }
  }
}"##;

/// The schema parsed in the test locale. Returns with the locale still set,
/// for [`section_display_name`]; the pin keeps other tests from changing it.
fn parse() -> Vec<SettingCategory> {
    fresh_i18n::register_locales(&[(LOCALE, CATALOG)]);
    fresh_i18n::set_locale(LOCALE);
    parse_schema(SCHEMA).unwrap()
}

fn category<'a>(categories: &'a [SettingCategory], name: &str) -> &'a SettingCategory {
    categories
        .iter()
        .find(|c| c.name == name)
        .unwrap_or_else(|| panic!("no category {name:?}"))
}

fn setting<'a>(settings: &'a [SettingSchema], path: &str) -> &'a SettingSchema {
    settings
        .iter()
        .find(|s| s.path == path)
        .unwrap_or_else(|| panic!("no setting {path:?}"))
}

/// The one field of an entry's object: map values and array items.
fn entry_field(entry: &SettingSchema) -> &SettingSchema {
    match &entry.setting_type {
        SettingType::Object { properties } => setting(properties, "/grammar"),
        other => panic!("entry is {other:?}, not an object"),
    }
}

#[test]
fn test_settings_schema_labels_follow_the_locale() {
    let _pin = crate::common::global_state::pin_config_globals();
    let categories = parse();

    let general = category(&categories, "General");
    let alpha = setting(&general.settings, "/alpha");
    assert_eq!(alpha.name, "Zulu alpha");
    assert_eq!(alpha.description.as_deref(), Some("Translated alpha."));

    let page = category(&categories, "Page");
    assert_eq!(page.display_name, "Translated page");
    assert_eq!(page.description.as_deref(), Some("Translated page."));

    // A nested field's path is relative to its object; its key is not.
    let SettingType::Object { properties } = &setting(&page.settings, "/page/nested").setting_type
    else {
        panic!("nested is not an object");
    };
    let inner = setting(properties, "/inner");
    assert_eq!(inner.name, "Translated inner");
    assert_eq!(inner.description.as_deref(), Some("Translated inner."));

    // Map values and array items stand for any key: `*`.
    let SettingType::Map { value_schema, .. } = &setting(&page.settings, "/page/table").setting_type
    else {
        panic!("table is not a map");
    };
    let grammar = entry_field(value_schema);
    assert_eq!(grammar.name, "Translated map grammar");
    assert_eq!(grammar.description.as_deref(), Some("Grammar."));

    let SettingType::ObjectArray { item_schema, .. } =
        &setting(&page.settings, "/page/list").setting_type
    else {
        panic!("list is not an object array");
    };
    let grammar = entry_field(item_schema);
    assert_eq!(grammar.name, "Grammar");
    assert_eq!(grammar.description.as_deref(), Some("Translated list grammar."));

    assert_eq!(inner.section.as_deref(), Some("Odd Bits"));
    assert_eq!(section_display_name("Odd Bits"), "Translated odd bits");
}

#[test]
fn test_settings_schema_labels_fall_back_to_the_schema() {
    let _pin = crate::common::global_state::pin_config_globals();
    let categories = parse();

    let general = category(&categories, "General");
    assert_eq!(general.display_name, "General");
    assert_eq!(general.description.as_deref(), Some("General settings"));

    let beta = setting(&general.settings, "/beta");
    assert_eq!(beta.name, "Beta");
    assert_eq!(beta.description.as_deref(), Some("Beta."));
    assert_eq!(section_display_name("Plain"), "Plain");
}

/// Settings are ordered by path, not by their translated names, so a page
/// lists them in the same order in every locale.
#[test]
fn test_settings_schema_order_ignores_translated_names() {
    let _pin = crate::common::global_state::pin_config_globals();
    let categories = parse();

    // "Zulu alpha" sorts after "Beta" by name.
    let paths: Vec<&str> = category(&categories, "General")
        .settings
        .iter()
        .map(|s| s.path.as_str())
        .collect();
    assert_eq!(paths, ["/alpha", "/beta"]);
}
