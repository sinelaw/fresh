//! Test that validates all theme schema i18n keys exist in theme_editor.i18n.json

#[cfg(feature = "runtime")]
use fresh::view::theme::get_theme_schema;

#[cfg(feature = "runtime")]
use std::collections::HashMap;

/// Parse the raw JSON schema and extract section/field names with their i18n keys
#[cfg(feature = "runtime")]
#[allow(clippy::type_complexity)]
fn parse_schema_i18n_keys(
    schema: &serde_json::Value,
) -> Vec<(String, String, String, Vec<(String, String, String)>)> {
    // Returns: Vec<(section_name, section_i18n_name, section_i18n_desc, Vec<(field_key, field_i18n_name, field_i18n_desc)>)>
    let mut result = Vec::new();

    let defs = schema.get("$defs");
    let properties = match schema.get("properties").and_then(|p| p.as_object()) {
        Some(p) => p,
        None => return result,
    };

    for (section_name, section_schema) in properties {
        // Skip "name" field - it's not a color section
        if section_name == "name" {
            continue;
        }

        let section_i18n_name = format!("section.{}", section_name);
        let section_i18n_desc = format!("section.{}_desc", section_name);

        // Resolve $ref to get the actual type definition
        let resolved_schema = section_schema
            .get("$ref")
            .and_then(|r| r.as_str())
            .and_then(|ref_str| {
                // $ref format: "#/$defs/TypeName"
                ref_str
                    .strip_prefix("#/$defs/")
                    .and_then(|type_name| defs?.get(type_name))
            })
            .unwrap_or(section_schema);

        let mut fields = Vec::new();

        if let Some(section_props) = resolved_schema
            .get("properties")
            .and_then(|p| p.as_object())
        {
            for (field_name, _field_schema) in section_props {
                let field_i18n_name = format!("field.{}", field_name);
                let field_i18n_desc = format!("field.{}_desc", field_name);
                fields.push((field_name.clone(), field_i18n_name, field_i18n_desc));
            }
        }

        // Sort fields alphabetically
        fields.sort_by(|a, b| a.0.cmp(&b.0));

        result.push((
            section_name.clone(),
            section_i18n_name,
            section_i18n_desc,
            fields,
        ));
    }

    result
}

#[cfg(feature = "runtime")]
#[test]
fn test_theme_schema_i18n_keys_exist() {
    // Load the theme schema
    let schema = get_theme_schema();

    // Parse schema to get i18n keys
    let sections = parse_schema_i18n_keys(&schema);

    // Load the i18n file
    let i18n_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("plugins")
        .join("theme_editor.i18n.json");

    let i18n_content = std::fs::read_to_string(&i18n_path)
        .unwrap_or_else(|e| panic!("Failed to read i18n file at {:?}: {}", i18n_path, e));

    let i18n: HashMap<String, HashMap<String, String>> = serde_json::from_str(&i18n_content)
        .unwrap_or_else(|e| panic!("Failed to parse i18n JSON: {}", e));

    // Get English translations (the primary/required locale)
    let en_translations = i18n
        .get("en")
        .expect("Missing 'en' locale in theme_editor.i18n.json");

    let mut missing_keys = Vec::new();

    // Check all sections and fields have their i18n keys
    for (section_name, section_i18n_name, section_i18n_desc, fields) in &sections {
        // Check section keys
        if !en_translations.contains_key(section_i18n_name) {
            missing_keys.push(format!(
                "Section '{}': missing key '{}'",
                section_name, section_i18n_name
            ));
        }
        if !en_translations.contains_key(section_i18n_desc) {
            missing_keys.push(format!(
                "Section '{}': missing key '{}'",
                section_name, section_i18n_desc
            ));
        }

        // Check field keys
        for (field_key, field_i18n_name, field_i18n_desc) in fields {
            if !en_translations.contains_key(field_i18n_name) {
                missing_keys.push(format!(
                    "Field '{}.{}': missing key '{}'",
                    section_name, field_key, field_i18n_name
                ));
            }
            if !en_translations.contains_key(field_i18n_desc) {
                missing_keys.push(format!(
                    "Field '{}.{}': missing key '{}'",
                    section_name, field_key, field_i18n_desc
                ));
            }
        }
    }

    if !missing_keys.is_empty() {
        panic!(
            "Missing {} i18n keys in theme_editor.i18n.json:\n  - {}",
            missing_keys.len(),
            missing_keys.join("\n  - ")
        );
    }
}

/// Test that the theme schema has expected sections
#[cfg(feature = "runtime")]
#[test]
fn test_theme_schema_has_expected_sections() {
    let schema = get_theme_schema();
    let sections = parse_schema_i18n_keys(&schema);

    let section_names: Vec<&str> = sections.iter().map(|s| s.0.as_str()).collect();

    // Verify we have all expected color sections
    let expected_sections = ["editor", "ui", "search", "diagnostic", "syntax"];

    for expected in &expected_sections {
        assert!(
            section_names.contains(expected),
            "Missing expected section '{}'. Found: {:?}",
            expected,
            section_names
        );
    }
}

/// Test that each section has at least one field
#[cfg(feature = "runtime")]
#[test]
fn test_theme_schema_sections_have_fields() {
    let schema = get_theme_schema();
    let sections = parse_schema_i18n_keys(&schema);

    for (section_name, _, _, fields) in &sections {
        assert!(
            !fields.is_empty(),
            "Section '{}' has no fields",
            section_name
        );
    }
}
