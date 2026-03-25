//! Generate JSON Schemas for Fresh configuration and themes
//!
//! This binary generates JSON Schemas from Rust structs using schemars.
//!
//! Usage:
//!   cargo run --features dev-bins --bin generate_schema config > plugins/config-schema.json
//!   cargo run --features dev-bins --bin generate_schema theme > plugins/schemas/theme.schema.json

use fresh::config::Config;
use fresh::services::packages::PackageManifest;
use fresh::view::theme::ThemeFile;
use schemars::schema_for;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let schema_type = args.get(1).map(|s| s.as_str()).unwrap_or("config");

    let json: serde_json::Value = match schema_type {
        "config" => {
            let schema = schema_for!(Config);
            let mut json = serde_json::to_value(&schema).expect("Failed to serialize schema");

            // Remove the default value for menu - it's too large and the schema
            // is for validation, not for storing defaults
            if let Some(properties) = json.get_mut("properties") {
                if let Some(menu) = properties.get_mut("menu") {
                    if let Some(obj) = menu.as_object_mut() {
                        obj.remove("default");
                    }
                }
            }
            json
        }
        "theme" => {
            let schema = schema_for!(ThemeFile);
            serde_json::to_value(&schema).expect("Failed to serialize schema")
        }
        "package" => {
            let schema = schema_for!(PackageManifest);
            let mut json = serde_json::to_value(&schema).expect("Failed to serialize schema");
            // Add $id for the package schema
            if let Some(obj) = json.as_object_mut() {
                obj.insert(
                    "$id".to_string(),
                    serde_json::Value::String(
                        "https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/package.schema.json".to_string()
                    ),
                );
            }
            json
        }
        other => {
            eprintln!(
                "Unknown schema type: {}. Use 'config', 'theme', or 'package'.",
                other
            );
            std::process::exit(1);
        }
    };

    let output = serde_json::to_string_pretty(&json).expect("Failed to serialize schema");
    println!("{}", output);
}
