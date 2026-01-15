//! Generate JSON Schema for Fresh configuration
//!
//! This binary generates a JSON Schema from the Config struct using schemars.
//! It's used to generate plugins/config-schema.json for the config editor.
//!
//! Usage:
//!   cargo run --features dev-bins --bin generate_schema > plugins/config-schema.json

use fresh::config::Config;
use schemars::schema_for;

fn main() {
    let schema = schema_for!(Config);
    let mut json: serde_json::Value =
        serde_json::to_value(&schema).expect("Failed to serialize schema");

    // Remove the default value for menu - it's too large and the schema
    // is for validation, not for storing defaults
    if let Some(properties) = json.get_mut("properties") {
        if let Some(menu) = properties.get_mut("menu") {
            if let Some(obj) = menu.as_object_mut() {
                obj.remove("default");
            }
        }
    }

    let output = serde_json::to_string_pretty(&json).expect("Failed to serialize schema");
    println!("{}", output);
}
