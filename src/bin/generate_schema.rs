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
    let json = serde_json::to_string_pretty(&schema).expect("Failed to serialize schema");
    println!("{}", json);
}
