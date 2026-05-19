//! Validation, default extraction, and merging for plugin-provided
//! config schemas.
//!
//! Plugins register config fields at load time by calling one of the
//! strongly-typed `editor.defineConfigBoolean / Integer / Number /
//! String / Enum / StringArray(...)` methods from TypeScript. Each
//! call sends an `AddPluginConfigField` command to the host. The host
//! accumulates fields into a per-plugin JSON Schema fragment stored in
//! `Editor::plugin_schemas`, pre-populates the declared default into
//! `plugins.<name>.settings.<field>`, and the Settings UI uses the
//! accumulated schema to render a per-plugin sub-category.
//!
//! Trust boundary: schemas reach the host already validated per-field
//! on the JS-binding side (where errors are thrown back to plugin
//! authors). This module re-validates defensively before merging into
//! the runtime tree:
//!
//! 1. Top-level must be a JSON object with `"type": "object"`.
//! 2. No `$ref`s allowed (cross-tree references would let a plugin
//!    pull types out of the host schema namespace).
//! 3. No `x-enum-from` extension (would let a plugin point at host
//!    config paths like `/languages` — explicit design decision).

use serde_json::{Map, Value};

/// Validate a plugin-supplied JSON Schema. Returns `Ok(())` if safe to
/// merge into the host's runtime schema tree; otherwise an error
/// describing why.
pub fn validate_plugin_schema(value: &Value) -> Result<(), String> {
    let obj = value
        .as_object()
        .ok_or_else(|| "schema root must be an object".to_string())?;

    match obj.get("type") {
        Some(Value::String(s)) if s == "object" => {}
        _ => return Err("schema root must have \"type\": \"object\"".to_string()),
    }

    check_no_forbidden_keys(value)?;

    // Allow empty `properties` so a brand-new plugin that hasn't yet
    // sent its first AddPluginConfigField doesn't fail validation.
    let _ = obj
        .get("properties")
        .and_then(|p| p.as_object())
        .ok_or_else(|| "schema must declare \"properties\"".to_string())?;
    Ok(())
}

fn check_no_forbidden_keys(value: &Value) -> Result<(), String> {
    match value {
        Value::Object(m) => {
            for (k, v) in m {
                if k == "$ref" {
                    return Err("plugin schemas may not use $ref".to_string());
                }
                if k == "x-enum-from" {
                    return Err(
                        "plugin schemas may not use x-enum-from (host coupling)".to_string()
                    );
                }
                check_no_forbidden_keys(v)?;
            }
        }
        Value::Array(a) => {
            for v in a {
                check_no_forbidden_keys(v)?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Deep-merge `defaults` UNDER `target` — i.e. fill in keys that
/// `target` does not already have. Used to seed
/// `plugins.<name>.settings` from registered schema defaults without
/// clobbering values the user has already saved.
pub fn deep_merge_under(target: &mut Value, defaults: &Value) {
    if target.is_null() {
        *target = defaults.clone();
        return;
    }
    let (Value::Object(t_map), Value::Object(d_map)) = (target, defaults) else {
        return;
    };
    for (k, v) in d_map {
        match t_map.get_mut(k) {
            Some(existing) => deep_merge_under(existing, v),
            None => {
                t_map.insert(k.clone(), v.clone());
            }
        }
    }
}

/// Extract default values from a schema recursively, walking
/// `properties.<name>.default`. Returns an object with defaults filled
/// in for every property that declares one.
pub fn defaults_from_schema(schema: &Value) -> Value {
    let mut out = Map::new();
    if let Some(props) = schema.get("properties").and_then(|p| p.as_object()) {
        for (k, prop) in props {
            if let Some(d) = prop.get("default") {
                out.insert(k.clone(), d.clone());
            } else if let Some(t) = prop.get("type").and_then(|t| t.as_str()) {
                if t == "object" {
                    let nested = defaults_from_schema(prop);
                    if !nested.as_object().map(|o| o.is_empty()).unwrap_or(true) {
                        out.insert(k.clone(), nested);
                    }
                }
            }
        }
    }
    Value::Object(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn rejects_non_object_root() {
        assert!(validate_plugin_schema(&json!("hello")).is_err());
        assert!(validate_plugin_schema(&json!([])).is_err());
    }

    #[test]
    fn rejects_missing_object_type() {
        assert!(validate_plugin_schema(&json!({})).is_err());
        assert!(validate_plugin_schema(&json!({"type": "string"})).is_err());
    }

    #[test]
    fn rejects_refs() {
        let schema = json!({
            "type": "object",
            "properties": {"foo": {"$ref": "#/$defs/Bar"}}
        });
        assert!(validate_plugin_schema(&schema).is_err());
    }

    #[test]
    fn rejects_x_enum_from() {
        let schema = json!({
            "type": "object",
            "properties": {"lang": {"type": "string", "x-enum-from": "/languages"}}
        });
        assert!(validate_plugin_schema(&schema).is_err());
    }

    #[test]
    fn accepts_valid_schema() {
        let schema = json!({
            "type": "object",
            "properties": {
                "auto_enable": {"type": "boolean", "default": false},
                "max_items": {"type": "integer", "minimum": 1, "default": 3}
            }
        });
        assert!(validate_plugin_schema(&schema).is_ok());
    }

    #[test]
    fn accepts_empty_properties() {
        let schema = json!({"type": "object", "properties": {}});
        assert!(validate_plugin_schema(&schema).is_ok());
    }

    #[test]
    fn defaults_extraction() {
        let schema = json!({
            "type": "object",
            "properties": {
                "auto_enable": {"type": "boolean", "default": false},
                "max_items": {"type": "integer", "minimum": 1, "default": 3},
                "no_default": {"type": "string"}
            }
        });
        let d = defaults_from_schema(&schema);
        assert_eq!(d, json!({"auto_enable": false, "max_items": 3}));
    }
}
