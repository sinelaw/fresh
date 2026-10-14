//! TypeScript types for the editor config, generated from its JSON Schema.
//!
//! `editor.getConfig()` and `editor.getUserConfig()` return the config as a
//! plain object. Its Rust type lives in `fresh-editor-core`, which this crate
//! can't depend on, but `crates/fresh-editor/plugins/config-schema.json` is
//! generated from it (and checked in CI), so the TypeScript is built from
//! that schema.
//!
//! The result is one global name: an `interface FreshConfig` for the whole
//! config, plus a `namespace FreshConfig` holding the types it refers to
//! (`FreshConfig.EditorConfig`, ...). Every property is optional, because
//! `getUserConfig()` returns only what the user set.

use serde_json::{Map, Value};

/// The global name of the config type.
pub const CONFIG_TYPE: &str = "FreshConfig";

/// Render the TypeScript declarations for a config JSON Schema.
pub fn schema_to_ts(schema: &Value) -> Result<String, String> {
    let root = schema
        .as_object()
        .ok_or("config schema is not a JSON object")?;
    let mut out = String::new();

    out.push_str(&jsdoc(
        "The editor's configuration, as returned by `editor.getConfig()` and \
         `editor.getUserConfig()`. Generated from the config's JSON Schema.\n\n\
         Every property is optional: `getUserConfig()` returns only the values \
         the user set, and `getConfig()` the full merged config.",
        None,
        "",
    ));
    out.push_str(&format!(
        "interface {CONFIG_TYPE} {}\n",
        object_body(root, "", true)?
    ));

    let defs = root
        .get("$defs")
        .and_then(Value::as_object)
        .cloned()
        .unwrap_or_default();
    let mut names: Vec<&String> = defs.keys().collect();
    names.sort();
    out.push_str(&format!("declare namespace {CONFIG_TYPE} {{\n"));
    for name in names {
        let def = defs[name]
            .as_object()
            .ok_or_else(|| format!("$defs.{name} is not an object"))?;
        out.push_str(&jsdoc_for(def, "  "));
        if is_plain_object(def) {
            out.push_str(&format!(
                "  interface {name} {}\n",
                object_body(def, "  ", false)?
            ));
        } else {
            out.push_str(&format!(
                "  type {name} = {};\n",
                type_of(def, "  ", false)?
            ));
        }
    }
    out.push_str("}\n");
    Ok(out)
}

/// An object schema with named properties and no other shape.
fn is_plain_object(s: &Map<String, Value>) -> bool {
    s.get("type").and_then(Value::as_str) == Some("object")
        && s.contains_key("properties")
        && !s.contains_key("additionalProperties")
}

/// `{ ... }` for an object schema's properties, one per line.
fn object_body(s: &Map<String, Value>, indent: &str, top: bool) -> Result<String, String> {
    let inner = format!("{indent}  ");
    let mut out = String::from("{\n");
    if let Some(props) = s.get("properties").and_then(Value::as_object) {
        for (key, prop) in props {
            let prop = prop
                .as_object()
                .ok_or_else(|| format!("property {key} is not an object"))?;
            out.push_str(&jsdoc_for(prop, &inner));
            out.push_str(&format!(
                "{inner}{}?: {};\n",
                property_name(key),
                type_of(prop, &inner, top)?
            ));
        }
    }
    out.push_str(indent);
    out.push('}');
    Ok(out)
}

/// The TypeScript type for a schema node. `top` means the node is outside
/// the namespace, so `$ref`s need the `FreshConfig.` prefix.
fn type_of(s: &Map<String, Value>, indent: &str, top: bool) -> Result<String, String> {
    if let Some(r) = s.get("$ref").and_then(Value::as_str) {
        let name = r
            .strip_prefix("#/$defs/")
            .ok_or_else(|| format!("unsupported $ref {r}"))?;
        return Ok(if top {
            format!("{CONFIG_TYPE}.{name}")
        } else {
            name.to_string()
        });
    }
    for key in ["anyOf", "oneOf"] {
        if let Some(alts) = s.get(key).and_then(Value::as_array) {
            let mut parts = Vec::new();
            for alt in alts {
                let alt = alt
                    .as_object()
                    .ok_or_else(|| format!("{key} member is not an object"))?;
                let t = type_of(alt, indent, top)?;
                if !parts.contains(&t) {
                    parts.push(t);
                }
            }
            return Ok(parts.join(" | "));
        }
    }
    if let Some(c) = s.get("const") {
        return Ok(c.to_string());
    }
    if let Some(values) = s.get("enum").and_then(Value::as_array) {
        return Ok(values
            .iter()
            .map(Value::to_string)
            .collect::<Vec<_>>()
            .join(" | "));
    }
    let types: Vec<&str> = match s.get("type") {
        Some(Value::String(t)) => vec![t.as_str()],
        Some(Value::Array(ts)) => ts.iter().filter_map(Value::as_str).collect(),
        _ => return Ok("unknown".to_string()),
    };
    let mut parts = Vec::new();
    for t in types {
        parts.push(match t {
            "string" => "string".to_string(),
            "integer" | "number" => "number".to_string(),
            "boolean" => "boolean".to_string(),
            "null" => "null".to_string(),
            "array" => match s.get("items").and_then(Value::as_object) {
                Some(items) => {
                    let item = type_of(items, indent, top)?;
                    if item.contains(' ') && !item.starts_with('{') {
                        format!("({item})[]")
                    } else {
                        format!("{item}[]")
                    }
                }
                None => "unknown[]".to_string(),
            },
            "object" => match (s.get("properties"), s.get("additionalProperties")) {
                (Some(_), _) => object_body(s, indent, top)?,
                (None, Some(Value::Object(values))) => {
                    format!("Record<string, {}>", type_of(values, indent, top)?)
                }
                _ => "Record<string, unknown>".to_string(),
            },
            other => return Err(format!("unsupported schema type {other}")),
        });
    }
    Ok(parts.join(" | "))
}

fn property_name(key: &str) -> String {
    let ident = key
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_' || c == '$')
        && key
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$');
    if ident {
        key.to_string()
    } else {
        Value::String(key.to_string()).to_string()
    }
}

/// JSDoc from a schema node's `description` and short `default`.
fn jsdoc_for(s: &Map<String, Value>, indent: &str) -> String {
    let description = s.get("description").and_then(Value::as_str).unwrap_or("");
    let doc = jsdoc(description, s.get("default"), indent);
    // Nothing to say: no description and no short default.
    if doc == format!("{indent}/**\n{indent} */\n") {
        String::new()
    } else {
        doc
    }
}

fn jsdoc(description: &str, default: Option<&Value>, indent: &str) -> String {
    let mut lines: Vec<String> = description
        .replace("*/", "*\\/")
        .lines()
        .map(str::to_string)
        .collect();
    if let Some(d) = default {
        let d = d.to_string();
        if d.len() <= 60 && d != "null" && d != "{}" && d != "[]" {
            if !lines.is_empty() {
                lines.push(String::new());
            }
            lines.push(format!("Default: `{}`", d.replace("*/", "*\\/")));
        }
    }
    let mut out = format!("{indent}/**\n");
    for line in lines {
        if line.is_empty() {
            out.push_str(&format!("{indent} *\n"));
        } else {
            out.push_str(&format!("{indent} * {line}\n"));
        }
    }
    out.push_str(&format!("{indent} */\n"));
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn converts_refs_unions_maps_and_enums() {
        let schema = json!({
            "title": "Config",
            "type": "object",
            "properties": {
                "editor": { "description": "Editor settings", "$ref": "#/$defs/Editor" },
                "lsp": { "type": "object", "additionalProperties": { "$ref": "#/$defs/Server" } },
                "theme": { "type": ["string", "null"], "default": "dark" }
            },
            "$defs": {
                "Editor": {
                    "type": "object",
                    "properties": {
                        "tab_size": { "type": "integer", "default": 4 },
                        "rulers": { "type": "array", "items": { "type": "integer" } }
                    }
                },
                "Server": { "anyOf": [{ "$ref": "#/$defs/Editor" }, { "type": "array", "items": { "$ref": "#/$defs/Editor" } }] },
                "Style": { "type": "string", "enum": ["bar", "block"] }
            }
        });
        let ts = schema_to_ts(&schema).unwrap();
        assert!(ts.contains("interface FreshConfig {"), "{ts}");
        assert!(ts.contains("  editor?: FreshConfig.Editor;"), "{ts}");
        assert!(
            ts.contains("  lsp?: Record<string, FreshConfig.Server>;"),
            "{ts}"
        );
        assert!(ts.contains("  theme?: string | null;"), "{ts}");
        assert!(ts.contains("Default: `\"dark\"`"), "{ts}");
        assert!(ts.contains("declare namespace FreshConfig {"), "{ts}");
        assert!(ts.contains("    tab_size?: number;"), "{ts}");
        assert!(ts.contains("    rulers?: number[];"), "{ts}");
        assert!(ts.contains("  type Server = Editor | Editor[];"), "{ts}");
        assert!(ts.contains("  type Style = \"bar\" | \"block\";"), "{ts}");
    }

    #[test]
    fn quotes_property_names_that_are_not_identifiers() {
        assert_eq!(property_name("tab_size"), "tab_size");
        assert_eq!(property_name("lsp/custom"), "\"lsp/custom\"");
    }
}
