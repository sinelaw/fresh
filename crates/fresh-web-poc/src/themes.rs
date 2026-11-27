//! The editor's themes, served to the client: the built-in themes the
//! editor's core embeds at build time, resolved by its own loader (so a
//! theme's inherited and defaulted keys arrive filled in), and the themes
//! skins bring (`skin/<id>`, `src/skins.rs`; a user skin's only from the
//! workspace's host). The client maps each theme's
//! keys onto its CSS variables.

use crate::error::{err, Code};
use crate::skins;
use crate::workspace::Workspace;
use anyhow::Result;
use fresh_editor_core::theme::{ThemeFile, ThemeLoader, ThemeRegistry};
use serde_json::{json, Value};
use std::sync::OnceLock;

fn registry() -> &'static ThemeRegistry {
    static REGISTRY: OnceLock<ThemeRegistry> = OnceLock::new();
    REGISTRY.get_or_init(|| ThemeLoader::embedded_only().load_all(&[]))
}

/// `theme.list`: every theme's key and display name, in the editor's order,
/// then the skins' themes.
pub fn list(ws: Option<&Workspace>) -> Value {
    let mut themes: Vec<Value> = registry()
        .list()
        .iter()
        .map(|t| json!({ "key": t.key, "name": t.display_name() }))
        .collect();
    for (key, name) in skins::themes(ws) {
        themes.push(json!({ "key": key, "name": format!("{name} (skin)") }));
    }
    json!({ "themes": themes })
}

/// `theme.get`: one theme, fully resolved, in the theme file format.
pub fn get(ws: Option<&Workspace>, key: &str) -> Result<Value> {
    let file = if key.starts_with(skins::THEME_PREFIX) {
        skins::theme(ws, key)?
    } else {
        let theme = registry()
            .get_cloned(key)
            .ok_or_else(|| err(Code::NotFound, format!("no theme {key}")))?;
        serde_json::to_value(ThemeFile::from(theme))?
    };
    Ok(hex_colors(file))
}

/// The theme file with every `[r, g, b]` colour as `"#rrggbb"`, so a colour
/// reaches the client in one of two forms: a string (a hex colour, a
/// colour's name, or "Default") or `{color, …}` holding one.
fn hex_colors(v: Value) -> Value {
    match v {
        Value::Array(a) => match rgb(&a) {
            Some([r, g, b]) => Value::String(format!("#{r:02x}{g:02x}{b:02x}")),
            None => Value::Array(a.into_iter().map(hex_colors).collect()),
        },
        Value::Object(o) => Value::Object(o.into_iter().map(|(k, x)| (k, hex_colors(x))).collect()),
        v => v,
    }
}

/// Three numbers 0–255: a colour.
fn rgb(a: &[Value]) -> Option<[u8; 3]> {
    let [r, g, b] = a else { return None };
    let c = |x: &Value| x.as_u64().and_then(|n| u8::try_from(n).ok());
    Some([c(r)?, c(g)?, c(b)?])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn colours_arrive_as_strings() {
        let v = hex_colors(
            json!({"editor": {"bg": [16, 32, 255], "fg": "White", "sel": {"color": [0, 0, 1]}}, "list": [1, 2]}),
        );
        assert_eq!(
            v,
            json!({"editor": {"bg": "#1020ff", "fg": "White", "sel": {"color": "#000001"}}, "list": [1, 2]})
        );
    }
}
