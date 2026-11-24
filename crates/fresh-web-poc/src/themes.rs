//! The editor's themes, served to the client: the built-in themes the
//! editor's core embeds at build time, resolved by its own loader (so a
//! theme's inherited and defaulted keys arrive filled in), and the themes
//! skins bring (`skin/<id>`, `src/skins.rs`). The client maps each theme's
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
pub fn list(ws: &Workspace) -> Value {
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
pub fn get(ws: &Workspace, key: &str) -> Result<Value> {
    if key.starts_with(skins::THEME_PREFIX) {
        return skins::theme(ws, key);
    }
    let theme = registry()
        .get_cloned(key)
        .ok_or_else(|| err(Code::NotFound, format!("no theme {key}")))?;
    Ok(serde_json::to_value(ThemeFile::from(theme))?)
}
