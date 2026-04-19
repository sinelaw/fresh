//! Runtime config overlay (design M1).
//!
//! Plugins — in practice, `init.ts` — can write individual config settings
//! at runtime via `editor.setSetting(path, value)`. These writes live in a
//! per-plugin overlay that is merged on top of the disk-loaded base config
//! (User / Project / System layers).
//!
//! The overlay is purely in-memory. Unloading a plugin (hot-reload, revert,
//! or the crash fuse) drops every setting that plugin wrote. Nothing is
//! ever persisted to `config.json` — removing `~/.config/fresh/init.ts` is
//! a complete undo.

use serde_json::Value;
use std::collections::HashMap;

/// Maps plugin name → (dot-path → JSON value) for in-memory setting writes.
///
/// Outer key is a plugin name (including `"init.ts"`). Inner key is a
/// dot-separated path into the config tree (e.g. `"editor.tab_size"`).
#[derive(Debug, Default, Clone)]
pub struct RuntimeConfigOverlay {
    entries: HashMap<String, HashMap<String, Value>>,
}

impl RuntimeConfigOverlay {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a setting write. The most recent plugin-write wins when
    /// multiple plugins target the same path.
    pub fn set(&mut self, plugin: &str, path: String, value: Value) {
        self.entries
            .entry(plugin.to_string())
            .or_default()
            .insert(path, value);
    }

    /// Drop every entry written by `plugin`. Returns the number removed.
    pub fn clear_plugin(&mut self, plugin: &str) -> usize {
        self.entries.remove(plugin).map(|m| m.len()).unwrap_or(0)
    }

    /// Does any plugin currently have a write overlaying this path?
    pub fn is_empty(&self) -> bool {
        self.entries.values().all(|m| m.is_empty())
    }

    /// Flatten all overlay entries into a deterministic order. Later writes
    /// (by iteration order across plugins, then by path order within a
    /// plugin) override earlier ones when applied to JSON. For
    /// single-writer init.ts this is stable.
    fn entries_for_apply(&self) -> Vec<(&str, &String, &Value)> {
        let mut out = Vec::new();
        let mut plugin_names: Vec<&str> = self.entries.keys().map(String::as_str).collect();
        plugin_names.sort();
        for plugin in plugin_names {
            let map = &self.entries[plugin];
            let mut paths: Vec<&String> = map.keys().collect();
            paths.sort();
            for path in paths {
                out.push((plugin, path, &map[path]));
            }
        }
        out
    }

    /// Apply every overlay entry on top of a JSON representation of the base
    /// config. Paths that don't exist in the base are inserted.
    pub fn apply_to(&self, base: &mut Value) {
        for (_plugin, path, value) in self.entries_for_apply() {
            set_dot_path(base, path, value.clone());
        }
    }
}

/// Set the value at a dot-separated path inside a JSON object, creating
/// intermediate maps as needed. Overwrites any existing value (including
/// non-object values).
///
/// Non-object intermediate values are replaced with an empty object — the
/// assumption is the overlay represents a typed write whose path the user
/// meant; we'd rather realise their intent than preserve an incompatible
/// existing value. The editor re-validates the merged JSON against the
/// `Config` schema, so genuinely malformed writes surface as errors there.
pub fn set_dot_path(root: &mut Value, path: &str, value: Value) {
    let segments: Vec<&str> = path.split('.').filter(|s| !s.is_empty()).collect();
    if segments.is_empty() {
        return;
    }
    let mut cur = root;
    for seg in &segments[..segments.len() - 1] {
        if !cur.is_object() {
            *cur = Value::Object(serde_json::Map::new());
        }
        let obj = cur.as_object_mut().expect("just made it an object");
        cur = obj.entry((*seg).to_string()).or_insert(Value::Null);
    }
    let last = segments[segments.len() - 1];
    if !cur.is_object() {
        *cur = Value::Object(serde_json::Map::new());
    }
    cur.as_object_mut()
        .expect("just made it an object")
        .insert(last.to_string(), value);
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn set_and_apply_a_single_path() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("init.ts", "editor.tab_size".into(), json!(2));

        let mut base = json!({ "editor": { "tab_size": 4 } });
        overlay.apply_to(&mut base);

        assert_eq!(base, json!({ "editor": { "tab_size": 2 } }));
    }

    #[test]
    fn set_creates_missing_intermediate_objects() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("init.ts", "terminal.mouse".into(), json!(false));

        let mut base = json!({ "editor": { "tab_size": 4 } });
        overlay.apply_to(&mut base);

        assert_eq!(base["terminal"]["mouse"], json!(false));
        assert_eq!(base["editor"]["tab_size"], json!(4));
    }

    #[test]
    fn clear_plugin_drops_only_that_plugin_writes() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("init.ts", "editor.tab_size".into(), json!(2));
        overlay.set("other_plugin", "editor.line_wrap".into(), json!(true));

        let removed = overlay.clear_plugin("init.ts");
        assert_eq!(removed, 1);

        let mut base = json!({ "editor": { "tab_size": 4, "line_wrap": false } });
        overlay.apply_to(&mut base);

        assert_eq!(base["editor"]["tab_size"], json!(4)); // reverted to base
        assert_eq!(base["editor"]["line_wrap"], json!(true)); // other plugin still wins
    }

    #[test]
    fn last_write_wins_across_plugins() {
        // Deterministic iteration order: plugin names sorted alphabetically,
        // so "zzz" comes after "aaa" and wins.
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("aaa", "x".into(), json!(1));
        overlay.set("zzz", "x".into(), json!(9));

        let mut base = json!({});
        overlay.apply_to(&mut base);
        assert_eq!(base["x"], json!(9));
    }

    #[test]
    fn is_empty_after_clear() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("init.ts", "a.b".into(), json!(1));
        overlay.clear_plugin("init.ts");
        assert!(overlay.is_empty());
    }

    #[test]
    fn set_dot_path_overwrites_non_object_intermediates() {
        // base.editor is a scalar — setting a sub-path should replace it.
        let mut v = json!({ "editor": 42 });
        set_dot_path(&mut v, "editor.tab_size", json!(2));
        assert_eq!(v, json!({ "editor": { "tab_size": 2 } }));
    }
}
