//! Runtime config overlay (design M1).
//!
//! `editor.setSetting(path, value)` writes land here — a flat map of
//! dot-path → JSON value, merged on top of the disk-loaded base config.
//!
//! The overlay is purely in-memory. Clearing it (on init.ts reload /
//! revert) restores the base. Nothing is ever persisted to `config.json`
//! — removing `~/.config/fresh/init.ts` is a complete undo.
//!
//! No per-plugin tracking: in practice only init.ts calls setSetting.
//! Reload = clear all + re-run. If multi-writer support is ever needed,
//! upgrade to a per-plugin or sequenced structure; the external API
//! (`setSetting`) doesn't change.

use serde_json::Value;
use std::collections::HashMap;

/// Flat path → value map. Last call to `set` for a given path wins.
#[derive(Debug, Default, Clone)]
pub struct RuntimeConfigOverlay {
    writes: HashMap<String, Value>,
}

impl RuntimeConfigOverlay {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record (or overwrite) a setting. Last write to a path wins.
    pub fn set(&mut self, path: String, value: Value) {
        self.writes.insert(path, value);
    }

    /// Drop every entry. Returns the number removed.
    pub fn clear(&mut self) -> usize {
        let n = self.writes.len();
        self.writes.clear();
        n
    }

    pub fn is_empty(&self) -> bool {
        self.writes.is_empty()
    }

    /// Apply every entry on top of a JSON base config.
    pub fn apply_to(&self, base: &mut Value) {
        for (path, value) in &self.writes {
            set_dot_path(base, path, value.clone());
        }
    }
}

/// Set the value at a dot-separated path inside a JSON object, creating
/// intermediate maps as needed.
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
        overlay.set("editor.tab_size".into(), json!(2));

        let mut base = json!({ "editor": { "tab_size": 4 } });
        overlay.apply_to(&mut base);

        assert_eq!(base, json!({ "editor": { "tab_size": 2 } }));
    }

    #[test]
    fn set_creates_missing_intermediate_objects() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("terminal.mouse".into(), json!(false));

        let mut base = json!({ "editor": { "tab_size": 4 } });
        overlay.apply_to(&mut base);

        assert_eq!(base["terminal"]["mouse"], json!(false));
        assert_eq!(base["editor"]["tab_size"], json!(4));
    }

    #[test]
    fn last_write_wins() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("x".into(), json!(1));
        overlay.set("x".into(), json!(9));

        let mut base = json!({});
        overlay.apply_to(&mut base);
        assert_eq!(base["x"], json!(9));
    }

    #[test]
    fn clear_drops_all() {
        let mut overlay = RuntimeConfigOverlay::new();
        overlay.set("a.b".into(), json!(1));
        overlay.set("c.d".into(), json!(2));
        assert_eq!(overlay.clear(), 2);
        assert!(overlay.is_empty());

        let mut base = json!({ "a": { "b": 0 }, "c": { "d": 0 } });
        overlay.apply_to(&mut base);
        assert_eq!(base["a"]["b"], json!(0));
        assert_eq!(base["c"]["d"], json!(0));
    }

    #[test]
    fn set_dot_path_overwrites_non_object_intermediates() {
        let mut v = json!({ "editor": 42 });
        set_dot_path(&mut v, "editor.tab_size", json!(2));
        assert_eq!(v, json!({ "editor": { "tab_size": 2 } }));
    }
}
