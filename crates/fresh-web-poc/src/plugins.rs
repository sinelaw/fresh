//! The client's plugins, compiled once at startup.
//!
//! Plugins run in the client. They are the editor's own bundled TypeScript
//! plugins, unchanged, compiled to JavaScript by the same compiler the editor
//! uses (`fresh_parser_js::compile_plugin`, which also bundles their
//! `./lib/...` imports). Shipping them is part of shipping the client, like
//! its HTML and scripts.

use serde_json::{json, Value};
use std::path::{Path, PathBuf};

/// The plugins the proof of concept runs, by path under the plugins folder.
/// Shipped plugins only: the editor's build compiles `plugins/*.ts`, not
/// `examples/`. UI-heavy plugins written against the terminal's widget model
/// are reimplemented natively in the client instead (see PLAN.md).
pub const PLUGINS: &[&str] = &[
    "markdown_compose.ts",
    "git_statusbar.ts",
    "path_complete.ts",
    "goto_with_selection.ts",
    "welcome.ts",
];

pub fn plugins_dir() -> PathBuf {
    if let Some(d) = std::env::var_os("FRESH_POC_PLUGINS") {
        return PathBuf::from(d);
    }
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../fresh-editor/plugins")
}

/// `[{name, code, i18n}]`, or `{name, error}` for one that did not compile.
pub fn compile_all() -> Value {
    let dir = plugins_dir();
    let mut out = Vec::new();
    for rel in PLUGINS {
        let path = dir.join(rel);
        let name = Path::new(rel)
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_default();
        let source = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                out.push(json!({"name": name, "error": e.to_string()}));
                continue;
            }
        };
        let i18n = std::fs::read_to_string(path.with_extension("i18n.json"))
            .ok()
            .and_then(|s| serde_json::from_str::<Value>(&s).ok())
            .unwrap_or(Value::Null);
        match fresh_parser_js::compile_plugin(&path, &source) {
            Ok(code) => out.push(json!({"name": name, "code": code, "i18n": i18n})),
            Err(e) => out.push(json!({"name": name, "error": e.to_string()})),
        }
    }
    Value::Array(out)
}
