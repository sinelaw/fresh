//! Write out the JavaScript the plugin runtime actually executes for each
//! bundled plugin, so it can be run on another JS engine.
//!
//! Used by the Debian QuickJS spike (`docs/internal/debian-quickjs-spike.md`):
//!
//! ```sh
//! cargo run -p fresh-parser-js --example dump_plugins_js -- \
//!     crates/fresh-editor/plugins \
//!     crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs \
//!     /tmp/plugins-js
//! ```
//!
//! Each `<plugin>.ts` becomes `<plugin>.js`, transpiled and bundled the same
//! way `fresh-plugin-runtime`'s loader does it, and `__bootstrap.js` holds the
//! JS the runtime evaluates into every plugin context before the plugin.

use std::fs;
use std::path::{Path, PathBuf};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let [plugins_dir, backend_rs, out_dir] = args.as_slice() else {
        anyhow::bail!("usage: dump_plugins_js <plugins-dir> <quickjs_backend.rs> <out-dir>");
    };
    let out_dir = PathBuf::from(out_dir);
    fs::create_dir_all(&out_dir)?;

    let backend = fs::read_to_string(backend_rs)?;
    let bootstrap = [
        "EDITOR_GLOBALS_BOOTSTRAP",
        "EDITOR_ON_OFF_SHIM",
        "EDITOR_PROMISE_BOOTSTRAP",
    ]
    .iter()
    .map(|name| extract_str_const(&backend, name))
    .collect::<anyhow::Result<Vec<_>>>()?
    .join("\n;\n");
    fs::write(out_dir.join("__bootstrap.js"), bootstrap)?;

    let mut entries: Vec<PathBuf> = fs::read_dir(plugins_dir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| {
            let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
            name.ends_with(".ts") && !name.ends_with(".d.ts")
        })
        .collect();
    entries.sort();

    let mut failed = 0;
    for path in &entries {
        let stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("plugin");
        match to_js(path) {
            Ok(js) => fs::write(out_dir.join(format!("{stem}.js")), js)?,
            Err(e) => {
                failed += 1;
                eprintln!("{stem}: {e}");
            }
        }
    }
    eprintln!(
        "wrote {} plugins to {} ({failed} failed)",
        entries.len() - failed,
        out_dir.display()
    );
    Ok(())
}

/// Same decision as `fresh_plugin_runtime::thread`'s plugin loader.
fn to_js(path: &Path) -> anyhow::Result<String> {
    let source = fs::read_to_string(path)?;
    let filename = path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("plugin.ts");
    Ok(if fresh_parser_js::has_es_imports(&source) {
        fresh_parser_js::bundle_module(path)?
    } else if fresh_parser_js::has_es_module_syntax(&source) {
        let stripped = fresh_parser_js::strip_imports_and_exports(&source);
        fresh_parser_js::transpile_typescript(&stripped, filename)?
    } else {
        fresh_parser_js::transpile_typescript(&source, filename)?
    })
}

/// The value of `const NAME: &str = ...;` — a raw `r#"..."#` literal, or a
/// plain literal whose only escapes are `\n` and `\"`.
fn extract_str_const(src: &str, name: &str) -> anyhow::Result<String> {
    let decl = format!("const {name}: &str = ");
    let start = src
        .find(&decl)
        .ok_or_else(|| anyhow::anyhow!("{name} not found"))?
        + decl.len();
    let rest = &src[start..];
    if let Some(body) = rest.strip_prefix("r#\"") {
        let end = body
            .find("\"#;")
            .ok_or_else(|| anyhow::anyhow!("{name}: unterminated"))?;
        Ok(body[..end].to_string())
    } else if let Some(body) = rest.strip_prefix('"') {
        let end = body
            .find("\";")
            .ok_or_else(|| anyhow::anyhow!("{name}: unterminated"))?;
        Ok(body[..end].replace("\\n", "\n").replace("\\\"", "\""))
    } else {
        anyhow::bail!("{name}: unsupported literal")
    }
}
