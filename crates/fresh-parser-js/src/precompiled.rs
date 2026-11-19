//! Plugins compiled to JavaScript when Fresh was built.
//!
//! A build can compile its bundled plugins ahead of time (fresh-editor's
//! `precompiled-plugins` feature runs [`crate::compile_plugin`] from its build
//! script) and register the results here, so loading those plugins needs no
//! TypeScript compiler at run time. For a Debian build that means no `esbuild`
//! unless the user writes TypeScript of their own. Such a build embeds no
//! TypeScript for these plugins: it extracts each one's JavaScript as
//! [`Entry::file`] into the registered directory, and the loader finds it there.
//!
//! An entry applies to a plugin only when it is loaded from the registered
//! directory under the same file name, and its contents are still the entry's
//! JavaScript. Anything else, an edited file included, is compiled as usual.

use std::path::{Path, PathBuf};
use std::sync::OnceLock;

/// One precompiled plugin.
#[derive(Debug)]
pub struct Entry {
    /// File name of the JavaScript in the registered directory (`name.js`).
    pub file: &'static str,
    pub js: &'static str,
    /// What [`crate::extract_plugin_dependencies`] found in the source.
    pub dependencies: &'static [&'static str],
    /// The source's [`crate::emit_isolated_declarations`], when the build
    /// could emit them.
    pub declarations: Option<&'static str>,
}

struct Registry {
    dir: PathBuf,
    entries: &'static [Entry],
}

static REGISTRY: OnceLock<Registry> = OnceLock::new();

/// Register the precompiled plugins that live in `dir`. Only the first call
/// takes effect.
pub fn register(dir: PathBuf, entries: &'static [Entry]) {
    let dir = dir.canonicalize().unwrap_or(dir);
    let _ = REGISTRY.set(Registry { dir, entries });
}

/// The precompiled plugin at `path` with contents `source`, if one was
/// registered for it.
pub fn lookup(path: &Path, source: &str) -> Option<&'static Entry> {
    let registry = REGISTRY.get()?;
    let parent = path.parent()?;
    let parent = parent
        .canonicalize()
        .unwrap_or_else(|_| parent.to_path_buf());
    if parent != registry.dir {
        return None;
    }
    let name = path.file_name()?.to_str()?;
    registry
        .entries
        .iter()
        .find(|e| e.file == name && e.js == source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_matches_dir_name_and_contents() {
        static ENTRIES: &[Entry] = &[Entry {
            file: "p.js",
            js: "run();",
            dependencies: &["dep"],
            declarations: None,
        }];
        let dir = std::env::temp_dir().join(format!("fresh-precompiled-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        register(dir.clone(), ENTRIES);

        let found = lookup(&dir.join("p.js"), "run();").expect("registered plugin");
        assert_eq!(found.dependencies, ["dep"]);
        // Edited, renamed, or elsewhere: compiled as usual.
        assert!(lookup(&dir.join("p.js"), "run(); edited();").is_none());
        assert!(lookup(&dir.join("q.js"), "run();").is_none());
        assert!(lookup(&std::env::temp_dir().join("p.js"), "run();").is_none());
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
