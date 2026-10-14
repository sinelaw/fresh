//! Plugins compiled to JavaScript when Fresh was built.
//!
//! A build can compile its bundled plugins ahead of time (fresh-editor's
//! `precompiled-plugins` feature runs [`crate::compile_plugin`] from its build
//! script) and register the results here, so loading those plugins needs no
//! TypeScript compiler at run time. For a Debian build that means no `esbuild`
//! unless the user writes TypeScript of their own.
//!
//! An entry applies to a plugin only when it is loaded from the registered
//! directory under the same file name, and its source still hashes to what was
//! compiled. Anything else, an edited file included, is compiled as usual.

use std::path::{Path, PathBuf};
use std::sync::OnceLock;

/// One precompiled plugin: its file name, the [`source_hash`] of the source it
/// was compiled from, and the JavaScript.
pub type Entry = (&'static str, u64, &'static str);

struct Registry {
    dir: PathBuf,
    entries: &'static [Entry],
}

static REGISTRY: OnceLock<Registry> = OnceLock::new();

/// 64-bit FNV-1a of `source`: stable across builds and platforms, so a build
/// script and the runtime agree on it.
pub fn source_hash(source: &str) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in source.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

/// Register the precompiled plugins that live in `dir`. Only the first call
/// takes effect.
pub fn register(dir: PathBuf, entries: &'static [Entry]) {
    let dir = dir.canonicalize().unwrap_or(dir);
    let _ = REGISTRY.set(Registry { dir, entries });
}

/// The precompiled JavaScript for the plugin at `path` with contents `source`,
/// if one was registered for it.
pub fn lookup(path: &Path, source: &str) -> Option<&'static str> {
    let registry = REGISTRY.get()?;
    let parent = path.parent()?;
    let parent = parent
        .canonicalize()
        .unwrap_or_else(|_| parent.to_path_buf());
    if parent != registry.dir {
        return None;
    }
    let name = path.file_name()?.to_str()?;
    let hash = source_hash(source);
    registry
        .entries
        .iter()
        .find(|(file, h, _)| *file == name && *h == hash)
        .map(|(_, _, js)| *js)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_hash_is_fnv1a() {
        // Reference values for 64-bit FNV-1a.
        assert_eq!(source_hash(""), 0xcbf2_9ce4_8422_2325);
        assert_eq!(source_hash("a"), 0xaf63_dc4c_8601_ec8c);
    }
}
