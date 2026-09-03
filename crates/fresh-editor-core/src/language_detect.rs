//! Mapping a file path onto a configured language key.
//!
//! Shared by LSP routing (`services::lsp::manager`, up in `fresh-editor`) and
//! by `primitives::detected_language`, so the rules live down here where both
//! can reach them.

/// Helper function to detect language from file path using the config's languages section.
///
/// Priority order matches `GrammarRegistry::find_by_path`:
/// 1. Exact filename match against `filenames` (highest priority)
/// 2. Glob pattern match against `filenames` entries containing wildcards
/// 3. File extension match against `extensions` (lowest config-based priority)
///
/// Kept separate from `find_by_path` because this returns the user's
/// config **key** (`[languages.mylang]` → `"mylang"`) rather than the
/// catalog entry's `language_id`, which is needed for LSP routing when a
/// user aliases an existing grammar.
pub fn detect_language(
    path: &std::path::Path,
    languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
) -> Option<String> {
    let detected = detect_language_by_config(path, languages);

    // `.h` headers: the default config maps the extension to C, but in C++
    // projects the header is still C++ and must route to clangd in C++ mode.
    // If the detected language is `c`, the file is `.h`, and the surrounding
    // tree smells like C++ (sibling C++ sources or an ancestor
    // `compile_commands.json`), promote to `cpp` so the LSP binding is right.
    if detected.as_deref() == Some("c")
        && path.extension().and_then(|e| e.to_str()) == Some("h")
        && languages.contains_key("cpp")
        && header_in_cpp_tree(path)
    {
        return Some("cpp".to_string());
    }

    detected
}

/// Pure config/path-based language detection without filesystem probing.
fn detect_language_by_config(
    path: &std::path::Path,
    languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
) -> Option<String> {
    use crate::primitives::glob_match::{glob_entry_matches, literal_entry_matches};

    if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
        // 1. Exact filename match (highest priority)
        for (language_name, lang_config) in languages {
            if lang_config
                .filenames
                .iter()
                .any(|f| literal_entry_matches(f, filename))
            {
                return Some(language_name.clone());
            }
        }

        // 2. Glob pattern match
        let path_str = path.to_str().unwrap_or("");
        for (language_name, lang_config) in languages {
            if lang_config
                .filenames
                .iter()
                .any(|f| glob_entry_matches(f, path_str, filename))
            {
                return Some(language_name.clone());
            }
        }
    }

    // 3. Extension match (lowest priority among config-based detection)
    if let Some(extension) = path.extension().and_then(|e| e.to_str()) {
        for (language_name, lang_config) in languages {
            if lang_config.extensions.iter().any(|ext| ext == extension) {
                return Some(language_name.clone());
            }
        }
    }

    None
}

/// Filesystem probe: does this header sit inside something that looks like
/// a C++ project? Two signals, both conservative:
///
///   * The file's own directory contains any C++ source or C++-specific
///     header (`.cc`, `.cpp`, `.cxx`, `.C`, `.c++`, `.hpp`, `.hh`, `.hxx`).
///     Decisive — if the siblings are C++, the header is too.
///   * An ancestor up to 10 levels deep contains a `compile_commands.json`
///     whose content carries a C++ marker. The mere presence of the file
///     is not enough: CMake emits `compile_commands.json` for pure-C
///     builds as well, so we peek inside and only promote when the
///     payload mentions a C++-specific compiler, flag, or source
///     extension (`c++`, `.cpp`, `.cc`, `.cxx`, `.C` ). This still covers
///     the fmt / Chromium / LLVM / Qt-style layouts where the header
///     lives deep under `include/` while sources sit in `src/` at the
///     project root.
///
/// Bounded by depth (10), by a single shallow `read_dir` at the start,
/// and by a capped 1 MiB read of `compile_commands.json`, so the cost is
/// a handful of `stat`s plus at most one bounded read on file open.
/// Silent on any I/O error — if we can't see the filesystem we fall back
/// to the default config answer (C), which is the pre-fix behavior.
///
/// NOTE(remote-fs): Uses `std::fs` directly, matching the pre-existing
/// `detect_workspace_root` in this module. On SSH sessions the probe
/// sees the local filesystem, so the promotion silently becomes a no-op
/// (returns `false`, falls back to `c`). Fixing this requires threading
/// `&dyn FileSystem` through `detect_language` and
/// `DetectedLanguage::from_path` — a cross-cutting refactor that should
/// be done alongside the same fix for `detect_workspace_root`.
fn header_in_cpp_tree(path: &std::path::Path) -> bool {
    let Some(start_dir) = path.parent() else {
        return false;
    };

    // 1. Sibling scan in the header's own directory.
    if let Ok(entries) = std::fs::read_dir(start_dir) {
        for entry in entries.flatten() {
            let p = entry.path();
            let Some(ext) = p.extension().and_then(|e| e.to_str()) else {
                continue;
            };
            if matches!(
                ext,
                "cc" | "cpp" | "cxx" | "C" | "c++" | "hpp" | "hh" | "hxx"
            ) {
                return true;
            }
        }
    }

    // 2. Walk ancestors for compile_commands.json, and only promote if
    //    the file actually carries a C++ marker — CMake emits it for
    //    pure-C builds too.
    let mut current = Some(start_dir);
    let mut depth = 0u32;
    while let Some(dir) = current {
        let cc = dir.join("compile_commands.json");
        if cc.is_file() && compile_commands_has_cpp_marker(&cc) {
            return true;
        }
        if depth >= 10 {
            break;
        }
        depth += 1;
        current = dir.parent();
    }

    false
}

/// Returns true when `compile_commands.json` contains a C++ marker —
/// either the literal substring `c++` (covers `-std=c++17`, `clang++`,
/// `g++`, the `c++` compiler name) or a C++ source extension in a
/// context where it cannot be confused with an adjacent header path
/// (`.cpp`, `.cc`, `.cxx`). Reads at most 1 MiB so multi-megabyte
/// compile DBs from large monorepos don't block file open; a valid CMake
/// entry fits comfortably in that window.
fn compile_commands_has_cpp_marker(path: &std::path::Path) -> bool {
    use std::io::Read;
    const MAX_READ: u64 = 1_048_576;

    let Ok(file) = std::fs::File::open(path) else {
        return false;
    };
    let mut buf = Vec::with_capacity(64 * 1024);
    if file.take(MAX_READ).read_to_end(&mut buf).is_err() {
        return false;
    }
    let Ok(text) = std::str::from_utf8(&buf) else {
        return false;
    };

    // Strongest single marker: literal "c++" appears in -std=c++NN,
    // clang++, g++, and the "c++" compiler name — never in a pure-C
    // compilation invocation.
    if text.contains("c++") {
        return true;
    }
    // Secondary markers: any mention of a C++ source extension in the
    // compile DB implies at least one C++ translation unit in the tree.
    text.contains(".cpp") || text.contains(".cxx") || text.contains(".cc\"")
}
