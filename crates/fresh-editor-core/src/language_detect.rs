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
///
/// `fs` is the filesystem that actually owns `path` — the buffer's own
/// filesystem, not the process's. Only the `.h` → `cpp` promotion below
/// touches it; every other rule is pure path/config matching. It is a
/// required argument rather than an `Option` precisely because the bug it
/// fixes was a probe that quietly answered "no" against the wrong
/// filesystem: on a remote session there is no correct behaviour for a
/// caller that cannot say which host the file lives on.
pub fn detect_language(
    path: &std::path::Path,
    languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
    fs: &dyn crate::model::filesystem::FileSystem,
) -> Option<String> {
    let detected = detect_language_by_config(path, languages);

    // `.h` headers: the default config maps the extension to C, but in C++
    // projects the header is still C++ and must route to clangd in C++ mode.
    // If the detected language is `c`, the file is `.h`, and the surrounding
    // tree smells like C++ (sibling C++ sources or an ancestor
    // `compile_commands.json`), promote to `cpp` so the LSP binding is right.
    //
    // The cheap path/config predicates are deliberately ordered before
    // `header_in_cpp_tree`, so the only I/O in this function is skipped
    // entirely for every file that is not a `.h` resolving to `c` in a
    // config that knows `cpp`.
    if detected.as_deref() == Some("c")
        && path.extension().and_then(|e| e.to_str()) == Some("h")
        && languages.contains_key("cpp")
        && header_in_cpp_tree(path, fs)
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

/// The build database a C++ tree is recognised by.
const COMPILE_COMMANDS: &str = "compile_commands.json";

/// Directories the `compile_commands.json` search may examine, counting the
/// header's own: deep enough for the fmt / Chromium / LLVM / Qt layouts where
/// a header sits several levels under `include/` while the build DB sits at
/// the project root.
const MAX_ANCESTOR_DIRS: usize = 11;

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
///     project root. The climb goes through [`FileSystem::find_up`], so a
///     remote host answers it in one request rather than a round trip per
///     level — local and remote run the same logic.
///
/// All access goes through `fs` — the filesystem that owns the header —
/// so the probe answers about the host the file actually lives on. Reading
/// the process-local disk here made the promotion a silent no-op on every
/// SSH session: a `.h` in a remote C++ tree found no siblings, fell back to
/// `c`, and highlighted as C.
///
/// Silent on any I/O error — if we can't see the filesystem we fall back
/// to the default config answer (C), which is the pre-fix behavior.
///
/// NOTE(remote-fs): `detect_workspace_root` in `fresh-editor`'s
/// `services::lsp::manager` still uses
/// `std::fs` via `Path::exists` and has the same remote blind spot (an
/// SSH workspace root resolves to the file's own directory instead of the
/// project root). It is *not* fixed here: its three call sites sit inside
/// LSP server spawn/initialize, `LspManager` holds no filesystem, and
/// `resolve_root_uri` deliberately walks *host* paths before applying
/// `path_translation` for devcontainers — so "which filesystem" is a real
/// design question there, not a mechanical substitution. (It is now a
/// smaller one: `find_up` is exactly the primitive it needs.)
fn header_in_cpp_tree(
    path: &std::path::Path,
    fs: &dyn crate::model::filesystem::FileSystem,
) -> bool {
    let Some(start_dir) = path.parent() else {
        return false;
    };
    // 1. Sibling scan in the header's own directory: one shallow,
    //    non-recursive listing, and the decisive signal.
    if let Ok(entries) = fs.read_dir(start_dir) {
        for entry in &entries {
            let Some(ext) = entry.path.extension().and_then(|e| e.to_str()) else {
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

    // 2. Walk up looking for compile_commands.json, and only promote if the
    //    file actually carries a C++ marker — CMake emits it for pure-C
    //    builds too, so an outer build DB can still be the answer when the
    //    nearest one does not qualify. `find_up` answers the whole climb in
    //    one call, which a remote filesystem serves with a single request
    //    rather than a round trip per level.
    let Ok(candidates) = fs.find_up(start_dir, &[COMPILE_COMMANDS], Some(MAX_ANCESTOR_DIRS)) else {
        return false;
    };
    candidates
        .iter()
        .any(|dir| compile_commands_has_cpp_marker(&dir.join(COMPILE_COMMANDS), fs))
}

/// Returns true when `compile_commands.json` exists at `path` and contains
/// a C++ marker — either the literal substring `c++` (covers `-std=c++17`,
/// `clang++`, `g++`, the `c++` compiler name) or a C++ source extension in
/// a context where it cannot be confused with an adjacent header path
/// (`.cpp`, `.cc`, `.cxx`).
///
/// Existence and size come from a single `metadata_if_exists`, which is one
/// filesystem op (one round trip on a remote host) and subsumes the former
/// separate `is_file` check. The read is then clamped to
/// `min(size, 1 MiB)`: the cap keeps multi-megabyte compile DBs from large
/// monorepos off the file-open path, and clamping to the real size is
/// required because `FileSystem::read_range` is `read_exact`-shaped and
/// fails outright on a short file. A directory or unreadable path simply
/// fails the read and answers `false`.
fn compile_commands_has_cpp_marker(
    path: &std::path::Path,
    fs: &dyn crate::model::filesystem::FileSystem,
) -> bool {
    const MAX_READ: u64 = 1_048_576;

    let Some(meta) = fs.metadata_if_exists(path) else {
        return false;
    };
    let len = meta.size.min(MAX_READ) as usize;
    if len == 0 {
        return false;
    }
    let Ok(buf) = fs.read_range(path, 0, len) else {
        return false;
    };
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
