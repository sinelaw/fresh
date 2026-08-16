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

/// How much filesystem I/O the `.h` tree probe may spend, decided by the
/// filesystem that owns the file rather than by a flag a caller could get
/// wrong.
///
/// This exists because the two backends have costs that differ by four
/// orders of magnitude for the *same* trait call. `StdFileSystem`'s
/// `read_dir` / `metadata` are microsecond syscalls. `RemoteFileSystem`
/// implements the whole sync `FileSystem` surface with
/// `AgentChannel::request_blocking` — one blocking SSH round trip each,
/// on the editor thread. Language detection runs on the file-open path, so
/// the unbudgeted probe (one `ls` plus up to eleven `stat`s plus a read)
/// would be up to thirteen serialized round trips before a remote `.h`
/// could be displayed, and the editor loop is single-threaded: a stalled
/// link would freeze the UI for the request timeout on each one. (The same
/// hazard is called out in `services/remote/filesystem.rs`, where the
/// `$HOME`/temp-dir lookups are cached specifically to keep blocking
/// requests off the editor thread.)
///
/// So the remote budget buys only the decisive signal — the single sibling
/// listing, which is one round trip and covers the ordinary
/// `widget.h`/`widget.cpp` layout that motivated #3009 — and declines the
/// ancestor walk. A remote header under an `include/` tree with sources
/// elsewhere therefore still reads as C: strictly better than today's
/// unconditional no-op, and it does not trade a highlighting nicety for a
/// frozen editor. Carrying the depth as data (rather than an
/// `if is_remote` inside the loop) keeps "walk ten ancestors over SSH"
/// unrepresentable instead of merely unreached.
#[derive(Clone, Copy)]
struct ProbeBudget {
    /// How many directories the `compile_commands.json` walk may visit,
    /// counting from the header's own directory upward. `0` disables the
    /// walk outright, so no request is issued at all.
    compile_commands_dirs: u32,
}

impl ProbeBudget {
    /// Directories visited on a local filesystem: the header's own plus
    /// ten ancestors — deep enough for the fmt / Chromium / LLVM / Qt
    /// layouts where the header sits several levels under `include/`.
    const LOCAL_COMPILE_COMMANDS_DIRS: u32 = 11;

    fn for_filesystem(fs: &dyn crate::model::filesystem::FileSystem) -> Self {
        // `remote_connection_info` is the trait's own locality signal:
        // `Some("user@host")` exactly for filesystems whose sync methods
        // are blocking round trips.
        let compile_commands_dirs = if fs.remote_connection_info().is_some() {
            0
        } else {
            Self::LOCAL_COMPILE_COMMANDS_DIRS
        };
        Self {
            compile_commands_dirs,
        }
    }
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
///     project root. This second signal is budgeted away on a remote
///     filesystem, where each level would be a blocking round trip —
///     see [`ProbeBudget`].
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
/// design question there, not a mechanical substitution.
fn header_in_cpp_tree(
    path: &std::path::Path,
    fs: &dyn crate::model::filesystem::FileSystem,
) -> bool {
    let Some(start_dir) = path.parent() else {
        return false;
    };
    let budget = ProbeBudget::for_filesystem(fs);

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

    // 2. Walk up looking for compile_commands.json, and only promote if
    //    the file actually carries a C++ marker — CMake emits it for
    //    pure-C builds too. `budget.compile_commands_dirs` is 0 on a
    //    remote filesystem, which skips this loop entirely without ever
    //    issuing a request (see `ProbeBudget`).
    let mut current = Some(start_dir);
    let mut visited = 0u32;
    while let Some(dir) = current {
        if visited >= budget.compile_commands_dirs {
            break;
        }
        if compile_commands_has_cpp_marker(&dir.join("compile_commands.json"), fs) {
            return true;
        }
        visited += 1;
        current = dir.parent();
    }

    false
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
