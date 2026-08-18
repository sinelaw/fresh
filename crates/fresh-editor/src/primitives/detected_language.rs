//! Unified language detection for editor buffers.
//!
//! This module provides `DetectedLanguage`, the single source of truth for
//! determining a buffer's language, syntax highlighter, and tree-sitter support.
//! All code paths that set or change a buffer's language should go through this module.

use crate::config::LanguageConfig;
use crate::primitives::grammar::GrammarEntry;
use crate::primitives::highlight_engine::HighlightEngine;
use crate::primitives::highlighter::Language;
use crate::primitives::GrammarRegistry;
use std::collections::HashMap;
use std::path::Path;

/// The result of language detection — groups the things that must stay in sync
/// on an `EditorState`: the language ID, display name, highlighting engine, and
/// tree-sitter `Language` (used for reference highlighting, indentation, etc.).
pub struct DetectedLanguage {
    /// The canonical language ID for LSP and config lookup (e.g., "csharp", "rust", "text").
    pub name: String,
    /// Human-readable display name shown in the status bar and Set Language prompt
    /// (e.g., "C#", "Rust", "Plain Text"). Matches the syntect syntax name where available.
    pub display_name: String,
    /// The highlighting engine to use for this buffer.
    pub highlighter: HighlightEngine,
    /// The tree-sitter Language, if available (used for reference highlighting,
    /// auto-indent, bracket matching, etc.). Only ~18 languages have tree-sitter
    /// support; this is `None` for the remaining 100+ syntect-only languages.
    pub ts_language: Option<Language>,
}

impl DetectedLanguage {
    /// Build a `DetectedLanguage` from a unified catalog entry.
    ///
    /// The single place that glues a `GrammarEntry` to a `HighlightEngine`.
    /// All path-based and name-based constructors funnel through this.
    pub fn from_entry(entry: &GrammarEntry, registry: &GrammarRegistry) -> Self {
        Self {
            name: entry.language_id.clone(),
            display_name: entry.display_name.clone(),
            highlighter: HighlightEngine::from_entry(entry, registry),
            ts_language: entry.engines.tree_sitter,
        }
    }

    /// Detect language from a file path using user configuration.
    ///
    /// This is the primary detection path used when opening, reloading, or saving files.
    /// Priority order matches the grammar registry:
    /// 1. Exact filename match in user config
    /// 2. Glob pattern match in user config
    /// 3. Extension match in user config
    /// 4. Built-in detection (catalog lookup)
    /// 5. Shebang / first-line regex against `first_line` (catalog lookup)
    /// 6. Fallback config (if set and no other match found)
    ///
    /// `first_line` is the literal first line of the file (including any
    /// trailing newline). The caller — which has already loaded the buffer
    /// via the `FileSystem` trait — supplies it so the registry never does
    /// its own I/O. Pass `None` when there is no content to inspect (e.g.,
    /// virtual buffers, unsaved files).
    ///
    /// `fs` is the filesystem that owns `path`, normally
    /// `buffer.filesystem()`. Detection is *not* purely a function of the
    /// path: `detect_language` probes the surrounding tree to decide whether
    /// a `.h` is a C or a C++ header (#3009). Passing the process-local
    /// filesystem for a file that lives on an SSH host makes that probe
    /// answer about the wrong machine, so the filesystem is threaded in
    /// rather than assumed.
    pub fn from_path(
        path: &Path,
        first_line: Option<&str>,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
        fs: &dyn crate::model::filesystem::FileSystem,
    ) -> Self {
        Self::from_path_with_fallback(path, first_line, registry, languages, None, fs)
    }

    /// Like `from_path`, but also accepts an optional default language name
    /// that is applied when no language is detected (#1219).
    /// The `default_language` must reference a key in the `languages` map.
    pub fn from_path_with_fallback(
        path: &Path,
        first_line: Option<&str>,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
        default_language: Option<&str>,
        fs: &dyn crate::model::filesystem::FileSystem,
    ) -> Self {
        // Resolve the config/LSP language id *independently* of the grammar
        // catalog. A file matching a `[languages.foo]` rule must end up with
        // `name = "foo"` so comment prefix / tab config / LSP routing all
        // work — even when the grammar registry is empty (common in tests)
        // or has no matching entry.
        let config_lang_id = crate::services::lsp::manager::detect_language(path, languages, fs);
        let align = |d: Self| -> Self {
            Self::align_with_config_id(d, &config_lang_id, registry, languages)
        };

        if let Some(entry) = registry.find_by_path(path, first_line) {
            return align(Self::from_entry(entry, registry));
        }

        // No grammar match — try the user-configured default language for
        // highlighting, and fall back to plain text. Either way, keep any
        // config-derived language id.
        if let Some(lang_key) = default_language {
            let grammar = languages
                .get(lang_key)
                .map(|lc| lc.grammar.as_str())
                .filter(|g| !g.is_empty())
                .unwrap_or(lang_key);
            if let Some(entry) = registry.find_by_name(grammar) {
                return align(Self::from_entry(entry, registry));
            }
        }

        align(Self::plain_text())
    }

    /// Make the highlighter, tree-sitter grammar and display name agree with
    /// the language id detection resolved.
    ///
    /// Detection has two halves that historically answered independently:
    /// `detect_language` resolves the config/LSP id from `[languages.*]`
    /// (including content-independent promotions such as `.h` → `cpp` inside
    /// a C++ tree, #3009), while `GrammarRegistry::find_by_path` resolves the
    /// grammar from the extension table. When they disagree the buffer ends
    /// up with a C++ language id and a C grammar: the status bar says `C`,
    /// keywords like `namespace` / `template` render unhighlighted, and `::`
    /// doesn't scope.
    ///
    /// Rather than patching each caller, this is the single fork where the two
    /// halves are reconciled: the config id wins, and the grammar is
    /// re-resolved through that id's `[languages.<id>].grammar`. If the id has
    /// no grammar the registry knows about, the path-resolved grammar is kept
    /// — an unknown config key must never downgrade working highlighting.
    ///
    /// Cost is one hash lookup; no filesystem access and no buffer scan are
    /// added on top of what `detect_language` already does.
    fn align_with_config_id(
        mut detected: Self,
        config_lang_id: &Option<String>,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
    ) -> Self {
        let Some(id) = config_lang_id.as_deref() else {
            return detected;
        };

        // The grammar the config id asks for. An empty `grammar` field means
        // "same name as the key" — matching `apply_language_config`.
        let grammar_name = languages
            .get(id)
            .map(|lc| lc.grammar.as_str())
            .filter(|g| !g.is_empty())
            .unwrap_or(id);

        if let Some(entry) = registry
            .find_by_name(grammar_name)
            .or_else(|| registry.find_by_name(id))
        {
            // `find_by_name` resolves aliases, so an entry with the same
            // `language_id` *is* the grammar we already have — the common
            // case (`[languages.rust] grammar = "Rust"` on a `.rs` file), and
            // nothing but the name needs stamping.
            if entry.language_id != detected.name {
                let mut aligned = Self::from_entry(entry, registry);
                aligned.name = id.to_string();
                return aligned;
            }
        }

        detected.name = id.to_string();
        detected
    }

    /// Set language by syntax name (user selected from the language palette).
    ///
    /// Looks up the entry in the unified catalog. The `languages` config is used
    /// to resolve the canonical language ID (e.g., "Rust" syntax → "rust" config key).
    /// Returns `None` if the name matches no catalog entry.
    pub fn from_syntax_name(
        name: &str,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
    ) -> Option<Self> {
        let entry = registry.find_by_name(name)?;
        let mut detected = Self::from_entry(entry, registry);
        // Prefer a matching config language ID so LSP lookup works when the
        // user has declared the language under a different key. `display_name`
        // keeps the catalog's canonical value ("Bourne Again Shell (bash)"),
        // not whatever casing the caller typed ("BASH").
        if let Some(id) = resolve_language_id(&entry.display_name, registry, languages) {
            detected.name = id;
        }
        Some(detected)
    }

    /// Plain text — no highlighting.
    pub fn plain_text() -> Self {
        Self {
            name: "text".to_string(),
            display_name: "Text".to_string(),
            highlighter: HighlightEngine::None,
            ts_language: None,
        }
    }

    /// Detect language from a virtual buffer name like `*OLD:test.ts*` or `*OURS*.c`.
    ///
    /// Strips surrounding `*` characters and extracts the filename after any
    /// prefix like "OLD:" or "NEW:".
    pub fn from_virtual_name(name: &str, registry: &GrammarRegistry) -> Self {
        let cleaned = name.trim_matches('*');
        let filename = if let Some(pos) = cleaned.rfind(':') {
            &cleaned[pos + 1..]
        } else {
            cleaned
        };
        registry
            .find_by_path(Path::new(filename), None)
            .map(|entry| Self::from_entry(entry, registry))
            .unwrap_or_else(Self::plain_text)
    }
}

/// Resolve a syntect syntax display name to its canonical config language ID.
///
/// The config `[languages]` section is the single authoritative registry of
/// language IDs. Each entry has a `grammar` field that is resolved to a
/// catalog entry; this function performs the reverse lookup.
pub fn resolve_language_id(
    syntax_name: &str,
    registry: &GrammarRegistry,
    languages: &HashMap<String, LanguageConfig>,
) -> Option<String> {
    for (lang_id, lang_config) in languages {
        if let Some(entry) = registry.find_by_name(&lang_config.grammar) {
            if entry.display_name == syntax_name {
                return Some(lang_id.clone());
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::model::filesystem::{
        DirEntry, EntryType, FileMetadata, FilePermissions, FileReader, FileSearchCursor,
        FileSearchOptions, FileSystem, FileWriter, NoopFileSystem, SearchMatch, StdFileSystem,
    };
    use std::io;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// The default `[languages]` table — `c` owns `h`, `cpp` does not.
    fn default_languages() -> HashMap<String, LanguageConfig> {
        Config::default().languages
    }

    fn registry_with(languages: &HashMap<String, LanguageConfig>) -> GrammarRegistry {
        let mut registry = GrammarRegistry::default();
        registry.apply_language_config(languages);
        registry
    }

    /// A `FileSystem` whose entire contents live in this struct and exist
    /// nowhere on the process's disk.
    ///
    /// This is the point of the type: the `.h` → C++ probe used to call
    /// `std::fs` directly, so on an SSH session it answered about the local
    /// machine instead of the host owning the file and the promotion became
    /// a silent no-op. A test that writes real files into a tempdir cannot
    /// catch that — `std::fs` and the injected filesystem agree there. These
    /// paths are unopenable by `std::fs`, so any answer other than "wrong"
    /// proves detection went through the trait.
    ///
    /// `ops` counts the calls the probe actually makes, which is how the
    /// remote I/O budget is asserted: each of these is one blocking agent
    /// round trip when the filesystem is a real `RemoteFileSystem`.
    struct FakeTree {
        /// Directory path → names of the entries it contains.
        dirs: HashMap<PathBuf, Vec<String>>,
        /// File path → contents.
        files: HashMap<PathBuf, Vec<u8>>,
        /// `Some(..)` makes this look like an SSH filesystem to
        /// `ProbeBudget`, exactly as `RemoteFileSystem` does.
        connection: Option<String>,
        ops: AtomicUsize,
    }

    impl FakeTree {
        fn new() -> Self {
            Self {
                dirs: HashMap::new(),
                files: HashMap::new(),
                connection: None,
                ops: AtomicUsize::new(0),
            }
        }

        /// Mark this filesystem as remote, so the probe budgets it like an
        /// SSH host (see `ProbeBudget` in `services/lsp/manager.rs`).
        fn remote(mut self) -> Self {
            self.connection = Some("user@fake-host".to_string());
            self
        }

        /// Add a file, registering it under its parent directory too.
        fn file(mut self, path: &str, contents: &str) -> Self {
            let path = PathBuf::from(path);
            if let (Some(parent), Some(name)) = (
                path.parent().map(Path::to_path_buf),
                path.file_name()
                    .and_then(|n| n.to_str())
                    .map(str::to_string),
            ) {
                self.dirs.entry(parent).or_default().push(name);
            }
            self.files.insert(path, contents.as_bytes().to_vec());
            self
        }

        fn ops(&self) -> usize {
            self.ops.load(Ordering::SeqCst)
        }

        fn count_op(&self) {
            self.ops.fetch_add(1, Ordering::SeqCst);
        }

        fn missing<T>() -> io::Result<T> {
            Err(io::Error::new(io::ErrorKind::NotFound, "not in fake tree"))
        }
    }

    impl FileSystem for FakeTree {
        fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
            self.count_op();
            let Some(names) = self.dirs.get(path) else {
                return Self::missing();
            };
            Ok(names
                .iter()
                .map(|name| DirEntry::new(path.join(name), name.clone(), EntryType::File))
                .collect())
        }

        fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
            self.count_op();
            match self.files.get(path) {
                Some(bytes) => Ok(FileMetadata::new(bytes.len() as u64)),
                None => Self::missing(),
            }
        }

        fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
            self.count_op();
            let Some(bytes) = self.files.get(path) else {
                return Self::missing();
            };
            let start = offset as usize;
            let end = start.saturating_add(len);
            if end > bytes.len() {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "read past end of fake file",
                ));
            }
            Ok(bytes[start..end].to_vec())
        }

        fn remote_connection_info(&self) -> Option<&str> {
            self.connection.as_deref()
        }

        // ---- boilerplate: everything the probe must never touch ----
        fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
            NoopFileSystem.read_file(path)
        }
        fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
            NoopFileSystem.write_file(path, data)
        }
        fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
            NoopFileSystem.create_file(path)
        }
        fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
            NoopFileSystem.open_file(path)
        }
        fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
            NoopFileSystem.open_file_for_write(path)
        }
        fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
            NoopFileSystem.open_file_for_append(path)
        }
        fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
            NoopFileSystem.set_file_length(path, len)
        }
        fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
            NoopFileSystem.rename(from, to)
        }
        fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
            NoopFileSystem.copy(from, to)
        }
        fn remove_file(&self, path: &Path) -> io::Result<()> {
            NoopFileSystem.remove_file(path)
        }
        fn remove_dir(&self, path: &Path) -> io::Result<()> {
            NoopFileSystem.remove_dir(path)
        }
        fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
            NoopFileSystem.symlink_metadata(path)
        }
        fn is_dir(&self, path: &Path) -> io::Result<bool> {
            NoopFileSystem.is_dir(path)
        }
        fn is_file(&self, path: &Path) -> io::Result<bool> {
            NoopFileSystem.is_file(path)
        }
        fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
            NoopFileSystem.set_permissions(path, permissions)
        }
        fn create_dir(&self, path: &Path) -> io::Result<()> {
            NoopFileSystem.create_dir(path)
        }
        fn create_dir_all(&self, path: &Path) -> io::Result<()> {
            NoopFileSystem.create_dir_all(path)
        }
        fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
            NoopFileSystem.canonicalize(path)
        }
        fn current_uid(&self) -> u32 {
            0
        }
        fn search_file(
            &self,
            path: &Path,
            pattern: &str,
            opts: &FileSearchOptions,
            cursor: &mut FileSearchCursor,
        ) -> io::Result<Vec<SearchMatch>> {
            NoopFileSystem.search_file(path, pattern, opts, cursor)
        }
        fn sudo_write(
            &self,
            path: &Path,
            data: &[u8],
            mode: u32,
            uid: u32,
            gid: u32,
        ) -> io::Result<()> {
            NoopFileSystem.sudo_write(path, data, mode, uid, gid)
        }
        fn walk_files(
            &self,
            root: &Path,
            skip_dirs: &[&str],
            cancel: &std::sync::atomic::AtomicBool,
            on_file: &mut dyn FnMut(&Path, &str) -> bool,
        ) -> io::Result<()> {
            NoopFileSystem.walk_files(root, skip_dirs, cancel, on_file)
        }
    }

    /// #3009: a `.h` header sitting next to C++ sources must highlight with
    /// the C++ grammar, not just route to clangd in C++ mode. Before the
    /// alignment step the id said `cpp` while the grammar and the status-bar
    /// label said `C`.
    #[test]
    fn test_h_header_beside_cpp_sibling_detects_cpp_grammar() {
        let tmp = tempfile::tempdir().unwrap();
        let header = tmp.path().join("widget.h");
        std::fs::write(&header, "namespace ui { class Widget {}; }\n").unwrap();
        std::fs::write(tmp.path().join("widget.cpp"), "").unwrap();

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected =
            DetectedLanguage::from_path(&header, None, &registry, &languages, &StdFileSystem);

        assert_eq!(detected.name, "cpp");
        assert_eq!(detected.display_name, "C++");
        assert_eq!(detected.ts_language, Some(Language::Cpp));
    }

    /// The mirror case: the same bytes in a plain C project stay C, so the
    /// promotion can't turn every header into C++.
    #[test]
    fn test_h_header_in_pure_c_project_stays_c_grammar() {
        let tmp = tempfile::tempdir().unwrap();
        let header = tmp.path().join("widget.h");
        std::fs::write(&header, "namespace ui { class Widget {}; }\n").unwrap();
        std::fs::write(tmp.path().join("widget.c"), "").unwrap();

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected =
            DetectedLanguage::from_path(&header, None, &registry, &languages, &StdFileSystem);

        assert_eq!(detected.name, "c");
        assert_eq!(detected.display_name, "C");
        assert_eq!(detected.ts_language, Some(Language::C));
    }

    /// The pre-existing user workaround — moving `h` into
    /// `languages.cpp.extensions` — must still force C++ unconditionally,
    /// with no C++ sibling anywhere in the tree.
    #[test]
    fn test_user_config_can_force_h_to_cpp_without_siblings() {
        let tmp = tempfile::tempdir().unwrap();
        let header = tmp.path().join("lonely.h");
        std::fs::write(&header, "").unwrap();

        let mut languages = default_languages();
        languages.get_mut("c").unwrap().extensions = vec!["c".to_string()];
        languages
            .get_mut("cpp")
            .unwrap()
            .extensions
            .push("h".to_string());
        let registry = registry_with(&languages);
        let detected =
            DetectedLanguage::from_path(&header, None, &registry, &languages, &StdFileSystem);

        assert_eq!(detected.name, "cpp");
        assert_eq!(detected.ts_language, Some(Language::Cpp));
    }

    /// And the other direction: a user with no `cpp` language configured
    /// keeps the C grammar even in a tree full of C++ sources.
    #[test]
    fn test_user_config_without_cpp_language_keeps_h_as_c() {
        let tmp = tempfile::tempdir().unwrap();
        let header = tmp.path().join("widget.h");
        std::fs::write(&header, "").unwrap();
        std::fs::write(tmp.path().join("widget.cpp"), "").unwrap();

        let mut languages = default_languages();
        languages.remove("cpp");
        let registry = registry_with(&languages);
        let detected =
            DetectedLanguage::from_path(&header, None, &registry, &languages, &StdFileSystem);

        assert_eq!(detected.name, "c");
        assert_eq!(detected.ts_language, Some(Language::C));
    }

    /// Aliasing a built-in grammar under a custom config key must keep that
    /// grammar — the alignment step resolves `[languages.mylang].grammar` and
    /// lands on the very entry the path lookup already produced.
    #[test]
    fn test_config_alias_keeps_aliased_grammar() {
        let mut languages = default_languages();
        let mut alias = languages.get("rust").cloned().unwrap();
        alias.extensions = vec!["ml2".to_string()];
        alias.grammar = "Rust".to_string();
        languages.insert("mylang".to_string(), alias);
        let registry = registry_with(&languages);

        let detected = DetectedLanguage::from_path(
            Path::new("a.ml2"),
            None,
            &registry,
            &languages,
            &StdFileSystem,
        );
        assert_eq!(detected.name, "mylang");
        assert_eq!(detected.ts_language, Some(Language::Rust));
    }

    /// A config key whose grammar the registry knows nothing about must not
    /// downgrade the highlighting the path lookup already resolved.
    #[test]
    fn test_unknown_config_grammar_keeps_path_resolved_highlighter() {
        let mut languages = default_languages();
        let mut odd = languages.get("rust").cloned().unwrap();
        odd.extensions = vec!["rs".to_string()];
        odd.grammar = "no-such-grammar".to_string();
        languages.remove("rust");
        languages.insert("weird".to_string(), odd);
        // Deliberately *not* applying the config: the registry still resolves
        // `.rs` to Rust, and the unknown grammar name must leave it alone.
        let registry = GrammarRegistry::default();

        let detected = DetectedLanguage::from_path(
            Path::new("a.rs"),
            None,
            &registry,
            &languages,
            &StdFileSystem,
        );
        assert_eq!(detected.name, "weird");
        assert_eq!(detected.ts_language, Some(Language::Rust));
    }

    /// The remote fix, stated positively: the C++ sibling exists only in the
    /// injected filesystem, at a path `std::fs` cannot open. Before the
    /// filesystem was threaded through, the probe called `std::fs::read_dir`
    /// on `/remote-project/src`, got `NotFound`, and left the header as C —
    /// which is exactly what an SSH user saw for every header in a remote
    /// C++ tree.
    #[test]
    fn test_h_header_promotion_reads_injected_filesystem_not_local_disk() {
        let fs = FakeTree::new()
            .remote()
            .file("/remote-project/src/widget.h", "")
            .file("/remote-project/src/widget.cpp", "");
        assert!(
            !Path::new("/remote-project/src/widget.cpp").exists(),
            "the fixture must not exist on the real disk, or it proves nothing"
        );

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected = DetectedLanguage::from_path(
            Path::new("/remote-project/src/widget.h"),
            None,
            &registry,
            &languages,
            &fs,
        );

        assert_eq!(detected.name, "cpp");
        assert_eq!(detected.display_name, "C++");
        assert_eq!(detected.ts_language, Some(Language::Cpp));
    }

    /// The same fix stated negatively, and the sharper half: the process's
    /// own disk *does* hold a C++ sibling next to the header path, while the
    /// injected filesystem holds only C. A probe that still reached for
    /// `std::fs` would promote to C++ here; the correct answer is C, because
    /// the injected filesystem is the one that owns the file.
    #[test]
    fn test_h_header_promotion_ignores_local_disk_when_injected_fs_says_c() {
        let tmp = tempfile::tempdir().unwrap();
        let header = tmp.path().join("widget.h");
        std::fs::write(&header, "").unwrap();
        std::fs::write(tmp.path().join("widget.cpp"), "").unwrap();

        // The fake reports the same directory containing only C sources.
        let dir = tmp.path().to_string_lossy().to_string();
        let fs = FakeTree::new()
            .remote()
            .file(&format!("{dir}/widget.h"), "")
            .file(&format!("{dir}/widget.c"), "");

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected = DetectedLanguage::from_path(&header, None, &registry, &languages, &fs);

        assert_eq!(
            detected.name, "c",
            "promotion must follow the injected filesystem, not the local disk"
        );
        assert_eq!(detected.ts_language, Some(Language::C));
    }

    /// A remote filesystem's sync methods are blocking agent round trips on
    /// the single-threaded editor loop, so the probe spends exactly one on
    /// the file-open path: the sibling listing. The ten-deep
    /// `compile_commands.json` ancestor walk is not affordable there and is
    /// budgeted away — a remote header under `include/` stays C rather than
    /// costing up to a dozen serialized round trips before the file can be
    /// shown.
    #[test]
    fn test_remote_probe_spends_one_op_and_skips_ancestor_walk() {
        let fs = FakeTree::new()
            .remote()
            .file("/proj/include/widget.h", "")
            .file(
                "/proj/compile_commands.json",
                r#"[{"command":"clang++ -std=c++17 -c widget.cpp"}]"#,
            );

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected = DetectedLanguage::from_path(
            Path::new("/proj/include/widget.h"),
            None,
            &registry,
            &languages,
            &fs,
        );

        assert_eq!(detected.name, "c", "remote budget skips the ancestor walk");
        assert_eq!(
            fs.ops(),
            1,
            "exactly one filesystem op (the sibling listing) may reach a remote host"
        );
    }

    /// The mirror of the budget test: the identical tree on a *local*
    /// filesystem still walks ancestors and finds the C++ marker, so
    /// budgeting the remote path costs local users nothing.
    #[test]
    fn test_local_probe_still_walks_ancestors_for_compile_commands() {
        let fs = FakeTree::new().file("/proj/include/widget.h", "").file(
            "/proj/compile_commands.json",
            r#"[{"command":"clang++ -std=c++17 -c widget.cpp"}]"#,
        );

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected = DetectedLanguage::from_path(
            Path::new("/proj/include/widget.h"),
            None,
            &registry,
            &languages,
            &fs,
        );

        assert_eq!(detected.name, "cpp");
        assert_eq!(detected.ts_language, Some(Language::Cpp));
    }

    /// `FileSystem::read_range` is `read_exact`-shaped: asking for more bytes
    /// than the file holds fails outright. The marker read therefore clamps
    /// to the file's real size, and a small `compile_commands.json` — the
    /// normal case — must still be read rather than erroring into "not C++".
    #[test]
    fn test_small_compile_commands_is_read_despite_the_one_mib_cap() {
        let fs = FakeTree::new().file("/proj/include/widget.h", "").file(
            "/proj/compile_commands.json",
            r#"[{"command":"g++ -c a.cpp"}]"#,
        );

        let languages = default_languages();
        let registry = registry_with(&languages);
        let detected = DetectedLanguage::from_path(
            Path::new("/proj/include/widget.h"),
            None,
            &registry,
            &languages,
            &fs,
        );

        assert_eq!(detected.name, "cpp");
    }

    /// Detection must not probe the filesystem at all for the overwhelming
    /// majority of files — only a `.h` resolving to `c` in a config that
    /// knows `cpp` can trigger it. Guards the ordering in `detect_language`
    /// that keeps file open off the I/O path for everything else.
    #[test]
    fn test_non_header_paths_touch_no_filesystem() {
        let fs = FakeTree::new().remote();
        let languages = default_languages();
        let registry = registry_with(&languages);

        for name in ["/proj/main.rs", "/proj/main.c", "/proj/widget.hpp"] {
            let _ = DetectedLanguage::from_path(Path::new(name), None, &registry, &languages, &fs);
        }

        assert_eq!(fs.ops(), 0, "no filesystem access for non-`.h` paths");
    }
}
