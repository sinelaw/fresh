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
    pub fn from_path(
        path: &Path,
        first_line: Option<&str>,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
    ) -> Self {
        Self::from_path_with_fallback(path, first_line, registry, languages, None)
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
    ) -> Self {
        // Resolve the config/LSP language id *independently* of the grammar
        // catalog. A file matching a `[languages.foo]` rule must end up with
        // `name = "foo"` so comment prefix / tab config / LSP routing all
        // work — even when the grammar registry is empty (common in tests)
        // or has no matching entry.
        let config_lang_id = crate::services::lsp::manager::detect_language(path, languages);
        let override_name = |mut d: Self| -> Self {
            if let Some(id) = config_lang_id.clone() {
                d.name = id;
            }
            d
        };

        if let Some(entry) = registry.find_by_path(path, first_line) {
            return override_name(Self::from_entry(entry, registry));
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
                return override_name(Self::from_entry(entry, registry));
            }
        }

        override_name(Self::plain_text())
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
