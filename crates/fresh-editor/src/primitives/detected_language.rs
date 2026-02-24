//! Unified language detection for editor buffers.
//!
//! This module provides `DetectedLanguage`, the single source of truth for
//! determining a buffer's language, syntax highlighter, and tree-sitter support.
//! All code paths that set or change a buffer's language should go through this module.

use crate::config::LanguageConfig;
use crate::primitives::highlight_engine::HighlightEngine;
use crate::primitives::highlighter::Language;
use crate::primitives::GrammarRegistry;
use std::collections::HashMap;
use std::path::Path;

/// The result of language detection — groups the three things that must stay in sync
/// on an `EditorState`: the language name, the highlighting engine, and the
/// tree-sitter `Language` (used for reference highlighting, indentation, etc.).
pub struct DetectedLanguage {
    /// The language name for LSP, status bar, and config lookup
    /// (e.g., "Rust", "Python", "text", "Plain Text").
    pub name: String,
    /// The highlighting engine to use for this buffer.
    pub highlighter: HighlightEngine,
    /// The tree-sitter Language, if available (used for reference highlighting,
    /// auto-indent, bracket matching, etc.). Only ~18 languages have tree-sitter
    /// support; this is `None` for the remaining 100+ syntect-only languages.
    pub ts_language: Option<Language>,
}

impl DetectedLanguage {
    /// Detect language from a file path using user configuration.
    ///
    /// This is the primary detection path used when opening, reloading, or saving files.
    /// Priority order matches the grammar registry:
    /// 1. Exact filename match in user config
    /// 2. Glob pattern match in user config
    /// 3. Extension match in user config
    /// 4. Built-in detection (tree-sitter `Language::from_path` + syntect)
    pub fn from_path(
        path: &Path,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
    ) -> Self {
        let highlighter = HighlightEngine::for_file_with_languages(path, registry, languages);
        let ts_language = Language::from_path(path);
        let name = if let Some(lang) = &ts_language {
            lang.to_string()
        } else {
            crate::services::lsp::manager::detect_language(path, languages)
                .unwrap_or_else(|| "text".to_string())
        };
        Self {
            name,
            highlighter,
            ts_language,
        }
    }

    /// Detect language from a file path using only built-in rules (no user config).
    ///
    /// Used by `from_file()` (the legacy constructor) and for virtual buffer names
    /// where user config doesn't apply.
    pub fn from_path_builtin(path: &Path, registry: &GrammarRegistry) -> Self {
        let highlighter = HighlightEngine::for_file(path, registry);
        let ts_language = Language::from_path(path);
        let name = ts_language
            .as_ref()
            .map(|l| l.to_string())
            .unwrap_or_else(|| "text".to_string());
        Self {
            name,
            highlighter,
            ts_language,
        }
    }

    /// Set language by syntax name (user selected from the language palette).
    ///
    /// Looks up the syntax in the grammar registry and optionally finds a
    /// tree-sitter language for enhanced features. The `languages` config is used
    /// to resolve the canonical language ID (e.g., "Rust" syntax → "rust" config key).
    /// Returns `None` if the syntax name is not found in the registry.
    pub fn from_syntax_name(
        name: &str,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
    ) -> Option<Self> {
        if registry.find_syntax_by_name(name).is_some() {
            let ts_language = Language::from_name(name);
            let highlighter = HighlightEngine::for_syntax_name(name, registry, ts_language);
            // Resolve the canonical language ID from config (e.g., "Rust" → "rust").
            let language_id =
                resolve_language_id(name, registry, languages).unwrap_or_else(|| name.to_string());
            Some(Self {
                name: language_id,
                highlighter,
                ts_language,
            })
        } else {
            None
        }
    }

    /// Plain text — no highlighting.
    pub fn plain_text() -> Self {
        Self {
            name: "text".to_string(),
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
        Self::from_path_builtin(Path::new(filename), registry)
    }
}

/// Resolve a syntect syntax display name to its canonical config language ID.
///
/// The config `[languages]` section is the single authoritative registry of
/// language IDs. Each entry has a `grammar` field that is resolved to a
/// syntect syntax via `GrammarRegistry::find_syntax_by_name`. This function
/// performs the reverse lookup: for each config entry, resolve its grammar
/// through the registry and check whether the resulting syntax matches.
pub fn resolve_language_id(
    syntax_name: &str,
    registry: &GrammarRegistry,
    languages: &HashMap<String, LanguageConfig>,
) -> Option<String> {
    for (lang_id, lang_config) in languages {
        if let Some(syntax) = registry.find_syntax_by_name(&lang_config.grammar) {
            if syntax.name == syntax_name {
                return Some(lang_id.clone());
            }
        }
    }
    None
}
