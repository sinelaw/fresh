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
    /// Detect language from a file path using user configuration.
    ///
    /// This is the primary detection path used when opening, reloading, or saving files.
    /// Priority order matches the grammar registry:
    /// 1. Exact filename match in user config
    /// 2. Glob pattern match in user config
    /// 3. Extension match in user config
    /// 4. Built-in detection (tree-sitter `Language::from_path` + syntect)
    /// 5. Fallback config (if set and no other match found)
    pub fn from_path(
        path: &Path,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
    ) -> Self {
        Self::from_path_with_fallback(path, registry, languages, None)
    }

    /// Like `from_path`, but also accepts an optional fallback language config
    /// that is applied when no language is detected (#1219).
    pub fn from_path_with_fallback(
        path: &Path,
        registry: &GrammarRegistry,
        languages: &HashMap<String, LanguageConfig>,
        fallback: Option<&LanguageConfig>,
    ) -> Self {
        let highlighter = HighlightEngine::for_file_with_languages(path, registry, languages);
        let ts_language = Language::from_path(path);
        // Prefer config-based language name (e.g., "csharp") so it matches
        // the LSP config key. Fall back to tree-sitter name (e.g., "c_sharp")
        // or "text" if neither is available.
        let name =
            crate::services::lsp::manager::detect_language(path, languages).unwrap_or_else(|| {
                ts_language
                    .as_ref()
                    .map(|l| l.to_string())
                    .unwrap_or_else(|| "text".to_string())
            });
        // Resolve display name from the syntax matched for this file.
        let display_name = registry
            .find_syntax_for_file_with_languages(path, languages)
            .map(|s| s.name.clone())
            .unwrap_or_else(|| name.clone());

        // If no language was detected and a fallback config is set with a grammar,
        // try to use the fallback grammar for highlighting (#1219)
        if name == "text" && matches!(highlighter, HighlightEngine::None) {
            if let Some(fb) = fallback {
                if !fb.grammar.is_empty() {
                    let fb_highlighter =
                        HighlightEngine::for_syntax_name(&fb.grammar, registry, ts_language);
                    if !matches!(fb_highlighter, HighlightEngine::None) {
                        let fb_display = registry
                            .find_syntax_by_name(&fb.grammar)
                            .map(|s| s.name.clone())
                            .unwrap_or_else(|| fb.grammar.clone());
                        return Self {
                            name,
                            display_name: fb_display,
                            highlighter: fb_highlighter,
                            ts_language,
                        };
                    }
                }
            }
        }

        Self {
            name,
            display_name,
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
        let display_name = registry
            .find_syntax_for_file(path)
            .map(|s| s.name.clone())
            .unwrap_or_else(|| name.clone());
        Self {
            name,
            display_name,
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
                display_name: name.to_string(),
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
        Self::from_path_builtin(Path::new(filename), registry)
    }
}

/// Resolve a syntect syntax display name to its canonical config language ID.
///
/// The config `[languages]` section is the single authoritative registry of
/// language IDs. Each entry has a `grammar` field that is resolved to a
/// syntect syntax via the grammar registry. This function performs the reverse
/// lookup: for each config entry, resolve its grammar through the registry
/// and check whether the resulting syntax matches.
pub fn resolve_language_id(
    syntax_name: &str,
    registry: &GrammarRegistry,
    languages: &HashMap<String, LanguageConfig>,
) -> Option<String> {
    for (lang_id, lang_config) in languages {
        // Use find_syntax_for_lang_config which also tries extension fallback,
        // needed when the grammar name doesn't match syntect's name
        // (e.g., grammar "c_sharp" → syntect syntax "C#").
        if let Some(syntax) = registry.find_syntax_for_lang_config(lang_config) {
            if syntax.name == syntax_name {
                return Some(lang_id.clone());
            }
        }
    }
    None
}
