//! A document's language, found in the editor's grammar catalog: the key
//! its language server is configured by, the grammar the client highlights
//! it with, and its line-comment prefix.

use fresh_editor_core::config::{Config, LanguageConfig};
use fresh_editor_core::primitives::grammar::{GrammarEntry, GrammarRegistry};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, LazyLock};

/// The editor's built-in grammars, loaded once.
static REGISTRY: LazyLock<Arc<GrammarRegistry>> = LazyLock::new(GrammarRegistry::defaults_only);

/// The editor's default per-language settings, by language id.
static LANGUAGE_CONFIG: LazyLock<HashMap<String, LanguageConfig>> =
    LazyLock::new(|| Config::default().languages);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Language {
    /// The catalog's language id ("rust", "typescript", "bash"): the key
    /// language servers are configured by.
    pub id: String,
    /// The syntect syntax the client highlights it with (`hl_open`); none
    /// for a language only tree-sitter knows.
    pub syntax: Option<String>,
    /// Its line-comment prefix, when it has one.
    pub comment: Option<String>,
}

impl Language {
    /// `entry`'s language for a file at `path` (an untitled document has
    /// none): a dialect grammar registered for the file's extension, such as
    /// TypeScriptReact for `.tsx`, is the one it is highlighted with.
    fn of_entry(entry: &GrammarEntry, path: Option<&Path>) -> Self {
        let syntax = path
            .map_or(entry.engines.syntect, |p| entry.syntect_for_path(p))
            .map(|index| REGISTRY.syntax_set().syntaxes()[index].name.clone());
        Language {
            id: entry.language_id.clone(),
            syntax,
            comment: LANGUAGE_CONFIG
                .get(&entry.language_id)
                .and_then(|config| config.comment_prefix.clone()),
        }
    }
}

/// The language of the file at `path`, whose first line is `first_line`
/// (for a shebang, when the name says nothing).
pub fn of_path(path: &Path, first_line: Option<&str>) -> Option<Language> {
    REGISTRY
        .find_by_path(path, first_line)
        .map(|entry| Language::of_entry(entry, Some(path)))
}

/// The language of id `id`, for a document of the file at `path` (if any).
pub fn by_id(id: &str, path: Option<&Path>) -> Option<Language> {
    REGISTRY
        .catalog()
        .iter()
        .find(|entry| entry.language_id == id)
        .map(|entry| Language::of_entry(entry, path))
}

/// Every language a document can be set to (`doc.formats`), by display
/// name: `{language, syntax, name}`.
pub fn catalog() -> Vec<Value> {
    let mut entries: Vec<&GrammarEntry> = REGISTRY.catalog().iter().collect();
    entries.sort_by_key(|entry| entry.display_name.to_lowercase());
    entries
        .into_iter()
        .map(|entry| {
            json!({
                "language": entry.language_id,
                "syntax": Language::of_entry(entry, None).syntax,
                "name": entry.display_name,
            })
        })
        .collect()
}
