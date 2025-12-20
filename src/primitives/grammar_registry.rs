//! Grammar registry for TextMate grammars
//!
//! This module handles discovery and loading of TextMate grammars from:
//! 1. Built-in syntect grammars (100+ languages)
//! 2. Embedded grammars for languages not in syntect (TOML, etc.)
//! 3. User-installed grammars in ~/.config/fresh/grammars/
//!
//! User grammars use VSCode extension format for compatibility.

use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use syntect::parsing::{SyntaxDefinition, SyntaxReference, SyntaxSet, SyntaxSetBuilder};

/// Embedded TOML grammar (syntect doesn't include one)
const TOML_GRAMMAR: &str = include_str!("../grammars/toml.sublime-syntax");

/// Registry of all available TextMate grammars
pub struct GrammarRegistry {
    /// Combined syntax set (built-in + embedded + user grammars)
    syntax_set: Arc<SyntaxSet>,
    /// Extension -> scope name mapping for user grammars (takes priority)
    user_extensions: HashMap<String, String>,
}

impl GrammarRegistry {
    /// Create a fully-loaded grammar registry for the editor
    /// Loads built-in, embedded, and user grammars
    pub fn for_editor() -> Arc<Self> {
        Arc::new(Self::load())
    }

    /// Load grammar registry, scanning user grammars directory
    pub fn load() -> Self {
        let mut user_extensions = HashMap::new();

        // Start with syntect defaults, convert to builder to add more
        let defaults = SyntaxSet::load_defaults_newlines();
        let mut builder = defaults.into_builder();

        // Add embedded grammars (TOML, etc.)
        Self::add_embedded_grammars(&mut builder);

        // Add user grammars from config directory
        if let Some(grammars_dir) = Self::grammars_directory() {
            if grammars_dir.exists() {
                Self::load_user_grammars_into(&grammars_dir, &mut builder, &mut user_extensions);
            }
        }

        let syntax_set = builder.build();

        tracing::info!(
            "Loaded {} syntaxes, {} user extension mappings",
            syntax_set.syntaxes().len(),
            user_extensions.len()
        );

        Self {
            syntax_set: Arc::new(syntax_set),
            user_extensions,
        }
    }

    /// Create an empty grammar registry (fast, for tests that don't need syntax highlighting)
    pub fn empty() -> Arc<Self> {
        let mut builder = SyntaxSetBuilder::new();
        builder.add_plain_text_syntax();
        Arc::new(Self {
            syntax_set: Arc::new(builder.build()),
            user_extensions: HashMap::new(),
        })
    }

    /// Get the grammars directory path
    pub fn grammars_directory() -> Option<PathBuf> {
        dirs::config_dir().map(|p| p.join("fresh/grammars"))
    }

    /// Add embedded grammars for languages not in syntect's defaults
    fn add_embedded_grammars(builder: &mut SyntaxSetBuilder) {
        // TOML grammar
        match SyntaxDefinition::load_from_str(TOML_GRAMMAR, true, Some("TOML")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded TOML grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded TOML grammar: {}", e);
            }
        }
    }

    /// Load user grammars into builder
    fn load_user_grammars_into(
        dir: &Path,
        builder: &mut SyntaxSetBuilder,
        user_extensions: &mut HashMap<String, String>,
    ) {
        // Iterate through subdirectories looking for package.json or direct grammar files
        let entries = match std::fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(e) => {
                tracing::warn!("Failed to read grammars directory {:?}: {}", dir, e);
                return;
            }
        };

        for entry in entries.flatten() {
            let path = entry.path();

            if !path.is_dir() {
                continue;
            }

            // Check for package.json (VSCode extension format)
            let manifest_path = path.join("package.json");
            if manifest_path.exists() {
                if let Ok(manifest) = Self::parse_package_json(&manifest_path) {
                    Self::process_manifest(&path, manifest, builder, user_extensions);
                }
                continue;
            }

            // Check for direct grammar files
            let mut found_any = false;
            Self::load_direct_grammar(&path, builder, user_extensions, &mut found_any);
        }
    }

    /// Load a grammar directly from a .tmLanguage.json file
    fn load_direct_grammar(
        dir: &Path,
        builder: &mut SyntaxSetBuilder,
        _user_extensions: &mut HashMap<String, String>,
        found_any: &mut bool,
    ) {
        // Look for .tmLanguage.json or .sublime-syntax files
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };

        for entry in entries.flatten() {
            let path = entry.path();
            let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

            if file_name.ends_with(".tmLanguage.json")
                || file_name.ends_with(".tmLanguage")
                || file_name.ends_with(".sublime-syntax")
            {
                if let Err(e) = builder.add_from_folder(&dir, false) {
                    tracing::warn!("Failed to load grammar from {:?}: {}", dir, e);
                } else {
                    tracing::info!("Loaded grammar from {:?}", dir);
                    *found_any = true;
                }
                break;
            }
        }
    }

    /// Parse a VSCode package.json manifest
    fn parse_package_json(path: &Path) -> Result<PackageManifest, String> {
        let content =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;

        serde_json::from_str(&content).map_err(|e| format!("Failed to parse JSON: {}", e))
    }

    /// Process a package manifest and load its grammars
    fn process_manifest(
        package_dir: &Path,
        manifest: PackageManifest,
        builder: &mut SyntaxSetBuilder,
        user_extensions: &mut HashMap<String, String>,
    ) {
        let contributes = match manifest.contributes {
            Some(c) => c,
            None => return,
        };

        // Build language ID -> extensions mapping
        let mut lang_extensions: HashMap<String, Vec<String>> = HashMap::new();
        for lang in &contributes.languages {
            lang_extensions.insert(lang.id.clone(), lang.extensions.clone());
        }

        // Process each grammar
        for grammar in &contributes.grammars {
            let grammar_path = package_dir.join(&grammar.path);

            if !grammar_path.exists() {
                tracing::warn!("Grammar file not found: {:?}", grammar_path);
                continue;
            }

            // Try to load the grammar
            let grammar_dir = grammar_path.parent().unwrap_or(package_dir);
            if let Err(e) = builder.add_from_folder(grammar_dir, false) {
                tracing::warn!("Failed to load grammar {:?}: {}", grammar_path, e);
                continue;
            }

            tracing::info!(
                "Loaded grammar {} from {:?}",
                grammar.scope_name,
                grammar_path
            );

            // Map extensions to scope name
            if let Some(extensions) = lang_extensions.get(&grammar.language) {
                for ext in extensions {
                    let ext_clean = ext.trim_start_matches('.');
                    user_extensions.insert(ext_clean.to_string(), grammar.scope_name.clone());
                    tracing::debug!("Mapped extension .{} to {}", ext_clean, grammar.scope_name);
                }
            }
        }
    }

    /// Find syntax for a file by path/extension/filename.
    ///
    /// Checks in order:
    /// 1. User-configured grammar extensions (by scope)
    /// 2. By extension (includes built-in + embedded grammars)
    /// 3. By filename (handles Makefile, .bashrc, etc.)
    pub fn find_syntax_for_file(&self, path: &Path) -> Option<&SyntaxReference> {
        // Try extension-based lookup first
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            // Check user grammars first (higher priority)
            if let Some(scope) = self.user_extensions.get(ext) {
                if let Some(syntax) = syntect::parsing::Scope::new(scope)
                    .ok()
                    .and_then(|s| self.syntax_set.find_syntax_by_scope(s))
                {
                    return Some(syntax);
                }
            }

            // Try extension lookup (includes embedded grammars like TOML)
            if let Some(syntax) = self.syntax_set.find_syntax_by_extension(ext) {
                return Some(syntax);
            }
        }

        // Try syntect's full file detection (handles special filenames like Makefile)
        // This may do I/O for first-line detection, but handles many cases
        if let Ok(Some(syntax)) = self.syntax_set.find_syntax_for_file(path) {
            return Some(syntax);
        }

        None
    }

    /// Find syntax by first line content (shebang, mode line, etc.)
    ///
    /// Use this when you have the file content but path-based detection failed.
    pub fn find_syntax_by_first_line(&self, first_line: &str) -> Option<&SyntaxReference> {
        self.syntax_set.find_syntax_by_first_line(first_line)
    }

    /// Find syntax by scope name
    pub fn find_syntax_by_scope(&self, scope: &str) -> Option<&SyntaxReference> {
        let scope = syntect::parsing::Scope::new(scope).ok()?;
        self.syntax_set.find_syntax_by_scope(scope)
    }

    /// Find syntax by name
    pub fn find_syntax_by_name(&self, name: &str) -> Option<&SyntaxReference> {
        self.syntax_set.find_syntax_by_name(name)
    }

    /// Get the underlying syntax set
    pub fn syntax_set(&self) -> &Arc<SyntaxSet> {
        &self.syntax_set
    }

    /// Get a clone of the Arc for sharing
    pub fn syntax_set_arc(&self) -> Arc<SyntaxSet> {
        Arc::clone(&self.syntax_set)
    }

    /// List all available syntax names
    pub fn available_syntaxes(&self) -> Vec<&str> {
        self.syntax_set
            .syntaxes()
            .iter()
            .map(|s| s.name.as_str())
            .collect()
    }

    /// Check if a syntax is available for an extension
    pub fn has_syntax_for_extension(&self, ext: &str) -> bool {
        if self.user_extensions.contains_key(ext) {
            return true;
        }

        // Check built-in syntaxes
        let dummy_path = PathBuf::from(format!("file.{}", ext));
        self.syntax_set
            .find_syntax_for_file(&dummy_path)
            .ok()
            .flatten()
            .is_some()
    }
}

impl Default for GrammarRegistry {
    fn default() -> Self {
        Self::load()
    }
}

// VSCode package.json structures

#[derive(Debug, Deserialize)]
struct PackageManifest {
    #[serde(default)]
    contributes: Option<Contributes>,
}

#[derive(Debug, Deserialize, Default)]
struct Contributes {
    #[serde(default)]
    languages: Vec<LanguageContribution>,
    #[serde(default)]
    grammars: Vec<GrammarContribution>,
}

#[derive(Debug, Deserialize)]
struct LanguageContribution {
    id: String,
    #[serde(default)]
    extensions: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct GrammarContribution {
    language: String,
    #[serde(rename = "scopeName")]
    scope_name: String,
    path: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_creation() {
        let registry = GrammarRegistry::load();
        // Should have built-in syntaxes
        assert!(!registry.available_syntaxes().is_empty());
    }

    #[test]
    fn test_find_syntax_for_common_extensions() {
        let registry = GrammarRegistry::load();

        // Test common extensions that syntect should support
        let test_cases = [
            ("test.py", true),
            ("test.rs", true),
            ("test.js", true),
            ("test.json", true),
            ("test.md", true),
            ("test.html", true),
            ("test.css", true),
            ("test.unknown_extension_xyz", false),
        ];

        for (filename, should_exist) in test_cases {
            let path = Path::new(filename);
            let result = registry.find_syntax_for_file(path);
            assert_eq!(
                result.is_some(),
                should_exist,
                "Expected {:?} for {}",
                should_exist,
                filename
            );
        }
    }

    #[test]
    fn test_syntax_set_arc() {
        let registry = GrammarRegistry::load();
        let arc1 = registry.syntax_set_arc();
        let arc2 = registry.syntax_set_arc();
        // Both should point to the same data
        assert!(Arc::ptr_eq(&arc1, &arc2));
    }

    #[test]
    fn test_list_all_syntaxes() {
        let registry = GrammarRegistry::load();
        let syntax_set = registry.syntax_set();

        let mut syntaxes: Vec<_> = syntax_set
            .syntaxes()
            .iter()
            .map(|s| (s.name.as_str(), s.file_extensions.clone()))
            .collect();
        syntaxes.sort_by(|a, b| a.0.cmp(b.0));

        println!("\n=== Available Syntaxes ({} total) ===", syntaxes.len());
        for (name, exts) in &syntaxes {
            println!("  {} -> {:?}", name, exts);
        }

        // Check TypeScript specifically
        println!("\n=== TypeScript Check ===");
        let ts_syntax = syntax_set.find_syntax_by_extension("ts");
        let tsx_syntax = syntax_set.find_syntax_by_extension("tsx");
        println!("  .ts  -> {:?}", ts_syntax.map(|s| &s.name));
        println!("  .tsx -> {:?}", tsx_syntax.map(|s| &s.name));

        // This test always passes - it's for dumping info
        assert!(syntaxes.len() > 0);
    }
}
