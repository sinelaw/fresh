//! Grammar registry for TextMate grammars
//!
//! This module handles discovery and loading of TextMate grammars from:
//! 1. Built-in syntect grammars (100+ languages)
//! 2. User-installed grammars in ~/.config/fresh/grammars/
//!
//! User grammars use VSCode extension format for compatibility.

use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use syntect::parsing::{SyntaxReference, SyntaxSet, SyntaxSetBuilder};

/// Registry of all available TextMate grammars
pub struct GrammarRegistry {
    /// Combined syntax set (built-in + user grammars)
    syntax_set: Arc<SyntaxSet>,
    /// Extension -> scope name mapping for user grammars (takes priority)
    user_extensions: HashMap<String, String>,
}

impl GrammarRegistry {
    /// Load grammar registry, scanning user grammars directory
    pub fn load() -> Self {
        let mut builder = SyntaxSetBuilder::new();
        let mut user_extensions = HashMap::new();

        // Add built-in syntect grammars
        builder.add_plain_text_syntax();

        // Load default syntect grammars
        let defaults = SyntaxSet::load_defaults_newlines();
        for syntax in defaults.syntaxes() {
            // SyntaxSetBuilder doesn't have a direct way to add from another set,
            // so we'll just use the defaults as our base and add user grammars on top
        }

        // For now, use the defaults directly and add user grammars separately
        // We'll merge them after loading user grammars
        let mut syntax_set = SyntaxSet::load_defaults_newlines();

        // Scan user grammars directory
        if let Some(grammars_dir) = Self::grammars_directory() {
            if grammars_dir.exists() {
                if let Some(user_set) =
                    Self::load_user_grammars(&grammars_dir, &mut user_extensions)
                {
                    // Merge user syntaxes with defaults
                    // Since SyntaxSet doesn't support merging, we need to rebuild
                    let mut builder = SyntaxSetBuilder::new();
                    builder.add_plain_text_syntax();

                    // Add defaults first
                    for syntax in syntax_set.syntaxes() {
                        // We can't directly add syntaxes, so we'll keep defaults as base
                    }

                    // For user grammars, we store the mappings and load them into a separate set
                    // The find_syntax_for_file will check user_extensions first
                    syntax_set = Self::merge_syntax_sets(syntax_set, user_set);
                }
            }
        }

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

    /// Get the grammars directory path
    pub fn grammars_directory() -> Option<PathBuf> {
        dirs::config_dir().map(|p| p.join("fresh/grammars"))
    }

    /// Merge two syntax sets (user grammars override defaults)
    fn merge_syntax_sets(base: SyntaxSet, user: SyntaxSet) -> SyntaxSet {
        // syntect doesn't have a great API for merging, so we use a builder
        let mut builder = SyntaxSetBuilder::new();
        builder.add_plain_text_syntax();

        // Unfortunately, SyntaxSetBuilder doesn't let us add from existing SyntaxSet
        // directly. The best approach is to load from folders.
        // For now, we'll just use the base set and rely on user_extensions mapping
        // to find user grammars by scope name.

        // In a more complete implementation, we'd serialize user syntaxes and reload
        base
    }

    /// Load user grammars from the grammars directory
    fn load_user_grammars(
        dir: &Path,
        user_extensions: &mut HashMap<String, String>,
    ) -> Option<SyntaxSet> {
        let mut builder = SyntaxSetBuilder::new();
        let mut found_any = false;

        // Iterate through subdirectories looking for package.json
        let entries = match std::fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(e) => {
                tracing::warn!("Failed to read grammars directory {:?}: {}", dir, e);
                return None;
            }
        };

        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }

            let package_json = path.join("package.json");
            if !package_json.exists() {
                // Also check for direct .tmLanguage.json files
                Self::load_direct_grammar(&path, &mut builder, user_extensions, &mut found_any);
                continue;
            }

            // Parse package.json
            match Self::parse_package_json(&package_json) {
                Ok(manifest) => {
                    Self::process_manifest(&path, manifest, &mut builder, user_extensions);
                    found_any = true;
                }
                Err(e) => {
                    tracing::warn!("Failed to parse {:?}: {}", package_json, e);
                }
            }
        }

        if found_any {
            Some(builder.build())
        } else {
            None
        }
    }

    /// Load a grammar directly from a .tmLanguage.json file
    fn load_direct_grammar(
        dir: &Path,
        builder: &mut SyntaxSetBuilder,
        user_extensions: &mut HashMap<String, String>,
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

    /// Find syntax for a file by path/extension
    pub fn find_syntax_for_file(&self, path: &Path) -> Option<&SyntaxReference> {
        let ext = path.extension()?.to_str()?;

        // Check user grammars first (higher priority)
        if let Some(scope) = self.user_extensions.get(ext) {
            if let Some(syntax) = self
                .syntax_set
                .find_syntax_by_scope(syntect::parsing::Scope::new(scope).ok()?)
            {
                return Some(syntax);
            }
        }

        // Fall back to built-in syntect detection
        self.syntax_set.find_syntax_for_file(path).ok().flatten()
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
    name: Option<String>,
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
    #[serde(default)]
    aliases: Vec<String>,
    #[serde(default)]
    filenames: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct GrammarContribution {
    language: String,
    #[serde(rename = "scopeName")]
    scope_name: String,
    path: String,
    #[serde(default)]
    embedded_languages: HashMap<String, String>,
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
