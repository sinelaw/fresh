//! Grammar loading with I/O abstraction.
//!
//! This module provides the `GrammarLoader` trait for loading grammars from various sources,
//! and `LocalGrammarLoader` as the default filesystem-based implementation.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use syntect::parsing::{SyntaxSet, SyntaxSetBuilder};

use super::types::{GrammarRegistry, PackageManifest};

/// Trait for loading grammar files from various sources.
///
/// This abstraction allows:
/// - Testing with mock implementations
/// - WASM builds with fetch-based loaders
/// - Custom grammar sources (network, embedded, etc.)
pub trait GrammarLoader: Send + Sync {
    /// Get the user grammars directory path.
    fn grammars_dir(&self) -> Option<PathBuf>;

    /// Get the language packages directory path (installed via pkg manager).
    fn languages_packages_dir(&self) -> Option<PathBuf>;

    /// Read file contents as string.
    fn read_file(&self, path: &Path) -> io::Result<String>;

    /// List entries in a directory.
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;

    /// Check if path exists.
    fn exists(&self, path: &Path) -> bool;

    /// Check if path is a directory.
    fn is_dir(&self, path: &Path) -> bool;
}

/// Default implementation using local filesystem.
pub struct LocalGrammarLoader {
    config_dir: Option<PathBuf>,
}

impl LocalGrammarLoader {
    /// Create a LocalGrammarLoader with the given config directory.
    pub fn new(config_dir: PathBuf) -> Self {
        Self {
            config_dir: Some(config_dir),
        }
    }

    /// Create a LocalGrammarLoader with no config directory (embedded grammars only).
    pub fn embedded_only() -> Self {
        Self { config_dir: None }
    }
}

impl GrammarLoader for LocalGrammarLoader {
    fn grammars_dir(&self) -> Option<PathBuf> {
        self.config_dir.as_ref().map(|p| p.join("grammars"))
    }

    fn languages_packages_dir(&self) -> Option<PathBuf> {
        self.config_dir
            .as_ref()
            .map(|p| p.join("languages/packages"))
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        for entry in std::fs::read_dir(path)? {
            entries.push(entry?.path());
        }
        Ok(entries)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }
}

// Builder/factory methods that use GrammarLoader
impl GrammarRegistry {
    /// Load grammar registry using a GrammarLoader.
    ///
    /// This loads:
    /// 1. Built-in syntect grammars
    /// 2. Embedded grammars (TOML, Odin, etc.)
    /// 3. User-installed grammars from ~/.config/fresh/grammars/
    /// 4. Language pack grammars from ~/.config/fresh/languages/packages/
    pub fn load(loader: &dyn GrammarLoader) -> Self {
        let mut user_extensions = HashMap::new();

        // Start with syntect defaults, convert to builder to add more
        let defaults = SyntaxSet::load_defaults_newlines();
        let mut builder = defaults.into_builder();

        // Add embedded grammars (TOML, etc.)
        Self::add_embedded_grammars(&mut builder);

        // Add user grammars from ~/.config/fresh/grammars/
        if let Some(grammars_dir) = loader.grammars_dir() {
            if loader.exists(&grammars_dir) {
                load_user_grammars(loader, &grammars_dir, &mut builder, &mut user_extensions);
            }
        }

        // Add language pack grammars from ~/.config/fresh/languages/packages/
        if let Some(packages_dir) = loader.languages_packages_dir() {
            if loader.exists(&packages_dir) {
                load_language_pack_grammars(
                    loader,
                    &packages_dir,
                    &mut builder,
                    &mut user_extensions,
                );
            }
        }

        let syntax_set = builder.build();
        let filename_scopes = Self::build_filename_scopes();

        tracing::info!(
            "Loaded {} syntaxes, {} user extension mappings, {} filename mappings",
            syntax_set.syntaxes().len(),
            user_extensions.len(),
            filename_scopes.len()
        );

        Self::new(syntax_set, user_extensions, filename_scopes)
    }

    /// Create a fully-loaded grammar registry for the editor.
    /// Uses LocalGrammarLoader to load grammars from the filesystem.
    pub fn for_editor(config_dir: std::path::PathBuf) -> Arc<Self> {
        Arc::new(Self::load(&LocalGrammarLoader::new(config_dir)))
    }

    /// Get the grammars directory path for the given config directory.
    pub fn grammars_directory(config_dir: &std::path::Path) -> PathBuf {
        config_dir.join("grammars")
    }
}

/// Load user grammars from a directory using the provided loader.
fn load_user_grammars(
    loader: &dyn GrammarLoader,
    dir: &Path,
    builder: &mut SyntaxSetBuilder,
    user_extensions: &mut HashMap<String, String>,
) {
    // Iterate through subdirectories looking for package.json or direct grammar files
    let entries = match loader.read_dir(dir) {
        Ok(entries) => entries,
        Err(e) => {
            tracing::warn!("Failed to read grammars directory {:?}: {}", dir, e);
            return;
        }
    };

    for path in entries {
        if !loader.is_dir(&path) {
            continue;
        }

        // Check for package.json (VSCode extension format)
        let manifest_path = path.join("package.json");
        if loader.exists(&manifest_path) {
            if let Ok(manifest) = parse_package_json(loader, &manifest_path) {
                process_manifest(loader, &path, manifest, builder, user_extensions);
            }
            continue;
        }

        // Check for direct grammar files
        let mut found_any = false;
        load_direct_grammar(loader, &path, builder, &mut found_any);
    }
}

/// Parse a VSCode package.json manifest using the loader.
fn parse_package_json(loader: &dyn GrammarLoader, path: &Path) -> Result<PackageManifest, String> {
    let content = loader
        .read_file(path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    serde_json::from_str(&content).map_err(|e| format!("Failed to parse JSON: {}", e))
}

/// Process a package manifest and load its grammars.
fn process_manifest(
    loader: &dyn GrammarLoader,
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

        if !loader.exists(&grammar_path) {
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

/// Load a grammar directly from a .sublime-syntax or .tmLanguage file.
fn load_direct_grammar(
    loader: &dyn GrammarLoader,
    dir: &Path,
    builder: &mut SyntaxSetBuilder,
    found_any: &mut bool,
) {
    // Look for .sublime-syntax or .tmLanguage files
    let entries = match loader.read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for path in entries {
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        if file_name.ends_with(".tmLanguage") || file_name.ends_with(".sublime-syntax") {
            if let Err(e) = builder.add_from_folder(dir, false) {
                tracing::warn!("Failed to load grammar from {:?}: {}", dir, e);
            } else {
                tracing::info!("Loaded grammar from {:?}", dir);
                *found_any = true;
            }
            break;
        }
    }
}

/// Fresh-specific language pack manifest format
#[derive(Debug, serde::Deserialize)]
struct FreshPackageManifest {
    name: String,
    #[serde(default)]
    fresh: Option<FreshConfig>,
}

#[derive(Debug, serde::Deserialize)]
struct FreshConfig {
    #[serde(default)]
    grammar: Option<FreshGrammarConfig>,
}

#[derive(Debug, serde::Deserialize)]
struct FreshGrammarConfig {
    file: String,
    #[serde(default)]
    extensions: Vec<String>,
}

/// Load grammars from Fresh language packages (installed via pkg manager).
///
/// These packages use a Fresh-specific package.json format with:
/// ```json
/// {
///   "name": "hare",
///   "fresh": {
///     "grammar": {
///       "file": "grammars/Hare.sublime-syntax",
///       "extensions": ["ha"]
///     }
///   }
/// }
/// ```
fn load_language_pack_grammars(
    loader: &dyn GrammarLoader,
    packages_dir: &Path,
    builder: &mut SyntaxSetBuilder,
    user_extensions: &mut HashMap<String, String>,
) {
    let entries = match loader.read_dir(packages_dir) {
        Ok(entries) => entries,
        Err(e) => {
            tracing::debug!(
                "Failed to read language packages directory {:?}: {}",
                packages_dir,
                e
            );
            return;
        }
    };

    for package_path in entries {
        if !loader.is_dir(&package_path) {
            continue;
        }

        let manifest_path = package_path.join("package.json");
        if !loader.exists(&manifest_path) {
            continue;
        }

        // Try to parse as Fresh language pack format
        let content = match loader.read_file(&manifest_path) {
            Ok(c) => c,
            Err(e) => {
                tracing::debug!("Failed to read {:?}: {}", manifest_path, e);
                continue;
            }
        };

        let manifest: FreshPackageManifest = match serde_json::from_str(&content) {
            Ok(m) => m,
            Err(e) => {
                tracing::debug!("Failed to parse {:?}: {}", manifest_path, e);
                continue;
            }
        };

        // Check for Fresh grammar config
        let grammar_config = match manifest.fresh.and_then(|f| f.grammar) {
            Some(g) => g,
            None => continue,
        };

        let grammar_path = package_path.join(&grammar_config.file);
        if !loader.exists(&grammar_path) {
            tracing::warn!(
                "Grammar file not found for language pack '{}': {:?}",
                manifest.name,
                grammar_path
            );
            continue;
        }

        // Load the grammar file
        let content = match loader.read_file(&grammar_path) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!("Failed to read grammar file {:?}: {}", grammar_path, e);
                continue;
            }
        };

        // Parse and add the syntax
        match syntect::parsing::SyntaxDefinition::load_from_str(
            &content,
            true,
            grammar_path.file_stem().and_then(|s| s.to_str()),
        ) {
            Ok(syntax) => {
                let scope = syntax.scope.to_string();
                tracing::info!(
                    "Loaded language pack grammar '{}' from {:?} (scope: {}, extensions: {:?})",
                    manifest.name,
                    grammar_path,
                    scope,
                    grammar_config.extensions
                );
                builder.add(syntax);

                // Map extensions to scope
                for ext in &grammar_config.extensions {
                    let ext_clean = ext.trim_start_matches('.');
                    user_extensions.insert(ext_clean.to_string(), scope.clone());
                }
            }
            Err(e) => {
                tracing::warn!(
                    "Failed to parse grammar for language pack '{}': {}",
                    manifest.name,
                    e
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Mock grammar loader for testing
    struct MockGrammarLoader {
        grammars_dir: Option<PathBuf>,
        files: HashMap<PathBuf, String>,
        dirs: HashMap<PathBuf, Vec<PathBuf>>,
    }

    impl MockGrammarLoader {
        fn new() -> Self {
            Self {
                grammars_dir: None,
                files: HashMap::new(),
                dirs: HashMap::new(),
            }
        }

        #[allow(dead_code)]
        fn with_grammars_dir(mut self, dir: PathBuf) -> Self {
            self.grammars_dir = Some(dir);
            self
        }
    }

    impl GrammarLoader for MockGrammarLoader {
        fn grammars_dir(&self) -> Option<PathBuf> {
            self.grammars_dir.clone()
        }

        fn languages_packages_dir(&self) -> Option<PathBuf> {
            None // Not used in current tests
        }

        fn read_file(&self, path: &Path) -> io::Result<String> {
            self.files
                .get(path)
                .cloned()
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "File not found"))
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            self.dirs
                .get(path)
                .cloned()
                .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Directory not found"))
        }

        fn exists(&self, path: &Path) -> bool {
            self.files.contains_key(path) || self.dirs.contains_key(path)
        }

        fn is_dir(&self, path: &Path) -> bool {
            self.dirs.contains_key(path)
        }
    }

    #[test]
    fn test_mock_loader_no_grammars() {
        let loader = MockGrammarLoader::new();
        let registry = GrammarRegistry::load(&loader);

        // Should still have built-in syntaxes
        assert!(!registry.available_syntaxes().is_empty());
    }

    #[test]
    fn test_local_loader_grammars_dir() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_dir = temp_dir.path().to_path_buf();
        let loader = LocalGrammarLoader::new(config_dir.clone());
        let grammars_dir = loader.grammars_dir();

        // Should return the grammars subdirectory
        assert!(grammars_dir.is_some());
        let dir = grammars_dir.unwrap();
        assert_eq!(dir, config_dir.join("grammars"));
    }

    #[test]
    fn test_for_editor() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_dir = temp_dir.path().to_path_buf();
        let registry = GrammarRegistry::for_editor(config_dir);
        // Should have built-in syntaxes
        assert!(!registry.available_syntaxes().is_empty());
    }

    #[test]
    fn test_find_syntax_with_custom_languages_config() {
        let temp_dir = tempfile::tempdir().unwrap();
        let registry = GrammarRegistry::for_editor(temp_dir.path().to_path_buf());

        // Create a custom languages config that maps "custom.myext" files to bash
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "bash".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["myext".to_string()],
                filenames: vec!["CUSTOMBUILD".to_string()],
                grammar: "Bourne Again Shell (bash)".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: false,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
            },
        );

        // Test that custom filename is detected via languages config
        let path = Path::new("CUSTOMBUILD");
        let result = registry.find_syntax_for_file_with_languages(path, &languages);
        assert!(
            result.is_some(),
            "CUSTOMBUILD should be detected via languages config"
        );
        let syntax = result.unwrap();
        assert!(
            syntax.name.to_lowercase().contains("bash")
                || syntax.name.to_lowercase().contains("shell"),
            "CUSTOMBUILD should be detected as shell/bash, got: {}",
            syntax.name
        );

        // Test that custom extension is detected via languages config
        let path = Path::new("script.myext");
        let result = registry.find_syntax_for_file_with_languages(path, &languages);
        assert!(
            result.is_some(),
            "script.myext should be detected via languages config"
        );
        let syntax = result.unwrap();
        assert!(
            syntax.name.to_lowercase().contains("bash")
                || syntax.name.to_lowercase().contains("shell"),
            "script.myext should be detected as shell/bash, got: {}",
            syntax.name
        );
    }

    #[test]
    fn test_list_all_syntaxes() {
        let temp_dir = tempfile::tempdir().unwrap();
        let registry = GrammarRegistry::for_editor(temp_dir.path().to_path_buf());
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
        assert!(!syntaxes.is_empty());
    }
}
