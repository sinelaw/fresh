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
    /// Create a new LocalGrammarLoader with default config directory.
    pub fn new() -> Self {
        Self {
            config_dir: dirs::config_dir(),
        }
    }

    /// Create a LocalGrammarLoader with a custom config directory.
    pub fn with_config_dir(config_dir: Option<PathBuf>) -> Self {
        Self { config_dir }
    }
}

impl Default for LocalGrammarLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl GrammarLoader for LocalGrammarLoader {
    fn grammars_dir(&self) -> Option<PathBuf> {
        self.config_dir.as_ref().map(|p| p.join("fresh/grammars"))
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
    /// 3. User-installed grammars from the config directory
    pub fn load(loader: &dyn GrammarLoader) -> Self {
        let mut user_extensions = HashMap::new();

        // Start with syntect defaults, convert to builder to add more
        let defaults = SyntaxSet::load_defaults_newlines();
        let mut builder = defaults.into_builder();

        // Add embedded grammars (TOML, etc.)
        Self::add_embedded_grammars(&mut builder);

        // Add user grammars via loader
        if let Some(grammars_dir) = loader.grammars_dir() {
            if loader.exists(&grammars_dir) {
                load_user_grammars(loader, &grammars_dir, &mut builder, &mut user_extensions);
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
    pub fn for_editor() -> Arc<Self> {
        Arc::new(Self::load(&LocalGrammarLoader::new()))
    }

    /// Get the grammars directory path (convenience method using default loader).
    pub fn grammars_directory() -> Option<PathBuf> {
        LocalGrammarLoader::default().grammars_dir()
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

/// Load a grammar directly from a .tmLanguage.json file.
fn load_direct_grammar(
    loader: &dyn GrammarLoader,
    dir: &Path,
    builder: &mut SyntaxSetBuilder,
    found_any: &mut bool,
) {
    // Look for .tmLanguage.json or .sublime-syntax files
    let entries = match loader.read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for path in entries {
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

        if file_name.ends_with(".tmLanguage.json")
            || file_name.ends_with(".tmLanguage")
            || file_name.ends_with(".sublime-syntax")
        {
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
        let loader = LocalGrammarLoader::new();
        let grammars_dir = loader.grammars_dir();

        // Should return a path if config_dir is available
        // (might be None in some test environments)
        if let Some(dir) = grammars_dir {
            assert!(dir.to_string_lossy().contains("fresh"));
            assert!(dir.to_string_lossy().contains("grammars"));
        }
    }

    #[test]
    fn test_for_editor() {
        let registry = GrammarRegistry::for_editor();
        // Should have built-in syntaxes
        assert!(!registry.available_syntaxes().is_empty());
    }

    #[test]
    fn test_find_syntax_with_custom_languages_config() {
        let registry = GrammarRegistry::for_editor();

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
        let registry = GrammarRegistry::for_editor();
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
