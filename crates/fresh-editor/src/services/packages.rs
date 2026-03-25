//! Package manifest types and startup package scanning.
//!
//! This module handles loading installed packages (language packs, bundles) at
//! startup from Rust, replacing the JS-based `loadInstalledPackages()` in the
//! pkg plugin. This eliminates async grammar rebuilds from plugin callbacks.

use std::path::{Path, PathBuf};

use schemars::JsonSchema;
use serde::Deserialize;

use crate::config::{FormatterConfig, LanguageConfig};
use crate::primitives::grammar::GrammarSpec;
use crate::types::{LspServerConfig, ProcessLimits};

// ── Manifest types ──────────────────────────────────────────────────────

/// Top-level package.json manifest for Fresh packages.
///
/// Matches the schema in `plugins/schemas/package.schema.json`.
/// All optional fields use `#[serde(default)]` so that unknown or missing
/// fields are silently ignored — ensuring forward compatibility.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct PackageManifest {
    /// Package name (lowercase, hyphens allowed)
    pub name: String,

    /// Semantic version
    #[serde(default)]
    pub version: Option<String>,

    /// Short package description
    #[serde(default)]
    pub description: Option<String>,

    /// Package type
    #[serde(rename = "type", default)]
    pub package_type: Option<PackageType>,

    /// Fresh-specific configuration
    #[serde(default)]
    pub fresh: Option<FreshManifestConfig>,

    /// Author name
    #[serde(default)]
    pub author: Option<String>,

    /// SPDX license identifier
    #[serde(default)]
    pub license: Option<String>,

    /// Git repository URL
    #[serde(default)]
    pub repository: Option<String>,

    /// Search keywords
    #[serde(default)]
    pub keywords: Vec<String>,
}

/// Package type discriminator.
#[derive(Debug, Clone, Deserialize, JsonSchema, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum PackageType {
    Plugin,
    Theme,
    ThemePack,
    Language,
    Bundle,
}

/// The `fresh` configuration block inside a package manifest.
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
pub struct FreshManifestConfig {
    /// Minimum required Fresh version
    #[serde(default)]
    pub min_version: Option<String>,

    /// Minimum required plugin API version
    #[serde(default)]
    pub min_api_version: Option<u32>,

    /// Plugin entry point file
    #[serde(default)]
    pub entry: Option<String>,

    /// Plugin entry point file (alias for entry)
    #[serde(default)]
    pub main: Option<String>,

    /// Single theme JSON file path (for theme packages)
    #[serde(default)]
    pub theme: Option<String>,

    /// Theme definitions (for theme packs and bundles)
    #[serde(default)]
    pub themes: Vec<BundleTheme>,

    /// JSON Schema for plugin configuration options
    #[serde(default)]
    pub config_schema: Option<serde_json::Value>,

    /// Grammar configuration (for language packs)
    #[serde(default)]
    pub grammar: Option<GrammarManifestConfig>,

    /// Language configuration (for language packs)
    #[serde(default)]
    pub language: Option<LanguageManifestConfig>,

    /// LSP server configuration (for language packs)
    #[serde(default)]
    pub lsp: Option<LspManifestConfig>,

    /// Language definitions (for bundles)
    #[serde(default)]
    pub languages: Vec<BundleLanguage>,

    /// Plugin definitions (for bundles)
    #[serde(default)]
    pub plugins: Vec<BundlePlugin>,
}

/// Grammar file configuration within a package manifest.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct GrammarManifestConfig {
    /// Path to grammar file (.sublime-syntax or .tmLanguage), relative to package
    pub file: String,

    /// File extensions this grammar handles (e.g., ["rs", "rust"])
    #[serde(default)]
    pub extensions: Vec<String>,

    /// Regex pattern for shebang/first-line detection
    #[serde(rename = "firstLine", default)]
    pub first_line: Option<String>,
}

/// Language configuration within a package manifest (camelCase to match JSON schema).
#[derive(Debug, Clone, Default, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct LanguageManifestConfig {
    /// Line comment prefix (e.g., "//" or "#")
    #[serde(default)]
    pub comment_prefix: Option<String>,

    /// Block comment start marker (e.g., "/*")
    #[serde(default)]
    pub block_comment_start: Option<String>,

    /// Block comment end marker (e.g., "*/")
    #[serde(default)]
    pub block_comment_end: Option<String>,

    /// Default tab size for this language
    #[serde(default)]
    pub tab_size: Option<usize>,

    /// Use tabs instead of spaces
    #[serde(default)]
    pub use_tabs: Option<bool>,

    /// Enable automatic indentation
    #[serde(default)]
    pub auto_indent: Option<bool>,

    /// Whether to show whitespace tab indicators
    #[serde(default)]
    pub show_whitespace_tabs: Option<bool>,

    /// Formatter configuration
    #[serde(default)]
    pub formatter: Option<FormatterManifestConfig>,
}

/// Formatter configuration within a package manifest.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct FormatterManifestConfig {
    /// Formatter command (e.g., "rustfmt", "prettier")
    pub command: String,

    /// Arguments to pass to the formatter
    #[serde(default)]
    pub args: Vec<String>,
}

/// LSP server configuration within a package manifest (camelCase to match JSON schema).
#[derive(Debug, Clone, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct LspManifestConfig {
    /// LSP server command
    pub command: String,

    /// Arguments to pass to the server
    #[serde(default)]
    pub args: Vec<String>,

    /// Auto-start the server when a matching file is opened
    #[serde(default)]
    pub auto_start: Option<bool>,

    /// LSP initialization options
    #[serde(default)]
    pub initialization_options: Option<serde_json::Value>,

    /// Process resource limits
    #[serde(default)]
    pub process_limits: Option<ProcessLimitsManifestConfig>,
}

/// Process limits within a package manifest (camelCase to match JSON schema).
#[derive(Debug, Clone, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct ProcessLimitsManifestConfig {
    #[serde(default)]
    pub max_memory_percent: Option<u32>,
    #[serde(default)]
    pub max_cpu_percent: Option<u32>,
    #[serde(default)]
    pub enabled: Option<bool>,
}

/// A language entry within a bundle manifest.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct BundleLanguage {
    /// Language identifier (e.g., "elixir", "heex")
    pub id: String,

    /// Grammar configuration for this language
    #[serde(default)]
    pub grammar: Option<GrammarManifestConfig>,

    /// Language configuration for this language
    #[serde(default)]
    pub language: Option<LanguageManifestConfig>,

    /// LSP server configuration for this language
    #[serde(default)]
    pub lsp: Option<LspManifestConfig>,
}

/// A plugin entry within a bundle manifest.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct BundlePlugin {
    /// Plugin entry point file relative to package
    pub entry: String,
}

/// A theme entry within a package manifest.
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct BundleTheme {
    /// Theme JSON file path relative to package
    pub file: String,

    /// Display name for the theme
    pub name: String,

    /// Theme variant (dark or light)
    #[serde(default)]
    pub variant: Option<ThemeVariant>,
}

/// Theme variant (dark or light).
#[derive(Debug, Clone, Deserialize, JsonSchema)]
#[serde(rename_all = "lowercase")]
pub enum ThemeVariant {
    Dark,
    Light,
}

// ── Conversion helpers ──────────────────────────────────────────────────

impl LanguageManifestConfig {
    /// Convert to the internal `LanguageConfig` used by the editor.
    pub fn to_language_config(&self) -> LanguageConfig {
        LanguageConfig {
            comment_prefix: self.comment_prefix.clone(),
            auto_indent: self.auto_indent.unwrap_or(true),
            show_whitespace_tabs: self.show_whitespace_tabs.unwrap_or(true),
            use_tabs: self.use_tabs,
            tab_size: self.tab_size,
            formatter: self.formatter.as_ref().map(|f| FormatterConfig {
                command: f.command.clone(),
                args: f.args.clone(),
                stdin: true,
                timeout_ms: 10000,
            }),
            ..Default::default()
        }
    }
}

impl LspManifestConfig {
    /// Convert to the internal `LspServerConfig` used by the editor.
    pub fn to_lsp_config(&self) -> LspServerConfig {
        let process_limits = self
            .process_limits
            .as_ref()
            .map(|pl| ProcessLimits {
                max_memory_percent: pl.max_memory_percent,
                max_cpu_percent: pl.max_cpu_percent,
                enabled: pl
                    .enabled
                    .unwrap_or(pl.max_memory_percent.is_some() || pl.max_cpu_percent.is_some()),
            })
            .unwrap_or_default();

        LspServerConfig {
            command: self.command.clone(),
            args: self.args.clone(),
            enabled: true,
            auto_start: self.auto_start.unwrap_or(true),
            initialization_options: self.initialization_options.clone(),
            process_limits,
            ..Default::default()
        }
    }
}

// ── Package scanner ─────────────────────────────────────────────────────

/// Results of scanning installed packages at startup.
#[derive(Debug, Default)]
pub struct PackageScanResult {
    /// Language configs to insert into Config.languages (package defaults)
    pub language_configs: Vec<(String, LanguageConfig)>,
    /// LSP configs to apply (package defaults)
    pub lsp_configs: Vec<(String, LspServerConfig)>,
    /// Additional grammar files for the background build
    pub additional_grammars: Vec<GrammarSpec>,
    /// Bundle plugin directories to add to the plugin loading list
    pub bundle_plugin_dirs: Vec<PathBuf>,
    /// Bundle theme directories (for theme loader to scan)
    pub bundle_theme_dirs: Vec<PathBuf>,
}

/// Scan all installed packages and collect configs, grammars, plugin dirs, and theme dirs.
///
/// This replaces the JS `loadInstalledPackages()` function, running synchronously
/// during editor startup before plugin loading. The scan covers:
/// - `languages/packages/` — language packs with grammar, language config, LSP config
/// - `bundles/packages/` — bundles with multiple languages, plugins, and themes
pub fn scan_installed_packages(config_dir: &Path) -> PackageScanResult {
    let mut result = PackageScanResult::default();

    // Scan language packs
    let languages_dir = config_dir.join("languages/packages");
    if languages_dir.is_dir() {
        scan_language_packs(&languages_dir, &mut result);
    }

    // Scan bundles
    let bundles_dir = config_dir.join("bundles/packages");
    if bundles_dir.is_dir() {
        scan_bundles(&bundles_dir, &mut result);
    }

    tracing::info!(
        "[package-scan] Found {} language configs, {} LSP configs, {} grammars, {} bundle plugin dirs, {} bundle theme dirs",
        result.language_configs.len(),
        result.lsp_configs.len(),
        result.additional_grammars.len(),
        result.bundle_plugin_dirs.len(),
        result.bundle_theme_dirs.len(),
    );

    result
}

/// Scan language packs from `languages/packages/`.
fn scan_language_packs(dir: &Path, result: &mut PackageScanResult) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            tracing::debug!("[package-scan] Failed to read {:?}: {}", dir, e);
            return;
        }
    };

    for entry in entries.flatten() {
        let pkg_dir = entry.path();
        if !pkg_dir.is_dir() {
            continue;
        }
        let manifest_path = pkg_dir.join("package.json");
        if let Some(manifest) = read_manifest(&manifest_path) {
            process_language_pack(&pkg_dir, &manifest, result);
        }
    }
}

/// Process a single language pack manifest.
fn process_language_pack(_pkg_dir: &Path, manifest: &PackageManifest, result: &mut PackageScanResult) {
    let fresh = match &manifest.fresh {
        Some(f) => f,
        None => return,
    };

    let lang_id = manifest.name.clone();

    // Grammar (note: the grammar loader already handles languages/packages/ grammars
    // via load_language_pack_grammars, so we don't add them to additional_grammars here)

    // Language config
    if let Some(lang_config) = &fresh.language {
        result
            .language_configs
            .push((lang_id.clone(), lang_config.to_language_config()));
    }

    // LSP config
    if let Some(lsp_config) = &fresh.lsp {
        result
            .lsp_configs
            .push((lang_id.clone(), lsp_config.to_lsp_config()));
    }
}

/// Scan bundles from `bundles/packages/`.
fn scan_bundles(dir: &Path, result: &mut PackageScanResult) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            tracing::debug!("[package-scan] Failed to read {:?}: {}", dir, e);
            return;
        }
    };

    for entry in entries.flatten() {
        let pkg_dir = entry.path();
        if !pkg_dir.is_dir() {
            continue;
        }
        let manifest_path = pkg_dir.join("package.json");
        if let Some(manifest) = read_manifest(&manifest_path) {
            process_bundle(&pkg_dir, &manifest, result);
        }
    }
}

/// Process a single bundle manifest.
fn process_bundle(pkg_dir: &Path, manifest: &PackageManifest, result: &mut PackageScanResult) {
    let fresh = match &manifest.fresh {
        Some(f) => f,
        None => return,
    };

    // Process each language in the bundle
    for lang in &fresh.languages {
        // Grammar
        if let Some(grammar) = &lang.grammar {
            let grammar_path = pkg_dir.join(&grammar.file);
            if grammar_path.exists() {
                result.additional_grammars.push(GrammarSpec {
                    language: lang.id.clone(),
                    path: grammar_path,
                    extensions: grammar.extensions.clone(),
                });
            } else {
                tracing::warn!(
                    "[package-scan] Grammar file not found for '{}' in bundle '{}': {:?}",
                    lang.id,
                    manifest.name,
                    grammar_path
                );
            }
        }

        // Language config
        if let Some(lang_config) = &lang.language {
            result
                .language_configs
                .push((lang.id.clone(), lang_config.to_language_config()));
        }

        // LSP config
        if let Some(lsp_config) = &lang.lsp {
            result
                .lsp_configs
                .push((lang.id.clone(), lsp_config.to_lsp_config()));
        }
    }

    // Bundle plugins
    for plugin in &fresh.plugins {
        let entry_path = pkg_dir.join(&plugin.entry);
        // The plugin loader expects the directory, not the entry file
        if let Some(plugin_dir) = entry_path.parent() {
            if plugin_dir.is_dir() {
                result.bundle_plugin_dirs.push(plugin_dir.to_path_buf());
            }
        }
    }

    // Bundle themes — record the bundle directory for the theme loader to scan
    if !fresh.themes.is_empty() {
        result.bundle_theme_dirs.push(pkg_dir.to_path_buf());
    }
}

/// Read and parse a package.json manifest, returning None on any error.
fn read_manifest(path: &Path) -> Option<PackageManifest> {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            tracing::debug!("[package-scan] Failed to read {:?}: {}", path, e);
            return None;
        }
    };

    match serde_json::from_str(&content) {
        Ok(m) => Some(m),
        Err(e) => {
            tracing::warn!("[package-scan] Failed to parse {:?}: {}", path, e);
            None
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_language_pack_manifest() {
        let json = r#"{
            "name": "hare",
            "version": "1.0.0",
            "description": "Hare language support",
            "type": "language",
            "fresh": {
                "grammar": {
                    "file": "grammars/Hare.sublime-syntax",
                    "extensions": ["ha"]
                },
                "language": {
                    "commentPrefix": "//",
                    "useTabs": true,
                    "tabSize": 8,
                    "showWhitespaceTabs": false,
                    "autoIndent": true
                },
                "lsp": {
                    "command": "hare-lsp",
                    "args": ["--stdio"],
                    "autoStart": true
                }
            }
        }"#;

        let manifest: PackageManifest = serde_json::from_str(json).unwrap();
        assert_eq!(manifest.name, "hare");
        assert_eq!(manifest.package_type, Some(PackageType::Language));

        let fresh = manifest.fresh.unwrap();
        let grammar = fresh.grammar.unwrap();
        assert_eq!(grammar.file, "grammars/Hare.sublime-syntax");
        assert_eq!(grammar.extensions, vec!["ha"]);

        let lang = fresh.language.unwrap();
        assert_eq!(lang.comment_prefix, Some("//".to_string()));
        assert_eq!(lang.use_tabs, Some(true));
        assert_eq!(lang.tab_size, Some(8));
        assert_eq!(lang.show_whitespace_tabs, Some(false));

        // Verify conversion to LanguageConfig
        let lang_config = lang.to_language_config();
        assert_eq!(lang_config.comment_prefix, Some("//".to_string()));
        assert_eq!(lang_config.use_tabs, Some(true));
        assert_eq!(lang_config.tab_size, Some(8));
        assert!(!lang_config.show_whitespace_tabs);

        let lsp = fresh.lsp.unwrap();
        assert_eq!(lsp.command, "hare-lsp");
        assert_eq!(lsp.args, vec!["--stdio"]);
        assert_eq!(lsp.auto_start, Some(true));

        // Verify conversion to LspServerConfig
        let lsp_config = lsp.to_lsp_config();
        assert_eq!(lsp_config.command, "hare-lsp");
        assert!(lsp_config.auto_start);
        assert!(lsp_config.enabled);
    }

    #[test]
    fn test_parse_bundle_manifest() {
        let json = r##"{
            "name": "elixir-bundle",
            "version": "1.0.0",
            "description": "Elixir language bundle",
            "type": "bundle",
            "fresh": {
                "languages": [
                    {
                        "id": "elixir",
                        "grammar": {
                            "file": "grammars/Elixir.sublime-syntax",
                            "extensions": ["ex", "exs"]
                        },
                        "language": {
                            "commentPrefix": "#",
                            "tabSize": 2
                        },
                        "lsp": {
                            "command": "elixir-ls",
                            "autoStart": true
                        }
                    },
                    {
                        "id": "heex",
                        "grammar": {
                            "file": "grammars/HEEx.sublime-syntax",
                            "extensions": ["heex"]
                        }
                    }
                ],
                "plugins": [
                    { "entry": "plugins/elixir-plugin.ts" }
                ],
                "themes": [
                    { "file": "themes/elixir-dark.json", "name": "Elixir Dark", "variant": "dark" }
                ]
            }
        }"##;

        let manifest: PackageManifest = serde_json::from_str(json).unwrap();
        assert_eq!(manifest.name, "elixir-bundle");
        assert_eq!(manifest.package_type, Some(PackageType::Bundle));

        let fresh = manifest.fresh.unwrap();
        assert_eq!(fresh.languages.len(), 2);
        assert_eq!(fresh.plugins.len(), 1);
        assert_eq!(fresh.themes.len(), 1);

        let elixir = &fresh.languages[0];
        assert_eq!(elixir.id, "elixir");
        assert_eq!(
            elixir.grammar.as_ref().unwrap().extensions,
            vec!["ex", "exs"]
        );

        let heex = &fresh.languages[1];
        assert_eq!(heex.id, "heex");
        assert!(heex.language.is_none());
        assert!(heex.lsp.is_none());
    }

    #[test]
    fn test_parse_minimal_manifest() {
        // Only required field is `name` — everything else should have defaults
        let json = r#"{ "name": "minimal" }"#;
        let manifest: PackageManifest = serde_json::from_str(json).unwrap();
        assert_eq!(manifest.name, "minimal");
        assert!(manifest.package_type.is_none());
        assert!(manifest.fresh.is_none());
    }

    #[test]
    fn test_parse_manifest_with_unknown_fields() {
        // Forward compatibility: unknown fields should be silently ignored
        let json = r#"{
            "name": "future-pkg",
            "version": "2.0.0",
            "description": "From the future",
            "type": "language",
            "future_field": true,
            "fresh": {
                "grammar": { "file": "grammar.sublime-syntax" },
                "future_nested": { "key": "value" }
            }
        }"#;
        let manifest: PackageManifest = serde_json::from_str(json).unwrap();
        assert_eq!(manifest.name, "future-pkg");
        assert!(manifest.fresh.unwrap().grammar.is_some());
    }

    #[test]
    fn test_scan_empty_directories() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_dir = temp_dir.path();

        let result = scan_installed_packages(config_dir);
        assert!(result.language_configs.is_empty());
        assert!(result.lsp_configs.is_empty());
        assert!(result.additional_grammars.is_empty());
        assert!(result.bundle_plugin_dirs.is_empty());
        assert!(result.bundle_theme_dirs.is_empty());
    }

    #[test]
    fn test_scan_language_pack() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_dir = temp_dir.path();

        // Create a language pack
        let lang_dir = config_dir.join("languages/packages/hare");
        std::fs::create_dir_all(&lang_dir).unwrap();
        std::fs::write(
            lang_dir.join("package.json"),
            r#"{
                "name": "hare",
                "version": "1.0.0",
                "description": "Hare language",
                "type": "language",
                "fresh": {
                    "grammar": {
                        "file": "grammars/Hare.sublime-syntax",
                        "extensions": ["ha"]
                    },
                    "language": {
                        "commentPrefix": "//",
                        "useTabs": true
                    },
                    "lsp": {
                        "command": "hare-lsp",
                        "args": ["--stdio"]
                    }
                }
            }"#,
        )
        .unwrap();

        let result = scan_installed_packages(config_dir);

        // Language config should be extracted
        assert_eq!(result.language_configs.len(), 1);
        assert_eq!(result.language_configs[0].0, "hare");
        assert_eq!(
            result.language_configs[0].1.comment_prefix,
            Some("//".to_string())
        );
        assert_eq!(result.language_configs[0].1.use_tabs, Some(true));

        // LSP config should be extracted
        assert_eq!(result.lsp_configs.len(), 1);
        assert_eq!(result.lsp_configs[0].0, "hare");
        assert_eq!(result.lsp_configs[0].1.command, "hare-lsp");

        // Grammar NOT in additional_grammars (handled by grammar loader)
        assert!(result.additional_grammars.is_empty());
    }

    #[test]
    fn test_scan_bundle() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_dir = temp_dir.path();

        // Create a bundle
        let bundle_dir = config_dir.join("bundles/packages/elixir-bundle");
        let grammars_dir = bundle_dir.join("grammars");
        let plugins_dir = bundle_dir.join("plugins");
        std::fs::create_dir_all(&grammars_dir).unwrap();
        std::fs::create_dir_all(&plugins_dir).unwrap();

        // Create a dummy grammar file
        std::fs::write(
            grammars_dir.join("Elixir.sublime-syntax"),
            "# dummy grammar",
        )
        .unwrap();

        // Create a dummy plugin entry
        std::fs::write(plugins_dir.join("elixir-plugin.ts"), "// dummy plugin").unwrap();

        std::fs::write(
            bundle_dir.join("package.json"),
            r##"{
                "name": "elixir-bundle",
                "version": "1.0.0",
                "description": "Elixir bundle",
                "type": "bundle",
                "fresh": {
                    "languages": [
                        {
                            "id": "elixir",
                            "grammar": {
                                "file": "grammars/Elixir.sublime-syntax",
                                "extensions": ["ex", "exs"]
                            },
                            "language": {
                                "commentPrefix": "#",
                                "tabSize": 2
                            },
                            "lsp": {
                                "command": "elixir-ls",
                                "autoStart": true
                            }
                        }
                    ],
                    "plugins": [
                        { "entry": "plugins/elixir-plugin.ts" }
                    ],
                    "themes": [
                        { "file": "themes/dark.json", "name": "Elixir Dark", "variant": "dark" }
                    ]
                }
            }"##,
        )
        .unwrap();

        let result = scan_installed_packages(config_dir);

        // Bundle grammars should be in additional_grammars
        assert_eq!(result.additional_grammars.len(), 1);
        assert_eq!(result.additional_grammars[0].language, "elixir");
        assert_eq!(result.additional_grammars[0].extensions, vec!["ex", "exs"]);

        // Language config
        assert_eq!(result.language_configs.len(), 1);
        assert_eq!(result.language_configs[0].0, "elixir");

        // LSP config
        assert_eq!(result.lsp_configs.len(), 1);
        assert_eq!(result.lsp_configs[0].1.command, "elixir-ls");

        // Bundle plugin directory
        assert_eq!(result.bundle_plugin_dirs.len(), 1);
        assert_eq!(result.bundle_plugin_dirs[0], plugins_dir);

        // Bundle theme directory
        assert_eq!(result.bundle_theme_dirs.len(), 1);
        assert_eq!(result.bundle_theme_dirs[0], bundle_dir);
    }

    #[test]
    fn test_scan_skips_malformed_manifest() {
        let temp_dir = tempfile::tempdir().unwrap();
        let config_dir = temp_dir.path();

        // Create a language pack with bad JSON
        let lang_dir = config_dir.join("languages/packages/broken");
        std::fs::create_dir_all(&lang_dir).unwrap();
        std::fs::write(lang_dir.join("package.json"), "{ invalid json }").unwrap();

        // Should not panic
        let result = scan_installed_packages(config_dir);
        assert!(result.language_configs.is_empty());
    }

    #[test]
    fn test_formatter_conversion() {
        let lang = LanguageManifestConfig {
            formatter: Some(FormatterManifestConfig {
                command: "prettier".to_string(),
                args: vec!["--stdin-filepath".to_string(), "$FILE".to_string()],
            }),
            ..Default::default()
        };

        let config = lang.to_language_config();
        let fmt = config.formatter.unwrap();
        assert_eq!(fmt.command, "prettier");
        assert_eq!(fmt.args, vec!["--stdin-filepath", "$FILE"]);
        assert!(fmt.stdin);
        assert_eq!(fmt.timeout_ms, 10000);
    }

    #[test]
    fn test_process_limits_conversion() {
        let lsp = LspManifestConfig {
            command: "test-lsp".to_string(),
            args: vec![],
            auto_start: None,
            initialization_options: None,
            process_limits: Some(ProcessLimitsManifestConfig {
                max_memory_percent: Some(30),
                max_cpu_percent: Some(50),
                enabled: Some(true),
            }),
        };

        let config = lsp.to_lsp_config();
        assert_eq!(config.process_limits.max_memory_percent, Some(30));
        assert_eq!(config.process_limits.max_cpu_percent, Some(50));
        assert!(config.process_limits.enabled);
    }
}
