//! Pure grammar registry types without I/O operations.
//!
//! This module contains the `GrammarRegistry` struct and all syntax lookup methods
//! that don't require filesystem access. This enables WASM compatibility and easier testing.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use syntect::parsing::{SyntaxDefinition, SyntaxReference, SyntaxSet, SyntaxSetBuilder};

// Re-export glob matching utilities for use by other modules
pub use crate::primitives::glob_match::{
    filename_glob_matches, is_glob_pattern, is_path_pattern, path_glob_matches,
};

/// A grammar specification: language name, path to grammar file, and associated file extensions.
///
/// Used to pass grammar information between the plugin layer, loader, and registry
/// without relying on anonymous tuples.
#[derive(Clone, Debug)]
pub struct GrammarSpec {
    /// Language identifier (e.g., "elixir")
    pub language: String,
    /// Path to the grammar file (.sublime-syntax)
    pub path: PathBuf,
    /// File extensions to associate with this grammar (e.g., ["ex", "exs"])
    pub extensions: Vec<String>,
}

/// Where a grammar was loaded from.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "type")]
pub enum GrammarSource {
    /// Built-in to Fresh (pre-compiled syntect defaults + embedded grammars)
    #[serde(rename = "built-in")]
    BuiltIn,
    /// Installed from a user grammar directory (~/.config/fresh/grammars/)
    #[serde(rename = "user")]
    User { path: PathBuf },
    /// From a language pack (~/.config/fresh/languages/packages/)
    #[serde(rename = "language-pack")]
    LanguagePack { name: String, path: PathBuf },
    /// From a bundle package (~/.config/fresh/bundles/packages/)
    #[serde(rename = "bundle")]
    Bundle { name: String, path: PathBuf },
    /// Registered by a plugin at runtime
    #[serde(rename = "plugin")]
    Plugin { plugin: String, path: PathBuf },
}

impl std::fmt::Display for GrammarSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GrammarSource::BuiltIn => write!(f, "built-in"),
            GrammarSource::User { path } => write!(f, "user ({})", path.display()),
            GrammarSource::LanguagePack { name, .. } => write!(f, "language-pack ({})", name),
            GrammarSource::Bundle { name, .. } => write!(f, "bundle ({})", name),
            GrammarSource::Plugin { plugin, .. } => write!(f, "plugin ({})", plugin),
        }
    }
}

/// Information about an available grammar, including its provenance.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GrammarInfo {
    /// The grammar name as used in config files (case-insensitive matching)
    pub name: String,
    /// Where this grammar was loaded from
    pub source: GrammarSource,
    /// File extensions associated with this grammar
    pub file_extensions: Vec<String>,
    /// Optional short name alias (e.g., "bash" for "Bourne Again Shell (bash)")
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub short_name: Option<String>,
}

/// Bridge between syntect display names and `fresh_languages::Language`.
///
/// Most syntect grammars map one-to-one: "Rust" → `Language::Rust`. A few
/// have verbose display names that don't match the tree-sitter enum's
/// `display_name()`, and `Language::from_name` has fuzzy "contains shell"
/// fallbacks that would wrongly tag Nushell as tree-sitter Bash. This is
/// the one place we spell the exceptions out explicitly.
const SYNTECT_TO_TREE_SITTER_ALIASES: &[(&str, fresh_languages::Language)] =
    &[("Bourne Again Shell (bash)", fresh_languages::Language::Bash)];

/// Resolve a syntect syntax display name to a tree-sitter language, using
/// strict equality against the alias table and `Language::display_name()`.
fn tree_sitter_for_syntect_name(display_name: &str) -> Option<fresh_languages::Language> {
    for (syntect_name, lang) in SYNTECT_TO_TREE_SITTER_ALIASES {
        if *syntect_name == display_name {
            return Some(*lang);
        }
    }
    fresh_languages::Language::all()
        .iter()
        .find(|l| l.display_name() == display_name)
        .copied()
}

/// Which highlighters can serve a given `GrammarEntry`.
///
/// A catalog entry may come from syntect (a TextMate grammar indexed into
/// `SyntaxSet`), tree-sitter (a `fresh_languages::Language`), or both.
#[derive(Clone, Debug, Default)]
pub struct GrammarEngines {
    /// Index into `GrammarRegistry::syntax_set().syntaxes()`, if a syntect
    /// grammar is available.
    pub syntect: Option<usize>,
    /// Tree-sitter language, if one is registered for this grammar.
    pub tree_sitter: Option<fresh_languages::Language>,
}

/// A single entry in the unified grammar catalog.
///
/// Each entry represents one logical language (e.g. "Rust", "TypeScript") and
/// records which highlighting engines can serve it, plus the names/extensions
/// used to look it up. The catalog is the single source of truth for grammar
/// lookups — `find_by_name`, `find_by_path`, `find_by_extension` all return
/// entries from here, and both `HighlightEngine::from_entry` and
/// `DetectedLanguage::from_entry` consume them.
#[derive(Clone, Debug)]
pub struct GrammarEntry {
    /// Human-readable display name (e.g. "TypeScript", "Bourne Again Shell (bash)").
    pub display_name: String,
    /// Canonical language ID used in config and LSP (e.g. "typescript", "csharp").
    pub language_id: String,
    /// Short alias, if one exists (e.g. "ts" for TypeScript).
    pub short_name: Option<String>,
    /// File extensions (without leading dot).
    pub extensions: Vec<String>,
    /// Exact filenames that map to this grammar (e.g. "Dockerfile").
    pub filenames: Vec<String>,
    /// Filename globs from user config (e.g. "*.conf", "/etc/**/rc.*").
    pub filename_globs: Vec<String>,
    /// Where this grammar was loaded from.
    pub source: GrammarSource,
    /// Highlighters that can serve this entry.
    pub engines: GrammarEngines,
}

/// Embedded TOML grammar (syntect doesn't include one)
pub const TOML_GRAMMAR: &str = include_str!("../../grammars/toml.sublime-syntax");

/// Embedded Odin grammar (syntect doesn't include one)
/// From: https://github.com/Tetralux/sublime-odin (MIT License)
pub const ODIN_GRAMMAR: &str = include_str!("../../grammars/odin/Odin.sublime-syntax");

/// Embedded Zig grammar (syntect doesn't include one)
pub const ZIG_GRAMMAR: &str = include_str!("../../grammars/zig.sublime-syntax");

/// Embedded Git Rebase Todo grammar for interactive rebase
pub const GIT_REBASE_GRAMMAR: &str = include_str!("../../grammars/git-rebase.sublime-syntax");

/// Embedded Git Commit Message grammar for COMMIT_EDITMSG, MERGE_MSG, etc.
pub const GIT_COMMIT_GRAMMAR: &str = include_str!("../../grammars/git-commit.sublime-syntax");

/// Embedded Gitignore grammar for .gitignore and similar files
pub const GITIGNORE_GRAMMAR: &str = include_str!("../../grammars/gitignore.sublime-syntax");

/// Embedded Git Config grammar for .gitconfig, .gitmodules
pub const GITCONFIG_GRAMMAR: &str = include_str!("../../grammars/gitconfig.sublime-syntax");

/// Embedded Git Attributes grammar for .gitattributes
pub const GITATTRIBUTES_GRAMMAR: &str = include_str!("../../grammars/gitattributes.sublime-syntax");

/// Embedded Typst grammar (syntect doesn't include one)
pub const TYPST_GRAMMAR: &str = include_str!("../../grammars/typst.sublime-syntax");

/// Embedded Dockerfile grammar
pub const DOCKERFILE_GRAMMAR: &str = include_str!("../../grammars/dockerfile.sublime-syntax");
/// Embedded INI grammar (also handles .env, .cfg, .editorconfig, etc.)
pub const INI_GRAMMAR: &str = include_str!("../../grammars/ini.sublime-syntax");
/// Embedded CMake grammar
pub const CMAKE_GRAMMAR: &str = include_str!("../../grammars/cmake.sublime-syntax");
/// Embedded SCSS grammar
pub const SCSS_GRAMMAR: &str = include_str!("../../grammars/scss.sublime-syntax");
/// Embedded LESS grammar
pub const LESS_GRAMMAR: &str = include_str!("../../grammars/less.sublime-syntax");
/// Embedded PowerShell grammar
pub const POWERSHELL_GRAMMAR: &str = include_str!("../../grammars/powershell.sublime-syntax");
/// Embedded Kotlin grammar
pub const KOTLIN_GRAMMAR: &str = include_str!("../../grammars/kotlin.sublime-syntax");
/// Embedded Swift grammar
pub const SWIFT_GRAMMAR: &str = include_str!("../../grammars/swift.sublime-syntax");
/// Embedded Dart grammar
pub const DART_GRAMMAR: &str = include_str!("../../grammars/dart.sublime-syntax");
/// Embedded Elixir grammar
pub const ELIXIR_GRAMMAR: &str = include_str!("../../grammars/elixir.sublime-syntax");
/// Embedded F# grammar
pub const FSHARP_GRAMMAR: &str = include_str!("../../grammars/fsharp.sublime-syntax");
/// Embedded Nix grammar
pub const NIX_GRAMMAR: &str = include_str!("../../grammars/nix.sublime-syntax");
/// Embedded HCL/Terraform grammar
pub const HCL_GRAMMAR: &str = include_str!("../../grammars/hcl.sublime-syntax");
/// Embedded Protocol Buffers grammar
pub const PROTOBUF_GRAMMAR: &str = include_str!("../../grammars/protobuf.sublime-syntax");
/// Embedded GraphQL grammar
pub const GRAPHQL_GRAMMAR: &str = include_str!("../../grammars/graphql.sublime-syntax");
/// Embedded Julia grammar
pub const JULIA_GRAMMAR: &str = include_str!("../../grammars/julia.sublime-syntax");
/// Embedded Nim grammar
pub const NIM_GRAMMAR: &str = include_str!("../../grammars/nim.sublime-syntax");
/// Embedded Gleam grammar
pub const GLEAM_GRAMMAR: &str = include_str!("../../grammars/gleam.sublime-syntax");
/// Embedded V language grammar
pub const VLANG_GRAMMAR: &str = include_str!("../../grammars/vlang.sublime-syntax");
/// Embedded Solidity grammar
pub const SOLIDITY_GRAMMAR: &str = include_str!("../../grammars/solidity.sublime-syntax");
/// Embedded KDL grammar
pub const KDL_GRAMMAR: &str = include_str!("../../grammars/kdl.sublime-syntax");
/// Embedded Nushell grammar
pub const NUSHELL_GRAMMAR: &str = include_str!("../../grammars/nushell.sublime-syntax");
/// Embedded Starlark/Bazel grammar
pub const STARLARK_GRAMMAR: &str = include_str!("../../grammars/starlark.sublime-syntax");
/// Embedded Justfile grammar
pub const JUSTFILE_GRAMMAR: &str = include_str!("../../grammars/justfile.sublime-syntax");
/// Embedded Earthfile grammar
pub const EARTHFILE_GRAMMAR: &str = include_str!("../../grammars/earthfile.sublime-syntax");
/// Embedded Go Module grammar
pub const GOMOD_GRAMMAR: &str = include_str!("../../grammars/gomod.sublime-syntax");
/// Embedded Vue grammar
pub const VUE_GRAMMAR: &str = include_str!("../../grammars/vue.sublime-syntax");
/// Embedded Svelte grammar
pub const SVELTE_GRAMMAR: &str = include_str!("../../grammars/svelte.sublime-syntax");
/// Embedded Astro grammar
pub const ASTRO_GRAMMAR: &str = include_str!("../../grammars/astro.sublime-syntax");
/// Embedded Hyprlang grammar (Hyprland config)
pub const HYPRLANG_GRAMMAR: &str = include_str!("../../grammars/hyprlang.sublime-syntax");
/// Embedded AutoHotkey grammar
/// From: https://github.com/SALZKARTOFFEEEL/ahk-sublime-syntax (MIT License)
pub const AUTOHOTKEY_GRAMMAR: &str =
    include_str!("../../grammars/autohotkey/AutoHotkey.sublime-syntax");
/// Embedded Racket grammar (syntect doesn't include one)
pub const RACKET_GRAMMAR: &str = include_str!("../../grammars/racket.sublime-syntax");
/// Embedded Verilog grammar (HDL)
pub const VERILOG_GRAMMAR: &str = include_str!("../../grammars/verilog.sublime-syntax");
/// Embedded SystemVerilog grammar (HDL)
pub const SYSTEMVERILOG_GRAMMAR: &str = include_str!("../../grammars/systemverilog.sublime-syntax");
/// Embedded VHDL grammar (HDL)
pub const VHDL_GRAMMAR: &str = include_str!("../../grammars/vhdl.sublime-syntax");

/// Registry of all available TextMate grammars.
///
/// This struct holds the compiled syntax set and provides lookup methods.
/// It does not perform I/O directly - use `GrammarLoader` for loading grammars.
impl std::fmt::Debug for GrammarRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GrammarRegistry")
            .field("syntax_count", &self.syntax_set.syntaxes().len())
            .finish()
    }
}

pub struct GrammarRegistry {
    /// Combined syntax set (built-in + embedded + user grammars)
    syntax_set: Arc<SyntaxSet>,
    /// Extension -> scope name mapping for user grammars (takes priority)
    user_extensions: HashMap<String, String>,
    /// Filename -> scope name mapping for dotfiles and special files
    filename_scopes: HashMap<String, String>,
    /// Paths to dynamically loaded grammar files (for reloading when adding more)
    loaded_grammar_paths: Vec<GrammarSpec>,
    /// Provenance info for each grammar (keyed by grammar name)
    grammar_sources: HashMap<String, GrammarInfo>,
    /// Short name aliases: lowercase short_name -> full syntect grammar name.
    /// Provides a deterministic, one-to-one mapping so users can write
    /// `grammar = "bash"` instead of `grammar = "Bourne Again Shell (bash)"`.
    aliases: HashMap<String, String>,
    /// Unified catalog of every known grammar. Rebuilt whenever the syntax set
    /// or alias table changes. Lookups (`find_by_name`, `find_by_path`, ...)
    /// all resolve against this.
    catalog: Vec<GrammarEntry>,
    /// Index from lowercased lookup keys (display name, language_id, short_name)
    /// to catalog index.
    catalog_by_name: HashMap<String, usize>,
    /// Index from file extension (without dot) to catalog index.
    catalog_by_extension: HashMap<String, usize>,
    /// Index from filename to catalog index.
    catalog_by_filename: HashMap<String, usize>,
    /// The most recent language config handed to `apply_language_config`.
    /// Retained so `rebuild_catalog` can replay it — otherwise a rebuild
    /// (triggered by e.g. `populate_built_in_aliases`) silently wipes user
    /// `[languages]` config that was merged on top.
    applied_language_config: HashMap<String, crate::config::LanguageConfig>,
    /// Monotonic generation, bumped on every catalog mutation. Lets
    /// observers (plugin state snapshot) detect changes with one integer
    /// compare instead of recounting entries.
    catalog_gen: u64,
}

impl GrammarRegistry {
    /// Create a new GrammarRegistry from pre-built components.
    ///
    /// This is typically called by `GrammarLoader` implementations after
    /// loading grammars from various sources.
    pub(crate) fn new(
        syntax_set: SyntaxSet,
        user_extensions: HashMap<String, String>,
        filename_scopes: HashMap<String, String>,
    ) -> Self {
        Self::new_with_loaded_paths(
            syntax_set,
            user_extensions,
            filename_scopes,
            Vec::new(),
            HashMap::new(),
        )
    }

    /// Create a GrammarRegistry with pre-loaded grammar path tracking.
    ///
    /// Used by the loader when plugin grammars were included in the initial build,
    /// so that `loaded_grammar_paths()` reflects what was actually loaded.
    pub(crate) fn new_with_loaded_paths(
        syntax_set: SyntaxSet,
        user_extensions: HashMap<String, String>,
        filename_scopes: HashMap<String, String>,
        loaded_grammar_paths: Vec<GrammarSpec>,
        grammar_sources: HashMap<String, GrammarInfo>,
    ) -> Self {
        let mut reg = Self {
            syntax_set: Arc::new(syntax_set),
            user_extensions,
            filename_scopes,
            loaded_grammar_paths,
            grammar_sources,
            aliases: HashMap::new(),
            catalog: Vec::new(),
            catalog_by_name: HashMap::new(),
            catalog_by_extension: HashMap::new(),
            catalog_by_filename: HashMap::new(),
            applied_language_config: HashMap::new(),
            catalog_gen: 0,
        };
        reg.rebuild_catalog();
        reg
    }

    /// Create an empty grammar registry (fast, for tests that don't need syntax highlighting)
    pub fn empty() -> Arc<Self> {
        let mut builder = SyntaxSetBuilder::new();
        builder.add_plain_text_syntax();
        let mut reg = Self {
            syntax_set: Arc::new(builder.build()),
            user_extensions: HashMap::new(),
            filename_scopes: HashMap::new(),
            loaded_grammar_paths: Vec::new(),
            grammar_sources: HashMap::new(),
            aliases: HashMap::new(),
            catalog: Vec::new(),
            catalog_by_name: HashMap::new(),
            catalog_by_extension: HashMap::new(),
            catalog_by_filename: HashMap::new(),
            applied_language_config: HashMap::new(),
            catalog_gen: 0,
        };
        reg.rebuild_catalog();
        Arc::new(reg)
    }

    /// Create a registry with only syntect's pre-compiled defaults (~0ms).
    ///
    /// This provides instant syntax highlighting for ~50 common languages
    /// (Rust, Python, JS/TS, C/C++, Go, Java, HTML, CSS, Markdown, etc.)
    /// without any `SyntaxSetBuilder::build()` call. Use this at startup,
    /// then swap in a full registry built on a background thread.
    pub fn defaults_only() -> Arc<Self> {
        // Load pre-compiled syntax set (defaults + embedded grammars) from
        // build-time packdump. This avoids the expensive into_builder() + build()
        // cycle at runtime (~12s → ~300ms).
        tracing::info!("defaults_only: loading pre-compiled syntax packdump...");
        let syntax_set: SyntaxSet = syntect::dumps::from_uncompressed_data(include_bytes!(
            concat!(env!("OUT_DIR"), "/default_syntaxes.packdump")
        ))
        .expect("Failed to load pre-compiled syntax packdump");
        tracing::info!(
            "defaults_only: loaded ({} syntaxes)",
            syntax_set.syntaxes().len()
        );
        let grammar_sources = Self::build_grammar_sources_from_syntax_set(&syntax_set);
        let filename_scopes = Self::build_filename_scopes();
        let extra_extensions = Self::build_extra_extensions();
        let mut registry = Self {
            syntax_set: Arc::new(syntax_set),
            user_extensions: extra_extensions,
            filename_scopes,
            loaded_grammar_paths: Vec::new(),
            grammar_sources,
            aliases: HashMap::new(),
            catalog: Vec::new(),
            catalog_by_name: HashMap::new(),
            catalog_by_extension: HashMap::new(),
            catalog_by_filename: HashMap::new(),
            applied_language_config: HashMap::new(),
            catalog_gen: 0,
        };
        registry.populate_built_in_aliases();
        registry.rebuild_catalog();
        Arc::new(registry)
    }

    /// Build extra extension -> scope mappings for extensions not covered by syntect defaults.
    ///
    /// These map common file extensions to existing syntect grammar scopes,
    /// filling gaps where syntect's built-in extension lists are incomplete.
    pub(crate) fn build_extra_extensions() -> HashMap<String, String> {
        let mut map = HashMap::new();

        // JavaScript variants not in syntect defaults (["js", "htc"])
        let js_scope = "source.js".to_string();
        map.insert("cjs".to_string(), js_scope.clone());
        map.insert("mjs".to_string(), js_scope);

        // Dockerfile variants (e.g. Dockerfile.dev -> .dev extension)
        // These won't match by extension, handled by filename_scopes and first_line_match

        map
    }

    /// Build the default filename -> scope mappings for dotfiles and special files.
    pub(crate) fn build_filename_scopes() -> HashMap<String, String> {
        let mut map = HashMap::new();

        // Shell configuration files -> Bash/Shell script scope
        let shell_scope = "source.shell.bash".to_string();
        for filename in [
            ".zshrc",
            ".zprofile",
            ".zshenv",
            ".zlogin",
            ".zlogout",
            ".bash_aliases",
            // .bashrc and .bash_profile are already recognized by syntect
            // Common shell script files without extensions
            "PKGBUILD",
            "APKBUILD",
        ] {
            map.insert(filename.to_string(), shell_scope.clone());
        }

        // Git rebase todo files
        let git_rebase_scope = "source.git-rebase-todo".to_string();
        map.insert("git-rebase-todo".to_string(), git_rebase_scope);

        // Git commit message files
        let git_commit_scope = "source.git-commit".to_string();
        for filename in ["COMMIT_EDITMSG", "MERGE_MSG", "SQUASH_MSG", "TAG_EDITMSG"] {
            map.insert(filename.to_string(), git_commit_scope.clone());
        }

        // Gitignore and similar files
        let gitignore_scope = "source.gitignore".to_string();
        for filename in [".gitignore", ".dockerignore", ".npmignore", ".hgignore"] {
            map.insert(filename.to_string(), gitignore_scope.clone());
        }

        // Git config files
        let gitconfig_scope = "source.gitconfig".to_string();
        for filename in [".gitconfig", ".gitmodules"] {
            map.insert(filename.to_string(), gitconfig_scope.clone());
        }

        // Git attributes files
        let gitattributes_scope = "source.gitattributes".to_string();
        map.insert(".gitattributes".to_string(), gitattributes_scope);

        // Jenkinsfile -> Groovy
        let groovy_scope = "source.groovy".to_string();
        map.insert("Jenkinsfile".to_string(), groovy_scope);

        // Vagrantfile -> Ruby (syntect already handles this, but be explicit)
        // Brewfile -> Ruby
        let ruby_scope = "source.ruby".to_string();
        map.insert("Brewfile".to_string(), ruby_scope);

        // Dockerfile and variants (exact names; Dockerfile.* handled via prefix check)
        let dockerfile_scope = "source.dockerfile".to_string();
        map.insert("Dockerfile".to_string(), dockerfile_scope.clone());
        map.insert("Containerfile".to_string(), dockerfile_scope.clone());
        // Common Dockerfile variants
        map.insert("Dockerfile.dev".to_string(), dockerfile_scope.clone());
        map.insert("Dockerfile.prod".to_string(), dockerfile_scope.clone());
        map.insert("Dockerfile.test".to_string(), dockerfile_scope.clone());
        map.insert("Dockerfile.build".to_string(), dockerfile_scope.clone());

        // CMake
        let cmake_scope = "source.cmake".to_string();
        map.insert("CMakeLists.txt".to_string(), cmake_scope);

        // Starlark/Bazel
        let starlark_scope = "source.starlark".to_string();
        map.insert("BUILD".to_string(), starlark_scope.clone());
        map.insert("BUILD.bazel".to_string(), starlark_scope.clone());
        map.insert("WORKSPACE".to_string(), starlark_scope.clone());
        map.insert("WORKSPACE.bazel".to_string(), starlark_scope.clone());
        map.insert("Tiltfile".to_string(), starlark_scope);

        // Justfile (various casings)
        let justfile_scope = "source.justfile".to_string();
        map.insert("justfile".to_string(), justfile_scope.clone());
        map.insert("Justfile".to_string(), justfile_scope.clone());
        map.insert(".justfile".to_string(), justfile_scope);

        // EditorConfig -> INI
        let ini_scope = "source.ini".to_string();
        map.insert(".editorconfig".to_string(), ini_scope);

        // Earthfile
        let earthfile_scope = "source.earthfile".to_string();
        map.insert("Earthfile".to_string(), earthfile_scope);

        // Hyprlang (Hyprland config files)
        let hyprlang_scope = "source.hyprlang".to_string();
        map.insert("hyprland.conf".to_string(), hyprlang_scope.clone());
        map.insert("hyprpaper.conf".to_string(), hyprlang_scope.clone());
        map.insert("hyprlock.conf".to_string(), hyprlang_scope);

        // go.mod / go.sum
        let gomod_scope = "source.gomod".to_string();
        map.insert("go.mod".to_string(), gomod_scope.clone());
        map.insert("go.sum".to_string(), gomod_scope);

        map
    }

    /// Add embedded grammars (TOML, Odin, etc.) to a syntax set builder.
    pub(crate) fn add_embedded_grammars(builder: &mut SyntaxSetBuilder) {
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

        // Odin grammar
        match SyntaxDefinition::load_from_str(ODIN_GRAMMAR, true, Some("Odin")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Odin grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Odin grammar: {}", e);
            }
        }

        // Zig grammar
        match SyntaxDefinition::load_from_str(ZIG_GRAMMAR, true, Some("Zig")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Zig grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Zig grammar: {}", e);
            }
        }

        // Git Rebase Todo grammar
        match SyntaxDefinition::load_from_str(GIT_REBASE_GRAMMAR, true, Some("Git Rebase Todo")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Git Rebase Todo grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Git Rebase Todo grammar: {}", e);
            }
        }

        // Git Commit Message grammar
        match SyntaxDefinition::load_from_str(GIT_COMMIT_GRAMMAR, true, Some("Git Commit Message"))
        {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Git Commit Message grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Git Commit Message grammar: {}", e);
            }
        }

        // Gitignore grammar
        match SyntaxDefinition::load_from_str(GITIGNORE_GRAMMAR, true, Some("Gitignore")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Gitignore grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Gitignore grammar: {}", e);
            }
        }

        // Git Config grammar
        match SyntaxDefinition::load_from_str(GITCONFIG_GRAMMAR, true, Some("Git Config")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Git Config grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Git Config grammar: {}", e);
            }
        }

        // Git Attributes grammar
        match SyntaxDefinition::load_from_str(GITATTRIBUTES_GRAMMAR, true, Some("Git Attributes")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Git Attributes grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Git Attributes grammar: {}", e);
            }
        }

        // Typst grammar
        match SyntaxDefinition::load_from_str(TYPST_GRAMMAR, true, Some("Typst")) {
            Ok(syntax) => {
                builder.add(syntax);
                tracing::debug!("Loaded embedded Typst grammar");
            }
            Err(e) => {
                tracing::warn!("Failed to load embedded Typst grammar: {}", e);
            }
        }

        // Additional embedded grammars for languages not in syntect defaults
        let additional_grammars: &[(&str, &str)] = &[
            (DOCKERFILE_GRAMMAR, "Dockerfile"),
            (INI_GRAMMAR, "INI"),
            (CMAKE_GRAMMAR, "CMake"),
            (SCSS_GRAMMAR, "SCSS"),
            (LESS_GRAMMAR, "LESS"),
            (POWERSHELL_GRAMMAR, "PowerShell"),
            (KOTLIN_GRAMMAR, "Kotlin"),
            (SWIFT_GRAMMAR, "Swift"),
            (DART_GRAMMAR, "Dart"),
            (ELIXIR_GRAMMAR, "Elixir"),
            (FSHARP_GRAMMAR, "FSharp"),
            (NIX_GRAMMAR, "Nix"),
            (HCL_GRAMMAR, "HCL"),
            (PROTOBUF_GRAMMAR, "Protocol Buffers"),
            (GRAPHQL_GRAMMAR, "GraphQL"),
            (JULIA_GRAMMAR, "Julia"),
            (NIM_GRAMMAR, "Nim"),
            (GLEAM_GRAMMAR, "Gleam"),
            (VLANG_GRAMMAR, "V"),
            (SOLIDITY_GRAMMAR, "Solidity"),
            (KDL_GRAMMAR, "KDL"),
            (NUSHELL_GRAMMAR, "Nushell"),
            (STARLARK_GRAMMAR, "Starlark"),
            (JUSTFILE_GRAMMAR, "Justfile"),
            (EARTHFILE_GRAMMAR, "Earthfile"),
            (GOMOD_GRAMMAR, "Go Module"),
            (VUE_GRAMMAR, "Vue"),
            (SVELTE_GRAMMAR, "Svelte"),
            (ASTRO_GRAMMAR, "Astro"),
            (HYPRLANG_GRAMMAR, "Hyprlang"),
            (AUTOHOTKEY_GRAMMAR, "AutoHotkey"),
            (RACKET_GRAMMAR, "Racket"),
            (VERILOG_GRAMMAR, "Verilog"),
            (SYSTEMVERILOG_GRAMMAR, "SystemVerilog"),
            (VHDL_GRAMMAR, "VHDL"),
        ];

        for (grammar_str, name) in additional_grammars {
            match SyntaxDefinition::load_from_str(grammar_str, true, Some(name)) {
                Ok(syntax) => {
                    builder.add(syntax);
                    tracing::debug!("Loaded embedded {} grammar", name);
                }
                Err(e) => {
                    tracing::warn!("Failed to load embedded {} grammar: {}", name, e);
                }
            }
        }
    }

    /// Find syntax for a file by path/extension/filename.
    ///
    /// Purely metadata-based — does not read the file. For first-line
    /// (shebang) fallback, use [`find_by_path`] with a `first_line` argument
    /// and resolve the returned entry's syntect index.
    pub fn find_syntax_for_file(&self, path: &Path) -> Option<&SyntaxReference> {
        let entry = self.find_by_path(path, None)?;
        entry
            .engines
            .syntect
            .map(|i| &self.syntax_set.syntaxes()[i])
    }

    /// Find syntax by name, with alias resolution.
    ///
    /// Thin wrapper around `find_by_name` that returns the associated syntect
    /// `SyntaxReference`. Tree-sitter-only entries return `None`.
    ///
    /// Falls back to a direct syntect lookup for "Plain Text", which the
    /// catalog deliberately omits but syntect still exposes.
    pub fn find_syntax_by_name(&self, name: &str) -> Option<&SyntaxReference> {
        if let Some(entry) = self.find_by_name(name) {
            if let Some(idx) = entry.engines.syntect {
                return Some(&self.syntax_set.syntaxes()[idx]);
            }
        }
        // Plain Text is excluded from the catalog (it's not a "grammar" a user
        // would ever pick), but syntect still stores it and a handful of
        // callers still ask for it by name.
        self.syntax_set.find_syntax_by_name(name)
    }

    // === Alias management ===

    /// Hardcoded short-name aliases for built-in and embedded grammars.
    ///
    /// Each entry maps a short name (lowercase) to the exact syntect grammar name.
    /// Only grammars whose full name differs significantly from a natural short
    /// form need an entry here. Grammars already short (e.g., "Rust", "Go") are
    /// reachable via case-insensitive matching and don't need aliases.
    fn built_in_aliases() -> Vec<(&'static str, &'static str)> {
        vec![
            // Syntect built-in grammars with verbose names
            ("bash", "Bourne Again Shell (bash)"),
            ("shell", "Bourne Again Shell (bash)"),
            ("sh", "Bourne Again Shell (bash)"),
            ("c++", "C++"),
            ("cpp", "C++"),
            ("csharp", "C#"),
            ("objc", "Objective-C"),
            ("objcpp", "Objective-C++"),
            ("regex", "Regular Expressions (Python)"),
            ("regexp", "Regular Expressions (Python)"),
            // Embedded grammars with multi-word or non-obvious names
            ("proto", "Protocol Buffers"),
            ("protobuf", "Protocol Buffers"),
            ("gomod", "Go Module"),
            ("git-rebase", "Git Rebase Todo"),
            ("git-commit", "Git Commit Message"),
            ("git-config", "Git Config"),
            ("git-attributes", "Git Attributes"),
            ("gitignore", "Gitignore"),
            ("fsharp", "FSharp"),
            ("f#", "FSharp"),
            ("terraform", "HCL"),
            ("tf", "HCL"),
            ("ts", "TypeScript"),
            ("js", "JavaScript"),
            ("py", "Python"),
            ("rb", "Ruby"),
            ("rs", "Rust"),
            ("md", "Markdown"),
            ("yml", "YAML"),
            ("dockerfile", "Dockerfile"),
        ]
    }

    /// Populate aliases from the built-in table.
    ///
    /// Validates that:
    /// - Each alias target (full name) exists in the syntax set
    /// - No alias collides (case-insensitive) with an existing grammar full name
    /// - No duplicate aliases exist
    pub(crate) fn populate_built_in_aliases(&mut self) {
        for (short, full) in Self::built_in_aliases() {
            self.register_alias_inner(short, full, true);
        }
        self.rebuild_catalog();
    }

    /// Register a short-name alias for a grammar.
    ///
    /// Returns `true` if the alias was registered, `false` if rejected due to
    /// collision or missing target. For built-in aliases, collisions panic
    /// (they indicate a bug). For dynamic aliases, collisions log a warning.
    ///
    /// Splices the alias directly into the catalog rather than rebuilding, so
    /// any user config previously merged via `apply_language_config` is
    /// preserved. A full rebuild would wipe those entries.
    pub(crate) fn register_alias(&mut self, short_name: &str, full_name: &str) -> bool {
        if !self.register_alias_inner(short_name, full_name, false) {
            return false;
        }
        let short_lower = short_name.to_lowercase();
        let full_lower = full_name.to_lowercase();
        if let Some(&idx) = self.catalog_by_name.get(&full_lower) {
            self.catalog_by_name
                .entry(short_lower.clone())
                .or_insert(idx);
            let entry = &mut self.catalog[idx];
            let replace = match &entry.short_name {
                None => true,
                Some(existing) => short_name.len() < existing.len(),
            };
            if replace {
                entry.short_name = Some(short_lower);
            }
        }
        true
    }

    fn register_alias_inner(
        &mut self,
        short_name: &str,
        full_name: &str,
        is_built_in: bool,
    ) -> bool {
        let short_lower = short_name.to_lowercase();

        // Validate: target grammar must exist in the syntax set
        let target_exists = self
            .syntax_set
            .syntaxes()
            .iter()
            .any(|s| s.name.eq_ignore_ascii_case(full_name));
        if !target_exists {
            // Tree-sitter-only targets (e.g. TypeScript) are expected to be
            // absent from the syntect set. `rebuild_catalog` attaches their
            // short names via a separate pass over `built_in_aliases()`.
            if tree_sitter_for_syntect_name(full_name).is_some() {
                return false;
            }
            if is_built_in {
                // Built-in alias targets should always exist; warn but don't panic
                // (grammar might have been removed from syntect upstream)
                tracing::warn!(
                    "[grammar-alias] Built-in alias '{}' -> '{}': target grammar not found, skipping",
                    short_name, full_name
                );
            } else {
                tracing::warn!(
                    "[grammar-alias] Alias '{}' -> '{}': target grammar not found, skipping",
                    short_name,
                    full_name
                );
            }
            return false;
        }

        // Validate: short name must not collide (case-insensitive) with any grammar full name
        let collides_with_full_name = self
            .syntax_set
            .syntaxes()
            .iter()
            .any(|s| s.name.eq_ignore_ascii_case(&short_lower));
        if collides_with_full_name {
            // This is actually fine — the short name matches a full name directly,
            // so find_syntax_by_name's case-insensitive search will find it.
            // No alias needed.
            tracing::debug!(
                "[grammar-alias] Alias '{}' matches an existing grammar name, skipping (not needed)",
                short_name
            );
            return false;
        }

        // Validate: no duplicate alias (case-insensitive)
        if let Some(existing_target) = self.aliases.get(&short_lower) {
            if existing_target.eq_ignore_ascii_case(full_name) {
                // Same mapping, no-op
                return true;
            }
            let msg = format!(
                "Alias '{}' already maps to '{}', cannot remap to '{}'",
                short_name, existing_target, full_name
            );
            if is_built_in {
                panic!("[grammar-alias] Built-in alias collision: {}", msg);
            } else {
                tracing::warn!("[grammar-alias] {}", msg);
                return false;
            }
        }

        // Resolve the exact syntect name (preserving original case)
        let exact_name = self
            .syntax_set
            .syntaxes()
            .iter()
            .find(|s| s.name.eq_ignore_ascii_case(full_name))
            .map(|s| s.name.clone())
            .unwrap();

        self.aliases.insert(short_lower, exact_name);
        true
    }

    // === Unified catalog ===

    /// Rebuild the flat catalog of grammar entries.
    ///
    /// Called after the syntax set, aliases, or filename scopes change.
    /// Produces one entry per logical language by merging:
    /// 1. Every `SyntaxReference` in the syntax set (except "Plain Text")
    /// 2. Every `fresh_languages::Language` not already covered by a syntect entry
    /// 3. Alias short-names attached to their target entry
    /// 4. Filename mappings from `filename_scopes` attached to their scope's entry
    /// 5. Extra extensions from `user_extensions` attached to their scope's entry
    ///
    /// Automatically replays the last `apply_language_config` at the end, so
    /// user `[languages]` config survives any rebuild.
    pub(crate) fn rebuild_catalog(&mut self) {
        // Reverse-map: full_name (lowercase) -> shortest alias.
        //
        // Seed from the built-in alias table as well as the live `aliases`
        // HashMap: the live map only contains aliases whose target exists in
        // the syntect set, so tree-sitter-only entries (TypeScript) would
        // otherwise never get their short name ("ts").
        let mut short_by_full: HashMap<String, String> = HashMap::new();
        let record = |map: &mut HashMap<String, String>, short: &str, full: &str| {
            let key = full.to_lowercase();
            let keep = match map.get(&key) {
                None => true,
                Some(existing) => short.len() < existing.len(),
            };
            if keep {
                map.insert(key, short.to_string());
            }
        };
        for (short, full) in Self::built_in_aliases() {
            record(&mut short_by_full, short, full);
        }
        for (short, full) in &self.aliases {
            record(&mut short_by_full, short, full);
        }

        let derive_language_id =
            |display_name: &str| -> (String, Option<fresh_languages::Language>) {
                let ts = tree_sitter_for_syntect_name(display_name);
                let id = ts
                    .map(|l| l.id().to_string())
                    .unwrap_or_else(|| display_name.to_lowercase());
                (id, ts)
            };

        let mut catalog: Vec<GrammarEntry> = Vec::new();
        let mut scope_to_index: HashMap<String, usize> = HashMap::new();

        // Syntect-backed entries (skip Plain Text).
        //
        // Syntect's `file_extensions` is a hybrid list: real extensions like
        // "rb" sit alongside bare filenames like "Gemfile", "Rakefile",
        // "Makefile". Syntect's own `find_syntax_for_file` tries each entry
        // against the whole filename AND against the path's extension, and
        // the catalog has to preserve that semantics. We keep everything in
        // `extensions` here and index each entry as *both* an extension and
        // a filename at the bottom of this method.
        for (idx, syntax) in self.syntax_set.syntaxes().iter().enumerate() {
            if syntax.name == "Plain Text" {
                continue;
            }
            let (language_id, tree_sitter) = derive_language_id(&syntax.name);
            let short_name = short_by_full.get(&syntax.name.to_lowercase()).cloned();
            let source = self
                .grammar_sources
                .get(&syntax.name)
                .map(|info| info.source.clone())
                .unwrap_or(GrammarSource::BuiltIn);
            let entry_index = catalog.len();
            scope_to_index.insert(syntax.scope.to_string(), entry_index);

            // Union syntect's file_extensions with tree-sitter's own
            // extension list when the entry carries both engines.
            // tree-sitter-javascript handles `.jsx`/`.mjs`/`.cjs` that
            // syntect's JS grammar doesn't list, and the old code used to
            // route those paths to tree-sitter via a separate lookup.
            let mut extensions = syntax.file_extensions.clone();
            if let Some(lang) = tree_sitter {
                for ext in lang.extensions() {
                    let ext = ext.to_string();
                    if !extensions.iter().any(|e| e == &ext) {
                        extensions.push(ext);
                    }
                }
            }

            catalog.push(GrammarEntry {
                display_name: syntax.name.clone(),
                language_id,
                short_name,
                extensions,
                filenames: Vec::new(),
                filename_globs: Vec::new(),
                source,
                engines: GrammarEngines {
                    syntect: Some(idx),
                    tree_sitter,
                },
            });
        }

        // Attach filename_scopes to their entries.
        for (filename, scope) in &self.filename_scopes {
            if let Some(&idx) = scope_to_index.get(scope) {
                if !catalog[idx].filenames.iter().any(|f| f == filename) {
                    catalog[idx].filenames.push(filename.clone());
                }
            }
        }

        // Attach user_extensions (extra → scope) to their entries.
        for (ext, scope) in &self.user_extensions {
            if let Some(&idx) = scope_to_index.get(scope) {
                if !catalog[idx].extensions.iter().any(|e| e == ext) {
                    catalog[idx].extensions.push(ext.clone());
                }
            }
        }

        // Ensure every tree-sitter language has an entry. If a syntect entry
        // already maps to the same tree-sitter language, skip it; otherwise
        // add a tree-sitter-only entry so the catalog is complete (TypeScript
        // being the motivating example — syntect ships no grammar for it).
        let mut ts_covered: std::collections::HashSet<fresh_languages::Language> =
            std::collections::HashSet::new();
        for entry in &catalog {
            if let Some(lang) = entry.engines.tree_sitter {
                ts_covered.insert(lang);
            }
        }
        for lang in fresh_languages::Language::all() {
            if ts_covered.contains(lang) {
                continue;
            }
            let display_name = lang.display_name().to_string();
            let language_id = lang.id().to_string();
            let short_name = short_by_full.get(&display_name.to_lowercase()).cloned();
            let extensions: Vec<String> = lang.extensions().iter().map(|s| s.to_string()).collect();
            catalog.push(GrammarEntry {
                display_name,
                language_id,
                short_name,
                extensions,
                filenames: Vec::new(),
                filename_globs: Vec::new(),
                source: GrammarSource::BuiltIn,
                engines: GrammarEngines {
                    syntect: None,
                    tree_sitter: Some(*lang),
                },
            });
        }

        // Build name / extension / filename indices.
        //
        // Every entry in `extensions` gets indexed in BOTH `by_extension`
        // (lowercased) AND `by_filename` (exact case) — syntect's
        // `file_extensions` list holds both real extensions ("rb") and bare
        // filenames ("Gemfile", "Rakefile", "Makefile"). Indexing both ways
        // matches syntect's own `find_syntax_for_file` semantics.
        let mut by_name: HashMap<String, usize> = HashMap::new();
        let mut by_extension: HashMap<String, usize> = HashMap::new();
        let mut by_filename: HashMap<String, usize> = HashMap::new();
        for (idx, entry) in catalog.iter().enumerate() {
            by_name.insert(entry.display_name.to_lowercase(), idx);
            by_name.insert(entry.language_id.to_lowercase(), idx);
            if let Some(short) = &entry.short_name {
                by_name.insert(short.to_lowercase(), idx);
            }
            for ext in &entry.extensions {
                by_extension.entry(ext.to_lowercase()).or_insert(idx);
                by_filename.entry(ext.clone()).or_insert(idx);
            }
            for filename in &entry.filenames {
                by_filename.entry(filename.clone()).or_insert(idx);
            }
        }

        self.catalog = catalog;
        self.catalog_by_name = by_name;
        self.catalog_by_extension = by_extension;
        self.catalog_by_filename = by_filename;

        // Replay the most recent user config so a rebuild doesn't silently
        // wipe out user `[languages]` rules. `take` + restore avoids both a
        // clone and a borrow checker fight with `apply_language_config_inner`.
        if !self.applied_language_config.is_empty() {
            let cfg = std::mem::take(&mut self.applied_language_config);
            self.apply_language_config_inner(&cfg);
            self.applied_language_config = cfg;
        }
        self.catalog_gen = self.catalog_gen.wrapping_add(1);
    }

    /// Return the full catalog of grammar entries.
    pub fn catalog(&self) -> &[GrammarEntry] {
        &self.catalog
    }

    /// Monotonic generation, bumped on every catalog mutation. Compare against
    /// a previously-observed value to decide whether to recompute derived
    /// state.
    pub fn catalog_gen(&self) -> u64 {
        self.catalog_gen
    }

    /// Look up a grammar entry by display name, language ID, or short alias
    /// (case-insensitive). All aliases — built-in and user-config-declared —
    /// are indexed directly in `catalog_by_name` during `rebuild_catalog` /
    /// `register_alias` / `apply_language_config`, so a single lookup covers
    /// every case.
    pub fn find_by_name(&self, name: &str) -> Option<&GrammarEntry> {
        self.catalog_by_name
            .get(&name.to_lowercase())
            .map(|&idx| &self.catalog[idx])
    }

    /// Look up a grammar entry by file path, with optional first-line content
    /// for shebang / `first_line_match` detection.
    ///
    /// Resolution order:
    /// 1. Exact filename (config-declared filenames and filename_scopes live here)
    /// 2. Glob patterns from user config (e.g. "*.conf", "/etc/**/rc.*")
    /// 3. File extension
    /// 4. Shebang / first-line regex match on `first_line` if supplied
    ///
    /// Globs take priority over extension so a user rule like `*.conf → bash`
    /// wins over any built-in extension match on `.conf`. The first-line
    /// fallback (#4) is last so catalog matches stay authoritative — syntect
    /// might otherwise misclassify `.fish` as bash via its first-line
    /// regexes.
    ///
    /// The first-line fallback is pure: it runs syntect's
    /// `find_syntax_by_first_line` regex cache against the caller-supplied
    /// string. The registry never touches the filesystem — the caller (who
    /// already loaded the buffer via the `FileSystem` trait) must extract
    /// the first line and pass it in.
    pub fn find_by_path(&self, path: &Path, first_line: Option<&str>) -> Option<&GrammarEntry> {
        let filename = path.file_name().and_then(|n| n.to_str());
        let path_str = path.to_str().unwrap_or("");

        if let Some(name) = filename {
            if let Some(&idx) = self.catalog_by_filename.get(name) {
                return Some(&self.catalog[idx]);
            }
        }

        // Glob walk — filenames with globs are rare so linear scan is fine.
        if let Some(name) = filename {
            for entry in &self.catalog {
                for pattern in &entry.filename_globs {
                    let matched = if is_path_pattern(pattern) {
                        path_glob_matches(pattern, path_str)
                    } else {
                        filename_glob_matches(pattern, name)
                    };
                    if matched {
                        return Some(entry);
                    }
                }
            }
        }

        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            if let Some(entry) = self.find_by_extension(ext) {
                return Some(entry);
            }
        }

        // Last resort: shebang / first-line regex match against the
        // caller-supplied content. Map the matched syntect grammar back to a
        // catalog entry by name — every syntect syntax has a catalog entry,
        // so this round-trip preserves tree-sitter attachment.
        let line = first_line?;
        let syntax = self.syntax_set.find_syntax_by_first_line(line)?;
        self.find_by_name(&syntax.name)
    }

    /// Look up a grammar entry by file extension (case-insensitive, without dot).
    pub fn find_by_extension(&self, ext: &str) -> Option<&GrammarEntry> {
        self.catalog_by_extension
            .get(&ext.to_lowercase())
            .map(|&idx| &self.catalog[idx])
    }

    /// Merge user `[languages]` config into the catalog.
    ///
    /// For each config entry, resolves its grammar to an existing catalog entry
    /// (by grammar name or by language id). Extensions are added and override
    /// the ext→entry index so config wins over built-in mappings. Filenames are
    /// split into exact matches (indexed) and globs (walked at lookup time).
    ///
    /// If no existing entry matches, a new engine-less entry is created so the
    /// language still appears in the palette.
    ///
    /// Idempotent. The config is cached on the registry so `rebuild_catalog`
    /// can replay it — callers don't need to re-apply after a rebuild.
    pub fn apply_language_config(
        &mut self,
        languages: &HashMap<String, crate::config::LanguageConfig>,
    ) {
        self.applied_language_config = languages.clone();
        self.apply_language_config_inner(languages);
        self.catalog_gen = self.catalog_gen.wrapping_add(1);
    }

    /// Do the actual catalog splicing without touching
    /// `applied_language_config`. Called from `apply_language_config` (which
    /// records the input) and from `rebuild_catalog` (which replays the
    /// cached input after wiping the catalog).
    fn apply_language_config_inner(
        &mut self,
        languages: &HashMap<String, crate::config::LanguageConfig>,
    ) {
        for (lang_id, lang_cfg) in languages {
            let grammar_name = if lang_cfg.grammar.is_empty() {
                lang_id.as_str()
            } else {
                lang_cfg.grammar.as_str()
            };

            // Resolve to an existing entry; fall back to creating one.
            let idx = self
                .catalog_by_name
                .get(&grammar_name.to_lowercase())
                .copied()
                .or_else(|| self.catalog_by_name.get(&lang_id.to_lowercase()).copied())
                .unwrap_or_else(|| {
                    let idx = self.catalog.len();
                    self.catalog.push(GrammarEntry {
                        display_name: lang_id.clone(),
                        language_id: lang_id.clone(),
                        short_name: None,
                        extensions: Vec::new(),
                        filenames: Vec::new(),
                        filename_globs: Vec::new(),
                        source: GrammarSource::BuiltIn,
                        engines: GrammarEngines::default(),
                    });
                    idx
                });

            // Always index the config key so `find_by_name("mylang")` resolves
            // even when `mylang` aliases an existing grammar (e.g.
            // `[languages.mylang] grammar = "Rust"`). `or_insert` preserves
            // any existing mapping — won't clobber the canonical entry.
            self.catalog_by_name
                .entry(lang_id.to_lowercase())
                .or_insert(idx);

            for ext in &lang_cfg.extensions {
                if !self.catalog[idx].extensions.iter().any(|e| e == ext) {
                    self.catalog[idx].extensions.push(ext.clone());
                }
                // Config-declared extensions override any previous mapping.
                self.catalog_by_extension.insert(ext.to_lowercase(), idx);
            }
            for filename in &lang_cfg.filenames {
                if is_glob_pattern(filename) {
                    if !self.catalog[idx]
                        .filename_globs
                        .iter()
                        .any(|f| f == filename)
                    {
                        self.catalog[idx].filename_globs.push(filename.clone());
                    }
                } else {
                    if !self.catalog[idx].filenames.iter().any(|f| f == filename) {
                        self.catalog[idx].filenames.push(filename.clone());
                    }
                    self.catalog_by_filename.insert(filename.clone(), idx);
                }
            }
        }
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

    /// List all available grammars with provenance information.
    ///
    /// Returns a sorted list of `GrammarInfo` entries derived from the unified
    /// catalog — this includes both syntect grammars and tree-sitter-only
    /// languages (like TypeScript). Each entry is listed exactly once even
    /// when both engines can serve it.
    pub fn available_grammar_info(&self) -> Vec<GrammarInfo> {
        let mut result: Vec<GrammarInfo> = self
            .catalog
            .iter()
            .map(|entry| GrammarInfo {
                name: entry.display_name.clone(),
                source: entry.source.clone(),
                file_extensions: entry.extensions.clone(),
                short_name: entry.short_name.clone(),
            })
            .collect();
        result.sort_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()));
        result
    }

    /// Get the grammar sources map.
    pub(crate) fn grammar_sources(&self) -> &HashMap<String, GrammarInfo> {
        &self.grammar_sources
    }

    /// Build grammar source info from a pre-compiled syntax set.
    ///
    /// All grammars in the packdump (syntect defaults + embedded) are tagged as built-in.
    pub(crate) fn build_grammar_sources_from_syntax_set(
        syntax_set: &SyntaxSet,
    ) -> HashMap<String, GrammarInfo> {
        let mut sources = HashMap::new();
        for syntax in syntax_set.syntaxes() {
            sources.insert(
                syntax.name.clone(),
                GrammarInfo {
                    name: syntax.name.clone(),
                    source: GrammarSource::BuiltIn,
                    file_extensions: syntax.file_extensions.clone(),
                    short_name: None,
                },
            );
        }
        sources
    }

    /// Get the user extensions mapping (extension -> scope name).
    #[cfg(test)]
    pub(crate) fn user_extensions(&self) -> &HashMap<String, String> {
        &self.user_extensions
    }

    /// Get the loaded grammar paths (for deduplication in flush_pending_grammars).
    #[cfg(test)]
    pub(crate) fn loaded_grammar_paths(&self) -> &[GrammarSpec] {
        &self.loaded_grammar_paths
    }

    /// Create a new registry with additional grammar files
    ///
    /// This builds a new GrammarRegistry that includes all grammars from
    /// the base registry plus the additional grammars specified.
    /// Uses the base registry's syntax_set as the builder base, preserving
    /// all existing grammars (user grammars, language packs, etc.).
    ///
    /// # Arguments
    /// * `base` - The base registry to extend
    /// * `additional` - List of (language, path, extensions) tuples for new grammars
    ///
    /// # Returns
    /// A new GrammarRegistry with the additional grammars, or None if rebuilding fails
    pub fn with_additional_grammars(
        base: &GrammarRegistry,
        additional: &[GrammarSpec],
    ) -> Option<Self> {
        tracing::info!(
            "[SYNTAX DEBUG] with_additional_grammars: adding {} grammars to base with {} syntaxes",
            additional.len(),
            base.syntax_set.syntaxes().len()
        );

        // Use the base registry's syntax_set as builder base — this preserves
        // ALL existing grammars (defaults, embedded, user, language packs)
        // without needing to reload them from disk.
        let mut builder = (*base.syntax_set).clone().into_builder();

        // Preserve existing user extensions and add new ones
        let mut user_extensions = base.user_extensions.clone();

        // Track loaded grammar paths (existing + new)
        let mut loaded_grammar_paths = base.loaded_grammar_paths.clone();

        // Preserve existing grammar sources
        let mut grammar_sources = base.grammar_sources.clone();

        // Add each new grammar
        for spec in additional {
            tracing::info!(
                "[SYNTAX DEBUG] loading new grammar file: lang='{}', path={:?}, extensions={:?}",
                spec.language,
                spec.path,
                spec.extensions
            );
            match Self::load_grammar_file(&spec.path) {
                Ok(syntax) => {
                    let scope = syntax.scope.to_string();
                    let syntax_name = syntax.name.clone();
                    tracing::info!(
                        "[SYNTAX DEBUG] grammar loaded successfully: name='{}', scope='{}'",
                        syntax_name,
                        scope
                    );
                    builder.add(syntax);
                    tracing::info!(
                        "Loaded grammar for '{}' from {:?} with extensions {:?}",
                        spec.language,
                        spec.path,
                        spec.extensions
                    );
                    // Register extensions for this grammar
                    for ext in &spec.extensions {
                        user_extensions.insert(ext.clone(), scope.clone());
                    }
                    // Track provenance
                    grammar_sources.insert(
                        syntax_name.clone(),
                        GrammarInfo {
                            name: syntax_name,
                            source: GrammarSource::Plugin {
                                plugin: spec.language.clone(),
                                path: spec.path.clone(),
                            },
                            file_extensions: spec.extensions.clone(),
                            short_name: None,
                        },
                    );
                    // Track this grammar path for future reloads
                    loaded_grammar_paths.push(spec.clone());
                }
                Err(e) => {
                    tracing::warn!(
                        "Failed to load grammar for '{}' from {:?}: {}",
                        spec.language,
                        spec.path,
                        e
                    );
                }
            }
        }

        let mut reg = Self {
            syntax_set: Arc::new(builder.build()),
            user_extensions,
            filename_scopes: base.filename_scopes.clone(),
            loaded_grammar_paths,
            grammar_sources,
            aliases: base.aliases.clone(),
            catalog: Vec::new(),
            catalog_by_name: HashMap::new(),
            catalog_by_extension: HashMap::new(),
            catalog_by_filename: HashMap::new(),
            applied_language_config: HashMap::new(),
            catalog_gen: 0,
        };
        reg.rebuild_catalog();
        Some(reg)
    }

    /// Load a grammar file from disk
    ///
    /// Only Sublime Text (.sublime-syntax) format is supported.
    /// TextMate (.tmLanguage) grammars use a completely different format
    /// and cannot be loaded by syntect's yaml-load feature.
    pub(crate) fn load_grammar_file(path: &Path) -> Result<SyntaxDefinition, String> {
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

        match ext {
            "sublime-syntax" => {
                let content = std::fs::read_to_string(path)
                    .map_err(|e| format!("Failed to read file: {}", e))?;
                SyntaxDefinition::load_from_str(
                    &content,
                    true,
                    path.file_stem().and_then(|s| s.to_str()),
                )
                .map_err(|e| format!("Failed to parse sublime-syntax: {}", e))
            }
            _ => Err(format!(
                "Unsupported grammar format: .{}. Only .sublime-syntax is supported.",
                ext
            )),
        }
    }
}

impl Default for GrammarRegistry {
    fn default() -> Self {
        // Create with defaults and embedded grammars only (no user grammars)
        let defaults = SyntaxSet::load_defaults_newlines();
        let mut builder = defaults.into_builder();
        Self::add_embedded_grammars(&mut builder);
        let syntax_set = builder.build();
        let filename_scopes = Self::build_filename_scopes();
        let extra_extensions = Self::build_extra_extensions();

        let mut registry = Self::new(syntax_set, extra_extensions, filename_scopes);
        registry.populate_built_in_aliases();
        registry.rebuild_catalog();
        registry
    }
}

// VSCode package.json structures for parsing grammar manifests

#[derive(Debug, Deserialize)]
pub struct PackageManifest {
    #[serde(default)]
    pub contributes: Option<Contributes>,
}

#[derive(Debug, Deserialize, Default)]
pub struct Contributes {
    #[serde(default)]
    pub languages: Vec<LanguageContribution>,
    #[serde(default)]
    pub grammars: Vec<GrammarContribution>,
}

#[derive(Debug, Deserialize)]
pub struct LanguageContribution {
    pub id: String,
    #[serde(default)]
    pub extensions: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct GrammarContribution {
    pub language: String,
    #[serde(rename = "scopeName")]
    pub scope_name: String,
    pub path: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_registry() {
        let registry = GrammarRegistry::empty();
        // Should have at least plain text
        assert!(!registry.available_syntaxes().is_empty());
    }

    #[test]
    fn test_default_registry() {
        let registry = GrammarRegistry::default();
        // Should have built-in syntaxes
        assert!(!registry.available_syntaxes().is_empty());
    }

    #[test]
    fn test_find_syntax_for_common_extensions() {
        let registry = GrammarRegistry::default();

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
    fn test_racket_grammar_loaded() {
        let registry = GrammarRegistry::default();
        for filename in ["main.rkt", "data.rktd", "info.rktl", "doc.scrbl"] {
            let result = registry.find_syntax_for_file(Path::new(filename));
            assert!(
                result.is_some(),
                "Racket grammar should be available for {}",
                filename
            );
            let entry = registry.find_by_path(Path::new(filename), None).unwrap();
            assert_eq!(entry.display_name, "Racket", "for {}", filename);
        }
    }

    #[test]
    fn test_syntax_set_arc() {
        let registry = GrammarRegistry::default();
        let arc1 = registry.syntax_set_arc();
        let arc2 = registry.syntax_set_arc();
        // Both should point to the same data
        assert!(Arc::ptr_eq(&arc1, &arc2));
    }

    #[test]
    fn test_shell_dotfiles_detection() {
        let registry = GrammarRegistry::default();

        // All these should be detected as shell scripts
        let shell_files = [".zshrc", ".zprofile", ".zshenv", ".bash_aliases"];

        for filename in shell_files {
            let path = Path::new(filename);
            let result = registry.find_syntax_for_file(path);
            assert!(
                result.is_some(),
                "{} should be detected as a syntax",
                filename
            );
            let syntax = result.unwrap();
            // Should be detected as Bash/Shell
            assert!(
                syntax.name.to_lowercase().contains("bash")
                    || syntax.name.to_lowercase().contains("shell"),
                "{} should be detected as shell/bash, got: {}",
                filename,
                syntax.name
            );
        }
    }

    #[test]
    fn test_pkgbuild_detection() {
        let registry = GrammarRegistry::default();

        // PKGBUILD and APKBUILD should be detected as shell scripts
        for filename in ["PKGBUILD", "APKBUILD"] {
            let path = Path::new(filename);
            let result = registry.find_syntax_for_file(path);
            assert!(
                result.is_some(),
                "{} should be detected as a syntax",
                filename
            );
            let syntax = result.unwrap();
            // Should be detected as Bash/Shell
            assert!(
                syntax.name.to_lowercase().contains("bash")
                    || syntax.name.to_lowercase().contains("shell"),
                "{} should be detected as shell/bash, got: {}",
                filename,
                syntax.name
            );
        }
    }

    #[test]
    fn test_find_syntax_with_glob_filenames() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "shell-configs".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["sh".to_string()],
                filenames: vec!["*.conf".to_string(), "*rc".to_string()],
                grammar: "bash".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );
        registry.apply_language_config(&languages);

        assert!(
            registry
                .find_by_path(Path::new("nftables.conf"), None)
                .is_some(),
            "*.conf should match nftables.conf"
        );
        assert!(
            registry.find_by_path(Path::new("lfrc"), None).is_some(),
            "*rc should match lfrc"
        );
        // Unrelated file shouldn't panic.
        let _ = registry.find_by_path(Path::new("randomfile"), None);
    }

    #[test]
    fn test_find_syntax_with_path_glob_filenames() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "shell-configs".to_string(),
            crate::config::LanguageConfig {
                extensions: vec!["sh".to_string()],
                filenames: vec!["/etc/**/rc.*".to_string()],
                grammar: "bash".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );
        registry.apply_language_config(&languages);

        assert!(
            registry
                .find_by_path(Path::new("/etc/rc.conf"), None)
                .is_some(),
            "/etc/**/rc.* should match /etc/rc.conf"
        );
        assert!(
            registry
                .find_by_path(Path::new("/etc/init/rc.local"), None)
                .is_some(),
            "/etc/**/rc.* should match /etc/init/rc.local"
        );
        let _ = registry.find_by_path(Path::new("/var/rc.conf"), None);
    }

    #[test]
    fn test_exact_filename_takes_priority_over_glob() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();

        // A language with exact filename "lfrc" -> python grammar
        languages.insert(
            "custom-lfrc".to_string(),
            crate::config::LanguageConfig {
                extensions: vec![],
                filenames: vec!["lfrc".to_string()],
                grammar: "python".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        // A language with glob "*rc" -> bash grammar
        languages.insert(
            "rc-files".to_string(),
            crate::config::LanguageConfig {
                extensions: vec![],
                filenames: vec!["*rc".to_string()],
                grammar: "bash".to_string(),
                comment_prefix: Some("#".to_string()),
                auto_indent: true,
                auto_close: None,
                auto_surround: None,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                line_wrap: None,
                wrap_column: None,
                page_view: None,
                page_width: None,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
                word_characters: None,
            },
        );

        registry.apply_language_config(&languages);

        // "lfrc" should match the exact rule (python), not the glob (bash)
        let entry = registry.find_by_path(Path::new("lfrc"), None).unwrap();
        assert!(
            entry.display_name.to_lowercase().contains("python"),
            "exact match should win over glob, got: {}",
            entry.display_name
        );
    }

    #[test]
    fn test_built_in_aliases_resolve() {
        let registry = GrammarRegistry::default();

        // "bash" should resolve to "Bourne Again Shell (bash)" via alias
        let syntax = registry.find_syntax_by_name("bash");
        assert!(syntax.is_some(), "alias 'bash' should resolve");
        assert_eq!(syntax.unwrap().name, "Bourne Again Shell (bash)");

        // "cpp" should resolve to "C++"
        let syntax = registry.find_syntax_by_name("cpp");
        assert!(syntax.is_some(), "alias 'cpp' should resolve");
        assert_eq!(syntax.unwrap().name, "C++");

        // "csharp" should resolve to "C#"
        let syntax = registry.find_syntax_by_name("csharp");
        assert!(syntax.is_some(), "alias 'csharp' should resolve");
        assert_eq!(syntax.unwrap().name, "C#");

        // "sh" should also resolve to bash
        let syntax = registry.find_syntax_by_name("sh");
        assert!(syntax.is_some(), "alias 'sh' should resolve");
        assert_eq!(syntax.unwrap().name, "Bourne Again Shell (bash)");

        // "proto" should resolve to "Protocol Buffers"
        let syntax = registry.find_syntax_by_name("proto");
        assert!(syntax.is_some(), "alias 'proto' should resolve");
        assert_eq!(syntax.unwrap().name, "Protocol Buffers");
    }

    #[test]
    fn test_alias_case_insensitive_input() {
        let registry = GrammarRegistry::default();

        // Aliases should be case-insensitive on input
        let syntax = registry.find_syntax_by_name("BASH");
        assert!(
            syntax.is_some(),
            "alias 'BASH' should resolve case-insensitively"
        );
        assert_eq!(syntax.unwrap().name, "Bourne Again Shell (bash)");

        let syntax = registry.find_syntax_by_name("Cpp");
        assert!(
            syntax.is_some(),
            "alias 'Cpp' should resolve case-insensitively"
        );
        assert_eq!(syntax.unwrap().name, "C++");
    }

    #[test]
    fn test_full_name_still_works() {
        let registry = GrammarRegistry::default();

        // Full names should still work (exact match)
        let syntax = registry.find_syntax_by_name("Bourne Again Shell (bash)");
        assert!(syntax.is_some(), "full name should still resolve");
        assert_eq!(syntax.unwrap().name, "Bourne Again Shell (bash)");

        // Case-insensitive full name should still work
        let syntax = registry.find_syntax_by_name("bourne again shell (bash)");
        assert!(
            syntax.is_some(),
            "case-insensitive full name should resolve"
        );
        assert_eq!(syntax.unwrap().name, "Bourne Again Shell (bash)");
    }

    #[test]
    fn test_alias_does_not_shadow_full_names() {
        let registry = GrammarRegistry::default();

        // "Rust" should resolve directly via case-insensitive match, not via alias
        let syntax = registry.find_syntax_by_name("rust");
        assert!(syntax.is_some());
        assert_eq!(syntax.unwrap().name, "Rust");

        // "Go" should resolve directly
        let syntax = registry.find_syntax_by_name("go");
        assert!(syntax.is_some());
        assert_eq!(syntax.unwrap().name, "Go");
    }

    #[test]
    fn test_register_alias_rejects_collision() {
        let mut registry = GrammarRegistry::default();

        // Trying to register an alias that maps to two different targets should fail
        assert!(registry.register_alias("myalias", "Rust"));
        assert!(!registry.register_alias("myalias", "Go"));

        // Same mapping is fine (idempotent)
        assert!(registry.register_alias("myalias", "Rust"));
    }

    #[test]
    fn test_register_alias_rejects_nonexistent_target() {
        let mut registry = GrammarRegistry::default();
        assert!(!registry.register_alias("nope", "Nonexistent Grammar"));
    }

    #[test]
    fn test_register_alias_skips_existing_grammar_name() {
        let mut registry = GrammarRegistry::default();

        // "rust" case-insensitively matches the grammar "Rust", so no alias needed
        assert!(!registry.register_alias("rust", "Rust"));
        // Should still be resolvable via case-insensitive match
        assert!(registry.find_syntax_by_name("rust").is_some());
    }

    #[test]
    fn test_available_grammar_info_includes_short_names() {
        let registry = GrammarRegistry::default();
        let infos = registry.available_grammar_info();

        let bash_info = infos.iter().find(|g| g.name == "Bourne Again Shell (bash)");
        assert!(bash_info.is_some(), "bash grammar should be in the list");
        let bash_info = bash_info.unwrap();
        assert!(
            bash_info.short_name.is_some(),
            "bash grammar should have a short_name"
        );
        // The shortest alias for bash is "sh"
        assert_eq!(bash_info.short_name.as_deref(), Some("sh"));
    }

    #[test]
    fn test_catalog_contains_each_language_once() {
        let registry = GrammarRegistry::default();
        let catalog = registry.catalog();

        // Every catalog entry must have a unique (case-insensitive) display name.
        let mut seen = std::collections::HashSet::new();
        for entry in catalog {
            let key = entry.display_name.to_lowercase();
            assert!(
                seen.insert(key.clone()),
                "duplicate catalog entry for display_name={:?}",
                entry.display_name
            );
        }

        // TypeScript is tree-sitter-only (syntect ships no grammar for it) yet
        // must still appear in the catalog.
        let ts = registry
            .find_by_name("TypeScript")
            .expect("TypeScript must be in the catalog");
        assert!(ts.engines.syntect.is_none());
        assert_eq!(
            ts.engines.tree_sitter,
            Some(fresh_languages::Language::TypeScript)
        );
        assert_eq!(ts.language_id, "typescript");
        assert!(ts.extensions.iter().any(|e| e == "ts"));

        // Languages that exist in both syntect and tree-sitter (Rust, Python,
        // JavaScript) must appear exactly once and prefer the syntect engine.
        for name in ["Rust", "Python", "JavaScript"] {
            let entry = registry
                .find_by_name(name)
                .unwrap_or_else(|| panic!("{} must be in the catalog", name));
            assert!(
                entry.engines.syntect.is_some(),
                "{} should have a syntect index",
                name
            );
            assert!(
                entry.engines.tree_sitter.is_some(),
                "{} should also have a tree-sitter language",
                name
            );
            // Only one entry with this display name (already checked above),
            // but also verify language_id lookup lands on the same entry.
            let by_id = registry
                .find_by_name(&entry.language_id)
                .expect("language_id should resolve");
            assert_eq!(by_id.display_name, entry.display_name);
        }
    }

    #[test]
    fn test_catalog_find_by_path_and_extension() {
        let registry = GrammarRegistry::default();
        let ts = registry
            .find_by_path(Path::new("foo.ts"), None)
            .expect("foo.ts should resolve");
        assert_eq!(ts.display_name, "TypeScript");
        let rs = registry.find_by_extension("rs").expect("rs should resolve");
        assert_eq!(rs.display_name, "Rust");
    }

    /// Build a minimal LanguageConfig for tests.
    fn lang_cfg(
        grammar: &str,
        extensions: &[&str],
        filenames: &[&str],
    ) -> crate::config::LanguageConfig {
        crate::config::LanguageConfig {
            extensions: extensions.iter().map(|s| s.to_string()).collect(),
            filenames: filenames.iter().map(|s| s.to_string()).collect(),
            grammar: grammar.to_string(),
            comment_prefix: None,
            auto_indent: true,
            auto_close: None,
            auto_surround: None,
            textmate_grammar: None,
            show_whitespace_tabs: true,
            line_wrap: None,
            wrap_column: None,
            page_view: None,
            page_width: None,
            use_tabs: None,
            tab_size: None,
            formatter: None,
            format_on_save: false,
            on_save: vec![],
            word_characters: None,
        }
    }

    /// Bug #1: a user-declared config key that aliases an existing grammar
    /// (e.g. `[languages.mylang] grammar = "Rust"`) must resolve via
    /// `find_by_name("mylang")` so the language palette can select it.
    #[test]
    fn test_user_alias_resolves_via_find_by_name() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        languages.insert("mylang".to_string(), lang_cfg("Rust", &[], &[]));
        registry.apply_language_config(&languages);

        let entry = registry
            .find_by_name("mylang")
            .expect("user-declared alias 'mylang' must resolve");
        assert_eq!(entry.display_name, "Rust");
    }

    /// Bug #2: `register_alias` used to rebuild the catalog from scratch,
    /// wiping out everything `apply_language_config` had merged. Registering
    /// an alias afterwards must not lose user config.
    #[test]
    fn test_register_alias_preserves_applied_language_config() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "shell-configs".to_string(),
            lang_cfg("bash", &["myconf"], &["*.myconf"]),
        );
        registry.apply_language_config(&languages);

        // Sanity: config applied.
        assert!(registry.find_by_extension("myconf").is_some());
        assert!(
            registry
                .find_by_path(Path::new("foo.myconf"), None)
                .is_some(),
            "glob should match before register_alias"
        );

        // Registering an alias must not erase the config we just applied.
        registry.register_alias("mycustom", "Rust");

        assert!(
            registry.find_by_extension("myconf").is_some(),
            "config extension must survive register_alias"
        );
        assert!(
            registry
                .find_by_path(Path::new("foo.myconf"), None)
                .is_some(),
            "glob must survive register_alias"
        );
    }

    /// Bug #4: `from_syntax_name` used to unconditionally overwrite the
    /// catalog's canonical display name with whatever the user typed (e.g.
    /// "BASH") — that string ended up in the status bar.
    #[test]
    fn test_from_syntax_name_preserves_canonical_display_name() {
        use crate::primitives::detected_language::DetectedLanguage;
        let registry = GrammarRegistry::default();
        let languages = std::collections::HashMap::new();

        let detected = DetectedLanguage::from_syntax_name("BASH", &registry, &languages)
            .expect("BASH should resolve via alias");
        assert_eq!(
            detected.display_name, "Bourne Again Shell (bash)",
            "display_name must be canonical, not user-typed"
        );
    }

    /// A config-only language (no matching syntect grammar) must still appear
    /// in the catalog so the language palette can offer it — the old
    /// `DetectedLanguage::from_config_language` branch was load-bearing.
    #[test]
    fn test_config_only_language_appears_in_catalog() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        // "fish" isn't in syntect; grammar="fish" doesn't resolve either.
        languages.insert("fish".to_string(), lang_cfg("fish", &["fish"], &[]));
        registry.apply_language_config(&languages);

        let entry = registry
            .find_by_name("fish")
            .expect("fish should be in the catalog after apply_language_config");
        assert!(entry.engines.syntect.is_none());
        assert!(entry.engines.tree_sitter.is_none());
        assert_eq!(entry.language_id, "fish");
        assert!(entry.extensions.iter().any(|e| e == "fish"));
    }

    /// Config-declared extensions must override the built-in mapping. If the
    /// user says `[languages.typescript-overlay] extensions = ["js"] grammar
    /// = "TypeScript"`, then `foo.js` must resolve to TypeScript, not
    /// JavaScript.
    #[test]
    fn test_config_extension_overrides_builtin() {
        let mut registry = GrammarRegistry::default();
        // Sanity: default mapping is JavaScript.
        assert_eq!(
            registry.find_by_extension("js").unwrap().display_name,
            "JavaScript"
        );

        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "ts-overlay".to_string(),
            lang_cfg("TypeScript", &["js"], &[]),
        );
        registry.apply_language_config(&languages);

        assert_eq!(
            registry.find_by_extension("js").unwrap().display_name,
            "TypeScript",
            "user-config extension must win over built-in"
        );
    }

    /// Bare filenames listed by syntect grammars (e.g. "Gemfile", "Makefile",
    /// "Rakefile") must resolve through `find_by_path`. Syntect stores these
    /// in each grammar's `file_extensions` field alongside real extensions
    /// like "rb"; its own `find_syntax_for_file` treats them as either. The
    /// catalog has to do the same or `HighlightEngine::for_file` breaks for
    /// every extensionless config file.
    #[test]
    fn test_bare_filename_resolves_via_find_by_path() {
        let registry = GrammarRegistry::default();
        for (filename, expected_substr) in [
            ("Gemfile", "ruby"),
            ("Rakefile", "ruby"),
            ("Vagrantfile", "ruby"),
            ("Makefile", "makefile"),
            ("GNUmakefile", "makefile"),
        ] {
            let entry = registry
                .find_by_path(Path::new(filename), None)
                .unwrap_or_else(|| panic!("{} must resolve via catalog", filename));
            assert!(
                entry.display_name.to_lowercase().contains(expected_substr),
                "{} should resolve to {} grammar, got {}",
                filename,
                expected_substr,
                entry.display_name
            );
        }
    }

    /// Languages that have both syntect and tree-sitter (e.g. JavaScript) must
    /// expose the union of both engines' extensions. Tree-sitter-javascript
    /// knows `.jsx`; syntect's JavaScript grammar does not. Both should route
    /// through the JavaScript catalog entry.
    #[test]
    fn test_jsx_resolves_to_javascript() {
        let registry = GrammarRegistry::default();
        let entry = registry
            .find_by_path(Path::new("foo.jsx"), None)
            .expect("foo.jsx must resolve");
        assert_eq!(entry.display_name, "JavaScript");
    }

    /// `rebuild_catalog` must replay the last-applied language config so it
    /// can never silently wipe user `[languages]` rules. This is the invariant
    /// that keeps `register_alias`, `populate_built_in_aliases`, and any
    /// future rebuild callsite safe-by-construction.
    #[test]
    fn test_rebuild_catalog_replays_language_config() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "myshell".to_string(),
            lang_cfg("bash", &["myext"], &["*.myglob"]),
        );
        registry.apply_language_config(&languages);
        assert!(registry.find_by_extension("myext").is_some());
        assert!(registry
            .find_by_path(Path::new("foo.myglob"), None)
            .is_some());

        // Force a rebuild — the catalog gets wiped and re-populated from
        // syntect / tree-sitter, but user config must come back on top.
        registry.rebuild_catalog();
        assert!(
            registry.find_by_extension("myext").is_some(),
            "rebuild_catalog must replay applied user config"
        );
        assert!(
            registry
                .find_by_path(Path::new("foo.myglob"), None)
                .is_some(),
            "rebuild_catalog must replay user globs"
        );
    }

    /// `apply_language_config` must be idempotent: calling it twice with the
    /// same config yields the same catalog state.
    #[test]
    fn test_apply_language_config_idempotent() {
        let mut registry = GrammarRegistry::default();
        let mut languages = std::collections::HashMap::new();
        languages.insert(
            "shell-cfg".to_string(),
            lang_cfg("bash", &["myconf"], &["*.myconf"]),
        );

        registry.apply_language_config(&languages);
        let first_extensions = registry
            .find_by_name("bash")
            .unwrap()
            .extensions
            .iter()
            .filter(|e| e == &"myconf")
            .count();
        let first_globs = registry
            .find_by_name("bash")
            .unwrap()
            .filename_globs
            .iter()
            .filter(|g| g == &"*.myconf")
            .count();
        assert_eq!(first_extensions, 1);
        assert_eq!(first_globs, 1);

        // Second call must not duplicate anything.
        registry.apply_language_config(&languages);
        let second_extensions = registry
            .find_by_name("bash")
            .unwrap()
            .extensions
            .iter()
            .filter(|e| e == &"myconf")
            .count();
        let second_globs = registry
            .find_by_name("bash")
            .unwrap()
            .filename_globs
            .iter()
            .filter(|g| g == &"*.myconf")
            .count();
        assert_eq!(second_extensions, 1, "extensions must not duplicate");
        assert_eq!(second_globs, 1, "globs must not duplicate");
    }

    /// `tree_sitter_for_syntect_name` handles the alias table + strict
    /// display-name match. The alias table catches syntect's verbose names;
    /// the strict match handles the common case.
    #[test]
    fn test_tree_sitter_bridge() {
        assert_eq!(
            tree_sitter_for_syntect_name("Bourne Again Shell (bash)"),
            Some(fresh_languages::Language::Bash)
        );
        assert_eq!(
            tree_sitter_for_syntect_name("Rust"),
            Some(fresh_languages::Language::Rust)
        );
        // Must NOT fuzzy-match Nushell to Bash.
        assert_eq!(tree_sitter_for_syntect_name("Nushell"), None);
        // Must NOT match arbitrary strings.
        assert_eq!(tree_sitter_for_syntect_name("does-not-exist"), None);
    }
}
