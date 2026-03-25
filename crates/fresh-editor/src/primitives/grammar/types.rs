//! Pure grammar registry types without I/O operations.
//!
//! This module contains the `GrammarRegistry` struct and all syntax lookup methods
//! that don't require filesystem access. This enables WASM compatibility and easier testing.

use serde::Deserialize;
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
}

impl GrammarRegistry {
    /// Create a new GrammarRegistry from pre-built components.
    ///
    /// This is typically called by `GrammarLoader` implementations after
    /// loading grammars from various sources.
    pub fn new(
        syntax_set: SyntaxSet,
        user_extensions: HashMap<String, String>,
        filename_scopes: HashMap<String, String>,
    ) -> Self {
        Self::new_with_loaded_paths(syntax_set, user_extensions, filename_scopes, Vec::new())
    }

    /// Create a GrammarRegistry with pre-loaded grammar path tracking.
    ///
    /// Used by the loader when plugin grammars were included in the initial build,
    /// so that `loaded_grammar_paths()` reflects what was actually loaded.
    pub fn new_with_loaded_paths(
        syntax_set: SyntaxSet,
        user_extensions: HashMap<String, String>,
        filename_scopes: HashMap<String, String>,
        loaded_grammar_paths: Vec<GrammarSpec>,
    ) -> Self {
        Self {
            syntax_set: Arc::new(syntax_set),
            user_extensions,
            filename_scopes,
            loaded_grammar_paths,
        }
    }

    /// Create an empty grammar registry (fast, for tests that don't need syntax highlighting)
    pub fn empty() -> Arc<Self> {
        let mut builder = SyntaxSetBuilder::new();
        builder.add_plain_text_syntax();
        Arc::new(Self {
            syntax_set: Arc::new(builder.build()),
            user_extensions: HashMap::new(),
            filename_scopes: HashMap::new(),
            loaded_grammar_paths: Vec::new(),
        })
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
        let filename_scopes = Self::build_filename_scopes();
        let extra_extensions = Self::build_extra_extensions();
        Arc::new(Self {
            syntax_set: Arc::new(syntax_set),
            user_extensions: extra_extensions,
            filename_scopes,
            loaded_grammar_paths: Vec::new(),
        })
    }

    /// Build extra extension -> scope mappings for extensions not covered by syntect defaults.
    ///
    /// These map common file extensions to existing syntect grammar scopes,
    /// filling gaps where syntect's built-in extension lists are incomplete.
    pub fn build_extra_extensions() -> HashMap<String, String> {
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
    pub fn build_filename_scopes() -> HashMap<String, String> {
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
    pub fn add_embedded_grammars(builder: &mut SyntaxSetBuilder) {
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
    /// Checks in order:
    /// 1. User-configured grammar extensions (by scope)
    /// 2. By extension (includes built-in + embedded grammars)
    /// 3. By filename (custom dotfile mappings like .zshrc)
    /// 4. By filename via syntect (handles Makefile, .bashrc, etc.)
    pub fn find_syntax_for_file(&self, path: &Path) -> Option<&SyntaxReference> {
        // Try filename-based lookup FIRST for dotfiles, special files, and exact matches
        // This must come before extension lookup since files like CMakeLists.txt
        // would otherwise match Plain Text via the .txt extension.
        if let Some(filename) = path.file_name().and_then(|n| n.to_str()) {
            if let Some(scope) = self.filename_scopes.get(filename) {
                if let Some(syntax) = syntect::parsing::Scope::new(scope)
                    .ok()
                    .and_then(|s| self.syntax_set.find_syntax_by_scope(s))
                {
                    return Some(syntax);
                }
            }
        }

        // Try extension-based lookup
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            // Check user grammars first (higher priority)
            if let Some(scope) = self.user_extensions.get(ext) {
                tracing::info!("[SYNTAX DEBUG] find_syntax_for_file: found ext '{}' in user_extensions -> scope '{}'", ext, scope);
                if let Some(syntax) = syntect::parsing::Scope::new(scope)
                    .ok()
                    .and_then(|s| self.syntax_set.find_syntax_by_scope(s))
                {
                    tracing::info!(
                        "[SYNTAX DEBUG] find_syntax_for_file: found syntax by scope: {}",
                        syntax.name
                    );
                    return Some(syntax);
                } else {
                    tracing::info!(
                        "[SYNTAX DEBUG] find_syntax_for_file: scope '{}' not found in syntax_set",
                        scope
                    );
                }
            } else {
                tracing::info!(
                    "[SYNTAX DEBUG] find_syntax_for_file: ext '{}' NOT in user_extensions",
                    ext
                );
            }

            // Try extension lookup (includes embedded grammars like TOML)
            if let Some(syntax) = self.syntax_set.find_syntax_by_extension(ext) {
                tracing::info!(
                    "[SYNTAX DEBUG] find_syntax_for_file: found by syntect extension: {}",
                    syntax.name
                );
                return Some(syntax);
            }
        }

        // Filename-based lookup already done above (before extension lookup)

        // Try syntect's full file detection (handles special filenames like Makefile)
        // This may do I/O for first-line detection, but handles many cases
        if let Ok(Some(syntax)) = self.syntax_set.find_syntax_for_file(path) {
            return Some(syntax);
        }

        tracing::info!(
            "[SYNTAX DEBUG] find_syntax_for_file: no syntax found for {:?}",
            path
        );
        None
    }

    /// Find syntax for a file, checking user-configured languages first.
    ///
    /// This method extends `find_syntax_for_file` by first checking the provided
    /// languages configuration for filename and extension matches. This allows
    /// users to configure custom filename patterns (like PKGBUILD for bash) that
    /// will be respected for syntax highlighting.
    ///
    /// Checks in order:
    /// 1. User-configured language filenames from config (exact match)
    /// 2. User-configured language filenames from config (glob patterns)
    /// 3. User-configured language extensions from config
    /// 4. Falls back to `find_syntax_for_file` for built-in detection
    pub fn find_syntax_for_file_with_languages(
        &self,
        path: &Path,
        languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
    ) -> Option<&SyntaxReference> {
        let extension = path.extension().and_then(|e| e.to_str());
        tracing::info!(
            "[SYNTAX DEBUG] find_syntax_for_file_with_languages: path={:?}, ext={:?}, languages_config_keys={:?}",
            path,
            extension,
            languages.keys().collect::<Vec<_>>()
        );

        // Try filename match from languages config first (exact then glob)
        if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
            // First pass: exact matches only (highest priority)
            for (lang_name, lang_config) in languages.iter() {
                if lang_config
                    .filenames
                    .iter()
                    .any(|f| !is_glob_pattern(f) && f == filename)
                {
                    tracing::info!(
                        "[SYNTAX DEBUG] filename match: {} -> grammar '{}'",
                        lang_name,
                        lang_config.grammar
                    );
                    if let Some(syntax) = self.find_syntax_for_lang_config(lang_config) {
                        return Some(syntax);
                    }
                }
            }

            // Second pass: glob pattern matches
            // Path patterns (containing `/`) are matched against the full path;
            // filename-only patterns are matched against just the filename.
            let path_str = path.to_str().unwrap_or("");
            for (lang_name, lang_config) in languages.iter() {
                if lang_config.filenames.iter().any(|f| {
                    if !is_glob_pattern(f) {
                        return false;
                    }
                    if is_path_pattern(f) {
                        path_glob_matches(f, path_str)
                    } else {
                        filename_glob_matches(f, filename)
                    }
                }) {
                    tracing::info!(
                        "[SYNTAX DEBUG] filename glob match: {} -> grammar '{}'",
                        lang_name,
                        lang_config.grammar
                    );
                    if let Some(syntax) = self.find_syntax_for_lang_config(lang_config) {
                        return Some(syntax);
                    }
                }
            }
        }

        // Try extension match from languages config
        if let Some(extension) = extension {
            for (lang_name, lang_config) in languages.iter() {
                if lang_config.extensions.iter().any(|ext| ext == extension) {
                    tracing::info!(
                        "[SYNTAX DEBUG] extension match in config: ext={}, lang={}, grammar='{}'",
                        extension,
                        lang_name,
                        lang_config.grammar
                    );
                    // Found a match - try to find syntax by grammar name
                    if let Some(syntax) = self.find_syntax_by_name(&lang_config.grammar) {
                        tracing::info!(
                            "[SYNTAX DEBUG] found syntax by grammar name: {}",
                            syntax.name
                        );
                        return Some(syntax);
                    } else {
                        tracing::info!(
                            "[SYNTAX DEBUG] grammar name '{}' not found in registry",
                            lang_config.grammar
                        );
                    }
                }
            }
        }

        // Fall back to built-in detection
        tracing::info!("[SYNTAX DEBUG] falling back to find_syntax_for_file");
        let result = self.find_syntax_for_file(path);
        tracing::info!(
            "[SYNTAX DEBUG] find_syntax_for_file result: {:?}",
            result.map(|s| &s.name)
        );
        result
    }

    /// Given a language config, find the syntax reference for it.
    ///
    /// Tries grammar name first, then falls back to extension-based lookup.
    /// This handles cases where the grammar name doesn't match syntect's name
    /// (e.g., grammar `"c_sharp"` maps to syntect syntax `"C#"` via `.cs` extension).
    pub fn find_syntax_for_lang_config(
        &self,
        lang_config: &crate::config::LanguageConfig,
    ) -> Option<&SyntaxReference> {
        if let Some(syntax) = self.find_syntax_by_name(&lang_config.grammar) {
            tracing::info!(
                "[SYNTAX DEBUG] found syntax by grammar name: {}",
                syntax.name
            );
            return Some(syntax);
        }
        // Also try finding by extension if grammar name didn't work
        // (some grammars are named differently)
        if !lang_config.extensions.is_empty() {
            if let Some(ext) = lang_config.extensions.first() {
                if let Some(syntax) = self.syntax_set.find_syntax_by_extension(ext) {
                    tracing::info!(
                        "[SYNTAX DEBUG] found syntax by extension fallback: {}",
                        syntax.name
                    );
                    return Some(syntax);
                }
            }
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

    /// Find syntax by name (case-insensitive)
    ///
    /// This allows config files to use lowercase grammar names like "go" while
    /// matching syntect's actual names like "Go".
    pub fn find_syntax_by_name(&self, name: &str) -> Option<&SyntaxReference> {
        // Try exact match first
        if let Some(syntax) = self.syntax_set.find_syntax_by_name(name) {
            return Some(syntax);
        }
        // Fall back to case-insensitive match
        let name_lower = name.to_lowercase();
        self.syntax_set
            .syntaxes()
            .iter()
            .find(|s| s.name.to_lowercase() == name_lower)
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

    /// Debug helper: get user extensions as a string for logging
    pub fn user_extensions_debug(&self) -> String {
        format!("{:?}", self.user_extensions.keys().collect::<Vec<_>>())
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

    /// Get the user extensions mapping (extension -> scope name)
    pub fn user_extensions(&self) -> &HashMap<String, String> {
        &self.user_extensions
    }

    /// Get the filename scopes mapping (filename -> scope name)
    pub fn filename_scopes(&self) -> &HashMap<String, String> {
        &self.filename_scopes
    }

    /// Get the loaded grammar paths (for deduplication in flush_pending_grammars)
    pub fn loaded_grammar_paths(&self) -> &[GrammarSpec] {
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
                    tracing::info!(
                        "[SYNTAX DEBUG] grammar loaded successfully: name='{}', scope='{}'",
                        syntax.name,
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

        Some(Self {
            syntax_set: Arc::new(builder.build()),
            user_extensions,
            filename_scopes: base.filename_scopes.clone(),
            loaded_grammar_paths,
        })
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

        Self::new(syntax_set, extra_extensions, filename_scopes)
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
        let registry = GrammarRegistry::default();
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
            },
        );

        // *.conf should match
        let result =
            registry.find_syntax_for_file_with_languages(Path::new("nftables.conf"), &languages);
        assert!(result.is_some(), "*.conf should match nftables.conf");

        // *rc should match
        let result = registry.find_syntax_for_file_with_languages(Path::new("lfrc"), &languages);
        assert!(result.is_some(), "*rc should match lfrc");

        // Unrelated file should not match via glob
        let result =
            registry.find_syntax_for_file_with_languages(Path::new("randomfile"), &languages);
        // May still match via built-in detection, but not via our config
        // Just verify it doesn't panic
        let _ = result;
    }

    #[test]
    fn test_find_syntax_with_path_glob_filenames() {
        let registry = GrammarRegistry::default();
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
            },
        );

        // /etc/**/rc.* should match via full path
        let result =
            registry.find_syntax_for_file_with_languages(Path::new("/etc/rc.conf"), &languages);
        assert!(result.is_some(), "/etc/**/rc.* should match /etc/rc.conf");

        let result = registry
            .find_syntax_for_file_with_languages(Path::new("/etc/init/rc.local"), &languages);
        assert!(
            result.is_some(),
            "/etc/**/rc.* should match /etc/init/rc.local"
        );

        // Should NOT match a different root
        let result =
            registry.find_syntax_for_file_with_languages(Path::new("/var/rc.conf"), &languages);
        // /var/rc.conf won't match the path glob, but may match built-in detection
        // Just verify no panic
        let _ = result;
    }

    #[test]
    fn test_exact_filename_takes_priority_over_glob() {
        let registry = GrammarRegistry::default();
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
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
                highlighter: crate::config::HighlighterPreference::Auto,
                textmate_grammar: None,
                show_whitespace_tabs: true,
                use_tabs: None,
                tab_size: None,
                formatter: None,
                format_on_save: false,
                on_save: vec![],
            },
        );

        // "lfrc" should match the exact rule (python), not the glob (bash)
        let result = registry.find_syntax_for_file_with_languages(Path::new("lfrc"), &languages);
        assert!(result.is_some());
        let syntax = result.unwrap();
        assert!(
            syntax.name.to_lowercase().contains("python"),
            "exact match should win over glob, got: {}",
            syntax.name
        );
    }
}
