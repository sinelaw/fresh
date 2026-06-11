use std::path::Path;

// Re-export tree-sitter crates for use by fresh-editor
pub use tree_sitter;
pub use tree_sitter_highlight;
pub use tree_sitter_highlight::HighlightConfiguration;

// Re-export the bundled language grammar crates (gated by features). Only the
// languages that must use tree-sitter because syntect ships no highlighting
// for them are bundled; the rest were removed (see Cargo.toml).
#[cfg(feature = "tree-sitter-go")]
pub use tree_sitter_go;
#[cfg(feature = "tree-sitter-javascript")]
pub use tree_sitter_javascript;
#[cfg(feature = "tree-sitter-json")]
pub use tree_sitter_json;
#[cfg(feature = "tree-sitter-templ")]
pub use tree_sitter_templ;
#[cfg(feature = "tree-sitter-typescript")]
pub use tree_sitter_typescript;

/// Highlight category names used for default languages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightCategory {
    Attribute,
    Comment,
    Constant,
    Function,
    Keyword,
    Number,
    Operator,
    PunctuationBracket,
    PunctuationDelimiter,
    Property,
    String,
    Type,
    Variable,
    VariableBuiltin,
    /// `markup.inserted.*` — added lines in a diff. The renderer
    /// fills the whole row's background with the theme's
    /// `editor.diff_add_bg`. Foreground stays default so the row
    /// stays readable.
    Inserted,
    /// `markup.deleted.*` — removed lines. Background fill from
    /// `editor.diff_remove_bg`.
    Deleted,
    /// `meta.diff.range.*` / `markup.changed.*` — hunk header rows
    /// and any "changed" markers. Background fill from
    /// `editor.diff_modify_bg`.
    Changed,
}

impl HighlightCategory {
    /// Whether this category's background fill should extend past
    /// the scoped text to the end of the visible row.
    ///
    /// Syntect's `Diff` grammar scopes each `+`/`-`/`@@` line up to
    /// the trailing newline; without this flag the renderer would
    /// stop the bg wash at the row's last character, leaving short
    /// rows half-coloured.
    pub fn bg_extends_to_line_end(&self) -> bool {
        matches!(self, Self::Inserted | Self::Deleted | Self::Changed)
    }

    /// Map a default language highlight index to a category
    pub fn from_default_index(index: usize) -> Option<Self> {
        match index {
            0 => Some(Self::Attribute),
            1 => Some(Self::Comment),
            2 => Some(Self::Constant),
            3 => Some(Self::Function),
            4 => Some(Self::Keyword),
            5 => Some(Self::Number),
            6 => Some(Self::Operator),
            7 => Some(Self::PunctuationBracket),
            8 => Some(Self::PunctuationDelimiter),
            9 => Some(Self::Property),
            10 => Some(Self::String),
            11 => Some(Self::Type),
            12 => Some(Self::Variable),
            13 => Some(Self::VariableBuiltin),
            _ => None,
        }
    }

    /// Map a TypeScript highlight index to a category.
    pub fn from_typescript_index(index: usize) -> Option<Self> {
        match index {
            0 => Some(Self::Attribute),             // attribute
            1 => Some(Self::Comment),               // comment
            2 => Some(Self::Constant),              // constant
            3 => Some(Self::Constant),              // constant.builtin
            4 => Some(Self::Type),                  // constructor
            5 => Some(Self::String),                // embedded (template substitutions)
            6 => Some(Self::Function),              // function
            7 => Some(Self::Function),              // function.builtin
            8 => Some(Self::Function),              // function.method
            9 => Some(Self::Keyword),               // keyword
            10 => Some(Self::Number),               // number
            11 => Some(Self::Operator),             // operator
            12 => Some(Self::Property),             // property
            13 => Some(Self::PunctuationBracket),   // punctuation.bracket
            14 => Some(Self::PunctuationDelimiter), // punctuation.delimiter
            15 => Some(Self::Constant),             // punctuation.special (template ${})
            16 => Some(Self::String),               // string
            17 => Some(Self::String),               // string.special (regex)
            18 => Some(Self::Type),                 // type
            19 => Some(Self::Type),                 // type.builtin
            20 => Some(Self::Variable),             // variable
            21 => Some(Self::VariableBuiltin),      // variable.builtin (this, super, arguments)
            22 => Some(Self::Variable),             // variable.parameter
            _ => None,
        }
    }

    /// Get the theme key path for this category (e.g., "syntax.keyword").
    pub fn theme_key(&self) -> &'static str {
        match self {
            Self::Keyword => "syntax.keyword",
            Self::String => "syntax.string",
            Self::Comment => "syntax.comment",
            Self::Function => "syntax.function",
            Self::Type => "syntax.type",
            Self::Variable | Self::Property => "syntax.variable",
            Self::VariableBuiltin => "syntax.variable_builtin",
            Self::Constant | Self::Number | Self::Attribute => "syntax.constant",
            Self::Operator => "syntax.operator",
            Self::PunctuationBracket => "syntax.punctuation_bracket",
            Self::PunctuationDelimiter => "syntax.punctuation_delimiter",
            // Diff categories are bg-driven; the inspector surfaces
            // the existing editor-level diff keys (also used by
            // live_diff / side-by-side diff) rather than a separate
            // syntax.* key.
            Self::Inserted => "editor.diff_add_bg",
            Self::Deleted => "editor.diff_remove_bg",
            Self::Changed => "editor.diff_modify_bg",
        }
    }

    /// Get a human-readable display name for this category.
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Attribute => "Attribute",
            Self::Comment => "Comment",
            Self::Constant => "Constant",
            Self::Function => "Function",
            Self::Keyword => "Keyword",
            Self::Number => "Number",
            Self::Operator => "Operator",
            Self::PunctuationBracket => "Punctuation Bracket",
            Self::PunctuationDelimiter => "Punctuation Delimiter",
            Self::Property => "Property",
            Self::String => "String",
            Self::Type => "Type",
            Self::Variable => "Variable",
            Self::VariableBuiltin => "Variable (Builtin)",
            Self::Inserted => "Diff Inserted",
            Self::Deleted => "Diff Deleted",
            Self::Changed => "Diff Changed",
        }
    }
}

/// Language configuration for syntax highlighting
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Language {
    Rust,
    Python,
    JavaScript,
    TypeScript,
    HTML,
    CSS,
    C,
    Cpp,
    Go,
    Json,
    Jsonc,
    Java,
    CSharp,
    Php,
    Ruby,
    Bash,
    Fish,
    Lua,
    Pascal,
    Odin,
    Templ,
}

impl Language {
    /// Detect language from file extension.
    ///
    /// Derived from `extensions()` — see `Self::all` / `Self::extensions` for
    /// the authoritative table. A linear scan over ~18 languages is cheap
    /// enough that the nicer invariant (no duplicate tables) beats a match.
    pub fn from_path(path: &Path) -> Option<Self> {
        let ext = path.extension()?.to_str()?;
        Self::all()
            .iter()
            .find(|lang| lang.extensions().contains(&ext))
            .copied()
    }

    /// Get tree-sitter highlight configuration for this language
    pub fn highlight_config(&self) -> Result<HighlightConfiguration, String> {
        match self {
            Self::JavaScript => {
                #[cfg(feature = "tree-sitter-javascript")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_javascript::LANGUAGE.into(),
                        "javascript",
                        tree_sitter_javascript::HIGHLIGHT_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create JavaScript highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-javascript"))]
                Err("JavaScript language support not enabled".to_string())
            }
            Self::TypeScript => {
                #[cfg(all(feature = "tree-sitter-typescript", feature = "tree-sitter-javascript"))]
                {
                    let combined_highlights = format!(
                        "{}\n{}",
                        tree_sitter_typescript::HIGHLIGHTS_QUERY,
                        tree_sitter_javascript::HIGHLIGHT_QUERY
                    );
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
                        "typescript",
                        &combined_highlights,
                        "",
                        tree_sitter_typescript::LOCALS_QUERY,
                    )
                    .map_err(|e| format!("Failed to create TypeScript highlight config: {e}"))?;
                    config.configure(TYPESCRIPT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(all(
                    feature = "tree-sitter-typescript",
                    feature = "tree-sitter-javascript"
                )))]
                Err("TypeScript language support not enabled".to_string())
            }
            Self::Go => {
                #[cfg(feature = "tree-sitter-go")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_go::LANGUAGE.into(),
                        "go",
                        tree_sitter_go::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Go highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-go"))]
                Err("Go language support not enabled".to_string())
            }
            Self::Json => {
                #[cfg(feature = "tree-sitter-json")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_json::LANGUAGE.into(),
                        "json",
                        tree_sitter_json::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create JSON highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-json"))]
                Err("JSON language support not enabled".to_string())
            }
            Self::Jsonc => {
                // JSONC (JSON with Comments) reuses the tree-sitter-json parser.
                // A dedicated JSONC grammar isn't published as a Rust crate; the
                // JSON parser recovers past comments and trailing commas well
                // enough for highlighting, which is the only consumer here.
                #[cfg(feature = "tree-sitter-json")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_json::LANGUAGE.into(),
                        "jsonc",
                        tree_sitter_json::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create JSONC highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-json"))]
                Err("JSONC language support not enabled".to_string())
            }
            Self::Templ => {
                // The templ grammar extends Go (see vrischmann/tree-sitter-templ),
                // so combining Go's highlights query with the templ-specific one
                // gives us reasonable highlighting for both the Go expressions
                // and the templ-specific component / element / CSS syntax.
                #[cfg(feature = "tree-sitter-templ")]
                {
                    let combined_highlights = format!(
                        "{}\n{}",
                        tree_sitter_go::HIGHLIGHTS_QUERY,
                        TEMPL_HIGHLIGHTS_QUERY,
                    );
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_templ::LANGUAGE.into(),
                        "templ",
                        &combined_highlights,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Templ highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-templ"))]
                Err("Templ language support not enabled".to_string())
            }
            // Every other language is highlighted by syntect; no tree-sitter
            // grammar is bundled for it (see Cargo.toml and `ts_language`).
            _ => Err("no bundled tree-sitter grammar for this language".to_string()),
        }
    }

    /// Map tree-sitter highlight index to a highlight category
    pub fn highlight_category(&self, index: usize) -> Option<HighlightCategory> {
        match self {
            Self::TypeScript => HighlightCategory::from_typescript_index(index),
            _ => HighlightCategory::from_default_index(index),
        }
    }

    /// The tree-sitter parser `Language` for this language, or `None` when its
    /// grammar is not compiled into this build.
    ///
    /// This is the single chokepoint for per-grammar `#[cfg]`s: callers in
    /// fresh-editor (indentation, reference highlighting) stay feature-agnostic
    /// — a `None` simply means "no grammar, use the syntect / indent-rules
    /// fallbacks". Only the languages that *must* use tree-sitter because
    /// syntect ships no grammar for them (JavaScript, TypeScript, JSON-with-
    /// comments, Templ — plus Go, which Templ extends) are bundled by default;
    /// every other arm returns `None` unless the opt-in `all-languages` feature
    /// re-enables its grammar.
    pub fn ts_language(&self) -> Option<tree_sitter::Language> {
        match self {
            Self::JavaScript => {
                #[cfg(feature = "tree-sitter-javascript")]
                {
                    Some(tree_sitter_javascript::LANGUAGE.into())
                }
                #[cfg(not(feature = "tree-sitter-javascript"))]
                {
                    None
                }
            }
            Self::TypeScript => {
                #[cfg(feature = "tree-sitter-typescript")]
                {
                    Some(tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into())
                }
                #[cfg(not(feature = "tree-sitter-typescript"))]
                {
                    None
                }
            }
            Self::Go => {
                #[cfg(feature = "tree-sitter-go")]
                {
                    Some(tree_sitter_go::LANGUAGE.into())
                }
                #[cfg(not(feature = "tree-sitter-go"))]
                {
                    None
                }
            }
            Self::Json | Self::Jsonc => {
                #[cfg(feature = "tree-sitter-json")]
                {
                    Some(tree_sitter_json::LANGUAGE.into())
                }
                #[cfg(not(feature = "tree-sitter-json"))]
                {
                    None
                }
            }
            Self::Templ => {
                #[cfg(feature = "tree-sitter-templ")]
                {
                    Some(tree_sitter_templ::LANGUAGE.into())
                }
                #[cfg(not(feature = "tree-sitter-templ"))]
                {
                    None
                }
            }
            // Every other language is highlighted by syntect and indented by
            // the regex rules tier; no tree-sitter grammar is bundled for it.
            _ => None,
        }
    }
}

impl Language {
    /// Returns all available language variants
    pub fn all() -> &'static [Language] {
        &[
            Language::Rust,
            Language::Python,
            Language::JavaScript,
            Language::TypeScript,
            Language::HTML,
            Language::CSS,
            Language::C,
            Language::Cpp,
            Language::Go,
            Language::Json,
            Language::Jsonc,
            Language::Java,
            Language::CSharp,
            Language::Php,
            Language::Ruby,
            Language::Bash,
            Language::Fish,
            Language::Lua,
            Language::Pascal,
            Language::Odin,
            Language::Templ,
        ]
    }

    /// Returns the language ID (lowercase identifier used in config/internal)
    pub fn id(&self) -> &'static str {
        match self {
            Self::Rust => "rust",
            Self::Python => "python",
            Self::JavaScript => "javascript",
            Self::TypeScript => "typescript",
            Self::HTML => "html",
            Self::CSS => "css",
            Self::C => "c",
            Self::Cpp => "cpp",
            Self::Go => "go",
            Self::Json => "json",
            Self::Jsonc => "jsonc",
            Self::Java => "java",
            Self::CSharp => "csharp",
            Self::Php => "php",
            Self::Ruby => "ruby",
            Self::Bash => "bash",
            Self::Fish => "fish",
            Self::Lua => "lua",
            Self::Pascal => "pascal",
            Self::Odin => "odin",
            Self::Templ => "templ",
        }
    }

    /// Returns the LSP languageId for use in textDocument/didOpen.
    ///
    /// This considers the file extension to return the correct LSP-spec language ID.
    /// For example, `.tsx` files return `"typescriptreact"` instead of `"typescript"`,
    /// and `.jsx` files return `"javascriptreact"` instead of `"javascript"`.
    ///
    /// See: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentItem>
    pub fn lsp_language_id(&self, path: &Path) -> &'static str {
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
        match (self, ext) {
            (Self::TypeScript, "tsx") => "typescriptreact",
            (Self::JavaScript, "jsx") => "javascriptreact",
            _ => self.id(),
        }
    }

    /// File extensions associated with this language.
    ///
    /// Keep in sync with `from_path`. Used by the grammar catalog so that
    /// tree-sitter-only languages (like TypeScript) still advertise the
    /// extensions they can highlight.
    pub fn extensions(&self) -> &'static [&'static str] {
        match self {
            Self::Rust => &["rs"],
            Self::Python => &["py"],
            Self::JavaScript => &["js", "jsx", "mjs", "cjs"],
            Self::TypeScript => &["ts", "tsx", "mts", "cts"],
            Self::HTML => &["html"],
            Self::CSS => &["css"],
            Self::C => &["c", "h"],
            Self::Cpp => &["cpp", "hpp", "cc", "hh", "cxx", "hxx", "cppm", "ixx"],
            Self::Go => &["go"],
            Self::Json => &["json"],
            Self::Jsonc => &["jsonc"],
            Self::Java => &["java"],
            Self::CSharp => &["cs"],
            Self::Php => &["php"],
            Self::Ruby => &["rb"],
            Self::Bash => &["sh", "bash"],
            Self::Fish => &["fish"],
            Self::Lua => &["lua"],
            Self::Pascal => &["pas", "p"],
            Self::Odin => &["odin"],
            Self::Templ => &["templ"],
        }
    }

    /// Returns the human-readable display name
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Rust => "Rust",
            Self::Python => "Python",
            Self::JavaScript => "JavaScript",
            Self::TypeScript => "TypeScript",
            Self::HTML => "HTML",
            Self::CSS => "CSS",
            Self::C => "C",
            Self::Cpp => "C++",
            Self::Go => "Go",
            Self::Json => "JSON",
            Self::Jsonc => "JSON with Comments",
            Self::Java => "Java",
            Self::CSharp => "C#",
            Self::Php => "PHP",
            Self::Ruby => "Ruby",
            Self::Bash => "Bash",
            Self::Fish => "Fish",
            Self::Lua => "Lua",
            Self::Pascal => "Pascal",
            Self::Odin => "Odin",
            Self::Templ => "Templ",
        }
    }

    /// Parse a language from its ID or display name
    pub fn from_id(id: &str) -> Option<Self> {
        let id_lower = id.to_lowercase();
        match id_lower.as_str() {
            "rust" => Some(Self::Rust),
            "python" => Some(Self::Python),
            "javascript" => Some(Self::JavaScript),
            "typescript" => Some(Self::TypeScript),
            "html" => Some(Self::HTML),
            "css" => Some(Self::CSS),
            "c" => Some(Self::C),
            "cpp" | "c++" => Some(Self::Cpp),
            "go" => Some(Self::Go),
            "json" => Some(Self::Json),
            "jsonc" => Some(Self::Jsonc),
            "java" => Some(Self::Java),
            "c_sharp" | "c#" | "csharp" => Some(Self::CSharp),
            "php" => Some(Self::Php),
            "ruby" => Some(Self::Ruby),
            "bash" => Some(Self::Bash),
            "fish" => Some(Self::Fish),
            "lua" => Some(Self::Lua),
            "pascal" => Some(Self::Pascal),
            "odin" => Some(Self::Odin),
            "templ" => Some(Self::Templ),
            _ => None,
        }
    }

    /// Try to map a syntect syntax name to a tree-sitter Language.
    ///
    /// This is used to get tree-sitter features (indentation, semantic highlighting)
    /// when using a syntect grammar for syntax highlighting. This is best-effort since
    /// tree-sitter only supports ~18 languages while syntect supports 100+.
    ///
    /// Syntect uses names like "Rust", "Python", "JavaScript", "JSON", "C++", "C#",
    /// "Bourne Again Shell (bash)", etc.
    pub fn from_name(name: &str) -> Option<Self> {
        // First try exact display name match
        for lang in Self::all() {
            if lang.display_name() == name {
                return Some(*lang);
            }
        }

        // Then try case-insensitive matching and common aliases
        let name_lower = name.to_lowercase();
        match name_lower.as_str() {
            "rust" => Some(Self::Rust),
            "python" => Some(Self::Python),
            "javascript" | "javascript (babel)" => Some(Self::JavaScript),
            "typescript" | "typescriptreact" => Some(Self::TypeScript),
            "html" => Some(Self::HTML),
            "css" => Some(Self::CSS),
            "c" => Some(Self::C),
            "c++" => Some(Self::Cpp),
            "go" | "golang" => Some(Self::Go),
            "json" => Some(Self::Json),
            "jsonc" | "json with comments" => Some(Self::Jsonc),
            "java" => Some(Self::Java),
            "c#" => Some(Self::CSharp),
            "php" => Some(Self::Php),
            "ruby" => Some(Self::Ruby),
            "fish" => Some(Self::Fish),
            "lua" => Some(Self::Lua),
            "pascal" => Some(Self::Pascal),
            "odin" => Some(Self::Odin),
            "templ" => Some(Self::Templ),
            _ => {
                // Try matching shell variants
                if name_lower.contains("bash") || name_lower.contains("shell") {
                    return Some(Self::Bash);
                }
                None
            }
        }
    }
}

impl std::fmt::Display for Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id())
    }
}

// Used by every per-language `highlight_config` arm; each arm is gated by its
// own grammar feature, so with zero grammars enabled this is (legitimately)
// unused.
#[allow(dead_code)]
const DEFAULT_HIGHLIGHT_CAPTURES: &[&str] = &[
    "attribute",
    "comment",
    "constant",
    "function",
    "keyword",
    "number",
    "operator",
    "punctuation.bracket",
    "punctuation.delimiter",
    "property",
    "string",
    "type",
    "variable",
    "variable.builtin",
];

/// Templ-specific highlight rules, vendored from the upstream
/// `tree-sitter-templ` crate's `queries/templ/highlights.scm`. The crate ships
/// this file but does not re-export it as a public Rust constant, so we keep
/// our own copy and concatenate it with Go's highlights query (templ extends
/// the Go grammar) to obtain the final highlight configuration.
///
/// Captures that aren't in `DEFAULT_HIGHLIGHT_CAPTURES` (e.g. `@tag`,
/// `@function.method`) simply go un-styled — the `tree-sitter-highlight`
/// configurator drops unknown capture names and matches on prefix for the
/// known ones, so this still produces correct output.
#[cfg(feature = "tree-sitter-templ")]
const TEMPL_HIGHLIGHTS_QUERY: &str = include_str!("../queries/templ/highlights.scm");

// Only referenced by the TypeScript arm; unused when that grammar is disabled.
#[allow(dead_code)]
const TYPESCRIPT_HIGHLIGHT_CAPTURES: &[&str] = &[
    "attribute",
    "comment",
    "constant",
    "constant.builtin",
    "constructor",
    "embedded",
    "function",
    "function.builtin",
    "function.method",
    "keyword",
    "number",
    "operator",
    "property",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.special",
    "string",
    "string.special",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.parameter",
];

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_lsp_language_id_tsx() {
        let lang = Language::TypeScript;
        assert_eq!(
            lang.lsp_language_id(Path::new("app.tsx")),
            "typescriptreact"
        );
    }

    #[test]
    fn test_lsp_language_id_ts() {
        let lang = Language::TypeScript;
        assert_eq!(lang.lsp_language_id(Path::new("app.ts")), "typescript");
    }

    #[test]
    fn test_lsp_language_id_jsx() {
        let lang = Language::JavaScript;
        assert_eq!(
            lang.lsp_language_id(Path::new("component.jsx")),
            "javascriptreact"
        );
    }

    #[test]
    fn test_lsp_language_id_js() {
        let lang = Language::JavaScript;
        assert_eq!(lang.lsp_language_id(Path::new("app.js")), "javascript");
    }

    #[test]
    fn test_lsp_language_id_csharp() {
        let lang = Language::CSharp;
        assert_eq!(lang.lsp_language_id(Path::new("main.cs")), "csharp");
    }

    #[test]
    fn test_lsp_language_id_other_languages() {
        assert_eq!(Language::Rust.lsp_language_id(Path::new("main.rs")), "rust");
        assert_eq!(
            Language::Python.lsp_language_id(Path::new("script.py")),
            "python"
        );
        assert_eq!(Language::Go.lsp_language_id(Path::new("main.go")), "go");
    }

    #[test]
    fn test_csharp_id_matches_config_key() {
        // Language::id() must return "csharp" to match the config key
        // used for LSP server lookup and language detection.
        assert_eq!(Language::CSharp.id(), "csharp");
    }

    #[test]
    fn test_templ_detected_from_extension() {
        let path = Path::new("home.templ");
        assert!(matches!(Language::from_path(path), Some(Language::Templ)));
    }

    #[test]
    fn test_fish_detected_from_extension() {
        let path = Path::new("config.fish");
        assert!(matches!(Language::from_path(path), Some(Language::Fish)));
    }

    #[test]
    #[cfg(feature = "tree-sitter-templ")]
    fn test_templ_highlight_config_builds() {
        // The combined Go + templ highlights query must parse cleanly against
        // the templ grammar; otherwise opening a `.templ` file would fall
        // back to plain text instead of highlighting.
        Language::Templ
            .highlight_config()
            .expect("Templ highlight config should build");
    }

    /// Guard: `from_path` and `extensions()` must stay in sync — they used to
    /// be two hand-maintained tables with a "keep in sync" comment, which
    /// silently drifted when either was edited in isolation.
    #[test]
    fn test_from_path_matches_extensions() {
        for lang in Language::all() {
            for ext in lang.extensions() {
                let path = std::path::PathBuf::from(format!("x.{}", ext));
                let detected = Language::from_path(&path).unwrap_or_else(|| {
                    panic!(
                        "extension .{} listed by {:?} but from_path returned None",
                        ext, lang
                    )
                });
                assert_eq!(
                    detected, *lang,
                    "extension .{} listed by {:?} but from_path returned {:?}",
                    ext, lang, detected
                );
            }
        }
    }
}
