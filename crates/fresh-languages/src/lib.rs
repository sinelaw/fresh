use std::path::Path;

// Re-export tree-sitter crates for use by fresh-editor
pub use tree_sitter;
pub use tree_sitter_highlight;
pub use tree_sitter_highlight::HighlightConfiguration;

// Re-export language crates (gated by features)
#[cfg(feature = "tree-sitter-bash")]
pub use tree_sitter_bash;
#[cfg(feature = "tree-sitter-c")]
pub use tree_sitter_c;
#[cfg(feature = "tree-sitter-c-sharp")]
pub use tree_sitter_c_sharp;
#[cfg(feature = "tree-sitter-cpp")]
pub use tree_sitter_cpp;
#[cfg(feature = "tree-sitter-css")]
pub use tree_sitter_css;
#[cfg(feature = "tree-sitter-go")]
pub use tree_sitter_go;
#[cfg(feature = "tree-sitter-html")]
pub use tree_sitter_html;
#[cfg(feature = "tree-sitter-java")]
pub use tree_sitter_java;
#[cfg(feature = "tree-sitter-javascript")]
pub use tree_sitter_javascript;
#[cfg(feature = "tree-sitter-json")]
pub use tree_sitter_json;
#[cfg(feature = "tree-sitter-lua")]
pub use tree_sitter_lua;
#[cfg(feature = "tree-sitter-odin")]
pub use tree_sitter_odin;
#[cfg(feature = "tree-sitter-pascal")]
pub use tree_sitter_pascal;
#[cfg(feature = "tree-sitter-php")]
pub use tree_sitter_php;
#[cfg(feature = "tree-sitter-python")]
pub use tree_sitter_python;
#[cfg(feature = "tree-sitter-ruby")]
pub use tree_sitter_ruby;
#[cfg(feature = "tree-sitter-rust")]
pub use tree_sitter_rust;
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
    Property,
    String,
    Type,
    Variable,
}

impl HighlightCategory {
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
            7 => Some(Self::Property),
            8 => Some(Self::String),
            9 => Some(Self::Type),
            10 => Some(Self::Variable),
            _ => None,
        }
    }

    /// Map a TypeScript highlight index to a category.
    pub fn from_typescript_index(index: usize) -> Option<Self> {
        match index {
            0 => Some(Self::Attribute), // attribute
            1 => Some(Self::Comment),   // comment
            2 => Some(Self::Constant),  // constant
            3 => Some(Self::Constant),  // constant.builtin
            4 => Some(Self::Type),      // constructor
            5 => Some(Self::String),    // embedded (template substitutions)
            6 => Some(Self::Function),  // function
            7 => Some(Self::Function),  // function.builtin
            8 => Some(Self::Function),  // function.method
            9 => Some(Self::Keyword),   // keyword
            10 => Some(Self::Number),   // number
            11 => Some(Self::Operator), // operator
            12 => Some(Self::Property), // property
            13 => Some(Self::Operator), // punctuation.bracket
            14 => Some(Self::Operator), // punctuation.delimiter
            15 => Some(Self::Constant), // punctuation.special (template ${})
            16 => Some(Self::String),   // string
            17 => Some(Self::String),   // string.special (regex)
            18 => Some(Self::Type),     // type
            19 => Some(Self::Type),     // type.builtin
            20 => Some(Self::Variable), // variable
            21 => Some(Self::Constant), // variable.builtin (this, super, arguments)
            22 => Some(Self::Variable), // variable.parameter
            _ => None,
        }
    }
}

/// Language configuration for syntax highlighting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Java,
    CSharp,
    Php,
    Ruby,
    Bash,
    Lua,
    Pascal,
    Odin,
}

impl Language {
    /// Detect language from file extension
    pub fn from_path(path: &Path) -> Option<Self> {
        match path.extension()?.to_str()? {
            "rs" => Some(Language::Rust),
            "py" => Some(Language::Python),
            "js" | "jsx" => Some(Language::JavaScript),
            "ts" | "tsx" => Some(Language::TypeScript),
            "html" => Some(Language::HTML),
            "css" => Some(Language::CSS),
            "c" | "h" => Some(Language::C),
            "cpp" | "hpp" | "cc" | "hh" | "cxx" | "hxx" => Some(Language::Cpp),
            "go" => Some(Language::Go),
            "json" => Some(Language::Json),
            "java" => Some(Language::Java),
            "cs" => Some(Language::CSharp),
            "php" => Some(Language::Php),
            "rb" => Some(Language::Ruby),
            "sh" | "bash" => Some(Language::Bash),
            "lua" => Some(Language::Lua),
            "pas" | "p" => Some(Language::Pascal),
            "odin" => Some(Language::Odin),
            _ => None,
        }
    }

    /// Get tree-sitter highlight configuration for this language
    pub fn highlight_config(&self) -> Result<HighlightConfiguration, String> {
        match self {
            Self::Rust => {
                #[cfg(feature = "tree-sitter-rust")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_rust::LANGUAGE.into(),
                        "rust",
                        tree_sitter_rust::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Rust highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-rust"))]
                Err("Rust language support not enabled".to_string())
            }
            Self::Python => {
                #[cfg(feature = "tree-sitter-python")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_python::LANGUAGE.into(),
                        "python",
                        tree_sitter_python::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Python highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-python"))]
                Err("Python language support not enabled".to_string())
            }
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
            Self::HTML => {
                #[cfg(feature = "tree-sitter-html")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_html::LANGUAGE.into(),
                        "html",
                        tree_sitter_html::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create HTML highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-html"))]
                Err("HTML language support not enabled".to_string())
            }
            Self::CSS => {
                #[cfg(feature = "tree-sitter-css")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_css::LANGUAGE.into(),
                        "css",
                        tree_sitter_css::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create CSS highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-css"))]
                Err("CSS language support not enabled".to_string())
            }
            Self::C => {
                #[cfg(feature = "tree-sitter-c")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_c::LANGUAGE.into(),
                        "c",
                        tree_sitter_c::HIGHLIGHT_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create C highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-c"))]
                Err("C language support not enabled".to_string())
            }
            Self::Cpp => {
                #[cfg(feature = "tree-sitter-cpp")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_cpp::LANGUAGE.into(),
                        "cpp",
                        tree_sitter_cpp::HIGHLIGHT_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create C++ highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-cpp"))]
                Err("C++ language support not enabled".to_string())
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
            Self::Java => {
                #[cfg(feature = "tree-sitter-java")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_java::LANGUAGE.into(),
                        "java",
                        tree_sitter_java::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Java highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-java"))]
                Err("Java language support not enabled".to_string())
            }
            Self::CSharp => {
                #[cfg(feature = "tree-sitter-c-sharp")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_c_sharp::LANGUAGE.into(),
                        "c_sharp",
                        "",
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create C# highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-c-sharp"))]
                Err("C# language support not enabled".to_string())
            }
            Self::Php => {
                #[cfg(feature = "tree-sitter-php")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_php::LANGUAGE_PHP.into(),
                        "php",
                        tree_sitter_php::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create PHP highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-php"))]
                Err("PHP language support not enabled".to_string())
            }
            Self::Ruby => {
                #[cfg(feature = "tree-sitter-ruby")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_ruby::LANGUAGE.into(),
                        "ruby",
                        tree_sitter_ruby::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Ruby highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-ruby"))]
                Err("Ruby language support not enabled".to_string())
            }
            Self::Bash => {
                #[cfg(feature = "tree-sitter-bash")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_bash::LANGUAGE.into(),
                        "bash",
                        tree_sitter_bash::HIGHLIGHT_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Bash highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-bash"))]
                Err("Bash language support not enabled".to_string())
            }
            Self::Lua => {
                #[cfg(feature = "tree-sitter-lua")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_lua::LANGUAGE.into(),
                        "lua",
                        tree_sitter_lua::HIGHLIGHTS_QUERY,
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Lua highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-lua"))]
                Err("Lua language support not enabled".to_string())
            }
            Self::Pascal => {
                #[cfg(feature = "tree-sitter-pascal")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_pascal::LANGUAGE.into(),
                        "pascal",
                        "",
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Pascal highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-pascal"))]
                Err("Pascal language support not enabled".to_string())
            }
            Self::Odin => {
                #[cfg(feature = "tree-sitter-odin")]
                {
                    let mut config = HighlightConfiguration::new(
                        tree_sitter_odin::LANGUAGE.into(),
                        "odin",
                        "",
                        "",
                        "",
                    )
                    .map_err(|e| format!("Failed to create Odin highlight config: {e}"))?;
                    config.configure(DEFAULT_HIGHLIGHT_CAPTURES);
                    Ok(config)
                }
                #[cfg(not(feature = "tree-sitter-odin"))]
                Err("Odin language support not enabled".to_string())
            }
        }
    }

    /// Map tree-sitter highlight index to a highlight category
    pub fn highlight_category(&self, index: usize) -> Option<HighlightCategory> {
        match self {
            Self::TypeScript => HighlightCategory::from_typescript_index(index),
            _ => HighlightCategory::from_default_index(index),
        }
    }
}

impl std::fmt::Display for Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
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
            Self::Java => "java",
            Self::CSharp => "c_sharp",
            Self::Php => "php",
            Self::Ruby => "ruby",
            Self::Bash => "bash",
            Self::Lua => "lua",
            Self::Pascal => "pascal",
            Self::Odin => "odin",
        };
        write!(f, "{}", s)
    }
}

const DEFAULT_HIGHLIGHT_CAPTURES: &[&str] = &[
    "attribute",
    "comment",
    "constant",
    "function",
    "keyword",
    "number",
    "operator",
    "property",
    "string",
    "type",
    "variable",
];

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
