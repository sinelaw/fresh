//! Syntax highlighting with tree-sitter
//!
//! # Design
//! - **Viewport-only parsing**: Only highlights visible lines for instant performance with large files
//! - **Incremental updates**: Re-parses only edited regions
//! - **Lazy initialization**: Parsing happens on first render
//!
//! # Performance
//! Must work instantly when loading a 1GB file and jumping to an arbitrary offset.
//! This is achieved by only parsing the visible viewport (~50 lines), not the entire file.

use crate::config::LARGE_FILE_THRESHOLD_BYTES;
use crate::model::buffer::Buffer;
use crate::view::theme::Theme;
use ratatui::style::Color;
use std::ops::Range;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter as TSHighlighter};

/// Maximum bytes to parse in a single operation (for viewport highlighting)
const MAX_PARSE_BYTES: usize = LARGE_FILE_THRESHOLD_BYTES as usize; // 1MB

/// Highlight category names used for default languages.
/// The order matches the `configure()` call in `highlight_config()`.
/// Index 0 = attribute, 1 = comment, 2 = constant, 3 = function, 4 = keyword,
/// 5 = number, 6 = operator, 7 = property, 8 = string, 9 = type, 10 = variable
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
    fn from_default_index(index: usize) -> Option<Self> {
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
    /// TypeScript has more categories; we map them to the closest theme color.
    fn from_typescript_index(index: usize) -> Option<Self> {
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

    /// Get the color for this category from the theme
    pub fn color(&self, theme: &Theme) -> Color {
        match self {
            Self::Attribute => theme.syntax_constant, // No specific attribute color, use constant
            Self::Comment => theme.syntax_comment,
            Self::Constant => theme.syntax_constant,
            Self::Function => theme.syntax_function,
            Self::Keyword => theme.syntax_keyword,
            Self::Number => theme.syntax_constant,
            Self::Operator => theme.syntax_operator,
            Self::Property => theme.syntax_variable, // Properties are like variables
            Self::String => theme.syntax_string,
            Self::Type => theme.syntax_type,
            Self::Variable => theme.syntax_variable,
        }
    }
}

/// A highlighted span of text
#[derive(Debug, Clone)]
pub struct HighlightSpan {
    /// Byte range in the buffer
    pub range: Range<usize>,
    /// Color for this span
    pub color: Color,
}

/// Internal span used for caching (stores category instead of color)
#[derive(Debug, Clone)]
struct CachedSpan {
    /// Byte range in the buffer
    range: Range<usize>,
    /// Highlight category for this span
    category: HighlightCategory,
}

/// Language configuration for syntax highlighting
#[derive(Debug, Clone, Copy)]
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
    // Markdown,  // Disabled due to tree-sitter version conflict
}

impl Language {
    /// Detect language from file extension
    pub fn from_path(path: &std::path::Path) -> Option<Self> {
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
            // "md" => Some(Language::Markdown),  // Disabled
            _ => None,
        }
    }

    /// Get tree-sitter highlight configuration for this language
    fn highlight_config(&self) -> Result<HighlightConfiguration, String> {
        match self {
            Language::Rust => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_rust::LANGUAGE.into(),
                    "rust",
                    tree_sitter_rust::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create Rust highlight config: {e}"))?;

                // Configure highlight names
                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Python => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_python::LANGUAGE.into(),
                    "python",
                    tree_sitter_python::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create Python highlight config: {e}"))?;

                // Configure highlight names
                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::JavaScript => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_javascript::LANGUAGE.into(),
                    "javascript",
                    tree_sitter_javascript::HIGHLIGHT_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create JavaScript highlight config: {e}"))?;

                // Configure highlight names
                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::TypeScript => {
                // TypeScript extends JavaScript, so we need to combine queries
                // TypeScript-specific highlights come first (higher priority),
                // followed by JavaScript base highlights
                let combined_highlights = format!(
                    "{}\n{}",
                    tree_sitter_typescript::HIGHLIGHTS_QUERY,
                    tree_sitter_javascript::HIGHLIGHT_QUERY
                );

                let mut config = HighlightConfiguration::new(
                    tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
                    "typescript",
                    &combined_highlights,
                    "",                                   // injections query
                    tree_sitter_typescript::LOCALS_QUERY, // locals query for proper scoping
                )
                .map_err(|e| format!("Failed to create TypeScript highlight config: {e}"))?;

                // Configure highlight names - must include all captures from both JS and TS queries
                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::HTML => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_html::LANGUAGE.into(),
                    "html",
                    tree_sitter_html::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create HTML highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::CSS => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_css::LANGUAGE.into(),
                    "css",
                    tree_sitter_css::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create CSS highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::C => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_c::LANGUAGE.into(),
                    "c",
                    tree_sitter_c::HIGHLIGHT_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create C highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Cpp => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_cpp::LANGUAGE.into(),
                    "cpp",
                    tree_sitter_cpp::HIGHLIGHT_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create C++ highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Go => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_go::LANGUAGE.into(),
                    "go",
                    tree_sitter_go::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create Go highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Json => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_json::LANGUAGE.into(),
                    "json",
                    tree_sitter_json::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create JSON highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Java => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_java::LANGUAGE.into(),
                    "java",
                    tree_sitter_java::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create Java highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::CSharp => {
                // Note: tree-sitter-c-sharp doesn't export HIGHLIGHTS_QUERY
                // Using empty query for now - basic parsing still works
                let mut config = HighlightConfiguration::new(
                    tree_sitter_c_sharp::LANGUAGE.into(),
                    "c_sharp",
                    "", // No HIGHLIGHTS_QUERY exported in 0.23.1
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create C# highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Php => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_php::LANGUAGE_PHP.into(),
                    "php",
                    tree_sitter_php::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create PHP highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Ruby => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_ruby::LANGUAGE.into(),
                    "ruby",
                    tree_sitter_ruby::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create Ruby highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Bash => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_bash::LANGUAGE.into(),
                    "bash",
                    tree_sitter_bash::HIGHLIGHT_QUERY, // Note: singular, not plural
                    "",                                // injections query
                    "",                                // locals query
                )
                .map_err(|e| format!("Failed to create Bash highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Lua => {
                let mut config = HighlightConfiguration::new(
                    tree_sitter_lua::LANGUAGE.into(),
                    "lua",
                    tree_sitter_lua::HIGHLIGHTS_QUERY,
                    "", // injections query
                    "", // locals query
                )
                .map_err(|e| format!("Failed to create Lua highlight config: {e}"))?;

                config.configure(&[
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
                ]);

                Ok(config)
            }
            Language::Pascal => {
                // Pascal highlighting is handled by syntect (TextMate) via Sublime's default packages
                // Tree-sitter is still used for auto-indentation and semantic highlighting
                tracing::warn!("Pascal highlighting uses TextMate/syntect, not tree-sitter. Tree-sitter is still used for auto-indentation and semantic highlighting.");

                let locals_query = include_str!("../../queries/pascal/locals.scm");

                let mut config = HighlightConfiguration::new(
                    tree_sitter_pascal::LANGUAGE.into(),
                    "pascal",
                    "", // No highlights query - syntect handles highlighting
                    "", // injections query
                    locals_query,
                )
                .map_err(|e| format!("Failed to create Pascal highlight config: {e}"))?;

                // Configure highlight names (even though we don't use highlights query)
                config.configure(&[
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
                ]);

                Ok(config)
            } // Language::Markdown => {
              //     // Disabled due to tree-sitter version conflict
              //     Err("Markdown highlighting not available".to_string())
              // }
        }
    }

    /// Map tree-sitter highlight index to a highlight category
    fn highlight_category(&self, index: usize) -> Option<HighlightCategory> {
        match self {
            Language::TypeScript => HighlightCategory::from_typescript_index(index),
            _ => HighlightCategory::from_default_index(index),
        }
    }
}

/// Cache of highlighted spans for a specific byte range
#[derive(Debug, Clone)]
struct HighlightCache {
    /// Byte range this cache covers
    range: Range<usize>,
    /// Highlighted spans within this range (stores categories for theme-independent caching)
    spans: Vec<CachedSpan>,
}

/// Syntax highlighter with incremental viewport-based parsing
pub struct Highlighter {
    /// Tree-sitter highlighter instance
    ts_highlighter: TSHighlighter,
    /// Language being highlighted
    language: Language,
    /// Highlight configuration for the language
    config: HighlightConfiguration,
    /// Cache of highlighted spans (only for visible viewport)
    cache: Option<HighlightCache>,
    /// Last known buffer length (for detecting complete buffer changes)
    last_buffer_len: usize,
}

impl Highlighter {
    /// Create a new highlighter for the given language
    pub fn new(language: Language) -> Result<Self, String> {
        let config = language.highlight_config()?;
        Ok(Self {
            ts_highlighter: TSHighlighter::new(),
            language,
            config,
            cache: None,
            last_buffer_len: 0,
        })
    }

    /// Highlight the visible viewport range
    ///
    /// This only parses the visible lines for instant performance with large files.
    /// Returns highlighted spans for the requested byte range, colored according to the theme.
    ///
    /// `context_bytes` controls how far before/after the viewport to parse for accurate
    /// highlighting of multi-line constructs (strings, comments, nested blocks).
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        // Check if cache is valid for this range
        if let Some(cache) = &self.cache {
            if cache.range.start <= viewport_start
                && cache.range.end >= viewport_end
                && self.last_buffer_len == buffer.len()
            {
                // Cache hit! Filter spans to the requested range and resolve colors from theme
                return cache
                    .spans
                    .iter()
                    .filter(|span| {
                        span.range.start < viewport_end && span.range.end > viewport_start
                    })
                    .map(|span| HighlightSpan {
                        range: span.range.clone(),
                        color: span.category.color(theme),
                    })
                    .collect();
            }
        }

        // Cache miss - need to parse
        // Extend range for context (helps with multi-line constructs like strings, comments, nested blocks)
        let parse_start = viewport_start.saturating_sub(context_bytes);
        let parse_end = (viewport_end + context_bytes).min(buffer.len());
        let parse_range = parse_start..parse_end;

        // Limit parse size for safety
        if parse_range.len() > MAX_PARSE_BYTES {
            tracing::warn!(
                "Parse range too large: {} bytes, truncating to {}",
                parse_range.len(),
                MAX_PARSE_BYTES
            );
            // Just return empty spans if the range is too large
            return Vec::new();
        }

        // Extract source bytes from buffer
        let source = buffer.slice_bytes(parse_range.clone());

        // Highlight the source - store categories for theme-independent caching
        let mut cached_spans = Vec::new();
        match self.ts_highlighter.highlight(
            &self.config,
            &source,
            None,     // cancellation flag
            |_| None, // injection callback
        ) {
            Ok(highlights) => {
                let mut current_highlight: Option<usize> = None;

                for event in highlights {
                    match event {
                        Ok(HighlightEvent::Source { start, end }) => {
                            let span_start = parse_start + start;
                            let span_end = parse_start + end;

                            if let Some(highlight_idx) = current_highlight {
                                if let Some(category) =
                                    self.language.highlight_category(highlight_idx)
                                {
                                    cached_spans.push(CachedSpan {
                                        range: span_start..span_end,
                                        category,
                                    });
                                }
                            }
                        }
                        Ok(HighlightEvent::HighlightStart(s)) => {
                            current_highlight = Some(s.0);
                        }
                        Ok(HighlightEvent::HighlightEnd) => {
                            current_highlight = None;
                        }
                        Err(e) => {
                            tracing::warn!("Highlight error: {}", e);
                            break;
                        }
                    }
                }
            }
            Err(e) => {
                tracing::error!("Failed to highlight: {}", e);
            }
        }

        // Update cache
        self.cache = Some(HighlightCache {
            range: parse_range,
            spans: cached_spans.clone(),
        });
        self.last_buffer_len = buffer.len();

        // Filter to requested viewport and resolve colors from theme
        cached_spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .map(|span| HighlightSpan {
                range: span.range,
                color: span.category.color(theme),
            })
            .collect()
    }

    /// Invalidate cache for an edited range
    ///
    /// Call this when the buffer is edited to mark the cache as stale.
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        if let Some(cache) = &self.cache {
            // If edit intersects cache, invalidate it
            if edit_range.start < cache.range.end && edit_range.end > cache.range.start {
                self.cache = None;
            }
        }
    }

    /// Invalidate entire cache
    pub fn invalidate_all(&mut self) {
        self.cache = None;
    }

    /// Get the current language
    pub fn language(&self) -> &Language {
        &self.language
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;

    #[test]
    fn test_language_detection() {
        let path = std::path::Path::new("test.rs");
        assert!(matches!(Language::from_path(path), Some(Language::Rust)));

        let path = std::path::Path::new("test.py");
        assert!(matches!(Language::from_path(path), Some(Language::Python)));

        let path = std::path::Path::new("test.js");
        assert!(matches!(
            Language::from_path(path),
            Some(Language::JavaScript)
        ));

        let path = std::path::Path::new("test.jsx");
        assert!(matches!(
            Language::from_path(path),
            Some(Language::JavaScript)
        ));

        let path = std::path::Path::new("test.ts");
        assert!(matches!(
            Language::from_path(path),
            Some(Language::TypeScript)
        ));

        let path = std::path::Path::new("test.tsx");
        assert!(matches!(
            Language::from_path(path),
            Some(Language::TypeScript)
        ));

        let path = std::path::Path::new("test.html");
        assert!(matches!(Language::from_path(path), Some(Language::HTML)));

        let path = std::path::Path::new("test.css");
        assert!(matches!(Language::from_path(path), Some(Language::CSS)));

        let path = std::path::Path::new("test.c");
        assert!(matches!(Language::from_path(path), Some(Language::C)));

        let path = std::path::Path::new("test.h");
        assert!(matches!(Language::from_path(path), Some(Language::C)));

        let path = std::path::Path::new("test.cpp");
        assert!(matches!(Language::from_path(path), Some(Language::Cpp)));

        let path = std::path::Path::new("test.hpp");
        assert!(matches!(Language::from_path(path), Some(Language::Cpp)));

        let path = std::path::Path::new("test.cc");
        assert!(matches!(Language::from_path(path), Some(Language::Cpp)));

        let path = std::path::Path::new("test.hh");
        assert!(matches!(Language::from_path(path), Some(Language::Cpp)));

        let path = std::path::Path::new("test.cxx");
        assert!(matches!(Language::from_path(path), Some(Language::Cpp)));

        let path = std::path::Path::new("test.hxx");
        assert!(matches!(Language::from_path(path), Some(Language::Cpp)));

        let path = std::path::Path::new("test.go");
        assert!(matches!(Language::from_path(path), Some(Language::Go)));

        let path = std::path::Path::new("test.json");
        assert!(matches!(Language::from_path(path), Some(Language::Json)));

        let path = std::path::Path::new("test.java");
        assert!(matches!(Language::from_path(path), Some(Language::Java)));

        let path = std::path::Path::new("test.cs");
        assert!(matches!(Language::from_path(path), Some(Language::CSharp)));

        let path = std::path::Path::new("test.php");
        assert!(matches!(Language::from_path(path), Some(Language::Php)));

        let path = std::path::Path::new("test.rb");
        assert!(matches!(Language::from_path(path), Some(Language::Ruby)));

        let path = std::path::Path::new("test.sh");
        assert!(matches!(Language::from_path(path), Some(Language::Bash)));

        let path = std::path::Path::new("test.bash");
        assert!(matches!(Language::from_path(path), Some(Language::Bash)));

        let path = std::path::Path::new("test.lua");
        assert!(matches!(Language::from_path(path), Some(Language::Lua)));

        let path = std::path::Path::new("test.pas");
        assert!(matches!(Language::from_path(path), Some(Language::Pascal)));

        let path = std::path::Path::new("test.p");
        assert!(matches!(Language::from_path(path), Some(Language::Pascal)));

        // Markdown disabled due to tree-sitter version conflict
        // let path = std::path::Path::new("test.md");
        // assert!(matches!(Language::from_path(path), Some(Language::Markdown)));

        let path = std::path::Path::new("test.txt");
        assert!(Language::from_path(path).is_none());
    }

    #[test]
    fn test_highlighter_basic() {
        let buffer = Buffer::from_str_test("fn main() {\n    println!(\"Hello\");\n}");
        let mut highlighter = Highlighter::new(Language::Rust).unwrap();
        let theme = Theme::dark();

        // Highlight entire buffer
        let spans = highlighter.highlight_viewport(&buffer, 0, buffer.len(), &theme, 100_000);

        // Should have some highlighted spans
        assert!(!spans.is_empty());

        // Keywords like "fn" should be highlighted with the theme's keyword color
        let has_keyword = spans.iter().any(|s| s.color == theme.syntax_keyword);
        assert!(has_keyword, "Should highlight keywords");
    }

    #[test]
    fn test_highlighter_viewport_only() {
        // Create a large buffer
        let mut content = String::new();
        for i in 0..1000 {
            content.push_str(&format!("fn function_{i}() {{}}\n"));
        }
        let buffer = Buffer::from_str_test(&content);

        let mut highlighter = Highlighter::new(Language::Rust).unwrap();
        let theme = Theme::dark();

        // Highlight only a small viewport in the middle
        let viewport_start = 10000;
        let viewport_end = 10500;
        let spans =
            highlighter.highlight_viewport(&buffer, viewport_start, viewport_end, &theme, 100_000);

        // Should have some spans in the viewport
        assert!(!spans.is_empty());

        // All spans should be within or near the viewport
        for span in &spans {
            assert!(
                span.range.start < viewport_end + 2000,
                "Span start {} should be near viewport end {}",
                span.range.start,
                viewport_end
            );
        }
    }

    #[test]
    fn test_cache_invalidation() {
        let buffer = Buffer::from_str_test("fn main() {\n    println!(\"Hello\");\n}");
        let mut highlighter = Highlighter::new(Language::Rust).unwrap();
        let theme = Theme::dark();

        // First highlight
        highlighter.highlight_viewport(&buffer, 0, buffer.len(), &theme, 100_000);
        assert!(highlighter.cache.is_some());

        // Invalidate a range
        highlighter.invalidate_range(5..10);
        assert!(highlighter.cache.is_none());

        // Highlight again to rebuild cache
        highlighter.highlight_viewport(&buffer, 0, buffer.len(), &theme, 100_000);
        assert!(highlighter.cache.is_some());

        // Invalidate all
        highlighter.invalidate_all();
        assert!(highlighter.cache.is_none());
    }

    #[test]
    fn test_theme_affects_colors() {
        let buffer = Buffer::from_str_test("fn main() {\n    println!(\"Hello\");\n}");
        let mut highlighter = Highlighter::new(Language::Rust).unwrap();

        // Highlight with dark theme
        let dark_theme = Theme::dark();
        let dark_spans =
            highlighter.highlight_viewport(&buffer, 0, buffer.len(), &dark_theme, 100_000);

        // Highlight with light theme (cache should still work, colors should change)
        let light_theme = Theme::light();
        let light_spans =
            highlighter.highlight_viewport(&buffer, 0, buffer.len(), &light_theme, 100_000);

        // Both should have spans
        assert!(!dark_spans.is_empty());
        assert!(!light_spans.is_empty());

        // Keywords should have different colors in different themes
        let dark_keyword = dark_spans
            .iter()
            .find(|s| s.color == dark_theme.syntax_keyword);
        let light_keyword = light_spans
            .iter()
            .find(|s| s.color == light_theme.syntax_keyword);

        assert!(dark_keyword.is_some(), "Dark theme should have keyword");
        assert!(light_keyword.is_some(), "Light theme should have keyword");

        // The keyword colors should be different between themes
        assert_ne!(
            dark_theme.syntax_keyword, light_theme.syntax_keyword,
            "Themes should have different keyword colors"
        );
    }
}
