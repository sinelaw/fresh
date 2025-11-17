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
use crate::text_buffer::Buffer;
use ratatui::style::Color;
use std::ops::Range;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter as TSHighlighter};

/// Maximum bytes to parse in a single operation (for viewport highlighting)
const MAX_PARSE_BYTES: usize = LARGE_FILE_THRESHOLD_BYTES as usize; // 1MB

/// A highlighted span of text
#[derive(Debug, Clone)]
pub struct HighlightSpan {
    /// Byte range in the buffer
    pub range: Range<usize>,
    /// Color for this span
    pub color: Color,
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
                    "", // injections query
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
            } // Language::Markdown => {
              //     // Disabled due to tree-sitter version conflict
              //     Err("Markdown highlighting not available".to_string())
              // }
        }
    }

    /// Map tree-sitter highlight index to color
    fn highlight_color(&self, index: usize) -> Color {
        match self {
            Language::Rust => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::Python => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::JavaScript => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::TypeScript => match index {
                // Maps to the configure() order in highlight_config
                0 => Color::Cyan,      // attribute
                1 => Color::DarkGray,  // comment
                2 => Color::Magenta,   // constant
                3 => Color::Magenta,   // constant.builtin (null, undefined, true, false)
                4 => Color::Blue,      // constructor
                5 => Color::Green,     // embedded (template substitutions)
                6 => Color::Yellow,    // function
                7 => Color::Yellow,    // function.builtin (require)
                8 => Color::Yellow,    // function.method
                9 => Color::Red,       // keyword
                10 => Color::Magenta,  // number
                11 => Color::White,    // operator
                12 => Color::Cyan,     // property
                13 => Color::White,    // punctuation.bracket
                14 => Color::White,    // punctuation.delimiter
                15 => Color::Cyan,     // punctuation.special (template ${})
                16 => Color::Green,    // string
                17 => Color::Green,    // string.special (regex)
                18 => Color::Blue,     // type
                19 => Color::Blue,     // type.builtin
                20 => Color::White,    // variable
                21 => Color::Magenta,  // variable.builtin (this, super, arguments)
                22 => Color::LightCyan, // variable.parameter
                _ => Color::White,     // default
            },
            Language::HTML | Language::CSS => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::C | Language::Cpp => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::Go => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::Json => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::Java | Language::CSharp => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
            Language::Php | Language::Ruby | Language::Bash | Language::Lua => match index {
                0 => Color::Cyan,     // attribute
                1 => Color::DarkGray, // comment
                2 => Color::Magenta,  // constant
                3 => Color::Yellow,   // function
                4 => Color::Red,      // keyword
                5 => Color::Magenta,  // number
                6 => Color::White,    // operator
                7 => Color::Cyan,     // property
                8 => Color::Green,    // string
                9 => Color::Blue,     // type
                10 => Color::White,   // variable
                _ => Color::White,    // default
            },
        }
    }
}

/// Cache of highlighted spans for a specific byte range
#[derive(Debug, Clone)]
struct HighlightCache {
    /// Byte range this cache covers
    range: Range<usize>,
    /// Highlighted spans within this range
    spans: Vec<HighlightSpan>,
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
    /// Returns highlighted spans for the requested byte range.
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
    ) -> Vec<HighlightSpan> {
        // Check if cache is valid for this range
        if let Some(cache) = &self.cache {
            if cache.range.start <= viewport_start
                && cache.range.end >= viewport_end
                && self.last_buffer_len == buffer.len()
            {
                // Cache hit! Filter spans to the requested range
                return cache
                    .spans
                    .iter()
                    .filter(|span| {
                        span.range.start < viewport_end && span.range.end > viewport_start
                    })
                    .cloned()
                    .collect();
            }
        }

        // Cache miss - need to parse
        // Extend range slightly for context (helps with multi-line constructs)
        let parse_start = viewport_start.saturating_sub(1000);
        let parse_end = (viewport_end + 1000).min(buffer.len());
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

        // Highlight the source
        let mut spans = Vec::new();
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
                                let color = self.language.highlight_color(highlight_idx);
                                spans.push(HighlightSpan {
                                    range: span_start..span_end,
                                    color,
                                });
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
            spans: spans.clone(),
        });
        self.last_buffer_len = buffer.len();

        // Filter to requested viewport
        spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
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
    use crate::text_buffer::Buffer;

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

        // Highlight entire buffer
        let spans = highlighter.highlight_viewport(&buffer, 0, buffer.len());

        // Should have some highlighted spans
        assert!(!spans.is_empty());

        // Keywords like "fn" should be highlighted
        let has_keyword = spans.iter().any(|s| s.color == Color::Red);
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

        // Highlight only a small viewport in the middle
        let viewport_start = 10000;
        let viewport_end = 10500;
        let spans = highlighter.highlight_viewport(&buffer, viewport_start, viewport_end);

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

        // First highlight
        highlighter.highlight_viewport(&buffer, 0, buffer.len());
        assert!(highlighter.cache.is_some());

        // Invalidate a range
        highlighter.invalidate_range(5..10);
        assert!(highlighter.cache.is_none());

        // Highlight again to rebuild cache
        highlighter.highlight_viewport(&buffer, 0, buffer.len());
        assert!(highlighter.cache.is_some());

        // Invalidate all
        highlighter.invalidate_all();
        assert!(highlighter.cache.is_none());
    }
}
