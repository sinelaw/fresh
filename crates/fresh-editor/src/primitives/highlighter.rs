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
use fresh_languages::tree_sitter_highlight::{
    HighlightConfiguration, HighlightEvent, Highlighter as TSHighlighter,
};
pub use fresh_languages::{HighlightCategory, Language};
use ratatui::style::Color;
use std::ops::Range;

/// Maximum bytes to parse in a single operation (for viewport highlighting)
const MAX_PARSE_BYTES: usize = LARGE_FILE_THRESHOLD_BYTES as usize; // 1MB

/// Get the color for a highlight category from the theme
pub fn highlight_color(category: HighlightCategory, theme: &Theme) -> Color {
    match category {
        HighlightCategory::Attribute => theme.syntax_constant, // No specific attribute color, use constant
        HighlightCategory::Comment => theme.syntax_comment,
        HighlightCategory::Constant => theme.syntax_constant,
        HighlightCategory::Function => theme.syntax_function,
        HighlightCategory::Keyword => theme.syntax_keyword,
        HighlightCategory::Number => theme.syntax_constant,
        HighlightCategory::Operator => theme.syntax_operator,
        HighlightCategory::Property => theme.syntax_variable, // Properties are like variables
        HighlightCategory::String => theme.syntax_string,
        HighlightCategory::Type => theme.syntax_type,
        HighlightCategory::Variable => theme.syntax_variable,
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
                        color: highlight_color(span.category, theme),
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
                color: highlight_color(span.category, theme),
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
    use crate::view::theme;

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
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

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
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

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
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

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
        let dark_theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let dark_spans =
            highlighter.highlight_viewport(&buffer, 0, buffer.len(), &dark_theme, 100_000);

        // Highlight with light theme (cache should still work, colors should change)
        let light_theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
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
