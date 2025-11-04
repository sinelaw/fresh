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

use crate::buffer::Buffer;
use ratatui::style::Color;
use std::ops::Range;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter as TSHighlighter};

/// Maximum bytes to parse in a single operation (for viewport highlighting)
const MAX_PARSE_BYTES: usize = 100_000; // ~50 lines * 2000 chars/line

/// A highlighted span of text
#[derive(Debug, Clone)]
pub struct HighlightSpan {
    /// Byte range in the buffer
    pub range: Range<usize>,
    /// Color for this span
    pub color: Color,
}

/// Language configuration for syntax highlighting
#[derive(Debug)]
pub enum Language {
    Rust,
    // Future: JavaScript, TypeScript, Python, JSON, Markdown
}

impl Language {
    /// Detect language from file extension
    pub fn from_path(path: &std::path::Path) -> Option<Self> {
        match path.extension()?.to_str()? {
            "rs" => Some(Language::Rust),
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
                .map_err(|e| format!("Failed to create Rust highlight config: {}", e))?;

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
        }
    }

    /// Map tree-sitter highlight index to color
    fn highlight_color(&self, index: usize) -> Color {
        match self {
            Language::Rust => match index {
                0 => Color::Cyan,        // attribute
                1 => Color::DarkGray,    // comment
                2 => Color::Magenta,     // constant
                3 => Color::Yellow,      // function
                4 => Color::Red,         // keyword
                5 => Color::Magenta,     // number
                6 => Color::White,       // operator
                7 => Color::Cyan,        // property
                8 => Color::Green,       // string
                9 => Color::Blue,        // type
                10 => Color::White,      // variable
                _ => Color::White,       // default
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
                    .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
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
            None, // cancellation flag
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
    use crate::buffer::Buffer;

    #[test]
    fn test_language_detection() {
        let path = std::path::Path::new("test.rs");
        assert!(matches!(Language::from_path(path), Some(Language::Rust)));

        let path = std::path::Path::new("test.txt");
        assert!(Language::from_path(path).is_none());
    }

    #[test]
    fn test_highlighter_basic() {
        let buffer = Buffer::from_str("fn main() {\n    println!(\"Hello\");\n}");
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
            content.push_str(&format!("fn function_{}() {{}}\n", i));
        }
        let buffer = Buffer::from_str(&content);

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
        let buffer = Buffer::from_str("fn main() {\n    println!(\"Hello\");\n}");
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
