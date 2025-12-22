//! TextMate grammar-based syntax highlighting
//!
//! This module provides syntax highlighting using TextMate grammars via the syntect library.
//! It mirrors the API of the tree-sitter highlighter for seamless integration.
//!
//! # Design
//! - **Viewport-only parsing**: Only highlights visible lines for instant performance
//! - **Incremental updates**: Re-parses only edited regions using line-based state
//! - **Theme-independent caching**: Stores categories, resolves colors on demand
//!
//! # Performance
//! Like the tree-sitter highlighter, this is designed for large files by only
//! parsing the visible viewport plus a small context buffer.

use crate::model::buffer::Buffer;
use crate::primitives::highlighter::{HighlightCategory, HighlightSpan};
use crate::view::theme::Theme;
use std::ops::Range;
use std::sync::Arc;
use syntect::parsing::{ParseState, ScopeStack, SyntaxReference, SyntaxSet};

/// Maximum bytes to parse in a single operation (for viewport highlighting)
const MAX_PARSE_BYTES: usize = 1024 * 1024; // 1MB

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
struct TextMateCache {
    /// Byte range this cache covers
    range: Range<usize>,
    /// Highlighted spans within this range
    spans: Vec<CachedSpan>,
}

/// TextMate grammar-based syntax highlighter
pub struct TextMateHighlighter {
    /// Reference to the syntax definition
    syntax: &'static SyntaxReference,
    /// Shared syntax set containing all grammars
    syntax_set: Arc<SyntaxSet>,
    /// Cache of highlighted spans (only for visible viewport)
    cache: Option<TextMateCache>,
    /// Last known buffer length (for detecting complete buffer changes)
    last_buffer_len: usize,
}

impl TextMateHighlighter {
    /// Create a new TextMate highlighter for the given syntax
    ///
    /// # Safety
    /// The syntax reference must outlive the highlighter. In practice, this is
    /// ensured by the GrammarRegistry holding the SyntaxSet for the app lifetime.
    pub fn new(syntax: &'static SyntaxReference, syntax_set: Arc<SyntaxSet>) -> Self {
        Self {
            syntax,
            syntax_set,
            cache: None,
            last_buffer_len: 0,
        }
    }

    /// Create a highlighter from a syntax set and syntax name
    pub fn from_syntax_name(_name: &str, _syntax_set: Arc<SyntaxSet>) -> Option<Self> {
        // We need a static reference, which is tricky with Arc
        // This is a limitation - in practice, we'll use find_syntax methods
        // that return references valid for the SyntaxSet's lifetime
        None // Placeholder - actual implementation needs careful lifetime handling
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
                // Cache hit! Filter spans to the requested range and resolve colors
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
                "Parse range too large: {} bytes, skipping TextMate highlighting",
                parse_range.len()
            );
            return Vec::new();
        }

        // Parse the viewport region
        let cached_spans = self.parse_region(buffer, parse_start, parse_end);

        // Update cache
        self.cache = Some(TextMateCache {
            range: parse_range,
            spans: cached_spans.clone(),
        });
        self.last_buffer_len = buffer.len();

        // Filter to requested viewport and resolve colors
        cached_spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .map(|span| HighlightSpan {
                range: span.range,
                color: span.category.color(theme),
            })
            .collect()
    }

    /// Parse a region of the buffer and return cached spans
    fn parse_region(&self, buffer: &Buffer, start_byte: usize, end_byte: usize) -> Vec<CachedSpan> {
        let mut spans = Vec::new();
        let mut state = ParseState::new(self.syntax);

        // Get the text content
        let content = buffer.slice_bytes(start_byte..end_byte);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => {
                tracing::warn!(
                    "Buffer contains invalid UTF-8 in range {}..{}",
                    start_byte,
                    end_byte
                );
                return spans;
            }
        };

        // Parse line by line - manually track line boundaries to handle CRLF correctly
        // str::lines() strips both \n and \r\n, losing the distinction
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = start_byte;
        let mut current_scopes = ScopeStack::new();

        while pos < content_bytes.len() {
            let line_start = pos;
            let mut line_end = pos;

            // Scan for line ending (find \n or \r\n or end of content)
            while line_end < content_bytes.len() {
                if content_bytes[line_end] == b'\n' {
                    line_end += 1;
                    break;
                } else if content_bytes[line_end] == b'\r' {
                    if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                        line_end += 2; // CRLF
                    } else {
                        line_end += 1; // CR only
                    }
                    break;
                }
                line_end += 1;
            }

            // Get the line content and actual byte length
            let line_bytes = &content_bytes[line_start..line_end];
            let actual_line_byte_len = line_bytes.len();

            // Create line string for syntect - strip CR if present, ensure single \n
            let line_str = match std::str::from_utf8(line_bytes) {
                Ok(s) => s,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    continue;
                }
            };

            // Remove trailing \r\n or \n, then add single \n for syntect
            let line_content = line_str.trim_end_matches(&['\r', '\n'][..]);
            let line_for_syntect = if line_end < content_bytes.len() || line_str.ends_with('\n') {
                format!("{}\n", line_content)
            } else {
                line_content.to_string()
            };

            // Parse this line
            let ops = match state.parse_line(&line_for_syntect, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    continue;
                }
            };

            // Convert parse operations to spans
            // Note: syntect offsets are relative to line_for_syntect, but we need
            // to map them to the actual buffer positions
            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                // Handle any text before this operation (but only within content, not newline)
                let clamped_op_offset = op_offset.min(line_content_len);
                if clamped_op_offset > syntect_offset {
                    if let Some(category) = scope_stack_to_category(&current_scopes) {
                        let byte_start = current_offset + syntect_offset;
                        let byte_end = current_offset + clamped_op_offset;
                        if byte_start < byte_end {
                            spans.push(CachedSpan {
                                range: byte_start..byte_end,
                                category,
                            });
                        }
                    }
                }
                syntect_offset = clamped_op_offset;

                // Apply the scope operation
                let _ = current_scopes.apply(&op);
            }

            // Handle remaining text on line (content only, not line ending)
            if syntect_offset < line_content_len {
                if let Some(category) = scope_stack_to_category(&current_scopes) {
                    let byte_start = current_offset + syntect_offset;
                    let byte_end = current_offset + line_content_len;
                    if byte_start < byte_end {
                        spans.push(CachedSpan {
                            range: byte_start..byte_end,
                            category,
                        });
                    }
                }
            }

            // Advance by actual byte length (including real line terminator)
            pos = line_end;
            current_offset += actual_line_byte_len;
        }

        // Merge adjacent spans with same category for efficiency
        merge_adjacent_spans(&mut spans);

        spans
    }

    /// Invalidate cache for an edited range
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

    /// Get the syntax name
    pub fn syntax_name(&self) -> &str {
        &self.syntax.name
    }
}

/// Map a TextMate scope stack to our HighlightCategory
fn scope_stack_to_category(scopes: &ScopeStack) -> Option<HighlightCategory> {
    // Check scopes from most specific (top) to least specific (bottom)
    for scope in scopes.as_slice().iter().rev() {
        let scope_str = scope.build_string();
        if let Some(category) = scope_to_category(&scope_str) {
            return Some(category);
        }
    }
    None
}

/// Map a single TextMate scope string to HighlightCategory
pub fn scope_to_category(scope: &str) -> Option<HighlightCategory> {
    let scope_lower = scope.to_lowercase();

    // Comments - highest priority
    if scope_lower.starts_with("comment") {
        return Some(HighlightCategory::Comment);
    }

    // Strings
    if scope_lower.starts_with("string") {
        return Some(HighlightCategory::String);
    }

    // Markdown/markup scopes - handle before generic keyword/punctuation checks
    // See: https://macromates.com/manual/en/language_grammars (TextMate scope naming)
    // Headings: markup.heading and entity.name.section (used by syntect's markdown grammar)
    if scope_lower.starts_with("markup.heading") || scope_lower.starts_with("entity.name.section") {
        return Some(HighlightCategory::Keyword); // Headers styled like keywords (bold, prominent)
    }
    // Bold: markup.bold
    if scope_lower.starts_with("markup.bold") {
        return Some(HighlightCategory::Constant); // Bold styled like constants (bright)
    }
    // Italic: markup.italic
    if scope_lower.starts_with("markup.italic") {
        return Some(HighlightCategory::Variable); // Italic styled like variables
    }
    // Inline code and code blocks: markup.raw, markup.inline.raw
    if scope_lower.starts_with("markup.raw") || scope_lower.starts_with("markup.inline.raw") {
        return Some(HighlightCategory::String); // Code styled like strings
    }
    // Links: markup.underline.link
    if scope_lower.starts_with("markup.underline.link") {
        return Some(HighlightCategory::Function); // Links styled like functions (distinct color)
    }
    // Generic underline (often links)
    if scope_lower.starts_with("markup.underline") {
        return Some(HighlightCategory::Function);
    }
    // Block quotes: markup.quote
    if scope_lower.starts_with("markup.quote") {
        return Some(HighlightCategory::Comment); // Quotes styled like comments (subdued)
    }
    // Lists: markup.list
    if scope_lower.starts_with("markup.list") {
        return Some(HighlightCategory::Operator); // List markers styled like operators
    }
    // Strikethrough: markup.strikethrough
    if scope_lower.starts_with("markup.strikethrough") {
        return Some(HighlightCategory::Comment); // Strikethrough styled subdued
    }

    // Keywords
    if scope_lower.starts_with("keyword.control")
        || scope_lower.starts_with("keyword.other")
        || scope_lower.starts_with("keyword.declaration")
        || scope_lower.starts_with("keyword")
    {
        // keyword.operator should map to Operator, not Keyword
        if !scope_lower.starts_with("keyword.operator") {
            return Some(HighlightCategory::Keyword);
        }
    }

    // Operators (including keyword.operator)
    if scope_lower.starts_with("keyword.operator") || scope_lower.starts_with("punctuation") {
        return Some(HighlightCategory::Operator);
    }

    // Functions
    if scope_lower.starts_with("entity.name.function")
        || scope_lower.starts_with("support.function")
        || scope_lower.starts_with("meta.function-call")
        || scope_lower.starts_with("variable.function")
    {
        return Some(HighlightCategory::Function);
    }

    // Types
    if scope_lower.starts_with("entity.name.type")
        || scope_lower.starts_with("entity.name.class")
        || scope_lower.starts_with("entity.name.struct")
        || scope_lower.starts_with("entity.name.enum")
        || scope_lower.starts_with("entity.name.interface")
        || scope_lower.starts_with("entity.name.trait")
        || scope_lower.starts_with("support.type")
        || scope_lower.starts_with("support.class")
        || scope_lower.starts_with("storage.type")
    {
        return Some(HighlightCategory::Type);
    }

    // Storage modifiers (pub, static, const as keywords)
    if scope_lower.starts_with("storage.modifier") {
        return Some(HighlightCategory::Keyword);
    }

    // Constants and numbers
    if scope_lower.starts_with("constant.numeric")
        || scope_lower.starts_with("constant.language.boolean")
    {
        return Some(HighlightCategory::Number);
    }
    if scope_lower.starts_with("constant") {
        return Some(HighlightCategory::Constant);
    }

    // Variables
    if scope_lower.starts_with("variable.parameter")
        || scope_lower.starts_with("variable.other")
        || scope_lower.starts_with("variable.language")
    {
        return Some(HighlightCategory::Variable);
    }

    // Properties / object keys
    if scope_lower.starts_with("entity.name.tag")
        || scope_lower.starts_with("support.other.property")
        || scope_lower.starts_with("meta.object-literal.key")
        || scope_lower.starts_with("variable.other.property")
        || scope_lower.starts_with("variable.other.object.property")
    {
        return Some(HighlightCategory::Property);
    }

    // Attributes (decorators, annotations)
    if scope_lower.starts_with("entity.other.attribute")
        || scope_lower.starts_with("meta.attribute")
        || scope_lower.starts_with("entity.name.decorator")
    {
        return Some(HighlightCategory::Attribute);
    }

    // Generic variable fallback
    if scope_lower.starts_with("variable") {
        return Some(HighlightCategory::Variable);
    }

    None
}

/// Merge adjacent spans with the same category
fn merge_adjacent_spans(spans: &mut Vec<CachedSpan>) {
    if spans.len() < 2 {
        return;
    }

    let mut write_idx = 0;
    for read_idx in 1..spans.len() {
        if spans[write_idx].category == spans[read_idx].category
            && spans[write_idx].range.end == spans[read_idx].range.start
        {
            // Merge: extend the write span
            spans[write_idx].range.end = spans[read_idx].range.end;
        } else {
            // Move to next write position
            write_idx += 1;
            if write_idx != read_idx {
                spans[write_idx] = spans[read_idx].clone();
            }
        }
    }
    spans.truncate(write_idx + 1);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_to_category_comments() {
        assert_eq!(
            scope_to_category("comment.line"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("comment.block"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("comment.line.double-slash.rust"),
            Some(HighlightCategory::Comment)
        );
    }

    #[test]
    fn test_scope_to_category_strings() {
        assert_eq!(
            scope_to_category("string.quoted.double"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("string.quoted.single.python"),
            Some(HighlightCategory::String)
        );
    }

    #[test]
    fn test_scope_to_category_keywords() {
        assert_eq!(
            scope_to_category("keyword.control.if"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("keyword.control.loop.rust"),
            Some(HighlightCategory::Keyword)
        );
    }

    #[test]
    fn test_scope_to_category_operators() {
        assert_eq!(
            scope_to_category("keyword.operator.arithmetic"),
            Some(HighlightCategory::Operator)
        );
        assert_eq!(
            scope_to_category("punctuation.separator"),
            Some(HighlightCategory::Operator)
        );
    }

    #[test]
    fn test_scope_to_category_functions() {
        assert_eq!(
            scope_to_category("entity.name.function"),
            Some(HighlightCategory::Function)
        );
        assert_eq!(
            scope_to_category("support.function.builtin"),
            Some(HighlightCategory::Function)
        );
    }

    #[test]
    fn test_scope_to_category_types() {
        assert_eq!(
            scope_to_category("entity.name.type"),
            Some(HighlightCategory::Type)
        );
        assert_eq!(
            scope_to_category("storage.type.rust"),
            Some(HighlightCategory::Type)
        );
        assert_eq!(
            scope_to_category("support.class"),
            Some(HighlightCategory::Type)
        );
    }

    #[test]
    fn test_scope_to_category_numbers() {
        assert_eq!(
            scope_to_category("constant.numeric.integer"),
            Some(HighlightCategory::Number)
        );
        assert_eq!(
            scope_to_category("constant.numeric.float"),
            Some(HighlightCategory::Number)
        );
    }

    #[test]
    fn test_scope_to_category_markup() {
        // Markdown/markup scopes
        assert_eq!(
            scope_to_category("markup.heading.1.markdown"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("markup.heading.2"),
            Some(HighlightCategory::Keyword)
        );
        // entity.name.section is used by syntect's markdown grammar for heading text
        assert_eq!(
            scope_to_category("entity.name.section.markdown"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("markup.bold"),
            Some(HighlightCategory::Constant)
        );
        assert_eq!(
            scope_to_category("markup.italic"),
            Some(HighlightCategory::Variable)
        );
        assert_eq!(
            scope_to_category("markup.raw.inline"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("markup.raw.block"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("markup.underline.link"),
            Some(HighlightCategory::Function)
        );
        assert_eq!(
            scope_to_category("markup.quote"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("markup.list.unnumbered"),
            Some(HighlightCategory::Operator)
        );
        assert_eq!(
            scope_to_category("markup.strikethrough"),
            Some(HighlightCategory::Comment)
        );
    }

    #[test]
    fn test_merge_adjacent_spans() {
        let mut spans = vec![
            CachedSpan {
                range: 0..5,
                category: HighlightCategory::Keyword,
            },
            CachedSpan {
                range: 5..10,
                category: HighlightCategory::Keyword,
            },
            CachedSpan {
                range: 10..15,
                category: HighlightCategory::String,
            },
        ];

        merge_adjacent_spans(&mut spans);

        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].range, 0..10);
        assert_eq!(spans[0].category, HighlightCategory::Keyword);
        assert_eq!(spans[1].range, 10..15);
        assert_eq!(spans[1].category, HighlightCategory::String);
    }

    /// Test that CRLF line endings are handled correctly in byte offset calculations.
    /// This is a regression test for the bug where using str::lines() caused
    /// byte offset drift because it strips line terminators, losing the distinction
    /// between \n (1 byte) and \r\n (2 bytes).
    #[test]
    fn test_crlf_line_boundary_detection() {
        // Simulate what parse_region does for CRLF content
        // Content: "abc\r\ndef\r\nghi"
        // Byte positions:
        //   a=0, b=1, c=2, \r=3, \n=4
        //   d=5, e=6, f=7, \r=8, \n=9
        //   g=10, h=11, i=12
        let content = "abc\r\ndef\r\nghi";
        let content_bytes = content.as_bytes();

        // Track line boundaries the way parse_region should
        let mut line_starts: Vec<usize> = vec![];
        let mut line_ends: Vec<usize> = vec![];
        let mut pos = 0;

        while pos < content_bytes.len() {
            let line_start = pos;
            let mut line_end = pos;

            // Scan for line ending (same logic as parse_region)
            while line_end < content_bytes.len() {
                if content_bytes[line_end] == b'\n' {
                    line_end += 1;
                    break;
                } else if content_bytes[line_end] == b'\r' {
                    if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                        line_end += 2; // CRLF
                    } else {
                        line_end += 1; // Standalone CR
                    }
                    break;
                }
                line_end += 1;
            }

            line_starts.push(line_start);
            line_ends.push(line_end);
            pos = line_end;
        }

        // Verify line boundaries
        assert_eq!(line_starts.len(), 3, "Should detect 3 lines");

        // Line 1: "abc\r\n" at bytes 0..5
        assert_eq!(line_starts[0], 0, "Line 1 should start at byte 0");
        assert_eq!(line_ends[0], 5, "Line 1 should end at byte 5 (after CRLF)");

        // Line 2: "def\r\n" at bytes 5..10
        assert_eq!(line_starts[1], 5, "Line 2 should start at byte 5");
        assert_eq!(line_ends[1], 10, "Line 2 should end at byte 10 (after CRLF)");

        // Line 3: "ghi" at bytes 10..13 (no terminator)
        assert_eq!(line_starts[2], 10, "Line 3 should start at byte 10");
        assert_eq!(line_ends[2], 13, "Line 3 should end at byte 13");

        // Contrast with the buggy behavior using str::lines()
        // which would give line lengths of 3, 3, 3 instead of 5, 5, 3
        let buggy_offsets: Vec<usize> = content
            .lines()
            .scan(0usize, |offset, line| {
                let start = *offset;
                // Bug: lines() strips terminators, so we'd add wrong length
                *offset += line.len() + 1; // Always adds 1 for \n, wrong for CRLF!
                Some(start)
            })
            .collect();

        // The buggy approach would give wrong offsets for lines 2 and 3
        assert_eq!(buggy_offsets[0], 0, "Buggy: Line 1 start correct");
        assert_eq!(buggy_offsets[1], 4, "Buggy: Line 2 would start at 4 (wrong! should be 5)");
        assert_eq!(buggy_offsets[2], 8, "Buggy: Line 3 would start at 8 (wrong! should be 10)");
    }

    /// Test LF-only content still works correctly
    #[test]
    fn test_lf_line_boundary_detection() {
        // Content: "abc\ndef\nghi"
        // Byte positions:
        //   a=0, b=1, c=2, \n=3
        //   d=4, e=5, f=6, \n=7
        //   g=8, h=9, i=10
        let content = "abc\ndef\nghi";
        let content_bytes = content.as_bytes();

        let mut line_starts: Vec<usize> = vec![];
        let mut pos = 0;

        while pos < content_bytes.len() {
            let line_start = pos;
            let mut line_end = pos;

            while line_end < content_bytes.len() {
                if content_bytes[line_end] == b'\n' {
                    line_end += 1;
                    break;
                } else if content_bytes[line_end] == b'\r' {
                    if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                        line_end += 2;
                    } else {
                        line_end += 1;
                    }
                    break;
                }
                line_end += 1;
            }

            line_starts.push(line_start);
            pos = line_end;
        }

        // Verify: LF content should have correct offsets
        assert_eq!(line_starts[0], 0, "Line 1 starts at 0");
        assert_eq!(line_starts[1], 4, "Line 2 starts at 4 (after 'abc\\n')");
        assert_eq!(line_starts[2], 8, "Line 3 starts at 8 (after 'def\\n')");
    }

    /// Test that TextMateHighlighter produces correct byte offsets for CRLF content.
    /// This is a regression test for a bug where using str::lines() caused 1-byte
    /// offset drift per line because it strips line terminators.
    #[test]
    fn test_textmate_highlighter_crlf_byte_offsets() {
        use crate::primitives::grammar_registry::GrammarRegistry;
        use crate::view::theme::Theme;

        let registry = GrammarRegistry::load();
        let syntax_set = registry.syntax_set_arc();

        // Find Java syntax (should be available in syntect)
        let java_syntax = syntax_set
            .find_syntax_by_extension("java")
            .expect("Java syntax should be available");

        let mut highlighter = TextMateHighlighter::new(
            // SAFETY: syntax_set is Arc and lives for test duration
            unsafe { &*(java_syntax as *const _) },
            syntax_set,
        );

        // Create CRLF content with keywords on each line
        // Each "public" keyword should be highlighted at byte positions:
        // Line 1: "public" at bytes 0-5
        // Line 2: "public" at bytes 9-14 (after "public\r\n" = 8 bytes)
        // Line 3: "public" at bytes 18-23 (after two "public\r\n" = 16 bytes)
        let content = b"public\r\npublic\r\npublic\r\n";
        let buffer = crate::model::buffer::TextBuffer::from_bytes(content.to_vec());
        let theme = Theme::dark();

        // Highlight the entire content
        let spans = highlighter.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

        // Find spans that cover keyword positions
        // The keyword "public" should have spans at these byte ranges:
        // Line 1: 0..6
        // Line 2: 8..14 (NOT 7..13 which would be the buggy offset)
        // Line 3: 16..22 (NOT 14..20 which would be the buggy offset)

        eprintln!("Spans: {:?}", spans.iter().map(|s| &s.range).collect::<Vec<_>>());

        // Check that we have spans covering the correct positions
        let has_span_at = |start: usize, end: usize| -> bool {
            spans.iter().any(|s| s.range.start <= start && s.range.end >= end)
        };

        // Line 1: "public" at bytes 0-6
        assert!(
            has_span_at(0, 6),
            "Should have span covering bytes 0-6 (line 1 'public'). Spans: {:?}",
            spans.iter().map(|s| &s.range).collect::<Vec<_>>()
        );

        // Line 2: "public" at bytes 8-14 (after "public\r\n")
        // If buggy, would be at 7-13
        assert!(
            has_span_at(8, 14),
            "Should have span covering bytes 8-14 (line 2 'public'). \
             If this fails, CRLF offset drift is occurring. Spans: {:?}",
            spans.iter().map(|s| &s.range).collect::<Vec<_>>()
        );

        // Line 3: "public" at bytes 16-22 (after two "public\r\n")
        // If buggy, would be at 14-20
        assert!(
            has_span_at(16, 22),
            "Should have span covering bytes 16-22 (line 3 'public'). \
             If this fails, CRLF offset drift is occurring. Spans: {:?}",
            spans.iter().map(|s| &s.range).collect::<Vec<_>>()
        );
    }
}
