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
    pub fn from_syntax_name(name: &str, syntax_set: Arc<SyntaxSet>) -> Option<Self> {
        // We need a static reference, which is tricky with Arc
        // This is a limitation - in practice, we'll use find_syntax methods
        // that return references valid for the SyntaxSet's lifetime
        None // Placeholder - actual implementation needs careful lifetime handling
    }

    /// Highlight the visible viewport range
    ///
    /// This only parses the visible lines for instant performance with large files.
    /// Returns highlighted spans for the requested byte range, colored according to the theme.
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
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
        // Extend range slightly for context (helps with multi-line constructs)
        let parse_start = viewport_start.saturating_sub(1000);
        let parse_end = (viewport_end + 1000).min(buffer.len());
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

        // Parse line by line (syntect works on lines)
        let mut current_offset = start_byte;
        let mut current_scopes = ScopeStack::new();

        for line in content_str.lines() {
            let line_with_newline = if current_offset + line.len() < end_byte {
                format!("{}\n", line)
            } else {
                line.to_string()
            };

            // Parse this line
            let ops = match state.parse_line(&line_with_newline, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => continue, // Skip lines that fail to parse
            };

            // Convert parse operations to spans
            let mut char_offset = 0;

            // ops is Vec<(usize, ScopeStackOp)>
            for (op_offset, op) in ops {
                // Handle any text before this operation
                if op_offset > char_offset {
                    if let Some(category) = scope_stack_to_category(&current_scopes) {
                        let byte_start = current_offset + char_offset;
                        let byte_end = current_offset + op_offset;
                        if byte_start < byte_end {
                            spans.push(CachedSpan {
                                range: byte_start..byte_end,
                                category,
                            });
                        }
                    }
                }
                char_offset = op_offset;

                // Apply the scope operation
                let _ = current_scopes.apply(&op);
            }

            // Handle remaining text on line
            let line_byte_len = line_with_newline.len();
            if char_offset < line_byte_len {
                if let Some(category) = scope_stack_to_category(&current_scopes) {
                    let byte_start = current_offset + char_offset;
                    let byte_end = current_offset + line_byte_len;
                    if byte_start < byte_end {
                        spans.push(CachedSpan {
                            range: byte_start..byte_end,
                            category,
                        });
                    }
                }
            }

            current_offset += line_byte_len;
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
fn scope_to_category(scope: &str) -> Option<HighlightCategory> {
    let scope_lower = scope.to_lowercase();

    // Comments - highest priority
    if scope_lower.starts_with("comment") {
        return Some(HighlightCategory::Comment);
    }

    // Strings
    if scope_lower.starts_with("string") {
        return Some(HighlightCategory::String);
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
}
