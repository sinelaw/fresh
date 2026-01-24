//! TextMate-based syntax highlighting engine (WASM-compatible)
//!
//! This module provides syntax highlighting using syntect's TextMate grammar engine.
//! It's completely WASM-compatible as syntect can use pure-Rust regex (fancy-regex).
//!
//! # Features
//!
//! - Syntax highlighting for 100+ languages via TextMate grammars
//! - Viewport-based highlighting with caching for performance
//! - No tree-sitter or native code dependencies

use crate::model::buffer::Buffer;
use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlight_types::{highlight_color, HighlightCategory, HighlightSpan};
use crate::view::theme::Theme;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;
use syntect::parsing::SyntaxSet;

/// Maximum bytes to parse in a single operation
const MAX_PARSE_BYTES: usize = 1024 * 1024;

/// TextMate highlighting engine
///
/// Uses syntect for TextMate grammar-based syntax highlighting.
/// This is WASM-compatible when syntect uses the `fancy-regex` feature.
pub struct TextMateEngine {
    syntax_set: Arc<SyntaxSet>,
    syntax_index: usize,
    cache: Option<TextMateCache>,
    last_buffer_len: usize,
}

#[derive(Debug, Clone)]
struct TextMateCache {
    range: Range<usize>,
    spans: Vec<CachedSpan>,
}

#[derive(Debug, Clone)]
struct CachedSpan {
    range: Range<usize>,
    category: HighlightCategory,
}

impl TextMateEngine {
    /// Create a new TextMate engine for the given syntax
    pub fn new(syntax_set: Arc<SyntaxSet>, syntax_index: usize) -> Self {
        Self {
            syntax_set,
            syntax_index,
            cache: None,
            last_buffer_len: 0,
        }
    }

    /// Create a TextMate engine for a file path
    pub fn for_file(path: &Path, registry: &GrammarRegistry) -> Option<Self> {
        let syntax_set = registry.syntax_set_arc();

        // Find syntax by file extension
        let syntax = registry.find_syntax_for_file(path)?;

        // Find the index of this syntax in the set
        let index = syntax_set
            .syntaxes()
            .iter()
            .position(|s| s.name == syntax.name)?;

        Some(Self::new(syntax_set, index))
    }

    /// Highlight the visible viewport range
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
        use syntect::parsing::{ParseState, ScopeStack};

        // Check cache validity
        if let Some(cache) = &self.cache {
            if cache.range.start <= viewport_start
                && cache.range.end >= viewport_end
                && self.last_buffer_len == buffer.len()
            {
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

        // Cache miss - parse viewport region
        let parse_start = viewport_start.saturating_sub(context_bytes);
        let parse_end = (viewport_end + context_bytes).min(buffer.len());

        if parse_end <= parse_start || parse_end - parse_start > MAX_PARSE_BYTES {
            return Vec::new();
        }

        let syntax = &self.syntax_set.syntaxes()[self.syntax_index];
        let mut state = ParseState::new(syntax);
        let mut spans = Vec::new();

        // Get content
        let content = buffer.slice_bytes(parse_start..parse_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        // Parse line by line
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = parse_start;
        let mut current_scopes = ScopeStack::new();

        while pos < content_bytes.len() {
            let line_start = pos;
            let mut line_end = pos;

            // Scan for line ending
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

            let line_bytes = &content_bytes[line_start..line_end];
            let actual_line_byte_len = line_bytes.len();

            let line_str = match std::str::from_utf8(line_bytes) {
                Ok(s) => s,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    continue;
                }
            };

            // Prepare line for syntect
            let line_content = line_str.trim_end_matches(&['\r', '\n'][..]);
            let line_for_syntect = if line_end < content_bytes.len() || line_str.ends_with('\n') {
                format!("{}\n", line_content)
            } else {
                line_content.to_string()
            };

            let ops = match state.parse_line(&line_for_syntect, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    continue;
                }
            };

            // Convert operations to spans
            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                let clamped_op_offset = op_offset.min(line_content_len);
                if clamped_op_offset > syntect_offset {
                    if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
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
                let _ = current_scopes.apply(&op);
            }

            // Handle remaining text on line
            if syntect_offset < line_content_len {
                if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
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

            pos = line_end;
            current_offset += actual_line_byte_len;
        }

        // Merge adjacent spans
        Self::merge_adjacent_spans(&mut spans);

        // Update cache
        self.cache = Some(TextMateCache {
            range: parse_start..parse_end,
            spans: spans.clone(),
        });
        self.last_buffer_len = buffer.len();

        // Filter and resolve colors
        spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .map(|span| HighlightSpan {
                range: span.range,
                color: highlight_color(span.category, theme),
            })
            .collect()
    }

    /// Map scope stack to highlight category
    fn scope_stack_to_category(scopes: &syntect::parsing::ScopeStack) -> Option<HighlightCategory> {
        for scope in scopes.as_slice().iter().rev() {
            let scope_str = scope.build_string();
            if let Some(cat) = scope_to_category(&scope_str) {
                return Some(cat);
            }
        }
        None
    }

    /// Merge adjacent spans with same category
    fn merge_adjacent_spans(spans: &mut Vec<CachedSpan>) {
        if spans.len() < 2 {
            return;
        }

        let mut write_idx = 0;
        for read_idx in 1..spans.len() {
            if spans[write_idx].category == spans[read_idx].category
                && spans[write_idx].range.end == spans[read_idx].range.start
            {
                spans[write_idx].range.end = spans[read_idx].range.end;
            } else {
                write_idx += 1;
                if write_idx != read_idx {
                    spans[write_idx] = spans[read_idx].clone();
                }
            }
        }
        spans.truncate(write_idx + 1);
    }

    /// Invalidate cache for edited range
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        if let Some(cache) = &self.cache {
            if edit_range.start < cache.range.end && edit_range.end > cache.range.start {
                self.cache = None;
            }
        }
    }

    /// Invalidate all cache
    pub fn invalidate_all(&mut self) {
        self.cache = None;
    }

    /// Get syntax name
    pub fn syntax_name(&self) -> &str {
        &self.syntax_set.syntaxes()[self.syntax_index].name
    }
}

/// Map TextMate scope to highlight category
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

    // Markdown/markup scopes
    if scope_lower.starts_with("markup.heading") || scope_lower.starts_with("entity.name.section") {
        return Some(HighlightCategory::Keyword);
    }
    if scope_lower.starts_with("markup.bold") {
        return Some(HighlightCategory::Constant);
    }
    if scope_lower.starts_with("markup.italic") {
        return Some(HighlightCategory::Variable);
    }
    if scope_lower.starts_with("markup.raw") || scope_lower.starts_with("markup.inline.raw") {
        return Some(HighlightCategory::String);
    }
    if scope_lower.starts_with("markup.underline.link")
        || scope_lower.starts_with("markup.underline")
    {
        return Some(HighlightCategory::Function);
    }
    if scope_lower.starts_with("markup.quote") || scope_lower.starts_with("markup.strikethrough") {
        return Some(HighlightCategory::Comment);
    }
    if scope_lower.starts_with("markup.list") {
        return Some(HighlightCategory::Operator);
    }

    // Keywords (but not keyword.operator)
    if scope_lower.starts_with("keyword") && !scope_lower.starts_with("keyword.operator") {
        return Some(HighlightCategory::Keyword);
    }

    // Operators
    if scope_lower.starts_with("keyword.operator") || scope_lower.starts_with("punctuation") {
        return Some(HighlightCategory::Operator);
    }

    // Functions
    if scope_lower.starts_with("entity.name.function")
        || scope_lower.starts_with("meta.function-call")
        || scope_lower.starts_with("support.function")
    {
        return Some(HighlightCategory::Function);
    }

    // Types
    if scope_lower.starts_with("entity.name.type")
        || scope_lower.starts_with("storage.type")
        || scope_lower.starts_with("support.type")
        || scope_lower.starts_with("entity.name.class")
    {
        return Some(HighlightCategory::Type);
    }

    // Constants and numbers
    if scope_lower.starts_with("constant.numeric")
        || scope_lower.starts_with("constant.language")
        || scope_lower.starts_with("constant.character")
    {
        return Some(HighlightCategory::Constant);
    }
    if scope_lower.starts_with("constant") {
        return Some(HighlightCategory::Constant);
    }

    // Variables and parameters
    if scope_lower.starts_with("variable.parameter") {
        return Some(HighlightCategory::Variable);
    }
    if scope_lower.starts_with("variable") {
        return Some(HighlightCategory::Variable);
    }

    // Storage modifiers (pub, static, const, etc.)
    if scope_lower.starts_with("storage.modifier") {
        return Some(HighlightCategory::Keyword);
    }

    // Entity names (catch-all for other named things)
    if scope_lower.starts_with("entity.name") {
        return Some(HighlightCategory::Function);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_to_category() {
        assert_eq!(
            scope_to_category("comment.line"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("string.quoted"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("keyword.control"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("keyword.operator"),
            Some(HighlightCategory::Operator)
        );
        assert_eq!(
            scope_to_category("entity.name.function"),
            Some(HighlightCategory::Function)
        );
        assert_eq!(
            scope_to_category("constant.numeric"),
            Some(HighlightCategory::Constant)
        );
        assert_eq!(
            scope_to_category("variable.parameter"),
            Some(HighlightCategory::Variable)
        );
    }
}
