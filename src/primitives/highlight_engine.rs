//! Unified highlighting engine
//!
//! This module provides a unified abstraction over different highlighting backends:
//! - Tree-sitter (built-in languages)
//! - TextMate grammars (user-installed or syntect built-ins)
//!
//! # Backend Selection Priority
//! 1. Tree-sitter (if built-in support exists) - faster, more accurate
//! 2. User TextMate grammar (from ~/.config/fresh/grammars/)
//! 3. Built-in syntect grammars
//! 4. No highlighting

use crate::model::buffer::Buffer;
use crate::primitives::grammar_registry::GrammarRegistry;
use crate::primitives::highlighter::{HighlightSpan, Highlighter, Language};
use crate::view::theme::Theme;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;
use syntect::parsing::SyntaxSet;

/// Preference for which highlighting backend to use
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HighlighterPreference {
    /// Use tree-sitter if available, fall back to TextMate
    #[default]
    Auto,
    /// Force tree-sitter only (no highlighting if unavailable)
    TreeSitter,
    /// Force TextMate grammar (skip tree-sitter even if available)
    TextMate,
}

/// Unified highlighting engine supporting multiple backends
pub enum HighlightEngine {
    /// Tree-sitter based highlighting (built-in languages)
    TreeSitter(Highlighter),
    /// TextMate grammar based highlighting
    TextMate(TextMateEngine),
    /// No highlighting available
    None,
}

/// TextMate highlighting engine wrapper
///
/// This struct handles the lifetime complexities of syntect by storing
/// the syntax set and using indices rather than references.
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
    category: crate::primitives::highlighter::HighlightCategory,
}

/// Maximum bytes to parse in a single operation
const MAX_PARSE_BYTES: usize = 1024 * 1024;

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

    /// Highlight the visible viewport range
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
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
                        color: span.category.color(theme),
                    })
                    .collect();
            }
        }

        // Cache miss - parse viewport region
        let parse_start = viewport_start.saturating_sub(1000);
        let parse_end = (viewport_end + 1000).min(buffer.len());

        if parse_end - parse_start > MAX_PARSE_BYTES {
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
        let mut current_offset = parse_start;
        let mut current_scopes = ScopeStack::new();

        for line in content_str.lines() {
            let line_with_newline = if current_offset + line.len() < parse_end {
                format!("{}\n", line)
            } else {
                line.to_string()
            };

            let ops = match state.parse_line(&line_with_newline, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => continue, // Skip lines that fail to parse
            };

            // Convert operations to spans
            let mut char_offset = 0;

            // ops is Vec<(usize, ScopeStackOp)>
            for (op_offset, op) in ops {
                if op_offset > char_offset {
                    if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
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

                let _ = current_scopes.apply(&op);
            }

            // Handle remaining text
            let line_len = line_with_newline.len();
            if char_offset < line_len {
                if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                    spans.push(CachedSpan {
                        range: (current_offset + char_offset)..(current_offset + line_len),
                        category,
                    });
                }
            }

            current_offset += line_len;
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
                color: span.category.color(theme),
            })
            .collect()
    }

    /// Map scope stack to highlight category
    fn scope_stack_to_category(
        scopes: &syntect::parsing::ScopeStack,
    ) -> Option<crate::primitives::highlighter::HighlightCategory> {
        use crate::primitives::highlighter::HighlightCategory;

        for scope in scopes.as_slice().iter().rev() {
            let scope_str = scope.build_string();
            if let Some(cat) = Self::scope_to_category(&scope_str) {
                return Some(cat);
            }
        }
        None
    }

    /// Map a scope string to category
    fn scope_to_category(scope: &str) -> Option<crate::primitives::highlighter::HighlightCategory> {
        use crate::primitives::highlighter::HighlightCategory;
        let s = scope.to_lowercase();

        if s.starts_with("comment") {
            return Some(HighlightCategory::Comment);
        }
        if s.starts_with("string") {
            return Some(HighlightCategory::String);
        }
        if s.starts_with("keyword.operator") || s.starts_with("punctuation") {
            return Some(HighlightCategory::Operator);
        }
        if s.starts_with("keyword") {
            return Some(HighlightCategory::Keyword);
        }
        if s.starts_with("entity.name.function")
            || s.starts_with("support.function")
            || s.starts_with("variable.function")
        {
            return Some(HighlightCategory::Function);
        }
        if s.starts_with("entity.name.type")
            || s.starts_with("entity.name.class")
            || s.starts_with("support.type")
            || s.starts_with("support.class")
            || s.starts_with("storage.type")
        {
            return Some(HighlightCategory::Type);
        }
        if s.starts_with("storage.modifier") {
            return Some(HighlightCategory::Keyword);
        }
        if s.starts_with("constant.numeric") {
            return Some(HighlightCategory::Number);
        }
        if s.starts_with("constant") {
            return Some(HighlightCategory::Constant);
        }
        if s.starts_with("variable") {
            return Some(HighlightCategory::Variable);
        }
        if s.starts_with("entity.name.tag") || s.starts_with("meta.object-literal.key") {
            return Some(HighlightCategory::Property);
        }
        if s.starts_with("entity.other.attribute") {
            return Some(HighlightCategory::Attribute);
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

impl HighlightEngine {
    /// Create a highlighting engine for a file, choosing the best available backend
    ///
    /// Priority:
    /// 1. Tree-sitter (if built-in support exists)
    /// 2. TextMate grammar (from registry)
    /// 3. No highlighting
    pub fn for_file(path: &Path, registry: &GrammarRegistry) -> Self {
        Self::for_file_with_preference(path, registry, HighlighterPreference::Auto)
    }

    /// Create a highlighting engine with explicit preference
    pub fn for_file_with_preference(
        path: &Path,
        registry: &GrammarRegistry,
        preference: HighlighterPreference,
    ) -> Self {
        match preference {
            HighlighterPreference::Auto => {
                // Try tree-sitter first
                if let Some(lang) = Language::from_path(path) {
                    if let Ok(highlighter) = Highlighter::new(lang) {
                        return Self::TreeSitter(highlighter);
                    }
                }

                // Fall back to TextMate
                Self::textmate_for_file(path, registry)
            }
            HighlighterPreference::TreeSitter => {
                if let Some(lang) = Language::from_path(path) {
                    if let Ok(highlighter) = Highlighter::new(lang) {
                        return Self::TreeSitter(highlighter);
                    }
                }
                Self::None
            }
            HighlighterPreference::TextMate => Self::textmate_for_file(path, registry),
        }
    }

    /// Create a TextMate engine for a file
    fn textmate_for_file(path: &Path, registry: &GrammarRegistry) -> Self {
        let syntax_set = registry.syntax_set_arc();

        // Find syntax by file extension
        if let Some(syntax) = registry.find_syntax_for_file(path) {
            // Find the index of this syntax in the set
            if let Some(index) = syntax_set
                .syntaxes()
                .iter()
                .position(|s| s.name == syntax.name)
            {
                return Self::TextMate(TextMateEngine::new(syntax_set, index));
            }
        }

        Self::None
    }

    /// Highlight the visible viewport
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
    ) -> Vec<HighlightSpan> {
        match self {
            Self::TreeSitter(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme)
            }
            Self::TextMate(h) => h.highlight_viewport(buffer, viewport_start, viewport_end, theme),
            Self::None => Vec::new(),
        }
    }

    /// Invalidate cache for an edited range
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        match self {
            Self::TreeSitter(h) => h.invalidate_range(edit_range),
            Self::TextMate(h) => h.invalidate_range(edit_range),
            Self::None => {}
        }
    }

    /// Invalidate entire cache
    pub fn invalidate_all(&mut self) {
        match self {
            Self::TreeSitter(h) => h.invalidate_all(),
            Self::TextMate(h) => h.invalidate_all(),
            Self::None => {}
        }
    }

    /// Check if this engine has highlighting available
    pub fn has_highlighting(&self) -> bool {
        !matches!(self, Self::None)
    }

    /// Get a description of the active backend
    pub fn backend_name(&self) -> &str {
        match self {
            Self::TreeSitter(_) => "tree-sitter",
            Self::TextMate(_) => "textmate",
            Self::None => "none",
        }
    }

    /// Get the language/syntax name if available
    pub fn syntax_name(&self) -> Option<&str> {
        match self {
            Self::TreeSitter(_) => None, // Tree-sitter doesn't expose name easily
            Self::TextMate(h) => Some(h.syntax_name()),
            Self::None => None,
        }
    }

    /// Get the tree-sitter Language if this is a tree-sitter highlighter
    /// Returns None for TextMate or no highlighting
    pub fn language(&self) -> Option<&Language> {
        match self {
            Self::TreeSitter(h) => Some(h.language()),
            Self::TextMate(_) => None,
            Self::None => None,
        }
    }
}

impl Default for HighlightEngine {
    fn default() -> Self {
        Self::None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_highlighter_preference_default() {
        let pref = HighlighterPreference::default();
        assert_eq!(pref, HighlighterPreference::Auto);
    }

    #[test]
    fn test_highlight_engine_default() {
        let engine = HighlightEngine::default();
        assert!(!engine.has_highlighting());
        assert_eq!(engine.backend_name(), "none");
    }

    #[test]
    fn test_tree_sitter_backend_selection() {
        let registry = GrammarRegistry::load();

        // Rust should use tree-sitter
        let engine = HighlightEngine::for_file(Path::new("test.rs"), &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");

        // Python should use tree-sitter
        let engine = HighlightEngine::for_file(Path::new("test.py"), &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");

        // JavaScript should use tree-sitter
        let engine = HighlightEngine::for_file(Path::new("test.js"), &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");
    }

    #[test]
    fn test_textmate_fallback() {
        let registry = GrammarRegistry::load();

        // Force TextMate for a tree-sitter supported language
        let engine = HighlightEngine::for_file_with_preference(
            Path::new("test.rs"),
            &registry,
            HighlighterPreference::TextMate,
        );
        // Should fall back to TextMate (syntect has Rust grammar)
        assert_eq!(engine.backend_name(), "textmate");
    }

    #[test]
    fn test_unknown_extension() {
        let registry = GrammarRegistry::load();

        // Unknown extension
        let engine = HighlightEngine::for_file(Path::new("test.unknown_xyz_123"), &registry);
        // Might be none or might find something via syntect
        // Just verify it doesn't panic
        let _ = engine.backend_name();
    }
}
