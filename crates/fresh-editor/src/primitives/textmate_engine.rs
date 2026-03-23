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

/// Interval between parse state checkpoints (in bytes).
const CHECKPOINT_INTERVAL: usize = 4096;

/// TextMate highlighting engine (WASM-compatible)
///
/// Uses syntect for TextMate grammar-based syntax highlighting with parse state
/// checkpointing for correct embedded language support. See the runtime
/// `TextMateEngine` in `highlight_engine.rs` for detailed documentation.
pub struct TextMateEngine {
    syntax_set: Arc<SyntaxSet>,
    syntax_index: usize,
    cache: Option<TextMateCache>,
    checkpoints: Vec<ParseCheckpoint>,
    parsed_up_to: usize,
    last_buffer_len: usize,
}

/// A saved parse state at a known byte offset.
struct ParseCheckpoint {
    byte_offset: usize,
    parse_state: syntect::parsing::ParseState,
    scope_stack: syntect::parsing::ScopeStack,
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
            checkpoints: Vec::new(),
            parsed_up_to: 0,
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

    /// Highlight the visible viewport range using parse state checkpoints.
    /// See the runtime `TextMateEngine` in `highlight_engine.rs` for detailed docs.
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {

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
                        category: Some(span.category),
                    })
                    .collect();
            }
        }

        // Cache miss
        let desired_parse_start = viewport_start.saturating_sub(context_bytes);
        let parse_end = (viewport_end + context_bytes).min(buffer.len());

        if parse_end <= desired_parse_start {
            return Vec::new();
        }

        let syntax = &self.syntax_set.syntaxes()[self.syntax_index];

        let (actual_start, mut state, mut current_scopes, use_checkpoints) =
            self.find_parse_resume_point(desired_parse_start, parse_end, syntax);

        let content = buffer.slice_bytes(actual_start..parse_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let mut spans = Vec::new();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = actual_start;
        let mut bytes_since_checkpoint: usize = 0;

        while pos < content_bytes.len() {
            if use_checkpoints
                && current_offset > self.parsed_up_to
                && bytes_since_checkpoint >= CHECKPOINT_INTERVAL
            {
                self.checkpoints.push(ParseCheckpoint {
                    byte_offset: current_offset,
                    parse_state: state.clone(),
                    scope_stack: current_scopes.clone(),
                });
                bytes_since_checkpoint = 0;
            }

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

            let line_bytes = &content_bytes[line_start..line_end];
            let actual_line_byte_len = line_bytes.len();

            let line_str = match std::str::from_utf8(line_bytes) {
                Ok(s) => s,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

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
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            let collect_spans = current_offset + actual_line_byte_len > desired_parse_start;
            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                let clamped_op_offset = op_offset.min(line_content_len);
                if collect_spans && clamped_op_offset > syntect_offset {
                    if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                        let byte_start = current_offset + syntect_offset;
                        let byte_end = current_offset + clamped_op_offset;
                        let clamped_start = byte_start.max(desired_parse_start);
                        if clamped_start < byte_end {
                            spans.push(CachedSpan {
                                range: clamped_start..byte_end,
                                category,
                            });
                        }
                    }
                }
                syntect_offset = clamped_op_offset;
                #[allow(clippy::let_underscore_must_use)]
                let _ = current_scopes.apply(&op);
            }

            if collect_spans && syntect_offset < line_content_len {
                if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                    let byte_start = current_offset + syntect_offset;
                    let byte_end = current_offset + line_content_len;
                    let clamped_start = byte_start.max(desired_parse_start);
                    if clamped_start < byte_end {
                        spans.push(CachedSpan {
                            range: clamped_start..byte_end,
                            category,
                        });
                    }
                }
            }

            pos = line_end;
            current_offset += actual_line_byte_len;
            bytes_since_checkpoint += actual_line_byte_len;
        }

        if use_checkpoints {
            self.parsed_up_to = self.parsed_up_to.max(parse_end);
        }

        Self::merge_adjacent_spans(&mut spans);

        self.cache = Some(TextMateCache {
            range: desired_parse_start..parse_end,
            spans: spans.clone(),
        });
        self.last_buffer_len = buffer.len();

        spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .map(|span| {
                let cat = span.category;
                HighlightSpan {
                    range: span.range,
                    color: highlight_color(cat, theme),
                    category: Some(cat),
                }
            })
            .collect()
    }

    fn find_parse_resume_point(
        &self,
        desired_start: usize,
        parse_end: usize,
        syntax: &syntect::parsing::SyntaxReference,
    ) -> (usize, syntect::parsing::ParseState, syntect::parsing::ScopeStack, bool) {
        use syntect::parsing::{ParseState, ScopeStack};

        let idx = self
            .checkpoints
            .partition_point(|cp| cp.byte_offset <= desired_start);

        if idx > 0 {
            let cp = &self.checkpoints[idx - 1];
            (cp.byte_offset, cp.parse_state.clone(), cp.scope_stack.clone(), true)
        } else if parse_end <= MAX_PARSE_BYTES {
            (0, ParseState::new(syntax), ScopeStack::new(), true)
        } else {
            (desired_start, ParseState::new(syntax), ScopeStack::new(), false)
        }
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

    /// Invalidate cache and checkpoints for an edited range.
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        if let Some(cache) = &self.cache {
            if edit_range.start < cache.range.end && edit_range.end > cache.range.start {
                self.cache = None;
            }
        }
        let keep = self
            .checkpoints
            .partition_point(|cp| cp.byte_offset < edit_range.start);
        self.checkpoints.truncate(keep);
        self.parsed_up_to = self.parsed_up_to.min(edit_range.start);
    }

    /// Invalidate all cache and checkpoints
    pub fn invalidate_all(&mut self) {
        self.cache = None;
        self.checkpoints.clear();
        self.parsed_up_to = 0;
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
    // Diff markup: inserted/deleted lines
    if scope_lower.starts_with("markup.inserted") {
        return Some(HighlightCategory::String); // green
    }
    if scope_lower.starts_with("markup.deleted") {
        return Some(HighlightCategory::Keyword); // red/magenta
    }
    // Diff metadata (range info like @@ -1,5 +1,6 @@)
    if scope_lower.starts_with("meta.diff.range")
        || scope_lower.starts_with("meta.diff.header")
        || scope_lower.starts_with("meta.diff.index")
    {
        return Some(HighlightCategory::Function); // cyan/yellow
    }
    // Diff from-file/to-file headers (--- a/file, +++ b/file)
    if scope_lower.starts_with("punctuation.definition.from-file")
        || scope_lower.starts_with("punctuation.definition.to-file")
    {
        return Some(HighlightCategory::Type); // type color
    }

    // Keywords (but not keyword.operator)
    if scope_lower.starts_with("keyword") && !scope_lower.starts_with("keyword.operator") {
        return Some(HighlightCategory::Keyword);
    }

    // Punctuation that belongs to a parent construct (comment/string delimiters)
    // These must be checked before the generic punctuation rule below.
    // TextMate grammars assign e.g. `punctuation.definition.comment` to # // /* etc.
    if scope_lower.starts_with("punctuation.definition.comment") {
        return Some(HighlightCategory::Comment);
    }
    if scope_lower.starts_with("punctuation.definition.string") {
        return Some(HighlightCategory::String);
    }

    // Operators (keyword.operator only)
    if scope_lower.starts_with("keyword.operator") {
        return Some(HighlightCategory::Operator);
    }

    // Punctuation brackets ({, }, (, ), [, ], <, >)
    // Covers punctuation.section.*, punctuation.bracket.*,
    // and punctuation.definition.{array,block,brackets,group,inline-table,section,table,tag}
    if scope_lower.starts_with("punctuation.section")
        || scope_lower.starts_with("punctuation.bracket")
        || scope_lower.starts_with("punctuation.definition.array")
        || scope_lower.starts_with("punctuation.definition.block")
        || scope_lower.starts_with("punctuation.definition.brackets")
        || scope_lower.starts_with("punctuation.definition.group")
        || scope_lower.starts_with("punctuation.definition.inline-table")
        || scope_lower.starts_with("punctuation.definition.section")
        || scope_lower.starts_with("punctuation.definition.table")
        || scope_lower.starts_with("punctuation.definition.tag")
    {
        return Some(HighlightCategory::PunctuationBracket);
    }

    // Punctuation delimiters (;, ,, .)
    if scope_lower.starts_with("punctuation.separator")
        || scope_lower.starts_with("punctuation.terminator")
        || scope_lower.starts_with("punctuation.accessor")
    {
        return Some(HighlightCategory::PunctuationDelimiter);
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

    #[test]
    fn test_comment_delimiter_uses_comment_color() {
        // Comment delimiters (#, //, /*) should use comment color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.comment"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.python"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.begin"),
            Some(HighlightCategory::Comment)
        );
    }

    #[test]
    fn test_string_delimiter_uses_string_color() {
        // String delimiters (", ', `) should use string color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.string.begin"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.string.end"),
            Some(HighlightCategory::String)
        );
    }

    #[test]
    fn test_diff_scopes_produce_categories() {
        // Diff-specific scopes should map to categories
        assert_eq!(
            scope_to_category("markup.inserted"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("markup.inserted.diff"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("markup.deleted"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("markup.deleted.diff"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("meta.diff.range"),
            Some(HighlightCategory::Function)
        );
        assert_eq!(
            scope_to_category("meta.diff.header"),
            Some(HighlightCategory::Function)
        );
    }

    #[test]
    fn test_diff_parsing_produces_scopes() {
        use syntect::parsing::{ParseState, ScopeStack, SyntaxSet};

        let ss = SyntaxSet::load_defaults_newlines();
        let syntax = ss
            .find_syntax_by_extension("diff")
            .expect("Diff syntax should exist");
        let mut state = ParseState::new(syntax);

        let lines = [
            "--- a/file.txt\n",
            "+++ b/file.txt\n",
            "@@ -1,3 +1,4 @@\n",
            " unchanged\n",
            "-removed line\n",
            "+added line\n",
        ];

        let mut found_inserted = false;
        let mut found_deleted = false;
        let mut found_range = false;
        let mut scopes = ScopeStack::new();

        for line in &lines {
            let ops = state.parse_line(line, &ss).unwrap();
            for (_offset, op) in &ops {
                scopes.apply(op).unwrap();
                let scope_str = scopes
                    .as_slice()
                    .iter()
                    .map(|s| s.build_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                if scope_str.contains("markup.inserted") {
                    found_inserted = true;
                }
                if scope_str.contains("markup.deleted") {
                    found_deleted = true;
                }
                if scope_str.contains("meta.diff") {
                    found_range = true;
                }
            }
        }

        eprintln!(
            "found_inserted={}, found_deleted={}, found_range={}",
            found_inserted, found_deleted, found_range
        );
        assert!(
            found_inserted || found_deleted || found_range,
            "Diff grammar should produce markup.inserted, markup.deleted, or meta.diff scopes"
        );
    }
}
