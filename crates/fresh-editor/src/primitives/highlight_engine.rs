//! Unified highlighting engine
//!
//! This module provides a unified abstraction over different highlighting backends:
//! - TextMate grammars via syntect (default for highlighting)
//! - Tree-sitter (available via explicit preference, also used for non-highlighting features)
//!
//! # Backend Selection
//! By default, syntect/TextMate is used for syntax highlighting because it provides
//! broader language coverage. Tree-sitter language detection is still performed
//! to support non-highlighting features like auto-indentation and semantic highlighting.
//!
//! # Non-Highlighting Features
//! Even when using TextMate for highlighting, tree-sitter `Language` is detected
//! and available via `.language()` for:
//! - Auto-indentation (via IndentCalculator)
//! - Semantic highlighting (variable scope tracking)
//! - Other syntax-aware features

use crate::model::buffer::Buffer;
use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlighter::{
    highlight_color, HighlightCategory, HighlightSpan, Highlighter, Language,
};
use crate::view::theme::Theme;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;
use syntect::parsing::SyntaxSet;

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

/// Preference for which highlighting backend to use
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HighlighterPreference {
    /// Use TextMate/syntect for highlighting (default)
    /// Tree-sitter language is still detected for other features (indentation, semantic highlighting)
    #[default]
    Auto,
    /// Force tree-sitter for highlighting (useful for testing/comparison)
    TreeSitter,
    /// Explicitly use TextMate grammar (same as Auto)
    TextMate,
}

/// Unified highlighting engine supporting multiple backends
#[derive(Default)]
pub enum HighlightEngine {
    /// Tree-sitter based highlighting (built-in languages)
    TreeSitter(Box<Highlighter>),
    /// TextMate grammar based highlighting
    TextMate(Box<TextMateEngine>),
    /// No highlighting available
    #[default]
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
    /// Tree-sitter language for non-highlighting features (indentation, semantic highlighting)
    /// Even when using syntect for highlighting, we track the language for other features
    ts_language: Option<Language>,
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
            ts_language: None,
        }
    }

    /// Create a new TextMate engine with a tree-sitter language for non-highlighting features
    pub fn with_language(
        syntax_set: Arc<SyntaxSet>,
        syntax_index: usize,
        ts_language: Option<Language>,
    ) -> Self {
        Self {
            syntax_set,
            syntax_index,
            cache: None,
            last_buffer_len: 0,
            ts_language,
        }
    }

    /// Get the tree-sitter language (for indentation, semantic highlighting, etc.)
    pub fn language(&self) -> Option<&Language> {
        self.ts_language.as_ref()
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

        // Parse line by line - manually track line boundaries to handle CRLF correctly
        // str::lines() strips both \n and \r\n, losing the distinction
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = parse_start;
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

            let ops = match state.parse_line(&line_for_syntect, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    continue;
                }
            };

            // Convert operations to spans
            // Note: syntect offsets are relative to line_for_syntect, but we need
            // to map them to the actual buffer positions
            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                // Handle any text before this operation (but only within content, not newline)
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

            // Handle remaining text on line (content only, not line ending)
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

            // Advance by actual byte length (including real line terminator)
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

impl HighlightEngine {
    /// Create a highlighting engine for a file
    ///
    /// Always uses syntect/TextMate for highlighting, but detects tree-sitter
    /// language for other features (indentation, semantic highlighting).
    pub fn for_file(path: &Path, registry: &GrammarRegistry) -> Self {
        Self::for_file_with_preference(path, registry, HighlighterPreference::Auto)
    }

    /// Create a highlighting engine for a file, using language configuration for detection.
    ///
    /// This method checks the provided languages configuration for filename and extension
    /// matches before falling back to built-in detection. This allows users to configure
    /// custom filename patterns (like PKGBUILD for bash) that will be respected for
    /// syntax highlighting.
    pub fn for_file_with_languages(
        path: &Path,
        registry: &GrammarRegistry,
        languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
    ) -> Self {
        Self::for_file_with_languages_and_preference(
            path,
            registry,
            languages,
            HighlighterPreference::Auto,
        )
    }

    /// Create a highlighting engine with explicit preference and language configuration.
    pub fn for_file_with_languages_and_preference(
        path: &Path,
        registry: &GrammarRegistry,
        languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
        preference: HighlighterPreference,
    ) -> Self {
        match preference {
            // Auto now defaults to TextMate for highlighting (syntect has broader coverage)
            // but still detects tree-sitter language for indentation/semantic features
            HighlighterPreference::Auto | HighlighterPreference::TextMate => {
                Self::textmate_for_file_with_languages(path, registry, languages)
            }
            HighlighterPreference::TreeSitter => {
                if let Some(lang) = Language::from_path(path) {
                    if let Ok(highlighter) = Highlighter::new(lang) {
                        return Self::TreeSitter(Box::new(highlighter));
                    }
                }
                Self::None
            }
        }
    }

    /// Create a highlighting engine with explicit preference
    pub fn for_file_with_preference(
        path: &Path,
        registry: &GrammarRegistry,
        preference: HighlighterPreference,
    ) -> Self {
        match preference {
            // Auto now defaults to TextMate for highlighting (syntect has broader coverage)
            // but still detects tree-sitter language for indentation/semantic features
            HighlighterPreference::Auto | HighlighterPreference::TextMate => {
                Self::textmate_for_file(path, registry)
            }
            HighlighterPreference::TreeSitter => {
                if let Some(lang) = Language::from_path(path) {
                    if let Ok(highlighter) = Highlighter::new(lang) {
                        return Self::TreeSitter(Box::new(highlighter));
                    }
                }
                Self::None
            }
        }
    }

    /// Create a TextMate engine for a file, falling back to tree-sitter if no TextMate grammar
    fn textmate_for_file(path: &Path, registry: &GrammarRegistry) -> Self {
        let syntax_set = registry.syntax_set_arc();

        // Detect tree-sitter language for non-highlighting features
        let ts_language = Language::from_path(path);

        // Find syntax by file extension
        if let Some(syntax) = registry.find_syntax_for_file(path) {
            // Find the index of this syntax in the set
            if let Some(index) = syntax_set
                .syntaxes()
                .iter()
                .position(|s| s.name == syntax.name)
            {
                return Self::TextMate(Box::new(TextMateEngine::with_language(
                    syntax_set,
                    index,
                    ts_language,
                )));
            }
        }

        // No TextMate grammar found - fall back to tree-sitter if available
        // This handles languages like TypeScript that syntect doesn't include by default
        if let Some(lang) = ts_language {
            if let Ok(highlighter) = Highlighter::new(lang) {
                tracing::debug!(
                    "No TextMate grammar for {:?}, falling back to tree-sitter",
                    path.extension()
                );
                return Self::TreeSitter(Box::new(highlighter));
            }
        }

        Self::None
    }

    /// Create a TextMate engine for a file with language configuration support
    fn textmate_for_file_with_languages(
        path: &Path,
        registry: &GrammarRegistry,
        languages: &std::collections::HashMap<String, crate::config::LanguageConfig>,
    ) -> Self {
        let syntax_set = registry.syntax_set_arc();

        // Detect tree-sitter language for non-highlighting features
        let ts_language = Language::from_path(path);

        // Find syntax by file extension, checking languages config first
        if let Some(syntax) = registry.find_syntax_for_file_with_languages(path, languages) {
            // Find the index of this syntax in the set
            if let Some(index) = syntax_set
                .syntaxes()
                .iter()
                .position(|s| s.name == syntax.name)
            {
                return Self::TextMate(Box::new(TextMateEngine::with_language(
                    syntax_set,
                    index,
                    ts_language,
                )));
            }
        }

        // No TextMate grammar found - fall back to tree-sitter if available
        // This handles languages like TypeScript that syntect doesn't include by default
        if let Some(lang) = ts_language {
            if let Ok(highlighter) = Highlighter::new(lang) {
                tracing::debug!(
                    "No TextMate grammar for {:?}, falling back to tree-sitter",
                    path.extension()
                );
                return Self::TreeSitter(Box::new(highlighter));
            }
        }

        Self::None
    }

    /// Create a highlighting engine for a specific tree-sitter language.
    ///
    /// This is useful when manually setting the language (e.g., from UI).
    /// Uses tree-sitter for the specified language.
    pub fn for_language(language: Language) -> Self {
        if let Ok(highlighter) = Highlighter::new(language) {
            Self::TreeSitter(Box::new(highlighter))
        } else {
            Self::None
        }
    }

    /// Create a highlighting engine for a syntax by name.
    ///
    /// This looks up the syntax in the grammar registry and creates a TextMate
    /// highlighter for it. This supports all syntect syntaxes (100+) including
    /// user-configured grammars.
    ///
    /// The `ts_language` parameter optionally provides a tree-sitter language
    /// for non-highlighting features (indentation, semantic highlighting).
    pub fn for_syntax_name(
        name: &str,
        registry: &GrammarRegistry,
        ts_language: Option<Language>,
    ) -> Self {
        let syntax_set = registry.syntax_set_arc();

        if let Some(syntax) = registry.find_syntax_by_name(name) {
            // Find the index of this syntax in the set
            if let Some(index) = syntax_set
                .syntaxes()
                .iter()
                .position(|s| s.name == syntax.name)
            {
                return Self::TextMate(Box::new(TextMateEngine::with_language(
                    syntax_set,
                    index,
                    ts_language,
                )));
            }
        }

        Self::None
    }

    /// Highlight the visible viewport
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
        match self {
            Self::TreeSitter(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme, context_bytes)
            }
            Self::TextMate(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme, context_bytes)
            }
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

    /// Get the tree-sitter Language for non-highlighting features
    /// Returns the language even when using TextMate for highlighting
    pub fn language(&self) -> Option<&Language> {
        match self {
            Self::TreeSitter(h) => Some(h.language()),
            Self::TextMate(h) => h.language(),
            Self::None => None,
        }
    }
}

/// Highlight a code string using syntect (for markdown code blocks, hover popups, etc.)
/// Returns spans with byte ranges relative to the input string.
///
/// This uses TextMate grammars via syntect which provides broader language coverage
/// than tree-sitter (~150+ languages vs ~17).
pub fn highlight_string(
    code: &str,
    lang_hint: &str,
    registry: &GrammarRegistry,
    theme: &Theme,
) -> Vec<HighlightSpan> {
    use syntect::parsing::{ParseState, ScopeStack};

    // Find syntax by language token (handles aliases like "py" -> Python)
    let syntax = match registry.syntax_set().find_syntax_by_token(lang_hint) {
        Some(s) => s,
        None => return Vec::new(),
    };

    let syntax_set = registry.syntax_set();
    let mut state = ParseState::new(syntax);
    let mut spans = Vec::new();
    let mut current_scopes = ScopeStack::new();
    let mut current_offset = 0;

    // Parse line by line
    for line in code.split_inclusive('\n') {
        let line_start = current_offset;
        let line_len = line.len();

        // Remove trailing newline for syntect, then add it back
        let line_content = line.trim_end_matches(&['\r', '\n'][..]);
        let line_for_syntect = if line.ends_with('\n') {
            format!("{}\n", line_content)
        } else {
            line_content.to_string()
        };

        let ops = match state.parse_line(&line_for_syntect, syntax_set) {
            Ok(ops) => ops,
            Err(_) => {
                current_offset += line_len;
                continue;
            }
        };

        let mut syntect_offset = 0;
        let line_content_len = line_content.len();

        for (op_offset, op) in ops {
            let clamped_op_offset = op_offset.min(line_content_len);
            if clamped_op_offset > syntect_offset {
                if let Some(category) = scope_stack_to_category(&current_scopes) {
                    let byte_start = line_start + syntect_offset;
                    let byte_end = line_start + clamped_op_offset;
                    if byte_start < byte_end {
                        spans.push(HighlightSpan {
                            range: byte_start..byte_end,
                            color: highlight_color(category, theme),
                        });
                    }
                }
            }
            syntect_offset = clamped_op_offset;
            let _ = current_scopes.apply(&op);
        }

        // Handle remaining text on line
        if syntect_offset < line_content_len {
            if let Some(category) = scope_stack_to_category(&current_scopes) {
                let byte_start = line_start + syntect_offset;
                let byte_end = line_start + line_content_len;
                if byte_start < byte_end {
                    spans.push(HighlightSpan {
                        range: byte_start..byte_end,
                        color: highlight_color(category, theme),
                    });
                }
            }
        }

        current_offset += line_len;
    }

    // Merge adjacent spans with same color
    merge_adjacent_highlight_spans(&mut spans);

    spans
}

/// Map scope stack to highlight category (for highlight_string)
fn scope_stack_to_category(scopes: &syntect::parsing::ScopeStack) -> Option<HighlightCategory> {
    for scope in scopes.as_slice().iter().rev() {
        let scope_str = scope.build_string();
        if let Some(cat) = scope_to_category(&scope_str) {
            return Some(cat);
        }
    }
    None
}

/// Merge adjacent spans with same color
fn merge_adjacent_highlight_spans(spans: &mut Vec<HighlightSpan>) {
    if spans.len() < 2 {
        return;
    }

    let mut write_idx = 0;
    for read_idx in 1..spans.len() {
        if spans[write_idx].color == spans[read_idx].color
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

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::view::theme;

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
    fn test_textmate_backend_selection() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Languages with TextMate grammars use TextMate for highlighting
        let engine = HighlightEngine::for_file(Path::new("test.rs"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        // Tree-sitter language should still be detected for other features
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.py"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.js"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.language().is_some());

        // TypeScript falls back to tree-sitter (syntect doesn't include TS by default)
        let engine = HighlightEngine::for_file(Path::new("test.ts"), &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.tsx"), &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");
        assert!(engine.language().is_some());
    }

    #[test]
    fn test_tree_sitter_explicit_preference() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Force tree-sitter for highlighting
        let engine = HighlightEngine::for_file_with_preference(
            Path::new("test.rs"),
            &registry,
            HighlighterPreference::TreeSitter,
        );
        assert_eq!(engine.backend_name(), "tree-sitter");
    }

    #[test]
    fn test_unknown_extension() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Unknown extension
        let engine = HighlightEngine::for_file(Path::new("test.unknown_xyz_123"), &registry);
        // Might be none or might find something via syntect
        // Just verify it doesn't panic
        let _ = engine.backend_name();
    }

    #[test]
    fn test_highlight_viewport_empty_buffer_no_panic() {
        // Regression test: calling highlight_viewport with an empty buffer
        // and non-zero viewport range previously caused subtraction overflow panic.
        //
        // The bug occurred when:
        // - buffer is empty (len = 0)
        // - viewport_start > context_bytes (so parse_start > 0 after saturating_sub)
        // - parse_end = min(viewport_end + context_bytes, buffer.len()) = 0
        // - parse_end - parse_start would underflow (0 - positive = overflow)
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), &registry);

        // Create empty buffer
        let buffer = Buffer::from_str("", 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Test the specific case that triggered the overflow:
        // viewport_start=100, context_bytes=10 => parse_start=90, parse_end=0
        // 0 - 90 = overflow!
        if let HighlightEngine::TextMate(ref mut tm) = engine {
            // Small context_bytes so parse_start remains > 0
            let spans = tm.highlight_viewport(&buffer, 100, 200, &theme, 10);
            assert!(spans.is_empty());
        }
    }

    /// Test that TextMateEngine produces correct byte offsets for CRLF content.
    /// This is a regression test for a bug where using str::lines() caused 1-byte
    /// offset drift per line because it strips line terminators.
    #[test]
    fn test_textmate_engine_crlf_byte_offsets() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        let mut engine = HighlightEngine::for_file(Path::new("test.java"), &registry);

        // Create CRLF content with keywords on each line
        // Each "public" keyword should be highlighted at byte positions:
        // Line 1: "public" at bytes 0-5
        // Line 2: "public" at bytes 8-13 (after "public\r\n" = 8 bytes)
        // Line 3: "public" at bytes 16-21 (after two "public\r\n" = 16 bytes)
        let content = b"public\r\npublic\r\npublic\r\n";
        let buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        if let HighlightEngine::TextMate(ref mut tm) = engine {
            // Highlight the entire content
            let spans = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

            // Find spans that cover keyword positions
            // The keyword "public" should have spans at these byte ranges:
            // Line 1: 0..6
            // Line 2: 8..14 (NOT 7..13 which would be the buggy offset)
            // Line 3: 16..22 (NOT 14..20 which would be the buggy offset)

            eprintln!(
                "Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Check that we have spans covering the correct positions
            let has_span_at = |start: usize, end: usize| -> bool {
                spans
                    .iter()
                    .any(|s| s.range.start <= start && s.range.end >= end)
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
        } else {
            panic!("Expected TextMate engine for .java file");
        }
    }

    #[test]
    fn test_git_rebase_todo_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // git-rebase-todo files should use the Git Rebase Todo grammar
        let engine = HighlightEngine::for_file(Path::new("git-rebase-todo"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_git_commit_message_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // COMMIT_EDITMSG should use the Git Commit Message grammar
        let engine = HighlightEngine::for_file(Path::new("COMMIT_EDITMSG"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // MERGE_MSG should also work
        let engine = HighlightEngine::for_file(Path::new("MERGE_MSG"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitignore_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitignore should use the Gitignore grammar
        let engine = HighlightEngine::for_file(Path::new(".gitignore"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // .dockerignore should also work
        let engine = HighlightEngine::for_file(Path::new(".dockerignore"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitconfig_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitconfig should use the Git Config grammar
        let engine = HighlightEngine::for_file(Path::new(".gitconfig"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // .gitmodules should also work
        let engine = HighlightEngine::for_file(Path::new(".gitmodules"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitattributes_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitattributes should use the Git Attributes grammar
        let engine = HighlightEngine::for_file(Path::new(".gitattributes"), &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }
}
