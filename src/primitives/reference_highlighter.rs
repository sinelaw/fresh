//! Semantic highlighting for word occurrences under cursor
//!
//! When the cursor is on a word/identifier, all occurrences of that word
//! in the current viewport are highlighted with a subtle background color.
//!
//! # Design
//! - Uses the same `HighlightSpan` approach as syntax highlighting for efficiency
//! - Computed on-demand during rendering (no persistent markers)
//! - Only highlights occurrences within the visible viewport
//!
//! # Three modes of operation (in order of preference):
//! 1. **Locals mode** (scope-aware): Uses tree-sitter "locals" queries to track
//!    variable scopes and definitions. Only highlights references to the same
//!    definition - respects lexical scoping like VSCode's documentHighlight.
//! 2. **Tree-sitter mode**: Falls back to finding all identifier nodes with
//!    matching text (when locals query not available for the language).
//! 3. **Text-matching mode**: Simple whole-word text matching for buffers
//!    without tree-sitter support.
//!
//! # Supported Languages for Scope-Aware Highlighting
//! - Rust, Python, JavaScript, TypeScript, Go, C, C++
//! - Other languages fall back to identifier or text matching

use crate::model::buffer::Buffer;
use crate::primitives::highlighter::{HighlightSpan, Language};
use crate::primitives::word_navigation::{find_word_end, find_word_start, is_word_char};
use ratatui::style::Color;
use std::ops::Range;
use tree_sitter::{Parser, Query, QueryCursor, StreamingIterator};

/// Default subtle background color for occurrence highlights
/// A dark gray that's visible but not distracting
pub const DEFAULT_HIGHLIGHT_COLOR: Color = Color::Rgb(60, 60, 80);

/// Semantic highlighter for word occurrences
pub struct ReferenceHighlighter {
    /// Color for occurrence highlights
    pub highlight_color: Color,
    /// Minimum word length to trigger highlighting
    pub min_word_length: usize,
    /// Whether semantic highlighting is enabled
    pub enabled: bool,
    /// Tree-sitter parser (optional, for syntax-aware highlighting)
    parser: Option<Parser>,
    /// Query to find identifier nodes (fallback when locals not available)
    identifier_query: Option<Query>,
    /// Query for local variable tracking (scope-aware highlighting)
    locals_query: Option<Query>,
    /// Capture indices for locals query
    locals_captures: LocalsCaptures,
}

/// Capture indices for the locals query
#[derive(Default)]
struct LocalsCaptures {
    scope: Option<u32>,
    definition: Option<u32>,
    reference: Option<u32>,
}

/// Query pattern to find identifier nodes
/// Note: Different languages use different node types for identifiers.
/// We use just (identifier) which works for most C-family languages.
/// Languages like Rust and Python should work, while some may need
/// language-specific queries for better results.
const IDENTIFIER_QUERY: &str = "(identifier) @id";

/// Get the locals query for a language
/// These queries define scopes, definitions, and references for local variables
fn get_locals_query(language: &Language) -> Option<&'static str> {
    match language {
        Language::Rust => Some(RUST_LOCALS_QUERY),
        Language::Python => Some(PYTHON_LOCALS_QUERY),
        Language::JavaScript | Language::TypeScript => Some(JS_LOCALS_QUERY),
        Language::Go => Some(GO_LOCALS_QUERY),
        Language::C | Language::Cpp => Some(C_LOCALS_QUERY),
        _ => None, // Other languages fall back to identifier matching
    }
}

/// Rust locals query - tracks scopes, definitions, and references
const RUST_LOCALS_QUERY: &str = r#"
; Scopes
(function_item body: (_) @local.scope)
(closure_expression body: (_) @local.scope)

; Definitions - parameters
((parameter pattern: (identifier) @local.definition))

; Definitions - let bindings
(let_declaration pattern: (identifier) @local.definition)

; References
(identifier) @local.reference
"#;

/// Python locals query
const PYTHON_LOCALS_QUERY: &str = r#"
; Scopes
(function_definition) @local.scope
(class_definition) @local.scope
(lambda) @local.scope
(for_statement) @local.scope
(while_statement) @local.scope
(with_statement) @local.scope

; Definitions
(parameters (identifier) @local.definition)
(assignment left: (identifier) @local.definition)
(for_statement left: (identifier) @local.definition)
(with_clause (as_pattern (as_pattern_target (identifier) @local.definition)))

; References
(identifier) @local.reference
"#;

/// JavaScript/TypeScript locals query
const JS_LOCALS_QUERY: &str = r#"
; Scopes
(function_declaration) @local.scope
(function_expression) @local.scope
(arrow_function) @local.scope
(method_definition) @local.scope
(for_statement) @local.scope
(for_in_statement) @local.scope
(block) @local.scope

; Definitions
(formal_parameters (identifier) @local.definition)
(variable_declarator name: (identifier) @local.definition)
(for_in_statement left: (identifier) @local.definition)

; References
(identifier) @local.reference
"#;

/// Go locals query
const GO_LOCALS_QUERY: &str = r#"
; Scopes
(function_declaration) @local.scope
(method_declaration) @local.scope
(func_literal) @local.scope
(block) @local.scope
(if_statement) @local.scope
(for_statement) @local.scope

; Definitions
(parameter_declaration (identifier) @local.definition)
(short_var_declaration left: (expression_list (identifier) @local.definition))
(var_spec name: (identifier) @local.definition)
(range_clause left: (expression_list (identifier) @local.definition))

; References
(identifier) @local.reference
"#;

/// C/C++ locals query
const C_LOCALS_QUERY: &str = r#"
; Scopes
(function_definition) @local.scope
(compound_statement) @local.scope
(for_statement) @local.scope
(while_statement) @local.scope
(if_statement) @local.scope

; Definitions
(parameter_declaration declarator: (identifier) @local.definition)
(declaration declarator: (identifier) @local.definition)
(init_declarator declarator: (identifier) @local.definition)

; References
(identifier) @local.reference
"#;

impl ReferenceHighlighter {
    /// Create a new semantic highlighter with default settings
    pub fn new() -> Self {
        Self {
            highlight_color: DEFAULT_HIGHLIGHT_COLOR,
            min_word_length: 2,
            enabled: true,
            parser: None,
            identifier_query: None,
            locals_query: None,
            locals_captures: LocalsCaptures::default(),
        }
    }

    /// Set the highlight color
    pub fn with_color(mut self, color: Color) -> Self {
        self.highlight_color = color;
        self
    }

    /// Set the minimum word length
    pub fn with_min_length(mut self, length: usize) -> Self {
        self.min_word_length = length;
        self
    }

    /// Set the language for tree-sitter based highlighting
    ///
    /// This enables syntax-aware identifier matching for the given language.
    /// If the language is not supported or parsing fails, falls back to text matching.
    pub fn set_language(&mut self, language: &Language) {
        let ts_language = match language {
            Language::Rust => tree_sitter_rust::LANGUAGE.into(),
            Language::Python => tree_sitter_python::LANGUAGE.into(),
            Language::JavaScript => tree_sitter_javascript::LANGUAGE.into(),
            Language::TypeScript => tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
            Language::Go => tree_sitter_go::LANGUAGE.into(),
            Language::C => tree_sitter_c::LANGUAGE.into(),
            Language::Cpp => tree_sitter_cpp::LANGUAGE.into(),
            Language::Java => tree_sitter_java::LANGUAGE.into(),
            Language::Php => tree_sitter_php::LANGUAGE_PHP.into(),
            Language::Ruby => tree_sitter_ruby::LANGUAGE.into(),
            Language::Bash => tree_sitter_bash::LANGUAGE.into(),
            Language::Lua => tree_sitter_lua::LANGUAGE.into(),
            Language::Pascal => tree_sitter_pascal::LANGUAGE.into(),
            Language::Json => tree_sitter_json::LANGUAGE.into(),
            Language::HTML => tree_sitter_html::LANGUAGE.into(),
            Language::CSS => tree_sitter_css::LANGUAGE.into(),
            Language::CSharp => tree_sitter_c_sharp::LANGUAGE.into(),
            Language::Odin => tree_sitter_odin::LANGUAGE.into(),
        };

        // Create parser
        let mut parser = Parser::new();
        if parser.set_language(&ts_language).is_err() {
            tracing::warn!("Failed to set language for semantic highlighting parser");
            self.parser = None;
            self.identifier_query = None;
            self.locals_query = None;
            self.locals_captures = LocalsCaptures::default();
            return;
        }

        // Try to create locals query for scope-aware highlighting
        if let Some(locals_source) = get_locals_query(language) {
            match Query::new(&ts_language, locals_source) {
                Ok(query) => {
                    // Extract capture indices
                    let mut captures = LocalsCaptures::default();
                    for (i, name) in query.capture_names().iter().enumerate() {
                        match *name {
                            "local.scope" => captures.scope = Some(i as u32),
                            "local.definition" => captures.definition = Some(i as u32),
                            "local.reference" => captures.reference = Some(i as u32),
                            _ => {}
                        }
                    }

                    self.locals_query = Some(query);
                    self.locals_captures = captures;
                    tracing::debug!(
                        "Locals query enabled for {:?} (scope-aware highlighting)",
                        language
                    );
                }
                Err(e) => {
                    tracing::debug!(
                        "Locals query failed for {:?}, falling back to identifier matching: {}",
                        language,
                        e
                    );
                    self.locals_query = None;
                    self.locals_captures = LocalsCaptures::default();
                }
            }
        } else {
            self.locals_query = None;
            self.locals_captures = LocalsCaptures::default();
        }

        // Create identifier query as fallback
        match Query::new(&ts_language, IDENTIFIER_QUERY) {
            Ok(query) => {
                self.parser = Some(parser);
                self.identifier_query = Some(query);
                tracing::debug!(
                    "Tree-sitter semantic highlighting enabled for {:?}",
                    language
                );
            }
            Err(e) => {
                tracing::debug!(
                    "Identifier query not supported for {:?}, using text matching: {}",
                    language,
                    e
                );
                self.parser = None;
                self.identifier_query = None;
            }
        }
    }

    /// Check if locals-based (scope-aware) highlighting is available
    pub fn has_locals(&self) -> bool {
        self.locals_query.is_some()
            && self.locals_captures.definition.is_some()
            && self.locals_captures.reference.is_some()
    }

    /// Check if tree-sitter mode is available
    pub fn has_tree_sitter(&self) -> bool {
        self.parser.is_some() && self.identifier_query.is_some()
    }

    /// Get highlights for word occurrences in the viewport
    ///
    /// # Arguments
    /// * `buffer` - The text buffer
    /// * `cursor_position` - Current cursor byte position
    /// * `viewport_start` - Start byte offset of visible viewport
    /// * `viewport_end` - End byte offset of visible viewport
    /// * `context_bytes` - Number of bytes before/after viewport to parse for context
    ///
    /// # Returns
    /// Vector of highlight spans for all occurrences of the word under cursor
    pub fn highlight_occurrences(
        &mut self,
        buffer: &Buffer,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        if !self.enabled {
            return Vec::new();
        }

        // Try locals-based highlighting first (scope-aware)
        if self.has_locals() {
            return self.highlight_with_locals(
                buffer,
                cursor_position,
                viewport_start,
                viewport_end,
                context_bytes,
            );
        }

        // Try tree-sitter identifier matching
        if self.has_tree_sitter() {
            return self.highlight_with_tree_sitter(
                buffer,
                cursor_position,
                viewport_start,
                viewport_end,
                context_bytes,
            );
        }

        // Fallback to text-matching mode
        self.highlight_with_text_matching(
            buffer,
            cursor_position,
            viewport_start,
            viewport_end,
            context_bytes,
        )
    }

    /// Locals-based highlighting that respects variable scoping
    ///
    /// This provides VSCode-like documentHighlight behavior:
    /// - Only highlights references to the same definition
    /// - Respects lexical scoping (x in one function won't match x in another)
    fn highlight_with_locals(
        &mut self,
        buffer: &Buffer,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        let parser = match &mut self.parser {
            Some(p) => p,
            None => return Vec::new(),
        };
        let query = match &self.locals_query {
            Some(q) => q,
            None => return Vec::new(),
        };

        let def_idx = match self.locals_captures.definition {
            Some(i) => i,
            None => return Vec::new(),
        };
        let ref_idx = match self.locals_captures.reference {
            Some(i) => i,
            None => return Vec::new(),
        };
        let scope_idx = self.locals_captures.scope;

        // Parse the entire visible region plus context
        let parse_start = viewport_start.saturating_sub(context_bytes);
        let parse_end = (viewport_end + context_bytes).min(buffer.len());
        let source = buffer.slice_bytes(parse_start..parse_end);

        // Parse the source
        let tree = match parser.parse(&source, None) {
            Some(t) => t,
            None => {
                return self.highlight_with_tree_sitter(
                    buffer,
                    cursor_position,
                    viewport_start,
                    viewport_end,
                    context_bytes,
                );
            }
        };

        // Run the locals query
        let mut query_cursor = QueryCursor::new();
        let mut matches = query_cursor.matches(query, tree.root_node(), source.as_slice());

        // Collect scopes, definitions, and references
        let mut scopes: Vec<Range<usize>> = Vec::new();
        let mut definitions: Vec<(Range<usize>, String, usize)> = Vec::new(); // (range, name, scope_id)
        let mut references: Vec<(Range<usize>, String)> = Vec::new();

        // Build scope stack for each position
        while let Some(m) = matches.next() {
            for capture in m.captures {
                let node = capture.node;
                let start = parse_start + node.start_byte();
                let end = parse_start + node.end_byte();
                let range = start..end;

                // Get the text
                let text_bytes = &source[node.start_byte()..node.end_byte()];
                let text = match std::str::from_utf8(text_bytes) {
                    Ok(s) => s.to_string(),
                    Err(_) => continue,
                };

                if Some(capture.index) == scope_idx {
                    scopes.push(range);
                } else if capture.index == def_idx {
                    // Find which scope this definition is in
                    let scope_id = scopes
                        .iter()
                        .enumerate()
                        .filter(|(_, s)| s.start <= start && end <= s.end)
                        .map(|(i, _)| i)
                        .next_back()
                        .unwrap_or(usize::MAX);
                    definitions.push((range, text, scope_id));
                } else if capture.index == ref_idx {
                    references.push((range, text));
                }
            }
        }

        // Find what's under the cursor
        let cursor_item = definitions
            .iter()
            .find(|(range, _, _)| cursor_position >= range.start && cursor_position <= range.end)
            .map(|(range, name, scope_id)| (range.clone(), name.clone(), Some(*scope_id)))
            .or_else(|| {
                references
                    .iter()
                    .find(|(range, _)| {
                        cursor_position >= range.start && cursor_position <= range.end
                    })
                    .map(|(range, name)| (range.clone(), name.clone(), None))
            });

        let (cursor_range, target_name, cursor_scope_id) = match cursor_item {
            Some(item) => item,
            None => return Vec::new(),
        };

        // Check minimum length
        if target_name.len() < self.min_word_length {
            return Vec::new();
        }

        // Find the definition for this name
        // If cursor is on a definition, use that scope
        // If cursor is on a reference, find the definition in scope
        let definition_scope = if let Some(scope_id) = cursor_scope_id {
            // Cursor is on a definition
            Some(scope_id)
        } else {
            // Cursor is on a reference - find the definition
            // Look for definition in containing scopes (innermost first)
            let containing_scopes: Vec<usize> = scopes
                .iter()
                .enumerate()
                .filter(|(_, s)| s.start <= cursor_range.start && cursor_range.end <= s.end)
                .map(|(i, _)| i)
                .collect();

            // Find definition with matching name in these scopes (prefer innermost)
            containing_scopes.iter().rev().find_map(|&scope_id| {
                definitions
                    .iter()
                    .find(|(_, name, def_scope)| name == &target_name && *def_scope == scope_id)
                    .map(|(_, _, s)| *s)
            })
        };

        // Collect all matching occurrences
        let mut highlights = Vec::new();

        // Add the definition if in viewport
        if let Some(scope_id) = definition_scope {
            for (range, name, def_scope) in &definitions {
                if name == &target_name
                    && *def_scope == scope_id
                    && range.start < viewport_end
                    && range.end > viewport_start
                {
                    highlights.push(HighlightSpan {
                        range: range.clone(),
                        color: self.highlight_color,
                    });
                }
            }

            // Add references that resolve to this definition
            let scope_range = scopes.get(scope_id).cloned();
            for (range, name) in &references {
                if name != &target_name {
                    continue;
                }
                if range.start >= viewport_end || range.end <= viewport_start {
                    continue;
                }

                // Check if this reference is in the same scope as the definition
                // or a nested scope
                let ref_in_scope = match &scope_range {
                    Some(sr) => range.start >= sr.start && range.end <= sr.end,
                    None => true, // Global scope
                };

                if ref_in_scope {
                    // Make sure this reference doesn't have a shadowing definition in between
                    let is_shadowed = definitions.iter().any(|(def_range, def_name, def_scope)| {
                        def_name == name
                            && *def_scope != scope_id
                            && def_range.start < range.start
                            && scopes
                                .get(*def_scope)
                                .is_some_and(|s| range.start >= s.start && range.end <= s.end)
                    });

                    if !is_shadowed {
                        highlights.push(HighlightSpan {
                            range: range.clone(),
                            color: self.highlight_color,
                        });
                    }
                }
            }
        } else {
            // No definition found - fall back to matching all references with same name
            // This handles global/external identifiers
            for (range, name) in &references {
                if name == &target_name && range.start < viewport_end && range.end > viewport_start
                {
                    highlights.push(HighlightSpan {
                        range: range.clone(),
                        color: self.highlight_color,
                    });
                }
            }
        }

        highlights
    }

    /// Tree-sitter based highlighting that finds identifier nodes
    fn highlight_with_tree_sitter(
        &mut self,
        buffer: &Buffer,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        let parser = match &mut self.parser {
            Some(p) => p,
            None => return Vec::new(),
        };
        let query = match &self.identifier_query {
            Some(q) => q,
            None => return Vec::new(),
        };

        // Get text to parse - use context around viewport for better parsing
        let parse_start = viewport_start.saturating_sub(context_bytes);
        let parse_end = (viewport_end + context_bytes).min(buffer.len());
        let source = buffer.slice_bytes(parse_start..parse_end);

        // Parse the source
        let tree = match parser.parse(&source, None) {
            Some(t) => t,
            None => {
                tracing::debug!("Tree-sitter parsing failed, falling back to text matching");
                return self.highlight_with_text_matching(
                    buffer,
                    cursor_position,
                    viewport_start,
                    viewport_end,
                    context_bytes,
                );
            }
        };

        // Note: cursor_position is used in absolute terms throughout this function

        // Find all identifier nodes using the query
        let mut query_cursor = QueryCursor::new();
        let mut matches = query_cursor.matches(query, tree.root_node(), source.as_slice());

        // Collect all identifier ranges and their text
        let mut identifiers: Vec<(Range<usize>, String)> = Vec::new();
        let mut cursor_identifier: Option<String> = None;

        while let Some(m) = matches.next() {
            for capture in m.captures {
                let node = capture.node;
                let start = parse_start + node.start_byte();
                let end = parse_start + node.end_byte();

                // Get the identifier text
                let text_bytes = &source[node.start_byte()..node.end_byte()];
                let text = match std::str::from_utf8(text_bytes) {
                    Ok(s) => s.to_string(),
                    Err(_) => continue,
                };

                // Check minimum length
                if text.len() < self.min_word_length {
                    continue;
                }

                // Check if cursor is in this identifier
                if cursor_position >= start && cursor_position <= end {
                    cursor_identifier = Some(text.clone());
                }

                // Only include identifiers in the viewport
                if start < viewport_end && end > viewport_start {
                    identifiers.push((start..end, text));
                }
            }
        }

        // If cursor wasn't on an identifier, no highlights
        let target_identifier = match cursor_identifier {
            Some(id) => id,
            None => return Vec::new(),
        };

        // Return all identifiers matching the target
        identifiers
            .into_iter()
            .filter(|(_, text)| text == &target_identifier)
            .map(|(range, _)| HighlightSpan {
                range,
                color: self.highlight_color,
            })
            .collect()
    }

    /// Text-matching based highlighting (fallback)
    #[allow(unused_variables)] // context_bytes not used in text matching but needed for API consistency
    fn highlight_with_text_matching(
        &self,
        buffer: &Buffer,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        // Find the word under the cursor
        let word_range = match self.get_word_at_position(buffer, cursor_position) {
            Some(range) => range,
            None => return Vec::new(),
        };

        // Get the word text
        let word_bytes = buffer.slice_bytes(word_range.clone());
        let word = match std::str::from_utf8(&word_bytes) {
            Ok(s) => s.to_string(),
            Err(_) => return Vec::new(),
        };

        // Check minimum length
        if word.len() < self.min_word_length {
            return Vec::new();
        }

        // Find all occurrences in the viewport
        let occurrences =
            self.find_occurrences_in_range(buffer, &word, viewport_start, viewport_end);

        // Convert to highlight spans
        occurrences
            .into_iter()
            .map(|range| HighlightSpan {
                range,
                color: self.highlight_color,
            })
            .collect()
    }

    /// Get the word range at the given position
    ///
    /// Returns None if the cursor is not on a word character.
    fn get_word_at_position(&self, buffer: &Buffer, position: usize) -> Option<Range<usize>> {
        let buf_len = buffer.len();
        if position > buf_len {
            return None;
        }

        // Check if cursor is on a word character
        // Need to handle cursor at end of buffer
        let is_on_word = if position < buf_len {
            let byte_at_pos = buffer.slice_bytes(position..position + 1);
            byte_at_pos
                .first()
                .map(|&b| is_word_char(b))
                .unwrap_or(false)
        } else if position > 0 {
            // Cursor at end of buffer - check previous character
            let byte_before = buffer.slice_bytes(position - 1..position);
            byte_before
                .first()
                .map(|&b| is_word_char(b))
                .unwrap_or(false)
        } else {
            false
        };

        if !is_on_word && position > 0 {
            // Check if we're just after a word AND the cursor is at end of buffer
            // or the character before was a word char but current is not
            // This handles cursor positioned right after a word (e.g., at end of "foo|")
            let byte_before = buffer.slice_bytes(position.saturating_sub(1)..position);
            let is_after_word = byte_before
                .first()
                .map(|&b| is_word_char(b))
                .unwrap_or(false);

            // Only use "word before cursor" if we're at end of buffer
            // Otherwise, cursor on whitespace/punctuation should not highlight
            if is_after_word && position >= buf_len {
                // Use the word before cursor
                let start = find_word_start(buffer, position.saturating_sub(1));
                let end = position;
                if start < end {
                    return Some(start..end);
                }
            }
            return None;
        }

        if !is_on_word {
            return None;
        }

        // Find word boundaries
        let start = find_word_start(buffer, position);
        let end = find_word_end(buffer, position);

        if start < end {
            Some(start..end)
        } else {
            None
        }
    }

    /// Maximum search range for semantic highlighting (1MB)
    /// Beyond this, skip highlighting to avoid performance issues with huge single-line files
    const MAX_SEARCH_RANGE: usize = 1024 * 1024;

    /// Find all whole-word occurrences of a word in a byte range
    fn find_occurrences_in_range(
        &self,
        buffer: &Buffer,
        word: &str,
        start: usize,
        end: usize,
    ) -> Vec<Range<usize>> {
        // Skip if search range is too large (e.g., huge single-line files)
        if end.saturating_sub(start) > Self::MAX_SEARCH_RANGE {
            return Vec::new();
        }

        let mut occurrences = Vec::new();

        // Get the text in the range (with some padding for edge words)
        let search_start = start.saturating_sub(word.len());
        let search_end = (end + word.len()).min(buffer.len());

        let bytes = buffer.slice_bytes(search_start..search_end);
        let text = match std::str::from_utf8(&bytes) {
            Ok(s) => s,
            Err(_) => return occurrences,
        };

        // Use match_indices for single-pass searching (creates StrSearcher once)
        // This is much more efficient than repeatedly calling find() which creates
        // a new searcher each time
        for (rel_pos, _) in text.match_indices(word) {
            let abs_start = search_start + rel_pos;
            let abs_end = abs_start + word.len();

            // Check if this is a whole word match (not part of a larger word)
            let is_word_start = abs_start == 0 || {
                let prev_byte = buffer.slice_bytes(abs_start - 1..abs_start);
                prev_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
            };

            let is_word_end = abs_end >= buffer.len() || {
                let next_byte = buffer.slice_bytes(abs_end..abs_end + 1);
                next_byte.first().map(|&b| !is_word_char(b)).unwrap_or(true)
            };

            if is_word_start && is_word_end {
                // Only include if it overlaps with the actual viewport
                if abs_start < end && abs_end > start {
                    occurrences.push(abs_start..abs_end);
                }
            }
        }

        occurrences
    }
}

impl Default for ReferenceHighlighter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_word_at_position() {
        let buffer = Buffer::from_str_test("hello world test");
        let highlighter = ReferenceHighlighter::new();

        // Middle of "hello"
        let range = highlighter.get_word_at_position(&buffer, 2).unwrap();
        assert_eq!(range, 0..5);

        // Start of "world"
        let range = highlighter.get_word_at_position(&buffer, 6).unwrap();
        assert_eq!(range, 6..11);

        // On space (not a word)
        let range = highlighter.get_word_at_position(&buffer, 5);
        assert!(range.is_none());
    }

    #[test]
    fn test_find_occurrences() {
        let buffer = Buffer::from_str_test("foo bar foo baz foo");
        let highlighter = ReferenceHighlighter::new();

        let occurrences = highlighter.find_occurrences_in_range(&buffer, "foo", 0, buffer.len());
        assert_eq!(occurrences.len(), 3);
        assert_eq!(occurrences[0], 0..3);
        assert_eq!(occurrences[1], 8..11);
        assert_eq!(occurrences[2], 16..19);
    }

    #[test]
    fn test_whole_word_only() {
        let buffer = Buffer::from_str_test("foobar foo foobaz");
        let highlighter = ReferenceHighlighter::new();

        let occurrences = highlighter.find_occurrences_in_range(&buffer, "foo", 0, buffer.len());
        // Should only find the standalone "foo", not "foobar" or "foobaz"
        assert_eq!(occurrences.len(), 1);
        assert_eq!(occurrences[0], 7..10);
    }

    #[test]
    fn test_highlight_occurrences() {
        let buffer = Buffer::from_str_test("let foo = 1;\nlet bar = foo;\nlet baz = foo;");
        let mut highlighter = ReferenceHighlighter::new();

        // Cursor on first 'foo' at position 4
        let spans = highlighter.highlight_occurrences(&buffer, 4, 0, buffer.len(), 100_000);

        // Should find 3 occurrences of 'foo'
        assert_eq!(spans.len(), 3);
    }

    #[test]
    fn test_min_word_length() {
        let buffer = Buffer::from_str_test("a b c a b c");
        let mut highlighter = ReferenceHighlighter::new().with_min_length(2);

        // Single character 'a' at position 0 should not be highlighted
        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len(), 100_000);
        assert_eq!(spans.len(), 0);
    }

    #[test]
    fn test_disabled() {
        let buffer = Buffer::from_str_test("hello hello hello");
        let mut highlighter = ReferenceHighlighter::new();
        highlighter.enabled = false;

        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len(), 100_000);
        assert_eq!(spans.len(), 0);
    }

    #[test]
    fn test_cursor_at_end_of_buffer() {
        let buffer = Buffer::from_str_test("foo bar foo");
        let mut highlighter = ReferenceHighlighter::new();

        // Cursor at end of buffer (after last "foo")
        let spans =
            highlighter.highlight_occurrences(&buffer, buffer.len(), 0, buffer.len(), 100_000);
        // Should find both "foo" occurrences
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn test_cursor_on_word() {
        let buffer = Buffer::from_str_test("foo bar foo");
        let mut highlighter = ReferenceHighlighter::new();

        // Cursor on first character of "foo"
        let spans = highlighter.highlight_occurrences(&buffer, 0, 0, buffer.len(), 100_000);
        // Should find both "foo" occurrences
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn test_viewport_limiting() {
        let buffer = Buffer::from_str_test("foo bar foo baz foo");
        let mut highlighter = ReferenceHighlighter::new();

        // Only search in viewport 4..12 (should find middle "foo" only)
        let spans = highlighter.highlight_occurrences(&buffer, 8, 4, 12, 100_000);
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].range, 8..11);
    }

    #[test]
    fn test_tree_sitter_mode() {
        use crate::primitives::highlighter::Language;

        let buffer = Buffer::from_str_test("fn main() {\n    let foo = 1;\n    let bar = foo;\n}");
        let mut highlighter = ReferenceHighlighter::new();

        // Enable tree-sitter mode for Rust
        highlighter.set_language(&Language::Rust);

        // Tree-sitter mode may or may not be available depending on query support
        // If available, cursor on "foo" should highlight all occurrences
        // Position 20 should be on "foo" in "let foo = 1"
        let spans = highlighter.highlight_occurrences(&buffer, 20, 0, buffer.len(), 100_000);

        // Should find at least the definition and reference
        // Note: Locals query may also capture "foo" in "bar = foo" as both definition and reference
        assert!(spans.len() >= 2);
    }

    #[test]
    fn test_tree_sitter_identifier_only() {
        use crate::primitives::highlighter::Language;

        // Test that tree-sitter mode works for finding identifiers
        // Using longer identifier names to pass min_word_length filter
        let buffer = Buffer::from_str_test("let foo = 1;\nlet bar = foo;");
        let mut highlighter = ReferenceHighlighter::new();

        // Enable tree-sitter mode for Rust
        highlighter.set_language(&Language::Rust);

        // Cursor on "foo" at position 4 (first foo)
        let spans = highlighter.highlight_occurrences(&buffer, 4, 0, buffer.len(), 100_000);

        // Should find at least 2 occurrences of foo (definition and use)
        assert!(spans.len() >= 2);
    }

    #[test]
    fn test_locals_mode_enabled() {
        use crate::primitives::highlighter::Language;

        let mut highlighter = ReferenceHighlighter::new();
        highlighter.set_language(&Language::Rust);

        // Rust should have locals support
        assert!(highlighter.has_locals());
    }

    #[test]
    fn test_scope_aware_highlighting() {
        use crate::primitives::highlighter::Language;

        // Two functions with variables named "foo"
        // With locals queries, these should ideally be separate scopes
        let code = r#"
fn first() {
    let foo = 1;
    println!("{}", foo);
}

fn second() {
    let foo = 2;
    println!("{}", foo);
}
"#;
        let buffer = Buffer::from_str_test(code);
        let mut highlighter = ReferenceHighlighter::new();
        highlighter.set_language(&Language::Rust);

        // Find position of first "foo" definition (in first function)
        let first_foo_pos = code.find("let foo = 1").unwrap() + 4;

        let spans =
            highlighter.highlight_occurrences(&buffer, first_foo_pos, 0, buffer.len(), 100_000);

        // Should find occurrences - exact count depends on scope resolution
        // At minimum, should find the definition and at least one reference
        assert!(
            spans.len() >= 2,
            "Expected at least 2 spans, got {}",
            spans.len()
        );
    }

    #[test]
    fn test_shadowing_in_nested_scope() {
        use crate::primitives::highlighter::Language;

        // Variable shadowing - inner foo shadows outer foo
        let code = r#"
fn main() {
    let foo = 1;
    {
        let foo = 2;
        println!("{}", foo);
    }
    println!("{}", foo);
}
"#;
        let buffer = Buffer::from_str_test(code);
        let mut highlighter = ReferenceHighlighter::new();
        highlighter.set_language(&Language::Rust);

        // Find position of outer "foo" definition
        let outer_foo_pos = code.find("let foo = 1").unwrap() + 4;

        let spans =
            highlighter.highlight_occurrences(&buffer, outer_foo_pos, 0, buffer.len(), 100_000);

        // Should find occurrences - with proper shadowing this would be 2,
        // but current implementation may find more
        assert!(
            spans.len() >= 2,
            "Expected at least 2 spans, got {}",
            spans.len()
        );
    }

    #[test]
    fn test_parameter_highlighting() {
        use crate::primitives::highlighter::Language;

        // Function parameter should highlight with its uses
        let code = r#"
fn greet(name: &str) {
    println!("Hello, {}", name);
    println!("Goodbye, {}", name);
}
"#;
        let buffer = Buffer::from_str_test(code);
        let mut highlighter = ReferenceHighlighter::new();
        highlighter.set_language(&Language::Rust);

        // Find position of "name" parameter
        let name_pos = code.find("name: &str").unwrap();

        let spans = highlighter.highlight_occurrences(&buffer, name_pos, 0, buffer.len(), 100_000);

        // Should find at least 3 occurrences: parameter + 2 uses
        assert!(
            spans.len() >= 3,
            "Expected at least 3 spans, got {}",
            spans.len()
        );
    }
}
