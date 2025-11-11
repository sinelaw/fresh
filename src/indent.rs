//! Auto-indentation using a hybrid tree-sitter + pattern matching approach
//!
//! # Architecture Overview
//!
//! This module implements a pragmatic hybrid approach for auto-indentation:
//!
//! ## 1. Tree-sitter Path (Language-Aware)
//! - Uses language-specific `indents.scm` query files
//! - Analyzes AST structure to determine proper indentation
//! - **Limitation**: Only works when syntax is complete (no ERROR nodes)
//! - **Reality**: During typing, syntax is almost always incomplete at cursor position
//!
//! ## 2. Pattern Matching Path (Language-Agnostic Fallback)
//! - Searches backwards through buffer looking for delimiters
//! - Tracks nesting depth to skip over already-matched pairs
//! - Works reliably with incomplete syntax (the common case during typing)
//! - Supports any C-style language (braces, brackets, parentheses)
//!
//! ## Why This Hybrid Approach?
//!
//! When you type `}` to close a block, the buffer looks like:
//! ```text
//! if (true) {
//!     hi
//!     <cursor>
//! ```
//!
//! Tree-sitter sees incomplete syntax (missing closing brace) and produces ERROR nodes
//! with no usable structure. The pattern matching fallback handles this gracefully by:
//! 1. Scanning backwards line by line
//! 2. Tracking depth when seeing closing delimiters (skip their matching open)
//! 3. Finding the unmatched opening delimiter to dedent to its level
//!
//! ## Performance
//! - Parses up to 2000 bytes before cursor (balances accuracy vs speed)
//! - Pattern matching is O(n) where n = lines scanned (typically < 100)
//! - Tree-sitter queries cached per-language
//!
//! # Query Captures (when tree-sitter is used)
//! - `@indent`: Increase indent after this node (e.g., `block`)
//! - `@dedent`: Decrease indent for this node (e.g., closing `}`)

use crate::buffer::Buffer;
use crate::highlighter::Language;
use std::collections::HashMap;
use tree_sitter::{Parser, Query, QueryCursor, StreamingIterator};

/// Maximum bytes to parse before cursor for indent calculation
const MAX_PARSE_BYTES: usize = 2000;

/// Indent calculator using tree-sitter queries
pub struct IndentCalculator {
    /// Map of language to (parser, query)
    configs: HashMap<&'static str, (Parser, Query)>,
}

impl IndentCalculator {
    /// Create a new indent calculator
    pub fn new() -> Self {
        Self {
            configs: HashMap::new(),
        }
    }

    /// Get or create parser and query for a language
    fn get_config(&mut self, language: &Language) -> Option<(&mut Parser, &Query)> {
        let (lang_name, ts_language, query_str) = match language {
            Language::Rust => (
                "rust",
                tree_sitter_rust::LANGUAGE.into(),
                include_str!("../queries/rust/indents.scm"),
            ),
            Language::Python => (
                "python",
                tree_sitter_python::LANGUAGE.into(),
                include_str!("../queries/python/indents.scm"),
            ),
            Language::JavaScript => (
                "javascript",
                tree_sitter_javascript::LANGUAGE.into(),
                include_str!("../queries/javascript/indents.scm"),
            ),
            Language::TypeScript => (
                "typescript",
                tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
                include_str!("../queries/typescript/indents.scm"),
            ),
            Language::C => (
                "c",
                tree_sitter_c::LANGUAGE.into(),
                include_str!("../queries/c/indents.scm"),
            ),
            Language::Cpp => (
                "cpp",
                tree_sitter_cpp::LANGUAGE.into(),
                include_str!("../queries/cpp/indents.scm"),
            ),
            Language::Go => (
                "go",
                tree_sitter_go::LANGUAGE.into(),
                include_str!("../queries/go/indents.scm"),
            ),
            Language::Java => (
                "java",
                tree_sitter_java::LANGUAGE.into(),
                include_str!("../queries/java/indents.scm"),
            ),
            Language::HTML => (
                "html",
                tree_sitter_html::LANGUAGE.into(),
                include_str!("../queries/html/indents.scm"),
            ),
            Language::CSS => (
                "css",
                tree_sitter_css::LANGUAGE.into(),
                include_str!("../queries/css/indents.scm"),
            ),
            Language::Bash => (
                "bash",
                tree_sitter_bash::LANGUAGE.into(),
                include_str!("../queries/bash/indents.scm"),
            ),
            Language::Json => (
                "json",
                tree_sitter_json::LANGUAGE.into(),
                include_str!("../queries/json/indents.scm"),
            ),
            Language::Ruby => (
                "ruby",
                tree_sitter_ruby::LANGUAGE.into(),
                include_str!("../queries/ruby/indents.scm"),
            ),
            Language::Php => (
                "php",
                tree_sitter_php::LANGUAGE_PHP.into(),
                include_str!("../queries/php/indents.scm"),
            ),
            Language::Lua => (
                "lua",
                tree_sitter_lua::LANGUAGE.into(),
                include_str!("../queries/lua/indents.scm"),
            ),
            Language::CSharp => {
                // C# doesn't have a highlight query, skip indent support for now
                tracing::warn!("Auto-indent not supported for C#");
                return None;
            }
        };

        // Check if we already have this config
        if !self.configs.contains_key(lang_name) {
            // Create parser
            let mut parser = Parser::new();
            if parser.set_language(&ts_language).is_err() {
                tracing::error!("Failed to set language for {}", lang_name);
                return None;
            }

            // Create query
            let query = match Query::new(&ts_language, query_str) {
                Ok(q) => q,
                Err(e) => {
                    tracing::error!("Failed to create query for {}: {:?}", lang_name, e);
                    return None;
                }
            };

            self.configs.insert(lang_name, (parser, query));
        }

        // Return mutable references
        let (parser, query) = self.configs.get_mut(lang_name)?;
        Some((parser, query))
    }

    /// Calculate indent for a new line at the given position
    ///
    /// Returns the number of spaces to indent, or None if auto-indent should be disabled
    pub fn calculate_indent(
        &mut self,
        buffer: &Buffer,
        position: usize,
        language: &Language,
        tab_size: usize,
    ) -> Option<usize> {
        // Try tree-sitter-based indent
        if let Some(indent) =
            self.calculate_indent_tree_sitter(buffer, position, language, tab_size)
        {
            return Some(indent);
        }

        // Fallback: pattern-based indent (for incomplete syntax)
        if let Some(indent) = Self::calculate_indent_pattern(buffer, position, tab_size) {
            return Some(indent);
        }

        // Final fallback: copy current line's indent (maintain indentation)
        Some(Self::get_current_line_indent(buffer, position))
    }

    /// Calculate indent without language/tree-sitter support
    /// Uses pattern matching and current line copying as fallback
    /// This is used for files without syntax highlighting (e.g., .txt files)
    pub fn calculate_indent_no_language(
        buffer: &Buffer,
        position: usize,
        tab_size: usize,
    ) -> usize {
        // Pattern-based indent (for incomplete syntax)
        if let Some(indent) = Self::calculate_indent_pattern(buffer, position, tab_size) {
            return indent;
        }

        // Final fallback: copy current line's indent
        Self::get_current_line_indent(buffer, position)
    }

    /// Calculate the correct indent for a closing delimiter being typed
    ///
    /// # Strategy: Tree-sitter with Pattern Fallback
    ///
    /// This function attempts to use tree-sitter first, but falls back to pattern matching
    /// when the syntax is incomplete (which is the common case during typing).
    ///
    /// ## Tree-sitter Path
    /// 1. Parse buffer content before cursor (up to 2000 bytes)
    /// 2. Count @indent nodes at cursor position vs reference line
    /// 3. Calculate dedent based on nesting level difference
    /// 4. **Problem**: Fails when syntax is incomplete (e.g., missing closing brace)
    ///
    /// ## Pattern Matching Fallback (see calculate_dedent_pattern)
    /// 1. Scan backwards line by line
    /// 2. Track nesting depth (closing delimiters increment, opening decrement)
    /// 3. Find first unmatched opening delimiter
    /// 4. Dedent to its indentation level
    ///
    /// # Example
    /// ```text
    /// if (1) {
    ///     if (2) {
    ///         hi
    ///     }      // inner closing at depth 1
    ///     more
    ///     <cursor typing }>  // should dedent to column 0, not 4
    /// ```
    ///
    /// Pattern matching correctly skips the matched inner block and finds the outer `if (1) {`.
    pub fn calculate_dedent_for_delimiter(
        &mut self,
        buffer: &Buffer,
        position: usize,
        _delimiter: char,
        language: &Language,
        tab_size: usize,
    ) -> Option<usize> {
        // Get parser and query for this language
        let (parser, query) = self.get_config(language)?;

        // Extract context before cursor (for parsing)
        let parse_start = position.saturating_sub(MAX_PARSE_BYTES);
        let parse_range = parse_start..position;

        if parse_range.is_empty() {
            return Some(0);
        }

        let source = buffer.slice_bytes(parse_range.clone());

        // Parse the source
        let tree = parser.parse(&source, None)?;
        let root = tree.root_node();

        // Find capture index for @indent
        let mut indent_capture_idx = None;
        for (i, name) in query.capture_names().iter().enumerate() {
            if *name == "indent" {
                indent_capture_idx = Some(i);
                break;
            }
        }

        let indent_capture_idx = indent_capture_idx?;

        let cursor_offset = position - parse_start;

        // Hybrid heuristic: find previous non-empty line as reference
        // This is the same approach used in calculate_indent_tree_sitter
        let (reference_line_indent, reference_line_offset) = {
            let mut search_pos = position;
            let mut reference_indent = 0;
            let mut reference_offset = cursor_offset;

            // Scan backwards through the buffer to find a non-empty line
            while search_pos > 0 {
                // Find start of current line
                let mut line_start = search_pos;
                while line_start > 0 {
                    if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                        break;
                    }
                    line_start = line_start.saturating_sub(1);
                }

                // Check if this line has non-whitespace content
                let mut has_content = false;
                let mut line_indent = 0;
                let mut content_pos = line_start;
                let mut pos = line_start;
                while pos < search_pos {
                    match Self::byte_at(buffer, pos) {
                        Some(b' ') => line_indent += 1,
                        Some(b'\t') => line_indent += tab_size,
                        Some(b'\n') => break,
                        Some(_) => {
                            has_content = true;
                            content_pos = pos; // Remember where we found content
                            break;
                        }
                        None => break,
                    }
                    pos += 1;
                }

                if has_content {
                    // Found a non-empty line, use it as reference
                    reference_indent = line_indent;
                    // Use position of first non-whitespace character as reference
                    if content_pos >= parse_start {
                        reference_offset = content_pos - parse_start;
                    } else {
                        // Reference line is before parse window - use start of parse window
                        reference_offset = 0;
                    }
                    break;
                }

                // Move to previous line
                if line_start == 0 {
                    break;
                }
                search_pos = line_start.saturating_sub(1);
            }

            (reference_indent, reference_offset)
        };

        // Count @indent nodes at reference and cursor positions
        let mut reference_indent_count: i32 = 0;
        let mut cursor_indent_count: i32 = 0;

        let mut query_cursor = QueryCursor::new();
        let mut captures = query_cursor.captures(query, root, source.as_slice());

        while let Some((match_result, _)) = captures.next() {
            for capture in match_result.captures {
                if capture.index == indent_capture_idx as u32 {
                    let node = capture.node;
                    let node_start = node.start_byte();
                    let node_end = node.end_byte();

                    // Count @indent nodes at reference position
                    if node_start < reference_line_offset && reference_line_offset <= node_end {
                        reference_indent_count += 1;
                    }

                    // Count @indent nodes at cursor position
                    if node_start < cursor_offset && cursor_offset <= node_end {
                        cursor_indent_count += 1;
                    }
                }
            }
        }

        // Tree-sitter fallback: incomplete syntax produces ERROR nodes with no structure
        // This is the common case when typing (e.g., "if (true) {\n    hi\n    " is incomplete)
        // Pattern matching handles this gracefully by tracking delimiter nesting
        if cursor_indent_count == 0 && reference_indent_count == 0 {
            tracing::debug!("No @indent nodes found (incomplete syntax), using pattern fallback");
            return Self::calculate_dedent_pattern(buffer, position, tab_size);
        }

        // Tree-sitter path: Calculate relative indent based on @indent node counts
        // The closing delimiter should be at one level less than current nesting
        // Formula: reference_indent + (cursor_depth - reference_depth - 1) * tab_size
        // The -1 accounts for the closing delimiter dedenting one level
        let indent_delta = cursor_indent_count - reference_indent_count - 1;
        let final_indent =
            (reference_line_indent as i32 + (indent_delta * tab_size as i32)).max(0) as usize;

        tracing::debug!(
            "Tree-sitter dedent: reference_indent={}, cursor_depth={}, reference_depth={}, delta={}, final_indent={}",
            reference_line_indent,
            cursor_indent_count,
            reference_indent_count,
            indent_delta,
            final_indent
        );

        Some(final_indent)
    }

    /// Calculate dedent using pattern matching (fallback for incomplete syntax)
    ///
    /// This is the **primary dedent algorithm** used during typing, since tree-sitter
    /// cannot handle incomplete syntax.
    ///
    /// # Algorithm: Nesting Depth Tracking
    ///
    /// Scans backwards line by line, tracking nesting depth to skip over already-matched
    /// delimiter pairs. This ensures we find the **matching** opening delimiter, not just
    /// any opening delimiter.
    ///
    /// ## Depth Counter Logic
    /// - **Closing delimiter** (`}`, `]`, `)`) → increment depth
    ///   - Reason: We need to skip its matching opening delimiter
    /// - **Opening delimiter** (`{`, `[`, `(`) → check depth:
    ///   - If depth > 0: decrement and continue (this open is matched)
    ///   - If depth == 0: **found it!** This is the unmatched opening we're looking for
    ///
    /// ## Example Walkthrough
    /// ```text
    /// if (1) {           // ← target: we want to find this
    ///     if (2) {
    ///         hi
    ///     }              // matched pair
    ///     more
    ///     <cursor>       // typing } here
    /// ```
    ///
    /// Search backwards:
    /// 1. Line "    more" → not a delimiter, continue
    /// 2. Line "    }" → closing delimiter, depth = 1 (skip next opening)
    /// 3. Line "        hi" → not a delimiter, continue
    /// 4. Line "    if (2) {" → opening delimiter, but depth = 1, so decrement to 0, continue
    /// 5. Line "if (1) {" → opening delimiter, depth = 0, **match found!** Return indent = 0
    ///
    /// # Language Agnostic
    /// Works for any language using C-style delimiters: { } [ ] ( )
    fn calculate_dedent_pattern(
        buffer: &Buffer,
        position: usize,
        tab_size: usize,
    ) -> Option<usize> {
        let mut depth = 0;
        let mut search_pos = position;

        while search_pos > 0 {
            // Find start of line
            let mut line_start = search_pos;
            while line_start > 0 {
                if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                    break;
                }
                line_start = line_start.saturating_sub(1);
            }

            // Get line content
            let line_bytes = buffer.slice_bytes(line_start..search_pos + 1);
            let last_non_ws = line_bytes
                .iter()
                .rev()
                .find(|&&b| b != b' ' && b != b'\t' && b != b'\r' && b != b'\n');

            if let Some(&last_char) = last_non_ws {
                // Calculate this line's indentation (count leading spaces/tabs)
                let mut line_indent = 0;
                let mut pos = line_start;
                while pos < search_pos {
                    match Self::byte_at(buffer, pos) {
                        Some(b' ') => line_indent += 1,
                        Some(b'\t') => line_indent += tab_size,
                        Some(b'\n') => break,
                        Some(_) => break, // Hit non-whitespace
                        None => break,
                    }
                    pos += 1;
                }

                // Apply nesting depth tracking based on last character
                match last_char {
                    // Closing delimiter: increment depth to skip its matching opening
                    b'}' | b']' | b')' => {
                        depth += 1;
                        tracing::debug!(
                            "Pattern dedent: found closing '{}', depth now {}",
                            last_char as char,
                            depth
                        );
                    }

                    // Opening delimiter: check if it's matched or unmatched
                    b'{' | b'[' | b'(' => {
                        if depth > 0 {
                            // Already matched by a closing delimiter we saw earlier
                            depth -= 1;
                            tracing::debug!(
                                "Pattern dedent: skipping matched '{}' (depth {}→{})",
                                last_char as char,
                                depth + 1,
                                depth
                            );
                        } else {
                            // Unmatched! This is the opening delimiter we're closing
                            tracing::debug!(
                                "Pattern dedent: found unmatched '{}' at indent {}",
                                last_char as char,
                                line_indent
                            );
                            return Some(line_indent);
                        }
                    }

                    // Content line: continue searching
                    _ => {
                        tracing::debug!(
                            "Pattern dedent: line ends with '{}', continuing",
                            last_char as char
                        );
                    }
                }
            }

            // Move to previous line
            if line_start == 0 {
                break;
            }
            search_pos = line_start.saturating_sub(1);
        }

        // No matching opening delimiter found - dedent to column 0
        Some(0)
    }

    /// Calculate indent using simple pattern matching (fallback for incomplete syntax)
    /// Uses hybrid heuristic: finds previous non-empty line as reference, then applies pattern-based deltas
    fn calculate_indent_pattern(
        buffer: &Buffer,
        position: usize,
        tab_size: usize,
    ) -> Option<usize> {
        if position == 0 {
            return None;
        }

        // Find start of the line we're currently on (before pressing Enter)
        let mut line_start = position;
        while line_start > 0 {
            if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                break;
            }
            line_start = line_start.saturating_sub(1);
        }

        // Get the content of the current line (the one we're leaving)
        let line_bytes = buffer.slice_bytes(line_start..position);

        // Find the last non-whitespace character on current line
        let last_non_whitespace = line_bytes
            .iter()
            .rev()
            .find(|&&b| b != b' ' && b != b'\t' && b != b'\r');

        // Check if current line is empty (only whitespace)
        let current_line_is_empty = last_non_whitespace.is_none();

        // Hybrid heuristic: find previous non-empty line for reference
        let reference_indent = if !current_line_is_empty {
            // Current line has content - use its indent as reference
            Self::get_current_line_indent(buffer, position)
        } else {
            // Current line is empty - find previous non-empty line and check for indent triggers
            let mut search_pos = if line_start > 0 {
                line_start - 1 // Position of \n before current line
            } else {
                0
            };

            let mut found_reference_indent = 0;
            while search_pos > 0 {
                // Find start of line
                let mut ref_line_start = search_pos;
                while ref_line_start > 0 {
                    if Self::byte_at(buffer, ref_line_start.saturating_sub(1)) == Some(b'\n') {
                        break;
                    }
                    ref_line_start = ref_line_start.saturating_sub(1);
                }

                // Check if this line has non-whitespace content
                let ref_line_bytes = buffer.slice_bytes(ref_line_start..search_pos + 1);
                let ref_last_non_ws = ref_line_bytes
                    .iter()
                    .rev()
                    .find(|&&b| b != b' ' && b != b'\t' && b != b'\r' && b != b'\n');

                if ref_last_non_ws.is_some() {
                    // Found a non-empty reference line - calculate its indent
                    let mut line_indent = 0;
                    let mut pos = ref_line_start;
                    while pos <= search_pos {
                        let byte_opt = Self::byte_at(buffer, pos);
                        match byte_opt {
                            Some(b' ') => line_indent += 1,
                            Some(b'\t') => line_indent += tab_size,
                            Some(b'\n') => break,
                            Some(_) => break, // Hit non-whitespace, done counting indent
                            None => break,
                        }
                        pos += 1;
                    }

                    found_reference_indent = line_indent;

                    // Check if reference line ends with indent trigger
                    if let Some(&last_char) = ref_last_non_ws {
                        match last_char {
                            b'{' | b'[' | b'(' => {
                                tracing::debug!(
                                    "Pattern match: reference line ends with '{}'",
                                    last_char as char
                                );
                                return Some(found_reference_indent + tab_size);
                            }
                            b':' => {
                                tracing::debug!("Pattern match: reference line ends with colon");
                                return Some(found_reference_indent + tab_size);
                            }
                            _ => {}
                        }
                    }
                    break;
                }

                // Move to previous line
                if ref_line_start == 0 {
                    break;
                }
                search_pos = ref_line_start.saturating_sub(1);
            }

            // Return the reference indent we found (or 0 if no non-empty line was found)
            found_reference_indent
        };

        // If current line ends with indent trigger, add to reference
        if let Some(&last_char) = last_non_whitespace {
            tracing::debug!("Pattern match: last char = '{}'", last_char as char);
            match last_char {
                b'{' | b'[' | b'(' => {
                    // Opening braces/brackets/parens: increase indent
                    tracing::debug!("Pattern match: found opening brace/bracket at end of line");
                    return Some(reference_indent + tab_size);
                }
                b':' => {
                    // Colon (for Python, YAML, etc.): increase indent
                    tracing::debug!("Pattern match: found colon at end of line");
                    return Some(reference_indent + tab_size);
                }
                _ => {
                    tracing::debug!("Pattern match: no indent trigger found");
                }
            }
        }

        // Current line is empty and has no indent trigger - use reference indent
        Some(reference_indent)
    }

    /// Calculate indent using tree-sitter queries
    fn calculate_indent_tree_sitter(
        &mut self,
        buffer: &Buffer,
        position: usize,
        language: &Language,
        tab_size: usize,
    ) -> Option<usize> {
        // Get parser and query
        let (parser, query) = self.get_config(language)?;

        // Extract context before cursor (for parsing)
        let parse_start = position.saturating_sub(MAX_PARSE_BYTES);
        let parse_range = parse_start..position;

        if parse_range.is_empty() {
            return None;
        }

        let source = buffer.slice_bytes(parse_range.clone());

        // Parse the source
        let tree = parser.parse(&source, None)?;
        let root = tree.root_node();

        // Find capture indices for @indent and @dedent
        let mut indent_capture_idx = None;
        let mut dedent_capture_idx = None;
        for (i, name) in query.capture_names().iter().enumerate() {
            if *name == "indent" {
                indent_capture_idx = Some(i);
            } else if *name == "dedent" {
                dedent_capture_idx = Some(i);
            }
        }

        // Query for indent/dedent captures
        let mut query_cursor = QueryCursor::new();

        // Count indent/dedent at cursor position
        // The cursor position in the parsed text is (position - parse_start)
        let cursor_offset = position - parse_start;

        let mut indent_delta = 0i32;
        let mut found_any_captures = false;

        // Find the line start to get the base column offset
        let mut line_start_offset = cursor_offset;
        while line_start_offset > 0 {
            if source.get(line_start_offset.saturating_sub(1)) == Some(&b'\n') {
                break;
            }
            line_start_offset = line_start_offset.saturating_sub(1);
        }

        // Find the previous non-empty line in the buffer to use as reference
        // This is the "hybrid heuristic" approach: calculate indent delta relative to actual code
        let (reference_line_indent, reference_line_offset) = {
            let mut search_pos = position;
            let mut reference_indent = 0;
            let mut reference_offset = cursor_offset;

            // Scan backwards through the buffer to find a non-empty line
            while search_pos > 0 {
                // Find start of current line
                let mut line_start = search_pos;
                while line_start > 0 {
                    if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                        break;
                    }
                    line_start = line_start.saturating_sub(1);
                }

                // Check if this line has non-whitespace content
                let mut has_content = false;
                let mut line_indent = 0;
                let mut content_pos = line_start;
                let mut pos = line_start;
                while pos < search_pos {
                    match Self::byte_at(buffer, pos) {
                        Some(b' ') => line_indent += 1,
                        Some(b'\t') => line_indent += tab_size,
                        Some(b'\n') => break,
                        Some(_) => {
                            has_content = true;
                            content_pos = pos; // Remember where we found content
                            break;
                        }
                        None => break,
                    }
                    pos += 1;
                }

                if has_content {
                    // Found a non-empty line, use it as reference
                    reference_indent = line_indent;
                    // Use position of first non-whitespace character as reference
                    // This ensures we're measuring from inside the content, not at line boundaries
                    if content_pos >= parse_start {
                        reference_offset = content_pos - parse_start;
                    } else {
                        // Reference line is before parse window - use start of parse window
                        reference_offset = 0;
                    }
                    break;
                }

                // Move to previous line
                if line_start == 0 {
                    break;
                }
                search_pos = line_start.saturating_sub(1);
            }

            (reference_indent, reference_offset)
        };

        // Check if the last non-whitespace character before cursor is a closing delimiter
        // If so, we should NOT be inside any @indent node for the purposes of the next line
        let last_nonws_is_closing = {
            let mut result = false;
            let mut pos = cursor_offset;
            while pos > line_start_offset {
                pos -= 1;
                match source.get(pos) {
                    Some(b' ') | Some(b'\t') | Some(b'\r') => continue,
                    Some(b'}') | Some(b']') | Some(b')') => {
                        result = true;
                        break;
                    }
                    _ => break,
                }
            }
            result
        };

        // Calculate indent delta using hybrid heuristic:
        // Count @indent nodes at reference line and at cursor, then compute the difference
        let mut reference_indent_count: i32 = 0;
        let mut cursor_indent_count: i32 = 0;

        // Manually iterate through matches to count indent/dedent captures
        let mut captures = query_cursor.captures(query, root, source.as_slice());
        while let Some((match_result, _)) = captures.next() {
            for capture in match_result.captures {
                let node = capture.node;
                let node_start = node.start_byte();
                let node_end = node.end_byte();

                // Count @indent nodes at reference position
                if let Some(idx) = indent_capture_idx {
                    if capture.index == idx as u32 {
                        // Reference line: count if reference position is inside this node
                        if node_start < reference_line_offset && reference_line_offset <= node_end {
                            reference_indent_count += 1;
                        }

                        // Cursor position: count if cursor is inside this node
                        // Also check: node must start on a previous line (not current line)
                        let node_on_previous_line = node_start < line_start_offset;
                        let cursor_inside_node =
                            node_start < cursor_offset && cursor_offset <= node_end;

                        if cursor_inside_node && node_on_previous_line && !last_nonws_is_closing {
                            cursor_indent_count += 1;
                            found_any_captures = true;
                        } else if last_nonws_is_closing && cursor_inside_node {
                            // Mark as found but don't count (closing delimiter line)
                            found_any_captures = true;
                        }
                    }
                }

                // Handle @dedent at cursor position
                if let Some(idx) = dedent_capture_idx {
                    if capture.index == idx as u32 {
                        // Dedent node: only apply if cursor is right at the start of this dedent marker
                        // Also ignore zero-width nodes (error recovery nodes)
                        if cursor_offset == node_start && node_end > node_start {
                            indent_delta -= 1;
                            found_any_captures = true;
                        }
                    }
                }
            }
        }

        // Calculate delta: how many more @indent levels are we at cursor vs reference
        indent_delta += cursor_indent_count - reference_indent_count;

        // If no captures were found, return None to trigger pattern-based fallback
        if !found_any_captures {
            tracing::debug!("No tree-sitter captures found, falling back to pattern matching");
            return None;
        }

        // Calculate final indent: reference line indent + delta
        let final_indent =
            (reference_line_indent as i32 + (indent_delta * tab_size as i32)).max(0) as usize;

        tracing::debug!(
            "Indent calculation: reference={}, delta={}, final={}",
            reference_line_indent,
            indent_delta,
            final_indent
        );

        Some(final_indent)
    }

    /// Get a single byte at a position
    fn byte_at(buffer: &Buffer, pos: usize) -> Option<u8> {
        if pos >= buffer.len() {
            return None;
        }
        buffer.slice_bytes(pos..pos + 1).first().copied()
    }

    /// Get the indent of the current line (the line cursor is on)
    fn get_current_line_indent(buffer: &Buffer, position: usize) -> usize {
        // Find start of current line
        let mut line_start = position;
        while line_start > 0 {
            if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                break;
            }
            line_start = line_start.saturating_sub(1);
        }

        // Count leading whitespace on current line
        let mut indent = 0;
        let mut pos = line_start;
        while pos < position {
            match Self::byte_at(buffer, pos) {
                Some(b' ') => indent += 1,
                Some(b'\t') => indent += 4, // Assuming tab = 4 spaces
                Some(_) => break,           // Hit non-whitespace
                None => break,
            }
            pos += 1;
        }

        indent
    }

    /// Get the indent of the previous line (line before cursor's line)
    fn get_previous_line_indent(buffer: &Buffer, position: usize) -> usize {
        // Find start of current line
        let mut line_start = position;
        while line_start > 0 {
            if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                break;
            }
            line_start = line_start.saturating_sub(1);
        }

        // Find start of previous line
        if line_start == 0 {
            return 0;
        }

        let mut prev_line_start = line_start - 1;
        while prev_line_start > 0 {
            if Self::byte_at(buffer, prev_line_start.saturating_sub(1)) == Some(b'\n') {
                break;
            }
            prev_line_start = prev_line_start.saturating_sub(1);
        }

        // Count leading whitespace on previous line
        let mut indent = 0;
        let mut pos = prev_line_start;
        while pos < line_start - 1 {
            match Self::byte_at(buffer, pos) {
                Some(b' ') => indent += 1,
                Some(b'\t') => indent += 4, // Assuming tab = 4 spaces
                Some(_) => break,           // Hit non-whitespace
                None => break,
            }
            pos += 1;
        }

        indent
    }
}

impl Default for IndentCalculator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;

    #[test]
    fn test_current_and_previous_line_indent() {
        let buffer = Buffer::from_str("fn main() {\n    let x = 1;");

        // At end of buffer (end of line 2)
        let current_indent = IndentCalculator::get_current_line_indent(&buffer, buffer.len());
        assert_eq!(current_indent, 4, "Current line (line 2) has 4 spaces");

        let prev_indent = IndentCalculator::get_previous_line_indent(&buffer, buffer.len());
        assert_eq!(prev_indent, 0, "Previous line (line 1) has 0 spaces");
    }

    #[test]
    fn test_pattern_matching_basic() {
        let buffer = Buffer::from_str("fn main() {");
        let position = buffer.len();
        let result = IndentCalculator::calculate_indent_pattern(&buffer, position, 4);
        println!("Pattern result for 'fn main() {{': {:?}", result);
        assert_eq!(
            result,
            Some(4),
            "Should detect {{ and return 4 space indent"
        );
    }

    #[test]
    fn test_rust_indent_after_brace_debug() {
        let mut calc = IndentCalculator::new();
        let buffer = Buffer::from_str("fn main() {");
        let position = buffer.len(); // After the {

        // Test pattern matching directly first
        let pattern_result = IndentCalculator::calculate_indent_pattern(&buffer, position, 4);
        println!("Pattern matching result: {:?}", pattern_result);

        // This should trigger tree-sitter parsing
        let indent = calc.calculate_indent(&buffer, position, &Language::Rust, 4);

        println!("Test buffer: {:?}", buffer.to_string());
        println!("Position: {}", position);
        println!("Result indent: {:?}", indent);

        assert!(indent.is_some(), "Should return Some indent");
        let indent_val = indent.unwrap();
        println!("Indent value: {}", indent_val);

        // Should suggest indenting (4 spaces)
        assert_eq!(
            indent_val, 4,
            "Should indent by 4 spaces after opening brace"
        );
    }

    #[test]
    fn test_python_indent_after_colon() {
        let mut calc = IndentCalculator::new();
        let buffer = Buffer::from_str("def foo():");
        let position = buffer.len(); // After the :

        let indent = calc.calculate_indent(&buffer, position, &Language::Python, 4);
        assert!(indent.is_some());
        // Should suggest indenting
        assert!(indent.unwrap() >= 4);
    }

    #[test]
    fn test_tree_sitter_used_for_complete_block() {
        // Test that tree-sitter is used when we have a complete block with context
        let mut calc = IndentCalculator::new();
        let buffer = Buffer::from_str("fn main() {\n    let x = 1;\n}");
        // Position after the closing }
        let position = buffer.len();

        // Tree-sitter should recognize this is a complete block
        // Pattern matching would see '}' and not indent, but tree-sitter context should work
        let ts_result = calc.calculate_indent_tree_sitter(&buffer, position, &Language::Rust, 4);

        // Tree-sitter should return Some (even if it's 0 indent)
        assert!(
            ts_result.is_some(),
            "Tree-sitter should handle complete blocks"
        );
    }

    #[test]
    fn test_nested_indent_maintained() {
        // Test that we maintain nested indentation correctly
        let mut calc = IndentCalculator::new();

        // Create nested structure - position at end of line with just whitespace
        let buffer = Buffer::from_str("fn main() {\n    if true {\n        ");
        let position = buffer.len();

        // This should be 8 spaces (maintaining nested indent from current line)
        let indent = calc.calculate_indent(&buffer, position, &Language::Rust, 4);
        assert_eq!(
            indent,
            Some(8),
            "Should maintain nested indent level (got {:?})",
            indent
        );
    }

    #[test]
    fn test_pattern_fallback_for_incomplete_syntax() {
        // Verify pattern matching kicks in when tree-sitter can't help
        let buffer = Buffer::from_str("fn main() {");
        let position = buffer.len();

        // Pattern matching should detect the '{'
        let pattern_result = IndentCalculator::calculate_indent_pattern(&buffer, position, 4);
        assert_eq!(
            pattern_result,
            Some(4),
            "Pattern matching should detect opening brace"
        );
    }

    #[test]
    fn test_fallback_to_previous_line() {
        let mut calc = IndentCalculator::new();
        // C# not supported, should fall back
        let buffer = Buffer::from_str("    var x = 1;");
        let position = buffer.len();

        let indent = calc.calculate_indent(&buffer, position, &Language::CSharp, 4);
        // Should fall back to previous line indent (4 spaces)
        assert_eq!(indent, Some(4));
    }

    #[test]
    fn test_typescript_interface_indent() {
        let mut calc = IndentCalculator::new();
        let buffer = Buffer::from_str("interface User {");
        let position = buffer.len(); // Position after the {

        let indent = calc.calculate_indent(&buffer, position, &Language::TypeScript, 4);
        assert!(indent.is_some(), "TypeScript interface should get indent");
        assert_eq!(
            indent.unwrap(),
            4,
            "Should indent 4 spaces after opening brace"
        );
    }

    #[test]
    fn test_no_language_fallback_copies_indent() {
        // Test that files without language support (like .txt) copy current line indent
        let buffer = Buffer::from_str("    indented text");
        let position = buffer.len();

        let indent = IndentCalculator::calculate_indent_no_language(&buffer, position, 4);
        assert_eq!(indent, 4, "Should copy 4-space indent from current line");
    }

    #[test]
    fn test_no_language_fallback_with_brace() {
        // Test that pattern matching works for files without language support
        let buffer = Buffer::from_str("some text {");
        let position = buffer.len();

        let indent = IndentCalculator::calculate_indent_no_language(&buffer, position, 4);
        assert_eq!(
            indent, 4,
            "Should indent 4 spaces after brace even without language"
        );
    }

    #[test]
    fn test_tree_sitter_enter_after_close_brace_returns_zero() {
        // Verify tree-sitter correctly handles Enter after closing brace
        let mut calc = IndentCalculator::new();
        let buffer = Buffer::from_str("fn main() {\n    let x = 1;\n}");
        let position = buffer.len(); // Position right after the }

        // Tree-sitter should recognize we're outside the block and return 0 indent
        let indent = calc.calculate_indent(&buffer, position, &Language::Rust, 4);
        assert_eq!(
            indent,
            Some(0),
            "Should return 0 indent after closing brace"
        );

        // Verify tree-sitter is being used (not just pattern fallback)
        let ts_result = calc.calculate_indent_tree_sitter(&buffer, position, &Language::Rust, 4);
        assert!(ts_result.is_some(), "Tree-sitter should handle this case");
    }

    #[test]
    fn test_tree_sitter_auto_dedent_on_close_brace() {
        // Verify tree-sitter correctly calculates dedent for closing delimiter
        let mut calc = IndentCalculator::new();

        // Simulate typing } on an indented line
        let buffer = Buffer::from_str("fn main() {\n    ");
        let position = buffer.len(); // Cursor after 4 spaces

        // Calculate where the } should be placed using tree-sitter
        let correct_indent =
            calc.calculate_dedent_for_delimiter(&buffer, position, '}', &Language::Rust, 4);

        // Should dedent to column 0 (same level as fn main)
        assert_eq!(
            correct_indent,
            Some(0),
            "Closing brace should dedent to column 0"
        );

        // Verify this uses tree-sitter by checking it works
        let nested_buffer = Buffer::from_str("fn main() {\n    if true {\n        ");
        let nested_pos = nested_buffer.len();

        let nested_indent = calc.calculate_dedent_for_delimiter(
            &nested_buffer,
            nested_pos,
            '}',
            &Language::Rust,
            4,
        );

        // Should return a valid indent level
        assert!(
            nested_indent.is_some(),
            "Nested closing brace should get valid indent"
        );
    }

    #[test]
    fn test_tree_sitter_handles_multiple_languages() {
        // Verify tree-sitter-based auto-dedent works across languages
        let mut calc = IndentCalculator::new();

        // Python
        let py_buffer = Buffer::from_str("def foo():\n    ");
        let py_indent = calc.calculate_indent(&py_buffer, py_buffer.len(), &Language::Python, 4);
        assert_eq!(py_indent, Some(4), "Python should indent after colon");

        // JavaScript
        let js_buffer = Buffer::from_str("function foo() {\n    ");
        let js_dedent = calc.calculate_dedent_for_delimiter(
            &js_buffer,
            js_buffer.len(),
            '}',
            &Language::JavaScript,
            4,
        );
        assert_eq!(js_dedent, Some(0), "JavaScript closing brace should dedent");

        // C++
        let cpp_buffer = Buffer::from_str("class Foo {\n    ");
        let cpp_dedent = calc.calculate_dedent_for_delimiter(
            &cpp_buffer,
            cpp_buffer.len(),
            '}',
            &Language::Cpp,
            4,
        );
        assert_eq!(cpp_dedent, Some(0), "C++ closing brace should dedent");
    }

    #[test]
    fn test_indent_after_empty_line_uses_reference() {
        // Test the hybrid heuristic: indent after empty line should use previous non-empty line as reference
        let mut calc = IndentCalculator::new();

        // Buffer with closing brace to test if tree-sitter works with complete syntax
        let buffer = Buffer::from_str("fn main() {\n    let x = 1;\n}");
        let position = 27; // Position after second \n, before the }

        let indent = calc.calculate_indent(&buffer, position, &Language::Rust, 4);
        eprintln!("TEST: With closing brace, indent = {:?}", indent);
        assert_eq!(
            indent,
            Some(4),
            "After empty line in function body, should indent to 4 spaces (reference line has 4, we're in same block)"
        );
    }

    #[test]
    fn test_indent_after_empty_line_incomplete_syntax() {
        // Test with incomplete syntax (no closing brace) - this is the real-world case
        let mut calc = IndentCalculator::new();

        let buffer = Buffer::from_str("fn main() {\n    let x = 1;\n");
        let position = buffer.len(); // After the second \n, start of empty line

        let indent = calc.calculate_indent(&buffer, position, &Language::Rust, 4);
        eprintln!("TEST: Without closing brace, indent = {:?}", indent);
        // With incomplete syntax, tree-sitter returns ERROR nodes
        // We should fall back to pattern matching or reference line heuristic
        assert_eq!(
            indent,
            Some(4),
            "After empty line in function body (incomplete syntax), should indent to 4 spaces using reference line"
        );
    }
}
