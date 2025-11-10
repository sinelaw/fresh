//! Auto-indentation using tree-sitter queries
//!
//! # Design
//! - **Tree-sitter query-based**: Uses `indents.scm` query files for each language
//! - **Local context parsing**: Only parses ~1000 bytes before cursor for performance
//! - **Fallback to previous line**: If parsing fails, copies previous line's indent
//!
//! # Query Captures
//! - `@indent`: Increase indent after this node (e.g., opening `{`)
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
        if let Some(indent) = self.calculate_indent_tree_sitter(buffer, position, language, tab_size) {
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
    /// Uses tree-sitter to count nesting level and determine correct indentation
    /// Works with partial parsing by only analyzing local context
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

        // Count how many @indent nodes contain the cursor position
        let cursor_offset = position - parse_start;
        let mut indent_level: usize = 0;

        let mut query_cursor = QueryCursor::new();
        let mut captures = query_cursor.captures(query, root, source.as_slice());

        while let Some((match_result, _)) = captures.next() {
            for capture in match_result.captures {
                if capture.index == indent_capture_idx as u32 {
                    let node = capture.node;
                    let node_start = node.start_byte();
                    let node_end = node.end_byte();

                    // If cursor is inside this @indent node, count it
                    if node_start < cursor_offset && cursor_offset <= node_end {
                        indent_level += 1;
                    }
                }
            }
        }

        // The closing delimiter should be at one level less than current nesting
        // (it closes the innermost block)
        let dedent_level = indent_level.saturating_sub(1_usize);

        // Get baseline indent from start of parse window
        let baseline_indent = {
            let mut indent = 0;
            for i in 0..source.len().min(100) {
                match source.get(i) {
                    Some(b' ') => indent += 1,
                    Some(b'\t') => indent += tab_size,
                    Some(b'\n') | None => break,
                    _ => break,
                }
            }
            indent
        };

        Some(baseline_indent + (dedent_level * tab_size))
    }

    /// Calculate indent using simple pattern matching (fallback for incomplete syntax)
    /// Checks if the line before cursor ends with indent-triggering characters
    fn calculate_indent_pattern(
        buffer: &Buffer,
        position: usize,
        tab_size: usize,
    ) -> Option<usize> {
        if position == 0 {
            return None;
        }

        let base_indent = Self::get_current_line_indent(buffer, position);

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

        // Find the last non-whitespace character
        let last_non_whitespace = line_bytes
            .iter()
            .rev()
            .find(|&&b| b != b' ' && b != b'\t' && b != b'\r');

        if let Some(&last_char) = last_non_whitespace {
            tracing::debug!("Pattern match: last char = '{}'", last_char as char);
            // Check for common indent triggers
            match last_char {
                b'{' | b'[' | b'(' => {
                    // Opening braces/brackets/parens: increase indent
                    tracing::debug!("Pattern match: found opening brace/bracket at end of line");
                    return Some(base_indent + tab_size);
                }
                b':' => {
                    // Colon (for Python, YAML, etc.): increase indent
                    tracing::debug!("Pattern match: found colon at end of line");
                    return Some(base_indent + tab_size);
                }
                _ => {
                    tracing::debug!("Pattern match: no indent trigger found");
                }
            }
        }

        None
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

        // Get baseline indent: the indent of the first line in our parse window
        // This handles partial parsing where we don't see the full file
        let baseline_indent = {
            let mut first_line_start = 0;
            while first_line_start < source.len() {
                if source.get(first_line_start) == Some(&b'\n') {
                    first_line_start += 1;
                    break;
                }
                if source.get(first_line_start) != Some(&b' ') && source.get(first_line_start) != Some(&b'\t') {
                    break;
                }
                first_line_start += 1;
            }
            let mut indent = 0;
            for i in 0..first_line_start.min(source.len()) {
                match source.get(i) {
                    Some(b' ') => indent += 1,
                    Some(b'\t') => indent += tab_size,
                    Some(b'\n') | None => break,
                    _ => break,
                }
            }
            indent
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

        // Manually iterate through matches
        let mut captures = query_cursor.captures(query, root, source.as_slice());
        while let Some((match_result, _)) = captures.next() {
            for capture in match_result.captures {
                let node = capture.node;
                let node_start = node.start_byte();
                let node_end = node.end_byte();

                // Check if this node affects indent at cursor position
                if let Some(idx) = indent_capture_idx {
                    if capture.index == idx as u32 {
                        // For @indent nodes: only count if cursor is inside the node AND the node's
                        // start is on a PREVIOUS line (not the current line we're calculating indent for)
                        // This prevents counting parent nodes that started way before
                        let node_on_previous_line = node_start < line_start_offset;
                        let cursor_inside_node = node_start < cursor_offset && cursor_offset <= node_end;

                        // If last character is a closing delimiter, treat cursor as outside all @indent nodes
                        // This handles "}" + Enter -> should not indent
                        if cursor_inside_node && node_on_previous_line && !last_nonws_is_closing {
                            indent_delta += 1;
                            found_any_captures = true;
                        } else if last_nonws_is_closing {
                            // Still mark as found so we don't fall back to pattern matching
                            found_any_captures = true;
                        }
                    }
                }

                if let Some(idx) = dedent_capture_idx {
                    if capture.index == idx as u32 {
                        // Dedent node: only apply if cursor is right at the start of this dedent marker
                        // (e.g., right after typing `}` on a new line)
                        // Don't dedent just because cursor is somewhere after a `)` or `}`
                        // Also ignore zero-width nodes (error recovery nodes)
                        if cursor_offset == node_start && node_end > node_start {
                            indent_delta -= 1;
                            found_any_captures = true;
                        }
                    }
                }
            }
        }

        // If no captures were found, return None to trigger pattern-based fallback
        if !found_any_captures {
            tracing::debug!("No tree-sitter captures found, falling back to pattern matching");
            return None;
        }

        // Calculate final indent: baseline (from start of parse window) + delta from @indent/@dedent nodes
        let final_indent = (baseline_indent as i32 + (indent_delta * tab_size as i32)).max(0) as usize;

        tracing::debug!(
            "Indent calculation: baseline={}, delta={}, final={}",
            baseline_indent,
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
                Some(_) => break, // Hit non-whitespace
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
                Some(_) => break, // Hit non-whitespace
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
        assert_eq!(result, Some(4), "Should detect {{ and return 4 space indent");
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
        assert_eq!(indent_val, 4, "Should indent by 4 spaces after opening brace");
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
        assert!(ts_result.is_some(), "Tree-sitter should handle complete blocks");
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
        assert_eq!(indent, Some(8), "Should maintain nested indent level (got {:?})", indent);
    }

    #[test]
    fn test_pattern_fallback_for_incomplete_syntax() {
        // Verify pattern matching kicks in when tree-sitter can't help
        let buffer = Buffer::from_str("fn main() {");
        let position = buffer.len();

        // Pattern matching should detect the '{'
        let pattern_result = IndentCalculator::calculate_indent_pattern(&buffer, position, 4);
        assert_eq!(pattern_result, Some(4), "Pattern matching should detect opening brace");
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
        assert_eq!(indent.unwrap(), 4, "Should indent 4 spaces after opening brace");
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
        assert_eq!(indent, 4, "Should indent 4 spaces after brace even without language");
    }

    #[test]
    fn test_tree_sitter_enter_after_close_brace_returns_zero() {
        // Verify tree-sitter correctly handles Enter after closing brace
        let mut calc = IndentCalculator::new();
        let buffer = Buffer::from_str("fn main() {\n    let x = 1;\n}");
        let position = buffer.len(); // Position right after the }

        // Tree-sitter should recognize we're outside the block and return 0 indent
        let indent = calc.calculate_indent(&buffer, position, &Language::Rust, 4);
        assert_eq!(indent, Some(0), "Should return 0 indent after closing brace");

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
        let correct_indent = calc.calculate_dedent_for_delimiter(
            &buffer,
            position,
            '}',
            &Language::Rust,
            4,
        );

        // Should dedent to column 0 (same level as fn main)
        assert_eq!(correct_indent, Some(0), "Closing brace should dedent to column 0");

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
        assert!(nested_indent.is_some(), "Nested closing brace should get valid indent");
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
}
