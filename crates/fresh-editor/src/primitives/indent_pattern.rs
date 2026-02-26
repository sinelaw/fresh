//! Pattern-based auto-indentation (WASM-compatible)
//!
//! This module provides language-agnostic indentation using pattern matching.
//! It works for any language using C-style delimiters: `{ } [ ] ( )`
//!
//! # Design
//!
//! When tree-sitter is not available (e.g., WASM builds) or when syntax is
//! incomplete during typing, this pattern-based approach provides reliable
//! indentation by:
//!
//! 1. Scanning backwards through the buffer looking for delimiters
//! 2. Tracking nesting depth to skip over already-matched pairs
//! 3. Finding the unmatched opening delimiter to determine indent level
//!
//! # Example
//!
//! ```text
//! if (true) {
//!     hello
//!     <cursor pressing Enter>
//! ```
//!
//! The pattern matcher sees the unmatched `{` and increases indent by tab_size.

use crate::model::buffer::Buffer;

/// Pattern-based indent calculator (WASM-compatible)
///
/// Uses heuristic pattern matching instead of tree-sitter AST analysis.
/// Works reliably with incomplete syntax which is common during typing.
pub struct PatternIndentCalculator;

impl PatternIndentCalculator {
    /// Calculate indent for a new line at the given position
    ///
    /// Returns the number of spaces to indent.
    pub fn calculate_indent(buffer: &Buffer, position: usize, tab_size: usize) -> usize {
        // Pattern-based indent (for incomplete syntax)
        if let Some(indent) = Self::calculate_indent_pattern(buffer, position, tab_size) {
            return indent;
        }

        // Final fallback: copy current line's indent (maintain indentation)
        Self::get_current_line_indent(buffer, position, tab_size)
    }

    /// Calculate the correct indent for a closing delimiter being typed
    ///
    /// When typing `}`, `]`, or `)`, this finds the matching opening delimiter
    /// and returns its indentation level for proper alignment.
    pub fn calculate_dedent_for_delimiter(
        buffer: &Buffer,
        position: usize,
        _delimiter: char,
        tab_size: usize,
    ) -> Option<usize> {
        Self::calculate_dedent_pattern(buffer, position, tab_size)
    }

    /// Pattern-based dedent calculation
    ///
    /// Scans backwards through the buffer tracking delimiter nesting to find
    /// the matching unmatched opening delimiter.
    ///
    /// # Algorithm
    /// 1. Start at cursor position, depth = 0
    /// 2. Scan backwards line by line
    /// 3. For each line's last non-whitespace character:
    ///    - Closing delimiter (`}`, `]`, `)`): increment depth
    ///    - Opening delimiter with depth > 0: decrement depth (matched pair)
    ///    - Opening delimiter with depth = 0: **found!** Return its indent
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
                // Calculate this line's indentation
                let line_indent =
                    Self::count_leading_indent(buffer, line_start, search_pos, tab_size);

                // Apply nesting depth tracking based on last character
                match last_char {
                    // Closing delimiter: increment depth to skip its matching opening
                    b'}' | b']' | b')' => {
                        depth += 1;
                    }

                    // Opening delimiter: check if it's matched or unmatched
                    b'{' | b'[' | b'(' => {
                        if depth > 0 {
                            // Already matched by a closing delimiter we saw earlier
                            depth -= 1;
                        } else {
                            // Unmatched! This is the opening delimiter we're closing
                            return Some(line_indent);
                        }
                    }

                    // Content line: continue searching
                    _ => {}
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

    /// Calculate indent using pattern matching
    ///
    /// Uses hybrid heuristic: finds previous non-empty line as reference,
    /// then applies pattern-based deltas for opening delimiters.
    fn calculate_indent_pattern(
        buffer: &Buffer,
        position: usize,
        tab_size: usize,
    ) -> Option<usize> {
        if position == 0 {
            return None;
        }

        // Find start of the line we're currently on
        let mut line_start = position;
        while line_start > 0 {
            if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                break;
            }
            line_start = line_start.saturating_sub(1);
        }

        // Get the content of the current line
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
            Self::get_current_line_indent(buffer, position, tab_size)
        } else {
            // Current line is empty - find previous non-empty line
            Self::find_reference_line_indent(buffer, line_start, tab_size)
        };

        // Check if line ends with an indent trigger
        if let Some(&last_char) = last_non_whitespace {
            match last_char {
                b'{' | b'[' | b'(' | b':' => {
                    return Some(reference_indent + tab_size);
                }
                _ => {}
            }
        }

        // No pattern match - use reference indent
        Some(reference_indent)
    }

    /// Find indent of previous non-empty line, checking for indent triggers
    fn find_reference_line_indent(buffer: &Buffer, line_start: usize, tab_size: usize) -> usize {
        let mut search_pos = if line_start > 0 {
            line_start - 1
        } else {
            return 0;
        };

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

            if let Some(&last_char) = ref_last_non_ws {
                // Found a non-empty reference line
                let line_indent =
                    Self::count_leading_indent(buffer, ref_line_start, search_pos, tab_size);

                // Check if reference line ends with indent trigger
                match last_char {
                    b'{' | b'[' | b'(' | b':' => {
                        return line_indent + tab_size;
                    }
                    _ => return line_indent,
                }
            }

            // Move to previous line
            if ref_line_start == 0 {
                break;
            }
            search_pos = ref_line_start.saturating_sub(1);
        }

        0
    }

    /// Get a single byte at a position
    pub fn byte_at(buffer: &Buffer, pos: usize) -> Option<u8> {
        if pos >= buffer.len() {
            return None;
        }
        buffer.slice_bytes(pos..pos + 1).first().copied()
    }

    /// Count leading whitespace indent
    pub fn count_leading_indent(
        buffer: &Buffer,
        line_start: usize,
        line_end: usize,
        tab_size: usize,
    ) -> usize {
        let mut indent = 0;
        let mut pos = line_start;
        while pos < line_end {
            match Self::byte_at(buffer, pos) {
                Some(b' ') => indent += 1,
                Some(b'\t') => indent += tab_size,
                Some(b'\n') => break,
                Some(_) => break, // Hit non-whitespace
                None => break,
            }
            pos += 1;
        }
        indent
    }

    /// Get the indent of the current line
    fn get_current_line_indent(buffer: &Buffer, position: usize, tab_size: usize) -> usize {
        // Find start of current line
        let mut line_start = position;
        while line_start > 0 {
            if Self::byte_at(buffer, line_start.saturating_sub(1)) == Some(b'\n') {
                break;
            }
            line_start = line_start.saturating_sub(1);
        }

        Self::count_leading_indent(buffer, line_start, position, tab_size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::NoopFileSystem;
    use std::sync::Arc;

    fn make_buffer(content: &str) -> Buffer {
        let fs = Arc::new(NoopFileSystem);
        let mut buf = Buffer::empty(fs);
        buf.insert(0, content);
        buf
    }

    #[test]
    fn test_indent_after_brace() {
        let buffer = make_buffer("fn main() {\n");
        let indent = PatternIndentCalculator::calculate_indent(&buffer, buffer.len(), 4);
        assert_eq!(indent, 4);
    }

    #[test]
    fn test_dedent_for_closing_brace() {
        let buffer = make_buffer("fn main() {\n    hello\n");
        let dedent =
            PatternIndentCalculator::calculate_dedent_for_delimiter(&buffer, buffer.len(), '}', 4);
        assert_eq!(dedent, Some(0));
    }

    #[test]
    fn test_maintain_indent() {
        let buffer = make_buffer("    hello\n");
        let indent = PatternIndentCalculator::calculate_indent(&buffer, buffer.len(), 4);
        assert_eq!(indent, 4);
    }

    #[test]
    fn test_nested_braces() {
        let buffer = make_buffer("fn main() {\n    if true {\n        inner\n    }\n");
        // Typing } should dedent to column 0 (matching the outer brace)
        let dedent =
            PatternIndentCalculator::calculate_dedent_for_delimiter(&buffer, buffer.len(), '}', 4);
        assert_eq!(dedent, Some(0));
    }
}
