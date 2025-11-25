//! Token-based view rendering pipeline
//!
//! This module provides a clean pipeline for rendering view tokens:
//!
//! ```text
//! source buffer
//!     ↓ build_base_tokens()
//! Vec<ViewTokenWire>  (base tokens with source mappings)
//!     ↓ plugin transform (optional)
//! Vec<ViewTokenWire>  (transformed tokens, may have injected content)
//!     ↓ apply_wrapping() (optional)
//! Vec<ViewTokenWire>  (with Break tokens for wrapped lines)
//!     ↓ ViewLineIterator
//! Iterator<ViewLine>  (one per display line, preserves token info)
//!     ↓ render
//! Display output
//! ```
//!
//! The key design principle: preserve token-level information through the pipeline
//! so rendering decisions (like line numbers) can be made based on token types,
//! not reconstructed from flattened text.

use crate::plugin_api::{ViewTokenStyle, ViewTokenWire, ViewTokenWireKind};
use std::collections::HashSet;

/// A display line built from tokens, preserving token-level information
#[derive(Debug, Clone)]
pub struct ViewLine {
    /// The display text for this line (tabs expanded, etc.)
    pub text: String,
    /// Source offset mapping for each character position
    pub char_mappings: Vec<Option<usize>>,
    /// Style for each character position (from token styles)
    pub char_styles: Vec<Option<ViewTokenStyle>>,
    /// Positions that are the start of a tab expansion
    pub tab_starts: HashSet<usize>,
    /// How this line started (what kind of token/boundary preceded it)
    pub line_start: LineStart,
    /// Whether this line ends with a newline character
    pub ends_with_newline: bool,
}

/// What preceded the start of a display line
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineStart {
    /// First line of the view (no preceding token)
    Beginning,
    /// Line after a source Newline token (source_offset: Some)
    AfterSourceNewline,
    /// Line after an injected Newline token (source_offset: None)
    AfterInjectedNewline,
    /// Line after a Break token (wrapped continuation)
    AfterBreak,
}

impl LineStart {
    /// Should this line show a line number in the gutter?
    ///
    /// - Beginning: yes (first source line)
    /// - AfterSourceNewline: yes (new source line)
    /// - AfterInjectedNewline: depends on content (if injected, no; if source, yes)
    /// - AfterBreak: no (wrapped continuation of same line)
    pub fn is_continuation(&self) -> bool {
        matches!(self, LineStart::AfterBreak)
    }
}

/// Standard tab width for terminal display
pub const TAB_WIDTH: usize = 8;

/// Expand a tab to spaces based on current column
fn tab_expansion_width(col: usize) -> usize {
    TAB_WIDTH - (col % TAB_WIDTH)
}

/// Iterator that converts a token stream into display lines
pub struct ViewLineIterator<'a> {
    tokens: &'a [ViewTokenWire],
    token_idx: usize,
    /// How the next line should start (based on what ended the previous line)
    next_line_start: LineStart,
}

impl<'a> ViewLineIterator<'a> {
    pub fn new(tokens: &'a [ViewTokenWire]) -> Self {
        Self {
            tokens,
            token_idx: 0,
            next_line_start: LineStart::Beginning,
        }
    }
}

impl<'a> Iterator for ViewLineIterator<'a> {
    type Item = ViewLine;

    fn next(&mut self) -> Option<Self::Item> {
        if self.token_idx >= self.tokens.len() {
            return None;
        }

        let line_start = self.next_line_start;
        let mut text = String::new();
        let mut char_mappings = Vec::new();
        let mut char_styles = Vec::new();
        let mut tab_starts = HashSet::new();
        let mut col = 0usize;
        let mut ends_with_newline = false;

        // Process tokens until we hit a line break
        while self.token_idx < self.tokens.len() {
            let token = &self.tokens[self.token_idx];
            let token_style = token.style.clone();

            match &token.kind {
                ViewTokenWireKind::Text(t) => {
                    let base = token.source_offset;
                    let mut byte_idx = 0;
                    for ch in t.chars() {
                        let ch_len = ch.len_utf8();
                        let source = base.map(|s| s + byte_idx);

                        if ch == '\t' {
                            let tab_start_pos = text.len();
                            tab_starts.insert(tab_start_pos);
                            let spaces = tab_expansion_width(col);
                            for _ in 0..spaces {
                                text.push(' ');
                                char_mappings.push(source);
                                char_styles.push(token_style.clone());
                            }
                            col += spaces;
                        } else {
                            text.push(ch);
                            char_mappings.push(source);
                            char_styles.push(token_style.clone());
                            col += 1;
                        }
                        byte_idx += ch_len;
                    }
                    self.token_idx += 1;
                }
                ViewTokenWireKind::Space => {
                    text.push(' ');
                    char_mappings.push(token.source_offset);
                    char_styles.push(token_style);
                    col += 1;
                    self.token_idx += 1;
                }
                ViewTokenWireKind::Newline => {
                    // Newline ends this line
                    text.push('\n');
                    char_mappings.push(token.source_offset);
                    char_styles.push(token_style);
                    ends_with_newline = true;

                    // Determine how the next line starts
                    self.next_line_start = if token.source_offset.is_some() {
                        LineStart::AfterSourceNewline
                    } else {
                        LineStart::AfterInjectedNewline
                    };
                    self.token_idx += 1;
                    break;
                }
                ViewTokenWireKind::Break => {
                    // Break is a synthetic line break from wrapping
                    text.push('\n');
                    char_mappings.push(None);
                    char_styles.push(None);
                    ends_with_newline = true;

                    self.next_line_start = LineStart::AfterBreak;
                    self.token_idx += 1;
                    break;
                }
            }
        }

        // Don't return empty lines at the end
        if text.is_empty() && self.token_idx >= self.tokens.len() {
            return None;
        }

        Some(ViewLine {
            text,
            char_mappings,
            char_styles,
            tab_starts,
            line_start,
            ends_with_newline,
        })
    }
}

/// Determine if a display line should show a line number
///
/// Rules:
/// - Wrapped continuation (line_start == AfterBreak): no line number
/// - Injected content (first char has source_offset: None): no line number
/// - Empty line at beginning or after source newline: yes line number
/// - Otherwise: show line number
pub fn should_show_line_number(line: &ViewLine) -> bool {
    // Wrapped continuations never show line numbers
    if line.line_start.is_continuation() {
        return false;
    }

    // Check if this line contains injected (non-source) content
    // An empty line is NOT injected if it's at the beginning or after a source newline
    if line.char_mappings.is_empty() {
        // Empty line - show line number if it's at beginning or after source newline
        // (not after injected newline or break)
        return matches!(
            line.line_start,
            LineStart::Beginning | LineStart::AfterSourceNewline
        );
    }

    let first_char_is_source = line
        .char_mappings
        .first()
        .map(|m| m.is_some())
        .unwrap_or(false);

    if !first_char_is_source {
        // Injected line (header, etc.) - no line number
        return false;
    }

    // Source content after a real line break - show line number
    true
}

// ============================================================================
// Layout: The computed display state for a view
// ============================================================================

use std::collections::BTreeMap;
use std::ops::Range;

/// The Layout represents the computed display state for a view.
///
/// This is **View state**, not Buffer state. Each split has its own Layout
/// computed from its view_transform (or base tokens if no transform).
///
/// The Layout provides:
/// - ViewLines for the current viewport region
/// - Bidirectional mapping between source bytes and view positions
/// - Scroll limit information
#[derive(Debug, Clone)]
pub struct Layout {
    /// Display lines for the current viewport region
    pub lines: Vec<ViewLine>,

    /// Source byte range this layout covers
    pub source_range: Range<usize>,

    /// Total view lines in entire document (estimated or exact)
    pub total_view_lines: usize,

    /// Total injected lines in entire document (from view transform)
    pub total_injected_lines: usize,

    /// Fast lookup: source byte → view line index
    byte_to_line: BTreeMap<usize, usize>,
}

impl Layout {
    /// Create a new Layout from ViewLines
    pub fn new(lines: Vec<ViewLine>, source_range: Range<usize>) -> Self {
        let mut byte_to_line = BTreeMap::new();

        // Build the byte→line index from char_mappings
        for (line_idx, line) in lines.iter().enumerate() {
            // Find the first source byte in this line
            if let Some(first_byte) = line.char_mappings.iter().find_map(|m| *m) {
                byte_to_line.insert(first_byte, line_idx);
            }
        }

        // Estimate total view lines (for now, just use what we have)
        let total_view_lines = lines.len();
        let total_injected_lines = lines.iter().filter(|l| !should_show_line_number(l)).count();

        Self {
            lines,
            source_range,
            total_view_lines,
            total_injected_lines,
            byte_to_line,
        }
    }

    /// Build a Layout from a token stream
    pub fn from_tokens(tokens: &[ViewTokenWire], source_range: Range<usize>) -> Self {
        let lines: Vec<ViewLine> = ViewLineIterator::new(tokens).collect();
        Self::new(lines, source_range)
    }

    /// Find the view position (line, column) for a source byte
    pub fn source_byte_to_view_position(&self, byte: usize) -> Option<(usize, usize)> {
        // Find the view line containing this byte
        if let Some((&_line_start_byte, &line_idx)) = self.byte_to_line.range(..=byte).last() {
            if line_idx < self.lines.len() {
                let line = &self.lines[line_idx];
                // Find the column within this line
                for (col, mapping) in line.char_mappings.iter().enumerate() {
                    if *mapping == Some(byte) {
                        return Some((line_idx, col));
                    }
                }
                // Byte is in this line's range but not at a character boundary
                // Return end of line
                return Some((line_idx, line.char_mappings.len()));
            }
        }
        None
    }

    /// Find the source byte for a view position (line, column)
    pub fn view_position_to_source_byte(&self, line_idx: usize, col: usize) -> Option<usize> {
        if line_idx >= self.lines.len() {
            return None;
        }
        let line = &self.lines[line_idx];
        if col < line.char_mappings.len() {
            line.char_mappings[col]
        } else if !line.char_mappings.is_empty() {
            // Past end of line, return last valid byte
            line.char_mappings.iter().rev().find_map(|m| *m)
        } else {
            None
        }
    }

    /// Get the source byte for the start of a view line
    pub fn get_source_byte_for_line(&self, line_idx: usize) -> Option<usize> {
        if line_idx >= self.lines.len() {
            return None;
        }
        self.lines[line_idx].char_mappings.iter().find_map(|m| *m)
    }

    /// Find the nearest view line for a source byte (for stabilization)
    pub fn find_nearest_view_line(&self, byte: usize) -> usize {
        if let Some((&_line_start_byte, &line_idx)) = self.byte_to_line.range(..=byte).last() {
            line_idx.min(self.lines.len().saturating_sub(1))
        } else {
            0
        }
    }

    /// Calculate the maximum top line for scrolling
    pub fn max_top_line(&self, viewport_height: usize) -> usize {
        self.lines.len().saturating_sub(viewport_height)
    }

    /// Check if there's content below the current layout
    pub fn has_content_below(&self, buffer_len: usize) -> bool {
        self.source_range.end < buffer_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_text_token(text: &str, source_offset: Option<usize>) -> ViewTokenWire {
        ViewTokenWire {
            kind: ViewTokenWireKind::Text(text.to_string()),
            source_offset,
            style: None,
        }
    }

    fn make_newline_token(source_offset: Option<usize>) -> ViewTokenWire {
        ViewTokenWire {
            kind: ViewTokenWireKind::Newline,
            source_offset,
            style: None,
        }
    }

    fn make_break_token() -> ViewTokenWire {
        ViewTokenWire {
            kind: ViewTokenWireKind::Break,
            source_offset: None,
            style: None,
        }
    }

    #[test]
    fn test_simple_source_lines() {
        let tokens = vec![
            make_text_token("Line 1", Some(0)),
            make_newline_token(Some(6)),
            make_text_token("Line 2", Some(7)),
            make_newline_token(Some(13)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens).collect();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].text, "Line 1\n");
        assert_eq!(lines[0].line_start, LineStart::Beginning);
        assert!(should_show_line_number(&lines[0]));

        assert_eq!(lines[1].text, "Line 2\n");
        assert_eq!(lines[1].line_start, LineStart::AfterSourceNewline);
        assert!(should_show_line_number(&lines[1]));
    }

    #[test]
    fn test_wrapped_continuation() {
        let tokens = vec![
            make_text_token("Line 1 start", Some(0)),
            make_break_token(), // Wrapped
            make_text_token("continued", Some(12)),
            make_newline_token(Some(21)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens).collect();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].line_start, LineStart::Beginning);
        assert!(should_show_line_number(&lines[0]));

        assert_eq!(lines[1].line_start, LineStart::AfterBreak);
        assert!(
            !should_show_line_number(&lines[1]),
            "Wrapped continuation should NOT show line number"
        );
    }

    #[test]
    fn test_injected_header_then_source() {
        // This is the bug scenario: header (injected) followed by source content
        let tokens = vec![
            // Injected header
            make_text_token("== HEADER ==", None),
            make_newline_token(None),
            // Source content
            make_text_token("Line 1", Some(0)),
            make_newline_token(Some(6)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens).collect();

        assert_eq!(lines.len(), 2);

        // Header line - no line number (injected content)
        assert_eq!(lines[0].text, "== HEADER ==\n");
        assert_eq!(lines[0].line_start, LineStart::Beginning);
        assert!(
            !should_show_line_number(&lines[0]),
            "Injected header should NOT show line number"
        );

        // Source line after header - SHOULD show line number
        assert_eq!(lines[1].text, "Line 1\n");
        assert_eq!(lines[1].line_start, LineStart::AfterInjectedNewline);
        assert!(
            should_show_line_number(&lines[1]),
            "BUG: Source line after injected header SHOULD show line number!\n\
             line_start={:?}, first_char_is_source={}",
            lines[1].line_start,
            lines[1]
                .char_mappings
                .first()
                .map(|m| m.is_some())
                .unwrap_or(false)
        );
    }

    #[test]
    fn test_mixed_scenario() {
        // Header -> Source Line 1 -> Source Line 2 (wrapped) -> Source Line 3
        let tokens = vec![
            // Injected header
            make_text_token("== Block 1 ==", None),
            make_newline_token(None),
            // Source line 1
            make_text_token("Line 1", Some(0)),
            make_newline_token(Some(6)),
            // Source line 2 (gets wrapped)
            make_text_token("Line 2 start", Some(7)),
            make_break_token(),
            make_text_token("wrapped", Some(19)),
            make_newline_token(Some(26)),
            // Source line 3
            make_text_token("Line 3", Some(27)),
            make_newline_token(Some(33)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens).collect();

        assert_eq!(lines.len(), 5);

        // Header - no line number
        assert!(!should_show_line_number(&lines[0]));

        // Line 1 - yes line number (source after header)
        assert!(should_show_line_number(&lines[1]));

        // Line 2 start - yes line number
        assert!(should_show_line_number(&lines[2]));

        // Line 2 wrapped - no line number (continuation)
        assert!(!should_show_line_number(&lines[3]));

        // Line 3 - yes line number
        assert!(should_show_line_number(&lines[4]));
    }
}
