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

use crate::primitives::ansi::AnsiParser;
use crate::primitives::display_width::char_width;
use crate::services::plugins::api::{ViewTokenStyle, ViewTokenWire, ViewTokenWireKind};
use std::collections::HashSet;

/// A display line built from tokens, preserving token-level information
#[derive(Debug, Clone)]
pub struct ViewLine {
    /// The display text for this line (tabs expanded to spaces, etc.)
    pub text: String,

    // === Per-CHARACTER mappings (indexed by char position in text) ===
    /// Source byte offset for each character
    /// Length == text.chars().count()
    pub char_source_bytes: Vec<Option<usize>>,
    /// Style for each character (from token styles)
    pub char_styles: Vec<Option<ViewTokenStyle>>,
    /// Visual column where each character starts
    pub char_visual_cols: Vec<usize>,

    // === Per-VISUAL-COLUMN mapping (indexed by visual column) ===
    /// Character index at each visual column (for O(1) mouse clicks)
    /// For double-width chars, consecutive visual columns map to the same char index
    /// Length == total visual width of line
    pub visual_to_char: Vec<usize>,

    /// Positions that are the start of a tab expansion
    pub tab_starts: HashSet<usize>,
    /// How this line started (what kind of token/boundary preceded it)
    pub line_start: LineStart,
    /// Whether this line ends with a newline character
    pub ends_with_newline: bool,
}

impl ViewLine {
    /// Get source byte at a given character index (O(1))
    #[inline]
    pub fn source_byte_at_char(&self, char_idx: usize) -> Option<usize> {
        self.char_source_bytes.get(char_idx).copied().flatten()
    }

    /// Get character index at a given visual column (O(1))
    #[inline]
    pub fn char_at_visual_col(&self, visual_col: usize) -> usize {
        self.visual_to_char
            .get(visual_col)
            .copied()
            .unwrap_or_else(|| self.char_source_bytes.len().saturating_sub(1))
    }

    /// Get source byte at a given visual column (O(1) for mouse clicks)
    #[inline]
    pub fn source_byte_at_visual_col(&self, visual_col: usize) -> Option<usize> {
        let char_idx = self.char_at_visual_col(visual_col);
        self.source_byte_at_char(char_idx)
    }

    /// Get the visual column for a character at the given index
    #[inline]
    pub fn visual_col_at_char(&self, char_idx: usize) -> usize {
        self.char_visual_cols.get(char_idx).copied().unwrap_or(0)
    }

    /// Total visual width of this line
    #[inline]
    pub fn visual_width(&self) -> usize {
        self.visual_to_char.len()
    }
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
    /// Whether to render in binary mode (unprintable chars shown as code points)
    binary_mode: bool,
    /// Whether to parse ANSI escape sequences (giving them zero visual width)
    ansi_aware: bool,
}

impl<'a> ViewLineIterator<'a> {
    pub fn new(tokens: &'a [ViewTokenWire]) -> Self {
        Self {
            tokens,
            token_idx: 0,
            next_line_start: LineStart::Beginning,
            binary_mode: false,
            ansi_aware: false,
        }
    }

    /// Create a new ViewLineIterator with binary mode enabled
    pub fn with_binary_mode(tokens: &'a [ViewTokenWire], binary: bool) -> Self {
        Self {
            tokens,
            token_idx: 0,
            next_line_start: LineStart::Beginning,
            binary_mode: binary,
            ansi_aware: false,
        }
    }

    /// Create a new ViewLineIterator with ANSI awareness enabled
    pub fn with_ansi_aware(tokens: &'a [ViewTokenWire], ansi_aware: bool) -> Self {
        Self {
            tokens,
            token_idx: 0,
            next_line_start: LineStart::Beginning,
            binary_mode: false,
            ansi_aware,
        }
    }

    /// Create a new ViewLineIterator with both binary mode and ANSI awareness configurable
    pub fn with_options(tokens: &'a [ViewTokenWire], binary_mode: bool, ansi_aware: bool) -> Self {
        Self {
            tokens,
            token_idx: 0,
            next_line_start: LineStart::Beginning,
            binary_mode,
            ansi_aware,
        }
    }
}

/// Check if a byte is an unprintable control character that should be rendered as <XX>
/// Returns true for control characters (0x00-0x1F, 0x7F) except tab and newline
fn is_unprintable_byte(b: u8) -> bool {
    // Only allow tab (0x09) and newline (0x0A) to render normally
    // Everything else in control range should be shown as <XX>
    if b == 0x09 || b == 0x0A {
        return false;
    }
    // Control characters (0x00-0x1F) including CR, VT, FF, ESC are unprintable
    if b < 0x20 {
        return true;
    }
    // DEL character (0x7F) is also unprintable
    if b == 0x7F {
        return true;
    }
    false
}

/// Format an unprintable byte as a code point string like "<00>"
fn format_unprintable_byte(b: u8) -> String {
    format!("<{:02X}>", b)
}

impl<'a> Iterator for ViewLineIterator<'a> {
    type Item = ViewLine;

    fn next(&mut self) -> Option<Self::Item> {
        if self.token_idx >= self.tokens.len() {
            return None;
        }

        let line_start = self.next_line_start;
        let mut text = String::new();

        // Per-character tracking (indexed by character position)
        let mut char_source_bytes: Vec<Option<usize>> = Vec::new();
        let mut char_styles: Vec<Option<ViewTokenStyle>> = Vec::new();
        let mut char_visual_cols: Vec<usize> = Vec::new();

        // Per-visual-column tracking (indexed by visual column)
        let mut visual_to_char: Vec<usize> = Vec::new();

        let mut tab_starts = HashSet::new();
        let mut col = 0usize; // Current visual column
        let mut ends_with_newline = false;

        // ANSI parser for tracking escape sequences (reuse existing implementation)
        let mut ansi_parser = if self.ansi_aware {
            Some(AnsiParser::new())
        } else {
            None
        };

        /// Helper to add a character with all its mappings
        macro_rules! add_char {
            ($ch:expr, $source:expr, $style:expr, $width:expr) => {{
                let char_idx = char_source_bytes.len();

                // Per-character data
                text.push($ch);
                char_source_bytes.push($source);
                char_styles.push($style);
                char_visual_cols.push(col);

                // Per-visual-column data (for O(1) mouse clicks)
                for _ in 0..$width {
                    visual_to_char.push(char_idx);
                }

                col += $width;
            }};
        }

        // Process tokens until we hit a line break
        while self.token_idx < self.tokens.len() {
            let token = &self.tokens[self.token_idx];
            let token_style = token.style.clone();

            match &token.kind {
                ViewTokenWireKind::Text(t) => {
                    let base = token.source_offset;
                    let t_bytes = t.as_bytes();
                    let mut byte_idx = 0;

                    while byte_idx < t_bytes.len() {
                        let b = t_bytes[byte_idx];
                        let source = base.map(|s| s + byte_idx);

                        // In binary mode, render unprintable bytes as code points
                        if self.binary_mode && is_unprintable_byte(b) {
                            let formatted = format_unprintable_byte(b);
                            for display_ch in formatted.chars() {
                                add_char!(display_ch, source, token_style.clone(), 1);
                            }
                            byte_idx += 1;
                            continue;
                        }

                        // Decode the character at this position
                        let ch = if b < 0x80 {
                            // ASCII character
                            byte_idx += 1;
                            b as char
                        } else {
                            // Multi-byte UTF-8 - decode carefully
                            let remaining = &t_bytes[byte_idx..];
                            match std::str::from_utf8(remaining) {
                                Ok(s) => {
                                    if let Some(ch) = s.chars().next() {
                                        byte_idx += ch.len_utf8();
                                        ch
                                    } else {
                                        byte_idx += 1;
                                        '\u{FFFD}'
                                    }
                                }
                                Err(e) => {
                                    // Invalid UTF-8 - in binary mode show as hex, otherwise replacement char
                                    if self.binary_mode {
                                        let formatted = format_unprintable_byte(b);
                                        for display_ch in formatted.chars() {
                                            add_char!(display_ch, source, token_style.clone(), 1);
                                        }
                                        byte_idx += 1;
                                        continue;
                                    } else {
                                        // Try to get valid portion, then skip the bad byte
                                        let valid_up_to = e.valid_up_to();
                                        if valid_up_to > 0 {
                                            if let Some(ch) =
                                                std::str::from_utf8(&remaining[..valid_up_to])
                                                    .ok()
                                                    .and_then(|s| s.chars().next())
                                            {
                                                byte_idx += ch.len_utf8();
                                                ch
                                            } else {
                                                byte_idx += 1;
                                                '\u{FFFD}'
                                            }
                                        } else {
                                            byte_idx += 1;
                                            '\u{FFFD}'
                                        }
                                    }
                                }
                            }
                        };

                        if ch == '\t' {
                            // Tab expands to spaces - record start position
                            let tab_start_pos = char_source_bytes.len();
                            tab_starts.insert(tab_start_pos);
                            let spaces = tab_expansion_width(col);

                            // Tab is ONE character that expands to multiple visual columns
                            let char_idx = char_source_bytes.len();
                            text.push(' '); // First space char
                            char_source_bytes.push(source);
                            char_styles.push(token_style.clone());
                            char_visual_cols.push(col);

                            // All visual columns of the tab map to the same char
                            for _ in 0..spaces {
                                visual_to_char.push(char_idx);
                            }
                            col += spaces;

                            // Push remaining spaces as separate display chars
                            // (text contains expanded spaces for rendering)
                            for _ in 1..spaces {
                                text.push(' ');
                                char_source_bytes.push(source);
                                char_styles.push(token_style.clone());
                                char_visual_cols
                                    .push(col - spaces + char_source_bytes.len() - char_idx);
                            }
                        } else {
                            // Handle ANSI escape sequences - give them width 0
                            let width = if let Some(ref mut parser) = ansi_parser {
                                // Use AnsiParser: parse_char returns None for escape chars
                                if parser.parse_char(ch).is_none() {
                                    0 // Part of escape sequence, zero width
                                } else {
                                    char_width(ch)
                                }
                            } else {
                                char_width(ch)
                            };
                            add_char!(ch, source, token_style.clone(), width);
                        }
                    }
                    self.token_idx += 1;
                }
                ViewTokenWireKind::Space => {
                    add_char!(' ', token.source_offset, token_style, 1);
                    self.token_idx += 1;
                }
                ViewTokenWireKind::Newline => {
                    // Newline ends this line - width 1 for the newline char
                    add_char!('\n', token.source_offset, token_style, 1);
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
                    add_char!('\n', None, None, 1);
                    ends_with_newline = true;

                    self.next_line_start = LineStart::AfterBreak;
                    self.token_idx += 1;
                    break;
                }
                ViewTokenWireKind::BinaryByte(b) => {
                    // Binary byte rendered as <XX> - all 4 chars map to same source byte
                    let formatted = format_unprintable_byte(*b);
                    for display_ch in formatted.chars() {
                        add_char!(display_ch, token.source_offset, token_style.clone(), 1);
                    }
                    self.token_idx += 1;
                }
            }
        }

        // Don't return empty lines at the end
        if text.is_empty() && self.token_idx >= self.tokens.len() {
            return None;
        }

        Some(ViewLine {
            text,
            char_source_bytes,
            char_styles,
            char_visual_cols,
            visual_to_char,
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
    if line.char_source_bytes.is_empty() {
        // Empty line - show line number if it's at beginning or after source newline
        // (not after injected newline or break)
        return matches!(
            line.line_start,
            LineStart::Beginning | LineStart::AfterSourceNewline
        );
    }

    let first_char_is_source = line
        .char_source_bytes
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

        // Build the byte→line index from char_source_bytes
        for (line_idx, line) in lines.iter().enumerate() {
            // Find the first source byte in this line
            if let Some(first_byte) = line.char_source_bytes.iter().find_map(|m| *m) {
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

    /// Find the view position (line, visual column) for a source byte
    pub fn source_byte_to_view_position(&self, byte: usize) -> Option<(usize, usize)> {
        // Find the view line containing this byte
        if let Some((&_line_start_byte, &line_idx)) = self.byte_to_line.range(..=byte).last() {
            if line_idx < self.lines.len() {
                let line = &self.lines[line_idx];
                // Find the character with this source byte, then get its visual column
                for (char_idx, mapping) in line.char_source_bytes.iter().enumerate() {
                    if *mapping == Some(byte) {
                        return Some((line_idx, line.visual_col_at_char(char_idx)));
                    }
                }
                // Byte is in this line's range but not at a character boundary
                // Return end of line (visual width)
                return Some((line_idx, line.visual_width()));
            }
        }
        None
    }

    /// Find the source byte for a view position (line, visual column)
    pub fn view_position_to_source_byte(&self, line_idx: usize, col: usize) -> Option<usize> {
        if line_idx >= self.lines.len() {
            return None;
        }
        let line = &self.lines[line_idx];
        if col < line.visual_width() {
            // Use O(1) lookup via visual_to_char -> char_source_bytes
            line.source_byte_at_visual_col(col)
        } else if !line.char_source_bytes.is_empty() {
            // Past end of line, return last valid byte
            line.char_source_bytes.iter().rev().find_map(|m| *m)
        } else {
            None
        }
    }

    /// Get the source byte for the start of a view line
    pub fn get_source_byte_for_line(&self, line_idx: usize) -> Option<usize> {
        if line_idx >= self.lines.len() {
            return None;
        }
        self.lines[line_idx]
            .char_source_bytes
            .iter()
            .find_map(|m| *m)
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
                .char_source_bytes
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

    #[test]
    fn test_is_unprintable_byte() {
        // Null byte is unprintable
        assert!(is_unprintable_byte(0x00));

        // Control characters 0x01-0x08 are unprintable
        assert!(is_unprintable_byte(0x01));
        assert!(is_unprintable_byte(0x02));
        assert!(is_unprintable_byte(0x08));

        // Tab (0x09) and LF (0x0A) are allowed
        assert!(!is_unprintable_byte(0x09)); // tab
        assert!(!is_unprintable_byte(0x0A)); // newline

        // VT (0x0B), FF (0x0C), CR (0x0D) are unprintable in binary mode
        assert!(is_unprintable_byte(0x0B)); // vertical tab
        assert!(is_unprintable_byte(0x0C)); // form feed
        assert!(is_unprintable_byte(0x0D)); // carriage return

        // 0x0E-0x1F are all unprintable (including ESC)
        assert!(is_unprintable_byte(0x0E));
        assert!(is_unprintable_byte(0x1A)); // SUB - this is in PNG headers
        assert!(is_unprintable_byte(0x1B)); // ESC
        assert!(is_unprintable_byte(0x1C));
        assert!(is_unprintable_byte(0x1F));

        // Printable ASCII (0x20-0x7E) is allowed
        assert!(!is_unprintable_byte(0x20)); // space
        assert!(!is_unprintable_byte(0x41)); // 'A'
        assert!(!is_unprintable_byte(0x7E)); // '~'

        // DEL (0x7F) is unprintable
        assert!(is_unprintable_byte(0x7F));

        // High bytes (0x80+) are allowed (could be UTF-8)
        assert!(!is_unprintable_byte(0x80));
        assert!(!is_unprintable_byte(0xFF));
    }

    #[test]
    fn test_format_unprintable_byte() {
        assert_eq!(format_unprintable_byte(0x00), "<00>");
        assert_eq!(format_unprintable_byte(0x01), "<01>");
        assert_eq!(format_unprintable_byte(0x1A), "<1A>");
        assert_eq!(format_unprintable_byte(0x7F), "<7F>");
        assert_eq!(format_unprintable_byte(0xFF), "<FF>");
    }

    #[test]
    fn test_binary_mode_renders_control_chars() {
        // Text with null byte and control character
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text("Hello\x00World\x01End".to_string()),
                source_offset: Some(0),
                style: None,
            },
            make_newline_token(Some(15)),
        ];

        // Without binary mode - control chars would be rendered raw or as replacement
        let lines_normal: Vec<_> = ViewLineIterator::new(&tokens).collect();
        assert_eq!(lines_normal.len(), 1);
        // In normal mode, we don't format control chars specially

        // With binary mode - control chars should be formatted as <XX>
        let lines_binary: Vec<_> = ViewLineIterator::with_binary_mode(&tokens, true).collect();
        assert_eq!(lines_binary.len(), 1);
        assert!(
            lines_binary[0].text.contains("<00>"),
            "Binary mode should format null byte as <00>, got: {}",
            lines_binary[0].text
        );
        assert!(
            lines_binary[0].text.contains("<01>"),
            "Binary mode should format 0x01 as <01>, got: {}",
            lines_binary[0].text
        );
    }

    #[test]
    fn test_binary_mode_png_header() {
        // PNG-like content with SUB control char (0x1A)
        // Using valid UTF-8 string with embedded control character
        let png_like = "PNG\r\n\x1A\n";
        let tokens = vec![ViewTokenWire {
            kind: ViewTokenWireKind::Text(png_like.to_string()),
            source_offset: Some(0),
            style: None,
        }];

        let lines: Vec<_> = ViewLineIterator::with_binary_mode(&tokens, true).collect();

        // Should have rendered the 0x1A as <1A>
        let combined: String = lines.iter().map(|l| l.text.as_str()).collect();
        assert!(
            combined.contains("<1A>"),
            "PNG SUB byte (0x1A) should be rendered as <1A>, got: {:?}",
            combined
        );
    }

    #[test]
    fn test_binary_mode_preserves_printable_chars() {
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text("Normal text 123".to_string()),
                source_offset: Some(0),
                style: None,
            },
            make_newline_token(Some(15)),
        ];

        let lines: Vec<_> = ViewLineIterator::with_binary_mode(&tokens, true).collect();
        assert_eq!(lines.len(), 1);
        assert!(
            lines[0].text.contains("Normal text 123"),
            "Printable chars should be preserved in binary mode"
        );
    }

    #[test]
    fn test_double_width_visual_mappings() {
        // "你好" - two Chinese characters, each 3 bytes and 2 columns wide
        // Byte layout: 你=bytes 0-2, 好=bytes 3-5
        // Visual layout: 你 takes columns 0-1, 好 takes columns 2-3
        let tokens = vec![
            make_text_token("你好", Some(0)),
            make_newline_token(Some(6)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens).collect();
        assert_eq!(lines.len(), 1);

        // visual_to_char should have one entry per visual column
        // 你 = 2 columns, 好 = 2 columns, \n = 1 column = 5 total
        assert_eq!(
            lines[0].visual_width(),
            5,
            "Expected 5 visual columns (2 for 你 + 2 for 好 + 1 for newline), got {}",
            lines[0].visual_width()
        );

        // char_source_bytes should have one entry per character
        // 3 characters: 你, 好, \n
        assert_eq!(
            lines[0].char_source_bytes.len(),
            3,
            "Expected 3 char entries (你, 好, newline), got {}",
            lines[0].char_source_bytes.len()
        );

        // Both columns of 你 should map to byte 0 via O(1) lookup
        assert_eq!(
            lines[0].source_byte_at_visual_col(0),
            Some(0),
            "Column 0 should map to byte 0"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(1),
            Some(0),
            "Column 1 should map to byte 0"
        );

        // Both columns of 好 should map to byte 3
        assert_eq!(
            lines[0].source_byte_at_visual_col(2),
            Some(3),
            "Column 2 should map to byte 3"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(3),
            Some(3),
            "Column 3 should map to byte 3"
        );

        // Newline maps to byte 6
        assert_eq!(
            lines[0].source_byte_at_visual_col(4),
            Some(6),
            "Column 4 (newline) should map to byte 6"
        );
    }

    #[test]
    fn test_mixed_width_visual_mappings() {
        // "a你b" - ASCII, Chinese (2 cols), ASCII
        // Byte layout: a=0, 你=1-3, b=4
        // Visual columns: a=0, 你=1-2, b=3
        let tokens = vec![
            make_text_token("a你b", Some(0)),
            make_newline_token(Some(5)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens).collect();
        assert_eq!(lines.len(), 1);

        // a=1 col, 你=2 cols, b=1 col, \n=1 col = 5 total visual width
        assert_eq!(
            lines[0].visual_width(),
            5,
            "Expected 5 visual columns, got {}",
            lines[0].visual_width()
        );

        // 4 characters: a, 你, b, \n
        assert_eq!(
            lines[0].char_source_bytes.len(),
            4,
            "Expected 4 char entries, got {}",
            lines[0].char_source_bytes.len()
        );

        // Test O(1) visual column to byte lookup
        assert_eq!(
            lines[0].source_byte_at_visual_col(0),
            Some(0),
            "Column 0 (a) should map to byte 0"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(1),
            Some(1),
            "Column 1 (你 col 1) should map to byte 1"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(2),
            Some(1),
            "Column 2 (你 col 2) should map to byte 1"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(3),
            Some(4),
            "Column 3 (b) should map to byte 4"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(4),
            Some(5),
            "Column 4 (newline) should map to byte 5"
        );
    }
}
