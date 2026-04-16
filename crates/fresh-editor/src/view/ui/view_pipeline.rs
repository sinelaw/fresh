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
use crate::primitives::display_width::str_width;
use fresh_core::api::{ViewTokenStyle, ViewTokenWire, ViewTokenWireKind};
use std::collections::HashSet;
use std::ops::Range;
use unicode_segmentation::UnicodeSegmentation;

/// A display line built from tokens, preserving token-level information
#[derive(Debug, Clone)]
pub struct ViewLine {
    /// The display text for this line (tabs expanded to spaces, etc.)
    pub text: String,

    /// Absolute source byte offset of the start of this line (if it has one)
    pub source_start_byte: Option<usize>,

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
    /// Tab width for rendering (number of spaces per tab)
    tab_size: usize,
    /// Whether the token stream covers the end of the buffer.
    /// When true, a trailing empty line is emitted after a final source newline
    /// (representing the empty line after a file's trailing '\n').
    at_buffer_end: bool,
    /// Sorted, non-overlapping source-byte ranges whose tokens should be
    /// skipped at the source level (collapsed folds). Empty slice disables
    /// skipping. Set via [`ViewLineIterator::with_fold_skip`].
    fold_skip: &'a [Range<usize>],
    /// Advances monotonically through `fold_skip` as token source offsets
    /// advance. Lets the per-token skip check run in O(1) amortised.
    fold_cursor: usize,
}

impl<'a> ViewLineIterator<'a> {
    /// Create a new ViewLineIterator with all options
    ///
    /// - `tokens`: The token stream to convert to display lines
    /// - `binary_mode`: Whether to render unprintable chars as code points
    /// - `ansi_aware`: Whether to parse ANSI escape sequences (giving them zero visual width)
    /// - `tab_size`: Tab width for rendering (number of spaces per tab, should be > 0)
    /// - `at_buffer_end`: Whether the token stream covers the end of the buffer.
    ///   When true, a trailing empty line is emitted after a final source newline.
    ///
    /// Note: If tab_size is 0, it will be treated as 4 (the default) to prevent division by zero.
    /// This is a defensive measure to handle invalid configuration gracefully.
    pub fn new(
        tokens: &'a [ViewTokenWire],
        binary_mode: bool,
        ansi_aware: bool,
        tab_size: usize,
        at_buffer_end: bool,
    ) -> Self {
        // Defensive: treat 0 as 4 (default) to prevent division by zero in tab_expansion_width
        // This can happen if invalid config (tab_size: 0) is loaded
        let tab_size = if tab_size == 0 { 4 } else { tab_size };
        Self {
            tokens,
            token_idx: 0,
            next_line_start: LineStart::Beginning,
            binary_mode,
            ansi_aware,
            tab_size,
            at_buffer_end,
            fold_skip: &[],
            fold_cursor: 0,
        }
    }

    /// Configure source-byte ranges to skip during iteration. `skip` must be
    /// sorted by `start` ascending and non-overlapping; caller is responsible
    /// (derived once per render from `FoldManager::resolved_ranges`). Tokens
    /// whose `source_offset` lies inside a skip range are consumed without
    /// contributing to a ViewLine, so folded content is never materialised.
    pub fn with_fold_skip(mut self, skip: &'a [Range<usize>]) -> Self {
        self.fold_skip = skip;
        self.fold_cursor = 0;
        self
    }

    /// Expand a tab to spaces based on current column and configured tab_size
    #[inline]
    fn tab_expansion_width(&self, col: usize) -> usize {
        self.tab_size - (col % self.tab_size)
    }

    /// Advance past tokens whose `source_offset` is inside a fold skip range.
    /// Monotonic in source offsets, so `fold_cursor` only moves forward.
    /// Tokens with `source_offset == None` (injected / virtual) are never
    /// skipped. Line-start transitions are NOT updated: the next emitted
    /// ViewLine's `line_start` continues to reflect the *last emitted*
    /// line's terminator (typically the fold header's source newline).
    #[inline]
    fn skip_folded_tokens(&mut self) {
        while self.token_idx < self.tokens.len() {
            let token = &self.tokens[self.token_idx];
            let Some(offset) = token.source_offset else {
                return;
            };
            while self.fold_cursor < self.fold_skip.len()
                && self.fold_skip[self.fold_cursor].end <= offset
            {
                self.fold_cursor += 1;
            }
            let in_skip = self
                .fold_skip
                .get(self.fold_cursor)
                .is_some_and(|r| r.start <= offset && offset < r.end);
            if !in_skip {
                return;
            }
            self.token_idx += 1;
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
        // Fold skip: advance past any tokens whose source bytes live inside
        // a collapsed fold range before inspecting the next visible token.
        self.skip_folded_tokens();

        if self.token_idx >= self.tokens.len() {
            // All tokens consumed.  If the previous line ended with a source
            // newline there is one more real (empty) document line to emit —
            // e.g. the empty line after a file's trailing '\n'.  Produce it
            // exactly once, then stop.  Only do this when the tokens cover
            // the actual end of the buffer (not a viewport slice).
            if self.at_buffer_end && matches!(self.next_line_start, LineStart::AfterSourceNewline) {
                // Flip to Beginning so the *next* call returns None.
                self.next_line_start = LineStart::Beginning;
                let last_source_byte = self.tokens.last().and_then(|t| t.source_offset);
                return Some(ViewLine {
                    text: String::new(),
                    source_start_byte: last_source_byte.map(|s| s + 1),
                    char_source_bytes: vec![],
                    char_styles: vec![],
                    char_visual_cols: vec![],
                    visual_to_char: vec![],
                    tab_starts: HashSet::new(),
                    line_start: LineStart::AfterSourceNewline,
                    ends_with_newline: false,
                });
            }
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

                // Per-visual-column data (for O(1) mouse clicks).
                // Note: $width is 0 for zero-width codepoints (combining
                // marks, ZWJ, continuation codepoints within a grapheme
                // cluster) — we deliberately emit no visual_to_char
                // entries for them.
                #[allow(clippy::reversed_empty_ranges)]
                for _ in 0..$width {
                    visual_to_char.push(char_idx);
                }

                col += $width;
            }};
        }

        // Process tokens until we hit a line break
        while self.token_idx < self.tokens.len() {
            // Skip tokens that fall inside a collapsed fold before
            // touching the current line's accumulators.
            self.skip_folded_tokens();
            if self.token_idx >= self.tokens.len() {
                break;
            }
            let token = &self.tokens[self.token_idx];
            let token_style = token.style.clone();

            match &token.kind {
                ViewTokenWireKind::Text(t) => {
                    let base = token.source_offset;
                    let t_bytes = t.as_bytes();
                    let mut byte_idx = 0;

                    while byte_idx < t_bytes.len() {
                        let b = t_bytes[byte_idx];

                        // In binary mode, render unprintable bytes as <XX> code points.
                        // These are never part of a grapheme cluster.
                        if self.binary_mode && is_unprintable_byte(b) {
                            let source = base.map(|s| s + byte_idx);
                            let formatted = format_unprintable_byte(b);
                            for display_ch in formatted.chars() {
                                add_char!(display_ch, source, token_style.clone(), 1);
                            }
                            byte_idx += 1;
                            continue;
                        }

                        // Decode the largest valid UTF-8 slice starting here so we can
                        // segment it into grapheme clusters. Any invalid byte is
                        // handled as a single-byte replacement char and we resume
                        // decoding afterwards.
                        let remaining = &t_bytes[byte_idx..];
                        let valid = match std::str::from_utf8(remaining) {
                            Ok(s) => s,
                            Err(e) => {
                                let valid_up_to = e.valid_up_to();
                                if valid_up_to == 0 {
                                    let source = base.map(|s| s + byte_idx);
                                    if self.binary_mode {
                                        let formatted = format_unprintable_byte(b);
                                        for display_ch in formatted.chars() {
                                            add_char!(display_ch, source, token_style.clone(), 1);
                                        }
                                    } else {
                                        add_char!('\u{FFFD}', source, token_style.clone(), 1);
                                    }
                                    byte_idx += 1;
                                    continue;
                                } else {
                                    // SAFETY: `valid_up_to` is a char boundary.
                                    unsafe {
                                        std::str::from_utf8_unchecked(&remaining[..valid_up_to])
                                    }
                                }
                            }
                        };

                        // Canonical Unicode handling: iterate grapheme clusters, not
                        // codepoints. The width of a cluster is `str_width(cluster)` —
                        // `unicode-width` 0.2 correctly returns 2 for ZWJ family emoji,
                        // 1 for a base+combining sequence like "é", 2 for fullwidth
                        // letters, and so on. This is the same width ratatui computes
                        // when it re-segments the span, so every stage of the pipeline
                        // (wrap, column tracking, span placement) agrees on how many
                        // cells each cluster occupies.
                        //
                        // We still record per-codepoint entries in the char-indexed
                        // arrays (char_source_bytes / char_styles / char_visual_cols)
                        // so byte↔column mapping stays exact for LSP positions, mouse
                        // clicks, and cursor arithmetic. But `col` advances exactly
                        // once per grapheme: the first codepoint of a cluster carries
                        // the full width, the rest carry 0.
                        let mut segmented_bytes = 0usize;
                        for (g_byte_offset, grapheme) in valid.grapheme_indices(true) {
                            segmented_bytes = g_byte_offset + grapheme.len();

                            // In binary mode, any ASCII unprintable byte inside the
                            // decoded slice must still be rendered as `<XX>`. This
                            // covers graphemes consisting entirely of one unprintable
                            // byte (e.g. `\x1A`) and CRLF (`\r\n`) where only the
                            // `\r` half is unprintable — we split those out.
                            if self.binary_mode {
                                let bytes = grapheme.as_bytes();
                                let has_unprintable =
                                    bytes.iter().any(|&b| b < 0x80 && is_unprintable_byte(b));
                                if has_unprintable {
                                    let mut inner = 0usize;
                                    for ch in grapheme.chars() {
                                        let ch_len = ch.len_utf8();
                                        let src =
                                            base.map(|s| s + byte_idx + g_byte_offset + inner);
                                        let ch_byte = ch as u32;
                                        if ch_byte < 0x80 && is_unprintable_byte(ch_byte as u8) {
                                            let formatted = format_unprintable_byte(ch_byte as u8);
                                            for display_ch in formatted.chars() {
                                                add_char!(display_ch, src, token_style.clone(), 1);
                                            }
                                        } else {
                                            add_char!(ch, src, token_style.clone(), 1);
                                        }
                                        inner += ch_len;
                                    }
                                    continue;
                                }
                            }

                            // Tab: a single codepoint forming its own grapheme, expanded to spaces.
                            if grapheme == "\t" {
                                let source = base.map(|s| s + byte_idx + g_byte_offset);
                                let tab_start_pos = char_source_bytes.len();
                                tab_starts.insert(tab_start_pos);
                                let spaces = self.tab_expansion_width(col);

                                let char_idx = char_source_bytes.len();
                                text.push(' ');
                                char_source_bytes.push(source);
                                char_styles.push(token_style.clone());
                                char_visual_cols.push(col);

                                for _ in 0..spaces {
                                    visual_to_char.push(char_idx);
                                }
                                col += spaces;

                                for _ in 1..spaces {
                                    text.push(' ');
                                    char_source_bytes.push(source);
                                    char_styles.push(token_style.clone());
                                    char_visual_cols
                                        .push(col - spaces + char_source_bytes.len() - char_idx);
                                }
                                continue;
                            }

                            // ANSI escape sequences. Process char-by-char so the
                            // AnsiParser state machine keeps track of the escape,
                            // and keep them as width 0. In practice ESC never sits
                            // inside a grapheme with visible content, so treating
                            // a grapheme that starts with ESC as width-0 here is
                            // correct.
                            if let Some(ref mut parser) = ansi_parser {
                                let first_ch = grapheme.chars().next().unwrap_or('\0');
                                if parser.parse_char(first_ch).is_none() {
                                    for ch in grapheme.chars() {
                                        // All codepoints of an escape grapheme are width 0.
                                        let src = base.map(|s| s + byte_idx + g_byte_offset);
                                        // Keep the parser fed so state transitions work
                                        // even across a multi-codepoint escape (rare).
                                        if ch != first_ch {
                                            let _ = parser.parse_char(ch);
                                        }
                                        add_char!(ch, src, token_style.clone(), 0);
                                    }
                                    continue;
                                }
                            }

                            // Normal case: emit one display unit per grapheme.
                            // Width goes on the FIRST codepoint, the rest are 0.
                            let cluster_width = str_width(grapheme);
                            let mut first = true;
                            let mut inner_byte_offset = 0usize;
                            for ch in grapheme.chars() {
                                let source =
                                    base.map(|s| s + byte_idx + g_byte_offset + inner_byte_offset);
                                let w = if first {
                                    first = false;
                                    cluster_width
                                } else {
                                    0
                                };
                                add_char!(ch, source, token_style.clone(), w);
                                inner_byte_offset += ch.len_utf8();
                            }
                        }

                        byte_idx += segmented_bytes.max(1);
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

        // col's final value is intentionally unused (only needed during iteration)
        let _ = col;

        // If we consumed all remaining tokens without hitting a Newline or Break,
        // the content didn't end with a line terminator.  Reset next_line_start
        // so the trailing-empty-line logic (at the top of next()) doesn't
        // incorrectly fire on the subsequent call.  The `ends_with_newline` flag
        // tells us whether the loop exited via a Newline/Break (true) or by
        // exhausting all tokens (false).
        if !ends_with_newline && self.token_idx >= self.tokens.len() {
            self.next_line_start = LineStart::Beginning;
        }

        // Don't return empty injected/virtual lines at the end of the token
        // stream.  However, DO return a trailing empty line that follows a source
        // newline — it represents a real document line (e.g. after a file's
        // trailing '\n') and the cursor may sit on it — but only when
        // at_buffer_end is set (otherwise this is just a viewport slice).
        if text.is_empty()
            && self.token_idx >= self.tokens.len()
            && !(self.at_buffer_end && matches!(line_start, LineStart::AfterSourceNewline))
        {
            return None;
        }

        Some(ViewLine {
            text,
            source_start_byte: char_source_bytes.iter().find_map(|s| *s),
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
    pub fn from_tokens(
        tokens: &[ViewTokenWire],
        source_range: Range<usize>,
        tab_size: usize,
    ) -> Self {
        let lines: Vec<ViewLine> =
            ViewLineIterator::new(tokens, false, false, tab_size, false).collect();
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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();

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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();

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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();

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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();

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
        let lines_normal: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();
        assert_eq!(lines_normal.len(), 1);
        // In normal mode, we don't format control chars specially

        // With binary mode - control chars should be formatted as <XX>
        let lines_binary: Vec<_> = ViewLineIterator::new(&tokens, true, false, 4, false).collect();
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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, true, false, 4, false).collect();

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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, true, false, 4, false).collect();
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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();
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

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();
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

    // ==================== CRLF Mode Tests ====================

    /// Test that ViewLineIterator correctly maps char_source_bytes for CRLF content.
    /// In CRLF mode, the Newline token is emitted at the \r position, and \n is skipped.
    /// This test verifies that char_source_bytes correctly tracks source byte positions.
    #[test]
    fn test_crlf_char_source_bytes_single_line() {
        // Simulate CRLF content "abc\r\n" where:
        // - bytes: a=0, b=1, c=2, \r=3, \n=4
        // - Newline token at source_offset=3 (position of \r)
        let tokens = vec![
            make_text_token("abc", Some(0)),
            make_newline_token(Some(3)), // \r position in CRLF
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();
        assert_eq!(lines.len(), 1);

        // The ViewLine should have: 'a', 'b', 'c', '\n'
        assert_eq!(lines[0].text, "abc\n");

        // char_source_bytes should correctly map each display char to source bytes
        assert_eq!(
            lines[0].char_source_bytes.len(),
            4,
            "Expected 4 chars: a, b, c, newline"
        );
        assert_eq!(
            lines[0].char_source_bytes[0],
            Some(0),
            "char 'a' should map to byte 0"
        );
        assert_eq!(
            lines[0].char_source_bytes[1],
            Some(1),
            "char 'b' should map to byte 1"
        );
        assert_eq!(
            lines[0].char_source_bytes[2],
            Some(2),
            "char 'c' should map to byte 2"
        );
        assert_eq!(
            lines[0].char_source_bytes[3],
            Some(3),
            "newline should map to byte 3 (\\r position)"
        );
    }

    /// Test CRLF char_source_bytes across multiple lines.
    /// This is the critical test for the accumulating offset bug.
    #[test]
    fn test_crlf_char_source_bytes_multiple_lines() {
        // Simulate CRLF content "abc\r\ndef\r\nghi\r\n" where:
        // Line 1: a=0, b=1, c=2, \r=3, \n=4 (5 bytes)
        // Line 2: d=5, e=6, f=7, \r=8, \n=9 (5 bytes)
        // Line 3: g=10, h=11, i=12, \r=13, \n=14 (5 bytes)
        let tokens = vec![
            // Line 1
            make_text_token("abc", Some(0)),
            make_newline_token(Some(3)), // \r at byte 3
            // Line 2
            make_text_token("def", Some(5)),
            make_newline_token(Some(8)), // \r at byte 8
            // Line 3
            make_text_token("ghi", Some(10)),
            make_newline_token(Some(13)), // \r at byte 13
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();
        assert_eq!(lines.len(), 3);

        // Line 1 verification
        assert_eq!(lines[0].text, "abc\n");
        assert_eq!(
            lines[0].char_source_bytes,
            vec![Some(0), Some(1), Some(2), Some(3)],
            "Line 1 char_source_bytes mismatch"
        );

        // Line 2 verification - THIS IS WHERE THE BUG WOULD MANIFEST
        // If there's an off-by-one per line, line 2 might have wrong offsets
        assert_eq!(lines[1].text, "def\n");
        assert_eq!(
            lines[1].char_source_bytes,
            vec![Some(5), Some(6), Some(7), Some(8)],
            "Line 2 char_source_bytes mismatch - possible CRLF offset drift"
        );

        // Line 3 verification - error accumulates
        assert_eq!(lines[2].text, "ghi\n");
        assert_eq!(
            lines[2].char_source_bytes,
            vec![Some(10), Some(11), Some(12), Some(13)],
            "Line 3 char_source_bytes mismatch - CRLF offset drift accumulated"
        );
    }

    /// Test CRLF visual column to source byte mapping.
    /// Verifies source_byte_at_visual_col works correctly for CRLF content.
    #[test]
    fn test_crlf_visual_to_source_mapping() {
        // CRLF content "ab\r\ncd\r\n"
        // Line 1: a=0, b=1, \r=2, \n=3
        // Line 2: c=4, d=5, \r=6, \n=7
        let tokens = vec![
            make_text_token("ab", Some(0)),
            make_newline_token(Some(2)),
            make_text_token("cd", Some(4)),
            make_newline_token(Some(6)),
        ];

        let lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();

        // Line 1: visual columns 0,1 should map to bytes 0,1
        assert_eq!(
            lines[0].source_byte_at_visual_col(0),
            Some(0),
            "Line 1 col 0"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(1),
            Some(1),
            "Line 1 col 1"
        );
        assert_eq!(
            lines[0].source_byte_at_visual_col(2),
            Some(2),
            "Line 1 col 2 (newline)"
        );

        // Line 2: visual columns 0,1 should map to bytes 4,5
        assert_eq!(
            lines[1].source_byte_at_visual_col(0),
            Some(4),
            "Line 2 col 0"
        );
        assert_eq!(
            lines[1].source_byte_at_visual_col(1),
            Some(5),
            "Line 2 col 1"
        );
        assert_eq!(
            lines[1].source_byte_at_visual_col(2),
            Some(6),
            "Line 2 col 2 (newline)"
        );
    }
}
