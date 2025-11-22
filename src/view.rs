//! View stream representation for rendering
//!
//! This module defines a lightweight, source-anchored view stream that can be
//! transformed (e.g., by plugins) before layout. It keeps mappings back to
//! source offsets for hit-testing and cursor positioning.

use crate::overlay::OverlayFace;
use crate::state::EditorState;
use crate::virtual_text::VirtualTextPosition;
use ratatui::style::Style;

/// Kind of token in the view stream
#[derive(Debug, Clone, PartialEq)]
pub enum ViewTokenKind {
    /// Plain text slice
    Text(String),
    /// Newline in the source
    Newline,
    /// Whitespace (commonly used when transforming newlines to spaces)
    Space,
    /// Virtual text (injected, not in source)
    VirtualText {
        text: String,
        style: Style,
        position: VirtualTextPosition,
        priority: i32,
    },
    /// Style span start/end (source-anchored)
    StyleStart(Style),
    StyleEnd,
    /// Overlay span (for decorations)
    Overlay(OverlayFace),
}

/// A view token with source mapping
#[derive(Debug, Clone, PartialEq)]
pub struct ViewToken {
    /// Byte offset in source for this token, if any
    pub source_offset: Option<usize>,
    /// The token kind
    pub kind: ViewTokenKind,
}

/// A view stream for a viewport
#[derive(Debug, Clone)]
pub struct ViewStream {
    pub tokens: Vec<ViewToken>,
    /// Mapping from view token index to source offset (if present)
    pub source_map: Vec<Option<usize>>,
}

impl ViewStream {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            source_map: Vec::new(),
        }
    }

    pub fn push(&mut self, token: ViewToken) {
        self.source_map.push(token.source_offset);
        self.tokens.push(token);
    }
}

/// Build a base view stream for a viewport range (byte offsets)
/// This stream contains plain text and newline tokens only; overlays and virtual
/// text are not included here (they remain applied during rendering).
pub fn build_base_stream(
    state: &mut EditorState,
    start: usize,
    end: usize,
) -> ViewStream {
    let mut stream = ViewStream::new();

    if start >= end {
        return stream;
    }

    let text = state.get_text_range(start, end);

    let mut current_offset = start;
    let mut buffer = String::new();

    for ch in text.chars() {
        if ch == '\n' {
            if !buffer.is_empty() {
                stream.push(ViewToken {
                    source_offset: Some(current_offset - buffer.len()),
                    kind: ViewTokenKind::Text(buffer.clone()),
                });
                buffer.clear();
            }
            stream.push(ViewToken {
                source_offset: Some(current_offset),
                kind: ViewTokenKind::Newline,
            });
            current_offset += 1;
        } else {
            buffer.push(ch);
            current_offset += ch.len_utf8();
        }
    }

    if !buffer.is_empty() {
        stream.push(ViewToken {
            source_offset: Some(current_offset - buffer.len()),
            kind: ViewTokenKind::Text(buffer),
        });
    }

    stream
}
/// Standard tab width for terminal display (tab stops every 8 columns)
pub const TAB_WIDTH: usize = 8;

/// Expand a tab character to spaces based on current column position.
/// Returns the number of spaces needed to reach the next tab stop.
#[inline]
pub fn tab_expansion_width(col: usize) -> usize {
    TAB_WIDTH - (col % TAB_WIDTH)
}

/// Result of flattening tokens into view text
pub struct FlattenedView {
    /// The view text with tabs expanded to spaces
    pub text: String,
    /// Mapping from view position to source byte offset
    pub mapping: Vec<Option<usize>>,
    /// Set of view positions that are the START of a tab expansion
    /// Used for rendering tab indicators and correct cursor positioning
    pub tab_starts: std::collections::HashSet<usize>,
}

/// Build a view string and source mapping from a wire token list
///
/// Tab characters are expanded to spaces based on tab stops (every 8 columns).
/// All expanded spaces map back to the original tab's source position.
/// Returns a FlattenedView with text, mapping, and tab start positions.
pub fn flatten_tokens(tokens: &[crate::plugin_api::ViewTokenWire]) -> FlattenedView {
    let mut view_text = String::new();
    let mut mapping: Vec<Option<usize>> = Vec::new();
    let mut tab_starts = std::collections::HashSet::new();
    let mut col: usize = 0; // Current column position, reset on newlines

    for token in tokens {
        match &token.kind {
            crate::plugin_api::ViewTokenWireKind::Text(t) => {
                let base = token.source_offset;
                let mut byte_idx = 0;
                for ch in t.chars() {
                    let ch_len = ch.len_utf8();
                    let source = base.map(|s| s + byte_idx);

                    if ch == '\t' {
                        // Record the start position of this tab expansion
                        let tab_start_pos = view_text.len();
                        tab_starts.insert(tab_start_pos);

                        // Expand tab to spaces up to next tab stop
                        let spaces = tab_expansion_width(col);
                        for _ in 0..spaces {
                            view_text.push(' ');
                            mapping.push(source);
                        }
                        col += spaces;
                    } else {
                        view_text.push(ch);
                        mapping.push(source);
                        col += 1;
                    }
                    byte_idx += ch_len;
                }
            }
            crate::plugin_api::ViewTokenWireKind::Newline => {
                view_text.push('\n');
                mapping.push(token.source_offset);
                col = 0; // Reset column on newline
            }
            crate::plugin_api::ViewTokenWireKind::Space => {
                view_text.push(' ');
                mapping.push(token.source_offset);
                col += 1;
            }
            crate::plugin_api::ViewTokenWireKind::Break => {
                view_text.push('\n');
                // Break tokens are synthetic, always have None mapping
                mapping.push(None);
                col = 0; // Reset column on break
            }
        }
    }

    FlattenedView {
        text: view_text,
        mapping,
        tab_starts,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tab_expansion_width() {
        // At column 0, tab expands to 8 spaces
        assert_eq!(tab_expansion_width(0), 8);
        // At column 1, need 7 spaces to reach column 8
        assert_eq!(tab_expansion_width(1), 7);
        // At column 3, need 5 spaces to reach column 8
        assert_eq!(tab_expansion_width(3), 5);
        // At column 7, need 1 space to reach column 8
        assert_eq!(tab_expansion_width(7), 1);
        // At column 8, need 8 spaces to reach column 16
        assert_eq!(tab_expansion_width(8), 8);
        // At column 9, need 7 spaces to reach column 16
        assert_eq!(tab_expansion_width(9), 7);
    }

    #[test]
    fn test_flatten_tokens_expands_tabs() {
        use crate::plugin_api::{ViewTokenWire, ViewTokenWireKind};

        // Create a token with a tab character
        let tokens = vec![ViewTokenWire {
            kind: ViewTokenWireKind::Text("a\tb".to_string()),
            source_offset: Some(0),
        }];

        let result = flatten_tokens(&tokens);

        // 'a' at col 0, then tab expands to 7 spaces (to reach col 8), then 'b'
        assert_eq!(result.text, "a       b"); // 'a' + 7 spaces + 'b'
        assert_eq!(result.text.len(), 9);

        // Mapping: 'a' -> 0, 7 spaces all -> 1 (the tab byte), 'b' -> 2
        assert_eq!(result.mapping.len(), 9);
        assert_eq!(result.mapping[0], Some(0)); // 'a'
        for i in 1..8 {
            assert_eq!(result.mapping[i], Some(1), "space {} should map to tab at byte 1", i);
        }
        assert_eq!(result.mapping[8], Some(2)); // 'b'

        // Tab start should be recorded at position 1 (first space of tab expansion)
        assert!(result.tab_starts.contains(&1));
    }

    #[test]
    fn test_flatten_tokens_tab_at_start() {
        use crate::plugin_api::{ViewTokenWire, ViewTokenWireKind};

        let tokens = vec![ViewTokenWire {
            kind: ViewTokenWireKind::Text("\tx".to_string()),
            source_offset: Some(0),
        }];

        let result = flatten_tokens(&tokens);

        // Tab at col 0 expands to 8 spaces, then 'x'
        assert_eq!(result.text, "        x"); // 8 spaces + 'x'
        assert_eq!(result.mapping.len(), 9);

        // All 8 spaces map to byte 0 (the tab)
        for i in 0..8 {
            assert_eq!(result.mapping[i], Some(0));
        }
        assert_eq!(result.mapping[8], Some(1)); // 'x'

        // Tab start should be at position 0
        assert!(result.tab_starts.contains(&0));
    }

    #[test]
    fn test_flatten_tokens_multiple_tabs() {
        use crate::plugin_api::{ViewTokenWire, ViewTokenWireKind};

        let tokens = vec![ViewTokenWire {
            kind: ViewTokenWireKind::Text("\t\t".to_string()),
            source_offset: Some(0),
        }];

        let result = flatten_tokens(&tokens);

        // First tab at col 0 -> 8 spaces, second tab at col 8 -> 8 spaces
        assert_eq!(result.text, "                "); // 16 spaces
        assert_eq!(result.mapping.len(), 16);

        // First 8 spaces map to byte 0, next 8 spaces map to byte 1
        for i in 0..8 {
            assert_eq!(result.mapping[i], Some(0));
        }
        for i in 8..16 {
            assert_eq!(result.mapping[i], Some(1));
        }

        // Tab starts at positions 0 and 8
        assert!(result.tab_starts.contains(&0));
        assert!(result.tab_starts.contains(&8));
    }

    #[test]
    fn test_flatten_tokens_tab_after_newline() {
        use crate::plugin_api::{ViewTokenWire, ViewTokenWireKind};

        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text("abc".to_string()),
                source_offset: Some(0),
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(3),
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Text("\tx".to_string()),
                source_offset: Some(4),
            },
        ];

        let result = flatten_tokens(&tokens);

        // "abc\n" then tab at col 0 (new line!) expands to 8 spaces, then 'x'
        assert_eq!(result.text, "abc\n        x");

        // After newline, column resets to 0, so tab expands to full 8 spaces
        assert_eq!(result.mapping.len(), 13); // 3 + 1 + 8 + 1

        // Tab start at position 4 (after "abc\n")
        assert!(result.tab_starts.contains(&4));
    }
}
