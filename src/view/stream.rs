//! View stream representation for rendering
//!
//! This module defines a lightweight, source-anchored view stream that can be
//! transformed (e.g., by plugins) before layout. It keeps mappings back to
//! source offsets for hit-testing and cursor positioning.

use crate::state::EditorState;
use crate::view::overlay::OverlayFace;
use crate::view::virtual_text::VirtualTextPosition;
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
#[derive(Debug, Clone, Default)]
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
pub fn build_base_stream(state: &mut EditorState, start: usize, end: usize) -> ViewStream {
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
