//! Shared data carriers used by the orchestration layer.
//!
//! These two structs are the *only* "shared mega-structs" that survive the
//! refactor. They are produced by builders in `overlays.rs` and consumed by
//! the three render orchestrators (`render_line`, `render_buffer`,
//! `render_composite`). Nothing outside `orchestration/` imports them.

use super::super::folding::FoldIndicator;
use crate::primitives::highlighter::HighlightSpan;
use crate::view::margin::LineIndicator;
use crate::view::overlay::Overlay;
use crate::view::virtual_text::VirtualText;
use ratatui::style::Style;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Range;

/// Per-viewport selection state used by the render loop.
pub(crate) struct SelectionContext {
    pub ranges: Vec<Range<usize>>,
    pub block_rects: Vec<(usize, usize, usize, usize)>,
    pub cursor_positions: Vec<usize>,
    pub primary_cursor_position: usize,
}

/// Per-viewport decorations (overlays, diagnostics, indicators, virtual text).
pub(crate) struct DecorationContext {
    pub highlight_spans: Vec<HighlightSpan>,
    pub semantic_token_spans: Vec<HighlightSpan>,
    pub viewport_overlays: Vec<(Overlay, Range<usize>)>,
    /// Indices into `viewport_overlays` sorted by `range.start` (ascending).
    /// Used by the per-cell sweep in `render_view_lines` to advance an
    /// active set without re-scanning the full overlay list each cell.
    pub overlay_position_index: Vec<usize>,
    pub virtual_text_lookup: HashMap<usize, Vec<VirtualText>>,
    /// Diagnostic lines indexed by line-start byte offset.
    pub diagnostic_lines: HashSet<usize>,
    /// Inline diagnostic text per line. Derived from viewport overlays;
    /// highest severity wins per line.
    pub diagnostic_inline_texts: HashMap<usize, (String, Style)>,
    /// Line indicators indexed by line-start byte offset.
    pub line_indicators: BTreeMap<usize, LineIndicator>,
    /// Fold indicators indexed by line-start byte offset.
    pub fold_indicators: BTreeMap<usize, FoldIndicator>,
}
