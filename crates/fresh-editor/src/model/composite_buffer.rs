//! Composite buffer for displaying multiple source buffers in a single view
//!
//! A composite buffer synthesizes its view from multiple source buffers,
//! enabling side-by-side diff, unified diff, 3-way merge, and code review views
//! within a single tab.

use crate::model::event::BufferId;
use serde::{Deserialize, Serialize};
use std::ops::Range;

/// A buffer that composes content from multiple source buffers
#[derive(Debug, Clone)]
pub struct CompositeBuffer {
    /// Unique ID for this composite buffer
    pub id: BufferId,

    /// Display name (shown in tab bar)
    pub name: String,

    /// Layout mode for this composite
    pub layout: CompositeLayout,

    /// Source buffer configurations
    pub sources: Vec<SourcePane>,

    /// Line alignment map (for side-by-side diff)
    /// Maps display_line -> (left_source_line, right_source_line)
    pub alignment: LineAlignment,

    /// Which pane currently has focus (for input routing)
    pub active_pane: usize,

    /// Mode for keybindings
    pub mode: String,
}

impl CompositeBuffer {
    /// Create a new composite buffer
    pub fn new(
        id: BufferId,
        name: String,
        mode: String,
        layout: CompositeLayout,
        sources: Vec<SourcePane>,
    ) -> Self {
        let pane_count = sources.len();
        Self {
            id,
            name,
            mode,
            layout,
            sources,
            alignment: LineAlignment::empty(pane_count),
            active_pane: 0,
        }
    }

    /// Get the number of source panes
    pub fn pane_count(&self) -> usize {
        self.sources.len()
    }

    /// Get the source pane at the given index
    pub fn get_pane(&self, index: usize) -> Option<&SourcePane> {
        self.sources.get(index)
    }

    /// Get the currently focused pane
    pub fn focused_pane(&self) -> Option<&SourcePane> {
        self.sources.get(self.active_pane)
    }

    /// Switch focus to the next pane
    pub fn focus_next(&mut self) {
        if !self.sources.is_empty() {
            self.active_pane = (self.active_pane + 1) % self.sources.len();
        }
    }

    /// Switch focus to the previous pane
    pub fn focus_prev(&mut self) {
        if !self.sources.is_empty() {
            self.active_pane = (self.active_pane + self.sources.len() - 1) % self.sources.len();
        }
    }

    /// Set the line alignment
    pub fn set_alignment(&mut self, alignment: LineAlignment) {
        self.alignment = alignment;
    }

    /// Get the total number of display rows
    pub fn row_count(&self) -> usize {
        self.alignment.rows.len()
    }
}

/// How the composite buffer arranges its source panes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CompositeLayout {
    /// Side-by-side columns (for diff view)
    SideBySide {
        /// Width ratio for each pane (must sum to 1.0)
        ratios: Vec<f32>,
        /// Show separator between panes
        show_separator: bool,
    },
    /// Vertically stacked sections (for notebook cells)
    Stacked {
        /// Spacing between sections (in lines)
        spacing: u16,
    },
    /// Interleaved lines (for unified diff)
    Unified,
}

impl Default for CompositeLayout {
    fn default() -> Self {
        CompositeLayout::SideBySide {
            ratios: vec![0.5, 0.5],
            show_separator: true,
        }
    }
}

/// Configuration for a single source pane within the composite
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourcePane {
    /// ID of the source buffer
    pub buffer_id: BufferId,

    /// Human-readable label (e.g., "OLD", "NEW", "BASE")
    pub label: String,

    /// Whether this pane accepts edits
    pub editable: bool,

    /// Visual style for this pane
    pub style: PaneStyle,

    /// Byte range in source buffer to display (None = entire buffer)
    pub range: Option<Range<usize>>,
}

impl SourcePane {
    /// Create a new source pane
    pub fn new(buffer_id: BufferId, label: impl Into<String>, editable: bool) -> Self {
        Self {
            buffer_id,
            label: label.into(),
            editable,
            style: PaneStyle::default(),
            range: None,
        }
    }

    /// Set the visual style
    pub fn with_style(mut self, style: PaneStyle) -> Self {
        self.style = style;
        self
    }

    /// Set the byte range to display
    pub fn with_range(mut self, range: Range<usize>) -> Self {
        self.range = Some(range);
        self
    }
}

/// Visual styling for a pane
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PaneStyle {
    /// Background color for added lines (RGB)
    pub add_bg: Option<(u8, u8, u8)>,
    /// Background color for removed lines (RGB)
    pub remove_bg: Option<(u8, u8, u8)>,
    /// Background color for modified lines (RGB)
    pub modify_bg: Option<(u8, u8, u8)>,
    /// Gutter indicator style
    pub gutter_style: GutterStyle,
}

impl PaneStyle {
    /// Create a style for the "old" side of a diff
    pub fn old_diff() -> Self {
        Self {
            remove_bg: Some((80, 30, 30)),
            gutter_style: GutterStyle::Both,
            ..Default::default()
        }
    }

    /// Create a style for the "new" side of a diff
    pub fn new_diff() -> Self {
        Self {
            add_bg: Some((30, 80, 30)),
            modify_bg: Some((80, 80, 30)),
            gutter_style: GutterStyle::Both,
            ..Default::default()
        }
    }
}

/// Gutter display style
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum GutterStyle {
    /// Show line numbers
    #[default]
    LineNumbers,
    /// Show diff markers (+/-/~)
    DiffMarkers,
    /// Show both line numbers and markers
    Both,
    /// Hide gutter
    None,
}

// ============================================================================
// Line Alignment
// ============================================================================

/// Alignment information for side-by-side views
#[derive(Debug, Clone, Default)]
pub struct LineAlignment {
    /// Each entry maps a display row to source lines in each pane
    /// None means padding (blank line) for that pane
    pub rows: Vec<AlignedRow>,
}

impl LineAlignment {
    /// Create an empty alignment for the given number of panes
    pub fn empty(_pane_count: usize) -> Self {
        Self { rows: Vec::new() }
    }

    /// Create alignment from simple line-by-line mapping (no diff)
    /// Assumes both buffers have the same number of lines
    pub fn simple(line_count: usize, pane_count: usize) -> Self {
        let rows = (0..line_count)
            .map(|line| AlignedRow {
                pane_lines: (0..pane_count)
                    .map(|_| {
                        Some(SourceLineRef {
                            line,
                            byte_range: 0..0, // Will be filled in during render
                        })
                    })
                    .collect(),
                row_type: RowType::Context,
            })
            .collect();
        Self { rows }
    }

    /// Create alignment from diff hunks
    pub fn from_hunks(hunks: &[DiffHunk], old_line_count: usize, new_line_count: usize) -> Self {
        let mut rows = Vec::new();
        let mut old_line = 0usize;
        let mut new_line = 0usize;

        for hunk in hunks {
            // Add context lines before this hunk
            while old_line < hunk.old_start && new_line < hunk.new_start {
                rows.push(AlignedRow::context(old_line, new_line));
                old_line += 1;
                new_line += 1;
            }

            // Add hunk header
            rows.push(AlignedRow::hunk_header());

            // Process hunk lines
            let old_end = hunk.old_start + hunk.old_count;
            let new_end = hunk.new_start + hunk.new_count;

            // Use a simple alignment: pair lines where possible, then pad
            let old_hunk_lines = old_end - hunk.old_start;
            let new_hunk_lines = new_end - hunk.new_start;
            let max_lines = old_hunk_lines.max(new_hunk_lines);

            for i in 0..max_lines {
                let old_idx = if i < old_hunk_lines {
                    Some(hunk.old_start + i)
                } else {
                    None
                };
                let new_idx = if i < new_hunk_lines {
                    Some(hunk.new_start + i)
                } else {
                    None
                };

                let row_type = match (old_idx, new_idx) {
                    (Some(_), Some(_)) => RowType::Modification,
                    (Some(_), None) => RowType::Deletion,
                    (None, Some(_)) => RowType::Addition,
                    (None, None) => continue,
                };

                rows.push(AlignedRow {
                    pane_lines: vec![
                        old_idx.map(|l| SourceLineRef {
                            line: l,
                            byte_range: 0..0,
                        }),
                        new_idx.map(|l| SourceLineRef {
                            line: l,
                            byte_range: 0..0,
                        }),
                    ],
                    row_type,
                });
            }

            old_line = old_end;
            new_line = new_end;
        }

        // Add remaining context lines after last hunk
        while old_line < old_line_count && new_line < new_line_count {
            rows.push(AlignedRow::context(old_line, new_line));
            old_line += 1;
            new_line += 1;
        }

        // Handle trailing lines in either buffer
        while old_line < old_line_count {
            rows.push(AlignedRow {
                pane_lines: vec![
                    Some(SourceLineRef {
                        line: old_line,
                        byte_range: 0..0,
                    }),
                    None,
                ],
                row_type: RowType::Deletion,
            });
            old_line += 1;
        }
        while new_line < new_line_count {
            rows.push(AlignedRow {
                pane_lines: vec![
                    None,
                    Some(SourceLineRef {
                        line: new_line,
                        byte_range: 0..0,
                    }),
                ],
                row_type: RowType::Addition,
            });
            new_line += 1;
        }

        Self { rows }
    }

    /// Get the aligned row at the given display index
    pub fn get_row(&self, display_row: usize) -> Option<&AlignedRow> {
        self.rows.get(display_row)
    }

    /// Get the number of display rows
    pub fn row_count(&self) -> usize {
        self.rows.len()
    }

    /// Find the next hunk header row after the given row
    pub fn next_hunk_row(&self, after_row: usize) -> Option<usize> {
        self.rows
            .iter()
            .enumerate()
            .skip(after_row + 1)
            .find(|(_, row)| row.row_type == RowType::HunkHeader)
            .map(|(i, _)| i)
    }

    /// Find the previous hunk header row before the given row
    pub fn prev_hunk_row(&self, before_row: usize) -> Option<usize> {
        self.rows
            .iter()
            .enumerate()
            .take(before_row)
            .rev()
            .find(|(_, row)| row.row_type == RowType::HunkHeader)
            .map(|(i, _)| i)
    }
}

/// A single aligned row mapping display to source lines
#[derive(Debug, Clone)]
pub struct AlignedRow {
    /// Source line for each pane (None = padding)
    pub pane_lines: Vec<Option<SourceLineRef>>,
    /// Type of this row for styling
    pub row_type: RowType,
}

impl AlignedRow {
    /// Create a context row (both sides have content)
    pub fn context(old_line: usize, new_line: usize) -> Self {
        Self {
            pane_lines: vec![
                Some(SourceLineRef {
                    line: old_line,
                    byte_range: 0..0,
                }),
                Some(SourceLineRef {
                    line: new_line,
                    byte_range: 0..0,
                }),
            ],
            row_type: RowType::Context,
        }
    }

    /// Create a hunk header row
    pub fn hunk_header() -> Self {
        Self {
            pane_lines: vec![None, None],
            row_type: RowType::HunkHeader,
        }
    }

    /// Get the source line for a specific pane
    pub fn get_pane_line(&self, pane_index: usize) -> Option<&SourceLineRef> {
        self.pane_lines.get(pane_index).and_then(|opt| opt.as_ref())
    }

    /// Check if this row has content in the given pane
    pub fn has_content(&self, pane_index: usize) -> bool {
        self.pane_lines
            .get(pane_index)
            .map(|opt| opt.is_some())
            .unwrap_or(false)
    }
}

/// Reference to a line in a source buffer
#[derive(Debug, Clone)]
pub struct SourceLineRef {
    /// Line number in source buffer (0-indexed)
    pub line: usize,
    /// Byte range in source buffer (computed during render)
    pub byte_range: Range<usize>,
}

/// Type of an aligned row for styling
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RowType {
    /// Both sides have matching content
    Context,
    /// Line exists only in left/old (deletion)
    Deletion,
    /// Line exists only in right/new (addition)
    Addition,
    /// Line differs between sides
    Modification,
    /// Hunk separator/header
    HunkHeader,
}

/// A diff hunk describing a contiguous change
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiffHunk {
    /// Starting line in old buffer (0-indexed)
    pub old_start: usize,
    /// Number of lines in old buffer
    pub old_count: usize,
    /// Starting line in new buffer (0-indexed)
    pub new_start: usize,
    /// Number of lines in new buffer
    pub new_count: usize,
    /// Optional header text (function context)
    pub header: Option<String>,
}

impl DiffHunk {
    /// Create a new diff hunk
    pub fn new(old_start: usize, old_count: usize, new_start: usize, new_count: usize) -> Self {
        Self {
            old_start,
            old_count,
            new_start,
            new_count,
            header: None,
        }
    }

    /// Set the header text
    pub fn with_header(mut self, header: impl Into<String>) -> Self {
        self.header = Some(header.into());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_alignment_from_hunks() {
        // Test with a single hunk: old has 2 lines deleted, new has 3 lines added
        let hunks = vec![DiffHunk::new(2, 2, 2, 3)];
        let alignment = LineAlignment::from_hunks(&hunks, 5, 6);

        // Should have:
        // - 2 context rows (lines 0-1)
        // - 1 hunk header
        // - 3 hunk rows (max of 2 old, 3 new)
        // - 1 context row (old line 4, new line 5)
        assert!(alignment.rows.len() >= 7);

        // First two rows should be context
        assert_eq!(alignment.rows[0].row_type, RowType::Context);
        assert_eq!(alignment.rows[1].row_type, RowType::Context);

        // Third row should be hunk header
        assert_eq!(alignment.rows[2].row_type, RowType::HunkHeader);
    }

    #[test]
    fn test_composite_buffer_focus() {
        let sources = vec![
            SourcePane::new(BufferId(1), "OLD", false),
            SourcePane::new(BufferId(2), "NEW", true),
        ];
        let mut composite = CompositeBuffer::new(
            BufferId(0),
            "Test".to_string(),
            "diff-view".to_string(),
            CompositeLayout::default(),
            sources,
        );

        assert_eq!(composite.active_pane, 0);

        composite.focus_next();
        assert_eq!(composite.active_pane, 1);

        composite.focus_next();
        assert_eq!(composite.active_pane, 0); // Wraps around

        composite.focus_prev();
        assert_eq!(composite.active_pane, 1);
    }
}
