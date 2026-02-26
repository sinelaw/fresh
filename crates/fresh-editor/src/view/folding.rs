//! Folding range infrastructure
//!
//! Provides a marker-based system for tracking collapsed folding ranges.
//! Fold ranges are stored as byte markers so they auto-adjust on edits.

use crate::model::buffer::Buffer;
use crate::model::marker::{MarkerId, MarkerList};

/// A collapsed fold range tracked by markers.
#[derive(Debug, Clone)]
pub struct FoldRange {
    /// Marker at the first hidden byte (start of line after header)
    start_marker: MarkerId,
    /// Marker at the end of the hidden range (start of line after fold end)
    end_marker: MarkerId,
    /// Optional placeholder text for the folded range
    placeholder: Option<String>,
}

/// A resolved fold range with computed line/byte info.
#[derive(Debug, Clone)]
pub struct ResolvedFoldRange {
    /// Header line number (the visible line that owns the fold)
    pub header_line: usize,
    /// First hidden line number (header_line + 1)
    pub start_line: usize,
    /// Last hidden line number (inclusive)
    pub end_line: usize,
    /// Start byte of hidden range
    pub start_byte: usize,
    /// End byte of hidden range (exclusive)
    pub end_byte: usize,
    /// Optional placeholder text
    pub placeholder: Option<String>,
}

/// Collapsed fold range represented by line numbers for persistence/cloning.
#[derive(Debug, Clone)]
pub struct CollapsedFoldLineRange {
    /// Header line number (visible line that owns the fold)
    pub header_line: usize,
    /// Last hidden line number (inclusive)
    pub end_line: usize,
    /// Optional placeholder text
    pub placeholder: Option<String>,
}

/// Manages collapsed fold ranges for a buffer.
#[derive(Debug, Clone)]
pub struct FoldManager {
    ranges: Vec<FoldRange>,
}

impl FoldManager {
    /// Create a new empty fold manager.
    pub fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    /// Returns true if there are no collapsed folds.
    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    /// Add a collapsed fold range.
    pub fn add(
        &mut self,
        marker_list: &mut MarkerList,
        start: usize,
        end: usize,
        placeholder: Option<String>,
    ) {
        if end <= start {
            return;
        }

        let start_marker = marker_list.create(start, true); // left affinity
        let end_marker = marker_list.create(end, false); // right affinity

        self.ranges.push(FoldRange {
            start_marker,
            end_marker,
            placeholder,
        });
    }

    /// Remove all fold ranges and their markers.
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        for range in &self.ranges {
            marker_list.delete(range.start_marker);
            marker_list.delete(range.end_marker);
        }
        self.ranges.clear();
    }

    /// Remove the fold range whose header line matches `header_line`.
    /// Returns true if a fold was removed.
    pub fn remove_by_header_line(
        &mut self,
        buffer: &Buffer,
        marker_list: &mut MarkerList,
        header_line: usize,
    ) -> bool {
        let mut to_delete = Vec::new();

        self.ranges.retain(|range| {
            let Some(start_byte) = marker_list.get_position(range.start_marker) else {
                return true;
            };
            let start_line = buffer.get_line_number(start_byte);
            if start_line == 0 {
                return true;
            }
            let current_header = start_line - 1;
            if current_header == header_line {
                to_delete.push((range.start_marker, range.end_marker));
                false
            } else {
                true
            }
        });

        for (start, end) in &to_delete {
            marker_list.delete(*start);
            marker_list.delete(*end);
        }

        !to_delete.is_empty()
    }

    /// Remove any fold that contains the given byte position.
    /// Returns true if a fold was removed.
    pub fn remove_if_contains_byte(&mut self, marker_list: &mut MarkerList, byte: usize) -> bool {
        let mut to_delete = Vec::new();

        self.ranges.retain(|range| {
            let Some(start_byte) = marker_list.get_position(range.start_marker) else {
                return true;
            };
            let Some(end_byte) = marker_list.get_position(range.end_marker) else {
                return true;
            };
            if start_byte <= byte && byte < end_byte {
                to_delete.push((range.start_marker, range.end_marker));
                false
            } else {
                true
            }
        });

        for (start, end) in &to_delete {
            marker_list.delete(*start);
            marker_list.delete(*end);
        }

        !to_delete.is_empty()
    }

    /// Resolve all fold ranges into line/byte ranges, filtering invalid entries.
    pub fn resolved_ranges(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
    ) -> Vec<ResolvedFoldRange> {
        let mut ranges = Vec::new();

        for range in &self.ranges {
            let Some(start_byte) = marker_list.get_position(range.start_marker) else {
                continue;
            };
            let Some(end_byte) = marker_list.get_position(range.end_marker) else {
                continue;
            };
            if end_byte <= start_byte {
                continue;
            }

            let start_line = buffer.get_line_number(start_byte);
            if start_line == 0 {
                continue;
            }
            let end_line = buffer.get_line_number(end_byte.saturating_sub(1));
            if end_line < start_line {
                continue;
            }

            ranges.push(ResolvedFoldRange {
                header_line: start_line - 1,
                start_line,
                end_line,
                start_byte,
                end_byte,
                placeholder: range.placeholder.clone(),
            });
        }

        ranges
    }

    /// Return a map of header line -> placeholder for collapsed folds.
    pub fn collapsed_headers(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
    ) -> std::collections::BTreeMap<usize, Option<String>> {
        let mut map = std::collections::BTreeMap::new();
        for range in self.resolved_ranges(buffer, marker_list) {
            map.insert(range.header_line, range.placeholder);
        }
        map
    }

    /// Return collapsed fold ranges as line-based data (for persistence/cloning).
    pub fn collapsed_line_ranges(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
    ) -> Vec<CollapsedFoldLineRange> {
        self.resolved_ranges(buffer, marker_list)
            .into_iter()
            .map(|range| CollapsedFoldLineRange {
                header_line: range.header_line,
                end_line: range.end_line,
                placeholder: range.placeholder,
            })
            .collect()
    }

    /// Count total hidden lines for folds with headers in the given range.
    pub fn hidden_line_count_in_range(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
        start_line: usize,
        end_line: usize,
    ) -> usize {
        let mut hidden = 0usize;
        for range in self.resolved_ranges(buffer, marker_list) {
            if range.header_line >= start_line && range.header_line <= end_line {
                hidden = hidden.saturating_add(range.end_line.saturating_sub(range.start_line) + 1);
            }
        }
        hidden
    }
}

impl Default for FoldManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Indent-based folding fallback for when LSP folding ranges are not available.
///
/// Computes foldable ranges by analyzing indentation levels, reusing the same
/// indent measurement logic as the auto-indent feature
/// ([`PatternIndentCalculator::count_leading_indent`]).
pub mod indent_folding {
    use crate::model::buffer::Buffer;
    use crate::primitives::indent_pattern::PatternIndentCalculator;

    /// Information about a line's indentation.
    struct LineIndent {
        indent: usize,
        is_blank: bool,
    }

    /// Measure the indent level of a given line, reusing the pattern-based
    /// indent calculator shared with auto-indent.
    fn line_indent(buffer: &Buffer, line: usize, tab_size: usize) -> Option<LineIndent> {
        let start = buffer.line_start_offset(line)?;
        let end = buffer
            .line_start_offset(line + 1)
            .unwrap_or_else(|| buffer.len());

        let is_blank = (start..end).all(|pos| {
            matches!(
                PatternIndentCalculator::byte_at(buffer, pos),
                Some(b' ' | b'\t' | b'\r' | b'\n') | None
            )
        });

        let indent = PatternIndentCalculator::count_leading_indent(buffer, start, end, tab_size);
        Some(LineIndent { indent, is_blank })
    }

    /// Compute the end line (inclusive) of an indent-based fold starting at
    /// `header_line`, or `None` if the line is not foldable.
    ///
    /// A line is foldable when the next non-blank line is more indented.
    /// The fold extends forward until a non-blank line at the header's indent
    /// level (or less) is found.  Trailing blank lines inside the fold are
    /// included up to (but not past) the last non-blank line that is still
    /// more indented than the header.
    pub fn indent_fold_end_line(
        buffer: &Buffer,
        header_line: usize,
        tab_size: usize,
    ) -> Option<usize> {
        let header = line_indent(buffer, header_line, tab_size)?;
        if header.is_blank {
            return None;
        }

        let line_count = buffer.line_count()?;

        // Find the next non-blank line after the header.
        let mut next = header_line + 1;
        while next < line_count {
            let li = line_indent(buffer, next, tab_size)?;
            if !li.is_blank {
                break;
            }
            next += 1;
        }
        if next >= line_count {
            return None;
        }

        let next_li = line_indent(buffer, next, tab_size)?;
        if next_li.indent <= header.indent {
            return None; // not more indented → not foldable
        }

        // Scan forward to find where the fold ends.
        let mut last_non_blank_in_fold = next;
        let mut current = next + 1;
        while current < line_count {
            let li = line_indent(buffer, current, tab_size)?;
            if li.is_blank {
                current += 1;
                continue;
            }
            if li.indent <= header.indent {
                break;
            }
            last_non_blank_in_fold = current;
            current += 1;
        }

        // Only foldable if we'd actually hide at least one line.
        if last_non_blank_in_fold > header_line {
            Some(last_non_blank_in_fold)
        } else {
            None
        }
    }
}
