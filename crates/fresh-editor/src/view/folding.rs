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
    /// Line-start byte of the fold header
    pub header_byte: usize,
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

            let header_byte =
                indent_folding::find_line_start_byte(buffer, start_byte.saturating_sub(1));

            ranges.push(ResolvedFoldRange {
                header_line: start_line - 1,
                start_line,
                end_line,
                start_byte,
                end_byte,
                header_byte,
                placeholder: range.placeholder.clone(),
            });
        }

        ranges
    }

    /// Return a map of header_byte -> placeholder for collapsed folds.
    pub fn collapsed_header_bytes(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
    ) -> std::collections::BTreeMap<usize, Option<String>> {
        let mut map = std::collections::BTreeMap::new();
        for range in self.resolved_ranges(buffer, marker_list) {
            map.insert(range.header_byte, range.placeholder);
        }
        map
    }

    /// Remove the fold range whose header byte matches `target_header_byte`.
    /// Returns true if a fold was removed.
    pub fn remove_by_header_byte(
        &mut self,
        buffer: &Buffer,
        marker_list: &mut MarkerList,
        target_header_byte: usize,
    ) -> bool {
        let mut to_delete = Vec::new();

        self.ranges.retain(|range| {
            let Some(start_byte) = marker_list.get_position(range.start_marker) else {
                return true;
            };
            let current_header =
                indent_folding::find_line_start_byte(buffer, start_byte.saturating_sub(1));
            if current_header == target_header_byte {
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

    /// Find the byte offset of the start of the line containing `pos`.
    /// Scans backward for `\n` (or returns 0).
    pub fn find_line_start_byte(buffer: &Buffer, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }
        let mut p = pos.min(buffer.len()).saturating_sub(1);
        loop {
            match PatternIndentCalculator::byte_at(buffer, p) {
                Some(b'\n') => return p + 1,
                None => return 0,
                _ => {
                    if p == 0 {
                        return 0;
                    }
                    p -= 1;
                }
            }
        }
    }

    /// Measure leading indent of a line given as a byte slice (no trailing `\n`).
    fn slice_indent(line: &[u8], tab_size: usize) -> (usize, bool) {
        let mut indent = 0;
        let mut all_blank = true;
        for &b in line {
            match b {
                b' ' => indent += 1,
                b'\t' => {
                    if tab_size > 0 {
                        indent += tab_size - (indent % tab_size);
                    } else {
                        indent += 1;
                    }
                }
                b'\r' => {}
                _ => {
                    all_blank = false;
                    break;
                }
            }
        }
        (indent, all_blank)
    }

    /// Identify foldable lines in a raw byte slice by analysing indentation.
    ///
    /// Works without any line metadata, so it can be used on large files whose
    /// piece tree has not been scanned for line feeds.
    ///
    /// `max_lookahead` limits how many lines *ahead* of each candidate we scan
    /// to decide foldability.
    ///
    /// Returns an iterator of 0-based line indices (within the slice) that are
    /// foldable.
    pub fn foldable_lines_in_bytes(
        bytes: &[u8],
        tab_size: usize,
        max_lookahead: usize,
    ) -> Vec<usize> {
        // Split into lines (preserving empty trailing line if present).
        let lines: Vec<&[u8]> = bytes.split(|&b| b == b'\n').collect();
        let line_count = lines.len();
        let mut result = Vec::new();

        for i in 0..line_count {
            let (header_indent, header_blank) = slice_indent(lines[i], tab_size);
            if header_blank {
                continue;
            }

            // Find next non-blank line within lookahead.
            let limit = line_count.min(i + 1 + max_lookahead);
            let mut next = i + 1;
            while next < limit {
                let (_, blank) = slice_indent(lines[next], tab_size);
                if !blank {
                    break;
                }
                next += 1;
            }
            if next >= limit {
                continue;
            }

            let (next_indent, _) = slice_indent(lines[next], tab_size);
            if next_indent > header_indent {
                result.push(i);
            }
        }

        result
    }

    /// Byte-based fold-end search for a single header line.
    ///
    /// Reads up to `max_scan_bytes` forward from `header_byte` and determines
    /// whether the line at that offset is foldable (next non-blank line is more
    /// indented).  Returns `Some(end_byte)` where `end_byte` is the start of
    /// the last non-blank line still inside the fold, or `None`.
    pub fn indent_fold_end_byte(
        buffer: &Buffer,
        header_byte: usize,
        tab_size: usize,
        max_scan_bytes: usize,
    ) -> Option<usize> {
        let buf_len = buffer.len();
        let end = buf_len.min(header_byte.saturating_add(max_scan_bytes));
        let bytes = buffer.slice_bytes(header_byte..end);
        if bytes.is_empty() {
            return None;
        }

        let lines: Vec<&[u8]> = bytes.split(|&b| b == b'\n').collect();
        if lines.is_empty() {
            return None;
        }

        let (header_indent, header_blank) = slice_indent(lines[0], tab_size);
        if header_blank {
            return None;
        }

        // Find next non-blank line.
        let mut next = 1;
        while next < lines.len() {
            let (_, blank) = slice_indent(lines[next], tab_size);
            if !blank {
                break;
            }
            next += 1;
        }
        if next >= lines.len() {
            return None;
        }

        let (next_indent, _) = slice_indent(lines[next], tab_size);
        if next_indent <= header_indent {
            return None;
        }

        // Scan forward for fold boundary.
        let mut last_non_blank_line = next;
        let mut current = next + 1;
        while current < lines.len() {
            let (indent, blank) = slice_indent(lines[current], tab_size);
            if blank {
                current += 1;
                continue;
            }
            if indent <= header_indent {
                break;
            }
            last_non_blank_line = current;
            current += 1;
        }

        if last_non_blank_line < 1 {
            return None;
        }

        // Convert line index back to byte offset: sum lengths of lines 0..last_non_blank_line
        // (each line was separated by a `\n`).
        let mut byte_offset = 0;
        for i in 0..last_non_blank_line {
            byte_offset += lines[i].len() + 1; // +1 for the \n
        }
        Some(header_byte + byte_offset)
    }

    /// Find the byte offset of the start of the *next* line after `pos`.
    /// Scans forward for `\n` and returns the byte after it. If no `\n` is
    /// found, returns `buffer.len()`.
    pub fn find_next_line_start_byte(buffer: &Buffer, pos: usize) -> usize {
        let mut p = pos;
        let len = buffer.len();
        while p < len {
            match PatternIndentCalculator::byte_at(buffer, p) {
                Some(b'\n') => return p + 1,
                None => return len,
                _ => p += 1,
            }
        }
        len
    }

    /// Byte-range of a fold that contains `target_byte`.
    ///
    /// Walks backward (up to `max_upward_lines` lines) from the line
    /// containing `target_byte`, trying each candidate as a fold header via
    /// [`indent_fold_end_byte`].  When a fold is found whose hidden range
    /// reaches at least `target_byte`, returns `(header_byte, start_byte,
    /// end_byte)` where:
    ///
    /// * `header_byte` – first byte of the fold header line
    /// * `start_byte`  – first hidden byte (start of the line after the header)
    /// * `end_byte`    – one past the last hidden byte (start of the line
    ///   *after* the last hidden line, or `buffer.len()`)
    ///
    /// Returns `None` if no enclosing fold is found within the search limit.
    pub fn find_fold_range_at_byte(
        buffer: &Buffer,
        target_byte: usize,
        tab_size: usize,
        max_scan_bytes: usize,
        max_upward_lines: usize,
    ) -> Option<(usize, usize, usize)> {
        let mut header_byte = find_line_start_byte(buffer, target_byte);

        for _ in 0..=max_upward_lines {
            if let Some(fold_end_byte) =
                indent_fold_end_byte(buffer, header_byte, tab_size, max_scan_bytes)
            {
                if fold_end_byte >= target_byte {
                    let eb = find_next_line_start_byte(buffer, fold_end_byte);
                    let sb = find_next_line_start_byte(buffer, header_byte);
                    if sb < eb {
                        return Some((header_byte, sb, eb));
                    }
                }
            }
            if header_byte == 0 {
                break;
            }
            header_byte = find_line_start_byte(buffer, header_byte.saturating_sub(1));
        }

        None
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_slice_indent_spaces() {
            assert_eq!(slice_indent(b"    hello", 4), (4, false));
            assert_eq!(slice_indent(b"hello", 4), (0, false));
            assert_eq!(slice_indent(b"        deep", 4), (8, false));
        }

        #[test]
        fn test_slice_indent_tabs() {
            assert_eq!(slice_indent(b"\thello", 4), (4, false));
            assert_eq!(slice_indent(b"\t\thello", 4), (8, false));
            // Mixed: 2 spaces + tab (tab_size=4) → 2 + (4-2) = 4
            assert_eq!(slice_indent(b"  \thello", 4), (4, false));
        }

        #[test]
        fn test_slice_indent_blank() {
            assert_eq!(slice_indent(b"", 4), (0, true));
            assert_eq!(slice_indent(b"   ", 4), (3, true));
            assert_eq!(slice_indent(b"  \r", 4), (2, true));
        }

        #[test]
        fn test_foldable_lines_basic() {
            let text = b"fn main() {\n    println!();\n}\n";
            let foldable = foldable_lines_in_bytes(text, 4, 50);
            assert_eq!(foldable, vec![0]); // line 0 is foldable
        }

        #[test]
        fn test_foldable_lines_nested() {
            let text = b"fn main() {\n    if true {\n        x();\n    }\n}\n";
            let foldable = foldable_lines_in_bytes(text, 4, 50);
            assert_eq!(foldable, vec![0, 1]); // both fn and if are foldable
        }

        #[test]
        fn test_foldable_lines_not_foldable() {
            let text = b"line1\nline2\nline3\n";
            let foldable = foldable_lines_in_bytes(text, 4, 50);
            assert!(foldable.is_empty());
        }

        #[test]
        fn test_foldable_lines_blank_lines_skipped() {
            // Blank line between header and indented line should still be foldable
            let text = b"fn main() {\n\n    println!();\n}\n";
            let foldable = foldable_lines_in_bytes(text, 4, 50);
            assert_eq!(foldable, vec![0]);
        }

        #[test]
        fn test_foldable_lines_max_lookahead() {
            // With max_lookahead=1, a blank line between header and content means
            // the lookahead can't reach the indented line.
            let text = b"fn main() {\n\n\n    println!();\n}\n";
            let foldable_short = foldable_lines_in_bytes(text, 4, 1);
            assert!(foldable_short.is_empty());

            let foldable_long = foldable_lines_in_bytes(text, 4, 50);
            assert_eq!(foldable_long, vec![0]);
        }
    }
}
