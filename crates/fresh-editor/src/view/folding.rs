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
    /// Header line text at the time this snapshot was taken (used by
    /// session restore to detect stale line numbers, issue #1568).
    pub header_text: Option<String>,
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
    ///
    /// Each entry captures the header line's text so session restore can
    /// detect external edits that shifted line numbers (issue #1568).
    pub fn collapsed_line_ranges(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
    ) -> Vec<CollapsedFoldLineRange> {
        self.resolved_ranges(buffer, marker_list)
            .into_iter()
            .map(|range| {
                let header_text = buffer.get_line(range.header_line).map(|bytes| {
                    String::from_utf8_lossy(&bytes)
                        .trim_end_matches('\n')
                        .trim_end_matches('\r')
                        .to_string()
                });
                CollapsedFoldLineRange {
                    header_line: range.header_line,
                    end_line: range.end_line,
                    placeholder: range.placeholder,
                    header_text,
                }
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

// ---------------------------------------------------------------------------
// LSP-provided foldable ranges, stored as markers so they auto-adjust on edits
// ---------------------------------------------------------------------------

/// One LSP fold range, tracked by byte markers that follow buffer edits.
#[derive(Debug, Clone)]
struct LspFoldEntry {
    /// Marker at the first byte of the fold's header line.
    /// Right affinity: text inserted at the line start pushes the marker down
    /// with the content, so line_number(marker) keeps pointing at the code
    /// that used to be at header_line.
    start_marker: MarkerId,
    /// Marker at the first byte of the fold's end line.
    /// Right affinity for the same reason as start_marker.
    end_marker: MarkerId,
    /// Optional kind forwarded from the LSP response (comment, imports, region …).
    kind: Option<lsp_types::FoldingRangeKind>,
    /// Optional placeholder text shown when the fold is collapsed.
    collapsed_text: Option<String>,
}

/// Store for LSP-provided fold ranges. Ranges are tracked as byte markers on
/// the shared [`MarkerList`], so inserting or deleting lines around (or
/// inside) a fold re-aligns its header line number automatically — no manual
/// shifting required. Fixes the "fold indicator lag" from issue #1571.
#[derive(Debug, Clone, Default)]
pub struct LspFoldRanges {
    ranges: Vec<LspFoldEntry>,
}

impl LspFoldRanges {
    /// Create an empty store.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns true if no LSP fold ranges are currently tracked.
    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    /// Number of tracked ranges.
    pub fn len(&self) -> usize {
        self.ranges.len()
    }

    /// Drop every tracked range and release its markers.
    pub fn clear(&mut self, marker_list: &mut MarkerList) {
        for range in &self.ranges {
            marker_list.delete(range.start_marker);
            marker_list.delete(range.end_marker);
        }
        self.ranges.clear();
    }

    /// Replace the tracked set with fresh LSP-provided ranges (line-based).
    ///
    /// Each range's start/end lines are translated to byte offsets via
    /// [`Buffer::line_start_offset`]; ranges that can't be resolved (e.g. line
    /// numbers past EOF) are silently dropped.
    pub fn set_from_lsp(
        &mut self,
        buffer: &Buffer,
        marker_list: &mut MarkerList,
        ranges: impl IntoIterator<Item = lsp_types::FoldingRange>,
    ) {
        self.clear(marker_list);
        for r in ranges {
            let Some(start_byte) = buffer.line_start_offset(r.start_line as usize) else {
                continue;
            };
            let Some(end_byte) = buffer.line_start_offset(r.end_line as usize) else {
                continue;
            };
            // Right affinity: text inserted at the line start pushes the marker
            // down with the content (so it keeps pointing at the same *code*,
            // not the same *byte offset*).
            let start_marker = marker_list.create(start_byte, false);
            let end_marker = marker_list.create(end_byte, false);
            self.ranges.push(LspFoldEntry {
                start_marker,
                end_marker,
                kind: r.kind,
                collapsed_text: r.collapsed_text,
            });
        }
    }

    /// Resolve to the current line-based LSP-style ranges (post-edit).
    ///
    /// Ranges whose markers have been invalidated (e.g. the header line was
    /// deleted out from under them such that end comes before start) are
    /// filtered out.
    pub fn resolved(
        &self,
        buffer: &Buffer,
        marker_list: &MarkerList,
    ) -> Vec<lsp_types::FoldingRange> {
        self.ranges
            .iter()
            .filter_map(|r| {
                let start_byte = marker_list.get_position(r.start_marker)?;
                let end_byte = marker_list.get_position(r.end_marker)?;
                let start_line = buffer.get_line_number(start_byte);
                let end_line = buffer.get_line_number(end_byte);
                if end_line <= start_line {
                    return None;
                }
                Some(lsp_types::FoldingRange {
                    start_line: start_line as u32,
                    end_line: end_line as u32,
                    start_character: None,
                    end_character: None,
                    kind: r.kind.clone(),
                    collapsed_text: r.collapsed_text.clone(),
                })
            })
            .collect()
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

    /// Find the exclusive byte offset just past the line containing `pos`
    /// (i.e. one byte past its terminating `\n`, or the buffer length if the
    /// line has no trailing newline). Scans forward for `\n`.
    pub fn find_line_end_byte(buffer: &Buffer, pos: usize) -> usize {
        let buf_len = buffer.len();
        let mut p = pos;
        while p < buf_len {
            match PatternIndentCalculator::byte_at(buffer, p) {
                Some(b'\n') => return p + 1,
                None => return buf_len,
                _ => p += 1,
            }
        }
        buf_len
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

    /// Check if the first line in the given slice is foldable.
    /// Uses subsequent lines in the slice for lookahead.
    pub fn is_line_foldable_in_bytes(lines: &[&[u8]], tab_size: usize) -> bool {
        if lines.is_empty() {
            return false;
        }

        let (header_indent, header_blank) = slice_indent(lines[0], tab_size);
        if header_blank {
            return false;
        }

        // Find next non-blank line within the provided lines.
        let mut next = 1;
        while next < lines.len() {
            let (_, blank) = slice_indent(lines[next], tab_size);
            if !blank {
                break;
            }
            next += 1;
        }

        if next >= lines.len() {
            return false;
        }

        let (next_indent, _) = slice_indent(lines[next], tab_size);
        next_indent > header_indent
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
        for line in &lines[..last_non_blank_line] {
            byte_offset += line.len() + 1; // +1 for the \n
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
        fn test_is_line_foldable_basic() {
            let lines: Vec<&[u8]> = vec![b"fn main() {", b"    println!();", b"}"];
            assert!(is_line_foldable_in_bytes(&lines, 4));
        }

        #[test]
        fn test_is_line_foldable_not_foldable() {
            let lines: Vec<&[u8]> = vec![b"line1", b"line2", b"line3"];
            assert!(!is_line_foldable_in_bytes(&lines, 4));
        }

        #[test]
        fn test_is_line_foldable_blank_lines_skipped() {
            let lines: Vec<&[u8]> = vec![b"fn main() {", b"", b"    println!();", b"}"];
            assert!(is_line_foldable_in_bytes(&lines, 4));
        }
    }
}
