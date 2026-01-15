//! Line-based diff algorithm for comparing saved vs current buffer content.
//!
//! This module provides a simple but robust diff algorithm that correctly handles
//! insertions, deletions, and modifications. It uses a longest common subsequence (LCS)
//! approach to identify which lines are unchanged, then marks the ranges that differ.

use std::ops::Range;

/// Type of change detected for a line range
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeType {
    /// Lines that were inserted (new lines not in saved)
    Inserted,
    /// Lines that were modified (same position, different content)
    Modified,
    /// Deletion marker (line after where content was deleted)
    Deleted,
}

/// A range of lines with a specific change type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineChange {
    /// The range of line indices in the current buffer
    pub range: Range<usize>,
    /// What type of change this represents
    pub change_type: ChangeType,
}

impl LineChange {
    pub fn new(range: Range<usize>, change_type: ChangeType) -> Self {
        Self { range, change_type }
    }
}

/// Result of comparing two text buffers line by line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineDiff {
    /// Whether the two buffers are identical.
    pub equal: bool,
    /// Line ranges in the "current" buffer that differ from "saved".
    /// These are the lines that should show modification indicators.
    pub changed_lines: Vec<Range<usize>>,
    /// Detailed changes with type information
    pub changes: Vec<LineChange>,
}

/// Compare two byte slices line by line and return which lines in `current` differ from `saved`.
///
/// This uses the classic LCS (Longest Common Subsequence) algorithm which is the
/// foundation of most diff tools including Unix `diff`. The algorithm:
/// 1. Find the longest common subsequence of lines between saved and current
/// 2. Lines in current not in the LCS are insertions/modifications
/// 3. Lines in saved not in the LCS represent deletions (marked at deletion point)
///
/// This correctly handles insertions, deletions, and modifications without
/// incorrectly marking shifted lines as changed.
pub fn diff_lines(saved: &[u8], current: &[u8]) -> LineDiff {
    let saved_lines: Vec<&[u8]> = saved.split(|&b| b == b'\n').collect();
    let current_lines: Vec<&[u8]> = current.split(|&b| b == b'\n').collect();

    // Quick check: if identical, return early
    if saved == current {
        return LineDiff {
            equal: true,
            changed_lines: vec![],
            changes: vec![],
        };
    }

    // Find LCS (longest common subsequence) of lines
    let lcs = longest_common_subsequence(&saved_lines, &current_lines);

    // Mark lines in current that are NOT part of the LCS as changed
    // Also mark deletion points where saved lines were removed
    let (changed_lines, changes) =
        find_changed_lines_with_deletions(&saved_lines, &current_lines, &lcs);

    LineDiff {
        equal: changed_lines.is_empty() && changes.is_empty(),
        changed_lines,
        changes,
    }
}

/// Represents a match between saved and current line indices
#[derive(Debug, Clone, Copy)]
struct LineMatch {
    saved_idx: usize,
    current_idx: usize,
}

/// Find the longest common subsequence of lines between saved and current.
/// Returns a list of LineMatch with both saved and current indices.
fn longest_common_subsequence(saved: &[&[u8]], current: &[&[u8]]) -> Vec<LineMatch> {
    let n = saved.len();
    let m = current.len();

    if n == 0 || m == 0 {
        return vec![];
    }

    // DP table for LCS length
    // dp[i][j] = length of LCS of saved[0..i] and current[0..j]
    let mut dp = vec![vec![0usize; m + 1]; n + 1];

    for i in 1..=n {
        for j in 1..=m {
            if saved[i - 1] == current[j - 1] {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = dp[i - 1][j].max(dp[i][j - 1]);
            }
        }
    }

    // Backtrack to find the actual LCS
    let mut lcs = Vec::new();
    let mut i = n;
    let mut j = m;

    while i > 0 && j > 0 {
        if saved[i - 1] == current[j - 1] {
            lcs.push(LineMatch {
                saved_idx: i - 1,
                current_idx: j - 1,
            });
            i -= 1;
            j -= 1;
        } else if dp[i - 1][j] > dp[i][j - 1] {
            i -= 1;
        } else {
            j -= 1;
        }
    }

    lcs.reverse();
    lcs
}

/// Given the LCS matches, find which lines in current are changed.
/// This includes:
/// - Lines in current that are not in the LCS (insertions/modifications)
/// - Deletion points where saved lines were removed (marked at the line after deletion)
///
/// Returns both the simple ranges (for backward compatibility) and typed changes.
fn find_changed_lines_with_deletions(
    saved: &[&[u8]],
    current: &[&[u8]],
    lcs: &[LineMatch],
) -> (Vec<Range<usize>>, Vec<LineChange>) {
    let mut matched_in_current: Vec<bool> = vec![false; current.len()];
    let mut matched_in_saved: Vec<bool> = vec![false; saved.len()];

    for m in lcs {
        matched_in_current[m.current_idx] = true;
        matched_in_saved[m.saved_idx] = true;
    }

    let mut ranges = Vec::new();
    let mut changes = Vec::new();

    // Determine change types by analyzing the LCS alignment
    // Build a map of which saved line each current line corresponds to
    let mut current_to_saved: Vec<Option<usize>> = vec![None; current.len()];
    for m in lcs {
        current_to_saved[m.current_idx] = Some(m.saved_idx);
    }

    // Find insertions/modifications: lines in current not in LCS
    let mut i = 0;
    while i < current.len() {
        if !matched_in_current[i] {
            let start = i;
            while i < current.len() && !matched_in_current[i] {
                i += 1;
            }
            let range = start..i;

            // Determine if this is an insertion or modification
            // It's a modification if there's a corresponding saved line at the same position
            // that was also not matched (i.e., both were changed)
            let change_type = classify_change(start, i, saved.len(), current.len(), lcs);
            changes.push(LineChange::new(range.clone(), change_type));
            ranges.push(range);
        } else {
            i += 1;
        }
    }

    // Find deletions: lines in saved not in LCS
    // Mark the deletion point in current (the line after where deletion occurred)
    let mut saved_idx = 0;
    let mut current_idx = 0;
    let mut lcs_idx = 0;

    while saved_idx < saved.len() {
        if lcs_idx < lcs.len() && lcs[lcs_idx].saved_idx == saved_idx {
            // This saved line is matched
            current_idx = lcs[lcs_idx].current_idx + 1;
            saved_idx += 1;
            lcs_idx += 1;
        } else {
            // This saved line was deleted - mark at current position
            let deletion_line = if current_idx < current.len() {
                current_idx
            } else if !current.is_empty() {
                current.len() - 1
            } else {
                0
            };
            let range = deletion_line..deletion_line + 1;
            changes.push(LineChange::new(range.clone(), ChangeType::Deleted));
            ranges.push(range);
            saved_idx += 1;
        }
    }

    // Sort changes by range start
    changes.sort_by_key(|c| c.range.start);

    // Merge ranges but keep changes separate (they have different types)
    let merged_ranges = merge_ranges(ranges);

    (merged_ranges, changes)
}

/// Classify a change as insertion or modification based on context
fn classify_change(
    start: usize,
    end: usize,
    saved_len: usize,
    current_len: usize,
    lcs: &[LineMatch],
) -> ChangeType {
    // If current is longer than saved, extra lines are insertions
    if current_len > saved_len {
        // Check if this range is beyond the original saved length
        // by looking at where matched lines are
        let last_matched_saved = lcs.iter().map(|m| m.saved_idx).max();
        let last_matched_current = lcs.iter().map(|m| m.current_idx).max();

        match (last_matched_saved, last_matched_current) {
            (Some(ls), Some(lc)) if start > lc && start >= ls => {
                // This is after all matched content - likely insertion
                return ChangeType::Inserted;
            }
            _ => {}
        }
    }

    // If there are matched lines before and after this range, it's likely an insertion
    let has_match_before = lcs.iter().any(|m| m.current_idx < start);
    let has_match_after = lcs.iter().any(|m| m.current_idx >= end);

    if has_match_before && has_match_after {
        // Lines in the middle between matched content - insertion
        ChangeType::Inserted
    } else if !has_match_before && has_match_after {
        // At the beginning before any matches
        if current_len > saved_len {
            ChangeType::Inserted
        } else {
            ChangeType::Modified
        }
    } else if has_match_before && !has_match_after {
        // At the end after all matches
        if current_len > saved_len {
            ChangeType::Inserted
        } else {
            ChangeType::Modified
        }
    } else {
        // No matches at all - whole file changed
        ChangeType::Modified
    }
}

/// Merge adjacent or overlapping ranges.
pub fn merge_ranges(ranges: Vec<Range<usize>>) -> Vec<Range<usize>> {
    if ranges.is_empty() {
        return ranges;
    }

    let mut sorted = ranges;
    sorted.sort_by_key(|r| r.start);

    let mut merged = Vec::new();
    let mut current = sorted[0].clone();

    for range in sorted.into_iter().skip(1) {
        if range.start <= current.end {
            current.end = current.end.max(range.end);
        } else {
            merged.push(current);
            current = range;
        }
    }
    merged.push(current);
    merged
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identical_content() {
        let content = b"line 1\nline 2\nline 3\n";
        let diff = diff_lines(content, content);
        assert!(diff.equal);
        assert!(diff.changed_lines.is_empty());
    }

    #[test]
    fn test_empty_files() {
        let diff = diff_lines(b"", b"");
        assert!(diff.equal);
        assert!(diff.changed_lines.is_empty());
    }

    #[test]
    fn test_single_line_modification() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"line 1\nmodified\nline 3\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        assert_eq!(diff.changed_lines, vec![1..2]);
    }

    #[test]
    fn test_insert_line_at_beginning() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"new line\nline 1\nline 2\nline 3\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Only the new line should be marked, not the shifted lines
        assert_eq!(diff.changed_lines, vec![0..1]);
    }

    #[test]
    fn test_insert_line_in_middle() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"line 1\nline 2\nnew line\nline 3\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Only the new line should be marked
        assert_eq!(diff.changed_lines, vec![2..3]);
    }

    #[test]
    fn test_insert_line_at_end() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"line 1\nline 2\nline 3\nnew line\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Only the new line should be marked
        assert_eq!(diff.changed_lines, vec![3..4]);
    }

    #[test]
    fn test_delete_line_from_beginning() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"line 2\nline 3\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Deletion at beginning - mark line 0 as the deletion point
        assert_eq!(diff.changed_lines, vec![0..1]);
    }

    #[test]
    fn test_delete_line_from_middle() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"line 1\nline 3\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Deletion after line 1 - mark line 1 as the deletion point
        assert_eq!(diff.changed_lines, vec![1..2]);
    }

    #[test]
    fn test_insert_newline_splits_line() {
        // This is the key test case: inserting Enter at end of line 2
        let saved = b"line 1\nline 2\nline 3\nline 4\nline 5\n";
        let current = b"line 1\nline 2\n\nline 3\nline 4\nline 5\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Only the new empty line should be marked (line index 2)
        // Lines 3, 4, 5 should NOT be marked even though they shifted
        assert_eq!(diff.changed_lines, vec![2..3]);
    }

    #[test]
    fn test_multiple_insertions() {
        let saved = b"a\nb\nc\n";
        let current = b"a\nx\nb\ny\nc\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Lines 1 (x) and 3 (y) are new
        assert_eq!(diff.changed_lines, vec![1..2, 3..4]);
    }

    #[test]
    fn test_multiple_deletions() {
        let saved = b"a\nx\nb\ny\nc\n";
        let current = b"a\nb\nc\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Deletions after 'a' and after 'b' - mark lines 1 and 2
        assert_eq!(diff.changed_lines, vec![1..3]);
    }

    #[test]
    fn test_replace_all_content() {
        let saved = b"old 1\nold 2\nold 3\n";
        let current = b"new 1\nnew 2\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // All lines in current are new
        assert_eq!(diff.changed_lines, vec![0..2]);
    }

    #[test]
    fn test_content_restored_via_paste() {
        // Simulates: cut "world", paste it back
        let saved = b"hello world\n";
        let current = b"hello world\n";
        let diff = diff_lines(saved, current);

        assert!(diff.equal);
        assert!(diff.changed_lines.is_empty());
    }

    #[test]
    fn test_interleaved_changes() {
        let saved = b"a\nb\nc\nd\ne\n";
        let current = b"a\nB\nc\nD\ne\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Lines 1 (B) and 3 (D) are modified
        assert_eq!(diff.changed_lines, vec![1..2, 3..4]);
    }

    #[test]
    fn test_merge_adjacent_ranges() {
        let ranges = vec![0..1, 1..2, 3..4];
        let merged = merge_ranges(ranges);
        assert_eq!(merged, vec![0..2, 3..4]);
    }

    #[test]
    fn test_merge_overlapping_ranges() {
        let ranges = vec![0..3, 2..5, 7..9];
        let merged = merge_ranges(ranges);
        assert_eq!(merged, vec![0..5, 7..9]);
    }

    #[test]
    fn test_delete_at_end() {
        let saved = b"line 1\nline 2\nline 3\n";
        let current = b"line 1\nline 2\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        // Deletion at end - mark last line as deletion point
        assert!(!diff.changed_lines.is_empty());
    }

    #[test]
    fn test_add_at_end_of_existing_line() {
        // Adding text to end of a line (not a newline)
        let saved = b"hello\n";
        let current = b"hello world\n";
        let diff = diff_lines(saved, current);

        assert!(!diff.equal);
        assert_eq!(diff.changed_lines, vec![0..1]);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    /// Generate a simple multi-line string
    fn multiline_string() -> impl Strategy<Value = Vec<u8>> {
        prop::collection::vec("[a-z ]{0,20}", 0..10).prop_map(|lines| {
            let joined = lines.join("\n");
            joined.into_bytes()
        })
    }

    proptest! {
        /// Identical content should always produce equal=true
        #[test]
        fn identical_content_is_equal(content in multiline_string()) {
            let diff = diff_lines(&content, &content);
            prop_assert!(diff.equal);
            prop_assert!(diff.changed_lines.is_empty());
        }

        /// Diff should be symmetric in terms of detecting changes
        /// (though the specific changed_lines may differ)
        #[test]
        fn diff_detects_any_difference(
            saved in multiline_string(),
            current in multiline_string()
        ) {
            let diff = diff_lines(&saved, &current);
            if saved == current {
                prop_assert!(diff.equal);
            } else {
                prop_assert!(!diff.equal);
            }
        }

        /// Inserting a single line should only mark that one line
        #[test]
        fn single_line_insert_marks_one_line(
            prefix_lines in prop::collection::vec("[a-z]{1,10}", 0..5),
            new_line in "[a-z]{1,10}",
            suffix_lines in prop::collection::vec("[a-z]{1,10}", 0..5)
        ) {
            let saved_lines: Vec<String> = prefix_lines.iter()
                .chain(suffix_lines.iter())
                .cloned()
                .collect();
            let saved = saved_lines.join("\n").into_bytes();

            let current_lines: Vec<String> = prefix_lines.iter()
                .cloned()
                .chain(std::iter::once(new_line))
                .chain(suffix_lines.iter().cloned())
                .collect();
            let current = current_lines.join("\n").into_bytes();

            let diff = diff_lines(&saved, &current);

            // Should detect a change
            prop_assert!(!diff.equal);

            // Should only mark the inserted line (at position prefix_lines.len())
            // The changed_lines should contain exactly one range of size 1
            let total_changed: usize = diff.changed_lines.iter()
                .map(|r| r.end - r.start)
                .sum();
            prop_assert_eq!(total_changed, 1, "Inserting one line should mark exactly one line");
        }

        /// Changed lines should always be valid indices in the current buffer
        #[test]
        fn changed_lines_are_valid_indices(
            saved in multiline_string(),
            current in multiline_string()
        ) {
            let diff = diff_lines(&saved, &current);
            let current_line_count = current.split(|&b| b == b'\n').count();

            for range in &diff.changed_lines {
                prop_assert!(range.start < current_line_count || current_line_count == 0,
                    "Range start {} should be < line count {}",
                    range.start, current_line_count);
                prop_assert!(range.end <= current_line_count || current_line_count == 0,
                    "Range end {} should be <= line count {}",
                    range.end, current_line_count);
            }
        }

        /// Changed line ranges should not overlap and should be sorted
        #[test]
        fn changed_lines_are_sorted_and_non_overlapping(
            saved in multiline_string(),
            current in multiline_string()
        ) {
            let diff = diff_lines(&saved, &current);

            for i in 1..diff.changed_lines.len() {
                let prev = &diff.changed_lines[i - 1];
                let curr = &diff.changed_lines[i];
                prop_assert!(prev.end <= curr.start,
                    "Ranges should not overlap: {:?} and {:?}", prev, curr);
            }
        }
    }
}
