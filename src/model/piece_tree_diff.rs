use std::ops::Range;
use std::sync::Arc;

use crate::model::piece_tree::{LeafData, PieceTreeNode};

/// Summary of differences between two piece tree roots.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PieceTreeDiff {
    /// Whether the two trees represent identical piece sequences.
    pub equal: bool,
    /// Changed byte ranges in the "after" tree (exclusive end). Empty when `equal` is true.
    pub byte_ranges: Vec<Range<usize>>,
    /// Changed line ranges in the "after" tree (exclusive end). `None` when line counts are unknown.
    pub line_ranges: Option<Vec<Range<usize>>>,
}

/// Compute a diff between two piece tree roots.
///
/// Comparison happens at the byte-span level (not whole leaves) so split leaves
/// still align. The result identifies the minimal contiguous range in the
/// "after" tree that differs from "before".
///
/// `line_counter` should return the number of line feeds in a slice of a leaf.
/// If it returns None for any consulted slice, the diff will have line_range=None.
pub fn diff_piece_trees(
    before: &Arc<PieceTreeNode>,
    after: &Arc<PieceTreeNode>,
    line_counter: &dyn Fn(&LeafData, usize, usize) -> Option<usize>,
) -> PieceTreeDiff {
    let mut before_leaves = Vec::new();
    collect_leaves(before, &mut before_leaves);
    let before_leaves = normalize_leaves(before_leaves);

    let mut after_leaves = Vec::new();
    collect_leaves(after, &mut after_leaves);
    let after_leaves = normalize_leaves(after_leaves);

    // Fast-path: identical leaf sequences.
    if leaf_slices_equal(&before_leaves, &after_leaves) {
        return PieceTreeDiff {
            equal: true,
            byte_ranges: Vec::new(),
            line_ranges: Some(Vec::new()),
        };
    }

    let before_spans = with_doc_offsets(&before_leaves);
    let after_spans = with_doc_offsets(&after_leaves);

    let _total_after = sum_bytes(&after_leaves);

    // Longest common prefix at byte granularity.
    let prefix = common_prefix_bytes(&before_spans, &after_spans);
    // Longest common suffix without overlapping prefix.
    let suffix = common_suffix_bytes(&before_spans, &after_spans, prefix);

    let ranges = collect_diff_ranges(&before_spans, &after_spans, prefix, suffix);

    // Map byte ranges to line ranges (best effort).
    let line_ranges = line_ranges(&after_spans, &ranges, line_counter);

    PieceTreeDiff {
        equal: false,
        byte_ranges: ranges,
        line_ranges,
    }
}

fn collect_leaves(node: &Arc<PieceTreeNode>, out: &mut Vec<LeafData>) {
    match node.as_ref() {
        PieceTreeNode::Internal { left, right, .. } => {
            collect_leaves(left, out);
            collect_leaves(right, out);
        }
        PieceTreeNode::Leaf {
            location,
            offset,
            bytes,
            line_feed_cnt,
        } => out.push(LeafData::new(*location, *offset, *bytes, *line_feed_cnt)),
    }
}

fn leaves_equal(a: &LeafData, b: &LeafData) -> bool {
    a.location == b.location && a.offset == b.offset && a.bytes == b.bytes
}

fn leaf_slices_equal(a: &[LeafData], b: &[LeafData]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| leaves_equal(x, y))
}

fn normalize_leaves(mut leaves: Vec<LeafData>) -> Vec<LeafData> {
    if leaves.is_empty() {
        return leaves;
    }

    let mut normalized = Vec::with_capacity(leaves.len());
    let mut current = leaves.remove(0);

    for leaf in leaves.into_iter() {
        let contiguous =
            current.location == leaf.location && current.offset + current.bytes == leaf.offset;
        if contiguous {
            // Merge by extending bytes and line feeds if known
            current.bytes += leaf.bytes;
            current.line_feed_cnt = match (current.line_feed_cnt, leaf.line_feed_cnt) {
                (Some(a), Some(b)) => Some(a + b),
                _ => None,
            };
        } else {
            normalized.push(current);
            current = leaf;
        }
    }

    normalized.push(current);
    normalized
}

fn sum_bytes(leaves: &[LeafData]) -> usize {
    leaves.iter().map(|leaf| leaf.bytes).sum()
}

#[derive(Clone)]
struct Span {
    leaf: LeafData,
    doc_offset: usize,
}

fn with_doc_offsets(leaves: &[LeafData]) -> Vec<Span> {
    let mut spans = Vec::with_capacity(leaves.len());
    let mut offset = 0;
    for leaf in leaves {
        spans.push(Span {
            leaf: *leaf,
            doc_offset: offset,
        });
        offset += leaf.bytes;
    }
    spans
}

fn common_prefix_bytes(before: &[Span], after: &[Span]) -> usize {
    let mut b_idx = 0;
    let mut a_idx = 0;
    let mut b_off = 0;
    let mut a_off = 0;
    let mut consumed = 0;

    while b_idx < before.len() && a_idx < after.len() {
        let b = &before[b_idx].leaf;
        let a = &after[a_idx].leaf;

        let b_pos = b.offset + b_off;
        let a_pos = a.offset + a_off;

        if b.location == a.location && b_pos == a_pos {
            let b_rem = b.bytes - b_off;
            let a_rem = a.bytes - a_off;
            let take = b_rem.min(a_rem);

            consumed += take;
            b_off += take;
            a_off += take;

            if b_off == b.bytes {
                b_idx += 1;
                b_off = 0;
            }
            if a_off == a.bytes {
                a_idx += 1;
                a_off = 0;
            }
        } else {
            break;
        }
    }

    consumed
}

fn common_suffix_bytes(before: &[Span], after: &[Span], prefix_bytes: usize) -> usize {
    let total_before = before.iter().map(|s| s.leaf.bytes).sum::<usize>();
    let total_after = after.iter().map(|s| s.leaf.bytes).sum::<usize>();

    let mut b_idx: isize = before.len() as isize - 1;
    let mut a_idx: isize = after.len() as isize - 1;
    let mut b_off = 0;
    let mut a_off = 0;
    let mut consumed = 0;

    while b_idx >= 0
        && a_idx >= 0
        && (total_before - consumed) > prefix_bytes
        && (total_after - consumed) > prefix_bytes
    {
        let b_leaf = &before[b_idx as usize].leaf;
        let a_leaf = &after[a_idx as usize].leaf;

        let b_pos = b_leaf.offset + b_leaf.bytes - b_off;
        let a_pos = a_leaf.offset + a_leaf.bytes - a_off;

        if b_leaf.location == a_leaf.location && b_pos == a_pos {
            let b_rem = b_leaf.bytes - b_off;
            let a_rem = a_leaf.bytes - a_off;
            let take = b_rem.min(a_rem);

            consumed += take;
            b_off += take;
            a_off += take;

            if b_off == b_leaf.bytes {
                b_idx -= 1;
                b_off = 0;
            }
            if a_off == a_leaf.bytes {
                a_idx -= 1;
                a_off = 0;
            }
        } else {
            break;
        }
    }

    consumed.min(total_after.saturating_sub(prefix_bytes))
}

fn collect_diff_ranges(
    before: &[Span],
    after: &[Span],
    prefix: usize,
    suffix: usize,
) -> Vec<Range<usize>> {
    let mut ranges = Vec::new();
    let mut b_idx = 0;
    let mut a_idx = 0;
    let mut b_off = 0;
    let mut a_off = 0;
    let mut matched_prefix = 0;

    // Skip matching prefix
    while matched_prefix < prefix && b_idx < before.len() && a_idx < after.len() {
        let b = &before[b_idx].leaf;
        let a = &after[a_idx].leaf;
        let b_rem = b.bytes - b_off;
        let a_rem = a.bytes - a_off;
        let take = b_rem.min(a_rem).min(prefix - matched_prefix);
        matched_prefix += take;
        b_off += take;
        a_off += take;
        if b_off == b.bytes {
            b_idx += 1;
            b_off = 0;
        }
        if a_off == a.bytes {
            a_idx += 1;
            a_off = 0;
        }
    }

    let total_after = after.iter().map(|s| s.leaf.bytes).sum::<usize>();
    let compare_limit = total_after.saturating_sub(suffix);

    let mut current_start: Option<usize> = None;
    let mut current_end: usize = 0;

    while a_idx < after.len() {
        let a = &after[a_idx];
        let pos = a.doc_offset + a_off;
        if pos >= compare_limit {
            break;
        }

        let matches = if b_idx < before.len() {
            let b = &before[b_idx].leaf;
            let b_pos = b.offset + b_off;
            let a_pos = a.leaf.offset + a_off;
            b.location == a.leaf.location && b_pos == a_pos
        } else {
            false
        };

        if matches {
            if let Some(start) = current_start.take() {
                ranges.push(start..current_end);
            }

            let b = &before[b_idx].leaf;
            let b_rem = b.bytes - b_off;
            let a_rem = a.leaf.bytes - a_off;
            let take = b_rem.min(a_rem).min(compare_limit.saturating_sub(pos));

            b_off += take;
            a_off += take;

            if b_off == b.bytes {
                b_idx += 1;
                b_off = 0;
            }
            if a_off == a.leaf.bytes {
                a_idx += 1;
                a_off = 0;
            }
        } else {
            if current_start.is_none() {
                current_start = Some(pos);
                current_end = pos;
            }
            let take = (a.leaf.bytes - a_off).min(compare_limit.saturating_sub(pos));
            current_end += take;
            a_off += take;
            if a_off == a.leaf.bytes {
                a_idx += 1;
                a_off = 0;
            }
        }
    }

    if let Some(start) = current_start {
        ranges.push(start..current_end);
    }

    // Any trailing unmatched "after" spans up to suffix boundary
    while a_idx < after.len() {
        let start = after[a_idx].doc_offset + a_off;
        if start >= compare_limit {
            break;
        }
        let end = (after[a_idx].doc_offset + after[a_idx].leaf.bytes).min(compare_limit);
        ranges.push(start..end);
        a_idx += 1;
        a_off = 0;
    }

    if ranges.is_empty() {
        let total_after = after.iter().map(|s| s.leaf.bytes).sum::<usize>();
        let compare_limit = total_after.saturating_sub(suffix);
        ranges.push(prefix..compare_limit);
    }

    ranges
}

fn count_lines_in_range(
    spans: &[Span],
    start: usize,
    len: usize,
    line_counter: &dyn Fn(&LeafData, usize, usize) -> Option<usize>,
) -> Option<usize> {
    if len == 0 {
        return Some(0);
    }

    let mut remaining = len;
    let mut offset = start;
    let mut line_feeds = 0usize;

    for span in spans {
        if remaining == 0 {
            break;
        }
        let span_start = span.doc_offset;
        let span_end = span_start + span.leaf.bytes;
        if offset >= span_end {
            continue;
        }
        let local_start = offset.saturating_sub(span_start);
        let available = span.leaf.bytes - local_start;
        let take = available.min(remaining);

        let chunk_lines = line_counter(&span.leaf, local_start, take)?;
        line_feeds += chunk_lines;

        offset += take;
        remaining -= take;
    }

    Some(line_feeds)
}

fn line_ranges(
    after_spans: &[Span],
    byte_ranges: &[Range<usize>],
    line_counter: &dyn Fn(&LeafData, usize, usize) -> Option<usize>,
) -> Option<Vec<Range<usize>>> {
    let mut accum = Vec::with_capacity(byte_ranges.len());
    for range in byte_ranges {
        let lf_before = count_lines_in_range(after_spans, 0, range.start, line_counter)?;
        let lf_in_range = count_lines_in_range(
            after_spans,
            range.start,
            range.end.saturating_sub(range.start),
            line_counter,
        )?;
        let start_line = lf_before;
        let end_line = if range.start == range.end {
            lf_before + 1
        } else {
            lf_before + lf_in_range + 1
        };
        accum.push(start_line..end_line);
    }

    Some(accum)
}

#[cfg(test)]
#[allow(clippy::single_range_in_vec_init)]
mod tests {
    use super::*;
    use crate::model::piece_tree::BufferLocation;

    fn leaf(loc: BufferLocation, offset: usize, bytes: usize, lfs: Option<usize>) -> LeafData {
        LeafData::new(loc, offset, bytes, lfs)
    }

    // Minimal balanced builder for tests.
    fn build(leaves: &[LeafData]) -> Arc<PieceTreeNode> {
        if leaves.is_empty() {
            return Arc::new(PieceTreeNode::Leaf {
                location: BufferLocation::Stored(0),
                offset: 0,
                bytes: 0,
                line_feed_cnt: Some(0),
            });
        }
        if leaves.len() == 1 {
            let l = leaves[0];
            return Arc::new(PieceTreeNode::Leaf {
                location: l.location,
                offset: l.offset,
                bytes: l.bytes,
                line_feed_cnt: l.line_feed_cnt,
            });
        }

        let mid = leaves.len() / 2;
        let left = build(&leaves[..mid]);
        let right = build(&leaves[mid..]);

        Arc::new(PieceTreeNode::Internal {
            left_bytes: sum_bytes(&leaves[..mid]),
            lf_left: leaves[..mid]
                .iter()
                .map(|l| l.line_feed_cnt)
                .try_fold(0usize, |acc, v| v.map(|b| acc + b)),
            left,
            right,
        })
    }

    fn count_line_feeds(leaf: &LeafData, start: usize, len: usize) -> Option<usize> {
        if len == 0 {
            return Some(0);
        }
        // If we know total LFs, assume uniform distribution only when full coverage.
        if start == 0 && len == leaf.bytes {
            return leaf.line_feed_cnt;
        }
        None
    }

    #[test]
    fn detects_identical_trees() {
        let leaves = vec![leaf(BufferLocation::Stored(0), 0, 10, Some(0))];
        let before = build(&leaves);
        let after = build(&leaves);

        let diff = diff_piece_trees(&before, &after, &count_line_feeds);
        assert!(diff.equal);
        assert!(diff.byte_ranges.is_empty());
        assert_eq!(diff.line_ranges, Some(Vec::new()));
    }

    #[test]
    fn detects_single_line_change() {
        let before = build(&[leaf(BufferLocation::Stored(0), 0, 5, Some(0))]);
        let after = build(&[leaf(BufferLocation::Added(1), 0, 5, Some(0))]);

        let diff = diff_piece_trees(&before, &after, &count_line_feeds);
        assert!(!diff.equal);
        assert_eq!(diff.byte_ranges, vec![0..5]);
        assert_eq!(diff.line_ranges, Some(vec![0..1])); // same line, different content
    }

    #[test]
    fn tracks_newlines_in_changed_span() {
        let before = build(&[leaf(BufferLocation::Stored(0), 0, 6, Some(0))]);
        let after = build(&[leaf(BufferLocation::Added(1), 0, 6, Some(1))]); // introduces a newline

        let diff = diff_piece_trees(&before, &after, &count_line_feeds);
        assert!(!diff.equal);
        assert_eq!(diff.byte_ranges, vec![0..6]);
        assert_eq!(diff.line_ranges, Some(vec![0..2])); // spans two lines after change
    }

    #[test]
    fn handles_deletion_by_marking_anchor_line() {
        let before = build(&[
            leaf(BufferLocation::Stored(0), 0, 6, Some(1)), // two lines
            leaf(BufferLocation::Stored(0), 6, 4, Some(0)), // trailing text
        ]);
        let after = build(&[leaf(BufferLocation::Stored(0), 0, 6, Some(1))]);

        let diff = diff_piece_trees(&before, &after, &count_line_feeds);
        assert!(!diff.equal);
        assert_eq!(diff.byte_ranges, vec![6..6]); // no bytes remain at the change site
        assert_eq!(diff.line_ranges, Some(vec![1..2])); // anchor after deleted span
    }

    #[test]
    fn tolerates_split_leaves_with_same_content_prefix() {
        let before = build(&[leaf(BufferLocation::Stored(0), 0, 100, Some(1))]);
        let after = build(&[
            leaf(BufferLocation::Stored(0), 0, 50, Some(0)),
            leaf(BufferLocation::Added(1), 0, 10, Some(0)),
            leaf(BufferLocation::Stored(0), 50, 50, Some(1)),
        ]);

        let diff = diff_piece_trees(&before, &after, &count_line_feeds);
        assert!(!diff.equal);
        // Only the inserted span should be marked.
        assert_eq!(diff.byte_ranges, vec![50..60]);
    }
}
