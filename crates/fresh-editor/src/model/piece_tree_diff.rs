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
    /// Number of tree nodes visited during the diff walk.
    /// When `Arc::ptr_eq` short-circuits effectively, this should be
    /// proportional to the edited region, not the entire tree.
    pub nodes_visited: usize,
}

/// Compute a diff between two piece tree roots.
///
/// Uses structural sharing (Arc::ptr_eq) to skip identical subtrees in O(1),
/// falling back to leaf-level comparison only for subtrees that actually differ.
/// After path-copying edits, this is O(changed_path) instead of O(all_leaves).
///
/// `line_counter` should return the number of line feeds in a slice of a leaf.
/// If it returns None for any consulted slice, the diff will have line_range=None.
pub fn diff_piece_trees(
    before: &Arc<PieceTreeNode>,
    after: &Arc<PieceTreeNode>,
    line_counter: &dyn Fn(&LeafData, usize, usize) -> Option<usize>,
) -> PieceTreeDiff {
    // Fast path: identical subtree (same Arc pointer)
    if Arc::ptr_eq(before, after) {
        return PieceTreeDiff {
            equal: true,
            byte_ranges: Vec::new(),
            line_ranges: Some(Vec::new()),
            nodes_visited: 1,
        };
    }

    // Collect leaves only from differing subtrees using structural walk.
    // Spans include document-absolute byte offsets.
    let mut before_spans = Vec::new();
    let mut after_spans = Vec::new();
    let mut nodes_visited: usize = 0;
    let mut before_doc_offset: usize = 0;
    let mut after_doc_offset: usize = 0;
    let mut after_doc_lf: Option<usize> = Some(0);
    diff_collect_leaves(
        before,
        after,
        &mut before_spans,
        &mut after_spans,
        &mut nodes_visited,
        &mut before_doc_offset,
        &mut after_doc_offset,
        &mut after_doc_lf,
    );

    let before_spans = normalize_spans(before_spans);
    let after_spans = normalize_spans(after_spans);

    // Fast-path: identical leaf sequences (same content, different tree structure).
    if span_slices_equal(&before_spans, &after_spans) {
        return PieceTreeDiff {
            equal: true,
            byte_ranges: Vec::new(),
            line_ranges: Some(Vec::new()),
            nodes_visited,
        };
    }

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
        nodes_visited,
    }
}

/// Total bytes stored under a node (without calling private methods).
fn node_bytes(node: &PieceTreeNode) -> usize {
    match node {
        PieceTreeNode::Internal {
            left_bytes, right, ..
        } => left_bytes + node_bytes(right),
        PieceTreeNode::Leaf { bytes, .. } => *bytes,
    }
}

/// Total line feeds under a node. Returns None if any leaf has unknown count.
fn node_line_feeds(node: &PieceTreeNode) -> Option<usize> {
    match node {
        PieceTreeNode::Internal { lf_left, right, .. } => {
            let left_lf = (*lf_left)?;
            let right_lf = node_line_feeds(right)?;
            Some(left_lf + right_lf)
        }
        PieceTreeNode::Leaf { line_feed_cnt, .. } => *line_feed_cnt,
    }
}

/// Parallel tree walk that uses Arc::ptr_eq to skip identical subtrees.
/// Only collects leaves from subtrees that differ between before and after.
/// Tracks running document byte offsets so collected spans have absolute positions.
/// Also tracks line feeds in all subtrees to populate doc_lf_offset.
fn diff_collect_leaves(
    before: &Arc<PieceTreeNode>,
    after: &Arc<PieceTreeNode>,
    before_out: &mut Vec<Span>,
    after_out: &mut Vec<Span>,
    nodes_visited: &mut usize,
    before_doc_offset: &mut usize,
    after_doc_offset: &mut usize,
    after_doc_lf: &mut Option<usize>,
) {
    *nodes_visited += 2; // counting both before and after nodes

    // Identical subtree - skip entirely, advancing document offsets
    if Arc::ptr_eq(before, after) {
        let bytes = node_bytes(before);
        *before_doc_offset += bytes;
        *after_doc_offset += bytes;
        *after_doc_lf = match (*after_doc_lf, node_line_feeds(after)) {
            (Some(a), Some(b)) => Some(a + b),
            _ => None,
        };
        return;
    }

    match (before.as_ref(), after.as_ref()) {
        // Both internal: recurse into children
        (
            PieceTreeNode::Internal {
                left: b_left,
                right: b_right,
                ..
            },
            PieceTreeNode::Internal {
                left: a_left,
                right: a_right,
                ..
            },
        ) => {
            diff_collect_leaves(
                b_left,
                a_left,
                before_out,
                after_out,
                nodes_visited,
                before_doc_offset,
                after_doc_offset,
                after_doc_lf,
            );
            diff_collect_leaves(
                b_right,
                a_right,
                before_out,
                after_out,
                nodes_visited,
                before_doc_offset,
                after_doc_offset,
                after_doc_lf,
            );
        }
        // Structure mismatch - fall back to full leaf collection for both subtrees
        _ => {
            let mut before_lf_dummy = Some(0); // Not needed for 'before' ranges
            collect_leaves_with_offsets(
                before,
                before_out,
                nodes_visited,
                before_doc_offset,
                &mut before_lf_dummy,
            );
            collect_leaves_with_offsets(
                after,
                after_out,
                nodes_visited,
                after_doc_offset,
                after_doc_lf,
            );
        }
    }
}

fn collect_leaves_with_offsets(
    node: &Arc<PieceTreeNode>,
    out: &mut Vec<Span>,
    nodes_visited: &mut usize,
    doc_offset: &mut usize,
    doc_lf: &mut Option<usize>,
) {
    *nodes_visited += 1;
    match node.as_ref() {
        PieceTreeNode::Internal { left, right, .. } => {
            collect_leaves_with_offsets(left, out, nodes_visited, doc_offset, doc_lf);
            collect_leaves_with_offsets(right, out, nodes_visited, doc_offset, doc_lf);
        }
        PieceTreeNode::Leaf {
            location,
            offset,
            bytes,
            line_feed_cnt,
        } => {
            let leaf = LeafData::new(*location, *offset, *bytes, *line_feed_cnt);
            out.push(Span {
                leaf,
                doc_offset: *doc_offset,
                doc_lf_offset: doc_lf.unwrap_or(0),
            });
            *doc_offset += bytes;
            *doc_lf = match (*doc_lf, line_feed_cnt) {
                (Some(a), Some(b)) => Some(a + b),
                _ => None,
            };
        }
    }
}

#[derive(Clone)]
struct Span {
    leaf: LeafData,
    doc_offset: usize,
    doc_lf_offset: usize,
}

fn spans_equal(a: &Span, b: &Span) -> bool {
    a.leaf.location == b.leaf.location
        && a.leaf.offset == b.leaf.offset
        && a.leaf.bytes == b.leaf.bytes
}

fn span_slices_equal(a: &[Span], b: &[Span]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| spans_equal(x, y))
}

fn normalize_spans(spans: Vec<Span>) -> Vec<Span> {
    if spans.is_empty() {
        return spans;
    }

    let mut it = spans.into_iter();
    let mut current = it.next().unwrap();
    let mut normalized = Vec::new();

    for span in it {
        let contiguous = current.leaf.location == span.leaf.location
            && current.leaf.offset + current.leaf.bytes == span.leaf.offset
            && current.doc_offset + current.leaf.bytes == span.doc_offset;
        if contiguous {
            current.leaf.bytes += span.leaf.bytes;
            current.leaf.line_feed_cnt = match (current.leaf.line_feed_cnt, span.leaf.line_feed_cnt)
            {
                (Some(a), Some(b)) => Some(a + b),
                _ => None,
            };
        } else {
            normalized.push(current);
            current = span;
        }
    }

    normalized.push(current);
    normalized
}

fn common_prefix_bytes(before: &[Span], after: &[Span]) -> usize {
    let mut b_idx = 0;
    let mut a_idx = 0;
    let mut b_off = 0;
    let mut a_off = 0;
    let mut consumed = 0;

    while b_idx < before.len() && a_idx < after.len() {
        let b_span = &before[b_idx];
        let a_span = &after[a_idx];
        let b = &b_span.leaf;
        let a = &a_span.leaf;

        let b_pos = b.offset + b_off;
        let a_pos = a.offset + a_off;

        // Must also ensure they are at the same document relative position
        // if they were separated by gaps.
        if b.location == a.location
            && b_pos == a_pos
            && (b_span.doc_offset + b_off) == (a_span.doc_offset + a_off)
        {
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
    let total_before = before
        .last()
        .map(|s| s.doc_offset + s.leaf.bytes)
        .unwrap_or(0);
    let total_after = after
        .last()
        .map(|s| s.doc_offset + s.leaf.bytes)
        .unwrap_or(0);

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
        let b_span = &before[b_idx as usize];
        let a_span = &after[a_idx as usize];
        let b_leaf = &b_span.leaf;
        let a_leaf = &a_span.leaf;

        let b_pos = b_leaf.offset + b_leaf.bytes - b_off;
        let a_pos = a_leaf.offset + a_leaf.bytes - a_off;

        // Compare by buffer identity only (location + offset). Suffix bytes are
        // at different doc_offsets in before vs after when insertions/deletions
        // change the total size, but they still contain the same data.
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

    // compare_limit in document-absolute space: end of collected spans minus suffix
    let doc_end = after
        .last()
        .map(|s| s.doc_offset + s.leaf.bytes)
        .unwrap_or(0);
    let compare_limit = doc_end.saturating_sub(suffix);

    // prefix_start in document-absolute space
    let doc_start = after.first().map(|s| s.doc_offset).unwrap_or(0);

    let mut current_start: Option<usize> = None;
    let mut current_end: usize = 0;

    while a_idx < after.len() {
        let a = &after[a_idx];
        let pos = a.doc_offset + a_off;
        if pos >= compare_limit {
            break;
        }

        // If we have a current range but there's a gap in document offset,
        // it means there was an identical subtree that was skipped.
        // We must break the range here.
        if let Some(start) = current_start {
            if pos > current_end {
                ranges.push(start..current_end);
                current_start = None;
            }
        }

        let matches = if b_idx < before.len() {
            let b = &before[b_idx].leaf;
            let b_pos = b.offset + b_off;
            let a_pos = a.leaf.offset + a_off;
            // Compare by buffer identity only. Insertions/deletions shift
            // doc_offsets, but matching buffer location + offset means same data.
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
            }
            let take = (a.leaf.bytes - a_off).min(compare_limit.saturating_sub(pos));
            current_end = pos + take;
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
        // Anchor range: either there's content between prefix and suffix, or
        // the trees differ only in size (deletion) — report the anchor point.
        ranges.push((doc_start + prefix)..compare_limit);
    }

    ranges
}

fn count_lines_in_range(
    spans: &[Span],
    start: usize,
    end: usize,
    line_counter: &dyn Fn(&LeafData, usize, usize) -> Option<usize>,
) -> Option<usize> {
    if start >= end {
        return Some(0);
    }

    let mut line_feeds = 0usize;
    for span in spans {
        let span_start = span.doc_offset;
        let span_end = span_start + span.leaf.bytes;

        if end <= span_start {
            break;
        }
        if start >= span_end {
            continue;
        }

        let overlap_start = start.max(span_start);
        let overlap_end = end.min(span_end);
        let local_start = overlap_start - span_start;
        let len = overlap_end - overlap_start;

        let chunk_lines = line_counter(&span.leaf, local_start, len)?;
        line_feeds += chunk_lines;
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
        // Find the span containing the start of the range to get its doc_lf_offset.
        // Use <= for the upper bound so deletion anchors at span boundaries are found.
        let span = after_spans
            .iter()
            .find(|s| range.start >= s.doc_offset && range.start <= s.doc_offset + s.leaf.bytes)?;

        let offset_into_span = (range.start - span.doc_offset).min(span.leaf.bytes);
        let lf_at_span_start = span.doc_lf_offset;
        let lf_to_range_start = line_counter(&span.leaf, 0, offset_into_span)?;
        let start_line = lf_at_span_start + lf_to_range_start;

        let lf_in_range = count_lines_in_range(after_spans, range.start, range.end, line_counter)?;

        let end_line = if range.start == range.end {
            start_line + 1
        } else {
            start_line + lf_in_range + 1
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

    fn sum_bytes(leaves: &[LeafData]) -> usize {
        leaves.iter().map(|leaf| leaf.bytes).sum()
    }

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

    /// Uses real PieceTree::insert (path-copy) near EOF.
    /// The diff must produce document-absolute offsets.
    #[test]
    fn diff_after_path_copy_insert_at_eof() {
        use crate::model::piece_tree::{PieceTree, StringBuffer};

        // 10 KB file, split into 1 KB chunks → 10 leaves.
        let chunk_size = 1000;
        let total = 10_000;
        // Build content: 10 LFs per 1000-byte chunk (one every 100 bytes).
        let content: Vec<u8> = (0..total)
            .map(|i| if i % 100 == 99 { b'\n' } else { b'A' })
            .collect();
        let buf = StringBuffer::new_loaded(0, content, false);

        let mut saved_tree = PieceTree::new(BufferLocation::Stored(0), 0, total, None);
        saved_tree.split_leaves_to_chunk_size(chunk_size);
        // Set line feeds for each leaf.
        let lf_updates: Vec<(usize, usize)> = (0..10).map(|i| (i, 10)).collect();
        saved_tree.update_leaf_line_feeds(&lf_updates);
        let saved_root = saved_tree.root();

        // Insert 5 bytes near EOF via real path-copy.
        let mut after_tree = saved_tree;
        let insert_offset = total - 100; // 9900
        let insert_buf = StringBuffer::new_loaded(1, b"HELLO".to_vec(), false);
        after_tree.insert(
            insert_offset,
            BufferLocation::Added(1),
            0,
            5,
            Some(0),
            &[buf, insert_buf],
        );
        let after_root = after_tree.root();

        let diff = diff_piece_trees(&saved_root, &after_root, &count_line_feeds);
        assert!(!diff.equal);

        assert!(
            diff.byte_ranges[0].start >= total - 200,
            "byte_ranges should be document-absolute (near EOF): got {:?}, expected near {}",
            diff.byte_ranges,
            insert_offset,
        );

        if let Some(ref lr) = diff.line_ranges {
            assert!(
                lr[0].start >= 90,
                "line_ranges should be document-absolute (>= 90), got {:?}",
                lr
            );
        }
    }

    /// After rebalance, Arc sharing is destroyed. The diff must still produce
    /// the same byte_ranges and line_ranges as the path-copy version.
    #[test]
    fn diff_after_rebalance_matches_path_copy_diff() {
        use crate::model::piece_tree::{PieceTree, StringBuffer};

        let chunk_size = 1000;
        let total = 10_000;
        let content: Vec<u8> = (0..total)
            .map(|i| if i % 100 == 99 { b'\n' } else { b'A' })
            .collect();
        let buf = StringBuffer::new_loaded(0, content, false);

        let mut saved_tree = PieceTree::new(BufferLocation::Stored(0), 0, total, None);
        saved_tree.split_leaves_to_chunk_size(chunk_size);
        let lf_updates: Vec<(usize, usize)> = (0..10).map(|i| (i, 10)).collect();
        saved_tree.update_leaf_line_feeds(&lf_updates);
        let saved_root = saved_tree.root();

        // Insert near EOF via path-copy.
        let mut after_tree = saved_tree;
        let insert_buf = StringBuffer::new_loaded(1, b"HELLO".to_vec(), false);
        after_tree.insert(
            total - 100,
            BufferLocation::Added(1),
            0,
            5,
            Some(0),
            &[buf.clone(), insert_buf.clone()],
        );

        // Diff with Arc sharing (path-copy).
        let diff_shared = diff_piece_trees(&saved_root, &after_tree.root(), &count_line_feeds);

        // Rebalance → destroys all Arc sharing.
        after_tree.rebalance();
        let diff_rebalanced = diff_piece_trees(&saved_root, &after_tree.root(), &count_line_feeds);

        assert!(!diff_shared.equal);
        assert!(!diff_rebalanced.equal);
        assert_eq!(
            diff_shared.byte_ranges, diff_rebalanced.byte_ranges,
            "byte_ranges should be identical whether or not Arc sharing exists"
        );
        assert_eq!(
            diff_shared.line_ranges, diff_rebalanced.line_ranges,
            "line_ranges should be identical whether or not Arc sharing exists"
        );
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

    #[test]
    fn diff_with_disjoint_changes_reports_correct_line_numbers() {
        let leaf1_before = leaf(BufferLocation::Stored(0), 0, 10, Some(0));
        let leaf2 = leaf(BufferLocation::Stored(0), 10, 10, Some(1)); // Has 1 LF
        let leaf3_before = leaf(BufferLocation::Stored(0), 20, 10, Some(0));

        let leaf1_after = leaf(BufferLocation::Added(1), 0, 10, Some(0));
        let leaf3_after = leaf(BufferLocation::Added(1), 10, 10, Some(0));

        // Manually build trees to ensure structural sharing of leaf2
        let leaf2_arc = Arc::new(PieceTreeNode::Leaf {
            location: leaf2.location,
            offset: leaf2.offset,
            bytes: leaf2.bytes,
            line_feed_cnt: leaf2.line_feed_cnt,
        });

        let before = Arc::new(PieceTreeNode::Internal {
            left_bytes: 10,
            lf_left: Some(0),
            left: Arc::new(PieceTreeNode::Leaf {
                location: leaf1_before.location,
                offset: leaf1_before.offset,
                bytes: leaf1_before.bytes,
                line_feed_cnt: leaf1_before.line_feed_cnt,
            }),
            right: Arc::new(PieceTreeNode::Internal {
                left_bytes: 10,
                lf_left: Some(1),
                left: Arc::clone(&leaf2_arc),
                right: Arc::new(PieceTreeNode::Leaf {
                    location: leaf3_before.location,
                    offset: leaf3_before.offset,
                    bytes: leaf3_before.bytes,
                    line_feed_cnt: leaf3_before.line_feed_cnt,
                }),
            }),
        });

        let after = Arc::new(PieceTreeNode::Internal {
            left_bytes: 10,
            lf_left: Some(0),
            left: Arc::new(PieceTreeNode::Leaf {
                location: leaf1_after.location,
                offset: leaf1_after.offset,
                bytes: leaf1_after.bytes,
                line_feed_cnt: leaf1_after.line_feed_cnt,
            }),
            right: Arc::new(PieceTreeNode::Internal {
                left_bytes: 10,
                lf_left: Some(1),
                left: Arc::clone(&leaf2_arc), // Shared!
                right: Arc::new(PieceTreeNode::Leaf {
                    location: leaf3_after.location,
                    offset: leaf3_after.offset,
                    bytes: leaf3_after.bytes,
                    line_feed_cnt: leaf3_after.line_feed_cnt,
                }),
            }),
        });

        let diff = diff_piece_trees(&before, &after, &count_line_feeds);

        assert_eq!(diff.byte_ranges, vec![0..10, 20..30]);
        // Change 1 is at line 0.
        // leaf2 has 1 LF, so Change 2 (leaf3) starts at line 1.
        assert_eq!(
            diff.line_ranges,
            Some(vec![0..1, 1..2]),
            "Change after a skipped subtree with line feeds should have correct line number"
        );
    }
}
