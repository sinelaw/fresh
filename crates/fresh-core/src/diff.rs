//! Native line-level diff engine.
//!
//! Shared by the `computeLineDiff` plugin API and the host-side diff
//! service; lives in `fresh-core` so both the editor and the plugin
//! runtime consume one implementation.
//!
//! A dependency-free patience diff: lines are interned to integer ids,
//! unique-in-both lines anchor the diff via a longest-increasing-
//! subsequence chain, and the regions between anchors recurse. Chunks
//! with no anchors fall back to a small dense LCS (precise, bounded at
//! `FALLBACK_LCS_CELLS`) or, past that bound, to a single coarse
//! replacement hunk — so no input is ever refused and no input is ever
//! quadratic over the whole file.
//!
//! This exists because plugins can't afford to implement diffs in
//! QuickJS: a dense LCS over two ~5k-line texts was slow enough there
//! that `live_diff.ts` refused to diff at all past a DP-cell cap.
//! Native patience runs the same input in
//! microseconds-to-milliseconds.

use crate::api::LineDiffHunk;
use std::collections::HashMap;

/// Anchor-free chunks up to this many DP cells get a precise dense-LCS
/// diff; larger ones collapse into one replacement hunk. 1M u32 cells
/// is a transient ~4MB table and well under a millisecond natively.
const FALLBACK_LCS_CELLS: u64 = 1_000_000;

/// Patience recursion bound. Each level consumes at least one anchor,
/// so real inputs stay in single digits; the cap only exists to keep
/// adversarial nesting off the native stack. Past it, chunks take the
/// LCS/replace fallback.
const MAX_PATIENCE_DEPTH: u32 = 32;

/// Compute line-level diff hunks transforming `old_text` into `new_text`.
///
/// Lines are `\n`-terminated segments (a final unterminated segment
/// counts as a line), so indices agree with splitting on `\n` and
/// dropping a trailing empty segment — the convention plugins already
/// use. The newline itself is part of the token: a trailing-newline-only
/// change on the last line is reported as a (1-line) hunk rather than
/// hidden, the same way git reports "\ No newline at end of file".
pub fn compute_line_diff(old_text: &str, new_text: &str) -> Vec<LineDiffHunk> {
    let mut interner = LineInterner::default();
    let old_ids: Vec<u32> = old_text
        .split_inclusive('\n')
        .map(|l| interner.intern(l))
        .collect();
    let new_ids: Vec<u32> = new_text
        .split_inclusive('\n')
        .map(|l| interner.intern(l))
        .collect();
    diff_interned_lines(&old_ids, &new_ids)
}

/// Map a byte offset in `old_text` to the corresponding byte offset in
/// `new_text`, so a cursor keeps pointing at the same logical text after
/// the buffer is rewritten (e.g. by format-on-save — issue #2777).
///
/// Semantics (VS Code-like):
/// - An offset before/after every changed region shifts by the byte delta
///   of the preceding hunks, staying anchored to its text.
/// - An offset inside a changed region is refined by the region's common
///   prefix/suffix (so reindenting a line keeps the cursor at its column);
///   if its own text was rewritten it snaps to the end of that common
///   prefix — the start of the replacement.
///
/// Cost is one line-level patience diff (`compute_line_diff`, which trims
/// the common prefix/suffix first) plus one O(lines) offset table per
/// side — no quadratic work on large files.
///
/// The returned offset is always a char boundary in `new_text`, provided
/// `offset` is a char boundary in `old_text`.
pub fn map_offset_through_diff(old_text: &str, new_text: &str, offset: usize) -> usize {
    let offset = offset.min(old_text.len());
    let old_starts = line_start_offsets(old_text);
    let new_starts = line_start_offsets(new_text);

    let mut delta: isize = 0;
    for h in compute_line_diff(old_text, new_text) {
        let old_start = old_starts[h.old_start as usize];
        if offset < old_start {
            // All remaining hunks are after the offset.
            break;
        }
        let old_end = old_starts[(h.old_start + h.old_count) as usize];
        let new_start = new_starts[h.new_start as usize];
        let new_end = new_starts[(h.new_start + h.new_count) as usize];
        if offset < old_end {
            return new_start
                + map_offset_within_replacement(
                    &old_text[old_start..old_end],
                    &new_text[new_start..new_end],
                    offset - old_start,
                );
        }
        delta += (new_end - new_start) as isize - (old_end - old_start) as isize;
    }
    ((offset as isize + delta).max(0) as usize).min(new_text.len())
}

/// Byte offset of each line token per [`compute_line_diff`]'s
/// `split_inclusive('\n')` tokenization, with a trailing sentinel at
/// `text.len()` so `starts[i]..starts[i + count]` is a hunk's byte range.
fn line_start_offsets(text: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for line in text.split_inclusive('\n') {
        starts.push(starts.last().unwrap() + line.len());
    }
    starts
}

/// Map char-boundary offset `rel` in `old` to an offset in `new`, where
/// `new` replaced `old` wholesale: keep it if it sits in the common
/// prefix, mirror it from the end if it sits in the (non-overlapping)
/// common suffix, else snap to the end of the common prefix. Prefix and
/// suffix are computed in whole chars, so the result is a char boundary.
fn map_offset_within_replacement(old: &str, new: &str, rel: usize) -> usize {
    let mut prefix = 0;
    for (a, b) in old.chars().zip(new.chars()) {
        if a != b {
            break;
        }
        prefix += a.len_utf8();
    }
    if rel <= prefix {
        return rel;
    }
    let mut suffix = 0;
    for (a, b) in old[prefix..].chars().rev().zip(new[prefix..].chars().rev()) {
        if a != b {
            break;
        }
        suffix += a.len_utf8();
    }
    if old.len() - rel <= suffix {
        return new.len() - (old.len() - rel);
    }
    prefix
}

/// Interns lines to `u32` ids so every comparison in the diff is an
/// integer compare, not a string compare. Callers that don't hold both
/// sides as contiguous strings (e.g. a host service iterating two
/// buffers' lines) intern each side themselves and call
/// [`diff_interned_lines`]. Equal ids must mean equal line content
/// *including* any line terminator, per the tokenization contract on
/// [`compute_line_diff`].
#[derive(Default)]
pub struct LineInterner<'a> {
    map: HashMap<&'a str, u32>,
}

impl<'a> LineInterner<'a> {
    pub fn intern(&mut self, line: &'a str) -> u32 {
        let next = self.map.len() as u32;
        *self.map.entry(line).or_insert(next)
    }
}

/// Diff two pre-interned line-id sequences. Same output contract as
/// [`compute_line_diff`]: hunks in increasing order, equal regions
/// unreported, never refuses an input.
pub fn diff_interned_lines(old_ids: &[u32], new_ids: &[u32]) -> Vec<LineDiffHunk> {
    let mut hunks = Vec::new();
    diff_range(old_ids, new_ids, 0, 0, 0, &mut hunks);
    hunks
}

/// Diff `old` against `new`, emitting hunks (offset by `old_base` /
/// `new_base` into absolute line numbers) onto `out` in increasing
/// order.
fn diff_range(
    old: &[u32],
    new: &[u32],
    old_base: u32,
    new_base: u32,
    depth: u32,
    out: &mut Vec<LineDiffHunk>,
) {
    // Strip the common prefix and suffix; most calls (a small edit, or
    // the gap between two adjacent patience anchors) reduce to a tiny
    // middle or to nothing here.
    let mut start = 0;
    let min_len = old.len().min(new.len());
    while start < min_len && old[start] == new[start] {
        start += 1;
    }
    let mut old_end = old.len();
    let mut new_end = new.len();
    while old_end > start && new_end > start && old[old_end - 1] == new[new_end - 1] {
        old_end -= 1;
        new_end -= 1;
    }

    let old_mid = &old[start..old_end];
    let new_mid = &new[start..new_end];
    let old_mid_base = old_base + start as u32;
    let new_mid_base = new_base + start as u32;

    if old_mid.is_empty() && new_mid.is_empty() {
        return;
    }
    if old_mid.is_empty() || new_mid.is_empty() {
        // Pure insertion or deletion.
        out.push(LineDiffHunk {
            old_start: old_mid_base,
            old_count: old_mid.len() as u32,
            new_start: new_mid_base,
            new_count: new_mid.len() as u32,
        });
        return;
    }

    if depth < MAX_PATIENCE_DEPTH {
        if let Some(anchors) = patience_anchors(old_mid, new_mid) {
            // Recurse into the gaps around the anchor chain. Anchors are
            // equal lines, so they never appear in a hunk themselves.
            let mut prev_old = 0u32;
            let mut prev_new = 0u32;
            for &(a_old, a_new) in &anchors {
                diff_range(
                    &old_mid[prev_old as usize..a_old as usize],
                    &new_mid[prev_new as usize..a_new as usize],
                    old_mid_base + prev_old,
                    new_mid_base + prev_new,
                    depth + 1,
                    out,
                );
                prev_old = a_old + 1;
                prev_new = a_new + 1;
            }
            diff_range(
                &old_mid[prev_old as usize..],
                &new_mid[prev_new as usize..],
                old_mid_base + prev_old,
                new_mid_base + prev_new,
                depth + 1,
                out,
            );
            return;
        }
    }

    // No anchors (all lines in the chunk repeat, or the depth cap hit):
    // precise dense LCS while it's cheap, one coarse replacement past
    // that. The coarse case marks the whole chunk modified — degraded
    // detail, never a refusal.
    if (old_mid.len() as u64) * (new_mid.len() as u64) <= FALLBACK_LCS_CELLS {
        lcs_diff(old_mid, new_mid, old_mid_base, new_mid_base, out);
    } else {
        out.push(LineDiffHunk {
            old_start: old_mid_base,
            old_count: old_mid.len() as u32,
            new_start: new_mid_base,
            new_count: new_mid.len() as u32,
        });
    }
}

/// Find the patience anchor chain for a chunk: lines that occur exactly
/// once on each side, chained by a longest increasing subsequence so
/// the matched pairs are in order on both sides. Returns `None` when the
/// chunk has no such lines (the caller falls back).
fn patience_anchors(old_mid: &[u32], new_mid: &[u32]) -> Option<Vec<(u32, u32)>> {
    #[derive(Default)]
    struct Occurrence {
        old_count: u32,
        new_count: u32,
        old_pos: u32,
    }

    let mut occurrences: HashMap<u32, Occurrence> = HashMap::new();
    for (i, &id) in old_mid.iter().enumerate() {
        let e = occurrences.entry(id).or_default();
        e.old_count += 1;
        e.old_pos = i as u32;
    }
    for &id in new_mid {
        if let Some(e) = occurrences.get_mut(&id) {
            e.new_count += 1;
        }
    }

    // Candidate pairs (old_pos, new_pos), naturally sorted by new_pos.
    let mut pairs: Vec<(u32, u32)> = Vec::new();
    for (j, &id) in new_mid.iter().enumerate() {
        if let Some(e) = occurrences.get(&id) {
            if e.old_count == 1 && e.new_count == 1 {
                pairs.push((e.old_pos, j as u32));
            }
        }
    }
    if pairs.is_empty() {
        return None;
    }

    // Longest increasing subsequence over old_pos (patience sorting with
    // backpointers). Old positions are distinct, so strict/non-strict
    // comparison is equivalent.
    let mut tails: Vec<usize> = Vec::new(); // indices into `pairs`
    let mut back: Vec<Option<usize>> = vec![None; pairs.len()];
    for (idx, &(old_pos, _)) in pairs.iter().enumerate() {
        let insert_at = tails.partition_point(|&t| pairs[t].0 < old_pos);
        if insert_at > 0 {
            back[idx] = Some(tails[insert_at - 1]);
        }
        if insert_at == tails.len() {
            tails.push(idx);
        } else {
            tails[insert_at] = idx;
        }
    }

    let mut chain = Vec::with_capacity(tails.len());
    let mut cursor = tails.last().copied();
    while let Some(idx) = cursor {
        chain.push(pairs[idx]);
        cursor = back[idx];
    }
    chain.reverse();
    Some(chain)
}

/// Precise diff of a small chunk via dense LCS DP. Only called with
/// `old.len() * new.len() <= FALLBACK_LCS_CELLS`.
fn lcs_diff(old: &[u32], new: &[u32], old_base: u32, new_base: u32, out: &mut Vec<LineDiffHunk>) {
    let m = old.len();
    let n = new.len();
    let stride = n + 1;
    // dp[i * stride + j] = LCS length of old[..i] and new[..j].
    let mut dp = vec![0u32; (m + 1) * stride];
    for i in 1..=m {
        let oi = old[i - 1];
        for j in 1..=n {
            dp[i * stride + j] = if oi == new[j - 1] {
                dp[(i - 1) * stride + (j - 1)] + 1
            } else {
                dp[(i - 1) * stride + j].max(dp[i * stride + (j - 1)])
            };
        }
    }

    // Backtrack, collecting (equal?) steps in reverse; then walk forward
    // grouping the non-equal runs into hunks.
    #[derive(Clone, Copy, PartialEq)]
    enum Step {
        Equal,
        Delete,
        Insert,
    }
    let mut steps: Vec<Step> = Vec::with_capacity(m + n);
    let mut i = m;
    let mut j = n;
    while i > 0 && j > 0 {
        if old[i - 1] == new[j - 1] {
            steps.push(Step::Equal);
            i -= 1;
            j -= 1;
        } else if dp[(i - 1) * stride + j] >= dp[i * stride + (j - 1)] {
            steps.push(Step::Delete);
            i -= 1;
        } else {
            steps.push(Step::Insert);
            j -= 1;
        }
    }
    for _ in 0..i {
        steps.push(Step::Delete);
    }
    for _ in 0..j {
        steps.push(Step::Insert);
    }
    steps.reverse();

    let mut old_pos = 0u32;
    let mut new_pos = 0u32;
    let mut k = 0;
    while k < steps.len() {
        if steps[k] == Step::Equal {
            old_pos += 1;
            new_pos += 1;
            k += 1;
            continue;
        }
        let hunk_old_start = old_pos;
        let hunk_new_start = new_pos;
        while k < steps.len() && steps[k] != Step::Equal {
            match steps[k] {
                Step::Delete => old_pos += 1,
                Step::Insert => new_pos += 1,
                Step::Equal => unreachable!(),
            }
            k += 1;
        }
        out.push(LineDiffHunk {
            old_start: old_base + hunk_old_start,
            old_count: old_pos - hunk_old_start,
            new_start: new_base + hunk_new_start,
            new_count: new_pos - hunk_new_start,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Split `text` the way `live_diff.ts`'s `splitLines` does: "a\nb\n"
    /// -> ["a", "b"], "" -> [], final unterminated segment kept.
    fn split_lines(text: &str) -> Vec<&str> {
        if text.is_empty() {
            return Vec::new();
        }
        let mut lines: Vec<&str> = text.split('\n').collect();
        if lines.last() == Some(&"") {
            lines.pop();
        }
        lines
    }

    /// Core correctness invariant: replaying the hunks against the old
    /// text's lines must reconstruct the new text's lines.
    fn assert_hunks_reconstruct(old_text: &str, new_text: &str, hunks: &[LineDiffHunk]) {
        let old_lines = split_lines(old_text);
        let new_lines = split_lines(new_text);
        let mut rebuilt: Vec<&str> = Vec::new();
        let mut old_pos = 0usize;
        for h in hunks {
            let old_start = h.old_start as usize;
            assert!(
                old_start >= old_pos,
                "hunks must be monotonically increasing"
            );
            rebuilt.extend(&old_lines[old_pos..old_start]);
            let new_start = h.new_start as usize;
            let new_end = new_start + h.new_count as usize;
            rebuilt.extend(&new_lines[new_start..new_end]);
            old_pos = old_start + h.old_count as usize;
        }
        rebuilt.extend(&old_lines[old_pos..]);
        assert_eq!(rebuilt, new_lines, "hunks must reconstruct the new text");
    }

    #[test]
    fn equal_texts_produce_no_hunks() {
        assert!(compute_line_diff("a\nb\nc\n", "a\nb\nc\n").is_empty());
        assert!(compute_line_diff("", "").is_empty());
    }

    #[test]
    fn pure_insertion() {
        let hunks = compute_line_diff("a\nc\n", "a\nb\nc\n");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 1,
                old_count: 0,
                new_start: 1,
                new_count: 1,
            }]
        );
    }

    #[test]
    fn pure_deletion() {
        let hunks = compute_line_diff("a\nb\nc\n", "a\nc\n");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 1,
                old_count: 1,
                new_start: 1,
                new_count: 0,
            }]
        );
    }

    #[test]
    fn replacement() {
        let hunks = compute_line_diff("a\nb\nc\n", "a\nX\nc\n");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 1,
                old_count: 1,
                new_start: 1,
                new_count: 1,
            }]
        );
    }

    #[test]
    fn empty_versus_content() {
        let hunks = compute_line_diff("", "a\nb\n");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 0,
                old_count: 0,
                new_start: 0,
                new_count: 2,
            }]
        );
        let hunks = compute_line_diff("a\nb\n", "");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 0,
                old_count: 2,
                new_start: 0,
                new_count: 0,
            }]
        );
    }

    #[test]
    fn trailing_newline_change_is_reported() {
        // "b" and "b\n" are different tokens: an honest 1-line hunk, the
        // same way git reports "\ No newline at end of file".
        let hunks = compute_line_diff("a\nb", "a\nb\n");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 1,
                old_count: 1,
                new_start: 1,
                new_count: 1,
            }]
        );
    }

    #[test]
    fn hunks_reconstruct_on_mixed_edits() {
        let old_text = "a\nb\nc\nd\ne\nf\ng\n";
        let new_text = "a\nB\nc\nnew1\nnew2\nd\nf\ng\ntail\n";
        let hunks = compute_line_diff(old_text, new_text);
        assert_hunks_reconstruct(old_text, new_text, &hunks);
    }

    #[test]
    fn anchors_split_around_unique_lines() {
        // "u1"/"u2" are unique on both sides; the noise around them must
        // not merge across the anchors into one giant hunk.
        let old_text = "n\nn\nu1\nn\nn\nu2\nn\n";
        let new_text = "m\nu1\nm\nm\nu2\nm\nm\n";
        let hunks = compute_line_diff(old_text, new_text);
        assert_hunks_reconstruct(old_text, new_text, &hunks);
        // u1 (old line 2 / new line 1) and u2 (old line 5 / new line 4)
        // are matched, so no hunk may span them.
        for h in &hunks {
            let old_range = h.old_start..h.old_start + h.old_count;
            let new_range = h.new_start..h.new_start + h.new_count;
            assert!(!old_range.contains(&2) && !new_range.contains(&1));
            assert!(!old_range.contains(&5) && !new_range.contains(&4));
        }
    }

    /// The old-checkout refusal scenario, at the same scale: two ~5-6k-line
    /// texts sharing almost nothing. The old dense-LCS plugin
    /// implementation refused this input (24.9M DP cells > its 16M cap);
    /// the native path must handle it outright.
    /// No timing assertions (see CONTRIBUTING) — a quadratic regression
    /// would show up as an external test timeout.
    #[test]
    fn near_total_rewrite_at_repro_scale() {
        let mut old_text = String::new();
        let mut new_text = String::new();
        // Deterministic, non-repetitive lines; ~1% shared so the common
        // prefix/suffix strip can't rescue a quadratic algorithm.
        for i in 0..5_118 {
            if i % 100 == 0 {
                old_text.push_str(&format!("shared line {i}\n"));
            } else {
                old_text.push_str(&format!("old line {i} payload {}\n", i * 7919));
            }
        }
        for i in 0..6_094 {
            if i % 100 == 0 {
                new_text.push_str(&format!("shared line {i}\n"));
            } else {
                new_text.push_str(&format!("new line {i} payload {}\n", i * 6271));
            }
        }
        let hunks = compute_line_diff(&old_text, &new_text);
        assert!(!hunks.is_empty(), "a near-total rewrite must yield hunks");
        assert_hunks_reconstruct(&old_text, &new_text, &hunks);
    }

    /// Pathological highly-repetitive input (no unique lines anywhere,
    /// so patience finds no anchors): the LCS/replace fallback must
    /// complete and stay correct.
    #[test]
    fn repetitive_input_falls_back_gracefully() {
        let old_text = "x\n".repeat(10_000) + "middle\n" + &"x\n".repeat(10_000);
        let new_text = "x\n".repeat(9_000) + "other\n" + &"x\n".repeat(11_000);
        let hunks = compute_line_diff(&old_text, &new_text);
        assert!(!hunks.is_empty());
        assert_hunks_reconstruct(&old_text, &new_text, &hunks);
    }

    /// Repetitive AND huge: the anchor-free middle exceeds
    /// FALLBACK_LCS_CELLS, taking the coarse replacement path. Output is
    /// coarse but must still reconstruct.
    #[test]
    fn huge_repetitive_input_takes_coarse_path() {
        let old_text = "x\ny\n".repeat(30_000) + "mid\n" + &"y\nx\n".repeat(30_000);
        let new_text = "y\nx\n".repeat(30_000) + "los\n" + &"x\ny\n".repeat(30_000);
        let hunks = compute_line_diff(&old_text, &new_text);
        assert!(!hunks.is_empty());
        assert_hunks_reconstruct(&old_text, &new_text, &hunks);
    }

    /// CRLF endings survive round-trip: tokens carry their `\r\n`, so a
    /// CRLF file diffs by line just like an LF file.
    #[test]
    fn crlf_lines_diff_by_line() {
        let hunks = compute_line_diff("a\r\nb\r\nc\r\n", "a\r\nX\r\nc\r\n");
        assert_eq!(
            hunks,
            vec![LineDiffHunk {
                old_start: 1,
                old_count: 1,
                new_start: 1,
                new_count: 1,
            }]
        );
    }

    // --- map_offset_through_diff (cursor mapping, issue #2777) ---

    /// The #2777 repro: a formatter deletes blank lines; an offset after
    /// the deleted region must shift back and stay anchored to its text.
    #[test]
    fn map_offset_shifts_past_deleted_lines() {
        let old = "alpha\n\n\n\nMARKER xyz\nomega\n";
        let new = "alpha\nMARKER xyz\nomega\n";
        // End of "MARKER xyz" (old line 5) -> end of "MARKER xyz" (new line 2).
        assert_eq!(map_offset_through_diff(old, new, 19), 16);
        // Start of "omega" tracks too.
        assert_eq!(map_offset_through_diff(old, new, 20), 17);
    }

    /// An offset entirely before every change must not move.
    #[test]
    fn map_offset_before_changes_is_identity() {
        let old = "alpha\n\n\n\nMARKER xyz\nomega\n";
        let new = "alpha\nMARKER xyz\nomega\n";
        for offset in 0..=6 {
            assert_eq!(map_offset_through_diff(old, new, offset), offset);
        }
    }

    /// Identical texts (formatter no-op): identity for every offset.
    #[test]
    fn map_offset_identity_on_equal_texts() {
        let text = "a\nb\nc\n";
        for offset in 0..=text.len() {
            assert_eq!(map_offset_through_diff(text, text, offset), offset);
        }
    }

    /// An offset inside a deleted region snaps to the start of the
    /// replacement (VS Code semantics), not into unrelated text.
    #[test]
    fn map_offset_inside_deleted_region_snaps_to_replacement_start() {
        let old = "alpha\n\n\n\nMARKER xyz\nomega\n";
        let new = "alpha\nMARKER xyz\nomega\n";
        // Offsets 6..9 are the three deleted blank lines.
        for offset in 6..9 {
            assert_eq!(map_offset_through_diff(old, new, offset), 6);
        }
    }

    /// The #2706 repro: reindentation rewrites the cursor's own line; the
    /// common-suffix refinement keeps the cursor at the end of its text.
    #[test]
    fn map_offset_tracks_reindented_line_via_common_suffix() {
        let old = "fn main() {\nlet a = 1;\nlet b = 2;\n}\n";
        let new = "fn main() {\n    let a = 1;\n    let b = 2;\n}\n";
        // End of "let b = 2;" (after the ';').
        let old_pos = old.find("2;").unwrap() + 2;
        let new_pos = new.find("2;").unwrap() + 2;
        assert_eq!(map_offset_through_diff(old, new, old_pos), new_pos);
    }

    /// An offset between two hunks shifts only by the earlier hunk's
    /// delta — it must not snap to either hunk.
    #[test]
    fn map_offset_between_hunks_shifts_by_earlier_delta_only() {
        let old = "one\ntwo\nmiddle\nthree\nfour\n";
        let new = "ONE CHANGED\ntwo\nmiddle\nthree\nFOUR CHANGED\n";
        // Start of "middle": line 1 ("two\n") onward is common; the first
        // hunk replaced "one\n" (4 bytes) with "ONE CHANGED\n" (12 bytes).
        let old_pos = old.find("middle").unwrap();
        let new_pos = new.find("middle").unwrap();
        assert_eq!(map_offset_through_diff(old, new, old_pos), new_pos);
    }

    /// Out-of-range input clamps to the new text's length.
    #[test]
    fn map_offset_clamps_to_new_len() {
        assert_eq!(map_offset_through_diff("abc\ndef\n", "abc\n", 8), 4);
        assert_eq!(map_offset_through_diff("abc\n", "abc\n", 100), 4);
    }

    /// Multibyte content: results stay on char boundaries.
    #[test]
    fn map_offset_multibyte_stays_on_char_boundaries() {
        let old = "héllo wörld\n\n\ntail é\n";
        let new = "héllo wörld\ntail é\n";
        // End of "tail é" tracks through the blank-line deletion.
        let old_pos = old.len() - 1; // before final '\n'
        let mapped = map_offset_through_diff(old, new, old_pos);
        assert_eq!(mapped, new.len() - 1);
        assert!(new.is_char_boundary(mapped));
        // An offset inside the rewritten region also lands on a boundary.
        for offset in 0..=old.len() {
            if old.is_char_boundary(offset) {
                assert!(new.is_char_boundary(map_offset_through_diff(old, new, offset)));
            }
        }
    }

    /// Deterministic pseudo-random edit fuzzing: every generated pair
    /// must satisfy the reconstruction invariant.
    #[test]
    fn randomized_edits_reconstruct() {
        // Tiny deterministic LCG so the test needs no rand dependency
        // and never flakes.
        let mut state = 0x2545F4914F6CDD1Du64;
        let mut next = move || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state
        };
        for _case in 0..50 {
            let base_len = (next() % 120) as usize;
            let old_lines: Vec<String> = (0..base_len)
                .map(|_| format!("line {}", next() % 40))
                .collect();
            // Derive the new side by mutating the old side.
            let mut new_lines = old_lines.clone();
            let edits = next() % 20;
            for _ in 0..edits {
                let kind = next() % 3;
                let len = new_lines.len();
                match kind {
                    0 => {
                        let at = if len == 0 {
                            0
                        } else {
                            (next() as usize) % (len + 1)
                        };
                        new_lines.insert(at, format!("ins {}", next() % 40));
                    }
                    1 if len > 0 => {
                        new_lines.remove((next() as usize) % len);
                    }
                    2 if len > 0 => {
                        let at = (next() as usize) % len;
                        new_lines[at] = format!("mod {}", next() % 40);
                    }
                    _ => {}
                }
            }
            let old_text = old_lines
                .iter()
                .map(|l| format!("{l}\n"))
                .collect::<String>();
            let new_text = new_lines
                .iter()
                .map(|l| format!("{l}\n"))
                .collect::<String>();
            let hunks = compute_line_diff(&old_text, &new_text);
            assert_hunks_reconstruct(&old_text, &new_text, &hunks);
        }
    }
}
