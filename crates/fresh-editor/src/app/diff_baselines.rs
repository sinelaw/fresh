//! Host-side diff baselines: the registry behind the
//! `registerDiffBaseline` / `diffAgainstBaseline` plugin API family.
//!
//! A baseline is a comparison target for a buffer — its last-saved
//! state, the file on disk, or the file at a git revision — registered
//! once and then diffed against cheaply, so plugins exchange hunks and
//! line slices with the host instead of shipping whole file contents
//! across the plugin bridge on every recompute. See
//! `docs/internal/host-diff-service-design.md`.
//!
//! Ownership: the editor thread allocates ids and runs diffs; content
//! loading (filesystem read, `git show`) happens off-loop via
//! [`crate::app::plugin_offloop::load_diff_baseline`], which fills the
//! shared store and settles the plugin's promise. The store is behind
//! `Arc<Mutex<..>>` solely for that producer/consumer handoff — the
//! same shape as the search-handle registry.

use fresh_core::api::DiffBaselineResult;
use fresh_core::BufferId;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

/// Fidelity strings for [`DiffBaselineResult`].
pub const FIDELITY_EXACT: &str = "exact";
pub const FIDELITY_BYTE_COARSE: &str = "byteCoarse";

/// What a baseline compares against, resolved to loadable form.
#[derive(Debug, Clone)]
pub enum BaselineSpec {
    /// The buffer's last-saved piece-tree snapshot. No content is stored;
    /// diffs read the saved tree directly (and get the structural-diff
    /// fast paths).
    Saved,
    /// The file as currently on disk, read via the owning window's
    /// filesystem authority.
    Disk { path: PathBuf },
    /// The file at a git revision: `git show <spec>` run in `cwd` on the
    /// owning window's authority. `show_spec` is the full `<rev>:<path>`
    /// (or `:0:<path>` for the index) argument; the repo-relative path is
    /// resolved off-loop at load time.
    Git {
        cwd: PathBuf,
        file_path: PathBuf,
        /// "gitRef" ref name (e.g. "HEAD", a branch, an OID), or None for
        /// the index (stage 0).
        git_ref: Option<String>,
    },
}

/// Loaded baseline content: the reference text plus precomputed line
/// starts so line slicing is O(1) per line.
pub struct BaselineContent {
    pub text: String,
    /// Byte offset of each line start (split_inclusive('\n') boundaries),
    /// plus one past-the-end entry.
    pub line_starts: Vec<usize>,
}

impl BaselineContent {
    pub fn new(text: String) -> Self {
        let mut line_starts = vec![0usize];
        let mut pos = 0usize;
        for seg in text.split_inclusive('\n') {
            pos += seg.len();
            line_starts.push(pos);
        }
        Self { text, line_starts }
    }

    /// Number of lines (final unterminated segment counts as a line).
    pub fn line_count(&self) -> usize {
        self.line_starts.len() - 1
    }

    /// Line `idx` without its trailing newline (`\n` or `\r\n`).
    pub fn line(&self, idx: usize) -> Option<&str> {
        let start = *self.line_starts.get(idx)?;
        let end = *self.line_starts.get(idx + 1)?;
        let mut s = &self.text[start..end];
        if let Some(stripped) = s.strip_suffix('\n') {
            s = stripped;
        }
        if let Some(stripped) = s.strip_suffix('\r') {
            s = stripped;
        }
        Some(s)
    }
}

/// One registered baseline.
pub struct BaselineEntry {
    pub buffer_id: BufferId,
    pub spec: BaselineSpec,
    /// Bumped every time content is (re)loaded.
    pub generation: u64,
    /// `None` for `Saved` (which reads the piece tree directly) and while
    /// the initial off-loop load is in flight — but registration only
    /// resolves after the load, so plugin-visible entries always have
    /// content when they need it.
    pub content: Option<BaselineContent>,
}

#[derive(Default)]
pub struct BaselineStoreInner {
    pub entries: HashMap<u64, BaselineEntry>,
}

/// Shared handle to the baseline registry. Cloned into off-loop loader
/// tasks; everything else touches it from the editor thread.
#[derive(Clone, Default)]
pub struct BaselineStore {
    pub inner: Arc<Mutex<BaselineStoreInner>>,
}

impl BaselineStore {
    /// Drop all baselines registered for `buffer_id` (buffer closed).
    pub fn drop_for_buffer(&self, buffer_id: BufferId) {
        if let Ok(mut inner) = self.inner.lock() {
            inner.entries.retain(|_, e| e.buffer_id != buffer_id);
        }
    }
}

/// Diff two loaded baseline contents (e.g. disk vs HEAD).
pub fn diff_contents(old: &BaselineContent, new: &BaselineContent) -> DiffBaselineResult {
    DiffBaselineResult {
        revision: 0,
        fidelity: FIDELITY_EXACT.to_string(),
        hunks: fresh_core::diff::compute_line_diff(&old.text, &new.text),
    }
}

/// Diff a buffer against its saved snapshot, content-accurately, using
/// the structural piece-tree diff as a bounding pre-pass.
///
/// Tier 1 (`diff_since_saved`, memoized) yields changed byte ranges on
/// the new side in O(changed path) — or answers "equal" outright for
/// the steady state. Tier 2 extracts only the changed region (aligned
/// to line boundaries; the bytes outside it are identical on both sides
/// by construction, so old and new region offsets coincide at the start
/// and differ by the length delta at the end) and runs the patience
/// line diff on that region alone. This is what removes the old
/// 64 KiB verify-cap semantics split: the answer is content-based at
/// every size, for the cost of diffing the edited region, not the file.
pub fn diff_against_saved(buffer: &mut crate::model::buffer::TextBuffer) -> DiffBaselineResult {
    let structural = buffer.diff_since_saved();
    if structural.equal {
        return DiffBaselineResult {
            revision: buffer.version(),
            fidelity: FIDELITY_EXACT.to_string(),
            hunks: Vec::new(),
        };
    }

    let new_len = buffer.len();
    let old_len = buffer.saved_total_bytes();

    // Changed span on the new side; empty ranges with !equal means the
    // structural diff couldn't localize (defensive) — treat the whole
    // buffer as the region.
    let (span_start, span_end) = match (
        structural.byte_ranges.first(),
        structural.byte_ranges.last(),
    ) {
        (Some(first), Some(last)) => (first.start, last.end.min(new_len)),
        _ => (0, new_len),
    };
    // Old-side span end via the common-suffix identity: bytes after the
    // last change are identical on both sides.
    let old_span_end = old_len.saturating_sub(new_len.saturating_sub(span_end));

    // Line-align the region on the new side. Requires the line index; a
    // large file before its line-feed scan reports byte-coarse instead.
    let Some(start_pos) = buffer.offset_to_position(span_start) else {
        return DiffBaselineResult {
            revision: buffer.version(),
            fidelity: FIDELITY_BYTE_COARSE.to_string(),
            hunks: Vec::new(),
        };
    };
    let region_start_line = start_pos.line;
    let Some(region_start) = buffer.line_start_offset(region_start_line) else {
        return DiffBaselineResult {
            revision: buffer.version(),
            fidelity: FIDELITY_BYTE_COARSE.to_string(),
            hunks: Vec::new(),
        };
    };
    // End of the line containing the last changed byte (exclusive,
    // including its newline): the start of the next line, or EOF.
    let region_end = if span_end == 0 {
        0
    } else {
        match buffer.offset_to_position(span_end - 1) {
            Some(pos) => buffer
                .line_start_offset(pos.line + 1)
                .unwrap_or(new_len)
                .min(new_len),
            None => new_len,
        }
    };

    // The prefix before `region_start` is byte-identical on both sides,
    // so the old region starts at the same offset; the region extends
    // past `span_end` into common-suffix bytes by (region_end - span_end)
    // on both sides equally.
    let old_region_start = region_start.min(old_len);
    let old_region_end = old_span_end
        .saturating_add(region_end.saturating_sub(span_end))
        .min(old_len);

    let old_bytes = buffer.extract_saved_range(old_region_start, old_region_end);
    let new_bytes = buffer
        .get_text_range_mut(region_start, region_end.saturating_sub(region_start))
        .ok();

    let (Some(old_bytes), Some(new_bytes)) = (old_bytes, new_bytes) else {
        // Unreadable region (unloaded chunk on a large file): report
        // coarse rather than wrong.
        return DiffBaselineResult {
            revision: buffer.version(),
            fidelity: FIDELITY_BYTE_COARSE.to_string(),
            hunks: Vec::new(),
        };
    };

    let old_text = String::from_utf8_lossy(&old_bytes);
    let new_text = String::from_utf8_lossy(&new_bytes);
    let mut hunks = fresh_core::diff::compute_line_diff(&old_text, &new_text);
    let base = region_start_line as u32;
    for h in &mut hunks {
        h.old_start += base;
        h.new_start += base;
    }

    DiffBaselineResult {
        revision: buffer.version(),
        fidelity: FIDELITY_EXACT.to_string(),
        hunks,
    }
}

/// Slice `ranges` of `(start_line, count)` out of loaded content, lines
/// without trailing newlines, grouped per range. Out-of-bounds lines are
/// simply absent from the group (the caller sized ranges from hunks it
/// was just handed, so mismatch means the baseline changed under it —
/// it will re-diff on the refresh notification).
pub fn slice_lines(content: &BaselineContent, ranges: &[(u32, u32)]) -> Vec<Vec<String>> {
    ranges
        .iter()
        .map(|&(start, count)| {
            (start..start.saturating_add(count))
                .filter_map(|i| content.line(i as usize).map(|s| s.to_string()))
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::TextBuffer;

    #[test]
    fn baseline_content_lines_and_slicing() {
        let c = BaselineContent::new("alpha\nbeta\r\ngamma".to_string());
        assert_eq!(c.line_count(), 3);
        assert_eq!(c.line(0), Some("alpha"));
        assert_eq!(c.line(1), Some("beta"));
        assert_eq!(c.line(2), Some("gamma"));
        assert_eq!(c.line(3), None);
        let sliced = slice_lines(&c, &[(0, 2), (2, 5)]);
        assert_eq!(sliced, vec![vec!["alpha", "beta"], vec!["gamma"]]);
    }

    #[test]
    fn diff_against_saved_equal_buffer_is_empty_exact() {
        let mut buffer = TextBuffer::from_str_test("a\nb\nc\n");
        let result = diff_against_saved(&mut buffer);
        assert_eq!(result.fidelity, FIDELITY_EXACT);
        assert!(result.hunks.is_empty());
        assert_eq!(result.revision, buffer.version());
    }

    /// The tier-1-bounded tier-2 composition must produce the same hunks
    /// a whole-file content diff would, while only reading the changed
    /// region.
    #[test]
    fn diff_against_saved_matches_whole_file_diff() {
        let original = (0..200)
            .map(|i| format!("line number {i}\n"))
            .collect::<String>();
        let mut buffer = TextBuffer::from_str_test(&original);

        // Replace line 50 and insert two lines after line 120.
        let l50_start: usize = (0..50).map(|i| format!("line number {i}\n").len()).sum();
        let l50_len = "line number 50\n".len();
        buffer.delete_bytes(l50_start, l50_len);
        buffer.insert(l50_start, "REPLACED fifty\n");
        let l121_start: usize = (0..121)
            .map(|i| format!("line number {i}\n").len())
            .sum::<usize>()
            + ("REPLACED fifty\n".len() - l50_len);
        buffer.insert(l121_start, "INSERTED one\nINSERTED two\n");

        let composed = diff_against_saved(&mut buffer);
        assert_eq!(composed.fidelity, FIDELITY_EXACT);

        let current = buffer
            .get_text_range_mut(0, buffer.len())
            .expect("readable test buffer");
        let whole_file =
            fresh_core::diff::compute_line_diff(&original, &String::from_utf8(current).unwrap());
        assert_eq!(composed.hunks, whole_file);
        // Sanity: the diff found both edits.
        assert_eq!(whole_file.len(), 2);
    }

    /// Above the old 64 KiB verify cap, the identity-based structural
    /// diff over-reports a delete-then-retype as changed; the composed
    /// content diff must not.
    #[test]
    fn diff_against_saved_is_content_accurate_above_verify_cap() {
        let big_line = format!("{}\n", "x".repeat(200));
        let original = big_line.repeat(600); // ~120 KB
        let mut buffer = TextBuffer::from_str_test(&original);
        // Delete a >64KiB middle region and retype the identical bytes.
        let start = big_line.len() * 100;
        let end = big_line.len() * 500;
        let removed = original[start..end].to_string();
        buffer.delete_bytes(start, end - start);
        buffer.insert(start, &removed);

        // The structural tier alone flags this as changed (identity, not
        // content, above the verify cap)...
        assert!(!buffer.diff_since_saved().equal);
        // ...but the composed diff reads the region and reports equality.
        let result = diff_against_saved(&mut buffer);
        assert_eq!(result.fidelity, FIDELITY_EXACT);
        assert!(
            result.hunks.is_empty(),
            "retyped-identical content must diff as equal, got {:?}",
            result.hunks
        );
    }
}
