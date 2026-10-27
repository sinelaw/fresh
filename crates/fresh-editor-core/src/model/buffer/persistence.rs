//! Persistence state for a `TextBuffer`.
//!
//! Owns the six former flat fields that describe "where this buffer
//! lives on disk and whether its in-memory state has diverged from
//! what's on disk": the filesystem handle, the optional on-disk path,
//! the modified / recovery-pending dirty flags, the saved-root
//! snapshot of the piece tree at last save, and the on-disk file
//! size at last save.
//!
//! The `mark_dirty` method is the single choke-point for flipping
//! both dirty flags. `TextBuffer::mark_content_modified` calls it and
//! then bumps the top-level version counter.

use crate::model::filesystem::{ContentDigest, FileSystem};
use crate::model::piece_tree::{BufferLocation, LeafData, PieceTree, PieceTreeNode, StringBuffer};
use crate::model::piece_tree_diff::PieceTreeDiff;
use crate::recovery_types::HotExitEntry;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

/// Filesystem + save-state for one `TextBuffer`.
pub struct Persistence {
    /// Filesystem abstraction for file I/O operations.
    fs: Arc<dyn FileSystem + Send + Sync>,

    /// Optional file path for persistence.
    file_path: Option<PathBuf>,

    /// Has the buffer been modified since last save?
    modified: bool,

    /// Does the buffer have unsaved changes for recovery auto-save?
    ///
    /// Separate from `modified` because recovery auto-save doesn't
    /// clear `modified` (buffer still differs from on-disk file).
    recovery_pending: bool,

    /// The hot-exit entry holding this buffer's unsaved content, when there
    /// is one: a save records itself there before writing the file, so the
    /// entry can never outlive the save as unsaved work (see
    /// `crate::recovery_types::SaveIntent`).
    hot_exit_entry: Option<HotExitEntry>,

    /// Snapshot of the piece tree root at last save (shared via Arc).
    saved_root: Arc<PieceTreeNode>,

    /// The file size on disk after the last save.
    ///
    /// Used for chunked recovery to know the original file size for
    /// reconstruction. Updated when loading from file or after
    /// saving.
    saved_file_size: Option<usize>,

    /// An in-place save of the file this buffer reads its unloaded parts
    /// from failed after it began overwriting it, so the file may be torn
    /// and those parts' offsets no longer name the bytes the buffer loaded
    /// (issue #3382). Refuses saves that would read them (see
    /// `save::refuse_read_from_torn_file`) even when no recovery metadata
    /// could be written to say so. A save that succeeds re-points the
    /// buffer at what it wrote, and clears it.
    source_torn: bool,

    /// The file this buffer's last in-place save tore (`save::TornWrite`),
    /// whatever file its unloaded parts are read from, and the mtime that
    /// write left on it. The file's mtime moved, but by this buffer's own
    /// write, not someone else's: while it still reads this, the file has
    /// not changed on disk since. A save that succeeds clears it.
    torn_write: Option<(PathBuf, SystemTime)>,

    /// The digest of what the last save wrote, when this buffer had every
    /// byte of it in hand (no Copy ops streamed from the old file) and it was
    /// small enough to be compared. See
    /// [`TextBuffer::saved_content`](super::TextBuffer::saved_content).
    saved_content: Option<ContentDigest>,

    /// Bumped by every write to `saved_root` or `modified`.
    ///
    /// [`Self::diff_since_saved`]'s answer depends on both, and a save
    /// changes them without touching the buffer's content version — so a
    /// render-side cache keyed only on content would keep showing the
    /// pre-save diff. Consumers fold this in; see
    /// [`TextBuffer::save_state_version`](super::TextBuffer::save_state_version).
    save_state_version: u64,

    /// Memo of the last [`Self::diff_since_saved`] result, keyed by
    /// `(content_version, save_state_version)`. The diff is re-derived
    /// per frame by up to three independent consumers (gutter,
    /// scrollbar, plugin state snapshot); the version pair is exactly
    /// the invalidation contract documented on `save_state_version`, so
    /// a single memo makes the second and third calls free. Interior
    /// mutability because the diff is computed behind `&self` on read
    /// paths.
    saved_diff_memo: std::sync::RwLock<Option<(u64, u64, PieceTreeDiff)>>,
}

impl Persistence {
    pub fn new(
        fs: Arc<dyn FileSystem + Send + Sync>,
        file_path: Option<PathBuf>,
        saved_root: Arc<PieceTreeNode>,
        saved_file_size: Option<usize>,
    ) -> Self {
        Self {
            fs,
            file_path,
            modified: false,
            recovery_pending: false,
            hot_exit_entry: None,
            saved_root,
            saved_file_size,
            source_torn: false,
            torn_write: None,
            saved_content: None,
            save_state_version: 0,
            saved_diff_memo: std::sync::RwLock::new(None),
        }
    }

    /// Version of the save state — the pair (`saved_root`, `modified`) that
    /// [`Self::diff_since_saved`] compares against.
    pub fn save_state_version(&self) -> u64 {
        self.save_state_version
    }

    pub fn fs(&self) -> &Arc<dyn FileSystem + Send + Sync> {
        &self.fs
    }

    pub fn file_path(&self) -> Option<&Path> {
        self.file_path.as_deref()
    }

    pub fn file_path_owned(&self) -> Option<PathBuf> {
        self.file_path.clone()
    }

    pub fn set_file_path(&mut self, path: PathBuf) {
        self.file_path = Some(path);
    }

    pub fn clear_file_path(&mut self) {
        self.file_path = None;
    }

    pub fn is_modified(&self) -> bool {
        self.modified
    }

    pub fn set_modified(&mut self, modified: bool) {
        self.modified = modified;
        self.save_state_version = self.save_state_version.wrapping_add(1);
    }

    pub fn clear_modified(&mut self) {
        self.modified = false;
        self.save_state_version = self.save_state_version.wrapping_add(1);
    }

    pub fn is_recovery_pending(&self) -> bool {
        self.recovery_pending
    }

    pub fn set_recovery_pending(&mut self, pending: bool) {
        self.recovery_pending = pending;
    }

    pub fn hot_exit_entry(&self) -> Option<&HotExitEntry> {
        self.hot_exit_entry.as_ref()
    }

    pub fn set_hot_exit_entry(&mut self, entry: Option<HotExitEntry>) {
        self.hot_exit_entry = entry;
    }

    /// The single choke-point for flipping the two dirty flags.
    ///
    /// Called from `TextBuffer::mark_content_modified` after every
    /// edit. Do **not** call `set_modified`/`set_recovery_pending`
    /// directly from edit paths — go through the orchestrator on
    /// `TextBuffer` so the version counter bumps too.
    pub(super) fn mark_dirty(&mut self) {
        self.modified = true;
        self.recovery_pending = true;
        self.save_state_version = self.save_state_version.wrapping_add(1);
    }

    pub fn saved_root(&self) -> &Arc<PieceTreeNode> {
        &self.saved_root
    }

    pub fn set_saved_root(&mut self, root: Arc<PieceTreeNode>) {
        self.saved_root = root;
        self.save_state_version = self.save_state_version.wrapping_add(1);
    }

    pub fn saved_file_size(&self) -> Option<usize> {
        self.saved_file_size
    }

    pub fn set_saved_file_size(&mut self, size: Option<usize>) {
        self.saved_file_size = size;
    }

    pub fn is_source_torn(&self) -> bool {
        self.source_torn
    }

    pub fn set_source_torn(&mut self, torn: bool) {
        self.source_torn = torn;
    }

    pub fn torn_write(&self) -> Option<(&Path, SystemTime)> {
        self.torn_write
            .as_ref()
            .map(|(path, mtime)| (path.as_path(), *mtime))
    }

    pub fn set_torn_write(&mut self, torn_write: Option<(PathBuf, SystemTime)>) {
        self.torn_write = torn_write;
    }

    pub fn saved_content(&self) -> Option<ContentDigest> {
        self.saved_content
    }

    pub fn set_saved_content(&mut self, content: Option<ContentDigest>) {
        self.saved_content = content;
    }

    // ---------- snapshot / diff operations ----------

    /// Replace the saved snapshot with the current piece tree and clear
    /// the modified flag.  Call this after a successful save.
    pub fn mark_saved_snapshot(&mut self, piece_tree: &PieceTree) {
        self.saved_root = piece_tree.root();
        self.modified = false;
        self.save_state_version = self.save_state_version.wrapping_add(1);
    }

    /// Refresh the saved root to match the current tree structure
    /// without clearing the modified flag.  Call this after
    /// structural-only changes (e.g. `chunk_split_and_load` during
    /// search scan) so that `diff_since_saved` can take the fast
    /// `Arc::ptr_eq` path.
    pub fn refresh_saved_root_if_unmodified(&mut self, piece_tree: &PieceTree) {
        if !self.modified {
            self.saved_root = piece_tree.root();
            self.save_state_version = self.save_state_version.wrapping_add(1);
        }
    }

    /// Apply a chunk-load buffer replacement to `saved_root`.
    ///
    /// When viewport loading converts a `Stored(buffer_id)` piece to
    /// `Added(new_buffer_id)` in the current tree and the buffer is
    /// already modified, we must apply the same transformation to
    /// `saved_root` so that `diff_since_saved` can match
    /// loaded-but-unedited regions by `(location, offset)` identity.
    pub fn apply_chunk_load_to_saved_root(
        &mut self,
        old_buffer_id: usize,
        chunk_offset_in_buffer: usize,
        chunk_bytes: usize,
        new_buffer_id: usize,
    ) {
        let mut leaves = Vec::new();
        self.saved_root.collect_leaves(&mut leaves);

        let mut modified = false;
        let mut new_leaves: Vec<LeafData> = Vec::with_capacity(leaves.len() + 2);

        for leaf in &leaves {
            if leaf.location.buffer_id() != old_buffer_id {
                new_leaves.push(*leaf);
                continue;
            }

            let leaf_start = leaf.offset;
            let leaf_end = leaf.offset + leaf.bytes;
            let chunk_start = chunk_offset_in_buffer;
            let chunk_end = chunk_offset_in_buffer + chunk_bytes;

            // Check if this leaf overlaps the chunk range
            if chunk_start >= leaf_end || chunk_end <= leaf_start {
                // No overlap — keep as-is
                new_leaves.push(*leaf);
                continue;
            }

            modified = true;

            // Prefix: portion of this leaf before the chunk
            if chunk_start > leaf_start {
                new_leaves.push(LeafData::new(
                    leaf.location,
                    leaf.offset,
                    chunk_start - leaf_start,
                    None, // line feed count unknown after split
                ));
            }

            // The chunk itself — replaced with Added(new_buffer_id)
            let actual_start = chunk_start.max(leaf_start);
            let actual_end = chunk_end.min(leaf_end);
            let offset_in_chunk = actual_start - chunk_start;
            new_leaves.push(LeafData::new(
                BufferLocation::Added(new_buffer_id),
                offset_in_chunk,
                actual_end - actual_start,
                None,
            ));

            // Suffix: portion of this leaf after the chunk
            if chunk_end < leaf_end {
                new_leaves.push(LeafData::new(
                    leaf.location,
                    chunk_end,
                    leaf_end - chunk_end,
                    None,
                ));
            }
        }

        if modified {
            self.saved_root = PieceTree::from_leaves(&new_leaves).root();
            self.save_state_version = self.save_state_version.wrapping_add(1);
        }
    }

    /// The saved snapshot as a tree.
    fn saved_tree(&self) -> PieceTree {
        PieceTree::from_root(Arc::clone(&self.saved_root))
    }

    /// Total bytes of the saved snapshot tree.
    pub fn saved_total_bytes(&self) -> usize {
        self.saved_tree().total_bytes()
    }

    /// Read a byte range out of the saved snapshot tree. `None` when any
    /// piece in the range is unreadable (e.g. unloaded chunk data).
    pub fn extract_saved_range(
        &self,
        start: usize,
        end: usize,
        buffers: &[StringBuffer],
    ) -> Option<Vec<u8>> {
        loaded_text_range(
            &self.saved_tree(),
            buffers,
            start,
            end.saturating_sub(start),
        )
    }

    /// Diff the current piece tree against the last saved snapshot.
    ///
    /// Two-phase algorithm:
    /// - Phase 1: structure-based diff to find changed byte ranges
    ///   (O(num_leaves)).
    /// - Phase 2: for small changed regions, compare actual bytes
    ///   (so paste-after-delete shows as no change).
    ///
    /// `piece_tree` + `buffers` are passed by reference — this
    /// method only reads them.
    pub fn diff_since_saved(
        &self,
        piece_tree: &PieceTree,
        buffers: &[StringBuffer],
        content_version: u64,
    ) -> PieceTreeDiff {
        // Fast path: if the buffer hasn't been modified since loading
        // or saving, content is identical by definition.
        if !self.modified {
            return PieceTreeDiff {
                equal: true,
                byte_ranges: Vec::new(),
                nodes_visited: 0,
            };
        }

        // Quick check: Arc::ptr_eq on tree roots.
        if Arc::ptr_eq(&self.saved_root, &piece_tree.root()) {
            return PieceTreeDiff {
                equal: true,
                byte_ranges: Vec::new(),
                nodes_visited: 0,
            };
        }

        // Memo probe: every mutation that can change this diff bumps
        // either `content_version` (edits) or `save_state_version`
        // (saves, saved-root rewrites on chunk load), so a matching
        // pair means the cached result is current.
        if let Ok(memo) = self.saved_diff_memo.read() {
            if let Some((cv, ssv, diff)) = memo.as_ref() {
                if *cv == content_version && *ssv == self.save_state_version {
                    return diff.clone();
                }
            }
        }

        let result = self.diff_since_saved_uncached(piece_tree, buffers);
        if let Ok(mut memo) = self.saved_diff_memo.write() {
            *memo = Some((content_version, self.save_state_version, result.clone()));
        }
        result
    }

    fn diff_since_saved_uncached(
        &self,
        piece_tree: &PieceTree,
        buffers: &[StringBuffer],
    ) -> PieceTreeDiff {
        // Phase 1: structure-based diff to find which byte ranges
        // differ. O(number of leaves).
        let structure_diff = self.diff_trees_by_structure(piece_tree);

        // If structure says trees are equal, we're done.
        if structure_diff.equal {
            return structure_diff;
        }

        // Phase 2: for small changed regions, verify with actual
        // content comparison (handles paste-after-delete).
        let total_changed_bytes: usize = structure_diff
            .byte_ranges
            .iter()
            .map(|r| r.end.saturating_sub(r.start))
            .sum();

        // Only verify if the changed region is reasonably small.
        const MAX_VERIFY_BYTES: usize = 64 * 1024;

        if total_changed_bytes <= MAX_VERIFY_BYTES && !structure_diff.byte_ranges.is_empty() {
            if self.verify_content_differs_in_ranges(
                &structure_diff.byte_ranges,
                piece_tree,
                buffers,
            ) {
                return structure_diff;
            } else {
                return PieceTreeDiff {
                    equal: true,
                    byte_ranges: Vec::new(),
                    nodes_visited: structure_diff.nodes_visited,
                };
            }
        }

        // Large changes: trust the structure diff.
        structure_diff
    }

    /// Structure-based diff comparing piece tree leaves
    pub fn diff_trees_by_structure(&self, piece_tree: &PieceTree) -> PieceTreeDiff {
        crate::model::piece_tree_diff::diff_piece_trees(&self.saved_root, &piece_tree.root())
    }

    /// Check if the actual byte content differs in the given ranges.
    fn verify_content_differs_in_ranges(
        &self,
        byte_ranges: &[std::ops::Range<usize>],
        piece_tree: &PieceTree,
        buffers: &[StringBuffer],
    ) -> bool {
        let saved_tree = self.saved_tree();
        let saved_bytes = saved_tree.total_bytes();
        let current_bytes = piece_tree.total_bytes();

        // Different total sizes → content definitely differs.
        if saved_bytes != current_bytes {
            return true;
        }

        for range in byte_ranges {
            if range.start >= range.end {
                continue;
            }

            let saved_slice = loaded_text_range(&saved_tree, buffers, range.start, range.len());
            let current_slice = loaded_text_range(piece_tree, buffers, range.start, range.len());

            match (saved_slice, current_slice) {
                (Some(saved), Some(current)) => {
                    if saved != current {
                        return true;
                    }
                }
                _ => {
                    // Couldn't read content, assume it differs to be safe.
                    return true;
                }
            }
        }

        false
    }
}

// ---------- private free-fn helpers over borrowed storage ----------

/// The bytes at `offset..offset + bytes` of the document `piece_tree` and
/// `buffers` make, or None if any of them aren't loaded. `TextBuffer`'s
/// read-only range reader, here so `verify_content_differs_in_ranges` can
/// use it without a `TextBuffer`.
pub(super) fn loaded_text_range(
    piece_tree: &PieceTree,
    buffers: &[StringBuffer],
    offset: usize,
    bytes: usize,
) -> Option<Vec<u8>> {
    if bytes == 0 {
        return Some(Vec::new());
    }

    let mut result = Vec::with_capacity(bytes);
    let end_offset = offset + bytes;
    for piece in piece_tree.iter_pieces_in_range(offset, end_offset) {
        let read_start = offset.max(piece.doc_offset);
        let read_end = end_offset.min(piece.doc_offset + piece.bytes);
        if read_end <= read_start {
            continue;
        }
        let start = piece.buffer_offset + (read_start - piece.doc_offset);
        let data = buffers.get(piece.location.buffer_id())?.get_data()?;
        result.extend_from_slice(data.get(start..start + (read_end - read_start))?);
    }
    Some(result)
}

#[cfg(test)]
impl Persistence {
    /// Test-only view of the memo key, so tests can distinguish a memo
    /// hit from a recompute (the returned diff is identical either way).
    pub(crate) fn saved_diff_memo_key(&self) -> Option<(u64, u64)> {
        self.saved_diff_memo
            .read()
            .ok()
            .and_then(|m| m.as_ref().map(|(cv, ssv, _)| (*cv, *ssv)))
    }
}

#[cfg(test)]
mod memo_tests {
    use crate::model::buffer::TextBuffer;

    /// The diff memo must populate on first computation, key on the
    /// (content_version, save_state_version) pair, and go stale on the
    /// next edit — the contract that lets the gutter, scrollbar, and
    /// plugin snapshot share one computation per frame.
    #[test]
    fn diff_since_saved_memoizes_on_version_pair() {
        let mut buffer = TextBuffer::from_str_test("alpha\nbeta\ngamma\n");
        // Unmodified: the !modified fast path answers without touching
        // the memo.
        assert!(buffer.diff_since_saved().equal);
        assert_eq!(buffer.persistence_for_test().saved_diff_memo_key(), None);

        buffer.insert(0, "x");
        let first = buffer.diff_since_saved();
        assert!(!first.equal);
        let key = buffer
            .persistence_for_test()
            .saved_diff_memo_key()
            .expect("memo populated after computing a real diff");
        assert_eq!(key, (buffer.version(), buffer.save_state_version()));

        // Second call with unchanged versions returns the same ranges
        // (served from the memo).
        let second = buffer.diff_since_saved();
        assert_eq!(first.byte_ranges, second.byte_ranges);

        // An edit bumps the content version: the stored key no longer
        // matches, and the next call recomputes and re-keys.
        buffer.insert(0, "y");
        assert_ne!(
            buffer.persistence_for_test().saved_diff_memo_key(),
            Some((buffer.version(), buffer.save_state_version()))
        );
        let third = buffer.diff_since_saved();
        assert!(!third.equal);
        assert_eq!(
            buffer.persistence_for_test().saved_diff_memo_key(),
            Some((buffer.version(), buffer.save_state_version()))
        );
    }
}
