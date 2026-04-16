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

use crate::model::filesystem::FileSystem;
use crate::model::piece_tree::{BufferLocation, LeafData, PieceTree, PieceTreeNode, StringBuffer};
use crate::model::piece_tree_diff::PieceTreeDiff;
use std::path::{Path, PathBuf};
use std::sync::Arc;

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

    /// Snapshot of the piece tree root at last save (shared via Arc).
    saved_root: Arc<PieceTreeNode>,

    /// The file size on disk after the last save.
    ///
    /// Used for chunked recovery to know the original file size for
    /// reconstruction. Updated when loading from file or after
    /// saving.
    saved_file_size: Option<usize>,
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
            saved_root,
            saved_file_size,
        }
    }

    pub fn fs(&self) -> &Arc<dyn FileSystem + Send + Sync> {
        &self.fs
    }

    pub fn set_fs(&mut self, fs: Arc<dyn FileSystem + Send + Sync>) {
        self.fs = fs;
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
    }

    pub fn clear_modified(&mut self) {
        self.modified = false;
    }

    pub fn is_recovery_pending(&self) -> bool {
        self.recovery_pending
    }

    pub fn set_recovery_pending(&mut self, pending: bool) {
        self.recovery_pending = pending;
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
    }

    pub fn saved_root(&self) -> &Arc<PieceTreeNode> {
        &self.saved_root
    }

    pub fn set_saved_root(&mut self, root: Arc<PieceTreeNode>) {
        self.saved_root = root;
    }

    pub fn saved_file_size(&self) -> Option<usize> {
        self.saved_file_size
    }

    pub fn set_saved_file_size(&mut self, size: Option<usize>) {
        self.saved_file_size = size;
    }

    // ---------- snapshot / diff operations ----------

    /// Replace the saved snapshot with the current piece tree and clear
    /// the modified flag.  Call this after a successful save.
    pub fn mark_saved_snapshot(&mut self, piece_tree: &PieceTree) {
        self.saved_root = piece_tree.root();
        self.modified = false;
    }

    /// Refresh the saved root to match the current tree structure
    /// without clearing the modified flag.  Call this after
    /// structural-only changes (e.g. `chunk_split_and_load` during
    /// search scan) so that `diff_since_saved` can take the fast
    /// `Arc::ptr_eq` path.
    pub fn refresh_saved_root_if_unmodified(&mut self, piece_tree: &PieceTree) {
        if !self.modified {
            self.saved_root = piece_tree.root();
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
        }
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
        let saved_bytes = tree_total_bytes(&self.saved_root);
        let current_bytes = piece_tree.total_bytes();

        // Different total sizes → content definitely differs.
        if saved_bytes != current_bytes {
            return true;
        }

        for range in byte_ranges {
            if range.start >= range.end {
                continue;
            }

            let saved_slice = extract_range_from_tree(
                &self.saved_root,
                range.start,
                range.end,
                buffers,
            );
            let current_slice = get_text_range(piece_tree, buffers, range.start, range.len());

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

/// Total bytes in a tree rooted at `root`.
fn tree_total_bytes(root: &Arc<PieceTreeNode>) -> usize {
    match root.as_ref() {
        PieceTreeNode::Internal {
            left_bytes, right, ..
        } => left_bytes + tree_total_bytes(right),
        PieceTreeNode::Leaf { bytes, .. } => *bytes,
    }
}

/// Extract a byte range from an arbitrary tree root.
fn extract_range_from_tree(
    root: &Arc<PieceTreeNode>,
    start: usize,
    end: usize,
    buffers: &[StringBuffer],
) -> Option<Vec<u8>> {
    let mut result = Vec::with_capacity(end.saturating_sub(start));
    collect_range_from_node(root, start, end, 0, buffers, &mut result)?;
    Some(result)
}

fn collect_range_from_node(
    node: &Arc<PieceTreeNode>,
    range_start: usize,
    range_end: usize,
    node_offset: usize,
    buffers: &[StringBuffer],
    result: &mut Vec<u8>,
) -> Option<()> {
    match node.as_ref() {
        PieceTreeNode::Internal {
            left_bytes,
            left,
            right,
            ..
        } => {
            let left_end = node_offset + left_bytes;

            if range_start < left_end {
                collect_range_from_node(left, range_start, range_end, node_offset, buffers, result)?;
            }

            if range_end > left_end {
                collect_range_from_node(right, range_start, range_end, left_end, buffers, result)?;
            }
        }
        PieceTreeNode::Leaf {
            location,
            offset,
            bytes,
            ..
        } => {
            let node_end = node_offset + bytes;

            if range_start < node_end && range_end > node_offset {
                let buf = buffers.get(location.buffer_id())?;
                let data = buf.get_data()?;

                let leaf_start = range_start.saturating_sub(node_offset);
                let leaf_end = (range_end - node_offset).min(*bytes);

                if leaf_start < leaf_end {
                    let slice = data.get(*offset + leaf_start..*offset + leaf_end)?;
                    result.extend_from_slice(slice);
                }
            }
        }
    }
    Some(())
}

/// Read-only equivalent of `TextBuffer::get_text_range`, duplicated
/// here so `verify_content_differs_in_ranges` doesn't need access to
/// `TextBuffer`.  Returns `None` if any required buffer is unloaded.
fn get_text_range(
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
    let mut collected = 0;

    for piece_view in piece_tree.iter_pieces_in_range(offset, end_offset) {
        let buffer_id = piece_view.location.buffer_id();
        if let Some(buffer) = buffers.get(buffer_id) {
            let piece_start_in_doc = piece_view.doc_offset;
            let piece_end_in_doc = piece_view.doc_offset + piece_view.bytes;

            let read_start = offset.max(piece_start_in_doc);
            let read_end = end_offset.min(piece_end_in_doc);

            if read_end > read_start {
                let offset_in_piece = read_start - piece_start_in_doc;
                let bytes_to_read = read_end - read_start;

                let buffer_start = piece_view.buffer_offset + offset_in_piece;
                let buffer_end = buffer_start + bytes_to_read;

                let data = buffer.get_data()?;

                if buffer_end <= data.len() {
                    result.extend_from_slice(&data[buffer_start..buffer_end]);
                    collected += bytes_to_read;

                    if collected >= bytes {
                        break;
                    }
                }
            }
        }
    }

    Some(result)
}
