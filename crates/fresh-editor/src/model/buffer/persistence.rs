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
use crate::model::piece_tree::PieceTreeNode;
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
}
