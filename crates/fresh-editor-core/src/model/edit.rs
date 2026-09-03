/// Represents a single edit operation in the buffer's history
#[derive(Clone, Debug)]
pub struct Edit {
    pub version: u64,
    pub kind: EditKind,
}

/// The type of edit operation
#[derive(Clone, Debug)]
pub enum EditKind {
    /// Insert operation: bytes were inserted at offset
    Insert { offset: usize, len: usize },
    /// Delete operation: bytes were deleted at offset
    Delete { offset: usize, len: usize },
}

impl Edit {
    /// Create a new insert edit
    pub fn insert(version: u64, offset: usize, len: usize) -> Self {
        Self {
            version,
            kind: EditKind::Insert { offset, len },
        }
    }

    /// Create a new delete edit
    pub fn delete(version: u64, offset: usize, len: usize) -> Self {
        Self {
            version,
            kind: EditKind::Delete { offset, len },
        }
    }
}
