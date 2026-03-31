// Filesystem service layer for async, pluggable file system access
//
// This module provides async filesystem operations with request deduplication
// and batching, wrapping the core FileSystem trait from model/filesystem.

pub mod manager;
pub mod slow;

// Re-export types from model::filesystem for convenience
pub use crate::model::filesystem::{
    DirEntry, EntryType, FileMetadata, FilePermissions, FileReader, FileSystem, FileSystemExt,
    FileWriter, NoopFileSystem, StdFileSystem,
};
pub use manager::FsManager;
pub use slow::{BackendMetrics, BlockControl, SlowFileSystem, SlowFsConfig};
