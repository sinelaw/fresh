// Filesystem abstraction layer for async, pluggable file system access
//
// This module provides a clean abstraction over filesystem operations,
// designed to work efficiently with both local and network filesystems.

pub mod backend;
pub mod local;
pub mod manager;
pub mod slow;

pub use backend::{FsBackend, FsEntry, FsEntryType, FsMetadata};
pub use local::LocalFsBackend;
pub use manager::FsManager;
pub use slow::{BackendMetrics, SlowFsBackend, SlowFsConfig};
