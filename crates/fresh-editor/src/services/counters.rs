//! Global performance counters for observability and testing.
//!
//! Atomic counters that can be incremented from anywhere and read/reset in tests.
//! All operations use relaxed ordering — these are best-effort metrics, not
//! synchronization primitives.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::LazyLock;

static GLOBAL: LazyLock<Counters> = LazyLock::new(Counters::default);

/// Access the global counters instance.
pub fn global() -> &'static Counters {
    &GLOBAL
}

#[derive(Debug, Default)]
pub struct Counters {
    /// Number of recovery chunk files loaded from disk.
    pub recovery_chunks_loaded: AtomicU64,
    /// Total bytes of recovery chunk data loaded into memory.
    pub recovery_bytes_loaded: AtomicU64,
    /// Total bytes read from disk via FileSystem trait methods.
    pub disk_bytes_read: AtomicU64,
}

impl Counters {
    pub fn reset(&self) {
        self.recovery_chunks_loaded.store(0, Ordering::Relaxed);
        self.recovery_bytes_loaded.store(0, Ordering::Relaxed);
        self.disk_bytes_read.store(0, Ordering::Relaxed);
    }

    pub fn inc_recovery_chunks(&self, n: u64) {
        self.recovery_chunks_loaded.fetch_add(n, Ordering::Relaxed);
    }

    pub fn inc_recovery_bytes(&self, n: u64) {
        self.recovery_bytes_loaded.fetch_add(n, Ordering::Relaxed);
    }

    pub fn inc_disk_bytes_read(&self, n: u64) {
        self.disk_bytes_read.fetch_add(n, Ordering::Relaxed);
    }

    pub fn get_recovery_chunks(&self) -> u64 {
        self.recovery_chunks_loaded.load(Ordering::Relaxed)
    }

    pub fn get_recovery_bytes(&self) -> u64 {
        self.recovery_bytes_loaded.load(Ordering::Relaxed)
    }

    pub fn get_disk_bytes_read(&self) -> u64 {
        self.disk_bytes_read.load(Ordering::Relaxed)
    }
}
