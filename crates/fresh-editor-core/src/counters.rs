//! Performance counters for observability and testing.
//!
//! Two kinds, for two different questions:
//!
//! * [`Counters`] — process-global atomics for I/O that happens on whatever
//!   thread does it (disk reads, recovery chunks). Relaxed ordering: these are
//!   best-effort metrics, not synchronization primitives.
//! * [`work`] — per-thread counters for the layout path, where a test needs to
//!   measure exactly the work *it* caused.

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

/// Per-thread work counters for the layout path.
///
/// The question these answer is "did this operation stay bounded by the
/// screen?": a frame, a keystroke or a scroll step should touch a few
/// screenfuls of text whatever the file's size, and a figure that tracks the
/// file instead is the signature of a per-line or whole-file scan — the cost
/// the large-file paths exist to avoid. Tests assert on these rather than on
/// the clock, so they mean the same on a loaded CI runner as on an idle
/// laptop.
///
/// **Per thread, not global**, unlike [`Counters`] above. Editing and
/// rendering are synchronous on the thread that drives them, so a test that
/// resets and reads these observes exactly its own work — no interference
/// from tests running beside it in the same process under `cargo test`, and
/// no atomic in a path that runs once per token.
pub mod work {
    use std::cell::Cell;

    thread_local! {
        static BUFFER_BYTES_READ: Cell<u64> = const { Cell::new(0) };
        static TEXT_BYTES_SEGMENTED: Cell<u64> = const { Cell::new(0) };
        static TEXT_BYTES_MEASURED: Cell<u64> = const { Cell::new(0) };
    }

    /// Bytes handed out by `TextBuffer`'s range reads on this thread.
    pub fn buffer_bytes_read() -> u64 {
        BUFFER_BYTES_READ.with(|c| c.get())
    }

    /// Bytes put through UAX #29 grapheme segmentation by the view pipeline
    /// on this thread. Text that reaches the segmenter but never reaches the
    /// screen is per-frame waste.
    pub fn text_bytes_segmented() -> u64 {
        TEXT_BYTES_SEGMENTED.with(|c| c.get())
    }

    /// Bytes whose display width the wrap machine measured on this thread
    /// ([`visual_layout::visual_width`](crate::primitives::visual_layout::visual_width)).
    /// Measuring the same run twice is pure waste, and on a long token it is
    /// the frame's second-largest cost after the split itself.
    pub fn text_bytes_measured() -> u64 {
        TEXT_BYTES_MEASURED.with(|c| c.get())
    }

    pub fn add_text_bytes_measured(n: u64) {
        TEXT_BYTES_MEASURED.with(|c| c.set(c.get().wrapping_add(n)));
    }

    pub fn add_buffer_bytes_read(n: u64) {
        BUFFER_BYTES_READ.with(|c| c.set(c.get().wrapping_add(n)));
    }

    pub fn add_text_bytes_segmented(n: u64) {
        TEXT_BYTES_SEGMENTED.with(|c| c.set(c.get().wrapping_add(n)));
    }

    /// Zero this thread's counters. Call immediately before the operation
    /// under measurement.
    pub fn reset() {
        BUFFER_BYTES_READ.with(|c| c.set(0));
        TEXT_BYTES_SEGMENTED.with(|c| c.set(0));
        TEXT_BYTES_MEASURED.with(|c| c.set(0));
    }
}
