//! Time source abstraction for testability.
//!
//! This module provides a `TimeSource` trait that abstracts time-related operations,
//! allowing production code to use real system time while tests can use a controllable
//! mock implementation for fast, deterministic testing.
//!
//! See `docs/internal/TIMESOURCE_DESIGN.md` for the full design document.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Abstraction over time-related operations.
///
/// This trait allows production code to use real system time while tests
/// can use a controllable mock implementation for fast, deterministic testing.
pub trait TimeSource: Send + Sync + std::fmt::Debug {
    /// Get the current instant for measuring elapsed time.
    fn now(&self) -> Instant;

    /// Sleep for the specified duration.
    ///
    /// In tests, this may be a no-op or advance logical time.
    fn sleep(&self, duration: Duration);

    /// Calculate elapsed time since an earlier instant.
    fn elapsed_since(&self, earlier: Instant) -> Duration {
        self.now().saturating_duration_since(earlier)
    }
}

/// Type alias for shared time source.
pub type SharedTimeSource = Arc<dyn TimeSource>;

/// Production implementation using actual system time.
#[derive(Debug, Clone, Copy, Default)]
pub struct RealTimeSource;

impl RealTimeSource {
    /// Create a new RealTimeSource.
    pub fn new() -> Self {
        Self
    }

    /// Create a shared RealTimeSource.
    pub fn shared() -> SharedTimeSource {
        Arc::new(Self)
    }
}

impl TimeSource for RealTimeSource {
    fn now(&self) -> Instant {
        Instant::now()
    }

    fn sleep(&self, duration: Duration) {
        std::thread::sleep(duration);
    }
}

/// Test implementation with controllable time.
///
/// - `now()` returns a logical instant based on internal counter
/// - `sleep()` advances logical time (no actual sleeping)
/// - Time can be advanced manually via `advance()`
///
/// # Example
///
/// ```
/// use fresh::services::time_source::{TimeSource, TestTimeSource};
/// use std::time::Duration;
///
/// let time = TestTimeSource::new();
/// let start = time.now();
///
/// // No actual sleeping - just advances logical time
/// time.sleep(Duration::from_secs(5));
///
/// assert!(time.elapsed_since(start) >= Duration::from_secs(5));
/// ```
#[derive(Debug)]
pub struct TestTimeSource {
    /// Logical time in nanoseconds since creation.
    logical_nanos: AtomicU64,
    /// Base instant (real time at creation, used for Instant arithmetic).
    base_instant: Instant,
}

impl Default for TestTimeSource {
    fn default() -> Self {
        Self::new()
    }
}

impl TestTimeSource {
    /// Create a new TestTimeSource with logical time starting at zero.
    pub fn new() -> Self {
        Self {
            logical_nanos: AtomicU64::new(0),
            base_instant: Instant::now(),
        }
    }

    /// Create a shared TestTimeSource.
    pub fn shared() -> Arc<Self> {
        Arc::new(Self::new())
    }

    /// Advance logical time by the given duration.
    ///
    /// This is the primary way to simulate time passage in tests.
    pub fn advance(&self, duration: Duration) {
        self.logical_nanos
            .fetch_add(duration.as_nanos() as u64, Ordering::SeqCst);
    }

    /// Get the logical elapsed time since creation.
    pub fn elapsed(&self) -> Duration {
        Duration::from_nanos(self.logical_nanos.load(Ordering::SeqCst))
    }

    /// Reset logical time to zero.
    pub fn reset(&self) {
        self.logical_nanos.store(0, Ordering::SeqCst);
    }

    /// Get the current logical time in nanoseconds.
    pub fn nanos(&self) -> u64 {
        self.logical_nanos.load(Ordering::SeqCst)
    }
}

impl TimeSource for TestTimeSource {
    fn now(&self) -> Instant {
        // Return base_instant + logical elapsed time.
        // This ensures the returned Instant is valid for duration calculations.
        self.base_instant + self.elapsed()
    }

    fn sleep(&self, duration: Duration) {
        // No actual sleeping - just advance logical time.
        // This makes tests run instantly while still simulating time passage.
        self.advance(duration);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn real_time_source_now_advances() {
        let ts = RealTimeSource::new();
        let t1 = ts.now();
        std::thread::sleep(Duration::from_millis(1));
        let t2 = ts.now();
        assert!(t2 > t1);
    }

    #[test]
    fn test_time_source_starts_at_zero() {
        let ts = TestTimeSource::new();
        assert_eq!(ts.nanos(), 0);
        assert_eq!(ts.elapsed(), Duration::ZERO);
    }

    #[test]
    fn test_time_source_advance() {
        let ts = TestTimeSource::new();
        let start = ts.now();

        ts.advance(Duration::from_secs(5));

        assert_eq!(ts.elapsed(), Duration::from_secs(5));
        assert!(ts.elapsed_since(start) >= Duration::from_secs(5));
    }

    #[test]
    fn test_time_source_sleep_advances_time() {
        let ts = TestTimeSource::new();
        let start = ts.now();

        ts.sleep(Duration::from_millis(100));

        assert_eq!(ts.elapsed(), Duration::from_millis(100));
        assert!(ts.elapsed_since(start) >= Duration::from_millis(100));
    }

    #[test]
    fn test_time_source_reset() {
        let ts = TestTimeSource::new();
        ts.advance(Duration::from_secs(10));
        assert_eq!(ts.elapsed(), Duration::from_secs(10));

        ts.reset();
        assert_eq!(ts.elapsed(), Duration::ZERO);
    }

    #[test]
    fn test_time_source_thread_safe() {
        use std::thread;

        let ts = Arc::new(TestTimeSource::new());
        let ts_clone = ts.clone();

        let handle = thread::spawn(move || {
            for _ in 0..100 {
                ts_clone.advance(Duration::from_millis(1));
            }
        });

        for _ in 0..100 {
            ts.advance(Duration::from_millis(1));
        }

        handle.join().unwrap();

        assert_eq!(ts.elapsed(), Duration::from_millis(200));
    }

    #[test]
    fn shared_time_source_works() {
        let real: SharedTimeSource = RealTimeSource::shared();
        let test: SharedTimeSource = TestTimeSource::shared();

        // Both should implement TimeSource
        let _ = real.now();
        let _ = test.now();
    }
}
