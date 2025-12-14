# TimeSource Abstraction Design

## Issue Reference
GitHub Issue #314 - Tests should not rely on real thread sleep

## Problem Statement

The codebase currently has direct calls to `std::thread::sleep()` and `std::time::Instant::now()`
scattered throughout production and test code. This creates several issues:

1. **Slow tests**: Tests that need to wait for time-based events (e.g., debouncing, periodic checks)
   must use real `thread::sleep`, making the test suite unnecessarily slow.
2. **Flaky tests**: Time-dependent tests can be flaky due to system load variations.
3. **Poor testability**: Code with hard-coded time dependencies is difficult to test in isolation.

## Current Time Usage Analysis

### Production Code (`src/`)

| File | Line | Usage | Purpose |
|------|------|-------|---------|
| `services/signal_handler.rs` | 114 | `thread::sleep(100ms)` | Wait for thread backtrace capture |
| `services/lsp/async_handler.rs` | 3872 | `thread::sleep(50ms)` | Retry delay in tests |
| `services/release_checker.rs` | 230 | `thread::sleep(increment)` | Periodic update check loop |
| `app/script_control.rs` | 829 | `thread::sleep(poll_interval)` | Wait-for condition polling |

### Time Measurement (`Instant::now()`)

- Frame timing in `main.rs` event loop
- LSP request timing
- Auto-save intervals in `app/mod.rs`
- Recovery service intervals
- Rate limiting for file change events
- Mouse hover delay timing

### System Time (`SystemTime::now()`)

- Session file timestamps in `session.rs`
- File explorer modification times
- Recovery file timestamps

## Proposed Solution

### TimeSource Trait

Create a `TimeSource` trait that abstracts all time-related operations:

```rust
// src/services/time_source.rs

use std::sync::Arc;
use std::time::{Duration, Instant};

/// Abstraction over time-related operations.
///
/// This trait allows production code to use real system time while tests
/// can use a controllable mock implementation for fast, deterministic testing.
pub trait TimeSource: Send + Sync {
    /// Get the current instant for measuring elapsed time.
    fn now(&self) -> Instant;

    /// Sleep for the specified duration.
    ///
    /// In tests, this may be a no-op or advance logical time.
    fn sleep(&self, duration: Duration);

    /// Get the current instant, usable for elapsed time comparisons.
    /// Returns an opaque value that can be compared with other instants
    /// from the same TimeSource.
    fn elapsed_since(&self, earlier: Instant) -> Duration {
        self.now().duration_since(earlier)
    }
}

/// Type alias for shared time source
pub type SharedTimeSource = Arc<dyn TimeSource>;
```

### RealTimeSource Implementation

```rust
/// Production implementation using actual system time.
#[derive(Debug, Clone, Default)]
pub struct RealTimeSource;

impl TimeSource for RealTimeSource {
    fn now(&self) -> Instant {
        Instant::now()
    }

    fn sleep(&self, duration: Duration) {
        std::thread::sleep(duration);
    }
}
```

### TestTimeSource Implementation

```rust
use std::sync::atomic::{AtomicU64, Ordering};

/// Test implementation with controllable time.
///
/// - `now()` returns a logical instant based on internal counter
/// - `sleep()` is a no-op by default (for fast tests)
/// - Time can be advanced manually via `advance()`
pub struct TestTimeSource {
    /// Logical time in nanoseconds since creation
    logical_nanos: AtomicU64,
    /// Base instant (real time at creation, used for Instant arithmetic)
    base_instant: Instant,
}

impl TestTimeSource {
    pub fn new() -> Self {
        Self {
            logical_nanos: AtomicU64::new(0),
            base_instant: Instant::now(),
        }
    }

    /// Advance logical time by the given duration.
    pub fn advance(&self, duration: Duration) {
        self.logical_nanos.fetch_add(
            duration.as_nanos() as u64,
            Ordering::SeqCst
        );
    }

    /// Get the logical elapsed time since creation.
    pub fn elapsed(&self) -> Duration {
        Duration::from_nanos(self.logical_nanos.load(Ordering::SeqCst))
    }
}

impl TimeSource for TestTimeSource {
    fn now(&self) -> Instant {
        // Return base_instant + logical elapsed time
        self.base_instant + self.elapsed()
    }

    fn sleep(&self, duration: Duration) {
        // No-op for fast tests - just advance logical time
        self.advance(duration);
    }
}
```

## Integration Architecture

### Flow Through Application Layers

```
main()
  │
  ├──► Creates RealTimeSource (or TestTimeSource in tests)
  │
  ├──► Editor::new(..., time_source)
  │      │
  │      ├──► LspManager::new(..., time_source.clone())
  │      │
  │      ├──► RecoveryService::new(..., time_source.clone())
  │      │
  │      └──► Other services that need time...
  │
  └──► run_event_loop(..., time_source)
         │
         └──► Uses time_source.now() for frame timing
```

### Changes Required

1. **New module**: `src/services/time_source.rs`
   - Define `TimeSource` trait
   - Implement `RealTimeSource`
   - Implement `TestTimeSource`

2. **Editor struct**: Add `time_source: SharedTimeSource` field
   - Pass through constructor
   - Pass to child services

3. **Services to update**:
   - `release_checker.rs`: Use time_source for periodic check loop
   - `script_control.rs`: Use time_source for wait_for polling
   - `app/mod.rs`: Use time_source for auto-save timing, hover delay
   - `lsp/manager.rs`: Use time_source for request timing
   - `lsp/async_handler.rs`: Use time_source for retry delays
   - `recovery/mod.rs`: Use time_source for auto-save intervals

4. **Main event loop** (`main.rs`):
   - Create time_source
   - Pass to Editor
   - Use for frame timing

5. **Test harness** (`tests/common/harness.rs`):
   - Create TestTimeSource
   - Pass to Editor in test setup
   - Expose `advance_time()` method

## Migration Strategy

### Phase 1: Infrastructure
1. Create `TimeSource` trait and implementations
2. Add `time_source` field to `Editor`
3. Wire through constructor

### Phase 2: Core Services
1. Update `release_checker.rs`
2. Update `script_control.rs` wait_for
3. Update main event loop frame timing

### Phase 3: Secondary Services
1. Update LSP timing code
2. Update recovery service
3. Update auto-save logic

### Phase 4: Test Migration
1. Update test harness to use `TestTimeSource`
2. Replace `thread::sleep` calls in tests with time advancement
3. Verify test speedup

## Special Cases

### Signal Handler
The sleep in `signal_handler.rs` (line 114) should remain as real `thread::sleep` because:
- It runs in a signal handler context
- It needs real wall-clock time for thread backtrace capture
- Tests don't typically exercise signal handlers

### Frame Timing
The frame timing in `main.rs` can use `TimeSource::now()` but the actual event poll
timeout (`event_poll(Duration)`) necessarily uses real time. This is acceptable because:
- The poll timeout is for responsiveness, not correctness
- Tests use the script control mode which doesn't use the event loop

## Benefits

1. **Fast tests**: No more waiting for real sleeps in tests
2. **Deterministic**: Tests run identically regardless of system load
3. **Better coverage**: Can test time-dependent edge cases easily
4. **Clean architecture**: Clear separation between time source and business logic

## API Examples

### Production Usage
```rust
// In main.rs
let time_source = Arc::new(RealTimeSource);
let editor = Editor::new(config, width, height, working_dir, dir_context, time_source);
```

### Test Usage
```rust
// In test
let time_source = Arc::new(TestTimeSource::new());
let harness = EditorTestHarness::with_time_source(80, 24, time_source.clone())?;

// Advance time by 5 seconds (instant, no actual waiting)
time_source.advance(Duration::from_secs(5));

// Check that time-based behavior occurred
assert!(harness.auto_save_triggered());
```
