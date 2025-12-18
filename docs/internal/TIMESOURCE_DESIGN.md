# TimeSource Abstraction (Implemented)

This document started as a design proposal. The `TimeSource` abstraction is now implemented in
`src/services/time_source.rs` and is wired through `Editor` for deterministic tests.

If you are looking for “what exists today”, prefer this document over older references.

## Problem Statement

The codebase currently has direct calls to `std::thread::sleep()` and `std::time::Instant::now()`
scattered throughout production and test code. This creates several issues:

1. **Slow tests**: Tests that need to wait for time-based events (e.g., debouncing, periodic checks)
   must use real `thread::sleep`, making the test suite unnecessarily slow.
2. **Flaky tests**: Time-dependent tests can be flaky due to system load variations.
3. **Poor testability**: Code with hard-coded time dependencies is difficult to test in isolation.

## Current Usage (Today)

As of the current code:
- Most editor subsystems that need time use `Editor`’s `time_source` (`SharedTimeSource`), so tests can run deterministically by swapping in `TestTimeSource`.
- The `main.rs` frame loop still uses real time (`std::time::Instant`) because terminal event polling (`crossterm::event::poll`) is inherently wall-clock driven.
- There are still a few direct `std::thread::sleep` usages in non-interactive/background paths (e.g., release checker loops) and in tests; use `rg "thread::sleep"` and `rg "Instant::now"` to get an up-to-date list.

System time (`SystemTime`) is still used where wall-clock timestamps are required (e.g., file mtimes).

## Proposed Solution

### TimeSource Trait

The `TimeSource` trait abstracts time-related operations:

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
        self.now().saturating_duration_since(earlier)
    }
}

/// Type alias for shared time source
pub type SharedTimeSource = Arc<dyn TimeSource>;
```

### RealTimeSource Implementation

Implemented as `RealTimeSource` in `src/services/time_source.rs`.

### TestTimeSource Implementation

Implemented as `TestTimeSource` in `src/services/time_source.rs`.

Note: `TestTimeSource::sleep()` advances logical time (it is not a no-op), which keeps tests fast
while still letting production code “sleep” against a controllable clock.

## Integration Architecture

### Flow Through Application Layers

```
main()
  │
  ├──► Creates RealTimeSource (or TestTimeSource in tests)
  │
  ├──► Editor::with_working_dir(..., time_source?)
  │      │
  │      ├──► LspManager::new(..., time_source.clone())
  │      │
  │      ├──► RecoveryService::new(..., time_source.clone())
  │      │
  │      └──► Other services that need time...
  │
  └──► run_event_loop(...)
         │
         └──► Uses std::time::Instant for frame timing
```

### Changes Required

This section is largely complete.

Current status:
- Implemented: `src/services/time_source.rs` and `src/services/mod.rs`
- Implemented: `Editor` owns a `SharedTimeSource` (defaulting to `RealTimeSource::shared()` when
  not provided); see `src/app/mod.rs`
- Implemented: most editor subsystems use `self.time_source` for timing (mouse hover delays,
  double-click detection, auto-save timers, file polling debouncing, etc.)
- Not implemented (by design): the `main.rs` frame loop uses real time (`std::time::Instant` and
  `crossterm::event::poll`), because terminal polling is inherently wall-clock driven

## Migration Strategy

Most migration steps are complete. Remaining work is to keep new time-based features using
`TimeSource` rather than introducing ad-hoc `Instant::now()` usage in test-sensitive code.

## Special Cases

### Signal Handler
The sleep in `signal_handler.rs` (line 114) should remain as real `thread::sleep` because:
- It runs in a signal handler context
- It needs real wall-clock time for thread backtrace capture
- Tests don't typically exercise signal handlers

### Frame Timing
The frame timing in `main.rs` currently uses `std::time::Instant`. This is acceptable because:
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
use fresh::services::time_source::RealTimeSource;

let time_source = RealTimeSource::shared();
// Editor construction defaults to RealTimeSource when not supplied explicitly.
// When you want to override (tests, embedding), pass `time_source` through the Editor constructor.
```

### Test Usage
```rust
use fresh::services::time_source::TestTimeSource;
use std::time::Duration;

let time_source = TestTimeSource::shared();

// Advance time by 5 seconds (instant, no actual waiting)
time_source.advance(Duration::from_secs(5));
// Check that time-based behavior occurred
assert!(harness.auto_save_triggered());
```
