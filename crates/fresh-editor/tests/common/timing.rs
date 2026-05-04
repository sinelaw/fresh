//! Lightweight phase-timing helper for understanding why semantic tests
//! are slow.
//!
//! Activate by setting `FRESH_TEST_TIMING=1`. When unset, every method
//! is a no-op and there is no overhead.
//!
//! Usage:
//! ```ignore
//! let mut t = Timer::start("buffer_scenario: my_test");
//! // ... do work ...
//! t.phase("harness_create");
//! // ... do work ...
//! t.phase("load_buffer");
//! // ... do work ...
//! t.finish();
//! ```
//!
//! Output (one line per phase + summary), printed to stderr:
//! ```text
//! [timing] buffer_scenario: my_test  start
//! [timing]   harness_create        +1480.3ms  (cumul 1480.3ms)
//! [timing]   load_buffer            +152.7ms  (cumul 1633.0ms)
//! [timing]   dispatch_actions         +0.4ms  (cumul 1633.4ms)
//! [timing]   assertions               +1.1ms  (cumul 1634.5ms)
//! [timing] buffer_scenario: my_test  total 1634.5ms
//! ```

use std::time::Instant;

pub fn enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED
        .get_or_init(|| std::env::var("FRESH_TEST_TIMING").is_ok_and(|v| !v.is_empty() && v != "0"))
}

pub struct Timer {
    label: String,
    start: Instant,
    last: Instant,
    indent: &'static str,
}

impl Timer {
    pub fn start(label: impl Into<String>) -> Self {
        let label = label.into();
        let now = Instant::now();
        if enabled() {
            eprintln!("[timing] {label}  start");
        }
        Self {
            label,
            start: now,
            last: now,
            indent: "  ",
        }
    }

    /// Mark a phase boundary. The reported delta is the time since the
    /// previous `phase` call (or the timer's start, for the first call).
    pub fn phase(&mut self, name: &str) {
        if !enabled() {
            return;
        }
        let now = Instant::now();
        let delta = now.duration_since(self.last);
        let cumul = now.duration_since(self.start);
        eprintln!(
            "[timing] {indent}{name:<24} +{delta:>8.1}ms  (cumul {cumul:.1}ms)",
            indent = self.indent,
            name = name,
            delta = delta.as_secs_f64() * 1000.0,
            cumul = cumul.as_secs_f64() * 1000.0,
        );
        self.last = now;
    }

    pub fn finish(self) {
        if !enabled() {
            return;
        }
        let total = self.start.elapsed();
        eprintln!(
            "[timing] {label}  total {total:.1}ms",
            label = self.label,
            total = total.as_secs_f64() * 1000.0,
        );
    }
}

/// Convenience: time a single closure and report it as one phase.
pub fn time_phase<R>(timer: &mut Timer, name: &str, f: impl FnOnce() -> R) -> R {
    let r = f();
    timer.phase(name);
    r
}
