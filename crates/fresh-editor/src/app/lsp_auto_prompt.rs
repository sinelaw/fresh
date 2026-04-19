//! Per-process default for the "auto-prompt on first open" LSP popup.
//!
//! Normally the editor surfaces the LSP status popup the first time
//! the user opens a file for a language that has an `enabled +
//! auto_start = false` server configured (see
//! `notify_lsp_file_opened` in `file_operations.rs`). That behaviour
//! is great for users but actively hostile to e2e tests: the popup
//! is shown on the first render after `open_file`, and from that
//! point on any `send_key` call targets the popup's input handler
//! rather than the buffer. Hundreds of tests that happen to open
//! `.rs` files (or any language with a default LSP config) suddenly
//! see their keystrokes swallowed.
//!
//! The default here is read once per `Editor` in its constructor
//! and stored as a per-editor flag — so parallel tests can't race
//! each other's toggles. The test-harness ctor flips the default to
//! `false`; tests that specifically exercise the auto-prompt pass
//! `HarnessOptions::with_lsp_auto_prompt(true)` to override on
//! their own editor instance.

use std::sync::atomic::{AtomicBool, Ordering};

/// Process-wide default read by `Editor::new` at construction time.
/// Real editor sessions leave this at its initial `true`; tests
/// flip it to `false` in their ctor (see `tests/common/harness.rs`).
static DEFAULT_ENABLED: AtomicBool = AtomicBool::new(true);

/// Snapshot the current default. Called once per Editor so each
/// editor instance is born with a stable flag that can't be
/// changed under it by a concurrently-running test.
pub fn default_enabled() -> bool {
    DEFAULT_ENABLED.load(Ordering::Relaxed)
}

/// Override the process-wide default. Expected to be called from
/// the test-harness ctor (before any `Editor` is constructed) or
/// from per-test setup when the default needs flipping.
pub fn set_default_enabled(enabled: bool) {
    DEFAULT_ENABLED.store(enabled, Ordering::Relaxed);
}
