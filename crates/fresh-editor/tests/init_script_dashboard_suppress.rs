//! Regression for issue #2028: when a user's init.ts disables the
//! dashboard auto-open via the exported plugin API
//! (`getPluginApi("dashboard")?.setAutoOpen(false)`), the
//! dashboard must NOT have already been opened by the time
//! init.ts gets to run.
//!
//! Root cause was a module-level `openDashboard()` call at the
//! bottom of `plugins/dashboard.ts` that fired the instant the
//! plugin loaded — which happens during the startup plugin
//! batch, *before* init.ts is queued. The dashboard appeared
//! regardless of `setAutoOpen(false)`.
//!
//! Why the test isn't an in-process e2e: the test harness loads
//! plugins synchronously (`defer_plugin_load=false`), and the
//! plugin-state snapshot is populated *after* construction
//! returns. So at plugin-load time in tests, `listBuffers()` is
//! empty and the immediate-open path was guarded by
//! `listBuffers().length > 0`. The buggy path only fired in the
//! production async-load codepath, where the snapshot (seeded
//! with the empty buffer) was visible to dashboard.ts at
//! evaluation time. Manual repro is documented in the
//! corresponding PR description; in-tree we pin the contract by
//! source-grepping `plugins/dashboard.ts`.

use std::fs;
use std::path::PathBuf;

fn dashboard_source() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("plugins")
        .join("dashboard.ts");
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e))
}

#[test]
fn dashboard_ts_must_not_call_open_dashboard_at_module_load() {
    // Auto-open is owned by the `ready` hook handler. Any
    // module-level call to `openDashboard()` (top-level, not
    // inside a function or a hook handler) races init.ts, since
    // init.ts is queued *after* the startup plugin batch in the
    // plugin-thread FIFO.
    //
    // The handlers above this point are allowed to call it via
    // their bodies; the bad pattern is a bare top-level
    // expression-statement near the bottom of the file.
    let src = dashboard_source();

    // The contract: every call to `openDashboard(` must be
    // awaited inside an async function/handler body. The two
    // legitimate call sites in dashboard.ts at the time of
    // writing are `await openDashboard()` inside the
    // `dashboardOnReady` and `dashboardOnBufferClosed`
    // handlers. The historic bug was a synchronous, un-awaited
    // `openDashboard();` reached during module-level eval. So:
    // forbid any occurrence of `openDashboard(` that is not
    // either (a) the declaration `async function openDashboard(`
    // or (b) preceded by the keyword `await `.
    let offending: Vec<(usize, String)> = src
        .lines()
        .enumerate()
        .filter_map(|(i, line)| {
            let trimmed = line.trim_start();
            if !trimmed.contains("openDashboard(") {
                return None;
            }
            if trimmed.starts_with("//") {
                return None;
            }
            if trimmed.starts_with("async function openDashboard(")
                || trimmed.starts_with("function openDashboard(")
            {
                return None;
            }
            // Any other occurrence must be `await openDashboard(`.
            // Look for the call substring and check the chars
            // immediately preceding it.
            let pos = trimmed.find("openDashboard(").unwrap();
            let prefix = &trimmed[..pos];
            if prefix.trim_end().ends_with("await") {
                None
            } else {
                Some((i, line.to_string()))
            }
        })
        .collect();

    assert!(
        offending.is_empty(),
        "plugins/dashboard.ts must not call openDashboard() at module \
         scope — that races init.ts's setAutoOpen(false). Found:\n{}",
        offending
            .iter()
            .map(|(i, l)| format!("  line {}: {}", i + 1, l))
            .collect::<Vec<_>>()
            .join("\n")
    );
}
