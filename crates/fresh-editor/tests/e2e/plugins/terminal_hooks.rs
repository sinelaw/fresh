//! E2E tests for the `terminal_output` and `terminal_exit` plugin hooks.
//!
//! These hooks are the smallest core change in the Conductor design
//! (`docs/internal/conductor-sessions-design.md`). They let plugins
//! observe PTY lifecycle without a separate readback API: every PTY
//! data batch fires `terminal_output` with a snapshot of the cursor
//! row's text, and every PTY-process exit fires `terminal_exit` with
//! the (currently always `None`) exit code.

use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use portable_pty::{native_pty_system, PtySize};
use std::time::Duration;

fn harness_or_skip(width: u16, height: u16) -> Option<EditorTestHarness> {
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal_hooks test: PTY unavailable in this environment");
        return None;
    }
    EditorTestHarness::with_temp_project(width, height).ok()
}

/// Load a plugin file via the "Load Plugin from Buffer" palette action.
/// Mirrors `load_from_buffer.rs`.
fn load_plugin(harness: &mut EditorTestHarness, source: &str, file_name: &str) {
    let project_dir = harness.project_dir().unwrap();
    let plugin_file = project_dir.join(file_name);
    std::fs::write(&plugin_file, source).unwrap();
    harness.open_file(&plugin_file).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Load Plugin from Buffer").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness.assert_no_plugin_errors();
}

/// Pump async + render until `cond` is true or `max_iters` elapse.
/// Returns `true` when the condition fired, `false` on timeout — caller
/// asserts so the failure message can include the rendered screen.
fn pump_until(
    harness: &mut EditorTestHarness,
    max_iters: usize,
    mut cond: impl FnMut(&EditorTestHarness) -> bool,
) -> bool {
    for _ in 0..max_iters {
        harness.process_async_and_render().unwrap();
        if cond(harness) {
            return true;
        }
        harness.sleep(Duration::from_millis(50));
    }
    cond(harness)
}

/// `terminal_output` fires with a `last_line` payload when a PTY
/// produces data. The plugin sets a status message of the form
/// `term-out:<last_line>` so we can observe the hook end-to-end via
/// the rendered status bar.
#[test]
fn test_terminal_output_hook_fires_with_last_line() {
    init_tracing_from_env();
    let Some(mut harness) = harness_or_skip(120, 24) else {
        return;
    };

    let plugin = r#"
const editor = getEditor();
editor.setStatus("plugin:ready");
editor.on("terminal_output", function(payload) {
    // Status bars are repainted every render — write a tagged
    // string the test asserts on. Truncate so it fits on screen.
    const last = String((payload && payload.last_line) || "");
    editor.setStatus("term-out:" + last.slice(-40));
});
"#;
    load_plugin(&mut harness, plugin, "terminal_output_observer.ts");

    // The plugin's load message ("plugin:ready") should be visible
    // before any terminal output, so we know setStatus is reaching
    // the rendered status bar.
    assert!(
        pump_until(&mut harness, 20, |h| h
            .screen_to_string()
            .contains("plugin:ready")),
        "plugin:ready never reached the status bar; screen:\n{}",
        harness.screen_to_string()
    );

    // Spawn a terminal — the spawned shell will at minimum emit a
    // prompt, which fires at least one `terminal_output` event.
    harness.editor_mut().open_terminal();

    // Wait for the hook to fire (overwriting the status with
    // `term-out:`). Don't assert on the last_line content because
    // the user's shell prompt is environment-dependent.
    assert!(
        pump_until(&mut harness, 80, |h| h
            .screen_to_string()
            .contains("term-out:")),
        "terminal_output hook did not update status; screen:\n{}",
        harness.screen_to_string()
    );

    harness.assert_no_plugin_errors();
}

/// `terminal_exit` fires once when the PTY process ends, with the
/// real exit code captured from `child.wait()`. The plugin records
/// the event by calling `setStatus("term-exit:<code>")`. Closing
/// the terminal via the editor's `close_terminal` action signals
/// the PTY to shut down — the killer sends SIGKILL/equivalent and
/// the wait-thread reaps the status.
///
/// We don't assert on a *specific* numeric code because PTY
/// shutdown semantics differ across platforms (SIGHUP, SIGTERM,
/// shell-specific exit codes). The contract is: the plugin sees
/// a *concrete* number, not the placeholder "none".
#[test]
fn test_terminal_exit_hook_fires_on_close() {
    init_tracing_from_env();
    let Some(mut harness) = harness_or_skip(120, 24) else {
        return;
    };

    let plugin = r#"
const editor = getEditor();
editor.setStatus("plugin:ready");
editor.on("terminal_exit", function(payload) {
    const code = payload && payload.exit_code;
    const codeStr = (code === null || code === undefined) ? "none" : String(code);
    editor.setStatus("term-exit:" + codeStr);
});
"#;
    load_plugin(&mut harness, plugin, "terminal_exit_observer.ts");

    assert!(
        pump_until(&mut harness, 20, |h| h
            .screen_to_string()
            .contains("plugin:ready")),
        "plugin:ready never reached the status bar; screen:\n{}",
        harness.screen_to_string()
    );

    harness.editor_mut().open_terminal();
    // Let the terminal actually start (so close has something to close).
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    harness.editor_mut().close_terminal();

    assert!(
        pump_until(&mut harness, 80, |h| h
            .screen_to_string()
            .contains("term-exit:")),
        "terminal_exit hook did not fire after close; screen:\n{}",
        harness.screen_to_string()
    );

    // Now the wait-thread carries the real exit code through, so
    // "term-exit:none" indicates a regression in `child.wait()`
    // capture. Any concrete number is acceptable — PTY shutdown
    // semantics vary by platform.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("term-exit:none"),
        "terminal_exit fired but exit_code was None — the wait-thread \
         capture regressed; screen:\n{screen}"
    );

    harness.assert_no_plugin_errors();
}
