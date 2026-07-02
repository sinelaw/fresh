//! E2E tests for the status-bar Remote Indicator's *rendered label* around a
//! failed reconnect.
//!
//! A failed reconnect records the SSH failure reason on the active window
//! (`remote_reconnect_error`). The indicator used to inline that whole sentence
//! (`Reconnect failed: Agent failed to start: SSH could not connect to …`),
//! which swamped the status bar. It must now render a short "Disconnected"
//! instead — the full reason is emitted as a `tracing::warn!` and surfaced in
//! the remote-indicator popup, not the bar.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::{Config, StatusBarConfig, StatusBarElement};

/// The multi-line-long SSH failure a failed reconnect records — the kind of
/// string that used to fill the status bar end to end.
const LONG_RECONNECT_ERROR: &str = "Agent failed to start: SSH could not connect to \
     root@localhost:2222. Check that the host is reachable, the hostname is correct, \
     and your SSH credentials are valid (exit code 255)";

#[test]
fn test_failed_reconnect_indicator_is_short_disconnected() {
    // A status bar that includes the {remote} indicator on the left.
    let mut config = Config::default();
    config.editor.status_bar = StatusBarConfig {
        left: vec![
            StatusBarElement::RemoteIndicator,
            StatusBarElement::Filename,
        ],
        right: vec![],
        ..StatusBarConfig::default()
    };

    let mut harness =
        EditorTestHarness::create(160, 30, HarnessOptions::new().with_config(config)).unwrap();

    // Simulate the state a failed reconnect leaves behind: the reason recorded
    // on the active window. (The warning-log side is exercised by the reconnect
    // dispatch itself; here we lock in the *rendered* indicator.)
    harness
        .editor_mut()
        .active_window_mut()
        .remote_reconnect_error = Some(LONG_RECONNECT_ERROR.to_string());
    harness.render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("Disconnected"),
        "Failed-reconnect indicator should read 'Disconnected'.\nStatus bar: {status}"
    );
    assert!(
        !status.contains("Reconnect failed") && !status.contains("Agent failed to start"),
        "Failed-reconnect indicator must not inline the long SSH error.\nStatus bar: {status}"
    );
}
