//! E2E tests for the update notification UI

use crate::common::harness::EditorTestHarness;
use fresh::services::release_checker::{
    start_periodic_update_check_with_interval, CURRENT_VERSION,
};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

/// Test helper: start a local HTTP server that returns a mock release JSON
/// Returns (stop_sender, url) - send to stop_sender to shut down the server
fn start_mock_release_server(version: &str) -> (mpsc::Sender<()>, String) {
    let server = tiny_http::Server::http("127.0.0.1:0").expect("Failed to start test server");
    let port = server.server_addr().to_ip().unwrap().port();
    let url = format!("http://127.0.0.1:{}/releases/latest", port);

    let (stop_tx, stop_rx) = mpsc::channel::<()>();

    let version = version.to_string();
    thread::spawn(move || loop {
        if stop_rx.try_recv().is_ok() {
            break;
        }

        match server.recv_timeout(Duration::from_millis(100)) {
            Ok(Some(request)) => {
                let response_body = format!(r#"{{"tag_name": "v{}"}}"#, version);
                let response = tiny_http::Response::from_string(response_body).with_header(
                    tiny_http::Header::from_bytes(&b"Content-Type"[..], &b"application/json"[..])
                        .unwrap(),
                );
                let _ = request.respond(response);
            }
            Ok(None) => {}
            Err(_) => break,
        }
    });

    (stop_tx, url)
}

/// Compute a version string that is one patch version higher than the current
fn next_patch_version() -> String {
    let parts: Vec<&str> = CURRENT_VERSION.split('.').collect();
    if parts.len() >= 3 {
        let major = parts[0];
        let minor = parts[1];
        let patch: u32 = parts[2]
            .split('-')
            .next()
            .unwrap_or("0")
            .parse()
            .unwrap_or(0);
        format!("{}.{}.{}", major, minor, patch + 1)
    } else {
        // Fallback: just return a high version
        "99.0.0".to_string()
    }
}

#[test]
fn test_update_notification_appears_in_status_bar() {
    // Start a mock server that returns a version higher than current
    let next_version = next_patch_version();
    let (stop_tx, url) = start_mock_release_server(&next_version);

    // Create a test harness with enough width to show the update notification
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Create an update checker pointing to our mock server
    let checker = start_periodic_update_check_with_interval(&url, Duration::from_secs(3600));

    // Inject the checker into the editor
    harness.editor_mut().set_update_checker(checker);

    // Wait for the update check to complete and result to be available
    let start = Instant::now();
    let timeout = Duration::from_secs(5);
    let mut found_update = false;

    while start.elapsed() < timeout {
        harness.process_async_and_render().unwrap();

        if harness.editor().is_update_available() {
            found_update = true;
            break;
        }
        harness.sleep(Duration::from_millis(50));
    }

    assert!(found_update, "Update check did not complete within timeout");

    // Render and check the status bar contains the update notification
    harness.render().unwrap();
    let status_bar = harness.get_status_bar();

    // The status bar should contain "Update:" and the version
    assert!(
        status_bar.contains("Update:"),
        "Status bar should contain 'Update:' indicator. Status bar: '{}'",
        status_bar
    );
    assert!(
        status_bar.contains(&next_version),
        "Status bar should contain version '{}'. Status bar: '{}'",
        next_version,
        status_bar
    );

    // Clean up
    drop(harness);
    let _ = stop_tx.send(());
}

#[test]
fn test_update_notification_not_shown_when_current() {
    // Start a mock server that returns the current version (no update)
    let (stop_tx, url) = start_mock_release_server(CURRENT_VERSION);

    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    let checker = start_periodic_update_check_with_interval(&url, Duration::from_secs(3600));
    harness.editor_mut().set_update_checker(checker);

    // Wait for the update check to complete
    let start = Instant::now();
    let timeout = Duration::from_secs(5);

    while start.elapsed() < timeout {
        harness.process_async_and_render().unwrap();

        // Check if we have a cached result (check completed)
        if harness.editor().get_update_result().is_some() {
            break;
        }
        harness.sleep(Duration::from_millis(50));
    }

    // Should NOT show update notification
    assert!(
        !harness.editor().is_update_available(),
        "Should not show update available when version is current"
    );

    harness.render().unwrap();
    let status_bar = harness.get_status_bar();

    // Status bar should NOT contain "Update:"
    assert!(
        !status_bar.contains("Update:"),
        "Status bar should NOT contain 'Update:' when version is current. Status bar: '{}'",
        status_bar
    );

    drop(harness);
    let _ = stop_tx.send(());
}

#[test]
fn test_update_notification_positioned_near_ctrl_p() {
    // Start a mock server with a newer version
    let next_version = next_patch_version();
    let (stop_tx, url) = start_mock_release_server(&next_version);

    let mut harness = EditorTestHarness::new(120, 24).unwrap();

    let checker = start_periodic_update_check_with_interval(&url, Duration::from_secs(3600));
    harness.editor_mut().set_update_checker(checker);

    // Wait for update check
    let start = Instant::now();
    while start.elapsed() < Duration::from_secs(5) {
        harness.process_async_and_render().unwrap();
        if harness.editor().is_update_available() {
            break;
        }
        harness.sleep(Duration::from_millis(50));
    }

    harness.render().unwrap();
    let status_bar = harness.get_status_bar();

    // Both "Update:" and "Ctrl+P" should be present
    let has_update = status_bar.contains("Update:");
    let has_ctrl_p = status_bar.contains("Ctrl+P");

    assert!(
        has_update && has_ctrl_p,
        "Status bar should contain both update notification and Ctrl+P hint. Status bar: '{}'",
        status_bar
    );

    // The update indicator should appear before Ctrl+P (to its left)
    if let (Some(update_pos), Some(ctrl_p_pos)) =
        (status_bar.find("Update:"), status_bar.find("Ctrl+P"))
    {
        assert!(
            update_pos < ctrl_p_pos,
            "Update notification should appear before (left of) Ctrl+P. \
             Update at {}, Ctrl+P at {}. Status bar: '{}'",
            update_pos,
            ctrl_p_pos,
            status_bar
        );
    }

    drop(harness);
    let _ = stop_tx.send(());
}
