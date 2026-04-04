//! Tests for remote channel timeout, disconnect, and reconnection behavior
//!
//! These tests verify that the AgentChannel:
//! - Does not hang forever when the remote server stops responding
//! - Transitions to disconnected state after timeout
//! - Fails fast when already disconnected
//! - Reconnects when a new transport is provided via replace_transport()

use fresh::services::remote::{spawn_local_agent_transport, AgentChannel, AgentResponse};
use std::sync::Arc;
use std::time::Duration;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command as TokioCommand;

/// Short timeout for tests so they complete quickly.
const TEST_TIMEOUT: Duration = Duration::from_secs(2);

/// Spawn a Python script that sends a ready message then never responds to requests.
/// The script reads stdin (so it doesn't die from SIGPIPE) but never writes back.
async fn spawn_silent_agent() -> Option<Arc<AgentChannel>> {
    let script = r#"
import sys, json
# Send ready message
sys.stdout.write(json.dumps({"id": 0, "ok": True, "v": 1}) + "\n")
sys.stdout.flush()
# Read stdin forever but never respond
for line in sys.stdin:
    pass
"#;

    let mut child = TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(script)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;

    let stdin = child.stdin.take()?;
    let stdout = child.stdout.take()?;
    let mut reader = BufReader::new(stdout);

    // Read ready message
    let mut ready_line = String::new();
    reader.read_line(&mut ready_line).await.ok()?;
    let ready: AgentResponse = serde_json::from_str(&ready_line).ok()?;
    if !ready.is_ready() {
        return None;
    }

    Some(Arc::new(AgentChannel::new(reader, stdin)))
}

/// Spawn a Python script that responds to the first request, then goes silent.
async fn spawn_one_shot_agent() -> Option<Arc<AgentChannel>> {
    let script = r#"
import sys, json
# Send ready message
sys.stdout.write(json.dumps({"id": 0, "ok": True, "v": 1}) + "\n")
sys.stdout.flush()
# Respond to exactly one request, then go silent
for line in sys.stdin:
    req = json.loads(line)
    req_id = req["id"]
    # Respond with a dummy stat result
    sys.stdout.write(json.dumps({"id": req_id, "r": {"size": 0, "mtime": 0, "mode": 0, "uid": 0, "gid": 0, "dir": False, "file": True, "link": False}}) + "\n")
    sys.stdout.flush()
    break
# Now read stdin forever but never respond
for line in sys.stdin:
    pass
"#;

    let mut child = TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(script)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;

    let stdin = child.stdin.take()?;
    let stdout = child.stdout.take()?;
    let mut reader = BufReader::new(stdout);

    // Read ready message
    let mut ready_line = String::new();
    reader.read_line(&mut ready_line).await.ok()?;
    let ready: AgentResponse = serde_json::from_str(&ready_line).ok()?;
    if !ready.is_ready() {
        return None;
    }

    Some(Arc::new(AgentChannel::new(reader, stdin)))
}

/// Test: A request to a server that never responds should not hang forever.
///
/// BUG: Currently hangs because `request()` awaits `result_rx` with no timeout.
/// After the fix, this should return a timeout error within a bounded duration.
#[test]
fn test_request_to_silent_server_does_not_hang() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_silent_agent()) else {
        eprintln!("Skipping test: could not spawn silent agent");
        return;
    };

    channel.set_request_timeout(TEST_TIMEOUT);

    // This should return an error (timeout), not hang forever.
    let result = channel.request_blocking("stat", serde_json::json!({"path": "/"}));

    assert!(
        result.is_err(),
        "Expected timeout error, got success: {:?}",
        result
    );
}

/// Test: After one successful request, a second request to a now-silent server
/// should not hang forever.
///
/// BUG: Currently hangs on the second request.
#[test]
fn test_second_request_hangs_after_server_goes_silent() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_one_shot_agent()) else {
        eprintln!("Skipping test: could not spawn one-shot agent");
        return;
    };

    channel.set_request_timeout(TEST_TIMEOUT);

    // First request should succeed
    let result1 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(
        result1.is_ok(),
        "First request should succeed: {:?}",
        result1
    );

    // Second request: server is now silent. Should timeout, not hang.
    let result2 = channel.request_blocking("stat", serde_json::json!({"path": "/tmp"}));
    assert!(
        result2.is_err(),
        "Expected timeout error on second request, got success: {:?}",
        result2
    );
}

/// Test: After a request times out, is_connected() should return false.
///
/// This validates the state transition: timeout → disconnected.
#[test]
fn test_connection_marked_disconnected_after_timeout() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_silent_agent()) else {
        eprintln!("Skipping test: could not spawn silent agent");
        return;
    };

    channel.set_request_timeout(TEST_TIMEOUT);

    assert!(channel.is_connected(), "Should start connected");

    // This request should timeout
    let _ = channel.request_blocking("stat", serde_json::json!({"path": "/"}));

    assert!(
        !channel.is_connected(),
        "Should be disconnected after timeout"
    );
}

/// Test: Once disconnected, subsequent requests should fail immediately
/// (ChannelClosed error) without waiting for a timeout.
///
/// This test doesn't hang today IF we can get the channel into disconnected
/// state — but without timeouts, we can't get there from a silent server.
/// So this test also hangs on the first request.
#[test]
fn test_requests_fail_fast_when_disconnected() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_silent_agent()) else {
        eprintln!("Skipping test: could not spawn silent agent");
        return;
    };

    channel.set_request_timeout(TEST_TIMEOUT);

    // First: get into disconnected state via timeout
    let _ = channel.request_blocking("stat", serde_json::json!({"path": "/"}));

    // Now: subsequent requests should fail immediately
    let start = std::time::Instant::now();
    let result = channel.request_blocking("stat", serde_json::json!({"path": "/tmp"}));
    let elapsed = start.elapsed();

    assert!(result.is_err(), "Should fail when disconnected");
    assert!(
        elapsed < std::time::Duration::from_millis(100),
        "Should fail fast (took {:?}), not wait for timeout",
        elapsed
    );
}

/// Test: After a connection drops and a new transport is provided via
/// replace_transport(), the channel reconnects and requests work again.
///
/// Flow:
/// 1. Start with a one-shot agent (responds once, then goes silent)
/// 2. First request succeeds
/// 3. Second request times out → channel is disconnected
/// 4. Test spawns a healthy agent and calls replace_transport()
/// 5. Channel reconnects — is_connected() returns true
/// 6. Third request succeeds on the new connection
#[test]
fn test_reconnection_via_replace_transport() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    // Start with a one-shot agent
    let Some(channel) = rt.block_on(spawn_one_shot_agent()) else {
        eprintln!("Skipping test: could not spawn one-shot agent");
        return;
    };

    channel.set_request_timeout(TEST_TIMEOUT);

    // First request works
    let r1 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(r1.is_ok(), "First request should succeed: {:?}", r1);

    // Second request times out (agent is now silent)
    let r2 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(r2.is_err(), "Second request should timeout");
    assert!(!channel.is_connected(), "Should be disconnected");

    // Spawn a healthy agent and reconnect
    let (new_reader, new_writer) = rt
        .block_on(spawn_local_agent_transport())
        .expect("Failed to spawn replacement agent");

    // replace_transport_blocking waits until the channel is connected
    channel.replace_transport_blocking(new_reader, new_writer);

    // Third request works on the new connection
    let r3 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(
        r3.is_ok(),
        "Request after reconnection should succeed: {:?}",
        r3
    );
}

/// Test: Multiple reconnections work (disconnect → reconnect → disconnect → reconnect).
#[test]
fn test_multiple_reconnections() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    // Start with a one-shot agent
    let Some(channel) = rt.block_on(spawn_one_shot_agent()) else {
        eprintln!("Skipping test: could not spawn one-shot agent");
        return;
    };

    channel.set_request_timeout(TEST_TIMEOUT);

    for round in 1..=3 {
        // Request works
        let r = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
        assert!(r.is_ok(), "Round {round}: request should succeed: {:?}", r);

        // Times out (agent answered one request, now silent)
        let r = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
        assert!(r.is_err(), "Round {round}: should timeout");
        assert!(
            !channel.is_connected(),
            "Round {round}: should be disconnected"
        );

        // Reconnect with a fresh one-shot agent
        // (We use spawn_one_shot_agent's script directly to get raw transport)
        let (new_reader, new_writer) = rt
            .block_on(spawn_one_shot_transport())
            .expect("Failed to spawn replacement agent");

        channel.replace_transport_blocking(new_reader, new_writer);
    }
}

/// Spawn a one-shot agent and return raw transport (responds once, then silent).
async fn spawn_one_shot_transport() -> Option<(
    BufReader<tokio::process::ChildStdout>,
    tokio::process::ChildStdin,
)> {
    let script = r#"
import sys, json
sys.stdout.write(json.dumps({"id": 0, "ok": True, "v": 1}) + "\n")
sys.stdout.flush()
for line in sys.stdin:
    req = json.loads(line)
    req_id = req["id"]
    sys.stdout.write(json.dumps({"id": req_id, "r": {"size": 0, "mtime": 0, "mode": 0, "uid": 0, "gid": 0, "dir": False, "file": True, "link": False}}) + "\n")
    sys.stdout.flush()
    break
for line in sys.stdin:
    pass
"#;

    let mut child = TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(script)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;

    let stdin = child.stdin.take()?;
    let stdout = child.stdout.take()?;
    let mut reader = BufReader::new(stdout);

    let mut ready_line = String::new();
    reader.read_line(&mut ready_line).await.ok()?;
    let ready: AgentResponse = serde_json::from_str(&ready_line).ok()?;
    if !ready.is_ready() {
        return None;
    }

    Some((reader, stdin))
}
