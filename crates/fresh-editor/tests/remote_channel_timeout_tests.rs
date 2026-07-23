//! Tests for remote channel timeout, disconnect, and reconnection behavior
//!
//! These tests verify that the AgentChannel:
//! - Does not hang forever when the remote server stops responding
//! - Transitions to disconnected state after timeout
//! - Fails fast when already disconnected
//! - Reconnects when a new transport is provided via replace_transport()

use fresh::services::remote::{
    spawn_local_agent_transport, spawn_reconnect_task_with, AgentChannel, AgentResponse,
    ReconnectConfig,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command as TokioCommand;

/// Short timeout used *only* for assertions whose contract is "this
/// request should hit the channel-level timeout."  Two seconds is
/// long enough to absorb scheduler jitter on slow CI runners while
/// still keeping these tests cheap when they fire as intended.
const TIMEOUT_FOR_INTENTIONAL_TIMEOUT: Duration = Duration::from_secs(2);

/// "Effectively infinite" timeout used before any happy-path
/// `request should succeed` assertion.  Honors CONTRIBUTING.md rule
/// #3 ("Wait indefinitely, don't put timeouts inside tests"):
/// load-spike pauses on CI must not flip a should-succeed call into
/// a spurious `Err(Timeout)`.  cargo nextest's own per-test cap
/// catches the case where the channel genuinely hangs.
///
/// One hour is large enough to be "infinity" for unit tests and
/// safely fits in `u64` milliseconds (the field type used by
/// `set_request_timeout`).
const TIMEOUT_FOR_HAPPY_PATH: Duration = Duration::from_secs(3600);

/// Re-arm the channel for an intentional-timeout assertion.
fn arm_intentional_timeout(channel: &AgentChannel) {
    channel.set_request_timeout(TIMEOUT_FOR_INTENTIONAL_TIMEOUT);
}

/// Re-arm the channel for a happy-path `should succeed` assertion.
fn arm_happy_path(channel: &AgentChannel) {
    channel.set_request_timeout(TIMEOUT_FOR_HAPPY_PATH);
}

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

    arm_intentional_timeout(&channel);

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

    // First request should succeed
    arm_happy_path(&channel);
    let result1 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(
        result1.is_ok(),
        "First request should succeed: {:?}",
        result1
    );

    // Second request: server is now silent. Should timeout, not hang.
    arm_intentional_timeout(&channel);
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

    arm_intentional_timeout(&channel);

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

    // First: get into disconnected state via timeout
    arm_intentional_timeout(&channel);
    let _ = channel.request_blocking("stat", serde_json::json!({"path": "/"}));

    // Now: subsequent requests should fail immediately, regardless of
    // the channel's current timeout setting (the request short-circuits
    // on `is_connected()`).  Re-arm to the happy-path value to prove
    // the fail-fast path is independent of timeout duration.
    arm_happy_path(&channel);
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

    // First request works
    arm_happy_path(&channel);
    let r1 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(r1.is_ok(), "First request should succeed: {:?}", r1);

    // Second request times out (agent is now silent)
    arm_intentional_timeout(&channel);
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
    arm_happy_path(&channel);
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

    for round in 1..=3 {
        // Request works
        arm_happy_path(&channel);
        let r = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
        assert!(r.is_ok(), "Round {round}: request should succeed: {:?}", r);

        // Times out (agent answered one request, now silent)
        arm_intentional_timeout(&channel);
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

/// Spawn an agent that, per request, streams several data chunks slowly — each
/// gap well under the request timeout, but the *total* transfer well over it —
/// then sends the final result. Models a large file read over a bandwidth-
/// throttled link (each chunk arrives steadily; the whole download takes
/// minutes).
async fn spawn_slow_streaming_agent(chunks: u32, gap_secs: f64) -> Option<Arc<AgentChannel>> {
    let script = format!(
        r#"
import sys, json, time
sys.stdout.write(json.dumps({{"id": 0, "ok": True, "v": 1}}) + "\n")
sys.stdout.flush()
for line in sys.stdin:
    req = json.loads(line)
    rid = req["id"]
    for i in range({chunks}):
        time.sleep({gap_secs})
        sys.stdout.write(json.dumps({{"id": rid, "d": "chunk-%d" % i}}) + "\n")
        sys.stdout.flush()
    sys.stdout.write(json.dumps({{"id": rid, "r": {{"ok": True}}}}) + "\n")
    sys.stdout.flush()
    break
for line in sys.stdin:
    pass
"#
    );

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

    Some(Arc::new(AgentChannel::new(reader, stdin)))
}

/// Test: a streaming read that makes steady progress must NOT be killed just
/// because its *total* duration exceeds the request timeout. The timeout is an
/// idle (no-progress) deadline, reset on each chunk.
///
/// Regression: `request_with_data` previously wrapped the entire chunk
/// collection in a single `timeout(total)`, so a healthy-but-slow read over a
/// throttled link (a 390 KB file at 2 KB/s ≈ 3 min) was aborted at the first
/// deadline with `Request timed out` — and the connection was falsely marked
/// dead. Here: 5 chunks 0.6 s apart (total 3 s) under a 2 s timeout. With the
/// bug this fails (total 3 s > 2 s); with the fix it succeeds (each gap < 2 s).
#[test]
fn test_slow_streaming_read_survives_when_total_exceeds_timeout() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_slow_streaming_agent(5, 0.6)) else {
        eprintln!("Skipping test: could not spawn slow streaming agent");
        return;
    };

    channel.set_request_timeout(Duration::from_secs(2));

    let result = channel.request_with_data_blocking("read", serde_json::json!({"path": "/big"}));

    let (data, _final) =
        result.expect("slow-but-steady streaming read should succeed, not time out");
    assert_eq!(data.len(), 5, "all streamed chunks should be collected");
    assert!(
        channel.is_connected(),
        "a healthy streaming read must not mark the connection dead"
    );
}

/// Test: a streaming read that genuinely *stalls* (a gap longer than the
/// timeout with no data) is still aborted — the idle deadline must keep
/// detecting dead connections, not just slow ones.
#[test]
fn test_streaming_read_stalled_gap_still_times_out() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    // One chunk after a 5 s gap — longer than the 2 s timeout, so the very
    // first await stalls past the idle deadline.
    let Some(channel) = rt.block_on(spawn_slow_streaming_agent(1, 5.0)) else {
        eprintln!("Skipping test: could not spawn slow streaming agent");
        return;
    };

    channel.set_request_timeout(Duration::from_secs(2));

    let result = channel.request_with_data_blocking("read", serde_json::json!({"path": "/big"}));

    assert!(result.is_err(), "a stalled stream should time out");
    assert!(
        !channel.is_connected(),
        "a stalled stream should mark the connection dead"
    );
}

/// Test: spawn_reconnect_task_with automatically reconnects when the channel
/// disconnects.
///
/// Flow:
/// 1. Start with a one-shot agent (responds once, then goes silent)
/// 2. First request succeeds
/// 3. Second request times out → channel is disconnected
/// 4. The reconnect task detects the disconnect, calls the factory
/// 5. Factory spawns a healthy agent → channel reconnects
/// 6. Third request succeeds
#[test]
fn test_auto_reconnect_task() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    // Start with a one-shot agent
    let Some(channel) = rt.block_on(spawn_one_shot_agent()) else {
        eprintln!("Skipping test: could not spawn one-shot agent");
        return;
    };

    // Spawn the reconnect task with a factory that spawns healthy agents.
    // We enter the runtime context so the task can be spawned.
    let channel_clone = channel.clone();
    let _guard = rt.enter();
    let connect_fn = || async {
        let (reader, writer) = spawn_local_agent_transport().await?;
        let reader: Box<dyn tokio::io::AsyncBufRead + Unpin + Send> = Box::new(reader);
        let writer: Box<dyn tokio::io::AsyncWrite + Unpin + Send> = Box::new(writer);
        Ok((reader, writer))
    };
    let _handle = spawn_reconnect_task_with(
        channel_clone,
        connect_fn,
        ReconnectConfig {
            // Fast, non-backing-off retry for tests.
            initial_interval: Duration::from_millis(100),
            max_interval: Duration::from_millis(100),
            poll_interval: Duration::from_millis(100),
        },
        "test",
    );

    // First request works
    arm_happy_path(&channel);
    let r1 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(r1.is_ok(), "First request should succeed: {:?}", r1);

    // Second request times out (agent went silent)
    arm_intentional_timeout(&channel);
    let r2 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(r2.is_err(), "Second request should timeout");
    assert!(!channel.is_connected(), "Should be disconnected");

    // Wait for auto-reconnection (semantic wait)
    while !channel.is_connected() {
        std::thread::sleep(Duration::from_millis(50));
    }

    // Third request works on the new connection
    arm_happy_path(&channel);
    let r3 = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(
        r3.is_ok(),
        "Request after auto-reconnect should succeed: {:?}",
        r3
    );
}

/// Regression: the blocking wrappers must be safe to call from *inside* a
/// Tokio runtime — the plugin thread's situation when the Orchestrator dock
/// does a synchronous remote `read_dir` while arrowing onto an unreachable
/// SSH workspace. A plain `Handle::block_on` there panics ("Cannot start a
/// runtime from within a runtime"); the fix drives the request off the
/// current runtime thread instead.
///
/// The channel is first driven to the disconnected state so the in-runtime
/// call returns immediately (ChannelClosed) rather than depending on
/// wall-clock timing — the only thing under test is that it returns at all
/// instead of panicking.
#[test]
fn test_request_blocking_within_runtime_does_not_panic() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_silent_agent()) else {
        eprintln!("Skipping test: could not spawn silent agent");
        return;
    };

    // Drive it to disconnected (one intentional timeout) so the in-runtime
    // request below short-circuits on `is_connected()`.
    arm_intentional_timeout(&channel);
    let _ = channel.request_blocking("stat", serde_json::json!({"path": "/"}));
    assert!(
        !channel.is_connected(),
        "precondition: channel disconnected"
    );

    // Invoke the blocking API synchronously from *within* an async task —
    // exactly what the plugin thread does (its JS `read_dir` call is sync
    // Rust running inside the plugin's async execution context). A plain
    // `Handle::block_on` here panics; the fix drives it off-thread. Before
    // the fix `rt.block_on` unwinds with the runtime-in-runtime panic and
    // fails the test; now it returns the fast ChannelClosed error.
    let ch = channel.clone();
    let result = rt
        .block_on(async move { ch.request_blocking("stat", serde_json::json!({"path": "/tmp"})) });
    assert!(
        result.is_err(),
        "disconnected channel should return an error, not panic"
    );
}
