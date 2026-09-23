//! Tests for remote channel timeout, disconnect, and reconnection behavior
//!
//! These tests verify that the AgentChannel:
//! - Does not hang forever when the remote server stops responding
//! - Transitions to disconnected state after timeout
//! - Fails fast when already disconnected
//! - Reconnects when a new transport is provided via replace_transport()

use fresh::services::remote::{
    spawn_local_agent_transport, spawn_reconnect_task_with, AgentChannel, AgentResponse, Carrier,
    ChannelError, ReconnectConfig,
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
        // The local test agent is not kill-on-drop, so there is no process to hold.
        Ok(Carrier {
            reader: Box::new(reader),
            writer: Box::new(writer),
            process: None,
        })
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

/// Whether `pid` is a live, non-zombie process. `kill(pid, 0)` succeeds on a
/// zombie, so read the `State:` line from procfs instead.
#[cfg(target_os = "linux")]
fn process_is_running(pid: u32) -> bool {
    std::fs::read_to_string(format!("/proc/{pid}/status"))
        .map(|status| {
            status
                .lines()
                .find_map(|l| l.strip_prefix("State:"))
                .map(|state| !state.trim_start().starts_with('Z'))
                .unwrap_or(false)
        })
        .unwrap_or(false)
}

/// A carrier that comes up on reconnect must stay up: the task holds its
/// process while that transport is installed. A real kill-on-drop process
/// stands in for the carrier: alive after reconnect, dead after abort.
#[cfg(target_os = "linux")]
#[test]
fn test_reconnect_task_holds_the_carrier_process() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_one_shot_agent()) else {
        eprintln!("Skipping test: could not spawn one-shot agent");
        return;
    };

    // The stand-in carrier, handed to the factory through a slot it takes once.
    let _guard = rt.enter();
    let mut stand_in = TokioCommand::new("sleep");
    stand_in.arg("600").kill_on_drop(true);
    let stand_in = stand_in.spawn().expect("spawn sleep");
    let pid = stand_in.id().expect("child has a pid");
    let slot = Arc::new(std::sync::Mutex::new(Some(stand_in)));
    assert!(process_is_running(pid), "stand-in carrier starts running");

    let taken = Arc::clone(&slot);
    let connect_fn = move || {
        let taken = Arc::clone(&taken);
        async move {
            let (reader, writer) = spawn_local_agent_transport().await?;
            Ok(Carrier {
                reader: Box::new(reader),
                writer: Box::new(writer),
                process: taken.lock().unwrap().take(),
            })
        }
    };
    let handle = spawn_reconnect_task_with(
        channel.clone(),
        connect_fn,
        ReconnectConfig {
            initial_interval: Duration::from_millis(100),
            max_interval: Duration::from_millis(100),
            poll_interval: Duration::from_millis(100),
        },
        "test",
    );

    // Use up the one-shot agent, then time out on it: disconnected.
    arm_happy_path(&channel);
    channel
        .request_blocking("stat", serde_json::json!({"path": "/"}))
        .expect("first request succeeds");
    arm_intentional_timeout(&channel);
    assert!(channel
        .request_blocking("stat", serde_json::json!({"path": "/"}))
        .is_err());
    while !channel.is_connected() {
        std::thread::sleep(Duration::from_millis(50));
    }

    assert!(
        slot.lock().unwrap().is_none(),
        "the factory handed its process to the task"
    );
    std::thread::sleep(Duration::from_millis(200));
    assert!(
        process_is_running(pid),
        "the carrier installed by the reconnect must outlive the reconnect"
    );
    arm_happy_path(&channel);
    channel
        .request_blocking("stat", serde_json::json!({"path": "/"}))
        .expect("request after reconnect succeeds");

    handle.abort();
    let deadline = std::time::Instant::now() + Duration::from_secs(5);
    while process_is_running(pid) && std::time::Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(50));
    }
    assert!(
        !process_is_running(pid),
        "aborting the reconnect task must take the carrier it holds down"
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

/// Build a channel over an in-memory duplex transport whose read/write tasks
/// ride on `transport_rt`, and hand back the far end of the pipe so a test can
/// observe what the channel actually sent.
///
/// Deliberately not a child process: the test needs to know *exactly* when a
/// request has reached the far end, and reading it off the pipe is the only
/// way to know that without sleeping.
fn duplex_channel_on(
    transport_rt: &tokio::runtime::Runtime,
) -> (Arc<AgentChannel>, tokio::io::DuplexStream) {
    let (client, server) = tokio::io::duplex(64 * 1024);
    let (reader, writer) = tokio::io::split(client);
    // `from_transport` spawns the read/write tasks, so it must run inside the
    // runtime that is meant to own them.
    let channel = transport_rt.block_on(async {
        AgentChannel::from_transport(BufReader::new(reader), writer, 64)
    });
    (Arc::new(channel), server)
}

/// A runtime for the test's own side of the pipe, so observing the transport
/// never depends on the runtime under test.
fn observer_rt() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
}

/// Test: a blocking request issued after the runtime the channel's transport
/// tasks ride on has been shut down fails promptly, and says the channel is
/// closed.
///
/// A contract test, not the repro: it already held before #3299 was fixed,
/// because the submit fails on the write half before any timer is polled.
/// What it pins is that the settled post-teardown state stays a prompt
/// `ChannelClosed` — every call after the first one takes this path — rather
/// than a hang or a panic. `test_transport_runtime_shutdown_mid_request_
/// errors_not_panics` below is the one that reproduces the panic.
#[test]
fn test_blocking_request_after_transport_runtime_shutdown_errors_not_panics() {
    let transport_rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();
    let (channel, _server) = duplex_channel_on(&transport_rt);

    // A should-succeed timeout: this assertion is about *not panicking*, and a
    // short deadline would let a regression pass as a plain timeout instead.
    arm_happy_path(&channel);

    // The session goes away: keepalive dropped, runtime with it.
    drop(transport_rt);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        channel.request_blocking("stat", serde_json::json!({ "path": "/tmp" }))
    }));

    match result {
        Err(_) => panic!(
            "request_blocking panicked after the transport runtime was shut down \
             — blocking calls must not be driven on a runtime that can be torn \
             down under them"
        ),
        Ok(Ok(_)) => panic!("request unexpectedly succeeded with no transport tasks left"),
        Ok(Err(e)) => assert!(
            matches!(e, ChannelError::ChannelClosed),
            "expected the channel to report itself closed, got {e}"
        ),
    }
}

/// Test: the transport runtime being shut down *while a blocking request is in
/// flight* must fail that request, not panic on the calling thread.
///
/// The repro for #3299, and the reported sequence: the git-index resolver was
/// parked in a remote `metadata` call on a background thread when the SSH
/// workspace was deleted from the Orchestrator dock, and polling the
/// request's timeout `Sleep` on the runtime the keepalive had just dropped
/// panicked with "A Tokio 1.x context was found, but it is being shutdown.".
/// Waiting for the far end to see the request is what makes the teardown
/// genuinely mid-flight; without that the submit just fails and the timer is
/// never reached.
#[test]
fn test_transport_runtime_shutdown_mid_request_errors_not_panics() {
    let transport_rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();
    let (channel, mut server) = duplex_channel_on(&transport_rt);

    // Nothing ever answers on `server`, so the request parks on its result
    // until the teardown below reaches it. The deadline is only a backstop
    // against hanging CI: the assertion below insists the request failed
    // *because the carrier went away*, not because it ran out of time.
    arm_intentional_timeout(&channel);

    let requester = {
        let channel = Arc::clone(&channel);
        std::thread::spawn(move || {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                channel.request_blocking("stat", serde_json::json!({ "path": "/tmp" }))
            }))
        })
    };

    // Wait — indefinitely — until the request has actually crossed the
    // transport. Only then is the call parked on its result.
    let mut line = String::new();
    observer_rt()
        .block_on(async { BufReader::new(&mut server).read_line(&mut line).await })
        .expect("read the request off the far end of the transport");
    assert!(
        line.contains("stat"),
        "expected the stat request on the wire, got {line:?}"
    );

    // Pull the runtime out from under the in-flight request.
    drop(transport_rt);

    match requester.join().expect("requester thread") {
        Err(_) => panic!(
            "request_blocking panicked when the transport runtime was shut down \
             mid-request — the in-flight call must fail, not take the thread down"
        ),
        Ok(Ok(_)) => panic!("request unexpectedly succeeded after its transport was torn down"),
        // The read task going away with its runtime must fail the request it
        // was holding, rather than leave the caller parked until the deadline
        // — `Timeout` here would mean nothing released the pending entry.
        Ok(Err(e)) => assert!(
            matches!(e, ChannelError::ChannelClosed | ChannelError::Remote(_)),
            "expected the in-flight request to fail with the carrier, got {e}"
        ),
    }
}
