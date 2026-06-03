//! A *failed* SSH connect must surface the carrier's own stderr as the error
//! text, not dump it onto the terminal.
//!
//! `SshConnection::connect` used to spawn `ssh` with an **inherited** stderr so
//! that, when the host was unreachable, ssh wrote "Could not resolve hostname …"
//! straight onto the editor's full-screen ratatui UI — corrupting the display
//! (ratatui never learns those cells changed). The fix pipes stderr and folds
//! the carrier's last diagnostic line into the connection error instead.
//!
//! This drives the real `connect` against a fake `ssh` shim that fails like an
//! unreachable host, so it is deterministic and needs no network. RED before
//! the fix (the error carried only the exit-code hint, never the carrier's
//! stderr); GREEN after.

use std::path::PathBuf;
use std::sync::Once;

use fresh::services::remote::{ConnectionParams, SshConnection, SshError};

static PATH_INIT: Once = Once::new();

/// Prepend the fake-ssh fixtures dir to PATH once, so `Command::new("ssh")`
/// resolves to the shim. `Once` keeps the process-global env mutation from
/// racing across parallel tests in this binary.
fn ensure_fake_ssh_on_path() {
    PATH_INIT.call_once(|| {
        let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fake-ssh");
        assert!(
            dir.join("ssh").exists(),
            "fake ssh shim missing at {}",
            dir.display()
        );
        let old = std::env::var("PATH").unwrap_or_default();
        std::env::set_var("PATH", format!("{}:{old}", dir.display()));
    });
}

#[test]
fn failed_ssh_connect_surfaces_carrier_stderr_in_the_error() {
    ensure_fake_ssh_on_path();

    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .expect("runtime");

    let params = ConnectionParams {
        user: Some("u".to_string()),
        host: "fake-host".to_string(),
        port: None,
        identity_file: None,
        extra_args: Vec::new(),
    };

    let err = rt
        .block_on(SshConnection::connect(params))
        .expect_err("fake ssh exits 255, so the connect must fail");

    let msg = err.to_string();
    assert!(
        matches!(err, SshError::AgentStartFailed(_)),
        "expected an AgentStartFailed error, got: {msg}"
    );
    // The whole point of the fix: the carrier's own stderr is captured and
    // folded into the error (so it reaches the dialog/status), rather than
    // being inherited onto the screen.
    assert!(
        msg.contains("simulated connect failure"),
        "the failed connect must surface the carrier's stderr in the error; got: {msg}"
    );
}
