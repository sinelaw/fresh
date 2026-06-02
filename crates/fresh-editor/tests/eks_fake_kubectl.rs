//! End-to-end EKS transport test against a fake `kubectl` — no cluster, no root.
//!
//! The EKS authority is, at bottom, "spawn `kubectl exec … -- python3 <agent>`
//! and speak the agent protocol over its stdio." `tests/fixtures/fake-kube/
//! kubectl` makes that runnable locally: it runs the post-`--` command on this
//! machine and returns canned success for the management verbs. So this test
//! drives the *real* production code path — `EksConnection::connect` builds the
//! real `kubectl_exec_argv`, spawns the (fake) `kubectl`, streams in the real
//! agent, and a real `RemoteFileSystem` round-trips a file over the resulting
//! channel — with the only substitution being the cluster itself.
//!
//! The "pod workspace" is a real local temp dir, so the agent's absolute paths
//! resolve without any chroot. (The fake also offers an unprivileged user+pid
//! namespace via `FAKE_KUBECTL_NS=1` for interactive realism; the protocol test
//! doesn't need it.)

use std::path::PathBuf;

use fresh::model::filesystem::FileSystem;
use fresh::services::remote::{EksConnection, EksTarget, RemoteFileSystem};

#[test]
fn eks_connection_round_trips_a_file_through_fake_kubectl() {
    // Put the fake `kubectl` first on PATH so `Command::new("kubectl")`
    // resolves to it. This test file compiles to its own test binary, so
    // mutating PATH here is process-local to just this test.
    let fixtures = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fake-kube");
    assert!(
        fixtures.join("kubectl").exists(),
        "fake kubectl shim missing at {}",
        fixtures.display()
    );
    let old_path = std::env::var("PATH").unwrap_or_default();
    std::env::set_var("PATH", format!("{}:{old_path}", fixtures.display()));

    // Skip cleanly where the fake can't run (no python3 on PATH).
    if which_python().is_none() {
        eprintln!("skipping: python3 not found on PATH");
        return;
    }

    let workspace = tempfile::tempdir().expect("tempdir");
    let ws = workspace.path().to_path_buf();

    // Hold the runtime for the whole test: the channel's reader/writer tasks
    // run on it, and `RemoteFileSystem`'s blocking ops drive it via the handle
    // captured when the channel was built.
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .expect("runtime");

    let target = EksTarget {
        context: Some("fake-ctx".to_string()),
        namespace: "test".to_string(),
        pod: "fake-pod".to_string(),
        container: None,
        workspace: Some(ws.to_string_lossy().into_owned()),
    };

    // The real connect: real argv, real agent bootstrap, real channel — only
    // the cluster is faked.
    let connection = rt
        .block_on(EksConnection::connect(target))
        .expect("EksConnection::connect over fake kubectl");
    assert!(connection.is_connected(), "agent channel is live");

    // A write+read round-trip over the real RemoteFileSystem exercises the
    // whole transport: kubectl_exec_argv → fake kubectl → local python agent →
    // AgentChannel → filesystem protocol.
    let fs = RemoteFileSystem::new(connection.channel(), "eks:test/fake-pod".to_string());
    let file = ws.join("hello.txt");
    fs.write_file(&file, b"cloud workspace")
        .expect("write_file over the agent channel");
    let got = fs.read_file(&file).expect("read_file over the agent channel");
    assert_eq!(got, b"cloud workspace", "round-tripped bytes match");

    // And the bytes really landed on the local "pod" filesystem.
    assert_eq!(
        std::fs::read(&file).expect("read the on-disk file"),
        b"cloud workspace"
    );

    // Metadata path works too (HeadObject-equivalent on the agent).
    assert!(fs.exists(&file), "exists() sees the written file");

    drop(connection);
}

fn which_python() -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let cand = dir.join("python3");
        if cand.exists() {
            return Some(cand);
        }
    }
    None
}
