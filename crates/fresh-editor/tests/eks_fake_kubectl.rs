//! End-to-end EKS tests against a fake `kubectl` — no cluster, no root.
//!
//! The EKS authority is, at bottom, "spawn `kubectl exec … -- <cmd>` and speak
//! to it." `tests/fixtures/fake-kube/kubectl` makes that runnable locally: it
//! runs the post-`--` command on this machine (the "pod") and returns canned
//! success for the management verbs. So these tests drive the *real*
//! production code paths — the real `kubectl_exec_argv`, the real agent
//! bootstrap, the real `RemoteFileSystem` / `RemoteProcessSpawner` /
//! `KubectlLongRunningSpawner` — with only the cluster substituted.
//!
//! The "pod workspace" is a real local temp dir, so the agent's absolute paths
//! resolve without any chroot.

use std::path::PathBuf;
use std::sync::{Arc, Once};

use fresh::model::filesystem::FileSystem;
use fresh::services::authority::{connect_eks_authority, RemoteAgentSpec};
use fresh::services::env_provider::EnvProvider;
use fresh::services::remote::{EksConnection, EksTarget, RemoteFileSystem};
use fresh::services::workspace_trust::WorkspaceTrust;

static PATH_INIT: Once = Once::new();

/// Prepend the fake-kubectl fixtures dir to PATH exactly once, so
/// `Command::new("kubectl")` resolves to the shim. `Once` keeps the
/// process-global env mutation from racing across parallel tests in this
/// binary.
fn ensure_fake_kubectl_on_path() {
    PATH_INIT.call_once(|| {
        let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fake-kube");
        assert!(
            dir.join("kubectl").exists(),
            "fake kubectl shim missing at {}",
            dir.display()
        );
        let old = std::env::var("PATH").unwrap_or_default();
        std::env::set_var("PATH", format!("{}:{old}", dir.display()));
    });
}

fn python3_available() -> bool {
    let Some(path) = std::env::var_os("PATH") else {
        return false;
    };
    std::env::split_paths(&path).any(|d| d.join("python3").exists())
}

fn multi_thread_rt() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .expect("runtime")
}

fn target(workspace: &std::path::Path) -> EksTarget {
    EksTarget {
        context: Some("fake-ctx".to_string()),
        namespace: "test".to_string(),
        pod: "fake-pod".to_string(),
        container: None,
        workspace: Some(workspace.to_string_lossy().into_owned()),
    }
}

#[test]
fn eks_connection_round_trips_a_file_through_fake_kubectl() {
    ensure_fake_kubectl_on_path();
    if !python3_available() {
        eprintln!("skipping: python3 not found on PATH");
        return;
    }

    let workspace = tempfile::tempdir().expect("tempdir");
    let ws = workspace.path().to_path_buf();
    let rt = multi_thread_rt();

    // The real connect: real argv, real agent bootstrap, real channel — only
    // the cluster is faked.
    let connection = rt
        .block_on(EksConnection::connect(target(&ws)))
        .expect("EksConnection::connect over fake kubectl");
    assert!(connection.is_connected(), "agent channel is live");

    // A write+read round-trip over the real RemoteFileSystem exercises the
    // whole transport: kubectl_exec_argv → fake kubectl → local python agent →
    // AgentChannel → filesystem protocol.
    let fs = RemoteFileSystem::new(connection.channel(), "eks:test/fake-pod".to_string());
    let file = ws.join("hello.txt");
    fs.write_file(&file, b"cloud workspace")
        .expect("write_file over the agent channel");
    assert_eq!(
        fs.read_file(&file).expect("read_file over the agent channel"),
        b"cloud workspace"
    );
    // The bytes really landed on the local "pod" filesystem.
    assert_eq!(std::fs::read(&file).unwrap(), b"cloud workspace");
    assert!(fs.exists(&file));

    drop(connection);
}

#[test]
fn attach_spec_payload_parses_and_connects_through_fake_kubectl() {
    ensure_fake_kubectl_on_path();
    if !python3_available() {
        eprintln!("skipping: python3 not found on PATH");
        return;
    }

    let workspace = tempfile::tempdir().expect("tempdir");
    let ws = workspace.path().to_path_buf();

    // The exact JSON the eks-workspace plugin emits and the
    // `attachRemoteAgent` op parses — driven through the real
    // parse → connect path `handle_attach_remote_agent` runs (minus the
    // editor's runtime/bridge hop, which is trivial plumbing).
    let json = serde_json::json!({
        "transport": {
            "kind": "kubectl-exec",
            "context": "fake-ctx",
            "namespace": "test",
            "pod": "fake-pod",
            "workspace": ws.to_string_lossy(),
        },
        "base_env": [["E2E_BASE", "base"]],
    });
    let spec: RemoteAgentSpec = serde_json::from_value(json).expect("attach spec parses");
    let (target, base_env) = spec.into_eks_target();

    let rt = multi_thread_rt();
    let (authority, _keepalive) = rt
        .block_on(connect_eks_authority(
            target,
            base_env,
            Arc::new(WorkspaceTrust::permissive()),
            Arc::new(EnvProvider::inactive()),
        ))
        .expect("connect from parsed attach spec");

    assert!(authority.display_label.starts_with("eks:"));
    assert_eq!(authority.terminal_wrapper.command, "kubectl");

    // The connected authority's filesystem works end to end.
    let f = ws.join("spec.txt");
    authority.filesystem.write_file(&f, b"via-spec").unwrap();
    assert_eq!(authority.filesystem.read_file(&f).unwrap(), b"via-spec");
}

#[test]
fn eks_authority_spawns_one_shot_and_lsp_through_fake_kubectl() {
    ensure_fake_kubectl_on_path();
    if !python3_available() {
        eprintln!("skipping: python3 not found on PATH");
        return;
    }

    let workspace = tempfile::tempdir().expect("tempdir");
    let ws = workspace.path().to_path_buf();
    let rt = multi_thread_rt();

    let trust = std::sync::Arc::new(WorkspaceTrust::permissive());
    let env = std::sync::Arc::new(EnvProvider::inactive());

    // Assemble the full authority over the fake cluster. `base_env` is the
    // captured in-pod probe applied to LSP spawns / command_exists.
    let (authority, _keepalive) = rt
        .block_on(connect_eks_authority(
            target(&ws),
            vec![("E2E_BASE".to_string(), "base".to_string())],
            std::sync::Arc::clone(&trust),
            std::sync::Arc::clone(&env),
        ))
        .expect("connect_eks_authority over fake kubectl");

    // The authority is shaped correctly.
    assert_eq!(authority.terminal_wrapper.command, "kubectl");
    assert!(authority.display_label.starts_with("eks:"));

    // Filesystem (agent channel).
    let f = ws.join("a.txt");
    authority.filesystem.write_file(&f, b"hi").unwrap();
    assert_eq!(authority.filesystem.read_file(&f).unwrap(), b"hi");

    // One-shot process spawn routes through the agent's `exec` (i.e. runs in
    // the "pod"). Use `sh -c` so it's robust across systems.
    let r = rt
        .block_on(authority.process_spawner.spawn(
            "sh".to_string(),
            vec!["-c".to_string(), "printf one-shot-ok".to_string()],
            None,
        ))
        .expect("one-shot spawn");
    assert_eq!(r.exit_code, 0);
    assert_eq!(r.stdout, "one-shot-ok");

    // command_exists routes through a separate `kubectl exec` (the LSP
    // spawner), replaying base_env so PATH matches what a server would see.
    assert!(
        rt.block_on(authority.long_running_spawner.command_exists("sh")),
        "sh resolves in the pod"
    );
    assert!(
        !rt.block_on(
            authority
                .long_running_spawner
                .command_exists("definitely-not-a-real-binary-xyz")
        ),
        "missing binary reported absent"
    );

    // Long-running (LSP-style) stdio spawn: env (base + per-call) and cwd must
    // be applied via the `sh -lc` wrapper, and stdin↔stdout must round-trip.
    let mut child = rt
        .block_on(authority.long_running_spawner.spawn_stdio(
            "sh",
            &[
                "-c".to_string(),
                "echo \"$E2E_VAR|$E2E_BASE|$(pwd)\"; cat".to_string(),
            ],
            vec![("E2E_VAR".to_string(), "call".to_string())],
            Some(ws.as_path()),
            None,
        ))
        .expect("spawn_stdio");

    let mut stdin = child.take_stdin().expect("stdin piped");
    let stdout = child.take_stdout().expect("stdout piped");

    let (header, echoed) = rt.block_on(async move {
        use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
        let mut reader = BufReader::new(stdout);
        let mut header = String::new();
        reader.read_line(&mut header).await.expect("read header line");
        // Round-trip through `cat`, then close stdin so it drains and exits.
        stdin.write_all(b"pong\n").await.expect("write stdin");
        stdin.flush().await.expect("flush stdin");
        drop(stdin);
        let mut echoed = String::new();
        reader.read_line(&mut echoed).await.expect("read echoed line");
        (header, echoed)
    });

    // per-call env, base env, and cwd all honored:
    assert!(header.contains("call"), "per-call env applied: {header:?}");
    assert!(header.contains("|base|"), "base_env applied: {header:?}");
    assert!(
        header.contains(ws.to_string_lossy().as_ref()),
        "cwd applied: {header:?} (ws={})",
        ws.display()
    );
    // stdin→cat→stdout round-trip:
    assert_eq!(echoed, "pong\n");

    rt.block_on(child.kill()).ok();
}
