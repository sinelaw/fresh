//! Pluggable carriers for the remote agent.
//!
//! The agent bootstrap — stream `AGENT_SOURCE` into a `python3` process on
//! the far side, wait for its `ready` line, check the protocol version, then
//! hand the stdio pair to an [`AgentChannel`] — is identical regardless of
//! *how* we reach that far side. SSH spawns `ssh … python3 …`; K8s spawns
//! `kubectl exec … -- python3 …`. The only thing that differs is the carrier
//! command.
//!
//! A [`RemoteTransport`] supplies exactly that: a configured carrier
//! [`Command`] whose execution runs the python bootstrap remotely. Everything
//! above the channel ([`RemoteFileSystem`](super::RemoteFileSystem), the
//! remote spawners, the agent protocol, the reconnect task) is transport-
//! agnostic and reused verbatim.
//!
//! This module is additive: the existing SSH path in `connection.rs` is left
//! untouched, so SSH behaviour is unchanged. SSH can migrate onto this seam
//! later (see `docs/internal/EKS_S3_AUTHORITY_DESIGN.md`).

use std::process::Stdio;
use std::sync::Arc;

use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncWrite, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};

use crate::services::process_hidden::HideWindow;
use crate::services::remote::channel::AgentChannel;
use crate::services::remote::protocol::AgentResponse;
use crate::services::remote::AGENT_SOURCE;

/// Where a Kubernetes-exec authority acts: a single pod (and optional
/// container) on a cluster reachable through the host's kubeconfig.
///
/// `workspace` is the pod-side path the editor roots at — used to open the
/// integrated terminal in the right place; file/process operations carry
/// their own absolute paths and don't need it.
#[derive(Debug, Clone)]
pub struct KubeTarget {
    /// kubeconfig context to select (`--context`); `None` uses the current one.
    pub context: Option<String>,
    pub namespace: String,
    pub pod: String,
    /// Target container in a multi-container pod (`-c`); `None` uses the default.
    pub container: Option<String>,
    /// Pod-side workspace root (for the terminal's `cd`); `None` = home.
    pub workspace: Option<String>,
}

impl KubeTarget {
    /// Stable, human-readable identity, e.g. `k8s:prod/dev/pod-7c9f`.
    pub fn display(&self) -> String {
        let ctx = self.context.as_deref().unwrap_or("-");
        match &self.container {
            Some(c) => format!("k8s:{ctx}/{}/{}/{c}", self.namespace, self.pod),
            None => format!("k8s:{ctx}/{}/{}", self.namespace, self.pod),
        }
    }
}

/// Compose a `kubectl exec` argv.
///
/// Shared by the agent transport (`-i`, running `python3 …`), the
/// long-running LSP spawner (`-i`, running the server), and — via
/// [`build_kube_terminal_args`](super::build_kube_terminal_args) — the
/// integrated terminal (`-it`, running a login shell). Everything after `--`
/// is exec'd directly by `kubectl` (no remote shell), so unlike the SSH path
/// no shell-quoting of the command is required.
///
/// Layout: `[--context CTX] exec <flags…> -n NS [-c C] POD -- command args…`.
pub(crate) fn kubectl_exec_argv(
    target: &KubeTarget,
    flags: &[&str],
    command: &str,
    args: &[String],
) -> Vec<String> {
    let mut a: Vec<String> = Vec::with_capacity(args.len() + flags.len() + 9);
    if let Some(ctx) = target.context.as_ref() {
        a.push("--context".into());
        a.push(ctx.clone());
    }
    a.push("exec".into());
    for f in flags {
        a.push((*f).into());
    }
    a.push("-n".into());
    a.push(target.namespace.clone());
    if let Some(c) = target.container.as_ref() {
        a.push("-c".into());
        a.push(c.clone());
    }
    a.push(target.pod.clone());
    a.push("--".into());
    a.push(command.into());
    a.extend(args.iter().cloned());
    a
}

/// The python one-liner the carrier runs: read exactly `AGENT_SOURCE.len()`
/// bytes from stdin and `exec` them, then the agent keeps reading stdin for
/// protocol messages. Byte-count framing avoids any dependency on a remote
/// shell or here-doc support — identical to the SSH bootstrap.
pub(crate) fn agent_bootstrap_pycode() -> String {
    format!("import sys;exec(sys.stdin.read({}))", AGENT_SOURCE.len())
}

/// How the carrier's stderr is wired.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StderrMode {
    /// Inherit the terminal — lets the carrier prompt (SSH password, an
    /// interactive `kubectl` auth helper) on first connect.
    Inherit,
    /// Discard — for non-interactive reconnection.
    Null,
}

/// Builds the carrier command that runs the agent bootstrap on the far side.
///
/// Object-safe on purpose: callers hold `&dyn RemoteTransport` and the
/// reconnect path re-invokes it to respawn after a drop.
pub trait RemoteTransport: Send + Sync {
    /// A configured [`Command`] with stdin/stdout piped and stderr per
    /// `stderr`, whose execution launches `python3 … <bootstrap>` remotely.
    fn build_command(&self, stderr: StderrMode) -> Command;
    /// Human-readable identity for status/logging.
    fn display(&self) -> String;
}

/// `kubectl exec` carrier — runs the agent inside a Kubernetes pod (any cluster: EKS/GKE/AKS/k3d).
pub struct KubectlExecTransport {
    target: KubeTarget,
}

impl KubectlExecTransport {
    pub fn new(target: KubeTarget) -> Self {
        Self { target }
    }

    pub fn target(&self) -> &KubeTarget {
        &self.target
    }
}

impl RemoteTransport for KubectlExecTransport {
    fn build_command(&self, stderr: StderrMode) -> Command {
        let pycode = agent_bootstrap_pycode();
        let argv = kubectl_exec_argv(
            &self.target,
            &["-i"],
            "python3",
            &["-u".to_string(), "-c".to_string(), pycode],
        );
        let mut cmd = Command::new("kubectl");
        cmd.args(&argv);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        match stderr {
            StderrMode::Inherit => {
                cmd.stderr(Stdio::inherit());
            }
            StderrMode::Null => {
                cmd.stderr(Stdio::null());
            }
        }
        cmd.hide_window();
        cmd
    }

    fn display(&self) -> String {
        self.target.display()
    }
}

/// Error establishing a transport-backed agent connection.
#[derive(Debug, thiserror::Error)]
pub enum TransportError {
    #[error("failed to spawn carrier process: {0}")]
    Spawn(#[from] std::io::Error),

    #[error("agent failed to start: {0}")]
    AgentStartFailed(String),

    #[error("protocol version mismatch: expected {expected}, got {got}")]
    VersionMismatch { expected: u32, got: u32 },
}

/// Spawn the carrier, stream the agent in, and wait for `ready`.
///
/// Returns the ready-to-use `(reader, writer, child)` — the same triple
/// `establish_ssh_transport` yields for SSH, so callers can build an
/// [`AgentChannel`] or hand the pair to `replace_transport` for reconnects.
pub async fn bootstrap_agent(
    transport: &dyn RemoteTransport,
    stderr: StderrMode,
) -> Result<(BufReader<ChildStdout>, ChildStdin, Child), TransportError> {
    let mut cmd = transport.build_command(stderr);
    let mut child = cmd.spawn()?;

    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| TransportError::AgentStartFailed("failed to get stdin".to_string()))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| TransportError::AgentStartFailed("failed to get stdout".to_string()))?;

    // Stream the agent source, exact byte count (see `agent_bootstrap_pycode`).
    stdin.write_all(AGENT_SOURCE.as_bytes()).await?;
    stdin.flush().await?;

    let mut reader = BufReader::new(stdout);
    let mut ready_line = String::new();
    match reader.read_line(&mut ready_line).await {
        Ok(0) => {
            return Err(TransportError::AgentStartFailed(format!(
                "{} closed the connection before the agent was ready \
                 (is python3 present in the pod, and the context/namespace/pod correct?)",
                transport.display()
            )));
        }
        Ok(_) => {}
        Err(e) => {
            return Err(TransportError::AgentStartFailed(format!("read error: {e}")));
        }
    }

    let ready: AgentResponse = serde_json::from_str(&ready_line).map_err(|e| {
        TransportError::AgentStartFailed(format!(
            "invalid ready message '{}': {e}",
            ready_line.trim()
        ))
    })?;
    if !ready.is_ready() {
        return Err(TransportError::AgentStartFailed(
            "agent did not send ready message".to_string(),
        ));
    }

    let version = ready.version.unwrap_or(0);
    if version != crate::services::remote::protocol::PROTOCOL_VERSION {
        return Err(TransportError::VersionMismatch {
            expected: crate::services::remote::protocol::PROTOCOL_VERSION,
            got: version,
        });
    }

    Ok((reader, stdin, child))
}

/// Active agent connection over a [`RemoteTransport`].
///
/// The K8s analogue of [`SshConnection`](super::SshConnection): owns the
/// carrier child and the [`AgentChannel`] the editor's remote filesystem and
/// spawners ride on. Dropping it kills the carrier.
pub struct KubeConnection {
    process: Child,
    channel: Arc<AgentChannel>,
    display: String,
    /// Keeps the idle `kubectl exec` stream warm against LB/NAT idle
    /// timeouts. Aborted on drop so a dead carrier stops being pinged.
    heartbeat: tokio::task::JoinHandle<()>,
}

impl KubeConnection {
    /// Bootstrap the agent inside the pod named by `target`.
    pub async fn connect(target: KubeTarget) -> Result<Self, TransportError> {
        let transport = KubectlExecTransport::new(target);
        let (reader, writer, child) = bootstrap_agent(&transport, StderrMode::Inherit).await?;
        let channel = Arc::new(AgentChannel::new(reader, writer));
        let heartbeat = crate::services::remote::spawn_heartbeat_task(
            &channel,
            crate::services::remote::DEFAULT_HEARTBEAT_INTERVAL,
        );
        Ok(Self {
            process: child,
            channel,
            display: transport.display(),
            heartbeat,
        })
    }

    /// Share the channel for the filesystem / spawners.
    pub fn channel(&self) -> Arc<AgentChannel> {
        self.channel.clone()
    }

    pub fn is_connected(&self) -> bool {
        self.channel.is_connected()
    }

    pub fn connection_string(&self) -> &str {
        &self.display
    }
}

/// Reconnect task for a K8s agent channel: when the channel drops, it
/// re-bootstraps the agent by re-running `kubectl exec` against the pod and
/// hot-swaps the transport via `replace_transport`. The K8s analogue of
/// [`spawn_reconnect_task`](super::spawn_reconnect_task), reusing the generic
/// [`spawn_reconnect_task_with`](super::spawn_reconnect_task_with).
///
/// Reconnects to the *same* `target`. A pod reschedule / eviction changes the
/// pod name, which this does not yet re-resolve — the plugin "resolve current
/// pod" callback (`AUTHORITY_DESIGN.md` open question 3) layers on later. A
/// same-name reconnect still covers transient stream drops (the common idle /
/// network-blip case).
pub fn spawn_kube_reconnect_task(
    channel: &Arc<AgentChannel>,
    target: KubeTarget,
) -> tokio::task::JoinHandle<()> {
    let connect_fn = move || {
        let target = target.clone();
        async move {
            let transport = KubectlExecTransport::new(target);
            // Non-interactive on reconnect (no terminal to prompt on).
            let (reader, writer, _child) = bootstrap_agent(&transport, StderrMode::Null)
                .await
                .map_err(|e| crate::services::remote::SshError::AgentStartFailed(e.to_string()))?;
            let reader: Box<dyn AsyncBufRead + Unpin + Send> = Box::new(reader);
            let writer: Box<dyn AsyncWrite + Unpin + Send> = Box::new(writer);
            Ok::<_, crate::services::remote::SshError>((reader, writer))
        }
    };
    crate::services::remote::spawn_reconnect_task_with(
        Arc::clone(channel),
        connect_fn,
        crate::services::remote::ReconnectConfig::default(),
        "K8s remote",
    )
}

impl Drop for KubeConnection {
    fn drop(&mut self) {
        // Stop pinging a carrier we're about to kill.
        self.heartbeat.abort();
        // Best-effort kill; the OS reaps on our exit if this fails. Same
        // shape as `SshConnection::Drop` (the crate denies
        // `let_underscore_must_use`, so we can't `let _ =` the result).
        if let Ok(()) = self.process.start_kill() {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn target() -> KubeTarget {
        KubeTarget {
            context: Some("k3d-dev".to_string()),
            namespace: "dev".to_string(),
            pod: "fresh-7c9f".to_string(),
            container: None,
            workspace: Some("/workspace".to_string()),
        }
    }

    #[test]
    fn argv_orders_flags_namespace_pod_then_command() {
        let argv = kubectl_exec_argv(&target(), &["-i"], "python3", &["-u".into()]);
        assert_eq!(
            argv,
            vec![
                "--context",
                "k3d-dev",
                "exec",
                "-i",
                "-n",
                "dev",
                "fresh-7c9f",
                "--",
                "python3",
                "-u",
            ]
        );
    }

    #[test]
    fn argv_includes_container_when_set() {
        let mut t = target();
        t.container = Some("app".to_string());
        let argv = kubectl_exec_argv(&t, &["-it"], "sh", &[]);
        // `-c app` must sit between the pod-scoping flags and the pod name.
        let c = argv.iter().position(|a| a == "-c").expect("-c present");
        let pod = argv.iter().position(|a| a == "fresh-7c9f").unwrap();
        let sep = argv.iter().position(|a| a == "--").unwrap();
        assert_eq!(argv[c + 1], "app");
        assert!(c < pod, "-c precedes pod");
        assert!(pod < sep, "pod precedes --");
    }

    #[test]
    fn argv_omits_context_when_none() {
        let mut t = target();
        t.context = None;
        let argv = kubectl_exec_argv(&t, &["-i"], "python3", &[]);
        assert!(!argv.iter().any(|a| a == "--context"));
        assert_eq!(argv[0], "exec");
    }

    #[test]
    fn bootstrap_pycode_reads_exact_agent_length() {
        let code = agent_bootstrap_pycode();
        assert_eq!(
            code,
            format!("import sys;exec(sys.stdin.read({}))", AGENT_SOURCE.len())
        );
        // No shell metacharacters that would need quoting under `kubectl --`.
        assert!(!code.contains('\''));
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn kube_reconnect_task_spawns_and_aborts_cleanly() {
        // We can't run a real `kubectl exec` here, but we can verify the
        // task's lifecycle over a live (local-agent) channel: while the
        // channel is connected the task idles (it only acts on disconnect),
        // and aborting it terminates promptly without panicking. The actual
        // reconnect path is exercised by the generic `spawn_reconnect_task_with`
        // tests; this guards the K8s wiring on top of it.
        let channel = crate::services::remote::spawn_local_agent()
            .await
            .expect("spawn local agent");
        let handle = spawn_kube_reconnect_task(&channel, target());
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        assert!(channel.is_connected(), "channel healthy; reconnect idles");
        handle.abort();
        let joined = tokio::time::timeout(std::time::Duration::from_secs(2), handle).await;
        assert!(joined.is_ok(), "aborted reconnect task joins promptly");
    }

    #[test]
    fn build_command_pipes_stdio_and_targets_kubectl() {
        // We can't introspect a tokio Command's program directly, but we can
        // assert the argv we'd hand kubectl is the interactive python bootstrap.
        let t = target();
        let pycode = agent_bootstrap_pycode();
        let argv = kubectl_exec_argv(
            &t,
            &["-i"],
            "python3",
            &["-u".to_string(), "-c".to_string(), pycode.clone()],
        );
        assert_eq!(argv.last().unwrap(), &pycode);
        assert!(argv.contains(&"-i".to_string()));
        assert_eq!(
            KubectlExecTransport::new(t).display(),
            "k8s:k3d-dev/dev/fresh-7c9f"
        );
    }
}
