//! Real end-to-end regression for "a restored remote terminal ran on the
//! LOCAL host" (the no-placeholder-authority refactor).
//!
//! The full arc, through the editor against a real SSH server:
//!   1. Two remote (SSH) sessions are created and **persisted** (the editor's
//!      own workspace-save path), one carrying an integrated terminal.
//!   2. The editor is **restarted** — a fresh harness over the same on-disk
//!      state, exactly what a relaunch does.
//!   3. We dive into the terminal-bearing session via the same host command the
//!      Orchestrator dock issues (`SetActiveWindow`), which reconnects its SSH
//!      backend and promotes it to a live window.
//!   4. The *restored* terminal must run its shell **on the remote host**.
//!      `$SSH_CONNECTION` is set only by sshd for a genuine SSH session, so
//!      observing a value carrying the loopback address proves the restored
//!      terminal connects to the remote host rather than spawning a local shell.
//!
//! Before the fix a restored remote session booted as a window holding a dummy
//! *local* authority, and diving materialized its terminal through that local
//! authority — the shell ran locally and `$SSH_CONNECTION` was empty (the test
//! never observes `CONN=127.0.0.1`, so it times out / fails under nextest).
//! After the fix the session stays dormant until the dive connects, then is
//! promoted to a window *born with* the SSH authority, so the restored terminal
//! spawns over SSH and `CONN=127.0.0.1…` appears.
//!
//! A throwaway, key-only `sshd` on 127.0.0.1 is the remote (same technique as
//! `remote_ssh_terminal.rs`; `$SSH_CONNECTION` is what distinguishes a genuine
//! SSH shell from a local one on loopback). Requires
//! `ssh`/`sshd`/`ssh-keygen`/`python3` (the SSH authority bootstraps a small
//! python agent on the remote) and is Linux-only; it *skips* (no-op) when any
//! are missing.
//!
//! This is a standalone integration binary (not part of `e2e_tests`) so it can
//! point `$XDG_DATA_HOME` at an isolated temp dir: the editor's workspace
//! persistence keys off `$XDG_DATA_HOME/fresh`, and we build the harness's
//! `DirectoryContext` to match, so save / discovery / promote-restore all share
//! one isolated directory. A single test per binary keeps the `set_var` safe.
//!
//! Gated on the `plugins` feature: the remote-connect machinery (and the
//! `SetActiveWindow` dive that drives it) is plugins-gated, so without it there
//! is nothing to exercise.
#![cfg(all(target_os = "linux", feature = "plugins"))]

mod common;

use std::net::{TcpListener, TcpStream};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use crossterm::event::{KeyCode, KeyModifiers};

use common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config_io::DirectoryContext;
use fresh::services::authority::{RemoteAgentSpec, RemoteTransportSpec, SessionAuthoritySpec};
use fresh_core::api::PluginCommand;

// --------------------------------------------------------------------------
// Throwaway sshd bring-up (key-only, loopback). Mirrors remote_ssh_terminal.rs.
// --------------------------------------------------------------------------

fn is_file(p: &Path) -> bool {
    p.is_file()
}

fn resolve(name: &str, fallbacks: &[&str]) -> Option<PathBuf> {
    if let Some(path) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&path) {
            let cand = dir.join(name);
            if is_file(&cand) {
                return Some(cand);
            }
        }
    }
    fallbacks
        .iter()
        .map(PathBuf::from)
        .find(|cand| is_file(cand))
}

fn keygen(keygen_bin: &Path, path: &Path) {
    let status = Command::new(keygen_bin)
        .args(["-t", "ed25519", "-q", "-N", ""])
        .arg("-f")
        .arg(path)
        .status()
        .expect("run ssh-keygen");
    assert!(status.success(), "ssh-keygen failed for {path:?}");
}

fn set_mode(path: &Path, mode: u32) {
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(mode)).unwrap();
}

fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .expect("bind ephemeral port")
        .local_addr()
        .unwrap()
        .port()
}

fn wait_for_listen(port: u16, timeout: Duration) -> bool {
    let start = Instant::now();
    while start.elapsed() < timeout {
        if TcpStream::connect(("127.0.0.1", port)).is_ok() {
            return true;
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    false
}

struct KillOnDrop(Child);
impl Drop for KillOnDrop {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn current_user() -> Option<String> {
    std::env::var("USER")
        .ok()
        .or_else(|| std::env::var("LOGNAME").ok())
        .or_else(|| {
            let out = Command::new("id").arg("-un").output().ok()?;
            String::from_utf8(out.stdout)
                .ok()
                .map(|s| s.trim().to_string())
        })
        .filter(|s| !s.is_empty())
}

/// A running throwaway sshd plus the client knobs needed to reach it without
/// touching the real user's `~/.ssh`.
struct SshServer {
    port: u16,
    user: String,
    identity: PathBuf,
    known_hosts: PathBuf,
    _tmp: tempfile::TempDir,
    _guard: KillOnDrop,
}

/// Bring up a key-only sshd on a loopback port, or `None` when the toolchain
/// (ssh/sshd/ssh-keygen/python3) isn't available so the test can skip.
fn start_sshd() -> Option<SshServer> {
    let (ssh, sshd, ssh_keygen, _python) = (
        resolve("ssh", &[])?,
        resolve(
            "sshd",
            &["/usr/sbin/sshd", "/sbin/sshd", "/usr/local/sbin/sshd"],
        )?,
        resolve("ssh-keygen", &[])?,
        resolve("python3", &["/usr/local/bin/python3", "/usr/bin/python3"])?,
    );
    let _ = ssh; // confirmed present; the wrappers invoke `ssh` by name.
    let user = current_user()?;

    let tmp = tempfile::tempdir().ok()?;
    let t = tmp.path();
    let hostkey = t.join("hostkey");
    let id = t.join("id");
    let authorized = t.join("authorized_keys");
    let config = t.join("sshd_config");
    let known_hosts = t.join("known_hosts");

    keygen(&ssh_keygen, &hostkey);
    keygen(&ssh_keygen, &id);
    std::fs::copy(t.join("id.pub"), &authorized).unwrap();
    set_mode(&authorized, 0o600);

    let port = free_port();
    std::fs::write(
        &config,
        format!(
            "Port {port}\n\
             ListenAddress 127.0.0.1\n\
             HostKey {hostkey}\n\
             PidFile {pid}\n\
             AuthorizedKeysFile {authorized}\n\
             StrictModes no\n\
             UsePAM no\n\
             PermitRootLogin prohibit-password\n\
             PasswordAuthentication no\n\
             PubkeyAuthentication yes\n",
            hostkey = hostkey.display(),
            pid = t.join("sshd.pid").display(),
            authorized = authorized.display(),
        ),
    )
    .unwrap();

    let log = t.join("sshd.log");
    let logf = std::fs::File::create(&log).unwrap();
    let child = Command::new(&sshd)
        .arg("-D")
        .arg("-e")
        .arg("-f")
        .arg(&config)
        .stdout(Stdio::from(logf.try_clone().unwrap()))
        .stderr(Stdio::from(logf))
        .spawn()
        .ok()?;
    let guard = KillOnDrop(child);

    if !wait_for_listen(port, Duration::from_secs(10)) {
        eprintln!(
            "skipping: sshd never listened on {port}.\nlog:\n{}",
            std::fs::read_to_string(&log).unwrap_or_default()
        );
        return None;
    }

    Some(SshServer {
        port,
        user,
        identity: id,
        known_hosts,
        _tmp: tmp,
        _guard: guard,
    })
}

impl SshServer {
    /// An SSH `authority_spec` pointing at this server, with host-key
    /// verification redirected into a temp `known_hosts` (carried on both the
    /// carrier connect and the terminal wrapper via `extra_args`) so the test
    /// never reads or writes the real `~/.ssh/known_hosts`.
    fn spec(&self, remote_path: &Path, label: &str) -> SessionAuthoritySpec {
        SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
            transport: RemoteTransportSpec::Ssh {
                user: Some(self.user.clone()),
                host: "127.0.0.1".to_string(),
                port: Some(self.port),
                identity_file: Some(self.identity.to_string_lossy().into_owned()),
                remote_path: Some(remote_path.to_string_lossy().into_owned()),
                extra_args: vec![
                    "-o".to_string(),
                    format!("UserKnownHostsFile={}", self.known_hosts.display()),
                    "-o".to_string(),
                    "GlobalKnownHostsFile=/dev/null".to_string(),
                ],
            },
            base_env: Vec::new(),
            window: true,
            label: Some(label.to_string()),
            command: None,
        })
    }
}

// --------------------------------------------------------------------------
// The test.
// --------------------------------------------------------------------------

const W: u16 = 120;
const H: u16 = 40;

fn canonical(p: &Path) -> PathBuf {
    p.canonicalize().unwrap_or_else(|_| p.to_path_buf())
}

#[test]
fn restored_remote_terminal_reconnects_over_ssh_after_restart() -> anyhow::Result<()> {
    let Some(server) = start_sshd() else {
        eprintln!("skipping: ssh/sshd/ssh-keygen/python3 not available");
        return Ok(());
    };

    let base = tempfile::tempdir()?;

    // Isolate ALL editor persistence into the temp tree: `$XDG_DATA_HOME/fresh`
    // is where workspace save/load (hence promote-restore) live, and we build
    // the harness's `DirectoryContext` so its `data_dir` is the SAME path — so
    // save, boot discovery, and promote all agree. Single test per binary, so
    // this process-global `set_var` races nothing.
    let xdg_data = base.path().join("xdg-data");
    std::fs::create_dir_all(&xdg_data)?;
    std::env::set_var("XDG_DATA_HOME", &xdg_data);
    let dir_context = DirectoryContext {
        data_dir: xdg_data.join("fresh"), // == get_data_dir()
        config_dir: base.path().join("config"),
        home_dir: Some(base.path().join("home")),
        documents_dir: None,
        downloads_dir: None,
    };

    let project = canonical_mkdir(base.path(), "project")?;
    let remote_a = canonical_mkdir(base.path(), "remoteA")?;
    let remote_b = canonical_mkdir(base.path(), "remoteB")?;

    // ---- Phase 1: create + persist two remote sessions (one with a terminal).
    //               The remote `authority_spec` is set *after* the terminal is
    //               opened so window activation never kicks off a connect; only
    //               the layout + spec need to reach disk. ----
    {
        let mut h = EditorTestHarness::create(
            W,
            H,
            HarnessOptions::new()
                .with_working_dir(project.clone())
                .with_shared_dir_context(dir_context.clone()),
        )?;

        // Session A — open an integrated terminal, then tag it remote.
        let a = h
            .editor_mut()
            .create_window_at(remote_a.clone(), "ssh-remote-A".to_string());
        h.editor_mut().set_active_window(a);
        h.editor_mut().open_terminal();
        h.wait_until(|h| h.screen_to_string().contains("*Terminal"))?;
        h.editor_mut()
            .set_session_authority_spec(a, server.spec(&remote_a, "ssh-remote-A"));

        // Session B — a second remote session (materialized so it persists) to
        // prove the restart restores *multiple* dormant remotes.
        let b = h
            .editor_mut()
            .create_window_at(remote_b.clone(), "ssh-remote-B".to_string());
        h.editor_mut().set_active_window(b);
        h.editor_mut()
            .set_session_authority_spec(b, server.spec(&remote_b, "ssh-remote-B"));

        // Persist every window's workspace (layout + terminal + authority_spec).
        h.editor_mut().save_all_windows_workspaces()?;
    } // harness dropped == editor shut down.

    // ---- Phase 2: restart over the same persistence dir. ----
    let mut h = EditorTestHarness::create(
        W,
        H,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context.clone()),
    )?;

    // Both remote sessions come back *dormant* (no live window yet): the fix's
    // whole point is that a remote session is never rebuilt as a placeholder
    // window at boot.
    let dormant = h.editor().dormant_remote_sessions_for_test();
    assert!(
        dormant.iter().any(|(_, l)| l == "ssh-remote-A")
            && dormant.iter().any(|(_, l)| l == "ssh-remote-B"),
        "both remote sessions must be restored as dormant descriptors; got {dormant:?}"
    );

    let a_id = dormant
        .iter()
        .find(|(_, l)| l == "ssh-remote-A")
        .map(|(id, _)| *id)
        .expect("dormant id for ssh-remote-A");

    // Dive into A exactly as the dock does: connect the SSH backend and promote
    // it to a real window, restoring its terminal through that authority.
    h.editor_mut()
        .handle_plugin_command(PluginCommand::SetActiveWindow { id: a_id })?;

    // Wait for the connect + promote to land and the terminal to restore.
    h.wait_until(|h| h.screen_to_string().contains("*Terminal"))?;

    // Make sure keystrokes go to the PTY (terminal input mode).
    if !h.editor().is_terminal_mode() {
        h.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
        h.wait_until(|h| h.editor().is_terminal_mode())?;
    }

    // Ask the restored shell where it's running. `$SSH_CONNECTION` is set only by
    // sshd on the remote side; a locally-spawned shell prints an empty value.
    h.type_text("printf 'CONN=%s\\n' \"$SSH_CONNECTION\"")?;
    h.send_key(KeyCode::Enter, KeyModifiers::NONE)?;

    // The marker carries the loopback address only when the shell genuinely runs
    // through SSH — proving the *restored* terminal connected to the remote host.
    h.wait_until(|h| h.screen_to_string().contains("CONN=127.0.0.1"))?;

    Ok(())
}

fn canonical_mkdir(base: &Path, name: &str) -> anyhow::Result<PathBuf> {
    let p = base.join(name);
    std::fs::create_dir_all(&p)?;
    Ok(canonical(&p))
}
