//! Integration test for the remote-mode integrated terminal.
//!
//! Regression for: when running `fresh ssh://host/path`, opening the
//! embedded terminal ran a shell on the *local* machine instead of on the
//! remote host. The fix builds an `ssh -t … user@host 'cd <dir>; exec
//! $SHELL -l'` wrapper for the SSH authority.
//!
//! This test spins up a throwaway, **non-root** `sshd` on `127.0.0.1` and
//! runs the real [`TerminalWrapper::ssh`] command against it — the same
//! command the terminal manager spawns. The shell's `$SSH_CONNECTION` is
//! set *only* by sshd on the remote side, so observing a non-empty value
//! proves the shell genuinely runs through SSH rather than locally. We
//! also check `pwd` to confirm the wrapper lands in the workspace dir.
//!
//! Linux-only: the non-root sshd bring-up relies on OpenSSH's Linux
//! behaviour (a non-root daemon happily serves logins for the user that
//! launched it). The test *skips* (rather than fails) when
//! `ssh`/`sshd`/`ssh-keygen` aren't installed, so it's a no-op on hosts
//! without OpenSSH.
#![cfg(target_os = "linux")]

use std::io::Write;
use std::net::{TcpListener, TcpStream};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use fresh::services::authority::TerminalWrapper;
use fresh::services::remote::ConnectionParams;

/// True if `p` is a regular file (good enough to treat as an executable
/// candidate here — we only ever feed these to `Command`).
fn is_file(p: &Path) -> bool {
    p.is_file()
}

/// Resolve a program by searching `PATH` and then a few well-known
/// absolute fallbacks (sshd usually lives in `/usr/sbin`, which is often
/// absent from a test process's `PATH`).
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

/// Generate an unencrypted ed25519 keypair at `path` (+ `path.pub`).
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

/// A free localhost TCP port. Bound briefly to discover the number, then
/// released for `sshd` to claim — a small race window that's fine for a
/// loopback test.
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

/// Kill the child `sshd` when the test ends (pass or panic).
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

#[test]
fn ssh_terminal_wrapper_runs_shell_on_remote_host() {
    let (Some(ssh), Some(sshd), Some(ssh_keygen)) = (
        resolve("ssh", &[]),
        resolve(
            "sshd",
            &["/usr/sbin/sshd", "/sbin/sshd", "/usr/local/sbin/sshd"],
        ),
        resolve("ssh-keygen", &[]),
    ) else {
        eprintln!("skipping: ssh/sshd/ssh-keygen not installed");
        return;
    };
    let _ = ssh; // resolved only to confirm the client exists; wrapper calls `ssh` by name.

    let Some(user) = current_user() else {
        eprintln!("skipping: could not determine current user");
        return;
    };

    let tmp = tempfile::tempdir().expect("tempdir");
    let t = tmp.path();
    let hostkey = t.join("hostkey");
    let id = t.join("id");
    let authorized = t.join("authorized_keys");
    let config = t.join("sshd_config");
    let work = t.join("work");
    std::fs::create_dir(&work).unwrap();
    // accept-new writes the host key here; pre-create so we never touch the
    // real user's ~/.ssh.
    let dot_ssh = t.join(".ssh");
    std::fs::create_dir(&dot_ssh).unwrap();
    set_mode(&dot_ssh, 0o700);

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
    let sshd_child = Command::new(&sshd)
        .arg("-D") // foreground
        .arg("-e") // log to stderr
        .arg("-f")
        .arg(&config)
        .stdout(Stdio::from(logf.try_clone().unwrap()))
        .stderr(Stdio::from(logf))
        .spawn()
        .expect("spawn sshd");
    let _sshd_guard = KillOnDrop(sshd_child);

    assert!(
        wait_for_listen(port, Duration::from_secs(10)),
        "sshd never listened on {port}.\nsshd log:\n{}",
        std::fs::read_to_string(&log).unwrap_or_default()
    );

    // --- The code under test: build the SSH authority's terminal wrapper. ---
    let params = ConnectionParams {
        user: user.clone(),
        host: "127.0.0.1".to_string(),
        port: Some(port),
        identity_file: Some(id.clone()),
    };
    let work_str = work.to_string_lossy().into_owned();
    let wrapper = TerminalWrapper::ssh(&params, Some(&work_str));
    assert_eq!(
        wrapper.command, "ssh",
        "remote terminal must launch via ssh"
    );
    assert!(
        wrapper.manages_cwd,
        "ssh terminal re-parents the shell, so it manages its own cwd"
    );

    // Spawn the wrapper exactly as the terminal manager would, feeding a
    // tiny script on stdin. HOME is redirected into the temp dir so the
    // accept-new host key lands there rather than the real user's file.
    let mut child = Command::new(&wrapper.command)
        .args(&wrapper.args)
        .env("HOME", t)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ssh terminal wrapper");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"pwd\nprintf 'CONN=%s\\n' \"$SSH_CONNECTION\"\nexit\n")
        .unwrap();
    let out = child.wait_with_output().expect("wait for ssh wrapper");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);

    // (1) The shell ran on the *remote* side. `$SSH_CONNECTION` is injected
    //     only by sshd for a real SSH session; a locally-spawned shell would
    //     print an empty "CONN=".
    let conn = stdout
        .lines()
        .find(|l| l.starts_with("CONN="))
        .unwrap_or_else(|| panic!("no CONN line.\nstdout:\n{stdout}\nstderr:\n{stderr}"));
    assert!(
        conn.len() > "CONN=".len() && conn.contains("127.0.0.1"),
        "SSH_CONNECTION empty — the terminal shell did NOT run through SSH.\n\
         line={conn:?}\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );

    // (2) The wrapper cd'd into the workspace directory before exec'ing the
    //     shell, so the user lands where the editor is rooted.
    assert!(
        stdout.contains(&work_str),
        "shell did not start in the workspace dir {work_str:?}.\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
}
