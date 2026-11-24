//! Hosts ("authorities"): where a workspace's files live and its processes
//! run — this machine, another one over SSH, or a container.
//!
//! An authority supplies two things: the host's filesystem, and the argv that
//! runs a POSIX `sh` script on the host ([`Authority::sh`]). Every process —
//! a client's `proc.run`, a terminal's shell, a language server, the probe
//! that finds the servers — is that script around the program, built here
//! once for every host:
//!
//! - **Environment.** On another host a program runs in the user's login
//!   environment there (`$SHELL -l`), so `PATH` is what an interactive login
//!   sees; on this machine it inherits this server's environment, which is
//!   the user's. A login profile's output to stdout is discarded, so it never
//!   reaches a program's stream (a language server's protocol, for one).
//! - **Stopping.** The script records its pid in a file on the host. Every
//!   authority starts its scripts as process-group leaders (sshd and `docker
//!   exec` start each as a session; here the server starts it as its own
//!   group), so [`Authority::stop`] signals the whole group on the host, not
//!   just the local `ssh` or `docker` client. A program's script stays as its
//!   parent and removes the pid file when it ends; a terminal's script
//!   becomes the shell, so the shell leads the session and a hang-up reaches
//!   its jobs the way closing a terminal does.

use crate::workspace::Fs;
use fresh::services::remote::ConnectionParams;
use fresh_editor_core::model::filesystem::StdFileSystem;
use std::path::Path;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, OnceLock};

/// A process started on a host, as the host knows it: the stem of its pid
/// file there, and the signal that stops it.
#[derive(Clone, Debug)]
pub struct HostProc {
    stem: String,
    signal: &'static str,
}

pub trait Authority: Send + Sync {
    /// A short name for the host, shown to the user.
    fn name(&self) -> String;
    /// The host's filesystem.
    fn fs(&self) -> Fs;
    /// The argv that runs the POSIX `sh` script `script` on the host, on a
    /// PTY when `tty`.
    fn sh(&self, script: &str, tty: bool) -> Vec<String>;
    /// Whether programs take the user's login environment on the host
    /// (another host) rather than this server's (this machine).
    fn login_env(&self) -> bool;

    /// `program` with `args` in `cwd` on the host, to be started with its
    /// stdio as the caller sets it.
    fn command(&self, program: &str, args: &[String], cwd: &Path) -> (Command, HostProc) {
        let p = HostProc::new("KILL");
        let mut line = sh_quote(program);
        for a in args {
            line.push(' ');
            line.push_str(&sh_quote(a));
        }
        let run = if self.login_env() {
            // The profile writes to a discarded stdout; the program gets
            // the real one back (fd 3).
            format!(
                "\"${{SHELL:-/bin/sh}}\" -lc {} 3>&1 1>/dev/null",
                sh_quote(&format!("exec {line} 1>&3 3>&-"))
            )
        } else {
            line
        };
        let script = [
            p.record(),
            format!(
                "cd {} || {{ rm -f \"$F.pid\"; exit 1; }}",
                sh_quote(&cwd.to_string_lossy())
            ),
            run,
            "s=$?".into(),
            "rm -f \"$F.pid\"".into(),
            "exit $s".into(),
        ]
        .join("\n");
        (local_command(&self.sh(&script, false)), p)
    }

    /// The user's login shell in `cwd` on the host, to be started on a PTY:
    /// interactive, or running `command` (a shell command line) and ending
    /// with it.
    fn shell_command(
        &self,
        cwd: &Path,
        command: Option<&str>,
    ) -> (portable_pty::CommandBuilder, HostProc) {
        let p = HostProc::new("HUP");
        let script = [
            p.record(),
            format!("cd {}", sh_quote(&cwd.to_string_lossy())),
            "TERM=xterm-256color COLORTERM=truecolor".into(),
            "export TERM COLORTERM".into(),
            match command {
                None => "exec \"${SHELL:-/bin/sh}\" -l".into(),
                Some(c) => format!("exec \"${{SHELL:-/bin/sh}}\" -lc {}", sh_quote(c)),
            },
        ]
        .join("\n");
        let argv = self.sh(&script, true);
        let mut c = portable_pty::CommandBuilder::new(&argv[0]);
        c.args(&argv[1..]);
        (c, p)
    }

    /// Signal `p`'s process group on the host; one not started yet exits as
    /// soon as it starts. Returns once the host has run the signal.
    fn stop(&self, p: &HostProc) {
        let script = format!(
            "F={}\ntouch \"$F.kill\"\nif [ -f \"$F.pid\" ]; then kill -s {} -- \"-$(cat \"$F.pid\")\" 2>/dev/null; rm -f \"$F.pid\" \"$F.kill\"; fi\ntrue",
            sh_quote(&p.stem),
            p.signal
        );
        run_quietly(local_command(&self.sh(&script, false)));
    }

    /// Remove what `p` left on the host once it has ended: a terminal's pid
    /// file, or the stop marker of a program stopped as it was ending.
    fn release(&self, p: &HostProc) {
        let script = format!("F={}\nrm -f \"$F.pid\" \"$F.kill\"", sh_quote(&p.stem));
        run_quietly(local_command(&self.sh(&script, false)));
    }
}

impl HostProc {
    fn new(signal: &'static str) -> Self {
        static INSTANCE: OnceLock<String> = OnceLock::new();
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let instance = INSTANCE.get_or_init(|| {
            let nanos = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0);
            format!("{}-{nanos}", std::process::id())
        });
        let n = NEXT.fetch_add(1, Ordering::SeqCst);
        HostProc {
            stem: format!("/tmp/fresh-poc-{instance}-{n}"),
            signal,
        }
    }

    /// The script's first lines: record its pid, and exit at once if it
    /// was stopped before it started.
    fn record(&self) -> String {
        format!(
            "F={}\necho $$ >\"$F.pid\"\nif [ -e \"$F.kill\" ]; then rm -f \"$F.pid\" \"$F.kill\"; exit 143; fi",
            sh_quote(&self.stem)
        )
    }
}

/// A local process from an argv, leading its own process group, so the host
/// process it is (here) or reaches (over ssh or docker) can be signalled as
/// a group, and a signal to this server's group does not reach it.
fn local_command(argv: &[String]) -> Command {
    let mut c = Command::new(&argv[0]);
    c.args(&argv[1..]);
    #[cfg(unix)]
    std::os::unix::process::CommandExt::process_group(&mut c, 0);
    c
}

fn run_quietly(mut c: Command) {
    let _ = c
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status();
}

/// Quote a word for a POSIX shell.
pub fn sh_quote(s: &str) -> String {
    if !s.is_empty()
        && s.bytes()
            .all(|b| b.is_ascii_alphanumeric() || b"-_./=:@%+,".contains(&b))
    {
        s.to_string()
    } else {
        format!("'{}'", s.replace('\'', "'\\''"))
    }
}

/// This machine.
pub struct Local {
    pub fs: Fs,
}

impl Local {
    pub fn new() -> Self {
        Local {
            fs: Arc::new(StdFileSystem),
        }
    }
}

fn local_sh(script: &str) -> Vec<String> {
    vec!["sh".into(), "-c".into(), script.into()]
}

impl Authority for Local {
    fn name(&self) -> String {
        "local".into()
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn sh(&self, script: &str, _tty: bool) -> Vec<String> {
        local_sh(script)
    }
    fn login_env(&self) -> bool {
        false
    }
}

/// Another machine over SSH: files through the editor's remote agent (one
/// persistent connection, ranged reads, newline counting and searching on
/// the remote side); processes and terminals through `ssh`, with the port,
/// identity and options of the agent's connection.
pub struct Ssh {
    pub params: ConnectionParams,
    pub fs: Fs,
    /// Keeps the connection's async tasks alive.
    pub _runtime: tokio::runtime::Runtime,
    pub _conn: fresh::services::remote::SshConnection,
}

/// `ssh … <target> 'exec sh -c <script>'`. The remote account's shell parses
/// only that line (it may not be POSIX). Without a PTY auth never prompts;
/// on one (a terminal) it may, in the terminal.
fn ssh_sh(params: &ConnectionParams, script: &str, tty: bool) -> Vec<String> {
    let mut a: Vec<String> = vec!["ssh".into()];
    if tty {
        a.push("-t".into());
    } else {
        a.extend(["-o".into(), "BatchMode=yes".into()]);
    }
    a.extend(["-o".into(), "StrictHostKeyChecking=accept-new".into()]);
    if let Some(port) = params.port {
        a.extend(["-p".into(), port.to_string()]);
    }
    if let Some(id) = &params.identity_file {
        a.extend(["-i".into(), id.to_string_lossy().into_owned()]);
    }
    a.extend(params.extra_args.iter().cloned());
    a.push(params.ssh_target());
    a.push(format!("exec sh -c {}", sh_quote(script)));
    a
}

impl Authority for Ssh {
    fn name(&self) -> String {
        format!("ssh {}", self.params)
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn sh(&self, script: &str, tty: bool) -> Vec<String> {
        ssh_sh(&self.params, script, tty)
    }
    fn login_env(&self) -> bool {
        true
    }
}

/// A container whose workspace is bind-mounted at the same path, as in a
/// devcontainer: files stay on this machine; processes, terminals and
/// language servers run in the container through `docker exec`.
pub struct Container {
    /// The container's name or id.
    pub id: String,
    pub fs: Fs,
}

impl Authority for Container {
    fn name(&self) -> String {
        format!("container {}", self.id)
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn sh(&self, script: &str, tty: bool) -> Vec<String> {
        // `-i` keeps stdin open: a language server is spoken to on it.
        let flags = if tty { "-it" } else { "-i" };
        ["docker", "exec", flags, &self.id, "sh", "-c", script]
            .map(String::from)
            .to_vec()
    }
    fn login_env(&self) -> bool {
        true
    }
}

/// The editor's remote agent run on this machine, without ssh: the whole
/// remote path (agent protocol, ranged reads, remote-side counting and
/// searching) with only the transport left out. Processes and terminals run
/// locally.
pub struct Agent {
    pub fs: Fs,
    pub _runtime: tokio::runtime::Runtime,
}

impl Authority for Agent {
    fn name(&self) -> String {
        "agent".into()
    }
    fn fs(&self) -> Fs {
        self.fs.clone()
    }
    fn sh(&self, script: &str, _tty: bool) -> Vec<String> {
        local_sh(script)
    }
    fn login_env(&self) -> bool {
        false
    }
}

/// Which host a workspace is on, as the command line and `workspace.open`
/// name it.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum HostSpec {
    /// This machine.
    Local,
    /// Another machine, `[user@]host[:port]`, through the editor's remote
    /// agent over `ssh`.
    Ssh { target: String },
    /// A running container: files here, processes through `docker exec`.
    Container { id: String },
    /// The remote agent on this machine, without `ssh`.
    Agent,
}

impl HostSpec {
    /// The host as the user names it.
    pub fn label(&self) -> String {
        match self {
            HostSpec::Local => "local".into(),
            HostSpec::Ssh { target } => format!("ssh {target}"),
            HostSpec::Container { id } => format!("container {id}"),
            HostSpec::Agent => "agent".into(),
        }
    }
}

/// Reach the host `spec` names; `slow` adds network-like delays to every
/// filesystem call.
pub fn connect(spec: &HostSpec, slow: bool) -> anyhow::Result<Arc<dyn Authority>> {
    use anyhow::anyhow;
    Ok(match spec {
        HostSpec::Ssh { target } => {
            let params = ConnectionParams::parse(target)
                .ok_or_else(|| anyhow!("bad ssh target {target}"))?;
            let runtime = tokio::runtime::Runtime::new()?;
            let conn = runtime.block_on(fresh::services::remote::SshConnection::connect(
                params.clone(),
            ))?;
            let rfs = fresh::services::remote::RemoteFileSystem::new(
                conn.channel(),
                conn.connection_string(),
            );
            runtime.block_on(rfs.prime_sys_info())?;
            Arc::new(Ssh {
                params,
                fs: maybe_slow(Arc::new(rfs), slow),
                _runtime: runtime,
                _conn: conn,
            })
        }
        HostSpec::Container { id } => Arc::new(Container {
            id: id.clone(),
            fs: maybe_slow(Arc::new(StdFileSystem), slow),
        }),
        HostSpec::Agent => {
            let runtime = tokio::runtime::Runtime::new()?;
            let channel = runtime.block_on(fresh::services::remote::spawn_local_agent())?;
            let rfs = fresh::services::remote::RemoteFileSystem::new(channel, "agent".into());
            runtime.block_on(rfs.prime_sys_info())?;
            Arc::new(Agent {
                fs: maybe_slow(Arc::new(rfs), slow),
                _runtime: runtime,
            })
        }
        HostSpec::Local => {
            let mut l = Local::new();
            l.fs = maybe_slow(l.fs, slow);
            Arc::new(l)
        }
    })
}

/// The filesystem, with network-like delays when asked for.
fn maybe_slow(fs: Fs, slow: bool) -> Fs {
    if slow {
        Arc::new(fresh::services::fs::slow::SlowFileSystem::new(
            fs,
            fresh::services::fs::slow::SlowFsConfig::slow_network(),
        ))
    } else {
        fs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Read;
    use std::time::{Duration, Instant};

    fn argv(c: &Command) -> Vec<String> {
        std::iter::once(c.get_program())
            .chain(c.get_args())
            .map(|a| a.to_string_lossy().into_owned())
            .collect()
    }

    fn container() -> Container {
        Container {
            id: "dev-1".into(),
            fs: Arc::new(StdFileSystem),
        }
    }

    #[test]
    fn container_runs_scripts_with_docker_exec() {
        let c = container();
        let (cmd, _) = c.command("rust-analyzer", &["--stdio".into()], Path::new("/w s/p"));
        let a = argv(&cmd);
        assert_eq!(a[..6], ["docker", "exec", "-i", "dev-1", "sh", "-c"]);
        assert_eq!(a.len(), 7);
        let script = &a[6];
        assert!(script.contains("cd '/w s/p' ||"), "{script}");
        assert!(
            script.contains(
                "\"${SHELL:-/bin/sh}\" -lc 'exec rust-analyzer --stdio 1>&3 3>&-' 3>&1 1>/dev/null"
            ),
            "{script}"
        );
        let (sh, _) = c.shell_command(Path::new("/w"), None);
        let sh: Vec<String> = sh
            .get_argv()
            .iter()
            .map(|a| a.to_string_lossy().into_owned())
            .collect();
        assert_eq!(sh[..6], ["docker", "exec", "-it", "dev-1", "sh", "-c"]);
        assert!(
            sh[6].ends_with("exec \"${SHELL:-/bin/sh}\" -l"),
            "{}",
            sh[6]
        );
        let (agent, _) = c.shell_command(Path::new("/w"), Some("claude --resume"));
        let agent = agent.get_argv()[6].to_string_lossy().into_owned();
        assert!(
            agent.ends_with("exec \"${SHELL:-/bin/sh}\" -lc 'claude --resume'"),
            "{agent}"
        );
    }

    #[test]
    fn ssh_carries_port_identity_and_options() {
        let mut p = ConnectionParams::parse("me@box:2222").unwrap();
        p.identity_file = Some("/k/id".into());
        p.extra_args = vec!["-J".into(), "jump".into()];
        let a = ssh_sh(&p, "echo 'hi'", false);
        assert_eq!(
            a,
            [
                "ssh",
                "-o",
                "BatchMode=yes",
                "-o",
                "StrictHostKeyChecking=accept-new",
                "-p",
                "2222",
                "-i",
                "/k/id",
                "-J",
                "jump",
                "me@box",
                "exec sh -c 'echo '\\''hi'\\'''",
            ]
        );
        let t = ssh_sh(&p, "x", true);
        assert_eq!(t[..2], ["ssh", "-t"]);
        assert!(!t.iter().any(|a| a == "BatchMode=yes"));
    }

    #[test]
    fn shell_quoting() {
        assert_eq!(sh_quote("plain/path-1.rs"), "plain/path-1.rs");
        assert_eq!(sh_quote(""), "''");
        assert_eq!(sh_quote("it's"), "'it'\\''s'");
        assert_eq!(sh_quote("$HOME"), "'$HOME'");
    }

    /// This machine, as a remote host is treated: programs in a login
    /// environment.
    struct LoginHere;

    impl Authority for LoginHere {
        fn name(&self) -> String {
            "login-here".into()
        }
        fn fs(&self) -> Fs {
            Arc::new(StdFileSystem)
        }
        fn sh(&self, script: &str, _tty: bool) -> Vec<String> {
            local_sh(script)
        }
        fn login_env(&self) -> bool {
            true
        }
    }

    #[test]
    fn programs_take_the_login_environment_without_its_output() {
        let dir = tempfile::tempdir().unwrap();
        // A login shell whose profile prints and sets a variable.
        let shell = dir.path().join("login-sh");
        std::fs::write(
            &shell,
            "#!/bin/sh\n[ \"$1\" = -lc ] || exit 9\necho profile noise\nFROM_PROFILE=yes; export FROM_PROFILE\nexec sh -c \"$2\"\n",
        )
        .unwrap();
        std::fs::set_permissions(&shell, std::os::unix::fs::PermissionsExt::from_mode(0o755))
            .unwrap();
        let (mut cmd, _) = LoginHere.command(
            "sh",
            &["-c".into(), "echo \"$FROM_PROFILE\"; echo err >&2".into()],
            dir.path(),
        );
        let out = cmd.env("SHELL", &shell).output().unwrap();
        assert_eq!(String::from_utf8_lossy(&out.stdout), "yes\n");
        assert_eq!(String::from_utf8_lossy(&out.stderr), "err\n");
        assert!(out.status.success());
    }

    /// Whether a process whose command line is `marker` is running.
    fn running(marker: &str) -> bool {
        std::fs::read_dir("/proc").unwrap().flatten().any(|e| {
            std::fs::read(e.path().join("cmdline")).is_ok_and(|c| {
                let line = String::from_utf8_lossy(&c).replace('\0', " ");
                line.trim_end() == marker
            })
        })
    }

    fn wait_until(mut f: impl FnMut() -> bool) -> bool {
        let end = Instant::now() + Duration::from_secs(10);
        while Instant::now() < end {
            if f() {
                return true;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        false
    }

    #[test]
    fn stop_signals_the_process_group_on_the_host() {
        let l = Local::new();
        let dir = tempfile::tempdir().unwrap();
        // A program with a child of its own: both go.
        let (mut cmd, p) = l.command(
            "sh",
            &["-c".into(), "sleep 7301 & sleep 7302; wait".into()],
            dir.path(),
        );
        let mut child = cmd.spawn().unwrap();
        assert!(wait_until(|| running("sleep 7301") && running("sleep 7302")));
        l.stop(&p);
        assert!(wait_until(
            || !running("sleep 7301") && !running("sleep 7302")
        ));
        child.wait().unwrap();
        assert!(!Path::new(&format!("{}.pid", p.stem)).exists());
        assert!(!Path::new(&format!("{}.kill", p.stem)).exists());
    }

    #[test]
    fn a_program_stopped_before_it_starts_never_runs() {
        let l = Local::new();
        let (mut cmd, p) = l.command("echo", &["ran".into()], Path::new("/"));
        l.stop(&p);
        let out = cmd.output().unwrap();
        assert!(out.stdout.is_empty());
        assert!(!out.status.success());
        assert!(!Path::new(&format!("{}.kill", p.stem)).exists());
    }

    #[test]
    fn a_program_ends_cleanly_and_keeps_its_streams() {
        let l = Local::new();
        let dir = tempfile::tempdir().unwrap();
        let (mut cmd, p) = l.command("sh", &["-c".into(), "pwd; cat; exit 3".into()], dir.path());
        let mut child = cmd
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .unwrap();
        use std::io::Write;
        child.stdin.take().unwrap().write_all(b"in\n").unwrap();
        let mut out = String::new();
        child
            .stdout
            .take()
            .unwrap()
            .read_to_string(&mut out)
            .unwrap();
        assert_eq!(child.wait().unwrap().code(), Some(3));
        let root = dir.path().canonicalize().unwrap();
        assert_eq!(out, format!("{}\nin\n", root.display()));
        assert!(!Path::new(&format!("{}.pid", p.stem)).exists());
    }
}
