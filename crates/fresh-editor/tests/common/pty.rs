//! A real controlling terminal for tests that drive the `fresh` *binary*.
//!
//! Most tests here drive the editor in-process through `EditorTestHarness`.
//! A few properties only exist in the process itself — what `main` does on
//! its way in and out, and what it forwards to a daemon it spawns — and
//! those need the binary, which in turn needs a terminal it will accept:
//! `fresh` reopens stdin from `/dev/tty` when stdin is a pipe, and the
//! daemon client refuses to attach without a tty.
//!
//! So: allocate a pty, hand the child the slave as its controlling
//! terminal, and read the ANSI it writes back through `vt100` — the same
//! emulator the in-process visual tests use. Reads block until the child
//! writes, which is the semantic wait CONTRIBUTING asks for: no sleeps, no
//! deadlines, and `cargo nextest` supplies the outer timeout.
//!
//! Linux-only. `ptsname_r` is not portable, and the callers are gated on
//! Linux anyway for `XDG_*` isolation.
#![cfg(target_os = "linux")]

use std::ffi::CStr;
use std::fs::File;
use std::io::{Read, Write};
use std::os::unix::io::{AsRawFd, FromRawFd};
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};

/// A pty pair plus the child attached to its slave.
pub struct PtyChild {
    /// Our end. Writing to it is typing; reading from it is the screen.
    master: File,
    child: Child,
    parser: vt100::Parser,
}

/// Where the child's stdin comes from.
pub enum ChildStdin {
    /// The terminal itself — an ordinary interactive launch.
    Terminal,
    /// A pipe carrying `bytes`, closed straight after — `… | fresh -`.
    /// The child still gets the pty as its controlling terminal, which is
    /// exactly the shape `fresh -` needs: a pipe on fd 0 *and* a
    /// `/dev/tty` to reopen for keystrokes.
    Piped(Vec<u8>),
}

/// Spawn `command` with the pty's slave as its controlling terminal.
///
/// `rows`/`cols` size both the pty and the `vt100` screen the output is
/// parsed into.
pub fn spawn_on_pty(
    mut command: Command,
    stdin: ChildStdin,
    cols: u16,
    rows: u16,
) -> std::io::Result<PtyChild> {
    let (master, slave_path) = open_pty()?;
    resize(&master, cols, rows)?;

    let slave = File::options().read(true).write(true).open(&slave_path)?;

    match &stdin {
        ChildStdin::Terminal => {
            command.stdin(Stdio::from(slave.try_clone()?));
        }
        ChildStdin::Piped(_) => {
            command.stdin(Stdio::piped());
        }
    }
    command.stdout(Stdio::from(slave.try_clone()?));
    command.stderr(Stdio::from(slave.try_clone()?));

    // SAFETY: runs in the forked child between fork and exec, where only
    // async-signal-safe calls are allowed. `setsid` and `ioctl` are both
    // such calls and touch no allocator or lock this process holds.
    // `setsid` drops the inherited controlling terminal, which is what lets
    // `TIOCSCTTY` on fd 1 (always the pty slave, in both stdin shapes)
    // adopt ours in its place.
    unsafe {
        command.pre_exec(|| {
            if libc::setsid() == -1 {
                return Err(std::io::Error::last_os_error());
            }
            if libc::ioctl(libc::STDOUT_FILENO, libc::TIOCSCTTY, 0) == -1 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }

    let mut child = command.spawn()?;
    // The parent's copy of the slave must go before any read of the master
    // returns EOF: while *anyone* holds the slave open, a read just blocks.
    drop(slave);

    if let ChildStdin::Piped(bytes) = stdin {
        let mut pipe = child.stdin.take().expect("piped stdin");
        pipe.write_all(&bytes)?;
        // Dropping closes the write end, so the child sees EOF on the pipe.
    }

    Ok(PtyChild {
        master,
        child,
        parser: vt100::Parser::new(rows, cols, 0),
    })
}

impl PtyChild {
    /// Read from the pty until `predicate` holds for the rendered screen.
    ///
    /// Blocks for as long as the child keeps the terminal open. Returns an
    /// error if the child exits (or closes the pty) without the screen ever
    /// satisfying `predicate`, with the last screen in the message so a
    /// failure says what was actually rendered.
    pub fn wait_for_screen<F>(&mut self, predicate: F) -> Result<(), String>
    where
        F: Fn(&str) -> bool,
    {
        let mut buf = [0u8; 8192];
        loop {
            if predicate(&self.screen()) {
                return Ok(());
            }
            match self.master.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => self.parser.process(&buf[..n]),
                // The pty reports the last slave closing as EIO, not EOF.
                Err(e) if e.raw_os_error() == Some(libc::EIO) => break,
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
                Err(e) => return Err(format!("reading from pty: {e}")),
            }
        }

        if predicate(&self.screen()) {
            return Ok(());
        }
        Err(format!(
            "child ended without the expected screen; last screen was:\n{}",
            self.screen()
        ))
    }

    /// The rendered screen, one line per row.
    pub fn screen(&self) -> String {
        let screen = self.parser.screen();
        let (rows, cols) = screen.size();
        (0..rows)
            .map(|row| {
                (0..cols)
                    .map(|col| {
                        screen
                            .cell(row, col)
                            .map(|cell| cell.contents())
                            .unwrap_or_default()
                    })
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Type `bytes` at the child, as a terminal would deliver them.
    pub fn send(&mut self, bytes: &[u8]) -> std::io::Result<()> {
        self.master.write_all(bytes)?;
        self.master.flush()
    }

    /// Wait for the child to exit and report its status.
    pub fn wait(&mut self) -> std::io::Result<std::process::ExitStatus> {
        self.child.wait()
    }

    /// Kill the child and reap it. Safe to call after it already exited.
    pub fn kill(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

impl Drop for PtyChild {
    fn drop(&mut self) {
        // A test that fails mid-wait must not leave an editor running on a
        // pty nobody is reading.
        self.kill();
    }
}

/// `posix_openpt` + `grantpt` + `unlockpt`, returning our end and the path
/// of the slave.
fn open_pty() -> std::io::Result<(File, PathBuf)> {
    // SAFETY: plain libc calls on a fd we own for the rest of this
    // function; every failure is converted from `errno` before use.
    unsafe {
        let master_fd = libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY);
        if master_fd == -1 {
            return Err(std::io::Error::last_os_error());
        }
        let master = File::from_raw_fd(master_fd);

        if libc::grantpt(master_fd) == -1 || libc::unlockpt(master_fd) == -1 {
            return Err(std::io::Error::last_os_error());
        }

        let mut name = [0i8; 128];
        if libc::ptsname_r(master_fd, name.as_mut_ptr(), name.len()) != 0 {
            return Err(std::io::Error::last_os_error());
        }
        let path = CStr::from_ptr(name.as_ptr())
            .to_str()
            .map_err(|e| std::io::Error::other(format!("pty name is not UTF-8: {e}")))?
            .to_string();

        Ok((master, PathBuf::from(path)))
    }
}

fn resize(master: &File, cols: u16, rows: u16) -> std::io::Result<()> {
    let size = libc::winsize {
        ws_row: rows,
        ws_col: cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    // SAFETY: `master` is an open pty master and `size` outlives the call.
    if unsafe { libc::ioctl(master.as_raw_fd(), libc::TIOCSWINSZ, &size) } == -1 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}
