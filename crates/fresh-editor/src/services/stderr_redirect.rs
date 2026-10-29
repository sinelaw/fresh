//! Keep stray stderr output off the TUI.
//!
//! While the editor owns the terminal, fd 2 is the same tty the frame is
//! drawn on. Anything that writes there lands on top of the UI at whatever
//! position the cursor was last left: a native library's diagnostic (macOS
//! AppKit logs `-[NSPasteboard _setData:forType:index:usesPboardTypes:]
//! returns false` when a clipboard write loses a race, #3662), or a child
//! process that inherited stderr. The differential renderer doesn't know
//! those cells changed, so the text sticks until a full redraw.
//!
//! [`redirect_to_log`] points fd 2 at `stderr-{PID}.log` for as long as the
//! TUI is up; [`restore`] puts the original stderr back, so messages printed
//! after teardown (panics, exit errors) still reach the user.

#[cfg(unix)]
use std::sync::atomic::{AtomicI32, Ordering};

/// Duplicate of the original fd 2 while redirected, `-1` otherwise.
#[cfg(unix)]
static SAVED_STDERR: AtomicI32 = AtomicI32::new(-1);

/// Redirect fd 2 to this process's stderr log, if fd 2 is a terminal.
///
/// A stderr the user already redirected (`fresh 2>err.txt`) is theirs and is
/// left alone. Returns true only if this call did the redirect, so the caller
/// knows the matching [`restore`] is its to make; a call while already
/// redirected does nothing and returns false.
pub fn redirect_to_log() -> bool {
    #[cfg(unix)]
    {
        use std::io::IsTerminal;
        if !std::io::stderr().is_terminal() {
            return false;
        }
        redirect_to(&crate::services::log_dirs::stderr_log_path())
    }
    #[cfg(not(unix))]
    false
}

/// Point fd 2 at `path` (appending), saving the original for [`restore`].
#[cfg(unix)]
fn redirect_to(path: &std::path::Path) -> bool {
    use std::os::fd::AsRawFd;

    if SAVED_STDERR.load(Ordering::SeqCst) >= 0 {
        return false;
    }
    let file = match std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
    {
        Ok(file) => file,
        Err(e) => {
            tracing::debug!("Cannot open stderr log {}: {}", path.display(), e);
            return false;
        }
    };
    // SAFETY: plain fd syscalls on fds we own (`file`) or that the process
    // was started with (fd 2). The saved copy is close-on-exec so children
    // never inherit the tty through it.
    unsafe {
        let saved = libc::fcntl(libc::STDERR_FILENO, libc::F_DUPFD_CLOEXEC, 0);
        if saved < 0 {
            return false;
        }
        if libc::dup2(file.as_raw_fd(), libc::STDERR_FILENO) < 0 {
            libc::close(saved);
            return false;
        }
        SAVED_STDERR.store(saved, Ordering::SeqCst);
    }
    tracing::debug!("Redirected stderr to {}", path.display());
    true
}

/// Put back the stderr saved by [`redirect_to_log`]. No-op if not redirected.
///
/// Async-signal-safe (an atomic swap plus `dup2`/`close`), so the panic hook
/// and the fatal-signal handler can call it via `emergency_cleanup`.
pub fn restore() {
    #[cfg(unix)]
    {
        let saved = SAVED_STDERR.swap(-1, Ordering::SeqCst);
        if saved >= 0 {
            // SAFETY: `saved` is the duplicate taken in `redirect_to`, owned
            // here exclusively thanks to the swap.
            unsafe {
                libc::dup2(saved, libc::STDERR_FILENO);
                libc::close(saved);
            }
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn redirect_sends_fd2_to_the_log_and_restore_puts_it_back() {
        let dir = tempfile::tempdir().unwrap();
        let log = dir.path().join("stderr-test.log");
        let before = std::fs::metadata("/proc/self/fd/2").ok();

        assert!(redirect_to(&log));
        // A raw write to fd 2, the way a native library's NSLog would do it.
        let msg = b"native diagnostic\n";
        // SAFETY: writing a valid buffer to fd 2.
        let n = unsafe { libc::write(libc::STDERR_FILENO, msg.as_ptr().cast(), msg.len()) };
        assert_eq!(n, msg.len() as isize);
        // Idempotent while redirected: the saved original is not overwritten
        // with the log file.
        assert!(!redirect_to(&log));
        restore();
        restore();

        assert_eq!(std::fs::read(&log).unwrap(), msg);
        // fd 2 refers to the original file again.
        if let (Some(before), Ok(after)) = (before, std::fs::metadata("/proc/self/fd/2")) {
            use std::os::unix::fs::MetadataExt;
            assert_eq!((before.dev(), before.ino()), (after.dev(), after.ino()));
        }
    }
}
