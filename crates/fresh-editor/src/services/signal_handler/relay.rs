//! The one thing a signal handler may safely do: write a byte to a pipe.
//!
//! POSIX gives a signal handler a very short list of calls it may make
//! (`signal-safety(7)`). `write(2)` is on it; `malloc`, any mutex, anything
//! that formats a string, and anything that unwinds are not. A handler that
//! reaches for one of those can deadlock against the thread it interrupted —
//! that thread is stopped mid-`malloc` and cannot release the lock the
//! handler now wants — and a `panic!` inside an `extern "C"` handler aborts
//! the process outright.
//!
//! So the handler does nothing but hand the signal to a thread that was
//! already waiting for it, and returns immediately. The interrupted thread
//! resumes, whatever lock it held is released on its own schedule, and the
//! reporting happens in ordinary Rust with no restrictions at all.
//!
//! The signal number travels as the byte itself: every signal number fits in
//! a `u8`, so one `write` says both *something happened* and *what*.

use std::io;
use std::os::unix::io::RawFd;

/// A self-pipe carrying one signal number from a handler to a waiting thread.
///
/// The waiting side owns this; the handler side needs only the raw
/// [`notify_fd`](Self::notify_fd), which it reads out of an atomic. That
/// keeps the handler's view to a plain `i32` load and a `write`, with no
/// `OnceLock`, no `Option`, and nothing to dereference.
#[derive(Debug)]
pub struct Relay {
    read_fd: RawFd,
    write_fd: RawFd,
}

impl Relay {
    /// Create the pipe. Both ends are close-on-exec: a child process has no
    /// business inheriting our wakeup channel.
    pub fn new() -> io::Result<Self> {
        let mut fds = [0 as RawFd; 2];
        // SAFETY: `fds` is a two-element array, which is what `pipe` writes.
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            return Err(io::Error::last_os_error());
        }

        for fd in fds {
            // SAFETY: `fd` was just returned by `pipe`. A failure here only
            // means the fd survives an exec, which is untidy rather than
            // unsound, so it is not worth failing construction over.
            unsafe {
                let flags = libc::fcntl(fd, libc::F_GETFD);
                if flags >= 0 {
                    libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC);
                }
            }
        }

        Ok(Self {
            read_fd: fds[0],
            write_fd: fds[1],
        })
    }

    /// The descriptor a signal handler writes to. Publish this into an
    /// atomic before installing the handler, and pass it to [`notify`].
    pub fn notify_fd(&self) -> RawFd {
        self.write_fd
    }

    /// Block until a handler notifies, and return the signal it saw.
    ///
    /// Blocking is the point: the waiting thread costs nothing while it
    /// waits, so there is no polling loop and no interval to tune.
    pub fn wait(&self) -> io::Result<libc::c_int> {
        let mut byte = [0u8; 1];
        loop {
            // SAFETY: reading one byte into a stack buffer we own.
            let n = unsafe { libc::read(self.read_fd, byte.as_mut_ptr().cast(), 1) };
            match n {
                1 => return Ok(byte[0] as libc::c_int),
                0 => {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "signal relay closed",
                    ))
                }
                _ => {
                    let e = io::Error::last_os_error();
                    // A signal delivered to *this* thread interrupts the
                    // read; that is not the notification we are waiting for.
                    if e.kind() == io::ErrorKind::Interrupted {
                        continue;
                    }
                    return Err(e);
                }
            }
        }
    }
}

impl Drop for Relay {
    fn drop(&mut self) {
        // SAFETY: both descriptors are ours and are not used after this.
        unsafe {
            libc::close(self.read_fd);
            libc::close(self.write_fd);
        }
    }
}

/// Hand `sig` to whoever is waiting on `fd`. **Callable from a signal
/// handler**, and deliberately the only thing that is.
///
/// Returns whether the byte was delivered, so a caller with nobody listening
/// can fall back rather than swallow a terminating signal.
///
/// `fd` is passed rather than read from a `Relay` so that this needs no
/// reference, no lifetime and no dereference — the handler holds a bare
/// `i32` it loaded from an atomic.
pub fn notify(fd: RawFd, sig: libc::c_int) -> bool {
    if fd < 0 {
        return false;
    }
    let byte = [sig as u8];
    // SAFETY: `write` is async-signal-safe. A partial write is impossible
    // for a single byte, and a full pipe (EAGAIN) or a closed reader
    // (EPIPE) is reported as "not delivered" rather than retried, because a
    // handler must not loop.
    let n = unsafe { libc::write(fd, byte.as_ptr().cast(), 1) };
    n == 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};

    /// Where the test's handler finds the descriptor, exactly as the real
    /// handler does.
    static TEST_FD: AtomicI32 = AtomicI32::new(-1);

    extern "C" fn test_handler(sig: libc::c_int) {
        notify(TEST_FD.load(Ordering::SeqCst), sig);
    }

    /// The whole point: a real signal, delivered to a real handler, arrives
    /// on a normal thread with nothing async-signal-unsafe in between.
    ///
    /// `SIGUSR2` because nothing else in the editor uses it, and `raise`
    /// delivers to this very thread — so the handler runs, returns, and the
    /// blocking `wait` below picks the signal up afterwards.
    #[test]
    fn a_signal_handler_hands_the_signal_to_a_waiting_thread() {
        let relay = Relay::new().expect("create relay");
        TEST_FD.store(relay.notify_fd(), Ordering::SeqCst);

        use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
        let action = SigAction::new(
            SigHandler::Handler(test_handler),
            SaFlags::empty(),
            SigSet::empty(),
        );

        // SAFETY: installing a handler whose body is async-signal-safe.
        let previous = unsafe { sigaction(Signal::SIGUSR2, &action).expect("install handler") };
        // SAFETY: `raise` is async-signal-safe and targets this process.
        unsafe { libc::raise(libc::SIGUSR2) };

        assert_eq!(
            relay.wait().expect("the relay should carry the signal"),
            libc::SIGUSR2,
            "the byte should name the signal that arrived"
        );

        // SAFETY: restoring whatever disposition the test found.
        unsafe { sigaction(Signal::SIGUSR2, &previous).expect("restore handler") };
    }

    /// Two signals do not collapse into one wakeup: the pipe buffers them,
    /// so a second terminating signal is still visible to the waiter.
    #[test]
    fn each_notification_is_delivered_in_order() {
        let relay = Relay::new().expect("create relay");

        assert!(notify(relay.notify_fd(), libc::SIGTERM));
        assert!(notify(relay.notify_fd(), libc::SIGINT));

        assert_eq!(relay.wait().unwrap(), libc::SIGTERM);
        assert_eq!(relay.wait().unwrap(), libc::SIGINT);
    }

    /// With nowhere to send it, `notify` says so rather than silently
    /// dropping a terminating signal — which is what lets the real handler
    /// fall back to the default disposition instead of ignoring a SIGTERM.
    #[test]
    fn notifying_nobody_reports_failure() {
        assert!(!notify(-1, libc::SIGTERM), "an unset fd is not a delivery");
    }
}
