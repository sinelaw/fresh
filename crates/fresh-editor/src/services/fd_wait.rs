//! Waiting for file descriptors to become readable, including the one
//! descriptor macOS's poll(2) cannot wait on (#3683).
//!
//! # Why not plain poll(2)
//!
//! When stdin is not the terminal (`cmd | fresh`, `fresh file </dev/null`),
//! `main` reopens fd 0 from `/dev/tty`. On macOS, poll(2) does not support
//! that device: it returns at once with `POLLNVAL`, whether or not a key is
//! pending (`man 2 poll`, BUGS: "The poll() system call currently does not
//! support devices"). The terminal's real device, `/dev/ttysNNN`, works, and
//! so does select(2) on `/dev/tty`.
//!
//! Every key-reading loop here counted only `POLLIN` as readable, so that
//! `POLLNVAL` read as "nothing yet" and the loop asked again immediately:
//! no key ever reached the editor, Ctrl+Q included, and the process spun a
//! core. [`wait_readable`] keeps poll(2) as the wait and falls back to
//! select(2) for the one shape that needs it: `POLLNVAL` on a descriptor that
//! is in fact open. On Linux, poll(2) reports `POLLNVAL` only for a closed
//! descriptor, so the fallback never runs there and behaviour is unchanged.

use std::io;
use std::os::unix::io::RawFd;
use std::time::Duration;

/// The most descriptors one [`wait_readable`] call takes. The callers wait
/// on at most four (stdin, two sockets and GPM's), and a fixed bound keeps
/// the wait free of allocation on the keystroke path.
pub const MAX_FDS: usize = 8;

/// What [`wait_readable`] found, by each descriptor's index in the slice it
/// was handed.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Ready {
    readable: u8,
    hung_up: u8,
}

impl Ready {
    /// Whether the descriptor at `index` is readable.
    pub fn readable(self, index: usize) -> bool {
        index < MAX_FDS && self.readable & (1 << index) != 0
    }

    /// Whether poll(2) reported `POLLHUP` or `POLLERR` for the descriptor at
    /// `index`. The select(2) fallback has no such report; a hung-up
    /// descriptor shows up there as readable, and its read says the rest.
    pub fn hung_up(self, index: usize) -> bool {
        index < MAX_FDS && self.hung_up & (1 << index) != 0
    }

    /// Whether nothing was reported at all: the wait timed out.
    pub fn is_empty(self) -> bool {
        self.readable == 0 && self.hung_up == 0
    }
}

/// Wait up to `timeout` for any of `fds` to become readable.
///
/// A descriptor counts as readable when poll(2) reports `POLLIN` for it —
/// the rule every caller used before — or, on the select(2) fallback, when
/// select(2) puts it in the read set. Returns an empty [`Ready`] on
/// timeout, and the error (`Interrupted` for `EINTR`, e.g. a `SIGWINCH`)
/// when the wait itself fails.
pub fn wait_readable(fds: &[RawFd], timeout: Duration) -> io::Result<Ready> {
    assert!(
        fds.len() <= MAX_FDS,
        "wait_readable takes at most {MAX_FDS} fds"
    );

    let mut pollfds = [libc::pollfd {
        fd: -1,
        events: 0,
        revents: 0,
    }; MAX_FDS];
    for (slot, &fd) in pollfds.iter_mut().zip(fds) {
        *slot = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
    }
    let pollfds = &mut pollfds[..fds.len()];
    let timeout_ms = timeout.as_millis().min(libc::c_int::MAX as u128) as libc::c_int;

    // SAFETY: `pollfds` is an initialized array of `fds.len()` entries that
    // outlives the call.
    let n = unsafe {
        libc::poll(
            pollfds.as_mut_ptr(),
            pollfds.len() as libc::nfds_t,
            timeout_ms,
        )
    };
    if n < 0 {
        return Err(io::Error::last_os_error());
    }

    let mut ready = Ready::default();
    if n == 0 {
        return Ok(ready);
    }
    for (i, p) in pollfds.iter().enumerate() {
        if p.revents & libc::POLLNVAL != 0 && fd_is_open(p.fd) {
            // An open descriptor poll(2) cannot wait on: macOS's `/dev/tty`.
            // Nothing poll said about the others is worth keeping, because
            // the wait itself has not happened yet — select(2) does it.
            return select_readable(fds, timeout);
        }
        if p.revents & libc::POLLIN != 0 {
            ready.readable |= 1 << i;
        }
        if p.revents & (libc::POLLHUP | libc::POLLERR) != 0 {
            ready.hung_up |= 1 << i;
        }
    }
    Ok(ready)
}

/// Whether `fd` names an open descriptor. Distinguishes macOS's "poll cannot
/// wait on this device" `POLLNVAL` from the ordinary closed-fd one.
fn fd_is_open(fd: RawFd) -> bool {
    // SAFETY: F_GETFD only reads the descriptor's flags.
    unsafe { libc::fcntl(fd, libc::F_GETFD) != -1 }
}

/// The select(2) form of [`wait_readable`].
///
/// select(2) cannot name a descriptor at or past `FD_SETSIZE`; such a wait
/// fails with `InvalidInput` instead of touching memory outside the set.
pub(crate) fn select_readable(fds: &[RawFd], timeout: Duration) -> io::Result<Ready> {
    if let Some(&fd) = fds
        .iter()
        .find(|&&fd| fd < 0 || fd as usize >= libc::FD_SETSIZE)
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("fd {fd} cannot be waited on with select(2)"),
        ));
    }

    // SAFETY: an all-zero `fd_set` is a valid empty set (what FD_ZERO makes).
    let mut set: libc::fd_set = unsafe { std::mem::zeroed() };
    let mut max_fd = -1;
    for &fd in fds {
        // SAFETY: `fd` is inside the set, checked above.
        unsafe { libc::FD_SET(fd, &mut set) };
        max_fd = max_fd.max(fd);
    }

    let mut tv = libc::timeval {
        tv_sec: timeout.as_secs().min(i32::MAX as u64) as libc::time_t,
        tv_usec: timeout.subsec_micros() as libc::suseconds_t,
    };
    // SAFETY: `set` and `tv` are initialized locals that outlive the call.
    let n = unsafe {
        libc::select(
            max_fd + 1,
            &mut set,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            &mut tv,
        )
    };
    if n < 0 {
        return Err(io::Error::last_os_error());
    }

    let mut ready = Ready::default();
    for (i, &fd) in fds.iter().enumerate() {
        // SAFETY: `fd` is inside the set, checked above.
        if unsafe { libc::FD_ISSET(fd, &set) } {
            ready.readable |= 1 << i;
        }
    }
    Ok(ready)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::io::{AsRawFd, FromRawFd, OwnedFd};

    fn pipe() -> (OwnedFd, OwnedFd) {
        let mut fds = [0; 2];
        // SAFETY: `fds` has room for the two descriptors pipe(2) writes.
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        // SAFETY: both were just returned by pipe(2) and are owned here.
        unsafe { (OwnedFd::from_raw_fd(fds[0]), OwnedFd::from_raw_fd(fds[1])) }
    }

    fn write_byte(fd: &OwnedFd) {
        // SAFETY: one byte from a local, to a descriptor we own.
        assert_eq!(
            unsafe { libc::write(fd.as_raw_fd(), b"x".as_ptr().cast(), 1) },
            1
        );
    }

    #[test]
    fn poll_path_reports_only_the_readable_fd() {
        let (r1, _w1) = pipe();
        let (r2, w2) = pipe();
        let fds = [r1.as_raw_fd(), r2.as_raw_fd()];

        let idle = wait_readable(&fds, Duration::from_millis(10)).unwrap();
        assert!(idle.is_empty(), "nothing written yet: {idle:?}");

        write_byte(&w2);
        let ready = wait_readable(&fds, Duration::from_secs(5)).unwrap();
        assert!(!ready.readable(0) && ready.readable(1), "{ready:?}");
    }

    /// The fallback that macOS's `/dev/tty` takes, exercised directly since
    /// no Linux descriptor reaches it through `wait_readable`.
    #[test]
    fn select_path_times_out_and_then_reports_the_readable_fd() {
        let (r1, w1) = pipe();
        let (r2, _w2) = pipe();
        let fds = [r1.as_raw_fd(), r2.as_raw_fd()];

        let idle = select_readable(&fds, Duration::from_millis(10)).unwrap();
        assert!(idle.is_empty(), "nothing written yet: {idle:?}");

        write_byte(&w1);
        let ready = select_readable(&fds, Duration::from_secs(5)).unwrap();
        assert!(ready.readable(0) && !ready.readable(1), "{ready:?}");
    }

    /// A closed descriptor is still "not readable", as before — the fallback
    /// is only for one that is open.
    #[test]
    fn a_closed_fd_is_not_readable_and_does_not_reach_select() {
        // Far above anything this process has open, so no other test thread
        // can reuse the number between a close and the wait.
        let closed: RawFd = 999_999;
        assert!(!fd_is_open(closed));
        let ready = wait_readable(&[closed], Duration::from_millis(10)).unwrap();
        assert!(!ready.readable(0) && !ready.hung_up(0), "{ready:?}");
    }

    #[test]
    fn a_pipe_whose_writer_closed_is_reported_hung_up() {
        let (r, w) = pipe();
        drop(w);
        let ready = wait_readable(&[r.as_raw_fd()], Duration::from_secs(5)).unwrap();
        assert!(ready.hung_up(0), "{ready:?}");
    }

    #[test]
    fn select_refuses_fds_it_cannot_represent() {
        let err = select_readable(&[libc::FD_SETSIZE as RawFd], Duration::ZERO).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }
}
