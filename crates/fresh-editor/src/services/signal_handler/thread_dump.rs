//! Backtraces for every thread, collected the only way a process can get
//! them: by asking each thread to look at its own stack.
//!
//! There is no call that reads another thread's stack from outside, so the
//! request travels as `SIGUSR1` and each thread captures its own. That puts
//! a capture inside a signal handler, which is the one place this file
//! cannot make fully safe — unwinding allocates. Two things keep it from
//! being the hazard it was:
//!
//!   * **Symbolisation is deferred.** `Backtrace::force_capture` records
//!     frame addresses; turning them into function names and line numbers
//!     happens on `Display`, which opens files, reads debug info and takes
//!     its own locks. Storing the `Backtrace` and formatting it later moves
//!     all of that off the handler.
//!   * **Nothing blocks.** The shared slot is taken with `try_lock` on both
//!     sides, so neither a capturing thread nor the reporter can ever wait
//!     on the other. A contended capture is dropped; a dropped backtrace is
//!     a worse diagnostic, not a wedged process.
//!
//! What remains — the allocation inside `force_capture` — is bounded by the
//! watchdog in the parent module, which ends the process on a deadline
//! whatever this code is doing.

use nix::sys::signal::{SigSet, Signal};
use std::backtrace::Backtrace;
use std::sync::Mutex;
use std::time::{Duration, Instant};

/// Captured stacks, keyed by thread id.
///
/// A `Vec` rather than a `HashMap` so it can be `const`-initialised and
/// needs no setup step: there are tens of threads at most, so the scan
/// below costs nothing.
static CAPTURED: Mutex<Vec<(i32, Backtrace)>> = Mutex::new(Vec::new());

/// Threads that must never be asked for a backtrace.
///
/// Only the signal machinery's own threads go in here, and they must: the
/// reporter is the thread running [`dump_all_thread_backtraces`], so
/// signalling it makes it capture a stack *from inside its own allocation*,
/// which deadlocks glibc's arena lock against itself; and a wedged watchdog
/// cannot end the process, which is the one guarantee that must hold. Their
/// stacks are known and useless anyway — one is in this function, the other
/// is asleep on a deadline.
static EXCLUDED: Mutex<Vec<i32>> = Mutex::new(Vec::new());

/// Take the calling thread out of the sweep, permanently.
///
/// Belt and braces: the thread is both recorded here, so it is never
/// signalled, and has `SIGUSR1` blocked, so it would not run the handler
/// even if something else sent one.
pub fn exclude_current_thread() {
    // SAFETY: `gettid` is a plain syscall with no arguments.
    let tid = unsafe { libc::syscall(libc::SYS_gettid) } as i32;
    if let Ok(mut excluded) = EXCLUDED.lock() {
        excluded.push(tid);
    }

    let mut mask = SigSet::empty();
    mask.add(Signal::SIGUSR1);
    // Best-effort: the exclusion above is what the sweep actually reads.
    #[allow(clippy::let_underscore_must_use)]
    let _ = mask.thread_block();
}

/// How long to wait for threads to answer before reporting what arrived.
///
/// A full sweep of two dozen threads comes back in a few milliseconds, so
/// this is a backstop rather than a budget.
const COLLECT_DEADLINE: Duration = Duration::from_millis(250);

/// How long to wait between checks while the answers come in.
///
/// Sleeping rather than spinning matters: the threads being waited on are
/// the ones that have to be scheduled to answer, and a `yield_now` loop
/// across two dozen of them starves exactly the work it is waiting for —
/// which showed up as sweeps that never completed and had to be cut short
/// by the watchdog.
const COLLECT_POLL: Duration = Duration::from_millis(1);

/// Install the `SIGUSR1` handler that makes a thread record its own stack.
pub fn install_capture_handler() {
    use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler};

    extern "C" fn capture(_: libc::c_int) {
        // Frames only; naming them happens on the reporting thread.
        let backtrace = Backtrace::force_capture();
        // SAFETY: `gettid` is async-signal-safe and takes no arguments.
        let tid = unsafe { libc::syscall(libc::SYS_gettid) } as i32;

        // `try_lock`, never `lock`: this runs on a thread that was stopped
        // wherever it happened to be, and a handler that waits for a lock
        // is a handler that may never return.
        if let Ok(mut captured) = CAPTURED.try_lock() {
            captured.push((tid, backtrace));
        }
    }

    let action = SigAction::new(
        SigHandler::Handler(capture),
        SaFlags::empty(),
        SigSet::empty(),
    );

    // SAFETY: installing a handler that captures and returns.
    unsafe {
        // Best-effort: without it the dump below reports no backtraces, but
        // the editor is otherwise unaffected.
        #[allow(clippy::let_underscore_must_use)]
        let _ = sigaction(Signal::SIGUSR1, &action);
    }
}

/// Ask every thread for its stack and log what comes back.
///
/// Runs on an ordinary thread, so everything here — allocating, formatting,
/// `tracing` — is unrestricted.
pub fn dump_all_thread_backtraces() {
    if let Ok(mut captured) = CAPTURED.try_lock() {
        captured.clear();
    }

    let thread_ids = threads_to_sweep();

    tracing::error!("=== Thread Backtrace Dump ===");
    tracing::error!("Total threads: {}", thread_ids.len());
    tracing::error!("Process ID: {}", std::process::id());

    for tid in &thread_ids {
        // SAFETY: `tgkill` on a thread of this very process.
        unsafe {
            libc::syscall(
                libc::SYS_tgkill,
                std::process::id() as i32,
                *tid,
                libc::SIGUSR1,
            );
        }
    }

    // Wait for the answers, but only until they are all in — a dump that
    // finishes in a millisecond should not sit out a fixed delay, and one
    // that never finishes should not sit here for ever.
    let deadline = Instant::now() + COLLECT_DEADLINE;
    loop {
        let collected = CAPTURED.try_lock().map(|c| c.len()).unwrap_or(0);
        if collected >= thread_ids.len() || Instant::now() >= deadline {
            break;
        }
        std::thread::sleep(COLLECT_POLL);
    }

    let Ok(captured) = CAPTURED.try_lock() else {
        tracing::error!("(backtrace storage is held by a stalled thread; none reported)");
        return;
    };

    for (i, tid) in thread_ids.iter().enumerate() {
        tracing::error!(
            "--- Thread {} (TID: {}, Name: {}) ---",
            i + 1,
            tid,
            thread_name(*tid)
        );
        match captured
            .iter()
            .find(|(captured_tid, _)| captured_tid == tid)
        {
            // Naming the frames happens here, not in the handler.
            Some((_, backtrace)) => tracing::error!("Backtrace:\n{}", backtrace),
            None => tracing::error!("(No backtrace captured for this thread)"),
        }
    }

    tracing::error!("=== End Thread Backtrace Dump ===");
}

/// The threads this sweep may signal: everything running, less the signal
/// machinery's own.
fn threads_to_sweep() -> Vec<i32> {
    let skip = EXCLUDED.lock().map(|e| e.clone()).unwrap_or_default();
    all_thread_ids()
        .into_iter()
        .filter(|tid| !skip.contains(tid))
        .collect()
}

/// Every thread id in this process, from `/proc/self/task`.
fn all_thread_ids() -> Vec<i32> {
    let mut thread_ids: Vec<i32> = std::fs::read_dir("/proc/self/task")
        .into_iter()
        .flatten()
        .flatten()
        .filter_map(|entry| entry.file_name().into_string().ok()?.parse().ok())
        .collect();
    thread_ids.sort_unstable();
    thread_ids
}

/// A thread's name, as `/proc` reports it.
fn thread_name(tid: i32) -> String {
    std::fs::read_to_string(format!("/proc/self/task/{tid}/comm"))
        .map(|name| name.trim().to_string())
        .unwrap_or_else(|_| String::from("unknown"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The enumeration has to see this thread and the process's others, or
    /// the dump silently reports nothing.
    #[test]
    fn every_running_thread_is_enumerated() {
        let started = std::sync::Arc::new(std::sync::Barrier::new(2));
        let finish = std::sync::Arc::new(std::sync::Barrier::new(2));
        let (s, f) = (started.clone(), finish.clone());

        let helper = std::thread::spawn(move || {
            s.wait();
            f.wait();
        });
        started.wait();

        let ids = all_thread_ids();
        // SAFETY: `gettid` is a plain syscall with no arguments.
        let me = unsafe { libc::syscall(libc::SYS_gettid) } as i32;
        assert!(ids.contains(&me), "the calling thread should be listed");
        assert!(
            ids.len() >= 2,
            "the helper thread should be listed too, got {ids:?}"
        );

        finish.wait();
        helper.join().unwrap();
    }

    /// The exclusion is what stops the sweep signalling itself. Without it
    /// the reporting thread asks itself for a backtrace *while it is
    /// already allocating*, deadlocks on the allocator's own lock, and the
    /// watchdog has to end the process — which is exactly what was observed
    /// before this filter existed.
    #[test]
    fn the_sweep_skips_the_threads_that_run_it() {
        let excluded = std::thread::spawn(|| {
            exclude_current_thread();
            // SAFETY: `gettid` is a plain syscall with no arguments.
            let tid = unsafe { libc::syscall(libc::SYS_gettid) } as i32;
            let swept = threads_to_sweep();
            assert!(
                all_thread_ids().contains(&tid),
                "the thread is running, so /proc lists it"
            );
            assert!(
                !swept.contains(&tid),
                "...but an excluded thread must not be swept"
            );
        });
        excluded.join().expect("the excluded thread should pass");
    }

    /// A thread that answers the request has its stack recorded against its
    /// own id — the property the dump reads back.
    #[test]
    fn a_signalled_thread_records_its_own_stack() {
        install_capture_handler();
        if let Ok(mut captured) = CAPTURED.try_lock() {
            captured.clear();
        }

        // SAFETY: `gettid` is a plain syscall with no arguments.
        let me = unsafe { libc::syscall(libc::SYS_gettid) } as i32;
        // SAFETY: signalling this very thread, whose handler is installed.
        unsafe { libc::raise(libc::SIGUSR1) };

        let captured = CAPTURED.try_lock().expect("storage should be free");
        assert!(
            captured.iter().any(|(tid, _)| *tid == me),
            "the signalled thread should have recorded a backtrace"
        );
    }
}
