use std::sync::Mutex;

/// Global storage for JavaScript execution state (thread-safe)
/// This is updated by the plugin thread and read by signal handlers
static JS_EXECUTION_STATE: Mutex<String> = Mutex::new(String::new());

/// Update the current JavaScript execution state.
/// Called by the plugin thread when execution state changes.
pub fn set_js_execution_state(state: String) {
    if let Ok(mut guard) = JS_EXECUTION_STATE.lock() {
        *guard = state;
    }
}

/// Clear the JavaScript execution state.
pub fn clear_js_execution_state() {
    if let Ok(mut guard) = JS_EXECUTION_STATE.lock() {
        guard.clear();
    }
}

/// Get the current JavaScript execution state (for signal handler).
pub fn get_js_execution_state() -> String {
    JS_EXECUTION_STATE
        .lock()
        .map(|g| g.clone())
        .unwrap_or_else(|_| "(mutex poisoned)".to_string())
}

/// Global callback for dumping JavaScript state on signal
static JS_DUMP_CALLBACK: Mutex<Option<Box<dyn Fn() + Send + Sync>>> = Mutex::new(None);

/// Register a callback to dump JavaScript state when a signal is received.
/// This is called by the plugin manager to register its dump function.
pub fn register_js_dump_callback<F>(callback: F)
where
    F: Fn() + Send + Sync + 'static,
{
    *JS_DUMP_CALLBACK.lock().unwrap() = Some(Box::new(callback));
}

/// Dump JavaScript state (called from signal handler)
pub fn dump_js_state() {
    // First dump the execution state string (thread-safe)
    let state = get_js_execution_state();
    if !state.is_empty() {
        tracing::error!("Current JS execution: {}", state);
    } else {
        tracing::error!("JS execution state: (idle or not tracked)");
    }

    // Then call the custom callback if registered
    if let Ok(guard) = JS_DUMP_CALLBACK.lock() {
        if let Some(ref callback) = *guard {
            callback();
        }
    }
}

/// Initialize signal handlers for SIGHUP, SIGINT, and SIGTERM.
/// On Linux, dumps thread backtraces before terminating.
/// On other Unix platforms (macOS), dumps JS state and current thread backtrace.
pub fn install_signal_handlers() {
    #[cfg(target_os = "linux")]
    linux::install_signal_handlers_with_backtrace();

    #[cfg(all(unix, not(target_os = "linux")))]
    unix_fallback::install_signal_handlers_basic();
}

/// Linux-specific implementation with thread backtrace dumping
#[cfg(target_os = "linux")]
mod linux {
    use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
    use std::collections::HashMap;
    use std::fs;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Mutex;

    static SIGNAL_RECEIVED: AtomicBool = AtomicBool::new(false);
    static BACKTRACE_STORAGE: Mutex<Option<HashMap<i32, String>>> = Mutex::new(None);

    pub fn install_signal_handlers_with_backtrace() {
        // Initialize backtrace storage
        *BACKTRACE_STORAGE.lock().unwrap() = Some(HashMap::new());

        // Install SIGUSR1 handler for individual thread backtrace capture
        install_backtrace_signal_handler();

        // Install SIGINT and SIGTERM handlers
        install_termination_signal_handlers();
    }

    /// Install signal handlers for SIGHUP / SIGINT / SIGTERM that dump
    /// backtraces before exiting.
    ///
    /// SIGHUP is included so the editor terminates promptly when its
    /// controlling terminal goes away (window/tab close, ssh disconnect),
    /// instead of relying on the kernel's default disposition — which can
    /// be subverted by a thread blocking the signal or by a third-party
    /// dependency installing SIG_IGN.
    fn install_termination_signal_handlers() {
        extern "C" fn termination_handler(signum: libc::c_int) {
            // Only handle the first signal
            if SIGNAL_RECEIVED.swap(true, Ordering::SeqCst) {
                return;
            }

            tracing::error!("=== SIGNAL {} RECEIVED - Dumping debug info ===", signum);

            // Dump JavaScript state first (if registered)
            tracing::error!("--- JavaScript State ---");
            super::dump_js_state();

            // Then dump Rust thread backtraces
            tracing::error!("--- Rust Thread Backtraces ---");
            dump_all_thread_backtraces();
            tracing::error!("=== Debug dump complete, terminating process ===");

            // Terminate with the conventional 128 + signum exit code so callers
            // can distinguish (130 = SIGINT, 143 = SIGTERM, 129 = SIGHUP).
            std::process::exit(128 + signum);
        }

        let handler = SigHandler::Handler(termination_handler);
        let action = SigAction::new(handler, SaFlags::empty(), SigSet::empty());

        unsafe {
            if let Err(e) = sigaction(Signal::SIGHUP, &action) {
                tracing::error!("Failed to set SIGHUP handler: {}", e);
            }
            if let Err(e) = sigaction(Signal::SIGINT, &action) {
                tracing::error!("Failed to set SIGINT handler: {}", e);
            }
            if let Err(e) = sigaction(Signal::SIGTERM, &action) {
                tracing::error!("Failed to set SIGTERM handler: {}", e);
            }
        }
    }

    /// Install SIGUSR1 handler that captures backtrace for the receiving thread
    fn install_backtrace_signal_handler() {
        extern "C" fn backtrace_signal_handler(_: libc::c_int) {
            // Capture backtrace for this thread
            let backtrace = std::backtrace::Backtrace::force_capture();
            let tid = unsafe { libc::syscall(libc::SYS_gettid) } as i32;

            // Store the backtrace
            if let Ok(mut storage) = BACKTRACE_STORAGE.lock() {
                if let Some(ref mut map) = *storage {
                    map.insert(tid, format!("{}", backtrace));
                }
            }
        }

        let handler = SigHandler::Handler(backtrace_signal_handler);
        let action = SigAction::new(handler, SaFlags::empty(), SigSet::empty());

        unsafe {
            // Best-effort signal handler install -- if it fails, backtrace capture won't work
            // but the editor can still function.
            #[allow(clippy::let_underscore_must_use)]
            let _ = sigaction(Signal::SIGUSR1, &action);
        }
    }

    /// Dump backtraces for all threads to the tracing log
    fn dump_all_thread_backtraces() {
        // Clear any old backtraces
        if let Ok(mut storage) = BACKTRACE_STORAGE.lock() {
            if let Some(ref mut map) = *storage {
                map.clear();
            }
        }

        // Get all thread IDs from /proc/self/task
        let thread_ids = get_all_thread_ids();

        tracing::error!("=== Thread Backtrace Dump ===");
        tracing::error!("Total threads: {}", thread_ids.len());
        tracing::error!("Process ID: {}", std::process::id());

        // Send SIGUSR1 to each thread to trigger backtrace capture
        for tid in &thread_ids {
            unsafe {
                // Send SIGUSR1 to the thread using tgkill
                libc::syscall(
                    libc::SYS_tgkill,
                    std::process::id() as i32,
                    *tid,
                    libc::SIGUSR1,
                );
            }
        }

        // Give threads time to capture their backtraces
        std::thread::sleep(std::time::Duration::from_millis(100));

        // Now print all captured backtraces
        let backtraces = BACKTRACE_STORAGE.lock().unwrap();
        if let Some(ref map) = *backtraces {
            for (i, tid) in thread_ids.iter().enumerate() {
                // Read thread name from /proc
                let thread_name = read_thread_name(*tid);
                tracing::error!(
                    "--- Thread {} (TID: {}, Name: {}) ---",
                    i + 1,
                    tid,
                    thread_name
                );

                if let Some(backtrace) = map.get(tid) {
                    tracing::error!("Backtrace:\n{}", backtrace);
                } else {
                    tracing::error!("(No backtrace captured for this thread)");
                }
            }
        }

        tracing::error!("=== End Thread Backtrace Dump ===");
    }

    /// Get all thread IDs (TIDs) in the process from /proc/self/task
    fn get_all_thread_ids() -> Vec<i32> {
        let mut thread_ids = Vec::new();

        if let Ok(entries) = fs::read_dir("/proc/self/task") {
            for entry in entries.flatten() {
                if let Ok(file_name) = entry.file_name().into_string() {
                    if let Ok(tid) = file_name.parse::<i32>() {
                        thread_ids.push(tid);
                    }
                }
            }
        }

        thread_ids.sort();
        thread_ids
    }

    /// Read the thread name from /proc/self/task/<tid>/comm
    fn read_thread_name(tid: i32) -> String {
        let path = format!("/proc/self/task/{}/comm", tid);
        fs::read_to_string(&path)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| String::from("unknown"))
    }
}

/// Fallback for non-Linux Unix platforms (macOS, BSDs).
/// Installs SIGINT/SIGTERM handlers that dump JS state and current thread backtrace.
#[cfg(all(unix, not(target_os = "linux")))]
mod unix_fallback {
    use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
    use std::sync::atomic::{AtomicBool, Ordering};

    static SIGNAL_RECEIVED: AtomicBool = AtomicBool::new(false);

    pub fn install_signal_handlers_basic() {
        install_termination_signal_handlers();
    }

    fn install_termination_signal_handlers() {
        extern "C" fn termination_handler(signum: libc::c_int) {
            if SIGNAL_RECEIVED.swap(true, Ordering::SeqCst) {
                return;
            }

            tracing::error!("=== SIGNAL {} RECEIVED - Dumping debug info ===", signum);

            tracing::error!("--- JavaScript State ---");
            super::dump_js_state();

            tracing::error!("--- Current Thread Backtrace ---");
            let bt = std::backtrace::Backtrace::force_capture();
            tracing::error!("Backtrace:\n{}", bt);

            tracing::error!("=== Debug dump complete, terminating process ===");
            std::process::exit(128 + signum);
        }

        let handler = SigHandler::Handler(termination_handler);
        let action = SigAction::new(handler, SaFlags::empty(), SigSet::empty());

        unsafe {
            if let Err(e) = sigaction(Signal::SIGHUP, &action) {
                tracing::error!("Failed to set SIGHUP handler: {}", e);
            }
            if let Err(e) = sigaction(Signal::SIGINT, &action) {
                tracing::error!("Failed to set SIGINT handler: {}", e);
            }
            if let Err(e) = sigaction(Signal::SIGTERM, &action) {
                tracing::error!("Failed to set SIGTERM handler: {}", e);
            }
        }
    }
}

/// Test that the editor terminates promptly on SIGHUP — the signal sent
/// by the kernel when the controlling terminal goes away (window/tab
/// close, ssh disconnect). Regression test for issue #1809.
///
/// Runs in a forked child so the test process's own signal disposition
/// is unaffected. The child installs the production signal handlers and
/// pauses; the parent waits for the child to signal readiness via a
/// pipe, then sends SIGHUP and asserts the child exits with the
/// conventional 128 + signum status (129 for SIGHUP).
#[cfg(all(test, unix))]
mod tests {
    use std::time::{Duration, Instant};

    #[test]
    fn sighup_terminates_process() {
        // Pipe used to synchronize: child writes after handlers are installed,
        // parent reads to know it's safe to send SIGHUP.
        let mut pipe_fds = [0i32; 2];
        let r = unsafe { libc::pipe(pipe_fds.as_mut_ptr()) };
        assert_eq!(r, 0, "pipe failed: {}", std::io::Error::last_os_error());
        let (read_fd, write_fd) = (pipe_fds[0], pipe_fds[1]);

        // SAFETY: fork() is called from a #[test] which Cargo runs serially
        // per-process; the child immediately installs handlers, signals
        // readiness, and pauses. It does not touch shared mutable state of
        // the parent runtime.
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0, "fork failed: {}", std::io::Error::last_os_error());

        if pid == 0 {
            // Child: install handlers, signal readiness, then sleep.
            unsafe {
                libc::close(read_fd);
            }
            super::install_signal_handlers();
            // Notify parent that handlers are installed.
            let byte: u8 = 1;
            let _ = unsafe { libc::write(write_fd, &byte as *const u8 as *const _, 1) };
            unsafe {
                libc::close(write_fd);
            }
            // pause() blocks until any signal is delivered. With our handler
            // installed, SIGHUP will run the handler which calls exit(129).
            // If the handler isn't installed for SIGHUP, default disposition
            // (term) still terminates the child — but with WIFSIGNALED, not
            // WIFEXITED, so the assertion below distinguishes them.
            unsafe {
                libc::pause();
            }
            // Should not be reached if the handler exits on signal.
            unsafe {
                libc::_exit(0);
            }
        }

        // Parent: close write end and wait for child's readiness byte.
        unsafe {
            libc::close(write_fd);
        }
        let mut buf: [u8; 1] = [0];
        let n = unsafe { libc::read(read_fd, buf.as_mut_ptr() as *mut _, 1) };
        unsafe {
            libc::close(read_fd);
        }
        assert_eq!(
            n, 1,
            "did not receive readiness byte from child: read returned {}",
            n
        );

        unsafe {
            libc::kill(pid, libc::SIGHUP);
        }

        let deadline = Instant::now() + Duration::from_secs(10);
        let mut status: libc::c_int = 0;
        loop {
            let r = unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) };
            if r == pid {
                break;
            }
            assert!(
                Instant::now() < deadline,
                "child did not exit within 10s after SIGHUP"
            );
            std::thread::sleep(Duration::from_millis(20));
        }

        assert!(
            libc::WIFEXITED(status),
            "child terminated abnormally (status raw = {}): handler did not run on SIGHUP",
            status
        );
        assert_eq!(
            libc::WEXITSTATUS(status),
            128 + libc::SIGHUP,
            "child exit code: expected 128+SIGHUP={}, got {}",
            128 + libc::SIGHUP,
            libc::WEXITSTATUS(status)
        );
    }
}
