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

/// Initialize signal handlers for SIGTERM and SIGINT.
/// On Linux, dumps thread backtraces before terminating.
/// On other platforms, this is a no-op (default terminal behavior applies).
pub fn install_signal_handlers() {
    #[cfg(target_os = "linux")]
    linux::install_signal_handlers_with_backtrace();
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

    /// Install signal handlers for SIGINT and SIGTERM that dump backtraces before exiting
    fn install_termination_signal_handlers() {
        extern "C" fn termination_handler(_: libc::c_int) {
            // Only handle the first signal
            if SIGNAL_RECEIVED.swap(true, Ordering::SeqCst) {
                return;
            }

            tracing::error!("=== SIGNAL RECEIVED - Dumping debug info ===");

            // Dump JavaScript state first (if registered)
            tracing::error!("--- JavaScript State ---");
            super::dump_js_state();

            // Then dump Rust thread backtraces
            tracing::error!("--- Rust Thread Backtraces ---");
            dump_all_thread_backtraces();
            tracing::error!("=== Debug dump complete, terminating process ===");

            // Terminate the process
            std::process::exit(130); // Standard exit code for Ctrl+C
        }

        let handler = SigHandler::Handler(termination_handler);
        let action = SigAction::new(handler, SaFlags::empty(), SigSet::empty());

        unsafe {
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
