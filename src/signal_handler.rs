use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use std::collections::HashMap;
use std::fs;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

static SIGNAL_RECEIVED: AtomicBool = AtomicBool::new(false);
static BACKTRACE_STORAGE: Mutex<Option<HashMap<i32, String>>> = Mutex::new(None);

/// Initialize signal handlers for SIGTERM and SIGINT that dump thread backtraces
/// and terminate the process.
pub fn install_signal_handlers() {
    // Initialize backtrace storage
    *BACKTRACE_STORAGE.lock().unwrap() = Some(HashMap::new());

    // Install SIGUSR1 handler for individual thread backtrace capture
    install_backtrace_signal_handler();

    let handler = move || {
        // Only handle the first signal
        if SIGNAL_RECEIVED.swap(true, Ordering::SeqCst) {
            return;
        }

        tracing::error!("Signal received, dumping thread backtraces...");
        dump_all_thread_backtraces();
        tracing::error!("Backtrace dump complete, terminating process");

        // Terminate the process
        std::process::exit(130); // Standard exit code for Ctrl+C
    };

    // Register SIGINT and SIGTERM handler
    if let Err(e) = ctrlc::set_handler(handler) {
        tracing::error!("Failed to set signal handler: {}", e);
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

