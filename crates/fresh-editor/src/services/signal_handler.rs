//! What the editor reports when it is asked to stop.
//!
//! `SIGINT` and `SIGTERM` dump the running JavaScript state and every
//! thread's backtrace before the process ends, which is how a hung editor
//! gets diagnosed after the fact.
//!
//! **None of that happens in the signal handler.** A handler runs on
//! whichever thread the kernel interrupted, wherever that thread happened to
//! be — quite possibly inside the allocator, or inside the logging
//! subscriber, holding a lock. POSIX therefore allows a handler only a short
//! list of calls (`signal-safety(7)`), and this dump needs almost nothing
//! from that list: it allocates, formats, locks, logs, and calls into plugin
//! code. Doing it inline deadlocked against the interrupted thread and
//! panicked out of an `extern "C"` frame, which aborts; under load that
//! showed up as a `SIGTERM` ending in `SIGABRT` or `SIGSEGV` instead of a
//! clean exit.
//!
//! So the handler hands the signal to a thread that is already waiting for
//! it (see [`relay`]) and returns. Everything above then runs in ordinary
//! Rust with no restrictions, and the interrupted thread carries on and
//! releases whatever it was holding.
//!
//! The terminal is handed back first, ahead of the dump: the process leaves
//! through `process::exit`, which runs no destructors, so nothing else
//! undoes raw mode and the alternate screen.
//!
//! Three things guarantee the process still dies, which is the part that
//! must not depend on the dump working:
//!
//!   * a **watchdog** thread, waiting on its own relay, ends the process on
//!     a deadline no matter what the reporter is doing;
//!   * a **second signal** restores the default disposition and re-raises,
//!     so a user who wants out immediately gets out immediately;
//!   * if the relays could not be created at all, no handler is installed,
//!     and the signals keep the default behaviour they had before.

use std::sync::Mutex;

#[cfg(unix)]
mod relay;
#[cfg(target_os = "linux")]
mod thread_dump;

/// Global storage for JavaScript execution state (thread-safe)
/// This is updated by the plugin thread and read by the reporting thread.
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

/// Get the current JavaScript execution state.
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

/// Dump JavaScript state.
///
/// Called from the reporting thread, never from a handler: it takes two
/// locks and calls into the plugin runtime.
pub fn dump_js_state() {
    let state = get_js_execution_state();
    if !state.is_empty() {
        tracing::error!("Current JS execution: {}", state);
    } else {
        tracing::error!("JS execution state: (idle or not tracked)");
    }

    if let Ok(guard) = JS_DUMP_CALLBACK.lock() {
        if let Some(ref callback) = *guard {
            callback();
        }
    }
}

/// Install the `SIGINT`/`SIGTERM` handlers and the machinery behind them.
///
/// Idempotent: the editor calls this once, and a couple of dozen tests call
/// it too, so repeated calls must not stack up threads or handlers.
pub fn install_signal_handlers() {
    #[cfg(unix)]
    unix::install();
}

#[cfg(unix)]
mod unix {
    use super::relay::{self, Relay};
    use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
    use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
    use std::sync::Once;
    use std::time::Duration;

    /// Where the handler finds the reporting thread. A bare `i32` in an
    /// atomic, so the handler's whole view is one relaxed load and a
    /// `write` — no `OnceLock`, no pointer to chase.
    static REPORT_FD: AtomicI32 = AtomicI32::new(-1);
    /// The same, for the watchdog.
    static WATCHDOG_FD: AtomicI32 = AtomicI32::new(-1);
    /// Whether a terminating signal has already been taken.
    static RECEIVED: AtomicBool = AtomicBool::new(false);
    static INSTALLED: Once = Once::new();

    /// How long the dump gets before the watchdog ends the process anyway.
    ///
    /// A complete dump of two dozen threads — capture, collect and
    /// symbolise — measures in the low hundreds of milliseconds, so this is
    /// several times the room it needs. It is deliberately not more
    /// generous than that: this doubles as the longest a `SIGTERM` can take
    /// when the dump wedges, and a container stop or a `pkill` should not
    /// have to wait. A user who will not wait even this long presses Ctrl+C
    /// again, which takes the escape path in the handler.
    const DUMP_DEADLINE: Duration = Duration::from_secs(2);

    /// Ctrl+C's conventional status, reported for both signals. Unchanged
    /// from before this file was rewritten; scripts may be reading it.
    const EXIT_CODE: i32 = 130;

    /// Set to anything to have a terminating signal dump every thread's
    /// backtrace. **Off by default**, which is a deliberate change.
    ///
    /// Reading another thread's stack means asking that thread to look at
    /// its own, from a signal handler — and capturing a backtrace
    /// allocates. A thread interrupted inside the allocator deadlocks
    /// against itself there, taking the allocator's lock (and, when it was
    /// mid-log, the logging subscriber's) with it, so the reporting thread
    /// then wedges too and the watchdog has to cut the process down. In an
    /// ordinary `… | fresh -` session under load that happened on roughly
    /// seven `SIGTERM`s in ten: every one still exited 130, but each took
    /// the watchdog's full deadline to do it.
    ///
    /// Nothing here can make that capture allocation-free — it would need a
    /// frame-walking primitive `std` does not expose and glibc's `execinfo`
    /// does not have on musl, which is a target this ships on. So the sweep
    /// is now what it always was in substance, a debugging aid, and it says
    /// so: a `pkill`, a container stop or a Ctrl+C gets a prompt exit and a
    /// short log, and a developer chasing a hang sets this and gets the
    /// full picture.
    ///
    /// Linux-only, like the sweep it gates: reading another thread's stack
    /// needs `/proc/self/task` to enumerate them and `tgkill` to ask.
    #[cfg(target_os = "linux")]
    const BACKTRACES_ENV: &str = "FRESH_SIGNAL_BACKTRACES";

    pub fn install() {
        INSTALLED.call_once(|| {
            let (report, watchdog) = match (Relay::new(), Relay::new()) {
                (Ok(report), Ok(watchdog)) => (report, watchdog),
                _ => {
                    // Without somewhere to hand the signal, a handler could
                    // only do the unsafe thing or nothing at all. Install
                    // none: the signals keep their default behaviour, which
                    // is what the editor had before this facility existed.
                    tracing::warn!(
                        "Could not create the signal relays; \
                         leaving SIGINT/SIGTERM at their default disposition"
                    );
                    return;
                }
            };

            REPORT_FD.store(report.notify_fd(), Ordering::SeqCst);
            WATCHDOG_FD.store(watchdog.notify_fd(), Ordering::SeqCst);

            spawn("signal-reporter", move || {
                exclude_from_the_sweep();
                if let Ok(signal) = report.wait() {
                    report_and_exit(signal);
                }
            });

            spawn("signal-watchdog", move || {
                exclude_from_the_sweep();
                if watchdog.wait().is_ok() {
                    std::thread::sleep(DUMP_DEADLINE);
                    // Still here: the dump has wedged, and the likeliest
                    // reason is that the allocator or the logger is stuck.
                    // `_exit` touches neither — it is a bare syscall, where
                    // `process::exit` would run atexit handlers and flush
                    // stdio, i.e. exactly the machinery under suspicion.
                    //
                    // SAFETY: `_exit` is async-signal-safe and cannot fail.
                    unsafe { libc::_exit(EXIT_CODE) };
                }
            });

            #[cfg(target_os = "linux")]
            super::thread_dump::install_capture_handler();

            install_termination_handler();
        });
    }

    /// Keep this thread out of the backtrace sweep.
    ///
    /// Neither of these two threads may be asked for its stack. The reporter
    /// *is* the thread running the sweep, so signalling it makes it capture
    /// a backtrace from inside its own allocation and deadlock against
    /// itself; and a watchdog stuck in a capture cannot end the process,
    /// which turns a slow exit into no exit at all. Both were observed
    /// before this call existed — a `SIGTERM` that took exactly the
    /// watchdog's deadline, and one that never landed.
    fn exclude_from_the_sweep() {
        #[cfg(target_os = "linux")]
        super::thread_dump::exclude_current_thread();
    }

    fn spawn(name: &str, body: impl FnOnce() + Send + 'static) {
        // Named so the dump can tell these two apart from the editor's own
        // threads when it lists them.
        if let Err(e) = std::thread::Builder::new()
            .name(name.to_string())
            .spawn(body)
        {
            tracing::error!("Failed to start the {name} thread: {e}");
        }
    }

    /// The handler. Every line of it is on POSIX's async-signal-safe list.
    extern "C" fn termination_handler(signal: libc::c_int) {
        if RECEIVED.swap(true, Ordering::SeqCst) {
            // A second signal means the first one is taking too long. Give
            // the user what they asked for: the default action, now.
            die_by_default(signal);
            return;
        }

        let reported = relay::notify(REPORT_FD.load(Ordering::SeqCst), signal);
        relay::notify(WATCHDOG_FD.load(Ordering::SeqCst), signal);

        if !reported {
            // Nobody is listening — the reporting thread failed to start,
            // or has already gone. Never swallow a terminating signal.
            die_by_default(signal);
        }
    }

    /// Restore `signal`'s default action and take it, so the process dies
    /// exactly as it would have with no handler installed.
    ///
    /// Callable from a handler: `signal(2)` and `raise(3)` are both on the
    /// async-signal-safe list.
    fn die_by_default(signal: libc::c_int) {
        // SAFETY: both calls are async-signal-safe; `raise` is delivered to
        // the default action the line above just restored.
        unsafe {
            libc::signal(signal, libc::SIG_DFL);
            libc::raise(signal);
        }
    }

    /// Everything the old handler did, on an ordinary thread where it is
    /// allowed to allocate, lock, log and call into plugin code.
    fn report_and_exit(signal: libc::c_int) -> ! {
        restore_terminal();

        tracing::error!("=== SIGNAL {signal} RECEIVED - Dumping debug info ===");

        tracing::error!("--- JavaScript State ---");
        super::dump_js_state();

        #[cfg(target_os = "linux")]
        if std::env::var(BACKTRACES_ENV).is_ok() {
            tracing::error!("--- Rust Thread Backtraces ---");
            super::thread_dump::dump_all_thread_backtraces();
        }
        #[cfg(not(target_os = "linux"))]
        {
            // Reading another thread's stack needs `/proc/self/task` plus
            // `tgkill`; elsewhere there is nothing to enumerate threads
            // with. Capturing in the handler instead is what this rewrite
            // exists to stop doing, and a backtrace of the reporting thread
            // would describe this function rather than the hang.
            tracing::error!("--- Rust thread backtraces: Linux only ---");
        }

        tracing::error!("=== Debug dump complete, terminating process ===");
        std::process::exit(EXIT_CODE);
    }

    /// Hand the terminal back, before the report rather than after it.
    ///
    /// The process leaves here through `process::exit` — no destructors, so
    /// the `TerminalModes` guard that normally undoes raw mode, the
    /// alternate screen, mouse capture and bracketed paste never runs — and
    /// the watchdog's `_exit` can cut in earlier still. Undoing the modes
    /// first means both exits give the shell back a usable terminal instead
    /// of one that echoes nothing and prints mouse reports at the prompt.
    ///
    /// Only when the terminal is ours to restore: a headless run (the
    /// daemon under systemd, a `--cmd` invocation in a pipeline) would
    /// otherwise write escape sequences into whatever is reading its stdout.
    fn restore_terminal() {
        if nix::unistd::isatty(std::io::stdout()).unwrap_or(false) {
            crate::services::terminal_modes::emergency_cleanup();
        }
    }

    fn install_termination_handler() {
        let action = SigAction::new(
            SigHandler::Handler(termination_handler),
            SaFlags::empty(),
            SigSet::empty(),
        );

        // SAFETY: the handler body is async-signal-safe by construction.
        unsafe {
            if let Err(e) = sigaction(Signal::SIGINT, &action) {
                tracing::error!("Failed to set SIGINT handler: {}", e);
            }
            if let Err(e) = sigaction(Signal::SIGTERM, &action) {
                tracing::error!("Failed to set SIGTERM handler: {}", e);
            }
        }
    }
}
