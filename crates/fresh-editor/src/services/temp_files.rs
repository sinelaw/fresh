//! Process-lifetime registry of scratch files this process created for its
//! own use, removed when the process exits.
//!
//! Today the only entry is the stdin spool file (`fresh-stdin-<pid>.tmp`):
//! `echo … | fresh -` drains the pipe into it and the buffer keeps reading
//! from it for as long as it lives, because a large stdin is loaded lazily
//! in chunks off that very file. So the file cannot be unlinked when the
//! buffer opens — it has to outlive the buffer, which means the process is
//! the only owner that can clean it up (#3134).
//!
//! Registration is deliberately by path rather than by handle for the same
//! reason: nothing here owns the file, it just knows what to delete.
//!
//! Three things drive the deletion, because a process leaves by more routes
//! than one:
//!
//!   * [`CleanupOnDrop`], parked in `real_main`, covers every normal exit
//!     and every error return — including the ones that never reach the
//!     editor loop, such as a failure in `initialize_app` *after* the spool
//!     file was created. A fatal panic is covered here too, by unwinding.
//!   * the signal handlers (`services::signal_handler`) call [`cleanup_all`]
//!     before the process goes. SIGTERM/SIGINT `exit()` without unwinding,
//!     and SIGHUP — closing the terminal window — had no handler at all, so
//!     the two most ordinary ways an editor dies both bypassed the guard.
//!   * the panic hook, but *only* under `panic = "abort"` (the `min-size`
//!     profile), where nothing unwinds. Deliberately not under unwinding:
//!     the hook fires for a panic on any thread, and a panicking thread is
//!     not a dying process — sweeping there would delete the file out from
//!     under a live buffer still reading chunks off it.
//!
//! The 24h sweep in `log_dirs::cleanup_stale_logs` stays as the backstop for
//! what none of them can catch — a `SIGKILL`, a segfault, a power loss.

use std::path::{Path, PathBuf};
use std::sync::Mutex;

/// Paths to remove on exit. Small and append-only in practice: one entry
/// per process, since stdin can only be spooled once.
static REGISTERED: Mutex<Vec<PathBuf>> = Mutex::new(Vec::new());

/// Register `path` for deletion when the process exits.
pub fn register(path: &Path) {
    // Recover from poisoning rather than dropping the path: a lost
    // registration is the one failure here with no second chance, since
    // nothing else knows the file exists.
    let mut paths = REGISTERED.lock().unwrap_or_else(|e| e.into_inner());
    if !paths.iter().any(|p| p == path) {
        paths.push(path.to_path_buf());
    }
}

/// Delete every registered file, then forget them.
///
/// Best-effort by design: this runs on the way out (and from the panic
/// hook), where there is nobody left to report a failure to, and a file
/// that is already gone is the expected case on a second call. Draining
/// the list keeps a second call — drop after the panic hook, say — a no-op.
pub fn cleanup_all() {
    let paths = match REGISTERED.lock() {
        Ok(mut guard) => std::mem::take(&mut *guard),
        // A panic while the list was held is exactly when we still want to
        // delete: take the paths out of the poisoned guard anyway.
        Err(poisoned) => std::mem::take(&mut *poisoned.into_inner()),
    };

    for path in paths {
        match std::fs::remove_file(&path) {
            Ok(()) => tracing::debug!("Removed temp file {:?}", path),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => tracing::debug!("Failed to remove temp file {:?}: {}", path, e),
        }
    }
}

/// Guard that calls [`cleanup_all`] when it goes out of scope.
///
/// Park one in the outermost frame that can reach [`register`], so the
/// files go away on every return path out of it — `Ok`, `Err`, or an
/// unwinding panic.
pub struct CleanupOnDrop;

impl Drop for CleanupOnDrop {
    fn drop(&mut self) {
        cleanup_all();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex as StdMutex;

    /// The registry is process-global, so these tests cannot run in
    /// parallel with each other.
    static SERIAL: StdMutex<()> = StdMutex::new(());

    fn temp_file(dir: &tempfile::TempDir, name: &str) -> PathBuf {
        let path = dir.path().join(name);
        std::fs::write(&path, b"spooled stdin").unwrap();
        path
    }

    #[test]
    fn cleanup_all_removes_registered_files() {
        let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
        cleanup_all(); // start from an empty registry

        let dir = tempfile::tempdir().unwrap();
        let a = temp_file(&dir, "fresh-stdin-1.tmp");
        let b = temp_file(&dir, "fresh-stdin-2.tmp");
        register(&a);
        register(&b);

        cleanup_all();

        assert!(!a.exists(), "registered file should be deleted");
        assert!(!b.exists(), "registered file should be deleted");
    }

    #[test]
    fn cleanup_all_leaves_unregistered_files_alone() {
        let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
        cleanup_all();

        let dir = tempfile::tempdir().unwrap();
        let registered = temp_file(&dir, "fresh-stdin-3.tmp");
        let untouched = temp_file(&dir, "some-other-file.txt");
        register(&registered);

        cleanup_all();

        assert!(!registered.exists());
        assert!(untouched.exists(), "only registered paths are deleted");
    }

    #[test]
    fn cleanup_all_is_idempotent_and_drains_the_registry() {
        let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
        cleanup_all();

        let dir = tempfile::tempdir().unwrap();
        let path = temp_file(&dir, "fresh-stdin-4.tmp");
        register(&path);

        cleanup_all();
        assert!(!path.exists());

        // Re-creating the file and sweeping again must not delete it: the
        // first sweep took the path off the list.
        std::fs::write(&path, b"a later, unrelated file").unwrap();
        cleanup_all();
        assert!(path.exists());
    }

    #[test]
    fn guard_cleans_up_when_dropped() {
        let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
        cleanup_all();

        let dir = tempfile::tempdir().unwrap();
        let path = temp_file(&dir, "fresh-stdin-5.tmp");

        {
            let _guard = CleanupOnDrop;
            register(&path);
            assert!(path.exists(), "file survives while the guard is alive");
        }

        assert!(!path.exists(), "dropping the guard deletes the file");
    }
}
