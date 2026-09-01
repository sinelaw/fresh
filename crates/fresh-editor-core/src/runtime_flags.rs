//! Process-wide flags pushed out of `Config` at startup.
//!
//! These are read from contexts that don't carry a `Config` handle (e.g. the
//! parameter-less `services::terminal::detect_shell`). The storage lives here
//! because `Config::apply_runtime_flags` is the writer and it sits in this
//! crate, while the readers are up in `fresh-editor`.

use std::sync::atomic::{AtomicBool, Ordering};

static SKIP_APP_EXECUTION_ALIAS: AtomicBool = AtomicBool::new(true);

/// Set the Windows App-Execution-Alias workaround flag at editor startup.
///
/// `true` (the default) makes Windows shell selection skip Microsoft Store
/// App Execution Alias stubs. Set to `false` to disable the workaround —
/// useful for users who want to debug or who only ever have a real
/// `pwsh.exe` on PATH.
pub fn set_skip_app_execution_alias(skip: bool) {
    SKIP_APP_EXECUTION_ALIAS.store(skip, Ordering::Relaxed);
}

/// Read the flag set by [`set_skip_app_execution_alias`].
pub fn skip_app_execution_alias() -> bool {
    SKIP_APP_EXECUTION_ALIAS.load(Ordering::Relaxed)
}
