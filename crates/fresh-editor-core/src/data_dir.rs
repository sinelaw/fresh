//! Fresh's per-user data directory.
//!
//! Shared by the input-history store (`fresh-editor`) and the in-place-write
//! recovery temp files (`model::buffer::save`), which is why it sits down here
//! rather than next to either caller.

use std::cell::RefCell;
use std::path::PathBuf;

thread_local! {
    /// Per-thread redirection of [`get_data_dir`], installed by
    /// [`set_data_dir_override`]. Never set outside tests.
    static DATA_DIR_OVERRIDE: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
}

/// Redirect [`get_data_dir`] **on the calling thread** to `dir`, returning
/// whatever it was pointing at before.
///
/// The data directory is otherwise derived from `$XDG_DATA_HOME`, which is
/// process-global. Integration tests that need their own persistence tree used
/// to point it there with `std::env::set_var`, which was safe only while each
/// of them had a test binary to itself. They no longer do —
/// `crates/fresh-editor/tests` links into a single `all_tests` binary whose
/// tests run as parallel threads of one process — so one test's `set_var` moved
/// every *other* test's workspace store out from under it mid-save.
///
/// A thread-local carries the same intent ("this session's persistence lives
/// here") without the collision: the editor is driven from the test's own
/// thread, and every path that resolves a workspace or file-state file
/// (`workspace::get_workspaces_dir`, `PersistedFileWorkspace::states_dir`,
/// the recovery store) runs synchronously under `&mut Editor` on that thread.
///
/// The scope is exactly one thread, deliberately: work handed to a background
/// thread (a PTY reader, a plugin host, a Tokio worker) does *not* inherit the
/// override and falls back to `$XDG_DATA_HOME` as it always has. Nothing in the
/// editor writes the workspace store from such a thread; if that ever changes,
/// the override has to move with it.
///
/// Test-only hook: production never calls this, and with no override installed
/// this costs one thread-local read per lookup. It is `#[doc(hidden)] pub`
/// rather than `#[cfg(test)]` because `#[cfg(test)]` in `fresh-editor-core` is
/// invisible to the integration tests in `fresh-editor` (see CONTRIBUTING,
/// "The data layer is its own crate").
#[doc(hidden)]
pub fn set_data_dir_override(dir: Option<PathBuf>) -> Option<PathBuf> {
    DATA_DIR_OVERRIDE.with(|slot| std::mem::replace(&mut *slot.borrow_mut(), dir))
}

/// The `fresh` data directory (`$XDG_DATA_HOME/fresh`, or the platform equivalent).
pub fn get_data_dir() -> std::io::Result<PathBuf> {
    if let Some(dir) = DATA_DIR_OVERRIDE.with(|slot| slot.borrow().clone()) {
        return Ok(dir);
    }
    let data_dir = dirs::data_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine data directory",
        )
    })?;
    Ok(data_dir.join("fresh"))
}
