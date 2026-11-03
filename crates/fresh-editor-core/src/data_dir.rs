//! Fresh's per-user data directory.
//!
//! Shared by the input-history store (`fresh-editor`) and the in-place-write
//! recovery temp files (`model::buffer::save`), which is why it sits down here
//! rather than next to either caller.

use std::cell::RefCell;
#[cfg(any(target_os = "macos", test))]
use std::ffi::OsStr;
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
    system_data_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine data directory",
        )
    })
}

/// The system `fresh` data directory, ignoring any test override.
///
/// On macOS, an absolute `XDG_DATA_HOME` selects `$XDG_DATA_HOME/fresh`; see
/// [`xdg_dir_with_continuity`] for why an install that predates that may still
/// be on `~/Library/Application Support/fresh`. Otherwise this is
/// `dirs::data_dir()/fresh`. A relative or empty value is ignored.
pub(crate) fn system_data_dir() -> Option<PathBuf> {
    #[cfg(target_os = "macos")]
    {
        xdg_dir_with_continuity(
            fresh_dir_from_xdg(std::env::var_os("XDG_DATA_HOME").as_deref()),
            dirs::data_dir().map(|p| p.join("fresh")),
        )
    }

    #[cfg(not(target_os = "macos"))]
    {
        dirs::data_dir().map(|p| p.join("fresh"))
    }
}

/// `$XDG_*/fresh` when `value` is an absolute path. Empty and relative values
/// are ignored, per the XDG base directory spec.
#[cfg(any(target_os = "macos", test))]
pub(crate) fn fresh_dir_from_xdg(value: Option<&OsStr>) -> Option<PathBuf> {
    let value = value?;
    if value.is_empty() {
        return None;
    }
    let path = PathBuf::from(value);
    if path.is_absolute() {
        Some(path.join("fresh"))
    } else {
        None
    }
}

/// Resolve the `$XDG_*/fresh` directory against the location Fresh used before
/// it honoured the variable.
///
/// Honouring the variable is a change on Fresh's side, not the user's, so an
/// install that already keeps its files at `previous` stays there until the new
/// directory exists: upgrading must not silently strand its sessions, workspace
/// trust decisions and crash-recovery files. The new directory wins as soon as
/// it is there, which makes the whole migration "create it" — and a launcher
/// that pre-creates the directory is always on the new path from its first run.
///
/// `previous` is `$HOME`-derived, so a redirected `HOME` never resolves to the
/// real user's directory behind it.
#[cfg(any(target_os = "macos", test))]
pub(crate) fn xdg_dir_with_continuity(
    new: Option<PathBuf>,
    previous: Option<PathBuf>,
) -> Option<PathBuf> {
    match new {
        Some(new) if !new.exists() => previous.filter(|p| p.exists()).or(Some(new)),
        Some(new) => Some(new),
        None => previous,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn absolute_xdg_dir_selects_fresh_subdirectory() {
        let base = std::env::temp_dir().join("xdg-base");
        let resolved = fresh_dir_from_xdg(Some(base.as_os_str())).unwrap();
        assert_eq!(resolved, base.join("fresh"));
    }

    #[test]
    fn relative_or_empty_xdg_dir_is_ignored() {
        assert!(fresh_dir_from_xdg(Some(OsStr::new("relative"))).is_none());
        assert!(fresh_dir_from_xdg(Some(OsStr::new(""))).is_none());
        assert!(fresh_dir_from_xdg(None).is_none());
    }

    #[test]
    fn a_new_xdg_dir_wins_over_an_existing_install() {
        let base = TempDir::new().unwrap();
        let new = base.path().join("xdg").join("fresh");
        let previous = base.path().join("Library/Application Support/fresh");
        std::fs::create_dir_all(&previous).unwrap();
        std::fs::create_dir_all(&new).unwrap();
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), Some(previous)),
            Some(new)
        );
    }

    #[test]
    fn an_existing_install_outlives_the_xdg_variable_until_the_new_dir_exists() {
        let base = TempDir::new().unwrap();
        let new = base.path().join("xdg").join("fresh");
        let previous = base.path().join("Library/Application Support/fresh");
        std::fs::create_dir_all(&previous).unwrap();
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), Some(previous.clone())),
            Some(previous)
        );
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), None),
            Some(new),
            "with nothing to continue, the variable is what the user asked for"
        );
    }

    #[test]
    fn the_platform_dir_stands_when_xdg_is_absent() {
        let base = TempDir::new().unwrap();
        let previous = base.path().join("fresh");
        assert_eq!(
            xdg_dir_with_continuity(None, Some(previous.clone())),
            Some(previous)
        );
        assert_eq!(xdg_dir_with_continuity(None, None), None);
    }
}
