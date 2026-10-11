//! Fresh's per-user data directory.
//!
//! Shared by the input-history store (`fresh-editor`) and the in-place-write
//! recovery temp files (`model::buffer::save`), which is why it sits down here
//! rather than next to either caller.

use std::cell::RefCell;
#[cfg(any(target_os = "macos", test))]
use std::ffi::OsStr;
#[cfg(any(target_os = "macos", test))]
use std::path::Path;
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
/// On macOS, an absolute `XDG_DATA_HOME` selects `$XDG_DATA_HOME/fresh`.
/// Otherwise this stays `dirs::data_dir()/fresh` (`~/Library/Application Support/fresh`).
/// A relative or empty value is ignored. `HOME` is not consulted for this override.
pub(crate) fn system_data_dir() -> Option<PathBuf> {
    #[cfg(target_os = "macos")]
    {
        macos_data_dir(
            std::env::var_os("XDG_DATA_HOME").as_deref(),
            dirs::data_dir().as_deref(),
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

/// macOS data directory: the XDG override, or `platform_data_dir/fresh`.
#[cfg(any(target_os = "macos", test))]
fn macos_data_dir(
    xdg_data_home: Option<&OsStr>,
    platform_data_dir: Option<&Path>,
) -> Option<PathBuf> {
    let platform = platform_data_dir.map(|p| p.join("fresh"));
    fresh_dir_from_xdg(xdg_data_home).or(platform)
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn macos_data_dir_uses_xdg_and_otherwise_the_platform_dir() {
        let xdg = std::env::temp_dir().join("data-home");
        let platform = std::env::temp_dir()
            .join("Library")
            .join("Application Support");
        let overridden = macos_data_dir(Some(xdg.as_os_str()), Some(&platform)).unwrap();
        assert_eq!(overridden, xdg.join("fresh"));
        let fallback = macos_data_dir(Some(OsStr::new("relative")), Some(&platform)).unwrap();
        assert_eq!(fallback, platform.join("fresh"));
    }

    #[test]
    fn macos_data_dir_without_xdg_uses_the_platform_dir() {
        let platform = std::env::temp_dir().join("platform-data");
        assert_eq!(
            macos_data_dir(None, Some(&platform)),
            Some(platform.join("fresh"))
        );
        assert!(macos_data_dir(None, None).is_none());
    }
}
