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
use std::sync::OnceLock;

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

/// A directory resolved once and then frozen for the life of the process.
///
/// Resolution reads the environment *and probes the filesystem* (see
/// [`xdg_dir_with_continuity`]), so running it twice can give two different
/// answers. Both of Fresh's user directories are resolved from two places
/// each: once into the [`crate::config_io::DirectoryContext`] that `main`
/// snapshots, and again — live — by [`get_data_dir`] (for the data dir) and
/// `Config::system_config_paths` (for the config dir). A directory that
/// changes under a running process puts the two out of step: with
/// `$XDG_DATA_HOME/fresh` appearing mid-session,
/// `workspace::get_workspaces_dir()` moved onto the new directory while
/// `dir_context.workspaces_dir()` stayed on the old one, so a session saved
/// through one path was looked for through the other and the restore silently
/// found nothing — the very stranding the continuity rule exists to prevent.
///
/// Freezing the answer is what keeps the two equal; it also drops the
/// `stat`/`read_dir` that every single lookup used to cost on macOS.
pub(crate) struct OnceDir(OnceLock<Option<PathBuf>>);

impl OnceDir {
    pub(crate) const fn new() -> Self {
        Self(OnceLock::new())
    }

    /// The resolved directory, running `resolve` only on the first call.
    pub(crate) fn get(&self, resolve: impl FnOnce() -> Option<PathBuf>) -> Option<PathBuf> {
        self.0.get_or_init(resolve).clone()
    }
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
/// Resolved once per process and frozen from then on — see [`OnceDir`] for why
/// the snapshot `main` takes and this function must never be able to disagree.
///
/// On macOS, an absolute `XDG_DATA_HOME` selects `$XDG_DATA_HOME/fresh`; see
/// [`xdg_dir_with_continuity`] for why an install that predates that may still
/// be on `~/Library/Application Support/fresh`. Otherwise this is
/// `dirs::data_dir()/fresh`. A relative or empty value is ignored.
pub(crate) fn system_data_dir() -> Option<PathBuf> {
    static RESOLVED: OnceDir = OnceDir::new();
    RESOLVED.get(resolve_system_data_dir)
}

/// The resolution [`system_data_dir`] freezes. Call it through that, not here:
/// two calls can answer differently.
fn resolve_system_data_dir() -> Option<PathBuf> {
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
/// install whose files are already at `previous` stays there until the new
/// directory is the one in use: upgrading must not silently strand its
/// sessions, workspace trust decisions and crash-recovery files.
///
/// "In use" means **holding files**, not merely existing, and that distinction
/// is the whole point. Keying on existence made `mkdir -p
/// $XDG_CONFIG_HOME/fresh` — the first half of the one migration step the docs
/// ask for — switch `config_dir` onto the still-empty new directory, and
/// everything hanging off it with no files to read: `config.json`, `themes/`,
/// `grammars/` and `plugins/` all read as absent while they sat untouched in
/// `~/.config/fresh`. Creating the directory now changes nothing; **moving the
/// files in** is what completes the migration, which is also the only order in
/// which no step loses anything.
///
/// A new install has nothing at `previous`, so an absolute `$XDG_*` is honoured
/// from the first run, pre-created directory or not.
///
/// `previous` is `$HOME`-derived, so a redirected `HOME` never resolves to the
/// real user's directory behind it.
#[cfg(any(target_os = "macos", test))]
pub(crate) fn xdg_dir_with_continuity(
    new: Option<PathBuf>,
    previous: Option<PathBuf>,
) -> Option<PathBuf> {
    match new {
        Some(new) if !holds_files(&new) => previous.filter(|p| holds_files(p)).or(Some(new)),
        Some(new) => Some(new),
        None => previous,
    }
}

/// Whether `dir` is a directory an install is actually keeping files in.
///
/// A directory that exists but cannot be listed (permissions) counts: we can
/// see it is there, and abandoning a directory we cannot look into is the one
/// answer that could lose data. A `dir` that is not there at all does not —
/// `read_dir` fails and `exists()` is false.
#[cfg(any(target_os = "macos", test))]
fn holds_files(dir: &Path) -> bool {
    match std::fs::read_dir(dir) {
        Ok(mut entries) => entries.next().is_some(),
        Err(_) => dir.exists(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn absolute_xdg_dir_selects_fresh_subdirectory() {
        let base = TempDir::new().unwrap();
        let resolved = fresh_dir_from_xdg(Some(base.path().as_os_str())).unwrap();
        assert_eq!(resolved, base.path().join("fresh"));
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
        std::fs::create_dir_all(previous.join("workspaces")).unwrap();
        std::fs::create_dir_all(new.join("workspaces")).unwrap();
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), Some(previous)),
            Some(new)
        );
    }

    #[test]
    fn an_existing_install_outlives_the_xdg_variable_until_its_files_move() {
        let base = TempDir::new().unwrap();
        let new = base.path().join("xdg").join("fresh");
        let previous = base.path().join("Library/Application Support/fresh");
        std::fs::create_dir_all(previous.join("workspaces")).unwrap();
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), Some(previous.clone())),
            Some(previous.clone())
        );

        // Creating the new directory is not the migration: an empty directory
        // holds none of the files the install is still reading.
        std::fs::create_dir_all(&new).unwrap();
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), Some(previous.clone())),
            Some(previous.clone()),
            "an empty new directory leaves the install where its files are"
        );

        std::fs::rename(previous.join("workspaces"), new.join("workspaces")).unwrap();
        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), Some(previous)),
            Some(new.clone()),
            "once the files are there, the new directory is the one in use"
        );

        assert_eq!(
            xdg_dir_with_continuity(Some(new.clone()), None),
            Some(new),
            "with nothing to continue, the variable is what the user asked for"
        );
    }

    /// The resolution used to run on *every* `get_data_dir()` call while
    /// `DirectoryContext::from_system()` snapshotted it once, so a
    /// `$XDG_DATA_HOME/fresh` that appeared mid-session — created by the user
    /// following the migration docs, by a pre-creating launcher, or by a second
    /// `fresh` — moved `workspace::get_workspaces_dir()` onto the new directory
    /// while `dir_context.workspaces_dir()` stayed on the old one. The session
    /// was then written through one path and looked for through the other.
    #[test]
    fn a_resolved_dir_is_frozen_for_the_process_even_as_the_answer_changes() {
        let base = TempDir::new().unwrap();
        let xdg = base.path().join("xdg");
        let previous = base.path().join("Library/Application Support/fresh");
        std::fs::create_dir_all(previous.join("workspaces")).unwrap();

        let resolve = || {
            xdg_dir_with_continuity(
                fresh_dir_from_xdg(Some(xdg.as_os_str())),
                Some(previous.clone()),
            )
        };

        let resolved = OnceDir::new();
        assert_eq!(resolved.get(resolve), Some(previous.clone()));

        std::fs::create_dir_all(xdg.join("fresh").join("workspaces")).unwrap();

        assert_eq!(
            resolve(),
            Some(xdg.join("fresh")),
            "re-resolving now answers differently — this is the divergence"
        );
        assert_eq!(
            resolved.get(resolve),
            Some(previous),
            "but a process keeps the directory it started on"
        );
    }

    /// The thread-local test override has to keep taking precedence over the
    /// frozen resolution: `tests/common/global_state.rs` and the recovery store
    /// both redirect the data dir through it.
    #[test]
    fn the_test_override_still_wins_over_the_frozen_resolution() {
        let base = TempDir::new().unwrap();
        let pinned = base.path().join("pinned");

        let system = get_data_dir().ok();
        let previous = set_data_dir_override(Some(pinned.clone()));
        assert_eq!(get_data_dir().ok(), Some(pinned));
        set_data_dir_override(previous);
        assert_eq!(get_data_dir().ok(), system);
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
