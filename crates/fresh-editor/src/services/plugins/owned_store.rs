//! Editor-owned filesystem operations for plugins.
//!
//! Plugins used to hold `removePath`, `renamePath` and `copyPath`: general
//! primitives that took any path the caller could spell. `removePath` was
//! nominally fenced to the temp and config directories, but the fence only
//! checked the top-level target, so a symlink *inside* it walked the recursive
//! delete straight back out; `renamePath` had no fence at all and fell back to
//! copy-then-delete, which made the fence on `removePath` decorative anyway.
//!
//! Nothing here takes a path from a plugin. A scratch directory is named by an
//! opaque token this module issued, and the path it maps to is looked up here;
//! a package is named by kind and name, and the directory is computed here; a
//! state entry is named by namespace and key, and the file is computed here. A
//! plugin can therefore ask for the removal of a thing it was given, never of
//! a path it chose.
//!
//! Anything the user would miss — an installed package being replaced or
//! uninstalled — goes to the system trash, the same way the file explorer's
//! delete does, so an uninstall is recoverable. Scratch directories are this
//! module's own working space and are unlinked outright.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::{Duration, SystemTime};

/// How long an abandoned staging directory survives before the next
/// `scratch_create` sweeps it. A directory younger than this may still belong
/// to another live editor process, which is why the sweep is age-based rather
/// than "anything not in our table".
const STAGING_MAX_AGE: Duration = Duration::from_secs(24 * 60 * 60);

/// Where staging directories live, relative to the config dir.
///
/// Deliberately a sibling of the package directories rather than the OS temp
/// dir: an install finishes by renaming the staging directory onto the target,
/// and a rename only works within one filesystem. Staging under the config dir
/// makes that rename an atomic publish on every platform, instead of a
/// cross-device copy that has to be cleaned up when it fails half-way.
const STAGING_SUBDIR: &str = ".staging";

/// Editor-owned filesystem operations, keyed by name rather than by path.
pub struct OwnedStore {
    config_dir: PathBuf,
    /// Where namespaced state lives. Separate from the config dir because
    /// state is long-lived plugin bookkeeping, not user configuration.
    data_dir: PathBuf,
    /// Live scratch tokens and the directory each names. A token absent from
    /// here resolves to nothing, so a forged or stale token removes nothing.
    scratch: Mutex<HashMap<String, PathBuf>>,
    counter: AtomicU64,
}

/// The config subdirectory each package kind installs into.
///
/// An unknown kind resolves to nothing rather than to a guess, so a typo
/// cannot land a package somewhere unrelated.
fn packages_subdir(kind: &str) -> Option<&'static str> {
    match kind {
        "plugin" => Some("plugins"),
        "theme" => Some("themes"),
        "language" => Some("languages"),
        "bundle" => Some("bundles"),
        _ => None,
    }
}

/// Resolve `name` to a direct child of `parent`, or `None` if it is not one.
///
/// This asks `std::path` what the name is instead of pattern-matching it. An
/// earlier version of this function guessed — an allow-list of
/// `[A-Za-z0-9._-]`, then a list of characters to forbid — and guessing was
/// wrong in both directions at once: it rejected "Café Dark", which
/// `save_theme_file` will happily write and which a user would then be unable
/// to delete, while the list of things to forbid was never anything better
/// than the separators I happened to think of.
///
/// `Components` already knows the answer. It yields `Normal` only for an
/// ordinary name — `.`, `..`, a root and a Windows prefix each have their own
/// variant — and it splits on whatever separates paths on this platform, so
/// there is no list to keep. Comparing what it read back against what we were
/// handed catches the inputs it would otherwise normalise away (`foo/`,
/// `./foo`): those are paths, not names, whatever they resolve to.
///
/// The final `parent()` check is belt and braces — if the first two hold it
/// cannot fail — and costs nothing to keep.
fn child_of(parent: &Path, name: &str) -> Option<PathBuf> {
    let mut components = Path::new(name).components();
    let Some(Component::Normal(only)) = components.next() else {
        return None;
    };
    if components.next().is_some() {
        return None;
    }
    if only != OsStr::new(name) {
        return None;
    }
    let child = parent.join(only);
    (child.parent() == Some(parent)).then_some(child)
}

/// Whether `name` collides with the bookkeeping this module keeps beside the
/// packages it installs: the registry index, staging directories, and a copy
/// set aside mid-upgrade. All of them are dot-prefixed, and
/// `getInstalledPackages` skips dot-prefixed entries for exactly that reason.
///
/// This is a rule about *this* directory's contents, not about path safety —
/// `child_of` handles that — so it is stated separately rather than folded in
/// as one more character check.
fn is_reserved_package_name(name: &str) -> bool {
    name.starts_with('.')
}

impl OwnedStore {
    pub fn new(config_dir: PathBuf, data_dir: PathBuf) -> Self {
        Self {
            config_dir,
            data_dir,
            scratch: Mutex::new(HashMap::new()),
            counter: AtomicU64::new(0),
        }
    }

    fn staging_root(&self) -> PathBuf {
        self.config_dir.join(STAGING_SUBDIR)
    }

    /// Remove staging directories left behind by a process that died before it
    /// could finish. Best-effort: a failure here only costs disk space, and
    /// must never fail the install that triggered the sweep.
    fn sweep_stale_staging(&self) {
        let Ok(entries) = std::fs::read_dir(self.staging_root()) else {
            return;
        };
        let live: Vec<PathBuf> = self
            .scratch
            .lock()
            .map(|m| m.values().cloned().collect())
            .unwrap_or_default();
        for entry in entries.flatten() {
            let path = entry.path();
            if live.contains(&path) {
                continue;
            }
            let old = entry
                .metadata()
                .and_then(|m| m.modified())
                .map(|t| {
                    SystemTime::now()
                        .duration_since(t)
                        .map(|age| age > STAGING_MAX_AGE)
                        .unwrap_or(false)
                })
                .unwrap_or(false);
            if !old {
                continue;
            }
            // `std::fs::remove_dir_all` unlinks symlinks rather than
            // descending through them, so a symlink dropped into a staging
            // directory cannot walk this out of the config dir.
            if let Err(e) = std::fs::remove_dir_all(&path) {
                tracing::debug!("could not sweep stale staging dir {:?}: {e}", path);
            }
        }
    }

    /// Create a staging directory and return the opaque token that names it.
    ///
    /// `label` is advisory — it only makes the directory recognisable to a
    /// human looking at the config dir — and is sanitised before use.
    pub fn scratch_create(&self, label: &str) -> Option<String> {
        let root = self.staging_root();
        if let Err(e) = std::fs::create_dir_all(&root) {
            tracing::warn!("could not create staging root {:?}: {e}", root);
            return None;
        }
        self.sweep_stale_staging();

        let safe_label: String = label
            .chars()
            .filter(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_'))
            .take(32)
            .collect();
        let nanos = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let n = self.counter.fetch_add(1, Ordering::Relaxed);
        let token = format!("{}-{}-{}", std::process::id(), n, nanos);

        let dir = root.join(if safe_label.is_empty() {
            format!("s-{token}")
        } else {
            format!("{safe_label}-{token}")
        });

        // `create_dir` rather than `create_dir_all`: the name carries a pid, a
        // counter and a nanosecond clock, so an existing directory means
        // something is wrong and reusing it would be worse than failing.
        if let Err(e) = std::fs::create_dir(&dir) {
            tracing::warn!("could not create staging dir {:?}: {e}", dir);
            return None;
        }
        self.scratch.lock().ok()?.insert(token.clone(), dir);
        Some(token)
    }

    /// The directory a token names, for the plugin to write into.
    pub fn scratch_path(&self, token: &str) -> Option<PathBuf> {
        self.scratch.lock().ok()?.get(token).cloned()
    }

    /// Discard a staging directory. The path comes from this module's table,
    /// never from the caller, so an unknown token is a no-op rather than a
    /// delete of something else.
    pub fn scratch_discard(&self, token: &str) -> bool {
        let Some(dir) = self.scratch.lock().ok().and_then(|mut m| m.remove(token)) else {
            return false;
        };
        match std::fs::remove_dir_all(&dir) {
            Ok(()) => true,
            // Already gone is the outcome the caller wanted.
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => true,
            Err(e) => {
                tracing::warn!("could not discard staging dir {:?}: {e}", dir);
                false
            }
        }
    }

    /// The directory an installed package occupies, or `None` if the kind or
    /// name is not one this module will act on.
    pub fn package_dir(&self, kind: &str, name: &str) -> Option<PathBuf> {
        if is_reserved_package_name(name) {
            tracing::warn!("refusing package name reserved for bookkeeping: {name:?}");
            return None;
        }
        let sub = packages_subdir(kind)?;
        let packages = self.config_dir.join(sub).join("packages");
        let dir = child_of(&packages, name);
        if dir.is_none() {
            tracing::warn!("refusing package name that is not a single entry: {name:?}");
        }
        dir
    }

    /// Publish a staging directory as the installed package `<kind>/<name>`,
    /// replacing whatever is installed under that name.
    ///
    /// `subpath` installs one directory out of the staging tree — a package
    /// living in a subdirectory of a cloned monorepo — and is relative to the
    /// staging directory; empty means the staging directory itself. It only
    /// ever selects the *source*: where the package lands is decided by `kind`
    /// and `name`, so a caller cannot steer the replacement anywhere.
    ///
    /// An existing install goes to the system trash rather than being
    /// unlinked, so a bad upgrade is recoverable from the desktop's own undo.
    /// If the target cannot be moved aside the install fails and the existing
    /// package is left exactly as it was — an upgrade never leaves the user
    /// with nothing.
    ///
    /// Installing the whole staging directory spends the token, since the
    /// directory it named is the installed package now. Installing a subpath
    /// leaves the token live, so the caller can discard the rest of the tree.
    pub fn install_scratch(&self, token: &str, kind: &str, name: &str, subpath: &str) -> bool {
        let Some(staging) = self.scratch_path(token) else {
            tracing::warn!("install refused: unknown staging token");
            return false;
        };
        let source = if subpath.is_empty() {
            staging.clone()
        } else {
            // A subpath selects inside the staging tree and nowhere else.
            // Each segment has to be a single entry in the directory reached
            // so far, so there is no `..` to climb and no absolute path to
            // jump to.
            let mut p = staging.clone();
            for part in subpath.split('/').filter(|s| !s.is_empty()) {
                let Some(next) = child_of(&p, part) else {
                    tracing::warn!(
                        "install refused: subpath {subpath:?} is not a path within staging"
                    );
                    return false;
                };
                p = next;
            }
            if !p.starts_with(&staging) || !p.is_dir() {
                tracing::warn!(
                    "install refused: subpath {subpath:?} is not a directory in staging"
                );
                return false;
            }
            p
        };
        let Some(target) = self.package_dir(kind, name) else {
            return false;
        };
        let Some(parent) = target.parent() else {
            return false;
        };
        if let Err(e) = std::fs::create_dir_all(parent) {
            tracing::warn!("could not create packages dir {:?}: {e}", parent);
            return false;
        }

        // Move any existing install aside first, and only dispose of it once
        // the replacement is actually in place.
        //
        // Trashing it up front looked simpler, but it staked the user's
        // working package on a rename that can still fail — a locked file on
        // Windows, a target on another mount — and left them with nothing
        // installed when it did. The order here is the one the package
        // manager used before this moved into the editor: aside, swap,
        // then dispose, and put it back if the swap fails.
        let aside = if target.exists() {
            let nanos = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0);
            // Dot-prefixed so `getInstalledPackages` does not serve it as a
            // package of its own while it is here.
            let aside = parent.join(format!(".{name}.replaced-{nanos}"));
            if let Err(e) = std::fs::rename(&target, &aside) {
                tracing::warn!(
                    "could not move the installed copy of {name:?} aside ({:?}): {e}",
                    aside
                );
                return false;
            }
            Some(aside)
        } else {
            None
        };

        match std::fs::rename(&source, &target) {
            Ok(()) => {
                // The replacement is in place, so the copy it replaced can
                // go. To the trash, which is what makes a bad upgrade
                // recoverable; if there is no trash to put it in — no
                // writable HOME, a container — it stays where it is rather
                // than being unlinked. Visible, inert, and the user's to
                // remove.
                if let Some(aside) = aside {
                    if !trash_path(&aside) {
                        tracing::warn!(
                            "trash unavailable; the previous {name:?} was left at {:?}",
                            aside
                        );
                    }
                }
                // When the whole staging directory moved, the token no longer
                // names anything: the directory it stood for is the installed
                // package now, and must not be reachable through a later
                // discard. A subpath install leaves the rest of the tree, and
                // therefore the token, alone.
                if subpath.is_empty() {
                    if let Ok(mut m) = self.scratch.lock() {
                        m.remove(token);
                    }
                }
                true
            }
            Err(e) => {
                tracing::warn!("could not install staged package to {:?}: {e}", target);
                // Put the working copy back. The user asked for an upgrade
                // and did not get one; they must not also lose what they had.
                if let Some(aside) = aside {
                    if let Err(restore) = std::fs::rename(&aside, &target) {
                        tracing::error!(
                            "could not restore the previous {name:?} from {:?}: {restore}",
                            aside
                        );
                    }
                }
                false
            }
        }
    }

    /// Create a staging directory holding a copy of the tree at `from`, and
    /// return the token that names it.
    ///
    /// This is how a package installed from a local directory reaches
    /// staging. `from` is a path on the editor host: staging directories,
    /// installed packages and plugin state all live there by design, so an
    /// install survives the SSH session that started it going away. There is
    /// no authority-path form of this, which is why the argument is a plain
    /// path and not a `PluginPath` — the case that would have to be rejected
    /// cannot be written.
    ///
    /// Creating and filling in one call is likewise not a convenience. Doing
    /// it in two meant a copy that failed part-way left a half-filled staging
    /// directory alive under a token the caller then had to remember to
    /// discard. Here a failure discards its own directory and answers `None`:
    /// either there is a staging directory holding the whole tree, or there
    /// is nothing.
    ///
    /// Reading a caller-supplied path is safe in a way the old `copyPath` was
    /// not: the destination is a directory this module just made, so a copy
    /// cannot land on — and destroy — anything else. Symlinks are recreated
    /// as symlinks rather than followed, so a link in the source does not
    /// silently pull in whatever it points at.
    pub fn scratch_from_directory(&self, from: &Path) -> Option<String> {
        if !from.is_dir() {
            tracing::warn!("staging refused: {:?} is not a directory", from);
            return None;
        }
        let label = from
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        let token = self.scratch_create(&label)?;
        let dest = self.scratch_path(&token)?;
        match copy_tree(from, &dest) {
            Ok(()) => Some(token),
            Err(e) => {
                tracing::warn!("could not copy {:?} into staging: {e}", from);
                self.scratch_discard(&token);
                None
            }
        }
    }

    /// Move an installed package to the system trash. Returns false if there
    /// was nothing installed under that name.
    pub fn uninstall_package(&self, kind: &str, name: &str) -> bool {
        let Some(target) = self.package_dir(kind, name) else {
            return false;
        };
        if !target.exists() {
            tracing::warn!("uninstall: nothing installed at {:?}", target);
            return false;
        }
        trash_path(&target)
    }

    /// Move a user theme to the system trash.
    ///
    /// Theme deletion was already keyed by name rather than path, but it
    /// unlinked the file outright — a theme someone had spent an evening
    /// tuning was gone for good on a mis-click. It goes to the trash now, like
    /// every other removal a plugin can ask for.
    pub fn trash_theme(&self, name: &str) -> bool {
        let themes = self.config_dir.join("themes");
        let Some(path) = child_of(&themes, &format!("{name}.json")) else {
            tracing::warn!("refusing theme name that is not a single entry: {name:?}");
            return false;
        };
        if !path.exists() {
            return false;
        }
        trash_path(&path)
    }

    // ========================================================================
    // Namespaced state
    // ========================================================================

    /// The file a `(namespace, key)` pair occupies.
    fn state_file(&self, namespace: &str, key: &str) -> Option<PathBuf> {
        // `state_keys` skips dot-prefixed entries, because that is what the
        // temp files a write leaves behind on a crash look like. A key that
        // would land on one would therefore be writable and readable but
        // never enumerable — invisible to exactly the cleanup that walks the
        // namespace. Refuse it instead of storing something that cannot be
        // found again.
        if key.starts_with('.') || key.is_empty() {
            return None;
        }
        let dir = self.state_dir(namespace)?;
        child_of(&dir, &format!("{key}.json"))
    }

    /// The directory a namespace occupies.
    fn state_dir(&self, namespace: &str) -> Option<PathBuf> {
        child_of(&self.data_dir.join("state"), namespace)
    }

    /// Write a state entry, replacing any previous value.
    ///
    /// Writes to a sibling temp file and renames, so a crash mid-write leaves
    /// the previous value intact rather than a truncated one.
    pub fn state_set(&self, namespace: &str, key: &str, value: &str) -> bool {
        let Some(path) = self.state_file(namespace, key) else {
            return false;
        };
        let Some(parent) = path.parent() else {
            return false;
        };
        if let Err(e) = std::fs::create_dir_all(parent) {
            tracing::warn!("could not create state dir {:?}: {e}", parent);
            return false;
        }
        let tmp = parent.join(format!(".{key}.{}.tmp", std::process::id()));
        if let Err(e) = std::fs::write(&tmp, value.as_bytes()) {
            tracing::warn!("could not write state temp {:?}: {e}", tmp);
            return false;
        }
        match std::fs::rename(&tmp, &path) {
            Ok(()) => true,
            Err(e) => {
                tracing::warn!("could not publish state {:?}: {e}", path);
                // Leaving the temp behind would be litter `state_keys` skips
                // but a user would still find.
                if let Err(cleanup) = std::fs::remove_file(&tmp) {
                    tracing::debug!("could not remove state temp {:?}: {cleanup}", tmp);
                }
                false
            }
        }
    }

    /// Read a state entry, or `None` if it is unset.
    pub fn state_get(&self, namespace: &str, key: &str) -> Option<String> {
        let path = self.state_file(namespace, key)?;
        std::fs::read_to_string(path).ok()
    }

    /// The keys set in a namespace, in no particular order.
    pub fn state_keys(&self, namespace: &str) -> Vec<String> {
        let Some(dir) = self.state_dir(namespace) else {
            return Vec::new();
        };
        let Ok(entries) = std::fs::read_dir(dir) else {
            return Vec::new();
        };
        entries
            .flatten()
            .filter_map(|e| {
                let name = e.file_name().to_string_lossy().to_string();
                // Skip the in-flight temp files `state_set` leaves behind on a
                // crash; they are not keys and must never be read back as one.
                if name.starts_with('.') {
                    return None;
                }
                name.strip_suffix(".json").map(|k| k.to_string())
            })
            .collect()
    }

    /// Clear a state entry. Unlike a package, this is the plugin's own
    /// bookkeeping rather than something the user installed, so it is unlinked
    /// rather than trashed — and it is one named file, never a tree.
    pub fn state_delete(&self, namespace: &str, key: &str) -> bool {
        let Some(path) = self.state_file(namespace, key) else {
            return false;
        };
        match std::fs::remove_file(&path) {
            Ok(()) => true,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => true,
            Err(e) => {
                tracing::warn!("could not clear state {:?}: {e}", path);
                false
            }
        }
    }
}

/// Copy `src`'s contents into `dst`, which must already exist.
///
/// Symlinks are recreated rather than followed. Following them would mean a
/// link in the source deciding what gets read and duplicated, which is the
/// same class of mistake the old recursive delete made in the other
/// direction.
fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        let ty = entry.file_type()?;
        if ty.is_symlink() {
            copy_symlink(&from, &to)?;
        } else if ty.is_dir() {
            std::fs::create_dir_all(&to)?;
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// Reproduce the symlink at `from` under `to`, or failing that, copy what it
/// points at.
///
/// Recreating the link is the faithful thing to do, and it is what keeps a
/// link out of the staging tree from dragging in whatever it points at. But
/// creating one is not always allowed: Windows needs a privilege that a
/// normal account does not have unless Developer Mode is on. Failing the
/// whole install over that would mean a package with a symlink in it could
/// not be installed from a local directory on most Windows machines at all.
///
/// So on failure it falls back to copying the target's contents, which is
/// what the `copyPath` this replaced always did. That is a worse copy — the
/// link is gone and the bytes are duplicated — but it is a working install,
/// and it still cannot write outside the staging directory.
fn copy_symlink(from: &Path, to: &Path) -> std::io::Result<()> {
    let target = std::fs::read_link(from)?;

    #[cfg(unix)]
    let made = std::os::unix::fs::symlink(&target, to);
    #[cfg(windows)]
    let made = if from.is_dir() {
        // Windows needs to know which kind of link to make, and the answer is
        // whatever the target is right now.
        std::os::windows::fs::symlink_dir(&target, to)
    } else {
        std::os::windows::fs::symlink_file(&target, to)
    };

    let Err(e) = made else {
        return Ok(());
    };
    tracing::debug!(
        "could not recreate symlink {:?} ({e}); copying its target",
        from
    );

    // `from`, not `target`: a relative link resolves against its own
    // directory, and following it here is the whole point of the fallback.
    // Only a link to a *file* is followed. Following one to a directory would
    // mean walking a tree reached through a link, and a link pointing at one
    // of its own ancestors would recurse until the disk filled — the hazard
    // `copy_dir_all` still documents, and there is no reason to reintroduce
    // it here. A directory link in a package source is unusual enough that
    // saying so and carrying on beats either risk; so is a dangling one,
    // which did not resolve in the source either.
    if from.is_file() {
        std::fs::copy(from, to).map(|_| ())
    } else {
        tracing::warn!(
            "skipping {:?}: its link could not be recreated and it does not point at a file",
            from
        );
        Ok(())
    }
}

/// Move `path` to the system trash, reporting whether it worked.
///
/// There is deliberately no unlink fallback: if the trash is unavailable the
/// operation fails and the user keeps their files. The point of routing
/// through the trash is that no plugin-triggered removal is permanent, and a
/// fallback would quietly undo that on exactly the systems where it matters.
fn trash_path(path: &Path) -> bool {
    match trash::delete(path) {
        Ok(()) => true,
        Err(e) => {
            tracing::warn!("could not move {:?} to the trash: {e}", path);
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn store() -> (OwnedStore, tempfile::TempDir) {
        let dir = tempfile::tempdir().unwrap();
        let store = OwnedStore::new(dir.path().join("config"), dir.path().join("data"));
        (store, dir)
    }

    #[test]
    fn scratch_round_trips_through_its_token() {
        let (s, _guard) = store();
        let token = s.scratch_create("pkg").unwrap();
        let dir = s.scratch_path(&token).unwrap();
        assert!(dir.is_dir());
        std::fs::write(dir.join("f.txt"), b"x").unwrap();

        assert!(s.scratch_discard(&token));
        assert!(!dir.exists());
        // The token is spent: discarding again removes nothing.
        assert!(!s.scratch_discard(&token));
    }

    #[test]
    fn an_unknown_scratch_token_removes_nothing() {
        let (s, _guard) = store();
        assert!(!s.scratch_discard("not-a-token"));
        assert!(!s.scratch_discard("../../etc"));
    }

    #[test]
    fn scratch_discard_does_not_follow_symlinks_out() {
        let (s, guard) = store();
        let outside = guard.path().join("outside");
        std::fs::create_dir(&outside).unwrap();
        std::fs::write(outside.join("precious.txt"), b"keep").unwrap();

        let token = s.scratch_create("pkg").unwrap();
        let dir = s.scratch_path(&token).unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(&outside, dir.join("link")).unwrap();
        #[cfg(windows)]
        std::os::windows::fs::symlink_dir(&outside, dir.join("link")).unwrap();

        assert!(s.scratch_discard(&token));
        assert!(
            outside.join("precious.txt").exists(),
            "a symlink inside the staging dir must be unlinked, not followed"
        );
    }

    #[test]
    fn a_package_name_must_be_a_single_entry() {
        let (s, _guard) = store();
        assert!(s.package_dir("plugin", "file-diff").is_some());
        // Names people actually give things, which `save_theme_file` writes
        // verbatim. An allow-list of `[A-Za-z0-9._-]` made these saveable but
        // not deletable.
        assert!(s.package_dir("theme", "Café Dark").is_some());
        assert!(s.package_dir("theme", "solarized (v2)").is_some());

        // Not a single entry.
        assert!(s.package_dir("plugin", "../../../etc/passwd").is_none());
        assert!(s.package_dir("plugin", "a/b").is_none());
        assert!(s.package_dir("plugin", "/etc").is_none());
        assert!(s.package_dir("plugin", "..").is_none());
        assert!(s.package_dir("plugin", ".").is_none());
        assert!(s.package_dir("plugin", "").is_none());
        // A path that resolves to one entry is still a path, not a name.
        assert!(s.package_dir("plugin", "./file-diff").is_none());
        assert!(s.package_dir("plugin", "file-diff/").is_none());

        // Reserved for this module's own bookkeeping.
        assert!(s.package_dir("plugin", ".index").is_none());
        assert!(s.package_dir("plugin", ".staging").is_none());

        // An unknown kind resolves nowhere rather than to a default.
        assert!(s.package_dir("not-a-kind", "file-diff").is_none());
    }

    #[test]
    fn install_publishes_the_staging_dir_and_spends_the_token() {
        let (s, _guard) = store();
        let token = s.scratch_create("pkg").unwrap();
        let dir = s.scratch_path(&token).unwrap();
        std::fs::write(dir.join("package.json"), b"{}").unwrap();

        assert!(s.install_scratch(&token, "plugin", "file-diff", ""));
        let installed = s.package_dir("plugin", "file-diff").unwrap();
        assert!(installed.join("package.json").exists());
        // The staging dir *is* the install now; the token must not be able to
        // remove it.
        assert!(s.scratch_path(&token).is_none());
        assert!(!s.scratch_discard(&token));
        assert!(installed.join("package.json").exists());
    }

    #[test]
    fn install_refuses_an_unknown_token() {
        let (s, _guard) = store();
        assert!(!s.install_scratch("not-a-token", "plugin", "file-diff", ""));
    }

    #[test]
    fn state_round_trips_and_clears() {
        let (s, _guard) = store();
        assert!(s.state_set("machines", "laptop", r#"{"host":"h"}"#));
        assert_eq!(
            s.state_get("machines", "laptop").as_deref(),
            Some(r#"{"host":"h"}"#)
        );
        assert_eq!(s.state_keys("machines"), vec!["laptop".to_string()]);

        assert!(s.state_delete("machines", "laptop"));
        assert!(s.state_get("machines", "laptop").is_none());
        assert!(s.state_keys("machines").is_empty());
        // Clearing something already gone is the caller's desired end state.
        assert!(s.state_delete("machines", "laptop"));
    }

    #[test]
    fn a_state_key_that_could_not_be_enumerated_is_refused() {
        let (s, _guard) = store();
        // `state_keys` skips dot-prefixed entries (they are what a crashed
        // write leaves behind), so a key landing on one would be writable and
        // readable but never listed.
        assert!(!s.state_set("machines", ".hidden", "{}"));
        assert!(!s.state_set("machines", "", "{}"));
        assert!(s.state_get("machines", ".hidden").is_none());
        assert!(s.state_keys("machines").is_empty());

        // What a caller can write, it can also find again.
        assert!(s.state_set("machines", "laptop", "{}"));
        assert_eq!(s.state_keys("machines"), vec!["laptop".to_string()]);
    }

    #[test]
    fn state_keys_must_be_one_safe_component() {
        let (s, _guard) = store();
        assert!(!s.state_set("machines", "../escape", "{}"));
        assert!(!s.state_set("../escape", "k", "{}"));
        assert!(s.state_get("machines", "../escape").is_none());
        assert!(!s.state_delete("machines", "../escape"));
    }

    #[test]
    fn a_subpath_install_takes_only_that_directory_and_keeps_the_token() {
        let (s, _guard) = store();
        let token = s.scratch_create("clone").unwrap();
        let dir = s.scratch_path(&token).unwrap();
        // A clone with the package in a subdirectory, plus repo noise beside it.
        std::fs::create_dir_all(dir.join("packages/thing")).unwrap();
        std::fs::write(dir.join("packages/thing/package.json"), b"{}").unwrap();
        std::fs::write(dir.join("README.md"), b"repo").unwrap();

        assert!(s.install_scratch(&token, "plugin", "thing", "packages/thing"));
        let installed = s.package_dir("plugin", "thing").unwrap();
        assert!(installed.join("package.json").exists());
        assert!(
            !installed.join("README.md").exists(),
            "only the subpath installs"
        );

        // The rest of the clone is still the caller's to discard.
        assert!(s.scratch_path(&token).is_some());
        assert!(s.scratch_discard(&token));
        assert!(installed.join("package.json").exists());
    }

    #[test]
    fn a_subpath_cannot_reach_outside_the_staging_dir() {
        let (s, guard) = store();
        std::fs::create_dir_all(guard.path().join("elsewhere")).unwrap();
        let token = s.scratch_create("clone").unwrap();

        assert!(!s.install_scratch(&token, "plugin", "thing", "../elsewhere"));
        assert!(!s.install_scratch(&token, "plugin", "thing", "/etc"));
        assert!(!s.install_scratch(&token, "plugin", "thing", "a/../../elsewhere"));
    }

    #[test]
    fn staging_from_a_directory_recreates_symlinks_instead_of_following_them() {
        let (s, guard) = store();
        let outside = guard.path().join("outside");
        std::fs::create_dir(&outside).unwrap();
        std::fs::write(outside.join("secret.txt"), b"not yours").unwrap();

        let source = guard.path().join("source");
        std::fs::create_dir(&source).unwrap();
        std::fs::write(source.join("plugin.ts"), b"export {}").unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(&outside, source.join("link")).unwrap();

        let token = s.scratch_from_directory(&source).unwrap();
        let dir = s.scratch_path(&token).unwrap();

        assert_eq!(std::fs::read(dir.join("plugin.ts")).unwrap(), b"export {}");
        #[cfg(unix)]
        {
            let meta = std::fs::symlink_metadata(dir.join("link")).unwrap();
            assert!(
                meta.file_type().is_symlink(),
                "the link must be copied as a link, not as the tree it points at"
            );
        }
        assert!(outside.join("secret.txt").exists());
    }

    #[test]
    fn staging_from_a_directory_refuses_anything_that_is_not_one() {
        let (s, guard) = store();
        let file = guard.path().join("a-file");
        std::fs::write(&file, b"x").unwrap();

        assert!(s.scratch_from_directory(&file).is_none());
        assert!(s
            .scratch_from_directory(&guard.path().join("nope"))
            .is_none());
        // Nothing was staged, so nothing is left behind to discard.
        assert!(
            !s.staging_root().exists() || std::fs::read_dir(s.staging_root()).unwrap().count() == 0
        );
    }

    #[test]
    fn a_failed_swap_puts_the_working_copy_back() {
        let (s, _guard) = store();
        let first = s.scratch_create("pkg").unwrap();
        std::fs::write(s.scratch_path(&first).unwrap().join("v"), b"1").unwrap();
        assert!(s.install_scratch(&first, "plugin", "file-diff", ""));
        let installed = s.package_dir("plugin", "file-diff").unwrap();

        // A staging directory whose contents went away underneath us: the
        // rename onto the target fails, standing in for the locked file or
        // cross-mount target that can fail it in the wild.
        let second = s.scratch_create("pkg").unwrap();
        std::fs::remove_dir_all(s.scratch_path(&second).unwrap()).unwrap();

        assert!(!s.install_scratch(&second, "plugin", "file-diff", ""));
        assert_eq!(
            std::fs::read(installed.join("v")).unwrap(),
            b"1",
            "a failed upgrade must leave the working copy installed"
        );
    }

    #[test]
    fn a_replaced_install_leaves_the_old_one_recoverable() {
        let (s, _guard) = store();
        let first = s.scratch_create("pkg").unwrap();
        std::fs::write(s.scratch_path(&first).unwrap().join("v"), b"1").unwrap();
        assert!(s.install_scratch(&first, "plugin", "file-diff", ""));

        let second = s.scratch_create("pkg").unwrap();
        std::fs::write(s.scratch_path(&second).unwrap().join("v"), b"2").unwrap();
        assert!(s.install_scratch(&second, "plugin", "file-diff", ""));

        let installed = s.package_dir("plugin", "file-diff").unwrap();
        assert_eq!(std::fs::read(installed.join("v")).unwrap(), b"2");
    }
}
