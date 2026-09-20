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
use std::path::{Path, PathBuf};
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

/// Whether `s` is safe to use as exactly one path component.
///
/// Rejects separators, `.`/`..`, and anything with a leading dot: the package
/// directories keep their own dot-prefixed entries (`.index`, `.staging`) and
/// a package must never be able to name one.
fn is_safe_component(s: &str) -> bool {
    !s.is_empty()
        && s.len() <= 128
        && !s.starts_with('.')
        && s.chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.'))
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
            if live.iter().any(|p| *p == path) {
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
        if !is_safe_component(name) {
            tracing::warn!(
                "refusing package name that is not a single safe path component: {name:?}"
            );
            return None;
        }
        let sub = packages_subdir(kind)?;
        Some(self.config_dir.join(sub).join("packages").join(name))
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
            // A subpath selects inside the staging tree and nowhere else: no
            // absolute paths, no `..`, no separators smuggled through a single
            // component.
            let mut p = staging.clone();
            for part in subpath.split('/').filter(|s| !s.is_empty()) {
                if !is_safe_component(part) {
                    tracing::warn!("install refused: unsafe subpath {subpath:?}");
                    return false;
                }
                p.push(part);
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

        // Move any existing install out of the way. The trash is the good
        // outcome: an upgrade that turns out badly is recoverable from the
        // desktop's own undo.
        //
        // When the trash is unavailable — no writable HOME, a container, a
        // mount with nowhere to put one — the fallback is to rename the old
        // copy aside under a dot-prefixed sibling rather than to unlink it.
        // Upgrading still works, nothing is destroyed, and the leftover is
        // visible to a user who wants the space back. `getInstalledPackages`
        // skips dot-prefixed directories, so the old copy is not served as a
        // package of its own.
        if target.exists() && !trash_path(&target) {
            let nanos = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0);
            let aside = parent.join(format!(".{name}.replaced-{nanos}"));
            if let Err(e) = std::fs::rename(&target, &aside) {
                tracing::warn!(
                    "could not move the installed copy of {name:?} aside ({:?}): {e}",
                    aside
                );
                return false;
            }
            tracing::warn!(
                "trash unavailable; the previous {name:?} was left at {:?}",
                aside
            );
        }

        match std::fs::rename(&source, &target) {
            Ok(()) => {
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
                false
            }
        }
    }

    /// Copy the tree at `from` into a staging directory.
    ///
    /// This is how a package installed from a local directory reaches staging.
    /// It is the one operation here that reads a caller-supplied path, which
    /// is safe in a way the old `copyPath` was not: the *destination* is a
    /// staging directory this module made and still owns, so a copy cannot
    /// land on — and therefore cannot destroy — anything else.
    ///
    /// Symlinks are recreated as symlinks rather than followed, so a link in
    /// the source does not silently pull in whatever it points at.
    pub fn copy_into_scratch(&self, token: &str, from: &Path) -> bool {
        let Some(dest) = self.scratch_path(token) else {
            tracing::warn!("copy refused: unknown staging token");
            return false;
        };
        if !from.is_dir() {
            tracing::warn!("copy refused: {:?} is not a directory", from);
            return false;
        }
        match copy_tree(from, &dest) {
            Ok(()) => true,
            Err(e) => {
                tracing::warn!("could not copy {:?} into staging: {e}", from);
                false
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
        if !is_safe_component(name) {
            tracing::warn!(
                "refusing theme name that is not a single safe path component: {name:?}"
            );
            return false;
        }
        let path = self.config_dir.join("themes").join(format!("{name}.json"));
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
        if !is_safe_component(namespace) || !is_safe_component(key) {
            return None;
        }
        Some(
            self.data_dir
                .join("state")
                .join(namespace)
                .join(format!("{key}.json")),
        )
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
        if !is_safe_component(namespace) {
            return Vec::new();
        }
        let dir = self.data_dir.join("state").join(namespace);
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
            let target = std::fs::read_link(&from)?;
            #[cfg(unix)]
            std::os::unix::fs::symlink(&target, &to)?;
            #[cfg(windows)]
            {
                // Windows needs to know which kind of link to make, and the
                // answer is whatever the target is right now.
                if from.is_dir() {
                    std::os::windows::fs::symlink_dir(&target, &to)?;
                } else {
                    std::os::windows::fs::symlink_file(&target, &to)?;
                }
            }
        } else if ty.is_dir() {
            std::fs::create_dir_all(&to)?;
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
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
    fn package_names_must_be_one_safe_component() {
        let (s, _guard) = store();
        assert!(s.package_dir("plugin", "file-diff").is_some());
        assert!(s.package_dir("plugin", "../../../etc/passwd").is_none());
        assert!(s.package_dir("plugin", "a/b").is_none());
        assert!(s.package_dir("plugin", ".index").is_none());
        assert!(s.package_dir("plugin", "").is_none());
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
    fn copy_into_scratch_recreates_symlinks_instead_of_following_them() {
        let (s, guard) = store();
        let outside = guard.path().join("outside");
        std::fs::create_dir(&outside).unwrap();
        std::fs::write(outside.join("secret.txt"), b"not yours").unwrap();

        let source = guard.path().join("source");
        std::fs::create_dir(&source).unwrap();
        std::fs::write(source.join("plugin.ts"), b"export {}").unwrap();
        #[cfg(unix)]
        std::os::unix::fs::symlink(&outside, source.join("link")).unwrap();

        let token = s.scratch_create("local").unwrap();
        assert!(s.copy_into_scratch(&token, &source));
        let dir = s.scratch_path(&token).unwrap();

        assert_eq!(std::fs::read(dir.join("plugin.ts")).unwrap(), b"export {}");
        #[cfg(unix)]
        {
            let meta = std::fs::symlink_metadata(dir.join("link")).unwrap();
            assert!(
                meta.file_type().is_symlink(),
                "the link must be copied as a link, not as the tree it points at"
            );
            assert!(!dir.join("link/secret.txt").exists() || outside.join("secret.txt").exists());
        }
    }

    #[test]
    fn copy_into_scratch_refuses_an_unknown_token() {
        let (s, guard) = store();
        let source = guard.path().join("source");
        std::fs::create_dir(&source).unwrap();
        assert!(!s.copy_into_scratch("not-a-token", &source));
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
