//! Workspace Trust — gate process execution by a per-project trust level.
//!
//! A freshly opened project may contain attacker-controlled content that
//! only becomes dangerous when *executed*: a repo-placed `./.venv/bin/python`,
//! a `.envrc`, a project's analyzers/build commands. Workspace Trust is the
//! single gate that decides, per workspace, whether such content may run.
//!
//! There are three levels (see `docs/internal/remote-env-manager-design.md`):
//!
//! - [`TrustLevel::Restricted`] (the eventual default): no repo-controlled
//!   code runs. A spawn whose **explicit executable path** resolves inside the
//!   workspace is refused; ordinary spawns of system/user tools (a bare command
//!   name resolved via `$PATH`) proceed. Env managers do not activate, so no
//!   repo `bin/` is ever prepended to `PATH` — which is why a bare name is
//!   safe to allow.
//! - [`TrustLevel::Trusted`]: every spawn is allowed.
//! - [`TrustLevel::Blocked`]: every spawn fails.
//!
//! ## Enforcement point
//!
//! Every editor primitive that runs a child — the integrated terminal, LSP
//! server spawn, plugin `spawnProcess`, formatters, find-in-files — routes
//! through the active [`Authority`](crate::services::authority::Authority)'s
//! [`ProcessSpawner`] / [`LongRunningSpawner`]. Wrapping those two spawners is
//! therefore the one place that covers all of them with no per-caller
//! cooperation. [`Authority::with_trust`](crate::services::authority::Authority::with_trust)
//! installs the wrappers; the server calls it once per editor build.
//!
//! ## What this does *not* yet cover
//!
//! `editor.spawnHostProcess` (plugin internals that must run on the host before
//! an authority exists, e.g. `devcontainer up`) deliberately bypasses the
//! authority and so is *not* gated here. Gating host spawns, and the
//! interactive "prompt each time" sub-mode of Blocked, land alongside the
//! trust-granting UI; this module is the enforcement core they build on.

use std::collections::HashMap;
use std::io;
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Arc, RwLock};

use crate::services::remote::{
    LongRunningSpawner, ProcessSpawner, SpawnError, SpawnResult, StdioChild,
};
use crate::types::ProcessLimits;

/// Per-workspace trust level.
///
/// `Default` is [`TrustLevel::Restricted`] — the safe choice for any
/// never-decided project, and the value persisted state should fall back to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum TrustLevel {
    /// No repo-controlled execution; system/user tools still run.
    #[default]
    Restricted,
    /// Full execution.
    Trusted,
    /// No execution at all.
    Blocked,
}

impl TrustLevel {
    fn from_u8(v: u8) -> Self {
        match v {
            1 => TrustLevel::Trusted,
            2 => TrustLevel::Blocked,
            // 0 and any unexpected value fall back to the safe default.
            _ => TrustLevel::Restricted,
        }
    }

    fn as_u8(self) -> u8 {
        match self {
            TrustLevel::Restricted => 0,
            TrustLevel::Trusted => 1,
            TrustLevel::Blocked => 2,
        }
    }
}

/// Outcome of consulting [`WorkspaceTrust::decide`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpawnDecision {
    /// The spawn may proceed.
    Allow,
    /// The spawn is refused; the string is a user-facing reason.
    Deny(String),
}

/// Shared, interior-mutable trust state for one workspace.
///
/// Held behind an `Arc` by the server (so the level survives editor rebuilds)
/// and by the guarding spawners (so they read the current level on every
/// spawn). The workspace root is mutable because a session can change its
/// working directory in place.
pub struct WorkspaceTrust {
    /// Normalized workspace roots a spawn is checked against: the working
    /// directory as given, plus its canonical form (they differ when the
    /// path traverses a symlink, e.g. `/tmp` → `/private/tmp` on macOS).
    /// A spawn inside *either* counts as inside the workspace.
    roots: RwLock<Vec<PathBuf>>,
    /// The workspace root as given (the persistence key). `None` when no
    /// working directory is known.
    root: RwLock<Option<PathBuf>>,
    level: AtomicU8,
    /// On-disk persistence. `None` for in-memory instances (e.g. tests);
    /// when present, [`Self::set_level`] writes the decision through.
    store: Option<TrustStore>,
}

impl WorkspaceTrust {
    /// Build in-memory trust state (no persistence) for `root` at `level`.
    pub fn new(root: Option<PathBuf>, level: TrustLevel) -> Self {
        Self::build(root, level, None)
    }

    /// Build trust state backed by `store`, so [`Self::set_level`] persists
    /// the decision keyed by the workspace path.
    pub fn new_persistent(root: Option<PathBuf>, level: TrustLevel, store: TrustStore) -> Self {
        Self::build(root, level, Some(store))
    }

    fn build(root: Option<PathBuf>, level: TrustLevel, store: Option<TrustStore>) -> Self {
        Self {
            roots: RwLock::new(compute_roots(root.clone())),
            root: RwLock::new(root),
            level: AtomicU8::new(level.as_u8()),
            store,
        }
    }

    /// Current trust level.
    pub fn level(&self) -> TrustLevel {
        TrustLevel::from_u8(self.level.load(Ordering::Relaxed))
    }

    /// Set the trust level. Takes effect on the next spawn — no rebuild
    /// required (the guarding spawners read this live). When the instance is
    /// persistent, the decision is written through to disk for this workspace.
    pub fn set_level(&self, level: TrustLevel) {
        self.level.store(level.as_u8(), Ordering::Relaxed);
        if let Some(store) = &self.store {
            if let Ok(root) = self.root.read() {
                if let Some(root) = root.as_ref() {
                    if let Err(e) = store.record(root, level) {
                        tracing::warn!("workspace trust: failed to persist level: {e}");
                    }
                }
            }
        }
    }

    /// Update the workspace root after a working-directory change. The trust
    /// level is per-path, so a persistent instance re-adopts the new path's
    /// stored decision (leaving the current level unchanged if none exists).
    pub fn set_root(&self, root: Option<PathBuf>) {
        if let Ok(mut guard) = self.roots.write() {
            *guard = compute_roots(root.clone());
        }
        if let Ok(mut guard) = self.root.write() {
            *guard = root.clone();
        }
        if let Some(store) = &self.store {
            if let Some(root) = root.as_ref() {
                if let Some(level) = store.level_for(root) {
                    self.level.store(level.as_u8(), Ordering::Relaxed);
                }
            }
        }
    }

    /// Decide whether spawning `command` (with the child's `cwd`) may proceed.
    pub fn decide(&self, command: &str, cwd: Option<&str>) -> SpawnDecision {
        match self.level() {
            TrustLevel::Trusted => SpawnDecision::Allow,
            TrustLevel::Blocked => SpawnDecision::Deny(
                "workspace trust is set to Blocked — no processes may run".to_string(),
            ),
            TrustLevel::Restricted => self.decide_restricted(command, cwd),
        }
    }

    fn decide_restricted(&self, command: &str, cwd: Option<&str>) -> SpawnDecision {
        // A bare command name (no path separator) is resolved by the OS via
        // `$PATH`. Under Restricted no env is activated, so the repo's `bin/`
        // is never on `$PATH` and a bare name resolves to a system/user tool.
        // Allow it; only explicit paths can be judged for containment.
        if !looks_like_path(command) {
            return SpawnDecision::Allow;
        }

        let roots = match self.roots.read() {
            Ok(g) => g,
            // A poisoned lock should never gate the editor open/shut; fail
            // open here (Restricted's job is to stop *repo* execution, and a
            // poisoned lock is an internal bug, not a hostile project).
            Err(_) => return SpawnDecision::Allow,
        };
        if roots.is_empty() {
            // No known workspace root → can't judge containment. Allow.
            return SpawnDecision::Allow;
        }

        let base = roots[0].as_path();
        let candidate = resolve_against(command, cwd, base);
        if roots.iter().any(|r| path_is_within(&candidate, r)) {
            SpawnDecision::Deny(format!(
                "workspace trust is Restricted — refusing to run '{command}' \
                 from inside the project; trust this folder to allow it"
            ))
        } else {
            SpawnDecision::Allow
        }
    }
}

/// Build the list of normalized roots (given + canonical) to check against.
fn compute_roots(root: Option<PathBuf>) -> Vec<PathBuf> {
    let Some(root) = root else {
        return Vec::new();
    };
    let mut roots = vec![lexical_normalize(&root)];
    if let Ok(canonical) = std::fs::canonicalize(&root) {
        let canonical = lexical_normalize(&canonical);
        if !roots.contains(&canonical) {
            roots.push(canonical);
        }
    }
    roots
}

/// Whether `command` names a path (vs. a bare name resolved via `$PATH`).
fn looks_like_path(command: &str) -> bool {
    command.contains('/') || command.contains('\\')
}

/// Resolve `command` to an absolute, lexically-normalized path. Relative
/// commands resolve against the child's `cwd` when given (else `base`).
fn resolve_against(command: &str, cwd: Option<&str>, base: &Path) -> PathBuf {
    let p = Path::new(command);
    if p.is_absolute() {
        return lexical_normalize(p);
    }
    let cwd_base = match cwd {
        Some(c) if Path::new(c).is_absolute() => PathBuf::from(c),
        Some(c) => base.join(c),
        None => base.to_path_buf(),
    };
    lexical_normalize(&cwd_base.join(p))
}

/// Lexically resolve `.`/`..` without touching the filesystem (so it never
/// fails or blocks, and works on paths that don't exist yet).
fn lexical_normalize(p: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for comp in p.components() {
        match comp {
            Component::CurDir => {}
            Component::ParentDir => {
                // Pop a real directory component; otherwise keep the `..`
                // (e.g. a leading `..` with nothing above it to cancel).
                if out.file_name().is_some() {
                    out.pop();
                } else {
                    out.push("..");
                }
            }
            other => out.push(other.as_os_str()),
        }
    }
    out
}

/// Whether `candidate` is at or under `root` (both already normalized).
fn path_is_within(candidate: &Path, root: &Path) -> bool {
    candidate == root || candidate.starts_with(root)
}

/// On-disk persistence of trust decisions, keyed by canonical workspace path.
///
/// A single small JSON map at `<config_dir>/workspace-trust.json`, e.g.
/// `{ "/home/u/proj": "trusted", "/home/u/other": "blocked" }`. This is a
/// core-owned file — trust is a per-user security decision and must never
/// live inside the repository (a repo could otherwise vouch for itself).
#[derive(Debug, Clone)]
pub struct TrustStore {
    path: PathBuf,
}

impl TrustStore {
    /// Store rooted at `<config_dir>/workspace-trust.json`.
    pub fn new(config_dir: &Path) -> Self {
        Self {
            path: config_dir.join("workspace-trust.json"),
        }
    }

    /// The persisted level for `workspace`, if any has been recorded.
    pub fn level_for(&self, workspace: &Path) -> Option<TrustLevel> {
        self.load().get(&canonical_key(workspace)).copied()
    }

    /// Record `level` for `workspace`, persisting the whole map atomically
    /// (write to a temp file, then rename).
    pub fn record(&self, workspace: &Path, level: TrustLevel) -> io::Result<()> {
        let mut map = self.load();
        map.insert(canonical_key(workspace), level);
        self.save(&map)
    }

    fn load(&self) -> HashMap<String, TrustLevel> {
        match std::fs::read_to_string(&self.path) {
            Ok(s) => serde_json::from_str(&s).unwrap_or_default(),
            // Missing file (first run) or unreadable → empty map. A
            // corrupt file is treated as empty rather than crashing the
            // editor; the next write rewrites it cleanly.
            Err(_) => HashMap::new(),
        }
    }

    fn save(&self, map: &HashMap<String, TrustLevel>) -> io::Result<()> {
        if let Some(parent) = self.path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(map).map_err(io::Error::other)?;
        let tmp = self.path.with_extension("json.tmp");
        std::fs::write(&tmp, json.as_bytes())?;
        std::fs::rename(&tmp, &self.path)?;
        Ok(())
    }
}

/// Stable persistence key for a workspace path: canonicalized when possible
/// (so symlinked spellings of the same directory share a decision), else
/// lexically normalized.
fn canonical_key(p: &Path) -> String {
    let canonical = std::fs::canonicalize(p).unwrap_or_else(|_| p.to_path_buf());
    lexical_normalize(&canonical).to_string_lossy().into_owned()
}

/// Wraps a [`ProcessSpawner`] so every one-shot spawn is gated by trust.
pub struct TrustGuardedProcessSpawner {
    inner: Arc<dyn ProcessSpawner>,
    trust: Arc<WorkspaceTrust>,
}

impl TrustGuardedProcessSpawner {
    pub fn new(inner: Arc<dyn ProcessSpawner>, trust: Arc<WorkspaceTrust>) -> Self {
        Self { inner, trust }
    }

    fn gate(&self, command: &str, cwd: Option<&str>) -> Result<(), SpawnError> {
        match self.trust.decide(command, cwd) {
            SpawnDecision::Allow => Ok(()),
            SpawnDecision::Deny(reason) => Err(SpawnError::Process(reason)),
        }
    }
}

#[async_trait::async_trait]
impl ProcessSpawner for TrustGuardedProcessSpawner {
    async fn spawn(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
    ) -> Result<SpawnResult, SpawnError> {
        self.gate(&command, cwd.as_deref())?;
        self.inner.spawn(command, args, cwd).await
    }

    async fn spawn_to_file(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: PathBuf,
    ) -> Result<SpawnResult, SpawnError> {
        self.gate(&command, cwd.as_deref())?;
        self.inner.spawn_to_file(command, args, cwd, stdout_to).await
    }

    async fn spawn_cancellable(
        &self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: Option<PathBuf>,
        kill_rx: tokio::sync::oneshot::Receiver<()>,
    ) -> Result<SpawnResult, SpawnError> {
        self.gate(&command, cwd.as_deref())?;
        self.inner
            .spawn_cancellable(command, args, cwd, stdout_to, kill_rx)
            .await
    }
}

/// Wraps a [`LongRunningSpawner`] so every LSP/tool spawn is gated by trust.
pub struct TrustGuardedLongRunningSpawner {
    inner: Arc<dyn LongRunningSpawner>,
    trust: Arc<WorkspaceTrust>,
}

impl TrustGuardedLongRunningSpawner {
    pub fn new(inner: Arc<dyn LongRunningSpawner>, trust: Arc<WorkspaceTrust>) -> Self {
        Self { inner, trust }
    }
}

#[async_trait::async_trait]
impl LongRunningSpawner for TrustGuardedLongRunningSpawner {
    async fn spawn_stdio(
        &self,
        command: &str,
        args: &[String],
        env: Vec<(String, String)>,
        cwd: Option<&Path>,
        limits: Option<&ProcessLimits>,
    ) -> Result<StdioChild, SpawnError> {
        let cwd_str = cwd.map(|p| p.to_string_lossy().into_owned());
        match self.trust.decide(command, cwd_str.as_deref()) {
            SpawnDecision::Allow => {}
            SpawnDecision::Deny(reason) => return Err(SpawnError::Process(reason)),
        }
        self.inner.spawn_stdio(command, args, env, cwd, limits).await
    }

    async fn command_exists(&self, command: &str) -> bool {
        // Existence probing is read-only — it doesn't run repo content — so
        // it isn't gated. (For local authorities this is `which::which`,
        // not a spawn at all.)
        self.inner.command_exists(command).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn trust(root: &str, level: TrustLevel) -> WorkspaceTrust {
        WorkspaceTrust::new(Some(PathBuf::from(root)), level)
    }

    #[test]
    fn trusted_allows_everything() {
        let t = trust("/home/u/proj", TrustLevel::Trusted);
        assert_eq!(t.decide("/home/u/proj/.venv/bin/python", None), SpawnDecision::Allow);
        assert_eq!(t.decide("rg", None), SpawnDecision::Allow);
    }

    #[test]
    fn blocked_denies_everything() {
        let t = trust("/home/u/proj", TrustLevel::Blocked);
        assert!(matches!(t.decide("rg", None), SpawnDecision::Deny(_)));
        assert!(matches!(
            t.decide("/usr/bin/git", None),
            SpawnDecision::Deny(_)
        ));
    }

    #[test]
    fn restricted_allows_bare_command_names() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        // System/user tools resolved via $PATH are fine.
        assert_eq!(t.decide("git", None), SpawnDecision::Allow);
        assert_eq!(t.decide("rg", Some("/home/u/proj")), SpawnDecision::Allow);
        assert_eq!(t.decide("python3", None), SpawnDecision::Allow);
    }

    #[test]
    fn restricted_blocks_absolute_path_inside_workspace() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        assert!(matches!(
            t.decide("/home/u/proj/.venv/bin/python", None),
            SpawnDecision::Deny(_)
        ));
    }

    #[test]
    fn restricted_allows_absolute_path_outside_workspace() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        assert_eq!(t.decide("/usr/bin/python3", None), SpawnDecision::Allow);
    }

    #[test]
    fn restricted_blocks_relative_path_resolving_into_workspace() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        // `./.venv/bin/python` from the project cwd.
        assert!(matches!(
            t.decide("./.venv/bin/python", Some("/home/u/proj")),
            SpawnDecision::Deny(_)
        ));
        // A nested cwd still resolves inside.
        assert!(matches!(
            t.decide("../.venv/bin/python", Some("/home/u/proj/src")),
            SpawnDecision::Deny(_)
        ));
    }

    #[test]
    fn restricted_allows_relative_path_escaping_workspace() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        // `../evil` from the project root lands outside the workspace.
        assert_eq!(
            t.decide("../evil", Some("/home/u/proj")),
            SpawnDecision::Allow
        );
    }

    #[test]
    fn restricted_does_not_confuse_sibling_prefix() {
        // `/home/u/proj-evil` must not count as inside `/home/u/proj`.
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        assert_eq!(
            t.decide("/home/u/proj-evil/bin/x", None),
            SpawnDecision::Allow
        );
    }

    #[test]
    fn restricted_without_root_allows() {
        let t = WorkspaceTrust::new(None, TrustLevel::Restricted);
        assert_eq!(
            t.decide("/anything/at/all", None),
            SpawnDecision::Allow
        );
    }

    #[test]
    fn set_level_takes_effect_immediately() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        let cmd = "/home/u/proj/.venv/bin/python";
        assert!(matches!(t.decide(cmd, None), SpawnDecision::Deny(_)));
        t.set_level(TrustLevel::Trusted);
        assert_eq!(t.decide(cmd, None), SpawnDecision::Allow);
        t.set_level(TrustLevel::Blocked);
        assert!(matches!(t.decide("rg", None), SpawnDecision::Deny(_)));
    }

    #[test]
    fn set_root_updates_containment() {
        let t = trust("/home/u/proj", TrustLevel::Restricted);
        let cmd = "/home/u/other/.venv/bin/python";
        assert_eq!(t.decide(cmd, None), SpawnDecision::Allow);
        t.set_root(Some(PathBuf::from("/home/u/other")));
        assert!(matches!(t.decide(cmd, None), SpawnDecision::Deny(_)));
    }

    #[test]
    fn level_round_trips_through_u8() {
        for lvl in [TrustLevel::Restricted, TrustLevel::Trusted, TrustLevel::Blocked] {
            assert_eq!(TrustLevel::from_u8(lvl.as_u8()), lvl);
        }
        // Unknown byte falls back to the safe default.
        assert_eq!(TrustLevel::from_u8(99), TrustLevel::Restricted);
    }

    #[test]
    fn lexical_normalize_resolves_dot_segments() {
        assert_eq!(
            lexical_normalize(Path::new("/a/b/../c/./d")),
            PathBuf::from("/a/c/d")
        );
    }

    #[test]
    fn store_round_trips_level_per_path() {
        let tmp = tempfile::tempdir().unwrap();
        let store = TrustStore::new(tmp.path());
        let ws = tmp.path().join("proj");
        std::fs::create_dir_all(&ws).unwrap();

        assert_eq!(store.level_for(&ws), None);
        store.record(&ws, TrustLevel::Trusted).unwrap();
        assert_eq!(store.level_for(&ws), Some(TrustLevel::Trusted));
        // Overwrite wins.
        store.record(&ws, TrustLevel::Blocked).unwrap();
        assert_eq!(store.level_for(&ws), Some(TrustLevel::Blocked));
        // A different workspace is independent.
        let other = tmp.path().join("other");
        std::fs::create_dir_all(&other).unwrap();
        assert_eq!(store.level_for(&other), None);
    }

    #[test]
    fn set_level_persists_through_store() {
        let tmp = tempfile::tempdir().unwrap();
        let ws = tmp.path().join("proj");
        std::fs::create_dir_all(&ws).unwrap();
        let wt = WorkspaceTrust::new_persistent(
            Some(ws.clone()),
            TrustLevel::Restricted,
            TrustStore::new(tmp.path()),
        );
        wt.set_level(TrustLevel::Trusted);
        // A fresh store sees the decision written to disk.
        assert_eq!(
            TrustStore::new(tmp.path()).level_for(&ws),
            Some(TrustLevel::Trusted)
        );
    }

    #[test]
    fn set_root_adopts_persisted_level_for_new_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let a = tmp.path().join("a");
        let b = tmp.path().join("b");
        std::fs::create_dir_all(&a).unwrap();
        std::fs::create_dir_all(&b).unwrap();
        let store = TrustStore::new(tmp.path());
        store.record(&b, TrustLevel::Blocked).unwrap();

        let wt = WorkspaceTrust::new_persistent(Some(a.clone()), TrustLevel::Trusted, store);
        assert_eq!(wt.level(), TrustLevel::Trusted);
        // Switching to a dir with a stored decision adopts it.
        wt.set_root(Some(b.clone()));
        assert_eq!(wt.level(), TrustLevel::Blocked);
    }

    #[test]
    fn in_memory_set_level_does_not_require_store() {
        // The non-persistent constructor must never touch disk.
        let wt = WorkspaceTrust::new(Some(PathBuf::from("/home/u/proj")), TrustLevel::Restricted);
        wt.set_level(TrustLevel::Blocked);
        assert_eq!(wt.level(), TrustLevel::Blocked);
    }
}
