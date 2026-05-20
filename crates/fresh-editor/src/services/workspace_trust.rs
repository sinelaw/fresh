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
//! `editor.spawnHostProcess` (plugin internals that must run on the host,
//! e.g. `devcontainer up`) bypasses the authority spawner, so it can't be
//! caught at this choke-point; it consults [`WorkspaceTrust::decide`]
//! directly at its call site instead, so the level still applies there.
//!
//! ## What this does *not* yet cover
//!
//! The interactive "prompt each time" sub-mode of Blocked (ask before each
//! spawn rather than failing outright) is not implemented; Blocked currently
//! always fails. That sub-mode needs an async UI round-trip from the spawn
//! site and lands later; this module is the enforcement core it builds on.

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
    /// The workspace root as given. `None` when no working directory is known.
    root: RwLock<Option<PathBuf>>,
    level: AtomicU8,
    /// On-disk persistence for *this* project (a per-project file). `None`
    /// for in-memory instances (e.g. tests); when present, [`Self::set_level`]
    /// writes the decision through. Swapped via [`Self::set_store`] when the
    /// working directory changes.
    store: RwLock<Option<TrustStore>>,
}

impl WorkspaceTrust {
    /// Build in-memory trust state (no persistence) for `root` at `level`.
    pub fn new(root: Option<PathBuf>, level: TrustLevel) -> Self {
        Self::build(root, level, None)
    }

    /// Build trust state backed by `store` (a per-project trust file), so
    /// [`Self::set_level`] persists the decision for this workspace.
    pub fn new_persistent(root: Option<PathBuf>, level: TrustLevel, store: TrustStore) -> Self {
        Self::build(root, level, Some(store))
    }

    fn build(root: Option<PathBuf>, level: TrustLevel, store: Option<TrustStore>) -> Self {
        Self {
            roots: RwLock::new(compute_roots(root.clone())),
            root: RwLock::new(root),
            level: AtomicU8::new(level.as_u8()),
            store: RwLock::new(store),
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
        if let Ok(store) = self.store.read() {
            if let Some(store) = store.as_ref() {
                if let Err(e) = store.record(level) {
                    tracing::warn!("workspace trust: failed to persist level: {e}");
                }
            }
        }
    }

    /// Update the workspace root after a working-directory change. Only the
    /// containment roots move here; the per-project store is swapped
    /// separately via [`Self::set_store`] (the caller knows the new project's
    /// state directory).
    pub fn set_root(&self, root: Option<PathBuf>) {
        if let Ok(mut guard) = self.roots.write() {
            *guard = compute_roots(root.clone());
        }
        if let Ok(mut guard) = self.root.write() {
            *guard = root;
        }
    }

    /// Point persistence at a new project's trust store and adopt that
    /// project's stored level (if any). Called on a working-directory change,
    /// since trust is per-project. Passing `None` detaches persistence.
    pub fn set_store(&self, store: Option<TrustStore>) {
        if let Some(store) = &store {
            // Adopt the new project's recorded decision, or fall back to the
            // safe default when it has none — never inherit the previous
            // project's level.
            let level = store.level().unwrap_or_default();
            self.level.store(level.as_u8(), Ordering::Relaxed);
        }
        if let Ok(mut guard) = self.store.write() {
            *guard = store;
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

/// Serialized form of one project's trust decision. A struct (rather than a
/// bare enum) leaves room to record more per-decision metadata later without
/// breaking the file format.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct StoredTrust {
    level: TrustLevel,
}

/// On-disk persistence of *one* project's trust decision: a small JSON file
/// (`trust.json`) inside that project's state directory
/// (`<data_dir>/workspaces/<encoded-path>/`).
///
/// One file per project — not a shared map — so concurrent `fresh` processes
/// on different projects never contend over the same file. Trust is a
/// per-user security decision and lives in the user's data dir, never inside
/// the repository (a repo must not be able to vouch for itself).
#[derive(Debug, Clone)]
pub struct TrustStore {
    path: PathBuf,
}

impl TrustStore {
    /// Trust file for the project whose state lives in `project_state_dir`
    /// (see `DirectoryContext::project_state_dir`).
    pub fn for_project_dir(project_state_dir: &Path) -> Self {
        Self {
            path: project_state_dir.join("trust.json"),
        }
    }

    /// The persisted level for this project, if one has been recorded.
    pub fn level(&self) -> Option<TrustLevel> {
        let text = std::fs::read_to_string(&self.path).ok()?;
        // A corrupt file is treated as "no decision" rather than crashing;
        // the next write rewrites it cleanly.
        serde_json::from_str::<StoredTrust>(&text)
            .ok()
            .map(|s| s.level)
    }

    /// Record `level` for this project, written atomically (a pid-tagged temp
    /// file, then rename, so a half-written file is never observed and two
    /// processes don't clobber each other's temp).
    pub fn record(&self, level: TrustLevel) -> io::Result<()> {
        if let Some(parent) = self.path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(&StoredTrust { level }).map_err(io::Error::other)?;
        let tmp = self
            .path
            .with_extension(format!("json.{}.tmp", std::process::id()));
        std::fs::write(&tmp, json.as_bytes())?;
        std::fs::rename(&tmp, &self.path)?;
        Ok(())
    }
}

/// Whether a workspace contains content whose execution trust matters — i.e.
/// whether opening it should prompt for a trust decision. Detection is
/// **passive** (a shallow scan of the root for marker files/dirs); it never
/// runs anything. A project with none of these markers is functional under
/// Restricted (system tools still run), so there's nothing to ask about.
pub fn workspace_has_executable_content(root: &Path) -> bool {
    // Files that drive env activation or repo-controlled execution.
    const FILE_MARKERS: &[&str] = &[
        ".envrc",         // direnv
        "mise.toml",      // mise
        ".mise.toml",     // mise
        ".tool-versions", // mise / asdf
        "Pipfile",        // pipenv
        "poetry.lock",    // poetry
    ];
    // Directories that hold a repo-local interpreter/toolchain.
    const DIR_MARKERS: &[&str] = &[".venv", "venv"];

    if FILE_MARKERS.iter().any(|m| root.join(m).is_file()) {
        return true;
    }
    if DIR_MARKERS.iter().any(|m| root.join(m).is_dir()) {
        return true;
    }
    // C# / .NET: loading a project runs restore/build and design-time
    // analyzers, so a solution or project file at the root is executable
    // content.
    if let Ok(entries) = std::fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                if matches!(ext, "sln" | "csproj" | "fsproj") {
                    return true;
                }
            }
        }
    }
    false
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
    fn store_round_trips_level_for_one_project() {
        let tmp = tempfile::tempdir().unwrap();
        let proj_dir = tmp.path().join("a/b/proj");
        let store = TrustStore::for_project_dir(&proj_dir);

        assert_eq!(store.level(), None);
        store.record(TrustLevel::Trusted).unwrap();
        assert_eq!(store.level(), Some(TrustLevel::Trusted));
        // Overwrite wins.
        store.record(TrustLevel::Blocked).unwrap();
        assert_eq!(store.level(), Some(TrustLevel::Blocked));
        // The file lives inside the project's own state directory.
        assert!(proj_dir.join("trust.json").exists());
    }

    #[test]
    fn separate_projects_use_separate_files() {
        let tmp = tempfile::tempdir().unwrap();
        let a = TrustStore::for_project_dir(&tmp.path().join("a"));
        let b = TrustStore::for_project_dir(&tmp.path().join("b"));
        a.record(TrustLevel::Trusted).unwrap();
        // b is untouched by a's write — no shared file.
        assert_eq!(a.level(), Some(TrustLevel::Trusted));
        assert_eq!(b.level(), None);
    }

    #[test]
    fn set_level_persists_through_store() {
        let tmp = tempfile::tempdir().unwrap();
        let proj_dir = tmp.path().join("proj");
        let wt = WorkspaceTrust::new_persistent(
            Some(proj_dir.clone()),
            TrustLevel::Restricted,
            TrustStore::for_project_dir(&proj_dir),
        );
        wt.set_level(TrustLevel::Trusted);
        // A fresh store reading the project's file sees the decision.
        assert_eq!(
            TrustStore::for_project_dir(&proj_dir).level(),
            Some(TrustLevel::Trusted)
        );
    }

    #[test]
    fn set_store_adopts_new_projects_persisted_level() {
        let tmp = tempfile::tempdir().unwrap();
        let a = tmp.path().join("a");
        let b = tmp.path().join("b");
        TrustStore::for_project_dir(&b)
            .record(TrustLevel::Blocked)
            .unwrap();

        let wt = WorkspaceTrust::new_persistent(
            Some(a.clone()),
            TrustLevel::Trusted,
            TrustStore::for_project_dir(&a),
        );
        assert_eq!(wt.level(), TrustLevel::Trusted);
        // Switching to project b adopts b's stored decision.
        wt.set_root(Some(b.clone()));
        wt.set_store(Some(TrustStore::for_project_dir(&b)));
        assert_eq!(wt.level(), TrustLevel::Blocked);
    }

    #[test]
    fn in_memory_set_level_does_not_require_store() {
        // The non-persistent constructor must never touch disk.
        let wt = WorkspaceTrust::new(Some(PathBuf::from("/home/u/proj")), TrustLevel::Restricted);
        wt.set_level(TrustLevel::Blocked);
        assert_eq!(wt.level(), TrustLevel::Blocked);
    }

    #[test]
    fn set_store_to_undecided_project_resets_to_default() {
        let tmp = tempfile::tempdir().unwrap();
        let a = tmp.path().join("a");
        let b = tmp.path().join("b"); // never recorded
        TrustStore::for_project_dir(&a)
            .record(TrustLevel::Trusted)
            .unwrap();
        let wt = WorkspaceTrust::new_persistent(
            Some(a.clone()),
            TrustLevel::Trusted,
            TrustStore::for_project_dir(&a),
        );
        assert_eq!(wt.level(), TrustLevel::Trusted);
        // Switching to an undecided project must not inherit Trusted.
        wt.set_store(Some(TrustStore::for_project_dir(&b)));
        assert_eq!(wt.level(), TrustLevel::default());
        assert_eq!(TrustLevel::default(), TrustLevel::Restricted);
    }

    #[test]
    fn executable_content_detection() {
        let tmp = tempfile::tempdir().unwrap();
        let empty = tmp.path().join("empty");
        std::fs::create_dir_all(&empty).unwrap();
        assert!(!workspace_has_executable_content(&empty));

        let envrc = tmp.path().join("envrc");
        std::fs::create_dir_all(&envrc).unwrap();
        std::fs::write(envrc.join(".envrc"), "use flake\n").unwrap();
        assert!(workspace_has_executable_content(&envrc));

        let venv = tmp.path().join("venv");
        std::fs::create_dir_all(venv.join(".venv")).unwrap();
        assert!(workspace_has_executable_content(&venv));

        let dotnet = tmp.path().join("dotnet");
        std::fs::create_dir_all(&dotnet).unwrap();
        std::fs::write(dotnet.join("App.csproj"), "<Project/>\n").unwrap();
        assert!(workspace_has_executable_content(&dotnet));
    }
}
