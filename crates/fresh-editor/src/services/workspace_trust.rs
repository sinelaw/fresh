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

use crate::services::remote::SpawnError;

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

    /// Stable lowercase name, matching the serde representation. Used to
    /// surface the level to plugins via the state snapshot.
    pub fn as_str(self) -> &'static str {
        match self {
            TrustLevel::Restricted => "restricted",
            TrustLevel::Trusted => "trusted",
            TrustLevel::Blocked => "blocked",
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

    /// A permissive, in-memory trust with no workspace root — every spawn is
    /// allowed. Used as the placeholder authority before the real trust is
    /// installed at boot, and by tests/fixtures that don't exercise gating.
    pub fn permissive() -> Self {
        Self::new(None, TrustLevel::Trusted)
    }

    /// Build a **per-session** trust handle for `root`, backed by that
    /// project's on-disk store at `project_state_dir`, adopting whatever level
    /// the user previously recorded for it (Restricted by default). Each
    /// session owns one of these, so trusting one project never raises the
    /// live trust level another open session's spawns are gated against —
    /// they read distinct handles. The shared "remember this folder" registry
    /// is the per-project store itself. See
    /// `docs/internal/PER_SESSION_BACKENDS_DESIGN.md`.
    pub fn for_session(root: &Path, project_state_dir: &Path) -> Arc<Self> {
        let trust = Self::new(Some(root.to_path_buf()), TrustLevel::Restricted);
        // `set_store` adopts the project's persisted level (or the safe
        // Restricted default for an undecided project).
        let store = TrustStore::for_project_dir(project_state_dir);
        let decided = store.is_decided();
        trust.set_store(Some(store));
        // For an *undecided* project, match the boot session's
        // `maybe_prompt_workspace_trust` default: a folder with no
        // executable-content markers (no Cargo.toml/build.rs/package.json/…)
        // is benign, so start Trusted rather than Restricted. Without this a
        // freshly-opened session for an ordinary folder would silently block
        // its own LSP / tooling. Folders *with* executable content stay
        // Restricted until the user trusts them (via the status-bar pill).
        if !decided && executable_content_markers(root).is_empty() {
            trust.set_level_transient(TrustLevel::Trusted);
        }
        Arc::new(trust)
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

    /// Set the in-memory level WITHOUT persisting. Used to seed an
    /// "initial level" while we wait for the user to explicitly choose
    /// (e.g., the open-time trust modal): the gate consults this level
    /// for any spawn that happens while the modal is up, but if the
    /// user cancels (quits) without picking a row, the on-disk store
    /// stays undecided and the modal fires again next open.
    pub fn set_level_transient(&self, level: TrustLevel) {
        self.level.store(level.as_u8(), Ordering::Relaxed);
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
            // Adopt the new project's level (the safe default when it has no
            // recorded decision) — never inherit the previous project's level.
            self.level.store(store.level().as_u8(), Ordering::Relaxed);
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

    /// This project's trust level. Always concrete: a project that has never
    /// been decided reads as the safe default (`Restricted`) — there is no
    /// "undecided" trust *value*. Whether a decision has actually been recorded
    /// (and thus whether to prompt) is a separate question, see [`Self::is_decided`].
    pub fn level(&self) -> TrustLevel {
        self.recorded_level().unwrap_or_default()
    }

    /// Whether this project has a recorded trust decision on disk. Drives the
    /// open-time prompt: undecided projects are prompted, decided ones are not.
    pub fn is_decided(&self) -> bool {
        self.recorded_level().is_some()
    }

    /// The raw recorded level, or `None` if no valid decision is on disk. A
    /// corrupt file reads as `None` (treated as undecided; the next write
    /// rewrites it cleanly) rather than crashing. Private: callers want either
    /// a concrete [`Self::level`] or the [`Self::is_decided`] predicate.
    fn recorded_level(&self) -> Option<TrustLevel> {
        let text = std::fs::read_to_string(&self.path).ok()?;
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
        let json =
            serde_json::to_string_pretty(&StoredTrust { level }).map_err(io::Error::other)?;
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
/// runs anything.
///
/// This covers both env-manager files *and* project manifests, because a
/// recognized project is one whose language server will auto-start — and that
/// load runs project-controlled code (analyzers, build scripts, proc-macros),
/// which is gated on trust (see `LspManager`). So prompting for a recognized
/// project is what lets the user enable its tooling. A folder with none of
/// these is plain text/docs — nothing to gate, no prompt.
pub fn workspace_has_executable_content(root: &Path) -> bool {
    !executable_content_markers(root).is_empty()
}

/// The specific marker files/dirs in `root` that make it "executable content"
/// (see [`workspace_has_executable_content`]). Returned so the trust prompt can
/// tell the user *why* it appeared (e.g. ".envrc, .venv, App.sln"). Shallow,
/// passive scan — never runs anything. Order is roughly env-managers, then
/// repo-local toolchains, then devcontainer, then .NET project files.
pub fn executable_content_markers(root: &Path) -> Vec<String> {
    let mut found = Vec::new();

    // Env-manager markers come from the single built-in detector list
    // (`config::default_env_detectors`), so the trust prompt and env
    // activation can never disagree about what an env file is. We check
    // existence only (file *or* dir) and ignore each detector's `require`
    // evidence: for trust, the mere presence of `.venv`/`.envrc`/`Pipfile`/…
    // is the signal; whether the env is actually *activatable* is the
    // activation path's concern (`detect_env`), not trust's.
    for d in crate::config::default_env_detectors() {
        for m in &d.markers {
            if root.join(m).exists() {
                found.push(m.clone());
            }
        }
    }

    // Project manifests whose language servers run project code at load.
    const FILE_MARKERS: &[&str] = &[
        "Cargo.toml",            // rust-analyzer: build scripts, proc-macros
        "go.mod",                // gopls
        "package.json",          // ts/eslint, npm scripts
        "pyproject.toml",        // python tooling
        "pom.xml",               // jdtls / maven
        "build.gradle",          // jdtls / gradle
        "build.gradle.kts",      // gradle (kotlin dsl)
        "CMakeLists.txt",        // clangd (compile_commands generation)
        "compile_commands.json", // clangd
        "Gemfile",               // ruby
        "composer.json",         // php
    ];
    for m in FILE_MARKERS {
        if root.join(m).is_file() {
            found.push((*m).to_string());
        }
    }
    // Dev container: reopening / building it runs code.
    if root
        .join(".devcontainer")
        .join("devcontainer.json")
        .is_file()
        || root.join(".devcontainer.json").is_file()
    {
        found.push("devcontainer.json".to_string());
    }
    // C# / .NET: loading a project runs restore/build and design-time
    // analyzers/source-generators, so a solution or project file at the root
    // is executable content. Report the actual file name(s).
    if let Ok(entries) = std::fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                if matches!(ext, "sln" | "csproj" | "fsproj") {
                    if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                        found.push(name.to_string());
                    }
                }
            }
        }
    }
    found
}

/// The activatable environment detected at a workspace root. Serializes to
/// `{ "name", "kind", "snippet" }` (kind as `"path-only"` / `"shell"`), which
/// is what the env-manager plugin reads via `editor.detectedEnv()`.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct DetectedEnv {
    /// Status-pill label (the matching detector's `name`).
    pub name: String,
    /// Activation risk class.
    pub kind: crate::config::EnvKind,
    /// Ready-to-run activation snippet (`{dir}` already expanded to `root`).
    pub snippet: String,
}

/// Detect the activatable environment at `root` using `detectors`
/// (first match wins). The single activation-detection entry point: the
/// env-manager plugin consumes this result and never probes the filesystem
/// itself, and core surfaces it through the plugin state snapshot.
///
/// A detector matches when any of its `markers` exists *and* — if it lists
/// `require` evidence — at least one required path also exists (e.g. a `.venv`
/// directory that actually contains an interpreter). The returned snippet has
/// `{dir}` expanded to `root`.
pub fn detect_env(root: &Path, detectors: &[crate::config::EnvDetector]) -> Option<DetectedEnv> {
    for d in detectors {
        if !d.markers.iter().any(|m| root.join(m).exists()) {
            continue;
        }
        if !d.require.is_empty() && !d.require.iter().any(|r| root.join(r).exists()) {
            continue;
        }
        return Some(DetectedEnv {
            name: d.name.clone(),
            kind: d.kind,
            snippet: d.snippet.replace("{dir}", &root.to_string_lossy()),
        });
    }
    None
}

/// Map a trust decision for `command` (with the child's `cwd`) onto a spawn
/// result: `Ok(())` to proceed, or an `Err` carrying the deny reason. The
/// shared one-liner every spawner impl calls at the top of each spawn method,
/// so the Allow/Deny→error policy lives in exactly one place even though the
/// *check site* is per-backend.
pub fn gate(trust: &WorkspaceTrust, command: &str, cwd: Option<&str>) -> Result<(), SpawnError> {
    match trust.decide(command, cwd) {
        SpawnDecision::Allow => Ok(()),
        SpawnDecision::Deny(reason) => Err(SpawnError::Process(reason)),
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
        assert_eq!(
            t.decide("/home/u/proj/.venv/bin/python", None),
            SpawnDecision::Allow
        );
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
        assert_eq!(t.decide("/anything/at/all", None), SpawnDecision::Allow);
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
        for lvl in [
            TrustLevel::Restricted,
            TrustLevel::Trusted,
            TrustLevel::Blocked,
        ] {
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

        // Undecided reads as the safe default, not as a missing value.
        assert!(!store.is_decided());
        assert_eq!(store.level(), TrustLevel::default());
        store.record(TrustLevel::Trusted).unwrap();
        assert!(store.is_decided());
        assert_eq!(store.level(), TrustLevel::Trusted);
        // Overwrite wins.
        store.record(TrustLevel::Blocked).unwrap();
        assert_eq!(store.level(), TrustLevel::Blocked);
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
        assert_eq!(a.level(), TrustLevel::Trusted);
        assert!(a.is_decided());
        assert!(!b.is_decided());
        assert_eq!(b.level(), TrustLevel::default());
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
            TrustLevel::Trusted
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

    #[test]
    fn executable_content_markers_lists_what_triggered() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        std::fs::write(root.join(".envrc"), "use flake\n").unwrap();
        std::fs::write(root.join("mise.toml"), "[tools]\n").unwrap();
        std::fs::create_dir_all(root.join(".venv")).unwrap();
        std::fs::create_dir_all(root.join(".devcontainer")).unwrap();
        std::fs::write(root.join(".devcontainer").join("devcontainer.json"), "{}\n").unwrap();
        std::fs::write(root.join("App.csproj"), "<Project/>\n").unwrap();

        let markers = executable_content_markers(root);
        for expected in [
            ".envrc",
            "mise.toml",
            ".venv",
            "devcontainer.json",
            "App.csproj",
        ] {
            assert!(
                markers.iter().any(|m| m == expected),
                "expected '{expected}' in {markers:?}"
            );
        }

        // A plain folder reports nothing.
        let empty = tmp.path().join("empty");
        std::fs::create_dir_all(&empty).unwrap();
        assert!(executable_content_markers(&empty).is_empty());
    }

    // === detect_env (the single activation-detection entry point) ===

    use crate::config::{default_env_detectors, EnvKind};

    fn detect_default(root: &Path) -> Option<DetectedEnv> {
        detect_env(root, &default_env_detectors())
    }

    #[test]
    fn detect_env_none_for_plain_folder() {
        let tmp = tempfile::tempdir().unwrap();
        assert_eq!(detect_default(tmp.path()), None);
    }

    #[test]
    fn detect_env_venv_requires_an_interpreter() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        // A bare `.venv` dir with no interpreter must NOT detect as activatable
        // (the trust marker scan still flags it, but activation needs evidence).
        std::fs::create_dir_all(root.join(".venv")).unwrap();
        assert_eq!(detect_default(root), None);
        assert!(executable_content_markers(root)
            .iter()
            .any(|m| m == ".venv"));

        // Add an interpreter → now it's an activatable path-only env.
        std::fs::create_dir_all(root.join(".venv/bin")).unwrap();
        std::fs::write(root.join(".venv/bin/python"), "").unwrap();
        let det = detect_default(root).expect("venv detected");
        assert_eq!(det.name, ".venv");
        assert_eq!(det.kind, EnvKind::PathOnly);
        // Relative path (recipe runs in the workspace root) — no absolute-path
        // interpolation, so no shell-injection surface.
        assert_eq!(det.snippet, "source .venv/bin/activate");
    }

    #[test]
    fn detect_env_direnv_and_mise_are_shell() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        std::fs::write(root.join(".envrc"), "use flake\n").unwrap();
        let det = detect_default(root).expect("direnv detected");
        assert_eq!(det.name, "direnv");
        assert_eq!(det.kind, EnvKind::Shell);
        assert_eq!(det.snippet, "eval \"$(direnv export bash)\"");

        let mise = tmp.path().join("mise");
        std::fs::create_dir_all(&mise).unwrap();
        std::fs::write(mise.join(".tool-versions"), "python 3.12\n").unwrap();
        let det = detect_default(&mise).expect("mise detected");
        assert_eq!(det.name, "mise");
        assert_eq!(det.kind, EnvKind::Shell);
    }

    #[test]
    fn detect_env_pipenv_and_poetry() {
        let tmp = tempfile::tempdir().unwrap();
        let pip = tmp.path().join("pip");
        std::fs::create_dir_all(&pip).unwrap();
        std::fs::write(pip.join("Pipfile"), "[packages]\n").unwrap();
        assert_eq!(detect_default(&pip).map(|d| d.name), Some("pipenv".into()));

        let poetry = tmp.path().join("poetry");
        std::fs::create_dir_all(&poetry).unwrap();
        std::fs::write(poetry.join("poetry.lock"), "\n").unwrap();
        assert_eq!(
            detect_default(&poetry).map(|d| d.name),
            Some("poetry".into())
        );
    }

    #[test]
    fn detect_env_first_detector_wins() {
        // A folder with both a real `.venv` and an `.envrc` resolves to the
        // first matching detector in the list (venv precedes direnv).
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        std::fs::create_dir_all(root.join(".venv/bin")).unwrap();
        std::fs::write(root.join(".venv/bin/python3"), "").unwrap();
        std::fs::write(root.join(".envrc"), "\n").unwrap();
        assert_eq!(detect_default(root).map(|d| d.name), Some(".venv".into()));
    }

    #[test]
    fn detect_env_honors_custom_config() {
        // A user-defined detector is respected — detection is data-driven.
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        std::fs::write(root.join(".nvmrc"), "20\n").unwrap();
        let detectors = vec![crate::config::EnvDetector {
            name: "node".into(),
            markers: vec![".nvmrc".into()],
            kind: EnvKind::Shell,
            snippet: "eval \"$(fnm env)\"".into(),
            require: vec![],
        }];
        let det = detect_env(root, &detectors).expect("custom env detected");
        assert_eq!(det.name, "node");
        assert_eq!(det.snippet, "eval \"$(fnm env)\"");
    }
}
