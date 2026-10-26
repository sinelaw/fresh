//! Live environment provider.
//!
//! The active environment is a **recipe** — a shell snippet plus the project
//! directory — not a stored snapshot. It is re-evaluated on demand by running
//! the snippet on the active backend's host and capturing the resulting
//! environment, so it can never go stale. A content-hash cache over the env
//! inputs (`.envrc`, `mise.toml`, …) keeps the common path free without making
//! correctness depend on the cache.
//!
//! Shared and interior-mutable, exactly like
//! [`WorkspaceTrust`](crate::services::workspace_trust::WorkspaceTrust): every
//! spawner holds the same `Arc<EnvProvider>`, the plugin sets the recipe in
//! place via `editor.setEnv` / `clearEnv`, and there is no authority rebuild.
//!
//! The provider is backend-agnostic: [`EnvProvider::current`] builds the
//! capture *script* and hands it to a caller-supplied `run` closure that
//! actually executes it on the right host (local tokio / SSH / docker). That
//! closure must run a **raw** spawn that does not itself apply this provider's
//! env — otherwise capturing the env would recurse.

use crate::services::process_hidden::HideWindow;
use std::collections::hash_map::DefaultHasher;
use std::future::Future;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

/// Files whose contents define a project's environment. Used to key the
/// capture cache: if none changed, the captured env can't have changed (for
/// the common, file-driven managers).
const ENV_INPUT_FILES: &[&str] = &[
    ".envrc",
    "mise.toml",
    ".mise.toml",
    ".tool-versions",
    "pyvenv.cfg",
    ".venv/pyvenv.cfg",
];

struct State {
    /// The activation snippet. Empty/whitespace ⇒ inactive (no env applied).
    snippet: String,
    /// Project directory the snippet runs in.
    dir: Option<PathBuf>,
    /// Last full-snapshot capture, keyed by the env-inputs hash it was produced
    /// under. Used by [`EnvProvider::current`] (LSP / one-shot spawns).
    cache: Option<Cached>,
    /// Last *delta* capture (activation changes over a clean login shell),
    /// keyed by the same hash. Used by the integrated terminal.
    delta_cache: Option<CachedDelta>,
}

struct Cached {
    inputs_hash: u64,
    vars: Vec<(String, String)>,
}

struct CachedDelta {
    inputs_hash: u64,
    delta: EnvDelta,
}

/// An environment **delta**: the changes the activation recipe introduces over
/// a clean login shell. `set` are keys to add/override; `unset` are keys the
/// recipe removed. Applying a delta to a child's environment is shell-agnostic
/// and carries only the activation's contribution — never volatile shell
/// bookkeeping (`PWD`, `SHLVL`, …), which is identical in baseline and
/// activated and so diffs out.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EnvDelta {
    pub set: Vec<(String, String)>,
    pub unset: Vec<String>,
}

impl EnvDelta {
    /// No changes — applying it is a no-op (inactive env or capture failure).
    pub fn is_empty(&self) -> bool {
        self.set.is_empty() && self.unset.is_empty()
    }
}

/// Shared, live environment recipe.
pub struct EnvProvider {
    state: RwLock<State>,
    /// Per-project recipe store. When set, [`Self::set`] / [`Self::clear`]
    /// write the recipe through to disk so the next launch can boot already
    /// in this env (no post-boot `setEnv` → restart flicker, issue #2280).
    /// `None` for placeholder / non-persistent providers (remote stubs, tests).
    store: RwLock<Option<EnvStore>>,
}

impl EnvProvider {
    /// An inactive provider — no snippet, applies no env, no persistence.
    pub fn inactive() -> Self {
        Self {
            state: RwLock::new(State {
                snippet: String::new(),
                dir: None,
                cache: None,
                delta_cache: None,
            }),
            store: RwLock::new(None),
        }
    }

    /// A per-session provider backed by this project's recipe store. When
    /// `trusted`, any recipe the user previously activated for the project is
    /// restored immediately, so the session boots already in its env and
    /// tooling spawns under it from frame zero — there is no auto-activation
    /// restart. An untrusted session restores nothing (the env gate mirrors
    /// the spawn gate); a later trust + activate persists through the store
    /// and is picked up on the next launch.
    pub fn for_session(project_state_dir: &Path, trusted: bool) -> Self {
        let p = Self::inactive();
        p.set_store(Some(EnvStore::for_project_dir(project_state_dir)), trusted);
        p
    }

    /// Attach (or replace) the recipe store. When `trusted` and the store has
    /// a recorded recipe, it is adopted as the live recipe — this is how a
    /// boot session re-enters its env once the working dir + trust level are
    /// known (mirrors `WorkspaceTrust::set_store` adopting the persisted level).
    pub fn set_store(&self, store: Option<EnvStore>, trusted: bool) {
        if trusted {
            if let Some(store) = store.as_ref() {
                if let Some((snippet, dir)) = store.recipe() {
                    if let Ok(mut s) = self.state.write() {
                        s.snippet = snippet;
                        s.dir = dir;
                        s.cache = None;
                        s.delta_cache = None;
                    }
                }
            }
        }
        if let Ok(mut slot) = self.store.write() {
            *slot = store;
        }
    }

    /// Set the active recipe (activation). Invalidates the cache and, when a
    /// store is attached, persists the recipe for the next launch.
    pub fn set(&self, snippet: String, dir: Option<PathBuf>) {
        if let Ok(mut s) = self.state.write() {
            s.snippet = snippet.clone();
            s.dir = dir.clone();
            s.cache = None;
            s.delta_cache = None;
        }
        if let Ok(store) = self.store.read() {
            if let Some(store) = store.as_ref() {
                if snippet.trim().is_empty() {
                    store.remove();
                } else if let Err(e) = store.record(&snippet, dir.as_deref()) {
                    tracing::warn!("env: failed to persist recipe: {e}");
                }
            }
        }
    }

    /// Deactivate — drop the snippet so no env is applied, and forget the
    /// persisted recipe so the next launch boots clean.
    pub fn clear(&self) {
        if let Ok(mut s) = self.state.write() {
            s.snippet.clear();
            s.cache = None;
            s.delta_cache = None;
        }
        if let Ok(store) = self.store.read() {
            if let Some(store) = store.as_ref() {
                store.remove();
            }
        }
    }

    /// Whether an environment is currently active.
    pub fn is_active(&self) -> bool {
        self.state
            .read()
            .map(|s| !s.snippet.trim().is_empty())
            .unwrap_or(false)
    }

    /// The current activation snippet (for status / inspection).
    pub fn snippet(&self) -> String {
        self.state
            .read()
            .map(|s| s.snippet.clone())
            .unwrap_or_default()
    }

    /// Resolve the active environment, capturing fresh when the env inputs have
    /// changed since the last capture. Returns an empty vec when inactive or
    /// when capture fails (degrade to the inherited env).
    ///
    /// `run` executes the capture *script* on the active host and returns its
    /// stdout (`None` on failure). It MUST be a raw spawn that does not apply
    /// this provider's env, or capture would recurse.
    pub async fn current<F, Fut>(&self, run: F) -> Vec<(String, String)>
    where
        F: FnOnce(String) -> Fut,
        Fut: Future<Output = Option<String>>,
    {
        let (snippet, dir) = match self.state.read() {
            Ok(s) => (s.snippet.clone(), s.dir.clone()),
            Err(_) => return Vec::new(),
        };
        if snippet.trim().is_empty() {
            return Vec::new();
        }

        let hash = inputs_hash(dir.as_deref());
        if let Ok(s) = self.state.read() {
            if let Some(c) = &s.cache {
                if c.inputs_hash == hash {
                    return c.vars.clone();
                }
            }
        }

        let script = build_capture_script(&snippet, dir.as_deref());
        let Some(stdout) = run(script).await else {
            return Vec::new();
        };
        let vars = parse_env(&stdout);

        if let Ok(mut s) = self.state.write() {
            s.cache = Some(Cached {
                inputs_hash: hash,
                vars: vars.clone(),
            });
        }
        vars
    }

    /// Capture the activation **delta** for a *local* backend by running the
    /// recipe through `$SHELL -lc` on this host, **blocking** the caller.
    ///
    /// The integrated-terminal spawn is a synchronous, non-tokio path
    /// (portable-pty + OS threads) and so cannot `await` the async capture; this
    /// is how the terminal applies the same activation everything else gets. The
    /// returned [`EnvDelta`] carries only what the recipe changed over a clean
    /// login shell, so applying it to the child's env is shell-agnostic and free
    /// of volatile shell bookkeeping. Shares a hash-keyed cache, so repeated
    /// terminal opens with unchanged env inputs run no subprocess. Empty when
    /// inactive or on failure — the caller degrades to the inherited env.
    pub fn current_local_delta_blocking(&self) -> EnvDelta {
        self.capture_delta_blocking(|script| {
            let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());
            let output = std::process::Command::new(&shell)
                .arg("-lc")
                .arg(&script)
                .hide_window()
                .output()
                .ok()?;
            Some(String::from_utf8_lossy(&output.stdout).into_owned())
        })
    }

    /// Shared body for a blocking delta capture: serve from the input-hash cache
    /// when warm, otherwise run `run` (the host's raw shell invocation over the
    /// baseline+activated capture script) and cache the diff. `run` MUST be a
    /// raw spawn that does not apply this provider's env, or capture would
    /// recurse.
    fn capture_delta_blocking(&self, run: impl FnOnce(String) -> Option<String>) -> EnvDelta {
        let (snippet, dir) = match self.state.read() {
            Ok(s) => (s.snippet.clone(), s.dir.clone()),
            Err(_) => return EnvDelta::default(),
        };
        if snippet.trim().is_empty() {
            return EnvDelta::default();
        }

        let hash = inputs_hash(dir.as_deref());
        if let Ok(s) = self.state.read() {
            if let Some(c) = &s.delta_cache {
                if c.inputs_hash == hash {
                    return c.delta.clone();
                }
            }
        }

        let script = build_delta_capture_script(&snippet, dir.as_deref());
        let Some(stdout) = run(script) else {
            return EnvDelta::default();
        };
        let delta = parse_delta(&stdout);

        if let Ok(mut s) = self.state.write() {
            s.delta_cache = Some(CachedDelta {
                inputs_hash: hash,
                delta: delta.clone(),
            });
        }
        delta
    }
}

/// Build the shell script the capture runs: `cd <dir>; <snippet>; <env dump>`.
/// The caller's `run` closure wraps this in the host's shell (`$SHELL -lc …`
/// locally, `ssh … sh -lc …` remotely, etc.). The dump is [`ENV_DUMP`]:
/// NUL-separated `KEY=VALUE` records, so a value may hold newlines.
fn build_capture_script(snippet: &str, dir: Option<&Path>) -> String {
    let mut script = String::new();
    if let Some(d) = dir {
        script.push_str("cd ");
        script.push_str(&shell_quote(&d.to_string_lossy()));
        script.push_str("; ");
    }
    let snippet = snippet.trim();
    if !snippet.is_empty() {
        script.push_str(snippet);
        script.push_str("; ");
    }
    script.push_str(ENV_DUMP);
    script
}

/// The marker line [`ENV_DUMP`] prints before the env records. Whatever the
/// login shell or the recipe wrote to stdout comes before it and is dropped,
/// so their output can never be read as (or merged into) a variable.
macro_rules! env_dump_marker {
    () => {
        "__FRESH_ENV_DUMP_5d0b7a__"
    };
}
pub(crate) const ENV_DUMP_MARKER: &str = env_dump_marker!();

/// The env dump every capture runs: the [`ENV_DUMP_MARKER`] line, then the
/// environment. `env -0` ends each `KEY=VALUE` record with NUL instead of a
/// newline, so multi-line values (exported Bash functions, `$'a\nb'` strings)
/// survive intact; an `env` without `-0` falls back to the line-per-record
/// form, which [`parse_env`] also reads. `command` bypasses any `env`
/// function/alias. `pub(crate)` so the SSH remote launcher
/// ([`crate::services::remote::spawner::ssh_remote_env_launcher`]) dumps the
/// same way.
pub(crate) const ENV_DUMP: &str = concat!(
    "printf '\\n%s\\n' ",
    env_dump_marker!(),
    "; command env -0 2>/dev/null || command env"
);

/// Marker printed between the baseline and activated env dumps in the delta
/// capture. A fixed, unlikely token — env values never legitimately contain it,
/// so it cleanly splits the two halves of the output. `pub(crate)` so the SSH
/// remote launcher ([`crate::services::remote::spawner::ssh_remote_env_launcher`])
/// can split on the identical marker when it captures the delta on the remote.
pub(crate) const DELTA_SENTINEL: &str = "__FRESH_ENV_DELTA_SENTINEL_b3f1c2__";

/// Build the script for a *delta* capture: dump the env once (baseline — login
/// shell in the project dir, recipe not yet run), print the sentinel, run the
/// recipe, then dump the env again (activated). Diffing the two halves yields
/// exactly what the recipe changed — see [`parse_delta`]. The `cd` precedes the
/// baseline dump so direnv (which keys off cwd) is still inactive in the
/// baseline and only activates in the second half.
fn build_delta_capture_script(snippet: &str, dir: Option<&Path>) -> String {
    let mut script = String::new();
    if let Some(d) = dir {
        script.push_str("cd ");
        script.push_str(&shell_quote(&d.to_string_lossy()));
        script.push_str("; ");
    }
    script.push_str(ENV_DUMP);
    script.push_str("; printf '%s\\n' ");
    script.push_str(&shell_quote(DELTA_SENTINEL));
    script.push_str("; ");
    let snippet = snippet.trim();
    if !snippet.is_empty() {
        script.push_str(snippet);
        script.push_str("; ");
    }
    script.push_str(ENV_DUMP);
    script
}

/// Diff a delta-capture's output into an [`EnvDelta`]. Splits on
/// [`DELTA_SENTINEL`] into baseline and activated halves; a key whose value is
/// new or changed in the activated half is a `set` (in activated order), and a
/// key present in the baseline but gone from the activated half is an `unset`.
/// Volatile keys identical in both halves (`PWD`, `SHLVL`, …) diff out and never
/// appear. If the sentinel is missing (capture malformed), returns an empty
/// delta rather than guessing.
fn parse_delta(stdout: &str) -> EnvDelta {
    let Some((baseline_text, activated_text)) = stdout.split_once(DELTA_SENTINEL) else {
        return EnvDelta::default();
    };
    let baseline = parse_env(baseline_text);
    let activated = parse_env(activated_text);

    let baseline_lookup: std::collections::HashMap<&str, &str> = baseline
        .iter()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect();

    let mut set = Vec::new();
    for (k, v) in &activated {
        if baseline_lookup.get(k.as_str()) != Some(&v.as_str()) {
            set.push((k.clone(), v.clone()));
        }
    }

    let activated_keys: std::collections::HashSet<&str> =
        activated.iter().map(|(k, _)| k.as_str()).collect();
    let unset = baseline
        .iter()
        .filter(|(k, _)| !activated_keys.contains(k.as_str()))
        .map(|(k, _)| k.clone())
        .collect();

    EnvDelta { set, unset }
}

/// Parse an [`ENV_DUMP`] into pairs. Everything up to the
/// [`ENV_DUMP_MARKER`] line is output that preceded the dump and is dropped
/// (output with no marker is read whole). Records are NUL-separated when the
/// host's `env` has `-0`, so a value keeps its newlines; otherwise they are
/// lines, as before. A value may itself contain `=`; only the first is the
/// separator. Records without `=` or with an empty key are skipped.
fn parse_env(stdout: &str) -> Vec<(String, String)> {
    let dump = match stdout.find(ENV_DUMP_MARKER) {
        Some(at) => {
            let rest = &stdout[at + ENV_DUMP_MARKER.len()..];
            rest.strip_prefix('\n').unwrap_or(rest)
        }
        None => stdout,
    };
    let records: Box<dyn Iterator<Item = &str>> = if dump.contains('\0') {
        Box::new(dump.split('\0'))
    } else {
        Box::new(dump.lines())
    };
    let mut out = Vec::new();
    for record in records {
        if let Some(eq) = record.find('=') {
            if eq == 0 {
                continue;
            }
            out.push((record[..eq].to_string(), record[eq + 1..].to_string()));
        }
    }
    out
}

/// POSIX single-quote escaping for splicing a path into a shell command.
fn shell_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('\'');
    for c in s.chars() {
        if c == '\'' {
            out.push_str("'\\''");
        } else {
            out.push(c);
        }
    }
    out.push('\'');
    out
}

/// Hash the env-input files under `dir` (existence + contents). The capture
/// cache is valid only while this is unchanged. Best-effort: unreadable files
/// hash as absent. `None` dir (or no inputs) yields a stable hash so a
/// snippet-only recipe still caches.
fn inputs_hash(dir: Option<&Path>) -> u64 {
    let mut hasher = DefaultHasher::new();
    if let Some(dir) = dir {
        for name in ENV_INPUT_FILES {
            let path = dir.join(name);
            match std::fs::read(&path) {
                Ok(bytes) => {
                    name.hash(&mut hasher);
                    bytes.hash(&mut hasher);
                }
                Err(_) => {
                    // record absence distinctly from "present and empty"
                    name.hash(&mut hasher);
                    0u8.hash(&mut hasher);
                }
            }
        }
    }
    hasher.finish()
}

/// On-disk record of a project's activated env recipe.
#[derive(serde::Serialize, serde::Deserialize)]
struct StoredEnv {
    snippet: String,
    #[serde(default)]
    dir: Option<PathBuf>,
}

/// Per-project persistence of the *activated env recipe* — the recipe the user
/// last activated for a project, so the next launch can boot directly into it.
///
/// One file per project (`env.json`), alongside `trust.json` in the project's
/// state dir (see `DirectoryContext::project_state_dir`), never inside the
/// repository. Presence of a recipe *is* the "this project's env is activated"
/// decision; restoring it is gated on trust exactly like a spawn.
#[derive(Debug, Clone)]
pub struct EnvStore {
    path: PathBuf,
}

impl EnvStore {
    /// Recipe file for the project whose state lives in `project_state_dir`.
    pub fn for_project_dir(project_state_dir: &Path) -> Self {
        Self {
            path: project_state_dir.join("env.json"),
        }
    }

    /// The recorded recipe (`snippet`, `dir`), or `None` when absent, empty, or
    /// corrupt (a corrupt file reads as "no recipe"; the next write rewrites it).
    fn recipe(&self) -> Option<(String, Option<PathBuf>)> {
        let text = std::fs::read_to_string(&self.path).ok()?;
        let stored: StoredEnv = serde_json::from_str(&text).ok()?;
        if stored.snippet.trim().is_empty() {
            return None;
        }
        Some((stored.snippet, stored.dir))
    }

    /// Record the activated recipe, written atomically (pid-tagged temp then
    /// rename) so a half-written file is never observed.
    fn record(&self, snippet: &str, dir: Option<&Path>) -> io::Result<()> {
        if let Some(parent) = self.path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let json = serde_json::to_string_pretty(&StoredEnv {
            snippet: snippet.to_string(),
            dir: dir.map(Path::to_path_buf),
        })
        .map_err(io::Error::other)?;
        let tmp = self
            .path
            .with_extension(format!("json.{}.tmp", std::process::id()));
        std::fs::write(&tmp, json.as_bytes())?;
        std::fs::rename(&tmp, &self.path)?;
        Ok(())
    }

    /// Forget the recipe (deactivation). A missing file is success; any other
    /// error is logged best-effort (the result is `#[must_use]`, so it is
    /// handled rather than discarded — the crate denies `let_underscore_must_use`).
    fn remove(&self) {
        if let Err(e) = std::fs::remove_file(&self.path) {
            if e.kind() != std::io::ErrorKind::NotFound {
                tracing::warn!("env: failed to remove recipe: {e}");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inactive_by_default_and_after_clear() {
        let p = EnvProvider::inactive();
        assert!(!p.is_active());
        p.set(
            "source .venv/bin/activate".into(),
            Some(PathBuf::from("/proj")),
        );
        assert!(p.is_active());
        assert_eq!(p.snippet(), "source .venv/bin/activate");
        p.clear();
        assert!(!p.is_active());
    }

    #[test]
    fn whitespace_snippet_is_inactive() {
        let p = EnvProvider::inactive();
        p.set("   \n  ".into(), None);
        assert!(!p.is_active());
    }

    #[test]
    fn build_capture_script_shapes() {
        assert_eq!(
            build_capture_script("source .venv/bin/activate", Some(Path::new("/a b"))),
            format!("cd '/a b'; source .venv/bin/activate; {ENV_DUMP}")
        );
        assert_eq!(build_capture_script("", None), ENV_DUMP);
        assert_eq!(
            build_capture_script(r#"eval "$(direnv export bash)""#, None),
            format!(r#"eval "$(direnv export bash)"; {ENV_DUMP}"#)
        );
    }

    #[test]
    fn build_delta_capture_script_shape() {
        let s = build_delta_capture_script("source .venv/bin/activate", Some(Path::new("/p")));
        assert_eq!(
            s,
            format!(
                "cd '/p'; {ENV_DUMP}; printf '%s\\n' '{DELTA_SENTINEL}'; \
                 source .venv/bin/activate; {ENV_DUMP}"
            )
        );
    }

    #[test]
    fn parse_env_basics() {
        let out = "PATH=/a:/b\nVIRTUAL_ENV=/p/.venv\nWEIRD=a=b=c\n=skipme\nnoeq\n";
        let vars = parse_env(out);
        assert_eq!(
            vars,
            vec![
                ("PATH".to_string(), "/a:/b".to_string()),
                ("VIRTUAL_ENV".to_string(), "/p/.venv".to_string()),
                ("WEIRD".to_string(), "a=b=c".to_string()),
            ]
        );
    }

    /// Issue #3586: with `env -0` records, a value keeps its newlines (an
    /// exported Bash function's body) and a continuation line is never read
    /// as a variable of its own.
    #[test]
    fn parse_env_nul_records_keep_multiline_values() {
        let out = "BASH_FUNC_f%%=() {  if true; then\n echo hi\n fi\n}\0\
                   MULTI=line1\nINJECTED=evil\0PATH=/a:/b\0";
        assert_eq!(
            parse_env(out),
            vec![
                (
                    "BASH_FUNC_f%%".to_string(),
                    "() {  if true; then\n echo hi\n fi\n}".to_string()
                ),
                ("MULTI".to_string(), "line1\nINJECTED=evil".to_string()),
                ("PATH".to_string(), "/a:/b".to_string()),
            ]
        );
    }

    /// What the login shell or the recipe printed before the dump — even a
    /// line holding `=` — is dropped at the marker; it is neither read as a
    /// variable nor merged into the first real one.
    #[test]
    fn parse_env_drops_output_before_the_marker() {
        let out = format!("Using python=3.11\nactivating...\n{ENV_DUMP_MARKER}\nPATH=/v\0BAR=2\0");
        assert_eq!(
            parse_env(&out),
            vec![("PATH".into(), "/v".into()), ("BAR".into(), "2".into())]
        );
        // Without `env -0` the records are lines, after the marker all the same.
        let out = format!("Using python=3.11\n{ENV_DUMP_MARKER}\nPATH=/v\nBAR=2\n");
        assert_eq!(
            parse_env(&out),
            vec![("PATH".into(), "/v".into()), ("BAR".into(), "2".into())]
        );
    }

    #[test]
    fn parse_delta_with_nul_records() {
        let out = format!(
            "\n{ENV_DUMP_MARKER}\nPATH=/a\0KEEP=1\0GONE=x\0{DELTA_SENTINEL}\n\
             activating, python=3.11\n{ENV_DUMP_MARKER}\nPATH=/v/bin:/a\0KEEP=1\0F=a\nb=c\0"
        );
        let d = parse_delta(&out);
        assert_eq!(
            d.set,
            vec![
                ("PATH".to_string(), "/v/bin:/a".to_string()),
                ("F".to_string(), "a\nb=c".to_string()),
            ]
        );
        assert_eq!(d.unset, vec!["GONE".to_string()]);
    }

    /// End to end through a real shell: an exported Bash function captured
    /// by the script comes back whole, and a child Bash given it runs it.
    #[cfg(unix)]
    #[test]
    fn exported_bash_function_survives_capture() {
        if std::process::Command::new("bash")
            .arg("--version")
            .output()
            .is_err()
        {
            return;
        }
        // The recipe also prints to stdout, as chatty ones do, `=` and all.
        let snippet = "echo 'Using python=3.11'; \
                       demo_function() { if true; then echo hello; fi; }; \
                       export -f demo_function; export MULTI=$'line1\\nINJECTED=evil'";
        let script = build_capture_script(snippet, None);
        let out = std::process::Command::new("bash")
            .arg("-c")
            .arg(&script)
            .output()
            .unwrap();
        let vars = parse_env(&String::from_utf8_lossy(&out.stdout));
        assert!(vars
            .iter()
            .all(|(k, _)| k != "INJECTED" && !k.contains("python")));
        assert!(vars.contains(&("MULTI".to_string(), "line1\nINJECTED=evil".to_string())));

        let child = std::process::Command::new("bash")
            .arg("-c")
            .arg("demo_function")
            .env_clear()
            .envs(vars)
            .output()
            .unwrap();
        assert_eq!(String::from_utf8_lossy(&child.stdout), "hello\n");
    }

    #[tokio::test]
    async fn current_inactive_returns_empty_without_running() {
        let p = EnvProvider::inactive();
        let ran = std::cell::Cell::new(false);
        let vars = p
            .current(|_script| {
                ran.set(true);
                async { Some("X=1".to_string()) }
            })
            .await;
        assert!(vars.is_empty());
        assert!(!ran.get(), "capture must not run when inactive");
    }

    #[tokio::test]
    async fn current_captures_and_caches() {
        let tmp = tempfile::tempdir().unwrap();
        let p = EnvProvider::inactive();
        p.set("true".into(), Some(tmp.path().to_path_buf()));

        let calls = std::cell::Cell::new(0);
        let run = || {
            calls.set(calls.get() + 1);
            async { Some("FOO=bar\nPATH=/x\n".to_string()) }
        };

        let v1 = p.current(|_s| run()).await;
        assert_eq!(
            v1,
            vec![("FOO".into(), "bar".into()), ("PATH".into(), "/x".into())]
        );
        // Second call with unchanged inputs hits the cache — no re-run.
        let v2 = p.current(|_s| run()).await;
        assert_eq!(v2, v1);
        assert_eq!(calls.get(), 1, "cache should prevent a second capture");
    }

    #[tokio::test]
    async fn cache_invalidated_when_inputs_change() {
        let tmp = tempfile::tempdir().unwrap();
        let p = EnvProvider::inactive();
        p.set("true".into(), Some(tmp.path().to_path_buf()));

        let n = std::cell::Cell::new(0);
        let v1 = p
            .current(|_s| {
                n.set(n.get() + 1);
                async move { Some("A=1".to_string()) }
            })
            .await;
        assert_eq!(v1, vec![("A".into(), "1".into())]);

        // Change an env input → cache must miss and re-capture.
        std::fs::write(tmp.path().join(".envrc"), "export A=2\n").unwrap();
        let v2 = p
            .current(|_s| {
                n.set(n.get() + 1);
                async move { Some("A=2".to_string()) }
            })
            .await;
        assert_eq!(v2, vec![("A".into(), "2".into())]);
        assert_eq!(n.get(), 2, "input change should force a re-capture");
    }

    /// Build a delta-capture stdout from a baseline and an activated env block.
    fn delta_output(baseline: &str, activated: &str) -> String {
        format!("{baseline}\n{DELTA_SENTINEL}\n{activated}")
    }

    #[test]
    fn parse_delta_extracts_added_changed_and_removed() {
        // baseline → activated: PATH changed, VIRTUAL_ENV added, GONE removed,
        // HOME unchanged (must diff out), SHLVL unchanged (volatile diffs out).
        let out = delta_output(
            "HOME=/h\nPATH=/usr/bin\nGONE=1\nSHLVL=3",
            "HOME=/h\nPATH=/p/.venv/bin:/usr/bin\nVIRTUAL_ENV=/p/.venv\nSHLVL=3",
        );
        let d = parse_delta(&out);
        assert_eq!(
            d.set,
            vec![
                ("PATH".into(), "/p/.venv/bin:/usr/bin".into()),
                ("VIRTUAL_ENV".into(), "/p/.venv".into()),
            ]
        );
        assert_eq!(d.unset, vec!["GONE".to_string()]);
    }

    #[test]
    fn parse_delta_missing_sentinel_is_empty() {
        assert!(parse_delta("HOME=/h\nPATH=/usr/bin").is_empty());
    }

    #[test]
    fn delta_capture_inactive_returns_empty_without_running() {
        let p = EnvProvider::inactive();
        let ran = std::cell::Cell::new(false);
        let d = p.capture_delta_blocking(|_script| {
            ran.set(true);
            Some(delta_output("A=1", "A=1\nB=2"))
        });
        assert!(d.is_empty());
        assert!(!ran.get(), "delta capture must not run when inactive");
    }

    #[test]
    fn delta_capture_caches_on_unchanged_inputs() {
        let tmp = tempfile::tempdir().unwrap();
        let p = EnvProvider::inactive();
        p.set("true".into(), Some(tmp.path().to_path_buf()));

        let calls = std::cell::Cell::new(0);
        let run = || {
            calls.set(calls.get() + 1);
            Some(delta_output("PATH=/usr/bin", "PATH=/usr/bin\nFOO=bar"))
        };

        let d1 = p.capture_delta_blocking(|_s| run());
        assert_eq!(d1.set, vec![("FOO".into(), "bar".into())]);
        // Second call hits the cache — no re-run.
        let d2 = p.capture_delta_blocking(|_s| run());
        assert_eq!(d2, d1);
        assert_eq!(calls.get(), 1, "warm cache should prevent a second capture");
    }

    #[tokio::test]
    async fn capture_failure_degrades_to_empty() {
        let p = EnvProvider::inactive();
        p.set("true".into(), None);
        let vars = p.current(|_s| async { None }).await;
        assert!(vars.is_empty());
    }

    #[test]
    fn for_session_restores_a_persisted_recipe_when_trusted() {
        let tmp = tempfile::tempdir().unwrap();
        // First session: trusted, user activates → recipe persists.
        let first = EnvProvider::for_session(tmp.path(), true);
        first.set(
            "eval \"$(direnv export bash)\"".into(),
            Some(PathBuf::from("/proj")),
        );
        assert!(first.is_active());

        // Next launch: a fresh trusted session boots already in the env, with
        // no plugin re-activation needed — this is what removes the flicker.
        let next = EnvProvider::for_session(tmp.path(), true);
        assert!(next.is_active());
        assert_eq!(next.snippet(), "eval \"$(direnv export bash)\"");
    }

    #[test]
    fn for_session_does_not_restore_when_untrusted() {
        let tmp = tempfile::tempdir().unwrap();
        EnvProvider::for_session(tmp.path(), true).set("true".into(), None);
        // An untrusted session must not silently re-enter a persisted env —
        // the env gate mirrors the spawn gate.
        let untrusted = EnvProvider::for_session(tmp.path(), false);
        assert!(!untrusted.is_active());
    }

    #[test]
    fn clear_forgets_the_persisted_recipe() {
        let tmp = tempfile::tempdir().unwrap();
        let p = EnvProvider::for_session(tmp.path(), true);
        p.set("true".into(), None);
        p.clear();
        // After deactivation the next launch boots clean.
        let next = EnvProvider::for_session(tmp.path(), true);
        assert!(!next.is_active());
    }

    #[test]
    fn inactive_provider_never_persists() {
        let tmp = tempfile::tempdir().unwrap();
        // A storeless provider applies env in-memory but writes nothing.
        let p = EnvProvider::inactive();
        p.set("true".into(), None);
        assert!(EnvStore::for_project_dir(tmp.path()).recipe().is_none());
    }
}
