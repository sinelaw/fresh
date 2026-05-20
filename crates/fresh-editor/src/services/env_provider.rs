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

use std::collections::hash_map::DefaultHasher;
use std::future::Future;
use std::hash::{Hash, Hasher};
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
    /// Last capture, keyed by the env-inputs hash it was produced under.
    cache: Option<Cached>,
}

struct Cached {
    inputs_hash: u64,
    vars: Vec<(String, String)>,
}

/// Shared, live environment recipe.
pub struct EnvProvider {
    state: RwLock<State>,
}

impl EnvProvider {
    /// An inactive provider — no snippet, applies no env.
    pub fn inactive() -> Self {
        Self {
            state: RwLock::new(State {
                snippet: String::new(),
                dir: None,
                cache: None,
            }),
        }
    }

    /// Set the active recipe (activation). Invalidates the cache.
    pub fn set(&self, snippet: String, dir: Option<PathBuf>) {
        if let Ok(mut s) = self.state.write() {
            s.snippet = snippet;
            s.dir = dir;
            s.cache = None;
        }
    }

    /// Deactivate — drop the snippet so no env is applied.
    pub fn clear(&self) {
        if let Ok(mut s) = self.state.write() {
            s.snippet.clear();
            s.cache = None;
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
        self.state.read().map(|s| s.snippet.clone()).unwrap_or_default()
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
}

/// Build the shell script the capture runs: `cd <dir>; <snippet>; command env`.
/// The caller's `run` closure wraps this in the host's shell (`$SHELL -lc …`
/// locally, `ssh … sh -lc …` remotely, etc.). `command env` prints the
/// resulting environment, one `KEY=VALUE` per line.
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
    // `command env` bypasses any `env` function/alias.
    script.push_str("command env");
    script
}

/// Parse `env` output (`KEY=VALUE` lines) into pairs. Lines without `=` or with
/// an empty key are skipped. A value may itself contain `=`; only the first is
/// the separator. (Values with embedded newlines — rare — are not handled.)
fn parse_env(stdout: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in stdout.lines() {
        if let Some(eq) = line.find('=') {
            if eq == 0 {
                continue;
            }
            out.push((line[..eq].to_string(), line[eq + 1..].to_string()));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inactive_by_default_and_after_clear() {
        let p = EnvProvider::inactive();
        assert!(!p.is_active());
        p.set("source .venv/bin/activate".into(), Some(PathBuf::from("/proj")));
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
            "cd '/a b'; source .venv/bin/activate; command env"
        );
        assert_eq!(build_capture_script("", None), "command env");
        assert_eq!(
            build_capture_script(r#"eval "$(direnv export bash)""#, None),
            r#"eval "$(direnv export bash)"; command env"#
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
        assert_eq!(v1, vec![("FOO".into(), "bar".into()), ("PATH".into(), "/x".into())]);
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

    #[tokio::test]
    async fn capture_failure_degrades_to_empty() {
        let p = EnvProvider::inactive();
        p.set("true".into(), None);
        let vars = p.current(|_s| async { None }).await;
        assert!(vars.is_empty());
    }
}
