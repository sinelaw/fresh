//! Capability-token authorization for the agent script channel.
//!
//! When the Orchestrator launcher creates a workspace it asks the host to mint
//! a random, unforgeable token bound to `{ window, may_script }`, and stamps it
//! into the spawned agent's environment as `FRESH_CMD_TOKEN`. A client that
//! later sends `RunScript` over the control socket presents that token in its
//! `Hello`; the server resolves it here to decide (a) which window the script
//! targets and (b) whether this agent may run scripts at all.
//!
//! ## Why one flag rather than a list of verbs
//!
//! An earlier design granted a *set of command ids* and dispatched them by
//! name. Scripts subsume that: a script can call anything the plugin API
//! exposes, including `editor.executeAction`, so a per-verb allowlist alongside
//! it would describe nothing real. The grant is therefore binary — this agent
//! either drives the editor or it doesn't — and the honest reading of holding
//! it is "may do anything the user could do from a plugin".
//!
//! This is deliberately **decoupled from Workspace Trust** — trust answers "is
//! this repo safe to load", script access answers "may this agent drive the
//! editor". The table is process-global and in-memory: tokens are registered at
//! workspace creation, revoked at teardown, and never persisted.
//!
//! Threat model: same-user, unix-socket-local. The token protects against a
//! process that was *not* granted access guessing a valid one (128 random bits
//! from a v4 UUID); it is not a secret from the user themselves.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use crate::app::Editor;

/// What a capability token is permitted to do.
#[derive(Debug, Clone, Default)]
pub struct Grant {
    /// The window/workspace this token drives. A script is evaluated with this
    /// window made active (derived from the token, never supplied by the
    /// client), so a token starts out pointed at its own workspace. `None` =
    /// not pinned to a window (falls back to the active one).
    pub window_id: Option<u64>,
    /// Whether this token may evaluate scripts. `false` — the default — refuses
    /// `RunScript` outright, which is what a token minted for a workspace whose
    /// agent wasn't granted editor control gets.
    pub may_script: bool,
}

impl Grant {
    pub fn new(window_id: Option<u64>, may_script: bool) -> Self {
        Self {
            window_id,
            may_script,
        }
    }
}

fn table() -> &'static Mutex<HashMap<String, Grant>> {
    static TABLE: OnceLock<Mutex<HashMap<String, Grant>>> = OnceLock::new();
    TABLE.get_or_init(|| Mutex::new(HashMap::new()))
}

// ===========================================================================
// In-flight script results
//
// A script runs on the plugin thread and is async by nature — it awaits host
// calls that only complete on later editor ticks — so its answer arrives long
// after it was submitted. Blocking the editor thread on it would deadlock,
// since servicing those host calls is the editor thread's own job.
//
// So a submission is *pending*: the host records which caller is waiting, the
// script reports its outcome when it settles, and the host answers then. This
// registry is the meeting point. It is process-global for the same reason the
// token table is — one editor process, several possible front doors (the
// daemon's IPC clients and the in-process control socket).
// ===========================================================================

/// The outcome of a script, once it has settled.
#[derive(Debug, Clone)]
pub struct CommandOutcome {
    /// The request id handed out by [`run_script`].
    pub request_id: u64,
    pub ok: bool,
    /// What the script returned, JSON-encoded (`None` when it returned
    /// nothing). This is what `fresh --cmd script run` prints.
    pub output: Option<String>,
    pub error: Option<String>,
}

/// A settled outcome plus the moment it stops being worth keeping, so one
/// nobody ever claims can be reaped instead of sitting in the queue for the
/// life of the process.
///
/// A deadline rather than an arrival time on purpose: deriving "long ago" from
/// the current instant means subtracting from it, which panics on a machine
/// whose monotonic clock is younger than the interval — a freshly booted CI
/// runner, most visibly. Adding is always representable.
struct QueuedOutcome {
    outcome: CommandOutcome,
    expires_at: std::time::Instant,
}

/// How long an unclaimed outcome is kept. Far longer than any caller waits (the
/// CLI gives up in seconds), so this only ever collects outcomes whose caller
/// disappeared — a disconnected client, a host that shut down mid-command.
const OUTCOME_RETENTION: std::time::Duration = std::time::Duration::from_secs(600);

fn completions() -> &'static Mutex<Vec<QueuedOutcome>> {
    static COMPLETED: OnceLock<Mutex<Vec<QueuedOutcome>>> = OnceLock::new();
    COMPLETED.get_or_init(|| Mutex::new(Vec::new()))
}

/// Allocate the id that ties a running script to the caller waiting on it.
/// Process-wide and monotonic; ids are never reused within a run.
fn next_request_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// Record a settled script. Called from the editor thread when the plugin
/// runtime reports the script's return value (or its failure).
pub fn complete(request_id: u64, ok: bool, output: Option<String>, error: Option<String>) {
    if let Ok(mut done) = completions().lock() {
        done.push(QueuedOutcome {
            outcome: CommandOutcome {
                request_id,
                ok,
                output,
                error,
            },
            expires_at: std::time::Instant::now() + OUTCOME_RETENTION,
        });
    }
}

/// Drain the settled outcomes this host is waiting on, leaving the rest queued.
///
/// Ownership-scoped on purpose: one editor process can run *two* front doors at
/// once — the daemon's IPC clients and the in-process control socket — and each
/// holds its own map of who is waiting for which request id. An unconditional
/// drain lets whichever host ticks first swallow an outcome belonging to the
/// other, which then waits forever for a reply that was already thrown away.
/// So each host takes only the ids it owns (`owned`), and an outcome nobody
/// claims stays queued until [`discard_completed_before`] reaps it.
pub fn take_completed_where(mut owned: impl FnMut(u64) -> bool) -> Vec<CommandOutcome> {
    let Ok(mut done) = completions().lock() else {
        return Vec::new();
    };
    // Reap on the way past: an outcome whose caller vanished is claimed by
    // nobody, and this scan is the only regular visitor to the queue.
    let now = std::time::Instant::now();
    let mut taken = Vec::new();
    let mut i = 0;
    while i < done.len() {
        if owned(done[i].outcome.request_id) {
            taken.push(done.remove(i).outcome);
        } else if now >= done[i].expires_at {
            done.remove(i);
        } else {
            i += 1;
        }
    }
    taken
}

/// What [`run_script`] decided about a submission.
#[derive(Debug, Clone)]
pub enum CommandDispatch {
    /// Nothing is running — the submission was refused, or failed before it
    /// reached the runtime. Answer the caller now.
    Settled {
        ok: bool,
        error: Option<String>,
        output: Option<String>,
    },
    /// The script was started and will settle later. Hold the caller until an
    /// outcome with this id comes out of [`take_completed_where`].
    Pending { request_id: u64 },
}

impl CommandDispatch {
    /// A refusal that never reached a handler.
    fn refused(error: impl Into<String>) -> Self {
        Self::Settled {
            ok: false,
            error: Some(error.into()),
            output: None,
        }
    }
}

/// Mint a fresh, unforgeable token string (a v4 UUID — 122 random bits). Not
/// registered; the caller registers it with its grant via [`register`].
pub fn new_token() -> String {
    uuid::Uuid::new_v4().to_string()
}

/// Mint a token and register it with `grant` in one step; returns the token.
pub fn mint(grant: Grant) -> String {
    let token = new_token();
    register(token.clone(), grant);
    token
}

/// Register a token with its grant (overwrites any prior grant for the token).
pub fn register(token: String, grant: Grant) {
    if let Ok(mut t) = table().lock() {
        t.insert(token, grant);
    }
}

/// Look up a token's grant, if any.
pub fn lookup(token: &str) -> Option<Grant> {
    table().lock().ok().and_then(|t| t.get(token).cloned())
}

/// Revoke a token (called on workspace teardown). No-op if unknown.
pub fn revoke(token: &str) {
    if let Ok(mut t) = table().lock() {
        t.remove(token);
    }
}

/// Whether `token` is known and may evaluate scripts.
pub fn may_script(token: &str) -> bool {
    lookup(token).map(|g| g.may_script).unwrap_or(false)
}

/// The JS that turns a submitted script into an *answerable* one.
///
/// The source becomes the body of an async function, so `await` works at its
/// top level and `return value` is how it answers — then the settled value (or
/// the throw) is reported under `request_id` through the same
/// `editor.completeCommand` path a plugin command's return value takes. Both
/// halves are deliberately plain source rather than host code: the runtime
/// already knows how to evaluate a plugin from a string and how to route a
/// completion, so a script needs no new runtime machinery at all.
///
/// `FRESH_WINDOW_ID` is the window the token is bound to. A script that outlives
/// its first `await` cannot trust the *active* window to still be its own (the
/// user keeps working while it runs), so anything window-scoped should be
/// addressed through this id — `editor.windowPath(FRESH_WINDOW_ID, path)` and
/// friends — rather than through whatever happens to be focused.
fn wrap_script(source: &str, request_id: u64, window_id: u64) -> String {
    format!(
        r#"
const FRESH_WINDOW_ID = {window_id};
(async () => {{
    var __rid = {request_id};
    var __done = function (ok, value, err) {{
        var out = null;
        if (ok && value !== undefined && value !== null) {{
            try {{ out = JSON.stringify(value); }} catch (e) {{ out = String(value); }}
        }}
        editor.completeCommand(__rid, ok, out, err);
    }};
    try {{
        const __result = await (async () => {{
{source}
        }})();
        __done(true, __result, null);
    }} catch (e) {{
        __done(false, null, (e && e.message) ? String(e.message) : String(e));
    }}
}})();
"#
    )
}

/// Authorize and evaluate an agent-submitted script against `editor`.
///
/// Returns a [`CommandDispatch`]: `Settled` only for a refusal or a script that
/// never reached the runtime, `Pending` for one that is now running and will
/// answer under the returned request id. Refused when there is no token, the
/// token is unknown/expired, or the grant doesn't include script access.
///
/// The script is evaluated as an ephemeral plugin — its own QuickJS context,
/// named for the request so two concurrent scripts can't collide or read each
/// other's globals. It is deliberately never unloaded: unloading runs the
/// plugin-cleanup path, which sends compensating commands that would tear down
/// the very things a script is usually run to create (a terminal, a workspace).
/// Run-and-forget is what makes the created state outlive the caller.
pub fn run_script(
    editor: Option<&mut Editor>,
    token: Option<&str>,
    source: &str,
) -> CommandDispatch {
    let Some(token) = token else {
        return CommandDispatch::refused(
            "no capability token: script evaluation is not authorized",
        );
    };
    let Some(grant) = lookup(token) else {
        return CommandDispatch::refused("unknown or expired capability token");
    };
    if !grant.may_script {
        return CommandDispatch::refused("this workspace's agent was not granted editor control");
    }
    let Some(editor) = editor else {
        return CommandDispatch::refused("editor unavailable");
    };

    // Point the editor at the token's own window before the script's first
    // statement. `set_active_window` is a no-op for an unknown id, so an
    // already-torn-down window falls back to the currently active one.
    if let Some(wid) = grant.window_id {
        editor.set_active_window(fresh_core::WindowId(wid));
    }
    let window_id = grant
        .window_id
        .unwrap_or_else(|| editor.active_window_id().0);

    let request_id = next_request_id();
    let wrapped = wrap_script(source, request_id, window_id);
    match editor.eval_agent_script(&wrapped, request_id) {
        Ok(()) => CommandDispatch::Pending { request_id },
        Err(e) => CommandDispatch::refused(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The wrapper is what makes a submitted script answerable, so its shape is
    /// a contract: the body must be reachable by `await`/`return`, and both the
    /// success and failure paths must call `completeCommand` with the request's
    /// own id. A script that can't answer leaves its caller waiting for the
    /// full timeout.
    #[test]
    fn wrapper_makes_the_script_answerable() {
        let js = wrap_script("return 41 + 1;", 99, 7);
        assert!(js.contains("return 41 + 1;"), "the body must be embedded");
        assert!(
            js.contains("var __rid = 99;"),
            "the wrapper must carry the request id: {js}"
        );
        assert!(
            js.contains("const FRESH_WINDOW_ID = 7;"),
            "a script must be able to address its own window: {js}"
        );
        // Both arms answer: a throw has to reach the caller as a failure
        // rather than as a timeout.
        assert!(js.contains("__done(true, __result, null)"));
        assert!(js.contains("__done(false, null,"));
        assert!(
            js.contains("await (async () =>"),
            "the body must be an async context so it can await: {js}"
        );
    }

    /// A script is submitted verbatim — quotes, newlines, backslashes and all.
    /// The wrapper is string concatenation, not a format string applied to the
    /// body, so nothing in the source can be mangled on the way in.
    #[test]
    fn wrapper_does_not_mangle_the_source() {
        let source = "const s = \"a\\nb\";\nreturn { s, t: `x${s}y` };";
        let js = wrap_script(source, 1, 1);
        assert!(js.contains(source), "source must survive intact: {js}");
    }

    #[test]
    fn mint_register_lookup_revoke() {
        let token = mint(Grant::new(Some(7), true));
        let grant = lookup(&token).expect("registered token resolves");
        assert_eq!(grant.window_id, Some(7));
        assert!(grant.may_script);
        assert!(may_script(&token));

        revoke(&token);
        assert!(lookup(&token).is_none());
        assert!(!may_script(&token));
    }

    /// A token minted without the grant is a real, resolvable token that still
    /// cannot run anything — the workspace gets addressing without authority.
    #[test]
    fn a_token_without_the_grant_denies() {
        let token = mint(Grant::new(Some(3), false));
        assert!(lookup(&token).is_some(), "the token itself is valid");
        assert!(!may_script(&token));
        revoke(&token);
    }

    #[test]
    fn unknown_token_denies() {
        assert!(!may_script("not-a-real-token"));
        assert!(lookup("not-a-real-token").is_none());
    }

    #[test]
    fn tokens_are_distinct() {
        assert_ne!(new_token(), new_token());
    }

    /// A refusal is settled immediately — nothing was submitted, so nothing
    /// can arrive later. Returns the reason.
    fn refusal_reason(dispatch: CommandDispatch) -> String {
        match dispatch {
            CommandDispatch::Settled { ok, error, .. } => {
                assert!(!ok, "expected a refusal");
                error.expect("a refusal carries a reason")
            }
            CommandDispatch::Pending { .. } => panic!("a refusal must not be dispatched"),
        }
    }

    #[test]
    fn run_script_without_token_is_refused() {
        let reason = refusal_reason(run_script(None, None, "return 1;"));
        assert!(reason.contains("not authorized"), "{}", reason);
    }

    #[test]
    fn run_script_unknown_token_is_refused() {
        let reason = refusal_reason(run_script(None, Some("not-a-real-token"), "return 1;"));
        assert!(reason.contains("unknown or expired"), "{}", reason);
    }

    /// A token that resolves but wasn't granted script access is refused before
    /// the editor is ever touched (so a `None` editor is fine here — the grant
    /// check returns first). This is the check that keeps the capability
    /// opt-in: minting is unconditional, authority is not.
    #[test]
    fn run_script_without_the_grant_is_refused() {
        let token = mint(Grant::new(Some(1), false));
        let reason = refusal_reason(run_script(None, Some(&token), "return 1;"));
        assert!(reason.contains("not granted editor control"), "{}", reason);
        revoke(&token);
    }

    /// An outcome reported by the plugin runtime is picked up exactly once by
    /// the host that owns the request — a second drain must not re-deliver it
    /// (which would write a stray reply to an unrelated client).
    #[test]
    fn completed_outcomes_drain_once() {
        // Ids are process-global; use one far from any other test's traffic.
        let request_id = u64::MAX - 7;
        complete(
            request_id,
            true,
            Some("{\"workspaceId\":\"ws-1\"}".to_string()),
            None,
        );
        let drained = take_completed_where(|id| id == request_id);
        assert_eq!(drained.len(), 1);
        assert!(drained[0].ok);
        assert_eq!(
            drained[0].output.as_deref(),
            Some("{\"workspaceId\":\"ws-1\"}")
        );
        assert!(
            take_completed_where(|id| id == request_id).is_empty(),
            "an outcome must be delivered once"
        );
    }

    /// Two hosts share this queue (the daemon's IPC clients and the in-process
    /// control socket), each owning different request ids. A drain must take
    /// only what the draining host owns and leave the rest — the bug this
    /// scoping fixes was one host swallowing the other's outcome, leaving that
    /// caller waiting for a reply that had already been discarded.
    #[test]
    fn a_drain_leaves_outcomes_owned_by_another_host() {
        let mine = u64::MAX - 11;
        let theirs = u64::MAX - 12;
        complete(mine, true, None, None);
        complete(theirs, true, None, None);

        let drained = take_completed_where(|id| id == mine);
        assert_eq!(drained.len(), 1);
        assert_eq!(drained[0].request_id, mine);

        let other = take_completed_where(|id| id == theirs);
        assert_eq!(
            other.len(),
            1,
            "the other host's outcome must still be waiting for it"
        );
    }

    /// An outcome older than the retention window is reaped by the next scan,
    /// so a caller that vanished cannot grow the queue for the life of the
    /// process. (Ages the entry directly — the alternative is a 10-minute test.)
    #[test]
    fn unclaimed_outcomes_are_reaped() {
        let orphan = u64::MAX - 21;
        complete(orphan, true, None, None);
        {
            // Expire it as of now; the reaping scan samples the clock after
            // this, so it always sees the deadline as passed.
            let mut done = completions().lock().unwrap();
            for q in done.iter_mut() {
                if q.outcome.request_id == orphan {
                    q.expires_at = std::time::Instant::now();
                }
            }
        }
        // A scan that claims nothing still reaps.
        let _ = take_completed_where(|_| false);
        assert!(
            take_completed_where(|id| id == orphan).is_empty(),
            "an unclaimed outcome must not linger"
        );
    }
}
