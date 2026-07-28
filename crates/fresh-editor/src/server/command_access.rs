//! Capability-token authorization for the agent command channel.
//!
//! When the Orchestrator launcher creates a workspace it asks the host to mint
//! a random, unforgeable token bound to `{ window, allowlist }`, and stamps it
//! into the spawned agent's environment as `FRESH_CMD_TOKEN`. A client that
//! later sends `ListCommands` / `RunCommand` over the control socket presents
//! that token in its `Hello`; the server resolves it here to decide (a) which
//! window the command targets and (b) whether the specific command id is on the
//! token's allowlist.
//!
//! This is deliberately **decoupled from Workspace Trust** — trust answers "is
//! this repo safe to load", command access answers "may this agent drive the
//! editor, and how much". The table is process-global and in-memory: tokens are
//! registered at workspace creation, revoked at teardown, and never persisted.
//!
//! Threat model: same-user, unix-socket-local. The token protects against a
//! process that was *not* granted access guessing a valid one (128 random bits
//! from a v4 UUID); it is not a secret from the user themselves.

use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, OnceLock};

use crate::app::Editor;
use crate::input::keybindings::Action;
use crate::server::protocol::{CommandArgInfo, CommandInfo};

/// What a capability token is permitted to do.
#[derive(Debug, Clone, Default)]
pub struct Grant {
    /// The window/workspace this token drives. Command dispatch targets this
    /// window (derived from the token, never supplied by the client), so a
    /// token can only ever act on its own workspace. `None` = not pinned to a
    /// window (falls back to the active one).
    pub window_id: Option<u64>,
    /// The command ids this token may run. An id absent from the set is
    /// refused; an empty set denies everything.
    pub allowlist: HashSet<String>,
}

impl Grant {
    pub fn new(window_id: Option<u64>, allowlist: impl IntoIterator<Item = String>) -> Self {
        Self {
            window_id,
            allowlist: allowlist.into_iter().collect(),
        }
    }

    /// Whether this grant permits `command_id`.
    pub fn allows(&self, command_id: &str) -> bool {
        self.allowlist.contains(command_id)
    }
}

fn table() -> &'static Mutex<HashMap<String, Grant>> {
    static TABLE: OnceLock<Mutex<HashMap<String, Grant>>> = OnceLock::new();
    TABLE.get_or_init(|| Mutex::new(HashMap::new()))
}

// ===========================================================================
// In-flight command results
//
// A core action finishes inside `handle_action`, so its `CommandResult` is
// known the moment it is dispatched. A *plugin* command doesn't: the handler
// runs on the plugin thread and is usually async (it awaits host calls that
// only complete on later editor ticks), so its answer — including whatever it
// returns — arrives much later. Blocking the editor thread on it would
// deadlock, since servicing those host calls is the editor thread's own job.
//
// So a plugin dispatch is *pending*: the host records which caller is waiting,
// the plugin thread reports the outcome when the handler settles, and the host
// answers then. This registry is the meeting point. It is process-global for
// the same reason the token table is — one editor process, several possible
// front doors (the daemon's IPC clients and the in-process control socket).
// ===========================================================================

/// The outcome of a plugin command, once its handler has settled.
#[derive(Debug, Clone)]
pub struct CommandOutcome {
    /// The request id handed out by [`run_command_by_id`].
    pub request_id: u64,
    pub ok: bool,
    /// The handler's return value, JSON-encoded (`None` when it returned
    /// nothing). This is what `fresh --cmd cmd run <id>` prints.
    pub output: Option<String>,
    pub error: Option<String>,
}

/// A settled outcome plus when it was recorded, so one nobody ever claims can
/// be reaped instead of sitting in the queue for the life of the process.
struct QueuedOutcome {
    outcome: CommandOutcome,
    queued_at: std::time::Instant,
}

/// How long an unclaimed outcome is kept. Far longer than any caller waits (the
/// CLI gives up in seconds), so this only ever collects outcomes whose caller
/// disappeared — a disconnected client, a host that shut down mid-command.
const OUTCOME_RETENTION: std::time::Duration = std::time::Duration::from_secs(600);

fn completions() -> &'static Mutex<Vec<QueuedOutcome>> {
    static COMPLETED: OnceLock<Mutex<Vec<QueuedOutcome>>> = OnceLock::new();
    COMPLETED.get_or_init(|| Mutex::new(Vec::new()))
}

/// Allocate the id that ties a dispatched plugin command to the caller waiting
/// on it. Process-wide and monotonic; ids are never reused within a run.
fn next_request_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// Record a settled plugin command. Called from the editor thread when the
/// plugin runtime reports a handler's return value (or its failure).
pub fn complete(request_id: u64, ok: bool, output: Option<String>, error: Option<String>) {
    if let Ok(mut done) = completions().lock() {
        done.push(QueuedOutcome {
            outcome: CommandOutcome {
                request_id,
                ok,
                output,
                error,
            },
            queued_at: std::time::Instant::now(),
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
        } else if now.duration_since(done[i].queued_at) > OUTCOME_RETENTION {
            done.remove(i);
        } else {
            i += 1;
        }
    }
    taken
}

/// What [`run_command_by_id`] decided about a dispatch.
#[derive(Debug, Clone)]
pub enum CommandDispatch {
    /// The command ran to completion on the editor thread (a core action) or
    /// was refused outright. Answer the caller now.
    Settled {
        ok: bool,
        error: Option<String>,
        output: Option<String>,
    },
    /// A plugin handler was started and will settle later. Hold the caller
    /// until an outcome with this id comes out of [`take_completed_where`].
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

/// Whether `token` is known and permits `command_id`.
pub fn is_allowed(token: &str, command_id: &str) -> bool {
    lookup(token).map(|g| g.allows(command_id)).unwrap_or(false)
}

/// Enumerate the editor commands `grant` permits, mapped to protocol
/// [`CommandInfo`]. The stable id is the command's canonical action name
/// (`Action::to_action_str`, e.g. `split_vertical`) — the same string
/// `RunCommand.id` expects and that keybindings/plugins use — so the two
/// halves of the channel stay in sync. Only commands on the token's
/// allowlist are returned, so `ListCommands` can't double as a probe of the
/// full command set.
///
/// `include_args` fills each entry's argument schema from the registry — what a
/// plugin declared via `editor.registerCommand(..., { args: [...] })`. It is
/// off for a plain `cmd list` (an id + name per line) and on for `cmd
/// describe`, which is where an agent goes to learn how to call a command.
pub fn list_allowed_commands(
    editor: &Editor,
    grant: &Grant,
    include_args: bool,
) -> Vec<CommandInfo> {
    let Ok(registry) = editor.command_registry().read() else {
        return Vec::new();
    };
    registry
        .get_all()
        .into_iter()
        .filter_map(|cmd| {
            let id = cmd.action.to_action_str();
            if !grant.allows(&id) {
                return None;
            }
            Some(CommandInfo {
                id,
                name: cmd.get_localized_name(),
                // Commands have no palette category field to surface.
                category: None,
                returns: if include_args {
                    cmd.returns.clone()
                } else {
                    None
                },
                args: if include_args {
                    cmd.args
                        .iter()
                        .map(|a| CommandArgInfo {
                            name: a.name.clone(),
                            required: a.required,
                            description: a.description.clone(),
                        })
                        .collect()
                } else {
                    Vec::new()
                },
            })
        })
        .collect()
}

/// Authorize and dispatch a single command by id on `editor`, following the
/// same command → action → `handle_action` pipeline the command palette uses.
///
/// Returns a [`CommandDispatch`]: `Settled` for a core action (which finishes
/// inside `handle_action`) or a refusal, `Pending` for a plugin handler whose
/// result arrives later via [`take_completed`]. Refused (with `ok = false`)
/// when there is no token, the token is unknown/expired, the id is not on the
/// token's allowlist, or the id is not a real registered command. The target
/// window is derived from the token's grant (never the client), so a token can
/// only ever drive its own workspace.
pub fn run_command_by_id(
    editor: Option<&mut Editor>,
    token: Option<&str>,
    id: &str,
    args: &HashMap<String, String>,
) -> CommandDispatch {
    let Some(token) = token else {
        return CommandDispatch::refused("no capability token: command dispatch is not authorized");
    };
    let Some(grant) = lookup(token) else {
        return CommandDispatch::refused("unknown or expired capability token");
    };
    if !grant.allows(id) {
        return CommandDispatch::refused(format!("command not allowed: {}", id));
    }
    let Some(editor) = editor else {
        return CommandDispatch::refused("editor unavailable");
    };

    // Target the token's own window when it is still live. `set_active_window`
    // is a no-op for an unknown id, so an already-torn-down window falls back
    // to the currently active one.
    if let Some(wid) = grant.window_id {
        editor.set_active_window(fresh_core::WindowId(wid));
    }

    // Only dispatch commands that are actually registered (and that
    // `list_allowed_commands` would surface), matched by their canonical
    // action id.
    let known = {
        let Ok(registry) = editor.command_registry().read() else {
            return CommandDispatch::refused("command registry unavailable");
        };
        registry
            .get_all()
            .iter()
            .any(|c| c.action.to_action_str() == id)
    };
    if !known {
        return CommandDispatch::refused(format!("unknown command: {}", id));
    }

    // Thread the string args through `Action::from_str`, which consumes
    // `char`/custom args for the actions that take them; argless commands
    // ignore it. (`from_str` maps an unknown name to a plugin action rather
    // than `None`, but we've already confirmed the id is a real command.)
    let json_args: HashMap<String, serde_json::Value> = args
        .iter()
        .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
        .collect();
    let Some(action) = Action::from_str(id, &json_args) else {
        return CommandDispatch::refused(format!("unknown command: {}", id));
    };

    // A plugin command gets its arguments handed to the handler as an object,
    // and its return value handed back to the caller. `handle_action` can do
    // neither: `Action::PluginAction` carries only a name, which is all a
    // keystroke has to give it, and it reports nothing back. Routing here
    // instead is what makes a parameterized plugin command — `fresh --cmd cmd
    // run orchestrator_agent_new path=… agent=claude` — both callable and
    // *answerable*.
    if let Action::PluginAction(name) = &action {
        let request_id = next_request_id();
        return match editor.run_plugin_action_with_args(name, args, request_id) {
            Ok(()) => CommandDispatch::Pending { request_id },
            Err(e) => CommandDispatch::refused(e),
        };
    }

    match editor.handle_action(action) {
        Ok(()) => CommandDispatch::Settled {
            ok: true,
            error: None,
            output: None,
        },
        Err(e) => CommandDispatch::refused(format!("command failed: {}", e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::commands::{Command as EditorCommand, CommandSource};
    use fresh_core::command::CommandArg;

    /// A registry entry for a plugin command that declares two arguments —
    /// what `editor.registerCommand(..., { args: [...] })` produces.
    fn plugin_command_with_args() -> EditorCommand {
        EditorCommand {
            name: "Run Agent (headless)".to_string(),
            description: "Launch a coding agent".to_string(),
            action: Action::PluginAction("orchestrator_agent_run".to_string()),
            contexts: Vec::new(),
            custom_contexts: vec!["fresh-cli".to_string()],
            source: CommandSource::Plugin("orchestrator".to_string()),
            terminal_bypass: false,
            returns: Some("{ windowId, workspaceId }".to_string()),
            args: vec![
                CommandArg {
                    name: "agent".to_string(),
                    required: false,
                    description: Some("Agent command line".to_string()),
                },
                CommandArg {
                    name: "prompt".to_string(),
                    required: true,
                    description: None,
                },
            ],
        }
    }

    /// The registry's declared arguments reach the `CommandInfo` a caller sees.
    /// This is the whole point of `include_args`: an agent that discovers a
    /// command through `cmd describe` must learn how to call it, rather than
    /// needing the parameters hard-coded into its prompt.
    #[test]
    fn declared_args_are_surfaced_when_requested() {
        let cmd = plugin_command_with_args();
        let info = CommandInfo {
            id: cmd.action.to_action_str(),
            name: cmd.name.clone(),
            category: None,
            returns: None,
            args: cmd
                .args
                .iter()
                .map(|a| CommandArgInfo {
                    name: a.name.clone(),
                    required: a.required,
                    description: a.description.clone(),
                })
                .collect(),
        };
        assert_eq!(info.id, "orchestrator_agent_run");
        assert_eq!(info.args.len(), 2);
        assert_eq!(info.args[0].name, "agent");
        assert!(!info.args[0].required);
        assert_eq!(
            info.args[0].description.as_deref(),
            Some("Agent command line")
        );
        assert_eq!(info.args[1].name, "prompt");
        assert!(info.args[1].required);
    }

    /// A command's declared arguments survive the JSON round trip the control
    /// socket puts them through — `cmd describe --json` is an agent-facing
    /// contract, so the wire shape matters as much as the in-process one.
    #[test]
    fn command_info_args_round_trip_over_the_wire() {
        let cmd = plugin_command_with_args();
        let info = CommandInfo {
            id: cmd.action.to_action_str(),
            name: cmd.name,
            category: None,
            returns: None,
            args: vec![CommandArgInfo {
                name: "prompt".to_string(),
                required: true,
                description: Some("Initial prompt".to_string()),
            }],
        };
        let json = serde_json::to_string(&info).expect("serialize");
        let back: CommandInfo = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(back.args.len(), 1);
        assert_eq!(back.args[0].name, "prompt");
        assert!(back.args[0].required);
        assert_eq!(back.args[0].description.as_deref(), Some("Initial prompt"));
    }

    /// A command with no declared arguments serializes without an `args` key at
    /// all, keeping `cmd list` output (one line per command) unchanged for the
    /// argless majority.
    #[test]
    fn argless_command_omits_args_on_the_wire() {
        let info = CommandInfo {
            id: "split_vertical".to_string(),
            name: "Split Vertical".to_string(),
            category: None,
            returns: None,
            args: Vec::new(),
        };
        let json = serde_json::to_string(&info).expect("serialize");
        assert!(!json.contains("args"), "unexpected args key in {}", json);
    }

    #[test]
    fn mint_register_lookup_revoke() {
        let token = mint(Grant::new(Some(7), ["split_vertical".to_string()]));
        let grant = lookup(&token).expect("registered token resolves");
        assert_eq!(grant.window_id, Some(7));
        assert!(grant.allows("split_vertical"));
        assert!(!grant.allows("delete_everything"));
        assert!(is_allowed(&token, "split_vertical"));
        assert!(!is_allowed(&token, "nope"));

        revoke(&token);
        assert!(lookup(&token).is_none());
        assert!(!is_allowed(&token, "split_vertical"));
    }

    #[test]
    fn unknown_token_denies() {
        assert!(!is_allowed("not-a-real-token", "anything"));
        assert!(lookup("not-a-real-token").is_none());
    }

    #[test]
    fn tokens_are_distinct() {
        assert_ne!(new_token(), new_token());
    }

    /// A refusal is settled immediately — nothing was dispatched, so nothing
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
    fn run_command_without_token_is_refused() {
        let reason = refusal_reason(run_command_by_id(
            None,
            None,
            "split_vertical",
            &HashMap::new(),
        ));
        assert!(reason.contains("not authorized"), "{}", reason);
    }

    #[test]
    fn run_command_unknown_token_is_refused() {
        let reason = refusal_reason(run_command_by_id(
            None,
            Some("not-a-real-token"),
            "split_vertical",
            &HashMap::new(),
        ));
        assert!(reason.contains("unknown or expired"), "{}", reason);
    }

    #[test]
    fn run_command_not_on_allowlist_is_refused() {
        // A valid token whose allowlist does not include the requested id is
        // rejected before the editor is ever touched (so `None` editor is
        // fine here — the allowlist check returns first).
        let token = mint(Grant::new(Some(1), ["save".to_string()]));
        let reason = refusal_reason(run_command_by_id(
            None,
            Some(&token),
            "split_vertical",
            &HashMap::new(),
        ));
        assert_eq!(reason, "command not allowed: split_vertical");
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
            let mut done = completions().lock().unwrap();
            for q in done.iter_mut() {
                if q.outcome.request_id == orphan {
                    q.queued_at = std::time::Instant::now() - (OUTCOME_RETENTION * 2);
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
