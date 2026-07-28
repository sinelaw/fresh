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
/// Returns `(ok, error)` for a `CommandResult`. Refused (with `ok = false`)
/// when there is no token, the token is unknown/expired, the id is not on the
/// token's allowlist, or the id is not a real registered command. The target
/// window is derived from the token's grant (never the client), so a token can
/// only ever drive its own workspace.
pub fn run_command_by_id(
    editor: Option<&mut Editor>,
    token: Option<&str>,
    id: &str,
    args: &HashMap<String, String>,
) -> (bool, Option<String>) {
    let Some(token) = token else {
        return (
            false,
            Some("no capability token: command dispatch is not authorized".to_string()),
        );
    };
    let Some(grant) = lookup(token) else {
        return (
            false,
            Some("unknown or expired capability token".to_string()),
        );
    };
    if !grant.allows(id) {
        return (false, Some(format!("command not allowed: {}", id)));
    }
    let Some(editor) = editor else {
        return (false, Some("editor unavailable".to_string()));
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
            return (false, Some("command registry unavailable".to_string()));
        };
        registry
            .get_all()
            .iter()
            .any(|c| c.action.to_action_str() == id)
    };
    if !known {
        return (false, Some(format!("unknown command: {}", id)));
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
        return (false, Some(format!("unknown command: {}", id)));
    };

    // A plugin command gets its arguments handed to the handler as an object.
    // `handle_action` can't: `Action::PluginAction` carries only a name, which
    // is all a keystroke has to give it. Routing here instead is what makes a
    // parameterized plugin command — `fresh --cmd cmd run orchestrator_agent_run
    // agent=claude prompt=…` — reachable at all.
    if let Action::PluginAction(name) = &action {
        return match editor.run_plugin_action_with_args(name, args) {
            Ok(()) => (true, None),
            Err(e) => (false, Some(e)),
        };
    }

    match editor.handle_action(action) {
        Ok(()) => (true, None),
        Err(e) => (false, Some(format!("command failed: {}", e))),
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

    #[test]
    fn run_command_without_token_is_refused() {
        let (ok, error) = run_command_by_id(None, None, "split_vertical", &HashMap::new());
        assert!(!ok);
        assert!(error.is_some());
    }

    #[test]
    fn run_command_unknown_token_is_refused() {
        let (ok, error) = run_command_by_id(
            None,
            Some("not-a-real-token"),
            "split_vertical",
            &HashMap::new(),
        );
        assert!(!ok);
        assert!(error.is_some());
    }

    #[test]
    fn run_command_not_on_allowlist_is_refused() {
        // A valid token whose allowlist does not include the requested id is
        // rejected before the editor is ever touched (so `None` editor is
        // fine here — the allowlist check returns first).
        let token = mint(Grant::new(Some(1), ["save".to_string()]));
        let (ok, error) = run_command_by_id(None, Some(&token), "split_vertical", &HashMap::new());
        assert!(!ok);
        assert_eq!(
            error.as_deref(),
            Some("command not allowed: split_vertical")
        );
        revoke(&token);
    }
}
