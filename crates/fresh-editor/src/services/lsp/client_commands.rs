//! The set of LSP *client commands* plugins have claimed.
//!
//! LSP lets a server return a `Command` it does not run itself: anything
//! outside the server's advertised `executeCommandProvider` list is the
//! client's to interpret. The protocol says nothing about what any such
//! command means or what its `arguments` contain — that is each server's
//! own extension (rust-analyzer's `runSingle` carries a `Runnable` object
//! describing a cargo invocation, for instance).
//!
//! The core therefore does not interpret them. A plugin claims a name with
//! `registerLspClientCommands`, and the core does exactly two things with
//! the registry:
//!
//! 1. advertises the claimed names at `initialize`, under
//!    `experimental.commands.commands` — rust-analyzer suppresses its
//!    runnable CodeLens entries entirely unless the client declares them;
//! 2. routes a claimed command to the `lsp_execute_command` hook rather
//!    than sending `workspace/executeCommand` to a server that never
//!    advertised it.
//!
//! **Claims only accumulate.** There is no unregister: a plugin that is
//! unloaded leaves its names claimed, so the core keeps routing them to a
//! hook rather than to the server. That is reported rather than silent —
//! `execute_code_lens_command` checks for a live handler first — but it is
//! not undone. Reloading a plugin is fine (it re-claims the same names).
//! Per-plugin ownership would be needed to do better.
//!
//! **Why this is process-global rather than editor state.** Both readers
//! need it from places that cannot reach the `Editor`: the handshake runs
//! on the LSP task, with no editor handle by construction. Plugin
//! registrations are themselves process-wide (a plugin is not scoped to a
//! window), so one registry per process is also the honest model rather
//! than a convenience. Mirrors `DIAGNOSTIC_CACHE` in this module.

use std::collections::BTreeSet;
use std::sync::{LazyLock, RwLock};

static CLIENT_COMMANDS: LazyLock<RwLock<BTreeSet<String>>> =
    LazyLock::new(|| RwLock::new(BTreeSet::new()));

/// Claim `commands` for plugin handling. Idempotent; returns whether any
/// of them were not already claimed, which is what tells the caller a
/// running server needs to re-handshake to hear about them.
pub fn register_all(commands: Vec<String>) -> bool {
    let Ok(mut set) = CLIENT_COMMANDS.write() else {
        return false;
    };
    let mut added = false;
    for command in commands {
        added |= set.insert(command);
    }
    added
}

/// Whether `command` has been claimed by a plugin.
pub fn is_registered(command: &str) -> bool {
    CLIENT_COMMANDS
        .read()
        .map(|set| set.contains(command))
        .unwrap_or(false)
}

/// The claimed names, sorted, for the `initialize` handshake.
pub fn snapshot() -> Vec<String> {
    CLIENT_COMMANDS
        .read()
        .map(|set| set.iter().cloned().collect())
        .unwrap_or_default()
}
