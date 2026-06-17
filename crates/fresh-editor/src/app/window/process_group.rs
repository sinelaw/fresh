//! Per-window process-group tracking + signalling.
//!
//! Each `Window` owns a [`ProcessGroups`] that records the leader
//! pid of every OS process group the window has spawned (today:
//! pty children from `terminal_manager.spawn`; later: long-running
//! tool agents, language servers spawned by the window's
//! authority, …). The window's authority provides a concrete
//! [`Signaller`] implementation, so "stop everything this window
//! owns" is a single `process_groups.signal_all("SIGTERM")` call
//! regardless of whether the spawns happened locally, inside a
//! container, or on a remote host.
//!
//! ## Why per-window?
//!
//! The Orchestrator lifecycle wants to terminate every process
//! belonging to one workspace (a `Window`) without touching the others.
//! Routing through the window keeps that aggregation in one
//! place — callers don't need to know how many terminals the
//! window has or whether a future feature added another kind of
//! background process; they just say "signal this window".
//!
//! ## Authority pluggability
//!
//! The [`Signaller`] trait is the seam between "I know who I
//! want to signal" and "I know how to deliver that signal in
//! this authority's namespace". Local pty processes are reached
//! by `kill(-pid, …)` on the host kernel. Container / SSH
//! authorities will plug in their own implementations that
//! forward through `docker exec kill -PGRP …` or an SSH
//! channel — see the design doc in
//! `docs/internal/orchestrator-open-dialog-and-lifecycle.md` for
//! how that fits into the broader lifecycle.

use std::sync::Arc;

/// Authority-pluggable mechanism for sending OS signals to a
/// process group whose leader pid is known. Concrete impls live
/// per-authority — see [`LocalSignaller`] for the host shell
/// case; container / SSH variants are tracked as future work.
pub trait Signaller: Send + Sync + std::fmt::Debug {
    /// Send `signal_name` ("SIGTERM" / "SIGKILL" / "SIGINT" /
    /// "SIGHUP") to the process group led by `leader_pid`.
    ///
    /// Returns `Ok(true)` when the signal was delivered to a
    /// live group; `Ok(false)` when the group has already exited
    /// (idempotent no-op so retry loops are safe); and `Err`
    /// for permission / lookup / authority-specific failures.
    fn signal(&self, leader_pid: u32, signal_name: &str) -> Result<bool, String>;
}

/// Local-process signaller. The pty puts every spawned shell at
/// the head of its own session, so `kill(-pid, sig)` targets the
/// shell and every subprocess it forked.
#[derive(Debug, Default)]
pub struct LocalSignaller;

impl Signaller for LocalSignaller {
    #[cfg(unix)]
    fn signal(&self, leader_pid: u32, signal_name: &str) -> Result<bool, String> {
        let sig = match signal_name {
            "SIGTERM" => libc::SIGTERM,
            "SIGKILL" => libc::SIGKILL,
            "SIGINT" => libc::SIGINT,
            "SIGHUP" => libc::SIGHUP,
            other => return Err(format!("unsupported signal: {}", other)),
        };
        // `kill(-pid, sig)` (note the negation) sends `sig` to the
        // process group whose leader is `pid`.
        let rc = unsafe { libc::kill(-(leader_pid as i32), sig) };
        if rc == 0 {
            Ok(true)
        } else {
            let err = std::io::Error::last_os_error();
            // ESRCH = no such process / group. Treat as
            // "nothing to signal" so the caller's stop flow
            // stays idempotent.
            if err.raw_os_error() == Some(libc::ESRCH) {
                Ok(false)
            } else {
                Err(format!("kill(-{}, {}): {}", leader_pid, signal_name, err))
            }
        }
    }

    #[cfg(windows)]
    fn signal(&self, _leader_pid: u32, signal_name: &str) -> Result<bool, String> {
        // Windows has no direct pgrp signaling. Callers wanting
        // a hard kill route through `TerminalManager::close`
        // (which uses the pty child killer).
        Err(format!(
            "Windows LocalSignaller cannot deliver {} — use TerminalManager::close()",
            signal_name
        ))
    }
}

/// One entry in a window's tracked process groups. The `label`
/// is a human-readable hint shown in error messages and the
/// Orchestrator preview pane (e.g. "terminal #3", "lsp:rust").
#[derive(Debug, Clone)]
pub struct ProcessGroupEntry {
    pub leader_pid: u32,
    pub label: String,
}

/// Per-window aggregation of process groups. Spawning code
/// (terminal manager, future LSP spawn paths) calls
/// [`ProcessGroups::register`] when a new leader pid is known;
/// `signal_all` fans out through the authority's [`Signaller`]
/// when the window-level lifecycle operation fires.
#[derive(Debug)]
pub struct ProcessGroups {
    signaller: Arc<dyn Signaller>,
    entries: Vec<ProcessGroupEntry>,
}

impl ProcessGroups {
    /// Construct with an explicit [`Signaller`]. Window's
    /// authority decides which signaller — local windows pass
    /// `Arc::new(LocalSignaller)`.
    pub fn new(signaller: Arc<dyn Signaller>) -> Self {
        Self {
            signaller,
            entries: Vec::new(),
        }
    }

    /// Track a new process group leader. Idempotent: calling
    /// with the same `leader_pid` twice replaces the label
    /// rather than duplicating the entry.
    pub fn register(&mut self, leader_pid: u32, label: impl Into<String>) {
        let label = label.into();
        if let Some(e) = self.entries.iter_mut().find(|e| e.leader_pid == leader_pid) {
            e.label = label;
        } else {
            self.entries.push(ProcessGroupEntry { leader_pid, label });
        }
    }

    /// Drop tracking for `leader_pid`. Doesn't signal — call
    /// when the process has already exited (e.g. from a
    /// `terminal_exit` hook).
    pub fn forget(&mut self, leader_pid: u32) {
        self.entries.retain(|e| e.leader_pid != leader_pid);
    }

    /// Send `signal_name` to every registered process group.
    /// Returns one result per entry — caller decides how to
    /// surface aggregate failures. Entries whose `signal` says
    /// "already exited" (`Ok(false)`) are forgotten in place so
    /// a follow-up signal cycle stays small.
    pub fn signal_all(
        &mut self,
        signal_name: &str,
    ) -> Vec<(ProcessGroupEntry, Result<bool, String>)> {
        let mut out = Vec::with_capacity(self.entries.len());
        let mut dead: Vec<u32> = Vec::new();
        for e in &self.entries {
            let r = self.signaller.signal(e.leader_pid, signal_name);
            if matches!(r, Ok(false)) {
                dead.push(e.leader_pid);
            }
            out.push((e.clone(), r));
        }
        self.entries.retain(|e| !dead.contains(&e.leader_pid));
        out
    }

    /// Replace the signaller (e.g. when the window's authority
    /// changes mid-life). Existing entries stay tracked; future
    /// `signal_all` calls go through the new signaller.
    pub fn set_signaller(&mut self, signaller: Arc<dyn Signaller>) {
        self.signaller = signaller;
    }

    pub fn entries(&self) -> &[ProcessGroupEntry] {
        &self.entries
    }
}

impl Default for ProcessGroups {
    fn default() -> Self {
        Self::new(Arc::new(LocalSignaller))
    }
}
