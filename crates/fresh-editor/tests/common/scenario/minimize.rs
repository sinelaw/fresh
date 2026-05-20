//! Minimization meta-driver — see
//! `docs/internal/scenario-meta-testing.md`.
//!
//! Gated behind `FRESH_MUTATION=1`. For each `BufferScenario`, greedily
//! drops actions while `check_buffer_scenario` still passes, and appends
//! an advisory record when the minimal still-passing action sequence is
//! shorter than the written one. `minimal_len == 0` means the actions
//! return to the initial state (no-op / idempotence / undo-to-original
//! / round-trip). It is advisory only: minimization alone cannot tell a
//! legitimate "this action is a no-op" assertion from a truly empty one
//! — that distinction needs the validation/flip guard.
//!
//! Advisory only: this never fails a test. Read the report (JSONL) at
//! `$FRESH_MUTATION_REPORT` (default `target/scenario-minimization.jsonl`).
//!
//! Scope: pure-`actions` buffer scenarios. Scenarios that also use
//! `events` are skipped (minimizing `actions` alone could pass for an
//! unrelated reason); `events` minimization is a follow-up.

use std::io::Write;
use std::sync::Mutex;

use crate::common::scenario::buffer_scenario::{check_buffer_scenario, BufferScenario};
use fresh::test_api::Action;

static REPORT_LOCK: Mutex<()> = Mutex::new(());

fn enabled() -> bool {
    std::env::var_os("FRESH_MUTATION").is_some()
}

/// Greedy 1-minimal subsequence for which `pred` still holds. Assumes
/// `pred(input)` is already true. O(n^2) checks worst case — fine for
/// the small action lists buffer scenarios carry; swap for ddmin if a
/// layer ever needs it.
fn minimize<T: Clone>(input: &[T], pred: impl Fn(&[T]) -> bool) -> Vec<T> {
    let mut cur = input.to_vec();
    let mut changed = true;
    while changed {
        changed = false;
        let mut i = 0;
        while i < cur.len() {
            let mut cand = cur.clone();
            cand.remove(i);
            if pred(&cand) {
                cur = cand;
                changed = true;
            } else {
                i += 1;
            }
        }
    }
    cur
}

/// If mutation mode is on and `s` passes as written, minimize its
/// action list and record any reduction. No-op otherwise.
pub(crate) fn report(s: &BufferScenario) {
    if !enabled() || s.actions.is_empty() || !s.events.is_empty() {
        return;
    }
    // Only minimize tests that pass as written — a genuinely failing
    // scenario is the real `assert_*`'s problem, and minimizing it
    // would emit a misleading record.
    if check_buffer_scenario(s.clone()).is_err() {
        return;
    }
    let minimal = minimize(&s.actions, |sub| {
        let mut cand = s.clone();
        cand.actions = sub.to_vec();
        check_buffer_scenario(cand).is_ok()
    });
    if minimal.len() < s.actions.len() {
        write_record(s, &minimal);
    }
}

/// Resolve the report path. `cargo test` runs the binary with cwd set
/// to the package dir (`crates/fresh-editor`), so a cwd-relative
/// `target/...` would point at a nonexistent dir; resolve the workspace
/// `target/` at compile time via `CARGO_MANIFEST_DIR` instead.
fn report_path() -> std::path::PathBuf {
    if let Ok(p) = std::env::var("FRESH_MUTATION_REPORT") {
        return std::path::PathBuf::from(p);
    }
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/scenario-minimization.jsonl")
}

#[derive(serde::Serialize)]
struct Record<'a> {
    layer: &'a str,
    /// `minimal_len == 0`: the actions net to the initial state. Not
    /// proof of vacuity (see module docs) — advisory.
    returns_to_initial: bool,
    original_len: usize,
    minimal_len: usize,
    description: &'a str,
    minimal_actions: &'a [Action],
}

fn write_record(s: &BufferScenario, minimal: &[Action]) {
    let path = report_path();
    let rec = Record {
        layer: "buffer",
        returns_to_initial: minimal.is_empty(),
        original_len: s.actions.len(),
        minimal_len: minimal.len(),
        description: &s.description,
        minimal_actions: minimal,
    };
    let mut line = serde_json::to_string(&rec).unwrap_or_else(|e| {
        format!(
            "{{\"error\":\"serialize failed: {e}\",\"description\":{:?}}}",
            s.description
        )
    });
    line.push('\n');
    let _guard = REPORT_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    match std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
    {
        Ok(mut f) => {
            if let Err(e) = f.write_all(line.as_bytes()) {
                eprintln!("minimize: failed writing report to {}: {e}", path.display());
            }
        }
        // Loud on failure — a silently-swallowed write is how the
        // cwd-relative-path bug hid for a whole corpus run.
        Err(e) => eprintln!("minimize: cannot open report {}: {e}", path.display()),
    }
}
