//! Minimization meta-driver — see
//! `docs/internal/scenario-meta-testing.md`.
//!
//! Gated behind `FRESH_MUTATION=1`. For each `BufferScenario`, greedily
//! drops actions while `check_buffer_scenario` still passes, and appends
//! an advisory record when the minimal still-passing action sequence is
//! shorter than the written one. A `minimal_len == 0` record means the
//! expectation holds with **no actions** — a vacuous test.
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

fn write_record(s: &BufferScenario, minimal: &[Action]) {
    let path = std::env::var("FRESH_MUTATION_REPORT")
        .unwrap_or_else(|_| "target/scenario-minimization.jsonl".to_string());
    let minimal_actions: Vec<String> = minimal.iter().map(|a| format!("{a:?}")).collect();
    let line = format!(
        "{{\"layer\":\"buffer\",\"vacuous\":{},\"original_len\":{},\"minimal_len\":{},\"description\":{:?},\"minimal_actions\":{:?}}}\n",
        minimal.is_empty(),
        s.actions.len(),
        minimal.len(),
        s.description,
        minimal_actions,
    );
    let _guard = REPORT_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
    {
        let _ = f.write_all(line.as_bytes());
    }
}
