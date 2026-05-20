# Scenario meta-testing: minimization & combination

Status: accepted (initial), 2026-05-20
Owner: test-infra
Related: `e2e-test-migration-design.md`, `scenario-migration-status.md`

## Context

The semantic test suite expresses tests as data: each scenario is a
value `(initial state, events, expected state)` consumed by a
fallible runner `check_*(s) -> Result<(), ScenarioFailure>` (and a
panicking `assert_*` wrapper). The `mod.rs` doc already frames these
values as consumed by multiple *drivers*: the regression runner,
proptest generators (`property.rs`), and shadow differentials
(`shadow.rs`).

Two recurring quality problems motivated this work (both surfaced by
the PR #2061 migration audit):

1. **Vacuous / non-load-bearing tests** — a scenario whose expectation
   holds even if the load-bearing action is dropped (or whose
   expectation is fully wildcarded). The hand-written `anti_*` tests +
   `check-semantic-migration-conventions.sh` guard this *manually*,
   per file.
2. **Over-specified tests** — long action sequences where most steps
   don't contribute to the assertion (setup bloat).

We want automated *meta-tests* — a fourth driver — that consume the
existing corpus and prove properties *about the tests themselves*.

## Decisions

### Scope & priority

- **Drop the "validation / flip-the-assert" driver for now.**
  Minimization subsumes its most important job: a vacuous test
  minimizes to zero actions (see below), so we still catch the FAKE /
  non-load-bearing class without a separate type-aware assertion-flip
  mechanism.
- **Priority order:**
  1. **Minimization** (delta-debug the action list).
  2. **Combination** with an **active reset** (not a fresh harness).
- **Deferred (do after the above land):**
  - (e) **Cross-driver agreement** — feed each scenario through the
    live runner and the shadow model and assert they agree.
  - (f) **`cargo-mutants`** on production code (the complementary
    axis: "do tests cover the code?" vs. "are the tests
    load-bearing?").
- **Deprioritized** (not now): determinism re-run, serde round-trip,
  static wildcard audit, assertion-coverage map.
- All meta-drivers run behind an env gate (`FRESH_MUTATION=1`) so the
  normal `cargo test` fast path is unchanged; a dedicated CI job runs
  the meta pass.

### 1. Minimization

- **Driver:** delta-debugging (ddmin, ~O(n log n) re-checks) over the
  scenario's `events`/`actions` vector. **Not** the 2ⁿ powerset.
- **Validity predicate:** a candidate subsequence is valid iff
  `check(candidate)` **passes**. (No assertion-flip guard, since
  validation is dropped.)
- **Report (advisory, never a hard failure):** per scenario, emit
  `original_len`, `minimal_len`, and the dropped actions. Sort by
  `original_len - minimal_len`.
  - `minimal_len == 0` → the expectation holds with **no actions** →
    **vacuous test** (this is the FAKE-test catch). Flag loudly.
  - `minimal_len ≪ original_len` → setup bloat / over-specification.
  - Long bug-repro scenarios that legitimately replay full user steps
    are expected to show some reducibility; this is a *report*, not a
    gate.
- **Note:** removing a *middle* action can shift byte offsets the
  assertion's absolute positions depend on, so it simply won't be
  removable — correct behavior, not a bug; ddmin handles it.
- **Start layer:** `BufferScenario` (its `check` is cheap — no render).

### 2. Combination with active reset

**Active reset = actions that reverse the editor to the scenario's
initial state**, driven through the production action path — *not* a
fresh harness. For the buffer layer:

```
Esc (cancel modal/selection)
RemoveSecondaryCursors
SelectAll
InsertChar × initial_text   (first char replaces the selection)
MoveDocumentStart
clear selection
```

**Honest limits (these bound where combination applies):**

- Active reset only restores **text + cursor + selection**. It does
  **not** clear the undo/event log, the modified flag, config toggles
  (line-wrap, line-numbers), markers/virtual-lines, or the clipboard —
  none of those are reachable by buffer-edit actions.
- **Decision:** reset will **not** drive `Undo`-to-empty (fragile).
  Instead, **combination is scoped to `BufferScenario`s whose
  observable is text/cursor/selection and that do not assert on
  `event_log_len` / `is_modified`.** Persistence / workspace / modal /
  temporal scenarios cannot use action-reset (fs, multi-buffer, clock)
  and are **out of scope** for combination.

**Procedure:**

0. **Validate reset in isolation first:** run a scenario, then `reset`,
   and assert the observable equals the fresh-harness baseline. If
   `SelectAll + retype + MoveDocumentStart` doesn't reproduce a clean
   baseline on its own, reset is buggy — finding #0.
1. Take N in-scope scenarios, run `S1; reset; S2; reset; …` on **one
   long-lived harness**, asserting each `Sᵢ`'s expectation at its
   checkpoint.
2. Repeat under **random permutations** of the order.

**Findings:** `Sᵢ` passes alone but fails after `Sⱼ` ⇒ either

- **reset is incomplete** for some state `Sᵢ` reads (extend reset, or
  document the leaked state), or
- **`Sᵢ` secretly depends on ambient state** (test smell).

Both are bugs the fresh-harness model can never surface — which is the
entire reason for using an active reset instead of a fresh harness.

## Build order

1. Minimization on `BufferScenario` (self-contained; no reset needed) →
   produce the ranked report; triage vacuous/bloated tests.
2. Active-reset primitive + the isolation check (finding #0).
3. Combination / permutation on top of the reset primitive.
4. Later: (e) cross-driver agreement, (f) `cargo-mutants` nightly job.

## Open questions

- Whether to later promote minimization's `minimal_len == 0` report to
  a hard CI failure (would replace the manual `anti_*` convention for
  the buffer layer).
- Extending combination beyond the buffer layer would need a richer
  reset (history/config/markers) — revisit only if the buffer-layer
  results prove valuable.
