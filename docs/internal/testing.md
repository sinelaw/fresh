# Testing Architecture

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: map Fresh's testing infrastructure — its layers, the headless
scenario framework, the determinism strategy, the ANSI capture backend, and
the meta-testing/migration efforts — distinguishing what is implemented from
what is planned.

## 1. Testing layers (overview)

Fresh layers tests by faithfulness vs. speed/coupling. From lowest coupling
(fast, refactor-proof) to highest (slow, terminal-faithful):

| Layer | Drives | Asserts on |
|---|---|---|
| Unit / `#[cfg(test)]` | direct calls | internal invariants |
| Property (proptest) | random op sequences | buffer == `Vec<u8>` oracle |
| Semantic scenarios | the `EditorTestApi` (headless) | typed observables |
| Imperative e2e | the imperative harness (send_key/render) | screen scrape + buffer |
| Integration | real sockets / FS / SSH | end-to-end behavior |
| Visual regression | rendered frames → SVG/byte | pixel/cell snapshots |

The active architectural direction is to collapse the imperative e2e suite
(screen scraping, terminal IO, render timing) into the semantic suite (typed
observables over a headless editor). That migration is the central story of
this doc; see §3, §6.

### 1.1 The two top-level docs

- This contributor-facing guide describes the older imperative regime —
  the imperative harness, shadow validation, proptest, and the standing
  conventions ("no timeouts / e2e over unit / reproduce before fixing").
  It remains accurate for the imperative e2e regime; it predates the scenario
  framework.
- A separate migration-design doc specifies the scenario framework that
  supersedes the imperative regime. The remaining migration plan, behavioral
  findings, and meta-testing notes each live in their own companion docs.

## 2. The imperative e2e harness

The imperative harness is the workhorse of the older regime and still backs
every scenario runner underneath.

- Construction: constructors create a harness at a given width/height, with a
  variant taking an options struct. Those options toggle plugins, working dir,
  project root, a custom `FileSystem`, slow-FS injection, a fake devcontainer,
  and the grammar registry.
- Input: key, text, paste, and mouse helpers — all route through the
  production key/mouse handlers.
- Render: one render path draws to a ratatui buffer; another drives a real
  `CrosstermBackend` whose bytes are parsed back through an embedded
  `vt100::Parser` for terminal-faithful assertions (screen-to-string and
  cursor-position helpers).
- Determinism hooks: advance-time and sleep helpers drive a `TestTimeSource`
  (§4); an accessor exposes it. FS-metric accessors count FS hits for slow-FS
  tests.
- An accessor returns a `&mut dyn EditorTestApi` — the bridge to the scenario
  framework (§3).

### 2.1 Shadow validation (in-harness)

Enabling shadow validation makes the harness maintain a plain `String` that
mirrors every edit; the buffer-content assertion then checks both the real
piece-tree buffer and the shadow agree. This is a cheap differential catch for
piece-tree bugs. It is distinct from the scenario `ShadowModel` framework (§7),
which is a corpus-wide differential.

## 3. The semantic test API (`EditorTestApi`)

The `EditorTestApi` is the **single observation surface** for semantic tests.
The contract: semantic tests bind *only* to this module — never to the editor
application type, the model layer, or the view layer. Production internals can
be refactored freely; this trait is the only thing that must stay stable.

- The `EditorTestApi` trait is implemented on the editor application type. The
  impl lives in the same file as the trait plus the projection types, so the
  entire test-facing surface reviews as one unit.
- Projection types — a caret projection, a modal snapshot, a popup view, a
  prompt view — are pure `Serialize`/`Deserialize` snapshots. They carry only
  the fields tests assert on; internal cursor fields (sticky column, block
  anchor) are intentionally hidden so production refactors don't break tests.
- Determinism baked in: the caret accessor sorts by ascending byte position so
  tests don't depend on internal `HashMap` iteration order.
- Drive surface: a single-action dispatch and a sequence dispatch route through
  the same action-handling path the input layer uses, then drain async
  messages — so a dispatched action settles its plugin/LSP/async follow-ups
  before the next observation.

The observable surface is organized into "classes," each tagged by the
migration phase that added it:

| Class | Observables | Phase |
|---|---|---|
| A — pure state | `buffer_text`, `primary_caret`, `carets`, `selection_text`, `is_modified` | 1 |
| B — layout | viewport top byte, terminal width/height, gutter width, hardware cursor position, visible byte range, top line number, scrollbar geometry | 2 |
| C — modal | modal snapshot (popup stacks + minibuffer prompt) | 3 |
| D — workspace | buffer count, active buffer path, buffer paths | 7 |
| E — input | mouse-click dispatch, full-redraw request | 9 |
| F — markers | marker seeding, marker positions, file-changed notification, event-log length | — |
| G — composite diff | side-by-side diff creation, hunk navigation, layout flush | — |
| H — layout seeding | virtual-line seeding, margin annotation, status message, margin width | — |

Notable design points encoded in the API:
- The buffer-text accessor panics on large-file (unloaded-region) buffers —
  semantic theorems are deliberately not the tool for large-file scenarios.
- The modal snapshot projects two distinct modal channels: the popup stacks
  (global popups plus per-window popups) **and** the minibuffer prompt on the
  active window. The prompt projection is load-bearing: without it, modal
  scenarios that drive command-palette / quick-open / search flows would "pass
  by tautology."
- Every accessor is additive and gated behind a test-only feature — never
  reachable from the production binary.

### 3.1 Enforcing the contract (CI lints)

Two shell linters run in CI (and locally) to keep the contract honest:

- A semantic-isolation linter forbids the semantic tests from importing the
  application, input, services, config-IO, state, or workspace modules
  outright. `crossterm::*`, the model layer, the view layer, and the config
  module are allowed *only* in "harness-direct" files that explicitly use the
  imperative harness (the documented escape hatch for cross-state claims and
  projection types with no API counterpart, such as marker IDs).
- A migration-convention linter requires that every migrated semantic file
  (1) cite its e2e source file in the docstring and (2) ship at least one
  `anti_*` test (see §6.2).

## 4. Determinism: the TimeSource abstraction

This is recorded as the project's first design decision.

**Problem:** wall-clock time makes tests slow and non-deterministic.

**Decision:** a `TimeSource` trait with two impls:
- `RealTimeSource` — real instant, real `thread::sleep`, real UTC clock.
- `TestTimeSource` — holds an atomic count of logical nanoseconds since
  creation. `now()` returns the base instant plus logical elapsed; `sleep(d)`
  does **not** sleep, it just advances the counter; the date accessor derives a
  calendar date from logical days elapsed. So time-driven code (animations,
  debounces, auto-save) runs instantly yet observably "passes" time.

**Trade-off — selective, not total, abstraction:** abstract time where it's
testable; keep real time where it fundamentally must be (the main loop's event
poll, signal-handler sleeps). Services receive a shared `Arc<dyn TimeSource>`
by composition.

This trait is the production hook that let the temporal scenario phase land
with zero new production code: the harness advance-time helper advances the
same `TestTimeSource` the editor already reads, and the clock-advance input
event routes through it. The abstraction is the *already-existing*
`TestTimeSource`, not a separate mock clock — the migration design's
"MockClock" naming is aspirational; the implemented type is `TestTimeSource`.

## 5. The capture backend (ANSI rendering)

The capture backend is a `ratatui::Backend` impl that, instead of writing to a
real terminal, captures all output as ANSI bytes into an in-memory `Vec<u8>`.
Its **primary production role** is the client/server architecture: the server
renders into a capture backend and ships the ANSI byte stream to thin clients.
It is also the mechanism behind ANSI-snapshot-style testing.

Key behaviors:
- The draw path walks cell triples, emitting a cursor-move escape only when the
  next cell isn't contiguous with the last — an optimization that mirrors a
  real diffing backend, so the byte stream is itself a regression target.
- The style writer does SGR diffing against tracked current fg/bg/modifiers,
  emitting a reset (`ESC[0m`) only when a modifier is *removed*, then the
  minimal set of SGR params. A color-params helper handles the 16-color,
  256-indexed, and truecolor paths. Redundant-SGR / missing-reset bugs are
  exactly what a byte-level snapshot catches.
- A style-state reset forces a full repaint when a new client connects.
- Cursor visibility (hide/show) *always* emits its escape, with no
  optimization, so a reconnecting client is never left out of sync.
- Terminal setup/teardown sequences are shared with the direct-mode terminal
  via a common sequences module, so capture and real terminal can't drift on
  which modes (alt screen, mouse, focus, bracketed paste) get enabled.
- The module ships unit tests asserting the exact escape bytes for clear, draw,
  cursor visibility, and setup/teardown.

### 5.1 vt100 round-trip (`TerminalIoScenario`, Phase 8)

The scenario framework's terminal-faithful layer doesn't assert on raw bytes —
it renders through the *real* `CrosstermBackend`, parses the emitted ANSI back
through `vt100`, and asserts on the resulting round-trip grid of visible rows
plus hardware cursor. This catches escape-emission and incremental-redraw bugs
without committing to a specific byte sequence — the byte stream is an
implementation detail, the displayed grid is the contract.

## 6. The scenario framework (headless, data-as-tests)

The framework lives alongside the semantic tests. Core idea: a test is a
**value** `(initial state, action sequence, expected observable)`, not a
script. One value feeds three drivers with no extra wiring:

```
            Scenario value
          /       |        \
 regression   proptest      shadow-model
   runner     generator     differential
```

### 6.1 Anatomy

- The `Observable` trait extracts a typed, serializable snapshot from a live
  harness after all events dispatch. It is implemented for the buffer state,
  modal state, workspace state, round-trip grid, styled frame, and tuples for
  cross-cutting scenarios.
- The `InputEvent` type is a superset of `Action`, adding mouse, prompt
  open/filter/confirm/cancel, clock advance, LSP injection, FS external-edit,
  and a semantic `Wait`. No variant is a raw `KeyCode` — even mouse projects
  through render state, not crossterm.
- Per-type runners each pair a fallible `check_*` (returning a
  `ScenarioFailure`) with a panicking `assert_*` wrapper: buffer, layout,
  modal, workspace, input, temporal, terminal-IO, persistence, and
  marker-roundtrip runners. `ScenarioFailure` is `Serialize`/`Deserialize` for
  typed CI signal.
- The render snapshot is the Phase-2 layout observable; it is currently
  *minimal* (viewport, hardware cursor, gutter) — richer per-row text/cell data
  is planned (§8).

### 6.2 Always-render evaluation primitive

A single buffer-action runner is the one way a buffer scenario's actions are
evaluated, and it **always renders** — a frame before the first action and
after every action — exactly as the real event loop does. Rationale:
layout-dependent actions (move-down, move-line-end, select-line-end) silently
no-op without a prior render, a footgun that bit a real change. Always-rendering
removes it, lets one unified corpus hold both logical and layout-dependent
scenarios, and means the layout scenario differs from the buffer scenario only
in *what it asserts*. Cost: a few ms per frame, a modest percentage added to the
semantic suite — accepted.

(A stale docstring inside the buffer runner says "the runner never calls
render," but the always-render evaluation it delegates to does. The
meta-testing doc's "always render" describes current behavior; the docstring
predates that decision.)

### 6.3 Scenario taxonomy and phase status

The design enumerates ten scenario types mapped onto the e2e files. Live
status:

| Phase | Type | Status |
|---|---|---|
| 1 | data-model lockdown | landed |
| 2 | `LayoutScenario` (+`LayoutShadow`) | landed (minimal render snapshot) |
| 3 | `ModalScenario` | landed (real modal state from popup manager) |
| 4 | `StyleScenario` | **skeleton** — needs `render()` split into layout/style/emit |
| 5 | `LspScenario` | **skeleton** — needs an LSP-transport seam at the LSP manager |
| 6 | `PersistenceScenario` | landed (real FS via harness temp dir + `FileSystem` trait) |
| 7 | `WorkspaceScenario` | landed |
| 8 | `TerminalIoScenario` | landed (vt100 round-trip) |
| 9 | `InputScenario` | landed minimal (left-click) |
| 10 | `TemporalScenario` | landed (`TestTimeSource`) |
| 11/12 | `PluginScenario`/`GuiScenario` | **dropped** — low test volume vs. heavy hooks |

Skeleton runners are honest: their `check_*` panics with the precise production
hook still needed rather than silently passing. The data shapes (LSP traffic,
styled frame, cell role) already exist and serialize into the corpus, so the
JSON schema is stable ahead of the runner.

The two still-blocked phases (4, 5) require real production refactors; phases
6/8/10 *appeared* blocked but production already had the right traits
(`FileSystem`, the vt100 parser, `TimeSource`) — only the runner needed wiring.

### 6.4 Migrated suite

The semantic tests already hold many `migrated_*` files plus domain files,
spanning buffer/cursor/selection, multi-cursor, undo/redo (including bulk-edit
and save-point boundary), auto-indent, case conversion, unicode/grapheme,
paste round-trip, search-modal flows, workspace buffer-count, dabbrev,
line-wrap/scroll layout, side-by-side diff, virtual lines/margins, and
numbered issue regressions. Migration found and pinned a catalogue of real
behavioral asymmetries — e.g. a move-line-end screen-column off-by-one, an
upper-case-without-selection upcasing the word under cursor, macro playback
becoming a single undo group. The discipline: pin observed behavior even when
surprising, add a finding entry, so an intentional change must update both.

## 7. Shadow-model framework (corpus differential)

A shadow model is an alternate implementation of `step: BufferState × Action →
BufferState`. The corpus differential runs every applicable scenario through
both the live editor and each shadow and asserts equal observables, reporting
typed shadow-disagreement failures.

- The `ShadowModel` trait exposes a name, a capabilities query, and an
  evaluate step over initial text plus actions. The capabilities let the runner
  skip scenarios a shadow can't simulate (e.g. a pure-state shadow skips
  layout-dependent cursor moves).
- Implemented today: only a buffer shadow, an **identity** shadow that re-runs
  through the live editor. Structurally a no-op, but it exercises the whole
  plumbing (capability filter → evaluate → field-by-field compare → typed
  disagreement) so the wiring is proven before the first real reference shadow
  ships.
- Layout shadow: a naive-wrap differential is implemented (Phase 2, landed).
- Planned: a rope shadow, multi-cursor shadow, undo shadow, and style shadow —
  each declares capabilities and is auto-picked-up by the corpus loop. Today's
  proptest shadow-model files are intended to fold into these.

### 7.1 Corpus

The corpus is a hand-curated, machine-readable list of buffer-scenario values.
A dump test serializes the whole corpus to JSON (an ignored test run explicitly
in CI) and a *gating* round-trip test ensures a schema change that breaks
deserialization fails even when the dump isn't run. A separate driver runs the
corpus-wide differential.

## 8. Meta-testing & migration efforts

The meta-testing design adds a **fourth driver**: tests *about the tests*,
gated behind an environment flag so the normal fast path is unchanged.

- **Minimization:** delta-debug (ddmin) the action vector. A minimal length of
  zero means the expectation holds with no actions — a vacuous/FAKE test (loud
  flag); a minimal length much smaller than the original means setup bloat.
  Advisory report, not a hard gate (yet — see open questions).
- **Combination with active reset:** instead of a fresh harness, drive
  *reversing* actions (escape, remove-secondary-cursors, select-all, retype,
  move-document-start) on one long-lived harness, then run `S1; reset; S2;
  reset; …` under random permutations. Surfaces ambient-state leaks a
  fresh-harness model can never see. Scoped to buffer-layer
  text/cursor/selection scenarios — active reset can't clear undo log, modified
  flag, config, markers, or clipboard.
- **Deferred:** cross-driver agreement (live vs. shadow) and `cargo-mutants` on
  production code. `cargo-mutants` is partially wired via a fast-mutants script
  that excludes a known list of slow/timed-out tests to keep each per-mutant
  cycle fast.

The `anti_*` convention (one per migrated file, enforced by the
migration-convention linter) is the per-file complement to corpus-wide
minimization: it asserts the scenario's `check_*` returns an error when the
load-bearing action is dropped, guarding against silently-inert migrations.

## 9. Integration and specialized tests

- **Client/server:** real-socket integration over the client-connection /
  socket-paths types — handshake/protocol version, session lifecycle, idle
  timeout, reconnection. Uses a read-until-contains polling helper (no
  wall-clock timeout; relies on nextest's external timeout) and PID+nanos-unique
  session names for parallel isolation.
- **Property/oracle:** proptest shadow-model files compare the piece-tree
  buffer against a `Vec<u8>` oracle; persisted regression files retain shrunk
  counterexamples. Undo/redo marker round-trip and persistence/agent property
  tests are similar.
- **Remote/SSH:** remote and ssh-attach tests; CI installs an SSH server and
  spins a throwaway non-root sshd on localhost; tests self-skip if absent.
- **Fakes:** a Bash-script JSON-RPC server that the real LSP manager connects
  to over stdin/stdout — usable but flaky, which is *why* Phase 5 wants an
  in-process transport seam. CLI shims provide a fake language server and a fake
  devcontainer for interactive flows.
- **Scene parity:** a scene-parity test drives the *same* editor through the web
  bridge and asserts the web scene's chrome also appears in TUI cells —
  guarding the single-source-of-truth model behind non-terminal UI.
- **Stress:** a stress script reproduces the parallel embedded-plugin-extraction
  race; another benchmarks serial-lag.

## 10. Visual regression

Two coexisting approaches:

1. **Current (byte/SVG snapshots):** dedicated e2e files capture rendered
   ratatui buffers into per-flow markdown plus image metadata. Checked-in
   references live under the visual-regression docs (SVG plus step docs). A
   dedicated CI job renders before/after galleries for any PR touching themes,
   running an ignored theme-diff gallery test and uploading the gallery as an
   artifact. Frame-to-GIF and asciinema-recording scripts produce blog/showcase
   animations.
2. **Planned (`StyleScenario`, Phase 4):** the design replaces byte-for-byte
   theme snapshots with a style scenario over a styled frame of role-tagged
   cells, diffed structurally as JSON. The acceptance criterion includes
   deleting the byte-snapshot pipeline. **Not yet implemented** — blocked on the
   `render()` → layout/style/emit refactor.

## 11. CI structure

The main CI workflow runs on PR plus pushes to the main branches, with
concurrency cancellation:

- `fmt` — formatting check.
- `clippy` — clippy across all features and targets; deliberately *not*
  `-D warnings` (a toolchain bump that adds pedantic warnings shouldn't redden
  the build); only error-level diagnostics fail, including the crate's deny of
  let-underscore-must-use.
- `doc` — docs build on nightly with the docsrs cfg.
- `schema` — regenerates the config JSON schema and diffs it against the
  checked-in file.
- `check-no-plugins` — a check with default features off and only the runtime
  feature.
- `test` — matrix over ubuntu/macos/windows, running nextest with all features
  and targets, no-fail-fast, locked. Linux runs under `xvfb-run` with lavapipe
  software rendering for headless GPU tests, and installs an SSH server for the
  remote-SSH test.

The semantic-isolation and migration-convention lints and the meta-testing /
mutation pass are described as CI jobs in the docs but are **not** present in
the committed main CI workflow; the scripts exist, but their CI wiring is not in
that workflow.

Other workflows are release/packaging and docs deployment, not test gates.

## 12. Certain clarifications

- **No JS-based e2e.** The root `package.json`, `bun.lock`, and
  `package-lock.json` are for the VitePress docs site (the docs scripts), not
  test runners. The only JS test is a standalone Node unit test for a
  merge-conflict regex (run with `node`, not in CI). There is no JS/bun e2e
  suite.
- **Mock clock vs `TestTimeSource`:** the migration design names a "MockClock";
  the implemented determinism type is `TestTimeSource` (§4). Same concept,
  different name.
- **Always-render docstring:** a stale docstring inside the buffer runner
  ("runner never renders") contradicts the always-render decision and the actual
  runner body (§6.2).
- **CI lint jobs** (semantic isolation, migration conventions, mutation pass):
  the scripts exist; their wiring into the main CI workflow is absent.

## 13. Net direction

The trajectory is: imperative, terminal-coupled e2e → headless, typed,
data-as-tests scenarios that triple-leverage each written test (regression +
proptest seed + shadow differential). Determinism comes from `TestTimeSource`
(time), per-harness temp dirs / the `FileSystem` trait (FS), sorted observables
(hash-order), and semantic waits (no wall-clock sleeps). The remaining blockers
are two production refactors — split `render()` for the style scenario, add an
LSP transport seam for the LSP scenario — after which visual regression and LSP
join the same unified corpus and the imperative/semantic split dissolves.
