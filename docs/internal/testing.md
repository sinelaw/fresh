# Testing Architecture

Purpose: map Fresh's testing infrastructure — its layers, the headless
scenario framework, the determinism strategy, the ANSI capture backend, and
the meta-testing/migration efforts — at the code level, distinguishing what is
implemented from what is planned.

## 1. Testing layers (overview)

Fresh layers tests by faithfulness vs. speed/coupling. From lowest coupling
(fast, refactor-proof) to highest (slow, terminal-faithful):

| Layer | Where | Drives | Asserts on |
|---|---|---|---|
| Unit / `#[cfg(test)]` | inline in `src/**` | direct calls | internal invariants |
| Property (proptest) | `tests/shadow_model_*.rs`, `tests/property_*.rs` | random op sequences | buffer == `Vec<u8>` oracle |
| Semantic scenarios | `tests/semantic/**` | `EditorTestApi` (headless) | typed observables |
| Imperative e2e | `tests/e2e/**` (250 files) | `EditorTestHarness` send_key/render | screen scrape + buffer |
| Integration | `src/server/tests.rs`, `tests/*_tests.rs` | real sockets / FS / SSH | end-to-end behavior |
| Visual regression | `tests/e2e/{theme_screenshots,blog_showcases,visual_regression}.rs` + CI | rendered frames → SVG/byte | pixel/cell snapshots |

The active architectural direction is to collapse `tests/e2e/**` (screen
scraping, terminal IO, render timing) into `tests/semantic/**` (typed
observables over a headless editor). That migration is the central story of
this doc; see §3, §6.

### 1.1 The two top-level docs

- `docs/internal/testing.md` — the older contributor-facing guide. Describes
  `EditorTestHarness`, shadow validation, proptest, "no timeouts / e2e over
  unit / reproduce before fixing." Still accurate for the imperative e2e
  regime; predates the scenario framework.
- `docs/internal/e2e-test-migration-design.md` — the design for the scenario
  framework that supersedes the imperative regime. The remaining migration
  plan lives in `scenario-migration-status.md`; behavioral findings in
  `scenario-migration-findings.md`; meta-testing in `scenario-meta-testing.md`.

## 2. The imperative e2e harness (`EditorTestHarness`)

`crates/fresh-editor/tests/common/harness.rs` (2786 lines) is the workhorse of
the older regime and still backs every scenario runner underneath.

- Construction: `EditorTestHarness::new(w,h)` / `with_temp_project(w,h)` /
  `create(w,h, HarnessOptions)` (`harness.rs:549`). `HarnessOptions`
  (`harness.rs:200`) toggles plugins, working dir, project root, a custom
  `FileSystem`, slow-FS injection (`with_slow_fs`), a fake devcontainer, and
  grammar registry.
- Input: `send_key`, `type_text`, `send_paste`, mouse helpers
  (`mouse_click`, `mouse_drag`, `mouse_scroll_*`) — all route through the
  production key/mouse handlers.
- Render: `render()` draws to a ratatui buffer; `render_real()` drives a real
  `CrosstermBackend` whose bytes are parsed back through an embedded
  `vt100::Parser` (`harness.rs:525`) for terminal-faithful assertions
  (`vt100_screen_to_string`, `vt100_cursor_position`).
- Determinism hooks: `advance_time(d)` / `sleep(d)` (`harness.rs:932`) drive a
  `TestTimeSource` (§4); `time_source()` exposes it. `fs_metrics()` /
  `get_fs_total_calls()` count FS hits for slow-FS tests.
- `api_mut()` returns `&mut dyn EditorTestApi` — the bridge to the scenario
  framework (§3).

### 2.1 Shadow validation (in-harness)

`enable_shadow_validation()` (`harness.rs:1025`) makes the harness maintain a
plain `String` that mirrors every edit; `assert_buffer_content` then checks
both the real piece-tree buffer and the shadow agree. Cheap differential
catch for piece-tree bugs. This is distinct from the scenario `ShadowModel`
framework (§7), which is a corpus-wide differential.

## 3. The semantic test API (`EditorTestApi`)

`crates/fresh-editor/src/test_api.rs` is the **single observation surface** for
semantic tests. The contract (file docstring, `test_api.rs:1-30`): semantic
tests bind *only* to this module — never to `crate::app::Editor`,
`crate::model::*`, or `crate::view::*`. Production internals can be refactored
freely; this trait is the only thing that must stay stable.

- `trait EditorTestApi` (`test_api.rs:142`) is implemented on
  `crate::app::Editor` (`test_api.rs:418`). The impl lives in the same file as
  the trait + projection types so the entire test-facing surface reviews as
  one unit.
- Projection types — `Caret`, `ModalSnapshot`/`PopupView`/`PromptView` — are
  pure `Serialize`/`Deserialize` snapshots (`test_api.rs:39-105`). They carry
  only the fields tests assert on; internal cursor fields (`sticky_column`,
  `block_anchor`) are intentionally hidden so production refactors don't break
  tests.
- Determinism baked in: `carets()` sorts by ascending byte position so tests
  don't depend on internal `HashMap` iteration order (`test_api.rs:21-24`,
  `448`).
- Drive surface: `dispatch(Action)` and `dispatch_seq(&[Action])`
  (`test_api.rs:419-431`) route through `dispatch_action_for_tests` (the same
  `handle_action` path the input layer uses) and then drain async messages —
  so a dispatched action settles its plugin/LSP/async follow-ups before the
  next observation.

The observable surface is organized into "classes," each tagged by the
migration phase that added it (`test_api.rs:152-407`):

| Class | Observables | Phase |
|---|---|---|
| A — pure state | `buffer_text`, `primary_caret`, `carets`, `selection_text`, `is_modified` | 1 |
| B — layout | `viewport_top_byte`, `terminal_{width,height}`, `gutter_width`, `hardware_cursor_position`, `visible_byte_range`, `top_line_number`, `primary_scrollbar_geometry` | 2 |
| C — modal | `modal_snapshot` (popup stacks + minibuffer prompt) | 3 |
| D — workspace | `buffer_count`, `active_buffer_path`, `buffer_paths` | 7 |
| E — input | `dispatch_mouse_click`, `take_full_redraw_request_for_tests` | 9 |
| F — markers | `seed_marker`, `marker_positions`, `notify_file_changed`, `active_event_log_len` | — |
| G — composite diff | `create_side_by_side_diff`, hunk-nav, `flush_layout_for_tests` | — |
| H — layout seeding | `seed_virtual_line`, `add_margin_annotation`, `status_message`, `margin_left_total_width` | — |

Notable design points encoded in the API:
- `buffer_text()` panics on large-file (unloaded-region) buffers
  (`test_api.rs:433-438`) — semantic theorems are deliberately not the tool for
  large-file scenarios.
- `modal_snapshot()` projects two distinct modal channels: the popup stacks
  (`global_popups` + per-window `popups`) **and** the minibuffer `prompt` on
  `active_window().prompt` (`test_api.rs:535-596`). The prompt projection is
  load-bearing: without it, modal scenarios that drive `CommandPalette`/
  `QuickOpen`/`Search` flows would "pass by tautology" (docstring `test_api.rs:54-61`).
- Every accessor is additive and gated behind `#[cfg(any(test, feature =
  "test-api"))]` per the migration design §2.1 — never reachable from the
  production binary.

### 3.1 Enforcing the contract (CI lints)

Two shell linters run in CI (and locally) to keep the contract honest:

- `scripts/check-semantic-test-isolation.sh` — forbids `tests/semantic/**` from
  importing `fresh::{app,input,services,config_io,state,workspace}` outright.
  `crossterm::*`, `fresh::model`, `fresh::view`, `fresh::config::` are allowed
  *only* in "harness-direct" files that explicitly `use
  crate::common::harness::EditorTestHarness` (the documented escape hatch for
  cross-state claims and projection types with no API counterpart, e.g.
  `MarkerId`).
- `scripts/check-semantic-migration-conventions.sh` — every
  `tests/semantic/migrated_*.rs` must (1) cite its `tests/e2e/<file>.rs`
  source in the `//!` docstring and (2) ship at least one `anti_*` test (see
  §6.2).

## 4. Determinism: the TimeSource abstraction

`crates/fresh-editor/src/services/time_source.rs`. Design decision #1
(`docs/internal/design-decisions.md:42`), formerly `timesource-design.md`.

**Problem:** wall-clock time makes tests slow and non-deterministic.

**Decision:** a `TimeSource` trait (`time_source.rs:18`) with two impls:
- `RealTimeSource` (`time_source.rs:41`) — `Instant::now()` / real
  `thread::sleep` / `Utc::now()`.
- `TestTimeSource` (`time_source.rs:91`) — holds an `AtomicU64` of logical
  nanoseconds since creation. `now()` returns `base_instant + logical_elapsed`;
  `sleep(d)` does **not** sleep, it just `advance(d)`s the counter
  (`time_source.rs:152`); `today_date()` derives a calendar date from logical
  days elapsed. So time-driven code (animations, debounces, auto-save) runs
  instantly yet observably "passes" time.

**Trade-off — selective, not total, abstraction** (`design-decisions.md:50-54`):
abstract time where it's testable; keep real time where it fundamentally must
be (the main loop's `crossterm::event::poll`, signal-handler sleeps). Services
receive a `SharedTimeSource = Arc<dyn TimeSource>` by composition
(`time_source.rs:37`).

This trait is the production hook that made Phase 10 (`TemporalScenario`) land
with zero new production code: `harness.advance_time(d)` advances the same
`TestTimeSource` the editor already reads, and `InputEvent::AdvanceClock(d)`
routes through it (migration design §10.1 phase 10). Note: the abstraction is
the *already-existing* `TestTimeSource`, not a separate `MockClock` — the
design doc's `MockClock` naming (`e2e-test-migration-design.md:154`,§7.4) is
aspirational; the implemented type is `TestTimeSource`.

## 5. The capture backend (ANSI rendering)

`crates/fresh-editor/src/server/capture_backend.rs` is a `ratatui::Backend`
impl that, instead of writing to a real terminal, captures all output as ANSI
bytes into an in-memory `Vec<u8>` (`capture_backend.rs:13-40`). Its **primary
production role** is the client/server architecture: the server renders into a
`CaptureBackend` and ships the ANSI byte stream to thin clients. It is also the
mechanism behind ANSI-snapshot-style testing.

Key behaviors:
- `draw()` (`capture_backend.rs:212`) walks `(x,y,cell)` triples, emitting a
  cursor-move escape only when the next cell isn't contiguous with the last —
  an optimization that mirrors a real diffing backend, so the byte stream is
  itself a regression target.
- `write_style()` (`capture_backend.rs:78`) does SGR diffing against tracked
  `current_fg/bg/modifiers`, emitting a reset (`ESC[0m`) only when a modifier is
  *removed*, then the minimal set of SGR params. `write_color_params`
  (`capture_backend.rs:172`) handles the 16-color, 256-indexed, and truecolor
  paths. Redundant-SGR / missing-reset bugs are exactly what a byte-level
  snapshot catches.
- `reset_style_state()` (`capture_backend.rs:64`) forces a full repaint when a
  new client connects.
- Cursor visibility (`hide_cursor`/`show_cursor`) *always* emits its escape, no
  optimization, so a reconnecting client is never left out of sync
  (`capture_backend.rs:245-258`).
- `terminal_setup_sequences` / `terminal_teardown_sequences`
  (`capture_backend.rs:335`,`370`) are shared with the direct-mode terminal via
  `services::terminal_modes::sequences`, so capture and real terminal can't
  drift on which modes (alt screen, mouse, focus, bracketed paste) get enabled.
- The module ships ~12 unit tests (`capture_backend.rs:396-512`) asserting the
  exact escape bytes for clear, draw, cursor visibility, and setup/teardown.

### 5.1 vt100 round-trip (`TerminalIoScenario`, Phase 8)

The scenario framework's terminal-faithful layer doesn't assert on raw bytes —
it renders through the *real* `CrosstermBackend`, parses the emitted ANSI back
through `vt100`, and asserts on the resulting `RoundTripGrid` of visible rows +
hardware cursor (`tests/common/scenario/observable.rs:220-249`). This catches
escape-emission and incremental-redraw bugs without committing to a specific
byte sequence — the byte stream is an implementation detail, the displayed grid
is the contract.

## 6. The scenario framework (headless, data-as-tests)

The framework lives in `crates/fresh-editor/tests/common/scenario/` and the
tests in `tests/semantic/**`. Core idea (`scenario/mod.rs:1-13`,
`e2e-test-migration-design.md:1-32`): a test is a **value**
`(initial state, action sequence, expected observable)`, not a script. One
value feeds three drivers with no extra wiring:

```
            Scenario value
          /       |        \
 regression   proptest      shadow-model
   runner     generator     differential
```

### 6.1 Anatomy

- `Observable` trait (`scenario/observable.rs:23`): `extract(&mut
  EditorTestHarness) -> Self`. Pulls a typed, serializable snapshot from a live
  harness after all events dispatch. Implemented for `BufferState`,
  `ModalState`, `WorkspaceState`, `RoundTripGrid`, `StyledFrame`, and tuples
  `(A,B)` for cross-cutting scenarios (`observable.rs:53-59`).
- `InputEvent` (`scenario/input_event.rs`): a superset of `Action` adding
  mouse, prompt open/filter/confirm/cancel, `AdvanceClock`, LSP injection, FS
  external-edit, and semantic `Wait`. No variant is a raw `KeyCode` — even
  mouse projects through render state, not crossterm
  (`e2e-test-migration-design.md:166-187`).
- Per-type runners, each `check_*` (fallible, returns `ScenarioFailure`) +
  `assert_*` (panicking wrapper): `buffer_scenario.rs`, `layout_scenario.rs`,
  `modal_scenario.rs`, `workspace_scenario.rs`, `input_scenario.rs`,
  `temporal_scenario.rs`, `terminal_io_scenario.rs`, `persistence_scenario.rs`,
  `marker_roundtrip_scenario.rs`. `ScenarioFailure` is `Serialize`/`Deserialize`
  for typed CI signal (`scenario/failure.rs`).
- `RenderSnapshot` (`scenario/render_snapshot.rs`) is the Phase-2 layout
  observable; currently *minimal* (viewport, hardware cursor, gutter) — richer
  per-row text/cell data is planned (§8).

### 6.2 Always-render evaluation primitive

`run_buffer_actions` (`buffer_scenario.rs:200`) is the single way a
`BufferScenario`'s actions are evaluated, and it **always renders** — a frame
before the first action and after every action — exactly as the real event loop
does (`scenario-meta-testing.md:124-160`). Rationale: layout-dependent actions
(`MoveDown`, `MoveLineEnd`, `SelectLineEnd`) silently no-op without a prior
render, a footgun that bit a real change. Always-rendering removes it, lets one
unified corpus hold both logical and layout-dependent scenarios, and means
`LayoutScenario` differs from `BufferScenario` only in *what it asserts*. Cost:
~4 ms/frame, ~15% added to the semantic suite (≈133 s → ≈155 s) — accepted.

(Note a doc inconsistency: `buffer_scenario.rs:222` says "the runner never calls
`harness.render()`," but `run_buffer_actions` it delegates to does. The
meta-testing doc's "always render" describes current behavior; the stale
docstring line predates that decision.)

### 6.3 Scenario taxonomy and phase status

The design enumerates ten scenario types mapped onto the 227 e2e files
(`e2e-test-migration-design.md:210-352`). Live status
(`e2e-test-migration-design.md:567-580`, `scenario-migration-status.md:14-22`):

| Phase | Type | Status |
|---|---|---|
| 1 | data-model lockdown | landed |
| 2 | `LayoutScenario` (+`LayoutShadow`) | landed (minimal `RenderSnapshot`) |
| 3 | `ModalScenario` | landed (real `ModalState` from popup manager) |
| 4 | `StyleScenario` | **skeleton** — needs `render()` split into `layout/style/emit` |
| 5 | `LspScenario` | **skeleton** — needs an `LspTransport` seam at `LspManager` |
| 6 | `PersistenceScenario` | landed (real FS via harness temp dir + `FileSystem` trait) |
| 7 | `WorkspaceScenario` | landed |
| 8 | `TerminalIoScenario` | landed (vt100 round-trip) |
| 9 | `InputScenario` | landed minimal (mouse `Click(Left)`) |
| 10 | `TemporalScenario` | landed (`TestTimeSource`) |
| 11/12 | `PluginScenario`/`GuiScenario` | **dropped** — low test volume vs. heavy hooks |

Skeleton runners are honest: their `check_*` panics with the precise production
hook still needed rather than silently passing (`lsp_scenario.rs:1-30`,
`style_scenario.rs:1-6`). The data shapes (`LspTraffic`, `StyledFrame`,
`CellRole`) already exist and serialize into the corpus
(`observable.rs:205-283`) so the JSON schema is stable ahead of the runner.

The two still-blocked phases (4, 5) require real production refactors; phases
6/8/10 *appeared* blocked but production already had the right traits
(`FileSystem`, vt100 parser, `TimeSource`) — only the runner needed wiring
(`e2e-test-migration-design.md:596-631`).

### 6.4 Migrated suite

`tests/semantic/**` already holds ~70 `migrated_*` files plus domain files
(`tests/semantic/mod.rs:1-108`) spanning buffer/cursor/selection, multi-cursor,
undo/redo (incl. bulk-edit + save-point boundary), auto-indent, case
conversion, unicode/grapheme, paste round-trip, search-modal flows, workspace
buffer-count, dabbrev, line-wrap/scroll layout, side-by-side diff, virtual
lines/margins, and numbered issue regressions. Migration found and pinned a
catalogue of real behavioral asymmetries (`scenario-migration-findings.md`) —
e.g. `MoveLineEnd` screen-column off-by-one (#1), `ToUpperCase`-without-
selection upcasing the word under cursor (#2), macro playback now a single undo
group (#13, fixed in #2062). The discipline: pin observed behavior even when
surprising, add a finding entry, so an intentional change must update both.

## 7. Shadow-model framework (corpus differential)

`tests/common/scenario/shadow.rs`. A shadow model is an alternate
implementation of `step: BufferState × Action → BufferState`. The corpus
differential runs every applicable scenario through both the live editor and
each shadow and asserts equal observables, reporting typed
`ScenarioFailure::ShadowDisagreement`s (`shadow.rs:79-139`).

- `trait ShadowModel` (`shadow.rs:50`): `name()`, `supports() ->
  ShadowCapabilities`, `evaluate(initial_text, actions) -> BufferState`.
  `ShadowCapabilities` (`shadow.rs:25`) lets the runner skip scenarios a shadow
  can't simulate (e.g. a pure-state shadow skips layout-dependent cursor moves).
- Implemented today: only `BufferShadow` (`shadow.rs:150`), an **identity**
  shadow that re-runs through the live editor. Structurally a no-op, but it
  exercises the whole plumbing (capability filter → evaluate → field-by-field
  compare → typed disagreement) so the wiring is proven before the first real
  reference shadow ships.
- Layout shadow: `layout_shadow.rs` + `tests/semantic/layout_shadow_diff.rs`
  implement a naive-wrap differential (Phase 2, "landed").
- Planned (`e2e-test-migration-design.md:534-545`): `RopeShadow`,
  `MultiCursorShadow`, `UndoShadow`, `StyleShadow` — each declares
  capabilities and is auto-picked-up by the corpus loop. Today's
  `tests/shadow_model_*.rs` proptest files are intended to fold into these.

### 7.1 Corpus

`tests/semantic/corpus.rs` is the hand-curated machine-readable list of
`BufferScenario` values (`corpus.rs:24`). `corpus_dump.rs` serializes the whole
corpus to `target/scenario-corpus.json` (an `#[ignore]`d test run explicitly in
CI) and ships a *gating* `corpus_round_trips_through_json` test so a schema
change that breaks deserialization fails even when the dump isn't run
(`corpus_dump.rs:24-39`). `shadow_corpus.rs` is the corpus-wide differential
driver.

## 8. Meta-testing & migration efforts

`docs/internal/scenario-meta-testing.md` (accepted 2026-05-20) adds a **fourth
driver**: tests *about the tests*, gated behind `FRESH_MUTATION=1` so the normal
fast path is unchanged.

- **Minimization** (`scenario/minimize.rs`): delta-debug (ddmin, ~O(n log n))
  the action vector. `minimal_len == 0` ⇒ the expectation holds with no actions
  ⇒ vacuous/FAKE test (loud flag); `minimal_len ≪ original_len` ⇒ setup bloat.
  Advisory report, not a hard gate (yet — see open questions).
- **Combination with active reset** (`scenario/reset.rs`,
  `tests/semantic/{combination,reset_isolation}.rs`): instead of a fresh
  harness, drive *reversing* actions (Esc, RemoveSecondaryCursors, SelectAll,
  retype, MoveDocumentStart) on one long-lived harness, then run `S1; reset;
  S2; reset; …` under random permutations. Surfaces ambient-state leaks a
  fresh-harness model can never see. Scoped to buffer-layer text/cursor/
  selection scenarios — active reset can't clear undo log, modified flag,
  config, markers, or clipboard.
- **Deferred:** cross-driver agreement (live vs. shadow) and `cargo-mutants` on
  production code. `cargo-mutants` is partially wired via
  `scripts/mutants-fast.sh`, which excludes a known list of slow/timed-out
  tests to keep each per-mutant cycle fast.

The `anti_*` convention (one per `migrated_*` file, enforced by
`check-semantic-migration-conventions.sh`) is the per-file complement to
corpus-wide minimization: it asserts the scenario goes `check_*_scenario.is_err()`
when the load-bearing action is dropped, guarding against silently-inert
migrations (`scenario-migration-status.md:156-173`).

## 9. Integration and specialized tests

- **Client/server** (`src/server/tests.rs`, 1623 lines): real-socket
  integration over `ClientConnection`/`SocketPaths` — handshake/protocol
  version, session lifecycle, idle timeout, reconnection. Uses
  `read_until_contains` polling (no wall-clock timeout; relies on nextest's
  external timeout) and PID+nanos-unique session names for parallel isolation.
- **Property/oracle:** `tests/shadow_model_tests.rs` and
  `shadow_model_{editor_state,multi_cursor}_tests.rs` use proptest to compare
  the piece-tree buffer against a `Vec<u8>` oracle; `.proptest-regressions`
  files persist shrunk counterexamples. `undo_redo_marker_roundtrip_tests.rs`,
  `property_{persistence,agent}_tests.rs` similar.
- **Remote/SSH:** `tests/remote_*.rs`, `ssh_attach_error.rs` — CI installs
  `openssh-server` and spins a throwaway non-root sshd on localhost; tests
  self-skip if absent (`ci.yml:106-114`).
- **Fakes:** `tests/common/fake_lsp.rs` is a Bash-script JSON-RPC server the
  real `LspManager` connects to over stdin/stdout (`fake_lsp.rs:1-30`) — usable
  but flaky, which is *why* Phase 5 wants an in-process transport seam.
  `scripts/fake-lsp/bin/fake-pylsp` and `scripts/fake-devcontainer/` provide
  CLI shims for interactive flows (`FAKE_DEVCONTAINER_TEST_PLAN.md`).
- **Scene parity:** `tests/scene_parity.rs` drives the *same* `Editor` through
  the web bridge (`webui::{build_editor, apply_step, scene_value,
  render_tui_cells}`) and asserts the web scene's chrome also appears in TUI
  cells — guarding the single-source-of-truth model behind non-terminal UI.
- **Stress:** `scripts/stress-extract-race.sh` reproduces the parallel
  embedded-plugin-extraction race; `scripts/serial_lag_bench.py` for serial-lag.

## 10. Visual regression

Two coexisting approaches:

1. **Current (byte/SVG snapshots):** `tests/e2e/theme_screenshots.rs`,
   `blog_showcases.rs`, `visual_regression.rs`. `tests/common/visual_testing.rs`
   captures rendered ratatui `Buffer`s into per-flow markdown + image metadata
   (`VisualFlow`, `visual_testing.rs:28`). Checked-in references live in
   `docs/visual-regression/{screenshots,tests}` (SVG + `.md` step docs). The
   `theme-screenshots.yml` CI job renders before/after galleries for any PR
   touching `themes/**`, running the `theme_diff_gallery` `#[ignore]`d test and
   uploading the gallery as an artifact. `scripts/frames-to-gif.sh` and
   `record-asciinema/` produce blog/showcase animations.
2. **Planned (`StyleScenario`, Phase 4):** the design replaces byte-for-byte
   `theme_screenshots.rs` with a `StyleScenario` over a `StyledFrame` of
   role-tagged cells, diffed structurally as JSON
   (`e2e-test-migration-design.md:429-437`, §7.3). Acceptance criterion §13
   includes deleting the byte-snapshot pipeline. **Not yet implemented** —
   blocked on the `render()` → `layout/style/emit` refactor.

## 11. CI structure (`.github/workflows/`)

`ci.yml` runs on PR + pushes to main/master/develop, with concurrency
cancellation:

- `fmt` — `cargo fmt --check`
- `clippy` — `cargo clippy --all-features --all-targets`; deliberately *not*
  `-D warnings` (a toolchain bump that adds pedantic warnings shouldn't redden
  the build); only error-level diagnostics fail, including the crate's
  `#![deny(clippy::let_underscore_must_use)]` (`ci.yml:39-46`).
- `doc` — `cargo doc` on nightly with `--cfg docsrs`.
- `schema` — regenerates the config JSON schema and diffs it against the
  checked-in file (`ci.yml:61-71`).
- `check-no-plugins` — `cargo check --no-default-features --features runtime`.
- `test` — matrix over ubuntu/macos/windows, `cargo nextest run -j=4
  --no-fail-fast --locked --all-features --all-targets`. Linux runs under
  `xvfb-run` with lavapipe (`LIBGL_ALWAYS_SOFTWARE=1`, `WGPU_BACKEND=vulkan`)
  for headless GPU tests; installs `openssh-server` for the remote-SSH test.

Note: the semantic-isolation and migration-convention lints
(`scripts/check-semantic-*.sh`) and the `FRESH_MUTATION=1` meta-pass /
`cargo-mutants` nightly are described as CI jobs in the docs but are **not**
present in the committed `ci.yml`; they appear to run as separate/external jobs
or are aspirational. Flag for verification.

Other workflows are release/packaging (`release*.yml`, `linux-packages.yml`,
`*-build.yml`, `aur/winget/flatpak/cargo-publish`) and `deploy-docs.yml`, not
test gates.

## 12. Discrepancies / things to verify

- **No JS-based e2e.** `package.json` + `bun.lock` + `package-lock.json` at the
  repo root are for the **VitePress docs site** (`fresh-docs`, scripts
  `docs:dev`/`docs:build`), not test runners. The only JS test is
  `tests/merge_parser_test.js`, a standalone Node unit test for a merge-conflict
  regex (run with `node`, not in CI). There is no JS/bun e2e suite.
- **`MockClock` vs `TestTimeSource`:** the design doc names `MockClock`; the
  implemented determinism type is `TestTimeSource` (§4). Same concept,
  different name.
- **`buffer_scenario.rs:222` docstring** ("runner never renders") contradicts
  the always-render decision in `scenario-meta-testing.md` and the actual
  `run_buffer_actions` body (§6.2).
- **e2e file count:** the design doc says 227 unique files; the directory
  currently lists 250 entries (`tests/e2e/`) including subdirs — counts drift as
  migration proceeds.
- **CI lint jobs** (semantic isolation, migration conventions, mutation pass):
  scripts exist; their CI wiring is not in `ci.yml`.

## 13. Net direction

The trajectory is: imperative, terminal-coupled e2e → headless, typed,
data-as-tests scenarios that triple-leverage each written test (regression +
proptest seed + shadow differential). Determinism comes from `TestTimeSource`
(time), per-harness temp dirs / `FileSystem` trait (FS), sorted observables
(hash-order), and semantic waits (no wall-clock sleeps). The remaining blockers
are two production refactors — split `render()` for `StyleScenario`, add an LSP
transport seam for `LspScenario` — after which visual regression and LSP join
the same unified corpus and the `tests/e2e` / `tests/semantic` split dissolves.
