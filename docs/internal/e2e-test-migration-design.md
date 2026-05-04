# E2E Test Migration — Composable Scenarios

**Scope:** every file under `crates/fresh-editor/tests/e2e/*` (227 files).
**Owner:** TBD.

## 1. Goal

Replace the imperative e2e harness with a single composable scenario
framework whose tests are values rather than scripts. Every e2e file
migrates to one of twelve scenario types. The framework covers
rendering, LSP, filesystem, mouse, and animations — not just pure
state — by layering observables, not by carving them out.

A scenario is:

```rust
Scenario { description, context, actions, expectation }
```

The same value drives three consumers without any extra wiring:

```
   ┌─────────────────────────────────────────────────────────┐
   │   Scenario  (ScenarioContext + Vec<InputEvent> + Obs)   │
   └─────────────────────────────────────────────────────────┘
        │                    │                     │
        ▼                    ▼                     ▼
   regression test     proptest generator     shadow-model check
   (one example)       (sample / shrink)      (editor ≡ reference)
```

That triple-leverage is the whole reason to do this work.

## 2. Why this beats the existing e2e regime

A migrated test produces three artifacts to today's one:

| Artifact | Today (imperative e2e) | After migration |
|---|---|---|
| Regression check | yes | yes |
| Proptest seed (corpus-guided generation) | no | free |
| Shadow-model differential check | no | free |
| Shrinkable counterexample on failure | no | free (`proptest` shrinks `Vec<InputEvent>`) |
| Serializable for regression file / CI artifact | no | free |
| Replayable across editor versions / branches | no | free |
| Mutation-test target | no | free |
| Cross-feature property check | no | free (the corpus *is* the domain) |
| CI dashboard signal | panic-string parse | typed JSON |

The *write* cost stays roughly constant; the *read* count
multiplies. This is what makes migrating the previously-deferred
categories (Class B viewport, modal UI, LSP, filesystem, rendering)
worth doing — each one joins three drivers simultaneously.

Structural wins beyond leverage:

- **No keymap coupling.** Tests reference `Action::ToUpperCase`, not
  `KeyCode::Char('u') + ALT`. Shortcut moves stop breaking tests.
- **No render coupling for state tests.** Zero `harness.render()`
  calls in scenarios that don't assert on layout.
- **No screen-scraping for logic.** Assertions go through typed
  observables; theme/render churn stops breaking logic tests.
- **Refactor freedom.** Production internals can rename freely; only
  `EditorTestApi` and the shadow `step` change.
- **Less flakiness.** No terminal IO, no async race, no render
  timing in scenarios that don't model them. The flake surface
  shrinks with the imperative surface.

## 3. What's already in place

The framework foundation has shipped. See
`docs/internal/scenario-migration-status.md` for the
forward-looking plan and `scenario-migration-findings.md` for
the running log of behavioral asymmetries surfaced during
migration.

- `crates/fresh-editor/src/test_api.rs` — `EditorTestApi` trait
  with the full per-phase observable surface
  (`dispatch`/`dispatch_seq`, buffer + cursor + selection,
  viewport top byte / hardware cursor / gutter / visible byte
  range, modal snapshot, workspace observables, mouse dispatch,
  `is_modified`).
- `crates/fresh-editor/tests/common/scenario/` — runners for
  `BufferScenario`, `LayoutScenario`, `ModalScenario`,
  `WorkspaceScenario`, `PersistenceScenario`,
  `TemporalScenario`, `InputScenario`, `TerminalIoScenario`.
  `LspScenario` and `StyleScenario` exist as honest skeletons
  whose runners panic with the precise prerequisite (see
  §10.1).
- A growing body of `tests/semantic/**` migrations spanning
  buffer / cursor / selection, multi-cursor, undo (incl.
  bulk-edit + save-point boundary), auto-indent (incl. bracket
  expansion), case conversion, indent / dedent, sort lines,
  duplicate line, toggle comment, unicode cursor + grapheme
  handling, paste (round-trip Copy/Paste), search-modal flows,
  workspace buffer-count, dabbrev word completion, and the
  issue-numbered regressions for #210, #629, #1147, #1258,
  #1305, #1566, #1574, #1577, #1697, plus the
  bulk-edit-after-save undo corruption.
- `Action` is the production input alphabet (`Serialize`).
- `ScenarioFailure` is `Serialize` + `Deserialize`.
- The `tests/semantic/**` lint forbids reaching into
  `fresh::app::*`, `fresh::model::*`, or `fresh::view::*`; only
  `fresh::test_api` and `crate::common::harness::EditorTestHarness`
  are reachable.

Production bugs and behavioral asymmetries have been found and
fixed (or pinned) along the way — see
`docs/internal/scenario-migration-findings.md` for the catalogue.

## 4. Data-model lockdown (Phase 1)

Prerequisite for everything else. One PR, mostly mechanical.

1. Derive `Serialize`/`Deserialize` on `BufferTheorem`,
   `TraceTheorem`, `LayoutTheorem`.
2. Replace `&'static str` with `String` (or `Cow<'static, str>`)
   on the same.
3. Lift `BehaviorFlags`, filename (= language), `TerminalSize` into
   the struct as fields. Delete the runner overloads
   (`assert_buffer_theorem_with_*`).
4. Rename `*Theorem` → `*Scenario` (the Lean ambition is dropped;
   "scenario" is more accurate for what these are).
5. Promote `EvaluatedState` (`property.rs:23`) to the canonical
   `BufferState` observable type.
6. Add a `ShadowModel` trait skeleton + a `BufferShadow` impl that
   delegates to the live editor (no-op differential — proves the
   trait + corpus loop work end-to-end before any real shadow
   ships).
7. CI job: dump the corpus to JSON; fail on schema-breaking diffs
   with no version bump.

**Acceptance:** every existing semantic test continues to pass;
corpus JSON exists; `BufferShadow` runs the corpus and reports
zero disagreements.

## 5. Composable scenario architecture

```rust
pub struct Scenario<Obs: Observable> {
    pub description: String,
    pub context:     ScenarioContext,
    pub actions:     Vec<InputEvent>,
    pub expectation: Obs,
}

pub struct ScenarioContext {
    pub buffer:    BufferContext,         // initial_text, behavior, language, terminal
    pub workspace: Option<WorkspaceContext>,
    pub fs:        Option<VirtualFs>,
    pub lsp:       Option<LspScript>,
    pub plugins:   Option<PluginScript>,
    pub theme:     Option<Theme>,
    pub clock:     Option<MockClock>,
}

/// Anything the runner can extract from a live editor and assert on.
pub trait Observable: Serialize + DeserializeOwned + PartialEq {
    fn extract(api: &mut dyn EditorTestApi) -> Self;
}

pub fn check_scenario<Obs: Observable>(s: Scenario<Obs>)
    -> Result<(), ScenarioFailure>;
```

`InputEvent` is the top-level alphabet. It's a superset of `Action`:

```rust
pub enum InputEvent {
    Action(Action),                  // existing 600-variant editor alphabet
    Mouse(MouseEvent),               // Click(x,y), Drag(start,end), Wheel(dx,dy)
    Compose(ComposeSeq),             // dead keys / IME
    OpenPrompt(PromptKind),          // for ModalScenario
    FilterPrompt(String),
    ConfirmPrompt,
    CancelPrompt,
    AdvanceClock(Duration),          // for TemporalScenario
    LspMessage(LspIncoming),         // server → client injection
    FsExternalEdit(PathBuf, String), // for auto_revert tests
    Wait(WaitCondition),             // semantic wait, never wall-clock sleep
}
```

The seven non-`Action` variants are the price of full coverage.
Each is a typed event the runner dispatches deterministically. No
variant is a `KeyCode` — even mouse events project through the
current `RenderSnapshot`, not through `crossterm`.

Each scenario type from §6 is a type alias / specialization:

```rust
pub type BufferScenario       = Scenario<BufferState>;
pub type LayoutScenario       = Scenario<RenderSnapshot>;
pub type ModalScenario        = Scenario<(BufferState, ModalState)>;
pub type WorkspaceScenario    = Scenario<(BufferState, WorkspaceState)>;
pub type PersistenceScenario  = Scenario<(BufferState, FsState)>;
pub type LspScenario          = Scenario<(BufferState, LspTraffic)>;
pub type StyleScenario        = Scenario<StyledFrame>;
pub type InputScenario        = Scenario<RenderSnapshot>;
pub type TemporalScenario     = Scenario<Vec<RenderSnapshot>>;
pub type TerminalIoScenario   = Scenario<RoundTripGrid>;
```

The runner is a single entry point parameterized by `Obs`; the
specializations exist for ergonomic constructors and for
proptest-strategy specialization, not because the runner branches.

`Observable` is the same interface shadow models implement (§8).

## 6. Scenario taxonomy — covering every e2e

### 6.1 The ten scenario types

| Type | Primary observable | Files (~) |
|---|---|---|
| `BufferScenario` | text + cursors + selection | 50 |
| `LayoutScenario` | `RenderSnapshot` (viewport, gutter, hw cursor) | 32 |
| `ModalScenario` | prompt/palette/picker/menu state | 43 |
| `WorkspaceScenario` | splits, tabs, dock layout, buffer list | 19 |
| `PersistenceScenario` | `VirtualFs` + session/recovery state | 23 |
| `LspScenario` | scripted LSP exchange + buffer | 29 |
| `StyleScenario` | `StyledFrame` (cell role × theme) | 12 |
| `InputScenario` | mouse/composition events as data | 7 |
| `TemporalScenario` | timed sequence of frames (`MockClock`) | 3 |
| `TerminalIoScenario` | ANSI bytes via vt100 round-trip | 7 |

Total ≈ 225; some files are dual-category. Unique e2e file count
is 227.

### 6.2 Per-category file mapping

Representative, not exhaustive. Full file lists belong in the
per-phase tickets.

**`BufferScenario` (~50)** — pure text/cursor/selection. Done:
`case_conversion`, `sort_lines`, `indent_dedent`, `smart_home`,
`duplicate_line`, `toggle_comment`, `unicode_cursor`, `undo_redo`,
`selection`, `auto_pairs`, `save_state`, `emacs_actions`. Pending:
`basic`, `movement`, `paste`, `shift_backspace`, `triple_click`,
`block_selection`, `multibyte_characters`, `smart_editing`,
`tab_indent_selection`, `select_to_paragraph`, `document_model`,
`goto_matching_bracket`, `multicursor`, `undo_redo_marker_roundtrip`,
`undo_bulk_edit_after_save`, `issue_1288_word_select_whitespace`,
`issue_1566_arrow_selection`, `issue_1697_ctrl_d_after_search`,
`search_selection_on_punctuation`, `overlay_extend_to_line_end`,
`search_navigation_after_move`.

**`LayoutScenario` (~32)** — viewport scroll, soft-wrap, gutter,
hardware cursor row/col. Unblocked by `RenderSnapshot` (§7.1).
Files: `issue_1147_wrapped_line_nav`, `scroll_clearing`,
`scroll_wrapped_reach_last_line`, `scrolling`, `line_wrap_*` (5
files), `line_number_bugs`, `search_center_on_scroll`,
`search_*_stall_after_wrap`, `hanging_wrap_indent`,
`horizontal_scrollbar`, `issue_1502_word_wrap_squished`,
`issue_1574_*_scroll`, `virtual_line*`, `popup_wrap_indent`,
`margin`, `vertical_rulers`, `memory_scroll_leak`,
`side_by_side_diff_*`, `markdown_compose*`, `redraw_screen`,
`tab_scrolling`, `folding`, `issue_1571_fold_indicator_lag`,
`issue_1568_session_fold_restore`, `issue_779_after_eof_shade`,
`issue_1790_compose_wrap_highlight`,
`test_scrollbar_keybinds_cursor`.

**`ModalScenario` (~43)** — adds `ModalState` to the observable and
`OpenPrompt`/`FilterPrompt`/`ConfirmPrompt`/`CancelPrompt`/
`MenuSelect` to `InputEvent`. Files: `command_palette`,
`file_browser`, `file_explorer`, `action_popup_global`, `prompt`,
`prompt_editing`, `popup_selection`, `menu_bar`, `menu_*_bleed`,
`explorer_*`, `live_grep`, `search`, `search_replace`,
`lsp_code_action_modal`, `lsp_completion_*`, `dabbrev_completion`,
`status_bar_message_click`, `update_notification`,
`sudo_save_prompt`, `save_nonexistent_directory`, `settings`,
`settings_*` (multiple), `keybinding_editor`, `unicode_prompt_bugs`,
`issue_1718_settings_search_utf8_panic`, `preview_lsp_popup_focus`,
`cursor_under_popup`, `toggle_bars`.

**`WorkspaceScenario` (~19)** — adds `WorkspaceState` to context and
observable. Splits and tabs are addressable as `SplitId`/`TabId`.
Files: `buffer_groups`, `buffer_lifecycle`,
`buffer_settings_commands`, `multi_file_opening`, `preview_tabs`,
`split_focus_tab_click`, `split_tabs`, `split_view`,
`split_view_expectations`, `split_view_markdown_compose`,
`tab_config`, `tab_drag`, `copy_buffer_path`,
`issue_1540_tab_click_focus`, `position_history*` (4 files).

**`PersistenceScenario` (~23)** — adds `VirtualFs` to context (an
in-memory FS the editor reads/writes through a fake adapter) and
`FsState` as observable. Files: `auto_revert`, `encoding`,
`external_file_save_as_tab`, `file_permissions`, `hot_exit_*`,
`large_file_*`, `on_save_actions`, `recovery`,
`save_as_language_detection`, `server_session_lifecycle`,
`session_hot_exit`, `slow_filesystem`, `stdin_input`, `symlinks`,
`unnamed_buffer_persistence`, `workspace`, `open_folder`,
`lifecycle`, `bash_profile_editing`, `binary_file`,
`save_nonexistent_directory` (dual with Modal),
`undo_bulk_edit_after_save` (dual with Buffer).

**`LspScenario` (~29)** — adds `LspScript`: an ordered list of
expected client-to-server messages and pre-written
server-to-client responses. The fake server matches messages by
shape, replies on cue, and records traffic for assertion. Files:
`lsp` and 26 `lsp_*` files; `language_features_e2e`;
`universal_lsp`; `inline_diagnostics`; `issue_1572_inlay_hint_drift`;
`issue_1573_format_buffer`. `hot_exit_recovery_lsp_sync` is dual
(Persistence + LSP).

**`StyleScenario` (~12)** — pulls a `StyledFrame` via the §7
`RenderSnapshot → StyledFrame` projection (theme + role table) and
asserts on cell roles + colors via `Inspect::{Cell, Row, Column,
Region, FullFrame}`. Subsumes today's `theme_screenshots` byte
snapshots with a diffable JSON form. Files: `theme`,
`theme_screenshots`, `blog_showcases`, `cursor_style_rendering`,
`crlf_rendering`, `syntax_highlighting_coverage`,
`syntax_highlighting_embedded_offset`, `syntax_language_case`,
`glob_language_detection`, `config_language_selector`,
`csharp_language_coherence`, `warning_indicators`,
`issue_1554_scrollbar_theme_color`, `issue_1577_unicode_width`,
`issue_1598_shebang_detection`, `issue_779_after_eof_shade`.

**`InputScenario` (~7)** — extends `InputEvent` with `Mouse`,
`Compose`, `KeyChord`. Mouse coordinates project to (line, byte)
via the current `RenderSnapshot`, not through `crossterm`. Files:
`mouse`, `capslock_shortcuts`, `altgr_shift`, `csi_u_session_input`,
`issue_1620_split_terminal_click_panic`, `locale`, `tab_drag` (dual
with Workspace).

**`TemporalScenario` (~3)** — adds `MockClock` to context and
`AdvanceClock(Duration)` to `InputEvent`. Expectation is a
`Vec<RenderSnapshot>` taken after each clock tick. Files:
`animation`, `flash`, `status_bar_config`.

**`TerminalIoScenario` (~7)** — projects `StyledFrame` through the
real escape-sequence emitter, then through `vt100` back to a
typed grid; asserts on the round-trip grid. Catches escape
emission bugs without committing to specific byte sequences.
Files: `ansi_cursor`, `terminal`, `terminal_close`,
`terminal_resize`, `terminal_split_focus_live`, `rendering`,
`redraw_screen` (dual). The harness already does most of this
through `render_real` / `render_real_incremental`; the migration
formalizes it into a scenario type.

**Dropped scenario types — `PluginScenario`, `GuiScenario`.** The
plugin scenarios (~5 files) and `gui.rs` (1 file) were originally
in scope but have been removed: their production hooks (plugin
runtime test entry, wgpu/winit test API) are heavy and the test
volumes don't justify the cost. Plugin test claims that reduce to
buffer state can fold into `BufferScenario` with an optional
context field if/when needed; `gui.rs` stays imperative — its
editor-level claims migrate to `BufferScenario` /
`LayoutScenario`, and the GUI-specific bits (font fallback, IME)
are properly imperative.

### 6.3 Cross-cutting observables

Some files exercise more than one subsystem. Composition is direct
— the scenario carries both context fields and the expectation
type pairs both observables:

| File | Categories | How it composes |
|---|---|---|
| `lsp_code_action_modal.rs` | LSP + Modal | context carries `LspScript`; expectation includes `ModalState` |
| `hot_exit_recovery_lsp_sync.rs` | Persistence + LSP | context carries `VirtualFs` + `LspScript` |
| `tab_drag.rs` | Workspace + Input | context carries `WorkspaceState`; actions include `Mouse::Drag` |
| `issue_1554_scrollbar_theme_color.rs` | Layout + Style | observable is `(RenderSnapshot, StyledFrame)` |

The runner does not branch on type; the scenario's `Observable`
implementation knows how to extract everything it cares about.

## 7. Rendering inside the framework

Rendering is not a separate test regime. The pipeline factors into
four pure-ish layers; each layer has a scenario type; tests target
the highest layer they care about and stop there.

### 7.1 The four rendering layers

```
        EditorState
             │  layout(width, height)
             ▼
       RenderSnapshot       ← LayoutScenario  (theme-free)
             │  style(Theme, RoleTable)
             ▼
        StyledFrame         ← StyleScenario   (role-tagged cells)
             │  emit(Capabilities, EmitState)
             ▼
        AnsiStream          ← (rarely tested directly)
             │  vt100 round-trip
             ▼
       RoundTripGrid        ← TerminalIoScenario
```

Each arrow is a function. None of these layers exists as a named
public type today; building them is the bulk of the rendering-side
work.

| Type | Where | Approx LOC |
|---|---|---|
| `RenderSnapshot` | `crates/fresh-editor/src/test_api.rs` | 300 |
| `StyledFrame` | same | 80 |
| `RoundTripGrid` | same | 60 |
| Layer functions | `src/view/render_layers.rs` (refactored from existing) | ~500 net |

The refactor does not rewrite the renderer. It splits the existing
`render()` body into three named functions:

```rust
fn layout(state: &EditorState, dim: TerminalDim) -> RenderSnapshot;
fn style(snapshot: &RenderSnapshot, theme: &Theme, roles: &RoleTable) -> StyledFrame;
fn emit(frame: &StyledFrame, caps: &Capabilities) -> AnsiStream;
```

Today's `render()` is the composition. Production stays unchanged
(it composes them in one call); tests call them individually.

### 7.2 What each rendering scenario type catches

| Type | Catches | Doesn't catch |
|---|---|---|
| `LayoutScenario` | viewport reconciliation, wrap math, gutter widths, hw cursor row/col, popup placement, scrollbar geometry | colors, glyph choice, escape correctness |
| `StyleScenario` | theme contrast, role-to-color mapping, modifier flags, syntax-highlight color regressions | terminal-emulator quirks |
| `TerminalIoScenario` | escape emission bugs, optimization regressions (e.g., redundant SGR resets), incremental redraw correctness | terminal-side bugs (xterm vs kitty) |
| `TemporalScenario` | animation frame correctness, fade/flash duration, blink phase, scroll smoothing | wall-clock drift |

Together these cover everything except actual terminal-emulator
and GPU-driver behavior, which are correctly outside the editor's
responsibility. (`gui.rs`, the wgpu/winit front-end, stays
imperative — see §6.2.)

### 7.3 Visual regression as a `StyleScenario`

A `StyleScenario` with `Inspect::FullFrame` and `expected:
StyledFrame` loaded from a JSON snapshot file. Diffs are
structural (cell `(x,y)` changed role from `Selection` to `Normal`,
fg `#abc` to `#def`). Snapshot regeneration is a CLI flag on the
test runner. Today's `theme_screenshots.rs` byte-for-byte pipeline
is deleted.

### 7.4 Animations as `TemporalScenario`

```rust
TemporalScenario {
    description: "Flash banner fades over 250ms".into(),
    context: ScenarioContext {
        clock: Some(MockClock::epoch()),
        ..Default::default()
    },
    actions: vec![
        InputEvent::Action(Action::ShowFlash("saved".into())),
        InputEvent::AdvanceClock(Duration::from_millis(50)),
        InputEvent::AdvanceClock(Duration::from_millis(50)),
        InputEvent::AdvanceClock(Duration::from_millis(150)),
    ],
    expectation: vec![
        snapshot_t0_with_banner,
        snapshot_t50_partially_faded,
        snapshot_t100_more_faded,
        snapshot_t250_no_banner,
    ],
}
```

Requires one production hook: `Editor` reads time through a
`Clock` trait, default-impl uses the system clock, test-impl uses
`MockClock`. ~30 LOC of production change, gated behind the
existing `#[cfg(any(test, feature = "test-api"))]`.

### 7.5 Layered shadows

Each layer admits its own shadow:

| Layer | Shadow | Catches |
|---|---|---|
| `step` | reference editor | logic bugs |
| `layout` | naive wrap algorithm | wrap regressions, viewport drift |
| `style` | role-table-driven projection | theme regressions, role-to-color mismatches |
| `emit` | minimal escape emitter | redundant escapes, incorrect cursor positioning |

Each shadow runs on every applicable scenario in the corpus. The
naive wrap shadow alone would have caught `issue_1502` and several
`line_wrap_*` regressions before they shipped — uniform proptest
never finds them because the failing inputs are specific
(double-width chars at exactly column `width-1`); the shadow finds
them on the first scenario that hits the case.

## 8. Shadow model framework

One trait, multiple impls, every applicable scenario auto-checked.

```rust
pub trait ShadowModel {
    /// Subset of `EditorTestApi` this shadow can simulate. The
    /// runner skips scenarios whose context references subsystems
    /// the shadow doesn't claim to handle.
    fn supports(&self) -> ShadowCapabilities;

    fn dispatch(&mut self, event: &InputEvent);

    fn extract<O: Observable>(&self) -> O;
}

pub struct ShadowCapabilities {
    pub buffer:    bool,
    pub workspace: bool,
    pub fs:        bool,
    pub lsp:       bool,
    pub layout:    bool,   // can produce RenderSnapshot
    pub style:     bool,   // can produce StyledFrame
}
```

The differential test:

```rust
#[test]
fn corpus_agrees_with_buffer_shadow() {
    let shadow = BufferShadow::new();
    for scenario in corpus::iter().filter(|s| BufferShadow::handles(s)) {
        check_scenario_against_shadow(&scenario, &shadow)
            .expect("shadow disagreement");
    }
}
```

Adding a new shadow:

1. Implement `ShadowModel` for the alternate semantics or
   alternate algorithm.
2. Declare which scenario types it supports via
   `ShadowCapabilities`.
3. The corpus-wide differential test picks it up automatically.

Shadows live in `tests/common/shadows/`:

| Shadow | Supports | Purpose |
|---|---|---|
| `BufferShadow` | buffer | reference editor; catches actions.rs / state.rs class bugs |
| `LayoutShadow` | buffer, layout | naive wrap algorithm; catches wrap-table regressions |
| `StyleShadow` | layout, style | role-driven projection from `RenderSnapshot` to `StyledFrame` |
| `RopeShadow` | buffer | text in `Vec<u8>` not the production rope; catches rope bugs |
| `MultiCursorShadow` | buffer | naive cursor merge; cross-checks production merge |
| `UndoShadow` | buffer | snapshot-stack undo; cross-checks action-trace undo |

Today's `tests/shadow_model_*.rs` files become `ShadowModel` impls
and are deleted from the bespoke test files (the corpus loop
subsumes them).

## 9. Corpus-guided proptest

Once Phase 1 lands and the corpus exists, build a proptest
strategy that samples scenario prefixes from the corpus and
generates random tails. Run as a soak job in CI.

Counterexamples write `tests/semantic/regressions/bug_NNNN.json`
files automatically. The next CI run loads them as permanent
scenarios. No source change per regression.

This thread runs in parallel with the migration phases and starts
paying off immediately — it does not block any phase.

## 10. Implementation roadmap

Phase 1 is on the critical path. Phases 2–12 are independent and
parallelizable; ordering below is by ROI.

### 10.1 Status (live)

| # | Phase | Status | Production hook |
|---|---|---|---|
| 1 | Data-model lockdown | **landed** | none beyond `EditorTestApi` extensions |
| 2 | `RenderSnapshot` + `LayoutScenario` | **landed** (minimal `RenderSnapshot`: viewport, hw cursor, gutter); `LayoutShadow` differential live | `EditorTestApi`-only |
| 3 | `ModalScenario` | **landed** (real `ModalState` from `PopupManager`) | `EditorTestApi::modal_snapshot` |
| 4 | `StyleScenario` | **skeleton** | needs `style()` extracted from `render()` ‹real production refactor› |
| 5 | `LspScenario` | **skeleton** | needs fake LSP transport adapter at `LspManager` boundary ‹real production refactor› |
| 6 | `PersistenceScenario` | **landed** (real-FS via harness temp dir; routes through the existing `model::filesystem::FileSystem` trait + `StdFileSystem` impl) | none beyond `EditorTestHarness::temp_dir_path`; an in-memory `FileSystem` impl would replace temp dir for speed but adds zero coverage |
| 7 | `WorkspaceScenario` | **landed** (real `WorkspaceState` from buffer map) | `EditorTestApi::buffer_count`/`active_buffer_path`/`buffer_paths` |
| 8 | `TerminalIoScenario` | **landed** (real `RoundTripGrid` via existing vt100 parser) | `EditorTestHarness::vt100_cursor_position` |
| 9 | `InputScenario` | **landed minimal** (mouse Click(Left) routes through `Editor::handle_mouse`) | `EditorTestApi::dispatch_mouse_click` |
| 10 | `TemporalScenario` | **landed** (`AdvanceClock(d)` calls `harness.advance_time(d)` which advances the existing `services::time_source::TestTimeSource` the editor already reads) | none — the `TimeSource` trait + `TestTimeSource` already exist |
| ~~11~~ | ~~`PluginScenario`~~ | **dropped** (low test volume vs heavy production hook); fold into `BufferScenario` if needed | n/a |
| ~~12~~ | ~~`GuiScenario`~~ | **dropped** (`gui.rs` stays imperative; editor-level claims covered by `BufferScenario`/`LayoutScenario`) | n/a |

### 10.2 What "landed" means here

For phases 2/3/6/7/8/9: the runner is real, the data model is
real, JSON round-trips through a gating test, and an end-to-end
proof in `tests/semantic/phase_proofs.rs` exercises the runner
against the live editor. Migrations of existing e2e files
proceed file-by-file from this baseline.

Each test-side accessor added to `EditorTestApi` is gated behind
`#[cfg(any(test, feature = "test-api"))]` per the original §2.1
contract — additive, never removed, never reachable from
production binaries.

### 10.3 What's blocked on production refactors

Two phases (4, 5) and one phase's optional form (11) need real
changes to production code, not just additive test-side
accessors:

- **Phase 4 (`StyleScenario`)** needs `render()` factored into
  named `layout` / `style` / `emit` functions so the cell-role ×
  theme projection is invocable from tests in isolation.
- **Phase 5 (`LspScenario`)** needs a transport-level seam in
  `LspManager` so a fake adapter can intercept JSON-RPC. The
  existing `tests/common/fake_lsp.rs` is a Bash subprocess that
  the real `LspManager` connects to over stdin/stdout — usable
  but flaky and not script-aware.

Three phases that *appeared* blocked turned out not to be —
production already had the right traits and only needed the
scenario runner wired through:

- **Phase 6 (`PersistenceScenario`)** uses the existing
  `model::filesystem::FileSystem` trait. Today's runner uses the
  real-FS impl (`StdFileSystem`) under a per-harness tempdir; the
  trait was already abstracted, so swapping in an in-memory
  `MemoryFileSystem` is a pure additive impl whenever the speed
  matters.
- **Phase 10 (`TemporalScenario`)** uses the existing
  `services::time_source::TimeSource` trait + `TestTimeSource`.
  `AdvanceClock(d)` routes through `harness.advance_time(d)`,
  which is the same path animations / debounces / auto-save
  already consult.
- **Phase 8 (`TerminalIoScenario`)** uses the existing vt100
  parser the harness already wires through `render_real()`.

Until the still-blocked phases land, the corresponding scenario
types ship as **honest skeletons**: data shape + JSON round-trip
work, the runner panics with the precise blocker message naming
the production hook still needed.

### 10.4 Sequencing

Estimated effort per non-trivial phase: 2–4 weeks for one
engineer. Total ≈ 6 person-months for the framework + 3 for
migrations, parallelizable.

## 11. Risks

- **`ScenarioContext` becomes a god object.** Mitigation: fields
  are `Option<...>`; a buffer-only scenario carries only
  `BufferContext`. JSON schema enforces presence iff the runner
  needs it.
- **Fake LSP / `VirtualFs` drift from real subsystems.**
  Mitigation: the imperative e2e files for those subsystems stay
  for one release after each phase ships. Differential testing
  between fake and real catches drift before retirement.
- **`InputEvent` enum grows unmaintainable.** Mitigation: keep
  `Action` separate; only add new variants when a scenario type
  legitimately needs them. The seven non-`Action` variants are
  believed to be the ceiling, not a starting point.
- **Snapshot churn on `RenderSnapshot` schema changes.**
  Mitigation: snapshot files use `serde_json` with
  `#[serde(default)]` on additive fields; schema changes are
  reviewed as data-model changes, not as test churn.
- **Corpus-guided proptest finds bugs that aren't in any migrated
  scenario but block CI.** Mitigation: the soak job is
  non-blocking; found bugs become regression JSON files plus a
  separate gating test if the underlying bug is to be fixed.

## 12. Non-goals

- **Theorem-prover export.** Considered and rejected. The data
  form is for proptest + shadow leverage, not Lean. Removing this
  constraint drops several requirements (formal `step` semantics,
  encoded unicode tables, refinement proofs).
- **Replacing the rope buffer with a verified one.** Out of scope;
  the rope is the production subject.
- **GPU/driver-level GUI tests.** wgpu rendering quality is wgpu's
  problem.
- **Terminal-emulator-level tests** (xterm vs kitty vs alacritty).
  We test the editor's *output*, not its consumers.
- **100% migration of `gui.rs`.** GUI-specific assertions may
  remain imperative if the single file doesn't justify a scenario
  type.

## 13. Acceptance criteria

The migration is "done" when:

- [ ] `tests/e2e/` either contains zero files or only the small
      set kept as redundant terminal-side proofs and the
      GUI-specific subset (per §6.1 and §12).
- [ ] `tests/semantic/` contains all twelve scenario types with
      at least one example per type.
- [ ] The corpus dumps to a JSON directory in CI artifacts on
      every run.
- [ ] At least three shadow models are wired into the
      corpus-differential CI job.
- [ ] Corpus-guided proptest runs as a soak job; counterexamples
      produce regression JSON files automatically.
- [ ] `theme_screenshots.rs` byte-snapshot pipeline is deleted.
- [ ] The split between `tests/e2e/` (renders) and
      `tests/semantic/` (doesn't) no longer exists; rendering is
      tested via §7's layered scenarios within the same framework.
- [ ] `CONTRIBUTING.md` is updated to describe the scenario-type
      taxonomy as the primary test idiom; terminal-side e2es are
      documented as redundant proofs allowed where useful.
