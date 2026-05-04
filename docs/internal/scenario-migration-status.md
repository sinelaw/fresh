# E2E → Scenario Migration: Forward Plan

A plan for the remaining work to retire `tests/e2e/` in favor of
`tests/semantic/`. See `e2e-test-migration-design.md` for the
design and `scenario-migration-findings.md` for behavioral
findings surfaced along the way.

The "done" criterion comes from the design's §13: every e2e
file is migrated, the e2e directory either disappears or holds
only redundant terminal-side proofs, and `CONTRIBUTING.md`
documents the scenario-type taxonomy as the primary test idiom.

## Where the work is right now

- **Phases 1, 2, 3, 6, 7, 8, 9, 10 are landed.** Migrations
  using `BufferScenario`, `LayoutScenario`, `ModalScenario`,
  `WorkspaceScenario`, `PersistenceScenario`,
  `TerminalIoScenario`, `InputScenario`, and `TemporalScenario`
  flow with no production refactor.
- **Phase 4 (`StyleScenario`) and Phase 5 (`LspScenario`) are
  still skeletons.** Both block sizable clusters of tests until
  their production hooks land.

## Categories that are clean to migrate today

Buffer / cursor / selection (incl. multi-cursor, block-select,
clipboard round-trip), undo / redo (incl. BulkEdit + save-point
boundary), auto-indent (incl. issue #629 bracket expansion),
goto-matching-bracket, arrow keys with selection, search-modal
flows (`Action::Search` + `InsertChar` + `PromptConfirm`),
unicode grapheme handling, wrapped-line viewport drift,
GotoLine, workspace buffer-count / active-path / Close /
NextBuffer, dabbrev word completion.

For any remaining e2e file that fits these categories, follow
the patterns below.

## Categories that are blocked

These are roughly ordered by "how much it would unblock":

### High leverage

- **Per-row screen-text inspection.** A large cluster
  (`scrolling.rs`, `line_wrap_*.rs`, `line_wrapping.rs`,
  `markdown_compose*.rs`, `horizontal_scrollbar.rs`,
  `popup_wrap_indent.rs`, `virtual_lines*.rs`,
  `redraw_screen.rs`, `tab_scrolling.rs`, `side_by_side_diff_*`,
  etc.) asserts on rendered glyphs at specific cells. Unblock
  by extending `RenderSnapshot` with per-row `Vec<CellText>`
  (text only — no styling) and exposing it via
  `EditorTestApi`. Estimated cost: 1–2 days. Pure additive
  test-side accessor; no production refactor.

- **`StyleScenario` (Phase 4).** Refactor `render()` into
  named `layout` / `style` / `emit` functions so the
  cell-role × theme projection is invocable from tests in
  isolation. Unblocks the theme / syntax-highlight / cell-color
  cluster (`theme.rs`, `theme_screenshots.rs`,
  `cursor_style_rendering.rs`, `crlf_rendering.rs`,
  `syntax_highlighting_*.rs`, `glob_language_detection.rs`,
  `config_language_selector.rs`, `csharp_language_coherence.rs`,
  `warning_indicators.rs`, `blog_showcases.rs`,
  `issue_1554_scrollbar_theme_color.rs`,
  `issue_1577_unicode_width.rs` cell-render half,
  `issue_1598_shebang_detection.rs`,
  `issue_779_after_eof_shade.rs`, `visual_regression.rs`,
  `overlay_extend_to_line_end.rs`, `vertical_rulers.rs`,
  `margin.rs`). Estimated cost: 2–3 days in `src/view/`.

- **`LspScenario` (Phase 5).** Add an `LspTransport` trait at
  the `LspManager` boundary so a scripted in-process adapter
  can intercept JSON-RPC. Unblocks the entire `lsp_*` cluster
  (`lsp.rs` + ~26 `lsp_*.rs` files, `language_features_e2e.rs`,
  `universal_lsp.rs`, `inline_diagnostics.rs`,
  `issue_1572_inlay_hint_drift.rs`,
  `issue_1573_format_buffer.rs`,
  `hot_exit_recovery_lsp_sync.rs`). Estimated cost: 1–2 days
  in `src/services/lsp/`.

### Medium leverage

- **Split-view + tab state.** `WorkspaceScenario` covers
  buffer-count and active-path but not split layout / tab
  order / per-split focus. Adding a split-tree observable
  unblocks `split_view*.rs` (3 files), `split_tabs.rs`,
  `split_focus_tab_click.rs`, `tab_drag.rs`, `tab_config.rs`,
  `preview_tabs.rs`, `buffer_groups.rs`, the advanced cases of
  `buffer_lifecycle.rs`, `issue_1540_tab_click_focus.rs`,
  `issue_1620_split_terminal_click_panic.rs`,
  `position_history*.rs`. Estimated cost: 1–2 days in
  `src/test_api.rs` + scenario context.

- **File-explorer state.** `Action::FileExplorer*` already
  exists; expose a `FileExplorerState` observable
  (visible nodes, expanded folders, scroll offset, focus).
  Unblocks `file_explorer.rs`, `file_browser.rs`,
  `explorer_*.rs`, `issue_1569_explorer_auto_expand.rs`.

- **Folding observables.** Display + restoration claims need
  fold-marker rows in `RenderSnapshot` plus a fold-state field
  in the persistence observable. Unblocks `folding.rs`,
  `issue_1571_fold_indicator_lag.rs`,
  `issue_1568_session_fold_restore.rs`.

### Lower leverage

- **Settings dialog.** Heavy UI surface (search, scroll,
  tree-view, text inputs). Migration only worth it if the
  cluster (`settings*.rs` × 8 files, `keybinding_editor.rs`,
  `issue_1718_settings_search_utf8_panic.rs`) is consolidated
  into a single `SettingsState` observable; otherwise these
  stay imperative.

- **Terminal-emulator escape emission.** `TerminalIoScenario`
  exists but coverage is thin. `terminal*.rs` (4 files),
  `ansi_cursor.rs`, `redraw_screen.rs` (dual) need richer
  cell-grid round-trip shapes — incremental as needed.

- **Plugin-driven tests.** `PluginScenario` was dropped per
  design §6.2. `vi_mode.rs`, `vi_mode_bugs.rs`, plugin
  folders. Some plugin claims that reduce to buffer state can
  fold into `BufferScenario` (load the plugin via the existing
  harness option and dispatch `Action::PluginAction(...)`),
  but the tests that genuinely depend on plugin runtime
  semantics stay where they are.

- **GUI / wgpu, external-process formatting, file-explorer
  panics with split state.** `gui.rs`,
  `issue_1573_format_buffer.rs`, `crash_repro.rs` — out of
  scope per the design or composite of multiple blocked
  observables.

- **Internal-only protocol units.** `csi_u_session_input.rs`
  parses `InputParser` directly — properly a unit test for
  the parser, not a buffer scenario. Keep it where it is.

## Patterns to follow

These are the conventions the migrated files use. Stick to
them.

### Single-file vs. extras vs. issue file

- **`migrated_<topic>_full.rs`** when migrating a whole e2e
  file.
- **`migrated_<topic>_extras.rs`** for adding gaps to an
  existing migration of the same topic. Smaller blast radius
  than expanding the original file.
- **`migrated_issue_<NNN>_<short>.rs`** for issue-numbered
  regressions (`tests/e2e/issue_NNN_*.rs`). Cross-reference
  the issue in the file-level docstring.

### Anti-test per file

Every new migration file ships at least one anti-test:

```rust
#[test]
fn anti_<topic>_dropping_<action>_yields_check_err() {
    let scenario = BufferScenario { /* same shape, action removed */ };
    assert!(check_buffer_scenario(scenario).is_err(), "...");
}
```

This is a permanent guard against the migration silently going
inert. If a future refactor breaks the `EditorTestApi`
projection so `assert_*_scenario` stops reading buffer state,
both the real test *and* the anti-test would pass and the
suite wouldn't notice. Use the fallible `check_*_scenario`
variant — `assert_*_scenario` plus `#[should_panic]` is harder
to read.

### Direct-harness for cross-state claims

When the claim is "value at point A == value at point B for
the same harness" (e.g. viewport drift, save-point boundary),
the canned `assert_layout_scenario` / `assert_buffer_scenario`
take a single end-state expectation and don't fit. Drop down
to `EditorTestHarness` + `EditorTestApi` directly. Still no
production refactor, no mocks; the harness reconciles render
state the same way the production binary does.

### Modal flow as pure dispatch

Action::Search + per-char InsertChar + PromptConfirm
reproduces the user-facing Ctrl+F flow. The editor's input
handler routes `InsertChar` to the active prompt automatically.
Same shape works for `Action::GotoLine`,
`Action::CommandPalette`, etc.

### Round-trip Copy/Paste over clipboard back-doors

When the e2e test calls `harness.editor_mut().set_clipboard_for_test("X")`
and then pastes, reshape the scenario as a real Copy → Paste
round-trip with `X` already present in the buffer. The
production action path is exercised end-to-end and there's no
test-only back-door to maintain. (See the paste extras
migration for the pattern.) Only fall back to extending
`EditorTestApi` with a clipboard setter when there's no way to
get `X` into the buffer first.

### Pin observed behavior, even when surprising

If migrating a test surfaces a behavior the e2e didn't
explicitly assert on (cursor lands at byte N, anchor advances
past inserted indent, etc.), don't paper over it. Pin the
observed value with a comment naming the asymmetry, and add an
entry to `scenario-migration-findings.md`. A future
intentional behavior change will then have to update the
finding *and* the migrated test — which is the right friction.

## How to continue

For each unmigrated e2e file:

1. **Categorize.** Buffer-only? Layout? Modal? Persistence?
   See "Categories that are clean to migrate today" vs.
   "Categories that are blocked" above.
2. **If clean:** copy a similar `migrated_*` neighbour;
   rewrite `send_key` → `Action::*`; run the test; adjust
   expected cursor / selection text to match observed reality;
   add an anti-test.
3. **If blocked:** identify which extension would unblock it
   (per the table above) and either pick that up or move on.
   Don't add mocks or test-only shortcuts — the migrated test
   must route through production code, otherwise the e2e file
   is doing more work than the scenario.

For each new behavioral finding, add a numbered entry to
`scenario-migration-findings.md` and reference it from the
migration file's header docstring.

## Suggested priorities

If you have one engineer-week to spend, the highest payoff
sequence is:

1. **Extend `RenderSnapshot` with per-row text.** Unblocks the
   largest single cluster of tests (~50 files) for ~1–2 days
   of work. Pure additive.
2. **Land Phase 5 (`LspScenario`) transport seam.** Unblocks
   ~30 LSP files for 1–2 days of work in `src/services/lsp/`.
3. **Land Phase 4 (`StyleScenario`) by factoring `render()`
   into `layout/style/emit`.** Unblocks ~17 theme/syntax files
   for 2–3 days in `src/view/`.

After those three, the remaining categories are smaller and
can be opportunistic. The end state is `tests/e2e/` containing
only the genuinely terminal-side proofs and the GUI-specific
subset, exactly as the design's §13 acceptance criteria
prescribe.
