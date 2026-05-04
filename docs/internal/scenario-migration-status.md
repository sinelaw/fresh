# E2E → Scenario Migration: Progress and Blocking

Living status document tracking the migration from
`tests/e2e/` (227 files, ~4400 imperative tests) to
`tests/semantic/` (scenario form). See
`e2e-test-migration-design.md` for the design.

## At-a-glance

- **Semantic tests passing:** 459 (1 ignored).
- **e2e tests remaining on disk:** all 227 files (none deleted yet
  per the design's "keep e2e for one release after each phase").
- **Infrastructure status (per phase):** see the full table in
  `e2e-test-migration-design.md` §10.1. Phases 4
  (`StyleScenario`) and 5 (`LspScenario`) are still skeletons.

## What migrates cleanly today

Files in these categories migrate to `BufferScenario`,
`LayoutScenario`, `WorkspaceScenario`, `PersistenceScenario`,
`ModalScenario`, or `TemporalScenario` with no production
refactor:

| Category | Status |
|---|---|
| Buffer / cursor / selection / clipboard | mostly migrated; remaining gaps are individual unmigrated tests |
| Multi-cursor (AddCursorBelow, AddCursorNextMatch, BlockSelect*) | migrated, including issue #210 + the 3-state AddCursorNextMatch nuance |
| Undo / redo (incl. BulkEdit consolidate-after-save) | migrated |
| Auto-indent / bracket expansion (issue #629) | migrated |
| Goto-matching-bracket (issue #1258) | migrated |
| Arrow keys with selection (issue #1566) | migrated |
| Search-modal flows (Action::Search + InsertChar + PromptConfirm) | migrated for issue #1697 |
| Unicode grapheme handling (issue #1577 — Zalgo, ZWJ, fullwidth) | migrated |
| Wrapped-line viewport drift (issue #1574, #1147) | migrated |
| GotoLine modal | exercised in issue #1147 migration |
| Workspace buffer-count, active path, Close/NextBuffer | migrated |

## What's blocked

| Category | What's blocking | Affected e2e files |
|---|---|---|
| Theme / cell-color / role-tag rendering | `StyleScenario` (Phase 4 skeleton — needs `render()` factored into `layout/style/emit`) | `theme.rs`, `theme_screenshots.rs`, `cursor_style_rendering.rs`, `crlf_rendering.rs`, `syntax_highlighting_*.rs`, `glob_language_detection.rs`, `config_language_selector.rs`, `csharp_language_coherence.rs`, `warning_indicators.rs`, `blog_showcases.rs`, `issue_1554_scrollbar_theme_color.rs`, `issue_1577_unicode_width.rs` (cell-render half), `issue_1598_shebang_detection.rs`, `issue_779_after_eof_shade.rs`, `visual_regression.rs`, `overlay_extend_to_line_end.rs`, `vertical_rulers.rs`, `margin.rs` |
| LSP exchange / diagnostics / completion | `LspScenario` (Phase 5 skeleton — needs `LspTransport` trait at `LspManager` boundary) | `lsp.rs` and 26 `lsp_*.rs` files, `language_features_e2e.rs`, `universal_lsp.rs`, `inline_diagnostics.rs`, `issue_1572_inlay_hint_drift.rs`, `issue_1573_format_buffer.rs`, `hot_exit_recovery_lsp_sync.rs` |
| Per-row screen-text inspection (assertions on rendered glyphs at specific cells) | `RenderSnapshot` extension — needs per-row `Vec<CellText>` exposed via `EditorTestApi` | `issue_1502_word_wrap_squished.rs`, `horizontal_scrollbar.rs`, `line_wrapping.rs` (~26 tests), `line_wrap_*.rs` (5 files), `scroll_clearing.rs`, `scroll_wrapped_reach_last_line.rs`, `scrolling.rs` (cell-content half), `markdown_compose*.rs`, `redraw_screen.rs`, `tab_scrolling.rs`, `popup_wrap_indent.rs`, `memory_scroll_leak.rs`, `side_by_side_diff_*.rs`, `test_scrollbar_keybinds_cursor.rs`, `virtual_line_bg_and_wrap.rs`, `virtual_lines.rs` |
| Folding (display + restoration) | `RenderSnapshot` needs fold-marker rows; session needs fold-state observable | `folding.rs`, `issue_1571_fold_indicator_lag.rs`, `issue_1568_session_fold_restore.rs` |
| Split-view + tab-drag + close-buffer interactions | `WorkspaceScenario` needs split-tree state observable + `Action::CloseBuffer` per split | `split_view*.rs` (3 files), `split_tabs.rs`, `split_focus_tab_click.rs`, `tab_drag.rs`, `tab_config.rs`, `preview_tabs.rs`, `buffer_groups.rs`, `buffer_lifecycle.rs` (advanced cases), `issue_1540_tab_click_focus.rs`, `issue_1620_split_terminal_click_panic.rs`, `position_history*.rs` |
| File-explorer flows | needs `FileExplorerState` observable + `Action::FileExplorer*` already exists | `file_explorer.rs`, `file_browser.rs`, `explorer_*.rs`, `issue_1569_explorer_auto_expand.rs` |
| Settings dialog | dialog-internal (heavy UI). Either expose `SettingsState` observable or stay imperative | `settings*.rs` (8 files), `keybinding_editor.rs`, `issue_1718_settings_search_utf8_panic.rs` |
| Terminal-emulator escape emission | `TerminalIoScenario` exists but coverage is thin; cell-grid round-trip needs more shapes | `terminal*.rs` (4 files), `ansi_cursor.rs`, `csi_u_session_input.rs` (input-parser unit tests, not buffer scenarios), `redraw_screen.rs` (dual) |
| Plugin-driven tests (vi-mode, tab-actions, etc.) | `PluginScenario` was dropped per design §6.2; some plugin claims that reduce to buffer state can fold into `BufferScenario` if needed | `vi_mode.rs`, `vi_mode_bugs.rs`, plugin folders |
| GUI / wgpu-specific tests | dropped per design §12 | `gui.rs` |
| External-process formatting | not covered by any scenario type (subprocess + UNIX-only) | `issue_1573_format_buffer.rs` |
| File explorer panic regressions | needs both file-explorer + workspace state | `crash_repro.rs` |

## Migration patterns established (this session)

1. **Single-file migration**: `migrated_<topic>_full.rs` with the
   complete e2e file's claims; faithful 1:1 with `Action::*`
   replacing keymap dispatches.
2. **Topic extras**: `migrated_<topic>_extras.rs` for the gaps
   not covered by an existing migration file.
3. **Issue regressions**: `migrated_issue_<NNN>_<short>.rs` for
   each issue-numbered e2e file.
4. **Anti-tests**: every new file ships one
   `check_*_scenario(...).is_err()` test that proves the
   assertion pipeline is genuinely sensitive to the action under
   test (see PR thread for the rationale).
5. **Direct-harness LayoutScenarios**: when the claim requires
   comparing two `viewport_top_byte` values within one harness
   (e.g. issue #1574 drift), use `EditorTestHarness` +
   `EditorTestApi` directly rather than two `LayoutScenario`
   values. Still no production refactor, no mocks.
6. **Modal flow as pure dispatch**: `Action::Search` +
   `Action::InsertChar` (routes into the active prompt) +
   `Action::PromptConfirm` reproduces the user-facing Ctrl+F
   flow without going through a modal-aware runner.

## How to continue

Pick an e2e file from the "What migrates cleanly today" list,
copy the structure of an existing `migrated_*` neighbour, replace
keymap dispatches with `Action::*` variants, run the migration,
adjust expected cursor/selection/text fields to match observed
behavior, add a `check_*_scenario().is_err()` anti-test.

For files in the "What's blocked" table, either pick up the
production hook for the relevant phase (see the per-phase
skeleton file) or extend `EditorTestApi` / `RenderSnapshot` with
the missing accessor.
