# Coverage report: dead code and large untested areas

*Snapshot from 2026-09-23 at v0.5.1. Line numbers go stale quickly; to
refresh them, re-run the script below.*

Snapshot of line coverage for the full test suite (unit, integration and e2e),
taken to find dead code and big untested chunks. Reproduce with
`xvfb-run scripts/coverage-full` (about 35 minutes on 4 cores). It runs the
tests, then `scripts/coverage-analyze.py` builds the lists below and
`scripts/coverage-html.py` bundles everything into
`target/coverage-report/coverage.html`, a single page with annotated source.
`scripts/coverage-full --report-only` regenerates the reports from the last
run's profile.

- **Run:** `cargo llvm-cov nextest --all-features --all-targets` (the same test
  set as CI's Linux job), under `xvfb-run`.
- **Tests:** 10,033 run, all passing[^env]; 163 `#[ignore]`d tests not run
  (benchmarks, known-flaky tests, tests waiting on a layout port, a 22-minute
  sweep; CI skips them too).
- **Scope:** all `src/` code in the workspace. Test files, in-tree `tests.rs`
  modules and `benches/` are excluded. Inline `#[cfg(test)] mod tests` blocks
  inside source files are still counted, which lifts the percentages slightly.
- **Subprocesses:** the e2e tests launch the instrumented `fresh` binary, and
  those runs are counted too.

[^env]: Two tests failed first only because of the container: the GUI test
    lacked `libxkbcommon-x11`, and the SSH test lacked `/run/sshd`. Both pass
    once those are installed.

## Summary

**85.2% line coverage** (225,231 of 264,341 lines). 3,229 of 24,073 functions
were never executed.

| crate | lines | uncovered | coverage |
|---|---:|---:|---:|
| fresh-editor | 185,492 | 28,506 | 84.6% |
| fresh-editor-core | 47,979 | 5,217 | 89.1% |
| fresh-plugin-runtime | 11,572 | 2,836 | 75.5% |
| fresh-ui | 8,834 | 762 | 91.4% |
| fresh-core | 3,453 | 412 | 88.1% |
| fresh-update | 3,330 | 318 | 90.5% |
| fresh-plugin-api-macros | 920 | 236 | 74.3% |
| fresh-input-parser | 825 | 79 | 90.4% |
| fresh-parser-js | 799 | 129 | 83.9% |
| fresh-gui | 591 | 530 | 10.3% |
| fresh-languages | 379 | 82 | 78.4% |
| fresh-i18n | 167 | 3 | 98.2% |

`rustc` doesn't warn about most of the dead code below because it's `pub`
inside a library crate, and `pub` items never trigger `dead_code` warnings.

## 1. Dead code: safe to delete once confirmed

### 1a. `fresh-editor-core/src/primitives/textmate_engine.rs`: a whole stale module (825 lines)

It is declared `pub mod textmate_engine;` in `primitives/mod.rs`, and nothing
else in the workspace refers to it. The `TextMateEngine` the editor actually
runs is a *different* struct, `highlight_engine.rs:266`, reached through
`HighlightEngine::TextMate`. The module's only callers are its own
`#[cfg(test)]` tests, which give it 33% coverage; the other 372 lines never
run. It looks like a copy left behind when the engine moved into
`highlight_engine.rs`, and the module table in `primitives/mod.rs:13` still
lists it.

### 1b. Never-executed functions that nothing references

These functions never ran, and their name appears nowhere else in the
repository: no Rust, TS or JS file mentions it outside the definition. The
list also includes functions referenced only from inside other such functions,
or only from test files. Name matching is textual, so functions with common
names (`new`, `run`, trait methods) are left out. That keeps false positives
low but means the list is not exhaustive. In total: **123 functions, about
1,240 lines**. The larger ones:

| lines | function | location |
|---:|---|---|
| 58 | `filter_commands` | `fresh-editor/src/input/commands.rs:1711` |
| 49 | `apply_hyperlink_overlays` (only a test mentions it) | `fresh-editor/src/view/ui/split_rendering/post_pass.rs:155` |
| 46+4 | `client::run_client_with_connection`, `client::run_client` (`fresh attach` uses `run_client_relay`) | `fresh-editor/src/client/mod.rs:57-111` |
| 39 | `Editor::run_shell_command_blocking` | `fresh-editor/src/app/shell_command.rs:250` |
| 35 | `TerminalState::full_content_string` | `fresh-editor/src/services/terminal/term.rs:869` |
| 34+34 | `TextBuffer::next_word_boundary` / `prev_word_boundary` | `fresh-editor-core/src/model/buffer/mod.rs:3228-3297` |
| 29+19 | `Viewport::scroll_view_lines`, `get_source_byte_for_view_line` | `fresh-editor/src/view/viewport.rs:1298, 1374` |
| 26 | `Editor::get_composite_view_state` | `fresh-editor/src/app/composite_buffer_actions.rs:512` |
| 26+7 | `release_checker::start_update_check`, `fetch_latest_version` | `fresh-editor/src/services/release_checker.rs:261, 292` |
| 25+14 | `TextBuffer::replace_all_regex`, `replace_next` | `fresh-editor-core/src/model/buffer/mod.rs:2862, 2906` |
| 21 | `CellThemeRecorder::run_owned` | `fresh-editor/src/app/types/theme.rs:115` |
| 21 | `Editor::save_histories` | `fresh-editor/src/app/render.rs:5964` |
| 20 | `LspManager::reset_for_new_project` | `fresh-editor/src/services/lsp/manager.rs:919` |
| 18+17+14 | `CompositeInputRouter::navigate_to_hunk`, `display_to_source`, `click_to_pane` | `fresh-editor/src/input/composite_router.rs:138-190` |
| 18 | `RecoveryStorage::cleanup_inplace_write_recovery` | `fresh-editor/src/services/recovery/storage.rs:738` |
| 17+17 | `SplitNode::grouped_ancestor_of`, `find_grouped` | `fresh-editor/src/view/split.rs:771, 791` |
| 17+5 | `WidgetRegistry::focus_follower_of`, `has_focus_follower` | `fresh-editor-core/src/widgets/registry.rs:822, 838` |
| 17 | `plugin_schemas::deep_merge_under` | `fresh-core/src/plugin_schemas.rs:79` |
| 16+12+13+12+9 | line-anchor API: `IntervalTree::insert_line_anchor`, `MarkerList::create_line_anchor`, `get_line_anchor_info`, `nearest_line_anchor_before{,_line}` | `fresh-editor-core/src/model/marker_tree.rs:325`, `model/marker.rs:339-428` |
| 15 | `BufferMetadata::hidden_virtual_buffer` | `fresh-editor/src/app/types/buffer_meta.rs:373` |
| 14 | `Editor::handle_custom_notification` | `fresh-editor/src/app/async_messages.rs:1339` |
| 14 | `Window::enter_terminal_scrollback_view` | `fresh-editor/src/app/window/mod.rs:2101` |
| 14 | `TerminalManager::cleanup_dead` | `fresh-editor/src/services/terminal/manager.rs:563` |
| 14 | `LineMappingsBuilder::add_tab` | `fresh-editor-core/src/primitives/visual_layout.rs:153` |
| 13 | `Editor::focused_widget_panel_for_buffer` | `fresh-editor/src/app/widget_runtime.rs:2298` |
| 13+11 | `WarningDomainRegistry::active_domains`, `highest_level` | `fresh-editor/src/app/warning_domains.rs:373, 388` |
| 13 | `LspManager::restart_attempt_count` | `fresh-editor/src/services/lsp/manager.rs:1784` |
| 13 | `Window::effective_tabs_width` | `fresh-editor/src/app/window/mod.rs:2691` |
| 12+10+7 | `RecoveryEntry::age_display`, `age_seconds`, `RecoveryMetadata::format_description` | `fresh-editor-core/src/recovery_types.rs` |
| 11 | `LspClientState::to_server_status` | `fresh-editor/src/services/lsp/async_handler.rs:362` |
| 11 | `Cursor::block_selection_bounds` | `fresh-editor-core/src/model/cursor.rs:108` |
| 8×6 | `FileSystem::{read_file,read_range,count_line_feeds_in_range,write_file,metadata,is_dir,is_file,canonicalize}_async` default methods | `fresh-editor-core/src/model/filesystem.rs:869-948` |
| 7×4 | `Constraints::tight_w/tight_h/loose_w/loose_h` | `fresh-ui/src/render/geom.rs:46-76` |

There are about 60 smaller ones (3–10 lines each), mostly accessors and
predicates such as `LspManager::{is_in_cooldown, has_pending_restart,
configured_languages, get_effective_root_uri, set_root_uri}`,
`TerminalModes::*_enabled`, `DirectoryContext::*_history_path` and
`EventLog::{last_event, disable_streaming, set_snapshot_interval}`. The full
list is in `target/coverage-report/analysis.json` (`unreferenced`) after a
run.

### 1c. Zero-coverage files that are *not* dead

`fresh-core/src/services.rs` (the no-op plugin service bridge),
`fresh-editor/src/app/agent_scripts.rs` (`eval_agent_script`, used by
`server/command_access.rs`) and `app/calibration_actions.rs` (used by
`shell_host.rs`) are all 0% covered. All three are wired up, so they're
untested rather than dead.

## 2. Plugin API methods no test calls

In total, 78 `JsEditorApi` methods (`quickjs_backend.rs`) never ran.

**52 of them (~640 lines) are called by no bundled plugin either.** They're
public plugin API, so they may still be worth keeping, but nothing in the repo
uses or tests them:

> addVirtualText, animateArea, animateVirtualBuffer, cancelAnimation, charWidth,
> clearConcealsInRangeForNamespace, clearOverlaysInRange, clearSplitLabel,
> clearVirtualTexts, clearWindowPreview, completeCommand¹, computeLineDiff,
> createScrollSyncGroup, defineConfigNumber, defineConfigStringArray, fileStat,
> getAllCursorPositions, getBufferLineCount, getCurrentLocale, getHandlers,
> getHighlights, getMacro, getPluginDir, getSplitByLabel, getUserConfig,
> getWorkingDataDir, isProcessRunning, killBackgroundProcess, killHostProcess,
> killProcess, listGrammars, listMacros, moveBufferToSplit, playMacro,
> pluginName, pluginTranslate, previewWindowInRect, prewarmWindow,
> removeOverlay, removeVirtualText, sendTerminalInput, setLineTargets,
> setPromptFooter, setPromptSelectedIndex, setPromptTitle,
> setScrollSyncAnchors, setSplitLabel, setSplitRatio, spawnBackgroundProcess,
> spawnProcessWait, updateCompositeAlignment, updateMarker

¹ `completeCommand` is emitted by the agent-script wrapper
(`server/command_access.rs`), so it isn't dead, but no test runs it.

These **are called by bundled plugins, but no test reaches them**, so a
regression there would ship unnoticed: `createTerminal`, `closeTerminal`,
`signalWindow`, `cancelRemoteAgent`, `httpFetch` (orchestrator, pkg),
`setFoldingRanges` (git_log, markdown_toc), `openFileInBackground`
(markdown_toc), `saveSetting`, `splitWindow`, `orchestratorMode`
(welcome_screen), `getBufferSavedDiff` (diff_nav), `isBufferModified`,
`saveBufferToPath` (vi_mode), `compositeNextHunk`, `compositePrevHunk`,
`closeCompositeBuffer`, `removeScrollSyncGroup` (audit_mode),
`markFileReadOnly`, `getTempDir` (slang-lsp), `releaseDiffBaseline`
(git_gutter, live_diff), `disableLspForLanguage` (LSP helper plugins),
`dismissPreview` (finder), `deleteTheme` (theme_editor), `clearEnv`
(env-manager).

## 3. Large untested areas (reachable, not dead)

Files with the most uncovered lines, with what the gap is:

| file | uncovered / lines | what isn't tested |
|---|---:|---|
| `fresh-editor/src/main.rs` | 2,122 / 4,159 (49%) | CLI subcommands: `--init`/package scaffolding (`create_{plugin,theme,language}_package`, `init_package_command`, `write_package_json`), `server`/`attach`/`open-files`/`cmd`/sessions list and kill, `plugin_help_text`, `script_api`/`script_check`, `connect_remote` |
| `fresh-plugin-runtime/src/backend/quickjs_backend.rs` | 1,843 / 8,937 | the plugin API methods in §2 |
| `fresh-editor/src/app/plugin_dispatch.rs` + `plugin_commands.rs` | 2,045 / 8,457 | the editor-side handlers for those same API calls (`handle_spawn_background_process`, `handle_add_virtual_text`, `handle_http_fetch`, `handle_add_menu`, animations, …) |
| `fresh-editor/src/services/lsp/async_handler.rs` + `app/lsp_requests.rs` | 1,421 / 7,692 | LSP rename/prepare-rename, signature help, server-initiated requests |
| `fresh-editor/src/webui/mod.rs` | 774 / 1,456 (47%) | web UI server: `run`, WebSocket upgrade/frame parsing, `apply_mouse`, `apply_settings`, session diffing |
| `fresh-editor/src/view/scene.rs` | 709 / 1,264 (44%) | scene projection for popups and other overlays (`project_popup`, 1584-1655, 1943-2012) |
| `fresh-editor/src/app/clipboard.rs` | 602 / 1,189 (49%) | `copy_selection_with_theme` / copy-with-formatting prompt, `resolve_pending_paste`, and the `yank_word_forward/backward`, `yank_vi_word_end`, `yank_to_line_end` actions. These have palette names, but no default keymap binds them and `vi_mode.ts` doesn't use them. |
| `fresh-plugin-runtime/src/ts_export.rs` | 574 / 1,196 | `write_fresh_dts` (522 lines). Its test is `#[ignore]` ("writes a file"). |
| `fresh-gui/src/lib.rs` | 516 / 575 (10%) | the winit/wgpu event loop and key translation. Only headless launch-and-quit is tested. |
| `fresh-editor/src/app/prompt_actions.rs`, `action_dispatch.rs` | 1,011 / 3,053 | individual actions without an e2e test (`handle_set_page_width`, `handle_restart_lsp_server`, …) |
| `fresh-editor-core/src/primitives/grammar/loader.rs` | 377 / 616 (39%) | user, language-pack and bundle grammar loading (`load_user_grammars`, `load_language_pack_grammars`, `load_bundle_grammars`, `process_manifest`) |
| `fresh-editor/src/services/remote/filesystem.rs` | 335 / 682 (51%) | the remote `walk`, `search_file` and `sudo_write` |
| `fresh-editor/src/app/popup_overlay_actions.rs` | 297 / 385 (23%) | the LSP confirmation popup flow and `notify_lsp_current_file_opened` |
| `fresh-editor/src/view/settings/mouse.rs` | 247 / 354 (30%) | mouse handling in the settings dialog |
| `fresh-editor/src/app/input_helpers.rs` | 179 / 301 (40%) | `switch_to_previous_tab`, `start_switch_to_tab_prompt` |
| `fresh-editor-core/src/primitives/ansi_background.rs` | 112 / 112 (0%) | ANSI-art background loading (`Editor::load_ansi_background`) |
| `fresh-editor/src/client/relay_unix.rs` | 156 / 156 (0%) | the `fresh attach` terminal relay |
| `fresh-editor/src/bin/{measure_startup,event_debug,generate_schema}.rs` | 386 (0%) | dev-only binaries (`dev-bins` feature) |

The largest single uncovered blocks: `ts_export.rs:531-1006` (475 lines),
`render.rs:1662-1849` (178, `calibration_description`, the input-calibration
wizard's scene), `clipboard.rs:1765-1987` (174, the `yank_*` actions),
`input_helpers.rs:37-216` (153), `gui/lib.rs:746-901` (131, key translation)
and `ansi_background.rs` (111).

## Limitations

- **Plugin TypeScript isn't measured.** Only the Rust side of each plugin API
  call is instrumented.
- **Platform-gated code doesn't appear in the report**, because it isn't
  compiled on Linux: `fresh-winterm`, the `*_windows.rs` files and
  `fresh-gui/src/macos/`. These missing files are not dead code.
- **Files with only type definitions have no coverage records.** No file under
  `src/` is orphaned: every one is reachable from a `mod` declaration.
- **Dead code in §1b is found by name matching, not a call graph.** Confirm
  each entry with a build before deleting it.
