# Search, Replace, Diff/Review & Macros

Purpose: explain how Fresh implements in-buffer search/replace, project-wide search and live grep, the diff/review (hunk) viewer, git-log viewing, and the keyboard-macro system — the decisions, the scan/diff algorithms, and what is shipped vs. planned, with `path:line` references.

Throughout: features split into a thin **Rust host** layer (incremental scans, piece-tree diff, overlays, search/replace orchestration, macro codegen) and **TypeScript/QuickJS plugins** that own list/picker UIs (`live_grep.ts`, `search_replace.ts`, `git_log.ts`, `audit_mode.ts`). Commit messages repeatedly justify *adding a host primitive* over a *plugin-side workaround*. Labels below: **IMPLEMENTED** (in code), **PLANNED** (design only / partially stubbed).

---

## 1. In-buffer search & replace (IMPLEMENTED)

The whole method cluster lives in `crates/fresh-editor/src/app/search_ops.rs` (1307 lines), extracted from `render.rs` in the editor-modules refactor (commit `2d54bac74`). Pure regex helpers are in `regex_replace.rs`; the large-file scan state in `search_scan.rs`; state types in `app/types/search_state.rs`.

### 1.1 Search state & the small/large-file split

`SearchState` (`app/types/search_state.rs:16`) holds the query, sorted `matches: Vec<usize>` (byte offsets), parallel `match_lengths`, the current index, a `wrap_search` flag, optional `search_range` (search-in-selection), and a `capped` flag. Matches are bounded at `SearchState::MAX_MATCHES = 100_000` (`search_state.rs:39`) to bound memory on pathological patterns.

`perform_search` (`search_ops.rs:66`) branches on file size:
- **Small files / search-in-selection** run inline: the whole buffer (or the selection slice) is loaded as a UTF-8 string and `regex.find_iter` collects matches up to the cap (`search_ops.rs:118-124`).
- **Large (lazy-loaded) files** with no selection start a non-blocking **incremental chunked scan** (`start_search_scan`, `search_ops.rs:368`) that processes a few ~1 MB chunks per render frame so the UI stays responsive.

Both paths converge in `finalize_search` (`search_ops.rs:148`): it sets `SearchState`, moves the cursor to the first match at/after the cursor, and creates overlays. The overlay strategy differs deliberately (`search_ops.rs:182-210`):
- **Small files**: overlays for **all** matches, using markers — so positions auto-track buffer edits and `F3`/Find-Next stays correct after edits.
- **Large files**: **viewport-only** overlays (`refresh_search_overlays`, `search_ops.rs:243`), to avoid multi-GB overlay allocations. `refresh_search_overlays` uses `partition_point` (binary search) on the sorted `matches` vec, so it's O(log N + visible). `check_search_overlay_refresh` (`search_ops.rs:333`) re-paints on scroll, but only for large files (small files already cover everything).

### 1.2 Regex construction & line anchoring

`regex_replace::build_search_regex` (`regex_replace.rs:12`) always returns a regex: in plain-text mode the query is `regex::escape`d, so one code path serves both literal and regex search. Whole-word wraps with `\b…\b`. The key decision (commit `d82be027b`, Jun 2025): the regex crate defaults to single-line mode where `^`/`$` bind to the haystack boundary — "the opposite of what every editor's find does." Both the Unicode search builder and the `bytes::Regex` replace builder set `.multi_line(true).crlf(true)` so anchors match every line boundary on LF and CRLF buffers (`regex_replace.rs:33-40`, `:62-67`). The chunked-scan path re-applies these flags from `regex.as_str()` because `as_str()` carries only the pattern, not builder flags (`search_ops.rs:385-390`). Tests at `regex_replace.rs:246-279` lock in per-line anchoring and that `.` does not cross newlines.

### 1.3 Find-next / find-previous & quick-find

`find_match_in_direction` (`search_ops.rs:461`) always searches **from the cursor**, not from a stored index (matches VS Code/IntelliJ). It binary-searches `match_positions` for the first match strictly after / last strictly before the cursor, with wrap-around governed by `wrap_search`. For small files it uses **overlay marker positions** (`get_search_match_positions`, `search_ops.rs:416`) as the source of truth (they track edits); for large files it reads `search_state.matches` directly.

`find_selection_next`/`find_selection_previous` (`search_ops.rs:574`, `:643`) are quick-find: search the current selection or word under cursor without opening the panel. `get_selection_or_word_for_search_with_pos` (`search_ops.rs:713`) only extracts a word when `is_cursor_on_word_char` is true — issue #1537 fix: a `goto_matching_bracket` leaving the cursor on `}` previously let Ctrl+F3 hijack the query into the bracket plus surrounding words. (Design doc `search-next-occurrence.md` proposed Ctrl+F3/Ctrl+Shift+F3 bindings and the "set term not options" rule; the handlers are now shipped.)

Other shipped fixes worth noting: off-viewport matches center vertically on scroll, already-visible matches aren't re-scrolled (`move_cursor_to_match` → `jump_active_cursor_to`, commit `b6fd256f6`, issue #1251); search-overlay end markers use left gravity so typing right after a match isn't swallowed into the highlight (commit `9e761d049`, issue #2053).

### 1.4 Replace-all & query-replace

`perform_replace` (`search_ops.rs:809`) finds **all** matches first, then applies them via **`BulkEdit`** — Delete+Insert event pairs processed in reverse, giving O(n) piece-tree work and a **single undo step** for the whole replace-all. This is the fix for the O(n²) hang (commit `34c979fff`: replace-all could take 60+s / 10 GB for ~1500 edits). Regex mode expands capture groups per match via `collect_regex_matches`; plain mode uses literal `find_next_in_range`.

`regex_replace.rs` owns the replacement template semantics:
- `interpret_escapes` (`:81`) turns `\n \t \r \\` into control chars, **only in regex mode** (commit `9b3c6f844`, issue #1256: the regex crate's `expand()` only handles `$N` and leaves `\n` literal). Unknown escapes (`\q`) and a trailing lone `\` pass through verbatim, to avoid surprising users who didn't mean to escape.
- `normalize_replacement` (`:107`) rewrites bare `$1` → `${1}` so the regex crate doesn't greedily eat trailing letters into the group name (Python/PCRE semantics; commit `43dbe879d`).

Interactive **query-replace** (`start_interactive_replace`, `:919`; `handle_interactive_replace_key`, `:984`) is lazy: it finds only the current match, prompts `Replace? (y)es (n)o (a)ll (c)ancel:`, and searches forward-then-wrap for the next. The undo model (commits `b4b1a8140`/`84b4ae3ef`): each `y`/`n` is one event, but `a`/`!` collects all remaining matches and applies them as one `BulkEdit` = one undo for all. `replace_current_match` (`:1209`) wraps MoveCursor+Delete+Insert in an atomic `Event::Batch`.

---

## 2. Project-wide search/replace & live grep

Two plugins, both on the `Rust` chunked-scan primitives and the shared `Finder` library:
- `search_replace.ts` (2166 lines) — the persistent search-and-replace panel.
- `live_grep.ts` (1134 lines) — the floating live-grep overlay.
- `git_grep.ts` (76 lines) and others reuse `plugins/lib/finder.ts` (1676 lines).

### 2.1 The chunked project-search primitive (IMPLEMENTED)

Designed in `project-search-replace.md` ("Feature-complete — needs tests and polish"). Four principles: everything goes through the editor's real abstractions (`FileSystem` trait + `TextBuffer`/piece-tree), search what the user *sees* (dirty buffers via piece tree, unopened files via `FileSystem`), no special-casing of large files, plugin is UI-only.

`grepProjectStreaming` snapshots dirty buffers on the main thread, spawns a tokio task that walks the tree via `ignore::WalkBuilder` (respects `.gitignore`), and processes **8 files in parallel via a semaphore**; each file is wrapped in a `TextBuffer` and run through `search_scan_all`. Results stream back as `GrepMatch` JSON over `AsyncBridge`.

The chunked scanner (`model/buffer/mod.rs`): `search_scan_init` (`:1758`) pre-splits the tree so every leaf ≤ `LOAD_CHUNK_SIZE` (1 MB, `mod.rs:89`) and builds a `ChunkedSearchState`; `search_scan_next_chunk` (`:1799`) processes one chunk, tracking line/column/context incrementally via a `running_line` cursor (O(chunk) not O(buffer)); `search_scan_all` is the synchronous `spawn_blocking` variant. Cross-chunk matches are handled by an `overlap_tail` window of `max(query_len, 256)` bytes.

Deliberate trade-offs (from the design doc): context truncation on lines longer than the overlap window (affects <1% of code); a **soft** match cap — the 8 searchers check `match_count` with relaxed atomics, so the total can slightly exceed `max_results` (a UI-responsiveness limit, not a contract); `\b` whole-word is ASCII-centric.

`SearchScan` (`app/search_scan.rs`) wraps the in-flight large-file scan with session metadata and the take/restore-chunked dance (`take_chunked`/`restore_chunked`) that lets the orchestrator pass `ChunkedSearchState` by `&mut` into the buffer without fighting the borrow checker. `scan_orchestrators.rs` drives both the search scan and the line-feed scan one batch per frame (`process_search_scan`, `:240`; `process_search_scan_batch`, `:267`), bounded by `config.editor.read_concurrency`. The line scan additionally fans unloaded leaves out to `tokio::task::spawn_blocking` for concurrent `count_line_feeds_in_range` (`scan_orchestrators.rs:159`), which remote filesystems override to count server-side without transferring data.

### 2.2 Replace in project & known bugs

`replaceInFile` opens the file as a hidden buffer if needed, sorts matches **descending by byte offset**, and applies all edits as one bulk op, saving via `FileSystem`. **Caveat (verified in `project-search-replace-bug-reproduction.md`, shipped 0.2.x):** the doc claims "single undo," but `replaceInFile` actually **bypasses the per-buffer undo stack** (Bug 1), and pressing replace a second time reuses stale byte offsets against now-modified content → corruption (Bug 4). These are real, open defects in the shipped path, not design intent.

### 2.3 Live Grep float, Resume, and the scope picker

`live_grep.ts` runs a centered overlay sized by frame **percentage** so it doesn't jump as results stream in (commit `46e7a133c`). The provider chain is `git-grep → rg → ag → ack → grep`. `dadd56693` added "highlight all matches + center on the *visual* row" (counting wrapped rows) in the preview.

**Resume ("Return to Work", issue #1796, IMPLEMENTED):** `services/live_grep_state.rs` caches the prior query, selected index, and the on-screen matches (`LiveGrepLastState`). `Action::ResumeLiveGrep` re-opens the overlay in the same state **without re-running ripgrep**, enabling the flip-between-editing-and-results flow. `cached_results` is invalidated on any query keystroke (`invalidate_cache`). `GrepMatch` is kept in core (not the plugin) because Quickfix export needs to land in an editor-owned virtual buffer. `last_results_snapshot_id` is reserved for the Quickfix round-trip (PLANNED — "unused until that wiring lands").

**Quickfix** moved from bespoke Rust to a `Finder` dock panel (commit `c4d60b584`, #2124); the one host need (tearing down the overlay) became a generic `editor.cancelPrompt()` rather than quickfix-specific code.

**Scope picker (`global-search-ux.md`, MIXED):** the vision is to grow live grep into one-stop "universal search" with a scope toolbar over project files, ignored/hidden files, open buffers, open terminals, diagnostics, symbols, git history, worktrees, and Orchestrator sessions. **Shipped:** the inline scope toggles dispatched as plugin actions via `Alt+<char>` (no new core Action), terminal search scoped per-cwd via a new `getTerminalDir()` host getter (commit `fd4437171`), and **closed-terminal retention** — on close the backing scrollback is *renamed* to `fresh-terminal-<id>-closed-<epoch>.txt` (ids restart per session, so deleting would let a reused id clobber retained logs), GC-bounded at `MAX_RETAINED = 200`, globbed as `*.txt` so it's searchable with no plugin change. **Planned:** the grouped `Scopes ▾` popover, sources rail, multi-root scopes, and the full-width header-band redesign (§12, "flagged for sign-off"). The fan-out algorithm — run every enabled scope, merge tagged `GrepMatch`es into one capped list with per-source caps and graceful per-source failure — is the design target.

**Replan note (`search-replace-scope-replan-on-widgets.md`, PLANNED):** an in-progress 27-commit search/replace UX branch must be re-landed atop master's declarative widget-runtime rewrite; per-behavior, master's `textInput`/`toggle`/`tree{checkable}` already covers Tab-cycle, search-on-type, and per-row checkboxes. Surviving tickets: empty-state quality, panel passthrough (`Mode(_)` in `KeyContext::allows_ui_fallthrough`), footer hints, **single-file scope** (a post-filter on the streaming callback for v1 — "we eat the wasted scan"), `Alt+A` binding, and multi-line input.

### 2.4 Quick Open & fuzzy matching (IMPLEMENTED)

The `Ctrl+P` Quick Open system (`input/quick_open/`) is prefix-routed (`QuickOpenRegistry::get_provider_for_input`, longest-prefix-first): empty = files, `>` = command palette, `#` = buffer switcher, `:` = goto-line. `#` also lists virtual plugin buffers like `*Git Log*` (issue #2373). `FileProvider` (`providers.rs`) uses `git ls-files` via `ProcessSpawner` as the fast path, falling back to a `FileSystem` walk; enumeration runs on a background thread with periodic partial results (`WALK_UPDATE_INTERVAL = 300ms`), plus a synchronous `probe_prefix` for instant path-prefix hits. Results are cwd-keyed so switching projects doesn't serve stale lists; `set_backends` re-points the spawner on authority swap (the "host files in a remote session" bug).

The fuzzy matcher (`input/fuzzy/`) is fzf-style subsequence matching with a scoring DP (`matcher.rs`) and an allocation-amortizing `FuzzyMatcher` (reusable scratch + `PreparedPattern`). Scoring bonuses (`fuzzy/mod.rs:45-79`) reward consecutive runs, word boundaries, start-of-string, camelCase transitions, contiguous substrings, and — decisively — **basename-prefix** (64) and **path-segment-prefix** (32), so typing `ts` ranks `tsconfig.json` above `pkg.ts`. Multi-term (space-separated) queries match each term independently and combine scores, with a tight-span bonus rewarding targets that reconstruct the query across a single separator (`/etc/hosts`, `saveFile`).

---

## 3. Diff/review (hunk) viewer

Three layers: the **pure piece-tree diff** and **line diff** (core), the **composite-buffer side-by-side renderer** (host), and the **`audit_mode.ts` review session** (plugin, 6609 lines).

### 3.1 Pure diff algorithms (IMPLEMENTED)

**`model/line_diff.rs`** (596 lines): classic LCS over lines for the saved-vs-current "modified line" gutter. `diff_lines` (`:57`) computes the LCS DP table, backtracks, then `find_changed_lines_with_deletions` marks current lines not in the LCS as insertions/modifications and deletion points where saved lines vanished. `classify_change` (`:228`) distinguishes Inserted/Modified/Deleted by LCS-alignment context. Extensive unit + prop tests verify only the truly-changed line is marked, never shifted lines.

**`model/piece_tree_diff.rs`** (718 lines): a structural diff that exploits the piece tree's `Arc` sharing. `diff_piece_trees` (`:24`) short-circuits identical subtrees via `Arc::ptr_eq` in O(1), so after path-copying edits it is **O(changed-path), not O(all-leaves)** (commit `8304974ec`). It walks both trees in parallel (`diff_collect_leaves`), collects leaves only from differing subtrees with **document-absolute byte offsets** (the offset-tracking fix, commit `a3246035a`, for gutter indicators at the wrong place in large files), then computes a longest common prefix/suffix at byte granularity and reports the changed `byte_ranges`. A rebalance test confirms identical `byte_ranges` whether or not `Arc` sharing survives. This backs `Buffer::diff_since_saved` (`model/buffer/mod.rs:903`, delegating to `persistence.rs:243`) and the `BufferSavedDiff` exposed to plugins (`plugin_dispatch.rs:5541`, snapshot field `buffer_saved_diffs`, TS `getBufferSavedDiff`). The "modified line" gutter renders from it in `view/ui/split_rendering/folding.rs:236` (`diff_indicators_for_viewport` intersects `byte_ranges` with the viewport). `diff_nav.ts` consumes the same `byte_ranges` as one of its three jump sources (git-gutter hunks via `git_gutter.ts`'s published `git_gutter_hunks`, live-diff hunks, and saved-diff).

### 3.2 Side-by-side composite buffers & rebindable hunk nav

Side-by-side drill-down is a **composite buffer** rendered as two-column OLD|NEW text. The deliberate constraint (commit `4ab6e0697`): composite buffers can't occupy a panel slot (the host rejects buffers outside the window's buffer map), so the in-panel side-by-side renders two columns inside the existing center buffer rather than a full-takeover that hid the sidebar.

The composite model is `model/composite_buffer.rs` (panes + layout mode + a display-line→old/new alignment map + `initial_focus_hunk` for scroll centering); view state per pane is `view/composite_view.rs`; rendering is `view/ui/split_rendering/orchestration/render_composite.rs` (`render_composite_buffer`, `:28`), which reuses `build_view_data` per pane to get syntax highlighting, selection, and inline diff highlights for free. Cursor/selection movement is re-implemented in display-row space (`composite_buffer_actions.rs`) rather than routed to source buffers, to handle padding rows and synced scrolling.

Hunk navigation lives in `app/composite_buffer_actions.rs`: `composite_next_hunk`/`composite_prev_hunk` (`:300`, `:325`) and the active-pair wrappers (`:351`, `:359`) scan the alignment for hunk boundaries and scroll to center the target hunk ~1/3 from top. `SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md` (PLANNED) notes these keys are currently **hardcoded in `CompositeInputRouter::route_key_event`** (`input/composite_router.rs`), intercepted before the Action keybinding system, so the keybinding editor can't rebind them; the plan adds `CompositeNextHunk`/`CompositePrevHunk` Actions under a `KeyContext::CompositeBuffer`.

A recurring root cause across review bugs is the **outer-vs-inner split-leaf mismatch** in buffer groups: handlers used `active_split()` where they needed `effective_active_pair()`/`effective_active_split()`, so cursor lookups missed the inner composite view-state (PARITY finding A1 `6093f61`; `REVIEW_DIFF_REMAINING_ISSUES.md` Issue 3, where `setBufferCursor` checks the wrong split and forces an O(n) `move_down` loop for 1000+ line diffs).

### 3.3 The review session & parity with the external `hunk` tool

`audit_mode.ts` is the magit-style review tool: `git status --porcelain -z` as the single source of truth (`review-diff-rewrite-plan.md`, v0.2.22), STAGED/UNSTAGED/UNTRACKED grouping, and **real git staging** (`git add` / `git reset HEAD` / `git checkout`) at file/hunk/line granularity. It renders exactly `viewportHeight` lines into one `editingDisabled` virtual buffer to prevent buffer-wide scrolling. Hunk-level staging uses `git apply --cached <tempfile>` — the canonical method (git's own `add-patch.c`, magit, lazygit) — chosen because `spawnProcess` has no stdin piping, with a `--check` dry-run first (`review-diff-feature-restoration-plan.md`). Actions are **context-sensitive**: the same key (`s`) stages whatever the cursor is on (file in the file pane, hunk in the diff pane).

**Parity goal** (`fresh-vs-hunk-review-gaps.md`, `REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md`): the through-line is *"hunk is a better reader; Fresh is a better actor"* — make Review Diff **read** as well as `hunk` (modem-dev/hunk v0.14.1) and **act** as well as Fresh.

| `hunk` has (Fresh lacks) | Fresh has (`hunk` lacks) |
|---|---|
| live split/stack/auto layouts (`1`/`2`/`0`) | real git stage/unstage/discard at hunk/line/file |
| per-token syntax highlight both panes + word-level intraline | index-aware STAGED/UNSTAGED/UNTRACKED grouping |
| 24-bit truecolor + diff-specific themes | jump-to-and-edit the real file in-app |
| dedicated file sidebar with `+N/−N`, `*N` comment badges | comment persistence to `.review/` + MD/JSON export |
| `?` keymap overlay, `F10` menu, multi-line bordered notes | PR-branch and range review modes |
| agent daemon + `hunk session` CLI | — |

The v2 design (PLANNED, "Nothing here is built yet") refuses a new bespoke renderer: it composes from existing primitives — **split = composite buffer, stack = virtual buffer**, layout toggle just swaps which occupies the panel; everything keyboard-driven is a rebindable Action in a `diff-view` mode (not a hardcoded router — "the v1 mistake"); colors come from theme keys (`editor.diff_*`/`review.*`), never hardcoded RGB; inline notes via `addVirtualLine`/`addVirtualTextStyled`; the Phase-4 agent surface reuses the existing `local_control.rs` IPC and native `SetReviewDiffHunks`/`ReviewHunk` rather than a new daemon (the host already has `Window::review_hunks` at `app/window/mod.rs:849`, dispatched via `PluginCommand::SetReviewDiffHunks` / `handle_set_review_diff_hunks` at `plugin_dispatch.rs:1158`/`:1776`) — and goes "beyond hunk" because the human can edit+stage in the same window. The one intentional key divergence: keep `s`=stage (Fresh's identity), move sidebar to `\` (hunk uses `s`=sidebar).

**The most-flagged remaining reader gap across every findings doc:** the unified/stack pane has **no per-keyword syntax highlighting** (side-by-side via composite buffers already does). `REVIEW_DIFF_NNG_USABILITY_EVAL.md` traces whitespace-only per-char highlight to inline-overlay bg being overwritten by whole-entry `extend_to_line_end` (needs an overlay-priority API addition).

**Other diff features:**
- **Next/Previous Change** (`diff-chunk-navigation.md`, PLANNED; `diff_nav.ts`, 215 lines exists): merges git hunks + piece-tree saved-diff (`byte_ranges`) into one jump list, deduped within 2 bytes, keys `Alt+F5`/`Shift+Alt+F5` (VS Code).
- **live-diff plugin** (`live_diff.ts`, 1376 lines; commit `af6b2283a`): inline diff in the live buffer, updating on `after_insert`/`after_delete` so external writes (a coding agent editing the file on disk) show live; per-buffer reference (HEAD/Disk/Branch).
- **review picker** (`review-picker-plan.md`, PLANNED): one **Review** command opening a two-screen picker with a `★ This PR` smart default, debounced live preview, and "since I last reviewed (N new)" watermarks.

---

## 4. Git-log viewing

`git_log.ts` (1293 lines) + `plugins/lib/git_history.ts` (605 lines). Two evolutions, both with concrete decisions:

**Streaming (`PLAN-git-log-streaming.md`, PARTIALLY SHIPPED):** opening a commit's `git show` diff used to buffer ~43 MB of stdout into JS (TTFP ~6.7 s). PR1/PR2 shipped (`spawnProcess(..., {stdoutTo})` pipes stdout straight to a temp file via `tokio::io::copy`, bytes never enter JS; `openFileStreaming(path)` opens it as a file-backed buffer that grows). `refreshBufferFromDisk` re-stats and does an **O(1) length bump** appending an `Unloaded` tail piece (shrink is treated as corruption and ignored). Growth is **polled at 200ms (~5 fps)**, not file-watched. TTFP drops to <100 ms (buffer opens at 0 bytes and grows under the cursor). The `--numstat` pre-pass (5.4 s on bun's history) and per-line overlay construction (~1 M objects) were dropped. PR3 (PLANNED) re-targets the panel at a per-commit `<sha>.diff`-named, SHA-cached buffer via `setBufferGroupPanelBuffer`, with `handle.kill()` cancellation on selection change (today holding `j` "leaks a trail of zombie git processes").

**Folding & highlighting (`PLAN-git-log-diff-folding-and-highlighting.md`, PLANNED):** Part 1 makes fold-by-file/hunk scale via an incremental, append-only fold scan (line-indexed `lsp_types::FoldingRange`), capped at ~4 MB per pass to handle the cache-hit "one giant 2 GB scan" case. `setFoldingRanges` (the publish-structure primitive) already landed; the earlier "Fold All" commands were **removed** (commit `ea5c23bb3`) because they sat on the wrong primitive (`addFold` collapses immediately). Part 2 replaces plugin per-line diff overlays with syntect's bundled `Diff` grammar — the bug is that `scope_to_category` (`primitives/highlight_engine.rs`) doesn't map `markup.inserted.diff`/`markup.deleted.diff`; the plan adds `HighlightCategory::{Inserted,Deleted,Changed}` with a background pathway (`Theme::bg_for`), reusing existing `diff_add_bg`/`diff_remove_bg`/`diff_modify_bg` theme keys, with whole-line bg fill gated by `extends_to_line_end()`. Current overlay highlighting is gated at **256 KB**.

**Widget migration (commits `c5a844e4a` → `620bc7279` → `08623b093`, IMPLEMENTED):** the toolbar and log pane moved off hand-rolled `setPanelContent` + utf8 byte-offset hit-testing onto host `WidgetPanel`/`List` widgets — the byte-offset arithmetic "was the most error-prone part of the plugin." Accepted trade-off: uniform Button styling replaced richer per-button colors (the custom overlay arithmetic "doesn't compose with the widget runtime"). The detail pane stayed on `setPanelContent` ("no concrete payoff"). Per-line diff highlighting runs **once after streaming settles**, coalescing same-kind rows, gated at 256 KB. Other shipped niceties: "Git Log (Current File)" scopes to the focused file via a `git-log-buffer-focused` context (`41b2296b0`); re-invoking activates the existing tab (`524c2c9c6`).

---

## 5. Keyboard macros (IMPLEMENTED)

State in `app/macros.rs` (`MacroState`), orchestration in `app/macro_actions.rs`, codegen in `app/macro_codegen.rs`. Design: `macro-system-improvements.md` ("Status: Implemented").

### 5.1 Record / play

`MacroState` (`macros.rs:28`) owns a `HashMap<char, Vec<Action>>` register map, the in-flight `RecordingState`, `last_register` (for "play last"), and a `playing` flag. `record_if_recording` (`:138`) appends actions but skips macro-control actions (`is_macro_control_action`, `:169`) — recording a "start recording" would cause infinite regress on replay — and never records while playing.

`record_macro_action` (`macro_actions.rs:157`) special-cases `PromptConfirm`: it snapshots the prompt text *now* into `PromptConfirmWithText` (via `record_transformed`, which bypasses the control filter), so replay uses the user's original input rather than whatever the prompt holds at replay time.

`play_macro` (`macro_actions.rs:109`) replays actions through `handle_action` in a tight loop, calling `recompute_layout` between each so visual-line moves see fresh layout. Replay is **one undo unit** (commit `ef2b27600`, issue #2062): it brackets the whole playback with `EventLog::begin_undo_group`/`end_undo_group` on the buffer that owned the log at start (so a mid-replay buffer switch can't leave a group dangling).

`ShowMacro` renders the macro as an `ActionSpec[]` view (read-only); `ListMacros` lists all registers.

### 5.2 Persist / edit / promote via init.ts (recent)

The decision (commits `5fae0aa27` → `dfb7f827c`): reuse `init.ts` as the macro store rather than inventing `macros.json`, because init.ts already has persistence, hot-reload, type-checking, and crash recovery — and it unlocks the "arbitrary logic" endpoint. The single enabling fix was adding optional `args` to `ActionSpec` (`plugin_dispatch.rs:2122` previously always passed empty args, so `InsertChar(char)` couldn't replay). With it, `getMacro(register)` returns the same `ActionSpec[]` shape `executeActions` consumes: **"a macro is a script and a script is a macro."**

`macro_codegen.rs` renders a recorded `Vec<Action>` into two `init.ts` forms, both wrapped in `// fresh:macro <key>` … `// fresh:end macro <key>` sentinels so `upsert_macro_block` (`:188`) rewrites in place instead of duplicating:
- **Save** — `generate_define_block` (`:127`): `editor.defineMacro("q", [ ...steps ])`, re-seeding the register at startup so `@q` works in a fresh session.
- **Promote** — `generate_promote_block` (`:149`): a `registerHandler` + `registerCommand` stub whose body is the steps wrapped in `executeActions`, ready to edit into arbitrary logic (loops, `getAllCursors`, etc.).

Both emit a `// types: "…"` comment coalescing runs of `insert_char` for readability (`typed_text_summary`, `:81`). `write_macro_to_init` (`macro_actions.rs:371`) upserts the block and hot-reloads init.ts so the macro is live immediately. The capability ramp: **record (@q) → save to init.ts → promote to registerHandler → arbitrary plugin code**, each step one command away.

**Removed (effective revert, commits `429e04cfc` → `4d01e6112`):** a `Macro: Load from buffer` command was prototyped then dropped — its hand-rolled JSON-array parser "broke on the ShowMacro header comment," and init.ts is already a robust edit surface, so it was "the lone hand-parse and its bug class." `ShowMacro` stays read-only. **Deferred:** a strict-mode lint flagging hand-edited macros referencing unknown action names (today `from_str` degrades unknowns to a no-op and `defineMacro` only logs a warning).

---

## Return: 2-paragraph summary + superseded docs

This document consolidates Fresh's search/replace, diff/review, git-log, and macro subsystems. The unifying patterns are: a thin Rust host providing incremental chunked scans (1 MB leaves, soft-capped at 100k matches, one batch per render frame), an `Arc::ptr_eq` structural piece-tree diff that costs O(changed-path), `BulkEdit`-based replace for O(n) single-undo edits, and feature UIs delivered as QuickJS plugins on a shared `Finder`/widget runtime. Search is regex-built with per-line `^`/`$` anchoring, small files track matches via edit-following overlay markers while large files use viewport-only overlays and an incremental scan. Macros are now persistable and promotable to arbitrary code by reusing init.ts as the store, enabled by adding `args` to `ActionSpec` so recorded keystrokes replay faithfully.

The most load-bearing *planned-vs-shipped* distinctions: in-buffer search/replace, the chunked project-search primitive, live-grep Resume, per-cwd/closed-terminal scope retention, the pure diff algorithms, composite side-by-side drill-down, real git staging in `audit_mode.ts`, git-log streaming PR1/PR2, and the full macro save/promote pipeline are **shipped**; the universal-search scope popover/header-band, rebindable composite hunk-nav Actions, the v2 hunk-parity reader (layouts/sidebar/agent CLI), unified-pane syntax highlighting, git-log streaming PR3 + fold/highlight scaling, and the review picker are **planned**. Two shipped *bugs* are documented as real (not design intent): project replace bypasses undo and reuses stale offsets on a second apply.

Old docs this supersedes / absorbs (move-to-archive candidates):
- `docs/internal/global-search-ux.md` — universal-search vision (shipped subset + planned popover).
- `docs/internal/project-search-replace.md` and `project-search-replace-bug-reproduction.md` — project search/replace design + bug report.
- `docs/internal/search-next-occurrence.md` — Ctrl+F3 quick-find (now shipped).
- `docs/internal/search-replace-scope-replan-on-widgets.md` — widget-runtime replan.
- `docs/internal/finder-abstraction.md` — `Finder<T>` library (built).
- `docs/internal/diff-view.md`, `diff-chunk-navigation.md` — diff UX + Next/Prev Change.
- `docs/internal/REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md`, `REVIEW_DIFF_HUNK_PARITY_FINDINGS.md`, `REVIEW_DIFF_COMBINED_UX_REPORT.md`, `REVIEW_DIFF_EXTENDED_SCENARIOS.md`, `REVIEW_DIFF_NNG_USABILITY_EVAL.md`, `REVIEW_DIFF_REMAINING_ISSUES.md`, `review-diff-feature-restoration-plan.md`, `review-diff-rewrite-plan.md`, `review-picker-plan.md`, `SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md` — the review-diff/hunk-parity doc set.
- `fresh-vs-hunk-review-gaps.md`, `hunk-diff-viewer-report.md` (repo root) — `hunk` parity comparison.
- `docs/internal/PLAN-git-log-streaming.md`, `PLAN-git-log-diff-folding-and-highlighting.md` — git-log streaming & fold/highlight.
- `docs/internal/macro-system-improvements.md` — macro persist/edit/promote.
- `docs/internal/design-decisions.md` #6 ("Diff View & Scroll Sync") — folded into §3.
