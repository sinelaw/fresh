# Search, Replace, Diff/Review & Macros

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh implements in-buffer search/replace, project-wide search and live grep, the diff/review (hunk) viewer, git-log viewing, and the keyboard-macro system — the decisions, the scan/diff algorithms, and what is shipped vs. planned.

Throughout: features split into a thin **Rust host** layer (incremental scans, piece-tree diff, overlays, search/replace orchestration, macro codegen) and **TypeScript/QuickJS plugins** that own list/picker UIs (live grep, search/replace, git log, audit mode). The design history repeatedly justifies *adding a host primitive* over a *plugin-side workaround*. Labels below: **IMPLEMENTED** (in code), **PLANNED** (design only / partially stubbed).

---

## 1. In-buffer search & replace (IMPLEMENTED)

The search/replace method cluster was extracted from the render layer in the editor-modules refactor. Pure regex helpers, the large-file scan state, and the search-state types live in separate modules.

### 1.1 Search state & the small/large-file split

The search state holds the query, a sorted vector of match byte offsets, parallel match lengths, the current index, a wrap-search flag, an optional search range (search-in-selection), and a "capped" flag. Matches are bounded at a fixed maximum to bound memory on pathological patterns.

Search branches on file size:
- **Small files / search-in-selection** run inline: the whole buffer (or the selection slice) is loaded as a UTF-8 string and all matches are collected up to the cap.
- **Large (lazy-loaded) files** with no selection start a non-blocking **incremental chunked scan** that processes a few chunks per render frame so the UI stays responsive.

Both paths converge in a single finalize step: it sets the search state, moves the cursor to the first match at/after the cursor, and creates overlays. The overlay strategy differs deliberately:
- **Small files**: overlays for **all** matches, using markers — so positions auto-track buffer edits and Find-Next stays correct after edits.
- **Large files**: **viewport-only** overlays, to avoid multi-GB overlay allocations. The overlay refresh uses a binary search (`partition_point`) on the sorted match vector, so it is O(log N + visible). Overlays re-paint on scroll, but only for large files (small files already cover everything).

### 1.2 Regex construction & line anchoring

The search-regex builder always returns a regex: in plain-text mode the query is regex-escaped, so one code path serves both literal and regex search. Whole-word wraps with `\b…\b`. The key decision: the regex crate defaults to single-line mode where `^`/`$` bind to the haystack boundary — the opposite of what every editor's find does. Both the Unicode search builder and the bytes-regex replace builder enable multi-line and CRLF-aware modes so anchors match every line boundary on LF and CRLF buffers. The chunked-scan path re-applies these flags because the serialized pattern string carries only the pattern, not builder flags. Tests lock in per-line anchoring and that `.` does not cross newlines.

### 1.3 Find-next / find-previous & quick-find

Find-next/previous always searches **from the cursor**, not from a stored index (matching VS Code/IntelliJ). It binary-searches the match positions for the first match strictly after / last strictly before the cursor, with wrap-around governed by the wrap-search flag. For small files it uses **overlay marker positions** as the source of truth (they track edits); for large files it reads the stored match offsets directly.

Quick-find searches the current selection or word under cursor without opening the panel. The word extraction only fires when the cursor sits on a word character — a fix for a case where a jump-to-matching-bracket leaving the cursor on `}` previously let quick-find hijack the query into the bracket plus surrounding words. (An earlier design proposed dedicated quick-find bindings and the "set term, not options" rule; those handlers are now shipped.)

Other shipped fixes worth noting: off-viewport matches center vertically on scroll, while already-visible matches aren't re-scrolled; search-overlay end markers use left gravity so typing right after a match isn't swallowed into the highlight.

### 1.4 Replace-all & query-replace

Replace-all finds **all** matches first, then applies them via **`BulkEdit`** — Delete+Insert event pairs processed in reverse, giving O(n) piece-tree work and a **single undo step** for the whole replace-all. This is the fix for an earlier O(n²) hang where replace-all could take tens of seconds and many gigabytes for a few thousand edits. Regex mode expands capture groups per match; plain mode uses literal matching.

The replacement-template semantics:
- Escape interpretation turns `\n \t \r \\` into control chars, **only in regex mode** (the regex crate's expansion handles only `$N` and leaves `\n` literal). Unknown escapes (e.g. `\q`) and a trailing lone `\` pass through verbatim, to avoid surprising users who didn't mean to escape.
- Replacement normalization rewrites bare `$1` → `${1}` so the regex crate doesn't greedily eat trailing letters into the group name (Python/PCRE semantics).

Interactive **query-replace** is lazy: it finds only the current match, prompts `Replace? (y)es (n)o (a)ll (c)ancel:`, and searches forward-then-wrap for the next. The undo model: each `y`/`n` is one event, but `a`/`!` collects all remaining matches and applies them as one `BulkEdit` = one undo for all. Replacing the current match wraps MoveCursor+Delete+Insert in an atomic batch event.

---

## 2. Project-wide search/replace & live grep

Two plugins, both on the Rust chunked-scan primitives and a shared finder library:
- the persistent search-and-replace panel,
- the floating live-grep overlay,
- plus a git-grep helper and others that reuse the shared finder.

### 2.1 The chunked project-search primitive (IMPLEMENTED)

Four design principles: everything goes through the editor's real abstractions (the filesystem trait plus the piece-tree text buffer), search what the user *sees* (dirty buffers via piece tree, unopened files via the filesystem), no special-casing of large files, plugin is UI-only.

The streaming project grep snapshots dirty buffers on the main thread, spawns an async task that walks the tree respecting `.gitignore`, and processes several files in parallel via a semaphore; each file is wrapped in a text buffer and run through the chunked scanner. Results stream back as JSON match records over the async bridge.

The chunked scanner pre-splits the tree so every leaf is at most one load-chunk in size and builds a chunked-search state; each step processes one chunk, tracking line/column/context incrementally via a running-line cursor (O(chunk), not O(buffer)); a synchronous variant runs on a blocking thread pool. Cross-chunk matches are handled by an overlap-tail window sized to the larger of the query length and a small floor.

Deliberate trade-offs: context truncation on lines longer than the overlap window (affects a tiny fraction of code); a **soft** match cap — the parallel searchers check the running count with relaxed atomics, so the total can slightly exceed the requested maximum (a UI-responsiveness limit, not a contract); `\b` whole-word matching is ASCII-centric.

The scan wrapper carries session metadata and a take/restore dance that lets the orchestrator pass the chunked-search state by mutable reference into the buffer without fighting the borrow checker. The orchestrator drives both the search scan and the line-feed scan one batch per frame, bounded by a configurable read-concurrency. The line scan additionally fans unloaded leaves out to the blocking thread pool for concurrent line-feed counting, which remote filesystems override to count server-side without transferring data.

### 2.2 Replace in project & known bugs

Project replace opens the file as a hidden buffer if needed, sorts matches **descending by byte offset**, and applies all edits as one bulk op, saving via the filesystem. **Two known limitations, present in the shipped path (not design intent):** project replace **bypasses the per-buffer undo stack**, and pressing replace a second time reuses stale byte offsets against now-modified content, which can corrupt the file.

### 2.3 Live Grep float, Resume, and the scope picker

The live-grep overlay is a centered float sized by frame **percentage** so it doesn't jump as results stream in. The provider chain is `git-grep → rg → ag → ack → grep`. The preview highlights all matches and centers on the *visual* row (counting wrapped rows).

**Resume ("Return to Work", IMPLEMENTED):** a live-grep state service caches the prior query, selected index, and the on-screen matches. A resume action re-opens the overlay in the same state **without re-running ripgrep**, enabling the flip-between-editing-and-results flow. The cached results are invalidated on any query keystroke. The match record is kept in core (not the plugin) because Quickfix export needs to land in an editor-owned virtual buffer. A results-snapshot id is reserved for the Quickfix round-trip (PLANNED — unused until that wiring lands).

**Quickfix** moved from bespoke Rust to a shared finder dock panel; the one host need (tearing down the overlay) became a generic cancel-prompt call rather than quickfix-specific code.

**Scope picker (MIXED):** the vision is to grow live grep into one-stop "universal search" with a scope toolbar over project files, ignored/hidden files, open buffers, open terminals, diagnostics, symbols, git history, worktrees, and Orchestrator sessions. **Shipped:** the inline scope toggles dispatched as plugin actions via `Alt+<char>` (no new core Action), terminal search scoped per-cwd via a new host getter for the terminal's directory, and **closed-terminal retention** — on close the backing scrollback is *renamed* to a closed-terminal filename keyed by id and epoch (ids restart per session, so deleting would let a reused id clobber retained logs), GC-bounded by a retention cap, and named with a `.txt` extension so it's searchable with no plugin change. **Planned:** the grouped scopes popover, sources rail, multi-root scopes, and the full-width header-band redesign (flagged for sign-off). The fan-out algorithm — run every enabled scope, merge tagged match records into one capped list with per-source caps and graceful per-source failure — is the design target.

**Replan note (PLANNED):** an in-progress search/replace UX branch must be re-landed atop master's declarative widget-runtime rewrite; per-behavior, master's text-input / toggle / checkable-tree widgets already cover Tab-cycle, search-on-type, and per-row checkboxes. Surviving tickets: empty-state quality, panel passthrough (UI fall-through in the key context), footer hints, **single-file scope** (a post-filter on the streaming callback for v1 — "we eat the wasted scan"), an `Alt+A` binding, and multi-line input.

### 2.4 Quick Open & fuzzy matching (IMPLEMENTED)

The Quick Open system is prefix-routed (longest-prefix-first): empty = files, `>` = command palette, `#` = buffer switcher, `:` = goto-line. `#` also lists virtual plugin buffers such as the git-log buffer. The file provider uses `git ls-files` via the process spawner as the fast path, falling back to a filesystem walk; enumeration runs on a background thread with periodic partial results, plus a synchronous prefix probe for instant path-prefix hits. Results are cwd-keyed so switching projects doesn't serve stale lists; the backends re-point on authority swap (the "host files in a remote session" case).

The fuzzy matcher is fzf-style subsequence matching with a scoring DP and an allocation-amortizing matcher (reusable scratch plus a prepared pattern). Scoring bonuses reward consecutive runs, word boundaries, start-of-string, camelCase transitions, contiguous substrings, and — decisively — **basename-prefix** and **path-segment-prefix**, so typing `ts` ranks `tsconfig.json` above `pkg.ts`. Multi-term (space-separated) queries match each term independently and combine scores, with a tight-span bonus rewarding targets that reconstruct the query across a single separator (`/etc/hosts`, `saveFile`).

---

## 3. Diff/review (hunk) viewer

Three layers: the **pure piece-tree diff** and **line diff** (core), the **composite-buffer side-by-side renderer** (host), and the **audit-mode review session** (plugin).

### 3.1 Pure diff algorithms (IMPLEMENTED)

**Line diff:** a classic LCS over lines for the saved-vs-current "modified line" gutter. It computes the LCS DP table, backtracks, then marks current lines not in the LCS as insertions/modifications and deletion points where saved lines vanished. A classifier distinguishes Inserted/Modified/Deleted by LCS-alignment context. Extensive unit and property tests verify only the truly-changed line is marked, never shifted lines.

**Piece-tree diff:** a structural diff that exploits the piece tree's `Arc` sharing. It short-circuits identical subtrees via pointer equality in O(1), so after path-copying edits it is **O(changed-path), not O(all-leaves)**. It walks both trees in parallel, collects leaves only from differing subtrees with **document-absolute byte offsets** (an offset-tracking fix for gutter indicators appearing at the wrong place in large files), then computes a longest common prefix/suffix at byte granularity and reports the changed byte ranges. A rebalance test confirms identical byte ranges whether or not `Arc` sharing survives. This backs the buffer's diff-since-saved query and the saved-diff exposed to plugins. The "modified line" gutter renders from it by intersecting the changed byte ranges with the viewport. Diff navigation consumes the same byte ranges as one of its three jump sources (git-gutter hunks, live-diff hunks, and saved-diff).

### 3.2 Side-by-side composite buffers & rebindable hunk nav

Side-by-side drill-down is a **composite buffer** rendered as two-column OLD|NEW text. The deliberate constraint: composite buffers can't occupy a panel slot (the host rejects buffers outside the window's buffer map), so the in-panel side-by-side renders two columns inside the existing center buffer rather than a full-takeover that hid the sidebar.

The composite model holds panes, a layout mode, a display-line→old/new alignment map, and an initial-focus hunk for scroll centering; view state is tracked per pane; rendering reuses the per-buffer view-data builder per pane to get syntax highlighting, selection, and inline diff highlights for free. Cursor/selection movement is re-implemented in display-row space rather than routed to source buffers, to handle padding rows and synced scrolling.

Hunk navigation scans the alignment for hunk boundaries and scrolls to center the target hunk roughly one-third from the top. These keys are currently **hardcoded in the composite input router**, intercepted before the Action keybinding system, so the keybinding editor can't rebind them (PLANNED to add next-/prev-hunk Actions under a dedicated composite-buffer key context).

A recurring root cause across review bugs is the **outer-vs-inner split-leaf mismatch** in buffer groups: handlers used the active split where they needed the effective active pair/split, so cursor lookups missed the inner composite view-state (one instance forced an O(n) move-down loop for thousand-line diffs).

### 3.3 The review session & parity with the external `hunk` tool

Audit mode is the magit-style review tool: `git status --porcelain -z` as the single source of truth, STAGED/UNSTAGED/UNTRACKED grouping, and **real git staging** (`git add` / `git reset HEAD` / `git checkout`) at file/hunk/line granularity. It renders exactly one viewport of lines into a single edit-disabled virtual buffer to prevent buffer-wide scrolling. Hunk-level staging uses `git apply --cached` against a tempfile — the canonical method (git's own add-patch, magit, lazygit) — chosen because the process spawner has no stdin piping, with a `--check` dry-run first. Actions are **context-sensitive**: the same key (`s`) stages whatever the cursor is on (file in the file pane, hunk in the diff pane).

**Parity goal:** the through-line is *"hunk is a better reader; Fresh is a better actor"* — make Review Diff **read** as well as the external `hunk` tool and **act** as well as Fresh.

| `hunk` has (Fresh lacks) | Fresh has (`hunk` lacks) |
|---|---|
| live split/stack/auto layouts (`1`/`2`/`0`) | real git stage/unstage/discard at hunk/line/file |
| per-token syntax highlight both panes + word-level intraline | index-aware STAGED/UNSTAGED/UNTRACKED grouping |
| 24-bit truecolor + diff-specific themes | jump-to-and-edit the real file in-app |
| dedicated file sidebar with `+N/−N`, `*N` comment badges | comment persistence to `.review/` + MD/JSON export |
| `?` keymap overlay, `F10` menu, multi-line bordered notes | PR-branch and range review modes |
| agent daemon + `hunk session` CLI | — |

The v2 design (PLANNED, nothing built yet) refuses a new bespoke renderer: it composes from existing primitives — **split = composite buffer, stack = virtual buffer**, layout toggle just swaps which occupies the panel; everything keyboard-driven is a rebindable Action in a `diff-view` mode (not a hardcoded router — "the v1 mistake"); colors come from theme keys, never hardcoded RGB; inline notes via virtual-line/virtual-text APIs; the planned agent surface reuses the existing local-control IPC and native review-hunk dispatch rather than a new daemon (the host already exposes review-hunk state and a set-review-diff-hunks command) — and goes "beyond hunk" because the human can edit+stage in the same window. The one intentional key divergence: keep `s`=stage (Fresh's identity), move sidebar to `\` (hunk uses `s`=sidebar).

**The most-flagged remaining reader gap across the findings docs:** the unified/stack pane has **no per-keyword syntax highlighting** (side-by-side via composite buffers already does). Analysis traces the whitespace-only per-char highlight to the inline-overlay background being overwritten by a whole-entry extend-to-line-end fill (needs an overlay-priority API addition).

**Other diff features:**
- **Next/Previous Change** (PLANNED; the diff-nav plugin exists): merges git hunks plus piece-tree saved-diff byte ranges into one jump list, deduped within a couple of bytes, on VS Code-style keys.
- **live-diff plugin** (IMPLEMENTED): inline diff in the live buffer, updating on insert/delete so external writes (a coding agent editing the file on disk) show live; per-buffer reference (HEAD/Disk/Branch).
- **review picker** (PLANNED): one **Review** command opening a two-screen picker with a `★ This PR` smart default, debounced live preview, and "since I last reviewed (N new)" watermarks.

---

## 4. Git-log viewing

The git-log plugin plus a shared git-history library. Two evolutions, both with concrete decisions:

**Streaming (PARTIALLY SHIPPED):** opening a commit's `git show` diff used to buffer tens of megabytes of stdout into JS (multi-second time-to-first-paint). The shipped path pipes stdout straight to a temp file (bytes never enter JS) and opens it as a file-backed buffer that grows. Refreshing re-stats and does an **O(1) length bump**, appending an unloaded tail piece (shrink is treated as corruption and ignored). Growth is **polled at a few frames per second**, not file-watched. Time-to-first-paint drops to well under a tenth of a second (the buffer opens empty and grows under the cursor). A prior `--numstat` pre-pass and per-line overlay construction (around a million objects) were dropped. A further step (PLANNED) re-targets the panel at a per-commit, SHA-cached diff buffer, with process-kill cancellation on selection change (today holding the down key leaks a trail of zombie git processes).

**Folding & highlighting (PLANNED):** Part 1 makes fold-by-file/hunk scale via an incremental, append-only fold scan over line-indexed folding ranges, capped per pass to handle the cache-hit "one giant multi-gigabyte scan" case. The publish-folding-structure primitive already landed; earlier "Fold All" commands were **removed** because they sat on the wrong primitive (immediate-collapse folds). Part 2 replaces plugin per-line diff overlays with syntect's bundled diff grammar — the bug is that the scope-to-category mapping doesn't map the diff insert/delete scopes; the plan adds inserted/deleted/changed highlight categories with a background pathway, reusing existing diff background theme keys, with whole-line background fill gated by an extends-to-line-end check. Current overlay highlighting is gated by a byte-size threshold.

**Widget migration (IMPLEMENTED):** the toolbar and log pane moved off hand-rolled panel-content plus utf8 byte-offset hit-testing onto host panel/list widgets — the byte-offset arithmetic "was the most error-prone part of the plugin." Accepted trade-off: uniform Button styling replaced richer per-button colors (the custom overlay arithmetic "doesn't compose with the widget runtime"). The detail pane stayed on raw panel content ("no concrete payoff"). Per-line diff highlighting runs **once after streaming settles**, coalescing same-kind rows, gated by a byte-size threshold. Other shipped niceties: a "Git Log (Current File)" command scopes to the focused file via a dedicated context; re-invoking activates the existing tab.

---

## 5. Keyboard macros (IMPLEMENTED)

Macro state, action orchestration, and codegen live in separate modules. Design status: implemented.

### 5.1 Record / play

The macro state owns a register map from character to action list, the in-flight recording state, the last register (for "play last"), and a playing flag. Recording appends actions but skips macro-control actions — recording a "start recording" would cause infinite regress on replay — and never records while playing.

Recording special-cases the confirm prompt: it snapshots the prompt text *now* into a with-text variant (bypassing the control filter), so replay uses the user's original input rather than whatever the prompt holds at replay time.

Playback replays actions through the action handler in a tight loop, recomputing layout between each so visual-line moves see fresh layout. Replay is **one undo unit**: it brackets the whole playback with begin/end undo-group calls on the buffer that owned the log at start (so a mid-replay buffer switch can't leave a group dangling).

Showing a macro renders it as a read-only action-spec view; listing macros lists all registers.

### 5.2 Persist / edit / promote via init.ts (recent)

The decision: reuse `init.ts` as the macro store rather than inventing a `macros.json`, because init.ts already has persistence, hot-reload, type-checking, and crash recovery — and it unlocks the "arbitrary logic" endpoint. The single enabling fix was adding optional `args` to the action-spec shape (the dispatch path previously always passed empty args, so a char-insert action couldn't replay). With it, fetching a macro returns the same action-spec shape the execute-actions API consumes: **"a macro is a script and a script is a macro."**

Codegen renders a recorded action list into two `init.ts` forms, both wrapped in `// fresh:macro <key>` … `// fresh:end macro <key>` sentinels so the upsert rewrites in place instead of duplicating:
- **Save** — emits a `defineMacro` call re-seeding the register at startup so `@q` works in a fresh session.
- **Promote** — emits a handler/command stub whose body is the steps wrapped in execute-actions, ready to edit into arbitrary logic (loops, all-cursors access, etc.).

Both emit a `// types:` comment coalescing runs of char inserts for readability. Writing the macro upserts the block and hot-reloads init.ts so the macro is live immediately. The capability ramp: **record (@q) → save to init.ts → promote to a handler → arbitrary plugin code**, each step one command away.

**Removed (effective revert):** a "Macro: Load from buffer" command was prototyped then dropped — its hand-rolled JSON-array parser "broke on the ShowMacro header comment," and init.ts is already a robust edit surface, so it was "the lone hand-parse and its bug class." Showing a macro stays read-only. **Deferred:** a strict-mode lint flagging hand-edited macros referencing unknown action names (today unknown actions degrade to a no-op and `defineMacro` only logs a warning).

---

## Summary + superseded docs

This document consolidates Fresh's search/replace, diff/review, git-log, and macro subsystems. The unifying patterns are: a thin Rust host providing incremental chunked scans (load-chunk-sized leaves, soft-capped match counts, one batch per render frame), a pointer-equality structural piece-tree diff that costs O(changed-path), `BulkEdit`-based replace for O(n) single-undo edits, and feature UIs delivered as QuickJS plugins on a shared finder/widget runtime. Search is regex-built with per-line `^`/`$` anchoring; small files track matches via edit-following overlay markers while large files use viewport-only overlays and an incremental scan. Macros are now persistable and promotable to arbitrary code by reusing init.ts as the store, enabled by adding `args` to the action-spec shape so recorded keystrokes replay faithfully.

The most load-bearing *planned-vs-shipped* distinctions: in-buffer search/replace, the chunked project-search primitive, live-grep Resume, per-cwd/closed-terminal scope retention, the pure diff algorithms, composite side-by-side drill-down, real git staging in audit mode, the shipped git-log streaming path, and the full macro save/promote pipeline are **shipped**; the universal-search scope popover/header-band, rebindable composite hunk-nav Actions, the v2 hunk-parity reader (layouts/sidebar/agent CLI), unified-pane syntax highlighting, the further git-log streaming step plus fold/highlight scaling, and the review picker are **planned**. Two shipped limitations are documented as real (not design intent): project replace bypasses undo, and it reuses stale offsets on a second apply.

Old docs this supersedes / absorbs (move-to-archive candidates):
- the universal-search vision doc — shipped subset plus planned popover.
- the project search/replace design doc and its bug-reproduction note.
- the quick-find / next-occurrence doc — now shipped.
- the widget-runtime search/replace replan doc.
- the finder library doc — built.
- the diff UX and Next/Prev Change docs.
- the review-diff/hunk-parity doc set (design, findings, usability evals, restoration and rewrite plans, review picker, rebindable hunk-nav).
- the `hunk` parity comparison notes.
- the git-log streaming and fold/highlight plan docs.
- the macro persist/edit/promote doc.
- the design-decisions entry on "Diff View & Scroll Sync" — folded into §3.
