# Global / Universal Search — UX Design

> **Status**: Design Document (UX exploration — wireframes + scope taxonomy)
> **Date**: May 2026
> **Branch**: `claude/live-grep-global-search-ux-EFOGo`
> **Driving idea**: Grow today's Live Grep (project-wide text search) into a
> single **"one-stop" universal search** that can also look inside terminals
> (including closed ones), diagnostics, git history, other worktrees, and
> all Orchestrator sessions — with an explicit, visible scope picker so the
> user controls *where* the search runs.

---

## 1. Goal

Live Grep today searches **project files only**. The ask is to turn it into a
universal search surface where the user can opt scopes in and out from
checkboxes in the overlay's toolbar, so it becomes the place you reach for
*whenever you're trying to find anything* — not just text in tracked files.

Two concrete behavioural changes are wanted up front:

1. **Stop searching ignored files by default.** Today this is perceived as
   noisy; ignored/hidden files should be an explicit, off-by-default opt-in.
2. **Include open terminals by default.** Terminal scrollback is often where
   the thing you're hunting for actually is (a stack trace, a printed path, a
   command you ran).

Everything else (closed-terminal history, diagnostics, git history, other
worktrees, all sessions, …) is opt-in.

---

## 2. How Live Grep works today (grounding)

Captured live from this repo (`Alt+/` on `crates/fresh-editor/src/app/workspace.rs`,
query `terminal`):

```
┌────────────────────────────────────────────────────────────────────┬───────────────────────────────┐
│Live grep: terminal                                          1 / 1000 │▾ 29 │   else                  │
│Provider: git-grep · 1000+ matches · Alt+P switch grep provider · Alt+M save … │ 30 │   VERSION=$(grep …    │  ← PREVIEW
│──────────────────────────────────────────────────────────────────────────────│ 31 │   fi                  │     PANE
│  .github/workflows/release-npm.yml:54  "description": "A modern termi… │ 32 │   echo "version=…    │
│  CHANGELOG.md:9                        New built-in **environment-man… │ 33 │   echo "Publishing…  │
│  CHANGELOG.md:43                       * **Terminals**: line-number g… │ 34 │                       │
│  CHANGELOG.md:45                       * **Closed terminals no longer… │ 35 │   - name: Setup Node… │
│  … (results list, grouped/streamed)                                    │ …                       │
└────────────────────────────────────────────────────────────────────┴───────────────────────────────┘
 Found 1000 matches                                                          [status bar]
```

Anatomy of the floating overlay:

- **Row 1 — input**: `Live grep: <query>` with a right-aligned `selected / total`
  counter.
- **Row 2 — toolbar/title**: `Provider: <name> · [1000+ matches] · <hint> · <hint>`.
  This is the only "chrome" row today and it is **purely informational** — there
  are no controls in it.
- **Separator**, then a two-pane body: **results list** (left, `file:line  content`)
  and a **preview pane** (right, the file around the match).
- **Status bar** (host, bottom): `Found N matches`.

Implementation facts that shape the design:

- **It's a plugin.** `crates/fresh-editor/plugins/live_grep.ts` drives a
  `Finder` overlay (`plugins/lib/finder.ts`) and owns a **provider registry**:
  `git-grep` (default inside a repo) → `rg` → `ag` → `ack` → `grep`, selected by
  priority + an `isAvailable()` probe. Each provider shells out and normalises
  to `{ file, line, column, content }`. `Alt+P` cycles providers.
- **Results are homogeneous today** — every row is a `file:line:col` location, so
  "open" just means `editor.openFile(file, line, col)`.
- **A "Return to Work" resume cache** lives in core
  (`services/live_grep_state.rs`): last query, selected index, and a *display*
  cache of matches so `Alt+R` re-opens the overlay without re-running the search.
- **Checkbox/toggle widgets already exist.** `plugins/lib/widgets.ts` exports
  `toggle(checked, label)` rendered as `[v] label` / `[ ] label`, plus
  `row`/`col`/`button`/`spacer`/`flexSpacer`. `search_replace.ts` already ships
  an options row built from these (`[v] All files  [ ] Case  [ ] Regex  [ ] Word`),
  with `Alt+C/R/W` mnemonics and Tab focus. **The new toolbar should reuse this
  exact vocabulary** for consistency (NN/g #4).

### 2.1 Terminal scrollback is *already* under the data dir

A central worry in the brief was "we may need to move scrollback buffers under
the data dir." Good news: **they already are.**

- `DirectoryContext::terminals_dir()` → `data_dir/terminals`
  (`$XDG_DATA_HOME/fresh/terminals/` on Linux), and
  `terminal_dir_for(working_dir)` namespaces by an encoded working-dir path
  (`config_io.rs`). Backing files are *not* written into the project tree.
- Each terminal streams scrollback incrementally to its backing file
  (`services/terminal/term.rs::flush_new_scrollback`).

So the storage **location** is fine. The blocker for searching *closed*
terminals is the **retention policy**: on explicit close, the backing file is
**deleted** —

```
// crates/fresh-editor/src/app/buffer_close.rs (close path)
let _ = self.authority.filesystem.remove_file(path);   // backing file
```

**Required change for closed-terminal search:** stop deleting on close; instead
**retain** the backing file and garbage-collect by age / count / total size.
A small **index** (terminal id → cwd, shell, title, closed-at, byte size, path)
makes retained terminals discoverable and GC-able. This is the only storage
change needed; see §8.

### 2.2 Where the "searches ignored files" feeling comes from

The default provider (`git-grep`) and `rg` both respect `.gitignore`. The `rg`
built-in additionally hard-excludes `.git`, `node_modules`, `target`, `*.lock`.
The annoyance is real in two cases: (a) when no VCS provider is available and a
raw `grep -rn .` runs, and (b) for users who *want* ignored content excluded but
have no visible control to confirm it. Either way the fix is the same: make
"ignored & hidden files" a **visible, off-by-default toggle** rather than a
provider-dependent accident (NN/g #6 recognition, #1 visibility).

---

## 3. What might a user want to search in?

The core design question. Below is the full candidate taxonomy, grouped, with a
proposed default. The grouping matters because it drives the wireframes — 12 flat
checkboxes is a usability failure (NN/g #8 minimalist design), but 4 groups of 2–3
is scannable.

| Group | Scope | Default | Notes / how |
|-------|-------|:------:|------|
| **Files** | Project files (tracked / not ignored) | **ON** | today's behaviour |
| | Ignored & hidden files (`.gitignore`, dotfiles) | OFF | flips `rg`/`grep` flags; fixes §2.2 |
| | Open buffers incl. **unsaved edits** | **ON** | on-disk grep misses unsaved changes; search live buffer text |
| **Terminals** | Open terminals (live scrollback) | **ON** | search current backing files |
| | Closed terminal history (retained) | OFF | needs §8 retention; can be large/old |
| **Code intelligence** | LSP diagnostics (all buffers / workspace) | OFF | match on message text; jump to range |
| | Workspace symbols | OFF | `workspace/symbol`; different match semantics (names not lines) |
| **History / VCS** | Git history (commit messages + diffs) | OFF | `git log -G<re>` / `git grep <rev>`; jump to commit/line |
| **Multi-root** | All git worktrees | OFF | iterate worktree roots |
| | All Orchestrator sessions | OFF | each session ≈ one worktree (see orchestrator-sessions-design.md) |

**Stretch / later candidates** (enumerated so we don't forget them, but *not*
proposed for v1 — adding them now would violate minimalist design):

- File **names / paths** (find a file, not its contents) — overlaps Quick Open.
- **Bookmarks** (`app/bookmarks.rs`) and TODO/FIXME markers.
- **Recent files / file history**, **command history**, **clipboard/kill-ring**.
- **Settings keys**, **help/docs**, **command palette** commands.
- **Quickfix snapshots** already exported from prior searches.
- **Plugin / LSP logs** under the state dir.

> **Design stance:** ship the 10 grouped scopes above; expose the long tail later
> behind the same "more scopes" affordance rather than the toolbar. A "one-stop
> search" is judged by *trustworthy defaults + easy opt-in*, not by how many
> checkboxes are visible at once.

### 3.1 Defaults on open (the "same as today, mostly" rule)

Opening universal search (the rebound `Alt+/`) starts with:

```
[v] Project files   [ ] Ignored/hidden   [v] Open buffers   [v] Open terminals
[ ] Closed terminals  [ ] Diagnostics  [ ] Symbols  [ ] Git history  [ ] Worktrees  [ ] Sessions
```

i.e. **today's project search, minus ignored files, plus open terminals + unsaved
buffers.** Toggles are **sticky** within a session and persisted to workspace
state, so a user who turns on "Git history" keeps it until they turn it off.
A `Default scope` reset (and named presets, §6/Alt-D) covers "put it back."

---

## 4. NN/g heuristics driving the design

| Heuristic | Application |
|-----------|-------------|
| #1 Visibility of system status | Per-scope **match counts** in the toolbar (`Files 320 · Terminals 14 · Git 5`); spinner per source while streaming; clear zero-result state. |
| #2 Match real world | Plain labels ("Open terminals", "Git history"), not flags (`--no-ignore`). |
| #3 User control & freedom | One keystroke per scope; `Esc` closes; resume (`Alt+R`) unchanged. |
| #4 Consistency & standards | Reuse `toggle()` widget + `Alt+letter` pattern from `search_replace.ts`. |
| #5 Error prevention | Ignored/hidden **off** by default; expensive scopes (closed terminals, all sessions) off by default and visibly labelled. |
| #6 Recognition over recall | Scopes are **visible checkboxes**, not memorised CLI flags. |
| #7 Flexibility & efficiency | Mnemonics for experts; **presets** ("Code", "Everything", "Terminals") for one-shot scoping. |
| #8 Aesthetic & minimalist | **Grouping + progressive disclosure** — primary toggles inline, long tail behind "More ▾". |
| #9 Recover from errors | If a source fails (no `git`, provider missing) show a per-source inline note, don't fail the whole search. |
| #10 Help & docs | Hint row shows the mnemonics; each scope has a tooltip/description line. |

Central tension: **discoverability of many scopes** vs **a crowded toolbar in a
narrow terminal**. The captured overlay's toolbar row already nearly overflows
with just provider + 2 hints. The alternatives below differ mainly in *how they
resolve this tension*.

---

## 5. Wireframe alternatives

All wireframes keep the existing two-pane body (results + preview) and status
bar; they differ in the **toolbar / scope-control region**.

### Alternative A — Flat inline checkbox toolbar (wrap to 2 rows)

The literal reading of the brief: more checkboxes in the top toolbar row.

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Search: terminal│                                                            128 matches · 1 / 128   │
│ [v] Project  [ ] Ignored  [v] Buffers  [v] Terminals  [ ] Closed term  [ ] Diagnostics  [ ] Git ▸  │
│ Provider: git-grep · Alt+P provider · Alt+M save · Tab to focus scopes                              │
│────────────────────────────────────────────────────────────────────────────────────────────────── │
│  src/app/workspace.rs:12   all terminal backing files contain complete state …                     │
│  term/manager.rs:465       "Terminal backing file write error: {}"                                  │
│  ⟫ TERMINAL  (closed) build-2  npm ERR! code ELIFECYCLE  …                          [source: term]  │
│  …                                                                                                  │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
```

- **Pros**: dead simple, exactly matches `search_replace.ts`, zero new widgets,
  fully keyboard-navigable today.
- **Cons**: 10 scopes do **not** fit on one 80-col row; wrapping to 2–3 rows eats
  vertical space that the results list needs; the `▸` overflow for the tail is
  awkward; no per-source counts without more width.
- **Verdict**: fine for ≤4 scopes, breaks down at 10. Good fallback for narrow
  terminals (see Alt-B's responsive collapse).

### Alternative B — Primary toggles inline + "Scopes ▾" popover (RECOMMENDED)

Progressive disclosure. The **4 default-on-ish scopes** stay inline; everything
else lives behind a `Scopes ▾` button that opens a checklist popover. The
button shows a summary count of active scopes.

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Search: terminal│                                                            128 matches · 1 / 128   │
│ [v] Files  [ ] Ignored  [v] Buffers  [v] Terminals   ▏  Scopes (4) ▾   ▏  Provider: git-grep        │
│────────────────────────────────────────────────────────────────────────────────────────────────── │
│  Files 96 · Buffers 18 · Terminals 14                                       Alt+P provider · Alt+M  │
│────────────────────────────────────────────────────────────────────────────────────────────────── │
│  src/app/workspace.rs:12   all terminal backing files contain complete state …                     │
│  … results …                                                                                        │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘

   pressing  Scopes ▾  (or Tab → Space) drops a popover:

   ┌─ Search in ───────────────────────────┐
   │ FILES                                  │
   │   [v] Project files          Alt+1     │
   │   [ ] Ignored & hidden       Alt+2     │
   │   [v] Open buffers (unsaved) Alt+3     │
   │ TERMINALS                              │
   │   [v] Open terminals         Alt+4     │
   │   [ ] Closed terminal history Alt+5    │
   │ CODE INTEL                             │
   │   [ ] Diagnostics            Alt+6     │
   │   [ ] Workspace symbols      Alt+7     │
   │ HISTORY & SCOPE                        │
   │   [ ] Git history            Alt+8     │
   │   [ ] All worktrees          Alt+9     │
   │   [ ] All sessions           Alt+0     │
   │ ─────────────────────────────────────  │
   │  Presets: Code · Everything · Terminals │
   │  [Reset to defaults]                    │
   └─────────────────────────────────────────┘
```

- **Pros**: toolbar stays uncluttered (NN/g #8); the popover *groups* scopes so
  they're scannable (NN/g #6); per-source counts fit on the secondary row
  (NN/g #1); scales to the long-tail scopes without redesign; the inline 4 are
  the high-frequency ones so most sessions never open the popover.
- **Cons**: the rarer scopes are one interaction away (acceptable — they're rare);
  one new popover widget (though it's just a `col()` of `toggle()`s in an
  existing overlay).
- **Verdict**: best balance of discoverability and restraint. **Recommended**,
  ideally combined with Alt-D presets inside the popover. Narrow terminals
  collapse the inline 4 into the popover too (everything behind `Scopes ▾`),
  which is the responsive answer to Alt-A's wrapping problem.

### Alternative C — Left "Sources" rail (three-pane)

A persistent vertical list of sources on the far left, each a checkbox with its
own live count; results in the middle; preview on the right.

```
┌────────────────┬───────────────────────────────────────────────┬───────────────────────────────┐
│ SEARCH IN      │ Search: terminal                  1 / 128      │  PREVIEW                       │
│ [v] Files   96 │───────────────────────────────────────────────│  12 │ all terminal backing …   │
│ [ ] Ignored  – │  src/app/workspace.rs:12  all terminal back…   │  13 │ before serializing …     │
│ [v] Buffers 18 │  term/manager.rs:465      "Terminal backing…   │  …                             │
│ [v] Terminal14 │  ⟫ build-2 (closed)  npm ERR! ELIFECYCLE …     │                                │
│ [ ] Closed   – │  diag  workspace.rs:120  unused import `Foo`   │                                │
│ [ ] Diagnos  – │  git  a1b2c3d  "fix terminal backing file…"    │                                │
│ [ ] Symbols  – │  …                                             │                                │
│ [ ] Git      – │                                                │                                │
│ [ ] Worktree – │                                                │                                │
│ [ ] Sessions – │                                                │                                │
│ ────────────── │                                                │                                │
│ Presets ▸      │                                                │                                │
└────────────────┴───────────────────────────────────────────────┴───────────────────────────────┘
```

- **Pros**: every scope + its live count visible at once (strong NN/g #1); toggling
  a source instantly re-filters; mirrors VS Code's search-sources mental model;
  great on wide screens.
- **Cons**: three panes is *tight* under ~120 cols — preview gets squeezed; a big
  departure from today's two-pane overlay (more implementation + a new layout);
  the rail is always-on chrome even for users who never change scopes.
- **Verdict**: most powerful for "command-center" search, best for wide monitors.
  Hold as a **power-user / future** layout, possibly a toggle (`Alt+B` shows/hides
  the rail). Not the default because of the narrow-terminal squeeze.

### Alternative D — Scope **presets** (segmented control)

Lead with named presets instead of individual checkboxes; expose the raw toggles
only via "Customize."

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Search: terminal│                                                            128 matches · 1 / 128   │
│ Scope:  ( Code )  Everything   Terminals   This session    ·   Customize ▾                          │
│────────────────────────────────────────────────────────────────────────────────────────────────── │
│  … results, grouped by source …                                                                     │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘

  Code         = Project files + Open buffers
  Everything   = + Ignored/hidden + Terminals (open+closed) + Diagnostics + Git history
  Terminals    = Open + Closed terminal history only
  This session = Files + Buffers + Terminals for the active worktree/session only
```

- **Pros**: fastest for the common intents (NN/g #7); tiny footprint; "Everything"
  literally delivers the "one-stop" promise in one click; great recognition.
- **Cons**: presets hide *which* scopes are active unless we annotate; users still
  need the raw toggles for odd combinations (hence "Customize"); preset names need
  care to be self-explanatory.
- **Verdict**: excellent **complement**, weak as the sole mechanism. **Fold the
  presets into Alt-B's popover** (and optionally surface them as a one-line
  segmented control) — presets for speed, checkboxes for precision.

---

## 6. Recommendation

**Adopt Alternative B (inline primary toggles + grouped `Scopes ▾` popover) with
Alternative D presets embedded in that popover**, and keep **Alternative C's
sources-rail as an opt-in power layout** behind a toggle for wide terminals.

Rationale: B keeps the everyday overlay calm and fast (most searches use the four
default scopes and never open the popover), satisfies discoverability through a
*grouped* checklist rather than a wall of checkboxes, and scales to the long-tail
scopes without another redesign. D makes the "one-stop everything" intent a single
choice. C remains the answer for users who want a persistent command center on a
big screen.

Responsive behaviour: under a width threshold, collapse the inline toggles into
`Scopes ▾` so a narrow terminal never wraps the toolbar (this is the principled
fix for Alt-A's overflow).

---

## 7. Cross-source result model

Universal search breaks the "every row is `file:line`" assumption. A unified
result needs:

- a **source tag** (small glyph or short label, themed) so a row's origin is
  obvious at a glance — e.g. `file`, `buf`, `term`, `diag`, `sym`, `git`;
- **grouping by source** in the list (collapsible headers with counts), so a
  burst of terminal hits doesn't bury file hits;
- a **type-appropriate activate action**:
  - file / buffer / ignored → open at `file:line:col` (today's path);
  - open terminal → focus that terminal + scroll to the line in scrollback;
  - closed terminal → open its retained backing file as a read-only buffer at
    the line (we already restore terminals this way — `workspace.rs::
    load_terminal_backing_file_as_buffer`);
  - diagnostic → open the buffer at the diagnostic range;
  - symbol → go-to-definition location;
  - git history → open the commit/diff (reuse `git_log` plugin) at the hit.

The plugin's provider registry generalises cleanly: today providers return
`GrepMatch[]`; universal search makes each **scope** a streaming source that
yields tagged results into one merged, capped, debounced list. Per-source caps
keep one chatty source (e.g. a 200 MB terminal log) from starving the rest, and
per-source failures degrade gracefully (NN/g #9) — a missing `git` binary shows
"Git history: git not found", it doesn't abort the file search.

---

## 8. Storage / retention change (closed terminals)

The only backend change the UX depends on. Scrollback already lives under
`data_dir/terminals/<encoded-cwd>/`; the work is **retention + discovery**:

1. **Don't delete on close.** In `buffer_close.rs`, stop `remove_file`-ing the
   terminal backing file when "search closed terminals" is a feature; move that
   file into a retained area (or just leave it and mark it closed).
2. **Index retained terminals.** A manifest (JSON under `data_dir/terminals/`)
   mapping `terminal_id → { cwd, shell, title, closed_at, bytes, path }` so the
   closed-terminals scope can enumerate, label ("`build-2 (closed 2h ago)`"),
   GC, and open them.
3. **Garbage collect.** Bound by count / age / total bytes (config). Without a
   cap, retained scrollback grows unbounded — note `term.rs` already keeps a
   "generous scrollback" per live terminal, so retained logs can be large.
4. **Scope by cwd by default.** Because files are namespaced per working dir,
   "closed terminals" naturally means *this project's* closed terminals; "all
   sessions / all worktrees" widens to other encoded-cwd dirs.

Privacy note worth flagging to the user: retaining closed-terminal scrollback
means commands and their output (possibly secrets) persist on disk after close.
Retention should be **opt-in / configurable** with a clear setting and a way to
purge (NN/g #5, #3).

---

## 9. Keyboard & interaction model

- Rebind `Alt+/` to open **universal search** (the overlay below); keep
  `Alt+R` resume and `Alt+P` provider-cycle behaviour.
- **Tab** moves focus into the toolbar; **Space** toggles the focused scope;
  **`Alt+1…0`** toggle scopes directly (mnemonic map shown in the popover).
- `Scopes ▾` opens/closes the popover (e.g. `Alt+S`).
- Preset keys optional (e.g. number row when popover focused) — or click.
- Toggling any scope **re-runs** the (debounced) search and updates per-source
  counts; flipping a scope off is instant client-side hide when results are
  cached, full re-run otherwise.
- Sticky scopes persisted to workspace state; `Reset to defaults` restores §3.1.

---

## 10. Phasing

1. **P0 — Scope toolbar + the two behavioural fixes.** Inline toggles
   (Alt-B primary row) for Files / Ignored / Buffers / Open terminals. Default
   ignored **off**, terminals **on**. No backend retention yet (open terminals
   only). Delivers the headline asks immediately.
2. **P1 — `Scopes ▾` popover + grouping + per-source counts + tagged results.**
   Adds Diagnostics, Symbols, Git history as streaming sources.
3. **P2 — Closed-terminal retention (§8)** and the Closed-terminals scope.
4. **P3 — Multi-root**: All worktrees, All Orchestrator sessions.
5. **P4 — Presets (Alt-D) and the opt-in sources rail (Alt-C)** for wide
   terminals; long-tail scopes (names, bookmarks, history, clipboard…).

---

## 10b. Implementation status (live)

Landed in `live_grep.ts` + keymap + i18n (no core Rust changes):

- **Scope model & fan-out.** `search()` runs every enabled scope and
  merges tagged `GrepMatch`es into one capped list; non-file rows carry a
  source badge (`ign` / `buf` / `term` / `diag`).
- **Toolbar checkboxes** rendered via `setPromptTitle`
  (`[v] Files  [ ] Ignored  [v] Buffers  [v] Terminals  [ ] Diagnostics · Provider: …`).
- **Toggles need no new core Action.** In prompt context the host resolves
  any `Alt+<char>` against the keymap and dispatches unknown action names
  as plugin actions, so `Alt+L/H/U/T/D` → `live_grep_toggle_*` handlers.
  This is the reusable mechanism for future scope toggles.
- **Scopes implemented:** Files (provider), Ignored (rg `--no-ignore
  --hidden` / git-grep `--untracked --no-exclude-standard`), Buffers
  (modified open buffers via `getBufferText`), Terminals (grep the
  `<data_dir>/terminals/*.txt` backing files, ANSI-stripped), Diagnostics
  (`getAllDiagnostics`). Defaults: Files/Buffers/Terminals on, Ignored/
  Diagnostics off.

Known limitations / follow-ups:

- Terminals scope currently spans **all** projects' terminals, not just the
  current cwd — cwd-scoping needs the host's `encode_path_for_filename`
  (percent-encodes non-ASCII, so not safely replicated in JS). Best solved
  by the `listTerminalLogs()` host API in §8.
- Terminal hits open the **backing file** at the line, not the live
  terminal. Fine for "find it"; focusing the live terminal is a refinement.
- Toggles only work in the live overlay, not the `Resume` (cached) overlay.

### Closed-terminal retention — LANDED

Backing files are named by terminal id: `<data_dir>/terminals/<encoded-cwd>/
fresh-terminal-<id>.txt`. Terminal ids restart per session, so simply
*not deleting* the file on close would let a new terminal with the same id
**clobber** a retained log from a prior session.

Implemented in `buffer_close.rs`: on terminal close the rendered backing
file is **renamed** to `fresh-terminal-<id>-closed-<epoch_ms>.txt` (instead
of deleted), which is collision-free against future same-id terminals. The
raw `.log` is still deleted. A count-bounded GC
(`gc_retained_terminal_backings`, currently `MAX_RETAINED = 200` per dir)
prunes the oldest retained files, ordering by the epoch embedded in the
filename so it needs no filesystem metadata. The Terminals scope already
globs `*.txt`, so retained logs are searchable with no plugin change —
verified end-to-end: open terminal → produce scrollback → close → Universal
Search finds the hit in the `-closed-` file.

Retention is currently unconditional (the chosen "on by default" stance).
Follow-ups: a config toggle to disable, a "purge terminal history" command,
age/size-based GC limits, and the per-cwd `listTerminalLogs()` host API
(§8) so the scope can scope to the current project and show friendly
titles instead of raw paths.

## 11. Open questions

- **Result ordering across sources** — interleave by relevance, or always group
  by source? (Proposed: group, with a "flatten" option.)
- **Ignored vs hidden** — one toggle or two? (Proposed: one "Ignored & hidden";
  split later if users ask.)
- **Buffers as a scope vs always-on overlay** — should unsaved-buffer hits always
  shadow on-disk hits regardless of the Files toggle? (Proposed: Buffers is its
  own scope, default on.)
- **Closed-terminal retention default** — off (privacy) vs on (utility)? Proposed
  off, with an obvious enable + purge.
- **Presets** — ship fixed presets, or user-definable saved scopes from the start?
