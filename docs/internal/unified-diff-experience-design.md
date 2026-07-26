# One Diff Experience — a unified UX concept for viewing and reviewing changes in Fresh

Status: **DESIGN / PLAN** (nothing here is built; supersedes the scattered per-feature
plans listed at the end).
Date: 2026-07-26.

Purpose: today Fresh has seven diff-flavored surfaces built in three generations, each
with its own chrome, keys, and renderer. This doc diagnoses the overlap, distills what
the best tools in the field do (terminal, IDE, web, and the new agent-era reviewers),
and lays out a single coherent architecture — one renderer, one review container, one
in-buffer lens — with task-focused entry points on top, and a phased migration.

The doc is deliberately split in two:

- **Part I — the review experience itself** (§3, phases 0–3): the UI/UX of
  navigating a review — files, hunks, baselines, lenses, branches, layouts,
  comments-as-notes, and the persistence that makes marathon reviews resumable.
  This is the priority, and it ships identically in the TUI and the web UI —
  every surface is a scene projection consumed by both renderers under the
  existing parity discipline, so the web needs **no special treatment** here.
- **Part II — talking to the outside world** (§4, phase 4): agent feedback
  loops, comment dispatch, review IPC, forge/PR sync. Orthogonal by
  construction and explicitly lower priority — nothing in Part I depends on it.

Method: the current features were driven interactively in `tmux` (release build,
scratch repo with staged/unstaged/untracked changes, a feature branch, and multi-commit
history); the field research covers ~40 tools (magit, lazygit, gitui, tig, delta,
difftastic, diffview.nvim, hunk, jj/scm-record, Sublime Merge, fugitive, octo.nvim,
gitlab.nvim, prr, gh-dash, tuicr, revdiff, difit, diffx, VS Code, JetBrains, GitLens,
Zed, Cursor, Xcode, Tower, GitHub, GitLab, Gerrit, Graphite, Reviewable, Phabricator,
CodeFlow lineage, Conductor, Crystal, claude-squad, cmux, vibe-kanban, Sculptor,
Codex, Devin, Claude Code, Copilot Workspace, Pierre/@pierre/diffs (diffs.com),
diffhub, Superset, Linear Diffs, ftdv, drft, Codiff, GitLab Rapid Diffs), plus a
deep-research sentiment pass over the same landscape (what agent-era users
praise, complain about, and churn over).

---

## 1. Where we are: seven surfaces, three generations

What exists today, as observed live:

| # | Surface | Owner | Renderer | Chrome / help | Keys |
|---|---------|-------|----------|---------------|------|
| 1 | **Review Diff** (worktree) | `audit_mode.ts` | Structured unified stream: section/file/hunk headers, dual line-number gutter, folds, inline comment boxes. **No syntax highlighting.** | 2-line top hint bar + `?` overlay | `n/p` hunks, `,/.` files, `Tab` focus, `1/2/0` layout, `s/u/d`, `c`, `v`, `W`, `/` |
| 2 | **Review: Range / Stash** | `audit_mode.ts` | Same as #1 (read-only verbs) | Same as #1 | Same as #1 |
| 3 | **Review: PR Branch** | `audit_mode.ts` (separate `branchState`) | Commit list + **raw `git show` text** (literal `diff --git` / `index` lines) | Footer hint line inside the log panel | `j/k`, `Enter` (focus detail), `r`, `q` |
| 4 | **Git Log** (+ Current File) | `git_log.ts` | Commit list (widgets) + **raw streamed `git show`** | Button toolbar on top | `Tab` (pane switch), `RET` (open file), `y`, `r`, `q` |
| 5 | **Side-by-Side Diff** | host composite buffer | Two-column OLD\|NEW, **full syntax highlighting** | Inline header row of hints | Hardcoded in composite input router (not rebindable) |
| 6 | **Live Diff** | `live_diff.ts` | In-buffer gutter marks + inline old-line rendering, word-level intraline | none (palette-only) | n/a (8 palette commands) |
| 7 | **Git Blame** | `git_blame.ts` | Magit-style blame panel | its own | its own (`Go Back` = re-blame at parent) |

Plus `git_gutter.ts` (a second in-buffer change-marking system), `diff_nav.ts` (a
*third* hunk-navigation system that merges git-gutter + live-diff + saved-diff jump
targets), and `merge_conflict.ts`.

### The specific weirdness (all observed, not hypothetical)

1. **Three chrome conventions.** Review Diff has a top hint bar and a `?` overlay;
   PR-Branch puts a hint *footer inside a panel*; Git Log uses a button toolbar;
   Side-by-Side crams hints into its header row. Same product, four dialects.
2. **The same view built twice.** Git Log and Review: PR Branch are both
   "commit list + `git show` detail", implemented separately with different widths,
   keys (`Enter` means different things), rendering, and toolbars.
3. **Structured vs raw rendering.** The worktree review renders a real UI (headers,
   gutters, folds, comments); Git Log and PR-Branch dump raw `git diff` text with
   add/remove coloring only.
4. **Syntax highlighting is a lottery.** Present in the composite side-by-side pane,
   absent in the unified review stream (confirmed: plain-white body text) — the
   single most visible reader-quality gap, already flagged in
   `search-and-diff.md` §3.3.
5. **Three hunk-navigation systems** that don't know about each other: review-mode
   `n/p`, the composite buffer's hardcoded keys, and the `diff_nav` plugin's merged
   jump list.
6. **Two in-buffer change-marking systems** (git-gutter and live-diff) with separate
   options and rendering.
7. **Entry points are a flat pile of palette commands** — Review Diff / Review Range /
   Review Stash / Review PR Branch / Git Log / Git Log (Current File) / Side-by-Side
   Diff / eight Live Diff commands / Next-Prev Diff Chunk — with no shared naming
   scheme and no guidance about which to use when.
8. **The task the product is increasingly used for — reviewing agent work — has no
   entry point at all.** The Orchestrator shows per-worktree diffstat pills and PR
   badges, but there is no "review this session's changes" action; you must switch
   into the session and manually run Review Diff, which then diffs against HEAD
   rather than against what the agent started from.
9. Small frictions that betray the seams: the hunk counter says "Hunk 1 of 1" while
   the session holds three files' hunks; `Tab` cycles focus in review but switches
   panes in git log; `q` vs `Esc` semantics vary.

None of this is a quality complaint about any single surface — Review Diff post-0.4.0
is genuinely competitive with the best external tools (in-session split/stack/auto,
watch mode, comments with persistence and export, real staging). The problem is that
the surfaces don't compose into one product.

---

## 2. What the field converged on (research distillation)

Full research notes live with the session; this is the actionable distillation.

### 2.1 The architecture the best products share

Across VS Code, JetBrains, Zed, and the strongest TUIs, successful designs reduce to
**three primitives plus thin navigation chrome**:

1. **One diff renderer.** Split ⇄ unified as a *single preference applied everywhere*
   (Zed: one `diff_view_style` setting covers Project Diff, File History, and Stash
   view), syntax highlighting on both sides, word-level intraline highlights layered
   on top, whitespace toggle, collapse-unchanged with expandable context, theme-key
   colors. JetBrains proves the value of one maximal component reused for files,
   revisions, shelf, and PRs; VS Code's scar tissue (two "next change" systems, the
   merge-editor backlash) shows what happens when surfaces multiply.
2. **One changeset container.** A multi-file, navigable review surface parameterized
   by *(base, target, file set, verb set)* — working tree, staged, a commit, a range,
   a branch, a stash, a PR, an agent's pending edits **all open in the same
   container**. Zed's Project Diff ("all changes are editable excerpts"; review and
   fix in the same surface) and VS Code's multi-diff editor are the models.
3. **In-buffer hunk affordances.** Gutter marks → expand the hunk *in place* → act on
   it (stage/revert or keep/reject) → auto-advance. Zed's expand-in-place (Esc
   collapses; the diff view and the file are literally the same buffer) is the
   cleanest; matklad's "Unified vs Split" essay lands in the same place: review
   should be *a lens over real, editable, navigable files*, not a frozen artifact.

Everything else is **navigation chrome that produces one of the above and never
renders diffs its own way**: a changes panel, a log/graph, a compare picker, history
views. GitLens is the existence proof that many *launchers* funneling into one
*terminal surface* stays coherent — and its three competing commit graphs are the
proof of what doesn't.

### 2.2 Interaction patterns with overwhelming convergence

- **Selection drives preview.** In lazygit, tig, magit, and every good log view, the
  detail pane is a *reactive projection of the list selection* — zero extra
  keypresses. tig's parent-drives-child split (scrub commits above, diff follows
  below) predates and outlives everything; magit adds the refinement of a
  *debounce delay* so holding `n`/`j` doesn't thrash the preview.
- **The magit section model.** Every magit buffer is a tree of *typed, foldable
  sections* (file/hunk/commit…) whose values make cursor position semantically
  meaningful: per-type keymaps (which map naturally onto per-type mouse context
  menus), `TAB` disclosure, depth presets (1–4 = "show me just file names" …
  "expand all hunks"), child counts on collapsed headers — and, critically,
  **fold state persists across refreshes**, so the buffer feels like a stable
  workspace rather than a re-rendered report. Fresh's review buffer is already
  section-shaped; the persistence and depth-preset details are the missing polish.
- **`Enter` = drill in, `Esc`/`q` = back out**, uniformly, across an object hierarchy
  (branch → commits → files → hunks → lines).
- **One selection vocabulary reused everywhere.** lazygit's `space` toggle / `v`
  range / `a` granularity works identically for staging, discarding, and its custom
  patch editor; magit's verb grammar (`s`/`u`/`k`/`v`/`a`) resolves its target from
  point — file, hunk, selected lines *within* a hunk, or a multi-section selection
  (with the safety rule that a multi-selection must include the cursor's section).
  Context-sensitive verbs — which Fresh's review already does with `s/u/d` — are
  the norm (fugitive, Sublime Merge). jj generalizes the other half: *"select a
  subset of this diff"* is one reusable checkbox-tree component (scm-record) that
  serves stage, split, squash-into, and discard alike.
- **The granularity ladder** file → hunk → line with explicit toggles; tri-state
  checkbox trees (scm-record/hunk.nvim) are the most legible mouse-friendly form.
- **Next/prev-file cycling from anywhere** without refocusing a sidebar
  (diffview.nvim's Tab/S-Tab is universally loved), and JetBrains' "F7 continues
  into the next file" chaining.
- **Discoverability is the recurring TUI failure mode.** jj's built-in diff editor
  became famous for an undiscoverable confirm key (users spent 10 minutes guessing);
  magit's answer (transient menus), lazygit's (`?` menu that is itself runnable),
  gitui's (persistent bottom hint bar + help popup), and hunk's (`?` overlay + real
  menus) all work. Fresh's hint-bar + `?` overlay in Review Diff is the right
  pattern — it just isn't applied to the other six surfaces.
- **Transparency builds trust**: Sublime Merge and jj-fzf echo the exact git command
  every UI action runs.
- **Read-only as a trust posture** (tig): a viewer that cannot mutate is one users
  open without fear; mutation lives behind clearly-marked verbs.

### 2.3 What review (as opposed to viewing) requires

From Gerrit, Reviewable, GitHub's 2025-26 Files-Changed revamp, and CodeFlow lineage:

- **A stable identity for "the change" across rewrites** (Gerrit Change-Id, Reviewable
  revisions, CodeFlow iterations) — the precondition for progress tracking and
  interdiff.
- **Base/head selection as a first-class control** (Gerrit's patchset A/B picker;
  the default should be *computed*: "from what you last reviewed to latest").
- **Per-file viewed marks with automatic invalidation** when content changes
  (GitHub/GitLab: the checkbox clears only for files changed since you viewed them),
  aggregate progress ("12/34 reviewed"), and **mark-viewed-auto-advances**.
- **Durable comment anchoring**: port comments forward across versions, keep a link
  to the original context, and *signal degraded confidence* (Phabricator's ghostly
  ported comments; Reviewable's red dog-ear).
- **Interdiff with noise suppression** — the decade-consistent #1 complaint about
  GitHub is force-push amnesia; only Gerrit (rebase-edit classification) and
  Reviewable (base-change collapse) solve it properly.
- **Noise suppression by default**: auto-collapse generated/lock files
  (GitLab's `gitlab-generated` gitattribute, difit's default collapse).
- **"Whose turn is it"** and completion conditions (all files viewed at latest
  version + no unresolved comments) gate the exit action.

### 2.4 What the agent era adds

- **Comments are prompts.** Every 2025-26 tool (difit's Copy-Prompt, revdiff's
  structured stdout + exit code 10, tuicr's clipboard export, cmux-hub's
  send-to-terminal, VS Code's Add-Feedback→agent, Zed's proposal for the same)
  treats review annotations as structured input for the next agent turn.
- **Agent review = same container, different baseline and verbs.** VS Code and Zed
  both converged on: the agent's pending edits are the *same* changeset surface with
  base = pre-turn checkpoint and verbs = **Keep / Reject** instead of Stage / Revert,
  plus a resolve→auto-advance loop and a layering rule (staging a file = accepting
  its pending edits). Zed goes further: agent review *temporarily replaces the
  buffer's git baseline* so all the ordinary hunk machinery just works.
- **Diff as live status.** Conductor's always-visible cumulative branch diff,
  cmux-hub's WebSocket refresh, hunk/diffnav watch modes: watching the diff evolve
  replaces reading agent chatter. Fresh's Live Diff and Review-Diff watch mode are
  already exactly this.
- **Checkpoints as the undo spine** (Cursor, Zed, Crystal's auto-commit per
  iteration): coarse rollback complements per-hunk keep/reject.
- **Review-state as inbox**: persisted reviewed-marks + unreviewed-only filters turn
  a big agent diff into something you burn down (tuicr, revdiff).
- **Comments carry severity and a destination.** diffhub tags notes
  (`[must-fix]`/`[suggestion]`/`[nit]`/`[question]`) and exports the set as one
  agent prompt; Superset goes further — select a line range, "Ask the AI about
  these lines", and *pick which running agent session* receives it. A review
  comment in 2026 is (file, range, severity, text) + a dispatch target.
- **Merge-base against the true upstream is the consensus baseline.** Superset's
  internal diff-semantics doc records the bug that forces this: diffing against the
  base branch's *tip* makes your changed-file counts creep as teammates merge;
  every serious tool (diffhub, Superset, GitHub, diffview.nvim) converged on
  merge-base ("me since I forked") against the branch's *configured upstream*, not
  a hardcoded `origin/main` — with staged/unstaged as separate overlaid buckets.
- **Narrative ordering is the newest frontier.** Devin's Review and Linear's Guided
  Reviews both restructure big diffs into chapters ordered by reasoning — core
  change first, consequences, then glue, each with a purpose blurb — because
  alphabetical file order ≠ the structure of the work. Linear also ships
  structural noise-stripped highlighting on by default.
- **The open gap nobody owns**: a good *local* interdiff over agent iterations —
  "what changed since I last reviewed this session, minus my own edits and rebase
  noise." Web tools have the model but not the locality; local tools have the
  locality but flat state. An editor that holds the worktrees, the checkpoints, and
  the review state can own this.
- **TUI fatigue is real, and the field's answer is a local-web escape hatch.**
  A whole category (difit, Codiff, diffx, diffhub, cmux-hub) exists because —
  in one author's words — "TUI review became overwhelming": parsing thousands of
  machine-generated lines in a constrained palette depletes faster than the same
  review in a browser. These tools all spawn a throwaway local web server showing
  a GitHub-style view of the pre-push diff. Newer file-tree TUIs (ftdv, drft)
  attack the same fatigue from the other side: persistent checkbox state written
  to disk mid-review, because a 50-file agent review is "a marathon, not a
  sprint" — progress, drafts, and read position must survive closing the app.
- **Two feedback transports, each with a documented failure mode.** Structured
  export (tuicr/Codiff/diffhub markdown with `file:line` anchors and severity
  classes) is portable and agent-agnostic; direct daemon/MCP integration
  (hunk's `session` CLI, difit's comment API) feels like pair programming when it
  works — but breaks opaquely when sandboxes or container network isolation block
  the agent from the local port. A robust design needs the rich path *and* a
  file-based fallback that any sandboxed agent can reach.
- **Verb model preference is stakes-dependent, not absolute.** Users favor
  keep/reject overlays for minor or boilerplate edits and deliberately fall back
  to comment-and-iterate for critical logic; and keep/reject has a documented
  failure mode — **context blindness**: keeping a signature change in one hunk
  while rejecting its call-site update in another breaks the build. The
  review-then-commit model's failure mode is the mirror image: validating only
  the combined result lets debug statements and scaffolding slip into commits.
- **One cautionary tale**: Cursor's review pane held a stale second writable copy of
  a file and silently reverted user edits. Review views must be read-through to the
  live buffer or strictly read-only — never a second writable copy.

---

## 3. Part I — the review experience: one experience, four doors

### 3.1 Mental model

> **Fresh has one way to look at a change, no matter where the change came from.**

Everything is built from three shared pieces:

- **The Diff Renderer** — how any two versions of a file are drawn.
- **The Review Session** — the container that holds a set of file diffs plus review
  state (comments, viewed marks, verbs). One implementation; every source of
  changes opens here.
- **The Buffer Lens** — in-buffer change marks and expandable hunks in the ordinary
  editor.

And the user reaches them through **four task-shaped doors** (entry points with a
focused verb set), all opening the same container:

| Door | Task in the user's head | Source | Verb set |
|------|------------------------|--------|----------|
| **Review Changes** | "What's in my working tree; stage and commit it" | worktree + index | stage / unstage / discard / commit |
| **Review Branch / PR** | "Read this branch or PR like a reviewer" | merge-base three-dot vs base; commit-scoped lens | comment / viewed / export / approve-note |
| **History** | "What happened; find and inspect commits" | log; any commit or range | read-only + pivot-to-review |
| **Review Agent Work** | "What did the agent do; keep or reject it" | session checkpoint → worktree | keep / reject (agent dialogue: Part II) |

The doors differ in *defaults and verbs*, never in rendering, navigation, or chrome.

### 3.2 The Diff Renderer (shared by everything)

One renderer with two layouts and one rule: **the layout toggle is global to the
session and works everywhere the renderer appears.**

- **Stack (unified)** — the virtual-buffer stream Review Diff uses today, upgraded
  with: per-token syntax highlighting (the #1 gap; see Phase 0), word-level
  intraline emphasis layered *on top* of syntax colors (delta's model, with the
  ~hundreds-of-tokens-per-line fallback guardrail), `@@` headers replaced by
  "line number + syntax-highlighted enclosing context" (delta's signature
  readability win), copy-safe bodies (no `+`/`-` prefixes; change identity lives in
  gutter + background), dual old⋮new line-number gutter.
- **Split (side-by-side)** — the composite buffer, kept, with its keys migrated to
  rebindable Actions in the `diff-view` context (killing the v1 hardcoded router,
  as `search-and-diff.md` already prescribes).
- Shared toggles, one keybinding each, identical in both layouts and in every door:
  whitespace-ignore, collapse-unchanged (with click/key expandable context), inline
  notes on/off, watch mode.
- Colors are theme keys only. Renderer must stream and render per-hunk incrementally
  (the lazygit-delta regression and gitui's benchmark both show render latency on
  selection-change is the make-or-break TUI metric).

The renderer is also the long-term home for niceties that should *not* block v1:
moved-code detection, "formatting-only change" dimming (a syntax-aware check with an
explicit budget and a visible fallback indicator — difftastic's lesson: never switch
modes silently).

### 3.3 The Review Session (one container, parameterized)

Generalize the existing `audit_mode` session into *the* container:

```
ReviewSession {
  source:  Worktree | Staged | Range{from,to} | Commit(sha) | Stash(ref)
         | Branch{base}            // merge-base three-dot semantics
         | AgentTurn{session, from_checkpoint}
         | Patch{file|stdin}
  lens:    Flattened | PerCommit(sha)   // see below
  verbs:   {stage,unstage,discard} | {keep,reject} | {} (read-only)
  state:   comments, viewed marks, watermark (per source identity)
}
```

Layout (today's three panels, kept):

```
┌ hint bar (uniform, door-aware) ──────────────────────────────────────────┐
│ FILES               │ DIFF (stack or split)              │ COMMENTS      │
│ dir-grouped,        │ one scrollable stream of all       │ threads,      │
│ ✓ viewed, *N notes, │ files (stack) or per-file SxS      │ jump/edit/    │
│ +N -N, status       │ (split); same renderer either way  │ export        │
└──────────────────────────────────────────────────────────────────────────┘
```

Key decisions:

1. **The commit dimension becomes a lens, not a separate feature.** Today
   "Review: Commit Range" (flattened) and "Review: PR Branch" (commit-by-commit) are
   two products. In the unified session, a branch/range source has a **lens toggle**:
   *Flattened* (the whole change as one diff — the default, per GitHub/Gerrit
   practice of reviewing the cumulative change) ⇄ *Per-commit* (a commit strip —
   list panel swaps to commits; selection drives the diff, tig-style; `[`/`]` or
   click to move between commits; "all commits" returns to flattened). This is
   exactly GitHub's Dec-2025 "review commit-by-commit without leaving Files
   Changed", and it deletes the PR-Branch mode's separate implementation.
2. **Comments work in every source, every lens, both layouts.** (Today they exist
   only in the worktree/range session and vanish in split-file drill-down history
   panes.) Comments persist per source identity as today (`.review/`), with
   re-anchoring by content match and a staleness marker when the anchor is fuzzy
   (Reviewable's dog-ear). Comments gain an optional **severity tag**
   (`must-fix` / `suggestion` / `nit` / `question` — diffhub's vocabulary), which
   colors the inline box and the panel entry, orders the export, and lets the
   agent door filter what gets dispatched. Longer term, comments are one instance
   of a general *line-anchored annotation* model (the `@pierre/diffs` framework
   pattern): review comments, agent notes, CI/lint findings, and LSP diagnostics
   all render through the same widget channel instead of growing parallel systems.
3. **Viewed marks + progress.** Per-file ✓ in the FILES panel, keyed by
   *(file, blob-hash-pair)* so a mark auto-invalidates exactly when content changes
   (no timestamps); "N/M reviewed" in the hint bar; `Space` = mark-viewed-and-advance
   (GitHub's auto-advance); `F`-style filter to unreviewed-only. This is what makes
   large (agent-sized) diffs tractable, and it's cheap: it reuses the existing
   `.review/` persistence. For range/branch sources, state should additionally key
   on a *stable change identity* (patch-id today; jj/Gerrit-style change-ids when
   available) so a rebase or an agent re-run doesn't reset review progress.
   Fold state persists across refresh too (magit's underrated detail — a refresh
   must never destroy the user's fold layout), and depth presets
   (`1`..`4` ≈ sections / files / hunks / everything) complement `z a`/`z r`.
   The whole bundle — viewed marks, fold layout, cursor/read position, and
   *unsent comment drafts* — persists to disk with the session, because a 50-file
   agent review is a marathon: closing the editor mid-review and resuming with
   nothing lost is the trust-defining behavior (ftdv/tuicr's persisted-progress
   lesson, and what `.review/` already does for comments).
4. **Verbs are context-sensitive and door-scoped.** `s/u/d` in the worktree door
   (file or hunk under cursor, `v` for lines — unchanged from today); `k`/`x`
   keep/reject in the agent door with the same granularity ladder; read-only doors
   simply don't bind mutation verbs — and say so in the hint bar. Destructive verbs
   (discard, reject) get an undo story rather than ever more confirmation prompts:
   at minimum a per-session "last discarded patch" buffer; longer term a small
   operation log (jj's lesson — universal undo removes fear, and fear is the main
   tax on git UIs).
5. **Jump-out is uniform.** `Enter` = drill (side-aware open, as today);
   `Alt+o` = open editable working-tree file at this line, from *any* source
   (for historical sources, opens read-only file-at-commit exactly like Git Log's
   current drill-down).
6. **Read-through, never a copy.** Where the target is the working tree, the NEW
   side must reflect live buffers (watch mode already reloads on save); the session
   never holds a second writable copy of a file (Cursor's failure mode).

### 3.4 The four doors, concretely

**Door 1 — Review Changes** (today's Review Diff, essentially unchanged in spirit):
`Review Changes` opens `source: Worktree`, STAGED/UNSTAGED/UNTRACKED sections,
staging verbs, watch mode. Everything the 0.4.0 rework shipped stays; it just
inherits the renderer upgrades and viewed marks. Stash and Patch are variants
reachable from the picker (below).

**Door 2 — Review Branch / PR**: one command replacing Review Range + Review PR
Branch. Opens `source: Branch{base}` with base defaulting to the branch's
**configured upstream tracking branch** (then origin/HEAD → main → master, as
today), and **merge-base three-dot semantics by default** — the consensus every
serious tool converged on after the same bug (diffing the base *tip* makes your
file counts creep as teammates merge; today's two-dot `diff base..HEAD` has exactly
this problem). Like Superset's diff-semantics doctrine, each source's exact git
comparison gets documented and integration-tested. Flattened lens by default;
per-commit lens one key away.
Typed revspecs (`A..B`, `A...B`, sha, stash) remain the power-user path in the same
prompt. (PR metadata import — threads, viewed-state sync — is Part II, §4.4.)

**Door 3 — History** (today's Git Log, kept as the *browse* surface): the commit
list stays a list — but its detail pane becomes **the shared renderer** (structured
headers, folds, syntax colors, per-commit lens of a one-commit session) instead of
raw `git show` text. Selection continues to drive the preview (streamed, cancellable,
SHA-cached per the existing streaming plan). From any commit: `Enter` = promote to a
full Review Session on that commit (comments and all); a range selection (lazygit
`v`-style) promotes to a session on the range; `y` copy hash; blame/file-history
pivots. Git Log (Current File) stays as a scoped variant. This deletes zero
functionality — it upgrades the detail pane and gives History a *pivot into* review
rather than being a parallel review implementation.

**Door 4 — Review Agent Work** (new; the missing entry point): from an
Orchestrator session card (key + click + palette), "Review Session's Changes" opens
the container with `source: AgentTurn{session, from_checkpoint}` in *that session's*
worktree context: base = the ref the agent started from (recorded when the
Orchestrator creates/attaches the worktree; fallback: merge-base with the default
branch), head = live worktree, watch mode ON by default (Conductor's live-diff-as-
status), verbs = **keep / reject** (per hunk/file/line; reject = revert those lines
in the worktree — implementable with the same `git apply`-style machinery staging
uses today). Two guardrails against keep/reject's documented failure modes:
**file-level is the default granularity** (hunk/line reject is a deliberate act),
and a partial keep/reject on a file whose *other* hunks touch the same symbols
gets a "related changes remain" nudge — cheap protection against the
keep-the-signature-reject-the-call-site build break. Agent diffs also get a
lightweight **leftover-noise scan** (debug prints, `dbg!`/`console.log`, stray
TODO/FIXME markers highlighted in the stream) — the review-then-commit model's
classic leak. The dock card's diffstat pill becomes a *door*: click → review.
Above the single session, the `Review…` picker's "Agent sessions" group is the
**review inbox**: every session with unreviewed changes, sorted by
awaiting-review state (Graphite's queue, scoped to the local farm). Reviewing the
*combined* output of parallel sessions (jj-style megamerge) stays out of scope
until worktree merging is itself a Fresh feature; the inbox plus per-session
review is the honest v1.
Note the scope line: this door, as Part I, is *pure UI* — a baseline choice
(the session's start ref), a verb set (keep/reject), and the same container.
Everything that makes it *converse* with the agent — sending comments to the
session, agent-seeded notes, pause-on-review, guided-review outlines — lives in
Part II and layers on later without changing the door. One later Part-I
capability worth naming now because it's baseline UX, not communication:
per-turn checkpoints recorded as refs enable a **"since I last reviewed"
interdiff lens** in the baseline picker — the gap no tool owns today (§2.4).

### 3.5 One picker in front of the doors

A single **`Review…`** palette entry (the "review picker" already sketched in
`search-and-diff.md`, upgraded to door-awareness):

```
Review…
  ★ This branch vs main (12 commits, +410 −102)     ← smart default (Door 2)
  ● Working tree (3 files: 1 staged, 1 unstaged, 1 untracked)   (Door 1)
  ▸ Agent sessions…  (2 with changes)                (Door 4)
  ▸ Recent: HEAD~3..HEAD · stash@{0} · v0.4.0..HEAD  (re-open with state)
  type a revspec: A..B · A...B · <sha> · stash@{N} · <branch>
```

Debounced live preview on selection (lazygit's selection-drives-preview), and
re-opening a recent shows "since you last reviewed: N files changed" (the watermark
lens). The individual door commands remain in the palette for muscle memory
(`Review Changes`, `Review Branch/PR…`, `Git Log`, `Review Agent Work`), all listed
under one `Review:`/`Git:` naming scheme.

### 3.6 The Buffer Lens (ambient tier)

Merge the three in-buffer systems into one concept with one options surface:

- **One hunk model** feeding gutter marks: reference = HEAD | disk | branch |
  agent-baseline (Live Diff's reference picker becomes *the* picker; git-gutter
  becomes the rendering of the same hunks at reference=HEAD rather than a parallel
  system).
- **One navigation**: Next/Previous Change (the `diff_nav` merged jump list) is the
  *only* in-buffer hunk navigation, bound once, listed once.
- **Expand-in-place** (host primitive, phased): a gutter/hunk key expands the hunk
  inline — old lines as virtual lines (Live Diff already renders exactly this) —
  with hunk-scoped actions (stage/revert; keep/reject when the reference is an
  agent baseline) and Esc-collapse. This is Zed's model and matklad's ideal, and it
  reuses Live Diff's virtual-line machinery rather than inventing a new surface.
- When an agent session is active on the buffer's worktree, the lens's reference
  auto-swaps to the agent baseline while review is pending (Zed's baseline-override
  trick), so ordinary editing shows "what the agent changed" without opening
  anything.

### 3.7 Uniform vocabulary (the coherence contract)

One keymap, defined once in a shared `review` key context, inherited by every door
and by the composite buffer (all rebindable):

| Key | Meaning — everywhere |
|-----|----------------------|
| `n` / `p` | next / prev hunk (chains across files, JetBrains-style) |
| `,` / `.` | prev / next file |
| `[` / `]` | prev / next commit (per-commit lens) · prev/next comment elsewhere |
| `Tab` / `S-Tab` | cycle panel focus (never "switch pane" vs "focus" ambiguity) |
| `Enter` / `Esc`+`q` | drill in / back out |
| `1` / `2` / `0` | split / stack / auto — session-wide |
| `Space` | mark viewed & advance (read doors) · toggle selection (staging ladder) |
| `s`/`u`/`d`, `S`/`U`/`D` | stage/unstage/discard — worktree door only |
| `k` / `x` | keep / reject — agent door only |
| `c` / `N` / `e` | comment / session note / export — every door |
| `v` | line-range selection (feeds whatever verb follows) |
| `W`, `/`, `z a`, `z r`, `r` | watch, filter files, fold/unfold all, refresh |
| `?` | full help overlay — every door, same layout |

Chrome contract: every door gets the same two-line hint bar (door name + source +
progress on line 1, context verbs on line 2), the same `?` overlay style, and the
same status-bar segment (`Review: Branch main...HEAD · 4/12 viewed · hunk 3/41` —
session-wide counters, fixing the "Hunk 1 of 1" confusion). Menus and mouse: every
key action is also a menu item (Fresh's identity is menus + palette + mouse; this is
our answer to the TUI discoverability failure mode, and it's cheaper than it sounds
because the verbs are defined once).

### 3.8 Web: parity, not a feature

The field builds separate local web apps to escape TUI fatigue (difit, Codiff,
diffhub — the "TUI review became overwhelming" complaint in §2.4). Fresh's answer
is structural, not a feature: every Part-I surface is a scene projection consumed
by both the TUI and the web renderer under the existing parity discipline
(`web-ui.md` — divergence is a test failure). The only work is making the review
panels' projections complete; after that the entire experience — container,
doors, picker, comments, viewed marks, fold state — is simply *also* in the
browser, with no web-specific design, no second implementation, and no state
divergence. This section exists only to state that constraint: **no review
feature may land as TUI-only logic.**

### 3.9 Blame and merge (adjacent, aligned, not absorbed)

- **Blame** stays its own surface (it is an annotation of one file, not a changeset)
  but adopts the vocabulary: `Enter` opens the commit *in the shared renderer*
  (one-commit Review Session), `,` re-blames at parent (already exists as "Go Back"
  — rebind to match tig's beloved verb), and a "hunk history" pivot from any review
  hunk into scoped Git Log (Current File) — the Sublime Merge pivot.
- **Merge conflicts** stay with `merge_conflict.ts` inline markers (never force-
  replace the inline flow — VS Code's lesson); the renderer gains zdiff3-style
  "each side as a diff against base" display as a later, additive option.

---

## 4. Part II — talking to agents and outside systems (lower priority)

Everything here is **orthogonal to Part I by construction**: it consumes the
container, the comment store, and the door model but changes none of their UX.
It is sequenced last deliberately — the UI must be nailed first, and each item
below layers onto a finished Part I without reopening it.

Fresh's unique position (from the hunk comparison docs): it can be both the *reader*
(hunk-quality rendering) and the *actor* (staging, editing) — and, with the
Orchestrator, the *host* of the agents whose work is being reviewed.

1. **Comments-as-prompts**: "Send open comments to session" serializes the
   comment set (file:line + severity + text, the existing JSON/MD export shapes)
   into the session's agent terminal as a prompt (the diffhub "Copy as prompt" /
   cmux-hub pattern). When multiple sessions touch the same worktree, a session
   picker chooses the recipient (Superset's "Ask the AI about these lines").
   Exit-state convention à la revdiff (structured block, stable `file:line`
   anchors) so any agent can parse it.
2. **Review IPC** (Phase 3): reuse the existing local-control IPC + native
   review-hunk state (host already exposes review-hunk state and
   `set-review-diff-hunks`) to let an agent open/inspect/annotate/navigate a review
   session — hunk-daemon parity (`session review --json`, `comment add`,
   `navigate`), without a new daemon. An agent pre-reviews; the human triages the
   agent's inline notes in the same session (difit's `--comment` seeding pattern).
   **Transport resilience**: the IPC socket is the rich path, but sandboxed agents
   (containers, network-isolated harnesses) demonstrably fail to reach local
   daemons — so the same exchange must also work through plain files the agent
   can always touch: comments out via a `.review/outbox.md` snapshot (regenerated
   on change), agent notes in via a watched `.review/inbox/` drop. Same schema,
   two transports, no opaque connection errors.
3. **Agent-seeded reviews**: an agent pre-reviews and its notes arrive as inline
   annotations for the human to triage (difit's `--comment` seeding); "pause
   agent while reviewing" (claude-squad's `c`); an optional **guided-review
   lens** — the agent that made the change emits a chapter outline (core change
   → consequences → glue, with purpose blurbs) that reorders the FILES panel
   narratively (Devin/Linear's answer to "alphabetical order ≠ the structure of
   the work").
4. **Forge/PR sync**: when a reviewed branch has an open PR and a forge plugin is
   present, import PR metadata — title, review threads as comments, viewed-state
   sync — and optionally publish local comments as a pending PR review.

(The iteration-interdiff lens and its checkpoint refs
(`refs/fresh/review/<source-id>/v<N>`, Reviewable's GC-proof pinning trick) are
*Part I* — they're baseline-selection UX — and are scheduled in Phase 3; they're
mentioned here only because agent turns are their richest source of versions.)

---

## 5. Phased plan

Sequenced so every phase ships a visible coherence win and nothing regresses.

**Phase 0 — stop the visible bleeding (small, high-leverage)**
- Syntax highlighting + intraline emphasis in the unified review stream (needs the
  overlay-priority/background-pathway fix already analyzed in `search-and-diff.md`;
  this is the #1 reader gap and it's pure renderer work).
- Composite-buffer keys → rebindable Actions in the `diff-view` context.
- Adopt the chrome contract on existing surfaces as-is: same hint-bar format and
  `?` overlay for Git Log, PR Branch, and Side-by-Side; fix `Tab`/`q` semantics;
  session-wide hunk/file counters in the status bar.
- Merge in-buffer hunk *navigation*: Next/Previous Change becomes the single
  advertised binding (diff_nav already merges the sources).

**Phase 1 — one renderer, one container**
- Extract the Review Session container from `audit_mode` with the `source`
  parameterization (worktree/range/stash already share most of it; this is mostly
  honest refactoring plus the verbs/lens seams).
- Git Log detail pane switches to the shared renderer (structured, streamed,
  SHA-cached, kill-on-scroll per the existing streaming plan); `Enter` promotes to
  a one-commit Review Session.
- Retire **Review: PR Branch** as a separate implementation: `Review Branch/PR…`
  opens the container on `Branch{base}` (three-dot), flattened lens; **per-commit
  lens** lands here (commit strip + selection-drives-diff), closing the gap the
  old mode covered. Range/stash/patch become picker variants.
- The `Review…` picker (smart default, recents, live preview).

**Phase 2 — review state + the agent door**
- Viewed marks (blob-hash-keyed), progress counters, Space=viewed-and-advance,
  unreviewed filter; recents re-open with full state (marks, folds, read
  position, drafts) + "since last review" watermark.
- Door 4: Orchestrator "Review Session's Changes" (base = recorded session start
  ref), keep/reject verbs (file-default granularity + related-changes nudge),
  diffstat-pill-as-door, sessions review inbox in the picker, leftover-noise
  scan. (No agent communication yet — that's Phase 4.)
- Buffer Lens unification: one reference picker (absorb git-gutter into the
  live-diff hunk model), agent-baseline auto-swap.
- Web parity for the review panels (§3.8): complete their scene projections so
  the whole experience is also in the web UI — enforced by the parity tests,
  no web-specific design.

**Phase 3 — review depth (still Part I)**
- Expand-hunk-in-place host primitive (Zed model) with hunk-scoped verbs.
- Iteration refs + "since I last reviewed" interdiff lens with base-noise
  suppression.
- Renderer niceties: moved-code detection, formatting-only dimming, generated-file
  auto-collapse (gitattributes-driven), zdiff3 conflict rendering.

**Phase 4 — Part II: agents and outside systems**
- Comments-as-prompts ("Send to session", session picker, export conventions).
- Review IPC for agents + file-based transport fallback; agent-seeded reviews;
  pause-on-review; guided-review lens.
- Forge/PR sync (threads ⇄ comments, viewed-state, pending-review publish).

Explicit non-goals (for now): a commit-graph visualization (History stays a linear
list; graph is additive later), interactive rebase UI, structural (AST) diffing as
default (budgeted opt-in only), replacing merge_conflict's inline flow.

---

## 6. What gets deleted or renamed

| Today | Becomes |
|-------|---------|
| Review Diff | **Review Changes** (Door 1) |
| Review Range / Review Stash | picker variants of the same session |
| Review: PR Branch | **Review Branch/PR…** (Door 2, per-commit lens) |
| Git Log / Git Log (Current File) | **History** (Door 3) — same list, shared renderer detail, pivot-to-review |
| Side-by-Side Diff (per-file command) | the session's `1` layout; standalone command stays as a thin alias that opens a one-file session |
| Live Diff + Git Gutter + Diff Chunk Nav | **Buffer Lens** (one hunk model, one reference picker, one nav) |
| (nothing) | **Review Agent Work** (Door 4) + `Review…` picker |

Docs pages, palette naming (`Review:` prefix family), and the keybinding editor's
`review` context all follow the same rename.

---

## 7. Risks and mitigations

- **Refactor risk in `audit_mode` (6.6k lines) and the outer-vs-inner split-leaf
  class of bugs** documented in `search-and-diff.md`: extract the container behind
  the existing e2e suites (review_diff_* tests) and add door-parameterized e2e
  fixtures before moving PR-Branch/Git Log onto it.
- **Perf**: the renderer must keep the streaming/SHA-cache/viewport-render
  discipline the current surfaces already learned (streamed `git show`, one-viewport
  virtual buffers); selection-driven preview must cancel in-flight processes
  (existing planned fix) — the field data says selection→render latency is the
  metric users feel first.
- **Muscle-memory breakage**: keep old palette names as aliases for one release;
  the keymap contract deliberately preserves today's Review Diff bindings (it is
  the newest and best surface; others converge *to* it).
- **Scope discipline**: Phases 0–1 contain no new capabilities, only convergence —
  resist bundling (the VS Code merge-editor lesson: additive, opt-in, never
  force-replace a flow users rely on).

---

## 8. Superseded / related docs

- `docs/internal/search-and-diff.md` §3 (diff/review viewer) — the v2 sketch and
  review-picker idea are folded in here; that doc remains the implementation-history
  record.
- `fresh-vs-hunk-review-gaps.md`, `hunk-diff-viewer-report.md` (repo root) — the
  hunk parity analysis that drove 0.4.0; Tier-3 items (agent surface, stdin/patch)
  are scheduled above.
- `docs/internal/orchestrator-sessions.md` — session/worktree model Door 4 builds on.
