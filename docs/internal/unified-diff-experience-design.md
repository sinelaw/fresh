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

## 3. Part I — the Review workspace (final design)

This section is the design of record. It keeps the established architecture
(one renderer; one `ReviewSession { source, lens, verbs, state }`; one buffer
lens), was designed from the research evidence rather than the current Fresh
chrome, and folds in the winning ideas from two blind independent redesigns
(Appendix A) — whose strong convergence with this skeleton is the main reason
to trust it.

### 3.1 Design principles (each traces to evidence)

1. **Two lines of chrome, total.** One *scope bar* on top (what am I
   comparing), one *context line* at the bottom (where am I; receipts).
   Everything else is content. (gitui's hint economy; Linear's "Fast,
   Focused"; the observed four-dialect chrome mess.)
2. **The comparison is the headline.** `base ⟶ target · lens` is a row of
   real, clickable controls in the scope bar — the review's identity, always
   visible, always editable in place. (Gerrit's A/B picker; Reviewable's
   bounds; Xcode's in-editor revision pickers.)
3. **Every control is visible and clickable; keys are accelerators printed
   on the controls.** Nothing is keyboard-only, nothing is mouse-only:
   every button shows its key, every menu prints its bindings, right-click
   opens the same verbs anywhere. The mouse path teaches the keyboard path.
   (The recurring TUI discoverability failure, inverted.)
4. **One list rail, many projections.** A single collapsible navigator shows
   *files*, *commits*, or *notes* — cycled, never three fixed panels. The
   rail is a map, not a second document.
5. **The stream is a section document.** Typed, foldable sections with
   persistent fold state, depth presets, sticky context header, delta-grade
   bodies (syntax + word-level emphasis, context headers, copy-safe text).
6. **Verbs live where they apply.** The focused section grows a row of
   small **buttons** (`[s Stage] [d Discard] [c Comment]`) that are
   simultaneously the click targets, the keymap hint, and the only mutation
   surface. Read-only scopes grow no buttons. Lowercase acts on the thing at
   point; **the same letter uppercased escalates to the whole file** — one
   rule makes the map guessable.
7. **`Space` is the review; `C` concludes it.** `Space` advances the
   burn-down (mark viewed/kept, go to next unreviewed); `C` is the single
   conclude verb whose label matches the scope — commit, submit, finish
   session. Progress is always visible and **noise never counts toward it**.
8. **`Enter` zooms in, `Esc` zooms out, everywhere.** scope → file → hunk →
   the real buffer; every diff line is a portal; double-click = `Enter`.
9. **Every mutation is undoable and transparent.** `Z` undoes
   stage/discard/keep/reject; the context line echoes the underlying git
   command as a receipt; `` ` `` opens the full command log. The undo
   journal is backed by git objects (`refs/fresh/review/undo/*`), so
   discard/reject are recoverable **across restarts**. Undo replaces
   confirmation prompts. (jj's op-log lesson; Sublime Merge transparency.)
10. **The review survives anything, and never moves under you.** Viewed
    marks, folds, cursor, unsent drafts, recents — all persist and restore.
    Reopening a reviewed scope defaults to *"since my last review"* (full
    range one click away). Live sources queue incoming edits on the file
    under your cursor instead of rewriting it (§3.9).

### 3.2 Anatomy: one workspace

The whole of Part I is **one full-bleed workspace** (a tab like any buffer,
but owning its area — no inner tab bars or toolbars). Everything drawn below
is clickable: scope-bar segments are buttons, `▾` marks a dropdown, section
triangles fold, verb buttons act, the scrollbar map jumps on click.

```
 ⟨ main ⟶ feature/eval ⟩ · ⟨flat ▾⟩   ▓▓▓▓░░░░░ 4/9   [/ filter] [o view] [? help]  eval review
──────────────────────────────────────────────────────────────────────────────────────────────
 files ⟨\ ▾⟩      │  src/eval.rs · pub fn eval                                             ▲
                  │ ─────────────────────────────────────────────────────────────────────  █
  ✓ parser.rs     │    4   4   pub fn eval(tokens: &[Token]) -> i64 {                      █
  ✓ lexer.rs   ¹  │        5       let mut acc: i64 = 0;                                   ▒
 ›  eval.rs       │    5   6       for t in tokens {                                       ▒
    pretty.rs     │    6       −       if let Token::Num(n) = t { acc += n; }              ░
    README.md     │        7   +       if let Token::Num(n) = t {                          ░
 ▸ noise (2)      │        8   +           acc = acc.wrapping_add(*n);                     ░
                  │        9   +       }                                                   ░
  4/9 viewed      │  [c Comment] [s Stage] [d Discard] [v Lines…]            hunk 5/12     ░
─────────────────────────────────────────────────────────────────────────────────────────────
 eval.rs · hunk 5/12 · Space = viewed & next · Enter = open in buffer                  [RO]
```

- **Scope bar** (line 1): the comparison (each side a button opening its
  picker), the lens dropdown, the burn-down gauge (click → filter menu),
  filter / view / help buttons with their keys, and the review's name.
- **Navigator rail** (left; the `⟨\ ▾⟩` control cycles files → commits →
  notes → hidden, by click or key): viewed ✓, comment badges, cursor’s file
  marked; auto-quarantined `noise` group (generated/lock/format-only) folded
  at the bottom and **excluded from the 4/9 denominator**. Hovering a row
  reveals its own small verb buttons (viewed-toggle, open).
- **Stream** (center): the section document. Sticky file·function header —
  which carries the current file's `[✓ Reviewed]` button and fold triangle,
  so the primary scroll-and-mark loop (§3.6) never leaves the stream; the
  focused hunk grows its **verb-button row** — click or key, same thing.
  Right-click any line/hunk/file opens a context menu listing the same verbs
  with their keys printed (menus teach the keymap).
- **Scrollbar map** (right): hunk positions, comment marks, viewed regions
  (dim); click/drag to jump.
- **Context line** (bottom): position, the two keys that matter now, and
  receipts after mutations: `✓ staged hunk — git apply --cached  [Z Undo]`
  (the `[Z Undo]` is a button).

### 3.3 The scope bar: presets, lens, and the A/B control

Opening **Review** with no argument opens the workspace in *scope-choosing
state* — same surface, scope bar focused, every row a clickable preset:

```
 ⟨ choose scope… ⟩                                                              · review
─────────────────────────────────────────────────────────────────────────────────────────
     ★  feature/eval vs main       3 commits · 4 files · +56 −7        [Enter Open]
     ⚙  agent: auth-refactor       ● waiting on you · 7 left  ▂▂▅▇     [Enter Open]
     ⚙  agent: docs-pass           ✓ idle · 2 left            ▂▁▁▂
     ⇵  PR #482 "retry logic"      review requested · 9 files · +214 −40
     ●  working tree               1 staged · 2 unstaged · 1 untracked
     ⟳  HEAD~3..HEAD               reviewed yesterday · 2 files changed since
     ⌗  stash@{0}                  "wip: tokenizer"
     ⌂  history                    browse commits · any range · blame

     revspec:  A..B · A...B · <sha> · stash@{N} · <branch> · PR # or URL
     > ▌
─────────────────────────────────────────────────────────────────────────────────────────
 ↑↓/click choose · a/b set an endpoint by hand · agent sessions sorted "waiting on you"
```

- **Agent sessions sort first when they're waiting on you**, with per-session
  progress sparklines — "which of my five agents next" is answered at entry.
- Branch presets use **merge-base against the configured upstream** (never
  the base tip, never hardcoded `origin/main`); each preset row's endpoints
  can be overridden (`a`/`b`, or click an endpoint) — the A/B picker is
  first-class, not buried.
- **Reopening a previously reviewed scope defaults to "since my last
  review"** (the interdiff); the full range is one click on the base
  segment away. This is the resume banner's question, answered by default.

The **lens menu** (`l`, or click `⟨flat ▾⟩`):

```
 ⟨ main ⟶ feature/eval ⟩ · ┌ lens ──────────────────────────────┐
                           │ › flat      one combined diff       │
                           │   commits   step commit by commit   │
                           │   since v2  my last review → now    │
                           └────────────────────────────────────┘
```

In the commits lens, a force-push does not destroy context: the commit strip
grows a **`⇅ v2→v3 interdiff`** pseudo-commit — "what changed between the
version I reviewed and this push" as just another selectable section.

**Reviewing a GitHub PR is a first-class scope.** Clicking the `⇵ PR` preset
(or typing a PR number/URL into the revspec prompt) opens the PR picker —
open PRs from the repo's forge, review-requested first, then yours, then the
rest:

```
 ┌ pull requests · sinelaw/fresh ─────────────────────────── [/ filter] ┐
 │ › #482  retry logic for flaky networks     sam   ● review requested  │
 │   #479  extract clock trait                ada   ✓ approved by you   │
 │   #475  bump deps                          bot   3 checks failing    │
 │   #468  token store groundwork             you   2 reviews pending   │
 └ Enter/click review · no checkout needed — your worktree is untouched ┘
```

Selecting one fetches `refs/pull/N/head` (no checkout — the working tree is
untouched), sets the scope to `merge-base(PR base) ⟶ PR head`, and titles
the review: `⟨ ⇵ PR #482 · retry logic ⟩`. Existing PR review threads
import as anchored comments (distinguished by author badge); your own
comments are local drafts with severity, exactly as everywhere else. The
commits lens, viewed marks, folds, and resume all work unchanged — and a
force-push to the PR shows up as the `⇅` interdiff pseudo-commit rather
than a reset review.

**`C` conclude on a PR scope = submit the review to GitHub.** The conclude
surface shows the verdict choice, the summary, and every draft that will be
published (severity tags rendered into the comment bodies):

```
 ┌ conclude: submit review · PR #482 ──────────────────────────────────────┐
 │  verdict:   (● Comment)  (○ Approve)  (○ Request changes)               │
 │  summary:   solid overall — one deadlock risk noted inline▌             │
 │  publishes: 3 inline comments (1 must-fix · 1 suggestion · 1 nit)       │
 │             anchored to their lines on the GitHub diff                  │
 │  [⏎ Submit to GitHub]        [e Export as Markdown instead]  [Esc Back] │
 └─────────────────────────────────────────────────────────────────────────┘
```

Submission posts through the forge plugin and echoes the receipt in the
command log like any other mutation. (Deeper bidirectional sync — replying
to threads, resolving conversations, live thread updates, viewed-state
sync — remains Part II, §4.4; the flow above is complete without it.)

**Underneath: the checkout question.** A PR review must never switch the
user's working tree (the `gh pr checkout` model destroys whatever you were
doing), but pure no-checkout review hits a wall the moment you want depth —
`Enter` on a line, LSP navigation, running the tests. The design is a lazy
ladder:

- **Tier 0 — objects only (instant, the default).** Selecting a PR runs
  `git fetch origin pull/N/head:refs/fresh/pr/N` and renders the whole
  review from git blobs. Reading, folds, viewed marks, comments,
  commit lens, and `C` submit all work; `Enter` opens *read-only
  file-at-PR-head* buffers. Zero disk cost, zero disturbance, and no
  foreign code is executed — most PR reviews never need more than this.
- **Tier 1 — materialize a review worktree (one click, on demand).** The
  scope bar carries `[⌗ Materialize]`, and any depth boundary offers it in
  place (first LSP request, first edit attempt, "run tests"): Fresh creates
  a linked worktree at the PR head **through the same worktree machinery
  the orchestrator uses**, and the review re-targets it. Now `Enter` opens
  real editable files, LSP/diagnostics/terminal work, and the buffer lens
  shows the PR diff in-buffer. This is the moment the existing
  workspace-trust prompt fires — trust is asked when code could first
  *run*, not when you merely read the diff.
- **Never — checkout in the user's main worktree.** Not offered.

Review state (marks, folds, drafts) is keyed to the PR's identity — repo +
number + head-sha version history — never to a worktree path, so
materializing, discarding the worktree, or a force-push (new `⇅` version)
carries the review forward intact.

**Orchestrator integration: one worktree engine, one dock.** A
materialized PR review *is* a workspace — same primitive as an agent
session, same dock, same lifecycle (reclaimed on conclude/close if
untouched, archivable otherwise):

- The dock shows a review card — `⇵ PR #482 · 4/9 viewed · ✓ checks` —
  and clicking it reopens the review exactly where it stood. The PR
  picker's rows and the scope bar's checks-chip reuse the orchestrator's
  existing PR metadata plumbing (checks, review decision, comment counts).
- **An agent session with an attached PR is the same review seen from two
  bases.** The session card's PR badge opens the PR scope; the scope bar
  then offers the flip between `merge-base(PR base) ⟶ head` (what the
  reviewer sees) and `session start ⟶ head` (what the agent did this run) —
  one control, no separate feature.
- The reverse door is Part II: from a PR review with open must-fix
  comments, "open an agent session in this worktree" hands the comment set
  to an agent working on the same checkout.

History is not a separate surface: **history = the commits lens over a wide
scope** (the `⌂ history` preset). The commit strip fills the rail, the shared
renderer previews the selection (debounced, in-flight `git show` cancelled),
`Enter` narrows scope to that commit, `v` over two commits arms a range. A
`⌕` query on the rail filters Sublime-Merge-style (`author:` `path:`
free-text).

### 3.4 The navigator rail

One rail, three projections, cycled with `\` or its dropdown; collapsible to
zero width (the stream is self-sufficient):

```
 files ⟨\ ▾⟩         commits ⟨\ ▾⟩              notes ⟨\ ▾⟩
  ✓ parser.rs         › adc21a9 lexer: add …     ● must-fix    session.rs:96
  ✓ lexer.rs   ¹        778ff10 eval: naive      │  lock held across await
 ›  eval.rs             bacff16 eval: wrap…      ○ nit         lexer.rs:31
    pretty.rs           ⇅ v2→v3 interdiff        ?  question   eval.rs:9
 ▸ noise (2)          2/3 reviewed               1 unresolved · click/Enter jumps
  4/9 viewed
```

- **files**: flat by default (tree toggle for deep repos); `/` filters live;
  `F` shows only unreviewed. In **agent scopes the default order is
  narrative** — the agent's edit sequence, deterministic, with path order
  one toggle away (alphabetical order ≠ the structure of the work).
- **commits**: the strip that *is* the commits lens; selection drives the
  stream; `[` `]` step it from anywhere; includes `⇅` interdiff rows.
- **notes**: every comment and draft, severity-first; unsent drafts render
  dimmed with a `draft` tag — nothing silently lost.

### 3.5 The stream: sections, buttons, selection, comments

Typed sections: *file* → *hunk*. Click the triangle or `-`/`=` to
collapse/expand; `1`–`4` are depth presets; fold state persists. Noise
suppression by default: formatting-only hunks render dimmed with a
`∅ format-only` tag; context between hunks collapses to interactive
separators — real buttons:

```
 │  ··· 14 unchanged ···  [↧ +10]  [↧↧ all]
```

**Selection** (`v`, or mouse-drag over lines): the verb buttons follow and
narrow to line granularity; hovering a hunk header or file row also reveals
a checkbox for building multi-selections by mouse:

```
 │    5   6       for t in tokens {
 │▌   6       −       if let Token::Num(n) = t { acc += n; }
 │▌       7   +       if let Token::Num(n) = t {
 │▌       8   +           acc = acc.wrapping_add(*n);
 │  3 lines — [s Stage lines] [d Discard lines] [c Comment] [Esc Cancel]
```

**Comments follow the same granularity ladder as every verb**: a single
line (cursor on the line, or hover it — a `+` button appears in the gutter,
one click to compose, the GitHub/Pierre affordance), a line range (`v` or
drag, then `[c Comment]`), a whole hunk (cursor on the hunk header), or a
whole file (cursor on the file header — for "this file shouldn't exist"-type
notes). Single-line is the base case, and the anchor is always stored at
line granularity (`file:line` or `file:line–line`), never widened to the
hunk.

**Comment compose** opens inline at the anchor; severity chips are buttons;
`Esc` keeps the draft:

```
 │        7   +       if let Token::Num(n) = t {
 │ ⊕      8   +           acc = acc.wrapping_add(*n);      ← hover: + appears in gutter
 │        9   +       }
```

```
 │        8   +           acc = acc.wrapping_add(*n);
 │ ┌ comment · eval.rs:8 ─ (● must-fix) (◐ suggestion) (○ nit) (? question) ────┐
 │ │ overflow wrap is intentional here? worth a unit test either way▌           │
 │ └ [⏎ Save] [Esc Keep draft] ─────────────────────────────────────────────────┘
```

Saved comments render as compact anchored boxes (severity-colored border)
that fold with their hunk and re-anchor across lenses; a fuzzy anchor shows
`≈` (content moved). Clicking a note in the rail jumps here.

### 3.6 The review loop: Space, Enter, Esc, C

**The primary use case is the flat stream, reviewed file-by-file, top to
bottom.** Everything else — commits lens, history, zoomed files — is
secondary; the design optimizes this loop first: scroll (or `Space`) down
one continuous document, and as each file is dealt with, mark it reviewed —
whereupon it **collapses in place to its one-line header**
(`✓ src/parser.rs  +6 −1`), GitHub-style. The stream physically shrinks
into a to-do list of what's left; unmarking or clicking a collapsed header
re-expands it. Scrolling itself is a first-class review motion: the sticky
file header carries the file's own `[✓ Reviewed]` button and fold triangle,
so scroll-and-mark never needs the rail or a jump back to the file's top.

**Collapsed and reviewed are independent states.** The fold triangle (or
`-`) collapses a file *without* marking it — it renders as
`▸ src/parser.rs  +6 −1` (no ✓), still counts as unreviewed in the gauge,
and still lands in the `Space` advance order ("I'll come back to this one"
is a normal move, not a lie to the progress bar). Marking reviewed
auto-collapses as a convenience; unfolding never unmarks. Both states
persist independently per scope across sittings — reopening the same diff
restores exactly which files were collapsed, which were reviewed, and
which were both.

- **`Space`** — mark the thing at point viewed (or *kept* in an agent scope)
  and advance: on a hunk, next hunk; on the last hunk of a file (or on a
  file header), the whole file — which collapses — and the cursor lands on
  the next unreviewed file's first hunk. The gauge ticks; noise files never
  inflate the denominator. When nothing is left, the workspace offers the
  exits as buttons:

```
─────────────────────────────────────────────────────────────────────────────────────────
   ✓ Review complete — 9/9 viewed · 1 must-fix comment unresolved

     [⏎ Jump to unresolved]  [C Conclude: commit staged]  [e Export notes]  [q Close]
─────────────────────────────────────────────────────────────────────────────────────────
```

- **`C` — conclude** is one verb everywhere, relabeled by scope: *commit
  staged* (worktree), *finish session review* (agent — applies keeps,
  reverts rejects, reports the tally), *export/submit review* (branch/PR).
  It appears in the completion banner and is always available in the scope
  bar's `⋯` menu — the primary action is printed on screen at the moment it
  becomes primary.
- **`Enter`** — zoom: file header → full-width file (layout `|` toggles
  unified/split and sticks per scope); hunk/line → the **real buffer** at
  that line with the buffer lens active (§3.8); `Esc` returns to the exact
  stream position. Fixing code mid-review is a zoom, not a mode switch.
- **`Esc`** — one level back out; at the top it closes, saving everything.

Resume (reopening a scope) leads with the delta because the default target
already is "since my last review" (§3.3):

```
 ⟨ v2 (my last review) ⟶ feature/eval ⟩ · flat ▾   resumed · 2 files changed since
   [⏎ Review the delta]   [b Full range instead — marks kept]
```

### 3.7 View options, help, and the command log

`o` (or `[o view]`) opens the one options transient — every toggle a
clickable row with its key, sticky per scope:

```
 ┌ view ────────────────────────────────────────────┐
 │ layout        [|] unified ◉ / split ○ / auto     │
 │ whitespace    [w] shown ◉ / ignored ○            │
 │ intraline     [i] on ◉                           │
 │ context       [+/−] 3 lines                      │
 │ format-only   [f] dimmed ◉ / hidden ○ / full ○   │
 │ generated     [g] collapsed ◉ / hidden ○         │
 │ file order    [n] narrative ◉ / path ○  (agent)  │
 │ depth         [1–4] current: 3                   │
 └──────────────────────────────────────────────────┘
```

`?` opens **runnable help — a command palette scoped to the review**: a
filterable list of every action with its key and menu path; `Enter` (or
click) *executes* the row. Reading the docs and using the tool are the same
act. The editor's menu bar also gains a **Review** menu mirroring every
action with its shortcut, so menu, palette, key, and click are one system.

`` ` `` toggles the **command log**: every VCS command the workspace ran,
timestamped, copyable — the receipts behind the context-line echoes.

### 3.8 The buffer portal (the lens)

The in-buffer tier: one hunk model (reference = HEAD | disk | branch | agent
baseline), gutter marks, one Next/Previous Change binding, expand-in-place.
The expanded hunk uses the *same verb-button row* as the stream — clicking
the gutter mark expands it, so the mouse path needs zero keys:

```
  10 │             '+' => { out.push(Token::Plus); chars.next(); }
 ┌╴vs HEAD ─ [s Stage] [d Discard] [c Comment] · [n p] change · [Esc Collapse] ╴┐
  11 │             '-' => { out.push(Token::Minus); chars.next(); }
 −   ┆             '-' => { out.push(Token::Minus); }
 └─────────────────────────────────────────────────────────────────────────────╴┘
 │12 │             '*' => { out.push(Token::Star); chars.next(); }
```

With an agent baseline active the buttons read `[k Keep] [x Reject]`, and
the buffer's gutter tracks the open review's baseline, so the buffer and the
workspace never disagree about what "changed" means.

### 3.9 Verb sets and the agent scope

| target | hunk buttons | `Space` | `C` concludes as |
|---|---|---|---|
| working tree | `[s Stage] [d Discard]` | viewed + next | commit staged |
| agent worktree | `[k Keep] [x Reject]` | keep + next | finish session review |
| historical | none (read-only) | viewed + next | export notes |

Agent-scope specifics, upgraded from the blind designs (Appendix A):

- **Live updates never move content under the cursor.** Files you are *not*
  inside refresh freely (the diff is the status display); the file you *are*
  inside queues incoming agent edits behind a `↻2` badge on its rail row and
  a context-line notice — `N` (or clicking the badge) applies the queue and
  jumps to the first new hunk. New hunks wear `NEW` until visited; viewed
  and kept marks auto-invalidate by content hash when the agent rewrites.
- **The reject guard is a symbol graph, not a heuristic.** Rejecting a hunk
  checks identifiers it removes against identifiers referenced by *kept*
  hunks; a conflict raises an inline warning listing the linked hunks with
  one-click resolution — never a modal:

```
 │ ⚠ 2 kept hunks call peek()'s new signature: mod.rs:44 · ast.rs:210
 │   [x Reject those too]  [⏎ Reject anyway]  [Esc Cancel]
```

  File-level reject stays the default granularity (`X` = whole file, the
  escalation rule); hunk/line rejection is the deliberate act the guard
  watches.
- **Leftover-noise findings** (debug prints, stray TODO/FIXME) render as `⚠`
  tags on hunks and `!` marks in the rail — display only; talking to the
  agent about them is Part II.
- Multiple sessions: the scope-choosing state sorts "waiting on you" first
  (§3.3), and `]s` / `[s` cycle between sessions with work pending without
  visiting the picker.

### 3.10 The mouse story (parity, stated once)

Every interaction has a pointer path, and the pointer path displays the key:

- **Click** selects; **double-click** = `Enter`; `Esc` = right-click "Back"
  or the breadcrumb in the scope bar.
- **Verb buttons** on the focused section, hover buttons on any hunk/file
  header, hover checkboxes for multi-select, and the hover `⊕` gutter
  button on any diff line for a one-click single-line comment.
- **Drag** over diff lines = line selection (verb buttons follow).
- **Right-click** = context menu for the section type, keys printed.
- **Scope bar**: every segment is a button (base picker, target picker, lens
  menu, gauge→filter, view, help, `⋯` overflow with Conclude/Export/Close).
- **Rail**: rows clickable, cycle control `⟨\ ▾⟩`, `↻N` badges clickable,
  noise group folds on click.
- **Scrollbar map**: click/drag jumps; markers are targets.
- **Wheel** scrolls the pane under the pointer; hover never steals
  selection or focus.

A click-by-click walkthrough of a complete flow, with no key pressed at any
point, is in §3.15.

### 3.11 Scaling down and up

- **< 100 columns — zoom model**: one pane at a time. The stream takes the
  full width; `Esc` shows the rail full-screen (Enter dives back). `Space`,
  `n/p`, and file-chaining work without ever visiting the rail, so an
  80-col review is a pure burn-down loop. The scope bar compresses to
  `⟶ feature/eval · 4/9`; the map strip stays (1 col, it's the map).
- **100–159 columns** (design target): the layouts drawn above; unified
  default.
- **≥ 160 columns**: split becomes the default layout, and comments move
  out of the stream into a third rail on the right, **aligned to their
  anchors** — the notes projection stays available for the severity-sorted
  index.

### 3.12 Web: parity, not a feature

Every element above — scope bar, rail, stream, buttons, transients — lands
as a scene projection consumed by both the TUI and web renderers under the
existing parity discipline (`web-ui.md`); divergence is a test failure.
**No review feature may land as TUI-only logic.** The buttons that look like
text in the terminal render as real buttons in the browser; nothing else is
designed for the web.

### 3.13 Blame and merge (adjacent, aligned, not absorbed)

- **Blame** stays its own surface but adopts the vocabulary: `Enter` on a
  blame line opens a one-commit scope in the workspace, `,` re-blames at the
  parent (with a breadcrumb; `Esc` pops), and any hunk offers a "history of
  this code" pivot — the workspace scoped to that path, commits lens.
- **Merge conflicts** stay with `merge_conflict.ts` inline markers (never
  force-replace an inline flow); the renderer gains zdiff3-style "each side
  vs base" display later, additively.

### 3.14 Provenance and migration

This design's skeleton was validated by two blind independent redesigns
(Appendix A) that converged on the same core; their distinctive winning
ideas are folded in above (conclude verb, escalation rule, runnable help,
git-object undo journal, waiting-on-you ordering, narrative agent order,
live-update queue, symbol-graph guard, last-review default target, interdiff
pseudo-commit, noise-free denominator, command log, zoom model, wide-screen
anchored comment rail). The earlier chrome-conservative draft remains in git
history as the low-risk migration fallback; phases 0–1 (§5) converge the
existing surfaces onto the shared renderer/session regardless of chrome, and
this workspace lands as the Phase 1–2 presentation layer. Two open questions
are flagged for prototyping, not argument: verb buttons on sections vs. a
persistent bottom hint bar, and `h`/`l` as a granularity ladder vs. pane
focus.

---

### 3.15 Mouse-only walkthrough

A click-by-click progression through a full flow (reviewing an agent
session), touching every pointer affordance class: menus, scope segments,
dropdowns, verb buttons, inline warnings, badges, rail cycling, note
jumping, hover checkboxes, and the conclude flow. No key is pressed at any
point. (Keys shown on buttons are the accelerators the mouse path teaches.)

**Frame 1 — ordinary editing.** The menu bar carries a `Review` menu.

```
 File   Edit   View   Selection   Go   Review   LSP   Help
 lexer.rs ×   +
   1 │ //! Lexer for the toy calculator language.
   2 │ #[derive(Debug, PartialEq)]
   3 │ pub enum Token { Num(i64), Plus, Minus, Star, Slash, Percent, LParen, RParen }
   4 │
   5 │ pub fn lex(input: &str) -> Vec<Token> {
   6 │     let mut out = Vec::new();
 ──────────────────────────────────────────────────────────────────────────────────────────
 Trusted  Local  Ln 1, Col 1                                              LF  UTF-8  Rust
```

*User clicks `Review` in the menu bar.*

**Frame 2 — the Review menu.** Every item prints its shortcut.

```
 File   Edit   View   Selection   Go  ┌ Review ──────────────────────────┐  LSP   Help
 lexer.rs ×   +                       │ Open Review…             Ctrl+R  │
   1 │ //! Lexer for the toy calcul…  │ Working Tree                     │
   2 │ #[derive(Debug, PartialEq)]    │ Branch vs Base                   │
   3 │ pub enum Token { Num(i64), P…  │ Agent Sessions                ▸  │
   4 │                                │ History                          │
   5 │ pub fn lex(input: &str) -> V…  │ ─────────────────────────────────│
   6 │     let mut out = Vec::new();  │ Next Change               Alt+↓  │
                                      │ Blame File                       │
                                      └──────────────────────────────────┘
```

*User clicks `Open Review…`.*

**Frame 3 — scope-choosing state.** Every row is a button; sessions with
work pending sort first.

```
 ⟨ choose scope… ⟩                                                              · review
──────────────────────────────────────────────────────────────────────────────────────────
     ⚙  agent: auth-refactor       ● waiting on you · 9 to review  ▂▂▅▇   [⏎ Open]
     ⚙  agent: docs-pass           ✓ idle · 2 left                 ▂▁▁▂
     ★  feature/eval vs main       3 commits · 4 files · +56 −7
     ●  working tree               1 staged · 2 unstaged · 1 untracked
     ⟳  HEAD~3..HEAD               reviewed yesterday · 2 files changed since
     ⌗  stash@{0}                  "wip: tokenizer"
     ⌂  history                    browse commits · any range · blame

     revspec:  A..B · A...B · <sha> · stash@{N} · <branch>
     > ▌
──────────────────────────────────────────────────────────────────────────────────────────
 click a row to open it · click an endpoint on any row to override base or target
```

*User clicks the `⚙ agent: auth-refactor` row.*

**Frame 4 — the workspace, agent scope, flat lens.** The focused hunk shows
its verb buttons; the rail shows kept/pending files.

```
 ⟨ session start ⟶ auth-refactor ⟩ · ⟨flat ▾⟩   ▓▓▓░░░░░░ 3/9 kept   [/][o][?]  ⟨⋯⟩
──────────────────────────────────────────────────────────────────────────────────────────
 files ⟨\ ▾⟩        │  src/auth/session.rs · fn refresh                                ▲
  ✓ token.rs        │ ───────────────────────────────────────────────────────────────  █
  ✓ store.rs        │   88   88   fn refresh(&mut self) -> Result<()> {                █
 ›  session.rs      │        89 +     let tok = self.store.lock()?;                    ▒
    mod.rs          │        90 +     eprintln!("refresh: {tok:?}");   ⚠ debug print   ░
    session_test.rs │   89   91       self.renew(tok)                                  ░
 ▸ noise (1)        │                                                                  ░
                    │  [k Keep] [x Reject] [c Comment] [v Lines…]         hunk 2/5     ░
  3/9 kept          │                                                                  ░
──────────────────────────────────────────────────────────────────────────────────────────
 session.rs · hunk 2/5 · agent running — files refresh live                        [RO]
```

*User clicks `⟨flat ▾⟩` in the scope bar.*

**Frame 5 — the lens dropdown.**

```
 ⟨ session start ⟶ auth-refactor ⟩ · ┌ lens ──────────────────────────────┐   [/][o][?]
─────────────────────────────────────│ › flat      one combined diff       │─────────────
 files ⟨\ ▾⟩        │  src/auth/sess │   commits   step commit by commit   │          ▲
  ✓ token.rs        │ ────────────── │   since v1  my last review → now    │          █
  ✓ store.rs        │   88   88   fn └────────────────────────────────────┘          █
 ›  session.rs      │        89 +     let tok = self.store.lock()?;                   ▒
```

*User clicks `commits`.*

**Frame 6 — commits lens.** The rail becomes the commit strip (with the
interdiff pseudo-row); the stream shows the selected commit only.

```
 ⟨ session start ⟶ auth-refactor ⟩ · ⟨commits ▾⟩   commit 2/3   [/][o][?]        ⟨⋯⟩
──────────────────────────────────────────────────────────────────────────────────────────
 commits ⟨\ ▾⟩      │  9d80aa · wire refresh into session                    2 files    ▲
   77aa02 add store │ ───────────────────────────────────────────────────────────────  █
 › 9d80aa wire refr…│   88   88   fn refresh(&mut self) -> Result<()> {                █
   f00c1a tests     │        89 +     let tok = self.store.lock()?;                    ▒
   ⇅ v1→v2 interdiff│        90 +     eprintln!("refresh: {tok:?}");   ⚠ debug print   ░
                    │   89   91       self.renew(tok)                                  ░
  1/3 reviewed      │  [k Keep] [x Reject] [c Comment] [v Lines…]         hunk 1/2     ░
──────────────────────────────────────────────────────────────────────────────────────────
 9d80aa · hunk 1/2 · click any commit row to preview it                            [RO]
```

*User clicks the `[x Reject]` button on the focused hunk.*

**Frame 7 — the symbol-graph guard.** Rejecting would orphan kept callers;
the warning is inline, with buttons.

```
 commits ⟨\ ▾⟩      │   88   88   fn refresh(&mut self) -> Result<()> {
   77aa02 add store │        89 +     let tok = self.store.lock()?;
 › 9d80aa wire refr…│        90 +     eprintln!("refresh: {tok:?}");
   f00c1a tests     │   89   91       self.renew(tok)
   ⇅ v1→v2 interdiff│ ┌────────────────────────────────────────────────────────────────┐
                    │ │ ⚠ 2 kept hunks call refresh()'s new signature:                 │
                    │ │   mod.rs:44 · session_test.rs:12                               │
                    │ │   [x Reject those too]   [⏎ Reject anyway]   [Esc Cancel]      │
                    │ └────────────────────────────────────────────────────────────────┘
```

*User clicks `[x Reject those too]`.*

**Frame 8 — the receipt.** Context line shows the command and an undo
button; the rail regroups. Meanwhile the agent edited a file the user
already reviewed — a queue badge appears instead of the content moving.

```
 ⟨ session start ⟶ auth-refactor ⟩ · ⟨commits ▾⟩   commit 2/3   [/][o][?]        ⟨⋯⟩
──────────────────────────────────────────────────────────────────────────────────────────
 commits ⟨\ ▾⟩      │  9d80aa · wire refresh into session                    2 files    ▲
   77aa02 add store │   88   88   fn refresh(&mut self) -> Result<()> {                █
 › 9d80aa wire refr…│   89   91       self.renew(tok)              (3 hunks rejected)  ▒
   f00c1a tests     │                                                                  ░
   ⇅ v1→v2 interdiff│                                                                  ░
  ✓ token.rs   ↻2   │                                                                  ░
──────────────────────────────────────────────────────────────────────────────────────────
 ✗ rejected 3 hunks — git apply -R (journaled)   [Z Undo]
```

*User clicks the `↻2` badge on `token.rs`.*

**Frame 9 — the queue applies.** The stream jumps to the first newly
arrived hunk, tagged `NEW`; the viewed mark on that file was dropped
automatically (content hash changed).

```
 ⟨ session start ⟶ auth-refactor ⟩ · ⟨commits ▾⟩   4/10 kept   [/][o][?]         ⟨⋯⟩
──────────────────────────────────────────────────────────────────────────────────────────
 commits ⟨\ ▾⟩      │  src/auth/token.rs · fn rotate                        NEW        ▲
   …                │ ───────────────────────────────────────────────────────────────  █
 › token.rs (live)  │  114  114   fn rotate(&mut self) -> Result<()> {                 █
                    │       115 +     self.audit.log("token.rotate");                  ▒
                    │  115  116       self.generate()                                  ░
                    │  [k Keep] [x Reject] [c Comment]                    hunk 1/2     ░
──────────────────────────────────────────────────────────────────────────────────────────
 token.rs · NEW hunk applied from queue · 1 more queued                            [RO]
```

*User clicks the rail-projection control `⟨\ ▾⟩`.*

**Frame 10 — the rail dropdown.**

```
 ┌ rail ─────────────┐ │  src/auth/token.rs · fn rotate                        NEW
 │ › files           │ │ ──────────────────────────────────────────────────────────────
 │   commits         │ │  114  114   fn rotate(&mut self) -> Result<()> {
 │   notes        2  │ │       115 +     self.audit.log("token.rotate");
 │   hidden          │ │  115  116       self.generate()
 └───────────────────┘ │
```

*User clicks `notes`.*

**Frame 11 — the notes rail.** Comments and drafts, severity-first.

```
 ⟨ session start ⟶ auth-refactor ⟩ · ⟨commits ▾⟩   4/10 kept   [/][o][?]         ⟨⋯⟩
──────────────────────────────────────────────────────────────────────────────────────────
 notes ⟨\ ▾⟩        │  src/auth/token.rs · fn rotate                                   ▲
  ● must-fix        │  114  114   fn rotate(&mut self) -> Result<()> {                 █
    session.rs:96   │       115 +     self.audit.log("token.rotate");                  ▒
    lock held acro… │  115  116       self.generate()                                  ░
  ○ nit (draft)     │                                                                  ░
    store.rs:31     │                                                                  ░
 1 unresolved       │                                                                  ░
──────────────────────────────────────────────────────────────────────────────────────────
 click a note to jump to its anchor
```

*User clicks the `● must-fix` note.*

**Frame 12 — jumped to the anchor.** The comment box is focused with its
own buttons.

```
 notes ⟨\ ▾⟩        │  src/auth/session.rs · fn refresh                                ▲
 ›● must-fix        │   95   95       let guard = self.store.lock()?;                  █
    session.rs:96   │   96   96       self.renew_all(guard).await                      █
  ○ nit (draft)     │ ┌ ● must-fix · session.rs:96 ────────────────────────────────┐   ▒
    store.rs:31     │ │ lock held across await point — deadlock risk under load    │   ░
                    │ │ [✎ Edit] [x Delete] [◐ Change severity ▾]                  │   ░
                    │ └─────────────────────────────────────────────────────────────┘  ░
──────────────────────────────────────────────────────────────────────────────────────────
 session.rs:96 · unresolved must-fix
```

*User reviews the remaining files by clicking the hover checkbox `☐→✓` on
each rail row (mark reviewed); when the last one is marked…*

**Frame 13 — completion.** The exits are buttons.

```
 ⟨ session start ⟶ auth-refactor ⟩ · ⟨commits ▾⟩   ▓▓▓▓▓▓▓▓ 10/10   [/][o][?]    ⟨⋯⟩
──────────────────────────────────────────────────────────────────────────────────────────

   ✓ Review complete — 10/10 · kept 7 · rejected 3 · 1 must-fix comment unresolved

     [⏎ Jump to unresolved]   [C Conclude: finish session review]   [e Export notes]
     [q Close — state saved]

──────────────────────────────────────────────────────────────────────────────────────────
```

*User clicks `[C Conclude: finish session review]`.*

**Frame 14 — the conclude summary.** One confirmation surface (this is a
bulk mutation), with the tally and the receipt-to-be.

```
 ┌ conclude: auth-refactor ────────────────────────────────────────────────────────────┐
 │  keep 7 hunks (already in worktree — no changes needed)                             │
 │  revert 3 rejected hunks            → git apply -R  (journaled, [Z] undoable)       │
 │  1 must-fix comment stays open      → visible in the session's notes                │
 │                                                                                     │
 │  [⏎ Finish]                [Esc Back to review]                                     │
 └─────────────────────────────────────────────────────────────────────────────────────┘
```

*User clicks `[⏎ Finish]` — the workspace closes to the dock, whose session
card now shows `reviewed · 3 reverted · 1 note`, and every mutation along
the way remains undoable from the command log (`` ` ``).*

---

## 4. Part II — talking to agents and outside systems (lower priority)

Everything here is **orthogonal to Part I by construction**: it consumes the
workspace, the comment store, and the scope-preset model but changes none of their UX.
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
4. **Forge/PR sync beyond submit**: the PR flow's UX — picker, thread import,
   `C` submit-to-GitHub — is specified in Part I (§3.3) and ships with the
   workspace. This item is the deeper bidirectional machinery: replying to and
   resolving threads from the notes rail, live thread updates while the review
   is open, and syncing viewed-state with the forge.

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
- The scope-choosing state (§3.3): presets, recents, typed revspecs, live preview.

**Phase 2 — review state + the agent scope**
- Viewed marks (blob-hash-keyed), progress counters, Space=viewed-and-advance,
  unreviewed filter; recents re-open with full state (marks, folds, read
  position, drafts) + "since last review" watermark.
- Agent scope: Orchestrator "Review Session's Changes" (base = recorded session
  start ref), keep/reject verbs (file-default granularity + related-changes
  nudge), diffstat-pill-as-entry-point, agent sessions as scope presets,
  leftover-noise scan. (No agent communication yet — that's Phase 4.)
- Buffer Lens unification: one reference picker (absorb git-gutter into the
  live-diff hunk model), agent-baseline auto-swap.
- Web parity for the workspace (§3.12): complete their scene projections so
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
| Review Diff | the workspace, *working tree* scope preset |
| Review Range / Review Stash | typed-revspec / stash scope presets |
| Review: PR Branch | *branch vs base* scope preset, commits lens |
| Git Log / Git Log (Current File) | *history* scope preset (commits lens over a wide scope; file-scoped variant) |
| Side-by-Side Diff (per-file command) | the zoomed-file `\|` layout; standalone command stays as a thin alias |
| Live Diff + Git Gutter + Diff Chunk Nav | **Buffer Lens** (one hunk model, one reference picker, one nav) |
| (nothing) | *agent session* scope preset + the scope-choosing state |

Docs pages, palette naming (`Review:` prefix family), and the keybinding editor's
`review` context all follow the same rename.

---

## 7. Risks and mitigations

- **Refactor risk in `audit_mode` (6.6k lines) and the outer-vs-inner split-leaf
  class of bugs** documented in `search-and-diff.md`: extract the container behind
  the existing e2e suites (review_diff_* tests) and add scope-parameterized e2e
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
- `docs/internal/orchestrator-sessions.md` — session/worktree model the agent scope builds on.

---

## Appendix A — the two blind independent redesigns

To pressure-test the workspace design, two designers were given the same brief — the task
list (§ Tasks) and the research evidence (§2) only, with the existing Fresh UI
and this document's workspace design deliberately withheld — and asked to
design a TUI review experience from a blank slate. Their full documents are
preserved with the session research notes; this section presents each design's
distinctive ideas and the resulting decisions.

**The headline is the convergence.** Working blind, both designs and the
workspace (§3) independently arrived at the same skeleton:

- the comparison (`base ⟶ target`) as an always-visible, clickable, editable
  control that *is* the review's identity — every source a preset of it;
- a magit-style typed section tree with persistent folds and depth presets;
- a navigator whose selection reactively drives a read-only diff projection
  (`Enter` = portal to the live buffer; never a second writable copy);
- verbs that relabel per source on unchanged keys (stage/discard ⇄
  keep/reject), shown in-place so nothing is hidden;
- viewed marks keyed by content hash, mark-and-advance, resume that leads
  with "what changed since I last looked";
- undo instead of confirmation prompts, with the real git command echoed;
- runnable `?` help; noise quarantined by default; one-column marker
  scrollbar as the review's map.

Three independent derivations from the same evidence landing on the same core
is the strongest de-risking signal this document has: **the §3 skeleton is not
one designer's taste.** The interesting material is where they differ.

#### Alternative A — "Ledger" (outline-first, conclude-verb design)

One frame (agent session; note the narrative ordering and the inline guard):

```
◆ Review: session fix-flaky-tests   base session-start ▾ ⟶ head worktree (live ◉)
   agent: working…  ·  kept 9 · rejected 2 · open 7  ·  viewed 11/18  ·  Flat ▾
├──────────────────────────────┬────────────────────────────────────────────────────┤
│ narrative order (agent's) ▾  │ ⌁ fn wait_for_port() · tests/util.rs               │
│  ✓ tests/util.rs     kept    │   88│ 88▎  let deadline = Instant::now() + timeout;│
│  ✗ tests/flaky.rs    rejected│   89│   ▁  «sleep(500)»                            │
│ ▸· tests/net.rs     ↺ changed│     │ 89▎ «backoff.retry(|| probe(port))»          │
│  · src/backoff.rs  ● new     │ ⚠ tests/net.rs:14 (kept) calls probe(port) —       │
│  ▸ 12 more (unreviewed)      │   rejecting this hunk removes its definition.      │
├──────────────────────────────┴────────────────────────────────────────────────────┤
│ hunk: s keep · x reject · v viewed · c note · N next open · live: files update    │
```

Distinctive ideas, with verdicts:

| Idea | Verdict for the workspace |
|---|---|
| **Uppercase escalates lowercase** (`v` viewed → `V` unmark; `c` comment → `C` *conclude*; one rule makes the map guessable) | **Adopt the rule** as a keymap design principle; `C` = conclude (commit / submit / finish) fills a gap — the workspace had completion *exits* but no single conclude verb |
| **`?` help = runnable, filterable command list** (help and palette are one thing) | **Adopt** (both designs had it; §3.7 said "runnable" — make it the palette itself, scoped) |
| **Undo journal persisted across restarts**, discarded hunks saved as patch files | **Adopt**; upgrade §3.1 principle 9 from "session undo" to journal-backed |
| **Entry rows sorted "waiting on you" first** with per-session progress sparklines | **Adopt** in the scope presets |
| **Narrative order (agent's edit sequence) as the default file order in agent scopes** | **Adopt as the default** there (deterministic — it's the edit log, not an AI summary; path order one toggle away) |
| Comments have **no panel at review width** — inline pills only; a comments rail exists only ≥160 cols | Partial: keep the notes rail as a cycled projection (a severity-sorted index of a 50-file review is worth a rail), but adopt the **wide-screen anchored rail** for ≥160 cols |
| `v` = viewed (because `Space` = extend selection) | Keep `Space` = advance (§3.6); selection stays on `v`/drag — the burn-down key deserves the biggest key |

#### Alternative B — the "lens" design (take/untake/drop, live-queue design)

One frame (the live-update queue and the symbol-graph guard — its two best
ideas — visible together):

```
┌ Review ▸ session lexer-rewrite  start ⇄ now  ● agent working   ▰▰▰▱ 11/18 · [s 1/3]┐
├──────────────────────────────┬─────────────────────────────────────────────────────┤
│ ▾ To review (7)              │ src/parser/lex.rs › fn next_token()                 │
│ ▸ ▾ src/parser/lex.rs   ↻2   │  150       ─   let c = self.peek();                │
│     ▾ fn next_token()        │       150  +   let Some(c) = self.peek() else {    │
│       hunk @201  NEW         │ ┌─────────────────────────────────────────────────┐│
│ ▾ Rejected (2)               │ │ ⚠ rejecting this hunk conflicts with 2 KEPT     ││
│  Sessions needing review:    │ │   hunks that call `peek()`'s new signature:     ││
│  ▸1 lexer-rewrite ● 7 left   │ │   mod.rs @44 · ast.rs @210                      ││
│   2 fix-ci        ✓ 3 left   │ │   x reject those too · Enter reject anyway      ││
├──────────────────────────────┴─────────────────────────────────────────────────────┤
│ s keep · x reject · u unkeep · N jump to 2 new changes · ]s next session · ? help  │
```

Distinctive ideas, with verdicts:

| Idea | Verdict for the workspace |
|---|---|
| **Live-update queueing**: files you're inside queue agent edits behind a `↻N` badge (`N` applies and jumps); other files refresh freely — *content never moves under your cursor* | **Adopt verbatim** — the best answer found to watch-mode vs. rug-pull, stronger than plain watch |
| **Symbol-graph context-blindness guard**: rejecting a hunk checks identifiers it removes against identifiers *kept* hunks reference; inline warning lists the linked hunks with one-key "reject those too" | **Adopt** — replaces §3.9's cruder file-default heuristic (keep file-level default too; they compose) |
| **"Since my last review" as the *default* target when reopening a previously-reviewed lens** (full range one keypress away) | **Adopt** — §3.6's resume banner asked; B's default answers |
| **`⇅ interdiff` pseudo-commit** in commits lens after a force-push (v3→v5 as just another section) | **Adopt** in the commits lens |
| **Noise excluded from the progress denominator** (lockfiles/generated/fmt-only don't count against 19/34) | **Adopt** — progress must measure judgment, not scrolling |
| **Undo ring backed by git objects** (`refs/…/undo/*`), destructive verbs recoverable across restarts | **Adopt** as the implementation of A's journal |
| **`` ` `` VCS command log** toggle (every echoed command, reviewable) | **Adopt** — cheap, completes the transparency story |
| **<100-col zoom model**: one pane at a time (Esc shows the tree full-screen, Enter dives back); burn-down loop works without ever visiting the navigator | **Adopt** — cleaner than shrinking panes side-by-side at 80 cols |
| **`h`/`l` widen/narrow granularity** (file ⇄ hunk ⇄ line as a ladder you climb) | Consider (conflicts with pane-focus muscle memory; prototype both) |
| Verb triad named **take/untake/drop** (`s`/`u`/`x`) relabeling per lens | Naming kept as-is (stage/keep are the users' words), but the *triad symmetry* (every take has an untake) is adopted as a rule |

#### What stays deliberately different in the workspace design

- **Chips over a global hint bar.** Both alternatives keep a persistent
  bottom hint bar listing current verbs; the workspace puts the verb hints
  *on the focused section* (chip row) with a leaner context line. Chips keep
  eyes on the code and scale better to the web renderer; the hint bar
  duplicates what chips already say. (If chips test poorly for
  discoverability, B's hint bar is the fallback — it's the same data.)
- **The rail cycles (files/commits/notes); the alternatives fix it to
  files+commits.** The notes projection earns its slot in marathon reviews;
  cycling keeps the one-rail budget.
- **Scope presets open in-workspace** (§3.3's choosing state) rather than as
  a modal overlay picker — one less floating surface, same content.

#### Adoption summary (now folded into §3 — see §3.14)

From A: uppercase-escalation rule; `C` conclude verb; runnable-help-as-palette;
persisted undo journal; waiting-on-you session ordering; narrative default
order in agent scopes; ≥160-col anchored comments rail.
From B: live-update queue (`↻N`/`N`); symbol-graph reject guard; last-review
default target; `⇅` interdiff pseudo-commit; noise out of the denominator;
git-object undo ring; `` ` `` command log; sub-100-col zoom model.
Under prototype: `h`/`l` granularity ladder vs. pane focus; chips vs. hint bar
(instrumented, not argued).
