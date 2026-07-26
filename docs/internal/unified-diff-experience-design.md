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

## 3. Part I — the Review workspace, redesigned

This section is a clean-slate UI design. It keeps the architecture already
established (one renderer; one `ReviewSession { source, lens, verbs, state }`;
one buffer lens) but deliberately does **not** inherit Fresh's current review
chrome — no three fixed panels, no two-line hint bar, no separate picker
dialog, no separate History tab. It is designed from the research lessons
alone, then mapped back onto Fresh's primitives.

### 3.1 Design principles (each one traces to evidence)

1. **Two lines of chrome, total.** One *scope bar* on top (what am I
   comparing), one *context line* at the bottom (where am I, what can I do
   here). Everything else is content. (gitui's hint bar + the observed failure
   of four competing chrome dialects; Linear's "Fast, Focused".)
2. **The comparison is the headline.** `base ⟶ target · lens` sits in the
   scope bar as a clickable, editable control — the review's identity, always
   visible, always changeable in place. (Gerrit's A/B patchset picker as the
   page's single source of truth; Reviewable's diff bounds; Xcode's
   revision-pickers-in-editor.)
3. **One list rail, many projections.** A single collapsible navigator rail
   shows *files*, *commits*, or *notes* — cycled, never three fixed panels
   competing for width. The rail is a map, not a second document. (GitHub's
   tray toggle file-tree⇄comments; Graphite's timeline⇄tree tray; the
   truncated fixed comments column observed in today's UI.)
4. **The stream is a section document.** Files and hunks are typed, foldable
   sections with persistent fold state, depth presets, and a sticky context
   header — magit's model, rendered with delta-grade bodies (syntax +
   word-level emphasis, enclosing-context headers, copy-safe text).
5. **Verbs live where they apply.** No global verb bar: the focused section
   grows a small chip row (`[s]tage · [d]iscard · [c]omment`) that is
   simultaneously the keymap hint, the click target, and the *only* place
   mutation is offered. Read-only scopes simply grow no chips. (lazygit's
   runnable help; scm-record's undiscoverable-confirm cautionary tale; VS
   Code's per-hunk gutter actions.)
6. **`Space` is the review.** One key advances the burn-down: mark the
   current thing viewed/kept, go to the next unreviewed thing — across hunks,
   files, and (in per-commit lens) commits. Progress is always visible.
   (GitHub's viewed-auto-advance; Zed's `StageAndNext`; tuicr/ftdv's
   persisted burn-down.)
7. **`Enter` zooms in, `Esc` zooms out, everywhere.** scope → file → hunk →
   the real buffer, one ladder, no dead ends. Every diff line is a portal to
   the live file. (lazygit/tig convergence; magit's "every line is
   operable".)
8. **Options are a transient, and they stick.** One key opens a small menu of
   visible, toggleable view options (layout, whitespace, context, intraline,
   noise filters) that persist with the scope. Menus show their keys and
   teach them. (magit transients; JetBrains' one-diff-viewer-many-options.)
9. **Every mutation is undoable and transparent.** `Z` undoes the last
   stage/discard/keep/reject; the context line briefly echoes the underlying
   git command after each action. (jj's op-log lesson — undo removes fear;
   Sublime Merge/jj-fzf's command echo — transparency builds trust.)
10. **The review survives anything.** Viewed marks, fold layout, cursor
    position, unsent comment drafts, scope recents — all persist and restore
    exactly. Reopening a scope shows what changed since you last looked
    before it shows anything else. (ftdv/tuicr persistence; Reviewable's
    "show diffs to review"; the marathon reality of agent-sized changesets.)

### 3.2 Anatomy: one workspace

The whole of Part I is **one full-bleed workspace** (it opens as a tab like
any buffer, but owns its area — no inner tab bars, no nested toolbars):

```
 ⟨ main ⟶ feature/eval ⟩ · flat ▾   ▓▓▓▓░░░░░ 4/9   ⌕ filter   ⌥ view   ? ·   eval review
─────────────────────────────────────────────────────────────────────────────────────────
 files  ‹\›       │  src/eval.rs · pub fn eval                                         ▲
                  │ ─────────────────────────────────────────────────────────────────  █
  ✓ parser.rs     │    4   4   pub fn eval(tokens: &[Token]) -> i64 {                  █
  ✓ lexer.rs   ¹  │        5       let mut acc: i64 = 0;                               ▒
 ›  eval.rs       │    5   6       for t in tokens {                                   ▒
    pretty.rs     │    6       −       if let Token::Num(n) = t { acc += n; }          ░
    README.md     │        7   +       if let Token::Num(n) = t {                      ░
                  │        8   +           acc = acc.wrapping_add(*n);                 ░
  4/9 viewed      │        9   +       }                                               ░
                  │   [c]omment  [s]tage  [d]iscard  [v] lines            hunk 5/12    ░
─────────────────────────────────────────────────────────────────────────────────────────
 eval.rs · hunk 5/12 · Space mark viewed & next · Enter open in buffer            [RO]
```

Reading the frame:

- **Scope bar** (line 1): the comparison `⟨ main ⟶ feature/eval ⟩` (each side
  clickable), the lens (`flat ▾`), the burn-down gauge, filter, the view
  transient, help. Nothing else. The right edge names the review (editable).
- **Navigator rail** (left, `\` cycles files → commits → notes → hidden):
  the file list is *flat by default, tree on demand*, with viewed ✓, comment
  count badges (`¹`), and the cursor's file marked. It is a map — selecting
  in the rail scrolls the stream; scrolling the stream tracks the rail.
- **Stream** (center): the section document. The current file+function is a
  **sticky header** at the top of the stream, so "where am I" survives long
  hunks. The focused hunk grows its **chip row** — the only verb surface.
- **Scrollbar** (right): reuses Fresh's marker primitive — hunk positions,
  comment positions, and viewed regions (dim) paint the whole review's shape.
- **Context line** (bottom): position, the two keys that matter right now,
  and read-only state. After a mutation it briefly shows the receipt:
  `✓ staged hunk — git apply --cached (Z to undo)`.

### 3.3 The scope bar: doors become presets

There is no separate picker dialog. Opening **Review** with no argument opens
the workspace in *scope-choosing state* — the same surface, empty stream,
scope bar focused:

```
 ⟨ choose scope… ⟩                                                            · review
─────────────────────────────────────────────────────────────────────────────────────────

     ★  feature/eval vs main       3 commits · 4 files · +56 −7          ← smart default
     ●  working tree               1 staged · 2 unstaged · 1 untracked
     ⚙  agent: auth-refactor       9 files pending · running…
     ⚙  agent: docs-pass           2 files pending · idle
     ⟳  HEAD~3..HEAD               reviewed yesterday · 2 files changed since
     ⌗  stash@{0}                  "wip: tokenizer"

     type a revspec:  A..B · A...B · <sha> · stash@{N} · <branch>
     > ▌
─────────────────────────────────────────────────────────────────────────────────────────
 ↑↓ choose · Enter open · every row = a scope preset, nothing here is a separate mode
```

The four "doors" of the earlier draft survive as the **preset rows** (working
tree, branch-vs-base, agent session, history/recents) — but they are entries
in one list feeding one control, not four commands. The scope bar afterwards
is the same list, collapsed: click either side of `⟨ main ⟶ feature/eval ⟩`
(or press `b`) to re-open it in place; pick a new base or target without
leaving the review. Presets carry their verb sets implicitly: target =
working tree → stage verbs; target = agent worktree → keep/reject; both sides
historical → no mutation chips at all.

The **lens menu** (`l`, or click `flat ▾`) is the second scope-bar control:

```
 ⟨ main ⟶ feature/eval ⟩ · ┌ lens ─────────────────────────────┐
                           │ › flat      one combined diff      │
                           │   commits   step commit by commit  │
                           │   since v2  what changed since my  │
                           │             last review (2 files)  │
                           └───────────────────────────────────┘
```

`flat` and `commits` replace the old Range/PR-Branch split; `since vN` is the
interdiff lens (checkpoint refs, Phase 3), listed greyed-out until versions
exist so the capability is discoverable before it is available.

History is not a separate surface either: **History = the commits lens over a
wide scope.** `Review` on `⟨ all ⟶ HEAD ⟩` (a preset: "history") puts the
commit strip in the rail, the shared renderer in the stream, and `Enter` on a
commit narrows the scope to that commit — the same workspace throughout. A
`⌕` query on the rail filters it Sublime-Merge-style (`author:` `path:`
free-text), which replaces log-filter commands.

### 3.4 The navigator rail

One rail, three projections, cycled with `\` (and collapsible to zero width —
the stream is self-sufficient):

```
 files ‹\›          commits ‹\›               notes ‹\›
  ✓ parser.rs        › adc21a9 lexer: add …    ● must-fix    session.rs:96
  ✓ lexer.rs   ¹       778ff10 eval: naive     │  lock held across await
 ›  eval.rs            bacff16 eval: wrap…     ○ nit         lexer.rs:31
    pretty.rs                                  ?  question   eval.rs:9
    README.md        2/3 reviewed              1 unresolved · [Enter] jump
  4/9 viewed
```

- **files**: flat list (tree toggle for deep repos), viewed ✓, badges, the
  filter (`/`) narrows it live; `F` shows only unreviewed. Generated/lock
  files land pre-collapsed at the bottom under `▸ generated (3)` —
  never interleaved with real changes.
- **commits**: the strip that *is* the per-commit lens — selection drives the
  stream (debounced, in-flight `git show` cancelled), `[` `]` step it from
  anywhere, and it shows per-commit reviewed state. In flat lens the strip
  still exists as a read-only map of what the scope contains.
- **notes**: every comment/draft in the review, severity-first, `Enter`
  jumps. Drafts that were never sent render dimmed with a `draft` tag —
  nothing silently lost. (This projection replaces the old fixed comments
  column; inline anchored boxes in the stream remain the primary rendering.)

### 3.5 The stream: sections, chips, selection, comments

The stream is one scrollable document of typed sections: *file* → *hunk*.
`-`/`=` collapse/expand the section at point, `1`–`4` are depth presets
(files only → +hunks → +bodies → everything), and fold state persists with
the review. Two display treatments do noise suppression by default:
formatting-only hunks render dimmed with a `∅ format-only` tag (expandable),
and context between hunks collapses to interactive separators
(`··· 14 unchanged ···`, click/`=` to grow — Sublime Merge's draggable hunk
edge, keyboardized).

**Selection state** (`v`, or mouse drag over lines): the chip row follows the
selection and the verbs narrow to line-granularity:

```
 │    5   6       for t in tokens {
 │▌   6       −       if let Token::Num(n) = t { acc += n; }
 │▌       7   +       if let Token::Num(n) = t {
 │▌       8   +           acc = acc.wrapping_add(*n);
 │        9   +       }
 │  3 lines — [s]tage lines  [d]iscard lines  [c]omment on selection  [Esc]
```

**Comment compose** opens inline, exactly where the comment will live, with
severity as one keystroke — and the draft persists if you close mid-thought:

```
 │        8   +           acc = acc.wrapping_add(*n);
 │ ┌ comment · eval.rs:8 ── severity: [1] must-fix [2] suggestion [3] nit [4] ? ┐
 │ │ overflow wrap is intentional here? worth a unit test either way▌           │
 │ │                                                                            │
 │ └ [Enter] save · [Esc] keep as draft ────────────────────────────────────────┘
```

Saved comments render as compact anchored boxes (severity-colored border,
author, age) that fold with their hunk and re-anchor across lenses; a
stale anchor shows a `≈` marker (content moved — Reviewable's dog-ear).

### 3.6 The review loop: Space, Enter, Esc

The intended rhythm for a big review is three keys:

- **`Space`** — mark the thing at point viewed (or *kept*, in an agent
  scope) and jump to the next unreviewed thing. The gauge in the scope bar
  ticks. When nothing is left, the workspace says so and offers the exits:

```
─────────────────────────────────────────────────────────────────────────────────────────
   ✓ Review complete — 9/9 viewed · 1 must-fix comment unresolved

     [Enter] jump to unresolved    [e] export notes    [g] stage all reviewed
     [q] close (state saved — reopening shows only what changes from now on)
─────────────────────────────────────────────────────────────────────────────────────────
```

- **`Enter`** — zoom: on a file section header → that file full-width (the
  split layout appears here naturally: `|` toggles unified/side-by-side for
  the zoomed file and remembers the choice per scope); on a hunk/line → the
  **real buffer** at that line, with the buffer lens active (§3.8) so the
  hunks are still visible, actionable, and `Esc` returns to the stream at the
  exact position. Editing during review is therefore never a mode switch —
  the portal *is* the fix-it flow (matklad/Zed).
- **`Esc`** — always one level back out; at the top it closes, saving
  everything.

Reopening any recent scope leads with the delta, not the diff:

```
 ⟨ main ⟶ feature/eval ⟩ · flat ▾   resumed — since your last visit: 2 files changed
   › show only what changed (interdiff)      · show everything (marks kept)
```

### 3.7 The view transient and help

`⌥ view` (key `o`) opens the one options menu — every toggle visible, keyed,
and sticky to the scope:

```
 ┌ view ────────────────────────────────────────────┐
 │ layout        [|] unified ◉ / split ○ / auto     │
 │ whitespace    [w] shown ◉ / ignored ○            │
 │ intraline     [i] on ◉                           │
 │ context       [+/−] 3 lines                      │
 │ format-only   [f] dimmed ◉ / hidden ○ / full ○   │
 │ generated     [g] collapsed ◉ / hidden ○         │
 │ depth         [1–4] current: 3                   │
 └──────────────────────────────────────────────────┘
```

`?` opens the full keymap as a *runnable* list (Enter executes the row —
lazygit's trick), grouped by the same names used in this doc: scope, rail,
stream, loop, view.

### 3.8 The buffer portal (the lens, unchanged in role)

The in-buffer tier survives the redesign intact: one hunk model (reference =
HEAD | disk | branch | agent baseline), gutter marks, one Next/Previous
Change binding, and expand-in-place — an expanded hunk shows old lines as
virtual text plus the same chip row the stream uses:

```
  10 │             '+' => { out.push(Token::Plus); chars.next(); }
 ┌╴vs HEAD ─ [s]tage  [d]iscard  [n p] change  [Esc] collapse ╴┐
  11 │             '-' => { out.push(Token::Minus); chars.next(); }
 −   ┆             '-' => { out.push(Token::Minus); }
 └────────────────────────────────────────────────────────────╴┘
 │12 │             '*' => { out.push(Token::Star); chars.next(); }
```

The chip row is the *same component* as the stream's — one verb surface
everywhere, so the portal and the workspace never teach different keys. With
an agent baseline active the chips read `[k]eep [x] reject`.

### 3.9 Verb sets and the agent scope

Scopes bind verbs; the UI shape never changes:

| target | chips on hunks | `Space` means | extra |
|---|---|---|---|
| working tree | `s` stage · `d` discard | viewed + next | `g` stage-all-reviewed |
| agent worktree | `k` keep · `x` reject | keep + next | `!` noise flags; `b` baseline |
| historical (commit/range/stash) | none (read-only) | viewed + next | comments only |

The agent scope keeps its two guardrails, now expressed in the redesigned
chrome: rejects at hunk/line level are chips *behind* the file-level default
(`x` on a file header rejects the file; on a hunk it asks once per session),
and the related-changes nudge is a context-line prompt, not a modal, unless
the reject would touch a kept sibling:

```
 ✗ rejected hunk · fn refresh — 2 kept hunks in mod.rs call this  [Enter] see · [Z] undo
```

Leftover-noise findings (debug prints, stray TODOs) render as `!` markers in
the rail and `⚠` tags on the hunks — display only, Part II owns talking to
the agent about them.

### 3.10 Web: parity, not a feature

Unchanged from before the redesign, and easier now: the workspace is *fewer*
distinct surfaces than the old three-panel design. Every element above —
scope bar, rail, stream, chips, transients — lands as a scene projection
consumed by both the TUI and web renderers under the existing parity
discipline (`web-ui.md`); divergence is a test failure. **No review feature
may land as TUI-only logic.** The same frames above, rendered by the web
frontend with real typography, are the web review experience; nothing else is
designed for it.

### 3.11 Blame and merge (adjacent, aligned, not absorbed)

- **Blame** stays its own surface (an annotation of one file, not a
  changeset) but adopts the vocabulary: `Enter` on a blame line opens a
  one-commit scope in the workspace, `,` re-blames at the parent (tig's
  beloved verb), and any hunk in the stream offers a "history of this code"
  pivot (Sublime Merge's Hunk History) which is just the workspace scoped to
  `path @ all ⟶ HEAD`, commits lens.
- **Merge conflicts** stay with `merge_conflict.ts` inline markers (never
  force-replace an inline flow users rely on — VS Code's lesson); the
  renderer gains zdiff3-style "each side as a diff against base" display as a
  later, additive option.

### 3.12 Migration note

The previous draft of this section kept Fresh's existing chrome (hint bars,
three fixed panels, palette-dialog picker) and its wireframes; that version
is preserved in git history and remains the low-risk fallback. The phased
plan (§5) is unchanged in substance — phases 0–1 converge the existing
surfaces onto the shared renderer/session regardless of chrome, and the
workspace chrome of this section lands as the Phase 1–2 presentation layer.
What the redesign *removes* relative to that draft: the fixed comments
column (→ notes rail + inline boxes), the two-line hint bar (→ scope bar +
context line + chips), the separate picker dialog (→ scope-choosing state),
and the separate History tab (→ history preset + commits lens).

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
- Web parity for the workspace (§3.10): complete their scene projections so
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
