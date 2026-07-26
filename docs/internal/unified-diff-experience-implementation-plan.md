# Review Workspace — implementation plan

Status: **PLAN** (companion to `unified-diff-experience-design.md`, which is
the design of record; this doc says how to build it in this codebase).
Date: 2026-07-26.

Scope: Part I of the design (the Review workspace and buffer lens) as
milestones M0–M5, then Part II (agent/forge communication) as M6. Each
milestone ships user-visible value on its own and none requires the next.

---

## 1. What we build on (existing code)

| Piece | Where | Role in the plan |
|---|---|---|
| Review session (worktree/range/stash review, staging, comments, `.review/` persistence, watch mode, split/stack) | `crates/fresh-editor/plugins/audit_mode.ts` | becomes the ReviewSession core; most Part I logic refactors out of here |
| Commit-list rendering | `plugins/lib/git_history.ts` | reused by the commits-lens rail |
| Git Log (buffer-group tab, streamed `git show` detail) | `plugins/git_log.ts` | its detail pane is replaced by the shared renderer; its list becomes the history preset |
| Side-by-side composite buffer | host (`fresh-core`/editor render) | the split layout; keys must become rebindable Actions |
| Live Diff (in-buffer hunks, virtual old-lines, word-level intraline, reference picker) | `plugins/live_diff.ts` | the buffer lens; its virtual-line machinery powers expand-in-place |
| Git gutter | `plugins/git_gutter.ts` | absorbed into the live-diff hunk model (M5) |
| Diff nav (merged jump list) | `plugins/diff_nav.ts` | the single in-buffer Next/Prev Change |
| Orchestrator (worktree workspaces, dock cards, PR badges/metadata, trust flow) | `plugins/orchestrator.ts` | workspace creation for PR checkout, session-start refs, dock-as-inbox badges |
| Scrollbar markers (live-diff hunks, diagnostics) | host | the review map strip |
| Piece-tree diff, review-hunk state, `set-review-diff-hunks`, local-control IPC | host | M6 agent surface; hunk model plumbing |
| Web scene projections + parity tests | host + `web-ui/` | every new surface lands as a projection (design §3.12) |

Known host gaps (from `search-and-diff.md` and observation): no syntax
highlighting in the unified review stream (overlay-priority/background
pathway missing); composite-buffer keys hardcoded; no inline click targets
("buttons") in panel content; no sticky header primitive; no hover events
for text elements.

## 2. New host primitives (the short list)

Ordered by how many milestones depend on them:

1. **Overlay priority / background layering** so token colors and
   add/remove backgrounds compose in the unified stream (unblocks M0's
   biggest win; already analyzed in `search-and-diff.md` §3.3).
2. **Inline actionable spans**: a text-property that marks a range as a
   click/hover target with an action id + hover style. Powers verb buttons,
   scope-bar segments, expanders, hover checkboxes, `⊕` gutter button,
   `[Z Undo]` receipts. (One primitive; every "button" in the design is
   this.) Must land as a scene projection so the web renders real buttons.
3. **Sticky first row(s)** for a panel/buffer view (file·function header;
   also useful for git log). Degrades gracefully: without it, the header
   just scrolls — ship M1 without blocking on it.
4. **Rebindable composite-buffer Actions** (`diff-view` key context)
   replacing the hardcoded input router.
5. **Per-hunk incremental render/stream + cancellation** for
   selection-driven previews (extends the existing streamed-`git show`
   work; kill-on-move is already planned there).
6. (M3) **Ref recording API** for plugins (`refs/fresh/review/*`,
   `refs/fresh/undo/*`): create/read/prune refs without shelling per call.

Everything else is plugin-side TypeScript.

## 3. Milestones

### M0 — stop the visible bleeding (design §5 Phase 0)

Small, independent, shippable one by one:

- Syntax highlighting + word-level intraline in the unified review pane
  (host primitive #1; syntect diff-scope mapping per the existing plan).
- Composite-buffer keys → `diff-view` Actions (primitive #4); keybinding
  editor lists them.
- Chrome convergence on the *existing* surfaces: shared hint-bar format and
  `?` overlay for Git Log, PR-Branch, Side-by-Side; unify `Tab`/`q`
  semantics; session-wide counters in the status bar.
- Advertise one in-buffer hunk navigation (diff_nav) in docs/palette; hide
  the duplicates.

Exit: no new features, but every current surface reads the same and the
unified pane has token colors. All existing e2e suites stay green.

### M1 — the workspace core (design §3.2, 3.5, 3.6, 3.7 minus lenses)

The big refactor plus the primary loop:

- **Extract `ReviewSession`** from `audit_mode.ts`: `source` (worktree /
  range / stash / patch), verbs, state; worktree/range/stash become
  parameterizations (they already share most code paths — this is honest
  refactoring behind the existing `review_diff_*` e2e suites).
- **Renderer upgrades** in the stream: enclosing-context hunk headers,
  copy-safe bodies (change identity in gutter/background only),
  interactive collapsed-context expanders, whitespace toggle,
  format-only detection (whitespace-insensitive diff == empty → dim + tag),
  generated/lock quarantine group (gitattributes + patterns).
- **Scope bar + chooser** (4 rows + revspec prompt) replacing the flat pile
  of palette commands; old command names stay as palette aliases.
- **The primary loop**: viewed marks keyed by `(path, old-blob, new-blob)`
  hashes; `Space` = mark & advance; mark-collapses-file; fold-without-mark
  independent and persisted; progress gauge excluding noise; `F`
  unreviewed filter; sticky header with `[✓ Reviewed]` (primitive #3, else
  header-in-place).
- **Verb buttons** on focused section + right-click context menus + hover
  checkboxes (primitive #2).
- **Comments v2**: severity tags, line/range anchors with ±context hashes,
  re-anchor + `≈` staleness, `⊕` hover gutter button, drafts persisted;
  notes rail projection.
- **State store v2**: `.review/<scope-id>.json` (scope identity hash;
  marks, folds, cursor, drafts, view options, `last_reviewed_head`);
  migration from the current `.review/` comment files.
- **Undo journal**: discarded/rejected hunks saved as patch files first;
  `Z` + receipt echo + `` ` `` command log. (Git-object ring in M3.)
- **Runnable `?`** (scoped palette).

Exit: worktree/range/stash review all run in the new workspace; marathon
persistence round-trips (close mid-review, reopen exact); staging parity
with today verified by the existing suites; chooser replaces old entries.

### M2 — lenses and history convergence (design §3.3 lens, history)

- **Commits lens**: rail swaps to the commit strip (`lib/git_history.ts`),
  selection drives the stream (debounced, cancellable — primitive #5),
  `[`/`]` stepping, per-commit + flat progress sharing the same
  content-hash marks.
- **History preset**: `git_log.ts` detail pane → shared renderer; `Enter`
  promotes commit/range to a full session; rail `⌕` query (`author:`
  `path:` text). Git Log (Current File) → history preset with a path
  filter.
- **Retire Review: PR Branch** (its two halves are now the commits lens and
  the history preset); palette alias points at the branch scope.
- **Version refs + interdiff**: record `refs/fresh/review/<scope-id>/v<N>`
  on review-open/conclude (primitive #6); "since my last review" as
  default reopened target; `⇅ vN→vM` pseudo-commit in the commits lens.

Exit: one commit-list implementation remains; git_log.ts shrinks to the
history preset shell; PR-Branch code deleted; force-push demo: review a
branch, force-push it, reopen → interdiff row.

### M3 — agent review, workspace-local (design §3.9; orchestrator side)

- **Orchestrator records the session-start ref** when creating/attaching a
  workspace; exposes it + review progress to the plugin API.
- **★ session scope** in the chooser inside agent workspaces
  (`session start ⟶ now`), keep/reject verbs (`git apply -R` + journal;
  file-level default, `k`/`x` on hunks/lines as deliberate acts).
- **Undo ring on git objects** (`refs/fresh/undo/*`) replacing patch files;
  survives restarts.
- **Live-update queue**: watch stays on; files outside the cursor refresh
  freely; the cursor's file queues behind a `↻N` badge; `N`/click applies;
  `NEW` tags; hash-invalidation of marks.
- **Reject guard**: v1 = same-file + cross-file identifier heuristic from
  the diff text (added/removed identifiers vs kept hunks' lines); upgrade
  to tree-sitter symbols where grammars exist. Non-blocking inline warning
  with "reject those too".
- **Dock integration**: cards show `n/m · k kept · x rejected`; waiting-
  on-you sort; card click → workspace → Review resumes.
- **Leftover-noise scan** (debug prints / TODO markers) as `⚠`/`!` display.
- **Narrative file order** in agent scopes (edit-sequence log from the
  orchestrator's session events; path order toggle).

Exit: end-to-end demo — run an agent in a workspace, watch the diff grow,
keep/reject with guard, conclude (reverts journaled + undoable), dock badge
lifecycle correct.

### M4 — the PR flow (design §3.3 PR)

- **Forge plugin API** (new plugin, GitHub first): list open PRs (+review
  state, checks), fetch `pull/N/head`, fetch review threads, submit review
  (verdict + summary + line comments). Auth via existing gh credentials /
  config; all calls surfaced in the command log.
- **PR picker** (review-requested first, `/` filter) from the `⇵` row or a
  typed `#N`/URL.
- **Read-without-checkout scope**: render from `refs/fresh/pr/N`; `Enter`
  opens read-only file-at-head buffers; depth boundaries offer checkout.
- **Checkout in a new workspace**: one orchestrator call (worktree at the
  PR branch, standard trust prompt), then open the review inside it;
  in-place open when the PR branch is already this workspace's checkout.
- **Thread import** as author-badged anchored comments; **`C` submit**
  (verdict / summary / drafts, severities rendered into bodies; Markdown
  export fallback).
- Review state keyed to PR identity (repo + number + head-sha versions).

Exit: select PR → read instantly; checkout on demand; submit a real review
to GitHub from the conclude surface; force-push shows `⇅` interdiff.

### M5 — buffer lens unification + web parity + scaling polish

- Absorb `git_gutter` into the live-diff hunk model (one reference picker;
  reference auto-swaps to session baseline when an agent review is open);
  expand-in-place hunk with the shared verb-button row (live_diff virtual
  lines + primitive #2); single Next/Prev Change everywhere.
- **Scene projections + parity tests** for every workspace surface (scope
  bar, rail, stream, buttons, transients, chooser, PR picker) — the web UI
  gets the whole experience with no web-specific code (design §3.12).
- Scaling: sub-100-col zoom model; ≥160-col split default + anchored
  comments rail.
- Blame alignment (`Enter` → one-commit scope; `,` re-blame at parent).

Exit: web e2e (Playwright) drives a full review mouse-only per design
§3.15; 80-col tmux run of the burn-down loop; blame pivots.

### M6 — Part II (design §4), gated on Part I being done

Comments-as-prompts ("send to session", session picker, export contract) →
review IPC + `.review/outbox|inbox` file transport → agent-seeded reviews /
pause-on-review / guided-review lens → deeper forge sync (thread replies,
resolution, viewed-state). Each independently shippable; none blocks or
reshapes Part I surfaces.

## 4. Cross-cutting workstreams

- **Keybindings**: one `review` context defined once, inherited everywhere;
  uppercase-escalation convention enforced in review-owned maps; all
  composite keys rebindable (M0).
- **Testing**: keep `review_diff_*` e2e green through the M1 extraction
  (they are the safety net); add scope-parameterized fixtures (worktree /
  range / agent / PR-readonly); parity test per new scene projection;
  perf assertions — first hunk painted < 100 ms on a 10k-line diff
  (streamed), selection-change render debounce 40 ms with cancellation
  (extend the planned git-show cancellation work).
- **i18n**: all new strings via the plugin `.i18n.json` pattern from day
  one (the existing plugins set the precedent).
- **Docs**: `docs/features/git.md` rewritten per milestone shipped; palette
  aliases listed; CHANGELOG entries per milestone.
- **Perf discipline**: viewport-rendered virtual buffers stay the base;
  per-hunk streaming; never a whole-diff overlay pass (the git-log
  million-overlay lesson).

## 5. Sequencing and risk

```
M0 ──► M1 ──► M2 ──► M3 ──► M4 ──► M6
              └────► M5 (can start after M1; finishes after M4)
```

- Biggest refactor risk: the M1 extraction from `audit_mode.ts` (6.6k
  lines) and the documented outer-vs-inner split-leaf bug class — mitigate
  by extracting behind the existing e2e suites and landing chrome changes
  separately from logic moves.
- Host-primitive risk: inline actionable spans (#2) is the one primitive
  the design leans on everywhere; prototype it first (M1 spike) — fallback
  is a persistent hint bar (design §3.14 explicitly keeps it as the
  fallback), which changes no data model.
- Forge risk (M4): API scope creep — the plugin API is four calls (list,
  fetch, threads, submit); everything else is Part II.
- The two flagged prototype questions (buttons vs hint bar; `h`/`l`
  ladder) get decided by instrumented dogfooding during M1/M2, not
  upfront.
