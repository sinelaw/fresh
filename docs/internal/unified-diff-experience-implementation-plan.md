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
| Review session (worktree/range/stash review, staging, comments, `.review/` persistence, watch mode, split/stack, sticky header panel) | `crates/fresh-editor/plugins/audit_mode.ts` | becomes the ReviewSession core; most Part I logic refactors out of here |
| Commit-list rendering | `plugins/lib/git_history.ts` | reused by the commits-lens rail |
| Git Log (buffer-group tab, streamed `git show` into a **file-backed `.diff` buffer** with real syntax highlighting, folds, scrollbar) | `plugins/git_log.ts` | the model for how the diff stream should be rendered (§2); its list becomes the history preset |
| Side-by-side composite buffer | host (`fresh-core` / render) | the split layout; keys must become rebindable Actions |
| Live Diff (in-buffer hunks, virtual old-lines, word-level intraline, reference picker) | `plugins/live_diff.ts` | the buffer lens; its virtual-line machinery powers expand-in-place |
| Git gutter | `plugins/git_gutter.ts` | absorbed into the live-diff hunk model (M5) |
| Diff nav (merged jump list) | `plugins/diff_nav.ts` | the single in-buffer Next/Prev Change |
| Orchestrator (worktree workspaces, dock cards, PR badges/metadata, trust flow, `windowEmbed` live preview) | `plugins/orchestrator.ts` | workspace creation for PR checkout, session-start refs, dock-as-inbox badges — and the working precedent for the workspace shell (§2) |
| **Widget framework** — 17 `WidgetSpec` kinds, host-owned instance state, keyboard *and* mouse dispatch, hit areas | host `src/widgets/*`, `src/app/widget_runtime.rs`, TS `plugins/lib/widgets.ts` | **the chrome of the entire Review workspace** (§2) |
| Buffer decoration APIs — `addOverlay` (fg/bg/underline/`extendToLineEnd`/`fgOnCollisionOnly`/OSC-8 `url`), virtual lines w/ priority, line indicators, `publishFoldingRanges` + collapsed folds, `setScrollbarMarkers[InRange]` | host + `fresh.d.ts` | diff body decoration, folds, the map strip |
| Piece-tree diff, review-hunk state, `set-review-diff-hunks`, local-control IPC | host | M6 agent surface; hunk model plumbing |
| Web scene projections + parity tests | host + `web-ui/` | free for widgets; every new surface lands as a projection (design §3.12) |

### 1.1 The widget toolkit, concretely

This was under-appreciated in the first draft of this plan and changes the
build substantially. What already ships:

- **Kinds**: `Row`, `Col`, `HintBar`, `Toggle`, `Number`, `Dropdown`,
  `DualList`, `Button`, `Spacer`, `Divider`, `List`, `Tree`, `Text`,
  `LabeledSection`, `WindowEmbed`, `Raw`, `Overlay` — with TS builders in
  `lib/widgets.ts` and `WidgetPanel` / `FloatingWidgetPanel` helpers.
- **Host-owned instance state keyed by widget key**: scroll offset, cursor,
  selection index, expanded tree keys and focus survive a full spec rebuild,
  so a plugin can re-emit its whole spec on every model change (exactly the
  render model a live-updating review needs).
- **Mouse is already solved for widgets**: the registry produces `HitArea`s
  (widget key, kind, row, byte range, payload, event type) and the runtime
  routes clicks — button activate, tree expand/collapse, list item select
  (item-granular even for multi-row cards via `itemSpecs`), dropdown
  open/select, text caret positioning. Right-click context menus and
  focus-follow are wired in `widget_runtime.rs`.
- **`Tree`** gives foldable typed nodes with host-retained expansion.
  **`List`** gives per-row `TextPropertyEntry`s *or* per-item widget cards.
  **`Overlay`** floats a child over the layout (dropdowns, popups).
  **`LabeledSection`** draws the rounded bordered box with a legend.
  **`HintBar`** renders keyed hint entries. **`Divider`**/`flexSpacer`
  size themselves to the panel — no width math in the plugin.
- **`WindowEmbed`** reserves a rectangle in a widget layout for the host to
  natively paint a real editor `Window` — syntax highlighting, folds,
  decorations, scrollbar and all. The Orchestrator already ships this for
  its live session preview.
- **`Raw`** is the escape hatch: pass-through `TextPropertyEntry` rows that
  emit **no hit areas**, leaving the plugin to do byte-offset math on
  `mouse_click` — which is what `audit_mode` does today, and what the
  git_log widget migration explicitly called out as *"the most error-prone
  part of the plugin"*.
- Direction of travel (`settings-widget-unification-plan.md`): the Settings
  UI has already been migrated onto this framework; the stated goal is one
  declarative widget framework for all controls. A new surface should not
  add a second hand-rolled one.

## 2. Architecture consequence: widgets for chrome, a real buffer for the diff

The first draft of this plan assumed the workspace would be built the way
`audit_mode` is built today — everything painted into virtual buffers via
`setPanelContent`, with the plugin hit-testing clicks by byte offset. Given
§1.1 that is the wrong build. The Review workspace is:

**Chrome = widgets.** Every element of the design maps onto an existing
kind, which means click targets, hover/focus behavior, host-retained state,
keyboard dispatch and **web parity all come for free**:

| Design element | Widget |
|---|---|
| scope bar (`⟨base ⟶ target⟩`, lens, gauge, `[/][o][?]`) | `Row` of `Button` + `Dropdown` + `Divider`/`flexSpacer` |
| scope chooser rows, PR picker | `List` with `itemSpecs` cards (item-granular clicks, `/` filter) |
| navigator rail — files / commits / notes | `Tree` (files, host-retained expansion) or `List`; the `⟨\ ▾⟩` cycle is a `Dropdown` |
| verb button rows (`[s Stage] [d Discard] …`) | `Row` of `Button` (label carries the key, per design §3.1 principle 3) |
| lens menu, rail dropdown, guard prompt | `Overlay` + `LabeledSection` |
| comment composer, conclude dialog | `LabeledSection` + `Text`/`textArea` + `Button` row |
| view transient | `LabeledSection` + `Toggle`/`Number`/`Dropdown` rows |
| context line / receipts | `HintBar` (+ a `Button` for `[Z Undo]`) |
| completion / resume banners | `LabeledSection` + `Button` row |

**The diff stream = a real editor buffer**, embedded in that widget layout
via `WindowEmbed` (or hosted as the buffer-group's center panel, whichever
the M1 spike shows is cleaner). Rendering the stream as a *buffer* rather
than as panel content is what unlocks, at zero cost: **syntax highlighting**
(git_log already proves the pattern — write the unified diff to a
`.diff`-suffixed file so the bundled grammar applies, then layer per-line
add/remove backgrounds with `addOverlay` + `fgOnCollisionOnly`), folding via
`publishFoldingRanges`, the scrollbar map via `setScrollbarMarkers`,
selection, in-buffer search, and the buffer lens itself.

This reframes the single biggest M0 item: *"syntax highlighting in the
unified review pane"* is not primarily a new host API — it is a **rendering
strategy change** (buffer instead of panel content) that the codebase has
already executed once, in git_log.

## 3. Host primitives — re-evaluated

The first draft listed six. After reading the toolkit, two are unnecessary,
two shrink to "finish what's declared", and one is a strategy change:

| # (old) | Proposed primitive | Verdict |
|---|---|---|
| 1 | Overlay priority / background layering | **Downgraded — strategy, not API.** Render the stream as a buffer (§2) and syntax highlighting is syntect's; `fgOnCollisionOnly` already exists for the row-bg-vs-token collision and `extendToLineEnd` for fills. Any residual composition bug is a fix in the existing overlay path, not a new primitive. |
| 2 | Inline actionable spans | **Keep, re-scoped and much smaller.** All *chrome* affordances are real widgets with hit areas already. This is needed only for **in-diff-body** affordances: the `⊕` hover-comment gutter button, context expanders, and any in-stream chips. The data model already declares it — `InlineOverlay.properties` is documented as *"click target metadata"* but **nothing consumes it**. Work = resolve a click/hover inside a buffer row to the innermost overlay carrying an action property and deliver it as a plugin event. Fallback if it slips: `mouse_click` byte math (today's audit_mode path) for those few targets only. |
| 3 | Sticky first row(s) | **Dropped — already exists.** `audit_mode` ships a `sticky` panel slot re-rendered on scroll; git_log ships a fixed toolbar band. Reuse the pattern. |
| 4 | Rebindable composite-buffer Actions (`diff-view` context) | **Keep.** The hardcoded input router is real and is the documented "v1 mistake". |
| 5 | Per-hunk incremental render + cancellation | **Keep, as an extension.** Streaming into file-backed buffers ships; growth polling and the planned kill-on-move cancellation are the remaining pieces. |
| 6 | Ref recording API for `refs/fresh/*` | **Dropped.** Plugins already spawn `git` directly for everything; version/undo refs are `git update-ref` calls. |

Net: **one genuinely new host capability** (overlay-property click/hover
resolution in buffer rows), one refactor (composite keys → Actions), one
perf extension (cancellation), and one strategy change (stream as buffer).
Everything else in Part I is plugin-side TypeScript over the existing
widget framework.

A corollary worth stating: because chrome is widgets and widgets already
project into the web scene with parity tests, the M5 "web parity" work
shrinks to whatever the `WindowEmbed`'d buffer needs — the chrome comes
across by construction.

## 4. Milestones

### M0 — stop the visible bleeding (design §5 Phase 0)

Small, independent, shippable one by one:

- Syntax highlighting + word-level intraline in the unified review pane —
  as the §2 strategy change (render the stream from a `.diff`-suffixed
  file-backed buffer like git_log already does, then layer add/remove
  backgrounds with `addOverlay` + `fgOnCollisionOnly`), not a new API.
  Doing this in M0 also de-risks M1, since it establishes the buffer the
  workspace will host.
- Composite-buffer keys → `diff-view` Actions (§3 #4); keybinding
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
  unreviewed filter; sticky header with `[✓ Reviewed]` (reusing
  `audit_mode`'s existing `sticky` panel slot).
- **Chrome on the widget framework** (§2): scope bar, chooser, rail, verb
  button rows, transients and banners as `Row`/`Button`/`List`/`Tree`/
  `Overlay`/`LabeledSection`/`HintBar` — deleting `audit_mode`'s
  byte-offset `mouse_click` hit-testing rather than extending it. Only
  in-diff-body affordances (`⊕` gutter, expanders) need the overlay-property
  click primitive; until it lands they can stay on the existing
  `mouse_click` path.
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
  selection drives the stream (debounced, cancellable — §3 #5),
  `[`/`]` stepping, per-commit + flat progress sharing the same
  content-hash marks.
- **History preset**: `git_log.ts` detail pane → shared renderer; `Enter`
  promotes commit/range to a full session; rail `⌕` query (`author:`
  `path:` text). Git Log (Current File) → history preset with a path
  filter.
- **Retire Review: PR Branch** (its two halves are now the commits lens and
  the history preset); palette alias points at the branch scope.
- **Version refs + interdiff**: record `refs/fresh/review/<scope-id>/v<N>`
  on review-open/conclude (plain `git update-ref`); "since my last review" as
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
  lines + the §3 #2 overlay-property click targets); single Next/Prev
  Change everywhere.
- **Scene projections + parity tests**: widget chrome projects already, so
  this shrinks to the stream surface (the embedded window) plus parity
  coverage for the new panels — the web UI gets the whole experience with
  no web-specific code (design §3.12).
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

## 5. Cross-cutting workstreams

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

## 6. Sequencing and risk

```
M0 ──► M1 ──► M2 ──► M3 ──► M4 ──► M6
              └────► M5 (can start after M1; finishes after M4)
```

- Biggest refactor risk: the M1 extraction from `audit_mode.ts` (6.6k
  lines) and the documented outer-vs-inner split-leaf bug class — mitigate
  by extracting behind the existing e2e suites and landing chrome changes
  separately from logic moves.
- Host-primitive risk is now small: the only new capability is
  overlay-property click/hover resolution (§3 #2), and it is confined to
  in-diff-body affordances — chrome rides the widget framework's existing
  hit areas. Fallback for those few targets is today's `mouse_click` byte
  math; the design's hint-bar alternative (§3.14) remains available and
  changes no data model.
- Integration risk moves to the §2 spike: does the stream live in a
  `WindowEmbed`'d window inside one widget panel, or as a buffer-group
  center panel with widget panels around it? Decide in an M1 spike before
  the chrome work; both keep the same data model, so the risk is layout
  plumbing, not rework.
- Forge risk (M4): API scope creep — the plugin API is four calls (list,
  fetch, threads, submit); everything else is Part II.
- The two flagged prototype questions (buttons vs hint bar; `h`/`l`
  ladder) get decided by instrumented dogfooding during M1/M2, not
  upfront.
