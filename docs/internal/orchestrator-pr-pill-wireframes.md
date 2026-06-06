# Orchestrator PR-aware "pill" rows — wireframe alternatives

Goal: make the orchestrator **Open** picker and the **Dock** panel show, per
session, an opportunistically-gathered GitHub PR summary (PR number, CI checks,
review/merge status, open comments) and present each session as a richer
multi-line "pill" — line 1 name + project, line 2 the PR badge.

This doc records (1) the reproduced current UI, (2) the one hard renderer
constraint that shapes the options, (3) three wireframe alternatives with a
recommendation, and (4) the data-plumbing sketch for the `gh` integration.

---

## 1. Current UI (reproduced in tmux, `capture-pane -e`)

Setup: a demo git repo `/tmp/demo-proj` with 4 live orchestrator sessions
(`demo-proj`, `agent-auth`, `agent-refactor`, `docs-rewrite`) plus 3 discovered
on-disk worktrees (`feature-login`, `docs-refresh`, `bugfix-crash-1991`).
Colored captures live in `docs/internal/_captures/` (`*.ansi`).

### Open modal (today)

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ORCHESTRATOR :: Sessions  —  all projects                                       │
│                                                                                │
│╭─ Sessions ─────────────────────────────────╮╭─ docs-rewrite ────────────────╮│
││ [ + New  Alt+N ]                           ││ [ Visit ]   [ Details ] [ Stop ]││
││ Project: [ All ▾   (Alt+P) ]               ││            [ Archive ] [ Delete]││
││ [v] Show all worktrees   (Alt+T)           ││                               ││
││ [ ] Show empty/1-file sessions   (Alt+I)   ││  *Terminal 0* ×               ││
││ Filter [type to search… ( / )            ] ││ root@vm:…/docs-rewrite#       ││
││                                            ││                               ││
││       NAME                                 ││                               ││
││ ✓ [ ] demo-proj                            ││                               ││
││ ✓ [ ] agent-auth                           ││         (live terminal embed) ││
││ ✓ [ ] agent-refactor                       ││                               ││
││ * [ ] docs-rewrite                         ││                               ││
││   [ ] feature-login · on-disk              ││                               ││
││   [ ] docs-refresh · on-disk               ││                               ││
││   [ ] bugfix-crash-1991 · on-disk          ││                               ││
│╰────────────────────────────────────────────╯╰───────────────────────────────╯│
│              ↑↓ nav  Enter dive  Space select  Alt+P current only  Tab focus … │
└────────────────────────────────────────────────────────────────────────────────┘
```

### Dock (today)

```
ORCHESTRATOR
[ + New Alt+N ]      [ all ▾ ]
[v] all worktrees
[ ] show empty
Filter [/ to search           ]
──────────────────────────────
✓ [ ] demo-proj
✓ [ ] agent-auth
✓ [ ] agent-refactor
* [ ] docs-rewrite
  [ ] feature-login · on-disk
  [ ] docs-refresh · on-disk
  [ ] bugfix-crash-1991 · on-disk

▸ (detached)
[ Stop ] [ Arch ] [ Del ]
 ↑↓ switch  Enter edit  Esc editor
```

Each row today is `<sym> [ ] <name>  <· project / · on-disk>` and is rendered by
`renderListItem()` (`orchestrator.ts:880`). `<sym>` is `*` (working, amber) / `✓`
(idle, green) / blank (on-disk).

---

## 2. The renderer constraint that shapes everything

The host `list` widget renders **exactly one terminal row per item**
(`crates/fresh-editor/src/widgets/render.rs:946-998`):

- one `TextPropertyEntry` → one `ensure_trailing_newline()` → one line;
- selection highlight is a single-row `extend_to_line_end` background;
- one `HitArea` per item at one `buffer_row`;
- scroll/`visibleRows` math counts **items == rows**.

So a true two-line pill is **not free**. That gives us the three options below,
from cheapest to most principled.

---

## 3. Wireframe alternatives

PR badge vocabulary (single-cell glyphs only, matching the existing
`* ✓ ▸ · ▾ ⚠` palette — no emoji, which render unevenly):

| token            | meaning                                  |
|------------------|------------------------------------------|
| `#1287`          | PR number                                |
| `✓7/8`           | checks passed / total (green)            |
| `✗1`             | checks failing (red)                     |
| `•2`             | checks pending/running                   |
| `●2`             | unresolved review comments               |
| `approved`       | reviewDecision = APPROVED                |
| `chg-req`        | CHANGES_REQUESTED                        |
| `review?`        | REVIEW_REQUIRED / none yet               |
| `draft`          | draft PR                                 |
| `merge ok` / `✗ conflicts` | mergeable state                |
| `↑3 ↓1`          | commits ahead/behind base (fallback when no PR) |
| `no PR`          | branch has no open PR                     |
| `…` (dim)        | PR info still loading / `gh` unavailable  |

### Option A — Enriched single line (zero host change)

Append a right-aligned PR badge to the existing one-line row. Project tag drops
first, then the badge truncates, when width is tight.

Dock (~38 cols):
```
──────────────────────────────────────
✓ agent-auth          #1287 ✓7 ●2 ✔
* docs-rewrite        #1290 ✗1 draft
✓ agent-refactor      no PR
  feature-login · on-disk
```

Modal list pane (~58 cols):
```
       NAME                          PR
✓ [ ] agent-auth        #1287  ✓7/8  ●2  approved
* [ ] docs-rewrite      #1290  ✗1/8       draft
✓ [ ] agent-refactor    no PR
  [ ] feature-login · on-disk
```

- ➕ No Rust change; ships entirely in `orchestrator.ts`.
- ➕ List nav / selection / scroll unchanged.
- ➖ Cramped in the narrow dock; name and badge fight for the same row.
- ➖ Can't show name + project + full PR status together.

### Option B — Two-line pill (host multi-row list items) — **RECOMMENDED**

Teach the `list` widget that an item may occupy N rows (e.g. an
`item_rows: u32` parallel to `item_keys`, or detect `\n` in the entry text),
then: selection bg spans all rows of the item, the hit area covers them, and
`visibleRows`/scroll count **pills** not rows.

Dock (~38 cols, 2 rows/pill, selected pill inverted across both lines):
```
──────────────────────────────────────
 * agent-auth                 demo-proj
   #1287 ✓7/8 ●2 approved · merge ok
 ✓ docs-rewrite               demo-proj
   #1290 ✗1/8 draft · ✗ conflicts
 ✓ agent-refactor             demo-proj
   no PR · ↑3 ahead
   feature-login              on-disk
   (worktree — not opened)
```

Modal list pane (~58 cols, line 2 carries the full badge):
```
 * [ ] agent-auth                 demo-proj · feature/auth
       PR #1287 · ✓ 7/8 · ●2 · approved · merge ok
 ✓ [ ] docs-rewrite               demo-proj · docs/rewrite
       PR #1290 · ✗ 1/8 · draft · ✗ conflicts
 ✓ [ ] agent-refactor             demo-proj · refactor
       no open PR · ↑3 ahead
   [ ] feature-login              on-disk
       (discovered worktree)
```

- ➕ Best fit for the ask: name+project on top, full PR badge below, all legible.
- ➕ Same model serves dock and modal; the second line is where PR detail lives.
- ➖ Requires a host renderer change (selection span, hit area, scroll math).
  Touch points: `widgets/render.rs` List arm, `WidgetInstanceState::List` scroll
  clamp, `ScrollRegion` emit, and the `list()` helper in `lib/widgets.ts`.

### Option C — Two-line pill via paired entries (no host change, has caveats)

Emit two list items per session (primary + detail), step Up/Down by two, and map
both rows' `select`/click back to the one session id.

```
 * agent-auth                  demo-proj      ← selectable (session id)
   #1287 ✗1/8 draft               (dim)       ← detail row, nav-skipped
```

- ➕ Gets the two-line look today, purely in the plugin.
- ➖ Selection highlight only paints one of the two lines (host paints per-row).
- ➖ `visibleRows`, the live-switch debounce, and the existing click/`select`
  handlers all need "skip/fold the detail row" logic — fiddly and easy to
  regress the careful list-height math.

**Recommendation:** ship **Option A** first (immediate value, no host risk),
then land the **Option B** host change as the real home for the two-line pill.
Skip C — it spends nearly as much plugin complexity as A for a worse result than B.

---

## 4. Opportunistic `gh` plumbing (applies to A, B, or C)

The pill must never block a render and must degrade silently.

- **Where:** add a per-session async fetch keyed by `(projectPath, branch)`,
  reusing `editor.spawnProcess("gh", […], cwd)` exactly like the existing git
  helpers (`spawnCollect`, `orchestrator.ts:3319`). Branch is already resolved
  best-effort (`AgentSession.branch`, populated from
  `git worktree list --porcelain`).
- **Command:** `gh pr view --json number,state,isDraft,reviewDecision,`
  `mergeable,statusCheckRollup,comments` (run in the session's worktree so `gh`
  picks the right branch/PR). Fall back to `gh pr status` / commits-ahead when
  there's no PR for the branch.
- **Caching & freshness:** store a `PrInfo | "loading" | "none"` on the session;
  populate lazily when a session first becomes visible in either panel; refresh
  on panel focus and on a slow timer (e.g. 60–90s), debounced so holding ↑/↓
  doesn't fan out a `gh` call per row.
- **Graceful absence:** `gh` missing / not a GitHub remote / not authed / no PR
  → render the dim `…`→`no PR` state, never an error. (In this very sandbox `gh`
  is not installed, so this path matters.)
- **Render:** `renderListItem()` gains the badge (A) or returns a 2-row item (B).

Captures that back this doc: `docs/internal/_captures/open-modal.ansi`,
`docs/internal/_captures/dock.ansi` (view with `cat`, they include color).

---

## 5. What shipped — bordered two-line pill (Option B, refined)

We went straight to the richest layout: a **rounded-border card per session**,
two content lines, in both the dock and the modal.

```
╭────────────────────────────────────╮
│ ✓ [ ] agent-auth          demo-proj │   line 1: status · [select] · name · · project
│ #1287 ✓7/8 ●2 approved · merge ok   │   line 2: PR badge (falls back to ▸ branch)
╰────────────────────────────────────╯
```

### Host change: `List { item_specs }`

The classic `list` widget rendered one `TextPropertyEntry` per row, which can't
express a bordered multi-row card. Rather than special-case a 2-row item, we
added a general primitive: **a list item can be an arbitrary `WidgetSpec`**.

- `WidgetSpec::List` gains `item_specs: Vec<WidgetSpec>` (`fresh-core/api.rs`).
  When non-empty it overrides `items`: each spec renders via the normal
  recursive widget renderer into a fixed-height block, and the list does
  selection, scroll, `visible_rows`, and click-routing in **item units** — one
  card per item, regardless of how many rows it draws.
- Renderer (`widgets/render.rs`, List arm): pre-renders each item spec to learn
  the uniform card height, computes the visible-item window from
  `visible_rows / item_height`, paints the selection highlight band across all
  rows of the selected card, and emits a `select` hit on every row so a click
  anywhere on a card selects it. Shorter cards pad to the tallest.
- `list()` helper (`plugins/lib/widgets.ts`) gains `itemSpecs`; the pill is a
  `labeledSection` (empty label ⇒ plain rounded box) wrapping a 2-row `raw`
  widget. The rounded corners come for free from the existing section-border
  renderer. Covered by `list_item_specs_render_multirow_cards_in_item_units`.
- Limitation (documented): interactive widgets *nested inside* a card aren't
  routed yet — the whole card is a single `select` target. That's all the pill
  needs; richer per-card controls can come later.

### PR plumbing — implemented as sketched in §4

`probePr()` runs `gh pr view <branch> --json …` per visible session via the
existing `spawnProcess`, throttled by an in-flight set + a 90 s TTL, kicked from
`refreshOpenDialog`. Results cache on `AgentSession.pr` (`loading|none|ok`).
Missing `gh` / no PR / not-a-GitHub-remote all degrade to the dim
`▸ <branch>` fallback (verified in this sandbox, where `gh` is absent).

`renderListItem` (the old single-line row) was removed; `renderPillSpec` +
`prBadgeEntries` replace it.
