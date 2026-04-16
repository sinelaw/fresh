# Review Picker — Plan

## Goal

Replace the two existing entry points to the review-diff feature
(`start_review_diff` and `start_review_range`) with a single command
**Review** that opens a dedicated **picker** screen. The picker covers
the four highest-leverage UX gaps in the current review-diff:

1. **No "type a revspec" tax for the common case** — auto-detected
   "This PR" preset is preselected; Enter opens the review immediately.
2. **No blind commits** — the picker has a live preview pane that
   re-renders the diff as the user moves through the list.
3. **"Since I last reviewed (N new)"** — uses watermarks persisted on
   review close; turns the existing per-range comment-persistence layer
   into a daily-visible, repeat-review-friendly feature.
4. **Comment-count badges** — saved comments become discoverable from
   the picker, not only after opening a range.

The existing review buffer group (toolbar + sticky header + diff +
comments) is unchanged except for one new 1-row **context ribbon**
between the toolbar and the sticky header that names what is being
reviewed and exposes a `g` keybind to re-open the picker.

## Non-Goals

- **No new keymaps inside the diff/comments panels.** All review-mode
  bindings stay as they are. The single addition is `g` → open picker.
- **No editor-core changes.** The picker is a buffer group built from
  the existing `createBufferGroup` primitive, the same way
  `start_review_branch` already is.
- **No new persistence schema.** The watermark file and the comment
  count come from the existing `<data_dir>/audit/<repo>/` directory.

## Two-screen model

The picker is **not** a left panel attached to the review. It is a
separate buffer group, opened in place of the review (or in place of
the editor when first launched). Same takeover pattern as the command
palette and `start_review_branch`.

```
┌────────────┐   Enter         ┌────────────┐
│   PICKER   │ ──────────────▶ │   REVIEW   │
│            │                 │            │
│            │ ◀────────────── │            │
└────────────┘   press  g       └────────────┘
        │                            │
        │ press q                    │ press q
        ▼                            ▼
    back to editor               back to editor
```

Why two screens (rather than a third permanent pane in the review):

- **Space**: the review already runs diff + comments side-by-side. A
  third permanent pane would starve the diff on terminals narrower
  than ~150 columns.
- **Focus clarity**: the picker is a *deciding* task; the review is a
  *reading* task. `j/k` means different things in each. Mixing the
  two in one layout costs a "which pane am I in?" check on every
  keystroke.
- **Consistency**: command palette, file picker, and the existing
  branch-review screen are all takeover screens. Users already know
  the pattern.
- **No wasted pixels after decision**: once a range is picked, the
  picker disappears and the review gets 100% of the area.

## Picker layout

```
split v 0.02
  fixed   header   h=1     "Pick what to review   Enter: open · Tab: pane · q: cancel"
  split h 0.4
    scrollable list        (presets, commits, branches, custom)
    scrollable preview     (live diff of the row under the cursor)
```

List pane content:

```
 ★ This PR  (main..HEAD)             7 commits  +52/−12   ●3
   Since I last reviewed                       3 new   ●1
   Working tree                                16 files ●2
   Last commit  (03637f8 feat(util))
 ─── COMMITS ─────────────────────────────────────────────
 ○ bca083a  feat: farewell                         ●0
 ● 9e478d5  feat: f-strings                        ●1
 ○ 03637f8  feat(util): sub/mul                    ●0
 ─── BRANCHES ────────────────────────────────────────────
 ✓ main                                            ●0
   origin/main                                     ●0
   release/v2                                      ●4
 ─── CUSTOM ──────────────────────────────────────────────
 :  type a revspec…
```

`●N` is the count of saved comments under the resulting range key.
`●0` is rendered dim. `★` marks the auto-detected default. `✓` marks
the current branch.

Layout flips to vertical (list on top, preview below) when
`viewport.width < 100`.

## Picker behaviour

### Smart default — `★ This PR`

On open, the cursor lands on `★ This PR`. The "from" is resolved in
this order, falling back through to the last entry that succeeds:

1. `git rev-parse --abbrev-ref @{u}` — upstream of current branch
2. `git merge-base HEAD <default-branch>` — `main`, then `master`,
   then `trunk`
3. `HEAD~1` — last commit only

The `<default-branch>` is whatever `git symbolic-ref refs/remotes/origin/HEAD`
points at, with `main`/`master`/`trunk` fallbacks if the remote head
isn't set.

If the resolved range is empty (you are sitting on the default branch
with no upstream divergence), the row is shown disabled and the
cursor falls through to `Working tree`.

### Live preview

Every cursor move in the list pane debounces a `git diff <from>..<to>`
(100 ms) and re-renders the right pane using the same
`buildListLines` / `parseDiffOutput` pipeline the real review uses.
Per-range cache; cleared when the picker closes. Result: scrolling
through commits feels instant on revisits, and what the user sees in
the preview is byte-identical to what they get on Enter.

### Since I last reviewed

On review close (`q` or `stop_review_diff`), write
`<data_dir>/audit/<repo>/watermarks.json`:

```json
{
  "branches": {
    "feature/x": { "tip": "abc123", "updated_at": "2026-04-16T…" }
  }
}
```

When the picker opens, if the current branch has a watermark **and**
the watermark differs from `HEAD`, render a `Since I last reviewed
(N new)` row resolving to `<watermark>..HEAD`. If the watermark
equals `HEAD`, hide the row (nothing new to review).

This is the unique-value-prop feature. Most reviewers come back to a
PR after the author pushes follow-up commits; today they have to find
the old SHA themselves.

### Comment badges

On picker open, list `<data_dir>/audit/<repo>/*.json` once, parse the
`comments.length` from each, and key the counts by review key
(`worktree`, `range-<from>__<to>`). Render `●N` next to any list row
whose resulting range key has a non-zero count. O(files), tens of ms
even with hundreds of saved reviews.

### Keys (picker mode)

```
j / k / Up / Down       move list cursor
Enter                   open the row's range as a review
Tab                     toggle focus between list and preview pane
PageDown / PageUp       scroll preview pane (when focused)
v                       (commits section) toggle "marked"
V                       (commits section) extend mark range
:                       focus the custom-revspec field
q  /  Esc               cancel; close picker, return to where you came from
g                       refresh the picker (re-scan branches/commits/badges)
```

Multi-commit non-contiguous selection (`v` to mark commits, Enter to
flatten the marks into a synthesised range) is in scope for the
picker but can ship in a follow-up — the rest of the picker
delivers >80% of the value without it.

## Review screen — the one new row

`REVIEW_LAYOUT` adds one fixed-height node:

```
split v 0.02
  fixed toolbar  h=2
  split v 0.02
    fixed ribbon h=1               ← NEW
    split v 0.02
      fixed sticky h=1
      split h 0.75
        scrollable diff
        scrollable comments
```

Ribbon content (mode-aware):

| Mode      | Ribbon text                                                       |
|-----------|--------------------------------------------------------------------|
| worktree  | `Working tree · 16 files · +40/−77 · 0 comments    [g] change range` |
| range     | `★ main..HEAD · 2 files · +10/−1 · 0 comments      [g] change range` |
| commit    | `bca083a feat: farewell · 1 file · +3/−0 · 0 comments [g] change range` |

Always visible. The "what am I reviewing?" question never requires a
keystroke to answer.

`g` (mnemonic: **g**o to picker) closes the review group and opens
the picker. Initial picker selection is the row corresponding to the
range you just left, so `g`-then-Enter is a no-op refresh.

## Code surface

| Concern                          | Where it lives                            | New / reused |
|----------------------------------|-------------------------------------------|--------------|
| Picker buffer group + layout     | new `audit_picker.ts` (sibling to `audit_mode.ts`) | **new**      |
| List rendering (presets/commits/branches/custom) | `audit_picker.ts`                  | **new**      |
| Live preview rendering           | `audit_picker.ts`, calls existing `parseDiffOutput` + `buildListLines` | **new** wrapper, **reuses** existing |
| Per-range diff cache             | `audit_picker.ts`                          | **new**      |
| `★ This PR` resolution           | `audit_picker.ts` helper                   | **new**      |
| Comment-count scan               | `audit_picker.ts` helper, reads `getDataDir() / audit / <repo> / *.json` | **new** (tiny) |
| Watermarks read / write          | `audit_picker.ts` (read), `audit_mode.ts` `stop_review_diff` (write) | **new** (tiny) |
| Ribbon row                       | `audit_mode.ts`: extend `REVIEW_LAYOUT`, add `buildRibbonEntries()` | **modified** |
| `[g] change range`               | `audit_mode.ts`: add to `review-mode` keymap; new handler `review_open_picker` | **modified** (~3 lines) |
| Open review with picked range    | reuses `bootstrapRangeReview` (`audit_mode.ts:3886`) and the worktree path of `start_review_diff` | **reused** |

## Lifecycle

1. User runs **Review** (single command; replaces both `Review Diff`
   and `Review Range (Commit or Branch)`).
2. Picker buffer group opens. Default-detection runs; cursor lands on
   the auto-selected row. Comment-count scan runs. Branches and
   recent commits enumerate. Preview pane shows the default's diff.
3. User browses with `j`/`k`. Preview pane debounce-updates.
4. Enter on a row → close picker group → open review group with that
   range. Saved comments load from `<data_dir>/audit/<repo>/<key>.json`
   exactly as they do today.
5. Inside review: ribbon reflects the slice. Reviewer reads,
   comments, navigates as today.
6. `g` from review → close review group → open picker, with the
   current range pre-selected. Comments are persisted continuously
   already, so nothing is lost.
7. `q` from review → close review; write `watermarks.json` for the
   branch's current `HEAD`.
8. `q` from picker → close picker; return to the editor (no review
   was opened).

## What goes away

- **`start_review_range`** and its single-prompt UI (the picker
  replaces it). The `cmd.review_range` i18n keys also drop.
- **The "type a revspec" friction** for users who want anything other
  than HEAD. Power users still have `:` inside the picker.
- **The "I have to open it to know if I have comments"** dance — the
  comment-count badges expose this in the picker.
- **The "what am I reviewing again?" check** — the ribbon names it.

## Out of scope (good follow-ups, not blockers)

- **Rebase-aware comment matching**: today comments roll forward only
  when the underlying lines still exist; a fingerprint match on
  `(file, surrounding-3-line-hash)` would survive minor rewrites.
  Independently useful; not required for the picker.
- **Resolved / unresolved comment state**: a third state beyond
  exists/deleted. Belongs in the comments panel, not the picker.
- **Per-line `git blame` in the diff**: useful in multi-author
  branches; orthogonal.
- **Mouse support in the picker** (click row to preview, double-click
  to open). Easy to add later.

## Risks and open questions

- **Preview fetch cost**: `git diff main..HEAD` on a large monorepo
  can take seconds. Mitigations: cache per range, render the preview
  pane with a "Loading…" placeholder, and cancel any in-flight fetch
  when the cursor moves again.
- **Watermark vs. HEAD on a freshly-checked-out branch**: there is no
  watermark yet. Hide the "Since I last reviewed" row in that case
  rather than show a confusing "0 new".
- **Default detection on detached HEAD**: no upstream, no branch.
  Fall through to merge-base with default branch; if that also
  fails, the default becomes "Working tree" rather than a broken
  range.
- **Picker on a non-git directory**: the picker should refuse to
  open with a single-line "Not a git repo" message, the same way the
  current review-diff already handles `emptyState === 'not_git'`.

## Phasing (purely sequencing — none of these are independent ship
points; the user-facing change is all-or-nothing once `Review` exists)

1. Ribbon row in the existing review (no behaviour change, just
   surface what the review already knows).
2. Picker buffer group with **presets only** (`★ This PR`, `Working
   tree`, `Last commit`, `:custom`). Live preview wired in. Replaces
   `start_review_range`.
3. Comments-count badges on preset rows.
4. Watermark write on close + `Since I last reviewed` preset.
5. Commit list section + per-row badges.
6. Branch list section.
7. Multi-commit `v`/`V` marking (follow-up).

Phases 1–4 are the smallest set that delivers the four headline UX
gains. Phases 5–6 round out coverage; phase 7 unlocks the long-tail
"these specific commits" use case.
