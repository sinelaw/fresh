# Live Diff Plugin Plan

**Status**: Planning
**Last Updated**: 2026-04-28
**Branch**: `claude/live-diff-plugin-AJv7o`

## Goal

A "live diff" plugin that turns the **live editable buffer** into a unified
diff view: gutter `-`/`+` indicators next to changed lines and virtual
lines of the old content rendered directly above the new lines, all while
the file remains fully editable. The diff updates continuously as the
buffer changes — including changes driven by an external process (a coding
agent modifying the file on disk and the editor reloading those changes
into the buffer).

The reference (left side of the diff) is configurable per buffer via a
command-palette command:

| Mode | Reference content |
|------|-------------------|
| `head` (default) | The file's contents at `HEAD` in the surrounding git repo |
| `disk` | The file's contents on disk (so unsaved buffer edits show as a diff) |
| `branch` | The file's contents on a user-chosen branch (typically `main`/`master`) |

## Why this is different from existing plugins

Fresh already ships `git_gutter.ts` and a side-by-side `diff-view`. Neither
addresses the agent-watching use case:

- `git_gutter.ts` — only gutter symbols. It runs `git diff HEAD` on the
  on-disk file (not the buffer), so the user can't see *what* the old
  text was without leaving the file. It doesn't render virtual lines.
- Side-by-side diff (review mode, composite buffers) — splits the screen
  into two read-only panes. Useful for code review, useless for
  observing live edits in your normal edit flow.

The live-diff plugin is a unified-diff *overlay* on the **real, editable
buffer**: the user keeps editing as normal, but they can see exactly
which lines the agent (or the user) changed and what was there before.

## Plugin API surface used

All identifiers below are from
`crates/fresh-editor/plugins/lib/fresh.d.ts`. The plugin is pure
TypeScript and ships alongside the existing plugins in
`crates/fresh-editor/plugins/`.

### Display primitives

- `addVirtualLine(bufferId, position, text, options, above, namespace, priority)`
  — renders a non-editable line above (or below) a real buffer line.
  Used to draw the **old content** above each changed line. `options`
  takes `{fg, bg}` as either RGB tuples or theme keys, so the rendering
  follows the active theme. (`fresh.d.ts:1664`, used by `git_blame.ts`
  and `markdown_compose.ts`.)
- `setLineIndicator(bufferId, line, namespace, symbol, r, g, b, priority)`
  / `setLineIndicators(...)` / `clearLineIndicators(bufferId, namespace)`
  — gutter glyphs. Used to draw the `+` / `-` / `~` markers.
  (`fresh.d.ts:1781`, used by `git_gutter.ts`.)
- `addOverlay(bufferId, namespace, start, end, options)` — text-range
  styling. We use it with `extend_to_line_end: true` to paint the
  background of added/changed lines, mirroring a unified diff's green
  highlight. Theme-key colors keep the highlight readable across themes.
  (`fresh.d.ts:1548`.)
- `clearNamespace(bufferId, namespace)` and
  `clearVirtualTextNamespace(bufferId, namespace)` — bulk-removal
  primitives we use on every redraw.

### Buffer / file access

- `getActiveBufferId()`, `getBufferPath(bufferId)`,
  `getBufferLength(bufferId)`, `getBufferText(bufferId, start, end)`
  — read the buffer's *current* text (the right side of the diff).
- `getLineStartPosition(line)`, `getLineEndPosition(line)`,
  `getBufferLineCount()` — convert between line numbers and byte
  offsets, which is the unit `addVirtualLine` and `setLineIndicator`
  expect.
- `readFile(path)` — synchronous read of the on-disk file (used for
  `disk` mode and as a quick path for `head`/`branch` when content is
  cached).
- `getBufferSavedDiff(bufferId)` — already exposes "buffer vs disk"
  byte-ranges; useful only as a coarse change signal in `disk` mode,
  not as the diff itself (it doesn't hand back the old text).

### Process spawning (git)

- `spawnProcess(command, args, cwd)` — used to run `git show
  <ref>:<path>`, `git rev-parse --abbrev-ref HEAD`, `git ls-files`, etc.
  Same pattern as `git_gutter.ts`, `git_blame.ts`.

### Events

- `after_file_open` — initialize the per-buffer state and compute the
  first diff.
- `after_insert` / `after_delete` — recompute the diff after any
  buffer mutation. **This is the live-update hook**: when an external
  process (e.g. an agent) writes to disk and Fresh reloads the buffer,
  Fresh emits `after_insert` / `after_delete` for the resulting
  in-buffer changes, and the plugin redraws.
- `after_file_save` — only relevant in `disk` mode (saving makes the
  unsaved-changes diff empty).
- `buffer_activated`, `buffer_closed` — lifecycle bookkeeping.

### Commands and state

- `registerCommand(name, description, handlerName, context?)` — for the
  command-palette entries that switch modes.
- `prompt(label, initialValue)` — interactive input for the branch
  name in `branch` mode.
- `setViewState(bufferId, key, value)` /
  `getViewState(bufferId, key)` — per-buffer mode persistence
  (each buffer remembers its own diff source).
- `setGlobalState(key, value)` / `getGlobalState(key)` — store the
  default mode/branch across sessions.

## Architecture

### Per-buffer state

```ts
type DiffMode =
  | { kind: "head" }
  | { kind: "disk" }
  | { kind: "branch"; ref: string };

interface BufferDiffState {
  bufferId: number;
  filePath: string;
  mode: DiffMode;
  /** Reference (left-side) text. Cached; refetched on save / mode change. */
  oldText: string | null;
  /** Hash/length of `oldText` for cheap change detection. */
  oldRev: string;
  /** Hunks computed from `oldText` vs current buffer. */
  hunks: Hunk[];
  /** True while a recompute is in flight. */
  updating: boolean;
  /** Coalesce burst edits (agent paste, undo) into one redraw. */
  pendingTimer: number | null;
}

const states: Map<number, BufferDiffState> = new Map();
```

### Hunk model

```ts
interface Hunk {
  /** First changed new-file line (0-indexed). */
  newStart: number;
  /** Number of lines on the new side (0 = pure deletion). */
  newCount: number;
  /** Old-side text, line by line, no trailing newlines (length 0 = pure insertion). */
  oldLines: string[];
  /** "added" | "removed" | "modified" — drives the gutter glyph and bg color. */
  kind: HunkKind;
}
```

A hunk represents one contiguous change. The diff algorithm produces:

- **modified**: `oldLines.length > 0 && newCount > 0` — old lines render
  as virtual lines above; new lines get `+`-gutter and added-bg overlay.
- **added**: `oldLines.length === 0 && newCount > 0` — new lines get
  `+`-gutter and added-bg.
- **removed**: `oldLines.length > 0 && newCount === 0` — virtual lines
  appear above the line that *now* sits where the deletion happened,
  and the gutter glyph on that anchor line is `-` (or `▾`).

### Pipeline (one buffer, one tick)

```
   external write           buffer edit (typing)
         │                          │
         ▼                          ▼
   editor reloads file       after_insert / after_delete
   on disk into buffer       fires for each edit
         │                          │
         └──────────────┬───────────┘
                        ▼
              schedule debounced recompute (50–150 ms)
                        │
                        ▼
        ┌──────────── recompute(bufferId) ──────────────┐
        │ 1. ensure oldText is loaded for current mode  │
        │ 2. read newText = getBufferText(0, length)    │
        │ 3. lineDiff(oldText, newText) → Hunk[]        │
        │ 4. clearNamespace + clearVirtualTextNamespace │
        │    + clearLineIndicators (LIVE_DIFF_NS)       │
        │ 5. for each hunk: addVirtualLine(s) for old,  │
        │    setLineIndicators for gutter,              │
        │    addOverlay extend_to_line_end for bg       │
        │ 6. publish hunks via setViewState so          │
        │    diff_nav.ts can navigate them              │
        └────────────────────────────────────────────────┘
```

### Reference loading per mode

| Mode | How `oldText` is fetched | Refresh trigger |
|------|--------------------------|-----------------|
| `head` | `git show HEAD:<repo-relative-path>` via `spawnProcess` | once per buffer; refetch on `git_head_changed` heuristic (TODO) and on manual refresh command |
| `disk` | `readFile(filePath)` | initial load + `after_file_save` (save resets the diff) |
| `branch` | `git show <ref>:<repo-relative-path>` | once per buffer + manual refresh command |

`oldText` is cached in `BufferDiffState` so each `after_insert` /
`after_delete` only re-runs the in-memory line diff — not git.

### Diff algorithm

A small line-level Myers / patience diff in pure TS. Inputs are the two
texts split on `\n`. Output is the `Hunk[]` shape above. Implementation
notes:

- Operate on lines, not bytes — virtual lines and gutter glyphs are
  per-line concepts, and the user-facing UX matches a unified diff.
- Fold `del+add` runs that share a line index into a single
  `modified` hunk so the old line renders directly above the new one.
- Cap diff size: if either side exceeds, e.g., 50 000 lines or the
  hunk count exceeds a threshold, fall back to gutter-only rendering
  (skip the virtual lines) and surface a status message.

## Rendering details

All decorations live under one namespace so they're trivial to clear
and re-apply on every recompute:

```ts
const LIVE_DIFF_NS = "live-diff";
const LIVE_DIFF_VLINE_NS = "live-diff-vlines";
const PRIORITY = 9; // just below git_gutter (10) so live-diff wins when both are present
```

### Old-line rendering (virtual lines)

For each `oldLines` entry in a hunk:

```ts
const anchor = await editor.getLineStartPosition(hunk.newStart);
if (anchor === null) continue;
for (let i = 0; i < hunk.oldLines.length; i++) {
  editor.addVirtualLine(
    bufferId,
    anchor,
    "- " + hunk.oldLines[i],            // unified-diff prefix
    {
      fg: "diff.removed_fg",            // theme key with sane fallback
      bg: "diff.removed_bg",
    },
    /* above */ true,
    LIVE_DIFF_VLINE_NS,
    /* priority */ i,                   // preserves order: oldest line on top
  );
}
```

For a pure **removed** hunk (no `newCount`), the anchor is
`getLineStartPosition(hunk.newStart)` — the line that now occupies the
slot where the deletion happened — and the gutter glyph on that anchor
line is the deletion marker (see below).

### Gutter indicators

Per hunk:

```ts
if (hunk.kind === "removed") {
  editor.setLineIndicator(bufferId, hunk.newStart, LIVE_DIFF_NS,
    "▾", 255, 85, 85, PRIORITY);
} else {
  const symbol = hunk.kind === "modified" ? "~" : "+";
  const color  = hunk.kind === "modified" ? [255, 184, 108] : [80, 250, 123];
  const lines: number[] = [];
  for (let i = 0; i < hunk.newCount; i++) lines.push(hunk.newStart + i);
  editor.setLineIndicators(bufferId, lines, LIVE_DIFF_NS,
    symbol, color[0], color[1], color[2], PRIORITY);
}
```

`setLineIndicators` (plural) is one IPC round-trip per hunk instead of
one per line — cheap even on large refactors.

### Background highlighting on changed lines

For each `added` or `modified` hunk, paint the new-side lines:

```ts
const start = await editor.getLineStartPosition(hunk.newStart);
const end   = await editor.getLineEndPosition(hunk.newStart + hunk.newCount - 1);
if (start !== null && end !== null) {
  editor.addOverlay(bufferId, LIVE_DIFF_NS, start, end, {
    bg: hunk.kind === "added" ? "diff.added_bg" : "diff.modified_bg",
    extend_to_line_end: true,
  });
}
```

`extend_to_line_end` is the existing flag in `addOverlay` (see
`fresh.d.ts:1535`) and is what makes the highlight look like a unified
diff stripe rather than a tightly clipped span.

### Theme keys

The plugin references theme keys (resolved at render time) so that the
look adapts automatically. Recommended keys, falling back to RGB
defaults if the theme doesn't define them:

| Key | Default RGB | Use |
|-----|-------------|-----|
| `diff.added_bg` | `[20, 60, 30]` | new-line bg |
| `diff.added_fg` | `[80, 250, 123]` | `+` glyph |
| `diff.removed_bg` | `[60, 20, 25]` | virtual old-line bg |
| `diff.removed_fg` | `[255, 85, 85]` | virtual old-line fg, `-`/`▾` glyph |
| `diff.modified_bg` | `[60, 50, 20]` | modified line bg |
| `diff.modified_fg` | `[255, 184, 108]` | `~` glyph |

(If these keys aren't already in `crates/fresh-editor/themes/...`, the
plan adds them in a follow-up theme PR. Until then the plugin uses
RGB tuples.)

## Live-update behavior

This is the critical UX requirement: when a coding agent writes to the
file on disk and Fresh reloads it into the buffer, the diff should
update with no perceptible lag and no flicker.

### Trigger

`after_insert` and `after_delete` are emitted for every buffer
mutation, including the in-memory edits Fresh applies when reloading
a file from disk. Both events carry `buffer_id`; we filter to buffers
the plugin is tracking.

```ts
editor.on("after_insert", ({ buffer_id }) => {
  scheduleRecompute(buffer_id);
  return true;
});
editor.on("after_delete", ({ buffer_id }) => {
  scheduleRecompute(buffer_id);
  return true;
});
```

### Coalescing

A single agent edit can produce dozens of `after_insert` / `after_delete`
events in quick succession (e.g. one per write the agent does, or one
per chunk Fresh's reload pipeline applies). We coalesce them with a
short debounce so we run the diff once per burst:

```ts
function scheduleRecompute(bufferId: number) {
  const s = states.get(bufferId);
  if (!s) return;
  if (s.pendingTimer !== null) return;
  s.pendingTimer = setTimeout(() => {
    s.pendingTimer = null;
    recompute(bufferId).catch((e) => editor.error(`live-diff: ${e}`));
  }, 75);
}
```

`setTimeout` is provided by the QuickJS runtime (`docs/quickjs.md`).
75 ms is a starting value — fast enough that the user sees the diff
"track" the agent in real time, slow enough that a 50-line paste
recomputes once instead of fifty times. Tuneable via config.

### Re-entry guard

`recompute` sets `state.updating = true` for the whole async section.
If a new edit arrives while we're computing, we set a `dirty` flag and
re-run once the in-flight pass finishes — same shape as
`git_gutter.ts`'s `updating` flag, plus the dirty bit so we don't drop
edits that arrived during compute.

### Decoration churn

Naive "clear all, re-add all" on every keystroke would flicker. Two
mitigations:

1. **Single-frame redraw.** All `clearLineIndicators`,
   `clearVirtualTextNamespace`, `clearNamespace`, then all the
   `setLineIndicators` / `addVirtualLine` / `addOverlay` calls happen
   inside one `recompute()` invocation. The renderer batches plugin
   API calls per frame.
2. **Hunk diffing (optional optimization).** If the new `Hunk[]` is
   identical to the previous one (compare by structural hash), skip
   the redraw entirely. Most keystrokes inside an unchanged region
   don't move any hunk boundaries and would otherwise repaint
   needlessly.

### Viewport awareness

For very large files the user only ever sees ~50 lines. If perf
becomes a concern, the plugin can be extended to render virtual lines
only for hunks whose `newStart` falls within
`getViewport()` ± a screenful, and rebuild on `viewport_changed`.
The gutter indicators and overlays are cheap and stay applied
across the whole buffer.

## Command palette / mode switching

The plugin registers commands the user can invoke through the
command palette (`Ctrl+P`):

| Command | i18n key | Action |
|---------|----------|--------|
| Live Diff: Toggle | `cmd.live_diff_toggle` | enable / disable for the active buffer |
| Live Diff: vs HEAD | `cmd.live_diff_vs_head` | set this buffer's mode to `head` |
| Live Diff: vs Disk (unsaved changes) | `cmd.live_diff_vs_disk` | set this buffer's mode to `disk` |
| Live Diff: vs Branch... | `cmd.live_diff_vs_branch` | prompt for a branch name, set mode to `branch` |
| Live Diff: vs Default Branch | `cmd.live_diff_vs_default_branch` | resolve `main`/`master` and set mode to `branch` |
| Live Diff: Refresh | `cmd.live_diff_refresh` | re-fetch reference text and recompute |
| Live Diff: Set Default Mode... | `cmd.live_diff_set_default` | persist the global default for new buffers |

Registration mirrors `git_gutter.ts`:

```ts
editor.registerCommand("%cmd.live_diff_toggle", "%cmd.live_diff_toggle_desc",
                       "live_diff_toggle", null);
editor.registerCommand("%cmd.live_diff_vs_head", "%cmd.live_diff_vs_head_desc",
                       "live_diff_vs_head", null);
editor.registerCommand("%cmd.live_diff_vs_disk", "%cmd.live_diff_vs_disk_desc",
                       "live_diff_vs_disk", null);
editor.registerCommand("%cmd.live_diff_vs_branch", "%cmd.live_diff_vs_branch_desc",
                       "live_diff_vs_branch", null);
// ...
```

Strings live in `live_diff.i18n.json` next to the source file, matching
the convention used by `git_gutter.i18n.json`, `diff_nav.i18n.json`, etc.

### Branch prompt

```ts
async function live_diff_vs_branch() {
  const initial = (editor.getGlobalState("live_diff.last_branch") as string) || "main";
  const ref = await editor.prompt(editor.t("prompt.branch"), initial);
  if (!ref) return;
  editor.setGlobalState("live_diff.last_branch", ref);
  await setMode(editor.getActiveBufferId(), { kind: "branch", ref });
}
```

`editor.prompt` returns a `Promise<string | null>` (`fresh.d.ts:1669`).
Cancelling the prompt is a no-op.

### Default-branch resolution

```ts
async function defaultBranch(cwd: string): Promise<string> {
  // Prefer origin/HEAD, fall back to main, then master.
  const head = await editor.spawnProcess(
    "git", ["symbolic-ref", "--short", "refs/remotes/origin/HEAD"], cwd);
  if (head.exit_code === 0) {
    return head.stdout.trim().replace(/^origin\//, "");
  }
  const showMain = await editor.spawnProcess(
    "git", ["rev-parse", "--verify", "main"], cwd);
  if (showMain.exit_code === 0) return "main";
  return "master";
}
```

### Persistence

- **Per-buffer mode** is stored via
  `editor.setViewState(bufferId, "live_diff.mode", mode)`. Fresh's
  view-state machinery already write-throughs to the snapshot, so the
  mode survives buffer reactivations and editor restarts (when
  Fresh's session restore brings the buffer back).
- **Global default mode** for new buffers is stored under
  `editor.setGlobalState("live_diff.default_mode", mode)`. Defaults
  to `{ kind: "head" }`.

## Edge cases

| Case | Behavior |
|------|----------|
| File not in a git repo | `head` and `branch` fall back to "no diff" with a one-shot status message; `disk` still works. |
| File untracked by git (in repo, but `git ls-files` empty) | Same as above for `head`/`branch`. |
| Buffer has no path (untitled, `is_virtual`) | Plugin is inert; no decorations applied; commands are no-ops with a status message. |
| Reference fetch fails (`git show` non-zero) | Cache `oldText = null`, set status to `"live-diff: <ref> not found"`, leave previous decorations in place until a manual refresh. |
| Buffer is binary | `getBufferLength` is fine but the diff would be huge/meaningless — bail if either side contains a `\0` in the first 8 KB sample. |
| Very large file (>50 k lines either side) | Skip virtual lines; only render gutter glyphs on a coarse line-level summary. Status message explains the degradation. |
| `git_gutter` is also enabled | Both plugins use distinct namespaces and priorities (`live-diff` priority 9, `git-gutter` priority 10). The user can disable `git_gutter` in config; alternatively, the live-diff plugin can detect that `git_gutter` is loaded and suppress its own gutter glyphs in `head` mode (the only overlap), keeping the virtual lines. |
| `disk` mode + agent edit on disk | Fresh's reload pipeline updates the buffer (firing `after_insert` / `after_delete`); we re-read disk on the next save or refresh. While the agent is mid-write, the diff transiently shows the in-flight state vs the previous on-disk version — which is exactly what the user wants. |
| Save in `disk` mode | `after_file_save` triggers a fresh `readFile` and recompute; the diff goes empty until the next edit. |
| Buffer closed | Drop state; nothing else to clean since decorations live on the (now-gone) buffer. |
| Plugin reload | All decorations are namespaced, so a `reloadPlugin` cycle calls the plugin's startup which clears+repaints. |

## Integration with existing plugins

- **`diff_nav.ts`** — already merges `git_gutter_hunks` and the
  saved-diff byte-ranges. Add a third source: the live-diff plugin
  publishes its hunks via
  `editor.setViewState(bufferId, "live_diff_hunks", hunks)`. Update
  `diff_nav.ts:collectTargets` to read that key (in addition to the
  existing two). When the user has live-diff enabled, `n` / `p`
  navigate the live-diff hunks as well.
- **`git_gutter.ts`** — unchanged. Coexists via namespaces. Document
  in the README that running both at once paints two stripes in the
  gutter; users who only want one can disable whichever they prefer.
- **Side-by-side review diff** — orthogonal: that view is for code
  review against a fixed ref, this plugin is for live observation
  while editing. No interaction needed.

## File layout

New files (all under `crates/fresh-editor/plugins/`):

```
live_diff.ts              — plugin source
live_diff.i18n.json       — translatable strings (commands, prompts, statuses)
```

Optional follow-ups:

- `docs/plugins/live-diff.md` — user-facing docs once the feature stabilizes.
- Theme keys `diff.added_*`, `diff.removed_*`, `diff.modified_*` in
  the bundled themes if not already present.

## Testing plan

The existing plugin test infrastructure (see
`docs/internal/lsp-plugin-testing.md` for the pattern, and the
`*.ts` tests Fresh runs as part of `check-types.sh`) supports headless
buffer-driven scenarios. Tests to add:

1. **`head` mode, simple modify** — open a tracked file, programmatically
   replace a line, assert one `modified` hunk with the expected old
   line shown as a virtual line and `~` in the gutter.
2. **`head` mode, pure addition** — insert a new line, assert one
   `added` hunk: no virtual line, `+` in the gutter, added-bg overlay
   on the new line.
3. **`head` mode, pure deletion** — delete a line, assert one
   `removed` hunk: virtual line above the line that took its place,
   `▾` in the gutter on that anchor line.
4. **`disk` mode** — open a file, edit without saving, assert the
   live-diff matches `getBufferSavedDiff` ranges.
5. **`branch` mode** — set up two branches in a fixture repo, switch
   the buffer's mode, assert the diff comes from the chosen ref.
6. **External-write live-update** — write a file on disk via Node's
   `fs`, wait for Fresh's reload, assert the diff updates within
   one debounce window. (This validates the agent use case.)
7. **Mode persistence** — set mode, close+reopen the buffer, assert
   the mode is restored from view state.
8. **Coalescing** — fire 100 `after_insert` events back-to-back,
   assert `recompute` runs exactly once.
9. **Plugin reload** — load, decorate, reload, assert no orphaned
   decorations and the new state matches the buffer.
10. **`git_gutter` coexistence** — enable both, assert both
    namespaces' indicators are present and don't trample each other.

Manual smoke test for the live-update path:

- Open a tracked file in Fresh.
- In a separate terminal, run a script that writes to the file every
  500 ms for 10 s.
- Confirm the gutter and virtual lines update continuously without
  flicker, and that typing into the file at the same time still works.

## Implementation milestones

Each milestone is independently mergeable and leaves the editor in a
working state.

1. **Skeleton + `head` mode, gutter only.**
   Plugin file, command registration, per-buffer state, `git show
   HEAD:<path>` fetch, line diff, gutter glyphs. No virtual lines,
   no overlays. ~150 LOC, copies the structure of `git_gutter.ts`.
2. **Virtual lines for old content.**
   Add `addVirtualLine` rendering for `removed` and `modified`
   hunks. Add the `LIVE_DIFF_VLINE_NS` clear path. This is the step
   that makes the plugin visually distinct from `git_gutter`.
3. **Background overlays on changed lines.**
   `addOverlay` with `extend_to_line_end` for `added` / `modified`
   hunks. Plumb theme keys (with RGB fallbacks).
4. **Live-update path.**
   `after_insert` / `after_delete` handlers, debounced recompute,
   re-entry guard, dirty bit.
5. **`disk` mode.**
   `readFile` reference path, `after_file_save` hook, command-palette
   entry.
6. **`branch` mode.**
   Branch prompt, default-branch resolver, command-palette entries.
7. **Per-buffer / global persistence.**
   `setViewState`, `setGlobalState`, plumb default mode for new buffers.
8. **`diff_nav.ts` integration.**
   Publish hunks under `live_diff_hunks` view-state key; update
   `diff_nav.ts:collectTargets`.
9. **Performance pass.**
   Hunk-equality short-circuit; large-file fallback; viewport-aware
   virtual-line rendering if benchmarks show it's needed.
10. **Docs + theme keys.**
    `docs/plugins/live-diff.md`, theme entries, README notes about
    coexistence with `git_gutter`.

## Open questions

- **Should the plugin replace or complement `git_gutter`?** The
  cleanest UX is for live-diff to subsume `git_gutter` in `head` mode,
  but that's a breaking change for users who set up keybindings around
  `git_gutter`'s namespace. The plan is to ship as a complement and
  let users disable whichever they don't want; reconsider after
  dogfooding.
- **How to detect agent-driven external writes vs. user typing?**
  Fresh emits the same `after_insert` / `after_delete` events for both,
  so the plugin treats them uniformly — which is the right answer for
  this feature. If we ever want to *distinguish* (e.g. flash a
  different color for agent edits), we'd need a new hook event in
  `fresh-core/src/hooks.rs`. Out of scope here.
- **Virtual-line placement on the very first line.** `addVirtualLine`
  with `above: true` at byte 0 should render above line 0 — this is
  already exercised by `markdown_compose.ts` (top border) and
  `git_blame.ts` (header above first block), so we expect it to
  Just Work. Worth a regression test.
- **Whitespace-only changes.** Show or hide? Default: show, with a
  config flag `live_diff.ignore_whitespace` to suppress
  whitespace-only hunks (passes `-w` to `git show`-equivalent diff
  inputs, or filters in the in-memory diff).
- **Submodule / symlink files.** `git show` may resolve oddly for
  these. Treat them like "reference fetch failed" and degrade
  gracefully.
