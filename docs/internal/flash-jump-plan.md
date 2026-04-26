# Flash-style label jump for Fresh

A plan to bring [flash.nvim](https://github.com/folke/flash.nvim)-style
label-based jump navigation to Fresh, plus the plugin-API improvements
the work surfaces along the way.

The user-visible feature: press a hotkey, type 1–2 characters, see
single-letter labels appear next to every visible match, press a label
to jump there. Labels are picked so they never collide with the next
character of any match, so typing more pattern characters never lands
on a label by accident.

Status: **proposal**. Not yet implemented.

## Why this is interesting

Fresh already has many of the pieces — a regex search engine, a
viewport API, virtual text, overlays, and a modal-mode plugin
mechanism. What's missing is mostly ergonomic: the existing plugin
API forces verbose per-key boilerplate (vi_mode pays this tax for
about 300 lines), and there's no first-class way for a plugin to
"read the next keypress" or coalesce decoration writes.

Fixing those gaps unlocks flash and meaningfully simplifies vi_mode
at the same time.

## Reference: how flash.nvim works

About 2,950 lines of Lua, but the core is small:

- `state.lua` (437 lines) drives the loop — read a char, extend
  pattern, re-match, re-label, redraw; repeat until a label is hit
  or aborted. Decoration is reattached every frame via
  `nvim_set_decoration_provider`.
- `search/pattern.lua` (108 lines) compiles user input into a regex
  per mode (`exact`, `fuzzy`, `search`) and exposes a `skip` regex
  used for label-conflict avoidance.
- `search/init.lua` + `matcher.lua` (~300 lines) run the regex over
  visible window ranges and return `Match { win, pos, end_pos }`.
- `labeler.lua` (225 lines) is the clever bit: sort matches by
  distance from cursor, then assign letters from the label pool,
  *removing* any letter that appears as the next-char-after-match.
  Lowercase labels are reused across pattern updates so positions
  stay visually stable.
- `highlight.lua` (215 lines) renders three layers via extmarks: a
  dim "backdrop", per-match highlight, and virtual-text labels.

The overall shape is: **regex match in viewport → sort → assign
labels avoiding next-char conflicts → virtual-text overlay → wait
for keypress → either jump or extend pattern**.

## Implementation strategy

**Plugin first, native if needed.** Fresh's plugin API exposes
enough primitives to implement flash entirely in TypeScript —
`getViewport`, `getBufferText`, `addOverlay`, `addVirtualText`,
`defineMode` + `setEditorMode`, `setBufferCursor`. No Rust changes
are strictly required for v1.

The catch: with today's plugin API this comes out clunky in the
same ways vi_mode is clunky. The plan is therefore in two phases:

1. **Phase 1: API improvements** that simplify both flash and
   vi_mode. Most are small.
2. **Phase 2: ship `flash.ts`** as a bundled plugin, ~200–300
   lines.

If Phase 1 gets blocked or scoped down, flash can still ship as a
~500-line plugin against the current API — same shape vi_mode uses
today.

## Plugin API improvements

Distilled from reading flash.nvim, fresh's plugin API surface
(`fresh.d.ts`), and how vi_mode/theme_editor/audit_mode/git_log
actually use it. Numbered for reference, not priority.

| # | Improvement | Problem it solves | Impact | Effort |
|---|---|---|---|---|
| 1 | `editor.getNextKey(): Promise<KeyEvent>` | vi_mode burns ~190 lines on per-key handler stubs just to read one keypress; flash needs the same pattern 2–3 times | Huge — collapses entire modes to ~5 lines | Small |
| 2 | Wildcard binding `["*", handler]` *or* `defineModeHandler(name, fn)` | `defineMode` requires enumerating ~95 keys; handler can't tell which key fired without per-key closures | Huge — kills the 35-line bindings tables | Small |
| 3 | Bindings carry an `arg`: `["a", { handler, arg: "a" }]` | Same root cause as #2; today every distinct arg needs its own registered handler | High (deeper fix than #2) | Medium |
| 4 | `defineMode` accepts string `parent` (not just `inheritNormalBindings: bool`) | vi_mode.ts:18 TODO — modes can't inherit from arbitrary parents | Medium — flash-mode could layer on vi-normal | Small |
| 5 | `editor.batchDecorations(buf, fn)` | Flash redraws every keypress: clear + N overlays + N virtual texts = many round-trips per frame | High at scale | Small–Medium |
| 6 | Theme-key colors for `addVirtualText` | `addOverlay` accepts `"ui.muted"`; `addVirtualText` only takes raw `r,g,b` | Medium — labels would follow theme | Small |
| 7 | `editor.getViewportText(buf): Promise<{text, topByte, lines}>` | Today: `getViewport` + `getLineEndPosition` + `getBufferText` = 3 round-trips for "give me visible text" | Medium — also helps live_grep, audit_mode | Small |
| 8 | `editor.on("frame", handler)` per-frame redraw hook | flash.nvim uses `nvim_set_decoration_provider`; Fresh overlays only update on user input | Medium — needed for animated/cursor-tracking decorations | Medium |
| 9 | Verify `clearNamespace` / `clearVirtualTextNamespace` are actually wired | `fresh.d.ts` exposes them but no surveyed plugin uses them — flash will lean on them heavily | Risk-mitigation | Investigation |
| 10 | High-level `editor.modal({ bindings, onChar, render })` helper | Wraps mode entry + key loop + render + cleanup in one call | Low (mostly subsumed by #1+#2) | Medium |
| 11 | Reconcile vi_mode.ts:17 TODO about `getLineStartPosition` with the API surface | TODO says it doesn't exist; `fresh.d.ts:1091` says it does. Stale doc or recent addition | Low — cleanup | Trivial |

Suggested landing order: **#1 → #2 → #5 → #6 → #7**. With just
#1 and #2 in hand, flash becomes a ~200-line plugin and vi_mode
sheds roughly 300 lines.

## Phase 2: the flash plugin

`crates/fresh-editor/plugins/flash.ts`, sketch:

```ts
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

interface Match { start: number; end: number; label?: string }
const LABELS = "asdfghjklqwertyuiopzxcvbnm";

async function flashJump() {
  const buf = editor.getActiveBufferId();
  let pattern = "";
  let matches: Match[] = [];

  while (true) {
    redraw(buf, matches);
    const ev = await editor.getNextKey();          // needs API #1
    if (ev.key === "Escape") break;

    // Did they press a label?
    const hit = matches.find(m => m.label === ev.char);
    if (hit) { editor.setBufferCursor(buf, hit.start); break; }

    pattern += ev.char ?? "";
    matches = await findInViewport(buf, pattern);
    matches = assignLabels(matches);                // skip next-char conflicts
  }
  editor.clearNamespace(buf, "flash");              // verify API #9
  editor.clearVirtualTextNamespace(buf, "flash");
}

registerHandler("flash_jump", flashJump);
editor.registerCommand("%cmd.flash_jump", "%cmd.flash_jump_desc",
                       "flash_jump", null);
```

Sub-pieces (~200 lines total):

- `findInViewport(buf, pattern)` — `getViewport` + `getBufferText`
  on the visible byte range, run a JS regex, return byte-offset
  matches. Reuse the [API #7](#plugin-api-improvements) helper if
  available.
- `assignLabels(matches)` — port of `flash.nvim/labeler.lua`. Sort
  by distance from cursor, walk pool of label letters, remove any
  letter that equals `text[match.end]` (case-folded if appropriate).
- `redraw(buf, matches)` — `clearNamespace` + per-match `addOverlay`
  (backdrop, match highlight) + `addVirtualText` for label.
  Wrapped in `batchDecorations` once API #5 lands.

Optional v1+ features (skip for first cut):

- Multi-window jumping (Fresh has splits — one buffer per pass for
  v1).
- `t`/`T`/`f`/`F` enhanced char motions (vi_mode already does this;
  reuse rather than reimplement unless the bindings make sense
  outside vi).
- Treesitter-node selection (Fresh's syntax stack is different,
  separate design).
- Dot-repeat — needs a hook flash.nvim has via `repeat.lua` and
  Vim's `'.'`. Out of scope for v1.

## Open questions

1. **`fresh.d.ts:1571`** — `addVirtualText(buf, id, pos, text, r, g, b, before, useBg)`
   takes raw RGB. Confirmed gap; tracked as #6. Backwards-compat:
   accept `(r, g, b)` *or* a style object via overload.

2. **`vi_mode.ts:16-19`** TODO list — partially stale (line 17
   `getLineStartPosition` exists in d.ts:1091), partially still
   valid (line 18 mode-parent gap). Worth a sweep.

3. **`clearNamespace` actual semantics** — d.ts comments suggest it
   clears overlays in a namespace. The agent survey didn't find any
   plugin using it; existing plugins clean up via state resets. We
   need to verify the implementation actually walks and removes
   overlays before flash relies on it for per-keypress redraws.
   This is item #9.

4. **Render cadence** — flash needs decorations to refresh after
   *each keypress*, not on a polling cycle. The current overlay
   pipeline updates on user input, which should suffice; needs to
   be confirmed in `view_pipeline.rs` and the plugin-write path.

5. **Multi-cursor interaction** — does flash move the primary
   cursor only (matches flash.nvim) or add cursors at the label
   target (a "scatter" mode unique to Fresh)? Default to primary;
   leave scatter as a follow-up.

## Validation pass needed

Before committing to API #1/#2/#5 specifically, the plugin
runtime, input dispatch, and render pipeline need a careful read
to confirm the assumed shape:

- **Plugin runtime** (`crates/fresh-plugin-runtime`) — how
  handlers are invoked from QuickJS. Specifically whether a
  `getNextKey()` Promise can park on the JS-side event loop while
  the editor pumps keys, or whether something else runs the
  reactor.
- **Input dispatch** (`crates/fresh-editor/src/input/` and
  `app/input_dispatch.rs`) — confirm the modal-mode key path,
  whether wildcard bindings are feasible, and how Escape/cancel
  propagates.
- **Render pipeline** (`crates/fresh-editor/src/view/`) — confirm
  per-keystroke redraw is automatic (overlays are read on every
  paint) and that namespace clear is honored.

This pass is the next step; findings will revise the table above
before any code lands.
