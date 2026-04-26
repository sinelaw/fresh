# Flash-style label jump for Fresh

A plan to bring [flash.nvim](https://github.com/folke/flash.nvim)-style
label-based jump navigation to Fresh, plus the plugin-API improvements
the work surfaces along the way.

The user-visible feature: press a hotkey, type 1–2 characters, see
single-letter labels appear next to every visible match, press a label
to jump there. Labels are picked so they never collide with the next
character of any match, so typing more pattern characters never lands
on a label by accident.

Status: **proposal, validated against source.** Not yet
implemented. Plugin runtime, input dispatch, and render pipeline
have been read end-to-end and confirmed to support the design;
findings recorded below in [Validation pass results](#validation-pass-results).

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
| 5 | Expose the existing `OverlayManager::extend()` fast-path to plugins as `setNamespaceOverlays(buf, ns, overlays[])` | Each `addOverlay()` calls `Vec::push + sort_by_key` — `O(N log N)` per call, `O(N² log N)` for a batch. The internal `extend()` already does one sort for a whole list (`overlay.rs:297`). | Medium — only matters past ~100 overlays | Small |
| 6 | Theme-key colors for `addVirtualText` | `addOverlay` accepts `"ui.muted"`; `addVirtualText` only takes raw `r,g,b` | Medium — labels would follow theme | Small |
| 7 | `editor.getViewportText(buf): Promise<{text, topByte, lines}>` | Today: `getViewport` + `getLineEndPosition` + `getBufferText` = 3 round-trips for "give me visible text" | Medium — also helps live_grep, audit_mode | Small |
| 8 | `editor.on("frame", handler)` per-frame redraw hook | flash.nvim uses `nvim_set_decoration_provider`; Fresh overlays only update on user input | Medium — needed for animated/cursor-tracking decorations | Medium |
| 9 | ~~Verify `clearNamespace` / `clearVirtualTextNamespace` are wired~~ **Resolved.** Implemented at `overlay.rs:319`, `O(N)` scan + marker cleanup, ~1 µs for 100 overlays. | — | — | — |
| 10 | High-level `editor.modal({ bindings, onChar, render })` helper | Wraps mode entry + key loop + render + cleanup in one call | Low (mostly subsumed by #1+#2) | Medium |
| 11 | Reconcile vi_mode.ts:17 TODO about `getLineStartPosition` with the API surface | TODO says it doesn't exist; `fresh.d.ts:1091` says it does. Stale doc or recent addition | Low — cleanup | Trivial |
| 12 | Document/expose the "single global mode" constraint | Fresh has one `editor_mode: Option<String>`, no stack. Plugins entering a mode must save & restore the prior mode themselves; Escape doesn't auto-pop. | Low — docs / convenience helper | Trivial–Small |

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
  (backdrop with `extendToLineEnd: true` and low priority, match
  highlight on top) + `addVirtualText` for label. Individual `add`
  calls are fine for v1; switch to `setNamespaceOverlays` (API #5)
  if profiling shows the per-call re-sort hurts.

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

## Validation pass results

A focused read of plugin runtime, input dispatch, and render
pipeline. Findings — all confirm the design or tighten it; no
blockers found.

### Plugin runtime

- Plugins run on a **dedicated single-threaded tokio runtime**
  (`fresh-plugin-runtime/src/thread.rs:245-295`) wrapped in a
  `LocalSet`. The editor thread is never blocked when calling a
  plugin handler — calls go over an unbounded mpsc channel.
- Existing async APIs (`prompt`, `getBufferText`, `spawnProcess`)
  use exactly the pattern `getNextKey` would need:
  1. JS wrapper allocates a `callback_id` and stores
     `_pendingCallbacks.set(id, { resolve, reject })`
     (`quickjs_backend.rs:5061-5080`).
  2. Editor side sends a `PluginCommand` with the id.
  3. When the result is ready, editor calls
     `PluginThreadHandle::resolve_callback(id, value)`.
  4. Plugin thread invokes `_resolveCallback(id, value)`
     (`quickjs_backend.rs:5808-5887`) which resolves the JS Promise.
  5. `poll_event_loop_once()` (`thread.rs:949-984`) drains
     microtasks every 1 ms while there's pending work.
- Multiple pending Promises per plugin coexist safely (distinct
  ids in `_pendingCallbacks`).
- **Verdict**: `getNextKey` (#1) is essentially copy-paste from
  the `prompt` template. Add `PluginCommand::KeyEventRequested
  { callback_id }`; resolve from the input dispatch path on the
  next key event.

### Input dispatch

- Mode bindings are stored in `plugin_defaults: HashMap<KeyContext,
  HashMap<(KeyCode, KeyModifiers), Action>>` keyed by
  `KeyContext::Mode(String)` (`keybindings.rs:1327-1338`). Lookup
  is plain `HashMap::get`.
- **Wildcard binding (#2)** — `~5–10 lines`. Add a fallback after
  the exact-key lookup at `keybindings.rs:1740-1748` that probes
  a sentinel "any-key" entry. Pass the actual `(KeyCode,
  KeyModifiers)` to the plugin handler.
- **Handler-with-arg (#3)** — `~20 lines` editor-side
  (`Action::PluginAction(name, arg: Option<String>)`) +
  `~30 lines` plugin runtime (forward arg into JS handler call).
- **`mode_parents` (#4)** — `~30 lines`, backward-compatible.
  Replace `inheriting_modes: HashSet<String>`
  (`keybindings.rs:1341-1343`) with
  `mode_parents: HashMap<String, String>` and recursively resolve
  in the fallthrough path at `:1756`.
- **Single global mode** —
  `editor_mode: Option<String>` (`editor_accessors.rs:276`). Modes
  do not stack; `setEditorMode("flash")` replaces whatever was
  there. The plugin must save the prior mode and restore on exit;
  Escape does not auto-pop. Captured as #12.
- **Mode bindings only consulted in `KeyContext::Normal`**
  (`input.rs:132`) — so flash's mode bindings never compete with
  a prompt or popup that's already taken modal focus. Fine for
  flash.

### Render pipeline

- **Event-driven, 60 Hz cap.** Render gate at `main.rs:3805`
  fires when `needs_render && last_render.elapsed() >=
  FRAME_DURATION` (16 ms). Plugin overlay writes set
  `plugin_render_requested = true`
  (`plugin_commands.rs:91`); user input also sets
  `needs_render`. **Same-frame pickup**: a plugin handler that
  runs in response to a keypress writes overlays before the
  render that the keypress already requested.
- **Overlay storage**: `EditorState::overlays: OverlayManager`
  (`state.rs:148`). `Vec<Overlay>` sorted by priority, with
  marker-tracked positions. Read path at render uses
  `OverlayManager::query_viewport()` (`overlay.rs:429-488`),
  marker-interval-tree `O(log N + k)` lookup (k = overlays in
  viewport, typically 2–10).
- **`clearNamespace`** is implemented at `overlay.rs:319`: scans
  `Vec<Overlay>`, retains overlays not in the namespace, deletes
  associated markers. `O(N)` + `O(M log M)` re-sort if anything
  remains. Roughly 1 µs for 100 overlays — **negligible for
  per-keypress flash redraws**. Resolves item #9.
- **`addOverlay` cost**: `Vec::push` + `sort_by_key`
  (`overlay.rs:283-287`) → `O(N log N)` per call. 100 successive
  adds: `O(N² log N)` ≈ ~100 µs. Acceptable for v1; the
  fast-path `extend()` (`overlay.rs:297`) sorts once for a whole
  list — this is what API #5 should expose, not a generic batch
  primitive.
- **Theme keys resolve at render time** (`char_style.rs:173-188`).
  Theme switches mid-flash-mode just work.
- **Backdrop dimming**: priority system + `extend_to_line_end`
  works as flash.nvim uses it. Backdrop at low priority + match
  highlight at higher priority + virtual-text label is the right
  pattern.
- **Virtual text** is interleaved cell-by-cell during line render,
  not a separate decoration layer (`overlays.rs:224-229`). 30+
  single-char labels per frame is a non-issue.

### Net effect on the API table above

- **#5 narrowed**: don't propose a generic `batchDecorations`;
  expose the already-existing `extend()` fast-path as
  `setNamespaceOverlays(buf, ns, overlays[])`. Smaller change,
  same win, clearer semantics.
- **#9 resolved**: `clearNamespace` works as expected.
- **#12 added**: document the single-global-mode constraint, or
  add a small "save/restore prior mode" convenience helper.
- All other items unchanged. **No design changes required to
  Phase 2 (`flash.ts`)**.

## Open questions

1. **`fresh.d.ts:1571`** — `addVirtualText(buf, id, pos, text, r, g, b, before, useBg)`
   takes raw RGB. Confirmed gap; tracked as #6. Backwards-compat:
   accept `(r, g, b)` *or* a style object via overload.

2. **`vi_mode.ts:16-19`** TODO list — partially stale (line 17
   `getLineStartPosition` exists in `fresh.d.ts:1091`), partially
   still valid (line 18 mode-parent gap, addressed by #4). Worth
   a sweep — tracked as #11.

3. **Multi-cursor interaction** — does flash move the primary
   cursor only (matches flash.nvim) or add cursors at the label
   target (a "scatter" mode unique to Fresh)? Default to primary;
   leave scatter as a follow-up.
