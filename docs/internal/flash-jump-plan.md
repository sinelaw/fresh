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
same ways vi_mode is clunky. The plan is therefore phased:

1. **Phase 1: API improvements** that simplify both flash and
   vi_mode. Most are small.
2. **Phase 2: ship `flash.ts`** as a bundled plugin, ~200–300
   lines. Active-split buffer only.
3. **Phase 3 (multi-split):** add per-split viewport read; flash
   labels appear in every visible buffer at once.
4. **Phase 4 (chrome flash / "hint mode"):** add a screen-cell
   overlay primitive + a chrome-target enumerator, then labels
   work on file explorer entries, tabs, status bar widgets,
   split-focus targets, popups. This is the editor-wide
   Vimium-style variant; meaningfully different scope than
   flash.nvim, which is buffer-only.

If Phase 1 gets blocked or scoped down, flash can still ship as a
~500-line plugin against the current API — same shape vi_mode uses
today. Phases 3 and 4 are independent and can land in any order.

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
| 13 | `editor.getViewportForSplit(splitId): ViewportInfo` (or `editor.listSplits(): { splitId, bufferId, viewport }[]`) | Today `getViewport()` only returns the active split. Multi-split flash needs per-split viewport reads to know what to label. | Medium — unlocks Phase 3 | Small (state already there, just expose) |
| 14 | `editor.addScreenOverlay(x, y, text, style): handle` — absolute screen-coordinate label | All chrome (file explorer, tabs, status bar, popups, split separators) is drawn outside any buffer. Today there's no plugin-visible way to draw a glyph at `(x, y)`. The infrastructure half-exists in `view/animation.rs:104-115` (post-render cell mutation for slide-ins) — promote it to a plugin-visible API. | High — unlocks Phase 4 + many other plugin use cases (toasts, HUDs, custom overlays) | Medium |
| 15 | `editor.listVisibleHintTargets(): HintTarget[]` — enumerate visible interactive elements with screen coords + activation handle | Each chrome element already has its own hit-test logic (`TabLayout::hit_test`, `MenuLayout::hit_test`, status bar `ElementKind` regions, file tree). Surface them as a unified plugin-visible list so chrome-flash can paint labels and dispatch click-equivalent actions. | High — required for Phase 4 | Medium |

Suggested landing order: **#1 → #2 → #5 → #6 → #7 → #13 → #14 → #15**.
With just #1 and #2 in hand, flash becomes a ~200-line plugin and
vi_mode sheds roughly 300 lines. #13 unlocks multi-split (Phase 3).
#14+#15 unlock chrome flash / hint mode (Phase 4).

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

Optional v1+ features (deferred to later phases):

- **Multi-split jumping (Phase 3):** label matches in every
  visible split simultaneously. Mechanically the same — overlays
  are buffer-anchored, so `addOverlay(otherBufId, ...)` already
  paints in whichever split is showing that buffer. Blocked
  only on per-split viewport read (API #13). flash.nvim's
  `multi_window: true` mode is exactly this.
- **Chrome flash / hint mode (Phase 4):** label visible file
  explorer entries, tabs, status bar widgets, split-focus
  targets, popup items. The agent confirmed all of these are
  drawn directly by Rust UI code — *not* buffers — so today's
  buffer-anchored `addOverlay`/`addVirtualText` cannot reach
  them. Blocked on a screen-cell overlay API (#14) plus a
  chrome-target enumerator (#15). On label press, dispatch the
  equivalent click action via existing per-element handlers
  (`TabLayout::hit_test → click`, file explorer click handler,
  etc.). This is meaningfully more powerful than flash.nvim and
  is the path to a Vimium-style editor-wide hint UI.
- `t`/`T`/`f`/`F` enhanced char motions (vi_mode already does this;
  reuse rather than reimplement unless the bindings make sense
  outside vi).
- Treesitter-node selection (Fresh's syntax stack is different,
  separate design).
- Dot-repeat — needs a hook flash.nvim has via `repeat.lua` and
  Vim's `'.'`. Out of scope for v1.

## What counts as "jumpable"?

Visible elements in Fresh fall into three classes that need
different machinery (validated against the source by the chrome
audit pass):

| Class | Examples | Mechanism | Phase |
|---|---|---|---|
| **Active-buffer content** | Text in the focused split | `addOverlay` + `addVirtualText` on `bufferId` | 2 (v1) |
| **Other-split content** | Text in non-focused splits | Same buffer-anchored overlays — overlays paint wherever the buffer is shown. Needs **API #13** to enumerate per-split viewports. | 3 |
| **Virtual-buffer content** | Diagnostics panel, search results, git log panels | Same as above — these *are* buffers (`createVirtualBufferInSplit`). No new API needed; just falls out of Phase 3. | 3 |
| **Chrome** | File explorer entries, tabs, status bar widgets, menu items, command-palette suggestions, popup items, split separators | Drawn directly by Rust UI code; no buffer addressability. Each element already has its own hit-test (`TabLayout::hit_test`, `MenuLayout::hit_test`, status bar regions, file tree click handler). Needs **API #14** (screen-cell overlay) + **API #15** (chrome target enumerator). | 4 |

The clean cut is: anything that's a buffer is reachable now;
anything that's chrome needs the new screen-overlay primitive.
Phase 4 is the most exciting feature but also the biggest design
piece, and #14 has high leverage beyond flash (toasts,
notifications, picture-in-picture HUDs, animated transitions
beyond slide-in).

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

## Testing strategy

`CONTRIBUTING.md` lays down five rules that shape the test plan:

1. **"Reproduce before claiming"** — every behavioural claim
   needs a test that fails without the change.
2. **"E2E observe, not inspect"** — drive keys/mouse, assert on
   rendered screen text, not on accessor calls into model state.
3. **No timeouts** — use `harness.wait_until(predicate)` for
   semantic waiting.
4. **Test isolation** — per-test temp dirs, internal clipboard
   mode, parallel-safe.
5. **"Enumerate cross-cutting state"** — list every subsystem
   the change touches and write an interaction test for each.

Flash is a feature that *touches everything visible* — overlays,
virtual text, modes, cursors, splits, themes, render cadence —
which makes rule 5 the binding constraint. Most of the testing
budget should go to interaction tests.

### Test layers

| Layer | Tool | What it covers |
|---|---|---|
| **Unit** | plain `#[test]` on pure helpers | Label-assignment algorithm, distance sort, regex pattern compilation, next-char skip. No editor required. |
| **Property** | `proptest` (already in use at `tests/property_tests.rs`) | Labeler invariants — generate buffer + cursor + pattern, assert no label collides with any match's next-char. |
| **Integration / plugin loading** | `EditorTestHarness` + `copy_plugin("flash")` | Plugin loads, command appears in palette, mode bindings register. |
| **E2E (rendered output)** | harness `send_key`/`type_text` + `screen_to_string`/`assert_screen_contains` | Activation, label rendering, jump, cancel — all observed via screen text. |
| **Cross-feature interaction** | E2E with concurrent feature active (vi_mode, multi-cursor, search, splits, theme, plugins) | Interaction matrix; see below. |
| **Performance / scale** | bench-style test with large file, viewport-localized assertion | Per-keystroke redraw stays viewport-bounded; `O(visible)` not `O(buffer)`. |
| **Snapshot** | insta snapshots of label assignments | Regression guard against silent label-pool changes. |

### Pure-logic tests (unit + property)

These can ship in `crates/fresh-editor/tests/flash_label_tests.rs`
even though the feature is plugin-side, because the labeler is
worth porting to a testable Rust module *or* tested inside the
plugin runtime via a small QuickJS harness — pick one based on
where the labeler ends up living.

**Property: no-collision invariant.** *The* core flash invariant.

```rust
proptest! {
    #[test]
    fn prop_assigned_labels_never_collide_with_next_char(
        text in "[a-zA-Z0-9 ]{0,500}",
        pattern in "[a-zA-Z]{1,3}",
        cursor in 0usize..500,
    ) {
        let matches = find_matches(&text, &pattern);
        let labelled = assign_labels(&text, &matches, cursor, LABEL_POOL);
        for m in &labelled {
            if let Some(label) = m.label {
                let next = text.as_bytes().get(m.end).copied();
                prop_assert_ne!(Some(label as u8), next);
            }
        }
    }
}
```

**Property: label stability across pattern extension.** Typing
one more character that *narrows* the match set should keep
labels on surviving matches stable (lowercase reuse policy from
flash.nvim).

**Property: distance ordering.** First N matches sorted by byte
distance from cursor receive the first N letters of the pool.

**Property: determinism.** Same buffer + same cursor + same
pattern → identical label assignment.

**Unit: edge cases.**

- Empty pattern → no matches, no labels.
- Pattern with zero matches → no labels, no panic.
- Match at end-of-buffer (no next char) → label may use any pool letter.
- Multi-byte UTF-8 inside match / next-char.
- Pattern that matches at cursor position (current-match handling).
- Pool exhaustion: more matches than label letters → first N labelled, rest skipped (matches flash.nvim).

### E2E tests (driven through the plugin)

Pattern matches existing plugin tests (`tests/e2e/live_grep.rs`,
`tests/e2e/search_replace.rs`):

```rust
fn flash_jumps_to_label() {
    let project = TestFixture::project_with_plugins(&["flash"]);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100, 30, Default::default(), project.root.clone()).unwrap();
    let file = TestFixture::file("buf.txt", "hello world hello there hello\n");
    harness.open_file(&file.path).unwrap();
    harness.render().unwrap();

    // Trigger flash via command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL).unwrap();
    harness.type_text("Flash Jump").unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Type pattern; wait for labels to appear (semantic wait)
    harness.type_text("hello").unwrap();
    harness.wait_until(|h| h.screen_to_string().contains("a")  // first label
                          && h.screen_to_string().contains("s")) // second label
           .unwrap();

    // Press a label → cursor jumps; mode exits; backdrop clears
    harness.send_key(KeyCode::Char('s'), KeyModifiers::NONE).unwrap();
    harness.wait_until(|h| !h.screen_to_string().contains("flash backdrop marker"))
           .unwrap();
    // Assert cursor is at the second "hello" via rendered cursor cell
    // (NOT via accessor — observe on screen)
}
```

**E2E coverage list (each is one test):**

- Activate flash; backdrop visible, no labels yet (empty pattern).
- Type pattern, ≥1 match → labels render adjacent to matches.
- Press valid label → cursor at match start, decorations gone, mode restored.
- Press Escape → cursor unchanged, decorations gone.
- Press Backspace → pattern shrinks, label set re-renders.
- Pattern with zero matches → no labels, friendly state (per flash.nvim, mode stays open).
- Single match + autojump enabled → cursor jumps without label press.
- Pattern continues past max length → behaviour matches the
  `max_length` config (jump on overflow vs cancel).
- Label letter typed when no matches yet → treated as pattern char,
  not a (nonexistent) label.
- Two label-races: typing more characters that would collide with
  an existing label letter — verify the next-char skip kept that
  letter out of the pool.

### Cross-feature interaction matrix

This is the rule-#8 list. Each row is one E2E test; the assertion
is *on rendered output and surviving cross-cutting state*. Pre-
existing plugins/features in the column header are active *while*
flash runs.

| Concurrent feature | Test |
|---|---|
| **Multi-cursor** | Two cursors active before flash; flash jumps primary; secondary cursors still rendered at their original positions. |
| **Visual / block selection** | Selection active; flash cancelled (or extends selection — pick one and test it). |
| **Existing search highlights** | `Ctrl+F` highlights present in their own namespace; flash backdrop + labels render *over* them; on flash exit, search highlights remain. |
| **vi_mode (when both loaded)** | vi-normal active when flash triggered. After flash exits, `editor_mode` is back to `vi-normal` — observed by typing `j` and seeing cursor move down. |
| **LSP diagnostics** | Diagnostic underlines on a line; flash overlays don't replace them (different namespace); after flash, underlines still present. |
| **Splits (Phase 3)** | Two splits open; flash labels in both; press a label in non-active split → focus *and* cursor move to that split. |
| **Folds** | Folded range contains a match; label appears on fold header (or match is skipped — pick the flash.nvim semantic and test it). No panic either way. |
| **Soft-wrapped lines** | Long line wrapped onto 3 visual rows; match in the middle visual row → label renders at the right cell. |
| **CRLF buffer** | Match spans across `\r\n` neighbour bytes correctly; jump lands on the right byte. |
| **Theme switch mid-flash** | Apply theme via `editor.applyTheme` while flash is active → next render uses new theme keys for backdrop/label colours. |
| **Resize during flash** | Terminal resize event mid-flash → viewport changes → matches re-computed for the new visible range. |
| **Buffer switch / close mid-flash** | User triggers buffer change while flash open → flash cancels, no orphan overlays in the new buffer. |
| **Modal opens (prompt, popup)** | Command palette opened mid-flash → flash mode yields, prior state restored on palette close. |
| **Concurrent plugin overlays** | `git_gutter` / `todo_highlighter` overlays present in their own namespaces → flash's `clearNamespace("flash")` doesn't touch them. |
| **Multi-cursor "scatter" mode (if implemented)** | Press all visible labels (or `<C-a>`) → cursor added at every match. |
| **Read-only buffer** | Flash works (it's read-only too — only moves cursor). |
| **Empty buffer** | Activate flash on empty buffer → no matches, friendly state, Escape exits cleanly. |
| **Huge file** (≥10 MB) | Activate flash → only viewport scanned (assert via timing or by instrumenting the test plugin). |

### "Reproduce before claiming" tests for each API addition

Each numbered API improvement (#1–#15) needs at least one test
that fails before the change and passes after. The minimum
shape: a tiny test plugin lives in `tests/fixtures/test_plugins/`
that uses the new API; the test asserts the outcome on screen.

| API | Failing-without-fix test |
|---|---|
| #1 `getNextKey` | Plugin awaits `getNextKey()`, harness sends a key, plugin renders the received char; assert on screen. Without #1, plugin can't compile / API doesn't exist. |
| #2 wildcard binding | Plugin defines mode with `["*", "h"]`, harness sends 5 different keys, plugin counts via virtual text. Without #2, only explicitly-bound keys reach the handler. |
| #3 binding arg | Plugin defines `["a", { handler: "h", arg: "A" }]` and `["b", { handler: "h", arg: "B" }]`, presses each, asserts arg captured. Without #3, plugin needs two handlers. |
| #4 mode parent | Mode `M2` declares parent `M1`, `M1` binds `q` → action; in `M2`, press `q` → action runs. Without #4, fall-through doesn't reach `M1`. |
| #5 `setNamespaceOverlays` | Plugin replaces 100 overlays via the new call vs 100 individual `addOverlay` calls; assert single-pass sort cost via instrumented test counter. |
| #6 theme-key virtual text | Plugin sets virtual text with theme key, assert rendered cell uses theme-resolved colour after `applyTheme`. |
| #7 `getViewportText` | Plugin makes one call vs three; harness counts plugin↔editor round-trips via instrumented channel. |
| #8 frame hook | Plugin subscribes to `"frame"`, harness forces N renders, assert handler called ≥ N times. |
| #13 `getViewportForSplit` | Two splits open; plugin queries non-active split's viewport; assert returned `topByte` matches what the renderer used. |
| #14 `addScreenOverlay` | Plugin draws label `"X"` at `(10, 5)`; assert `harness.get_cell(10, 5)` shows `X`. |
| #15 `listVisibleHintTargets` | Plugin lists targets, asserts tab labels and file-explorer entries appear in the list with screen coords matching what mouse-click hit-testing would resolve to. |

### Performance assertions

CONTRIBUTING.md rule #2 forbids full-buffer scans. Two tests
guard this:

- **Viewport-bounded match collection.** Open a 10 MB synthetic
  buffer where line N contains the unique pattern only at line
  500. Activate flash with the pattern; assert the matches the
  plugin computed include only viewport lines, not line 500.
  Implementation: instrument the test plugin to log how many
  lines it scanned, and assert `≤ viewport.height + 2`.
- **Per-keystroke redraw budget.** With 100 visible matches,
  measure `harness.render()` time on a synthetic deterministic
  setup. Compare against a baseline; alert (don't fail) on
  regression. Use the test-time clock (`advance_time`) where
  applicable.

### Snapshot tests

Use `insta` (already a dep based on `tests/common/snapshots/`):
fix a buffer + pattern + cursor, snapshot the assigned labels.
A refactor that quietly changes the label pool ordering or
distance metric will diff visibly. Snapshot the plain
`assignLabels` output, not the rendered screen — small,
readable, deterministic.

### Test infrastructure additions needed

- **A flash-test plugin fixture** (`tests/fixtures/test_plugins/flash_test/`):
  exposes commands that reach into flash internals enough for
  assertion (e.g., "log current label assignment to a virtual
  buffer", "report viewport scan count"). Lives only in tests;
  not a real distributed plugin. Mirrors the pattern other
  plugin-feature tests use.
- **`harness.wait_until_screen_matches(regex)`** convenience —
  if it doesn't already exist, adds it; current tests open-code
  the predicate.
- **A "no overlay leak" assertion**: extend the harness with a
  `harness.assert_no_orphan_overlays_in_namespace(ns)` that
  verifies the editor state has no overlays for a namespace
  *after* flash should have cleaned up. This is the cleanup
  invariant test rule #5 needs.

### Tests that should *not* be written

- Anything that asserts on internal flash state via accessors
  (rule #2). Always go through screen output.
- Time-based waits (rule #3). `wait_until` only.
- Tests that only run when a real terminal is attached. Use the
  fake terminal harness.
