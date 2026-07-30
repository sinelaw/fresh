# Code Tour — Utility Dock redesign

> _Design note. Status: **PLANNED** — nothing in this document ships yet. The
> "Today" section describes the code-tour plugin as it behaves at the time of
> writing; everything from "Target design" onward is proposal._

The guided-tour feature (`plugins/code-tour.ts`, driven by a `.fresh-tour.json`
manifest) currently presents each step in a floating **action popup**. This note
records what that actually looks and feels like when you drive it, and proposes
replacing the popup with a **Utility Dock panel** rendered through the plugin
**widget library** — clickable Prev/Next controls, a step rail, and properly
wrapped step prose.

---

## 1. Today

Driven manually in tmux at 160×48 against this repo's own `.fresh-tour.json`
("Fresh Plugin System Tour", 4 steps). Command palette → `Tour: Load
Definition...` → accept `.fresh-tour.json`.

```text
    32 │ //! }                                                                                    ┌Step 1/4: Plugin API Overview──────────────────────────[×]┐
    33 │ //!                                                                                      │## QuickJS Backend Overview                               │
    34 │ //! ## 3. Implement the API Method                                                       │point for the Fresh plugin system. It uses QuickJS, a     │
    35 │ //!                                                                                      │lightweight JavaScript engine, to execute TypeScript      │
    36 │ //! In `JsEditorApi`, use typed parameters for automatic deserialization:                 │plugins.                                                  │
    37 │ //! ```rust,ignore                                                                       │TypeScript and transpiled to JavaScript                   │
    38 │ //! /// Description of what this method does                                              │`JsEditorApi` struct exposes editor functionality to      │
    39 │ //! pub fn my_method(&self, language: String, config: MyConfig) -> bool {                 │plugins                                                   │
    40 │ //!     self.command_sender                                                               │`#[plugin_api]` attribute                                 │
    41 │ //!         .send(PluginCommand::MyCommand { language, config })                          │                                                          │
    42 │ //!         .is_ok()                                                                      │Next →                                                    │
    43 │ //! }                                                                                     │Exit Tour                                                 │
    44 │ //! ```                                                                                   └──────────────────────────────────────────────────────────┘
```

### What is wrong with it

1. **The step text is mangled.** The popup body is a plain 60-column text box
   with no markdown handling and a wrap that drops the head of every wrapped
   line. Step 1's `This is the main entry point for…` renders as
   `point for the Fresh plugin system…`; the `**Key points:**` line vanishes
   entirely; markdown bullets lose their `- ` and their leading words. Step 2's
   ``- `InsertText` - Insert text at a position`` disappears outright, and
   ``- `SetStatus` - Update the status bar`` renders as `Update the status bar`.
   Step 3's fenced `bash` block collapses to a bare
   `write_fresh_dts_file -- --ignored`. Tour prose is *authored* in markdown —
   the schema's `explanation` field is prose with headings, bullets, and code
   fences — and none of it survives.

2. **It covers the code it is talking about.** The popup is pinned bottom-right
   at a fixed 60×15, floating over the very buffer the step is explaining. On a
   160-column terminal it wastes the width it could have had and still occludes
   a third of the visible source.

3. **It steals the keyboard.** While the popup is up, arrow keys move the
   popup's selection instead of the cursor, and `Ctrl+P` does not open the
   command palette. You cannot scroll around the code you are being shown
   without first dismissing the tour.

4. **Dismissing it loses the tour.** `Esc` closes the popup, but the tour is
   still "active" — and now there is *no* on-screen trace of it. No status-bar
   indicator, no panel, nothing. The only way back is the command palette
   (`Tour: Next Step`).

5. **There are no keybindings.** The plugin's own header comment advertises
   "Space/Right (next), Backspace/Left (prev), Tab (resume), Esc (exit)". None
   of those are bound anywhere — not in the plugin, not in the default keymap.
   `Tour: Next Step` / `Tour: Previous Step` / `Tour: Exit` are palette-only,
   gated on the `tour-active` context.

6. **The actions are list rows, not buttons.** `← Previous` / `Next →` /
   `Exit Tour` are rows in a `PopupContentData::List`. They read as a menu, not
   as navigation, and `Enter` silently activates whichever row happens to be
   selected.

7. **The highlight does not render.** `renderStepOverlays` resolves the step's
   line range with `getLineStartPosition` / `getLineEndPosition`, which answer
   for the *active* buffer, and then applies the overlay to the buffer it looked
   up by path. In a terminal capture of step 1 (lines 1–88) no line in the range
   carries the highlight background. Two `await`s land between `openFile` and
   the overlay call, so which buffer is "active" at that moment is not pinned.

8. **Long tours have no map.** There is no way to see where you are in a 20-step
   tour, jump back three steps, or skim what is coming.

Points 1, 3, 4, 6 and 8 are all consequences of the same choice: a transient
notification widget is the wrong container for a persistent, navigable reading
surface.

---

## 2. Why the Utility Dock

The Utility Dock is the editor's existing shared bottom panel — a single
role-tagged split leaf that Diagnostics, Search/Replace, Quickfix and dock
terminals all *swap into* rather than each spawning their own split. It already
provides, for free, everything the tour popup lacks:

- **Persistence with a visible handle.** The dock has a tab bar; a
  `*Tour: <title>*` tab is durable proof the tour is still running, and clicking
  it returns to it. The tab bar holds several, so several tours can be open at
  once (§5).
- **Full width, user-resizable.** Drag the separator; the panel reflows.
- **Focus you can leave and come back to.** `Alt+J` (`Toggle Utility Dock`)
  moves keyboard focus between the dock and the editor. Read the code with the
  panel still on screen.
- **Coexistence.** The tour tab sits beside Search/Replace and Diagnostics
  instead of floating over them.
- **A widget runtime.** Panels mounted in the dock render a `WidgetSpec` tree,
  so buttons, lists and hint bars are host-owned: theming, focus affordance,
  click hit-testing, and virtual scrolling all come from the host.

For reference, this is the dock as Search/Replace uses it today, at the same
160×48:

```text
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 *Search/Replace* ×   +                                                                                                                                      □×
 Search: [WidgetSpec                ]  Replace: [                         ]  (584 matches / 38 files)
 Files: [                                                      ]
 [v] All Files  [ ] Case(Alt+C)  [ ] Regex(Alt+R)  [ ] Whole(Alt+W)                                                                  [ Replace All (Alt+Ret) ]
Tab next  Space include/exclude  Enter open  Alt+Ret replace selected  Esc close
───────────────────────────────────────────────────────────── Matches (584 in 38 files) ──────────────────────────────────────────────────────────────────────
▼ [v] RS crates/fresh-core/src/api.rs (30/30)
    [v] crates/fresh-core/src/api.rs:1702 - // Plugins describe a widget tree as a `WidgetSpec`; the host reconciles the
    [v] crates/fresh-core/src/api.rs:1884 - pub enum WidgetSpec {
```

The tour panel should look like it belongs next to that.

---

## 3. Target design

### 3.1 Anatomy

Five bands, top to bottom:

| Band | Content | Widgets |
|---|---|---|
| **Tab bar** | one `*Tour: <title>*` tab per open tour (host chrome) | — |
| **Header** | tour title · step counter · progress meter · Prev / Next / Exit | `row`, `raw`, `button` |
| **Rule** | full-width separator | `divider` |
| **Body** | Steps rail + step prose | `row` of two `labeledSection`s, each wrapping a `list` |
| **Source line** | file + line range, Jump / Re-highlight | `row`, `button` |
| **Hints** | key legend | `hintBar` |

### 3.2 Wireframes

### A. Wide — 160×48 (the default laptop case)

```text
 File   Edit   View   Selection   Go   LSP   Help
 api.rs ×   quickjs_backend.rs ×   +                                                            □×
   728 │     /// New pane first: left (vertical) or above (horizontal).
   729 │     Before,
   730 │ }
   731 │
▌  732 │ pub enum PluginCommand {          ░░░ step range 732–780 painted with the tour highlight ░░░
▌  733 │     InsertText { pos: usize, text: String },
▌  734 │     AddOverlay { ns: String, .. },
▌  735 │     SetStatus  { message: String },
▌   ⋮   ⋮
▌  779 │     RegisterCommand { name: String, .. },
▌  780 │ }
   781 │
~
~
~
~
~
~
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 *Tour: Plugin System* ×   *Tour: Rendering* ×   *Search/Replace* ×   +                                                                                       □×
 Fresh Plugin System Tour                                                                                Step 2 of 4  ▰▰▱▱   [ ◀ Prev ]  [ Next ▶ ]  [ ✕ Exit ] 
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
╭─ Steps ────────────────────────────────╮╭─ 2/4 · Plugin Command Types ───────────────────────────────────────────────────────────────────────────────────────╮
│  ✓ 1  Plugin API Overview              ││ PluginCommand Enum                                                                                                 │
│  ▸ 2  Plugin Command Types             ││ ──────────────────                                                                                                 │
│    3  TypeScript API Definition        ││                                                                                                                    │
│    4  Example Plugin: Git Find File    ││ Plugins communicate with the editor through typed commands. Each variant of this enum represents an                │
│                                        ││ action a plugin can request.                                                                                       │
│                                        ││                                                                                                                    │
│                                        ││ Common commands:                                                                                                   │
│                                        ││   • InsertText       Insert text at a position                                                                     │
│                                        ││   • AddOverlay       Add visual decorations                                                                        │
│                                        ││   • SetStatus        Update the status bar                                                                         │
│                                        ││   • RegisterCommand  Add to the command palette                                                                    │
╰────────────────────────────────────────╯╰────────────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
 ▸ crates/fresh-core/src/api.rs   lines 732–780                                                                            [ Jump to code ⏎ ]  [ Re-highlight ] 
 n/→ next   p/← prev   ⏎ jump to code   ↑↓ scroll   Tab focus   g steps   q exit
 Trusted  Local  Ln 732, Col 1   Tour: step 2 of 4 — Plugin Command Types                                                     LF  UTF-8  Rust   Palette: Ctrl+P 
```

### B. Medium — dock ≤ 110 cols: the Steps rail folds away (reachable with `g`)

```text
────────────────────────────────────────────────────────────────────────────────────────────────────
 *Tour: Plugin System* ×   *Tour: Rendering* ×   +                                                □×
 Fresh Plugin System Tour                                             2/4 ▰▰▱▱  [ ◀ ]  [ ▶ ]  [ ✕ ] 
────────────────────────────────────────────────────────────────────────────────────────────────────
╭─ 2/4 · Plugin Command Types ─────────────────────────────────────────────────────────────────────╮
│ PluginCommand Enum                                                                               │
│ ──────────────────                                                                               │
│                                                                                                  │
│ Plugins communicate with the editor through typed                                                │
│ commands. Each variant of this enum represents an                                                │
│ action a plugin can request.                                                                     │
│                                                                                                  │
│ Common commands:                                                                                 │
│   • InsertText       Insert text at a position                                                   │
╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
 ▸ crates/fresh-core/src/api.rs:732–780                                                  [ Jump ⏎ ] 
 n next  p prev  ⏎ jump  ↑↓ scroll  g steps  q exit
```

### C. Narrow — 72 cols: header wraps, button row gets its own line

```text
────────────────────────────────────────────────────────────────────────
 *Tour: Plugin Sys…* ×   *Tour: Render…* ×   +                        □×
 Fresh Plugin System Tour                                      2/4 ▰▰▱▱ 
 [ ◀ Prev ]  [ Next ▶ ]  [ ✕ Exit ]
╭─ 2/4 · Plugin Command Types ─────────────────────────────────────────╮
│ PluginCommand Enum                                                   │
│                                                                      │
│ Plugins communicate with the editor through                          │
│ typed commands. Each variant of this enum                            │
│ represents an action a plugin can request.                           │
│                                                                      │
│ Common commands:                                                     │
╰──────────────────────────────────────────────────────────────────────╯
 ▸ api.rs:732–780
 n next  p prev  ⏎ jump  q exit
```

### D. Terminal state — last step

```text
──────────────────────────────────────────────────────────────────────────────────────────────────────────────
 *Tour: Plugin System* ×   *Tour: Rendering* ×   +                                                          □×
 Fresh Plugin System Tour                            Step 4 of 4  ▰▰▰▰   [ ◀ Prev ]  [ Next ▶ ]  [ ✓ Finish ] 
──────────────────────────────────────────────────────────────────────────────────────────────────────────────
╭─ 4/4 · Example Plugin: Git Find File ──────────────────────────────────────────────────────────────────────╮
│ Simple Plugin Example                                                                                      │
│ ─────────────────────                                                                                      │
│                                                                                                            │
│ This plugin demonstrates the core patterns:                                                                │
│                                                                                                            │
│   1. Get editor API with getEditor()                                                                       │
│   2. Define handler functions on globalThis                                                                │
│                                                                                                            │
╰────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
 ▸ crates/fresh-editor/plugins/git_find_file.ts   lines 1–76             [ Jump to code ⏎ ]  [ Re-highlight ] 
 p/← prev   ⏎ jump to code   ↑↓ scroll   Tab focus   g steps   q finish tour
```

### E. Degraded state — step points at a file that is gone

```text
──────────────────────────────────────────────────────────────────────────────────────────────────────────────
 *Tour: Plugin System* ×   *Tour: Rendering* ×   +                                                          □×
 Fresh Plugin System Tour                              Step 3 of 4  ▰▰▰▱   [ ◀ Prev ]  [ Next ▶ ]  [ ✕ Exit ] 
──────────────────────────────────────────────────────────────────────────────────────────────────────────────
╭─ 3/4 · TypeScript API Definition ──────────────────────────────────────────────────────────────────────────╮
│ ⚠  crates/fresh-editor/plugins/lib/fresh.d.ts is not in this working tree.                                 │
│    The tour was recorded at commit a1b2c3d; you are on 4f103dc.                                            │
│                                                                                                            │
│ EditorAPI Interface                                                                                        │
│ ───────────────────                                                                                        │
│                                                                                                            │
│ This TypeScript interface defines all methods available to plugins.                                        │
│                                                                                                            │
╰────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
 ⚠ file not found — step text still readable                                               [ Skip to next ▶ ] 
 n/→ next   p/← prev   ↑↓ scroll   g steps   q exit
```


### 3.3 Why each choice

- **Prose lives in a `list`, not a `raw` block.** The `list` widget owns its
  scroll offset as host instance state and virtualises rows, so a 200-line step
  explanation scrolls inside the panel with `↑↓` / `PageUp` / `PageDown` and no
  scroll arithmetic in the plugin. A `raw` block would push the source line and
  hint bar off the bottom of the dock.

- **Markdown is rendered, not dumped.** The plugin gains a small
  markdown-to-`styledRow` pass over the step's `explanation`, run once per step
  against the section's *inner* width (panel width minus the border and the
  Steps rail):
  - `##`/`###` headings → bold, followed by an underline rule;
  - `-` / `*` bullets → `  • ` with a hanging indent on wrap;
  - `1.` ordered items → preserved numbering, hanging indent;
  - `` `code` `` → the inline-code theme key, backticks stripped;
  - `**bold**` → bold, markers stripped;
  - fenced blocks → indented two columns, code background, fence lines dropped.
  Each output row is one `styledRow` with segments; wrapping happens in the
  plugin because it must be re-run when the dock is resized (`viewport_changed`).

- **The Steps rail is a `list`, not a `tree`.** A tour manifest is flat
  (`steps: TourStep[]`), so a tree's disclosure machinery buys nothing. The rail
  shows `✓` for visited, `▸` for current, blank for unvisited, and a click or
  `Enter` jumps straight to that step — this is the "map" that item 8 above is
  missing. It folds away below ~110 dock columns and stays reachable with `g`.

- **Prev/Next/Exit are real `button`s.** `[ ◀ Prev ]` / `[ Next ▶ ]` render
  through `render_button`, which gives them the shared focused fg/bg, the
  `primary` accent on Next, and — the point of the exercise — host hit-testing,
  so clicking them works. `Prev` is `disabled: true` on step 1 and `Next` on the
  last step, which keeps the row from reflowing as you move through the tour
  (today the popup simply omits the row, so the buttons jump around).

- **Exit becomes Finish on the last step.** `[ ✕ Exit ]` → `[ ✓ Finish ]` with
  `intent: "primary"`, so a completed tour has an affirmative end rather than an
  abandon.

- **The progress meter is `▰▰▱▱`.** Four to sixteen cells scaled to the step
  count, sitting next to `Step 2 of 4`. Legible in a plain capture, no colour
  dependency.

- **The source line is its own band.** The step's `file_path` and line range are
  data the reader wants (and wants to click), not something to bury in the
  prose. `[ Jump to code ⏎ ]` moves focus into the editor split at the step's
  first line; `[ Re-highlight ]` re-applies the overlay after the user has
  wandered off — which is the "detour" case the current `lastKnownTopByte` /
  `lastKnownBufferId` fields in `TourManager` were clearly reaching for and
  never used.

### 3.4 Responsive rules

The panel re-renders on `viewport_changed`; three breakpoints, measured on the
dock's own width:

| Dock width | Layout |
|---|---|
| `≥ 130` | Steps rail (26%) + prose (74%); full button labels; full hint bar |
| `100 – 129` | Rail folds away (`g` opens it as a temporary full-width list); full labels |
| `< 100` | Header title and the counter/button cluster split onto two lines via `wrappingRow`; buttons shrink to `[ ◀ ]` `[ ▶ ]` `[ ✕ ]`; hint bar trims to the four core keys |

Dock height is requested at `ratio: 0.35`. Below ~8 rows of content the body
section drops its border (`labeledSection` → bare `list`) to buy back two rows.

---

## 4. Interaction model

### 4.1 Keys — panel mode (`code-tour-panel`)

Registered with `editor.defineMode("code-tour-panel", bindings, …)`, matching
how Search/Replace binds its dock panel.

| Key | Action |
|---|---|
| `n`, `→`, `Space`, `PageDown`¹ | Next step |
| `p`, `←`, `Backspace`, `PageUp`¹ | Previous step |
| `Enter` | Activate focused widget — on the prose: jump to code; on the rail: go to that step |
| `↑` / `↓` | Scroll prose, or move the rail cursor, depending on focus |
| `Tab` / `Shift+Tab` | Cycle focus: rail → prose → Prev → Next → Exit |
| `g` | Focus the Steps rail (opening it first if folded) |
| `r` | Re-apply the step highlight and re-centre the source view |
| `q`, `Esc` | Exit the tour (closes the dock tab) |
| `Alt+J` | Host builtin — toggle focus between dock and editor |

¹ `PageUp`/`PageDown` step the *tour* only when focus is not on the prose list;
with the prose focused they scroll it, which is the behaviour the widget runtime
already gives for a focused `list`.

### 4.2 Keys — editor context, while `tour-active`

The point of the dock is that you can read the code with the tour still up. So
step navigation must also work from the editor split:

| Key | Action |
|---|---|
| `Alt+]` | Next step |
| `Alt+[` | Previous step |

These bind against the existing `tour-active` context, which the plugin already
sets and which currently gates only the palette entries.

### 4.3 Focus policy

- Loading a tour mounts the dock panel **focused**, with the prose list focused
  inside it, so `n`/`p` work immediately.
- `Enter` on the prose (or the `[ Jump to code ⏎ ]` button) moves focus to the
  editor split at the step's line. The panel stays mounted and keeps showing the
  step. `Alt+J` comes back.
- Advancing a step re-renders the panel but **does not** move focus. If you were
  reading in the editor, `Alt+]` advances the step and re-centres the source
  without yanking you into the dock.
- Closing the dock tab (`×`, `q`, `Esc`) exits the tour: overlays cleared,
  `tour-active` unset. There is no longer an invisible-but-active state.

### 4.4 Events

Everything routes through one `widget_event` listener keyed on the panel id,
the same shape Search/Replace uses:

| `event_type` | `widget_key` | Handling |
|---|---|---|
| `activate` | `prev` / `next` / `exit` | Step navigation / teardown |
| `activate` | `jump` / `rehighlight` | Focus editor at step lines / re-apply overlay |
| `select` | `stepList` | Move rail cursor; `payload.via === "click"` also jumps to that step |
| `activate` | `stepList` | Jump to the selected step |
| `select` | `proseList` | Cursor move only — prose rows are not actionable |

---

---

## 5. Multiple tours — one buffer per tour

A tour is a **virtual buffer**, not a singleton panel. Loading a second manifest
opens a second buffer, and because both carry `role: "utility_dock"` the dock
leaf shows them as sibling tabs:

```text
 *Tour: Plugin System* ×   *Tour: Rendering* ×   *Search/Replace* ×   +                                                                                       □×
```

This is not a new capability — it is how the dock already behaves. Verified by
opening Search/Replace and Diagnostics in one session:

```text
 *Search/Replace* ×   *Diagnostics* ×   +                                                                                                                    □×
```

The dock dispatcher's fast path creates a fresh buffer and calls
`set_pane_buffer` on the existing dock leaf, which adds it as a tab and makes it
active. Nothing swaps a previous tour out.

### What this forces in the plugin

The current plugin holds exactly one tour in a module-level `tourManager`
singleton. That becomes a map, and four things that are implicitly global today
become per-tour:

| Concern | Today | Multi-tour |
|---|---|---|
| Tour state | one `tourManager` object | `Map<tourId, TourInstance>`, plus `bufferId → tourId` and `panelId → tourId` indexes |
| Overlay namespace | one constant `"code-tour"` | `"code-tour:<tourId>"` — so closing one tour cannot clear another's highlight, and two tours may highlight the same buffer at once |
| `clearTourOverlays` | clears the shared namespace on **every** open buffer | clears only that tour's namespace |
| `tour-active` context | set on load, unset on exit | set while **any** tour is open, unset when the last one closes |

The panel mode (`code-tour-panel`) stays a single shared mode — modes are keyed
by name, not by buffer. Every handler resolves its instance from
`editor.getActiveBufferId()`, and the `widget_event` listener resolves its
instance from `args.panel_id`, so both paths land on the right tour without the
mode needing to know how many exist.

### Tab naming and de-duplication

- The tab is named from the manifest: `*Tour: <manifest.title>*`, truncated to
  the dock's per-tab cap. Two manifests with the same title get a disambiguating
  suffix (`*Tour: Rendering (2)*`).
- The tour id is the **resolved absolute manifest path**. Loading a manifest
  that is already open focuses its existing tab instead of opening a duplicate —
  the plugin must do this itself, because the dock fast path runs before the
  `panelId` de-duplication path and will happily mint a second buffer.
- `buffer_closed` on a tour buffer tears down that tour only: clear its overlay
  namespace, unmount its `WidgetPanel`, drop it from the map, drop it from
  persisted state.

### Which tour do the editor-context keys drive?

`Alt+]` / `Alt+[` (§4.2) fire from the editor split, where there is no tour
buffer to read. They target the **most recently active tour**: a `lastTourId`
updated on any panel interaction and on `buffer_activated` for a tour buffer.
With one tour open that is simply "the tour". With several, it is the one whose
tab you last touched — which is also the one whose highlight is on screen.

### Side effect worth having

Because each tour is a real buffer, the existing tab-drag machinery applies: a
tour tab can be dragged out of the dock into an ordinary split. Two tours side
by side, or one tour docked and another pinned to a right-hand pane, come for
free — no additional design.

---

## 6. Persistence across restarts

An unfinished tour survives a restart, including its step position.

### Mechanism

Plugin state written with `editor.setWindowState(key, value)` lands on the
session's `plugin_state`, which is serialized into the per-directory workspace
file as `session_plugin_state` and reloaded on restore. It is workspace-scoped,
which is the right granularity: a tour is about *this* checkout.

The plugin keeps one key:

```ts
type PersistedTour = {
  manifestPath: string;   // relative to the workspace root
  step: number;           // current step index
  visited: number[];      // step indices already seen — drives the ✓ column
  railOpen: boolean;      // user's Steps-rail preference for this tour
};

editor.setWindowState("openTours", tours);   // ordered: dock tab order
```

Write-through on every state change — step navigation, rail toggle, open, close.
The writes are small and the API is already a snapshot write-through, so there
is no need to batch them; a crash then costs at most the step you were on.

### Restore

`editor.on("ready", …)` is the restore point. The `ready` lifecycle hook fires
**after** the workspace restore and after initial buffers are opened, so
`getWindowState("openTours")` already reflects the persisted session by the time
the handler runs. (`plugins_loaded` fires earlier, before restore — too early.)

For each entry, in stored order:

1. Re-read and re-validate the manifest. If it is gone, unparseable, or its
   `schema_version` no longer matches, skip it, drop it from the persisted list,
   and say so once on the status bar — one line naming the tours dropped, not
   one popup per tour.
2. Re-check `commit_hash` against `HEAD`. Drift is normal after a restart, so it
   is surfaced the same way as at load time: the dim `recorded at … · you are on
   …` line in the header, not a blocking warning.
3. Mount the dock tab and render the stored step, restoring `visited` so the
   rail's `✓` column is right.
4. **Do not** open the step's source file, move the editor viewport, or take
   focus. Restoring a tour must not fight the workspace restore for what the
   editor is showing. The step highlight is applied only if the step's file
   happens to be among the restored buffers; otherwise `[ Re-highlight ]` is
   emphasised and applies it on demand.

The last-active tour's tab is the one left selected in the dock.

### Interaction with restore settings

No special handling is needed for `editor.restore_previous_session = false`,
`--no-restore`, or a first run in a directory: in each of those cases the
workspace is not applied, so `session_plugin_state` is never loaded and
`getWindowState` returns `undefined`. Tours simply do not come back, which
matches what the flag promises for tabs and splits. `--restore` brings them back
for the same reason.

Daemon mode needs nothing extra either — detach/reattach never tears the session
down, so the tour buffers are still mounted when the client reconnects.

---

## 7. States

Beyond the steady state, five cases the panel must render (D and E are
wireframed above):

- **First step** — `[ ◀ Prev ]` disabled, rail shows `▸ 1`.
- **Last step** — `[ Next ▶ ]` disabled, Exit becomes `[ ✓ Finish ]`.
- **Missing file** — the step's `file_path` is not in the working tree. Today
  this shows `ERROR: File not found` glued in front of the explanation. Instead:
  a warning band at the top of the prose section naming the file *and* the
  commit drift, the step text still readable below it, and the source band
  offering `[ Skip to next ▶ ]` in place of Jump.
- **Commit drift** — `manifest.commit_hash` does not match `HEAD`. Today this is
  a transient `editor.warn` at load time that scrolls away. Instead it becomes a
  persistent dim line in the header: `recorded at a1b2c3d · you are on 4f103dc`,
  and it is what the missing-file warning cites.
- **Manifest load failure** — bad JSON, wrong `schema_version`, zero steps. No
  dock panel is mounted; these stay `editor.error` on the status bar, as now.

---

## 8. Spec sketch

```ts
panel.set(col(
  // ── header ───────────────────────────────────────────────
  row(
    spacer(1),
    raw([styledRow([{ text: manifest.title, style: { bold: true } }])], "title"),
    flexSpacer(),
    raw([styledRow([
      { text: `Step ${i + 1} of ${n}  ` },
      { text: meter(i, n), style: { fg: "ui.help_key_fg" } },
    ])], "progress"),
    spacer(3),
    button(t("prev"), { key: "prev", disabled: i === 0 }),
    spacer(2),
    button(t("next"), { key: "next", intent: "primary", disabled: i === n - 1 }),
    spacer(2),
    button(i === n - 1 ? t("finish") : t("exit"),
           { key: "exit", intent: i === n - 1 ? "primary" : "normal" }),
    spacer(1),
  ),
  divider({ style: { fg: "ui.separator" } }),

  // ── body ─────────────────────────────────────────────────
  showRail
    ? row(
        labeledSection({ label: t("steps"), widthPct: 26, key: "railBox",
          child: list({ items: railRows, itemKeys: stepKeys,
                        selectedIndex: i, visibleRows: bodyRows, key: "stepList" }) }),
        labeledSection({ label: `${i + 1}/${n} · ${step.title}`, widthPct: 74, key: "proseBox",
          child: list({ items: proseRows, visibleRows: bodyRows,
                        focusable: true, key: "proseList" }) }),
      )
    : labeledSection({ label: `${i + 1}/${n} · ${step.title}`, key: "proseBox",
        child: list({ items: proseRows, visibleRows: bodyRows,
                      focusable: true, key: "proseList" }) }),

  // ── source line ──────────────────────────────────────────
  row(
    spacer(1),
    raw([locationRow(step, fileMissing)], "location"),
    flexSpacer(),
    fileMissing
      ? button(t("skip"), { key: "next" })
      : row(button(t("jump"), { key: "jump" }),
            spacer(2),
            button(t("rehighlight"), { key: "rehighlight" })),
    spacer(1),
  ),

  // ── hints ────────────────────────────────────────────────
  hintBar(hintsForWidth(dockWidth)),
));
```

Mount is the standard dock recipe — the same call Search/Replace makes, with
`role: "utility_dock"` doing the swap-into-the-shared-leaf routing:

```ts
const { bufferId, splitId } = await editor.createVirtualBufferInSplit({
  name: "*Tour*",
  mode: "code-tour-panel",
  role: "utility_dock",     // shared bottom dock, not a new split
  readOnly: true,
  editingDisabled: true,
  showLineNumbers: false,
  showCursors: false,
  scrollable: false,        // the lists own their scroll
  ratio: 0.35,
});
panel = new WidgetPanel(bufferId);
```

---

## 9. What changes in `code-tour.ts`

| Area | Change |
|---|---|
| `showStepPopup` | Deleted. Replaced by `renderPanel()` building the spec above. |
| `action_popup_result` listener | Deleted. Replaced by a `widget_event` listener. |
| `TourManager` | Singleton → `Map<tourId, TourInstance>` keyed by resolved manifest path, with `bufferId → tourId` and `panelId → tourId` indexes (§5). `dockBufferId` / `dockSplitId` / `contentBufferId` / `contentSplitId` are declared today and never assigned — per instance they become real, alongside `widgetPanel`, `visited: Set<number>`, `railOpen`, `dockWidth`. |
| `TOUR_NAMESPACE` | Constant → `code-tour:<tourId>` per instance, so one tour's teardown cannot clear another's highlight. `clearTourOverlays` narrows to the instance's own namespace. |
| Persistence | New: `setWindowState("openTours", …)` write-through on every step/rail/open/close change, and an `editor.on("ready", …)` restore pass (§6). |
| `buffer_closed` | New listener — tears down the one tour whose buffer closed; unsets `tour-active` only when the last tour goes. |
| `renderStepOverlays` | Fix the buffer mismatch: resolve line positions against the step's buffer id rather than whatever is active after the intervening `await`s, so the highlight actually paints. |
| Mode + keys | New `code-tour-panel` mode with the table in §4.1; `Alt+]` / `Alt+[` in the `tour-active` editor context. |
| Markdown | New `renderExplanation(md, width): TextPropertyEntry[]`. |
| i18n | New `code-tour.i18n.json` — the plugin currently hardcodes every user-visible string, unlike its neighbours. |
| Detour tracking | `lastKnownTopByte` / `lastKnownBufferId` get their intended use: they drive whether `[ Re-highlight ]` is emphasised. |

Nothing in the host changes. Every widget the design uses — `button`,
`list`, `labeledSection`, `divider`, `hintBar`, `spacer`, `flexSpacer`,
`styledRow` — already ships, and the dock already accepts a widget panel via
`role: "utility_dock"`.

---

## 10. Open questions

Two earlier questions are now settled and folded into the design: an unfinished
tour **does** persist across restarts (§6), and each tour **is** its own buffer
so several can be open at once (§5). What remains:

1. **Should `Enter` on the rail also move focus to the editor**, or only change
   the step? Proposal above keeps focus in the panel; jumping is `Enter` on the
   prose or the explicit button.
2. **Cap on restored tours.** A workspace could accumulate a dozen persisted
   tours and restore a dock full of tabs. Restore all of them, cap at the N most
   recent, or restore only the last-active one and list the rest behind a
   command?
3. **Authoring.** Nothing in the editor writes `.fresh-tour.json`. A "record
   step from selection" command would make the dock panel a two-way surface, but
   that is a separate feature.
