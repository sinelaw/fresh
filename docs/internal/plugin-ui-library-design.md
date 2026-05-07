# Plugin UI Widget Library — Design

> **Status**: Design Document
> **Date**: May 2026
> **Branch**: `claude/design-plugin-ui-library-pxri8`
> **Scope**: Plugin-facing UI primitives. Does *not* propose new IPC, a
> Webview-style escape hatch, or a non-Rust UI runtime.

## TL;DR

The single biggest UI dependency for first-party plugins is already built —
just not exposed. `crates/fresh-editor/src/view/controls/` contains 8,162
lines of Rust controls (`Toggle`, `TextInput`, `Dropdown`, `Button`,
`NumberInput`, `TextList`, `MapInput`, `DualList`, `KeybindingList`) that the
settings UI uses today. The plugin runtime drives buffer-group panels with
text-property entries; plugins paint `[v]`, `▸`, `[Replace All]`,
column-aligned tables, etc., from raw bytes. The pkg.ts plugin literally has
a `TODO: Plugin UI Component Library` comment at lines 16–29 asking for the
controls to be plumbed through.

This document picks **option (b): targeted helpers**, not a full widget
library, and not status quo. Specifically:

- Ship a small, opinionated TS-side helper library
  (`plugins/lib/widgets.ts`, ~600 LOC) that wraps `TextPropertyEntry`
  building for **6 widgets** that recur in 3+ plugins: scrollable list,
  table, key-value form, checkbox row, button row, and inline text input.
- Introduce a `VirtualBufferBuilder` (~150 LOC) that handles byte-offset
  bookkeeping so plugins stop computing `utf8ByteLength()` deltas by hand.
- Wire plugin `mode_text_input` events into a small `TextInputState`
  helper so search_replace-style "type characters into a field inside a
  panel" stops being a per-plugin reinvention.
- Standardize on **buffer groups** (already shipped — see
  `docs/internal/buffer-groups-design.md`) as the layout primitive. We do
  not add a new layout engine.
- Keep `Popup`, `Prompt`, `Finder`, `showActionPopup` as the v1 primitives
  for transient UI. Rewrite *none* of them.
- Defer (explicitly) a retained widget tree, focus manager owned by core,
  and ratatui-side rendering of plugin widgets. Three concrete plugins
  (search_replace, pkg, theme_editor) drive every decision; nothing here
  is added for hypothetical callers.

The bar to clear: **after this lands, search_replace.ts's hand-rolled
control bar (250 LOC of `inlineOverlays` arithmetic) becomes ~50 LOC of
declarative widget calls, and pkg.ts's `buildPkg{Header,List,Detail,Footer}Entries`
collapse to ~150 LOC total.** No new editor commands, no new IPC, no new
process boundary, no new theming system. Estimated effort: 2–3 weeks for
v1; 1 week to migrate the three named plugins.

The rest of this document defends that recommendation against the
alternatives, names each widget by its callers, and answers the specific
design questions in the brief.

---

## 1. Why this is even a question

Fresh's plugin UI stack has grown features faster than it has grown
abstractions:

```
TextPropertyEntry  ←─ raw line + byte-offset overlays
OverlayOptions     ←─ per-range styling, theme-key-aware
addOverlay         ←─ post-hoc styling on a byte range
defineMode         ←─ buffer-local keybindings + allowTextInput
showActionPopup    ←─ choose-from-list popup
Popup (Rust)       ←─ richer scrollable popup, internal-only API today
Prompt (Rust)      ←─ minibuffer or floating overlay text input
Finder<T> (TS)     ←─ search/filter/livePanel orchestration on Prompt
BufferGroup        ←─ N panels appearing as one tab + auto scrolling
CompositeBuffer    ←─ aligned multi-pane diff view
ScrollSyncGroup    ←─ anchor-based synced scroll for split pairs
```

The high-level helpers (`Finder`, `BufferGroup`, `Popup`, `Prompt`) all
work well on their own and have been adopted by the plugins they were
designed for. The complaint is **not** that primitives are missing — it's
that there's no helper for "I have a panel buffer and I want to draw a
checkbox in it." Plugins fall off the cliff between the high-level
abstractions and `entries.push({ text, properties, inlineOverlays })`.

Three concrete pain points, by name:

- **search_replace.ts:290–544** hand-rolls a 2-line control bar:
  text inputs with a cursor, `[v]/[ ]` checkbox toggles, a `[Replace All]`
  button, a centered separator with an embedded label, a virtual-scrolled
  hierarchical match tree with selection backgrounds, and a help footer.
  ~250 LOC of `inlineOverlays` arithmetic, byte offset bookkeeping, and
  `mode_text_input` plumbing for inline cursor editing. The cursor
  highlight is a manually positioned overlay; the focus model is a
  `FocusPanel` enum interpreted top-to-bottom in the renderer.

- **pkg.ts:16–29** has the explicit TODO:
  > Plugin UI Component Library. The UI code in this plugin manually
  > constructs buttons, lists, split views, and focus management using raw
  > text property entries. (...) The editor's settings UI already
  > implements similar components — these could be unified into a shared
  > framework.
  Lines 2016–2145 hand-roll header (search input + filter buttons + sync
  button), 2-section installed/available list with `▸` selection prefix,
  detail panel with action buttons, and footer help. Each of those is a
  separate `build*Entries` function and they all share the same
  focus-state-encoding-via-bracket-characters pattern.

- **theme_editor.ts:1399–1442** has tree-panel + picker-panel + footer
  builders. Selection styling lives in `applySelectionHighlighting` /
  `styleForLeftEntry` (~150 LOC together). The right-panel color picker
  rows are bespoke.

We have ample evidence (8 plugins, ~14k LOC of plugin code) of what
plugins actually need; we don't need to invent a category and hope.

## 2. What already exists (so we don't re-invent it)

Citations are file:line into the current tree.

### 2.1 Rust controls library — `crates/fresh-editor/src/view/controls/`

```
controls/
├── button/         207 LOC input + 232 mod + 121 render
├── dropdown/       297 + 643 + 203
├── dual_list/      577 + 212
├── keybinding_list/  204 + 337 + 190
├── map_input/      265 + 559 + 287
├── number_input/   374 + 828 + 224
├── text_input/     264 + 516 + 198
├── text_list/      254 + 491 + 156
└── toggle/         186 + 221 + 116
                                           Σ 8,162 LOC
```

`crates/fresh-editor/src/view/controls/mod.rs:1` documents the pattern:

> Each control follows a consistent pattern:
> - `*State` struct containing the control's data
> - `*Colors` struct for theming
> - `render_*` function that renders to a frame and returns hit areas

Each control already has `FocusState` (`Normal`/`Focused`/`Hovered`/
`Disabled`), `from_theme()` color derivation, and a `*Layout` struct with
`hit_test()` for mouse routing. They are **used today** by
`crates/fresh-editor/src/view/settings/render.rs` (3,682 LOC) — every
toggle, dropdown, and number input the user sees in the settings panel
goes through them.

Plugins cannot call any of this. Settings doesn't go through
`TextPropertyEntry` — it renders directly to a ratatui `Frame` from
`view/settings/render.rs`. There's no plugin command that says "draw a
toggle at this row of this virtual buffer."

### 2.2 Existing extraction work — `docs/internal/UNIFIED_UI_FRAMEWORK_PLAN.md`

A 1,702-line plan from earlier in 2026. Steps 1–7 (point_in_rect,
FocusManager, MenuLayout, TabLayout) **already shipped** (`Status: ✅` in
that doc, lines 1207–1218). The plan's Part 2 ("TypeScript Mirrors of Rust
Controls", lines 304–488) is the part still unbuilt — and it's exactly
what this doc scopes.

We pick up where that plan stopped. Key alignment with it:

- It also concludes "no virtual DOM, no reactive bindings, no constraint
  solver" (line 1148–1156). Same.
- Its `VirtualBufferBuilder` (line 490+) and our `VirtualBufferBuilder`
  are the same idea. We adopt it.
- It proposes `ButtonControl`, `ListControl`, `FocusManager` in TS that
  *mirror* the Rust shapes. We narrow that to the 6 widgets the survey
  shows actually recur.

We do **not** repeat its Rust-side extraction work. v1 here is purely the
plugin-facing layer.

### 2.3 Plugin lib — `crates/fresh-editor/plugins/lib/`

| File | LOC | Purpose | Adoption (8 large plugins) |
|---|---|---|---|
| `fresh.d.ts` | 2,350 | Generated TS API surface (~150 methods) | All (auto) |
| `finder.ts` | 1,560 | Search/Filter/LivePanel picker UX | 1/8 (pkg only, legacy install flow) |
| `git_history.ts` | 596 | Git log/blame helpers | audit_mode |
| `search-utils.ts` | 343 | Fuzzy match + grep parsing | finder.ts internal |
| `panel-manager.ts` | 214 | Open/close/update virtual-buffer panel lifecycle | **0/8** |
| `navigation-controller.ts` | 214 | Up/Down navigation in panels | **0/8** |
| `virtual-buffer-factory.ts` | 141 | createVirtualBuffer wrappers | imported by audit_mode but bypassed for actual `createVirtualBuffer` calls |
| `types.ts` | 86 | Shared types (Location, RGB, …) | Stable |
| `index.ts` | 46 | Barrel | Stable |

`finder.ts` is the only large helper today. Its scope is **transient
"find and navigate" workflows** — `prompt()` for live search,
`panel()` for static results, `livePanel()` for reactive providers
(diagnostics). It is *not* a widget library; it knows about Prompt,
Suggestion, preview pane, and file location, full stop. Don't extend it
to cover panel-side widgets — its model is one window of results, and
plugins like search_replace and pkg need many widgets in one buffer.
Adding widgets to Finder would entangle two responsibilities. Keep
Finder; ship widgets next to it.

**Adoption-failure lesson.** `panel-manager.ts` and
`virtual-buffer-factory.ts` are textbook cases where a wrapper exists
but no plugin uses it. search_replace and merge_conflict are perfect
PanelManager candidates and re-roll its job by hand
(search_replace.ts:31–60 tracks `panel.resultsBufferId` /
`sourceSplitId` manually; merge_conflict.ts:1325–1388 manually
orchestrates `createVirtualBuffer` + `distributeSplitsEvenly`).
devcontainer.ts:91–125 even invents its own "single shared panel slot"
helper because it didn't notice PanelManager existed. The widget
library must not repeat this. Two implications:

1. The widgets must obviously cover the painful bits the plugin
   *was already going to write* — not bits that look generic but
   require the plugin to restructure its state to use them. This is
   why widgets emit `RowCells` / `TextPropertyEntry[]` (the shape
   plugins are already producing) instead of, say, a retained
   `ListController.attach(buffer)` shape.
2. The library's first migrations must be visibly net-negative LOC
   in plugins that authors maintain (pkg, search_replace,
   theme_editor). If those land and the LOC reduction is real,
   subsequent plugins will follow. If they don't, the widgets
   become the new PanelManager.

### 2.4 Buffer groups — `docs/internal/buffer-groups-design.md`

Ship-day primitive: `editor.createBufferGroup({ layout, ... })` builds a
real split tree of real buffers under one tab, suppressing chrome.
Already in production use by `audit_mode.ts` (review-mode), `pkg.ts`
(packages list + detail), `theme_editor.ts` (tree + picker + footer).
This is our layout primitive. We do not propose a flexbox/CSS-grid
abstraction over it.

The plugin-facing API is already adequate: layout JSON describes
`scrollable` panels, `fixed` headers/footers, and `split` containers
with ratios. `editor.setPanelContent(groupId, panelName, entries)` is
how content moves into a panel. Buffer-group focus cycling already
exists (`editor.focusBufferGroupPanel`).

### 2.5 Popup, Prompt, defineMode

- `Popup` (`crates/fresh-editor/src/view/popup.rs:228`) supports
  `PopupKind::Completion / Hover / Action / List / Text`,
  `PopupPosition::AtCursor / Centered / CenteredOverlay / BottomRight /
  AboveStatusBarAt / Fixed`, scroll, text selection, list items with
  optional icons + detail. Plugins reach this through:
  - `editor.showActionPopup({ id, title, message, actions })` — used by
    30+ plugins (LSP installers, devcontainer, code-tour, etc.) for
    short choose-from-list dialogs. The `action_popup_result` event
    delivers the chosen `actionId`.
- `Prompt` (`view/prompt.rs:188`) is the minibuffer or floating-overlay
  text input: `editor.startPrompt(label, customType, floatingOverlay?)`,
  with `setPromptSuggestions(suggestions[])` and the
  `prompt_changed`/`prompt_confirmed`/`prompt_cancelled` event triple.
  Live grep, command palette, theme picker, file picker — all driven
  by this.
- `defineMode(name, bindings, readOnly?, allowTextInput?, inheritNormal?)`
  + the `mode_text_input` event (`buffer_mode.rs:18`) is what
  search_replace.ts uses to do *inline* text entry inside a panel
  buffer. With `allowTextInput=true`, unbound printable characters
  arrive as `mode_text_input` events (one per character). This
  primitive is **strictly necessary** for in-panel text inputs — there
  is no other way for a panel to consume keystrokes without binding
  every printable character explicitly.

These are good. The widget library does **not** redo any of them. v1
calls `Popup`/`Prompt` for modal-ish UI and `defineMode + mode_text_input`
for in-panel input.

### 2.6 What's not relevant

- `CompositeBuffer` (3-way diff) — a specialized layout for synced
  scrolling across panes; orthogonal.
- `ScrollSyncGroup` — anchor-based scroll synchronization; orthogonal.
- `setFileExplorerDecorations` — file-tree symbols; orthogonal.

## 3. Survey — what plugins build by hand

Methodology: for each plugin we list the UI patterns it constructs from
`TextPropertyEntry`/`InlineOverlay`/`addOverlay` primitives, with file
citations. The "Plugins" column counts *first-party* plugins (in
`crates/fresh-editor/plugins/`) using each pattern. Anything that doesn't
appear in 3+ plugins is a one-off and out of scope for v1.

### 3.1 Patterns recurring in 3+ plugins

Counts are over the 8 surveyed plugins (audit_mode, theme_editor,
devcontainer, pkg, dashboard, markdown_compose, merge_conflict,
search_replace).

| # | Pattern | Plugins (count) | Where | Approx LOC built by hand |
|---|---|---|---|---|
| 1 | **Selection-highlighted scrollable list** (`▸`/`>` prefix, current-row bg, header label, manual scroll-clamp) | audit_mode, theme_editor, pkg, search_replace, merge_conflict, dashboard (focusedIndex variant) | audit_mode.ts:1308–1395 / 4469–4488; theme_editor.ts:847–911; pkg.ts:2043–2090; search_replace.ts:463–530; merge_conflict.ts:488–510; dashboard.ts:808–834 | 80–400 each, ≈1,800 across 6 plugins |
| 2 | **Section header / title row** (uppercase or bold, often `extendToLineEnd` with reverse-video bg) | audit_mode, theme_editor, pkg, devcontainer, dashboard, merge_conflict, search_replace | audit_mode.ts:1313–1322; theme_editor.ts:852–855; pkg.ts:2050; devcontainer.ts:280–402; dashboard.ts:538–552; merge_conflict.ts:620–625; search_replace.ts:411–428 | 5–15 each, but every plugin re-derives the styling |
| 3 | **Key-binding hint / footer** (`[n] next  [p] prev  │  [q] close` — bracketed key + label, theme-keyed colors) | audit_mode, theme_editor, pkg, devcontainer, search_replace, code-tour, merge_conflict (footer-less but has equivalent toolbar) | audit_mode.ts:1068–1125 (≈60 LOC, 2-row wrap-aware); theme_editor.ts:1440–1442; pkg.ts:2136–2145; devcontainer.ts:433–437; search_replace.ts:535–541 | ≈30 each, ≈250 across 8 |
| 4 | **Column-aligned table** (computed widths, optional `─` rule, padded rows) | devcontainer (ports), pkg (installed list rows), audit_mode (toolbar groups, file-summary), dashboard (PR rows), theme_editor (palette grid) | devcontainer.ts:1277–1374; pkg.ts:2050–2080; audit_mode.ts:1086–1124; dashboard.ts:200–520 | 70–200 each, ≈600 |
| 5 | **Bracket-button with focus affordance** (`[ Label ]` focused vs. ` Label ` unfocused) | devcontainer, pkg (action buttons + filter pills), search_replace (Replace All) | devcontainer.ts:416–432; pkg.ts:2125–2127 / 2031–2038; search_replace.ts:396–402 | ≈30 each |
| 6 | **Tab-cycle focus across heterogeneous targets** (each plugin has its own focus enum & cycle helper) | pkg, devcontainer, dashboard, search_replace | pkg.ts:2252–2272 (`FocusTarget` union); devcontainer.ts:596+; dashboard.ts:770–776; search_replace.ts focus enum | ≈40 each |
| 7 | **Checkbox / boolean toggle** (`[v]`/`[ ]` indicator with on/off colors) | search_replace (3 toggles), settings UI (Toggle widget), partial in pkg (filter pills look like radio toggles) | search_replace.ts:368–403; settings UI Toggle | ≈40 in search_replace |
| 8 | **Inline text input with cursor inside a panel** (not a Prompt) | search_replace (real cursor + `mode_text_input`), pkg (display-only mirror that delegates to a Prompt) | search_replace.ts:193–207, 301–367; pkg.ts:2019–2024 + 2452 | ~100 in search_replace |

The first six recur in 3+ plugins, the last two appear in 1–2. We
include #7 and #8 in v1 anyway because:

- **#7 checkbox**: not strictly 3+ today, but `Widgets.checkbox` plus
  `Widgets.button` cover the same bracket-with-focus convention pkg
  uses for filter pills (`[ All ]`/` All `). pkg's filter row is
  rendered with the *same code path* a checkbox row would use. So if
  `Widgets.button` exists, `Widgets.checkbox` is a one-method addition
  that prevents pkg from inventing its own toggle.
- **#8 inline text input**: only one full-fidelity caller
  (search_replace), but the cursor-overlay arithmetic is the most
  delicate code in the whole plugin set, and pkg already wants it (it
  mirrors a prompt instead of doing it inline because it's hard).
  Costing it out: a TextInputState + bind helper costs ~120 LOC; pkg's
  prompt-mirror workaround adds ~30 LOC of its own; once shipped, both
  plugins use the helper.

That gives v1 **6 widgets** (list, table, key-value form, checkbox row,
button row, inline text input) plus the **section-header** primitive
folded into the row-cells builder helpers, plus the **help-footer**
widget. None require a layout engine; all are line-oriented. None are
mouse-only.

### 3.2 Patterns appearing in only one or two plugins (NOT in v1)

| Pattern | Plugins | Disposition |
|---|---|---|
| Box-drawing frame around content (corner glyphs, borders) | dashboard only (renderFrame at dashboard.ts:540–644) | One-off. Skip. |
| Click-action ranges with row-level dispatch | dashboard only (Draw.rowActions) | One-off. Skip. The Popup/Prompt action paths cover the common cases. |
| OSC-8 hyperlink spans inside content | dashboard only | Already supported via `OverlayOptions.url`; no widget needed. |
| Markdown-style table virtual borders | markdown_compose only | A renderer concern; addressed by `addVirtualLine`. Skip. |
| Hierarchical tree with expand/collapse | search_replace (file-group tree) | Borderline. v1 doesn't ship; survivable as a List with a `prefix` callback that builds the tree glyphs. Revisit in v2 if a second plugin needs it. |
| Two-row toolbar with grouped key hints | audit_mode only | Survivable as a styled help-footer call with multiple groups. We expose the helper. |
| Inline diff highlights inside row text | search_replace match rows, audit_mode comments | Already a string of `inlineOverlays`. Not a widget. |
| Color-swatch palette rows | theme_editor only | One-off. Skip. |
| Severity icons (`[E]`/`[W]`/`[I]`/`[H]`) | finder.ts panel mode (used by diagnostics_panel, find_references) | Already in finder.ts. Skip. |
| Modal "are you sure?" dialog | many — ALL via `editor.showActionPopup` | Already covered. Skip. |
| Tab strip across the top of a panel | none observed in first-party plugins | No demand. Skip. (Buffer groups already give one tab per group; nothing under a group needs sub-tabs.) |
| Radio group | none observed in first-party plugins | No demand. Skip. |

The bar held: only widgets that 3+ plugins build by hand make it into v1.
That gives us 6 widgets, not 12. The "suggested floor" in the brief
included tab strip, radio group, modal dialog — the survey doesn't
support adding any of them.

### 3.3 Buffer-group adoption is uneven, but not the bottleneck

Of the 8 surveyed plugins:

| Plugin | Uses BufferGroup? | Why / why not |
|---|---|---|
| audit_mode | ✅ (3-pane review-diff at line 3723; 2-pane review-branch at 4567) | Best example of buffer-group fit. |
| theme_editor | ✅ (line 2675) | Tree + picker + footer fits cleanly. |
| pkg | ✅ (line 2204) | Header + list + detail + footer. |
| devcontainer | ❌ | Rolled its own panel-slot helper at lines 91–125 (independent reinvention of PanelManager). |
| dashboard | ❌ | Single buffer with custom Draw frame; no multi-pane needs. |
| markdown_compose | ❌ | Buffer post-processor, not a UI plugin. |
| merge_conflict | ❌ | Three panes managed manually via `createVirtualBuffer` + `distributeSplitsEvenly` (lines 1325–1388). Strong BufferGroup candidate. |
| search_replace | ❌ | Single panel + control bar; hand-rolled. Could use a BufferGroup but doesn't need to since it's one logical pane. |

**This is fine.** Per-widget renderers don't depend on the buffer
hosting them being a buffer-group panel, a stand-alone virtual buffer,
or a Finder panel. The widget library composes with all three. (The
two BufferGroup-candidates that don't use it — merge_conflict and
devcontainer — would benefit from migration, but that's an
orthogonal cleanup tracked elsewhere.)

### 3.4 What the high-level helpers do not cover

- **Finder** covers the prompt + selection + preview workflow. It
  doesn't help when a panel needs to host *multiple* widgets at once (a
  search input AND a checkbox row AND a result list AND a footer).
  Three plugins (search_replace, pkg, theme_editor) need the latter
  shape; that's the gap.
- **BufferGroup** gives layout + scroll, but the panels still get
  `TextPropertyEntry[]` from the plugin and the plugin still hand-builds
  every cell. Buffer groups solved layout/scroll; they did not solve
  per-cell rendering.
- **showActionPopup** covers transient "choose one of N actions"
  perfectly. It doesn't host text input or persistent state.

The widget library fills a specific gap: **per-row content building
inside a buffer-group panel.**

## 4. The decision: (a) library, (b) targeted helpers, or (c) status quo

The brief mandates picking one. I pick **(b) targeted helpers**, sized
to the recurring set. Reasoning:

### (a) Full widget library with layout engine, theming, focus model

Rejected. Specifically:

- **Layout engine**: `BufferGroup` + ratatui `Constraint` already cover
  every observed plugin layout. The survey shows 0 plugins doing nested
  flexbox across more than two axes; theme_editor uses a 2-deep nesting
  (header → splits) which buffer-groups already handle. Adding a layout
  engine is invented demand. Cost: 1500–2000 LOC of solver +
  serialization between TS and Rust.
- **Theming**: already exists. `OverlayColorSpec::ThemeKey(String)`
  resolves at render time; plugins already use `"ui.help_key_fg"` etc.
  A widget library that defined its own colors would defeat user
  theming — exactly the brief's warning. v1 reuses the existing theme
  keys.
- **Focus model owned by core**: doesn't solve a problem. Today every
  panel-with-multiple-widgets plugin has its own focus enum
  (`pkg.focus.type: "search"|"filter"|"list"|"action"|"sync"`,
  `searchReplace.focusPanel: "query"|"options"|"matches"`). They differ
  intentionally — pkg has a sync button, search_replace has options
  toggles. A core focus manager either has to be generic enough to model
  all of them (so it's just a list with a current index, which TS does
  in 20 lines) or it has to be plugin-specific (so it's not a library).
  We ship a 30-line `FocusRing<T>` helper and stop.
- **Migration cost**: rewriting all 8 large plugins as a precondition is
  roughly 8 person-weeks. v1 does not require any plugin rewrite — the
  helpers compose with hand-rolled code, so plugins migrate
  incrementally.

### (c) Status quo + better docs/examples

Rejected. The survey shows ~1,800 LOC of duplicated list-rendering code
across 8 plugins, with subtle inconsistencies that have already produced
bugs (search_replace's truncate logic vs pkg's; inconsistent selection
prefixes `▸` vs `>`). Documentation does not deduplicate code. New
plugins (and the existing pkg.ts TODO) keep asking for the same thing.

### (b) Targeted helpers

Accepted.

- 6 widget helpers, sized to the recurring set
- 1 `VirtualBufferBuilder` for byte-offset bookkeeping
- 1 `TextInputState` helper plumbing `mode_text_input`
- 1 `FocusRing<T>` for cycling between sub-widgets
- All TS-side; no new Rust commands except eventual exposure of theme
  defaults (one method, see §6.5)
- v1 cost ≈ 800 LOC of TypeScript + ≈ 50 LOC of Rust theme exposure
- Migration is opt-in; existing plugins continue working

This matches the bar the brief sets: every feature is tied to a named
plugin, the API surface grows by one TS file, the cost is small, and
the gain is concrete (search_replace.ts:290–544 → ~50 LOC).

## 5. Programming model

The brief asks: immediate-mode (re-emit on each event), retained (mutate
instances), or declarative-with-diff (describe state, runtime diffs).

**Pick: immediate-mode, host-rebuild.**

```ts
// Plugin code, called on every state change:
function rerender() {
  const builder = new VirtualBufferBuilder();
  builder.append(Widgets.header(state.title));
  builder.append(Widgets.textInput(state.searchInput, { focused: state.focus === "search" }));
  builder.append(Widgets.checkboxRow([
    { label: "Case", checked: state.caseSensitive, focused: state.focus === "case" },
    { label: "Regex", checked: state.useRegex, focused: state.focus === "regex" },
  ]));
  builder.append(Widgets.list(state.matches, {
    renderItem: (m) => `${m.file}:${m.line}  ${m.snippet}`,
    selected: state.selectedIndex,
  }));
  builder.append(Widgets.helpFooter([
    { key: "Enter", label: "select" },
    { key: "Esc", label: "close" },
  ]));
  editor.setPanelContent(state.groupId, "main", builder.entries());
}
```

Three reasons:

1. **ratatui itself is immediate-mode.** Every visible frame of fresh is
   already rebuilt from scratch on every tick by ratatui (the Rust TUI
   library Fresh is built on). Trying to ride a retained tree on top of
   that adds bookkeeping without a payoff.

2. **`setVirtualBufferContent` and `setPanelContent` are atomic
   replace-all calls.** They take `entries: TextPropertyEntry[]` —
   nothing in the editor command channel today is *delta*-shaped for
   panel content. (Compare: overlays *are* delta-shaped via `addOverlay`/
   `clearNamespace`, but plugins keep getting that wrong, see
   `dashboard.ts:33–41` in the comment about "single atomic command"
   to avoid frame-slip.) Forcing a retained tree on plugins would
   require either inventing a panel-diff command or doing the diff in
   TS and then sending the full replace anyway. The latter is a
   pessimization; the former is a new IPC the brief forbids.

3. **QuickJS dispatch is the bottleneck, not allocation.** Plugin
   commands are JSON-serialized over an MPSC channel
   (`PluginCommand` in `crates/fresh-core/src/api.rs:1052`). The cost
   is dominated by serializing the entries vector and the QuickJS
   re-entry, not the JS-side allocation of TextPropertyEntry objects.
   A retained model wouldn't reduce the serialized payload because
   QuickJS still has to ship the new content.

Trade-offs (named explicitly):

- Immediate-mode rebuild of a 1,000-row list is wasteful when only
  one row's selection changed. Counter: `setPanelContent` already does
  the work, and the bottleneck under load is the editor's render
  pipeline, not TS allocation. Where this matters (matches list
  growing to 10,000 rows in search_replace), the plugin already
  virtual-scrolls (search_replace.ts:430–525) — only visible rows are
  built. The widget library inherits that pattern: `Widgets.list` only
  builds the visible window.
- Retained UI is more familiar to web-frontend devs. Counter: this is
  a TUI editor's plugin layer, not a web framework. Fitting in with
  ratatui's mental model is more important than fitting in with React's.

One nuance: **per-widget state lives on the plugin's side** (e.g. the
`TextInputState` for the search input, the scroll offset of a list).
That state is *retained* on the plugin's heap. What's immediate-mode is
the *render output* per tick. This matches how the Rust controls work
(`ToggleState` is retained; `render_toggle` is immediate).

## 6. The widgets

For each widget: signature, named callers, what they build by hand
today, and v2/deferred items.

### 6.1 `Widgets.list<T>`

```ts
interface ListOptions<T> {
  items: T[];
  selectedIndex?: number;          // -1 for none
  scrollOffset: number;            // plugin-owned; widget reads/writes via state arg
  visibleRows: number;             // panel height minus chrome
  renderItem(item: T, ctx: RenderCtx): RowCells;
  groupBy?: { headerFor(item: T): string | null };  // simple grouping
  emptyText?: string;
  width: number;                   // viewport width — required; widget pads to it
}

interface RowCells {
  // plugin returns one or more strings + optional inline overlays;
  // widget joins them, applies selection bg, and emits a TextPropertyEntry.
  text: string;
  overlays?: InlineOverlay[];
  properties?: Record<string, unknown>;  // forwarded to TextPropertyEntry
}
```

Returns a `WidgetEntries` (a `TextPropertyEntry[]` plus a
`lineToItemIndex: Map<number, number>` so the plugin can wire up
cursor-line → item-index navigation).

Callers (today):
- search_replace.ts:430–525 (file groups + matches, virtual-scrolled)
- pkg.ts:2050–2080 (installed + available, with section headers)
- theme_editor.ts:1399–1418 (theme tree)
- audit_mode.ts (comment list, log list)
- dashboard.ts (PR list, branch list)
- finder.ts:1224–1296 — already has its own copy, keep it (used in
  prompt context, scope is different)

What plugins build by hand: `entries.push({ text: prefix + padded + status + "\n", style: isSelected ? { bg } : undefined, inlineOverlays })`, plus a manual `lineToItemIndex` map. Approx 80 LOC each.

Open question, deferred: **does the widget own the buffer cursor?**
Today plugins keep their own `selectedIndex` and use `setBufferCursor`
to nudge the user-visible caret. The widget could update the cursor on
the plugin's behalf, but `setBufferShowCursors` is per-plugin policy
(some panels show the caret, some don't). v1: plugin owns cursor; widget
returns line-index mapping.

### 6.2 `Widgets.table`

```ts
interface TableOptions<R> {
  columns: { label: string; width?: number | "auto" | "flex" }[];
  rows: R[];
  cellFor(row: R, columnIndex: number): { text: string; style?: Partial<OverlayOptions> };
  showHeader?: boolean;
  showRule?: boolean;       // ─ between header and rows
  selectedIndex?: number;
  totalWidth: number;
}
```

Computes `auto` widths from the longest cell-or-header per column,
distributes leftover space across `flex` columns. Pads each cell. Emits
a header row, optional `─` rule, and one entry per data row.

Callers (today):
- devcontainer.ts:1277–1374 (ports table, with `pad()` helper inlined)
- pkg.ts:2050–2080 (technically a list, but with version + status as
  pseudo-columns; the column logic ends up identical)
- audit_mode.ts (file-summary view)
- dashboard.ts (PR rows)

What plugins build today: ad-hoc `Math.max(label.length, ...values.map(v => v.length))` for each column, then a `headerLine` string by hand,
then a `─`-repeat rule, then per-row formatted strings. ~100 LOC each.

Deferred: column sorting, mouse-resize of column boundaries (no plugin
needs them today).

### 6.3 `Widgets.keyValueForm`

```ts
interface KvFormOptions {
  rows: { label: string; value: string; muted?: boolean }[];
  labelWidth?: number | "auto";
  align?: "left" | "right";
  width: number;
}
```

A 2-column table specialized for `Field: value` rendering. Distinct
from `Widgets.table` because the survey shows it dominates settings-style
content and `width.auto` of one fixed-label column is the only common
case.

Callers (today):
- pkg.ts:2092–2120 (detail panel: name / version / author / license / repository)
- devcontainer.ts:273+ (`buildInfoEntries`)
- merge_conflict.ts (status panels)
- audit_mode.ts (commit-detail header)

LOC saved: ~50 per call site.

### 6.4 `Widgets.checkboxRow` / `Widgets.checkbox`

```ts
interface CheckboxOptions {
  checked: boolean;
  label: string;
  focused?: boolean;
  // theme keys; defaults to ui.toggle_on_fg / ui.toggle_off_fg
  onColor?: OverlayColorSpec;
  offColor?: OverlayColorSpec;
}
function checkboxRow(items: CheckboxOptions[], opts?: { separator?: string }): RowCells;
```

Renders `[v]/[ ]` + label, with focused-state coloring. The row helper
spaces multiple checkboxes with a separator (default `"  "`).

Callers (today):
- search_replace.ts:368–403 (case / regex / wholeWords)
- pkg.ts:2024–2039 (filter pills — they're effectively radio-styled
  checkboxes with `[`/`]` brackets indicating focus, identical render)
- merge_conflict.ts (resolution flags)

LOC saved: ~40 per use.

Open question, **deferred**: should checkboxes be mouse-clickable?
The brief asks about input/focus. The Rust `ToggleState::handle_mouse`
does mouse hit-testing. Plugins today aren't mouse-driven for their
checkboxes (search_replace uses M-c/M-r/M-w; pkg uses Tab+Enter). v1
ships keyboard-only widgets. Mouse routing for plugin widgets is a v2
item; the editor-side plumbing is `mouse_click` events with
`buffer_row` / `buffer_col`, which plugins already receive. Adding
mouse just means each widget includes its line-and-column hit area in
its return value.

### 6.5 `Widgets.button` / `Widgets.buttonRow`

```ts
interface ButtonOptions {
  label: string;
  focused?: boolean;
  disabled?: boolean;
  // optional theme key for the focused-bg; default ui.button_focused_bg
  focusedBg?: OverlayColorSpec;
}
function buttonRow(buttons: ButtonOptions[], opts?: { separator?: string; align?: "left" | "right" }): RowCells;
```

Renders `[ Label ]`-style buttons. When a button is focused, the
background is a theme key, not a hardcoded color.

To make theming work without each plugin guessing colors, we expose
**defaults in the theme registry**. The single Rust change v1 needs:
add `button_focused_bg`, `button_focused_fg`, `button_normal_fg`,
`toggle_on_fg`, `toggle_off_fg`, `help_key_fg`, `help_label_fg` keys to
`Theme` (defaults derived from existing keys). Plugins that pass no
explicit color get these. Plugins that want bespoke (e.g. dashboard's
clickable rows) override.

Why theme defaults are necessary: today plugins hardcode colors like
`button: [80, 140, 220]` (search_replace.ts:80) and `button: [100, 149, 237]` (merge_conflict.ts:127). A widget library that lets each plugin pick its own colors fails the brief's "must integrate with existing theming." Theme defaults solve it.

Callers (today):
- pkg.ts:2122–2128 (action buttons in detail panel)
- search_replace.ts:373–402 (Replace All)
- audit_mode.ts toolbar (key-hint rows act as buttons)

### 6.6 `Widgets.textInput`

```ts
interface TextInputOptions {
  state: TextInputState;       // retained on plugin side
  label?: string;
  width: number;
  focused: boolean;
  placeholder?: string;
}

class TextInputState {
  value: string;
  cursor: number;              // byte offset within value
  // plugin calls these from key handlers (Backspace, Left, Right, Home, End, Delete, mode_text_input)
  insert(s: string): void;
  backspace(): void;
  delete(): void;
  cursorLeft(): void;
  cursorRight(): void;
  home(): void;
  end(): void;
}
```

Returns a single-row `RowCells`: `Label: [value with cursor highlight]`
or `[value]` when unfocused. The cursor cell gets a reverse-video
overlay (theme key `editor.cursor_bg`).

Crucially this widget does **not** subscribe to events itself — that
would couple it to plugin lifecycle. The plugin keeps a `TextInputState`
and pumps it from its own `mode_text_input` handler (see §7).

Callers (today):
- search_replace.ts:200–207 + 301–367 (search and replace inline fields)
- pkg.ts:2019–2024 (search bar — currently no real cursor, just a focus
  bracket; the widget makes adding a real cursor trivial)

Deferred: multi-line input (no first-party plugin needs it). Selection
highlighting (no plugin needs it inside a panel; the Prompt minibuffer
already has it for its primary use case).

### 6.7 `Widgets.helpFooter`

```ts
function helpFooter(
  hints: { key: string; label: string }[],
  opts?: { groups?: { key: string; label: string }[][]; width: number; bg?: OverlayColorSpec }
): RowCells | RowCells[];     // 1-row by default; 2-row if groups don't fit
```

The single most-duplicated UI: the bottom-of-panel keyboard-hint row.
Renders `[key] label  [key] label  │  [key] label  [key] label`. Wraps
to 2 rows if the content doesn't fit, mirroring audit_mode's strategy.

Callers (today):
- audit_mode.ts:1068–1158 (~90 LOC: `buildToolbarRow` + `buildToolbar`)
- theme_editor.ts:1440–1442 (single-line)
- pkg.ts:2136–2145 (single-line)
- search_replace.ts:535–541 (single-line)
- merge_conflict.ts (single-line)
- devcontainer.ts (footer string)
- code-tour.ts (single-line)

LOC saved: ~30 per single-line caller; ~80 for audit_mode's wrap-aware
caller.

This widget specifically replaces audit_mode's `buildToolbarRow` logic
and exposes the wrap-to-two-rows behavior to anyone who needs it.

## 7. The plumbing

### 7.1 `VirtualBufferBuilder`

```ts
class VirtualBufferBuilder {
  append(cells: RowCells | RowCells[]): this;   // adds one or more entries
  appendBlank(): this;
  appendSeparator(opts?: { label?: string; style?: Partial<OverlayOptions> }): this;
  entries(): TextPropertyEntry[];
  // bookkeeping the plugin can read after a build:
  cursorRowFor(itemTag: unknown): number | null;
  byteOffsetForLine(line: number): number;
}
```

This is the single piece that handles UTF-8 byte-offset bookkeeping and
inline-overlay coordinate translation. Plugins today routinely do
`bytePos += getByteLength(piece)` (search_replace.ts has 6 instances of
this; pkg.ts has 4; audit_mode has 14). The builder hides it.

### 7.2 `TextInputState` + `mode_text_input` integration

`mode_text_input` (`buffer_mode.rs:18`) is already wired: when a buffer
mode has `allow_text_input=true`, unbound printable keys arrive as
`PluginAction("mode_text_input:<char>")`. search_replace uses this
today. The library exposes a `bindTextInput(state, panel, isFocused)`
helper that registers a mode handler:

```ts
// In plugin init:
const searchInput = new TextInputState();
const replaceInput = new TextInputState();

editor.defineMode("search-replace-mode", BINDINGS, true, true);
const router = new TextInputRouter();
router.add("search", searchInput);
router.add("replace", replaceInput);

registerHandler("mode_text_input", (args: { text: string }) => {
  const target = router.get(state.focus);
  if (target) { target.insert(args.text); rerender(); }
});

// In key handlers (mapped explicitly by mode bindings):
function search_replace_backspace() {
  const target = router.get(state.focus);
  target?.backspace(); rerender();
}
// (… cursor_left, cursor_right, home, end, delete, similar)
```

The library does *not* try to register the explicit-key bindings for
the plugin — the plugin still calls `defineMode` with its own bindings
and forwards the calls. This keeps key composition with `defineMode`
explicit and avoids a magic global handler that fights other plugins.

### 7.3 `FocusRing<T>`

```ts
class FocusRing<T extends string> {
  constructor(elements: T[], wrap: boolean = true);
  current(): T;
  next(): T;
  prev(): T;
  set(t: T): void;
  is(t: T): boolean;
}
```

Replaces every plugin's hand-rolled focus enum + cycle logic. ~30 LOC.
The brief asks: does Tab between widgets work? **Tab is owned by the
plugin's mode bindings.** The widget library does *not* inject a global
Tab handler — `editor.defineMode` already lets the plugin bind Tab to
its `nav_next` handler, which calls `focusRing.next()`. Composition
with `defineMode` is intentional.

For multi-pane focus (table on left + form on right via buffer-group
panels), the existing `editor.focusBufferGroupPanel(groupId, panelName)`
covers it. No widget-library change needed.

### 7.4 What lives where

```
crates/fresh-editor/plugins/lib/
├── widgets.ts                 NEW  ~600 LOC — the 6 widgets above
├── vbuffer-builder.ts         NEW  ~150 LOC — VirtualBufferBuilder
├── text-input-state.ts        NEW  ~120 LOC — TextInputState, FocusRing,
│                                              TextInputRouter
├── finder.ts                  unchanged
├── panel-manager.ts           unchanged
└── …
```

```
crates/fresh-editor/src/view/theme/    (Rust)
└── add 7 new theme keys + defaults    NEW  ~50 LOC
```

No new `PluginCommand` variants. No new Rust commands. No new
TypeScript bindings emitted by `fresh-plugin-api-macros`. The widgets
compose existing primitives.

## 8. Specific design questions from the brief

### 8.1 Layout system

ratatui's `Layout`/`Constraint` is **not exposed to plugins** today and
**will not be** in v1. Plugins use `BufferGroup` for layout, which is a
constrained subset (`Split { direction, ratio }`, `fixed { height }`,
`scrollable`). The survey shows zero plugins want more — every observed
nesting is at most 2 levels deep (e.g. theme_editor's `header → (tree |
picker)`).

If a plugin somehow needed flexbox-grade layout, the right answer is to
expose more of ratatui's `Constraint::Length / Percentage / Min / Max /
Ratio` through the BufferGroup layout JSON, not invent a parallel
plugin layout language. We do not commit to this in v1; the survey shows
no demand.

### 8.2 Input / focus

- **Tab between widgets**: plugin's `defineMode` binds Tab to a handler
  that calls `focusRing.next()`. Library does not auto-bind Tab.
- **Composition with `defineMode`**: plugin defines the mode once with
  `allowTextInput=true` and binds the special keys (Tab, Enter, Esc,
  Backspace, …). The library exposes `TextInputRouter` so the plugin's
  single `mode_text_input` handler routes to the focused field.
- **Who owns focus state**: the plugin. The library provides
  `FocusRing<T>` as a typed cycle helper; that's it. A core-owned
  focus model would have to encode each plugin's focus shape (search
  panel focus vs. pkg's filter+sync+list+action focus); the survey
  shows the shapes legitimately differ.
- **Multi-pane focus (table left + form right)**: already solved by
  `editor.focusBufferGroupPanel`. Within a panel, `FocusRing` is used.

### 8.3 Theming

All widgets accept theme-key strings (`OverlayColorSpec::ThemeKey`),
which `OverlayOptions` resolves at render time. The library never holds
a hardcoded RGB by default. v1 adds 7 keys to `Theme` (see §6.5) so the
defaults aren't ad-hoc.

User-defined themes already override these via the existing
`getThemeSchema` / `applyTheme` flow. No changes there.

### 8.4 Plugin API surface in `fresh.d.ts`

**No additions to `fresh.d.ts`.** All 6 widgets, `VirtualBufferBuilder`,
`TextInputState`, and `FocusRing` live in `plugins/lib/widgets.ts` (and
sibling files). They are TS classes/functions, not editor commands.
The only Rust-facing change is:

```rust
// crates/fresh-editor/src/view/theme/mod.rs (or wherever Theme lives)
pub struct Theme {
    // … existing fields …
    pub button_normal_fg: Color,
    pub button_focused_fg: Color,
    pub button_focused_bg: Color,
    pub toggle_on_fg: Color,
    pub toggle_off_fg: Color,
    pub help_key_fg: Color,
    pub help_label_fg: Color,
}
```

These don't require a new editor command — `OverlayColorSpec::ThemeKey("ui.button_focused_bg")` already works once the key is registered.

### 8.5 Concrete example: search_replace's match list as a `Widgets.table`

**Today** (search_replace.ts:430–525, abbreviated):

```ts
for (let i = panel.scrollOffset; i < panel.scrollOffset + treeVisibleRows; i++) {
  if (i >= flatItems.length) break;
  const item = flatItems[i];
  const isSelected = focusPanel === "matches" && panel.matchIndex === i;
  if (item.type === "file") {
    const group = fileGroups[item.fileIndex];
    const expandIcon = group.expanded ? "v" : ">";
    const badge = getFileExtBadge(group.relPath);
    const matchCount = group.matches.length;
    const selectedInFile = group.matches.filter(m => m.selected).length;
    const fileLineText = ` ${expandIcon} ${badge} ${group.relPath} (${selectedInFile}/${matchCount})`;
    const fileOverlays: InlineOverlay[] = [];
    const eiStart = byteLen(" ");
    const eiEnd = eiStart + byteLen(expandIcon);
    fileOverlays.push({ start: eiStart, end: eiEnd, style: { fg: C.expandIcon } });
    // ...20 more lines of byte arithmetic per row variant...
    entries.push({
      text: padStr(fileLineText, W) + "\n",
      properties: { type: "file-row", fileIndex: item.fileIndex },
      style: isSelected ? { bg: C.selectedBg } : undefined,
      inlineOverlays: fileOverlays,
    });
  } else {
    // 30 more lines for match rows, including manual highlightMatches() into inlineOverlays
  }
}
```

**With `Widgets.list`**:

```ts
const listOut = Widgets.list({
  items: flatItems,
  selectedIndex: panel.focusPanel === "matches" ? panel.matchIndex : -1,
  scrollOffset: panel.scrollOffset,
  visibleRows: treeVisibleRows,
  width: W,
  renderItem: (item, ctx) => {
    if (item.type === "file") {
      const group = fileGroups[item.fileIndex];
      return ctx.compose(
        ctx.span(group.expanded ? "v" : ">", { fg: "ui.expand_icon_fg" }),
        " ",
        ctx.span(getFileExtBadge(group.relPath), { fg: "ui.file_badge_fg", bold: true }),
        " ",
        ctx.span(group.relPath, { fg: "ui.file_path_fg" }),
        ` (${group.matches.filter(m => m.selected).length}/${group.matches.length})`,
      );
    } else {
      const result = fileGroups[item.fileIndex].matches[item.matchIndex!];
      return ctx.compose(
        ctx.span(result.selected ? "[v]" : "[ ]", {
          fg: result.selected ? "ui.toggle_on_fg" : "ui.toggle_off_fg",
        }),
        ` ${result.match.file}:${result.match.line}  `,
        ctx.highlightSubstrings(result.match.context.trim(), {
          pattern: panel.searchPattern,
          regex: panel.useRegex,
          caseSensitive: panel.caseSensitive,
          style: { bg: "ui.search_match_bg", fg: "ui.search_match_fg" },
        }),
      );
    }
  },
});

builder.append(listOut.entries);
panel.lineToItemIndex = listOut.lineToItemIndex;
```

`ctx.compose` is the only new concept the plugin sees: it joins
`(string | StyledSpan)*` into a single `RowCells` and tracks byte
offsets. `ctx.span(text, style)` returns a `StyledSpan`.
`ctx.highlightSubstrings(text, options)` reuses the regex/literal
highlighting code from `lib/search-utils.ts:343` (it already exists; we
expose it on the ctx). Selection background, prefix, padding to width,
and the `lineToItemIndex` map are handled by `Widgets.list`.

Net: ~50 LOC instead of ~140 LOC, and the next plugin that needs a list
inherits the bug fixes from this one.

### 8.6 Performance — back-of-envelope

Current plugin pipeline (verified by inspecting
`crates/fresh-core/src/api.rs:1052` and the dispatch in
`crates/fresh-editor/src/app/plugin_dispatch.rs`):

```
plugin event → JS handler runs in QuickJS context → emit PluginCommand
            → MPSC send to editor thread → drained next tick (idle/key)
            → editor mutates state → ratatui re-renders affected splits
```

Worst case from the brief: typing in a text input refreshes a 100-row
table at 60 Hz. Per-keystroke:

- TS-side: build 100 `TextPropertyEntry`s + 100×k `InlineOverlay`s.
  Each entry is small (~5 string allocs + array push). Even at 100
  rows × 5 overlays = 500 small JS objects, this is well under 1 ms in
  QuickJS based on existing search_replace + Live Grep observations
  (Live Grep streams 100 results per batch and renders within frame).
- IPC: `setVirtualBufferContent` is one `PluginCommand::SetVirtualBufferContent { entries }` — a single MPSC message, serialized once. Payload size ~5 KB for 100 rows. Channel send is sub-millisecond.
- Editor: re-render of one panel split is ratatui's normal cost. With
  100 visible rows and a few overlays each, well within frame budget.

60 fps is therefore *achievable* but not guaranteed — the editor's
overall tick-rate cap is ~30–60 Hz today and depends on terminal flush
behavior. The widget library does not regress this; it inherits the
existing `setVirtualBufferContent` performance envelope.

What it does *not* do: avoid sending the full 100 rows when one cell
changed. That's a `setVirtualBufferContent`-level concern, not a widget
concern. Plugins that expect to update a single row at >30 Hz today
already bypass full rebuild via `addOverlay` on the existing entry's
byte range; the widget library does not block that — `Widgets.list`
returns the byte ranges of each row.

For perspective on chattiness: the existing CompositeBuffer rendering
re-emits all visible lines on every scroll without observable
slowdown; widgets are not heavier than that.

### 8.7 Testing

Unchanged from existing patterns:

- E2E tests under `crates/fresh-editor/tests/e2e/` use
  `EditorTestHarness::screen_to_string()` (`tests/common/harness.rs:1571`)
  to read rendered output and assert on substrings. This works
  unchanged for widget-library output — the screen contains the same
  characters.
- Widget unit tests: `widgets.ts` is pure TS that takes inputs and
  returns `TextPropertyEntry[]`. Test by importing the function in a
  Deno or node test runner and asserting on the output structure. No
  editor harness needed for the row-shape tests.
- Snapshot tests: `theme_screenshots.rs` already does golden screenshots
  for theme rendering. The same harness can take a snapshot of a
  widget-only buffer; we add fixtures for each of the 6 widgets and
  assert the rendered text and overlay locations.

Specifically, the brief asks if widgets fit the existing harness. They
do — the harness reads the terminal, not the plugin. It does not care
which TS module produced the bytes.

### 8.8 Migration

**No plugin is forced to migrate.** The widget library composes with
hand-rolled code (the panel still receives a `TextPropertyEntry[]`).
Migration story:

| Plugin | When | What changes | Effort |
|---|---|---|---|
| pkg.ts | First (matches the explicit TODO) | `buildPkg{Header,List,Detail,Footer}Entries` → widget calls; ~600 LOC removed; net ~150 LOC of glue | ~3 days |
| search_replace.ts | Second | Control bar + match list become widgets; `mode_text_input` plumbing replaced with `TextInputRouter` | ~3 days |
| theme_editor.ts | Third | Tree panel + footer become widgets; picker stays bespoke (color swatches are out of scope) | ~2 days |
| audit_mode.ts | Optional | Toolbar uses `Widgets.helpFooter`; comment list uses `Widgets.list`; ~150 LOC saved | ~2 days |
| dashboard.ts | Skip | Bespoke Draw model, click ranges, OSC-8 — not a target for widgets | — |
| merge_conflict.ts | Optional | Help footer; status row uses `Widgets.keyValueForm` | ~1 day |
| devcontainer.ts | Optional | Ports panel uses `Widgets.table` | ~0.5 day |
| markdown_compose.ts | Skip | Markdown rendering, not a widget concern | — |

Total migration: ≈10 person-days for opt-in conversions, no big-bang
rewrite. The deprecation story for hand-rolled code is "we don't
deprecate; both shapes coexist forever." The widgets are additive.

### 8.9 Accessibility

Fresh runs in a terminal. The accessibility surface is the rendered
text — what a screen-reader reads when the user navigates over the
panel. Three notes:

- **Cell semantics**: today, screen readers (orca terminal mode, mac
  VoiceOver in Terminal) read cells row-by-row. Widgets that emit
  `[v]` and `▸` and `[Replace All]` are already legible because the
  bracketed glyphs are common AT conventions. The library should not
  switch to Unicode-only glyphs (e.g. `☑`) without ASCII fallbacks.
- **Focus**: the user-visible focus-state today is "this row has a
  bracket" or "this row is reverse-video." Both convey through screen
  readers. The widget library preserves both: `[v]` for state, bg
  highlight for focus, sometimes `▸` prefix.
- **Keybinding hints in footer**: the `Widgets.helpFooter` widget
  emits the hints as plain text (`[n] next`, etc.), which a screen
  reader will read out. This is strictly better than a graphical
  hint — terminal AT users get the same affordance as sighted users.

No changes to the terminal escape sequences emitted; whatever ratatui
emits today is what plugins emit through the widget library.

## 9. Things explicitly **deferred** or rejected

For each one: alternatives, trade-offs, and the rationale for
deferring vs. closing it now.

| Item | Status | Reason |
|---|---|---|
| Retained widget tree in core | Rejected | §5 — adds bookkeeping without payoff against an immediate-mode renderer; doubles serialization cost. |
| Layout engine (flexbox/grid) | Rejected | §8.1 — survey shows 0 plugins need >2 levels of nesting; BufferGroup covers it. |
| Tab strip widget | Rejected | §3.2 — no first-party caller. Buffer groups give one tab per group. |
| Radio group widget | Rejected | §3.2 — no first-party caller; `Widgets.checkboxRow` with mutual-exclusion logic in the plugin is fine for the imagined cases. |
| Modal dialog widget | Rejected | §3.2 — `editor.showActionPopup` covers all observed callers (30+ plugins). |
| Sub-tabs inside a buffer group panel | Rejected | No demand; buffer groups already give logical separation. |
| Mouse routing in widgets | Deferred (v2) | §6.4 — no first-party caller is mouse-driven. Adding it later: each widget already returns the row-and-column hit area in its `RowCells`; v2 wires those through `mouse_click`. The shape is forward-compatible. |
| Hierarchical tree widget (expand/collapse) | Deferred | One first-party caller (search_replace). Survivable as a `List` with a custom `renderItem` that emits `>`/`v` glyphs and a parent-child flat list. v2 if a second plugin asks. |
| Color swatch / palette row | Deferred | One first-party caller (theme_editor). Bespoke code remains; no plugin reuses it. |
| Multi-line text input inside a panel | Deferred | No first-party caller. The Prompt floating overlay covers the only adjacent use case (Live Grep). |
| Selection/copy inside text inputs | Deferred | No first-party caller (Prompt has it; in-panel inputs don't need it yet). |
| Plugin-defined theme keys | Deferred | The current `getThemeSchema` covers user-side; v1 adds 7 well-known keys. v2 could let a plugin register its own. |
| Animation primitives for widgets | Rejected | `editor.animateArea` already handles this for the cases that exist (devcontainer status indicator, dashboard auto-fade). Widgets don't animate themselves. |
| Webview-style escape hatch (HTML in plugins) | Rejected (per brief) | Survey confirms no demand. |
| Cross-plugin widget exports | Deferred | `editor.exportPluginApi` already exists. A plugin that wants to share a widget exports it. We don't formalize until a second plugin imports. |

## 10. Comparison to prior art (briefly)

The brief asks how comparable systems handle this. Short notes only —
each is a section, not a chapter:

- **VS Code Webview API**: full DOM/JS in a separate process. Cost we
  can't pay (the brief forbids new IPC). Lesson: when plugins want to
  draw arbitrary UI, the cost is huge. Stay narrow.
- **Vim popup / Neovim floating windows**: shape is much closer to
  fresh's `Popup` and `Prompt`. Plugins build content from text +
  highlights, exactly like our `TextPropertyEntry`. Confirms the model.
- **Emacs `widget.el`**: retained widget instances with mutation. Fits
  Emacs's display-table model where redrawing is expensive; not a fit
  for ratatui-style frame-rebuilds.
- **Emacs `tabulated-list-mode`**: the closest analogue to what we
  ship as `Widgets.table` + `Widgets.list`. Confirms that
  table-with-fixed-columns-and-selection is a worthwhile primitive.
- **Helix**: pure Rust, `tui` (ratatui's predecessor), no plugin layer
  yet. Their open issue threads explicitly call out wanting a plugin
  widget toolkit; the conclusion mirrors ours (immediate-mode, ratatui
  primitives, narrow scope).
- **ratatui**: immediate-mode, widget trait + `render_ref` / `render`.
  We compose its primitives (Block, Paragraph, List, Table) inside the
  buffer-group panel rendering pipeline; we do not try to expose the
  trait to plugins (they don't have a Rust handle). Lesson: keep TS
  widgets aligned with ratatui's mental model so ratatui can render
  what plugins ask for without translation.
- **Textual (Python)**: retained widget tree, CSS-like styling. Solves
  a different problem (declarative apps without the editor host).
  Their level of investment isn't justified for fresh's plugin layer.
- **egui / Slint / GPUI**: GUI-runtime libraries. Out of scope per
  brief.

The well-trodden decisions: immediate-mode renderer, line-oriented
widget output, plugin-owned focus, theme-key-resolved colors, no
process boundary. The genuinely open decision is **which widgets ship
in v1**, and the brief asks us to size that to the survey, not to a
hypothetical user.

## 11. v1 scope, milestones, exit criteria

**v1 = these 6 widgets + `VirtualBufferBuilder` + `TextInputState` + `FocusRing` + 7 new theme keys.** Nothing else. Not negotiable upward
in v1.

| Milestone | What | Exit criterion |
|---|---|---|
| M1 (3 days) | Land 7 theme keys + defaults; add e2e snapshot | `cargo check`, snapshot diff approved |
| M2 (5 days) | Ship `widgets.ts`, `vbuffer-builder.ts`, `text-input-state.ts`; unit tests for each widget | `npm test` in plugins/lib passes; `check-types.sh` clean |
| M3 (3 days) | Migrate pkg.ts; remove the TODO comment from `pkg.ts:16–29`; update e2e | E2E `pkg_panel.rs` passes; LOC delta ≥ 400 lines removed |
| M4 (3 days) | Migrate search_replace.ts; route input via `TextInputRouter` | E2E `search_replace.rs` passes; the cursor-overlay code in search_replace.ts:557–565 is gone |
| M5 (2 days) | Migrate theme_editor.ts tree + footer (picker stays bespoke); audit_mode toolbar uses `Widgets.helpFooter` | E2E theme_editor screenshot match; audit_mode.rs E2E unchanged |
| M6 (1 day) | Document patterns in `plugins/README.md`; add 1 example plugin showing all widgets | Doc landed; example loads |

≈17 person-days = ~3 weeks for one engineer. No ratatui changes. No new
`PluginCommand` variants. No new generated TypeScript bindings. The
`VirtualBufferBuilder`/widgets land entirely in `plugins/lib/`.

**v1 is a success if**:

- `pkg.ts:16–29` TODO is removed (the comment is the canonical
  callout).
- Net LOC across the three migrated plugins falls by ≥ 600 (target:
  ~800).
- No regressions in any of the existing E2E tests for those three
  plugins.
- A new plugin can render a panel with a search input, a checkbox row,
  a list, and a help footer in <50 LOC.

**v1 is a failure if**:

- Widget library starts growing primitives that no current plugin
  needs (resist the urge — the v2/deferred list is the answer).
- Migration of a single named plugin requires more than 3 person-days.
- Widget output ends up shaped differently from what plugins build
  today (then plugins won't migrate).

## 12. Should we even build it?

Yes. The survey supports it: 1,800+ LOC of duplicated rendering across
8 plugins, with the most-invested plugin (pkg.ts) explicitly asking
for the library, and the existing `UNIFIED_UI_FRAMEWORK_PLAN.md` having
already done the Rust-side prerequisite work. The cost is bounded
(~3 weeks), the API surface is bounded (one TS lib file), and the
migration story is opt-in.

The bar set in the brief — "every feature must be tied to an existing
plugin's pain point, called out by name" — is met by every widget here.

The bar *not* met by a full widget library — "is this worth permanent
expansion of the plugin API surface?" — is the reason we ship targeted
helpers instead.
