# The web protocol: the display list and pane rows, not cells

> _Design note. Status: **PLANNED** — nothing here ships yet. It answers
> "can the web UI protocol carry the higher-level layout tree instead of the
> low-level cells the terminal draws?" The answer is yes for everything the
> `fresh-ui` tree describes, and "rows, not a tree" for the text panes. The
> tree must be sent **after** layout, never before it. Companion to
> [web-ui.md](web-ui.md) (the frontend as built, and its gap list) and
> [retained-mode-ui.md](retained-mode-ui.md) (the tree the protocol would
> carry, whose "The web" entry this design carries out)._

---

## 1. What the protocol carries today

The bridge's scene mixes three levels of abstraction, one per stage of the
web frontend's history:

1. **Hand-written semantic projections** — the `Scene` views on `Editor`
   (`menu_view`, `status_view`, `palette_view`, `popups_view`,
   `file_explorer_view`, `file_browser_view`, `settings_view`,
   `keybinding_editor_view`, …). Each is a serialisable model with its own
   hand-written JS builder on the frontend. Together they are a few thousand
   lines on each side, all kept in step with the TUI by the scene-parity test.
2. **The display list, for plugin panels only.** `Editor::tree_view` ships the
   dock, floating panels and sidebar sections as the display-list items the
   tree painted for them, and the frontend's tree fold turns them into DOM with
   no knowledge of widgets. This is the proof that the model works.
3. **Cells**, for pane interiors, the line-number gutter and the palette's
   preview band. The bridge runs `Editor::render` into an in-memory ratatui
   buffer and slices each rectangle into rows of runs carrying **resolved
   colours**. The theme keys, syntax categories and source positions behind
   those colours are all discarded at this step.

The web render already builds the **whole frame's** display list. The frame's
one paint folds `ui.spec()` with `Paints::HostsOnly` when chrome cells are
suppressed, so every chrome surface is laid out and painted into the list, and
only the host leaves (panes) are drawn as cells. Sending the whole list costs
nothing to produce.

The transport is a WebSocket with a full-scene `hello` followed by pushed
diffs (web-ui.md §3.1). The diff unit is a top-level region, except for panes,
which diff per pane.

## 2. Caveats of the current protocol

Measured with a temporary probe on a debug build at 140×44, with
`src/app/render.rs` open. "Rows" is the estimate for pane diffs keyed per row
(new rows by content, plus the row order). "DL" is the display list diffed per
item. The DL column is plain JSON with full theme strings, so a real encoding
would be smaller.

| Step | Today (region diff) | Rows | DL (chrome) |
|---|---|---|---|
| Caret move, no scroll | 25.9 KB | 2.3 KB | 0.14 KB |
| Type one char | 39.9 KB (menus 14 KB resent) | 1.2 KB | 1.0 KB |
| Scroll 1 row | 24.5 KB | 1.7–4.6 KB | 0.13 KB |
| Type 5 chars | 25.8 KB | 0.8 KB | 0.14 KB |
| PageDown | 25.8 KB | 24.8 KB | 0.27 KB |
| Open File menu | 0.8 KB | – | 5.6 KB |

The hello was 40.7 KB, of which panes were 25.1 KB. The whole frame's display
list was 154 items (330 with the file explorer open), about 17 KB.

What the numbers and the code say:

- **A pane is one diff unit.** Any change to a pane resends all of it. A caret
  move changes the current-line highlight, which is drawn into the cells, so
  it costs the same as rewriting every row. Scrolling one line shifts every
  row index and resends everything. The frontend then rebuilds the pane's
  whole SVG (per-row patching is web-ui.md §3.4's top open item).
- **Region diffs are coarse for chrome too.** The menu projection carries the
  whole menu tree, including closed menus, so any enabled-state change (Undo
  becoming available after a keystroke) resends 14 KB.
- **Colours arrive resolved.** A theme switch resends the full scene. Web
  themes cannot style buffer text by meaning (a syntax category, a diagnostic)
  because the meaning is not sent.
- **Two chrome paths.** The projections and their JS builders are a second
  statement of what the chrome is, kept honest by a parity test rather than by
  construction. The display list already states the same thing once.

## 3. The design

### 3.1 The rule for choosing a level

**Send the tree after layout, never before it.** Layout is in integer cells,
and the editor reads the resulting geometry back: hit-tests, pane content
rectangles, wrap widths, popup anchors, view-line mappings. Input comes back
from the browser as cells, which only means something if the browser drew at
the cells layout chose. A browser-side layout would be a second geometry
source, which breaks retained-mode-ui.md's "one source of geometry" and the
parity discipline. It cannot be serialised either: `Component` is an
`Rc<dyn AnyComponent>` and `LayoutReader` runs builder closures during layout.

**Send the text pipeline's rows, not a tree, for panes.** Panes stay `Host`
leaves by standing decision. Shipping buffer text or tokens for the browser to
lay out would re-implement wrapping, folding, conceal, soft breaks and virtual
text in JS, which breaks the frontend rule that nothing is re-implemented. The
level above cells already exists: the content pass lays out each pane's rows
(`PaneContent`: styled spans per visual row plus `ViewLineMapping`s) before any
cell is painted, and the cell pass records the theme-key provenance of every
cell (`CellThemeInfo`: fg key, bg key, syntax category).

### 3.2 The protocol after all the changes

**Sent as laid-out tree info (keyed display-list items, diffed per item):**

- **All chrome:** menu bar and dropdowns, tabs, status bar, prompt and
  palette, popups, context menus, file explorer, file browser, trust dialog,
  confirmations, Settings, keybinding editor, dock, floating panels and
  sidebar sections.
- **What each item carries:** a stable identity, its parent's identity, a cell
  rectangle and clip, a draw kind (fill, wash, lines, rule, border, scrollbar
  with marks, overflow cap, scrim, selectable, host), its classes, and a
  role/state slot (role, label, checked / expanded / selected / disabled,
  focused) for ARIA.
- **A style table.** Each item names an entry the server resolves once:
  fg/bg colours, text modifiers, and the theme keys they came from. A frame
  ships only the entries that are new. A theme switch resends only the table.
- **Where hosts go.** A pane, a terminal grid or a window embed appears only as
  a `host` item with a rectangle. Its contents travel in the channel below.

**Sent as rows of styled runs (cell-positioned, diffed per row):**

- **Buffer panes.** Each row is gutter runs plus text runs, and each run is
  text plus a style-table id. The style carries the theme key and syntax
  category, so a web theme can style code by meaning.
- **Rows are keyed**, by source position, the row's kind (source line, wrap
  continuation, injected virtual line) and a content hash. A pane frame is the
  new row order plus only the rows the client does not have. Scrolling one
  line is one new row. A caret move is the two rows whose highlight changed.
- **Carets, and optionally selections, as overlays** rather than cells, so a
  caret move need not dirty a row at all and a native selection layer has
  something to draw from.
- **The palette's preview band** uses the same row format.

**Still raw cell grids:**

- **The terminal grid.** A PTY screen really is a grid of cells. It keeps the
  cell form but gets row-keyed diffs.
- **Composite (side-by-side diff) panes.** They are painted, not described
  (retained-mode-ui.md, "Composite buffer panes"), so they stay cells until
  that migration lands.

**Unchanged:**

- **Transport:** one WebSocket, a full `hello`, then pushed diffs only when
  something changed. The shared-view session model is unchanged.
- **Input:** still keys, and pointer events at cell coordinates, into the
  editor's own `handle_key` / `handle_mouse` and the tree's hit-testing. The
  browser never resolves a click to a byte itself.
- **Layout:** stays on the server and in whole cells.

### 3.3 Message shape (sketch)

```text
hello: { w, h, windowId,
         styles: { <id>: {fg, bg, b, i, u, fgKey, bgKey, cat} },
         items:  [ {id, parent, rect, clip, kind, cls, role?, state?, style, ...draw} ],
         hosts:  { <leaf>: {kind: "pane"|"grid"|"cells",
                            rows: { <rowKey>: [runs] }, order: [rowKey],
                            caret?, selections? } },
         clipboard, poll }

frame: { seq,
         styles?: { <id>: … },                      // new entries only
         items?:  { upsert: [ … ], remove: [id] },
         hosts?:  { <leaf>: { rows?: {…}, order?, caret?, selections? } | null },
         w?, h?, clipboard?, poll? }
```

Item identity is the producing element plus an ordinal within that element's
draws. `LayoutSpec::index` already maps keys to item ranges, and elements
persist across rebuilds by `(type, key)`, so identities are stable across
frames without anything new in the library. Removals are explicit so the
client never has to diff.

## 4. What changing it buys

- **Bytes.** Typing, caret moves and scrolling drop from about 25–40 KB a
  frame to about 0.1–2 KB (§2). The server-side change detection is
  per item and per row, so there is no coarse region to resend.
- **One chrome path.** The `Scene` projections and their JS builders retire
  one surface at a time. The frontend becomes one generic fold (the current
  tree fold, extended), and parity holds by construction: the same list folds
  to cells for the terminal and to DOM for the browser.
- **Per-row DOM patching** (web-ui.md §3.4) falls out of the row keys instead
  of being a separate frontend project.
- **Theming.** Theme switches cost one table. Web themes dress chrome by class
  and theme key (already how plugin panels work) and code by syntax category.
- **Accessibility.** The role/state slot gives ARIA a single source for every
  surface, which web-ui.md §3.8 asks for.

## 5. What it costs

- **Truly native chrome.** Display-list text is already fitted to cell widths,
  and a long list only exists as its visible rows. Chrome that reflows in a
  proportional font (the macOS web theme leans on this today), native
  `<input>` controls in Settings, and natively scrolled lists would all fall
  back to server-driven behaviour, as plugin panels already are: wheel
  forwarding and cell-positioned text. `paint_subtree` (an unclipped subtree)
  could supply extra rows for short lists. This is the real product tradeoff:
  one source of truth versus a native feel. Surfaces where the native feel
  matters most (Settings, the keybinding editor) move last, and may keep a
  projection.
- **Hover** is decided on the server (the pointer is a layout input), so it is
  a round trip per cell crossing. Plugin panels already work this way through
  `moved` events. CSS `:hover` on classes can add cosmetic feedback.
- **Some opening payloads grow.** Opening a menu costs about 5.6 KB of items
  against 0.8 KB of state, because the semantic model was already on the
  client. Small either way.
- **The library gains a semantic slot.** `Desc` has no role or state today.
  Adding it needs a caller and a test per retained-mode-ui.md's working rules,
  and it must land before any projection that carries `enabled`/`checked`
  retires, or the web loses information.
- **Caret and selection overlays** need a render mode that keeps them out of
  the cells, in the same spirit as `suppress_chrome_cells`, and the parity test
  must cover it.

## 6. Order of work

Each step ships on its own and keeps parity.

1. **Row-keyed pane diffs.** Only the bridge and the frontend's pane renderer
   change; no core change. This is the largest byte reduction, and it delivers
   web-ui.md §3.4's per-row patching.
2. **Provenance on pane runs.** Split runs on the theme keys and syntax
   category from the cell theme map, and introduce the style table.
3. **The whole-frame display list.** Generalise `tree_view` from plugin panels
   to every surface, with per-item diffs and the style table. Retire the
   `Scene` views one surface at a time: the status bar and tabs first (the
   tree already measures both), then menus, the prompt and palette, popups,
   and Settings and the keybinding editor last.
4. **The role/state slot**, in `Desc` and the display list, and ARIA from it.
5. **Rows from the content pass.** Build pane rows straight from `PaneContent`
   instead of reading them back out of the ratatui buffer, and move carets
   (then selections) to overlays.

Not planned, and argued against above: sending the description tree for the
browser to lay out, and sending buffer text for the browser to render panes
itself.
