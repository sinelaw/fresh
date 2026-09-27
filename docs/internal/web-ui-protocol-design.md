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
- **The page rebuilds instead of patching.** A changed pane clears its
  container and re-emits its whole SVG as one string, and the plugin-panel
  layer rebuilds every item on any change. Smaller diffs alone would not fix
  this: the protocol has to name what changed in terms the page can apply to
  existing nodes (§3.4).
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
  fg/bg colours, text modifiers, and the theme keys they came from. An entry's
  id is its *provenance* (the ink, or the theme keys, category and modifiers
  of a run), never its resolved colour, so ids survive a theme switch. A frame
  ships only the entries that are new. A theme switch resends only the
  table's values, under the same ids.
- **Where hosts go.** A pane, a terminal grid or a window embed appears only as
  a `host` item with a rectangle. Its contents travel in the channel below.

**Sent as rows of styled runs (cell-positioned, diffed per row):**

- **Buffer panes.** Each row is a list of text runs, and each run is text
  plus a style-table id. The style carries the theme key and syntax category,
  so a web theme can style code by meaning. The gutter is a **separate column
  of rows** with its own ids, not part of the text row: a line number is a
  function of screen position far more often than of content, and folding it
  into the text row would make every row below an inserted line change.
- **Rows have opaque ids the server assigns by content.** A row whose runs
  are identical to a row the client already holds reuses that row's id;
  identical rows (blank lines) are matched in screen order. Ids are never
  derived from a source byte offset or line number, because typing one
  character shifts the offsets of every row below it. A pane frame is the new
  row order plus only the rows the client does not have. Scrolling one line is
  one new row. Typing is the edited row. Inserting a line is one new text row
  and one new gutter row at the bottom — the gutter's other rows still show
  the same numbers at the same screen rows.
- **Carets, and optionally selections, as overlays** rather than cells, so a
  caret move need not dirty a row at all and a native selection layer has
  something to draw from. The current-line highlight is an overlay for the
  same reason: drawn into the rows, it makes a caret move dirty two rows.
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
         styles: { <styleId>: {fg, bg, b, i, u, fgKey, bgKey, cat} },
         items:  [ {id, parent, rect, clip, kind, cls, role?, state?, style, ...draw} ],
                                                   // in paint order within each parent
         hosts:  { <leaf>: { kind: "pane"|"grid"|"cells",
                             rows:   { <rowId>: [ [text, styleId], … ] },
                             text:   [rowId, …],   // screen order, top to bottom
                             gutter: [rowId, …],
                             caret?, line?, selections? } },
         clipboard, poll }

frame: { seq,
         styles?: { <styleId>: {…} },              // new ids, or new values for old ids
         items?:  { add:    [ {id, parent, before, …full item} ],
                    patch:  [ {id, <only the fields that changed>} ],
                    move:   [ {id, parent, before} ],
                    remove: [id] },
         hosts?:  { <leaf>: { rows?: { <rowId>: [runs] },   // rows the client lacks
                              text?: [rowId, …], gutter?: [rowId, …],
                              drop?: [rowId, …],
                              caret?, line?, selections? } | null },
         w?, h?, clipboard?, poll? }
```

Item identity is the producing element plus an ordinal within that element's
draws. `LayoutSpec::index` already maps keys to item ranges, and elements
persist across rebuilds by `(type, key)`, so identities are stable across
frames without anything new in the library. Row ids are the server's, assigned
as described in §3.2. Everything the client must do is stated explicitly —
adds, patches, moves and removals — so the client never diffs anything itself.

### 3.4 Designed for in-place DOM patching

The protocol's operations are chosen so that each one is **at most one DOM
operation on one node**, and so that the common frames touch almost nothing.
The server already keeps the last-sent scene per client for today's region
diffs; it keeps the item and row state per client instead, and computes all of
the following against it.

- **Identity is DOM identity.** An item id or a row id names exactly one DOM
  node for as long as it lives. The client keeps `id → element` maps and never
  looks a node up by position or selector.
- **Patches are field-level.** `patch` carries only the fields that changed,
  and each field maps to one property of the item's node: `rect` to its
  transform and size, `style` to its class, `lines` to its text children,
  `state` to its ARIA attributes, `thumb` to the thumb's offset. The server
  compares field by field against what it last sent.
- **Order is sibling order, stated locally.** Paint order is DOM order within
  a parent, and it is stated as `parent` plus `before` (the id of the next
  sibling, or none for last). An insertion is one `insertBefore`; nothing else
  is renumbered. A numeric paint index or `z-index` is deliberately not used:
  inserting one item would renumber every item after it. The display list's
  in-flow half and its layers (`LayoutSpec::layers_from`) are two root
  containers, so a popup opening never reorders the chrome under it.
- **Moves are rare, and explicit.** An item changes parent or position among
  its siblings only when the tree's structure changes; `move` says so. A pure
  geometry change (a pane resized, a panel dragged) is a `rect` patch, never a
  move.
- **Rows are placed by index, not by content.** The `text` and `gutter` lists
  give each row id its screen row. A row that is still on screen keeps its
  node and only its vertical offset changes. `drop` lists the row ids that
  left the screen, so the client frees them; a row that later comes back is
  sent again, under a new id.
- **Style ids are stable.** Runs name a style by provenance (§3.2), and the
  client turns each style into a CSS class once. A theme switch changes the
  class rules, not the runs.
- **Carets, the current line and selections are overlays.** They are
  cell rectangles on their own layer above the rows, so moving them changes
  one transform each and never touches text.
- **Operations are idempotent and mergeable.** Each is a set, not a delta
  (a patch sets fields; a row list replaces the previous list), so a client
  that falls behind merges several frames into one before touching the DOM:
  later values win, and an add followed by a remove cancels.
- **Geometry needs no measuring.** Every position is a cell rectangle, and the
  client already knows the cell size, so applying a frame never reads layout
  back from the DOM.

### 3.5 Client-side application

How the frontend applies a frame. None of it needs the page to diff or
re-derive anything.

- **Coalesce, then apply once per animation frame.** Incoming frames are
  merged into a pending frame (§3.4's merge rule). One `requestAnimationFrame`
  callback applies it. Nothing is applied from the socket handler directly.
- **Write-only pass.** The apply pass only writes: create, patch, move,
  remove. It never reads `getBoundingClientRect`, `offsetWidth` or computed
  style, so the browser lays the page out once, after the pass, instead of
  once per read.
- **Items.** An `add` builds one node and inserts it with `insertBefore`
  under its parent's node. A `patch` touches only the named properties. A
  `remove` removes the node and its subtree from the map. Containers are
  never cleared and rebuilt.
- **Positions via transforms.** Items and rows are absolutely positioned with
  `transform: translate(x, y)` in pixels computed from cells. Changing a
  transform only composites; it doesn't reflow the page.
- **Rows.** Each pane holds a text layer and a gutter layer. A row is one node
  (one SVG `<text>`, or one line element if the medium changes) built once
  from its runs, with every glyph pinned to its cell column as today. Applying
  a new `text` list walks it once: an existing row whose index changed gets a
  new `translateY`, a new row id gets a node built and inserted, and ids not
  in the list are freed. A one-line scroll is therefore about 44 transform
  writes and one new row; typing rebuilds the one edited row.
- **Styles as a stylesheet.** The client keeps one `<style>` element with a
  rule per style id (`.s17 { fill: …; font-weight: … }`). Runs carry the class
  only; no colour is inlined. A theme switch rewrites that one element's text
  and touches no other node. Web themes override the same rules by theme key
  or category, as the tree fold already does with `--ink-*`.
- **Overlays.** The caret, current-line band and selection rectangles are
  nodes on a layer above the rows, patched by transform. The caret node
  persists, so its blink phase survives unrelated frames (the property
  today's caret code already preserves by hand).
- **Containment.** Each pane, surface and row sets CSS containment
  (`contain: strict` for panes and surfaces, `contain: content` for rows), so
  a change inside one never invalidates layout or paint outside it.
- **Reconnect.** A `hello` discards every map and rebuilds from scratch; it
  is the only operation that does.

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
- **Caret, current-line and selection overlays** need a render mode that
  keeps them out of the cells, in the same spirit as `suppress_chrome_cells`,
  and the parity test must cover it. Until then, a caret move dirties the two
  rows whose highlight changed — still two rows, not a pane.
- **Per-client state on the server.** Item fields, row ids and the style ids
  sent are kept per connected client, replacing today's per-region hashes. It
  is bounded by what is on screen, and a `hello` resets it.

## 6. Order of work

Each step ships on its own and keeps parity.

1. **Row-keyed pane diffs and the row applier.** The bridge assigns row ids
   by content (text and gutter separately, §3.2), and the frontend applies
   rows as §3.5 describes. No core change. This is the largest byte reduction,
   and it delivers web-ui.md §3.4's per-row patching.
2. **Provenance on pane runs.** Split runs on the theme keys and syntax
   category from the cell theme map, and introduce the style table.
3. **The whole-frame display list.** Generalise `tree_view` from plugin panels
   to every surface, with the item operations of §3.4 and the keyed applier of
   §3.5 replacing the tree fold's rebuild-everything pass. Retire the
   `Scene` views one surface at a time: the status bar and tabs first (the
   tree already measures both), then menus, the prompt and palette, popups,
   and Settings and the keybinding editor last.
4. **The role/state slot**, in `Desc` and the display list, and ARIA from it.
5. **Rows from the content pass.** Build pane rows straight from `PaneContent`
   instead of reading them back out of the ratatui buffer, and move the caret,
   the current-line band and then selections to overlays.

Not planned, and argued against above: sending the description tree for the
browser to lay out, and sending buffer text for the browser to render panes
itself.
