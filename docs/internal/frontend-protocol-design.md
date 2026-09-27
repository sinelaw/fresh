# The frontend protocol: one wire format for web and native clients

> _Design note. Status: **PLANNED** — nothing here ships yet. It began as the
> answer to "can the web UI protocol carry the higher-level layout tree instead
> of the low-level cells the terminal draws?", and now also answers "can the
> same protocol drive a native Windows UI?". The answer to both is yes: every
> surface the `fresh-ui` tree describes goes out as its laid-out display list,
> the text panes go out as keyed rows, and the few surfaces the operating
> system owns (menu bar, clipboard, window title) go out as semantic models.
> The tree is sent **after** layout, never before it, so no client needs a
> layout engine. The protocol is a `fresh-ui` backend on the far side of a
> wire (§4.1): native look comes from each client's class table, and native
> behaviour only from OS facilities that replace a surface wholesale and answer
> with actions (§4.10; Appendix A inventories where the editor reads geometry
> back). The protocol is one
> schema with two encodings (JSON, and a binary encoding for native clients)
> and two transports (the web bridge's WebSocket, and the session daemon's
> local socket — a named pipe on Windows). Companion to [web-ui.md](web-ui.md) (the web frontend as built),
> [retained-mode-ui.md](retained-mode-ui.md) (the tree the protocol carries)
> and [00-overview.md](00-overview.md) (the session daemon)._

---

## 1. What exists today

**The web bridge's scene** mixes three levels of abstraction, one per stage of
the web frontend's history:

1. **Hand-written semantic projections** — the `Scene` views on `Editor`
   (`menu_view`, `status_view`, `palette_view`, `popups_view`,
   `file_explorer_view`, `file_browser_view`, `settings_view`,
   `keybinding_editor_view`, …). Each is a serialisable model with its own
   hand-written JS builder. Together they are a few thousand lines on each
   side, kept in step with the TUI by the scene-parity test.
2. **The display list, for plugin panels only.** `Editor::tree_view` ships the
   dock, floating panels and sidebar sections as the display-list items the
   tree painted for them, and the frontend's tree fold turns them into DOM with
   no knowledge of widgets. This is the proof that the model works.
3. **Cells**, for pane interiors, the line-number gutter and the palette's
   preview band. The bridge runs `Editor::render` into an in-memory ratatui
   buffer and slices each rectangle into rows of runs carrying **resolved
   colours**. The theme keys, syntax categories and source positions behind
   those colours are discarded at this step.

The web render already builds the **whole frame's** display list. The frame's
one paint folds `ui.spec()` with `Paints::HostsOnly` when chrome cells are
suppressed, so every chrome surface is laid out and painted into the list, and
only the host leaves (panes) are drawn as cells. Sending the whole list costs
nothing to produce. The web transport is a WebSocket with a full-scene `hello`
followed by pushed diffs (web-ui.md §3.1); the diff unit is a top-level
region, except for panes, which diff per pane.

**The other frontends** do not use the scene at all:

- **Terminal clients** attach to the **session daemon** (`fresh -a`) over a
  local socket — Unix domain sockets on Linux/macOS, **named pipes on
  Windows** (the `interprocess` crate). A daemon has a data channel (raw
  bytes: the terminal's ANSI stream out, input bytes in) and a control channel
  (versioned JSON messages: hello, resize, clipboard, title, open files). A
  `fresh --web` daemon hosts the web bridge beside them, so all of these are
  clients of one editor.
- **The in-process GUI** (`--gui`, the `fresh-gui` crate) draws the TUI's
  cells in a winit + wgpu window. On macOS it builds the **native menu bar**
  from the editor's menu model (`expanded_menu_definitions`, `menu_context`)
  and dispatches menu picks back as actions — the one place today where an
  OS-owned surface is fed by a semantic model.

There is no native client that renders anything other than cells.

## 2. Caveats of the current web protocol

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

- **A pane is one diff unit.** Any change to a pane resends all of it. A caret
  move changes the current-line highlight, which is drawn into the cells, so
  it costs the same as rewriting every row. Scrolling one line shifts every
  row index and resends everything.
- **Region diffs are coarse for chrome too.** The menu projection carries the
  whole menu tree, including closed menus, so any enabled-state change (Undo
  becoming available after a keystroke) resends 14 KB.
- **Colours arrive resolved.** A theme switch resends the full scene. A client
  cannot style buffer text by meaning (a syntax category, a diagnostic)
  because the meaning is not sent.
- **The page rebuilds instead of patching.** A changed pane clears its
  container and re-emits its whole SVG as one string, and the plugin-panel
  layer rebuilds every item on any change. Smaller diffs alone would not fix
  this: the protocol has to name what changed in terms a client can apply to
  existing nodes (§5).
- **Two chrome paths.** The projections and their JS builders are a second
  statement of what the chrome is, kept honest by a parity test rather than by
  construction. A native client would need a third.
- **Web-only.** The scene is JSON shaped for one JavaScript page, carried only
  over the bridge's WebSocket, with no schema, no version negotiation and no
  rule for what an older client does with a field it has never seen.

## 3. Goals

1. **One protocol, many clients.** The web page today; a native Windows client
   next; possibly the in-process GUI later, so it stops drawing terminal cells
   in a window. Nothing in the protocol is specific to a DOM, to XAML or to a
   language.
2. **The editor stays the single source of truth.** No client lays anything
   out, re-implements a text transform, or decides what a click means. This is
   the rule web-ui.md and retained-mode-ui.md already hold the web to.
3. **Clients patch retained trees in place.** A DOM and a XAML/Composition
   visual tree are both retained, and both are expensive to rebuild: a
   rebuilt DOM loses scroll and selection, a rebuilt WinUI tree pays COM
   instantiation, flickers, and breaks focus and UI Automation contexts. Every
   operation must map to a small, local mutation of an existing node.
4. **Cheap to decode off the UI thread.** Native UI frameworks (WinUI 3 is a
   single-threaded apartment) must decode and merge on a background thread and
   hand the UI thread a finished list of mutations.
5. **Evolvable.** A newer editor must be able to talk to an older client and
   the reverse, without a crash and with a defined fallback.
6. **Native where the OS owns the surface.** Menu bar, clipboard, window title,
   IME placement and accessibility are the operating system's, and a native
   client should use the OS's own.
7. **Stay a `fresh-ui` backend.** The library already defines what a backend
   consumes and sends — the display list, `Input`, host content — and what it
   may decide for itself: appearance, through its class table. The protocol
   adds a wire, not concepts.

## 4. The design

### 4.1 The protocol is a `fresh-ui` backend

The library already says what a client is. Its backend-independence goal
reads "paint produces a display list; TUI cells, the web DOM and test
assertions are consumers of that list", and the shell's stylesheet says "each
backend owns a table from class to appearance". A web page or a Windows client
on this protocol is exactly that: a backend folding the display list the
terminal folds, on the far side of a wire. Most of the design follows from
taking that literally:

| `fresh-ui` says | So the protocol |
|---|---|
| The display list is flat, ordered, absolute and keyed; paint order is list order; layers start at `layers_from` | Ships the list as it is: no parent links, order stated as position in the list, two halves. Where a client needs grouping (an accessibility tree), it reads the list's `index` ranges, which nest. |
| Identity is explicit: elements are matched by `(type, key)` at a position and survive rebuilds | Names an item by the element that painted it plus its ordinal among that element's draws. It never infers a tree item's identity from content. |
| A class names what a thing is; each backend owns a table from class to appearance; an unknown class decorates nothing | Carries classes untouched. A client's class table is where native look lives (CSS on the web, Fluent visuals on Windows) and where accessibility roles and states are read from. No new slot. |
| A theme key says how a thing is painted, and an unresolvable ink is a loud failure | Resolves inks on the server into the style table and keeps the theme keys beside the colours, as `tree_view` already does. |
| `Draw` is the whole vocabulary of what paint can say | Mirrors `Draw` one for one. A new draw kind is a library change with a caller, plus a fallback for clients that predate it (§4.5). |
| A `Host` leaf is content the host owns and draws | Sends host content on its own channel: panes as rows, terminals as grids. |
| Input is `Input` — pointer events at a position, keys along the focus chain — and it is one path for every frontend | Carries `Input` as it is, plus the host's own events (paste, committed text, resize). No client sends "activate item 3": that would be a second input path the tree does not route. |
| The tree is the whole keyboard | Lets no client-drawn surface hold keyboard focus the tree does not know about. |
| Geometry is produced by layout or recorded by ruling, never by accident | Takes no geometry back from clients. |
| Cell-level damage tracking is a non-goal | Diffs in the transport, outside the library, just as crossterm diffs cells for the terminal. |
| `Selectable` says where selecting is meaningful; selection is the host's | Confines a client's native selection to `selectable` items and host rows; buffer selection stays the editor's. |

### 4.2 The rule for choosing a level

**Send the tree after layout, never before it.** Layout is in integer cells,
and the editor reads the resulting geometry back: hit-tests, pane content
rectangles, wrap widths, popup anchors, view-line mappings. Input comes back
from a client as cells, which only means something if the client drew at the
cells layout chose. A client-side layout would be a second geometry source,
which breaks retained-mode-ui.md's "one source of geometry" and the parity
discipline. It cannot be serialised either: `Component` is an
`Rc<dyn AnyComponent>` and `LayoutReader` runs builder closures during layout.

This is also why **no client needs a layout engine.** A server-driven UI that
sends CSS-style flex properties forces every native client to embed one (Yoga
or a port of it) and to reproduce the web's layout exactly. Here every item
arrives with its final cell rectangle, so a native client positions children
absolutely — in WinUI 3, one custom panel whose arrange pass places each child
at `rect × cell size`. Nothing about flexbox, grid or text measurement has to
agree between clients, because only the editor ever computes it.

**Send the text pipeline's rows, not a tree, for panes.** Panes stay `Host`
leaves by standing decision. Shipping buffer text or tokens for a client to
lay out would re-implement wrapping, folding, conceal, soft breaks and virtual
text in every client. The level above cells already exists: the content pass
lays out each pane's rows (`PaneContent`: styled spans per visual row plus
`ViewLineMapping`s) before any cell is painted, and the cell pass records the
theme-key provenance of every cell (`CellThemeInfo`: fg key, bg key, syntax
category).

**Send a semantic model where the OS owns the surface.** A native menu bar, an
OS file dialog, the clipboard, the window title and the IME candidate window
cannot be drawn from display-list items; they need the model. The menu
model already exists for the macOS native menu bar. These few projections are
kept and formalised (§4.4); every other `Scene` projection retires.

### 4.3 What the protocol carries

**Laid-out tree info (keyed display-list items, diffed per item):**

- **All chrome:** menu bar and dropdowns, tabs, status bar, prompt and
  palette, popups, context menus, file explorer, file browser, trust dialog,
  confirmations, Settings, keybinding editor, dock, floating panels and
  sidebar sections.
- **What each item carries:** its identity (the element that painted it and
  its ordinal among that element's draws), a cell rectangle and clip, a draw
  kind — `Draw`'s variants one for one (fill, wash, lines, rule, border,
  scrollbar with marks, overflow cap, scrim, selectable, host) — its classes,
  and a style-table id. Classes are also where roles and states live
  (`button primary focused`, as the shell's buttons already say). A client's
  class table maps them to appearance and, for accessibility, to ARIA roles
  and states on the web and UI Automation control types and patterns on
  Windows. Each frame also names the focused element, from the tree's focus.
- **A style table.** Each item names an entry the server resolves once:
  fg/bg colours, text modifiers, and the theme keys they came from. An entry's
  id is its *provenance* (the ink, or the theme keys, category and modifiers
  of a run), never its resolved colour, so ids survive a theme switch. A frame
  ships only the entries that are new. A theme switch resends only the
  table's values, under the same ids.
- **Where hosts go.** A pane, a terminal grid or a window embed appears only as
  a `host` item with a rectangle. Its contents travel in the channel below.

**Rows of styled runs (cell-positioned, diffed per row):**

- **Buffer panes.** Each row is a list of text runs, and each run is text
  plus a style-table id. The style carries the theme key and syntax category,
  so a client can style code by meaning. The gutter is a **separate column
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
- **Carets, the current line and selections as overlays** rather than cells,
  so a caret move need not dirty a row at all, a native selection layer has
  something to draw from, and a client can place the IME candidate window at
  the caret's rectangle.
- **The palette's preview band** uses the same row format.

**Raw cell grids (the content really is a grid, or is not described yet):**

- **The terminal grid.** A PTY screen is a grid of cells. It keeps the cell
  form but gets row-keyed diffs.
- **Composite (side-by-side diff) panes.** They are painted, not described
  (retained-mode-ui.md, "Composite buffer panes"), so they stay cells until
  that migration lands.

**OS-service models (§4.4):** the menu model, the window title, the clipboard
text, and notifications.

**Unchanged:** input stays keys, text, and pointer events at cell coordinates,
into the editor's own `handle_key` / `handle_mouse` and the tree's
hit-testing; no client resolves a click to a byte itself. Layout stays on the
server, in whole cells. The shared-view session model stays: every attached
client mirrors one editor, and the grid is fitted to the smallest viewport.

### 4.4 Surfaces the operating system owns

Some surfaces are better as the OS's own than as items drawn in the grid:

| Surface | Model sent | Web client | Windows client |
|---|---|---|---|
| Menu bar | the menu model (menus, items, enabled/checked, accelerators) | draws the in-grid items | native `MenuBar`, or the title-bar menu |
| Window title | title string | `document.title` | window title |
| Clipboard | `{seq, text}` (web-ui.md §3.5) | `navigator.clipboard` | Win32 clipboard, with no gesture restriction |
| IME placement | the caret overlay's rect | hidden input at the caret | TSF / IMM candidate window at the caret |
| Notifications | kind, text | toast in page | Windows notification |
| Open / Save As | the prompt's kind and starting directory | the in-grid browser | the OS file dialog |

An OS surface answers only with an `Action` — the editor's rebinding and
serialisation currency, which carries no position (retained-mode-ui.md,
"Messages, and the applier"). A pick from a native menu comes back as the
item's action and arguments, the path the macOS menu bar already uses; an OS
file dialog comes back as the action the prompt would have run, with the
chosen path. §4.10 gives the rule for which surfaces qualify.

**Per-client chrome in a shared frame.** Every attached client mirrors one
frame. If a native client draws the menu bar natively, the in-grid menu bar is
still in the frame for the web client beside it. The rule follows the
existing grid-size rule: an in-grid surface is dropped from the layout only
when **every** attached client declares it owns that surface natively (a
capability in its hello). Otherwise it stays in the frame, and a client that
owns it natively skips drawing those items (they are recognisable by class)
and leaves the row blank. When the last web client leaves, the frame reflows
without the row.

### 4.5 One schema, versioned, two encodings

**The schema is Rust types.** The message types live in one module (a
`fresh-protocol` crate, or a module the daemon and the bridge share), with
`serde` derives. A JSON Schema is generated from them with `schemars` — the
crate already generates the config schema — and non-Rust clients generate
their types from it (a C# client with a JSON-Schema code generator). There is
no second schema to keep in sync.

**Two encodings of the same types:**

- **JSON** for the web page, `curl`, the parity harness and debugging.
- **MessagePack** for native clients: compact, fast to decode, and produced by
  the same `serde` derives (`rmp-serde`) with the same field names, so it is
  the same schema byte-for-byte in meaning. On .NET, MessagePack-CSharp
  decodes it from pooled buffers without per-field string allocation.
  Protocol Buffers were considered: stronger tooling for evolution, but a
  `.proto` file would be a second source of truth beside the Rust types.

A client names the encoding it wants in its hello; the web page always uses
JSON.

**Versioning and evolution:**

- The client hello carries a protocol version and a **capability list**
  (`rows`, `overlays`, `roles`, `native-menu`, `binary`, the draw kinds it
  knows, …). The server answers with its own version and the capabilities it
  will use.
- **Fields are additive and optional.** A field's meaning never changes; a
  change of meaning is a new field. Clients ignore unknown fields.
- **Unknown draw kinds degrade on the server.** The server never sends a draw
  kind a client did not list; it sends the kind's declared fallback instead
  (usually a `fill` or nothing). A client that nonetheless meets an unknown
  item keeps it in its maps and draws nothing, so later patches and removes
  still resolve.
- **Unknown roles** are treated as a generic group by the client's
  accessibility mapping.
- A client too old for the server's minimum version gets a version-mismatch
  message, as terminal clients do today.

### 4.6 Transports

The messages are the same on every transport; only framing differs.

- **Browsers:** the web bridge's WebSocket, one message per WebSocket frame
  (text frames for JSON, binary for MessagePack). Hosted by the standalone
  bridge or by a `fresh --web` daemon, as today.
- **Native clients:** the **session daemon's local socket** — a named pipe on
  Windows, a Unix socket elsewhere — the same endpoint `fresh -a` attaches to.
  The client's hello on the control channel asks for the frontend protocol
  instead of the terminal byte stream, and the data channel then carries
  length-prefixed messages. A native client therefore gets everything the
  daemon already gives terminals: the session keyed by working directory or
  `--session-name`, workspace restore, detach and reattach, and coexisting
  with terminals and browsers on one editor. The data channel is a local
  kernel pipe, with no network stack in the path.
- No request/response RPC layer (JSON-RPC, gRPC) is needed. The protocol is
  two streams: frames pushed by the server, input sent by the client. The
  only reply-shaped exchange is the hello.

### 4.7 Message shape (sketch)

```text
client hello: { proto, client, encoding: "json"|"msgpack",
                caps: ["rows","overlays","native-menu","os-file-dialog","kind:overflow",…],
                cols, rows, cell: {w, h, dpi} }

server hello: { proto, caps, w, h, windowId,
                styles: { <styleId>: {fg, bg, b, i, u, fgKey, bgKey, cat} },
                items:  [ {id, rect, clip, kind, cls, style, ...draw} ],  // list order
                layersFrom: <id of the first layer item> | null,
                groups: [ {key, first, last} ],     // the list's index: keyed subtrees
                focus:  <id> | null,                // the focused element's first item
                hosts:  { <leaf>: { kind: "pane"|"grid"|"cells",
                                    rows:   { <rowId>: [ [text, styleId], … ] },
                                    text:   [rowId, …],   // screen order, top to bottom
                                    gutter: [rowId, …],
                                    caret?, line?, selections? } },
                os:     { menu?, title?, clipboard?, notify? } }

frame:        { seq,
                styles?: { <styleId>: {…} },       // new ids, or new values for old ids
                items?:  { add:    [ {id, before, …full item} ],
                           patch:  [ {id, <only the fields that changed>} ],
                           move:   [ {id, before} ],
                           remove: [id] },
                layersFrom?, groups?, focus?,
                hosts?:  { <leaf>: { rows?: { <rowId>: [runs] },  // rows the client lacks
                                     text?: [rowId, …], gutter?: [rowId, …],
                                     drop?: [rowId, …],
                                     caret?, line?, selections? } | null },
                os?:     { menu?, title?, clipboard?, notify? },
                w?, h? }

input:        // fresh_ui::Input, one for one, at cell positions:
              press {col, row, button, mods, clicks} | release {col, row, button, mods}
              | move {col, row, mods} | wheel {col, row, delta, axis, mods}
              | key {chord}
              // the host's own events:
              | text {text} | paste {text} | resize {cols, rows, cell}
              // an OS surface's answer (§4.4):
              | action {name, args}
```

The pointer and key messages are `fresh_ui::Input`'s variants, so a client's
input enters the same two entry points a terminal's does and is routed by the
tree. Keys travel as chords in the editor's one key vocabulary
([key-vocabulary-unification.md](key-vocabulary-unification.md)), so the web
page and a Windows client each translate their platform's key events into the
same spelling. Item identity is the producing element plus an ordinal within
that element's draws: `LayoutSpec::index` already maps keys to item ranges,
and elements persist across rebuilds by `(type, key)`, so identities are
stable across frames with nothing new in the library. On the wire an element is
named by its arena slot and generation, as an opaque value: the generation is
bumped whenever a slot is freed, so a name is never reused for a different
element, and the server never uses a name to look an element up. Row ids are the
server's, assigned as §4.3 describes. Everything a client must do is stated
explicitly — adds, patches, moves and removals — so no client diffs anything.

### 4.8 Designed for in-place patching

The operations are chosen so that each one is **at most one mutation of one
node** in a retained client tree — a DOM element, or a XAML element or
Composition visual — and so that the common frames touch almost nothing. The
server keeps what it last sent to each client (item fields, row ids, style
ids) and computes everything below against it. **The diffing happens once, on
the server, where the tree's keyed reconciler already knows identity**; no
client runs a virtual-tree reconciler, a keyed-list move minimiser or an
interruptible diff.

- **Identity is node identity.** An item id or a row id names exactly one
  client node for as long as it lives. Clients keep `id → node` maps and never
  look a node up by position or selector.
- **An id never changes kind.** An item's draw kind is fixed for its
  lifetime; a change of kind is a `remove` and an `add` under a new id. A
  client can therefore keep a **pool per kind** and recycle nodes without
  ever converting one kind of node into another.
- **Patches are field-level.** `patch` carries only the fields that changed,
  and each field maps to one property of the node: `rect` to its offset and
  size, `style` to its class or brush, `lines` to its text, `cls` to its
  class list (and so its ARIA or UI Automation state), `thumb` to the thumb's
  offset. The server
  compares field by field against what it last sent.
- **Order is list order, stated locally.** The list is flat, as the library
  makes it, and paint order is its order. An item's position is stated as
  `before`: the id of the next item in the same half of the list, or none for
  last. An insertion is one `insertBefore` in a DOM, or one `Children.Insert`
  at the index of `before` in XAML; nothing else is renumbered. A numeric
  paint index or z-index is deliberately not used: inserting one item would
  renumber every item after it. The in-flow half and the layers
  (`LayoutSpec::layers_from`) are two root containers, so a popup opening
  never reorders the chrome under it.
- **Moves are rare, and explicit.** An item changes position in the list only
  when the tree's structure changes; `move` says so. A pure geometry change (a
  pane resized, a panel dragged) is a `rect` patch, never a move.
- **Grouping is read, not built.** A client that needs nesting — an
  accessibility tree, a scroll container — takes it from `groups`, the list's
  own index of keyed subtrees, whose ranges nest. The flat list stays the only
  thing a client patches.
- **Rows are placed by index, not by content.** The `text` and `gutter` lists
  give each row id its screen row. A row still on screen keeps its node and
  only its vertical offset changes. `drop` lists the row ids that left the
  screen; a row that later comes back is sent again, under a new id.
- **Style ids are stable.** Runs name a style by provenance (§4.3), and a
  client turns each style into one shared resource — a CSS class, a brush. A
  theme switch changes the resource, not the runs.
- **Carets, the current line and selections are overlays.** They are cell
  rectangles on their own layer above the rows, so moving one changes one
  offset and never touches text.
- **Operations are idempotent and mergeable.** Each is a set, not a delta (a
  patch sets fields; a row list replaces the previous list), so a client that
  falls behind merges several frames into one — on a background thread — and
  applies the merged result once: later values win, and an add followed by a
  remove cancels.
- **Geometry needs no measuring.** Every position is a cell rectangle and the
  client knows its cell size, so applying a frame never reads layout back.

**What an insertion costs.** Take a node inserted in the middle of a list:

- **Identity survives the insert when siblings are keyed.** `fresh-ui`'s
  reconciler looks a keyed description's key up anywhere in the old child
  list, so every keyed sibling keeps its element, and so its protocol id. The
  insert is one `add {id, before}`: one `insertBefore` in a DOM, one
  `Children.Insert` in WinUI. Nothing else is renumbered, because order is
  stated relative to the next item rather than as an index.
- **Siblings pay for moving, not for rebuilding.** Items below the insertion
  point move, so each gets a `rect` patch: a few bytes on the wire and one
  transform write on the client, with no reflow. A viewport materialises only
  its window, so this is bounded by the visible rows (tens, not the list's
  length), and the row pushed out of the window is a `remove`.
- **Position keys defeat this, and most shell lists use them today.** Rows
  keyed by index — the prompt's suggestions, the file browser, the keybinding
  table and its autocomplete, popup items, Settings' categories and search
  results — are matched by position. Inserting at row 5 gives the element at 5
  the new row's content, the element at 6 the old 5's, and so on: the wire
  carries a content patch for every visible row below the insertion plus one
  `add` at the end, and the client rebuilds those rows' text instead of moving
  nodes. Still bounded by the window and correct, but not minimal. The fix is
  in the tree, not the protocol: key rows by what they are — the command, the
  path, the setting — as the library asks ("keys are supplied by the caller
  and never inferred"). Plugin widget lists already key by the plugin's own
  keys, falling back to the index only where one is missing. The terminal
  gains too: per-row element state, such as hover, stays with its row.
- **Moves are minimised once, on the server.** When surviving items change
  order, the server finds the longest run of them still in their old relative
  order and emits `move` only for the rest (a longest-increasing-subsequence
  over their old positions). A plain insertion finds no moves; a reorder moves
  as few nodes as possible. This is the one place keyed-list move minimisation
  runs, and no client repeats it.
- **Ordinals are local to one element.** An item's id is its element plus its
  ordinal among that element's draws. When one element's draws change — a
  text run gains a differently styled piece in the middle — the pieces after
  it within that element shift ordinals and arrive as patches. The effect
  stops at the element's boundary.
- **Pane rows behave the same way.** Inserting a buffer line is one new text
  row, the new row order (a few dozen short ids), and one new gutter row at the
  bottom: the gutter's other rows still show the same numbers at the same
  screen rows. Rows below keep their nodes and change only their offsets.

**What the server pays.** The server's work per frame is proportional to what
is on screen, not to what changed. `fresh-ui` rebuilds the display list every
frame — the paint walk is O(visible items) — and the transport compares it
item by item against what each client last received. At the few hundred
items a frame holds (§2), that is small. The library's element-level dirty
tracking could later let the transport skip subtrees that did not relayout or
repaint, and retained-mode-ui.md already lists the per-frame clones and the
memos that sit below the work as open; this design does not depend on either
and does not claim incrementality on the server. The saving it does claim is
on the wire and in every client, where cost follows the change.

### 4.9 Client application

**Common to every client:**

- **Decode and merge off the UI thread; apply on it.** The socket reader
  decodes each message and merges it into one pending frame (§4.8's merge
  rule). The UI thread takes the pending frame once per display frame and
  applies it. Nothing touches the UI tree from the reader.
- **Write-only apply.** The apply pass creates, patches, moves and removes. It
  never reads layout back, so the framework lays out once after the pass.
- **Absolute placement.** Items and rows are positioned at `cell × cell size`.
  Changing an offset only recomposites.
- **Rows.** Each pane holds a text layer and a gutter layer. A row is one node
  built once from its runs, with every glyph pinned to its cell column.
  Applying a new `text` list walks it once: a row whose index changed gets a
  new offset, a new row id gets a node, ids not in the list are freed. A
  one-line scroll is about 44 offset writes and one new row; typing rebuilds
  the one edited row.
- **Pointer input is one handler at the root.** The client converts the
  pointer's position to a cell and sends it as `Input`. There are no per-node
  handlers to attach, detach or leak, because the tree's hit-test decides what
  the cell means.
- **The class table is the client's.** Each client keeps its own table from
  class to appearance and to accessibility semantics, as the terminal keeps
  `shell_style`. An unknown class decorates nothing; an unknown role maps to a
  generic group.
- **Reconnect.** A server hello discards every map and rebuilds from scratch;
  it is the only operation that does.

**The web page:**

- One `requestAnimationFrame` callback applies the pending frame.
- `transform: translate(x, y)` for placement.
- A row is one SVG `<text>` (or one line element if the medium changes).
- Styles are one `<style>` element with a rule per style id
  (`.s17 { fill: …; font-weight: … }`); a theme switch rewrites that element
  and touches no other node. Web themes override the same rules by theme key
  or category, as the tree fold already does with `--ink-*`.
- CSS containment (`contain: strict` on panes and surfaces, `contain: content`
  on rows) keeps a change inside one from invalidating layout outside it.
- The caret node persists, so its blink phase survives unrelated frames.
- ARIA roles and states come from the class table; `aria-owns` or nesting
  follows `groups`; focus follows the frame's `focus`.

**A Windows client (WinUI 3):**

- The reader runs on a background thread over the named pipe, decoding
  MessagePack from pooled buffers. The merged frame is handed to the UI thread
  with `DispatcherQueue.TryEnqueue`, at most one pending apply at a time.
- **Chrome** is XAML elements or Composition visuals under two custom panels,
  one per half of the list, whose arrange pass places each child at its cell
  rectangle — no measure-driven layout and no layout engine. Nodes are pooled
  per draw kind. The layers panel sits above the in-flow one.
- **Native look is the class table.** `button` draws as a Fluent button face
  at the tree's rectangle, `focused` as the system focus visual, `selected` in
  the system selection brush, a layer's ground as an acrylic flyout with a
  shadow; the chrome font is the system's, and the accent colour is the
  user's. Geometry, focus and input stay the tree's.
- **Panes are not XAML text controls.** A pane's rows are thousands of runs;
  one `TextBlock` per run would be thousands of COM objects. Each row is one
  Composition visual whose surface is drawn once with DirectWrite, glyphs at
  their cell columns; scrolling changes the visuals' offsets on the
  compositor. The caret is a Composition visual whose blink is a compositor
  animation, so it never costs the UI thread.
- **Styles are brushes.** One `SolidColorBrush` per style id, shared by every
  element and row that uses it. A theme switch sets each brush's colour, and
  everything using it repaints with no element touched.
- **Accessibility** maps classes through the class table to UI Automation
  control types and patterns (toggle, expand/collapse, selection item), names
  an element by its `lines` text, builds the automation tree from `groups`,
  and raises focus changes from the frame's `focus`.
  A pane's text needs a UI Automation text provider; the rows give it the
  visible text, and the accessible-text projection web-ui.md §3.8 proposes
  would give it the rest. The same projection serves ARIA on the web.
- **Input.** Key events are translated into the key vocabulary's chords;
  text from TSF arrives as `text`; the candidate window is placed at the
  caret overlay's rectangle. The cell size comes from DirectWrite metrics in
  device-independent pixels and is re-measured on a DPI change, which sends a
  `resize` as the web page does on zoom.
- **OS surfaces** per §4.4: a native menu bar and flyouts built from the menu
  model, the window title, the clipboard, notifications.

**Language for the Windows client.** The protocol does not decide it. A C#
client gets WinUI 3 and its accessibility peers first-hand and generates its
message types from the JSON Schema. A Rust client (windows-rs over Win32,
Composition and DirectWrite) would reuse the protocol crate's types directly
and share the key translation with `fresh-gui`, but WinUI 3 from Rust is
immature, so it would likely draw chrome itself rather than use XAML
controls. Recommended: C# and WinUI 3 for a client meant to feel native.

### 4.10 Native look, native behaviour, and where each belongs

The design takes one side of the tradeoff in §6: **the editor lays out, in
cells, and clients draw what they are given.** Within that, "native" means two
different things, and `fresh-ui` already puts them in different places.

**Native look belongs to the class table, and every surface gets it.** A class
names what an item is, and each backend owns the table from class to
appearance (§4.1). A Windows client draws `button` as a Fluent button face,
`focused` as the system focus visual, `selected` in the system selection
brush, a toggle's `checked` state as a switch, a layer's ground as an acrylic
flyout, all in the system font; the web does the same in CSS. The item still
sits at the rectangle the tree gave it, keys still go along the tree's focus
chain, and a press is still hit-tested by the tree. This applies to every
surface: Settings, the keybinding editor, dialogs, menus, the palette,
plugin panels.

**Native behaviour belongs to the OS, and only for surfaces that leave the
frame.** A client may replace a surface with an OS facility — something that
lays itself out, takes its own input and answers when it is done — only when
all of these hold:

1. **The OS facility replaces the surface wholesale.** The tree does not lay
   the surface out for that client, under the all-clients rule of §4.4.
2. **It answers only with an `Action`.** `UiMsg` already draws this line: an
   `Action` is something a user could bind, meaningful without this frame; a
   `UiFact` is positional, about this frame's tree. An OS surface can produce
   the first and never the second.
3. **The editor never reads its geometry back, and nothing is anchored to
   it.** Appendix A is the check.
4. **The tree holds no focus inside it.** While it is open the OS owns its
   input, as a native menu bar's tracking loop or a file dialog does, and the
   tree's focus stays where it was.

That admits the OS menu bar, whose items are actions with arguments (macOS
already does it), and the OS file dialog in place of Open File / Save As,
which answers with the action the prompt would have run and the chosen path.
The OS services of §4.4 — clipboard, title, IME placement, notifications — are
not surfaces in the frame at all.

It does not admit Settings or the keybinding editor as native forms, though
they are modal and look like the obvious candidates:

- Their interactions are positional facts (`UiFact`s), not actions, and their
  text fields are the tree's `TextField`s, whose caret the library places. A
  native `TextBox` would take the keyboard from the tree and hold an editing
  state the editor does not.
- The web's existing semantic paths for them (`SettingsHit` by index, a
  keybinding row by index) are exactly the second input path §4.1 rules out,
  and they retire with the `Scene` projections, as retained-mode-ui.md's "The
  web" already plans.

They get native look through the class table, like everything else. A
client-reported viewport, which an earlier draft of this design proposed so
their page keys could use a native list's height, is dropped: it is geometry
coming from a client, which the library rules out.

**The read-backs are residue, not a boundary.** Every client draws the tree's
layout, so none of the read-backs in Appendix A can disagree with what a
client shows; they are not hazards for this protocol. They are the places the
host still does layout's work, which retained-mode-ui.md's working rule — "a
surface is done when the tree measures it" — says should move into layout.
The keyboard ones are the clearest: a host reading a panel's rectangle to size
PageUp/PageDown, or a height written while the description is built, is what
an anchor answered at layout (`ScrollByPages`, `Reveal`) already expresses.
Moving them is worth doing for the terminal alone, and it also fixes the
inconsistencies the sweep turned up (Appendix A).

## 5. What changing it buys

- **Bytes.** Typing, caret moves and scrolling drop from about 25–40 KB a
  frame to about 0.1–2 KB (§2), on every transport.
- **One chrome path for every client.** The drawing projections and their JS
  builders retire one surface at a time; the web page and a native client are
  each one generic applier. Parity holds by construction: the same list folds
  to cells for the terminal, to DOM for the browser and to XAML for Windows.
- **No layout engine and no reconciler in any client.** Both are the editor's
  already; clients only apply mutations.
- **Native look on every surface** from each client's class table, and
  **native OS integration** where the OS owns the surface: menu bar, file
  dialog, clipboard, IME, accessibility, title.
- **No new library concepts.** The protocol is the display list, `Input` and
  host channels — what `fresh-ui` already defines for a backend.
- **Per-row patching** (web-ui.md §3.4) falls out of the row ids.
- **Theming.** Theme switches cost one table. Clients dress chrome by class
  and theme key, and code by syntax category.
- **Accessibility.** One class vocabulary feeds ARIA and UI Automation.
- **Sessions for free.** A native client is a daemon client: attach, detach,
  restore, and sharing one editor with terminals and browsers.

## 6. What it costs

- **Truly native layout.** Display-list text is already fitted to cell widths,
  and a long list only exists as its visible rows. Chrome that reflows in a
  proportional font, native `<input>` / `TextBox` controls, and natively
  scrolled lists fall back to server-driven behaviour, as plugin panels
  already are: wheel forwarding and cell-positioned text. `paint_subtree` (an
  unclipped subtree) could supply extra rows for short lists. This is the real
  product tradeoff — one source of truth versus a native feel — and it applies
  to the web and Windows alike. The design takes the single-source side
  (§4.10): native look everywhere through class tables, and native behaviour
  only where an OS facility replaces a surface wholesale — the menu bar and the
  file dialog. Settings stays a tree surface, so it gets Fluent styling but
  not native text boxes.
- **Hover** is decided on the server (the pointer is a layout input), so it is
  a round trip per cell crossing, over a local pipe or loopback. Clients can
  add cosmetic hover feedback by class.
- **Some opening payloads grow.** Opening a menu costs about 5.6 KB of items
  against 0.8 KB of state, because the semantic model was already on the
  client. Small either way, and a client with a native menu gets the model
  instead.
- **A class vocabulary for roles and states.** The shell's buttons already
  emit state classes (`focused`, `hovered`, `disabled`); roles and the other
  states (checked, expanded, selected, heading) need the same, each with a
  caller and a test per retained-mode-ui.md's working rules. There is no new
  slot in `Desc`. The vocabulary must be in place before any projection that
  carries `enabled`/`checked` retires.
- **Overlays** need a render mode that keeps the caret, current line and
  selections out of the cells, in the same spirit as `suppress_chrome_cells`,
  covered by the parity test. Until then a caret move dirties two rows.
- **Per-client state on the server.** Item fields, row ids and style ids sent
  are kept per client, replacing today's per-region hashes. It is bounded by
  what is on screen, and a hello resets it.
- **A protocol to maintain.** A schema, two encodings, a capability list and
  fallbacks per draw kind, plus a second client codebase for Windows.

## 7. Where this departs from the native-client research

The research this revision was checked against (a survey of native Windows
renderers for server-driven UI) assumes a server that sends a pre-layout,
web-shaped UI tree. Fresh's editor sends a laid-out one, which changes several
of its recommendations:

| Research recommends | This design | Why |
|---|---|---|
| WinUI 3, C#, Native AOT | Same, recommended | Native fidelity, UI Automation, compositor-thread animation |
| Embed a flexbox engine (Yoga.Net) in a custom panel | Not needed; a panel that places children at their cell rects | The editor lays out once; clients never do (§4.2) |
| Client-side virtual tree, O(n) keyed diff, LIS moves, interruptible reconcile | Not needed; the server sends explicit `add`/`patch`/`move`/`remove` | The editor's reconciler already knows identity; diffing once on the server is cheaper than in every client |
| Element pooling | Kept, per draw kind | Ids never change kind (§4.8) |
| Background decode, UI-thread apply via `DispatcherQueue` | Kept | Operations are mergeable, so the UI thread gets one batch |
| Protocol Buffers for schema evolution | MessagePack from the same Rust types, plus a generated JSON Schema, capabilities and server-side fallbacks | One source of truth for the schema |
| Named pipes with StreamJsonRpc or gRPC | The daemon's existing named pipe, length-prefixed frames, no RPC layer | The protocol is two streams, not calls |
| Tag-based event dispatch on pooled controls | One root pointer handler sending cells | The editor's hit-test decides what a cell means |
| Map semantics to UI Automation | Kept, through the class vocabulary | Classes already say what an item is; one vocabulary feeds ARIA and UIA |
| Native controls for app surfaces | Native look from a class table at the tree's rectangles; OS facilities only for surfaces that leave the frame (§4.10) | The tree is the whole keyboard and the one input path |
| Controls for all content | Composition visuals drawn with DirectWrite for pane rows | A code pane is too many runs for per-run controls |

## 8. Order of work

Each step ships on its own and keeps parity. Steps 1–5 and 8–9 pay for
themselves on the web and the terminal; steps 6, 7 and 10 add the native
client.

1. **Row-keyed pane diffs and the row applier.** The bridge assigns row ids
   by content (text and gutter separately), and the page applies rows as §4.9
   describes. No core change. This is the largest byte reduction, and it
   delivers web-ui.md §3.4's per-row patching.
2. **Provenance on pane runs.** Split runs on the theme keys and syntax
   category from the cell theme map, and introduce the style table.
3. **The whole-frame display list.** Generalise `tree_view` from plugin panels
   to every surface, with the item operations of §4.8 and the keyed applier of
   §4.9 replacing the tree fold's rebuild-everything pass. Retire the drawing
   `Scene` views one surface at a time: the status bar and tabs first (the
   tree already measures both), then the prompt and palette, popups, and
   Settings and the keybinding editor last. Keep the OS-service models (§4.4),
   including the menu model for the OS menu bar.
4. **The role and state class vocabulary**, emitted by the shell's widgets,
   and ARIA from it.
5. **Rows from the content pass and overlays.** Build pane rows straight from
   `PaneContent` instead of reading them back out of the ratatui buffer, and
   move the caret, the current-line band and then selections to overlays.
6. **Extract the protocol.** Move the message types into their own crate or
   module, generate the JSON Schema, add the hello's version and capability
   negotiation and the per-kind fallbacks, and add the MessagePack encoding.
   The web bridge keeps JSON.
7. **The protocol on the daemon's socket.** A client hello that asks for the
   frontend protocol gets frames on the data channel, beside terminal
   clients and browsers on the same editor; add the `native-menu` capability
   and the all-clients rule of §4.4.
8. **Layout's own answers for paging and reveal.** Move the keyboard
   read-backs of Appendix A into layout: page sizes read off panel rectangles
   (Settings, the keybinding editor) and heights written while the description
   is built (the file explorer) become anchors answered at layout
   (`ScrollByPages`, `Reveal`), with a page intent on `List` and `Tree`, and
   the Settings tree stops following the body's scroll through a cached
   `top_item`. No client needs this; it is retained-mode-ui.md's rule, and it
   fixes the sweep's inconsistencies for the terminal too.
9. **Rows keyed by what they are.** Replace the index keys on the shell's
   lists — prompt suggestions, the file browser, the keybinding table and its
   autocomplete, popup items, Settings' categories and search results — with
   domain keys (command, path, setting), so an insertion or a reorder moves
   nodes instead of rewriting every row below it (§4.8, "What an insertion
   costs").
10. **A Windows client.** WinUI 3, per §4.9: the chrome panel and pools, the
   Composition row visuals, brushes, the class table's Fluent look, the OS
   menu bar and file dialog, clipboard, IME, and UI Automation.

Not planned, and argued against above: sending the description tree for a
client to lay out, sending buffer text for a client to render panes itself,
embedding a layout engine or a tree reconciler in any client, semantic input
paths beside `Input`, and geometry reported back by clients.

## Appendix A. Where the editor reads geometry back

A sweep of the editor crate (the web bridge and its projections excluded)
for every behaviour that reads laid-out geometry after layout. "Pointer" is a
hit-test, hover or drag; "keyboard" is a key, command or plugin behaviour that
depends on on-screen size; "placement" anchors one surface to another;
"sizing" sizes something that is not drawn (a viewport, a PTY, a plugin
report). Most event-time readers ask the retained tree by key at the last
frame's size; the few caches still written during render are named.

None of these is a hazard for a client of this protocol: every client draws
the tree's layout, so a read-back reads what the client shows. They are the
places the host still does layout's work, and step 8 of §8 moves the keyboard
ones into layout.

**Menu bar and dropdowns.** No host read-back. Presses are hit-tested inside
the tree; menu keys move by index.

**Confirm and trust dialogs.** No host read-back. Their in-grid box is sized
from the last frame when the description is built; arrow keys are not
geometric.

**Open File / Save As browser.** No host read-back outside the web's
projection. Paging is a fixed 10; the list window follows the selection
inside layout.

**Context menus.** Hit-tested inside the tree. Two placement reads: the
"Close split" menu is anchored under the split's × control
(`close_split_button` via `split_control_rect`), and a right-click in the file
explorer asks the explorer's region whether it was the title row
(`explorer_body_context`).

**Settings modal.** Keyboard: body PageUp/PageDown page by the body window's
height (`select_next_page` / `select_prev_page` over `BodyWindow`, filled by
`refresh_settings_body_window` from the items viewport's rectangle and
scroll); category-tree PageUp/PageDown page by `tree_page_rows`, set in
`settle_modal_viewports` from the categories panel's rectangle; the left tree's
highlight follows the body's scroll through `top_item`
(`sync_tree_cursor_to_body_scroll`, `current_section_index`, also used by
`jump_to_search_result`); the search results and entry dialogs' scroll
offsets are read off their viewports. The web reaches it through a second,
index-based input path (`dispatch_settings_hit` with a `SettingsHit`), which
retires with its projection (§4.10).

**Keybinding editor.** Keyboard: PageUp/PageDown page by `scroll.viewport`,
set in `settle_modal_viewports` from the editor box's rectangle
(`keybinding::table_rows`). The web selects rows through an index-based path
(`kbedit_select_display_row`), which retires with its projection.

**Prompt and palette.** Keyboard: none geometric — PageUp/PageDown move by a
fixed 10. Description-time feedback: suggestion column widths are measured
over the last layout's visible window (`record_suggestions_window` →
`suggestions_window`, read by `suggestions_description`). The Live Grep card
scrolls its selection into the results region's height
(`settle_prompt_suggestions`, `ensure_selected_visible_within`), and its
preview viewport is resized to the card's inner rectangle.

**Popups (completion, hover, signature, action).** Placement: anchored to the
caret cell and the completion word's start column, read from the pane's
settled view (`publish_popup_carets`, `popup_caret_cells`). Pointer: hover
stays alive while the pointer is over a popup's rectangle
(`is_mouse_over_transient_popup` via `popup_rects`). Keyboard: paging uses the
model's `max_height`, not the drawn height.

**Status bar.** Placement: popups opened from a status element (LSP status,
remote indicator, read-only, update) are placed above that element from the
description's cached layout (`popup_above_status_bar` reading
`shell_frame_status_bar`). Right-side elements are dropped by frame width at
description time.

**Tabs.** Pointer: drag drop zones and insertion index (`compute_tab_drop_zone`
via `tab_rects` and `pane_strip_at`). Keyboard: reveal-active-tab is decided in
layout; tab-switch animations use the pane's content rectangle
(`cycle_tab` via `pane_or_group_content_rect`). Feedback: whether tab names
are shortened comes from the last frame's strip width (`Window::pane_strips`).

**File explorer.** Keyboard: PageUp/PageDown, scroll-to-selection, maximum
scroll and sticky ancestors all read `FileTreeView::viewport_height`, written
while the description is built (`explorer_body`). Pointer: the wheel clamps
against the same height; right-click reads the explorer's region.

**Split grid and panes.** Sizing: every layout pass reads the pane boxes
(`PaneRects::read`, retained per window, and `layout_panes_offscreen` for
windows not on screen). From them: each split's viewport size
(`Window::apply_layout`), PTY rows and columns and scrollback wrap width
(`resize_visible_terminals`, and `paint_embed` for embedded windows), the
plugin snapshot's split rectangles and viewport sizes
(`populate_plugin_state_snapshot`, `getViewport`, `listSplits`), the
split-window reply's rectangle, and the pane chosen by `pane_beside`.
Keyboard: page motion, visual-line Up/Down/Home/End and smart Home read the
pane's settled rows (`PaneView`, via `handle_page_motion`,
`handle_visual_line_movement`, `compute_wrap_aware_visual_move_fallback`,
`smart_home_visual_line`); every `Viewport` scroll and ensure-visible path
reads the viewport's size; search highlighting and semantic-token requests
are bounded by it. Pointer: click-to-byte, selection drag, fold toggles, LSP
hover, terminal link hover and forwarding, all through the content rectangle;
scrollbar clicks, drags and hover through the bar rectangles and thumb facts;
divider drags through the body area.

**Composite (diff) panes.** Keyboard: cursor movement and horizontal scroll
use the text width derived from the content rectangle
(`get_cursor_line_info`, `PaneLayout`); hunk navigation and paging use the
viewport height less the header.

**Plugin panels (dock, floating, sidebar sections, pane-mounted).** Keyboard:
list and tree paging read the published item window (`widget_viewport`,
`described_widget_viewport`); arrow focus moves by laid-out rectangles
(`move_panel_focus`, the directional focus policy); markdown prose Up/Down
reads wrapped rows (`prose_vertical_key`); page panels seat the reading row
and focus by widget rectangles (`page_widget_spans`,
`page_anchor_of_widget`). A pane-mounted panel's buffer text is its laid-out,
unclipped rows (`mirror_pane_panels`), so every buffer action in that pane
depends on layout width. Sizing: auto-sized lists get a row budget from the
terminal height or the sidebar section's resolved height
(`floating_panel_inner_height`, `resolve_sidebar_sections`).

**Frame-wide.** The frame's caret is the display list's cursor; the theme
inspector reads the per-cell provenance map written during paint
(`resolve_theme_key_at`, `inspect_theme_at_cursor`); macro replay re-lays the
shell at the last frame's size after each action (`recompute_layout`).

**Incidental findings from the sweep** (inconsistencies, not violations):

- `ensure_cursor_visible_for_navigation` scrolls horizontally with a
  hard-coded gutter of 6 columns instead of the measured gutter.
- Popups page by `max_height` less borders, while the drawn height may be
  clamped smaller by the chrome area.
- Settings body paging applies the body's height in rows as a count of item
  steps, so a page of multi-row cards moves further than one screen.
- Visible terminals get PTY sizes from hand-subtracted rows and columns off the
  pane box, while embedded windows use the tree's content rectangle.
- Prompt, Live Grep and file-browser paging is a fixed 10 whatever the visible
  row count.
- The multi-line text widget pages by its spec's row count, not the height of
  a growing box.
- The keybinding editor's `ScrollState` offset and content height are never
  read or set outside tests; only its page size is live. `EntryDialogState`'s
  `viewport_height` is never updated from its default.
