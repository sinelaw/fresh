# Web UI

> **Status: built.** This is the one design document for the web frontend. It
> replaced every earlier web design (the shared-scene design, the per-client
> `fresh-ui` tree design, and the "browser lays out the editor's semantic
> surfaces" design — all in git history, and their code is deleted). *As
> built* says where each part lives and what is still open. Where this
> disagrees with the source, the source is authoritative.

**Goal.** The core is a headless editor engine; the web UI is an ordinary
client-side web application built on it. The web client is free to look and
behave differently from the terminal. What it gets from the core is the
editor's engine — above all the lazy, high-performance buffer machinery — as
data and commands, never as UI.

---

## The shape

```
┌────────────── Rust core (headless engine) ──────────────┐      ┌──────── Web client (app) ────────┐
│ buffers · piece tree · lazy loading · wrap/row caches   │      │ layout · menus · palette · panels │
│ syntax · LSP · search · git · plugins · terminals       │ ───► │ focus · hover · scroll · dialogs  │
│ cursors · selections · undo · editing commands          │ data │ its own keymap and theming        │
│ settings · sessions · files                             │ ◄─── │ draws buffer lines it is sent     │
└──────────────────────────────────────────────────────────┘ cmds └───────────────────────────────────┘
          the terminal frontend keeps its own shell (fresh-ui), unchanged
```

There is no UI loop between the two: the client reads data, subscribes to
changes, and sends commands. The core holds no web UI state — no menu open, no
hover, no focus, no layout.

## What the core provides

### Buffers, through views

The core's value to the web is its text engine, so text stays there and only
windows of it travel.

- **A view** is the client's window onto a buffer: `{buffer, width, height,
  top}`, where `width` is in columns. The client opens one per editor pane it
  shows and tells the core when its size or scroll changes.
- **Visual lines on request.** The core lays the buffer out at the view's
  width — wrapping, folds, virtual text, conceal, wide and combined
  characters — from its piece tree and its wrap and visual-row caches, and
  sends the visual lines in the window: for each, its text and styled spans
  (syntax, selections, cursors, diagnostics, search matches, inlay hints, fold
  markers) and the buffer position it starts at. A huge file costs what its
  window costs: nothing outside the requested range is loaded or laid out.
- **Scrolling is the client's**, by visual line; the core answers for any
  window and keeps the view's cursor in sight only when asked (after an edit
  or a jump).
- **Positions are the core's.** A click is sent as (view, visual line,
  column); the core resolves it to a buffer position, because only it knows
  the wrapping, folds and character widths that produced the line.

### Editing, as commands

Cursors, selections, multi-cursor, undo and every editing operation stay in
the core, so text editing behaves like Fresh everywhere. The client sends
commands — insert text, delete, move by word/line/page, select, undo, indent,
add cursor, run any editor action by name — and receives the changed lines of
the views affected.

### Everything else, as data plus change events

- Files and the project tree; open buffers and their state (modified, read
  only, language, encoding).
- The command list with names and keybindings (the client may use or ignore
  them), settings and their schema, themes.
- Diagnostics; LSP results on request (completion, hover, signature help,
  definitions, references, code actions); search and replace; git.
- Sessions, windows and splits as data (the client decides how to show them).
- Terminals: PTY output as a cell grid per terminal, input as bytes.
- Notifications and messages (what the terminal shows in its status bar).

### Plugins

Plugins keep their API. What they show is already data — a plugin panel is a
`WidgetSpec`, a prompt is a list of suggestions, a popup is content — so the
core forwards it to the client as data, and the client's events come back as
the same widget events and prompt answers plugins receive from the terminal.
How a panel looks and where it goes is the client's.

## What the client owns

All of the UI: layout, menus, palette, explorer, tabs, status, dialogs,
settings screens, focus, hover, scrolling, drag and drop, theming, and its own
keymap. It decides what is on screen and how it behaves, and calls core
commands for anything that changes the editor. Its interactions never wait on
the core unless they change editor state or need data it has not fetched.

## Per-client state

Several clients may connect to one core (browsers, and terminals through the
session daemon). Shared: buffers, their content and undo. Each browser's own:
its views (size, scroll, wrap width), its cursors and selections in each of
them, and all of its UI state. The terminals share the panes' own cursors.
A command a browser sends without naming a view (a palette entry, a menu
item, an answer to a prompt) acts in that browser's active view, and the
status it is sent is that view's.

Positions put aside while another client edits — a browser's cursors and
scroll between its turns, the terminal's during one — are carried through
the edits made meanwhile (the buffer's coordinate map) when they come back,
so typing above someone's cursor moves it along with the text.

## Transport

One WebSocket per client, carrying JSON:

- **Requests and replies** for data the client asks for (view lines, LSP
  results, file listings, settings), tagged with an id.
- **Commands** (editing, actions, file operations), answered with the result
  or an error.
- **Events** pushed by the core when something the client subscribed to
  changes: lines of an open view, buffer state, diagnostics, files, settings,
  plugin surfaces, terminal output, notifications.

Same-origin and `Host` checks, bounded requests, a heartbeat and non-blocking
writes carry over from today's bridge unchanged.

## As built

- **The API** is `crates/fresh-editor/src/webui/api.rs`: one `ApiClient` per
  connection holds its views and a fingerprint of each topic it was last sent;
  after every pass the host pushes what changed. The transport, the security
  checks and the heartbeat are `webui/mod.rs`; the same `WebBridge` runs
  standalone (`webui::run`) and inside the session daemon (`fresh --web`).
- **Views** are `app/client_pane.rs`. A `ClientPane` is a client's window onto
  one split leaf: its size, its scroll and cursors in each buffer the leaf
  shows, and the visual rows it was last drawn with. They are swapped into the leaf for that
  client's render and its commands and swapped back out, so motion by visual
  line and clicks resolve against the rows the client sees, and the terminal's
  own pane — and every other client's — is untouched. The render is the text
  pipeline's per-pane path (`SplitRenderer::render_view`), laid out at the
  client's width; the client gets styled runs per visual row and the caret.
  The editor's own frame is still laid out headlessly each pass at the
  editor's size, because plugin render hooks and mounted panels run in it.
- **Workspaces.** A `ClientPane` names its window as well as its leaf: leaf
  ids are numbered per window, and the Orchestrator switches the active
  window. The `root` and `layout` events carry the active window's root and
  id. A view of a window switched away from is not drawn; the client, seeing
  another window in `layout`, closes its views and opens the new window's.
  A view closed while its window is merely inactive is kept by the
  `ApiClient`, keyed by window and leaf, and handed back when the client
  opens that pane again, so its scroll and cursors survive the round trip.
- **Data** is `app/engine_data.rs`: layout, buffers, status facts, commands,
  menus, the question being asked, popups, plugin panels, settings, files.
- **Editing** goes through the same entry points the terminal uses: keys
  through the core's keymap (`handle_key`) in the view's pane, actions by
  name, the palette and menu commands, the editor's own click handling.
- **The client** is `crates/fresh-editor/web-ui/` (see its README).

- **Terminals** are buffers in views like any other: the view paints the
  live grid over its rows, and the PTY takes the size of the view last
  opened, resized or typed into (the terminal's pane does the same on its
  relayouts), the way tmux's `window-size latest` does.
- **Screens with a model of their own** are edited through it: the
  keybinding editor's `KeybindingEditor` is held per client (`keys.*`) and
  saved as the terminal's screen saves it; the file prompts carry the
  directory they browse (`browser.*`).
- **Keyboard calibration** is the terminal's alone: it calibrates a
  terminal's key encoding, and a browser reports keys directly. The web is
  sent neither its menu item nor its command, and a web request that opens it
  anyway is answered with a message instead of an invisible modal.
- **Skins** are the client's ([web-ui-skins.md](../features/web-ui-skins.md)):
  CSS, inert furniture and an optional editor theme, chosen per browser and
  applied by `web-ui/js/15-skins.js`. The client lays itself out, so a skin
  arranges it with plain CSS; nothing about a skin reaches the core's layout.
  The core serves the skins folder (`/skins/…`, `webui/skins.rs`), installs a
  skin's editor theme on a pick (`skin.theme`, since the editor theme is the
  editor's shared, saved setting), and sends the live values furniture binds
  to (`skinValues`: what plugins publish with `editor.setSkinValue`, and the
  host's `skin_data:*` figures).
- **The editor's own frame** is still laid out each pass that pushes, headless
  at the editor's size: plugin render hooks and the mounting of plugin panels
  run there, and the terminals attached to a daemon are drawn from it.

## Views — the design

Views are explicit per-client data in the core, and the terminal is one
client among the browsers. Every command, hook, plugin call and per-frame pass
names the view it is for, or covers all views in `ViewId` order. The renderer
lays out each view at its own width from structure that has no width in it.
Nothing is inferred: there is no fallback view, no "current" view, and no
heuristic that depends on which other views exist.

### The model

- **A view** is one client's window onto one buffer in one pane, named by a
  `ViewId`. It holds its size, scroll, cursors, selections, wrap width,
  compose width and the visual rows it was last drawn with. It is plain data
  in the core, keyed by its id, and is never swapped in or out.
- **A client** owns a set of views. The terminal is one client, shared by every
  terminal attached to the session daemon, with one view per pane. Each browser
  is another client.
- **The buffer is shared by every view:** its content, undo, syntax,
  diagnostics, LSP state and plugin decorations.
- **Every view's positions follow edits.** Cursors and scroll are carried
  through each edit by the buffer's coordinate map, whichever view made it.

### Operations name their view

- **Every input arrives tagged with its view.** A key in the terminal names the
  terminal's view of the active pane; a browser message names its view, or its
  client's active view when the message does not name one. Editing, motion and
  actions take a `ViewId` and act on that view's cursors.
- **Plugin code runs in a view's context.** A hook or action handler runs with
  the `ViewId` of the input or frame that triggered it, and every API call it
  makes takes that view explicitly, including calls made after an `await`.
  Cursor-, selection- and viewport-dependent calls (`getPrimaryCursor`,
  `getSelectedText`, `insertAtCursor`, `getViewport` and the rest) take a view
  or fail. None of them falls back to another view.
- **Plugin code with no triggering view has no default view.** Timers and
  background events get no view; a cursor- or viewport-dependent call made
  there without naming a view is an error.

### Per-frame passes cover every view

- **`visible_views()`** lists every visible view of every client, in `ViewId`
  order. Each per-view pass iterates it:
  - offering lines to plugins (`lines_changed`);
  - `viewport_changed`;
  - keeping a view's cursor in sight when asked;
  - loading a large file's data under each view.
- **Buffer-wide requests** cover exactly the union of the visible views'
  ranges: LSP semantic-token ranges and search-as-you-type highlights.
- **Scroll-locked panes** sync among one client's own views.

### Plugin API

- **Hooks carry the view.** Every view-scoped hook carries a `view_id` and the
  view's geometry; `split_id` stays for compatibility.
- **`getViews()`** lists every view, for plugins that care about all of them.
- **Decorations are per buffer and have no width in them.** Overlays, conceals,
  virtual text and structure (fenced regions, code frames, tables) are stated in
  byte ranges and roles, never in columns.

### Layout is per view

- **The renderer does everything that depends on width,** per view, at that
  view's width: wrapping, compose's reflow, code-frame borders and rails, and
  table column widths. Structure comes from the core's grammar classification
  (`structure_lines_in`) and from what plugins add to it. Two views of one
  buffer at different widths are therefore both laid out correctly.
- **Compose width is a setting of the view,** applied to that view's own
  width. No other view affects it.

### The terminal

The terminal is one client with one view per pane, and its input always names
the active pane's view. Every rule above therefore reduces to the terminal's
behaviour as it is today, and no terminal behaviour changes.

## Decisions — settled, not re-argued

- **The web client is free to diverge from the terminal.** One behaviour
  across both frontends is not a goal for the UI; it remains one for editing,
  because editing is in the core.
- **Text layout stays in the core.** Wrapping and visual lines are computed by
  the core at the client's width, because that is where the lazy buffer's
  performance lives; the client only draws the lines it receives.
- **No shared UI tree.** The web does not consume `fresh-ui` descriptions,
  elements, layout or display lists, nor the shell's per-surface projections.
- **One authority for editor state.** The core owns buffers, cursors and undo;
  the client never edits text optimistically.
- **No editor code in the browser.** No WebAssembly build of the core for the
  web client.

## Related

- [retained-mode-ui.md](retained-mode-ui.md) — the terminal's shell, which this
  design leaves as it is.
- [rendering-and-layout.md](rendering-and-layout.md) — the text pipeline whose
  wrap and visual-row caches the views are served from.
- [plugins.md](plugins.md) — the plugin API whose surfaces the client renders.
- [fresh-web-poc](../../crates/fresh-web-poc/README.md) — a separate proof of
  concept of the other split: a data server (workspace, documents over the
  lazy buffer, edit log, terminals, language servers, behind SSH, container
  and local authorities) and a thick client that owns editing and layout.
