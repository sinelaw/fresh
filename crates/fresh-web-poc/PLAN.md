# Porting Fresh's features to the data-server / web-client split

Each feature on getfresh.dev (`docs/features/`) is ported one of three ways:

- **Run** — the editor's bundled plugin runs unchanged in the client's
  plugin runtime; the work is implementing the plugin API it calls.
- **Native** — reimplemented in the client (or as a server data API). Used
  where a plugin is written against the terminal's widget model (floating
  widgets, widget panels, buffer groups, key capture) and a browser-native
  version is less work and better UX than emulating that model.
- **Skip** — out of scope for the proof of concept, with the reason.

## Plugins with UI: a web-native panel API

The terminal's widget plugins are written against its cell-based widget model
(`mountWidgetPanel`, `mountFloatingWidget`, buffer groups, key capture). The
client does not reproduce that API. It gives plugins one designed for the
browser instead (`web/js/ui.js`):

- `ui.panel({id, title, place})` — a panel in the sidebar, the bottom dock,
  beside the editor, or a dialog; returns a handle with `render(tree)`,
  `focus()`, `close()`.
- `h(type, props, ...children)` — a declarative tree of real DOM controls:
  `box`/`row`/`col`, `text`, `input`, `button`, `checkbox`, `select`,
  `list` (virtualised, keyboard-navigable, `onActivate`), `tree`, `table`,
  `badge`. Re-rendering keeps focus and caret by element key.
- Events are callbacks on the props; keyboard navigation and focus are the
  browser's.

Every feature with UI is a client plugin (`web/plugins/*.js`) on the
client's own plugin API (`web/js/host.js`: buffers, views, decorations,
commands, keys, host processes, LSP, and the panel kit in `web/js/ui.js`).
The editor's plugins are reimplemented on it, not run through a
compatibility layer: their API is inspiration, not a contract.

Plugin freshness was checked (`git log` per file, and every `editor.*` call
against `fresh.d.ts`): no shipped plugin calls a removed API. The
`examples/` plugins are not shipped by the editor (its build compiles only
top-level `plugins/*.ts`) and are dropped. `diff_nav`, `tab_actions`,
`goto_with_selection` and the LSP-helper plugins have had no commits in 90
days; where they matter the feature is reimplemented rather than run.

## Plan

| Feature (docs/features) | How | Notes |
|---|---|---|
| **Editing** — multi-cursor, selection, undo/redo | Native | Done: cursors, selection, per-client undo grouped by typing run. To add: Ctrl+D next occurrence, Alt+click/Ctrl+Alt+↑↓ add cursor, all edits at every cursor, word/line selection, duplicate/move line, comment toggle, auto-close brackets, smart indent/dedent |
| Editing — current-line highlight, occurrence highlight, rulers, whitespace indicators, line wrap, read-only, auto-save | Native | Client view options; auto-save is a client timer calling the server's save |
| Editing — code folding | Native | Folding ranges from the language server (server routes `textDocument/foldingRange`), indentation-based otherwise; folds are view state |
| Editing — interrupted saves, hot exit | Adapt | The server holds unsaved documents across client reloads already, and a save refuses over another writer's change unless forced; add a recovery write on the server |
| **Markdown** — compose | Native | `web/plugins/markdown.js` declares block nodes (headings, paragraphs, lists, quotes, code, tables, rules) and hides markup; compose views draw them as a real typographic document (`web/js/doc.js`) (done) |
| Markdown — dual source/compose view | Native | Split showing the same buffer, one view composing |
| Markdown — mermaid/math previews | Skip | Needs external renderers (`mmdc`, `utftex`) on the host; the plugin path exists but is not wired |
| Markdown — contents panel | Native | Outline from the client's text, click to jump |
| **Command palette** — commands, files, buffers, go to line | Native | Done for commands/files/go to line; add buffer switching and `>`/`:`/`@` prefixes |
| **Navigation** — tabs, splits, position history, large files | Native | Done: tabs, splits, huge files. Add back/forward position history |
| Navigation — go to definition / references / symbols | Native | Definition done; references and document symbols through the LSP multiplexer, results in a navigable panel |
| **File explorer** — open, create, rename, delete, copy/cut/paste, follow active, .gitignore, sorting | Native + server | Server file operations (`fs.create/rename/delete/copy`), gitignore-aware listing; client context menu and keys |
| **Search and replace** — in buffer: case, whole word, regex, captures, step, replace one/all | Native | Server search for windowed (huge) buffers; client for whole ones |
| Search — project-wide search/replace, live grep | Native + server | Server `search.project` (regex, gitignore-aware, streamed results); replace applies through documents |
| **Terminal** — open, multiple, restart, scrollback, keyboard capture | Native | Done with xterm.js over the server's PTY stream. Add restart of an exited terminal |
| Terminal — send selection to terminal, clickable paths | Native | xterm link provider for `path:line:col` |
| **Scripting** — drive the editor from a shell | Skip | Scripts drive an editor UI, which now lives in each client; needs a design for addressing a client |
| **Code tours** | Native | Load the tour JSON through the file API; a panel stepping through steps |
| **LSP** — diagnostics, hover, completion, definition | Native + server | Done |
| LSP — status in status bar, diagnostics panel, signature help, rename, code actions, formatting, multi-server, root detection | Native + server | Server routes the requests; client panels/popups. Status bar shows server state |
| **Git** — gutter, blame, log, review diff, live diff, change navigation | Native + server | Gutter: server `git.diff` per file against HEAD → client line marks, next/prev change. Blame: server `git blame --porcelain` → inline annotations. Log: panel of commits, show a commit's diff. Status bar branch: `git_statusbar` runs unchanged (done) |
| Git — review diff mode (`audit_mode`), live diff (`live_diff`) | Skip | Large terminal-UI plugins (composite buffers); review via the log/diff views instead |
| **Compare files** | Native | Side-by-side split with diff marks on both sides |
| **Themes** | Native + server | Done: the server serves the editor's built-in themes resolved (`theme.list`, `theme.get`); the client maps every theme key onto its CSS colour variables, so each theme styles the whole UI, terminal included. Live-previewing picker; the choice persists. User themes: not yet |
| **Encoding** | Adapt | Server reports a document's detected encoding (done: `encoding` in `doc.info`); status bar shows it. Reload-with-encoding: skip |
| **Remote editing (SSH)** | Adapt | Done (`--ssh`): files through the editor's remote agent (ranged reads, line counting, walking and searching on the remote side); processes, terminals and language servers through `ssh` with the connection's port, identity and options. Exercised end to end against a local sshd, with and without `--slow-network` |
| **Devcontainers** | Adapt | Done (`--container`): files stay local; processes, terminals and language servers run through `docker exec -w <dir>` in a container whose workspace is bind-mounted at the same path (checked at start). Exercised against a `docker` stand-in; not yet against a real daemon |
| **Daemon mode** | Adapt | The server *is* the daemon: clients come and go, documents and terminals stay; a disconnected client's documents, watches and processes are released. Done in substance |
| **Orchestrator**, **Workboard**, agent sessions | Skip | Multi-workspace agent management; large, and orthogonal to the split |
| **Keybinding editor** | Native | A table of the client's keymap with search; edits saved in the browser |
| **Emacs keymap** | Native | An alternative keymap table for the common movement/editing keys |
| **Vi mode** | Skip | `vi_mode` depends on key capture and modal editor state across ~4k lines; a native vi layer is its own project |
| **Dashboard** | Skip | Widgets (git, disk, weather, PRs) are terminal-panel UI |
| **Workspace trust** | Adapt | Done (`--restricted`): an untrusted workspace refuses processes, terminals and language servers and runs no probe for them; trusting starts the servers for open documents |
| **Web UI skins** | Native + server | Done: a skin is a folder (`skin.css` with a header, optional `furniture.html` and `theme.json`, images, fonts) against the client's documented DOM and theme variables; frames (full / bezel with furniture), density (`@scale`, the editor cell re-measured), `@extends`, and an editor theme that is an ordinary theme (`skin/<id>`). Built-in skins embedded (Default, Compact, Winamp Classic, Bridge), user skins from `~/.config/fresh/web-skins` through the authority; live readouts from the host's `/proc`. Picker with live preview, Ctrl+Alt+T, `?skin=`. Not ported: installing from a URL or files in the browser, `@keys symbols`, `@explorer-side`, live re-apply on save |

## Order

1. Editing (multi-cursor, smart editing, view options, auto-save), palette prefixes, position history.
2. Search and replace (buffer and project), file explorer operations.
3. LSP: references, symbols, rename, code actions, formatting, signature help, folding, diagnostics panel.
4. Git: gutter, change navigation, blame, log, commit diff; compare files.
5. Markdown dual view and contents; code tours; themes; keymaps and keybinding editor.
6. Server: recovery writes for unsaved documents. (Authorities for SSH and containers, workspace trust and the encoding report are done.)

## UI audit against the editor

What a user of the editor finds on screen, checked against this client: the
editor's menus (`crates/fresh-editor-core/src/config.rs`, the menu bar's
File / Edit / View / Selection / Go / LSP / Explorer / Help), its default
keymap (`keymaps/default.json`), its status bar and its web UI
(`crates/fresh-editor/web-ui`: menu bar, context menus, sashes, skin picker),
and the user documentation (`docs/features/*.md`, `docs/getting-started`).
Status: **present** (was there), **fixed** (added by this audit), **missing**
(not yet, with why), **skipped** (by design, with why).

| Area | Feature | Status |
|---|---|---|
| Chrome | Menu bar: File, Edit, View, Selection, Go, LSP, Explorer, Help, from the command registry (keys shown, toggles checked, submenus); F10 and Alt+F / E / V / S / G / L / X / H, arrows, letters, Escape; plugins add rows (`fresh.menus.add`); one menu component for the menu bar and every context menu; styled by Winamp (engraved rail, olive plate) and Bridge (coloured segments in the top bar) | fixed |
| Chrome | Status bar items open their action: position → Go to Line, line ending → Set Line Ending, encoding → reopen / save with, language → Set Language (the language server follows), diagnostics → panel, LSP → server status, git branch → log, theme → Select Theme, read-only, cursors, compose; tooltips name the action and its key | fixed |
| Chrome | Skin picker button and Ctrl+Alt+T | present |
| Chrome | Theme picker | present |
| Chrome | Zoom: editor font size larger / smaller / reset | missing |
| Layout | Drag to resize: editor splits (double-click: even) | fixed |
| Layout | Drag to resize: sidebar width, bottom dock and terminal height, right dock width (remembered per workspace; double-click: default) | fixed |
| Layout | Split right / down, close, next / previous split | present |
| Layout | Maximize split (toggle) | missing |
| Layout | Scroll sync of same-buffer splits | missing (rarely used; after the rest) |
| Layout | Session restore per workspace (in the browser): the splits and ratios, each split's file tabs with cursor and scroll, the active tab, sizes, sidebar and terminal | fixed |
| Tabs | Close with ×, middle-click | present |
| Tabs | Context menu: Close, Close Others, Close to the Right, Close Saved, Close All, Split Right / Down, Copy Path / Relative Path, Reveal in Explorer | fixed |
| Tabs | Drag to reorder; drag onto another split's tabs or text to move it there (cursors, scroll, mode and folds kept; an emptied split closes) | fixed |
| Tabs | Wheel over the tab bar scrolls it | fixed |
| Tabs | Same-name tabs show their folder | present |
| Tabs | Preview (ephemeral) tab from a single click in the explorer | missing (needs a tab state model; after the rest) |
| Editor | Right-click context menu (Cut / Copy / Paste, Go to Definition, References, Rename, Code Actions, Hover, Format, comment / duplicate / delete line, select all / occurrences, Revert); a right press outside the selection moves the caret | fixed |
| Editor | Scrollbar: drag, click to jump | present |
| Editor | Scrollbar markers: diagnostics, search matches, git changes, cursor | missing |
| Editor | Horizontal scrolling of long lines (wrap off) | missing |
| Editor | Line wrap toggle (Alt+Z), line numbers, current line, occurrences, whitespace, rulers | present |
| Editor | Indentation guides, matching-bracket highlight, rainbow brackets | missing (drawing extras; after the chrome) |
| Editor | Minimap, breadcrumbs | skipped (the editor has neither) |
| Explorer | Sticky ancestors: the folders of the top visible row stay pinned (any UI-kit tree); a click scrolls back to the folder | fixed |
| Explorer | Drag and drop to move files and folders (confirmed) | missing |
| Explorer | Refresh / collapse-all buttons in the header | missing |
| Explorer | Toggle hidden / gitignored files | missing |
| Explorer | Multi-selection (Shift+Up/Down) for cut / copy / delete | missing (after the rest) |
| Explorer | Open, new file / folder, rename, delete, copy / cut / paste, copy path, follow active file, gitignore dimming, keyboard | present |
| Files | Recent files (File ▸ Open Recent, and the file palette with nothing typed) | fixed |
| Files | Close All, Close Others | fixed |
| Files | Save As | missing (a copy of a document to a new path: needs a server op) |
| Files | Set line ending (LF / CRLF / CR), reopen or save with an encoding, set language, revert | fixed (server `doc.set`, `doc.reload` with `encoding`) |
| Files | New untitled buffer (Ctrl+N) | missing (documents are files on the server; New File asks for a name) |
| Palette | Commands, files, buffers, go to line; `path:line:col` | present |
| Palette | Go to symbol in the file (`@`), in the workspace | missing (`LSP: Document Symbols` is a panel) |
| Navigation | Back / forward, go to definition / references, matching bracket, paragraphs | present |
| Navigation | Next / previous error (F8 / Shift+F8) | missing |
| Navigation | Bookmarks (Ctrl+Shift+N set, Alt+N jump) | missing |
| Editing | Multi-cursor, selection, undo / redo, comment, duplicate / move line, fold, auto-close, smart home, kill ring | present |
| Editing | Case conversion, sort lines, trim trailing whitespace, transpose, block selection, expand selection, macros | missing (editing commands, not chrome; after the UI) |
| Search | Find / replace bar with toggles, project search panel | present |
| Search | Find in selection, query replace, find selection next | missing |
| LSP | Hover, completion, definition, references, rename, code actions, format, symbols, signature help, diagnostics panel | present |
| LSP | Server status (status bar, `LSP: Show Status`) | fixed |
| LSP | Restart / stop a server, inlay hints | missing (the server starts one per language and keeps it) |
| Git | Gutter, blame, log, diff, change navigation, branch in status | present |
| Terminal | Multiple, tabs, toggle, keyboard capture (xterm) | present |
| Terminal | Restart an exited terminal, send selection to terminal | missing |
| Help | Welcome, keyboard shortcuts (keybinding editor), show log | present |
| Help | Settings screen | skipped (settings are palette toggles in the browser; a settings UI needs the editor's schema) |
| Workspaces | Orchestrator / multi-session management | in progress (another agent) |
