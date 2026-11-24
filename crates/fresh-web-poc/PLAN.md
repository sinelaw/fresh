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
| **Themes** | Native + server | Server lists and serves the editor's theme JSON files; client maps theme keys to CSS. Theme picker in the palette |
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
| **Web UI skins** | Skip | Belongs to the existing web UI; this client is styled with its own CSS |

## Order

1. Editing (multi-cursor, smart editing, view options, auto-save), palette prefixes, position history.
2. Search and replace (buffer and project), file explorer operations.
3. LSP: references, symbols, rename, code actions, formatting, signature help, folding, diagnostics panel.
4. Git: gutter, change navigation, blame, log, commit diff; compare files.
5. Markdown dual view and contents; code tours; themes; keymaps and keybinding editor.
6. Server: recovery writes for unsaved documents. (Authorities for SSH and containers, workspace trust and the encoding report are done.)
