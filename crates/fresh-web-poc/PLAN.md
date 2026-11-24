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
- `h(type, props, children)` (`children` one array) — a declarative tree of real DOM controls:
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
| **Orchestrator**, agent sessions | Native + server | Done; the gap table against the editor's is below ("Orchestrator: against the editor's") |
| **Workboard** | Native | Inbox and Board lenses (see below) |
| **Keybinding editor** | Native | A table of the client's keymap with search; edits saved in the browser |
| **Emacs keymap** | Native | An alternative keymap table for the common movement/editing keys |
| **Vi mode** | Skip | `vi_mode` depends on key capture and modal editor state across ~4k lines; a native vi layer is its own project |
| **Dashboard** | Skip | Widgets (git, disk, weather, PRs) are terminal-panel UI |
| **Workspace trust** | Adapt | Done (`--restricted`): an untrusted workspace refuses processes, terminals and language servers and runs no probe for them; trusting starts the servers for open documents |
| **Web UI skins** | Native + server | Done: a skin is a folder (`skin.css` with a header, optional `furniture.html` and `theme.json`, images, fonts) against the client's documented DOM and theme variables; frames (full / bezel with furniture), density (`@scale`, the editor cell re-measured), `@extends`, and an editor theme that is an ordinary theme (`skin/<id>`). Built-in skins embedded (Default, Compact, Winamp Classic, Bridge), user skins from `~/.config/fresh/web-skins` through the authority; live readouts from the host's `/proc`. Picker with live preview, Ctrl+Alt+T, `?skin=`. Not ported: installing from a URL or files in the browser, `@keys symbols`, `@explorer-side`, live re-apply on save |

## Orchestrator: against the editor's

Reviewed against `crates/fresh-editor/plugins/orchestrator.ts` (+ i18n),
`workboard.ts`, `agent_sessions*.ts`, `agent_discovery.ts`,
`live_sessions.ts`, `lib/agent_*.ts`, and `docs/features/orchestrator.md`,
`workboard.md`, `session-persistence.md`, `docs/internal/orchestrator-*.md`,
`tmux-native-sessions.md`, `agent-fresh-cli-exposure-plan.md`.
Present = done here; Adapt = done differently for this split; Later = worth
porting, not yet; Skip = with the reason.

| Editor feature | Here | Notes |
|---|---|---|
| Workspaces, each with its own files, terminals, LSP, authority, trust | Present | `Session` per workspace in one server; a page views one (`?ws=`) |
| Dock: persistent left column, Alt+O focus, View menu, × | Present | `#left-dock` (UI kit place `left`); open state remembered per browser |
| State marks ● ✓ * · ? and ! (failed + Retry) | Present | Server-side state (`src/terminal.rs`) with the editor's rules: 5 s idle window, 1.5 s work burst, OSC 133 / 9;4, built-in question rules |
| Attention line, status-bar beacon, Jump to Attention / Jump Back | Present | Waiting first, then done; jump-back per tab |
| Folders (nested, roll-ups), Move to Folder, New/Rename/Delete folder | Present | Server registry (`folder.*`); drag and drop into folders: Later |
| Group by date (Today / This week / Older by last worked in) | Present | "Worked in" = typed into its files or terminals |
| Pins, Rename, auto-name from the agent terminal's title | Present | |
| Search `/` | Present | |
| Compact view | Present | |
| Cards view (branch, changes, PR per card) | Later | The UI kit's virtual list has fixed rows; cards need a variable-height list |
| Show empty workspaces / All worktrees (on-disk worktrees not open) | Later | Needs `git worktree list` per repository through the authority |
| Project scope (limit the dock to one project) | Later | |
| New Workspace: agent picker from the registry (Claude, Codex, OpenCode, Aider, terminal, custom), prompt, auto mode | Present | `web/js/agents.js` mirrors `lib/agent_*.ts` |
| New Workspace: git — new worktree on a new branch (default) / work in folder; branch and base | Present | Through the workspace's authority; `~/.local/share/fresh-poc/worktrees/<repo>/` on the host (the editor: its data dir, `~/.fresh/worktrees` remotely) |
| Launch / Launch in background, pending row, failure with Retry / Dismiss | Present | Pending rows are this page's; a failed restore is the server's |
| "Here" mode (run the agent in the current workspace) | Adapt | Run Agent… (palette pick from the registry, then the task) |
| Teach Fresh CLI (agent drives the editor) | Skip | No scripting surface in this split (PLAN: Scripting) |
| Agent resume across restarts (`--session-id` provision / `--continue`) | Present (continue) | A registry agent's terminal carries its resume command (`claude --continue`, `codex resume --last`, `opencode --continue`, `aider --restore-chat-history`, with its auto mode); agents running when the registry is written are started that way with their workspace; an imported session resumes by its id. Provisioning a session id at launch: Later |
| Projects list (repos and folders, main clone per machine, clone from URL) | Adapt / Later | Recent folders per browser; a server-kept project list and cloning: Later |
| Machines list (saved SSH hosts, Test connection, ssh config hints, host-key prompt) | Adapt / Later | Host and target remembered per browser; SSH runs with `BatchMode`, so no host-key prompt |
| Kubernetes / devcontainer up | Skip | Containers attach to a running one (`docker exec`); no `devcontainer up` |
| Workspace persistence across restarts | Present | `~/.local/share/fresh-poc/orchestrator-<port>.json`, same ids |
| Disconnected / unreachable host: listed with why, Retry | Present | Not reconnected in the background |
| Archive (worktree to `.archived/`, unarchive) | Later | Close and Delete Worktree is there |
| Delete worktree (branch kept) | Present | `git worktree remove --force` |
| Stop (kill the agent's process group) | Adapt | Closing a terminal hangs up its process group; per-row Stop: Later |
| PR pills (gh) | Skip | Needs `gh` and network; out of scope |
| Git summary (↑ ahead, + changes) | Later | Branch only (read from `.git/HEAD`, no process) |
| Explain State, detection rules file / URL, Reload Detection Rules | Later | The rules are built in |
| Notifications settings (needs-you / all / off, bell sound) | Adapt | Status bar beacon and message; no settings |
| Import Sessions (Claude Code, Codex transcripts) | Present | `agents.scan` through the host's authority; opens New Workspace to resume |
| Import: tmux / screen / zellij panes, other orchestrators' worktrees, aider/opencode history | Later | Scanners to add to `src/agents.rs` |
| External live sessions (Claude Desktop, cloud), Teleport, Take Over | Skip | Needs those products' local and cloud APIs |
| Workboard: Inbox and Board lenses, `n`, Enter | Present | Dialog; lenses Tab / 1–2 |
| Workboard: Projects and Folders lenses, pins on the Board, time in state, sources API | Later | |
| Quick switcher | Present | Ctrl+Alt+O (the editor's Open dialog is the dock here) |
| Kill selected / bulk actions | Later | |

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
| Chrome | Menu bar: File, Edit, View, Selection, Go, LSP, Explorer, Help, from the command registry (keys shown, toggles checked, submenus); F10 and Alt+F / E / V / S / G / X / H, arrows, letters, Escape; plugins add rows (`fresh.menus.add`); one menu component for the menu bar and every context menu; styled by Winamp (engraved rail, olive plate) and Bridge (coloured segments in the top bar) | fixed |
| Chrome | Status bar items open their action: position → Go to Line, line ending → Set Line Ending, encoding → reopen / save with, language → Set Language (the language server follows), diagnostics → panel, LSP → server status, git branch → log, theme → Select Theme, read-only, cursors, compose; tooltips name the action and its key | fixed |
| Chrome | Skin picker button and Ctrl+Alt+T | present |
| Chrome | Theme picker | present |
| Chrome | Zoom: the editor's and terminals' text larger / smaller / reset (Ctrl+= / Ctrl+- / Ctrl+0, View menu), remembered | fixed |
| Layout | Drag to resize: editor splits (double-click: even) | fixed |
| Layout | Drag to resize: sidebar width, bottom dock and terminal height, right dock width (remembered per workspace; double-click: default) | fixed |
| Layout | Split right / down, close, next / previous split | present |
| Layout | Maximize split (toggle; the others hidden, not closed; focus moves it) | fixed |
| Layout | Scroll sync of same-buffer splits | missing (rarely used; after the rest) |
| Layout | Session restore per workspace (in the browser): the splits and ratios, each split's file tabs with cursor and scroll, the active tab, sizes, sidebar and terminal | fixed |
| Tabs | Close with ×, middle-click | present |
| Tabs | Context menu: Close, Close Others, Close to the Right, Close Saved, Close All, Split Right / Down, Copy Path / Relative Path, Reveal in Explorer | fixed |
| Tabs | Drag to reorder; drag onto another split's tabs or text to move it there (cursors, scroll, mode and folds kept; an emptied split closes) | fixed |
| Tabs | Wheel over the tab bar scrolls it | fixed |
| Tabs | Same-name tabs show their folder | present |
| Tabs | Preview (ephemeral) tab, italic: a single click or the arrows in the explorer, and selecting a project-search result or a reference, show the file in the split's preview tab (the keyboard stays in the list), replaced by the next preview; editing it, Enter or a double-click, clicking its tab, splitting, or Keep Open (tab menu, palette) keep it; Edit ▸ Settings ▸ Preview Tabs turns them off; remembered with the layout | fixed |
| Editor | Right-click context menu (Cut / Copy / Paste, Go to Definition, References, Rename, Code Actions, Hover, Format, comment / duplicate / delete line, select all / occurrences, Revert); a right press outside the selection moves the caret | fixed |
| Editor | Scrollbar: drag, click to jump | present |
| Editor | Scrollbar markers: git changes, search matches, diagnostics (in lanes), the cursor's line; for buffers held whole | fixed |
| Editor | Horizontal scrolling of long lines (wrap off): the cursor kept in view, Shift+wheel and sideways wheels | fixed |
| Editor | Line wrap toggle (Alt+Z), line numbers, current line, occurrences, whitespace, rulers | present |
| Editor | Indentation guides, matching-bracket highlight (theme's `bracket_match_fg`, bold), View toggles | fixed |
| Editor | Rainbow brackets | missing (needs bracket depth over the whole file, skipping strings and comments, kept through edits) |
| Editor | Minimap, breadcrumbs | skipped (the editor has neither) |
| Explorer | Sticky ancestors: the folders of the top visible row stay pinned (any UI-kit tree); a click scrolls back to the folder | fixed |
| Explorer | Drag and drop to move files and folders onto a folder (or the root), confirmed; open files follow | fixed |
| Explorer | New file / new folder / refresh / collapse-all buttons in the header (UI kit: `panel.actions`) | fixed |
| Explorer | Show hidden / gitignored files (hidden by default, as the editor; Explorer menu, checked), remembered | fixed |
| Explorer | Multi-selection: Ctrl+click toggles, Shift+click and Shift+Up/Down extend from the anchor, Ctrl+A all, Escape clears (and a pending cut, drawn dimmed); delete, copy, cut, paste, drag and the right-click menu act on the selection (UI kit: a list's `marked` rows) | fixed |
| Explorer | Open, new file / folder, rename, delete, copy / cut / paste, copy path, follow active file, gitignore dimming, keyboard | present |
| Files | Recent files (File ▸ Open Recent, and the file palette with nothing typed) | fixed |
| Files | Close All, Close Others | fixed |
| Files | Save As (the text written through `fs.write`, then opened; overwrite confirmed) | fixed |
| Files | Set line ending (LF / CRLF / CR), reopen or save with an encoding, set language, revert | fixed (server `doc.set`, `doc.reload` with `encoding`) |
| Files | New untitled buffer (Ctrl+N, File ▸ New File): `Untitled-N`, a server document bound to no file (`doc.new`); Save asks for a name (`doc.save_as`, which Save As uses too: the document moves to the new file); closing one with text asks Save / Discard / Cancel; restored on reload while the server keeps its unsaved text | fixed |
| Palette | Commands, files, buffers, go to line; `path:line:col` | present |
| Palette | Go to symbol in the file: the palette's `@` (Ctrl+Shift+O), previewed as selected, Escape goes back; plugins provide prefixes (`fresh.palette.provide`) | fixed |
| Palette | Go to symbol in the workspace: the palette's `%` (the editor has no such mode, and its `#` is buffers), Go ▸ / LSP ▸ Go to Symbol in Workspace…; `workspace/symbol` asked of every running server (`lsp.call` with a `language`) | fixed |
| Navigation | Back / forward, go to definition / references, matching bracket, paragraphs | present |
| Navigation | Next / previous problem (F8 / Shift+F8), its message in the status bar | fixed |
| Navigation | Bookmarks: Ctrl+Shift+0…9 set / clear, Alt+0…9 jump, Bookmarks: List; a gutter digit that moves with edits | fixed |
| Editing | Multi-cursor, selection, undo / redo, comment, duplicate / move line, fold, auto-close, smart home, kill ring | present |
| Editing | Uppercase / lowercase (Alt+U / Alt+L), sort lines, trim trailing whitespace (Edit ▸ Transform) | fixed |
| Editing | Expand / Shrink Selection (Ctrl+Alt+Right / Left; the editor binds no key): word, a string's inside and the string, each bracket pair's inside and the pair, the line's text and the line, indentation blocks, the document — or the language server's `textDocument/selectionRange` where it answers (pyright does not; the structure then); block selection: Alt+Shift+arrows, Alt+drag or a middle-button drag (a cursor a line, screen columns; the middle button never pastes); Transpose Characters (Ctrl+T) and Transpose Lines; keyboard macros: Record Macro… into a register 0–9, F5 stops, F4 plays the last, Play Macro…, List Macros, `● REC` in the status bar (commands and typed or pasted text; not what is typed into prompts) | fixed |
| Search | Find / replace bar with toggles, project search panel | present |
| Search | Find in Selection (and Find / Replace opened over a selection of several lines; the bar's ⊂ toggle), the range shaded (`--search-scope-bg`) and moved by edits; Find Selection Next / Previous (Ctrl+F3 / Ctrl+Shift+F3, Alt+N / Alt+P); Query Replace (Ctrl+Alt+R, or the replace row's ? toggle): at each match y, n, a or !, q or c, wrapping once; the current match drawn over the selection (decorations: `above`) | fixed |
| LSP | Hover, completion, definition, references, rename, code actions, format, symbols, signature help, diagnostics panel | present |
| LSP | Server status (status bar, `LSP: Show Status`) | fixed |
| LSP | Start/Restart and Stop a server (LSP menu, palette, and per server in Show Status; the status bar reads `LSP (off)` while stopped; server ops `lsp.restart` / `lsp.stop` on the workspace's manager), inlay hints (`textDocument/inlayHint` drawn as inline decorations in the line numbers' colour, asked again as edits settle; LSP ▸ Inlay Hints toggles) | fixed |
| Git | Gutter, blame, log, diff, change navigation, branch in status | present |
| Terminal | Multiple, tabs, toggle, keyboard capture (xterm) | present |
| Terminal | Restart the terminal, send the selection (or line) to it | fixed |
| Help | Welcome, keyboard shortcuts (keybinding editor), show log | present |
| Help | Settings screen | skipped (settings are palette toggles in the browser; a settings UI needs the editor's schema) |
| Workspaces | Orchestrator / multi-session management | present (dock Alt+O, switcher Ctrl+Alt+O, New Workspace, Run Agent, Import Sessions, Workboard, attention beacon) |

In all: 56 rows — 17 present, 28 fixed by this audit, 9 missing, 2 skipped.
What is missing is behind the chrome: preview tabs, explorer
multi-selection, scroll sync, rainbow brackets, untitled buffers,
workspace symbols, the rarer editing and search commands (transpose,
block and expand selection, macros, find in selection, query replace) and
restarting a language server.
