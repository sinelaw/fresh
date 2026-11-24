# fresh-poc

A proof of concept of Fresh split in two: a Rust **data server** that owns a
workspace on some host, and a thick **web client** that is the whole editor
experience. It reuses the editor's engine where it matters — the lazy
piece-tree buffer, its filesystem abstraction and the remote agent — and
nothing of its UI.

## Running

```
cargo run -p fresh-web-poc -- [--port N] [--restricted] \
    [--ssh [user@]host[:port] | --container NAME_OR_ID | --agent] [--slow-network] [DIR]
```

Then open the printed URL (`http://127.0.0.1:8090/` by default; the server
listens on loopback only). `DIR` (default: the current directory) on the
host the flags name is the first workspace; the web client opens more, on
any of these hosts, and moves between them (see Orchestrator).

| Flag | Meaning |
|---|---|
| *(none)* | Local: files, processes and terminals on this machine. |
| `--ssh [user@]host[:port]` | The workspace is on another machine. Files go through the editor's remote agent (a Python script bootstrapped over one `ssh` connection: ranged reads, line counting, walking and searching on the remote side). Processes, terminals and language servers run there through `ssh` with the same port, identity and options; `BatchMode` is on for processes, so key-based login is required. Needs `python3` on the remote. |
| `--container NAME_OR_ID` | A devcontainer-style workspace: files are read on this machine; processes, terminals and language servers run in the running container through `docker exec`. `DIR` must be bind-mounted at the same path in the container; this is checked at start. |
| `--agent` | The remote agent run locally without `ssh`: the whole remote file path minus the transport, for testing it. Processes are local. |
| `--slow-network` | Adds network-like delays to every filesystem call (any authority). |
| `--restricted` | The first workspace starts untrusted: no processes, terminals or language servers (and no probing for them) until the client trusts it. Files and documents work. |

`FRESH_POC_WEB=crates/fresh-web-poc/web` serves the client from that folder
instead of the copy embedded at build time, so client changes need only a
reload.

## Architecture

**The server is the data authority.** It holds workspaces
(`src/orchestrator.rs`), each a root on a host reached through its own
`Authority` (`src/authority.rs`): `Local`, `Ssh`, `Container` or `Agent`.
Each workspace (`src/session.rs`) owns its trust, documents, terminals,
processes, watches and language servers; nothing is shared between two. The authority supplies the host's filesystem (the
editor's `FileSystem` trait: `StdFileSystem` or the agent's
`RemoteFileSystem`) and builds every process — a client's `proc.run`, a
terminal's shell, a language server and the probe that finds the servers.
Nothing in the server touches the disk or spawns a program except through it;
the one exception is reading the client's own files under `FRESH_POC_WEB`.

Every process is one `sh` script on the host, built the same way for every
authority. On another host (SSH, container) the program runs in the user's
login environment there (`$SHELL -l`, its stdout output discarded), so `PATH`
is an interactive login's; on this machine it inherits the server's
environment. The script records its pid in `/tmp/fresh-poc-*.pid` on the
host and leads its own process group (sshd and `docker exec` start it as a
session; locally the server starts it as a group), so killing a process
signals its whole group on the host (`SIGKILL`), closing a terminal hangs up
its shell there (`SIGHUP`, passed on to its jobs), and a process stopped
before its script ran exits at once. A disconnected client's processes, and
on `SIGINT`/`SIGTERM` everything the server started, are stopped this way.

**Documents** (`src/docs.rs`) are the editor's lazy piece-tree `Buffer`
behind a windowed API: a client asks for runs of lines around a byte offset
and never receives a whole file, so a multi-gigabyte file costs what its
window costs. A file over 10 MB opens without a line index; the index is
built in the background, counting newlines through the filesystem (on the
remote side over SSH). Each document has one ordered edit log: clients edit
optimistically against a log version, the server transforms late edits past
what was sequenced since (`src/ot.rs`, mirrored by `web/js/ot.js`) and
broadcasts each batch with the newlines it changed. A document is shared by
every client that opens it; it is dropped when the last one closes it,
unless it has unsaved edits, which wait for the next client. Closing with
`discard` gives them up instead: when no other client holds the document it
is dropped, and the file opens again as it is on disk; while others hold it,
the edits are theirs too and stay (the reply says how many others there
are). The client asks Save / Discard / Cancel when the last tab of a
modified file (or a split holding it) closes. Saving refuses
if the file changed on disk since it was loaded or last saved, unless forced.

Documents follow the workspace's own file operations. `fs.rename` of an
open file, or of a folder holding open files, moves their documents with
them: the path changes, the language is detected again from the new name,
the language server gets `didClose` for the old URI and `didOpen` for the
new, and every client holding one gets `doc.renamed` and relabels its
buffer; a later save writes the new path. `fs.delete` of an open file keeps
its document with its text, marked unsaved, and its clients get
`doc.deleted`: closing it asks to save or discard like any unsaved buffer,
and saving writes the file again (with its folder). A huge file open as a
document is read from disk as it is viewed, so renaming or deleting it
while open is refused (`in_use`). Renaming onto the name of an unsaved
document is refused (`exists`).

Documents also follow changes made outside the editor (a terminal, `git
checkout`, another tool). The server polls each open document's file
signature (size, mtime) through the filesystem every half second; its own
saves record the signature they leave, so they are never taken for a
change. When the file changed:
- with no unsaved edits, the new text is applied as an edit from the server
  (client `0`): a line diff trimmed to the characters that differ, sent as
  ordinary `doc.ops`, so every client's text, cursors, decorations and undo
  carry through it as through anyone's edit;
- with unsaved edits, the clients get `doc.changed_on_disk` once per change
  and offer Reload (`doc.reload`: the disk's text replaces every client's
  unsaved edits, as edits) or Keep mine (the next save is forced);
- a huge document reads its unedited text from the file, so it cannot keep
  anything over a change: it is loaded again with a new log, and clients get
  `doc.reloaded` (`lost` when it had unsaved edits) and fetch their windows
  again; edits based on the old log are refused (`resync`);
- a deleted file is handled as `fs.delete` is (`doc.deleted`).

**Terminals** (`src/terminal.rs`) are PTYs running the authority's shell
(or a command in it: an agent session); the server streams raw bytes with
stream offsets and keeps a replay buffer, and each client runs its own
emulator (xterm.js). Terminals belong to the workspace and outlive clients.
The server also reads each terminal's stream for the orchestrator: the title
a program sets, the bell, and its state — `working` (output in the last two
seconds), `waiting` (quiet, its last line a question or a `[y/n]` prompt),
`idle`, `exited`. **Language servers** (`src/lsp.rs`): one per
language, kept in sync from the edit log; requests are routed in, and
diagnostics go out as data tagged with the version they describe.
**Ignore rules** (`Ignores` in `src/workspace.rs`) are git's:
`.git/info/exclude`, the root's `.gitignore` and every nested `.gitignore`,
a deeper file deciding over a shallower one, and nothing inside an ignored
folder is ever included again. They are read through the filesystem; file
finding and project search leave ignored files out, and `fs.list` marks each
entry `ignored` (the explorer dims them). **Watches** poll a path's size and modification time through the filesystem,
so they work on any host.

**The client is the editor** (`web/`): layout, views, cursors, undo,
keymaps, search, the palette and every decoration and marker live there,
and features are client plugins (`web/plugins/`) on a browser-native panel
API. See `PLAN.md` for how each editor feature is ported.

## Protocol

`POST /api` with a JSON object `{op, client, ...}` answers `{"ok": ...}` or
`{"error": {"code", "message"}}`: `code` is stable and meant for programs,
`message` is for people. The client sends its operations that change
documents or files (`doc.open`, `doc.close`, `doc.edit`, `doc.save`,
`doc.reload`, `fs.write`, `fs.create`, `fs.rename`, `fs.delete`, `fs.copy`,
`workspace.trust`) one at a time, each after the previous one is answered,
so the server applies them in the order they were made; reads, searches and
language-server requests are not held back. `hello` registers a client and returns its id; the
client then opens `GET /events?client=ID`, a Server-Sent Events stream of
JSON events. When that stream ends the client is forgotten: its documents
are closed, its watches removed and its processes killed.

A client views one workspace. `hello {workspace?}` binds it to that one (by
default the first open), and answers `{client, workspace, workspaces}`:
`workspace` is what the client starts from (`{id, name, root, host,
trusted, terminals, lsp}`), or `null` when the one asked for is closed (or
none is open); `workspaces` is every open workspace (`{id, name, root, host,
status}`). Every other operation acts on the client's workspace; a client
of a closed one gets `not_found`. Viewing another workspace is a new
client.

Operations:

- `hello`, `workspace.trust`, `workspace.seen` (the user has seen what
  needed attention)
- `workspace.list`, `workspace.open` (`{root, host, trusted, agent}`; `host`
  is `{kind: "local"}`, `{kind: "ssh", target}`, `{kind: "container", id}`
  or `{kind: "agent"}`; `~` is the home folder on that host; a folder
  already open on the same host answers that workspace as it is; `agent`, a
  command, starts an agent session in it, which needs `trusted`; answers
  `{workspace, agent}`), `workspace.close` (`{workspace, discard}`: stops its
  terminals, processes and language servers; refused with `unsaved` while
  it holds unsaved documents, unless `discard`)
- `fs.list`, `fs.stat`, `fs.read`, `fs.write`, `fs.find`, `fs.create`,
  `fs.rename`, `fs.delete`, `fs.copy`, `fs.watch`, `fs.unwatch`,
  `search.project`
- `doc.open`, `doc.close` (`discard` to give up unsaved edits; answers
  `{dropped, others}`), `doc.info`, `doc.lines`, `doc.lines_before`,
  `doc.window`, `doc.text`, `doc.search`, `doc.line_start`, `doc.edit`,
  `doc.save` (`force` to overwrite an external change), `doc.reload`
  (`encoding` to read the file in that one), `doc.formats` (the languages,
  encodings and line endings a document can be set to), `doc.set` (a
  document's `language`, or the `line_ending` and `encoding` its next save
  writes)
- `term.open` (`{cols, rows, name, command}`: the login shell, or
  `command` in it — an agent session), `term.input`, `term.resize`,
  `term.attach` (the replay, and `{name, command, agent, title, state}`),
  `term.close`
- `proc.run`, `proc.kill`
- `lsp.call` (any request about a document), `lsp.hover`,
  `lsp.definition`, `lsp.completion`, `lsp.status`
- `theme.list`, `theme.get` (the editor's built-in themes and the skins'
  themes, resolved)
- `skin.list` (every skin's header), `host.stats` (the host's load, for skin
  readouts); `GET /skins/<id>/<path>` serves a built-in skin's files,
  `GET /workspaces/<ws>/skins/<id>/<path>` any skin's as that workspace
  sees it (a user skin is read from its host)

Error codes (`src/error.rs`):

| Code | Meaning |
|---|---|
| `bad_request` | A missing or mistyped argument, or one out of range |
| `unknown_op` | No such operation |
| `unknown_client` | The client has no live event stream (no `hello`, or the stream ended) |
| `untrusted` | The workspace is restricted; trust it to run processes, terminals and language servers |
| `not_found` | No such file, folder, document or terminal |
| `exists` | The target name is taken, by a file or by an open document |
| `changed_on_disk` | `doc.save`: the file changed since it was loaded or last saved; `force` overwrites it |
| `in_use` | `fs.rename`/`fs.delete` of a huge file open as a document (it is read from disk as it is viewed) |
| `resync` | `doc.edit`: the base version is older than the kept log; reopen the document |
| `unsaved` | `workspace.close`: the workspace holds unsaved documents; `discard` closes it anyway |
| `binary` | `doc.open`: a binary file |
| `too_large` | A whole read over the cap (`fs.read`, `doc.text`) |
| `no_language_server` | No running language server for the document's language |
| `language_server` | The language server answered with an error, or not in time |
| `permission_denied` | The host refused the file operation |
| `io` | Any other I/O error on the host |
| `failed` | Anything else |

Events: `doc.ops`, `doc.saved`, `doc.renamed` (`{doc, from, info}`),
`doc.deleted` (`{doc, info}`), `doc.changed_on_disk` (`{doc, info}`),
`doc.reloaded` (`{doc, lost, info}`), `doc.info` (`{doc, info}`, after `doc.set`), `doc.indexing`, `doc.indexed`,
`lsp.diagnostics`, `term.data`, `term.size`, `term.exit`, `proc.exit`,
`fs.changed`, `workspace.trusted` (with the available language servers) —
to the clients of the workspace they concern; and to every client,
`workspace.added` (`{workspace}`), `workspace.removed` (`{workspace: id}`)
and `workspace.status` (`{workspace: id, status}`, when it changes).

A workspace's `status`: `trusted`; `docs` and `modified` (open and unsaved
documents); `terminals` (each `{id, name, command, agent, title, state,
bells}`); `procs`; `errors` and `warnings` (from the language servers'
latest reports for its open documents); `branch` (read from `.git/HEAD`
through its filesystem); `viewers` (clients viewing it); and `attention`,
why it needs the user — a terminal rang the bell, or an agent session went
from working to waiting, idle ("finished") or exited — until a client
viewing it sends `workspace.seen`.

## Orchestrator

One server, many workspaces — the editor's Orchestrator, for this split.
Each workspace is a folder on a host (this machine, an SSH host, a
container, the local agent) with its own trust, files, terminals, agents
and language servers. A browser tab views one at a time (`?ws=<id>`);
switching loads the page for the other one, so its tabs, splits, terminals
and plugins come back as this browser left them and never mix with
another's. Two tabs can view two workspaces side by side.

- **Workspaces** (Alt+O, or the workspace's name in the top bar): every
  workspace with its live status — `●` it needs you (and why), `*` an agent
  is working; its branch, host and folder, `restricted`, open and unsaved
  files, terminals, each agent session and its state, diagnostics, how
  many pages view it — with **switch** and **close** (Enter, Delete).
- **Switch Workspace…** (Ctrl+Alt+O): the same as a quick pick.
- **New Workspace…**: a folder (`~` for home, recent ones offered), the host,
  whether to trust it, and optionally an agent command (a coding agent's
  CLI, or any long-running program) to start in a terminal there. **Open**
  (Ctrl+Enter) switches to it; **Open in Background** (Alt+Enter) stays.
- **Start Agent Session…**: a terminal running a command here; its tab
  shows its state (`*` working, `●` waiting for you, `·` idle, `✕` exited).
- The status bar counts the other workspaces that need you
  (`● 1 needs you`); a click, or **Jump to Attention**, switches to the one
  that has. Viewing a workspace that needed you says why in the status bar.
- **Close Workspace** stops its terminals and agents (asking first) and
  asks before discarding unsaved files; pages viewing it are told and show
  the overview.

## Themes and colours

Every colour the client draws is a CSS variable; `web/style.css` names no
colour except in `:root`, which holds the dark theme's values. Theme keys
that plugins pass as `fg`/`bg` (`"syntax.keyword"`, `"editor.selection_bg"`,
`"search.match_bg"`, ...) resolve to these variables (`THEME` in
`web/js/highlight.js`), so whatever is drawn follows a theme change without
being drawn again; the terminal (a canvas) copies them when they change.

The themes are the editor's own (`crates/fresh-editor-core/themes`, embedded
in the binary by the core crate): `theme.list` names them and `theme.get`
returns one fully resolved by the editor's loader. `web/plugins/themes.js`
maps a theme's keys onto every variable (`themeVars`), so each theme styles
the whole UI; `Select Theme…` previews live and the choice is remembered in
the browser. A plugin sets them with `fresh.ui.theme({"--bg": ...})`; a
skin overrides them in its stylesheet (see Skins).

| Variable | What it colours |
|---|---|
| `--bg`, `--fg` | the page and the editor text |
| `--muted`, `--faint` | secondary text; line numbers and revealed Markdown markup |
| `--border`, `--panel` | separators; sidebars, docks, tab and top bars |
| `--accent`, `--on-accent` | focus marks, primary buttons, toggles; text on them |
| `--emphasis`, `--link` | highlighted names (keys, hashes, the brand); links |
| `--status-bg`, `--status-fg` | the status bar |
| `--control-bg`, `--control-fg` | inputs, buttons, selects, badges |
| `--hover-bg` | a row under the pointer |
| `--sel-bg`, `--sel-fg`, `--sel-inactive-bg` | the selected row (focused list, palette, menu); in an unfocused list |
| `--popup-bg`, `--popup-fg`, `--popup-border` | palette, popups, context menus, dialogs |
| `--shadow` | shadows and the dialog backdrop |
| `--terminal-bg`, `--terminal-fg` | the terminal |
| `--caret`, `--selection`, `--current-line`, `--occurrence` | the editor's cursor, selection, current line, occurrences |
| `--whitespace`, `--ruler` | whitespace indicators; rulers |
| `--match-bg`, `--match-fg`, `--match-current-bg`, `--match-current-fg` | search matches |
| `--code-bg` | code in documents and Markdown text |
| `--tour-step-bg`, `--diff-add-bg`, `--diff-remove-bg` | a tour's step line; added and removed lines |
| `--git-added`, `--git-modified`, `--git-deleted` | git marks |
| `--syntax-keyword`, `-string`, `-comment`, `-function`, `-type`, `-variable`, `-constant`, `-number`, `-operator`, `-punctuation` | syntax |
| `--error`, `--warning`, `--info`, `--hint` | diagnostics |

## Skins

A theme colours the page; a **skin** dresses it: the frame around the
editor, where the chrome goes and how it looks, its type and density, and the
editor theme that paints the buffers. Built in: **Default** (the page's own
look), **Compact** (a dense frame mixed from any editor theme),
**Winamp Classic** (a brushed-metal player window with LCD content) and
**Bridge** (a starship console, with its own editor theme and font). Pick one
with **Select Skin…** in the palette or the skin button at the end of the top
bar (moving through the list tries each on; Escape puts the last one back),
or cycle with **Ctrl+Alt+T**. The choice is per browser; `?skin=default` in
the URL overrides it, to get back from a skin that hides the way.

A skin is a folder of plain web files and runs no code:

```
my-skin/
  skin.css         the skin (required), starting with its header
  furniture.html   decorative markup for a bezel frame (optional)
  theme.json       an editor theme that comes with the skin (optional)
  img/, fonts/     anything the CSS refers to (optional)
```

Built-in skins are `web/skins/NN-<id>/` (embedded with the client; the number
orders the list). Your own go in `~/.config/fresh/web-skins/<id>/` on the
workspace's host — read through the workspace's authority, like any other
file, so over SSH that is the remote home — and appear in the picker on its
next opening. A user skin cannot reuse a built-in id. The server serves a
skin's files at `skins/<id>/`, so the stylesheet's relative `url()`s work.

### The header

```css
/* ==FreshSkin==
@id           neon-night
@name         Neon Night
@description  Dark glass with a magenta accent
@extends      compact
@frame        bezel
@scale        1
@furniture    furniture.html
@editor-theme theme.json
==/FreshSkin== */
```

| Key | Meaning |
|---|---|
| `@name` | Shown in the picker (default: the id) |
| `@id` | `a-z`, `0-9`, `-`, up to 48 characters; default: the folder's name without its `NN-` |
| `@description`, `@author` | Shown in the picker |
| `@extends` | Another skin's id: its stylesheet, frame, scale, furniture and theme come first, and this one layers on top |
| `@frame` | `full` (the client fills the window, the default) or `bezel` (the client sits inset in `#device`, with the page's background around it) |
| `@scale` | Density, `0.5`–`1.5`: the chrome's text and the editor's font (the editor's cell is measured again, terminals refit) |
| `@furniture` | An HTML file built inside `#device` |
| `@editor-theme` | A `.json` theme file in the folder (a `theme.json` is found without this line), or an editor theme's key (`nord`, `builtin://nord`) |

Picking a skin with an editor theme switches the editor theme too (it is an
ordinary choice after that: Select Theme… changes it). A skin's own theme is
a theme like the editor's: `skin/<id>` in `theme.list`, resolved by the
editor's loader (so `"extends": "builtin://dark"` works), and listed in
Select Theme… as "<name> (skin)".

### The skinning surface

A skin is CSS against the client's stable structure. `<html>` carries
`skin-<id>` for the skin and every skin it extends, and `frame-full` or
`frame-bezel`. The parts:

| Selector | What it is |
|---|---|
| `#device` | the frame a bezel skin's furniture is built in; `#app` is inset in it by `--shell-pad` and `--bezel-top`/`-side`/`-bot` (set them on `:root.skin-<id>`); `.dv-screen` is a dark well around the client |
| `#app` | the window: `#topbar`, `#main`, `#status`, top to bottom |
| `#topbar` | `.brand`, `#menubar` (the menu bar: `.mtop` titles, `.mtop.open`; a column when a skin lays it out as one, and its menus open beside it), `#ws-name` (the workspace), `.hint`, `#skin-btn` |
| `#main` | `#sidebar` (`#side-panels`: plugin panels, the explorer first), its `.sash`, and `#work` |
| `#work` | `#bar` (a plugin bar), `#center` (`#editor-area`, `.sash`, `#right-dock`), `.sash`, `#dock` (`.dock-tabs`, `.dock-body`), `.sash`, `#terminal` (`.term-tabs`, `.term-screens`) |
| `.sash` | a drag handle between two parts (`.sash.x` moves sideways, `.sash.y` up and down; `.dragging` while dragged): no size in the layout, its grip reaches 3px either side; a split's sash is its dividing line (its border) |
| `#editor-area` | `.split.h` / `.split.v` of `.pane`s with a `.sash` between them; a pane is `.tabs` (`.tab`, `.tab.active`, `.x`) over `.surface`; the focused pane is `.pane.active`; while a tab is dragged, `<html>` has `.tab-dragging` and its landing place is `.tab.drop-before` / `.tab.drop-after` or `.pane.drop-into` |
| `.surface` | a buffer: `.gutter`, `.text` (`.row`, `.row.current`), `.carets`, `.scrollbar` / `.thumb` and `.marks` (`.mark.lane0` git, `.lane1` search, `.lane2` diagnostics, `.mark.cursor`); in compose mode `.surface.compose` holds `.doc` / `.doc-content` / `.doc-body` (headings, `.doc-li`, `.doc-code`, `.doc-table`) |
| `#status` | `#status-left` (the message), `#status-right > span` (the facts; `.click` when clickable) |
| `#prompt` | the palette and prompts: `.bar` (`.label`, `input`), `.list` (`.item`, `.item.sel`, `.d` its detail), `.hint` |
| `#popup`, `.ctxmenu` | hover and other pop-ups; every menu — the menu bar's, their submenus and the context menus: `.menu-item` rows (`.check`, `.label`, `.keys`; `.hi` highlighted, `.sub`, `.disabled`), `hr` separators, `.checks` on a menu with a check column |
| `.panel` | a plugin panel: `.panel-head` (`.t`, `.panel-actions` > `.panel-action` buttons, `.x`), `.panel-body`; placed as `.panel-sidebar`, `.panel-right`, `.panel-bar`, `.panel-dialog` (over a `.backdrop`) |
| `.ui-*` | the panel kit: `.ui-list` / `.ui-list-row` (`.sel`, `.header`; a tree's pinned ancestor rows `.sticky`, the innermost `.last`; while a row is dragged the list is `.dragging` and its landing row (or the list) `.drop-target`), `.ui-tree-row`, `.ui-input`, `.ui-button` (`.primary`), `.ui-toggle` (`.on`), `.ui-select`, `.ui-badge`, `.ui-table`, `.ui-markdown` |

**Colours.** The theme sets its variables (the table above) on `<html>`. A
skin overrides any of them on `body` or on a part — the parts below inherit
from there — and leaves the buffers' ink to the theme by scoping the chrome's:

```css
.skin-neon-night :is(#topbar, #sidebar, #status, .tabs, #prompt, #popup, .ctxmenu) {
  --fg: #e6e1f0; --muted: #9a93ad; --panel: #16131f; --accent: #ff2fa0;
}
```

To follow any editor theme, mix from its ground and ink, as Compact does:
`--shell: color-mix(in srgb, var(--bg), var(--fg) 4%)`.

**Type.** On `body`: `--ui-font` (the chrome's face), `--ui-font-size` (and
`--ui-font-sm`, `--ui-font-xs`, derived from it), `--editor-font-size` (the
buffers and terminals; keep `--mono` monospace). All follow `@scale` unless
the skin sets them. A font can come with the skin: an `@font-face` whose
`src` is a file in its folder, as Bridge bundles Antonio.

**Layout** is plain CSS too: Bridge takes the top bar and the status bar out
of the window's flow (`position: fixed`) into its frame's bars, and Winamp
draws its title bar and foot around the client.

### Furniture and live values

`furniture.html` is decoration built inside `#device`: the client rebuilds it
from `div`, `span`, `i`, `b`, `em`, `strong`, `small` and `p` elements,
keeping only their `class`, `data-bind` / `data-bind-var` and text; anything
else is dropped, and it takes no pointer events. Furniture can show live
values: `<b data-bind="cpu">–</b>` shows one as text,
`<p data-bind-var="--v:mem">` sets it as a custom property for CSS
(`width: calc(var(--v) * 1%)`). The values: `cpu`, `mem` (percent), `memText`,
`load`, `uptime` — the workspace host's, read from its `/proc` through the
authority (`host.stats`; absent on a host without one) — and `buffers`,
`errors`, `warnings`, `time` from the client. They refresh every few
seconds while some furniture shows them.

### Writing one

Copy a built-in skin into `~/.config/fresh/web-skins/`, change `@id` and
`@name`, and pick it. While developing the built-in ones,
`FRESH_POC_WEB=crates/fresh-web-poc/web` serves them from the source tree:
re-picking the skin (or reloading) shows a change. Check it with the
explorer, a source file with a selection, a split, the terminal, the
palette, a context menu, a plugin dialog (Keyboard Shortcuts), and a Markdown
file in compose mode, under a dark and a light editor theme.

## Type checking the client

The client is plain JavaScript checked by [inty](https://github.com/sinelaw/inty)
(types are inferred; where one is stated it is a `/** … */` annotation, so
the files run as they are):

```
cargo install --git https://github.com/sinelaw/inty inty-cli
crates/fresh-web-poc/typecheck.sh        # INTY=path/to/inty to pick the binary
```

It checks every module under `web/js` and `web/plugins` (not the vendored
xterm.js) in one run, with two declaration files: `types/browser.d.js`
(browser APIs inty's DOM library leaves out) and `types/fresh.d.js` (the
shapes modules share: edit primitives, the server's events, documents,
diagnostics, colours, syntax spans).

`ot.js`, `rpc.js`'s functions, `diff.js`, `mdstruct.js` and `highlight.js`
are typed. The rest does not check yet: inty does not parse `export class`,
a class constructor that does more than assign fields, `extends`, or default
and destructured parameters of class methods; a default parameter can't be
left out of a call; and a record's optional field (`fg?: Color`) can't be
read, so option bags and partial styles — decorations, the UI kit's
properties, most of the plugin API — have no type yet.

## Limits

- Ignore rules are git's own files only (`.git/info/exclude` and every
  `.gitignore`), not `core.excludesFile`. Each folder's `.gitignore` costs one
  read through the filesystem per walk (a round trip on a remote host).
- The open workspaces are not remembered across a server restart (each
  browser remembers its own layout per workspace); an SSH connection is not
  re-established if it drops. Agent sessions are terminals: the state is
  read from the stream, not from the agent; there is no worktree creation,
  project list or import of sessions run elsewhere.
