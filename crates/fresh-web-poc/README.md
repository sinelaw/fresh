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
listens on loopback only). `DIR` defaults to the current directory.

| Flag | Meaning |
|---|---|
| *(none)* | Local: files, processes and terminals on this machine. |
| `--ssh [user@]host[:port]` | The workspace is on another machine. Files go through the editor's remote agent (a Python script bootstrapped over one `ssh` connection: ranged reads, line counting, walking and searching on the remote side). Processes, terminals and language servers run there through `ssh` with the same port, identity and options; `BatchMode` is on for processes, so key-based login is required. Needs `python3` on the remote. |
| `--container NAME_OR_ID` | A devcontainer-style workspace: files are read on this machine; processes, terminals and language servers run in the running container through `docker exec -w <dir>`. `DIR` must be bind-mounted at the same path in the container; this is checked at start. |
| `--agent` | The remote agent run locally without `ssh`: the whole remote file path minus the transport, for testing it. Processes are local. |
| `--slow-network` | Adds network-like delays to every filesystem call (any authority). |
| `--restricted` | Start untrusted: no processes, terminals or language servers (and no probing for them) until the client trusts the workspace. Files and documents work. |

`FRESH_POC_WEB=crates/fresh-web-poc/web` serves the client from that folder
instead of the copy embedded at build time, so client changes need only a
reload.

## Architecture

**The server is the data authority.** It holds one workspace on one host,
reached through an `Authority` (`src/workspace.rs`): `Local`, `Ssh`,
`Container` or `Agent`. The authority supplies the host's filesystem (the
editor's `FileSystem` trait: `StdFileSystem` or the agent's
`RemoteFileSystem`) and builds every process — a client's `proc.run`, a
terminal's shell, a language server and the probe that finds the servers.
Nothing in the server touches the disk or spawns a program except through it;
the one exception is reading the client's own files under `FRESH_POC_WEB`.

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
unless it has unsaved edits, which wait for the next client. Saving refuses
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
document is refused (`exists`). Changes made outside the editor are not
followed: a save then finds the file changed (`changed_on_disk`).

**Terminals** (`src/terminal.rs`) are PTYs running the authority's shell;
the server streams raw bytes with stream offsets and keeps a replay buffer,
and each client runs its own emulator (xterm.js). Terminals belong to the
workspace and outlive clients. **Language servers** (`src/lsp.rs`): one per
language, kept in sync from the edit log; requests are routed in, and
diagnostics go out as data tagged with the version they describe.
**Watches** poll a path's size and modification time through the filesystem,
so they work on any host.

**The client is the editor** (`web/`): layout, views, cursors, undo,
keymaps, search, the palette and every decoration and marker live there,
and features are client plugins (`web/plugins/`) on a browser-native panel
API. See `PLAN.md` for how each editor feature is ported.

## Protocol

`POST /api` with a JSON object `{op, client, ...}` answers `{"ok": ...}` or
`{"error": {"code", "message"}}`: `code` is stable and meant for programs,
`message` is for people. `hello` registers a client and returns its id; the
client then opens `GET /events?client=ID`, a Server-Sent Events stream of
JSON events. When that stream ends the client is forgotten: its documents
are closed, its watches removed and its processes killed.

Operations:

- `hello`, `workspace.trust`
- `fs.list`, `fs.stat`, `fs.read`, `fs.write`, `fs.find`, `fs.create`,
  `fs.rename`, `fs.delete`, `fs.copy`, `fs.watch`, `fs.unwatch`,
  `search.project`
- `doc.open`, `doc.close`, `doc.info`, `doc.lines`, `doc.lines_before`,
  `doc.window`, `doc.text`, `doc.search`, `doc.line_start`, `doc.edit`,
  `doc.save` (`force` to overwrite an external change)
- `term.open`, `term.input`, `term.resize`, `term.attach`, `term.close`
- `proc.run`, `proc.kill`
- `lsp.call` (any request about a document), `lsp.hover`,
  `lsp.definition`, `lsp.completion`, `lsp.status`

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
| `binary` | `doc.open`: a binary file |
| `too_large` | A whole read over the cap (`fs.read`, `doc.text`) |
| `no_language_server` | No running language server for the document's language |
| `language_server` | The language server answered with an error, or not in time |
| `permission_denied` | The host refused the file operation |
| `io` | Any other I/O error on the host |
| `failed` | Anything else |

Events: `doc.ops`, `doc.saved`, `doc.renamed` (`{doc, from, info}`),
`doc.deleted` (`{doc, info}`), `doc.indexing`, `doc.indexed`,
`lsp.diagnostics`, `term.data`, `term.size`, `term.exit`, `proc.exit`,
`fs.changed`, `workspace.trusted` (with the available language servers).

## Limits

- Killing a process or closing a terminal kills the local `ssh` or
  `docker exec` client; the remote or in-container process ends when its
  input closes or on hang-up, not by signal.
- Ignore rules are the root's `.gitignore` and `.git/info/exclude`;
  nested `.gitignore` files are not read.
- One workspace per server; the SSH connection is not re-established if it
  drops.
