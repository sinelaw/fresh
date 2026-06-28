# System Overview & Runtime Model

Purpose: the keystone map of Fresh — an 8-crate Rust workspace that ships a terminal text editor. This doc covers the crate split and why it exists, cargo feature gating, the main entrypoint and ~60fps event loop, the threading model, the client/server+daemon architecture, async message flow, the `Editor` god object, and the Action-vs-Event distinction. It indexes the sibling subsystem docs at the end. Code references are `path:line` against the tree at the time of writing; treat code as authoritative where this and any older design doc disagree. Status: **IMPLEMENTED** unless flagged **PLANNED**.

All paths are under `crates/` unless noted. The older user-facing `docs/architecture.md` is a good but partial starting point and uses a stale `src/...` layout — the real layout is `crates/fresh-editor/src/...`.

---

## 1. Workspace layout & the crate DAG

Root `Cargo.toml:3-21` declares 8 members. `fresh-gui` is a member but **not** in `default-members` — it only builds when explicitly requested (`Cargo.toml:13-21`).

| Crate | Role | Local deps |
|---|---|---|
| `fresh-editor` | The `fresh` binary + all runtime subsystems | core, parser-js, languages, plugin-runtime, plugin-api-macros, gui, winterm (all optional / target-gated) |
| `fresh-core` | Dependency-light shared models, ID types, plugin API surface | — (leaf) |
| `fresh-parser-js` | JS/TS transpile, bundle, `.d.ts` emit (oxc toolchain) | — (leaf) |
| `fresh-languages` | tree-sitter grammars + `Language`/`HighlightCategory` enums | — (leaf) |
| `fresh-plugin-runtime` | QuickJS (rquickjs) plugin host on a dedicated thread | core (feature `plugins`), parser-js, plugin-api-macros |
| `fresh-plugin-api-macros` | proc-macro: Rust API impl → TypeScript `.d.ts` | — (leaf, proc-macro) |
| `fresh-gui` | winit + wgpu native window backend (`publish = false`) | core |
| `fresh-winterm` | Windows console VT input + relay; empty crate off-Windows | — (leaf) |

Dependency DAG (local path deps only):

```
fresh-editor ──┬─► fresh-core
               ├─► fresh-parser-js
               ├─► fresh-languages
               ├─► fresh-plugin-runtime ─► fresh-core (feature=plugins)
               │                          ├► fresh-parser-js
               │                          └► fresh-plugin-api-macros
               ├─► fresh-gui ─► fresh-core
               └─► fresh-winterm   (Windows target dep)
```

`fresh-core` is the universal sink; `fresh-editor` is the root that pulls in everything.

### Why `fresh-core` is separate from `fresh-editor`

`fresh-core` is deliberately **dependency-light**: only serde/serde_json/schemars/anyhow/lsp-types/ts-rs/unicode-width plus an *optional* `rquickjs` gated behind feature `plugins` (`fresh-core/Cargo.toml`). No tree-sitter, ratatui, crossterm, tokio, or platform crates. It holds pure-data ID types and shared models (`fresh-core/src/lib.rs`): `CursorId`, `SplitId`/`LeafId`/`ContainerId`, `BufferId`, `TerminalId`, `WindowId`, `SplitDirection`, plus submodules `action`, `api`, `command`, `hooks`, `config`, `menu`, `overlay`, `services`, `text_property`, `file_explorer`, `file_uri`, `display_width`. The reasons for the boundary:

- **Shared by three crates** — `fresh-plugin-runtime` and `fresh-gui` need the command/action/hook/menu/ID models without dragging in the editor's heavy dependency tree (e.g. `fresh-gui` consumes `fresh_core::menu::{Menu, MenuContext}` as the single menu source of truth).
- **`ts-rs` export boundary** — these types are `#[ts(export)]` so the plugin API and `.d.ts` generation reference one canonical definition.
- **WASM / schema friendliness** — pure-Rust + serde links cleanly into the wasm target and into the minimal schema-generation builds.

The crate was introduced in `e8246d70f` ("Refactor into Cargo workspace with modular crate architecture"). The settled pattern in git history (e.g. `4beda963d` `Session`, `732da005f` banners): plugin-visible **IDs and data models live in `fresh-core`**, while the **behavioral logic lives in `fresh-editor`**.

`fresh-editor` by contrast holds the actual editor: the `fresh` binary and all runtime subsystems (`app`, `input`, `services`, `state`, `client`, `server`, `webui`, `view`, `model`, `primitives`) and the entire heavy stack (crossterm, ratatui, tokio, syntect, alacritty_terminal, lsp-types, portable-pty, notify).

---

## 2. Feature gating: `runtime` / `wasm` / `dev-bins`

`fresh-editor/Cargo.toml:38-127` defines the features. `default = ["plugins", "runtime", "embed-plugins", "tree-sitter", "http"]`.

- **`runtime`** — the big one: all heavy native deps (crossterm, ratatui, tokio, syntect, alacritty_terminal, portable-pty, lsp-types, notify, libc/nix, `fresh-languages`). The `fresh` binary has `required-features = ["runtime"]`.
- **`plugins`** — pulls in `fresh-plugin-runtime`/`fresh-parser-js`/`fresh-plugin-api-macros` + oxc to syntax-check `init.ts`.
- **`tree-sitter`** = `["fresh-languages/bundled-languages"]` — tree-sitter AST features; off ⇒ indentation falls back to regex pattern rules.
- **`embed-plugins`** — bakes plugins into the binary as a fallback.
- **`http`** — `dep:ureq`; drops the whole TLS stack when off.
- **`gui`** (not default) — `["runtime", "dep:fresh-gui"]`; native wgpu window.
- **`dev-bins`** (not default) — gates the dev binaries `generate_schema`, `event_debug`, `measure_startup`. `generate_schema` emits JSON Schema from Rust structs (`Config`, `ThemeFile`, `PackageManifest`) via `schemars::schema_for!`.
- **`wasm`** (not default) — browser build of the shared editor core *without* native deps; swaps syntect's onig (C library) for pure-Rust `regex-fancy`, enables crossterm event types only (no backend), and omits tokio/pty/lsp/alacritty entirely.

The same discipline is enforced **inside** `fresh-editor` via `#[cfg]` in `src/lib.rs`: `model`, `primitives`, `widgets`, `config`, `types`, `plugin_schemas` are "always available (pure Rust)"; `view` is `cfg(any(runtime, wasm, dev-bins))`; and `app`/`input`/`services`/`state`/`client`/`server`/`webui` are `cfg(feature = "runtime")` only (`lib.rs:15-77`). The rationale (commit `7541d884b`): only pure-Rust modules compile to `wasm32`, and `config`/`types`/`view::theme` must build in a minimal config so `generate_schema` runs with just serde + schemars. Keeping the core dependency-light is the within-crate version of the `fresh-core` split.

---

## 3. Entrypoint & the ~60fps event loop

Entry: `fresh-editor/src/main.rs` (`main` `:3528` → `real_main` `:3744`). Startup sequence:

1. `main` (`:3528`) wraps `real_main`, printing a clean message (no backtrace) for `SshError` (an expected user failure, not a bug).
2. `real_main` (`:3744`): init i18n *before* clap parses (so `--help` is localized), parse CLI (`:3765`), set `FRESH_INTERACTIVE=1` for genuine interactive launches.
3. `run_if_subcommand` (`:3786`) dispatches non-editor invocations: `--server` (daemon), `--attach`/`-a`, `--cmd daemon|config|grammar|init`, `--stdin`, and the GUI path. These never enter the TUI loop.
4. `try_forward_nested` (`:3799`) — if launched from inside Fresh's own embedded terminal (`FRESH_SESSION` set), forward file/dir opens to the parent editor instead of starting a second one.
5. `initialize_app` (`:3826`) builds the terminal, config, tracing, key translator, terminal modes, and the startup `Authority` (local or SSH).
6. `local_control::start()` (`:3876`) binds this process's control socket so nested `fresh` launches can forward opens back here.
7. The **restart loop** (`:3882`) constructs an `Editor`, runs the event loop, and reconstructs on `request_restart` (Open-Folder context switch or a plugin's `setAuthority`). The `Authority` is single-owner and non-`Clone`, so it is moved into the editor and re-extracted on restart.

The TUI loop is `run_event_loop_common<F>` (`:4362`), reached via one of three `run_event_loop` wrappers selected by `cfg`: Linux GPM mouse polling (`:4174`), Windows VT-input reader thread (`:4194`), and the default crossterm poller (`:4294`). All three differ only in their `poll_event` closure.

Loop structure (`:4384`+), per iteration:

1. `local_control::pump(editor)` (`:4388`) — drain nested forward requests.
2. `editor_tick` (`:4395`) — shared per-tick housekeeping: drains async messages (§5), timers, auto-save, file-change polling.
3. Quit handling (`:4403`): auto-save buffers, end recovery session, save every window's workspace, write orchestrator state, then `break`.
4. Suspend handling (`:4441`) — `handle_suspend_request` tears down terminal modes, raises `SIGTSTP`, restores on `SIGCONT`.
5. Force a render if animations are active, an LSP `$/progress` spinner is live, or terminal titles need a poll (`:4453-4464`) — these are wall-clock-driven and need periodic frames even with no input.
6. **Render gate** (`:4466`): `needs_render && last_render.elapsed() >= FRAME_DURATION && !should_suppress_render`. `FRAME_DURATION = 16ms` (`:4375`, the 60fps cap). The draw is bracketed in `BeginSynchronizedUpdate`/`EndSynchronizedUpdate` (`:4475-4477`) so the terminal shows a coherent frame.
7. **Input poll** (`:4484-4504`): timeout is `FRAME_DURATION - elapsed` when a render is pending, else 50ms, further capped by `next_periodic_redraw_deadline` so animations/spinners stay on schedule. The loop **sleeps in the input poll**, never on async work.
8. Coalesce mouse-moves (`:4520`), dismiss the idle wave-screensaver on first input (`:4526`), route the event through `editor.handle_input_event` (`:4569`), which returns whether a redraw is wanted.

Key property: Fresh is **immediate-mode**. Every frame re-derives the whole screen from editor state (`app/render.rs`); ratatui resets its back-buffer and crossterm diffs back vs front, so only changed cells are written to the terminal even though the drawing is full. The editor never decides *when* to redraw — the loop does, and the 16ms gate means a burst of async messages can't exceed 60fps. (`e9b82585a` reverted a "drain buffered input before render" optimization, keeping the simple model.)

---

## 4. Threading model

Communication is **uniformly one-directional**: every background OS thread / tokio task is a producer; the **main thread is the sole consumer** of editor state, draining a channel once per frame.

- **Main thread** — the render/event loop (`main.rs:4362`). Sole owner of `EditorState`; renders via ratatui, polls crossterm input, runs `editor_tick` → `process_async_messages` every iteration.
- **Tokio runtime: 2 worker threads** — created once at `app/editor_init.rs:880-886` (`Builder::new_multi_thread().worker_threads(2).thread_name("editor-async")`), held as `Arc<Runtime>` on the `Editor` (`mod.rs:537`) and mirrored into windows. Hosts all LSP I/O tasks, file-explorer scans, plugin spawn/HTTP, remote connect/reconnect, and quick-open scans. If the runtime fails to build, async features degrade gracefully — every use is `if let Some(runtime)`.
- **Plugin thread** — one dedicated OS thread running the QuickJS/TS runtime (`services/plugins/manager.rs:57`, `PluginThreadHandle::spawn`). Returns results to the main loop via the async bridge as `AsyncMessage::Plugin(...)`; health-checked each frame (`async_dispatch.rs:92`).
- **Terminal threads (per PTY)** — each spawned terminal gets a plain `std::thread` reader thread (PTY → alacritty emulator → posts `TerminalOutput`) and a wait thread (blocks on `child.wait()`, posts `TerminalExited`) (`services/terminal/manager.rs:397,401`). Not tokio.
- **File-watch thread** — `notify`'s own backend thread; a `'static` callback translates events to `AsyncMessage::PathChanged` (`services/file_watcher.rs`).
- **stdin-stream thread** — when launched with piped stdin, a `std::thread` spools input to a temp file. This one is **polled, not bridged** (`app/stdin_stream.rs`, polled via `editor.poll_stdin_streaming()`).
- **Remote runtimes** — the authority subsystem builds additional multi-thread tokio runtimes for SSH/k8s carriers, separate from the editor-async runtime.

---

## 5. Async message flow

The bridge is `AsyncBridge` (`services/async_bridge.rs:551-609`). Key decision: it uses **`std::sync::mpsc`, unbounded — not tokio mpsc** (`:19,566`). Rationale documented inline (`:559-564`): the main loop drains every 16ms and LSP/async traffic is low-rate, so an unbounded std channel avoids needing the main loop to touch any tokio receive primitive. The receiver is wrapped `Arc<Mutex<…>>` so the bridge is `Clone` and senders are cheaply handed to background tasks. Tokio tasks therefore hold a *std* `Sender<AsyncMessage>` and `.send()` directly: the handoff is **tokio-task → std mpsc → sync main loop**.

There are **two bridge scopes**: one editor-global `async_bridge` (`Option<AsyncBridge>`, `editor_init.rs:894`) for plugin-runtime callbacks, the file-open dialog, clipboard, grammar build, and remote attach; and one **per-window** `Window.bridge` (`app/window/mod.rs:221`) for that window's LSP, terminal output, and file-explorer expansion — so closing a window drops its channel and its tasks error out automatically.

`AsyncMessage` (`async_bridge.rs:88-470`) is the single bridge type. Categories: **LSP** (the dominant ~30 variants — diagnostics push/pull, lifecycle/status, every feature response, server-driven edits/progress/messages), **PTY/terminal** (`TerminalOutput`, `TerminalExited`, each tagged with the owning `WindowId`), **file I/O / dialogs** (`FileChanged` auto-revert, file-open directory/shortcuts, quick-open scan results), **file watch** (`PathChanged`), **file explorer**, **plugin** (process output, delay-complete, responses, startup-async), **remote sessions** (attach/reconnect/failed), and **misc** (grammar build, clipboard-paste result).

Per-frame dispatch is `process_async_messages` (`app/async_dispatch.rs:89-521`): check plugin-thread health (`:92`), drain the global bridge then every window's bridge into one `Vec` (`:110-116`), compute `needs_render`, then a single big `match` (`:137-451`) routes each variant to a `handle_*` method. Most handlers live in `app/async_messages.rs`. **All state mutation happens on the main thread inside this dispatcher** — handlers call `self.active_window_mut()` / `self.windows.get_mut(...)`; background tasks never touch `EditorState`. Stale responses are dropped via per-request-id pending maps and buffer-version checks. Post-match housekeeping (`:454-517`) refreshes the plugin snapshot, processes plugin commands and pending actions, applies LSP restarts with exponential backoff, and polls file/tree changes.

**PLANNED in this layer:** `GitStatusChanged` handler is a `// TODO` (`async_dispatch.rs:306-309`); `TerminalExited.exit_code` is wired to `None` end-to-end currently (full wait-status capture is a follow-up); `handle_custom_notification` is `#[allow(dead_code)]` awaiting an `LspCustomNotification` variant.

---

## 6. Client / server + daemon architecture

Fresh can run as a single in-process editor, but the production "session-persistent" mode splits into a **daemon server that hosts the `Editor`** and a **thin client that relays bytes**. `server/mod.rs:1-12` states the design: "The server runs as a daemon and holds all editor state. Clients connect via IPC to send input and receive rendered output." `client/mod.rs:1-11`: "All complexity (input parsing, rendering, editor logic) lives server-side."

### Why it exists — session persistence

The daemon hosts the `Editor` (buffers, LSP servers, plugin runtime, workspace, undo) and **survives client disconnect**, like tmux for an editor:

- **Detach / reattach** — the client can detach (`relay_unix.rs`, EOF on stdin → `Detach`) leaving the server warm; reattach with `fresh -a`.
- **Suspend** (`Ctrl-Z`) — only the *client* gets `SIGTSTP` and drops to its shell; the daemon keeps running (`editor_server.rs:418-442`, commit `4113ca32a`). Raising `SIGTSTP` on the daemon would be wrong — it has no foreground shell.
- **Expensive state stays warm** across client churn — LSP servers, the plugin runtime, remote SSH backends.
- **Remote authority** — the daemon can boot already attached to an SSH host (`--ssh-url`), keeping the client thin regardless of where the files live.

The terminology was deliberately realigned from "session" to **daemon / workspace / backend** (commit `f03cf2ad6`).

### Spawn, sockets, IPC

The daemon is **lazily spawned by the client** (`main.rs:2946`): if no server is alive, the client calls `spawn_server_detached` (`server/daemon/unix.rs:66-90`), which re-execs the current binary with `--server` and stdio to `/dev/null`, then waits on the PID file before connecting. Daemonization (`daemon/unix.rs:17-56`) is the classic **double-fork** — `fork → setsid → fork → redirect stdio → chdir("/") → umask(0)`; Windows uses `CreateProcess` with `DETACHED_PROCESS`.

IPC uses the `interprocess` crate's local sockets — **Unix domain sockets** on Linux/macOS, **named pipes** on Windows (`server/ipc/mod.rs`). Per daemon there are three files in the platform socket dir (XDG runtime on Unix): `<key>.data.sock` (raw byte stream), `<key>.ctrl.sock` (JSON control), and `<key>.pid`. The key is the session name or the encoded working directory, so the default is **one daemon per working directory**. Liveness is checked via `kill(pid,0)` with a connect-probe fallback; stale sockets are cleaned up.

### Protocol (`server/protocol.rs`)

`PROTOCOL_VERSION = 2`. Two channels: **data** = raw unframed bytes; **control** = newline-delimited serde-tagged JSON. Client→server `ClientControl`: `Hello`, `Resize`, `Ping`, `Detach`, `Quit`, `OpenFiles{wait}`, `OpenWindow{path}`. Server→client `ServerControl`: `Hello`/`VersionMismatch`, `Pong`, `Quit`, `Error`, `WaitComplete` (releases a `--wait` client), `SetClipboard` (clipboard travels back to the client), `SuspendClient`. A framing subtlety (`ipc/mod.rs:386-424`): `write_control` forces blocking mode for the duration of a write so a large message (e.g. a 4 MiB clipboard) can't be truncated without its trailing `\n`, which would wedge the client's blocking read.

### Server-side rendering — `capture_backend.rs`

The daemon has no terminal of its own. `CaptureBackend` implements ratatui's `Backend` trait but **emits ANSI escape sequences into an in-memory `Vec<u8>`** instead of writing to a TTY (`capture_backend.rs:13-26`). The editor renders normally via `Terminal<CaptureBackend>`; `draw` does its own cursor-move + SGR diff optimization (`:212-243`), `take_buffer` hands the bytes to the loop, and `terminal_setup_sequences`/`teardown_sequences` (`:335-394`) are shared with direct mode so the two paths can't diverge. `render_and_broadcast` (`editor_server.rs:1218-1281`) pushes each frame to per-client non-blocking writers that **drop frames** if a client is too slow rather than blocking the loop.

### Server loop & runner

`editor_server.rs` runs a 60fps loop (`:263-522`) that accepts connections (first client triggers `initialize_editor`), parses each client's raw data bytes through `InputParser` → crossterm events, handles control messages, feeds events to `editor.handle_key/handle_mouse/resize/paste_text`, and broadcasts frames. `server/runner.rs` is an **earlier, editor-less skeleton compiled only under `#[cfg(test)]`** — the production server is `editor_server.rs`.

### `local_control.rs` — nested-`fresh` forwarding (direct mode)

Not the daemon. When Fresh runs as a plain in-process editor it still binds **one** control socket so a `fresh` launched from inside its own embedded terminal forwards file/dir opens back to the running process instead of launching a second editor (`local_control.rs:1-28`). It reuses the same `ServerListener`/`ClientControl` primitives as the daemon but **does not render** (`pump` runs once per frame on the editor thread, draining requests into `queue_file_open`/`create_window_at`).

### Web UI bridge (`webui/mod.rs`)

A **dependency-free, single-threaded HTTP server** (raw `TcpListener`) hosting a **real `Editor`, no mocks** (commit `40116ac0d`). Single-threaded because the editor is not `Send`. Routes run the real `handle_key`/`handle_mouse`/`handle_action`/`resize`; `GET /state` runs the real render pipeline and returns JSON. The split: **buffer interiors** are sliced as real syntax-highlighted cells from a `TestBackend` render; **chrome** (menu, status bar, tabs, scrollbars, file explorer, popups, palette, settings, trust dialog) is serialized as semantic regions for the frontend to draw natively (`suppress_chrome_cells = true`). This bridge does **not** use the daemon/IPC path. **PLANNED:** ship only the visible-window cell diff per tick instead of re-serializing the whole scene (commit `79c505f8a`).

### GUI mode (`gui/mod.rs`)

**Implemented, not a stub.** A thin adapter: all windowing/GPU/input lives in the separate `fresh-gui` crate (winit + wgpu via `ratatui-wgpu`); `gui/mod.rs` implements the `fresh_gui::GuiApplication` trait for `Editor` (`on_key`/`on_mouse`/`render`/`tick`/menu hooks). `run_gui` builds the real editor in-process (plugins, init.ts, workspace restore) — like direct mode, it does **not** go through the daemon. Software cursor only (ratatui-wgpu has no hardware cursor); macOS gets a native menu bar via `muda`. Gated behind the non-default `gui` feature.

---

## 7. The `Editor` god object and the buffer/view state split

`Editor` (`app/mod.rs:342`) is the central object. Its behavior is decomposed across ~90 sibling modules under `app/` (one `impl Editor` block each: `input.rs`, `render.rs`, `lifecycle.rs`, `search_ops.rs`, `lsp_actions.rs`, …) rather than living in one file — the modules were extracted in the editor-modules refactor.

The notable structural decision is that **most per-document and per-UI state has moved off `Editor` onto `Window`** (`app/window/mod.rs:121`). Each `Window` owns its `buffers` (`WindowBuffers`), `buffer_metadata`, `event_logs` (undo), `lsp` (`LspManager` rooted at the window's project root), `file_explorer`, `status_message`, `prompt`, `bridge`, and crucially its own `authority` (owned outright, never shared, so one workspace's trust/env can't leak into another — issue #2280). `Editor` keeps the genuinely global state: config (`Arc<Config>` with copy-on-write via `Arc::make_mut`), theme (`Arc<RwLock<Theme>>`), keybindings, clipboard, grammar registry, the tokio runtime handle, the global async bridge, and the `windows` map + `active_window` id. There is **no `active_buffer` field** — the active buffer is derived from the split manager to keep a single source of truth.

Two distinct state structs hold "the document" vs "a view of it":

- **`EditorState`** (`state.rs:137`) — **buffer state, shared per buffer**: the `Buffer` text, the `HighlightEngine`, content-anchored decorations (`overlays`, `marker_list`, `virtual_texts`, `conceals`, `soft_breaks`, `popups`, `margins`), `text_properties`, `reference_highlighter`, and per-buffer `buffer_settings`. Note `state.rs:134-136`: **viewport is NOT stored here** because it is view-specific.
- **`SplitViewState`** (`view/split.rs:310`) — **view state, per split** (the Emacs model: each split has its own point, window-start, and tabs). Holds `active_buffer`, a `keyed_states: HashMap<BufferId, BufferViewState>` of per-buffer cursors/viewport/scroll, `open_buffers` (tab order), the computed `layout`, `sync_group` for synchronized scrolling, and composite-view state. Opening the same buffer in two splits gives two independent scroll/cursor positions over one shared `EditorState`.

The single layout funnel is `Editor::relayout` (`app/lifecycle.rs:352`): every geometry-changing event mutates its own source-of-truth then calls `relayout`, which derives authoritative geometry once and pushes it **down** (one-directional) to split viewports, terminal PTYs (all windows), the dock, and the plugin `resize` hook. It is intentionally cheap to call redundantly (PTY resizes are idempotent; the plugin hook is signature-deduped to break the orchestrator's resize→`dock_width`→relayout feedback loop).

---

## 8. Action vs Event (high level)

Fresh keeps two layers distinct (depth in **input-keybindings-actions.md** and **text-model.md**):

- **`Action`** = *intent* — "what the user wants" (`Save`, `MoveLeft`, `InsertChar('a')`, `CommandPalette`, `LspHover`, `PluginAction(...)`). Produced by keybindings, menus, the command palette, and UI handlers; executed via `Editor::handle_action`.
- **`Event`** = *state change* — the event-sourced "what changed" layer for undoable mutations (`Insert`, `Delete`, `MoveCursor`, `Batch`, plus some view events), stored in a per-buffer `EventLog` for undo/redo and modified-since-saved tracking (`model/event.rs`).

Many editing/navigation actions convert into one or more `Event`s; multi-cursor edits become `Event::Batch` so undo is atomic. All undoable buffer mutations funnel through `Editor::apply_event_to_active_buffer`, which centralizes cross-cutting concerns: apply to `EditorState`, sync cursors into the split view state, invalidate layouts for splits viewing that buffer, adjust other splits' cursors, update search highlights, fire plugin edit hooks, and send LSP change notifications.

---

## Subsystem map

Each sibling doc lives alongside this one in `docs/internal/`.

- **[text-model.md](text-model.md)** — the `Buffer` text representation (the persistent path-copying piece tree), positions and line indexing, the `Event`/`BulkEdit` model and `EventLog`, cursors/selections, and markers as the content-anchored primitive. The conceptual base under buffers-splits-undo and rendering.
- **[rendering-and-layout.md](rendering-and-layout.md)** — the immediate-mode `Editor::render` pipeline (`app/render.rs:59`), the token→`ViewLine` projection, line-wrap and visual-row caches for cheap scrolling of huge files, folding/conceal/virtual-text decorations, split-pane layout, the `Scene` semantic projections shared with the web frontend, and mouse hit-testing.
- **[syntax-highlighting.md](syntax-highlighting.md)** — the syntect-first (TextMate) checkpoint/incremental highlighter with a tree-sitter fallback for the gaps, grammar selection, how spans reach the renderer, and the bracket- and reference-highlight overlays layered on top.
- **[input-keybindings-actions.md](input-keybindings-actions.md)** — the depth doc for §8: key translation, the modal dispatch priority (settings → menu → prompt → popup → normal), unified keybinding resolution and chords, the command→action→event pipeline, multi-cursor, and fuzzy/quick-open input.
- **[buffers-splits-undo.md](buffers-splits-undo.md)** — buffer ownership and identity (per-window storage), the split/window tree layout, the per-buffer vs per-split state separation (§7), marker displacement/preservation across undo/redo, and hot-exit + crash recovery persistence.
- **[lsp.md](lsp.md)** — the embedded LSP client: multi-server routing, the request queuing/concurrency model, the async result flow (§5), diagnostics-as-markers, completion merging, and the feature set with its concessions vs. a full client. Layers across main thread / `LspManager` / tokio runtime.
- **[plugins.md](plugins.md)** — sandboxed TypeScript plugins in QuickJS on the dedicated plugin thread (§4), the `PluginCommand`/hook protocol bridging that thread to `Editor`, the provider pattern, the declarative widget runtime, and package/marketplace loading.
- **[remote-authority-trust.md](remote-authority-trust.md)** — the `Authority` backend slot (local / SSH / docker-exec / kubectl-exec) that answers "where does this primitive run?", how remote backends are built/kept-alive/reconnected, and Workspace Trust + the live env provider deciding *whether* code may run there. The as-built companion to AUTHORITY_DESIGN.md.
- **[orchestrator-sessions.md](orchestrator-sessions.md)** — managing many concurrent editor/agent sessions via the Orchestrator "dock", per-session state persistence/restore across restarts (`save_orchestrator_state`, §3 step 3), and the Live/Dormant window lifecycle. Built on the multi-window model of §7.
- **[terminal.md](terminal.md)** — the integrated terminal: PTY spawning, VT100/ANSI parsing delegated to the `alacritty_terminal` library (not a custom emulator), the live/scrollback per-buffer model, mouse/link/title/clipboard handling, and the Windows-specific `fresh-winterm` VT *input* crate.
- **[config-themes-settings.md](config-themes-settings.md)** — layered config resolution, JSON-Schema generation (`generate_schema`, §2) and the schema-driven Settings UI, JSONC comment-preserving read/write, themes, the keybinding editor, and the `init.ts` programmable-config surface.
- **[search-and-diff.md](search-and-diff.md)** — in-buffer search/replace, project-wide search and live grep, the diff/review (hunk) viewer and git-log viewing, and the keyboard-macro system — split between a thin Rust host (scans, piece-tree diff, codegen) and TypeScript picker plugins.
- **[editor-ux-features.md](editor-ux-features.md)** — the catch-all for UX features without a dedicated doc (markdown compose mode, bookmarks, calibration wizard, dabbrev, screensaver, etc.), each with shipped-vs-planned status.
- **[testing.md](testing.md)** — the testing layers, the headless scenario framework, the determinism strategy (injected time source, §3 step 8), the ANSI capture backend (shared with the daemon's `capture_backend.rs`), and the meta-testing/migration efforts.

---

## Discrepancies & notes

- `docs/architecture.md` uses the pre-workspace `src/...` paths; the real layout is `crates/fresh-editor/src/...`. Its runtime-model and Action/Event sections remain conceptually accurate.
- `server/runner.rs` is a test-only skeleton, not the production server (which is `editor_server.rs`).
- Several "future" doc-comments in `async_bridge.rs` are already implemented (e.g. `FileChanged` auto-revert); the genuinely unbuilt ones are `GitStatusChanged` and terminal exit-code capture.
