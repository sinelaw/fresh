# System Overview & Runtime Model

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: the keystone map of Fresh — an 8-crate Rust workspace that ships a terminal text editor. This document covers the crate split and why it exists, cargo feature gating, the main entrypoint and ~60fps event loop, the threading model, the client/server+daemon architecture, async message flow, the `Editor` god object, and the Action-vs-Event distinction. It indexes the sibling subsystem docs at the end. Status: **IMPLEMENTED** unless flagged **PLANNED**.

The older user-facing `docs/architecture.md` is a partial starting point and uses a stale module layout.

---

## 1. Workspace layout & the crate DAG

The workspace declares 8 members. `fresh-gui` is a member but **not** in `default-members` — it only builds when explicitly requested.

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

`fresh-core` is deliberately **dependency-light**: only serde/serde_json/schemars/anyhow/lsp-types/ts-rs/unicode-width plus an *optional* `rquickjs` gated behind feature `plugins`. No tree-sitter, ratatui, crossterm, tokio, or platform crates. It holds pure-data ID types (cursor, split/leaf/container, buffer, terminal, window) and shared models — the action, plugin-api, command, hooks, config, menu, overlay, services, text-property, file-explorer, file-uri, and display-width surfaces. The reasons for the boundary:

- **Shared by three crates** — `fresh-plugin-runtime` and `fresh-gui` need the command/action/hook/menu/ID models without dragging in the editor's heavy dependency tree (e.g. `fresh-gui` consumes the core menu types as the single menu source of truth).
- **`ts-rs` export boundary** — these types are exported so the plugin API and `.d.ts` generation reference one canonical definition.
- **WASM / schema friendliness** — pure-Rust + serde links cleanly into the wasm target and into the minimal schema-generation builds.

The crate was introduced when the project was refactored into a Cargo workspace with a modular crate architecture. The settled pattern: plugin-visible **IDs and data models live in `fresh-core`**, while the **behavioral logic lives in `fresh-editor`**.

`fresh-editor` by contrast holds the actual editor: the `fresh` binary and all runtime subsystems (app, input, services, state, client, server, webui, view, model, primitives) and the entire heavy stack (crossterm, ratatui, tokio, syntect, alacritty_terminal, lsp-types, portable-pty, notify).

---

## 2. Feature gating: `runtime` / `wasm` / `dev-bins`

`fresh-editor` defines the workspace's feature surface. The default set is `plugins`, `runtime`, `embed-plugins`, `tree-sitter`, and `http`.

- **`runtime`** — the big one: all heavy native deps (crossterm, ratatui, tokio, syntect, alacritty_terminal, portable-pty, lsp-types, notify, libc/nix, `fresh-languages`). The `fresh` binary requires this feature.
- **`plugins`** — pulls in `fresh-plugin-runtime`/`fresh-parser-js`/`fresh-plugin-api-macros` + oxc to syntax-check `init.ts`.
- **`tree-sitter`** — enables `fresh-languages`' bundled grammars and the tree-sitter AST features; off ⇒ indentation falls back to regex pattern rules.
- **`embed-plugins`** — bakes plugins into the binary as a fallback.
- **`http`** — adds the HTTP client; drops the whole TLS stack when off.
- **`gui`** (not default) — implies `runtime` and pulls in `fresh-gui`; native wgpu window.
- **`dev-bins`** (not default) — gates the dev binaries `generate_schema`, `event_debug`, `measure_startup`. `generate_schema` emits JSON Schema from the config, theme-file, and package-manifest structs.
- **`wasm`** (not default) — browser build of the shared editor core *without* native deps; swaps syntect's onig (C library) for a pure-Rust regex engine, enables crossterm event types only (no backend), and omits tokio/pty/lsp/alacritty entirely.

The same discipline is enforced **inside** `fresh-editor` via `#[cfg]`: the model, primitives, widgets, config, types, and plugin-schemas modules are always available (pure Rust); the view module builds under runtime, wasm, or dev-bins; and the app/input/services/state/client/server/webui modules build under `runtime` only. The rationale: only pure-Rust modules compile to `wasm32`, and the config/types/theme modules must build in a minimal configuration so `generate_schema` runs with just serde + schemars. Keeping the core dependency-light is the within-crate version of the `fresh-core` split.

---

## 3. Entrypoint & the ~60fps event loop

Entry runs through `main`, which wraps a fallible `real_main`. Startup sequence:

1. `main` wraps `real_main`, printing a clean message (no backtrace) for an SSH error (an expected user failure, not a bug).
2. `real_main` initializes i18n *before* clap parses (so `--help` is localized), parses the CLI, and sets `FRESH_INTERACTIVE=1` for genuine interactive launches.
3. Subcommand dispatch handles non-editor invocations: `--server` (daemon), `--attach`/`-a`, the `daemon`/`config`/`grammar`/`init` commands, `--stdin`, and the GUI path. These never enter the TUI loop.
4. Nested-launch forwarding — if launched from inside Fresh's own embedded terminal (detected by an environment marker), file/dir opens are forwarded to the parent editor instead of starting a second one.
5. App initialization builds the terminal, config, tracing, key translator, terminal modes, and the startup `Authority` (local or SSH).
6. The local-control listener binds this process's control socket so nested `fresh` launches can forward opens back here.
7. The **restart loop** constructs an `Editor`, runs the event loop, and reconstructs on a restart request (Open-Folder context switch or a plugin's `setAuthority`). The `Authority` is single-owner and non-`Clone`, so it is moved into the editor and re-extracted on restart.

The TUI loop is a shared `run_event_loop_common`, reached via one of three wrappers selected by `cfg`: Linux GPM mouse polling, a Windows VT-input reader thread, and the default crossterm poller. All three differ only in their event-poll closure.

Per-iteration loop structure:

1. Pump local control — drain nested forward requests.
2. Editor tick — shared per-tick housekeeping: drains async messages (§5), timers, auto-save, file-change polling.
3. Quit handling: auto-save buffers, end recovery session, save every window's workspace, write orchestrator state, then break.
4. Suspend handling — tears down terminal modes, raises `SIGTSTP`, restores on `SIGCONT`.
5. Force a render if animations are active, an LSP `$/progress` spinner is live, or terminal titles need a poll — these are wall-clock-driven and need periodic frames even with no input.
6. **Render gate**: render only when one is needed, the frame duration has elapsed since the last render, and rendering is not suppressed. The frame duration is the 60fps cap (~16ms). The draw is bracketed in the terminal's synchronized-update sequences so the terminal shows a coherent frame.
7. **Input poll**: timeout is the remaining time in the frame when a render is pending, else a longer idle interval, further capped by the next periodic-redraw deadline so animations/spinners stay on schedule. The loop **sleeps in the input poll**, never on async work.
8. Coalesce mouse-moves, dismiss the idle wave-screensaver on first input, and route the event through the editor's input handler, which returns whether a redraw is wanted.

Key property: Fresh is **immediate-mode**. Every frame re-derives the whole screen from editor state; ratatui resets its back-buffer and crossterm diffs back vs front, so only changed cells are written to the terminal even though the drawing is full. The editor never decides *when* to redraw — the loop does, and the frame gate means a burst of async messages cannot exceed 60fps. A "drain buffered input before render" optimization was tried and reverted, keeping the simple model.

---

## 4. Threading model

Communication is **uniformly one-directional**: every background OS thread / tokio task is a producer; the **main thread is the sole consumer** of editor state, draining a channel once per frame.

- **Main thread** — the render/event loop. Sole owner of `EditorState`; renders via ratatui, polls crossterm input, runs the editor tick → async-message processing every iteration.
- **Tokio runtime: 2 worker threads** — a multi-thread runtime named `editor-async`, created once during editor init, held as a shared handle on the `Editor` and mirrored into windows. Hosts all LSP I/O tasks, file-explorer scans, plugin spawn/HTTP, remote connect/reconnect, and quick-open scans. If the runtime fails to build, async features degrade gracefully — every use is optional.
- **Plugin thread** — one dedicated OS thread running the QuickJS/TS runtime. Returns results to the main loop via the async bridge as plugin messages; health-checked each frame.
- **Terminal threads (per PTY)** — each spawned terminal gets a plain `std::thread` reader thread (PTY → alacritty emulator → posts terminal output) and a wait thread (blocks on the child process, posts a terminal-exited message). Not tokio.
- **File-watch thread** — `notify`'s own backend thread; a `'static` callback translates events to path-changed messages.
- **stdin-stream thread** — when launched with piped stdin, a `std::thread` spools input to a temp file. This one is **polled, not bridged**.
- **Remote runtimes** — the authority subsystem builds additional multi-thread tokio runtimes for SSH/k8s carriers, separate from the editor-async runtime.

---

## 5. Async message flow

The bridge is `AsyncBridge`. Key decision: it uses **`std::sync::mpsc`, unbounded — not tokio mpsc**. Rationale: the main loop drains every frame and LSP/async traffic is low-rate, so an unbounded std channel avoids needing the main loop to touch any tokio receive primitive. The receiver is wrapped in a shared mutex so the bridge is `Clone` and senders are cheaply handed to background tasks. Tokio tasks therefore hold a *std* sender and send directly: the handoff is **tokio-task → std mpsc → sync main loop**.

There are **two bridge scopes**: one editor-global bridge for plugin-runtime callbacks, the file-open dialog, clipboard, grammar build, and remote attach; and one **per-window** bridge for that window's LSP, terminal output, and file-explorer expansion — so closing a window drops its channel and its tasks error out automatically.

`AsyncMessage` is the single bridge type. Categories: **LSP** (the dominant set, ~30 variants — diagnostics push/pull, lifecycle/status, every feature response, server-driven edits/progress/messages), **PTY/terminal** (terminal output and exit, each tagged with the owning window), **file I/O / dialogs** (file-changed auto-revert, file-open directory/shortcuts, quick-open scan results), **file watch** (path changed), **file explorer**, **plugin** (process output, delay-complete, responses, startup-async), **remote sessions** (attach/reconnect/failed), and **misc** (grammar build, clipboard-paste result).

Per-frame dispatch checks plugin-thread health, drains the global bridge then every window's bridge into one vector, computes whether a render is needed, then routes each variant to a handler method through a single large match. Most handlers live in a dedicated async-messages module. **All state mutation happens on the main thread inside this dispatcher** — handlers operate on the active or named window; background tasks never touch `EditorState`. Stale responses are dropped via per-request-id pending maps and buffer-version checks. Post-match housekeeping refreshes the plugin snapshot, processes plugin commands and pending actions, applies LSP restarts with exponential backoff, and polls file/tree changes.

**PLANNED in this layer:** a git-status-changed handler is not yet implemented; terminal exit-code capture is stubbed to none end-to-end (full wait-status capture is a follow-up); a custom-LSP-notification handler exists but is unused, awaiting its message variant.

---

## 6. Client / server + daemon architecture

Fresh can run as a single in-process editor, but the production "session-persistent" mode splits into a **daemon server that hosts the `Editor`** and a **thin client that relays bytes**. The design: the server runs as a daemon and holds all editor state; clients connect via IPC to send input and receive rendered output. All complexity — input parsing, rendering, editor logic — lives server-side.

### Why it exists — session persistence

The daemon hosts the `Editor` (buffers, LSP servers, plugin runtime, workspace, undo) and **survives client disconnect**, like tmux for an editor:

- **Detach / reattach** — the client can detach (EOF on stdin maps to a detach control message) leaving the server warm; reattach with `fresh -a`.
- **Suspend** (`Ctrl-Z`) — only the *client* gets `SIGTSTP` and drops to its shell; the daemon keeps running. Raising `SIGTSTP` on the daemon would be wrong — it has no foreground shell.
- **Expensive state stays warm** across client churn — LSP servers, the plugin runtime, remote SSH backends.
- **Remote authority** — the daemon can boot already attached to an SSH host, keeping the client thin regardless of where the files live.

The terminology was deliberately realigned from "session" to **daemon / workspace / backend**.

### Spawn, sockets, IPC

The daemon is **lazily spawned by the client**: if no server is alive, the client spawns a detached server, which re-execs the current binary with `--server` and stdio to `/dev/null`, then waits on the PID file before connecting. Daemonization is the classic **double-fork** — fork, `setsid`, fork again, redirect stdio, change to root directory, reset umask; Windows uses a detached process.

IPC uses the `interprocess` crate's local sockets — **Unix domain sockets** on Linux/macOS, **named pipes** on Windows. Per daemon there are three files in the platform socket dir (XDG runtime on Unix): a raw data-stream socket, a JSON control socket, and a PID file. The key is the session name or the encoded working directory, so the default is **one daemon per working directory**. Liveness is checked via a signal-zero probe with a connect-probe fallback; stale sockets are cleaned up.

### Protocol

The protocol is versioned. Two channels: **data** = raw unframed bytes; **control** = newline-delimited serde-tagged JSON. Client→server control messages cover hello, resize, ping, detach, quit, open-files (with an optional wait), and open-window. Server→client control messages cover hello / version-mismatch, pong, quit, error, wait-complete (releases a `--wait` client), set-clipboard (clipboard travels back to the client), and suspend-client. A framing subtlety: the control writer forces blocking mode for the duration of a write so a large message (e.g. a multi-megabyte clipboard) cannot be truncated without its trailing newline, which would wedge the client's blocking read.

### Server-side rendering — the capture backend

The daemon has no terminal of its own. The capture backend implements ratatui's `Backend` trait but **emits ANSI escape sequences into an in-memory byte buffer** instead of writing to a TTY. The editor renders normally through this backend; the draw does its own cursor-move + SGR diff optimization, a take-buffer call hands the bytes to the loop, and the terminal setup/teardown sequences are shared with direct mode so the two paths cannot diverge. Frame broadcast pushes each frame to per-client non-blocking writers that **drop frames** if a client is too slow rather than blocking the loop.

### Server loop & runner

The production server runs a 60fps loop that accepts connections (the first client triggers editor initialization), parses each client's raw data bytes through the input parser into crossterm events, handles control messages, feeds events to the editor's key/mouse/resize/paste handlers, and broadcasts frames. An **earlier, editor-less server skeleton compiled only under test** also exists — the production server is the editor server, not the skeleton.

### Local control — nested-`fresh` forwarding (direct mode)

Not the daemon. When Fresh runs as a plain in-process editor it still binds **one** control socket so a `fresh` launched from inside its own embedded terminal forwards file/dir opens back to the running process instead of launching a second editor. It reuses the same listener/control primitives as the daemon but **does not render** — its pump runs once per frame on the editor thread, draining requests into file-open and window-creation queues.

### Web UI bridge

A **dependency-free, single-threaded HTTP server** (raw `TcpListener`) hosting a **real `Editor`, no mocks**. Single-threaded because the editor is not `Send`. Routes run the real key/mouse/action/resize handlers; a state route runs the real render pipeline and returns JSON. The split: **buffer interiors** are sliced as real syntax-highlighted cells from a test-backend render; **chrome** (menu, status bar, tabs, scrollbars, file explorer, popups, palette, settings, trust dialog) is serialized as semantic regions for the frontend to draw natively, with the chrome cells suppressed. This bridge does **not** use the daemon/IPC path. **PLANNED:** ship only the visible-window cell diff per tick instead of re-serializing the whole scene.

### GUI mode

**Implemented, not a stub.** A thin adapter: all windowing/GPU/input lives in the separate `fresh-gui` crate (winit + wgpu via `ratatui-wgpu`); the GUI module implements `fresh-gui`'s application trait for `Editor` (key/mouse/render/tick/menu hooks). The GUI entrypoint builds the real editor in-process (plugins, init.ts, workspace restore) — like direct mode, it does **not** go through the daemon. Software cursor only (ratatui-wgpu has no hardware cursor); macOS gets a native menu bar via `muda`. Gated behind the non-default `gui` feature.

---

## 7. The `Editor` god object and the buffer/view state split

`Editor` is the central object. Its behavior is decomposed across roughly ninety sibling modules (one `impl Editor` block each — input, render, lifecycle, search ops, LSP actions, and so on) rather than living in one file; the modules were extracted in the editor-modules refactor.

The notable structural decision is that **most per-document and per-UI state has moved off `Editor` onto `Window`**. Each `Window` owns its buffers, buffer metadata, event logs (undo), an `LspManager` rooted at the window's project root, file explorer, status message, prompt, async bridge, and crucially its own `authority` (owned outright, never shared, so one workspace's trust/env cannot leak into another). `Editor` keeps the genuinely global state: config (a shared `Config` with copy-on-write), theme (a shared, lock-guarded `Theme`), keybindings, clipboard, grammar registry, the tokio runtime handle, the global async bridge, and the windows map plus the active-window id. There is **no active-buffer field** — the active buffer is derived from the split manager to keep a single source of truth.

Two distinct state structs hold "the document" vs "a view of it":

- **`EditorState`** — **buffer state, shared per buffer**: the `Buffer` text, the highlight engine, content-anchored decorations (overlays, markers, virtual texts, conceals, soft breaks, popups, margins), text properties, the reference highlighter, and per-buffer buffer settings. **Viewport is NOT stored here** because it is view-specific.
- **`SplitViewState`** — **view state, per split** (the Emacs model: each split has its own point, window-start, and tabs). Holds the active buffer, a map of per-buffer cursors/viewport/scroll, the open-buffer tab order, the computed layout, a sync group for synchronized scrolling, and composite-view state. Opening the same buffer in two splits gives two independent scroll/cursor positions over one shared `EditorState`.

The single layout funnel is the editor's relayout pass: every geometry-changing event mutates its own source-of-truth then calls relayout, which derives authoritative geometry once and pushes it **down** (one-directional) to split viewports, terminal PTYs (all windows), the dock, and the plugin `resize` hook. It is intentionally cheap to call redundantly (PTY resizes are idempotent; the plugin hook is signature-deduped to break the orchestrator's resize → dock-width → relayout feedback loop).

---

## 8. Action vs Event (high level)

Fresh keeps two layers distinct (depth in **input-keybindings-actions.md** and **text-model.md**):

- **`Action`** = *intent* — "what the user wants" (save, move-left, insert-char, command-palette, LSP-hover, plugin-action). Produced by keybindings, menus, the command palette, and UI handlers; executed via the editor's action handler.
- **`Event`** = *state change* — the event-sourced "what changed" layer for undoable mutations (insert, delete, move-cursor, batch, plus some view events), stored in a per-buffer `EventLog` for undo/redo and modified-since-saved tracking.

Many editing/navigation actions convert into one or more events; multi-cursor edits become a batch event so undo is atomic. All undoable buffer mutations funnel through a single apply-event path on the active buffer, which centralizes cross-cutting concerns: apply to `EditorState`, sync cursors into the split view state, invalidate layouts for splits viewing that buffer, adjust other splits' cursors, update search highlights, fire plugin edit hooks, and send LSP change notifications.

---

## Subsystem map

Each sibling doc lives alongside this one in `docs/internal/`.

- **[text-model.md](text-model.md)** — the `Buffer` text representation (the persistent path-copying piece tree), positions and line indexing, the `Event`/`BulkEdit` model and `EventLog`, cursors/selections, and markers as the content-anchored primitive. The conceptual base under buffers-splits-undo and rendering.
- **[rendering-and-layout.md](rendering-and-layout.md)** — the immediate-mode render pipeline, the token→`ViewLine` projection, line-wrap and visual-row caches for cheap scrolling of huge files, folding/conceal/virtual-text decorations, split-pane layout, the `Scene` semantic projections shared with the web frontend, and mouse hit-testing.
- **[syntax-highlighting.md](syntax-highlighting.md)** — the syntect-first (TextMate) checkpoint/incremental highlighter with a tree-sitter fallback for the gaps, grammar selection, how spans reach the renderer, and the bracket- and reference-highlight overlays layered on top.
- **[input-keybindings-actions.md](input-keybindings-actions.md)** — the depth doc for §8: key translation, the modal dispatch priority (settings → menu → prompt → popup → normal), unified keybinding resolution and chords, the command→action→event pipeline, multi-cursor, and fuzzy/quick-open input.
- **[buffers-splits-undo.md](buffers-splits-undo.md)** — buffer ownership and identity (per-window storage), the split/window tree layout, the per-buffer vs per-split state separation (§7), marker displacement/preservation across undo/redo, and hot-exit + crash recovery persistence.
- **[lsp.md](lsp.md)** — the embedded LSP client: multi-server routing, the request queuing/concurrency model, the async result flow (§5), diagnostics-as-markers, completion merging, and the feature set with its concessions vs. a full client. Layers across main thread / `LspManager` / tokio runtime.
- **[plugins.md](plugins.md)** — sandboxed TypeScript plugins in QuickJS on the dedicated plugin thread (§4), the `PluginCommand`/hook protocol bridging that thread to `Editor`, the provider pattern, the declarative widget runtime, and package/marketplace loading.
- **[remote-authority-trust.md](remote-authority-trust.md)** — the `Authority` backend slot (local / SSH / docker-exec / kubectl-exec) that answers "where does this primitive run?", how remote backends are built/kept-alive/reconnected, and Workspace Trust + the live env provider deciding *whether* code may run there. The as-built companion to AUTHORITY_DESIGN.md.
- **[orchestrator-sessions.md](orchestrator-sessions.md)** — managing many concurrent editor/agent sessions via the Orchestrator "dock", per-session state persistence/restore across restarts (§3 step 3), and the Live/Dormant window lifecycle. Built on the multi-window model of §7.
- **[terminal.md](terminal.md)** — the integrated terminal: PTY spawning, VT100/ANSI parsing delegated to the `alacritty_terminal` library (not a custom emulator), the live/scrollback per-buffer model, mouse/link/title/clipboard handling, and the Windows-specific `fresh-winterm` VT *input* crate.
- **[config-themes-settings.md](config-themes-settings.md)** — layered config resolution, JSON-Schema generation (the `generate_schema` dev binary, §2) and the schema-driven Settings UI, JSONC comment-preserving read/write, themes, the keybinding editor, and the `init.ts` programmable-config surface.
- **[search-and-diff.md](search-and-diff.md)** — in-buffer search/replace, project-wide search and live grep, the diff/review (hunk) viewer and git-log viewing, and the keyboard-macro system — split between a thin Rust host (scans, piece-tree diff, codegen) and TypeScript picker plugins.
- **[editor-ux-features.md](editor-ux-features.md)** — the catch-all for UX features without a dedicated doc (markdown compose mode, bookmarks, calibration wizard, dabbrev, screensaver, etc.), each with shipped-vs-planned status.
- **[testing.md](testing.md)** — the testing layers, the headless scenario framework, the determinism strategy (injected time source, §3 step 8), the ANSI capture backend (shared with the daemon's capture backend), and the meta-testing/migration efforts.

---

## Discrepancies & notes

- `docs/architecture.md` uses a pre-workspace module layout. Its runtime-model and Action/Event sections remain conceptually accurate.
- The editor-less server skeleton is a test-only artifact, not the production server (the editor server).
- Several "future" doc-comments in the async bridge are already implemented (e.g. file-changed auto-revert); the genuinely unbuilt ones are git-status-changed handling and terminal exit-code capture.
