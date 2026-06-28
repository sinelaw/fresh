# Plugin System Architecture

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh runs sandboxed TypeScript plugins in a QuickJS
runtime on a dedicated thread, the `PluginCommand`/hook protocol that connects
that thread to the editor, the provider pattern, the declarative widget
runtime, and package/marketplace loading — with a clear split between what is
implemented and what is still planned.

---

## 1. Overview & crate layout

A Fresh plugin is a `.ts` (or `.js`) file. Plugins are pure TypeScript against
an ambient `editor` API; they observe editor events via **hooks** and mutate
editor state by issuing **commands**. They never touch editor memory directly —
all interaction is message-passing across a thread boundary.

The system spans several crates:

- The **plugin runtime** crate houses the QuickJS runtime: the thread runner,
  the JS backend, subprocess spawning, and `.d.ts` export.
- A **proc-macro** crate provides `#[plugin_api_impl]`, which generates the
  plugin-facing TypeScript declarations from the Rust API.
- A **JS parser** crate provides oxc-based TS→JS transpilation, `.d.ts` emit,
  and dependency extraction.
- The **core** crate defines the shared protocol types — `PluginCommand`,
  `PluginResponse`, `EditorStateSnapshot`, widget specs, the `JsEditorApi`
  surface, and the hook/command/action/overlay/text-property vocabulary.
- The **editor** crate holds the editor-side `PluginManager`, the service
  bridge, embedded plugins, the dev workspace, command dispatch and the widget
  reconciler, plus package scanning and the Linux-console mouse FFI (see §11).

The whole system is behind a `plugins` Cargo feature. `PluginManager` wraps the
thread handle in an `Option` and no-ops the entire API when the feature is off,
so the editor compiles and runs without a JS runtime.

---

## 2. Runtime model: QuickJS on a dedicated thread

Plugins run in a QuickJS JavaScript runtime on **one dedicated OS thread**,
separate from the main editor thread.

### 2.1 Why QuickJS

The runtime was migrated from Deno to QuickJS. The rationale:

- Far fewer dependencies and faster compilation (no V8 snapshot generation).
- A much lighter runtime (QuickJS is small, on the order of hundreds of
  kilobytes, versus a multi-megabyte V8).
- Per-plugin context isolation rather than Deno's shared global VM.

TypeScript is transpiled ahead of execution by **oxc**, not by QuickJS —
QuickJS only ever sees plain JS. There is **no WASM plugin backend**: the
backend module re-exports only the QuickJS backend, which is the single
implementation. A second backend would require introducing a trait and a
parallel module — PLANNED at most, no code exists. (Note: separate WASM
documentation in the tree concerns compiling *the editor itself* to WASM for
the browser, an unrelated effort; and "WASM OOM" notes elsewhere refer to LSP
*servers* that happen to ship as WASM, not to plugins.)

### 2.2 Thread spawning & runtime

Spawning the plugin thread creates a real OS thread. Each thread owns a
single-threaded Tokio runtime driven on a `LocalSet` so async tasks run
concurrently without crossing threads. The QuickJS `Runtime` and per-plugin
`Context`s are `!Send`, so they stay pinned to this thread.

A thread-health check joins the thread and re-propagates a panic to the main
thread, so a crashing plugin surfaces rather than hanging.

### 2.3 Per-plugin context isolation

The QuickJS backend holds one shared `Runtime` plus a map of per-plugin
contexts, created lazily on first load. Each context gets:

- a per-plugin name global;
- an `editor` object instantiated from the `JsEditorApi` Rust class;
- bootstrap scripts: `getEditor()`/`registerHandler()` globals, the
  `editor.on`/`editor.off` shim, a `console` shim forwarding to the tracing
  layer, and the Promise/async-callback infrastructure.

Plugin source is wrapped in an IIFE before eval to prevent temporal-dead-zone
errors and global-scope pollution. Isolation is **scope isolation, not a
security sandbox** — see §10.

---

## 3. The PluginCommand / hook protocol

Communication is fully asynchronous and bidirectional:

```
Main editor thread                    Plugin thread (QuickJS)
──────────────────                    ───────────────────────
run_hook(name, args)  ──RunHook──►    handler(args) executes
                                       │ editor.insertText(...)  etc.
                       ◄─PluginCommand─┤  (queued back)
process_commands()                     │
drains next frame    ◄─HookCompleted───┘  sentinel
```

### 3.1 Editor → plugin: requests and hooks

A `PluginRequest` is the message type into the plugin thread, sent over an
unbounded channel. Variants include loading a plugin, loading plugins from a
directory, unload, reload, executing an action, running a hook, querying hook
handlers, listing plugins, resolving/rejecting callbacks, tracking async
resources, and shutdown. Most carry a oneshot reply channel; running a hook is
**fire-and-forget**.

Hooks are the editor's event notifications. `HookArgs` is an untagged enum with
roughly sixty variants — file lifecycle (before file open, after file save,
buffer closed), text mutations (before/after insert, before/after delete,
carrying byte ranges, line numbers and added/removed line counts), cursor
moved, rendering (render start, render line, lines changed, view-transform
request), prompt and mouse events, LSP events, lifecycle (editor initialized,
plugins loaded, ready), process/terminal output, file-watch path changes, and
widget events. Hook serialization emits a variant as its fields only (no
discriminant). Plugins subscribe via `editor.on(eventName, handlerName)`.

The hook event loop runs a biased select that favors request handling over a
short periodic poll of the JS event loop. After a hook's handlers run, the
thread emits a `HookCompleted` sentinel so the editor knows when the command
batch for that hook is complete.

The in-process hook registry runs registered callbacks; a hook returns false if
any callback returns false, which lets **before-hooks veto** an operation (e.g.
cancel an insert). The editor-side mapping from low-level buffer events to
before/after hooks runs the before-hook, applies the event, then the
after-hook.

### 3.2 Plugin → editor: PluginCommand

When a plugin calls `editor.insertText(...)`, `editor.addOverlay(...)`, etc.,
the `JsEditorApi` method packages a `PluginCommand` and sends it back over a
channel. `PluginCommand` is a large enum — on the order of two hundred
variants. The main thread drains them once per frame via the manager's
command-processing step inside the editor's async message pump, and dispatches
each through a central command handler, which delegates the heavier handlers to
a dedicated command module.

Command families (representative, not exhaustive):

| Family | Examples |
|---|---|
| Text edit | `InsertText`, `DeleteRange`, `InsertAtCursor` |
| Overlays | `AddOverlay`, `ClearNamespace`, `ClearOverlaysInRange[ForNamespace]` |
| Virtual text/lines | `AddVirtualText[Styled]`, `AddVirtualLine`, `ClearVirtualTextNamespace` |
| Conceals / soft breaks / folds | `AddConceal`, `AddSoftBreak`, `AddFold`, `SetFoldingRanges` |
| View pipeline | `SetViewMode`, `SetLineWrap`, `SubmitViewTransform`, `SetLayoutHints`, `RefreshLines` |
| Plugin state | `SetViewState`, `SetGlobalState`, `SetWindowState` |
| Gutter / explorer | `SetLineIndicator(s)`, `SetFileExplorerDecorations`, `SetFileExplorerSlots` |
| Prompts / input | `StartPrompt[Async]`, `SetPromptSuggestions`, `AwaitNextKey`, `SetPromptToolbar` |
| Commands / modes | `RegisterCommand`, `UnregisterCommand`, `DefineMode`, `RegisterStatusBarElement` |
| Windows / sessions | `CreateWindow[WithTerminal]`, `SetActiveWindow`, `CloseWindow`, `WatchPath` |
| Async work | `SpawnProcess`, `SpawnBackgroundProcess`, `Delay`, `HttpFetch`, `SendLspRequest` |
| Virtual / composite buffers | `CreateVirtualBuffer*`, `CreateCompositeBuffer`, `CreateBufferGroup` |
| Authority / remote / env | `SetAuthority`, `AttachRemoteAgent`, `SetEnv`, `SetRemoteIndicatorState` |
| Terminals | `CreateTerminal`, `SendTerminalInput`, `SignalWindow` |
| Search | `GrepProject`, `BeginSearch` (streaming), `ReplaceInBuffer` |
| Widgets | `MountWidgetPanel`, `UpdateWidgetPanel`, `WidgetMutate`, `MountFloatingWidget` (see §7) |

**Timing consequence:** because hooks are async and commands drain on the next
frame, plugin-driven effects (overlays, virtual text, view transforms) become
visible on the **next render frame**, not the current one. This one-frame async
lag is deliberate — it keeps the render loop deterministic and prevents a
plugin from blocking the UI. The design accepts the lag and mitigates it in
production: proactive synchronous `refreshLines()` for inter-line cursor moves,
atomic clear+rebuild batching within one hook execution, preferring
marker-based soft breaks over a view-transform request, and namespace
separation of static versus dynamic overlays.

### 3.3 Async callbacks

Async API methods (process spawn, getting buffer text, delay, prompts, LSP
requests, buffer creation) return a JS `Promise` backed by a callback id. The
bootstrap installs pending-callback bookkeeping and resolve/reject helpers. The
editor completes the operation and sends a `PluginResponse` via the manager's
delivery path, which either fulfills a Rust-side oneshot held in a
pending-responses map or resolves the JS callback. Resources created this way
are recorded per plugin so they can be cleaned up on unload.

### 3.4 The synchronous service bridge

Separate from the async command channel, an editor service bridge implements
the core `PluginServiceBridge` trait — synchronous services the runtime needs
*during* JS execution: i18n translation and string registration, theme
schema/data, command registry register/unregister, the config/data/plugins
directories, and JS-execution-state signaling for the SIGINT handler. It is
passed into the thread at spawn and stored on the manager.

### 3.5 Shared read-only state

Plugins read editor state from an `EditorStateSnapshot`, shared behind a
read-write lock. Before each hook dispatch, the snapshot is refreshed:
per-window fields (active buffer, cursors, viewport) plus editor-wide fields
(clipboard, working dir, terminal size, authority label, workspace trust, env,
window list, config). Config serialization is copy-on-write-cached via `Arc`
pointer comparison. Because the snapshot is a write-through cache, a plugin can
read back state it just wrote within the same hook execution without waiting a
frame.

---

## 4. The plugin API surface

The API is the `JsEditorApi` Rust class, exposed to JS through `rquickjs` class
and methods attributes with camelCase renaming. Methods are annotated with a
`plugin_api` attribute to drive TypeScript generation and async classification.

Key shared protocol types in the core crate:

- **Commands**: a `Command` (name, description, action name, plugin name,
  custom contexts, terminal-bypass flag), a command source, and a `Suggestion`
  (prompt autocomplete row, with optional styled description spans and
  keybinding).
- **Actions**: the `Action` enum is the editor's built-in action vocabulary;
  `Action::PluginAction(name)` is the bridge by which a registered plugin
  command name becomes an executable action. `KeyContext` defines the context
  (Normal/Prompt/Popup/FileExplorer/Menu/Terminal/Settings/`Mode(name)`) a
  binding is active in; plugins create custom contexts via `Mode(name)`.
- **Overlays**: overlay handles and namespaces are opaque string handles
  (atomic-counter generated) for grouping and removing decorations.
- **Text properties**: a text property attaches an arbitrary key/value map to a
  byte range — the mechanism behind virtual buffers where each line carries
  structured metadata (diagnostic, search hit, etc.). An offset-unit selector
  lets plugins emit byte or char offsets.

### 4.1 Entry points

Plugins call `getEditor()` for an editor-API instance scoped to the calling
plugin, and `registerHandler(name, fn)` to register named handlers. Handler
names are then referenceable from `editor.registerCommand()`, `editor.on()`,
and `defineMode` keybindings — this replaced an older `globalThis` pattern.

### 4.2 TypeScript type generation

The plugin-facing types are generated, not hand-maintained, from two sources
merged at build time:

1. The `#[plugin_api_impl]` proc macro parses the `JsEditorApi` impl block and
   emits TypeScript definitions and JS method shims, writing the declaration
   file when content changes. Per method, the `plugin_api` attribute controls
   skipping, JS naming, promise-returning async, cancellable-thenable async,
   and TypeScript type/return overrides. Async is explicit — no heuristic
   detection.
2. The runtime's type-export step collects all `ts-rs`-derived types from the
   core API module, validates and reformats them with oxc, and writes the type
   half of the declaration file. A few types whose authoritative definitions
   live in the editor crate (which the runtime crate cannot depend on) are
   hand-written here. It also emits a hook-event-map interface so
   `editor.on(hook, handler)` is typed.

---

## 5. Provider pattern

**Problem.** Plugins that "own the UI" (the *Controller* pattern, built on
virtual buffers) must reimplement navigation, selection, and keybindings,
producing inconsistent UX and keybinding/i18n bugs (custom-mode keys not
firing; translation not interpolating).

**Decision.** Standardize on the **Provider pattern**: the plugin provides
*data*, the editor renders the *UI* and owns navigation. Two tiers:

- **QuickPick** — transient searches (Live Grep, Git Grep): the plugin streams
  results, the editor renders the picker with standard navigation.
- **ResultsPanel** — persistent panels (Find References, Diagnostics) with
  bidirectional cursor sync.

Related decisions: prefer **atomic Rust actions** (e.g. delete-word-right) over
selection-then-delete for operator+motion combos to avoid async timing races,
with a batch action API (with count) for patterns like `3dw`; and the
**view-transform pipeline** (view-transform request → plugin rewrites tokens →
submit view transform) for content-transforming plugins like markdown compose —
which flickers one frame late under rapid scroll, which is why newer plugins
prefer marker-based layout hints and virtual lines instead.

The widget runtime (§7) is the structural successor to the provider idea: the
host owns layout, focus, and hit-testing; the plugin describes data.

---

## 6. Plugin loading & lifecycle

### 6.1 Manager

The `PluginManager` owns the optional plugin thread handle and exposes
load/unload/reload, run-hook, deliver-response, process-commands, list-plugins,
async action execution, and accessors for the shared editor-state snapshot and
search-handle registry. A render-blocking variant that drains commands until a
hook completes exists but is currently unused (effectively dead code) —
PLANNED/unused.

### 6.2 Two-phase parallel loading

Bulk load is split to overlap I/O while keeping JS execution serial and
ordered:

- **Phase 1 — prepare (parallel, scoped threads, no external dependency):** for
  each discovered `.ts`/`.js`: read file, load its i18n JSON, extract
  `fresh:plugin/<name>` dependencies, transpile to JS, emit `.d.ts`, and hash
  for cache. Produces a list of prepared plugins.
- **Phase 2 — execute (serial, on the plugin thread):** topologically sort by
  declared dependencies (Kahn's algorithm; cycles reported with the full path
  and refused), register i18n strings, and eval each plugin's JS in order.
  Independent plugins keep alphabetical order for determinism.

A test-timing environment variable surfaces the phase split. **First-writer-wins**
collision detection makes command/grammar/language/LSP registration throw on a
duplicate, so non-deterministic prepare order cannot silently change behavior;
same-plugin re-registration (hot reload) is allowed.

After discovery and the init plugin, the editor fires the plugins-loaded hook
then the ready hook.

### 6.3 Unload cleanup

Unload removes the JS context, event handlers, actions and callback contexts,
then sends *compensating* commands to clear everything the plugin created —
overlays, conceals, soft breaks, virtual lines, explorer decorations, line
indicators, contexts — and tears down resources (background processes,
scroll-sync groups, buffers, terminals, path watches), driven by a per-plugin
tracked-state record.

### 6.4 Embedded plugins & dev workspace

With an embed-plugins feature, the plugins tree is compiled in and extracted to
a content-addressed cache with an atomic extract-to-temp-then-rename publish and
an extracted marker.

A plugin dev workspace supports "Load Plugin from Buffer": it writes the
buffer's content plus a copy of the plugin declaration file and a `tsconfig.json`
into a temp dir so the TypeScript LSP can give autocomplete/hover on the plugin
under development; the dir is removed on drop. The fuller design
(untitled-buffer LSP integration) is DESIGN, partly realized by this workspace.

---

## 7. Declarative widget runtime

The widget system is a virtual-DOM-like tree (`WidgetSpec`) authored in
TypeScript and reconciled host-side in Rust. The plugin describes data; the host
owns layout, rendering, focus, hit-testing, scroll, selection, and cursor
placement.

### 7.1 Widget catalogue (IMPLEMENTED)

Spec kinds in the core API: `Row`, `Col` (flex containers), `Spacer`,
`Divider`, `HintBar`, `Toggle`, `Button` (intent Normal/Primary/Danger), `Text`
(unified single-line vs multi-line by row count), `List` (virtual-scrolled,
host-owned scroll/selection), `Tree` (disclosure + optional per-row
checkboxes), and `Raw` (pre-rendered text-property escape hatch). PLANNED per
the design: `Table`, tabs/group, and a layer/prompt compositor for
modals/tooltips.

### 7.2 Mount and reconcile

Mounting a widget panel (plugin, panel id, buffer id, spec) resets instance
state, renders the spec to produce entries, hits, instance states, focus key and
tabbable set, stores it in a widget registry keyed by owning plugin and panel
id, and writes the rendered text-property entries as the buffer's virtual
content. There is also a floating/dock variant.

**Spec/instance separation (the central rule):** spec values are *initial only*;
after first render, host-owned widget instance state (List scroll/selection,
Text editor/scroll/completions, Tree scroll/selection/expanded keys) is
authoritative. Stable `key` fields preserve instance state across spec re-emits.
Updating a panel re-renders preserving state; a widget mutate is a fast path for
targeted updates (set value, set checked, set items, set expanded keys, set
completions, append tree nodes, set focus key, …).

### 7.3 Events back to the plugin

Key/mouse input is routed through the widget runtime (command handling, smart
key dispatch, hit delivery). The host fires the widget-event hook carrying
panel id, widget key, event type and payload — event type is one of
activate/toggle/change/submit/hover/dismiss/focus (plus select/expand for
List/Tree). Events are delivered **only to the panel's owning plugin**. Plugins
never see raw row/col coordinates — hit-testing is host-owned.

Status: the runtime, all eleven widgets, keyed reconciliation, mutators,
completion popups, and floating/dock panels are IMPLEMENTED and have live plugin
call-sites (New Session dialog, search/replace, git log). PLANNED per the
design: the unified compositor for modals/tooltips/context menus (the gating
item for remaining plugin migrations), full role-based theming (only button
intent ships), per-widget unwind-catching fault isolation, IME preedit, and
spec-as-state session restore.

---

## 8. Package loading

The package scanner scans installed packages *synchronously at startup*,
replacing an older async JS path that serialized one grammar rebuild per
package. It returns a scan result carrying language configs, LSP configs,
additional grammars, and bundle plugin/theme directories, applied during editor
init with insert-if-absent so user config wins over package defaults; bundle
plugin/theme dirs feed the plugin loader and theme loader.

A package is a directory with a `package.json` manifest: a name, optional
version/description, a type (`plugin`/`theme`/`theme-pack`/`language`/`bundle`),
and a `fresh` block carrying grammar/language/LSP config (language packs) or
arrays of languages, plugins, and themes (bundles). All fields default for
forward compatibility. The JSON schema is generated from the Rust types via
schemars — IMPLEMENTED.

---

## 9. Marketplace / GPM (Git Package Manager)

> Naming note: in the marketplace context "GPM" means the **Git Package
> Manager** plugin model. This is distinct from the editor's mouse module,
> which is the unrelated Linux-console **General Purpose Mouse** FFI (§11). The
> two share an abbreviation only; the code does not group them.

The marketplace is **git-as-distribution**: a plugin is a git repo cloned from
any URL; the registry is itself just a git repo of `plugins.json`/`themes.json`;
package management lives in a `pkg.ts` plugin, not editor core. That plugin is
IMPLEMENTED and provides registry sync/load/cache, installed-package listing,
and git-command wrappers, installing into the user's plugin packages directory
via a shallow `git clone`, with commands like Install Plugin, Update All, and
Sync Registry.

Status: git distribution, registry sync/cache, basic install/update/remove, and
the code-review confirmation dialog are IMPLEMENTED. PLANNED/DESIGNED-only:
full semver matching (only latest/main/commit/local resolve today), monorepo
subdir fragments, lockfile generation/restore, registry signing, blocklist,
theme preview, and a reusable plugin UI component library.

---

## 10. Sandboxing & security trade-offs

The plugin "sandbox" is **scope isolation, not a security boundary**. The
runtime has **no memory limit, no execution timeout or gas, and no interrupt
handler** on the QuickJS runtime — a plugin can infinite-loop. (The
JS-execution-state signal in the service bridge lets the SIGINT handler surface
a stuck plugin, but does not preempt it.) Isolation comes from per-plugin
contexts plus IIFE wrapping (§2.3); there is no global removal or syscall
filtering. Filesystem and network access are not ambient — plugins use
`editor.readFile()` and `editor.spawnProcess()` / process commands, which are
auditable and routed through the editor — but a granted `spawnProcess` is
arbitrary code execution by design.

The security posture is therefore mostly **install-time trust**: a confirmation
dialog showing source/author/license before clone (IMPLEMENTED), with registry
signing and a malicious-package blocklist still PLANNED. Workspace-trust gating
exists at the command layer (setting/clearing env requires a Trusted workspace;
a trust-changed hook signals transitions), and authority/env are owned
per-window (setting authority triggers a controlled editor restart). Net: Fresh
trusts plugin code once installed and leans on git provenance plus an explicit
consent step rather than runtime confinement.

---

## 11. Subprocesses and GPM (mouse) — not a package manager

The runtime's process module spawns plugin subprocesses: a Tokio command with
piped stdio, concurrent stdout/stderr reads, no-window creation on Windows, and
streaming output back as async process-output messages. This backs the spawn and
spawn-background process commands.

The editor's mouse module is **General Purpose Mouse** — runtime FFI to the GPM
library giving mouse events on Linux virtual consoles where xterm/SGR mouse
protocols are unavailable. The client connects only when stdin is a real virtual
console (not a pseudoterminal or SSH), and a converter maps GPM events to
crossterm. It degrades gracefully when the GPM library is absent or an xterm is
detected. It is unrelated to plugins or packages and is documented here only to
dispel the name collision.

---

## 12. Implemented vs planned (summary)

IMPLEMENTED:
- QuickJS runtime on a dedicated thread; per-plugin context isolation; oxc TS
  transpile and `.d.ts` emit; async callback bridge; hook system with veto.
- Roughly two hundred `PluginCommand` variants dispatched through the central
  command handler.
- Generated plugin declaration file from proc macro plus ts-rs.
- Two-phase parallel loading, dependency topo-sort, first-writer-wins collisions.
- Provider pattern (QuickPick / ResultsPanel); view-transform pipeline.
- Declarative widget runtime (eleven widgets, keyed reconciliation, mutators,
  floating/dock panels, completion popups).
- Synchronous package scanner; git-based install/update via the `pkg.ts` plugin.
- Embedded plugins; plugin-dev LSP workspace.

PLANNED / PARTIAL:
- Any runtime sandbox limits (memory/timeout/gas) and a WASM backend — none.
- Widget compositor for modals/tooltips; full role theming; per-widget fault
  isolation; IME preedit; widget session restore.
- Marketplace semver, lockfiles, monorepo fragments, registry signing,
  blocklist, theme preview, plugin UI component library.
- Render-blocking hooks (drain-until-hook-completed) — unused.
- Transpile cache and an inter-plugin runtime export API.
