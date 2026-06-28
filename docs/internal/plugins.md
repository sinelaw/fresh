# Plugin System Architecture

Purpose: explain how Fresh runs sandboxed TypeScript plugins in a QuickJS
runtime on a dedicated thread, the `PluginCommand`/hook protocol that connects
that thread to the `Editor`, the provider pattern, the declarative widget
runtime, and package/marketplace loading — with code-level references and a
clear split between what is implemented and what is still planned.

> Verified against the code at the paths cited. Where a source design doc
> diverges from the code, the discrepancy is flagged inline. Line numbers are
> accurate as of writing but drift; treat them as anchors, not contracts.

---

## 1. Overview & crate layout

A Fresh plugin is a `.ts` (or `.js`) file. Plugins are pure TypeScript against
an ambient `editor` API; they observe editor events via **hooks** and mutate
editor state by issuing **commands**. They never touch editor memory directly —
all interaction is message-passing across a thread boundary.

| Crate / path | Role |
|---|---|
| `crates/fresh-plugin-runtime/` | The QuickJS runtime: thread runner, JS backend, subprocess spawning, `.d.ts` export |
| `crates/fresh-plugin-api-macros/` | `#[plugin_api_impl]` proc macro: generates `fresh.d.ts` from the Rust API |
| `crates/fresh-parser-js/` | oxc-based TS→JS transpile, `.d.ts` emit, dependency extraction |
| `crates/fresh-core/src/api.rs` | `PluginCommand`, `PluginResponse`, `EditorStateSnapshot`, widget specs, the `JsEditorApi` surface |
| `crates/fresh-core/src/{hooks,command,action,overlay,text_property}.rs` | The shared protocol types |
| `crates/fresh-editor/src/services/plugins/` | Editor-side `PluginManager`, service bridge, embedded plugins, dev workspace |
| `crates/fresh-editor/src/app/{plugin_dispatch,plugin_commands,widget_runtime}.rs` | Editor-side command dispatch and widget reconciler |
| `crates/fresh-editor/src/services/{packages.rs,gpm/}` | Package scanning; Linux-console mouse FFI (see §11) |

The whole system is behind the `plugins` Cargo feature. `PluginManager` wraps
the thread handle in an `Option` and no-ops the entire API when the feature is
off (`services/plugins/manager.rs:24`), so the editor compiles and runs without
a JS runtime.

---

## 2. Runtime model: QuickJS on a dedicated thread

Plugins run in a QuickJS JavaScript runtime (via the `rquickjs` crate, ~0.11+)
on **one dedicated OS thread**, separate from the main editor thread. This is
design-decisions.md §13 ("Plugin Architecture & Runtime").

### 2.1 Why QuickJS

The runtime was migrated from Deno to QuickJS in commit `562925f15` ("Replace
Deno with QuickJS for Plugin System"). The stated rationale (`docs/quickjs.md`):

- Far fewer dependencies (~315 → ~183 crates) and faster compilation (no V8
  snapshot generation).
- Lighter runtime (~700 KB QuickJS vs multi-MB V8).
- Per-plugin context isolation rather than Deno's shared global VM.

TypeScript is transpiled ahead of execution by **oxc** (`oxc_transformer`),
not by QuickJS — QuickJS only ever sees plain JS. There is **no WASM plugin
backend**: `backend/mod.rs` re-exports only `QuickJsBackend`, and
`quickjs_backend.rs` is the single implementation (~11k lines). A second backend
would require introducing a trait and a parallel module — PLANNED at most, no
code exists. (Note: `docs/wasm.md` is about compiling *the editor itself* to
WASM for the browser, an unrelated effort; and the "WASM OOM" notes in
`lsp-plugin-testing.md` refer to LSP *servers* that happen to ship as WASM, not
to plugins.)

### 2.2 Thread spawning & runtime

`PluginThreadHandle::spawn()` (`fresh-plugin-runtime/src/thread.rs:236`) creates
a real OS thread (`thread.rs:275`). Each thread owns a single-threaded Tokio
runtime (`Builder::new_current_thread().enable_all()`, `thread.rs:278`) driven on
a `LocalSet` (`thread.rs:318`) so async tasks run concurrently without crossing
threads. The QuickJS `Runtime` and per-plugin `Context`s are `!Send`, so they
stay pinned to this thread.

`check_thread_health()` (`thread.rs:372`) joins the thread and re-propagates a
panic to the main thread, so a crashing plugin surfaces rather than hanging.

### 2.3 Per-plugin context isolation

`QuickJsBackend` holds one shared `Runtime` plus a `HashMap<String, Context>` of
per-plugin contexts (`quickjs_backend.rs:6730`), created lazily on first load
(`:7369`). Each context gets:

- a `__pluginName__` global (`:7269`);
- an `editor` object instantiated from the `JsEditorApi` Rust class (`:7274`);
- bootstrap scripts (`:7284`): `getEditor()`/`registerHandler()` globals, the
  `editor.on/off` shim, a `console` shim forwarding to `tracing`, and the
  Promise/async-callback infrastructure.

Plugin source is wrapped in an IIFE before eval (`:7385`) to prevent TDZ errors
and global-scope pollution. Isolation is **scope isolation, not a security
sandbox** — see §10.

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

`PluginRequest` (`thread.rs:44`) is the message type into the plugin thread,
sent over a `tokio::mpsc::UnboundedSender` (`thread.rs:268`). Variants include
`LoadPlugin`, `LoadPluginsFromDir(WithConfig)`, `UnloadPlugin`, `ReloadPlugin`,
`ExecuteAction`, `RunHook`, `HasHookHandlers`, `ListPlugins`,
`ResolveCallback`/`RejectCallback`, `TrackAsyncResource`, and `Shutdown`. Most
carry a `oneshot::Sender` for their reply; `RunHook` is **fire-and-forget**.

Hooks are the editor's event notifications. `HookArgs`
(`fresh-core/src/hooks.rs:16`) is a `#[serde(untagged)]` enum with ~60 variants —
file lifecycle (`BeforeFileOpen`, `AfterFileSave`, `BufferClosed`), text
mutations (`Before/AfterInsert`, `Before/AfterDelete`, carrying byte ranges,
line numbers and added/removed line counts), `CursorMoved`, rendering
(`RenderStart`, `RenderLine`, `LinesChanged`, `ViewTransformRequest`), prompt
and mouse events, LSP events, lifecycle (`EditorInitialized`, `PluginsLoaded`,
`Ready`), process/terminal output, file-watch `PathChanged`, and `WidgetEvent`.
`hook_args_to_json()` (`hooks.rs:586`) serializes a variant as its fields only
(no discriminant). Plugins subscribe via `editor.on(eventName, handlerName)`.

The hook event loop runs `biased` select (`thread.rs:1048`) favoring request
handling over a 1 ms JS-event-loop poll (`thread.rs:1030`). After a hook's
handlers run, the thread emits a `HookCompleted` sentinel (`thread.rs:1224`) so
the editor knows when the command batch for that hook is complete.

`HookRegistry` (`hooks.rs:526`) is the in-process registry; `run_hooks()` returns
`false` if any callback returns `false`, which lets **before-hooks veto** an
operation (e.g. cancel an insert). The editor-side mapping from low-level
buffer `Event`s to before/after hooks lives in
`services/plugins/event_hooks.rs`, where `apply_event_with_hooks()` runs the
before-hook, applies the event, then the after-hook.

### 3.2 Plugin → editor: PluginCommand

When a plugin calls `editor.insertText(...)`, `editor.addOverlay(...)`, etc.,
the `JsEditorApi` method packages a `PluginCommand` and sends it back over an
`mpsc::Sender<PluginCommand>` (`thread.rs:240/296`). `PluginCommand`
(`fresh-core/src/api.rs:2168`) is a large enum — roughly **190+ variants**
(`api.rs:2170`–`4017`). The main thread drains them once per frame via
`PluginManager::process_commands()` (`manager.rs:287`) inside the editor's async
message pump, and dispatches each in
`handle_plugin_command()` (`app/plugin_dispatch.rs:273`), which delegates the
heavier handlers to `app/plugin_commands.rs`.

Command families (representative, not exhaustive):

| Family | Examples | Dispatch anchor |
|---|---|---|
| Text edit | `InsertText`, `DeleteRange`, `InsertAtCursor` | `plugin_dispatch.rs:275` |
| Overlays | `AddOverlay`, `ClearNamespace`, `ClearOverlaysInRange[ForNamespace]` | `:293`, `plugin_commands.rs:53` |
| Virtual text/lines | `AddVirtualText[Styled]`, `AddVirtualLine`, `ClearVirtualTextNamespace` | `:330` |
| Conceals / soft breaks / folds | `AddConceal`, `AddSoftBreak`, `AddFold`, `SetFoldingRanges` | `:419`–`:464` |
| View pipeline | `SetViewMode`, `SetLineWrap`, `SubmitViewTransform`, `SetLayoutHints`, `RefreshLines` | `:558` |
| Plugin state | `SetViewState`, `SetGlobalState`, `SetWindowState` | `:593` |
| Gutter / explorer | `SetLineIndicator(s)`, `SetFileExplorerDecorations`, `SetFileExplorerSlots` | `:623` |
| Prompts / input | `StartPrompt[Async]`, `SetPromptSuggestions`, `AwaitNextKey`, `SetPromptToolbar` | `:713` |
| Commands / modes | `RegisterCommand`, `UnregisterCommand`, `DefineMode`, `RegisterStatusBarElement` | `:838` |
| Windows / sessions | `CreateWindow[WithTerminal]`, `SetActiveWindow`, `CloseWindow`, `WatchPath` | `:777` |
| Async work | `SpawnProcess`, `SpawnBackgroundProcess`, `Delay`, `HttpFetch`, `SendLspRequest` | `:957`, `:942` |
| Virtual / composite buffers | `CreateVirtualBuffer*`, `CreateCompositeBuffer`, `CreateBufferGroup` | `:1052`, `:1287` |
| Authority / remote / env | `SetAuthority`, `AttachRemoteAgent`, `SetEnv`, `SetRemoteIndicatorState` | `:981` |
| Terminals | `CreateTerminal`, `SendTerminalInput`, `SignalWindow` | `:1380` |
| Search | `GrepProject`, `BeginSearch` (streaming), `ReplaceInBuffer` | `:1409` |
| Widgets | `MountWidgetPanel`, `UpdateWidgetPanel`, `WidgetMutate`, `MountFloatingWidget` | `:1463` (see §7) |

**Timing consequence** (design-decisions.md §13): because hooks are async and
commands drain next frame, plugin-driven effects (overlays, virtual text, view
transforms) become visible on the **next render frame**, not the current one.
This is deliberate — it keeps the render loop deterministic and prevents a
plugin from blocking the UI. The doc catalogs the production mitigations for the
resulting one-frame lag: proactive synchronous `refreshLines()` for inter-line
cursor moves, atomic clear+rebuild batching within one hook execution,
preferring marker-based soft breaks over `view_transform_request`, and
namespace separation of static vs dynamic overlays.

### 3.3 Async callbacks

Async API methods (process spawn, `getBufferText`, `delay`, prompts,
`sendLspRequest`, buffer creation) return a JS `Promise` backed by a
`JsCallbackId`. The bootstrap installs `_pendingCallbacks` and
`_resolveCallback`/`_rejectCallback` (`quickjs_backend.rs:6823`). The editor
completes the operation and sends a `PluginResponse` via
`PluginManager::deliver_response()` (`manager.rs:274`) →
`thread.rs:394`, which either fulfills a Rust-side `oneshot` (in
`PendingResponses`, an `Arc<Mutex<HashMap<u64, oneshot::Sender>>>`,
`thread.rs:650`) or resolves the JS callback. Resources created this way are
recorded in `async_resource_owners` so they can be cleaned up on unload.

### 3.4 The synchronous service bridge

Separate from the async command channel, `EditorServiceBridge`
(`services/plugins/bridge.rs:12`) implements `fresh_core::services::
PluginServiceBridge` — synchronous services the runtime needs *during* JS
execution: i18n translation (`translate`, `register_plugin_strings`), theme
schema/data, command registry register/unregister, config/data/plugins
directories, and JS-execution-state signaling for the SIGINT handler. It is
passed into the thread at spawn and stored on the manager.

### 3.5 Shared read-only state

Plugins read editor state from `EditorStateSnapshot`, shared as
`Arc<RwLock<…>>`. Before each hook dispatch,
`update_plugin_state_snapshot()` (`plugin_dispatch.rs:126`) refreshes it:
per-window fields (active buffer, cursors, viewport) plus editor-wide fields
(clipboard, working dir, terminal size, authority label, workspace trust, env,
window list, config). Config serialization is CoW-cached via `Arc` pointer
comparison (`:248`). Because the snapshot is a write-through cache, a plugin can
read back state it just wrote within the same hook execution without waiting a
frame (design-decisions.md §13, "Plugin State").

---

## 4. The plugin API surface

The API is the `JsEditorApi` Rust class (`quickjs_backend.rs:773`), exposed to JS
through `#[rquickjs::class]` + `#[rquickjs::methods(rename_all = "camelCase")]`.
Methods are annotated with `#[plugin_api(...)]` (from `fresh-plugin-api-macros`)
to drive TypeScript generation and async classification.

Key shared protocol types in `fresh-core`:

- **Commands** (`command.rs`): `Command` (name, description, action_name,
  plugin_name, custom_contexts, `terminal_bypass`), `CommandSource`,
  `Suggestion` (prompt autocomplete row, with optional styled `description_spans`
  and keybinding).
- **Actions** (`action.rs`): the `Action` enum is the editor's built-in action
  vocabulary; `Action::PluginAction(String)` is the bridge by which a registered
  plugin command name becomes an executable action. `KeyContext` defines the
  context (Normal/Prompt/Popup/FileExplorer/Menu/Terminal/Settings/`Mode(name)`)
  a binding is active in; plugins create custom contexts via `Mode(name)`.
- **Overlays** (`overlay.rs`): `OverlayHandle` and `OverlayNamespace` are opaque
  string handles (atomic-counter generated) for grouping/removing decorations.
- **Text properties** (`text_property.rs`): `TextProperty` attaches an arbitrary
  `HashMap<String, Value>` to a byte range — the mechanism behind virtual
  buffers where each line carries structured metadata (diagnostic, search hit,
  etc.). `OffsetUnit` lets plugins emit `Byte` or `Char` offsets.

### 4.1 Entry points

Plugins call `getEditor()` for an `EditorAPI` instance scoped to the calling
plugin, and `registerHandler(name, fn)` to register named handlers
(design-decisions.md §13). Handler names are then referenceable from
`editor.registerCommand()`, `editor.on()`, and `defineMode` keybindings — this
replaced an older `globalThis` pattern.

### 4.2 TypeScript type generation

`fresh.d.ts` (the plugin-facing types) is generated, not hand-maintained, from
two sources merged at build time:

1. `#[plugin_api_impl]` (`fresh-plugin-api-macros/src/lib.rs`) parses the
   `JsEditorApi` impl block and emits `…_TYPESCRIPT_DEFINITIONS` and
   `…_JS_METHODS`, writing `plugins/lib/fresh.d.ts` when content changes. Per
   method, `#[plugin_api(...)]` controls `skip`, `js_name`, `async_promise`
   (→ `Promise<T>`), `async_thenable` (→ cancellable `ProcessHandle<T>`),
   `ts_type`, and `ts_return`. Async is explicit — no heuristic detection.
2. `ts_export.rs` (`fresh-plugin-runtime`) collects all `#[derive(ts_rs::TS)]`
   types from `fresh-core/src/api.rs` (`collect_ts_types()`, `:366`), validates
   and reformats them with oxc, and writes the type half of `fresh.d.ts`. A few
   types whose authoritative definitions live in `fresh-editor` (which this crate
   cannot depend on) are hand-written here (`AuthorityPayload`,
   `RemoteAgentSpec`, `RemoteIndicatorStatePayload`, `:190`). It also emits the
   `HookEventMap` interface (`:509`) so `editor.on(hook, handler)` is typed.

> `docs/quickjs.md` carries an "In Progress / 75 of 122 methods" status table
> and lists `api.rs`/`transpile.rs`/`thread.rs` under `src/services/plugins/`.
> That layout is stale — those modules now live in the `fresh-plugin-runtime`
> crate, and the method list there should be treated as historical, not current.

---

## 5. Provider pattern (design-decisions.md §13)

**Problem.** Plugins that "own the UI" (the *Controller* pattern, built on
virtual buffers) must reimplement navigation, selection, and keybindings,
producing inconsistent UX and the keybinding/i18n bugs catalogued in
`plugin-usability-review.md` (custom-mode keys not firing; `editor.t()` not
interpolating).

**Decision.** Standardize on the **Provider pattern**: the plugin provides
*data*, the editor renders the *UI* and owns navigation. Two tiers:

- **QuickPick** — transient searches (Live Grep, Git Grep): the plugin streams
  results, the editor renders the picker with standard navigation.
- **ResultsPanel** — persistent panels (Find References, Diagnostics) with
  bidirectional cursor sync via `syncWithEditor`.

Related decisions in §13: prefer **atomic Rust actions** (e.g.
`delete_word_right`) over selection-then-delete for operator+motion combos to
avoid async timing races, with an `executeActions()` batch API (with count) for
patterns like `3dw`; and the **view-transform pipeline**
(`view_transform_request` → plugin rewrites tokens → `submitViewTransform`) for
content-transforming plugins like markdown compose — noted to flicker one frame
late under rapid scroll, which is why newer plugins prefer marker-based
`setLayoutHints`/`addVirtualLine` instead.

The widget runtime (§7) is the structural successor to the provider idea: the
host owns layout, focus, and hit-testing; the plugin describes data.

---

## 6. Plugin loading & lifecycle

### 6.1 Manager

`PluginManager` (`services/plugins/manager.rs`) owns the
`Option<PluginThreadHandle>` and exposes load/unload/reload, `run_hook`,
`deliver_response`, `process_commands`, `list_plugins`, `execute_action_async`,
and accessors for the shared `EditorStateSnapshot` and `SearchHandleRegistry`.
`process_commands_until_hook_completed()` (`manager.rs:303`) exists for a
render-blocking hook semantics but is currently unused (effectively dead code) —
PLANNED/unused.

### 6.2 Two-phase parallel loading

Bulk load (`thread.rs:1335`) is split to overlap I/O while keeping JS execution
serial and ordered (`parallel-plugin-loading.md`, commit `7a63ee073`):

- **Phase 1 — prepare (parallel, `std::thread::scope`, no external dep):** for
  each discovered `.ts`/`.js`: read file, load `.i18n.json`, extract
  `fresh:plugin/<name>` dependencies, transpile to JS, emit `.d.ts`, hash for
  cache. Produces `Vec<PreparedPlugin>`.
- **Phase 2 — execute (serial, on the plugin thread):** topologically sort by
  declared dependencies (Kahn's algorithm; cycles reported with the full path
  and refused), register i18n strings, eval each plugin's JS in order.
  Independent plugins keep alphabetical order for determinism.

`FRESH_TEST_TIMING=1` surfaces the phase split. **First-writer-wins** collision
detection (commit `26a03625f`) makes command/grammar/language/LSP registration
throw on a duplicate, so non-deterministic prepare order can't silently change
behavior; same-plugin re-registration (hot reload) is allowed.

After discovery and `init.ts`, the editor fires `PluginsLoaded` then `Ready`
(`hooks.rs:132`/`:137`).

### 6.3 Unload cleanup

Unload (`quickjs_backend.rs:7458`) removes the JS context, event handlers,
actions and callback contexts, then sends *compensating* commands to clear
everything the plugin created — overlays, conceals, soft breaks, virtual lines,
explorer decorations, line indicators, contexts — and tears down resources
(background processes, scroll-sync groups, buffers, terminals, path watches),
driven by the per-plugin `PluginTrackedState` record (`:674`).

### 6.4 Embedded plugins & dev workspace

With the `embed-plugins` feature, the `plugins/` tree is compiled in via
`include_dir!` and extracted to a content-addressed cache
(`~/.cache/fresh/embedded-plugins/{hash}/`) with an atomic
extract-to-temp-then-rename publish and an `.extracted` marker
(`services/plugins/embedded.rs`).

`PluginDevWorkspace` (`services/plugins/plugin_dev_workspace.rs`) supports
"Load Plugin from Buffer": it writes the buffer's content plus a copy of
`fresh.d.ts` and a `tsconfig.json` into a temp dir so the TypeScript LSP can
give autocomplete/hover on the plugin under development; the dir is removed on
`Drop`. The fuller design (untitled-buffer LSP integration) is in
`PLAN-lsp-plugin-buffer.md` — DESIGN, partly realized by this workspace.

---

## 7. Declarative widget runtime

The widget system is a virtual-DOM-like tree (`WidgetSpec`) authored in
TypeScript and reconciled host-side in Rust. The plugin describes data; the host
(`app/widget_runtime.rs`, `crates/fresh-editor/src/widgets/`) owns layout,
rendering, focus, hit-testing, scroll, selection, and cursor placement. Design:
`plugin-widget-library-design.md`.

### 7.1 Widget catalogue (IMPLEMENTED)

Spec kinds in `fresh-core/src/api.rs` (~`:1493`–`:1820`): `Row`, `Col`
(flex containers), `Spacer`, `Divider`, `HintBar`, `Toggle`, `Button`
(`intent: Normal|Primary|Danger`), `Text` (unified single-line vs multi-line by
`rows`), `List` (virtual-scrolled, host-owned scroll/selection), `Tree`
(disclosure + optional per-row checkboxes), and `Raw` (pre-rendered
`TextPropertyEntry[]` escape hatch). PLANNED per the design doc: `Table`,
`Tabs`/`Group`, and a `Layer`/`Prompt` compositor for modals/tooltips.

### 7.2 Mount and reconcile

`MountWidgetPanel { plugin, panel_id, buffer_id, spec }` (`api.rs` ~`:3918`) is
handled at `plugin_dispatch.rs:4268`: it resets instance state, calls
`render_spec()` (`widgets/render.rs:263`) to produce
`{ entries, hits, instance_states, focus_key, tabbable }`, stores it in the
`WidgetRegistry` keyed by `(owning plugin, panel_id)` (commit `87face687`), and
writes the rendered `TextPropertyEntry[]` as the buffer's virtual content. There
is also a floating/dock variant (`MountFloatingWidget`, commit `e64df8c29`).

**Spec/instance separation (the central rule):** spec values are *initial only*;
after first render, host-owned `WidgetInstanceState` (`widgets/registry.rs:88` —
`List { scroll, selected }`, `Text { editor, scroll, completions, … }`,
`Tree { scroll, selected, expanded_keys }`) is authoritative. Stable `key` fields
preserve instance state across spec re-emits. `UpdateWidgetPanel` re-renders
preserving state; `WidgetMutate` (`api.rs` ~`:2081`) is a fast path for targeted
updates (`SetValue`, `SetChecked`, `SetItems`, `SetExpandedKeys`,
`SetCompletions`, `AppendTreeNodes`, `SetFocusKey`, …) handled at
`plugin_dispatch.rs:4367`.

### 7.3 Events back to the plugin

Key/mouse input is routed through `widget_runtime.rs` (`handle_widget_command`
~`:378`, smart key dispatch `:406`, hit delivery `:113`). The host fires the
`WidgetEvent` hook (`hooks.rs:483`) carrying `(panel_id, widget_key, event_type,
payload)` — `event_type` ∈ `activate`/`toggle`/`change`/`submit`/`hover`/
`dismiss`/`focus` (plus `select`/`expand` for List/Tree). Events are delivered
**only to the panel's owning plugin** (commit `25375af6f`). Plugins never see
raw `(row, col)` — hit-testing is host-owned (design §9).

Status: the runtime, all 11 widgets, keyed reconciliation, mutators, completion
popups, and floating/dock panels are IMPLEMENTED and have live plugin call-sites
(New Session dialog, search/replace, git log). PLANNED per the design doc: the
unified compositor for modals/tooltips/context menus (the gating item for
remaining plugin migrations), full role-based theming (only `Button.intent`
ships), per-widget `catch_unwind` fault isolation, IME preedit, and spec-as-state
session restore.

---

## 8. Package loading

`services/packages.rs` (commit `597969946`) scans installed packages
*synchronously at startup* (`scan_installed_packages()`, `:358`), replacing an
older async JS path that serialized one grammar rebuild per package. It returns
`PackageScanResult { language_configs, lsp_configs, additional_grammars,
bundle_plugin_dirs, bundle_theme_dirs }`, applied in `editor_init.rs` (~`:761`)
with `or_insert` so user config wins over package defaults; bundle plugin/theme
dirs feed the plugin loader and theme loader.

A package is a directory with a `package.json` manifest
(`PackageManifest`, `:28`): `name`, optional `version`/`description`,
`type` ∈ `plugin|theme|theme-pack|language|bundle`, and a `fresh` block carrying
grammar/language/LSP config (language packs) or arrays of languages, plugins,
and themes (bundles). All fields use `#[serde(default)]` for forward
compatibility. The JSON schema is generated from the Rust types via schemars
(`src/bin/generate_schema.rs`, `./scripts/gen_schema.sh`) — IMPLEMENTED.

---

## 9. Marketplace / GPM (Git Package Manager)

> Naming note: in the marketplace context "GPM" means the **Git Package
> Manager** plugin model. This is distinct from the `services/gpm/` crate
> module, which is the unrelated Linux-console **General Purpose Mouse** FFI
> (§11). The task brief grouped them; the code does not.

The marketplace (`plugin-marketplace-design.md`) is **git-as-distribution**: a
plugin is a git repo cloned from any URL; the registry is itself just a git repo
of `plugins.json`/`themes.json`; package management lives in a `pkg.ts` plugin,
not editor core. The plugin (`crates/fresh-editor/plugins/pkg.ts`) is IMPLEMENTED
and provides `syncRegistry`/`loadRegistry`/`cacheRegistry`,
`getInstalledPackages`, and `gitCommand` wrappers, installing into
`~/.config/fresh/plugins/packages/<name>` via `git clone --depth 1`, with
commands like `pkg: Install Plugin`, `pkg: Update All`, `pkg: Sync Registry`.

Status: git distribution, registry sync/cache, basic install/update/remove, and
the code-review confirmation dialog are IMPLEMENTED. PLANNED/DESIGNED-only:
full semver matching (only `latest`/`main`/commit/`local` resolve today),
monorepo `#subdir` fragments, lockfile generation/restore, registry signing,
blocklist, theme preview, and a reusable plugin UI component library.

---

## 10. Sandboxing & security trade-offs

The plugin "sandbox" is **scope isolation, not a security boundary**. The code
has **no memory limit, no execution timeout/gas, and no interrupt handler** on
the QuickJS runtime — a plugin can infinite-loop (the JS-execution-state signal
in the service bridge lets the SIGINT handler surface it, but does not preempt).
Isolation comes from per-plugin contexts + IIFE wrapping (§2.3); there is no
global removal or syscall filtering. Filesystem and network access are not
ambient — plugins use `editor.readFile()` and `editor.spawnProcess()` / process
commands, which are auditable and routed through the editor — but a granted
`spawnProcess` is arbitrary code execution by design.

The marketplace design's security posture is therefore mostly **install-time
trust**: a confirmation dialog showing source/author/license before clone
(IMPLEMENTED), with registry signing and a malicious-package blocklist still
PLANNED. Workspace-trust gating exists at the command layer
(`SetEnv`/`ClearEnv` require a Trusted workspace; `TrustChanged` hook,
`hooks.rs:159`), and authority/env are owned per-window (`SetAuthority` triggers
a controlled editor restart). Net: Fresh trusts plugin code once installed and
leans on git provenance + an explicit consent step rather than runtime
confinement.

---

## 11. Subprocesses and GPM (mouse) — not a package manager

`fresh-plugin-runtime/src/process.rs` implements `spawn_plugin_process()`
(`:36`): a Tokio `Command` with piped stdio, concurrent stdout/stderr reads
(`tokio::join!`), `CREATE_NO_WINDOW` on Windows, streaming output back as
`AsyncMessage::ProcessOutput`. This backs the `SpawnProcess`/
`SpawnBackgroundProcess` commands.

`services/gpm/` is **General Purpose Mouse** — runtime `libloading` FFI to
`libgpm.so` (`ffi.rs`) giving mouse events on Linux virtual consoles where
xterm/SGR mouse protocols are unavailable. `client.rs` connects only when stdin
is a real `/dev/ttyN` (not a pseudoterminal/SSH), and `convert.rs` maps GPM
events to crossterm. It degrades to `Ok(None)` when libgpm is absent or an
xterm is detected. It is unrelated to plugins or packages and is documented here
only to dispel the name collision.

---

## 12. Implemented vs planned (summary)

IMPLEMENTED:
- QuickJS runtime on a dedicated thread; per-plugin context isolation; oxc TS
  transpile and `.d.ts` emit; async callback bridge; hook system with veto.
- ~190+ `PluginCommand` variants dispatched in `plugin_dispatch.rs`.
- Generated `fresh.d.ts` from proc macro + ts-rs.
- Two-phase parallel loading, dependency topo-sort, first-writer-wins collisions.
- Provider pattern (QuickPick / ResultsPanel); view-transform pipeline.
- Declarative widget runtime (11 widgets, keyed reconciliation, mutators,
  floating/dock panels, completion popups).
- Synchronous package scanner; `pkg.ts` git-based install/update.
- Embedded plugins; plugin-dev LSP workspace.

PLANNED / PARTIAL:
- Any runtime sandbox limits (memory/timeout/gas) and a WASM backend — none.
- Widget compositor for modals/tooltips; full role theming; per-widget fault
  isolation; IME preedit; widget session restore.
- Marketplace semver, lockfiles, monorepo fragments, registry signing, blocklist,
  theme preview, plugin UI component library.
- `process_commands_until_hook_completed` (render-blocking hooks) — unused.
- Transpile cache and an inter-plugin runtime export API (`parallel-plugin-
  loading.md` phases 4–5).
```
