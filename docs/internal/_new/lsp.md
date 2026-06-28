# LSP Integration

Purpose: document how Fresh embeds a full Language Server Protocol client — multi-server routing, the queuing/concurrency model, async result flow, diagnostics-as-markers, completion merging, and the feature set with its concessions vs. a full LSP client.

Scope: `crates/fresh-editor/src/services/lsp/` (`manager.rs`, `async_handler.rs`, `diagnostics.rs`, `mod.rs`), `crates/fresh-editor/src/services/completion/`, and the app-level glue in `app/` (`lsp_actions.rs`, `lsp_requests.rs`, `lsp_event_notify.rs`, `lsp_status.rs`, `diagnostic_jumps.rs`, `hover.rs`, `dabbrev_actions.rs`). Line refs are `file:line` against the tree at the time of writing; treat code as authoritative over the source design docs, several of which are now implemented or partially superseded (see end).

---

## 1. Layered architecture

Three layers, each on a different thread/runtime:

```
 Main thread (Editor)                 LspManager                Tokio runtime
 ───────────────────                  ──────────                ─────────────
 request_hover() ───► with_lsp_for_buffer() ─► LspHandle.hover() ─► command_tx
                                                                      │
 process_async_messages() ◄── AsyncBridge ◄── async_tx ◄── LspTask ◄─┘ (stdin/stdout
   handle_*_response()                                                  to subprocess)
```

- **`Editor` (main thread)** owns one `LspManager` and issues requests synchronously via dispatch helpers. It never blocks on a server: every request returns immediately, and responses arrive later as `AsyncMessage`s drained once per tick by `process_async_messages`.
- **`LspManager`** (`manager.rs:397`) is the routing/lifecycle layer. It owns `handles: Vec<ServerHandle>` (`manager.rs:407`) — **not** a `HashMap<String, LspHandle>`. This Vec-of-handles is the multi-server data model; routing is by `(language, feature)` rather than by a single per-language slot.
- **`LspHandle` / `LspTask`** (`async_handler.rs`) are the async client. `LspHandle` (`async_handler.rs:4448`) is a cheap sync handle holding an `mpsc::Sender<LspCommand>`; `LspTask` (`async_handler.rs:2790`) runs on tokio, owns the subprocess stdio, and does JSON-RPC.

The module header (`mod.rs:1-116`) still draws the older "one `LspHandle` per language" diagram; the real `handles` field is now a flat Vec to support multiple servers per language (the diagram is stale, the code is multi-server).

---

## 2. Multi-LSP architecture (IMPLEMENTED)

Fresh implements the "Option A + Option I" recommendation from `multi-lsp-design.md`: array-of-objects config and a multi-handle Vec. The design doc was a research draft; the feature is now shipped.

### 2.1 Data model

- `config: HashMap<String, Vec<LspServerConfig>>` (`manager.rs:410`) — N servers per language.
- `universal_configs: Vec<LspServerConfig>` (`manager.rs:413`) — servers that attach to every buffer regardless of language (e.g. a spell/grammar server).
- `handles: Vec<ServerHandle>` (`manager.rs:407`). Each `ServerHandle` (`manager.rs:347`) carries:
  - `name: String` — display/status identity.
  - `handle: LspHandle` — the async channel.
  - `feature_filter: FeatureFilter` — the only/except routing config.
  - `capabilities: ServerCapabilitySummary` — actual server capabilities from `InitializeResult`, intersected at dispatch time with the filter.

### 2.2 Feature classification: merged vs. exclusive

`LspFeature` (`types.rs`) splits features into two routing classes via `LspFeature::is_merged()` (`types.rs:45`):

- **Merged** (results combined from all eligible servers): `Diagnostics`, `Completion`, `CodeAction`, `DocumentSymbols`, `WorkspaceSymbols`.
- **Exclusive** (first eligible server in Vec order wins): `Hover`, `Definition`, `Implementation`, `References`, `Format`, `Rename`, `SignatureHelp`, `InlayHints`, `FoldingRange`, `SemanticTokens`, `DocumentHighlight`.

This matches the Helix model documented in `multi-lsp-design.md §4.3`. Vec order = priority for exclusive features. There is **no fallback-on-null**: if the first eligible server returns empty for an exclusive feature, that is the final answer (the doc's recommended "no fallback initially").

### 2.3 Feature routing

`FeatureFilter` (`types.rs:65`) is `All | Only(set) | Except(set)`, built from `LspServerConfig.only_features` / `except_features`. `allows(feature)` (`types.rs:72`) gates whether a server is eligible. Dispatch helpers in `manager.rs`:

- `handle_for_feature(language, feature) -> Option<&ServerHandle>` (`manager.rs:1032`) — exclusive: first handle whose `feature_filter.allows(feature)` **and** `capabilities.has_capability(feature)`.
- `handle_for_feature_mut(...)` (`manager.rs:1042`).
- `handles_for_feature(...) -> Vec<&ServerHandle>` (`manager.rs:1056`) and `_mut` (`manager.rs:1067`) — merged: all eligible handles.

`ServerHandle::has_capability` (`manager.rs:367`) **returns `false` before the server is initialized** (`ServerCapabilitySummary.initialized`, `manager.rs:196`). This is the load-bearing fix from `lsp-request-queuing-plan.md`: uninitialized servers are invisible to routing, so a request never lands on a server whose capabilities are unknown. No separate "pending handles" map is needed.

### 2.4 App-level dispatch wrappers

`lsp_requests.rs` wraps the manager helpers with `didOpen`-before-request guarantees:

- `with_lsp_for_buffer(buffer_id, feature, f) -> Option<R>` (`lsp_requests.rs:364`) — exclusive; ensures `didOpen`, calls `f` with the first eligible handle.
- `with_all_lsp_for_buffer_feature(...) -> Vec<R>` (`lsp_requests.rs:412`) — merged; calls `f` for every eligible handle.
- `with_all_lsp_for_buffer_feature_named(...)` (`lsp_requests.rs:473`) — same, but passes the server name so results can be attributed (e.g. code-action provenance).
- `ensure_did_open_all(...)` (`lsp_requests.rs:532`) — sends `didOpen` to every handle not yet in `metadata.lsp_opened_with`.

---

## 3. Document synchronization

LSP requires `didOpen` before any document request. Fresh tracks this **per buffer, per server instance**:

- Each `LspHandle` has a unique monotonic `id: u64` (`async_handler.rs:4450`, allocated from `NEXT_HANDLE_ID` at `async_handler.rs:4445`). A server restart gets a fresh id.
- `BufferMetadata.lsp_opened_with: HashSet<u64>` holds the handle ids that have received `didOpen` for that buffer. Before a request, the dispatch helper sends `didOpen` to any handle whose id is absent, then inserts the id. This naturally handles multi-server and restart cases (mod.rs:98-108).
- Broadcast notifications fan out to **all** handles for the language: `didOpen` on open/spawn (`reopen_buffers_for_language`, `lsp_actions.rs:126`), `didChange` on edit, `didClose` on close/disable (`disable_lsp_for_buffer`, `lsp_actions.rs:997`), `didSave` with full text on save (`notify_lsp_save_buffer`, `lsp_event_notify.rs:170`).

### 3.1 didChange conversion and version tracking

- `collect_lsp_changes(event)` (`lsp_event_notify.rs:16`) translates editor `Event::Insert`/`Delete`/`Batch` into `TextDocumentContentChangeEvent`s (incremental ranged changes, not full-document resync).
- Versions live in `document_versions: Arc<Mutex<HashMap<PathBuf, i64>>>` (`async_handler.rs:2812`), shared between `LspHandle` and `LspTask` so apply-edit version checks and didChange increments stay consistent.
- **didOpen grace period** (`DID_OPEN_GRACE_PERIOD_MS = 200`, `async_handler.rs:53`): a `didChange` issued within 200 ms of `didOpen` waits out the remainder before sending (`async_handler.rs:1392-1406`), tracked in `pending_opens` (`async_handler.rs:2816`). This avoids servers that race their own open processing.

---

## 4. Concurrency model: tasks, channels, queuing

### 4.1 Per-server tasks

Each server spawns one `LspTask` (`async_handler.rs:2790`) which in turn runs:

1. **Command dispatch loop** (`async_handler.rs:3169-3742`) — pulls `LspCommand`s off the `mpsc::channel(100)` (`async_handler.rs:4495`).
2. **stdout reader task** (`async_handler.rs:2965-3053`) — reads JSON-RPC frames continuously and independently.
3. **stderr logging task** (`async_handler.rs:2908`) — copies server stderr to a log file.
4. **Per-request spawned tasks** — feature requests are spawned onto independent tokio tasks via the `spawn_request!` macro (`async_handler.rs:3156`), so a slow request never blocks others on the same server.

Writes to stdin are serialized through a shared `tokio::Mutex<ChildStdin>` (`async_handler.rs:932`, used by both the command loop and the reader task) so JSON-RPC frames never interleave.

### 4.2 The queuing / init-gating model (and why)

The design problem (`lsp-request-queuing-plan.md`): a `ServerHandle` exists before `initialize` completes; requests sent in that window hit a server of unknown capabilities, and empty/error responses get mistaken for "nothing found," poisoning a "request already sent" flag.

The shipped solution is a **gate-and-retry** model, not a full request queue:

1. **Notifications are queued in the task.** `didOpen`/`didChange`/`didClose`/`didSave` received before init are pushed to `pending_commands` and replayed after the `initialize` handshake (`async_handler.rs` replay path; `state.initialized` gate at `async_handler.rs:3185`).
2. **Feature requests are gated on the main loop, not the task.** Because `has_capability` returns `false` pre-init (§2.3), no eligible handle exists yet, so the dispatch helper returns `None`/empty and the request simply isn't sent.
3. **Retry is natural, not queued.** Editor-initiated requests (semantic tokens, folding ranges) are re-issued from the `LspInitialized` handler. User-initiated requests (completion, definition, references, rename, code actions, signature help) are re-triggered by the user. Hover was the one gap — fixed by having `request_hover_at_position` return `Ok(bool)` (`lsp_requests.rs:883`) so the mouse-hover state machine only sets its "sent" flag on `Ok(true)` (see `hover.rs`).

The doc explicitly rejected the VS Code "queue everything" model: Fresh's `LspInitialized` handler already fires the right follow-ups, so the simpler gate+retry achieves zero-loss without a `PendingFeatureRequest` queue.

### 4.3 Debouncing

Debouncing is cost-proportional (per `design-decisions.md:560`, "50–500ms depending on cost"):

- **Completion**: trigger-char completions fire immediately if `suggest_on_trigger_characters`; word-char completions are delayed by `quick_suggestions_delay_ms` via `scheduled_completion_trigger` (`maybe_trigger_completion`, `lsp_requests.rs:721`).
- **Semantic tokens (range)**: `SEMANTIC_TOKENS_RANGE_DEBOUNCE_MS = 50` with `SEMANTIC_TOKENS_RANGE_PADDING_LINES = 10` (`lsp_requests.rs:99-100`).
- **Request timeout / cancellation**: `DEFAULT_REQUEST_TIMEOUT_MS = 30_000` (`async_handler.rs:60`). On timeout the task removes the pending entry and sends `$/cancelRequest` to the server (`async_handler.rs:1213-1232`). Editor-side cancellation maps `editor_request_id → lsp_request_id` via `active_requests` (`async_handler.rs:961`) and is driven by `CancelRequest`. Completions and code actions also cancel superseded in-flight requests on the main loop to avoid stale merges (the "clear previous pending set" guard in `request_completion`/`request_code_actions`).

### 4.4 Result flow back to the main thread

`LspTask` sends `AsyncMessage`s over `async_tx` (an `AsyncBridge` channel) which the main loop drains. Key variants: `LspInitialized { language, server_name, capabilities }`, `LspStatusUpdate`, `LspCompletion`, `LspHover`, `LspGotoDefinition`, `LspReferences`, `LspCodeActions`, `LspCodeActionResolved`, `LspFormatting`, `LspRename`, `LspPrepareRename`, `LspSemanticTokens`, `LspFoldingRanges`, `LspInlayHints`, `LspDiagnostics { uri, diagnostics, server_name }`, `LspPulledDiagnostics`, `LspProgress`, `LspApplyEdit`, `LspServerRequest`, `LspError`. Each is consumed by a `handle_*_response` method in `lsp_requests.rs` that validates the request id (rejecting stale responses) before touching UI.

### 4.5 Server→client requests handled in the reader task

The stdout reader (`async_handler.rs:4082-4333`) answers server-initiated requests **directly**, without going through the command loop — this is what makes nested `executeCommand → applyEdit` deadlock-free (`lsp-feature-parity-plan.md` Phase 1 edge case):

- `workspace/applyEdit` → relays `LspApplyEdit` to main loop, replies `{ applied: bool }` (`async_handler.rs:4270`).
- `workspace/configuration` → answers from `initialization_options`.
- `client/registerCapability` / `unregisterCapability` → mutates the capability snapshot (lets servers like pyright register diagnostics dynamically).
- `workspace/{diagnostic,inlayHint,semanticTokens}/refresh` → triggers re-pulls.
- `window/workDoneProgress/create` → acked.
- Unknown methods → forwarded to plugins as `LspServerRequest`.

---

## 5. Server lifecycle

Spawning is funneled through a single throttle, `spawn_decision` (`manager.rs:1086`), used by `try_spawn`, manual restart, and pending-restart processing.

- **Restart throttling / backoff** (`manager.rs:85-88`): `MAX_RESTARTS_IN_WINDOW = 5`, `RESTART_WINDOW_SECS = 180`, `RESTART_BACKOFF_BASE_MS = 1000`. Backoff is exponential: `delay = 1000 * (1 << attempt)` → 1s, 2s, 4s, 8s (`manager.rs:1537`). Exceeding the window cap drops the server into `restart_cooldown` (`manager.rs:451`).
- **Crash isolation**: `handle_server_crash` (`manager.rs:1467`) removes only the crashing handle and schedules its own backoff restart; other servers for the language continue. Universal servers are drained together.
- **auto_start default is `false`** for most language servers (confirmed by `lsp-plugin-testing.md` and the heuristic eval). LSP must be started manually via the command palette unless configured otherwise. This is a deliberate resource choice but is the root of heuristic finding H-1 (dormant LSP invisible).
- Manual lifecycle commands live in `lsp_actions.rs`: `handle_lsp_restart` (`:17`), `handle_lsp_stop` (`:268`), `handle_lsp_toggle_for_buffer` (`:351`), and the status-popup action router `handle_lsp_status_action` (`:409`) which handles `restart:` / `start:` / `stop:` / `autostart:` / `dismiss:` / `enable:` / `plugin:` action keys.

### 5.1 Workspace root detection (IMPLEMENTED)

`detect_workspace_root(file_path, root_markers)` (`manager.rs:166`) walks upward from the file's directory looking for any marker, returning the first match or the file's parent (never `$HOME`/cwd). Resolution priority in `resolve_root_uri` (`manager.rs:899`): plugin-set `per_language_root_uris` → marker walk from the file → global `root_uri` fallback. `root_markers` is a real `LspServerConfig` field, so different servers for one language can resolve different roots (monorepo case). This is `multi-lsp-design.md §4.8` Phase 0, shipped.

### 5.2 Language detection and the `.h` problem

`detect_language` (`manager.rs:1944`) resolves by exact filename → glob → extension. The `.h`→C-vs-C++ ambiguity (heuristic eval H-8) is handled: `header_in_cpp_tree` (`manager.rs:2048`) promotes a `.h` from `c` to `cpp` when it has C++ sibling sources or an ancestor `compile_commands.json` (remediation H-8 marked done).

---

## 6. Diagnostics: storage as markers

Diagnostics are stored two ways:

1. **Raw, per URI**: `stored_diagnostics: HashMap<String, Vec<Diagnostic>>` keyed by document URI (per window). This is the source of truth for hover fusion, code-action context, and re-application. `LspDiagnostics` carries `server_name`, so per-server diagnostics can be tracked/cleared without clobbering another server's set (`clear_diagnostics_for_server`).
2. **As editor overlays/markers**: `diagnostics.rs` converts each `Diagnostic` into an `Overlay` in the `lsp-diagnostic` namespace (`diagnostics.rs:16`), anchored to `marker_list` so positions track edits. `diagnostic_to_overlay` (`diagnostics.rs:150`) maps severity → themed background face + priority (error 100, warning 50, info 30, hint 10). Overlays carry the diagnostic message so the status bar and navigation can show it.

A content hash cache (`DIAGNOSTIC_CACHE`, `diagnostics.rs:22`) keyed by file path skips overlay rebuilds when diagnostics are unchanged on a keystroke (`apply_diagnostics_to_state_cached`, `diagnostics.rs:97`); it is invalidated on edit (`invalidate_cache_for_file`) and on theme change (`invalidate_cache_all`).

**Navigation**: `diagnostic_jumps.rs` implements F8/Shift+F8 by reading overlay positions in the diagnostic namespace — `jump_to_next_error` (`:16`) finds the first diagnostic after the cursor (wrapping), `jump_to_previous_error` (`:87`) the reverse, both showing the message in the status bar.

**Hover fusion** (heuristic eval H-5, done): `handle_hover_response` (`lsp_requests.rs:939`) calls `compose_hover_diagnostic_lines(pos)` (`lsp_requests.rs:1193`), which filters `stored_diagnostics` for the buffer's URI and selects diagnostics whose range overlaps the hover position via `lsp_range_contains`/`lsp_range_overlaps` (`lsp_requests.rs:41`,`:64`). Matching diagnostics are rendered (severity glyph + label + source + message) above a separator, then the hover body. An empty hover still opens the popup if a diagnostic is present.

---

## 7. Completion sources and merging

The completion stack is a separate service (`services/completion/`) from LSP, into which LSP feeds as one async provider.

### 7.1 The provider framework

`CompletionService` (`service.rs:28`) owns `Vec<Box<dyn CompletionProvider>>` and a `pending_async` list. Built-in providers registered at construction: `BufferWordProvider`, `DabbrevProvider` (`service.rs:41-42`). The LSP provider and TypeScript-plugin providers register dynamically. Each `CompletionProvider` (`provider.rs:188`) declares `id`, `is_enabled`, `provide`, and `priority` (convention `provider.rs:213`: **0 = LSP, 10 = ctags/index, 20 = buffer words, 30 = dabbrev**).

`provide` returns `ProviderResult::Ready(candidates)` or `Pending(request_id)` (`provider.rs:171`). LSP is the canonical `Pending` source — its results arrive asynchronously and are fed back via `supply_async_results(request_id, candidates)` (`service.rs:108`).

### 7.2 Merge / rank / dedup

- **Sync merge**: `request(ctx, buffer_window)` (`service.rs:69`) sorts providers by priority, runs each enabled one over the same pre-sliced byte window, tags candidates with their source, concatenates, then `rank`s.
- **`rank`** (`service.rs:135`): sort by score desc, tie-break by lowercased label; dedup by `(lowercase label, insert_text)` keeping the highest score.
- **Async merge (LSP, multi-server)**: each server's completion response arrives separately. `handle_completion_response(request_id, items)` (`lsp_requests.rs:106`) removes the id from `pending_completion_requests`, prefix-filters, and **extends** the accumulated `completion_items` — first response creates the popup, later responses extend it (the Helix "first creates, rest extend" pattern; multi-server merge for the `Completion` merged feature). Stale-merge guard: a fresh `request_completion` clears the previous pending set/items first.
- **Fallback**: if all LSP servers return empty, the editor falls back to a buffer-word popup (`show_buffer_word_completion_popup`, `lsp_requests.rs:682`).

### 7.3 dabbrev and snippets

- **dabbrev** is both a completion provider (`dabbrev.rs`, scans a proximity-ordered byte window, then other open buffers in MRU order, Unicode-grapheme-aware, language-aware word chars) **and** a standalone Emacs-style `M-/` cycling command (`dabbrev_actions.rs`: `dabbrev_expand` `:23`, `dabbrev_expand_first` `:81`, `dabbrev_cycle` `:32`). The cycling path bypasses the popup entirely — it deletes and re-inserts in place, holding `dabbrev_state`.
- **Huge-file contract**: providers never touch the `Buffer`; the service pre-slices a `scan_range` — 512 KB radius normally, 32 KB for ≥100 MB lazily-loaded files (`provider.rs:147-150`, `mod.rs:39-47`). This makes an accidental full-buffer scan structurally impossible.
- **Snippets**: `CompletionCandidate.is_snippet` (`provider.rs:36`) marks LSP-style `${1:...}`/`$0` insert text; snippet content itself is supplied by TypeScript plugin providers (per the Rust-core-vs-plugin split in `mod.rs`). There is no dedicated Rust snippet provider — snippets ride through the LSP and plugin providers.

---

## 8. Supported features and concessions

**Implemented** (request method in `lsp_requests.rs` / handler unless noted):

| Feature | Status / notes |
|---|---|
| Completion (+ `completionItem/resolve`) | Multi-server merged; resolve applies `additionalTextEdits` (auto-imports) on accept |
| Hover (+ diagnostic fusion) | Exclusive; fuses overlapping diagnostics |
| Go to definition / implementation | Exclusive; jumps to first location |
| Find references | Exclusive; results delivered to a results panel via the `lsp_references` plugin hook |
| Rename (+ `prepareRename`) | Exclusive; prepareRename pre-validates when advertised |
| Code actions (+ `codeAction/resolve`, `workspace/executeCommand`, `workspace/applyEdit`) | Merged across servers, server-attributed; full three-way dispatch in `execute_code_action` (`:1930`) — edit / command / resolve-then-execute |
| Signature help | Exclusive |
| Diagnostics (publish + pull) | Per-server tracked, overlay markers, F8 navigation, hover fusion |
| Inlay hints | Exclusive; rendered as virtual text |
| Semantic tokens (full / delta / range) | Exclusive; range-debounced |
| Folding ranges | Exclusive |
| Document formatting / range formatting | Exclusive; `LspFormatting` applies edits (parity Phase 2, now implemented per `LspCommand` variants) |
| `workspace/applyEdit` with version checking + resource ops | `apply_workspace_edit` (`:2623`) handles `CreateFile`/`RenameFile`/`DeleteFile` and rejects stale-version `TextDocumentEdit`s — `lsp-feature-parity-plan.md` Phase 1, done |
| Progress (`$/progress`) | Relayed to status bar (see gap below) |
| Plugin-buffer LSP | `setup_plugin_dev_lsp` (`lsp_actions.rs:1252`) writes a temp `.ts` + `tsconfig.json` + `fresh.d.ts` so unnamed plugin buffers get TS intelligence (`PLAN-lsp-plugin-buffer.md` Alternative A, implemented) |

**Concessions / gaps vs. a full LSP client:**

- **No fallback-on-null for exclusive features** — first eligible server's answer is final even if empty (deliberate, §2.2).
- **auto_start defaults off** — servers are dormant until manually started; the dormant state is weakly surfaced (heuristic eval H-1; `compose_lsp_status` renders an `LSP (off)` indicator state, `lsp_status.rs:69`, but see §9).
- **`$/progress` may not render** during some indexing sessions (heuristic eval H-2): the relay exists end-to-end (`LspProgress` → status bar) but did not surface during the clangd/fmt run; classified as a bug in an existing feature, not a missing one.
- **Plan-only / lower-priority** per `lsp-feature-parity-plan.md`: `textDocument/documentSymbol`, `workspace/symbol`, `textDocument/documentHighlight`, `window/showMessageRequest`, `window/showDocument`, on-type formatting, linked editing, selection range, `workspace/didChangeWatchedFiles`, and file-operation events (`willCreate/Rename/Delete`). Treat these as PLANNED unless code in `LspCommand` (`async_handler.rs:720`) shows otherwise — `LspFeature` lists `DocumentSymbols`/`WorkspaceSymbols` as merged classes, so the routing slots exist even where request wiring may be partial.
- **References is exclusive**, not merged — `multi-lsp-design.md` Open Question 1 (merge references across servers) was left as exclusive.

---

## 9. Discrepancies between docs and code

- **Status-bar composition moved.** `LSP_HEURISTIC_EVAL_REMEDIATION.md §2.3` plans the dormant-server indicator to land in `app/render.rs` as a four-case branch. The current code instead has a dedicated, unit-tested pure function `compose_lsp_status(...)` in `app/lsp_status.rs:69` returning `(String, LspIndicatorState)` with a documented priority order (buffer-disabled → progress → error → running → configured-off → empty). The render-time branch described in the remediation doc is superseded by this function. Indicator width is fixed at `INDICATOR_WIDTH = 11` (`lsp_status.rs:24`) to prevent status-bar reflow.
- **`mod.rs` architecture diagram is stale.** It shows one `LspHandle` per language keyed in a manager map; the real model is `handles: Vec<ServerHandle>` routed by `(language, feature)` (§2).
- **`multi-lsp-design.md` and `lsp-request-queuing-plan.md` are research/plan drafts that are now implemented.** The Vec-of-handles, `FeatureFilter`, `has_capability`-pre-init-false gate, and per-server diagnostics are all in code.

---

## Summary for callers

Fresh runs a real multi-server LSP client: per language you can configure N servers, each with `only_features`/`except_features` routing; merged features (diagnostics, completion, code actions, symbols) fan out to all eligible servers and combine, while exclusive features (hover, definition, format, rename, …) take the first eligible server by config order. Each server is one tokio `LspTask` with a stdin-serialized writer and an independent stdout reader that answers server→client requests (applyEdit, configuration, capability registration) directly to avoid nested-request deadlock; feature requests spawn as independent tasks so one slow request can't block the server. Results return to the main thread as `AsyncMessage`s drained once per tick.

The concurrency story is "gate and retry," not "queue everything": notifications are queued in the task and replayed after `initialize`, while feature requests are simply not routed until `has_capability` flips true post-init, with editor-initiated requests re-issued on `LspInitialized` and user-initiated ones re-triggered naturally. Diagnostics are stored both per-URI (truth) and as marker-anchored overlays (display), feeding hover fusion and F8 navigation. Completion is a provider framework where LSP is one async source merged with dabbrev/buffer-words/plugins by priority-then-score with dedup. The main concessions vs. a full client are auto-start-off-by-default, no null-fallback for exclusive features, references-as-exclusive, and a set of still-planned navigation/symbol/file-watch features.

### Old docs superseded or now-implemented by this document

- `docs/internal/multi-lsp-design.md` — research draft; the recommended Option A config + Option I multi-handle Vec, `FeatureFilter`, per-server diagnostics, and `root_markers` are all implemented.
- `docs/internal/lsp-request-queuing-plan.md` — the gate-and-retry model (`has_capability` false pre-init, hover `Ok(bool)` flag fix) is shipped; no full request queue was built, as the plan recommended.
- `docs/internal/lsp-feature-parity-plan.md` — Phase 1 (applyEdit version checking + resource ops, server-side applyEdit, executeCommand, codeAction/resolve) and key Phase 2 items (formatting, prepareRename, completionItem/resolve) are implemented; Phase 3–4 remain PLANNED.
- `docs/internal/PLAN-lsp-plugin-buffer.md` — Alternative A (temp `.ts` + tsconfig + fresh.d.ts) implemented as `setup_plugin_dev_lsp`.
- `docs/internal/LSP_HEURISTIC_EVAL_REMEDIATION.md` — partially superseded: status composition now lives in `app/lsp_status.rs::compose_lsp_status`, not the planned `render.rs` branch; H-5 (hover/diagnostic fusion) and H-8 (`.h`→C++) are done.
- `docs/internal/LSP_HEURISTIC_EVAL_CLANGD.md` and `docs/internal/lsp-plugin-testing.md` — point-in-time evaluations; retain as historical UX/test records, not architecture.
