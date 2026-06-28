# LSP Integration

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: document how Fresh embeds a full Language Server Protocol client — multi-server routing, the queuing/concurrency model, async result flow, diagnostics-as-markers, completion merging, and the feature set with its concessions vs. a full LSP client.

Scope: the LSP service layer (the LSP manager, the async client, diagnostics), the completion service, and the app-level glue (LSP actions, requests, event notification, status, diagnostic navigation, hover, dabbrev). The source is authoritative; several of the older internal design notes referenced here are now implemented or partially superseded (see end).

---

## 1. Layered architecture

Three layers, each on a different thread/runtime:

- **Editor (main thread)** owns one LSP manager and issues requests synchronously via dispatch helpers. It never blocks on a server: every request returns immediately, and responses arrive later as async messages drained once per tick.
- **The LSP manager** is the routing/lifecycle layer. It owns a flat `Vec` of server handles — **not** a per-language map. This Vec-of-handles is the multi-server data model; routing is by `(language, feature)` rather than by a single per-language slot.
- **Server handle / LSP task** are the async client. The handle is a cheap sync object holding a command-channel sender; the task runs on tokio, owns the subprocess stdio, and does JSON-RPC.

An older module-header diagram still draws the "one handle per language" model; the real handles field is a flat Vec to support multiple servers per language (the diagram is stale, the code is multi-server).

---

## 2. Multi-LSP architecture (IMPLEMENTED)

Fresh implements the array-of-objects config plus multi-handle Vec recommendation from the multi-LSP design note. That note was a research draft; the feature is now shipped.

### 2.1 Data model

- A config map from language to a list of server configs — N servers per language.
- A list of universal configs — servers that attach to every buffer regardless of language (e.g. a spell/grammar server).
- A flat list of server handles. Each handle carries:
  - A name — display/status identity.
  - The async channel handle.
  - A feature filter — the only/except routing config.
  - A capability summary — actual server capabilities from the initialize result, intersected at dispatch time with the filter.

### 2.2 Feature classification: merged vs. exclusive

`LspFeature` splits features into two routing classes:

- **Merged** (results combined from all eligible servers): diagnostics, completion, code actions, document symbols, workspace symbols.
- **Exclusive** (first eligible server in Vec order wins): hover, definition, implementation, references, format, rename, signature help, inlay hints, folding range, semantic tokens, document highlight.

This matches the Helix model. Vec order = priority for exclusive features. There is **no fallback-on-null**: if the first eligible server returns empty for an exclusive feature, that is the final answer (the deliberate "no fallback initially" choice).

### 2.3 Feature routing

The feature filter is `All | Only(set) | Except(set)`, built from each server config's only/except feature lists. An `allows(feature)` check gates whether a server is eligible. Dispatch helpers on the manager:

- An exclusive lookup returning the first handle whose filter allows the feature **and** whose capabilities advertise it.
- A merged lookup returning all eligible handles.

Crucially, a handle **reports no capabilities before the server is initialized**. This is the load-bearing fix from the request-queuing plan: uninitialized servers are invisible to routing, so a request never lands on a server whose capabilities are unknown. No separate "pending handles" map is needed.

### 2.4 App-level dispatch wrappers

The request layer wraps the manager helpers with `didOpen`-before-request guarantees:

- An exclusive wrapper that ensures `didOpen`, then calls the caller's closure with the first eligible handle.
- A merged wrapper that calls the closure for every eligible handle.
- A named merged variant that also passes the server name so results can be attributed (e.g. code-action provenance).
- A helper that sends `didOpen` to every handle not yet recorded as having opened the buffer.

---

## 3. Document synchronization

LSP requires `didOpen` before any document request. Fresh tracks this **per buffer, per server instance**:

- Each handle has a unique monotonic id allocated from a global counter. A server restart gets a fresh id.
- Buffer metadata holds the set of handle ids that have received `didOpen` for that buffer. Before a request, the dispatch helper sends `didOpen` to any handle whose id is absent, then records the id. This naturally handles multi-server and restart cases.
- Broadcast notifications fan out to **all** handles for the language: `didOpen` on open/spawn, `didChange` on edit, `didClose` on close/disable, `didSave` with full text on save.

### 3.1 didChange conversion and version tracking

- Editor insert/delete/batch events are translated into content-change events as incremental ranged changes, not full-document resync.
- Document versions live in a shared, mutex-guarded map keyed by path, shared between handle and task so apply-edit version checks and `didChange` increments stay consistent.
- **didOpen grace period**: a `didChange` issued shortly after `didOpen` waits out the remainder of a short grace window before sending, tracked in a pending-opens set. This avoids servers that race their own open processing.

---

## 4. Concurrency model: tasks, channels, queuing

### 4.1 Per-server tasks

Each server spawns one LSP task, which in turn runs:

1. **A command dispatch loop** — pulls commands off a bounded command channel.
2. **A stdout reader task** — reads JSON-RPC frames continuously and independently.
3. **A stderr logging task** — copies server stderr to a log file.
4. **Per-request spawned tasks** — feature requests are spawned onto independent tokio tasks, so a slow request never blocks others on the same server.

Writes to stdin are serialized through a shared async mutex over the child's stdin (used by both the command loop and the reader task) so JSON-RPC frames never interleave.

### 4.2 The queuing / init-gating model (and why)

The design problem: a server handle exists before `initialize` completes; requests sent in that window hit a server of unknown capabilities, and empty/error responses get mistaken for "nothing found," poisoning a "request already sent" flag.

The shipped solution is a **gate-and-retry** model, not a full request queue:

1. **Notifications are queued in the task.** `didOpen`/`didChange`/`didClose`/`didSave` received before init are pushed to a pending-commands buffer and replayed after the `initialize` handshake, gated on an initialized flag.
2. **Feature requests are gated on the main loop, not the task.** Because a handle reports no capabilities pre-init (§2.3), no eligible handle exists yet, so the dispatch helper returns nothing and the request simply isn't sent.
3. **Retry is natural, not queued.** Editor-initiated requests (semantic tokens, folding ranges) are re-issued from the initialized handler. User-initiated requests (completion, definition, references, rename, code actions, signature help) are re-triggered by the user. Hover was the one gap — fixed by having the hover request return a boolean so the mouse-hover state machine only sets its "sent" flag on a true result.

The "queue everything" model (as in VS Code) was explicitly rejected: Fresh's initialized handler already fires the right follow-ups, so the simpler gate-and-retry achieves zero-loss without a pending-feature-request queue.

### 4.3 Debouncing

Debouncing is cost-proportional (more expensive features wait longer):

- **Completion**: trigger-char completions fire immediately when suggest-on-trigger-characters is set; word-char completions are delayed by a configurable quick-suggestions delay via a scheduled trigger.
- **Semantic tokens (range)**: short debounce with a small padding of lines around the visible range.
- **Request timeout / cancellation**: a default request timeout applies. On timeout the task removes the pending entry and sends `$/cancelRequest` to the server. Editor-side cancellation maps an editor request id to the LSP request id via an active-requests map, driven by a cancel command. Completions and code actions also cancel superseded in-flight requests on the main loop to avoid stale merges (the "clear previous pending set" guard).

### 4.4 Result flow back to the main thread

The task sends async messages over a bridge channel which the main loop drains. Variants cover initialization, status updates, completion, hover, goto-definition, references, code actions and resolution, formatting, rename and prepare-rename, semantic tokens, folding ranges, inlay hints, diagnostics (published and pulled), progress, apply-edit, server-initiated requests, and errors. The diagnostics variant carries the server name so per-server sets stay distinct. Each is consumed by a response handler that validates the request id (rejecting stale responses) before touching UI.

### 4.5 Server→client requests handled in the reader task

The stdout reader answers server-initiated requests **directly**, without going through the command loop — this is what makes nested `executeCommand → applyEdit` deadlock-free:

- `workspace/applyEdit` → relays an apply-edit message to the main loop, replies with an applied flag.
- `workspace/configuration` → answers from the configured initialization options.
- `client/registerCapability` / `unregisterCapability` → mutates the capability snapshot (lets servers register diagnostics dynamically).
- `workspace/{diagnostic,inlayHint,semanticTokens}/refresh` → triggers re-pulls.
- `window/workDoneProgress/create` → acked.
- Unknown methods → forwarded to plugins as a server-request message.

---

## 5. Server lifecycle

Spawning is funneled through a single throttle/decision point, used by automatic spawn, manual restart, and pending-restart processing.

- **Restart throttling / backoff**: a capped number of restarts within a rolling window, with exponential backoff between attempts. Exceeding the window cap drops the server into a restart cooldown.
- **Crash isolation**: a crash handler removes only the crashing handle and schedules its own backoff restart; other servers for the language continue. Universal servers are drained together.
- **auto_start defaults to `false`** for most language servers. LSP must be started manually via the command palette unless configured otherwise. This is a deliberate resource choice but is the root of a known finding (a dormant LSP being weakly surfaced).
- Manual lifecycle commands live in the LSP actions layer: restart, stop, per-buffer toggle, and a status-popup action router handling restart / start / stop / autostart / dismiss / enable / plugin action keys.

### 5.1 Workspace root detection (IMPLEMENTED)

Root detection walks upward from the file's directory looking for any configured marker, returning the first match or the file's parent (never `$HOME`/cwd). Resolution priority: plugin-set per-language root URIs → marker walk from the file → global root-URI fallback. Root markers are a real per-server config field, so different servers for one language can resolve different roots (the monorepo case).

### 5.2 Language detection and the `.h` problem

Language detection resolves by exact filename → glob → extension. The `.h`→C-vs-C++ ambiguity is handled: a `.h` is promoted from `c` to `cpp` when it has C++ sibling sources or an ancestor `compile_commands.json`.

---

## 6. Diagnostics: storage as markers

Diagnostics are stored two ways:

1. **Raw, per URI**: a map keyed by document URI (per window). This is the source of truth for hover fusion, code-action context, and re-application. The diagnostics message carries the server name, so per-server diagnostics can be tracked and cleared without clobbering another server's set.
2. **As editor overlays/markers**: each diagnostic is converted into an overlay in a dedicated diagnostic namespace, anchored to the marker list so positions track edits. Severity maps to a themed background face plus a priority ordering (error > warning > info > hint). Overlays carry the diagnostic message so the status bar and navigation can show it.

A content-hash cache keyed by file path skips overlay rebuilds when diagnostics are unchanged on a keystroke; it is invalidated on edit and on theme change.

**Navigation**: next/previous-error commands read overlay positions in the diagnostic namespace — one finds the first diagnostic after the cursor (wrapping), the other the reverse, both showing the message in the status bar.

**Hover fusion**: the hover response handler composes diagnostic lines for the position by filtering stored diagnostics for the buffer's URI and selecting those whose range overlaps the hover position. Matching diagnostics are rendered (severity glyph + label + source + message) above a separator, then the hover body. An empty hover still opens the popup if a diagnostic is present.

---

## 7. Completion sources and merging

The completion stack is a separate service from LSP, into which LSP feeds as one async provider.

### 7.1 The provider framework

The completion service owns a list of boxed providers and a pending-async list. Built-in providers registered at construction: a buffer-word provider and a dabbrev provider. The LSP provider and TypeScript-plugin providers register dynamically. Each provider declares an id, an enabled check, a provide method, and a priority. The priority convention orders LSP first, then ctags/index, then buffer words, then dabbrev.

Provide returns either ready candidates or a pending request id. LSP is the canonical pending source — its results arrive asynchronously and are fed back through a supply-async-results entry point.

### 7.2 Merge / rank / dedup

- **Sync merge**: the request entry point sorts providers by priority, runs each enabled one over the same pre-sliced byte window, tags candidates with their source, concatenates, then ranks.
- **Rank**: sort by score descending, tie-break by lowercased label; dedup by (lowercased label, insert text) keeping the highest score.
- **Async merge (LSP, multi-server)**: each server's completion response arrives separately. The completion-response handler removes the id from the pending set, prefix-filters, and **extends** the accumulated items — the first response creates the popup, later responses extend it (the Helix "first creates, rest extend" pattern; multi-server merge for the merged completion feature). Stale-merge guard: a fresh completion request clears the previous pending set and items first.
- **Fallback**: if all LSP servers return empty, the editor falls back to a buffer-word popup.

### 7.3 dabbrev and snippets

- **dabbrev** is both a completion provider (scanning a proximity-ordered byte window, then other open buffers in MRU order, Unicode-grapheme-aware, language-aware word chars) **and** a standalone Emacs-style cycling command. The cycling path bypasses the popup entirely — it deletes and re-inserts in place, holding cycling state.
- **Huge-file contract**: providers never touch the buffer directly; the service pre-slices a scan range — a bounded radius normally, narrower for very large lazily-loaded files. This makes an accidental full-buffer scan structurally impossible.
- **Snippets**: a candidate flag marks LSP-style placeholder insert text; snippet content itself is supplied by TypeScript plugin providers. There is no dedicated Rust snippet provider — snippets ride through the LSP and plugin providers.

---

## 8. Supported features and concessions

**Implemented:**

| Feature | Status / notes |
|---|---|
| Completion (+ resolve) | Multi-server merged; resolve applies additional text edits (auto-imports) on accept |
| Hover (+ diagnostic fusion) | Exclusive; fuses overlapping diagnostics |
| Go to definition / implementation | Exclusive; jumps to first location |
| Find references | Exclusive; results delivered to a results panel via a plugin hook |
| Rename (+ prepareRename) | Exclusive; prepareRename pre-validates when advertised |
| Code actions (+ resolve, executeCommand, applyEdit) | Merged across servers, server-attributed; full three-way dispatch — edit / command / resolve-then-execute |
| Signature help | Exclusive |
| Diagnostics (publish + pull) | Per-server tracked, overlay markers, navigation commands, hover fusion |
| Inlay hints | Exclusive; rendered as virtual text |
| Semantic tokens (full / delta / range) | Exclusive; range-debounced |
| Folding ranges | Exclusive |
| Document formatting / range formatting | Exclusive; applies edits |
| `workspace/applyEdit` with version checking + resource ops | Handles create/rename/delete file and rejects stale-version document edits |
| Progress (`$/progress`) | Relayed to status bar (see gap below) |
| Plugin-buffer LSP | A setup path writes a temp `.ts` + `tsconfig.json` + type-declaration file so unnamed plugin buffers get TS intelligence |

**Concessions / gaps vs. a full LSP client:**

- **No fallback-on-null for exclusive features** — the first eligible server's answer is final even if empty (deliberate, §2.2).
- **auto_start defaults off** — servers are dormant until manually started; the dormant state is weakly surfaced. The status composition renders an off indicator state, but surfacing is limited (see §9).
- **`$/progress` may not render** during some indexing sessions: the relay exists end-to-end (progress message → status bar) but did not surface in observed runs; classified as a bug in an existing feature, not a missing one.
- **PLANNED / lower-priority**: document symbol, workspace symbol, document highlight, `window/showMessageRequest`, `window/showDocument`, on-type formatting, linked editing, selection range, `workspace/didChangeWatchedFiles`, and file-operation events (will-create/rename/delete). Treat these as PLANNED; `LspFeature` lists document and workspace symbols as merged classes, so the routing slots exist even where request wiring may be partial.
- **References is exclusive**, not merged — merging references across servers was left open and references kept exclusive.

---

## 9. Discrepancies between docs and code

- **Status-bar composition is a pure function.** An earlier remediation note planned the dormant-server indicator as a multi-case branch at render time. The current code instead has a dedicated, unit-tested pure function returning a label plus indicator state with a documented priority order (buffer-disabled → progress → error → running → configured-off → empty). The planned render-time branch is superseded by this function. Indicator width is fixed to prevent status-bar reflow.
- **The module architecture diagram is stale.** It shows one handle per language keyed in a manager map; the real model is a flat Vec of handles routed by `(language, feature)` (§2).
- **The multi-LSP design and request-queuing notes are research/plan drafts that are now implemented.** The Vec-of-handles, the feature filter, the no-capabilities-pre-init gate, and per-server diagnostics are all in code.

---

## Summary for callers

Fresh runs a real multi-server LSP client: per language you can configure N servers, each with only/except feature routing; merged features (diagnostics, completion, code actions, symbols) fan out to all eligible servers and combine, while exclusive features (hover, definition, format, rename, …) take the first eligible server by config order. Each server is one tokio task with a stdin-serialized writer and an independent stdout reader that answers server→client requests (applyEdit, configuration, capability registration) directly to avoid nested-request deadlock; feature requests spawn as independent tasks so one slow request can't block the server. Results return to the main thread as async messages drained once per tick.

The concurrency story is "gate and retry," not "queue everything": notifications are queued in the task and replayed after `initialize`, while feature requests are simply not routed until a server reports capabilities post-init, with editor-initiated requests re-issued on initialization and user-initiated ones re-triggered naturally. Diagnostics are stored both per-URI (truth) and as marker-anchored overlays (display), feeding hover fusion and error navigation. Completion is a provider framework where LSP is one async source merged with dabbrev/buffer-words/plugins by priority-then-score with dedup. The main concessions vs. a full client are auto-start-off-by-default, no null-fallback for exclusive features, references-as-exclusive, and a set of still-planned navigation/symbol/file-watch features.

### Older notes superseded or now-implemented by this document

- The multi-LSP design note — research draft; the recommended array-of-objects config, multi-handle Vec, feature filter, per-server diagnostics, and root markers are all implemented.
- The request-queuing plan — the gate-and-retry model (no capabilities pre-init, the hover boolean-flag fix) is shipped; no full request queue was built, as the plan itself recommended.
- The feature-parity plan — applyEdit version checking and resource ops, server-side applyEdit, executeCommand, code-action resolve, and the key formatting / prepareRename / completion-resolve items are implemented; later phases remain PLANNED.
- The plugin-buffer plan — the temp `.ts` + tsconfig + type-declaration approach is implemented as the plugin-dev LSP setup path.
- The heuristic-eval remediation note — partially superseded: status composition now lives in a pure compose function, not the planned render-time branch; hover/diagnostic fusion and the `.h`→C++ promotion are done.
- The point-in-time clangd evaluation and plugin-testing notes — retain as historical UX/test records, not architecture.
