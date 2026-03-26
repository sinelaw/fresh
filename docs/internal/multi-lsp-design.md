# Multi-LSP Server Support: Design Document

## Status: Research & Design Draft

This document explores supporting an arbitrary number of LSP servers per buffer/language
in Fresh, drawing on research of how other editors approach this problem, and proposing
design alternatives for Fresh's implementation.

---

## 1. Problem Statement

Fresh currently supports **one LSP server per language**. The `LspManager` stores handles in a
`HashMap<String, LspHandle>` keyed by language name. LSP dispatch is correctly tied to the
buffer's language: `with_lsp_for_buffer()` reads `state.language` from the buffer and uses it
to find the corresponding handle, so requests always go to the right server for the file type.
However, the one-handle-per-language constraint means:

- You cannot run both `pyright` (type checking) and `ruff` (linting/formatting) for Python simultaneously.
- You cannot run `typescript-language-server` alongside `eslint-lsp` for TypeScript.
- You cannot have a general-purpose server (e.g., `tailwindcss-language-server`) augment a
  primary server for HTML/JSX files.
- You cannot use a fast formatter server alongside a slower full-featured server.

Many real-world workflows require multiple complementary servers per language:

| Primary Server | Add-on Server | Why |
|---|---|---|
| `rust-analyzer` | `bacon-ls` (diagnostics from `cargo check`) | Faster incremental diagnostics |
| `pyright` | `ruff` | Type checking + linting/formatting |
| `typescript-language-server` | `eslint-lsp` | Language features + linting |
| `vscode-html-language-server` | `tailwindcss-language-server` | HTML + CSS utility classes |
| `gopls` | `golangci-lint-langserver` | Go features + extended linting |

---

## 2. How Other Editors Handle This

### 2.1 Neovim

**Approach: Native multi-client per buffer**

Neovim's built-in LSP client natively supports attaching multiple LSP clients to a single
buffer. Each client is independent and managed separately.

**Request dispatch:**
- `vim.lsp.buf.*` methods (hover, definition, references, etc.) are sent to **all** attached clients.
- Neovim 0.11 redesigned the handler architecture: responses are now handled per-client
  rather than through global handlers, fixing the old "last response wins" race condition
  for hover/signatureHelp.
- For list-based results (references, symbols), `vim.lsp.buf_request_all` collects responses
  from all clients and merges them.
- Completion is typically handled by external plugins (e.g., `nvim-cmp`) which aggregate
  items from all attached clients.

**Conflict resolution:**
- `root_markers` + `workspace_required` (0.11.1+) control which servers activate per project.
- Custom `root_dir` functions allow dynamic per-buffer activation decisions.
- No built-in per-feature routing; users must use workarounds (no-op handlers) to suppress
  specific capabilities from specific servers.

**Configuration:** Per-server configs in `nvim-lspconfig`, per-buffer attachment via autocommands.

**Pain points:**
- No declarative per-feature capability routing (e.g., "use server A for formatting, server B for everything else").
- Conflicting servers (e.g., `ts_ls` + `denols`) require manual activation guards.
- Diagnostics from multiple servers can clutter if not carefully managed.

### 2.2 VS Code

**Approach: Extension-mediated multi-server**

VS Code does not directly expose multi-LSP configuration to users. Instead, each **extension**
can register its own language server(s). Multiple extensions targeting the same language
result in multiple servers running simultaneously.

**Request dispatch:**
- The editor internally dispatches to all registered providers for a given capability.
- Completions, code actions, and diagnostics are **merged** from all providers.
- For single-result features (hover, definition), VS Code uses a priority/ordering system
  among registered providers.

**Conflict resolution:**
- Extension authors are responsible for scoping their activation (via `activationEvents`).
- Users can disable specific extensions per workspace.
- No fine-grained per-feature routing exposed to end users.

**Pain points:**
- "Extension soup" — hard to know which extension provides what.
- Duplicate diagnostics when extensions overlap.
- No user-facing control over which server handles which feature.

### 2.3 Emacs lsp-mode

**Approach: Primary server + add-on servers**

lsp-mode has native multi-server support via the `:add-on?` flag.

**Request dispatch:**
- Without `:add-on?`, only the **highest-priority** server starts for a given mode.
- Servers registered with `:add-on? t` start **in parallel** alongside the primary server.
- Diagnostics and completions are merged from all active servers.
- Formatting triggers all servers that support it (can cause conflicts).

**Configuration:**
- `:priority` controls which non-add-on server wins.
- `lsp-enabled-clients` / `lsp-disabled-clients` per project (via `.dir-locals.el`).

**Pain points:**
- Formatting from multiple servers causes conflicts without manual configuration.
- Completion can get disabled when multiple servers are active (known bug).
- No per-feature routing — add-on servers provide all their capabilities.

### 2.4 Emacs Eglot + Rassumfrassum

**Approach: External multiplexer**

Eglot (Emacs's built-in LSP client) takes a fundamentally different approach: it delegates
multi-server coordination to an **external multiplexer** called `rassumfrassum` (`rass`).

**How it works:**
- `rass` is a standalone process that presents itself as a single LSP server to Eglot.
- Internally, it spawns and manages multiple real LSP servers.
- It routes requests based on server capabilities and merges responses.
- Eglot communicates with one process; the multiplexer handles the complexity.

**Usage:** `C-u M-x eglot RET rass -- server1 --stdio -- server2 --stdio RET`

**Advantages:**
- Editor-agnostic: `rass` works with any LSP client (Neovim, Helix, etc.).
- Merging logic is centralized and can be optimized (threading, caching).
- Keeps the editor client simple.

**Disadvantages:**
- Extra process overhead and potential latency.
- Configuration is external to the editor.
- Debugging is harder (another layer of indirection).
- Uncommon approach — most editors have built-in support.

### 2.5 Helix

**Approach: Declarative per-feature routing (best-in-class)**

Helix has the most sophisticated and ergonomic multi-LSP support among terminal editors.

**Configuration:**
```toml
[language-server.typescript-language-server]
command = "typescript-language-server"
args = ["--stdio"]

[language-server.eslint-lsp]
command = "vscode-eslint-language-server"
args = ["--stdio"]

[[language]]
name = "typescript"
language-servers = [
  { name = "eslint-lsp", only-features = ["diagnostics", "code-action"] },
  "typescript-language-server"
]
```

**Request dispatch:**
- **Priority-ordered**: The first server in the `language-servers` array that supports a
  feature handles it.
- **Merged features**: `diagnostics`, `code-action`, `completion`, `document-symbols`, and
  `workspace-symbols` are collected from **all** servers and merged.
- **Exclusive features**: `hover`, `goto-definition`, `format`, `rename`, `signature-help`,
  etc. use only the first capable server.

**Per-feature routing:**
- `only-features = [...]` — server only provides listed capabilities.
- `except-features = [...]` — server provides everything except listed capabilities.

**Advantages:**
- Declarative, predictable configuration.
- Clear feature ownership.
- Merged results for naturally-mergeable features.
- Shared server instances across languages.

**Pain points:**
- Configuration can be verbose for complex setups.
- No runtime switching between servers for a feature.

### 2.6 Zed

**Approach: Extension-based multi-server with per-server diagnostics**

Zed supports multiple language servers per language through its extension system.

**Request dispatch:**
- Extensions declare language servers with target languages in `extension.toml`.
- Diagnostics are tracked per-server: `Vec<(LanguageServerId, Vec<DiagnosticEntry>)>`.
- `merge_lsp_diagnostics` merges new diagnostics without clobbering other servers' entries.

**Conflict resolution:**
- Users can configure active servers per language in `settings.json`.
- Hard-coded defaults determine the "primary" server for each language.

**Limitations:**
- Multiplexing strategy (how to route non-diagnostic requests across multiple servers) is
  still under active design discussion.
- Community requests for fallback behavior (try server A, if null, try server B).

### 2.7 Sublime Text LSP

**Approach: Multiple clients with `disabled_capabilities`**

Sublime Text's LSP package natively supports multiple active servers per file type.

**Conflict resolution:**
- `disabled_capabilities` setting on individual server configs to suppress specific features.
- Example: Run `LSP-pyright` for completions/navigation and `LSP-ruff` for linting, with
  formatting disabled on one of them.

**Configuration:** Per-server settings in `LSP.sublime-settings` with per-project overrides.

**Scope:** Each server instance is bound to a single Sublime Text window.

### 2.8 Summary Comparison

| Editor | Multi-server | Routing strategy | Merged features | Per-feature control | Configuration |
|--------|-------------|------------------|-----------------|--------------------|----|
| **Neovim** | Native | All clients get all requests | Completions (via plugins), references | No (workarounds only) | Lua/autocommands |
| **VS Code** | Via extensions | Extension-mediated | Completions, diagnostics, code actions | No (extension-controlled) | Extensions + settings |
| **lsp-mode** | Native | Primary + add-ons | Diagnostics, completions | Priority only | Elisp + dir-locals |
| **Eglot** | External mux | Multiplexer-mediated | Depends on mux | Depends on mux | External tool config |
| **Helix** | Native | Priority-ordered + merged | diagnostics, completion, code-action, symbols | `only-features` / `except-features` | TOML declarative |
| **Zed** | Via extensions | Per-extension | Diagnostics | Settings-based | JSON + extension.toml |
| **Sublime** | Native | All active | All features | `disabled_capabilities` | JSON settings |

---

## 3. Design Principles and Goals

Based on the research above and Fresh's existing architecture, we propose the following
principles and goals for multi-LSP support:

### 3.1 Principles

1. **Predictability over magic.** Users should be able to understand which server handles
   which feature by reading their configuration. No hidden heuristics or race conditions.

2. **Declarative configuration.** Feature routing should be configured statically, not
   determined at runtime by arrival order of responses. Helix's `only-features` /
   `except-features` model is the gold standard here.

3. **Sensible defaults with escape hatches.** Common multi-server setups (e.g., primary +
   linter) should work out of the box with minimal configuration. Power users should have
   full control.

4. **Merged where natural, exclusive where necessary.** Features that are naturally
   aggregatable (diagnostics, completions, code actions, symbols) should merge results from
   all servers. Features that produce a single result (hover, definition, formatting, rename)
   should use the highest-priority server.

5. **Backward compatibility.** Existing single-server configurations must continue to work
   unchanged. Multi-server support is additive.

6. **Resource awareness.** Running multiple servers has real cost (memory, CPU, startup time).
   The system should make it easy to keep servers lean (e.g., `only-features` to avoid
   unnecessary work).

7. **Observability.** Users should be able to see which servers are running, which features
   each provides, and which server responded to a given request. This is critical for
   debugging.

### 3.2 Goals

1. **G1: Multiple servers per language.** Support configuring and running N servers for a
   single language, each with its own command, args, and initialization options.

2. **G2: Per-feature routing.** Allow users to declaratively control which server handles
   which LSP features (completions, diagnostics, formatting, hover, etc.).

3. **G3: Response merging.** For mergeable features (diagnostics, completions, code actions,
   symbols), collect and merge responses from all eligible servers.

4. **G4: Priority ordering.** For exclusive features, use a configurable priority order to
   determine which server handles the request.

5. **G5: Independent lifecycle.** Each server should start, stop, crash-recover, and restart
   independently. A crash in the linting server should not affect the primary server.

6. **G6: Shared document sync.** All servers attached to a buffer must receive `didOpen`,
   `didChange`, `didClose`, and `didSave` notifications for that buffer.

7. **G7: Per-server capability tracking.** Track and expose each server's actual capabilities
   (from `InitializeResult`) separately, and intersect with the user's feature routing config.

8. **G8: Status/observability UI.** Show per-server status (running, error, capabilities)
   in the status bar or a dedicated panel.

### 3.3 User Flows

**Flow 1: Basic multi-server setup (primary + linter)**

1. User adds a second server to their `config.json`:
   ```json
   {
     "lsp": {
       "typescript": [
         { "command": "typescript-language-server", "args": ["--stdio"] },
         { "command": "vscode-eslint-language-server", "args": ["--stdio"],
           "only_features": ["diagnostics", "code_action"] }
       ]
     }
   }
   ```
2. User opens a `.ts` file.
3. Both servers start. `typescript-language-server` handles completions, hover, definition, etc.
   `eslint-lsp` provides additional diagnostics and code actions.
4. Diagnostics from both servers appear in the editor, distinguished by source.
5. Code actions from both servers appear in the code action menu.

**Flow 2: Formatter override**

1. User wants to use `prettier` for formatting TypeScript instead of `tsserver`'s formatter:
   ```json
   {
     "lsp": {
       "typescript": [
         { "command": "typescript-language-server", "args": ["--stdio"],
           "except_features": ["format"] },
         { "command": "prettier-lsp", "args": ["--stdio"],
           "only_features": ["format"] }
       ]
     }
   }
   ```
2. When user formats, only `prettier-lsp` is invoked.

**Flow 3: Observability**

1. User opens the command palette (`Ctrl+P > >`) and selects "Show LSP Status".
2. A popup or panel shows:
   ```
   typescript (2 servers):
     typescript-language-server [running] — completions, hover, definition, references, rename
     eslint-lsp [running] — diagnostics, code_action
   ```

**Flow 4: Server crash isolation**

1. `eslint-lsp` crashes.
2. `typescript-language-server` continues working unaffected.
3. Status bar shows `eslint-lsp` in error state.
4. Exponential backoff restart kicks in for `eslint-lsp` only.
5. On restart, `didOpen` is re-sent to `eslint-lsp` for all open TS buffers.

**Flow 5: Per-project override**

1. Project `.fresh/config.json` overrides the global config to add a project-specific server
   or disable one of the global servers for this project.

---

## 4. Design Alternatives

### 4.0 Fresh's Current Configuration System

Fresh uses **JSON** configuration with a 4-level layered resolution (highest to lowest priority):

1. **Session** — `.fresh/session.json` (temporary per-session overrides)
2. **Project** — `.fresh/config.json` (per-project)
3. **User Platform** — `~/.config/fresh/config_linux.json` etc. (OS-specific)
4. **User** — `~/.config/fresh/config.json` (global defaults)

LSP servers are configured in the `"lsp"` key as a `HashMap<String, LspServerConfig>` where
keys are language names. The `LspServerConfig` struct (in `types.rs`) has:
- `command`, `args`, `enabled`, `auto_start`
- `process_limits` (memory/CPU per-server)
- `initialization_options` (passed to LSP `Initialize`)
- `env` (environment variables for the server process)
- `language_id_overrides` (extension → LSP languageId mapping)

The `PartialConfig` system enables layered merging: project configs fill in only the fields
they override; missing fields fall through to user/default config via `merge_with_defaults()`.

**UI surface for LSP commands:**
- **Command palette** (`Ctrl+P` then `>`): "Show LSP Status", "Start/Restart LSP Server",
  "Stop LSP Server", "Toggle LSP for Current Buffer", "Rust LSP: Configure Mode"
- **LSP menu bar**: Go to Definition, Find References, Rename Symbol, Code Actions,
  Show Completions, Show Hover Info, Show Signature Help, Restart/Stop Server, Toggle Inlay Hints
- **Status bar**: Clickable LSP indicator showing server state; click opens LSP status popup
- **Diagnostics**: F8/Shift+F8 navigation, diagnostics panel, inline diagnostics

The `LspManager` stores `handles: HashMap<String, LspHandle>` (one handle per language) and
`config: HashMap<String, LspServerConfig>` (one config per language). This is the primary
structure that changes to support multiple servers.

### 4.1 Configuration Model

#### Option A: Array-of-objects per language (Recommended)

Extend the existing `"lsp"` config key so that a language value can be either a single
`LspServerConfig` object (backward compatible) or an array of named server configs:

```json
{
  "lsp": {
    "rust": { "command": "rust-analyzer", "auto_start": true },

    "typescript": [
      { "name": "tsserver", "command": "typescript-language-server", "args": ["--stdio"],
        "auto_start": true, "except_features": ["format"] },
      { "name": "eslint", "command": "vscode-eslint-language-server", "args": ["--stdio"],
        "auto_start": true, "only_features": ["diagnostics", "code_action"] },
      { "name": "prettier", "command": "prettier-lsp", "args": ["--stdio"],
        "only_features": ["format"] }
    ]
  }
}
```

| Aspect | Assessment |
|---|---|
| Backward compat | Single-object form still works; serde `#[serde(untagged)]` enum |
| Feature routing | `only_features` / `except_features` per server |
| Priority | Array order = priority (first capable server wins for exclusive features) |
| Server identity | `name` field for display/status; defaults to command basename |
| Complexity | Moderate — config schema becomes a union type |

#### Option B: Separate server definitions + per-language references (Helix-style)

Servers defined globally, languages reference them by name:

```json
{
  "lsp_servers": {
    "tsserver": { "command": "typescript-language-server", "args": ["--stdio"] },
    "eslint": { "command": "vscode-eslint-language-server", "args": ["--stdio"] }
  },
  "lsp": {
    "typescript": {
      "servers": [
        "tsserver",
        { "name": "eslint", "only_features": ["diagnostics", "code_action"] }
      ]
    }
  }
}
```

| Aspect | Assessment |
|---|---|
| Backward compat | Breaking change — requires migration |
| Reuse | Servers shared across languages (e.g., `prettier` for JS + TS + CSS) |
| Complexity | Higher — two config sections to coordinate |
| Readability | Better for large configs with many languages sharing servers |

#### Option C: Primary + add-on model (lsp-mode-style)

Keep current single-server config as "primary" and add an `addons` array:

```json
{
  "lsp": {
    "typescript": {
      "command": "typescript-language-server", "args": ["--stdio"],
      "addons": [
        { "command": "vscode-eslint-language-server", "args": ["--stdio"],
          "only_features": ["diagnostics", "code_action"] }
      ]
    }
  }
}
```

| Aspect | Assessment |
|---|---|
| Backward compat | Fully compatible — addons is a new optional field |
| Conceptual model | Clear primary/secondary hierarchy |
| Limitation | No way for an add-on to override primary for a feature (except_features only on primary) |
| Complexity | Low — minimal schema change |

### 4.2 Internal Architecture

#### Option I: Multi-handle HashMap (Recommended)

Change `handles: HashMap<String, LspHandle>` to `handles: HashMap<String, Vec<LspHandle>>`
where the key remains the language name and the vec is ordered by priority.

Each `LspHandle` gains:
- A `name: String` (e.g., "tsserver", "eslint")
- A `feature_filter: FeatureFilter` (only/except features)
- Server capabilities from `InitializeResult` intersected with the feature filter

**Dispatch logic:**
```
fn handles_for_feature(&self, language: &str, feature: LspFeature) -> Vec<&LspHandle>
```
- For merged features (diagnostics, completion, code_action, symbols): return all handles
  whose feature filter allows the feature AND whose server capabilities include it.
- For exclusive features (hover, definition, format, rename, etc.): return the first handle
  matching both the feature filter and server capability.

| Aspect | Assessment |
|---|---|
| Invasiveness | Moderate — touches LspManager, dispatch, config loading |
| Parallelism | Merged features send requests to all handles concurrently |
| Lifecycle | Each handle independent — crash/restart isolation built-in |
| Document sync | `didOpen`/`didChange` sent to all handles for the language |

#### Option II: Compound key HashMap

Change to `handles: HashMap<(String, String), LspHandle>` keyed by `(language, server_name)`.

| Aspect | Assessment |
|---|---|
| Lookup | O(n) scan needed to find all servers for a language |
| Simplicity | Minimal change to handle storage |
| Iteration | Awkward iteration patterns for per-language operations |

#### Option III: External multiplexer support (Rassumfrassum-style)

Instead of changing internal architecture, support configuring an external multiplexer as
the "server" for a language:

```json
{
  "lsp": {
    "python": {
      "command": "rass",
      "args": ["--", "pyright", "--", "ruff", "server"]
    }
  }
}
```

| Aspect | Assessment |
|---|---|
| Invasiveness | Zero — no internal changes |
| Flexibility | Delegates all complexity to the multiplexer |
| Dependency | Requires external tool installation |
| Observability | Opaque — Fresh can't see individual servers |
| User experience | Poor — debugging through two layers of abstraction |

### 4.3 Request Dispatch and Response Merging

#### Feature Classification

Based on research across all editors, features naturally divide into two categories:

**Merged features** (results from all servers concatenated/unioned):
- `textDocument/publishDiagnostics` and pull diagnostics
- `textDocument/completion`
- `textDocument/codeAction`
- `textDocument/documentSymbol`
- `workspace/symbol`

**Exclusive features** (first-priority server wins):
- `textDocument/hover`
- `textDocument/definition` (and declaration, typeDefinition, implementation)
- `textDocument/references`
- `textDocument/formatting` and `textDocument/rangeFormatting`
- `textDocument/rename` (and `prepareRename`)
- `textDocument/signatureHelp`
- `textDocument/inlayHint`
- `textDocument/foldingRange`
- `textDocument/semanticTokens/*`
- `textDocument/documentHighlight`

#### Merging strategies for merged features

**Diagnostics:**
- Each server's diagnostics are tracked separately (keyed by server name).
- `publishDiagnostics` from server A never clears server B's diagnostics.
- Display shows unified list with optional server-name annotation.
- Diagnostics panel can optionally filter by server.

**Completions:**
- Send completion request to all eligible servers concurrently.
- First response populates the menu immediately.
- Subsequent responses extend the menu (Helix pattern: "first future creates, rest extend").
- Items tagged with server name for disambiguation if needed.
- Debounce: wait a short window (e.g., 50ms) for fast servers before showing.

**Code Actions:**
- Send to all eligible servers concurrently.
- Merge into a single list, grouped or tagged by server name.
- Apply workspace edits from the selected action's originating server only.

#### Exclusive feature dispatch

For exclusive features, the dispatch helper becomes:

```rust
fn with_lsp_for_buffer(
    &mut self,
    buffer_id: BufferId,
    feature: LspFeature,
    f: impl FnOnce(&LspHandle, &Uri, &str) -> R,
) -> Option<R>
```

This iterates the priority-ordered handles for the buffer's language, finds the first that
(a) has the feature in its filter, (b) has the capability from the server, and (c) is in
`Running` state.

#### Fallback behavior

For exclusive features, if the primary server returns `null`/empty, there is a design choice:

1. **No fallback** (Helix default): the first eligible server's answer is final, even if empty.
2. **Fallback on null**: if the primary returns null, try the next server. This adds latency
   but increases the chance of a useful result.

Recommendation: start with no fallback (simpler), add opt-in fallback later.

### 4.4 Document Synchronization

All servers for a language must receive document lifecycle notifications:

- `textDocument/didOpen`: sent to all servers when a buffer is opened or a new server starts.
- `textDocument/didChange`: sent to all servers on every edit.
- `textDocument/didClose`: sent to all servers when a buffer is closed.
- `textDocument/didSave`: sent to all servers when a buffer is saved.

**Key change:** `BufferMetadata.lsp_opened_with: HashSet<u64>` already tracks which server
instance IDs have received `didOpen`. This naturally extends to multiple servers — each
server has its own handle ID, and `didOpen` is sent independently per handle.

**Document version:** Each server independently tracks document versions. Since Fresh sends
the same edits to all servers in the same order, versions stay synchronized.

**Risk:** If one server is slow to process `didChange`, it may have a stale view when
receiving a request. This is inherent to the LSP protocol and is the server's responsibility
to handle (the protocol includes version numbers for this reason).

### 4.5 `workspace/applyEdit` Handling

When a server sends `workspace/applyEdit` (e.g., from a code action or rename):

1. **Sequential application**: edits are applied as they arrive, first-come-first-served.
2. **Version checking**: if the edit targets a specific document version and the document has
   changed since, reject with `applied: false`.
3. **Post-edit sync**: after applying an edit from server A, `didChange` is sent to all
   servers (including server A, per protocol spec).
4. **No cross-server merging**: edits from different servers are never merged. Each edit is
   from a single server and applied atomically.

### 4.6 Server Lifecycle and Resource Management

#### Independent lifecycle (Recommended)

Each server handle manages its own:
- Process spawning and stdio/JSON-RPC communication
- Initialization handshake
- Crash detection and exponential backoff restart
- Shutdown sequence

This is already the architecture of `LspHandle`/`LspTask` — the change is simply having
multiple handles per language instead of one.

#### Resource concerns

- **Memory**: Each server is a separate process. Two servers per language roughly doubles
  memory for LSP. Mitigation: `process_limits` already exist per-server.
- **CPU**: Sending `didChange` to N servers means N servers parsing on every keystroke.
  Mitigation: servers that only need diagnostics can use `TextDocumentSyncKind::Full` with
  debounced saves rather than incremental sync.
- **Startup time**: N servers means N initialization handshakes. Mitigation: `auto_start`
  controls which servers start eagerly vs. lazily.

### 4.7 Observability

#### Status bar: filter to active buffer's language

Today `update_lsp_status_from_server_statuses()` shows all running servers across all
languages (e.g., `LSP [python: ready, rust: ready, typescript: ready]`). This is already
noisy and would be worse with multi-server (e.g., 3 languages × 2 servers = 6 entries).

**Recommendation (independent of multi-LSP):** Filter the status bar to show only the
server(s) relevant to the active buffer's language. The active buffer's language is already
available via `self.buffers.get(&self.active_buffer()).map(|s| &s.language)`. This makes
the status bar contextual and directly actionable — what you see is what affects the file
you're editing.

With multi-LSP this becomes: `LSP [pyright: ready, ruff: ready]` when editing a Python file,
rather than listing every server across every language.

#### Option: Enhanced "Show LSP Status" command

Extend the existing LSP status display (accessible via command palette or status bar click)
to show per-server information:

```
Language: typescript (2 servers)
  ┌─ tsserver [Running]
  │  Command: typescript-language-server --stdio
  │  Features: completion, hover, definition, references, rename, signature-help, inlay-hints
  │  PID: 12345, Memory: 120MB
  │
  └─ eslint [Running]
     Command: vscode-eslint-language-server --stdio
     Features: diagnostics, code-action (only_features filter)
     PID: 12346, Memory: 45MB
```

#### Diagnostics attribution

Each diagnostic in the diagnostics panel could optionally show its source server:

```
error[tsserver]: Type 'string' is not assignable to type 'number'  src/foo.ts:10:5
warning[eslint]: Unexpected console statement (no-console)         src/foo.ts:15:3
```

### 4.8 Per-Language Workspace Root Detection

#### Problem

`LspManager` currently gets its `root_uri` from `cwd` at startup. When a user runs
`fresh ~/.config/wezterm/wezterm.lua` from `$HOME`, the workspace root becomes `$HOME`,
and servers like LuaLS correctly refuse to scan it. This is the only editor that uses
cwd-based root detection — every other editor walks upward from the file looking for
language-specific root markers.

#### Design

Add a `root_markers` field to `LspServerConfig`:

```rust
pub struct LspServerConfig {
    // ... existing fields ...

    /// File/directory names to search for when detecting the project root.
    /// The editor walks upward from the opened file's directory looking for
    /// any of these markers. The first directory containing a match becomes
    /// the workspace root sent to the LSP server.
    ///
    /// If empty, falls back to the file's parent directory.
    /// If the walk reaches a filesystem boundary without a match, uses the
    /// file's parent directory (never cwd or $HOME).
    #[serde(default)]
    pub root_markers: Vec<String>,
}
```

#### Root resolution algorithm

```
detect_workspace_root(file_path, root_markers) -> PathBuf:
    dir = file_path.parent()
    while dir is not None:
        for marker in root_markers:
            if dir.join(marker).exists():
                return dir
        dir = dir.parent()
    return file_path.parent()   // fallback: file's directory
```

#### Resolution priority

1. If `per_language_root_uris` has an entry (plugin-set, e.g. C# plugin) → use it
2. Else if `config.root_markers` is non-empty → walk upward from `file_path`
3. Else → walk upward from `file_path` using generic markers `[".git"]`
4. Final fallback → file's parent directory

This requires `force_spawn` (or its caller) to know which file triggered the spawn.
Currently `force_spawn` takes only `language: &str` — add an optional `file_path` parameter.

#### Sensible defaults per language

```json
{
  "lua": { "root_markers": [".luarc.json", ".luarc.jsonc", ".luacheckrc", ".stylua.toml", ".git"] },
  "rust": { "root_markers": ["Cargo.toml", "rust-project.json", ".git"] },
  "python": { "root_markers": ["pyproject.toml", "setup.py", "setup.cfg", "pyrightconfig.json", ".git"] },
  "javascript": { "root_markers": ["tsconfig.json", "jsconfig.json", "package.json", ".git"] },
  "typescript": { "root_markers": ["tsconfig.json", "jsconfig.json", "package.json", ".git"] },
  "go": { "root_markers": ["go.mod", "go.work", ".git"] },
  "c": { "root_markers": ["compile_commands.json", "CMakeLists.txt", "Makefile", ".git"] },
  "cpp": { "root_markers": ["compile_commands.json", "CMakeLists.txt", "Makefile", ".git"] }
}
```

Languages without explicit `root_markers` get `[".git"]` as a universal fallback, with the
file's parent directory as the final fallback (matching Helix/Neovim behavior).

#### Files changed

| File | Change |
|---|---|
| `types.rs` | Add `root_markers: Vec<String>` to `LspServerConfig`, update `merge_with_defaults` |
| `config.rs` | Add default `root_markers` for each language in `populate_lsp_config` |
| `services/lsp/manager.rs` | Add `detect_root_from_file(file_path, markers) -> PathBuf`. Change `force_spawn` to accept optional file path, use new root detection instead of `get_effective_root_uri` |
| `app/file_operations.rs` | Pass the file path through when calling `try_spawn` / `force_spawn` |

#### Backward compatibility

- `per_language_root_uris` (plugin-set roots) still take priority — no breakage for C# plugin etc.
- The global `root_uri` from cwd becomes the last resort fallback (after markers and file-parent),
  or could be removed entirely since it's never the right answer when markers exist.
- `root_markers: []` in config → file's parent directory (still better than cwd).

#### User config example

```json
{
  "lsp": {
    "lua": {
      "command": "lua-language-server",
      "root_markers": [".luarc.json", ".git"]
    }
  }
}
```

Or `"root_markers": []` to force file-directory-only behavior (no upward walk).

#### Interaction with multi-LSP

Since `root_markers` is a field on `LspServerConfig`, it naturally becomes per-server in the
array config form. Different servers for the same language can have different workspace roots:

```json
{
  "lsp": {
    "typescript": [
      { "name": "tsserver", "command": "typescript-language-server", "args": ["--stdio"],
        "root_markers": ["tsconfig.json", "package.json", ".git"] },
      { "name": "tailwind", "command": "tailwindcss-language-server", "args": ["--stdio"],
        "only_features": ["completions"],
        "root_markers": ["tailwind.config.js", "tailwind.config.ts", ".git"] }
    ]
  }
}
```

Each server's root is resolved independently using its own markers. This matters for monorepo
setups where a linter might need the monorepo root (where the config lives) while the type
checker needs the package root (where `tsconfig.json` lives).

---

## 5. Recommendations and Testing Plan

### 5.1 Recommended Design

Based on the research and analysis above, we recommend:

| Decision | Choice | Rationale |
|---|---|---|
| **Config model** | Option A: Array-of-objects | Best balance of backward compat, simplicity, and expressiveness |
| **Internal arch** | Option I: Multi-handle Vec | Natural extension of existing architecture, clean dispatch |
| **Feature routing** | `only_features` / `except_features` | Proven in Helix, declarative, predictable |
| **Merged features** | diagnostics, completion, code_action, document_symbols, workspace_symbols | Consensus across all editors studied |
| **Exclusive dispatch** | Priority-ordered, no fallback (initially) | Simpler, predictable; fallback can be added later |
| **Diagnostics** | Per-server tracking with merged display | Prevents clobbering, enables attribution |
| **Observability** | Enhanced "Show LSP Status" + diagnostic attribution | Essential for debugging multi-server setups |
| **Workspace root** | Per-language `root_markers` with upward walk | Fixes cwd-based root; matches every other editor |

### 5.2 Implementation Phases

**Phase 0: Per-language workspace root detection (independent of multi-LSP)**
- Add `root_markers: Vec<String>` to `LspServerConfig` in `types.rs`.
- Update `merge_with_defaults` to merge `root_markers` (non-empty overrides default).
- Add default `root_markers` per language in `populate_lsp_config` (see section 4.8).
- Implement `detect_root_from_file(file_path, markers) -> PathBuf` in `manager.rs`.
- Change `force_spawn` to accept optional file path for root detection.
- Update callers in `file_operations.rs` to pass file path through.
- Resolution priority: plugin-set root > marker walk > `[".git"]` walk > file's parent dir.

**Phase 1: Core multi-handle infrastructure**
- Extend `LspServerConfig` with `name`, `only_features`, `except_features` fields.
- Add `LspFeature` enum listing all routable features.
- Add `FeatureFilter` type implementing the only/except logic.
- Change config deserialization to accept `LspServerConfig | Vec<LspServerConfig>`
  (via `#[serde(untagged)]` enum in `PartialConfig`; update `merge_hashmap_recursive` in
  `partial_config.rs` to handle vec-valued entries).
- Change `LspManager.handles` to `HashMap<String, Vec<LspHandle>>`.
- Change `LspManager.config` to `HashMap<String, Vec<LspServerConfig>>`.
- Update `try_spawn` / `force_spawn` to manage multiple handles per language.
- Update `didOpen`/`didChange`/`didClose`/`didSave` to broadcast to all handles.

**Phase 2: Dispatch routing**
- Implement `handles_for_feature(language, feature)` → `Vec<&LspHandle>` / `Option<&LspHandle>`.
- Refactor `with_lsp_for_buffer` into `with_lsp_for_buffer` (exclusive) and
  `with_all_lsp_for_buffer_feature` (merged).
- Update all request dispatch methods in `lsp_requests.rs` to use new dispatch helpers.
- For merged features: send requests concurrently, collect and merge responses.
- For exclusive features: send to first matching handle only.

**Phase 3: Diagnostics per-server tracking**
- Change diagnostic storage to track `(server_name, diagnostics)` pairs.
- Update `publishDiagnostics` handler to replace only the originating server's diagnostics.
- Update `diagnostics.rs` overlay application to merge all servers' diagnostics.
- Add optional server attribution to diagnostic display.

**Phase 4: Completion merging**
- Send completion requests to all eligible servers.
- First response populates the completion menu.
- Subsequent responses extend the menu without resetting selection.
- Tag completion items with source server for disambiguation.

**Phase 5: Observability**
- Update "Show LSP Status" (command palette + status bar click) to show per-server details.
- Update status bar LSP indicator to show multi-server state.
- Update "Start/Restart LSP Server" to present a server picker when multiple servers exist.
- Update "Stop LSP Server" to list individual servers (already shows a list; extend for multi-server).
- Add server name to LSP menu bar entries where applicable.

### 5.3 Testing Plan

#### Unit tests

1. **Config deserialization:**
   - Single object config (backward compat) deserializes correctly.
   - Array config deserializes with names, features, and priority order.
   - `only_features` and `except_features` are mutually exclusive (validation error if both).
   - Missing `name` defaults to command basename.
   - Empty array is a validation error.

2. **Feature filter:**
   - `FeatureFilter::All` allows all features.
   - `FeatureFilter::Only(set)` allows only listed features.
   - `FeatureFilter::Except(set)` allows all except listed features.
   - Intersection with server capabilities: filter allows it AND server supports it.

3. **Dispatch routing:**
   - `handles_for_feature` returns correct handles for merged features (all eligible).
   - `handles_for_feature` returns first eligible handle for exclusive features.
   - Handles with non-Running state are skipped.
   - Handles without the server capability are skipped even if filter allows.

4. **Diagnostics merging:**
   - Server A's diagnostics update doesn't clear server B's diagnostics.
   - Server A sending empty diagnostics clears only server A's diagnostics.
   - Merged display contains diagnostics from both servers.
   - After server A crashes and restarts, its stale diagnostics are cleared.

5. **Document sync:**
   - `didOpen` sent to all handles when buffer opens.
   - `didOpen` re-sent to a restarted handle (new handle ID).
   - `didChange` sent to all handles on edit.
   - `didClose` sent to all handles when buffer closes.
   - `didSave` sent to all handles when buffer saves.

6. **Workspace root detection:**
   - `detect_root_from_file` finds marker in parent dir → returns parent dir.
   - `detect_root_from_file` finds marker two levels up → returns grandparent dir.
   - `detect_root_from_file` with no marker found → returns file's parent dir.
   - `detect_root_from_file` with empty markers list → returns file's parent dir.
   - Plugin-set `per_language_root_uris` takes priority over marker walk.
   - Different servers for same language can resolve different roots via different markers.
   - Never returns `$HOME` or filesystem root as workspace root.

#### Integration / E2E tests

6. **Two-server lifecycle:**
   - Start two mock LSP servers for the same language.
   - Verify both initialize successfully.
   - Crash one server; verify the other continues working.
   - Verify crashed server restarts independently.

7. **Merged diagnostics E2E:**
   - Mock server A publishes diagnostics `[d1, d2]`.
   - Mock server B publishes diagnostics `[d3, d4]`.
   - Verify all four diagnostics appear in the buffer.
   - Server A publishes updated diagnostics `[d1']`.
   - Verify diagnostics are `[d1', d3, d4]` (server A's updated, server B's unchanged).

8. **Exclusive feature routing E2E:**
   - Server A configured with `except_features: ["format"]`.
   - Server B configured with `only_features: ["format"]`.
   - Trigger format → verify request goes to server B only.
   - Trigger hover → verify request goes to server A only.

9. **Completion merging E2E:**
   - Server A returns completions `[c1, c2]`.
   - Server B returns completions `[c3, c4]`.
   - Verify completion menu contains all four items.

10. **Backward compatibility E2E:**
    - Existing single-server config continues to work without any changes.
    - Single-server config with new fields (`name`, feature filters) works.

11. **Workspace root detection E2E:**
    - Open a file in a directory with `Cargo.toml` two levels up — verify LSP receives the
      `Cargo.toml` directory as `rootUri` in `Initialize`.
    - Open a file from `$HOME` with no markers — verify LSP receives the file's parent dir,
      not `$HOME`.
    - Two servers for the same language with different `root_markers` — verify each server
      receives a different `rootUri`.

#### Performance tests

12. **Latency impact:**
    - Measure completion latency with 1 vs. 2 vs. 3 servers.
    - Ensure merged completion shows first results within 100ms.

13. **Memory overhead:**
    - Measure memory with 1 vs. 2 servers for the same language.
    - Verify `process_limits` are respected per-server.

14. **Edit throughput:**
    - Measure `didChange` broadcast overhead with 1 vs. 3 servers.
    - Ensure no perceivable editor lag during rapid typing.

### 5.4 Open Questions

1. **Should `references` be a merged feature?** Helix treats it as exclusive, but merging
   references from multiple servers could be useful (e.g., one server finds TypeScript
   references, another finds CSS class usage). Risk: duplicates and confusion.

2. **Should we support per-buffer server selection?** E.g., a `.tsx` file might want different
   servers than a `.ts` file, even though both are "typescript". The existing
   `language_id_overrides` partially addresses this.

3. **How should `workspace/applyEdit` from add-on servers work?** If an eslint code action
   wants to apply a fix, it sends `workspace/applyEdit`. This should work fine as long as
   edits are applied atomically and `didChange` is broadcast afterward.

4. **Should we support the external multiplexer approach in addition to native multi-server?**
   Users could always configure `rass` as their server command today. No changes needed, but
   we could document it as an alternative.

5. **How should "Start/Restart LSP Server" work with multiple servers?** Options: restart all
   servers for the language, present a picker listing individual servers by name, or add
   separate "Restart All LSP Servers" command. The existing "Stop LSP Server" already shows
   a selection list, so extending this pattern to restart is natural.

---

## Appendix: References

- [Neovim LSP multi-client support (Issue #12755)](https://github.com/neovim/neovim/issues/12755)
- [Neovim merge LSP results (Issue #17712)](https://github.com/neovim/neovim/issues/17712)
- [Neovim 0.11 multi-client changes](https://gpanders.com/blog/whats-new-in-neovim-0-11/)
- [Reconciling conflicting LSP servers in Neovim 0.11+](https://pawelgrzybek.com/reconcile-two-conflicting-lsp-servers-in-neovim-0-11/)
- [Helix multi-LSP PR #2507](https://github.com/helix-editor/helix/pull/2507)
- [Helix language server docs](https://docs.helix-editor.com/languages.html)
- [Eglot multi-server discussion #1429](https://github.com/joaotavora/eglot/discussions/1429)
- [Rassumfrassum (LSP multiplexer)](https://github.com/joaotavora/rassumfrassum)
- [Eglot + rassumfrassum blog post](https://www.rahuljuliato.com/posts/eglot-rassumfrassum)
- [lsp-mode multi-server PR #469](https://github.com/emacs-lsp/lsp-mode/pull/469)
- [lsp-mode multi-server issue #424](https://github.com/emacs-lsp/lsp-mode/issues/424)
- [Zed multi-server discussion #24100](https://github.com/zed-industries/zed/discussions/24100)
- [Zed language server integration (DeepWiki)](https://deepwiki.com/zed-industries/zed/5.2-language-server-integration)
- [Sublime Text LSP client configuration](https://lsp.sublimetext.io/client_configuration/)
- [VS Code Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide)
- [LSP spec: the good, the bad, and the ugly](https://www.michaelpj.com/blog/2024/09/03/lsp-good-bad-ugly.html)
- [LSP spec 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [VS Code multiple LSP issue #199520](https://github.com/microsoft/vscode/issues/199520)
- [Kakoune-lsp multi-server issue #17](https://github.com/kakoune-lsp/kakoune-lsp/issues/17)
