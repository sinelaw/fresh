# LSP Feature Parity Plan

## Context

Several LSP features are missing or incomplete compared to VS Code. This document tracks
the gaps and implementation plan, starting with the code action gaps that are causing
real user-visible bugs, then covering broader feature parity.

## Phase 1: Code Action Gaps (blocking real workflows)

### 0. Fix apply_workspace_edit: version checking + resource operations

**Current bugs:**
- `TextDocumentEdit.text_document.version` is completely ignored. Stale edits can corrupt buffers.
- `DocumentChanges::Operations` filters for `Edit` only — `CreateFile`, `RenameFile`, `DeleteFile` are silently dropped.
- Change annotations (`needsConfirmation`) are unwrapped to plain `TextEdit`.

**Fix:**
- Version checking: if `version` is `Some(v)`, compare against `document_versions` in the LSP manager. Reject on mismatch. If `None`, apply blind (spec says null = "version unknown").
- Resource operations: handle `CreateFile` (respect `overwrite`/`ignoreIfExists`), `RenameFile` (rename on disk, update open buffers), `DeleteFile` (respect `recursive`/`ignoreIfNotExists`). Apply in order alongside `Edit` operations.

**Files:** `crates/fresh-editor/src/app/lsp_requests.rs` (~line 1579)

### 1. Handle `workspace/applyEdit` (server→client request)

Currently falls through to the default handler which returns null — edits are silently dropped.

**Fix:** Add case in server request dispatch (`async_handler.rs` ~line 3197). Parse `ApplyWorkspaceEditParams`, send `AsyncMessage::LspApplyEdit` to main loop, respond with `{ applied: true }`.

**Edge cases:**
- Nested request deadlock: NOT an issue — the reader task processes server requests independently and writes responses directly to stdin, so `executeCommand` await doesn't block `applyEdit` handling.
- Files not open: `apply_workspace_edit` already handles this via `open_file()`.
- Undo: all edits in one `applyEdit` = one undo step. Use `label` field from params.

**Files:** `async_handler.rs`, `async_bridge.rs`, `mod.rs`

### 2. Add `workspace/executeCommand` (client→server request)

Code actions with a `command` field (but no `edit`) currently log a warning and do nothing.

**Fix:** New `LspCommand::ExecuteCommand` variant, sequential handler that sends `"workspace/executeCommand"` request. Response is usually null — the real effect comes via `workspace/applyEdit` sent during processing (handled by step 1).

**Edge cases:**
- Multiple `applyEdit`s from a single command: arrive as separate server requests, serialized by reader task.
- ContentModified (-32801): don't show to user.

**Files:** `async_handler.rs`, `lsp_requests.rs`

### 3. Add `codeAction/resolve` (client→server request)

Servers can return lightweight code actions without `edit`; the client must resolve before execution. Currently these actions silently do nothing.

**Fix:**
- Track `resolveProvider` in `ServerCapabilitySummary` (`manager.rs`).
- New `LspCommand::CodeActionResolve` variant, handler sends `"codeAction/resolve"`, returns resolved `CodeAction` via `AsyncMessage::LspCodeActionResolved`.
- Only resolve when needed: no `edit`, no `command`, has `data` field, server supports resolve.
- Cancel on popup dismiss: ignore stale responses via request ID matching.

**Edge cases:**
- Staleness: actions computed at version N, user may have edited. Server should cope, but only resolve if needed (avoid Zed #24375 bug of resolving already-complete actions).

**Files:** `async_handler.rs`, `async_bridge.rs`, `manager.rs`

### 4. Fix `execute_code_action` to use all three

Rewrite the dispatch in `lsp_requests.rs` (~line 1301):
- `CodeAction` with no edit/command but has `data` + server supports resolve → send `codeAction/resolve`
- `CodeAction` with `edit` → apply workspace edit
- `CodeAction` with `command` → send `workspace/executeCommand`
- `CodeAction` with both `edit` and `command` → apply edit THEN execute command
- `Command` → send `workspace/executeCommand`

Handle `LspCodeActionResolved` in `mod.rs` to execute the resolved action.

### 5. E2E tests

New file: `tests/e2e/lsp_code_action_resolve_and_commands.rs`

Custom fake LSP server with logging:
1. **`test_code_action_with_command_sends_execute_and_applies_edit`** — command-only action → `executeCommand` → server sends `applyEdit` → verify edit applied
2. **`test_code_action_resolve_then_apply`** — resolve-needed action → `codeAction/resolve` → server fills edit → verify applied
3. **`test_code_action_with_edit_and_command`** — both edit + command → verify edit applied AND `executeCommand` sent

## Phase 2: Completion and Formatting (high impact)

### 6. `completionItem/resolve`

Servers return minimal completion items; the client must resolve for full documentation, additional text edits, etc. Currently the editor shows whatever the server returns in the initial response.

**Impact:** Missing documentation in completion popups, missing auto-imports on completion accept.

### 7. `textDocument/formatting` / `textDocument/rangeFormatting`

Format document / format selection. Capabilities are advertised but no request handler exists.

**Impact:** Format-on-save, format selection — core editing features.

### 8. `textDocument/prepareRename`

Validates rename before showing the rename UI. Advertised via `prepare_support: Some(true)` but no handler.

**Impact:** Renaming may fail with unhelpful errors instead of pre-validating.

## Phase 3: Navigation and Symbols (medium impact)

### 9. `textDocument/documentSymbol`

Get all symbols in a document (for outline view, breadcrumbs, go-to-symbol).

### 10. `workspace/symbol`

Search for symbols across the workspace.

### 11. `textDocument/documentHighlight`

Highlight all references to the symbol under cursor.

### 12. `window/showMessageRequest`

Server asks client to show a message with action buttons. Currently falls through to plugin handler which returns null. Some servers use this for user confirmations during refactoring.

### 13. `window/showDocument`

Server asks client to open/reveal a document. Used by some servers to show generated files or documentation.

## Phase 4: Advanced Features (lower priority)

### 14. `textDocument/onTypeFormatting`

Format as user types (e.g., auto-indent after `{`).

### 15. `textDocument/linkedEditingRange`

Linked editing (e.g., rename HTML open/close tags simultaneously).

### 16. `textDocument/selectionRange`

Smart expand/shrink selection based on AST.

### 17. `workspace/didChangeWatchedFiles`

Notify server when watched files change on disk.

### 18. File operation events (`workspace/will{Create,Rename,Delete}Files`)

Server participates in file operations (e.g., updating imports when files move).

## Race Conditions & Edge Cases Reference

### workspace/applyEdit
- Version checking: `TextDocumentEdit.version` non-null → must match, null → apply blind. Legacy `changes` field has NO version info.
- Edit ordering: edits within a single `TextDocumentEdit` must not overlap, all ranges refer to original document.
- Resource operations must execute in order (create file before editing it).
- Return `applied: false` on version mismatch, invalid ranges, file not found.

### workspace/executeCommand
- Server may send `workspace/applyEdit` BACK during command execution (nested request). Our architecture handles this — reader task processes independently.
- Response is usually null. Real effect via `applyEdit`.

### codeAction/resolve
- Only resolve when needed: no edit + no command + has data + server supports resolve.
- Don't modify the CodeAction object before sending it back.
- Ignore stale responses if user dismissed popup.

### General
- `stdin_writer` contention: tokio::Mutex prevents corruption. Sequential request model means no interleaving.
- `process_async_messages` drains all messages synchronously before user input — workspace edits from `applyEdit` don't race with typing within a single tick.
