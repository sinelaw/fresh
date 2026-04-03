# VS Code Extension API vs Fresh Plugin API: Gap Analysis

Comprehensive comparison of what VS Code extensions actually use (based on AST
analysis of 12 top extensions) versus what Fresh's plugin API currently provides.

---

## Executive Summary

Of the 12 most popular VS Code extensions analyzed, **Fresh can already support
the core features of 5-6 of them** with its current plugin API. Another 3-4 would
need moderate API additions. Only 2-3 require major new subsystems (debug adapter
protocol, webview panels).

The biggest systemic gaps are:
1. **No language provider registration** - Fresh delegates everything to LSP servers,
   but extensions like ESLint, Prettier, and C/C++ need to register their own
   completion, hover, formatting, and diagnostic providers alongside or instead of LSP.
2. **Limited user notification model** - Fresh has `setStatus()` (one line) but no
   modal messages, output channels, or multiple status bar items.
3. **No file/folder picker dialogs** - needed by 5 extensions.
4. **Missing file watcher** - 6 extensions watch for external file changes.

However, Fresh has **unique strengths** that VS Code lacks: virtual buffers with
custom modes, composite side-by-side views, view transforms, streaming grep,
overlay/conceal system, and direct mouse event handling.

---

## Extension Features: What Users Actually Get

### Per-extension feature analysis

| Extension | What It Does For Users | Could Fresh Plugin Do This? |
|-----------|----------------------|----------------------------|
| **Code Runner** | Run code files in 40+ languages via terminal/output | **Yes** - Fresh has `spawnProcess()`, `createTerminal()`, `getConfiguration()`, `registerCommand()`. All needed APIs exist. |
| **Material Icon Theme** | Custom file/folder icons in explorer sidebar | **Mostly** - Fresh has `setFileExplorerDecorations()`. Would need icon theme registration in `contributes`. |
| **Path Intellisense** | Autocomplete file paths while typing | **No** - Needs `languages.registerCompletionItemProvider()` to inject completions into the editor. Fresh only has prompt-level suggestions, not in-buffer completions from plugins. |
| **Auto Rename Tag** | When you rename an HTML tag, renames the matching closing tag | **Yes** - Fresh has `on('after_insert')`, `getBufferText()`, `insertText()`, `deleteRange()`. Pure text manipulation. |
| **Live Server** | Launch local dev server with live reload for HTML | **Partial** - Fresh has `spawnProcess()` but no `http.createServer` or `env.openExternal()` to open browser. Could work if plugin bundles its own server and user opens browser manually. |
| **ESLint** | Show lint errors inline, auto-fix on save | **Partial** - Fresh has LSP integration which handles diagnostics. But ESLint extension also needs `languages.registerCodeActionsProvider()` for quick fixes, `tasks.registerTaskProvider()` for lint tasks, and `createFileSystemWatcher()` to watch config files. |
| **Prettier** | Auto-format code on save/command | **Partial** - Fresh has LSP formatting. But Prettier extension needs `languages.registerDocumentFormattingEditProvider()` for direct formatting, `createFileSystemWatcher()` for config, and `workspace.applyEdit()` for applying changes. |
| **REST Client** | Send HTTP requests from `.http` files, show responses | **Partial** - Fresh has process spawning (could shell out to curl). But full experience needs `fetch`/HTTP client, `createWebviewPanel()` for rich response display, `languages.registerCompletionItemProvider()` for HTTP header completion, `languages.registerCodeLensProvider()` for "Send Request" buttons. |
| **Docker** | Manage Docker containers/images from sidebar | **No** - Ships without source (closed binary). Would need tree views, webview panels, and deep Docker daemon integration. |
| **GitLens** | Rich git blame, history, diff annotations inline | **Partial** - Fresh has overlays, virtual text, decorations (many of the visual primitives). But needs `authentication.getSession()` for GitHub, `window.createTreeView()` for sidebar, `window.createWebviewPanel()` for commit graph, `workspace.registerFileSystemProvider()` for virtual git content. |
| **Python** | Full Python IDE: IntelliSense, debugging, notebooks, testing | **Partial** - Fresh has LSP for language features. But Python extension also needs `debug.*` API (debugging), `notebooks.*` (Jupyter), `tasks.*` (run configurations), `languages.register*()` for non-LSP providers. Heavy extension. |
| **C/C++** | Full C/C++ IDE: IntelliSense, debugging, formatting | **Partial** - Same story as Python. LSP covers basics, but needs `debug.*`, `tasks.*`, `languages.register*()` for full feature set. Heaviest type user (128 types). |

### Feature viability summary

| Viability | Extensions | What's Needed |
|-----------|------------|---------------|
| **Ready now** | Code Runner, Auto Rename Tag | Nothing - current API is sufficient |
| **Minor gaps** | Material Icon Theme, Live Server | Icon theme contributes; `env.openExternal()` |
| **Moderate gaps** | ESLint, Prettier | File watcher, language provider registration, config change events |
| **Major gaps** | GitLens, REST Client | Tree views, webviews, HTTP client, authentication |
| **Not feasible** | Python, C/C++, Docker | Debug protocol, notebooks, task system, extensive language providers |

---

## Gap Summary

| Category | Fresh Has | Partial | Missing | N/A |
|----------|-----------|---------|---------|-----|
| Workspace/Configuration | 7 | 4 | 3 | 2 |
| Window/UI | 10 | 5 | 9 | 0 |
| Commands | 3 | 0 | 1 | 0 |
| Extensions | 1 | 1 | 1 | 0 |
| Languages | 2 | 1 | 11 | 0 |
| Environment | 2 | 2 | 3 | 3 |
| Uri | 2 | 1 | 2 | 0 |
| Debug | 0 | 0 | 5 | 0 |
| Tasks | 0 | 1 | 1 | 0 |
| Core Types | 5 | 7 | 5 | 0 |
| Node.js Builtins | 14 | 2 | 10 | 0 |
| Process/Environment | 2 | 1 | 1 | 2 |
| Globals | 2 | 0 | 3 | 0 |
| **TOTAL** | **50** | **25** | **55** | **7** |

---

## Feature-Driven Priority: What to Implement

### Essential (unlocks real user features)

These API gaps directly block user-facing features that popular extensions provide:

| Gap | Impact | Unlocks |
|-----|--------|---------|
| **`workspace.createFileSystemWatcher()`** | Config/file change detection | ESLint, Prettier, C/C++ auto-reload on config change |
| **`workspace.onDidChangeConfiguration`** | React to settings changes | Every extension that has settings (8/11) |
| **`env.openExternal()`** | Open URLs in default browser | Live Server (its entire purpose), GitLens PR links |
| **`globalThis.fetch`** / HTTP client | Make HTTP requests | REST Client (its entire purpose), GitLens GitHub API, extension update checks |
| **`languages.registerCompletionItemProvider()`** | Plugin-provided autocomplete | Path Intellisense (its entire purpose), REST Client headers |
| **`languages.registerDocumentFormattingEditProvider()`** | Plugin-provided formatting | Prettier (its core feature when not using LSP) |
| **`languages.registerCodeActionsProvider()`** | Quick fix actions | ESLint auto-fix, C/C++ quick fixes |
| **`languages.createDiagnosticCollection()`** | Plugin-created error markers | ESLint (when not using LSP mode), custom linters |
| **`path.resolve` / `path.normalize` / `path.sep`** | Path manipulation | Used by 4-6 extensions for cross-platform path handling |
| **`crypto.createHash` / `crypto.randomBytes`** | Hashing and random data | Cache keys, temp file names, telemetry IDs (5 extensions) |

### Nice to Have (improves polish but not essential for core feature)

These are used by extensions but are not critical to their primary user-facing feature:

| Gap | Why It's Not Essential |
|-----|----------------------|
| **`Uri` class** | Fresh uses string paths; works fine for local files. Uri objects mainly matter for remote/virtual filesystems. |
| **`Disposable` pattern** | Internal cleanup pattern. Plugins can use `editor.off()` manually. No user impact. |
| **Multiple `StatusBarItem`s** | Nice for showing extension state, but `setStatus()` covers the critical case. |
| **`window.createOutputChannel()`** | Named output panels. `console.log()` + editor log viewer covers most cases. |
| **`window.showOpenDialog()` / `showSaveDialog()`** | File pickers. Plugins can use `editor.prompt()` with path completion as workaround. |
| **`window.withProgress()`** | Progress bars. `setStatus()` can show progress text. |
| **`window.showInformationMessage()` modals** | `setStatus()` covers most notification needs. Modals are often annoying. |
| **`ConfigurationTarget` enum** | Scope distinction (global vs workspace). Most plugins just read config. |
| **`WorkspaceEdit` batched edits** | Can be composed from individual `insertText`/`deleteRange` calls. |
| **`env.machineId` / `env.sessionId`** | Used for telemetry only. Not user-facing. |
| **`setInterval` / `setImmediate`** | `setTimeout` + recursion works. `editor.delay()` covers async waiting. |
| **`os.EOL` / `os.platform`** | Can derive from `editor.getEnv()`. |

### Not Needed (advanced subsystems for specific extensions)

These represent entire subsystems that only 1-2 of the heaviest extensions use:

| Gap | Why It's Not Needed Now |
|-----|------------------------|
| **Debug Adapter Protocol** | Only Python and C/C++ use it. These are IDE-level features that require massive implementation effort. Fresh can delegate debugging to external tools (terminal debuggers). |
| **Task system** | Only ESLint and C/C++ use it. `spawnProcess()` covers the actual need (running builds/lints). |
| **Notebook API** | Only Python uses it. Very specialized Jupyter integration. |
| **Authentication providers** | Only GitLens uses it (GitHub login). Could be handled via `spawnProcess()` + OAuth flow. |
| **Webview panels** | Used by GitLens (commit graph) and REST Client (response viewer). Big feature but Fresh's virtual buffers cover simpler cases. |
| **Tree view panels** | Sidebar panels (GitLens views, Docker container tree). Would be a significant UI addition. |
| **Language Model / AI** | `vscode.lm.*` used by 3 extensions. Emerging API, not essential. |
| **Custom editors** | Only GitLens uses it. Niche feature. |

---

## Recommended Implementation Order

Based on the feature analysis above, here's what to implement to maximize
the number of extensions Fresh can support:

**Phase 1** (unlocks: Code Runner, Auto Rename Tag, Material Icon Theme, Live Server):
- `env.openExternal()` - open URLs in browser
- `workspace.onDidChangeConfiguration` - config change event  
- `path.resolve`, `path.normalize`, `path.sep`, `path.relative`, `path.parse`
- `crypto.createHash`, `crypto.randomBytes`, `crypto.randomUUID`
- `os.platform`, `os.EOL`

**Phase 2** (unlocks: ESLint, Prettier, Path Intellisense):
- `workspace.createFileSystemWatcher()` - file change watching
- `languages.registerCompletionItemProvider()` - plugin completions
- `languages.registerDocumentFormattingEditProvider()` - plugin formatting
- `languages.registerCodeActionsProvider()` - quick fixes
- `languages.createDiagnosticCollection()` - plugin diagnostics
- `globalThis.fetch` - HTTP client

**Phase 3** (improves: GitLens, REST Client):
- `window.createOutputChannel()` - named output panels
- Multiple `window.createStatusBarItem()` instances
- `window.showOpenDialog()` / `window.showSaveDialog()`
- `window.withProgress()` - progress notifications
- `window.createWebviewPanel()` - HTML panels
- `window.createTreeView()` - sidebar tree panels

---

## What Fresh Does Better

Fresh's plugin API has capabilities not available in VS Code's extension API:

1. **Virtual buffers** with custom content and keybinding modes
2. **Composite buffers** (side-by-side diff views from plugins)
3. **View transforms** (real-time token transformation during rendering)
4. **Overlay system** with namespace-based management
5. **Virtual text and line decorations** with fine-grained control
6. **Scroll sync groups** between splits
7. **Line indicators** (gutter symbols)
8. **Conceals** (hide/replace text ranges)
9. **Direct byte-offset buffer access** (vs line/character abstraction)
10. **Editor mode definition** from plugins (custom vim-like modes)
11. **Streaming grep** with progress callbacks
12. **Mouse event handling** (click, move, scroll)

---

## Detailed API Comparison Tables

**Legend:**
- **Fresh has** = Fresh provides equivalent or better functionality
- **Partial** = Fresh has something related but not a direct equivalent
- **Missing** = No equivalent in Fresh; would need to be added

## 1. Workspace / Configuration

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `workspace.getConfiguration()` | 11/11 | `editor.getConfig()`, `editor.getUserConfig()` | **Fresh has** |
| `workspace.workspaceFolders` | 9/11 | `editor.getCwd()` (single root only) | **Partial** - no multi-root workspace |
| `workspace.onDidChangeConfiguration` | 8/11 | No event for config changes | **Missing** |
| `workspace.getWorkspaceFolder()` | 8/11 | `editor.getCwd()` | **Partial** - no per-URI folder resolution |
| `workspace.fs` (readFile, writeFile, stat) | 6/11 | `editor.readFile()`, `editor.writeFile()`, `editor.fileStat()` | **Fresh has** |
| `workspace.openTextDocument()` | 6/11 | `editor.openFile()` | **Fresh has** |
| `workspace.createFileSystemWatcher()` | 6/11 | No file watcher API | **Missing** |
| `workspace.onDidChangeTextDocument` | 5/11 | `editor.on('after_insert')`, `editor.on('after_delete')` | **Fresh has** (via hooks) |
| `workspace.onDidOpenTextDocument` | 5/11 | `editor.on('after_file_open')` | **Fresh has** |
| `workspace.onDidCloseTextDocument` | 5/11 | `editor.on('buffer_closed')` | **Fresh has** |
| `workspace.textDocuments` | 5/11 | `editor.listBuffers()` | **Fresh has** |
| `workspace.findFiles()` | 3/11 | `editor.grepProject()` (content search, not glob) | **Partial** - no glob-based file find |
| `workspace.applyEdit()` | 3/11 | `editor.insertText()` + `editor.deleteRange()` (manual) | **Partial** - no batched WorkspaceEdit |
| `workspace.onDidSaveTextDocument` | 3/11 | `editor.on('after_file_save')` | **Fresh has** |
| `workspace.isTrusted` | 4/11 | Not applicable | N/A |
| `workspace.saveAll()` | 2/11 | No bulk save API | **Missing** |
| `workspace.onDidChangeWorkspaceFolders` | 4/11 | Not applicable (single root) | N/A |
| `workspace.registerTextDocumentContentProvider` | 2/11 | `editor.createVirtualBuffer()` | **Fresh has** (different approach) |

## 2. Window / UI

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `window.activeTextEditor` | 9/11 | `editor.getActiveBufferId()`, `editor.getPrimaryCursor()` | **Fresh has** |
| `window.createOutputChannel()` | 8/11 | `editor.info()`, `editor.debug()`, `console.log()` | **Partial** - no named output channels |
| `window.showInformationMessage()` | 8/11 | `editor.setStatus()` | **Partial** - status bar only, no modal |
| `window.showQuickPick()` | 8/11 | `editor.prompt()` + `editor.setPromptSuggestions()` | **Fresh has** (via prompt system) |
| `window.onDidChangeActiveTextEditor` | 7/11 | `editor.on('buffer_activated')` | **Fresh has** |
| `window.showErrorMessage()` | 6/11 | `editor.error()` + `editor.setStatus()` | **Partial** - no user-facing modal |
| `window.showWarningMessage()` | 6/11 | `editor.warn()` + `editor.setStatus()` | **Partial** - no user-facing modal |
| `window.createStatusBarItem()` | 6/11 | `editor.setStatus()` (single global status) | **Partial** - no multiple status items |
| `window.showTextDocument()` | 5/11 | `editor.showBuffer()`, `editor.openFile()` | **Fresh has** |
| `window.createTerminal()` | 5/11 | `editor.createTerminal()` | **Fresh has** |
| `window.showInputBox()` | 5/11 | `editor.prompt()` | **Fresh has** |
| `window.showOpenDialog()` | 5/11 | No file picker dialog | **Missing** |
| `window.onDidCloseTerminal` | 3/11 | No terminal lifecycle event | **Missing** |
| `window.withProgress()` | 3/11 | No progress notification API | **Missing** |
| `window.createTextEditorDecorationType()` | 3/11 | `editor.addOverlay()` | **Fresh has** |
| `window.visibleTextEditors` | 3/11 | No direct equivalent (splits ≠ editors) | **Partial** |
| `window.createTreeView()` | 3/11 | No tree view / sidebar panel API | **Missing** |
| `window.createWebviewPanel()` | 3/11 | No webview panel API | **Missing** |
| `window.createQuickPick()` | 4/11 | `editor.prompt()` + suggestions | **Fresh has** |
| `window.showSaveDialog()` | 3/11 | No save dialog | **Missing** |
| `window.onDidChangeTextEditorSelection` | 3/11 | `editor.on('cursor_moved')` | **Fresh has** |
| `window.onDidChangeVisibleTextEditors` | 3/11 | No equivalent event | **Missing** |
| `window.onDidChangeWindowState` | 3/11 | `editor.on('focus_gained')` | **Partial** |
| `window.createInputBox()` | 3/11 | `editor.prompt()` | **Fresh has** |
| `window.registerTerminalLinkProvider` | 2/11 | No terminal link API | **Missing** |
| `window.registerFileDecorationProvider` | 1/11 | `editor.setFileExplorerDecorations()` | **Fresh has** |

## 3. Commands

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `commands.registerCommand()` | 9/11 | `editor.registerCommand()` | **Fresh has** |
| `commands.executeCommand()` | 8/11 | `editor.executeAction()` | **Fresh has** |
| `commands.registerTextEditorCommand()` | 3/11 | `editor.registerCommand()` (all commands have editor access) | **Fresh has** |
| `commands.getCommands()` | 1/11 | No list-commands API | **Missing** |

## 4. Extensions

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `extensions.getExtension()` | 8/11 | `editor.listPlugins()` (list only, no API access) | **Partial** - can't get another plugin's API |
| `extensions.all` | 2/11 | `editor.listPlugins()` | **Fresh has** |
| `extensions.onDidChange` | 2/11 | No event | **Missing** |

## 5. Languages

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `languages.registerCompletionItemProvider()` | 4/11 | No completion provider API (LSP handles this) | **Missing** (direct plugin API) |
| `languages.registerHoverProvider()` | 4/11 | No hover provider API (LSP handles this) | **Missing** (direct plugin API) |
| `languages.createLanguageStatusItem()` | 4/11 | No language status API | **Missing** |
| `languages.registerCodeActionsProvider()` | 3/11 | No code actions API (LSP handles this) | **Missing** (direct plugin API) |
| `languages.createDiagnosticCollection()` | 3/11 | `editor.getAllDiagnostics()` (read-only from LSP) | **Partial** - can't create diagnostics from plugins |
| `languages.setLanguageConfiguration()` | 3/11 | `editor.registerLanguageConfig()` | **Fresh has** |
| `languages.registerCodeLensProvider()` | 2/11 | No code lens API | **Missing** |
| `languages.registerDocumentFormattingEditProvider()` | 2/11 | No formatting provider API (LSP handles this) | **Missing** (direct plugin API) |
| `languages.registerDocumentSymbolProvider()` | 2/11 | No symbol provider API | **Missing** |
| `languages.registerReferenceProvider()` | 2/11 | No reference provider API | **Missing** |
| `languages.match()` | 2/11 | No document selector matching | **Missing** |
| `languages.registerDocumentRangeFormattingEditProvider()` | 2/11 | No range formatting API | **Missing** |
| `languages.getDiagnostics()` | 1/11 | `editor.getAllDiagnostics()` | **Fresh has** |
| `languages.setTextDocumentLanguage()` | 2/11 | No set-language API | **Missing** |

## 6. Environment

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `env.openExternal()` | 5/11 | No open-URL-in-browser API | **Missing** |
| `env.clipboard` / `env.clipboard.writeText` | 4/11 | `editor.copyToClipboard()`, `editor.setClipboard()` | **Fresh has** (write only; no read) |
| `env.shell` | 3/11 | `editor.getEnv('SHELL')` | **Fresh has** |
| `env.language` | 3/11 | `editor.getCurrentLocale()` | **Fresh has** |
| `env.remoteName` | 3/11 | Not applicable | N/A |
| `env.machineId` | 3/11 | No equivalent | **Missing** |
| `env.uiKind` | 3/11 | Not applicable (always TUI) | N/A |
| `env.appName` | 3/11 | Hardcoded "Fresh" | **Partial** |
| `env.uriScheme` | 2/11 | Not applicable | N/A |
| `env.sessionId` | 2/11 | No equivalent | **Missing** |
| `env.asExternalUri()` | 2/11 | No equivalent | **Missing** |
| `env.appRoot` | 2/11 | `editor.getPluginDir()` or `editor.getConfigDir()` | **Partial** |

## 7. Uri Class

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `Uri.file()` | 7/11 | `editor.pathToFileUri()` | **Fresh has** |
| `Uri.parse()` | 6/11 | No URI parsing | **Missing** |
| `Uri.joinPath()` | 5/11 | `editor.pathJoin()` (path only, not URI) | **Partial** |
| `Uri.fsPath` (property) | many | `editor.fileUriToPath()` | **Fresh has** |
| `Uri` as a class/type | 9/11 | No Uri object model | **Missing** - Fresh uses strings |

## 8. Debug

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `debug.registerDebugConfigurationProvider()` | 2/11 | No debug API | **Missing** |
| `debug.registerDebugAdapterDescriptorFactory()` | 2/11 | No debug API | **Missing** |
| `debug.startDebugging()` | 2/11 | No debug API | **Missing** |
| `debug.activeDebugSession` | 2/11 | No debug API | **Missing** |
| All other debug APIs | 1-2/11 | No debug API | **Missing** |

## 9. Tasks

| vscode API | Ext Count | Fresh Equivalent | Status |
|------------|-----------|------------------|--------|
| `tasks.registerTaskProvider()` | 2/11 | No task system | **Missing** |
| `tasks.executeTask()` | 1/11 | `editor.spawnProcess()` (lower level) | **Partial** |

## 10. Core Types

| vscode Type | Ext Count | Fresh Equivalent | Status |
|-------------|-----------|------------------|--------|
| `ExtensionContext` | 8/11 | `getEditor()` return value | **Fresh has** (different shape) |
| `Uri` | 9/11 | Plain strings | **Missing** - no Uri object |
| `Disposable` | 8/11 | No dispose pattern | **Missing** |
| `CancellationToken` | 7/11 | `ProcessHandle.kill()` (for processes only) | **Partial** |
| `Range` | 7/11 | Byte offset pairs (start, end) | **Partial** - different model (bytes vs line:col) |
| `TextDocument` | 7/11 | `BufferInfo` + `editor.getBufferText()` | **Partial** - different model |
| `Position` | 6/11 | Byte offsets | **Partial** - different model |
| `Event` / `EventEmitter` | 6/11 | `editor.on()` / `editor.off()` | **Fresh has** (different pattern) |
| `OutputChannel` | 5/11 | `console.log()` / `editor.info()` | **Partial** |
| `WorkspaceConfiguration` | 5/11 | `editor.getConfig()` return | **Fresh has** |
| `TextEditor` | 5/11 | Split + Buffer concept | **Partial** |
| `Selection` | 4/11 | `CursorInfo` (has selection range) | **Fresh has** |
| `WorkspaceEdit` | 4/11 | No batched edit type | **Missing** |
| `ConfigurationTarget` enum | 4/11 | No scope distinction | **Missing** |
| `CompletionItem` | 2/11 | `PromptSuggestion` (for prompts, not code) | **Missing** for code |
| `Diagnostic` / `DiagnosticSeverity` | 3/11 | `JsDiagnostic` (read-only from LSP) | **Partial** |
| `StatusBarItem` | 5/11 | `editor.setStatus()` | **Partial** - single global |
| `QuickPickItem` | 2/11 | `PromptSuggestion` | **Fresh has** |
| `TreeItem` / `TreeDataProvider` | 1-2/11 | No tree view types | **Missing** |
| `WebviewPanel` | 1-2/11 | No webview types | **Missing** |

---

## 11. Node.js Builtins

| Node.js API | Ext Count | Fresh Equivalent | Status |
|-------------|-----------|------------------|--------|
| `path.join` | 11/11 | `editor.pathJoin()` | **Fresh has** |
| `path.dirname` | 9/11 | `editor.pathDirname()` | **Fresh has** |
| `path.basename` | 9/11 | `editor.pathBasename()` | **Fresh has** |
| `path.extname` | 7/11 | `editor.pathExtname()` | **Fresh has** |
| `path.isAbsolute` | 7/11 | `editor.pathIsAbsolute()` | **Fresh has** |
| `path.resolve` | 6/11 | No path resolve | **Missing** |
| `path.parse` | 5/11 | No path parse | **Missing** |
| `path.sep` | 5/11 | No path separator constant | **Missing** |
| `path.normalize` | 4/11 | No path normalize | **Missing** |
| `path.relative` | 3/11 | No relative path | **Missing** |
| `fs.existsSync` | 6/11 | `editor.fileExists()` | **Fresh has** |
| `fs.readFileSync` | 5/11 | `editor.readFile()` | **Fresh has** |
| `fs.writeFileSync` | 4/11 | `editor.writeFile()` | **Fresh has** |
| `fs.readdirSync` | 4/11 | `editor.readDir()` | **Fresh has** |
| `fs.mkdirSync` / `fs/promises.mkdir` | 3/11 | `editor.createDir()` | **Fresh has** |
| `fs.unlinkSync` | 3/11 | `editor.removePath()` | **Fresh has** |
| `fs.statSync` / `fs.promises.stat` | 3/11 | `editor.fileStat()` | **Fresh has** |
| `fs/promises.*` (async variants) | 3/11 | All Fresh fs ops are sync-style | **Partial** |
| `os.tmpdir` | 5/11 | `editor.getTempDir()` | **Fresh has** |
| `os.homedir` | 5/11 | `editor.getEnv('HOME')` | **Fresh has** (via env) |
| `os.EOL` | 3/11 | No line ending constant | **Missing** |
| `os.platform` | 2/11 | No platform constant | **Missing** |
| `crypto.createHash` | 5/11 | No crypto API | **Missing** |
| `crypto.randomBytes` | 4/11 | No crypto API | **Missing** |
| `crypto.randomUUID` | 2/11 | No UUID generation | **Missing** |
| `http.createServer` | 3/11 | No HTTP server API | **Missing** |
| `child_process.spawn` | 4/11 | `editor.spawnProcess()` / `editor.spawnBackgroundProcess()` | **Fresh has** |
| `child_process.exec` | 3/11 | `editor.spawnProcess()` (with shell) | **Fresh has** |
| `child_process.execSync` | 4/11 | No sync process execution | **Partial** |

## 12. Process/Environment

| API | Ext Count | Fresh Equivalent | Status |
|-----|-----------|------------------|--------|
| `process.env` | 6/11 | `editor.getEnv()` | **Fresh has** |
| `process.platform` | 6/11 | `editor.getEnv('OS')` (indirect) | **Partial** |
| `process.cwd()` | 6/11 | `editor.getCwd()` | **Fresh has** |
| `process.exit` | 4/11 | Not applicable for plugins | N/A |
| `process.argv` | 3/11 | Not applicable for plugins | N/A |
| `process.arch` | 2/11 | No arch info | **Missing** |

## 13. Globals

| API | Ext Count | Fresh Equivalent | Status |
|-----|-----------|------------------|--------|
| `setTimeout` | 8/11 | `editor.delay()` (async) or QuickJS timers | **Fresh has** |
| `setInterval` | 4/11 | QuickJS may not have setInterval | **Missing** (verify) |
| `setImmediate` | 3/11 | Not available in QuickJS | **Missing** |
| `fetch` | 3/11 | No HTTP client API | **Missing** |
| `console.log/warn/error` | all | `console.log/warn/error` | **Fresh has** |

