# VS Code Extension API & Side Effects Analysis

AST-based analysis of the top 12 most-installed open-source VS Code extensions
to determine the minimal API surface and platform capabilities required to
support them.

## Extensions Analyzed

| Extension | Installs | Repository | vscode API | Types | Node.js | Process | Shell | Network |
|-----------|----------|------------|-----------|-------|---------|---------|-------|---------|
| Python | ~211M | microsoft/vscode-python | 161 | 113 | 53 | 36 | 3 | 0 |
| C/C++ | ~97M | microsoft/vscode-cpptools | 196 | 39 | 78 | 40 | 5 | 5 |
| Live Server | ~76M | ritwickdey/vscode-live-server | 16 | 5 | 12 | 2 | 0 | 0 |
| Prettier | ~67M | prettier/prettier-vscode | 32 | 18 | 15 | 4 | 1 | 0 |
| Docker | ~50M | microsoft/vscode-docker | 0 | 0 | 0 | 0 | 0 | 0 |
| ESLint | ~49M | microsoft/vscode-eslint | 43 | 18 | 23 | 9 | 2 | 0 |
| GitLens | ~49M | gitkraken/vscode-gitlens | 207 | 98 | 54 | 0 | 3 | 1 |
| Code Runner | ~39M | formulahendry/vscode-code-runner | 20 | 1 | 8 | 0 | 0 | 0 |
| Material Icon Theme | ~33M | PKief/vscode-material-icon-theme | 9 | 4 | 17 | 2 | 1 | 1 |
| Path Intellisense | ~20M | ChristianKohler/PathIntellisense | 22 | 3 | 4 | 0 | 0 | 0 |
| Auto Rename Tag | ~18M | formulahendry/vscode-auto-rename-tag | 29 | 0 | 6 | 5 | 0 | 0 |
| REST Client | ~14M | Huachao/vscode-restclient | 60 | 38 | 19 | 6 | 0 | 2 |

Notes:
- Docker extension ships without source code in the public repo (0 files).
- Column counts are unique API calls (not invocations).
- C/C++ has the heaviest Node.js and process usage; GitLens has the most vscode API surface.
- Code Runner is a good "minimal viable extension" reference (5 files, 20 API calls).

## Methodology

The analysis uses the **TypeScript compiler API** (AST-based) to parse extension
source files. For each file, it:

1. **Collects all imports** (ES modules, CommonJS `require()`, namespace imports,
   destructured imports, aliased imports, type-only imports)
2. **Walks the AST** to find all property access chains and call expressions
3. **Resolves each access** back to its imported module to categorize it
4. **Distinguishes type-only usage** from runtime usage via position analysis

Categories tracked:
- **vscode API** - methods, properties, events, enum values from the `vscode` module
- **Node.js builtins** - `fs`, `path`, `os`, `child_process`, `crypto`, `http`, etc.
- **Process/environment** - `process.env`, `process.platform`, `process.cwd()`, etc.
- **Shell execution** - `child_process.exec`, `spawn`, `execSync`, etc.
- **Network/HTTP** - `fetch`, `axios`, `node-fetch`, `http.createServer`
- **Global side effects** - `setTimeout`, `setInterval`, `setImmediate`

Advantages over regex: resolves import aliases (`import { window as codeWindow }`),
ignores strings/comments, tracks deep property chains (`vscode.ConfigurationTarget.Global`),
distinguishes type vs runtime usage.

Source: `scripts/vscode-extension-analysis/analyze-vscode-api-ast.ts`

---

## Part 1: vscode API Surface

### Namespace Priority

```
vscode.workspace     11/11 extensions   45 unique methods/properties
vscode.window        10/11 extensions   53 unique methods/properties
vscode.commands      10/11 extensions    6 unique methods/properties
vscode.extensions     8/11 extensions    5 unique methods/properties
vscode.env            7/11 extensions   18 unique methods/properties
vscode.languages      7/11 extensions   25 unique methods/properties
vscode.lm             3/11 extensions    5 unique methods/properties
vscode.debug          2/11 extensions   14 unique methods/properties
vscode.tasks          2/11 extensions    8 unique methods/properties
vscode.chat           1/11 extensions    1 unique methods/properties
vscode.authentication 1/11 extensions    2 unique methods/properties
vscode.l10n           1/11 extensions    1 unique methods/properties
vscode.notebooks      1/11 extensions    1 unique methods/properties
```

### Essential API (8+ of 11 extensions)

These form the absolute core - every non-trivial extension uses them:

| API | Ext Count | Description |
|-----|-----------|-------------|
| `workspace.getConfiguration()` | 11/11 | Read user/workspace settings |
| `window.activeTextEditor` | 9/11 | Current focused editor |
| `workspace.workspaceFolders` | 9/11 | Open workspace folders |
| `commands.registerCommand()` | 9/11 | Register a command handler |
| `window.createOutputChannel()` | 8/11 | Create extension output panel |
| `workspace.onDidChangeConfiguration` | 8/11 | Settings change event |
| `extensions.getExtension()` | 8/11 | Query another extension |
| `commands.executeCommand()` | 8/11 | Execute any command |
| `window.showInformationMessage()` | 8/11 | Info notification |
| `window.showQuickPick()` | 8/11 | Selection dropdown |
| `workspace.getWorkspaceFolder()` | 8/11 | Resolve folder for URI |

### Common API (5-7 of 11 extensions)

| API | Ext Count | Description |
|-----|-----------|-------------|
| `window.onDidChangeActiveTextEditor` | 7/11 | Editor focus change |
| `Uri.file()` | 7/11 | Create file URI |
| `window.activeTextEditor.document` | 6/11 | Current document |
| `workspace.fs` | 6/11 | Virtual file system |
| `workspace.openTextDocument()` | 6/11 | Open doc by URI |
| `window.showErrorMessage()` | 6/11 | Error notification |
| `Uri.parse()` | 6/11 | Parse URI string |
| `window.showWarningMessage()` | 6/11 | Warning notification |
| `workspace.createFileSystemWatcher()` | 6/11 | Watch file changes |
| `window.createStatusBarItem()` | 6/11 | Status bar item |
| `StatusBarAlignment.Right` | 6/11 | Status bar position |
| `workspace.onDidChangeTextDocument` | 5/11 | Document content change |
| `window.showTextDocument()` | 5/11 | Show doc in editor |
| `window.createTerminal()` | 5/11 | Integrated terminal |
| `env.openExternal()` | 5/11 | Open URL in browser |
| `window.showInputBox()` | 5/11 | Text input dialog |
| `window.showOpenDialog()` | 5/11 | File open dialog |
| `workspace.onDidOpenTextDocument` | 5/11 | Document opened |
| `workspace.onDidCloseTextDocument` | 5/11 | Document closed |
| `workspace.textDocuments` | 5/11 | All open documents |
| `Uri.joinPath()` | 5/11 | URI path manipulation |

### Secondary API (3-4 of 11 extensions)

| API | Ext Count | Category |
|-----|-----------|----------|
| `window.withProgress()` | 3 | Progress UI |
| `ProgressLocation.Notification` | 3 | Progress location enum |
| `commands.registerTextEditorCommand()` | 3 | Editor-scoped commands |
| `env.remoteName` | 3 | Remote dev detection |
| `env.shell` | 3 | User's shell |
| `env.clipboard` / `env.clipboard.writeText` | 4/3 | Clipboard access |
| `window.visibleTextEditors` | 3 | All visible editors |
| `window.onDidChangeTextEditorSelection` | 3 | Cursor/selection change |
| `window.onDidChangeVisibleTextEditors` | 3 | Editor visibility |
| `window.createTextEditorDecorationType()` | 3 | Inline decorations |
| `window.createTreeView()` | 3 | Sidebar tree panels |
| `window.createWebviewPanel()` | 3 | HTML webview panels |
| `window.onDidChangeWindowState` | 3 | Window focus |
| `window.onDidCloseTerminal` | 3 | Terminal lifecycle |
| `workspace.applyEdit()` | 3 | Programmatic edits |
| `workspace.onDidSaveTextDocument` | 3 | Document saved |
| `workspace.onDidChangeWorkspaceFolders` | 4 | Folder changes |
| `workspace.isTrusted` | 4 | Workspace trust |
| `languages.registerCompletionItemProvider()` | 4 | Autocomplete |
| `languages.registerHoverProvider()` | 4 | Hover tooltips |
| `languages.createLanguageStatusItem()` | 4 | Language status |
| `languages.createDiagnosticCollection()` | 3 | Error squiggles |
| `languages.setLanguageConfiguration()` | 3 | Bracket/comment rules |
| `languages.registerCodeActionsProvider()` | 3 | Quick fixes |
| `ConfigurationTarget.Global/Workspace/WorkspaceFolder` | 4 | Config scope enums |
| `Disposable.from()` | 4 | Combine disposables |
| `CodeActionKind.QuickFix` | 3 | Code action enum |
| `ViewColumn.Active` | 3 | Editor column enum |
| `DiagnosticSeverity.Warning` | 3 | Severity enum |
| `UIKind.Web` | 3 | Web/desktop detection |
| `QuickPickItemKind.Separator` | 3 | Quick pick enum |

### Essential Types (type-position only, used by 4+ extensions)

These are types that extensions reference in type annotations, parameters, and
return types. An implementation needs corresponding TypeScript interfaces/classes.

| Type | Ext Count | Purpose |
|------|-----------|---------|
| `Uri` | 9 | Universal resource identifier |
| `ExtensionContext` | 8 | Activation lifecycle context |
| `Disposable` | 8 | Resource cleanup |
| `CancellationToken` | 7 | Cancellation signaling |
| `Range` | 7 | Text range (start-end positions) |
| `TextDocument` | 7 | Open document interface |
| `Position` | 6 | Line/character position |
| `Event` | 6 | Event signature type |
| `OutputChannel` | 5 | Output panel interface |
| `WorkspaceConfiguration` | 5 | Settings interface |
| `TextEditor` | 5 | Editor instance |
| `WorkspaceFolder` | 5 | Workspace folder |
| `EventEmitter` | 4 | Event emitter class |
| `WorkspaceEdit` | 4 | Batched edit operations |
| `ConfigurationChangeEvent` | 4 | Settings change event |
| `Selection` | 4 | Cursor selection |
| `Extension` | 4 | Extension metadata |

---

## Part 2: Platform Side Effects (Non-vscode)

### Node.js Builtin Usage

Extensions heavily depend on Node.js builtins for file I/O, path manipulation,
cryptography, and system information.

**Path manipulation (universally used):**

| API | Ext Count | Purpose |
|-----|-----------|---------|
| `path.join` | 11/11 | Every single extension |
| `path.dirname` | 9/11 | Directory from path |
| `path.basename` | 9/11 | Filename from path |
| `path.extname` | 7/11 | File extension |
| `path.isAbsolute` | 7/11 | Absolute path check |
| `path.resolve` | 6/11 | Resolve to absolute |
| `path.parse` | 5/11 | Parse path components |
| `path.sep` | 5/11 | Platform path separator |
| `path.normalize` | 4/11 | Normalize separators |
| `path.relative` | 3/11 | Relative path between |

**File system (core requirement):**

| API | Ext Count | Purpose |
|-----|-----------|---------|
| `fs.existsSync` | 6/11 | Check file existence |
| `fs.readFileSync` | 5/11 | Read file (sync) |
| `fs.writeFileSync` | 4/11 | Write file (sync) |
| `fs.readdirSync` | 4/11 | List directory (sync) |
| `fs.readdir` | 4/11 | List directory (async) |
| `fs.promises.readdir` | 3/11 | List directory (promises) |
| `fs.promises.readFile` | 3/11 | Read file (promises) |
| `fs.promises.writeFile` | 3/11 | Write file (promises) |
| `fs.promises.stat` | 3/11 | File stats (promises) |
| `fs/promises.mkdir` | 3/11 | Create directory |
| `fs/promises.readFile` | 3/11 | Read file (promises) |
| `fs/promises.writeFile` | 3/11 | Write file (promises) |
| `fs.unlinkSync` | 3/11 | Delete file (sync) |
| `fs.statSync` | 2/11 | File stats (sync) |
| `fs.mkdirSync` | 2/11 | Create directory (sync) |

**OS information:**

| API | Ext Count | Purpose |
|-----|-----------|---------|
| `os.tmpdir` | 5/11 | Temporary directory |
| `os.homedir` | 5/11 | Home directory |
| `os.EOL` | 3/11 | Line ending char |
| `os.platform` | 2/11 | OS platform string |
| `os.release` | 2/11 | OS version |
| `os.networkInterfaces` | 2/11 | Network info |

**Cryptography:**

| API | Ext Count | Purpose |
|-----|-----------|---------|
| `crypto.createHash` | 5/11 | Hashing (MD5, SHA) |
| `crypto.randomBytes` | 4/11 | Random data |
| `crypto.randomUUID` | 2/11 | UUID generation |

**HTTP servers:**

| API | Ext Count | Purpose |
|-----|-----------|---------|
| `http.createServer` | 3/11 | Local HTTP server (live-server, gitlens, rest-client) |

### Process/Environment Access

Extensions extensively access the process environment for configuration,
platform detection, and path resolution.

| API | Ext Count | Purpose |
|-----|-----------|---------|
| `process.env` | 6/11 | Environment variables |
| `process.platform` | 6/11 | OS detection (win32/linux/darwin) |
| `process.cwd` | 6/11 | Current working directory |
| `process.exit` | 4/11 | Exit process |
| `process.on` | 4/11 | Process event handlers |
| `process.argv` | 3/11 | Command line arguments |
| `process.env.PATH` | 2/11 | System PATH |
| `process.arch` | 2/11 | CPU architecture |
| `process.pid` | 2/11 | Process ID |
| `process.execPath` | 2/11 | Node executable path |
| `process.stdout` | 2/11 | Standard output |
| `process.stdin` | 2/11 | Standard input |
| `process.kill` | 2/11 | Signal process |
| `process.chdir` | 2/11 | Change directory |
| `process.hrtime` | 2/11 | High-res time |

Specific env vars accessed by multiple extensions:
- `process.env.PATH` (cpptools, python) - find executables
- `process.env.XDG_CACHE_HOME` (cpptools, rest-client) - cache directory

### Shell/Process Execution

Extensions that need to run external tools (compilers, formatters, linters, git):

| API | Ext Count | Extensions |
|-----|-----------|------------|
| `child_process.spawn` | 4/11 | cpptools, gitlens, prettier, python |
| `child_process.execSync` | 4/11 | cpptools, eslint, material-icon-theme, python |
| `child_process.exec` | 3/11 | cpptools, gitlens, python |
| `child_process.spawnSync` | 2/11 | cpptools, eslint |
| `child_process.execFileSync` | 1/11 | cpptools |
| `child_process.execFile` | 1/11 | gitlens |

Key insight: 6 of 11 extensions (55%) need to spawn external processes.

### Network/HTTP Usage

| API | Ext Count | Extensions |
|-----|-----------|------------|
| `globalThis.fetch` | 3/11 | cpptools, gitlens, rest-client |
| `node-fetch` | 2/11 | cpptools, rest-client |
| `axios` | 2/11 | cpptools, material-icon-theme |
| `http.createServer` | 3/11 | gitlens, live-server, rest-client |

### Global Side Effects (Timers)

| API | Ext Count |
|-----|-----------|
| `setTimeout` | 8/11 |
| `setInterval` | 4/11 |
| `setImmediate` | 3/11 |

---

## Part 3: package.json Contributes

Static declarations processed at extension load time (before activation):

| Contribute Key | Ext Count | Purpose |
|---------------|-----------|---------|
| `configuration` | 9/11 | Settings schema |
| `commands` | 8/11 | Command palette entries |
| `keybindings` | 5/11 | Keyboard shortcuts |
| `menus` | 5/11 | Context menu entries |
| `languages` | 5/11 | Language declarations |
| `grammars` | 3/11 | TextMate grammars |
| `jsonValidation` | 3/11 | JSON schema validation |
| `configurationDefaults` | 2/11 | Default settings |
| `submenus` | 2/11 | Submenu definitions |
| `viewsWelcome` | 2/11 | Welcome content |
| `walkthroughs` | 2/11 | Getting started |

---

## Part 4: Minimal Implementation Roadmap

### Phase 1: Core Extension Host

**Goal:** Support Code Runner, Live Server, Material Icon Theme, Path Intellisense, Auto Rename Tag

Implement:

**vscode runtime:**
- Extension lifecycle (activate/deactivate)
- `ExtensionContext` (subscriptions, globalState, workspaceState, extensionPath, extensionUri)
- `Disposable` class + `Disposable.from()`
- `Uri` class (file, parse, joinPath, fsPath, scheme, path, authority)
- `commands.registerCommand()` / `commands.executeCommand()`
- `workspace.getConfiguration()` → `WorkspaceConfiguration` (get, has, update, inspect)
- `workspace.onDidChangeConfiguration` event
- `workspace.workspaceFolders` / `workspace.getWorkspaceFolder()`
- `workspace.openTextDocument()` / `window.showTextDocument()`
- `workspace.fs` (readFile, writeFile, stat, readDirectory, delete, rename, createDirectory)
- `workspace.createFileSystemWatcher()`
- `window.activeTextEditor` / `window.onDidChangeActiveTextEditor`
- `window.showInformationMessage/showWarningMessage/showErrorMessage()`
- `window.showQuickPick()` / `window.showInputBox()`
- `window.createOutputChannel()`
- `window.createStatusBarItem()` + `StatusBarAlignment` enum
- `extensions.getExtension()`

**Node.js builtins needed:**
- `path` (join, dirname, basename, extname, isAbsolute, resolve, sep, parse, normalize)
- `fs` (existsSync, readFileSync, writeFileSync, readdirSync, statSync)
- `os` (tmpdir, homedir, platform, EOL)
- `crypto` (createHash, randomBytes)

**Globals:**
- `process.env`, `process.platform`, `process.cwd()`
- `setTimeout`, `setInterval`

**package.json:**
- `contributes.configuration`, `contributes.commands`, `contributes.keybindings`,
  `contributes.menus`, `contributes.languages`

### Phase 2: Rich UI and Language Features

**Goal:** Add support for ESLint, Prettier, REST Client, most of GitLens

Additional vscode API:
- `window.createTerminal()` / `window.onDidCloseTerminal`
- `window.showOpenDialog()` / `window.showSaveDialog()`
- `window.withProgress()` + `ProgressLocation` enum
- `window.createTextEditorDecorationType()` (inline decorations)
- `window.createTreeView()` (sidebar panels)
- `window.createWebviewPanel()` (HTML panels)
- `window.visibleTextEditors` / `window.onDidChangeVisibleTextEditors`
- Document lifecycle events (open, close, save, change)
- `workspace.applyEdit()` + `WorkspaceEdit`
- `workspace.textDocuments`, `workspace.findFiles()`
- `workspace.isTrusted` / `workspace.onDidGrantWorkspaceTrust`
- `env.clipboard` (read/write), `env.openExternal()`, `env.shell`, `env.language`
- `ConfigurationTarget` enum (Global, Workspace, WorkspaceFolder)
- `languages.registerCompletionItemProvider()` (autocomplete)
- `languages.registerHoverProvider()` (hover)
- `languages.registerCodeActionsProvider()` (quick fixes) + `CodeActionKind` enum
- `languages.registerCodeLensProvider()` (inline actions)
- `languages.registerDocumentFormattingEditProvider()` (formatting)
- `languages.createDiagnosticCollection()` + `DiagnosticSeverity` enum
- `languages.setLanguageConfiguration()` (brackets, comments)
- `languages.createLanguageStatusItem()`
- `ViewColumn` enum

Additional platform:
- `child_process.spawn`, `child_process.exec`, `child_process.execSync`
- `fs/promises` (readFile, writeFile, mkdir, stat, readdir, rm)
- `http.createServer`
- `globalThis.fetch`

### Phase 3: Full Feature Support

**Goal:** Complete support for C/C++, Python (debug/notebooks), GitLens (auth/AI)

Additional vscode API:
- `debug.*` (registerDebugConfigurationProvider, registerDebugAdapterDescriptorFactory,
  startDebugging, activeDebugSession, breakpoints, session events)
- `tasks.*` (registerTaskProvider, executeTask, lifecycle events)
- `lm.*` (registerTool, invokeTool, selectChatModels)
- `notebooks.createNotebookController()`
- `authentication.getSession()`
- Full `languages` provider set (references, symbols, rename, call hierarchy,
  semantic tokens, inlay hints, folding, on-type formatting)
- `window.registerFileDecorationProvider()`, `window.registerUriHandler()`
- `window.registerWebviewViewProvider()`, `window.registerCustomEditorProvider()`
- `workspace.registerTextDocumentContentProvider()`
- `workspace.registerFileSystemProvider()`

---

## Code Runner: Reference "Minimal Extension"

Code Runner is an excellent reference for what a minimal but useful extension
needs. It has only 5 source files and exercises the core API surface:

**vscode API used (20 unique):**
```
workspace.getConfiguration        workspace.workspaceFolders
workspace.getWorkspaceFolder      workspace.openTextDocument
workspace.saveAll                 workspace (direct reference)
window.activeTextEditor           window.showInformationMessage
window.showQuickPick              window.createOutputChannel
window.createTerminal             window.onDidCloseTerminal
window (direct reference)         commands.executeCommand
commands.registerCommand          commands (direct reference)
extensions.getExtension           extensions (direct reference)
env.shell                         env (direct reference)
```

**Node.js builtins:** path.join, path.dirname, path.basename, path.extname, os.tmpdir,
fs.existsSync, fs.writeFileSync, fs.unlinkSync

**Types:** TextDocument, WorkspaceConfiguration, Uri, Disposable, OutputChannel,
Terminal, ExtensionContext

**Contributes:** 4 commands, 4 keybindings, 1 configuration, 1 language, 1 grammar, 1 menu

This represents a good "smoke test" for Phase 1 implementation.

---

## Automation

### Running the analysis

```bash
cd scripts/vscode-extension-analysis

# Full AST-based analysis (recommended)
npx ts-node analyze-vscode-api-ast.ts extensions

# Clone repos and analyze
npx ts-node analyze-vscode-api-ast.ts --clone-and-analyze

# Legacy regex-based analysis (faster, less precise)
npx ts-node analyze-vscode-api.ts extensions
```

### Output files

```
scripts/vscode-extension-analysis/analysis/
  ast_analysis_report.txt        # Combined text report (AST)
  ast_api_usage.json             # Structured JSON (AST) - for programmatic use
  minimal_api_surface.txt        # Combined text report (regex)
  api_usage.json                 # Structured JSON (regex)
  <extension>/
    ast_api_usage.txt            # Per-extension AST report
    api_usage.txt                # Per-extension regex report
```

### JSON structure

The `ast_api_usage.json` contains:

```json
{
  "crossExtension": {
    "vscodeApi": { "<api>": { "count": N, "extensions": [...] } },
    "vscodeTypes": { ... },
    "nodeBuiltins": { ... },
    "processUsage": { ... },
    "shellUsage": { ... },
    "networkUsage": { ... },
    "globalSideEffects": { ... },
    "contributes": { ... }
  },
  "extensions": [
    {
      "name": "...",
      "vscodeApi": { "<api>": { "count": N, "files": [...] } },
      "nodeBuiltins": { ... },
      ...
    }
  ]
}
```

This can be consumed by other tools to generate type stubs, test harnesses,
compatibility matrices, or extension sandboxing policies.
