# Fresh vs VSCode Plugin API Comparison

This document provides a comprehensive method-by-method comparison between Fresh's TypeScript plugin API and VSCode's Extension API, identifying feature gaps and parity status.

## Executive Summary

Fresh provides **~60 methods** across a single `editor` API object.
VSCode provides **hundreds of methods** across 15+ namespaces including `window`, `workspace`, `languages`, `commands`, `debug`, `scm`, `tasks`, `notebooks`, and more.

**Overall Parity: ~25-30%** of VSCode's core extension capabilities.

---

## Category-by-Category Comparison

### 1. Commands & Actions

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Register command | `editor.registerCommand(name, desc, action, contexts)` | `vscode.commands.registerCommand(id, callback)` | ✅ Partial |
| Execute command | ❌ Not available | `vscode.commands.executeCommand(id, ...args)` | ❌ Missing |
| List commands | ❌ Not available | `vscode.commands.getCommands()` | ❌ Missing |
| Text editor command | ❌ Not available | `vscode.commands.registerTextEditorCommand()` | ❌ Missing |
| Keybinding context | `contexts` parameter in registerCommand | `when` clauses in package.json | ⚠️ Different approach |

**Gap Analysis:**
- Fresh lacks programmatic command execution
- No way to query available commands
- No command argument passing or return values

---

### 2. User Interface & Notifications

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Status message | `editor.setStatus(message)` | `vscode.window.setStatusBarMessage()` | ✅ Equivalent |
| Debug logging | `editor.debug(message)` | `console.log()` + output channel | ✅ Equivalent |
| Information dialog | ❌ Not available | `vscode.window.showInformationMessage()` | ❌ Missing |
| Warning dialog | ❌ Not available | `vscode.window.showWarningMessage()` | ❌ Missing |
| Error dialog | ❌ Not available | `vscode.window.showErrorMessage()` | ❌ Missing |
| Text input box | `editor.startPrompt()` | `vscode.window.showInputBox()` | ✅ Partial |
| Quick pick selector | `editor.setPromptSuggestions()` | `vscode.window.showQuickPick()` | ✅ Partial |
| Progress indicator | ❌ Not available | `vscode.window.withProgress()` | ❌ Missing |
| Tree view | ❌ Not available | `vscode.window.registerTreeDataProvider()` | ❌ Missing |
| Webview panels | ❌ Not available | `vscode.window.createWebviewPanel()` | ❌ Missing |
| Output channel | ❌ Not available | `vscode.window.createOutputChannel()` | ❌ Missing |
| Integrated terminal | ❌ Not available | `vscode.window.createTerminal()` | ❌ Missing |

**Gap Analysis:**
- Fresh lacks modal dialogs for user interaction
- No progress indicators for long-running tasks
- No tree view UI for hierarchical data (file explorer, outline)
- No webview support for custom HTML/JS UIs
- No output channels for streaming logs
- No terminal integration

---

### 3. Buffer/Document Management

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Get active buffer | `editor.getActiveBufferId()` | `vscode.window.activeTextEditor.document` | ✅ Equivalent |
| Get buffer content | `editor.getBufferText(id, start, end)` | `document.getText(range?)` | ✅ Equivalent |
| Get buffer path | `editor.getBufferPath(id)` | `document.uri.fsPath` | ✅ Equivalent |
| Get buffer length | `editor.getBufferLength(id)` | `document.getText().length` | ✅ Equivalent |
| Is buffer modified | `editor.isBufferModified(id)` | `document.isDirty` | ✅ Equivalent |
| List all buffers | `editor.listBuffers()` | `vscode.workspace.textDocuments` | ✅ Equivalent |
| Get buffer info | `editor.getBufferInfo(id)` | Multiple document properties | ✅ Equivalent |
| Open document | `editor.openFile(path, line, col)` | `vscode.workspace.openTextDocument()` + `vscode.window.showTextDocument()` | ✅ Equivalent |
| Open in split | `editor.openFileInSplit(split, path, line, col)` | `vscode.window.showTextDocument(doc, column)` | ✅ Equivalent |
| Close document | ❌ Not available | `vscode.window.tabGroups.close()` | ❌ Missing |
| Document language ID | ❌ Not available | `document.languageId` | ❌ Missing |
| Document encoding | ❌ Not available | `document.encoding` | ❌ Missing |
| Line separator | ❌ Not available | `document.eol` | ❌ Missing |
| Line count | ❌ Not available | `document.lineCount` | ❌ Missing |
| Get line at position | ❌ Not available | `document.lineAt(position)` | ❌ Missing |
| Position at offset | ❌ Not available | `document.positionAt(offset)` | ❌ Missing |
| Offset at position | ❌ Not available | `document.offsetAt(position)` | ❌ Missing |
| Word at position | ❌ Not available | `document.getWordRangeAtPosition()` | ❌ Missing |
| Validate position | ❌ Not available | `document.validatePosition()` | ❌ Missing |

**Gap Analysis:**
- Fresh lacks structured line/column access (byte offsets only)
- No language identification
- No document metadata (encoding, line endings)
- No word boundary detection
- No position validation utilities

---

### 4. Text Editing & Mutations

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Insert text | `editor.insertText(id, pos, text)` | `editor.edit(builder => builder.insert())` | ✅ Equivalent |
| Delete range | `editor.deleteRange(id, start, end)` | `editor.edit(builder => builder.delete())` | ✅ Equivalent |
| Replace text | Insert + delete combination | `editor.edit(builder => builder.replace())` | ⚠️ Manual |
| Insert at cursor | `editor.insertAtCursor(text)` | `editor.edit()` with selection | ✅ Equivalent |
| Atomic multi-edit | ❌ Not available | Single `editor.edit()` call | ❌ Missing |
| Workspace-wide edit | ❌ Not available | `vscode.workspace.applyEdit()` | ❌ Missing |
| Undo/redo control | ❌ Not available | Part of edit API | ❌ Missing |
| Insert snippet | ❌ Not available | `editor.insertSnippet()` | ❌ Missing |
| Format document | ❌ Not available | `vscode.commands.executeCommand('editor.action.formatDocument')` | ❌ Missing |
| Format selection | ❌ Not available | `vscode.commands.executeCommand('editor.action.formatSelection')` | ❌ Missing |

**Gap Analysis:**
- Fresh lacks atomic multi-edit transactions
- No workspace-wide refactoring support
- No snippet insertion with tab stops
- No format-on-save or programmatic formatting

---

### 5. Cursor & Selection

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Get cursor position | `editor.getCursorPosition()` | `editor.selection.active` | ✅ Equivalent |
| Get cursor line | `editor.getCursorLine()` | `editor.selection.active.line` | ✅ Equivalent |
| Get all cursors | `editor.getAllCursorPositions()` | `editor.selections` | ✅ Equivalent |
| Get primary cursor with selection | `editor.getPrimaryCursor()` | `editor.selection` | ✅ Equivalent |
| Get all cursors with selections | `editor.getAllCursors()` | `editor.selections` | ✅ Equivalent |
| Set cursor position | ❌ Not available | `editor.selection = new Selection()` | ❌ Missing |
| Set multiple cursors | ❌ Not available | `editor.selections = []` | ❌ Missing |
| Move cursor programmatically | ❌ Not available | Cursor commands or selection assignment | ❌ Missing |
| Reveal range (scroll to) | ❌ Not available | `editor.revealRange()` | ❌ Missing |

**Gap Analysis:**
- Fresh can read but not write cursor/selection state
- No programmatic cursor movement
- No ability to scroll viewport to specific location
- Critical gap for refactoring and navigation plugins

---

### 6. Visual Decorations

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Add highlight overlay | `editor.addOverlay(id, overlayId, start, end, r, g, b, underline)` | `editor.setDecorations(type, ranges)` | ✅ Equivalent |
| Remove overlay | `editor.removeOverlay(id, overlayId)` | `editor.setDecorations(type, [])` | ✅ Equivalent |
| Remove by prefix | `editor.removeOverlaysByPrefix(id, prefix)` | Manual tracking required | ✅ Better |
| Clear all overlays | `editor.clearAllOverlays(id)` | Clear all decoration types | ✅ Equivalent |
| Custom decoration type | RGB + underline only | `vscode.window.createTextEditorDecorationType()` | ⚠️ Limited |
| Background color | RGB in addOverlay | Full CSS color support | ✅ Partial |
| Border styling | ❌ Not available | Full CSS border support | ❌ Missing |
| Gutter icons | ❌ Not available | `gutterIconPath` in decoration | ❌ Missing |
| Outline styling | ❌ Not available | `outline` in decoration | ❌ Missing |
| Before/after pseudo content | ❌ Not available | `before`/`after` decoration | ❌ Missing |
| Hover message on decoration | ❌ Not available | `hoverMessage` in decoration | ❌ Missing |

**Gap Analysis:**
- Fresh has basic RGB highlighting only
- No gutter icons for error/warning markers
- No pseudo-content (inline badges, counters)
- No hover information on decorations
- Limited styling options compared to full CSS

---

### 7. File System Operations

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Read file | `editor.readFile(path)` (async) | `vscode.workspace.fs.readFile()` | ✅ Equivalent |
| Write file | `editor.writeFile(path, content)` (async) | `vscode.workspace.fs.writeFile()` | ✅ Equivalent |
| File exists | `editor.fileExists(path)` | `vscode.workspace.fs.stat()` | ✅ Equivalent |
| File stat | `editor.fileStat(path)` | `vscode.workspace.fs.stat()` | ✅ Equivalent |
| Read directory | `editor.readDir(path)` | `vscode.workspace.fs.readDirectory()` | ✅ Equivalent |
| Delete file | ❌ Not available | `vscode.workspace.fs.delete()` | ❌ Missing |
| Rename/move file | ❌ Not available | `vscode.workspace.fs.rename()` | ❌ Missing |
| Create directory | ❌ Not available | `vscode.workspace.fs.createDirectory()` | ❌ Missing |
| Copy file | ❌ Not available | `vscode.workspace.fs.copy()` | ❌ Missing |
| Find files (glob) | ❌ Not available | `vscode.workspace.findFiles()` | ❌ Missing |
| File watcher | ❌ Not available | `vscode.workspace.createFileSystemWatcher()` | ❌ Missing |
| Save all files | ❌ Not available | `vscode.workspace.saveAll()` | ❌ Missing |

**Gap Analysis:**
- Fresh lacks file modification operations (delete, rename, copy)
- No glob-based file search
- No file system watchers for real-time updates
- No programmatic save-all functionality

---

### 8. Path & Environment

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Get environment variable | `editor.getEnv(name)` | `process.env[name]` | ✅ Equivalent |
| Get working directory | `editor.getCwd()` | `vscode.workspace.workspaceFolders[0].uri.fsPath` | ✅ Equivalent |
| Path join | `editor.pathJoin(...parts)` | Node.js `path.join()` | ✅ Equivalent |
| Path dirname | `editor.pathDirname(path)` | Node.js `path.dirname()` | ✅ Equivalent |
| Path basename | `editor.pathBasename(path)` | Node.js `path.basename()` | ✅ Equivalent |
| Path extension | `editor.pathExtname(path)` | Node.js `path.extname()` | ✅ Equivalent |
| Is absolute path | `editor.pathIsAbsolute(path)` | Node.js `path.isAbsolute()` | ✅ Equivalent |
| Path resolve | ❌ Not available | Node.js `path.resolve()` | ❌ Missing |
| Path relative | ❌ Not available | Node.js `path.relative()` | ❌ Missing |
| Path normalize | ❌ Not available | Node.js `path.normalize()` | ❌ Missing |
| Clipboard access | ❌ Not available | `vscode.env.clipboard` | ❌ Missing |
| App URI handler | ❌ Not available | `vscode.window.registerUriHandler()` | ❌ Missing |

**Gap Analysis:**
- Good path utility coverage
- Missing clipboard access
- No URI scheme handling

---

### 9. Event System

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Register event handler | `editor.on(eventName, handlerName)` | `vscode.workspace.onDidX(callback)` | ✅ Equivalent |
| Unregister handler | `editor.off(eventName, handlerName)` | `disposable.dispose()` | ✅ Equivalent |
| List handlers | `editor.getHandlers(eventName)` | ❌ Not available | ✅ Better |
| File open event | `after_file_open` | `onDidOpenTextDocument` | ✅ Equivalent |
| File save event | `after_file_save` | `onDidSaveTextDocument` | ✅ Equivalent |
| Buffer close event | `buffer_closed` | `onDidCloseTextDocument` | ✅ Equivalent |
| Text change event | `after_insert`, `after_delete` | `onDidChangeTextDocument` | ✅ Equivalent |
| Cursor move event | `cursor_moved` | `onDidChangeTextEditorSelection` | ✅ Equivalent |
| Buffer activate event | `buffer_activated` | `onDidChangeActiveTextEditor` | ✅ Equivalent |
| Before save hook | `before_file_save` | `onWillSaveTextDocument` | ✅ Equivalent |
| Render line hook | `render_line` | ❌ Not available (use decorations) | ✅ Better |
| Pre/post command hooks | `pre_command`, `post_command` | ❌ Not available | ✅ Better |
| Idle event | `idle` | ❌ Not available | ✅ Better |
| Configuration change | ❌ Not available | `onDidChangeConfiguration` | ❌ Missing |
| Workspace folder change | ❌ Not available | `onDidChangeWorkspaceFolders` | ❌ Missing |
| Extension activate/deactivate | ❌ Not available | `activate()` / `deactivate()` | ❌ Missing |
| Terminal events | ❌ Not available | `onDidOpenTerminal`, `onDidCloseTerminal` | ❌ Missing |

**Gap Analysis:**
- Fresh has excellent hook coverage with unique features (render_line, pre/post command, idle)
- Missing configuration and workspace structure events
- No extension lifecycle hooks (activate/deactivate pattern)

---

### 10. External Process Execution

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Spawn process | `editor.spawnProcess(cmd, args, cwd)` (async) | Node.js `child_process` | ✅ Equivalent |
| Capture stdout/stderr | Returns `SpawnResult` with stdout, stderr, exit_code | Full stream access | ✅ Equivalent |
| Kill process | ❌ Not available | `process.kill()` | ❌ Missing |
| Process events | ❌ Not available | Stream events | ❌ Missing |
| Shell execution task | ❌ Not available | `vscode.tasks.registerTaskProvider()` | ❌ Missing |
| Process execution task | ❌ Not available | `vscode.tasks.executeTask()` | ❌ Missing |

**Gap Analysis:**
- Fresh has good basic process spawning
- No streaming stdout/stderr (waits for completion)
- No process lifecycle management
- No task system integration

---

### 11. Language Services (IntelliSense)

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Completion provider | ❌ Not available | `languages.registerCompletionItemProvider()` | ❌ Missing |
| Hover provider | ❌ Not available | `languages.registerHoverProvider()` | ❌ Missing |
| Definition provider | ❌ Not available | `languages.registerDefinitionProvider()` | ❌ Missing |
| Reference provider | ❌ Not available | `languages.registerReferenceProvider()` | ❌ Missing |
| Signature help | ❌ Not available | `languages.registerSignatureHelpProvider()` | ❌ Missing |
| Code actions | ❌ Not available | `languages.registerCodeActionsProvider()` | ❌ Missing |
| Code lens | ❌ Not available | `languages.registerCodeLensProvider()` | ❌ Missing |
| Document symbols | ❌ Not available | `languages.registerDocumentSymbolProvider()` | ❌ Missing |
| Workspace symbols | ❌ Not available | `languages.registerWorkspaceSymbolProvider()` | ❌ Missing |
| Document formatting | ❌ Not available | `languages.registerDocumentFormattingEditProvider()` | ❌ Missing |
| Range formatting | ❌ Not available | `languages.registerDocumentRangeFormattingEditProvider()` | ❌ Missing |
| Rename provider | ❌ Not available | `languages.registerRenameProvider()` | ❌ Missing |
| Document links | ❌ Not available | `languages.registerDocumentLinkProvider()` | ❌ Missing |
| Folding range | ❌ Not available | `languages.registerFoldingRangeProvider()` | ❌ Missing |
| Color provider | ❌ Not available | `languages.registerDocumentColorProvider()` | ❌ Missing |
| Diagnostic collection | ❌ Not available | `languages.createDiagnosticCollection()` | ❌ Missing |
| Language configuration | ❌ Not available | `languages.setLanguageConfiguration()` | ❌ Missing |
| Semantic tokens | ❌ Not available | `languages.registerDocumentSemanticTokensProvider()` | ❌ Missing |

**Gap Analysis:**
- **This is the largest gap.** Fresh has no language service provider infrastructure.
- No IntelliSense/autocomplete
- No hover documentation
- No go-to-definition
- No code actions/quick fixes
- No diagnostics/error reporting
- No semantic highlighting
- Fresh relies on external LSP servers (not plugin-based)

---

### 12. Source Control (Git)

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| SCM provider | ❌ Not available | `vscode.scm.createSourceControl()` | ❌ Missing |
| SCM resource groups | ❌ Not available | `sourceControl.createResourceGroup()` | ❌ Missing |
| Git status integration | Can call `git` via spawnProcess | Native SCM API | ⚠️ Manual only |
| Diff view | ❌ Not available | `vscode.commands.executeCommand('vscode.diff')` | ❌ Missing |
| Git decorations | ❌ Not available | SCM decorations | ❌ Missing |

**Gap Analysis:**
- Fresh has no source control management abstraction
- Git integration requires manual process spawning
- No visual git status indicators

---

### 13. Debugging

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Start debug session | ❌ Not available | `vscode.debug.startDebugging()` | ❌ Missing |
| Stop debug session | ❌ Not available | `vscode.debug.stopDebugging()` | ❌ Missing |
| Add breakpoints | ❌ Not available | `vscode.debug.addBreakpoints()` | ❌ Missing |
| Remove breakpoints | ❌ Not available | `vscode.debug.removeBreakpoints()` | ❌ Missing |
| Debug configuration provider | ❌ Not available | `vscode.debug.registerDebugConfigurationProvider()` | ❌ Missing |
| Debug adapter factory | ❌ Not available | `vscode.debug.registerDebugAdapterDescriptorFactory()` | ❌ Missing |
| Active debug session | ❌ Not available | `vscode.debug.activeDebugSession` | ❌ Missing |
| Debug console | ❌ Not available | Debug console API | ❌ Missing |

**Gap Analysis:**
- Fresh has no debugging infrastructure whatsoever
- No breakpoint management
- No debug adapter protocol support
- Complete feature gap

---

### 14. Testing

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Test controller | ❌ Not available | `vscode.tests.createTestController()` | ❌ Missing |
| Test items | ❌ Not available | `controller.createTestItem()` | ❌ Missing |
| Test run profiles | ❌ Not available | `controller.createRunProfile()` | ❌ Missing |
| Test results | ❌ Not available | Test result API | ❌ Missing |

**Gap Analysis:**
- Fresh has no test framework integration
- No test discovery or execution
- No test result visualization

---

### 15. Configuration

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Get configuration | ❌ Not available | `vscode.workspace.getConfiguration()` | ❌ Missing |
| Update configuration | ❌ Not available | `configuration.update()` | ❌ Missing |
| Inspect configuration | ❌ Not available | `configuration.inspect()` | ❌ Missing |
| Configuration change event | ❌ Not available | `onDidChangeConfiguration` | ❌ Missing |

**Gap Analysis:**
- Fresh has no plugin configuration system
- No user/workspace settings per plugin
- No settings UI contribution

---

### 16. Virtual Buffers & Custom UI

| Feature | Fresh API | VSCode API | Parity |
|---------|-----------|------------|--------|
| Create virtual buffer | `editor.createVirtualBufferInSplit()` | Webview or custom editor | ⚠️ Different approach |
| Define custom mode | `editor.defineMode()` | N/A (uses webview) | ✅ Better |
| Set virtual buffer content | `editor.setVirtualBufferContent()` | Webview postMessage | ✅ Equivalent |
| Get text properties at cursor | `editor.getTextPropertiesAtCursor()` | N/A (Fresh-specific) | ✅ Better |
| Show buffer | `editor.showBuffer()` | `vscode.window.showTextDocument()` | ✅ Equivalent |

**Gap Analysis:**
- Fresh has unique virtual buffer with embedded properties - useful for diagnostics panels, search results
- VSCode uses webviews (HTML/CSS/JS) for custom UI - more flexible but more complex
- Fresh's approach is more integrated with text editing

---

## Summary: Top 10 Critical Feature Gaps

### 1. **Language Services Infrastructure** (Critical)
- No IntelliSense providers
- No hover/definition/reference providers
- No code actions or quick fixes
- No diagnostic collections
- **Impact**: Cannot create language support plugins

### 2. **Cursor/Selection Write Operations** (High)
- Cannot set cursor position programmatically
- Cannot modify selections
- Cannot scroll to location
- **Impact**: Cannot create navigation or refactoring plugins

### 3. **UI Dialogs & User Interaction** (High)
- No information/warning/error dialogs
- No progress indicators
- No output channels
- **Impact**: Limited user feedback mechanisms

### 4. **Tree View & Custom UI** (High)
- No tree data providers
- No webview panels
- No custom sidebar views
- **Impact**: Cannot create file explorers, outline views, etc.

### 5. **Debugging Infrastructure** (Medium-High)
- No debug session management
- No breakpoint control
- No debug adapter support
- **Impact**: Cannot create debugger extensions

### 6. **Workspace-Wide Operations** (Medium)
- No workspace edit (multi-file refactoring)
- No file watchers
- No glob file search
- **Impact**: Cannot create refactoring tools

### 7. **Source Control Management** (Medium)
- No SCM provider abstraction
- No git status decorations
- No diff view support
- **Impact**: Manual git via process spawning only

### 8. **Testing Framework** (Medium)
- No test controller
- No test discovery
- No test result visualization
- **Impact**: Cannot create test runners

### 9. **Configuration System** (Medium)
- No plugin settings API
- No workspace configuration
- No settings change events
- **Impact**: Plugins cannot have user-configurable options

### 10. **Terminal Integration** (Low-Medium)
- No integrated terminal creation
- No terminal send/receive
- **Impact**: Must use external terminals

---

## Fresh's Unique Strengths

Despite the gaps, Fresh has some advantages:

1. **Render Line Hook** - Direct access to line rendering for syntax highlighting
2. **Pre/Post Command Hooks** - Intercept any command execution
3. **Idle Event** - React to editor inactivity
4. **Virtual Buffers with Properties** - Rich metadata attached to text
5. **Mode Definition** - Custom keybinding modes per plugin
6. **Overlay Prefix Management** - Bulk overlay operations
7. **Simpler API** - Single object vs 15+ namespaces
8. **Native Async/Await** - Modern JavaScript patterns
9. **Deno Runtime** - TypeScript transpilation built-in

---

## Recommended Priority for Closing Gaps

### Phase 1: Foundation (Essential for Basic Plugin Ecosystem)
1. **Cursor/Selection Write** - `setCursorPosition()`, `setSelections()`, `revealRange()`
2. **Basic Dialogs** - `showMessage()`, `showInputDialog()`
3. **Command Execution** - `executeCommand()`, `getCommands()`
4. **File Operations** - `deleteFile()`, `renameFile()`, `findFiles()`

### Phase 2: Language Services (For Code Intelligence)
1. **Diagnostic Collection** - Report errors/warnings to editor
2. **Completion Provider** - IntelliSense/autocomplete
3. **Hover Provider** - Documentation on hover
4. **Definition Provider** - Go-to-definition

### Phase 3: Advanced UI (For Rich Extensions)
1. **Tree View Provider** - Hierarchical data UI
2. **Progress Indicator** - Long-running task feedback
3. **Output Channel** - Streaming log output
4. **Webview Panels** - Custom HTML/JS UI

### Phase 4: Development Tools (For Full IDE Experience)
1. **Debug Adapter Protocol** - Debugging support
2. **Test Controller** - Test framework integration
3. **SCM Provider** - Git integration
4. **Task Provider** - Build task system

---

## Conclusion

Fresh's plugin API provides solid foundational capabilities (~25-30% of VSCode's features), particularly strong in:
- Buffer querying and basic mutations
- Event/hook system
- File system operations
- Process spawning
- Visual overlays

The critical gaps are:
- **Language services** (no IntelliSense infrastructure)
- **Cursor/selection manipulation** (read-only)
- **Rich UI components** (no dialogs, trees, webviews)
- **Developer tooling** (no debug, test, SCM)

To achieve feature parity with VSCode's core extension capabilities, Fresh would need to implement approximately **200+ additional API methods** across **10+ new feature categories**, with language services and cursor manipulation being the highest priority gaps.
