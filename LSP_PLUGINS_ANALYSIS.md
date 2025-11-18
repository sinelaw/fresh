# LSP-Related Plugins Analysis Report

## Executive Summary

This analysis examines 6 LSP-related plugins in the codebase to identify common patterns for extraction into a shared library. The plugins show significant code duplication in state management, split/buffer handling, navigation, and UI patterns.

---

## 1. LSP-Related Plugins Found

### Plugin Inventory

| Plugin | File | Size | Purpose |
|--------|------|------|---------|
| Git Log | `/home/user/fresh/plugins/git_log.ts` | 25,087 bytes | Magit-style git log viewer with commit details |
| Diagnostics Panel | `/home/user/fresh/plugins/diagnostics_panel.ts` | 8,821 bytes | LSP diagnostics display with navigation |
| Find References | `/home/user/fresh/plugins/find_references.ts` | 10,960 bytes | LSP find references results display |
| Git Grep | `/home/user/fresh/plugins/git_grep.ts` | 5,068 bytes | Interactive git grep search |
| Git Find File | `/home/user/fresh/plugins/git_find_file.ts` | 8,413 bytes | Fuzzy file finder with prompt interface |
| TODO Highlighter | `/home/user/fresh/plugins/todo_highlighter.ts` | 5,645 bytes | Keyword highlighting with overlays |

**Total Plugin Code: ~63.9 KB**

---

## 2. Common Patterns Identified

### 2.1 State Management Pattern

All plugins use a similar state management approach with module-level variables:

#### Git Log Pattern (lines 59-79):
```typescript
const gitLogState: GitLogState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  sourceSplitId: null,
  sourceBufferId: null,
  commits: [],
  selectedIndex: 0,
  options: { ... }
};

const commitDetailState: GitCommitDetailState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  commit: null,
};
```

#### Diagnostics Panel Pattern (lines 10-15):
```typescript
let panelOpen = false;
let diagnosticsBufferId: number | null = null;
let sourceSplitId: number | null = null;
let currentDiagnostics: DiagnosticItem[] = [];
let selectedIndex = 0;
```

#### Find References Pattern (lines 11-21):
```typescript
let panelOpen = false;
let referencesBufferId: number | null = null;
let sourceSplitId: number | null = null;
let referencesSplitId: number | null = null;
let currentReferences: ReferenceItem[] = [];
let currentSymbol: string = "";
let lineCache: Map<string, string[]> = new Map();
```

**Pattern Summary:**
- Module-level state variables for UI state (isOpen, bufferId, splitId)
- Separate state for source context (sourceSplitId, sourceBufferId)
- Data arrays/lists for display content
- Selected/active index for navigation
- Configuration objects for options

---

### 2.2 Split View Management Pattern

All panel-based plugins follow the same split management workflow:

#### Workflow (Git Log: lines 611-662):
```typescript
// 1. Save the current split context BEFORE creating panel
gitLogState.sourceSplitId = editor.getActiveSplitId();
gitLogState.sourceBufferId = editor.getActiveBufferId();

// 2. Create virtual buffer in split
const bufferId = await editor.createVirtualBufferInSplit({
  name: "*Git Log*",
  mode: "git-log",
  read_only: true,
  entries: entries,
  ratio: 0.6,           // Original takes 60%, new takes 40%
  panel_id: "git-log-panel",
  show_line_numbers: false,
  show_cursors: true,
  editing_disabled: true,
});

// 3. Track the IDs
gitLogState.isOpen = true;
gitLogState.bufferId = bufferId;
gitLogState.splitId = editor.getActiveSplitId();

// 4. Apply highlighting
applyGitLogHighlighting();
```

#### Closing Pattern (Git Log: lines 665-688):
```typescript
globalThis.git_log_close = function(): void {
  if (!gitLogState.isOpen) return;

  // 1. Close buffer
  if (gitLogState.bufferId !== null) {
    editor.closeBuffer(gitLogState.bufferId);
  }

  // 2. Close split
  if (gitLogState.splitId !== null) {
    editor.closeSplit(gitLogState.splitId);
  }

  // 3. Reset state
  gitLogState.isOpen = false;
  gitLogState.bufferId = null;
  gitLogState.splitId = null;
  gitLogState.sourceSplitId = null;
  gitLogState.sourceBufferId = null;
  gitLogState.commits = [];
  gitLogState.selectedIndex = 0;
};
```

**Identical Pattern in Diagnostics (lines 189-201):**
```typescript
globalThis.hide_diagnostics_panel = function (): void {
  if (!panelOpen) return;
  panelOpen = false;
  diagnosticsBufferId = null;
  sourceSplitId = null;
  selectedIndex = 0;
  currentDiagnostics = [];
  editor.setStatus("Diagnostics panel closed");
};
```

**Candidate for Shared Library:** PanelManager utility class

---

### 2.3 Buffer Entry Building Pattern

All plugins construct TextPropertyEntry arrays for virtual buffers:

#### Git Log Pattern (lines 236-282):
```typescript
function buildGitLogEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "Commits:\n",
    properties: { type: "section-header" },
  });

  if (gitLogState.commits.length === 0) {
    entries.push({
      text: "  No commits found\n",
      properties: { type: "empty" },
    });
  } else {
    for (let i = 0; i < gitLogState.commits.length; i++) {
      const commit = gitLogState.commits[i];
      entries.push({
        text: formatCommitRow(commit, i),
        properties: {
          type: "commit",
          index: i,
          hash: commit.hash,
          shortHash: commit.shortHash,
          author: commit.author,
          date: commit.relativeDate,
          subject: commit.subject,
          refs: commit.refs,
        },
      });
    }
  }

  // Footer
  entries.push({ text: "\n", properties: { type: "blank" } });
  entries.push({ 
    text: `${gitLogState.commits.length} commits | ...`,
    properties: { type: "footer" }
  });

  return entries;
}
```

#### Diagnostics Pattern (lines 58-105):
```typescript
function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "═══ LSP Diagnostics ═══\n",
    properties: { type: "header" },
  });

  if (currentDiagnostics.length === 0) {
    entries.push({
      text: "  No diagnostics available\n",
      properties: { type: "empty" },
    });
  } else {
    for (let i = 0; i < currentDiagnostics.length; i++) {
      const diag = currentDiagnostics[i];
      entries.push({
        text: formatDiagnostic(diag, i),
        properties: { type: "diagnostic", index: i, ... },
      });
    }
  }

  // Footer
  entries.push({
    text: `───────────────────────\n`,
    properties: { type: "separator" },
  });
  entries.push({
    text: `Total: ${errorCount} error(s), ...`,
    properties: { type: "summary" },
  });

  return entries;
}
```

**Pattern Summary:**
- Header entry with type metadata
- Empty state entry if no items
- Items loop with index tracking
- Footer with help text or summary
- Each entry has `text` and `properties` with at least `type`

**Candidate for Shared Library:** PanelBuilder or ListBuilder base class

---

### 2.4 Syntax Highlighting / Overlay Pattern

Plugins apply syntax highlighting via overlay API:

#### Git Log Pattern (lines 284-394):
```typescript
function applyGitLogHighlighting(): void {
  if (gitLogState.bufferId === null) return;

  const bufferId = gitLogState.bufferId;

  // 1. Clear existing overlays
  editor.removeOverlaysByPrefix(bufferId, "gitlog-");

  // 2. Get buffer content
  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);
  const lines = content.split("\n");

  // 3. Iterate lines and apply highlighting
  let byteOffset = 0;
  const headerLines = 1;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];

    // Highlight specific line types
    if (line === "Commits:") {
      editor.addOverlay(
        bufferId,
        `gitlog-section-${lineIdx}`,
        byteOffset,
        byteOffset + line.length,
        colors.header[0], colors.header[1], colors.header[2],
        true // underline
      );
    }

    // Highlight hash
    const hashStart = byteOffset + pos;
    const hashEnd = hashStart + commit.shortHash.length;
    editor.addOverlay(
      bufferId,
      `gitlog-hash-${lineIdx}`,
      hashStart,
      hashEnd,
      colors.hash[0], colors.hash[1], colors.hash[2],
      false
    );

    byteOffset += line.length + 1;
  }
}
```

#### Commit Detail Pattern (lines 500-605):
```typescript
function applyCommitDetailHighlighting(): void {
  if (commitDetailState.bufferId === null) return;

  const bufferId = commitDetailState.bufferId;
  editor.removeOverlaysByPrefix(bufferId, "gitdetail-");

  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);
  const lines = content.split("\n");

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight diff additions
    if (line.startsWith("+") && !line.startsWith("+++")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-add-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.diffAdd[0], colors.diffAdd[1], colors.diffAdd[2],
        false
      );
    }
    // Highlight diff deletions
    else if (line.startsWith("-") && !line.startsWith("---")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-del-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.diffDel[0], colors.diffDel[1], colors.diffDel[2],
        false
      );
    }
    
    byteOffset += line.length + 1;
  }
}
```

#### TODO Highlighter Pattern (lines 31-88):
```typescript
function highlightLine(
  bufferId: number,
  lineNumber: number,
  byteStart: number,
  content: string
): void {
  // Search for keywords
  for (const keyword of config.keywords) {
    let searchStart = 0;
    while (true) {
      const pos = content.indexOf(keyword.word, searchStart);
      if (pos === -1) break;

      // Check whole word
      const isWordStart = pos === 0 || !/\w/.test(content[pos - 1]);
      const isWordEnd = pos + keyword.word.length >= content.length ||
                        !/\w/.test(content[pos + keyword.word.length]);

      if (isWordStart && isWordEnd) {
        const absoluteStart = byteStart + pos;
        const absoluteEnd = absoluteStart + keyword.word.length;
        const overlayId = `todo-${bufferId}-${lineNumber}-${pos}`;

        editor.addOverlay(
          bufferId,
          overlayId,
          absoluteStart,
          absoluteEnd,
          keyword.color[0],
          keyword.color[1],
          keyword.color[2],
          false
        );
      }
      searchStart = pos + 1;
    }
  }
}
```

**Pattern Summary:**
- Clear existing overlays with prefix
- Get buffer content
- Iterate through lines tracking byte offset
- Apply overlays based on patterns
- Use consistent naming convention for overlay IDs

**Candidate for Shared Library:** HighlightingHelper, OverlayManager

---

### 2.5 Navigation Pattern

All plugins implement similar navigation commands:

#### Git Log Pattern (lines 690-723):
```typescript
globalThis.git_log_next = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = Math.min(
    gitLogState.selectedIndex + 1,
    gitLogState.commits.length - 1
  );
  updateGitLogView();
  editor.setStatus(`Commit ${gitLogState.selectedIndex + 1}/${gitLogState.commits.length}`);
};

globalThis.git_log_prev = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = Math.max(gitLogState.selectedIndex - 1, 0);
  updateGitLogView();
  editor.setStatus(`Commit ${gitLogState.selectedIndex + 1}/${gitLogState.commits.length}`);
};

globalThis.git_log_first = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = 0;
  updateGitLogView();
  editor.setStatus(`Commit 1/${gitLogState.commits.length}`);
};

globalThis.git_log_last = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = gitLogState.commits.length - 1;
  updateGitLogView();
  editor.setStatus(`Commit ${gitLogState.commits.length}/${gitLogState.commits.length}`);
};
```

#### Diagnostics Pattern (lines 254-270):
```typescript
globalThis.diagnostics_next = function (): void {
  if (currentDiagnostics.length === 0) return;

  selectedIndex = (selectedIndex + 1) % currentDiagnostics.length;
  updatePanelContent();
  editor.setStatus(`Diagnostic ${selectedIndex + 1}/${currentDiagnostics.length}`);
};

globalThis.diagnostics_prev = function (): void {
  if (currentDiagnostics.length === 0) return;

  selectedIndex = selectedIndex > 0 ? selectedIndex - 1 : currentDiagnostics.length - 1;
  updatePanelContent();
  editor.setStatus(`Diagnostic ${selectedIndex + 1}/${currentDiagnostics.length}`);
};
```

**Pattern Summary:**
- Check if panel is open and has items
- Update selectedIndex with bounds checking
- Call updateView() to refresh display
- Set status with "X/Y" format

**Candidate for Shared Library:** NavigationController class

---

### 2.6 Mode and Keybinding Pattern

All plugins define buffer modes with keybindings:

#### Git Log Pattern (lines 107-146):
```typescript
editor.defineMode(
  "git-log",
  null, // no parent mode
  [
    ["Return", "git_log_show_commit"],
    ["Tab", "git_log_show_commit"],
    ["j", "git_log_next"],
    ["k", "git_log_prev"],
    ["n", "git_log_next"],
    ["p", "git_log_prev"],
    ["Down", "git_log_next"],
    ["Up", "git_log_prev"],
    ["g", "git_log_first"],
    ["M-<", "git_log_first"],
    ["G", "git_log_last"],
    ["M->", "git_log_last"],
    ["q", "git_log_close"],
    ["Escape", "git_log_close"],
    ["r", "git_log_refresh"],
    ["y", "git_log_copy_hash"],
  ],
  true // read-only
);
```

#### Diagnostics Pattern (lines 35-48):
```typescript
editor.defineMode(
  "diagnostics-list",
  null,
  [
    ["Return", "diagnostics_goto"],
    ["n", "diagnostics_next"],
    ["p", "diagnostics_prev"],
    ["j", "diagnostics_next"],
    ["k", "diagnostics_prev"],
    ["q", "diagnostics_close"],
    ["Escape", "diagnostics_close"],
  ],
  true // read-only
);
```

**Pattern Summary:**
- One call to defineMode() per plugin
- Mode name follows kebab-case convention
- Most modes are read-only (true)
- Common keybindings: j/k/n/p for navigation, Return to confirm, q/Escape to close

**Candidate for Shared Library:** ModeFactory with preset bindings

---

### 2.7 Event Handling and Subscriptions

Plugins use event registration for dynamic interactions:

#### Git Grep Pattern (lines 173-175):
```typescript
editor.on("prompt_changed", "onGitGrepPromptChanged");
editor.on("prompt_confirmed", "onGitGrepPromptConfirmed");
editor.on("prompt_cancelled", "onGitGrepPromptCancelled");
```

#### Find References Pattern (lines 236, 263):
```typescript
editor.on("lsp_references", "on_lsp_references");
editor.on("cursor_moved", "on_references_cursor_moved");
```

#### TODO Highlighter Pattern (lines 128-132):
```typescript
editor.on("render_start", "onRenderStart");
editor.on("render_line", "onRenderLine");
editor.on("after-insert", "onAfterInsert");
editor.on("after-delete", "onAfterDelete");
editor.on("buffer_closed", "onBufferClosed");
```

#### Git Grep Handler Pattern (lines 75-123):
```typescript
globalThis.onGitGrepPromptChanged = function(args: {
  prompt_type: string;
  input: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true; // Not our prompt
  }

  const query = args.input;
  if (!query || query.trim() === "") {
    editor.setPromptSuggestions([]);
    return true;
  }

  // Spawn process asynchronously
  editor.spawnProcess("git", ["grep", "-n", "--column", "-I", "--", query])
    .then((result) => {
      if (result.exit_code === 0) {
        const { results, suggestions } = parseGitGrepOutput(result.stdout);
        gitGrepResults = results;
        editor.setPromptSuggestions(suggestions);
        editor.setStatus(`Found ${results.length} matches`);
      } else if (result.exit_code === 1) {
        gitGrepResults = [];
        editor.setPromptSuggestions([]);
        editor.setStatus("No matches found");
      } else {
        editor.setStatus(`Git grep error: ${result.stderr}`);
      }
    })
    .catch((e) => {
      editor.setStatus(`Git grep error: ${e}`);
    });

  return true;
};
```

**Pattern Summary:**
- Event handlers defined as global functions
- Type guard checking for event type/prompt_type
- Early return pattern (return true for other handlers to process)
- Async operations with .then() and .catch()
- Status updates for user feedback

**Candidate for Shared Library:** EventSubscription manager

---

### 2.8 File Navigation Pattern

Plugins that jump to files share common patterns:

#### Diagnostics Pattern (lines 220-252):
```typescript
globalThis.diagnostics_goto = function (): void {
  if (currentDiagnostics.length === 0) {
    editor.setStatus("No diagnostics to jump to");
    return;
  }

  if (sourceSplitId === null) {
    editor.setStatus("Source split not available");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0) {
    const location = props[0].location as { file: string; line: number; column: number } | undefined;
    if (location) {
      // Open file in the source split, not the diagnostics split
      editor.openFileInSplit(sourceSplitId, location.file, location.line, location.column || 0);
      editor.setStatus(`Jumped to ${location.file}:${location.line}`);
    } else {
      editor.setStatus("No location info for this diagnostic");
    }
  } else {
    // Fallback: use selectedIndex
    const diag = currentDiagnostics[selectedIndex];
    if (diag) {
      editor.openFileInSplit(sourceSplitId, diag.file, diag.line, diag.column);
      editor.setStatus(`Jumped to ${diag.file}:${diag.line}`);
    }
  }
};
```

#### Find References Pattern (lines 290-334):
```typescript
globalThis.references_goto = function (): void {
  if (currentReferences.length === 0) {
    editor.setStatus("No references to jump to");
    return;
  }

  if (sourceSplitId === null) {
    editor.setStatus("Source split not available");
    return;
  }

  if (referencesBufferId === null) {
    return;
  }

  const props = editor.getTextPropertiesAtCursor(referencesBufferId);
  editor.debug(`references_goto: props.length=${props.length}`);

  if (props.length > 0) {
    const location = props[0].location as
      | { file: string; line: number; column: number }
      | undefined;
    if (location) {
      editor.openFileInSplit(
        sourceSplitId,
        location.file,
        location.line,
        location.column || 0
      );
      const displayPath = getRelativePath(location.file);
      editor.setStatus(`Jumped to ${displayPath}:${location.line}`);
    } else {
      editor.setStatus("Move cursor to a reference line");
    }
  } else {
    editor.setStatus("Move cursor to a reference line");
  }
};
```

**Pattern Summary:**
- Check prerequisites (data exists, sourceSplitId is set)
- Get text properties at cursor
- Extract location from properties
- Call openFileInSplit() with location
- Display relative path in status message

**Candidate for Shared Library:** LocationNavigator utility

---

## 3. Duplicated Functionality Analysis

### 3.1 Exact Code Duplication

#### Panel Open/Close State Management
- **Git Log** (lines 59-79, 665-688): State object + close function
- **Diagnostics** (lines 10-15, 189-201): Module variables + hide function
- **Find References** (lines 11-21, 266-288): Module variables + hide function

**Duplication Score:** 95% similar

#### Virtual Buffer Creation
- **Git Log** (lines 637-647): createVirtualBufferInSplit
- **Diagnostics** (lines 167-176): createVirtualBufferInSplit
- **Find References** (lines 190-199): createVirtualBufferInSplit

**Duplication Score:** 90% similar - only differences are name, mode, ratio, panel_id

#### Navigation Controller
- **Git Log** (690-723): 4 functions (next, prev, first, last)
- **Diagnostics** (254-270): 2 functions (next, prev)
- **Find References**: Uses cursor navigation naturally

**Duplication Score:** 80% similar

### 3.2 Conceptual Duplication

#### Text Property Entry Building
- **Git Log** (236-282): buildGitLogEntries()
- **Diagnostics** (58-105): buildPanelEntries()
- **Find References** (76-123): buildPanelEntries()
- **Commit Detail** (408-498): buildCommitDetailEntries()

**Pattern Similarity:** 85% - All follow header > empty/items > footer structure

#### Syntax Highlighting
- **Git Log** (284-394): applyGitLogHighlighting()
- **Commit Detail** (500-605): applyCommitDetailHighlighting()
- **TODO Highlighter** (31-82): highlightLine()

**Pattern Similarity:** 75% - Different implementations but similar workflow

#### Command Registration
All plugins use same pattern:
```typescript
editor.registerCommand(name, description, action, context);
```

**Duplication Score:** 100% for the API pattern

---

## 4. Shared Library Recommendations

### 4.1 Recommended Shared Utilities

#### 1. **PanelManager** - State and Lifecycle Management
```typescript
interface PanelConfig {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceSplitId: number | null;
  sourceBufferId: number | null;
  selectedIndex: number;
}

class PanelManager {
  open(config: PanelConfig): void;
  close(): void;
  updateSelectedIndex(newIndex: number): void;
  reset(): void;
}
```

**Current Locations of Duplication:**
- git_log.ts lines 59-79, 611-662, 665-688
- diagnostics_panel.ts lines 10-15, 147-186, 189-201
- find_references.ts lines 11-21, 166-217, 266-288

**Estimated Code Saved:** ~150 lines

---

#### 2. **VirtualBufferFactory** - Buffer Creation Helper
```typescript
interface BufferOptions {
  name: string;
  mode: string;
  entries: TextPropertyEntry[];
  ratio?: number;
  panelId?: string;
}

class VirtualBufferFactory {
  createInSplit(options: BufferOptions): Promise<number>;
  createInExistingSplit(options: BufferOptions, splitId: number): Promise<number>;
}
```

**Current Locations:**
- git_log.ts lines 637-647
- diagnostics_panel.ts lines 167-176
- find_references.ts lines 190-199

**Estimated Code Saved:** ~80 lines

---

#### 3. **NavigationController** - Cursor and Selection Management
```typescript
class NavigationController<T> {
  constructor(items: T[], onUpdate: () => void);
  
  next(): void;
  prev(): void;
  first(): void;
  last(): void;
  jumpTo(index: number): void;
  
  get selectedIndex(): number;
  get selected(): T | null;
  get count(): number;
}
```

**Current Locations:**
- git_log.ts lines 690-723
- diagnostics_panel.ts lines 254-270

**Estimated Code Saved:** ~60 lines

---

#### 4. **HighlightingHelper** - Overlay Management
```typescript
class HighlightingHelper {
  clearHighlights(bufferId: number, prefix: string): void;
  applyLineHighlight(bufferId: number, line: string, patterns: HighlightPattern[]): void;
  applyByteRangeHighlight(bufferId: number, overlayId: string, 
                          start: number, end: number, rgb: [number, number, number]): void;
}

interface HighlightPattern {
  match: (line: string) => boolean;
  rgb: [number, number, number];
  underline?: boolean;
}
```

**Current Locations:**
- git_log.ts lines 284-394 (800+ lines with highlighting)
- diagnostics_panel.ts lines ~50-150 (minimal highlighting)
- todo_highlighter.ts lines 31-88

**Estimated Code Saved:** ~200 lines

---

#### 5. **LocationNavigator** - File Navigation
```typescript
class LocationNavigator {
  jumpToLocation(location: Location, sourceSplitId: number): void;
  getLocationFromTextProperties(props: Record<string, unknown>[]): Location | null;
}

interface Location {
  file: string;
  line: number;
  column: number;
}
```

**Current Locations:**
- diagnostics_panel.ts lines 220-252
- find_references.ts lines 290-334
- git_grep.ts lines 125-155

**Estimated Code Saved:** ~70 lines

---

#### 6. **ModeBuilder** - Mode and Keybinding Configuration
```typescript
class ModeBuilder {
  defineMode(name: string, keybindings: [string, string][]): ModeBuilder;
  addBinding(key: string, command: string): ModeBuilder;
  setReadOnly(readonly: boolean): ModeBuilder;
  build(): void;
}

// Preset bindings
export const COMMON_BINDINGS = {
  navigation: [
    ["j", "next"],
    ["k", "prev"],
    ["n", "next"],
    ["p", "prev"],
  ],
  dismiss: [
    ["q", "close"],
    ["Escape", "close"],
  ],
};
```

**Current Locations:**
- git_log.ts lines 107-146
- diagnostics_panel.ts lines 35-48
- find_references.ts lines 33-42

**Estimated Code Saved:** ~40 lines

---

#### 7. **EventSubscriptionManager** - Event Handling
```typescript
class EventSubscriptionManager {
  subscribe(eventName: string, handlerName: string, filterFn?: (data: any) => boolean): void;
  unsubscribe(eventName: string, handlerName: string): void;
  registerHandlers(handlers: Record<string, Function>): void;
}
```

**Current Locations:**
- git_grep.ts lines 173-175
- find_references.ts lines 236, 263
- todo_highlighter.ts lines 128-132

**Estimated Code Saved:** ~20 lines

---

#### 8. **ListBuilder** - Virtual Buffer Content Builder
```typescript
class ListBuilder<T> {
  addHeader(text: string, properties?: Record<string, unknown>): this;
  addEmpty(text: string): this;
  addItems(items: T[], formatter: (item: T, index: number) => TextPropertyEntry): this;
  addFooter(text: string, properties?: Record<string, unknown>): this;
  build(): TextPropertyEntry[];
}
```

**Current Locations:**
- git_log.ts lines 236-282
- diagnostics_panel.ts lines 58-105
- find_references.ts lines 76-123

**Estimated Code Saved:** ~120 lines

---

### 4.2 Shared Utilities Summary Table

| Utility | Classes | Estimated Savings | Priority | Complexity |
|---------|---------|-------------------|----------|------------|
| PanelManager | 1 | ~150 lines | HIGH | Medium |
| VirtualBufferFactory | 1 | ~80 lines | HIGH | Low |
| NavigationController | 1 | ~60 lines | HIGH | Medium |
| HighlightingHelper | 1 | ~200 lines | MEDIUM | High |
| LocationNavigator | 1 | ~70 lines | HIGH | Low |
| ModeBuilder | 1 | ~40 lines | MEDIUM | Low |
| EventSubscriptionManager | 1 | ~20 lines | LOW | Low |
| ListBuilder | 1 | ~120 lines | MEDIUM | Medium |

**Total Estimated Code Savings: ~740 lines (12% reduction)**

---

## 5. Pattern Analysis Summary

### State Management Patterns
- **Pattern:** Module-level state with interfaces
- **Frequency:** 6/6 plugins
- **Complexity:** Low-Medium
- **Extraction Potential:** High (PanelManager)

### Split/Buffer Management
- **Pattern:** Save context → Create split → Track IDs → Close and restore
- **Frequency:** 5/6 plugins (all but git_grep which uses prompts)
- **Complexity:** Medium
- **Extraction Potential:** High (VirtualBufferFactory + PanelManager)

### Navigation
- **Pattern:** Bounded index management with view update
- **Frequency:** 3/6 plugins
- **Complexity:** Low
- **Extraction Potential:** High (NavigationController)

### Highlighting
- **Pattern:** Clear overlays → Iterate lines → Apply overlays
- **Frequency:** 3/6 plugins
- **Complexity:** High
- **Extraction Potential:** Medium (HighlightingHelper)

### Event Handling
- **Pattern:** Hook registration + type guards + async operations
- **Frequency:** 4/6 plugins
- **Complexity:** Medium
- **Extraction Potential:** Medium (EventSubscriptionManager)

### UI/Display Patterns
- **Pattern:** Header → Items → Footer structure for lists
- **Frequency:** 4/6 plugins
- **Complexity:** Low
- **Extraction Potential:** High (ListBuilder)

### File Navigation
- **Pattern:** Extract location → Open in split → Status message
- **Frequency:** 3/6 plugins
- **Complexity:** Low
- **Extraction Potential:** High (LocationNavigator)

### Keybinding Patterns
- **Pattern:** defineMode() with common navigation keys
- **Frequency:** 5/6 plugins
- **Complexity:** Low
- **Extraction Potential:** Medium (ModeBuilder)

---

## 6. Code Examples by Pattern

### Example 1: Before and After - PanelManager

**BEFORE (Duplicated in 3 plugins):**
```typescript
// git_log.ts
interface GitLogState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceSplitId: number | null;
  sourceBufferId: number | null;
  commits: GitCommit[];
  selectedIndex: number;
  options: GitLogOptions;
}

const gitLogState: GitLogState = { ... };

globalThis.git_log_close = function(): void {
  if (!gitLogState.isOpen) return;
  if (gitLogState.bufferId !== null) {
    editor.closeBuffer(gitLogState.bufferId);
  }
  if (gitLogState.splitId !== null) {
    editor.closeSplit(gitLogState.splitId);
  }
  gitLogState.isOpen = false;
  gitLogState.bufferId = null;
  gitLogState.splitId = null;
  gitLogState.sourceSplitId = null;
  gitLogState.sourceBufferId = null;
  gitLogState.commits = [];
  gitLogState.selectedIndex = 0;
};
```

**AFTER (Using PanelManager):**
```typescript
// git_log.ts
const panelManager = new PanelManager();

async function showGitLog() {
  gitLogState.commits = await fetchGitLog();
  
  const bufferId = await panelManager.openPanel({
    name: "*Git Log*",
    mode: "git-log",
    entries: buildGitLogEntries(),
    ratio: 0.6,
    sourceSplitId: editor.getActiveSplitId(),
    sourceBufferId: editor.getActiveBufferId(),
  });
  
  gitLogState.bufferId = bufferId;
  applyGitLogHighlighting();
}

globalThis.git_log_close = function(): void {
  panelManager.closePanel();
};
```

---

### Example 2: Before and After - NavigationController

**BEFORE (Duplicated in 2+ plugins):**
```typescript
globalThis.diagnostics_next = function (): void {
  if (currentDiagnostics.length === 0) return;
  selectedIndex = (selectedIndex + 1) % currentDiagnostics.length;
  updatePanelContent();
  editor.setStatus(`Diagnostic ${selectedIndex + 1}/${currentDiagnostics.length}`);
};

globalThis.diagnostics_prev = function (): void {
  if (currentDiagnostics.length === 0) return;
  selectedIndex = selectedIndex > 0 ? selectedIndex - 1 : currentDiagnostics.length - 1;
  updatePanelContent();
  editor.setStatus(`Diagnostic ${selectedIndex + 1}/${currentDiagnostics.length}`);
};
```

**AFTER (Using NavigationController):**
```typescript
const navigator = new NavigationController(
  currentDiagnostics,
  () => updatePanelContent(),
  "Diagnostic"
);

globalThis.diagnostics_next = () => navigator.next();
globalThis.diagnostics_prev = () => navigator.prev();
globalThis.diagnostics_first = () => navigator.first();
globalThis.diagnostics_last = () => navigator.last();
```

---

## 7. Implementation Recommendations

### Phase 1: Foundation (High Priority)
1. **PanelManager** - Simplifies state and lifecycle for all panel-based plugins
2. **VirtualBufferFactory** - Reduces boilerplate for buffer creation
3. **NavigationController** - Eliminates duplicate navigation logic

### Phase 2: Enhancement (Medium Priority)
4. **ListBuilder** - Makes list UI building more declarative
5. **LocationNavigator** - Centralizes file jumping logic
6. **ModeBuilder** - Makes keybinding configuration more readable

### Phase 3: Polish (Lower Priority)
7. **HighlightingHelper** - Complex but reduces very large highlighting blocks
8. **EventSubscriptionManager** - Improves event handling consistency

### Implementation Strategy
- Create new file: `/home/user/fresh/plugins/lib/lsp-panel-utils.ts`
- Export all utilities from a barrel file
- Update each plugin to import and use utilities
- Add comprehensive JSDoc comments
- Create unit tests for each utility

---

## 8. Files and Line Count Summary

### LSP-Related Plugins
- `git_log.ts`: 857 lines
- `diagnostics_panel.ts`: 309 lines
- `find_references.ts`: 359 lines
- `git_grep.ts`: 189 lines
- `git_find_file.ts`: 304 lines
- `todo_highlighter.ts`: 193 lines

**Total LSP Code: 2,211 lines**

### Potential After Extraction
- With shared libraries: ~1,470 lines (-33%)
- Common utilities: ~300 lines
- **Net reduction: ~540 lines**

