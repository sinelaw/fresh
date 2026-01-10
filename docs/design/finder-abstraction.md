# Unified Finder Abstraction Design

> **Status**: Design Document
> **Date**: January 2026
> **Author**: Claude (with user direction)

## Overview

This document describes a unified `Finder<T>` abstraction for Fresh editor plugins that handle "find something and navigate to it" workflows. The design is inspired by VSCode's QuickPick API and Neovim's Telescope.nvim.

## Problem Statement

Currently, Fresh has multiple plugins that implement similar "find and navigate" functionality with significant code duplication:

- **live_grep.ts** (423 lines) - Project-wide ripgrep search
- **git_grep.ts** (190 lines) - Git grep search
- **git_find_file.ts** (301 lines) - Fuzzy file finder
- **find_references.ts** (213 lines) - LSP references panel
- **diagnostics_panel.ts** (~300 lines) - LSP diagnostics panel

Each plugin manually implements:
- Event handler registration (4 handlers per prompt-based plugin)
- State management (results array, current query)
- Debouncing and process cancellation
- Preview panel creation and updates
- Focus management
- File navigation on selection
- Cleanup on close/cancel

## Research

### VSCode QuickPick API

VSCode's QuickPick provides:
- Event-driven model: `onDidChangeValue`, `onDidAccept`, `onDidChangeActive`, `onDidChangeSelection`
- State properties: `items`, `busy`, `activeItems`, `selectedItems`
- Disposable pattern for cleanup
- Separation of UI component from data fetching

**Key insight**: Clean event model, but still requires manual wiring.

### Telescope.nvim

Telescope uses a pipeline architecture:
- `Finder` → `EntryMaker` → `Sorter` → `Previewer` → `Actions`
- Each component is independently replaceable
- `finder`: Produces raw data (static or async)
- `entry_maker`: Transforms raw data to display entries
- `sorter`: Ranks entries by relevance
- `previewer`: Shows context for selected item
- `attach_mappings`: Defines selection behavior

**Key insight**: Excellent separation of concerns, highly composable.

### Fresh's Current Patterns

Fresh plugins use two distinct patterns:

| Pattern | UI | Lifecycle | Examples |
|---------|-----|-----------|----------|
| **Prompt-based** | Transient prompt with suggestions | Opens → Search → Select → Closes | Live Grep, Git Grep, Git Find File |
| **Panel-based** | Persistent split panel | Opens → Navigate → Stays open | Find References, Diagnostics |

Both patterns share core behaviors:
1. Display a list of locations
2. Preview file content on navigation
3. Open file on selection

## Design

### Key Insight: Three Data Source Patterns

```
┌─────────────────────────────────────────────────────────────┐
│                      Data Sources                           │
├─────────────────┬─────────────────┬─────────────────────────┤
│     Search      │     Filter      │        Event/Live       │
│  (per-query)    │  (load + filter)│     (external push)     │
├─────────────────┼─────────────────┼─────────────────────────┤
│ Live Grep       │ Git Find File   │ Find References (event) │
│ Git Grep        │                 │ Diagnostics (live)      │
└─────────────────┴─────────────────┴─────────────────────────┘
```

### Architecture

```
                    ┌─────────────────┐
                    │   Finder<T>     │
                    │  (Core Logic)   │
                    └────────┬────────┘
                             │
         ┌───────────────────┼───────────────────┐
         │                   │                   │
         ▼                   ▼                   ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│ finder.prompt() │ │ finder.panel()  │ │finder.livePanel()│
│  (Prompt UI)    │ │  (Panel UI)     │ │ (Live Panel UI) │
└─────────────────┘ └─────────────────┘ └─────────────────┘
   Live Grep          Find References      Diagnostics
   Git Grep
   Git Find File
```

### Core Types

```typescript
// plugins/lib/finder.ts

/**
 * Location in a file for preview and navigation
 */
interface FileLocation {
  file: string;
  line: number;
  column?: number;
}

/**
 * How a result should be displayed
 */
interface DisplayEntry {
  label: string;           // Primary text (e.g., "src/main.rs:42")
  description?: string;    // Secondary text (e.g., code snippet)
  location?: FileLocation; // For preview and "go to"
}

/**
 * Data source for search mode (external command per query)
 */
interface SearchSource<T> {
  mode: "search";
  search: (query: string) => Promise<T[]> | ProcessHandle;
  debounceMs?: number;      // Default: 150
  minQueryLength?: number;  // Default: 2
}

/**
 * Data source for filter mode (load once, filter client-side)
 */
interface FilterSource<T> {
  mode: "filter";
  load: () => Promise<T[]>;
  filter?: (items: T[], query: string) => T[];  // Default: fuzzy match
}

/**
 * Main Finder configuration
 */
interface FinderConfig<T> {
  /** Unique identifier (used for prompt_type, panel IDs) */
  id: string;

  /** Transform raw result to display format */
  format: (item: T, index: number) => DisplayEntry;

  /** Preview configuration (default: auto-enabled if format returns location) */
  preview?: boolean | {
    enabled: boolean;
    contextLines?: number;  // Default: 5
  };

  /** Maximum results to display (default: 100) */
  maxResults?: number;

  /** Custom selection handler (default: open file at location) */
  onSelect?: (item: T, entry: DisplayEntry) => void;

  /** Panel-specific: group results by file */
  groupBy?: "file" | "none";

  /** Panel-specific: sync cursor with editor */
  syncWithEditor?: boolean;
}

/**
 * Options for prompt-based display
 */
interface PromptOptions<T> {
  title: string;
  source: SearchSource<T> | FilterSource<T>;
}

/**
 * Options for panel-based display (static data)
 */
interface PanelOptions<T> {
  title: string;
  items: T[];
  ratio?: number;  // Default: 0.3
}

/**
 * Options for live panel display (provider-based)
 */
interface LivePanelOptions<T> {
  title: string;
  provider: Provider<T>;
  ratio?: number;
}
```

### The Finder Class

```typescript
class Finder<T> {
  private config: FinderConfig<T>;
  private editor: EditorAPI;

  // Internal state
  private results: T[] = [];
  private entries: DisplayEntry[] = [];
  private preview: SearchPreview | null = null;
  private search: DebouncedSearch | null = null;
  private isPromptMode: boolean = false;
  private isPanelMode: boolean = false;

  constructor(editor: EditorAPI, config: FinderConfig<T>) {
    this.editor = editor;
    this.config = config;

    // Initialize preview if enabled
    if (this.shouldEnablePreview()) {
      this.preview = new SearchPreview(editor, `${config.id}-preview`);
    }
  }

  /**
   * Start interactive prompt mode
   * Used for: Live Grep, Git Grep, Git Find File
   */
  prompt(options: PromptOptions<T>): void {
    this.isPromptMode = true;
    this.registerPromptHandlers();

    if (options.source.mode === "search") {
      this.search = new DebouncedSearch(this.editor, {
        debounceMs: options.source.debounceMs,
        minQueryLength: options.source.minQueryLength,
      });
    } else {
      // Filter mode: load items upfront
      this.loadFilterItems(options.source);
    }

    this.editor.startPrompt(options.title, this.config.id);
  }

  /**
   * Show static results in panel
   * Used for: Find References
   */
  panel(options: PanelOptions<T>): void {
    this.isPanelMode = true;
    this.results = options.items;
    this.entries = this.results.map((item, i) => this.config.format(item, i));
    this.showPanel(options.title, options.ratio);
  }

  /**
   * Show live-updating results in panel
   * Used for: Diagnostics
   */
  livePanel(options: LivePanelOptions<T>): void {
    this.isPanelMode = true;
    options.provider.subscribe((items) => {
      this.results = items;
      this.entries = this.results.map((item, i) => this.config.format(item, i));
      this.updatePanel();
    });
    this.showPanel(options.title, options.ratio);
  }

  /**
   * Close the finder (prompt or panel)
   */
  close(): void {
    if (this.preview) {
      this.preview.close();
    }
    if (this.search) {
      this.search.cancel();
    }
    // ... cleanup handlers, close panel/prompt
  }

  get isOpen(): boolean {
    return this.isPromptMode || this.isPanelMode;
  }

  // ... private implementation methods
}
```

## Usage Examples

### 1. Live Grep (Prompt + Search Mode)

```typescript
import { Finder, parseGrepOutput } from "./lib/finder.ts";

const editor = getEditor();

const finder = new Finder(editor, {
  id: "live-grep",
  format: (match) => ({
    label: `${match.file}:${match.line}`,
    description: match.content.trim(),
    location: { file: match.file, line: match.line, column: match.column },
  }),
  preview: true,
});

async function runRipgrep(query: string) {
  const result = await editor.spawnProcess("rg", [
    "--line-number", "--column", "--no-heading",
    "--color=never", "--smart-case", "--max-count=100",
    "-g", "!.git", "-g", "!node_modules",
    "--", query,
  ]);
  return result.exit_code === 0 ? parseGrepOutput(result.stdout) : [];
}

editor.registerCommand("Live Grep", "Search project with ripgrep", () => {
  finder.prompt({
    title: editor.t("prompt.live_grep"),
    source: { mode: "search", search: runRipgrep, debounceMs: 150 },
  });
}, "normal");
```

**Estimated: ~40 lines** (down from 423)

### 2. Git Grep (Prompt + Search Mode)

```typescript
const finder = new Finder(editor, {
  id: "git-grep",
  format: grepFormatter,  // Reuse from live_grep!
  preview: true,
});

async function runGitGrep(query: string) {
  const result = await editor.spawnProcess("git", ["grep", "-n", "--column", "-I", "--", query]);
  return result.exit_code === 0 ? parseGrepOutput(result.stdout) : [];
}

editor.registerCommand("Git Grep", "Search with git grep", () => {
  finder.prompt({
    title: editor.t("prompt.grep"),
    source: { mode: "search", search: runGitGrep, minQueryLength: 1 },
  });
}, "normal");
```

**Estimated: ~35 lines** (down from 190)

### 3. Git Find File (Prompt + Filter Mode)

```typescript
const finder = new Finder(editor, {
  id: "git-find-file",
  format: (file) => ({
    label: file,
    location: { file, line: 1, column: 1 },
  }),
  preview: false,
});

async function loadGitFiles() {
  const result = await editor.spawnProcess("git", ["ls-files"]);
  return result.exit_code === 0 ? result.stdout.split("\n").filter(Boolean) : [];
}

editor.registerCommand("Find File", "Find file by name", () => {
  finder.prompt({
    title: editor.t("prompt.find_file"),
    source: { mode: "filter", load: loadGitFiles },  // Uses built-in fuzzy filter
  });
}, "normal");
```

**Estimated: ~40 lines** (down from 301)

### 4. Find References (Panel + Event Data)

```typescript
const finder = new Finder(editor, {
  id: "references",
  format: (ref) => ({
    label: `${ref.line}:${ref.column}`,
    description: ref.content || "",
    location: { file: ref.file, line: ref.line, column: ref.column },
  }),
  groupBy: "file",
  syncWithEditor: true,
});

editor.on("lsp_references", (data) => {
  if (data.locations.length === 0) {
    editor.setStatus(`No references found for '${data.symbol}'`);
    return;
  }

  finder.panel({
    title: `References to '${data.symbol}': ${data.locations.length}`,
    items: data.locations,
  });
});
```

**Estimated: ~30 lines** (down from 213)

### 5. Diagnostics Panel (Live Panel + Provider)

```typescript
const finder = new Finder(editor, {
  id: "diagnostics",
  format: (d) => ({
    label: `${severityIcon(d.severity)} ${d.message}`,
    description: d.source || "",
    location: {
      file: d.uri.replace("file://", ""),
      line: d.range.start.line + 1,
      column: d.range.start.character + 1,
    },
  }),
  groupBy: "file",
  syncWithEditor: true,
});

const provider = createLiveProvider(() => editor.getAllDiagnostics());

editor.registerCommand("Show Diagnostics", "Show all diagnostics", () => {
  finder.livePanel({
    title: "Diagnostics",
    provider,
  });
}, "normal");
```

**Estimated: ~35 lines** (down from ~300)

## Code Reduction Summary

| Plugin | Before | After | Reduction |
|--------|--------|-------|-----------|
| live_grep.ts | 423 lines | ~40 lines | **91%** |
| git_grep.ts | 190 lines | ~35 lines | **82%** |
| git_find_file.ts | 301 lines | ~40 lines | **87%** |
| find_references.ts | 213 lines | ~30 lines | **86%** |
| diagnostics_panel.ts | ~300 lines | ~35 lines | **88%** |
| **Total** | **~1,427 lines** | **~180 lines** | **87%** |

## What Gets Automated

| Responsibility | Before (Manual) | After (Finder Handles) |
|----------------|-----------------|------------------------|
| Register event handlers | Plugin code | Automatic |
| Check `prompt_type` in handlers | Plugin code | Automatic |
| State management (results) | Plugin code | Automatic |
| Debouncing | Plugin code | Automatic |
| Process cancellation | Plugin code | Automatic |
| Preview panel lifecycle | Plugin code | Automatic |
| Focus management | Plugin code | Automatic |
| File opening on select | Plugin code | Automatic (default) |
| Status messages | Plugin code | Automatic |
| Cleanup on close | Plugin code | Automatic |

## Design Principles

1. **Convention over configuration**: Preview auto-enables when `location` is returned
2. **Low floor, high ceiling**: Simple cases need minimal config; complex cases can override
3. **Composition**: Uses `SearchPreview` and `DebouncedSearch` internally
4. **Single abstraction**: One API for both prompt and panel modes
5. **Type safety**: Generics ensure `format` receives correct type `T`
6. **Sensible defaults**: Most plugins need only 3-4 config fields

## Non-Goals

The Finder abstraction is NOT intended for:

- **Multi-step workflows** (like search_replace.ts with selection toggles)
- **Custom UI layouts** (like calculator, theme_editor)
- **Visual decorators** (like git_gutter, color_highlighter)
- **Completion providers** (like path_complete)

These patterns have fundamentally different interaction models.

## Implementation Plan

1. Create `plugins/lib/finder.ts` with core types and Finder class
2. Implement prompt mode (search + filter sources)
3. Implement panel mode (static + live)
4. Add built-in fuzzy filter
5. Refactor live_grep.ts as proof of concept
6. Refactor remaining 4 plugins
7. Update documentation

## References

- [VSCode QuickPick API](https://code.visualstudio.com/api/references/vscode-api)
- [VSCode QuickPick Samples](https://github.com/microsoft/vscode-extension-samples/blob/main/quickinput-sample/src/extension.ts)
- [Telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
- [Telescope Developer Guide](https://github.com/nvim-telescope/telescope.nvim/blob/master/developers.md)
