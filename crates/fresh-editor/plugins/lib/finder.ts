/// <reference path="./fresh.d.ts" />

/**
 * Unified Finder Abstraction for Fresh Editor Plugins
 *
 * Provides a single API for "find something and navigate to it" workflows.
 * Inspired by VSCode's QuickPick API and Neovim's Telescope.nvim.
 *
 * Key features:
 * - Prompt mode for interactive search (Live Grep, Git Grep, Git Find File)
 * - Panel mode for displaying results (Find References)
 * - Live panel mode for reactive data (Diagnostics)
 * - Built-in fuzzy filtering
 * - Automatic preview panel management
 * - Automatic debouncing and process cancellation
 *
 * @example
 * ```typescript
 * const finder = new Finder(editor, {
 *   id: "live-grep",
 *   format: (match) => ({
 *     label: `${match.file}:${match.line}`,
 *     description: match.content.trim(),
 *     location: { file: match.file, line: match.line, column: match.column },
 *   }),
 *   preview: true,
 * });
 *
 * finder.prompt({
 *   title: "Search:",
 *   source: { mode: "search", search: runRipgrep, debounceMs: 150 },
 * });
 * ```
 */

import type { Location, RGB } from "./types.ts";

// ============================================================================
// Core Types
// ============================================================================

/**
 * How a result should be displayed
 */
export interface DisplayEntry {
  /** Primary text (e.g., "src/main.rs:42") */
  label: string;
  /** Secondary text (e.g., code snippet) */
  description?: string;
  /** Location for preview and navigation */
  location?: Location;
  /** Severity for visual styling */
  severity?: "error" | "warning" | "info" | "hint";
  /** Custom metadata */
  metadata?: unknown;
}

/**
 * Data source for search mode (external command per query)
 */
export interface SearchSource<T> {
  mode: "search";
  /** Function that returns a ProcessHandle or Promise of results */
  search: (query: string) => ProcessHandle<SpawnResult> | Promise<T[]>;
  /** Debounce delay in ms (default: 150) */
  debounceMs?: number;
  /** Minimum query length to trigger search (default: 2) */
  minQueryLength?: number;
}

/**
 * Data source for filter mode (load once, filter client-side)
 */
export interface FilterSource<T> {
  mode: "filter";
  /** Function to load all items */
  load: () => Promise<T[]>;
  /** Optional custom filter function (default: fuzzy match on formatted label) */
  filter?: (items: T[], query: string) => T[];
}

/**
 * Preview configuration
 */
export interface PreviewConfig {
  enabled: boolean;
  /** Lines of context before and after (default: 5) */
  contextLines?: number;
}

/**
 * Main Finder configuration
 */
export interface FinderConfig<T> {
  /** Unique identifier (used for prompt_type, panel IDs) */
  id: string;

  /** Transform raw result to display format */
  format: (item: T, index: number) => DisplayEntry;

  /** Preview configuration (default: auto-enabled if format returns location) */
  preview?: boolean | PreviewConfig;

  /** Maximum results to display (default: 100) */
  maxResults?: number;

  /** Custom selection handler (default: open file at location) */
  onSelect?: (item: T, entry: DisplayEntry) => void;

  /** Panel-specific: group results by file */
  groupBy?: "file" | "severity" | "none";

  /** Panel-specific: sync cursor with editor */
  syncWithEditor?: boolean;
}

/**
 * Options for prompt-based display
 */
export interface PromptOptions<T> {
  title: string;
  source: SearchSource<T> | FilterSource<T>;
  /** Initial query value */
  initialQuery?: string;
}

/**
 * Options for panel-based display (static data)
 */
export interface PanelOptions<T> {
  title: string;
  items: T[];
  /** Split ratio (default: 0.3) */
  ratio?: number;
}

/**
 * Provider interface for live panel data
 */
export interface FinderProvider<T> {
  /** Get current items */
  getItems(): T[];
  /** Subscribe to changes, returns unsubscribe function */
  subscribe(callback: () => void): () => void;
}

/**
 * Options for live panel display (provider-based)
 */
export interface LivePanelOptions<T> {
  title: string;
  provider: FinderProvider<T>;
  /** Split ratio (default: 0.3) */
  ratio?: number;
}

// ============================================================================
// Colors
// ============================================================================

const colors = {
  selected: [80, 80, 120] as RGB,
  location: [150, 255, 150] as RGB,
  help: [150, 150, 150] as RGB,
  title: [200, 200, 255] as RGB,
  error: [255, 100, 100] as RGB,
  warning: [255, 200, 100] as RGB,
  info: [100, 200, 255] as RGB,
  hint: [150, 150, 150] as RGB,
  fileHeader: [180, 180, 255] as RGB,
  match: [255, 255, 150] as RGB,
  context: [180, 180, 180] as RGB,
  header: [200, 200, 255] as RGB,
  separator: [100, 100, 100] as RGB,
};

// ============================================================================
// Fuzzy Filter
// ============================================================================

/**
 * Score a fuzzy match (higher is better, -1 means no match)
 */
function fuzzyScore(str: string, pattern: string): number {
  if (pattern === "") return 0;

  str = str.toLowerCase();
  pattern = pattern.toLowerCase();

  let score = 0;
  let strIdx = 0;
  let patIdx = 0;
  let consecutiveMatches = 0;
  let lastMatchIdx = -1;

  while (strIdx < str.length && patIdx < pattern.length) {
    if (str[strIdx] === pattern[patIdx]) {
      // Bonus for consecutive matches
      if (lastMatchIdx === strIdx - 1) {
        consecutiveMatches++;
        score += consecutiveMatches * 10;
      } else {
        consecutiveMatches = 1;
        score += 1;
      }

      // Bonus for matching at start of path segments
      if (
        strIdx === 0 ||
        str[strIdx - 1] === "/" ||
        str[strIdx - 1] === "_" ||
        str[strIdx - 1] === "-"
      ) {
        score += 15;
      }

      // Bonus for matching filename (after last /)
      const lastSlash = str.lastIndexOf("/");
      if (strIdx > lastSlash) {
        score += 5;
      }

      lastMatchIdx = strIdx;
      patIdx++;
    }
    strIdx++;
  }

  // Penalty for longer paths
  score -= str.length * 0.1;

  return patIdx >= pattern.length ? score : -1;
}

/**
 * Default fuzzy filter implementation
 */
export function defaultFuzzyFilter<T>(
  items: T[],
  query: string,
  format: (item: T, index: number) => DisplayEntry,
  maxResults: number = 100
): T[] {
  if (query === "" || query.trim() === "") {
    return items.slice(0, maxResults);
  }

  const scored: Array<{ item: T; score: number }> = [];

  for (let i = 0; i < items.length; i++) {
    const entry = format(items[i], i);
    const score = fuzzyScore(entry.label, query);
    if (score > 0) {
      scored.push({ item: items[i], score });
    }

    // Stop early if we have enough high-quality matches
    if (scored.length >= 500) {
      break;
    }
  }

  // Sort by score descending
  scored.sort((a, b) => b.score - a.score);

  return scored.slice(0, maxResults).map((s) => s.item);
}

// ============================================================================
// Parse Utilities
// ============================================================================

/**
 * Parse a grep-style output line (file:line:column:content)
 */
export function parseGrepLine(line: string): {
  file: string;
  line: number;
  column: number;
  content: string;
} | null {
  const match = line.match(/^([^:]+):(\d+):(\d+):(.*)$/);
  if (match) {
    return {
      file: match[1],
      line: parseInt(match[2], 10),
      column: parseInt(match[3], 10),
      content: match[4],
    };
  }
  return null;
}

/**
 * Parse ripgrep/grep output into results array
 */
export function parseGrepOutput(
  stdout: string,
  maxResults: number = 100
): Array<{ file: string; line: number; column: number; content: string }> {
  const results: Array<{
    file: string;
    line: number;
    column: number;
    content: string;
  }> = [];

  for (const line of stdout.split("\n")) {
    if (!line.trim()) continue;
    const match = parseGrepLine(line);
    if (match) {
      results.push(match);
      if (results.length >= maxResults) {
        break;
      }
    }
  }

  return results;
}

// ============================================================================
// Internal State Types
// ============================================================================

interface PromptState<T> {
  results: T[];
  entries: DisplayEntry[];
  lastQuery: string;
  searchVersion: number;
  currentSearch: ProcessHandle<SpawnResult> | null;
  pendingKill: Promise<boolean> | null;
  originalSplitId: number | null;
}

interface PreviewState {
  bufferId: number | null;
  splitId: number | null;
}

interface PanelState<T> {
  bufferId: number | null;
  splitId: number | null;
  sourceSplitId: number | null;
  items: T[];
  entries: DisplayEntry[];
  cursorLine: number;
  cachedContent: string;
  lineToItemIndex: Map<number, number>;
  unsubscribe: (() => void) | null;
}

// ============================================================================
// Finder Class
// ============================================================================

/**
 * Unified Finder for "find something and navigate to it" workflows
 */
export class Finder<T> {
  private config: FinderConfig<T>;
  private editor: EditorAPI;

  // Prompt mode state
  private promptState: PromptState<T> = {
    results: [],
    entries: [],
    lastQuery: "",
    searchVersion: 0,
    currentSearch: null,
    pendingKill: null,
    originalSplitId: null,
  };

  // Preview state (shared between prompt and panel modes)
  private previewState: PreviewState = {
    bufferId: null,
    splitId: null,
  };

  // Panel mode state
  private panelState: PanelState<T> = {
    bufferId: null,
    splitId: null,
    sourceSplitId: null,
    items: [],
    entries: [],
    cursorLine: 1,
    cachedContent: "",
    lineToItemIndex: new Map(),
    unsubscribe: null,
  };

  // Mode flags
  private isPromptMode = false;
  private isPanelMode = false;

  // Handler names (for cleanup)
  private handlerPrefix: string;
  private modeName: string;
  private previewModeName: string;

  // Current source (for prompt mode)
  private currentSource: SearchSource<T> | FilterSource<T> | null = null;
  private allItems: T[] = []; // For filter mode

  constructor(editor: EditorAPI, config: FinderConfig<T>) {
    this.editor = editor;
    this.config = {
      maxResults: 100,
      groupBy: "none",
      syncWithEditor: false,
      ...config,
    };

    this.handlerPrefix = `_finder_${config.id}`;
    this.modeName = `${config.id}-results`;
    this.previewModeName = `${config.id}-preview`;

    // Register handlers
    this.registerPromptHandlers();
    this.registerPanelHandlers();
  }

  // ==========================================================================
  // Public API
  // ==========================================================================

  /**
   * Check if the finder is currently open
   */
  get isOpen(): boolean {
    return this.isPromptMode || this.isPanelMode;
  }

  /**
   * Start interactive prompt mode
   */
  prompt(options: PromptOptions<T>): void {
    this.isPromptMode = true;
    this.isPanelMode = false;
    this.currentSource = options.source;

    // Reset state
    this.promptState = {
      results: [],
      entries: [],
      lastQuery: "",
      searchVersion: 0,
      currentSearch: null,
      pendingKill: null,
      originalSplitId: this.editor.getActiveSplitId(),
    };

    // For filter mode, load items upfront
    if (options.source.mode === "filter") {
      this.loadFilterItems(options.source);
    }

    // Start the prompt
    if (options.initialQuery) {
      this.editor.startPromptWithInitial(
        options.title,
        this.config.id,
        options.initialQuery
      );
    } else {
      this.editor.debug(`[Finder] calling startPrompt with title="${options.title}", id="${this.config.id}"`);
      const result = this.editor.startPrompt(options.title, this.config.id);
      this.editor.debug(`[Finder] startPrompt returned: ${result}`);
    }
    this.editor.setStatus("Type to search...");
  }

  /**
   * Show static results in panel
   */
  async panel(options: PanelOptions<T>): Promise<void> {
    this.isPromptMode = false;
    this.isPanelMode = true;

    // Save source context
    this.panelState.sourceSplitId = this.editor.getActiveSplitId();
    this.panelState.items = options.items;
    this.panelState.entries = options.items.map((item, i) =>
      this.config.format(item, i)
    );

    await this.showPanel(options.title, options.ratio ?? 0.3);
  }

  /**
   * Show live-updating results in panel
   */
  async livePanel(options: LivePanelOptions<T>): Promise<void> {
    this.isPromptMode = false;
    this.isPanelMode = true;

    // Save source context
    this.panelState.sourceSplitId = this.editor.getActiveSplitId();

    // Initial load
    this.panelState.items = options.provider.getItems();
    this.panelState.entries = this.panelState.items.map((item, i) =>
      this.config.format(item, i)
    );

    // Subscribe to updates
    this.panelState.unsubscribe = options.provider.subscribe(() => {
      if (this.isPanelMode && this.panelState.bufferId !== null) {
        this.panelState.items = options.provider.getItems();
        this.panelState.entries = this.panelState.items.map((item, i) =>
          this.config.format(item, i)
        );
        this.refreshPanel(options.title);
      }
    });

    await this.showPanel(options.title, options.ratio ?? 0.3);
  }

  /**
   * Close the finder (prompt or panel)
   */
  close(): void {
    if (this.isPromptMode) {
      this.closePrompt();
    }
    if (this.isPanelMode) {
      this.closePanel();
    }
  }

  /**
   * Update panel title (for live panels)
   */
  updateTitle(title: string): void {
    if (this.isPanelMode && this.panelState.bufferId !== null) {
      this.refreshPanel(title);
    }
  }

  // ==========================================================================
  // Prompt Mode Implementation
  // ==========================================================================

  private registerPromptHandlers(): void {
    const self = this;
    const id = this.config.id;

    // Handle prompt input changes
    (globalThis as Record<string, unknown>)[`${this.handlerPrefix}_changed`] =
      function (args: { prompt_type: string; input: string }): boolean {
        if (args.prompt_type !== id) {
          return true;
        }
        self.onPromptChanged(args.input);
        return true;
      };

    // Handle selection changes
    (globalThis as Record<string, unknown>)[
      `${this.handlerPrefix}_selection`
    ] = function (args: {
      prompt_type: string;
      selected_index: number;
    }): boolean {
      if (args.prompt_type !== id) {
        return true;
      }
      self.onPromptSelectionChanged(args.selected_index);
      return true;
    };

    // Handle prompt confirmation
    (globalThis as Record<string, unknown>)[`${this.handlerPrefix}_confirmed`] =
      function (args: {
        prompt_type: string;
        selected_index: number | null;
        input: string;
      }): boolean {
        if (args.prompt_type !== id) {
          return true;
        }
        self.onPromptConfirmed(args.selected_index);
        return true;
      };

    // Handle prompt cancellation
    (globalThis as Record<string, unknown>)[`${this.handlerPrefix}_cancelled`] =
      function (args: { prompt_type: string }): boolean {
        if (args.prompt_type !== id) {
          return true;
        }
        self.onPromptCancelled();
        return true;
      };

    // Register event handlers
    this.editor.on("prompt_changed", `${this.handlerPrefix}_changed`);
    this.editor.on("prompt_selection_changed", `${this.handlerPrefix}_selection`);
    this.editor.on("prompt_confirmed", `${this.handlerPrefix}_confirmed`);
    this.editor.on("prompt_cancelled", `${this.handlerPrefix}_cancelled`);
  }

  private async loadFilterItems(source: FilterSource<T>): Promise<void> {
    try {
      this.allItems = await source.load();
      // Show initial suggestions
      const filtered = this.filterItems("", source);
      this.updatePromptResults(filtered);
      this.editor.setStatus(`${this.allItems.length} items available`);
    } catch (e) {
      this.editor.debug(`[Finder] Failed to load items: ${e}`);
      this.editor.setStatus(`Failed to load items: ${e}`);
    }
  }

  private filterItems(query: string, source: FilterSource<T>): T[] {
    if (source.filter) {
      return source.filter(this.allItems, query);
    }
    return defaultFuzzyFilter(
      this.allItems,
      query,
      this.config.format,
      this.config.maxResults
    );
  }

  private async onPromptChanged(input: string): Promise<void> {
    if (!this.currentSource) return;

    if (this.currentSource.mode === "filter") {
      // Filter mode: filter client-side
      const filtered = this.filterItems(input, this.currentSource);
      this.updatePromptResults(filtered);

      if (filtered.length > 0) {
        this.editor.setStatus(`Found ${filtered.length} matches`);
      } else {
        this.editor.setStatus("No matches");
      }
    } else {
      // Search mode: run external search
      await this.runSearch(input, this.currentSource);
    }
  }

  private async runSearch(
    query: string,
    source: SearchSource<T>
  ): Promise<void> {
    const debounceMs = source.debounceMs ?? 150;
    const minQueryLength = source.minQueryLength ?? 2;
    const thisVersion = ++this.promptState.searchVersion;

    // Kill any existing search
    if (this.promptState.currentSearch) {
      this.promptState.pendingKill = this.promptState.currentSearch.kill();
      this.promptState.currentSearch = null;
    }

    // Check minimum query length
    if (!query || query.trim().length < minQueryLength) {
      if (this.promptState.pendingKill) {
        await this.promptState.pendingKill;
        this.promptState.pendingKill = null;
      }
      this.editor.setPromptSuggestions([]);
      this.promptState.results = [];
      return;
    }

    // Debounce
    await this.editor.delay(debounceMs);

    // Wait for pending kill
    if (this.promptState.pendingKill) {
      await this.promptState.pendingKill;
      this.promptState.pendingKill = null;
    }

    // Check if superseded
    if (this.promptState.searchVersion !== thisVersion) {
      return;
    }

    // Skip duplicate queries
    if (query === this.promptState.lastQuery) {
      return;
    }
    this.promptState.lastQuery = query;

    try {
      const searchResult = source.search(query);

      // Check if it's a ProcessHandle or a Promise
      if ("kill" in searchResult) {
        // ProcessHandle
        this.promptState.currentSearch = searchResult;
        const result = await searchResult;

        // Check if cancelled
        if (this.promptState.searchVersion !== thisVersion) {
          return;
        }
        this.promptState.currentSearch = null;

        if (result.exit_code === 0) {
          // Parse as grep output by default
          const parsed = parseGrepOutput(
            result.stdout,
            this.config.maxResults
          ) as unknown as T[];
          this.updatePromptResults(parsed);

          if (parsed.length > 0) {
            this.editor.setStatus(`Found ${parsed.length} matches`);
            // Show preview of first result
            if (this.shouldShowPreview()) {
              await this.updatePreview(this.promptState.entries[0]);
            }
          } else {
            this.editor.setStatus("No matches");
          }
        } else if (result.exit_code === 1) {
          // No matches
          this.updatePromptResults([]);
          this.editor.setStatus("No matches");
        } else if (result.exit_code !== -1) {
          // Error (ignore -1 which means killed)
          this.editor.setStatus(`Search error: ${result.stderr}`);
        }
      } else {
        // Promise<T[]>
        const results = await searchResult;

        // Check if cancelled
        if (this.promptState.searchVersion !== thisVersion) {
          return;
        }

        this.updatePromptResults(results);

        if (results.length > 0) {
          this.editor.setStatus(`Found ${results.length} matches`);
          if (this.shouldShowPreview()) {
            await this.updatePreview(this.promptState.entries[0]);
          }
        } else {
          this.editor.setStatus("No matches");
        }
      }
    } catch (e) {
      const errorMsg = String(e);
      if (!errorMsg.includes("killed") && !errorMsg.includes("not found")) {
        this.editor.setStatus(`Search error: ${e}`);
      }
    }
  }

  private updatePromptResults(results: T[]): void {
    this.promptState.results = results;
    this.promptState.entries = results.map((item, i) =>
      this.config.format(item, i)
    );

    const suggestions: PromptSuggestion[] = this.promptState.entries.map(
      (entry, i) => ({
        text: entry.label,
        description: entry.description,
        value: `${i}`,
        disabled: false,
      })
    );

    this.editor.setPromptSuggestions(suggestions);
  }

  private async onPromptSelectionChanged(selectedIndex: number): Promise<void> {
    const entry = this.promptState.entries[selectedIndex];
    if (entry && this.shouldShowPreview()) {
      await this.updatePreview(entry);
    }
  }

  private onPromptConfirmed(selectedIndex: number | null): void {
    // Kill any running search
    if (this.promptState.currentSearch) {
      this.promptState.currentSearch.kill();
      this.promptState.currentSearch = null;
    }

    // Close preview
    this.closePreview();

    // Handle selection
    if (
      selectedIndex !== null &&
      this.promptState.results[selectedIndex] !== undefined
    ) {
      const item = this.promptState.results[selectedIndex];
      const entry = this.promptState.entries[selectedIndex];

      if (this.config.onSelect) {
        this.config.onSelect(item, entry);
      } else if (entry.location) {
        // Default: open file at location
        this.editor.openFile(
          entry.location.file,
          entry.location.line,
          entry.location.column
        );
        this.editor.setStatus(
          `Opened ${entry.location.file}:${entry.location.line}`
        );
      }
    } else {
      this.editor.setStatus("No selection");
    }

    // Clear state
    this.isPromptMode = false;
    this.promptState.results = [];
    this.promptState.entries = [];
    this.promptState.originalSplitId = null;
  }

  private onPromptCancelled(): void {
    // Kill any running search
    if (this.promptState.currentSearch) {
      this.promptState.currentSearch.kill();
      this.promptState.currentSearch = null;
    }

    // Close preview
    this.closePreview();

    // Clear state
    this.isPromptMode = false;
    this.promptState.results = [];
    this.promptState.entries = [];
    this.promptState.originalSplitId = null;
    this.editor.setStatus("Cancelled");
  }

  private closePrompt(): void {
    this.onPromptCancelled();
  }

  // ==========================================================================
  // Preview Implementation
  // ==========================================================================

  private shouldShowPreview(): boolean {
    if (this.config.preview === false) {
      return false;
    }
    if (this.config.preview === true) {
      return true;
    }
    if (typeof this.config.preview === "object") {
      return this.config.preview.enabled;
    }
    // Auto-detect: enable if any entry has a location
    return this.promptState.entries.some((e) => e.location);
  }

  private getContextLines(): number {
    if (typeof this.config.preview === "object") {
      return this.config.preview.contextLines ?? 5;
    }
    return 5;
  }

  private async updatePreview(entry: DisplayEntry): Promise<void> {
    if (!entry.location) return;

    try {
      const content = await this.editor.readFile(entry.location.file);
      const lines = content.split("\n");

      const contextLines = this.getContextLines();
      const startLine = Math.max(0, entry.location.line - 1 - contextLines);
      const endLine = Math.min(lines.length, entry.location.line + contextLines);

      const entries: TextPropertyEntry[] = [];

      // Header
      entries.push({
        text: `  ${entry.location.file}:${entry.location.line}:${entry.location.column ?? 1}\n`,
        properties: { type: "header" },
      });
      entries.push({
        text: "─".repeat(60) + "\n",
        properties: { type: "separator" },
      });

      // Content lines with line numbers
      for (let i = startLine; i < endLine; i++) {
        const lineNum = i + 1;
        const lineContent = lines[i] || "";
        const isMatchLine = lineNum === entry.location.line;
        const prefix = isMatchLine ? "> " : "  ";
        const lineNumStr = String(lineNum).padStart(4, " ");

        entries.push({
          text: `${prefix}${lineNumStr} │ ${lineContent}\n`,
          properties: {
            type: isMatchLine ? "match" : "context",
            line: lineNum,
          },
        });
      }

      if (this.previewState.bufferId === null) {
        // Define preview mode
        this.editor.defineMode(
          this.previewModeName,
          "special",
          [["q", "close_buffer"]],
          true
        );

        // Create preview split
        const result = await this.editor.createVirtualBufferInSplit({
          name: "*Preview*",
          mode: this.previewModeName,
          readOnly: true,
          entries,
          ratio: 0.5,
          direction: "vertical",
          panelId: `${this.config.id}-preview`,
          showLineNumbers: false,
          editingDisabled: true,
        });

        this.previewState.bufferId = result.bufferId;
        this.previewState.splitId = result.splitId ?? null;

        // Return focus to original split
        if (this.promptState.originalSplitId !== null) {
          this.editor.focusSplit(this.promptState.originalSplitId);
        }
      } else {
        // Update existing preview
        this.editor.setVirtualBufferContent(this.previewState.bufferId, entries);
      }
    } catch (e) {
      this.editor.debug(`[Finder] Failed to update preview: ${e}`);
    }
  }

  private closePreview(): void {
    if (this.previewState.bufferId || this.previewState.bufferId == 0) {
      this.editor.closeBuffer(this.previewState.bufferId);
      this.previewState.bufferId = null;
    }
    if (this.previewState.splitId || this.previewState.splitId == 0) {
      this.editor.closeSplit(this.previewState.splitId);
      this.previewState.splitId = null;
    }
  }

  // ==========================================================================
  // Panel Mode Implementation
  // ==========================================================================

  private registerPanelHandlers(): void {
    const self = this;

    // Define panel mode
    this.editor.defineMode(
      this.modeName,
      "normal",
      [
        ["Return", `${this.handlerPrefix}_panel_select`],
        ["Escape", `${this.handlerPrefix}_panel_close`],
      ],
      true
    );

    // Select handler
    (globalThis as Record<string, unknown>)[
      `${this.handlerPrefix}_panel_select`
    ] = function (): void {
      self.onPanelSelect();
    };

    // Close handler
    (globalThis as Record<string, unknown>)[
      `${this.handlerPrefix}_panel_close`
    ] = function (): void {
      self.closePanel();
    };

    // Cursor movement handler
    (globalThis as Record<string, unknown>)[
      `${this.handlerPrefix}_panel_cursor`
    ] = function (data: {
      buffer_id: number;
      cursor_id: number;
      old_position: number;
      new_position: number;
      line: number;
    }): void {
      if (!self.isPanelMode || self.panelState.bufferId === null) return;
      if (data.buffer_id !== self.panelState.bufferId) return;

      self.panelState.cursorLine = data.line;
      self.applyPanelHighlighting();

      const itemIndex = self.panelState.lineToItemIndex.get(data.line);
      if (itemIndex !== undefined && itemIndex < self.panelState.items.length) {
        self.editor.setStatus(
          `Item ${itemIndex + 1}/${self.panelState.items.length}`
        );
      }
    };

    // Register cursor movement handler
    this.editor.on("cursor_moved", `${this.handlerPrefix}_panel_cursor`);

    // Sync with editor handler (if enabled)
    if (this.config.syncWithEditor) {
      (globalThis as Record<string, unknown>)[
        `${this.handlerPrefix}_editor_cursor`
      ] = function (data: {
        buffer_id: number;
        cursor_id: number;
        old_position: number;
        new_position: number;
        line: number;
      }): void {
        if (!self.isPanelMode || self.panelState.bufferId === null) return;
        if (data.buffer_id === self.panelState.bufferId) return;

        const filePath = self.editor.getBufferPath(data.buffer_id);
        if (!filePath) return;

        // Find matching item
        const matchingIndex = self.panelState.entries.findIndex((entry) => {
          if (!entry.location) return false;
          return (
            entry.location.file === filePath &&
            entry.location.line === data.line
          );
        });

        if (matchingIndex >= 0) {
          self.revealItem(matchingIndex);
        }
      };

      this.editor.on("cursor_moved", `${this.handlerPrefix}_editor_cursor`);
    }
  }

  private async showPanel(title: string, ratio: number): Promise<void> {
    const entries = this.buildPanelEntries(title);
    this.panelState.cachedContent = entries.map((e) => e.text).join("");
    this.panelState.cursorLine = this.findFirstItemLine();

    try {
      const result = await this.editor.createVirtualBufferInSplit({
        name: `*${this.config.id.charAt(0).toUpperCase() + this.config.id.slice(1)}*`,
        mode: this.modeName,
        readOnly: true,
        entries,
        ratio,
        direction: "horizontal",
        panelId: this.config.id,
        showLineNumbers: false,
        showCursors: true,
        editingDisabled: true,
      });

      if (result.bufferId !== null) {
        this.panelState.bufferId = result.bufferId;
        this.panelState.splitId = result.splitId ?? null;
        this.applyPanelHighlighting();

        const count = this.panelState.items.length;
        this.editor.setStatus(`${title}: ${count} item${count !== 1 ? "s" : ""}`);
      } else {
        this.editor.setStatus("Failed to open panel");
      }
    } catch (e) {
      this.editor.setStatus(`Failed to open panel: ${e}`);
      this.editor.debug(`[Finder] Panel error: ${e}`);
    }
  }

  private refreshPanel(title: string): void {
    if (this.panelState.bufferId === null) return;

    const entries = this.buildPanelEntries(title);
    this.panelState.cachedContent = entries.map((e) => e.text).join("");

    this.editor.setVirtualBufferContent(this.panelState.bufferId, entries);
    this.applyPanelHighlighting();

    const count = this.panelState.items.length;
    this.editor.setStatus(`${title}: ${count} item${count !== 1 ? "s" : ""}`);
  }

  private buildPanelEntries(title: string): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    this.panelState.lineToItemIndex.clear();

    let currentLine = 1;

    // Title line
    entries.push({
      text: `${title}\n`,
      properties: { type: "title" },
    });
    currentLine++;

    if (this.panelState.entries.length === 0) {
      entries.push({
        text: "  No results\n",
        properties: { type: "empty" },
      });
      currentLine++;
    } else if (this.config.groupBy === "file") {
      // Group by file
      const byFile = new Map<
        string,
        Array<{ entry: DisplayEntry; index: number }>
      >();

      for (let i = 0; i < this.panelState.entries.length; i++) {
        const entry = this.panelState.entries[i];
        const file = entry.location?.file ?? "(no file)";
        if (!byFile.has(file)) {
          byFile.set(file, []);
        }
        byFile.get(file)!.push({ entry, index: i });
      }

      for (const [file, itemsInFile] of byFile) {
        // File header
        const fileName = file.split("/").pop() ?? file;
        entries.push({
          text: `\n${fileName}:\n`,
          properties: { type: "file-header", file },
        });
        currentLine += 2;

        // Items in this file
        for (const { entry, index } of itemsInFile) {
          entries.push(this.buildItemEntry(entry));
          this.panelState.lineToItemIndex.set(currentLine, index);
          currentLine++;
        }
      }
    } else {
      // Flat list
      for (let i = 0; i < this.panelState.entries.length; i++) {
        const entry = this.panelState.entries[i];
        entries.push(this.buildItemEntry(entry));
        this.panelState.lineToItemIndex.set(currentLine, i);
        currentLine++;
      }
    }

    // Help footer
    entries.push({
      text: "\n",
      properties: { type: "blank" },
    });
    entries.push({
      text: "Enter:select | Esc:close\n",
      properties: { type: "help" },
    });

    return entries;
  }

  private buildItemEntry(entry: DisplayEntry): TextPropertyEntry {
    const severityIcon =
      entry.severity === "error"
        ? "[E]"
        : entry.severity === "warning"
          ? "[W]"
          : entry.severity === "info"
            ? "[I]"
            : entry.severity === "hint"
              ? "[H]"
              : "";

    const prefix = severityIcon ? `${severityIcon} ` : "  ";
    const desc = entry.description ? `  ${entry.description}` : "";

    let line = `${prefix}${entry.label}${desc}`;
    const maxLen = 100;
    if (line.length > maxLen) {
      line = line.slice(0, maxLen - 3) + "...";
    }

    return {
      text: `${line}\n`,
      properties: {
        type: "item",
        location: entry.location,
        severity: entry.severity,
        metadata: entry.metadata,
      },
    };
  }

  private findFirstItemLine(): number {
    for (const [line] of this.panelState.lineToItemIndex) {
      return line;
    }
    return 2;
  }

  private onPanelSelect(): void {
    const itemIndex = this.panelState.lineToItemIndex.get(
      this.panelState.cursorLine
    );
    if (itemIndex === undefined) {
      this.editor.setStatus("No item selected");
      return;
    }

    const item = this.panelState.items[itemIndex];
    const entry = this.panelState.entries[itemIndex];

    if (this.config.onSelect) {
      this.config.onSelect(item, entry);
    } else if (entry.location) {
      // Default: open file at location
      if (this.panelState.sourceSplitId !== null) {
        this.editor.focusSplit(this.panelState.sourceSplitId);
      }
      this.editor.openFile(
        entry.location.file,
        entry.location.line,
        entry.location.column
      );
      this.editor.setStatus(
        `Jumped to ${entry.location.file}:${entry.location.line}`
      );
    }
  }

  private closePanel(): void {
    // Unsubscribe from provider
    if (this.panelState.unsubscribe) {
      this.panelState.unsubscribe();
      this.panelState.unsubscribe = null;
    }

    // Close split and buffer
    const splitId = this.panelState.splitId;
    const bufferId = this.panelState.bufferId;
    const sourceSplitId = this.panelState.sourceSplitId;

    // Clear state
    this.isPanelMode = false;
    this.panelState.bufferId = null;
    this.panelState.splitId = null;
    this.panelState.sourceSplitId = null;
    this.panelState.items = [];
    this.panelState.entries = [];
    this.panelState.cachedContent = "";
    this.panelState.cursorLine = 1;
    this.panelState.lineToItemIndex.clear();

    // Close UI
    if (splitId !== null) {
      this.editor.closeSplit(splitId);
    }
    if (bufferId !== null) {
      this.editor.closeBuffer(bufferId);
    }

    // Focus source
    if (sourceSplitId !== null) {
      this.editor.focusSplit(sourceSplitId);
    }

    this.editor.setStatus("Closed");
  }

  private revealItem(index: number): void {
    if (this.panelState.bufferId === null) return;

    // Find the panel line for this item
    for (const [line, idx] of this.panelState.lineToItemIndex) {
      if (idx === index) {
        this.panelState.cursorLine = line;

        // Move cursor to this line
        const byteOffset = this.lineToByteOffset(line);
        this.editor.setBufferCursor(this.panelState.bufferId, byteOffset);
        this.applyPanelHighlighting();
        break;
      }
    }
  }

  private lineToByteOffset(lineNumber: number): number {
    const lines = this.panelState.cachedContent.split("\n");
    let offset = 0;
    for (let i = 0; i < lineNumber - 1 && i < lines.length; i++) {
      offset += lines[i].length + 1;
    }
    return offset;
  }

  private applyPanelHighlighting(): void {
    if (this.panelState.bufferId === null) return;

    const bufferId = this.panelState.bufferId;
    const namespace = this.config.id;
    this.editor.clearNamespace(bufferId, namespace);

    if (!this.panelState.cachedContent) return;

    const lines = this.panelState.cachedContent.split("\n");
    let byteOffset = 0;

    for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
      const line = lines[lineIdx];
      const lineStart = byteOffset;
      const lineEnd = byteOffset + line.length;
      const lineNumber = lineIdx + 1;
      const isCurrentLine = lineNumber === this.panelState.cursorLine;
      const isItemLine = this.panelState.lineToItemIndex.has(lineNumber);

      // Highlight current line if it's an item line
      if (isCurrentLine && isItemLine && line.trim() !== "") {
        this.editor.addOverlay(
          bufferId,
          namespace,
          lineStart,
          lineEnd,
          colors.selected[0],
          colors.selected[1],
          colors.selected[2],
          -1,
          -1,
          -1,
          false,
          true,
          false,
          true
        );
      }

      // Title line
      if (lineNumber === 1) {
        this.editor.addOverlay(
          bufferId,
          namespace,
          lineStart,
          lineEnd,
          colors.title[0],
          colors.title[1],
          colors.title[2],
          -1,
          -1,
          -1,
          false,
          true,
          false,
          false
        );
      }

      // File header (ends with : but isn't title)
      if (line.endsWith(":") && lineNumber > 1 && !line.startsWith(" ")) {
        this.editor.addOverlay(
          bufferId,
          namespace,
          lineStart,
          lineEnd,
          colors.fileHeader[0],
          colors.fileHeader[1],
          colors.fileHeader[2],
          -1,
          -1,
          -1,
          false,
          true,
          false,
          false
        );
      }

      // Severity icon highlighting
      const iconMatch = line.match(/^\[([EWIH])\]/);
      if (iconMatch) {
        const iconEnd = lineStart + 3;
        let color: RGB;
        switch (iconMatch[1]) {
          case "E":
            color = colors.error;
            break;
          case "W":
            color = colors.warning;
            break;
          case "I":
            color = colors.info;
            break;
          case "H":
            color = colors.hint;
            break;
          default:
            color = colors.hint;
        }

        this.editor.addOverlay(
          bufferId,
          namespace,
          lineStart,
          iconEnd,
          color[0],
          color[1],
          color[2],
          -1,
          -1,
          -1,
          false,
          true,
          false,
          false
        );
      }

      // Help line (dimmed)
      if (line.startsWith("Enter:") || line.includes("|")) {
        this.editor.addOverlay(
          bufferId,
          namespace,
          lineStart,
          lineEnd,
          colors.help[0],
          colors.help[1],
          colors.help[2],
          -1,
          -1,
          -1,
          false,
          false,
          false,
          false
        );
      }

      byteOffset += line.length + 1;
    }
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Get relative path for display
 */
export function getRelativePath(editor: EditorAPI, filePath: string): string {
  const cwd = editor.getCwd();
  if (filePath.startsWith(cwd)) {
    return filePath.slice(cwd.length + 1);
  }
  return filePath;
}

/**
 * Create a simple live provider from a getter function
 */
export function createLiveProvider<T>(
  getItems: () => T[]
): FinderProvider<T> & { notify: () => void } {
  const listeners: Array<() => void> = [];

  return {
    getItems,
    subscribe(callback: () => void) {
      listeners.push(callback);
      return () => {
        const index = listeners.indexOf(callback);
        if (index >= 0) {
          listeners.splice(index, 1);
        }
      };
    },
    notify() {
      for (const listener of listeners) {
        listener();
      }
    },
  };
}
