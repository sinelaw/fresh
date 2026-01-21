/// <reference path="./fresh.d.ts" />

/**
 * Shared utilities for search plugins (Live Grep, Git Grep, etc.)
 *
 * Provides:
 * - Debounced search execution
 * - Preview panel management
 * - Common search result types
 *
 * NOTE: These utilities receive the editor instance as a parameter
 * to avoid calling getEditor() at module scope (which causes errors).
 */

// ============================================================================
// Types
// ============================================================================

export interface SearchMatch {
  file: string;
  line: number;
  column: number;
  content: string;
}

export interface PreviewState {
  bufferId: number | null;
  splitId: number | null;
  originalSplitId: number | null;
}

export interface DebouncedSearchOptions {
  debounceMs?: number;
  minQueryLength?: number;
}

// Editor interface (subset of what we need)
interface EditorApi {
  readFile(path: string): Promise<string>;
  defineMode(name: string, parent: string, bindings: [string, string][], readOnly: boolean): void;
  createVirtualBufferInSplit(options: {
    name: string;
    mode: string;
    readOnly: boolean;
    entries: TextPropertyEntry[];
    ratio: number;
    direction: string;
    panelId: string;
    showLineNumbers: boolean;
    editingDisabled: boolean;
  }): Promise<{ bufferId: number; splitId?: number }>;
  setVirtualBufferContent(bufferId: number, entries: TextPropertyEntry[]): void;
  closeBuffer(bufferId: number): void;
  closeSplit(splitId: number): void;
  focusSplit(splitId: number): void;
  delay(ms: number): Promise<void>;
  debug(msg: string): void;
}

// ============================================================================
// Preview Panel
// ============================================================================

/**
 * Creates and manages a preview panel for search results.
 * Shows file content with context around the match.
 */
export class SearchPreview {
  private bufferId: number | null = null;
  private splitId: number | null = null;
  private originalSplitId: number | null = null;
  private panelId: string;
  private modeName: string;
  private editor: EditorApi;

  constructor(editor: EditorApi, panelId: string) {
    this.editor = editor;
    this.panelId = panelId;
    this.modeName = `${panelId}-preview`;
  }

  /**
   * Remember the original split before creating preview
   */
  setOriginalSplit(splitId: number): void {
    this.originalSplitId = splitId;
  }

  /**
   * Update the preview to show a match with surrounding context
   */
  async update(match: SearchMatch): Promise<void> {
    try {
      const content = await this.editor.readFile(match.file);
      const lines = content.split("\n");

      // Calculate context window (5 lines before and after)
      const contextBefore = 5;
      const contextAfter = 5;
      const startLine = Math.max(0, match.line - 1 - contextBefore);
      const endLine = Math.min(lines.length, match.line + contextAfter);

      const entries: TextPropertyEntry[] = [];

      // Header
      entries.push({
        text: `  ${match.file}:${match.line}:${match.column}\n`,
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
        const isMatchLine = lineNum === match.line;
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

      if (this.bufferId === null) {
        // Create preview mode if not exists
        this.editor.defineMode(this.modeName, "special", [["q", "close_buffer"]], true);

        // Create preview in a split on the right
        const result = await this.editor.createVirtualBufferInSplit({
          name: "*Preview*",
          mode: this.modeName,
          readOnly: true,
          entries,
          ratio: 0.5,
          direction: "vertical",
          panelId: this.panelId,
          showLineNumbers: false,
          editingDisabled: true,
        });

        this.bufferId = result.bufferId;
        this.splitId = result.splitId ?? null;

        // Return focus to original split so prompt stays active
        if (this.originalSplitId !== null) {
          this.editor.focusSplit(this.originalSplitId);
        }
      } else {
        // Update existing buffer content
        this.editor.setVirtualBufferContent(this.bufferId, entries);
      }
    } catch (e) {
      this.editor.debug(`[SearchPreview] Failed to update: ${e}`);
    }
  }

  /**
   * Close the preview panel and clean up
   */
  close(): void {
    if (this.bufferId !== null) {
      this.editor.closeBuffer(this.bufferId);
      this.bufferId = null;
    }
    if (this.splitId !== null) {
      this.editor.closeSplit(this.splitId);
      this.splitId = null;
    }
    this.originalSplitId = null;
  }

  /**
   * Check if preview is currently open
   */
  isOpen(): boolean {
    return this.bufferId !== null;
  }
}

// ============================================================================
// Debounced Search
// ============================================================================

/**
 * Creates a debounced search executor that:
 * - Waits for user to stop typing before searching
 * - Cancels previous searches when new input arrives
 * - Tracks search version to discard stale results
 */
export class DebouncedSearch {
  private currentSearch: ProcessHandle<SpawnResult> | null = null;
  private pendingKill: Promise<boolean> | null = null;
  private searchVersion = 0;
  private lastQuery = "";
  private debounceMs: number;
  private minQueryLength: number;
  private editor: EditorApi;

  constructor(editor: EditorApi, options: DebouncedSearchOptions = {}) {
    this.editor = editor;
    this.debounceMs = options.debounceMs ?? 150;
    this.minQueryLength = options.minQueryLength ?? 2;
  }

  /**
   * Execute a search with debouncing.
   * Returns results via callback to allow async processing.
   */
  async search(
    query: string,
    executor: () => ProcessHandle<SpawnResult>,
    onResults: (result: SpawnResult) => void
  ): Promise<void> {
    const thisVersion = ++this.searchVersion;

    // Kill any existing search immediately
    if (this.currentSearch) {
      this.pendingKill = this.currentSearch.kill();
      this.currentSearch = null;
    }

    // Check minimum query length
    if (!query || query.trim().length < this.minQueryLength) {
      if (this.pendingKill) {
        await this.pendingKill;
        this.pendingKill = null;
      }
      return;
    }

    // Debounce
    await this.editor.delay(this.debounceMs);

    // Wait for pending kill
    if (this.pendingKill) {
      await this.pendingKill;
      this.pendingKill = null;
    }

    // Check if superseded
    if (this.searchVersion !== thisVersion) {
      return;
    }

    // Skip duplicate queries
    if (query === this.lastQuery) {
      return;
    }
    this.lastQuery = query;

    try {
      this.currentSearch = executor();
      const result = await this.currentSearch;

      // Check if this search was cancelled
      if (this.searchVersion !== thisVersion) {
        return;
      }

      this.currentSearch = null;
      onResults(result);
    } catch (e) {
      const errorMsg = String(e);
      if (!errorMsg.includes("killed") && !errorMsg.includes("not found")) {
        this.editor.debug(`[DebouncedSearch] Error: ${e}`);
      }
    }
  }

  /**
   * Cancel any running search
   */
  cancel(): void {
    if (this.currentSearch) {
      this.currentSearch.kill();
      this.currentSearch = null;
    }
  }

  /**
   * Reset state for new search session
   */
  reset(): void {
    this.cancel();
    this.lastQuery = "";
    this.searchVersion = 0;
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Parse a grep-style output line (file:line:column:content)
 */
export function parseGrepLine(line: string): SearchMatch | null {
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
 * Convert search matches to prompt suggestions
 */
export function matchesToSuggestions(
  matches: SearchMatch[],
  maxResults: number = 100
): PromptSuggestion[] {
  const suggestions: PromptSuggestion[] = [];

  for (let i = 0; i < Math.min(matches.length, maxResults); i++) {
    const match = matches[i];
    const displayContent =
      match.content.length > 60
        ? match.content.substring(0, 57) + "..."
        : match.content;

    suggestions.push({
      text: `${match.file}:${match.line}`,
      description: displayContent.trim(),
      value: `${i}`,
      disabled: false,
    });
  }

  return suggestions;
}
