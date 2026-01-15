/// <reference path="./fresh.d.ts" />

import type { PanelOptions, PanelState } from "./types.ts";

/**
 * PanelManager - Manages panel lifecycle for split-view plugins
 *
 * Handles the common pattern of:
 * - Opening a virtual buffer in a split
 * - Tracking source split for navigation
 * - Closing and restoring previous state
 * - Updating panel content
 *
 * @example
 * ```typescript
 * const panel = new PanelManager("diagnostics", "diagnostics-list");
 *
 * async function open() {
 *   await panel.open({ entries: buildEntries(), ratio: 0.3 });
 * }
 *
 * function close() {
 *   panel.close();
 * }
 *
 * function update() {
 *   panel.updateContent(buildEntries());
 * }
 * ```
 */
export class PanelManager {
  private state: PanelState = {
    isOpen: false,
    bufferId: null,
    splitId: null,
    sourceSplitId: null,
    sourceBufferId: null,
  };

  /**
   * Create a new PanelManager
   *
   * @param editor - The editor API instance
   * @param panelName - Display name for the panel (e.g., "*Diagnostics*")
   * @param modeName - Mode name for keybindings (e.g., "diagnostics-list")
   */
  constructor(
    private readonly editor: EditorAPI,
    private readonly panelName: string,
    private readonly modeName: string
  ) {}

  /**
   * Check if the panel is currently open
   */
  get isOpen(): boolean {
    return this.state.isOpen;
  }

  /**
   * Get the panel's buffer ID (null if not open)
   */
  get bufferId(): number | null {
    return this.state.bufferId;
  }

  /**
   * Get the panel's split ID (null if not open)
   */
  get splitId(): number | null {
    return this.state.splitId;
  }

  /**
   * Get the source split ID (where user was before opening panel)
   */
  get sourceSplitId(): number | null {
    return this.state.sourceSplitId;
  }

  /**
   * Get the source buffer ID (to restore when closing)
   */
  get sourceBufferId(): number | null {
    return this.state.sourceBufferId;
  }

  /**
   * Open the panel in a new split
   *
   * If already open, updates the content instead.
   *
   * @param options - Panel configuration
   * @returns The buffer ID of the panel
   */
  async open(options: PanelOptions): Promise<number> {
    const { entries, ratio = 0.3, showLineNumbers = false, editingDisabled = true } = options;

    if (this.state.isOpen && this.state.bufferId !== null) {
      // Panel already open - just update content
      this.updateContent(entries);
      return this.state.bufferId;
    }

    // Save current context
    this.state.sourceSplitId = this.editor.getActiveSplitId();
    this.state.sourceBufferId = this.editor.getActiveBufferId();

    // Create virtual buffer in split
    const result = await this.editor.createVirtualBufferInSplit({
      name: this.panelName,
      mode: this.modeName,
      read_only: true,
      entries,
      ratio,
      panel_id: this.panelName,
      show_line_numbers: showLineNumbers,
      editing_disabled: editingDisabled,
    });

    // Track state
    this.state.bufferId = result.buffer_id;
    this.state.splitId = result.split_id ?? this.editor.getActiveSplitId();
    this.state.isOpen = true;

    return result.buffer_id;
  }

  /**
   * Close the panel and restore previous state
   */
  close(): void {
    if (!this.state.isOpen) {
      return;
    }

    // Close the split containing the panel
    if (this.state.splitId !== null) {
      this.editor.closeSplit(this.state.splitId);
    }

    // Focus back on source split
    if (this.state.sourceSplitId !== null) {
      this.editor.focusSplit(this.state.sourceSplitId);
    }

    // Reset state
    this.reset();
  }

  /**
   * Update the panel content without closing/reopening
   *
   * @param entries - New entries to display
   */
  updateContent(entries: TextPropertyEntry[]): void {
    if (!this.state.isOpen || this.state.bufferId === null) {
      return;
    }

    this.editor.setVirtualBufferContent(this.state.bufferId, entries);
  }

  /**
   * Reset panel state (called internally on close)
   */
  reset(): void {
    this.state = {
      isOpen: false,
      bufferId: null,
      splitId: null,
      sourceSplitId: null,
      sourceBufferId: null,
    };
  }

  /**
   * Focus the source split (useful for "goto" operations)
   */
  focusSource(): void {
    if (this.state.sourceSplitId !== null) {
      this.editor.focusSplit(this.state.sourceSplitId);
    }
  }

  /**
   * Focus the panel split
   */
  focusPanel(): void {
    if (this.state.splitId !== null) {
      this.editor.focusSplit(this.state.splitId);
    }
  }

  /**
   * Open a file in the source split (for navigation operations)
   *
   * @param filePath - Path to the file to open
   * @param line - Line number to jump to (1-indexed)
   * @param column - Column number to jump to (1-indexed)
   */
  async openInSource(filePath: string, line: number, column: number): Promise<void> {
    if (this.state.sourceSplitId === null) {
      return;
    }

    // Focus source split and open file at location
    this.editor.focusSplit(this.state.sourceSplitId);
    this.editor.openFile(filePath, line, column);

    // Focus back on panel
    this.focusPanel();
  }
}
