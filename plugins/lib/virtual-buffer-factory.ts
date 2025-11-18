/// <reference path="../../types/fresh.d.ts" />

/**
 * Options for creating a virtual buffer
 */
export interface VirtualBufferOptions {
  /** Display name (e.g., "*Commit Details*") */
  name: string;
  /** Mode name for keybindings */
  mode: string;
  /** Text property entries */
  entries: TextPropertyEntry[];
  /** Whether to show line numbers (default false) */
  showLineNumbers?: boolean;
  /** Whether editing is disabled (default true) */
  editingDisabled?: boolean;
  /** Whether buffer is read-only (default true) */
  readOnly?: boolean;
}

/**
 * Options for creating a virtual buffer in a new split
 */
export interface SplitBufferOptions extends VirtualBufferOptions {
  /** Split ratio (default 0.3) */
  ratio?: number;
  /** Panel ID for idempotent operations */
  panelId?: string;
}

/**
 * VirtualBufferFactory - Simplified virtual buffer creation
 *
 * Provides convenience methods for creating virtual buffers with
 * sensible defaults for read-only panel views.
 *
 * @example
 * ```typescript
 * // Create buffer in existing split (e.g., commit detail view)
 * const bufferId = await VirtualBufferFactory.createInSplit(splitId, {
 *   name: "*Commit Details*",
 *   mode: "git-commit-detail",
 *   entries: detailEntries,
 * });
 *
 * // Create buffer in new split
 * const bufferId = await VirtualBufferFactory.createWithSplit({
 *   name: "*References*",
 *   mode: "references-list",
 *   entries: refEntries,
 *   ratio: 0.4,
 * });
 * ```
 */
export const VirtualBufferFactory = {
  /**
   * Create a virtual buffer in an existing split
   *
   * @param splitId - Target split ID
   * @param options - Buffer configuration
   * @returns Buffer ID
   */
  async createInSplit(splitId: number, options: VirtualBufferOptions): Promise<number> {
    const {
      name,
      mode,
      entries,
      showLineNumbers = false,
      editingDisabled = true,
      readOnly = true,
    } = options;

    return await editor.createVirtualBufferInExistingSplit({
      name,
      mode,
      read_only: readOnly,
      entries,
      split_id: splitId,
      show_line_numbers: showLineNumbers,
      editing_disabled: editingDisabled,
    });
  },

  /**
   * Create a virtual buffer in a new split
   *
   * @param options - Buffer and split configuration
   * @returns Buffer ID
   */
  async createWithSplit(options: SplitBufferOptions): Promise<number> {
    const {
      name,
      mode,
      entries,
      ratio = 0.3,
      panelId,
      showLineNumbers = false,
      editingDisabled = true,
      readOnly = true,
    } = options;

    return await editor.createVirtualBufferInSplit({
      name,
      mode,
      read_only: readOnly,
      entries,
      ratio,
      panel_id: panelId,
      show_line_numbers: showLineNumbers,
      editing_disabled: editingDisabled,
    });
  },

  /**
   * Update content of an existing virtual buffer
   *
   * @param bufferId - Buffer to update
   * @param entries - New entries
   */
  updateContent(bufferId: number, entries: TextPropertyEntry[]): void {
    editor.setVirtualBufferContent(bufferId, entries);
  },
};
