/// <reference path="./fresh.d.ts" />

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
 * Create a VirtualBufferFactory bound to a specific editor instance.
 *
 * @example
 * ```typescript
 * const editor = getEditor();
 * const bufferFactory = createVirtualBufferFactory(editor);
 *
 * // Create buffer as a tab in current split
 * const bufferId = await bufferFactory.create({
 *   name: "*Help*",
 *   mode: "help-manual",
 *   entries: helpEntries,
 * });
 * ```
 */
export function createVirtualBufferFactory(editor: EditorAPI) {
  return {
    /**
     * Create a virtual buffer as a new tab in the current split
     */
    async create(options: VirtualBufferOptions): Promise<number> {
      const {
        name,
        mode,
        entries,
        showLineNumbers = false,
        editingDisabled = true,
        readOnly = true,
      } = options;

      return await editor.createVirtualBuffer({
        name,
        mode,
        readOnly,
        entries,
        showLineNumbers,
        editingDisabled,
      });
    },

    /**
     * Create a virtual buffer in an existing split
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
        readOnly,
        entries,
        splitId,
        showLineNumbers,
        editingDisabled,
      });
    },

    /**
     * Create a virtual buffer in a new split
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
        readOnly,
        entries,
        ratio,
        panelId,
        showLineNumbers,
        editingDisabled,
      });
    },

    /**
     * Update content of an existing virtual buffer
     */
    updateContent(bufferId: number, entries: TextPropertyEntry[]): void {
      editor.setVirtualBufferContent(bufferId, entries);
    },
  };
}
