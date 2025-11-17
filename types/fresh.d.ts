/**
 * Fresh Editor TypeScript Plugin API
 *
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generated from src/ts_runtime.rs by build.rs
 *
 * This file provides type definitions for the Fresh editor's TypeScript plugin system.
 * Plugins have access to the global `editor` object which provides methods to:
 * - Query editor state (buffers, cursors, viewports)
 * - Modify buffer content (insert, delete text)
 * - Add visual decorations (overlays, highlighting)
 * - Interact with the editor UI (status messages, prompts)
 */

declare global {
  /**
   * Global editor API object available to all TypeScript plugins
   */
  const editor: EditorAPI;
}

/**
 * Buffer identifier (unique numeric ID)
 */
type BufferId = number;

/**
 * Result of spawning an external process
 */
interface SpawnResult {
  stdout: string;
  stderr: string;
  exit_code: number;
}

/**
 * File stat information
 */
interface FileStat {
  exists: boolean;
  is_file: boolean;
  is_dir: boolean;
  size: number;
  readonly: boolean;
}

/**
 * Directory entry information
 */
interface DirEntry {
  name: string;
  is_file: boolean;
  is_dir: boolean;
}

/**
 * Buffer information
 */
interface BufferInfo {
  id: number;
  path: string;
  modified: boolean;
  length: number;
}

/**
 * Selection range
 */
interface SelectionRange {
  start: number;
  end: number;
}

/**
 * Cursor information with optional selection
 */
interface CursorInfo {
  position: number;
  selection: SelectionRange | null;
}

/**
 * Viewport information
 */
interface ViewportInfo {
  top_byte: number;
  left_column: number;
  width: number;
  height: number;
}

/**
 * Suggestion for prompt autocomplete
 */
interface PromptSuggestion {
  text: string;
  description?: string;
  value?: string;
  disabled?: boolean;
  keybinding?: string;
}

/**
 * Text property entry for virtual buffers
 * Each entry contains text and associated metadata properties
 */
interface TextPropertyEntry {
  text: string;
  properties: Record<string, unknown>;
}

/**
 * Options for creating a virtual buffer in a split
 */
interface CreateVirtualBufferOptions {
  /** Display name (e.g., "*Diagnostics*") */
  name: string;
  /** Mode name for buffer-local keybindings (e.g., "diagnostics-list") */
  mode: string;
  /** Whether the buffer is read-only */
  read_only: boolean;
  /** Entries with text and embedded properties */
  entries: TextPropertyEntry[];
  /** Split ratio (0.0 to 1.0, where 0.7 = original takes 70%, new buffer takes 30%) */
  ratio: number;
  /** Optional panel ID for idempotent operations (if panel exists, update content) */
  panel_id?: string;
  /** Whether to show line numbers in the buffer (default true) */
  show_line_numbers?: boolean;
  /** Whether to show cursors in the buffer (default true) */
  show_cursors?: boolean;
}

/**
 * Main editor API interface
 */
interface EditorAPI {
  // === Status and Logging ===
  setStatus(message: string): void;
  debug(message: string): void;

  // === Buffer Queries ===
  getActiveBufferId(): number;
  getCursorPosition(): number;
  getBufferPath(buffer_id: number): string;
  getBufferLength(buffer_id: number): number;
  isBufferModified(buffer_id: number): boolean;
  getActiveSplitId(): number;
  getBufferText(buffer_id: number, start: number, end: number): string;
  getCursorLine(): number;
  getAllCursorPositions(): number[];

  // === Buffer Info Queries ===
  /**
   * Get full information about a buffer
   * @param buffer_id - Buffer ID
   * @returns BufferInfo object or null if buffer not found
   */
  getBufferInfo(buffer_id: number): BufferInfo | null;

  /**
   * List all open buffers
   * @returns Array of BufferInfo objects
   */
  listBuffers(): BufferInfo[];

  /**
   * Get primary cursor with selection info
   * @returns CursorInfo object or null if no cursor
   */
  getPrimaryCursor(): CursorInfo | null;

  /**
   * Get all cursors (for multi-cursor support)
   * @returns Array of CursorInfo objects
   */
  getAllCursors(): CursorInfo[];

  /**
   * Get viewport information
   * @returns ViewportInfo object or null if no viewport
   */
  getViewport(): ViewportInfo | null;

  // === Prompt Operations ===
  /**
   * Start an interactive prompt
   * @param label - Label to display (e.g., "Git grep: ")
   * @param promptType - Type identifier (e.g., "git-grep")
   * @returns true if prompt was started successfully
   */
  startPrompt(label: string, promptType: string): boolean;

  /**
   * Set suggestions for the current prompt
   * @param suggestions - Array of suggestions to display
   * @returns true if suggestions were set successfully
   */
  setPromptSuggestions(suggestions: PromptSuggestion[]): boolean;

  // === Buffer Mutations ===
  insertText(buffer_id: number, position: number, text: string): boolean;
  deleteRange(buffer_id: number, start: number, end: number): boolean;
  insertAtCursor(text: string): boolean;
  registerCommand(name: string, description: string, action: string, contexts: string): boolean;
  openFile(path: string, line: number, column: number): boolean;
  openFileInSplit(split_id: number, path: string, line: number, column: number): boolean;

  // === Async Operations ===
  /**
   * Spawn an external process asynchronously
   * @param command - Command to execute (e.g., "git", "npm")
   * @param args - Array of command arguments (default: [])
   * @param cwd - Optional working directory (default: null)
   * @returns Promise with stdout, stderr, and exit_code
   */
  spawnProcess(command: string, args?: string[], cwd?: string | null): Promise<SpawnResult>;

  // === Overlay Operations ===
  addOverlay(buffer_id: number, overlay_id: string, start: number, end: number, r: number, g: number, b: number, underline: boolean): boolean;
  removeOverlay(buffer_id: number, overlay_id: string): boolean;
  removeOverlaysByPrefix(buffer_id: number, prefix: string): boolean;
  clearAllOverlays(buffer_id: number): boolean;

  // === File System Operations ===
  /**
   * Read a file's contents asynchronously
   * @param path - Absolute or relative path to the file
   * @returns Promise resolving to file contents as string
   */
  readFile(path: string): Promise<string>;

  /**
   * Write content to a file asynchronously
   * @param path - Absolute or relative path to the file
   * @param content - String content to write
   * @returns Promise resolving when write completes
   */
  writeFile(path: string, content: string): Promise<void>;

  /**
   * Check if a file or directory exists
   * @param path - Path to check
   * @returns true if path exists, false otherwise
   */
  fileExists(path: string): boolean;

  /**
   * Get file/directory metadata
   * @param path - Path to stat
   * @returns FileStat object with existence, type, size, and permissions info
   */
  fileStat(path: string): FileStat;

  // === Environment Operations ===
  /**
   * Get an environment variable
   * @param name - Name of environment variable
   * @returns Value if set, null if not set
   */
  getEnv(name: string): string | null;

  /**
   * Get the current working directory
   * @returns Absolute path to cwd
   */
  getCwd(): string;

  // === Path Operations ===
  /**
   * Join path components
   * @param parts - Path components to join
   * @returns Joined path string
   */
  pathJoin(...parts: string[]): string;

  /**
   * Get the directory name of a path
   * @param path - Path to process
   * @returns Parent directory path
   */
  pathDirname(path: string): string;

  /**
   * Get the base name of a path
   * @param path - Path to process
   * @returns File or directory name without parent path
   */
  pathBasename(path: string): string;

  /**
   * Get the file extension
   * @param path - Path to process
   * @returns Extension including dot (e.g., ".ts"), or empty string
   */
  pathExtname(path: string): string;

  /**
   * Check if a path is absolute
   * @param path - Path to check
   * @returns true if path is absolute, false otherwise
   */
  pathIsAbsolute(path: string): boolean;

  /**
   * Read directory contents
   * @param path - Path to directory
   * @returns Array of directory entries with name and type info
   */
  readDir(path: string): DirEntry[];

  // === Event/Hook Operations ===
  /**
   * Register an event handler
   * @param eventName - Name of the event (e.g., "buffer_save", "cursor_moved")
   * @param handlerName - Name of a global JavaScript function to call
   * @returns true if registration succeeded
   * @example
   * // Define global handler
   * globalThis.onSave = (data) => { console.log("Saved:", data); };
   * // Register it
   * editor.on("buffer_save", "onSave");
   */
  on(eventName: string, handlerName: string): boolean;

  /**
   * Unregister an event handler
   * @param eventName - Name of the event
   * @param handlerName - Name of the handler to remove
   * @returns true if handler was found and removed
   */
  off(eventName: string, handlerName: string): boolean;

  /**
   * Get list of registered handlers for an event
   * @param eventName - Name of the event
   * @returns Array of handler function names
   */
  getHandlers(eventName: string): string[];

  // === Virtual Buffer Operations ===

  /**
   * Create a virtual buffer in a horizontal split below the current pane
   * This is the key operation for creating diagnostic panels, search results, etc.
   * @param options - Configuration for the virtual buffer
   * @returns true if buffer creation was initiated successfully
   * @example
   * editor.createVirtualBufferInSplit({
   *   name: "*Diagnostics*",
   *   mode: "diagnostics-list",
   *   read_only: true,
   *   entries: [
   *     { text: "Error at line 42\n", properties: { severity: "error", line: 42 } },
   *     { text: "Warning at line 100\n", properties: { severity: "warning", line: 100 } }
   *   ],
   *   ratio: 0.7, // Original pane takes 70%, new buffer takes 30%
   *   panel_id: "diagnostics",
   *   show_line_numbers: false,
   *   show_cursors: false
   * });
   */
  createVirtualBufferInSplit(options: CreateVirtualBufferOptions): boolean;

  /**
   * Define a buffer mode with keybindings
   * Modes can inherit from parent modes (e.g., "diagnostics-list" inherits from "special")
   * @param name - Mode name (e.g., "diagnostics-list")
   * @param parent - Parent mode name for inheritance (e.g., "special"), or null
   * @param bindings - Array of [key_string, command_name] pairs
   * @param readOnly - Whether buffers in this mode are read-only (default false)
   * @returns true if mode was defined successfully
   * @example
   * editor.defineMode("diagnostics-list", "special", [
   *   ["Return", "diagnostics_goto"],
   *   ["n", "diagnostics_next"],
   *   ["p", "diagnostics_prev"],
   *   ["q", "close_buffer"]
   * ], true);
   */
  defineMode(name: string, parent: string | null, bindings: [string, string][], readOnly?: boolean): boolean;

  /**
   * Switch the current split to display a buffer
   * @param bufferId - ID of the buffer to show
   * @returns true if buffer was shown successfully
   */
  showBuffer(bufferId: number): boolean;

  /**
   * Get text properties at the cursor position in a buffer
   * Returns all properties for text ranges that contain the cursor position
   * @param bufferId - ID of the buffer to query
   * @returns Array of property objects (key-value maps)
   * @example
   * const props = editor.getTextPropertiesAtCursor(bufferId);
   * if (props.length > 0 && props[0].location) {
   *   editor.openFile(props[0].location.file, props[0].location.line, 0);
   * }
   */
  getTextPropertiesAtCursor(bufferId: number): Record<string, unknown>[];

  /**
   * Set the content of a virtual buffer with text properties
   * Replaces all content in the buffer
   * @param bufferId - ID of the virtual buffer
   * @param entries - Array of text entries with properties
   * @returns true if content was set successfully
   */
  setVirtualBufferContent(bufferId: number, entries: TextPropertyEntry[]): boolean;
}

// Export for module compatibility
export {};
