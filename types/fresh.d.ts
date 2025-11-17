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
}

// Export for module compatibility
export {};
