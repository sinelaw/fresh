/**
 * Fresh Editor TypeScript Plugin API
 *
 * This file provides type definitions for the Fresh editor's TypeScript plugin system.
 * Plugins have access to the global `editor` object which provides methods to:
 * - Query editor state (buffers, cursors, viewports)
 * - Modify buffer content (insert, delete text)
 * - Add visual decorations (overlays, highlighting)
 * - Interact with the editor UI (status messages, prompts)
 *
 * Note: types/fresh.d.ts is auto-generated from this template and src/ts_runtime.rs
 *
 * ## Core Concepts
 *
 * ### Buffers
 * A buffer holds text content and may or may not be associated with a file.
 * Each buffer has a unique numeric ID that persists for the editor session.
 * Buffers track their content, modification state, cursor positions, and path.
 * All text operations (insert, delete, read) use byte offsets, not character indices.
 *
 * ### Splits
 * A split is a viewport pane that displays a buffer. The editor can have multiple
 * splits arranged in a tree layout. Each split shows exactly one buffer, but the
 * same buffer can be displayed in multiple splits. Use split IDs to control which
 * pane displays which buffer.
 *
 * ### Virtual Buffers
 * Special buffers created by plugins to display structured data like search results,
 * diagnostics, or git logs. Virtual buffers support text properties (metadata attached
 * to text ranges) that plugins can query when the user selects a line. Unlike normal
 * buffers, virtual buffers are typically read-only and not backed by files.
 *
 * ### Text Properties
 * Metadata attached to text ranges in virtual buffers. Each entry has text content
 * and a properties object with arbitrary key-value pairs. Use `getTextPropertiesAtCursor`
 * to retrieve properties at the cursor position (e.g., to get file/line info for "go to").
 *
 * ### Overlays
 * Visual decorations applied to buffer text without modifying content. Overlays can
 * change text color and add underlines. Use overlay IDs to manage them; prefix IDs
 * enable batch removal (e.g., "lint:" prefix for all linter highlights).
 *
 * ### Modes
 * Keybinding contexts that determine how keypresses are interpreted. Each buffer has
 * a mode (e.g., "normal", "insert", "special"). Custom modes can inherit from parents
 * and define buffer-local keybindings. Virtual buffers typically use custom modes.
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

/** View token wire format for view transforms */
interface ViewTokenWire {
  /** Source byte offset (null for injected view-only content) */
  source_offset: number | null;
  /** Token kind: Text, Newline, Space, or Break */
  kind: ViewTokenWireKind;
}

/** View token kind discriminated union */
type ViewTokenWireKind =
  | { Text: string }
  | "Newline"
  | "Space"
  | "Break";

/** Layout hints for compose mode */
interface LayoutHints {
  /** Optional compose width for centering/wrapping */
  compose_width?: number | null;
  /** Optional column guides for tables */
  column_guides?: number[] | null;
}

/** Result from spawnProcess */
interface SpawnResult {
  /** Complete stdout as string. Newlines preserved; trailing newline included. */
  stdout: string;
  /** Complete stderr as string. Contains error messages and warnings. */
  stderr: string;
  /** Process exit code. 0 usually means success; -1 if process was killed. */
  exit_code: number;
}

/** Result from spawnBackgroundProcess - just the process ID */
interface BackgroundProcessResult {
  /** Unique process ID for later reference (kill, status check) */
  process_id: number;
}

/** File stat information */
interface FileStat {
  /** Whether the path exists */
  exists: boolean;
  /** Whether the path is a file */
  is_file: boolean;
  /** Whether the path is a directory */
  is_dir: boolean;
  /** File size in bytes */
  size: number;
  /** Whether the file is read-only */
  readonly: boolean;
}

/** Buffer information */
interface BufferInfo {
  /** Unique buffer ID */
  id: number;
  /** File path (empty string if no path) */
  path: string;
  /** Whether buffer has unsaved changes */
  modified: boolean;
  /** Buffer length in bytes */
  length: number;
}

/** Diff vs last save for a buffer */
interface TsBufferSavedDiff {
  equal: boolean;
  byte_ranges: [number, number][];
  line_ranges?: [number, number][] | null;
}

/** Selection range */
interface SelectionRange {
  /** Start byte position */
  start: number;
  /** End byte position */
  end: number;
}

/** Cursor information with optional selection */
interface CursorInfo {
  /** Byte position of the cursor */
  position: number;
  /** Selection range if text is selected, null otherwise */
  selection?: SelectionRange | null;
}

/** LSP diagnostic position */
interface TsDiagnosticPosition {
  line: number;
  character: number;
}

/** LSP diagnostic range */
interface TsDiagnosticRange {
  start: TsDiagnosticPosition;
  end: TsDiagnosticPosition;
}

/** LSP diagnostic item for TypeScript plugins */
interface TsDiagnostic {
  /** File URI (e.g., "file:///path/to/file.rs") */
  uri: string;
  /** Diagnostic severity: 1=Error, 2=Warning, 3=Info, 4=Hint */
  severity: number;
  /** Diagnostic message */
  message: string;
  /** Source of the diagnostic (e.g., "rust-analyzer") */
  source?: string | null;
  /** Location range in the file */
  range: TsDiagnosticRange;
}

/** Viewport information */
interface ViewportInfo {
  /** Byte offset of the top-left visible position */
  top_byte: number;
  /** Column offset for horizontal scrolling */
  left_column: number;
  /** Viewport width in columns */
  width: number;
  /** Viewport height in rows */
  height: number;
}

/** Suggestion for prompt autocomplete */
interface PromptSuggestion {
  /** Display text for the suggestion */
  text: string;
  /** Optional description shown alongside */
  description?: string | null;
  /** Optional value to use instead of text when selected */
  value?: string | null;
  /** Whether the suggestion is disabled */
  disabled?: boolean | null;
  /** Optional keybinding hint */
  keybinding?: string | null;
}

/** Directory entry from readDir */
interface DirEntry {
  /** Entry name only (not full path). Join with parent path to get absolute path. */
  name: string;
  /** True if entry is a regular file */
  is_file: boolean;
  /** True if entry is a directory. Note: symlinks report the target type. */
  is_dir: boolean;
}

/** Entry for virtual buffer content with embedded metadata */
interface TextPropertyEntry {
  /** Text to display. Include trailing newline for separate lines. */
  text: string;
  /** Arbitrary metadata queryable via getTextPropertiesAtCursor. */
  properties: Record<string, unknown>;
}

/** Result from createVirtualBufferInSplit */
interface CreateVirtualBufferResult {
  buffer_id: number;
  split_id?: number | null;
}

/** Configuration for createVirtualBufferInSplit */
interface CreateVirtualBufferOptions {
  /** Buffer name shown in status bar (convention: "*Name*") */
  name: string;
  /** Mode for keybindings; define with defineMode first */
  mode: string;
  /** Prevent text modifications */
  read_only: boolean;
  /** Content with embedded metadata */
  entries: TextPropertyEntry[];
  /** Split ratio (0.3 = new pane gets 30% of space) */
  ratio: number;
  /** Split direction: "horizontal" (below) or "vertical" (side-by-side). Default: horizontal */
  direction?: string | null;
  /** If set and panel exists, update content instead of creating new buffer */
  panel_id?: string | null;
  /** Show line numbers gutter (default: true) */
  show_line_numbers?: boolean | null;
  /** Show cursor in buffer (default: true) */
  show_cursors?: boolean | null;
  /** Disable all editing commands (default: false) */
  editing_disabled?: boolean | null;
}

/** Options for creating a virtual buffer in an existing split */
interface CreateVirtualBufferInExistingSplitOptions {
  /** Display name (e.g., "*Commit Details*") */
  name: string;
  /** Mode name for buffer-local keybindings */
  mode: string;
  /** Whether the buffer is read-only */
  read_only: boolean;
  /** Entries with text and embedded properties */
  entries: TextPropertyEntry[];
  /** Target split ID where the buffer should be displayed */
  split_id: number;
  /** Whether to show line numbers in the buffer (default true) */
  show_line_numbers?: boolean | null;
  /** Whether to show cursors in the buffer (default true) */
  show_cursors?: boolean | null;
  /** Whether editing is disabled for this buffer (default false) */
  editing_disabled?: boolean | null;
}

/** Options for creating a virtual buffer in the current split as a new tab */
interface CreateVirtualBufferInCurrentSplitOptions {
  /** Display name (e.g., "*Help*") */
  name: string;
  /** Mode name for buffer-local keybindings */
  mode: string;
  /** Whether the buffer is read-only */
  read_only: boolean;
  /** Entries with text and embedded properties */
  entries: TextPropertyEntry[];
  /** Whether to show line numbers in the buffer (default false for help/docs) */
  show_line_numbers?: boolean | null;
  /** Whether to show cursors in the buffer (default true) */
  show_cursors?: boolean | null;
  /** Whether editing is disabled for this buffer (default false) */
  editing_disabled?: boolean | null;
}

/**
 * Main editor API interface
 */
interface EditorAPI {
  // === Status and Logging ===
  /**
   * Display a transient message in the editor's status bar
   *
   * The message will be shown until the next status update or user action.
   * Use for feedback on completed operations (e.g., "File saved", "2 matches found").
   * @param message - Text to display; keep short (status bar has limited width)
   */
  setStatus(message: string): void;
  /**
   * Log a debug message to the editor's trace output
   *
   * Messages appear in stderr when running with RUST_LOG=debug.
   * Useful for plugin development and troubleshooting.
   * @param message - Debug message; include context like function name and relevant values
   */
  debug(message: string): void;

  // === Buffer Queries ===
  /**
   * Get the buffer ID of the focused editor pane
   *
   * Returns 0 if no buffer is active (rare edge case).
   * Use this ID with other buffer operations like insertText.
   */
  getActiveBufferId(): number;
  /**
   * Get the byte offset of the primary cursor in the active buffer
   *
   * Returns 0 if no cursor exists. For multi-cursor scenarios, use getAllCursors
   * to get all cursor positions with selection info.
   * Note: This is a byte offset, not a character index (UTF-8 matters).
   */
  getCursorPosition(): number;
  /**
   * Get the absolute file path for a buffer
   *
   * Returns empty string for unsaved buffers or virtual buffers.
   * The path is always absolute. Use this to determine file type,
   * construct related paths, or display to the user.
   * @param buffer_id - Target buffer ID
   */
  getBufferPath(buffer_id: number): string;
  /**
   * Get the total byte length of a buffer's content
   *
   * Returns 0 if buffer doesn't exist.
   * @param buffer_id - Target buffer ID
   */
  getBufferLength(buffer_id: number): number;
  /**
   * Check if a buffer has been modified since last save
   *
   * Returns false if buffer doesn't exist or has never been saved.
   * Virtual buffers are never considered modified.
   * @param buffer_id - Target buffer ID
   */
  isBufferModified(buffer_id: number): boolean;
  /**
   * Get the ID of the focused split pane
   *
   * Use with focusSplit, setSplitBuffer, or createVirtualBufferInExistingSplit
   * to manage split layouts.
   */
  getActiveSplitId(): number;
  /**
   * Get the line number of the primary cursor (1-indexed)
   *
   * Line numbers start at 1. Returns 1 if no cursor exists.
   * For byte offset use getCursorPosition instead.
   */
  getCursorLine(): number;
  /**
   * Get byte offsets of all cursors (multi-cursor support)
   *
   * Returns array of positions; empty if no cursors. Primary cursor
   * is typically first. For selection info use getAllCursors instead.
   */
  getAllCursorPositions(): number[];
  /**
   * Check if a background process is still running
   *
   * @param process_id - ID returned from spawnBackgroundProcess
   * @returns true if process is running, false if not found or exited
   */
  isProcessRunning(#[bigint] process_id: number): boolean;
  /** Get diff vs last saved snapshot for a buffer */
  getBufferSavedDiff(buffer_id: number): TsBufferSavedDiff | null;
  /**
   * Get all LSP diagnostics across all files
   * @returns Array of Diagnostic objects with file URI, severity, message, and range
   */
  getAllDiagnostics(): TsDiagnostic[];

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
   * @param prompt_type - Type identifier (e.g., "git-grep")
   * @returns true if prompt was started successfully
   */
  startPrompt(label: string, prompt_type: string): boolean;
  /**
   * Set suggestions for the current prompt
   * @param suggestions - Array of suggestions to display
   * @returns true if suggestions were set successfully
   */
  setPromptSuggestions(suggestions: PromptSuggestion[]): boolean;

  // === Buffer Mutations ===
  /**
   * Apply a theme by name
   *
   * Loads and applies the specified theme immediately. The theme can be a built-in
   * theme name or a custom theme from the themes directory.
   * @param theme_name - Name of the theme to apply (e.g., "dark", "light", "my-custom-theme")
   */
  applyTheme(theme_name: string): void;
  /**
   * Reload configuration from file
   *
   * After a plugin saves config changes to the config file, call this to reload
   * the editor's in-memory configuration. This ensures the editor and plugins
   * stay in sync with the saved config.
   */
  reloadConfig(): void;
  /**
   * Copy text to the system clipboard
   *
   * Copies the provided text to both the internal and system clipboard.
   * Uses OSC 52 and arboard for cross-platform compatibility.
   * @param text - Text to copy to clipboard
   */
  setClipboard(text: string): void;
  /**
   * Insert text at a byte position in a buffer
   *
   * Text is inserted before the byte at position. Position must be valid
   * (0 to buffer length). Insertion shifts all text after position.
   * Operation is asynchronous; returns true if command was sent successfully.
   * @param buffer_id - Target buffer ID
   * @param position - Byte offset where text will be inserted (must be at char boundary)
   * @param text - UTF-8 text to insert
   */
  insertText(buffer_id: number, position: number, text: string): boolean;
  /**
   * Delete a byte range from a buffer
   *
   * Deletes bytes from start (inclusive) to end (exclusive).
   * Both positions must be at valid UTF-8 char boundaries.
   * Operation is asynchronous; returns true if command was sent successfully.
   * @param buffer_id - Target buffer ID
   * @param start - Start byte offset (inclusive)
   * @param end - End byte offset (exclusive)
   */
  deleteRange(buffer_id: number, start: number, end: number): boolean;
  /**
   * Clear all overlays in a namespace
   * @param buffer_id - The buffer ID
   * @param namespace - The namespace to clear
   * @returns true if successful
   */
  clearNamespace(buffer_id: number, namespace: string): boolean;
  /**
   * Enable/disable line numbers for a buffer
   * @param buffer_id - The buffer ID
   * @param enabled - Whether to show line numbers
   * @returns true if successful
   */
  setLineNumbers(buffer_id: number, enabled: boolean): boolean;
  /**
   * Add a virtual line above or below a source line
   * @param buffer_id - The buffer ID
   * @param position - Byte position to anchor the virtual line to
   * @param text - The text content of the virtual line
   * @param fg_r - Foreground red color component (0-255)
   * @param fg_g - Foreground green color component (0-255)
   * @param fg_b - Foreground blue color component (0-255)
   * @param bg_r - Background red color component (0-255), -1 for transparent
   * @param bg_g - Background green color component (0-255), -1 for transparent
   * @param bg_b - Background blue color component (0-255), -1 for transparent
   * @param above - Whether to insert above (true) or below (false) the line
   * @param namespace - Namespace for bulk removal (e.g., "git-blame")
   * @param priority - Priority for ordering multiple lines at same position
   * @returns true if virtual line was added
   */
  addVirtualLine(buffer_id: number, position: number, text: string, fg_r: number, fg_g: number, fg_b: number, bg_r: i16, bg_g: i16, bg_b: i16, above: boolean, namespace: string, priority: number): boolean;
  /**
   * Set a line indicator in the gutter's indicator column
   * @param buffer_id - The buffer ID
   * @param line - Line number (0-indexed)
   * @param namespace - Namespace for grouping (e.g., "git-gutter", "breakpoints")
   * @param symbol - Symbol to display (e.g., "│", "●", "★")
   * @param r - Red color component (0-255)
   * @param g - Green color component (0-255)
   * @param b - Blue color component (0-255)
   * @param priority - Priority for display when multiple indicators exist (higher wins)
   * @returns true if indicator was set
   */
  setLineIndicator(buffer_id: number, line: number, namespace: string, symbol: string, r: number, g: number, b: number, priority: number): boolean;
  /**
   * Clear all line indicators for a specific namespace
   * @param buffer_id - The buffer ID
   * @param namespace - Namespace to clear (e.g., "git-gutter")
   * @returns true if indicators were cleared
   */
  clearLineIndicators(buffer_id: number, namespace: string): boolean;
  /**
   * Submit a transformed view stream for a viewport
   * @param buffer_id - Buffer to apply the transform to
   * @param start - Viewport start byte
   * @param end - Viewport end byte
   * @param tokens - Array of tokens with source offsets
   * @param source_map - Array of source offsets (null for injected)
   * @param layout_hints - Optional layout hints (compose width, column guides)
   */
  submitViewTransform(buffer_id: number, split_id?: number | null, start: number, end: number, tokens: ViewTokenWire[], layout_hints?: LayoutHints | null): boolean;
  /**
   * Clear view transform for a buffer/split (returns to normal rendering)
   * @param buffer_id - Buffer ID
   * @param split_id - Optional split ID (uses active split if not specified)
   * @returns true if clear succeeded
   */
  clearViewTransform(buffer_id: number, split_id?: number | null): boolean;
  /**
   * Insert text at the current cursor position in the active buffer
   * @param text - The text to insert
   * @returns true if insertion succeeded
   */
  insertAtCursor(text: string): boolean;
  /**
   * Register a custom command that can be triggered by keybindings or the command palette
   * @param name - Unique command name (e.g., "my_plugin_action")
   * @param description - Human-readable description
   * @param action - JavaScript function name to call when command is triggered
   * @param contexts - Comma-separated list of contexts, including both built-in (normal, prompt, popup,
   * fileexplorer, menu) and custom plugin-defined contexts (e.g., "normal,config-editor")
   * @param source - Plugin source name (empty string for builtin)
   * @returns true if command was registered
   */
  registerCommand(name: string, description: string, action: string, contexts: string, source: string): boolean;
  /**
   * Unregister a custom command by name
   * @param name - The name of the command to unregister
   * @returns true if the command was successfully unregistered
   */
  unregisterCommand(name: string): boolean;
  /**
   * Set or unset a custom context for command visibility
   * Custom contexts allow plugins to control when their commands are available.
   * For example, setting "config-editor" context makes config editor commands visible.
   * @param name - Context name (e.g., "config-editor")
   * @param active - Whether the context is active (true = set, false = unset)
   * @returns true if the context was updated
   */
  setContext(name: string, active: boolean): boolean;
  /**
   * Open a file in the editor, optionally at a specific location
   * @param path - File path to open
   * @param line - Line number to jump to (0 for no jump)
   * @param column - Column number to jump to (0 for no jump)
   * @returns true if file was opened
   */
  openFile(path: string, line: number, column: number): boolean;
  /**
   * Open a file in a specific split pane
   * @param split_id - The split ID to open the file in
   * @param path - File path to open
   * @param line - Line number to jump to (0 for no jump)
   * @param column - Column number to jump to (0 for no jump)
   * @returns true if file was opened
   */
  openFileInSplit(split_id: number, path: string, line: number, column: number): boolean;
  /**
   * Spawn a long-running background process
   *
   * Unlike spawnProcess which waits for completion, this starts a process
   * in the background and returns immediately with a process ID.
   * Use killProcess(id) to terminate the process later.
   * Use isProcessRunning(id) to check if it's still running.
   *
   * @param command - Program name (searched in PATH) or absolute path
   * @param args - Command arguments (each array element is one argument)
   * @param cwd - Working directory; null uses editor's cwd
   * @returns Object with process_id for later reference
   * @example
   * const proc = await editor.spawnBackgroundProcess("asciinema", ["rec", "output.cast"]);
   * // Later...
   * await editor.killProcess(proc.process_id);
   */
  spawnBackgroundProcess(command: string, args: string[], cwd?: string | null): Promise<BackgroundProcessResult>;
  /**
   * Kill a background process by ID
   *
   * Sends SIGTERM to gracefully terminate the process.
   * Returns true if the process was found and killed, false if not found.
   *
   * @param process_id - ID returned from spawnBackgroundProcess
   * @returns true if process was killed, false if not found
   */
  killProcess(#[bigint] process_id: number): Promise<boolean>;
  /**
   * Start a prompt with pre-filled initial value
   * @param label - Label to display (e.g., "Git grep: ")
   * @param prompt_type - Type identifier (e.g., "git-grep")
   * @param initial_value - Initial text to pre-fill in the prompt
   * @returns true if prompt was started successfully
   */
  startPromptWithInitial(label: string, prompt_type: string, initial_value: string): boolean;
  /**
   * Send an arbitrary LSP request and receive the raw JSON response
   * @param language - Language ID (e.g., "cpp")
   * @param method - Full LSP method (e.g., "textDocument/switchSourceHeader")
   * @param params - Optional request payload
   * @returns Promise resolving to the JSON response value
   */
  sendLspRequest(language: string, method: string, params?: unknown | null): Promise<unknown>;
  /**
   * Set the ratio of a split container
   * @param split_id - ID of the split
   * @param ratio - Ratio between 0.0 and 1.0 (0.5 = equal split)
   * @returns true if the ratio was set successfully
   */
  setSplitRatio(split_id: number, ratio: number): boolean;
  /**
   * Distribute all visible splits evenly
   * This adjusts the ratios of all container splits so each leaf split gets equal space
   * @returns true if the command was sent successfully
   */
  distributeSplitsEvenly(): boolean;
  /**
   * Set cursor position in a buffer (also scrolls viewport to show cursor)
   * @param buffer_id - ID of the buffer
   * @param position - Byte offset position for the cursor
   * @returns true if the command was sent successfully
   */
  setBufferCursor(buffer_id: number, position: number): boolean;

  // === Async Operations ===
  /**
   * Run an external command and capture its output
   *
   * Waits for process to complete before returning. For long-running processes,
   * consider if this will block your plugin. Output is captured completely;
   * very large outputs may use significant memory.
   * @param command - Program name (searched in PATH) or absolute path
   * @param args - Command arguments (each array element is one argument)
   * @param cwd - Working directory; null uses editor's cwd
   * @example
   * const result = await editor.spawnProcess("git", ["log", "--oneline", "-5"]);
   * if (result.exit_code !== 0) {
   * editor.setStatus(`git failed: ${result.stderr}`);
   * }
   */
  spawnProcess(command: string, args: string[], cwd?: string | null): Promise<SpawnResult>;

  // === Overlay Operations ===
  /**
   * Add a colored highlight overlay to text without modifying content
   *
   * Overlays are visual decorations that persist until explicitly removed.
   * Add an overlay (visual decoration) to a buffer
   * Use namespaces for easy batch removal (e.g., "spell", "todo").
   * Multiple overlays can apply to the same range; colors blend.
   * @param buffer_id - Target buffer ID
   * @param namespace - Optional namespace for grouping (use clearNamespace for batch removal)
   * @param start - Start byte offset
   * @param end - End byte offset
   * @param r - Red (0-255)
   * @param g - Green (0-255)
   * @param b - Blue (0-255)
   * @param underline - Add underline decoration
   * @param bold - Use bold text
   * @param italic - Use italic text
   * @returns true if overlay was added
   */
  addOverlay(buffer_id: number, namespace: string, start: number, end: number, r: number, g: number, b: number, underline: boolean, bold: boolean, italic: boolean): boolean;
  /**
   * Remove a specific overlay by its handle
   * @param buffer_id - The buffer ID
   * @param handle - The overlay handle to remove
   * @returns true if overlay was removed
   */
  removeOverlay(buffer_id: number, handle: string): boolean;
  /**
   * Clear all overlays that overlap with a byte range
   * @param buffer_id - The buffer ID
   * @param start - Start byte position (inclusive)
   * @param end - End byte position (exclusive)
   * @returns true if successful
   */
  clearOverlaysInRange(buffer_id: number, start: number, end: number): boolean;
  /**
   * Remove all overlays from a buffer
   * @param buffer_id - The buffer ID
   * @returns true if overlays were cleared
   */
  clearAllOverlays(buffer_id: number): boolean;
  /**
   * Add virtual text (inline decoration) at a position
   * @param buffer_id - The buffer ID
   * @param virtual_text_id - Unique identifier for this virtual text
   * @param position - Byte position to insert at
   * @param text - The virtual text to display
   * @param r - Red color component (0-255)
   * @param g - Green color component (0-255)
   * @param b - Blue color component (0-255)
   * @param before - Whether to insert before (true) or after (false) the position
   * @param use_bg - Whether to use the color as background (true) or foreground (false)
   * @returns true if virtual text was added
   */
  addVirtualText(buffer_id: number, virtual_text_id: string, position: number, text: string, r: number, g: number, b: number, before: boolean, use_bg: boolean): boolean;
  /**
   * Remove virtual text by ID
   * @param buffer_id - The buffer ID
   * @param virtual_text_id - The virtual text ID to remove
   * @returns true if virtual text was removed
   */
  removeVirtualText(buffer_id: number, virtual_text_id: string): boolean;
  /**
   * Remove all virtual texts with IDs starting with a prefix
   * @param buffer_id - The buffer ID
   * @param prefix - The prefix to match virtual text IDs against
   * @returns true if any virtual texts were removed
   */
  removeVirtualTextsByPrefix(buffer_id: number, prefix: string): boolean;
  /**
   * Remove all virtual texts from a buffer
   * @param buffer_id - The buffer ID
   * @returns true if virtual texts were cleared
   */
  clearVirtualTexts(buffer_id: number): boolean;
  /**
   * Clear all virtual texts in a namespace
   * @param buffer_id - The buffer ID
   * @param namespace - The namespace to clear (e.g., "git-blame")
   * @returns true if namespace was cleared
   */
  clearVirtualTextNamespace(buffer_id: number, namespace: string): boolean;
  /**
   * Force a refresh of line display for a buffer
   * @param buffer_id - The buffer ID
   * @returns true if refresh was triggered
   */
  refreshLines(buffer_id: number): boolean;

  // === File System Operations ===
  /**
   * Read entire file contents as UTF-8 string
   *
   * Throws if file doesn't exist, isn't readable, or isn't valid UTF-8.
   * For binary files, this will fail. For large files, consider memory usage.
   * @param path - File path (absolute or relative to cwd)
   */
  readFile(path: string): Promise<string>;
  /**
   * Write string content to a file, creating or overwriting
   *
   * Creates parent directories if they don't exist (behavior may vary).
   * Replaces file contents entirely; use readFile + modify + writeFile for edits.
   * @param path - Destination path (absolute or relative to cwd)
   * @param content - UTF-8 string to write
   */
  writeFile(path: string, content: string): Promise<[]>;
  /**
   * Check if a path exists (file, directory, or symlink)
   *
   * Does not follow symlinks; returns true for broken symlinks.
   * Use fileStat for more detailed information.
   * @param path - Path to check (absolute or relative to cwd)
   */
  fileExists(path: string): boolean;
  /**
   * Get metadata about a file or directory
   *
   * Follows symlinks. Returns exists=false for non-existent paths
   * rather than throwing. Size is in bytes; directories may report 0.
   * @param path - Path to stat (absolute or relative to cwd)
   */
  fileStat(path: string): FileStat;
  /**
   * List directory contents
   *
   * Returns unsorted entries with type info. Entry names are relative
   * to the directory (use pathJoin to construct full paths).
   * Throws on permission errors or if path is not a directory.
   * @param path - Directory path (absolute or relative to cwd)
   * @example
   * const entries = editor.readDir("/home/user");
   * for (const e of entries) {
   * const fullPath = editor.pathJoin("/home/user", e.name);
   * }
   */
  readDir(path: string): DirEntry[];

  // === Environment Operations ===
  /**
   * Get an environment variable
   * @param name - Name of environment variable
   * @returns Value if set, null if not set
   */
  getEnv(name: string): string;
  /**
   * Get the editor's current working directory
   *
   * Returns the editor's working directory (set when the editor was started).
   * Use as base for resolving relative paths and spawning processes.
   * Note: This returns the editor's stored working_dir, not process CWD,
   * which is important for test isolation.
   */
  getCwd(): string;

  // === Path Operations ===
  /**
   * Join path segments using the OS path separator
   *
   * Handles empty segments and normalizes separators.
   * If a segment is absolute, previous segments are discarded.
   * @param parts - Path segments to join
   * @example
   * pathJoin("/home", "user", "file.txt") // "/home/user/file.txt"
   * pathJoin("relative", "/absolute") // "/absolute"
   */
  pathJoin(parts: string[]): string;
  /**
   * Get the parent directory of a path
   *
   * Returns empty string for root paths or paths without parent.
   * Does not resolve symlinks or check existence.
   * @param path - File or directory path
   * @example
   * pathDirname("/home/user/file.txt") // "/home/user"
   * pathDirname("/") // ""
   */
  pathDirname(path: string): string;
  /**
   * Get the final component of a path
   *
   * Returns empty string for root paths.
   * Does not strip file extension; use pathExtname for that.
   * @param path - File or directory path
   * @example
   * pathBasename("/home/user/file.txt") // "file.txt"
   * pathBasename("/home/user/") // "user"
   */
  pathBasename(path: string): string;
  /**
   * Get the file extension including the dot
   *
   * Returns empty string if no extension. Only returns the last extension
   * for files like "archive.tar.gz" (returns ".gz").
   * @param path - File path
   * @example
   * pathExtname("file.txt") // ".txt"
   * pathExtname("archive.tar.gz") // ".gz"
   * pathExtname("Makefile") // ""
   */
  pathExtname(path: string): string;
  /**
   * Check if a path is absolute
   *
   * On Unix: starts with "/". On Windows: starts with drive letter or UNC path.
   * @param path - Path to check
   */
  pathIsAbsolute(path: string): boolean;

  // === Event/Hook Operations ===
  /**
   * Subscribe to an editor event
   *
   * Handler must be a global function name (not a closure).
   * Multiple handlers can be registered for the same event.
   * Events: "buffer_save", "cursor_moved", "buffer_modified", etc.
   * @param event_name - Event to subscribe to
   * @param handler_name - Name of globalThis function to call with event data
   * @example
   * globalThis.onSave = (data) => {
   * editor.setStatus(`Saved: ${data.path}`);
   * };
   * editor.on("buffer_save", "onSave");
   */
  on(event_name: string, handler_name: string): boolean;
  /**
   * Unregister an event handler
   * @param event_name - Name of the event
   * @param handler_name - Name of the handler to remove
   * @returns true if handler was found and removed
   */
  off(event_name: string, handler_name: string): boolean;
  /**
   * Get list of registered handlers for an event
   * @param event_name - Name of the event
   * @returns Array of handler function names
   */
  getHandlers(event_name: string): string[];

  // === Virtual Buffer Operations ===
  /**
   * Create a virtual buffer in a new horizontal split below current pane
   *
   * Use for results panels, diagnostics, logs, etc. The panel_id enables
   * idempotent updates: if a panel with that ID exists, its content is replaced
   * instead of creating a new split. Define the mode with defineMode first.
   * @param options - Buffer configuration
   * @example
   * // First define the mode with keybindings
   * editor.defineMode("search-results", "special", [
   * ["Return", "search_goto"],
   * ["q", "close_buffer"]
   * ], true);
   *
   * // Then create the buffer
   * const id = await editor.createVirtualBufferInSplit({
   * name: "*Search*",
   * mode: "search-results",
   * read_only: true,
   * entries: [
   * { text: "src/main.rs:42: match\n", properties: { file: "src/main.rs", line: 42 } }
   * ],
   * ratio: 0.3,
   * panel_id: "search"
   * });
   */
  createVirtualBufferInSplit(options: CreateVirtualBufferOptions): Promise<CreateVirtualBufferResult>;
  /**
   * Create a virtual buffer in an existing split
   * @param options - Configuration for the virtual buffer
   * @returns Promise resolving to the buffer ID of the created virtual buffer
   */
  createVirtualBufferInExistingSplit(options: CreateVirtualBufferInExistingSplitOptions): Promise<number>;
  /**
   * Create a virtual buffer in the current split as a new tab
   * This is useful for help panels, documentation, etc. that should open
   * alongside other buffers rather than in a separate split.
   * @param options - Configuration for the virtual buffer
   * @returns Promise resolving to the buffer ID of the created virtual buffer
   */
  createVirtualBuffer(options: CreateVirtualBufferInCurrentSplitOptions): Promise<number>;
  /**
   * Define a buffer mode with keybindings
   * @param name - Mode name (e.g., "diagnostics-list")
   * @param parent - Parent mode name for inheritance (e.g., "special"), or null
   * @param bindings - Array of [key_string, command_name] pairs
   * @param read_only - Whether buffers in this mode are read-only
   * @returns true if mode was defined successfully
   * @example
   * editor.defineMode("diagnostics-list", "special", [
   * ["Return", "diagnostics_goto"],
   * ["q", "close_buffer"]
   * ], true);
   */
  defineMode(name: string, parent?: string | null, bindings: Vec<(String, String): boolean;
  /**
   * Switch the current split to display a buffer
   * @param buffer_id - ID of the buffer to show
   * @returns true if buffer was shown successfully
   */
  showBuffer(buffer_id: number): boolean;
  /**
   * Close a buffer and remove it from all splits
   * @param buffer_id - ID of the buffer to close
   * @returns true if buffer was closed successfully
   */
  closeBuffer(buffer_id: number): boolean;
  /**
   * Focus a specific split
   * @param split_id - ID of the split to focus
   * @returns true if split was focused successfully
   */
  focusSplit(split_id: number): boolean;
  /**
   * Set the buffer displayed in a specific split
   * @param split_id - ID of the split
   * @param buffer_id - ID of the buffer to display in the split
   * @returns true if the buffer was set successfully
   */
  setSplitBuffer(split_id: number, buffer_id: number): boolean;
  /**
   * Close a split (if not the last one)
   * @param split_id - ID of the split to close
   * @returns true if the split was closed successfully
   */
  closeSplit(split_id: number): boolean;
  /**
   * Get text properties at the cursor position in a buffer
   * @param buffer_id - ID of the buffer to query
   * @returns Array of property objects for text ranges containing the cursor
   * @example
   * const props = editor.getTextPropertiesAtCursor(bufferId);
   * if (props.length > 0 && props[0].location) {
   * editor.openFile(props[0].location.file, props[0].location.line, 0);
   * }
   */
  getTextPropertiesAtCursor(buffer_id: number): Record<string, unknown>[];
  /**
   * Set the content of a virtual buffer with text properties
   * @param buffer_id - ID of the virtual buffer
   * @param entries - Array of text entries with properties
   * @returns true if content was set successfully
   */
  setVirtualBufferContent(buffer_id: number, entries: TextPropertyEntry[]): boolean;

}

// Export for module compatibility
export {};
