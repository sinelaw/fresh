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

  // === Buffer Mutations ===
  insertText(buffer_id: number, position: number, text: string): boolean;
  deleteRange(buffer_id: number, start: number, end: number): boolean;
  insertAtCursor(text: string): boolean;

  // === Overlay Operations ===
  addOverlay(buffer_id: number, overlay_id: string, start: number, end: number, r: number, g: number, b: number, underline: boolean): boolean;
  removeOverlay(buffer_id: number, overlay_id: string): boolean;
  removeOverlaysByPrefix(buffer_id: number, prefix: string): boolean;
  clearAllOverlays(buffer_id: number): boolean;
}

// Export for module compatibility
export {};
