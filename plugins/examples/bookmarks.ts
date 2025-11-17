/// <reference path="../../types/fresh.d.ts" />

/**
 * Bookmarks Plugin for Fresh Editor (TypeScript)
 *
 * Demonstrates the new TypeScript plugin ops:
 * - editor.registerCommand() - Register plugin commands
 * - editor.openFile() - Open file at specific location
 * - editor.getActiveSplitId() - Get current split ID
 * - editor.openFileInSplit() - Open file in specific split
 *
 * Features:
 * - Add bookmarks at current cursor position
 * - List all bookmarks
 * - Jump to bookmarks
 * - Remove bookmarks
 * - Split-aware navigation
 */

// Bookmark storage
interface Bookmark {
  id: number;
  name: string;
  path: string;
  line: number;
  column: number;
  splitId: number;
}

const bookmarks: Map<number, Bookmark> = new Map();
let nextBookmarkId = 1;

// Helper: Get current location info
function getCurrentLocation(): {
  path: string;
  position: number;
  splitId: number;
} {
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  const position = editor.getCursorPosition();
  const splitId = editor.getActiveSplitId();

  return { path, position, splitId };
}

// Helper: Convert byte position to line/column (simplified)
// In a real implementation, this would use buffer content
function positionToLineCol(position: number): { line: number; column: number } {
  // Simplified: assume ~80 chars per line
  const line = Math.floor(position / 80) + 1;
  const column = (position % 80) + 1;
  return { line, column };
}

// Action: Add bookmark at current position
globalThis.bookmark_add = function (): void {
  const { path, position, splitId } = getCurrentLocation();
  const { line, column } = positionToLineCol(position);

  if (!path) {
    editor.setStatus("Cannot bookmark: buffer has no file path");
    return;
  }

  const id = nextBookmarkId++;
  const name = `Bookmark ${id}`;

  const bookmark: Bookmark = {
    id,
    name,
    path,
    line,
    column,
    splitId,
  };

  bookmarks.set(id, bookmark);

  // Add visual indicator
  const bufferId = editor.getActiveBufferId();
  editor.addOverlay(
    bufferId,
    `bookmark-${id}`,
    position,
    position + 1,
    0, // Red
    128, // Green (teal color)
    255, // Blue
    true // Underline
  );

  editor.setStatus(`Added ${name} at ${path}:${line}:${column}`);
  editor.debug(`Bookmark ${id} created: ${JSON.stringify(bookmark)}`);
};

// Action: List all bookmarks
globalThis.bookmark_list = function (): void {
  if (bookmarks.size === 0) {
    editor.setStatus("No bookmarks");
    return;
  }

  const list: string[] = [];
  bookmarks.forEach((bm, id) => {
    list.push(`[${id}] ${bm.path}:${bm.line}:${bm.column}`);
  });

  editor.setStatus(`Bookmarks: ${list.join(" | ")}`);
  editor.debug(`All bookmarks: ${JSON.stringify([...bookmarks.values()])}`);
};

// Action: Jump to bookmark by ID
globalThis.bookmark_goto = function (): void {
  if (bookmarks.size === 0) {
    editor.setStatus("No bookmarks to jump to");
    return;
  }

  // Jump to the first bookmark (simplified)
  const firstBookmark = bookmarks.values().next().value;
  if (firstBookmark) {
    const success = editor.openFile(
      firstBookmark.path,
      firstBookmark.line,
      firstBookmark.column
    );

    if (success) {
      editor.setStatus(
        `Jumped to ${firstBookmark.name}: ${firstBookmark.path}:${firstBookmark.line}`
      );
    } else {
      editor.setStatus(`Failed to open ${firstBookmark.path}`);
    }
  }
};

// Action: Jump to bookmark in same split (split-aware)
globalThis.bookmark_goto_split = function (): void {
  if (bookmarks.size === 0) {
    editor.setStatus("No bookmarks");
    return;
  }

  const currentSplit = editor.getActiveSplitId();
  const firstBookmark = bookmarks.values().next().value;

  if (firstBookmark) {
    // Open in the current split, not the bookmark's original split
    const success = editor.openFileInSplit(
      currentSplit,
      firstBookmark.path,
      firstBookmark.line,
      firstBookmark.column
    );

    if (success) {
      editor.setStatus(
        `Opened ${firstBookmark.name} in split ${currentSplit}`
      );
    } else {
      editor.setStatus(`Failed to open in split ${currentSplit}`);
    }
  }
};

// Action: Remove all bookmarks
globalThis.bookmark_clear = function (): void {
  const bufferId = editor.getActiveBufferId();

  // Remove all bookmark overlays
  editor.removeOverlaysByPrefix(bufferId, "bookmark-");

  const count = bookmarks.size;
  bookmarks.clear();

  editor.setStatus(`Cleared ${count} bookmark(s)`);
};

// Action: Show current split info
globalThis.show_split_info = function (): void {
  const splitId = editor.getActiveSplitId();
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);

  editor.setStatus(`Split ${splitId} | Buffer ${bufferId} | ${path || "[untitled]"}`);
};

// Register commands on plugin load
editor.registerCommand(
  "Add Bookmark",
  "Add a bookmark at the current cursor position",
  "bookmark_add",
  "normal"
);

editor.registerCommand(
  "List Bookmarks",
  "Show all bookmarks",
  "bookmark_list",
  "normal"
);

editor.registerCommand(
  "Go to Bookmark",
  "Jump to the first bookmark",
  "bookmark_goto",
  "normal"
);

editor.registerCommand(
  "Go to Bookmark (Current Split)",
  "Jump to bookmark in current split",
  "bookmark_goto_split",
  "normal"
);

editor.registerCommand(
  "Clear Bookmarks",
  "Remove all bookmarks",
  "bookmark_clear",
  "normal"
);

editor.registerCommand(
  "Show Split Info",
  "Display current split and buffer information",
  "show_split_info",
  "" // Available in all contexts
);

// Plugin initialized
editor.setStatus("Bookmarks plugin loaded - 6 commands registered");
editor.debug("Bookmarks plugin initialized with command registration");
