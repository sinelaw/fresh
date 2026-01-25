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

// Helper: Get actual line number using the API
function getCurrentLineCol(): { line: number; column: number } {
  // Use the actual getCursorLine API for accurate line number
  const lineNumber = editor.getCursorLine();

  // Get cursor position within the line by reading buffer content
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  const bufferInfo = editor.getBufferInfo(bufferId);

  // Calculate column by finding start of current line
  let column = 1;
  if (bufferInfo && cursorPos > 0) {
    // Read a small chunk before cursor to find line start
    const readStart = Math.max(0, cursorPos - 1000);
    const textBefore = editor.getBufferText(bufferId, readStart, cursorPos);
    const lastNewline = textBefore.lastIndexOf("\n");
    if (lastNewline !== -1) {
      column = cursorPos - (readStart + lastNewline);
    } else {
      // No newline found, column is position from readStart
      column = cursorPos - readStart + 1;
    }
  }

  return { line: lineNumber, column };
}

// Action: Add bookmark at current position
globalThis.bookmark_add = function (): void {
  const { path, position, splitId } = getCurrentLocation();
  const { line, column } = getCurrentLineCol();

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

  // Add visual indicator with bookmark namespace
  const bufferId = editor.getActiveBufferId();
  editor.addOverlay(bufferId, "bookmark", position, position + 1, {
    fg: [0, 128, 255],  // Teal color
    underline: true,
  });

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

  // Remove all bookmark overlays using namespace
  editor.clearNamespace(bufferId, "bookmark");

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

// Interactive bookmark selection using prompt API
let bookmarkSuggestionIds: number[] = [];

globalThis.bookmark_select = function (): void {
  if (bookmarks.size === 0) {
    editor.setStatus("No bookmarks to select");
    return;
  }

  // Create suggestions from bookmarks
  const suggestions: PromptSuggestion[] = [];
  bookmarkSuggestionIds = [];

  bookmarks.forEach((bm) => {
    const filename = bm.path.split("/").pop() || bm.path;
    suggestions.push({
      text: `${bm.name}: ${bm.path}:${bm.line}:${bm.column}`,
      description: `${filename} at line ${bm.line}`,
      value: String(bm.id),
      disabled: false,
    });
    bookmarkSuggestionIds.push(bm.id);
  });

  editor.startPrompt("Select bookmark: ", "bookmark-select");
  editor.setPromptSuggestions(suggestions);
  editor.setStatus(`${bookmarks.size} bookmark(s) available`);
};

// Handle bookmark selection confirmation
globalThis.onBookmarkSelectConfirmed = function (args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "bookmark-select") {
    return true;
  }

  if (args.selected_index !== null && bookmarkSuggestionIds[args.selected_index] !== undefined) {
    const bookmarkId = bookmarkSuggestionIds[args.selected_index];
    const bookmark = bookmarks.get(bookmarkId);

    if (bookmark) {
      editor.openFile(bookmark.path, bookmark.line, bookmark.column);
      editor.setStatus(`Jumped to ${bookmark.name}: ${bookmark.path}:${bookmark.line}`);
    }
  } else {
    editor.setStatus("No bookmark selected");
  }

  return true;
};

// Handle bookmark selection cancellation
globalThis.onBookmarkSelectCancelled = function (args: { prompt_type: string }): boolean {
  if (args.prompt_type !== "bookmark-select") {
    return true;
  }

  editor.setStatus("Bookmark selection cancelled");
  return true;
};

// Register bookmark event handlers
editor.on("prompt_confirmed", "onBookmarkSelectConfirmed");
editor.on("prompt_cancelled", "onBookmarkSelectCancelled");

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

editor.registerCommand(
  "Select Bookmark",
  "Interactively select and jump to a bookmark",
  "bookmark_select",
  "normal"
);

// Plugin initialized
editor.setStatus("Bookmarks plugin loaded - 7 commands registered");
editor.debug("Bookmarks plugin initialized with command registration and prompt API support");
