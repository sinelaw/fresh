/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Buffer Modified Plugin
 *
 * Shows indicators in the gutter for lines that have been modified since the last save.
 * This tracks in-memory changes, not git changes.
 *
 * This plugin uses a simpler approach: it marks lines as modified when edits happen
 * (after_insert/after_delete hooks), and clears all modified markers on save.
 * It doesn't compare content - it just tracks which lines have been touched since save.
 *
 * Indicator symbols:
 * - │ (blue): Line has been modified since last save
 */

// =============================================================================
// Constants
// =============================================================================

const NAMESPACE = "buffer-modified";
const PRIORITY = 5; // Lower than git-gutter (10) and diagnostics

// Colors (RGB) - Blue to distinguish from git gutter (green/yellow/red)
const COLOR = [100, 149, 237] as [number, number, number]; // Cornflower blue

// Symbol
const SYMBOL = "│";

// =============================================================================
// Types
// =============================================================================

interface BufferState {
  /** Whether we're tracking this buffer */
  tracking: boolean;
}

// =============================================================================
// State
// =============================================================================

/** State per buffer */
const bufferStates: Map<number, BufferState> = new Map();

// =============================================================================
// Line Tracking
// =============================================================================

/**
 * Initialize state for a buffer (on file open)
 * Starts with no modified lines since file was just loaded
 */
function initBufferState(bufferId: number): void {
  bufferStates.set(bufferId, {
    tracking: true,
  });
  // Clear any leftover indicators
  editor.clearLineIndicators(bufferId, NAMESPACE);
}

/**
 * Clear modified state for a buffer (on save)
 * Removes all modified markers since buffer now matches disk
 */
function clearModifiedState(bufferId: number): void {
  editor.clearLineIndicators(bufferId, NAMESPACE);
}

/**
 * Mark a range of lines as modified and set indicators
 *
 * Note: The indicator markers automatically track their byte positions,
 * so we don't need to manually track which lines are modified - we just
 * set indicators and they'll stay on the correct lines as edits happen.
 */
function markLinesModified(bufferId: number, startLine: number, endLine: number): void {
  const state = bufferStates.get(bufferId);
  if (!state || !state.tracking) return;

  // Add indicator for each affected line
  // Note: If an indicator already exists at this position, it will be updated
  for (let line = startLine; line <= endLine; line++) {
    editor.setLineIndicator(
      bufferId,
      line,
      NAMESPACE,
      SYMBOL,
      COLOR[0],
      COLOR[1],
      COLOR[2],
      PRIORITY
    );
  }
}

function reapplyIndicatorsFromDiff(bufferId: number): void {
  const diff = editor.getBufferSavedDiff(bufferId);
  if (!diff) return;

  // If buffer matches saved snapshot, clear everything.
  if (diff.equal) {
    editor.clearLineIndicators(bufferId, NAMESPACE);
    return;
  }

  const ranges = diff.line_ranges;
  // If line info is unavailable, leave existing indicators (best effort).
  if (!ranges) return;

  // Reset namespace to drop stale indicators outside the changed ranges.
  editor.clearLineIndicators(bufferId, NAMESPACE);
  for (const [start, end] of ranges) {
    for (let line = start; line < end; line++) {
      editor.setLineIndicator(
        bufferId,
        line,
        NAMESPACE,
        SYMBOL,
        COLOR[0],
        COLOR[1],
        COLOR[2],
        PRIORITY
      );
    }
  }
}

// =============================================================================
// Event Handlers
// =============================================================================

/**
 * Handle after file open - initialize state
 */
globalThis.onBufferModifiedAfterFileOpen = function (args: {
  buffer_id: number;
  path: string;
}): boolean {
  const bufferId = args.buffer_id;

  if (!args.path || args.path === "") {
    return true;
  }

  // Initialize tracking - file just loaded, no modifications yet
  initBufferState(bufferId);
  editor.debug(editor.t("status.initialized", { path: args.path }));

  return true;
};

/**
 * Handle buffer activation - ensure we're tracking
 */
globalThis.onBufferModifiedBufferActivated = function (args: {
  buffer_id: number;
}): boolean {
  const bufferId = args.buffer_id;

  // If we don't have state yet, initialize it
  if (!bufferStates.has(bufferId)) {
    const filePath = editor.getBufferPath(bufferId);
    if (filePath && filePath !== "") {
      initBufferState(bufferId);
    }
  }

  return true;
};

/**
 * Handle after file save - clear modified state
 */
globalThis.onBufferModifiedAfterSave = function (args: {
  buffer_id: number;
  path: string;
}): boolean {
  const bufferId = args.buffer_id;

  // Clear all modified markers - buffer now matches disk
  clearModifiedState(bufferId);
  editor.debug(editor.t("status.cleared_on_save"));

  return true;
};

/**
 * Handle after insert - mark affected lines as modified
 *
 * Note: Line indicators automatically track position changes via byte-position markers.
 * We only need to add new indicators for the modified lines; existing indicators
 * will automatically shift to stay on the correct lines.
 */
globalThis.onBufferModifiedAfterInsert = function (args: {
  buffer_id: number;
  position: number;
  text: string;
  affected_start: number;
  affected_end: number;
  start_line: number;
  end_line: number;
  lines_added: number;
}): boolean {
  const bufferId = args.buffer_id;

  if (!bufferStates.has(bufferId)) {
    return true;
  }

  // Mark all affected lines (from start_line to end_line inclusive)
  // The indicator markers will automatically track their positions
  markLinesModified(bufferId, args.start_line, args.end_line);
  reapplyIndicatorsFromDiff(bufferId);

  return true;
};

/**
 * Handle after delete - mark affected line as modified
 *
 * Note: Line indicators automatically track position changes via byte-position markers.
 * Markers within deleted ranges are automatically removed. We only need to mark the
 * line where the deletion occurred.
 */
globalThis.onBufferModifiedAfterDelete = function (args: {
  buffer_id: number;
  range: { start: number; end: number };
  deleted_text: string;
  affected_start: number;
  deleted_len: number;
  start_line: number;
  end_line: number;
  lines_removed: number;
}): boolean {
  const bufferId = args.buffer_id;

  if (!bufferStates.has(bufferId)) {
    return true;
  }

  // Mark the line where deletion occurred
  // Markers for deleted lines are automatically cleaned up
  markLinesModified(bufferId, args.start_line, args.start_line);
  reapplyIndicatorsFromDiff(bufferId);

  return true;
};

/**
 * Handle buffer closed - cleanup state
 */
globalThis.onBufferModifiedBufferClosed = function (args: {
  buffer_id: number;
}): boolean {
  bufferStates.delete(args.buffer_id);
  return true;
};

// =============================================================================
// Registration
// =============================================================================

// Register event handlers
editor.on("after_file_open", "onBufferModifiedAfterFileOpen");
editor.on("buffer_activated", "onBufferModifiedBufferActivated");
editor.on("after_file_save", "onBufferModifiedAfterSave");
editor.on("after_insert", "onBufferModifiedAfterInsert");
editor.on("after_delete", "onBufferModifiedAfterDelete");
editor.on("buffer_closed", "onBufferModifiedBufferClosed");

// Initialize for the current buffer
const initBufferId = editor.getActiveBufferId();
const initPath = editor.getBufferPath(initBufferId);
if (initPath && initPath !== "") {
  initBufferState(initBufferId);
}

editor.debug(editor.t("status.loaded"));
