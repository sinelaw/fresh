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
 *
 * Performance: For large files, indicators are viewport-filtered via the batch API.
 * The after_insert/after_delete hooks refresh diff-based indicators (viewport-filtered,
 * using the last-rendered viewport snapshot) then add the immediate edit range.
 * The viewport_changed hook corrects indicators with fresh viewport bounds from
 * the render loop, handling scroll/jump cases where the snapshot was stale.
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
 * Mark a range of lines as modified and set indicators.
 * Called from after_insert/after_delete with the immediate edit range (always small).
 * No viewport filtering — the edit range is at most a few lines.
 */
function markLinesModified(bufferId: number, startLine: number, endLine: number): void {
  const state = bufferStates.get(bufferId);
  if (!state || !state.tracking) return;

  // The edit range is always small (the lines just inserted/deleted),
  // so we can safely set indicators without viewport filtering.
  const lines: number[] = [];
  for (let line = startLine; line <= endLine; line++) {
    lines.push(line);
  }
  if (lines.length > 0) {
    editor.setLineIndicators(bufferId, lines, NAMESPACE, SYMBOL, COLOR[0], COLOR[1], COLOR[2], PRIORITY);
  }
}

/**
 * Reapply indicators from the saved diff, filtered to a viewport range.
 * Called from viewport_changed with fresh viewport bounds from the render loop.
 *
 * @param bufferId - The buffer to refresh indicators for
 * @param vpStart - First visible line number (from hook args)
 * @param vpHeight - Viewport height in rows (from hook args)
 */
function reapplyIndicatorsFromDiff(bufferId: number, vpStart: number, vpHeight: number, caller: string): boolean {
  const diff = editor.getBufferSavedDiff(bufferId);
  if (!diff) {
    editor.debug(`[buf-mod] reapply(${caller}) buf=${bufferId} vp=${vpStart}..${vpStart + vpHeight}: no diff`);
    return false;
  }

  if (diff.equal) {
    editor.debug(`[buf-mod] reapply(${caller}) buf=${bufferId} vp=${vpStart}..${vpStart + vpHeight}: diff.equal=true, clearing`);
    editor.clearLineIndicators(bufferId, NAMESPACE);
    return true;
  }

  const ranges = diff.line_ranges;
  if (!ranges) {
    editor.debug(`[buf-mod] reapply(${caller}) buf=${bufferId} vp=${vpStart}..${vpStart + vpHeight}: line_ranges=null (no line info)`);
    return;
  }

  editor.debug(`[buf-mod] reapply(${caller}) buf=${bufferId} vp=${vpStart}..${vpStart + vpHeight}: ${ranges.length} diff ranges: ${JSON.stringify(ranges)}`);

  editor.clearLineIndicators(bufferId, NAMESPACE);

  const vpEnd = vpStart + vpHeight;
  const lines: number[] = [];
  for (const [start, end] of ranges) {
    const lo = Math.max(start, vpStart);
    const hi = Math.min(end, vpEnd);
    for (let line = lo; line < hi; line++) {
      lines.push(line);
    }
  }

  editor.debug(`[buf-mod] reapply(${caller}): setting ${lines.length} indicators: [${lines.join(",")}]`);
  if (lines.length > 0) {
    editor.setLineIndicators(bufferId, lines, NAMESPACE, SYMBOL, COLOR[0], COLOR[1], COLOR[2], PRIORITY);
  }
  return false;
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
 * Handle after insert - refresh diff indicators then mark affected lines.
 *
 * Calls reapplyIndicatorsFromDiff first (viewport-filtered via the last-rendered
 * snapshot from getViewport()), then markLinesModified for the immediate edit range.
 * Order matters: reapplyIndicatorsFromDiff clears the namespace, so markLinesModified
 * must run after to ensure the current edit line is always indicated.
 *
 * If the viewport snapshot is stale (e.g. after a jump), viewport_changed will
 * correct indicators with fresh bounds on the next render.
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

  const vp = editor.getViewport();
  editor.debug(`[buf-mod] after_insert: buf=${bufferId} lines=${args.start_line}..${args.end_line} vp.topLine=${vp?.topLine} vp.height=${vp?.height}`);

  let diffEqual = false;
  if (vp && vp.topLine != null) {
    diffEqual = reapplyIndicatorsFromDiff(bufferId, vp.topLine, vp.height, "after_insert");
  }

  if (!diffEqual) {
    markLinesModified(bufferId, args.start_line, args.end_line);
  }

  return true;
};

/**
 * Handle after delete - refresh diff indicators then mark deletion line.
 *
 * Same strategy as after_insert: diff refresh first, then immediate edit marker.
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

  const vp = editor.getViewport();
  editor.debug(`[buf-mod] after_delete: buf=${bufferId} line=${args.start_line} vp.topLine=${vp?.topLine} vp.height=${vp?.height}`);

  let diffEqual = false;
  if (vp && vp.topLine != null) {
    diffEqual = reapplyIndicatorsFromDiff(bufferId, vp.topLine, vp.height, "after_delete");
  }

  if (!diffEqual) {
    markLinesModified(bufferId, args.start_line, args.start_line);
  }

  return true;
};

/**
 * Handle viewport changed - reapply indicators for new visible range.
 * The hook args provide fresh viewport bounds directly from the render loop,
 * so we use those instead of the potentially stale editor.getViewport() snapshot.
 */
globalThis.onBufferModifiedViewportChanged = function (args: {
  buffer_id: number;
  top_byte: number;
  top_line: number | null;
  width: number;
  height: number;
}): boolean {
  editor.debug(`[buf-mod] viewport_changed: buf=${args.buffer_id} top_line=${args.top_line} height=${args.height} top_byte=${args.top_byte}`);
  if (args.top_line == null) return true; // no line info yet (large file, pre-scan)
  reapplyIndicatorsFromDiff(args.buffer_id, args.top_line, args.height, "viewport_changed");
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
editor.on("viewport_changed", "onBufferModifiedViewportChanged");
editor.on("buffer_closed", "onBufferModifiedBufferClosed");

// Initialize for the current buffer
const initBufferId = editor.getActiveBufferId();
const initPath = editor.getBufferPath(initBufferId);
if (initPath && initPath !== "") {
  initBufferState(initBufferId);
}

editor.debug(editor.t("status.loaded"));
