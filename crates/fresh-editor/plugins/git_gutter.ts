/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Git Gutter Plugin
 *
 * Shows git diff indicators in the gutter for modified, added, and deleted lines.
 * Uses `git diff` to compare the current buffer content against the index (staged changes)
 * or HEAD if nothing is staged.
 *
 * Indicator symbols:
 * - │ (green): Added line
 * - │ (yellow): Modified line
 * - ▾ (red): Deleted line(s) below
 *
 * The same hunks are marked on the vertical scrollbar in the same colours, so
 * uncommitted changes elsewhere in the file are visible without scrolling to
 * find them.
 */

// =============================================================================
// Constants
// =============================================================================

const NAMESPACE = "git-gutter";
/** Scrollbar markers live in their own namespace — a separate decoration
 *  surface with its own store, so clearing one never disturbs the other. */
const SCROLL_NAMESPACE = "git-gutter-scroll";
const PRIORITY = 10; // Lower than diagnostics

// Colors (RGB)
const COLORS = {
  added: [80, 250, 123] as [number, number, number],    // Green
  modified: [255, 184, 108] as [number, number, number], // Orange/Yellow
  deleted: [255, 85, 85] as [number, number, number],    // Red
};

// Symbols
const SYMBOLS = {
  added: "│",
  modified: "│",
  deleted: "▾",
};

// =============================================================================
// Types
// =============================================================================

interface DiffHunk {
  /** Type of change */
  type: "added" | "modified" | "deleted";
  /** Starting line number in the new file (1-indexed) */
  startLine: number;
  /** Number of lines affected */
  lineCount: number;
}

interface BufferGitState {
  /** File path for this buffer */
  filePath: string;
  /** Last known hunks for this buffer */
  hunks: DiffHunk[];
  /** Whether we're currently updating */
  updating: boolean;
  /** Host-side baseline for the file on disk (the diff's new side). */
  diskBaselineId: number | null;
  /** Host-side baseline for the file at HEAD (the diff's old side). */
  headBaselineId: number | null;
}

// =============================================================================
// State
// =============================================================================

/** Git state per buffer */
const bufferStates: Map<number, BufferGitState> = new Map();


// =============================================================================
// Hunk mapping (host-side diff)
// =============================================================================

/**
 * Map host `LineDiffHunk`s (0-based, old/new line ranges) onto this
 * plugin's gutter hunks, preserving the placement conventions of the
 * old `git diff -U0` parser exactly:
 *
 *   - a replacement pairs up min(oldCount, newCount) lines as
 *     `modified`, then classifies the excess as `added` (after the
 *     modified run) or `deleted`;
 *   - `deleted.startLine` is 1-based and names the row the ▾ glyph
 *     sits on: the line *before* the seam for a pure deletion
 *     (`newStart`, since `newStart` 0-based is the line after it), or
 *     the first modified row for a shrink-replacement (`newStart + 1`).
 */
function hostHunksToGutterHunks(raw: LineDiffHunk[]): DiffHunk[] {
  const hunks: DiffHunk[] = [];
  for (const h of raw) {
    const paired = Math.min(h.oldCount, h.newCount);
    if (paired > 0) {
      hunks.push({ type: "modified", startLine: h.newStart + 1, lineCount: paired });
    }
    if (h.newCount > h.oldCount) {
      hunks.push({
        type: "added",
        startLine: h.newStart + paired + 1,
        lineCount: h.newCount - h.oldCount,
      });
    } else if (h.oldCount > h.newCount) {
      hunks.push({
        type: "deleted",
        startLine: paired > 0 ? h.newStart + 1 : h.newStart,
        lineCount: h.oldCount - h.newCount,
      });
    }
  }
  return hunks;
}

// =============================================================================
// Baseline management
// =============================================================================

/**
 * Ensure disk + HEAD baselines are registered for this buffer. Returns
 * false when the file has no HEAD version (untracked, no repo) — the
 * cases the old `git ls-files` probe reported.
 */
async function ensureBaselines(bufferId: number, state: BufferGitState): Promise<boolean> {
  // HEAD first: registering it is the tracked-file probe (it fails for
  // untracked files and non-repos, like the old `git ls-files` check),
  // and it must run before the disk baseline so an untracked file never
  // pays the disk read — restore of a large untracked file counts on
  // that bounded I/O.
  if (state.headBaselineId === null) {
    try {
      state.headBaselineId = await editor.registerDiffBaseline(bufferId, "gitRef", "HEAD");
    } catch (_e) {
      return false;
    }
  }
  if (state.diskBaselineId === null) {
    try {
      state.diskBaselineId = await editor.registerDiffBaseline(bufferId, "disk", null);
    } catch (_e) {
      return false;
    }
  }
  return true;
}

/** Forget this buffer's baselines (save-as; the host drops them on buffer close). */
function releaseBaselines(state: BufferGitState): void {
  if (state.diskBaselineId !== null) {
    editor.releaseDiffBaseline(state.diskBaselineId);
    state.diskBaselineId = null;
  }
  if (state.headBaselineId !== null) {
    editor.releaseDiffBaseline(state.headBaselineId);
    state.headBaselineId = null;
  }
}

// =============================================================================
// Indicator Management
// =============================================================================

/**
 * Update git gutter indicators for a buffer
 */
async function updateGitGutter(bufferId: number): Promise<void> {
  const state = bufferStates.get(bufferId);
  if (!state || state.updating) return;

  state.updating = true;

  try {
    editor.debug(`Git Gutter: updating for ${state.filePath}`);

    // Register (or re-register) the disk + HEAD baselines. Registration
    // failing means no HEAD version exists — untracked file or no repo.
    if (!(await ensureBaselines(bufferId, state))) {
      editor.debug("Git Gutter: file not tracked by git");
      editor.clearLineIndicators(bufferId, NAMESPACE);
      editor.clearScrollbarMarkers(bufferId, SCROLL_NAMESPACE);
      state.hunks = [];
      // Signal to other plugins that git is not available for this buffer
      editor.setViewState(bufferId, "git_gutter_hunks", null);
      return;
    }

    // This update runs on open, save, and manual refresh — moments when
    // the disk content (and possibly HEAD) just changed, so re-fetch
    // both references before diffing. The diff itself runs host-side;
    // no file content crosses the plugin bridge.
    await editor.refreshDiffBaseline(state.diskBaselineId!);
    await editor.refreshDiffBaseline(state.headBaselineId!);
    const result = await editor.diffBaselinePair(
      state.headBaselineId!,
      state.diskBaselineId!,
    );
    const hunks = hostHunksToGutterHunks(result.hunks);
    editor.debug(`Git Gutter: ${hunks.length} hunks from host diff`);

    // Clear existing indicators
    editor.clearLineIndicators(bufferId, NAMESPACE);

    // Scrollbar marks, collected alongside the gutter glyphs and published in
    // one batched command below. They reuse the gutter palette deliberately, so
    // a hunk's glyph and its mark on the track are the same colour.
    //
    // Positions are 0-based logical lines: this plugin never reads the buffer
    // text — it parses `git diff` output — so line numbers are the only
    // coordinate it can supply honestly, and the editor converts each to a byte
    // anchor at set time against the buffer being decorated.
    //
    // One mark per *hunk*, spanning `line`..`endLine`, not one per changed
    // line. The track paints the same streak either way, but per-line markers
    // cost a byte lookup and two anchors each: on a file with 20 000 changed
    // lines that added ~27 ms to every save, on top of the same again when the
    // projection re-read them.
    const scrollMarkers: ScrollbarMarker[] = [];

    // Apply new indicators
    for (const hunk of hunks) {
      const color = COLORS[hunk.type];
      const symbol = SYMBOLS[hunk.type];

      if (hunk.type === "deleted") {
        // Deleted indicator shows on a single line
        // Line numbers are 1-indexed in diff, but 0-indexed in editor
        const line = Math.max(0, hunk.startLine - 1);
        editor.setLineIndicator(
          bufferId,
          line,
          NAMESPACE,
          symbol,
          color[0],
          color[1],
          color[2],
          PRIORITY
        );
        // The deleted lines are gone from the new side, so the mark sits on the
        // seam where they were — the same line the ▾ glyph points from.
        scrollMarkers.push({ line, color, priority: PRIORITY });
      } else {
        // Added/modified indicators show on each affected line
        for (let i = 0; i < hunk.lineCount; i++) {
          // Line numbers are 1-indexed in diff, but 0-indexed in editor
          const line = Math.max(0, hunk.startLine - 1 + i);
          editor.setLineIndicator(
            bufferId,
            line,
            NAMESPACE,
            symbol,
            color[0],
            color[1],
            color[2],
            PRIORITY
          );
        }
        // `endLine` is inclusive, so a one-line hunk marks one row.
        const first = Math.max(0, hunk.startLine - 1);
        scrollMarkers.push({
          line: first,
          endLine: Math.max(first, first + hunk.lineCount - 1),
          color,
          priority: PRIORITY,
        });
      }
    }

    // Replace the whole namespace in a single command: the diff above covers
    // the entire file, so this set is complete and authoritative, and swapping
    // it atomically means a refresh never shows a half-rebuilt track. An empty
    // set clears the marks, which is what a file with no changes wants.
    editor.setScrollbarMarkers(bufferId, SCROLL_NAMESPACE, scrollMarkers);

    state.hunks = hunks;

    // Export hunks for other plugins (e.g. diff_nav) via shared view state
    editor.setViewState(bufferId, "git_gutter_hunks", hunks);
  } finally {
    state.updating = false;
  }
}


// =============================================================================
// Event Handlers
// =============================================================================

/**
 * Handle after file open - initialize git state and update indicators
 */


/**
 * Handle buffer activation - update if we have state but indicators might be stale
 */


/**
 * Handle after file save - refresh indicators
 */


// Note: Git diff compares the file on disk, not the in-memory buffer.
// Line indicators automatically track position changes via byte-position markers.
// A full re-diff happens on save. Unsaved changes are shown natively by the editor.

/**
 * Handle buffer closed - cleanup state
 */


// =============================================================================
// Commands
// =============================================================================

/**
 * Manually refresh git gutter for the current buffer
 */
function git_gutter_refresh() : void {
  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);

  if (!filePath || filePath === "") {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }

  // Ensure state exists
  if (!bufferStates.has(bufferId)) {
    bufferStates.set(bufferId, {
      filePath,
      hunks: [],
      updating: false,
      diskBaselineId: null,
      headBaselineId: null,
    });
  }

  // Force immediate update
  updateGitGutter(bufferId).then(() => {
    const state = bufferStates.get(bufferId);
    const count = state?.hunks.length || 0;
    editor.setStatus(editor.t("status.changes", { count: String(count) }));
  });
}
registerHandler("git_gutter_refresh", git_gutter_refresh);

// =============================================================================
// Registration
// =============================================================================

// Register event handlers
// Note: No need to register after-insert/after-delete hooks - indicators
// automatically track position changes via byte-position markers in the editor.
editor.on("after_file_open", (args) => {
  const bufferId = args.buffer_id;
  const filePath = args.path;

  if (!filePath || filePath === "") {
    return true;
  }

  // Initialize state for this buffer
  bufferStates.set(bufferId, {
    filePath,
    hunks: [],
    updating: false,
    diskBaselineId: null,
    headBaselineId: null,
  });

  // Update immediately (no debounce for file open)
  updateGitGutter(bufferId);

  return true;
});
editor.on("buffer_activated", (args) => {
  const bufferId = args.buffer_id;

  // If we don't have state yet, try to initialize from buffer path
  if (!bufferStates.has(bufferId)) {
    const filePath = editor.getBufferPath(bufferId);
    if (filePath && filePath !== "") {
      bufferStates.set(bufferId, {
        filePath,
        hunks: [],
        updating: false,
        diskBaselineId: null,
        headBaselineId: null,
      });
      updateGitGutter(bufferId);
    }
  }
  // If we already have state, the indicators should be current
  // (they update on file open and save)

  return true;
});
editor.on("after_file_save", (args) => {
  const bufferId = args.buffer_id;

  // Update state with new path (in case of save-as); a changed path
  // invalidates both baselines, which are bound to the old one.
  const state = bufferStates.get(bufferId);
  if (state) {
    if (state.filePath !== args.path) {
      state.filePath = args.path;
      releaseBaselines(state);
    }
  } else {
    bufferStates.set(bufferId, {
      filePath: args.path,
      hunks: [],
      updating: false,
      diskBaselineId: null,
      headBaselineId: null,
    });
  }

  // Update immediately after save (no debounce)
  updateGitGutter(bufferId);

  return true;
});
editor.on("after_file_revert", (args) => {
  const bufferId = args.buffer_id;

  // A reload replaced the buffer content (auto-revert after an external
  // change like `git checkout <ref> -- <file>`, or an explicit revert),
  // which drops the old indicators along with the old buffer state — but
  // never goes through after_file_save. Re-diff so the gutter and
  // scrollbar reflect the reloaded content.
  const state = bufferStates.get(bufferId);
  if (state) {
    state.filePath = args.path;
  } else {
    bufferStates.set(bufferId, {
      filePath: args.path,
      hunks: [],
      updating: false,
    });
  }

  updateGitGutter(bufferId);

  return true;
});
editor.on("buffer_closed", (args) => {
  bufferStates.delete(args.buffer_id);
  return true;
});

// Register commands
editor.registerCommand(
  "%cmd.refresh",
  "%cmd.refresh_desc",
  "git_gutter_refresh",
  null
);

// Initialize for the current buffer
const initBufferId = editor.getActiveBufferId();
const initPath = editor.getBufferPath(initBufferId);
if (initPath && initPath !== "") {
  bufferStates.set(initBufferId, {
    filePath: initPath,
    hunks: [],
    updating: false,
    diskBaselineId: null,
    headBaselineId: null,
  });
  updateGitGutter(initBufferId);
}

editor.debug("Git Gutter plugin loaded");
