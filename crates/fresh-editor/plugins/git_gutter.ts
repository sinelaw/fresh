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
 */

// =============================================================================
// Constants
// =============================================================================

const NAMESPACE = "git-gutter";
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
}

// =============================================================================
// State
// =============================================================================

/** Git state per buffer */
const bufferStates: Map<number, BufferGitState> = new Map();


// =============================================================================
// Git Diff Parsing
// =============================================================================

/**
 * Parse unified diff output to extract hunks
 * Unified diff format:
 * @@ -start,count +start,count @@
 */
function parseDiffOutput(diffOutput: string): DiffHunk[] {
  const hunks: DiffHunk[] = [];
  const lines = diffOutput.split("\n");

  let currentOldLine = 0;
  let currentNewLine = 0;
  let inHunk = false;
  let addedStart = 0;
  let addedCount = 0;
  let modifiedStart = 0;
  let modifiedCount = 0;
  let deletedAtLine = 0;
  let deletedCount = 0;

  const flushAdded = () => {
    if (addedCount > 0) {
      hunks.push({ type: "added", startLine: addedStart, lineCount: addedCount });
      addedCount = 0;
    }
  };

  const flushModified = () => {
    if (modifiedCount > 0) {
      hunks.push({ type: "modified", startLine: modifiedStart, lineCount: modifiedCount });
      modifiedCount = 0;
    }
  };

  const flushDeleted = () => {
    if (deletedCount > 0) {
      // Deleted lines are shown as a marker on the line after the deletion
      hunks.push({ type: "deleted", startLine: deletedAtLine, lineCount: deletedCount });
      deletedCount = 0;
    }
  };

  for (const line of lines) {
    // Match hunk header: @@ -old_start,old_count +new_start,new_count @@
    const hunkMatch = line.match(/^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@/);
    if (hunkMatch) {
      // Flush any pending changes from previous hunk
      flushAdded();
      flushModified();
      flushDeleted();

      currentOldLine = parseInt(hunkMatch[1], 10);
      currentNewLine = parseInt(hunkMatch[3], 10);
      inHunk = true;
      continue;
    }

    if (!inHunk) continue;

    if (line.startsWith("+") && !line.startsWith("+++")) {
      // Added line
      if (deletedCount > 0) {
        // If there were deletions right before, this is a modification
        if (modifiedCount === 0) {
          modifiedStart = currentNewLine;
        }
        modifiedCount++;
        deletedCount--;
      } else {
        // Pure addition
        if (addedCount === 0) {
          addedStart = currentNewLine;
        }
        addedCount++;
      }
      currentNewLine++;
    } else if (line.startsWith("-") && !line.startsWith("---")) {
      // Deleted line - flush any pending additions first
      flushAdded();

      if (deletedCount === 0) {
        deletedAtLine = currentNewLine;
      }
      deletedCount++;
      currentOldLine++;
    } else if (line.startsWith(" ")) {
      // Context line (unchanged)
      flushAdded();
      flushModified();
      flushDeleted();
      currentOldLine++;
      currentNewLine++;
    } else if (line === "\\ No newline at end of file") {
      // Ignore this marker
      continue;
    }
  }

  // Flush any remaining changes
  flushAdded();
  flushModified();
  flushDeleted();

  return hunks;
}

// =============================================================================
// Git Operations
// =============================================================================

/**
 * Get the directory containing a file
 */
function getFileDirectory(filePath: string): string {
  const lastSlash = filePath.lastIndexOf("/");
  if (lastSlash > 0) {
    return filePath.substring(0, lastSlash);
  }
  return ".";
}

/**
 * Check if a file is tracked by git
 */
async function isGitTracked(filePath: string): Promise<boolean> {
  const cwd = getFileDirectory(filePath);
  const result = await editor.spawnProcess("git", ["ls-files", "--error-unmatch", filePath], cwd);
  return result.exit_code === 0;
}

/**
 * Get git diff for a file
 * Compares working tree against HEAD to show all uncommitted changes
 * (both staged and unstaged)
 */
async function getGitDiff(filePath: string): Promise<string> {
  const cwd = getFileDirectory(filePath);

  // Diff against HEAD to show all changes (staged + unstaged) vs last commit
  const result = await editor.spawnProcess("git", [
    "diff",
    "HEAD",
    "--no-color",
    "--unified=0", // No context lines for cleaner parsing
    "--",
    filePath,
  ], cwd);

  // Exit code 0 = no differences, 1 = differences found, >1 = error
  if (result.exit_code <= 1) {
    return result.stdout;
  }

  return "";
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

    // Check if file is git tracked
    const tracked = await isGitTracked(state.filePath);
    if (!tracked) {
      // Clear indicators for non-tracked files
      editor.debug("Git Gutter: file not tracked by git");
      editor.clearLineIndicators(bufferId, NAMESPACE);
      state.hunks = [];
      return;
    }

    editor.debug("Git Gutter: file is tracked, getting diff...");

    // Get diff
    const diffOutput = await getGitDiff(state.filePath);
    editor.debug(`Git Gutter: diff output length = ${diffOutput.length}`);
    if (diffOutput.length > 0 && diffOutput.length < 500) {
      editor.debug(`Git Gutter: diff = ${diffOutput.replace(/\n/g, "\\n")}`);
    }
    const hunks = parseDiffOutput(diffOutput);
    editor.debug(`Git Gutter: parsed ${hunks.length} hunks`);

    // Clear existing indicators
    editor.clearLineIndicators(bufferId, NAMESPACE);

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
      } else {
        // Added/modified indicators show on each affected line
        for (let i = 0; i < hunk.lineCount; i++) {
          // Line numbers are 1-indexed in diff, but 0-indexed in editor
          const line = hunk.startLine - 1 + i;
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
      }
    }

    state.hunks = hunks;
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
globalThis.onGitGutterAfterFileOpen = function (args: {
  buffer_id: number;
  path: string;
}): boolean {
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
  });

  // Update immediately (no debounce for file open)
  updateGitGutter(bufferId);

  return true;
};

/**
 * Handle buffer activation - update if we have state but indicators might be stale
 */
globalThis.onGitGutterBufferActivated = function (args: {
  buffer_id: number;
}): boolean {
  const bufferId = args.buffer_id;

  // If we don't have state yet, try to initialize from buffer path
  if (!bufferStates.has(bufferId)) {
    const filePath = editor.getBufferPath(bufferId);
    if (filePath && filePath !== "") {
      bufferStates.set(bufferId, {
        filePath,
        hunks: [],
        updating: false,
      });
      updateGitGutter(bufferId);
    }
  }
  // If we already have state, the indicators should be current
  // (they update on file open and save)

  return true;
};

/**
 * Handle after file save - refresh indicators
 */
globalThis.onGitGutterAfterSave = function (args: {
  buffer_id: number;
  path: string;
}): boolean {
  const bufferId = args.buffer_id;

  // Update state with new path (in case of save-as)
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

  // Update immediately after save (no debounce)
  updateGitGutter(bufferId);

  return true;
};

// Note: Git diff compares the file on disk, not the in-memory buffer.
// Line indicators automatically track position changes via byte-position markers.
// A full re-diff happens on save. Unsaved changes are shown natively by the editor.

/**
 * Handle buffer closed - cleanup state
 */
globalThis.onGitGutterBufferClosed = function (args: {
  buffer_id: number;
}): boolean {
  bufferStates.delete(args.buffer_id);
  return true;
};

// =============================================================================
// Commands
// =============================================================================

/**
 * Manually refresh git gutter for the current buffer
 */
globalThis.git_gutter_refresh = function (): void {
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
    });
  }

  // Force immediate update
  updateGitGutter(bufferId).then(() => {
    const state = bufferStates.get(bufferId);
    const count = state?.hunks.length || 0;
    editor.setStatus(editor.t("status.changes", { count: String(count) }));
  });
};

// =============================================================================
// Registration
// =============================================================================

// Register event handlers
// Note: No need to register after-insert/after-delete hooks - indicators
// automatically track position changes via byte-position markers in the editor.
editor.on("after_file_open", "onGitGutterAfterFileOpen");
editor.on("buffer_activated", "onGitGutterBufferActivated");
editor.on("after_file_save", "onGitGutterAfterSave");
editor.on("buffer_closed", "onGitGutterBufferClosed");

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
  });
  updateGitGutter(initBufferId);
}

editor.debug("Git Gutter plugin loaded");
