/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * 3-Way Merge Conflict Resolution Plugin
 *
 * Provides an interactive merge conflict resolution interface with:
 * - Automatic detection of git conflict markers when files are opened
 * - Multi-panel UI showing OURS, THEIRS, and editable RESULT
 * - Keyboard navigation between conflicts
 * - One-key resolution (accept ours, theirs, or both)
 * - git-mediate style auto-resolution for trivial conflicts
 * - Visual highlighting with intra-line diffing
 *
 * Architecture: Plugin-based implementation following the spec in docs/MERGE.md
 *
 * TODO: Consider rewriting to use the built-in side-by-side diff in the core
 * editor, or use composite buffers for a simpler implementation.
 */

// =============================================================================
// Types and Interfaces
// =============================================================================

interface ConflictBlock {
  /** Index of this conflict (0-based) */
  index: number;
  /** Byte offset where the conflict starts (<<<<<<< marker) */
  startOffset: number;
  /** Byte offset where the conflict ends (after >>>>>>> marker) */
  endOffset: number;
  /** Content from "ours" side (our branch) */
  ours: string;
  /** Content from "base" (common ancestor) - may be empty if no diff3 */
  base: string;
  /** Content from "theirs" side (incoming changes) */
  theirs: string;
  /** Whether this conflict has been resolved */
  resolved: boolean;
  /** Resolution type if resolved */
  resolution?: "ours" | "theirs" | "both" | "manual";
  /** The resolved content (if resolved) */
  resolvedContent?: string;
}

interface MergeState {
  /** Whether merge mode is active */
  isActive: boolean;
  /** The original buffer ID (file with conflicts) */
  sourceBufferId: number | null;
  /** The original file path */
  sourcePath: string | null;
  /** Original file content (for abort) */
  originalContent: string;
  /** List of detected conflicts */
  conflicts: ConflictBlock[];
  /** Index of currently selected conflict */
  selectedIndex: number;
  /** The OURS panel buffer ID */
  oursPanelId: number | null;
  /** The THEIRS panel buffer ID */
  theirsPanelId: number | null;
  /** The RESULT panel buffer ID (editable) */
  resultPanelId: number | null;
  /** Split IDs for each panel */
  oursSplitId: number | null;
  theirsSplitId: number | null;
  resultSplitId: number | null;
  /** Content for OURS side */
  oursContent: string;
  /** Content for THEIRS side */
  theirsContent: string;
  /** Content for BASE side (common ancestor) */
  baseContent: string;
  /** Current result content */
  resultContent: string;
}

// =============================================================================
// State Management
// =============================================================================

const mergeState: MergeState = {
  isActive: false,
  sourceBufferId: null,
  sourcePath: null,
  originalContent: "",
  conflicts: [],
  selectedIndex: 0,
  oursPanelId: null,
  theirsPanelId: null,
  resultPanelId: null,
  oursSplitId: null,
  theirsSplitId: null,
  resultSplitId: null,
  oursContent: "",
  theirsContent: "",
  baseContent: "",
  resultContent: "",
};

// =============================================================================
// Color Definitions
// =============================================================================

const colors = {
  // Panel headers
  oursHeader: [100, 200, 255] as [number, number, number],    // Cyan for OURS
  theirsHeader: [255, 180, 100] as [number, number, number],  // Orange for THEIRS
  resultHeader: [150, 255, 150] as [number, number, number],  // Green for RESULT

  // Conflict highlighting
  conflictOurs: [50, 80, 100] as [number, number, number],    // Blue-tinted background
  conflictTheirs: [100, 70, 50] as [number, number, number],  // Orange-tinted background
  conflictBase: [70, 70, 70] as [number, number, number],     // Gray for base

  // Intra-line diff colors
  diffAdd: [50, 100, 50] as [number, number, number],         // Green for additions
  diffDel: [100, 50, 50] as [number, number, number],         // Red for deletions
  diffMod: [50, 50, 100] as [number, number, number],         // Blue for modifications

  // Selection
  selected: [80, 80, 120] as [number, number, number],        // Selection highlight

  // Buttons/actions
  button: [100, 149, 237] as [number, number, number],        // Cornflower blue
  resolved: [100, 200, 100] as [number, number, number],      // Green for resolved
  unresolved: [200, 100, 100] as [number, number, number],    // Red for unresolved
};

// =============================================================================
// Mode Definition
// =============================================================================

// Define merge-conflict mode with keybindings
// Inherits from "normal" so cursor movement (hjkl) works
// Uses ] and [ for conflict navigation to avoid overriding j/k
editor.defineMode(
  "merge-conflict",
  "normal", // inherit from normal mode for cursor movement
  [
    // Conflict navigation (use ] and [ to avoid overriding j/k cursor movement)
    ["]", "merge_next_conflict"],
    ["[", "merge_prev_conflict"],
    // Also support n/p for navigation
    ["n", "merge_next_conflict"],
    ["p", "merge_prev_conflict"],

    // Resolution actions
    ["u", "merge_use_ours"],      // Use ours
    ["t", "merge_take_theirs"],   // Take theirs
    ["b", "merge_use_both"],      // Use both

    // Completion
    ["s", "merge_save_and_exit"], // Save & exit
    ["q", "merge_abort"],         // Abort

    // Help
    ["?", "merge_show_help"],
  ],
  true // read-only for navigation panels
);

// Define merge-result mode for the editable RESULT panel
editor.defineMode(
  "merge-result",
  "normal", // inherit from normal mode for editing
  [
    // Navigation - use C-j/C-k to avoid conflicting with C-p (command palette)
    ["C-j", "merge_next_conflict"],
    ["C-k", "merge_prev_conflict"],

    // Resolution shortcuts
    ["C-u", "merge_use_ours"],
    ["C-t", "merge_take_theirs"],
    ["C-b", "merge_use_both"],

    // Completion
    ["C-s", "merge_save_and_exit"],
    ["C-q", "merge_abort"],
  ],
  false // editable
);

// =============================================================================
// Conflict Detection and Parsing
// =============================================================================

/**
 * Check if content contains git conflict markers
 */
function hasConflictMarkers(content: string): boolean {
  return content.includes("<<<<<<<") &&
         content.includes("=======") &&
         content.includes(">>>>>>>");
}

/**
 * Parse conflict markers from file content
 * Supports both 2-way (no base) and 3-way (with base via diff3) conflicts
 */
function parseConflicts(content: string): ConflictBlock[] {
  const conflicts: ConflictBlock[] = [];

  // Regex to match conflict blocks
  // Supports optional base section (||||||| marker)
  // Key: use ^ anchors to ensure markers are at start of lines (multiline mode)
  // Note: use \r?\n to handle both LF and CRLF line endings
  const conflictRegex = /^<<<<<<<[^\r\n]*\r?\n([\s\S]*?)(?:^\|\|\|\|\|\|\|[^\r\n]*\r?\n([\s\S]*?))?^=======\r?\n([\s\S]*?)^>>>>>>>[^\r\n]*$/gm;

  let match;
  let index = 0;

  while ((match = conflictRegex.exec(content)) !== null) {
    const startOffset = match.index;
    const endOffset = match.index + match[0].length;

    conflicts.push({
      index: index++,
      startOffset,
      endOffset,
      ours: match[1] || "",
      base: match[2] || "",
      theirs: match[3] || "",
      resolved: false,
    });
  }

  return conflicts;
}

/**
 * Extract non-conflict sections and build initial result content
 */
function buildInitialResult(content: string, conflicts: ConflictBlock[]): string {
  if (conflicts.length === 0) return content;

  let result = "";
  let lastEnd = 0;

  for (const conflict of conflicts) {
    // Add non-conflict text before this conflict
    result += content.substring(lastEnd, conflict.startOffset);

    // Add a placeholder for the conflict
    result += `<<<CONFLICT_${conflict.index}>>>`;

    lastEnd = conflict.endOffset;
  }

  // Add remaining text after last conflict
  result += content.substring(lastEnd);

  return result;
}

// =============================================================================
// Git Data Fetching
// =============================================================================

/**
 * Fetch the base (common ancestor), ours, and theirs versions from git
 */
async function fetchGitVersions(filePath: string): Promise<{
  base: string;
  ours: string;
  theirs: string;
} | null> {
  try {
    // Get the directory of the file for running git commands
    const fileDir = editor.pathDirname(filePath);

    // Get the git repository root
    const repoRootResult = await editor.spawnProcess("git", [
      "rev-parse", "--show-toplevel"
    ], fileDir);

    if (repoRootResult.exit_code !== 0) {
      editor.debug(`fetchGitVersions: failed to get repo root`);
      return null;
    }

    const repoRoot = repoRootResult.stdout.trim();

    // Compute the relative path from repo root to the file
    // filePath is absolute, repoRoot is absolute
    let relativePath = filePath;
    if (filePath.startsWith(repoRoot + "/")) {
      relativePath = filePath.substring(repoRoot.length + 1);
    } else if (filePath.startsWith(repoRoot)) {
      relativePath = filePath.substring(repoRoot.length);
      if (relativePath.startsWith("/")) {
        relativePath = relativePath.substring(1);
      }
    }

    editor.debug(`fetchGitVersions: repoRoot=${repoRoot}, relativePath=${relativePath}`);

    // Get OURS version (--ours or :2:)
    const oursResult = await editor.spawnProcess("git", [
      "show", `:2:${relativePath}`
    ], fileDir);
    editor.debug(`fetchGitVersions: ours exit_code=${oursResult.exit_code}, stdout length=${oursResult.stdout.length}`);

    // Get THEIRS version (--theirs or :3:)
    const theirsResult = await editor.spawnProcess("git", [
      "show", `:3:${relativePath}`
    ], fileDir);
    editor.debug(`fetchGitVersions: theirs exit_code=${theirsResult.exit_code}, stdout length=${theirsResult.stdout.length}`);

    // Get BASE version (common ancestor, :1:)
    const baseResult = await editor.spawnProcess("git", [
      "show", `:1:${relativePath}`
    ], fileDir);
    editor.debug(`fetchGitVersions: base exit_code=${baseResult.exit_code}, stdout length=${baseResult.stdout.length}`);

    return {
      base: baseResult.exit_code === 0 ? baseResult.stdout : "",
      ours: oursResult.exit_code === 0 ? oursResult.stdout : "",
      theirs: theirsResult.exit_code === 0 ? theirsResult.stdout : "",
    };
  } catch (e) {
    editor.debug(`Failed to fetch git versions: ${e}`);
    return null;
  }
}

// =============================================================================
// Auto-Resolution (git-mediate style)
// =============================================================================

/**
 * Attempt to auto-resolve trivial conflicts using git-mediate logic
 * A conflict is trivially resolvable if only one side changed from base
 */
function autoResolveConflicts(conflicts: ConflictBlock[]): void {
  for (const conflict of conflicts) {
    if (conflict.resolved) continue;

    // If we have base content, check for trivial resolution
    if (conflict.base) {
      const oursChanged = conflict.ours.trim() !== conflict.base.trim();
      const theirsChanged = conflict.theirs.trim() !== conflict.base.trim();

      if (oursChanged && !theirsChanged) {
        // Only ours changed - use ours
        conflict.resolved = true;
        conflict.resolution = "ours";
        conflict.resolvedContent = conflict.ours;
        editor.debug(`Auto-resolved conflict ${conflict.index}: using OURS (theirs unchanged)`);
      } else if (!oursChanged && theirsChanged) {
        // Only theirs changed - use theirs
        conflict.resolved = true;
        conflict.resolution = "theirs";
        conflict.resolvedContent = conflict.theirs;
        editor.debug(`Auto-resolved conflict ${conflict.index}: using THEIRS (ours unchanged)`);
      } else if (!oursChanged && !theirsChanged) {
        // Neither changed (identical) - use either
        conflict.resolved = true;
        conflict.resolution = "ours";
        conflict.resolvedContent = conflict.ours;
        editor.debug(`Auto-resolved conflict ${conflict.index}: both identical to base`);
      }
      // If both changed differently, leave unresolved
    }

    // Check if ours and theirs are identical
    if (!conflict.resolved && conflict.ours.trim() === conflict.theirs.trim()) {
      conflict.resolved = true;
      conflict.resolution = "ours";
      conflict.resolvedContent = conflict.ours;
      editor.debug(`Auto-resolved conflict ${conflict.index}: ours and theirs identical`);
    }
  }
}

// =============================================================================
// Word-Level Diff
// =============================================================================

/**
 * Simple word-level diff for intra-line highlighting
 */
function computeWordDiff(a: string, b: string): Array<{
  type: "same" | "add" | "del" | "mod";
  aStart: number;
  aEnd: number;
  bStart: number;
  bEnd: number;
}> {
  // Split into words (preserving whitespace positions)
  const aWords = a.split(/(\s+)/);
  const bWords = b.split(/(\s+)/);

  const diffs: Array<{
    type: "same" | "add" | "del" | "mod";
    aStart: number;
    aEnd: number;
    bStart: number;
    bEnd: number;
  }> = [];

  let aPos = 0;
  let bPos = 0;
  let aIdx = 0;
  let bIdx = 0;

  // Simple LCS-based diff (for short texts)
  while (aIdx < aWords.length || bIdx < bWords.length) {
    if (aIdx >= aWords.length) {
      // Rest of b is additions
      const bWord = bWords[bIdx];
      diffs.push({
        type: "add",
        aStart: aPos,
        aEnd: aPos,
        bStart: bPos,
        bEnd: bPos + bWord.length,
      });
      bPos += bWord.length;
      bIdx++;
    } else if (bIdx >= bWords.length) {
      // Rest of a is deletions
      const aWord = aWords[aIdx];
      diffs.push({
        type: "del",
        aStart: aPos,
        aEnd: aPos + aWord.length,
        bStart: bPos,
        bEnd: bPos,
      });
      aPos += aWord.length;
      aIdx++;
    } else if (aWords[aIdx] === bWords[bIdx]) {
      // Same
      const word = aWords[aIdx];
      diffs.push({
        type: "same",
        aStart: aPos,
        aEnd: aPos + word.length,
        bStart: bPos,
        bEnd: bPos + word.length,
      });
      aPos += word.length;
      bPos += word.length;
      aIdx++;
      bIdx++;
    } else {
      // Different - mark as modification
      const aWord = aWords[aIdx];
      const bWord = bWords[bIdx];
      diffs.push({
        type: "mod",
        aStart: aPos,
        aEnd: aPos + aWord.length,
        bStart: bPos,
        bEnd: bPos + bWord.length,
      });
      aPos += aWord.length;
      bPos += bWord.length;
      aIdx++;
      bIdx++;
    }
  }

  return diffs;
}

// =============================================================================
// View Rendering - Full File Content (JetBrains-style)
// =============================================================================

/**
 * Build entries showing the full file content for OURS or THEIRS
 * This displays the complete file from git, highlighting conflict regions
 */
function buildFullFileEntries(side: "ours" | "theirs"): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const content = side === "ours" ? mergeState.oursContent : mergeState.theirsContent;

  // If we don't have the git version, fall back to showing conflict regions only
  if (!content) {
    entries.push({
      text: editor.t("panel.git_unavailable") + "\n\n",
      properties: { type: "warning" },
    });

    // Show conflict regions from parsed conflicts
    for (const conflict of mergeState.conflicts) {
      const conflictContent = side === "ours" ? conflict.ours : conflict.theirs;
      const isSelected = conflict.index === mergeState.selectedIndex;

      entries.push({
        text: `--- ${editor.t("panel.conflict", { index: String(conflict.index + 1) })} ---\n`,
        properties: {
          type: "conflict-header",
          conflictIndex: conflict.index,
          selected: isSelected,
        },
      });

      entries.push({
        text: (conflictContent || editor.t("panel.empty")) + "\n",
        properties: {
          type: "conflict-content",
          conflictIndex: conflict.index,
          side: side,
        },
      });
    }
    return entries;
  }

  // Show full file content with conflict regions highlighted
  // The content from git is the clean version without markers
  const lines = content.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // Check if this line is in a conflict region
    const inConflict = isLineInConflict(i, side);

    entries.push({
      text: line + (i < lines.length - 1 ? "\n" : ""),
      properties: {
        type: inConflict ? "conflict-line" : "normal-line",
        lineNumber: i + 1,
        side: side,
        ...(inConflict ? { conflictIndex: getConflictIndexForLine(i, side) } : {}),
      },
    });
  }

  return entries;
}

/**
 * Check if a line number falls within a conflict region
 */
function isLineInConflict(_lineNumber: number, _side: "ours" | "theirs"): boolean {
  // For now, we don't have line mapping from git versions to original file
  // This would require proper diff/alignment between versions
  // TODO: Implement proper line-to-conflict mapping
  return false;
}

/**
 * Get the conflict index for a line number
 */
function getConflictIndexForLine(_lineNumber: number, _side: "ours" | "theirs"): number {
  return 0;
}

/**
 * Build entries showing the merged result content
 * This shows the file with resolved/unresolved conflict regions
 */
function buildResultFileEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Build the result by combining non-conflict regions with resolved conflicts
  const originalContent = mergeState.originalContent;
  if (!originalContent) {
    entries.push({
      text: "(No content available)\n",
      properties: { type: "error" },
    });
    return entries;
  }

  // Parse the original content and replace conflict regions with resolutions
  let result = originalContent;

  // Process conflicts in reverse order to maintain correct positions
  const sortedConflicts = [...mergeState.conflicts].sort((a, b) => b.startOffset - a.startOffset);

  for (const conflict of sortedConflicts) {
    let replacement: string;

    if (conflict.resolved && conflict.resolvedContent !== undefined) {
      replacement = conflict.resolvedContent;
    } else {
      // Show unresolved conflict with markers
      replacement = `<<<<<<< OURS\n${conflict.ours || ""}\n=======\n${conflict.theirs || ""}\n>>>>>>> THEIRS`;
    }

    // Replace the conflict region in the result
    const before = result.substring(0, conflict.startOffset);
    const after = result.substring(conflict.endOffset);
    result = before + replacement + after;
  }

  // Now display the result content
  const lines = result.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const isConflictMarker = line.startsWith("<<<<<<<") || line.startsWith("=======") || line.startsWith(">>>>>>>");

    entries.push({
      text: line + (i < lines.length - 1 ? "\n" : ""),
      properties: {
        type: isConflictMarker ? "conflict-marker" : "result-line",
        lineNumber: i + 1,
      },
    });
  }

  return entries;
}

// =============================================================================
// View Rendering - Summary Style (Legacy)
// =============================================================================

/**
 * Build entries for OURS panel (summary style)
 */
function buildOursEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: "  " + editor.t("panel.ours_header") + "\n",
    properties: { type: "header", panel: "ours" },
  });
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Show each conflict's OURS side
  for (const conflict of mergeState.conflicts) {
    const isSelected = conflict.index === mergeState.selectedIndex;
    const marker = isSelected ? "> " : "  ";
    const status = conflict.resolved ? editor.t("panel.resolved") : editor.t("panel.pending");

    entries.push({
      text: `\n${marker}${editor.t("panel.conflict", { index: String(conflict.index + 1) })} ${status}\n`,
      properties: {
        type: "conflict-header",
        conflictIndex: conflict.index,
        selected: isSelected,
        resolved: conflict.resolved,
      },
    });

    entries.push({
      text: "─────────────────────────────────────────────────────────────────────────────\n",
      properties: { type: "separator" },
    });

    // Content
    const content = conflict.ours || editor.t("panel.empty");
    for (const line of content.split("\n")) {
      entries.push({
        text: `  ${line}\n`,
        properties: {
          type: "conflict-content",
          conflictIndex: conflict.index,
          side: "ours",
        },
      });
    }
  }

  return entries;
}

/**
 * Build entries for THEIRS panel
 */
function buildTheirsEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: "  " + editor.t("panel.theirs_header") + "\n",
    properties: { type: "header", panel: "theirs" },
  });
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Show each conflict's THEIRS side
  for (const conflict of mergeState.conflicts) {
    const isSelected = conflict.index === mergeState.selectedIndex;
    const marker = isSelected ? "> " : "  ";
    const status = conflict.resolved ? editor.t("panel.resolved") : editor.t("panel.pending");

    entries.push({
      text: `\n${marker}${editor.t("panel.conflict", { index: String(conflict.index + 1) })} ${status}\n`,
      properties: {
        type: "conflict-header",
        conflictIndex: conflict.index,
        selected: isSelected,
        resolved: conflict.resolved,
      },
    });

    entries.push({
      text: "─────────────────────────────────────────────────────────────────────────────\n",
      properties: { type: "separator" },
    });

    // Content
    const content = conflict.theirs || editor.t("panel.empty");
    for (const line of content.split("\n")) {
      entries.push({
        text: `  ${line}\n`,
        properties: {
          type: "conflict-content",
          conflictIndex: conflict.index,
          side: "theirs",
        },
      });
    }
  }

  return entries;
}

/**
 * Build entries for RESULT panel
 */
function buildResultEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: "  " + editor.t("panel.result_header") + "\n",
    properties: { type: "header", panel: "result" },
  });
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Build result content
  const unresolvedCount = mergeState.conflicts.filter(c => !c.resolved).length;

  if (unresolvedCount > 0) {
    entries.push({
      text: `\n  ⚠ ${editor.t("panel.remaining", { count: String(unresolvedCount) })}\n\n`,
      properties: { type: "warning" },
    });
  } else {
    entries.push({
      text: "\n  ✓ " + editor.t("panel.all_resolved") + "\n\n",
      properties: { type: "success" },
    });
  }

  // Show resolved content or action buttons for each conflict
  for (const conflict of mergeState.conflicts) {
    const isSelected = conflict.index === mergeState.selectedIndex;
    const marker = isSelected ? "> " : "  ";

    entries.push({
      text: `${marker}${editor.t("panel.conflict", { index: String(conflict.index + 1) })}:\n`,
      properties: {
        type: "conflict-header",
        conflictIndex: conflict.index,
        selected: isSelected,
      },
    });

    if (conflict.resolved && conflict.resolvedContent !== undefined) {
      // Show resolved content
      entries.push({
        text: `  ${editor.t("panel.resolved_with", { resolution: conflict.resolution || "" })}\n`,
        properties: { type: "resolution-info", resolution: conflict.resolution },
      });

      for (const line of conflict.resolvedContent.split("\n")) {
        entries.push({
          text: `  ${line}\n`,
          properties: {
            type: "resolved-content",
            conflictIndex: conflict.index,
          },
        });
      }
    } else {
      // Show clickable action buttons
      // Each button is a separate entry with onClick for mouse support
      entries.push({
        text: "  << ",
        properties: { type: "action-prefix" },
      });
      entries.push({
        text: editor.t("btn.accept_ours"),
        properties: {
          type: "action-button",
          conflictIndex: conflict.index,
          onClick: "merge_use_ours",
        },
      });
      entries.push({
        text: " | ",
        properties: { type: "action-separator" },
      });
      entries.push({
        text: editor.t("btn.accept_theirs"),
        properties: {
          type: "action-button",
          conflictIndex: conflict.index,
          onClick: "merge_take_theirs",
        },
      });
      entries.push({
        text: " | ",
        properties: { type: "action-separator" },
      });
      entries.push({
        text: editor.t("btn.both"),
        properties: {
          type: "action-button",
          conflictIndex: conflict.index,
          onClick: "merge_use_both",
        },
      });
      entries.push({
        text: " >>\n",
        properties: { type: "action-suffix" },
      });
    }

    entries.push({
      text: "─────────────────────────────────────────────────────────────────────────────\n",
      properties: { type: "separator" },
    });
  }

  // Help bar with clickable buttons
  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });
  // Navigation
  entries.push({
    text: editor.t("btn.next"),
    properties: { type: "help-button", onClick: "merge_next_conflict" },
  });
  entries.push({
    text: " ",
    properties: { type: "help-separator" },
  });
  entries.push({
    text: editor.t("btn.prev"),
    properties: { type: "help-button", onClick: "merge_prev_conflict" },
  });
  entries.push({
    text: " | ",
    properties: { type: "help-separator" },
  });
  // Resolution
  entries.push({
    text: editor.t("btn.use_ours"),
    properties: { type: "help-button", onClick: "merge_use_ours" },
  });
  entries.push({
    text: " ",
    properties: { type: "help-separator" },
  });
  entries.push({
    text: editor.t("btn.take_theirs"),
    properties: { type: "help-button", onClick: "merge_take_theirs" },
  });
  entries.push({
    text: " ",
    properties: { type: "help-separator" },
  });
  entries.push({
    text: editor.t("btn.both"),
    properties: { type: "help-button", onClick: "merge_use_both" },
  });
  entries.push({
    text: " | ",
    properties: { type: "help-separator" },
  });
  // Completion
  entries.push({
    text: editor.t("btn.save_exit"),
    properties: { type: "help-button", onClick: "merge_save_and_exit" },
  });
  entries.push({
    text: " ",
    properties: { type: "help-separator" },
  });
  entries.push({
    text: editor.t("btn.abort"),
    properties: { type: "help-button", onClick: "merge_abort" },
  });
  entries.push({
    text: "\n",
    properties: { type: "help-newline" },
  });

  return entries;
}

/**
 * Apply visual highlighting to panels
 */
function applyHighlighting(): void {
  // Highlight OURS panel
  if (mergeState.oursPanelId !== null) {
    editor.clearAllOverlays(mergeState.oursPanelId);
    highlightPanel(mergeState.oursPanelId, "ours");
  }

  // Highlight THEIRS panel
  if (mergeState.theirsPanelId !== null) {
    editor.clearAllOverlays(mergeState.theirsPanelId);
    highlightPanel(mergeState.theirsPanelId, "theirs");
  }

  // Highlight RESULT panel
  if (mergeState.resultPanelId !== null) {
    editor.clearAllOverlays(mergeState.resultPanelId);
    highlightResultPanel(mergeState.resultPanelId);
  }
}

/**
 * Highlight a side panel (OURS or THEIRS)
 * Note: We compute content from our entries since getBufferText was removed
 *
 * TODO: Implement proper conflict region highlighting:
 * - Find actual conflict regions in git content by searching for conflict.ours/conflict.theirs text
 * - Highlight each conflict region with appropriate color (conflictOurs/conflictTheirs)
 * - Use different highlight for selected conflict vs unselected
 * - Consider using line-based highlighting for better visual effect
 */
function highlightPanel(bufferId: number, side: "ours" | "theirs"): void {
  // Build content from entries (same as what we set on the buffer)
  const entries = buildFullFileEntries(side);
  const content = entries.map(e => e.text).join("");
  const lines = content.split("\n");

  let byteOffset = 0;
  const conflictColor = side === "ours" ? colors.conflictOurs : colors.conflictTheirs;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight conflict header lines
    if (line.includes("--- Conflict")) {
      editor.addOverlay(
        bufferId,
        `merge-conflict-header-${lineIdx}`,
        lineStart,
        lineEnd,
        conflictColor[0],
        conflictColor[1],
        conflictColor[2],
        true // underline
      );
    }

    byteOffset = lineEnd + 1;
  }
}

/**
 * Highlight the RESULT panel
 * Note: We compute content from our entries since getBufferText was removed
 */
function highlightResultPanel(bufferId: number): void {
  // Build content from entries (same as what we set on the buffer)
  const entries = buildResultFileEntries();
  const content = entries.map(e => e.text).join("");
  const lines = content.split("\n");

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight conflict markers
    if (line.startsWith("<<<<<<<") || line.startsWith("=======") || line.startsWith(">>>>>>>")) {
      editor.addOverlay(
        bufferId,
        `merge-marker-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.unresolved[0],
        colors.unresolved[1],
        colors.unresolved[2],
        true // underline
      );
    }

    byteOffset = lineEnd + 1;
  }
}

/**
 * Update all panel views
 */
function updateViews(): void {
  if (mergeState.oursPanelId !== null) {
    editor.setVirtualBufferContent(mergeState.oursPanelId, buildFullFileEntries("ours"));
  }

  if (mergeState.theirsPanelId !== null) {
    editor.setVirtualBufferContent(mergeState.theirsPanelId, buildFullFileEntries("theirs"));
  }

  if (mergeState.resultPanelId !== null) {
    editor.setVirtualBufferContent(mergeState.resultPanelId, buildResultFileEntries());
  }

  applyHighlighting();
  updateStatusBar();
}

/**
 * Update status bar with merge progress
 */
function updateStatusBar(): void {
  const total = mergeState.conflicts.length;
  const resolved = mergeState.conflicts.filter(c => c.resolved).length;
  const remaining = total - resolved;

  if (remaining > 0) {
    editor.setStatus(editor.t("status.progress", { remaining: String(remaining), total: String(total), current: String(mergeState.selectedIndex + 1) }));
  } else {
    editor.setStatus(editor.t("status.all_resolved", { total: String(total) }));
  }
}

/**
 * Scroll all three panels to show the selected conflict
 * This computes the byte offset where the conflict appears in each panel's content
 * and uses setBufferCursor to scroll the viewport.
 */
function scrollToSelectedConflict(): void {
  const conflict = mergeState.conflicts[mergeState.selectedIndex];
  if (!conflict) return;

  // Scroll OURS panel
  if (mergeState.oursPanelId !== null) {
    const oursOffset = computeConflictOffset("ours", conflict.index);
    if (oursOffset >= 0) {
      editor.setBufferCursor(mergeState.oursPanelId, oursOffset);
    }
  }

  // Scroll THEIRS panel
  if (mergeState.theirsPanelId !== null) {
    const theirsOffset = computeConflictOffset("theirs", conflict.index);
    if (theirsOffset >= 0) {
      editor.setBufferCursor(mergeState.theirsPanelId, theirsOffset);
    }
  }

  // Scroll RESULT panel
  if (mergeState.resultPanelId !== null) {
    const resultOffset = computeResultConflictOffset(conflict.index);
    if (resultOffset >= 0) {
      editor.setBufferCursor(mergeState.resultPanelId, resultOffset);
    }
  }
}

/**
 * Compute the byte offset where a conflict appears in the OURS or THEIRS panel content.
 * We search for the actual conflict text (conflict.ours or conflict.theirs) in the
 * git content to find the exact position.
 */
function computeConflictOffset(side: "ours" | "theirs", conflictIndex: number): number {
  const gitContent = side === "ours" ? mergeState.oursContent : mergeState.theirsContent;
  const conflict = mergeState.conflicts[conflictIndex];

  if (!conflict) return 0;

  // Get the conflict text for this side
  const conflictText = side === "ours" ? conflict.ours : conflict.theirs;

  if (gitContent && conflictText) {
    // Strategy 1: Search for the exact conflict text (trimmed)
    const trimmedText = conflictText.trim();
    if (trimmedText.length > 0) {
      const pos = gitContent.indexOf(trimmedText);
      if (pos >= 0) {
        return pos;
      }
    }

    // Strategy 2: Search for the first line of the conflict
    const firstLine = conflictText.split("\n")[0]?.trim();
    if (firstLine && firstLine.length > 5) {
      const pos = gitContent.indexOf(firstLine);
      if (pos >= 0) {
        return pos;
      }
    }

    // Strategy 3: Ratio-based fallback
    const originalLength = mergeState.originalContent.length;
    if (originalLength > 0) {
      const ratio = conflict.startOffset / originalLength;
      return Math.floor(ratio * gitContent.length);
    }
  }

  // If no git content, we built entries manually - find "--- Conflict N ---"
  const entries = buildFullFileEntries(side);
  let offset = 0;
  for (const entry of entries) {
    if (entry.text.includes(`--- Conflict ${conflictIndex + 1} ---`)) {
      return offset;
    }
    offset += entry.text.length;
  }

  return 0;
}

/**
 * Compute the byte offset where a conflict appears in the RESULT panel content.
 * The RESULT panel shows the original file with conflict markers (<<<<<<< OURS, etc.)
 * We need to find the Nth <<<<<<< marker.
 */
function computeResultConflictOffset(conflictIndex: number): number {
  const entries = buildResultFileEntries();
  const content = entries.map(e => e.text).join("");

  // Find the Nth occurrence of <<<<<<< marker
  let searchPos = 0;
  let conflictCount = 0;

  while (searchPos < content.length) {
    const markerPos = content.indexOf("<<<<<<<", searchPos);
    if (markerPos === -1) break;

    if (conflictCount === conflictIndex) {
      return markerPos;
    }

    conflictCount++;
    searchPos = markerPos + 7; // Skip past "<<<<<<<" to continue searching
  }

  // Fallback: use ratio-based estimation like we do for OURS/THEIRS
  const conflict = mergeState.conflicts[conflictIndex];
  if (conflict && mergeState.originalContent.length > 0) {
    const ratio = conflict.startOffset / mergeState.originalContent.length;
    return Math.floor(ratio * content.length);
  }
  return 0;
}

// =============================================================================
// Public Commands - Activation
// =============================================================================

/**
 * Start merge conflict resolution for current buffer
 */
globalThis.start_merge_conflict = async function(): Promise<void> {
  if (mergeState.isActive) {
    editor.setStatus(editor.t("status.already_active"));
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const info = editor.getBufferInfo(bufferId);

  if (!info || !info.path) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }

  editor.debug(`Merge: starting for ${info.path}`);

  // Get the directory of the file for running git commands
  const fileDir = editor.pathDirname(info.path);
  editor.debug(`Merge: file directory is ${fileDir}`);

  // Check if we're in a git repo (run from file's directory)
  const gitCheck = await editor.spawnProcess("git", ["rev-parse", "--is-inside-work-tree"], fileDir);
  editor.debug(`Merge: git rev-parse exit_code=${gitCheck.exit_code}, stdout=${gitCheck.stdout.trim()}`);

  if (gitCheck.exit_code !== 0 || gitCheck.stdout.trim() !== "true") {
    editor.setStatus(editor.t("status.not_git_repo"));
    return;
  }

  // Check if file has unmerged entries using git (run from file's directory)
  const lsFilesResult = await editor.spawnProcess("git", ["ls-files", "-u", info.path], fileDir);
  editor.debug(`Merge: git ls-files -u exit_code=${lsFilesResult.exit_code}, stdout length=${lsFilesResult.stdout.length}, stderr=${lsFilesResult.stderr}`);

  const hasUnmergedEntries = lsFilesResult.exit_code === 0 && lsFilesResult.stdout.trim().length > 0;

  if (!hasUnmergedEntries) {
    editor.setStatus(editor.t("status.no_unmerged"));
    return;
  }

  // Get file content from git's working tree (has conflict markers)
  const catFileResult = await editor.spawnProcess("git", ["show", `:0:${info.path}`]);

  // If :0: doesn't exist, read the working tree file directly
  let content: string;
  if (catFileResult.exit_code !== 0) {
    editor.debug(`Merge: git show :0: failed, reading working tree file`);
    const fileContent = await editor.readFile(info.path);
    if (!fileContent) {
      editor.setStatus(editor.t("status.failed_read"));
      return;
    }
    content = fileContent;
  } else {
    // The staged version shouldn't have conflict markers, use working tree
    const fileContent = await editor.readFile(info.path);
    if (!fileContent) {
      editor.setStatus(editor.t("status.failed_read"));
      return;
    }
    content = fileContent;
  }

  // Check for conflict markers in content
  const hasMarkers = hasConflictMarkers(content);
  editor.debug(`Merge: file has conflict markers: ${hasMarkers}, content length: ${content.length}`);

  if (!hasMarkers) {
    editor.setStatus(editor.t("status.no_markers"));
    return;
  }

  editor.setStatus(editor.t("status.starting"));

  // Store original state
  mergeState.sourceBufferId = bufferId;
  mergeState.sourcePath = info.path;
  mergeState.originalContent = content;

  // Parse conflicts
  mergeState.conflicts = parseConflicts(content);

  // Debug: log parse results
  editor.debug(`Merge: parseConflicts found ${mergeState.conflicts.length} conflicts`);

  if (mergeState.conflicts.length === 0) {
    editor.setStatus(editor.t("status.failed_parse"));
    // Log more detail for debugging
    editor.debug(`Merge: regex failed, content has <<<<<<< at index ${content.indexOf("<<<<<<<")}`);
    editor.debug(`Merge: content around <<<<<<< : ${content.substring(content.indexOf("<<<<<<<") - 20, content.indexOf("<<<<<<<") + 100)}`);
    return;
  }

  editor.debug(`Found ${mergeState.conflicts.length} conflicts`);

  // Fetch git versions for auto-resolution
  const versions = await fetchGitVersions(info.path);
  if (versions) {
    mergeState.baseContent = versions.base;
    mergeState.oursContent = versions.ours;
    mergeState.theirsContent = versions.theirs;
    editor.debug("Fetched git versions for auto-resolution");
  }

  // Attempt auto-resolution
  autoResolveConflicts(mergeState.conflicts);

  const autoResolved = mergeState.conflicts.filter(c => c.resolved).length;
  if (autoResolved > 0) {
    editor.debug(`Auto-resolved ${autoResolved} trivial conflicts`);
  }

  // Find first unresolved conflict
  mergeState.selectedIndex = 0;
  for (let i = 0; i < mergeState.conflicts.length; i++) {
    if (!mergeState.conflicts[i].resolved) {
      mergeState.selectedIndex = i;
      break;
    }
  }

  // Create the merge UI panels
  await createMergePanels();

  mergeState.isActive = true;

  // Register merge-mode commands now that we're active
  registerMergeModeCommands();

  updateViews();

  // Scroll all panels to show the first conflict
  scrollToSelectedConflict();

  const remaining = mergeState.conflicts.length - autoResolved;
  if (remaining > 0) {
    editor.setStatus(editor.t("status.conflicts_to_resolve", { remaining: String(remaining), auto_resolved: String(autoResolved) }));
  } else {
    editor.setStatus(editor.t("status.all_auto_resolved", { total: String(mergeState.conflicts.length) }));
  }
};

/**
 * Create the multi-panel merge UI (JetBrains-style: OURS | RESULT | THEIRS)
 *
 * Creates three vertical splits and then calls distributeSplitsEvenly()
 * to ensure all panels get equal width.
 */
async function createMergePanels(): Promise<void> {
  // Get the source file's extension for syntax highlighting
  // Tree-sitter uses filename extension to determine language
  const sourceExt = mergeState.sourcePath
    ? mergeState.sourcePath.substring(mergeState.sourcePath.lastIndexOf("."))
    : "";

  editor.debug(`Merge: source extension '${sourceExt}' for syntax highlighting`);

  // Create OURS panel first (takes over current view)
  // Include extension in name so tree-sitter can apply highlighting
  const oursId = await editor.createVirtualBuffer({
    name: `*OURS*${sourceExt}`,
    mode: "merge-conflict",
    read_only: true,
    entries: buildFullFileEntries("ours"),
    show_line_numbers: true,
    show_cursors: true,
    editing_disabled: true,
  });

  if (oursId !== null) {
    mergeState.oursPanelId = oursId;
    mergeState.oursSplitId = editor.getActiveSplitId();
  }

  // Create THEIRS panel to the right (vertical split)
  const theirsResult = await editor.createVirtualBufferInSplit({
    name: `*THEIRS*${sourceExt}`,
    mode: "merge-conflict",
    read_only: true,
    entries: buildFullFileEntries("theirs"),
    ratio: 0.5,  // Will be equalized by distributeSplitsEvenly
    direction: "vertical",
    panel_id: "merge-theirs",
    show_line_numbers: true,
    show_cursors: true,
    editing_disabled: true,
  });

  if (theirsResult !== null) {
    mergeState.theirsPanelId = theirsResult.buffer_id;
    mergeState.theirsSplitId = theirsResult.split_id ?? editor.getActiveSplitId();
  }

  // Focus back on OURS and create RESULT in the middle
  if (mergeState.oursSplitId !== null) {
    editor.focusSplit(mergeState.oursSplitId);
  }

  const resultResult = await editor.createVirtualBufferInSplit({
    name: `*RESULT*${sourceExt}`,
    mode: "merge-result",
    read_only: false,
    entries: buildResultFileEntries(),
    ratio: 0.5,  // Will be equalized by distributeSplitsEvenly
    direction: "vertical",
    panel_id: "merge-result",
    show_line_numbers: true,
    show_cursors: true,
    editing_disabled: false,
  });

  if (resultResult !== null) {
    mergeState.resultPanelId = resultResult.buffer_id;
    mergeState.resultSplitId = resultResult.split_id ?? editor.getActiveSplitId();
  }

  // Distribute splits evenly so all three panels get equal width
  editor.distributeSplitsEvenly();

  // Focus the RESULT panel since that's where the user will resolve conflicts
  if (mergeState.resultSplitId !== null) {
    editor.focusSplit(mergeState.resultSplitId);
  }
}

// =============================================================================
// Public Commands - Navigation
// =============================================================================

globalThis.merge_next_conflict = function(): void {
  editor.debug(`merge_next_conflict called, isActive=${mergeState.isActive}, conflicts=${mergeState.conflicts.length}`);

  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.no_active_merge"));
    return;
  }
  if (mergeState.conflicts.length === 0) {
    editor.setStatus(editor.t("status.no_conflicts"));
    return;
  }
  if (mergeState.conflicts.length === 1) {
    // Single conflict: just re-scroll to it (useful for re-focusing)
    editor.setStatus(editor.t("status.single_refocused"));
    scrollToSelectedConflict();
    return;
  }

  // Find next unresolved conflict (or wrap around)
  let startIndex = mergeState.selectedIndex;
  let index = (startIndex + 1) % mergeState.conflicts.length;

  // First try to find next unresolved
  while (index !== startIndex) {
    if (!mergeState.conflicts[index].resolved) {
      mergeState.selectedIndex = index;
      editor.setStatus(editor.t("status.conflict_of", { current: String(index + 1), total: String(mergeState.conflicts.length) }));
      updateViews();
      scrollToSelectedConflict();
      return;
    }
    index = (index + 1) % mergeState.conflicts.length;
  }

  // If all resolved, just move to next
  mergeState.selectedIndex = (mergeState.selectedIndex + 1) % mergeState.conflicts.length;
  editor.setStatus(editor.t("status.conflict_all_resolved", { current: String(mergeState.selectedIndex + 1), total: String(mergeState.conflicts.length) }));
  updateViews();
  scrollToSelectedConflict();
};

globalThis.merge_prev_conflict = function(): void {
  editor.debug(`merge_prev_conflict called, isActive=${mergeState.isActive}, conflicts=${mergeState.conflicts.length}`);

  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.no_active_merge"));
    return;
  }
  if (mergeState.conflicts.length === 0) {
    editor.setStatus(editor.t("status.no_conflicts"));
    return;
  }
  if (mergeState.conflicts.length === 1) {
    // Single conflict: just re-scroll to it (useful for re-focusing)
    editor.setStatus(editor.t("status.single_refocused"));
    scrollToSelectedConflict();
    return;
  }

  // Find previous unresolved conflict (or wrap around)
  let startIndex = mergeState.selectedIndex;
  let index = (startIndex - 1 + mergeState.conflicts.length) % mergeState.conflicts.length;

  // First try to find previous unresolved
  while (index !== startIndex) {
    if (!mergeState.conflicts[index].resolved) {
      mergeState.selectedIndex = index;
      editor.setStatus(editor.t("status.conflict_of", { current: String(index + 1), total: String(mergeState.conflicts.length) }));
      updateViews();
      scrollToSelectedConflict();
      return;
    }
    index = (index - 1 + mergeState.conflicts.length) % mergeState.conflicts.length;
  }

  // If all resolved, just move to previous
  mergeState.selectedIndex = (mergeState.selectedIndex - 1 + mergeState.conflicts.length) % mergeState.conflicts.length;
  editor.setStatus(editor.t("status.conflict_all_resolved", { current: String(mergeState.selectedIndex + 1), total: String(mergeState.conflicts.length) }));
  updateViews();
  scrollToSelectedConflict();
};

// =============================================================================
// Public Commands - Resolution
// =============================================================================

globalThis.merge_use_ours = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.no_active_merge"));
    return;
  }

  const conflict = mergeState.conflicts[mergeState.selectedIndex];
  if (!conflict) return;

  conflict.resolved = true;
  conflict.resolution = "ours";
  conflict.resolvedContent = conflict.ours;

  editor.debug(`Resolved conflict ${conflict.index} with OURS`);

  // Move to next unresolved conflict
  moveToNextUnresolved();
  updateViews();
};

globalThis.merge_take_theirs = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.no_active_merge"));
    return;
  }

  const conflict = mergeState.conflicts[mergeState.selectedIndex];
  if (!conflict) return;

  conflict.resolved = true;
  conflict.resolution = "theirs";
  conflict.resolvedContent = conflict.theirs;

  editor.debug(`Resolved conflict ${conflict.index} with THEIRS`);

  // Move to next unresolved conflict
  moveToNextUnresolved();
  updateViews();
};

globalThis.merge_use_both = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.no_active_merge"));
    return;
  }

  const conflict = mergeState.conflicts[mergeState.selectedIndex];
  if (!conflict) return;

  conflict.resolved = true;
  conflict.resolution = "both";
  conflict.resolvedContent = conflict.ours + conflict.theirs;

  editor.debug(`Resolved conflict ${conflict.index} with BOTH`);

  // Move to next unresolved conflict
  moveToNextUnresolved();
  updateViews();
};

/**
 * Move selection to the next unresolved conflict
 */
function moveToNextUnresolved(): void {
  const startIndex = mergeState.selectedIndex;
  let index = (startIndex + 1) % mergeState.conflicts.length;

  while (index !== startIndex) {
    if (!mergeState.conflicts[index].resolved) {
      mergeState.selectedIndex = index;
      return;
    }
    index = (index + 1) % mergeState.conflicts.length;
  }

  // All resolved, stay where we are
}

// =============================================================================
// Public Commands - Completion
// =============================================================================

globalThis.merge_save_and_exit = async function(): Promise<void> {
  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.no_active_merge"));
    return;
  }

  const unresolvedCount = mergeState.conflicts.filter(c => !c.resolved).length;

  if (unresolvedCount > 0) {
    // TODO: Add confirmation prompt
    editor.setStatus(editor.t("status.cannot_save", { count: String(unresolvedCount) }));
    return;
  }

  // Build final content by replacing conflict markers with resolved content
  let finalContent = mergeState.originalContent;

  // Process conflicts in reverse order to preserve offsets
  const sortedConflicts = [...mergeState.conflicts].sort((a, b) => b.startOffset - a.startOffset);

  for (const conflict of sortedConflicts) {
    if (conflict.resolvedContent !== undefined) {
      finalContent =
        finalContent.substring(0, conflict.startOffset) +
        conflict.resolvedContent +
        finalContent.substring(conflict.endOffset);
    }
  }

  // Update the original buffer with resolved content
  if (mergeState.sourceBufferId !== null) {
    const bufferLength = editor.getBufferLength(mergeState.sourceBufferId);

    // Delete all content
    if (bufferLength > 0) {
      editor.deleteRange(mergeState.sourceBufferId, 0, bufferLength);
    }

    // Insert resolved content
    editor.insertText(mergeState.sourceBufferId, 0, finalContent);

    editor.debug("Applied resolved content to source buffer");
  }

  // Close merge panels
  closeMergePanels();

  editor.setStatus(editor.t("status.complete"));
};

globalThis.merge_abort = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus(editor.t("status.nothing_to_abort"));
    return;
  }

  // TODO: Add confirmation prompt if there are resolutions

  // Close merge panels without saving
  closeMergePanels();

  editor.setStatus(editor.t("status.aborted"));
};

/**
 * Close all merge panels and reset state
 */
function closeMergePanels(): void {
  // Close buffers
  if (mergeState.oursPanelId !== null) {
    editor.closeBuffer(mergeState.oursPanelId);
  }
  if (mergeState.theirsPanelId !== null) {
    editor.closeBuffer(mergeState.theirsPanelId);
  }
  if (mergeState.resultPanelId !== null) {
    editor.closeBuffer(mergeState.resultPanelId);
  }

  // Close splits
  if (mergeState.oursSplitId !== null) {
    editor.closeSplit(mergeState.oursSplitId);
  }
  if (mergeState.theirsSplitId !== null) {
    editor.closeSplit(mergeState.theirsSplitId);
  }
  if (mergeState.resultSplitId !== null) {
    editor.closeSplit(mergeState.resultSplitId);
  }

  // Focus back on source buffer if it exists
  if (mergeState.sourceBufferId !== null) {
    editor.showBuffer(mergeState.sourceBufferId);
  }

  // Unregister merge-mode commands
  unregisterMergeModeCommands();

  // Reset state
  mergeState.isActive = false;
  mergeState.sourceBufferId = null;
  mergeState.sourcePath = null;
  mergeState.originalContent = "";
  mergeState.conflicts = [];
  mergeState.selectedIndex = 0;
  mergeState.oursPanelId = null;
  mergeState.theirsPanelId = null;
  mergeState.resultPanelId = null;
  mergeState.oursSplitId = null;
  mergeState.theirsSplitId = null;
  mergeState.resultSplitId = null;
  mergeState.oursContent = "";
  mergeState.theirsContent = "";
  mergeState.baseContent = "";
  mergeState.resultContent = "";
}

// =============================================================================
// Public Commands - Help
// =============================================================================

globalThis.merge_show_help = function(): void {
  editor.setStatus(editor.t("status.help"));
};

// =============================================================================
// Hook Handlers - Auto-Detection
// =============================================================================

/**
 * Handle buffer activation - check for conflict markers
 */
globalThis.onMergeBufferActivated = async function(data: { buffer_id: number }): Promise<void> {
  // Don't trigger if already in merge mode
  if (mergeState.isActive) return;

  // Don't trigger for virtual buffers
  const info = editor.getBufferInfo(data.buffer_id);
  if (!info || !info.path) return;

  // Get the directory of the file for running git commands
  const fileDir = editor.pathDirname(info.path);

  // Check if we're in a git repo first
  try {
    const gitCheck = await editor.spawnProcess("git", ["rev-parse", "--is-inside-work-tree"], fileDir);
    if (gitCheck.exit_code !== 0) return;

    // Check for unmerged entries
    const lsFiles = await editor.spawnProcess("git", ["ls-files", "-u", info.path], fileDir);
    if (lsFiles.exit_code === 0 && lsFiles.stdout.trim().length > 0) {
      editor.setStatus(editor.t("status.detected"));
    }
  } catch (e) {
    // Not in git repo or other error, ignore
  }
};

/**
 * Handle file open - check for conflict markers
 */
globalThis.onMergeAfterFileOpen = async function(data: { buffer_id: number; path: string }): Promise<void> {
  // Don't trigger if already in merge mode
  if (mergeState.isActive) return;

  // Get the directory of the file for running git commands
  const fileDir = editor.pathDirname(data.path);

  // Check if we're in a git repo first
  try {
    const gitCheck = await editor.spawnProcess("git", ["rev-parse", "--is-inside-work-tree"], fileDir);
    if (gitCheck.exit_code !== 0) return;

    // Check for unmerged entries
    const lsFiles = await editor.spawnProcess("git", ["ls-files", "-u", data.path], fileDir);
    if (lsFiles.exit_code === 0 && lsFiles.stdout.trim().length > 0) {
      editor.setStatus(editor.t("status.detected_file", { path: data.path }));
    }
  } catch (e) {
    // Not in git repo or other error, ignore
  }
};

// =============================================================================
// Hook Registration
// =============================================================================

editor.on("buffer_activated", "onMergeBufferActivated");
editor.on("after_file_open", "onMergeAfterFileOpen");

// =============================================================================
// Command Registration - Dynamic based on merge mode state
// =============================================================================

// Commands that are only available during active merge mode
const MERGE_MODE_COMMANDS = [
  { name: "%cmd.next", desc: "%cmd.next_desc", action: "merge_next_conflict" },
  { name: "%cmd.prev", desc: "%cmd.prev_desc", action: "merge_prev_conflict" },
  { name: "%cmd.use_ours", desc: "%cmd.use_ours_desc", action: "merge_use_ours" },
  { name: "%cmd.take_theirs", desc: "%cmd.take_theirs_desc", action: "merge_take_theirs" },
  { name: "%cmd.use_both", desc: "%cmd.use_both_desc", action: "merge_use_both" },
  { name: "%cmd.save_exit", desc: "%cmd.save_exit_desc", action: "merge_save_and_exit" },
  { name: "%cmd.abort", desc: "%cmd.abort_desc", action: "merge_abort" },
];

/**
 * Register merge-mode specific commands (called when merge mode starts)
 */
function registerMergeModeCommands(): void {
  for (const cmd of MERGE_MODE_COMMANDS) {
    editor.registerCommand(cmd.name, cmd.desc, cmd.action, "normal");
  }
}

/**
 * Unregister merge-mode specific commands (called when merge mode ends)
 */
function unregisterMergeModeCommands(): void {
  for (const cmd of MERGE_MODE_COMMANDS) {
    editor.unregisterCommand(cmd.name);
  }
}

// Only register "Start Resolution" at plugin load - other commands are registered dynamically
editor.registerCommand(
  "%cmd.start",
  "%cmd.start_desc",
  "start_merge_conflict",
  "normal"
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.setStatus(editor.t("status.ready"));
editor.debug("Merge plugin initialized - Use 'Merge: Start Resolution' for files with conflicts");
