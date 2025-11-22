/// <reference path="../types/fresh.d.ts" />

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
editor.defineMode(
  "merge-conflict",
  null, // no parent mode
  [
    // Navigation
    ["n", "merge_next_conflict"],
    ["j", "merge_next_conflict"],
    ["Down", "merge_next_conflict"],
    ["p", "merge_prev_conflict"],
    ["k", "merge_prev_conflict"],
    ["Up", "merge_prev_conflict"],

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
    // Navigation (override in result panel)
    ["C-n", "merge_next_conflict"],
    ["C-p", "merge_prev_conflict"],

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
  const conflictRegex = /^<<<<<<<[^\n]*\n([\s\S]*?)(?:^\|\|\|\|\|\|\|[^\n]*\n([\s\S]*?))?^=======\n([\s\S]*?)^>>>>>>>[^\n]*$/gm;

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
    // Get OURS version (--ours or :2:)
    const oursResult = await editor.spawnProcess("git", [
      "show", `:2:${filePath}`
    ]);

    // Get THEIRS version (--theirs or :3:)
    const theirsResult = await editor.spawnProcess("git", [
      "show", `:3:${filePath}`
    ]);

    // Get BASE version (common ancestor, :1:)
    const baseResult = await editor.spawnProcess("git", [
      "show", `:1:${filePath}`
    ]);

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
// View Rendering
// =============================================================================

/**
 * Build entries for OURS panel
 */
function buildOursEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: "  OURS (Read-only) - Changes from your branch\n",
    properties: { type: "header", panel: "ours" },
  });
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Show each conflict's OURS side
  for (const conflict of mergeState.conflicts) {
    const isSelected = conflict.index === mergeState.selectedIndex;
    const marker = isSelected ? "▶ " : "  ";
    const status = conflict.resolved ? "[RESOLVED]" : "[PENDING]";

    entries.push({
      text: `\n${marker}Conflict ${conflict.index + 1} ${status}\n`,
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
    const content = conflict.ours || "(empty)";
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
    text: "  THEIRS (Read-only) - Incoming changes\n",
    properties: { type: "header", panel: "theirs" },
  });
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Show each conflict's THEIRS side
  for (const conflict of mergeState.conflicts) {
    const isSelected = conflict.index === mergeState.selectedIndex;
    const marker = isSelected ? "▶ " : "  ";
    const status = conflict.resolved ? "[RESOLVED]" : "[PENDING]";

    entries.push({
      text: `\n${marker}Conflict ${conflict.index + 1} ${status}\n`,
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
    const content = conflict.theirs || "(empty)";
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
    text: "  RESULT (Editable) - Resolved content\n",
    properties: { type: "header", panel: "result" },
  });
  entries.push({
    text: "═══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Build result content
  let resultText = "";
  const unresolvedCount = mergeState.conflicts.filter(c => !c.resolved).length;

  if (unresolvedCount > 0) {
    entries.push({
      text: `\n  ⚠ ${unresolvedCount} conflict(s) remaining\n\n`,
      properties: { type: "warning" },
    });
  } else {
    entries.push({
      text: "\n  ✓ All conflicts resolved!\n\n",
      properties: { type: "success" },
    });
  }

  // Show resolved content or action buttons for each conflict
  for (const conflict of mergeState.conflicts) {
    const isSelected = conflict.index === mergeState.selectedIndex;
    const marker = isSelected ? "▶ " : "  ";

    entries.push({
      text: `${marker}Conflict ${conflict.index + 1}:\n`,
      properties: {
        type: "conflict-header",
        conflictIndex: conflict.index,
        selected: isSelected,
      },
    });

    if (conflict.resolved && conflict.resolvedContent !== undefined) {
      // Show resolved content
      entries.push({
        text: `  [Resolved: ${conflict.resolution}]\n`,
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
      // Show action prompt
      entries.push({
        text: "  << [u] Accept Ours | [t] Accept Theirs | [b] Both >>\n",
        properties: {
          type: "action-buttons",
          conflictIndex: conflict.index,
        },
      });
    }

    entries.push({
      text: "─────────────────────────────────────────────────────────────────────────────\n",
      properties: { type: "separator" },
    });
  }

  // Help bar
  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });
  entries.push({
    text: "[n] Next [p] Prev | [u] Use Ours [t] Take Theirs [b] Both | [s] Save & Exit [q] Abort\n",
    properties: { type: "help-bar" },
  });

  return entries;
}

/**
 * Apply visual highlighting to panels
 */
function applyHighlighting(): void {
  // Highlight OURS panel
  if (mergeState.oursPanelId !== null) {
    editor.removeOverlaysByPrefix(mergeState.oursPanelId, "merge-");
    highlightPanel(mergeState.oursPanelId, "ours");
  }

  // Highlight THEIRS panel
  if (mergeState.theirsPanelId !== null) {
    editor.removeOverlaysByPrefix(mergeState.theirsPanelId, "merge-");
    highlightPanel(mergeState.theirsPanelId, "theirs");
  }

  // Highlight RESULT panel
  if (mergeState.resultPanelId !== null) {
    editor.removeOverlaysByPrefix(mergeState.resultPanelId, "merge-");
    highlightResultPanel(mergeState.resultPanelId);
  }
}

/**
 * Highlight a side panel (OURS or THEIRS)
 */
function highlightPanel(bufferId: number, side: "ours" | "theirs"): void {
  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);
  const lines = content.split("\n");

  let byteOffset = 0;
  const headerColor = side === "ours" ? colors.oursHeader : colors.theirsHeader;
  const conflictColor = side === "ours" ? colors.conflictOurs : colors.conflictTheirs;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight headers
    if (line.includes("OURS") || line.includes("THEIRS")) {
      editor.addOverlay(
        bufferId,
        `merge-header-${lineIdx}`,
        lineStart,
        lineEnd,
        headerColor[0],
        headerColor[1],
        headerColor[2],
        true // underline
      );
    }

    // Highlight selected conflict
    if (line.startsWith("▶ ")) {
      editor.addOverlay(
        bufferId,
        `merge-selected-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.selected[0],
        colors.selected[1],
        colors.selected[2],
        false
      );
    }

    // Highlight resolved/pending status
    if (line.includes("[RESOLVED]")) {
      const statusStart = lineStart + line.indexOf("[RESOLVED]");
      editor.addOverlay(
        bufferId,
        `merge-resolved-${lineIdx}`,
        statusStart,
        statusStart + 10,
        colors.resolved[0],
        colors.resolved[1],
        colors.resolved[2],
        false
      );
    } else if (line.includes("[PENDING]")) {
      const statusStart = lineStart + line.indexOf("[PENDING]");
      editor.addOverlay(
        bufferId,
        `merge-pending-${lineIdx}`,
        statusStart,
        statusStart + 9,
        colors.unresolved[0],
        colors.unresolved[1],
        colors.unresolved[2],
        false
      );
    }

    byteOffset = lineEnd + 1;
  }
}

/**
 * Highlight the RESULT panel
 */
function highlightResultPanel(bufferId: number): void {
  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);
  const lines = content.split("\n");

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight header
    if (line.includes("RESULT")) {
      editor.addOverlay(
        bufferId,
        `merge-header-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.resultHeader[0],
        colors.resultHeader[1],
        colors.resultHeader[2],
        true // underline
      );
    }

    // Highlight selected conflict
    if (line.startsWith("▶ ")) {
      editor.addOverlay(
        bufferId,
        `merge-selected-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.selected[0],
        colors.selected[1],
        colors.selected[2],
        false
      );
    }

    // Highlight action buttons
    if (line.includes("<<") && line.includes(">>")) {
      editor.addOverlay(
        bufferId,
        `merge-buttons-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.button[0],
        colors.button[1],
        colors.button[2],
        false
      );
    }

    // Highlight help bar
    if (line.includes("[n] Next") && line.includes("[s] Save")) {
      editor.addOverlay(
        bufferId,
        `merge-help-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.button[0],
        colors.button[1],
        colors.button[2],
        false
      );
    }

    // Highlight warning/success messages
    if (line.includes("conflict(s) remaining")) {
      editor.addOverlay(
        bufferId,
        `merge-warning-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.unresolved[0],
        colors.unresolved[1],
        colors.unresolved[2],
        false
      );
    } else if (line.includes("All conflicts resolved")) {
      editor.addOverlay(
        bufferId,
        `merge-success-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.resolved[0],
        colors.resolved[1],
        colors.resolved[2],
        false
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
    editor.setVirtualBufferContent(mergeState.oursPanelId, buildOursEntries());
  }

  if (mergeState.theirsPanelId !== null) {
    editor.setVirtualBufferContent(mergeState.theirsPanelId, buildTheirsEntries());
  }

  if (mergeState.resultPanelId !== null) {
    editor.setVirtualBufferContent(mergeState.resultPanelId, buildResultEntries());
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
    editor.setStatus(`Merge: ${remaining} of ${total} conflicts remaining | Current: ${mergeState.selectedIndex + 1}`);
  } else {
    editor.setStatus(`Merge: All ${total} conflicts resolved! Press 's' to save`);
  }
}

// =============================================================================
// Public Commands - Activation
// =============================================================================

/**
 * Start merge conflict resolution for current buffer
 */
globalThis.start_merge_conflict = async function(): Promise<void> {
  if (mergeState.isActive) {
    editor.setStatus("Merge mode already active");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const info = editor.getBufferInfo(bufferId);

  if (!info || !info.path) {
    editor.setStatus("No file open");
    return;
  }

  // Get buffer content
  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);

  // Check for conflict markers
  if (!hasConflictMarkers(content)) {
    editor.setStatus("No conflict markers found in this file");
    return;
  }

  editor.setStatus("Starting merge conflict resolution...");

  // Store original state
  mergeState.sourceBufferId = bufferId;
  mergeState.sourcePath = info.path;
  mergeState.originalContent = content;

  // Parse conflicts
  mergeState.conflicts = parseConflicts(content);

  if (mergeState.conflicts.length === 0) {
    editor.setStatus("Failed to parse conflict markers");
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
  updateViews();

  const remaining = mergeState.conflicts.length - autoResolved;
  if (remaining > 0) {
    editor.setStatus(`Merge: ${remaining} conflicts to resolve (${autoResolved} auto-resolved)`);
  } else {
    editor.setStatus(`Merge: All ${mergeState.conflicts.length} conflicts auto-resolved! Press 's' to save`);
  }
};

/**
 * Create the multi-panel merge UI
 */
async function createMergePanels(): Promise<void> {
  // Create OURS panel (top-left)
  const oursId = await editor.createVirtualBufferInSplit({
    name: "*Merge: OURS*",
    mode: "merge-conflict",
    read_only: true,
    entries: buildOursEntries(),
    ratio: 0.5,
    panel_id: "merge-ours",
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (oursId !== null) {
    mergeState.oursPanelId = oursId;
    mergeState.oursSplitId = editor.getActiveSplitId();
  }

  // Create THEIRS panel (right of OURS) - using horizontal split
  const theirsId = await editor.createVirtualBufferInSplit({
    name: "*Merge: THEIRS*",
    mode: "merge-conflict",
    read_only: true,
    entries: buildTheirsEntries(),
    ratio: 0.5,
    panel_id: "merge-theirs",
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (theirsId !== null) {
    mergeState.theirsPanelId = theirsId;
    mergeState.theirsSplitId = editor.getActiveSplitId();
  }

  // Create RESULT panel (bottom, full width)
  const resultId = await editor.createVirtualBufferInSplit({
    name: "*Merge: RESULT*",
    mode: "merge-conflict",
    read_only: true,
    entries: buildResultEntries(),
    ratio: 0.4,
    panel_id: "merge-result",
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (resultId !== null) {
    mergeState.resultPanelId = resultId;
    mergeState.resultSplitId = editor.getActiveSplitId();
  }
}

// =============================================================================
// Public Commands - Navigation
// =============================================================================

globalThis.merge_next_conflict = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus("No active merge - use 'Merge: Start Resolution' first");
    return;
  }
  if (mergeState.conflicts.length === 0) return;

  // Find next unresolved conflict (or wrap around)
  let startIndex = mergeState.selectedIndex;
  let index = (startIndex + 1) % mergeState.conflicts.length;

  // First try to find next unresolved
  while (index !== startIndex) {
    if (!mergeState.conflicts[index].resolved) {
      mergeState.selectedIndex = index;
      updateViews();
      return;
    }
    index = (index + 1) % mergeState.conflicts.length;
  }

  // If all resolved (or only one), just move to next
  mergeState.selectedIndex = (mergeState.selectedIndex + 1) % mergeState.conflicts.length;
  updateViews();
};

globalThis.merge_prev_conflict = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus("No active merge - use 'Merge: Start Resolution' first");
    return;
  }
  if (mergeState.conflicts.length === 0) return;

  // Find previous unresolved conflict (or wrap around)
  let startIndex = mergeState.selectedIndex;
  let index = (startIndex - 1 + mergeState.conflicts.length) % mergeState.conflicts.length;

  // First try to find previous unresolved
  while (index !== startIndex) {
    if (!mergeState.conflicts[index].resolved) {
      mergeState.selectedIndex = index;
      updateViews();
      return;
    }
    index = (index - 1 + mergeState.conflicts.length) % mergeState.conflicts.length;
  }

  // If all resolved (or only one), just move to previous
  mergeState.selectedIndex = (mergeState.selectedIndex - 1 + mergeState.conflicts.length) % mergeState.conflicts.length;
  updateViews();
};

// =============================================================================
// Public Commands - Resolution
// =============================================================================

globalThis.merge_use_ours = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus("No active merge - use 'Merge: Start Resolution' first");
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
    editor.setStatus("No active merge - use 'Merge: Start Resolution' first");
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
    editor.setStatus("No active merge - use 'Merge: Start Resolution' first");
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
    editor.setStatus("No active merge - use 'Merge: Start Resolution' first");
    return;
  }

  const unresolvedCount = mergeState.conflicts.filter(c => !c.resolved).length;

  if (unresolvedCount > 0) {
    // TODO: Add confirmation prompt
    editor.setStatus(`Cannot save: ${unresolvedCount} unresolved conflicts remaining`);
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
      editor.deleteRange(mergeState.sourceBufferId, { start: 0, end: bufferLength });
    }

    // Insert resolved content
    editor.insertText(mergeState.sourceBufferId, 0, finalContent);

    editor.debug("Applied resolved content to source buffer");
  }

  // Close merge panels
  closeMergePanels();

  editor.setStatus("Merge complete! File updated with resolved content");
};

globalThis.merge_abort = function(): void {
  if (!mergeState.isActive) {
    editor.setStatus("No active merge - nothing to abort");
    return;
  }

  // TODO: Add confirmation prompt if there are resolutions

  // Close merge panels without saving
  closeMergePanels();

  editor.setStatus("Merge aborted - no changes made");
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
  editor.setStatus(
    "Merge: [n/p] Navigate | [u] Ours [t] Theirs [b] Both | [s] Save [q] Abort"
  );
};

// =============================================================================
// Hook Handlers - Auto-Detection
// =============================================================================

/**
 * Handle buffer activation - check for conflict markers
 */
globalThis.onMergeBufferActivated = function(data: { buffer_id: number }): void {
  // Don't trigger if already in merge mode
  if (mergeState.isActive) return;

  // Don't trigger for virtual buffers
  const info = editor.getBufferInfo(data.buffer_id);
  if (!info || !info.path) return;

  // Check for conflict markers
  const bufferLength = editor.getBufferLength(data.buffer_id);
  const content = editor.getBufferText(data.buffer_id, 0, Math.min(bufferLength, 10000));

  if (hasConflictMarkers(content)) {
    editor.setStatus(`Conflicts detected! Use 'Merge: Start Resolution' or run start_merge_conflict`);
  }
};

/**
 * Handle file open - check for conflict markers
 */
globalThis.onMergeAfterFileOpen = function(data: { buffer_id: number; path: string }): void {
  // Don't trigger if already in merge mode
  if (mergeState.isActive) return;

  // Check for conflict markers
  const bufferLength = editor.getBufferLength(data.buffer_id);
  const content = editor.getBufferText(data.buffer_id, 0, Math.min(bufferLength, 10000));

  if (hasConflictMarkers(content)) {
    editor.setStatus(`⚠ Merge conflicts detected in ${data.path} - Use 'Merge: Start Resolution'`);
  }
};

// =============================================================================
// Hook Registration
// =============================================================================

editor.on("buffer_activated", "onMergeBufferActivated");
editor.on("after-file-open", "onMergeAfterFileOpen");

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "Merge: Start Resolution",
  "Start 3-way merge conflict resolution for current file",
  "start_merge_conflict",
  "normal"
);

editor.registerCommand(
  "Merge: Next Conflict",
  "Jump to next unresolved conflict",
  "merge_next_conflict",
  "normal"
);

editor.registerCommand(
  "Merge: Previous Conflict",
  "Jump to previous unresolved conflict",
  "merge_prev_conflict",
  "normal"
);

editor.registerCommand(
  "Merge: Use Ours",
  "Accept our version for current conflict",
  "merge_use_ours",
  "normal"
);

editor.registerCommand(
  "Merge: Take Theirs",
  "Accept their version for current conflict",
  "merge_take_theirs",
  "normal"
);

editor.registerCommand(
  "Merge: Use Both",
  "Accept both versions for current conflict",
  "merge_use_both",
  "normal"
);

editor.registerCommand(
  "Merge: Save & Exit",
  "Save resolved content and exit merge mode",
  "merge_save_and_exit",
  "normal"
);

editor.registerCommand(
  "Merge: Abort",
  "Abort merge resolution without saving",
  "merge_abort",
  "normal"
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.setStatus("Merge Conflict Resolution plugin loaded");
editor.debug("Merge plugin initialized - Use 'Merge: Start Resolution' for files with conflicts");
