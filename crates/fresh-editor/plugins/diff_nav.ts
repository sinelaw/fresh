/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Diff Navigation Plugin
 *
 * Provides unified next/previous change commands that merge changes from all
 * available diff sources: git diff AND piece-tree saved-diff. This means a
 * single keybinding pair navigates both committed and unsaved changes.
 *
 * When only one source is available (e.g. file not tracked by git), it still
 * works using that source alone.
 */

// =============================================================================
// Types
// =============================================================================

interface DiffHunk {
  type: "added" | "modified" | "deleted";
  startLine: number; // 1-indexed
  lineCount: number;
}

/** Hunk shape published by live_diff.ts on `live_diff_hunks` view state. */
interface LiveDiffHunk {
  kind: "added" | "removed" | "modified";
  newStart: number; // 0-indexed
  newCount: number;
}

/** A jump target with a byte position for sorting/deduplication */
interface JumpTarget {
  bytePos: number;
  line: number; // 0-indexed, for scrollToLineCenter
}

// =============================================================================
// Collecting jump targets from all sources
// =============================================================================

async function collectTargets(bid: number): Promise<JumpTarget[]> {
  const targets: JumpTarget[] = [];

  // Source 1: git gutter hunks
  const hunks = editor.getViewState(bid, "git_gutter_hunks") as DiffHunk[] | null;
  if (hunks && hunks.length > 0) {
    for (const hunk of hunks) {
      const line = Math.max(0, hunk.startLine - 1); // 0-indexed
      const pos = await editor.getLineStartPosition(line);
      if (pos !== null) {
        targets.push({ bytePos: pos, line });
      }
    }
  }

  // Source 2: live-diff hunks (head/disk/branch comparison from live_diff.ts)
  const liveHunks = editor.getViewState(bid, "live_diff_hunks") as LiveDiffHunk[] | null;
  if (liveHunks && liveHunks.length > 0) {
    for (const hunk of liveHunks) {
      const line = Math.max(0, hunk.newStart);
      const pos = await editor.getLineStartPosition(line);
      if (pos !== null) {
        targets.push({ bytePos: pos, line });
      }
    }
  }

  // Source 3: saved-diff (unsaved changes)
  const diff = editor.getBufferSavedDiff(bid);
  if (diff && !diff.equal) {
    for (const [start, _end] of diff.byte_ranges) {
      // We don't know the line yet; resolve it lazily after dedup
      targets.push({ bytePos: start, line: -1 });
    }
  }

  if (targets.length === 0) return targets;

  // Sort by byte position
  targets.sort((a, b) => a.bytePos - b.bytePos);

  // Deduplicate: if two targets are on the same line, keep the first.
  // Resolve line numbers for saved-diff targets that still have line = -1.
  const deduped: JumpTarget[] = [];
  const seenLines = new Set<number>();

  for (const t of targets) {
    // Resolve line if unknown
    if (t.line === -1) {
      // Jump cursor temporarily to find the line, then restore.
      // Instead, use a simpler heuristic: find the line by checking
      // existing targets or using getLineStartPosition in reverse.
      // Actually, we can set cursor, read line, but that's side-effectful.
      // Simpler: just check if any existing target has a bytePos close enough.
      // For dedup, we check if any already-added target has same bytePos.
      let isDup = false;
      for (const existing of deduped) {
        if (Math.abs(existing.bytePos - t.bytePos) < 2) {
          isDup = true;
          break;
        }
      }
      if (isDup) continue;
      deduped.push(t);
    } else {
      if (seenLines.has(t.line)) continue;
      seenLines.add(t.line);
      // Also check if a saved-diff target at similar byte pos was already added
      let isDup = false;
      for (const existing of deduped) {
        if (existing.line === -1 && Math.abs(existing.bytePos - t.bytePos) < 2) {
          // Replace the unresolved one with this one (which has a known line)
          existing.line = t.line;
          isDup = true;
          break;
        }
      }
      if (isDup) continue;
      deduped.push(t);
    }
  }

  return deduped;
}

// =============================================================================
// Navigation
// =============================================================================

function goToTarget(bid: number, target: JumpTarget): void {
  if (target.line >= 0) {
    const splitId = editor.getActiveSplitId();
    editor.scrollToLineCenter(splitId, bid, target.line);
  }
  editor.setBufferCursor(bid, target.bytePos);
}

async function diff_nav_next(): Promise<void> {
  const bid = editor.getActiveBufferId();
  const targets = await collectTargets(bid);

  if (targets.length === 0) {
    editor.setStatus(editor.t("status.no_changes"));
    return;
  }

  const cursor = editor.getCursorPosition();
  let idx = targets.findIndex((t) => t.bytePos > cursor);
  let wrapped = false;
  if (idx === -1) {
    idx = 0;
    wrapped = true;
  }

  goToTarget(bid, targets[idx]);

  const msg = wrapped
    ? editor.t("status.change_wrapped", { n: String(idx + 1), total: String(targets.length) })
    : editor.t("status.change", { n: String(idx + 1), total: String(targets.length) });
  editor.setStatus(msg);
}
registerHandler("diff_nav_next", diff_nav_next);

async function diff_nav_prev(): Promise<void> {
  const bid = editor.getActiveBufferId();
  const targets = await collectTargets(bid);

  if (targets.length === 0) {
    editor.setStatus(editor.t("status.no_changes"));
    return;
  }

  const cursor = editor.getCursorPosition();
  let idx = -1;
  for (let i = targets.length - 1; i >= 0; i--) {
    if (targets[i].bytePos < cursor) {
      idx = i;
      break;
    }
  }
  let wrapped = false;
  if (idx === -1) {
    idx = targets.length - 1;
    wrapped = true;
  }

  goToTarget(bid, targets[idx]);

  const msg = wrapped
    ? editor.t("status.change_wrapped", { n: String(idx + 1), total: String(targets.length) })
    : editor.t("status.change", { n: String(idx + 1), total: String(targets.length) });
  editor.setStatus(msg);
}
registerHandler("diff_nav_prev", diff_nav_prev);

// =============================================================================
// Registration
// =============================================================================

editor.registerCommand(
  "%cmd.next_change",
  "%cmd.next_change_desc",
  "diff_nav_next",
  null
);

editor.registerCommand(
  "%cmd.prev_change",
  "%cmd.prev_change_desc",
  "diff_nav_prev",
  null
);

editor.debug("Diff Nav plugin loaded");
