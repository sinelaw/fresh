/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Live Diff Plugin
 *
 * Renders a unified-diff view directly inside the live editable buffer:
 *   - `-`/`+`/`~` indicators in the gutter for changed lines
 *   - virtual lines containing the OLD content rendered above edited lines
 *   - background highlight on added/modified new-side lines
 *
 * Target use case: a coding agent (or any background process) is modifying
 * the file on disk while the user watches.  `after_insert` / `after_delete`
 * fire when Fresh reloads the buffer from disk, so the diff updates live.
 *
 * The diff reference (left side) is selectable per buffer via the
 * command palette:
 *   - Live Diff: vs HEAD               — git HEAD revision (default)
 *   - Live Diff: vs Disk               — file content currently on disk
 *   - Live Diff: vs Branch...          — user-supplied git ref
 *   - Live Diff: vs Default Branch     — origin/HEAD or main/master
 *   - Live Diff: Toggle                — disable/enable for the active buffer
 *   - Live Diff: Refresh               — re-fetch reference and recompute
 *   - Live Diff: Set Default Mode...   — pick the default for new buffers
 */

// =============================================================================
// Constants
// =============================================================================

const NS_GUTTER = "live-diff";
const NS_VLINE = "live-diff-vlines";
const NS_OVERLAY = "live-diff-overlay";

// Lower priority than git_gutter (10) so live-diff loses if both are active
// on the same line — but in practice users will run one or the other.
const PRIORITY = 9;

// RGB defaults; the plan calls for theme keys, but the bundled themes don't
// define them yet, so we inline the colors here. Easy to swap to theme keys
// once the theme PR lands.
const COLORS = {
  added: [80, 250, 123] as [number, number, number],
  modified: [255, 184, 108] as [number, number, number],
  removed: [255, 85, 85] as [number, number, number],
};
const SYMBOLS = {
  added: "+",
  modified: "~",
  removed: "-",
};

// Coalesce edit bursts (agent paste, undo, editor reload) into one
// recompute. Token-bumped delay loop, mirrors git_log.ts's CURSOR_DEBOUNCE_MS.
const DEBOUNCE_MS = 75;

// Skip virtual-line rendering when either side is huge — line-by-line
// LCS would be too slow. Gutter glyphs still render via a degraded path.
const MAX_DIFF_LINES = 20_000;
// Soft cap on the LCS DP table; past this we stop computing virtual lines.
const MAX_DP_CELLS = 4_000_000;

// =============================================================================
// Types
// =============================================================================

type DiffMode =
  | { kind: "head" }
  | { kind: "disk" }
  | { kind: "branch"; ref: string };

type HunkKind = "added" | "removed" | "modified";

interface Hunk {
  kind: HunkKind;
  /** First changed new-side line (0-indexed). */
  newStart: number;
  /** Number of new-side lines (0 for pure deletion). */
  newCount: number;
  /** Old-side text, line by line, no trailing newline. */
  oldLines: string[];
}

interface BufferDiffState {
  bufferId: number;
  filePath: string;
  mode: DiffMode;
  /** Reference text. `null` while loading or when no reference is available. */
  oldText: string | null;
  /** Pre-split cached lines from `oldText` to skip resplit on every keystroke. */
  oldLines: string[];
  /** Most recent hunks, published to view state for diff_nav.ts. */
  hunks: Hunk[];
  /** True while a recompute is in flight. */
  updating: boolean;
  /** Token bumped on every scheduleRecompute; mismatched tokens are stale. */
  pendingToken: number;
  /** Plugin disabled for this buffer (Live Diff: Toggle). */
  disabled: boolean;
}

const states: Map<number, BufferDiffState> = new Map();

// =============================================================================
// Persistence helpers
// =============================================================================

function getDefaultMode(): DiffMode {
  const stored = editor.getGlobalState("live_diff.default_mode") as DiffMode | null;
  if (stored && (stored.kind === "head" || stored.kind === "disk" || stored.kind === "branch")) {
    return stored;
  }
  return { kind: "head" };
}

function setDefaultMode(mode: DiffMode): void {
  editor.setGlobalState("live_diff.default_mode", mode);
}

function getStoredMode(bufferId: number): DiffMode | null {
  const stored = editor.getViewState(bufferId, "live_diff.mode") as DiffMode | null;
  if (stored && (stored.kind === "head" || stored.kind === "disk" || stored.kind === "branch")) {
    return stored;
  }
  return null;
}

function storeMode(bufferId: number, mode: DiffMode): void {
  editor.setViewState(bufferId, "live_diff.mode", mode);
}

// =============================================================================
// Reference loading
// =============================================================================

function fileDir(filePath: string): string {
  const lastSlash = filePath.lastIndexOf("/");
  return lastSlash > 0 ? filePath.substring(0, lastSlash) : ".";
}

async function repoRelativePath(filePath: string): Promise<string | null> {
  const cwd = fileDir(filePath);
  const result = await editor.spawnProcess(
    "git", ["ls-files", "--full-name", "--", filePath], cwd,
  );
  if (result.exit_code !== 0) return null;
  const path = result.stdout.split("\n")[0]?.trim();
  return path && path.length > 0 ? path : null;
}

async function loadHeadRef(filePath: string): Promise<string | null> {
  const repoPath = await repoRelativePath(filePath);
  if (!repoPath) return null;
  const cwd = fileDir(filePath);
  const result = await editor.spawnProcess(
    "git", ["show", `HEAD:${repoPath}`], cwd,
  );
  return result.exit_code === 0 ? result.stdout : null;
}

async function loadBranchRef(filePath: string, ref: string): Promise<string | null> {
  const repoPath = await repoRelativePath(filePath);
  if (!repoPath) return null;
  const cwd = fileDir(filePath);
  const result = await editor.spawnProcess(
    "git", ["show", `${ref}:${repoPath}`], cwd,
  );
  return result.exit_code === 0 ? result.stdout : null;
}

function loadDiskRef(filePath: string): string | null {
  return editor.readFile(filePath);
}

async function resolveDefaultBranch(filePath: string): Promise<string> {
  const cwd = fileDir(filePath);
  const head = await editor.spawnProcess(
    "git", ["symbolic-ref", "--short", "refs/remotes/origin/HEAD"], cwd,
  );
  if (head.exit_code === 0) {
    const trimmed = head.stdout.trim();
    if (trimmed.startsWith("origin/")) return trimmed.substring("origin/".length);
    if (trimmed.length > 0) return trimmed;
  }
  const main = await editor.spawnProcess(
    "git", ["rev-parse", "--verify", "main"], cwd,
  );
  if (main.exit_code === 0) return "main";
  return "master";
}

async function loadReference(state: BufferDiffState): Promise<string | null> {
  switch (state.mode.kind) {
    case "head":
      return await loadHeadRef(state.filePath);
    case "disk":
      return loadDiskRef(state.filePath);
    case "branch":
      return await loadBranchRef(state.filePath, state.mode.ref);
  }
}

// =============================================================================
// Line diff (LCS, with prefix/suffix stripping for speed)
// =============================================================================

interface DiffOp {
  /** "=" equal, "-" delete (old only), "+" insert (new only). */
  op: "=" | "-" | "+";
  /** 0-indexed line in the old file (for "=" and "-"). */
  oldLine: number;
  /** 0-indexed line in the new file (for "=" and "+"). */
  newLine: number;
}

function splitLines(text: string): string[] {
  // Preserve empty trailing line semantics: "foo\n" -> ["foo"], "" -> [].
  if (text.length === 0) return [];
  const lines = text.split("\n");
  if (lines.length > 0 && lines[lines.length - 1] === "") {
    lines.pop();
  }
  return lines;
}

/**
 * Line-level LCS diff. Returns ops in old/new order. Bails (returns null)
 * when the DP table would exceed MAX_DP_CELLS — caller falls back to a
 * coarser representation.
 */
function lineDiff(oldLines: string[], newLines: string[]): DiffOp[] | null {
  let prefix = 0;
  const minLen = Math.min(oldLines.length, newLines.length);
  while (prefix < minLen && oldLines[prefix] === newLines[prefix]) prefix++;

  let oldEnd = oldLines.length;
  let newEnd = newLines.length;
  while (oldEnd > prefix && newEnd > prefix && oldLines[oldEnd - 1] === newLines[newEnd - 1]) {
    oldEnd--;
    newEnd--;
  }

  const ops: DiffOp[] = [];
  for (let i = 0; i < prefix; i++) {
    ops.push({ op: "=", oldLine: i, newLine: i });
  }

  const m = oldEnd - prefix;
  const n = newEnd - prefix;

  if (m === 0 && n === 0) {
    // Pure prefix; tail equal-block follows below.
  } else if (m === 0) {
    for (let j = 0; j < n; j++) {
      ops.push({ op: "+", oldLine: prefix, newLine: prefix + j });
    }
  } else if (n === 0) {
    for (let i = 0; i < m; i++) {
      ops.push({ op: "-", oldLine: prefix + i, newLine: prefix });
    }
  } else {
    if ((m + 1) * (n + 1) > MAX_DP_CELLS) return null;

    // dp[(i)*(n+1) + j] = LCS length of oldMid[0..i] vs newMid[0..j].
    // Plain Array — QuickJS doesn't expose typed arrays in this runtime.
    const stride = n + 1;
    const dp: number[] = new Array((m + 1) * stride).fill(0);
    for (let i = 1; i <= m; i++) {
      const oi = oldLines[prefix + i - 1];
      for (let j = 1; j <= n; j++) {
        if (oi === newLines[prefix + j - 1]) {
          dp[i * stride + j] = dp[(i - 1) * stride + (j - 1)] + 1;
        } else {
          const a = dp[(i - 1) * stride + j];
          const b = dp[i * stride + (j - 1)];
          dp[i * stride + j] = a >= b ? a : b;
        }
      }
    }

    // Backtrack — push ops in reverse, then reverse at the end of this block.
    const middle: DiffOp[] = [];
    let i = m;
    let j = n;
    while (i > 0 && j > 0) {
      if (oldLines[prefix + i - 1] === newLines[prefix + j - 1]) {
        middle.push({ op: "=", oldLine: prefix + i - 1, newLine: prefix + j - 1 });
        i--;
        j--;
      } else if (dp[(i - 1) * stride + j] >= dp[i * stride + (j - 1)]) {
        middle.push({ op: "-", oldLine: prefix + i - 1, newLine: prefix + j });
        i--;
      } else {
        middle.push({ op: "+", oldLine: prefix + i, newLine: prefix + j - 1 });
        j--;
      }
    }
    while (i > 0) {
      middle.push({ op: "-", oldLine: prefix + i - 1, newLine: prefix });
      i--;
    }
    while (j > 0) {
      middle.push({ op: "+", oldLine: prefix + i, newLine: prefix + j - 1 });
      j--;
    }
    middle.reverse();
    for (const m of middle) ops.push(m);
  }

  for (let i = 0; i < oldLines.length - oldEnd; i++) {
    ops.push({ op: "=", oldLine: oldEnd + i, newLine: newEnd + i });
  }

  return ops;
}

/**
 * Group a diff-op stream into hunks. Adjacent `-` and `+` runs collapse into
 * a single `modified` hunk so the old line renders directly above the new one.
 */
function opsToHunks(ops: DiffOp[]): Hunk[] {
  const hunks: Hunk[] = [];
  let i = 0;
  while (i < ops.length) {
    if (ops[i].op === "=") {
      i++;
      continue;
    }
    let dels = 0;
    let ins = 0;
    const oldLines: string[] = [];
    let firstNew = ops[i].newLine;
    while (i < ops.length && ops[i].op !== "=") {
      if (ops[i].op === "-") {
        dels++;
      } else {
        ins++;
      }
      i++;
    }
    // Walk back over the run we just consumed to capture old-side text and
    // the first new-side line, since op order may interleave.
    const start = i - (dels + ins);
    firstNew = ops[start].newLine;
    for (let k = start; k < i; k++) {
      const o = ops[k];
      if (o.op === "+") firstNew = Math.min(firstNew, o.newLine);
    }
    // We don't carry old-side text on DiffOp (memory), so look it up later.
    // Stash indices for now; the caller resolves text from `oldLines[]`.
    const kind: HunkKind = dels > 0 && ins > 0 ? "modified" : ins > 0 ? "added" : "removed";
    hunks.push({
      kind,
      newStart: firstNew,
      newCount: ins,
      // oldLines populated by the caller from the source array; placeholder:
      oldLines: [],
    });
    // Save indices so we can fill oldLines outside.
    (hunks[hunks.length - 1] as Hunk & { _oldStart?: number; _oldEnd?: number })._oldStart = ops[start].oldLine;
    (hunks[hunks.length - 1] as Hunk & { _oldStart?: number; _oldEnd?: number })._oldEnd = ops[start].oldLine + dels;
  }
  return hunks;
}

function fillOldLines(hunks: Hunk[], oldLines: string[]): void {
  for (const h of hunks) {
    const meta = h as Hunk & { _oldStart?: number; _oldEnd?: number };
    const s = meta._oldStart ?? 0;
    const e = meta._oldEnd ?? 0;
    h.oldLines = oldLines.slice(s, e);
    delete meta._oldStart;
    delete meta._oldEnd;
  }
}

// =============================================================================
// Rendering
// =============================================================================

function clearDecorations(bufferId: number): void {
  editor.clearLineIndicators(bufferId, NS_GUTTER);
  editor.clearVirtualTextNamespace(bufferId, NS_VLINE);
  editor.clearNamespace(bufferId, NS_OVERLAY);
}

async function renderHunks(state: BufferDiffState): Promise<void> {
  const bid = state.bufferId;
  clearDecorations(bid);

  // Group new-side lines per kind for batched setLineIndicators.
  const addedLines: number[] = [];
  const modifiedLines: number[] = [];
  const removedAnchors: number[] = [];

  for (const h of state.hunks) {
    if (h.kind === "removed") {
      // Anchor on the line that took the deletion's place.  If newStart
      // is past EOF (`getLineStartPosition` returns null), step back
      // until we find a real line.
      let anchor = h.newStart;
      while (anchor > 0) {
        const pos = await editor.getLineStartPosition(anchor);
        if (pos !== null) break;
        anchor--;
      }
      removedAnchors.push(anchor);
    } else if (h.kind === "added") {
      for (let i = 0; i < h.newCount; i++) addedLines.push(h.newStart + i);
    } else {
      for (let i = 0; i < h.newCount; i++) modifiedLines.push(h.newStart + i);
    }
  }

  if (addedLines.length > 0) {
    editor.setLineIndicators(
      bid, addedLines, NS_GUTTER, SYMBOLS.added,
      COLORS.added[0], COLORS.added[1], COLORS.added[2], PRIORITY,
    );
  }
  if (modifiedLines.length > 0) {
    editor.setLineIndicators(
      bid, modifiedLines, NS_GUTTER, SYMBOLS.modified,
      COLORS.modified[0], COLORS.modified[1], COLORS.modified[2], PRIORITY,
    );
  }
  if (removedAnchors.length > 0) {
    editor.setLineIndicators(
      bid, removedAnchors, NS_GUTTER, SYMBOLS.removed,
      COLORS.removed[0], COLORS.removed[1], COLORS.removed[2], PRIORITY,
    );
  }

  // Background highlights and virtual lines need byte positions, so fan
  // out per-hunk. Each hunk does at most O(oldLines + 1) API calls.
  for (const h of state.hunks) {
    if (h.kind === "added" || h.kind === "modified") {
      const start = await editor.getLineStartPosition(h.newStart);
      const end = await editor.getLineEndPosition(h.newStart + h.newCount - 1);
      if (start !== null && end !== null) {
        editor.addOverlay(bid, NS_OVERLAY, start, end, {
          bg: h.kind === "added" ? COLORS.added : COLORS.modified,
          extendToLineEnd: true,
        });
      }
    }

    if (h.oldLines.length === 0) continue;

    // Anchor: for removed/modified hunks, use the line that follows the
    // deletion on the new side. If newStart is past EOF (line index out
    // of range), step back to the last real line and anchor "below" so
    // the virtual lines appear after the existing tail.
    let anchorLine = h.newStart;
    let above = true;
    let anchor = await editor.getLineStartPosition(anchorLine);
    while (anchor === null && anchorLine > 0) {
      anchorLine--;
      above = false;
      anchor = await editor.getLineStartPosition(anchorLine);
    }
    if (anchor === null) continue;

    for (let i = 0; i < h.oldLines.length; i++) {
      editor.addVirtualLine(
        bid,
        anchor,
        "- " + h.oldLines[i],
        {
          fg: COLORS.removed,
          bg: [60, 20, 25] as [number, number, number],
        },
        above,
        NS_VLINE,
        i,
      );
    }
  }
}

// =============================================================================
// Recompute pipeline
// =============================================================================

async function recompute(bufferId: number): Promise<void> {
  const state = states.get(bufferId);
  if (!state) return;
  if (state.disabled) return;
  if (state.updating) return;

  state.updating = true;
  try {
    if (state.oldText === null) {
      const ref = await loadReference(state);
      if (ref === null) {
        // Reference fetch failed (file untracked, no repo, etc.).
        clearDecorations(bufferId);
        state.hunks = [];
        editor.setViewState(bufferId, "live_diff_hunks", null);
        return;
      }
      state.oldText = ref;
      state.oldLines = splitLines(ref);
    }

    const length = editor.getBufferLength(bufferId);
    const newText = await editor.getBufferText(bufferId, 0, length);
    const newLines = splitLines(newText);

    if (state.oldLines.length > MAX_DIFF_LINES || newLines.length > MAX_DIFF_LINES) {
      // Files too large for line-level diff. Don't render anything; surface
      // a status so the user knows why the gutter is empty.
      clearDecorations(bufferId);
      state.hunks = [];
      editor.setViewState(bufferId, "live_diff_hunks", null);
      editor.setStatus(editor.t("status.too_large"));
      return;
    }

    const ops = lineDiff(state.oldLines, newLines);
    if (ops === null) {
      clearDecorations(bufferId);
      state.hunks = [];
      editor.setViewState(bufferId, "live_diff_hunks", null);
      editor.setStatus(editor.t("status.too_large"));
      return;
    }

    const hunks = opsToHunks(ops);
    fillOldLines(hunks, state.oldLines);
    state.hunks = hunks;

    await renderHunks(state);

    editor.setViewState(bufferId, "live_diff_hunks", hunks);
  } finally {
    state.updating = false;
  }
}

async function scheduleRecompute(bufferId: number): Promise<void> {
  const state = states.get(bufferId);
  if (!state) return;
  const myToken = ++state.pendingToken;
  await editor.delay(DEBOUNCE_MS);
  if (myToken !== state.pendingToken) return;
  await recompute(bufferId);
}

// =============================================================================
// State helpers
// =============================================================================

function ensureState(bufferId: number): BufferDiffState | null {
  const existing = states.get(bufferId);
  if (existing) return existing;

  const info = editor.getBufferInfo(bufferId);
  if (!info) return null;
  if (info.is_virtual) return null;
  if (!info.path || info.path.length === 0) return null;

  const mode = getStoredMode(bufferId) ?? getDefaultMode();
  const state: BufferDiffState = {
    bufferId,
    filePath: info.path,
    mode,
    oldText: null,
    oldLines: [],
    hunks: [],
    updating: false,
    pendingToken: 0,
    disabled: false,
  };
  states.set(bufferId, state);
  return state;
}

function dropReference(state: BufferDiffState): void {
  state.oldText = null;
  state.oldLines = [];
}

async function setMode(bufferId: number, mode: DiffMode): Promise<void> {
  const state = ensureState(bufferId);
  if (!state) return;
  state.mode = mode;
  state.disabled = false;
  storeMode(bufferId, mode);
  dropReference(state);
  await recompute(bufferId);
}

// =============================================================================
// Commands
// =============================================================================

function live_diff_toggle(): void {
  const bid = editor.getActiveBufferId();
  const state = ensureState(bid);
  if (!state) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  state.disabled = !state.disabled;
  if (state.disabled) {
    clearDecorations(bid);
    state.hunks = [];
    editor.setViewState(bid, "live_diff_hunks", null);
    editor.setStatus(editor.t("status.disabled"));
  } else {
    editor.setStatus(editor.t("status.enabled"));
    recompute(bid).catch((e) => editor.error(`live-diff: ${e}`));
  }
}
registerHandler("live_diff_toggle", live_diff_toggle);

async function live_diff_vs_head(): Promise<void> {
  await setMode(editor.getActiveBufferId(), { kind: "head" });
  editor.setStatus(editor.t("status.mode_head"));
}
registerHandler("live_diff_vs_head", live_diff_vs_head);

async function live_diff_vs_disk(): Promise<void> {
  await setMode(editor.getActiveBufferId(), { kind: "disk" });
  editor.setStatus(editor.t("status.mode_disk"));
}
registerHandler("live_diff_vs_disk", live_diff_vs_disk);

async function live_diff_vs_branch(): Promise<void> {
  const last = (editor.getGlobalState("live_diff.last_branch") as string | null) ?? "main";
  const ref = await editor.prompt(editor.t("prompt.branch"), last);
  if (!ref || ref.trim().length === 0) return;
  const trimmed = ref.trim();
  editor.setGlobalState("live_diff.last_branch", trimmed);
  await setMode(editor.getActiveBufferId(), { kind: "branch", ref: trimmed });
  editor.setStatus(editor.t("status.mode_branch", { ref: trimmed }));
}
registerHandler("live_diff_vs_branch", live_diff_vs_branch);

async function live_diff_vs_default_branch(): Promise<void> {
  const bid = editor.getActiveBufferId();
  const path = editor.getBufferPath(bid);
  if (!path) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  const ref = await resolveDefaultBranch(path);
  await setMode(bid, { kind: "branch", ref });
  editor.setStatus(editor.t("status.mode_branch", { ref }));
}
registerHandler("live_diff_vs_default_branch", live_diff_vs_default_branch);

async function live_diff_refresh(): Promise<void> {
  const bid = editor.getActiveBufferId();
  const state = ensureState(bid);
  if (!state) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  dropReference(state);
  await recompute(bid);
  editor.setStatus(editor.t("status.refreshed"));
}
registerHandler("live_diff_refresh", live_diff_refresh);

async function live_diff_set_default(): Promise<void> {
  const choice = await editor.prompt(editor.t("prompt.default_mode"), "head");
  if (!choice) return;
  const c = choice.trim().toLowerCase();
  if (c === "head") setDefaultMode({ kind: "head" });
  else if (c === "disk") setDefaultMode({ kind: "disk" });
  else if (c.startsWith("branch:")) setDefaultMode({ kind: "branch", ref: c.substring("branch:".length) });
  else {
    editor.setStatus(editor.t("status.bad_default"));
    return;
  }
  editor.setStatus(editor.t("status.default_set"));
}
registerHandler("live_diff_set_default", live_diff_set_default);

// =============================================================================
// Event wiring
// =============================================================================

editor.on("after_file_open", (args) => {
  const state = ensureState(args.buffer_id);
  if (!state) return true;
  recompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  return true;
});

editor.on("buffer_activated", (args) => {
  const state = ensureState(args.buffer_id);
  if (!state) return true;
  // Indicators stick around across activations; only repaint if we never
  // ran a first pass (e.g. plugin loaded after the buffer opened).
  if (state.hunks.length === 0 && state.oldText === null) {
    recompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  }
  return true;
});

editor.on("after_insert", (args) => {
  if (!states.has(args.buffer_id)) return true;
  scheduleRecompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  return true;
});

editor.on("after_delete", (args) => {
  if (!states.has(args.buffer_id)) return true;
  scheduleRecompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  return true;
});

editor.on("after_file_save", (args) => {
  const state = states.get(args.buffer_id);
  if (!state) return true;
  // Save changes the file path (save-as) and invalidates the disk-mode reference.
  state.filePath = args.path;
  if (state.mode.kind === "disk") {
    dropReference(state);
  }
  recompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  return true;
});

editor.on("buffer_closed", (args) => {
  states.delete(args.buffer_id);
  return true;
});

// =============================================================================
// Command registration
// =============================================================================

editor.registerCommand("%cmd.toggle", "%cmd.toggle_desc", "live_diff_toggle", null);
editor.registerCommand("%cmd.vs_head", "%cmd.vs_head_desc", "live_diff_vs_head", null);
editor.registerCommand("%cmd.vs_disk", "%cmd.vs_disk_desc", "live_diff_vs_disk", null);
editor.registerCommand("%cmd.vs_branch", "%cmd.vs_branch_desc", "live_diff_vs_branch", null);
editor.registerCommand("%cmd.vs_default_branch", "%cmd.vs_default_branch_desc", "live_diff_vs_default_branch", null);
editor.registerCommand("%cmd.refresh", "%cmd.refresh_desc", "live_diff_refresh", null);
editor.registerCommand("%cmd.set_default", "%cmd.set_default_desc", "live_diff_set_default", null);

// =============================================================================
// Initialization
// =============================================================================

const initBid = editor.getActiveBufferId();
if (initBid !== 0) {
  const state = ensureState(initBid);
  if (state) {
    recompute(initBid).catch((e) => editor.error(`live-diff: ${e}`));
  }
}

editor.debug("Live Diff plugin loaded");
