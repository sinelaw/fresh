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

// Theme keys for backgrounds and virtual-line foregrounds. These are
// resolved at render time by the editor, so the diff colors track
// the active theme automatically.  All bundled themes provide
// `editor.diff_*_bg` (defaulted via serde) and `ui.file_status_*_fg`
// (falls through to `diagnostic.{info,warning,error}_fg` when the
// theme doesn't override).
const THEME = {
  addedBg: "editor.diff_add_bg",
  addedFg: "ui.file_status_added_fg",
  modifiedBg: "editor.diff_modify_bg",
  modifiedFg: "ui.file_status_modified_fg",
  removedBg: "editor.diff_remove_bg",
  removedFg: "ui.file_status_deleted_fg",
};

// `setLineIndicator` only accepts RGB triples (not theme keys), so the
// gutter glyphs use a fixed palette. Keep them muted so they read on
// both light and dark themes; the visual signal is the glyph shape.
const GUTTER_COLORS = {
  added: [80, 200, 120] as [number, number, number],
  modified: [220, 160, 90] as [number, number, number],
  removed: [220, 90, 90] as [number, number, number],
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
  /**
   * Per-buffer enable override. `null` means "follow the global toggle";
   * `true` forces live-diff on for this buffer regardless of the global
   * setting; `false` forces it off. Set by `Live Diff: Toggle (Buffer)`.
   */
  override: boolean | null;
  /**
   * Last buffer text we ran the diff against. `lines_changed` fires for
   * viewport scrolls too — comparing the text catches those cheaply and
   * skips the expensive clear-and-repaint that caused flicker on cursor
   * movement.
   */
  lastBufferText: string | null;
  /**
   * Stringified hunks from the previous successful render. When a
   * recompute produces an identical structure we skip the redraw to
   * avoid a clear-then-set flash even when the buffer itself did
   * change (e.g., the user typed inside an already-modified line).
   */
  lastHunksKey: string;
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

// Plugin is opt-in: `live_diff.global_enabled` defaults to false. Users
// flip it via "Live Diff: Toggle (Global)" or override per buffer with
// "Live Diff: Toggle (Buffer)".
function isGlobalEnabled(): boolean {
  return editor.getGlobalState("live_diff.global_enabled") === true;
}

function setGlobalEnabled(enabled: boolean): void {
  editor.setGlobalState("live_diff.global_enabled", enabled);
}

function getStoredOverride(bufferId: number): boolean | null {
  const stored = editor.getViewState(bufferId, "live_diff.override");
  if (stored === true || stored === false) return stored;
  return null;
}

function storeOverride(bufferId: number, override: boolean | null): void {
  editor.setViewState(bufferId, "live_diff.override", override);
}

function isEnabledForBuffer(state: BufferDiffState): boolean {
  if (state.override !== null) return state.override;
  return isGlobalEnabled();
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

/**
 * Compute byte offsets of every line start in the buffer (and one
 * extra entry past the last line) so renderHunks can map line indices
 * to byte ranges synchronously, without awaiting `getLineStartPosition`
 * per line.
 *
 * `getLineStartPosition` is async and yields back to the editor
 * event loop on every call. With one await per overlay we add, the
 * editor renders frames mid-render and the user sees green stripes
 * fill in one line at a time. Computing locally from the buffer text
 * keeps the whole render in a single JS turn → instant repaint.
 */
function computeLineByteStarts(text: string): number[] {
  const starts = [0];
  let pos = 0;
  for (let i = 0; i < text.length; i++) {
    pos += editor.utf8ByteLength(text[i]);
    if (text[i] === "\n") starts.push(pos);
  }
  return starts;
}

function renderHunks(state: BufferDiffState, newText: string): void {
  const bid = state.bufferId;
  clearDecorations(bid);

  const lineStarts = computeLineByteStarts(newText);
  const totalBytes = lineStarts[lineStarts.length - 1] || newText.length;
  // For line N, lineStarts[N] = byte of first char on line N. lineEnd
  // before the trailing newline = lineStarts[N+1] - 1 (when a newline
  // follows) or totalBytes when N is the last line. Empty/last-line
  // edge cases default to lineStarts[N].
  const lineEndExclusive = (line: number): number => {
    if (line + 1 < lineStarts.length) return lineStarts[line + 1] - 1;
    return totalBytes;
  };
  const lineCount = lineStarts.length;

  // Group new-side lines per kind for batched setLineIndicators.
  const addedLines: number[] = [];
  const modifiedLines: number[] = [];
  const removedAnchors: number[] = [];

  for (const h of state.hunks) {
    if (h.kind === "removed") {
      // Anchor on the line that took the deletion's place. If newStart
      // is past EOF, step back to the last real line.
      let anchor = h.newStart;
      if (anchor >= lineCount) anchor = Math.max(0, lineCount - 1);
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
      GUTTER_COLORS.added[0], GUTTER_COLORS.added[1], GUTTER_COLORS.added[2], PRIORITY,
    );
  }
  if (modifiedLines.length > 0) {
    editor.setLineIndicators(
      bid, modifiedLines, NS_GUTTER, SYMBOLS.modified,
      GUTTER_COLORS.modified[0], GUTTER_COLORS.modified[1], GUTTER_COLORS.modified[2], PRIORITY,
    );
  }
  if (removedAnchors.length > 0) {
    editor.setLineIndicators(
      bid, removedAnchors, NS_GUTTER, SYMBOLS.removed,
      GUTTER_COLORS.removed[0], GUTTER_COLORS.removed[1], GUTTER_COLORS.removed[2], PRIORITY,
    );
  }

  // Background highlights and virtual lines, all sync now.
  for (const h of state.hunks) {
    if (h.kind === "added" || h.kind === "modified") {
      const bg = h.kind === "added" ? THEME.addedBg : THEME.modifiedBg;
      for (let i = 0; i < h.newCount; i++) {
        const line = h.newStart + i;
        if (line >= lineCount) break;
        const start = lineStarts[line];
        const end = lineEndExclusive(line);
        editor.addOverlay(bid, NS_OVERLAY, start, end, {
          bg,
          underline: false,
          bold: false,
          italic: false,
          strikethrough: false,
          extendToLineEnd: true,
        });
      }
    }

    if (h.oldLines.length === 0) continue;

    // Anchor: line that follows the deletion on the new side. If past
    // EOF, anchor on the last real line and place "below".
    let anchorLine = h.newStart;
    let above = true;
    if (anchorLine >= lineCount) {
      anchorLine = Math.max(0, lineCount - 1);
      above = false;
    }
    const anchor = lineStarts[anchorLine];

    for (let i = 0; i < h.oldLines.length; i++) {
      // No "- " prefix — the red bg/fg is the visual signal, and the user
      // prefers any "-" indicator to live in the gutter rather than
      // inside the buffer content.
      editor.addVirtualLine(
        bid,
        anchor,
        h.oldLines[i],
        {
          fg: THEME.removedFg,
          bg: THEME.removedBg,
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
  if (!isEnabledForBuffer(state)) return;
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

    // Skip 1: same buffer text as last recompute. `lines_changed` fires
    // on viewport scrolls (cursor up/down past the visible area), and
    // re-clearing then re-painting the same decorations causes a
    // visible flash on the highlighted lines. The string comparison is
    // microseconds for typical source files; we only fall through when
    // the buffer actually changed.
    if (state.lastBufferText === newText) {
      return;
    }
    state.lastBufferText = newText;

    const newLines = splitLines(newText);

    if (state.oldLines.length > MAX_DIFF_LINES || newLines.length > MAX_DIFF_LINES) {
      // Files too large for line-level diff. Don't render anything; surface
      // a status so the user knows why the gutter is empty.
      clearDecorations(bufferId);
      state.hunks = [];
      state.lastHunksKey = "";
      editor.setViewState(bufferId, "live_diff_hunks", null);
      editor.setStatus(editor.t("status.too_large"));
      return;
    }

    const ops = lineDiff(state.oldLines, newLines);
    if (ops === null) {
      clearDecorations(bufferId);
      state.hunks = [];
      state.lastHunksKey = "";
      editor.setViewState(bufferId, "live_diff_hunks", null);
      editor.setStatus(editor.t("status.too_large"));
      return;
    }

    const hunks = opsToHunks(ops);
    fillOldLines(hunks, state.oldLines);

    // Skip 2: same hunks as last render. The user can edit inside an
    // already-flagged region without changing line counts (e.g., typing
    // mid-word on a modified line). Without this guard we still
    // clear+repaint each keystroke, producing visible flicker.
    const hunksKey = JSON.stringify(hunks);
    if (hunksKey === state.lastHunksKey) {
      state.hunks = hunks;
      return;
    }
    state.hunks = hunks;
    state.lastHunksKey = hunksKey;

    renderHunks(state, newText);

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
    override: getStoredOverride(bufferId),
    lastBufferText: null,
    lastHunksKey: "",
  };
  states.set(bufferId, state);
  return state;
}

function dropReference(state: BufferDiffState): void {
  state.oldText = null;
  state.oldLines = [];
  // Force the next recompute to repaint even if the buffer itself
  // hasn't changed (mode swap rebuilds against a new reference).
  state.lastBufferText = null;
  state.lastHunksKey = "";
}

async function setMode(bufferId: number, mode: DiffMode): Promise<void> {
  const state = ensureState(bufferId);
  if (!state) return;
  state.mode = mode;
  // Choosing a comparison reference is a clear "I want to see the diff"
  // signal — force-on for this buffer so the command works even when the
  // global toggle is off.
  state.override = true;
  storeOverride(bufferId, true);
  storeMode(bufferId, mode);
  dropReference(state);
  await recompute(bufferId);
}

// =============================================================================
// Commands
// =============================================================================

/**
 * Reflect the current effective enabled state for a buffer in the
 * editor: paint or clear decorations and (re)compute as needed.
 * Called from both toggle commands.
 */
function syncBufferToEnabledState(state: BufferDiffState): void {
  if (isEnabledForBuffer(state)) {
    recompute(state.bufferId).catch((e) => editor.error(`live-diff: ${e}`));
  } else {
    clearDecorations(state.bufferId);
    state.hunks = [];
    state.lastBufferText = null;
    state.lastHunksKey = "";
    editor.setViewState(state.bufferId, "live_diff_hunks", null);
  }
}

/**
 * Toggle the per-buffer override for the active buffer. Sets the
 * override to the opposite of the buffer's current effective state, so
 * one invocation always flips what the user sees on screen.
 */
function live_diff_toggle_buffer(): void {
  const bid = editor.getActiveBufferId();
  const state = ensureState(bid);
  if (!state) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  const newEnabled = !isEnabledForBuffer(state);
  state.override = newEnabled;
  storeOverride(bid, newEnabled);
  syncBufferToEnabledState(state);
  editor.setStatus(editor.t(newEnabled ? "status.buffer_enabled" : "status.buffer_disabled"));
}
registerHandler("live_diff_toggle_buffer", live_diff_toggle_buffer);

/**
 * Toggle the global enable flag. Refreshes every tracked buffer that
 * doesn't have its own override set so the change is visible immediately.
 */
function live_diff_toggle_global(): void {
  const newEnabled = !isGlobalEnabled();
  setGlobalEnabled(newEnabled);
  for (const state of states.values()) {
    if (state.override === null) {
      syncBufferToEnabledState(state);
    }
  }
  editor.setStatus(editor.t(newEnabled ? "status.global_enabled" : "status.global_disabled"));
}
registerHandler("live_diff_toggle_global", live_diff_toggle_global);

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

// `lines_changed` fires on every visible-line redraw, including the ones
// driven by Fresh's external-file-watch reload (which doesn't go through
// after_insert/after_delete). This is the hook that makes the live-diff
// view update when a coding agent rewrites the file on disk.
editor.on("lines_changed", (args) => {
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

editor.registerCommand("%cmd.toggle_global", "%cmd.toggle_global_desc", "live_diff_toggle_global", null);
editor.registerCommand("%cmd.toggle_buffer", "%cmd.toggle_buffer_desc", "live_diff_toggle_buffer", null);
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
