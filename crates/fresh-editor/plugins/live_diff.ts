/// <reference path="./lib/fresh.d.ts" />

import {
  computeWordDiff,
  type WordDiff,
  type WordRange,
  utf8ByteLength,
} from "./lib/word_diff.ts";

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
const NS_SCROLL = "live-diff-scroll";

// Lower priority than git_gutter (10) so live-diff loses if both are active
// on the same line — but in practice users will run one or the other.
const PRIORITY = 9;

// Theme keys for backgrounds and "on top of bg" foregrounds. These
// are resolved at render time by the editor, so the diff colors track
// the active theme automatically. The `editor.diff_*_collision_fg`
// keys pair with `fgOnCollisionOnly: true` below: themes that define
// them get a contrasting fg painted only on cells whose syntax fg
// happens to match the diff bg (e.g. ANSI Green-on-Green); every
// other cell keeps its syntax colour.
const THEME = {
  addedBg: "editor.diff_add_bg",
  addedFg: "editor.diff_add_collision_fg",
  modifiedBg: "editor.diff_modify_bg",
  modifiedFg: "editor.diff_modify_collision_fg",
  removedBg: "editor.diff_remove_bg",
  removedFg: "editor.diff_remove_collision_fg",
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

// The line diff itself runs natively in the host (`diffAgainstBaseline`
// against a registered baseline — a patience diff that never refuses an
// input), so no size cap guards the *diff*. The caps below guard
// *rendering* cost only, and each one degrades detail locally instead
// of clearing the view:
//
//   full      — everything: virtual deletion lines, word-level
//               refinement, per-line backgrounds, gutter, scrollbar.
//   no-vlines — skip virtual deletion lines + word refinement;
//               backgrounds, gutter and scrollbar still render.
//   outline   — gutter indicators and scrollbar markers only.
type DetailLevel = "full" | "no-vlines" | "outline";

// Skip virtual deletion lines (and word refinement) when the diff would
// need more than this many of them.
const MAX_VIRTUAL_LINES = 2_000;
// Skip per-line background overlays too when more new-side lines than
// this changed; gutter + scrollbar only.
const MAX_OVERLAY_LINES = 20_000;
// Global budget for `refineHunks`' per-pair char/word LCS work in a
// single recompute, measured in DP cells (~oldLen*newLen per pair).
// Pairs past the budget keep plain modified rendering.
const MAX_REFINE_CELLS = 10_000_000;

// Similarity (Sørensen–Dice over character LCS) above which a 1:1
// modified pair is rendered as "modified" (bg-only highlight on the
// new line, no deletion virtual line). Below this we split the pair
// into a `removed` (virtual deletion line) + `added` (bg-highlighted)
// hunk pair so the change reads as a rewrite, not an in-place edit.
// Tunable at runtime via `editor.getPluginApi("live-diff").setSimilarityThreshold(x)`.
let similarityThreshold = 0.95;
// Bail out of char-LCS on huge lines; cost is O(m * n).
const MAX_LINE_LCS_CHARS = 2000;
// Bail out of shared word alignment when either side has more tokens than
// this; the dynamic program is O(m * n) in tokens.
const MAX_WORD_TOKENS = 1000;

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
  /** Word-level diff results, one entry per new-side line in this
   * hunk. Set by `refineHunks` on 1:1-paired hunks — `modified` hunks
   * above the similarity threshold and `added` hunks from
   * low-similarity splits — to bold + underline the actually-changed
   * words on the new line. `undefined` for unrefined hunks. */
  wordRanges?: WordRange[][];
  /** Old-side word ranges, one entry per `oldLines` line. Set on
   * `removed` hunks emitted by `refineHunks` for 1:1 pairs where
   * words were dropped/changed; passed to addVirtualLine as
   * `textOverlays` so the deletion virtual line bolds + underlines
   * the actually-removed words. */
  oldWordRanges?: WordRange[][];
}

interface BufferDiffState {
  bufferId: number;
  filePath: string;
  mode: DiffMode;
  /**
   * Host-side baseline id for the current mode (`registerDiffBaseline`).
   * `null` before the first successful registration and after a mode
   * change. The reference content lives host-side; this plugin only
   * ever fetches the old-side lines it renders.
   */
  baselineId: number | null;
  /** Most recent hunks, published to view state for diff_nav.ts. */
  hunks: Hunk[];
  /** True while a recompute is in flight. */
  updating: boolean;
  /**
   * Set when a recompute is requested while one is already in flight for
   * this buffer. The running pass re-runs once more when it finishes so the
   * request isn't lost (a dropped post-commit refresh left a stale diff on
   * screen forever — the #2503 `focus_gained`/reflog path).
   */
  rerunRequested: boolean;
  /**
   * Request to refresh the host-side baseline on the next recompute pass
   * (a HEAD move: commit, checkout, reset, merge — or a save in disk
   * mode). Consumed *inside* the recompute mutex so a concurrent pass
   * never observes a half-refreshed reference — refreshing directly from
   * an event handler would race an in-flight recompute the same way the
   * old in-plugin reference cache did.
   */
  reloadRef: boolean;
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
  /**
   * Detail level of the previous render. The "simplified view" status
   * is emitted only on a transition into a degraded level, so typing
   * inside a huge diff doesn't re-announce it on every recompute.
   */
  lastDetail: DetailLevel;
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

/** Baseline registration parameters for a diff mode. */
function baselineParams(mode: DiffMode): { kind: string; gitRef: string | null } {
  switch (mode.kind) {
    case "head":
      return { kind: "gitRef", gitRef: "HEAD" };
    case "disk":
      return { kind: "disk", gitRef: null };
    case "branch":
      return { kind: "gitRef", gitRef: mode.ref };
  }
}

/**
 * Ensure a host-side baseline is registered for the buffer's current
 * mode. Returns the baseline id, or `null` when no reference exists
 * (file untracked, no repo, no file path) — the same cases the old
 * in-plugin `git show` loader reported as `null`.
 */
async function ensureBaseline(state: BufferDiffState): Promise<number | null> {
  if (state.baselineId !== null) return state.baselineId;
  const params = baselineParams(state.mode);
  try {
    state.baselineId = await editor.registerDiffBaseline(
      state.bufferId,
      params.kind,
      params.gitRef,
    );
  } catch (_e) {
    state.baselineId = null;
  }
  return state.baselineId;
}

/** Release the buffer's baseline (mode change, save-as). */
function releaseBaseline(state: BufferDiffState): void {
  if (state.baselineId !== null) {
    editor.releaseDiffBaseline(state.baselineId);
    state.baselineId = null;
  }
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

// =============================================================================
// Line diff (native, host-side via diffAgainstBaseline)
// =============================================================================

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
 * Map the host's line-range hunks onto this plugin's `Hunk` shape.
 * `oldGroups` holds the old-side line contents for hunks with
 * `oldCount > 0`, in hunk order — fetched in one batched
 * `getBaselineLines` call at full detail, or empty when degraded
 * rendering skips old-side text entirely.
 */
function hostHunksToHunks(raw: LineDiffHunk[], oldGroups: string[][]): Hunk[] {
  const hunks: Hunk[] = [];
  let group = 0;
  for (const h of raw) {
    const kind: HunkKind =
      h.oldCount > 0 && h.newCount > 0
        ? "modified"
        : h.newCount > 0
          ? "added"
          : "removed";
    hunks.push({
      kind,
      newStart: h.newStart,
      newCount: h.newCount,
      oldLines: h.oldCount > 0 ? (oldGroups[group++] ?? []) : [],
    });
  }
  return hunks;
}

// =============================================================================
// Similarity + word-level diff
// =============================================================================

/**
 * Sørensen–Dice-style similarity ratio over a character LCS:
 *
 *   ratio = 2 * |LCS(a, b)| / (|a| + |b|)
 *
 * Range `0.0..1.0`. Empty / empty is `1.0`; either-side-empty is `0.0`.
 * Both sides are stripped of their common prefix and suffix first so
 * "abcdef" vs "abcXYZdef" pays only for the middle DP table.
 */
function lineSimilarity(a: string, b: string): number {
  if (a.length === 0 && b.length === 0) return 1.0;
  if (a.length === 0 || b.length === 0) return 0.0;
  if (a.length > MAX_LINE_LCS_CHARS || b.length > MAX_LINE_LCS_CHARS) {
    // Quadratic char LCS is too expensive on huge lines (minified
    // JS, base64 blobs). Treat as different so we don't stall the
    // render; the caller falls back to "split into removed+added".
    return 0.0;
  }
  let prefix = 0;
  const minLen = Math.min(a.length, b.length);
  while (prefix < minLen && a[prefix] === b[prefix]) prefix++;
  let aEnd = a.length;
  let bEnd = b.length;
  while (aEnd > prefix && bEnd > prefix && a[aEnd - 1] === b[bEnd - 1]) {
    aEnd--;
    bEnd--;
  }
  const equal = prefix + (a.length - aEnd);
  const m = aEnd - prefix;
  const n = bEnd - prefix;
  if (m === 0 || n === 0) {
    return (2 * equal) / (a.length + b.length);
  }
  const stride = n + 1;
  const dp: number[] = new Array((m + 1) * stride).fill(0);
  for (let i = 1; i <= m; i++) {
    const ai = a[prefix + i - 1];
    for (let j = 1; j <= n; j++) {
      if (ai === b[prefix + j - 1]) {
        dp[i * stride + j] = dp[(i - 1) * stride + (j - 1)] + 1;
      } else {
        const x = dp[(i - 1) * stride + j];
        const y = dp[i * stride + (j - 1)];
        dp[i * stride + j] = x >= y ? x : y;
      }
    }
  }
  const middleLcs = dp[m * stride + n];
  return (2 * (equal + middleLcs)) / (a.length + b.length);
}

/**
 * Live Diff ignores whitespace-only edits in its word emphasis and preserves
 * its bounded fallback for generated lines, while sharing the actual token
 * alignment and UTF-8 range calculation with Git Log.
 */
function computeLiveWordDiff(oldText: string, newText: string): WordDiff {
  return computeWordDiff(oldText, newText, {
    maxTokensPerSide: MAX_WORD_TOKENS,
    includeWhitespace: false,
    highlightAllOnLimit: true,
    groupWhitespace: true,
  })!;
}

/**
 * Post-process `hostHunksToHunks` output: split low-similarity 1:1
 * `modified` hunks into separate `removed` (virtual deletion line) +
 * `added` (bg-highlighted) hunks. High-similarity pairs stay as
 * `modified` and gain a `wordRanges` entry that drives the bold +
 * underline word-level overlay on the new line. When a high-similarity
 * pair also has *removed* (or otherwise unmatched) old-side words,
 * we additionally emit a `removed` hunk carrying the old line plus
 * `oldWordRanges` so the deletion virtual line shows the user which
 * words went away — not just that *something* did. Low-similarity
 * splits get the same word-level treatment on both halves of the
 * pair, so even a rewrite-style change calls out the words it shares
 * (and doesn't) with the old line.
 *
 * Hunks that don't have a 1:1 mapping (e.g. 3 old lines becoming 2
 * new lines) keep their original shape — the pairing is ambiguous,
 * and forcing a rewrite-style split would just create misleading
 * "removed" lines.
 *
 * Total char-LCS work across all pairs is bounded by MAX_REFINE_CELLS
 * per call: a large drift can hold hundreds of 1:1 pairs, and at up to
 * ~4M DP cells per pair the refinement — not the line diff — would
 * become the slow part. Hunks past the budget pass through unrefined
 * (plain `modified` rendering, no word underlines).
 */
function refineHunks(hunks: Hunk[], newLines: string[]): Hunk[] {
  const out: Hunk[] = [];
  let cellBudget = MAX_REFINE_CELLS;
  for (const h of hunks) {
    if (h.kind !== "modified" || h.oldLines.length !== h.newCount) {
      out.push(h);
      continue;
    }
    // Upper bound on this hunk's similarity + word-diff DP work.
    let hunkCost = 0;
    for (let i = 0; i < h.newCount; i++) {
      const oldLen = h.oldLines[i].length;
      const newLen = (newLines[h.newStart + i] ?? "").length;
      hunkCost += oldLen * newLen + 1;
    }
    if (hunkCost > cellBudget) {
      out.push(h);
      continue;
    }
    cellBudget -= hunkCost;
    for (let i = 0; i < h.newCount; i++) {
      const oldLine = h.oldLines[i];
      const newLine = newLines[h.newStart + i] ?? "";
      const sim = lineSimilarity(oldLine, newLine);
      if (sim >= similarityThreshold) {
        const wd = computeLiveWordDiff(oldLine, newLine);
        // Always emit the in-place modified hunk (drives the new-line
        // bg highlight + new-side word-diff). When old-side has
        // unmatched non-WS tokens, also emit a deletion line above
        // it carrying old-side word ranges so the user can see which
        // words were removed/replaced.
        if (wd.oldRanges.length > 0) {
          out.push({
            kind: "removed",
            newStart: h.newStart + i,
            newCount: 0,
            oldLines: [oldLine],
            oldWordRanges: [wd.oldRanges],
          });
        }
        out.push({
          kind: "modified",
          newStart: h.newStart + i,
          newCount: 1,
          oldLines: [],
          wordRanges: [wd.newRanges],
        });
      } else {
        // Low-similarity rewrite: the pair splits into a deletion
        // virtual line + a bg-highlighted added line. The word-level
        // diff still runs so the pair calls out *which* words were
        // removed/added — tokens common to both lines stay unmarked
        // (issue #1949).
        const wd = computeLiveWordDiff(oldLine, newLine);
        out.push({
          kind: "removed",
          newStart: h.newStart + i,
          newCount: 0,
          oldLines: [oldLine],
          ...(wd.oldRanges.length > 0
            ? { oldWordRanges: [wd.oldRanges] }
            : {}),
        });
        out.push({
          kind: "added",
          newStart: h.newStart + i,
          newCount: 1,
          oldLines: [],
          ...(wd.newRanges.length > 0 ? { wordRanges: [wd.newRanges] } : {}),
        });
      }
    }
  }
  return out;
}

// =============================================================================
// Rendering
// =============================================================================

function clearDecorations(bufferId: number): void {
  editor.clearLineIndicators(bufferId, NS_GUTTER);
  editor.clearVirtualTextNamespace(bufferId, NS_VLINE);
  editor.clearNamespace(bufferId, NS_OVERLAY);
  editor.clearScrollbarMarkers(bufferId, NS_SCROLL);
}

/**
 * Compute byte offsets of every line start in the buffer (one entry per
 * line, plus one past-the-end entry) so renderHunks can map line indices
 * to byte ranges synchronously, without awaiting `getLineStartPosition`
 * per line.
 *
 * `getLineStartPosition` is async and yields back to the editor event
 * loop on every call. With one await per overlay we add, the editor
 * renders frames mid-render and the user sees green stripes fill in one
 * line at a time. Computing locally from the buffer text keeps the
 * whole render in a single JS turn → instant repaint.
 *
 * Uses the shared `utf8ByteLength` once per *whole* line. Computing it per
 * character would be incorrect because `text[i]` splits a surrogate
 * pair into invalid half-code-units; passing whole lines is safe —
 * `splitLines` always returns valid Unicode strings.
 */
function computeLineByteStarts(lines: string[]): number[] {
  const starts: number[] = new Array(lines.length + 1);
  let pos = 0;
  starts[0] = 0;
  for (let i = 0; i < lines.length; i++) {
    pos += utf8ByteLength(lines[i]) + 1; // +1 for the trailing newline
    starts[i + 1] = pos;
  }
  return starts;
}

function renderHunks(state: BufferDiffState, newLines: string[], detail: DetailLevel): void {
  const bid = state.bufferId;
  clearDecorations(bid);

  const lineStarts = computeLineByteStarts(newLines);
  const totalBytes = lineStarts[lineStarts.length - 1] || 0;
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
  // `removed` hunks have no new-side line they belong on — their
  // indicator rides directly on the virtual deletion line itself
  // via `addVirtualLine`'s `gutterGlyph`, so it sits next to the
  // deleted content instead of on the source line that happens to
  // follow it.
  const addedLines: number[] = [];
  const modifiedLines: number[] = [];

  for (const h of state.hunks) {
    if (h.kind === "added") {
      for (let i = 0; i < h.newCount; i++) addedLines.push(h.newStart + i);
    } else if (h.kind === "modified") {
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

  // Scrollbar marks, one per hunk — so unsaved changes elsewhere in the file
  // are visible without scrolling to find them.
  //
  // Whole-namespace replace (not the range-scoped form) is right here: unlike
  // a `lines_changed` producer, this pass recomputes the diff for the *entire*
  // buffer every time, so the hunk list it holds is complete and authoritative.
  // Sending it in one command also means the marks swap atomically.
  //
  // Added/modified hunks span their new-side lines, so they carry an `end` and
  // paint a proportional streak. A removed hunk has no new-side line of its
  // own (`newCount === 0`) — its content is gone — so it marks the seam where
  // the deletion happened, matching where the virtual deletion line renders.
  // Colours reuse the gutter palette deliberately: a hunk's gutter glyph and
  // its scrollbar mark are then the same colour.
  const scrollMarkers = [];
  for (const h of state.hunks) {
    if (h.newStart >= lineCount) continue;
    const color = h.kind === "added"
      ? GUTTER_COLORS.added
      : h.kind === "modified"
        ? GUTTER_COLORS.modified
        : GUTTER_COLORS.removed;
    const start = lineStarts[h.newStart];
    // Byte offsets, not line numbers: exact at any file size, and the editor
    // anchors them so marks ride subsequent edits until the next recompute.
    const marker: { position: number; end?: number; color: [number, number, number]; priority: number } = {
      position: start,
      color,
      priority: PRIORITY,
    };
    if (h.newCount > 0) {
      const lastLine = Math.min(h.newStart + h.newCount - 1, lineCount - 1);
      marker.end = lineEndExclusive(lastLine);
    }
    scrollMarkers.push(marker);
  }
  editor.setScrollbarMarkers(bid, NS_SCROLL, scrollMarkers);

  // Background highlights and virtual lines, all sync now. At
  // "outline" detail both are skipped — the gutter indicators and
  // scrollbar markers above already put the change on screen.
  if (detail === "outline") return;
  for (const h of state.hunks) {
    if (h.kind === "added" || h.kind === "modified") {
      const bg = h.kind === "added" ? THEME.addedBg : THEME.modifiedBg;
      // `fgOnCollisionOnly` keeps syntax highlighting intact on the
      // rest of the line and only paints `fg` where a token's colour
      // would otherwise be invisible against the diff bg.
      const fg = h.kind === "added" ? THEME.addedFg : THEME.modifiedFg;
      for (let i = 0; i < h.newCount; i++) {
        const line = h.newStart + i;
        if (line >= lineCount) break;
        const start = lineStarts[line];
        let end = lineEndExclusive(line);
        // Empty source lines have lineEndExclusive == lineStart. A
        // zero-width overlay never enters the renderer's byte sweep
        // (the chars iter has no chars to advance over), so the
        // extend_to_line_end fill never fires for empty lines and the
        // user sees a "skipped" row in the middle of an added block.
        // Bump the end by one so the range covers the trailing
        // newline byte; the sweep advances at the next non-empty line
        // and catches our overlay.
        if (end <= start) end = start + 1;
        editor.addOverlay(bid, NS_OVERLAY, start, end, {
          bg,
          fg,
          fgOnCollisionOnly: true,
          underline: false,
          bold: false,
          italic: false,
          strikethrough: false,
          extendToLineEnd: true,
        });
      }

      // Word-level diff: bold + underline the changed words on the
      // new-side line of a refined 1:1 pair (a high-similarity
      // `modified` hunk or the `added` half of a low-similarity split).
      // `wordRanges` is set only by `refineHunks` and uses byte
      // offsets relative to each new-side line's start, so we add the
      // line's own start byte before passing to `addOverlay`.
      if (h.wordRanges && detail === "full") {
        for (let i = 0; i < h.newCount; i++) {
          const line = h.newStart + i;
          if (line >= lineCount) break;
          const lineByteStart = lineStarts[line];
          const ranges = h.wordRanges[i];
          if (!ranges) continue;
          for (const r of ranges) {
            editor.addOverlay(
              bid,
              NS_OVERLAY,
              lineByteStart + r.start,
              lineByteStart + r.end,
              {
                bold: true,
                underline: true,
                italic: false,
                strikethrough: false,
                extendToLineEnd: false,
              },
            );
          }
        }
      }
    }

    if (h.oldLines.length === 0 || detail !== "full") continue;

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
      // No "- " prefix in the line text — the indicator goes in the
      // gutter via `gutterGlyph` so it sits next to the deletion
      // line itself, not on the source line that follows it.
      // When `oldWordRanges` is set (from a refined 1:1 pair), bold +
      // underline the actually-removed words so the user can see
      // *what* changed, not just that something did.
      const wordRanges = h.oldWordRanges?.[i];
      const textOverlays = wordRanges
        ? wordRanges.map((r) => ({
            start: r.start,
            end: r.end,
            bold: true,
            underline: true,
          }))
        : [];
      editor.addVirtualLine(
        bid,
        anchor,
        h.oldLines[i],
        {
          fg: THEME.removedFg,
          bg: THEME.removedBg,
          gutterGlyph: SYMBOLS.removed,
          gutterColor: GUTTER_COLORS.removed,
          textOverlays,
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
  // Serialize recomputes per buffer. A recompute suspends at its `await`
  // points (baseline registration, buffer-text fetch, host diff). A second
  // trigger that arrives meanwhile must neither start a concurrent pass
  // (it would race on the shared baseline state and render a bogus
  // "everything added" diff) nor be silently dropped (a dropped post-commit refresh left
  // the stale diff on screen until the external test timeout — #2503's
  // `focus_gained`/reflog path). Coalesce: mark that another pass is needed
  // and let the in-flight one run it before it releases the mutex.
  if (state.updating) {
    state.rerunRequested = true;
    return;
  }

  state.updating = true;
  try {
    do {
      state.rerunRequested = false;
      // Consume a reference-reload request *inside* the mutex so no other
      // pass can observe a half-refreshed reference. Event handlers that
      // need the baseline re-fetched (a HEAD move, a save in disk mode)
      // set `reloadRef` rather than touching the baseline directly.
      // `lastHunksKey` is intentionally kept so an unchanged diff still
      // suppresses a no-op repaint (no flicker on every window focus).
      if (state.reloadRef) {
        state.reloadRef = false;
        state.lastBufferText = null;
        if (state.baselineId !== null) {
          try {
            await editor.refreshDiffBaseline(state.baselineId);
          } catch (_e) {
            // Refresh failed (e.g. the file left the repo). Drop the
            // baseline; the pass below re-registers or clears.
            releaseBaseline(state);
          }
        }
      }
      await onePass();
    } while (state.rerunRequested);
  } finally {
    state.updating = false;
  }

  // A single diff pass. Early `return`s end this pass only; the wrapper's
  // `do…while` still honors any `rerunRequested` recorded meanwhile.
  async function onePass(): Promise<void> {
    const baselineId = await ensureBaseline(state);
    if (baselineId === null) {
      // No reference exists (file untracked, no repo, etc.).
      clearDecorations(bufferId);
      state.hunks = [];
      editor.setViewState(bufferId, "live_diff_hunks", null);
      return;
    }

    const length = editor.getBufferLength(bufferId);
    let newText: string;
    try {
      newText = await editor.getBufferText(bufferId, 0, length);
    } catch (e) {
      // The buffer can close between the awaits above (the git `show`
      // for the reference is slow) and this fetch — e.g. the user
      // closes the file, or an external process churns it while it's
      // open. `buffer_closed` deletes our state, so if it's gone the
      // recompute is moot: bail quietly instead of logging an error.
      if (!states.has(bufferId)) return;
      throw e;
    }

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

    // Host-side diff: hunks come back, file contents don't cross the
    // bridge in either direction. The host never refuses an input; a
    // non-"exact" fidelity means the buffer's line index isn't
    // available yet (large file before its line-feed scan) and there
    // are no line hunks to render.
    let result: DiffBaselineResult;
    try {
      result = await editor.diffAgainstBaseline(bufferId, baselineId);
    } catch (e) {
      if (!states.has(bufferId)) return;
      throw e;
    }
    if (result.fidelity !== "exact") {
      clearDecorations(bufferId);
      state.hunks = [];
      state.lastHunksKey = "";
      editor.setViewState(bufferId, "live_diff_hunks", null);
      if (state.lastDetail === "full") {
        editor.setStatus(editor.t("status.simplified"));
      }
      state.lastDetail = "outline";
      return;
    }
    const raw = result.hunks;

    let totalVirtual = 0;
    let totalChanged = 0;
    for (const h of raw) {
      totalVirtual += h.oldCount;
      totalChanged += h.newCount;
    }
    const detail: DetailLevel =
      totalChanged > MAX_OVERLAY_LINES
        ? "outline"
        : totalVirtual > MAX_VIRTUAL_LINES
          ? "no-vlines"
          : "full";

    // Old-side text is fetched only at full detail, and only the lines
    // the diff actually names — one batched call, traffic proportional
    // to the change, not the file. Degraded levels never render
    // old-side text, so they carry none.
    let oldGroups: string[][] = [];
    if (detail === "full") {
      const ranges = raw
        .filter((h) => h.oldCount > 0)
        .map((h) => [h.oldStart, h.oldCount]);
      if (ranges.length > 0) {
        try {
          oldGroups = await editor.getBaselineLines(baselineId, ranges);
        } catch (e) {
          if (!states.has(bufferId)) return;
          throw e;
        }
      }
    }
    const rawHunks = hostHunksToHunks(raw, oldGroups);
    const hunks =
      detail === "full"
        ? refineHunks(rawHunks, newLines)
        : rawHunks.map((h) => ({
            kind: h.kind,
            newStart: h.newStart,
            newCount: h.newCount,
            oldLines: [],
          }));

    // Skip 2: same hunks as last render. The user can edit inside an
    // already-flagged region without changing line counts (e.g., typing
    // mid-word on a modified line). Without this guard we still
    // clear+repaint each keystroke, producing visible flicker. The
    // detail level is part of the key: the same hunks at a different
    // level must repaint.
    const hunksKey = detail + "|" + JSON.stringify(hunks);
    if (hunksKey === state.lastHunksKey) {
      state.hunks = hunks;
      return;
    }
    state.hunks = hunks;
    state.lastHunksKey = hunksKey;

    renderHunks(state, newLines, detail);

    editor.setViewState(bufferId, "live_diff_hunks", hunks);

    // Announce degraded rendering once, on the transition into it —
    // not on every recompute while the user types inside a huge diff.
    if (detail !== "full" && state.lastDetail === "full") {
      editor.setStatus(editor.t("status.simplified"));
    }
    state.lastDetail = detail;
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
    baselineId: null,
    hunks: [],
    updating: false,
    rerunRequested: false,
    reloadRef: false,
    pendingToken: 0,
    override: getStoredOverride(bufferId),
    lastBufferText: null,
    lastHunksKey: "",
    lastDetail: "full",
  };
  states.set(bufferId, state);
  return state;
}

function dropReference(state: BufferDiffState): void {
  releaseBaseline(state);
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
    state.lastDetail = "full";
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
// Git reference watching (#2503)
// =============================================================================
//
// In `head` / `branch` mode the reference (left) side is the *committed* file,
// not the working copy. Committing the buffer's changes moves HEAD so the
// reference now matches the buffer and the diff should empty out — but a commit
// doesn't touch the buffer, so none of the edit-driven recompute hooks fire and
// the cached reference goes stale: the gutter keeps showing the change against
// the pre-commit HEAD until a manual "Live Diff: Refresh".
//
// Fix: watch the HEAD reflog and drop the cached reference when it moves. A
// plain `git commit` on the current branch appends to `logs/HEAD` (and updates
// the branch ref) but leaves the bare `HEAD` file untouched, so the reflog —
// not `HEAD` — is the file that reliably moves on every commit, checkout, reset
// and merge (it also covers detached-HEAD commits, which rewrite `HEAD`
// itself). `focus_gained` is a cheap fallback for the rare repo with reflogs
// disabled, or when the watch can't be registered.

let headWatchHandle: number | null = null;
let watchedCwd: string | null = null;
let ensureWatchInFlight: Promise<void> | null = null;

// Resolve a directory inside the repo we should watch. The plugin loads each
// reference relative to its *file's* directory (see `loadReference`), not
// `editor.getCwd()` — the active buffer can live in a different repo than the
// editor cwd — so we resolve the reflog the same way for consistency.
function gitWatchCwd(): string | null {
  const activeState = states.get(editor.getActiveBufferId());
  if (activeState) return fileDir(activeState.filePath);
  for (const s of states.values()) return fileDir(s.filePath);
  return null;
}

async function discoverHeadLogPath(cwd: string): Promise<string | null> {
  // `--absolute-git-dir` resolves the git dir (worktrees / submodules /
  // `--git-dir` setups all included) as a normalized *absolute* path. We must
  // not use `--git-path logs/HEAD` here: it returns a path *relative to cwd*
  // (e.g. `../.git/logs/HEAD` when the buffer is in a sub-directory), and the
  // file watcher compares against notify's canonical, `..`-free event paths —
  // a `..` in the watch path means the reflog event never matches (#2503).
  const result = await editor.spawnProcess(
    "git", ["rev-parse", "--absolute-git-dir"], cwd,
  );
  if (result.exit_code !== 0) return null;
  const gitDir = result.stdout.trim();
  if (!gitDir) return null;
  return `${gitDir}/logs/HEAD`;
}

async function ensureHeadWatch(): Promise<void> {
  const cwd = gitWatchCwd();
  if (!cwd) return;
  if (watchedCwd === cwd && headWatchHandle !== null) return;
  if (ensureWatchInFlight) return ensureWatchInFlight;

  ensureWatchInFlight = (async () => {
    try {
      if (headWatchHandle !== null) {
        editor.unwatchPath(headWatchHandle);
        headWatchHandle = null;
      }
      watchedCwd = cwd;
      const headLog = await discoverHeadLogPath(cwd);
      if (!headLog) return;
      try {
        headWatchHandle = await editor.watchPath(headLog, false);
      } catch (_e) {
        // Watch registration failed (reflog missing in a fresh repo with no
        // commits yet, kernel watch limit). focus_gained is the fallback.
      }
    } finally {
      ensureWatchInFlight = null;
    }
  })();
  return ensureWatchInFlight;
}

// Drop the cached git reference for every git-backed (head/branch) buffer and
// recompute, so the diff re-fetches its reference and clears if the buffer now
// matches the committed tree. `disk` mode tracks the working copy, not a
// committed ref, so a HEAD move doesn't affect it.
function refreshGitReferences(): void {
  for (const state of states.values()) {
    if (state.mode.kind === "disk") continue;
    if (!isEnabledForBuffer(state)) continue;
    // Request a baseline refresh (HEAD may have moved) via the flag the
    // recompute consumes under its mutex — NOT by refreshing here, which
    // would race an in-flight recompute for this buffer the same way the
    // old in-plugin reference cache did (every line shown as added, and
    // never cleared). `reloadRef` keeps `lastHunksKey`, so when the diff
    // is in fact unchanged the recompute's hunk-equality guard still
    // suppresses the repaint and there's no flicker on every window focus.
    state.reloadRef = true;
    recompute(state.bufferId).catch((e) => editor.error(`live-diff: ${e}`));
  }
}

// =============================================================================
// Event wiring
// =============================================================================

editor.on("after_file_open", (args) => {
  const state = ensureState(args.buffer_id);
  if (!state) return true;
  ensureHeadWatch().catch((e) => editor.error(`live-diff: ${e}`));
  recompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  return true;
});

// A git HEAD movement (commit, checkout, reset, merge) makes every "vs HEAD" /
// "vs <branch>" reference potentially stale — re-fetch and repaint (#2503).
editor.on("path_changed", (args) => {
  if (args.handle !== headWatchHandle) return true;
  refreshGitReferences();
  return true;
});

// Picks up commits made while fresh was unfocused (e.g. via an external
// terminal), and is the fallback when the reflog watch couldn't be registered.
editor.on("focus_gained", () => {
  ensureHeadWatch().catch((e) => editor.error(`live-diff: ${e}`));
  refreshGitReferences();
  return true;
});

editor.on("buffer_activated", (args) => {
  const state = ensureState(args.buffer_id);
  if (!state) return true;
  // Indicators stick around across activations; only repaint if we never
  // ran a first pass (e.g. plugin loaded after the buffer opened).
  if (state.hunks.length === 0 && state.baselineId === null) {
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
  if (state.filePath !== args.path) {
    // Save-as: the registered baseline is bound to the old path; drop it
    // so the next recompute registers against the new one.
    state.filePath = args.path;
    dropReference(state);
  } else if (state.mode.kind === "disk") {
    // A plain save rewrites the disk reference: refresh it under the
    // recompute mutex.
    state.reloadRef = true;
  }
  recompute(args.buffer_id).catch((e) => editor.error(`live-diff: ${e}`));
  return true;
});

editor.on("after_file_revert", (args) => {
  const state = states.get(args.buffer_id);
  if (!state) return true;
  // The buffer was reloaded from disk (auto-revert after an external change,
  // or an explicit revert). The disk-mode reference *is* the file on disk,
  // so it is stale now; every mode needs a recompute against the reloaded
  // content — the edit hooks don't fire for a wholesale buffer replacement.
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
// Plugin API
// =============================================================================

export type LiveDiffApi = {
  /** Lines whose Sørensen–Dice similarity ratio is at least this value
   * render as in-place "modified" (bg highlight + word-level diff on the
   * new line). Below this they split into a removed + added pair so the
   * change reads as a rewrite. Range 0..1; clamped. */
  setSimilarityThreshold(value: number): void;
  getSimilarityThreshold(): number;
};

declare global {
  interface FreshPluginRegistry {
    "live-diff": LiveDiffApi;
  }
}

editor.exportPluginApi("live-diff", {
  setSimilarityThreshold(value: number): void {
    const clamped = Math.max(0, Math.min(1, value));
    if (clamped === similarityThreshold) return;
    similarityThreshold = clamped;
    // Invalidate cached hunks so the next recompute repaints with the
    // new threshold instead of short-circuiting on the same hunksKey.
    for (const state of states.values()) {
      state.lastHunksKey = "";
      scheduleRecompute(state.bufferId).catch((e) =>
        editor.error(`live-diff: ${e}`),
      );
    }
  },
  getSimilarityThreshold: () => similarityThreshold,
} satisfies LiveDiffApi);

// =============================================================================
// Initialization
// =============================================================================

ensureHeadWatch().catch((e) => editor.error(`live-diff: ${e}`));

const initBid = editor.getActiveBufferId();
if (initBid !== 0) {
  const state = ensureState(initBid);
  if (state) {
    recompute(initBid).catch((e) => editor.error(`live-diff: ${e}`));
  }
}

editor.debug("Live Diff plugin loaded");
