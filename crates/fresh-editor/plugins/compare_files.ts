/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Compare Files Plugin
 *
 * Shows two arbitrary files side by side, both editable, with the
 * differences aligned and highlighted — what `vimdiff A B` or a GUI diff
 * tool does, and what `fresh --diff A B` (and so `git difftool`) opens.
 *
 * The two panels are views of the files' own buffers (a buffer-group leaf
 * with `bufferId`), not read-only copies, so everything a file buffer does
 * works in them: typing, undo, save, LSP, search. What this plugin adds:
 *
 *   - a background on every changed line, and on the changed span of a
 *     line that was edited in place;
 *   - filler rows on the shorter side of a change, so the lines that match
 *     sit on the same screen row in both panels;
 *   - scroll sync between the panels, anchored at every change;
 *   - a fresh diff after every edit to either side.
 *
 * Commands:
 *   - Compare Files...                   — pick two files
 *   - Compare Active File With...        — the active file and one picked file
 *   - Compare: Next / Previous Difference (Alt+F5 / Shift+Alt+F5)
 *   - Compare: Switch Side (Ctrl+F6)       — focus the other panel
 *   - Compare: Copy Change to Other Side — make the other side's lines of
 *                                          the change under the cursor match
 *                                          this side (vimdiff's `dp`)
 *   - Compare: Take Change from Other Side — the reverse (vimdiff's `do`)
 *   - Compare: Close
 *
 * The decorations belong to the buffers, so while a comparison is open
 * they also show in any other pane that has one of its files open; closing
 * the comparison removes them.
 */

// =============================================================================
// Constants
// =============================================================================

const NS_LINES = "compare-files";
const NS_SPANS = "compare-files-spans";
const NS_FILL = "compare-files-fill";
const NS_GUTTER = "compare-files-gutter";
const NS_SCROLL = "compare-files-scroll";

// Above git_gutter (10) and live_diff (9): inside a comparison the gutter
// says how this file differs from the other one, not from HEAD.
const PRIORITY = 20;

const THEME = {
  removedBg: "editor.diff_remove_bg",
  removedFg: "editor.diff_remove_collision_fg",
  removedSpanBg: "editor.diff_remove_highlight_bg",
  addedBg: "editor.diff_add_bg",
  addedFg: "editor.diff_add_collision_fg",
  addedSpanBg: "editor.diff_add_highlight_bg",
  modifiedBg: "editor.diff_modify_bg",
  modifiedFg: "editor.diff_modify_collision_fg",
};

// Gutter glyphs and scrollbar marks take RGB only; same palette as
// live_diff.ts so a change reads the same in either plugin.
const COLORS = {
  added: [80, 200, 120] as [number, number, number],
  modified: [220, 160, 90] as [number, number, number],
  removed: [220, 90, 90] as [number, number, number],
};

// Coalesce an edit burst (typing, a paste, an undo) into one diff.
const DEBOUNCE_MS = 75;

// The changed-span emphasis compares a pair of lines character by
// character; past this many changed lines in one diff it is left out and
// the whole-line backgrounds carry the change on their own.
const MAX_SPAN_LINES = 5_000;

// Per-line scroll-sync anchors across one-sided changes (see
// `syncAnchors`), at most this many in all.
const MAX_EDGE_ANCHORS = 20_000;

// Scroll-sync group ids are per plugin; start high so they never meet an
// id the plugin hands out for something else.
let nextSyncGroupId = 7_000;

// =============================================================================
// Types
// =============================================================================

type Side = "left" | "right";

interface Comparison {
  groupId: number;
  windowId: number;
  syncGroupId: number;
  left: number;
  right: number;
  /** Hunks of the last diff, in left/right line coordinates. */
  hunks: LineDiffHunk[];
  /** Line-start byte offsets of each side as of the last diff. */
  starts: { left: number[]; right: number[] };
  /** Each side's text as of the last diff, split after every `\n`. */
  segments: { left: string[]; right: string[] };
  /** Bumped on every scheduled diff; a stale timer sees a newer value. */
  token: number;
  running: boolean;
  rerun: boolean;
}

const comparisons: Comparison[] = [];

// =============================================================================
// Text helpers
// =============================================================================

function utf8Len(s: string): number {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c < 0x80) n += 1;
    else if (c < 0x800) n += 2;
    else if (c >= 0xd800 && c <= 0xdbff) {
      // A surrogate pair is one 4-byte code point.
      n += 4;
      i++;
    } else n += 3;
  }
  return n;
}

/** Split `text` into lines the way `computeLineDiff` counts them: each
 * segment keeps its `\n`, and a final unterminated segment is a line too. */
function splitSegments(text: string): string[] {
  const out: string[] = [];
  let from = 0;
  for (;;) {
    const nl = text.indexOf("\n", from);
    if (nl < 0) break;
    out.push(text.slice(from, nl + 1));
    from = nl + 1;
  }
  if (from < text.length) out.push(text.slice(from));
  return out;
}

/** Byte offset of every line start, plus one past the end. */
function lineStarts(segments: string[]): number[] {
  const starts = new Array<number>(segments.length + 1);
  let pos = 0;
  for (let i = 0; i < segments.length; i++) {
    starts[i] = pos;
    pos += utf8Len(segments[i]);
  }
  starts[segments.length] = pos;
  return starts;
}

function stripNewline(segment: string): string {
  if (segment.endsWith("\r\n")) return segment.slice(0, -2);
  if (segment.endsWith("\n")) return segment.slice(0, -1);
  return segment;
}

/** The part of `a` that differs from `b`, as UTF-8 byte offsets into `a`:
 * what is left once their common prefix and suffix are taken off. `null`
 * when the lines are equal. */
function changedSpan(a: string, b: string): { start: number; end: number } | null {
  let pre = 0;
  const max = Math.min(a.length, b.length);
  while (pre < max && a.charCodeAt(pre) === b.charCodeAt(pre)) pre++;
  let suf = 0;
  while (
    suf < max - pre &&
    a.charCodeAt(a.length - 1 - suf) === b.charCodeAt(b.length - 1 - suf)
  ) suf++;
  // Keep a surrogate pair whole on both ends.
  if (pre > 0 && isHighSurrogate(a.charCodeAt(pre - 1))) pre--;
  if (suf > 0 && isHighSurrogate(a.charCodeAt(a.length - suf - 1))) suf--;
  if (pre + suf >= a.length) return null;
  const start = utf8Len(a.slice(0, pre));
  return { start, end: start + utf8Len(a.slice(pre, a.length - suf)) };
}

function isHighSurrogate(c: number): boolean {
  return c >= 0xd800 && c <= 0xdbff;
}

// =============================================================================
// Lookup
// =============================================================================

function comparisonOf(bufferId: number): Comparison | null {
  return comparisons.find((c) => c.left === bufferId || c.right === bufferId) ?? null;
}

function sideOf(c: Comparison, bufferId: number): Side {
  return c.left === bufferId ? "left" : "right";
}

function other(side: Side): Side {
  return side === "left" ? "right" : "left";
}

function bufferOf(c: Comparison, side: Side): number {
  return side === "left" ? c.left : c.right;
}

/** A hunk's first line and line count on one side. */
function span(h: LineDiffHunk, side: Side): { start: number; count: number } {
  return side === "left"
    ? { start: h.oldStart, count: h.oldCount }
    : { start: h.newStart, count: h.newCount };
}

/** The comparison the user is in: the one showing the focused buffer. */
function activeComparison(): { c: Comparison; side: Side } | null {
  const bid = editor.getActiveBufferId();
  const c = comparisonOf(bid);
  return c ? { c, side: sideOf(c, bid) } : null;
}

function baseName(path: string): string {
  return editor.pathBasename(path) || path;
}

/** What the tab calls the two files: their names, with the directory that
 * holds each when the names alone are the same (as they are for every file
 * `git difftool` hands over). */
function tabLabels(leftPath: string, rightPath: string): { left: string; right: string } {
  const left = baseName(leftPath);
  const right = baseName(rightPath);
  if (left !== right) return { left, right };
  const withDir = (p: string, name: string) => {
    const dir = baseName(editor.pathDirname(p));
    return dir ? `${dir}/${name}` : name;
  };
  return { left: withDir(leftPath, left), right: withDir(rightPath, right) };
}

// =============================================================================
// Rendering
// =============================================================================

function clearDecorations(bufferId: number): void {
  editor.clearNamespace(bufferId, NS_LINES);
  editor.clearNamespace(bufferId, NS_SPANS);
  editor.clearVirtualTextNamespace(bufferId, NS_FILL);
  editor.clearLineIndicators(bufferId, NS_GUTTER);
  editor.clearScrollbarMarkers(bufferId, NS_SCROLL);
}

/** Paint one side: line backgrounds, changed spans, gutter glyphs,
 * scrollbar marks, and the filler rows that keep it level with the other
 * side. */
function paintSide(c: Comparison, side: Side): void {
  const bid = bufferOf(c, side);
  const segs = c.segments[side];
  const otherSegs = c.segments[other(side)];
  const starts = c.starts[side];
  const lineCount = segs.length;
  const total = starts[lineCount];
  const left = side === "left";
  clearDecorations(bid);

  const pairedBg = THEME.modifiedBg;
  const pairedFg = THEME.modifiedFg;
  const loneBg = left ? THEME.removedBg : THEME.addedBg;
  const loneFg = left ? THEME.removedFg : THEME.addedFg;
  const spanBg = left ? THEME.removedSpanBg : THEME.addedSpanBg;
  const loneColor = left ? COLORS.removed : COLORS.added;
  const loneGlyph = left ? "-" : "+";

  let changedLines = 0;
  for (const h of c.hunks) changedLines += Math.max(h.oldCount, h.newCount);
  const withSpans = changedLines <= MAX_SPAN_LINES;

  const pairedLines: number[] = [];
  const loneLines: number[] = [];
  const markers: { position: number; end?: number; color: [number, number, number]; priority: number }[] = [];

  for (const h of c.hunks) {
    const mine = span(h, side);
    const theirs = span(h, other(side));
    const paired = Math.min(mine.count, theirs.count);

    for (let i = 0; i < mine.count; i++) {
      const line = mine.start + i;
      if (line >= lineCount) break;
      const isPaired = i < paired;
      (isPaired ? pairedLines : loneLines).push(line);
      const start = starts[line];
      const text = stripNewline(segs[line]);
      // An empty line still needs a byte under the overlay for the
      // line-end fill to reach it (see live_diff.ts `renderHunks`).
      const end = Math.max(start + utf8Len(text), start + 1);
      editor.addOverlay(bid, NS_LINES, start, Math.min(end, Math.max(total, start + 1)), {
        bg: isPaired ? pairedBg : loneBg,
        fg: isPaired ? pairedFg : loneFg,
        fgOnCollisionOnly: true,
        extendToLineEnd: true,
      });
      if (isPaired && withSpans) {
        const otherLine = theirs.start + i;
        const changed = changedSpan(text, stripNewline(otherSegs[otherLine] ?? ""));
        if (changed && changed.end > changed.start) {
          editor.addOverlay(bid, NS_SPANS, start + changed.start, start + changed.end, {
            bg: spanBg,
          });
        }
      }
    }

    // Filler rows for the lines the other side has here and this one
    // doesn't. They go below the hunk's last line on this side — above the
    // first line after it when the hunk is empty here — so they sit exactly
    // where the other side's extra lines do.
    const fill = theirs.count - mine.count;
    if (fill > 0) {
      let anchorLine: number;
      let above: boolean;
      if (mine.count > 0) {
        anchorLine = mine.start + mine.count - 1;
        above = false;
      } else if (mine.start < lineCount) {
        anchorLine = mine.start;
        above = true;
      } else {
        anchorLine = Math.max(0, lineCount - 1);
        above = lineCount === 0;
      }
      const anchor = starts[Math.min(anchorLine, lineCount)] ?? 0;
      // The rows stand for lines only the other side has, so they take
      // those lines' colour.
      const fillBg = left ? THEME.addedBg : THEME.removedBg;
      for (let i = 0; i < fill; i++) {
        editor.addVirtualLine(bid, anchor, "", { bg: fillBg }, above, NS_FILL, i);
      }
    }

    // A scrollbar mark per change, on the seam when this side has no lines.
    const markLine = Math.min(mine.start, Math.max(0, lineCount - 1));
    const marker: { position: number; end?: number; color: [number, number, number]; priority: number } = {
      position: starts[markLine] ?? 0,
      color: mine.count === 0 ? (left ? COLORS.added : COLORS.removed) : paired > 0 ? COLORS.modified : loneColor,
      priority: PRIORITY,
    };
    if (mine.count > 0) {
      const last = Math.min(mine.start + mine.count, lineCount);
      marker.end = Math.max(starts[last] - 1, marker.position);
    }
    markers.push(marker);
  }

  if (pairedLines.length > 0) {
    editor.setLineIndicators(
      bid, pairedLines, NS_GUTTER, "~",
      COLORS.modified[0], COLORS.modified[1], COLORS.modified[2], PRIORITY,
    );
  }
  if (loneLines.length > 0) {
    editor.setLineIndicators(
      bid, loneLines, NS_GUTTER, loneGlyph,
      loneColor[0], loneColor[1], loneColor[2], PRIORITY,
    );
  }
  editor.setScrollbarMarkers(bid, NS_SCROLL, markers);
}

/** Scroll-sync anchors: the two sides line up at the start and the end of
 * every change, and move line for line in between.
 *
 * Sync works in whole lines, so a pane whose top is inside the lines only
 * it has here can't put its partner part-way through the filler rows that
 * stand for them. One anchor per such line pins the partner to the edge of
 * the change instead of carrying it on past, into lines that don't match.
 * Very large one-sided changes keep just the two edge anchors. */
function syncAnchors(c: Comparison): number[][] {
  const anchors: number[][] = [[0, 0]];
  let budget = MAX_EDGE_ANCHORS;
  for (const h of c.hunks) {
    anchors.push([h.oldStart, h.newStart]);
    const oldEnd = h.oldStart + h.oldCount;
    const newEnd = h.newStart + h.newCount;
    const paired = Math.min(h.oldCount, h.newCount);
    const extra = Math.abs(h.oldCount - h.newCount);
    if (extra > 0 && extra <= budget) {
      budget -= extra;
      for (let i = paired; i < Math.max(h.oldCount, h.newCount); i++) {
        anchors.push(h.oldCount > h.newCount ? [h.oldStart + i, newEnd] : [oldEnd, h.newStart + i]);
      }
    }
    anchors.push([oldEnd, newEnd]);
  }
  return anchors;
}

function summary(c: Comparison): string {
  const n = c.hunks.length;
  return n === 0
    ? editor.t("status.identical")
    : editor.t("status.differences", { n: String(n) });
}

// =============================================================================
// Diffing
// =============================================================================

async function rediff(c: Comparison): Promise<void> {
  if (c.running) {
    c.rerun = true;
    return;
  }
  c.running = true;
  try {
    do {
      c.rerun = false;
      const [leftText, rightText] = await Promise.all([
        editor.getBufferText(c.left),
        editor.getBufferText(c.right),
      ]);
      // Closed while the text was on its way.
      if (!comparisons.includes(c)) return;
      c.segments = { left: splitSegments(leftText), right: splitSegments(rightText) };
      c.starts = { left: lineStarts(c.segments.left), right: lineStarts(c.segments.right) };
      c.hunks = editor.computeLineDiff(leftText, rightText);
      paintSide(c, "left");
      paintSide(c, "right");
      editor.setScrollSyncAnchors(c.syncGroupId, syncAnchors(c));
    } while (c.rerun);
  } finally {
    c.running = false;
  }
}

async function scheduleRediff(c: Comparison): Promise<void> {
  const token = ++c.token;
  await editor.delay(DEBOUNCE_MS);
  if (token !== c.token || !comparisons.includes(c)) return;
  await rediff(c);
}

// =============================================================================
// Opening and closing
// =============================================================================

/** The id of the buffer holding `path`, opening the file (without taking
 * the focus) if no buffer does. 0 when it could not be opened. */
async function bufferFor(path: string): Promise<number> {
  const find = (): number => {
    const direct = editor.findBufferByPath(path);
    if (direct) return direct;
    for (const info of editor.listBuffers()) {
      if (info.path && info.path === path) return info.id;
    }
    return 0;
  };
  const open = find();
  if (open) return open;
  editor.openFileInBackground(path);
  await editor.flush();
  return find();
}

function absolute(path: string): string {
  return editor.pathIsAbsolute(path) ? path : editor.pathJoin(editor.getCwd(), path);
}

/** Show `leftPath` and `rightPath` side by side. */
async function compare(leftPath: string, rightPath: string): Promise<boolean> {
  const leftAbs = absolute(leftPath);
  const rightAbs = absolute(rightPath);
  for (const p of [leftAbs, rightAbs]) {
    if (!editor.fileExists(p)) {
      editor.setStatus(editor.t("status.no_such_file", { path: p }));
      return false;
    }
  }
  const left = await bufferFor(leftAbs);
  const right = await bufferFor(rightAbs);
  if (!left || !right) {
    editor.setStatus(editor.t("status.open_failed"));
    return false;
  }
  if (left === right) {
    editor.setStatus(editor.t("status.same_file"));
    return false;
  }
  // A buffer is in one comparison at a time: its decorations are its own.
  for (const bid of [left, right]) {
    const existing = comparisonOf(bid);
    if (existing) closeComparison(existing);
  }

  const name = editor.t("tab_name", tabLabels(leftAbs, rightAbs));
  const panel = (id: string, bufferId: number) => ({ type: "scrollable", id, bufferId });
  const group = await editor.createBufferGroup(
    name,
    "",
    JSON.stringify({
      type: "split",
      direction: "h",
      ratio: 0.5,
      first: panel("left", left),
      second: panel("right", right),
    }),
  );
  const syncGroupId = nextSyncGroupId++;
  const c: Comparison = {
    groupId: group.groupId,
    windowId: editor.getBufferInfo(left)?.window_id ?? 0,
    syncGroupId,
    left,
    right,
    hunks: [],
    starts: { left: [0], right: [0] },
    segments: { left: [], right: [] },
    token: 0,
    running: false,
    rerun: false,
  };
  comparisons.push(c);
  editor.setContext("compare-active", true);

  // Fillers keep rows level only while one buffer line is one screen row.
  editor.setLineWrap(left, group.panelSplits.left, false);
  editor.setLineWrap(right, group.panelSplits.right, false);
  editor.createScrollSyncGroup(syncGroupId, group.panelSplits.left, group.panelSplits.right);
  editor.focusBufferGroupPanel(group.groupId, "left");

  await rediff(c);
  if (c.hunks.length > 0) jumpTo(c, "left", c.hunks[0]);
  editor.setStatus(summary(c));
  return true;
}

function forget(c: Comparison): void {
  const i = comparisons.indexOf(c);
  if (i < 0) return;
  comparisons.splice(i, 1);
  for (const bid of [c.left, c.right]) clearDecorations(bid);
  editor.removeScrollSyncGroup(c.syncGroupId);
  if (comparisons.length === 0) editor.setContext("compare-active", false);
}

function closeComparison(c: Comparison): void {
  forget(c);
  editor.closeBufferGroup(c.groupId);
}

// =============================================================================
// Navigation and merging
// =============================================================================

/** Put both cursors on the start of hunk `h`, `side` leading. */
function jumpTo(c: Comparison, side: Side, h: LineDiffHunk): void {
  for (const s of [other(side), side]) {
    const { start } = span(h, s);
    const starts = c.starts[s];
    const line = Math.min(start, Math.max(0, starts.length - 2));
    editor.setBufferCursor(bufferOf(c, s), starts[line] ?? 0);
  }
}

function cursorLine(): number {
  return editor.getPrimaryCursor()?.line ?? 0;
}

/** The hunk the cursor is in on `side`: the one whose lines it is on, or,
 * for a hunk with no lines on that side, the one at its seam. */
function hunkAtCursor(c: Comparison, side: Side): LineDiffHunk | null {
  const line = cursorLine();
  for (const h of c.hunks) {
    const { start, count } = span(h, side);
    if (count > 0 ? line >= start && line < start + count : line === start || line === start - 1) {
      return h;
    }
  }
  return null;
}

function step(forward: boolean): void {
  const at = activeComparison();
  if (!at) {
    editor.setStatus(editor.t("status.not_comparing"));
    return;
  }
  const { c, side } = at;
  if (c.hunks.length === 0) {
    editor.setStatus(editor.t("status.identical"));
    return;
  }
  // Where each hunk puts the cursor on this side (see `jumpTo`): a hunk
  // past this side's last line lands on that line.
  const last = Math.max(0, c.segments[side].length - 1);
  const pos = (h: LineDiffHunk) => Math.min(span(h, side).start, last);
  const line = cursorLine();
  const n = c.hunks.length;
  let index = -1;
  if (forward) {
    index = c.hunks.findIndex((h) => pos(h) > line);
  } else {
    for (let i = n - 1; i >= 0; i--) {
      if (pos(c.hunks[i]) < line) {
        index = i;
        break;
      }
    }
  }
  const wrapped = index < 0;
  if (wrapped) index = forward ? 0 : n - 1;
  jumpTo(c, side, c.hunks[index]);
  editor.setStatus(
    editor.t(wrapped ? "status.difference_wrapped" : "status.difference", {
      n: String(index + 1),
      total: String(n),
    }),
  );
}

/** Replace `to`'s lines of hunk `h` with `from`'s. */
function copyChange(c: Comparison, from: Side, to: Side, h: LineDiffHunk): void {
  const src = span(h, from);
  const dst = span(h, to);
  const srcSegs = c.segments[from];
  const dstSegs = c.segments[to];
  const dstStarts = c.starts[to];
  const dstTotal = dstStarts[dstSegs.length];
  let text = srcSegs.slice(src.start, src.start + src.count).join("");
  const startByte = dstStarts[Math.min(dst.start, dstSegs.length)];
  const endByte = dstStarts[Math.min(dst.start + dst.count, dstSegs.length)];
  const dstEndsFile = endByte >= dstTotal;
  const dstHasFinalNewline = dstSegs.length === 0 || dstSegs[dstSegs.length - 1].endsWith("\n");
  if (dstEndsFile && !dstHasFinalNewline) {
    // This side's file ends without a newline: keep it that way, and start
    // lines appended after its last one on a row of their own.
    if (text.endsWith("\n")) text = text.slice(0, -1);
    if (startByte === dstTotal && startByte > 0 && text.length > 0) text = "\n" + text;
  } else if (text.length > 0 && !text.endsWith("\n")) {
    // The source's last line had no newline (the end of its file); here
    // it is followed by more lines, which keep their own rows.
    text += "\n";
  }
  const bid = bufferOf(c, to);
  if (endByte > startByte) editor.deleteRange(bid, startByte, endByte);
  if (text.length > 0) editor.insertText(bid, startByte, text);
}

async function merge(direction: "copy" | "take"): Promise<void> {
  const at = activeComparison();
  if (!at) {
    editor.setStatus(editor.t("status.not_comparing"));
    return;
  }
  const { c, side } = at;
  const h = hunkAtCursor(c, side);
  if (!h) {
    editor.setStatus(editor.t("status.no_change_here"));
    return;
  }
  if (direction === "copy") copyChange(c, side, other(side), h);
  else copyChange(c, other(side), side, h);
  // A plugin's own edits fire no `buffer_modified`, so diff again here.
  await editor.flush();
  await scheduleRediff(c);
}

// =============================================================================
// Commands
// =============================================================================

async function compare_files(): Promise<void> {
  const left = await editor.pickFile(editor.t("prompt.left"));
  if (!left) return;
  const right = await editor.pickFile(editor.t("prompt.right"));
  if (!right) return;
  await compare(left, right);
}
registerHandler("compare_files", compare_files);

async function compare_with_file(): Promise<void> {
  const active = editor.getActiveBufferId();
  const path = editor.getBufferPath(active);
  if (!path) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  const right = await editor.pickFile(editor.t("prompt.with", { name: baseName(path) }));
  if (!right) return;
  await compare(path, right);
}
registerHandler("compare_with_file", compare_with_file);

function compare_next_difference(): void {
  step(true);
}
registerHandler("compare_next_difference", compare_next_difference);

function compare_prev_difference(): void {
  step(false);
}
registerHandler("compare_prev_difference", compare_prev_difference);

async function compare_copy_change(): Promise<void> {
  await merge("copy");
}
registerHandler("compare_copy_change", compare_copy_change);

async function compare_take_change(): Promise<void> {
  await merge("take");
}
registerHandler("compare_take_change", compare_take_change);

/** Focus the other panel. Next/Previous Split move between the editor's
 * splits, not between the panels of a tab, and the panels are file buffers
 * the plugin can't give keys of their own without taking them from typing. */
function compare_switch_side(): void {
  const at = activeComparison();
  if (!at) {
    editor.setStatus(editor.t("status.not_comparing"));
    return;
  }
  editor.focusBufferGroupPanel(at.c.groupId, other(at.side));
}
registerHandler("compare_switch_side", compare_switch_side);

function compare_close(): void {
  const at = activeComparison();
  if (at) closeComparison(at.c);
  else editor.setStatus(editor.t("status.not_comparing"));
}
registerHandler("compare_close", compare_close);

editor.registerCommand("%cmd.compare_files", "%cmd.compare_files_desc", "compare_files", null);
editor.registerCommand("%cmd.compare_with", "%cmd.compare_with_desc", "compare_with_file", null);
editor.registerCommand("%cmd.next", "%cmd.next_desc", "compare_next_difference", "compare-active");
editor.registerCommand("%cmd.prev", "%cmd.prev_desc", "compare_prev_difference", "compare-active");
editor.registerCommand("%cmd.switch", "%cmd.switch_desc", "compare_switch_side", "compare-active");
editor.registerCommand("%cmd.copy", "%cmd.copy_desc", "compare_copy_change", "compare-active");
editor.registerCommand("%cmd.take", "%cmd.take_desc", "compare_take_change", "compare-active");
editor.registerCommand("%cmd.close", "%cmd.close_desc", "compare_close", "compare-active");

// =============================================================================
// Events
// =============================================================================

// `fresh --diff LEFT RIGHT`: the editor has opened both files by now.
editor.on("ready", async () => {
  const pair = editor.diffLaunch();
  if (pair.length === 2) await compare(pair[0], pair[1]);
});

editor.on("buffer_modified", (args) => {
  const c = comparisonOf(args.buffer_id);
  if (c) void scheduleRediff(c);
});

editor.on("after_file_revert", (args) => {
  const c = comparisonOf(args.buffer_id);
  if (c) void scheduleRediff(c);
});

editor.on("buffer_group_closed", (args) => {
  const c = comparisons.find((x) => x.groupId === args.group_id && x.windowId === args.window_id);
  if (c) forget(c);
});

editor.on("buffer_closed", (args) => {
  const c = comparisonOf(args.buffer_id);
  if (c) forget(c);
});

// For scripts (`fresh --cmd script run`) and other plugins.
editor.exportPluginApi("compare-files", {
  compare: (left: string, right: string) => compare(left, right),
});
