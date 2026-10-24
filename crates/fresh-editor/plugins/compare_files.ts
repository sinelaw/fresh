/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Compare Files Plugin
 *
 * Shows two arbitrary files side by side, both editable, with the
 * differences aligned and highlighted — what `vimdiff A B` or a GUI diff
 * tool does, and what `fresh --diff A B` (and so `git difftool`) opens.
 *
 * The comparison is one tab: a buffer group whose two panels name the files
 * by `path`. A file that is already open is shown from its own buffer; one
 * that isn't is opened into its panel without a tab. Either way the panels
 * are views of real file buffers, so everything a file buffer does works in
 * them: typing, undo, save, LSP, search. What this plugin adds:
 *
 *   - a background on every changed line, and on the changed span of a
 *     line that was edited in place;
 *   - filler rows on the shorter side of a change, so the lines that match
 *     sit on the same screen row in both panels;
 *   - a note where two lines differ only in their line ending;
 *   - scroll sync between the panels, anchored at every change;
 *   - a fresh diff after every edit to either side.
 *
 * The diff and every offset come from the host (`compareBuffers`): no file
 * text is copied into the plugin, which is what kept large files slow.
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
 * A file that was already open keeps its tab, and while the comparison is
 * open that tab shows the same highlights; closing the comparison removes
 * them.
 */

// =============================================================================
// Constants
// =============================================================================

const NS_LINES = "compare-files";
const NS_SPANS = "compare-files-spans";
const NS_FILL = "compare-files-fill";
const NS_GUTTER = "compare-files-gutter";
const NS_SCROLL = "compare-files-scroll";
const EOL_NOTE_PREFIX = "compare-files-eol-";

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
  noteFg: "editor.line_number_fg",
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

// =============================================================================
// Types
// =============================================================================

type Side = "left" | "right";

interface Comparison {
  groupId: number;
  windowId: number;
  /** The right panel's split id: unique while the panel lives. */
  syncGroupId: number;
  left: number;
  right: number;
  /** The last diff, or null before the first one lands. */
  result: CompareResult | null;
  /** Bumped on every scheduled diff; a stale timer sees a newer value. */
  token: number;
  running: boolean;
  rerun: boolean;
  /** A diff was wanted while another window was active. */
  deferred: boolean;
}

const comparisons: Comparison[] = [];

// =============================================================================
// Lookup
// =============================================================================

function comparisonOf(bufferId: number): Comparison | null {
  return comparisons.find((c) => c.left === bufferId || c.right === bufferId) ?? null;
}

function other(side: Side): Side {
  return side === "left" ? "right" : "left";
}

function bufferOf(c: Comparison, side: Side): number {
  return side === "left" ? c.left : c.right;
}

/** The comparison the user is in: the one showing the focused buffer. */
function activeComparison(): { c: Comparison; side: Side } | null {
  const bid = editor.getActiveBufferId();
  const c = comparisonOf(bid);
  return c ? { c, side: c.left === bid ? "left" : "right" } : null;
}

/** A failure's message, without the `Error: ` its string form adds. */
function reason(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
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

/** Run plugin commands one at a time. A command reads the cursor, and a
 * jump the previous one made is only visible once it is applied, so two
 * quick presses of Next Difference must not both read the old position. */
let serial: Promise<void> = Promise.resolve();
function oneAtATime(task: () => Promise<void>): Promise<void> {
  serial = serial.then(task).catch((e) => editor.error(`compare_files: ${e}`));
  return serial;
}

// =============================================================================
// Rendering
// =============================================================================

function clearDecorations(bufferId: number): void {
  editor.clearNamespace(bufferId, NS_LINES);
  editor.clearNamespace(bufferId, NS_SPANS);
  editor.clearVirtualTextNamespace(bufferId, NS_FILL);
  editor.removeVirtualTextsByPrefix(bufferId, EOL_NOTE_PREFIX);
  editor.clearLineIndicators(bufferId, NS_GUTTER);
  editor.clearScrollbarMarkers(bufferId, NS_SCROLL);
}

/** How line `i` of a hunk side ends: the bytes between its text and the
 * next line (or the hunk's end). */
function terminator(s: CompareSide, i: number): number {
  const next = i + 1 < s.count ? s.lines[i + 1][0] : s.to;
  return next - s.lines[i][1];
}

function eolNote(bytes: number): string {
  if (bytes === 0) return editor.t("eol.none");
  return bytes === 2 ? editor.t("eol.crlf") : editor.t("eol.lf");
}

/** Paint one side: line backgrounds, changed spans, line-ending notes,
 * gutter glyphs, scrollbar marks, and the filler rows that keep it level
 * with the other side. */
function paintSide(c: Comparison, r: CompareResult, side: Side): void {
  const bid = bufferOf(c, side);
  const len = side === "left" ? r.leftLen : r.rightLen;
  const left = side === "left";
  clearDecorations(bid);

  const loneBg = left ? THEME.removedBg : THEME.addedBg;
  const loneFg = left ? THEME.removedFg : THEME.addedFg;
  const spanBg = left ? THEME.removedSpanBg : THEME.addedSpanBg;
  const loneColor = left ? COLORS.removed : COLORS.added;
  // Filler rows stand for lines only the other side has: their colour.
  const fillBg = left ? THEME.addedBg : THEME.removedBg;

  const pairedLines: number[] = [];
  const loneLines: number[] = [];
  const markers: ScrollbarMarker[] = [];

  r.hunks.forEach((h, n) => {
    const mine = h[side];
    const theirs = h[other(side)];
    const paired = Math.min(mine.count, theirs.count);

    for (let i = 0; i < mine.count; i++) {
      const [start, end] = mine.lines[i];
      const isPaired = i < paired;
      (isPaired ? pairedLines : loneLines).push(mine.start + i);
      // An empty line still needs a byte under the overlay for the
      // line-end fill to reach it (see live_diff.ts `renderHunks`); its
      // terminator is that byte.
      editor.addOverlay(bid, NS_LINES, start, Math.max(end, Math.min(start + 1, len)), {
        bg: isPaired ? THEME.modifiedBg : loneBg,
        fg: isPaired ? THEME.modifiedFg : loneFg,
        fgOnCollisionOnly: true,
        extendToLineEnd: true,
      });
      const span = isPaired ? mine.spans[i] : null;
      if (span && span[1] > span[0]) {
        editor.addOverlay(bid, NS_SPANS, span[0], span[1], { bg: spanBg });
      }
      // A paired line with no changed span differs only in how it ends.
      if (isPaired && !span) {
        // Before the line's terminator; a last line with none has no byte
        // there to hang the note on, so it goes after the line's last one.
        const atEnd = end >= len && end > start;
        editor.addVirtualTextStyled(
          bid,
          `${EOL_NOTE_PREFIX}${n}-${i}`,
          atEnd ? end - 1 : end,
          ` ${eolNote(terminator(mine, i))}`,
          { fg: THEME.noteFg, italic: true },
          !atEnd,
        );
      }
    }

    // Filler rows for the lines the other side has here and this one
    // doesn't, placed right after the line before them: below the hunk's
    // last line on this side, or below the line before the change when the
    // hunk is empty here. Drawn after a line, they stay out of a pane whose
    // top is the first line after the change, so scrolling past the change
    // keeps the two panes level. Only at the very start of a file do they
    // go above the first line.
    const fill = theirs.count - mine.count;
    if (fill > 0) {
      let anchor: number;
      let above = false;
      if (mine.count > 0) {
        anchor = mine.lines[mine.count - 1][0];
      } else if (mine.from > 0) {
        anchor = mine.from - 1;
      } else {
        anchor = 0;
        above = true;
      }
      for (let i = 0; i < fill; i++) {
        editor.addVirtualLine(bid, anchor, "", { bg: fillBg }, above, NS_FILL, i);
      }
    }

    // A scrollbar mark per change, on the seam when this side has no lines.
    const position = Math.min(mine.from, Math.max(0, len - 1));
    const marker: ScrollbarMarker = {
      position,
      color: mine.count === 0 ? (left ? COLORS.added : COLORS.removed) : paired > 0 ? COLORS.modified : loneColor,
      priority: PRIORITY,
    };
    if (mine.count > 0) marker.end = Math.max(mine.to - 1, position);
    markers.push(marker);
  });

  if (pairedLines.length > 0) {
    editor.setLineIndicators(
      bid, pairedLines, NS_GUTTER, "~",
      COLORS.modified[0], COLORS.modified[1], COLORS.modified[2], PRIORITY,
    );
  }
  if (loneLines.length > 0) {
    editor.setLineIndicators(
      bid, loneLines, NS_GUTTER, left ? "-" : "+",
      loneColor[0], loneColor[1], loneColor[2], PRIORITY,
    );
  }
  editor.setScrollbarMarkers(bid, NS_SCROLL, markers);
}

/** Scroll-sync anchors: the two sides line up at the end of every change.
 *
 * The panes sync in screen rows, filler rows included, and an anchor pairs
 * the rows its two lines' text is on. With the fillers keeping the sides
 * level, the rows between two changes' ends match one for one, so the ends
 * are all the anchors need; they hold the panes together while an edit
 * waits for its diff. The start of a change is no anchor: on the side with
 * no lines there, its line is drawn after the fillers, a row the other side
 * gives to the change's first line. */
function syncAnchors(r: CompareResult): number[][] {
  return r.hunks.map(({ left, right }) => [left.start + left.count, right.start + right.count]);
}

function summary(r: CompareResult): string {
  const n = r.hunks.length;
  return n === 0 ? editor.t("status.identical") : editor.t("status.differences", { n: String(n) });
}

// =============================================================================
// Diffing
// =============================================================================

function apply(c: Comparison, r: CompareResult): void {
  c.result = r;
  paintSide(c, r, "left");
  paintSide(c, r, "right");
  editor.setScrollSyncAnchors(c.syncGroupId, syncAnchors(r));
}

async function rediff(c: Comparison): Promise<void> {
  // The host diffs and decorates the active window's buffers: a comparison
  // in a window in the background waits for it to come back.
  if (editor.activeWindow() !== c.windowId) {
    c.deferred = true;
    return;
  }
  if (c.running) {
    c.rerun = true;
    return;
  }
  c.running = true;
  try {
    do {
      c.rerun = false;
      const r = await editor.compareBuffers(c.left, c.right);
      // Closed while the diff was on its way.
      if (!comparisons.includes(c)) return;
      apply(c, r);
    } while (c.rerun);
  } catch (e) {
    editor.setStatus(editor.t("status.failed", { reason: reason(e) }));
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
  if (leftAbs === rightAbs) {
    editor.setStatus(editor.t("status.same_file"));
    return false;
  }
  // A buffer is in one comparison at a time: its decorations are its own.
  for (const p of [leftAbs, rightAbs]) {
    const existing = comparisonOf(editor.findBufferByPath(p));
    if (existing) closeComparison(existing);
  }
  await editor.flush();

  const panel = (id: string, path: string) => ({ type: "scrollable", id, path });
  let group: BufferGroupResult;
  try {
    group = await editor.createBufferGroup(
      editor.t("tab_name", tabLabels(leftAbs, rightAbs)),
      "",
      JSON.stringify({
        type: "split",
        direction: "h",
        ratio: 0.5,
        first: panel("left", leftAbs),
        second: panel("right", rightAbs),
      }),
    );
  } catch (e) {
    editor.setStatus(editor.t("status.failed", { reason: reason(e) }));
    return false;
  }
  const left = group.panels.left;
  const right = group.panels.right;
  if (left === right) {
    // Two paths to one file (a symlink, say).
    editor.closeBufferGroup(group.groupId);
    editor.setStatus(editor.t("status.same_file"));
    return false;
  }

  const c: Comparison = {
    groupId: group.groupId,
    windowId: editor.activeWindow(),
    syncGroupId: group.panelSplits.right,
    left,
    right,
    result: null,
    token: 0,
    running: false,
    rerun: false,
    deferred: false,
  };

  let first: CompareResult;
  try {
    first = await editor.compareBuffers(left, right);
  } catch (e) {
    // A binary file, or one too large to diff: nothing to show.
    editor.closeBufferGroup(group.groupId);
    editor.setStatus(editor.t("status.failed", { reason: reason(e) }));
    return false;
  }

  comparisons.push(c);
  editor.setContext("compare-active", true);
  // Fillers keep rows level only while one buffer line is one screen row.
  editor.setLineWrap(left, group.panelSplits.left, false);
  editor.setLineWrap(right, group.panelSplits.right, false);
  editor.createScrollSyncGroup(c.syncGroupId, group.panelSplits.left, group.panelSplits.right);
  editor.focusBufferGroupPanel(group.groupId, "left");
  apply(c, first);
  if (first.hunks.length > 0) jumpTo(c, first.hunks[0]);
  editor.setStatus(summary(first));
  return true;
}

/** Drop `c` and its decorations. Its scroll-sync group goes with its
 * panels, so a comparison whose tab already closed has none to remove. */
function forget(c: Comparison): void {
  const i = comparisons.indexOf(c);
  if (i < 0) return;
  comparisons.splice(i, 1);
  for (const bid of [c.left, c.right]) clearDecorations(bid);
  if (comparisons.length === 0) editor.setContext("compare-active", false);
}

function closeComparison(c: Comparison): void {
  forget(c);
  editor.removeScrollSyncGroup(c.syncGroupId);
  editor.closeBufferGroup(c.groupId);
}

// =============================================================================
// Navigation and merging
// =============================================================================

/** Put both cursors on the start of hunk `h`. */
function jumpTo(c: Comparison, h: CompareHunk): void {
  editor.setBufferCursor(c.left, h.left.from);
  editor.setBufferCursor(c.right, h.right.from);
}

/** The hunk the cursor is in on `side`: the one whose lines it is on, or,
 * for a hunk with no lines on that side, the one at its seam. */
function hunkAtCursor(r: CompareResult, side: Side): CompareHunk | null {
  const line = editor.getPrimaryCursor()?.line ?? 0;
  for (const h of r.hunks) {
    const { start, count } = h[side];
    if (count > 0 ? line >= start && line < start + count : line === start || line === start - 1) {
      return h;
    }
  }
  return null;
}

async function step(forward: boolean): Promise<void> {
  // The cursor as the last jump left it.
  await editor.flush();
  const at = activeComparison();
  if (!at) {
    editor.setStatus(editor.t("status.not_comparing"));
    return;
  }
  const { c, side } = at;
  const r = c.result;
  if (!r || r.hunks.length === 0) {
    editor.setStatus(editor.t("status.identical"));
    return;
  }
  // By byte, so a change past the end of one side (which can't hold a
  // cursor line of its own) still has a place in the order.
  const pos = editor.getPrimaryCursor()?.position ?? 0;
  const n = r.hunks.length;
  let index = -1;
  if (forward) {
    index = r.hunks.findIndex((h) => h[side].from > pos);
  } else {
    for (let i = n - 1; i >= 0; i--) {
      if (r.hunks[i][side].from < pos) {
        index = i;
        break;
      }
    }
  }
  const wrapped = index < 0;
  if (wrapped) index = forward ? 0 : n - 1;
  jumpTo(c, r.hunks[index]);
  editor.setStatus(
    editor.t(wrapped ? "status.difference_wrapped" : "status.difference", {
      n: String(index + 1),
      total: String(n),
    }),
  );
}

/** Make `to`'s lines of the change under the cursor the same bytes as
 * `from`'s — line endings and a missing final newline included, so the
 * change is gone after. One edit, one undo step, and only if `to` hasn't
 * changed since the diff it was measured from. */
async function merge(direction: "copy" | "take"): Promise<void> {
  await editor.flush();
  const at = activeComparison();
  if (!at) {
    editor.setStatus(editor.t("status.not_comparing"));
    return;
  }
  const { c, side } = at;
  // Offsets from a diff of the text as it is now, not as of the last
  // debounced one.
  c.token++;
  let r: CompareResult;
  try {
    r = await editor.compareBuffers(c.left, c.right);
  } catch (e) {
    editor.setStatus(editor.t("status.failed", { reason: reason(e) }));
    return;
  }
  apply(c, r);
  const h = hunkAtCursor(r, side);
  if (!h) {
    editor.setStatus(editor.t("status.no_change_here"));
    return;
  }
  const from = direction === "copy" ? side : other(side);
  const to = other(from);
  const src = h[from];
  const dst = h[to];
  const text = src.to > src.from ? await editor.getBufferText(bufferOf(c, from), src.from, src.to) : "";
  const version = to === "left" ? r.leftVersion : r.rightVersion;
  const applied = await editor.replaceRange(bufferOf(c, to), dst.from, dst.to, text, version);
  // A plugin's own edits fire no `buffer_modified`, so diff again here.
  await rediff(c);
  if (!applied) {
    const path = editor.getBufferPath(bufferOf(c, to));
    editor.setStatus(editor.t("status.not_applied", { name: baseName(path) }));
  } else if (c.result) {
    editor.setStatus(summary(c.result));
  }
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
  const path = editor.getBufferPath(editor.getActiveBufferId());
  if (!path) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  const right = await editor.pickFile(editor.t("prompt.with", { name: baseName(path) }));
  if (!right) return;
  await compare(path, right);
}
registerHandler("compare_with_file", compare_with_file);

function compare_next_difference(): Promise<void> {
  return oneAtATime(() => step(true));
}
registerHandler("compare_next_difference", compare_next_difference);

function compare_prev_difference(): Promise<void> {
  return oneAtATime(() => step(false));
}
registerHandler("compare_prev_difference", compare_prev_difference);

function compare_copy_change(): Promise<void> {
  return oneAtATime(() => merge("copy"));
}
registerHandler("compare_copy_change", compare_copy_change);

function compare_take_change(): Promise<void> {
  return oneAtATime(() => merge("take"));
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

// `fresh --diff LEFT RIGHT`: the editor leaves opening the files to us.
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

editor.on("active_window_changed", (args) => {
  for (const c of comparisons) {
    if (c.deferred && c.windowId === args.active_id) {
      c.deferred = false;
      void rediff(c);
    }
  }
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
