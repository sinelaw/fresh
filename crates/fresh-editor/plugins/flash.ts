/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Flash Jump
 *
 * Label-based jump navigation, ported in spirit from flash.nvim.
 *
 *   1. User invokes the `Flash: Jump` command.
 *   2. Each character typed extends a literal-substring pattern. Every
 *      visible match across **every split** gets a single-letter label.
 *   3. Pressing a label moves the cursor to that match.  If the match
 *      lives in a different split, focus is transferred to that split
 *      first.  Backspace shrinks the pattern; Enter jumps to the
 *      closest match in the active split; Escape (or any non-character
 *      key) cancels and restores the prior cursor and mode.
 *
 * Labels are picked so that no label letter equals the next character
 * after any visible match — this is the flash.nvim "skip" rule and
 * guarantees that pressing a label is never ambiguous with continuing
 * to type the pattern.
 */

const NS_MATCH = "flash";
const VTEXT_PREFIX = "flash-";

// Same pool flash.nvim uses by default — home-row first, ranked by
// reachability.  All lowercase: case-sensitive matching keeps the
// label letter from also being a valid pattern continuation.
const LABEL_POOL = "asdfghjklqwertyuiopzxcvbnm";

interface Match {
  /** Byte offset where the match starts in its buffer. */
  start: number;
  /** Byte offset just past the end of the match. */
  end: number;
  /** Char index of the first match char in the split's viewport text. */
  charIdx: number;
  /** Char index just past the end of the match in the split's viewport text. */
  charEnd: number;
  /** The buffer this match lives in. */
  bufferId: number;
  /** The split currently displaying that buffer. */
  splitId: number;
  /** Assigned label letter, or undefined when out of label pool. */
  label?: string;
}

interface SplitView {
  splitId: number;
  bufferId: number;
  snap: ViewportSnapshot;
}

interface FlashState {
  active: boolean;
  pattern: string;
  matches: Match[];
  /** Buffers we've drawn decorations on — track for cleanup. */
  touchedBuffers: Set<number>;
  /** Active-split's primary cursor at activation, used as distance origin. */
  startCursor: number;
  startBufferId: number;
  startSplitId: number;
  priorMode: string | null;
}

const state: FlashState = {
  active: false,
  pattern: "",
  matches: [],
  touchedBuffers: new Set<number>(),
  startCursor: 0,
  startBufferId: 0,
  startSplitId: 0,
  priorMode: null,
};

// =============================================================================
// Byte-offset bookkeeping
// =============================================================================

// JS strings are UTF-16; the editor talks in UTF-8 byte offsets.  Build a
// once-per-frame lookup so substring matches translate to buffer byte
// offsets in O(1).  byteAt[i] is the byte offset of char i; byteAt has
// length = text.length + 1 so byteAt[text.length] is the total byte length.
function buildByteIndex(text: string): number[] {
  const out = new Array<number>(text.length + 1);
  out[0] = 0;
  for (let i = 0; i < text.length; i++) {
    const c = text.charCodeAt(i);
    let b: number;
    if (c < 0x80) b = 1;
    else if (c < 0x800) b = 2;
    else if (c >= 0xd800 && c <= 0xdbff) {
      // High surrogate of a 4-byte codepoint; the paired low surrogate
      // contributes 0 below.
      b = 4;
    } else if (c >= 0xdc00 && c <= 0xdfff) {
      b = 0;
    } else {
      b = 3;
    }
    out[i + 1] = out[i] + b;
  }
  return out;
}

// =============================================================================
// Viewport read (one snapshot per split)
// =============================================================================

interface ViewportSnapshot {
  text: string;
  topByte: number;
  byteAt: number[];
}

async function readSplitViewport(
  bufferId: number,
  topByte: number,
  width: number,
  height: number,
): Promise<ViewportSnapshot | null> {
  const bufLen = editor.getBufferLength(bufferId);
  // Over-read by a generous margin (height × (width+4)), capped at
  // buffer length.  Over-read is harmless: matches outside the actual
  // viewport just render off-screen and clearNamespace wipes them.
  const estEnd = Math.min(bufLen, topByte + (height + 2) * (width + 4));
  if (estEnd <= topByte) return null;
  const text = await editor.getBufferText(bufferId, topByte, estEnd);
  return { text, topByte, byteAt: buildByteIndex(text) };
}

async function readAllSplits(): Promise<SplitView[]> {
  const splits = editor.listSplits();
  const out: SplitView[] = [];
  for (const s of splits) {
    const snap = await readSplitViewport(
      s.bufferId,
      s.viewport.topByte,
      s.viewport.width,
      s.viewport.height,
    );
    if (snap) {
      out.push({ splitId: s.splitId, bufferId: s.bufferId, snap });
    }
  }
  return out;
}

// =============================================================================
// Matching (across every split)
// =============================================================================

function findMatchesInSplit(view: SplitView, pattern: string): Match[] {
  if (!pattern) return [];
  const out: Match[] = [];
  let from = 0;
  while (true) {
    const i = view.snap.text.indexOf(pattern, from);
    if (i < 0) break;
    out.push({
      start: view.snap.topByte + view.snap.byteAt[i],
      end: view.snap.topByte + view.snap.byteAt[i + pattern.length],
      charIdx: i,
      charEnd: i + pattern.length,
      bufferId: view.bufferId,
      splitId: view.splitId,
    });
    // Allow overlapping advances by one char so e.g. pattern "aa" in
    // "aaa" produces two matches; flash.nvim does the same.
    from = i + 1;
  }
  return out;
}

function findMatches(views: SplitView[], pattern: string): Match[] {
  const all: Match[] = [];
  for (const v of views) {
    for (const m of findMatchesInSplit(v, pattern)) {
      all.push(m);
    }
  }
  return all;
}

// Empty-pattern mode: label every visible word start.
//
// A "word start" is any alphanumeric / `_` char preceded by a non-word
// character (or sitting at the start of the viewport snapshot).  Each
// becomes a 1-char synthetic match anchored at the word's first letter
// — pressing the assigned label teleports the cursor to that word.
// This is the "no-filter, jump anywhere visible" mode that flash.nvim
// ships with `min_pattern_length = 0`.
function isWordChar(ch: number): boolean {
  return (
    (ch >= 0x30 && ch <= 0x39) || // 0-9
    (ch >= 0x41 && ch <= 0x5a) || // A-Z
    (ch >= 0x61 && ch <= 0x7a) || // a-z
    ch === 0x5f                   // _
  );
}

function findWordStartMatchesInSplit(view: SplitView): Match[] {
  const out: Match[] = [];
  const text = view.snap.text;
  let prevWord = false;
  for (let i = 0; i < text.length; i++) {
    const cur = isWordChar(text.charCodeAt(i));
    if (cur && !prevWord) {
      out.push({
        start: view.snap.topByte + view.snap.byteAt[i],
        end: view.snap.topByte + view.snap.byteAt[i + 1],
        charIdx: i,
        charEnd: i + 1,
        bufferId: view.bufferId,
        splitId: view.splitId,
      });
    }
    prevWord = cur;
  }
  return out;
}

function findWordStartMatches(views: SplitView[]): Match[] {
  const all: Match[] = [];
  for (const v of views) {
    for (const m of findWordStartMatchesInSplit(v)) {
      all.push(m);
    }
  }
  return all;
}

// =============================================================================
// Labeler — port of flash.nvim labeler.lua
// =============================================================================

// Build the set of label letters to skip:
//
//   - In **search mode** (non-empty pattern): every char that appears
//     immediately AFTER a visible match could be a valid pattern
//     continuation.  Pressing it must extend the pattern unambiguously,
//     so it can't also be a label.  Skip those letters.
//
//   - In **word-start mode** (empty pattern): every char that is the
//     FIRST letter of a visible word is reserved for "start a search
//     with this letter".  Pressing it must enter search mode, not jump.
//     Skip those.
//
// Returns the set of letters to remove from the label pool.
function buildSkipSet(
  matches: Match[],
  views: SplitView[],
  emptyPattern: boolean,
): Set<string> {
  const byBufferToText = new Map<number, string>();
  for (const v of views) byBufferToText.set(v.bufferId, v.snap.text);
  const skip = new Set<string>();
  for (const m of matches) {
    const text = byBufferToText.get(m.bufferId);
    if (!text) continue;
    const idx = emptyPattern ? m.charIdx : m.charEnd;
    if (idx < text.length) {
      const ch = text.charAt(idx);
      // Pool is lowercase only.  Skip the char and its lower-case form
      // — the conservative "case-sensitive labels never collide with
      // case-insensitive continuation" rule.
      skip.add(ch);
      skip.add(ch.toLowerCase());
    }
  }
  return skip;
}

// Sort matches with active-split-first ordering.  Within the active
// split, sort by byte distance from the start cursor (mimics
// flash.nvim's `distance = true`).  Other splits go after, ordered by
// byte position.  Ties are broken by start byte for determinism.
function sortMatches(
  matches: Match[],
  activeSplitId: number,
  startCursor: number,
): Match[] {
  return [...matches].sort((a, b) => {
    const aActive = a.splitId === activeSplitId ? 0 : 1;
    const bActive = b.splitId === activeSplitId ? 0 : 1;
    if (aActive !== bActive) return aActive - bActive;
    if (aActive === 0) {
      const da = Math.abs(a.start - startCursor);
      const db = Math.abs(b.start - startCursor);
      if (da !== db) return da - db;
    } else {
      if (a.splitId !== b.splitId) return a.splitId - b.splitId;
    }
    return a.start - b.start;
  });
}

function assignLabels(
  matches: Match[],
  views: SplitView[],
  startCursor: number,
  startSplitId: number,
  emptyPattern: boolean,
): Match[] {
  if (matches.length === 0) return matches;
  const skip = buildSkipSet(matches, views, emptyPattern);
  const pool: string[] = [];
  for (const c of LABEL_POOL) if (!skip.has(c)) pool.push(c);

  const sorted = sortMatches(matches, startSplitId, startCursor);
  for (let i = 0; i < sorted.length && i < pool.length; i++) {
    sorted[i].label = pool[i];
  }
  return sorted;
}

// =============================================================================
// Render
// =============================================================================

function clearTouched(): void {
  for (const buf of state.touchedBuffers) {
    editor.clearNamespace(buf, NS_MATCH);
    editor.removeVirtualTextsByPrefix(buf, VTEXT_PREFIX);
  }
  state.touchedBuffers.clear();
}

function redraw(matches: Match[]): void {
  // Clear last frame's decorations on every buffer we touched, then
  // repaint.  Flash never accumulates state across iterations.
  clearTouched();
  for (const m of matches) {
    state.touchedBuffers.add(m.bufferId);
    editor.addOverlay(m.bufferId, NS_MATCH, m.start, m.end, {
      bg: "search.match_bg",
      fg: "search.match_fg",
      bold: true,
    });
    if (m.label) {
      // Anchor the label at `position = m.end` with `before = true`
      // — renders in the gap right after the match (BeforeChar of
      // the first char past the match).  `before = false` would
      // render *after* that next char, off-by-one.
      //
      // Colours come from the theme via `search.label_bg` /
      // `search.label_fg` so the label automatically follows theme
      // changes and is high-contrast against the match's own bg.
      editor.addVirtualTextStyled(
        m.bufferId,
        VTEXT_PREFIX + String(m.bufferId) + ":" + String(m.start),
        m.end,
        m.label,
        {
          fg: "search.label_fg",
          bg: "search.label_bg",
          bold: true,
        },
        true, // before = true
      );
    }
  }
}

// =============================================================================
// Jump
// =============================================================================

function jumpTo(m: Match): void {
  if (m.splitId !== state.startSplitId) {
    editor.focusSplit(m.splitId);
  }
  editor.setBufferCursor(m.bufferId, m.start);
}

// =============================================================================
// Main loop
// =============================================================================

async function flashJump(): Promise<void> {
  if (state.active) return;

  const startBufferId = editor.getActiveBufferId();
  if (!startBufferId) return;
  const startCursor = editor.getCursorPosition();
  if (startCursor === null) return;
  const startSplitId = editor.getActiveSplitId();

  state.active = true;
  state.startBufferId = startBufferId;
  state.startSplitId = startSplitId;
  state.startCursor = startCursor;
  state.pattern = "";
  state.matches = [];
  state.touchedBuffers = new Set<number>();
  state.priorMode = editor.getEditorMode();

  editor.setEditorMode("flash");
  // Begin lossless key capture — keys typed between two `getNextKey()`
  // iterations are buffered and replayed in order.  Released in the
  // `finally` below.
  editor.beginKeyCapture();
  // Short status string — long enough to be informative, short
  // enough to survive status-bar truncation.  Includes the current
  // pattern so tests (and careful users) can confirm the plugin has
  // accepted each typed key.
  const setStatusForPattern = (): void => {
    editor.setStatus("Flash[" + state.pattern + "]");
  };
  setStatusForPattern();

  try {
    while (true) {
      const views = await readAllSplits();
      // Empty pattern → label every visible word start ("jump
      // anywhere" mode).  Non-empty pattern → label every literal
      // substring match.
      const emptyPattern = state.pattern.length === 0;
      const rawMatches = emptyPattern
        ? findWordStartMatches(views)
        : findMatches(views, state.pattern);
      state.matches = assignLabels(
        rawMatches,
        views,
        state.startCursor,
        state.startSplitId,
        emptyPattern,
      );
      redraw(state.matches);

      const ev = await editor.getNextKey();

      if (ev.key === "escape") break;

      if (ev.key === "enter") {
        // Jump to the first (closest, active-split-preferred) match.
        const target = state.matches[0];
        if (target) jumpTo(target);
        break;
      }

      if (ev.key === "backspace") {
        if (state.pattern.length > 0) {
          state.pattern = state.pattern.slice(0, -1);
        }
        setStatusForPattern();
        continue;
      }

      // Plain single-character key (no modifiers).  Could be a label
      // press or a pattern extension.
      if (ev.key.length === 1 && !ev.ctrl && !ev.alt && !ev.meta) {
        const hit = state.matches.find((m) => m.label === ev.key);
        if (hit) {
          jumpTo(hit);
          break;
        }
        state.pattern += ev.key;
        setStatusForPattern();
        continue;
      }

      // Anything else (arrow keys, function keys, modified keys) ends
      // the session without jumping — keeps the cursor at startCursor.
      break;
    }
  } finally {
    editor.endKeyCapture();
    clearTouched();
    editor.setEditorMode(state.priorMode);
    editor.setStatus("");
    state.active = false;
  }
}

registerHandler("flash_jump", flashJump);
editor.registerCommand(
  "Flash: Jump",
  "Jump to any visible match across every split",
  "flash_jump",
  null,
);
