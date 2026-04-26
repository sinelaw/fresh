/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Flash Jump
 *
 * Label-based jump navigation, ported in spirit from flash.nvim.
 *
 *   1. User invokes the `Flash: Jump` command (Cmd+P → "Flash: Jump",
 *      or a custom binding to action `flash_jump`).
 *   2. Each character typed extends a literal-substring pattern. Every
 *      visible match in the active buffer's viewport is highlighted; a
 *      single-letter label is rendered after each match.
 *   3. Pressing a label moves the cursor to that match.  Pressing
 *      Backspace shrinks the pattern.  Pressing Enter jumps to the
 *      closest match.  Pressing Escape (or any non-character key)
 *      cancels and restores the prior cursor and mode.
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
  /** Byte offset where the match starts in the buffer. */
  start: number;
  /** Byte offset just past the end of the match. */
  end: number;
  /** Char index of the first match char in the viewport text snapshot. */
  charIdx: number;
  /** Char index just past the end of the match in the viewport text. */
  charEnd: number;
  /** Assigned label letter, or undefined when out of label pool. */
  label?: string;
}

interface FlashState {
  active: boolean;
  bufferId: number;
  pattern: string;
  matches: Match[];
  startCursor: number;
  priorMode: string | null;
}

const state: FlashState = {
  active: false,
  bufferId: 0,
  pattern: "",
  matches: [],
  startCursor: 0,
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
// Viewport read
// =============================================================================

interface ViewportSnapshot {
  text: string;
  topByte: number;
  byteAt: number[];
}

async function readViewport(bufferId: number): Promise<ViewportSnapshot | null> {
  const vp = editor.getViewport();
  if (!vp) return null;
  const bufLen = editor.getBufferLength(bufferId);
  // We don't know exact end-of-viewport byte offset without an extra
  // round-trip, so we over-read by a generous margin (height × (width+4),
  // capped at buffer length).  The over-read is harmless: matches outside
  // the actual viewport just render off-screen and the next clearNamespace
  // wipes them.
  const estEnd = Math.min(bufLen, vp.topByte + (vp.height + 2) * (vp.width + 4));
  if (estEnd <= vp.topByte) return null;
  const text = await editor.getBufferText(bufferId, vp.topByte, estEnd);
  return { text, topByte: vp.topByte, byteAt: buildByteIndex(text) };
}

// =============================================================================
// Matching
// =============================================================================

function findMatches(snap: ViewportSnapshot, pattern: string): Match[] {
  if (!pattern) return [];
  const out: Match[] = [];
  let from = 0;
  while (true) {
    const i = snap.text.indexOf(pattern, from);
    if (i < 0) break;
    out.push({
      start: snap.topByte + snap.byteAt[i],
      end: snap.topByte + snap.byteAt[i + pattern.length],
      charIdx: i,
      charEnd: i + pattern.length,
    });
    // Allow overlapping advances by one char so e.g. pattern "aa" in
    // "aaa" produces two matches; flash.nvim does the same.
    from = i + 1;
  }
  return out;
}

// =============================================================================
// Labeler — port of flash.nvim labeler.lua
// =============================================================================

// Sort by byte distance from cursor (closest first).
function sortByDistance(matches: Match[], cursor: number): Match[] {
  return [...matches].sort(
    (a, b) => Math.abs(a.start - cursor) - Math.abs(b.start - cursor),
  );
}

// Build the set of label letters to skip: every char that appears
// immediately after a visible match (and so could be a valid pattern
// extension).  Pressing such a letter must be unambiguous; if it is
// also a label, ambiguity.  Remove from the pool.
function buildSkipSet(matches: Match[], text: string): Set<string> {
  const skip = new Set<string>();
  for (const m of matches) {
    if (m.charEnd < text.length) {
      const next = text.charAt(m.charEnd);
      // Pool is lowercase only.  Skip the next-char and its lower-case
      // form; this is the conservative "case-sensitive labels never
      // collide with case-insensitive pattern extension" rule.
      skip.add(next);
      skip.add(next.toLowerCase());
    }
  }
  return skip;
}

function assignLabels(
  matches: Match[],
  snap: ViewportSnapshot,
  cursor: number,
): Match[] {
  if (matches.length === 0) return matches;
  const skip = buildSkipSet(matches, snap.text);
  const pool: string[] = [];
  for (const c of LABEL_POOL) if (!skip.has(c)) pool.push(c);

  const sorted = sortByDistance(matches, cursor);
  for (let i = 0; i < sorted.length && i < pool.length; i++) {
    sorted[i].label = pool[i];
  }
  return sorted;
}

// =============================================================================
// Render
// =============================================================================

function clearAll(bufferId: number): void {
  editor.clearNamespace(bufferId, NS_MATCH);
  editor.removeVirtualTextsByPrefix(bufferId, VTEXT_PREFIX);
}

function redraw(bufferId: number, matches: Match[]): void {
  clearAll(bufferId);
  for (const m of matches) {
    editor.addOverlay(bufferId, NS_MATCH, m.start, m.end, {
      bg: "search.match_bg",
      fg: "search.match_fg",
      bold: true,
    });
    if (m.label) {
      // RGB until plugin API #6 (theme-key support for addVirtualText).
      // Gold-on-dark is legible against typical themes.
      //
      // Anchor the label at `position = m.end` with `before = true`.
      // That renders in the gap immediately after the match (i.e.
      // BeforeChar of the first char past the match).  Using
      // `before = false` here would render *after* that next char,
      // off-by-one.
      editor.addVirtualText(
        bufferId,
        VTEXT_PREFIX + String(m.start),
        m.end,
        m.label,
        255, 215, 0,
        true,  // before = true → render in the gap right after the match
        true,  // useBg = true → label gets its own background
      );
    }
  }
}

// =============================================================================
// Main loop
// =============================================================================

async function flashJump(): Promise<void> {
  if (state.active) return;

  const bufferId = editor.getActiveBufferId();
  if (!bufferId) return;
  const startCursor = editor.getCursorPosition();
  if (startCursor === null) return;

  state.active = true;
  state.bufferId = bufferId;
  state.startCursor = startCursor;
  state.pattern = "";
  state.matches = [];
  state.priorMode = editor.getEditorMode();

  editor.setEditorMode("flash");
  // Begin lossless key capture: any keys typed between two
  // `getNextKey()` iterations (e.g. fast typing, paste-bursts, or
  // held-key auto-repeat) are buffered in-order rather than falling
  // through to the buffer.  Released in the `finally` below.
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
      const snap = await readViewport(bufferId);
      if (!snap) break;

      state.matches = state.pattern
        ? assignLabels(findMatches(snap, state.pattern), snap, state.startCursor)
        : [];
      redraw(bufferId, state.matches);

      const ev = await editor.getNextKey();

      if (ev.key === "escape") break;

      if (ev.key === "enter") {
        // Jump to the closest (first) match if any.
        const target = state.matches[0];
        if (target) editor.setBufferCursor(bufferId, target.start);
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
          editor.setBufferCursor(bufferId, hit.start);
          break;
        }
        state.pattern += ev.key;
        setStatusForPattern();
        continue;
      }

      // Anything else (arrow keys, function keys, modified keys) ends the
      // session without jumping — keeps the cursor at startCursor.
      break;
    }
  } finally {
    editor.endKeyCapture();
    clearAll(bufferId);
    editor.setEditorMode(state.priorMode);
    editor.setStatus("");
    state.active = false;
  }
}

registerHandler("flash_jump", flashJump);
editor.registerCommand(
  "Flash: Jump",
  "Jump to any visible match in the active buffer",
  "flash_jump",
  null,
);
