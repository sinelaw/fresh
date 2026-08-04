/// <reference path="./fresh.d.ts" />

const editor = getEditor();

/** UTF-8 byte range inside one side of a paired line. */
export interface WordRange {
  start: number;
  end: number;
}

/** Shared token-alignment result for intraline diff rendering. */
export interface WordDiff {
  /** Delta-style normalized edit distance in the inclusive range 0..1. */
  distance: number;
  oldRanges: WordRange[];
  newRanges: WordRange[];
}

export interface WordDiffOptions {
  /** Skip the quadratic alignment when its table would exceed this size. */
  maxAlignmentCells?: number;
  /** Skip when either side contains more than this many tokens. */
  maxTokensPerSide?: number;
  /** Include unmatched whitespace in the returned highlight ranges. */
  includeWhitespace?: boolean;
  /**
   * Return every eligible token as changed when a limit is exceeded.
   * Otherwise return `null` so the caller can keep its whole-line fallback.
   */
  highlightAllOnLimit?: boolean;
  /** Group adjacent whitespace into one token instead of one code point each. */
  groupWhitespace?: boolean;
}

interface Token {
  text: string;
  byteStart: number;
  byteEnd: number;
  whitespace: boolean;
}

const WORD_CHAR = /\w/;
const WHITESPACE_CHAR = /\s/;
const ALIGN_MATCH = 0;
const ALIGN_DELETE = 1;
const ALIGN_INSERT = 2;
const ALIGN_EDIT_COST = 2;
const ALIGN_INITIAL_EDIT_PENALTY = 1;

/** UTF-8 byte length without a plugin-host bridge call. */
export function utf8ByteLength(text: string): number {
  let bytes = 0;
  for (let i = 0; i < text.length; i++) {
    const codeUnit = text.charCodeAt(i);
    if (codeUnit < 0x80) {
      bytes += 1;
    } else if (codeUnit < 0x800) {
      bytes += 2;
    } else if (codeUnit >= 0xd800 && codeUnit < 0xdc00 && i + 1 < text.length) {
      const next = text.charCodeAt(i + 1);
      if (next >= 0xdc00 && next < 0xe000) {
        bytes += 4;
        i += 1;
      } else {
        bytes += 3;
      }
    } else {
      bytes += 3;
    }
  }
  return bytes;
}

/**
 * Tokenize into word runs and individual punctuation code points. Whitespace
 * can be grouped for Live Diff's existing behavior or kept at code-point
 * granularity for delta-style Git Log alignment. Iterating code points keeps
 * surrogate pairs intact, while offsets remain bytes for overlay APIs.
 */
function tokenize(text: string, groupWhitespace: boolean): Token[] {
  const tokens: Token[] = [];
  let run = "";
  let runStart = 0;
  let runWhitespace = false;
  let bytePos = 0;

  const flushRun = (): void => {
    if (run.length === 0) return;
    tokens.push({
      text: run,
      byteStart: runStart,
      byteEnd: bytePos,
      whitespace: runWhitespace,
    });
    run = "";
  };

  for (const char of text) {
    const byteLength = utf8ByteLength(char);
    const isWord = WORD_CHAR.test(char);
    const isGroupedWhitespace = groupWhitespace && WHITESPACE_CHAR.test(char);
    if (isWord || isGroupedWhitespace) {
      const whitespace = !isWord;
      if (run.length > 0 && runWhitespace !== whitespace) flushRun();
      if (run.length === 0) {
        runStart = bytePos;
        runWhitespace = whitespace;
      }
      run += char;
    } else {
      flushRun();
      tokens.push({
        text: char,
        byteStart: bytePos,
        byteEnd: bytePos + byteLength,
        whitespace: WHITESPACE_CHAR.test(char),
      });
    }
    bytePos += byteLength;
  }
  flushRun();
  return tokens;
}

function collapseRanges(
  tokens: Token[],
  matched: boolean[],
  includeWhitespace: boolean,
): WordRange[] {
  const ranges: WordRange[] = [];
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    if (matched[i] || (!includeWhitespace && token.whitespace)) continue;
    const last = ranges[ranges.length - 1];
    if (last && last.end === token.byteStart) {
      last.end = token.byteEnd;
    } else {
      ranges.push({ start: token.byteStart, end: token.byteEnd });
    }
  }
  return ranges;
}

function allChanged(tokens: Token[], includeWhitespace: boolean): WordRange[] {
  return collapseRanges(
    tokens,
    new Array(tokens.length).fill(false) as boolean[],
    includeWhitespace,
  );
}

/**
 * Align two lines once for every intraline diff consumer.
 *
 * The dynamic program follows delta's `align.rs`/`edits.rs`: insertions and
 * deletions cost two, beginning a changed run costs one, and ties prefer an
 * insertion then a deletion. Returned ranges cover unmatched tokens; the
 * distance weights unchanged display columns twice, making a small edit to a
 * long line close to zero and a complete rewrite equal to one.
 */
export function computeWordDiff(
  oldText: string,
  newText: string,
  options: WordDiffOptions = {},
): WordDiff | null {
  const includeWhitespace = options.includeWhitespace ?? false;
  const oldTokens = tokenize(oldText, options.groupWhitespace ?? true);
  const newTokens = tokenize(newText, options.groupWhitespace ?? true);
  const rows = oldTokens.length + 1;
  const columns = newTokens.length + 1;
  const exceedsLimit =
    (options.maxAlignmentCells !== undefined &&
      rows * columns > options.maxAlignmentCells) ||
    (options.maxTokensPerSide !== undefined &&
      (oldTokens.length > options.maxTokensPerSide ||
        newTokens.length > options.maxTokensPerSide));
  if (exceedsLimit) {
    if (!options.highlightAllOnLimit) return null;
    return {
      distance: 1,
      oldRanges: allChanged(oldTokens, includeWhitespace),
      newRanges: allChanged(newTokens, includeWhitespace),
    };
  }

  const costs = new Uint32Array(rows * columns);
  const operations = new Uint8Array(rows * columns);
  const index = (i: number, j: number): number => i * columns + j;

  for (let i = 1; i < rows; i++) {
    costs[index(i, 0)] = i * ALIGN_EDIT_COST + ALIGN_INITIAL_EDIT_PENALTY;
    operations[index(i, 0)] = ALIGN_DELETE;
  }
  for (let j = 1; j < columns; j++) {
    costs[index(0, j)] = j * ALIGN_EDIT_COST + ALIGN_INITIAL_EDIT_PENALTY;
    operations[index(0, j)] = ALIGN_INSERT;
  }

  for (let i = 1; i < rows; i++) {
    for (let j = 1; j < columns; j++) {
      const insertionParent = index(i, j - 1);
      const deletionParent = index(i - 1, j);
      const diagonalParent = index(i - 1, j - 1);
      let bestOperation = ALIGN_INSERT;
      let bestCost = costs[insertionParent] + ALIGN_EDIT_COST +
        (operations[insertionParent] === ALIGN_MATCH
          ? ALIGN_INITIAL_EDIT_PENALTY
          : 0);
      const deletionCost = costs[deletionParent] + ALIGN_EDIT_COST +
        (operations[deletionParent] === ALIGN_MATCH
          ? ALIGN_INITIAL_EDIT_PENALTY
          : 0);
      if (deletionCost < bestCost) {
        bestCost = deletionCost;
        bestOperation = ALIGN_DELETE;
      }
      if (
        oldTokens[i - 1].text === newTokens[j - 1].text &&
        costs[diagonalParent] < bestCost
      ) {
        bestCost = costs[diagonalParent];
        bestOperation = ALIGN_MATCH;
      }
      const cell = index(i, j);
      costs[cell] = bestCost;
      operations[cell] = bestOperation;
    }
  }

  const matchedOld = new Array(oldTokens.length).fill(false) as boolean[];
  const matchedNew = new Array(newTokens.length).fill(false) as boolean[];
  let changedWidth = 0;
  let unchangedWidth = 0;
  let i = oldTokens.length;
  let j = newTokens.length;
  while (i > 0 || j > 0) {
    const operation = operations[index(i, j)];
    if (i > 0 && j > 0 && operation === ALIGN_MATCH) {
      i -= 1;
      j -= 1;
      matchedOld[i] = true;
      matchedNew[j] = true;
      unchangedWidth += editor.stringWidth(oldTokens[i].text.trim());
    } else if (i > 0 && (j === 0 || operation === ALIGN_DELETE)) {
      i -= 1;
      changedWidth += editor.stringWidth(oldTokens[i].text.trim());
    } else {
      j -= 1;
      changedWidth += editor.stringWidth(newTokens[j].text.trim());
    }
  }

  const denominator = changedWidth + 2 * unchangedWidth;
  return {
    distance: denominator === 0 ? 0 : changedWidth / denominator,
    oldRanges: collapseRanges(oldTokens, matchedOld, includeWhitespace),
    newRanges: collapseRanges(newTokens, matchedNew, includeWhitespace),
  };
}
