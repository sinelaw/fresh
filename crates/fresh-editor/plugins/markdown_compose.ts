/// <reference path="./lib/fresh.d.ts" />
// Markdown Compose Mode Plugin
// Provides compose mode for Markdown documents with:
// - Soft wrapping at a configurable width
// - Hanging indents for lists and block quotes
// - Centered margins
//
// Syntax highlighting is handled by the TextMate grammar (built-in to the editor)
// This plugin only adds the compose mode layout features.
const editor = getEditor();


interface MarkdownConfig {
  composeWidth: number | null;
  maxWidth: number;
  hideLineNumbers: boolean;
}

const config: MarkdownConfig = {
  composeWidth: null,
  maxWidth: 100,
  hideLineNumbers: true,
};

// When true, compose/preview mode is automatically enabled for all open and
// newly opened markdown buffers.  Toggled by the "Toggle Compose/Preview
// (All Files)" command.  Persisted across sessions via global plugin state.
function getGlobalComposeEnabled(): boolean {
  return (editor.getGlobalState("globalComposeEnabled") as boolean) ?? false;
}
function setGlobalComposeEnabled(value: boolean): void {
  editor.setGlobalState("globalComposeEnabled", value);
}

// Table column widths stored per-buffer-per-split via setViewState/getViewState.
// Persisted across sessions and independent per split.
interface TableWidthInfo {
  maxW: number[];
  allocated: number[];
  // True iff this row is the markdown source separator (`|---|---|---|`) — the
  // border code uses this to avoid drawing a duplicate `├─┼─┤` next to it.
  // Optional for backwards-compat with persisted view states from older
  // sessions.
  isSourceSep?: boolean;
}

// Helper: check whether the active split has compose mode for this buffer
function isComposing(bufferId: number): boolean {
  const info = editor.getBufferInfo(bufferId);
  return info != null && info.view_mode === "compose";
}

// Helper: check whether ANY split showing this buffer has compose mode.
// Use this for decoration maintenance (conceals, soft breaks, overlays) since
// decorations live on the buffer and are filtered per-split at render time.
function isComposingInAnySplit(bufferId: number): boolean {
  const info = editor.getBufferInfo(bufferId);
  return info != null && info.is_composing_in_any_split;
}

// ---------------------------------------------------------------------------
// Fenced-code-block context.
//
// The per-line pipeline (processLineConceals / processLineSoftBreaks /
// processTableAlignment) has no cross-line context, so without help it
// treats lines INSIDE ``` fences as markdown — e.g. a TypeScript union
// `| 'item_assigned'` grows table borders. We cache the byte ranges of all
// fenced blocks per buffer (rebuilt on enable and after edits) and skip
// markdown processing for any line inside one.
// ---------------------------------------------------------------------------
const fenceRangesByBuffer = new Map<number, Array<{ start: number; end: number }>>();

// UTF-8 byte length of a JS string (buffer offsets are UTF-8 bytes).
function utf8ByteLen(s: string): number {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.codePointAt(i) as number;
    if (c > 0xffff) i++; // surrogate pair consumed two UTF-16 units
    n += c <= 0x7f ? 1 : c <= 0x7ff ? 2 : c <= 0xffff ? 3 : 4;
  }
  return n;
}

// Rebuilds by reading the WHOLE buffer: fences are cross-line state, so a
// partial scan can't tell whether an edit opened or closed one. There is no
// size cap — markdown files are typically small, and the scan is a single
// linear string pass. If huge generated markdown ever makes edits feel slow,
// bail above a byte threshold here (stale ranges degrade gracefully: lines
// just get markdown styling they shouldn't).
async function rebuildFenceRanges(bufferId: number): Promise<void> {
  try {
    const len = editor.getBufferLength(bufferId);
    const text = await editor.getBufferText(bufferId, 0, len);
    if (typeof text !== "string") return;
    const ranges: Array<{ start: number; end: number }> = [];
    const lines = text.split("\n");
    let offset = 0;
    let openStart: number | null = null;
    let fenceChar = "";
    for (const line of lines) {
      const lineStart = offset;
      const lineEnd = lineStart + utf8ByteLen(line);
      offset = lineEnd + 1; // '\n'
      const trimmed = line.trim();
      if (openStart === null) {
        const m = trimmed.match(/^(```+|~~~+)/);
        if (m) {
          openStart = lineStart;
          fenceChar = m[1][0];
        }
      } else if (
        trimmed.startsWith(fenceChar.repeat(3)) &&
        trimmed.split(fenceChar).join("").trim() === ""
      ) {
        ranges.push({ start: openStart, end: lineEnd });
        openStart = null;
      }
    }
    if (openStart !== null) ranges.push({ start: openStart, end: offset });
    fenceRangesByBuffer.set(bufferId, ranges);
  } catch (_e) {
    /* keep the previous ranges on failure */
  }
}

// True if `byte` falls inside a cached fenced code block (the opening and
// closing fence lines themselves count as inside).
function insideFence(bufferId: number, byte: number): boolean {
  const ranges = fenceRangesByBuffer.get(bufferId);
  if (!ranges) return false;
  for (const r of ranges) {
    if (byte >= r.start && byte <= r.end) return true;
  }
  return false;
}

// Helper: get cached table column widths from per-buffer-per-split view state
function getTableWidths(bufferId: number): Map<number, TableWidthInfo> | undefined {
  const obj = editor.getViewState(bufferId, "table-widths") as Record<string, { maxW: number[]; allocated: number[] }> | undefined;
  if (!obj || typeof obj !== "object") return undefined;
  const map = new Map<number, TableWidthInfo>();
  for (const [k, v] of Object.entries(obj)) {
    map.set(parseInt(k, 10), v);
  }
  return map;
}

// Helper: store cached table column widths in per-buffer-per-split view state
function setTableWidths(bufferId: number, widthMap: Map<number, TableWidthInfo>): void {
  const obj: Record<string, TableWidthInfo> = {};
  for (const [k, v] of widthMap) {
    obj[String(k)] = v;
  }
  editor.setViewState(bufferId, "table-widths", obj);
}

// Helper: clear cached table column widths
function clearTableWidths(bufferId: number): void {
  editor.setViewState(bufferId, "table-widths", null);
}

// Static map of named HTML entities to their Unicode replacements
const HTML_ENTITY_MAP: Record<string, string> = {
  nbsp: "\u00A0", amp: "&", lt: "<", gt: ">", mdash: "\u2014", ndash: "\u2013",
  hellip: "\u2026", rsquo: "\u2019", lsquo: "\u2018", rdquo: "\u201D", ldquo: "\u201C",
  bull: "\u2022", middot: "\u00B7", copy: "\u00A9", reg: "\u00AE", trade: "\u2122",
  times: "\u00D7", divide: "\u00F7", plusmn: "\u00B1", deg: "\u00B0",
  frac12: "\u00BD", frac14: "\u00BC", rarr: "\u2192", larr: "\u2190",
  harr: "\u2194", uarr: "\u2191", darr: "\u2193", euro: "\u20AC", pound: "\u00A3",
  yen: "\u00A5", cent: "\u00A2", sect: "\u00A7", para: "\u00B6",
  laquo: "\u00AB", raquo: "\u00BB", ensp: "\u2002", emsp: "\u2003", thinsp: "\u2009",
};

// =============================================================================
// Table border virtual lines (top/bottom + inter-row separators)
// =============================================================================
//
// Markdown tables source-encode only an underline-style separator between the
// header and the first data row.  In compose mode we already conceal the
// pipe characters into Unicode box-drawing (`│`, `├`, `┼`, `┤`).  This module
// adds the *missing* visual frame: a `┌─┬─┐` top border above the header,
// `├─┼─┤` separators between consecutive data rows (so each row reads as a
// distinct cell), and a `└─┴─┘` bottom border below the last row.
//
// Implementation:
//
//   * Borders are virtual lines (no source bytes), keyed per-line via a
//     unique namespace `md-tb-${lineNumber}`.  The namespace lets us
//     clear+rebuild borders for one row without disturbing other tables.
//   * "First/last/source-separator" classification is derived from the
//     cached widthMap (a row is "known" iff it has a TableWidthInfo entry).
//     This is cheap and stable across scrolls because widthMap accumulates.
//   * Border column widths come from the same `allocated` widths used by
//     processLineConceals, so the borders line up exactly with the cell
//     conceals.

/** Build a horizontal table border line of the given style for a row. */
function buildTableBorderLine(
  allocated: number[],
  left: string,
  mid: string,
  right: string,
): string {
  // Each cell render is `│ <text padded to allocated[i] - 2> │` (2 chars of
  // inside padding).  The matching border slot must therefore be
  // `allocated[i]` wide of `─` characters between the corner/junction marks.
  const parts: string[] = [];
  for (let i = 0; i < allocated.length; i++) {
    const fill = "─".repeat(Math.max(1, allocated[i]));
    parts.push(fill);
  }
  return left + parts.join(mid) + right;
}

/** True if `lineContent` looks like a markdown table separator row. */
function isTableSeparatorContent(lineContent: string): boolean {
  return /^\|[-:\s|]+\|$/.test(lineContent.trim());
}

/** True if the `|` at char offset `i` is escaped (`\|`) — i.e. preceded by an
 *  odd run of backslashes. Escaped pipes are cell *content*, not column
 *  separators, and must not split the row. */
function isEscapedPipe(line: string, i: number): boolean {
  let bs = 0;
  for (let j = i - 1; j >= 0 && line[j] === '\\'; j--) bs++;
  return bs % 2 === 1;
}

/** Char offsets of every *unescaped* `|` in `line` (the column separators).
 *  Escaped pipes (`\|`) are skipped — they render as a literal `|` inside a
 *  cell rather than ending it. */
function tablePipePositions(line: string): number[] {
  const pos: number[] = [];
  for (let i = 0; i < line.length; i++) {
    if (line[i] === '|' && !isEscapedPipe(line, i)) pos.push(i);
  }
  return pos;
}

/** Strip a table row's outer border pipes, leaving the inner cell text. A
 *  trailing `|` that is escaped (`\|`) is cell content, not a border. */
function tableRowInner(trimmed: string): string {
  let inner = trimmed;
  if (inner.startsWith('|')) inner = inner.slice(1);
  if (inner.endsWith('|') && !isEscapedPipe(inner, inner.length - 1)) {
    inner = inner.slice(0, -1);
  }
  return inner;
}

/** Split a table row's inner text (outer pipes already stripped via
 *  `tableRowInner`) into cells on *unescaped* pipes, unescaping `\|` → `|`
 *  within each cell so the literal pipe renders as one character. */
function splitTableCells(inner: string): string[] {
  const cells: string[] = [];
  let cur = '';
  for (let i = 0; i < inner.length; i++) {
    const ch = inner[i];
    if (ch === '\\' && inner[i + 1] === '|') {
      cur += '|';
      i++;
      continue;
    }
    if (ch === '|') {
      cells.push(cur);
      cur = '';
      continue;
    }
    cur += ch;
  }
  cells.push(cur);
  return cells;
}

/**
 * Wrap a markdown table data row into per-column visual-line fragments.
 *
 * `colWidths[i]` is the *total* allocated cell width (including the two
 * inside-padding spaces), matching `buildTableBorderLine`. Each column's text
 * is word-wrapped to `colWidths[i] - 2` cells. Returns the per-column fragment
 * arrays plus `maxVisualLines` — the number of stacked visual rows this row
 * occupies (the tallest column). Cells are read through `concealedText` (so
 * emphasis/link markers are hidden) unless `raw` is set.
 *
 * This is the single source of truth shared by the first-visual-line conceal
 * (`processLineConceals`) and the continuation virtual lines
 * (`processTableBorders`), so the two never disagree about row height.
 */
function wrapTableRow(
  lineContent: string,
  colWidths: number[],
  raw: boolean,
): { cellWrapped: string[][]; numCols: number; maxVisualLines: number } {
  const cells = splitTableCells(tableRowInner(lineContent.trim()));
  const numCols = Math.min(cells.length, colWidths.length);
  const cellWrapped: string[][] = [];
  let maxVisualLines = 1;
  for (let ci = 0; ci < numCols; ci++) {
    const cellText = (raw ? cells[ci] : concealedText(cells[ci])).trim();
    const wrapW = Math.max(1, colWidths[ci] - 2);
    const wrapped = wrapText(cellText, wrapW);
    cellWrapped.push(wrapped);
    maxVisualLines = Math.max(maxVisualLines, wrapped.length);
  }
  return { cellWrapped, numCols, maxVisualLines };
}

/**
 * Render visual row `vl` of a wrapped table row as `│ c0 │ c1 │ … │`, each
 * column padded to its allocated width. Columns with no fragment at `vl`
 * render as blank padding, so a short column sits empty while a tall prose
 * column keeps wrapping — exactly how a rendered README table lays out.
 */
function buildTableRowVisualLine(
  cellWrapped: string[][],
  colWidths: number[],
  numCols: number,
  vl: number,
): string {
  let line = '│';
  for (let ci = 0; ci < numCols; ci++) {
    const wrapW = Math.max(1, colWidths[ci] - 2);
    const frag = (cellWrapped[ci] && cellWrapped[ci][vl]) || '';
    line += ' ' + frag + ' '.repeat(Math.max(0, wrapW - displayWidth(frag))) + ' │';
  }
  return line;
}

/** Re-emit the table border virtual lines for the given table-row group.
 *
 * Detects the group's first/last visible rows by consulting `widthMap`
 * (which is updated by `processTableAlignment` before this runs).  A row at
 * `lineNumber - 1` or `lineNumber + 1` that is *not* in `widthMap` is treated
 * as the boundary of the table's visible extent.
 */
function processTableBorders(
  bufferId: number,
  lines: Array<{
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
  }>,
  widthMap: Map<number, TableWidthInfo>,
  cursors: number[],
): void {
  // Use theme keys (resolved at render time so the borders follow theme
  // changes — same pattern as addOverlay's fg/bg options).
  //
  //   * fg → editor.fg (the default document foreground, matching the
  //     concealed `│` / `─` glyphs inside row text so the virtual
  //     `┌─┬─┐` / `├─┼─┤` / `└─┴─┘` frame doesn't create a visible seam
  //     where it meets the in-text borders)
  //   * bg → editor.bg (matches the document background so the borders
  //     blend in rather than carving an opaque slab through the page)
  const borderOptions = { fg: "editor.fg", bg: "editor.bg" };

  for (const line of lines) {
    const ns = `md-tb-${line.line_number}`;
    // Always start by clearing this row's previous borders (handles
    // edits that removed/widened the row, scrolls that change the
    // first/last classification, etc.).
    editor.clearVirtualTextNamespace(bufferId, ns);

    const trimmed = line.content.trim();
    const isTableRow =
      (trimmed.startsWith("|") || trimmed.endsWith("|")) &&
      !insideFence(bufferId, line.byte_start);
    if (!isTableRow) continue;

    const widthInfo = widthMap.get(line.line_number);
    if (!widthInfo || widthInfo.allocated.length === 0) continue;

    const allocated = widthInfo.allocated;
    // Prefer the cached flag (set by processTableAlignment from the source
    // text of this exact row); fall back to a regex check in case this row
    // was loaded from a persisted view state without the flag.
    const isSourceSep = widthInfo.isSourceSep === true
      || isTableSeparatorContent(line.content);

    const prevIsTable = widthMap.has(line.line_number - 1);
    const nextIsTable = widthMap.has(line.line_number + 1);

    // Top border: only above the very first known row of the table.
    // ┌─┬─┐ — opens the frame above the header.
    if (!prevIsTable) {
      editor.addVirtualLine(
        bufferId,
        line.byte_start,
        buildTableBorderLine(allocated, "┌", "┬", "┐"),
        borderOptions,
        true, // above
        ns,
        0,
      );
    }

    // Inter-row separator: between consecutive *data* rows.
    //
    // Skip if either side is the source separator row (`|---|---|---|`)
    // because the source already provides `├─┼─┤` there via conceals —
    // adding another above/below would draw two adjacent separator lines.
    //
    // Drawn ABOVE the current row when its predecessor is also a (non-
    // source-separator) table row, so each row owns the separator that
    // appears above it.
    const prevInfo = widthMap.get(line.line_number - 1);
    const prevIsSourceSep = prevInfo?.isSourceSep === true;
    if (prevIsTable && !isSourceSep && !prevIsSourceSep) {
      editor.addVirtualLine(
        bufferId,
        line.byte_start,
        buildTableBorderLine(allocated, "├", "┼", "┤"),
        borderOptions,
        true, // above
        ns,
        1,
      );
    }

    // Wrapped-cell continuation lines: the row's first visual line is rendered
    // in place (conceals in processLineConceals); visual lines 2..N stack below
    // the row as virtual lines. Suppressed on the source-separator row and
    // while the cursor is on the row (it shows raw source for editing). The
    // ascending priority `vl` keeps them ordered, below the row but above the
    // high-priority bottom border emitted next.
    const cursorOnRow = cursors.some(c => c >= line.byte_start && c < line.byte_end);
    if (!isSourceSep && !cursorOnRow) {
      const { cellWrapped, numCols, maxVisualLines } =
        wrapTableRow(line.content, allocated, false);
      if (maxVisualLines > 1) {
        const anchor = Math.max(line.byte_start, line.byte_end - 1);
        for (let vl = 1; vl < maxVisualLines; vl++) {
          editor.addVirtualLine(
            bufferId,
            anchor,
            buildTableRowVisualLine(cellWrapped, allocated, numCols, vl),
            borderOptions,
            false, // below
            ns,
            vl,
          );
        }
      }
    }

    // Bottom border: only below the last known row of the table.
    // └─┴─┘ — closes the frame.  Anchor at the END of the row's bytes
    // (one before the trailing newline) and place "below". Priority is high
    // so it renders after any wrapped-cell continuation lines above it.
    if (!nextIsTable) {
      // byte_end points just past the newline; anchor at last byte of
      // the row content so the virtual line renders directly under it.
      const anchor = Math.max(line.byte_start, line.byte_end - 1);
      editor.addVirtualLine(
        bufferId,
        anchor,
        buildTableBorderLine(allocated, "└", "┴", "┘"),
        borderOptions,
        false, // below
        ns,
        1000,
      );
    }
  }
}

// =============================================================================
// Block-based parser for hanging indent support
// =============================================================================

interface ParsedBlock {
  type: 'paragraph' | 'list-item' | 'ordered-list' | 'checkbox' | 'blockquote' |
        'heading' | 'code-fence' | 'code-content' | 'hr' | 'empty' | 'image' |
        'table-row';
  startByte: number;           // First byte of the line
  endByte: number;             // Byte after last char (before newline)
  leadingIndent: number;       // Spaces before marker/content
  marker: string;              // "- ", "1. ", "> ", "## ", etc.
  markerStartByte: number;     // Where marker begins
  contentStartByte: number;    // Where content begins (after marker)
  content: string;             // The actual text content (after marker)
  hangingIndent: number;       // Continuation indent for wrapped lines
  forceHardBreak: boolean;     // Should this block end with hard newline?
  headingLevel?: number;       // For headings (1-6)
  checked?: boolean;           // For checkboxes
}

/**
 * Parse a markdown document into blocks with structure info for wrapping
 */
function parseMarkdownBlocks(text: string): ParsedBlock[] {
  const blocks: ParsedBlock[] = [];
  const lines = text.split('\n');
  let byteOffset = 0;
  let inCodeBlock = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Code block detection
    const trimmed = line.trim();
    if (trimmed.startsWith('```')) {
      inCodeBlock = !inCodeBlock;
      blocks.push({
        type: 'code-fence',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent: line.length - line.trimStart().length,
        marker: '',
        markerStartByte: lineStart,
        contentStartByte: lineStart,
        content: line,
        hangingIndent: 0,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    if (inCodeBlock) {
      blocks.push({
        type: 'code-content',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent: 0,
        marker: '',
        markerStartByte: lineStart,
        contentStartByte: lineStart,
        content: line,
        hangingIndent: 0,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Empty line
    if (trimmed.length === 0) {
      blocks.push({
        type: 'empty',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent: 0,
        marker: '',
        markerStartByte: lineStart,
        contentStartByte: lineStart,
        content: '',
        hangingIndent: 0,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Headers: # Heading
    const headerMatch = line.match(/^(\s*)(#{1,6})\s+(.*)$/);
    if (headerMatch) {
      const leadingIndent = headerMatch[1].length;
      const marker = headerMatch[2] + ' ';
      const content = headerMatch[3];
      blocks.push({
        type: 'heading',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent,
        marker,
        markerStartByte: lineStart + leadingIndent,
        contentStartByte: lineStart + leadingIndent + marker.length,
        content,
        hangingIndent: 0,
        forceHardBreak: true,
        headingLevel: headerMatch[2].length,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Horizontal rule
    if (trimmed.match(/^(-{3,}|\*{3,}|_{3,})$/)) {
      blocks.push({
        type: 'hr',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent: line.length - line.trimStart().length,
        marker: '',
        markerStartByte: lineStart,
        contentStartByte: lineStart,
        content: line,
        hangingIndent: 0,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Checkbox: - [ ] or - [x]
    const checkboxMatch = line.match(/^(\s*)([-*+])\s+(\[[ x]\])\s+(.*)$/);
    if (checkboxMatch) {
      const leadingIndent = checkboxMatch[1].length;
      const bullet = checkboxMatch[2];
      const checkbox = checkboxMatch[3];
      const marker = bullet + ' ' + checkbox + ' ';
      const content = checkboxMatch[4];
      const checked = checkbox === '[x]';
      blocks.push({
        type: 'checkbox',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent,
        marker,
        markerStartByte: lineStart + leadingIndent,
        contentStartByte: lineStart + leadingIndent + marker.length,
        content,
        hangingIndent: leadingIndent + marker.length,
        forceHardBreak: true,
        checked,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Unordered list: - item or * item or + item
    const bulletMatch = line.match(/^(\s*)([-*+])\s+(.*)$/);
    if (bulletMatch) {
      const leadingIndent = bulletMatch[1].length;
      const bullet = bulletMatch[2];
      const marker = bullet + ' ';
      const content = bulletMatch[3];
      blocks.push({
        type: 'list-item',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent,
        marker,
        markerStartByte: lineStart + leadingIndent,
        contentStartByte: lineStart + leadingIndent + marker.length,
        content,
        hangingIndent: leadingIndent + marker.length,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Ordered list: 1. item
    const orderedMatch = line.match(/^(\s*)(\d+\.)\s+(.*)$/);
    if (orderedMatch) {
      const leadingIndent = orderedMatch[1].length;
      const number = orderedMatch[2];
      const marker = number + ' ';
      const content = orderedMatch[3];
      blocks.push({
        type: 'ordered-list',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent,
        marker,
        markerStartByte: lineStart + leadingIndent,
        contentStartByte: lineStart + leadingIndent + marker.length,
        content,
        hangingIndent: leadingIndent + marker.length,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Block quote: > text
    const quoteMatch = line.match(/^(\s*)(>)\s*(.*)$/);
    if (quoteMatch) {
      const leadingIndent = quoteMatch[1].length;
      const marker = '> ';
      const content = quoteMatch[3];
      blocks.push({
        type: 'blockquote',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent,
        marker,
        markerStartByte: lineStart + leadingIndent,
        contentStartByte: lineStart + leadingIndent + 2, // "> " is 2 chars
        content,
        hangingIndent: leadingIndent + 2,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Image: ![alt](url)
    if (trimmed.match(/^!\[.*\]\(.*\)$/)) {
      blocks.push({
        type: 'image',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent: line.length - line.trimStart().length,
        marker: '',
        markerStartByte: lineStart,
        contentStartByte: lineStart,
        content: line,
        hangingIndent: 0,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Table row: | cell | cell | or separator |---|---|
    if (trimmed.startsWith('|') || trimmed.endsWith('|')) {
      blocks.push({
        type: 'table-row',
        startByte: lineStart,
        endByte: lineEnd,
        leadingIndent: line.length - line.trimStart().length,
        marker: '',
        markerStartByte: lineStart,
        contentStartByte: lineStart,
        content: line,
        hangingIndent: 0,
        forceHardBreak: true,
      });
      byteOffset = lineEnd + 1;
      continue;
    }

    // Hard break (trailing spaces or backslash)
    const hasHardBreak = line.endsWith('  ') || line.endsWith('\\');

    // Default: paragraph
    const leadingIndent = line.length - line.trimStart().length;
    blocks.push({
      type: 'paragraph',
      startByte: lineStart,
      endByte: lineEnd,
      leadingIndent,
      marker: '',
      markerStartByte: lineStart + leadingIndent,
      contentStartByte: lineStart + leadingIndent,
      content: trimmed,
      hangingIndent: leadingIndent,  // Paragraph continuation aligns with first line
      forceHardBreak: hasHardBreak,
    });
    byteOffset = lineEnd + 1;
  }

  return blocks;
}

// Check if a file is a markdown file
function isMarkdownFile(path: string): boolean {
  return path.endsWith('.md') || path.endsWith('.markdown');
}


// Enable full compose mode for a buffer (explicit toggle or restore from session).
// Idempotent: safe to call when already in compose mode (re-applies line numbers,
// line wrap, and layout hints — needed after session restore where Rust already has
// ViewMode::Compose but the plugin hasn't applied its settings yet).
function enableMarkdownCompose(bufferId: number): void {
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  // Tell Rust side this buffer is in compose mode (idempotent)
  editor.setViewMode(bufferId, "compose");

  // Hide line numbers in compose mode
  editor.setLineNumbers(bufferId, false);

  // Enable native line wrapping so that long lines without whitespace
  // (which the plugin can't soft-break) are force-wrapped by the Rust
  // wrapping transform at the content width.
  editor.setLineWrap(bufferId, null, true);

  // Set layout hints for centered margins
  editor.setLayoutHints(bufferId, null, { composeWidth: config.composeWidth ?? undefined });

  // Two refreshes, deliberately: the synchronous one below paints compose
  // mode immediately (no blank frame while the buffer read awaits); the one
  // chained on rebuildFenceRanges repaints once the fence cache exists, fixing
  // any lines the first pass styled as markdown that are actually inside a
  // fence. Dropping the first refresh trades a visible delay for the flicker;
  // dropping the second leaves fence interiors mis-styled until the next edit.
  void rebuildFenceRanges(bufferId).then(() => editor.refreshLines(bufferId));
  editor.refreshLines(bufferId);
  editor.debug(`Markdown compose enabled for buffer ${bufferId}`);
}

// Disable compose mode for a buffer
function disableMarkdownCompose(bufferId: number): void {
  if (isComposing(bufferId)) {
    editor.setViewState(bufferId, "last-cursor-line", null);
    clearTableWidths(bufferId);

    // Tell Rust side this buffer is back in source mode
    editor.setViewMode(bufferId, "source");

    // Re-enable line numbers
    editor.setLineNumbers(bufferId, true);

    // Clear layout hints, emphasis overlays, conceals, and soft breaks
    editor.setLayoutHints(bufferId, null, {});
    editor.clearNamespace(bufferId, "md-emphasis");
    editor.clearConcealNamespace(bufferId, "md-syntax");
    editor.clearSoftBreakNamespace(bufferId, "md-wrap");
    fenceRangesByBuffer.delete(bufferId);

    editor.refreshLines(bufferId);
    editor.debug(`Markdown compose disabled for buffer ${bufferId}`);
  }
}

// Toggle markdown compose mode for current buffer
function markdownToggleCompose() : void {
  const bufferId = editor.getActiveBufferId();
  const info = editor.getBufferInfo(bufferId);

  if (!info) return;

  // Only work with markdown files
  if (!info.path.endsWith('.md') && !info.path.endsWith('.markdown')) {
    editor.setStatus(editor.t("status.not_markdown_file"));
    return;
  }

  if (isComposing(bufferId)) {
    disableMarkdownCompose(bufferId);
    editor.setStatus(editor.t("status.compose_off"));
  } else {
    enableMarkdownCompose(bufferId);
    // Trigger a re-render to apply the transform
    editor.refreshLines(bufferId);
    editor.setStatus(editor.t("status.compose_on"));
  }
}
registerHandler("markdownToggleCompose", markdownToggleCompose);

// Toggle compose/preview mode for ALL open (and future) markdown buffers.
function markdownToggleComposeAll(): void {
  const newValue = !getGlobalComposeEnabled();
  setGlobalComposeEnabled(newValue);

  const buffers = editor.listBuffers();
  for (const buf of buffers) {
    if (!isMarkdownFile(buf.path)) continue;

    if (newValue) {
      enableMarkdownCompose(buf.id);
    } else {
      disableMarkdownCompose(buf.id);
    }
  }

  if (newValue) {
    editor.setStatus(editor.t("status.compose_all_on"));
  } else {
    editor.setStatus(editor.t("status.compose_all_off"));
  }
}
registerHandler("markdownToggleComposeAll", markdownToggleComposeAll);

/**
 * Extract text content from incoming tokens
 * Reconstructs the source text from ViewTokenWire tokens
 */
function extractTextFromTokens(tokens: ViewTokenWire[]): string {
  let text = '';
  for (const token of tokens) {
    const kind = token.kind;
    if (kind === "Newline") {
      text += '\n';
    } else if (kind === "Space") {
      text += ' ';
    } else if (kind === "Break") {
      // Soft break, ignore for text extraction
    } else if (typeof kind === 'object' && 'Text' in kind) {
      text += kind.Text;
    }
  }
  return text;
}

/**
 * Transform tokens for markdown compose mode with hanging indents
 *
 * Strategy: Parse the source text to identify block structure, then walk through
 * incoming tokens and emit transformed tokens with soft wraps and hanging indents.
 */
function transformMarkdownTokens(
  inputTokens: ViewTokenWire[],
  width: number,
  viewportStart: number
): ViewTokenWire[] {
  // First, extract text to understand block structure
  const text = extractTextFromTokens(inputTokens);
  const blocks = parseMarkdownBlocks(text);

  // Build a map of source_offset -> block info for quick lookup
  // Block byte positions are 0-based within extracted text
  // Source offsets are actual buffer positions (viewportStart + position_in_text)
  const offsetToBlock = new Map<number, ParsedBlock>();
  for (const block of blocks) {
    // Map byte positions that fall within this block to the block
    // contentStartByte and endByte are positions within extracted text (0-based)
    // source_offset = viewportStart + position_in_extracted_text
    for (let textPos = block.startByte; textPos < block.endByte; textPos++) {
      const sourceOffset = viewportStart + textPos;
      offsetToBlock.set(sourceOffset, block);
    }
  }

  const outputTokens: ViewTokenWire[] = [];
  let column = 0;  // Current column position
  let currentBlock: ParsedBlock | null = null;
  let lineStarted = false;  // Have we output anything on current line?

  for (let i = 0; i < inputTokens.length; i++) {
    const token = inputTokens[i];
    const kind = token.kind;
    const sourceOffset = token.source_offset;

    // Track which block we're in based on source offset
    if (sourceOffset !== null) {
      const block = offsetToBlock.get(sourceOffset);
      if (block) {
        currentBlock = block;
      }
    }

    // Get hanging indent for current block (default 0)
    const hangingIndent = currentBlock?.hangingIndent ?? 0;

    // Determine if current block should be soft-wrapped
    const blockType = currentBlock?.type;
    const noWrap = blockType === 'table-row' || blockType === 'code-fence' ||
                   blockType === 'code-content' || blockType === 'hr' ||
                   blockType === 'heading' || blockType === 'image' ||
                   blockType === 'empty';

    // Handle different token types
    if (kind === "Newline") {
      // Real newlines pass through - they end a block
      outputTokens.push(token);
      column = 0;
      lineStarted = false;
      currentBlock = null;  // Reset at line boundary
    } else if (kind === "Space") {
      // Space handling - potentially wrap before space + next word
      if (!lineStarted) {
        // Leading space on a line - preserve it
        outputTokens.push(token);
        column++;
        lineStarted = true;
      } else {
        // Mid-line space - look ahead to see if we need to wrap
        // Find next non-space token to check word length
        let nextWordLen = 0;
        for (let j = i + 1; j < inputTokens.length; j++) {
          const nextKind = inputTokens[j].kind;
          if (nextKind === "Space" || nextKind === "Newline" || nextKind === "Break") {
            break;
          }
          if (typeof nextKind === 'object' && 'Text' in nextKind) {
            nextWordLen += nextKind.Text.length;
          }
        }

        // Check if space + next word would exceed width
        if (!noWrap && column + 1 + nextWordLen > width && nextWordLen > 0) {
          // Wrap: emit soft newline + hanging indent instead of space
          outputTokens.push({ source_offset: null, kind: "Newline" });
          for (let j = 0; j < hangingIndent; j++) {
            outputTokens.push({ source_offset: null, kind: "Space" });
          }
          column = hangingIndent;
          // Don't emit the space - we wrapped instead
        } else {
          // No wrap needed - emit the space normally
          outputTokens.push(token);
          column++;
        }
      }
    } else if (kind === "Break") {
      // Existing soft breaks - we're replacing wrapping logic, so skip these
      // and handle wrapping ourselves
    } else if (typeof kind === 'object' && 'Text' in kind) {
      const text = kind.Text;

      if (!lineStarted) {
        lineStarted = true;
      }

      // Check if this word alone would exceed width (need to wrap)
      if (!noWrap && column > hangingIndent && column + text.length > width) {
        // Wrap before this word
        outputTokens.push({ source_offset: null, kind: "Newline" });
        for (let j = 0; j < hangingIndent; j++) {
          outputTokens.push({ source_offset: null, kind: "Space" });
        }
        column = hangingIndent;
      }

      // Emit the text token
      outputTokens.push(token);
      column += text.length;
    } else {
      // Unknown token type - pass through
      outputTokens.push(token);
    }
  }

  return outputTokens;
}

// =============================================================================
// Line-level conceal/overlay processing
// =============================================================================
// Conceals and overlays are managed per-line using targeted range-based clearing.
// The lines_changed hook processes newly visible or edited lines.
// The after_insert/after_delete hooks clear affected byte ranges.
// The view_transform_request hook handles cursor-aware reveal/conceal updates
// and soft wrapping.

/**
 * Convert a char offset within lineContent to a buffer byte offset.
 * Handles UTF-8 multi-byte characters correctly.
 */
function charToByte(lineContent: string, charOffset: number, lineByteStart: number): number {
  return lineByteStart + editor.utf8ByteLength(lineContent.slice(0, charOffset));
}

// ---------------------------------------------------------------------------
// Shared inline span detection — used by both processLineConceals (to apply
// conceals + overlays) and concealedText (to compute visible table widths).
// ---------------------------------------------------------------------------

// Superscript form of a footnote label: numeric labels map to Unicode
// superscript digits (¹, ²³, …); non-numeric labels keep a compact caret
// form (^note) since most of the alphabet has no superscript codepoint.
const SUPERSCRIPT_DIGITS = "⁰¹²³⁴⁵⁶⁷⁸⁹";
function superscriptLabel(label: string): string {
  let out = "";
  for (const ch of label) {
    if (ch < "0" || ch > "9") return "^" + label;
    out += SUPERSCRIPT_DIGITS[ch.charCodeAt(0) - 48];
  }
  return out;
}

interface InlineSpan {
  type: 'code' | 'bold-italic' | 'bold' | 'italic' | 'strikethrough' | 'link' | 'entity' | 'footnote';
  matchStart: number;    // char offset of full match start
  matchEnd: number;      // char offset of full match end
  contentStart: number;  // char offset of visible content start
  contentEnd: number;    // char offset of visible content end
  concealRanges: Array<{start: number; end: number; replacement: string | null}>;
  linkUrl?: string;
}

/** Find all inline spans that would produce conceals in the given text. */
function findInlineSpans(text: string): InlineSpan[] {
  const spans: InlineSpan[] = [];
  let m: RegExpExecArray | null;

  // 1. Code spans (also builds exclusion set)
  const codeSpanCharRanges: [number, number][] = [];
  const codeRe = /(?<!`)(`)((?:[^`]|(?<=\\)`)+)\1(?!`)/g;
  while ((m = codeRe.exec(text)) !== null) {
    const ms = m.index;
    const me = ms + m[0].length;
    codeSpanCharRanges.push([ms, me]);
    spans.push({
      type: 'code',
      matchStart: ms, matchEnd: me,
      contentStart: ms + 1, contentEnd: me - 1,
      concealRanges: [
        { start: ms, end: ms + 1, replacement: null },
        { start: me - 1, end: me, replacement: null },
      ],
    });
  }

  function inCodeSpan(charPos: number): boolean {
    for (const [s, e] of codeSpanCharRanges) {
      if (charPos >= s && charPos < e) return true;
    }
    return false;
  }

  // 2. Emphasis
  const emphasisPatterns: [RegExp, InlineSpan['type'], number][] = [
    [/\*{3}([^*]+)\*{3}/g, 'bold-italic', 3],
    [/(?<!\*)\*{2}(?!\*)([^*]+?)(?<!\*)\*{2}(?!\*)/g, 'bold', 2],
    [/(?<!\*)\*(?!\*)([^*]+?)(?<!\*)\*(?!\*)/g, 'italic', 1],
    [/~~([^~]+)~~/g, 'strikethrough', 2],
  ];
  for (const [pattern, type, markerLen] of emphasisPatterns) {
    const re = new RegExp(pattern.source, pattern.flags);
    while ((m = re.exec(text)) !== null) {
      if (inCodeSpan(m.index)) continue;
      const ms = m.index;
      const me = ms + m[0].length;
      spans.push({
        type,
        matchStart: ms, matchEnd: me,
        contentStart: ms + markerLen,
        contentEnd: ms + markerLen + m[1].length,
        concealRanges: [
          { start: ms, end: ms + markerLen, replacement: null },
          { start: me - markerLen, end: me, replacement: null },
        ],
      });
    }
  }

  // 3. Links
  const linkRe = /(?<!!)\[([^\]]+)\]\(([^)]+)\)/g;
  while ((m = linkRe.exec(text)) !== null) {
    if (inCodeSpan(m.index)) continue;
    const ms = m.index;
    const me = ms + m[0].length;
    const textEnd = ms + 1 + m[1].length;
    spans.push({
      type: 'link',
      matchStart: ms, matchEnd: me,
      contentStart: ms + 1, contentEnd: textEnd,
      concealRanges: [
        { start: ms, end: ms + 1, replacement: null },
        { start: textEnd, end: me, replacement: ` — ${m[2]}` },
      ],
      linkUrl: m[2],
    });
  }

  // 3b. Footnote references: [^1] → superscript ¹. Definition lines
  // (`[^1]: text`) are excluded by the (?!:) guard and handled per-line in
  // processLineConceals.
  const footnoteRe = /\[\^([^\]\s]+)\](?!:)/g;
  while ((m = footnoteRe.exec(text)) !== null) {
    if (inCodeSpan(m.index)) continue;
    const ms = m.index;
    const me = ms + m[0].length;
    spans.push({
      type: 'footnote',
      matchStart: ms, matchEnd: me,
      contentStart: ms, contentEnd: me,
      concealRanges: [{ start: ms, end: me, replacement: superscriptLabel(m[1]) }],
    });
  }

  // 4. HTML entities
  const namedEntityRe = /&(nbsp|amp|lt|gt|mdash|ndash|hellip|rsquo|lsquo|rdquo|ldquo|bull|middot|copy|reg|trade|times|divide|plusmn|deg|frac12|frac14|rarr|larr|harr|uarr|darr|euro|pound|yen|cent|sect|para|laquo|raquo|ensp|emsp|thinsp);/g;
  while ((m = namedEntityRe.exec(text)) !== null) {
    if (inCodeSpan(m.index)) continue;
    const replacement = HTML_ENTITY_MAP[m[1]];
    if (!replacement) continue;
    spans.push({
      type: 'entity',
      matchStart: m.index, matchEnd: m.index + m[0].length,
      contentStart: m.index, contentEnd: m.index + m[0].length,
      concealRanges: [{ start: m.index, end: m.index + m[0].length, replacement }],
    });
  }
  const numericDecEntityRe = /&#(\d{1,6});/g;
  while ((m = numericDecEntityRe.exec(text)) !== null) {
    if (inCodeSpan(m.index)) continue;
    const cp = parseInt(m[1], 10);
    if (cp < 1 || cp > 0x10FFFF) continue;
    spans.push({
      type: 'entity',
      matchStart: m.index, matchEnd: m.index + m[0].length,
      contentStart: m.index, contentEnd: m.index + m[0].length,
      concealRanges: [{ start: m.index, end: m.index + m[0].length, replacement: String.fromCodePoint(cp) }],
    });
  }
  const numericHexEntityRe = /&#x([0-9a-fA-F]{1,6});/g;
  while ((m = numericHexEntityRe.exec(text)) !== null) {
    if (inCodeSpan(m.index)) continue;
    const cp = parseInt(m[1], 16);
    if (cp < 1 || cp > 0x10FFFF) continue;
    spans.push({
      type: 'entity',
      matchStart: m.index, matchEnd: m.index + m[0].length,
      contentStart: m.index, contentEnd: m.index + m[0].length,
      concealRanges: [{ start: m.index, end: m.index + m[0].length, replacement: String.fromCodePoint(cp) }],
    });
  }

  return spans;
}

/**
 * Return the visible text of a string after applying all inline conceals.
 * Used for table column width calculation so emphasis/link syntax is not
 * counted towards cell width.
 */
function concealedText(text: string): string {
  const ranges: Array<{start: number; end: number; replacement: string | null}> = [];
  for (const span of findInlineSpans(text)) {
    ranges.push(...span.concealRanges);
  }
  ranges.sort((a, b) => a.start - b.start);

  let result = '';
  let pos = 0;
  for (const r of ranges) {
    if (r.start < pos) continue; // overlapping range
    if (r.start > pos) result += text.slice(pos, r.start);
    if (r.replacement !== null) result += r.replacement;
    pos = r.end;
  }
  result += text.slice(pos);
  return result;
}

const MIN_COL_W = 3;

/**
 * Return the effective compose width for layout: the configured compose
 * width clamped to the available viewport width.
 *
 * When `config.composeWidth` is explicitly set (e.g. 80) but the editor
 * content area is smaller (e.g. after the File Explorer sidebar opens),
 * using the configured value verbatim overflows the viewport. The Rust
 * render layer already clamps the compose area the same way in
 * `calculate_compose_layout`; plugin-side computations (table column
 * allocation, soft-wrap width) need to match.
 */
function effectiveComposeWidth(viewportWidth: number): number {
  const cw = config.composeWidth;
  if (cw == null) return viewportWidth;
  return Math.min(cw, viewportWidth);
}

// A table's right edge must never land in the final terminal column — most
// terminals auto-wrap the cursor there, which pushes the closing border glyph
// onto its own screen row. One cell of slack keeps the frame intact.
const TABLE_RIGHT_MARGIN = 1;

/**
 * Cells available to a table's *columns* (i.e. excluding the `numCols + 1`
 * vertical border glyphs), for a given viewport width.
 *
 * Tables are laid out like a rendered README table, not stretched edge-to-edge:
 * the total frame is capped at the configured compose width (or `maxWidth` when
 * none is set) and clamped to the viewport, minus a one-cell right margin. On a
 * wide terminal this keeps a 4-column status table readable instead of fanning
 * the prose column across 150+ cells; on a narrow one it shrinks to fit.
 */
function tableAvailableWidth(viewportWidth: number, numCols: number): number {
  const cap = config.composeWidth != null ? config.composeWidth : config.maxWidth;
  const frame = Math.min(cap, viewportWidth) - TABLE_RIGHT_MARGIN;
  return frame - (numCols + 1);
}

/**
 * Column width distribution via water-filling ("max-content with fair
 * shrinking"), the model browsers and pandoc use for `table-layout: auto`.
 *
 * The old approach distributed the available width *proportionally to each
 * column's natural width*, which over-rewards a column that is already wide
 * (a prose "Notes" column) and squeezes short label columns far below their
 * natural width — so a single word like `workflowTransitions` gets chopped
 * mid-word into `workflowTr`/`ansitions` even though a much wider neighbour
 * could have absorbed all the shrinking.
 *
 * Water-filling instead finds the largest cap C such that
 *   sum(clamp(maxW[i], MIN_COL_W, C)) <= available.
 * Columns narrower than C keep their natural width untouched (so short label
 * columns never wrap), and only columns wider than C are clipped to C — the
 * deficit comes entirely out of the table's widest prose columns, which wrap
 * cleanly at word boundaries.
 */
function distributeColumnWidths(maxW: number[], available: number): number[] {
  const numCols = maxW.length;
  if (numCols === 0) return [];
  const total = maxW.reduce((s, w) => s + w, 0);
  if (total <= available) return maxW.slice();
  // Not even room for every column's minimum: give everyone the floor.
  if (numCols * MIN_COL_W >= available) return maxW.map(() => MIN_COL_W);

  // Width consumed if no column is allowed past `cap` (and none below the
  // floor). Monotonic non-decreasing in `cap`, so binary-searchable.
  const widthAt = (cap: number) =>
    maxW.reduce((s, w) => s + Math.min(Math.max(w, MIN_COL_W), cap), 0);

  // Largest cap whose total still fits. widthAt(MIN_COL_W) = numCols*MIN_COL_W,
  // already known to be < available, so `lo` starts valid.
  let lo = MIN_COL_W;
  let hi = Math.max(...maxW);
  while (lo < hi) {
    const mid = Math.ceil((lo + hi) / 2);
    if (widthAt(mid) <= available) lo = mid;
    else hi = mid - 1;
  }
  const cap = lo;
  const widths = maxW.map(w => Math.min(Math.max(w, MIN_COL_W), cap));

  // Integer flooring at the cap can leave a few cells unspent; hand them to
  // the capped (widest) columns, widest-natural first, so the prose column
  // reclaims every available cell instead of leaving a ragged right edge.
  let leftover = available - widths.reduce((s, w) => s + w, 0);
  const capped = maxW
    .map((w, i) => ({ i, w }))
    .filter(c => c.w > cap)
    .sort((a, b) => b.w - a.w);
  for (let k = 0; leftover > 0 && capped.length > 0; k++, leftover--) {
    widths[capped[k % capped.length].i]++;
  }
  return widths;
}

/**
 * Terminal cell width of a single code point. Tables are padded to align in
 * *cells*, not code units — an emoji like ✅ is one UTF-16 unit but occupies
 * two cells, so `.length`-based padding pushes the right border out of line.
 */
function charDisplayWidth(cp: number): number {
  // Zero-width: combining marks, ZWJ, zero-width space, variation selectors.
  if (
    cp === 0x200b || cp === 0x200d ||
    (cp >= 0x0300 && cp <= 0x036f) ||
    (cp >= 0xfe00 && cp <= 0xfe0f)
  ) return 0;
  // Symbols in U+2300–U+27BF that default to emoji presentation (2 cells in
  // most terminals): watches, weather, ✅ ❌ ❓ ❗ ⭕ etc.
  if (
    (cp >= 0x231a && cp <= 0x231b) || (cp >= 0x23e9 && cp <= 0x23ec) ||
    cp === 0x23f0 || cp === 0x23f3 || (cp >= 0x25fd && cp <= 0x25fe) ||
    (cp >= 0x2614 && cp <= 0x2615) || (cp >= 0x2648 && cp <= 0x2653) ||
    cp === 0x267f || cp === 0x2693 || cp === 0x26a1 ||
    (cp >= 0x26aa && cp <= 0x26ab) || (cp >= 0x26bd && cp <= 0x26be) ||
    (cp >= 0x26c4 && cp <= 0x26c5) || cp === 0x26ce || cp === 0x26d4 ||
    cp === 0x26ea || (cp >= 0x26f2 && cp <= 0x26f3) || cp === 0x26f5 ||
    cp === 0x26fa || cp === 0x26fd || cp === 0x2705 ||
    (cp >= 0x270a && cp <= 0x270b) || cp === 0x2728 || cp === 0x274c ||
    cp === 0x274e || (cp >= 0x2753 && cp <= 0x2755) || cp === 0x2757 ||
    (cp >= 0x2795 && cp <= 0x2797) || cp === 0x27b0 || cp === 0x27bf ||
    (cp >= 0x2b1b && cp <= 0x2b1c) || cp === 0x2b50 || cp === 0x2b55
  ) return 2;
  // East Asian Wide / Fullwidth and the main emoji planes.
  if (
    (cp >= 0x1100 && cp <= 0x115f) ||
    (cp >= 0x2e80 && cp <= 0xa4cf && cp !== 0x303f) ||
    (cp >= 0xac00 && cp <= 0xd7a3) ||
    (cp >= 0xf900 && cp <= 0xfaff) ||
    (cp >= 0xfe30 && cp <= 0xfe4f) ||
    (cp >= 0xff00 && cp <= 0xff60) ||
    (cp >= 0xffe0 && cp <= 0xffe6) ||
    (cp >= 0x1f1e6 && cp <= 0x1f1ff) ||   // regional indicators (flags)
    (cp >= 0x1f300 && cp <= 0x1f64f) ||
    (cp >= 0x1f680 && cp <= 0x1f6ff) ||
    // Colored circles & squares (🔴🟠🟡🟢🔵🟣🟤🟥🟧🟨🟩🟦🟪🟫): a gap between
    // the ranges above and below. 🟡 (U+1F7E1) is a common "Partial" status
    // marker; without this it counts as 1 cell but renders as 2, shifting every
    // border to its right by one. (The surrounding U+1F700–U+1F77F alchemical
    // symbols are NOT emoji-presentation, so the range is deliberately tight.)
    (cp >= 0x1f7e0 && cp <= 0x1f7eb) ||
    (cp >= 0x1f900 && cp <= 0x1faff) ||
    (cp >= 0x20000 && cp <= 0x3fffd)
  ) return 2;
  return 1;
}

/** Display width of a string in terminal cells. */
function displayWidth(text: string): number {
  let w = 0;
  let prevBase = 0;
  for (const ch of text) {
    const cp = ch.codePointAt(0)!;
    if (cp === 0xfe0f) {
      // Emoji presentation selector upgrades a narrow base char (e.g. ⚠︎→⚠️)
      // to two cells.
      if (prevBase === 1) { w += 1; prevBase = 2; }
      continue;
    }
    const cw = charDisplayWidth(cp);
    w += cw;
    if (cw > 0) prevBase = cw;
  }
  return w;
}

/** Longest prefix of `text` whose display width is at most `width`. */
function truncateToWidth(text: string, width: number): string {
  let w = 0;
  let out = '';
  for (const ch of text) {
    const cw = charDisplayWidth(ch.codePointAt(0)!);
    if (w + cw > width) break;
    out += ch;
    w += cw;
  }
  return out;
}

/**
 * Wrap text into lines of at most `width` cells, breaking at word boundaries.
 */
function wrapText(text: string, width: number): string[] {
  if (width <= 0 || displayWidth(text) <= width) return [text];
  const lines: string[] = [];
  let pos = 0;
  while (pos < text.length) {
    const rest = text.slice(pos);
    if (displayWidth(rest) <= width) {
      lines.push(rest);
      break;
    }
    // Char index just past the last char that fits in `width` cells.
    const fit = truncateToWidth(rest, width).length || 1;
    let breakAt = text.lastIndexOf(' ', pos + fit);
    if (breakAt <= pos) {
      breakAt = pos + fit;
      lines.push(text.slice(pos, breakAt));
      pos = breakAt;
    } else {
      lines.push(text.slice(pos, breakAt));
      pos = breakAt + 1;
    }
  }
  return lines.length > 0 ? lines : [text];
}

/**
 * Process a single line: add overlays (emphasis, link styling) and conceals
 * (hide markdown syntax markers). Cursor-aware: when cursor is inside a span,
 * markers are revealed instead of concealed.
 */
function processLineConceals(
  bufferId: number,
  lineContent: string,
  byteStart: number,
  byteEnd: number,
  cursors: number[],
  lineNumber?: number,
): void {
  // Clear existing conceals and overlays for this line first.
  // This ensures clear+add commands are sent together from the plugin thread
  // and processed atomically in the same process_commands() batch, avoiding
  // the one-frame glitch where conceals are cleared but not yet rebuilt.
  // Slice by code POINTS, not UTF-16 units: `.slice(0, 40)` can cut an astral
  // char (e.g. an emoji like 🟡) between its surrogate halves, leaving a lone
  // surrogate the host's string→UTF-8 conversion rejects — that would throw out
  // of this debug line and abort composition for this line and every line after.
  editor.debug(`[mc] processLine clear+rebuild bytes=${byteStart}..${byteEnd} content="${[...lineContent].slice(0, 40).join("")}"`);
  // Namespace-scoped for the same reason as the overlay clear below: an
  // unscoped clear also wiped other plugins' conceals on these lines (e.g.
  // fresh-markdown-preview collapsing rendered mermaid blocks).
  editor.clearConcealsInRangeForNamespace(bufferId, "md-syntax", byteStart, byteEnd);
  // Only clear our own emphasis overlays — clearing ALL overlays in the range
  // would also wipe editor-owned overlays like LSP diagnostics (issue #2146).
  editor.clearOverlaysInRangeForNamespace(bufferId, "md-emphasis", byteStart, byteEnd);

  // `byteEnd` points just past this line's trailing newline — i.e. it is the
  // first byte of the *next* line. A cursor there belongs to the next line, so
  // it must not reveal this line's concealed markers (the bug where a heading's
  // `##` stays visible while the cursor sits on the blank line just below it).
  // Exclude that boundary — UNLESS the line has no trailing newline (the last
  // line of the buffer), where `byteEnd` is the true content end and a cursor
  // at it is still editing this line.
  const lineEndForCursor = lineContent.endsWith('\n') ? byteEnd - 1 : byteEnd;
  const cursorOnLine = cursors.some(c => c >= byteStart && c <= lineEndForCursor);
  // Strict version: excludes the boundary at byteEnd so that the cursor
  // sitting at the start of the *next* line doesn't count as being on
  // *this* line.  Used for table row auto-expose to avoid exposing the
  // previous row's emphasis markers.
  const cursorStrictlyOnLine = cursors.some(c => c >= byteStart && c < byteEnd);

  const trimmed = lineContent.trim();

  // --- Fenced code blocks ---
  // Fence marker lines: conceal the ``` markers + language tag (revealed
  // while the cursor is on the line) so code blocks render as a clean well.
  if (/^(```|~~~)/.test(trimmed)) {
    if (!cursorOnLine) {
      let effLen = lineContent.length;
      if (effLen > 0 && lineContent[effLen - 1] === '\n') effLen--;
      if (effLen > 0 && lineContent[effLen - 1] === '\r') effLen--;
      if (effLen > 0) {
        editor.addConceal(
          bufferId,
          "md-syntax",
          byteStart,
          charToByte(lineContent, effLen, byteStart),
          null,
        );
      }
    }
    return;
  }
  // Lines inside a fence are code, not markdown: no tables, no emphasis,
  // no conceals. (Cross-line context comes from the cached fence ranges.)
  if (insideFence(bufferId, byteStart)) return;

  // --- ATX headings ---
  // Conceal the `#` markers (revealed while the cursor is on the line) and
  // style the heading text by level. A terminal can't change font size, so
  // levels are distinguished by color/weight/underline instead.
  const headingMatch = lineContent.match(/^(\s{0,3})(#{1,6})\s+/);
  if (headingMatch) {
    const level = headingMatch[2].length;
    const markerStart = charToByte(lineContent, headingMatch[1].length, byteStart);
    const markerEnd = charToByte(lineContent, headingMatch[0].length, byteStart);
    if (!cursorOnLine) {
      editor.addConceal(bufferId, "md-syntax", markerStart, markerEnd, null);
    }
    let effLen = lineContent.length;
    if (effLen > 0 && lineContent[effLen - 1] === '\n') effLen--;
    if (effLen > 0 && lineContent[effLen - 1] === '\r') effLen--;
    const textEnd = charToByte(lineContent, effLen, byteStart);
    const headingStyles: Record<string, unknown>[] = [
      { fg: "syntax.keyword", bold: true, underline: true },  // H1
      { fg: "syntax.function", bold: true, underline: true }, // H2
      { fg: "syntax.function", bold: true },                  // H3
      { fg: "syntax.type", bold: true },                      // H4
      { fg: "syntax.constant", bold: true },                  // H5
      { fg: "syntax.constant", italic: true },                // H6
    ];
    if (textEnd > markerEnd) {
      editor.addOverlay(
        bufferId,
        "md-emphasis",
        markerEnd,
        textEnd,
        headingStyles[Math.min(level, 6) - 1],
      );
    }
    // Fall through: headings may still contain inline emphasis/code/links.
  }

  // --- Footnote definitions: [^1]: text ---
  // The `[^1]:` marker renders as the same superscript the in-text reference
  // uses, and the definition text is dimmed, mirroring GitHub's footnotes
  // section. Revealed while the cursor is on the line.
  const footDefMatch = lineContent.match(/^(\s{0,3})\[\^([^\]\s]+)\]:( ?)/);
  if (footDefMatch) {
    if (!cursorOnLine) {
      editor.addConceal(
        bufferId,
        "md-syntax",
        charToByte(lineContent, footDefMatch[1].length, byteStart),
        charToByte(lineContent, footDefMatch[0].length, byteStart),
        superscriptLabel(footDefMatch[2]) + " ",
      );
    }
    let effLen = lineContent.length;
    if (effLen > 0 && lineContent[effLen - 1] === '\n') effLen--;
    if (effLen > 0 && lineContent[effLen - 1] === '\r') effLen--;
    const defTextStart = charToByte(lineContent, footDefMatch[0].length, byteStart);
    const defTextEnd = charToByte(lineContent, effLen, byteStart);
    if (defTextEnd > defTextStart) {
      editor.addOverlay(bufferId, "md-emphasis", defTextStart, defTextEnd, {
        fg: "syntax.comment",
      });
    }
    // Fall through: definitions may still contain inline emphasis/code/links.
  }

  // --- Block quotes ---
  // `> text` (and nested `> > text`): each `>` marker renders as a vertical
  // quote bar and the quoted text is dimmed, approximating how GitHub
  // displays block quotes. The bar glyph is width-preserving (one cell per
  // `>`), so soft-wrap budgets and hanging indents are unaffected. Markers
  // are revealed while the cursor is on the line.
  const quoteMatch = lineContent.match(/^(\s{0,3})(>(?:[ \t]?>)*)/);
  if (quoteMatch) {
    const runStart = quoteMatch[1].length;
    const markerRun = quoteMatch[2];
    if (!cursorOnLine) {
      for (let ci = 0; ci < markerRun.length; ci++) {
        if (markerRun[ci] !== '>') continue;
        const pos = runStart + ci;
        editor.addConceal(
          bufferId,
          "md-syntax",
          charToByte(lineContent, pos, byteStart),
          charToByte(lineContent, pos + 1, byteStart),
          "▌",
        );
      }
    }
    let effLen = lineContent.length;
    if (effLen > 0 && lineContent[effLen - 1] === '\n') effLen--;
    if (effLen > 0 && lineContent[effLen - 1] === '\r') effLen--;
    const textStart = charToByte(lineContent, runStart + markerRun.length, byteStart);
    const textEnd = charToByte(lineContent, effLen, byteStart);
    if (textEnd > textStart) {
      editor.addOverlay(bufferId, "md-emphasis", textStart, textEnd, {
        fg: "syntax.comment",
        italic: true,
      });
    }
    // Fall through: quoted text may still contain inline emphasis/code/links.
  }

  // --- Horizontal rules ---
  // `---` / `***` / `___` render as a rule spanning the compose width
  // (revealed while the cursor is on the line).
  if (/^(-{3,}|\*{3,}|_{3,})$/.test(trimmed)) {
    if (!cursorOnLine) {
      let effLen = lineContent.length;
      if (effLen > 0 && lineContent[effLen - 1] === '\n') effLen--;
      if (effLen > 0 && lineContent[effLen - 1] === '\r') effLen--;
      if (effLen > 0) {
        const viewport = editor.getViewport();
        const ruleW = Math.max(3, effectiveComposeWidth(viewport ? viewport.width : 80) - 2);
        editor.addConceal(
          bufferId,
          "md-syntax",
          byteStart,
          charToByte(lineContent, effLen, byteStart),
          "─".repeat(ruleW),
        );
      }
    }
    return;
  }

  // --- List bullets and task checkboxes ---
  // `- ` / `* ` / `+ ` bullets render as `•` (width-preserving), and task
  // boxes `[ ]` / `[x]` render as ☐ / ☑. Both revealed while the cursor is
  // on the line. Ordered-list numbers stay as-is — they're already readable.
  const bulletMatch = lineContent.match(/^(\s*)([-*+])(\s+)/);
  if (bulletMatch && !cursorOnLine) {
    const bulletPos = bulletMatch[1].length;
    editor.addConceal(
      bufferId,
      "md-syntax",
      charToByte(lineContent, bulletPos, byteStart),
      charToByte(lineContent, bulletPos + 1, byteStart),
      "•",
    );
    const boxMatch = lineContent.slice(bulletMatch[0].length).match(/^\[([ xX])\](?= |$)/);
    if (boxMatch) {
      const boxPos = bulletMatch[0].length;
      editor.addConceal(
        bufferId,
        "md-syntax",
        charToByte(lineContent, boxPos, byteStart),
        charToByte(lineContent, boxPos + 3, byteStart),
        boxMatch[1] === ' ' ? "☐" : "☑",
      );
    }
    // Fall through: list items may still contain inline emphasis/code/links.
  }

  // --- Table row handling ---
  // Always apply table conceals even when cursor is on the line.
  // Tables are structural: pipes → box-drawing, cells padded for alignment.
  // Toggling conceals on/off per cursor line causes visual width shifts that
  // break cursor navigation (stuck cursor, ghost cursors) and lose alignment.
  const truncatedByteRanges: Array<{start: number; end: number}> = [];
  let isTableRow = false;
  if (trimmed.startsWith('|') || trimmed.endsWith('|')) {
    isTableRow = true;
    const isSeparator = /^\|[-:\s|]+\|$/.test(trimmed);

    // Look up stored column widths for alignment padding
    const bufWidths = lineNumber !== undefined ? getTableWidths(bufferId) : undefined;
    const widthInfo = bufWidths && lineNumber !== undefined ? bufWidths.get(lineNumber) : undefined;
    const colWidths = widthInfo ? widthInfo.allocated : undefined;

    // Split the line into cells to compute per-cell padding. Escaped pipes
    // (`\|`) are cell content, so split only on the unescaped column borders.
    const cells = splitTableCells(tableRowInner(trimmed));

    // Pipe positions in the (untrimmed) source line — shared by the wrapped
    // first-line path and the single-line path below. Unescaped pipes only:
    // an escaped `\|` is rendered inline by the char loop, not as a border.
    const pipePositions = tablePipePositions(lineContent);

    // Multi-line cell wrapping. When a column's text is wider than its
    // allocated width the row spans several visual lines. The FIRST visual
    // line is rendered in place here — each cell is concealed to its first
    // wrapped fragment (padded to the column width) and each pipe → │ — while
    // the continuation lines are emitted as virtual lines below the row by
    // processTableBorders. Keeping every source row exactly one source line
    // means alignment and borders are computed from generated text, like a
    // rendered README table, instead of splitting the source with soft breaks
    // (which can't align independent columns and corrupted neighbouring lines).
    let handledByWrapping = false;
    if (colWidths && !isSeparator && !cursorStrictlyOnLine) {
      const { cellWrapped, numCols, maxVisualLines } =
        wrapTableRow(lineContent, colWidths, false);
      if (maxVisualLines > 1 && pipePositions.length >= numCols + 1) {
        for (let ci = 0; ci < numCols; ci++) {
          const wrapW = Math.max(1, colWidths[ci] - 2);
          const frag = cellWrapped[ci][0] || '';
          const cellRender =
            ' ' + frag + ' '.repeat(Math.max(0, wrapW - displayWidth(frag))) + ' ';
          const cStart = charToByte(lineContent, pipePositions[ci] + 1, byteStart);
          const cEnd = charToByte(lineContent, pipePositions[ci + 1], byteStart);
          editor.addConceal(bufferId, "md-syntax", cStart, cEnd, cellRender);
        }
        for (let pi = 0; pi < pipePositions.length; pi++) {
          const pStart = charToByte(lineContent, pipePositions[pi], byteStart);
          const pEnd = charToByte(lineContent, pipePositions[pi] + 1, byteStart);
          editor.addConceal(bufferId, "md-syntax", pStart, pEnd, "│");
        }
        handledByWrapping = true;
      }
    }

    if (!handledByWrapping) {

      // Precompute which cells will be truncated. Per-character conceals
      // that land inside a truncated cell must be suppressed — the cell-
      // wide truncate conceal already renders the replacement. When both
      // fire, the per-char conceal at the cell's first byte emits its
      // replacement, and the cell-wide conceal emits its replacement one
      // byte later, producing a cell one character wider than allocated.
      const truncatedCellCharRanges: Array<{start: number; end: number}> = [];
      if (!cursorStrictlyOnLine && colWidths) {
        for (let ci = 0; ci < Math.min(cells.length, colWidths.length); ci++) {
          const cellText = concealedText(cells[ci]);
          if (displayWidth(cellText) > colWidths[ci]) {
            const prevPipe = pipePositions[ci];
            const nextPipe = pipePositions[ci + 1];
            if (prevPipe !== undefined && nextPipe !== undefined) {
              truncatedCellCharRanges.push({ start: prevPipe + 1, end: nextPipe });
            }
          }
        }
      }

      // Track which pipe index we're on (0 = leading pipe)
      let pipeIdx = 0;
      for (let i = 0; i < lineContent.length; i++) {
        // Escaped pipe `\|`: cell content, not a column border. Render it as a
        // literal `|` (the backslash is just the escape marker), and do NOT
        // count it as a column separator. Skip the conceal when the cursor is
        // on the row (leave the raw `\|` visible for editing, like other
        // revealed markers) or when the enclosing cell is truncated (its
        // cell-wide conceal already covers these bytes).
        if (lineContent[i] === '|' && isEscapedPipe(lineContent, i)) {
          if (!cursorStrictlyOnLine) {
            const inTruncated = truncatedCellCharRanges.some(
              r => (i - 1) >= r.start && (i - 1) < r.end,
            );
            if (!inTruncated) {
              const escStart = charToByte(lineContent, i - 1, byteStart);
              const escEnd = charToByte(lineContent, i + 1, byteStart);
              editor.addConceal(bufferId, "md-syntax", escStart, escEnd, "|");
            }
          }
          continue;
        }
        if (lineContent[i] === '|') {
          const pipeByte = charToByte(lineContent, i, byteStart);
          const pipeByteEnd = charToByte(lineContent, i + 1, byteStart);

          // Compute padding or truncation for the cell that just ended.
          // When the cursor is on this row, skip truncation/padding entirely
          // so that only pipe→│ conceals exist. This ensures cursor positioning
          // works correctly (segment conceals break cursor mapping).
          let padding = "";
          const cellIdx = pipeIdx - 1;
          if (!cursorStrictlyOnLine && colWidths && pipeIdx > 0 && cellIdx < cells.length && cellIdx < colWidths.length) {
            const cellText = concealedText(cells[cellIdx]);
            const cellWidth = displayWidth(cellText);
            const allocatedWidth = colWidths[cellIdx];

            if (cellWidth > allocatedWidth) {
              // Truncate: conceal entire cell content and replace with truncated text.
              // Separator rows use box-drawing ─ to match the non-truncated path
              // (per-char conceals replace source `-` with ─ and pad via pipe replacement).
              const prevPipeCharPos = pipePositions[pipeIdx - 1];
              const cellByteStart = charToByte(lineContent, prevPipeCharPos + 1, byteStart);
              const cellByteEnd = pipeByte;
              // Width-aware truncation can land 1 cell short when it would
              // split a double-width char; pad back up to the allocation.
              const cut = truncateToWidth(cellText, allocatedWidth - 1) + '-';
              const truncated = isSeparator
                ? '─'.repeat(allocatedWidth)
                : cut + ' '.repeat(Math.max(0, allocatedWidth - displayWidth(cut)));
              editor.addConceal(bufferId, "md-syntax", cellByteStart, cellByteEnd, truncated);
              truncatedByteRanges.push({start: cellByteStart, end: cellByteEnd});
            } else {
              const padCount = allocatedWidth - cellWidth;
              if (padCount > 0) {
                padding = isSeparator ? "─".repeat(padCount) : " ".repeat(padCount);
              }
            }
          }

          if (isSeparator) {
            const pipeIndex = pipeIdx + 1;
            const totalPipes = pipePositions.length;
            let replacement = '┼';
            if (pipeIndex === 1) replacement = '├';
            else if (pipeIndex === totalPipes) replacement = '┤';
            editor.addConceal(bufferId, "md-syntax", pipeByte, pipeByteEnd, padding + replacement);
          } else {
            editor.addConceal(bufferId, "md-syntax", pipeByte, pipeByteEnd, padding + "│");
          }
          pipeIdx++;
        } else if (isSeparator && (lineContent[i] === '-' || lineContent[i] === ':')) {
          // Alignment colons (`:---:`) render as part of the rule line too —
          // leaving them visible bleeds `:----:` through the concealed row.
          // Skip per-character conceals that land inside a truncated cell;
          // the cell-wide truncate conceal already handles the rendering.
          const inTruncated = truncatedCellCharRanges.some(r => i >= r.start && i < r.end);
          if (inTruncated) continue;
          const db = charToByte(lineContent, i, byteStart);
          editor.addConceal(bufferId, "md-syntax", db, charToByte(lineContent, i + 1, byteStart), "─");
        }
      }
    }
    // For wrapped rows, entire line is concealed — skip emphasis processing.
    // For non-wrapped rows, fall through to emphasis / link / entity processing.
    if (handledByWrapping) return;
  }

  // --- Image links: ![alt](url) → "Image: alt — url" ---
  const imageRe = /^!\[([^\]]*)\]\(([^)]+)\)$/;
  const imageMatch = trimmed.match(imageRe);
  if (imageMatch && !cursorOnLine) {
    const alt = imageMatch[1];
    const url = imageMatch[2];
    editor.addConceal(bufferId, "md-syntax", byteStart, byteEnd, `Image: ${alt} — ${url}`);
    return;
  }

  // --- Inline spans: code, emphasis, links, entities ---
  const spans = findInlineSpans(lineContent);
  for (const span of spans) {
    const byteCS = charToByte(lineContent, span.contentStart, byteStart);
    const byteCE = charToByte(lineContent, span.contentEnd, byteStart);
    const byteMS = charToByte(lineContent, span.matchStart, byteStart);
    const byteME = charToByte(lineContent, span.matchEnd, byteStart);

    // Skip overlays and conceals for spans inside truncated table cells —
    // the cell content has already been fully replaced by truncated text.
    const inTruncated = truncatedByteRanges.some(r => byteMS >= r.start && byteME <= r.end);
    if (inTruncated) continue;

    // Overlays (styling)
    switch (span.type) {
      case 'code':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { fg: "syntax.constant" });
        break;
      case 'bold':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { bold: true });
        break;
      case 'italic':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { italic: true });
        break;
      case 'bold-italic':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { bold: true, italic: true });
        break;
      case 'strikethrough':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { strikethrough: true });
        break;
      case 'link':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, {
          fg: "syntax.link",
          underline: true,
          url: span.linkUrl,
        });
        break;
      case 'footnote':
        editor.addOverlay(bufferId, "md-emphasis", byteCS, byteCE, { fg: "syntax.link" });
        break;
      // entities: no overlay
    }

    // Conceals (cursor-aware).
    // For table rows: skip ALL emphasis conceals when cursor is on the line,
    // not just the span the cursor is in. This "auto-expose entire row"
    // approach keeps the row layout consistent with the raw-text-based
    // column widths, preventing overflow/wrapping.
    const cursorInSpan = cursors.some(c => c >= byteMS && c <= byteME);
    const skipConceal = (isTableRow && cursorStrictlyOnLine) || cursorInSpan;
    if (!skipConceal) {
      for (const range of span.concealRanges) {
        const rStart = charToByte(lineContent, range.start, byteStart);
        const rEnd = charToByte(lineContent, range.end, byteStart);
        editor.addConceal(bufferId, "md-syntax", rStart, rEnd, range.replacement);
      }
    }
  }
}

// Last cursor line is tracked per-buffer-per-split via setViewState/getViewState

// Track viewport width per buffer for resize detection
let lastViewportWidth = 0;

// =============================================================================
// Hook handlers
// =============================================================================

/**
 * Compute soft break points for a single line, using the same block parsing
 * and word-wrap logic as the old transformMarkdownTokens, but emitting
 * marker-based soft breaks instead of view_transform tokens.
 */
function processLineSoftBreaks(
  bufferId: number,
  lineContent: string,
  byteStart: number,
  byteEnd: number,
  cursors: number[],
  lineNumber?: number,
): void {
  // Clear existing soft breaks for this line range
  editor.clearSoftBreaksInRange(bufferId, byteStart, byteEnd);

  // Code lines never wrap and must not be misread as markdown (a `|`-leading
  // code line would otherwise get table-cell soft breaks).
  if (insideFence(bufferId, byteStart)) return;

  const viewport = editor.getViewport();
  if (!viewport) return;
  const width = effectiveComposeWidth(viewport.width);

  // Parse this single line to get block structure
  const blocks = parseMarkdownBlocks(lineContent);
  if (blocks.length === 0) return;

  const block = blocks[0]; // Single line = single block

  // Determine if this block type should be soft-wrapped
  const noWrap = block.type === 'table-row' || block.type === 'code-fence' ||
                 block.type === 'code-content' || block.type === 'hr' ||
                 block.type === 'heading' || block.type === 'image' ||
                 block.type === 'empty';

  // Image blocks: add a trailing blank line for visual separation when concealed
  if (block.type === 'image') {
    const cursorOnLine = cursors.some(c => c >= byteStart && c <= byteEnd);
    if (!cursorOnLine) {
      editor.addSoftBreak(bufferId, "md-wrap", byteEnd - 1, 0);
    }
  }

  // Table rows never use soft breaks: a wrapped cell's overflow is rendered as
  // virtual continuation lines (processTableBorders), not by splitting the
  // single source line. (table-row is in `noWrap`, so we'd return below anyway.)

  if (noWrap) return;

  const hangingIndent = block.hangingIndent;

  // Compute per-character visual width so concealed markup (emphasis
  // markers, link syntax, entities) doesn't count towards line width.
  const spans = findInlineSpans(lineContent);
  const charW = new Array<number>(lineContent.length).fill(1);
  for (const span of spans) {
    for (const range of span.concealRanges) {
      for (let c = range.start; c < range.end && c < lineContent.length; c++) {
        charW[c] = 0;
      }
      // Entity replacements contribute their replacement's length
      if (range.replacement !== null && range.start < lineContent.length) {
        charW[range.start] = range.replacement.length;
      }
    }
  }

  // Walk through the line content and find word-wrap break points
  // We need to find Space positions where wrapping should occur.
  //
  // The wrap budget must reserve columns to match the Rust renderer's
  // `apply_wrapping_transform`, which subtracts one from `content_width`
  // to keep the end-of-line cursor off the scrollbar track. If the
  // plugin uses the full viewport width, it produces lines that fit
  // exactly N columns; the renderer then re-wraps them at N-1, splitting
  // off the trailing word into a single-word "orphan" visual row
  // (issue #1789).
  //
  // We subtract two rather than just one so the plugin's wrap output
  // stays a column inside the renderer's threshold across platforms,
  // covering minor differences in scrollbar / gutter / EOL-cursor
  // reservation between terminals.
  const wrapBudget = Math.max(1, width - 2);
  let column = 0;
  let i = 0;

  while (i < lineContent.length) {
    const ch = lineContent[i];

    if (ch === ' ' && column > 0 && charW[i] > 0) {
      // Look ahead to find the next word's visual length
      let nextWordLen = 0;
      for (let j = i + 1; j < lineContent.length; j++) {
        if ((lineContent[j] === ' ' || lineContent[j] === '\n') && charW[j] > 0) break;
        nextWordLen += charW[j];
      }

      // Check if space + next word would exceed wrap budget
      if (column + 1 + nextWordLen > wrapBudget && nextWordLen > 0) {
        // Add a soft break at this space's buffer position
        const breakBytePos = byteStart + editor.utf8ByteLength(lineContent.slice(0, i));
        editor.addSoftBreak(bufferId, "md-wrap", breakBytePos, hangingIndent);
        column = hangingIndent;
        i++;
        continue;
      }
    }

    column += charW[i];
    i++;
  }
}

/**
 * Pre-compute column widths for table groups in a batch of lines.
 * Groups consecutive table rows and computes max visible width per column.
 *
 * Uses an accumulate-and-grow strategy: widths are merged with previously
 * cached values (taking the max per column) so that as the user scrolls
 * through a large table, column widths converge to the true maximum and
 * never shrink.
 */
function processTableAlignment(
  bufferId: number,
  lines: Array<{ line_number: number; byte_start: number; byte_end: number; content: string }>,
): boolean {
  // Get existing cache (accumulate-and-grow — don't discard previous widths)
  const widthMap = getTableWidths(bufferId) ?? new Map<number, TableWidthInfo>();
  let needsRefresh = false;

  // Group consecutive table rows
  const groups: Array<typeof lines> = [];
  let currentGroup: typeof lines = [];
  let lastLineNum = -2;

  for (const line of lines) {
    const trimmed = line.content.trim();
    const isTableRow =
      (trimmed.startsWith('|') || trimmed.endsWith('|')) &&
      !insideFence(bufferId, line.byte_start);
    if (isTableRow && line.line_number === lastLineNum + 1) {
      currentGroup.push(line);
    } else if (isTableRow) {
      if (currentGroup.length > 0) groups.push(currentGroup);
      currentGroup = [line];
    } else {
      if (currentGroup.length > 0) groups.push(currentGroup);
      currentGroup = [];
    }
    lastLineNum = line.line_number;
  }
  if (currentGroup.length > 0) groups.push(currentGroup);

  // For each group, compute max column widths and merge with cache
  for (const group of groups) {
    const allCells: string[][] = [];

    for (const line of group) {
      const trimmed = line.content.trim();
      // Strip outer pipes and split on unescaped inner pipes (so `\|` stays
      // cell content and doesn't inflate the column count / widths).
      const cells = splitTableCells(tableRowInner(trimmed));
      allCells.push(cells);
    }

    // Find max column count
    const maxCols = allCells.reduce((max, row) => Math.max(max, row.length), 0);

    // Compute max visible width per column from the currently visible rows
    const newWidths: number[] = [];
    for (let col = 0; col < maxCols; col++) {
      let maxW = 0;
      for (const row of allCells) {
        if (col < row.length) {
          // For separator rows, use 0 width (they adapt to data rows).
          // Use RAW text width (not concealedText) so that columns are always
          // sized to accommodate revealed emphasis markers when cursor enters
          // a row. Concealed rows simply get extra padding.
          const isSep = /^[-:\s]+$/.test(row[col]);
          if (!isSep) {
            maxW = Math.max(maxW, displayWidth(row[col]));
          }
        }
      }
      newWidths.push(maxW);
    }

    // Merge with any previously cached maxW arrays for lines in this group
    // (they may have been computed from a different visible slice of the
    // same table). Take the max per column — widths only grow.
    let merged = newWidths;
    const mergeWith = (cached: number[]) => {
      const cols = Math.max(merged.length, cached.length);
      const wider: number[] = [];
      for (let c = 0; c < cols; c++) {
        wider.push(Math.max(merged[c] ?? 0, cached[c] ?? 0));
      }
      merged = wider;
    };

    for (const line of group) {
      const cached = widthMap.get(line.line_number);
      if (cached) mergeWith(cached.maxW);
    }

    // Also merge with adjacent cached lines above/below the group.
    // When mouse-scrolling, lines_changed only delivers NEW lines (not
    // previously seen), so the group may not overlap with earlier cached
    // rows of the same table. Scanning adjacently bridges the gap.
    const firstLine = group[0].line_number;
    const lastLine = group[group.length - 1].line_number;
    for (let ln = firstLine - 1; widthMap.has(ln); ln--) {
      mergeWith(widthMap.get(ln)!.maxW);
    }
    for (let ln = lastLine + 1; widthMap.has(ln); ln++) {
      mergeWith(widthMap.get(ln)!.maxW);
    }

    // Compute allocated widths constrained to viewport. Clamp the
    // configured compose width to the actual viewport — otherwise a
    // large configured width overflows when the editor area shrinks
    // (e.g. when the File Explorer sidebar opens).
    const viewport = editor.getViewport();
    const numCols = merged.length;
    const available = tableAvailableWidth(viewport ? viewport.width : 80, numCols);
    const allocated = distributeColumnWidths(merged, available);

    // Check if adjacent cached lines had narrower allocated widths — if so,
    // they need their conceals recomputed (they were already rendered with
    // old widths and won't be re-delivered by lines_changed).
    const allocGrew = (old: TableWidthInfo) =>
      allocated.some((w, i) => w > (old.allocated[i] ?? 0));
    for (let ln = firstLine - 1; widthMap.has(ln); ln--) {
      if (allocGrew(widthMap.get(ln)!)) { needsRefresh = true; break; }
    }
    for (let ln = lastLine + 1; widthMap.has(ln); ln++) {
      if (allocGrew(widthMap.get(ln)!)) { needsRefresh = true; break; }
    }

    // Store merged widths for each line in the group.  We tag the source
    // separator row (`|---|---|---|`) so the border renderer can skip
    // drawing a duplicate `├─┼─┤` adjacent to it (the source separator is
    // already concealed into one).  Each line gets its own info object so
    // the per-row `isSourceSep` flag is independent.
    for (const line of group) {
      const isSep = /^\|[-:\s|]+\|$/.test(line.content.trim());
      widthMap.set(line.line_number, { maxW: merged, allocated, isSourceSep: isSep });
    }
    // Adjacent cached lines (already-processed neighbours of this group)
    // need their `allocated` updated but should keep their existing
    // `isSourceSep` flag — they were classified when they were processed.
    for (let ln = firstLine - 1; widthMap.has(ln); ln--) {
      const prev = widthMap.get(ln)!;
      widthMap.set(ln, { maxW: merged, allocated, isSourceSep: prev.isSourceSep });
    }
    for (let ln = lastLine + 1; widthMap.has(ln); ln++) {
      const prev = widthMap.get(ln)!;
      widthMap.set(ln, { maxW: merged, allocated, isSourceSep: prev.isSourceSep });
    }
  }

  setTableWidths(bufferId, widthMap);
  return needsRefresh;
}

// lines_changed: called for newly visible or invalidated lines


// after_insert: no-op for conceals/overlays.
// The edit automatically invalidates seen_byte_ranges for affected lines,
// causing lines_changed to fire on the next render. processLineConceals
// handles clearing and rebuilding atomically.
// Marker-based positions auto-adjust with buffer edits, so existing conceals
// remain visually correct until lines_changed rebuilds them.


// after_delete: no-op for conceals/overlays (same reasoning as after_insert).


// cursor_moved: update cursor-aware reveal/conceal for old and new cursor lines


// view_transform_request is no longer needed — soft wrapping is handled by
// marker-based soft breaks (computed in lines_changed), and layout hints
// are set directly via setLayoutHints. This eliminates the one-frame flicker
// caused by the async view_transform round-trip.

// Handle buffer close events - clean up compose mode tracking


// viewport_changed: recalculate table column widths on terminal resize


// Re-enable compose mode for buffers restored from a saved session.
// The Rust side restores ViewMode::Compose and compose_width, but the plugin
// needs to re-apply line numbers, line wrap, and layout hints when activated.


// Register hooks
editor.on("lines_changed", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  const lineNums = data.lines.map(l => `${l.line_number}(${l.byte_start}..${l.byte_end})`).join(', ');
  editor.debug(`[mc] lines_changed: ${data.lines.length} lines: [${lineNums}]`);
  // Only use cursor positions for reveal/conceal decisions when the active
  // split is in compose mode.  When a source-mode split is active, the cursor
  // lives in that source view — it should NOT trigger "reveal" (skip-conceal)
  // in the compose-mode split, because conceals are buffer-level decorations
  // shared across splits.
  const cursors = isComposing(data.buffer_id) ? [editor.getCursorPosition()] : [];

  // Pre-compute table column widths for alignment.
  // If widths grew from merging with adjacent cached rows (e.g. after a
  // mouse-scroll jump), force a full re-render so already-visible lines
  // pick up the wider columns. The second pass will be a no-op (widths
  // already converged) so this doesn't loop.
  const tableWidthsGrew = processTableAlignment(data.buffer_id, data.lines);

  // Process each line independently. A throw on one line (e.g. an unexpected
  // character sequence) must NOT abort the loop — otherwise that line AND every
  // line after it in the batch would be left uncomposed, rendering as raw
  // markdown from the failure point onward. Isolate per line and keep going.
  for (const line of data.lines) {
    try {
      processLineConceals(data.buffer_id, line.content, line.byte_start, line.byte_end, cursors, line.line_number);
      processLineSoftBreaks(data.buffer_id, line.content, line.byte_start, line.byte_end, cursors, line.line_number);
    } catch (e) {
      editor.debug(`[mc] line ${line.line_number} failed to compose: ${e}`);
    }
  }

  // Add/refresh table border virtual lines (top/bottom + inter-row separators).
  // Runs AFTER processTableAlignment so the widthMap reflects the latest
  // allocated widths, and AFTER processLineConceals so the borders we draw
  // line up with the cell pipes the conceals produce.
  const widthMapForBorders = getTableWidths(data.buffer_id);
  if (widthMapForBorders) {
    processTableBorders(data.buffer_id, data.lines, widthMapForBorders, cursors);
  }

  if (tableWidthsGrew) {
    editor.refreshLines(data.buffer_id);
  }
});
editor.on("after_insert", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  editor.debug(`[mc] after_insert: pos=${data.position} text="${data.text.replace(/\n/g,'\\n')}" affected=${data.affected_start}..${data.affected_end}`);
  // Keep fence ranges current so typed/removed fences change classification.
  // The constant cursor_moved refreshes pick the new ranges up next frame.
  void rebuildFenceRanges(data.buffer_id);
});
editor.on("after_delete", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  editor.debug(`[mc] after_delete: start=${data.start} end=${data.end} deleted="${data.deleted_text.replace(/\n/g,'\\n')}" affected_start=${data.affected_start} deleted_len=${data.deleted_len}`);
  void rebuildFenceRanges(data.buffer_id);
});
editor.on("cursor_moved", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;

  const prevLine = editor.getViewState(data.buffer_id, "last-cursor-line") as number | undefined;
  editor.setViewState(data.buffer_id, "last-cursor-line", data.line);

  editor.debug(`[mc] cursor_moved: old_pos=${data.old_position} new_pos=${data.new_position} line=${data.line} prevLine=${prevLine}`);

  // Always refresh: even intra-line movements need conceal updates because
  // auto-expose is span-level (cursor entering/leaving an emphasis or link
  // span within the same line must toggle its syntax markers).
  editor.refreshLines(data.buffer_id);
});
// view_transform_request hook no longer needed — wrapping is handled by soft breaks
editor.on("buffer_closed", (data) => {
  // View state is cleaned up automatically when the buffer is removed from keyed_states
});
editor.on("viewport_changed", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  if (data.width === lastViewportWidth) return;
  lastViewportWidth = data.width;

  // Recompute allocated table column widths for new viewport width
  const bufWidths = getTableWidths(data.buffer_id);
  if (bufWidths) {
    const seen = new Set<string>(); // Track by JSON key to deduplicate shared TableWidthInfo
    for (const [lineNum, info] of bufWidths) {
      const key = info.maxW.join(",");
      if (seen.has(key)) continue;
      seen.add(key);
      const numCols = info.maxW.length;
      const available = tableAvailableWidth(data.width, numCols);
      info.allocated = distributeColumnWidths(info.maxW, available);
    }
    setTableWidths(data.buffer_id, bufWidths);
  }
  editor.refreshLines(data.buffer_id);
});
editor.on("prompt_confirmed", (args) => {
  if (args.prompt_type !== "markdown-compose-width") return;

  const input = args.input.trim();
  if (input.toLowerCase() === "none") {
    config.composeWidth = null;
    editor.setStatus(editor.t("status.width_none"));

    const bufferId = editor.getActiveBufferId();
    if (isComposing(bufferId)) {
      editor.setLayoutHints(bufferId, null, {});
      editor.refreshLines(bufferId);
    }
    return;
  }

  const width = parseInt(input, 10);
  if (!isNaN(width) && width > 20 && width < 300) {
    config.composeWidth = width;
    editor.setStatus(editor.t("status.width_set", { width: String(width) }));

    // Re-process active buffer if in compose mode
    const bufferId = editor.getActiveBufferId();
    if (isComposing(bufferId)) {
      editor.setLayoutHints(bufferId, null, { composeWidth: config.composeWidth ?? undefined });
      editor.refreshLines(bufferId);  // Trigger soft break recomputation
    }
  } else {
    editor.setStatus(editor.t("status.invalid_width"));
  }
});
editor.on("buffer_activated", (data) => {
  const bufferId = data.buffer_id;

  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  if (info.view_mode === "compose") {
    // Restore config.composeWidth from the persisted session value
    // before enabling compose mode, so enableMarkdownCompose uses
    // the correct width (same path as a fresh toggle).
    if (info.compose_width != null) {
      config.composeWidth = info.compose_width;
    }
    enableMarkdownCompose(bufferId);
  } else if (getGlobalComposeEnabled()) {
    // Global compose/preview mode is active — auto-enable for newly opened
    // markdown buffers that aren't already in compose mode.
    enableMarkdownCompose(bufferId);
  }
});

// Set compose width command - starts interactive prompt
function markdownSetComposeWidth() : void {
  const currentValue = config.composeWidth === null ? "None" : String(config.composeWidth);
  editor.startPromptWithInitial(editor.t("prompt.compose_width"), "markdown-compose-width", currentValue);
  editor.setPromptInputSync(true);
  editor.setPromptSuggestions([
    { text: "None", description: editor.t("suggestion.none") },
    { text: "120", description: editor.t("suggestion.default") },
  ]);
}
registerHandler("markdownSetComposeWidth", markdownSetComposeWidth);

// Handle compose width prompt confirmation


// Register commands
editor.registerCommand(
  "%cmd.toggle_compose",
  "%cmd.toggle_compose_desc",
  "markdownToggleCompose",
  null
);

editor.registerCommand(
  "%cmd.toggle_compose_all",
  "%cmd.toggle_compose_all_desc",
  "markdownToggleComposeAll",
  null
);

editor.registerCommand(
  "%cmd.set_compose_width",
  "%cmd.set_compose_width_desc",
  "markdownSetComposeWidth",
  null
);

// Initialization
editor.debug("Markdown Compose plugin loaded - use 'Markdown: Toggle Compose/Preview' command");
