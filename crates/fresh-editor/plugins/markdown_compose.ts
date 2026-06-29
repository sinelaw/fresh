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

// =============================================================================
// Table borders: per-line, like conceals (NO stored table model, NO byte state)
// =============================================================================
//
// Table border virtual lines (the `┌─┬─┐` / `├─┼─┤` / `└─┴─┘` frame) are emitted
// PER LINE, in lockstep with the per-line conceal pass, and that is the whole
// design:
//
//   * For every line in a `lines_changed` batch we `clearVirtualLinesInRange`
//     the single byte at that row's start (removing its old frame) and re-add
//     the frame for its role. Role — first / last / source-separator-adjacent —
//     is local: it comes from the row plus its immediate neighbours in the same
//     batch (see the `lines_changed` handler + `emitRowBorders`).
//   * All borders live in ONE namespace (`md-tb`); the clears are byte-range
//     scoped, so adjacent rows and distinct tables never collide.
//   * Column widths are computed per render from the batch's table groups
//     (`computeRowWidths`) and shared by the border and conceal passes, so they
//     always line up. There is no cross-frame width memory.
//
// Why per-line instead of a stored table model: the previous design held each
// table as a core interval marker with a stored row array and rebuilt the whole
// frame from it. But `lines_changed` is fired fire-and-forget to the plugin
// thread, which reads markers off a snapshot the editor mutates concurrently —
// so a batch for edit N could be processed after the marker was shifted for edit
// N+1, leaving the stored rows a few bytes off the event and doubling the
// separators. The fix removes the stored model entirely: positions come only
// from the live event each frame (the conceals are rendered from them, so they
// are always correct), and the marker-backed border virtual lines auto-shift
// between frames, so nothing the plugin persists can desync. Edits need no
// special handling — the affected lines re-fire `lines_changed` and are
// cleared+rebuilt; unaffected rows' borders just ride. See
// docs/internal/MARKDOWN_COMPOSE_TABLE_POSITION_OWNERSHIP.md.

type LineInfoLike = {
  line_number: number;
  byte_start: number;
  byte_end: number;
  content: string;
};

// Table borders are emitted PER LINE, exactly like conceals: for each table row
// in a `lines_changed` batch we clear the virtual lines anchored at that row and
// re-add its frame, reading column widths from a per-render map. There is no
// stored table model — row positions come straight from the live event each
// frame, and the marker-backed border virtual lines auto-shift between frames,
// so nothing the plugin persists can desync. This replaced an interval-marker
// "block" model whose stored byte positions desynced across the async
// `lines_changed` thread boundary and doubled separators
// (see docs/internal/MARKDOWN_COMPOSE_TABLE_POSITION_OWNERSHIP.md).

// All table borders share one namespace; per-line clears are byte-range scoped
// via `clearVirtualLinesInRange`, so adjacent rows / distinct tables never
// collide and a tall table needs no whole-table rebuild.
const TABLE_BORDER_NS = "md-tb";

// Per-render column widths, keyed by a row's byte_start. Rebuilt at the top of
// every `lines_changed` pass (computeRowWidths) and read synchronously in the
// same pass by the conceal and border code.
let currentRowWidths: Map<number, number[]> = new Map();

// Width memo: each table carries a marker whose payload is its accumulated
// per-column max content widths — NUMBERS ONLY, no positions. The marker's byte
// range is editor-owned and auto-shifts; it is used only to associate a later
// `lines_changed` batch with the same table (a race-tolerant overlap query),
// never for rendering — rows/borders are still positioned from the live event.
//
// Why a memo at all: the plugin only ever sees the rows in the current batch
// (off-screen rows aren't readable synchronously), so column widths computed
// from a single partial batch (e.g. a mouse-wheel scroll that misses the widest
// row) come out narrower than a batch that includes it — and the two render at
// different widths side by side. Accumulating GROW-ONLY across batches makes
// the widths converge upward and stay consistent as a table scrolls into view.
// Widths are numbers, so the memo is immune to the marker/event desync that the
// per-line border model removed. The marker is reset on edits that touch the
// table (see after_insert/after_delete) so a narrowed cell isn't stuck wide.
const TABLE_WIDTH_NS_PREFIX = "tw"; // marker id prefix
let nextTableWidthId = 1;

// Allocated column widths for the table row starting at `byte`, for the current
// render pass. Undefined if it isn't a width-resolved table row this pass.
function widthsForRow(byte: number): number[] | undefined {
  const w = currentRowWidths.get(byte);
  return w && w.length ? w : undefined;
}

function isTableRowContent(content: string): boolean {
  const t = content.trim();
  return t.startsWith("|") || t.endsWith("|");
}

function isSepRowContent(content: string): boolean {
  return /^\|[-:\s|]+\|$/.test(content.trim());
}

function tableCells(content: string): string[] {
  let inner = content.trim();
  if (inner.startsWith("|")) inner = inner.slice(1);
  if (inner.endsWith("|")) inner = inner.slice(0, -1);
  return inner.split("|");
}

// Viewport-constrained per-column widths from accumulated max raw widths.
function allocatedFor(maxW: number[]): number[] {
  const viewport = editor.getViewport();
  const composeW = effectiveComposeWidth(viewport ? viewport.width : 80);
  const available = composeW - (maxW.length + 1);
  return distributeColumnWidths(maxW, available);
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
//   * Borders are virtual lines (no source bytes) anchored at a row's
//     byte_start, emitted PER LINE in the shared namespace `md-tb`. Each row's
//     frame is cleared (byte-range scoped) and re-added every time that line is
//     in a `lines_changed` batch — exactly like the per-line conceal pass.
//   * First/last/source-separator classification is local: it comes from the
//     row plus its immediate neighbours in the same batch (see emitRowBorders).
//   * Border column widths are this render's `widthsForRow`, the same widths the
//     conceal pass uses, so borders line up with the cell conceals.

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

// Theme keys (resolved at render time so borders follow theme changes — same
// pattern as addOverlay's fg/bg). fg → editor.fg matches the concealed
// `│`/`─` glyphs inside rows so the frame has no seam; bg → editor.bg blends
// with the page rather than carving an opaque slab.
const tableBorderOptions = { fg: "editor.fg", bg: "editor.bg" };

/** Remove table border virtual lines for this row, clearing its *whole content
 * range* `[byteStart, byteEnd)` — exactly like the per-line conceal clear.
 *
 * The frame is anchored at `byteStart`, but the clear must be range-wide, not a
 * single byte: under the async `lines_changed` lag a previously-emitted frame
 * rides a few bytes ahead of the event's `byteStart`, so a one-byte clear would
 * miss it and leave a stale doubled separator. A line-wide range tolerates that
 * lag the same way `clearConcealsInRange` does. The next row's frame (anchored
 * at this row's `byte_end`) is excluded by the half-open range, and re-emitted
 * by its own line. Called for *every* line in a batch — table or not — so a row
 * that stops being a table row loses its frame. */
function clearRowBorders(bufferId: number, byteStart: number, byteEnd: number): void {
  editor.clearVirtualLinesInRange(
    bufferId, TABLE_BORDER_NS, byteStart, Math.max(byteEnd, byteStart + 1),
  );
}

/** Emit this row's border frame, anchored at `byteStart`, for the current
 * render. Role is local: `isFirst`/`isLast` come from the row's immediate
 * neighbours in the same batch; `isSep`/`prevIsSep` skip the virtual separator
 * adjacent to the source `|---|` row (the conceals already render it). Pairs
 * with `clearRowBorders` (called first) so the clear+add land in one
 * `process_commands` batch — no one-frame strobe. */
function emitRowBorders(
  bufferId: number,
  byteStart: number,
  widths: number[],
  isFirst: boolean,
  isSep: boolean,
  prevIsSep: boolean,
  isLast: boolean,
): void {
  if (widths.length === 0) return;

  if (isFirst) {
    // Top border above the first row. ┌─┬─┐
    editor.addVirtualLine(
      bufferId, byteStart,
      buildTableBorderLine(widths, "┌", "┬", "┐"),
      tableBorderOptions, true, TABLE_BORDER_NS, 0,
    );
  } else if (!isSep && !prevIsSep) {
    // Inter-row separator above this row. ├─┼─┤
    editor.addVirtualLine(
      bufferId, byteStart,
      buildTableBorderLine(widths, "├", "┼", "┤"),
      tableBorderOptions, true, TABLE_BORDER_NS, 1,
    );
  }

  if (isLast) {
    // Bottom border below the last row. └─┴─┘
    editor.addVirtualLine(
      bufferId, byteStart,
      buildTableBorderLine(widths, "└", "┴", "┘"),
      tableBorderOptions, false, TABLE_BORDER_NS, 0,
    );
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

  // Trigger a refresh so lines_changed hooks fire for visible content
  editor.refreshLines(bufferId);
  editor.debug(`Markdown compose enabled for buffer ${bufferId}`);
}

// Disable compose mode for a buffer
function disableMarkdownCompose(bufferId: number): void {
  if (isComposing(bufferId)) {
    editor.setViewState(bufferId, "last-cursor-line", null);
    // Clear all table border virtual lines (one shared namespace) so the frame
    // can't linger as orphaned virtual lines after compose is toggled off, and
    // drop the per-table width memos.
    editor.clearVirtualTextNamespace(bufferId, TABLE_BORDER_NS);
    const memos = (editor.queryMarkers(bufferId, 0, 0x7fffffff) as Array<{ id: string }>) || [];
    for (const m of memos) {
      if (m.id.startsWith(TABLE_WIDTH_NS_PREFIX)) editor.deleteMarker(bufferId, m.id);
    }

    // Tell Rust side this buffer is back in source mode
    editor.setViewMode(bufferId, "source");

    // Re-enable line numbers
    editor.setLineNumbers(bufferId, true);

    // Clear layout hints, emphasis overlays, conceals, and soft breaks
    editor.setLayoutHints(bufferId, null, {});
    editor.clearNamespace(bufferId, "md-emphasis");
    editor.clearConcealNamespace(bufferId, "md-syntax");
    editor.clearSoftBreakNamespace(bufferId, "md-wrap");

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

interface InlineSpan {
  type: 'code' | 'bold-italic' | 'bold' | 'italic' | 'strikethrough' | 'link' | 'entity';
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

// Terminal column width (wide glyphs = 2), matching the renderer's layout.
function displayWidth(text: string): number {
  return editor.stringWidth(text);
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

/**
 * W3C-inspired column width distribution.
 * Constrains columns to fit within `available` width, distributing space
 * proportionally to each column's natural (max) width.
 */
function distributeColumnWidths(maxW: number[], available: number): number[] {
  const numCols = maxW.length;
  const total = maxW.reduce((s, w) => s + w, 0);
  if (total <= available) return maxW;
  if (numCols * MIN_COL_W >= available) return maxW.map(() => MIN_COL_W);

  const remaining = available - numCols * MIN_COL_W;
  const excess = maxW.reduce((s, w) => s + Math.max(0, w - MIN_COL_W), 0);
  return maxW.map(w => {
    const extra = excess > 0 ? Math.floor(remaining * Math.max(0, w - MIN_COL_W) / excess) : 0;
    return MIN_COL_W + extra;
  });
}

/**
 * Wrap text into lines of at most `width` characters, breaking at word boundaries.
 */
function wrapText(text: string, width: number): string[] {
  if (width <= 0 || text.length <= width) return [text];
  const lines: string[] = [];
  let pos = 0;
  while (pos < text.length) {
    if (pos + width >= text.length) {
      lines.push(text.slice(pos));
      break;
    }
    let breakAt = text.lastIndexOf(' ', pos + width);
    if (breakAt <= pos) {
      breakAt = pos + width;
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
  editor.debug(`[mc] processLine clear+rebuild bytes=${byteStart}..${byteEnd} content="${lineContent.slice(0,40)}"`);
  editor.clearConcealsInRange(bufferId, byteStart, byteEnd);
  // Only clear our own emphasis overlays — clearing ALL overlays in the range
  // would also wipe editor-owned overlays like LSP diagnostics (issue #2146).
  editor.clearOverlaysInRangeForNamespace(bufferId, "md-emphasis", byteStart, byteEnd);

  const cursorOnLine = cursors.some(c => c >= byteStart && c <= byteEnd);
  // Strict version: excludes the boundary at byteEnd so that the cursor
  // sitting at the start of the *next* line doesn't count as being on
  // *this* line.  Used for table row auto-expose to avoid exposing the
  // previous row's emphasis markers.
  const cursorStrictlyOnLine = cursors.some(c => c >= byteStart && c < byteEnd);

  // Skip lines inside code fences (we'd need multi-line context for this;
  // for now, detect fence lines and code content lines)
  const trimmed = lineContent.trim();
  if (trimmed.startsWith('```')) return; // fence line itself

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

    // Column widths come from this render's per-row width map (computed from the
    // batch's table groups at the top of the lines_changed pass).
    const colWidths = widthsForRow(byteStart);

    // Split the line into cells to compute per-cell padding
    let inner = trimmed;
    if (inner.startsWith('|')) inner = inner.slice(1);
    if (inner.endsWith('|')) inner = inner.slice(0, -1);
    const cells = inner.split('|');

    // Check if any data cell needs multi-line wrapping
    let handledByWrapping = false;
    if (colWidths && !isSeparator && !cursorStrictlyOnLine) {
      const numCols = Math.min(cells.length, colWidths.length);
      const cellWrapped: string[][] = [];
      let maxVisualLines = 1;
      for (let ci = 0; ci < numCols; ci++) {
        // When cursor is on the row, use raw text (emphasis markers revealed).
        const cellText = cursorStrictlyOnLine ? cells[ci].trim() : concealedText(cells[ci]).trim();
        const wrapW = Math.max(1, colWidths[ci] - 2); // 1 leading + 1 trailing space margin
        const wrapped = wrapText(cellText, wrapW);
        cellWrapped.push(wrapped);
        maxVisualLines = Math.max(maxVisualLines, wrapped.length);
      }
      // Cap to available source bytes (excluding trailing newline)
      let effLen = lineContent.length;
      if (effLen > 0 && lineContent[effLen - 1] === '\n') effLen--;
      if (effLen > 0 && lineContent[effLen - 1] === '\r') effLen--;
      maxVisualLines = Math.min(maxVisualLines, effLen);

      if (maxVisualLines > 1) {
        // Build formatted visual line for each wrapped row
        const visualLines: string[] = [];
        for (let vl = 0; vl < maxVisualLines; vl++) {
          let vline = '│';
          for (let ci = 0; ci < numCols; ci++) {
            const wrapW = Math.max(1, colWidths[ci] - 2);
            const wrapped = cellWrapped[ci] || [];
            const text = vl < wrapped.length ? wrapped[vl] : '';
            vline += ' ' + text + ' '.repeat(Math.max(0, wrapW - displayWidth(text))) + ' │';
          }
          visualLines.push(vline);
        }

        // Divide source bytes into segments, one per visual line.
        // Soft breaks at segment boundaries (added by processLineSoftBreaks)
        // create the visual line breaks; conceals replace each segment.
        //
        // IMPORTANT: break positions MUST land on Space characters.
        // Space tokens have individual source_offset values matching their
        // byte positions, so soft breaks will reliably trigger. Non-space
        // characters inside Text tokens share the token's START offset,
        // so breaks at mid-token positions silently fail.
        // The consumed space (replaced by Newline) must NOT be covered by
        // any segment's conceal range, so segment N+1 starts at spacePos+1.
        // Exclude trailing newline from segment range so the Newline token
        // at the end of the source line is NOT concealed (preserves the
        // line break between adjacent source rows).
        let lineCharLen = lineContent.length;
        if (lineCharLen > 0 && lineContent[lineCharLen - 1] === '\n') lineCharLen--;
        if (lineCharLen > 0 && lineContent[lineCharLen - 1] === '\r') lineCharLen--;
        const spacePositions: number[] = [];
        for (let i = 1; i < lineCharLen; i++) {
          if (lineContent[i] === ' ') spacePositions.push(i);
        }
        const breakChars = spacePositions.slice(0, maxVisualLines - 1);
        // Trim visual lines if we couldn't find enough break positions
        const actualVisualLines = breakChars.length + 1;
        // Segments: first starts at 0, subsequent start AFTER the consumed space
        const segStarts = [0, ...breakChars.map(c => c + 1)];
        const segEnds = [...breakChars, lineCharLen];
        for (let vl = 0; vl < actualVisualLines; vl++) {
          const sByteS = charToByte(lineContent, segStarts[vl], byteStart);
          const sByteE = charToByte(lineContent, segEnds[vl], byteStart);
          editor.addConceal(bufferId, "md-syntax", sByteS, sByteE, visualLines[vl] || '');
        }
        handledByWrapping = true;
      }
    }

    if (!handledByWrapping) {
      // Find pipe positions for byte-range computation of truncated cells
      const pipePositions: number[] = [];
      for (let i = 0; i < lineContent.length; i++) {
        if (lineContent[i] === '|') pipePositions.push(i);
      }

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
        if (lineContent[i] === '|') {
          const pipeByte = charToByte(lineContent, i, byteStart);
          const pipeByteEnd = charToByte(lineContent, i + 1, byteStart);

          // Compute padding (and, off the cursor row, truncation) for the cell
          // that just ended.
          //
          // Columns are sized to the widest *raw* cell, so a row only aligns
          // when its rendered cell is padded out to that width. The row the
          // cursor is on renders raw (emphasis markers revealed), so it must be
          // padded too — otherwise that row's cell collapses to its natural
          // width and its borders fall out of the frame as the cursor passes
          // through it (a very visible "the table breaks under the cursor"
          // glitch). Trailing padding never hides content, so it is safe on the
          // cursor row; we still skip *truncation* there so a too-wide cell
          // stays fully visible for editing.
          let padding = "";
          const cellIdx = pipeIdx - 1;
          if (colWidths && pipeIdx > 0 && cellIdx < cells.length && cellIdx < colWidths.length) {
            // The cursor row shows raw text (markers revealed), so measure its
            // raw width; every other row shows concealed text.
            const cellText = cursorStrictlyOnLine ? cells[cellIdx] : concealedText(cells[cellIdx]);
            const cellWidth = displayWidth(cellText);
            const allocatedWidth = colWidths[cellIdx];

            if (cellWidth > allocatedWidth) {
              if (!cursorStrictlyOnLine) {
                // Truncate: conceal entire cell content and replace with truncated text.
                // Separator rows use box-drawing ─ to match the non-truncated path
                // (per-char conceals replace source `-` with ─ and pad via pipe replacement).
                const prevPipeCharPos = pipePositions[pipeIdx - 1];
                const cellByteStart = charToByte(lineContent, prevPipeCharPos + 1, byteStart);
                const cellByteEnd = pipeByte;
                const truncated = isSeparator
                  ? '─'.repeat(allocatedWidth)
                  : cellText.slice(0, allocatedWidth - 1) + '-';
                editor.addConceal(bufferId, "md-syntax", cellByteStart, cellByteEnd, truncated);
                truncatedByteRanges.push({start: cellByteStart, end: cellByteEnd});
              }
              // On the cursor row a too-wide cell is left raw (no padding, no
              // truncation) so its full content stays editable.
            } else {
              const padCount = allocatedWidth - cellWidth;
              if (padCount > 0) {
                padding = isSeparator ? "─".repeat(padCount) : " ".repeat(padCount);
              }
            }
          }

          if (isSeparator) {
            const pipeIndex = lineContent.substring(0, i + 1).split('|').length - 1;
            const totalPipes = lineContent.split('|').length - 1;
            let replacement = '┼';
            if (pipeIndex === 1) replacement = '├';
            else if (pipeIndex === totalPipes) replacement = '┤';
            editor.addConceal(bufferId, "md-syntax", pipeByte, pipeByteEnd, padding + replacement);
          } else {
            editor.addConceal(bufferId, "md-syntax", pipeByte, pipeByteEnd, padding + "│");
          }
          pipeIdx++;
        } else if (isSeparator && lineContent[i] === '-') {
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

  // Table row wrapping: add soft breaks for multi-line cells
  if (block.type === 'table-row') {
    const trimmedLine = lineContent.trim();
    const isSep = /^\|[-:\s|]+\|$/.test(trimmedLine);
    if (!isSep) {
      const colWidths = widthsForRow(byteStart);
      if (colWidths) {
        let innerLine = trimmedLine;
        if (innerLine.startsWith('|')) innerLine = innerLine.slice(1);
        if (innerLine.endsWith('|')) innerLine = innerLine.slice(0, -1);
        const tableCells = innerLine.split('|');
        let maxVisualLines = 1;
        const numCols = Math.min(tableCells.length, colWidths.length);
        const cursorOnTableLine = cursors.some(c => c >= byteStart && c < byteEnd);
        for (let ci = 0; ci < numCols; ci++) {
          const cellText = cursorOnTableLine ? tableCells[ci].trim() : concealedText(tableCells[ci]).trim();
          const wrapW = Math.max(1, colWidths[ci] - 2);
          const wrapped = wrapText(cellText, wrapW);
          maxVisualLines = Math.max(maxVisualLines, wrapped.length);
        }
        // Exclude trailing newline (same as processLineConceals)
        let effLineLen = lineContent.length;
        if (effLineLen > 0 && lineContent[effLineLen - 1] === '\n') effLineLen--;
        if (effLineLen > 0 && lineContent[effLineLen - 1] === '\r') effLineLen--;
        maxVisualLines = Math.min(maxVisualLines, effLineLen);

        if (maxVisualLines > 1) {
          // Must match the break positions from processLineConceals:
          // pick Space chars (they have individual source_offsets that match).
          const spacePositions: number[] = [];
          for (let i = 1; i < effLineLen; i++) {
            if (lineContent[i] === ' ') spacePositions.push(i);
          }
          const breakChars = spacePositions.slice(0, maxVisualLines - 1);
          for (const charPos of breakChars) {
            const breakBytePos = byteStart + editor.utf8ByteLength(lineContent.slice(0, charPos));
            editor.addSoftBreak(bufferId, "md-wrap", breakBytePos, 0);
          }
        }
      }
    }
  }

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

/** Group consecutive table rows in a `lines_changed` batch (adjacency by
 * line_number). Each group is one table's currently-visible run; column widths
 * are uniform within a group. */
function groupTableRows(lines: LineInfoLike[]): LineInfoLike[][] {
  const groups: LineInfoLike[][] = [];
  let cur: LineInfoLike[] = [];
  let lastLn = -2;
  for (const line of lines) {
    if (isTableRowContent(line.content) && line.line_number === lastLn + 1) {
      cur.push(line);
    } else if (isTableRowContent(line.content)) {
      if (cur.length) groups.push(cur);
      cur = [line];
    } else {
      if (cur.length) groups.push(cur);
      cur = [];
    }
    lastLn = line.line_number;
  }
  if (cur.length) groups.push(cur);
  return groups;
}

/** Populate `currentRowWidths` for this render: one allocated-width array per
 * table row in the batch, uniform within each table.
 *
 * Column widths are accumulated GROW-ONLY in the table's width memo (a
 * widths-only marker, see TABLE_WIDTH_NS_PREFIX) so a partial batch that misses
 * the table's widest row doesn't lay its rows out narrower than a batch that
 * includes it. The marker's coordinates are only ever used to find the same
 * table's memo across batches (overlap query) and to extend its span as more of
 * a tall table scrolls into view; rows and borders are still positioned from the
 * live event, so an offset memo marker can at worst contribute stale *numbers*
 * (recovered on the next batch), never a misplaced border.
 *
 * Returns true if any table's accumulated widths grew this batch — the caller
 * then forces one `refreshLines` so already-visible rows (which were laid out at
 * the old, narrower width before the wider row scrolled in) re-render at the new
 * width. The follow-up pass finds the memo unchanged, so it does not loop. */
function computeRowWidths(bufferId: number, lines: LineInfoLike[]): boolean {
  currentRowWidths = new Map();
  let grew = false;
  for (const group of groupTableRows(lines)) {
    const gStart = group[0].byte_start;
    const gEnd = group[group.length - 1].byte_end;

    // This batch's per-column max content width.
    const batchMaxW: number[] = [];
    for (const line of group) {
      const isSep = isSepRowContent(line.content);
      const cells = tableCells(line.content);
      for (let c = 0; c < cells.length; c++) {
        // Separator-row cells (`---`) adapt to data rows: width 0. Use RAW
        // display width (not concealed) so columns fit revealed emphasis markers
        // and wide/CJK/emoji cells.
        const w = isSep || /^[-:\s]+$/.test(cells[c]) ? 0 : displayWidth(cells[c]);
        batchMaxW[c] = Math.max(batchMaxW[c] ?? 0, w);
      }
    }

    // Find this table's width memo (overlap query; write-through means two
    // groups of one table in the same batch share it). Consolidate duplicates.
    const near = (editor.queryMarkers(bufferId, gStart - 1, gEnd + 1) as Array<{
      id: string; start: number; end: number; payload: unknown;
    }>) || [];
    const memo = near.filter((m) => m.id.startsWith(TABLE_WIDTH_NS_PREFIX));
    const existing = memo.length ? memo[0] : undefined;
    for (let k = 1; k < memo.length; k++) editor.deleteMarker(bufferId, memo[k].id);

    // Accumulate (grow-only).
    const acc: number[] = [];
    if (existing) {
      const p = (existing.payload || {}) as { maxW?: number[] };
      for (const w of p.maxW ?? []) acc.push(w);
    }
    const prevLen = acc.length;
    for (let c = 0; c < batchMaxW.length; c++) {
      const before = acc[c] ?? 0;
      acc[c] = Math.max(before, batchMaxW[c]);
      if (acc[c] > before || c >= prevLen) grew = true;
    }

    // Upsert. Extend the marker's span to cover every row seen so far, so the
    // next batch (further down a tall table) still overlaps and finds this memo
    // instead of starting a second one.
    const id = existing ? existing.id : `${TABLE_WIDTH_NS_PREFIX}${nextTableWidthId++}`;
    const start = existing ? Math.min(existing.start, gStart) : gStart;
    const end = existing ? Math.max(existing.end, gEnd) : gEnd;
    editor.createMarker(bufferId, id, start, end, { maxW: acc });

    const widths = allocatedFor(acc);
    for (const line of group) currentRowWidths.set(line.byte_start, widths);
  }
  return grew;
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

  // Column widths for every table row in this batch (uniform per table, via the
  // grow-only memo). If a wider row scrolled into view and grew a table's
  // columns, force a refresh so already-visible rows re-render at the new width.
  const tableWidthsGrew = computeRowWidths(data.buffer_id, data.lines);

  // Line-number → line, for local first/last-row classification of borders.
  const byLineNum = new Map<number, LineInfoLike>();
  for (const line of data.lines) byLineNum.set(line.line_number, line);

  // Per line: clear+rebuild conceals, soft-breaks, and the table border frame —
  // all anchored to this one line. No whole-table rebuild, no stored row model;
  // borders for lines not in this batch keep riding their auto-shift markers.
  for (const line of data.lines) {
    // Clear this row's border range first (covers a row that stopped being a
    // table row, e.g. its pipes were deleted, and stale frames left a few bytes
    // off by the async lag).
    clearRowBorders(data.buffer_id, line.byte_start, line.byte_end);

    processLineConceals(data.buffer_id, line.content, line.byte_start, line.byte_end, cursors, line.line_number);
    processLineSoftBreaks(data.buffer_id, line.content, line.byte_start, line.byte_end, cursors, line.line_number);

    if (isTableRowContent(line.content)) {
      const widths = currentRowWidths.get(line.byte_start) ?? [];
      const prev = byLineNum.get(line.line_number - 1);
      const next = byLineNum.get(line.line_number + 1);
      // First/last is local. A row is first if it's the buffer's line 0 or its
      // previous line is present in this batch and is NOT a table row; last if
      // its next line is present and not a table row. When a neighbour is
      // off-screen (absent from the batch) we conservatively treat the row as
      // mid-table, so a tall table scrolled past its top/bottom never draws a
      // spurious frame edge — it redraws when that neighbour re-enters a batch.
      const isFirst = line.line_number === 0 || (prev !== undefined && !isTableRowContent(prev.content));
      const isLast = next !== undefined && !isTableRowContent(next.content);
      const isSep = isSepRowContent(line.content);
      const prevIsSep = prev !== undefined && isSepRowContent(prev.content);
      emitRowBorders(data.buffer_id, line.byte_start, widths, isFirst, isSep, prevIsSep, isLast);
    }
  }

  if (tableWidthsGrew) {
    editor.refreshLines(data.buffer_id);
  }
});
// after_insert / after_delete: conceals and borders need no work — an edit
// invalidates `seen_byte_ranges`, so `lines_changed` re-fires for the affected
// lines and the per-line pass clears+rebuilds them; unaffected rows' borders
// auto-shift. We only reset the *width memo* of a table the edit touched, so a
// cell that just got narrower (or a row removed) isn't stuck at the old wide
// column. The memo re-accumulates from scratch on the following render.
function resetEditedTableWidths(bufferId: number, affStart: number, affEnd: number): void {
  const near = (editor.queryMarkers(bufferId, affStart, affEnd) as Array<{
    id: string; start: number; end: number; payload: unknown;
  }>) || [];
  for (const m of near) {
    if (m.id.startsWith(TABLE_WIDTH_NS_PREFIX)) editor.deleteMarker(bufferId, m.id);
  }
}
editor.on("after_insert", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  resetEditedTableWidths(data.buffer_id, data.affected_start, data.affected_end);
});
editor.on("after_delete", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  resetEditedTableWidths(data.buffer_id, data.affected_start, data.affected_start);
});
editor.on("cursor_moved", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;

  const prevLine = editor.getViewState(data.buffer_id, "last-cursor-line") as number | undefined;
  editor.setViewState(data.buffer_id, "last-cursor-line", data.line);

  editor.debug(`[mc] cursor_moved: old_pos=${data.old_position} new_pos=${data.new_position} line=${data.line} prevLine=${prevLine}`);

  // Refresh all visible lines so span-level auto-expose (revealing the markup
  // the cursor sits on) and table-row un/re-wrap stay consistent across the
  // whole viewport, including intra-line moves.
  //
  // This re-fires `lines_changed` for the viewport. Tables are emitted per line
  // (clear+rebuild each row's conceals and border frame from the live event),
  // so repeated refreshes are idempotent and the frame stays correct no matter
  // how often the cursor moves.
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

  // Refresh all visible lines: the per-line pass recomputes column widths for
  // the new viewport width (allocatedFor reads the live viewport) and re-emits
  // each row's border frame to match.
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
