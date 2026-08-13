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
  // Page width for compose mode, as a three-state value:
  //   * `undefined` — the user has not chosen one for this session; resolve it
  //     from the editor config (see `configuredPageWidth`). This is the
  //     startup state, so compose defaults to a readable measure.
  //   * `number`    — an explicit width in columns.
  //   * `null`      — explicitly full viewport width ("None" in the
  //                   Set Compose Width prompt).
  // The `undefined`/`null` distinction is what lets a config-driven default
  // exist without overriding a user who deliberately asked for full width.
  composeWidth: number | null | undefined;
  maxWidth: number;
  hideLineNumbers: boolean;
}

const config: MarkdownConfig = {
  composeWidth: undefined,
  maxWidth: 100,
  hideLineNumbers: true,
};

// Fallback measure when the editor config can't be read at all. Matches the
// editor's own `editor.page_width` default (see `default_page_width` in
// config.rs) so the two never disagree.
const FALLBACK_PAGE_WIDTH = 80;

/** Shape of the bits of the resolved editor config this plugin reads. */
interface PageWidthConfig {
  editor?: { page_width?: number | null };
  languages?: Record<string, { page_width?: number | null } | undefined>;
}

/**
 * The compose width the *config* asks for, in columns, or `null` for the full
 * viewport width.
 *
 * Mirrors the Rust resolution in `buffer_config_resolve::page_view`:
 * `languages.markdown.page_width` wins over the global `editor.page_width`,
 * and a language-level `0` means "unset at this level" (inherit the global)
 * rather than "full width" — the same rule `Config::normalize_zero_sentinels`
 * applies. A global `0`/`null` *does* mean full width, which is how the
 * pre-default behaviour stays reachable.
 */
function configuredPageWidth(): number | null {
  const cfg = editor.getConfig() as PageWidthConfig | null;
  if (!cfg || typeof cfg !== "object") return FALLBACK_PAGE_WIDTH;

  const lang = cfg.languages?.markdown?.page_width;
  if (typeof lang === "number" && lang > 0) return lang;

  const global = cfg.editor?.page_width;
  if (typeof global === "number") return global > 0 ? global : null;
  if (global === null) return null;

  return FALLBACK_PAGE_WIDTH;
}

/**
 * The compose width in effect right now: an explicit session choice if the
 * user made one, else whatever the config resolves to.
 */
function activeComposeWidth(): number | null {
  if (config.composeWidth !== undefined) return config.composeWidth;
  return configuredPageWidth();
}

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
  region?: RegionLine;
};

// =============================================================================
// Fenced code blocks: framed from the editor's own region classification
// =============================================================================
//
// A code block gets the same treatment a table does — its delimiters concealed
// into a box-drawing frame, its body left to the syntax highlighter (embedded
// fence highlighting has worked since issue #2689, so the body already renders
// in the fence's own language).
//
// The one thing that is NOT like a table: "is this line a table row" is decided
// from that line alone, but "is this line inside a fence" is not. A bare ```
// opens or closes depending on every fence above it, `lines_changed` batches
// carry only the lines an edit or scroll touched, and the plugin cannot read
// the buffer above its batch synchronously (`getBufferText` is a Promise, and
// the decoration pass has to emit its clear+add in one command batch).
//
// Deriving it plugin-side is therefore not possible correctly: any batch-local
// rule frames a block whose opening fence happens to be visible and gives up on
// one that isn't, so the frame appears and disappears with scroll position.
// Nor can it be memoised — the table width memo is safe *because* it carries
// only numbers, where staleness costs a column width for one frame; a memo of
// fence extents would carry structure, and staleness there means framing the
// wrong lines.
//
// So the editor answers it. `line.region` on the `lines_changed` payload is the
// classification the highlighting engine already makes per line while parsing
// (`open` / `body` / `close`, absent outside a region) — the same source of
// truth that colours the block's contents, so the frame can never disagree with
// the highlighting. It arrives with the batch, alongside coordinates the plugin
// already trusts, so there is nothing to store and nothing to go stale.
//
// `region` is absent both for ordinary lines and when the editor could not
// resolve the state (a >1MiB buffer whose viewport has no parse checkpoint
// before it yet). Absence therefore means *unknown*, and an unknown fence line
// is left rendering literally — the same conservative choice the table frame
// makes when a neighbouring row is off-screen.
type RegionLine = "open" | "body" | "close";

/** Whether a line is inside a fenced code block (delimiters included). */
function isCodeRegion(region: RegionLine | undefined): boolean {
  return region !== undefined;
}

/** Whether a line reads as a fence delimiter from its own text alone.
 *
 * Only ever used to decide whether an *edit* may have restructured the
 * document (see `fenceEditArmed`) — never to decide how a line renders, which
 * is what `region` is for. It has to be textual precisely because it must also
 * fire for a line that has just STOPPED being a delimiter. */
function looksLikeFence(content: string): boolean {
  return /^\s*(?:`{3,}|~{3,})/.test(content);
}

// Region membership is the one per-line property an edit can change for lines
// it does not touch: appending a word to a closing ``` (CommonMark forbids an
// info string on a closer) turns every line below it into code. The editor
// re-fires `lines_changed` only for the edited line when the edit doesn't
// change the line count — a structural edit already forces a whole-viewport
// refresh in `event_apply` — so those lines below would keep the frame they
// were last rendered with, disagreeing with the highlighting, which does
// re-converge.
//
// Two narrow triggers force the one extra whole-viewport pass that repairs it,
// between them covering both directions:
//
//   * the edit put a fence character into, or took one out of, the buffer —
//     the only way to create or destroy a delimiter outright
//     (`touchesFenceChars`, checked on the edit itself because the *deleted*
//     text is not visible in any later batch);
//   * the edited line is still delimiter-shaped afterwards — the ``` that just
//     stopped closing because a word was appended to it (`fenceEditArmed`,
//     checked on the batch, since only there is the line's new text known).
//
// Deliberately NOT "the edited line is inside a block": that is true of every
// keystroke while typing code, and would put a whole-viewport refresh behind
// each one. Typing that restructures a block always types a backtick.
//
// The armed flag is disarmed by the batch that acts on it, so the refresh's own
// whole-viewport batch cannot re-arm it.
const fenceEditArmed = new Set<number>();

/** Whether an edit's text could create or destroy a fence delimiter. */
function touchesFenceChars(text: string): boolean {
  return /[`~]/.test(text);
}

/** The info string of an opening fence — its language — or "" when bare.
 *
 * CommonMark's first word of the info string; the engine resolves the same
 * token to a grammar (`resolve_embedded_syntax`), so the label names exactly
 * the language the body is being highlighted as. */
function fenceInfoString(content: string): string {
  const m = content.match(/^\s*(?:`{3,}|~{3,})\s*(\S+)/);
  return m ? m[1] : "";
}

/** A horizontal code-block frame line of the given corner style, filling the
 * page measure. Two columns short of the measure for the same reason the
 * thematic-break rule is (see `wrapBudget`): the renderer reserves an
 * end-of-line column, and a border that reached it would wrap. */
function buildCodeFrameLine(measure: number, label: string, left: string, right: string): string {
  const inner = Math.max(1, measure - 2 - displayWidth(left) - displayWidth(right));
  if (label) {
    const tag = `─ ${label} `;
    const tagW = displayWidth(tag);
    if (tagW < inner) return left + tag + "─".repeat(inner - tagW) + right;
  }
  return left + "─".repeat(inner) + right;
}

// fg matches the table frame's so the two kinds of block read as one system.
const codeFrameStyle = { fg: "editor.fg" };

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

// =============================================================================
// Heading markers on the scrollbar
// =============================================================================
//
// Headings are marked on the scrollbar track so a document's structure is
// visible at a glance and off-screen sections can be located without
// scrolling. This rides the same per-line pass as conceals and borders, and
// uses the *range-scoped* form of the API for the same reason the conceal
// clears are range-scoped:
//
//   * `lines_changed` only ever reports the lines the editor decided to
//     (re)process — typically the viewport, or the lines an edit touched. A
//     whole-namespace replace would therefore delete the markers for every
//     heading currently off-screen, and the scrollbar would show only the
//     headings near the viewport.
//   * `setScrollbarMarkersInRange` replaces only the markers anchored inside
//     the batch's byte span, so headings scrolled past keep their marks and
//     coverage accumulates as the document is explored. Editor-side byte
//     anchors then shift those accumulated markers through later edits, so
//     nothing goes stale between batches.
//
// Marks are emitted for every batch — including batches with no headings —
// so a line that stops being a heading loses its mark.
const HEADING_MARKER_NS = "md-headings";

// Heading level → marker color. Theme keys resolve at render time, so the
// marks follow theme changes like the emphasis overlays do.
const HEADING_MARKER_COLORS: string[] = [
  "syntax.keyword",  // # h1
  "syntax.function", // ## h2
  "syntax.type",     // ### h3+
];

function headingMarkerColor(level: number): string {
  return HEADING_MARKER_COLORS[Math.min(level, HEADING_MARKER_COLORS.length) - 1];
}

/** Heading level of a source line (1-6), or 0 when it isn't an ATX heading. */
function headingLevel(content: string): number {
  const m = content.match(/^\s*(#{1,6})\s+\S/);
  return m ? m[1].length : 0;
}

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

/** Whether a source line is a list item, for the inter-item spacing pass.
 * Indent widening doesn't matter here, so the measure is irrelevant. */
function isListItemContent(content: string): boolean {
  return listItemInfo(content, 0) !== null;
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

  // The editor auto-stamps the lines_changed epoch onto each addVirtualLine and
  // remaps the anchor forward before placing it (see CoordMap) — the plugin
  // passes no epoch.
  const opts = tableBorderOptions;

  if (isFirst) {
    // Top border above the first row. ┌─┬─┐
    editor.addVirtualLine(
      bufferId, byteStart,
      buildTableBorderLine(widths, "┌", "┬", "┐"),
      opts, true, TABLE_BORDER_NS, 0,
    );
  } else if (!isSep && !prevIsSep) {
    // Inter-row separator above this row. ├─┼─┤
    editor.addVirtualLine(
      bufferId, byteStart,
      buildTableBorderLine(widths, "├", "┼", "┤"),
      opts, true, TABLE_BORDER_NS, 1,
    );
  }

  if (isLast) {
    // Bottom border below the last row. └─┴─┘
    editor.addVirtualLine(
      bufferId, byteStart,
      buildTableBorderLine(widths, "└", "┴", "┘"),
      opts, false, TABLE_BORDER_NS, 0,
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

  // Set layout hints for centered margins. With no explicit session choice
  // this resolves to the configured page width (default 80), so compose opens
  // as a readable measure rather than running the full pane width.
  editor.setLayoutHints(bufferId, null, { composeWidth: activeComposeWidth() ?? undefined });

  // Trigger a refresh so lines_changed hooks fire for visible content
  editor.refreshLines(bufferId);
  editor.debug(`Markdown compose enabled for buffer ${bufferId}`);
}

// Disable compose mode for a buffer
function disableMarkdownCompose(bufferId: number): void {
  if (isComposing(bufferId)) {
    // Clear all table border virtual lines (one shared namespace) so the frame
    // can't linger as orphaned virtual lines after compose is toggled off, and
    // drop the per-table width memos.
    editor.clearVirtualTextNamespace(bufferId, TABLE_BORDER_NS);
    // Same for the inter-list-item spacer rows, so they can't linger as
    // orphaned virtual lines after compose is toggled off.
    editor.clearVirtualTextNamespace(bufferId, LIST_SPACING_NS);
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
    editor.clearScrollbarMarkers(bufferId, HEADING_MARKER_NS);

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


// =============================================================================
// Line-level conceal/overlay processing
// =============================================================================
// Conceals and overlays are managed per-line using targeted range-based clearing.
// The lines_changed hook processes newly visible or edited lines.
// The after_insert/after_delete hooks clear affected byte ranges.
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
  const cw = activeComposeWidth();
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

// =============================================================================
// Heading text styling
// =============================================================================
//
// Headings ride the same conceal + overlay pass as emphasis: the `#` run is
// concealed (so the rendered line is just its text) and the text carries an
// overlay chosen by level. Both are per-line and stateless, so headings need
// none of the block bookkeeping tables need.
//
// This is deliberately separate from the scrollbar heading marks
// (HEADING_MARKER_NS above), which answer a different question — "where are
// the headings in the whole document" rather than "how does this line look".
//
// Theme keys resolve at render time, so heading colors follow theme changes
// the way the emphasis overlays do. Levels descend in visual weight: the top
// three are bold and colour-coded, deeper ones lighten to italic so a heading
// still reads as structure without shouting.
const HEADING_TEXT_STYLES: Array<Record<string, unknown>> = [
  { fg: "syntax.keyword", bold: true },                // #
  { fg: "syntax.function", bold: true },               // ##
  { fg: "syntax.type", bold: true },                   // ###
  { fg: "syntax.type", bold: true, italic: true },     // ####
  { fg: "syntax.constant", bold: true, italic: true }, // #####
  { fg: "syntax.constant", italic: true },             // ######
];

// =============================================================================
// List rendering
// =============================================================================
//
// Three things, each decided from a single source line so the whole pass stays
// per-line and stateless:
//   * the `-`/`*`/`+` bullet becomes a real bullet glyph (1 char for 1 char,
//     so nothing shifts),
//   * a nested item's leading indent is widened, so nesting depth is legible
//     at a glance rather than by counting two-space steps,
//   * each item gets a blank row after it (see LIST_SPACING_NS).
//
// The indent widening is the one piece that has to be known by *both* decoration
// passes: the conceal pass renders it, and the soft-break pass has to charge the
// wider indent against the wrap budget and use it as the hanging indent, or a
// wrapped nested item's continuation rows would not line up under its text and
// could run past the measure. `listItemInfo` is the single source of truth both
// call.
const LIST_BULLET = "•";

// Nested indents are scaled rather than stepped by a fixed amount because the
// source's own step is unknown — 2- and 4-space nesting are both common, and
// scaling preserves relative depth either way. Capped at a quarter of the
// measure so a deeply nested list can't push its text off the page.
const LIST_INDENT_SCALE = 2;

interface ListLineInfo {
  sourceIndent: number;  // chars of leading whitespace
  renderedIndent: number; // columns it renders as
  bullet: string | null;  // the `-`/`*`/`+` char, or null for an ordered item
  markerLen: number;      // chars of marker + the whitespace after it
}

function expandListIndent(sourceIndent: number, measure: number): number {
  if (sourceIndent === 0) return 0;
  const cap = Math.max(sourceIndent, Math.floor(measure / 4));
  return Math.min(sourceIndent * LIST_INDENT_SCALE, cap);
}

/** List-item structure for a source line, or null when it isn't one.
 *
 * Requires whitespace and then non-space content after the marker, so a line
 * that merely starts with `*` (e.g. `*emphasis*`) is not treated as a bullet.
 * Ordered markers follow `parseMarkdownBlocks`' `\d+.` form. Thematic breaks
 * never reach here — they are handled and returned earlier in the pass. */
function listItemInfo(content: string, measure: number): ListLineInfo | null {
  const m = content.match(/^(\s*)([-*+]|\d+\.)([ \t]+)(?=\S)/);
  if (!m) return null;
  const sourceIndent = m[1].length;
  return {
    sourceIndent,
    renderedIndent: expandListIndent(sourceIndent, measure),
    bullet: /^[-*+]$/.test(m[2]) ? m[2] : null,
    markerLen: m[2].length + m[3].length,
  };
}

// Blank rows between consecutive list items. Emitted per line in their own
// namespace and cleared range-scoped for *every* line in a batch — exactly the
// discipline the table border frame uses, and for the same reason: a line that
// stops being a list item has to lose its spacer, and a spacer that rides a few
// bytes ahead under the async `lines_changed` lag must still be caught by the
// clear.
const LIST_SPACING_NS = "md-ls";
const listSpacingOptions = { bg: "editor.bg" };
const listBulletStyle = { fg: "syntax.keyword" };

// Block-quote rendering. The bar is a left half-block: it fills the full cell
// height, so consecutive quote lines join into an unbroken vertical rule the
// way a rendered blockquote's border does. Theme keys resolve at render time,
// like the emphasis overlays.
const QUOTE_BAR = "▌";
const quoteBarStyle = { fg: "syntax.type" };
const quoteTextStyle = { fg: "syntax.comment", italic: true };

function headingTextStyle(level: number): Record<string, unknown> {
  const idx = Math.min(Math.max(level, 1), HEADING_TEXT_STYLES.length) - 1;
  return HEADING_TEXT_STYLES[idx];
}

/** `[indent, markerLen, level]` for an ATX heading line, else null.
 *
 * `markerLen` covers the `#` run *and* the spaces after it, so concealing
 * `[indent, indent + markerLen)` leaves the heading text starting at the
 * line's left edge. Requires at least one space and some non-space text after
 * the hashes, so a bare `#` or a `#hashtag` is left alone — matching how
 * CommonMark reads them. */
/** Byte offset of the end of a line's *content*, excluding any trailing
 * newline.
 *
 * `lines_changed` line content can carry its terminator, and a conceal whose
 * range covers the newline swallows the line break — the concealed line and
 * the one after it render as a single row. Whole-line conceals must therefore
 * stop here rather than at the reported `byte_end`. Same trailing-CR/LF trim
 * the table wrapping path does for its segment ranges. */
function lineContentEndByte(lineContent: string, byteStart: number): number {
  let len = lineContent.length;
  if (len > 0 && lineContent[len - 1] === '\n') len--;
  if (len > 0 && lineContent[len - 1] === '\r') len--;
  return charToByte(lineContent, len, byteStart);
}

function atxHeading(content: string): [number, number, number] | null {
  const m = content.match(/^(\s*)(#{1,6})([ \t]+)(?=\S)/);
  if (!m) return null;
  return [m[1].length, m[2].length + m[3].length, m[2].length];
}

/**
 * Process a single line: add overlays (emphasis, link styling) and conceals
 * (hide markdown syntax markers).
 *
 * Cursor-dependent rendering (revealing the markup under the cursor,
 * un-truncating the table row the cursor is on) is expressed as *activation
 * scopes* on the markers themselves — for each cursor-revealable decoration
 * this pass emits the concealed variant tagged `unless-cursor-in` and, where
 * the cursor-row rendering differs, a counterpart tagged `if-cursor-in`.
 * The renderer picks per frame from the live cursor positions, so this pass
 * runs only when *content* changes — never on cursor movement. That is what
 * keeps arrow-key navigation free of marker churn (and of the whole-buffer
 * cache invalidation it used to cause).
 */
function processLineConceals(
  bufferId: number,
  lineContent: string,
  byteStart: number,
  byteEnd: number,
  measure: number,
  region: RegionLine | undefined,
  lineNumber?: number,
): void {
  // Clear existing conceals and overlays for this line first.
  // This ensures clear+add commands are sent together from the plugin thread
  // and processed atomically in the same process_commands() batch, avoiding
  // the one-frame glitch where conceals are cleared but not yet rebuilt.
  editor.clearConcealsInRange(bufferId, byteStart, byteEnd);
  // Only clear our own emphasis overlays — clearing ALL overlays in the range
  // would also wipe editor-owned overlays like LSP diagnostics (issue #2146).
  editor.clearOverlaysInRangeForNamespace(bufferId, "md-emphasis", byteStart, byteEnd);

  // Activation scopes:
  //   - lineScopeInclEnd: "cursor anywhere on the line", including the
  //     boundary position at byteEnd — a cursor sitting at the start of the
  //     next line still reveals this line (matches the old `c <= byteEnd`).
  //   - lineScopeStrictEnd: excludes byteEnd; used for table rows so a
  //     cursor at the start of the next row doesn't expose this one.
  const lineScopeInclEnd = byteEnd + 1;
  const lineScopeStrictEnd = byteEnd;

  const trimmed = lineContent.trim();

  // --- Fenced code blocks ---
  // The delimiters become the block's frame; the body is left entirely alone,
  // so the embedded-language highlighting shows through untouched and none of
  // the inline markdown rules below fire inside code (a `*` in a glob, a `-`
  // starting a shell line, a `#` comment are not emphasis, a bullet, or a
  // heading). See the RegionLine notes above for why this is the editor's
  // answer and not a local guess.
  if (region === "body") return;
  if (region === "open" || region === "close") {
    const frameEnd = lineContentEndByte(lineContent, byteStart);
    const frame = region === "open"
      ? buildCodeFrameLine(measure, fenceInfoString(lineContent), "┌", "┐")
      : buildCodeFrameLine(measure, "", "└", "┘");
    editor.addConceal(
      bufferId, "md-syntax", byteStart, frameEnd, frame,
      "unless-cursor-in", byteStart, lineScopeInclEnd,
    );
    editor.addOverlay(bufferId, "md-emphasis", byteStart, frameEnd, codeFrameStyle);
    return;
  }
  // A fence-looking line the editor could not classify: leave it literal
  // rather than guess a corner. Same conservative choice the table frame
  // makes for a row whose neighbour is off-screen.
  if (looksLikeFence(lineContent)) return;

  // --- Thematic breaks: `---` / `***` / `___` → one full-measure rule ---
  // Rendered as a single conceal replacement rather than per-character
  // substitution so the rule spans the page measure regardless of how many
  // dashes the source used. Two columns short of the measure for the same
  // reason the wrap budget is (see `wrapBudget`): the renderer reserves an
  // end-of-line column, and a rule that reaches it would wrap onto a second
  // visual row. Matches the `hr` pattern in `parseMarkdownBlocks`.
  if (/^(-{3,}|\*{3,}|_{3,})$/.test(trimmed)) {
    const hrWidth = Math.max(1, measure - 2);
    const hrEnd = lineContentEndByte(lineContent, byteStart);
    editor.addConceal(
      bufferId, "md-syntax", byteStart, hrEnd, "─".repeat(hrWidth),
      "unless-cursor-in", byteStart, lineScopeInclEnd,
    );
    editor.addOverlay(
      bufferId, "md-emphasis", byteStart, hrEnd, { fg: "ui.split_separator_fg" },
    );
    return;
  }

  // --- Table row handling ---
  // Table conceals apply even when the cursor is on the line (pipes stay
  // box-drawing). Tables are structural: pipes → box-drawing, cells padded
  // for alignment. Only *cell padding* and *truncation/wrapping* differ on
  // the cursor row, and those are emitted as unless/if-cursor-in pairs.
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

    // Pipe char positions, shared by truncation and both padding variants.
    const pipePositions: number[] = [];
    for (let i = 0; i < lineContent.length; i++) {
      if (lineContent[i] === '|') pipePositions.push(i);
    }

    // Check if any data cell needs multi-line wrapping (concealed widths —
    // the wrapped rendering is the cursor-OFF variant).
    let handledByWrapping = false;
    if (colWidths && !isSeparator) {
      const numCols = Math.min(cells.length, colWidths.length);
      const cellWrapped: string[][] = [];
      let maxVisualLines = 1;
      for (let ci = 0; ci < numCols; ci++) {
        const cellText = concealedText(cells[ci]).trim();
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
          editor.addConceal(
            bufferId, "md-syntax", sByteS, sByteE, visualLines[vl] || '',
            "unless-cursor-in", byteStart, lineScopeStrictEnd,
          );
        }
        handledByWrapping = true;

        // Cursor-ON variant: the row the cursor is on renders as a plain
        // single-line row — raw text, raw-width padding, no wrapping — so
        // its full content stays editable.
        let pipeIdx = 0;
        for (let i = 0; i < lineContent.length; i++) {
          if (lineContent[i] !== '|') continue;
          const pipeByte = charToByte(lineContent, i, byteStart);
          const pipeByteEnd = charToByte(lineContent, i + 1, byteStart);
          let padOn = "";
          const cellIdx = pipeIdx - 1;
          if (pipeIdx > 0 && cellIdx < cells.length && cellIdx < colWidths.length) {
            const rawW = displayWidth(cells[cellIdx]);
            if (rawW <= colWidths[cellIdx]) {
              padOn = " ".repeat(colWidths[cellIdx] - rawW);
            }
          }
          editor.addConceal(
            bufferId, "md-syntax", pipeByte, pipeByteEnd, padOn + "│",
            "if-cursor-in", byteStart, lineScopeStrictEnd,
          );
          pipeIdx++;
        }
      }
    }

    if (!handledByWrapping) {
      // Precompute which cells the cursor-off state truncates (concealed
      // width exceeds the allocation), so the '-' substitution pass —
      // which visits a cell's chars BEFORE its closing pipe — can tell.
      const truncatedCellCharRanges: Array<{start: number; end: number}> = [];
      if (colWidths) {
        for (let ci = 0; ci < Math.min(cells.length, colWidths.length); ci++) {
          if (displayWidth(concealedText(cells[ci])) > colWidths[ci]) {
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

          // Compute padding for the cell that just ended, in both cursor
          // states.
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
          let padOff = ""; // cursor elsewhere: concealed text width
          let padOn = ""; // cursor on this row: raw text width
          const cellIdx = pipeIdx - 1;
          if (colWidths && pipeIdx > 0 && cellIdx < cells.length && cellIdx < colWidths.length) {
            const offText = concealedText(cells[cellIdx]);
            const offWidth = displayWidth(offText);
            const rawWidth = displayWidth(cells[cellIdx]);
            const allocatedWidth = colWidths[cellIdx];

            if (offWidth > allocatedWidth) {
              // Truncate (cursor-off only): conceal entire cell content and
              // replace with truncated text. Separator rows use box-drawing ─
              // to match the non-truncated path (per-char conceals replace
              // source `-` with ─ and pad via pipe replacement).
              const prevPipeCharPos = pipePositions[pipeIdx - 1];
              const cellByteStart = charToByte(lineContent, prevPipeCharPos + 1, byteStart);
              const cellByteEnd = pipeByte;
              const truncated = isSeparator
                ? '─'.repeat(allocatedWidth)
                : offText.slice(0, allocatedWidth - 1) + '-';
              editor.addConceal(
                bufferId, "md-syntax", cellByteStart, cellByteEnd, truncated,
                "unless-cursor-in", byteStart, lineScopeStrictEnd,
              );
              truncatedByteRanges.push({start: cellByteStart, end: cellByteEnd});
              // padOff stays "" — the truncate conceal fills the cell.
            } else {
              const padCount = allocatedWidth - offWidth;
              if (padCount > 0) {
                padOff = isSeparator ? "─".repeat(padCount) : " ".repeat(padCount);
              }
            }
            if (rawWidth <= allocatedWidth) {
              const padCount = allocatedWidth - rawWidth;
              if (padCount > 0) {
                padOn = isSeparator ? "─".repeat(padCount) : " ".repeat(padCount);
              }
            }
            // rawWidth > allocatedWidth: the cursor row keeps the too-wide
            // cell raw (no padding, no truncation) so it stays editable.
          }

          let glyph = "│";
          if (isSeparator) {
            const pipeIndex = lineContent.substring(0, i + 1).split('|').length - 1;
            const totalPipes = lineContent.split('|').length - 1;
            glyph = '┼';
            if (pipeIndex === 1) glyph = '├';
            else if (pipeIndex === totalPipes) glyph = '┤';
          }
          if (padOff === padOn) {
            // Same rendering in both cursor states — one always-active conceal.
            editor.addConceal(bufferId, "md-syntax", pipeByte, pipeByteEnd, padOff + glyph);
          } else {
            editor.addConceal(
              bufferId, "md-syntax", pipeByte, pipeByteEnd, padOff + glyph,
              "unless-cursor-in", byteStart, lineScopeStrictEnd,
            );
            editor.addConceal(
              bufferId, "md-syntax", pipeByte, pipeByteEnd, padOn + glyph,
              "if-cursor-in", byteStart, lineScopeStrictEnd,
            );
          }
          pipeIdx++;
        } else if (isSeparator && lineContent[i] === '-') {
          // Per-character conceals inside a truncated cell are suppressed in
          // the cursor-off state — the cell-wide truncate conceal already
          // renders the replacement; if both fired, the cell would come out
          // one character wider than allocated. On the cursor row the cell is
          // raw (no truncate conceal), so the ─ substitution applies there.
          const inTruncated = truncatedCellCharRanges.some(r => i >= r.start && i < r.end);
          const db = charToByte(lineContent, i, byteStart);
          const de = charToByte(lineContent, i + 1, byteStart);
          if (inTruncated) {
            editor.addConceal(
              bufferId, "md-syntax", db, de, "─",
              "if-cursor-in", byteStart, lineScopeStrictEnd,
            );
          } else {
            editor.addConceal(bufferId, "md-syntax", db, de, "─");
          }
        }
      }
    }
    // For wrapped rows, entire line is concealed — skip emphasis processing.
    // For non-wrapped rows, fall through to emphasis / link / entity processing.
    if (handledByWrapping) return;
  }

  // --- Image links: ![alt](url) → "Image: alt — url" ---
  // The concealed banner deactivates while the cursor is on the line, which
  // leaves the raw `![alt](url)` markup visible for editing.
  const imageRe = /^!\[([^\]]*)\]\(([^)]+)\)$/;
  const imageMatch = trimmed.match(imageRe);
  if (imageMatch) {
    const alt = imageMatch[1];
    const url = imageMatch[2];
    editor.addConceal(
      bufferId, "md-syntax", byteStart, byteEnd, `Image: ${alt} — ${url}`,
      "unless-cursor-in", byteStart, lineScopeInclEnd,
    );
    return;
  }

  // --- Block quotes: `>` markers become a left bar ---
  // Each `>` is concealed into a bar glyph in place, one character for one
  // character, so the quote's text keeps its source column and the bars of
  // consecutive quote lines stack into a continuous rule down the block. A
  // nested quote (`> >`) therefore renders as two bars, which is the nesting
  // depth made visible for free.
  //
  // Per-character rather than one conceal over the whole marker run because
  // width must be preserved exactly: the run can contain spaces between the
  // markers, and collapsing them would shift the quoted text left of where the
  // soft-break hanging indent (leadingIndent + 2) puts its continuation rows.
  //
  // Falls through to inline-span processing, so emphasis and links inside a
  // quote still render.
  const quoteRun = lineContent.match(/^(\s*)(>[>\s]*)/);
  if (quoteRun) {
    const runStart = quoteRun[1].length;
    const runEnd = runStart + quoteRun[2].length;
    for (let i = runStart; i < runEnd; i++) {
      if (lineContent[i] !== '>') continue;
      const markerByte = charToByte(lineContent, i, byteStart);
      const markerByteEnd = charToByte(lineContent, i + 1, byteStart);
      editor.addConceal(
        bufferId, "md-syntax", markerByte, markerByteEnd, QUOTE_BAR,
        "unless-cursor-in", byteStart, lineScopeInclEnd,
      );
      editor.addOverlay(
        bufferId, "md-emphasis", markerByte, markerByteEnd, quoteBarStyle,
      );
    }
    // Quoted text reads as an aside, not as body copy.
    const quotedStart = charToByte(lineContent, runEnd, byteStart);
    const quotedEnd = lineContentEndByte(lineContent, byteStart);
    if (quotedEnd > quotedStart) {
      editor.addOverlay(
        bufferId, "md-emphasis", quotedStart, quotedEnd, quoteTextStyle,
      );
    }
  }

  // --- ATX headings: conceal the `#` run, style the text by level ---
  // Falls through to inline-span processing below, so emphasis and links
  // inside a heading still render. The conceal is cursor-revealable (the `#`
  // markers come back while editing the line); the overlay is not, so the
  // heading keeps its colour in both states and doesn't flicker weight as the
  // cursor passes through it.
  const heading = atxHeading(lineContent);
  if (heading) {
    const [indent, markerLen, level] = heading;
    const markerByteStart = charToByte(lineContent, indent, byteStart);
    const markerByteEnd = charToByte(lineContent, indent + markerLen, byteStart);
    editor.addConceal(
      bufferId, "md-syntax", markerByteStart, markerByteEnd, null,
      "unless-cursor-in", byteStart, lineScopeInclEnd,
    );
    editor.addOverlay(
      bufferId, "md-emphasis", markerByteEnd, byteEnd, headingTextStyle(level),
    );
  }

  // --- List items: bullet glyph + widened nesting indent ---
  // Falls through to inline-span processing so emphasis inside an item works.
  const listInfo = listItemInfo(lineContent, measure);
  if (listInfo) {
    if (listInfo.renderedIndent > listInfo.sourceIndent) {
      // Widen the indent by concealing the source whitespace and replacing it
      // with a longer run. processLineSoftBreaks charges the same widened
      // indent against the wrap budget, so the two passes agree.
      const indentEndByte = charToByte(lineContent, listInfo.sourceIndent, byteStart);
      editor.addConceal(
        bufferId, "md-syntax", byteStart, indentEndByte,
        " ".repeat(listInfo.renderedIndent),
        "unless-cursor-in", byteStart, lineScopeInclEnd,
      );
    }
    if (listInfo.bullet !== null) {
      const bulletByte = charToByte(lineContent, listInfo.sourceIndent, byteStart);
      const bulletByteEnd = charToByte(lineContent, listInfo.sourceIndent + 1, byteStart);
      editor.addConceal(
        bufferId, "md-syntax", bulletByte, bulletByteEnd, LIST_BULLET,
        "unless-cursor-in", byteStart, lineScopeInclEnd,
      );
      editor.addOverlay(
        bufferId, "md-emphasis", bulletByte, bulletByteEnd, listBulletStyle,
      );
    }
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

    // Conceals deactivate while a cursor is inside their reveal scope.
    // For table rows the scope is the whole line ("auto-expose entire row"):
    // this keeps the row layout consistent with the raw-text-based column
    // widths, preventing overflow/wrapping. For other lines the scope is the
    // span itself, so only the markup under the cursor is revealed.
    const scopeStart = isTableRow ? byteStart : byteMS;
    const scopeEnd = isTableRow ? lineScopeStrictEnd : byteME + 1;
    for (const range of span.concealRanges) {
      const rStart = charToByte(lineContent, range.start, byteStart);
      const rEnd = charToByte(lineContent, range.end, byteStart);
      editor.addConceal(
        bufferId, "md-syntax", rStart, rEnd, range.replacement,
        "unless-cursor-in", scopeStart, scopeEnd,
      );
    }
  }
}


// Track viewport width per buffer for resize detection
let lastViewportWidth = 0;

// =============================================================================
// Hook handlers
// =============================================================================

/**
 * Compute soft break points for a single line, using the same block parsing
 * and word-wrap logic as the old token transform, but emitting
 * marker-based soft breaks.
 */
function processLineSoftBreaks(
  bufferId: number,
  lineContent: string,
  byteStart: number,
  byteEnd: number,
  region: RegionLine | undefined,
  lineNumber?: number,
): void {
  // Clear existing soft breaks for this line range
  editor.clearSoftBreaksInRange(bufferId, byteStart, byteEnd);

  // Code is never re-wrapped: its line breaks are significant, and a body line
  // is not a markdown block at all. `parseMarkdownBlocks` cannot tell — it sees
  // one line with no fence context — so it would read an indented code line as
  // a paragraph and wrap it at the measure.
  if (isCodeRegion(region)) return;

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

  // Image blocks: add a trailing blank line for visual separation when
  // concealed. Deactivates while the cursor is on the line (same scope as
  // the image conceal), where the raw markup shows instead.
  if (block.type === 'image') {
    editor.addSoftBreak(
      bufferId, "md-wrap", byteEnd - 1, 0,
      "unless-cursor-in", byteStart, byteEnd + 1,
    );
  }

  // Table row wrapping: add soft breaks for multi-line cells. Computed from
  // concealed cell widths — the cursor-OFF rendering. The breaks deactivate
  // while the cursor is on the row (strict scope, matching the segment
  // conceals), where the row renders as a plain single line.
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
        for (let ci = 0; ci < numCols; ci++) {
          const cellText = concealedText(tableCells[ci]).trim();
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
            editor.addSoftBreak(
              bufferId, "md-wrap", breakBytePos, 0,
              "unless-cursor-in", byteStart, byteEnd,
            );
          }
        }
      }
    }
  }

  if (noWrap) return;

  // A nested list item renders at a widened indent (see listItemInfo), so both
  // the wrap budget and the continuation indent have to use the *rendered*
  // width, not the source's. Otherwise a wrapped nested item's continuation
  // rows sit left of its text and its last row can overrun the measure.
  const listInfo = listItemInfo(lineContent, width);
  const hangingIndent = listInfo
    ? listInfo.renderedIndent + listInfo.markerLen
    : block.hangingIndent;

  // Compute per-character visual width so concealed markup (emphasis
  // markers, link syntax, entities) doesn't count towards line width.
  const spans = findInlineSpans(lineContent);
  const charW = new Array<number>(lineContent.length).fill(1);

  // Charge the widened indent to its first character and zero the rest, the
  // same shape the entity replacements below use.
  if (listInfo && listInfo.sourceIndent > 0) {
    for (let c = 0; c < listInfo.sourceIndent && c < charW.length; c++) charW[c] = 0;
    charW[0] = listInfo.renderedIndent;
  }
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
 * are uniform within a group. Lines inside a fenced code block are not table
 * rows however many pipes they contain, so they both fail to join a group and
 * break one — matching how a code block separates two tables in the source. */
function groupTableRows(lines: LineInfoLike[]): LineInfoLike[][] {
  const groups: LineInfoLike[][] = [];
  let cur: LineInfoLike[] = [];
  let lastLn = -2;
  for (const line of lines) {
    const isRow = !isCodeRegion(line.region) && isTableRowContent(line.content);
    if (isRow && line.line_number === lastLn + 1) {
      cur.push(line);
    } else if (isRow) {
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
    // Clamp the low end at 0: `queryMarkers`'s binding takes a u32, so a table
    // at byte 0 (`gStart - 1 === -1`) would throw an underflow out of the whole
    // handler and leave the file rendering raw `|`.
    const near = (editor.queryMarkers(bufferId, Math.max(0, gStart - 1), gEnd + 1) as Array<{
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


// Soft wrapping is handled by
// marker-based soft breaks (computed in lines_changed), and layout hints
// are set directly via setLayoutHints. This eliminates the one-frame flicker
// caused by an async round-trip.

// Handle buffer close events - clean up compose mode tracking


// viewport_changed: recalculate table column widths on terminal resize


// Re-enable compose mode for buffers restored from a saved session.
// The Rust side restores ViewMode::Compose and compose_width, but the plugin
// needs to re-apply line numbers, line wrap, and layout hints when activated.


// Register hooks
editor.on("lines_changed", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  // Cursor reveal/conceal decisions are NOT made here: every emitted
  // decoration carries an activation scope and the renderer evaluates it
  // per frame against each split's own cursors. This handler runs only
  // when content changes — cursor movement re-renders with the existing
  // markers, so navigating never rebuilds anything.

  // Column widths for every table row in this batch (uniform per table, via the
  // grow-only memo). If a wider row scrolled into view and grew a table's
  // columns, force a refresh so already-visible rows re-render at the new width.
  const tableWidthsGrew = computeRowWidths(data.buffer_id, data.lines);

  // Line-number → line, for local first/last-row classification of borders.
  const byLineNum = new Map<number, LineInfoLike>();
  for (const line of data.lines) byLineNum.set(line.line_number, line);

  // Buffer end byte, so a table whose last row is the final line of the buffer
  // (no following line in the batch, ever) still closes its frame. Without this
  // `isLast` can never be true at end-of-buffer and the bottom border is missing.
  const bufEnd = editor.getBufferLength(data.buffer_id);

  // The page measure, read once for the batch rather than per line — the
  // conceal pass needs it for the thematic-break rule and the list indent, and
  // it can't change within a single render.
  const batchViewport = editor.getViewport();
  const measure = effectiveComposeWidth(batchViewport ? batchViewport.width : 80);

  // Per line: clear+rebuild conceals, soft-breaks, and the table border frame —
  // all anchored to this one line. No whole-table rebuild, no stored row model;
  // borders for lines not in this batch keep riding their auto-shift markers.
  for (const line of data.lines) {
    // Clear this row's border range first (covers a row that stopped being a
    // table row, e.g. its pipes were deleted, and stale frames left a few bytes
    // off by the async lag).
    clearRowBorders(data.buffer_id, line.byte_start, line.byte_end);
    // Same range-scoped clear-then-rebuild as the borders, so a line that stops
    // being a list item loses its spacer.
    editor.clearVirtualLinesInRange(
      data.buffer_id, LIST_SPACING_NS,
      line.byte_start, Math.max(line.byte_end, line.byte_start + 1),
    );

    processLineConceals(data.buffer_id, line.content, line.byte_start, line.byte_end, measure, line.region, line.line_number);
    processLineSoftBreaks(data.buffer_id, line.content, line.byte_start, line.byte_end, line.region, line.line_number);

    // Blank row after each list item, so items read as discrete entries.
    //
    // Deliberately decided from this line ALONE, not from its neighbours. The
    // obvious rule — "a spacer above an item whose predecessor is also an
    // item" — needs two lines, and an edit-sized `lines_changed` batch
    // contains only the lines the edit touched. The spacer would then be
    // cleared (this line is in the batch) but not re-derivable (its neighbour
    // isn't), so editing inside a list silently dropped its spacing and never
    // got it back: unlike the table frame, there is no later batch that
    // restores it, because `lines_changed` is edge-triggered on ranges it
    // hasn't seen.
    //
    // Anchoring below every item is line-local, so clear-and-rebuild is always
    // a complete decision. The cost is one blank row after a list's final item
    // as well as between items, which reads as ordinary block separation.
    //
    // Inside a fence none of this applies: `- foo` in a shell block is not a
    // list item and `| a | b |` in a code sample is not a table row, so both
    // the spacer and the frame are skipped (the clears above already ran, so a
    // line that becomes code loses whichever it had).
    if (!isCodeRegion(line.region) && isListItemContent(line.content)) {
      editor.addVirtualLine(
        data.buffer_id, line.byte_start, "",
        listSpacingOptions, false, LIST_SPACING_NS, 0,
      );
    }

    if (!isCodeRegion(line.region) && isTableRowContent(line.content)) {
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
      // Last if the next line is present and not a table row, OR this row is the
      // final line of the buffer (no next line exists at all — distinct from a
      // next line merely off-screen, which we treat as mid-table).
      const isLast =
        (next !== undefined && !isTableRowContent(next.content)) ||
        (next === undefined && line.byte_end >= bufEnd);
      const isSep = isSepRowContent(line.content);
      const prevIsSep = prev !== undefined && isSepRowContent(prev.content);
      emitRowBorders(data.buffer_id, line.byte_start, widths, isFirst, isSep, prevIsSep, isLast);
    }
  }

  // Publish this batch's heading marks. Range-scoped to the batch's byte span
  // so headings outside it — already-visited parts of the document — keep
  // their marks; see HEADING_MARKER_NS. Emitted even when the batch has no
  // headings, so a line that stops being one loses its mark.
  if (data.lines.length > 0) {
    const batchStart = data.lines[0].byte_start;
    const batchEnd = data.lines[data.lines.length - 1].byte_end;
    const headingMarkers = [];
    for (const line of data.lines) {
      const level = headingLevel(line.content);
      if (level === 0) continue;
      headingMarkers.push({
        // Byte offsets, not line numbers: they are exact regardless of file
        // size, and the editor anchors them so later edits shift the marks.
        position: line.byte_start,
        color: headingMarkerColor(level),
        priority: 10 - level, // shallower headings win a shared track cell
      });
    }
    editor.setScrollbarMarkersInRange(
      data.buffer_id, HEADING_MARKER_NS,
      batchStart, Math.max(batchEnd, batchStart + 1),
      headingMarkers,
    );
  }

  // One whole-viewport re-fire when an edit left a delimiter-shaped line
  // behind: every line below it may have crossed into or out of a block, and
  // those lines are not in this batch (see `fenceEditArmed`). Disarmed first,
  // so the refresh's own batch — which contains the same line — cannot re-arm.
  const fenceEdit = fenceEditArmed.delete(data.buffer_id)
    && data.lines.some((l) => looksLikeFence(l.content));

  if (tableWidthsGrew || fenceEdit) {
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
  // Typed backticks can open or close a block; anything else can only stop an
  // already-delimiter-shaped line from being one, which the batch will see.
  if (touchesFenceChars(data.text)) editor.refreshLines(data.buffer_id);
  else fenceEditArmed.add(data.buffer_id);
});
editor.on("after_delete", (data) => {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  resetEditedTableWidths(data.buffer_id, data.affected_start, data.affected_start);
  // Deleted backticks are checked here, not in the batch: the text that is gone
  // appears in no later `lines_changed` payload.
  if (touchesFenceChars(data.deleted_text)) editor.refreshLines(data.buffer_id);
  else fenceEditArmed.add(data.buffer_id);
});
// cursor_moved: no handler. Cursor-dependent reveal/conceal and table-row
// un/re-wrap are baked into the markers as activation scopes (emitted in the
// lines_changed pass above) and evaluated by the renderer per frame — cursor
// movement changes what's *active* without touching any marker, so it never
// re-fires lines_changed, never bumps the conceal/soft-break versions, and
// never invalidates the line-wrap cache or visual-row index.
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
  const active = activeComposeWidth();
  const currentValue = active === null ? "None" : String(active);
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
