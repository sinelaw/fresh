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

// Table column widths stored per-buffer-per-split via setViewState/getViewState.
// Persisted across sessions and independent per split.
interface TableWidthInfo {
  maxW: number[];
  allocated: number[];
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
  editor.setLayoutHints(bufferId, null, { composeWidth: config.composeWidth });

  // Trigger a refresh so lines_changed hooks fire for visible content
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

    editor.refreshLines(bufferId);
    editor.debug(`Markdown compose disabled for buffer ${bufferId}`);
  }
}

// Toggle markdown compose mode for current buffer
globalThis.markdownToggleCompose = function(): void {
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
};

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

const MIN_COL_W = 3;

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
  editor.clearOverlaysInRange(bufferId, byteStart, byteEnd);

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

    // Look up stored column widths for alignment padding
    const bufWidths = lineNumber !== undefined ? getTableWidths(bufferId) : undefined;
    const widthInfo = bufWidths && lineNumber !== undefined ? bufWidths.get(lineNumber) : undefined;
    const colWidths = widthInfo ? widthInfo.allocated : undefined;

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
            vline += ' ' + text + ' '.repeat(Math.max(0, wrapW - text.length)) + ' │';
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

      // Track which pipe index we're on (0 = leading pipe)
      let pipeIdx = 0;
      for (let i = 0; i < lineContent.length; i++) {
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
            const cellWidth = cellText.length;
            const allocatedWidth = colWidths[cellIdx];

            if (cellWidth > allocatedWidth) {
              // Truncate: conceal entire cell content and replace with truncated text
              const prevPipeCharPos = pipePositions[pipeIdx - 1];
              const cellByteStart = charToByte(lineContent, prevPipeCharPos + 1, byteStart);
              const cellByteEnd = pipeByte;
              const truncated = cellText.slice(0, allocatedWidth - 1) + '-';
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
  const width = config.composeWidth ?? viewport.width;

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
  if (block.type === 'table-row' && lineNumber !== undefined) {
    const trimmedLine = lineContent.trim();
    const isSep = /^\|[-:\s|]+\|$/.test(trimmedLine);
    if (!isSep) {
      const bufWidths = getTableWidths(bufferId);
      const widthInfo = bufWidths ? bufWidths.get(lineNumber) : undefined;
      const colWidths = widthInfo ? widthInfo.allocated : undefined;
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
  // We need to find Space positions where wrapping should occur
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

      // Check if space + next word would exceed width
      if (column + 1 + nextWordLen > width && nextWordLen > 0) {
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
    const isTableRow = trimmed.startsWith('|') || trimmed.endsWith('|');
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
      // Strip outer pipes and split on inner pipes
      let inner = trimmed;
      if (inner.startsWith('|')) inner = inner.slice(1);
      if (inner.endsWith('|')) inner = inner.slice(0, -1);
      const cells = inner.split('|');
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
            maxW = Math.max(maxW, row[col].length);
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

    // Compute allocated widths constrained to viewport
    const viewport = editor.getViewport();
    const composeW = config.composeWidth ?? (viewport ? viewport.width : 80);
    const numCols = merged.length;
    const available = composeW - (numCols + 1); // subtract pipe/box-drawing characters
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

    // Store merged widths for all lines in the group AND propagate
    // back to adjacent cached lines so they pick up wider columns
    // without needing to be re-delivered by lines_changed.
    const info: TableWidthInfo = { maxW: merged, allocated };
    for (const line of group) {
      widthMap.set(line.line_number, info);
    }
    for (let ln = firstLine - 1; widthMap.has(ln); ln--) {
      widthMap.set(ln, info);
    }
    for (let ln = lastLine + 1; widthMap.has(ln); ln++) {
      widthMap.set(ln, info);
    }
  }

  setTableWidths(bufferId, widthMap);
  return needsRefresh;
}

// lines_changed: called for newly visible or invalidated lines
globalThis.onMarkdownLinesChanged = function(data: {
  buffer_id: number;
  lines: Array<{
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
  }>;
}): void {
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

  for (const line of data.lines) {
    processLineConceals(data.buffer_id, line.content, line.byte_start, line.byte_end, cursors, line.line_number);
    processLineSoftBreaks(data.buffer_id, line.content, line.byte_start, line.byte_end, cursors, line.line_number);
  }

  if (tableWidthsGrew) {
    editor.refreshLines(data.buffer_id);
  }
};

// after_insert: no-op for conceals/overlays.
// The edit automatically invalidates seen_byte_ranges for affected lines,
// causing lines_changed to fire on the next render. processLineConceals
// handles clearing and rebuilding atomically.
// Marker-based positions auto-adjust with buffer edits, so existing conceals
// remain visually correct until lines_changed rebuilds them.
globalThis.onMarkdownAfterInsert = function(data: {
  buffer_id: number;
  position: number;
  text: string;
  affected_start: number;
  affected_end: number;
}): void {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  editor.debug(`[mc] after_insert: pos=${data.position} text="${data.text.replace(/\n/g,'\\n')}" affected=${data.affected_start}..${data.affected_end}`);
};

// after_delete: no-op for conceals/overlays (same reasoning as after_insert).
globalThis.onMarkdownAfterDelete = function(data: {
  buffer_id: number;
  start: number;
  end: number;
  deleted_text: string;
  affected_start: number;
  deleted_len: number;
}): void {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  editor.debug(`[mc] after_delete: start=${data.start} end=${data.end} deleted="${data.deleted_text.replace(/\n/g,'\\n')}" affected_start=${data.affected_start} deleted_len=${data.deleted_len}`);
};

// cursor_moved: update cursor-aware reveal/conceal for old and new cursor lines
globalThis.onMarkdownCursorMoved = function(data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
  line: number;
}): void {
  if (!isComposingInAnySplit(data.buffer_id)) return;

  const prevLine = editor.getViewState(data.buffer_id, "last-cursor-line") as number | undefined;
  editor.setViewState(data.buffer_id, "last-cursor-line", data.line);

  editor.debug(`[mc] cursor_moved: old_pos=${data.old_position} new_pos=${data.new_position} line=${data.line} prevLine=${prevLine}`);

  // Always refresh: even intra-line movements need conceal updates because
  // auto-expose is span-level (cursor entering/leaving an emphasis or link
  // span within the same line must toggle its syntax markers).
  editor.refreshLines(data.buffer_id);
};

// view_transform_request is no longer needed — soft wrapping is handled by
// marker-based soft breaks (computed in lines_changed), and layout hints
// are set directly via setLayoutHints. This eliminates the one-frame flicker
// caused by the async view_transform round-trip.

// Handle buffer close events - clean up compose mode tracking
globalThis.onMarkdownBufferClosed = function(data: { buffer_id: number }): void {
  // View state is cleaned up automatically when the buffer is removed from keyed_states
};

// viewport_changed: recalculate table column widths on terminal resize
globalThis.onMarkdownViewportChanged = function(data: {
  split_id: number;
  buffer_id: number;
  top_byte: number;
  width: number;
  height: number;
}): void {
  if (!isComposingInAnySplit(data.buffer_id)) return;
  if (data.width === lastViewportWidth) return;
  lastViewportWidth = data.width;

  // Recompute allocated table column widths for new viewport width
  const bufWidths = getTableWidths(data.buffer_id);
  if (bufWidths) {
    const composeW = config.composeWidth ?? data.width;
    const seen = new Set<string>(); // Track by JSON key to deduplicate shared TableWidthInfo
    for (const [lineNum, info] of bufWidths) {
      const key = info.maxW.join(",");
      if (seen.has(key)) continue;
      seen.add(key);
      const numCols = info.maxW.length;
      const available = composeW - (numCols + 1);
      info.allocated = distributeColumnWidths(info.maxW, available);
    }
    setTableWidths(data.buffer_id, bufWidths);
  }
  editor.refreshLines(data.buffer_id);
};

// Re-enable compose mode for buffers restored from a saved session.
// The Rust side restores ViewMode::Compose and compose_width, but the plugin
// needs to re-apply line numbers, line wrap, and layout hints when activated.
globalThis.onMarkdownBufferActivated = function(data: { buffer_id: number }): void {
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
  }
};

// Register hooks
editor.on("lines_changed", "onMarkdownLinesChanged");
editor.on("after_insert", "onMarkdownAfterInsert");
editor.on("after_delete", "onMarkdownAfterDelete");
editor.on("cursor_moved", "onMarkdownCursorMoved");
// view_transform_request hook no longer needed — wrapping is handled by soft breaks
editor.on("buffer_closed", "onMarkdownBufferClosed");
editor.on("viewport_changed", "onMarkdownViewportChanged");
editor.on("prompt_confirmed", "onMarkdownComposeWidthConfirmed");
editor.on("buffer_activated", "onMarkdownBufferActivated");

// Set compose width command - starts interactive prompt
globalThis.markdownSetComposeWidth = function(): void {
  const currentValue = config.composeWidth === null ? "None" : String(config.composeWidth);
  editor.startPromptWithInitial(editor.t("prompt.compose_width"), "markdown-compose-width", currentValue);
  editor.setPromptInputSync(true);
  editor.setPromptSuggestions([
    { text: "None", description: editor.t("suggestion.none") },
    { text: "120", description: editor.t("suggestion.default") },
  ]);
};

// Handle compose width prompt confirmation
globalThis.onMarkdownComposeWidthConfirmed = function(args: {
  prompt_type: string;
  input: string;
}): void {
  if (args.prompt_type !== "markdown-compose-width") return;

  const input = args.input.trim();
  if (input.toLowerCase() === "none") {
    config.composeWidth = null;
    editor.setStatus(editor.t("status.width_none"));

    const bufferId = editor.getActiveBufferId();
    if (isComposing(bufferId)) {
      editor.setLayoutHints(bufferId, null, { composeWidth: null });
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
      editor.setLayoutHints(bufferId, null, { composeWidth: config.composeWidth });
      editor.refreshLines(bufferId);  // Trigger soft break recomputation
    }
  } else {
    editor.setStatus(editor.t("status.invalid_width"));
  }
};

// Register commands
editor.registerCommand(
  "%cmd.toggle_compose",
  "%cmd.toggle_compose_desc",
  "markdownToggleCompose",
  null
);

editor.registerCommand(
  "%cmd.set_compose_width",
  "%cmd.set_compose_width_desc",
  "markdownSetComposeWidth",
  null
);

// Initialization
editor.debug("Markdown Compose plugin loaded - use 'Markdown: Toggle Compose' command");
