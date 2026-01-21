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
  composeWidth: number;
  maxWidth: number;
  hideLineNumbers: boolean;
}

const config: MarkdownConfig = {
  composeWidth: 80,
  maxWidth: 100,
  hideLineNumbers: true,
};

// Track buffers in compose mode (explicit toggle)
const composeBuffers = new Set<number>();

// =============================================================================
// Block-based parser for hanging indent support
// =============================================================================

interface ParsedBlock {
  type: 'paragraph' | 'list-item' | 'ordered-list' | 'checkbox' | 'blockquote' |
        'heading' | 'code-fence' | 'code-content' | 'hr' | 'empty' | 'image';
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

// Process a buffer in compose mode - just enables compose mode
// The actual transform happens via view_transform_request hook
function processBuffer(bufferId: number, _splitId?: number): void {
  if (!composeBuffers.has(bufferId)) return;

  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  editor.debug(`processBuffer: enabling compose mode for ${info.path}, buffer_id=${bufferId}`);

  // Trigger a refresh to get the view_transform_request hook called
  editor.refreshLines(bufferId);
}

// Enable full compose mode for a buffer (explicit toggle)
function enableMarkdownCompose(bufferId: number): void {
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  if (!composeBuffers.has(bufferId)) {
    composeBuffers.add(bufferId);

    // Hide line numbers in compose mode
    editor.setLineNumbers(bufferId, false);

    processBuffer(bufferId);
    editor.debug(`Markdown compose enabled for buffer ${bufferId}`);
  }
}

// Disable compose mode for a buffer
function disableMarkdownCompose(bufferId: number): void {
  if (composeBuffers.has(bufferId)) {
    composeBuffers.delete(bufferId);

    // Re-enable line numbers
    editor.setLineNumbers(bufferId, true);

    // Clear view transform to return to normal rendering
    editor.clearViewTransform(bufferId);

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

  if (composeBuffers.has(bufferId)) {
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
        if (column + 1 + nextWordLen > width && nextWordLen > 0) {
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
      if (column > hangingIndent && column + text.length > width) {
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

// Handle view transform request - receives tokens from core for transformation
// Only applies transforms when in compose mode
globalThis.onMarkdownViewTransform = function(data: {
  buffer_id: number;
  split_id: number;
  viewport_start: number;
  viewport_end: number;
  tokens: ViewTokenWire[];
}): void {
  // Only transform when in compose mode
  if (!composeBuffers.has(data.buffer_id)) return;

  const info = editor.getBufferInfo(data.buffer_id);
  if (!info || !isMarkdownFile(info.path)) return;

  editor.debug(`onMarkdownViewTransform: buffer=${data.buffer_id}, split=${data.split_id}, tokens=${data.tokens.length}`);

  // Transform the incoming tokens with markdown-aware wrapping
  const transformedTokens = transformMarkdownTokens(
    data.tokens,
    config.composeWidth,
    data.viewport_start
  );

  // Submit the transformed tokens - keep composeWidth for margins/centering
  const layoutHints: LayoutHints = {
    composeWidth: config.composeWidth,
    columnGuides: null,
  };

  editor.submitViewTransform(
    data.buffer_id,
    data.split_id,
    data.viewport_start,
    data.viewport_end,
    transformedTokens,
    layoutHints
  );
};

// Handle buffer close events - clean up compose mode tracking
globalThis.onMarkdownBufferClosed = function(data: { buffer_id: number }): void {
  composeBuffers.delete(data.buffer_id);
};

// Register hooks
editor.on("view_transform_request", "onMarkdownViewTransform");
editor.on("buffer_closed", "onMarkdownBufferClosed");
editor.on("prompt_confirmed", "onMarkdownComposeWidthConfirmed");

// Set compose width command - starts interactive prompt
globalThis.markdownSetComposeWidth = function(): void {
  editor.startPrompt(editor.t("prompt.compose_width"), "markdown-compose-width");
  editor.setPromptSuggestions([
    { text: "60", description: editor.t("suggestion.narrow") },
    { text: "72", description: editor.t("suggestion.classic") },
    { text: "80", description: editor.t("suggestion.standard") },
    { text: "100", description: editor.t("suggestion.wide") },
  ]);
};

// Handle compose width prompt confirmation
globalThis.onMarkdownComposeWidthConfirmed = function(args: {
  prompt_type: string;
  text: string;
}): void {
  if (args.prompt_type !== "markdown-compose-width") return;

  const width = parseInt(args.text, 10);
  if (!isNaN(width) && width > 20 && width < 300) {
    config.composeWidth = width;
    editor.setStatus(editor.t("status.width_set", { width: String(width) }));

    // Re-process active buffer if in compose mode
    const bufferId = editor.getActiveBufferId();
    if (composeBuffers.has(bufferId)) {
      editor.refreshLines(bufferId);  // Trigger re-transform
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
  "normal"
);

editor.registerCommand(
  "%cmd.set_compose_width",
  "%cmd.set_compose_width_desc",
  "markdownSetComposeWidth",
  "normal"
);

// Initialization
editor.debug("Markdown Compose plugin loaded - use 'Markdown: Toggle Compose' command");
