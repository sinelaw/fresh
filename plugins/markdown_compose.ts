// Markdown Compose Mode Plugin
// Provides beautiful, semi-WYSIWYG rendering of Markdown documents
// Implements soft breaks, structure styling, and view transforms

interface MarkdownConfig {
  enabled: boolean;
  composeWidth: number;
  maxWidth: number;
  hideLineNumbers: boolean;
}

const config: MarkdownConfig = {
  enabled: false,
  composeWidth: 80,
  maxWidth: 100,
  hideLineNumbers: true,
};

// Track buffers in compose mode
const composeBuffers = new Set<number>();

// Markdown token types for parsing
enum TokenType {
  Header1,
  Header2,
  Header3,
  Header4,
  Header5,
  Header6,
  ListItem,
  OrderedListItem,
  Checkbox,
  CodeBlockFence,
  CodeBlockContent,
  BlockQuote,
  HorizontalRule,
  Paragraph,
  HardBreak,
  InlineCode,
  Bold,
  Italic,
  Strikethrough,
  Link,
  LinkText,
  LinkUrl,
  Text,
}

interface Token {
  type: TokenType;
  start: number;  // byte offset
  end: number;    // byte offset
  text: string;
  level?: number; // For headers, list indentation
  checked?: boolean; // For checkboxes
}

// Types match the Rust ViewTokenWire structure
interface ViewTokenWire {
  source_offset: number | null;
  kind: ViewTokenWireKind;
}

type ViewTokenWireKind =
  | { Text: string }
  | "Newline"
  | "Space"
  | "Break";

interface LayoutHints {
  compose_width?: number | null;
  column_guides?: number[] | null;
}

// Colors for styling (RGB tuples)
const COLORS = {
  header: [100, 149, 237] as [number, number, number], // Cornflower blue
  code: [152, 195, 121] as [number, number, number],   // Green
  codeBlock: [152, 195, 121] as [number, number, number],
  fence: [128, 128, 128] as [number, number, number],  // Gray
  link: [86, 156, 214] as [number, number, number],    // Light blue
  linkUrl: [128, 128, 128] as [number, number, number], // Gray
  bold: [229, 192, 123] as [number, number, number],   // Gold
  italic: [198, 120, 221] as [number, number, number], // Purple
  quote: [128, 128, 128] as [number, number, number],  // Gray
  checkbox: [152, 195, 121] as [number, number, number], // Green
  listBullet: [86, 156, 214] as [number, number, number], // Light blue
};

// Simple Markdown parser
class MarkdownParser {
  private text: string;
  private tokens: Token[] = [];

  constructor(text: string) {
    this.text = text;
  }

  parse(): Token[] {
    const lines = this.text.split('\n');
    let byteOffset = 0;
    let inCodeBlock = false;
    let codeFenceStart = -1;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const lineStart = byteOffset;
      const lineEnd = byteOffset + line.length;

      // Code block detection
      if (line.trim().startsWith('```')) {
        if (!inCodeBlock) {
          inCodeBlock = true;
          codeFenceStart = lineStart;
          this.tokens.push({
            type: TokenType.CodeBlockFence,
            start: lineStart,
            end: lineEnd,
            text: line,
          });
        } else {
          this.tokens.push({
            type: TokenType.CodeBlockFence,
            start: lineStart,
            end: lineEnd,
            text: line,
          });
          inCodeBlock = false;
        }
      } else if (inCodeBlock) {
        this.tokens.push({
          type: TokenType.CodeBlockContent,
          start: lineStart,
          end: lineEnd,
          text: line,
        });
      } else {
        // Parse line structure
        this.parseLine(line, lineStart, lineEnd);
      }

      byteOffset = lineEnd + 1; // +1 for newline
    }

    // Parse inline styles after structure
    this.parseInlineStyles();

    return this.tokens;
  }

  private parseLine(line: string, start: number, end: number): void {
    const trimmed = line.trim();

    // Headers
    const headerMatch = trimmed.match(/^(#{1,6})\s+(.+)$/);
    if (headerMatch) {
      const level = headerMatch[1].length;
      const type = [
        TokenType.Header1,
        TokenType.Header2,
        TokenType.Header3,
        TokenType.Header4,
        TokenType.Header5,
        TokenType.Header6,
      ][level - 1];
      this.tokens.push({
        type,
        start,
        end,
        text: line,
        level,
      });
      return;
    }

    // Horizontal rule
    if (trimmed.match(/^(-{3,}|\*{3,}|_{3,})$/)) {
      this.tokens.push({
        type: TokenType.HorizontalRule,
        start,
        end,
        text: line,
      });
      return;
    }

    // List items
    const bulletMatch = line.match(/^(\s*)([-*+])\s+(.*)$/);
    if (bulletMatch) {
      const indent = bulletMatch[1].length;
      const hasCheckbox = bulletMatch[3].match(/^\[([ x])\]\s+/);

      if (hasCheckbox) {
        this.tokens.push({
          type: TokenType.Checkbox,
          start,
          end,
          text: line,
          level: indent,
          checked: hasCheckbox[1] === 'x',
        });
      } else {
        this.tokens.push({
          type: TokenType.ListItem,
          start,
          end,
          text: line,
          level: indent,
        });
      }
      return;
    }

    // Ordered list
    const orderedMatch = line.match(/^(\s*)(\d+\.)\s+(.*)$/);
    if (orderedMatch) {
      const indent = orderedMatch[1].length;
      this.tokens.push({
        type: TokenType.OrderedListItem,
        start,
        end,
        text: line,
        level: indent,
      });
      return;
    }

    // Block quote
    if (trimmed.startsWith('>')) {
      this.tokens.push({
        type: TokenType.BlockQuote,
        start,
        end,
        text: line,
      });
      return;
    }

    // Hard breaks (two spaces + newline, or backslash + newline)
    if (line.endsWith('  ') || line.endsWith('\\')) {
      this.tokens.push({
        type: TokenType.HardBreak,
        start,
        end,
        text: line,
      });
      return;
    }

    // Default: paragraph
    if (trimmed.length > 0) {
      this.tokens.push({
        type: TokenType.Paragraph,
        start,
        end,
        text: line,
      });
    }
  }

  private parseInlineStyles(): void {
    // Parse inline markdown (bold, italic, code, links) within text
    // This is a simplified parser - a full implementation would use a proper MD parser

    for (const token of this.tokens) {
      if (token.type === TokenType.Paragraph ||
          token.type === TokenType.ListItem ||
          token.type === TokenType.OrderedListItem) {
        // Find inline code
        this.findInlineCode(token);
        // Find bold/italic
        this.findEmphasis(token);
        // Find links
        this.findLinks(token);
      }
    }
  }

  private findInlineCode(token: Token): void {
    const regex = /`([^`]+)`/g;
    let match;
    while ((match = regex.exec(token.text)) !== null) {
      this.tokens.push({
        type: TokenType.InlineCode,
        start: token.start + match.index,
        end: token.start + match.index + match[0].length,
        text: match[0],
      });
    }
  }

  private findEmphasis(token: Token): void {
    // Bold: **text** or __text__
    const boldRegex = /(\*\*|__)([^*_]+)\1/g;
    let match;
    while ((match = boldRegex.exec(token.text)) !== null) {
      this.tokens.push({
        type: TokenType.Bold,
        start: token.start + match.index,
        end: token.start + match.index + match[0].length,
        text: match[0],
      });
    }

    // Italic: *text* or _text_
    const italicRegex = /(\*|_)([^*_]+)\1/g;
    while ((match = italicRegex.exec(token.text)) !== null) {
      // Skip if it's part of bold
      const isBold = this.tokens.some(t =>
        t.type === TokenType.Bold &&
        t.start <= token.start + match.index &&
        t.end >= token.start + match.index + match[0].length
      );
      if (!isBold) {
        this.tokens.push({
          type: TokenType.Italic,
          start: token.start + match.index,
          end: token.start + match.index + match[0].length,
          text: match[0],
        });
      }
    }

    // Strikethrough: ~~text~~
    const strikeRegex = /~~([^~]+)~~/g;
    while ((match = strikeRegex.exec(token.text)) !== null) {
      this.tokens.push({
        type: TokenType.Strikethrough,
        start: token.start + match.index,
        end: token.start + match.index + match[0].length,
        text: match[0],
      });
    }
  }

  private findLinks(token: Token): void {
    // Links: [text](url)
    const linkRegex = /\[([^\]]+)\]\(([^)]+)\)/g;
    let match;
    while ((match = linkRegex.exec(token.text)) !== null) {
      const fullStart = token.start + match.index;
      const textStart = fullStart + 1; // After [
      const textEnd = textStart + match[1].length;
      const urlStart = textEnd + 2; // After ](
      const urlEnd = urlStart + match[2].length;

      this.tokens.push({
        type: TokenType.Link,
        start: fullStart,
        end: fullStart + match[0].length,
        text: match[0],
      });

      this.tokens.push({
        type: TokenType.LinkText,
        start: textStart,
        end: textEnd,
        text: match[1],
      });

      this.tokens.push({
        type: TokenType.LinkUrl,
        start: urlStart,
        end: urlEnd,
        text: match[2],
      });
    }
  }
}

// Apply styling overlays based on parsed tokens
function applyMarkdownStyling(bufferId: number, tokens: Token[]): void {
  // Clear existing markdown overlays
  editor.removeOverlaysByPrefix(bufferId, "md:");

  for (const token of tokens) {
    let color: [number, number, number] | null = null;
    let underline = false;
    let overlayId = `md:${token.type}:${token.start}`;

    switch (token.type) {
      case TokenType.Header1:
      case TokenType.Header2:
      case TokenType.Header3:
      case TokenType.Header4:
      case TokenType.Header5:
      case TokenType.Header6:
        color = COLORS.header;
        underline = true;
        break;

      case TokenType.InlineCode:
        color = COLORS.code;
        break;

      case TokenType.CodeBlockFence:
        color = COLORS.fence;
        break;

      case TokenType.CodeBlockContent:
        color = COLORS.codeBlock;
        break;

      case TokenType.BlockQuote:
        color = COLORS.quote;
        break;

      case TokenType.Bold:
        color = COLORS.bold;
        break;

      case TokenType.Italic:
        color = COLORS.italic;
        break;

      case TokenType.LinkText:
        color = COLORS.link;
        underline = true;
        break;

      case TokenType.LinkUrl:
        color = COLORS.linkUrl;
        break;

      case TokenType.ListItem:
      case TokenType.OrderedListItem:
        // Style just the bullet/number
        const bulletMatch = token.text.match(/^(\s*)([-*+]|\d+\.)/);
        if (bulletMatch) {
          const bulletEnd = token.start + bulletMatch[0].length;
          editor.addOverlay(
            bufferId,
            `md:bullet:${token.start}`,
            token.start,
            bulletEnd,
            COLORS.listBullet[0],
            COLORS.listBullet[1],
            COLORS.listBullet[2],
            false
          );
        }
        break;

      case TokenType.Checkbox:
        // Style checkbox and bullet
        const checkboxMatch = token.text.match(/^(\s*[-*+]\s+\[[ x]\])/);
        if (checkboxMatch) {
          const checkboxEnd = token.start + checkboxMatch[0].length;
          editor.addOverlay(
            bufferId,
            `md:checkbox:${token.start}`,
            token.start,
            checkboxEnd,
            COLORS.checkbox[0],
            COLORS.checkbox[1],
            COLORS.checkbox[2],
            false
          );
        }
        break;
    }

    if (color) {
      editor.addOverlay(
        bufferId,
        overlayId,
        token.start,
        token.end,
        color[0],
        color[1],
        color[2],
        underline
      );
    }
  }
}

// Build view transform with soft breaks
function buildViewTransform(
  bufferId: number,
  splitId: number | null,
  text: string,
  viewportStart: number,
  viewportEnd: number,
  tokens: Token[]
): void {
  const viewTokens: ViewTokenWire[] = [];

  // Get the relevant portion of text
  const viewportText = text.substring(viewportStart, viewportEnd);

  // Track which lines should have hard breaks
  let lineStart = viewportStart;
  let i = 0;

  while (i < viewportText.length) {
    const absOffset = viewportStart + i;
    const ch = viewportText[i];

    if (ch === '\n') {
      // Check if this line should have a hard break
      const hasHardBreak = tokens.some(t =>
        (t.type === TokenType.HardBreak ||
         t.type === TokenType.Header1 ||
         t.type === TokenType.Header2 ||
         t.type === TokenType.Header3 ||
         t.type === TokenType.Header4 ||
         t.type === TokenType.Header5 ||
         t.type === TokenType.Header6 ||
         t.type === TokenType.ListItem ||
         t.type === TokenType.OrderedListItem ||
         t.type === TokenType.Checkbox ||
         t.type === TokenType.BlockQuote ||
         t.type === TokenType.CodeBlockFence ||
         t.type === TokenType.CodeBlockContent ||
         t.type === TokenType.HorizontalRule) &&
        t.start <= lineStart && t.end >= lineStart
      );

      // Empty lines are also hard breaks
      const lineContent = viewportText.substring(lineStart - viewportStart, i).trim();
      const isEmptyLine = lineContent.length === 0;

      if (hasHardBreak || isEmptyLine) {
        // Hard break - keep newline
        viewTokens.push({
          source_offset: absOffset,
          kind: "Newline",
        });
      } else {
        // Soft break - replace with space
        viewTokens.push({
          source_offset: absOffset,
          kind: "Space",
        });
      }

      lineStart = absOffset + 1;
      i++;
    } else if (ch === ' ') {
      viewTokens.push({
        source_offset: absOffset,
        kind: "Space",
      });
      i++;
    } else {
      // Accumulate consecutive text characters
      let textStart = i;
      let textContent = '';
      while (i < viewportText.length) {
        const c = viewportText[i];
        if (c === '\n' || c === ' ') {
          break;
        }
        textContent += c;
        i++;
      }

      viewTokens.push({
        source_offset: viewportStart + textStart,
        kind: { Text: textContent },
      });
    }
  }

  // Submit the view transform with layout hints
  const layoutHints: LayoutHints = {
    compose_width: config.composeWidth,
    column_guides: null,
  };

  editor.debug(`buildViewTransform: submitting ${viewTokens.length} tokens, compose_width=${config.composeWidth}`);
  if (viewTokens.length > 0 && viewTokens.length < 10) {
    editor.debug(`buildViewTransform: first tokens: ${JSON.stringify(viewTokens.slice(0, 5))}`);
  }

  const success = editor.submitViewTransform(
    bufferId,
    splitId,
    viewportStart,
    viewportEnd,
    viewTokens,
    layoutHints
  );

  editor.debug(`buildViewTransform: submit result = ${success}`);
}

// Process a buffer in compose mode
function processBuffer(bufferId: number, splitId?: number): void {
  if (!config.enabled) return;
  if (!composeBuffers.has(bufferId)) return;

  // Get buffer info
  const info = editor.getBufferInfo(bufferId);
  if (!info) {
    editor.debug(`processBuffer: no buffer info for ${bufferId}`);
    return;
  }

  // Only process markdown files
  if (!info.path.endsWith('.md') && !info.path.endsWith('.markdown')) {
    editor.debug(`processBuffer: not a markdown file: ${info.path}`);
    return;
  }

  editor.debug(`processBuffer: processing ${info.path}, buffer_id=${bufferId}`);

  // Get buffer content
  const bufferLength = editor.getBufferLength(bufferId);
  editor.debug(`processBuffer: getBufferLength returned ${bufferLength}`);

  const text = editor.getBufferText(bufferId, 0, bufferLength);
  editor.debug(`processBuffer: getBufferText returned ${text.length} bytes, first 100 chars: ${text.substring(0, 100)}`);

  // Parse markdown
  const parser = new MarkdownParser(text);
  const tokens = parser.parse();
  editor.debug(`processBuffer: parsed ${tokens.length} markdown tokens`);

  // Apply styling with overlays
  applyMarkdownStyling(bufferId, tokens);
  editor.debug(`processBuffer: applied styling overlays`);

  // Get viewport info (no buffer_id parameter - it's for the active buffer)
  const viewport = editor.getViewport();
  if (!viewport) {
    editor.debug(`processBuffer: no viewport, processing whole buffer`);
    // No viewport, process whole buffer
    const viewportStart = 0;
    const viewportEnd = text.length;
    buildViewTransform(bufferId, splitId || null, text, viewportStart, viewportEnd, tokens);
    return;
  }

  // Calculate viewport range
  // We need to process a bit more than the visible area to handle wrapping
  const viewportStart = Math.max(0, viewport.top_byte - 500);
  const viewportEnd = Math.min(text.length, viewport.top_byte + (viewport.height * 200));
  editor.debug(`processBuffer: viewport ${viewportStart}-${viewportEnd}, top_byte=${viewport.top_byte}, height=${viewport.height}`);

  // Build and submit view transform
  buildViewTransform(bufferId, splitId || null, text, viewportStart, viewportEnd, tokens);
}

// Enable markdown compose for a buffer
function enableMarkdownCompose(bufferId: number): void {
  const info = editor.getBufferInfo(bufferId);
  if (!info) return;

  // Only work with markdown files
  if (!info.path.endsWith('.md') && !info.path.endsWith('.markdown')) {
    return;
  }

  if (!composeBuffers.has(bufferId)) {
    composeBuffers.add(bufferId);
    config.enabled = true;
    processBuffer(bufferId);
    editor.debug(`Markdown compose enabled for buffer ${bufferId}`);
  }
}

// Disable markdown compose for a buffer
function disableMarkdownCompose(bufferId: number): void {
  if (composeBuffers.has(bufferId)) {
    composeBuffers.delete(bufferId);
    editor.removeOverlaysByPrefix(bufferId, "md:");
    // Clear view transform to return to normal rendering
    // (Submit empty transform = identity/source view)
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
    editor.setStatus("Not a Markdown file");
    return;
  }

  if (composeBuffers.has(bufferId)) {
    disableMarkdownCompose(bufferId);
    editor.setStatus("Markdown Compose: OFF");
  } else {
    enableMarkdownCompose(bufferId);
    // Trigger a re-render to apply the transform
    editor.refreshLines(bufferId);
    editor.setStatus("Markdown Compose: ON (soft breaks, styled)");
  }
};

// Handle view transform request - receives tokens from core for transformation
// This is the streaming approach: core pushes tokens, plugin transforms them
globalThis.onMarkdownViewTransform = function(data: {
  buffer_id: number;
  split_id: number;
  viewport_start: number;
  viewport_end: number;
  tokens: ViewTokenWire[];
}): void {
  if (!config.enabled) return;
  if (!composeBuffers.has(data.buffer_id)) return;

  const info = editor.getBufferInfo(data.buffer_id);
  if (!info) return;
  if (!info.path.endsWith('.md') && !info.path.endsWith('.markdown')) return;

  editor.debug(`onMarkdownViewTransform: buffer=${data.buffer_id}, split=${data.split_id}, tokens=${data.tokens.length}`);

  // Reconstruct text from tokens for parsing (we need text for markdown parsing)
  let reconstructedText = '';
  for (const token of data.tokens) {
    if (typeof token.kind === 'object' && 'Text' in token.kind) {
      reconstructedText += token.kind.Text;
    } else if (token.kind === 'Newline') {
      reconstructedText += '\n';
    } else if (token.kind === 'Space') {
      reconstructedText += ' ';
    }
  }

  // Parse markdown from reconstructed text
  const parser = new MarkdownParser(reconstructedText);
  const mdTokens = parser.parse();

  // Apply overlays for styling (this still works via the existing overlay API)
  // Offset the markdown tokens by viewport_start for correct positioning
  const offsetTokens = mdTokens.map(t => ({
    ...t,
    start: t.start + data.viewport_start,
    end: t.end + data.viewport_start,
  }));
  applyMarkdownStyling(data.buffer_id, offsetTokens);

  // Transform the view tokens based on markdown structure
  // Convert newlines to spaces for soft breaks (paragraphs)
  const transformedTokens = transformTokensForMarkdown(data.tokens, mdTokens, data.viewport_start);

  // Submit the transformed tokens
  const layoutHints: LayoutHints = {
    compose_width: config.composeWidth,
    column_guides: null,
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

// Transform view tokens based on markdown structure
function transformTokensForMarkdown(
  tokens: ViewTokenWire[],
  mdTokens: Token[],
  viewportStart: number
): ViewTokenWire[] {
  const result: ViewTokenWire[] = [];

  // Build a set of positions that should have hard breaks
  const hardBreakPositions = new Set<number>();
  for (const t of mdTokens) {
    if (t.type === TokenType.HardBreak ||
        t.type === TokenType.Header1 ||
        t.type === TokenType.Header2 ||
        t.type === TokenType.Header3 ||
        t.type === TokenType.Header4 ||
        t.type === TokenType.Header5 ||
        t.type === TokenType.Header6 ||
        t.type === TokenType.ListItem ||
        t.type === TokenType.OrderedListItem ||
        t.type === TokenType.Checkbox ||
        t.type === TokenType.CodeBlockFence ||
        t.type === TokenType.CodeBlockContent ||
        t.type === TokenType.BlockQuote ||
        t.type === TokenType.HorizontalRule) {
      // Mark the end of these elements as hard breaks
      hardBreakPositions.add(t.end + viewportStart);
    }
  }

  // Also mark empty lines (two consecutive newlines) as hard breaks
  let lastWasNewline = false;
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    if (token.kind === 'Newline') {
      if (lastWasNewline && token.source_offset !== null) {
        hardBreakPositions.add(token.source_offset);
      }
      lastWasNewline = true;
    } else {
      lastWasNewline = false;
    }
  }

  // Transform tokens
  for (const token of tokens) {
    if (token.kind === 'Newline') {
      const pos = token.source_offset;
      if (pos !== null && hardBreakPositions.has(pos)) {
        // Keep as newline (hard break)
        result.push(token);
      } else {
        // Convert to space (soft break)
        result.push({
          source_offset: token.source_offset,
          kind: 'Space',
        });
      }
    } else {
      // Keep other tokens as-is
      result.push(token);
    }
  }

  return result;
}

// Handle render_start for overlays only (not transform)
globalThis.onMarkdownRenderStart = function(data: { buffer_id: number }): void {
  // Nothing to do here now - view_transform_request handles everything
};

// Handle content changes - clear seen lines to trigger re-transform
globalThis.onMarkdownAfterInsert = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;
  if (composeBuffers.has(data.buffer_id)) {
    editor.refreshLines(data.buffer_id);
  }
};

globalThis.onMarkdownAfterDelete = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;
  if (composeBuffers.has(data.buffer_id)) {
    editor.refreshLines(data.buffer_id);
  }
};

// Register hooks - use the new streaming view_transform_request hook
editor.on("view_transform_request", "onMarkdownViewTransform");
editor.on("render_start", "onMarkdownRenderStart");
editor.on("after-insert", "onMarkdownAfterInsert");
editor.on("after-delete", "onMarkdownAfterDelete");

// Register command
editor.registerCommand(
  "Markdown: Toggle Compose",
  "Toggle beautiful Markdown rendering (soft breaks, syntax highlighting)",
  "markdownToggleCompose",
  "normal"
);

// Initialization
editor.debug("Markdown Compose plugin loaded - use 'Markdown: Toggle Compose' command");
editor.setStatus("Markdown plugin ready");
