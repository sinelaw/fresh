// Markdown Compose Mode Plugin
// Provides beautiful, semi-WYSIWYG rendering of Markdown documents
// - Highlighting: automatically enabled for all markdown files
// - Compose mode: explicitly toggled, adds margins, soft-wrapping, different editing

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

// Track buffers with highlighting enabled (auto for markdown files)
const highlightingBuffers = new Set<number>();

// Track buffers in compose mode (explicit toggle)
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
  Image,  // Images should have hard breaks (not soft breaks)
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
  fence: [80, 80, 80] as [number, number, number],     // Subdued gray for ```
  link: [86, 156, 214] as [number, number, number],    // Light blue
  linkUrl: [80, 80, 80] as [number, number, number],   // Subdued gray
  bold: [255, 255, 220] as [number, number, number],   // Bright for bold text
  boldMarker: [80, 80, 80] as [number, number, number], // Subdued for ** markers
  italic: [198, 180, 221] as [number, number, number], // Light purple for italic
  italicMarker: [80, 80, 80] as [number, number, number], // Subdued for * markers
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

    // Images: ![alt](url) - these should have hard breaks to keep each on its own line
    if (trimmed.match(/^!\[.*\]\(.*\)$/)) {
      this.tokens.push({
        type: TokenType.Image,
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
        // Style bold markers (** or __) subdued, content bold
        const boldMatch = token.text.match(/^(\*\*|__)(.*)(\*\*|__)$/);
        if (boldMatch) {
          const markerLen = boldMatch[1].length;
          // Subdued markers
          editor.addOverlay(bufferId, `md:bold-start:${token.start}`,
            token.start, token.start + markerLen,
            COLORS.boldMarker[0], COLORS.boldMarker[1], COLORS.boldMarker[2], false, false, false);
          editor.addOverlay(bufferId, `md:bold-end:${token.start}`,
            token.end - markerLen, token.end,
            COLORS.boldMarker[0], COLORS.boldMarker[1], COLORS.boldMarker[2], false, false, false);
          // Bold content with bold=true
          editor.addOverlay(bufferId, `md:bold-content:${token.start}`,
            token.start + markerLen, token.end - markerLen,
            COLORS.bold[0], COLORS.bold[1], COLORS.bold[2], false, true, false);
        } else {
          color = COLORS.bold;
        }
        break;

      case TokenType.Italic:
        // Style italic markers (* or _) subdued, content italic
        const italicMatch = token.text.match(/^(\*|_)(.*)(\*|_)$/);
        if (italicMatch) {
          const markerLen = 1;
          // Subdued markers
          editor.addOverlay(bufferId, `md:italic-start:${token.start}`,
            token.start, token.start + markerLen,
            COLORS.italicMarker[0], COLORS.italicMarker[1], COLORS.italicMarker[2], false, false, false);
          editor.addOverlay(bufferId, `md:italic-end:${token.start}`,
            token.end - markerLen, token.end,
            COLORS.italicMarker[0], COLORS.italicMarker[1], COLORS.italicMarker[2], false, false, false);
          // Italic content with italic=true
          editor.addOverlay(bufferId, `md:italic-content:${token.start}`,
            token.start + markerLen, token.end - markerLen,
            COLORS.italic[0], COLORS.italic[1], COLORS.italic[2], false, false, true);
        } else {
          color = COLORS.italic;
        }
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
         t.type === TokenType.HorizontalRule ||
         t.type === TokenType.Image) &&
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

// Check if a file is a markdown file
function isMarkdownFile(path: string): boolean {
  return path.endsWith('.md') || path.endsWith('.markdown');
}

// Apply highlighting only (no compose mode features)
function applyHighlighting(bufferId: number): void {
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  const bufferLength = editor.getBufferLength(bufferId);
  const text = editor.getBufferText(bufferId, 0, bufferLength);
  const parser = new MarkdownParser(text);
  const tokens = parser.parse();
  applyMarkdownStyling(bufferId, tokens);
}

// Process a buffer in compose mode (highlighting + view transform)
function processBuffer(bufferId: number, splitId?: number): void {
  if (!composeBuffers.has(bufferId)) return;

  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  editor.debug(`processBuffer: processing ${info.path}, buffer_id=${bufferId}`);

  const bufferLength = editor.getBufferLength(bufferId);
  const text = editor.getBufferText(bufferId, 0, bufferLength);
  const parser = new MarkdownParser(text);
  const tokens = parser.parse();

  // Apply styling with overlays
  applyMarkdownStyling(bufferId, tokens);

  // Get viewport info and build view transform
  const viewport = editor.getViewport();
  if (!viewport) {
    const viewportStart = 0;
    const viewportEnd = text.length;
    buildViewTransform(bufferId, splitId || null, text, viewportStart, viewportEnd, tokens);
    return;
  }

  const viewportStart = Math.max(0, viewport.top_byte - 500);
  const viewportEnd = Math.min(text.length, viewport.top_byte + (viewport.height * 200));
  buildViewTransform(bufferId, splitId || null, text, viewportStart, viewportEnd, tokens);
}

// Enable highlighting for a markdown buffer (auto on file open)
function enableHighlighting(bufferId: number): void {
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  if (!highlightingBuffers.has(bufferId)) {
    highlightingBuffers.add(bufferId);
    applyHighlighting(bufferId);
    editor.debug(`Markdown highlighting enabled for buffer ${bufferId}`);
  }
}

// Enable full compose mode for a buffer (explicit toggle)
function enableMarkdownCompose(bufferId: number): void {
  const info = editor.getBufferInfo(bufferId);
  if (!info || !isMarkdownFile(info.path)) return;

  if (!composeBuffers.has(bufferId)) {
    composeBuffers.add(bufferId);
    highlightingBuffers.add(bufferId);  // Also ensure highlighting is on
    processBuffer(bufferId);
    editor.debug(`Markdown compose enabled for buffer ${bufferId}`);
  }
}

// Disable compose mode for a buffer (but keep highlighting)
function disableMarkdownCompose(bufferId: number): void {
  if (composeBuffers.has(bufferId)) {
    composeBuffers.delete(bufferId);
    // Keep highlighting on, just clear the view transform
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
// Only applies transforms when in compose mode (not just highlighting)
globalThis.onMarkdownViewTransform = function(data: {
  buffer_id: number;
  split_id: number;
  viewport_start: number;
  viewport_end: number;
  tokens: ViewTokenWire[];
}): void {
  // Only transform when in compose mode (view transforms change line wrapping etc)
  if (!composeBuffers.has(data.buffer_id)) return;

  const info = editor.getBufferInfo(data.buffer_id);
  if (!info || !isMarkdownFile(info.path)) return;

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
        t.type === TokenType.HorizontalRule ||
        t.type === TokenType.Image) {
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

// Handle render_start - apply highlighting for markdown files
globalThis.onMarkdownRenderStart = function(data: { buffer_id: number }): void {
  // Auto-enable highlighting for markdown files
  if (highlightingBuffers.has(data.buffer_id)) {
    applyHighlighting(data.buffer_id);
  }
};

// Handle buffer activation - auto-enable highlighting for markdown files
globalThis.onMarkdownBufferActivated = function(data: { buffer_id: number }): void {
  enableHighlighting(data.buffer_id);
};

// Handle content changes - refresh highlighting and transforms
globalThis.onMarkdownAfterInsert = function(data: { buffer_id: number }): void {
  if (highlightingBuffers.has(data.buffer_id)) {
    applyHighlighting(data.buffer_id);
  }
  if (composeBuffers.has(data.buffer_id)) {
    editor.refreshLines(data.buffer_id);
  }
};

globalThis.onMarkdownAfterDelete = function(data: { buffer_id: number }): void {
  if (highlightingBuffers.has(data.buffer_id)) {
    applyHighlighting(data.buffer_id);
  }
  if (composeBuffers.has(data.buffer_id)) {
    editor.refreshLines(data.buffer_id);
  }
};

// Register hooks
editor.on("view_transform_request", "onMarkdownViewTransform");
editor.on("render_start", "onMarkdownRenderStart");
editor.on("buffer_activated", "onMarkdownBufferActivated");
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
