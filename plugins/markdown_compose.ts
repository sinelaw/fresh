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

  editor.submitViewTransform(
    bufferId,
    splitId,
    viewportStart,
    viewportEnd,
    viewTokens,
    layoutHints
  );
}

// Process a buffer in compose mode
function processBuffer(bufferId: number, splitId?: number): void {
  if (!config.enabled) return;
  if (!composeBuffers.has(bufferId)) return;

  // Get buffer info
  const info = editor.getBufferInfo(bufferId);
  if (!info) return;

  // Only process markdown files
  if (!info.path.endsWith('.md') && !info.path.endsWith('.markdown')) {
    return;
  }

  // Get buffer content
  const text = editor.getBufferText(bufferId);

  // Parse markdown
  const parser = new MarkdownParser(text);
  const tokens = parser.parse();

  // Apply styling with overlays
  applyMarkdownStyling(bufferId, tokens);

  // Get viewport info
  const viewport = editor.getViewportInfo(bufferId);
  if (!viewport) {
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

  // Build and submit view transform
  buildViewTransform(bufferId, splitId || null, text, viewportStart, viewportEnd, tokens);
}

// Toggle compose mode
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
    // Disable compose mode
    composeBuffers.delete(bufferId);
    editor.removeOverlaysByPrefix(bufferId, "md:");
    editor.setStatus("Markdown Compose: Disabled");
  } else {
    // Enable compose mode
    config.enabled = true;
    composeBuffers.add(bufferId);
    processBuffer(bufferId);
    editor.setStatus("Markdown Compose: Enabled");
  }
};

// Handle render events
globalThis.onMarkdownRenderStart = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;
  processBuffer(data.buffer_id);
};

// Handle content changes
globalThis.onMarkdownAfterInsert = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;
  if (composeBuffers.has(data.buffer_id)) {
    processBuffer(data.buffer_id);
  }
};

globalThis.onMarkdownAfterDelete = function(data: { buffer_id: number }): void {
  if (!config.enabled) return;
  if (composeBuffers.has(data.buffer_id)) {
    processBuffer(data.buffer_id);
  }
};

// Register hooks
editor.on("render_start", "onMarkdownRenderStart");
editor.on("after-insert", "onMarkdownAfterInsert");
editor.on("after-delete", "onMarkdownAfterDelete");

// Register commands
editor.registerCommand(
  "Markdown: Toggle Compose Mode",
  "Toggle semi-WYSIWYG Markdown rendering",
  "markdownToggleCompose",
  "normal"
);

// Initialization
editor.setStatus("Markdown Compose plugin loaded");
editor.debug("Markdown Compose initialized - press :MarkdownToggleCompose to enable");
