/// <reference path="./lib/fresh.d.ts" />
// Markdown Source Mode Plugin
// Provides smart editing features for Markdown files in source (non-compose) mode:
// - Enter: auto-continue list items (bullets, ordered, checkboxes) with matching
//   indentation; on an empty list item, removes the marker instead
// - Tab: on a blank list item, indents and cycles the bullet (* -> - -> + -> *);
//   otherwise inserts spaces (always spaces, never literal tabs)
// - Shift+Tab: on a blank list item, de-indents and cycles the bullet in reverse
//   (+ -> - -> * -> +); otherwise falls through to built-in dedent_selection
//
// This plugin defines a "markdown-source" mode that auto-activates when a
// markdown file is opened in source view, or when the buffer language is set
// to markdown. It uses readOnly=false so that normal character insertion is
// unaffected.

const editor = getEditor();

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

const TAB_SIZE = 4;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function isMarkdownFile(path: string): boolean {
  return path.endsWith(".md") || path.endsWith(".markdown") || path.endsWith(".mdx");
}

function isMarkdownLanguage(language: string): boolean {
  return language === "markdown" || language === "multimarkdown";
}

// Check whether a buffer should use markdown-source mode based on its
// file path OR its language setting.
function isMarkdownBuffer(info: BufferInfo): boolean {
  return isMarkdownFile(info.path) || isMarkdownLanguage(info.language);
}

// ---------------------------------------------------------------------------
// List marker parsing
// ---------------------------------------------------------------------------

interface ListMarkerInfo {
  type: "unordered" | "ordered" | "checkbox";
  indent: string;       // leading whitespace
  bullet?: string;      // -, *, + for unordered/checkbox
  number?: string;      // "1", "10" etc. for ordered
  checked?: boolean;    // for checkbox
  content: string;      // text after marker (may be empty)
}

// Parse the text of a line (from line start up to cursor) for a list marker.
// Returns null if the line doesn't start with a recognised list pattern.
function parseListMarker(lineText: string): ListMarkerInfo | null {
  let indent = "";
  let i = 0;
  while (i < lineText.length && (lineText[i] === " " || lineText[i] === "\t")) {
    indent += lineText[i];
    i++;
  }
  const rest = lineText.substring(i);

  // Checkbox: "- [ ] content" / "* [x] content" / "+ [X] "
  const cbMatch = rest.match(/^([-*+]) \[([ xX])\] (.*)$/);
  if (cbMatch) {
    return {
      type: "checkbox",
      indent,
      bullet: cbMatch[1],
      checked: cbMatch[2] !== " ",
      content: cbMatch[3],
    };
  }

  // Ordered list: "1. content" / "10. "
  const olMatch = rest.match(/^(\d+)\. (.*)$/);
  if (olMatch) {
    return {
      type: "ordered",
      indent,
      number: olMatch[1],
      content: olMatch[2],
    };
  }

  // Unordered list: "- content" / "* content" / "+ content"
  const ulMatch = rest.match(/^([-*+]) (.*)$/);
  if (ulMatch) {
    return {
      type: "unordered",
      indent,
      bullet: ulMatch[1],
      content: ulMatch[2],
    };
  }

  return null;
}

// Build the marker text for the *next* list item (used by Enter handler).
function nextMarkerText(info: ListMarkerInfo): string {
  if (info.type === "ordered") {
    const num = parseInt(info.number!, 10) + 1;
    return info.indent + num + ". ";
  }
  if (info.type === "checkbox") {
    // New checkbox is always unchecked
    return info.indent + info.bullet + " [ ] ";
  }
  // unordered — same bullet
  return info.indent + info.bullet + " ";
}

// Cycle bullet character forward: * -> - -> + -> *
function cycleBullet(bullet: string): string {
  switch (bullet) {
    case "*": return "-";
    case "-": return "+";
    case "+": return "*";
    default: return "-";
  }
}

// Cycle bullet character in reverse: * -> + -> - -> *
function reverseCycleBullet(bullet: string): string {
  switch (bullet) {
    case "*": return "+";
    case "+": return "-";
    case "-": return "*";
    default: return "-";
  }
}

// Read the text on the current line after the cursor (up to the next newline).
async function readRestOfLine(bufferId: number, cursorPos: number): Promise<string> {
  const bufLen = editor.getBufferLength(bufferId);
  const afterLen = Math.min(1024, bufLen - cursorPos);
  if (afterLen <= 0) return "";
  const textAfter = await editor.getBufferText(bufferId, cursorPos, cursorPos + afterLen);
  const nextNl = textAfter.indexOf("\n");
  return nextNl >= 0 ? textAfter.substring(0, nextNl) : textAfter;
}

// ---------------------------------------------------------------------------
// Enter handler: auto-continue list items or match indentation
// ---------------------------------------------------------------------------

globalThis.md_src_enter = async function (): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (!bufferId) {
    editor.executeAction("insert_newline");
    return;
  }

  const cursorPos = editor.getCursorPosition();

  // Read a window of text before the cursor to find the current line.
  const windowStart = Math.max(0, cursorPos - 1024);
  const textWindow = await editor.getBufferText(bufferId, windowStart, cursorPos);

  const lastNl = textWindow.lastIndexOf("\n");
  const lineText = lastNl >= 0 ? textWindow.substring(lastNl + 1) : textWindow;

  // Try to parse as a list item
  const listMatch = parseListMarker(lineText);

  if (listMatch) {
    const restOfLine = await readRestOfLine(bufferId, cursorPos);

    if (listMatch.content.trim() === "" && restOfLine.trim() === "") {
      // Empty list item (just marker, no content) — remove the marker
      const lineStartByte = cursorPos - editor.utf8ByteLength(lineText);
      const lineEndByte = cursorPos + editor.utf8ByteLength(restOfLine);
      editor.deleteRange(bufferId, lineStartByte, lineEndByte);
      return;
    }

    // Non-empty list item — insert newline + next marker
    editor.insertAtCursor("\n" + nextMarkerText(listMatch));
    return;
  }

  // No list marker — just copy leading whitespace
  let indent = "";
  for (let i = 0; i < lineText.length; i++) {
    const ch = lineText[i];
    if (ch === " " || ch === "\t") {
      indent += ch;
    } else {
      break;
    }
  }
  editor.insertAtCursor("\n" + indent);
};

// ---------------------------------------------------------------------------
// Tab handler: indent + cycle bullet on blank list items, else insert spaces
// ---------------------------------------------------------------------------

globalThis.md_src_tab = async function (): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (!bufferId) {
    editor.insertAtCursor(" ".repeat(TAB_SIZE));
    return;
  }

  const cursorPos = editor.getCursorPosition();
  const windowStart = Math.max(0, cursorPos - 1024);
  const textBefore = await editor.getBufferText(bufferId, windowStart, cursorPos);

  const lastNl = textBefore.lastIndexOf("\n");
  const lineText = lastNl >= 0 ? textBefore.substring(lastNl + 1) : textBefore;

  const listMatch = parseListMarker(lineText);

  if (listMatch && (listMatch.type === "unordered" || listMatch.type === "checkbox")) {
    const restOfLine = await readRestOfLine(bufferId, cursorPos);

    if (listMatch.content.trim() === "" && restOfLine.trim() === "") {
      // Blank list item — indent + cycle bullet
      const lineStartByte = cursorPos - editor.utf8ByteLength(lineText);
      const lineEndByte = cursorPos + editor.utf8ByteLength(restOfLine);
      const newBullet = cycleBullet(listMatch.bullet!);
      let newLine: string;
      if (listMatch.type === "checkbox") {
        const check = listMatch.checked ? "x" : " ";
        newLine = listMatch.indent + " ".repeat(TAB_SIZE) + newBullet + " [" + check + "] ";
      } else {
        newLine = listMatch.indent + " ".repeat(TAB_SIZE) + newBullet + " ";
      }
      editor.deleteRange(bufferId, lineStartByte, lineEndByte);
      editor.insertText(bufferId, lineStartByte, newLine);
      editor.setBufferCursor(bufferId, lineStartByte + editor.utf8ByteLength(newLine));
      return;
    }
  }

  // Default: insert spaces
  editor.insertAtCursor(" ".repeat(TAB_SIZE));
};

// ---------------------------------------------------------------------------
// Shift+Tab handler: de-indent + reverse-cycle bullet on blank list items
// ---------------------------------------------------------------------------

globalThis.md_src_shift_tab = async function (): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (!bufferId) {
    editor.executeAction("dedent_selection");
    return;
  }

  const cursorPos = editor.getCursorPosition();
  const windowStart = Math.max(0, cursorPos - 1024);
  const textBefore = await editor.getBufferText(bufferId, windowStart, cursorPos);

  const lastNl = textBefore.lastIndexOf("\n");
  const lineText = lastNl >= 0 ? textBefore.substring(lastNl + 1) : textBefore;

  const listMatch = parseListMarker(lineText);

  if (listMatch && (listMatch.type === "unordered" || listMatch.type === "checkbox")) {
    const restOfLine = await readRestOfLine(bufferId, cursorPos);

    if (listMatch.content.trim() === "" && restOfLine.trim() === "") {
      // Blank list item — de-indent + reverse cycle bullet
      const currentIndent = listMatch.indent;
      // Only de-indent if there is indentation to remove
      if (currentIndent.length >= TAB_SIZE) {
        const lineStartByte = cursorPos - editor.utf8ByteLength(lineText);
        const lineEndByte = cursorPos + editor.utf8ByteLength(restOfLine);
        const newBullet = reverseCycleBullet(listMatch.bullet!);
        const newIndent = currentIndent.substring(TAB_SIZE);
        let newLine: string;
        if (listMatch.type === "checkbox") {
          const check = listMatch.checked ? "x" : " ";
          newLine = newIndent + newBullet + " [" + check + "] ";
        } else {
          newLine = newIndent + newBullet + " ";
        }
        editor.deleteRange(bufferId, lineStartByte, lineEndByte);
        editor.insertText(bufferId, lineStartByte, newLine);
        editor.setBufferCursor(bufferId, lineStartByte + editor.utf8ByteLength(newLine));
        return;
      }
    }
  }

  // Default: fall through to built-in dedent
  editor.executeAction("dedent_selection");
};

// ---------------------------------------------------------------------------
// Mode definition
// ---------------------------------------------------------------------------

// Define a non-read-only mode so unmapped keys insert normally.
// Enter, Tab, and Shift+Tab are intercepted for smart list handling.
editor.defineMode("markdown-source", null, [
  ["Enter", "md_src_enter"],
  ["Tab", "md_src_tab"],
  ["BackTab", "md_src_shift_tab"],
], false);

// ---------------------------------------------------------------------------
// Auto-activation: switch mode when a markdown file is focused or language changes
// ---------------------------------------------------------------------------

function updateMarkdownMode(): void {
  const bufferId = editor.getActiveBufferId();
  if (!bufferId) return;

  const info = editor.getBufferInfo(bufferId);
  if (!info) return;

  const currentMode = editor.getEditorMode();

  if (isMarkdownBuffer(info) && info.view_mode === "source") {
    // Only activate if no other mode is already set (e.g., vi-mode)
    if (currentMode == null) {
      editor.setEditorMode("markdown-source");
    }
  } else {
    // Leaving a markdown file or switching to compose mode: deactivate
    if (currentMode === "markdown-source") {
      editor.setEditorMode(null);
    }
  }
}

globalThis.md_src_on_buffer_activated = function (): void {
  updateMarkdownMode();
};

globalThis.md_src_on_language_changed = function (): void {
  updateMarkdownMode();
};

editor.on("buffer_activated", "md_src_on_buffer_activated");
editor.on("language_changed", "md_src_on_language_changed");

editor.debug("markdown_source plugin loaded");
