/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Vi Mode Plugin for Fresh Editor
 *
 * Implements vi-style modal editing with:
 * - Normal mode: navigation and commands
 * - Insert mode: text input
 * - Operator-pending mode: composable operators with motions
 *
 * Uses the plugin API's executeAction() for true operator+motion composability:
 * any operator works with any motion via O(operators + motions) code.
 *
 * TODO: This plugin uses APIs that don't exist yet:
 * - getLineStartPosition() - for visual block mode column calculation
 * - defineMode with null parent - needs string parent mode
 */

// Vi mode state
type ViMode = "normal" | "insert" | "operator-pending" | "find-char" | "visual" | "visual-line" | "visual-block" | "text-object";
type FindCharType = "f" | "t" | "F" | "T" | null;
type TextObjectType = "inner" | "around" | null;
type ModeBinding = [string, string];
type WORDMotionKind = "forward" | "backward" | "end";
type WordSearchDirection = "forward" | "backward";
type WordSearchTarget = { start: number; end: number; text: string; wholeWord: boolean };

// Types for tracking repeatable changes
type ChangeType = "simple" | "operator-motion" | "operator-textobj" | "insert" | "line-op";

interface LastChange {
  type: ChangeType;
  action?: string;           // For simple actions like "delete_forward", "delete_line"
  operator?: string;         // For operator+motion/textobj: "d", "c", "y"
  motion?: string;           // For operator+motion: the motion action
  textObject?: { modifier: TextObjectType; object: string }; // For operator+textobj
  count?: number;            // Count used with the command
  insertedText?: string;     // Text inserted during insert mode
}

interface ViState {
  mode: ViMode;
  pendingOperator: string | null;
  pendingFindChar: FindCharType; // For f/t/F/T motions
  pendingTextObject: TextObjectType; // For i/a text objects
  lastFindChar: { type: FindCharType; char: string } | null; // For ; and , repeat
  count: number | null;
  lastChange: LastChange | null; // For '.' repeat
  lastYankWasLinewise: boolean; // Track if last yank was line-wise for proper paste
  visualAnchor: number | null; // Starting position for visual mode selection
  visualHead: number | null; // Active end of computed visual selections
  visualRange: { start: number; end: number } | null; // Characterwise visual range for computed motions
  insertStartPos: number | null; // Cursor position when entering insert mode
  visualBlockAnchor: { line: number; col: number } | null; // For visual block mode
  lastWordSearch: { text: string; direction: WordSearchDirection; wholeWord: boolean } | null; // For n/N after * or #
}

const state: ViState = {
  mode: "normal",
  pendingOperator: null,
  pendingFindChar: null,
  pendingTextObject: null,
  lastFindChar: null,
  count: null,
  lastChange: null,
  lastYankWasLinewise: false,
  visualAnchor: null,
  visualHead: null,
  visualRange: null,
  insertStartPos: null,
  visualBlockAnchor: null,
  lastWordSearch: null,
};

const autoStart = editor.defineConfigBoolean("autoStart", {
  default: false,
  description:
    "Automatically enable vi mode when the editor starts. Default off — users opt in.",
});

const arrowKeys = editor.defineConfigBoolean("arrowKeys", {
  default: true,
  description:
    "Enable arrow key navigation in vi mode.",
});

const searchWordUnderCursor = editor.defineConfigBoolean("searchWordUnderCursor", {
  default: true,
  description:
    "Enable * and # to search for the word under the cursor.",
});

function configuredBindings(enabled: boolean, bindings: ModeBinding[]): ModeBinding[] {
  return enabled ? bindings : [];
}

// Safe getBufferText that clamps end to buffer length
async function safeGetBufferText(bufferId: number, start: number, end: number): Promise<string | null> {
  const bufLen = editor.getBufferLength(bufferId);
  const clampedEnd = Math.min(end, bufLen);
  if (clampedEnd <= start) return null;
  return editor.getBufferText(bufferId, start, clampedEnd);
}

// Mode indicator for status bar
function getModeIndicator(mode: ViMode): string {
  const countPrefix = state.count !== null ? `${state.count} ` : "";
  switch (mode) {
    case "normal":
      return `-- ${editor.t("mode.normal")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "insert":
      return `-- ${editor.t("mode.insert")} --`;
    case "operator-pending":
      return `-- ${editor.t("mode.operator")} (${state.pendingOperator}) --${countPrefix ? ` (${state.count})` : ""}`;
    case "find-char":
      return `-- ${editor.t("mode.find")} (${state.pendingFindChar}) --`;
    case "visual":
      return `-- ${editor.t("mode.visual")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "visual-line":
      return `-- ${editor.t("mode.visual_line")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "visual-block":
      return `-- ${editor.t("mode.visual_block")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "text-object":
      return `-- ${state.pendingOperator}${state.pendingTextObject === "inner" ? "i" : "a"}? --`;
    default:
      return "";
  }
}

// Switch between modes
function switchMode(newMode: ViMode): void {
  const oldMode = state.mode;
  state.mode = newMode;

  // Only clear pendingOperator when leaving operator-pending and text-object modes
  if (newMode !== "operator-pending" && newMode !== "text-object") {
    state.pendingOperator = null;
  }

  // Clear text object type when leaving text-object mode
  if (newMode !== "text-object") {
    state.pendingTextObject = null;
  }

  // Preserve count when entering operator-pending or text-object mode (for 3dw = delete 3 words)
  // Also preserve count in visual modes
  if (newMode !== "operator-pending" && newMode !== "text-object" &&
      newMode !== "visual" && newMode !== "visual-line" && newMode !== "visual-block") {
    state.count = null;
  }

  // Clear visual anchor when leaving visual modes
  if (newMode !== "visual" && newMode !== "visual-line" && newMode !== "visual-block") {
    state.visualAnchor = null;
    state.visualHead = null;
    state.visualRange = null;
    state.visualBlockAnchor = null;
    // Clear any selection when leaving visual mode by moving cursor
    // (any non-select movement clears selection in Fresh)
    if (oldMode === "visual" || oldMode === "visual-line" || oldMode === "visual-block") {
      editor.executeAction("move_left");
      editor.executeAction("move_right");
    }
  }

  // Track insert mode start position for '.' repeat
  if (newMode === "insert" && oldMode !== "insert") {
    state.insertStartPos = editor.getCursorPosition();
  }

  // Capture inserted text when leaving insert mode (for '.' repeat)
  if (oldMode === "insert" && newMode !== "insert" && state.insertStartPos !== null) {
    captureInsertedText();
  }

  // All modes use vi-{mode} naming, including insert mode
  // vi-insert has read_only=false so normal typing works, but Escape is bound
  editor.setEditorMode(`vi-${newMode}`);
  editor.setStatus(getModeIndicator(newMode));
}

// Capture text inserted during insert mode for '.' repeat
async function captureInsertedText(): Promise<void> {
  if (state.insertStartPos === null) return;

  const endPos = editor.getCursorPosition();
  if (endPos === null || endPos <= state.insertStartPos) {
    state.insertStartPos = null;
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const text = await editor.getBufferText(bufferId, state.insertStartPos, endPos);

  if (text && text.length > 0) {
    // Only record if we have a pending insert change or if there was actual text inserted
    if (state.lastChange?.type === "insert" || !state.lastChange) {
      state.lastChange = {
        type: "insert",
        insertedText: text,
      };
    } else if (state.lastChange.type === "simple" || state.lastChange.type === "operator-motion" ||
               state.lastChange.type === "operator-textobj" || state.lastChange.type === "line-op") {
      // A change command (c, s, etc.) was used - append the inserted text
      state.lastChange.insertedText = text;
    }
  }

  state.insertStartPos = null;
}

// Get the current count (defaults to 1 if no count specified)
// Does NOT clear the count - that's done in switchMode or explicitly
function getCount(): number {
  return state.count ?? 1;
}

// Consume the current count and clear it
// Returns the count (defaults to 1)
function consumeCount(): number {
  const count = state.count ?? 1;
  if (state.count !== null) {
    state.count = null;
    // Update status to clear the count display
    editor.setStatus(getModeIndicator(state.mode));
  }
  return count;
}

function consumeCountOrDefault(defaultCount: number): number {
  if (state.count === null) {
    return defaultCount;
  }
  return consumeCount();
}

// Accumulate a digit into the count
function accumulateCount(digit: number): void {
  if (state.count === null) {
    state.count = digit;
  } else {
    state.count = state.count * 10 + digit;
  }
  // Update status to show accumulated count
  editor.setStatus(getModeIndicator(state.mode));
}

// Execute a single action with count (uses new executeActions API for efficiency)
function executeWithCount(action: string, count?: number): void {
  const n = count ?? consumeCount();
  if (n === 1) {
    editor.executeAction(action);
  } else {
    editor.executeActions([{ action, count: n }]);
  }
}

function selectWithCount(action: string, count: number): void {
  if (count === 1) {
    editor.executeAction(action);
  } else {
    editor.executeActions([{ action, count }]);
  }
}

function cutCharacterwiseSelection(hasSelectedRange: boolean): boolean {
  if (!hasSelectedRange) {
    return false;
  }

  editor.executeAction("cut");
  state.lastYankWasLinewise = false;
  return true;
}

function copyCharacterwiseSelection(hasSelectedRange: boolean): boolean {
  if (!hasSelectedRange) {
    return false;
  }

  state.lastYankWasLinewise = false;
  editor.executeAction("copy");
  return true;
}

async function canSelectionActionSelect(action: string, count: number): Promise<boolean> {
  if (count <= 0) {
    return false;
  }

  const cursor = editor.getPrimaryCursor();
  const position = cursor?.position ?? editor.getCursorPosition();
  const line = cursor?.line ?? null;
  const bufferId = editor.getActiveBufferId();

  switch (action) {
    case "select_left":
    case "select_word_left":
    case "select_document_start":
      return position > 0;
    case "select_right":
    case "select_word_right":
    case "vi_select_word_end":
    case "select_document_end":
      return position < editor.getBufferLength(bufferId);
    case "select_to_paragraph_up":
      return position > 0;
    case "select_to_paragraph_down":
      return position < editor.getBufferLength(bufferId);
    case "select_line_start": {
      if (line === null) {
        return false;
      }
      const lineStart = await editor.getLineStartPosition(line);
      return lineStart !== null && position > lineStart;
    }
    case "select_line_end": {
      if (line === null) {
        return false;
      }
      const lineEnd = await editor.getLineEndPosition(line);
      return lineEnd !== null && position < lineEnd;
    }
    case "select_up":
      return line !== null && line > 0;
    case "select_down": {
      if (line === null) {
        return false;
      }
      const lineCount = await editor.getBufferLineCount();
      return lineCount !== null && line < lineCount - 1;
    }
    default:
      return true;
  }
}

async function selectThenCutCharacterwise(action: string, count: number): Promise<boolean> {
  const hasSelectedRange = await canSelectionActionSelect(action, count);
  if (!hasSelectedRange) {
    return false;
  }

  selectWithCount(action, count);
  if (action === "vi_select_word_end") {
    editor.executeAction("select_right");
  }
  return cutCharacterwiseSelection(true);
}

async function selectThenCopyCharacterwise(action: string, count: number): Promise<boolean> {
  const hasSelectedRange = await canSelectionActionSelect(action, count);
  if (!hasSelectedRange) {
    return false;
  }

  selectWithCount(action, count);
  if (action === "vi_select_word_end") {
    editor.executeAction("select_right");
  }
  return copyCharacterwiseSelection(true);
}

function getLinewiseReplacementText(deletedText: string): string | null {
  const trailingTerminator = deletedText.match(/(\r\n|\n|\r)$/);
  if (trailingTerminator) {
    return trailingTerminator[0];
  }

  const firstTerminator = deletedText.match(/\r\n|\n|\r/);
  return firstTerminator?.[0] ?? null;
}

interface LinewiseRange {
  bufferId: number;
  start: number;
  end: number;
  text: string;
  lineTerminator: string;
}

async function getLinewiseTerminator(bufferId: number, start: number, text: string): Promise<string> {
  const ownTerminator = text.match(/\r\n|\n|\r/);
  if (ownTerminator) {
    return ownTerminator[0];
  }

  const sampleStart = Math.max(0, start - 4096);
  if (sampleStart < start) {
    const prefix = await editor.getBufferText(bufferId, sampleStart, start);
    const matches = prefix.match(/\r\n|\n|\r/g);
    const lastMatch = matches?.[matches.length - 1];
    if (lastMatch) {
      return lastMatch;
    }
  }

  return "\n";
}

function ensureLinewiseRegisterText(text: string, lineTerminator: string): string {
  return /(\r\n|\n|\r)$/.test(text) ? text : text + lineTerminator;
}

async function findLineStartAtPosition(bufferId: number, position: number): Promise<number> {
  let searchEnd = Math.max(0, position);

  while (searchEnd > 0) {
    const chunkStart = Math.max(0, searchEnd - 4096);
    const text = await editor.getBufferText(bufferId, chunkStart, searchEnd);
    const lf = text.lastIndexOf("\n");
    const cr = text.lastIndexOf("\r");
    const lineBreak = Math.max(lf, cr);
    if (lineBreak !== -1) {
      return chunkStart + editor.utf8ByteLength(text.slice(0, lineBreak + 1));
    }
    searchEnd = chunkStart;
  }

  return 0;
}

function nextLineTerminatorEnd(text: string, searchFrom: number): number | null {
  const lf = text.indexOf("\n", searchFrom);
  const cr = text.indexOf("\r", searchFrom);
  if (lf === -1 && cr === -1) {
    return null;
  }

  if (cr !== -1 && (lf === -1 || cr < lf)) {
    return text[cr + 1] === "\n" ? cr + 2 : cr + 1;
  }

  return lf + 1;
}

async function findLinewiseEndFromStart(bufferId: number, start: number, count: number): Promise<number> {
  let position = start;
  let remainingLines = count;
  const chunkSize = 4096;

  while (true) {
    const chunkEnd = position + chunkSize;
    const text = await editor.getBufferText(bufferId, position, chunkEnd);
    if (!text) {
      return position;
    }
    let searchFrom = 0;

    while (remainingLines > 0) {
      const terminatorEnd = nextLineTerminatorEnd(text, searchFrom);
      if (terminatorEnd === null) {
        break;
      }

      remainingLines--;
      const nextLineStart = position + editor.utf8ByteLength(text.slice(0, terminatorEnd));
      if (remainingLines === 0) {
        return nextLineStart;
      }
      searchFrom = terminatorEnd;
    }

    const consumed = editor.utf8ByteLength(text);
    if (consumed === 0) {
      return position;
    }
    position += consumed;
    if (consumed < chunkSize) {
      return position;
    }
  }
}

function isActiveBufferEditingDisabled(bufferId: number): boolean {
  return editor.getBufferInfo(bufferId)?.editing_disabled ?? false;
}

async function getLinewiseRange(count: number): Promise<LinewiseRange | null> {
  if (count <= 0) {
    return null;
  }

  const bufferId = editor.getActiveBufferId();
  const cursor = editor.getPrimaryCursor();
  const position = cursor?.position ?? editor.getCursorPosition();
  const start = await findLineStartAtPosition(bufferId, position);
  const end = await findLinewiseEndFromStart(bufferId, start, count);
  if (end <= start) {
    return null;
  }

  const text = await editor.getBufferText(bufferId, start, end);
  if (!text) {
    return null;
  }

  const lineTerminator = await getLinewiseTerminator(bufferId, start, text);

  return { bufferId, start, end, text, lineTerminator };
}

async function yankLinewise(count: number): Promise<void> {
  const range = await getLinewiseRange(count);
  if (range === null) {
    return;
  }

  editor.setClipboard(ensureLinewiseRegisterText(range.text, range.lineTerminator));
  state.lastYankWasLinewise = true;
}

async function cutLinewise(count: number): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    return;
  }

  const range = await getLinewiseRange(count);
  if (range === null) {
    return;
  }

  editor.setClipboard(ensureLinewiseRegisterText(range.text, range.lineTerminator));
  editor.deleteRange(range.bufferId, range.start, range.end);
  state.lastYankWasLinewise = true;
  editor.setBufferCursor(range.bufferId, Math.min(range.start, editor.getBufferLength(range.bufferId)));
}

async function changeLinewise(count: number): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    return;
  }

  const range = await getLinewiseRange(count);
  if (range === null) {
    return;
  }

  editor.setClipboard(ensureLinewiseRegisterText(range.text, range.lineTerminator));
  editor.deleteRange(range.bufferId, range.start, range.end);
  editor.insertText(range.bufferId, range.start, getLinewiseReplacementText(range.text) ?? range.lineTerminator);
  editor.setBufferCursor(range.bufferId, range.start);
  state.lastYankWasLinewise = true;
}

// Map motion actions to their selection equivalents
const motionToSelection: Record<string, string> = {
  move_left: "select_left",
  move_right: "select_right",
  move_up: "select_up",
  move_down: "select_down",
  move_word_left: "select_word_left",
  move_word_right: "select_word_right",
  vi_move_word_end: "vi_select_word_end",
  move_to_paragraph_up: "select_to_paragraph_up",
  move_to_paragraph_down: "select_to_paragraph_down",
  move_line_start: "select_line_start",
  move_line_end: "select_line_end",
  move_document_start: "select_document_start",
  move_document_end: "select_document_end",
};

function stringIndexToByteOffset(text: string, index: number): number {
  return editor.utf8ByteLength(text.slice(0, index));
}

function byteOffsetToStringIndex(text: string, byteOffset: number): number {
  if (byteOffset <= 0) {
    return 0;
  }

  let index = 0;
  let bytes = 0;
  while (index < text.length && bytes < byteOffset) {
    const codePoint = text.codePointAt(index);
    const char = String.fromCodePoint(codePoint ?? text.charCodeAt(index));
    const nextBytes = bytes + editor.utf8ByteLength(char);
    if (nextBytes > byteOffset) {
      break;
    }
    bytes = nextBytes;
    index += char.length;
  }
  return index;
}

function isWhitespaceChar(char: string | undefined): boolean {
  return char === undefined || /\s/.test(char);
}

function isWordChar(char: string | undefined): boolean {
  return char !== undefined && /[a-zA-Z0-9_]/.test(char);
}

function charAtStringIndex(text: string, index: number): string | undefined {
  if (index < 0 || index >= text.length) {
    return undefined;
  }
  const codePoint = text.codePointAt(index);
  return codePoint === undefined ? undefined : String.fromCodePoint(codePoint);
}

function nextStringIndex(text: string, index: number): number {
  if (index >= text.length) {
    return text.length;
  }
  return Math.min(text.length, index + (charAtStringIndex(text, index)?.length ?? 1));
}

function previousStringIndex(text: string, index: number): number {
  if (index <= 0) {
    return 0;
  }

  let previous = index - 1;
  const codeUnit = text.charCodeAt(previous);
  if (codeUnit >= 0xDC00 && codeUnit <= 0xDFFF && previous > 0) {
    const maybeHighSurrogate = text.charCodeAt(previous - 1);
    if (maybeHighSurrogate >= 0xD800 && maybeHighSurrogate <= 0xDBFF) {
      previous--;
    }
  }
  return previous;
}

function isWhitespaceAt(text: string, index: number): boolean {
  return isWhitespaceChar(charAtStringIndex(text, index));
}

function isLineBreakAt(text: string, index: number): boolean {
  return text[index] === "\n" || text[index] === "\r";
}

function lineStartIndex(text: string, index: number): number {
  let lineStart = Math.min(Math.max(index, 0), text.length);
  while (lineStart > 0) {
    const previous = previousStringIndex(text, lineStart);
    if (text[previous] === "\n" || text[previous] === "\r") {
      break;
    }
    lineStart = previous;
  }
  return lineStart;
}

function isEmptyLineStart(text: string, index: number): boolean {
  if (text[index] === "\n" && index > 0 && text[index - 1] === "\r") {
    return false;
  }
  return index >= 0
    && index < text.length
    && lineStartIndex(text, index) === index
    && (text[index] === "\n" || text[index] === "\r");
}

function hasOnlyWhitespaceAfter(text: string, index: number): boolean {
  let next = nextStringIndex(text, index);
  while (next < text.length) {
    if (!isWhitespaceAt(text, next)) {
      return false;
    }
    next = nextStringIndex(text, next);
  }
  return true;
}

function nextWORDIndex(text: string, startIndex: number): number {
  let index = Math.min(startIndex, text.length);
  const startedOnWhitespace = index < text.length && isWhitespaceAt(text, index);
  let lastNonWhitespace = index;
  let lastWhitespace = index;

  if (index < text.length && !isWhitespaceAt(text, index)) {
    while (index < text.length && !isWhitespaceAt(text, index)) {
      lastNonWhitespace = index;
      index = nextStringIndex(text, index);
    }
  }

  while (index < text.length && isWhitespaceAt(text, index)) {
    if (isEmptyLineStart(text, index)) {
      break;
    }
    if (!isLineBreakAt(text, index)) {
      lastWhitespace = index;
    }
    index = nextStringIndex(text, index);
  }

  if (index >= text.length) {
    return startedOnWhitespace ? lastWhitespace : lastNonWhitespace;
  }

  return index;
}

function previousWORDIndex(text: string, startIndex: number): number {
  if (startIndex <= 0) {
    return 0;
  }

  let index = previousStringIndex(text, Math.min(startIndex, text.length));
  while (index > 0 && isWhitespaceAt(text, index)) {
    if (isEmptyLineStart(text, index)) {
      return index;
    }
    index = previousStringIndex(text, index);
  }

  while (index > 0 && !isWhitespaceAt(text, previousStringIndex(text, index))) {
    index = previousStringIndex(text, index);
  }

  return index;
}

function endWORDIndex(text: string, startIndex: number): number {
  let index = Math.min(startIndex, text.length);
  if (index >= text.length) {
    return text.length;
  }

  if (!isWhitespaceAt(text, index)) {
    const next = nextStringIndex(text, index);
    if (next >= text.length) {
      return index;
    }
    if (isWhitespaceAt(text, next)) {
      index = next;
      while (index < text.length && isWhitespaceAt(text, index)) {
        index = nextStringIndex(text, index);
      }
    }
    while (nextStringIndex(text, index) < text.length && !isWhitespaceAt(text, nextStringIndex(text, index))) {
      index = nextStringIndex(text, index);
    }
    return index;
  }

  while (index < text.length && isWhitespaceAt(text, index)) {
    index = nextStringIndex(text, index);
  }
  while (nextStringIndex(text, index) < text.length && !isWhitespaceAt(text, nextStringIndex(text, index))) {
    index = nextStringIndex(text, index);
  }

  return index;
}

// Word-class helpers for the `cw` special case (lowercase `w`, which — unlike
// the whitespace-delimited WORD motions above — treats a run of word characters
// and a run of punctuation as separate words).

// Return the string index of the last character of the same-class (word or
// punctuation) run that `index` is in. `index` must point at a non-whitespace
// character.
function tokenRunEnd(text: string, index: number): number {
  const startIsWord = isWordChar(charAtStringIndex(text, index));
  while (true) {
    const next = nextStringIndex(text, index);
    if (next >= text.length || isWhitespaceAt(text, next)) {
      break;
    }
    if (isWordChar(charAtStringIndex(text, next)) !== startIsWord) {
      break;
    }
    index = next;
  }
  return index;
}

// Vim `e`-style advance: from `index`, move forward one character, skip any
// whitespace, then return the last character of the next word. Used for the
// trailing words of a `cNw` change. Returns `index` unchanged if there is no
// further word.
function viWordEndAdvance(text: string, index: number): number {
  let next = nextStringIndex(text, index);
  while (next < text.length && isWhitespaceAt(text, next)) {
    next = nextStringIndex(text, next);
  }
  if (next >= text.length) {
    return index;
  }
  return tokenRunEnd(text, next);
}

function computeWORDMotionTargetIndex(text: string, startIndex: number, kind: WORDMotionKind, count: number): number {
  let index = startIndex;
  for (let i = 0; i < Math.max(1, count); i++) {
    if (kind === "forward") {
      index = nextWORDIndex(text, index);
    } else if (kind === "backward") {
      index = previousWORDIndex(text, index);
    } else {
      index = endWORDIndex(text, index);
    }
  }
  return index;
}

async function computeWORDMotionTarget(kind: WORDMotionKind, count: number, origin: number | null = null): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = origin ?? editor.getCursorPosition();
  if (cursorPos === null) {
    return null;
  }

  const bufferLength = editor.getBufferLength(bufferId);
  const text = await editor.getBufferText(bufferId, 0, bufferLength);
  const index = computeWORDMotionTargetIndex(text, byteOffsetToStringIndex(text, cursorPos), kind, count);

  return stringIndexToByteOffset(text, index);
}

function nextLineStartIndex(text: string, index: number): number {
  let cursor = lineStartIndex(text, index);
  while (cursor < text.length) {
    const char = charAtStringIndex(text, cursor);
    cursor = nextStringIndex(text, cursor);
    if (char === "\n") {
      return cursor;
    }
    if (char === "\r") {
      if (text[cursor] === "\n") {
        cursor = nextStringIndex(text, cursor);
      }
      return cursor;
    }
  }
  return text.length;
}

function paragraphDownEofIndex(text: string): number {
  if (text.length === 0) {
    return 0;
  }

  let contentEnd = text.length;
  if (text[contentEnd - 1] === "\n") {
    contentEnd--;
    if (contentEnd > 0 && text[contentEnd - 1] === "\r") {
      contentEnd--;
    }
  } else if (text[contentEnd - 1] === "\r") {
    contentEnd--;
  }

  return contentEnd === 0 ? 0 : previousStringIndex(text, contentEnd);
}

function paragraphDownMotionTargetIndex(text: string, startIndex: number): { index: number; reachedEof: boolean } {
  let lineStart = nextLineStartIndex(text, startIndex);
  while (lineStart < text.length) {
    const nextLineStart = nextLineStartIndex(text, lineStart);
    const lineContent = text.slice(lineStart, nextLineStart);
    if (lineContent.replace(/[\r\n]+$/, "") === "") {
      return { index: lineStart, reachedEof: false };
    }
    lineStart = nextLineStart;
  }

  return { index: paragraphDownEofIndex(text), reachedEof: true };
}

async function computeParagraphDownOperatorRange(count: number): Promise<{ start: number; end: number } | null> {
  const start = editor.getCursorPosition();
  if (start === null) {
    return null;
  }

  const bufferId = editor.getActiveBufferId();
  const bufferText = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  let target = byteOffsetToStringIndex(bufferText, start);
  let reachedEof = false;
  for (let i = 0; i < Math.max(1, count); i++) {
    const next = paragraphDownMotionTargetIndex(bufferText, target);
    target = next.index;
    reachedEof = next.reachedEof;
  }

  const endIndex = reachedEof ? nextStringIndex(bufferText, target) : target;
  return {
    start,
    end: stringIndexToByteOffset(bufferText, endIndex),
  };
}

function byteLengthOfCharAt(text: string, index: number): number {
  if (index < 0 || index >= text.length) {
    return 0;
  }
  const codePoint = text.codePointAt(index);
  return editor.utf8ByteLength(String.fromCodePoint(codePoint ?? text.charCodeAt(index)));
}

async function applyOperatorWithRange(operator: string, start: number, end: number): Promise<void> {
  const rangeStart = Math.min(start, end);
  const rangeEnd = Math.max(start, end);
  if (rangeEnd <= rangeStart) {
    switchMode("normal");
    return;
  }

  const bufferId = editor.getActiveBufferId();

  // Indent/dedent are line-wise: resolve the byte range to the whole lines it
  // touches and shift them, rather than operating on the exact byte span.
  if (operator === ">" || operator === "<") {
    const span = await lineSpanOfRange(bufferId, rangeStart, rangeEnd);
    await applyIndentToLineRange(operator, span.firstLineStart, span.lineCount);
    return;
  }

  if ((operator === "d" || operator === "c") && isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }

  const text = await editor.getBufferText(bufferId, rangeStart, rangeEnd);
  if (text) {
    editor.setClipboard(text);
  }

  switch (operator) {
    case "d":
      editor.deleteRange(bufferId, rangeStart, rangeEnd);
      editor.setBufferCursor(bufferId, Math.min(rangeStart, editor.getBufferLength(bufferId)));
      state.lastYankWasLinewise = false;
      break;
    case "c":
      editor.deleteRange(bufferId, rangeStart, rangeEnd);
      editor.setBufferCursor(bufferId, Math.min(rangeStart, editor.getBufferLength(bufferId)));
      state.lastYankWasLinewise = false;
      switchMode("insert");
      return;
    case "y":
      editor.setBufferCursor(bufferId, rangeStart);
      state.lastYankWasLinewise = false;
      break;
  }

  switchMode("normal");
}

// ============================================================================
// Indent / dedent operators ( >>, <<, >motion/<motion, and visual >/< )
// ============================================================================
//
// Indentation is inherently line-wise, so each entry point resolves a byte
// range to the whole lines it touches and then reuses the editor's own
// `insert_tab` / `dedent_selection` actions. Routing through the built-in
// actions keeps tab width and tabs-vs-spaces a single source of truth
// (per-language config, `use_tabs`, …) instead of the plugin re-deriving them.

// Count the line terminators contained in `text` (handles LF, CR and CRLF).
function countLineTerminators(text: string): number {
  let count = 0;
  let searchFrom = 0;
  while (true) {
    const end = nextLineTerminatorEnd(text, searchFrom);
    if (end === null) {
      break;
    }
    count++;
    searchFrom = end;
  }
  return count;
}

// Resolve a byte range to the first line it starts on and the number of whole
// lines it spans. An exclusive end sitting exactly on a line start does not
// pull in the following line.
async function lineSpanOfRange(
  bufferId: number,
  startByte: number,
  endByte: number,
): Promise<{ firstLineStart: number; lineCount: number }> {
  const lo = Math.min(startByte, endByte);
  const hi = Math.max(startByte, endByte);
  const firstLineStart = await findLineStartAtPosition(bufferId, lo);
  const lastTouched = hi > lo ? hi - 1 : lo;
  const lastLineStart = await findLineStartAtPosition(bufferId, lastTouched);
  const between = await editor.getBufferText(bufferId, firstLineStart, lastLineStart);
  return { firstLineStart, lineCount: 1 + countLineTerminators(between) };
}

// Move the cursor to the first non-blank character of the line starting at
// `lineStart` (Vim leaves the cursor there after >>/<<).
async function placeCursorAtFirstNonBlank(bufferId: number, lineStart: number): Promise<void> {
  const bufferLength = editor.getBufferLength(bufferId);
  const sampleEnd = Math.min(bufferLength, lineStart + 4096);
  const sample = await editor.getBufferText(bufferId, lineStart, sampleEnd);
  let index = 0;
  while (index < sample.length && (sample[index] === " " || sample[index] === "\t")) {
    index++;
  }
  const offset = lineStart + editor.utf8ByteLength(sample.slice(0, index));
  editor.setBufferCursor(bufferId, Math.min(offset, bufferLength));
}

// Indent (">") or dedent ("<") `lineCount` whole lines starting at
// `firstLineStart`, leave the cursor on the first non-blank of the first line,
// and return to normal mode.
async function applyIndentToLineRange(
  operator: string,
  firstLineStart: number,
  lineCount: number,
): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }

  // Build a whole-line selection [firstLineStart .. end of last line] so the
  // editor's selection-aware indent/dedent acts on every line in the range.
  editor.setBufferCursor(bufferId, firstLineStart);
  editor.executeAction("move_line_start");
  for (let i = 1; i < Math.max(1, lineCount); i++) {
    editor.executeAction("select_down");
  }
  editor.executeAction("select_line_end");

  editor.executeAction(operator === ">" ? "insert_tab" : "dedent_selection");

  state.lastYankWasLinewise = false;
  await placeCursorAtFirstNonBlank(bufferId, firstLineStart);
  switchMode("normal");
}

// >>/<<: indent or dedent `count` lines starting at the cursor's line.
async function applyLineOpIndent(operator: string, count: number): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }
  const position = editor.getCursorPosition();
  if (position === null) {
    switchMode("normal");
    return;
  }
  const firstLineStart = await findLineStartAtPosition(bufferId, position);
  await applyIndentToLineRange(operator, firstLineStart, Math.max(1, count));
}

// >motion / <motion: indent the whole lines the motion spans. Built entirely
// from ordered editor actions (select-by-motion, then indent the selection) so
// it never relies on reading a position back mid-handler — the plugin's cursor
// snapshot is not refreshed until the handler yields. Extending the active end
// to its line end makes a forward/downward motion include the destination line
// even when it stops at column 0, matching Vim's line-wise `>`.
async function applyIndentViaMotion(
  operator: string,
  selectAction: string,
  motionAction: string,
  count: number,
): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }
  const startPos = editor.getCursorPosition();
  state.lastChange = { type: "operator-motion", operator, motion: motionAction, count };

  for (let i = 0; i < Math.max(1, count); i++) {
    editor.executeAction(selectAction);
  }
  editor.executeAction("select_line_end");
  editor.executeAction(operator === ">" ? "insert_tab" : "dedent_selection");
  state.lastYankWasLinewise = false;

  // Leave the cursor on the first non-blank of the line the motion started on
  // (its byte offset is unchanged by indenting at line starts).
  if (startPos !== null) {
    const firstLineStart = await findLineStartAtPosition(bufferId, startPos);
    await placeCursorAtFirstNonBlank(bufferId, firstLineStart);
  } else {
    switchMode("normal");
    return;
  }
  switchMode("normal");
}

async function computeWORDOperatorRange(
  kind: WORDMotionKind,
  count: number,
  useChangeForwardSemantics: boolean,
): Promise<{ start: number; end: number; cursorAfter?: number } | null> {
  const start = editor.getCursorPosition();
  if (start === null) {
    return null;
  }

  const bufferId = editor.getActiveBufferId();
  const bufferText = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const startIndex = byteOffsetToStringIndex(bufferText, start);

  if (useChangeForwardSemantics && kind === "forward" && !isWhitespaceAt(bufferText, startIndex)) {
    let endIndex = startIndex;
    for (let i = 0; i < Math.max(1, count); i++) {
      endIndex = endWORDIndex(bufferText, endIndex);
    }
    const endTarget = stringIndexToByteOffset(bufferText, endIndex);
    return {
      start,
      end: endTarget + byteLengthOfCharAt(bufferText, endIndex),
    };
  }

  const targetIndex = computeWORDMotionTargetIndex(bufferText, startIndex, kind, count);
  const target = stringIndexToByteOffset(bufferText, targetIndex);
  let end = target;
  let cursorAfter: number | undefined;
  if (kind === "end") {
    end += byteLengthOfCharAt(bufferText, targetIndex);
  } else if (kind === "forward") {
    if (target >= start && !isWhitespaceAt(bufferText, targetIndex) && hasOnlyWhitespaceAfter(bufferText, targetIndex)) {
      end += byteLengthOfCharAt(bufferText, targetIndex);
    } else if (target >= start && isWhitespaceAt(bufferText, targetIndex) && hasOnlyWhitespaceAfter(bufferText, targetIndex)) {
      end += byteLengthOfCharAt(bufferText, targetIndex);
      if (!useChangeForwardSemantics && startIndex > 0) {
        cursorAfter = stringIndexToByteOffset(bufferText, previousStringIndex(bufferText, startIndex));
      }
    }
  }

  return { start, end, cursorAfter };
}

// Compute the change range for `cw` / `cNw`. Vim treats `cw` like `ce` when the
// cursor is on a non-blank: it changes only up to the end of the word and does
// NOT consume the trailing whitespace (`:help cw`). Returns null when the cursor
// is on a blank (or past EOF), in which case the caller falls back to plain `w`
// (i.e. `dw`-style) semantics.
async function computeWordChangeRange(count: number): Promise<{ start: number; end: number } | null> {
  const start = editor.getCursorPosition();
  if (start === null) {
    return null;
  }

  const bufferId = editor.getActiveBufferId();
  const bufferText = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const startIndex = byteOffsetToStringIndex(bufferText, start);
  if (startIndex >= bufferText.length || isWhitespaceAt(bufferText, startIndex)) {
    return null;
  }

  // First word: stop at the end of the current word (no forward advance, so the
  // cursor sitting on a word's last character changes only that character).
  // Each additional count behaves like Vim's `e` motion.
  let endIndex = tokenRunEnd(bufferText, startIndex);
  for (let i = 1; i < Math.max(1, count); i++) {
    endIndex = viWordEndAdvance(bufferText, endIndex);
  }

  const endTarget = stringIndexToByteOffset(bufferText, endIndex);
  return { start, end: endTarget + byteLengthOfCharAt(bufferText, endIndex) };
}

async function applyWORDOperatorMotion(
  operator: string,
  kind: WORDMotionKind,
  count: number,
  useChangeForwardSemantics: boolean = operator === "c",
): Promise<void> {
  const range = await computeWORDOperatorRange(kind, count, useChangeForwardSemantics);
  if (range === null) {
    switchMode("normal");
    return;
  }

  await applyOperatorWithRange(operator, range.start, range.end);
  if (operator === "d" && range.cursorAfter !== undefined) {
    editor.setBufferCursor(editor.getActiveBufferId(), range.cursorAfter);
  }
}

function WORDMotionKindFromRepeatMotion(motion: string): WORDMotionKind | null {
  switch (motion) {
    case "vi_WORD_forward":
      return "forward";
    case "vi_WORD_backward":
      return "backward";
    case "vi_WORD_end":
      return "end";
    default:
      return null;
  }
}

async function handleWORDMotionWithOperator(kind: WORDMotionKind): Promise<void> {
  if (!state.pendingOperator) {
    switchMode("normal");
    return;
  }

  const operator = state.pendingOperator;
  const count = consumeCount();
  if (operator === "d" || operator === "c" || operator === ">" || operator === "<") {
    state.lastChange = { type: "operator-motion", operator, motion: `vi_WORD_${kind}`, count };
  }

  await applyWORDOperatorMotion(operator, kind, count);
}

async function selectToPosition(target: number, includeTarget: boolean = false): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const start = editor.getCursorPosition();
  if (start === null) {
    return;
  }

  if (start === target && !includeTarget) {
    return;
  }

  const bufferText = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  let destination = target;
  if (includeTarget) {
    const targetIndex = byteOffsetToStringIndex(bufferText, target);
    destination += byteLengthOfCharAt(bufferText, targetIndex);
  }

  const startIndex = byteOffsetToStringIndex(bufferText, start);
  const destinationIndex = byteOffsetToStringIndex(bufferText, destination);
  const action = startIndex < destinationIndex ? "select_right" : "select_left";
  let steps = Math.abs(destinationIndex - startIndex);
  while (steps > 0) {
    editor.executeAction(action);
    steps--;
  }
}

// Apply an operator by turning the motion into a selection, then applying the
// operator to that selected range.
// The count parameter specifies how many times to apply the motion (e.g., d3w = delete 3 words)
async function applyOperatorWithMotion(operator: string, motionAction: string, count: number = 1): Promise<void> {
  // Record last change for '.' repeat (only for delete and change, not yank)
  if (operator === "d" || operator === "c") {
    state.lastChange = { type: "operator-motion", operator, motion: motionAction, count };
  }

  const selectAction = motionToSelection[motionAction];
  if (!selectAction) {
    editor.debug(`No selection equivalent for motion: ${motionAction}`);
    switchMode("normal");
    return;
  }

  if (operator === ">" || operator === "<") {
    await applyIndentViaMotion(operator, selectAction, motionAction, count);
    return;
  }

  switch (operator) {
    case "d": // delete
      await selectThenCutCharacterwise(selectAction, count);
      break;
    case "c": // change (delete and enter insert mode)
      if (await selectThenCutCharacterwise(selectAction, count)) {
        switchMode("insert");
        return; // Don't switch back to normal mode
      }
      break;
    case "y": // yank
      if (await selectThenCopyCharacterwise(selectAction, count)) {
        // Move cursor back to start of selection (left side)
        editor.executeAction("move_left");
      }
      break;
  }

  switchMode("normal");
}

// Handle motion in operator-pending mode
// Consumes any pending count and applies it to the motion
async function handleMotionWithOperator(motionAction: string): Promise<void> {
  if (!state.pendingOperator) {
    switchMode("normal");
    return;
  }

  const count = consumeCount();
  await applyOperatorWithMotion(state.pendingOperator, motionAction, count);
}

// ============================================================================
// Normal Mode Commands
// ============================================================================

// Navigation (all support count prefix, e.g., 5j moves down 5 lines)
function vi_left() : void {
  // h — line-bounded move left (vim doesn't wrap across lines)
  executeWithCount("move_left_in_line");
}
registerHandler("vi_left", vi_left);

function vi_down() : void {
  // vi_move_down clamps the caret to the destination line's last char (Vim
  // never lets the cursor rest past it in NORMAL mode), while still
  // remembering the goal column for the next vertical move.
  executeWithCount("vi_move_down");
}
registerHandler("vi_down", vi_down);

function vi_up() : void {
  executeWithCount("vi_move_up");
}
registerHandler("vi_up", vi_up);

function vi_right() : void {
  // l — line-bounded move right (vim doesn't wrap across lines)
  executeWithCount("move_right_in_line");
}
registerHandler("vi_right", vi_right);

function vi_word() : void {
  executeWithCount("move_word_right");
}
registerHandler("vi_word", vi_word);

function vi_word_back() : void {
  executeWithCount("move_word_left");
}
registerHandler("vi_word_back", vi_word_back);

function vi_word_end() : void {
  // Vim 'e' motion — uses native vi_move_word_end action
  executeWithCount("vi_move_word_end");
}
registerHandler("vi_word_end", vi_word_end);

async function vi_WORD() : Promise<void> {
  const target = await computeWORDMotionTarget("forward", consumeCount());
  if (target !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), target);
  }
}
registerHandler("vi_WORD", vi_WORD);

async function vi_WORD_back() : Promise<void> {
  const target = await computeWORDMotionTarget("backward", consumeCount());
  if (target !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), target);
  }
}
registerHandler("vi_WORD_back", vi_WORD_back);

async function vi_WORD_end() : Promise<void> {
  const target = await computeWORDMotionTarget("end", consumeCount());
  if (target !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), target);
  }
}
registerHandler("vi_WORD_end", vi_WORD_end);

function vi_line_start() : void {
  consumeCount(); // Count doesn't apply to line start
  editor.executeAction("move_line_start");
}
registerHandler("vi_line_start", vi_line_start);

function vi_line_end() : void {
  consumeCount(); // Count doesn't apply to line end
  editor.executeAction("move_line_end");
  // In vim normal mode, cursor should be ON the last char, not past it
  // move_line_end goes past the last char; move_left corrects this
  editor.executeAction("move_left");
}
registerHandler("vi_line_end", vi_line_end);

async function vi_first_non_blank() : Promise<void> {
  consumeCount(); // Count doesn't apply
  // Get line start position directly (avoids stale snapshot from executeAction)
  const line = editor.getPrimaryCursor()?.line ?? 0;
  const bufferId = editor.getActiveBufferId();
  const lineStart = await editor.getLineStartPosition(line);
  if (lineStart === null) {
    editor.executeAction("move_line_start");
    return;
  }
  const text = await safeGetBufferText(bufferId, lineStart, lineStart + 200);
  if (text) {
    let offset = 0;
    while (offset < text.length && (text[offset] === ' ' || text[offset] === '\t')) {
      offset++;
    }
    if (offset < text.length && text[offset] !== '\n' && text[offset] !== '\r') {
      editor.setBufferCursor(bufferId, lineStart + offset);
    } else {
      editor.setBufferCursor(bufferId, lineStart);
    }
  } else {
    editor.executeAction("move_line_start");
  }
}
registerHandler("vi_first_non_blank", vi_first_non_blank);

function vi_doc_start() : void {
  consumeCount(); // Count doesn't apply
  editor.executeAction("move_document_start");
}
registerHandler("vi_doc_start", vi_doc_start);

function vi_doc_end() : void {
  const count = state.count;
  consumeCount();
  if (count !== null) {
    // nG = go to line n (1-indexed; goto_line expects 0-indexed internally)
    // Use setBufferCursor to move to line start via getLineStartPosition
    const line = count - 1; // Convert to 0-indexed
    editor.getLineStartPosition(line).then((pos) => {
      if (pos !== null) {
        editor.setBufferCursor(editor.getActiveBufferId(), pos);
      }
    });
  } else {
    editor.executeAction("move_document_end");
  }
  // Update status to clear any count display
  editor.setStatus(getModeIndicator(state.mode));
}
registerHandler("vi_doc_end", vi_doc_end);

function vi_page_down() : void {
  executeWithCount("page_down");
}
registerHandler("vi_page_down", vi_page_down);

function vi_page_up() : void {
  executeWithCount("page_up");
}
registerHandler("vi_page_up", vi_page_up);

function vi_matching_bracket() : void {
  editor.executeAction("goto_matching_bracket");
}
registerHandler("vi_matching_bracket", vi_matching_bracket);

function vi_paragraph_up() : void {
  executeWithCount("move_to_paragraph_up");
}
registerHandler("vi_paragraph_up", vi_paragraph_up);

function realLineStarts(text: string): number[] {
  const starts = [0];
  for (let index = 0; index < text.length; index = nextStringIndex(text, index)) {
    const char = text[index];
    if (char === "\n" && index + 1 < text.length) {
      starts.push(index + 1);
    } else if (char === "\r" && text[index + 1] !== "\n" && index + 1 < text.length) {
      starts.push(index + 1);
    }
  }
  return starts;
}

function lineContentEndIndex(text: string, lineStarts: number[], lineIndex: number): number {
  const nextLineStart = lineStarts[lineIndex + 1];
  let end = nextLineStart === undefined ? text.length : nextLineStart - 1;
  if (end > lineStarts[lineIndex] && text[end - 1] === "\n") {
    end--;
  }
  if (end > lineStarts[lineIndex] && text[end - 1] === "\r") {
    end--;
  }
  return end;
}

function lineIndexForStringIndex(lineStarts: number[], index: number): number {
  let lineIndex = 0;
  while (lineIndex + 1 < lineStarts.length && lineStarts[lineIndex + 1] <= index) {
    lineIndex++;
  }
  return lineIndex;
}

async function vi_paragraph_down() : Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return;
  }

  const text = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const lineStarts = realLineStarts(text);
  if (lineStarts.length === 0) {
    return;
  }

  let lineIndex = lineIndexForStringIndex(lineStarts, byteOffsetToStringIndex(text, cursorPos));
  let count = consumeCount();
  while (count-- > 0) {
    let didSkip = false;
    for (let first = true; ; first = false) {
      if (lineContentEndIndex(text, lineStarts, lineIndex) > lineStarts[lineIndex]) {
        didSkip = true;
      }

      if (!first && didSkip && lineContentEndIndex(text, lineStarts, lineIndex) === lineStarts[lineIndex]) {
        break;
      }

      lineIndex++;
      if (lineIndex >= lineStarts.length) {
        lineIndex = lineStarts.length - 1;
        break;
      }
    }
  }

  if (lineIndex === lineStarts.length - 1) {
    const lineEnd = lineContentEndIndex(text, lineStarts, lineIndex);
    if (lineEnd > lineStarts[lineIndex]) {
      editor.setBufferCursor(bufferId, stringIndexToByteOffset(text, previousStringIndex(text, lineEnd)));
      return;
    }
  }

  editor.setBufferCursor(bufferId, stringIndexToByteOffset(text, lineStarts[lineIndex]));
}
registerHandler("vi_paragraph_down", vi_paragraph_down);

function findLineEndIndex(text: string, index: number): number {
  let end = Math.min(Math.max(index, 0), text.length);
  while (end < text.length && text[end] !== "\n" && text[end] !== "\r") {
    end = nextStringIndex(text, end);
  }
  return end;
}

function findSearchTargetUnderCursor(text: string, cursorIndex: number, scanForwardOnMiss: boolean = false): WordSearchTarget | null {
  const originalIndex = Math.min(cursorIndex, Math.max(0, text.length - 1));
  const lineEnd = findLineEndIndex(text, originalIndex);
  let index = originalIndex;

  if (!scanForwardOnMiss && !isWordChar(text[index]) && index > 0 && isWordChar(text[index - 1])) {
    index--;
  }
  if (scanForwardOnMiss && !isWordChar(text[index])) {
    while (index < lineEnd && !isWordChar(text[index])) {
      index = nextStringIndex(text, index);
    }
  }
  if (isWordChar(text[index])) {
    let start = index;
    let end = index + 1;
    while (start > 0 && isWordChar(text[start - 1])) {
      start--;
    }
    while (end < text.length && isWordChar(text[end])) {
      end++;
    }

    return { start, end, text: text.slice(start, end), wholeWord: true };
  }

  index = originalIndex;
  while (index < lineEnd && isWhitespaceAt(text, index)) {
    index = nextStringIndex(text, index);
  }
  if (index >= lineEnd) {
    return null;
  }

  let start = index;
  let end = index;
  while (end < lineEnd && !isWhitespaceAt(text, end)) {
    end = nextStringIndex(text, end);
  }

  return { start, end, text: text.slice(start, end), wholeWord: false };
}

function isWholeWordMatch(text: string, start: number, end: number): boolean {
  return !isWordChar(text[start - 1]) && !isWordChar(text[end]);
}

function isSearchMatch(text: string, start: number, end: number, wholeWord: boolean): boolean {
  return !wholeWord || isWholeWordMatch(text, start, end);
}

function findNextSearchMatch(text: string, target: string, wholeWord: boolean, from: number, until: number): number | null {
  let index = text.indexOf(target, from);
  while (index !== -1 && index < until) {
    const end = index + target.length;
    if (isSearchMatch(text, index, end, wholeWord)) {
      return index;
    }
    index = text.indexOf(target, index + 1);
  }
  return null;
}

function findPreviousSearchMatch(text: string, target: string, wholeWord: boolean, before: number, min: number): number | null {
  if (before <= min) {
    return null;
  }

  let index = text.lastIndexOf(target, before - 1);
  while (index !== -1 && index >= min) {
    const end = index + target.length;
    if (isSearchMatch(text, index, end, wholeWord)) {
      return index;
    }
    if (index === 0) {
      return null;
    }
    index = text.lastIndexOf(target, index - 1);
  }
  return null;
}

async function executeStoredWordSearch(
  target: string,
  direction: WordSearchDirection,
  count: number,
  wholeWord: boolean,
  currentTarget?: WordSearchTarget,
): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return;
  }

  const text = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const cursorIndex = byteOffsetToStringIndex(text, cursorPos);
  const targetAtCursor = currentTarget ?? findSearchTargetUnderCursor(text, cursorIndex);
  const onSameTarget = targetAtCursor?.text === target && targetAtCursor.wholeWord === wholeWord;
  const searchStart = onSameTarget && targetAtCursor !== null ? targetAtCursor.start : cursorIndex;
  const searchEnd = onSameTarget && targetAtCursor !== null ? targetAtCursor.end : nextStringIndex(text, cursorIndex);

  let match: number | null = null;
  let fromStart = searchStart;
  let fromEnd = searchEnd;
  for (let i = 0; i < count; i++) {
    if (direction === "forward") {
      match = findNextSearchMatch(text, target, wholeWord, fromEnd, text.length)
        ?? findNextSearchMatch(text, target, wholeWord, 0, fromStart);
    } else {
      match = findPreviousSearchMatch(text, target, wholeWord, fromStart, 0)
        ?? findPreviousSearchMatch(text, target, wholeWord, text.length, fromEnd);
    }

    if (match === null) {
      return;
    }

    fromStart = match;
    fromEnd = match + target.length;
  }

  if (match !== null) {
    editor.setBufferCursor(bufferId, stringIndexToByteOffset(text, match));
  }
}

async function executeWordUnderCursorSearch(direction: WordSearchDirection, count: number): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return;
  }

  const text = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const cursorIndex = byteOffsetToStringIndex(text, cursorPos);
  const currentTarget = findSearchTargetUnderCursor(text, cursorIndex, true);
  if (currentTarget === null) {
    return;
  }

  state.lastWordSearch = { text: currentTarget.text, direction, wholeWord: currentTarget.wholeWord };
  editor.setBufferCursor(bufferId, stringIndexToByteOffset(text, currentTarget.start));
  await executeStoredWordSearch(currentTarget.text, direction, count, currentTarget.wholeWord, currentTarget);
}

async function vi_search_word_forward() : Promise<void> {
  await executeWordUnderCursorSearch("forward", consumeCount());
}
registerHandler("vi_search_word_forward", vi_search_word_forward);

async function vi_search_word_backward() : Promise<void> {
  await executeWordUnderCursorSearch("backward", consumeCount());
}
registerHandler("vi_search_word_backward", vi_search_word_backward);

// Mode switching
function vi_insert_before() : void {
  switchMode("insert");
}
registerHandler("vi_insert_before", vi_insert_before);

function vi_insert_after() : void {
  editor.executeAction("move_right");
  switchMode("insert");
}
registerHandler("vi_insert_after", vi_insert_after);

function vi_insert_line_start() : void {
  editor.executeAction("move_line_start");
  switchMode("insert");
}
registerHandler("vi_insert_line_start", vi_insert_line_start);

function vi_insert_line_end() : void {
  editor.executeAction("move_line_end");
  switchMode("insert");
}
registerHandler("vi_insert_line_end", vi_insert_line_end);

function vi_open_below() : void {
  editor.executeAction("move_line_end");
  editor.executeAction("insert_newline");
  switchMode("insert");
}
registerHandler("vi_open_below", vi_open_below);

function vi_open_above() : void {
  editor.executeAction("move_line_start");
  editor.executeAction("insert_newline");
  editor.executeAction("move_up");
  switchMode("insert");
}
registerHandler("vi_open_above", vi_open_above);

function vi_escape() : void {
  // When leaving insert mode, vi_mode should move the cursor one
  // column left (clamped to the line start), since the insert-mode
  // cursor sits one position right of normal. This aligns with the
  // actual vi/vim behavior. Guard on the current mode so a
  // normal-mode Escape (cancel count/operator) does not move the
  // cursor.
  const leavingInsert = state.mode === "insert";
  switchMode("normal");
  if (leavingInsert) {
    editor.executeAction("move_left_in_line");
  }
}
registerHandler("vi_escape", vi_escape);

// Operators
function vi_delete_operator() : void {
  state.pendingOperator = "d";
  switchMode("operator-pending");
}
registerHandler("vi_delete_operator", vi_delete_operator);

function vi_change_operator() : void {
  state.pendingOperator = "c";
  switchMode("operator-pending");
}
registerHandler("vi_change_operator", vi_change_operator);

function vi_yank_operator() : void {
  state.pendingOperator = "y";
  switchMode("operator-pending");
}
registerHandler("vi_yank_operator", vi_yank_operator);

// Line operations (dd, cc, yy) - support count prefix (3dd = delete 3 lines)
async function vi_delete_line() : Promise<void> {
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "delete_line", count };
  await cutLinewise(count);
  switchMode("normal");
}
registerHandler("vi_delete_line", vi_delete_line);

async function vi_change_line() : Promise<void> {
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "change_line", count };
  await changeLinewise(count);
  switchMode("insert");
}
registerHandler("vi_change_line", vi_change_line);

async function vi_yank_line() : Promise<void> {
  const count = consumeCount();
  await yankLinewise(count);
  editor.setStatus(editor.t("status.yanked_lines", { count: String(count) }));
  switchMode("normal");
}
registerHandler("vi_yank_line", vi_yank_line);

// `>` / `<` operators: enter operator-pending so a motion or a doubled
// operator (>>/<<) can follow, mirroring d/c/y.
function vi_indent_operator() : void {
  state.pendingOperator = ">";
  switchMode("operator-pending");
}
registerHandler("vi_indent_operator", vi_indent_operator);

function vi_dedent_operator() : void {
  state.pendingOperator = "<";
  switchMode("operator-pending");
}
registerHandler("vi_dedent_operator", vi_dedent_operator);

// Doubled operators >> and <<. Only fire when the matching operator is
// pending, so invalid combos like `d>` cancel instead of indenting.
async function vi_indent_line() : Promise<void> {
  if (state.pendingOperator !== ">") {
    switchMode("normal");
    return;
  }
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "indent_line", count };
  await applyLineOpIndent(">", count);
}
registerHandler("vi_indent_line", vi_indent_line);

async function vi_dedent_line() : Promise<void> {
  if (state.pendingOperator !== "<") {
    switchMode("normal");
    return;
  }
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "dedent_line", count };
  await applyLineOpIndent("<", count);
}
registerHandler("vi_dedent_line", vi_dedent_line);

// Single character operations - support count prefix (3x = delete 3 chars)
async function vi_delete_char() : Promise<void> {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "delete_forward", count };
  await selectThenCutCharacterwise("select_right", count);
}
registerHandler("vi_delete_char", vi_delete_char);

async function vi_delete_char_before() : Promise<void> {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "delete_backward", count };
  await selectThenCutCharacterwise("select_left", count);
}
registerHandler("vi_delete_char_before", vi_delete_char_before);

// Replace-char (`r<char>`): wait for one keypress and replace the
// character(s) under the cursor with it.  Uses `editor.getNextKey()`
// (plugin API #1) — same pattern as find-char above.
async function vi_replace_char(): Promise<void> {
  state.mode = "find-char"; // reuse find-char state slot for status
  editor.setEditorMode("vi-replace-char");
  editor.setStatus("-- REPLACE CHAR --");

  editor.beginKeyCapture();
  let ev;
  try {
    ev = await editor.getNextKey();
  } finally {
    editor.endKeyCapture();
  }

  // Escape / non-character keys cancel the replacement.
  if (ev.key.length !== 1) {
    switchMode("normal");
    return;
  }

  const count = consumeCount();
  for (let i = 0; i < count; i++) {
    editor.executeAction("delete_forward");
    editor.insertAtCursor(ev.key);
  }
  // Move cursor back to stay on the replaced char (vim behavior).
  editor.executeAction("move_left");
  switchMode("normal");
}
registerHandler("vi_replace_char", vi_replace_char);

// Substitute (delete char and enter insert mode)
async function vi_substitute() : Promise<void> {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "substitute", count };
  if (await selectThenCutCharacterwise("select_right", count)) {
    switchMode("insert");
  }
}
registerHandler("vi_substitute", vi_substitute);

// Delete to end of line (D)
async function vi_delete_to_end() : Promise<void> {
  state.lastChange = { type: "operator-motion", operator: "d", motion: "move_line_end" };
  await selectThenCutCharacterwise("select_line_end", 1);
}
registerHandler("vi_delete_to_end", vi_delete_to_end);

// Change to end of line (C)
async function vi_change_to_end() : Promise<void> {
  state.lastChange = { type: "operator-motion", operator: "c", motion: "move_line_end" };
  await selectThenCutCharacterwise("select_line_end", 1);
  switchMode("insert");
}
registerHandler("vi_change_to_end", vi_change_to_end);

// Clipboard
function vi_paste_after() : void {
  if (state.lastYankWasLinewise) {
    // Line-wise paste: go to next line start and paste there
    // The yanked text includes trailing \n which pushes subsequent lines down
    editor.executeAction("move_down");
    editor.executeAction("move_line_start");
    editor.executeAction("paste");
    editor.executeAction("move_up"); // Stay on the pasted line
    editor.executeAction("move_line_start");
  } else {
    // Character-wise paste: insert after cursor
    editor.executeAction("move_right");
    editor.executeAction("paste");
  }
}
registerHandler("vi_paste_after", vi_paste_after);

function vi_paste_before() : void {
  if (state.lastYankWasLinewise) {
    // Line-wise paste: paste at current line start
    // The yanked text includes trailing \n which pushes current line down
    editor.executeAction("move_line_start");
    editor.executeAction("paste");
    editor.executeAction("move_up"); // Stay on the pasted line
    editor.executeAction("move_line_start");
  } else {
    // Character-wise paste: insert at cursor
    editor.executeAction("paste");
  }
}
registerHandler("vi_paste_before", vi_paste_before);

// Undo/Redo
function vi_undo() : void {
  editor.executeAction("undo");
}
registerHandler("vi_undo", vi_undo);

function vi_redo() : void {
  editor.executeAction("redo");
}
registerHandler("vi_redo", vi_redo);

// Repeat last change (. command)
async function vi_repeat() : Promise<void> {
  if (!state.lastChange) {
    editor.setStatus(editor.t("status.no_change_to_repeat"));
    return;
  }

  const change = state.lastChange;
  const count = consumeCountOrDefault(change.count ?? 1);

  switch (change.type) {
    case "simple": {
      // Simple actions like x, X, s
      if (change.action === "substitute") {
        // Substitute: delete chars and insert text
        if ((await selectThenCutCharacterwise("select_right", count)) && change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      } else if (change.action) {
        // Simple action like delete_forward, delete_backward
        if (change.action === "delete_forward") {
          await selectThenCutCharacterwise("select_right", count);
        } else if (change.action === "delete_backward") {
          await selectThenCutCharacterwise("select_left", count);
        } else {
          executeWithCount(change.action, count);
        }
      }
      break;
    }

    case "line-op": {
      // Line operations like dd, cc
      if (change.action === "delete_line") {
        await cutLinewise(count);
      } else if (change.action === "change_line") {
        await changeLinewise(count);
        if (change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      } else if (change.action === "indent_line") {
        await applyLineOpIndent(">", count);
      } else if (change.action === "dedent_line") {
        await applyLineOpIndent("<", count);
      }
      break;
    }

    case "operator-motion": {
      // Operator + motion like dw, cw, d$
      if (change.operator && change.motion) {
        if (change.motion === "vi_word_change") {
          // `cw`/`cNw` special case — recompute the change range at the current
          // cursor position (mirrors the WORD `cW` repeat path).
          const range = await computeWordChangeRange(count);
          if (range !== null) {
            await applyOperatorWithRange("d", range.start, range.end);
          } else {
            await applyOperatorWithMotion("d", "move_word_right", count);
          }
          if (change.insertedText) {
            editor.insertAtCursor(change.insertedText);
          }
          break;
        }
        const WORDKind = WORDMotionKindFromRepeatMotion(change.motion);
        if (change.operator === "c") {
          if (WORDKind) {
            await applyWORDOperatorMotion("d", WORDKind, count, true);
          } else {
            // For change: do the delete part, then insert the text
            await applyOperatorWithMotion("d", change.motion, count);
          }
          if (change.insertedText) {
            editor.insertAtCursor(change.insertedText);
          }
        } else if (WORDKind) {
          await applyWORDOperatorMotion(change.operator, WORDKind, count);
        } else {
          await applyOperatorWithMotion(change.operator, change.motion, count);
        }
      }
      break;
    }

    case "operator-textobj": {
      // Operator + text object like diw, ci"
      if (change.operator && change.textObject) {
        // Set up the pending state and call applyTextObject
        state.pendingOperator = change.operator === "c" ? "d" : change.operator;
        state.pendingTextObject = change.textObject.modifier;
        await applyTextObject(change.textObject.object);
        if (change.operator === "c" && change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      }
      break;
    }

    case "insert": {
      // Pure insert (i, a, o, O)
      if (change.insertedText) {
        editor.insertAtCursor(change.insertedText);
      }
      break;
    }
  }
}
registerHandler("vi_repeat", vi_repeat);

// Join lines — delete newline at end of current line and insert a space
function vi_join() : void {
  editor.executeAction("move_line_end");
  // Delete the newline character
  editor.executeAction("delete_forward");
  // Insert a space between the joined content
  editor.insertAtCursor(" ");
}
registerHandler("vi_join", vi_join);

// Toggle case (~) — uses native toggle_case action
function vi_toggle_case() : void {
  executeWithCount("toggle_case");
}
registerHandler("vi_toggle_case", vi_toggle_case);

// Search
function vi_search_forward() : void {
  state.lastWordSearch = null;
  editor.executeAction("search");
}
registerHandler("vi_search_forward", vi_search_forward);

function vi_search_backward() : void {
  state.lastWordSearch = null;
  // Use same search dialog, user can search backward manually
  editor.executeAction("search");
}
registerHandler("vi_search_backward", vi_search_backward);

async function vi_find_next() : Promise<void> {
  if (state.lastWordSearch) {
    await executeStoredWordSearch(
      state.lastWordSearch.text,
      state.lastWordSearch.direction,
      consumeCount(),
      state.lastWordSearch.wholeWord,
    );
    return;
  }
  editor.executeAction("find_next");
}
registerHandler("vi_find_next", vi_find_next);

async function vi_find_prev() : Promise<void> {
  if (state.lastWordSearch) {
    const direction = state.lastWordSearch.direction === "forward" ? "backward" : "forward";
    await executeStoredWordSearch(
      state.lastWordSearch.text,
      direction,
      consumeCount(),
      state.lastWordSearch.wholeWord,
    );
    return;
  }
  editor.executeAction("find_previous");
}
registerHandler("vi_find_prev", vi_find_prev);

// Center view
function vi_center_cursor() : void {
  editor.executeAction("center_cursor");
}
registerHandler("vi_center_cursor", vi_center_cursor);

// Half page movements
function vi_half_page_down() : void {
  // Approximate half page with multiple down movements
  const count = consumeCount();
  editor.executeActions([{ action: "move_down", count: 10 * count }]);
}
registerHandler("vi_half_page_down", vi_half_page_down);

function vi_half_page_up() : void {
  const count = consumeCount();
  editor.executeActions([{ action: "move_up", count: 10 * count }]);
}
registerHandler("vi_half_page_up", vi_half_page_up);

// ============================================================================
// Count Prefix (digit keys 1-9, and 0 after initial digit)
// ============================================================================

// Digit handlers for count prefix
function vi_digit_1() : void { accumulateCount(1); }
registerHandler("vi_digit_1", vi_digit_1);
function vi_digit_2() : void { accumulateCount(2); }
registerHandler("vi_digit_2", vi_digit_2);
function vi_digit_3() : void { accumulateCount(3); }
registerHandler("vi_digit_3", vi_digit_3);
function vi_digit_4() : void { accumulateCount(4); }
registerHandler("vi_digit_4", vi_digit_4);
function vi_digit_5() : void { accumulateCount(5); }
registerHandler("vi_digit_5", vi_digit_5);
function vi_digit_6() : void { accumulateCount(6); }
registerHandler("vi_digit_6", vi_digit_6);
function vi_digit_7() : void { accumulateCount(7); }
registerHandler("vi_digit_7", vi_digit_7);
function vi_digit_8() : void { accumulateCount(8); }
registerHandler("vi_digit_8", vi_digit_8);
function vi_digit_9() : void { accumulateCount(9); }
registerHandler("vi_digit_9", vi_digit_9);

// 0 is special: if count is already started, it appends; otherwise it's "go to line start"
function vi_digit_0_or_line_start() : void {
  if (state.count !== null) {
    accumulateCount(0);
  } else {
    editor.executeAction("move_line_start");
  }
}
registerHandler("vi_digit_0_or_line_start", vi_digit_0_or_line_start);

// 0 in operator-pending mode: if count is started, append; otherwise apply operator to line start
async function vi_op_digit_0_or_line_start() : Promise<void> {
  if (state.count !== null) {
    accumulateCount(0);
  } else {
    await handleMotionWithOperator("move_line_start");
  }
}
registerHandler("vi_op_digit_0_or_line_start", vi_op_digit_0_or_line_start);

// ============================================================================
// Visual Mode
// ============================================================================

function clearComputedVisualRange(): void {
  state.visualHead = null;
  state.visualRange = null;
}

// Enter character-wise visual mode
function vi_visual_char() : void {
  state.visualAnchor = editor.getCursorPosition();
  state.visualHead = state.visualAnchor;
  state.visualRange = null;
  // Select the character under cursor to establish the anchor.
  // This moves cursor one position right (the selection end), which is
  // standard visual mode behavior — the first char is part of the selection.
  editor.executeAction("select_right");
  switchMode("visual");
}
registerHandler("vi_visual_char", vi_visual_char);

// Enter line-wise visual mode
function vi_visual_line() : void {
  state.visualAnchor = editor.getCursorPosition();
  state.visualHead = state.visualAnchor;
  state.visualRange = null;
  // Select full line including newline (select_line selects and moves to next line)
  editor.executeAction("select_line");
  switchMode("visual-line");
}
registerHandler("vi_visual_line", vi_visual_line);

// Toggle between visual and visual-line modes
function vi_visual_toggle_line() : void {
  clearComputedVisualRange();
  if (state.mode === "visual") {
    // Switch to line mode - extend selection to full lines
    editor.executeAction("select_line");
    state.mode = "visual-line";
    editor.setEditorMode("vi-visual-line");
    editor.setStatus(getModeIndicator("visual-line"));
  } else if (state.mode === "visual-line") {
    // Switch to char mode (keep selection but change mode)
    state.mode = "visual";
    editor.setEditorMode("vi-visual");
    editor.setStatus(getModeIndicator("visual"));
  }
}
registerHandler("vi_visual_toggle_line", vi_visual_toggle_line);

// Enter visual block mode (Ctrl-v)
async function vi_visual_block() : Promise<void> {
  // Store anchor position for block selection
  state.visualAnchor = editor.getCursorPosition();
  state.visualHead = state.visualAnchor;
  state.visualRange = null;

  // Calculate line and column for block anchor
  const cursorPos = editor.getCursorPosition();
  if (cursorPos !== null) {
    const line = editor.getPrimaryCursor()?.line ?? 1;
    const lineStart = await editor.getLineStartPosition(line);
    const col = lineStart !== null ? cursorPos - lineStart : 0;
    state.visualBlockAnchor = { line, col };
  }

  // Select current character to start
  editor.executeAction("select_right");
  switchMode("visual-block");
}
registerHandler("vi_visual_block", vi_visual_block);

// Visual block mode motions - these extend the rectangular selection
function vi_vblock_left() : void {
  executeWithCount("select_left");
}
registerHandler("vi_vblock_left", vi_vblock_left);

function vi_vblock_down() : void {
  executeWithCount("select_down");
}
registerHandler("vi_vblock_down", vi_vblock_down);

function vi_vblock_up() : void {
  executeWithCount("select_up");
}
registerHandler("vi_vblock_up", vi_vblock_up);

function vi_vblock_right() : void {
  executeWithCount("select_right");
}
registerHandler("vi_vblock_right", vi_vblock_right);

function vi_vblock_line_start() : void {
  consumeCount();
  editor.executeAction("select_line_start");
}
registerHandler("vi_vblock_line_start", vi_vblock_line_start);

function vi_vblock_line_end() : void {
  consumeCount();
  editor.executeAction("select_line_end");
}
registerHandler("vi_vblock_line_end", vi_vblock_line_end);

// Visual block delete - delete the selected block
function vi_vblock_delete() : void {
  editor.executeAction("cut");
  state.lastYankWasLinewise = false;
  switchMode("normal");
}
registerHandler("vi_vblock_delete", vi_vblock_delete);

// Visual block change - delete and enter insert mode
function vi_vblock_change() : void {
  editor.executeAction("cut");
  switchMode("insert");
}
registerHandler("vi_vblock_change", vi_vblock_change);

// Visual block yank
function vi_vblock_yank() : void {
  editor.executeAction("copy");
  state.lastYankWasLinewise = false;
  // Move cursor to start of selection
  editor.executeAction("move_left");
  switchMode("normal");
}
registerHandler("vi_vblock_yank", vi_vblock_yank);

// Exit visual block mode
function vi_vblock_escape() : void {
  switchMode("normal");
}
registerHandler("vi_vblock_escape", vi_vblock_escape);

// Toggle from visual block to other visual modes
function vi_vblock_toggle_char() : void {
  // Switch to character visual mode
  state.mode = "visual";
  editor.setEditorMode("vi-visual");
  editor.setStatus(getModeIndicator("visual"));
}
registerHandler("vi_vblock_toggle_char", vi_vblock_toggle_char);

function vi_vblock_toggle_line() : void {
  // Switch to line visual mode
  editor.executeAction("select_line");
  state.mode = "visual-line";
  editor.setEditorMode("vi-visual-line");
  editor.setStatus(getModeIndicator("visual-line"));
}
registerHandler("vi_vblock_toggle_line", vi_vblock_toggle_line);

// Visual mode motions - these extend the selection
function vi_vis_left() : void {
  clearComputedVisualRange();
  executeWithCount("select_left");
}
registerHandler("vi_vis_left", vi_vis_left);

function vi_vis_down() : void {
  clearComputedVisualRange();
  executeWithCount("select_down");
}
registerHandler("vi_vis_down", vi_vis_down);

function vi_vis_up() : void {
  clearComputedVisualRange();
  executeWithCount("select_up");
}
registerHandler("vi_vis_up", vi_vis_up);

function vi_vis_right() : void {
  clearComputedVisualRange();
  executeWithCount("select_right");
}
registerHandler("vi_vis_right", vi_vis_right);

function vi_vis_word() : void {
  clearComputedVisualRange();
  executeWithCount("select_word_right");
}
registerHandler("vi_vis_word", vi_vis_word);

function vi_vis_word_back() : void {
  clearComputedVisualRange();
  executeWithCount("select_word_left");
}
registerHandler("vi_vis_word_back", vi_vis_word_back);

function vi_vis_word_end() : void {
  clearComputedVisualRange();
  // Extend selection to end of word
  const count = consumeCount();
  for (let i = 0; i < count; i++) {
    editor.executeAction("select_word_right");
    editor.executeAction("select_left");
  }
}
registerHandler("vi_vis_word_end", vi_vis_word_end);

function visualWORDMotionOrigin(): number | null {
  return state.visualHead ?? state.visualAnchor ?? editor.getCursorPosition();
}

async function vi_vis_WORD() : Promise<void> {
  const target = await computeWORDMotionTarget("forward", consumeCount(), visualWORDMotionOrigin());
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}
registerHandler("vi_vis_WORD", vi_vis_WORD);

async function vi_vis_WORD_back() : Promise<void> {
  const target = await computeWORDMotionTarget("backward", consumeCount(), visualWORDMotionOrigin());
  if (target !== null) {
    await selectVisualRangeToTarget(target, false);
  }
}
registerHandler("vi_vis_WORD_back", vi_vis_WORD_back);

async function vi_vis_WORD_end() : Promise<void> {
  const target = await computeWORDMotionTarget("end", consumeCount(), visualWORDMotionOrigin());
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}
registerHandler("vi_vis_WORD_end", vi_vis_WORD_end);

function vi_vis_line_start() : void {
  clearComputedVisualRange();
  consumeCount();
  editor.executeAction("select_line_start");
}
registerHandler("vi_vis_line_start", vi_vis_line_start);

function vi_vis_line_end() : void {
  clearComputedVisualRange();
  consumeCount();
  editor.executeAction("select_line_end");
}
registerHandler("vi_vis_line_end", vi_vis_line_end);

function vi_vis_doc_start() : void {
  clearComputedVisualRange();
  consumeCount();
  editor.executeAction("select_document_start");
}
registerHandler("vi_vis_doc_start", vi_vis_doc_start);

function vi_vis_doc_end() : void {
  clearComputedVisualRange();
  consumeCount();
  editor.executeAction("select_document_end");
}
registerHandler("vi_vis_doc_end", vi_vis_doc_end);

function vi_vis_paragraph_up() : void {
  clearComputedVisualRange();
  executeWithCount("select_to_paragraph_up");
}
registerHandler("vi_vis_paragraph_up", vi_vis_paragraph_up);

function vi_vis_paragraph_down() : void {
  clearComputedVisualRange();
  executeWithCount("select_to_paragraph_down");
}
registerHandler("vi_vis_paragraph_down", vi_vis_paragraph_down);

// Visual line mode motions - extend selection by whole lines
function vi_vline_down() : void {
  clearComputedVisualRange();
  executeWithCount("select_down");
  // Ensure full line selection
  editor.executeAction("select_line_end");
}
registerHandler("vi_vline_down", vi_vline_down);

function vi_vline_up() : void {
  clearComputedVisualRange();
  executeWithCount("select_up");
  // Ensure full line selection
  editor.executeAction("select_line_start");
}
registerHandler("vi_vline_up", vi_vline_up);

async function selectVisualRangeToTarget(target: number, includeDisplayTarget: boolean = true): Promise<void> {
  const anchor = state.visualAnchor;
  if (anchor === null) {
    state.visualHead = target;
    await selectToPosition(target, includeDisplayTarget);
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const bufferText = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const anchorIndex = byteOffsetToStringIndex(bufferText, anchor);
  const targetIndex = byteOffsetToStringIndex(bufferText, target);
  const anchorEnd = anchor + byteLengthOfCharAt(bufferText, anchorIndex);
  const targetEnd = target + byteLengthOfCharAt(bufferText, targetIndex);

  state.visualRange = target >= anchor
    ? { start: anchor, end: targetEnd }
    : { start: target, end: anchorEnd };
  state.visualHead = target;

  await selectToPosition(target, includeDisplayTarget && target >= anchor);
}

async function takeVisualRangeText(): Promise<{ bufferId: number; start: number; end: number; text: string } | null> {
  const range = state.visualRange;
  if (range === null || range.end <= range.start) {
    return null;
  }

  const bufferId = editor.getActiveBufferId();
  const text = await editor.getBufferText(bufferId, range.start, range.end);
  return { bufferId, start: range.start, end: range.end, text };
}
// Visual mode operators - act on selection
async function vi_vis_delete() : Promise<void> {
  const directRange = await takeVisualRangeText();
  if (directRange !== null) {
    editor.setClipboard(directRange.text);
    editor.deleteRange(directRange.bufferId, directRange.start, directRange.end);
    state.lastYankWasLinewise = false;
    switchMode("normal");
    editor.setBufferCursor(directRange.bufferId, Math.min(directRange.start, editor.getBufferLength(directRange.bufferId)));
    return;
  }

  const wasLinewise = state.mode === "visual-line";
  editor.executeAction("cut");
  state.lastYankWasLinewise = wasLinewise;
  switchMode("normal");
}
registerHandler("vi_vis_delete", vi_vis_delete);

async function vi_vis_change() : Promise<void> {
  const directRange = await takeVisualRangeText();
  if (directRange !== null) {
    editor.setClipboard(directRange.text);
    editor.deleteRange(directRange.bufferId, directRange.start, directRange.end);
    switchMode("insert");
    editor.setBufferCursor(directRange.bufferId, directRange.start);
    state.insertStartPos = directRange.start;
    state.lastYankWasLinewise = false;
    return;
  }

  editor.executeAction("cut");
  switchMode("insert");
}
registerHandler("vi_vis_change", vi_vis_change);

async function vi_vis_yank() : Promise<void> {
  const directRange = await takeVisualRangeText();
  if (directRange !== null) {
    editor.setClipboard(directRange.text);
    state.lastYankWasLinewise = false;
    switchMode("normal");
    editor.setBufferCursor(directRange.bufferId, directRange.start);
    return;
  }

  const wasLinewise = state.mode === "visual-line";
  editor.executeAction("copy");
  state.lastYankWasLinewise = wasLinewise;
  // Move cursor to start of selection (vim behavior)
  editor.executeAction("move_left");
  switchMode("normal");
}
registerHandler("vi_vis_yank", vi_vis_yank);

// Visual mode > / < — indent or dedent every line the selection touches, then
// return to normal mode (Vim behavior). The editor's indent/dedent already act
// on the live selection per line (the same selection visual-mode d/y operate
// on), so we drive them directly rather than recomputing the line span — that
// keeps the affected lines exactly in sync with what's highlighted.
async function applyVisualIndent(operator: string): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }
  // Remember the first selected line so the cursor can land there afterwards.
  const range = state.visualRange ?? editor.getPrimaryCursor()?.selection ?? null;
  const firstByte = range
    ? Math.min(range.start, range.end)
    : editor.getCursorPosition();

  editor.executeAction(operator === ">" ? "insert_tab" : "dedent_selection");
  state.lastYankWasLinewise = false;

  if (firstByte !== null && firstByte !== undefined) {
    const firstLineStart = await findLineStartAtPosition(bufferId, firstByte);
    await placeCursorAtFirstNonBlank(bufferId, firstLineStart);
  }
  switchMode("normal");
}

async function vi_vis_indent() : Promise<void> {
  await applyVisualIndent(">");
}
registerHandler("vi_vis_indent", vi_vis_indent);

async function vi_vis_dedent() : Promise<void> {
  await applyVisualIndent("<");
}
registerHandler("vi_vis_dedent", vi_vis_dedent);

// Exit visual mode without doing anything
function vi_vis_escape() : void {
  switchMode("normal");
}
registerHandler("vi_vis_escape", vi_vis_escape);

// ============================================================================
// Text Objects (iw, aw, i", a", etc.)
// ============================================================================

// Enter text-object mode with "inner" modifier
function vi_text_object_inner() : void {
  state.pendingTextObject = "inner";
  state.mode = "text-object";
  editor.setEditorMode("vi-text-object");
  editor.setStatus(getModeIndicator("text-object"));
}
registerHandler("vi_text_object_inner", vi_text_object_inner);

// Enter text-object mode with "around" modifier
function vi_text_object_around() : void {
  state.pendingTextObject = "around";
  state.mode = "text-object";
  editor.setEditorMode("vi-text-object");
  editor.setStatus(getModeIndicator("text-object"));
}
registerHandler("vi_text_object_around", vi_text_object_around);

// Apply text object selection and then the pending operator
async function applyTextObject(objectType: string): Promise<void> {
  const operator = state.pendingOperator;
  const isInner = state.pendingTextObject === "inner";
  const modifier = state.pendingTextObject;

  if (!operator) {
    switchMode("normal");
    return;
  }

  // Record last change for '.' repeat (only for delete and change, not yank)
  if ((operator === "d" || operator === "c") && modifier) {
    state.lastChange = { type: "operator-textobj", operator, textObject: { modifier, object: objectType } };
  }

  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    switchMode("normal");
    return;
  }

  // Get text around cursor to find the text object boundaries
  const windowSize = 1000;
  const startOffset = Math.max(0, cursorPos - windowSize);
  const bufLen = editor.getBufferLength(bufferId);
  const endOffset = Math.min(bufLen, cursorPos + windowSize);
  const text = await editor.getBufferText(bufferId, startOffset, endOffset);
  if (!text) {
    switchMode("normal");
    return;
  }

  const posInChunk = cursorPos - startOffset;
  let selectStart = -1;
  let selectEnd = -1;

  switch (objectType) {
    case "word": {
      // Find word boundaries
      const wordChars = /[a-zA-Z0-9_]/;
      let start = posInChunk;
      let end = posInChunk;

      // Expand to find word start
      while (start > 0 && wordChars.test(text[start - 1])) start--;
      // Expand to find word end
      while (end < text.length && wordChars.test(text[end])) end++;

      if (!isInner) {
        // "a word" includes trailing whitespace
        while (end < text.length && /\s/.test(text[end]) && text[end] !== '\n') end++;
      }

      selectStart = startOffset + start;
      selectEnd = startOffset + end;
      break;
    }

    case "WORD": {
      // WORD is whitespace-delimited
      let start = posInChunk;
      let end = posInChunk;

      while (start > 0 && !/\s/.test(text[start - 1])) start--;
      while (end < text.length && !/\s/.test(text[end])) end++;

      if (!isInner) {
        while (end < text.length && /\s/.test(text[end]) && text[end] !== '\n') end++;
      }

      selectStart = startOffset + start;
      selectEnd = startOffset + end;
      break;
    }

    case "\"":
    case "'":
    case "`": {
      // Find matching quotes on current line
      // First find line boundaries
      let lineStart = posInChunk;
      let lineEnd = posInChunk;
      while (lineStart > 0 && text[lineStart - 1] !== '\n') lineStart--;
      while (lineEnd < text.length && text[lineEnd] !== '\n') lineEnd++;

      const line = text.substring(lineStart, lineEnd);
      const colInLine = posInChunk - lineStart;

      // Find the quote pair to operate on. Vim's rule for i"/a" is to use the
      // pair the cursor is inside, or — when the cursor is before the quotes on
      // the line — to search forward on the current line for the next pair. We
      // therefore pick the first complete pair whose closing quote is at or
      // after the cursor (covers both "inside" and "before" the quotes), which
      // makes ci"/di" work from the start of a line (the common case).
      let quoteStart = -1;
      let quoteEnd = -1;
      let openIdx = -1;

      for (let i = 0; i < line.length; i++) {
        if (line[i] !== objectType) continue;
        if (openIdx === -1) {
          openIdx = i; // opening quote of a candidate pair
        } else {
          // Completed a pair [openIdx, i].
          if (colInLine <= i) {
            quoteStart = openIdx;
            quoteEnd = i;
            break; // first pair at/after the cursor wins (forward search)
          }
          openIdx = -1; // pair is entirely before the cursor; keep searching
        }
      }

      if (quoteStart !== -1 && quoteEnd !== -1) {
        if (isInner) {
          selectStart = startOffset + lineStart + quoteStart + 1;
          selectEnd = startOffset + lineStart + quoteEnd;
        } else {
          selectStart = startOffset + lineStart + quoteStart;
          selectEnd = startOffset + lineStart + quoteEnd + 1;
        }
      }
      break;
    }

    case "(":
    case ")":
    case "b": {
      // Find matching parentheses
      const result = findMatchingPair(text, posInChunk, '(', ')');
      if (result) {
        if (isInner) {
          selectStart = startOffset + result.start + 1;
          selectEnd = startOffset + result.end;
        } else {
          selectStart = startOffset + result.start;
          selectEnd = startOffset + result.end + 1;
        }
      }
      break;
    }

    case "{":
    case "}":
    case "B": {
      const result = findMatchingPair(text, posInChunk, '{', '}');
      if (result) {
        if (isInner) {
          selectStart = startOffset + result.start + 1;
          selectEnd = startOffset + result.end;
        } else {
          selectStart = startOffset + result.start;
          selectEnd = startOffset + result.end + 1;
        }
      }
      break;
    }

    case "[":
    case "]": {
      const result = findMatchingPair(text, posInChunk, '[', ']');
      if (result) {
        if (isInner) {
          selectStart = startOffset + result.start + 1;
          selectEnd = startOffset + result.end;
        } else {
          selectStart = startOffset + result.start;
          selectEnd = startOffset + result.end + 1;
        }
      }
      break;
    }

    case "<":
    case ">": {
      const result = findMatchingPair(text, posInChunk, '<', '>');
      if (result) {
        if (isInner) {
          selectStart = startOffset + result.start + 1;
          selectEnd = startOffset + result.end;
        } else {
          selectStart = startOffset + result.start;
          selectEnd = startOffset + result.end + 1;
        }
      }
      break;
    }
  }

  if (selectStart === -1 || selectEnd === -1 || selectStart >= selectEnd) {
    switchMode("normal");
    return;
  }

  // Apply the operator directly using deleteRange/copyRange
  switch (operator) {
    case "d": {
      const deletedText = await editor.getBufferText(bufferId, selectStart, selectEnd);
      if (deletedText) {
        editor.setClipboard(deletedText);
      }
      editor.deleteRange(bufferId, selectStart, selectEnd);
      // Land the cursor where the text object was, even if it was forward of the
      // cursor on the line (e.g. di" from before the quotes).
      editor.setBufferCursor(bufferId, selectStart);
      state.lastYankWasLinewise = false;
      break;
    }
    case "c": {
      const deletedText = await editor.getBufferText(bufferId, selectStart, selectEnd);
      if (deletedText) {
        editor.setClipboard(deletedText);
      }
      editor.deleteRange(bufferId, selectStart, selectEnd);
      // Insert at the deletion point so ci" works even when the quoted string
      // was forward of the cursor on the line (not just when already inside it).
      editor.setBufferCursor(bufferId, selectStart);
      state.lastYankWasLinewise = false;
      switchMode("insert");
      return;
    }
    case "y": {
      // For yank, we need to select the range and copy
      // First move cursor to start
      editor.setBufferCursor(bufferId, selectStart);
      // Select the range
      for (let i = 0; i < selectEnd - selectStart; i++) {
        editor.executeAction("select_right");
      }
      editor.executeAction("copy");
      state.lastYankWasLinewise = false;
      // Move back to start
      editor.setBufferCursor(bufferId, selectStart);
      break;
    }
  }

  switchMode("normal");
}

// Helper to find matching bracket pair containing the cursor
function findMatchingPair(text: string, pos: number, openChar: string, closeChar: string): { start: number; end: number } | null {
  let depth = 0;
  let start = -1;

  // Search backward for opening bracket
  for (let i = pos; i >= 0; i--) {
    if (text[i] === closeChar) depth++;
    if (text[i] === openChar) {
      if (depth === 0) {
        start = i;
        break;
      }
      depth--;
    }
  }

  if (start === -1) return null;

  // Search forward for closing bracket
  depth = 0;
  for (let i = start; i < text.length; i++) {
    if (text[i] === openChar) depth++;
    if (text[i] === closeChar) {
      depth--;
      if (depth === 0) {
        return { start, end: i };
      }
    }
  }

  return null;
}

// Text object handlers
async function vi_to_word() : Promise<void> { await applyTextObject("word"); }
registerHandler("vi_to_word", vi_to_word);
async function vi_to_WORD() : Promise<void> { await applyTextObject("WORD"); }
registerHandler("vi_to_WORD", vi_to_WORD);
async function vi_to_dquote() : Promise<void> { await applyTextObject("\""); }
registerHandler("vi_to_dquote", vi_to_dquote);
async function vi_to_squote() : Promise<void> { await applyTextObject("'"); }
registerHandler("vi_to_squote", vi_to_squote);
async function vi_to_backtick() : Promise<void> { await applyTextObject("`"); }
registerHandler("vi_to_backtick", vi_to_backtick);
async function vi_to_paren() : Promise<void> { await applyTextObject("("); }
registerHandler("vi_to_paren", vi_to_paren);
async function vi_to_brace() : Promise<void> { await applyTextObject("{"); };
async function vi_to_bracket(): Promise<void> { await applyTextObject("["); }
registerHandler("vi_to_bracket", vi_to_bracket);
async function vi_to_angle(): Promise<void> { await applyTextObject("<"); }
registerHandler("vi_to_angle", vi_to_angle);

// Cancel text object mode
function vi_to_cancel(): void {
  switchMode("normal");
}
registerHandler("vi_to_cancel", vi_to_cancel);

// ============================================================================
// Find Character Motions (f/t/F/T)
// ============================================================================

// Enter find-char mode, await one keypress, then dispatch.
//
// Implemented via `editor.getNextKey()` (plugin API #1) — the editor
// hands the next keypress to this awaiting handler before any other
// dispatch, which means the mode itself does not need any per-key
// bindings.  Keeps `setEditorMode("vi-find-char")` set across the
// await purely for the status-bar indicator.
async function enterFindCharMode(findType: FindCharType): Promise<void> {
  state.pendingFindChar = findType;
  state.mode = "find-char";
  editor.setEditorMode("vi-find-char");
  editor.setStatus(getModeIndicator("find-char"));

  // Capture the key losslessly — without this, a user pressing the
  // target character very quickly after `f`/`t`/`F`/`T` could see the
  // key fall through to the buffer.
  editor.beginKeyCapture();
  try {
    const ev = await editor.getNextKey();
    state.pendingFindChar = null;
    // Escape (or any non-character key) cancels the motion.
    if (ev.key.length === 1) {
      await executeFindChar(findType, ev.key);
    }
  } finally {
    editor.endKeyCapture();
  }
  switchMode("normal");
}

// Execute find char motion (async because getBufferText is async)
async function executeFindChar(findType: FindCharType, char: string): Promise<void> {
  if (!findType) return;

  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null || (cursorPos === 0 && (findType === "F" || findType === "T"))) {
    // Can't search backward from position 0
    return;
  }

  // Get text around cursor to find line boundaries
  // Read up to 10KB before and after cursor for context
  const windowSize = 10000;
  const startOffset = Math.max(0, cursorPos - windowSize);
  const bufLen = editor.getBufferLength(bufferId);
  const endOffset = Math.min(bufLen, cursorPos + windowSize);

  // Get buffer text around cursor
  const text = await editor.getBufferText(bufferId, startOffset, endOffset);
  if (!text) return;

  // Calculate position within this text chunk
  const posInChunk = cursorPos - startOffset;

  // Find line start (last newline before cursor, or start of chunk)
  let lineStart = 0;
  for (let i = posInChunk - 1; i >= 0; i--) {
    if (text[i] === '\n') {
      lineStart = i + 1;
      break;
    }
  }

  // Find line end (next newline after cursor, or end of chunk)
  let lineEnd = text.length;
  for (let i = posInChunk; i < text.length; i++) {
    if (text[i] === '\n') {
      lineEnd = i;
      break;
    }
  }

  // Extract line text and calculate column
  const lineText = text.substring(lineStart, lineEnd);
  const col = posInChunk - lineStart;

  let targetCol = -1;

  if (findType === "f" || findType === "t") {
    // Search forward on the line
    for (let i = col + 1; i < lineText.length; i++) {
      if (lineText[i] === char) {
        targetCol = findType === "f" ? i : i - 1;
        break;
      }
    }
  } else {
    // Search backward (F/T)
    for (let i = col - 1; i >= 0; i--) {
      if (lineText[i] === char) {
        targetCol = findType === "F" ? i : i + 1;
        break;
      }
    }
  }

  if (targetCol >= 0 && targetCol !== col) {
    // Move to target column
    const diff = targetCol - col;
    const moveAction = diff > 0 ? "move_right" : "move_left";
    const steps = Math.abs(diff);
    for (let i = 0; i < steps; i++) {
      editor.executeAction(moveAction);
    }
    // Save for ; and , repeat
    state.lastFindChar = { type: findType, char };
  }
}

// Commands to enter find-char mode (async; await getNextKey internally)
async function vi_find_char_f(): Promise<void> { return enterFindCharMode("f"); }
registerHandler("vi_find_char_f", vi_find_char_f);

async function vi_find_char_t(): Promise<void> { return enterFindCharMode("t"); }
registerHandler("vi_find_char_t", vi_find_char_t);

async function vi_find_char_F(): Promise<void> { return enterFindCharMode("F"); }
registerHandler("vi_find_char_F", vi_find_char_F);

async function vi_find_char_T(): Promise<void> { return enterFindCharMode("T"); }
registerHandler("vi_find_char_T", vi_find_char_T);

// Repeat last find char (async)
async function vi_find_char_repeat(): Promise<void> {
  if (state.lastFindChar) {
    await executeFindChar(state.lastFindChar.type, state.lastFindChar.char);
  }
}
registerHandler("vi_find_char_repeat", vi_find_char_repeat);

// Repeat last find char in opposite direction (async)
async function vi_find_char_repeat_reverse(): Promise<void> {
  if (state.lastFindChar) {
    const reversedType: FindCharType =
      state.lastFindChar.type === "f" ? "F" :
      state.lastFindChar.type === "F" ? "f" :
      state.lastFindChar.type === "t" ? "T" : "t";
    await executeFindChar(reversedType, state.lastFindChar.char);
  }
}
registerHandler("vi_find_char_repeat_reverse", vi_find_char_repeat_reverse);

// ============================================================================
// Operator-Pending Mode Commands
// ============================================================================

async function vi_op_left(): Promise<void> {
  await handleMotionWithOperator("move_left");
}
registerHandler("vi_op_left", vi_op_left);

async function vi_op_down(): Promise<void> {
  await handleMotionWithOperator("move_down");
}
registerHandler("vi_op_down", vi_op_down);

async function vi_op_up(): Promise<void> {
  await handleMotionWithOperator("move_up");
}
registerHandler("vi_op_up", vi_op_up);

async function vi_op_right(): Promise<void> {
  await handleMotionWithOperator("move_right");
}
registerHandler("vi_op_right", vi_op_right);

async function vi_op_word(): Promise<void> {
  // Vim special case (`:help cw`): when changing (`c`) with the cursor on a
  // non-blank character, `cw` behaves like `ce` — it changes only up to the end
  // of the word and does NOT consume the trailing whitespace. Plain `dw`/`yw`
  // and `cw` on a blank keep the regular word-forward semantics.
  if (state.pendingOperator === "c") {
    const count = consumeCount();
    const range = await computeWordChangeRange(count);
    if (range !== null) {
      state.lastChange = { type: "operator-motion", operator: "c", motion: "vi_word_change", count };
      await applyOperatorWithRange("c", range.start, range.end);
      return;
    }
    await applyOperatorWithMotion("c", "move_word_right", count);
    return;
  }
  await handleMotionWithOperator("move_word_right");
}
registerHandler("vi_op_word", vi_op_word);

async function vi_op_word_back(): Promise<void> {
  await handleMotionWithOperator("move_word_left");
}
registerHandler("vi_op_word_back", vi_op_word_back);

// Operator-pending e (word end) - select to word end, then apply operator
// Operator-pending e (word end) — uses native vi_move_word_end motion
async function vi_op_word_end(): Promise<void> {
  await handleMotionWithOperator("vi_move_word_end");
}
registerHandler("vi_op_word_end", vi_op_word_end);

async function vi_op_WORD(): Promise<void> {
  await handleWORDMotionWithOperator("forward");
}
registerHandler("vi_op_WORD", vi_op_WORD);

async function vi_op_WORD_back(): Promise<void> {
  await handleWORDMotionWithOperator("backward");
}
registerHandler("vi_op_WORD_back", vi_op_WORD_back);

async function vi_op_WORD_end(): Promise<void> {
  await handleWORDMotionWithOperator("end");
}
registerHandler("vi_op_WORD_end", vi_op_WORD_end);

async function vi_op_line_start(): Promise<void> {
  await handleMotionWithOperator("move_line_start");
}
registerHandler("vi_op_line_start", vi_op_line_start);

async function vi_op_line_end(): Promise<void> {
  await handleMotionWithOperator("move_line_end");
}
registerHandler("vi_op_line_end", vi_op_line_end);

async function vi_op_doc_start(): Promise<void> {
  await handleMotionWithOperator("move_document_start");
}
registerHandler("vi_op_doc_start", vi_op_doc_start);

async function vi_op_doc_end(): Promise<void> {
  await handleMotionWithOperator("move_document_end");
}
registerHandler("vi_op_doc_end", vi_op_doc_end);

// NOTE: operator + `%` (d%/c%/y%) is currently a no-op. `applyOperatorWithMotion`
// resolves a motion via `motionToSelection`, and `goto_matching_bracket` is not
// in that map, so it bails without deleting. Making this work needs a
// selection-extending action (e.g. `select_to_matching_bracket`) plus a
// `motionToSelection` entry. See test_vi_bug_d_percent_ignored.
async function vi_op_matching_bracket(): Promise<void> {
  await handleMotionWithOperator("goto_matching_bracket");
}
registerHandler("vi_op_matching_bracket", vi_op_matching_bracket);

async function vi_op_paragraph_up(): Promise<void> {
  await handleMotionWithOperator("move_to_paragraph_up");
}
registerHandler("vi_op_paragraph_up", vi_op_paragraph_up);

async function vi_op_paragraph_down(): Promise<void> {
  if (!state.pendingOperator) {
    switchMode("normal");
    return;
  }

  const operator = state.pendingOperator;
  const count = consumeCount();
  if (operator === "d" || operator === "c") {
    state.lastChange = { type: "operator-motion", operator, motion: "move_to_paragraph_down", count };
  }

  const range = await computeParagraphDownOperatorRange(count);
  if (range === null) {
    switchMode("normal");
    return;
  }

  await applyOperatorWithRange(operator, range.start, range.end);
}
registerHandler("vi_op_paragraph_down", vi_op_paragraph_down);

function vi_cancel(): void {
  switchMode("normal");
}
registerHandler("vi_cancel", vi_cancel);

// ============================================================================
// Mode Definitions
// ============================================================================

// Define vi-normal mode
editor.defineMode("vi-normal", [
  // Count prefix (digits 1-9 start count, 0 is special)
  ["1", "vi_digit_1"],
  ["2", "vi_digit_2"],
  ["3", "vi_digit_3"],
  ["4", "vi_digit_4"],
  ["5", "vi_digit_5"],
  ["6", "vi_digit_6"],
  ["7", "vi_digit_7"],
  ["8", "vi_digit_8"],
  ["9", "vi_digit_9"],
  ["0", "vi_digit_0_or_line_start"], // 0 appends to count, or moves to line start

  // Navigation
  ["h", "vi_left"],
  ["j", "vi_down"],
  ["k", "vi_up"],
  ["l", "vi_right"],
  ["w", "vi_word"],
  ["b", "vi_word_back"],
  ["e", "vi_word_end"],
  ...configuredBindings(arrowKeys, [
    ["Left", "vi_left"],
    ["Down", "vi_down"],
    ["Up", "vi_up"],
    ["Right", "vi_right"],
  ]),
  ["W", "vi_WORD"],
  ["B", "vi_WORD_back"],
  ["E", "vi_WORD_end"],
  ["$", "vi_line_end"],
  ["^", "vi_first_non_blank"],
  ["g g", "vi_doc_start"],
  ["G", "vi_doc_end"],
  ["C-f", "vi_page_down"],
  ["C-b", "vi_page_up"],
  ["C-d", "vi_half_page_down"],
  ["C-u", "vi_half_page_up"],
  ["%", "vi_matching_bracket"],
  ["z z", "vi_center_cursor"],
  ["{", "vi_paragraph_up"],
  ["}", "vi_paragraph_down"],

  // Search
  ["/", "vi_search_forward"],
  ["?", "vi_search_backward"],
  ["n", "vi_find_next"],
  ["N", "vi_find_prev"],
  ...configuredBindings(searchWordUnderCursor, [
    ["*", "vi_search_word_forward"],
    ["#", "vi_search_word_backward"],
  ]),

  // Find character on line
  ["f", "vi_find_char_f"],
  ["t", "vi_find_char_t"],
  ["F", "vi_find_char_F"],
  ["T", "vi_find_char_T"],
  [";", "vi_find_char_repeat"],
  [",", "vi_find_char_repeat_reverse"],

  // Mode switching
  ["i", "vi_insert_before"],
  ["a", "vi_insert_after"],
  ["I", "vi_insert_line_start"],
  ["A", "vi_insert_line_end"],
  ["o", "vi_open_below"],
  ["O", "vi_open_above"],
  ["Escape", "vi_escape"],

  // Operators (single key - switches to operator-pending mode)
  // The second d/c/y is handled in operator-pending mode
  ["d", "vi_delete_operator"],
  ["c", "vi_change_operator"],
  ["y", "vi_yank_operator"],
  [">", "vi_indent_operator"],
  ["<", "vi_dedent_operator"],

  // Single char operations
  ["x", "vi_delete_char"],
  ["X", "vi_delete_char_before"],
  ["r", "vi_replace_char"],
  ["s", "vi_substitute"],
  ["S", "vi_change_line"],
  ["D", "vi_delete_to_end"],
  ["C", "vi_change_to_end"],

  // Clipboard
  ["p", "vi_paste_after"],
  ["P", "vi_paste_before"],

  // Undo/Redo
  ["u", "vi_undo"],
  ["C-r", "vi_redo"],

  // Repeat last change
  [".", "vi_repeat"],

  // Visual mode
  ["v", "vi_visual_char"],
  ["V", "vi_visual_line"],
  ["C-v", "vi_visual_block"],

  // Other
  ["J", "vi_join"],
  ["~", "vi_toggle_case"],

  // Command mode
  [":", "vi_command_mode"],

  // Pass through to standard editor shortcuts
  ["C-p", "command_palette"],
  ["C-q", "quit"],
], true); // read_only = true to prevent character insertion

// Define vi-insert mode - only Escape is special, other keys insert text
editor.defineMode("vi-insert", [
  ["Escape", "vi_escape"],
  ...configuredBindings(arrowKeys, [
    ["Left", "move_left"],
    ["Down", "move_down"],
    ["Up", "move_up"],
    ["Right", "move_right"],
  ]),
  // Pass through to standard editor shortcuts
  ["C-p", "command_palette"],
  ["C-q", "quit"],
], false); // read_only = false to allow normal typing

// vi-find-char and vi-replace-char modes do not need bindings:
// their entry-point handlers (vi_find_char_f/t/F/T, vi_replace_char) call
// editor.getNextKey() to read the next character.  setEditorMode(...) is
// still set across the await purely so the status bar shows the mode.

// Define vi-operator-pending mode
editor.defineMode("vi-operator-pending", [
  // Count prefix in operator-pending mode (for d3w = delete 3 words)
  ["1", "vi_digit_1"],
  ["2", "vi_digit_2"],
  ["3", "vi_digit_3"],
  ["4", "vi_digit_4"],
  ["5", "vi_digit_5"],
  ["6", "vi_digit_6"],
  ["7", "vi_digit_7"],
  ["8", "vi_digit_8"],
  ["9", "vi_digit_9"],
  ["0", "vi_op_digit_0_or_line_start"], // 0 appends to count, or is motion to line start

  // Motions for operators
  ["h", "vi_op_left"],
  ["j", "vi_op_down"],
  ["k", "vi_op_up"],
  ["l", "vi_op_right"],
  ["w", "vi_op_word"],
  ["b", "vi_op_word_back"],
  ["e", "vi_op_word_end"],
  ...configuredBindings(arrowKeys, [
    ["Left", "vi_op_left"],
    ["Down", "vi_op_down"],
    ["Up", "vi_op_up"],
    ["Right", "vi_op_right"],
  ]),
  ["W", "vi_op_WORD"],
  ["B", "vi_op_WORD_back"],
  ["E", "vi_op_WORD_end"],
  ["$", "vi_op_line_end"],
  ["g g", "vi_op_doc_start"],
  ["G", "vi_op_doc_end"],
  ["%", "vi_op_matching_bracket"],
  ["{", "vi_op_paragraph_up"],
  ["}", "vi_op_paragraph_down"],

  // Text objects
  ["i", "vi_text_object_inner"],
  ["a", "vi_text_object_around"],

  // Double operator = line operation
  ["d", "vi_delete_line"],
  ["c", "vi_change_line"],
  ["y", "vi_yank_line"],
  [">", "vi_indent_line"],
  ["<", "vi_dedent_line"],

  // Cancel
  ["Escape", "vi_cancel"],
], true);

// Define vi-text-object mode (waiting for object type: w, ", (, etc.)
editor.defineMode("vi-text-object", [
  // Word objects
  ["w", "vi_to_word"],
  ["W", "vi_to_WORD"],

  // Quote objects
  ["\"", "vi_to_dquote"],
  ["'", "vi_to_squote"],
  ["`", "vi_to_backtick"],

  // Bracket objects
  ["(", "vi_to_paren"],
  [")", "vi_to_paren"],
  ["b", "vi_to_paren"],
  ["{", "vi_to_brace"],
  ["}", "vi_to_brace"],
  ["B", "vi_to_brace"],
  ["[", "vi_to_bracket"],
  ["]", "vi_to_bracket"],
  ["<", "vi_to_angle"],
  [">", "vi_to_angle"],

  // Cancel
  ["Escape", "vi_to_cancel"],
], true);

// Define vi-visual mode (character-wise)
editor.defineMode("vi-visual", [
  // Count prefix
  ["1", "vi_digit_1"],
  ["2", "vi_digit_2"],
  ["3", "vi_digit_3"],
  ["4", "vi_digit_4"],
  ["5", "vi_digit_5"],
  ["6", "vi_digit_6"],
  ["7", "vi_digit_7"],
  ["8", "vi_digit_8"],
  ["9", "vi_digit_9"],
  ["0", "vi_vis_line_start"], // 0 moves to line start in visual mode

  // Motions (extend selection)
  ["h", "vi_vis_left"],
  ["j", "vi_vis_down"],
  ["k", "vi_vis_up"],
  ["l", "vi_vis_right"],
  ["w", "vi_vis_word"],
  ["b", "vi_vis_word_back"],
  ["e", "vi_vis_word_end"],
  ...configuredBindings(arrowKeys, [
    ["Left", "vi_vis_left"],
    ["Down", "vi_vis_down"],
    ["Up", "vi_vis_up"],
    ["Right", "vi_vis_right"],
  ]),
  ["W", "vi_vis_WORD"],
  ["B", "vi_vis_WORD_back"],
  ["E", "vi_vis_WORD_end"],
  ["$", "vi_vis_line_end"],
  ["^", "vi_vis_line_start"],
  ["g g", "vi_vis_doc_start"],
  ["G", "vi_vis_doc_end"],
  ["{", "vi_vis_paragraph_up"],
  ["}", "vi_vis_paragraph_down"],

  // Switch visual sub-modes
  ["V", "vi_visual_toggle_line"],
  ["C-v", "vi_visual_block"],  // Switch to block mode

  // Operators
  ["d", "vi_vis_delete"],
  ["x", "vi_vis_delete"],
  ["c", "vi_vis_change"],
  ["s", "vi_vis_change"],
  ["y", "vi_vis_yank"],
  [">", "vi_vis_indent"],
  ["<", "vi_vis_dedent"],

  // Exit
  ["Escape", "vi_vis_escape"],
  ["v", "vi_vis_escape"], // v again exits visual mode

  // Pass through to standard editor shortcuts
  ["C-p", "command_palette"],
  ["C-q", "quit"],
], true);

// Define vi-visual-line mode (line-wise)
editor.defineMode("vi-visual-line", [
  // Count prefix
  ["1", "vi_digit_1"],
  ["2", "vi_digit_2"],
  ["3", "vi_digit_3"],
  ["4", "vi_digit_4"],
  ["5", "vi_digit_5"],
  ["6", "vi_digit_6"],
  ["7", "vi_digit_7"],
  ["8", "vi_digit_8"],
  ["9", "vi_digit_9"],

  // Line motions (extend selection by lines)
  ["j", "vi_vline_down"],
  ["k", "vi_vline_up"],
  ...configuredBindings(arrowKeys, [
    ["Down", "vi_vline_down"],
    ["Up", "vi_vline_up"],
  ]),
  ["g g", "vi_vis_doc_start"],
  ["G", "vi_vis_doc_end"],

  // Switch visual sub-modes
  ["v", "vi_visual_toggle_line"],
  ["C-v", "vi_visual_block"],  // Switch to block mode

  // Operators
  ["d", "vi_vis_delete"],
  ["x", "vi_vis_delete"],
  ["c", "vi_vis_change"],
  ["s", "vi_vis_change"],
  ["y", "vi_vis_yank"],
  [">", "vi_vis_indent"],
  ["<", "vi_vis_dedent"],

  // Exit
  ["Escape", "vi_vis_escape"],
  ["V", "vi_vis_escape"], // V again exits visual-line mode

  // Pass through to standard editor shortcuts
  ["C-p", "command_palette"],
  ["C-q", "quit"],
], true);

// Define vi-visual-block mode (column/block selection)
editor.defineMode("vi-visual-block", [
  // Count prefix
  ["1", "vi_digit_1"],
  ["2", "vi_digit_2"],
  ["3", "vi_digit_3"],
  ["4", "vi_digit_4"],
  ["5", "vi_digit_5"],
  ["6", "vi_digit_6"],
  ["7", "vi_digit_7"],
  ["8", "vi_digit_8"],
  ["9", "vi_digit_9"],
  ["0", "vi_vblock_line_start"],

  // Motions (extend block selection)
  ["h", "vi_vblock_left"],
  ["j", "vi_vblock_down"],
  ["k", "vi_vblock_up"],
  ["l", "vi_vblock_right"],
  ...configuredBindings(arrowKeys, [
    ["Left", "vi_vblock_left"],
    ["Down", "vi_vblock_down"],
    ["Up", "vi_vblock_up"],
    ["Right", "vi_vblock_right"],
  ]),
  ["$", "vi_vblock_line_end"],
  ["^", "vi_vblock_line_start"],

  // Switch to other visual modes
  ["v", "vi_vblock_toggle_char"],
  ["V", "vi_vblock_toggle_line"],

  // Operators
  ["d", "vi_vblock_delete"],
  ["x", "vi_vblock_delete"],
  ["c", "vi_vblock_change"],
  ["s", "vi_vblock_change"],
  ["y", "vi_vblock_yank"],

  // Exit
  ["Escape", "vi_vblock_escape"],
  ["C-v", "vi_vblock_escape"], // Ctrl-v again exits visual-block mode

  // Pass through to standard editor shortcuts
  ["C-p", "command_palette"],
  ["C-q", "quit"],
], true);

// ============================================================================
// Register Commands
// ============================================================================

// Navigation commands
const navCommands = [
  ["vi_left", "move_left"],
  ["vi_down", "move_down"],
  ["vi_up", "move_up"],
  ["vi_right", "move_right"],
  ["vi_word", "move_word"],
  ["vi_word_back", "move_word_back"],
  ["vi_word_end", "move_word_end"],
  ["vi_line_start", "move_line_start"],
  ["vi_line_end", "move_line_end"],
  ["vi_doc_start", "move_doc_start"],
  ["vi_doc_end", "move_doc_end"],
  ["vi_page_down", "page_down"],
  ["vi_page_up", "page_up"],
  ["vi_half_page_down", "half_page_down"],
  ["vi_half_page_up", "half_page_up"],
  ["vi_center_cursor", "center_cursor"],
  ["vi_search_forward", "search_forward"],
  ["vi_search_backward", "search_backward"],
  ["vi_find_next", "find_next"],
  ["vi_find_prev", "find_prev"],
  ["vi_find_char_f", "find_char_f"],
  ["vi_find_char_t", "find_char_t"],
  ["vi_find_char_F", "find_char_F"],
  ["vi_find_char_T", "find_char_T"],
  ["vi_find_char_repeat", "find_char_repeat"],
  ["vi_find_char_repeat_reverse", "find_char_repeat_reverse"],
];

for (const [name, key] of navCommands) {
  editor.registerCommand(`%cmd.${key}`, `%cmd.${key}`, name, "vi-normal");
}

// Mode commands
const modeCommands = [
  ["vi_insert_before", "insert_before"],
  ["vi_insert_after", "insert_after"],
  ["vi_insert_line_start", "insert_line_start"],
  ["vi_insert_line_end", "insert_line_end"],
  ["vi_open_below", "open_below"],
  ["vi_open_above", "open_above"],
  ["vi_escape", "return_to_normal"],
];

for (const [name, key] of modeCommands) {
  editor.registerCommand(`%cmd.${key}`, `%cmd.${key}`, name, "vi-normal");
}

// Operator commands
const opCommands = [
  ["vi_delete_operator", "delete_operator"],
  ["vi_change_operator", "change_operator"],
  ["vi_yank_operator", "yank_operator"],
  ["vi_delete_line", "delete_line"],
  ["vi_change_line", "change_line"],
  ["vi_yank_line", "yank_line"],
  ["vi_delete_char", "delete_char"],
  ["vi_delete_char_before", "delete_char_before"],
  ["vi_substitute", "substitute"],
  ["vi_delete_to_end", "delete_to_end"],
  ["vi_change_to_end", "change_to_end"],
  ["vi_paste_after", "paste_after"],
  ["vi_paste_before", "paste_before"],
  ["vi_undo", "undo"],
  ["vi_redo", "redo"],
  ["vi_join", "join_lines"],
];

for (const [name, key] of opCommands) {
  editor.registerCommand(`%cmd.${key}`, `%cmd.${key}`, name, "vi-normal");
}

// ============================================================================
// Colon Command Mode (:w, :q, :wq, :q!, :e, etc.)
// ============================================================================

// Start command mode - shows ":" prompt at the bottom
function vi_command_mode(): void {
  editor.startPrompt(":", "vi-command");
}
registerHandler("vi_command_mode", vi_command_mode);

// Handle command execution when user presses Enter


interface CommandResult {
  error?: string;
  message?: string;
}

// Command definition for the command table
interface CommandDef {
  name: string;           // Full command name
  minAbbrev: number;      // Minimum abbreviation length (e.g., 1 for "w" -> "write")
  allowBang: boolean;     // Whether command accepts ! suffix
  hasArgs: boolean;       // Whether command accepts arguments
}

// Command table - defines all supported commands with their abbreviations
// Vim allows any unambiguous prefix of a command name
const commandTable: CommandDef[] = [
  // File operations
  { name: "write", minAbbrev: 1, allowBang: true, hasArgs: true },     // :w, :wri, :write
  { name: "quit", minAbbrev: 1, allowBang: true, hasArgs: false },     // :q, :qu, :quit
  { name: "wq", minAbbrev: 2, allowBang: true, hasArgs: false },       // :wq
  { name: "wall", minAbbrev: 2, allowBang: false, hasArgs: false },    // :wa, :wall
  { name: "qall", minAbbrev: 2, allowBang: true, hasArgs: false },     // :qa, :qall
  { name: "wqall", minAbbrev: 3, allowBang: false, hasArgs: false },   // :wqa, :wqall
  { name: "xit", minAbbrev: 1, allowBang: false, hasArgs: false },     // :x, :xit (same as :wq)
  { name: "exit", minAbbrev: 3, allowBang: false, hasArgs: false },    // :exi, :exit
  { name: "edit", minAbbrev: 1, allowBang: true, hasArgs: true },      // :e, :ed, :edit
  { name: "enew", minAbbrev: 3, allowBang: true, hasArgs: false },     // :ene, :enew
  { name: "saveas", minAbbrev: 3, allowBang: false, hasArgs: true },   // :sav, :saveas

  // Buffer navigation
  { name: "next", minAbbrev: 1, allowBang: true, hasArgs: false },     // :n, :next
  { name: "previous", minAbbrev: 4, allowBang: true, hasArgs: false }, // :prev, :previous
  { name: "bnext", minAbbrev: 2, allowBang: false, hasArgs: false },   // :bn, :bnext
  { name: "bprevious", minAbbrev: 2, allowBang: false, hasArgs: false },// :bp, :bprev, :bprevious
  { name: "bdelete", minAbbrev: 2, allowBang: true, hasArgs: false },  // :bd, :bdelete
  { name: "buffer", minAbbrev: 1, allowBang: false, hasArgs: true },   // :b, :buffer
  { name: "buffers", minAbbrev: 2, allowBang: false, hasArgs: false }, // :bu, :buffers (same as :ls)
  { name: "ls", minAbbrev: 2, allowBang: false, hasArgs: false },      // :ls
  { name: "files", minAbbrev: 3, allowBang: false, hasArgs: false },   // :fil, :files

  // Splits
  { name: "split", minAbbrev: 2, allowBang: false, hasArgs: true },    // :sp, :split
  { name: "vsplit", minAbbrev: 2, allowBang: false, hasArgs: true },   // :vs, :vsplit
  { name: "new", minAbbrev: 3, allowBang: false, hasArgs: true },      // :new
  { name: "vnew", minAbbrev: 3, allowBang: false, hasArgs: true },     // :vne, :vnew
  { name: "only", minAbbrev: 2, allowBang: true, hasArgs: false },     // :on, :only
  { name: "close", minAbbrev: 3, allowBang: true, hasArgs: false },    // :clo, :close

  // Tabs (mapped to buffers in Fresh)
  { name: "tabnew", minAbbrev: 4, allowBang: false, hasArgs: true },   // :tabn, :tabnew
  { name: "tabedit", minAbbrev: 4, allowBang: false, hasArgs: true },  // :tabe, :tabedit
  { name: "tabclose", minAbbrev: 4, allowBang: true, hasArgs: false }, // :tabc, :tabclose
  { name: "tabnext", minAbbrev: 5, allowBang: false, hasArgs: false }, // :tabne, :tabnext (note: different from :tabn)
  { name: "tabprevious", minAbbrev: 4, allowBang: false, hasArgs: false }, // :tabp, :tabprevious

  // Quickfix (mapped to diagnostics in Fresh)
  { name: "copen", minAbbrev: 3, allowBang: false, hasArgs: false },   // :cop, :copen
  { name: "cclose", minAbbrev: 3, allowBang: false, hasArgs: false },  // :ccl, :cclose
  { name: "cnext", minAbbrev: 2, allowBang: true, hasArgs: false },    // :cn, :cnext
  { name: "cprevious", minAbbrev: 2, allowBang: true, hasArgs: false },// :cp, :cprev, :cprevious
  { name: "cfirst", minAbbrev: 3, allowBang: true, hasArgs: false },   // :cfir, :cfirst
  { name: "clast", minAbbrev: 3, allowBang: true, hasArgs: false },    // :cla, :clast

  // Search and replace
  { name: "nohlsearch", minAbbrev: 3, allowBang: false, hasArgs: false }, // :noh, :nohlsearch
  { name: "substitute", minAbbrev: 1, allowBang: false, hasArgs: true },  // :s, :substitute
  { name: "global", minAbbrev: 1, allowBang: false, hasArgs: true },      // :g, :global
  { name: "vglobal", minAbbrev: 2, allowBang: false, hasArgs: true },     // :vg, :vglobal

  // Undo/redo
  { name: "undo", minAbbrev: 1, allowBang: true, hasArgs: false },     // :u, :undo
  { name: "redo", minAbbrev: 3, allowBang: false, hasArgs: false },    // :red, :redo

  // Settings
  { name: "set", minAbbrev: 2, allowBang: false, hasArgs: true },      // :se, :set

  // Info commands
  { name: "pwd", minAbbrev: 2, allowBang: false, hasArgs: false },     // :pw, :pwd
  { name: "cd", minAbbrev: 2, allowBang: false, hasArgs: true },       // :cd
  { name: "file", minAbbrev: 1, allowBang: false, hasArgs: true },     // :f, :file
  { name: "help", minAbbrev: 1, allowBang: false, hasArgs: true },     // :h, :help
  { name: "version", minAbbrev: 3, allowBang: false, hasArgs: false }, // :ver, :version

  // Other
  { name: "marks", minAbbrev: 4, allowBang: false, hasArgs: false },   // :mark, :marks
  { name: "registers", minAbbrev: 3, allowBang: false, hasArgs: false },// :reg, :registers
  { name: "jumps", minAbbrev: 2, allowBang: false, hasArgs: false },   // :ju, :jumps
  { name: "syntax", minAbbrev: 2, allowBang: false, hasArgs: true },   // :sy, :syntax
  { name: "read", minAbbrev: 1, allowBang: false, hasArgs: true },     // :r, :read
  { name: "grep", minAbbrev: 2, allowBang: false, hasArgs: true },     // :gr, :grep
  { name: "vimgrep", minAbbrev: 3, allowBang: false, hasArgs: true },  // :vim, :vimgrep
  { name: "make", minAbbrev: 3, allowBang: true, hasArgs: true },      // :mak, :make
  { name: "ascii", minAbbrev: 2, allowBang: false, hasArgs: false },   // :as, :ascii
  { name: "revert", minAbbrev: 3, allowBang: false, hasArgs: false },  // :rev, :revert (Fresh-specific)
];

// Find a command by name or abbreviation
function findCommand(input: string): CommandDef | null {
  // Exact match first
  for (const cmd of commandTable) {
    if (cmd.name === input) {
      return cmd;
    }
  }

  // Then try abbreviation matching
  const matches: CommandDef[] = [];
  for (const cmd of commandTable) {
    // Input must be at least minAbbrev chars and be a prefix of the command name
    if (input.length >= cmd.minAbbrev && cmd.name.startsWith(input)) {
      matches.push(cmd);
    }
  }

  // Return only if unambiguous
  if (matches.length === 1) {
    return matches[0];
  }

  // Handle special short aliases that vim supports even if ambiguous
  // These are the classic vim abbreviations that always work
  const shortAliases: Record<string, string> = {
    "w": "write",
    "q": "quit",
    "e": "edit",
    "n": "next",
    "N": "previous",
    "b": "buffer",
    "f": "file",
    "h": "help",
    "u": "undo",
    "r": "read",
    "s": "substitute",
    "g": "global",
    "x": "xit",
  };

  if (shortAliases[input]) {
    return commandTable.find(c => c.name === shortAliases[input]) || null;
  }

  return null;
}

// Execute a vi command and return result
async function executeViCommand(cmd: string): Promise<CommandResult> {
  // Handle pure line numbers first (e.g., :42)
  const lineNumMatch = cmd.match(/^(\d+)$/);
  if (lineNumMatch) {
    const lineNum = parseInt(lineNumMatch[1], 10);
    return gotoLine(lineNum);
  }

  // Handle range prefix with command (e.g., :1,10d or :%d)
  // Supported range formats: %, ., $, 'a, line numbers, and combinations with ,
  let processedCmd = cmd;
  let range: string | null = null;

  const rangePattern = /^([%.$]|\d+|'[a-z])?(?:,([%.$]|\d+|'[a-z]))?\s*(.*)$/;
  const rangeMatch = cmd.match(rangePattern);
  if (rangeMatch && rangeMatch[3]) {
    // There's a command after the range
    range = (rangeMatch[1] || "") + (rangeMatch[2] ? "," + rangeMatch[2] : "");
    processedCmd = rangeMatch[3];
  }

  // Handle special commands that start with symbols
  if (processedCmd.startsWith("!")) {
    // Shell command - not implemented
    return { error: editor.t("error.shell_not_supported") };
  }

  // Handle +cmd syntax for :e +10 file (open file at line 10)
  let plusCmd: string | null = null;
  if (processedCmd.startsWith("+")) {
    const plusMatch = processedCmd.match(/^\+(\S*)\s*(.*)/);
    if (plusMatch) {
      plusCmd = plusMatch[1] || "$"; // + alone means go to end
      processedCmd = plusMatch[2];
    }
  }

  // Split command into command name and arguments
  // Supports: cmd, cmd!, cmd args, cmd! args
  const match = processedCmd.match(/^([a-zA-Z]\w*)(!)?(?:\s+(.*))?$/);
  if (!match) {
    // Maybe it's just a command name without arguments
    if (processedCmd.match(/^[a-zA-Z]+$/)) {
      const cmdDef = findCommand(processedCmd);
      if (cmdDef) {
        return executeCommand(cmdDef.name, false, null, range);
      }
    }
    return { error: editor.t("error.not_valid_command", { cmd: processedCmd }) };
  }

  const [, commandInput, bang, args] = match;
  const force = bang === "!";

  // Look up the command
  const cmdDef = findCommand(commandInput);
  if (!cmdDef) {
    return { error: editor.t("error.unknown_command", { cmd: commandInput }) };
  }

  // Validate bang usage
  if (force && !cmdDef.allowBang) {
    return { error: editor.t("error.command_no_bang", { cmd: cmdDef.name }) };
  }

  // Execute the command
  return executeCommand(cmdDef.name, force, args || null, range);
}

// Execute a resolved command
async function executeCommand(
  command: string,
  force: boolean,
  args: string | null,
  _range: string | null  // Range support is limited for now
): Promise<CommandResult> {

  switch (command) {
    case "write": {
      // :w - save current file
      // :w filename - save to specified filename
      if (args) {
        const bufferId = editor.getActiveBufferId();
        // Resolve path (could be relative or absolute)
        const path = args.startsWith("/") ? args : `${editor.getCwd()}/${args}`;
        editor.saveBufferToPath(bufferId, path);
        return { message: editor.t("status.file_saved") };
      }
      editor.executeAction("save");
      return { message: editor.t("status.file_saved") };
    }

    case "quit": {
      // :q - quit editor (like vim)
      // :q! - force quit (discard unsaved changes)
      if (force) {
        editor.executeAction("force_quit");
        return {};
      }
      // Check ALL buffers for unsaved changes
      const buffers = editor.listBuffers() as Array<{ id: number; modified: boolean }>;
      const hasModified = buffers.some((b) => b.modified);
      if (hasModified) {
        return { error: editor.t("error.no_write_since_change", { cmd: ":q!" }) };
      }
      editor.executeAction("force_quit");
      return {};
    }

    case "wq":
    case "xit":
    case "exit": {
      // :wq or :x - save current buffer and quit
      // :wq filename - save to filename and quit
      const wqBufferId = editor.getActiveBufferId();

      if (args) {
        // Save to specified filename
        const path = args.startsWith("/") ? args : `${editor.getCwd()}/${args}`;
        editor.saveBufferToPath(wqBufferId, path);
      } else {
        // Save to existing path
        const wqPath = editor.getBufferPath(wqBufferId);
        if (!wqPath) {
          return { error: editor.t("error.no_file_name") };
        }
        editor.executeAction("save");
      }

      // Check if any OTHER buffers have unsaved changes
      const allBuffers = editor.listBuffers() as Array<{ id: number; modified: boolean }>;
      const otherModified = allBuffers.some((b: { id: number; modified: boolean }) => b.id !== wqBufferId && b.modified);
      if (otherModified) {
        return { error: editor.t("error.other_buffers_modified", { cmd: ":wqa" }) };
      }
      editor.executeAction("force_quit");
      return {};
    }

    case "wall": {
      // :wa - save all buffers
      editor.executeAction("save_all");
      return { message: editor.t("status.all_files_saved") };
    }

    case "qall": {
      // :qa - quit all
      // :qa! - force quit all
      if (force) {
        editor.executeAction("force_quit");
      } else {
        // Check if any buffer is modified
        const allBufs = editor.listBuffers() as Array<{ id: number; modified: boolean }>;
        const anyModified = allBufs.some((b) => b.modified);
        if (anyModified) {
          return { error: editor.t("error.no_write_since_change", { cmd: ":qa!" }) };
        }
        editor.executeAction("force_quit");
      }
      return {};
    }

    case "wqall": {
      // :wqa or :xa - save all and quit
      editor.executeAction("save_all");
      editor.executeAction("force_quit");
      return {};
    }

    case "edit": {
      // :e - reload current file
      // :e filename - open file
      // :e! - force reload (discard changes)
      if (!args) {
        if (force) {
          editor.executeAction("revert");
          return { message: editor.t("status.file_reverted_discarded") };
        }
        const bufferId = editor.getActiveBufferId();
        if (editor.isBufferModified(bufferId)) {
          return { error: editor.t("error.no_write_since_change", { cmd: ":e!" }) };
        }
        editor.executeAction("revert");
        return { message: editor.t("status.file_reverted") };
      }
      // Open the specified file
      const path = args.trim();
      editor.openFile(path, 0, 0);
      return {};
    }

    case "enew": {
      // :enew - create new buffer in current split
      if (!force) {
        const bufferId = editor.getActiveBufferId();
        if (editor.isBufferModified(bufferId)) {
          return { error: editor.t("error.no_write_since_change", { cmd: ":enew!" }) };
        }
      }
      editor.executeAction("new_buffer");
      return {};
    }

    case "revert": {
      // :revert - Fresh-specific command to reload file
      editor.executeAction("revert");
      return { message: editor.t("status.file_reverted") };
    }

    case "next": {
      // :n - next buffer
      editor.executeAction("next_buffer");
      return {};
    }

    case "previous": {
      // :prev - previous buffer
      editor.executeAction("prev_buffer");
      return {};
    }

    case "bnext": {
      // :bn - next buffer
      editor.executeAction("next_buffer");
      return {};
    }

    case "bprevious": {
      // :bp - previous buffer
      editor.executeAction("prev_buffer");
      return {};
    }

    case "bdelete": {
      // :bd - delete buffer (close)
      // :bd! - force close even if modified
      const bufferId = editor.getActiveBufferId();
      if (!force && editor.isBufferModified(bufferId)) {
        return { error: editor.t("error.no_write_since_change", { cmd: ":bd!" }) };
      }
      editor.executeAction("close");
      return {};
    }

    case "buffer": {
      // :b [N] - go to buffer N
      // :b name - go to buffer matching name
      if (!args) {
        // Show current buffer info
        const bufferId = editor.getActiveBufferId();
        const info = editor.getBufferInfo(bufferId);
        if (info) {
          const name = info.path ? editor.pathBasename(info.path) : editor.t("info.no_name");
          return { message: editor.t("info.buffer", { id: String(info.id), name }) };
        }
        return {};
      }
      // Try to parse as buffer number
      const bufNum = parseInt(args.trim(), 10);
      if (!isNaN(bufNum)) {
        const buffers = editor.listBuffers();
        const target = buffers.find(b => b.id === bufNum);
        if (target) {
          editor.showBuffer(target.id);
          return {};
        }
        return { error: editor.t("error.buffer_not_found", { id: String(bufNum) }) };
      }
      // Try to match buffer by name
      const buffers = editor.listBuffers();
      const pattern = args.trim().toLowerCase();
      const matches = buffers.filter(b => {
        const name = b.path ? editor.pathBasename(b.path).toLowerCase() : "";
        return name.includes(pattern);
      });
      if (matches.length === 1) {
        editor.showBuffer(matches[0].id);
        return {};
      } else if (matches.length > 1) {
        return { error: editor.t("error.multiple_buffers_match", { pattern: args }) };
      }
      return { error: editor.t("error.no_buffer_matching", { pattern: args }) };
    }

    case "buffers":
    case "ls":
    case "files": {
      // :ls - list buffers
      const buffers = editor.listBuffers();
      const lines = buffers.map(buf => {
        const modified = buf.modified ? " [+]" : "";
        const current = buf.id === editor.getActiveBufferId() ? "%" : " ";
        const name = buf.path ? editor.pathBasename(buf.path) : editor.t("info.no_name");
        return `${current}${buf.id}: ${name}${modified}`;
      });
      return { message: lines.join(" | ") || editor.t("info.no_buffers") };
    }

    case "split": {
      // :sp - horizontal split
      editor.executeAction("split_horizontal");
      if (args) {
        // Open file in new split
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "vsplit": {
      // :vs - vertical split
      editor.executeAction("split_vertical");
      if (args) {
        // Open file in new split
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "new": {
      // :new - create new buffer in horizontal split
      editor.executeAction("split_horizontal");
      editor.executeAction("new_buffer");
      if (args) {
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "vnew": {
      // :vnew - create new buffer in vertical split
      editor.executeAction("split_vertical");
      editor.executeAction("new_buffer");
      if (args) {
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "only": {
      // :only - close all other splits
      editor.executeAction("close_other_splits");
      return {};
    }

    case "close": {
      // :close - close current split (same as :q for Fresh)
      const bufferId = editor.getActiveBufferId();
      if (!force && editor.isBufferModified(bufferId)) {
        return { error: editor.t("error.no_write_since_change", { cmd: ":close!" }) };
      }
      editor.executeAction("close");
      return {};
    }

    case "tabnew":
    case "tabedit": {
      // :tabnew - new tab (creates new buffer in Fresh)
      editor.executeAction("new_buffer");
      if (args) {
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "tabclose": {
      // :tabclose - close current tab/buffer
      const bufferId = editor.getActiveBufferId();
      if (!force && editor.isBufferModified(bufferId)) {
        return { error: editor.t("error.no_write_since_change", { cmd: ":tabclose!" }) };
      }
      editor.executeAction("close");
      return {};
    }

    case "tabnext": {
      // :tabnext - next tab/buffer
      editor.executeAction("next_buffer");
      return {};
    }

    case "tabprevious": {
      // :tabprev - previous tab/buffer
      editor.executeAction("prev_buffer");
      return {};
    }

    case "copen": {
      // :copen - open diagnostics panel (Fresh equivalent)
      editor.executeAction("show_diagnostics");
      return {};
    }

    case "cclose": {
      // :cclose - close diagnostics panel
      return { message: editor.t("info.close_diagnostics") };
    }

    case "cnext": {
      // :cnext - next diagnostic
      editor.executeAction("goto_next_diagnostic");
      return {};
    }

    case "cprevious": {
      // :cprev - previous diagnostic
      editor.executeAction("goto_prev_diagnostic");
      return {};
    }

    case "cfirst": {
      // :cfirst - first diagnostic
      editor.executeAction("goto_first_diagnostic");
      return {};
    }

    case "clast": {
      // :clast - last diagnostic
      editor.executeAction("goto_last_diagnostic");
      return {};
    }

    case "nohlsearch": {
      // :noh - clear search highlighting
      editor.executeAction("clear_search");
      return {};
    }

    case "substitute": {
      // :s - substitute (not implemented)
      // This would require parsing /pattern/replacement/flags
      return { error: editor.t("error.substitute_not_implemented") };
    }

    case "global":
    case "vglobal": {
      // :g - global command (not implemented)
      return { error: editor.t("error.global_not_implemented") };
    }

    case "undo": {
      // :undo - undo
      editor.executeAction("undo");
      return {};
    }

    case "redo": {
      // :redo - redo
      editor.executeAction("redo");
      return {};
    }

    case "set": {
      // :set - set options (limited implementation)
      if (!args) {
        return { error: editor.t("error.set_usage") };
      }
      return handleSetCommand(args);
    }

    case "pwd": {
      // :pwd - print working directory
      const cwd = editor.getCwd();
      return { message: cwd };
    }

    case "cd": {
      // :cd - change directory (info only, can't actually change)
      if (!args) {
        return { message: editor.getCwd() };
      }
      return { error: editor.t("error.cannot_change_directory") };
    }

    case "file": {
      // :f - show current file info
      // :f name - rename current buffer (not implemented)
      if (args) {
        return { error: editor.t("error.rename_not_implemented") };
      }
      const bufferId = editor.getActiveBufferId();
      const info = editor.getBufferInfo(bufferId);
      if (info) {
        const modified = info.modified ? editor.t("info.modified") : "";
        const path = info.path || editor.t("info.no_name");
        const line = editor.getPrimaryCursor()?.line ?? 0;
        return { message: editor.t("info.file", { path, modified, line: String(line), bytes: String(info.length) }) };
      }
      return { error: editor.t("error.no_buffer") };
    }

    case "help": {
      // :help - show help
      if (args) {
        return { message: editor.t("info.help_not_available", { topic: args }) };
      }
      return {
        message: editor.t("info.help_commands")
      };
    }

    case "version": {
      // :version - show version
      return { message: editor.t("info.version") };
    }

    case "marks": {
      // :marks - show marks (not implemented)
      return { error: editor.t("error.marks_not_implemented") };
    }

    case "registers": {
      // :registers - show registers (not implemented)
      return { error: editor.t("error.registers_not_implemented") };
    }

    case "jumps": {
      // :jumps - show jump list (not implemented)
      return { error: editor.t("error.jump_list_not_implemented") };
    }

    case "syntax": {
      // :syntax - syntax info
      if (args === "off") {
        return { error: editor.t("error.syntax_cannot_disable") };
      }
      return { message: editor.t("status.syntax_always_on") };
    }

    case "read": {
      // :r - read file into buffer (not implemented)
      return { error: editor.t("error.read_not_implemented") };
    }

    case "saveas": {
      // :saveas - save as (not implemented)
      return { error: editor.t("error.saveas_not_implemented") };
    }

    case "grep":
    case "vimgrep": {
      // :grep - search (use Fresh's grep)
      if (args) {
        // Could potentially pass args to search, but for now just open search
        editor.executeAction("search");
        return { message: editor.t("info.use_search_dialog", { pattern: args }) };
      }
      editor.executeAction("search");
      return {};
    }

    case "make": {
      // :make - run build command (not implemented)
      return { error: editor.t("error.use_terminal") };
    }

    case "ascii": {
      // :ascii - show ASCII value of char under cursor
      return { message: editor.t("info.status_bar_char") };
    }

    default: {
      return { error: editor.t("error.unknown_command", { cmd: command }) };
    }
  }
}

// Go to a specific line number
async function gotoLine(lineNum: number): Promise<CommandResult> {
  if (lineNum < 1) {
    return { error: editor.t("error.line_must_be_positive") };
  }

  const bufferId = editor.getActiveBufferId();
  const bufferLength = editor.getBufferLength(bufferId);

  // Get the text to find the line offset
  const text = await editor.getBufferText(bufferId, 0, bufferLength);
  if (!text) {
    return { error: editor.t("error.cannot_read_buffer") };
  }

  let lineStart = 0;
  let currentLine = 1;

  for (let i = 0; i < text.length && currentLine < lineNum; i++) {
    if (text[i] === '\n') {
      currentLine++;
      lineStart = i + 1;
    }
  }

  if (currentLine >= lineNum || lineStart < text.length) {
    editor.setBufferCursor(bufferId, lineStart);
    return {};
  }

  // If requested line is beyond file, go to last line
  editor.executeAction("move_document_end");
  return { message: editor.t("status.line_beyond_end", { line: String(lineNum) }) };
}

// Handle :set command options
function handleSetCommand(args: string): CommandResult {
  const parts = args.split("=");
  const option = parts[0].trim();
  const value = parts.length > 1 ? parts[1].trim() : null;

  switch (option) {
    case "number":
    case "nu": {
      // :set number - show line numbers
      const bufferId = editor.getActiveBufferId();
      editor.setLineNumbers(bufferId, true);
      return { message: editor.t("status.line_numbers_on") };
    }

    case "nonumber":
    case "nonu": {
      // :set nonumber - hide line numbers
      const bufferId = editor.getActiveBufferId();
      editor.setLineNumbers(bufferId, false);
      return { message: editor.t("status.line_numbers_off") };
    }

    case "wrap": {
      // :set wrap - enable line wrap
      editor.executeAction("toggle_wrap");
      return { message: editor.t("status.line_wrap_toggled") };
    }

    case "nowrap": {
      // :set nowrap - disable line wrap
      editor.executeAction("toggle_wrap");
      return { message: editor.t("status.line_wrap_toggled") };
    }

    default: {
      return { error: editor.t("error.unknown_option", { option }) };
    }
  }
}

// Register event handler for prompt confirmation
editor.on("prompt_confirmed", async (args) => {
  if (args.prompt_type !== "vi-command") {
    return false; // Not our prompt, let other handlers process it
  }

  const input = args.input.trim();
  if (!input) {
    return true; // Empty command, just dismiss
  }

  // Parse the command
  const result = await executeViCommand(input);

  if (result.error) {
    editor.setStatus(`E: ${result.error}`);
  } else if (result.message) {
    editor.setStatus(result.message);
  }

  return true; // We handled it
});

// ============================================================================
// Toggle Command
// ============================================================================

let viModeEnabled = false;

function enableVi(): void {
  if (viModeEnabled) return;
  viModeEnabled = true;
  switchMode("normal");
  editor.setStatus(editor.t("status.enabled"));
}

function disableVi(): void {
  if (!viModeEnabled) return;
  viModeEnabled = false;
  editor.setEditorMode(null);
  state.mode = "normal";
  state.pendingOperator = null;
  editor.setStatus(editor.t("status.disabled"));
}

function vi_mode_toggle(): void {
  if (viModeEnabled) disableVi();
  else enableVi();
}
registerHandler("vi_mode_toggle", vi_mode_toggle);

editor.registerCommand(
  "%cmd.toggle_vi_mode",
  "%cmd.toggle_vi_mode_desc",
  "vi_mode_toggle",
  null,  // Always visible - needed to enable vi mode in the first place
);

export type ViModeApi = {
  toggle(): void;
  enable(): void;
  disable(): void;
  isEnabled(): boolean;
};

declare global {
  interface FreshPluginRegistry {
    "vi-mode": ViModeApi;
  }
}

editor.exportPluginApi("vi-mode", {
  toggle: vi_mode_toggle,
  enable: enableVi,
  disable: disableVi,
  isEnabled: () => viModeEnabled,
} satisfies ViModeApi);

// ============================================================================
// Initialization
// ============================================================================

if (autoStart) {
  enableVi();
}

registerHandler("vi_to_brace", vi_to_brace);
