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

// What `.` replays.
//
// Every variant can carry a count and the text typed into the insert that
// followed it, so those two stay common. The rest is per-variant: as a single
// interface with everything optional, each branch of the replay had to
// re-check the fields its own variant already guarantees, and any variant
// could be built missing the fields it needs.
interface ChangeCommon {
  count?: number; // Count used with the command
  insertedText?: string; // Text typed into the insert that followed, if any
}

type LastChange =
  // x, X, s
  | (ChangeCommon & { type: "simple"; action: string })
  // dd, cc, >>, <<
  | (ChangeCommon & { type: "line-op"; action: string })
  // dw, ce, d}, D, C
  | (ChangeCommon & { type: "operator-motion"; operator: string; motion: string })
  // diw, ci"
  | (ChangeCommon & {
      type: "operator-textobj";
      operator: string;
      textObject: { modifier: TextObjectType; object: string };
    })
  // dfx, dtx, cfx
  | (ChangeCommon & {
      type: "operator-find-char";
      operator: string;
      findType: FindCharType;
      findCharTarget: string;
    })
  // rx
  | (ChangeCommon & { type: "replace-char"; replacement: string })
  // i/a/I/A/o/O, and a bare insert with no entering command recorded
  | (ChangeCommon & { type: "insert"; insertCommand?: string });

// One insert session: where the insert began and which command opened it.
// The two are written at different moments — the entering command stages
// itself in `pendingInsertCommand`, the position is sampled inside
// `switchMode` — but they are only ever *read* together, by the capture at
// Escape time. As one value the pairing is in the type: a session cannot be
// built with a start and no command, and the capture takes both or neither.
interface InsertSession {
  startPos: number; // Cursor position when insert mode was entered
  command: string | null; // i/a/I/A/o/O, or null when entered via c/s
}

// The active visual selection, or null outside the visual modes. `anchor` is
// the fixed end and `head` the moving one; `range` is the byte range computed
// by motions that resolve their own target (for the rest, the host's own
// selection is the range). `head` goes null when a computed range is dropped,
// which makes the motion origin fall back to the anchor.
//
// These four were separate nullable fields, but they are established
// together on entering a visual mode and cleared together on leaving it —
// there is no state in which some are meaningful and others are not. As one
// value that invariant is in the type instead of in four call sites.
interface VisualState {
  anchor: number;
  head: number | null;
  range: { start: number; end: number } | null;
  // The line-wise span, as the anchor's and the moving end's line numbers.
  //
  // `V` has to be able to grow its selection *upward* from a fixed anchor, and
  // the host's `select_*` actions cannot express that: `select_up` from a
  // selection that already ends at a line start shrinks it to nothing, which
  // is why `Vk` used to select no lines at all. So the line-wise mode keeps
  // its span here and redraws the whole selection on every motion. `anchor` is
  // sampled on entering a visual mode (the caret is on it at that moment), so
  // `v` carries it too and `v` -> `V` can pick it up.
  lines: { anchor: number; head: number } | null;
}

// An operator waiting for the range it will consume. `d`, `c`, `y`, `>` and
// `<` establish one; a following `i`/`a` fills in `textObject`. The two were
// separate fields with the same lifetime — both cleared by the same
// `switchMode` transitions — so an operator with a modifier but no operator,
// or the reverse, was representable but meaningless.
interface PendingOperator {
  operator: string; // d, c, y, >, <
  textObject: TextObjectType; // set once i/a is typed
}

// What the *next* command will replay: `.`, `;`/`,`, `n`/`N` after `*`, and
// the shape `p` pastes in. None of it belongs to a mode — it is written by
// one command and read by another, arbitrarily later — so it is deliberately
// not part of `ViState` below, which is the state the current mode owns and
// `switchMode` is free to clear.
interface ViMemory {
  lastChange: LastChange | null; // For '.' repeat
  lastFindChar: { type: FindCharType; char: string } | null; // For ; and , repeat
  lastWordSearch: { text: string; direction: WordSearchDirection; wholeWord: boolean } | null; // For n/N after * or #
  lastYankWasLinewise: boolean; // Track if last yank was line-wise for proper paste
}

// The state the current mode owns: established on entering a mode and cleared
// on leaving it. `switchMode` is where that happens for every ordinary
// transition; the few sites that set `state.mode` directly each say why they
// cannot use it. `pendingFindChar` is the exception to the clearing — it is
// staged by the handler that awaits the key, not by a transition.
interface ViState {
  mode: ViMode;
  pending: PendingOperator | null; // The operator awaiting a range; null when none
  pendingFindChar: FindCharType; // For f/t/F/T motions
  count: number | null;
  visual: VisualState | null; // The active visual selection; null outside the visual modes
  insert: InsertSession | null; // The in-progress insert session; null outside insert mode
  pendingInsertCommand: string | null; // Insert-entering command (i/a/I/A/o/O) staged for the next switchMode("insert")
  // The most recent `captureInsertedText`, settled or not.
  //
  // Leaving insert mode starts that capture, and it has to `await` a buffer
  // read before it can put the typed text on `memory.lastChange`. Nothing used
  // to wait for it, so a `.` arriving before the read resolved replayed a
  // change whose `insertedText` was not filled in yet — the delete happened
  // and the insert did not. `Esc j j .` is fast enough to lose that race on a
  // loaded machine.
  //
  // Never cleared: awaiting a promise that already settled costs nothing, and
  // clearing it from inside the capture would wipe the handle to a *newer*
  // capture that started while this one was still awaiting its buffer read.
  pendingCapture: Promise<void> | null;
}

const memory: ViMemory = {
  lastChange: null,
  lastFindChar: null,
  lastWordSearch: null,
  lastYankWasLinewise: false,
};

function recordChange(change: LastChange): void {
  memory.lastChange = change;
}

const state: ViState = {
  mode: "normal",
  pending: null,
  pendingFindChar: null,
  count: null,
  visual: null,
  insert: null,
  pendingInsertCommand: null,
  pendingCapture: null,
};

// Bumped every time the modal state is dropped out from under whatever was
// using it. `f`/`t`/`F`/`T`, `d f` and `r` copy their operator and count into
// locals and then await a keypress; if the state is reset during that await —
// a buffer switch, vi being turned off — the awaiting handler must abandon
// the command instead of applying the old buffer's operator to the new one.
let modalGeneration = 0;

// Return the modal state to what it is on a fresh normal mode. `memory` is
// untouched: what `.` and `;` replay is not a property of any mode.
function resetModalState(): void {
  modalGeneration += 1;
  state.mode = "normal";
  state.pending = null;
  state.pendingFindChar = null;
  state.count = null;
  state.visual = null;
  state.insert = null;
  state.pendingInsertCommand = null;
}

const autoStart = editor.defineConfigBoolean("autoStart", {
  default: false,
  description:
    "Automatically enable vi mode when the editor starts. Default off — users opt in.",
});

// `let`, not `const`: both feed the mode binding tables built in
// `defineViModes()`, and the `config_changed` subscription at the bottom
// of this file re-reads them and re-emits the modes so a Settings change
// takes effect without an editor restart.
let arrowKeys = editor.defineConfigBoolean("arrowKeys", {
  default: true,
  description:
    "Enable arrow key navigation in vi mode.",
});

let searchWordUnderCursor = editor.defineConfigBoolean("searchWordUnderCursor", {
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
      return `-- ${editor.t("mode.operator")} (${state.pending?.operator ?? ""}) --${countPrefix ? ` (${state.count})` : ""}`;
    case "find-char":
      return `-- ${editor.t("mode.find")} (${state.pendingFindChar}) --`;
    case "visual":
      return `-- ${editor.t("mode.visual")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "visual-line":
      return `-- ${editor.t("mode.visual_line")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "visual-block":
      return `-- ${editor.t("mode.visual_block")} --${countPrefix ? ` (${state.count})` : ""}`;
    case "text-object":
      return `-- ${state.pending?.operator ?? ""}${state.pending?.textObject === "inner" ? "i" : "a"}? --`;
    default:
      return "";
  }
}

// Switch between modes
function switchMode(newMode: ViMode): void {
  const oldMode = state.mode;
  state.mode = newMode;

  // The pending operator outlives only operator-pending and text-object mode;
  // its text-object modifier outlives only text-object mode.
  if (newMode !== "operator-pending" && newMode !== "text-object") {
    state.pending = null;
  } else if (newMode !== "text-object" && state.pending !== null) {
    state.pending.textObject = null;
  }

  // Preserve count when entering operator-pending or text-object mode (for 3dw = delete 3 words)
  // Also preserve count in visual modes
  if (newMode !== "operator-pending" && newMode !== "text-object" &&
      newMode !== "visual" && newMode !== "visual-line" && newMode !== "visual-block") {
    state.count = null;
  }

  // Drop the visual selection when leaving visual modes
  if (newMode !== "visual" && newMode !== "visual-line" && newMode !== "visual-block") {
    state.visual = null;
    // Clear any selection when leaving visual mode by moving cursor
    // (any non-select movement clears selection in Fresh)
    if (oldMode === "visual" || oldMode === "visual-line" || oldMode === "visual-block") {
      editor.executeAction("move_left");
      editor.executeAction("move_right");
    }
  }

  // Open the insert session for '.' repeat, pairing the start position with
  // whichever command staged itself in `pendingInsertCommand`.
  if (newMode === "insert" && oldMode !== "insert") {
    state.insert = {
      startPos: editor.getCursorPosition(),
      command: state.pendingInsertCommand,
    };
    state.pendingInsertCommand = null;
  }

  // Capture inserted text when leaving insert mode (for '.' repeat)
  if (oldMode === "insert" && newMode !== "insert" && state.insert !== null) {
    // Deliberately not awaited here — `switchMode` is called from
    // synchronous paths and the mode must flip now. The promise is kept so
    // that whoever *reads* what the capture produces can wait for it; see
    // `state.pendingCapture`.
    state.pendingCapture = captureInsertedText();
  }

  // All modes use vi-{mode} naming, including insert mode
  // vi-insert has read_only=false so normal typing works, but Escape is bound
  editor.setEditorMode(`vi-${newMode}`);
  editor.setStatus(getModeIndicator(newMode));
}

// Capture text inserted during insert mode for '.' repeat
async function captureInsertedText(): Promise<void> {
  // Take the session up front, before the `await` below yields. Clearing it
  // here is what makes a second capture a no-op rather than a second reading
  // of the same span.
  const session = state.insert;
  if (session === null) return;
  state.insert = null;

  const startPos = session.startPos;
  const insertCommand = session.command;

  const endPos = editor.getCursorPosition();
  let text = "";
  if (endPos !== null && endPos > startPos) {
    const bufferId = editor.getActiveBufferId();
    text = (await editor.getBufferText(bufferId, startPos, endPos)) ?? "";
  }

  if (insertCommand !== null) {
    // Insert entered via i/a/I/A/o/O: '.' replays the command's cursor
    // motion plus the typed text. An empty i/a/I/A insert is not a change
    // (Vim keeps the previous one for '.'), but o/O open a line even when
    // nothing is typed, which alone is repeatable.
    if (text.length > 0 || insertCommand === "o" || insertCommand === "O") {
      memory.lastChange = { type: "insert", insertCommand };
      if (text.length > 0) {
        memory.lastChange.insertedText = text;
      }
    }
    return;
  }

  if (text.length > 0) {
    if (!memory.lastChange || memory.lastChange.type === "insert") {
      memory.lastChange = {
        type: "insert",
        insertedText: text,
      };
    } else {
      // A change command (c, s, etc.) was used - append the inserted text
      memory.lastChange.insertedText = text;
    }
  }
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

// The word motions, named once each.
//
// `w`, `b`, `e` and their WORD counterparts were previously written three
// times over — once for normal mode, once for operator-pending, once for
// visual — and the two families were written twice more on top of that, the
// lowercase one by composing editor actions and the uppercase one by computing
// byte offsets. Only the computing version knew Vim's awkward rules (that `dw`
// on a line's last word stops at the line end rather than eating the newline,
// that `cw` behaves like `ce`), so the lowercase motions quietly got them
// wrong.
//
// Here a family is a triple of pure index functions, a *kind* says how a
// target becomes a range, and the three modes are adapters over both.
type WordFamily = "word" | "WORD";

interface WordFamilyIndexes {
  forward: (text: string, index: number) => number;
  backward: (text: string, index: number) => number;
  end: (text: string, index: number) => number;
  // The end of the run the caret is already in, used by the `cw`-is-`ce` rule.
  // Distinct from `end`, which advances to the next word when the caret is
  // already on a word's last character.
  runEnd: (text: string, index: number) => number;
}

const WORD_FAMILIES: Record<WordFamily, WordFamilyIndexes> = {
  word: {
    forward: nextWordIndex,
    backward: previousWordIndex,
    end: endWordIndex,
    runEnd: tokenRunEnd,
  },
  WORD: {
    forward: nextWORDIndex,
    backward: previousWORDIndex,
    end: endWORDIndex,
    runEnd: endWORDIndex,
  },
};

// `e`/`E` land *on* the last character they cover, so an operator's range runs
// one character past the target; `w`/`W`/`b`/`B` land just after, which is
// where the range ends anyway.
const WORD_MOTION_KIND: Record<WORDMotionKind, MotionKind> = {
  forward: "exclusive",
  backward: "exclusive",
  end: "inclusive",
};

// The range an operator takes when a motion starting at `origin` lands on
// `target`. Used by the motion families that resolve a plain target; the word
// motions go through `computeWordOperatorRange` below, which also carries
// Vim's end-of-line special cases.
async function rangeFromMotion(
  origin: number,
  target: number,
  kind: MotionKind,
): Promise<OperatorRange | null> {
  const bufferId = editor.getActiveBufferId();
  if (kind === "linewise") {
    const span = await lineSpanOfRange(bufferId, Math.min(origin, target), Math.max(origin, target));
    return linewiseRangeAt(bufferId, span.firstLineStart, span.lineCount);
  }
  if (kind === "inclusive" && target >= origin) {
    return charwiseRange(origin, await charEndOffset(bufferId, target));
  }
  return charwiseRange(origin, target);
}

// The motions an operator can take, each resolving to a target byte offset,
// each with the kind that turns that target into a range.
//
// This is the last of the three command paths to be folded in. The word
// motions and the text objects already resolved their own ranges; these were
// still composing the host's `select_*` actions and consuming whatever
// selection came out, which is why `d%` silently did nothing (no selection
// equivalent existed) and why the inclusive/exclusive distinction had to be
// re-derived at each site that cared.
interface OperatorMotion {
  kind: MotionKind;
  // Resolved against the *operator's* semantics, which are not always the
  // caret's: `l` will not move the caret past a line's last character, but
  // `dl` deletes that character.
  resolve: (origin: number, count: number) => Promise<number | null>;
}

async function lineStartTarget(origin: number): Promise<number | null> {
  return findLineStartAtPosition(editor.getActiveBufferId(), origin);
}

async function lineEndTarget(origin: number): Promise<number | null> {
  const line = editor.getPrimaryCursor()?.line ?? null;
  if (line === null) {
    return null;
  }
  const end = await editor.getLineEndPosition(line);
  return end === null ? null : Math.max(end, origin);
}

async function paragraphTarget(origin: number, count: number, forward: boolean): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const text = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  let index = byteOffsetToStringIndex(text, origin);
  if (!forward) {
    for (let i = 0; i < Math.max(1, count); i++) {
      index = paragraphUpMotionTargetIndex(text, index);
    }
    return stringIndexToByteOffset(text, index);
  }
  let reachedEof = false;
  for (let i = 0; i < Math.max(1, count); i++) {
    const next = paragraphDownMotionTargetIndex(text, index);
    index = next.index;
    reachedEof = next.reachedEof;
  }
  // At the end of the buffer the motion stops *on* the last character rather
  // than at a paragraph break, so the range has to reach past it.
  return stringIndexToByteOffset(text, reachedEof ? nextStringIndex(text, index) : index);
}

const OPERATOR_MOTIONS: Record<string, OperatorMotion> = {
  move_left: { kind: "exclusive", resolve: (o, c) => horizontalTarget(o, c, false, false) },
  move_right: { kind: "exclusive", resolve: (o, c) => horizontalTarget(o, c, true, false) },
  move_line_start: { kind: "exclusive", resolve: (o) => lineStartTarget(o) },
  // `$` is inclusive in Vim's vocabulary, but the editor's line end already
  // sits one past the last character, which is where an exclusive range ends.
  move_line_end: { kind: "exclusive", resolve: (o) => lineEndTarget(o) },
  move_to_paragraph_up: { kind: "exclusive", resolve: (o, c) => paragraphTarget(o, c, false) },
  move_to_paragraph_down: { kind: "exclusive", resolve: (o, c) => paragraphTarget(o, c, true) },
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

// Vim `w`: the start of the next word. Unlike `W`, a run of word characters
// and a run of punctuation are separate words, so `w` stops between them.
function nextWordIndex(text: string, startIndex: number): number {
  let index = Math.min(startIndex, text.length);
  if (index >= text.length) {
    return text.length;
  }

  if (!isWhitespaceAt(text, index)) {
    const startedOnWord = isWordChar(charAtStringIndex(text, index));
    while (
      index < text.length &&
      !isWhitespaceAt(text, index) &&
      isWordChar(charAtStringIndex(text, index)) === startedOnWord
    ) {
      index = nextStringIndex(text, index);
    }
  }

  while (index < text.length && isWhitespaceAt(text, index)) {
    // An empty line is a word of its own to Vim, so `w` stops on it.
    if (isEmptyLineStart(text, index)) {
      break;
    }
    index = nextStringIndex(text, index);
  }
  return index;
}

// Vim `e`: the last character of the current word, or of the next one when the
// caret is already on it. `viWordEndAdvance` is that rule exactly — it was
// written for the trailing words of a `cNw` and is reused rather than copied.
function endWordIndex(text: string, startIndex: number): number {
  return viWordEndAdvance(text, startIndex);
}

// Vim `b`: the start of the word before `startIndex`. Unlike `B` above, a run
// of word characters and a run of punctuation are separate words.
function previousWordIndex(text: string, startIndex: number): number {
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
  if (isWhitespaceAt(text, index)) {
    return index;
  }

  const startedOnWord = isWordChar(charAtStringIndex(text, index));
  while (index > 0) {
    const previous = previousStringIndex(text, index);
    if (isWhitespaceAt(text, previous)) {
      break;
    }
    if (isWordChar(charAtStringIndex(text, previous)) !== startedOnWord) {
      break;
    }
    index = previous;
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

function computeWordMotionTargetIndex(
  family: WordFamily,
  text: string,
  startIndex: number,
  kind: WORDMotionKind,
  count: number,
): number {
  const step = WORD_FAMILIES[family][kind];
  let index = startIndex;
  for (let i = 0; i < Math.max(1, count); i++) {
    index = step(text, index);
  }
  return index;
}

async function computeWordMotionTarget(
  family: WordFamily,
  kind: WORDMotionKind,
  count: number,
  origin: number | null = null,
): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = origin ?? editor.getCursorPosition();
  if (cursorPos === null) {
    return null;
  }

  const bufferLength = editor.getBufferLength(bufferId);
  const text = await editor.getBufferText(bufferId, 0, bufferLength);
  const index = computeWordMotionTargetIndex(family, text, byteOffsetToStringIndex(text, cursorPos), kind, count);

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

// `{`: the previous empty line, or the start of the buffer. Mirrors
// `paragraphDownMotionTargetIndex` below.
function paragraphUpMotionTargetIndex(text: string, startIndex: number): number {
  let lineStart = lineStartIndex(text, Math.min(startIndex, text.length));
  while (lineStart > 0) {
    lineStart = lineStartIndex(text, previousStringIndex(text, lineStart));
    const nextLineStart = nextLineStartIndex(text, lineStart);
    const lineContent = text.slice(lineStart, nextLineStart);
    if (lineContent.replace(/[\r\n]+$/, "") === "") {
      return lineStart;
    }
  }
  return 0;
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


function byteLengthOfCharAt(text: string, index: number): number {
  if (index < 0 || index >= text.length) {
    return 0;
  }
  const codePoint = text.codePointAt(index);
  return editor.utf8ByteLength(String.fromCodePoint(codePoint ?? text.charCodeAt(index)));
}

// Index of the last line that has content. A file ending in a newline has no
// line after it in Vim's model, but the editor still places a caret there.
async function lastContentLine(bufferId: number): Promise<number | null> {
  const lineCount = editor.getBufferInfo(bufferId)?.line_count ?? null;
  if (lineCount === null || lineCount <= 0) {
    return null;
  }
  const length = editor.getBufferLength(bufferId);
  let last = lineCount - 1;
  const start = await editor.getLineStartPosition(last);
  if (start !== null && start >= length && last > 0) {
    last -= 1;
  }
  return last;
}

// Motions Vim treats as *linewise*: an operator over one of these takes whole
// lines, newline included, not the byte span between the two carets. Without
// this, `dj` deleted the tail of one line and the head of the next, and `dG`
// deleted to the end of the buffer charwise.
const LINEWISE_MOTIONS: Record<string, true> = {
  move_down: true,
  move_up: true,
  move_document_start: true,
  move_document_end: true,
};

// The line span an operator+linewise-motion covers, as [firstLine, lineCount].
// `explicitCount` is the count the user actually typed, or null when there was
// none. The document motions need the difference: bare `dG` deletes to the end
// of the file, `d3G` deletes to line 3, and a count defaulted to 1 makes those
// two indistinguishable.
async function linewiseSpanForMotion(
  motionAction: string,
  count: number,
  explicitCount: number | null = null,
): Promise<{ firstLine: number; lineCount: number } | null> {
  const bufferId = editor.getActiveBufferId();
  const line = editor.getPrimaryCursor()?.line ?? null;
  if (line === null) {
    return null;
  }
  // Vim fails the whole operator when the motion cannot move: `dk` on the
  // first line and `dj` on the last change nothing at all. Clamping the span
  // instead would delete the current line, which is the opposite of a no-op.
  switch (motionAction) {
    case "move_down": {
      const last = await lastContentLine(bufferId);
      if (last === null || line + count > last) {
        return null;
      }
      return { firstLine: line, lineCount: count + 1 };
    }
    case "move_up": {
      if (line - count < 0) {
        return null;
      }
      return { firstLine: line - count, lineCount: count + 1 };
    }
    case "move_document_start": {
      // `dgg` goes to the first line, `d3gg` to line 3 (1-based).
      const target = Math.max(0, (explicitCount ?? 1) - 1);
      const first = Math.min(target, line);
      return { firstLine: first, lineCount: Math.abs(line - target) + 1 };
    }
    case "move_document_end": {
      const last = await lastContentLine(bufferId);
      if (last === null) {
        return null;
      }
      // Bare `dG` goes to the last line with content; `d3G` to line 3.
      const target = explicitCount === null ? last : Math.min(Math.max(0, explicitCount - 1), last);
      const first = Math.min(target, line);
      return { firstLine: first, lineCount: Math.abs(line - target) + 1 };
    }
    default:
      return null;
  }
}

// Apply an operator to whole lines. The linewise helpers all count forward
// from the caret's line, so a span that starts above the caret is handled by
// seating the caret on its first line first.
async function applyOperatorLinewise(
  operator: string,
  firstLine: number,
  lineCount: number,
): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const start = await editor.getLineStartPosition(firstLine);
  if (start === null) {
    switchMode("normal");
    return;
  }
  await applyOperator(operator, await linewiseRangeAt(bufferId, start, lineCount));
}

// ============================================================================
// Motions, ranges and operators
// ============================================================================
//
// Vim's model, which this plugin follows: a *motion* resolves to a target and
// a *kind*; the kind decides how the span between the caret and that target
// becomes a *range*; an *operator* consumes the range. The kind is the whole
// of the difference between `dw` and `de` (exclusive vs inclusive) and between
// `dw` and `dj` (charwise vs linewise).
//
// Keeping that as data rather than as code at each call site is the point.
// Before, "this motion is inclusive" was open-coded five separate times — a
// trailing `select_right` after `vi_select_word_end` in two helpers, another
// after `select_word_right` in visual `w`, another after `select_line_end` in
// visual `$`, and a `+ charLength` inside the find-char operator — and "this
// motion is line-wise" a sixth. Each site could disagree with the others, and
// several did.

type MotionKind = "exclusive" | "inclusive" | "linewise";

// The one distinction that outlives resolution. A line-wise range covers whole
// lines, registers as line-wise (so `p` puts it on its own lines rather than
// inline) and leaves an empty line behind under `c`.
type RangeShape = "charwise" | "linewise";

interface OperatorRange {
  start: number; // byte offset, inclusive
  end: number; // byte offset, exclusive
  shape: RangeShape;
  // Where to leave the caret afterwards. Defaults to `start`, which is right
  // for every operator except `yy` and friends, where Vim does not move it.
  cursorAfter?: number;
}

function charwiseRange(start: number, end: number, cursorAfter?: number): OperatorRange {
  return { start: Math.min(start, end), end: Math.max(start, end), shape: "charwise", cursorAfter };
}

// The line-wise range covering `lineCount` lines from the line `position` is on.
async function linewiseRangeAt(
  bufferId: number,
  position: number,
  lineCount: number,
  cursorAfter?: number,
): Promise<OperatorRange | null> {
  if (lineCount <= 0) {
    return null;
  }
  const start = await findLineStartAtPosition(bufferId, position);
  const end = await findLinewiseEndFromStart(bufferId, start, lineCount);
  if (end <= start) {
    return null;
  }
  return { start, end, shape: "linewise", cursorAfter };
}

// The single place `d`, `c`, `y`, `>` and `<` act on a range.
//
// Every entry point — operator + motion, operator + text object, operator +
// find-char, the doubled line operators, and the visual modes — resolves its
// own range and hands it here. That is what keeps the register shape, the
// caret's landing place and the `c`-before-insert flush from drifting apart
// between them, which they did when there were four copies of this switch.
async function applyOperator(
  operator: string,
  range: OperatorRange | null,
  options: { enterInsert?: boolean } = {},
): Promise<void> {
  if (range === null || range.end <= range.start) {
    switchMode("normal");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  if (operator !== "y" && isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }

  // Indent and dedent are line-wise whatever the range's shape: they resolve it
  // to the whole lines it touches and shift those.
  if (operator === ">" || operator === "<") {
    const span = await lineSpanOfRange(bufferId, range.start, range.end);
    await applyIndentToLineRange(operator, span.firstLineStart, span.lineCount);
    return;
  }

  const linewise = range.shape === "linewise";
  const text = (await editor.getBufferText(bufferId, range.start, range.end)) ?? "";
  const terminator = linewise ? await getLinewiseTerminator(bufferId, range.start, text) : "";
  if (text) {
    editor.setClipboard(linewise ? ensureLinewiseRegisterText(text, terminator) : text);
  }
  memory.lastYankWasLinewise = linewise;

  // The caret is placed *after* the mode switch throughout: leaving a visual
  // mode, `switchMode` drops the host selection by nudging the caret left then
  // right, which is a net move at offset 0. Placing afterwards makes the
  // landing position below the authoritative one for every caller.
  switch (operator) {
    case "y": {
      // Vim leaves the caret at the start of the yanked text (`yb` from the
      // middle of a line lands where the yank began). `yy` and `Vy` are the
      // exceptions and pass their own `cursorAfter`.
      const landing = range.cursorAfter ?? range.start;
      switchMode("normal");
      editor.setBufferCursor(bufferId, landing);
      return;
    }

    case "d": {
      editor.deleteRange(bufferId, range.start, range.end);
      const landing = Math.min(range.cursorAfter ?? range.start, editor.getBufferLength(bufferId));
      switchMode("normal");
      editor.setBufferCursor(bufferId, landing);
      return;
    }

    case "c": {
      editor.deleteRange(bufferId, range.start, range.end);
      if (linewise) {
        // Vim's `cc`/`cj`/`Vc` leave an empty line to type into, rather than
        // closing the gap the way `d` does.
        editor.insertText(bufferId, range.start, getLinewiseReplacementText(text) ?? terminator);
      }
      editor.setBufferCursor(bufferId, range.start);
      // `.` replays a change as the delete half plus the recorded keystrokes,
      // so it wants the range consumed with `c` semantics — the empty line a
      // line-wise change leaves behind — without the mode switch.
      if (options.enterInsert === false) {
        await editor.flush();
        switchMode("normal");
        editor.setBufferCursor(bufferId, range.start);
        return;
      }
      // Flush before entering insert: `deleteRange` and `setBufferCursor` are
      // queued to the editor thread, so without this `switchMode("insert")`
      // samples the *pre-command* cursor as the session start. The Escape-time
      // capture then measures from there and records surrounding buffer text as
      // if it had been typed, which `.` replays into the next line (issue
      // #2443, the `c`-operator half).
      await editor.flush();
      switchMode("insert");
      editor.setBufferCursor(bufferId, range.start);
      if (state.insert !== null) {
        state.insert.startPos = range.start;
      }
      return;
    }
  }

  switchMode("normal");
}

// Charwise convenience wrapper: the many callers that already know both byte
// offsets and have nothing else to say about the range.
async function applyOperatorWithRange(operator: string, start: number, end: number): Promise<void> {
  await applyOperator(operator, charwiseRange(start, end));
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
async function firstNonBlankOffset(bufferId: number, lineStart: number): Promise<number> {
  const bufferLength = editor.getBufferLength(bufferId);
  const sampleEnd = Math.min(bufferLength, lineStart + 4096);
  const sample = await editor.getBufferText(bufferId, lineStart, sampleEnd);
  let index = 0;
  while (index < sample.length && (sample[index] === " " || sample[index] === "\t")) {
    index++;
  }
  const offset = lineStart + editor.utf8ByteLength(sample.slice(0, index));
  return Math.min(offset, bufferLength);
}

async function placeCursorAtFirstNonBlank(bufferId: number, lineStart: number): Promise<void> {
  editor.setBufferCursor(bufferId, await firstNonBlankOffset(bufferId, lineStart));
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

  memory.lastYankWasLinewise = false;
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


async function computeWordOperatorRange(
  family: WordFamily,
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

  // Vim's `cw` is `ce` (`:help cw`): on a non-blank, a change stops at the end
  // of the word instead of eating the whitespace after it. The first word uses
  // the family's `runEnd` so a caret already on a word's last character changes
  // only that character; each further count is an ordinary `e`.
  if (useChangeForwardSemantics && kind === "forward" && !isWhitespaceAt(bufferText, startIndex)) {
    let endIndex = WORD_FAMILIES[family].runEnd(bufferText, startIndex);
    for (let i = 1; i < Math.max(1, count); i++) {
      endIndex = WORD_FAMILIES[family].end(bufferText, endIndex);
    }
    const endTarget = stringIndexToByteOffset(bufferText, endIndex);
    return {
      start,
      end: endTarget + byteLengthOfCharAt(bufferText, endIndex),
    };
  }

  const targetIndex = computeWordMotionTargetIndex(family, bufferText, startIndex, kind, count);
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


async function applyWordOperatorMotion(
  family: WordFamily,
  operator: string,
  kind: WORDMotionKind,
  count: number,
  useChangeForwardSemantics: boolean = operator === "c",
): Promise<void> {
  const range = await computeWordOperatorRange(family, kind, count, useChangeForwardSemantics);
  if (range === null) {
    switchMode("normal");
    return;
  }

  await applyOperatorWithRange(operator, range.start, range.end);
  if (operator === "d" && range.cursorAfter !== undefined) {
    editor.setBufferCursor(editor.getActiveBufferId(), range.cursorAfter);
  }
}


// Normal mode: the motion just moves the caret.
async function moveByWordMotion(family: WordFamily, kind: WORDMotionKind): Promise<void> {
  const target = await computeWordMotionTarget(family, kind, consumeCount());
  if (target !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), target);
  }
}

// Visual mode: the head moves to the target, and the selection covers it.
//
// A visual selection always includes the character under its head, so the kind
// never enters into it here — which is why `vw` and `ve` each used to need a
// hand-written `select_right`/`select_left` to undo the adjustment the
// operator path had made for them.
async function extendVisualByWordMotion(family: WordFamily, kind: WORDMotionKind): Promise<void> {
  const target = await computeWordMotionTarget(family, kind, consumeCount(), visualWORDMotionOrigin());
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}

// Operator-pending mode: the target becomes a range, which the operator takes.
async function handleWordMotionWithOperator(family: WordFamily, kind: WORDMotionKind): Promise<void> {
  if (!state.pending) {
    switchMode("normal");
    return;
  }

  const operator = state.pending.operator;
  const count = consumeCount();
  if (operator === "d" || operator === "c" || operator === ">" || operator === "<") {
    recordChange({ type: "operator-motion", operator, motion: wordRepeatMotionName(family, kind), count });
  }

  await applyWordOperatorMotion(family, operator, kind, count);
}

// `.` records a word motion by name and decodes it back on replay, so the
// repeat re-resolves the motion at the new caret instead of replaying a range.
function wordRepeatMotionName(family: WordFamily, kind: WORDMotionKind): string {
  return `vi_word_motion_${family}_${kind}`;
}

function wordMotionFromRepeatMotion(motion: string): { family: WordFamily; kind: WORDMotionKind } | null {
  for (const family of ["word", "WORD"] as WordFamily[]) {
    for (const kind of ["forward", "backward", "end"] as WORDMotionKind[]) {
      if (motion === wordRepeatMotionName(family, kind)) {
        return { family, kind };
      }
    }
  }
  return null;
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
// `record` is false when `.` is driving this.
//
// A recorded `c` replays as its *delete* half followed by the captured text,
// so the commands the replay drives are handed `"d"`; letting them re-record
// would overwrite `ci"` with `di"`, and the next `.` would delete without
// inserting. This was a module-level flag held across the replay's `await`s,
// which silently swallowed the recording of anything that ran in one of those
// windows. Only three functions record on a replay path, so each is told
// directly instead.
async function applyOperatorWithMotion(
  operator: string,
  motionAction: string,
  count: number = 1,
  explicitCount: number | null = null,
  record: boolean = true,
): Promise<void> {
  const records = record && (operator === "d" || operator === "c");

  if (LINEWISE_MOTIONS[motionAction]) {
    const span = await linewiseSpanForMotion(motionAction, count, explicitCount);
    if (span === null) {
      // The motion could not move — `dk` on the first line, `dj` on the last —
      // so the whole operator fails and `.` keeps whatever it was holding.
      // Recording before this point overwrote it with a change that never
      // happened.
      switchMode("normal");
      return;
    }
    if (records) {
      recordChange({ type: "operator-motion", operator, motion: motionAction, count: explicitCount ?? undefined });
    }
    await applyOperatorLinewise(operator, span.firstLine, span.lineCount);
    return;
  }

  const motion = OPERATOR_MOTIONS[motionAction];
  if (motion === undefined) {
    editor.debug(`No operator motion for: ${motionAction}`);
    switchMode("normal");
    return;
  }

  const origin = editor.getCursorPosition();
  const target = origin === null ? null : await motion.resolve(origin, count);
  if (origin === null || target === null) {
    switchMode("normal");
    return;
  }

  // Record last change for '.' repeat (only for delete and change, not yank),
  // and only once the motion has resolved: a motion that cannot move fails the
  // whole operator, and must not overwrite what `.` is holding on its way out.
  if (records) {
    recordChange({ type: "operator-motion", operator, motion: motionAction, count });
  }

  await applyOperator(operator, await rangeFromMotion(origin, target, motion.kind));
}

// Handle motion in operator-pending mode
// Consumes any pending count and applies it to the motion
async function handleMotionWithOperator(motionAction: string): Promise<void> {
  if (!state.pending) {
    switchMode("normal");
    return;
  }

  const operator = state.pending.operator;
  const explicitCount = state.count;
  const count = consumeCount();
  await applyOperatorWithMotion(operator, motionAction, count, explicitCount);
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

async function vi_word() : Promise<void> {
  await moveByWordMotion("word", "forward");
}
registerHandler("vi_word", vi_word);

async function vi_word_back() : Promise<void> {
  await moveByWordMotion("word", "backward");
}
registerHandler("vi_word_back", vi_word_back);

async function vi_word_end() : Promise<void> {
  await moveByWordMotion("word", "end");
}
registerHandler("vi_word_end", vi_word_end);

async function vi_WORD() : Promise<void> {
  await moveByWordMotion("WORD", "forward");
}
registerHandler("vi_WORD", vi_WORD);

async function vi_WORD_back() : Promise<void> {
  await moveByWordMotion("WORD", "backward");
}
registerHandler("vi_WORD_back", vi_WORD_back);

async function vi_WORD_end() : Promise<void> {
  await moveByWordMotion("WORD", "end");
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
  await moveToFirstNonBlank();
}
registerHandler("vi_first_non_blank", vi_first_non_blank);

async function moveToFirstNonBlank() : Promise<void> {
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

function vi_doc_start() : void {
  consumeCount(); // Count doesn't apply
  editor.executeAction("move_document_start");
}
registerHandler("vi_doc_start", vi_doc_start);

async function vi_doc_end() : Promise<void> {
  const explicitCount = state.count;
  consumeCount();
  const bufferId = editor.getActiveBufferId();
  const lastLine = await lastContentLine(bufferId);
  if (lastLine === null) {
    editor.executeAction("move_document_end");
    return;
  }
  // Vim's bare `G` goes to the *last line with content*: a file ending in a
  // newline has no line after it in Vim's model, but the editor does place a
  // caret there, and `move_document_end` lands on it — where `x`, `dd` and the
  // rest then have nothing to act on. `nG` goes to line n instead.
  const target = explicitCount === null
    ? lastLine
    : Math.min(Math.max(0, explicitCount - 1), lastLine);
  const lineStart = await editor.getLineStartPosition(target);
  if (lineStart === null) {
    editor.executeAction("move_document_end");
    return;
  }
  editor.setBufferCursor(bufferId, lineStart);
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

  memory.lastWordSearch = { text: currentTarget.text, direction, wholeWord: currentTarget.wholeWord };
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
//
// Each insert-entering command records itself in `pendingInsertCommand` so
// the Escape-time capture can build a `lastChange` that '.' replays by
// re-executing the command's cursor motion at the new cursor and then
// re-inserting the recorded keystrokes (Vim `:help .`). Commands that
// reposition the cursor before inserting must also flush the editor's
// command queue first: `executeAction` is queued and applied on the editor
// thread, so without the flush `switchMode("insert")` would record the
// PRE-reposition cursor position as the session's start, and the '.' capture
// would span intervening buffer text instead of just the typed keystrokes
// (issue #2443: `o`/`a`/`A` + `.` injected unrelated line content).
async function enterInsertRepositioned(command: string): Promise<void> {
  await editor.flush();
  state.pendingInsertCommand = command;
  switchMode("insert");
}

function vi_insert_before() : void {
  // No repositioning, so no flush needed: the cursor snapshot is already
  // accurate. Recording the entering command keeps a stale previous change
  // (e.g. an `x`) from swallowing this insert's recording at capture time.
  state.pendingInsertCommand = "i";
  switchMode("insert");
}
registerHandler("vi_insert_before", vi_insert_before);

async function vi_insert_after() : Promise<void> {
  editor.executeAction("move_right");
  await enterInsertRepositioned("a");
}
registerHandler("vi_insert_after", vi_insert_after);

async function vi_insert_line_start() : Promise<void> {
  // Vim's `I` inserts before the first non-blank, not in column 0 — that is
  // `gI`. `.` replays the same motion below.
  await moveToFirstNonBlank();
  await enterInsertRepositioned("I");
}
registerHandler("vi_insert_line_start", vi_insert_line_start);

async function vi_insert_line_end() : Promise<void> {
  editor.executeAction("move_line_end");
  await enterInsertRepositioned("A");
}
registerHandler("vi_insert_line_end", vi_insert_line_end);

async function vi_open_below() : Promise<void> {
  editor.executeAction("move_line_end");
  editor.executeAction("insert_newline");
  await enterInsertRepositioned("o");
}
registerHandler("vi_open_below", vi_open_below);

// Open a line above the cursor's line and leave the cursor at its start.
// Implemented with an explicit insert at the line-start byte offset instead of
// `insert_newline` + `move_up`: the offsets are computed from the pre-command
// cursor, so the new empty line and the cursor position cannot disagree.
async function openLineAbove(): Promise<boolean> {
  const bufferId = editor.getActiveBufferId();
  if (isActiveBufferEditingDisabled(bufferId)) {
    return false;
  }
  const range = await getLinewiseRange(1);
  if (range === null) {
    // Empty buffer: open an (empty) line below the cursor's empty line.
    if (editor.getBufferLength(bufferId) === 0) {
      editor.insertText(bufferId, 0, "\n");
      editor.setBufferCursor(bufferId, 0);
      return true;
    }
    return false;
  }
  editor.insertText(range.bufferId, range.start, range.lineTerminator);
  editor.setBufferCursor(range.bufferId, range.start);
  return true;
}

async function vi_open_above() : Promise<void> {
  if (!(await openLineAbove())) {
    switchMode("normal");
    return;
  }
  await enterInsertRepositioned("O");
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
  state.pending = { operator: "d", textObject: null };
  switchMode("operator-pending");
}
registerHandler("vi_delete_operator", vi_delete_operator);

function vi_change_operator() : void {
  state.pending = { operator: "c", textObject: null };
  switchMode("operator-pending");
}
registerHandler("vi_change_operator", vi_change_operator);

function vi_yank_operator() : void {
  state.pending = { operator: "y", textObject: null };
  switchMode("operator-pending");
}
registerHandler("vi_yank_operator", vi_yank_operator);

// Line operations (dd, cc, yy) - support count prefix (3dd = delete 3 lines)

// The line-wise range `dd`/`cc`/`yy` act on: `count` whole lines from the
// caret's own line. `yy` names its own `cursorAfter` because Vim leaves the
// caret where it was, unlike `yj`, which moves it to the start of the yank.
async function lineOperatorRange(count: number, cursorAfter?: number): Promise<OperatorRange | null> {
  const bufferId = editor.getActiveBufferId();
  const position = editor.getPrimaryCursor()?.position ?? editor.getCursorPosition();
  if (position === null) {
    return null;
  }
  return linewiseRangeAt(bufferId, position, count, cursorAfter);
}

async function vi_delete_line() : Promise<void> {
  const count = consumeCount();
  recordChange({ type: "line-op", action: "delete_line", count });
  await applyOperator("d", await lineOperatorRange(count));
}
registerHandler("vi_delete_line", vi_delete_line);

async function vi_change_line() : Promise<void> {
  const count = consumeCount();
  recordChange({ type: "line-op", action: "change_line", count });
  await applyOperator("c", await lineOperatorRange(count));
}
registerHandler("vi_change_line", vi_change_line);

async function vi_yank_line() : Promise<void> {
  const count = consumeCount();
  const position = editor.getPrimaryCursor()?.position ?? editor.getCursorPosition();
  await applyOperator("y", await lineOperatorRange(count, position ?? undefined));
  editor.setStatus(editor.t("status.yanked_lines", { count: String(count) }));
}
registerHandler("vi_yank_line", vi_yank_line);


// `>` / `<` operators: enter operator-pending so a motion or a doubled
// operator (>>/<<) can follow, mirroring d/c/y.
function vi_indent_operator() : void {
  state.pending = { operator: ">", textObject: null };
  switchMode("operator-pending");
}
registerHandler("vi_indent_operator", vi_indent_operator);

function vi_dedent_operator() : void {
  state.pending = { operator: "<", textObject: null };
  switchMode("operator-pending");
}
registerHandler("vi_dedent_operator", vi_dedent_operator);

// Doubled operators >> and <<. Only fire when the matching operator is
// pending, so invalid combos like `d>` cancel instead of indenting.
async function vi_indent_line() : Promise<void> {
  if (state.pending?.operator !== ">") {
    switchMode("normal");
    return;
  }
  const count = consumeCount();
  recordChange({ type: "line-op", action: "indent_line", count });
  await applyLineOpIndent(">", count);
}
registerHandler("vi_indent_line", vi_indent_line);

async function vi_dedent_line() : Promise<void> {
  if (state.pending?.operator !== "<") {
    switchMode("normal");
    return;
  }
  const count = consumeCount();
  recordChange({ type: "line-op", action: "dedent_line", count });
  await applyLineOpIndent("<", count);
}
registerHandler("vi_dedent_line", vi_dedent_line);

// Single character operations - support count prefix (3x = delete 3 chars)
// How many characters `x`/`X` may take without leaving the line.
//
// Neither ever deletes a line break in Vim: `x` on an empty line does
// nothing, and `X` in column 1 does nothing. The generic guard only asks
// whether the caret is inside the *buffer*, so both used to select across the
// newline and silently join two lines — a file-corrupting edit the user did
// not ask for and would not see.
// How many *characters* the caret can move within its own line.
//
// `x`, `X` and `r` take their counts in characters, so a byte distance is the
// wrong thing to clamp them with: on a line of two multi-byte characters it
// reads as six, and `5x` runs past the line end and joins the next line — the
// very thing the clamp exists to prevent.
async function charsAvailableOnLine(forward: boolean): Promise<number> {
  const bufferId = editor.getActiveBufferId();
  const cursor = editor.getPrimaryCursor();
  const position = cursor?.position ?? editor.getCursorPosition();
  const line = cursor?.line ?? null;
  if (line === null || position === null) {
    return 0;
  }
  const bound = forward
    ? await editor.getLineEndPosition(line)
    : await editor.getLineStartPosition(line);
  if (bound === null) {
    return 0;
  }
  const start = Math.min(position, bound);
  const end = Math.max(position, bound);
  if (end <= start) {
    return 0;
  }
  const text = (await editor.getBufferText(bufferId, start, end)) ?? "";
  // Spread, not `.length`: a character outside the BMP is two UTF-16 units and
  // one keystroke.
  return [...text].length;
}

// The range `x`/`X`/`s` take: `count` characters from the caret, stopping at
// the line's own end. `horizontalTarget` does the bounding, which is why these
// no longer clamp against `charsAvailableOnLine` first — Vim's `5x` on a
// two-character line deletes both rather than refusing, unlike `5r`.
async function charwiseCountRange(count: number, forward: boolean): Promise<OperatorRange | null> {
  const origin = editor.getCursorPosition();
  if (origin === null) {
    return null;
  }
  const target = await horizontalTarget(origin, count, forward, false);
  return target === null ? null : charwiseRange(origin, target);
}

// The range `D`/`C` take: the caret to the end of its line.
async function toLineEndRange(): Promise<OperatorRange | null> {
  const origin = editor.getCursorPosition();
  if (origin === null) {
    return null;
  }
  const target = await lineEndTarget(origin);
  return target === null ? null : charwiseRange(origin, target);
}

async function vi_delete_char() : Promise<void> {
  const count = consumeCount();
  recordChange({ type: "simple", action: "delete_forward", count });
  await applyOperator("d", await charwiseCountRange(count, true));
}
registerHandler("vi_delete_char", vi_delete_char);

async function vi_delete_char_before() : Promise<void> {
  const count = consumeCount();
  recordChange({ type: "simple", action: "delete_backward", count });
  await applyOperator("d", await charwiseCountRange(count, false));
}
registerHandler("vi_delete_char_before", vi_delete_char_before);

// Replace-char (`r<char>`): wait for one keypress and replace the
// character(s) under the cursor with it.  Uses `editor.getNextKey()`
// (plugin API #1) — same pattern as find-char above.
async function vi_replace_char(): Promise<void> {
  // Set directly rather than through `switchMode`, which can only emit
  // `vi-<mode>` and so cannot produce the `vi-replace-char` editor mode this
  // needs. The vi-side mode is borrowed purely for the status indicator.
  state.mode = "find-char"; // reuse find-char state slot for status
  editor.setEditorMode("vi-replace-char");
  editor.setStatus("-- REPLACE CHAR --");

  editor.beginKeyCapture();
  const generation = modalGeneration;
  let ev;
  try {
    ev = await editor.getNextKey();
  } finally {
    editor.endKeyCapture();
  }

  // The state was reset while the key was awaited — the buffer this `r` was
  // aimed at is no longer the one in front of us.
  if (generation !== modalGeneration) return;

  // Escape / non-character keys cancel the replacement.
  if (ev.key.length !== 1) {
    switchMode("normal");
    return;
  }

  const count = consumeCount();
  recordChange({ type: "replace-char", replacement: ev.key, count });
  await replaceCharsUnderCursor(ev.key, count);
  switchMode("normal");
}
registerHandler("vi_replace_char", vi_replace_char);

// `r` replaces characters on the caret's own line and nowhere else: Vim
// refuses `5rz` outright when fewer than five characters remain, rather than
// running over the line break the way an unclamped loop would.
async function replaceCharsUnderCursor(replacement: string, count: number): Promise<void> {
  if (count > (await charsAvailableOnLine(true))) {
    return;
  }
  for (let i = 0; i < count; i++) {
    editor.executeAction("delete_forward");
    editor.insertAtCursor(replacement);
  }
  // Move cursor back to stay on the replaced char (vim behavior).
  editor.executeAction("move_left");
}

// Substitute (delete char and enter insert mode)
async function vi_substitute() : Promise<void> {
  const count = consumeCount();
  recordChange({ type: "simple", action: "substitute", count });
  await applyOperator("c", await charwiseCountRange(count, true));
}
registerHandler("vi_substitute", vi_substitute);

// Delete to end of line (D)
async function vi_delete_to_end() : Promise<void> {
  recordChange({ type: "operator-motion", operator: "d", motion: "move_line_end" });
  await applyOperator("d", await toLineEndRange());
}
registerHandler("vi_delete_to_end", vi_delete_to_end);

// Change to end of line (C)
async function vi_change_to_end() : Promise<void> {
  recordChange({ type: "operator-motion", operator: "c", motion: "move_line_end" });
  await applyOperator("c", await toLineEndRange());
}
registerHandler("vi_change_to_end", vi_change_to_end);

// Clipboard
function vi_paste_after() : void {
  if (memory.lastYankWasLinewise) {
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
  if (memory.lastYankWasLinewise) {
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
  // Let a capture still in flight finish writing `insertedText` first.
  // Without this, `.` immediately after Escape replays the change's motion
  // and deletion but inserts nothing.
  if (state.pendingCapture !== null) {
    await state.pendingCapture;
  }

  if (!memory.lastChange) {
    editor.setStatus(editor.t("status.no_change_to_repeat"));
    return;
  }

  const change = memory.lastChange;
  const count = consumeCountOrDefault(change.count ?? 1);
  await replayChange(change, count);
}

async function replayChange(change: LastChange, count: number): Promise<void> {
  switch (change.type) {
    case "simple": {
      // Simple actions like x, X, s
      if (change.action === "substitute") {
        // Substitute: delete chars and insert text
        await applyOperator("c", await charwiseCountRange(count, true), { enterInsert: false });
        if (change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      } else if (change.action) {
        // Simple action like delete_forward, delete_backward
        if (change.action === "delete_forward") {
          await applyOperator("d", await charwiseCountRange(count, true));
        } else if (change.action === "join_lines") {
          await joinLines(Math.max(1, count - 1));
        } else if (change.action === "delete_backward") {
          await applyOperator("d", await charwiseCountRange(count, false));
        } else {
          executeWithCount(change.action, count);
        }
      }
      break;
    }

    case "line-op": {
      // Line operations like dd, cc
      if (change.action === "delete_line") {
        await applyOperator("d", await lineOperatorRange(count));
      } else if (change.action === "change_line") {
        // `enterInsert: false`: the replay supplies the recorded keystrokes
        // itself and must come back to normal mode, not leave the user in
        // insert after a `.`.
        await applyOperator("c", await lineOperatorRange(count), { enterInsert: false });
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
      if (LINEWISE_MOTIONS[change.motion]) {
        // A line-wise change leaves an empty line to type into; replaying it
        // as a plain delete closed the gap instead, and the recorded text
        // landed on the following line.
        const span = await linewiseSpanForMotion(change.motion, count, change.count ?? null);
        if (span === null) {
          break;
        }
        const bufferId = editor.getActiveBufferId();
        const spanStart = await editor.getLineStartPosition(span.firstLine);
        if (spanStart === null) {
          break;
        }
        await applyOperator(
          change.operator === "c" ? "c" : change.operator,
          await linewiseRangeAt(bufferId, spanStart, span.lineCount),
          { enterInsert: false },
        );
        if (change.operator === "c" && change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
        break;
      }

      const wordMotion = wordMotionFromRepeatMotion(change.motion);
      if (change.operator === "c") {
        // A recorded `c` replays as its delete half; the insert follows. The
        // `true` keeps Vim's `cw`-is-`ce` rule on the replay, which is what the
        // original `c` used to pick its range.
        if (wordMotion) {
          await applyWordOperatorMotion(wordMotion.family, "d", wordMotion.kind, count, true);
        } else {
          await applyOperatorWithMotion("d", change.motion, count, null, false);
        }
        if (change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      } else if (wordMotion) {
        await applyWordOperatorMotion(wordMotion.family, change.operator, wordMotion.kind, count);
      } else {
        await applyOperatorWithMotion(change.operator, change.motion, count, null, false);
      }
      break;
    }

    case "operator-textobj": {
      // Operator + text object like diw, ci"
      // A recorded `c` replays as its delete half; the insert follows below.
      await applyTextObject(
        change.textObject.object,
        change.operator === "c" ? "d" : change.operator,
        change.textObject.modifier,
        false,
      );
      if (change.operator === "c" && change.insertedText) {
        editor.insertAtCursor(change.insertedText);
      }
      break;
    }

    case "operator-find-char": {
      // Operator + find-char like dfx, dtx, cfx
      if (change.findType) {
        if (change.operator === "c") {
          // Replay change as delete-then-insert so '.' doesn't block on input.
          await executeFindCharOperator("d", change.findType, change.findCharTarget, count, false);
          if (change.insertedText) {
            editor.insertAtCursor(change.insertedText);
          }
        } else {
          await executeFindCharOperator(change.operator, change.findType, change.findCharTarget, count, false);
        }
      }
      break;
    }

    case "replace-char": {
      await replaceCharsUnderCursor(change.replacement, count);
      break;
    }

    case "insert": {
      // Pure insert (i, a, I, A, o, O): re-execute the entering command's
      // cursor motion at the current cursor, then re-insert the recorded
      // keystrokes (Vim `:help .`). Replaying only the text without the
      // motion — or vice versa — is what corrupted buffers in issue #2443.
      switch (change.insertCommand) {
        case "a":
          editor.executeAction("move_right");
          break;
        case "I":
          await moveToFirstNonBlank();
          await editor.flush();
          break;
        case "A":
          editor.executeAction("move_line_end");
          break;
        case "o":
          editor.executeAction("move_line_end");
          editor.executeAction("insert_newline");
          break;
        case "O":
          if (!(await openLineAbove())) {
            return;
          }
          break;
      }
      if (change.insertedText) {
        editor.insertAtCursor(change.insertedText);
        // Leave the cursor on the last inserted character, exactly as
        // Escape does when the original insert ended.
        editor.executeAction("move_left_in_line");
      }
      break;
    }
  }
}
registerHandler("vi_repeat", vi_repeat);

// Pull the next `joins` lines onto the caret's, a space between each.
//
// Bounded by the buffer: an unbounded loop at the end of the file deletes the
// trailing newline and appends a space to the last line, where Vim joins what
// it can and stops (and `J` on the last line does nothing at all).
async function joinLines(joins: number): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const line = editor.getPrimaryCursor()?.line ?? null;
  const lastLine = await lastContentLine(bufferId);
  if (line === null || lastLine === null) {
    return;
  }
  for (let i = 0; i < Math.min(joins, lastLine - line); i++) {
    editor.executeAction("move_line_end");
    // Delete the newline character
    editor.executeAction("delete_forward");
    // Insert a space between the joined content
    editor.insertAtCursor(" ");
  }
}

// Join lines — delete newline at end of current line and insert a space
async function vi_join() : Promise<void> {
  // Vim's `[count]J` joins `count` lines, so it performs count-1 joins, and a
  // count below 2 still joins one pair.
  const count = Math.max(2, consumeCount());
  recordChange({ type: "simple", action: "join_lines", count });
  await joinLines(count - 1);
}
registerHandler("vi_join", vi_join);

// Toggle case (~) — uses native toggle_case action
function vi_toggle_case() : void {
  executeWithCount("toggle_case");
}
registerHandler("vi_toggle_case", vi_toggle_case);

// Search
function vi_search_forward() : void {
  memory.lastWordSearch = null;
  editor.executeAction("search");
}
registerHandler("vi_search_forward", vi_search_forward);

function vi_search_backward() : void {
  memory.lastWordSearch = null;
  // Use same search dialog, user can search backward manually
  editor.executeAction("search");
}
registerHandler("vi_search_backward", vi_search_backward);

async function vi_find_next() : Promise<void> {
  if (memory.lastWordSearch) {
    await executeStoredWordSearch(
      memory.lastWordSearch.text,
      memory.lastWordSearch.direction,
      consumeCount(),
      memory.lastWordSearch.wholeWord,
    );
    return;
  }
  editor.executeAction("find_next");
}
registerHandler("vi_find_next", vi_find_next);

async function vi_find_prev() : Promise<void> {
  if (memory.lastWordSearch) {
    const direction = memory.lastWordSearch.direction === "forward" ? "backward" : "forward";
    await executeStoredWordSearch(
      memory.lastWordSearch.text,
      direction,
      consumeCount(),
      memory.lastWordSearch.wholeWord,
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
  if (state.visual === null) return;
  state.visual.head = null;
  state.visual.range = null;
}

// Start a visual selection anchored at the cursor.
function beginVisualSelection(): void {
  const anchor = editor.getCursorPosition();
  const anchorLine = editor.getPrimaryCursor()?.line ?? null;
  state.visual = {
    anchor,
    head: anchor,
    range: null,
    lines: anchorLine === null ? null : { anchor: anchorLine, head: anchorLine },
  };
}

// Enter character-wise visual mode
function vi_visual_char() : void {
  beginVisualSelection();
  // Select the character under cursor to establish the anchor.
  // This moves cursor one position right (the selection end), which is
  // standard visual mode behavior — the first char is part of the selection.
  editor.executeAction("select_right");
  switchMode("visual");
}
registerHandler("vi_visual_char", vi_visual_char);

// Enter line-wise visual mode
async function vi_visual_line() : Promise<void> {
  beginVisualSelection();
  switchMode("visual-line");
  // Establish the one-line span the mode starts on, so every line-wise
  // command reads the same computed range whether or not a motion followed.
  const anchorLine = state.visual?.lines?.anchor ?? null;
  if (anchorLine === null) {
    editor.executeAction("select_line");
    return;
  }
  await setVisualLineHead(anchorLine);
}
registerHandler("vi_visual_line", vi_visual_line);

// Toggle between visual and visual-line modes
async function vi_visual_toggle_line() : Promise<void> {
  if (state.mode === "visual") {
    const head = editor.getPrimaryCursor()?.line ?? null;
    clearComputedVisualRange();
    switchMode("visual-line");
    if (head === null || state.visual?.lines == null) {
      editor.executeAction("select_line");
      return;
    }
    await setVisualLineHead(head);
  } else if (state.mode === "visual-line") {
    // Switch to char mode (keep selection but change mode)
    clearComputedVisualRange();
    switchMode("visual");
  }
}
registerHandler("vi_visual_toggle_line", vi_visual_toggle_line);

// Enter visual block mode (Ctrl-v)
// The (line, col) origin a rectangular selection would need is not computed
// here: block motions extend the host's own selection, and nothing read the
// stored anchor. It comes back when block-wise `I`/`A` land, which are the
// commands that actually need a column.
function vi_visual_block() : void {
  beginVisualSelection();

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
  memory.lastYankWasLinewise = false;
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
  memory.lastYankWasLinewise = false;
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
  switchMode("visual");
}
registerHandler("vi_vblock_toggle_char", vi_vblock_toggle_char);

function vi_vblock_toggle_line() : void {
  // Switch to line visual mode
  editor.executeAction("select_line");
  switchMode("visual-line");
}
registerHandler("vi_vblock_toggle_line", vi_vblock_toggle_line);

// Visual block > / < — Vim shifts every line the block touches by one
// shiftwidth (whole lines, not just the selected columns) and returns to
// normal mode. The block selection is the same live per-line selection that
// vi_vblock_delete/yank act on, so applyVisualIndent (which drives the
// editor's selection-aware indent/dedent) does exactly the right thing here.
async function vi_vblock_indent() : Promise<void> {
  await applyVisualIndent(">");
}
registerHandler("vi_vblock_indent", vi_vblock_indent);

async function vi_vblock_dedent() : Promise<void> {
  await applyVisualIndent("<");
}
registerHandler("vi_vblock_dedent", vi_vblock_dedent);

// Visual mode motions - these extend the selection
// The charwise visual motions all compute a head and let
// `selectVisualRangeToTarget` build the range around the anchor. Driving the
// host's `select_*` instead lost the character `v` started on whenever the head
// crossed behind the anchor: the host's selection has no anchor character, so
// `vhd` and `vkd` shrank the selection to nothing instead of growing it the
// other way.
async function vi_vis_left() : Promise<void> {
  const head = visualHead();
  if (head === null) return;
  const target = await horizontalTarget(head, consumeCount(), false);
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}
registerHandler("vi_vis_left", vi_vis_left);

async function vi_vis_down() : Promise<void> {
  const head = visualHead();
  if (head === null) return;
  const target = await verticalTarget(head, consumeCount());
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}
registerHandler("vi_vis_down", vi_vis_down);

async function vi_vis_up() : Promise<void> {
  const head = visualHead();
  if (head === null) return;
  const target = await verticalTarget(head, -consumeCount());
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}
registerHandler("vi_vis_up", vi_vis_up);

async function vi_vis_right() : Promise<void> {
  const head = visualHead();
  if (head === null) return;
  const target = await horizontalTarget(head, consumeCount(), true);
  if (target !== null) {
    await selectVisualRangeToTarget(target);
  }
}
registerHandler("vi_vis_right", vi_vis_right);

async function vi_vis_word() : Promise<void> {
  await extendVisualByWordMotion("word", "forward");
}
registerHandler("vi_vis_word", vi_vis_word);

// Byte offset just past the character starting at `offset`.
async function charEndOffset(bufferId: number, offset: number): Promise<number> {
  const length = editor.getBufferLength(bufferId);
  if (offset >= length) {
    return length;
  }
  const sample = (await editor.getBufferText(bufferId, offset, Math.min(offset + 4, length))) ?? "";
  if (sample.length === 0) {
    return Math.min(offset + 1, length);
  }
  const codePoint = sample.codePointAt(0);
  const char = String.fromCodePoint(codePoint ?? sample.charCodeAt(0));
  return Math.min(offset + editor.utf8ByteLength(char), length);
}

// Vim's head: the character the caret sits *on*, not the exclusive end of the
// host's selection. `selectVisualRangeToTarget` writes it, so every motion that
// goes through that stays consistent with the next one.
function visualHead(): number | null {
  return state.visual?.head ?? state.visual?.anchor ?? editor.getCursorPosition();
}

// The offset `count` characters left or right of `from`, stopping at the
// caret's own line — Vim's `h` and `l` do not wrap.
//
// `clampToLastCharacter` is the difference between the caret's `l`, which will
// not step past a line's last character, and `dl`'s, which must reach one past
// it to delete that character.
async function horizontalTarget(
  from: number,
  count: number,
  forward: boolean,
  clampToLastCharacter: boolean = true,
): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const lineStart = await findLineStartAtPosition(bufferId, from);
  const lineText = (await safeGetBufferText(bufferId, lineStart, lineStart + 4096)) ?? "";
  const bounded = lineText.split(/\r?\n/, 1)[0] ?? "";
  let index = byteOffsetToStringIndex(bounded, from - lineStart);
  for (let i = 0; i < Math.max(1, count); i++) {
    const next = forward ? nextStringIndex(bounded, index) : previousStringIndex(bounded, index);
    if (next === index || next < 0 || next > bounded.length) {
      break;
    }
    index = next;
  }
  if (clampToLastCharacter && forward && index >= bounded.length && bounded.length > 0) {
    index = previousStringIndex(bounded, bounded.length);
  }
  return lineStart + stringIndexToByteOffset(bounded, index);
}

// The offset on the line `delta` away, at the same column as `from` — the
// character boundary at or before it, so a multi-byte line cannot split.
async function verticalTarget(from: number, delta: number): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const cursorLine = editor.getPrimaryCursor()?.line ?? null;
  const lastLine = await lastContentLine(bufferId);
  if (cursorLine === null || lastLine === null) {
    return null;
  }
  const fromLineStart = await findLineStartAtPosition(bufferId, from);
  const column = from - fromLineStart;

  const targetLine = Math.max(0, Math.min(cursorLine + delta, lastLine));
  const targetStart = await editor.getLineStartPosition(targetLine);
  const targetEnd = await editor.getLineEndPosition(targetLine);
  if (targetStart === null) {
    return null;
  }
  const limit = targetEnd === null ? targetStart : Math.max(targetStart, targetEnd);
  const lineText = (await safeGetBufferText(bufferId, targetStart, limit)) ?? "";
  const index = byteOffsetToStringIndex(lineText, Math.min(column, limit - targetStart));
  return targetStart + stringIndexToByteOffset(lineText, index);
}

// Extend a visual selection backwards.
//
// `v` leaves the anchor on the character under the caret and the head one past
// it, which is what makes a forward selection inclusive. A head moving *behind*
// the anchor has to take that character with it — Vim's `v0d` and `vbd` both
// remove the character `v` started on — so the anchor is re-seated one
// character forward before the backward selection is built.
//
// Only when the head is actually at or behind the anchor: re-seating while the
// head is still forward of it (`v w w b`) would teleport the head instead of
// shrinking the selection.
async function extendVisualBackward(selectAction: string, count: number): Promise<void> {
  const visual = state.visual;
  if (visual !== null) {
    const bufferId = editor.getActiveBufferId();
    const anchorEnd = await charEndOffset(bufferId, visual.anchor);
    const head = editor.getPrimaryCursor()?.position ?? anchorEnd;
    if (head <= anchorEnd) {
      editor.setBufferCursor(bufferId, anchorEnd);
      await editor.flush();
    }
  }
  selectWithCount(selectAction, count);
}

// Not routed through `extendVisualBackward`: re-seating the caret before a
// *word* motion changes what that motion means — `b` from the first character
// of a word goes to the previous word, but from one character later it goes to
// the start of the current one. The shared resolver computes the target from
// the real head, and the range is built around it.
async function vi_vis_word_back() : Promise<void> {
  await extendVisualByWordMotion("word", "backward");
}
registerHandler("vi_vis_word_back", vi_vis_word_back);

async function vi_vis_word_end() : Promise<void> {
  await extendVisualByWordMotion("word", "end");
}
registerHandler("vi_vis_word_end", vi_vis_word_end);

function visualWORDMotionOrigin(): number | null {
  return state.visual?.head ?? state.visual?.anchor ?? editor.getCursorPosition();
}

async function vi_vis_WORD() : Promise<void> {
  await extendVisualByWordMotion("WORD", "forward");
}
registerHandler("vi_vis_WORD", vi_vis_WORD);

async function vi_vis_WORD_back() : Promise<void> {
  await extendVisualByWordMotion("WORD", "backward");
}
registerHandler("vi_vis_WORD_back", vi_vis_WORD_back);

async function vi_vis_WORD_end() : Promise<void> {
  await extendVisualByWordMotion("WORD", "end");
}
registerHandler("vi_vis_WORD_end", vi_vis_WORD_end);

function vi_vis_line_start() : Promise<void> {
  clearComputedVisualRange();
  consumeCount();
  return extendVisualBackward("select_line_start", 1);
}
registerHandler("vi_vis_line_start", vi_vis_line_start);

function vi_vis_line_end() : void {
  clearComputedVisualRange();
  consumeCount();
  editor.executeAction("select_line_end");
  // Vim's `$` in visual mode lands *on* the end-of-line position, so the
  // inclusive selection takes the line break with it and `v$d` joins the next
  // line up. That differs from `d$`, which stops at the last character.
  editor.executeAction("select_right");
}
registerHandler("vi_vis_line_end", vi_vis_line_end);

async function vi_vis_doc_start() : Promise<void> {
  consumeCount();
  await selectVisualRangeToTarget(0);
}
registerHandler("vi_vis_doc_start", vi_vis_doc_start);

async function vi_vis_doc_end() : Promise<void> {
  const explicitCount = state.count;
  consumeCount();
  const bufferId = editor.getActiveBufferId();
  const lastLine = await lastContentLine(bufferId);
  if (lastLine === null) {
    return;
  }
  // Like normal-mode `G`: the last line with content, or the counted line, and
  // on its first non-blank — not the buffer's last byte, which would take the
  // whole final line with it.
  const target = explicitCount === null ? lastLine : Math.min(Math.max(0, explicitCount - 1), lastLine);
  const lineStart = await editor.getLineStartPosition(target);
  if (lineStart === null) {
    return;
  }
  await selectVisualRangeToTarget(await firstNonBlankOffset(bufferId, lineStart));
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
// Redraw a line-wise visual selection so it covers whole lines from its
// anchor's line to `headLine`, in both directions. The computed range is what
// `d`/`c`/`y` consume; the host selection is redrawn alongside it because the
// indent operators drive the editor's own selection-aware actions.
async function setVisualLineHead(headLine: number): Promise<void> {
  const visual = state.visual;
  if (visual === null || visual.lines === null) {
    return;
  }
  const bufferId = editor.getActiveBufferId();
  const lastLine = await lastContentLine(bufferId);
  if (lastLine === null) {
    return;
  }
  const head = Math.max(0, Math.min(headLine, lastLine));
  visual.lines.head = head;

  const firstLine = Math.min(visual.lines.anchor, head);
  const lastSelected = Math.max(visual.lines.anchor, head);
  const lineCount = lastSelected - firstLine + 1;
  const start = await editor.getLineStartPosition(firstLine);
  const headStart = await editor.getLineStartPosition(head);
  if (start === null) {
    return;
  }
  // The line-wise end is where the line after the last selected one begins,
  // or the buffer end when there is no such line. Asking directly beats
  // scanning the span for its line breaks on every keystroke.
  const afterLast = await editor.getLineStartPosition(lastSelected + 1);
  const end = afterLast ?? editor.getBufferLength(bufferId);
  visual.range = { start, end };
  visual.head = headStart;

  editor.setBufferCursor(bufferId, start);
  await editor.flush();
  // One batched action rather than `lineCount` dispatched ones: this runs on
  // every keystroke, so a loop here makes holding `j` quadratic.
  selectWithCount("select_down", lineCount);
}

// The tracked head, falling back to the caret's line before a span has been
// established. The caret alone will not do: a redrawn selection leaves it at
// the far end, not on the moving one.
function visualLineHead(): number | null {
  return state.visual?.lines?.head ?? editor.getPrimaryCursor()?.line ?? null;
}

async function vi_vline_down() : Promise<void> {
  const head = visualLineHead();
  if (head === null) {
    executeWithCount("select_down");
    return;
  }
  await setVisualLineHead(head + consumeCount());
}
registerHandler("vi_vline_down", vi_vline_down);

async function vi_vline_up() : Promise<void> {
  const head = visualLineHead();
  if (head === null) {
    executeWithCount("select_up");
    return;
  }
  await setVisualLineHead(head - consumeCount());
}
registerHandler("vi_vline_up", vi_vline_up);

async function selectVisualRangeToTarget(target: number, includeDisplayTarget: boolean = true): Promise<void> {
  const visual = state.visual;
  if (visual === null) {
    // Not in a visual mode, so there is no selection to record against.
    // `selectToPosition` leaves the cursor on the target, which is what the
    // motion origin would have read back out of `head` anyway.
    await selectToPosition(target, includeDisplayTarget);
    return;
  }
  const anchor = visual.anchor;

  const bufferId = editor.getActiveBufferId();
  const bufferText = await editor.getBufferText(bufferId, 0, editor.getBufferLength(bufferId));
  const anchorIndex = byteOffsetToStringIndex(bufferText, anchor);
  const targetIndex = byteOffsetToStringIndex(bufferText, target);
  const anchorEnd = anchor + byteLengthOfCharAt(bufferText, anchorIndex);
  const targetEnd = target + byteLengthOfCharAt(bufferText, targetIndex);

  visual.range = target >= anchor
    ? { start: anchor, end: targetEnd }
    : { start: target, end: anchorEnd };
  visual.head = target;

  await selectToPosition(target, includeDisplayTarget && target >= anchor);
}

// The visual selection, as a range the one applier can take.
//
// A visual mode has one selection, but it had two representations: the range a
// motion computed for itself, and the host's own selection for the motions
// that only drive `select_*`. Reading the host's back when there is no
// computed range leaves one representation at the point of use, which is what
// lets `d`, `c` and `y` here stop being three hand-rolled copies of the
// operator switch — copies that had already drifted over the register's shape.
async function visualOperatorRange(): Promise<OperatorRange | null> {
  const shape: RangeShape = state.mode === "visual-line" ? "linewise" : "charwise";

  const computed = state.visual?.range ?? null;
  if (computed !== null && computed.end > computed.start) {
    return { start: computed.start, end: computed.end, shape };
  }

  await editor.flush();
  const selection = editor.getPrimaryCursor()?.selection ?? null;
  if (selection === null) {
    return null;
  }
  const start = Math.min(selection.start, selection.end);
  const end = Math.max(selection.start, selection.end);
  return end > start ? { start, end, shape } : null;
}

// Visual mode operators - act on the selection
async function vi_vis_delete() : Promise<void> {
  await applyOperator("d", await visualOperatorRange());
}
registerHandler("vi_vis_delete", vi_vis_delete);

async function vi_vis_change() : Promise<void> {
  await applyOperator("c", await visualOperatorRange());
}
registerHandler("vi_vis_change", vi_vis_change);

async function vi_vis_yank() : Promise<void> {
  await applyOperator("y", await visualOperatorRange());
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
  const range = state.visual?.range ?? editor.getPrimaryCursor()?.selection ?? null;
  const firstByte = range
    ? Math.min(range.start, range.end)
    : editor.getCursorPosition();

  editor.executeAction(operator === ">" ? "insert_tab" : "dedent_selection");
  memory.lastYankWasLinewise = false;

  if (firstByte !== null && firstByte !== undefined) {
    const firstLineStart = await findLineStartAtPosition(bufferId, firstByte);
    await placeCursorAtFirstNonBlank(bufferId, firstLineStart);
  }
  switchMode("normal");
}

async function vi_vis_indent() : Promise<void> {
  await applyOperator(">", await visualOperatorRange());
}
registerHandler("vi_vis_indent", vi_vis_indent);

async function vi_vis_dedent() : Promise<void> {
  await applyOperator("<", await visualOperatorRange());
}
registerHandler("vi_vis_dedent", vi_vis_dedent);

// The text object each key names, shared by the operator-pending bindings and
// the visual-mode `i`/`a` capture below.
// The text object each key names, and the operator-pending bindings that reach
// it. One table: a second copy meant adding an object in two places and having
// them disagree in between.
const TEXT_OBJECT_KEYS: Array<{ key: string; object: string; handler: string }> = [
  { key: "w", object: "word", handler: "vi_to_word" },
  { key: "W", object: "WORD", handler: "vi_to_WORD" },
  { key: '"', object: '"', handler: "vi_to_dquote" },
  { key: "'", object: "'", handler: "vi_to_squote" },
  { key: "`", object: "`", handler: "vi_to_backtick" },
  { key: "(", object: "(", handler: "vi_to_paren" },
  { key: ")", object: "(", handler: "vi_to_paren" },
  { key: "b", object: "(", handler: "vi_to_paren" },
  { key: "{", object: "{", handler: "vi_to_brace" },
  { key: "}", object: "{", handler: "vi_to_brace" },
  { key: "B", object: "{", handler: "vi_to_brace" },
  { key: "[", object: "[", handler: "vi_to_bracket" },
  { key: "]", object: "[", handler: "vi_to_bracket" },
  { key: "<", object: "<", handler: "vi_to_angle" },
  { key: ">", object: "<", handler: "vi_to_angle" },
];

function textObjectForKey(key: string): string | undefined {
  return TEXT_OBJECT_KEYS.find((entry) => entry.key === key)?.object;
}

// Re-seat the character-wise visual selection onto an exact byte range.
async function setVisualRange(start: number, end: number): Promise<void> {
  const visual = state.visual;
  if (visual === null) {
    return;
  }
  const bufferId = editor.getActiveBufferId();
  visual.anchor = start;
  // The head is the character the caret sits *on*, which is the last one in
  // the range — not `end`, which is one past it. A motion typed after `viw`
  // resolves from the head, so an exclusive one sent it off by a character.
  visual.head = await charStartBefore(bufferId, end);
  visual.range = { start, end };
  editor.setBufferCursor(bufferId, start);
  await editor.flush();
  await selectToPosition(end);
}

// The byte offset of the character ending at `offset`.
async function charStartBefore(bufferId: number, offset: number): Promise<number> {
  if (offset <= 0) {
    return 0;
  }
  const sampleStart = Math.max(0, offset - 4);
  const sample = (await editor.getBufferText(bufferId, sampleStart, offset)) ?? "";
  if (sample.length === 0) {
    return Math.max(0, offset - 1);
  }
  const index = previousStringIndex(sample, sample.length);
  return sampleStart + stringIndexToByteOffset(sample, index);
}

// Visual-mode `i`/`a`: the next key names a text object, and the selection
// becomes that object. Driven by `getNextKey` rather than by its own editor
// mode — the operator-pending path needs a mode because an operator is already
// staged and waiting, while here there is nothing to stage.
async function enterVisualTextObject(modifier: TextObjectType): Promise<void> {
  editor.setStatus(`-- ${editor.t("mode.visual")} (${modifier === "inner" ? "i" : "a"}) --`);

  editor.beginKeyCapture();
  const generation = modalGeneration;
  let ev;
  try {
    ev = await editor.getNextKey();
  } finally {
    editor.endKeyCapture();
  }

  // The state was reset while the key was awaited — the selection this was
  // aimed at is gone.
  if (generation !== modalGeneration) return;

  const objectType = ev.key.length === 1 ? textObjectForKey(ev.key) : undefined;
  if (objectType === undefined) {
    editor.setStatus(getModeIndicator(state.mode));
    return;
  }

  const range = await computeTextObjectRange(objectType, modifier === "inner");
  if (range !== null) {
    await setVisualRange(range.start, range.end);
  }
  editor.setStatus(getModeIndicator(state.mode));
}

async function vi_vis_text_object_inner() : Promise<void> {
  await enterVisualTextObject("inner");
}
registerHandler("vi_vis_text_object_inner", vi_vis_text_object_inner);

async function vi_vis_text_object_around() : Promise<void> {
  await enterVisualTextObject("around");
}
registerHandler("vi_vis_text_object_around", vi_vis_text_object_around);

// Visual `J` — join every line the selection touches into one. Vim performs
// one join fewer than the number of selected lines, and a single-line
// selection still joins it with the line below.
//
// The line span comes from the same range every other visual operator takes:
// reading `state.visual.lines` instead only worked in the line-wise mode,
// where that field is maintained, so charwise `vjjJ` joined a single pair.
async function vi_vis_join() : Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const range = await visualOperatorRange();
  switchMode("normal");

  if (range === null) {
    await joinLines(1);
    return;
  }
  const span = await lineSpanOfRange(bufferId, range.start, range.end);
  editor.setBufferCursor(bufferId, span.firstLineStart);
  await editor.flush();
  await joinLines(Math.max(1, span.lineCount - 1));
}
registerHandler("vi_vis_join", vi_vis_join);

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
  if (state.pending !== null) state.pending.textObject = "inner";
  switchMode("text-object");
}
registerHandler("vi_text_object_inner", vi_text_object_inner);

// Enter text-object mode with "around" modifier
function vi_text_object_around() : void {
  if (state.pending !== null) state.pending.textObject = "around";
  switchMode("text-object");
}
registerHandler("vi_text_object_around", vi_text_object_around);

// Apply a text object and then the operator that is consuming it.
//
// The operator and modifier are arguments rather than reads of `state.pending`
// so that '.' can replay a recorded `diw`/`ci"` by passing what it recorded,
// instead of staging the pending state back up just to be read here.
async function applyTextObject(
  objectType: string,
  operator: string | null,
  modifier: TextObjectType,
  record: boolean = true,
): Promise<void> {
  if (!operator) {
    switchMode("normal");
    return;
  }

  // Record last change for '.' repeat (only for delete and change, not yank)
  if (record && (operator === "d" || operator === "c") && modifier) {
    recordChange({ type: "operator-textobj", operator, textObject: { modifier, object: objectType } });
  }

  const bufferId = editor.getActiveBufferId();
  const range = await computeTextObjectRange(objectType, modifier === "inner");
  if (range === null) {
    switchMode("normal");
    return;
  }
  await applyOperator(operator, charwiseRange(range.start, range.end));
}

// The byte range a text object covers at the caret, or null when there is no
// such object there. Split out of `applyTextObject` so visual mode can set the
// selection to an object (`viw`, `va"`) without an operator to apply.
async function computeTextObjectRange(
  objectType: string,
  isInner: boolean,
): Promise<{ start: number; end: number } | null> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return null;
  }

  // Get text around cursor to find the text object boundaries
  const windowSize = 1000;
  const startOffset = Math.max(0, cursorPos - windowSize);
  const bufLen = editor.getBufferLength(bufferId);
  const endOffset = Math.min(bufLen, cursorPos + windowSize);
  const text = await editor.getBufferText(bufferId, startOffset, endOffset);
  if (!text) {
    return null;
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
          // `a"` includes the quotes plus surrounding whitespace, matching Vim
          // (`:help aquote`): trailing whitespace after the closing quote is
          // included; if there is none, leading whitespace before the opening
          // quote is included instead.
          let aStart = quoteStart;
          let aEnd = quoteEnd + 1; // exclusive, just past the closing quote
          const isBlank = (c: string) => c === " " || c === "\t";
          let trailingEnd = aEnd;
          while (trailingEnd < line.length && isBlank(line[trailingEnd])) trailingEnd++;
          if (trailingEnd > aEnd) {
            aEnd = trailingEnd;
          } else {
            while (aStart > 0 && isBlank(line[aStart - 1])) aStart--;
          }
          selectStart = startOffset + lineStart + aStart;
          selectEnd = startOffset + lineStart + aEnd;
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
    return null;
  }
  return { start: selectStart, end: selectEnd };
}

// Helper to find matching bracket pair containing the cursor.
//
// When the caret is not inside a pair, Vim does not fail: `di(` from the start
// of `foo(bar)` still empties the parentheses, and with nothing left on the
// line it carries on into the lines below. So a failed backward search falls
// forward to the next opening bracket in the window.
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

  if (start === -1) {
    for (let i = pos; i < text.length; i++) {
      if (text[i] === openChar) {
        start = i;
        break;
      }
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
// The `vi_to_*` handlers run in text-object mode, where the operator and its
// modifier are exactly what `state.pending` is holding.
function applyPendingTextObject(objectType: string): Promise<void> {
  return applyTextObject(
    objectType,
    state.pending?.operator ?? null,
    state.pending?.textObject ?? null,
  );
}

async function vi_to_word() : Promise<void> { await applyPendingTextObject("word"); }
registerHandler("vi_to_word", vi_to_word);
async function vi_to_WORD() : Promise<void> { await applyPendingTextObject("WORD"); }
registerHandler("vi_to_WORD", vi_to_WORD);
async function vi_to_dquote() : Promise<void> { await applyPendingTextObject("\""); }
registerHandler("vi_to_dquote", vi_to_dquote);
async function vi_to_squote() : Promise<void> { await applyPendingTextObject("'"); }
registerHandler("vi_to_squote", vi_to_squote);
async function vi_to_backtick() : Promise<void> { await applyPendingTextObject("`"); }
registerHandler("vi_to_backtick", vi_to_backtick);
async function vi_to_paren() : Promise<void> { await applyPendingTextObject("("); }
registerHandler("vi_to_paren", vi_to_paren);
async function vi_to_brace() : Promise<void> { await applyPendingTextObject("{"); };
async function vi_to_bracket(): Promise<void> { await applyPendingTextObject("["); }
registerHandler("vi_to_bracket", vi_to_bracket);
async function vi_to_angle(): Promise<void> { await applyPendingTextObject("<"); }
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
  // Set directly rather than through `switchMode`, which would clear the
  // count that `3fx` must still be holding when the target key arrives.
  state.mode = "find-char";
  editor.setEditorMode("vi-find-char");
  editor.setStatus(getModeIndicator("find-char"));

  // Capture the key losslessly — without this, a user pressing the
  // target character very quickly after `f`/`t`/`F`/`T` could see the
  // key fall through to the buffer.
  editor.beginKeyCapture();
  const generation = modalGeneration;
  try {
    const ev = await editor.getNextKey();
    if (generation !== modalGeneration) return;
    state.pendingFindChar = null;
    // Escape (or any non-character key) cancels the motion.
    if (ev.key.length === 1) {
      await executeFindChar(findType, ev.key);
    }
  } finally {
    editor.endKeyCapture();
  }
  if (generation !== modalGeneration) return;
  switchMode("normal");
}

// Extract the current cursor line as a string plus the cursor's column index
// within it. Returns null if the buffer text can't be read. Used by both the
// pure find-char motion and the operator-pending find-char paths.
async function getFindCharLineContext(
  bufferId: number,
  cursorPos: number,
): Promise<{ lineText: string; col: number } | null> {
  // Read up to 10KB before and after cursor for context.
  const windowSize = 10000;
  const startOffset = Math.max(0, cursorPos - windowSize);
  const bufLen = editor.getBufferLength(bufferId);
  const endOffset = Math.min(bufLen, cursorPos + windowSize);

  const text = await editor.getBufferText(bufferId, startOffset, endOffset);
  if (!text) return null;

  const posInChunk = cursorPos - startOffset;

  // Find line start (last newline before cursor, or start of chunk).
  let lineStart = 0;
  for (let i = posInChunk - 1; i >= 0; i--) {
    if (text[i] === '\n') {
      lineStart = i + 1;
      break;
    }
  }

  // Find line end (next newline after cursor, or end of chunk).
  let lineEnd = text.length;
  for (let i = posInChunk; i < text.length; i++) {
    if (text[i] === '\n') {
      lineEnd = i;
      break;
    }
  }

  return { lineText: text.substring(lineStart, lineEnd), col: posInChunk - lineStart };
}

// Compute the landing column for a find-char motion on a single line.
// `count` selects the Nth occurrence (1-based). Returns the target column
// within `lineText`, or -1 if the target isn't found.
function computeFindCharTargetCol(
  findType: FindCharType,
  char: string,
  lineText: string,
  col: number,
  count: number,
): number {
  let remaining = Math.max(1, count);

  if (findType === "f" || findType === "t") {
    // Search forward on the line.
    for (let i = col + 1; i < lineText.length; i++) {
      if (lineText[i] === char && --remaining === 0) {
        return findType === "f" ? i : i - 1;
      }
    }
  } else {
    // Search backward (F/T).
    for (let i = col - 1; i >= 0; i--) {
      if (lineText[i] === char && --remaining === 0) {
        return findType === "F" ? i : i + 1;
      }
    }
  }
  return -1;
}

// Execute find char motion (async because getBufferText is async)
//
// `isRepeat` marks the `;`/`,` path. A `t` that already landed just before its
// target would otherwise never move again, so Vim's `;` after `t` skips the
// adjacent match and goes to the next one (`:help cpo-;`).
async function executeFindChar(findType: FindCharType, char: string, isRepeat: boolean = false): Promise<void> {
  if (!findType) return;

  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null || (cursorPos === 0 && (findType === "F" || findType === "T"))) {
    // Can't search backward from position 0
    return;
  }

  const ctx = await getFindCharLineContext(bufferId, cursorPos);
  if (!ctx) return;
  const { lineText, col } = ctx;

  let targetCol = computeFindCharTargetCol(findType, char, lineText, col, 1);
  if (isRepeat && targetCol === col && (findType === "t" || findType === "T")) {
    targetCol = computeFindCharTargetCol(findType, char, lineText, col, 2);
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
    memory.lastFindChar = { type: findType, char };
  }
}

// Apply a pending operator (d/c/y) over a find-char motion (df/dt/cf/ct/dF/dT…).
// In vim these are *inclusive* motions: the found character is part of the
// affected range. Forward (f/t) deletes from the cursor up to and including the
// landing column; backward (F/T) deletes from the landing column up to (but not
// including) the cursor.
async function executeFindCharOperator(
  operator: string,
  findType: FindCharType,
  char: string,
  count: number,
  record: boolean = true,
): Promise<void> {
  if (!findType) {
    switchMode("normal");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null || (cursorPos === 0 && (findType === "F" || findType === "T"))) {
    switchMode("normal");
    return;
  }

  const ctx = await getFindCharLineContext(bufferId, cursorPos);
  if (!ctx) {
    switchMode("normal");
    return;
  }
  const { lineText, col } = ctx;

  const targetCol = computeFindCharTargetCol(findType, char, lineText, col, count);
  if (targetCol < 0 || targetCol === col) {
    // Target not found on the line: vim leaves the buffer unchanged and cancels
    // the operator. Do NOT consume the operator as a different motion.
    switchMode("normal");
    return;
  }

  // Save for ; and , repeat (vim records the find even in operator form).
  memory.lastFindChar = { type: findType, char };

  // The landing column becomes a byte offset: column indices are measured in
  // the line string, byte offsets from the live caret plus the UTF-8 length of
  // the text between.
  const target = targetCol > col
    ? cursorPos + editor.utf8ByteLength(lineText.substring(col, targetCol))
    : cursorPos - editor.utf8ByteLength(lineText.substring(targetCol, col));

  // Record for '.' repeat (delete/change only, matching operator-motion).
  if (record && (operator === "d" || operator === "c")) {
    recordChange({
      type: "operator-find-char",
      operator,
      findType,
      findCharTarget: char,
      count,
    });
  }

  // `f` and `t` are inclusive motions, `F` and `T` exclusive — the same
  // distinction `rangeFromMotion` applies to every other motion, rather than
  // the `+ byteLengthOfCharAt` this used to do by hand.
  const kind: MotionKind = findType === "f" || findType === "t" ? "inclusive" : "exclusive";
  await applyOperator(operator, await rangeFromMotion(cursorPos, target, kind));
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
  if (memory.lastFindChar) {
    await executeFindChar(memory.lastFindChar.type, memory.lastFindChar.char, true);
  }
}
registerHandler("vi_find_char_repeat", vi_find_char_repeat);

// Repeat last find char in opposite direction (async)
async function vi_find_char_repeat_reverse(): Promise<void> {
  if (memory.lastFindChar) {
    const reversedType: FindCharType =
      memory.lastFindChar.type === "f" ? "F" :
      memory.lastFindChar.type === "F" ? "f" :
      memory.lastFindChar.type === "t" ? "T" : "t";
    await executeFindChar(reversedType, memory.lastFindChar.char, true);
  }
}
registerHandler("vi_find_char_repeat_reverse", vi_find_char_repeat_reverse);

// Enter find-char mode with a pending operator (df/dt/cf/ct/dF/dT/…). Awaits the
// target keypress, then applies the operator over the find-char range. Mirrors
// enterFindCharMode but preserves the pending operator across the await and
// dispatches to the operator-aware executor.
async function enterFindCharOperatorMode(findType: FindCharType): Promise<void> {
  if (!state.pending) {
    switchMode("normal");
    return;
  }
  const operator = state.pending.operator;
  // Consume any count now (e.g. d2fx); the find-char await must not lose it.
  const count = consumeCountOrDefault(1);

  state.pendingFindChar = findType;
  // Set directly rather than through `switchMode`: the operator and count are
  // already copied into locals above, but `switchMode` would also clear
  // `state.pending` and so blank the operator out of the status indicator
  // while the target key is awaited.
  state.mode = "find-char";
  editor.setEditorMode("vi-find-char");
  editor.setStatus(getModeIndicator("find-char"));

  editor.beginKeyCapture();
  const generation = modalGeneration;
  try {
    const ev = await editor.getNextKey();
    // The operator and count in the locals above belong to the state that has
    // since been reset; applying them now would target the wrong buffer.
    if (generation !== modalGeneration) return;
    state.pendingFindChar = null;
    if (ev.key.length === 1) {
      await executeFindCharOperator(operator, findType, ev.key, count);
    } else {
      // Escape (or any non-character key) cancels the operator.
      switchMode("normal");
    }
  } finally {
    editor.endKeyCapture();
  }
}

async function vi_op_find_char_f(): Promise<void> { return enterFindCharOperatorMode("f"); }
registerHandler("vi_op_find_char_f", vi_op_find_char_f);

async function vi_op_find_char_t(): Promise<void> { return enterFindCharOperatorMode("t"); }
registerHandler("vi_op_find_char_t", vi_op_find_char_t);

async function vi_op_find_char_F(): Promise<void> { return enterFindCharOperatorMode("F"); }
registerHandler("vi_op_find_char_F", vi_op_find_char_F);

async function vi_op_find_char_T(): Promise<void> { return enterFindCharOperatorMode("T"); }
registerHandler("vi_op_find_char_T", vi_op_find_char_T);

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
  // Vim's `cw` is `ce` (`:help cw`) — that rule now lives in
  // `computeWordOperatorRange`, keyed off the operator, for both families.
  await handleWordMotionWithOperator("word", "forward");
}
registerHandler("vi_op_word", vi_op_word);

async function vi_op_word_back(): Promise<void> {
  await handleWordMotionWithOperator("word", "backward");
}
registerHandler("vi_op_word_back", vi_op_word_back);

// Operator-pending e (word end) - select to word end, then apply operator
// Operator-pending e (word end) — uses native vi_move_word_end motion
async function vi_op_word_end(): Promise<void> {
  await handleWordMotionWithOperator("word", "end");
}
registerHandler("vi_op_word_end", vi_op_word_end);

async function vi_op_WORD(): Promise<void> {
  await handleWordMotionWithOperator("WORD", "forward");
}
registerHandler("vi_op_WORD", vi_op_WORD);

async function vi_op_WORD_back(): Promise<void> {
  await handleWordMotionWithOperator("WORD", "backward");
}
registerHandler("vi_op_WORD_back", vi_op_WORD_back);

async function vi_op_WORD_end(): Promise<void> {
  await handleWordMotionWithOperator("WORD", "end");
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

// NOTE: operator + `%` (d%/c%/y%) is currently a no-op — `goto_matching_bracket`
// has no entry in `OPERATOR_MOTIONS`, so the operator bails without deleting.
// Making it work now needs only a resolver returning the matching bracket's
// offset, with kind "inclusive". See test_vi_bug_d_percent_ignored.
async function vi_op_matching_bracket(): Promise<void> {
  await handleMotionWithOperator("goto_matching_bracket");
}
registerHandler("vi_op_matching_bracket", vi_op_matching_bracket);

async function vi_op_paragraph_up(): Promise<void> {
  await handleMotionWithOperator("move_to_paragraph_up");
}
registerHandler("vi_op_paragraph_up", vi_op_paragraph_up);

async function vi_op_paragraph_down(): Promise<void> {
  await handleMotionWithOperator("move_to_paragraph_down");
}
registerHandler("vi_op_paragraph_down", vi_op_paragraph_down);

function vi_cancel(): void {
  switchMode("normal");
}
registerHandler("vi_cancel", vi_cancel);

// ============================================================================
// Mode Definitions
// ============================================================================

// All mode definitions live in one function so they can be re-emitted
// when a binding-affecting setting changes (see the `config_changed`
// subscription below). `defineMode` replaces rather than accumulates —
// the host clears the mode's existing plugin defaults before
// re-registering — so calling this again is idempotent.
function defineViModes(): void {
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
    // Vim's `Y` is `yy`, not `y$`. It had no binding at all.
    ["Y", "vi_yank_line"],
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

    // Find-char motions (df/dt/cf/ct and backward dF/dT) — inclusive motions
    ["f", "vi_op_find_char_f"],
    ["t", "vi_op_find_char_t"],
    ["F", "vi_op_find_char_F"],
    ["T", "vi_op_find_char_T"],

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
    ...TEXT_OBJECT_KEYS.map(({ key, handler }): ModeBinding => [key, handler]),

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

    // Text objects (viw, vi", va(, …) — replace the selection with the object
    ["i", "vi_vis_text_object_inner"],
    ["a", "vi_vis_text_object_around"],

    // Operators
    ["d", "vi_vis_delete"],
    ["x", "vi_vis_delete"],
    ["c", "vi_vis_change"],
    ["s", "vi_vis_change"],
    ["y", "vi_vis_yank"],
    ["J", "vi_vis_join"],
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
    ["J", "vi_vis_join"],
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
    [">", "vi_vblock_indent"],
    ["<", "vi_vblock_dedent"],

    // Exit
    ["Escape", "vi_vblock_escape"],
    ["C-v", "vi_vblock_escape"], // Ctrl-v again exits visual-block mode

    // Pass through to standard editor shortcuts
    ["C-p", "command_palette"],
    ["C-q", "quit"],
  ], true);
}

defineViModes();

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
  resetModalState();
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

// Adopt Settings changes without an editor restart. `arrowKeys` and
// `searchWordUnderCursor` are baked into the mode binding tables at
// `defineMode` time, so re-reading them isn't enough — the modes have to
// be re-emitted. Guarded on an actual change: `config_changed` fires for
// every config save, and re-registering the tables on an unrelated one
// would be pure churn.
//
// `autoStart` is deliberately not applied live. It means "enable vi when
// the editor starts"; flipping vi on mid-session because a startup
// preference changed is a different behaviour than the setting promises.
editor.on("config_changed", () => {
  const cfg = (editor.getPluginConfig() ?? {}) as {
    arrowKeys?: boolean;
    searchWordUnderCursor?: boolean;
  };
  const nextArrowKeys = cfg.arrowKeys !== false;
  const nextSearchWord = cfg.searchWordUnderCursor !== false;
  if (nextArrowKeys === arrowKeys && nextSearchWord === searchWordUnderCursor) return;
  arrowKeys = nextArrowKeys;
  searchWordUnderCursor = nextSearchWord;
  defineViModes();
});

registerHandler("vi_to_brace", vi_to_brace);

// Vi's modal state belongs to the buffer it was entered in. A pending
// operator, a visual anchor and an insert session are all byte offsets into
// one buffer's text, so carrying them across a buffer switch would apply
// them to another buffer's bytes. Vim agrees: leaving a window drops the
// operator, the visual selection and insert mode.
//
// `memory` deliberately survives. What `.`, `;` and `n` replay is global in
// Vim too, and is stored as commands rather than offsets, so it stays valid.
//
// The hook does NOT mean "the active buffer changed". `set_active_buffer`
// early-returns when it did not, but `switch_split`, window activation and
// the in-place load of the initial empty buffer all fire it unconditionally —
// `switch_split` deliberately so, because focus moving between two splits on
// the same buffer never reaches `set_active_buffer` at all. Moving focus
// between two views of one file must not drop the selection being made in
// it, so the id is tracked here and an unchanged one is ignored.
let lastActiveBuffer: number | null = null;

// Buffers left holding a visual selection vi has since abandoned.
//
// Vi's modal state is global but the host's selection is per-buffer
// (`TextEdit::selection_anchor`), so returning to normal mode here cannot
// reach the selection in the buffer being left — and the plugin API has no
// way to address another buffer's selection. Left alone, coming back to that
// buffer would find normal mode over a live selection, where `x` extends it
// and cuts the lot. Instead, note the buffer and collapse it on the way back
// in. `setBufferCursor` is the collapse: the host treats a seated caret as a
// placement, and a placement drops the anchor.
const buffersWithAbandonedSelection = new Set<number>();

function isVisualMode(mode: ViMode): boolean {
  return mode === "visual" || mode === "visual-line" || mode === "visual-block";
}

// A closed buffer's id can be handed to the next buffer opened, and the note
// below outlives the buffer it was about — so the next occupant of that id
// would get its caret reseated on activation for no reason.
editor.on("buffer_closed", (args) => {
  buffersWithAbandonedSelection.delete(args.buffer_id);
});

editor.on("buffer_activated", (args) => {
  // Outside the `viModeEnabled` guard below: the buffer vi is later asked to
  // leave is usually opened before vi is turned on, and this is the only
  // place its id can be learned. `enableVi` cannot ask for it — `autoStart`
  // calls that from the top-level body, before any buffer exists.
  const bufferId = args.buffer_id;
  const previous = lastActiveBuffer;
  const changed = previous !== null && previous !== bufferId;
  lastActiveBuffer = bufferId;

  if (!viModeEnabled) return;

  if (changed && buffersWithAbandonedSelection.delete(bufferId)) {
    // Re-entering a buffer vi left mid-selection: collapse it before any
    // normal-mode command can extend it.
    editor.setBufferCursor(bufferId, editor.getCursorPosition());
  }

  if (!changed) return;

  const leaving = state.mode;
  if (isVisualMode(leaving) && previous !== null) {
    // The selection is in the buffer being *left*, not the one arriving.
    buffersWithAbandonedSelection.add(previous);
  }

  // Dropping the insert session rather than closing it through `switchMode`
  // is deliberate: the Escape-time capture measures the typed text from its
  // start offset to the cursor, and the cursor is already in the new buffer.
  // There is nothing left to record, and reading it would splice in
  // unrelated text — the failure #2443 was filed for.
  resetModalState();

  // Not `switchMode("normal")`, for a related reason: leaving a visual mode
  // there clears the selection with a `move_left`/`move_right` pair, which
  // would land on — and at offset 0 actually move — the cursor in the buffer
  // just opened.
  if (leaving !== "normal") {
    editor.setEditorMode("vi-normal");
  }
  // Unconditional: `resetModalState` also drops a half-typed count, which the
  // indicator is showing.
  editor.setStatus(getModeIndicator("normal"));
});
