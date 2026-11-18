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
type FindKind = "f" | "t" | "F" | "T";
type FindCharType = FindKind | null;
// `;` repeats the last f/t/F/T in its own direction, `,` in the opposite one.
type FindRepeat = ";" | ",";
type TextObjectType = "inner" | "around" | null;
type ModeBinding = [string, string];
type RegisterShape = "charwise" | "linewise" | "empty";
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
  // dfx, dtx, cfx (d; and d, are operator-motion: they name no character)
  | (ChangeCommon & { type: "operator-find-char"; operator: string; find: CharFind })
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
  // Visual-block's rectangle, as the anchor's and the moving corner's line
  // and byte column (both inclusive, like Vim's). Null outside `CTRL-V`.
  block: BlockCorners | null;
}

interface BlockCorners {
  anchorLine: number;
  anchorCol: number;
  headLine: number;
  headCol: number;
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
  lastFindChar: CharFind | null; // For ; and , repeat
  lastWordSearch: { text: string; direction: WordSearchDirection; wholeWord: boolean } | null; // For n/N after * or #
  // The shape of what the register holds, which decides how `p` puts it: on
  // lines of its own, inline, or (after yanking empty text) not at all.
  register: RegisterShape;
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
  register: "charwise",
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

const autoStart = editor.defineConfigBoolean("auto_start", {
  default: false,
  description:
    "Automatically enable vi mode when the editor starts. Default off — users opt in.",
});

// `let`, not `const`: both feed the mode binding tables built in
// `defineViModes()`, and the `config_changed` subscription at the bottom
// of this file re-reads them and re-emits the modes so a Settings change
// takes effect without an editor restart.
let arrowKeys = editor.defineConfigBoolean("arrow_keys", {
  default: true,
  description:
    "Enable arrow key navigation in vi mode.",
});

let searchWordUnderCursor = editor.defineConfigBoolean("search_word_under_cursor", {
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
  editor.setInputMode(`vi-${newMode}`);
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
      // Measured back from `searchEnd`: the chunk may start in the middle of
      // a character, but the text after the line break cannot.
      return searchEnd - editor.utf8ByteLength(text.slice(lineBreak + 1));
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
    runEnd: WORDRunEnd,
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
    const span = await lineSpanOfPositions(bufferId, origin, target);
    const range = await linewiseRangeAt(bufferId, span.firstLineStart, span.lineCount);
    // The range starts at its first line's start, but the operated text starts
    // at the earlier of the two positions, column included: `yk` leaves the
    // caret on the upper line at the motion's column, `yj` where it was.
    return range === null ? null : { ...range, startPosition: Math.min(origin, target) };
  }
  if (kind === "inclusive") {
    return target >= origin
      ? charwiseRange(origin, await charEndOffset(bufferId, target))
      : charwiseRange(target, await charEndOffset(bufferId, origin));
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
//
// A motion resolves to a target *and* its kind. For most motions the kind is
// fixed, but not for all: `;` repeats whatever find is current, so it is
// inclusive after `f`/`t` and exclusive after `F`/`T`. Resolving the two
// together is what lets `d;` be an ordinary motion, recorded and replayed by
// `.` like `dw`, instead of a special case of its own.
interface ResolvedMotion {
  target: number;
  kind: MotionKind;
}

interface OperatorMotion {
  // Resolved against the *operator's* semantics, which are not always the
  // caret's: `l` will not move the caret past a line's last character, but
  // `dl` deletes that character. Null when the motion fails (`fz` with no
  // `z` on the line, `j` on the last line), which cancels the operator.
  //
  // `origin` is always the caret. The host has no offset-to-line lookup, so
  // the line-relative motions (`j`, `k`, `_`, `+`, `-`, `$`) read the caret's
  // line instead of deriving it from `origin`; a caller resolving from
  // anywhere else would have to move the caret there first.
  //
  // `count` defaults to 1; `explicitCount` is what was actually typed, for
  // the motions where a bare command differs from a count of 1 (`G` vs `1G`).
  resolve: (origin: number, count: number, explicitCount: number | null) => Promise<ResolvedMotion | null>;
}

// The usual case: a motion whose kind does not depend on where it lands.
function fixedKindMotion(
  kind: MotionKind,
  resolveTarget: (origin: number, count: number, explicitCount: number | null) => Promise<number | null>,
): OperatorMotion {
  return {
    resolve: async (origin, count, explicitCount) => {
      const target = await resolveTarget(origin, count, explicitCount);
      return target === null ? null : { target, kind };
    },
  };
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

const BRACKET_PARTNERS: Record<string, string> = {
  "(": ")",
  "[": "]",
  "{": "}",
  ")": "(",
  "]": "[",
  "}": "{",
};

// How far either side of the caret `%` looks for the partner bracket. The
// search reads buffer text, so it is bounded: an unmatched bracket in a large
// file would otherwise read the whole file on every `%`.
const BRACKET_SEARCH_LIMIT = 1 << 20;

// `%` (`:help %`): the bracket under the caret, or else the next one on the
// line, and then its partner. Both reads start on a line start, a character
// boundary, so string indexes convert back to byte offsets exactly.
async function matchingBracketTarget(origin: number): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const bufferLength = editor.getBufferLength(bufferId);
  const lineStart = await findLineStartAtPosition(bufferId, origin);
  const after = await editor.getBufferText(
    bufferId,
    lineStart,
    Math.min(bufferLength, origin + BRACKET_SEARCH_LIMIT),
  );

  let index = byteOffsetToStringIndex(after, origin - lineStart);
  while (index < after.length && !isLineBreakAt(after, index) && BRACKET_PARTNERS[after[index]] === undefined) {
    index++;
  }
  const bracket = after[index];
  const partner = bracket === undefined ? undefined : BRACKET_PARTNERS[bracket];
  if (bracket === undefined || partner === undefined) {
    return null;
  }

  const forward = bracket === "(" || bracket === "[" || bracket === "{";
  // A closing bracket's partner is behind it, possibly on earlier lines.
  const textStart = forward
    ? lineStart
    : await findLineStartAtPosition(bufferId, Math.max(0, lineStart - BRACKET_SEARCH_LIMIT));
  const before = forward ? "" : await editor.getBufferText(bufferId, textStart, lineStart);
  const text = before + after;

  let depth = 0;
  for (let i = before.length + index; i >= 0 && i < text.length; i += forward ? 1 : -1) {
    if (text[i] === bracket) {
      depth++;
    } else if (text[i] === partner) {
      depth--;
      if (depth === 0) {
        return textStart + stringIndexToByteOffset(text, i);
      }
    }
  }
  return null;
}

// The end of the line's leading blanks (spaces and tabs), and whether that is
// the end of the line — a line of nothing but blanks, or an empty one.
async function leadingBlanksEnd(bufferId: number, lineStart: number): Promise<{ offset: number; atLineEnd: boolean }> {
  const bufferLength = editor.getBufferLength(bufferId);
  const sample = await editor.getBufferText(bufferId, lineStart, Math.min(bufferLength, lineStart + 4096));
  let index = 0;
  while (index < sample.length && (sample[index] === " " || sample[index] === "\t")) {
    index++;
  }
  // Blanks are one byte each, so the string index is the byte count.
  return { offset: lineStart + index, atLineEnd: index >= sample.length || isLineBreakAt(sample, index) };
}

// Where `I` inserts: just past the leading blanks (Vim's `BL_WHITE`), which on
// a line of nothing but blanks is the end of the line.
async function pastLeadingBlanks(bufferId: number, lineStart: number): Promise<number> {
  return (await leadingBlanksEnd(bufferId, lineStart)).offset;
}

// Where the caret lands for `^`, `_`, `+`, `-`, `gg`, `G` and after `>>` or a
// line-wise delete: the first non-blank, but never on the line terminator
// (`BL_WHITE | BL_FIX`), so a line of nothing but blanks lands on its last one.
async function firstNonBlankOffset(bufferId: number, lineStart: number): Promise<number> {
  const { offset, atLineEnd } = await leadingBlanksEnd(bufferId, lineStart);
  return atLineEnd && offset > lineStart ? offset - 1 : offset;
}

// The line a line-relative motion lands on: `delta` lines from `line`, clamped
// to the buffer. Like Vim's `cursor_down`/`cursor_up` it fails only when it
// cannot move at all — `j` or `+` on the last line, `k` or `-` on the first —
// while `5j` two lines from the end stops on the last line.
function movedLine(line: number, delta: number, lastLine: number): number | null {
  const target = Math.min(Math.max(line + delta, 0), lastLine);
  return delta !== 0 && target === line ? null : target;
}

// `_`, `+` and `-`: the first non-blank of the line `delta` lines from the
// caret's. Line-wise motions, so `d+` takes both lines.
async function firstNonBlankOfLineTarget(delta: number): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const line = editor.getPrimaryCursor()?.line ?? null;
  const lastLine = await lastContentLine(bufferId);
  const targetLine = line === null || lastLine === null ? null : movedLine(line, delta, lastLine);
  if (targetLine === null) {
    return null;
  }
  const lineStart = await editor.getLineStartPosition(targetLine);
  return lineStart === null ? null : firstNonBlankOffset(bufferId, lineStart);
}

// `j` and `k`: the line `delta` away, at the caret's column (or the line's
// last character boundary before it). Line-wise, so only the line matters to
// `d`, but `yk` leaves the caret at this column on the upper line.
async function verticalLineTarget(origin: number, delta: number): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const line = editor.getPrimaryCursor()?.line ?? null;
  const lastLine = await lastContentLine(bufferId);
  const targetLine = line === null || lastLine === null ? null : movedLine(line, delta, lastLine);
  if (targetLine === null) {
    return null;
  }
  const column = origin - await findLineStartAtPosition(bufferId, origin);
  return offsetAtColumn(bufferId, targetLine, column);
}

// `gg` and `G`: line `explicitCount` (1-based), or without one the first or
// the last line with content; on its first non-blank (`'startofline'`).
// Never fails: the count is clamped to the buffer.
async function documentLineTarget(explicitCount: number | null, toEnd: boolean): Promise<number | null> {
  const bufferId = editor.getActiveBufferId();
  const lastLine = await lastContentLine(bufferId);
  if (lastLine === null) {
    return null;
  }
  const targetLine = explicitCount === null
    ? (toEnd ? lastLine : 0)
    : Math.min(Math.max(0, explicitCount - 1), lastLine);
  const lineStart = await editor.getLineStartPosition(targetLine);
  return lineStart === null ? null : firstNonBlankOffset(bufferId, lineStart);
}

async function firstNonBlankTarget(origin: number): Promise<number> {
  const bufferId = editor.getActiveBufferId();
  return firstNonBlankOffset(bufferId, await findLineStartAtPosition(bufferId, origin));
}

const OPERATOR_MOTIONS: Record<string, OperatorMotion> = {
  goto_matching_bracket: fixedKindMotion("inclusive", (o) => matchingBracketTarget(o)),
  vi_first_non_blank: fixedKindMotion("exclusive", (o) => firstNonBlankTarget(o)),
  // `[count]_` is the line count-1 below, so a bare `d_` is `dd`. Under an
  // operator Vim's `_` keeps the caret's column (`y_` leaves the caret where
  // it is); only the bare motion goes to the first non-blank, which
  // `vi_first_non_blank_line` does itself.
  vi_first_non_blank_line: fixedKindMotion("linewise", (o, c) => verticalLineTarget(o, c - 1)),
  vi_first_non_blank_next_line: fixedKindMotion("linewise", (_o, c) => firstNonBlankOfLineTarget(c)),
  vi_first_non_blank_prev_line: fixedKindMotion("linewise", (_o, c) => firstNonBlankOfLineTarget(-c)),
  vi_find_repeat: { resolve: (o, c) => findRepeatMotion(o, c, ";") },
  vi_find_repeat_reverse: { resolve: (o, c) => findRepeatMotion(o, c, ",") },
  // Line-wise: an operator over these takes whole lines, newline included,
  // not the byte span between the two positions.
  move_down: fixedKindMotion("linewise", (o, c) => verticalLineTarget(o, c)),
  move_up: fixedKindMotion("linewise", (o, c) => verticalLineTarget(o, -c)),
  move_document_start: fixedKindMotion("linewise", (_o, _c, explicit) => documentLineTarget(explicit, false)),
  move_document_end: fixedKindMotion("linewise", (_o, _c, explicit) => documentLineTarget(explicit, true)),
  move_left: fixedKindMotion("exclusive", (o, c) => horizontalTarget(o, c, false, false)),
  move_right: fixedKindMotion("exclusive", (o, c) => horizontalTarget(o, c, true, false)),
  move_line_start: fixedKindMotion("exclusive", (o) => lineStartTarget(o)),
  // `$` is inclusive in Vim's vocabulary, but the editor's line end already
  // sits one past the last character, which is where an exclusive range ends.
  move_line_end: fixedKindMotion("exclusive", (o) => lineEndTarget(o)),
  move_to_paragraph_up: fixedKindMotion("exclusive", (o, c) => paragraphTarget(o, c, false)),
  move_to_paragraph_down: fixedKindMotion("exclusive", (o, c) => paragraphTarget(o, c, true)),
};

// Move the caret the way the motion `name` moves an operator. Only for the
// motions whose caret target and operator target agree (`%`, `^`, `+`, `-`,
// `;`, `,`); `l`, `$` and `_` are the counter-examples. A failed motion
// leaves the caret where it is.
async function moveByMotion(name: string, count: number, explicitCount: number | null = null): Promise<void> {
  const motion = OPERATOR_MOTIONS[name];
  const origin = editor.getCursorPosition();
  if (motion === undefined || origin === null) {
    return;
  }
  const resolved = await motion.resolve(origin, count, explicitCount);
  if (resolved !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), resolved.target);
  }
}


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

// A keyword character for `w`, `b`, `e`, `iw` and `aw`: a letter or digit
// in any script, or `_`, as Vim's default 'iskeyword' takes it in UTF-8.
function isWordChar(char: string | undefined): boolean {
  return char !== undefined && /^[\p{L}\p{N}\p{M}_]/u.test(char);
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

// The last character of the WORD (run of non-blanks) that `index` is in.
// `index` must point at a non-whitespace character.
function WORDRunEnd(text: string, index: number): number {
  while (true) {
    const next = nextStringIndex(text, index);
    if (next >= text.length || isWhitespaceAt(text, next)) {
      return index;
    }
    index = next;
  }
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

// The start of the line `position` is on, where a position at or past the
// end of a buffer that ends in a newline counts as the last line with content
// (the editor's empty line after it is not a line in Vim's model).
async function lineStartAtOrBeforeEnd(bufferId: number, position: number): Promise<number> {
  const length = editor.getBufferLength(bufferId);
  const clamped = Math.min(position, length);
  const lineStart = await findLineStartAtPosition(bufferId, clamped);
  return lineStart >= length && length > 0 ? findLineStartAtPosition(bufferId, length - 1) : lineStart;
}

// `position` moved back onto the last character of its line when it sits at
// the line's end, where Vim's normal mode never leaves the caret (`x` on the
// last character, `d$`, `di(` at a line end). An empty line keeps its start.
async function onLineCharacter(bufferId: number, position: number): Promise<number> {
  const length = editor.getBufferLength(bufferId);
  const clamped = Math.min(position, length);
  const lineStart = await findLineStartAtPosition(bufferId, clamped);
  if (clamped === lineStart) {
    return clamped;
  }
  const next = clamped < length ? await editor.getBufferText(bufferId, clamped, clamped + 1) : "";
  if (next !== "" && next !== "\n" && next !== "\r") {
    return clamped;
  }
  const line = await editor.getBufferText(bufferId, lineStart, clamped);
  return lineStart + editor.utf8ByteLength(line.slice(0, previousStringIndex(line, line.length)));
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
  // Where the operated text starts, column included, when that is not
  // `start`: a line-wise range from a motion starts at its first line's start,
  // but Vim's `y` leaves the caret at the earlier motion position.
  startPosition?: number;
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

// The operators `.` repeats: every one that changes the buffer, so not `y`.
function isRepeatableOperator(operator: string): boolean {
  return operator === "d" || operator === "c" || operator === ">" || operator === "<";
}

// The single place `d`, `c`, `y`, `>` and `<` act on a range.
//
// Every entry point — operator + motion, operator + text object, operator +
// find-char, the doubled line operators, the visual modes and `.` — resolves
// its own range and hands it here. That is what keeps the register shape, the
// caret's landing place and the `c`-before-insert flush from drifting apart
// between them, which they did when there were four copies of this switch.
//
// A null range is a motion or object that failed (`fz` with no `z`, `dk` on
// the first line): nothing happens. An *empty* range is a motion that
// succeeded without covering anything (`cT` next to its target, `c0` or `>0`
// in column 0, `s` or `C` on an empty line, `ci(` on `()`), and each operator does what Vim
// does with it: `c` still opens its insert, `>`/`<` still shift the line, `y`
// yanks the empty text, and `d` has nothing to delete. `d` and `c` over
// nothing leave the register alone.
async function applyOperator(
  operator: string,
  range: OperatorRange | null,
  options: { enterInsert?: boolean } = {},
): Promise<void> {
  if (range === null) {
    switchMode("normal");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  if (operator !== "y" && isActiveBufferEditingDisabled(bufferId)) {
    switchMode("normal");
    return;
  }

  // Indent and dedent are line-wise whatever the range's shape: they resolve it
  // to the whole lines it touches (the caret's own, for an empty range) and
  // shift those.
  if (operator === ">" || operator === "<") {
    const span = await lineSpanOfRange(bufferId, range.start, range.end);
    await applyIndentToLineRange(operator, span.firstLineStart, span.lineCount);
    return;
  }

  const empty = range.end <= range.start;
  const linewise = range.shape === "linewise";
  const text = empty ? "" : (await editor.getBufferText(bufferId, range.start, range.end)) ?? "";
  const terminator = linewise ? await getLinewiseTerminator(bufferId, range.start, text) : "";
  if (!empty || operator === "y") {
    editor.setClipboard(linewise ? ensureLinewiseRegisterText(text, terminator) : text);
    memory.register = text === "" ? "empty" : linewise ? "linewise" : "charwise";
  }

  // The caret is placed *after* the mode switch throughout: leaving a visual
  // mode, `switchMode` drops the host selection by nudging the caret left then
  // right, which is a net move at offset 0. Placing afterwards makes the
  // landing position below the authoritative one for every caller.
  switch (operator) {
    case "y": {
      // Vim leaves the caret at the start of the yanked text (`yb` from the
      // middle of a line lands where the yank began, `yk` on the upper line at
      // its column). `yy` and `Vy` are the exceptions and pass their own
      // `cursorAfter`.
      const landing = range.cursorAfter ?? range.startPosition ?? range.start;
      switchMode("normal");
      editor.setBufferCursor(bufferId, landing);
      return;
    }

    case "d": {
      if (!empty) {
        editor.deleteRange(bufferId, range.start, range.end);
      }
      // A line-wise delete leaves the caret on the first non-blank of the line
      // that moved up into place (or of the new last line), as Vim's does.
      let landing: number;
      if (linewise && range.cursorAfter === undefined) {
        await editor.flush();
        landing = await firstNonBlankOffset(bufferId, await lineStartAtOrBeforeEnd(bufferId, range.start));
      } else {
        await editor.flush();
        landing = await onLineCharacter(bufferId, range.cursorAfter ?? range.start);
      }
      switchMode("normal");
      editor.setBufferCursor(bufferId, landing);
      return;
    }

    case "c": {
      if (range.end > range.start) {
        editor.deleteRange(bufferId, range.start, range.end);
      }
      if (linewise) {
        // Vim's `cc`/`cj`/`Vc` leave an empty line to type into, rather than
        // closing the gap the way `d` does.
        editor.insertText(bufferId, range.start, getLinewiseReplacementText(text) ?? terminator);
      }
      editor.setBufferCursor(bufferId, range.start);
      // `.` (`replayOperator`) wants the range consumed with `c` semantics —
      // the empty line a line-wise change leaves behind — without the mode
      // switch, and types the recorded text itself.
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

// The lines from the one `a` is on to the one `b` is on, both included, in
// either order — for a line-wise motion, whose two ends are positions *on*
// lines rather than an exclusive end (`lineSpanOfRange` would drop the later
// line when that end sits at its line start).
async function lineSpanOfPositions(
  bufferId: number,
  a: number,
  b: number,
): Promise<{ firstLineStart: number; lineCount: number }> {
  const firstLineStart = await findLineStartAtPosition(bufferId, Math.min(a, b));
  const lastLineStart = await findLineStartAtPosition(bufferId, Math.max(a, b));
  const between = await editor.getBufferText(bufferId, firstLineStart, lastLineStart);
  return { firstLineStart, lineCount: 1 + countLineTerminators(between) };
}

async function placeCursorAtFirstNonBlank(bufferId: number, lineStart: number): Promise<void> {
  editor.setBufferCursor(bufferId, await firstNonBlankOffset(bufferId, lineStart));
}

// Indent (">") or dedent ("<") `lineCount` whole lines starting at
// `firstLineStart`, leave the cursor on the first non-blank of the first line,
// and return to normal mode.
// `>`/`<` over `lineCount` lines from `firstLineStart` (Vim's `op_shift`).
// Each line's indent, measured in columns with tabs expanded, grows or
// shrinks by one shift width (the buffer's `tab_size`, never below zero) and
// is written back as tabs or spaces per the buffer's `use_tabs`. Empty lines
// are left alone, as in Vim. The lines are replaced in one edit, so `u`
// undoes the whole shift.
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

  const end = await findLinewiseEndFromStart(bufferId, firstLineStart, Math.max(1, lineCount));
  const text = await editor.getBufferText(bufferId, firstLineStart, end);
  const info = editor.getBufferInfo(bufferId);
  const width = info !== null && info.tab_size > 0 ? info.tab_size : 4;
  const useTabs = info?.use_tabs ?? false;
  const shifted = text
    .split(/(\r\n|\n|\r)/)
    .map((piece, i) => (i % 2 === 1 || piece === "" ? piece : shiftLine(piece, operator === ">", width, useTabs)))
    .join("");
  if (shifted !== text) {
    await editor.replaceRange(bufferId, firstLineStart, end, shifted);
  }

  // Vim leaves the caret on the first line's first non-blank.
  switchMode("normal");
  await editor.flush();
  await placeCursorAtFirstNonBlank(bufferId, firstLineStart);
}

// One line shifted a shift width right or left (see `applyIndentToLineRange`).
function shiftLine(line: string, right: boolean, width: number, useTabs: boolean): string {
  let column = 0;
  let index = 0;
  for (; index < line.length && (line[index] === " " || line[index] === "\t"); index++) {
    column = line[index] === "\t" ? (Math.floor(column / width) + 1) * width : column + 1;
  }
  const indent = right ? column + width : Math.max(0, column - width);
  const prefix = useTabs
    ? "\t".repeat(Math.floor(indent / width)) + " ".repeat(indent % width)
    : " ".repeat(indent);
  return prefix + line.slice(index);
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
): Promise<{ start: number; end: number } | null> {
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

  if (kind === "forward") {
    // `w` under an operator (`:help word`): the last word moved over stops at
    // the end of its line, so `dw` on a line's last word leaves the line
    // break alone and `yw` there yanks just the word. Earlier counts move as
    // the plain motion does.
    let index = startIndex;
    for (let i = 1; i < Math.max(1, count); i++) {
      index = WORD_FAMILIES[family].forward(bufferText, index);
    }
    const endIndex = forwardWordEndOnLine(family, bufferText, index);
    return { start, end: stringIndexToByteOffset(bufferText, endIndex) };
  }

  const targetIndex = computeWordMotionTargetIndex(family, bufferText, startIndex, kind, count);
  const target = stringIndexToByteOffset(bufferText, targetIndex);
  return {
    start,
    end: kind === "end" ? target + byteLengthOfCharAt(bufferText, targetIndex) : target,
  };
}

// Where the last `w` of an operator stops (Vim's `fwd_word` with `eol`): past
// the word under `index` and the blanks after it, but no further than the end
// of the line. On an empty line it is the next line's start, which the
// exclusive-linewise rule then turns into the whole line (`dw` on an empty
// line deletes it).
function forwardWordEndOnLine(family: WordFamily, text: string, index: number): number {
  if (index >= text.length) {
    return text.length;
  }
  if (isLineBreakAt(text, index)) {
    return text.startsWith("\r\n", index) ? index + 2 : index + 1;
  }
  if (!isWhitespaceAt(text, index)) {
    index = nextStringIndex(text, WORD_FAMILIES[family].runEnd(text, index));
  }
  while (index < text.length && (text[index] === " " || text[index] === "\t")) {
    index++;
  }
  return index;
}


// The range `operator` takes over a word motion. `c` uses Vim's `cw`-is-`ce`
// rule; the caret adjustment `computeWordOperatorRange` asks for is `d`'s.
async function wordOperatorRange(
  family: WordFamily,
  operator: string,
  kind: WORDMotionKind,
  count: number,
): Promise<OperatorRange | null> {
  const range = await computeWordOperatorRange(family, kind, count, operator === "c");
  if (range === null) {
    return null;
  }
  // Every word motion but `e` is exclusive, so an end on a later line's start
  // follows the exclusive-linewise rule (`dw` on an empty line).
  return kind === "end"
    ? charwiseRange(range.start, range.end)
    : exclusiveOperatorRange(editor.getActiveBufferId(), range.start, range.end);
}

async function applyWordOperatorMotion(
  family: WordFamily,
  operator: string,
  kind: WORDMotionKind,
  count: number,
): Promise<void> {
  await applyOperator(operator, await wordOperatorRange(family, operator, kind, count));
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
  if (isRepeatableOperator(operator)) {
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

// Operator + named motion (`dw` aside, which has its own word path): resolve
// the motion from the caret, record it for `.`, apply the operator. `count`
// is how many times to apply the motion (d3j). `.` does not come through
// here: it resolves the same range with `motionOperatorRange` and hands it to
// `replayOperator`, so a replay never records.
async function applyOperatorWithMotion(
  operator: string,
  motionAction: string,
  count: number = 1,
  explicitCount: number | null = null,
): Promise<void> {
  const range = await motionOperatorRange(motionAction, count, explicitCount);
  if (range === null) {
    // The motion could not move — `dk` on the first line, `fz` with no `z` —
    // so the whole operator fails, and `.` keeps whatever it was holding.
    switchMode("normal");
    return;
  }
  // Record for '.' only once the motion has resolved (every operator but
  // yank). The explicit count is what is kept: `.` after a bare `dG`
  // must not turn into `d1G`.
  if (isRepeatableOperator(operator)) {
    recordChange({ type: "operator-motion", operator, motion: motionAction, count: explicitCount ?? undefined });
  }
  await applyOperator(operator, range);
}

// The range an operator takes over the named motion from the caret, or null
// when the motion fails.
async function motionOperatorRange(
  motionAction: string,
  count: number,
  explicitCount: number | null,
): Promise<OperatorRange | null> {
  const motion = OPERATOR_MOTIONS[motionAction];
  if (motion === undefined) {
    editor.debug(`No operator motion for: ${motionAction}`);
    return null;
  }
  const origin = editor.getCursorPosition();
  const resolved = origin === null ? null : await motion.resolve(origin, count, explicitCount);
  if (origin === null || resolved === null) {
    return null;
  }
  return rangeFromMotion(origin, resolved.target, resolved.kind);
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
  await moveByMotion("vi_first_non_blank", 1);
}
registerHandler("vi_first_non_blank", vi_first_non_blank);

// `I` and its `.` replay: just past the caret line's leading blanks, which on
// a line of nothing but blanks is its end — not `^`, which never lands on
// the line terminator.
async function moveCaretPastLeadingBlanks() : Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const origin = editor.getCursorPosition();
  if (origin === null) {
    return;
  }
  editor.setBufferCursor(bufferId, await pastLeadingBlanks(bufferId, await findLineStartAtPosition(bufferId, origin)));
}

// `_` lands on the first non-blank of the line count-1 below; `+` and `-`
// move exactly as they do for an operator (`d+`, `d-`).
async function vi_first_non_blank_line() : Promise<void> {
  const target = await firstNonBlankOfLineTarget(consumeCount() - 1);
  if (target !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), target);
  }
}
registerHandler("vi_first_non_blank_line", vi_first_non_blank_line);

async function vi_first_non_blank_next_line() : Promise<void> {
  await moveByMotion("vi_first_non_blank_next_line", consumeCount());
}
registerHandler("vi_first_non_blank_next_line", vi_first_non_blank_next_line);

async function vi_first_non_blank_prev_line() : Promise<void> {
  await moveByMotion("vi_first_non_blank_prev_line", consumeCount());
}
registerHandler("vi_first_non_blank_prev_line", vi_first_non_blank_prev_line);

// `gg` and `G` go to the first non-blank of their line (Vim's 'startofline'):
// the first and the last line, or line N with a count. They resolve through
// the same motions `dgg` and `dG` take.
async function vi_doc_start() : Promise<void> {
  const explicitCount = state.count;
  consumeCount();
  await moveByMotion("move_document_start", 1, explicitCount);
}
registerHandler("vi_doc_start", vi_doc_start);

async function vi_doc_end() : Promise<void> {
  const explicitCount = state.count;
  consumeCount();
  await moveByMotion("move_document_end", 1, explicitCount);
}
registerHandler("vi_doc_end", vi_doc_end);

function vi_page_down() : void {
  executeWithCount("move_page_down");
}
registerHandler("vi_page_down", vi_page_down);

function vi_page_up() : void {
  executeWithCount("move_page_up");
}
registerHandler("vi_page_up", vi_page_up);

// `%` moves where `d%` acts: see `matchingBracketTarget`. (The core
// `goto_matching_bracket` jumps to the enclosing pair when the caret is off a
// bracket, so `%` and `d%` disagreed.) `N%` (go to N percent) is not
// supported; the count is dropped.
async function vi_matching_bracket() : Promise<void> {
  consumeCount();
  await moveByMotion("goto_matching_bracket", 1);
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

/** A character the editor's search counts as part of a word (its `\w`
 *  and `\b`): Unicode letters, marks, digits and connectors. `*` and `#`
 *  pick their word by it, so it is the word the whole-word search finds —
 *  `caf` in `café caf` is not a word, and `café` is. */
function isSearchWordChar(char: string | undefined): boolean {
  return char !== undefined && /[\p{Alphabetic}\p{M}\p{Nd}\p{Pc}]/u.test(char);
}

function findSearchTargetUnderCursor(text: string, cursorIndex: number, scanForwardOnMiss: boolean = false): WordSearchTarget | null {
  const originalIndex = Math.min(cursorIndex, Math.max(0, text.length - 1));
  const lineEnd = findLineEndIndex(text, originalIndex);
  let index = originalIndex;

  if (!scanForwardOnMiss && !isSearchWordChar(text[index]) && index > 0 && isSearchWordChar(text[index - 1])) {
    index--;
  }
  if (scanForwardOnMiss && !isSearchWordChar(text[index])) {
    while (index < lineEnd && !isSearchWordChar(text[index])) {
      index = nextStringIndex(text, index);
    }
  }
  if (isSearchWordChar(text[index])) {
    let start = index;
    let end = index + 1;
    while (start > 0 && isSearchWordChar(text[start - 1])) {
      start--;
    }
    while (end < text.length && isSearchWordChar(text[end])) {
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

/** The line the cursor is on: where it starts, its text with its line
 * break, and the cursor's index in that text. */
async function readCursorLine(bufferId: number, cursorPos: number): Promise<{ start: number; text: string; cursorIndex: number }> {
  const start = await findLineStartAtPosition(bufferId, cursorPos);
  const end = await findLinewiseEndFromStart(bufferId, start, 1);
  const text = await editor.getBufferText(bufferId, start, end);
  return { start, text, cursorIndex: byteOffsetToStringIndex(text, cursorPos - start) };
}

/** Where `target` (found in the line starting at `lineStart`) is in the
 * buffer, in bytes. */
function targetByteRange(lineStart: number, lineText: string, target: WordSearchTarget): { start: number; end: number } {
  return {
    start: lineStart + stringIndexToByteOffset(lineText, target.start),
    end: lineStart + stringIndexToByteOffset(lineText, target.end),
  };
}

// `*`, `#`, `n` and `N` after them: the next (or previous) match of `target`,
// wrapping around, found with the editor's search, so only the cursor's line
// is read rather than the whole buffer. `currentTarget` is the occurrence the
// search starts from, in bytes; without it, the one under the cursor if that
// is `target`.
async function executeStoredWordSearch(
  target: string,
  direction: WordSearchDirection,
  count: number,
  wholeWord: boolean,
  currentTarget?: { start: number; end: number },
): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return;
  }

  let at = currentTarget;
  if (at === undefined) {
    const line = await readCursorLine(bufferId, cursorPos);
    const underCursor = findSearchTargetUnderCursor(line.text, line.cursorIndex);
    if (underCursor !== null && underCursor.text === target && underCursor.wholeWord === wholeWord) {
      at = targetByteRange(line.start, line.text, underCursor);
    }
  }
  // Matches start on character boundaries, so the byte after the cursor
  // stands for the character after it.
  let fromStart = at?.start ?? cursorPos;
  let fromEnd = at?.end ?? cursorPos + 1;

  const matches = await editor.searchBuffer(bufferId, target, { wholeWords: wholeWord });
  let match: GrepMatch | undefined;
  for (let i = 0; i < count; i++) {
    if (direction === "forward") {
      match = matches.find((m) => m.byteOffset >= fromEnd)
        ?? (matches.length > 0 && matches[0].byteOffset < fromStart ? matches[0] : undefined);
    } else {
      const before = matches.filter((m) => m.byteOffset < fromStart);
      const last = matches.length > 0 ? matches[matches.length - 1] : undefined;
      match = before.length > 0
        ? before[before.length - 1]
        : (last !== undefined && last.byteOffset >= fromEnd ? last : undefined);
    }

    if (match === undefined) {
      return;
    }

    fromStart = match.byteOffset;
    fromEnd = match.byteOffset + match.length;
  }

  if (match !== undefined) {
    editor.setBufferCursor(bufferId, match.byteOffset);
  }
}

async function executeWordUnderCursorSearch(direction: WordSearchDirection, count: number): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return;
  }

  const line = await readCursorLine(bufferId, cursorPos);
  const currentTarget = findSearchTargetUnderCursor(line.text, line.cursorIndex, true);
  if (currentTarget === null) {
    return;
  }

  memory.lastWordSearch = { text: currentTarget.text, direction, wholeWord: currentTarget.wholeWord };
  const at = targetByteRange(line.start, line.text, currentTarget);
  editor.setBufferCursor(bufferId, at.start);
  await executeStoredWordSearch(currentTarget.text, direction, count, currentTarget.wholeWord, at);
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
  await moveCaretPastLeadingBlanks();
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
  const count = consumeCount();
  recordChange({ type: "line-op", action: "indent_line", count });
  await applyLineOpIndent(">", count);
}
registerHandler("vi_indent_line", vi_indent_line);

async function vi_dedent_line() : Promise<void> {
  const count = consumeCount();
  recordChange({ type: "line-op", action: "dedent_line", count });
  await applyLineOpIndent("<", count);
}
registerHandler("vi_dedent_line", vi_dedent_line);

// An operator key after an operator: the same one doubled (`dd`, `cc`, `yy`,
// `>>`, `<<`) acts on lines; a different one (`dy`, `c>`) cancels the
// pending operator, as in Vim.
function doubledOperator(operator: string, lineOperation: () => Promise<void>): () => Promise<void> {
  return async () => {
    if (state.pending?.operator !== operator) {
      switchMode("normal");
      return;
    }
    await lineOperation();
  };
}
registerHandler("vi_op_doubled_d", doubledOperator("d", vi_delete_line));
registerHandler("vi_op_doubled_c", doubledOperator("c", vi_change_line));
registerHandler("vi_op_doubled_y", doubledOperator("y", vi_yank_line));
registerHandler("vi_op_doubled_indent", doubledOperator(">", vi_indent_line));
registerHandler("vi_op_doubled_dedent", doubledOperator("<", vi_dedent_line));

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
  editor.setInputMode("vi-replace-char");
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
  const bufferId = editor.getActiveBufferId();
  const position = editor.getCursorPosition();
  if (position === null) {
    return;
  }
  const sample = (await safeGetBufferText(bufferId, position, position + count * 4)) ?? "";
  const replaced = [...sample].slice(0, count).join("");
  const text = replacement.repeat(count);
  // One edit, so one `u` undoes it.
  await editor.replaceRange(bufferId, position, position + editor.utf8ByteLength(replaced), text);
  // The caret stays on the last replaced character (Vim).
  editor.setBufferCursor(
    bufferId,
    position + editor.utf8ByteLength(text) - editor.utf8ByteLength(replacement),
  );
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
// `p` and `P`. A line-wise register goes on lines of its own, below or above
// the caret's line, and the caret lands on the first non-blank of the first
// pasted line (Vim). An empty register (yanked from an empty range) puts
// nothing and leaves the caret where it is.
async function vi_paste_after() : Promise<void> {
  if (memory.register === "empty") {
    return;
  }
  if (memory.register === "linewise") {
    // The yanked text includes its trailing line break, which pushes the
    // following lines down.
    editor.executeAction("move_down");
    editor.executeAction("move_line_start");
    await pasteLinewiseAtCaret();
  } else {
    // Character-wise paste: after the character under the caret. On an empty
    // line there is none, and the paste goes where the caret is (a plain
    // `move_right` would wrap onto the next line first).
    const bufferId = editor.getActiveBufferId();
    const caret = editor.getCursorPosition();
    if (caret !== null) {
      const under = await editor.getBufferText(bufferId, caret, Math.min(caret + 1, editor.getBufferLength(bufferId)));
      if (under !== "" && under !== "\n" && under !== "\r") {
        editor.setBufferCursor(bufferId, await charEndOffset(bufferId, caret));
      }
    }
    await pasteCharwiseAtCaret();
  }
}
registerHandler("vi_paste_after", vi_paste_after);

async function vi_paste_before() : Promise<void> {
  if (memory.register === "empty") {
    return;
  }
  if (memory.register === "linewise") {
    editor.executeAction("move_line_start");
    await pasteLinewiseAtCaret();
  } else {
    // Character-wise paste: insert at cursor
    await pasteCharwiseAtCaret();
  }
}

// Ctrl+C in normal mode is the editor's own copy, which takes the whole
// line when nothing is selected. Record that shape so `p` puts the line on a
// line of its own, as it does after `yy`. A selection of whole lines (Ctrl+L)
// is line-wise too.
async function vi_copy() : Promise<void> {
  const selection = editor.getPrimaryCursor()?.selection ?? null;
  let shape: RegisterShape = "linewise";
  if (selection !== null && selection.end > selection.start) {
    const bufferId = editor.getActiveBufferId();
    const text = (await editor.getBufferText(bufferId, selection.start, selection.end)) ?? "";
    const startsLine = (await findLineStartAtPosition(bufferId, selection.start)) === selection.start;
    shape = startsLine && /(\r\n|\n|\r)$/.test(text) ? "linewise" : "charwise";
  }
  editor.executeAction("copy");
  memory.register = shape;
}
registerHandler("vi_copy", vi_copy);

// Paste the character-wise register at the caret. Vim leaves the caret on
// the last pasted character, or at the start of the pasted text when it
// spans lines.
async function pasteCharwiseAtCaret(): Promise<void> {
  await editor.flush();
  const bufferId = editor.getActiveBufferId();
  const pasteStart = editor.getCursorPosition();
  editor.executeAction("paste");
  await editor.flush();
  const pasteEnd = editor.getCursorPosition();
  if (pasteStart === null || pasteEnd === null || pasteEnd <= pasteStart) {
    return;
  }
  const pasted = await editor.getBufferText(bufferId, pasteStart, pasteEnd);
  if (/[\r\n]/.test(pasted)) {
    editor.setBufferCursor(bufferId, pasteStart);
  } else {
    editor.setBufferCursor(bufferId, pasteStart + editor.utf8ByteLength(pasted.slice(0, previousStringIndex(pasted, pasted.length))));
  }
}

// Paste the line-wise register at the caret, a line start, and land on the
// first non-blank of the first pasted line.
async function pasteLinewiseAtCaret(): Promise<void> {
  await editor.flush();
  const bufferId = editor.getActiveBufferId();
  const pasteStart = editor.getCursorPosition();
  editor.executeAction("paste");
  await editor.flush();
  if (pasteStart !== null) {
    editor.setBufferCursor(bufferId, await firstNonBlankOffset(bufferId, pasteStart));
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
  // A count typed before `.` replaces the recorded one (`3.`).
  const explicitCount = state.count ?? change.count ?? null;
  const count = consumeCountOrDefault(change.count ?? 1);
  await replayChange(change, count, explicitCount);
}

// Replay a recorded operator over `range`, which the replay has resolved
// afresh at the caret, as Vim's `.` re-executes the command. A range that no
// longer resolves aborts the whole repeat, recorded text included. `c` takes
// the range with its own semantics (a line-wise change leaves an empty line to
// type into) but without entering insert: the replay types the recorded text
// itself and stays in normal mode.
async function replayOperator(operator: string, range: OperatorRange | null, insertedText?: string): Promise<void> {
  if (range === null) {
    switchMode("normal");
    return;
  }
  await applyOperator(operator, range, { enterInsert: false });
  if (operator === "c" && insertedText) {
    editor.insertAtCursor(insertedText);
    // Leave the cursor on the last inserted character, exactly as Escape
    // does when the original insert ended.
    editor.executeAction("move_left_in_line");
  }
}

async function replayChange(change: LastChange, count: number, explicitCount: number | null): Promise<void> {
  switch (change.type) {
    case "simple": {
      // Simple actions like x, X, s
      if (change.action === "substitute") {
        await replayOperator("c", await charwiseCountRange(count, true), change.insertedText);
      } else if (change.action === "delete_forward") {
        await replayOperator("d", await charwiseCountRange(count, true));
      } else if (change.action === "delete_backward") {
        await replayOperator("d", await charwiseCountRange(count, false));
      } else if (change.action === "join_lines") {
        await joinLines(Math.max(1, count - 1));
      } else if (change.action) {
        executeWithCount(change.action, count);
      }
      break;
    }

    case "line-op": {
      // Line operations like dd, cc
      if (change.action === "delete_line") {
        await replayOperator("d", await lineOperatorRange(count));
      } else if (change.action === "change_line") {
        await replayOperator("c", await lineOperatorRange(count), change.insertedText);
      } else if (change.action === "indent_line") {
        await applyLineOpIndent(">", count);
      } else if (change.action === "dedent_line") {
        await applyLineOpIndent("<", count);
      }
      break;
    }

    case "operator-motion": {
      // Operator + motion like dw, cw, d$, dj, d;
      const wordMotion = wordMotionFromRepeatMotion(change.motion);
      const range = wordMotion
        ? await wordOperatorRange(wordMotion.family, change.operator, wordMotion.kind, count)
        : await motionOperatorRange(change.motion, count, explicitCount);
      await replayOperator(change.operator, range, change.insertedText);
      break;
    }

    case "operator-textobj": {
      // Operator + text object like diw, ci"
      const range = await textObjectOperatorRange(change.textObject.object, change.textObject.modifier);
      await replayOperator(change.operator, range, change.insertedText);
      break;
    }

    case "operator-find-char": {
      // Operator + find-char like dfx, dtx, cfx
      await replayOperator(change.operator, await findOperatorRange(change.find, count), change.insertedText);
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
          await moveCaretPastLeadingBlanks();
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
  const lines = Math.min(joins, lastLine - line);
  if (lines <= 0) {
    return;
  }
  const start = await editor.getLineStartPosition(line);
  const end = await editor.getLineEndPosition(line + lines);
  if (start === null || end === null) {
    return;
  }
  const text = (await editor.getBufferText(bufferId, start, end)) ?? "";
  // Vim's `J`: each following line loses its leading blanks and is joined
  // with one space, or none when the line so far already ends in a blank,
  // the next line is empty, or it starts with `)`. The caret lands where the
  // last join happened.
  const pieces = text.split(/\r\n|\n|\r/);
  let joined = pieces[0];
  let joinAt = 0;
  for (const piece of pieces.slice(1)) {
    const next = piece.replace(/^[ \t]+/, "");
    joinAt = editor.utf8ByteLength(joined);
    const separator = next === "" || /[ \t]$/.test(joined) || next.startsWith(")") ? "" : " ";
    joined += separator + next;
  }
  // One edit, so one `u` undoes the whole join.
  await editor.replaceRange(bufferId, start, end, joined);
  editor.setBufferCursor(bufferId, start + joinAt);
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

// `?` has no counterpart in the editor's search, which only runs forward, so
// it prompts for the text itself and searches like `#` does: `n` then goes on
// backward and `N` forward.
let backwardSearchCount = 1;

function vi_search_backward() : void {
  backwardSearchCount = consumeCount();
  editor.startPrompt("?", "vi-search-backward");
}
registerHandler("vi_search_backward", vi_search_backward);

async function runBackwardSearch(input: string): Promise<void> {
  // An empty `?` repeats the last search backward, as in Vim.
  const text = input.length > 0 ? input : memory.lastWordSearch?.text;
  if (text === undefined || text.length === 0) {
    return;
  }
  const wholeWord = input.length > 0 ? false : (memory.lastWordSearch?.wholeWord ?? false);
  memory.lastWordSearch = { text, direction: "backward", wholeWord };
  await executeStoredWordSearch(text, "backward", backwardSearchCount, wholeWord);
}

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
  editor.executeAction("recenter");
}
registerHandler("vi_center_cursor", vi_center_cursor);

async function vi_save_quit(): Promise<void> {
  const result = await executeViCommand("x");
  if (result.error) {
    editor.setStatus(`E: ${result.error}`);
  }
}
registerHandler("vi_save_quit", vi_save_quit);

async function vi_quit_force(): Promise<void> {
  const result = await executeViCommand("q!");
  if (result.error) {
    editor.setStatus(`E: ${result.error}`);
  }
}
registerHandler("vi_quit_force", vi_quit_force);

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
    block: null,
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
//
// The rectangle is the editor's own block selection (the one
// `block_select_*` draws), so copy, delete and typing act on a column.
// Its anchor is fixed where the block selection starts, and the host's
// rectangle is half-open on the moving side, so the plugin keeps both
// corners itself (`state.visual.block`, inclusive like Vim's) and redraws
// the host's rectangle from them after every motion — the only way a head
// moving left of the anchor still keeps the anchor's column selected.
async function vi_visual_block() : Promise<void> {
  beginVisualSelection();
  switchMode("visual-block");
  const visual = state.visual;
  const line = editor.getPrimaryCursor()?.line ?? null;
  if (visual === null || line === null) {
    return;
  }
  const lineStart = await editor.getLineStartPosition(line);
  if (lineStart === null) {
    return;
  }
  const col = visual.anchor - lineStart;
  visual.block = { anchorLine: line, anchorCol: col, headLine: line, headCol: col };
  await drawVisualBlock();
}
registerHandler("vi_visual_block", vi_visual_block);

// Redraw the host's block selection from `state.visual.block`.
async function drawVisualBlock(): Promise<void> {
  const block = state.visual?.block ?? null;
  if (block === null) {
    return;
  }
  const bufferId = editor.getActiveBufferId();
  const anchorStart = await editor.getLineStartPosition(block.anchorLine);
  if (anchorStart === null) {
    return;
  }
  // Inclusive corners to the host's half-open span: the edge column on the
  // far side of the head is one past it.
  const rightward = block.headCol >= block.anchorCol;
  const anchorCol = rightward ? block.anchorCol : block.anchorCol + 1;
  const headCol = rightward ? block.headCol + 1 : block.headCol;
  const lines = block.headLine - block.anchorLine;
  const cols = headCol - anchorCol;
  // Any non-block action ends the previous rectangle.
  editor.executeAction("move_left");
  editor.setBufferCursor(bufferId, anchorStart + anchorCol);
  const steps: ActionSpec[] = [
    { action: cols > 0 ? "block_select_right" : "block_select_left", count: Math.abs(cols) },
  ];
  if (lines !== 0) {
    steps.push({ action: lines > 0 ? "block_select_down" : "block_select_up", count: Math.abs(lines) });
  }
  editor.executeActions(steps);
}

// Line length in bytes, without its terminator; null past the buffer.
async function lineLength(line: number): Promise<number | null> {
  const start = await editor.getLineStartPosition(line);
  const end = await editor.getLineEndPosition(line);
  if (start === null || end === null) {
    return null;
  }
  return end - start;
}

// Move the block's head and redraw it. `to` gets the head and returns its
// new place; columns are clamped to the head line's last character.
async function moveBlockHead(
  to: (head: { line: number; col: number }) => Promise<{ line: number; col: number }>,
): Promise<void> {
  const block = state.visual?.block ?? null;
  if (block === null) {
    return;
  }
  const next = await to({ line: block.headLine, col: block.headCol });
  const length = await lineLength(next.line);
  if (length === null) {
    return;
  }
  block.headLine = next.line;
  block.headCol = Math.max(0, Math.min(next.col, Math.max(0, length - 1)));
  await drawVisualBlock();
}

async function vi_vblock_left() : Promise<void> {
  const count = consumeCount();
  await moveBlockHead(async (h) => ({ line: h.line, col: h.col - count }));
}
registerHandler("vi_vblock_left", vi_vblock_left);

async function vi_vblock_down() : Promise<void> {
  const count = consumeCount();
  await moveBlockHead(async (h) => {
    let line = h.line;
    for (let i = 0; i < count && (await lineLength(line + 1)) !== null; i++) {
      line++;
    }
    return { line, col: h.col };
  });
}
registerHandler("vi_vblock_down", vi_vblock_down);

async function vi_vblock_up() : Promise<void> {
  const count = consumeCount();
  await moveBlockHead(async (h) => ({ line: Math.max(0, h.line - count), col: h.col }));
}
registerHandler("vi_vblock_up", vi_vblock_up);

async function vi_vblock_right() : Promise<void> {
  const count = consumeCount();
  await moveBlockHead(async (h) => ({ line: h.line, col: h.col + count }));
}
registerHandler("vi_vblock_right", vi_vblock_right);

async function vi_vblock_line_start() : Promise<void> {
  consumeCount();
  await moveBlockHead(async (h) => ({ line: h.line, col: 0 }));
}
registerHandler("vi_vblock_line_start", vi_vblock_line_start);

async function vi_vblock_first_non_blank() : Promise<void> {
  consumeCount();
  await moveBlockHead(async (h) => {
    const bufferId = editor.getActiveBufferId();
    const start = await editor.getLineStartPosition(h.line);
    if (start === null) {
      return h;
    }
    return { line: h.line, col: (await firstNonBlankOffset(bufferId, start)) - start };
  });
}
registerHandler("vi_vblock_first_non_blank", vi_vblock_first_non_blank);

async function vi_vblock_line_end() : Promise<void> {
  consumeCount();
  await moveBlockHead(async (h) => ({ line: h.line, col: Number.MAX_SAFE_INTEGER }));
}
registerHandler("vi_vblock_line_end", vi_vblock_line_end);

// The rectangle's top-left character, where the caret lands after an
// operator, as in Vim.
async function blockTopLeft(): Promise<number | null> {
  const block = state.visual?.block ?? null;
  if (block === null) {
    return null;
  }
  const start = await editor.getLineStartPosition(Math.min(block.anchorLine, block.headLine));
  return start === null ? null : start + Math.min(block.anchorCol, block.headCol);
}

// Visual block delete - delete the selected column. `copy` takes the block's
// text; deleting through the editing path splits the rectangle into one
// selection per line, so each line loses only its part.
async function vi_vblock_delete() : Promise<void> {
  const topLeft = await blockTopLeft();
  editor.executeAction("copy");
  editor.executeAction("delete_forward");
  editor.executeAction("remove_secondary_cursors");
  memory.register = "charwise";
  switchMode("normal");
  if (topLeft !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), topLeft);
  }
}
registerHandler("vi_vblock_delete", vi_vblock_delete);

// Visual block change - delete the column and type into every line of it:
// the per-line cursors the delete leaves are what makes the insert land on
// each line, as Vim's block `c` does.
function vi_vblock_change() : void {
  editor.executeAction("copy");
  editor.executeAction("delete_forward");
  memory.register = "charwise";
  // Leave the visual mode by hand: `switchMode`'s exit from one nudges
  // every cursor left and right to drop the selection, which the delete has
  // already done, and the nudge would wrap a cursor at column 0 onto the
  // line above.
  state.mode = "normal";
  switchMode("insert");
}
registerHandler("vi_vblock_change", vi_vblock_change);

// Visual block yank
async function vi_vblock_yank() : Promise<void> {
  const topLeft = await blockTopLeft();
  editor.executeAction("copy");
  memory.register = "charwise";
  switchMode("normal");
  if (topLeft !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), topLeft);
  }
}
registerHandler("vi_vblock_yank", vi_vblock_yank);

// Exit visual block mode, leaving the caret on the head corner.
async function vi_vblock_escape() : Promise<void> {
  const block = state.visual?.block ?? null;
  switchMode("normal");
  if (block === null) {
    return;
  }
  const start = await editor.getLineStartPosition(block.headLine);
  if (start !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), start + block.headCol);
  }
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
  await applyBlockIndent(">");
}
registerHandler("vi_vblock_indent", vi_vblock_indent);

async function vi_vblock_dedent() : Promise<void> {
  await applyBlockIndent("<");
}
registerHandler("vi_vblock_dedent", vi_vblock_dedent);

async function applyBlockIndent(operator: string): Promise<void> {
  const block = state.visual?.block ?? null;
  if (block === null) {
    await applyVisualIndent(operator);
    return;
  }
  const firstLine = Math.min(block.anchorLine, block.headLine);
  const lineCount = Math.abs(block.headLine - block.anchorLine) + 1;
  const firstLineStart = await editor.getLineStartPosition(firstLine);
  // Drop the rectangle before the lines move under it.
  editor.executeAction("move_left");
  if (firstLineStart === null) {
    switchMode("normal");
    return;
  }
  await applyIndentToLineRange(operator, firstLineStart, lineCount);
}

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
  const column = from - await findLineStartAtPosition(bufferId, from);
  return offsetAtColumn(bufferId, Math.max(0, Math.min(cursorLine + delta, lastLine)), column);
}

// The offset `column` bytes into `line`, or the line's last character
// boundary before that — a multi-byte line cannot be split, and a short line
// takes the caret to its end.
async function offsetAtColumn(bufferId: number, line: number, column: number): Promise<number | null> {
  const lineStart = await editor.getLineStartPosition(line);
  const lineEnd = await editor.getLineEndPosition(line);
  if (lineStart === null) {
    return null;
  }
  const limit = lineEnd === null ? lineStart : Math.max(lineStart, lineEnd);
  const lineText = (await safeGetBufferText(bufferId, lineStart, limit)) ?? "";
  const index = byteOffsetToStringIndex(lineText, Math.min(column, limit - lineStart));
  return lineStart + stringIndexToByteOffset(lineText, index);
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

// The text object each key names, read after `i`/`a` by both the operator
// path and visual mode.
const TEXT_OBJECT_KEYS: Array<{ key: string; object: string }> = [
  { key: "w", object: "word" },
  { key: "W", object: "WORD" },
  { key: '"', object: '"' },
  { key: "'", object: "'" },
  { key: "`", object: "`" },
  { key: "(", object: "(" },
  { key: ")", object: "(" },
  { key: "b", object: "(" },
  { key: "{", object: "{" },
  { key: "}", object: "{" },
  { key: "B", object: "{" },
  { key: "[", object: "[" },
  { key: "]", object: "[" },
  { key: "<", object: "<" },
  { key: ">", object: "<" },
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
// becomes that object. The key is read with `getNextKey`, as after an
// operator (`enterOperatorTextObject`).
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

  // An empty object (`vi(` on `()`) leaves the selection as it is, as Vim's
  // does; an operator over one still acts (`ci(` types between the brackets).
  const range = await computeTextObjectRange(objectType, modifier === "inner");
  if (range !== null && range.end > range.start) {
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

// Exit visual mode without doing anything. The caret stays on the head
// character: the host's caret sits just past it (the selection's exclusive
// end), and dropping the selection would leave it there, one column right of
// where `v` started (#3797).
function vi_vis_escape() : void {
  const head = state.mode === "visual" ? visualHead() : null;
  switchMode("normal");
  if (head !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), head);
  }
}
registerHandler("vi_vis_escape", vi_vis_escape);

// ============================================================================
// Text Objects (iw, aw, i", a", etc.)
// ============================================================================

// `i`/`a` after an operator: the next key names the text object. It is read
// with `getNextKey`, as `f` reads its target, so a key that names no object
// (`dit`, `dq`) cancels the operator, as in Vim, instead of leaving it
// waiting for a key that would eventually match. `text-object` mode is set
// across the wait for the status bar and so the staged operator survives it.
async function enterOperatorTextObject(modifier: "inner" | "around"): Promise<void> {
  if (state.pending === null) {
    switchMode("normal");
    return;
  }
  state.pending.textObject = modifier;
  switchMode("text-object");

  editor.beginKeyCapture();
  const generation = modalGeneration;
  let ev;
  try {
    ev = await editor.getNextKey();
  } finally {
    editor.endKeyCapture();
  }
  if (generation !== modalGeneration) return;

  const objectType = ev.key.length === 1 ? textObjectForKey(ev.key) : undefined;
  if (objectType === undefined) {
    switchMode("normal");
    return;
  }
  await applyTextObject(objectType, state.pending?.operator ?? null, modifier);
}

async function vi_text_object_inner() : Promise<void> {
  await enterOperatorTextObject("inner");
}
registerHandler("vi_text_object_inner", vi_text_object_inner);

async function vi_text_object_around() : Promise<void> {
  await enterOperatorTextObject("around");
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
): Promise<void> {
  if (!operator) {
    switchMode("normal");
    return;
  }

  const range = await textObjectOperatorRange(objectType, modifier);
  if (range === null) {
    // No such object at the caret: the operator fails, and `.` keeps what it
    // was holding.
    switchMode("normal");
    return;
  }
  // Record last change for '.' repeat (only for delete and change, not yank)
  if (isRepeatableOperator(operator) && modifier) {
    recordChange({ type: "operator-textobj", operator, textObject: { modifier, object: objectType } });
  }
  await applyOperator(operator, range);
}

async function textObjectOperatorRange(objectType: string, modifier: TextObjectType): Promise<OperatorRange | null> {
  const range = await computeTextObjectRange(objectType, modifier === "inner");
  if (range === null) {
    return null;
  }
  return range.exclusive
    ? exclusiveOperatorRange(editor.getActiveBufferId(), range.start, range.end)
    : charwiseRange(range.start, range.end);
}

// `i"`/`a"` (and `'`, `` ` ``) at index `col` of `line`, as Vim's
// `current_quote` finds them. On a quote, whether it opens or closes is
// decided by pairing the line's quotes from its start. Off a quote, the pair
// is the nearest quote before the caret and the next one after it — so
// between two strings it is the text between them — or, with no quote
// before, the first pair after the caret. A quote after a backslash does not
// close a string. `a"` adds the blanks after the closing quote, or the blanks
// before the opening one when there are none after. Null when there is no
// such pair on the line.
function quoteObject(line: string, col: number, quote: string, inner: boolean): { start: number; end: number } | null {
  // The next quote at or after `from`; with `escaped`, a backslash hides the
  // character after it.
  const nextQuote = (from: number, escaped: boolean): number => {
    for (let i = from; i < line.length; i++) {
      if (escaped && line[i] === "\\") {
        i++;
      } else if (line[i] === quote) {
        return i;
      }
    }
    return -1;
  };
  // The nearest unescaped quote before `from`.
  const previousQuote = (from: number): number => {
    for (let i = from - 1; i >= 0; i--) {
      let backslashes = 0;
      while (i - backslashes > 0 && line[i - backslashes - 1] === "\\") {
        backslashes++;
      }
      if (backslashes % 2 === 1) {
        i -= backslashes;
      } else if (line[i] === quote) {
        return i;
      }
    }
    return -1;
  };

  let open = -1;
  let close = -1;
  if (line[col] === quote) {
    for (let from = 0; ; from = close + 1) {
      open = nextQuote(from, false);
      if (open < 0 || open > col) {
        return null;
      }
      close = nextQuote(open + 1, true);
      if (close < 0) {
        return null;
      }
      if (col <= close) {
        break;
      }
    }
  } else {
    open = previousQuote(col);
    if (open < 0) {
      open = nextQuote(col, false);
      if (open < 0) {
        return null;
      }
    }
    close = nextQuote(open + 1, true);
    if (close < 0) {
      return null;
    }
  }

  if (inner) {
    return { start: open + 1, end: close };
  }
  const isBlank = (c: string | undefined) => c === " " || c === "\t";
  let start = open;
  let end = close + 1;
  if (isBlank(line[end])) {
    while (isBlank(line[end])) end++;
  } else {
    while (start > 0 && isBlank(line[start - 1])) start--;
  }
  return { start, end };
}

// `iw`/`aw` and `iW`/`aW` at string index `pos` of `text`, within the
// caret's line (`:help iw`). A word is a run of one class: keyword
// characters, other non-blank characters, or blanks (for `W`, every non-blank
// is one class). `iw` is the run under the caret. `aw` adds the blanks after
// the word, or the blanks before it when there are none after; on blanks,
// `aw` is the blanks and the word that follows them.
//
// `textReachesBufferEnd` says `text` runs to the end of the buffer, so that
// its last line is the buffer's.
function wordObject(
  text: string,
  pos: number,
  bigWord: boolean,
  inner: boolean,
  textReachesBufferEnd: boolean,
): { start: number; end: number } | null {
  const atLineEnd = (i: number) => i >= text.length || isLineBreakAt(text, i);
  if (atLineEnd(pos)) {
    return null;
  }
  const BLANK = 0;
  const classOf = (ch: string): number =>
    ch === " " || ch === "\t" ? BLANK : bigWord || !isWordChar(ch) ? 1 : 2;
  const runEnd = (i: number): number => {
    const cls = classOf(text[i]);
    while (!atLineEnd(i) && classOf(text[i]) === cls) i++;
    return i;
  };
  const runStart = (i: number): number => {
    const cls = classOf(text[i]);
    while (i > 0 && !isLineBreakAt(text, i - 1) && classOf(text[i - 1]) === cls) i--;
    return i;
  };

  let start = runStart(pos);
  let end = runEnd(pos);
  if (inner) {
    return { start, end };
  }
  if (classOf(text[pos]) === BLANK) {
    if (!atLineEnd(end)) {
      return { start, end: runEnd(end) };
    }
    // Blanks that run to the end of the line have no word after them: Vim
    // takes just the blanks, except on the last line, where it fails.
    const breakEnd = end + (text.startsWith("\r\n", end) ? 2 : 1);
    return textReachesBufferEnd && breakEnd >= text.length ? null : { start, end };
  }
  if (!atLineEnd(end) && classOf(text[end]) === BLANK) {
    return { start, end: runEnd(end) };
  }
  // No blanks after the word: take the ones before it, but never a line's
  // indent.
  if (start > 0 && !isLineBreakAt(text, start - 1) && classOf(text[start - 1]) === BLANK) {
    const blanksStart = runStart(start - 1);
    if (blanksStart > 0 && !isLineBreakAt(text, blanksStart - 1)) {
      start = blanksStart;
    }
  }
  return { start, end };
}

// A text object's byte range. `exclusive` marks an inner block that ends at
// the start of its closing bracket's line (see `bracketObject`); an operator
// over it follows Vim's rule for exclusive ranges (`exclusiveOperatorRange`),
// visual mode selects it as it is.
interface TextObjectRange {
  start: number;
  end: number;
  exclusive: boolean;
}

const BRACKET_OBJECT_PAIRS: Record<string, [string, string]> = {
  "(": ["(", ")"], ")": ["(", ")"], b: ["(", ")"],
  "{": ["{", "}"], "}": ["{", "}"], B: ["{", "}"],
  "[": ["[", "]"], "]": ["[", "]"],
  "<": ["<", ">"], ">": ["<", ">"],
};

// `a(` / `i(` and the other blocks around string index `pos` (`:help a(`),
// as string indexes into `text`. `a(` is the brackets and all between them.
// `i(` is what is between them, with Vim's two line rules: a `(` that ends
// its line starts the block on the next line, and a `)` with only blanks
// before it on its line ends the block at that line's start, which makes the
// block exclusive of the line break before it. Either can leave it empty.
function bracketObject(
  text: string,
  pos: number,
  open: string,
  close: string,
  inner: boolean,
): { start: number; end: number; exclusive: boolean } | null {
  const pair = findMatchingPair(text, pos, open, close);
  if (pair === null) {
    return null;
  }
  if (!inner) {
    return { start: pair.start, end: pair.end + 1, exclusive: false };
  }
  let start = pair.start + 1;
  if (isLineBreakAt(text, start)) {
    start += text[start] === "\r" && text[start + 1] === "\n" ? 2 : 1;
  }
  let closeLineStart = pair.end;
  while (closeLineStart > 0 && text[closeLineStart - 1] !== "\n") {
    closeLineStart--;
  }
  if (closeLineStart > pair.start && /^[ \t]*$/.test(text.slice(closeLineStart, pair.end))) {
    return { start: Math.min(start, closeLineStart), end: closeLineStart, exclusive: true };
  }
  return { start, end: pair.end, exclusive: false };
}

// Vim's rule for an operator over an exclusive range that ends at the start
// of a later line (`:help exclusive-linewise`): the end moves back to the end
// of the line before, and when the range starts at or before its line's first
// non-blank the operator takes whole lines — `di{` on a block of its own
// lines deletes those lines, `ci{` leaves one empty line to type into.
async function exclusiveOperatorRange(bufferId: number, start: number, end: number): Promise<OperatorRange | null> {
  if (end <= start || (await findLineStartAtPosition(bufferId, end)) !== end) {
    return charwiseRange(start, end);
  }
  const startLineStart = await findLineStartAtPosition(bufferId, start);
  if (start <= await pastLeadingBlanks(bufferId, startLineStart)) {
    const span = await lineSpanOfRange(bufferId, startLineStart, end);
    return linewiseRangeAt(bufferId, startLineStart, span.lineCount);
  }
  const breakText = await editor.getBufferText(bufferId, Math.max(start, end - 2), end);
  return charwiseRange(start, end - (breakText.endsWith("\r\n") ? 2 : 1));
}

// The byte range a text object covers at the caret, or null when there is no
// such object there. Split out of `applyTextObject` so visual mode can set the
// selection to an object (`viw`, `va"`) without an operator to apply.
async function computeTextObjectRange(
  objectType: string,
  isInner: boolean,
): Promise<TextObjectRange | null> {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  if (cursorPos === null) {
    return null;
  }

  // Get text around cursor to find the text object boundaries. The window
  // starts on a line start, a character boundary, and is read in two pieces
  // split at the caret, so the caret's string index is the length of the
  // first; everything below works in string indexes, converted to byte
  // offsets once at the end.
  const windowSize = 1000;
  const windowStart = await findLineStartAtPosition(bufferId, Math.max(0, cursorPos - windowSize));
  const bufLen = editor.getBufferLength(bufferId);
  const before = await editor.getBufferText(bufferId, windowStart, cursorPos);
  const windowEnd = Math.min(bufLen, cursorPos + windowSize);
  const after = await editor.getBufferText(bufferId, cursorPos, windowEnd);
  const text = before + after;
  if (!text) {
    return null;
  }

  const posInChunk = before.length;
  let selectStart = -1;
  let selectEnd = -1;
  // Brackets and quotes can be empty (`ci(` on `()` types between them); a
  // word cannot.
  let allowEmpty = false;
  let exclusive = false;

  switch (objectType) {
    case "word":
    case "WORD": {
      const word = wordObject(text, posInChunk, objectType === "WORD", isInner, windowEnd >= bufLen);
      if (word !== null) {
        selectStart = word.start;
        selectEnd = word.end;
      }
      break;
    }

    case "\"":
    case "'":
    case "`": {
      let lineStart = posInChunk;
      let lineEnd = posInChunk;
      while (lineStart > 0 && text[lineStart - 1] !== '\n') lineStart--;
      while (lineEnd < text.length && text[lineEnd] !== '\n') lineEnd++;

      const quote = quoteObject(text.substring(lineStart, lineEnd), posInChunk - lineStart, objectType, isInner);
      if (quote !== null) {
        allowEmpty = true;
        selectStart = lineStart + quote.start;
        selectEnd = lineStart + quote.end;
      }
      break;
    }

    case "(":
    case ")":
    case "b":
    case "{":
    case "}":
    case "B":
    case "[":
    case "]":
    case "<":
    case ">": {
      const pair = BRACKET_OBJECT_PAIRS[objectType];
      const block = bracketObject(text, posInChunk, pair[0], pair[1], isInner);
      if (block !== null) {
        selectStart = block.start;
        selectEnd = block.end;
        exclusive = block.exclusive;
        allowEmpty = true;
      }
      break;
    }
  }

  if (selectStart === -1 || selectEnd === -1 || selectEnd < selectStart || (selectEnd === selectStart && !allowEmpty)) {
    return null;
  }
  return {
    start: windowStart + stringIndexToByteOffset(text, selectStart),
    end: windowStart + stringIndexToByteOffset(text, selectEnd),
    exclusive,
  };
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

  // Search backward for opening bracket. A closing bracket under the caret is
  // the end of the block the caret is on, not a nested one to skip.
  for (let i = text[pos] === closeChar ? pos - 1 : pos; i >= 0; i--) {
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


// ============================================================================
// Find Character Motions (f/t/F/T)
// ============================================================================

// Enter find-char mode, await one keypress, then dispatch.
//
// Implemented via `editor.getNextKey()` (plugin API #1) — the editor
// hands the next keypress to this awaiting handler before any other
// dispatch, which means the mode itself does not need any per-key
// bindings.  Keeps `setInputMode("vi-find-char")` set across the
// await purely for the status-bar indicator.
async function enterFindCharMode(findType: FindKind): Promise<void> {
  state.pendingFindChar = findType;
  // Set directly rather than through `switchMode`, which would clear the
  // count that `3fx` must still be holding when the target key arrives.
  state.mode = "find-char";
  editor.setInputMode("vi-find-char");
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
      await executeFindChar({ type: findType, char: ev.key }, consumeCount());
    }
  } finally {
    editor.endKeyCapture();
  }
  if (generation !== modalGeneration) return;
  switchMode("normal");
}

// The caret's line as an array of characters (code points), and the caret's
// index in it. The read starts on the line start, a character boundary, so
// the caret's byte offset converts to a character index exactly; the find
// works in characters and converts back with `utf8ByteLength`, so non-ASCII
// text anywhere before the caret cannot shift the target into the middle of a
// character. Returns null if the buffer text can't be read.
async function getFindCharLineContext(
  bufferId: number,
  cursorPos: number,
): Promise<{ chars: string[]; col: number } | null> {
  const lineStart = await findLineStartAtPosition(bufferId, cursorPos);
  const bufLen = editor.getBufferLength(bufferId);
  const before = await editor.getBufferText(bufferId, lineStart, cursorPos);
  const after = await editor.getBufferText(bufferId, cursorPos, Math.min(bufLen, cursorPos + 10000));
  if (before === null || after === null) {
    return null;
  }
  const lineBreak = after.search(/[\r\n]/);
  const head = Array.from(before);
  const tail = Array.from(lineBreak === -1 ? after : after.slice(0, lineBreak));
  return { chars: head.concat(tail), col: head.length };
}

// The character index a find lands on, searching `chars` from `col`. `count`
// selects the Nth match (1-based). Returns -1 when the line has no such match.
function computeFindCharTargetCol(
  findType: FindCharType,
  char: string,
  chars: string[],
  col: number,
  count: number,
): number {
  let remaining = Math.max(1, count);

  if (findType === "f" || findType === "t") {
    for (let i = col + 1; i < chars.length; i++) {
      if (chars[i] === char && --remaining === 0) {
        return findType === "f" ? i : i - 1;
      }
    }
  } else {
    for (let i = col - 1; i >= 0; i--) {
      if (chars[i] === char && --remaining === 0) {
        return findType === "F" ? i : i + 1;
      }
    }
  }
  return -1;
}

// One find on the caret's line: `f`/`t`/`F`/`T` and its target character.
interface CharFind {
  type: FindKind;
  char: string;
}

// `f` and `t` are inclusive motions, `F` and `T` exclusive — the same
// distinction `rangeFromMotion` applies to every other motion.
function findMotionKind(type: FindKind): MotionKind {
  return type === "f" || type === "t" ? "inclusive" : "exclusive";
}

// The same find in the opposite direction, which is what `,` repeats.
function reverseFind(find: CharFind): CharFind {
  const type: FindKind =
    find.type === "f" ? "F" :
    find.type === "F" ? "f" :
    find.type === "t" ? "T" : "t";
  return { type, char: find.char };
}

// Where `find` lands from `origin` as a motion, or null when the line has no
// `count`-th match (the motion fails).
//
// A `t`/`T` whose target is adjacent is *found*, landing on the caret itself:
// `t.` then does not move, `dt.` takes the character under the caret and
// `cT.` changes nothing but still opens an insert. `asRepeat` is the `;`/`,`
// path, where such a `t` would never move again, so a bare `;` skips the
// adjacent match and goes to the next one (`:help cpo-;`).
async function resolveFind(
  origin: number,
  find: CharFind,
  count: number,
  asRepeat: boolean,
): Promise<ResolvedMotion | null> {
  const ctx = await getFindCharLineContext(editor.getActiveBufferId(), origin);
  if (!ctx) {
    return null;
  }
  const { chars, col } = ctx;
  // Only a bare `;`/`,` skips: with a count, Vim searches for that many
  // matches from the caret as usual (`2;` after an adjacent `t.` lands before
  // the second `.`, not the third).
  const skipsAdjacent = asRepeat
    && count === 1
    && (find.type === "t" || find.type === "T")
    && computeFindCharTargetCol(find.type, find.char, chars, col, 1) === col;
  const targetCol = computeFindCharTargetCol(find.type, find.char, chars, col, skipsAdjacent ? 2 : count);
  if (targetCol < 0) {
    return null;
  }
  // The byte offset is the caret's plus or minus the UTF-8 length of the
  // characters in between.
  const target = targetCol >= col
    ? origin + editor.utf8ByteLength(chars.slice(col, targetCol).join(""))
    : origin - editor.utf8ByteLength(chars.slice(targetCol, col).join(""));
  return { target, kind: findMotionKind(find.type) };
}

// `;` and `,` as a motion: the current find, in its own or the opposite
// direction. Resolved when it runs, so `.` after `d;` repeats whatever find is
// current then, as Vim's does, and a repeat never replaces the stored find.
async function findRepeatMotion(origin: number, count: number, repeat: FindRepeat): Promise<ResolvedMotion | null> {
  const last = memory.lastFindChar;
  if (last === null) {
    return null;
  }
  return resolveFind(origin, repeat === ";" ? last : reverseFind(last), count, true);
}

// `fx`, `3tx`, …: move to the find. The find becomes the one `;` and `,`
// repeat even when the line has no match, as Vim stores it before searching.
async function executeFindChar(find: CharFind, count: number): Promise<void> {
  memory.lastFindChar = find;
  const origin = editor.getCursorPosition();
  const resolved = origin === null ? null : await resolveFind(origin, find, count, false);
  if (resolved !== null) {
    editor.setBufferCursor(editor.getActiveBufferId(), resolved.target);
  }
}

// The range an operator takes over a find (`dfx`, `c2tx`), which also becomes
// the find `;` and `,` repeat, matched or not.
async function findOperatorRange(find: CharFind, count: number): Promise<OperatorRange | null> {
  memory.lastFindChar = find;
  const origin = editor.getCursorPosition();
  const resolved = origin === null ? null : await resolveFind(origin, find, count, false);
  return origin === null || resolved === null ? null : rangeFromMotion(origin, resolved.target, resolved.kind);
}

// `dfx`, `c2tx`, …: operator + motion, except that the target character is
// part of the command, so `.` records it as its own kind of change rather than
// as a named motion.
async function executeFindCharOperator(operator: string, find: CharFind, count: number): Promise<void> {
  const range = await findOperatorRange(find, count);
  if (range === null) {
    // Target not found on the line: vim leaves the buffer unchanged and cancels
    // the operator. Do NOT consume the operator as a different motion.
    switchMode("normal");
    return;
  }
  if (isRepeatableOperator(operator)) {
    recordChange({ type: "operator-find-char", operator, find, count });
  }
  await applyOperator(operator, range);
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

// `;` and `,` move exactly as they do for an operator (`d;`, `d,`).
async function vi_find_char_repeat(): Promise<void> {
  await moveByMotion("vi_find_repeat", consumeCount());
}
registerHandler("vi_find_char_repeat", vi_find_char_repeat);

async function vi_find_char_repeat_reverse(): Promise<void> {
  await moveByMotion("vi_find_repeat_reverse", consumeCount());
}
registerHandler("vi_find_char_repeat_reverse", vi_find_char_repeat_reverse);

// Enter find-char mode with a pending operator (df/dt/cf/ct/dF/dT/…). Awaits the
// target keypress, then applies the operator over the find-char range. Mirrors
// enterFindCharMode but preserves the pending operator across the await and
// dispatches to the operator-aware executor.
async function enterFindCharOperatorMode(findType: FindKind): Promise<void> {
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
  editor.setInputMode("vi-find-char");
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
      await executeFindCharOperator(operator, { type: findType, char: ev.key }, count);
    } else {
      // Escape (or any non-character key) cancels the operator.
      switchMode("normal");
    }
  } finally {
    editor.endKeyCapture();
  }
}

async function vi_op_find_char_repeat(): Promise<void> {
  await handleMotionWithOperator("vi_find_repeat");
}
registerHandler("vi_op_find_char_repeat", vi_op_find_char_repeat);

async function vi_op_find_char_repeat_reverse(): Promise<void> {
  await handleMotionWithOperator("vi_find_repeat_reverse");
}
registerHandler("vi_op_find_char_repeat_reverse", vi_op_find_char_repeat_reverse);

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

async function vi_op_first_non_blank(): Promise<void> {
  await handleMotionWithOperator("vi_first_non_blank");
}
registerHandler("vi_op_first_non_blank", vi_op_first_non_blank);

async function vi_op_first_non_blank_line(): Promise<void> {
  await handleMotionWithOperator("vi_first_non_blank_line");
}
registerHandler("vi_op_first_non_blank_line", vi_op_first_non_blank_line);

async function vi_op_first_non_blank_next_line(): Promise<void> {
  await handleMotionWithOperator("vi_first_non_blank_next_line");
}
registerHandler("vi_op_first_non_blank_next_line", vi_op_first_non_blank_next_line);

async function vi_op_first_non_blank_prev_line(): Promise<void> {
  await handleMotionWithOperator("vi_first_non_blank_prev_line");
}
registerHandler("vi_op_first_non_blank_prev_line", vi_op_first_non_blank_prev_line);

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
    ["_", "vi_first_non_blank_line"],
    ["+", "vi_first_non_blank_next_line"],
    ["-", "vi_first_non_blank_prev_line"],
    ["g g", "vi_doc_start"],
    ["G", "vi_doc_end"],
    ["C-f", "vi_page_down"],
    ["C-b", "vi_page_up"],
    ["C-d", "vi_half_page_down"],
    ["C-u", "vi_half_page_up"],
    ["%", "vi_matching_bracket"],
    ["z z", "vi_center_cursor"],
    ["Z Z", "vi_save_quit"],
    ["Z Q", "vi_quit_force"],
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
    ["C-c", "vi_copy"],
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
  // editor.getNextKey() to read the next character.  setInputMode(...) is
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
    ["^", "vi_op_first_non_blank"],
    ["_", "vi_op_first_non_blank_line"],
    ["+", "vi_op_first_non_blank_next_line"],
    ["-", "vi_op_first_non_blank_prev_line"],
    [";", "vi_op_find_char_repeat"],
    [",", "vi_op_find_char_repeat_reverse"],
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
    ["d", "vi_op_doubled_d"],
    ["c", "vi_op_doubled_c"],
    ["y", "vi_op_doubled_y"],
    [">", "vi_op_doubled_indent"],
    ["<", "vi_op_doubled_dedent"],

    // Cancel
    ["Escape", "vi_cancel"],
  ], true);

  // vi-text-object mode needs no bindings either: `enterOperatorTextObject`
  // reads the object key with getNextKey. The mode is defined so the buffer
  // is read-only while it waits.
  editor.defineMode("vi-text-object", [], true);

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
    ["^", "vi_vblock_first_non_blank"],

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
      //
      // `force_quit` throws unsaved changes away (hot exit does not bring
      // them back), so the plain form only reaches it with nothing unsaved.
      if (force) {
        editor.executeAction("force_quit");
        return {};
      }
      if (anyBufferModified()) {
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
      // :x / ZZ write only when the buffer has changes (`:help :x`), so an
      // unchanged file is not rewritten and an unchanged unnamed buffer quits.
      const wqBufferId = editor.getActiveBufferId();
      const writeOnlyIfModified = command !== "wq";
      const currentModified = editor.getBufferInfo(wqBufferId)?.modified ?? true;

      if (writeOnlyIfModified && !currentModified) {
        // Nothing to write, to the buffer's own file or a named one.
      } else if (args) {
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

      // Quit only once the write has landed: `force_quit` discards whatever
      // is still unsaved, so a write that failed (or asked something) must
      // keep the editor open. Its own status says why.
      await editor.flush();
      if (editor.getBufferInfo(wqBufferId)?.modified) {
        return {};
      }
      if (anyBufferModified()) {
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
        if (anyBufferModified()) {
          return { error: editor.t("error.no_write_since_change", { cmd: ":qa!" }) };
        }
        editor.executeAction("force_quit");
      }
      return {};
    }

    case "wqall": {
      // :wqa or :xa - save all and quit, unless something could not be
      // written (`save_all`'s status says what).
      editor.executeAction("save_all");
      await editor.flush();
      if (anyBufferModified()) {
        return {};
      }
      editor.executeAction("force_quit");
      return {};
    }

    case "edit": {
      // :e - reload current file
      // :e filename - open file
      // :e! - force reload (discard changes)
      if (!args) {
        if (!force && editor.isBufferModified(editor.getActiveBufferId())) {
          return { error: editor.t("error.no_write_since_change", { cmd: ":e!" }) };
        }
        return revertActiveBuffer();
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
      editor.executeAction("new");
      return {};
    }

    case "revert": {
      // :revert - Fresh-specific command to reload file
      return revertActiveBuffer();
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
      editor.executeAction("new");
      if (args) {
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "vnew": {
      // :vnew - create new buffer in vertical split
      editor.executeAction("split_vertical");
      editor.executeAction("new");
      if (args) {
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "only": {
      // :only - close all other splits
      const activeSplit = editor.getActiveSplitId();
      for (const split of editor.listSplits()) {
        if (split.splitId !== activeSplit) {
          editor.closeSplit(split.splitId);
        }
      }
      return {};
    }

    case "close": {
      // :close - close the current split. Its buffers stay open (as tabs
      // of the remaining splits), so nothing unsaved is lost and there is
      // nothing to refuse; the last split is not closed.
      editor.executeAction("close_split");
      return {};
    }

    case "tabnew":
    case "tabedit": {
      // :tabnew - new tab (creates new buffer in Fresh)
      editor.executeAction("new");
      if (args) {
        const path = args.trim();
        editor.openFile(path, 0, 0);
      }
      return {};
    }

    case "tabclose": {
      // :tabclose - close current tab/buffer. A buffer another split still
      // shows loses nothing when this tab goes.
      const bufferId = editor.getActiveBufferId();
      if (!force && editor.isBufferModified(bufferId) && !shownInAnotherSplit(bufferId)) {
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
      editor.executeAction("show_diagnostics_panel");
      return {};
    }

    case "cclose": {
      // :cclose - close diagnostics panel
      return { message: editor.t("info.close_diagnostics") };
    }

    case "cnext": {
      // :cnext - next diagnostic
      editor.executeAction("jump_to_next_error");
      return {};
    }

    case "cprevious": {
      // :cprev - previous diagnostic
      editor.executeAction("jump_to_previous_error");
      return {};
    }

    case "cfirst": {
      // :cfirst - first diagnostic
      return gotoDiagnostic("first");
    }

    case "clast": {
      // :clast - last diagnostic
      return gotoDiagnostic("last");
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

// Whether any buffer of any window has unsaved changes. `force_quit`
// discards them all, so a quit that must not lose work checks every window,
// not just the active one.
function anyBufferModified(): boolean {
  return editor.listWindows().some((w) => editor.listBuffers(w.id).some((b) => b.modified));
}

function shownInAnotherSplit(bufferId: number): boolean {
  const activeSplit = editor.getActiveSplitId();
  return editor.listSplits().some((s) => s.splitId !== activeSplit && s.bufferId === bufferId);
}

// `revert` asks before throwing unsaved changes away; until it is answered
// nothing has been reverted, so only an unmodified buffer reports it.
function revertActiveBuffer(): CommandResult {
  const modified = editor.isBufferModified(editor.getActiveBufferId());
  editor.executeAction("revert");
  return modified ? {} : { message: editor.t("status.file_reverted") };
}

// :cfirst / :clast - the first or last diagnostic, by file, line and column.
function gotoDiagnostic(which: "first" | "last"): CommandResult {
  const toPath = (uri: string) => editor.fileUriToPath(uri) || uri;
  const diagnostics = editor.getAllDiagnostics().slice().sort((a, b) => {
    const pa = toPath(a.uri);
    const pb = toPath(b.uri);
    if (pa !== pb) return pa < pb ? -1 : 1;
    if (a.range.start.line !== b.range.start.line) return a.range.start.line - b.range.start.line;
    return a.range.start.character - b.range.start.character;
  });
  const target = which === "first" ? diagnostics[0] : diagnostics[diagnostics.length - 1];
  if (target === undefined) {
    return {};
  }
  editor.openFile(toPath(target.uri), target.range.start.line + 1, target.range.start.character + 1);
  return {};
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

    case "wrap":
    case "nowrap": {
      // :set wrap / :set nowrap - set (not toggle) wrapping in this window
      editor.setLineWrap(editor.getActiveBufferId(), editor.getActiveSplitId(), option === "wrap");
      return {};
    }

    default: {
      return { error: editor.t("error.unknown_option", { option }) };
    }
  }
}

// Register event handler for prompt confirmation
editor.on("prompt_confirmed", async (args) => {
  if (args.prompt_type === "vi-search-backward") {
    await runBackwardSearch(args.input);
    return true;
  }
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

// Vi is an editor-wide input mode (`editor.setInputMode`), not a window's
// editor mode. "Vi is on" is a preference about how the user types, so it
// holds in every window — one created after vi was enabled included — and no
// window-scoped code (a dialog, a window switch, another plugin's per-window
// mode) can clear it. The sub-mode (normal/insert/visual/…) is the input
// mode's value: one for the whole editor, applied wherever the keyboard is,
// the same way `state` below is one for the plugin.
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
  editor.setInputMode(null);
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

// Adopt Settings changes without an editor restart. `arrow_keys` and
// `search_word_under_cursor` are baked into the mode binding tables at
// `defineMode` time, so re-reading them isn't enough — the modes have to
// be re-emitted. Guarded on an actual change: `config_changed` fires for
// every config save, and re-registering the tables on an unrelated one
// would be pure churn.
//
// `auto_start` is deliberately not applied live. It means "enable vi when
// the editor starts"; flipping vi on mid-session because a startup
// preference changed is a different behaviour than the setting promises.
editor.on("config_changed", () => {
  const cfg = (editor.getPluginConfig() ?? {}) as {
    arrow_keys?: boolean;
    search_word_under_cursor?: boolean;
  };
  const nextArrowKeys = cfg.arrow_keys !== false;
  const nextSearchWord = cfg.search_word_under_cursor !== false;
  if (nextArrowKeys === arrowKeys && nextSearchWord === searchWordUnderCursor) return;
  arrowKeys = nextArrowKeys;
  searchWordUnderCursor = nextSearchWord;
  defineViModes();
});

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
  // place its id can be learned. `enableVi` cannot ask for it — `auto_start`
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
    editor.setInputMode("vi-normal");
  }
  // Unconditional: `resetModalState` also drops a half-typed count, which the
  // indicator is showing.
  editor.setStatus(getModeIndicator("normal"));
});
