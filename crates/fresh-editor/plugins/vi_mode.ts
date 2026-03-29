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
  insertStartPos: number | null; // Cursor position when entering insert mode
  visualBlockAnchor: { line: number; col: number } | null; // For visual block mode
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
  insertStartPos: null,
  visualBlockAnchor: null,
};

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

// Map motion actions to their selection equivalents
const motionToSelection: Record<string, string> = {
  move_left: "select_left",
  move_right: "select_right",
  move_up: "select_up",
  move_down: "select_down",
  move_word_left: "select_word_left",
  move_word_right: "select_word_right",
  vi_move_word_end: "vi_select_word_end",
  move_line_start: "select_line_start",
  move_line_end: "select_line_end",
  move_document_start: "select_document_start",
  move_document_end: "select_document_end",
};

// Map (operator, motion) pairs to atomic Rust actions
// These are single actions that combine the operator and motion atomically
// This avoids async issues with selection-based approach
type OperatorMotionMap = Record<string, Record<string, string>>;
const atomicOperatorActions: OperatorMotionMap = {
  d: {
    // Delete operators
    move_word_right: "delete_word_forward",
    move_word_left: "delete_word_backward",
    vi_move_word_end: "delete_vi_word_end",
    move_line_end: "delete_to_line_end",
    move_line_start: "delete_to_line_start",
  },
  y: {
    // Yank operators
    move_word_right: "yank_word_forward",
    move_word_left: "yank_word_backward",
    vi_move_word_end: "yank_vi_word_end",
    move_line_end: "yank_to_line_end",
    move_line_start: "yank_to_line_start",
  },
};

// Apply an operator using atomic actions if available, otherwise selection-based approach
// The count parameter specifies how many times to apply the motion (e.g., d3w = delete 3 words)
function applyOperatorWithMotion(operator: string, motionAction: string, count: number = 1): void {
  // Record last change for '.' repeat (only for delete and change, not yank)
  if (operator === "d" || operator === "c") {
    state.lastChange = { type: "operator-motion", operator, motion: motionAction, count };
  }

  // For "change" operator, use delete action and then enter insert mode
  const lookupOperator = operator === "c" ? "d" : operator;

  // Check if we have an atomic action for this operator+motion combination
  const operatorActions = atomicOperatorActions[lookupOperator];
  const atomicAction = operatorActions?.[motionAction];

  if (atomicAction) {
    // Use the atomic action - single command, no async issues
    // Apply count times for 3dw, etc.
    if (count === 1) {
      editor.executeAction(atomicAction);
    } else {
      editor.executeActions([{ action: atomicAction, count }]);
    }
    if (operator === "y") {
      state.lastYankWasLinewise = false;
    }
    if (operator === "c") {
      switchMode("insert");
      return;
    }
    switchMode("normal");
    return;
  }

  // Fall back to selection-based approach for motions without atomic actions
  const selectAction = motionToSelection[motionAction];
  if (!selectAction) {
    editor.debug(`No selection equivalent for motion: ${motionAction}`);
    switchMode("normal");
    return;
  }

  // Execute the selection action count times (synchronous - extends selection to target)
  if (count === 1) {
    editor.executeAction(selectAction);
  } else {
    editor.executeActions([{ action: selectAction, count }]);
  }

  switch (operator) {
    case "d": // delete
      editor.executeAction("cut"); // Cut removes selection
      break;
    case "c": // change (delete and enter insert mode)
      editor.executeAction("cut");
      switchMode("insert");
      return; // Don't switch back to normal mode
    case "y": // yank
      state.lastYankWasLinewise = false; // Motion-based yank is character-wise
      editor.executeAction("copy");
      // Move cursor back to start of selection (left side)
      editor.executeAction("move_left");
      break;
  }

  switchMode("normal");
}

// Handle motion in operator-pending mode
// Consumes any pending count and applies it to the motion
function handleMotionWithOperator(motionAction: string): void {
  if (!state.pendingOperator) {
    switchMode("normal");
    return;
  }

  const count = consumeCount();
  applyOperatorWithMotion(state.pendingOperator, motionAction, count);
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
  executeWithCount("move_down");
}
registerHandler("vi_down", vi_down);

function vi_up() : void {
  executeWithCount("move_up");
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
  const line = editor.getCursorLine();
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
  editor.executeAction("go_to_matching_bracket");
}
registerHandler("vi_matching_bracket", vi_matching_bracket);

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
  switchMode("normal");
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
function vi_delete_line() : void {
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "delete_line", count };
  if (count === 1) {
    editor.executeAction("delete_line");
  } else {
    editor.executeActions([{ action: "delete_line", count }]);
  }
  switchMode("normal");
}
registerHandler("vi_delete_line", vi_delete_line);

function vi_change_line() : void {
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "change_line", count };
  // Select from line start to line end, then cut (avoids stale snapshot issue)
  editor.executeAction("move_line_start");
  editor.executeAction("select_line_end");
  editor.executeAction("cut");
  switchMode("insert");
}
registerHandler("vi_change_line", vi_change_line);

function vi_yank_line() : void {
  const count = consumeCount();
  // select_line selects current line and moves cursor to next line
  if (count === 1) {
    editor.executeAction("select_line");
  } else {
    editor.executeActions([{ action: "select_line", count }]);
  }
  editor.executeAction("copy");
  // Move back to original line using synchronous actions
  // (setBufferCursor is async and doesn't take effect in time)
  editor.executeAction("move_up");
  editor.executeAction("move_line_start");
  state.lastYankWasLinewise = true;
  editor.setStatus(editor.t("status.yanked_lines", { count: String(count) }));
  switchMode("normal");
}
registerHandler("vi_yank_line", vi_yank_line);

// Single character operations - support count prefix (3x = delete 3 chars)
function vi_delete_char() : void {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "delete_forward", count };
  executeWithCount("delete_forward", count);
}
registerHandler("vi_delete_char", vi_delete_char);

function vi_delete_char_before() : void {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "delete_backward", count };
  executeWithCount("delete_backward", count);
}
registerHandler("vi_delete_char_before", vi_delete_char_before);

function vi_replace_char() : void {
  // Enter replace-char mode to read the replacement character
  state.mode = "find-char"; // Reuse find-char mode mechanism
  editor.setEditorMode("vi-replace-char");
  editor.setStatus("-- REPLACE CHAR --");
}
registerHandler("vi_replace_char", vi_replace_char);

// Handler for replacement character input
async function vi_replace_char_handler(char: string): Promise<void> {
  const count = consumeCount();
  // Replace character(s) under cursor without moving
  for (let i = 0; i < count; i++) {
    editor.executeAction("delete_forward");
    editor.insertAtCursor(char);
  }
  // Move cursor back to stay on the replaced char (vim behavior)
  editor.executeAction("move_left");
  switchMode("normal");
}
registerHandler("vi_replace_char_handler", vi_replace_char_handler);

// Substitute (delete char and enter insert mode)
function vi_substitute() : void {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "substitute", count };
  if (count > 1) {
    editor.executeActions([{ action: "delete_forward", count }]);
  } else {
    editor.executeAction("delete_forward");
  }
  switchMode("insert");
}
registerHandler("vi_substitute", vi_substitute);

// Delete to end of line (D)
function vi_delete_to_end() : void {
  state.lastChange = { type: "operator-motion", operator: "d", motion: "move_line_end" };
  // Use the atomic delete_to_line_end action to avoid stale snapshot issues
  editor.executeAction("delete_to_line_end");
}
registerHandler("vi_delete_to_end", vi_delete_to_end);

// Change to end of line (C)
function vi_change_to_end() : void {
  state.lastChange = { type: "operator-motion", operator: "c", motion: "move_line_end" };
  // Use the atomic delete_to_line_end action to avoid stale snapshot issues
  editor.executeAction("delete_to_line_end");
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
  const count = consumeCount() || change.count || 1;

  switch (change.type) {
    case "simple": {
      // Simple actions like x, X, s
      if (change.action === "substitute") {
        // Substitute: delete chars and insert text
        if (count > 1) {
          editor.executeActions([{ action: "delete_forward", count }]);
        } else {
          editor.executeAction("delete_forward");
        }
        if (change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      } else if (change.action) {
        // Simple action like delete_forward, delete_backward
        if (count > 1) {
          editor.executeActions([{ action: change.action, count }]);
        } else {
          editor.executeAction(change.action);
        }
      }
      break;
    }

    case "line-op": {
      // Line operations like dd, cc
      if (change.action === "delete_line") {
        if (count > 1) {
          editor.executeActions([{ action: "delete_line", count }]);
        } else {
          editor.executeAction("delete_line");
        }
      } else if (change.action === "change_line") {
        // Change line: select line content, cut, insert text
        editor.executeAction("move_line_start");
        editor.executeAction("select_line_end");
        editor.executeAction("cut");
        if (change.insertedText) {
          editor.insertAtCursor(change.insertedText);
        }
      }
      break;
    }

    case "operator-motion": {
      // Operator + motion like dw, cw, d$
      if (change.operator && change.motion) {
        if (change.operator === "c") {
          // For change: do the delete part, then insert the text
          applyOperatorWithMotion("d", change.motion, count);
          if (change.insertedText) {
            editor.insertAtCursor(change.insertedText);
          }
        } else {
          applyOperatorWithMotion(change.operator, change.motion, count);
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
  editor.executeAction("search");
}
registerHandler("vi_search_forward", vi_search_forward);

function vi_search_backward() : void {
  // Use same search dialog, user can search backward manually
  editor.executeAction("search");
}
registerHandler("vi_search_backward", vi_search_backward);

function vi_find_next() : void {
  editor.executeAction("find_next");
}
registerHandler("vi_find_next", vi_find_next);

function vi_find_prev() : void {
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
function vi_op_digit_0_or_line_start() : void {
  if (state.count !== null) {
    accumulateCount(0);
  } else {
    handleMotionWithOperator("move_line_start");
  }
}
registerHandler("vi_op_digit_0_or_line_start", vi_op_digit_0_or_line_start);

// ============================================================================
// Visual Mode
// ============================================================================

// Enter character-wise visual mode
function vi_visual_char() : void {
  state.visualAnchor = editor.getCursorPosition();
  // Set anchor at current position so subsequent select_* motions extend from here
  editor.executeAction("set_mark");
  switchMode("visual");
}
registerHandler("vi_visual_char", vi_visual_char);

// Enter line-wise visual mode
function vi_visual_line() : void {
  state.visualAnchor = editor.getCursorPosition();
  // Select full line including newline (select_line selects and moves to next line)
  editor.executeAction("select_line");
  switchMode("visual-line");
}
registerHandler("vi_visual_line", vi_visual_line);

// Toggle between visual and visual-line modes
function vi_visual_toggle_line() : void {
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

  // Calculate line and column for block anchor
  const cursorPos = editor.getCursorPosition();
  if (cursorPos !== null) {
    const line = editor.getCursorLine() ?? 1;
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
  executeWithCount("select_left");
}
registerHandler("vi_vis_left", vi_vis_left);

function vi_vis_down() : void {
  executeWithCount("select_down");
}
registerHandler("vi_vis_down", vi_vis_down);

function vi_vis_up() : void {
  executeWithCount("select_up");
}
registerHandler("vi_vis_up", vi_vis_up);

function vi_vis_right() : void {
  executeWithCount("select_right");
}
registerHandler("vi_vis_right", vi_vis_right);

function vi_vis_word() : void {
  executeWithCount("select_word_right");
}
registerHandler("vi_vis_word", vi_vis_word);

function vi_vis_word_back() : void {
  executeWithCount("select_word_left");
}
registerHandler("vi_vis_word_back", vi_vis_word_back);

function vi_vis_word_end() : void {
  // Extend selection to end of word — uses native vi_select_word_end action
  executeWithCount("vi_select_word_end");
}
registerHandler("vi_vis_word_end", vi_vis_word_end);

function vi_vis_line_start() : void {
  consumeCount();
  editor.executeAction("select_line_start");
}
registerHandler("vi_vis_line_start", vi_vis_line_start);

function vi_vis_line_end() : void {
  consumeCount();
  editor.executeAction("select_line_end");
}
registerHandler("vi_vis_line_end", vi_vis_line_end);

function vi_vis_doc_start() : void {
  consumeCount();
  editor.executeAction("select_document_start");
}
registerHandler("vi_vis_doc_start", vi_vis_doc_start);

function vi_vis_doc_end() : void {
  consumeCount();
  editor.executeAction("select_document_end");
}
registerHandler("vi_vis_doc_end", vi_vis_doc_end);

// Visual line mode motions - extend selection by whole lines
function vi_vline_down() : void {
  executeWithCount("select_down");
  // Ensure full line selection
  editor.executeAction("select_line_end");
}
registerHandler("vi_vline_down", vi_vline_down);

function vi_vline_up() : void {
  executeWithCount("select_up");
  // Ensure full line selection
  editor.executeAction("select_line_start");
}
registerHandler("vi_vline_up", vi_vline_up);

// Visual mode operators - act on selection
function vi_vis_delete() : void {
  const wasLinewise = state.mode === "visual-line";
  editor.executeAction("cut");
  state.lastYankWasLinewise = wasLinewise;
  switchMode("normal");
}
registerHandler("vi_vis_delete", vi_vis_delete);

function vi_vis_change() : void {
  editor.executeAction("cut");
  switchMode("insert");
}
registerHandler("vi_vis_change", vi_vis_change);

function vi_vis_yank() : void {
  const wasLinewise = state.mode === "visual-line";
  editor.executeAction("copy");
  state.lastYankWasLinewise = wasLinewise;
  // Move cursor to start of selection (vim behavior)
  editor.executeAction("move_left");
  switchMode("normal");
}
registerHandler("vi_vis_yank", vi_vis_yank);

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

      // Find quote pair containing cursor
      let quoteStart = -1;
      let quoteEnd = -1;
      let inQuote = false;

      for (let i = 0; i < line.length; i++) {
        if (line[i] === objectType) {
          if (!inQuote) {
            quoteStart = i;
            inQuote = true;
          } else {
            quoteEnd = i;
            if (colInLine >= quoteStart && colInLine <= quoteEnd) {
              break; // Found the pair containing cursor
            }
            inQuote = false;
          }
        }
      }

      if (quoteStart !== -1 && quoteEnd !== -1 && colInLine >= quoteStart && colInLine <= quoteEnd) {
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
      // Delete the range directly
      editor.deleteRange(bufferId, selectStart, selectEnd);
      state.lastYankWasLinewise = false;
      break;
    }
    case "c": {
      // Delete and enter insert mode
      editor.deleteRange(bufferId, selectStart, selectEnd);
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

// Enter find-char mode waiting for the target character
function enterFindCharMode(findType: FindCharType): void {
  state.pendingFindChar = findType;
  state.mode = "find-char";
  editor.setEditorMode("vi-find-char");
  editor.setStatus(getModeIndicator("find-char"));
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

// Handler for when a character is typed in find-char mode (async)
async function vi_find_char_handler(char: string): Promise<void> {
  if (state.pendingFindChar) {
    await executeFindChar(state.pendingFindChar, char);
  }
  // Return to normal mode
  state.pendingFindChar = null;
  switchMode("normal");
}
registerHandler("vi_find_char_handler", vi_find_char_handler);

// Commands to enter find-char mode
function vi_find_char_f(): void {
  enterFindCharMode("f");
}
registerHandler("vi_find_char_f", vi_find_char_f);

function vi_find_char_t(): void {
  enterFindCharMode("t");
}
registerHandler("vi_find_char_t", vi_find_char_t);

function vi_find_char_F(): void {
  enterFindCharMode("F");
}
registerHandler("vi_find_char_F", vi_find_char_F);

function vi_find_char_T(): void {
  enterFindCharMode("T");
}
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

// Cancel find-char mode
function vi_find_char_cancel(): void {
  state.pendingFindChar = null;
  switchMode("normal");
}
registerHandler("vi_find_char_cancel", vi_find_char_cancel);

// ============================================================================
// Operator-Pending Mode Commands
// ============================================================================

function vi_op_left(): void {
  handleMotionWithOperator("move_left");
}
registerHandler("vi_op_left", vi_op_left);

function vi_op_down(): void {
  handleMotionWithOperator("move_down");
}
registerHandler("vi_op_down", vi_op_down);

function vi_op_up(): void {
  handleMotionWithOperator("move_up");
}
registerHandler("vi_op_up", vi_op_up);

function vi_op_right(): void {
  handleMotionWithOperator("move_right");
}
registerHandler("vi_op_right", vi_op_right);

function vi_op_word(): void {
  handleMotionWithOperator("move_word_right");
}
registerHandler("vi_op_word", vi_op_word);

function vi_op_word_back(): void {
  handleMotionWithOperator("move_word_left");
}
registerHandler("vi_op_word_back", vi_op_word_back);

// Operator-pending e (word end) - select to word end, then apply operator
// Operator-pending e (word end) — uses native vi_move_word_end motion
function vi_op_word_end(): void {
  handleMotionWithOperator("vi_move_word_end");
}
registerHandler("vi_op_word_end", vi_op_word_end);

function vi_op_line_start(): void {
  handleMotionWithOperator("move_line_start");
}
registerHandler("vi_op_line_start", vi_op_line_start);

function vi_op_line_end(): void {
  handleMotionWithOperator("move_line_end");
}
registerHandler("vi_op_line_end", vi_op_line_end);

function vi_op_doc_start(): void {
  handleMotionWithOperator("move_document_start");
}
registerHandler("vi_op_doc_start", vi_op_doc_start);

function vi_op_doc_end(): void {
  handleMotionWithOperator("move_document_end");
}
registerHandler("vi_op_doc_end", vi_op_doc_end);

function vi_op_matching_bracket(): void {
  handleMotionWithOperator("go_to_matching_bracket");
}
registerHandler("vi_op_matching_bracket", vi_op_matching_bracket);

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

  // Search
  ["/", "vi_search_forward"],
  ["?", "vi_search_backward"],
  ["n", "vi_find_next"],
  ["N", "vi_find_prev"],

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
  // Pass through to standard editor shortcuts
  ["C-p", "command_palette"],
  ["C-q", "quit"],
], false); // read_only = false to allow normal typing

// Define vi-find-char mode - binds all printable chars to the handler
// This mode waits for a single character input for f/t/F/T motions

// Explicitly define handlers for each character to ensure they're accessible
// These return Promises so the runtime can await them
async function vi_fc_a(): Promise<void> { return vi_find_char_handler("a"); }
registerHandler("vi_fc_a", vi_fc_a);
async function vi_fc_b(): Promise<void> { return vi_find_char_handler("b"); }
registerHandler("vi_fc_b", vi_fc_b);
async function vi_fc_c(): Promise<void> { return vi_find_char_handler("c"); }
registerHandler("vi_fc_c", vi_fc_c);
async function vi_fc_d(): Promise<void> { return vi_find_char_handler("d"); }
registerHandler("vi_fc_d", vi_fc_d);
async function vi_fc_e(): Promise<void> { return vi_find_char_handler("e"); }
registerHandler("vi_fc_e", vi_fc_e);
async function vi_fc_f(): Promise<void> { return vi_find_char_handler("f"); }
registerHandler("vi_fc_f", vi_fc_f);
async function vi_fc_g(): Promise<void> { return vi_find_char_handler("g"); }
registerHandler("vi_fc_g", vi_fc_g);
async function vi_fc_h(): Promise<void> { return vi_find_char_handler("h"); }
registerHandler("vi_fc_h", vi_fc_h);
async function vi_fc_i(): Promise<void> { return vi_find_char_handler("i"); }
registerHandler("vi_fc_i", vi_fc_i);
async function vi_fc_j(): Promise<void> { return vi_find_char_handler("j"); }
registerHandler("vi_fc_j", vi_fc_j);
async function vi_fc_k(): Promise<void> { return vi_find_char_handler("k"); }
registerHandler("vi_fc_k", vi_fc_k);
async function vi_fc_l(): Promise<void> { return vi_find_char_handler("l"); }
registerHandler("vi_fc_l", vi_fc_l);
async function vi_fc_m(): Promise<void> { return vi_find_char_handler("m"); }
registerHandler("vi_fc_m", vi_fc_m);
async function vi_fc_n(): Promise<void> { return vi_find_char_handler("n"); }
registerHandler("vi_fc_n", vi_fc_n);
async function vi_fc_o(): Promise<void> { return vi_find_char_handler("o"); }
registerHandler("vi_fc_o", vi_fc_o);
async function vi_fc_p(): Promise<void> { return vi_find_char_handler("p"); }
registerHandler("vi_fc_p", vi_fc_p);
async function vi_fc_q(): Promise<void> { return vi_find_char_handler("q"); }
registerHandler("vi_fc_q", vi_fc_q);
async function vi_fc_r(): Promise<void> { return vi_find_char_handler("r"); }
registerHandler("vi_fc_r", vi_fc_r);
async function vi_fc_s(): Promise<void> { return vi_find_char_handler("s"); }
registerHandler("vi_fc_s", vi_fc_s);
async function vi_fc_t(): Promise<void> { return vi_find_char_handler("t"); }
registerHandler("vi_fc_t", vi_fc_t);
async function vi_fc_u(): Promise<void> { return vi_find_char_handler("u"); }
registerHandler("vi_fc_u", vi_fc_u);
async function vi_fc_v(): Promise<void> { return vi_find_char_handler("v"); }
registerHandler("vi_fc_v", vi_fc_v);
async function vi_fc_w(): Promise<void> { return vi_find_char_handler("w"); }
registerHandler("vi_fc_w", vi_fc_w);
async function vi_fc_x(): Promise<void> { return vi_find_char_handler("x"); }
registerHandler("vi_fc_x", vi_fc_x);
async function vi_fc_y(): Promise<void> { return vi_find_char_handler("y"); }
registerHandler("vi_fc_y", vi_fc_y);
async function vi_fc_z(): Promise<void> { return vi_find_char_handler("z"); }
registerHandler("vi_fc_z", vi_fc_z);
async function vi_fc_A(): Promise<void> { return vi_find_char_handler("A"); }
registerHandler("vi_fc_A", vi_fc_A);
async function vi_fc_B(): Promise<void> { return vi_find_char_handler("B"); }
registerHandler("vi_fc_B", vi_fc_B);
async function vi_fc_C(): Promise<void> { return vi_find_char_handler("C"); }
registerHandler("vi_fc_C", vi_fc_C);
async function vi_fc_D(): Promise<void> { return vi_find_char_handler("D"); }
registerHandler("vi_fc_D", vi_fc_D);
async function vi_fc_E(): Promise<void> { return vi_find_char_handler("E"); }
registerHandler("vi_fc_E", vi_fc_E);
async function vi_fc_F(): Promise<void> { return vi_find_char_handler("F"); }
registerHandler("vi_fc_F", vi_fc_F);
async function vi_fc_G(): Promise<void> { return vi_find_char_handler("G"); }
registerHandler("vi_fc_G", vi_fc_G);
async function vi_fc_H(): Promise<void> { return vi_find_char_handler("H"); }
registerHandler("vi_fc_H", vi_fc_H);
async function vi_fc_I(): Promise<void> { return vi_find_char_handler("I"); }
registerHandler("vi_fc_I", vi_fc_I);
async function vi_fc_J(): Promise<void> { return vi_find_char_handler("J"); }
registerHandler("vi_fc_J", vi_fc_J);
async function vi_fc_K(): Promise<void> { return vi_find_char_handler("K"); }
registerHandler("vi_fc_K", vi_fc_K);
async function vi_fc_L(): Promise<void> { return vi_find_char_handler("L"); }
registerHandler("vi_fc_L", vi_fc_L);
async function vi_fc_M(): Promise<void> { return vi_find_char_handler("M"); }
registerHandler("vi_fc_M", vi_fc_M);
async function vi_fc_N(): Promise<void> { return vi_find_char_handler("N"); }
registerHandler("vi_fc_N", vi_fc_N);
async function vi_fc_O(): Promise<void> { return vi_find_char_handler("O"); }
registerHandler("vi_fc_O", vi_fc_O);
async function vi_fc_P(): Promise<void> { return vi_find_char_handler("P"); }
registerHandler("vi_fc_P", vi_fc_P);
async function vi_fc_Q(): Promise<void> { return vi_find_char_handler("Q"); }
registerHandler("vi_fc_Q", vi_fc_Q);
async function vi_fc_R(): Promise<void> { return vi_find_char_handler("R"); }
registerHandler("vi_fc_R", vi_fc_R);
async function vi_fc_S(): Promise<void> { return vi_find_char_handler("S"); }
registerHandler("vi_fc_S", vi_fc_S);
async function vi_fc_T(): Promise<void> { return vi_find_char_handler("T"); }
registerHandler("vi_fc_T", vi_fc_T);
async function vi_fc_U(): Promise<void> { return vi_find_char_handler("U"); }
registerHandler("vi_fc_U", vi_fc_U);
async function vi_fc_V(): Promise<void> { return vi_find_char_handler("V"); }
registerHandler("vi_fc_V", vi_fc_V);
async function vi_fc_W(): Promise<void> { return vi_find_char_handler("W"); }
registerHandler("vi_fc_W", vi_fc_W);
async function vi_fc_X(): Promise<void> { return vi_find_char_handler("X"); }
registerHandler("vi_fc_X", vi_fc_X);
async function vi_fc_Y(): Promise<void> { return vi_find_char_handler("Y"); }
registerHandler("vi_fc_Y", vi_fc_Y);
async function vi_fc_Z(): Promise<void> { return vi_find_char_handler("Z"); }
registerHandler("vi_fc_Z", vi_fc_Z);
async function vi_fc_0(): Promise<void> { return vi_find_char_handler("0"); }
registerHandler("vi_fc_0", vi_fc_0);
async function vi_fc_1(): Promise<void> { return vi_find_char_handler("1"); }
registerHandler("vi_fc_1", vi_fc_1);
async function vi_fc_2(): Promise<void> { return vi_find_char_handler("2"); }
registerHandler("vi_fc_2", vi_fc_2);
async function vi_fc_3(): Promise<void> { return vi_find_char_handler("3"); }
registerHandler("vi_fc_3", vi_fc_3);
async function vi_fc_4(): Promise<void> { return vi_find_char_handler("4"); }
registerHandler("vi_fc_4", vi_fc_4);
async function vi_fc_5(): Promise<void> { return vi_find_char_handler("5"); }
registerHandler("vi_fc_5", vi_fc_5);
async function vi_fc_6(): Promise<void> { return vi_find_char_handler("6"); }
registerHandler("vi_fc_6", vi_fc_6);
async function vi_fc_7(): Promise<void> { return vi_find_char_handler("7"); }
registerHandler("vi_fc_7", vi_fc_7);
async function vi_fc_8(): Promise<void> { return vi_find_char_handler("8"); }
registerHandler("vi_fc_8", vi_fc_8);
async function vi_fc_9(): Promise<void> { return vi_find_char_handler("9"); }
registerHandler("vi_fc_9", vi_fc_9);
async function vi_fc_space(): Promise<void> { return vi_find_char_handler(" "); }
registerHandler("vi_fc_space", vi_fc_space);

// Punctuation character handlers for find-char mode
const punctuationChars: [string, string][] = [
  ["!", "excl"], ["@", "at"], ["#", "hash"], ["$", "dollar"], ["%", "percent"],
  ["^", "caret"], ["&", "amp"], ["*", "star"], ["(", "lparen"], [")", "rparen"],
  ["-", "minus"], ["_", "underscore"], ["=", "equals"], ["+", "plus"],
  ["[", "lbracket"], ["]", "rbracket"], ["{", "lbrace"], ["}", "rbrace"],
  ["\\", "backslash"], ["|", "pipe"], [";", "semi"], [":", "colon"],
  ["'", "squote"], ["\"", "dquote"], [",", "comma"], [".", "dot"],
  ["<", "lt"], [">", "gt"], ["/", "slash"], ["?", "question"], ["`", "backtick"],
  ["~", "tilde"],
];

for (const [char, name] of punctuationChars) {
  const handlerName = `vi_fc_p_${name}`;
  const handler = async (): Promise<void> => { return vi_find_char_handler(char); };
  registerHandler(handlerName, handler);
}

// Define vi-find-char mode with all the character bindings
editor.defineMode("vi-find-char", [
  ["Escape", "vi_find_char_cancel"],
  // Letters
  ["a", "vi_fc_a"], ["b", "vi_fc_b"], ["c", "vi_fc_c"], ["d", "vi_fc_d"],
  ["e", "vi_fc_e"], ["f", "vi_fc_f"], ["g", "vi_fc_g"], ["h", "vi_fc_h"],
  ["i", "vi_fc_i"], ["j", "vi_fc_j"], ["k", "vi_fc_k"], ["l", "vi_fc_l"],
  ["m", "vi_fc_m"], ["n", "vi_fc_n"], ["o", "vi_fc_o"], ["p", "vi_fc_p"],
  ["q", "vi_fc_q"], ["r", "vi_fc_r"], ["s", "vi_fc_s"], ["t", "vi_fc_t"],
  ["u", "vi_fc_u"], ["v", "vi_fc_v"], ["w", "vi_fc_w"], ["x", "vi_fc_x"],
  ["y", "vi_fc_y"], ["z", "vi_fc_z"],
  ["A", "vi_fc_A"], ["B", "vi_fc_B"], ["C", "vi_fc_C"], ["D", "vi_fc_D"],
  ["E", "vi_fc_E"], ["F", "vi_fc_F"], ["G", "vi_fc_G"], ["H", "vi_fc_H"],
  ["I", "vi_fc_I"], ["J", "vi_fc_J"], ["K", "vi_fc_K"], ["L", "vi_fc_L"],
  ["M", "vi_fc_M"], ["N", "vi_fc_N"], ["O", "vi_fc_O"], ["P", "vi_fc_P"],
  ["Q", "vi_fc_Q"], ["R", "vi_fc_R"], ["S", "vi_fc_S"], ["T", "vi_fc_T"],
  ["U", "vi_fc_U"], ["V", "vi_fc_V"], ["W", "vi_fc_W"], ["X", "vi_fc_X"],
  ["Y", "vi_fc_Y"], ["Z", "vi_fc_Z"],
  // Digits
  ["0", "vi_fc_0"], ["1", "vi_fc_1"], ["2", "vi_fc_2"], ["3", "vi_fc_3"],
  ["4", "vi_fc_4"], ["5", "vi_fc_5"], ["6", "vi_fc_6"], ["7", "vi_fc_7"],
  ["8", "vi_fc_8"], ["9", "vi_fc_9"],
  // Space and punctuation
  ["Space", "vi_fc_space"],
  ["!", "vi_fc_p_excl"], ["@", "vi_fc_p_at"], ["#", "vi_fc_p_hash"],
  ["$", "vi_fc_p_dollar"], ["%", "vi_fc_p_percent"], ["^", "vi_fc_p_caret"],
  ["&", "vi_fc_p_amp"], ["*", "vi_fc_p_star"], ["(", "vi_fc_p_lparen"],
  [")", "vi_fc_p_rparen"], ["-", "vi_fc_p_minus"], ["_", "vi_fc_p_underscore"],
  ["=", "vi_fc_p_equals"], ["+", "vi_fc_p_plus"], ["[", "vi_fc_p_lbracket"],
  ["]", "vi_fc_p_rbracket"], ["{", "vi_fc_p_lbrace"], ["}", "vi_fc_p_rbrace"],
  ["\\", "vi_fc_p_backslash"], ["|", "vi_fc_p_pipe"], [";", "vi_fc_p_semi"],
  [":", "vi_fc_p_colon"], ["'", "vi_fc_p_squote"], ["\"", "vi_fc_p_dquote"],
  [",", "vi_fc_p_comma"], [".", "vi_fc_p_dot"], ["<", "vi_fc_p_lt"],
  [">", "vi_fc_p_gt"], ["/", "vi_fc_p_slash"], ["?", "vi_fc_p_question"],
  ["`", "vi_fc_p_backtick"], ["~", "vi_fc_p_tilde"],
], true);

// Define vi-replace-char mode — reuses find-char handlers but calls vi_replace_char_handler
// We create replace-char specific handlers that delegate to the replace handler
const rcHandlers: [string, string][] = [];
for (const c of "abcdefghijklmnopqrstuvwxyz") {
  const name = `vi_rc_${c}`;
  const handler = async (): Promise<void> => { return vi_replace_char_handler(c); };
  registerHandler(name, handler);
  rcHandlers.push([c, name]);
}
for (const c of "ABCDEFGHIJKLMNOPQRSTUVWXYZ") {
  const name = `vi_rc_${c}`;
  const handler = async (): Promise<void> => { return vi_replace_char_handler(c); };
  registerHandler(name, handler);
  rcHandlers.push([c, name]);
}
for (const c of "0123456789") {
  const name = `vi_rc_d${c}`;
  const handler = async (): Promise<void> => { return vi_replace_char_handler(c); };
  registerHandler(name, handler);
  rcHandlers.push([c, name]);
}
// Space and common punctuation for replace-char mode
const rcSpaceHandler = async (): Promise<void> => { return vi_replace_char_handler(" "); };
registerHandler("vi_rc_space", rcSpaceHandler);
for (const [char, pname] of punctuationChars) {
  const name = `vi_rc_p_${pname}`;
  const handler = async (): Promise<void> => { return vi_replace_char_handler(char); };
  registerHandler(name, handler);
}

editor.defineMode("vi-replace-char", [
  ["Escape", "vi_find_char_cancel"],
  ...rcHandlers.map(([key, name]): [string, string] => [key, name]),
  ["Space", "vi_rc_space"],
  ...punctuationChars.map(([char, pname]): [string, string] => [char, `vi_rc_p_${pname}`]),
], true);

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
  ["$", "vi_op_line_end"],
  ["g g", "vi_op_doc_start"],
  ["G", "vi_op_doc_end"],
  ["%", "vi_op_matching_bracket"],

  // Text objects
  ["i", "vi_text_object_inner"],
  ["a", "vi_text_object_around"],

  // Double operator = line operation
  ["d", "vi_delete_line"],
  ["c", "vi_change_line"],
  ["y", "vi_yank_line"],

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
  ["$", "vi_vis_line_end"],
  ["^", "vi_vis_line_start"],
  ["g g", "vi_vis_doc_start"],
  ["G", "vi_vis_doc_end"],

  // Switch visual sub-modes
  ["V", "vi_visual_toggle_line"],
  ["C-v", "vi_visual_block"],  // Switch to block mode

  // Operators
  ["d", "vi_vis_delete"],
  ["x", "vi_vis_delete"],
  ["c", "vi_vis_change"],
  ["s", "vi_vis_change"],
  ["y", "vi_vis_yank"],

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
async function vi_command_handler(args: { prompt_type: string; input: string }): Promise<boolean> {
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
}
registerHandler("vi_command_handler", vi_command_handler);

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
        const line = editor.getCursorLine();
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
editor.on("prompt_confirmed", "vi_command_handler");

// ============================================================================
// Toggle Command
// ============================================================================

let viModeEnabled = false;

function vi_mode_toggle(): void {
  editor.debug("[vi_mode_toggle] called, viModeEnabled was: " + viModeEnabled);
  viModeEnabled = !viModeEnabled;
  editor.debug("[vi_mode_toggle] viModeEnabled now: " + viModeEnabled);

  if (viModeEnabled) {
    editor.debug("[vi_mode_toggle] enabling vi mode, calling switchMode('normal')");
    switchMode("normal");
    editor.debug("[vi_mode_toggle] switchMode done, setting status");
    editor.setStatus(editor.t("status.enabled"));
  } else {
    editor.debug("[vi_mode_toggle] disabling vi mode");
    editor.setEditorMode(null);
    state.mode = "normal";
    state.pendingOperator = null;
    editor.setStatus(editor.t("status.disabled"));
  }
  editor.debug("[vi_mode_toggle] done");
}
registerHandler("vi_mode_toggle", vi_mode_toggle);

editor.registerCommand(
  "%cmd.toggle_vi_mode",
  "%cmd.toggle_vi_mode_desc",
  "vi_mode_toggle",
  null,  // Always visible - needed to enable vi mode in the first place
);

// ============================================================================
// Initialization
// ============================================================================


registerHandler("vi_to_brace", vi_to_brace);