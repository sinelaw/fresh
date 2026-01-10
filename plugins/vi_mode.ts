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
  state.count = null;
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
    move_line_end: "delete_to_line_end",
    move_line_start: "delete_to_line_start",
  },
  y: {
    // Yank operators
    move_word_right: "yank_word_forward",
    move_word_left: "yank_word_backward",
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
globalThis.vi_left = function (): void {
  executeWithCount("move_left");
};

globalThis.vi_down = function (): void {
  executeWithCount("move_down");
};

globalThis.vi_up = function (): void {
  executeWithCount("move_up");
};

globalThis.vi_right = function (): void {
  executeWithCount("move_right");
};

globalThis.vi_word = function (): void {
  executeWithCount("move_word_right");
};

globalThis.vi_word_back = function (): void {
  executeWithCount("move_word_left");
};

globalThis.vi_word_end = function (): void {
  // Move to end of word - for count, repeat the whole operation
  const count = consumeCount();
  for (let i = 0; i < count; i++) {
    editor.executeAction("move_word_right");
    editor.executeAction("move_left");
  }
};

globalThis.vi_line_start = function (): void {
  consumeCount(); // Count doesn't apply to line start
  editor.executeAction("move_line_start");
};

globalThis.vi_line_end = function (): void {
  consumeCount(); // Count doesn't apply to line end
  editor.executeAction("move_line_end");
};

globalThis.vi_first_non_blank = function (): void {
  consumeCount(); // Count doesn't apply
  editor.executeAction("move_line_start");
  // TODO: skip whitespace
};

globalThis.vi_doc_start = function (): void {
  consumeCount(); // Count doesn't apply
  editor.executeAction("move_document_start");
};

globalThis.vi_doc_end = function (): void {
  consumeCount(); // Count doesn't apply
  editor.executeAction("move_document_end");
};

globalThis.vi_page_down = function (): void {
  executeWithCount("page_down");
};

globalThis.vi_page_up = function (): void {
  executeWithCount("page_up");
};

globalThis.vi_matching_bracket = function (): void {
  editor.executeAction("go_to_matching_bracket");
};

// Mode switching
globalThis.vi_insert_before = function (): void {
  switchMode("insert");
};

globalThis.vi_insert_after = function (): void {
  editor.executeAction("move_right");
  switchMode("insert");
};

globalThis.vi_insert_line_start = function (): void {
  editor.executeAction("move_line_start");
  switchMode("insert");
};

globalThis.vi_insert_line_end = function (): void {
  editor.executeAction("move_line_end");
  switchMode("insert");
};

globalThis.vi_open_below = function (): void {
  editor.executeAction("move_line_end");
  editor.executeAction("insert_newline");
  switchMode("insert");
};

globalThis.vi_open_above = function (): void {
  editor.executeAction("move_line_start");
  editor.executeAction("insert_newline");
  editor.executeAction("move_up");
  switchMode("insert");
};

globalThis.vi_escape = function (): void {
  switchMode("normal");
};

// Operators
globalThis.vi_delete_operator = function (): void {
  state.pendingOperator = "d";
  switchMode("operator-pending");
};

globalThis.vi_change_operator = function (): void {
  state.pendingOperator = "c";
  switchMode("operator-pending");
};

globalThis.vi_yank_operator = function (): void {
  state.pendingOperator = "y";
  switchMode("operator-pending");
};

// Line operations (dd, cc, yy) - support count prefix (3dd = delete 3 lines)
globalThis.vi_delete_line = function (): void {
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "delete_line", count };
  if (count === 1) {
    editor.executeAction("delete_line");
  } else {
    editor.executeActions([{ action: "delete_line", count }]);
  }
  switchMode("normal");
};

globalThis.vi_change_line = function (): void {
  const count = consumeCount();
  state.lastChange = { type: "line-op", action: "change_line", count };
  editor.executeAction("move_line_start");
  const start = editor.getCursorPosition();
  editor.executeAction("move_line_end");
  const end = editor.getCursorPosition();
  if (start !== null && end !== null) {
    editor.deleteRange(editor.getActiveBufferId(), start, end);
  }
  switchMode("insert");
};

globalThis.vi_yank_line = function (): void {
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
};

// Single character operations - support count prefix (3x = delete 3 chars)
globalThis.vi_delete_char = function (): void {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "delete_forward", count };
  executeWithCount("delete_forward", count);
};

globalThis.vi_delete_char_before = function (): void {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "delete_backward", count };
  executeWithCount("delete_backward", count);
};

globalThis.vi_replace_char = function (): void {
  // TODO: implement character replacement (need to read next char)
  editor.setStatus(editor.t("status.replace_not_implemented"));
};

// Substitute (delete char and enter insert mode)
globalThis.vi_substitute = function (): void {
  const count = consumeCount();
  state.lastChange = { type: "simple", action: "substitute", count };
  if (count > 1) {
    editor.executeActions([{ action: "delete_forward", count }]);
  } else {
    editor.executeAction("delete_forward");
  }
  switchMode("insert");
};

// Delete to end of line
globalThis.vi_delete_to_end = function (): void {
  state.lastChange = { type: "operator-motion", operator: "d", motion: "move_line_end" };
  const start = editor.getCursorPosition();
  editor.executeAction("move_line_end");
  const end = editor.getCursorPosition();
  if (start !== null && end !== null && end > start) {
    editor.deleteRange(editor.getActiveBufferId(), start, end);
  }
};

// Change to end of line
globalThis.vi_change_to_end = function (): void {
  state.lastChange = { type: "operator-motion", operator: "c", motion: "move_line_end" };
  const start = editor.getCursorPosition();
  editor.executeAction("move_line_end");
  const end = editor.getCursorPosition();
  if (start !== null && end !== null && end > start) {
    editor.deleteRange(editor.getActiveBufferId(), start, end);
  }
  switchMode("insert");
};

// Clipboard
globalThis.vi_paste_after = function (): void {
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
};

globalThis.vi_paste_before = function (): void {
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
};

// Undo/Redo
globalThis.vi_undo = function (): void {
  editor.executeAction("undo");
};

globalThis.vi_redo = function (): void {
  editor.executeAction("redo");
};

// Repeat last change (. command)
globalThis.vi_repeat = async function (): Promise<void> {
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
        // Change line: delete line content and insert text
        editor.executeAction("move_line_start");
        const start = editor.getCursorPosition();
        editor.executeAction("move_line_end");
        const end = editor.getCursorPosition();
        if (start !== null && end !== null) {
          editor.deleteRange(editor.getActiveBufferId(), start, end);
        }
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
};

// Join lines
globalThis.vi_join = function (): void {
  editor.executeAction("move_line_end");
  editor.executeAction("delete_forward");
  editor.executeAction("insert_text_at_cursor");
};

// Search
globalThis.vi_search_forward = function (): void {
  editor.executeAction("search");
};

globalThis.vi_search_backward = function (): void {
  // Use same search dialog, user can search backward manually
  editor.executeAction("search");
};

globalThis.vi_find_next = function (): void {
  editor.executeAction("find_next");
};

globalThis.vi_find_prev = function (): void {
  editor.executeAction("find_previous");
};

// Center view
globalThis.vi_center_cursor = function (): void {
  editor.executeAction("center_cursor");
};

// Half page movements
globalThis.vi_half_page_down = function (): void {
  // Approximate half page with multiple down movements
  const count = consumeCount();
  editor.executeActions([{ action: "move_down", count: 10 * count }]);
};

globalThis.vi_half_page_up = function (): void {
  const count = consumeCount();
  editor.executeActions([{ action: "move_up", count: 10 * count }]);
};

// ============================================================================
// Count Prefix (digit keys 1-9, and 0 after initial digit)
// ============================================================================

// Digit handlers for count prefix
globalThis.vi_digit_1 = function (): void { accumulateCount(1); };
globalThis.vi_digit_2 = function (): void { accumulateCount(2); };
globalThis.vi_digit_3 = function (): void { accumulateCount(3); };
globalThis.vi_digit_4 = function (): void { accumulateCount(4); };
globalThis.vi_digit_5 = function (): void { accumulateCount(5); };
globalThis.vi_digit_6 = function (): void { accumulateCount(6); };
globalThis.vi_digit_7 = function (): void { accumulateCount(7); };
globalThis.vi_digit_8 = function (): void { accumulateCount(8); };
globalThis.vi_digit_9 = function (): void { accumulateCount(9); };

// 0 is special: if count is already started, it appends; otherwise it's "go to line start"
globalThis.vi_digit_0_or_line_start = function (): void {
  if (state.count !== null) {
    accumulateCount(0);
  } else {
    editor.executeAction("move_line_start");
  }
};

// 0 in operator-pending mode: if count is started, append; otherwise apply operator to line start
globalThis.vi_op_digit_0_or_line_start = function (): void {
  if (state.count !== null) {
    accumulateCount(0);
  } else {
    handleMotionWithOperator("move_line_start");
  }
};

// ============================================================================
// Visual Mode
// ============================================================================

// Enter character-wise visual mode
globalThis.vi_visual_char = function (): void {
  state.visualAnchor = editor.getCursorPosition();
  // Select current character to start visual selection
  editor.executeAction("select_right");
  switchMode("visual");
};

// Enter line-wise visual mode
globalThis.vi_visual_line = function (): void {
  state.visualAnchor = editor.getCursorPosition();
  // Select current line
  editor.executeAction("move_line_start");
  editor.executeAction("select_line");
  switchMode("visual-line");
};

// Toggle between visual and visual-line modes
globalThis.vi_visual_toggle_line = function (): void {
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
};

// Enter visual block mode (Ctrl-v)
globalThis.vi_visual_block = function (): void {
  // Store anchor position for block selection
  state.visualAnchor = editor.getCursorPosition();

  // Calculate line and column for block anchor
  const cursorPos = editor.getCursorPosition();
  if (cursorPos !== null) {
    const line = editor.getCursorLine() ?? 1;
    const lineStart = editor.getLineStartPosition(line);
    const col = lineStart !== null ? cursorPos - lineStart : 0;
    state.visualBlockAnchor = { line, col };
  }

  // Select current character to start
  editor.executeAction("select_right");
  switchMode("visual-block");
};

// Visual block mode motions - these extend the rectangular selection
globalThis.vi_vblock_left = function (): void {
  executeWithCount("select_left");
};

globalThis.vi_vblock_down = function (): void {
  executeWithCount("select_down");
};

globalThis.vi_vblock_up = function (): void {
  executeWithCount("select_up");
};

globalThis.vi_vblock_right = function (): void {
  executeWithCount("select_right");
};

globalThis.vi_vblock_line_start = function (): void {
  consumeCount();
  editor.executeAction("select_line_start");
};

globalThis.vi_vblock_line_end = function (): void {
  consumeCount();
  editor.executeAction("select_line_end");
};

// Visual block delete - delete the selected block
globalThis.vi_vblock_delete = function (): void {
  editor.executeAction("cut");
  state.lastYankWasLinewise = false;
  switchMode("normal");
};

// Visual block change - delete and enter insert mode
globalThis.vi_vblock_change = function (): void {
  editor.executeAction("cut");
  switchMode("insert");
};

// Visual block yank
globalThis.vi_vblock_yank = function (): void {
  editor.executeAction("copy");
  state.lastYankWasLinewise = false;
  // Move cursor to start of selection
  editor.executeAction("move_left");
  switchMode("normal");
};

// Exit visual block mode
globalThis.vi_vblock_escape = function (): void {
  switchMode("normal");
};

// Toggle from visual block to other visual modes
globalThis.vi_vblock_toggle_char = function (): void {
  // Switch to character visual mode
  state.mode = "visual";
  editor.setEditorMode("vi-visual");
  editor.setStatus(getModeIndicator("visual"));
};

globalThis.vi_vblock_toggle_line = function (): void {
  // Switch to line visual mode
  editor.executeAction("select_line");
  state.mode = "visual-line";
  editor.setEditorMode("vi-visual-line");
  editor.setStatus(getModeIndicator("visual-line"));
};

// Visual mode motions - these extend the selection
globalThis.vi_vis_left = function (): void {
  executeWithCount("select_left");
};

globalThis.vi_vis_down = function (): void {
  executeWithCount("select_down");
};

globalThis.vi_vis_up = function (): void {
  executeWithCount("select_up");
};

globalThis.vi_vis_right = function (): void {
  executeWithCount("select_right");
};

globalThis.vi_vis_word = function (): void {
  executeWithCount("select_word_right");
};

globalThis.vi_vis_word_back = function (): void {
  executeWithCount("select_word_left");
};

globalThis.vi_vis_word_end = function (): void {
  const count = consumeCount();
  for (let i = 0; i < count; i++) {
    editor.executeAction("select_word_right");
    editor.executeAction("select_left");
  }
};

globalThis.vi_vis_line_start = function (): void {
  consumeCount();
  editor.executeAction("select_line_start");
};

globalThis.vi_vis_line_end = function (): void {
  consumeCount();
  editor.executeAction("select_line_end");
};

globalThis.vi_vis_doc_start = function (): void {
  consumeCount();
  editor.executeAction("select_document_start");
};

globalThis.vi_vis_doc_end = function (): void {
  consumeCount();
  editor.executeAction("select_document_end");
};

// Visual line mode motions - extend selection by whole lines
globalThis.vi_vline_down = function (): void {
  executeWithCount("select_down");
  // Ensure full line selection
  editor.executeAction("select_line_end");
};

globalThis.vi_vline_up = function (): void {
  executeWithCount("select_up");
  // Ensure full line selection
  editor.executeAction("select_line_start");
};

// Visual mode operators - act on selection
globalThis.vi_vis_delete = function (): void {
  const wasLinewise = state.mode === "visual-line";
  editor.executeAction("cut");
  state.lastYankWasLinewise = wasLinewise;
  switchMode("normal");
};

globalThis.vi_vis_change = function (): void {
  editor.executeAction("cut");
  switchMode("insert");
};

globalThis.vi_vis_yank = function (): void {
  const wasLinewise = state.mode === "visual-line";
  editor.executeAction("copy");
  state.lastYankWasLinewise = wasLinewise;
  // Move cursor to start of selection (vim behavior)
  editor.executeAction("move_left");
  switchMode("normal");
};

// Exit visual mode without doing anything
globalThis.vi_vis_escape = function (): void {
  switchMode("normal");
};

// ============================================================================
// Text Objects (iw, aw, i", a", etc.)
// ============================================================================

// Enter text-object mode with "inner" modifier
globalThis.vi_text_object_inner = function (): void {
  state.pendingTextObject = "inner";
  state.mode = "text-object";
  editor.setEditorMode("vi-text-object");
  editor.setStatus(getModeIndicator("text-object"));
};

// Enter text-object mode with "around" modifier
globalThis.vi_text_object_around = function (): void {
  state.pendingTextObject = "around";
  state.mode = "text-object";
  editor.setEditorMode("vi-text-object");
  editor.setStatus(getModeIndicator("text-object"));
};

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
globalThis.vi_to_word = async function (): Promise<void> { await applyTextObject("word"); };
globalThis.vi_to_WORD = async function (): Promise<void> { await applyTextObject("WORD"); };
globalThis.vi_to_dquote = async function (): Promise<void> { await applyTextObject("\""); };
globalThis.vi_to_squote = async function (): Promise<void> { await applyTextObject("'"); };
globalThis.vi_to_backtick = async function (): Promise<void> { await applyTextObject("`"); };
globalThis.vi_to_paren = async function (): Promise<void> { await applyTextObject("("); };
globalThis.vi_to_brace = async function (): Promise<void> { await applyTextObject("{"); };
globalThis.vi_to_bracket = async function (): Promise<void> { await applyTextObject("["); };
globalThis.vi_to_angle = async function (): Promise<void> { await applyTextObject("<"); };

// Cancel text object mode
globalThis.vi_to_cancel = function (): void {
  switchMode("normal");
};

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
globalThis.vi_find_char_handler = async function (char: string): Promise<void> {
  if (state.pendingFindChar) {
    await executeFindChar(state.pendingFindChar, char);
  }
  // Return to normal mode
  state.pendingFindChar = null;
  switchMode("normal");
};

// Commands to enter find-char mode
globalThis.vi_find_char_f = function (): void {
  enterFindCharMode("f");
};

globalThis.vi_find_char_t = function (): void {
  enterFindCharMode("t");
};

globalThis.vi_find_char_F = function (): void {
  enterFindCharMode("F");
};

globalThis.vi_find_char_T = function (): void {
  enterFindCharMode("T");
};

// Repeat last find char (async)
globalThis.vi_find_char_repeat = async function (): Promise<void> {
  if (state.lastFindChar) {
    await executeFindChar(state.lastFindChar.type, state.lastFindChar.char);
  }
};

// Repeat last find char in opposite direction (async)
globalThis.vi_find_char_repeat_reverse = async function (): Promise<void> {
  if (state.lastFindChar) {
    const reversedType: FindCharType =
      state.lastFindChar.type === "f" ? "F" :
      state.lastFindChar.type === "F" ? "f" :
      state.lastFindChar.type === "t" ? "T" : "t";
    await executeFindChar(reversedType, state.lastFindChar.char);
  }
};

// Cancel find-char mode
globalThis.vi_find_char_cancel = function (): void {
  state.pendingFindChar = null;
  switchMode("normal");
};

// ============================================================================
// Operator-Pending Mode Commands
// ============================================================================

globalThis.vi_op_left = function (): void {
  handleMotionWithOperator("move_left");
};

globalThis.vi_op_down = function (): void {
  handleMotionWithOperator("move_down");
};

globalThis.vi_op_up = function (): void {
  handleMotionWithOperator("move_up");
};

globalThis.vi_op_right = function (): void {
  handleMotionWithOperator("move_right");
};

globalThis.vi_op_word = function (): void {
  handleMotionWithOperator("move_word_right");
};

globalThis.vi_op_word_back = function (): void {
  handleMotionWithOperator("move_word_left");
};

globalThis.vi_op_line_start = function (): void {
  handleMotionWithOperator("move_line_start");
};

globalThis.vi_op_line_end = function (): void {
  handleMotionWithOperator("move_line_end");
};

globalThis.vi_op_doc_start = function (): void {
  handleMotionWithOperator("move_document_start");
};

globalThis.vi_op_doc_end = function (): void {
  handleMotionWithOperator("move_document_end");
};

globalThis.vi_op_matching_bracket = function (): void {
  handleMotionWithOperator("go_to_matching_bracket");
};

globalThis.vi_cancel = function (): void {
  switchMode("normal");
};

// ============================================================================
// Mode Definitions
// ============================================================================

// Define vi-normal mode
editor.defineMode("vi-normal", null, [
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

  // Command mode
  [":", "vi_command_mode"],
], true); // read_only = true to prevent character insertion

// Define vi-insert mode - only Escape is special, other keys insert text
editor.defineMode("vi-insert", null, [
  ["Escape", "vi_escape"],
], false); // read_only = false to allow normal typing

// Define vi-find-char mode - binds all printable chars to the handler
// This mode waits for a single character input for f/t/F/T motions

// Explicitly define handlers for each character to ensure they're accessible
// These return Promises so the runtime can await them
globalThis.vi_fc_a = async function(): Promise<void> { return globalThis.vi_find_char_handler("a"); };
globalThis.vi_fc_b = async function(): Promise<void> { return globalThis.vi_find_char_handler("b"); };
globalThis.vi_fc_c = async function(): Promise<void> { return globalThis.vi_find_char_handler("c"); };
globalThis.vi_fc_d = async function(): Promise<void> { return globalThis.vi_find_char_handler("d"); };
globalThis.vi_fc_e = async function(): Promise<void> { return globalThis.vi_find_char_handler("e"); };
globalThis.vi_fc_f = async function(): Promise<void> { return globalThis.vi_find_char_handler("f"); };
globalThis.vi_fc_g = async function(): Promise<void> { return globalThis.vi_find_char_handler("g"); };
globalThis.vi_fc_h = async function(): Promise<void> { return globalThis.vi_find_char_handler("h"); };
globalThis.vi_fc_i = async function(): Promise<void> { return globalThis.vi_find_char_handler("i"); };
globalThis.vi_fc_j = async function(): Promise<void> { return globalThis.vi_find_char_handler("j"); };
globalThis.vi_fc_k = async function(): Promise<void> { return globalThis.vi_find_char_handler("k"); };
globalThis.vi_fc_l = async function(): Promise<void> { return globalThis.vi_find_char_handler("l"); };
globalThis.vi_fc_m = async function(): Promise<void> { return globalThis.vi_find_char_handler("m"); };
globalThis.vi_fc_n = async function(): Promise<void> { return globalThis.vi_find_char_handler("n"); };
globalThis.vi_fc_o = async function(): Promise<void> { return globalThis.vi_find_char_handler("o"); };
globalThis.vi_fc_p = async function(): Promise<void> { return globalThis.vi_find_char_handler("p"); };
globalThis.vi_fc_q = async function(): Promise<void> { return globalThis.vi_find_char_handler("q"); };
globalThis.vi_fc_r = async function(): Promise<void> { return globalThis.vi_find_char_handler("r"); };
globalThis.vi_fc_s = async function(): Promise<void> { return globalThis.vi_find_char_handler("s"); };
globalThis.vi_fc_t = async function(): Promise<void> { return globalThis.vi_find_char_handler("t"); };
globalThis.vi_fc_u = async function(): Promise<void> { return globalThis.vi_find_char_handler("u"); };
globalThis.vi_fc_v = async function(): Promise<void> { return globalThis.vi_find_char_handler("v"); };
globalThis.vi_fc_w = async function(): Promise<void> { return globalThis.vi_find_char_handler("w"); };
globalThis.vi_fc_x = async function(): Promise<void> { return globalThis.vi_find_char_handler("x"); };
globalThis.vi_fc_y = async function(): Promise<void> { return globalThis.vi_find_char_handler("y"); };
globalThis.vi_fc_z = async function(): Promise<void> { return globalThis.vi_find_char_handler("z"); };
globalThis.vi_fc_A = async function(): Promise<void> { return globalThis.vi_find_char_handler("A"); };
globalThis.vi_fc_B = async function(): Promise<void> { return globalThis.vi_find_char_handler("B"); };
globalThis.vi_fc_C = async function(): Promise<void> { return globalThis.vi_find_char_handler("C"); };
globalThis.vi_fc_D = async function(): Promise<void> { return globalThis.vi_find_char_handler("D"); };
globalThis.vi_fc_E = async function(): Promise<void> { return globalThis.vi_find_char_handler("E"); };
globalThis.vi_fc_F = async function(): Promise<void> { return globalThis.vi_find_char_handler("F"); };
globalThis.vi_fc_G = async function(): Promise<void> { return globalThis.vi_find_char_handler("G"); };
globalThis.vi_fc_H = async function(): Promise<void> { return globalThis.vi_find_char_handler("H"); };
globalThis.vi_fc_I = async function(): Promise<void> { return globalThis.vi_find_char_handler("I"); };
globalThis.vi_fc_J = async function(): Promise<void> { return globalThis.vi_find_char_handler("J"); };
globalThis.vi_fc_K = async function(): Promise<void> { return globalThis.vi_find_char_handler("K"); };
globalThis.vi_fc_L = async function(): Promise<void> { return globalThis.vi_find_char_handler("L"); };
globalThis.vi_fc_M = async function(): Promise<void> { return globalThis.vi_find_char_handler("M"); };
globalThis.vi_fc_N = async function(): Promise<void> { return globalThis.vi_find_char_handler("N"); };
globalThis.vi_fc_O = async function(): Promise<void> { return globalThis.vi_find_char_handler("O"); };
globalThis.vi_fc_P = async function(): Promise<void> { return globalThis.vi_find_char_handler("P"); };
globalThis.vi_fc_Q = async function(): Promise<void> { return globalThis.vi_find_char_handler("Q"); };
globalThis.vi_fc_R = async function(): Promise<void> { return globalThis.vi_find_char_handler("R"); };
globalThis.vi_fc_S = async function(): Promise<void> { return globalThis.vi_find_char_handler("S"); };
globalThis.vi_fc_T = async function(): Promise<void> { return globalThis.vi_find_char_handler("T"); };
globalThis.vi_fc_U = async function(): Promise<void> { return globalThis.vi_find_char_handler("U"); };
globalThis.vi_fc_V = async function(): Promise<void> { return globalThis.vi_find_char_handler("V"); };
globalThis.vi_fc_W = async function(): Promise<void> { return globalThis.vi_find_char_handler("W"); };
globalThis.vi_fc_X = async function(): Promise<void> { return globalThis.vi_find_char_handler("X"); };
globalThis.vi_fc_Y = async function(): Promise<void> { return globalThis.vi_find_char_handler("Y"); };
globalThis.vi_fc_Z = async function(): Promise<void> { return globalThis.vi_find_char_handler("Z"); };
globalThis.vi_fc_0 = async function(): Promise<void> { return globalThis.vi_find_char_handler("0"); };
globalThis.vi_fc_1 = async function(): Promise<void> { return globalThis.vi_find_char_handler("1"); };
globalThis.vi_fc_2 = async function(): Promise<void> { return globalThis.vi_find_char_handler("2"); };
globalThis.vi_fc_3 = async function(): Promise<void> { return globalThis.vi_find_char_handler("3"); };
globalThis.vi_fc_4 = async function(): Promise<void> { return globalThis.vi_find_char_handler("4"); };
globalThis.vi_fc_5 = async function(): Promise<void> { return globalThis.vi_find_char_handler("5"); };
globalThis.vi_fc_6 = async function(): Promise<void> { return globalThis.vi_find_char_handler("6"); };
globalThis.vi_fc_7 = async function(): Promise<void> { return globalThis.vi_find_char_handler("7"); };
globalThis.vi_fc_8 = async function(): Promise<void> { return globalThis.vi_find_char_handler("8"); };
globalThis.vi_fc_9 = async function(): Promise<void> { return globalThis.vi_find_char_handler("9"); };
globalThis.vi_fc_space = async function(): Promise<void> { return globalThis.vi_find_char_handler(" "); };

// Define vi-find-char mode with all the character bindings
editor.defineMode("vi-find-char", null, [
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
  // Common punctuation
  ["Space", "vi_fc_space"],
], true);

// Define vi-operator-pending mode
editor.defineMode("vi-operator-pending", null, [
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
editor.defineMode("vi-text-object", null, [
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
editor.defineMode("vi-visual", null, [
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

  // Switch to line mode
  ["V", "vi_visual_toggle_line"],

  // Operators
  ["d", "vi_vis_delete"],
  ["x", "vi_vis_delete"],
  ["c", "vi_vis_change"],
  ["s", "vi_vis_change"],
  ["y", "vi_vis_yank"],

  // Exit
  ["Escape", "vi_vis_escape"],
  ["v", "vi_vis_escape"], // v again exits visual mode
], true);

// Define vi-visual-line mode (line-wise)
editor.defineMode("vi-visual-line", null, [
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

  // Switch to char mode
  ["v", "vi_visual_toggle_line"],

  // Operators
  ["d", "vi_vis_delete"],
  ["x", "vi_vis_delete"],
  ["c", "vi_vis_change"],
  ["s", "vi_vis_change"],
  ["y", "vi_vis_yank"],

  // Exit
  ["Escape", "vi_vis_escape"],
  ["V", "vi_vis_escape"], // V again exits visual-line mode
], true);

// Define vi-visual-block mode (column/block selection)
editor.defineMode("vi-visual-block", null, [
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
globalThis.vi_command_mode = function (): void {
  editor.startPrompt(":", "vi-command");
};

// Handle command execution when user presses Enter
globalThis.vi_command_handler = async function (args: { prompt_type: string; input: string }): Promise<boolean> {
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
};

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
      // :w filename - save as filename (not implemented yet)
      if (args) {
        return { error: editor.t("error.save_as_not_implemented") };
      }
      editor.executeAction("save");
      return { message: editor.t("status.file_saved") };
    }

    case "quit": {
      // :q - quit (close buffer)
      // :q! - force quit (discard changes)
      const bufferId = editor.getActiveBufferId();
      if (!force && editor.isBufferModified(bufferId)) {
        return { error: editor.t("error.no_write_since_change", { cmd: ":q!" }) };
      }
      editor.executeAction("close_buffer");
      return {};
    }

    case "wq":
    case "xit":
    case "exit": {
      // :wq or :x - save and quit
      editor.executeAction("save");
      editor.executeAction("close_buffer");
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
        editor.executeAction("quit_all");
      } else {
        // Check if any buffer is modified
        const buffers = editor.listBuffers();
        for (const buf of buffers) {
          if (buf.modified) {
            return { error: editor.t("error.no_write_since_change", { cmd: ":qa!" }) };
          }
        }
        editor.executeAction("quit_all");
      }
      return {};
    }

    case "wqall": {
      // :wqa or :xa - save all and quit
      editor.executeAction("save_all");
      editor.executeAction("quit_all");
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
      editor.executeAction("close_buffer");
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
      editor.executeAction("close_buffer");
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
      editor.executeAction("close_buffer");
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

globalThis.vi_mode_toggle = function (): void {
  viModeEnabled = !viModeEnabled;

  if (viModeEnabled) {
    switchMode("normal");
    editor.setStatus(editor.t("status.enabled"));
  } else {
    editor.setEditorMode(null);
    state.mode = "normal";
    state.pendingOperator = null;
    editor.setStatus(editor.t("status.disabled"));
  }
};

editor.registerCommand(
  "%cmd.toggle_vi_mode",
  "%cmd.toggle_vi_mode_desc",
  "vi_mode_toggle",
  "normal",
);

// ============================================================================
// Initialization
// ============================================================================

editor.setStatus(editor.t("status.loaded"));
