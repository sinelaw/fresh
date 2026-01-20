/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Calculator Plugin for Fresh Editor
 *
 * A sleek visual calculator with:
 * - Mouse-clickable buttons (anywhere in button area)
 * - Keyboard input support
 * - Expression parsing with parentheses and basic arithmetic
 * - Modern calculator styling with ANSI colors
 * - Compact fixed-size layout centered in view
 */

// ANSI color codes
const C = {
  RESET: "\x1b[0m",
  BOLD: "\x1b[1m",
  DIM: "\x1b[2m",
  // Colors
  RED: "\x1b[31m",
  GREEN: "\x1b[32m",
  YELLOW: "\x1b[33m",
  BLUE: "\x1b[34m",
  MAGENTA: "\x1b[35m",
  CYAN: "\x1b[36m",
  WHITE: "\x1b[37m",
  BRIGHT_RED: "\x1b[91m",
  BRIGHT_GREEN: "\x1b[92m",
  BRIGHT_YELLOW: "\x1b[93m",
  BRIGHT_BLUE: "\x1b[94m",
  BRIGHT_MAGENTA: "\x1b[95m",
  BRIGHT_CYAN: "\x1b[96m",
  // Backgrounds
  BG_BLACK: "\x1b[40m",
  BG_RED: "\x1b[41m",
  BG_GREEN: "\x1b[42m",
  BG_YELLOW: "\x1b[43m",
  BG_BLUE: "\x1b[44m",
  BG_MAGENTA: "\x1b[45m",
  BG_CYAN: "\x1b[46m",
  BG_WHITE: "\x1b[47m",
  BG_BRIGHT_BLACK: "\x1b[100m",
};

// Calculator state
interface CalculatorState {
  expression: string;
  result: string;
  error: string;
  bufferId: number;
  splitId: number;
  lastViewport: ViewportInfo | null;
}

const state: CalculatorState = {
  expression: "",
  result: "0",
  error: "",
  bufferId: 0,
  splitId: 0,
  lastViewport: null,
};

// Cache the layout so it doesn't jump around
let cachedLayout: LayoutMetrics | null = null;

// Track hovered button for visual feedback
let hoveredButton: { row: number; col: number } | null = null;

// Track if copy button is hovered
let copyButtonHovered = false;

// Button definitions
interface Button {
  label: string;
  action: string;
  type: "number" | "operator" | "function" | "clear" | "equals";
}

const BUTTON_LAYOUT: Button[][] = [
  [
    { label: "C", action: "clear", type: "clear" },
    { label: "(", action: "(", type: "function" },
    { label: ")", action: ")", type: "function" },
    { label: "^", action: "^", type: "operator" },
    { label: "÷", action: "/", type: "operator" },
  ],
  [
    { label: "sqrt", action: "sqrt(", type: "function" },
    { label: "ln", action: "ln(", type: "function" },
    { label: "log", action: "log(", type: "function" },
    { label: "π", action: "pi", type: "number" },
    { label: "×", action: "*", type: "operator" },
  ],
  [
    { label: "sin", action: "sin(", type: "function" },
    { label: "cos", action: "cos(", type: "function" },
    { label: "tan", action: "tan(", type: "function" },
    { label: "e", action: "e", type: "number" },
    { label: "-", action: "-", type: "operator" },
  ],
  [
    { label: "7", action: "7", type: "number" },
    { label: "8", action: "8", type: "number" },
    { label: "9", action: "9", type: "number" },
    { label: "⌫", action: "backspace", type: "clear" },
    { label: "+", action: "+", type: "operator" },
  ],
  [
    { label: "4", action: "4", type: "number" },
    { label: "5", action: "5", type: "number" },
    { label: "6", action: "6", type: "number" },
    { label: "±", action: "negate", type: "function" },
    { label: "=", action: "equals", type: "equals" },
  ],
  [
    { label: "1", action: "1", type: "number" },
    { label: "2", action: "2", type: "number" },
    { label: "3", action: "3", type: "number" },
    { label: "0", action: "0", type: "number" },
    { label: ".", action: ".", type: "number" },
  ],
];

// Fixed layout constants
const BUTTON_WIDTH = 5;
const NUM_COLS = 5;
const NUM_ROWS = 6;
const CALC_WIDTH = BUTTON_WIDTH * NUM_COLS + 1; // 26 chars
const DISPLAY_LINES = 2;

// Get color for button type (with optional hover highlight)
function getButtonColor(type: Button["type"], isHovered: boolean): string {
  if (isHovered) {
    // Bright/inverted colors for hover
    return C.BG_WHITE + "\x1b[30m"; // White background, black text
  }
  switch (type) {
    case "number": return C.WHITE;
    case "operator": return C.BRIGHT_YELLOW;
    case "function": return C.BRIGHT_CYAN;
    case "clear": return C.BRIGHT_RED;
    case "equals": return C.BRIGHT_GREEN;
    default: return C.WHITE;
  }
}

// Layout metrics
interface LayoutMetrics {
  startX: number;
  startY: number;
}

function calculateLayout(_viewport: ViewportInfo): LayoutMetrics {
  // Position at top-left with 1 row/column gap
  const startX = 1;
  const startY = 1;

  return { startX, startY };
}

// Render the calculator with ANSI colors
function renderCalculator(): TextPropertyEntry[] {
  const viewport = editor.getViewport();
  if (!viewport) {
    return [{ text: "No viewport\n", properties: {} }];
  }

  state.lastViewport = viewport;

  // Use cached layout to prevent jumping, or calculate new one
  if (!cachedLayout) {
    cachedLayout = calculateLayout(viewport);
  }
  const layout = cachedLayout;
  const entries: TextPropertyEntry[] = [];

  const addLine = (text: string): void => {
    entries.push({ text: text + "\n", properties: {} });
  };

  // Top margin
  for (let i = 0; i < layout.startY; i++) {
    addLine("");
  }

  const pad = " ".repeat(layout.startX);

  // Unicode box drawing chars
  const TL = "╭", TR = "╮", BL = "╰", BR = "╯";
  const V = "│";
  const LT = "├", RT = "┤", X = "┼";

  // Generate border patterns dynamically
  const cellWidth = BUTTON_WIDTH - 1; // 4 dashes per cell
  const topBorder = TL + "─".repeat(CALC_WIDTH - 2) + TR;
  const sepTop = LT + Array(NUM_COLS).fill("─".repeat(cellWidth)).join("┬") + RT;
  const sepMid = LT + Array(NUM_COLS).fill("─".repeat(cellWidth)).join("┼") + RT;
  const sepBot = BL + Array(NUM_COLS).fill("─".repeat(cellWidth)).join("┴") + BR;

  // Display - top border
  addLine(`${pad}${C.CYAN}${topBorder}${C.RESET}`);

  // Expression line
  let expr = state.expression || "";
  const maxLen = CALC_WIDTH - 4;
  if (expr.length > maxLen) expr = expr.slice(-maxLen);
  addLine(`${pad}${C.CYAN}${V}${C.RESET} ${C.BRIGHT_GREEN}${expr.padStart(maxLen)}${C.RESET} ${C.CYAN}${V}${C.RESET}`);

  // Result line with copy button on left - slightly different background
  let result = state.error || state.result;
  const copyBtnWidth = 6; // "Copy" + 2 spaces
  const resultMaxLen = maxLen - copyBtnWidth;
  if (result.length > resultMaxLen) result = result.slice(0, resultMaxLen);
  const resultColor = state.error ? C.BRIGHT_RED : C.BRIGHT_GREEN;
  const copyBtnColor = copyButtonHovered ? (C.BG_WHITE + "\x1b[30m") : (C.BG_BRIGHT_BLACK + C.BRIGHT_MAGENTA);
  const resultBg = C.BG_BRIGHT_BLACK;
  addLine(`${pad}${C.CYAN}${V}${C.RESET}${copyBtnColor}Copy${C.RESET}${resultBg}  ${C.BOLD}${resultColor}${result.padStart(resultMaxLen)}${C.RESET}${resultBg}  ${C.RESET}${C.CYAN}${V}${C.RESET}`);

  // Separator between display and buttons
  addLine(`${pad}${C.CYAN}${sepTop}${C.RESET}`);

  // Button rows
  for (let rowIdx = 0; rowIdx < BUTTON_LAYOUT.length; rowIdx++) {
    const buttonRow = BUTTON_LAYOUT[rowIdx];
    let line = `${pad}${C.CYAN}${V}${C.RESET}`;

    for (let colIdx = 0; colIdx < buttonRow.length; colIdx++) {
      const btn = buttonRow[colIdx];
      const isHovered = hoveredButton?.row === rowIdx && hoveredButton?.col === colIdx;
      const color = getButtonColor(btn.type, isHovered);
      const label = btn.label;
      const innerWidth = BUTTON_WIDTH - 1;
      const leftSpace = Math.floor((innerWidth - label.length) / 2);
      const rightSpace = innerWidth - label.length - leftSpace;
      line += `${color}${C.BOLD}${" ".repeat(leftSpace)}${label}${" ".repeat(rightSpace)}${C.RESET}${C.CYAN}${V}${C.RESET}`;
    }

    addLine(line);

    // Row separator (except after last row)
    if (rowIdx < BUTTON_LAYOUT.length - 1) {
      addLine(`${pad}${C.CYAN}${sepMid}${C.RESET}`);
    }
  }

  // Bottom border
  addLine(`${pad}${C.CYAN}${sepBot}${C.RESET}`);

  // Help line
  addLine("");
  addLine(`${pad}${C.DIM}  Esc:close  =/Enter:calc  Del:clear${C.RESET}`);

  return entries;
}

// Check if click is on copy button (returns true if on copy button)
function isCopyButtonAt(contentCol: number, contentRow: number): boolean {
  if (!cachedLayout) return false;

  // Copy button is on result line (row 2 after top margin)
  const resultLineY = cachedLayout.startY + 2; // top border + expression line
  const copyBtnStartX = cachedLayout.startX + 1; // after left border
  const copyBtnEndX = copyBtnStartX + 4; // "Copy" is 4 chars

  return contentRow === resultLineY &&
         contentCol >= copyBtnStartX &&
         contentCol < copyBtnEndX;
}

// Copy result to clipboard
function copyResultToClipboard(): void {
  const textToCopy = state.error || state.result;
  editor.copyToClipboard(textToCopy);
  editor.setStatus(editor.t("status.copied", { value: textToCopy }));
}

// Get button position at content-relative coordinates
function getButtonPosition(contentCol: number, contentRow: number): { row: number; col: number } | null {
  if (!cachedLayout) return null;

  // Button area starts after: marginY + display(2 lines) + borders(2)
  const buttonAreaStartY = cachedLayout.startY + DISPLAY_LINES + 2;
  const buttonAreaStartX = cachedLayout.startX + 1; // +1 for left border

  const relY = contentRow - buttonAreaStartY;
  const relX = contentCol - buttonAreaStartX;

  if (relX < 0 || relY < 0) return null;
  if (relX >= BUTTON_WIDTH * NUM_COLS) return null;

  // Check if on horizontal separator line (odd rows are separators)
  if (relY % 2 === 1) return null;

  // Check if on vertical border (every BUTTON_WIDTH chars, minus 1 for the separator)
  const posInButton = relX % BUTTON_WIDTH;
  if (posInButton === BUTTON_WIDTH - 1) return null; // On the | border

  // Each button row = 2 lines (content + separator)
  const buttonRowIdx = Math.floor(relY / 2);
  if (buttonRowIdx < 0 || buttonRowIdx >= NUM_ROWS) return null;

  // Column
  const buttonColIdx = Math.floor(relX / BUTTON_WIDTH);
  if (buttonColIdx < 0 || buttonColIdx >= NUM_COLS) return null;

  return { row: buttonRowIdx, col: buttonColIdx };
}

// Get button at content-relative position
function getButtonAt(contentCol: number, contentRow: number): Button | null {
  const pos = getButtonPosition(contentCol, contentRow);
  if (!pos) return null;
  return BUTTON_LAYOUT[pos.row][pos.col];
}

// Expression parser
interface Token {
  type: "number" | "operator" | "lparen" | "rparen" | "function" | "constant";
  value: string | number;
}

// Known functions and constants
const FUNCTIONS = ["sqrt", "ln", "log", "sin", "cos", "tan", "asin", "acos", "atan", "abs"];
const CONSTANTS: Record<string, number> = {
  pi: Math.PI,
  e: Math.E,
};

function tokenize(expr: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  while (i < expr.length) {
    const ch = expr[i];

    if (/\s/.test(ch)) { i++; continue; }

    // Numbers
    if (/[0-9.]/.test(ch)) {
      let num = "";
      while (i < expr.length && /[0-9.]/.test(expr[i])) {
        num += expr[i];
        i++;
      }
      tokens.push({ type: "number", value: parseFloat(num) });
      continue;
    }

    // Identifiers (functions and constants)
    if (/[a-zA-Z]/.test(ch)) {
      let ident = "";
      while (i < expr.length && /[a-zA-Z0-9]/.test(expr[i])) {
        ident += expr[i];
        i++;
      }
      if (FUNCTIONS.includes(ident)) {
        tokens.push({ type: "function", value: ident });
      } else if (ident in CONSTANTS) {
        tokens.push({ type: "constant", value: ident });
      } else {
        throw new Error(`Unknown: ${ident}`);
      }
      continue;
    }

    if (ch === "(") { tokens.push({ type: "lparen", value: "(" }); i++; continue; }
    if (ch === ")") { tokens.push({ type: "rparen", value: ")" }); i++; continue; }
    if (/[+\-*/^]/.test(ch)) { tokens.push({ type: "operator", value: ch }); i++; continue; }

    i++;
  }

  return tokens;
}

// Precedence: + - < * / < ^ < unary - < functions
function parseExpression(tokens: Token[], pos: { idx: number }): number {
  let left = parseTerm(tokens, pos);

  while (pos.idx < tokens.length) {
    const token = tokens[pos.idx];
    if (token.type === "operator" && (token.value === "+" || token.value === "-")) {
      pos.idx++;
      const right = parseTerm(tokens, pos);
      left = token.value === "+" ? left + right : left - right;
    } else {
      break;
    }
  }

  return left;
}

function parseTerm(tokens: Token[], pos: { idx: number }): number {
  let left = parsePower(tokens, pos);

  while (pos.idx < tokens.length) {
    const token = tokens[pos.idx];
    if (token.type === "operator" && (token.value === "*" || token.value === "/")) {
      pos.idx++;
      const right = parsePower(tokens, pos);
      if (token.value === "*") {
        left = left * right;
      } else {
        if (right === 0) throw new Error("Div by 0");
        left = left / right;
      }
    } else {
      break;
    }
  }

  return left;
}

function parsePower(tokens: Token[], pos: { idx: number }): number {
  const base = parseUnary(tokens, pos);

  if (pos.idx < tokens.length && tokens[pos.idx].type === "operator" && tokens[pos.idx].value === "^") {
    pos.idx++;
    const exp = parsePower(tokens, pos); // Right associative
    return Math.pow(base, exp);
  }

  return base;
}

function parseUnary(tokens: Token[], pos: { idx: number }): number {
  if (pos.idx >= tokens.length) throw new Error("Unexpected end");

  const token = tokens[pos.idx];

  if (token.type === "operator" && token.value === "-") {
    pos.idx++;
    return -parseUnary(tokens, pos);
  }

  return parsePrimary(tokens, pos);
}

function parsePrimary(tokens: Token[], pos: { idx: number }): number {
  if (pos.idx >= tokens.length) throw new Error("Unexpected end");

  const token = tokens[pos.idx];

  // Function call
  if (token.type === "function") {
    const fname = token.value as string;
    pos.idx++;
    if (pos.idx >= tokens.length || tokens[pos.idx].type !== "lparen") {
      throw new Error(`Expected ( after ${fname}`);
    }
    pos.idx++; // skip (
    const arg = parseExpression(tokens, pos);
    if (pos.idx >= tokens.length || tokens[pos.idx].type !== "rparen") {
      throw new Error("Missing )");
    }
    pos.idx++; // skip )

    switch (fname) {
      case "sqrt": return Math.sqrt(arg);
      case "ln": return Math.log(arg);
      case "log": return Math.log10(arg);
      case "sin": return Math.sin(arg);
      case "cos": return Math.cos(arg);
      case "tan": return Math.tan(arg);
      case "asin": return Math.asin(arg);
      case "acos": return Math.acos(arg);
      case "atan": return Math.atan(arg);
      case "abs": return Math.abs(arg);
      default: throw new Error(`Unknown function: ${fname}`);
    }
  }

  // Constant
  if (token.type === "constant") {
    pos.idx++;
    return CONSTANTS[token.value as string];
  }

  // Number
  if (token.type === "number") {
    pos.idx++;
    return token.value as number;
  }

  // Parenthesized expression
  if (token.type === "lparen") {
    pos.idx++;
    const result = parseExpression(tokens, pos);
    if (pos.idx >= tokens.length || tokens[pos.idx].type !== "rparen") {
      throw new Error("Missing )");
    }
    pos.idx++;
    return result;
  }

  throw new Error("Syntax error");
}

function evaluateExpression(expr: string): string {
  if (!expr.trim()) return "0";

  const tokens = tokenize(expr);
  if (tokens.length === 0) return "0";

  const pos = { idx: 0 };
  const result = parseExpression(tokens, pos);

  if (pos.idx < tokens.length) throw new Error("Syntax error");

  if (Number.isInteger(result)) {
    return result.toString();
  } else {
    return parseFloat(result.toFixed(10)).toString();
  }
}

// Handle button press
function handleButton(button: Button): void {
  state.error = "";

  switch (button.action) {
    case "clear":
      state.expression = "";
      state.result = "0";
      break;
    case "backspace":
      if (state.expression.length > 0) {
        state.expression = state.expression.slice(0, -1);
      }
      break;
    case "negate":
      // Toggle sign: if expression is empty, negate last result; otherwise toggle current number
      if (state.expression === "") {
        // Use negated result as new expression
        if (state.result !== "0") {
          const num = parseFloat(state.result);
          state.expression = (-num).toString();
          state.result = state.expression;
        } else {
          state.expression = "-";
        }
      } else {
        // Try to toggle sign of last number in expression
        const match = state.expression.match(/(-?\d+\.?\d*)$/);
        if (match) {
          const numStr = match[1];
          const prefix = state.expression.slice(0, state.expression.length - numStr.length);
          const num = parseFloat(numStr);
          state.expression = prefix + (-num).toString();
        } else {
          // No number at end, just add minus
          state.expression += "-";
        }
      }
      break;
    case "equals":
      try {
        state.result = evaluateExpression(state.expression);
      } catch (e) {
        state.error = e instanceof Error ? e.message : "Error";
      }
      break;
    default:
      state.expression += button.action;
      break;
  }

  updateDisplay();
}

function updateDisplay(): void {
  if (state.bufferId) {
    const entries = renderCalculator();
    editor.setVirtualBufferContent(state.bufferId, entries);
  }
}

// Mouse click handler
globalThis.onCalculatorMouseClick = function (data: {
  column: number;
  row: number;
  button: string;
  modifiers: string;
  content_x: number;
  content_y: number;
}): boolean {
  if (data.button !== "left") return true;

  const activeBuffer = editor.getActiveBufferId();
  if (activeBuffer !== state.bufferId || state.bufferId === 0) return true;

  // Convert screen coordinates to content-relative coordinates
  const relCol = data.column - data.content_x;
  const relRow = data.row - data.content_y;

  // Check for copy button click
  if (isCopyButtonAt(relCol, relRow)) {
    copyResultToClipboard();
    return false;
  }

  const button = getButtonAt(relCol, relRow);
  if (button) {
    handleButton(button);
    return false;
  }

  return true;
};

// Keyboard handlers
globalThis.calc_digit_0 = function (): void { handleButton({ label: "0", action: "0", type: "number" }); };
globalThis.calc_digit_1 = function (): void { handleButton({ label: "1", action: "1", type: "number" }); };
globalThis.calc_digit_2 = function (): void { handleButton({ label: "2", action: "2", type: "number" }); };
globalThis.calc_digit_3 = function (): void { handleButton({ label: "3", action: "3", type: "number" }); };
globalThis.calc_digit_4 = function (): void { handleButton({ label: "4", action: "4", type: "number" }); };
globalThis.calc_digit_5 = function (): void { handleButton({ label: "5", action: "5", type: "number" }); };
globalThis.calc_digit_6 = function (): void { handleButton({ label: "6", action: "6", type: "number" }); };
globalThis.calc_digit_7 = function (): void { handleButton({ label: "7", action: "7", type: "number" }); };
globalThis.calc_digit_8 = function (): void { handleButton({ label: "8", action: "8", type: "number" }); };
globalThis.calc_digit_9 = function (): void { handleButton({ label: "9", action: "9", type: "number" }); };

globalThis.calc_add = function (): void { handleButton({ label: "+", action: "+", type: "operator" }); };
globalThis.calc_subtract = function (): void { handleButton({ label: "-", action: "-", type: "operator" }); };
globalThis.calc_multiply = function (): void { handleButton({ label: "×", action: "*", type: "operator" }); };
globalThis.calc_divide = function (): void { handleButton({ label: "÷", action: "/", type: "operator" }); };
globalThis.calc_lparen = function (): void { handleButton({ label: "(", action: "(", type: "function" }); };
globalThis.calc_rparen = function (): void { handleButton({ label: ")", action: ")", type: "function" }); };
globalThis.calc_dot = function (): void { handleButton({ label: ".", action: ".", type: "number" }); };
globalThis.calc_equals = function (): void { handleButton({ label: "=", action: "equals", type: "equals" }); };
globalThis.calc_clear = function (): void { handleButton({ label: "C", action: "clear", type: "clear" }); };
globalThis.calc_backspace = function (): void { handleButton({ label: "⌫", action: "backspace", type: "clear" }); };
globalThis.calc_power = function (): void { handleButton({ label: "^", action: "^", type: "operator" }); };

// Letter handlers for typing function names
const letterHandler = (ch: string) => () => {
  state.error = "";
  state.expression += ch;
  updateDisplay();
};
for (const ch of "abcdefghijklmnopqrstuvwxyz") {
  (globalThis as Record<string, unknown>)[`calc_letter_${ch}`] = letterHandler(ch);
}

globalThis.calc_close = function (): void {
  if (state.bufferId) {
    editor.closeBuffer(state.bufferId);
    state.bufferId = 0;
  }
};

// Open calculator
globalThis.calculator_open = async function (): Promise<void> {
  if (state.bufferId) {
    const bufferInfo = editor.getBufferInfo(state.bufferId);
    if (bufferInfo) {
      editor.showBuffer(state.bufferId);
      return;
    }
    state.bufferId = 0;
  }

  state.expression = "";
  state.result = "0";
  state.error = "";
  cachedLayout = null; // Reset layout for fresh calculation
  hoveredButton = null; // Reset hover state
  copyButtonHovered = false; // Reset copy button hover state

  const modeBindings: [string, string][] = [
    ["0", "calc_digit_0"], ["1", "calc_digit_1"], ["2", "calc_digit_2"],
    ["3", "calc_digit_3"], ["4", "calc_digit_4"], ["5", "calc_digit_5"],
    ["6", "calc_digit_6"], ["7", "calc_digit_7"], ["8", "calc_digit_8"],
    ["9", "calc_digit_9"],
    ["+", "calc_add"], ["-", "calc_subtract"], ["*", "calc_multiply"],
    ["/", "calc_divide"], ["(", "calc_lparen"], [")", "calc_rparen"],
    [".", "calc_dot"], ["^", "calc_power"],
    ["Return", "calc_equals"], ["=", "calc_equals"],
    ["Delete", "calc_clear"],
    ["Backspace", "calc_backspace"],
    ["Escape", "calc_close"],
  ];
  // Add letter bindings for typing function names
  for (const ch of "abcdefghijklmnopqrstuvwxyz") {
    modeBindings.push([ch, `calc_letter_${ch}`]);
  }
  editor.defineMode("calculator", "special", modeBindings, true);

  const cmds = [
    ["calc_digit_0", "0"], ["calc_digit_1", "1"], ["calc_digit_2", "2"],
    ["calc_digit_3", "3"], ["calc_digit_4", "4"], ["calc_digit_5", "5"],
    ["calc_digit_6", "6"], ["calc_digit_7", "7"], ["calc_digit_8", "8"],
    ["calc_digit_9", "9"], ["calc_add", "+"], ["calc_subtract", "-"],
    ["calc_multiply", "*"], ["calc_divide", "/"], ["calc_lparen", "("],
    ["calc_rparen", ")"], ["calc_dot", "."], ["calc_equals", "="],
    ["calc_clear", "C"], ["calc_backspace", "BS"], ["calc_close", "close"],
    ["calc_power", "^"],
  ];
  for (const [name, desc] of cmds) {
    editor.registerCommand(name, `Calc: ${desc}`, name, "calculator");
  }
  // Register letter commands
  for (const ch of "abcdefghijklmnopqrstuvwxyz") {
    editor.registerCommand(`calc_letter_${ch}`, `Calc: ${ch}`, `calc_letter_${ch}`, "calculator");
  }

  const entries = renderCalculator();

  const result = await editor.createVirtualBuffer({
    name: "*Calculator*",
    mode: "calculator",
    readOnly: true,
    entries,
    showLineNumbers: false,
    showCursors: false,
    editingDisabled: true,
  });
  state.bufferId = result.bufferId;
  state.splitId = editor.getActiveSplitId();

  editor.setStatus(editor.t("status.opened"));
};

// Mouse move handler for hover effect
globalThis.onCalculatorMouseMove = function (data: {
  column: number;
  row: number;
  content_x: number;
  content_y: number;
}): boolean {
  const activeBuffer = editor.getActiveBufferId();
  if (activeBuffer !== state.bufferId || state.bufferId === 0) return true;

  // Convert screen coordinates to content-relative coordinates
  const relCol = data.column - data.content_x;
  const relRow = data.row - data.content_y;

  const newHover = getButtonPosition(relCol, relRow);
  const newCopyHover = isCopyButtonAt(relCol, relRow);

  // Check if hover changed
  const buttonChanged =
    (newHover === null && hoveredButton !== null) ||
    (newHover !== null && hoveredButton === null) ||
    (newHover !== null && hoveredButton !== null &&
      (newHover.row !== hoveredButton.row || newHover.col !== hoveredButton.col));
  const copyChanged = newCopyHover !== copyButtonHovered;

  if (buttonChanged || copyChanged) {
    hoveredButton = newHover;
    copyButtonHovered = newCopyHover;
    updateDisplay();
  }

  return true;
};

// Register hooks
editor.on("mouse_click", "onCalculatorMouseClick");
editor.on("mouse_move", "onCalculatorMouseMove");

// Register main command
editor.registerCommand("%cmd.calculator", "%cmd.calculator_desc", "calculator_open", null);

