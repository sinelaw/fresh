/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// Alternative 5 — live preview while you type.
//
// Use the event-driven prompt API: `prompt_changed` fires on every
// keystroke, so we render the would-be boilerplate as virtual lines
// (not real text) at the cursor. Enter inserts it for real; Esc clears it.
// (Note: virtual lines don't show on the very last, empty line of a file.)

const PROMPT = "boilerplate-live";
const NS = "boilerplate-preview";
const render = (n: string) =>
  `\n#[test]\nfn regression_issue_${n || "?"}() {\n    // https://github.com/acme/app/issues/${n || "?"}\n}\n`;

let target: { buf: number; pos: number } | null = null;

function showPreview(n: string) {
  if (!target) return;
  editor.clearVirtualTextNamespace(target.buf, NS);
  render(n).trimEnd().split("\n").forEach((line, i) => {
    // `above: true` stacks the lines just above the cursor; priority keeps them in order.
    editor.addVirtualLine(target!.buf, target!.pos, line, { fg: "ui.help_key_fg", italic: true }, true, NS, i);
  });
}

registerHandler("insert_regression_test", () => {
  target = { buf: editor.getActiveBufferId(), pos: editor.getCursorPosition() };
  editor.startPrompt("Issue number (live preview):", PROMPT);
  showPreview("");
});

editor.on("prompt_changed", (a) => {
  if (a.prompt_type === PROMPT) showPreview(a.input);
  return true;
});

editor.on("prompt_confirmed", (a) => {
  if (a.prompt_type !== PROMPT || !target) return true;
  editor.clearVirtualTextNamespace(target.buf, NS);
  editor.insertText(target.buf, target.pos, render(a.input));
  target = null;
  return true;
});

editor.on("prompt_cancelled", (a) => {
  if (a.prompt_type === PROMPT && target) editor.clearVirtualTextNamespace(target.buf, NS);
  target = null;
  return true;
});

editor.registerCommand("Insert: Regression Test", "Insert a regression test with a live preview", "insert_regression_test");
