/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 1. Prompt line — `await editor.prompt(label, initial)`.
// Opens the one-line prompt at the bottom of the screen and resolves with
// the typed text, or null if the user pressed Esc.

registerHandler("ask_prompt_line", async () => {
  const value = await editor.prompt("Issue number:", "");
  if (value === null) return;
  editor.setStatus(`You entered: ${value}`);
});

editor.registerCommand("Ask: Prompt Line", "Ask via the bottom prompt line", "ask_prompt_line");
