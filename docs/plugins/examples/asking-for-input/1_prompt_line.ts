/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 1. Prompt line
// Asks for text on the bottom line.
// Returns what the user typed, or null if they pressed Esc.

registerHandler("ask_prompt_line", async () => {
  const value = await editor.prompt("Issue number:", "");
  if (value === null) return;
  editor.setStatus(`You entered: ${value}`);
});

editor.registerCommand("Ask: Prompt Line", "Ask via the bottom prompt line", "ask_prompt_line");
