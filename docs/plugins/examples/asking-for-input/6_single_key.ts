/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 6. Single key
// Shows a hint, then waits for one key. The user doesn't press Enter.

registerHandler("ask_single_key", async () => {
  editor.setStatus("Level? 1-6");
  const k = await editor.getNextKey();
  if (!/^[1-6]$/.test(k.key)) { editor.setStatus("Cancelled"); return; }
  editor.setStatus(`You pressed: ${k.key}`);
});

editor.registerCommand("Ask: Single Key", "Read one keypress", "ask_single_key");
