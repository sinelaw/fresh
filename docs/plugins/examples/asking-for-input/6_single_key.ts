/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 6. Single keypress — `await editor.getNextKey()`.
// No prompt line at all: show a hint, take the very next key.
// Good for "pick 1-9" or y/n where Enter would be one key too many.

registerHandler("ask_single_key", async () => {
  editor.setStatus("Level? 1-6");
  const k = await editor.getNextKey();
  if (!/^[1-6]$/.test(k.key)) { editor.setStatus("Cancelled"); return; }
  editor.setStatus(`You pressed: ${k.key}`);
});

editor.registerCommand("Ask: Single Key", "Read one keypress", "ask_single_key");
