/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 7. File picker
// Lets the user choose a file. Returns its path, or null if cancelled.
// The second argument is the folder to start in.

registerHandler("ask_file", async () => {
  const path = await editor.pickFile("Fixture file:", "tests/fixtures");
  if (path === null) return;
  editor.setStatus(`You picked: ${path}`);
});

editor.registerCommand("Ask: File Picker", "Ask the user to choose a file", "ask_file");
