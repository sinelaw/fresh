/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 7. File picker — `await editor.pickFile(label, dir?)`.
// Opens Fresh's own Open File browser and resolves with the chosen path
// (or null). Nothing is opened; you just get the path back. The optional
// second argument picks the starting directory.

registerHandler("ask_file", async () => {
  const path = await editor.pickFile("Fixture file:", "tests/fixtures");
  if (path === null) return;
  editor.setStatus(`You picked: ${path}`);
});

editor.registerCommand("Ask: File Picker", "Ask the user to choose a file", "ask_file");
