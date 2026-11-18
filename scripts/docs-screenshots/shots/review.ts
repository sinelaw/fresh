// review.ts — the file under review on the left, its test beside it,
// this terminal along the bottom. Re-runnable: it tidies up first.
const shell = editor.describeWorkspace().panes.find((p) => p.kind === "terminal");
for (const pane of editor.describeWorkspace().panes) {
  if (pane.splitId !== shell.splitId) editor.closeSplit(pane.splitId);
}
await editor.flush();

await editor.splitWindow({
  direction: "horizontal",
  place: "before",
  ratio: 0.7,
  file: "crates/fresh-editor/src/server/command_access.rs",
});
await editor.splitWindow({
  direction: "vertical",
  place: "after",
  file: "crates/fresh-editor/tests/terminal_restore_script_token.rs",
});

// Hand the keyboard back to the pane you are typing in.
editor.focusSplit(shell.splitId);
await editor.flush();
return editor.describeWorkspace().panes.map((p) => p.name);
