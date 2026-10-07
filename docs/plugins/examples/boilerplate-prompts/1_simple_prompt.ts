/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// Alternative 1 — the simplest thing: ask for one value, then insert.
//
// `editor.prompt(label, initial)` opens the prompt line and resolves with
// what the user typed (or null on Esc). Build the boilerplate with a
// template string and insert it at the cursor — no hand-editing afterwards.

registerHandler("insert_test_case", async () => {
  const n = await editor.prompt("Test case number:", "");
  if (n === null || n.trim() === "") return; // Esc / empty -> do nothing

  editor.insertAtCursor(
`// ------------------------------------------------------------
// Test case ${n}
// ------------------------------------------------------------
#[test]
fn test_case_${n}() {
    let input = load_fixture("case_${n}.json");
    assert_eq!(run(input), expected(${n}));
}
`);
  editor.setStatus(`Inserted test case ${n}`);
});

editor.registerCommand("Insert: Test Case", "Prompt for a number and insert a test-case skeleton", "insert_test_case");
