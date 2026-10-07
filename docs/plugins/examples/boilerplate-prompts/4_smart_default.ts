/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// Alternative 4 — guess the value so you usually just press Enter.
//
// Scan the buffer for the last "Step N" and prefill the prompt with N+1.
// If text is selected, it is used as the default instead and replaced.

registerHandler("insert_next_step", async () => {
  const buf = editor.getActiveBufferId();
  const text = await editor.getBufferText(buf, 0, editor.getBufferLength(buf));
  const nums = [...text.matchAll(/^## Step (\d+)/gm)].map((m) => Number(m[1]));
  const next = nums.length ? Math.max(...nums) + 1 : 1;

  const sel = editor.getPrimaryCursor()?.selection;
  const selected = sel && sel.end > sel.start ? await editor.getBufferText(buf, sel.start, sel.end) : null;

  const n = await editor.prompt("Step number:", selected ?? String(next));
  if (n === null) return;

  const snippet = `## Step ${n}\n\nTODO: describe step ${n}.\n\n`;
  if (sel && selected !== null) {
    editor.deleteRange(buf, sel.start, sel.end);
    editor.insertText(buf, sel.start, snippet);
  } else {
    editor.insertAtCursor(snippet);
  }
});

editor.registerCommand("Insert: Next Step", "Insert '## Step N' with N guessed from the buffer", "insert_next_step");
