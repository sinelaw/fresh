/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 3. Pick list
// Shows a list of choices on the bottom line.
// The chosen item's value comes in prompt_confirmed.

const PROMPT = "ask-pick";

registerHandler("ask_pick_list", () => {
  editor.startPrompt("Priority:", PROMPT);
  editor.setPromptSuggestions([
    // Each item needs an `id` that is unique within the list.
    { id: "p0", text: "P0", value: "P0", description: "Drop everything" },
    { id: "p1", text: "P1", value: "P1", description: "This sprint" },
    { id: "p2", text: "P2", value: "P2", description: "Next sprint" },
    { id: "p3", text: "P3", value: "P3", description: "Backlog" },
  ]);
});

editor.on("prompt_confirmed", (a) => {
  if (a.prompt_type !== PROMPT) return true;
  editor.setStatus(`You picked: ${a.input}`);
  return true;
});

editor.registerCommand("Ask: Pick List", "Ask the user to choose from a list", "ask_pick_list");
