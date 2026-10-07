/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 3. Pick list — `startPrompt` + `setPromptSuggestions`.
// The user picks one of several values (or types their own). The chosen
// suggestion's `value` arrives as `input` in `prompt_confirmed`.

const PROMPT = "ask-pick";

registerHandler("ask_pick_list", () => {
  editor.startPrompt("Priority:", PROMPT);
  editor.setPromptSuggestions([
    // Fresh 0.5.x. Newer builds also need a unique `id` on each suggestion.
    { text: "P0", value: "P0", description: "Drop everything" },
    { text: "P1", value: "P1", description: "This sprint" },
    { text: "P2", value: "P2", description: "Next sprint" },
    { text: "P3", value: "P3", description: "Backlog" },
  ]);
});

editor.on("prompt_confirmed", (a) => {
  if (a.prompt_type !== PROMPT) return true;
  editor.setStatus(`You picked: ${a.input}`);
  return true;
});

editor.registerCommand("Ask: Pick List", "Ask the user to choose from a list", "ask_pick_list");
