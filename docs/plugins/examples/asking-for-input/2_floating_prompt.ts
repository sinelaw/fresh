/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// 2. Floating prompt — `startPrompt(label, type, floatingOverlay = true)`.
// A centred card with an input row and a results pane, as Live Grep uses.
// It suits "type to search, then pick", and free text still works.
// `prompt_changed` fires on every keystroke so you can refilter.
// The answer arrives in `prompt_confirmed`.

const PROMPT = "ask-floating";
const RECENT = [
  { n: "1234", title: "Crash when saving an empty file" },
  { n: "1240", title: "Tab width ignored in Makefiles" },
  { n: "1251", title: "Slow startup with many plugins" },
  { n: "1263", title: "Search highlights stay after Esc" },
];

function show(filter: string) {
  const f = filter.toLowerCase();
  editor.setPromptSuggestions(
    RECENT.filter((i) => (i.n + " " + i.title).toLowerCase().includes(f))
      // Fresh 0.5.x. Newer builds also need a unique `id` on each suggestion.
      .map((i) => ({ text: `#${i.n}`, value: i.n, description: i.title })),
  );
}

registerHandler("ask_floating_prompt", () => {
  editor.startPrompt("Issue: ", PROMPT, true);
  editor.setPromptFooter([
    { text: "Enter", style: { fg: "ui.help_key_fg" } }, { text: " confirm   " },
    { text: "Esc", style: { fg: "ui.help_key_fg" } }, { text: " cancel" },
  ]);
  show("");
});

editor.on("prompt_changed", (a) => {
  if (a.prompt_type === PROMPT) show(a.input);
  return true;
});

editor.on("prompt_confirmed", (a) => {
  if (a.prompt_type !== PROMPT) return true;
  editor.setStatus(`You entered: ${a.input}`);
  return true;
});

editor.registerCommand("Ask: Floating Prompt", "Ask via a centred floating prompt", "ask_floating_prompt");
