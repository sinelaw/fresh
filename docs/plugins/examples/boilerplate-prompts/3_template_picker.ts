/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// Alternative 3 — one command, many templates: pick from a list, then fill.
//
// `startPrompt` + `setPromptSuggestions` shows a pick list;
// the `prompt_confirmed` hook fires with the chosen suggestion's `value`.
// The chosen template is then filled with the same {{placeholder}} loop
// as alternative 2.

const TEMPLATES: Record<string, { desc: string; body: string }> = {
  section: { desc: "Numbered section banner", body: "// ===== {{n}}. {{title}} =====\n" },
  issue:   { desc: "Issue reference comment", body: "// See issue #{{n}}: https://github.com/acme/app/issues/{{n}}\n" },
  todo:    { desc: "TODO with owner + date",  body: "// TODO({{owner:me}}, {{date}}): {{what}}\n" },
  step:    { desc: "Numbered procedure step", body: "## Step {{n}}\n\n{{text}}\n\n" },
};

async function fillTemplate(tpl: string): Promise<string | null> {
  const values: Record<string, string> = {};
  for (const m of tpl.matchAll(/\{\{(\w+)(?::([^}]*))?\}\}/g)) {
    const [, name, def = ""] = m;
    if (name in values) continue;
    const v = await editor.prompt(`${name}:`, name === "date" ? new Date().toISOString().slice(0, 10) : def);
    if (v === null) return null;
    values[name] = v;
  }
  return tpl.replace(/\{\{(\w+)(?::[^}]*)?\}\}/g, (_, name) => values[name]);
}

registerHandler("pick_template", () => {
  editor.startPrompt("Template:", "boilerplate-pick");
  editor.setPromptSuggestions(
    Object.entries(TEMPLATES).map(([key, t]) => ({
      // Fresh 0.5.x. On newer builds suggestions also need a unique `id: key`.
      text: key, value: key, description: t.desc,
    })),
  );
});

editor.on("prompt_confirmed", async (args) => {
  if (args.prompt_type !== "boilerplate-pick") return true;
  const tpl = TEMPLATES[args.input];
  if (!tpl) return true;
  const text = await fillTemplate(tpl.body);
  if (text !== null) editor.insertAtCursor(text);
  return true;
});

editor.registerCommand("Insert: Boilerplate…", "Pick a template and fill in its fields", "pick_template");
