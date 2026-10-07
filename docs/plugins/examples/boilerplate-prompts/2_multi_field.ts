/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// Alternative 2 — several fields, driven by {{placeholders}} in the template.
//
// Write the boilerplate once with {{name}} or {{name:default}} markers.
// The command asks for each distinct placeholder in order (prefilled with
// its default), substitutes every occurrence, and inserts the result.

const HEADER = `/*
 * {{module}} — {{summary}}
 *
 * Author:  {{author:Jane Doe}}
 * Ticket:  PROJ-{{ticket}}
 * Since:   v{{version:1.0.0}}
 */
`;

async function fillTemplate(tpl: string): Promise<string | null> {
  const values: Record<string, string> = {};
  for (const m of tpl.matchAll(/\{\{(\w+)(?::([^}]*))?\}\}/g)) {
    const [, name, def = ""] = m;
    if (name in values) continue; // ask once per name
    const v = await editor.prompt(`${name}:`, def);
    if (v === null) return null; // Esc cancels the whole insert
    values[name] = v;
  }
  return tpl.replace(/\{\{(\w+)(?::[^}]*)?\}\}/g, (_, name) => values[name]);
}

registerHandler("insert_file_header", async () => {
  const text = await fillTemplate(HEADER);
  if (text !== null) editor.insertAtCursor(text);
});

editor.registerCommand("Insert: File Header", "Fill in a file header template", "insert_file_header");
