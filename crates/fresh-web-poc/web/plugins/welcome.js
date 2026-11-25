// The welcome page: what this editor is and its keys.

import * as fresh from "../js/api.js";

// The title, and the closing note.
/** const TITLE: Style */
const TITLE = { fg: "syntax.type", bg: null, bold: true, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false };
/** const NOTE: Style */
const NOTE = { fg: "editor.line_number_fg", bg: null, bold: false, italic: false, underline: false, strike: false, mono: false, extendToLineEnd: false };

export function activate(app) {
  const keys = [
    ["palette.commands", "commands"], ["palette.files", "open a file (found on the server)"], ["palette.buffers", "switch buffer"],
    ["palette.line", "go to line"], ["file.save", "save"], ["edit.undo", "undo (this client's own edits)"],
    ["search.find", "find / replace in the buffer"], ["search.project", "search the workspace"], ["terminal.toggle", "terminal"],
    ["view.splitRight", "split right"], ["select.nextOccurrence", "add the next occurrence to the selection"],
    ["lsp.definition", "go to definition"], ["lsp.references", "find references"], ["git.log", "git log"], ["go.back", "go back"],
  ];
  // A key's line, which runs its command on Enter.
  /** function keyLine(id: String, what: String) => Entry */
  function keyLine(id, what) {
    return { text: `  ${(fresh.keyLabel(app, id) || "palette").padEnd(22)} ${what}\n`, props: { command: id }, style: null };
  }
  const show = () => {
    /** const entries: Entry[] */
    const entries = [
      { text: "Fresh\n", props: {}, style: TITLE },
      { text: "\nThe server holds the workspace: its files, each open document's ordered edit log,\nterminals, processes and one connection per language server. This page does the\nrest: layout, editing, undo, decorations and plugins.\n\n", props: {}, style: null },
      ...keys.map(([id, what]) => keyLine(id, what)),
      { text: "\nOpen the same file in two browser tabs to edit it together.\nIn a Markdown file, run “Markdown: Toggle Compose”.\n", props: {}, style: NOTE },
    ];
    const b = fresh.createVirtual(app, { name: "Welcome", entries, mode: "welcome", readOnly: true, language: null, keep: false, place: "tab", lineNumbers: false });
    return b;
  };
  fresh.defineMode(app, "welcome", {
    Enter: () => {
      const v = fresh.activeView(app);
      const props = v.buffer.propsAt(v.cursor())[0];
      if (props?.command) fresh.run(app, props.command);
    },
  });
  fresh.register(app, "help.welcome", { title: "Help: Welcome", hidden: false, run: show });
  fresh.on(app, "app.ready", () => {
    if (!fresh.activeView(app)) show();
  });
}
