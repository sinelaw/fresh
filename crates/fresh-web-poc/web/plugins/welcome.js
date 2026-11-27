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
  /** const keyLine: (String, String) => Entry */
  const keyLine = (id, what) => ({ text: `  ${(fresh.keyLabel(app, id).at(0) ?? "palette").padEnd(22)} ${what}\n`, props: { command: id }, style: null });
  const show = () => {
    /** const entries: Entry[] */
    const entries = [
      { text: "Fresh\n", props: {}, style: TITLE },
      { text: "\nThe server holds the workspace: its files, each open document's ordered edit log,\nterminals, processes and one connection per language server. This page does the\nrest: layout, editing, undo, decorations and plugins.\n\n", props: {}, style: null },
      ...keys.map(([id, what]) => keyLine(id, what)),
      { text: "\nOpen the same file in two browser tabs to edit it together.\nIn a Markdown file, run “Markdown: Toggle Compose”.\n", props: {}, style: NOTE },
    ];
    fresh.createVirtual(app, { name: "Welcome", entries, mode: "welcome", readOnly: true, language: null, keep: false, place: "tab", lineNumbers: false });
  };
  fresh.defineMode(app, "welcome", [{
    key: "Enter",
    run: () => {
      const v = fresh.activeView(app);
      if (v === null) return;
      const command = v.buffer.propsAt(v.cursor()).at(0)?.command;
      if (command !== undefined) fresh.run(app, command);
    },
  }]);
  fresh.register(app, "help.welcome", { title: "Help: Welcome", hidden: false, run: () => { show(); } });
  fresh.on(app, "app.ready", () => {
    if (!fresh.activeView(app)) show();
  });
}
