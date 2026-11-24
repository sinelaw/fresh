// The welcome page: what this editor is and its keys.

export function activate(fresh) {
  const keys = [
    ["palette.commands", "commands"], ["palette.files", "open a file (found on the server)"], ["palette.buffers", "switch buffer"],
    ["palette.line", "go to line"], ["file.save", "save"], ["edit.undo", "undo (this client's own edits)"],
    ["search.find", "find / replace in the buffer"], ["search.project", "search the workspace"], ["terminal.toggle", "terminal"],
    ["view.splitRight", "split right"], ["select.nextOccurrence", "add the next occurrence to the selection"],
    ["lsp.definition", "go to definition"], ["lsp.references", "find references"], ["git.log", "git log"], ["go.back", "go back"],
  ];
  const show = () => {
    const entries = [
      { text: "Fresh\n", style: { fg: "syntax.type", bold: true } },
      { text: "\nThe server holds the workspace: its files, each open document's ordered edit log,\nterminals, processes and one connection per language server. This page does the\nrest: layout, editing, undo, decorations and plugins.\n\n" },
      ...keys.map(([id, what]) => ({ text: `  ${(fresh.keys.label(id) || "palette").padEnd(22)} ${what}\n`, props: { command: id } })),
      { text: "\nOpen the same file in two browser tabs to edit it together.\nIn a Markdown file, run “Markdown: Toggle Compose”.\n", style: { fg: "editor.line_number_fg" } },
    ];
    const b = fresh.buffers.createVirtual({ name: "Welcome", entries, mode: "welcome", place: "tab", lineNumbers: false });
    return b;
  };
  fresh.modes.define("welcome", {
    Enter: () => {
      const v = fresh.views.active();
      const props = v.buffer.propsAt(v.cursor)[0];
      if (props?.command) fresh.commands.run(props.command);
    },
  });
  fresh.commands.register("help.welcome", { title: "Help: Welcome", run: show });
  fresh.events.on("app.ready", () => {
    if (!fresh.views.active()) show();
  });
}
