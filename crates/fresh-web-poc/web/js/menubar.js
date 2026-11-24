// The menu bar: File, Edit, View, Selection, Go, LSP, Explorer, Help, as in
// the editor. Its rows are commands from the registry (a row whose command
// no plugin registered is left out), shown with their keys and, for
// toggles, a check read when the menu opens. Plugins add rows with
// `fresh.menus.add(menu, row)`.
//
// The mouse: a title opens its menu, moving onto another title switches to
// it. The keyboard: F10 opens the first menu, Alt+F / E / V / S / G / X / H
// the one with that letter (LSP has none: Alt+L is lowercase); Left and Right past a menu's edge move to
// the next title's.

import { Menu } from "./menu.js";

const $ = (s) => document.querySelector(s);

// A row: a command id; {command, label?, keys?, checked?}; {label, items} (a
// submenu); null (a separator). `checked(app)` makes the row a toggle.
const view = (app) => app.view();
const setting = (k) => (app) => !!app.settings[k];
export const MENUS = [
  { label: "File", key: "f", items: [
    { command: "explorer.newFile", label: "New File…" },
    { command: "palette.files", label: "Open File…" },
    { command: "file.recent", label: "Open Recent…" },
    null,
    { command: "file.save", label: "Save" },
    { command: "file.saveAs", label: "Save As…" },
    { command: "file.saveAll", label: "Save All" },
    { command: "file.revert", label: "Revert File" },
    null,
    { command: "file.language", label: "Set Language…" },
    { command: "file.encoding", label: "Set Encoding…" },
    { command: "file.lineEnding", label: "Set Line Ending…" },
    { command: "file.toggleReadOnly", label: "Read-Only", checked: (app) => !!view(app)?.buf.readOnly },
    null,
    { command: "view.closeTab", label: "Close Tab" },
    { command: "view.closeAllTabs", label: "Close All Tabs" },
    null,
    { command: "workspace.trust", label: "Trust Workspace" },
    { command: "orchestrator.overview", label: "Workspaces…" },
    { command: "orchestrator.switch", label: "Switch Workspace…" },
    { command: "orchestrator.new", label: "New Workspace…" },
    { command: "orchestrator.agent", label: "Start Agent Session…" },
    { command: "orchestrator.close", label: "Close Workspace" },
  ] },
  { label: "Edit", key: "e", items: [
    { command: "edit.undo", label: "Undo" },
    { command: "edit.redo", label: "Redo" },
    null,
    { command: "edit.cut", label: "Cut", keys: "Ctrl+X" },
    { command: "edit.copy", label: "Copy", keys: "Ctrl+C" },
    { command: "edit.paste", label: "Paste", keys: "Ctrl+V" },
    null,
    { command: "search.find", label: "Find…" },
    { command: "search.replace", label: "Replace…" },
    { command: "search.findNext", label: "Find Next" },
    { command: "search.findPrevious", label: "Find Previous" },
    { command: "search.project", label: "Search in Workspace…" },
    null,
    { command: "edit.deleteLine", label: "Delete Line" },
    { command: "edit.duplicateLine", label: "Duplicate Line" },
    { command: "edit.moveLineUp", label: "Move Line Up" },
    { command: "edit.moveLineDown", label: "Move Line Down" },
    { command: "edit.toggleComment", label: "Toggle Comment" },
    { command: "lsp.format", label: "Format Document" },
    { label: "Transform", items: [
      { command: "edit.upperCase", label: "Uppercase" }, { command: "edit.lowerCase", label: "Lowercase" },
      { command: "edit.sortLines", label: "Sort Lines" }, { command: "edit.trimTrailing", label: "Trim Trailing Whitespace" },
    ] },
    null,
    { label: "Settings", items: [
      { command: "settings.autoSave", label: "Auto Save", checked: setting("auto_save") },
      { command: "settings.autoClose", label: "Auto Close Brackets", checked: setting("auto_close") },
    ] },
    { command: "keys.editor", label: "Keyboard Shortcuts" },
  ] },
  { label: "View", key: "v", items: [
    { command: "palette.commands", label: "Command Palette…" },
    null,
    { command: "explorer.toggle", label: "File Explorer", checked: () => !$("#sidebar").classList.contains("hidden") },
    { command: "terminal.toggle", label: "Terminal", checked: (app) => !!app.term?.visible },
    { command: "lsp.diagnostics", label: "Diagnostics" },
    { command: "markdown.contents", label: "Markdown Contents" },
    null,
    { command: "view.toggleLineNumbers", label: "Line Numbers", checked: (app) => !!view(app)?.opts.lineNumbers },
    { command: "view.toggleWrap", label: "Line Wrap", checked: (app) => !!view(app)?.opts.wrap },
    { command: "view.toggleWhitespace", label: "Whitespace", checked: setting("show_whitespace") },
    { command: "view.toggleCurrentLine", label: "Current Line", checked: setting("highlight_current_line") },
    { command: "view.toggleOccurrences", label: "Occurrences", checked: setting("highlight_occurrences") },
    { command: "view.toggleIndentGuides", label: "Indentation Guides", checked: setting("show_indent_guides") },
    { command: "view.toggleBrackets", label: "Matching Brackets", checked: setting("highlight_brackets") },
    { command: "markdown.toggleCompose", label: "Markdown Compose", checked: (app) => view(app)?.mode === "compose" },
    { label: "Rulers", items: ["view.addRuler", "view.removeRulers"] },
    { label: "Folding", items: [{ command: "fold.toggle", label: "Toggle Fold" }, { command: "fold.all", label: "Fold All" }, { command: "fold.none", label: "Unfold All" }] },
    null,
    { command: "view.zoomIn", label: "Zoom In" },
    { command: "view.zoomOut", label: "Zoom Out" },
    { command: "view.zoomReset", label: "Reset Zoom" },
    null,
    { command: "theme.select", label: "Select Theme…" },
    { command: "skin.select", label: "Select Skin…" },
    { label: "Keymap", items: [
      { command: "settings.keymapDefault", label: "Default", checked: (app) => app.settings.keymap !== "emacs" },
      { command: "settings.keymapEmacs", label: "Emacs", checked: (app) => app.settings.keymap === "emacs" },
    ] },
    null,
    { command: "view.splitRight", label: "Split Right" },
    { command: "view.splitDown", label: "Split Down" },
    { command: "view.closeSplit", label: "Close Split" },
    { command: "view.maximizeSplit", label: "Maximize Split" },
    { command: "view.nextSplit", label: "Next Split" },
    { command: "view.prevSplit", label: "Previous Split" },
    null,
    { label: "Terminal", items: [
      { command: "terminal.toggle", label: "Toggle Terminal" }, { command: "terminal.new", label: "New Terminal" },
      { command: "terminal.restart", label: "Restart Terminal" }, { command: "terminal.sendSelection", label: "Send Selection to Terminal" },
    ] },
    { label: "Git", items: [
      { command: "git.log", label: "Log" }, { command: "git.logFile", label: "Log of Current File" },
      { command: "git.blame", label: "Toggle Blame" }, { command: "git.diffFile", label: "Diff Current File" },
      { command: "compare.head", label: "Compare with Last Commit" }, null, { command: "git.refresh", label: "Refresh" },
    ] },
    { label: "Compare", items: [{ command: "compare.files", label: "Compare Files…" }, { command: "compare.next", label: "Next Difference" }, { command: "compare.prev", label: "Previous Difference" }] },
    { label: "Code Tour", items: [{ command: "tour.open", label: "Open…" }, { command: "tour.next", label: "Next Step" }, { command: "tour.prev", label: "Previous Step" }] },
  ] },
  { label: "Selection", key: "s", items: [
    { command: "select.all", label: "Select All" },
    { command: "select.line", label: "Select Line" },
    null,
    { command: "cursor.addAbove", label: "Add Cursor Above" },
    { command: "cursor.addBelow", label: "Add Cursor Below" },
    { command: "select.nextOccurrence", label: "Add Next Occurrence" },
    { command: "select.allOccurrences", label: "Select All Occurrences" },
    { command: "edit.escape", label: "Single Cursor" },
  ] },
  { label: "Go", key: "g", items: [
    { command: "go.back", label: "Back" },
    { command: "go.forward", label: "Forward" },
    null,
    { command: "palette.files", label: "Go to File…" },
    { command: "palette.buffers", label: "Switch Buffer…" },
    { command: "palette.line", label: "Go to Line…" },
    { command: "lsp.symbols", label: "Go to Symbol in File…" },
    null,
    { command: "lsp.definition", label: "Go to Definition" },
    { command: "lsp.references", label: "Find References" },
    { command: "lsp.nextDiagnostic", label: "Next Problem" },
    { command: "lsp.prevDiagnostic", label: "Previous Problem" },
    { command: "git.nextChange", label: "Next Change" },
    { command: "git.prevChange", label: "Previous Change" },
    null,
    { command: "cursor.matchingBracket", label: "Matching Bracket" },
    { command: "cursor.paragraphDown", label: "Next Paragraph" },
    { command: "cursor.paragraphUp", label: "Previous Paragraph" },
    null,
    { command: "view.nextTab", label: "Next Tab" },
    { command: "view.prevTab", label: "Previous Tab" },
  ] },
  { label: "LSP", key: "", items: [
    { command: "lsp.hover", label: "Show Hover" },
    { command: "lsp.definition", label: "Go to Definition" },
    { command: "lsp.references", label: "Find References" },
    { command: "lsp.rename", label: "Rename Symbol…" },
    { command: "lsp.completion", label: "Completion" },
    { command: "lsp.signatureHelp", label: "Signature Help" },
    { command: "lsp.codeActions", label: "Code Actions…" },
    { command: "lsp.format", label: "Format Document" },
    { command: "lsp.symbols", label: "Go to Symbol in File…" },
    { command: "lsp.diagnostics", label: "Diagnostics" },
    null,
    { command: "lsp.status", label: "Show Status" },
  ] },
  { label: "Explorer", key: "x", items: [
    { command: "explorer.focus", label: "Focus Explorer" },
    { command: "explorer.toggle", label: "Show Explorer", checked: () => !$("#sidebar").classList.contains("hidden") },
    null,
    { command: "explorer.newFile", label: "New File…" },
    { command: "explorer.newFolder", label: "New Folder…" },
    { command: "explorer.rename", label: "Rename…" },
    { command: "explorer.delete", label: "Delete" },
    null,
    { command: "explorer.reveal", label: "Reveal Active File" },
    { command: "explorer.collapseAll", label: "Collapse All" },
    { command: "explorer.refresh", label: "Refresh" },
  ] },
  { label: "Help", key: "h", items: [
    { command: "help.welcome", label: "Welcome" },
    { command: "keys.editor", label: "Keyboard Shortcuts" },
    { command: "palette.commands", label: "All Commands…" },
    null,
    { command: "help.log", label: "Show Log" },
  ] },
];

export class MenuBar {
  constructor(app) {
    this.app = app;
    this.host = $("#menubar");
    this.menus = MENUS.map((m) => ({ ...m, items: [...m.items] }));
    this.open = -1;
    this.menu = null;
  }

  // Draw the titles and take their pointer; register the commands that
  // open the menus (bound to F10 and Alt+letter in the keymap).
  listen() {
    this.render();
    this.host.addEventListener("mousedown", (e) => {
      const t = e.target.closest(".mtop");
      if (!t) return;
      e.preventDefault();
      const i = +t.dataset.i;
      if (this.open === i) this.close(); else this.show(i, false);
    });
    // Moving onto another title switches to its menu.
    this.host.addEventListener("mousemove", (e) => {
      const t = e.target.closest(".mtop");
      if (t && this.open >= 0 && +t.dataset.i !== this.open) this.show(+t.dataset.i, false);
    });
    const cmd = (id, title, run) => this.app.commands.set(id, { title, run, source: "core", hidden: true });
    cmd("menu.open", "Open the Menu Bar", () => this.show(0, true));
    for (const [i, m] of this.menus.entries()) cmd(`menu.${m.label.toLowerCase()}`, `Open the ${m.label} Menu`, () => this.show(i, true));
  }

  render() {
    this.host.replaceChildren(...this.menus.map((m, i) => {
      const b = document.createElement("button");
      b.className = "mtop" + (this.open === i ? " open" : "");
      b.dataset.i = String(i);
      b.tabIndex = -1;
      b.textContent = m.label;
      b.title = m.key ? `${m.label} (Alt+${m.key.toUpperCase()})` : m.label;
      b.setAttribute("aria-haspopup", "true");
      b.setAttribute("aria-expanded", String(this.open === i));
      return b;
    }));
  }

  // A plugin's row at the end of menu `label` (a new menu when there is
  // none by that name).
  add(label, row) {
    let m = this.menus.find((x) => x.label === label);
    if (!m) { m = { label, key: "", items: [] }; this.menus.push(m); }
    m.items.push(row);
    this.render();
  }

  // Rows as the Menu component takes them: commands resolved now (title,
  // key, check), unknown ones left out, separators never doubled.
  resolve(rows) {
    const app = this.app;
    const out = [];
    for (const r of rows) {
      if (!r) { if (out.length && out[out.length - 1]) out.push(null); continue; }
      const row = typeof r === "string" ? { command: r } : r;
      if (row.items) {
        const items = this.resolve(row.items);
        if (items.some(Boolean)) out.push({ label: row.label, items });
        continue;
      }
      const c = app.commands.get(row.command);
      if (!c) continue;
      out.push({
        label: row.label || c.title,
        keys: row.keys || app.keyLabel(row.command),
        checked: row.checked ? row.checked(app) : undefined,
        run: () => app.run(row.command),
      });
    }
    while (out.length && !out[out.length - 1]) out.pop();
    return out;
  }

  // Open menu `i` under its title; `keys`: highlight its first row.
  show(i, keys) {
    const prev = this.menu;
    this.menu = null;
    if (prev) prev.close();
    this.open = i;
    this.render();
    const n = this.menus.length;
    const menu = new Menu(this.resolve(this.menus[i].items), {
      app: this.app,
      ignore: "#menubar",
      onSide: (d) => this.show((i + d + n) % n, true),
      onClose: () => { if (this.menu === menu) { this.menu = null; this.open = -1; this.render(); } },
    }).build();
    this.menu = menu;
    const title = this.host.children[i].getBoundingClientRect();
    menu.openUnder(title, getComputedStyle(this.host).flexDirection.startsWith("column"));
    if (keys) menu.enter();
  }

  close() { if (this.menu) this.menu.close(); }
}
