// The menu bar: File, Edit, View, Selection, Go, LSP, Explorer, Help, as in
// the editor. Its rows are commands from the registry (a row whose command
// no plugin registered is left out), shown with their keys and, for
// toggles, a check read when the menu opens. Plugins add rows with
// `fresh.addMenuItem(app, menu, row)` (api.js).
//
// The mouse: a title opens its menu, moving onto another title switches to
// it. The keyboard: F10 opens the first menu, Alt+F / E / V / S / G / X / H
// the one with that letter (LSP has none: Alt+L is lowercase); Left and Right past a menu's edge move to
// the next title's.

import { makeMenu, menuSpec, actionOf, submenu, SEPARATOR } from "./menu.js";
import { asCommand } from "./commands.js";

import { $ } from "./dom.js";

// A menu bar row: a command (its label, keys and, for a toggle, the check
// `checked(app)` reads when the menu opens; none given: the command's
// own), a submenu, or a separator.
// A toggle's check: a setting (on when true), a piece of the editor's
// state (MenuBar.state), or a plugin's own reading.
/** type Check = {kind: "setting", key: String} | {kind: "state", name: String} | {kind: "read", read: () => Boolean} */
/** type MenuRow =
 *   | {kind: "command", command: String, label: String | Null, keys: String | Null, checked: Check | Null}
 *   | {kind: "submenu", label: String, items: MenuRow[]}
 *   | {kind: "separator"} */
/** function rowOf(command: String, label: String | Null, keys: String | Null, checked: Check | Null) => MenuRow */
export function rowOf(command, label, keys, checked) { return { kind: "command", command, label, keys, checked }; }
/** function cmd(command: String, label: String | Null) => MenuRow */
export function cmd(command, label) { return rowOf(command, label, null, null); }
/** function keyed(command: String, label: String, keys: String) => MenuRow */
function keyed(command, label, keys) { return rowOf(command, label, keys, null); }
/** function check(command: String, label: String, checked: Check) => MenuRow */
function check(command, label, checked) { return rowOf(command, label, null, checked); }
// A plugin's toggle row: checked while `read()` is true.
/** function toggle(command: String, label: String, read: () => Boolean) => MenuRow */
export function toggle(command, label, read) { return check(command, label, { kind: "read", read }); }
/** function sub(label: String, items: MenuRow[]) => MenuRow */
function sub(label, items) { return { kind: "submenu", label, items }; }
/** const SEP: MenuRow */
export const SEP = { kind: "separator" };
/** function setting(key: String) => Check */
function setting(key) { return { kind: "setting", key }; }
/** function state(name: String) => Check */
function state(name) { return { kind: "state", name }; }

// A menu bar menu: its title, its Alt+letter (null: none) and its rows.
/** function menuOf(label: String, key: String | Null, items: MenuRow[]) => {label: String, key: String | Null, items: MenuRow[]} */
function menuOf(label, key, items) { return { label, key, items }; }
export const MENUS = [
  menuOf("File", "f", [
    cmd("file.new", "New File"),
    cmd("palette.files", "Open File…"),
    cmd("file.recent", "Open Recent…"),
    SEP,
    cmd("file.save", "Save"),
    cmd("file.saveAs", "Save As…"),
    cmd("file.saveAll", "Save All"),
    cmd("file.revert", "Revert File"),
    SEP,
    cmd("file.language", "Set Language…"),
    cmd("file.encoding", "Set Encoding…"),
    cmd("file.lineEnding", "Set Line Ending…"),
    check("file.toggleReadOnly", "Read-Only", state("readOnly")),
    SEP,
    cmd("view.closeTab", "Close Tab"),
    cmd("view.closeAllTabs", "Close All Tabs"),
    SEP,
    cmd("workspace.trust", "Trust Workspace"),
    cmd("orchestrator.new", "New Workspace…"),
    cmd("orchestrator.switch", "Switch Workspace…"),
    cmd("orchestrator.agent", "Run Agent…"),
    cmd("orchestrator.close", "Close Workspace"),
  ]),
  menuOf("Edit", "e", [
    cmd("edit.undo", "Undo"),
    cmd("edit.redo", "Redo"),
    SEP,
    keyed("edit.cut", "Cut", "Ctrl+X"),
    keyed("edit.copy", "Copy", "Ctrl+C"),
    keyed("edit.paste", "Paste", "Ctrl+V"),
    SEP,
    cmd("search.find", "Find…"),
    cmd("search.replace", "Replace…"),
    cmd("search.findInSelection", "Find in Selection…"),
    cmd("search.queryReplace", "Query Replace…"),
    cmd("search.findNext", "Find Next"),
    cmd("search.findPrevious", "Find Previous"),
    cmd("search.project", "Search in Workspace…"),
    SEP,
    cmd("edit.deleteLine", "Delete Line"),
    cmd("edit.duplicateLine", "Duplicate Line"),
    cmd("edit.moveLineUp", "Move Line Up"),
    cmd("edit.moveLineDown", "Move Line Down"),
    cmd("edit.toggleComment", "Toggle Comment"),
    cmd("lsp.format", "Format Document"),
    sub("Transform", [
      cmd("edit.upperCase", "Uppercase"), cmd("edit.lowerCase", "Lowercase"),
      cmd("edit.sortLines", "Sort Lines"), cmd("edit.trimTrailing", "Trim Trailing Whitespace"),
      cmd("edit.transposeChars", "Transpose Characters"), cmd("edit.transposeLines", "Transpose Lines"),
    ]),
    sub("Macros", [
      cmd("macro.record", "Record Macro…"), cmd("macro.stop", "Stop Recording"),
      cmd("macro.playLast", "Play Last Macro"), cmd("macro.play", "Play Macro…"),
      cmd("macro.list", "List Macros"),
    ]),
    SEP,
    sub("Settings", [
      check("settings.autoSave", "Auto Save", setting("auto_save")),
      check("settings.autoClose", "Auto Close Brackets", setting("auto_close")),
      check("settings.previewTabs", "Preview Tabs", setting("preview_tabs")),
    ]),
    cmd("keys.editor", "Keyboard Shortcuts"),
  ]),
  menuOf("View", "v", [
    check("orchestrator.dock", "Orchestrator Dock", state("orchestrator")),
    cmd("palette.commands", "Command Palette…"),
    SEP,
    check("explorer.toggle", "File Explorer", state("explorer")),
    check("terminal.toggle", "Terminal", state("terminal")),
    cmd("lsp.diagnostics", "Diagnostics"),
    cmd("markdown.contents", "Markdown Contents"),
    SEP,
    check("view.toggleLineNumbers", "Line Numbers", state("lineNumbers")),
    check("view.toggleWrap", "Line Wrap", state("wrap")),
    check("view.toggleWhitespace", "Whitespace", setting("show_whitespace")),
    check("view.toggleCurrentLine", "Current Line", setting("highlight_current_line")),
    check("view.toggleOccurrences", "Occurrences", setting("highlight_occurrences")),
    check("view.toggleIndentGuides", "Indentation Guides", setting("show_indent_guides")),
    check("view.toggleBrackets", "Matching Brackets", setting("highlight_brackets")),
    check("view.toggleRainbow", "Rainbow Brackets", setting("rainbow_brackets")),
    check("view.toggleScrollSync", "Scroll Sync", setting("scroll_sync")),
    check("markdown.toggleCompose", "Markdown Compose", state("compose")),
    sub("Rulers", [cmd("view.addRuler", null), cmd("view.removeRulers", null)]),
    sub("Folding", [cmd("fold.toggle", "Toggle Fold"), cmd("fold.all", "Fold All"), cmd("fold.none", "Unfold All")]),
    SEP,
    cmd("view.zoomIn", "Zoom In"),
    cmd("view.zoomOut", "Zoom Out"),
    cmd("view.zoomReset", "Reset Zoom"),
    SEP,
    cmd("theme.select", "Select Theme…"),
    cmd("skin.select", "Select Skin…"),
    sub("Keymap", [
      check("settings.keymapDefault", "Default", state("keymapDefault")),
      check("settings.keymapEmacs", "Emacs", state("keymapEmacs")),
    ]),
    SEP,
    cmd("view.splitRight", "Split Right"),
    cmd("view.splitDown", "Split Down"),
    cmd("view.closeSplit", "Close Split"),
    cmd("view.maximizeSplit", "Maximize Split"),
    cmd("view.nextSplit", "Next Split"),
    cmd("view.prevSplit", "Previous Split"),
    SEP,
    sub("Terminal", [
      cmd("terminal.toggle", "Toggle Terminal"), cmd("terminal.new", "New Terminal"),
      cmd("terminal.restart", "Restart Terminal"), cmd("terminal.sendSelection", "Send Selection to Terminal"),
    ]),
    sub("Git", [
      cmd("git.log", "Log"), cmd("git.logFile", "Log of Current File"),
      cmd("git.blame", "Toggle Blame"), cmd("git.diffFile", "Diff Current File"),
      cmd("compare.head", "Compare with Last Commit"), SEP, cmd("git.refresh", "Refresh"),
    ]),
    sub("Compare", [cmd("compare.files", "Compare Files…"), cmd("compare.next", "Next Difference"), cmd("compare.prev", "Previous Difference")]),
    sub("Code Tour", [cmd("tour.open", "Open…"), cmd("tour.next", "Next Step"), cmd("tour.prev", "Previous Step")]),
  ]),
  menuOf("Selection", "s", [
    cmd("select.all", "Select All"),
    cmd("select.line", "Select Line"),
    cmd("select.expand", "Expand Selection"),
    cmd("select.shrink", "Shrink Selection"),
    sub("Block Select", [
      cmd("select.blockUp", "Up"), cmd("select.blockDown", "Down"),
      cmd("select.blockLeft", "Left"), cmd("select.blockRight", "Right"),
    ]),
    SEP,
    cmd("cursor.addAbove", "Add Cursor Above"),
    cmd("cursor.addBelow", "Add Cursor Below"),
    cmd("select.nextOccurrence", "Add Next Occurrence"),
    cmd("select.allOccurrences", "Select All Occurrences"),
    cmd("search.findSelectionNext", "Find Selection Next"),
    cmd("search.findSelectionPrevious", "Find Selection Previous"),
    cmd("edit.escape", "Single Cursor"),
  ]),
  menuOf("Go", "g", [
    cmd("go.back", "Back"),
    cmd("go.forward", "Forward"),
    SEP,
    cmd("palette.files", "Go to File…"),
    cmd("palette.buffers", "Switch Buffer…"),
    cmd("palette.line", "Go to Line…"),
    cmd("lsp.symbols", "Go to Symbol in File…"),
    cmd("lsp.workspaceSymbols", "Go to Symbol in Workspace…"),
    SEP,
    cmd("lsp.definition", "Go to Definition"),
    cmd("lsp.references", "Find References"),
    cmd("lsp.nextDiagnostic", "Next Problem"),
    cmd("lsp.prevDiagnostic", "Previous Problem"),
    cmd("git.nextChange", "Next Change"),
    cmd("git.prevChange", "Previous Change"),
    SEP,
    cmd("cursor.matchingBracket", "Matching Bracket"),
    cmd("cursor.paragraphDown", "Next Paragraph"),
    cmd("cursor.paragraphUp", "Previous Paragraph"),
    SEP,
    cmd("view.nextTab", "Next Tab"),
    cmd("view.prevTab", "Previous Tab"),
  ]),
  menuOf("LSP", null, [
    cmd("lsp.hover", "Show Hover"),
    cmd("lsp.definition", "Go to Definition"),
    cmd("lsp.references", "Find References"),
    cmd("lsp.rename", "Rename Symbol…"),
    cmd("lsp.completion", "Completion"),
    cmd("lsp.signatureHelp", "Signature Help"),
    cmd("lsp.codeActions", "Code Actions…"),
    cmd("lsp.format", "Format Document"),
    cmd("lsp.symbols", "Go to Symbol in File…"),
    cmd("lsp.workspaceSymbols", "Go to Symbol in Workspace…"),
    cmd("lsp.diagnostics", "Diagnostics"),
    check("lsp.toggleInlayHints", "Inlay Hints", setting("inlay_hints")),
    SEP,
    cmd("lsp.status", "Show Status"),
    cmd("lsp.restart", "Restart Server"),
    cmd("lsp.stop", "Stop Server"),
  ]),
  menuOf("Explorer", "x", [
    cmd("explorer.focus", "Focus Explorer"),
    check("explorer.toggle", "Show Explorer", state("explorer")),
    SEP,
    cmd("explorer.newFile", "New File…"),
    cmd("explorer.newFolder", "New Folder…"),
    cmd("explorer.rename", "Rename…"),
    cmd("explorer.delete", "Delete"),
    SEP,
    cmd("explorer.reveal", "Reveal Active File"),
    cmd("explorer.collapseAll", "Collapse All"),
    cmd("explorer.refresh", "Refresh"),
  ]),
  menuOf("Help", "h", [
    cmd("help.welcome", "Welcome"),
    cmd("keys.editor", "Keyboard Shortcuts"),
    cmd("palette.commands", "All Commands…"),
    SEP,
    cmd("help.log", "Show Log"),
  ]),
];

export class MenuBar {
  constructor(app) {
    this.app = app;
    this.host = $("#menubar");
    this.menus = MENUS.map((m) => menuOf(m.label, m.key, [...m.items]));
    // The menu open, and its title's index: a list of at most one.
    this.opened = [];
  }


  // Draw the titles and take their pointer; register the commands that
  // open the menus (bound to F10 and Alt+letter in the keymap).
  listen() {
    this.render();
    // The title under a pointer event, as its menu's index.
    const titleAt = (e) => {
      const t = e.target.closest(".mtop");
      return t === null ? null : Number(t.dataset.i);
    };
    this.host.addEventListener("mousedown", (e) => {
      const i = titleAt(e);
      if (i === null) return;
      e.preventDefault();
      if (this.isOpen(i)) this.close(); else this.show(i, false);
    });
    // Moving onto another title switches to its menu.
    this.host.addEventListener("mousemove", (e) => {
      const i = titleAt(e);
      if (i !== null && this.opened.length > 0 && !this.isOpen(i)) this.show(i, false);
    });
    const cmd = (id, title, run) => { this.app().commands.set(id, { title, run: asCommand(run), source: "core", hidden: true }); };
    cmd("menu.open", "Open the Menu Bar", () => this.show(0, true));
    this.menus.forEach((m, i) => { cmd(`menu.${m.label.toLowerCase()}`, `Open the ${m.label} Menu`, () => this.show(i, true)); });
  }

  render() {
    this.host.replaceChildren(...this.menus.map((m, i) => {
      const b = document.createElement("button");
      b.className = "mtop" + (this.isOpen(i) ? " open" : "");
      b.dataset.i = String(i);
      b.tabIndex = -1;
      b.textContent = m.label;
      const key = m.key;
      b.title = key ? `${m.label} (Alt+${key.toUpperCase()})` : m.label;
      b.setAttribute("aria-haspopup", "true");
      b.setAttribute("aria-expanded", String(this.isOpen(i)));
      return b;
    }));
  }

  // A plugin's row at the end of menu `label` (a new menu when there is
  // none by that name).
  add(label, row) {
    const m = this.menus.find((x) => x.label === label);
    if (m) m.items.push(row);
    else this.menus.push(menuOf(label, null, [row]));
    this.render();
  }

  // Rows as the Menu component takes them: commands resolved now (title,
  // key, check), unknown ones left out, separators never doubled.
  resolve(rows) {
    const app = this.app();
    /** const out: MenuItem[] */
    const out = [];
    const lastIsRow = () => out.length > 0 && out[out.length - 1].kind !== "separator";
    for (const r of rows) {
      if (r.kind === "separator") { if (lastIsRow()) out.push(SEPARATOR); continue; }
      if (r.kind === "submenu") {
        const items = this.resolve(r.items);
        if (items.length) out.push(submenu(r.label, items));
        continue;
      }
      const c = app.commands.get(r.command);
      if (!c) continue;
      const checked = r.checked;
      const command = r.command;
      out.push(actionOf(r.label ?? c.title, r.keys ?? app.keyLabel(command), checked !== null ? this.isChecked(checked) : null, false, () => app.run(command)));
    }
    return lastIsRow() || !out.length ? out : out.slice(0, -1);
  }

  // Whether a toggle's check is on now.
  isChecked(c) {
    if (c.kind === "read") return c.read();
    const app = this.app();
    if (c.kind === "setting") return !!app.settings[c.key];
    return this.state(c.name);
  }
  // A piece of the editor's state, by name.
  state(name) {
    const app = this.app();
    const v = app.view();
    const reads = new Map([
      ["readOnly", () => !!v && v.buf.readOnly],
      ["lineNumbers", () => !!v && v.opts.lineNumbers],
      ["wrap", () => !!v && v.opts.wrap],
      ["compose", () => !!v && v.mode === "compose"],
      ["orchestrator", () => app.orchestrator.dockOpen()],
      ["explorer", () => !$("#sidebar").classList.contains("hidden")],
      ["terminal", () => app.term.visible],
      ["keymapDefault", () => app.settings.keymap !== "emacs"],
      ["keymapEmacs", () => app.settings.keymap === "emacs"],
    ]);
    const read = reads.get(name);
    return read ? read() : false;
  }

  isOpen(i) { return this.opened.some((o) => o.i === i); }

  // Open menu `i` under its title; `keys`: highlight its first row.
  show(i, keys) {
    const prev = this.opened;
    this.opened = [];
    for (const o of prev) o.menu.close();
    const n = this.menus.length;
    const menu = makeMenu(this.resolve(this.menus[i].items), menuSpec(() => { const app = this.app(); app.focusPane(app.activePane); }, ["#menubar"],
      (d) => { this.show((i + d + n) % n, true); },
      () => { if (this.opened.some((o) => o.menu === menu)) { this.opened = []; this.render(); } }, null), []);
    this.opened = [{ i, menu }];
    this.render();
    // A menu bar's menu: a skin may join it to its title.
    menu.el.classList.add("of-bar");
    const title = this.host.children[i].getBoundingClientRect();
    menu.openUnder(title, getComputedStyle(this.host).flexDirection.startsWith("column"));
    if (keys) menu.enter();
  }

  close() { for (const o of this.opened) o.menu.close(); }
}
