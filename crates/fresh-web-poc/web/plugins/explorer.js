// The file explorer: the workspace's tree in the sidebar, gitignore-aware
// (as the server lists it), with file operations through the server.
//
// Keys in the tree: Enter open/expand, Left/Right collapse/expand,
// a or C-n new file, A new folder, F2 or r rename, Delete remove, C-c copy,
// C-x cut, C-v paste, C-a select all, Escape clear the selection. Ctrl+click
// and Shift+click (Shift+Up/Down) select several; deleting, copying,
// cutting, dragging and the right-click menu act on all of them.

import * as fresh from "../js/api.js";
import { h } from "../js/ui.js";

// A row of the tree.
/** type Row = {name: String, path: String, dir: Boolean, ignored: Boolean, depth: Number} */

// The selection: the cursor row, the rows marked (Ctrl+click, Shift+click,
// Shift+arrows, Ctrl+A) and the row a range is extended from. With none
// marked, the cursor row is the selection.
/** type Sel = {cursor: String | Null, marks: String[], anchor: String | Null} */
/** const NO_SEL: Sel */
const NO_SEL = { cursor: null, marks: [], anchor: null };
// `p` alone.
/** function selectOnly(p: String) => Sel */
function selectOnly(p) { return { cursor: p, marks: [p], anchor: p }; }
// `p` added to the marked rows, or taken out (the cursor row counts as
// marked when none is).
/** function selectToggled(s: Sel, p: String) => Sel */
function selectToggled(s, p) {
  const c = s.cursor;
  const marked = s.marks.length || c === null ? s.marks : [c];
  return { cursor: p, marks: marked.includes(p) ? marked.filter((x) => x !== p) : [...marked, p], anchor: p };
}
// The rows from the anchor to `p` (or `p` alone, without an anchor).
/** function selectRange(s: Sel, rows: Row[], p: String) => Sel */
function selectRange(s, rows, p) {
  const a = s.anchor;
  const i = a === null ? -1 : rows.findIndex((r) => r.path === a), j = rows.findIndex((r) => r.path === p);
  if (i < 0 || j < 0) return selectOnly(p);
  return { cursor: p, marks: rows.slice(Math.min(i, j), Math.max(i, j) + 1).map((r) => r.path), anchor: a };
}
// `paths` marked, the cursor on the first.
/** function selectPaths(paths: String[]) => Sel */
function selectPaths(paths) {
  return paths.length ? { cursor: paths[0], marks: paths, anchor: paths[0] } : NO_SEL;
}
// The cursor on `p`, the marks kept.
/** function cursorOn(s: Sel, p: String) => Sel */
function cursorOn(s, p) { return { cursor: p, marks: s.marks, anchor: s.anchor }; }

// The folder an action on row `r` (or on no row: the root) acts in.
/** function dirOf(root: String, r: Row | Undefined) => String */
function dirOf(root, r) {
  if (r === undefined) return root;
  return r.dir ? r.path : r.path.replace(/\/[^/]*$/, "");
}
// What an action on row `r` acts on: the marked rows when `r` is one of
// them (in tree order, leaving out what a marked folder holds), else `r`.
/** function targetsOf(s: Sel, rows: Row[], r: Row | Undefined) => Row[] */
function targetsOf(s, rows, r) {
  if (r === undefined) return [];
  if (!(s.marks.length > 1 && s.marks.includes(r.path))) return [r];
  const out = [];
  for (const x of rows) if (s.marks.includes(x.path) && !out.some((o) => o.dir && x.path.startsWith(o.path + "/"))) out.push(x);
  return out;
}

// What is copied or cut, to paste.
/** type Clip = {kind: "none"} | {kind: "copy", paths: String[]} | {kind: "cut", paths: String[]} */
/** const NO_CLIP: Clip */
const NO_CLIP = { kind: "none" };
/** function copiedClip(paths: String[]) => Clip */
function copiedClip(paths) { return { kind: "copy", paths }; }
/** function cutClip(paths: String[]) => Clip */
function cutClip(paths) { return { kind: "cut", paths }; }

// What the tree shows besides the rest: dotfiles, and files the ignore
// rules leave out (dimmed when shown).
/** type Show = {hidden: Boolean, ignored: Boolean} */
const SHOW = "fresh-poc:explorer";
/** function savedShow() => Show */
function savedShow() {
  try {
    const saved = JSON.parse(localStorage.getItem(SHOW) || "{}");
    return { hidden: saved.hidden === true, ignored: saved.ignored === true };
  } catch { return { hidden: false, ignored: false }; }
}

export function activate(app) {
  const root = fresh.root(app);
  const open = new Set();
  const children = new Map(); // dir -> entries, as the server lists them
  /** let rows: Row[] */
  let rows = [];
  /** let sel: Sel */
  let sel = NO_SEL;
  /** let clip: Clip */
  let clip = NO_CLIP;
  /** let show: Show */
  let show = savedShow();
  const visible = (e) => (show.hidden || !e.name.startsWith(".")) && (show.ignored || !e.ignored);
  const panel = fresh.panel(app, { id: "explorer", title: "Explorer", place: "sidebar", onClose: null });

  const load = async (dir) => {
    try { children.set(dir, await fresh.list(app, dir)); } catch (e) { children.set(dir, []); }
  };
  // Every shown folder is watched, so files made or removed outside the
  // editor (a terminal, git, another client) appear: dir -> the watch's
  // stop, once it is set up.
  const watching = new Map();
  const expand = async (dir) => {
    open.add(dir);
    await load(dir);
    if (!watching.has(dir)) watching.set(dir, fresh.watch(app, dir, async () => { await load(dir); render(); }));
  };
  const collapse = (dir) => {
    open.delete(dir);
    const stop = watching.get(dir);
    watching.delete(dir);
    if (stop) stop.then((f) => { f(); });
  };
  // The rows: the open folders' entries, as a tree.
  /** const tree: () => Row[] */
  const tree = () => {
    const out = [];
    const walk = (dir, depth) => {
      const list = children.get(dir);
      if (!list) return;
      for (const e of list) {
        if (!visible(e)) continue;
        out.push({ name: e.name, path: e.path, dir: e.dir, ignored: e.ignored, depth });
        if (e.dir && open.has(e.path)) walk(e.path, depth + 1);
      }
    };
    walk(root, 0);
    return out;
  };
  const marked = (p) => sel.marks.length > 1 && sel.marks.includes(p);
  // Select row `r` as a press or a key asks: alone, toggled (`ctrl`), or
  // the range from the anchor (`shift`). The list is drawn again only when
  // the marked rows change (a redraw replaces the row under the pointer,
  // and its click never arrives): returns whether it was.
  const pick = (r, shift, ctrl) => {
    const before = sel.marks.length > 1 ? sel.marks.join("\n") : "";
    sel = shift ? selectRange(sel, rows, r.path) : ctrl ? selectToggled(sel, r.path) : selectOnly(r.path);
    const after = sel.marks.length > 1 ? sel.marks.join("\n") : "";
    if (after === before) return false;
    render();
    return true;
  };
  // Whether a press or a key `e` (if any) holds Shift, or Ctrl (Cmd).
  const shiftOf = (e) => e !== undefined && e.shiftKey;
  const ctrlOf = (e) => e !== undefined && (e.ctrlKey || e.metaKey);
  const targets = (r) => targetsOf(sel, rows, r);
  const named = (list) => (list.length === 1 ? list[0].name : `${list.length} items`);
  const cutPending = (r) => { const c = clip; return c.kind === "cut" && c.paths.includes(r.path); };
  const render = () => {
    rows = tree();
    const idx = Math.max(0, rows.findIndex((r) => r.path === sel.cursor));
    panel.render(h("list", {
      key: "tree",
      items: rows,
      selected: idx,
      // Folders of the top row stay pinned (the UI kit's tree rows).
      depth: (r) => r.depth,
      marked: (r) => marked(r.path),
      // Files the ignore rules leave out, and what is cut and not yet
      // pasted, are shown dimmed.
      render: (r) => h("text", { mono: true, dim: r.ignored || cutPending(r) }, [" ".repeat(r.depth * 2) + (r.dir ? (open.has(r.path) ? "▾ " : "▸ ") : "  ") + r.name]),
      // Moving onto a file with the keys, or a click on it, shows it in the
      // preview tab; Enter or a double-click opens it for good. With Shift
      // or Ctrl held, the press or key selects instead.
      // A plain press on a row of the selection keeps the selection (it
      // may be dragged); its click selects the row alone.
      onSelect: (r, _i, how, e) => {
        const shift = shiftOf(e), ctrl = ctrlOf(e);
        if (how === "mouse" && !shift && !ctrl && marked(r.path)) { sel = cursorOn(sel, r.path); return; }
        const redrawn = pick(r, shift, ctrl);
        // A press that redrew the list loses its click: it acts here.
        if (how === "mouse" && redrawn && !shift && !ctrl && e !== undefined && e.button === 0) activate(r, "click");
        else if (how === "key" && !r.dir && !shift && !ctrl) showFile(r, true);
      },
      onActivate: (r, _i, how, e) => {
        if (shiftOf(e) || ctrlOf(e)) return;
        if (how === "click" && sel.marks.length > 1) pick(r, false, false);
        activate(r, how);
      },
      activateOnClick: true,
      onContext: (r, _i, at) => menu(r, at),
      onKey: (e, r) => key(e, r),
      // Drag a file or folder (or the selection it is in) onto a folder (or
      // a file in it, or the space below: the root) to move it there.
      canDrop: (r, onto) => {
        const to = dirOf(root, onto);
        return targets(r).every((x) => to !== parentOf(x.path) && to !== x.path && !to.startsWith(x.path + "/"));
      },
      onDrop: (r, onto) => ops.move(targets(r), dirOf(root, onto)),
    }));
  };
  const refresh = async () => {
    await load(root);
    for (const d of open) if (d !== root) await load(d);
    render();
  };
  // A folder opens or closes (on its first click); a file opens in the
  // preview tab on a click, and in a tab of its own, with the keyboard,
  // on a double-click or Enter.
  const activate = async (r, how) => {
    sel = cursorOn(sel, r.path);
    if (r.dir) {
      if (how === "double") return;
      if (open.has(r.path)) collapse(r.path);
      else await expand(r.path);
      render();
    } else showFile(r, how === "click");
  };
  const showFile = (r, preview) => fresh.open(app, r.path, { line: null, column: 1, place: "tab", preview, focus: !preview });
  const parentOf = (p) => p.replace(/\/[^/]*$/, "");
  const rel = (p) => (p === root ? "the workspace's root" : p.slice(root.length + 1));
  const forget = (path) => { for (const d of [...open]) if (d === path || d.startsWith(path + "/")) collapse(d); };
  // Run `f` on each of `list`, one after the other; says what failed.
  const each = async (list, f) => {
    const failed = [];
    for (const x of list) {
      try { await f(x); } catch (e) { failed.push(`${x.name}: ${fresh.errorText(e)}`); }
    }
    if (failed.length) fresh.message(app, `Not done: ${failed.join("; ")}`);
    return list.length - failed.length;
  };
  const ops = {
    newFile: async (dir) => {
      const name = await fresh.input(app, { title: "New file" });
      if (!name) return;
      const p = `${dir}/${name}`;
      await fresh.create(app, p, false);
      await expand(dir);
      sel = selectOnly(p);
      await refresh();
      fresh.open(app, p, { line: null, column: 1, place: "tab", preview: null, focus: true });
    },
    newFolder: async (dir) => {
      const name = await fresh.input(app, { title: "New folder" });
      if (!name) return;
      await fresh.create(app, `${dir}/${name}`, true);
      await expand(dir);
      await refresh();
    },
    rename: async (r) => {
      const name = await fresh.input(app, { title: "Rename to", value: r.name });
      if (!name || name === r.name) return;
      const to = r.path.replace(/[^/]*$/, name);
      await fresh.rename(app, r.path, to);
      sel = selectOnly(to);
      await refresh();
    },
    remove: async (list) => {
      if (!list.length) return;
      const folders = list.some((x) => x.dir);
      if (!(await fresh.confirm(app, `Delete ${named(list)}${folders ? " and everything in them" : ""}?`, "Delete"))) return;
      const n = await each(list, async (x) => { await fresh.remove(app, x.path); forget(x.path); });
      sel = NO_SEL;
      await refresh();
      if (n) fresh.message(app, `Deleted ${n === list.length ? named(list) : `${n} of ${list.length} items`}`);
    },
    copy: (list) => { if (list.length) { clip = copiedClip(list.map((x) => x.path)); render(); fresh.message(app, `Copied ${named(list)}`); } },
    cut: (list) => { if (list.length) { clip = cutClip(list.map((x) => x.path)); render(); fresh.message(app, `Cut ${named(list)}`); } },
    // Paste into folder `dest`: copies get a free name there; a cut moves
    // (back into its own folder: the cut is given up).
    paste: async (dest) => {
      const c = clip;
      if (c.kind === "none") return;
      const list = c.paths.map((p) => ({ path: p, name: p.slice(p.lastIndexOf("/") + 1) }));
      if (c.kind === "cut") {
        await each(list.filter((x) => parentOf(x.path) !== dest), async (x) => { await fresh.rename(app, x.path, `${dest}/${x.name}`); forget(x.path); });
        clip = NO_CLIP;
      } else await each(list, (x) => fresh.copy(app, x.path, dest));
      await expand(dest);
      await refresh();
    },
    copyPath: (list) => { if (list.length) navigator.clipboard.writeText(list.map((x) => x.path).join("\n")); },
    // Move into folder `to`, once confirmed (open files follow, as a rename).
    move: async (list, to) => {
      if (!(await fresh.confirm(app, `Move ${named(list)} into ${rel(to)}?`, "Move"))) return;
      const n = await each(list, async (x) => { await fresh.rename(app, x.path, `${to}/${x.name}`); forget(x.path); });
      await expand(to);
      sel = selectPaths(list.map((x) => `${to}/${x.name}`));
      await refresh();
      if (n) fresh.message(app, `Moved ${n === list.length ? named(list) : `${n} of ${list.length} items`} into ${rel(to)}`);
    },
  };
  // A row's menu acts on the selection when the row is in it.
  const menu = (r, at) => {
    const list = targets(r);
    const many = list.length > 1 ? ` (${list.length})` : "";
    fresh.menu(app, [
      fresh.menuAction("New File…", null, () => ops.newFile(dirOf(root, r))),
      fresh.menuAction("New Folder…", null, () => ops.newFolder(dirOf(root, r))),
      fresh.MENU_SEPARATOR,
      fresh.menuActionOf("Rename…", null, null, list.length !== 1, () => ops.rename(list[0])),
      fresh.menuAction(`Delete${many}`, "Delete", () => ops.remove(list)),
      fresh.MENU_SEPARATOR,
      fresh.menuAction(`Copy${many}`, "Ctrl+C", () => ops.copy(list)),
      fresh.menuAction(`Cut${many}`, "Ctrl+X", () => ops.cut(list)),
      fresh.menuActionOf("Paste", "Ctrl+V", null, clip.kind === "none", () => ops.paste(dirOf(root, r))),
      fresh.menuAction(`Copy Path${many}`, null, () => ops.copyPath(list)),
      fresh.MENU_SEPARATOR,
      fresh.menuAction("Select All", "Ctrl+A", selectAll),
      fresh.menuAction("Refresh", null, refresh),
    ], at);
  };
  const selectAll = () => { sel = selectPaths(rows.map((r) => r.path)); render(); };
  // Escape gives up the selection (the cursor row stays) and a pending cut.
  const clear = () => {
    const c = sel.cursor;
    sel = c === null ? NO_SEL : selectOnly(c);
    if (clip.kind === "cut") clip = NO_CLIP;
    render();
  };
  const key = (e, r) => {
    const k = (e.ctrlKey ? "C-" : "") + e.key;
    const keys = new Map([
      ["a", () => { ops.newFile(dirOf(root, r)); }], ["A", () => { ops.newFolder(dirOf(root, r)); }], ["C-n", () => { ops.newFile(dirOf(root, r)); }],
      ["F2", () => { if (r !== undefined) ops.rename(r); }], ["r", () => { if (r !== undefined) ops.rename(r); }],
      ["Delete", () => { ops.remove(targets(r)); }], ["C-c", () => { ops.copy(targets(r)); }], ["C-x", () => { ops.cut(targets(r)); }],
      ["C-v", () => { ops.paste(dirOf(root, r)); }], ["C-a", selectAll], ["Escape", clear],
      ["ArrowRight", () => { if (r !== undefined && r.dir && !open.has(r.path)) activate(r, "key"); }],
      ["ArrowLeft", () => { if (r !== undefined && r.dir && open.has(r.path)) activate(r, "key"); }],
    ]);
    const f = keys.get(k);
    if (f) { e.preventDefault(); f(); }
  };

  // Follow the active file: open its folders and select it.
  const reveal = async (path) => {
    if (path === null || !path.startsWith(root + "/")) return;
    let d = path.replace(/\/[^/]*$/, "");
    const chain = [];
    while (d.length > root.length) { chain.unshift(d); d = d.replace(/\/[^/]*$/, ""); }
    for (const dir of chain) if (!open.has(dir)) await expand(dir);
    sel = cursorOn(sel, path);
    render();
  };

  fresh.onBuffer(app, "buffer.activated", (buffer) => reveal(buffer.path));
  fresh.onBuffer(app, "buffer.saved", refresh);
  fresh.register(app, "explorer.focus", { title: "Explorer: Focus", hidden: false, run: () => panel.focus("tree") });
  fresh.register(app, "explorer.toggle", { title: "View: Toggle File Explorer", hidden: false, run: () => fresh.run(app, "view.toggleSidebar") });
  fresh.register(app, "explorer.newFile", { title: "Explorer: New File…", hidden: false, run: () => ops.newFile(root) });
  fresh.register(app, "explorer.refresh", { title: "Explorer: Refresh", hidden: false, run: refresh });
  // The same on the explorer's selected row (the menu bar's Explorer menu).
  const current = () => rows.find((r) => r.path === sel.cursor);
  fresh.register(app, "explorer.newFolder", { title: "Explorer: New Folder…", hidden: false, run: () => ops.newFolder(dirOf(root, current())) });
  fresh.register(app, "explorer.rename", { title: "Explorer: Rename…", hidden: false, run: () => { const r = current(); if (r !== undefined) ops.rename(r); } });
  fresh.register(app, "explorer.delete", { title: "Explorer: Delete", hidden: false, run: () => ops.remove(targets(current())) });
  fresh.register(app, "explorer.selectAll", { title: "Explorer: Select All", hidden: false, run: () => { selectAll(); panel.focus("tree"); } });
  fresh.register(app, "explorer.reveal", { title: "Explorer: Reveal Active File", hidden: false, run: async () => {
    fresh.run(app, "view.showSidebar");
    const b = fresh.activeBuffer(app);
    if (b) await reveal(b.path);
    panel.focus("tree");
  } });
  const setShow = (next) => {
    show = next;
    try { localStorage.setItem(SHOW, JSON.stringify(show)); } catch {}
    render();
  };
  fresh.register(app, "explorer.toggleHidden", { title: "Explorer: Toggle Hidden Files", hidden: false, run: () => setShow({ hidden: !show.hidden, ignored: show.ignored }) });
  fresh.register(app, "explorer.toggleIgnored", { title: "Explorer: Toggle Gitignored Files", hidden: false, run: () => setShow({ hidden: show.hidden, ignored: !show.ignored }) });
  fresh.addMenuItem(app, "Explorer", fresh.MENU_BAR_SEPARATOR);
  fresh.addMenuToggle(app, "Explorer", "explorer.toggleHidden", "Show Hidden Files", () => show.hidden);
  fresh.addMenuToggle(app, "Explorer", "explorer.toggleIgnored", "Show Gitignored Files", () => show.ignored);
  panel.actions([
    { icon: "+", title: "New File…", run: () => ops.newFile(dirOf(root, current())) },
    { icon: "⊞", title: "New Folder…", run: () => ops.newFolder(dirOf(root, current())) },
    { icon: "⟳", title: "Refresh", run: refresh },
    { icon: "⊟", title: "Collapse All", run: () => fresh.run(app, "explorer.collapseAll") },
  ]);
  fresh.register(app, "explorer.collapseAll", { title: "Explorer: Collapse All", hidden: false, run: () => { for (const d of [...open]) if (d !== root) collapse(d); render(); } });
  fresh.bindKey(app, "C-e", "explorer.focus");
  fresh.bindKey(app, "C-b", "explorer.toggle");
  expand(root).then(() => { render(); });
}
