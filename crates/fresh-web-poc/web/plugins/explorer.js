// The file explorer: the workspace's tree in the sidebar, gitignore-aware
// (as the server lists it), with file operations through the server.
//
// Keys in the tree: Enter open/expand, Left/Right collapse/expand,
// a or C-n new file, A new folder, F2 or r rename, Delete remove, C-c copy,
// C-x cut, C-v paste, C-a select all, Escape clear the selection. Ctrl+click
// and Shift+click (Shift+Up/Down) select several; deleting, copying,
// cutting, dragging and the right-click menu act on all of them.

export function activate(fresh) {
  const { h } = fresh;
  const root = fresh.workspace.root;
  const open = new Set();
  const children = new Map(); // dir -> entries
  let rows = [];
  let selected = null; // path
  let clip = null; // {paths, cut}
  // What the tree leaves out, as the editor's explorer: dotfiles and files
  // the ignore rules leave out, until shown (then ignored ones are dimmed).
  const SHOW = "fresh-poc:explorer";
  let show = { hidden: false, ignored: false };
  try { show = { ...show, ...JSON.parse(localStorage.getItem(SHOW) || "{}") }; } catch {}
  const visible = (e) => (show.hidden || !e.name.startsWith(".")) && (show.ignored || !e.ignored);
  const panel = fresh.ui.panel({ id: "explorer", title: "Explorer", place: "sidebar", onClose: null });

  const load = async (dir) => {
    try { children.set(dir, await fresh.host.list(dir)); } catch (e) { children.set(dir, []); }
  };
  // Every shown folder is watched, so files made or removed outside the
  // editor (a terminal, git, another client) appear.
  const watching = new Map(); // dir -> stop
  const expand = async (dir) => {
    open.add(dir);
    await load(dir);
    if (watching.has(dir)) return;
    watching.set(dir, null);
    const stop = await fresh.host.watch(dir, async () => { await load(dir); render(); });
    if (watching.has(dir)) watching.set(dir, stop); else stop();
  };
  const collapse = (dir) => {
    open.delete(dir);
    const stop = watching.get(dir);
    watching.delete(dir);
    stop?.();
  };
  const flatten = () => {
    rows = [];
    const walk = (dir, depth) => {
      for (const e of children.get(dir) || []) {
        if (!visible(e)) continue;
        rows.push({ ...e, depth });
        if (e.dir && open.has(e.path)) walk(e.path, depth + 1);
      }
    };
    walk(root, 0);
  };
  // The selection: the rows marked (Ctrl+click, Shift+click,
  // Shift+arrows, Ctrl+A), the anchor a range is extended from, and the
  // cursor row (`selected`). With none marked, the cursor row is the
  // selection.
  let marks = new Set();
  let anchor = null;
  const range = (a, b) => {
    const i = rows.findIndex((r) => r.path === a), j = rows.findIndex((r) => r.path === b);
    if (i < 0 || j < 0) return new Set([b]);
    return new Set(rows.slice(Math.min(i, j), Math.max(i, j) + 1).map((r) => r.path));
  };
  // Select row `r` as a press or a key with modifiers `e` asks: alone,
  // toggled (Ctrl), or the range from the anchor (Shift).
  // The list is drawn again only when the marked rows change (a redraw
  // replaces the row under the pointer, and its click never arrives):
  // returns whether it was.
  const pick = (r, e) => {
    const shown = () => (marks.size > 1 ? [...marks].join("\n") : "");
    const before = shown();
    if (e && e.shiftKey && anchor) marks = range(anchor, r.path);
    else if (e && (e.ctrlKey || e.metaKey)) {
      if (!marks.size && selected) marks.add(selected);
      if (marks.has(r.path)) marks.delete(r.path); else marks.add(r.path);
      anchor = r.path;
    } else { marks = new Set([r.path]); anchor = r.path; }
    selected = r.path;
    if (shown() === before) return false;
    render();
    return true;
  };
  // What an action on row `r` acts on: the marked rows when `r` is one of
  // them (in tree order, leaving out what a marked folder holds), else `r`.
  const targets = (r) => {
    if (!r) return [];
    if (marks.size < 2 || !marks.has(r.path)) return [r];
    const out = [];
    for (const x of rows) if (marks.has(x.path) && !out.some((o) => o.dir && x.path.startsWith(o.path + "/"))) out.push(x);
    return out;
  };
  const named = (list) => (list.length === 1 ? list[0].name : `${list.length} items`);
  const cutPending = (r) => !!clip && clip.cut && clip.paths.includes(r.path);
  const render = () => {
    flatten();
    const idx = Math.max(0, rows.findIndex((r) => r.path === selected));
    panel.render(h("list", {
      key: "tree",
      items: rows,
      selected: idx,
      // Folders of the top row stay pinned (the UI kit's tree rows).
      depth: (r) => r.depth,
      marked: (r) => marks.size > 1 && marks.has(r.path),
      // Files the ignore rules leave out, and what is cut and not yet
      // pasted, are shown dimmed.
      render: (r) => h("text", { mono: true, dim: r.ignored || cutPending(r) }, [" ".repeat(r.depth * 2) + (r.dir ? (open.has(r.path) ? "▾ " : "▸ ") : "  ") + r.name]),
      // Moving onto a file with the keys, or a click on it, shows it in the
      // preview tab; Enter or a double-click opens it for good. With Shift
      // or Ctrl held, the press or key selects instead.
      // A plain press on a row of the selection keeps the selection (it
      // may be dragged); its click selects the row alone.
      onSelect: (r, _i, how, e) => {
        const mods = !!e && (e.shiftKey || e.ctrlKey || e.metaKey);
        if (how === "mouse" && !mods && marks.size > 1 && marks.has(r.path)) { selected = r.path; return; }
        const redrawn = pick(r, e);
        // A press that redrew the list loses its click: it acts here.
        if (how === "mouse" && redrawn && !mods && e.button === 0) activate(r, "click");
        else if (how === "key" && !r.dir && !mods) showFile(r, true);
      },
      onActivate: (r, _i, how, e) => {
        if (e && (e.shiftKey || e.ctrlKey || e.metaKey)) return;
        if (how === "click" && marks.size > 1) pick(r, null);
        activate(r, how);
      },
      activateOnClick: true,
      onContext: (r, _i, at) => menu(r, at),
      onKey: (e, r) => key(e, r),
      // Drag a file or folder (or the selection it is in) onto a folder (or
      // a file in it, or the space below: the root) to move it there.
      canDrop: (r, onto) => {
        const to = dirOf(onto);
        return targets(r).every((x) => to !== parentOf(x.path) && to !== x.path && !to.startsWith(x.path + "/"));
      },
      onDrop: (r, onto) => ops.move(targets(r), dirOf(onto)),
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
    if (!r) return;
    selected = r.path;
    if (r.dir) {
      if (how === "double") return;
      if (open.has(r.path)) collapse(r.path);
      else await expand(r.path);
      render();
    } else showFile(r, how === "click");
  };
  const showFile = (r, preview) => fresh.buffers.open(r.path, { line: null, column: 1, place: "tab", preview, focus: !preview });
  const dirOf = (r) => (r ? (r.dir ? r.path : r.path.replace(/\/[^/]*$/, "")) : root);
  const parentOf = (p) => p.replace(/\/[^/]*$/, "");
  const rel = (p) => (p === root ? "the workspace's root" : p.slice(root.length + 1));
  const forget = (path) => { for (const d of [...open]) if (d === path || d.startsWith(path + "/")) collapse(d); };
  // Run `f` on each of `list`, one after the other; says what failed.
  const each = async (list, f) => {
    const failed = [];
    for (const x of list) {
      try { await f(x); } catch (e) { failed.push(`${x.name}: ${e.detail || e.message || e}`); }
    }
    if (failed.length) fresh.ui.message(`Not done: ${failed.join("; ")}`);
    return list.length - failed.length;
  };
  const ops = {
    newFile: async (r) => {
      const name = await fresh.ui.input({ title: "New file" });
      if (!name) return;
      const p = `${dirOf(r)}/${name}`;
      await fresh.host.create(p, false);
      await expand(dirOf(r));
      selected = p;
      await refresh();
      fresh.buffers.open(p, { line: null, column: 1, place: "tab", preview: null, focus: true });
    },
    newFolder: async (r) => {
      const name = await fresh.ui.input({ title: "New folder" });
      if (!name) return;
      await fresh.host.create(`${dirOf(r)}/${name}`, true);
      await expand(dirOf(r));
      await refresh();
    },
    rename: async (r) => {
      if (!r) return;
      const name = await fresh.ui.input({ title: "Rename to", value: r.name });
      if (!name || name === r.name) return;
      const to = r.path.replace(/[^/]*$/, name);
      await fresh.host.rename(r.path, to);
      selected = to;
      marks = new Set([to]);
      await refresh();
    },
    remove: async (list) => {
      if (!list.length) return;
      const folders = list.some((x) => x.dir);
      if (!(await fresh.ui.confirm(`Delete ${named(list)}${folders ? " and everything in them" : ""}?`, "Delete"))) return;
      const n = await each(list, async (x) => { await fresh.host.remove(x.path); forget(x.path); });
      marks = new Set();
      await refresh();
      if (n) fresh.ui.message(`Deleted ${n === list.length ? named(list) : `${n} of ${list.length} items`}`);
    },
    copy: (list) => { if (list.length) { clip = { paths: list.map((x) => x.path), cut: false }; render(); fresh.ui.message(`Copied ${named(list)}`); } },
    cut: (list) => { if (list.length) { clip = { paths: list.map((x) => x.path), cut: true }; render(); fresh.ui.message(`Cut ${named(list)}`); } },
    // Paste into the folder of `r`: copies get a free name there; a cut
    // moves (back into its own folder: the cut is given up).
    paste: async (r) => {
      if (!clip) return;
      const dest = dirOf(r);
      const list = clip.paths.map((p) => ({ path: p, name: p.split("/").pop() }));
      if (clip.cut) {
        await each(list.filter((x) => parentOf(x.path) !== dest), async (x) => { await fresh.host.rename(x.path, `${dest}/${x.name}`); forget(x.path); });
        clip = null;
      } else await each(list, (x) => fresh.host.copy(x.path, dest));
      await expand(dest);
      await refresh();
    },
    copyPath: (list) => list.length && navigator.clipboard?.writeText(list.map((x) => x.path).join("\n")),
    // Move into folder `to`, once confirmed (open files follow, as a rename).
    move: async (list, to) => {
      if (!(await fresh.ui.confirm(`Move ${named(list)} into ${rel(to)}?`, "Move"))) return;
      const n = await each(list, async (x) => { await fresh.host.rename(x.path, `${to}/${x.name}`); forget(x.path); });
      await expand(to);
      if (list.length === 1) selected = `${to}/${list[0].name}`;
      marks = new Set(list.map((x) => `${to}/${x.name}`));
      await refresh();
      if (n) fresh.ui.message(`Moved ${n === list.length ? named(list) : `${n} of ${list.length} items`} into ${rel(to)}`);
    },
  };
  // A row's menu acts on the selection when the row is in it.
  const menu = (r, at) => {
    const list = targets(r);
    const many = list.length > 1 ? ` (${list.length})` : "";
    fresh.ui.menu([
      { label: "New File…", run: () => ops.newFile(r) },
      { label: "New Folder…", run: () => ops.newFolder(r) },
      null,
      { label: "Rename…", disabled: list.length !== 1, run: () => ops.rename(r) },
      { label: `Delete${many}`, keys: "Delete", run: () => ops.remove(list) },
      null,
      { label: `Copy${many}`, keys: "Ctrl+C", run: () => ops.copy(list) },
      { label: `Cut${many}`, keys: "Ctrl+X", run: () => ops.cut(list) },
      { label: "Paste", keys: "Ctrl+V", disabled: !clip, run: () => ops.paste(r) },
      { label: `Copy Path${many}`, run: () => ops.copyPath(list) },
      null,
      { label: "Select All", keys: "Ctrl+A", run: selectAll },
      { label: "Refresh", run: refresh },
    ], at);
  };
  const selectAll = () => { marks = new Set(rows.map((r) => r.path)); anchor = rows[0]?.path || null; render(); };
  // Escape gives up the selection and a pending cut.
  const clear = () => { marks = new Set(); if (clip && clip.cut) clip = null; render(); };
  const key = (e, r) => {
    const k = (e.ctrlKey ? "C-" : "") + e.key;
    const map = {
      a: () => ops.newFile(r), A: () => ops.newFolder(r), "C-n": () => ops.newFile(r), F2: () => ops.rename(r), r: () => ops.rename(r),
      Delete: () => ops.remove(targets(r)), "C-c": () => ops.copy(targets(r)), "C-x": () => ops.cut(targets(r)), "C-v": () => ops.paste(r),
      "C-a": selectAll, Escape: clear,
      ArrowRight: () => r?.dir && !open.has(r.path) && activate(r, "key"), ArrowLeft: () => r?.dir && open.has(r.path) && activate(r, "key"),
    };
    if (map[k]) { e.preventDefault(); map[k](); }
  };

  // Follow the active file: open its folders and select it.
  const reveal = async (path) => {
    if (!path || !path.startsWith(root + "/")) return;
    let d = path.replace(/\/[^/]*$/, "");
    const chain = [];
    while (d.length > root.length) { chain.unshift(d); d = d.replace(/\/[^/]*$/, ""); }
    for (const dir of chain) if (!open.has(dir)) await expand(dir);
    selected = path;
    render();
  };

  fresh.events.on("buffer.activated", ({ buffer }) => reveal(buffer.path));
  fresh.events.on("buffer.saved", refresh);
  fresh.commands.register("explorer.focus", { title: "Explorer: Focus", hidden: false, run: () => panel.focus("tree") });
  fresh.commands.register("explorer.toggle", { title: "View: Toggle File Explorer", hidden: false, run: () => fresh.commands.run("view.toggleSidebar") });
  fresh.commands.register("explorer.newFile", { title: "Explorer: New File…", hidden: false, run: () => ops.newFile(null) });
  fresh.commands.register("explorer.refresh", { title: "Explorer: Refresh", hidden: false, run: refresh });
  // The same on the explorer's selected row (the menu bar's Explorer menu).
  const current = () => rows.find((r) => r.path === selected) || null;
  fresh.commands.register("explorer.newFolder", { title: "Explorer: New Folder…", hidden: false, run: () => ops.newFolder(current()) });
  fresh.commands.register("explorer.rename", { title: "Explorer: Rename…", hidden: false, run: () => ops.rename(current()) });
  fresh.commands.register("explorer.delete", { title: "Explorer: Delete", hidden: false, run: () => ops.remove(targets(current())) });
  fresh.commands.register("explorer.selectAll", { title: "Explorer: Select All", hidden: false, run: () => { selectAll(); panel.focus("tree"); } });
  fresh.commands.register("explorer.reveal", { title: "Explorer: Reveal Active File", hidden: false, run: async () => {
    document.querySelector("#sidebar").classList.remove("hidden");
    await reveal(fresh.buffers.active()?.path);
    panel.focus("tree");
  } });
  const toggleShow = (k) => {
    show[k] = !show[k];
    try { localStorage.setItem(SHOW, JSON.stringify(show)); } catch {}
    render();
  };
  fresh.commands.register("explorer.toggleHidden", { title: "Explorer: Toggle Hidden Files", hidden: false, run: () => toggleShow("hidden") });
  fresh.commands.register("explorer.toggleIgnored", { title: "Explorer: Toggle Gitignored Files", hidden: false, run: () => toggleShow("ignored") });
  fresh.menus.add("Explorer", null);
  fresh.menus.add("Explorer", { command: "explorer.toggleHidden", label: "Show Hidden Files", checked: () => show.hidden });
  fresh.menus.add("Explorer", { command: "explorer.toggleIgnored", label: "Show Gitignored Files", checked: () => show.ignored });
  panel.actions([
    { icon: "+", title: "New File…", run: () => ops.newFile(current()) },
    { icon: "⊞", title: "New Folder…", run: () => ops.newFolder(current()) },
    { icon: "⟳", title: "Refresh", run: refresh },
    { icon: "⊟", title: "Collapse All", run: () => fresh.commands.run("explorer.collapseAll") },
  ]);
  fresh.commands.register("explorer.collapseAll", { title: "Explorer: Collapse All", hidden: false, run: () => { for (const d of [...open]) if (d !== root) collapse(d); render(); } });
  fresh.keys.bind("C-e", "explorer.focus");
  fresh.keys.bind("C-b", "explorer.toggle");
  expand(root).then(render);
}
