// The file explorer: the workspace's tree in the sidebar, gitignore-aware
// (as the server lists it), with file operations through the server.
//
// Keys in the tree: Enter open/expand, Left/Right collapse/expand,
// a new file, A new folder, F2 or r rename, Delete remove, C-c copy,
// C-x cut, C-v paste. Right-click for the same as a menu.

export function activate(fresh) {
  const { h } = fresh;
  const root = fresh.workspace.root;
  const open = new Set([root]);
  const children = new Map(); // dir -> entries
  let rows = [];
  let selected = null; // path
  let clip = null; // {path, cut}
  const panel = fresh.ui.panel({ id: "explorer", title: "Explorer", place: "sidebar" });

  const load = async (dir) => {
    try { children.set(dir, await fresh.host.list(dir)); } catch (e) { children.set(dir, []); }
  };
  const flatten = () => {
    rows = [];
    const walk = (dir, depth) => {
      for (const e of children.get(dir) || []) {
        rows.push({ ...e, depth });
        if (e.dir && open.has(e.path)) walk(e.path, depth + 1);
      }
    };
    walk(root, 0);
  };
  const render = () => {
    flatten();
    const idx = Math.max(0, rows.findIndex((r) => r.path === selected));
    panel.render(h("list", {
      key: "tree",
      items: rows,
      selected: idx,
      render: (r) => h("text", { mono: true }, " ".repeat(r.depth * 2) + (r.dir ? (open.has(r.path) ? "▾ " : "▸ ") : "  ") + r.name),
      onSelect: (r) => { selected = r.path; },
      onActivate: (r) => activate(r),
      activateOnClick: true,
      onContext: (r, _i, at) => menu(r, at),
      onKey: (e, r) => key(e, r),
    }));
  };
  const refresh = async () => {
    await load(root);
    for (const d of open) if (d !== root) await load(d);
    render();
  };
  const activate = async (r) => {
    if (!r) return;
    selected = r.path;
    if (r.dir) {
      if (open.has(r.path)) open.delete(r.path);
      else { open.add(r.path); await load(r.path); }
      render();
    } else fresh.buffers.open(r.path);
  };
  const dirOf = (r) => (r ? (r.dir ? r.path : r.path.replace(/\/[^/]*$/, "")) : root);
  const ops = {
    newFile: async (r) => {
      const name = await fresh.ui.input({ title: "New file" });
      if (!name) return;
      const p = `${dirOf(r)}/${name}`;
      await fresh.host.create(p, false);
      open.add(dirOf(r));
      selected = p;
      await refresh();
      fresh.buffers.open(p);
    },
    newFolder: async (r) => {
      const name = await fresh.ui.input({ title: "New folder" });
      if (!name) return;
      await fresh.host.create(`${dirOf(r)}/${name}`, true);
      open.add(dirOf(r));
      await refresh();
    },
    rename: async (r) => {
      if (!r) return;
      const name = await fresh.ui.input({ title: "Rename to", value: r.name });
      if (!name || name === r.name) return;
      const to = r.path.replace(/[^/]*$/, name);
      await fresh.host.rename(r.path, to);
      selected = to;
      await refresh();
    },
    remove: async (r) => {
      if (!r) return;
      const ok = await fresh.ui.input({ title: `Delete ${r.name}${r.dir ? " and everything in it" : ""}? Type yes`, value: "" });
      if (ok !== "yes") return;
      await fresh.host.remove(r.path);
      await refresh();
      fresh.ui.message(`Deleted ${r.name}`);
    },
    copy: (r) => { if (r) { clip = { path: r.path, cut: false }; fresh.ui.message(`Copied ${r.name}`); } },
    cut: (r) => { if (r) { clip = { path: r.path, cut: true }; fresh.ui.message(`Cut ${r.name}`); } },
    paste: async (r) => {
      if (!clip) return;
      const dest = dirOf(r);
      if (clip.cut) await fresh.host.rename(clip.path, `${dest}/${clip.path.split("/").pop()}`);
      else await fresh.host.copy(clip.path, dest);
      if (clip.cut) clip = null;
      open.add(dest);
      await refresh();
    },
    copyPath: (r) => r && navigator.clipboard?.writeText(r.path),
  };
  const menu = (r, at) => fresh.ui.menu([
    { label: "New File…", run: () => ops.newFile(r) },
    { label: "New Folder…", run: () => ops.newFolder(r) },
    null,
    { label: "Rename…", run: () => ops.rename(r) },
    { label: "Delete", run: () => ops.remove(r) },
    null,
    { label: "Copy", run: () => ops.copy(r) },
    { label: "Cut", run: () => ops.cut(r) },
    { label: "Paste", run: () => ops.paste(r) },
    { label: "Copy Path", run: () => ops.copyPath(r) },
    null,
    { label: "Refresh", run: refresh },
  ], at);
  const key = (e, r) => {
    const k = (e.ctrlKey ? "C-" : "") + e.key;
    const map = {
      a: () => ops.newFile(r), A: () => ops.newFolder(r), F2: () => ops.rename(r), r: () => ops.rename(r),
      Delete: () => ops.remove(r), "C-c": () => ops.copy(r), "C-x": () => ops.cut(r), "C-v": () => ops.paste(r),
      ArrowRight: () => r?.dir && !open.has(r.path) && activate(r), ArrowLeft: () => r?.dir && open.has(r.path) && activate(r),
      Escape: () => fresh.commands.run("explorer.leave"),
    };
    if (map[k]) { e.preventDefault(); map[k](); }
  };

  // Follow the active file: open its folders and select it.
  const reveal = async (path) => {
    if (!path || !path.startsWith(root + "/")) return;
    let d = path.replace(/\/[^/]*$/, "");
    const chain = [];
    while (d.length > root.length) { chain.unshift(d); d = d.replace(/\/[^/]*$/, ""); }
    for (const dir of chain) if (!open.has(dir)) { open.add(dir); await load(dir); }
    selected = path;
    render();
  };

  fresh.events.on("buffer.activated", ({ buffer }) => reveal(buffer.path));
  fresh.events.on("buffer.saved", refresh);
  fresh.commands.register("explorer.focus", { title: "Explorer: Focus", run: () => panel.focus("tree") });
  fresh.commands.register("explorer.leave", { title: "Explorer: Back to Editor", run: () => fresh.commands.run("view.focusEditor"), hidden: true });
  fresh.commands.register("explorer.toggle", { title: "View: Toggle File Explorer", run: () => document.querySelector("#sidebar").classList.toggle("hidden") });
  fresh.commands.register("explorer.newFile", { title: "File: New File…", run: () => ops.newFile(null) });
  fresh.commands.register("explorer.refresh", { title: "Explorer: Refresh", run: refresh });
  fresh.keys.bind("C-e", "explorer.focus");
  fresh.keys.bind("C-b", "explorer.toggle");
  refresh();
}
