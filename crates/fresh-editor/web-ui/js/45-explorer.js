// The file explorer: the client's own tree over the core's filesystem
// (`dir.list`, local or remote alike). Opening, renaming, creating and
// deleting go to the core; expansion, selection and the context menu are
// the client's.

const explorer = {
  expanded: new Set(),
  children: new Map(),
  selected: null,
  shown: true,
};

async function loadDir(path) {
  try {
    const r = await api.call("dir.list", { path });
    explorer.children.set(path, r.entries);
  } catch (e) {
    explorer.children.set(path, []);
  }
}

async function refreshDir(path) {
  await loadDir(path);
  renderExplorer();
}

function renderExplorer() {
  const host = document.getElementById("explorer");
  const sidebar = document.getElementById("sidebar");
  sidebar.hidden = !explorer.shown;
  document.getElementById("sash").hidden = !explorer.shown;
  if (!core.root) return;
  const active = bufferById(core.status && core.status.buffer);
  const activePath = active && active.path;
  const rows = [];
  const head = el("div", "ex-head", baseName(core.root).toUpperCase());
  head.title = core.root;
  const walk = (dir, depth) => {
    const entries = explorer.children.get(dir);
    if (!entries) return;
    for (const e of entries) {
      const path = joinPath(dir, e.name);
      const open = explorer.expanded.has(path);
      const row = div("ex-row" + (explorer.selected === path ? " selected" : "") + (path === activePath ? " current" : ""));
      row.setAttribute("role", "treeitem");
      if (e.dir) row.setAttribute("aria-expanded", String(open));
      row.style.paddingLeft = 8 + depth * 14 + "px";
      row.append(el("span", "ex-twisty", e.dir ? (open ? "▾" : "▸") : ""), el("span", "ex-name" + (e.dir ? " dir" : ""), e.name));
      row.dataset.path = path;
      row.dataset.dir = e.dir ? "1" : "";
      row.dataset.parent = dir;
      row.setAttribute("aria-selected", String(explorer.selected === path));
      row.setAttribute("aria-level", String(depth + 1));
      if (!e.dir) {
        row.draggable = true;
        row.addEventListener("dragstart", (ev) => {
          ev.dataTransfer.setData(DRAG_PATH, path);
          ev.dataTransfer.effectAllowed = "copy";
        });
      }
      row.addEventListener("click", () => {
        document.getElementById("explorer").focus({ preventScroll: true });
        activateEntry(path, e.dir);
      });
      row.addEventListener("contextmenu", (ev) => { ev.preventDefault(); explorer.selected = path; renderExplorer(); entryMenu(ev, path, e.dir, dir); });
      rows.push(row);
      if (e.dir && open) walk(path, depth + 1);
    }
  };
  walk(core.root, 0);
  head.addEventListener("contextmenu", (ev) => { ev.preventDefault(); entryMenu(ev, core.root, true, null); });
  host.replaceChildren(head, ...rows);
}

async function activateEntry(path, isDir) {
  explorer.selected = path;
  if (isDir) {
    if (explorer.expanded.has(path)) explorer.expanded.delete(path);
    else {
      explorer.expanded.add(path);
      if (!explorer.children.has(path)) await loadDir(path);
    }
    renderExplorer();
    return;
  }
  renderExplorer();
  api.send("file.open", { path });
}

/** The explorer's context menu for one entry. */
async function entryMenu(ev, path, isDir, parent) {
  const dir = isDir ? path : parent;
  const items = [
    { label: "New File…", run: async () => {
      const name = await askText("New file in " + baseName(dir), "");
      if (!name) return;
      const p = joinPath(dir, name);
      await api.call("file.create", { path: p, dir: false }).catch(showError);
      explorer.expanded.add(dir);
      await refreshDir(dir);
      api.send("file.open", { path: p });
    } },
    { label: "New Folder…", run: async () => {
      const name = await askText("New folder in " + baseName(dir), "");
      if (!name) return;
      await api.call("file.create", { path: joinPath(dir, name), dir: true }).catch(showError);
      explorer.expanded.add(dir);
      await refreshDir(dir);
    } },
  ];
  if (parent !== null) {
    items.push({ label: "Rename…", run: async () => {
      const name = await askText("Rename " + baseName(path), baseName(path));
      if (!name || name === baseName(path)) return;
      await api.call("file.rename", { from: path, to: joinPath(parent, name) }).catch(showError);
      await refreshDir(parent);
    } });
    items.push({ label: "Delete", run: async () => {
      if (!(await askConfirm(`Delete ${baseName(path)}?`, isDir ? "The folder must be empty." : "This cannot be undone.", "Delete"))) return;
      await api.call("file.delete", { path }).catch(showError);
      await refreshDir(parent);
    } });
  }
  items.push({ label: "Refresh", run: () => refreshDir(dir) });
  // Then what plugins offer for this entry (the terminal's explorer menu
  // shows them after its own, too).
  const plugin = parent !== null ? await coreMenuItems({ at: "explorer", path, dir: isDir }) : [];
  contextMenu(ev.clientX, ev.clientY, plugin.length ? [...items, null, ...plugin] : items);
}

function toggleExplorer() {
  explorer.shown = !explorer.shown;
  renderExplorer();
}

function focusExplorer() {
  explorer.shown = true;
  renderExplorer();
  document.getElementById("explorer").focus();
}

// The tree is the workspace's: a new root (the first, or the one the
// Orchestrator switched to) is listed afresh. Expanded folders are kept by
// path, so going back to a workspace finds its tree as it was left.
api.on("root", async () => {
  const root = core.root;
  explorer.children.clear();
  if (explorer.selected && !explorer.selected.startsWith(root)) explorer.selected = null;
  await loadDir(root);
  // A later switch has its own listing on the way.
  if (core.root === root) renderExplorer();
});
api.on("status", () => {
  // Keep the open file marked.
  const host = document.getElementById("explorer");
  if (host.childElementCount) renderExplorer();
});

columnSash(document.getElementById("sash"), document.getElementById("sidebar"), 140);
document.getElementById("explorer").tabIndex = 0;

// The tree from the keyboard: Up/Down move, Right opens a folder (or steps
// into it), Left closes it (or steps out), Enter opens.
document.getElementById("explorer").addEventListener("keydown", async (e) => {
  const rows = [...document.querySelectorAll("#explorer .ex-row")];
  if (!rows.length) return;
  let i = rows.findIndex((r) => r.dataset.path === explorer.selected);
  const row = rows[i];
  const select = (r) => {
    if (!r) return;
    explorer.selected = r.dataset.path;
    renderExplorer();
    const now = document.querySelector(`#explorer .ex-row[data-path="${CSS.escape(r.dataset.path)}"]`);
    if (now) now.scrollIntoView({ block: "nearest" });
  };
  const key = e.key;
  if (!["ArrowDown", "ArrowUp", "ArrowRight", "ArrowLeft", "Enter", "Home", "End"].includes(key)) return;
  e.preventDefault();
  if (key === "ArrowDown") return select(rows[Math.min(rows.length - 1, i + 1)]);
  if (key === "ArrowUp") return select(rows[Math.max(0, i - 1)]);
  if (key === "Home") return select(rows[0]);
  if (key === "End") return select(rows[rows.length - 1]);
  if (!row) return select(rows[0]);
  const path = row.dataset.path, isDir = !!row.dataset.dir;
  if (key === "Enter") {
    await activateEntry(path, isDir);
    if (!isDir) focusKeyboard();
    return;
  }
  if (key === "ArrowRight" && isDir) {
    if (!explorer.expanded.has(path)) await activateEntry(path, true);
    else select(rows[i + 1]);
    return;
  }
  if (key === "ArrowLeft") {
    if (isDir && explorer.expanded.has(path)) { explorer.expanded.delete(path); renderExplorer(); return; }
    select(rows.find((r) => r.dataset.path === row.dataset.parent));
  }
});
