// The editor area: the core's splits as data, laid out by the client. Each
// leaf is a pane with its own tabs and an editor view (or a plugin panel the
// core mounted in it). Dragging a tab moves it (onto a tab strip, a pane, or
// a pane's edge to split it); dragging a split's sash sets its ratio in the
// core; files dropped on a pane open there.

/** Pane DOM by leaf, kept across layout events so views survive. */
const panes = new Map();
/** A split being dragged: its ratio here until the core's arrives. */
const ratioDrag = new Map();

/** What a drag carries: a tab, or an explorer entry. */
const DRAG_TAB = "application/x-fresh-tab";
const DRAG_PATH = "application/x-fresh-path";

function paneFor(leaf) {
  let p = panes.get(leaf);
  if (p) return p;
  const root = div("pane");
  const tabs = div("tabs");
  tabs.setAttribute("role", "tablist");
  const body = div("pane-body");
  const panel = div("pane-panel");
  panel.hidden = true;
  const zone = div("drop-zone");
  zone.hidden = true;
  body.appendChild(panel);
  body.appendChild(zone);
  root.append(tabs, body);
  // A pane belongs to the window (workspace) it was built for, and every
  // request it sends names it: a request from a workspace switched away
  // from is refused by the core, not acted on in the same-numbered pane of
  // the new one.
  const win = layoutWindow;
  const view = new EditorView(leaf, win);
  body.insertBefore(view.root, panel);
  views.set(leaf, view);
  root.addEventListener("mousedown", () => {
    if (core.layout && core.layout.active !== leaf) api.send("split.focus", { window: win, leaf });
  }, true);
  p = { leaf, window: win, root, tabs, body, panel, view, zone };
  dropTarget(p);
  panes.set(leaf, p);
  return p;
}

/** Where on a pane's body a drop lands: an edge splits, the middle joins. */
function zoneAt(body, e) {
  const r = body.getBoundingClientRect();
  const x = (e.clientX - r.left) / r.width, y = (e.clientY - r.top) / r.height;
  const edge = 0.22;
  if (x < edge) return "left";
  if (x > 1 - edge) return "right";
  if (y < edge) return "top";
  if (y > 1 - edge) return "bottom";
  return "center";
}

function dropTarget(p) {
  const carries = (e) => [...e.dataTransfer.types].some((t) => t === DRAG_TAB || t === DRAG_PATH || t === "Files");
  // On the pane's body: a tab to a zone, a file into the pane.
  p.body.addEventListener("dragover", (e) => {
    if (!carries(e)) return;
    e.preventDefault();
    const isTab = [...e.dataTransfer.types].includes(DRAG_TAB);
    const z = isTab ? zoneAt(p.body, e) : "center";
    p.zone.hidden = false;
    p.zone.className = "drop-zone " + z;
  });
  p.body.addEventListener("dragleave", (e) => {
    if (!p.body.contains(e.relatedTarget)) p.zone.hidden = true;
  });
  p.body.addEventListener("drop", (e) => {
    p.zone.hidden = true;
    if (!carries(e)) return;
    e.preventDefault();
    const tab = e.dataTransfer.getData(DRAG_TAB);
    if (tab) {
      const t = JSON.parse(tab);
      api.send("tab.drop", { window: t.window, buffer: t.buffer, leaf: t.leaf, target: p.leaf, zone: zoneAt(p.body, e) });
      return;
    }
    dropFiles(p, e);
  });
  // On the tab strip: a tab between tabs, a file as a new tab.
  p.tabs.addEventListener("dragover", (e) => {
    if (!carries(e)) return;
    e.preventDefault();
    markInsert(p, e);
  });
  p.tabs.addEventListener("dragleave", () => markInsert(p, null));
  p.tabs.addEventListener("drop", (e) => {
    const index = markInsert(p, null, e);
    if (!carries(e)) return;
    e.preventDefault();
    const tab = e.dataTransfer.getData(DRAG_TAB);
    if (tab) {
      const t = JSON.parse(tab);
      api.send("tab.drop", { window: t.window, buffer: t.buffer, leaf: t.leaf, target: p.leaf, zone: "bar", index });
      return;
    }
    dropFiles(p, e);
  });
}

/** Show where a dropped tab would go; returns its index in the strip. */
function markInsert(p, e, at) {
  const tabs = [...p.tabs.querySelectorAll(".tab")];
  for (const t of tabs) t.classList.remove("insert-before", "insert-after");
  const ev = e || at;
  if (!ev) return null;
  let index = tabs.length;
  for (let i = 0; i < tabs.length; i++) {
    const r = tabs[i].getBoundingClientRect();
    if (ev.clientX < r.left + r.width / 2) { index = i; break; }
  }
  if (e) {
    if (index < tabs.length) tabs[index].classList.add("insert-before");
    else if (tabs.length) tabs[tabs.length - 1].classList.add("insert-after");
  }
  return tabs[index] ? Number(tabs[index].dataset.index) : null;
}

/** Files dropped on pane `p`: from the explorer by path, from the computer by contents. */
async function dropFiles(p, e) {
  const path = e.dataTransfer.getData(DRAG_PATH);
  if (core.layout && core.layout.active !== p.leaf) api.send("split.focus", { window: p.window, leaf: p.leaf });
  if (path) { api.send("file.open", { path }); return; }
  for (const f of e.dataTransfer.files) {
    // In the desktop app a dropped file has a path, and opens as itself.
    const local = window.freshDesktop && window.freshDesktop.pathForFile(f);
    if (local) { api.send("file.open", { path: local }); continue; }
    // A browser hands over contents, not paths: the file becomes a new,
    // unsaved buffer under its name, which Save As puts somewhere.
    if (f.size > 20 * 1024 * 1024) { showError(`${f.name} is too large to open from here`); continue; }
    const text = await f.text();
    if (text.includes("\u0000")) { showError(`${f.name} looks binary`); continue; }
    await api.call("buffer.new", { name: f.name, text }).catch(showError);
  }
}

function renderTabs(p, node) {
  const frag = [];
  node.tabs.forEach((id, index) => {
    const b = bufferById(id);
    if (!b || (b.hidden && id !== node.buffer)) return;
    const t = el("div", "tab" + (id === node.buffer ? " active" : ""));
    t.dataset.index = index;
    t.setAttribute("role", "tab");
    t.setAttribute("aria-selected", String(id === node.buffer));
    t.title = b.path || b.name;
    t.draggable = true;
    t.addEventListener("dragstart", (e) => {
      e.dataTransfer.setData(DRAG_TAB, JSON.stringify({ window: p.window, buffer: id, leaf: p.leaf }));
      e.dataTransfer.effectAllowed = "move";
    });
    const name = el("span", "tab-name", baseName(b.name));
    const mark = el("span", "tab-close", b.modified ? "●" : "×");
    mark.classList.toggle("modified", b.modified);
    mark.title = "Close";
    mark.addEventListener("mousedown", (e) => { e.stopPropagation(); e.preventDefault(); });
    const tab = { window: p.window, leaf: p.leaf, buffer: id };
    mark.addEventListener("click", (e) => { e.stopPropagation(); api.send("tab.close", tab); });
    t.addEventListener("mousedown", (e) => {
      if (e.button === 1) { e.preventDefault(); api.send("tab.close", tab); return; }
      if (e.button === 0) api.send("tab.show", tab);
    });
    // The terminal's tab menu: close this, others, to the side, all; copy
    // its path; move it to a workspace of its own.
    t.addEventListener("contextmenu", (e) => {
      e.preventDefault();
      contextMenu(e.clientX, e.clientY, coreMenuItems({ at: "tab", ...tab }));
    });
    t.append(name, mark);
    frag.push(t);
  });
  // The terminal's "+" menu: a new file or a new terminal in this pane.
  const add = el("button", "tab-new", "+");
  add.title = "New file or terminal";
  add.setAttribute("aria-label", "New file or terminal");
  add.setAttribute("aria-haspopup", "menu");
  add.addEventListener("mousedown", (e) => e.preventDefault());
  add.addEventListener("click", () => {
    const r = add.getBoundingClientRect();
    contextMenu(r.left, r.bottom + 2, coreMenuItems({ at: "newTab", window: p.window, leaf: p.leaf }));
  });
  frag.push(add);
  p.tabs.replaceChildren(...frag);
}

function layoutNode(node, live) {
  if (node.leaf !== undefined) {
    live.add(node.leaf);
    const p = paneFor(node.leaf);
    renderTabs(p, node);
    p.root.classList.toggle("active", core.layout.active === node.leaf);
    p.root.style.flex = "";
    return p.root;
  }
  if (node.group !== undefined) {
    const g = div("group");
    g.appendChild(el("div", "group-title", node.name));
    const inner = layoutNode(node.layout, live);
    inner.style.flex = "1";
    g.appendChild(inner);
    return g;
  }
  const box = div("split " + (node.stack ? "stack" : "side"));
  const a = layoutNode(node.a, live);
  const b = layoutNode(node.b, live);
  const ratio = ratioDrag.get(node.id) ?? node.ratio;
  a.style.flex = `${ratio} 1 0`;
  b.style.flex = `${1 - ratio} 1 0`;
  const sash = div("split-sash");
  sash.setAttribute("role", "separator");
  sash.setAttribute("aria-orientation", node.stack ? "horizontal" : "vertical");
  const win = layoutWindow;
  sash.addEventListener("mousedown", (e) => {
    e.preventDefault();
    const r = box.getBoundingClientRect();
    let v = ratio;
    const move = (m) => {
      const f = node.stack ? (m.clientY - r.top) / r.height : (m.clientX - r.left) / r.width;
      v = Math.min(0.95, Math.max(0.05, f));
      ratioDrag.set(node.id, v);
      a.style.flex = `${v} 1 0`;
      b.style.flex = `${1 - v} 1 0`;
    };
    const up = () => {
      window.removeEventListener("mousemove", move);
      window.removeEventListener("mouseup", up);
      // The ratio is the core's: the terminal's panes take it too.
      api.call("split.ratio", { window: win, split: node.id, ratio: v }).catch(showError)
        .finally(() => ratioDrag.delete(node.id));
    };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  });
  box.append(a, sash, b);
  return box;
}

/** The window (workspace) the panes are of: leaf ids are numbered per
 *  window, so another window's panes are all new ones. */
let layoutWindow = null;

function renderLayout() {
  const host = document.getElementById("editors");
  if (!core.layout || !core.layout.root) { host.replaceChildren(); return; }
  if (core.layout.window !== layoutWindow) {
    // The Orchestrator switched workspaces. Closing a view of the one left
    // has the core keep this client's scroll and cursors there for when it
    // comes back.
    layoutWindow = core.layout.window;
    for (const p of panes.values()) p.view.close();
    views.clear();
    panes.clear();
  }
  const live = new Set();
  const tree = layoutNode(core.layout.root, live);
  tree.style.flex = "1";
  host.replaceChildren(tree);
  for (const [leaf, p] of panes) {
    if (live.has(leaf)) continue;
    p.view.close();
    views.delete(leaf);
    panes.delete(leaf);
  }
  for (const v of views.values()) v.placeCaret();
  renderPanePanels();
}

api.on("layout", renderLayout);
api.on("buffers", renderLayout);

/** The view the keyboard edits in: the active pane's. */
function activeView() {
  return core.layout ? views.get(core.layout.active) : undefined;
}

/** A side column's sash: dragging it sets the column's width, measured from
 *  the column's own outer edge (so it holds in any frame, on either side). */
function columnSash(sash, column, min) {
  sash.addEventListener("mousedown", (e) => {
    e.preventDefault();
    const r = column.getBoundingClientRect();
    const onRight = r.left > sash.getBoundingClientRect().left;
    const app = document.getElementById("app").getBoundingClientRect();
    const move = (m) => {
      const w = onRight ? r.right - m.clientX : m.clientX - r.left;
      column.style.width = Math.max(min, Math.min(app.width * 0.6, w)) + "px";
    };
    const up = () => { window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
  });
}
columnSash(document.getElementById("dock-sash"), document.getElementById("dock"), 200);
