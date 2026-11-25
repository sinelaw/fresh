// The panel UI kit for plugins (and the client's own features).
//
// A plugin asks for a panel in a place — the sidebar, the bottom dock, a
// column beside the editor, a bar above it, or a dialog — and renders a
// declarative tree into it, built with `h(type, props, children)`:
//
//   panel.render(h("col", {}, [
//     h("input", {key: "q", value: q, placeholder: "Search", onInput: (v) => ...}),
//     h("list", {key: "hits", items, render: (it) => it.text, onActivate: (it) => ...}),
//   ]));
//
// Controls are real DOM controls, so focus, text editing, IME and
// accessibility are the browser's. Re-rendering rebuilds the panel and puts
// focus and the caret back on the control with the same key. Lists are
// virtualised (any number of items) and keyboard-navigable.

import { $, setClass, escapeHtml } from "./dom.js";
import { color } from "./highlight.js";
import { makeMenu, menuSpec } from "./menu.js";

// `children` is one array of nodes and strings; nested arrays are
// flattened, and null and false left out (for conditional parts).
export function h(type, props, children = []) {
  return { type, props: props || {}, children: children.flat(Infinity).filter((c) => c != null && c !== false) };
}

const ROW_H = 22;

export class UIKit {
  constructor(app) {
    this.app = app;
    this.panels = new Map();
    this.dock = $("#dock");
    this.dockTabs = this.dock.querySelector(".dock-tabs");
    this.dockBody = this.dock.querySelector(".dock-body");
    this.side = $("#side-panels");
    this.right = $("#right-dock");
    this.left = $("#left-dock");
    this.bar = $("#bar");
    this.activeDock = null;
    this.contexts = []; // the open context menu: a list of at most one
  }

  // A PanelOptions (types/fresh.d.js): place "bottom" | "sidebar" | "left"
  // (a column before the sidebar) | "right" | "bar" | "dialog".
  panel({ id, title, place, onClose }) {
    let p = this.panels.get(id);
    if (p) { p.show(); return p; }
    p = new Panel(this, id, title, place, onClose);
    p.listen();
    this.panels.set(id, p);
    p.show();
    return p;
  }

  // The panel `id` if it is on screen, else undefined (never makes one).
  shown(id) {
    return this.panels.get(id);
  }

  // A context menu of MenuItems (menu.js) at {x, y}; it has the keyboard
  // until it closes. One is open at a time.
  menu(items, at) {
    this.contextMenu(items, () => {}).openAt(at.x, at.y);
  }
  // A context menu of `items`, the one open (`closed()` when it closes).
  contextMenu(items, closed) {
    for (const open of this.contexts) open.close();
    const m = makeMenu(items, menuSpec(() => { const app = this.app(); app.focusPane(app.activePane); }, [], null, () => { this.contexts = this.contexts.filter((x) => x !== m); closed(); }, null), []);
    this.contexts = [m];
    return m;
  }

  // A menu dropping from `anchor` (a ▾, or the joined group it is in):
  // under it, at least as wide, and drawn as one shape with it while open.
  dropdown(items, anchor) {
    const group = anchor.closest(".ui-joined") || anchor;
    const m = this.contextMenu(items, () => { group.classList.remove("open"); });
    m.el.classList.add("attached");
    // Under a box, the menu is that box's list of values, drawn as the box
    // is: in its type, each value starting where the box's text does.
    const box = group.querySelector(".ui-input");
    const boxStyle = box ? getComputedStyle(box) : null;
    if (boxStyle) {
      m.el.classList.add("of-field");
      for (const k of ["fontFamily", "fontSize", "fontWeight", "letterSpacing", "textTransform"]) m.el.style[k] = boxStyle[k];
    }
    const r = group.getBoundingClientRect();
    m.el.style.minWidth = `${r.width}px`;
    group.classList.add("open");
    // A group in the window's right half drops its menu ending at its right edge.
    m.openBelow(r, r.left + r.width / 2 > innerWidth / 2);
    // Where the menu is wider than the group, the corner it overhangs is its own.
    const mr = m.el.getBoundingClientRect();
    if (mr.left < r.left - 1) m.el.classList.add("overhang-left");
    if (mr.right > r.right + 1) m.el.classList.add("overhang-right");
    const first = m.el.querySelector(".menu-item");
    if (box && boxStyle && first) {
      const textLeft = box.getBoundingClientRect().left + parseFloat(boxStyle.getPropertyValue("border-left-width")) + parseFloat(boxStyle.getPropertyValue("padding-left"));
      m.el.style.setProperty("--field-text", `${Math.max(0, textLeft - first.getBoundingClientRect().left)}px`);
    }
  }

  dockShow(p) {
    this.activeDock = p;
    for (const x of this.panels.values()) if (x.place === "bottom") x.el.style.display = x === p ? "flex" : "none";
    setClass(this.dock, "hidden", ![...this.panels.values()].some((x) => x.place === "bottom"));
    this.renderDockTabs();
    this.app().renderAll();
  }
  renderDockTabs() {
    this.dockTabs.innerHTML = "";
    for (const p of this.panels.values()) {
      if (p.place !== "bottom") continue;
      const t = document.createElement("div");
      t.className = "tab" + (p === this.activeDock ? " active" : "");
      t.textContent = p.title;
      t.onmousedown = (e) => { e.preventDefault(); this.dockShow(p); p.focus(); };
      const x = document.createElement("span");
      x.className = "x";
      x.textContent = "×";
      x.onmousedown = (e) => { e.preventDefault(); e.stopPropagation(); p.close(); };
      t.appendChild(x);
      this.dockTabs.appendChild(t);
    }
  }
}

class Panel {
  constructor(kit, id, title, place, onClose) {
    this.kit = kit;
    this.id = id;
    this.title = title;
    this.place = place;
    this.onClose = onClose;
    this.state = new Map(); // per-key control state (list selection, tree expansion)
    this.tree = null;
    this.el = document.createElement("div");
    this.el.className = `panel panel-${place}`;
    this.el.tabIndex = -1;
    this.body = document.createElement("div");
    this.body.className = "panel-body";
    this.backdrop = place === "dialog" ? document.createElement("div") : null;
    if (place === "sidebar" || place === "left" || place === "right" || place === "dialog") {
      const head = document.createElement("div");
      head.className = "panel-head";
      head.innerHTML = `<span class="t"></span><span class="x" title="Close">×</span>`;
      head.querySelector(".t").textContent = title;
      this.el.appendChild(head);
    }
    this.el.appendChild(this.body);
    const backdrop = this.backdrop;
    if (backdrop) {
      backdrop.className = "backdrop";
      backdrop.appendChild(this.el);
      document.body.appendChild(backdrop);
    } else if (place === "bottom") kit.dockBody.appendChild(this.el);
    else if (place === "sidebar") kit.side.appendChild(this.el);
    else if (place === "left") kit.left.appendChild(this.el);
    else if (place === "right") kit.right.appendChild(this.el);
    else if (place === "bar") kit.bar.appendChild(this.el);
  }

  // Take the close button, Escape and a dialog's backdrop (once, after
  // `new`).
  listen() {
    const x = this.el.querySelector(".panel-head .x");
    if (x) x.onmousedown = (e) => { e.preventDefault(); this.close(); };
    // Escape closes a dialog or a bar; from a docked panel it goes back to
    // the editor, leaving the panel where it is.
    this.el.addEventListener("keydown", (e) => {
      if (e.key !== "Escape") return;
      e.preventDefault();
      e.stopPropagation();
      if (this.place === "dialog" || this.place === "bar") this.close();
      else this.kit.app().focusPane(this.kit.app().activePane);
    });
    const backdrop = this.backdrop;
    if (backdrop) backdrop.onmousedown = (e) => { if (e.target === backdrop) this.close(); };
  }

  show() {
    if (this.place === "bottom") this.kit.dockShow(this);
    if (this.place === "left") this.kit.left.classList.remove("hidden");
    if (this.place === "right") this.kit.right.classList.remove("hidden");
    if (this.place === "bar") this.kit.bar.classList.remove("hidden");
    this.kit.app().renderAll();
  }

  close() {
    this.kit.panels.delete(this.id);
    if (this.backdrop) this.backdrop.remove();
    else this.el.remove();
    if (this.place === "bottom") {
      const next = [...this.kit.panels.values()].find((p) => p.place === "bottom") || null;
      this.kit.dockShow(next);
    }
    if (this.place === "left" && !this.kit.left.children.length) this.kit.left.classList.add("hidden");
    if (this.place === "right" && !this.kit.right.children.length) this.kit.right.classList.add("hidden");
    if (this.place === "bar" && !this.kit.bar.children.length) this.kit.bar.classList.add("hidden");
    this.onClose?.();
    this.kit.app().focusPane(this.kit.app().activePane);
    this.kit.app().renderAll();
  }

  get open() { return this.kit.panels.get(this.id) === this; }

  // Buttons in the panel's head, before its close: [{icon, title, run}].
  actions(list) {
    const head = this.el.querySelector(".panel-head");
    if (!head) return;
    head.querySelector(".panel-actions")?.remove();
    const box = document.createElement("span");
    box.className = "panel-actions";
    for (const a of list) {
      const b = document.createElement("button");
      b.className = "panel-action";
      b.textContent = a.icon;
      b.title = a.title;
      b.tabIndex = -1;
      b.onmousedown = (e) => e.preventDefault();
      b.onclick = () => a.run();
      box.appendChild(b);
    }
    head.insertBefore(box, head.querySelector(".x"));
  }

  focus(key) {
    const el = key ? this.body.querySelector(`[data-key="${CSS.escape(key)}"]`) : this.body.querySelector("input, select, button, .list");
    (el || this.el).focus();
  }

  render(tree) {
    this.tree = tree;
    const active = document.activeElement;
    const key = this.body.contains(active) ? active.dataset.key : null;
    const sel = key && active.selectionStart != null ? [active.selectionStart, active.selectionEnd] : null;
    this.body.innerHTML = "";
    this.body.appendChild(this.build(tree));
    if (key) {
      const el = this.body.querySelector(`[data-key="${CSS.escape(key)}"]`);
      if (el) {
        el.focus({ preventScroll: true });
        if (sel && el.setSelectionRange) try { el.setSelectionRange(sel[0], sel[1]); } catch {}
      }
    }
  }

  build(n) {
    if (typeof n === "string" || typeof n === "number") return document.createTextNode(String(n));
    const p = n.props;
    const kids = (el) => { for (const c of n.children) el.appendChild(this.build(c)); return el; };
    let el;
    switch (n.type) {
      case "row": case "col": case "box":
        el = kids(div(`ui-${n.type}`));
        break;
      case "text":
        el = kids(document.createElement("span"));
        el.className = "ui-text" + (p.dim ? " dim" : "") + (p.mono ? " mono" : "") + (p.bold ? " bold" : "");
        // A theme key ("syntax.type") or a CSS colour.
        if (p.color) el.style.color = color(p.color) || p.color;
        if (p.title) el.title = p.title;
        break;
      case "badge":
        el = kids(div("ui-badge"));
        if (p.color) el.style.background = color(p.color) || p.color;
        break;
      case "input":
        el = document.createElement("input");
        el.className = "ui-input";
        el.value = p.value ?? "";
        el.placeholder = p.placeholder || "";
        el.spellcheck = false;
        el.oninput = () => p.onInput?.(el.value);
        el.onkeydown = (e) => {
          if (p.onKey && p.onKey(e) === true) { e.preventDefault(); return; }
          if (e.key === "Enter") { e.preventDefault(); p.onEnter?.(el.value, e); }
          if ((e.key === "ArrowDown" || e.key === "ArrowUp") && p.list) {
            e.preventDefault();
            this.listMove(p.list, e.key === "ArrowDown" ? 1 : -1);
          }
        };
        break;
      case "textarea":
        // Text of several lines (Enter is a new line); `rows` tall.
        el = document.createElement("textarea");
        el.className = "ui-textarea";
        el.rows = p.rows || 4;
        el.value = p.value ?? "";
        el.placeholder = p.placeholder || "";
        el.oninput = () => p.onInput?.(el.value);
        el.onkeydown = (e) => { if (p.onKey && p.onKey(e) === true) e.preventDefault(); };
        break;
      case "button":
        el = kids(document.createElement("button"));
        // A `field` button is part of a box beside it (a ▾ for its values):
        // drawn as the box is, not as a button.
        el.className = p.field ? "ui-field-button" : "ui-button" + (p.primary ? " primary" : "");
        el.disabled = !!p.disabled;
        el.title = p.title || "";
        el.onclick = (e) => p.onClick?.(e);
        break;
      case "toggle": {
        // A compact on/off button, as in find bars: [Aa] [ab] [.*]
        el = kids(document.createElement("button"));
        el.className = "ui-toggle" + (p.on ? " on" : "");
        el.title = p.title || "";
        el.onclick = () => p.onChange?.(!p.on);
        break;
      }
      case "checkbox": {
        el = document.createElement("label");
        el.className = "ui-check";
        const c = document.createElement("input");
        c.type = "checkbox";
        c.checked = !!p.checked;
        c.disabled = !!p.disabled;
        if (p.disabled) el.classList.add("disabled");
        if (p.title) el.title = p.title;
        c.onchange = () => p.onChange?.(c.checked);
        el.append(c, ...n.children.map((x) => this.build(x)));
        break;
      }
      case "select": {
        el = document.createElement("select");
        el.className = "ui-select";
        for (const o of p.options || []) {
          const op = document.createElement("option");
          op.value = o.value;
          op.textContent = o.label ?? o.value;
          op.disabled = !!o.disabled;
          el.appendChild(op);
        }
        el.value = p.value;
        el.onchange = () => p.onChange?.(el.value);
        break;
      }
      case "list": el = this.list(n); break;
      case "tree": el = this.treeView(n); break;
      case "table": el = this.table(n); break;
      case "spacer": el = div("ui-spacer"); break;
      case "markdown":
        el = div("ui-markdown");
        el.innerHTML = markdownHtml(p.text || "");
        break;
      default: el = kids(div(`ui-${n.type}`));
    }
    if (p.key) el.dataset.key = p.key;
    if (p.class) el.classList.add(...p.class.split(" "));
    if (p.grow) el.style.flex = "1 1 0";
    return el;
  }

  // A virtualised list. State (selection, scroll) lives in the panel by key.
  list(n) {
    const p = n.props;
    const key = p.key || "list";
    const st = this.state.get(key) || { sel: 0, scroll: 0 };
    this.state.set(key, st);
    const items = p.items || [];
    // A selection the plugin moves (the explorer following the active file)
    // is scrolled into view.
    const moved = p.selected != null && p.selected !== st.sel;
    if (p.selected != null) st.sel = p.selected;
    st.sel = Math.max(0, Math.min(items.length - 1, st.sel));
    const el = div("ui-list");
    el.tabIndex = 0;
    el.dataset.key = key;
    const inner = div("ui-list-inner");
    inner.style.height = `${items.length * ROW_H}px`;
    el.appendChild(inner);
    // A tree's rows (`depth` given): the rows of the folders holding the
    // top visible row stay pinned at the top, outermost first — each
    // ancestor of the row just under the pinned ones.
    const ancestors = (i) => {
      const out = [];
      let d = p.depth(items[i]);
      for (let k = i - 1; k >= 0 && d > 0; k--) {
        const dk = p.depth(items[k]);
        if (dk < d) { out.unshift(k); d = dk; }
      }
      return out;
    };
    const pinned = (top) => {
      let chain = [];
      if (!p.depth) return chain;
      for (let guard = 0; guard < 64; guard++) {
        const i = top + chain.length;
        if (i >= items.length) break;
        const a = ancestors(i);
        if (a.length <= chain.length) break;
        chain = a.slice(0, chain.length + 1);
      }
      return chain;
    };
    const row = (i, at, cls) => {
      const it = items[i];
      // `marked(item)`: the row is in a multiple selection the plugin
      // keeps (drawn as selected).
      const r = div("ui-list-row" + (i === st.sel ? " sel" : "") + (it && p.marked && p.marked(it) ? " marked" : "") + (it && it.header ? " header" : "") + cls);
      r.style.top = `${at}px`;
      const content = p.render ? p.render(it, i) : String(it);
      r.appendChild(typeof content === "string" ? document.createTextNode(content) : this.build(content));
      r.dataset.i = i;
      // Mark the selection in place: redrawing here would replace the row
      // under the pointer, and its click would never arrive.
      r.onmousedown = (e) => {
        e.preventDefault();
        st.sel = i;
        for (const x of inner.children) setClass(x, "sel", +x.dataset.i === i);
        el.focus({ preventScroll: true });
        // `onSelect(item, index, how, event)`: how is "mouse" (a press) or
        // "key" (the arrows); the event says which modifiers were held.
        p.onSelect?.(it, i, "mouse", e);
        if (e.button === 0 && p.onDrop) dragFrom(e, it);
      };
      // `onActivate(item, index, how, event)`: how is "click" (with
      // `activateOnClick`, the first click of one or more), "double" or
      // "key" (Enter).
      r.ondblclick = (e) => p.onActivate?.(it, i, "double", e);
      r.oncontextmenu = (e) => { e.preventDefault(); st.sel = i; draw(); p.onContext?.(it, i, { x: e.clientX, y: e.clientY }); };
      if (p.activateOnClick) r.onclick = (e) => { if (e.detail <= 1) p.onActivate?.(it, i, "click", e); };
      return r;
    };
    // Dragging a row (`onDrop(item, onto)` given): past a few pixels it
    // drags; the row under the pointer that `canDrop(item, onto)` accepts
    // is marked `.drop-target` (empty space below the rows: the list
    // itself, `onto` null); letting go there drops.
    const dragFrom = (e, it) => {
      const x0 = e.clientX, y0 = e.clientY;
      let dragging = false, onto, ok = false;
      const mark = (row) => { for (const x of inner.children) setClass(x, "drop-target", x === row); setClass(el, "drop-target", ok && !row); };
      const move = (ev) => {
        if (!dragging && Math.hypot(ev.clientX - x0, ev.clientY - y0) < 5) return;
        if (!dragging) { dragging = true; el.classList.add("dragging"); }
        const hit = document.elementFromPoint(ev.clientX, ev.clientY);
        const row = hit && hit.closest(".ui-list-row");
        onto = row && inner.contains(row) ? items[+row.dataset.i] : el.contains(hit) ? null : undefined;
        ok = onto !== undefined && (!p.canDrop || p.canDrop(it, onto));
        mark(ok && row ? row : null);
      };
      const up = () => {
        window.removeEventListener("mousemove", move);
        window.removeEventListener("mouseup", up);
        el.classList.remove("dragging");
        mark(null);
        el.classList.remove("drop-target");
        if (dragging && ok) p.onDrop(it, onto);
      };
      window.addEventListener("mousemove", move);
      window.addEventListener("mouseup", up);
    };
    const draw = () => {
      const h0 = el.clientHeight || 300;
      const scroll = el.scrollTop;
      const first = Math.max(0, Math.floor(scroll / ROW_H) - 5);
      const last = Math.min(items.length, first + Math.ceil(h0 / ROW_H) + 10);
      const rows = [];
      for (let i = first; i < last; i++) rows.push(row(i, i * ROW_H, ""));
      // Pinned rows go over the rows they cover. A click on one scrolls
      // its folder back to where it is (under the rows pinned above it).
      const chain = pinned(Math.floor(scroll / ROW_H));
      chain.forEach((i, k) => {
        const r = row(i, scroll + k * ROW_H, " sticky" + (k === chain.length - 1 ? " last" : ""));
        const down = r.onmousedown;
        r.onmousedown = (e) => { el.scrollTop = (i - k) * ROW_H; down(e); };
        rows.push(r);
      });
      inner.replaceChildren(...rows);
    };
    el.onscroll = () => { st.scroll = el.scrollTop; draw(); };
    el.onkeydown = (e) => {
      if (e.key === "ArrowDown" || e.key === "ArrowUp") { e.preventDefault(); this.listMove(key, e.key === "ArrowDown" ? 1 : -1, e); }
      else if (e.key === "PageDown" || e.key === "PageUp") { e.preventDefault(); this.listMove(key, (e.key === "PageDown" ? 1 : -1) * Math.floor(el.clientHeight / ROW_H), e); }
      else if (e.key === "Enter") { e.preventDefault(); p.onActivate?.(items[st.sel], st.sel, "key", e); }
      else p.onKey?.(e, items[st.sel], st.sel);
    };
    el._move = (d, e) => {
      let i = st.sel + d;
      // Skip header rows.
      while (items[i] && items[i].header && i > 0 && i < items.length - 1) i += Math.sign(d);
      st.sel = Math.max(0, Math.min(items.length - 1, i));
      show();
      draw();
      p.onSelect?.(items[st.sel], st.sel, "key", e);
    };
    el._draw = draw;
    const show = () => {
      const top = st.sel * ROW_H;
      // In a tree, as many rows as the row has ancestors may be pinned
      // over the top.
      const pin = p.depth && items[st.sel] ? p.depth(items[st.sel]) * ROW_H : 0;
      if (top - pin < el.scrollTop) el.scrollTop = Math.max(0, top - pin);
      else if (top + ROW_H > el.scrollTop + el.clientHeight) el.scrollTop = top + ROW_H - el.clientHeight;
    };
    requestAnimationFrame(() => { el.scrollTop = st.scroll; if (moved) show(); draw(); });
    draw();
    return el;
  }

  // Move a list's selection `d` rows (the key `e` that moved it, if any).
  listMove(key, d, e) {
    const el = this.body.querySelector(`.ui-list[data-key="${CSS.escape(key)}"]`);
    el?._move(d, e || null);
  }

  // A tree: nodes {id, label, children, open, leaf, data} (a leaf has no
  // children, a folder may have none); expansion is kept by id.
  treeView(n) {
    const p = n.props;
    const key = p.key || "tree";
    const open = this.state.get(key + ":open") || new Set();
    this.state.set(key + ":open", open);
    const rows = [];
    const walk = (nodes, depth, path) => {
      nodes.forEach((nd, i) => {
        const id = nd.id ?? `${path}/${i}`;
        const isOpen = !nd.leaf && (open.has(id) || (nd.open && !open.has("!" + id)));
        rows.push({ nd, depth, id, isOpen });
        if (isOpen) walk(nd.children, depth + 1, id);
      });
    };
    walk(p.nodes || [], 0, "");
    return this.list(h("list", {
      key,
      items: rows,
      depth: (r) => r.depth,
      render: (r) => h("row", { class: "ui-tree-row" }, [
        h("text", { class: "ui-tree-indent" }, [" ".repeat(r.depth * 2) + (r.nd.leaf ? "  " : r.isOpen ? "▾ " : "▸ ")]),
        typeof r.nd.label === "string" ? h("text", {}, [r.nd.label]) : r.nd.label,
      ]),
      onActivate: (r, _i, how) => {
        if (!r.nd.leaf) {
          // A folder opens or closes on its first click.
          if (how === "double") return;
          if (r.isOpen) { open.delete(r.id); open.add("!" + r.id); } else { open.add(r.id); open.delete("!" + r.id); }
          this.render(this.tree);
          this.focus(key);
        } else p.onActivate?.(r.nd.data, r.nd);
      },
      onSelect: (r) => p.onSelect?.(r.nd.data, r.nd),
      onKey: (e, r) => p.onKey?.(e, r?.nd.data, r?.nd),
      onContext: (r, _i, at) => p.onContext?.(r.nd.data, r.nd, at),
      activateOnClick: true,
    }));
  }

  table(n) {
    const p = n.props;
    const t = document.createElement("table");
    t.className = "ui-table";
    const head = document.createElement("tr");
    for (const c of p.columns || []) { const th = document.createElement("th"); th.textContent = c; head.appendChild(th); }
    t.appendChild(head);
    (p.rows || []).forEach((r, i) => {
      const tr = document.createElement("tr");
      for (const c of r.cells) { const td = document.createElement("td"); td.appendChild(this.build(typeof c === "object" ? c : String(c))); tr.appendChild(td); }
      if (p.onActivate) tr.ondblclick = () => p.onActivate(r, i);
      t.appendChild(tr);
    });
    return t;
  }
}

// Markdown as HTML, for text a plugin shows (a tour step's description):
// headings, paragraphs, lists, quotes, fenced code, rules, and inline code,
// bold, italic and links. Everything is escaped first; links keep only web,
// mail and relative targets.
const esc = escapeHtml;
function inline(t) {
  return t.split(/(`[^`]+`)/).map((part, i) => {
    if (i % 2) return `<code>${esc(part.slice(1, -1))}</code>`;
    return esc(part)
      .replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (m, label, url) => (/^(https?:|mailto:|#|\.{0,2}\/|[\w.-]+(\/|$|#))/i.test(url) ? `<a href="${url}" target="_blank" rel="noopener">${label}</a>` : label))
      .replace(/\*\*([^*]+)\*\*|__([^_]+)__/g, (m, a, b) => `<strong>${a || b}</strong>`)
      .replace(/(^|[^\w*])\*([^*\s][^*]*)\*|(^|\W)_([^_\s][^_]*)_(?!\w)/g, (m, p1, a, p2, b) => `${p1 ?? p2}<em>${a ?? b}</em>`);
  }).join("");
}
export function markdownHtml(text) {
  const out = [];
  const lines = text.replace(/\r\n?/g, "\n").split("\n");
  let para = [], list = null;
  const flush = () => {
    if (para.length) out.push(`<p>${inline(para.join(" "))}</p>`);
    para = [];
    if (list) out.push(`<${list.tag}>${list.items.map((x) => `<li>${inline(x)}</li>`).join("")}</${list.tag}>`);
    list = null;
  };
  for (let i = 0; i < lines.length; i++) {
    const l = lines[i];
    let m;
    if ((m = /^\s*(```|~~~)/.exec(l))) {
      flush();
      const code = [];
      while (++i < lines.length && !lines[i].trimStart().startsWith(m[1])) code.push(lines[i]);
      out.push(`<pre><code>${esc(code.join("\n"))}</code></pre>`);
    } else if ((m = /^(#{1,6})\s+(.*)$/.exec(l))) {
      flush();
      out.push(`<h${m[1].length}>${inline(m[2])}</h${m[1].length}>`);
    } else if (/^\s*([-*_])(\s*\1){2,}\s*$/.test(l)) {
      flush();
      out.push("<hr>");
    } else if ((m = /^\s*>\s?(.*)$/.exec(l))) {
      flush();
      out.push(`<blockquote>${inline(m[1])}</blockquote>`);
    } else if ((m = /^\s*(?:([-*+])|\d+[.)])\s+(.*)$/.exec(l))) {
      const tag = m[1] ? "ul" : "ol";
      if (para.length || (list && list.tag !== tag)) flush();
      list = list || { tag, items: [] };
      list.items.push(m[2]);
    } else if (!l.trim()) flush();
    else if (list && /^\s+\S/.test(l)) list.items[list.items.length - 1] += " " + l.trim();
    else {
      if (list) flush();
      para.push(l.trim());
    }
  }
  flush();
  return out.join("");
}

function div(cls) {
  const d = document.createElement("div");
  d.className = cls;
  return d;
}
