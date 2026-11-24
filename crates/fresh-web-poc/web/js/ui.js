// The panel UI kit for plugins (and the client's own features).
//
// A plugin asks for a panel in a place — the sidebar, the bottom dock, a
// column beside the editor, a bar above it, or a dialog — and renders a
// declarative tree into it, built with `h(type, props, ...children)`:
//
//   panel.render(h("col", {},
//     h("input", {key: "q", value: q, placeholder: "Search", onInput: (v) => ...}),
//     h("list", {key: "hits", items, render: (it) => it.text, onActivate: (it) => ...})));
//
// Controls are real DOM controls, so focus, text editing, IME and
// accessibility are the browser's. Re-rendering rebuilds the panel and puts
// focus and the caret back on the control with the same key. Lists are
// virtualised (any number of items) and keyboard-navigable.

import { color } from "./highlight.js";

export function h(type, props, ...children) {
  return { type, props: props || {}, children: children.flat(Infinity).filter((c) => c != null && c !== false) };
}

const ROW_H = 22;

export class UIKit {
  constructor(app) {
    this.app = app;
    this.panels = new Map();
    this.dock = document.querySelector("#dock");
    this.dockTabs = this.dock.querySelector(".dock-tabs");
    this.dockBody = this.dock.querySelector(".dock-body");
    this.side = document.querySelector("#side-panels");
    this.right = document.querySelector("#right-dock");
    this.bar = document.querySelector("#bar");
    this.activeDock = null;
  }

  // place: "bottom" | "sidebar" | "right" | "bar" | "dialog"
  panel({ id, title, place = "bottom", onClose }) {
    let p = this.panels.get(id);
    if (p) { p.show(); return p; }
    p = new Panel(this, id, title, place, onClose);
    this.panels.set(id, p);
    p.show();
    return p;
  }

  // A context menu: items [{label, run}] (null for a separator) at {x, y}.
  menu(items, at) {
    document.querySelector(".ctxmenu")?.remove();
    const m = div("ctxmenu");
    for (const it of items) {
      if (!it) { const hr = document.createElement("hr"); hr.style.cssText = "border:0;border-top:1px solid var(--border);margin:4px 0"; m.appendChild(hr); continue; }
      const d = div("");
      d.textContent = it.label;
      d.onmousedown = (e) => { e.preventDefault(); m.remove(); it.run(); };
      m.appendChild(d);
    }
    m.style.left = `${at.x}px`;
    m.style.top = `${at.y}px`;
    document.body.appendChild(m);
    const off = (e) => { if (!m.contains(e.target)) { m.remove(); window.removeEventListener("mousedown", off, true); } };
    window.addEventListener("mousedown", off, true);
  }

  // Draw every panel again (the theme changed).
  redraw() { for (const p of this.panels.values()) if (p.tree) p.render(p.tree); }

  dockShow(p) {
    this.activeDock = p;
    for (const x of this.panels.values()) if (x.place === "bottom") x.el.style.display = x === p ? "flex" : "none";
    this.dock.classList.toggle("hidden", ![...this.panels.values()].some((x) => x.place === "bottom"));
    this.renderDockTabs();
    this.app.renderAll();
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
    if (place === "sidebar" || place === "right" || place === "dialog") {
      const head = document.createElement("div");
      head.className = "panel-head";
      head.innerHTML = `<span class="t"></span><span class="x" title="Close">×</span>`;
      head.querySelector(".t").textContent = title;
      head.querySelector(".x").onmousedown = (e) => { e.preventDefault(); this.close(); };
      this.el.appendChild(head);
    }
    this.el.appendChild(this.body);
    // Escape closes a dialog or a bar; from a docked panel it goes back to
    // the editor, leaving the panel where it is.
    this.el.addEventListener("keydown", (e) => {
      if (e.key !== "Escape") return;
      e.preventDefault();
      e.stopPropagation();
      if (place === "dialog" || place === "bar") this.close();
      else kit.app.focusPane(kit.app.activePane);
    });
    if (place === "dialog") {
      this.backdrop = document.createElement("div");
      this.backdrop.className = "backdrop";
      this.backdrop.onmousedown = (e) => { if (e.target === this.backdrop) this.close(); };
      this.backdrop.appendChild(this.el);
      document.body.appendChild(this.backdrop);
    } else if (place === "bottom") kit.dockBody.appendChild(this.el);
    else if (place === "sidebar") kit.side.appendChild(this.el);
    else if (place === "right") kit.right.appendChild(this.el);
    else if (place === "bar") kit.bar.appendChild(this.el);
  }

  show() {
    if (this.place === "bottom") this.kit.dockShow(this);
    if (this.place === "right") this.kit.right.classList.remove("hidden");
    if (this.place === "bar") this.kit.bar.classList.remove("hidden");
    this.kit.app.renderAll();
  }

  close() {
    this.kit.panels.delete(this.id);
    if (this.backdrop) this.backdrop.remove();
    else this.el.remove();
    if (this.place === "bottom") {
      const next = [...this.kit.panels.values()].find((p) => p.place === "bottom") || null;
      this.kit.dockShow(next);
    }
    if (this.place === "right" && !this.kit.right.children.length) this.kit.right.classList.add("hidden");
    if (this.place === "bar" && !this.kit.bar.children.length) this.kit.bar.classList.add("hidden");
    this.onClose?.();
    this.kit.app.focusPane(this.kit.app.activePane);
    this.kit.app.renderAll();
  }

  get open() { return this.kit.panels.get(this.id) === this; }

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
      case "button":
        el = kids(document.createElement("button"));
        el.className = "ui-button" + (p.primary ? " primary" : "");
        el.disabled = !!p.disabled;
        el.title = p.title || "";
        el.onclick = () => p.onClick?.();
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
    const draw = () => {
      inner.innerHTML = "";
      const h0 = el.clientHeight || 300;
      const first = Math.max(0, Math.floor(el.scrollTop / ROW_H) - 5);
      const last = Math.min(items.length, first + Math.ceil(h0 / ROW_H) + 10);
      for (let i = first; i < last; i++) {
        const it = items[i];
        const r = div("ui-list-row" + (i === st.sel ? " sel" : "") + (it && it.header ? " header" : ""));
        r.style.top = `${i * ROW_H}px`;
        const content = p.render ? p.render(it, i) : String(it);
        r.appendChild(typeof content === "string" ? document.createTextNode(content) : this.build(content));
        r.dataset.i = i;
        // Mark the selection in place: redrawing here would replace the row
        // under the pointer, and its click would never arrive.
        r.onmousedown = (e) => {
          e.preventDefault();
          st.sel = i;
          for (const x of inner.children) x.classList.toggle("sel", +x.dataset.i === i);
          el.focus({ preventScroll: true });
          p.onSelect?.(it, i);
        };
        r.ondblclick = () => p.onActivate?.(it, i);
        r.oncontextmenu = (e) => { e.preventDefault(); st.sel = i; draw(); p.onContext?.(it, i, { x: e.clientX, y: e.clientY }); };
        if (p.activateOnClick) r.onclick = () => p.onActivate?.(it, i);
        inner.appendChild(r);
      }
    };
    el.onscroll = () => { st.scroll = el.scrollTop; draw(); };
    el.onkeydown = (e) => {
      if (e.key === "ArrowDown" || e.key === "ArrowUp") { e.preventDefault(); this.listMove(key, e.key === "ArrowDown" ? 1 : -1); }
      else if (e.key === "PageDown" || e.key === "PageUp") { e.preventDefault(); this.listMove(key, (e.key === "PageDown" ? 1 : -1) * Math.floor(el.clientHeight / ROW_H)); }
      else if (e.key === "Enter") { e.preventDefault(); p.onActivate?.(items[st.sel], st.sel); }
      else p.onKey?.(e, items[st.sel], st.sel);
    };
    el._move = (d) => {
      let i = st.sel + d;
      // Skip header rows.
      while (items[i] && items[i].header && i > 0 && i < items.length - 1) i += Math.sign(d);
      st.sel = Math.max(0, Math.min(items.length - 1, i));
      show();
      draw();
      p.onSelect?.(items[st.sel], st.sel);
    };
    el._draw = draw;
    const show = () => {
      const top = st.sel * ROW_H;
      if (top < el.scrollTop) el.scrollTop = top;
      else if (top + ROW_H > el.scrollTop + el.clientHeight) el.scrollTop = top + ROW_H - el.clientHeight;
    };
    requestAnimationFrame(() => { el.scrollTop = st.scroll; if (moved) show(); draw(); });
    draw();
    return el;
  }

  listMove(key, d) {
    const el = this.body.querySelector(`.ui-list[data-key="${CSS.escape(key)}"]`);
    el?._move(d);
  }

  // A tree: nodes {label, children?, data, open?}; expansion is kept by path.
  treeView(n) {
    const p = n.props;
    const key = p.key || "tree";
    const open = this.state.get(key + ":open") || new Set();
    this.state.set(key + ":open", open);
    const rows = [];
    const walk = (nodes, depth, path) => {
      nodes.forEach((nd, i) => {
        const id = nd.id ?? `${path}/${i}`;
        const isOpen = nd.children && (open.has(id) || (nd.open && !open.has("!" + id)));
        rows.push({ nd, depth, id, isOpen });
        if (isOpen) walk(nd.children, depth + 1, id);
      });
    };
    walk(p.nodes || [], 0, "");
    return this.list(h("list", {
      key,
      items: rows,
      render: (r) => h("row", { class: "ui-tree-row" },
        h("text", { class: "ui-tree-indent" }, " ".repeat(r.depth * 2) + (r.nd.children ? (r.isOpen ? "▾ " : "▸ ") : "  ")),
        typeof r.nd.label === "string" ? h("text", {}, r.nd.label) : r.nd.label),
      onActivate: (r) => {
        if (r.nd.children) {
          if (r.isOpen) { open.delete(r.id); open.add("!" + r.id); } else { open.add(r.id); open.delete("!" + r.id); }
          this.render(this.tree);
          this.focus(key);
        } else p.onActivate?.(r.nd.data, r.nd);
      },
      onSelect: (r) => p.onSelect?.(r.nd.data, r.nd),
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

function div(cls) {
  const d = document.createElement("div");
  d.className = cls;
  return d;
}
