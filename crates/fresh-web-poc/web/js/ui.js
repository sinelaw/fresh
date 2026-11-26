// The panel UI kit for plugins (and the client's own features).
//
// A plugin asks for a panel in a place — the sidebar, the bottom dock, a
// column beside the editor, a bar above it, or a dialog — and renders a
// tree of elements into it, each made by its kind's function:
//
//   panel.render(col(BOX, [
//     input({ ...INPUT, key: "q", value: q, placeholder: "Search", onInput: (v) => ... }),
//     list({ ...listProps("hits", items, (it, _i) => text(TEXT, it.text)), onActivate: (it, _i, _how, _e) => ... }),
//   ]));
//
// Controls are real DOM controls, so focus, text editing, IME and
// accessibility are the browser's. Re-rendering rebuilds the panel and puts
// focus and the caret back on the control with the same key. Lists are
// virtualised (any number of items) and keyboard-navigable.

import { $, setClass, escapeHtml, within } from "./dom.js";
import { listOf, found } from "./maybe.js";
import { color } from "./highlight.js";
import { makeMenu, menuSpec } from "./menu.js";

// ---------------------------------------------------------------- elements
//
// A panel's elements (types/fresh.d.js `UINode`), each made from its
// kind's props. The defaults are each kind's props when nothing is said;
// an element says what differs: `button({ ...BUTTON, onClick }, "Save")`.
// A part shown only sometimes is spread in: `...(error ? [text(TEXT, error)] : [])`.

/** const BOX: BoxProps */
export const BOX = { class: "" };
/** const TEXT: TextProps */
export const TEXT = { class: "", dim: false, mono: false, bold: false, color: [], title: "" };
/** const BUTTON: {primary: Boolean, field: Boolean, disabled: Boolean, title: String} */
export const BUTTON = { primary: false, field: false, disabled: false, title: "" };
/** const TOGGLE: {title: String} */
export const TOGGLE = { title: "" };
/** const INPUT: {
 *   placeholder: String,
 *   grow: Boolean,
 *   list: String[],
 *   onInput: (String) => Undefined,
 *   onEnter: (String, DomEvent<El>) => Undefined,
 *   onKey: (DomEvent<El>) => Boolean,
 * } */
export const INPUT = { placeholder: "", grow: false, list: [], onInput: (_v) => {}, onEnter: (_v, _e) => {}, onKey: (_e) => false };
/** const TEXTAREA: {rows: Number, placeholder: String, onKey: (DomEvent<El>) => Boolean} */
export const TEXTAREA = { rows: 4, placeholder: "", onKey: (_e) => false };
/** const CHECKBOX: {disabled: Boolean, title: String} */
export const CHECKBOX = { disabled: false, title: "" };

/** function row(p: BoxProps, children: UINode[]) => UINode */
export function row(p, children) { return { kind: "row", props: p, children }; }
/** function col(p: BoxProps, children: UINode[]) => UINode */
export function col(p, children) { return { kind: "col", props: p, children }; }
// Room between: it takes what the row has left.
/** function spacer() => UINode */
export function spacer() { return { kind: "spacer" }; }
/** function text(p: TextProps, s: String) => UINode */
export function text(p, s) { return { kind: "text", props: p, text: s }; }
// Markdown (`markdownHtml`): a tour step's description.
/** function markdown(s: String) => UINode */
export function markdown(s) { return { kind: "markdown", text: s }; }
/** function button(p: ButtonProps, label: String) => UINode */
export function button(p, label) { return { kind: "button", props: p, label }; }
/** function toggle(p: ToggleProps, label: String) => UINode */
export function toggle(p, label) { return { kind: "toggle", props: p, label }; }
/** function checkbox(p: CheckboxProps, label: String) => UINode */
export function checkbox(p, label) { return { kind: "checkbox", props: p, label }; }
/** function input(p: InputProps) => UINode */
export function input(p) { return { kind: "input", props: p }; }
/** function textarea(p: TextareaProps) => UINode */
export function textarea(p) { return { kind: "textarea", props: p }; }
/** function select(p: SelectProps) => UINode */
export function select(p) { return { kind: "select", props: p }; }

// A list's props when nothing more is said: nothing selected, no rows
// headers or marked, a row doing nothing when pressed.
/** function listProps<T>(key: String, items: T[], render: (T, Number) => UINode) => ListProps<T> */
export function listProps(key, items, render) {
  return {
    key, class: "", items, render, selected: [], activateOnClick: false, depth: [],
    header: (_it) => false, marked: (_it) => false,
    onSelect: (_it, _i, _how, _e) => {}, onActivate: (_it, _i, _how, _e) => {},
    onContext: (_it, _i, _at) => {}, onKey: (_e, _it, _i) => {}, drop: [],
  };
}
// A list's rows by index: what each does with its item.
/** function rowsOf<T>(p: ListProps<T>) => Rows */
function rowsOf(p) {
  const items = p.items;
  return {
    key: p.key, class: p.class, count: items.length,
    row: (i) => p.render(items[i], i),
    selected: p.selected, activateOnClick: p.activateOnClick,
    depth: p.depth.map((depth) => (i) => depth(items[i])),
    header: (i) => p.header(items[i]),
    marked: (i) => p.marked(items[i]),
    onSelect: (i, how, e) => { p.onSelect(items[i], i, how, e); },
    onActivate: (i, how, e) => { p.onActivate(items[i], i, how, e); },
    onContext: (i, at) => { p.onContext(items[i], i, at); },
    onKey: (e, i) => { p.onKey(e, items.slice(i, i + 1), i); },
    drop: p.drop.map((d) => ({
      canDrop: (i, onto) => d.canDrop(items[i], onto.map((j) => items[j])),
      onDrop: (i, onto) => { d.onDrop(items[i], onto.map((j) => items[j])); },
    })),
  };
}
/** function list<T>(p: ListProps<T>) => UINode */
export function list(p) { return { kind: "list", rows: rowsOf(p) }; }

// A tree's props when nothing more is said: its nodes doing nothing.
/** function treeProps<D>(key: String, nodes: TreeNode<D>[]) => TreeProps<D> */
export function treeProps(key, nodes) {
  return { key, nodes, onActivate: (_d, _nd) => {}, onSelect: (_d, _nd) => {}, onContext: (_d, _nd, _at) => {}, onKey: (_e, _d) => {} };
}
// A tree's node with what it does.
/** function branchOf<D>(p: TreeProps<D>, nd: TreeNode<D>) => Branch */
function branchOf(p, nd) {
  return {
    id: nd.id, label: nd.label, children: nd.children.map((c) => branchOf(p, c)), open: nd.open, leaf: nd.leaf,
    activate: () => { p.onActivate(nd.data, nd); },
    select: () => { p.onSelect(nd.data, nd); },
    context: (at) => { p.onContext(nd.data, nd, at); },
    key: (e) => { p.onKey(e, [nd.data]); },
  };
}
/** function tree<D>(p: TreeProps<D>) => UINode */
export function tree(p) {
  return { kind: "tree", key: p.key, nodes: p.nodes.map((nd) => branchOf(p, nd)), none: (e) => { p.onKey(e, []); } };
}

const ROW_H = 22;

export class UIKit {
  // `editor` (main.js): focus back to the editor, and redraw its panes
  // (the dock took or gave room).
  constructor(editor) {
    const port /*: UIKitEditor */ = editor;
    this.editor = port;
    this.panels = new Map();
    this.dock = $("#dock");
    this.dockTabs = this.dock.querySelector(".dock-tabs");
    this.dockBody = this.dock.querySelector(".dock-body");
    this.side = $("#side-panels");
    this.right = $("#right-dock");
    this.left = $("#left-dock");
    this.bar = $("#bar");
    this.activeDock = []; // the dock's panel shown: a list of at most one
    this.contexts = []; // the open context menu: a list of at most one
  }

  // A PanelOptions (types/fresh.d.js): place "bottom" | "sidebar" | "left"
  // (a column before the sidebar) | "right" | "bar" | "dialog".
  panel({ id, title, place, onClose }) {
    const had = this.panels.get(id);
    if (had !== undefined) { had.show(); return had; }
    const p = new Panel(this, id, title, place, onClose);
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
    this.contextMenu(items, () => {}, []).openAt(at.x, at.y);
  }
  // A context menu of `items`, the one open (`closed()` when it closes);
  // presses on `ignore` leave it open.
  contextMenu(items, closed, ignore) {
    for (const open of this.contexts) open.close();
    const m = makeMenu(items, menuSpec(() => { this.editor.focus(); }, ignore, null, () => { this.contexts = this.contexts.filter((x) => x !== m); closed(); }, null), []);
    this.contexts = [m];
    return m;
  }

  // A menu dropping from `anchor` (a ▾, or the joined group it is in):
  // under it, at least as wide, and drawn as one shape with it while open.
  // Pressing the anchor again closes it.
  dropdown(items, anchor) {
    const group = anchor.closest(".ui-joined") ?? anchor;
    if (group.classList.contains("open")) { for (const open of this.contexts) open.close(); return; }
    const m = this.contextMenu(items, () => { group.classList.remove("open"); }, [group]);
    m.el.classList.add("attached");
    // Under a box, the menu is that box's list of values, drawn as the box
    // is: in its type, each value starting where the box's text does.
    const boxes = listOf(group.querySelector(".ui-input")).map((el) => ({ el, style: getComputedStyle(el) }));
    for (const box of boxes) {
      m.el.classList.add("of-field");
      for (const k of ["font-family", "font-size", "font-weight", "letter-spacing", "text-transform"]) m.el.style.setProperty(k, box.style.getPropertyValue(k));
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
    // Its top edge opens where the group sits on it, so the two are one outline.
    m.el.style.setProperty("--joint-left", `${r.left - mr.left}px`);
    m.el.style.setProperty("--joint-width", `${r.width}px`);
    const first = m.el.querySelector(".menu-item");
    for (const box of boxes) for (const f of listOf(first)) {
      const textLeft = box.el.getBoundingClientRect().left + parseFloat(box.style.getPropertyValue("border-left-width")) + parseFloat(box.style.getPropertyValue("padding-left"));
      m.el.style.setProperty("--field-text", `${Math.max(0, textLeft - f.getBoundingClientRect().left)}px`);
    }
  }

  // Show `shown` in the dock (a list of at most one panel; none: none).
  dockShow(shown) {
    this.activeDock = shown;
    for (const x of this.panels.values()) if (x.place === "bottom") x.el.style.display = shown.includes(x) ? "flex" : "none";
    setClass(this.dock, "hidden", ![...this.panels.values()].some((x) => x.place === "bottom"));
    this.renderDockTabs();
    this.editor.redraw();
  }
  renderDockTabs() {
    this.dockTabs.innerHTML = "";
    for (const p of this.panels.values()) {
      if (p.place !== "bottom") continue;
      const t = document.createElement("div");
      t.className = "tab" + (this.activeDock.includes(p) ? " active" : "");
      t.textContent = p.title;
      t.onmousedown = (e) => { e.preventDefault(); this.dockShow([p]); p.focusFirst(); };
      const x = document.createElement("span");
      x.className = "x";
      x.textContent = "×";
      x.onmousedown = (e) => { e.preventDefault(); e.stopPropagation(); p.close(); };
      t.appendChild(x);
      this.dockTabs.appendChild(t);
    }
  }
}

// A panel's controls' state, by key: lists' selection and scroll, trees'
// expanded node ids.
/** function noLists() => Map<String, {sel: Number, scroll: Number}> */
function noLists() { return new Map(); }
/** function noExpanded() => Map<String, Map<String, Boolean>> */
function noExpanded() { return new Map(); }
/** function noMovers() => Map<String, (Number, DomEvent<El>) => Undefined> */
function noMovers() { return new Map(); }
/** function noTree() => UINode[] */
function noTree() { return []; }
// A tree's folders opened or closed by hand (true: open), by id.
/** function noToggled() => Map<String, Boolean> */
function noToggled() { return new Map(); }

class Panel {
  constructor(kit, id, title, place, onClose) {
    this.kit = kit;
    this.id = id;
    this.title = title;
    this.place = place;
    this.onClose = onClose;
    // Per-key control state: a list's selection and scroll, how the arrow
    // keys move it, and a tree's expanded nodes.
    this.lists = noLists();
    this.movers = noMovers();
    this.expanded = noExpanded();
    this.shownTree = noTree(); // the tree shown: a list of at most one
    this.el = document.createElement("div");
    this.el.className = `panel panel-${place}`;
    this.el.tabIndex = -1;
    this.body = document.createElement("div");
    this.body.className = "panel-body";
    this.backdrop = place === "dialog" ? document.createElement("div") : null;
    if (place === "sidebar" || place === "left" || place === "right" || place === "dialog") {
      const head = document.createElement("div");
      head.className = "panel-head";
      head.innerHTML = `<span class="t">${escapeHtml(title)}</span><span class="x" title="Close">×</span>`;
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
      else this.kit.editor.focus();
    });
    const backdrop = this.backdrop;
    if (backdrop) backdrop.onmousedown = (e) => { if (e.target === backdrop) this.close(); };
  }

  show() {
    if (this.place === "bottom") this.kit.dockShow([this]);
    if (this.place === "left") this.kit.left.classList.remove("hidden");
    if (this.place === "right") this.kit.right.classList.remove("hidden");
    if (this.place === "bar") this.kit.bar.classList.remove("hidden");
    this.kit.editor.redraw();
  }

  close() {
    this.kit.panels.delete(this.id);
    const backdrop = this.backdrop;
    if (backdrop !== null) backdrop.remove();
    else this.el.remove();
    if (this.place === "bottom") {
      this.kit.dockShow([...this.kit.panels.values()].filter((p) => p.place === "bottom").slice(0, 1));
    }
    if (this.place === "left" && !this.kit.left.children.length) this.kit.left.classList.add("hidden");
    if (this.place === "right" && !this.kit.right.children.length) this.kit.right.classList.add("hidden");
    if (this.place === "bar" && !this.kit.bar.children.length) this.kit.bar.classList.add("hidden");
    this.onClose?.();
    this.kit.editor.focus();
    this.kit.editor.redraw();
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
    within(head, ".x").before(box);
  }

  // The keyboard to the control keyed `key`, or to the first control.
  focus(key) {
    for (const el of listOf(this.body.querySelector(`[data-key="${CSS.escape(key)}"]`))) el.focus();
  }
  focusFirst() {
    for (const el of [...listOf(this.body.querySelector("input, select, button, .ui-list")), this.el].slice(0, 1)) el.focus();
  }

  render(tree) {
    this.shownTree = [tree];
    const had = focusIn(this.body);
    this.body.replaceChildren(this.build(tree));
    // The keyed control that had the keyboard has it again, its selection
    // as it was.
    for (const f of had) {
      for (const el of listOf(this.body.querySelector(`[data-key="${CSS.escape(f.key)}"]`))) {
        el.focus();
        // (A control without a text selection refuses one.)
        try { el.selectionStart = f.start; el.selectionEnd = f.end; } catch {}
      }
    }
  }

  /** function build(n: UINode) => El */
  build(n) {
    if (n.kind === "row") return withClass(this.kids(div("ui-row"), n.children), n.props.class);
    if (n.kind === "col") return withClass(this.kids(div("ui-col"), n.children), n.props.class);
    if (n.kind === "spacer") return div("ui-spacer");
    if (n.kind === "text") return textEl(n.props, n.text);
    if (n.kind === "markdown") return markdownEl(n.text);
    if (n.kind === "button") return buttonEl(n.props, n.label);
    if (n.kind === "toggle") return toggleEl(n.props, n.label);
    if (n.kind === "checkbox") return checkboxEl(n.props, n.label);
    if (n.kind === "input") return this.inputEl(n.props);
    if (n.kind === "textarea") return textareaEl(n.props);
    if (n.kind === "select") return selectEl(n.props);
    if (n.kind === "list") return this.list(n.rows);
    return this.treeView(n.key, n.nodes, n.none);
  }
  kids(el, children) {
    for (const c of children) el.appendChild(this.build(c));
    return el;
  }

  inputEl(p) {
    const el = document.createElement("input");
    el.className = "ui-input";
    el.dataset.key = p.key;
    el.value = p.value;
    el.placeholder = p.placeholder;
    el.setAttribute("spellcheck", "false");
    if (p.grow) el.style.flex = "1 1 0";
    el.oninput = (_e) => { p.onInput(el.value); };
    el.onkeydown = (e) => {
      if (p.onKey(e)) { e.preventDefault(); return; }
      if (e.key === "Enter") { e.preventDefault(); p.onEnter(el.value, e); }
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        for (const key of p.list) { e.preventDefault(); this.listMove(key, e.key === "ArrowDown" ? 1 : -1, e); }
      }
    };
    return el;
  }

  // A virtualised list: only the rows in sight are drawn. Its selection and
  // scroll live in the panel, by key.
  list(r) {
    const key = r.key;
    const n = r.count;
    const st = this.lists.get(key) ?? { sel: 0, scroll: 0 };
    this.lists.set(key, st);
    // A selection the plugin moves (the explorer following the active file)
    // is scrolled into view.
    const moved = r.selected.some((s) => s !== st.sel);
    for (const s of r.selected) st.sel = s;
    st.sel = Math.max(0, Math.min(n - 1, st.sel));
    const el = withClass(div("ui-list"), r.class);
    el.tabIndex = 0;
    el.dataset.key = key;
    const inner = div("ui-list-inner");
    inner.style.height = `${n * ROW_H}px`;
    el.appendChild(inner);
    /** let drawn: DrawnRow[] */
    let drawn = []; // the rows drawn, by index
    // A row, at `at` px; `jump` runs first when it is pressed.
    const rowEl = (i, at, cls, jump) => {
      const rw = div("ui-list-row" + (i === st.sel ? " sel" : "") + (r.marked(i) ? " marked" : "") + (r.header(i) ? " header" : "") + cls);
      rw.style.top = `${at}px`;
      rw.appendChild(this.build(r.row(i)));
      // Mark the selection in place: redrawing here would replace the row
      // under the pointer, and its click would never arrive.
      rw.onmousedown = (e) => {
        jump();
        e.preventDefault();
        st.sel = i;
        for (const d of drawn) setClass(d.el, "sel", d.i === i);
        el.focus({ preventScroll: true });
        r.onSelect(i, "mouse", e);
        if (e.button === 0) for (const d of r.drop) dragFrom(e, i, d);
      };
      rw.ondblclick = (e) => { r.onActivate(i, "double", e); };
      rw.oncontextmenu = (e) => { e.preventDefault(); st.sel = i; draw(); r.onContext(i, { x: e.clientX, y: e.clientY }); };
      if (r.activateOnClick) rw.onclick = (e) => { if (e.detail <= 1) r.onActivate(i, "click", e); };
      return { i, el: rw };
    };
    // Dragging row `i`: past a few pixels it drags; the row under the
    // pointer that `canDrop` accepts (or the empty space below the rows: the
    // list itself, onto none) is marked `.drop-target`; letting go there
    // drops.
    const dragFrom = (e, i, d) => {
      const x0 = e.clientX, y0 = e.clientY;
      let dragging = false;
      /** let target: DropTarget[] */
      let target = []; // where it would drop: a list of at most one
      const mark = () => {
        for (const x of drawn) setClass(x.el, "drop-target", target.some((t) => t.onto.includes(x.i)));
        setClass(el, "drop-target", target.some((t) => t.onto.length === 0));
      };
      const move = (ev) => {
        if (!dragging && Math.hypot(ev.clientX - x0, ev.clientY - y0) < 5) return;
        if (!dragging) { dragging = true; el.classList.add("dragging"); }
        const hit = listOf(document.elementFromPoint(ev.clientX, ev.clientY));
        const under = drawn.filter((x) => hit.some((h) => x.el.contains(h)));
        const onto = under.length > 0 ? [{ onto: under.map((x) => x.i).slice(0, 1) }] : hit.some((h) => el.contains(h)) ? [{ onto: [] }] : [];
        target = onto.filter((t) => d.canDrop(i, t.onto));
        mark();
      };
      const up = (_ev) => {
        window.removeEventListener("mousemove", move);
        window.removeEventListener("mouseup", up);
        el.classList.remove("dragging");
        const dropped = dragging ? target : [];
        target = [];
        mark();
        for (const t of dropped) d.onDrop(i, t.onto);
      };
      window.addEventListener("mousemove", move);
      window.addEventListener("mouseup", up);
    };
    const draw = () => {
      const h0 = el.clientHeight > 0 ? el.clientHeight : 300;
      const scroll = el.scrollTop;
      const first = Math.max(0, Math.floor(scroll / ROW_H) - 5);
      const last = Math.min(n, first + Math.ceil(h0 / ROW_H) + 10);
      const rows = [];
      for (let i = first; i < last; i++) rows.push(rowEl(i, i * ROW_H, "", () => {}));
      // Pinned rows go over the rows they cover. A press on one scrolls
      // its folder back to where it is (under the rows pinned above it).
      const chain = r.depth.flatMap((depth) => pinnedAt(depth, n, Math.floor(scroll / ROW_H)));
      chain.forEach((i, k) => {
        rows.push(rowEl(i, scroll + k * ROW_H, " sticky" + (k === chain.length - 1 ? " last" : ""), () => { el.scrollTop = (i - k) * ROW_H; }));
      });
      drawn = rows;
      inner.replaceChildren(...rows.map((x) => x.el));
    };
    el.onscroll = (_e) => { st.scroll = el.scrollTop; draw(); };
    el.onkeydown = (e) => {
      if (e.key === "ArrowDown" || e.key === "ArrowUp") { e.preventDefault(); this.listMove(key, e.key === "ArrowDown" ? 1 : -1, e); }
      else if (e.key === "PageDown" || e.key === "PageUp") { e.preventDefault(); this.listMove(key, (e.key === "PageDown" ? 1 : -1) * Math.floor(el.clientHeight / ROW_H), e); }
      else if (e.key === "Enter") { e.preventDefault(); if (n > 0) r.onActivate(st.sel, "key", e); }
      else r.onKey(e, st.sel);
    };
    // In a tree, as many rows as the selected row has ancestors may be
    // pinned over the top.
    const show = () => {
      const top = st.sel * ROW_H;
      const pin = r.depth.filter((_d) => st.sel < n).map((depth) => depth(st.sel) * ROW_H).reduce((a, b) => a + b, 0);
      if (top - pin < el.scrollTop) el.scrollTop = Math.max(0, top - pin);
      else if (top + ROW_H > el.scrollTop + el.clientHeight) el.scrollTop = top + ROW_H - el.clientHeight;
    };
    this.movers.set(key, (d, e) => {
      let i = st.sel + d;
      // Skip header rows.
      while (i > 0 && i < n - 1 && r.header(i)) i += Math.sign(d);
      st.sel = Math.max(0, Math.min(n - 1, i));
      show();
      draw();
      if (n > 0) r.onSelect(st.sel, "key", e);
    });
    requestAnimationFrame((_t) => { el.scrollTop = st.scroll; if (moved) show(); draw(); });
    draw();
    return el;
  }

  // Move a list's selection `d` rows (the key `e` that moved it).
  listMove(key, d, e) {
    for (const move of found(this.movers.get(key))) move(d, e);
  }

  // A tree: its open folders' children shown under them; which are open is
  // kept by id: a folder opened or closed by hand stays so, else it is as
  // its node says (`open`).
  treeView(key, nodes, none) {
    const toggled = this.expanded.get(key) ?? noToggled();
    this.expanded.set(key, toggled);
    const shown = treeRows(nodes, toggled, 0);
    return this.list(rowsOf({
      ...listProps(key, shown, (t, _i) => row({ ...BOX, class: "ui-tree-row" }, [
        text({ ...TEXT, class: "ui-tree-indent" }, " ".repeat(t.depth * 2) + (t.b.leaf ? "  " : t.isOpen ? "▾ " : "▸ ")),
        t.b.label,
      ])),
      depth: [(t) => t.depth],
      activateOnClick: true,
      onActivate: (t, _i, how, _e) => {
        if (t.b.leaf) { t.b.activate(); return; }
        // A folder opens or closes on its first click.
        if (how === "double") return;
        toggled.set(t.b.id, !t.isOpen);
        for (const n of this.shownTree) this.render(n);
        this.focus(key);
      },
      onSelect: (t, _i, _how, _e) => { t.b.select(); },
      onKey: (e, sel, _i) => { if (sel.length === 0) none(e); for (const t of sel) t.b.key(e); },
      onContext: (t, _i, at) => { t.b.context(at); },
    }));
  }
}

// A list row drawn: its index and element.
/** type DrawnRow = {i: Number, el: El} */
// Where a dragged row would drop: onto a row, or (none) below the rows.
/** type DropTarget = {onto: Number[]} */
// A tree's row: its node, how deep, and whether it is open.
/** type TreeRow = {b: Branch, depth: Number, isOpen: Boolean} */

// The rows of `nodes` at `depth`, each open folder's children under it.
// (`toggled`: the folders opened or closed by hand.)
/** function treeRows(nodes: Branch[], toggled: Map<String, Boolean>, depth: Number) => TreeRow[] */
function treeRows(nodes, toggled, depth) {
  return nodes.flatMap((b) => {
    const isOpen = !b.leaf && (toggled.get(b.id) ?? b.open);
    return [{ b, depth, isOpen }, ...(isOpen ? treeRows(b.children, toggled, depth + 1) : [])];
  });
}

// The rows pinned over a tree list scrolled to row `top` (of `n`, each
// `depth(i)` deep): the folders holding the row just under the pinned
// ones, outermost first.
/** function pinnedAt(depth: (Number) => Number, n: Number, top: Number) => Number[] */
function pinnedAt(depth, n, top) {
  /** let chain: Number[] */
  let chain = [];
  for (let guard = 0; guard < 64; guard++) {
    const i = top + chain.length;
    if (i >= n) break;
    const a = ancestorsOf(depth, i);
    if (a.length <= chain.length) break;
    chain = a.slice(0, chain.length + 1);
  }
  return chain;
}
// The rows of the folders holding row `i`, outermost first.
/** function ancestorsOf(depth: (Number) => Number, i: Number) => Number[] */
function ancestorsOf(depth, i) {
  /** const out: Number[] */
  const out = [];
  let d = depth(i);
  for (let k = i - 1; k >= 0 && d > 0; k--) {
    const dk = depth(k);
    if (dk < d) { out.unshift(k); d = dk; }
  }
  return out;
}

// `el` with the classes `cls` (space-separated) added.
/** function withClass(el: El, cls: String) => El */
function withClass(el, cls) {
  for (const c of cls.split(" ")) if (c !== "") el.classList.add(c);
  return el;
}

/** function textEl(p: TextProps, s: String) => El */
function textEl(p, s) {
  const el = document.createElement("span");
  el.className = "ui-text" + (p.dim ? " dim" : "") + (p.mono ? " mono" : "") + (p.bold ? " bold" : "");
  el.textContent = s;
  // A theme key ("syntax.type") or a CSS colour.
  for (const c of p.color) el.style.color = color(c) ?? c;
  el.title = p.title;
  return withClass(el, p.class);
}

/** function markdownEl(s: String) => El */
function markdownEl(s) {
  const el = div("ui-markdown");
  el.innerHTML = markdownHtml(s);
  return el;
}

/** function buttonEl(p: ButtonProps, label: String) => El */
function buttonEl(p, label) {
  const el = document.createElement("button");
  // A `field` button is part of a box beside it (a ▾ for its values):
  // drawn as the box is, not as a button.
  el.className = p.field ? "ui-field-button" : "ui-button" + (p.primary ? " primary" : "");
  el.textContent = label;
  el.disabled = p.disabled;
  el.title = p.title;
  el.onclick = (e) => { p.onClick(e); };
  return el;
}

/** function toggleEl(p: ToggleProps, label: String) => El */
function toggleEl(p, label) {
  const el = document.createElement("button");
  el.className = "ui-toggle" + (p.on ? " on" : "");
  el.dataset.key = p.key;
  el.textContent = label;
  el.title = p.title;
  el.onclick = (_e) => { p.onChange(!p.on); };
  return el;
}

/** function checkboxEl(p: CheckboxProps, label: String) => El */
function checkboxEl(p, label) {
  const el = document.createElement("label");
  el.className = "ui-check" + (p.disabled ? " disabled" : "");
  el.title = p.title;
  const c = document.createElement("input");
  c.type = "checkbox";
  c.dataset.key = p.key;
  c.checked = p.checked;
  c.disabled = p.disabled;
  c.onchange = (_e) => { p.onChange(c.checked); };
  el.appendChild(c);
  el.appendChild(document.createTextNode(label));
  return el;
}

/** function textareaEl(p: TextareaProps) => El */
function textareaEl(p) {
  const el = document.createElement("textarea");
  el.className = "ui-textarea";
  el.dataset.key = p.key;
  el.setAttribute("rows", String(p.rows));
  el.value = p.value;
  el.placeholder = p.placeholder;
  el.oninput = (_e) => { p.onInput(el.value); };
  el.onkeydown = (e) => { if (p.onKey(e)) e.preventDefault(); };
  return el;
}

/** function selectEl(p: SelectProps) => El */
function selectEl(p) {
  const el = document.createElement("select");
  el.className = "ui-select";
  el.dataset.key = p.key;
  for (const o of p.options) {
    const op = document.createElement("option");
    op.value = o.value;
    op.textContent = o.label;
    el.appendChild(op);
  }
  el.value = p.value;
  el.onchange = (_e) => { p.onChange(el.value); };
  return el;
}

// Markdown as HTML, for text a plugin shows (a tour step's description):
// headings, paragraphs, lists, quotes, fenced code, rules, and inline code,
// bold, italic and links. Everything is escaped first; links keep only web,
// mail and relative targets.
// `text` with each match of `re` (a global pattern) replaced by `f` of the
// match and its groups (`m[0]` the match; a group that took no part is
// undefined).
/** function replacing(text: String, re: Regex, f: (String[]) => String) => String */
function replacing(text, re, f) {
  let out = "", last = 0;
  for (;;) {
    const m = re.exec(text);
    if (m === null) break;
    const at = re.lastIndex - m[0].length;
    out += text.slice(last, at) + f(m);
    last = re.lastIndex;
    if (m[0] === "") re.lastIndex = last + 1;
  }
  return out + text.slice(last);
}
const SAFE_URL = /^(https?:|mailto:|#|\.{0,2}\/|[\w.-]+(\/|$|#))/i;
/** function inline(t: String) => String */
function inline(t) {
  return t.split(/(`[^`]+`)/).map((part, i) => {
    if (i % 2) return `<code>${escapeHtml(part.slice(1, -1))}</code>`;
    const links = replacing(escapeHtml(part), /\[([^\]]+)\]\(([^)\s]+)\)/g, (m) => (SAFE_URL.test(m[2]) ? `<a href="${m[2]}" target="_blank" rel="noopener">${m[1]}</a>` : m[1]));
    const strong = replacing(links, /\*\*([^*]+)\*\*|__([^_]+)__/g, (m) => `<strong>${m[1] || m[2]}</strong>`);
    return replacing(strong, /(^|[^\w*])\*([^*\s][^*]*)\*|(^|\W)_([^_\s][^_]*)_(?!\w)/g, (m) => `${m[1] ?? m[3]}<em>${m[2] ?? m[4]}</em>`);
  }).join("");
}
// A list being read: its tag ("ul", "ol") and its items so far.
/** type MdList = {tag: String, items: String[]} */
/** function markdownHtml(text: String) => String */
export function markdownHtml(text) {
  /** const out: String[] */
  const out = [];
  const lines = text.replace(/\r\n?/g, "\n").split("\n");
  /** let para: String[] */
  let para = [];
  /** let list: MdList[] */
  let list = []; // the list being read: a list of at most one
  const flush = () => {
    if (para.length) out.push(`<p>${inline(para.join(" "))}</p>`);
    para = [];
    for (const x of list) out.push(`<${x.tag}>${x.items.map((y) => `<li>${inline(y)}</li>`).join("")}</${x.tag}>`);
    list = [];
  };
  for (let i = 0; i < lines.length; i++) {
    const l = lines[i];
    const fence = /^\s*(```|~~~)/.exec(l);
    const heading = /^(#{1,6})\s+(.*)$/.exec(l);
    const quote = /^\s*>\s?(.*)$/.exec(l);
    const item = /^\s*(?:([-*+])|\d+[.)])\s+(.*)$/.exec(l);
    if (fence !== null) {
      flush();
      const code = [];
      while (++i < lines.length && !lines[i].trimStart().startsWith(fence[1])) code.push(lines[i]);
      out.push(`<pre><code>${escapeHtml(code.join("\n"))}</code></pre>`);
    } else if (heading !== null) {
      flush();
      out.push(`<h${heading[1].length}>${inline(heading[2])}</h${heading[1].length}>`);
    } else if (/^\s*([-*_])(\s*\1){2,}\s*$/.test(l)) {
      flush();
      out.push("<hr>");
    } else if (quote !== null) {
      flush();
      out.push(`<blockquote>${inline(quote[1])}</blockquote>`);
    } else if (item !== null) {
      const tag = item[1] ? "ul" : "ol";
      if (para.length || list.some((x) => x.tag !== tag)) flush();
      list = list.length ? list.map((x) => ({ tag: x.tag, items: [...x.items, item[2]] })) : [{ tag, items: [item[2]] }];
    } else if (!l.trim()) flush();
    else if (list.length && /^\s+\S/.test(l)) list = list.map((x) => ({ tag: x.tag, items: [...x.items.slice(0, -1), `${x.items.at(-1) ?? ""} ${l.trim()}`] }));
    else {
      if (list.length) flush();
      para.push(l.trim());
    }
  }
  flush();
  return out.join("");
}

// The keyed control in `body` that has the keyboard, and its selection: a
// list of at most one.
function focusIn(body) {
  const active = document.activeElement;
  if (active === null || !body.contains(active)) return [];
  return found(active.dataset.key).map((key) => ({ key, start: active.selectionStart, end: active.selectionEnd }));
}

function div(cls) {
  const d = document.createElement("div");
  d.className = cls;
  return d;
}
