// Menus: the one component every menu the client draws is made of — the
// menu bar's menus and their submenus, and the context menus (explorer,
// tabs, editor, the tab bar's "+"). Its rows, highlight, mouse, keyboard,
// submenus, placement and closing live here once.
//
// What the core owns is not drawn with this: the palette and other prompts,
// confirmations, popups and plugin panels keep their selection in the core,
// and their keys go there.
//
// An item is {label, run, keys?, checked?, disabled?, items?} (`items` makes
// a submenu), or {separator: true}, or {info: text}; `null` is a separator.
// The items may also be a promise of them (rows the core is asked for): the
// menu opens at once and has the keyboard; keys pressed before the rows
// arrive are kept and replayed on them, and Escape closes it meanwhile.

class Menu {
  /** `opts`: `onSide(dir)` — Left/Right past the menu's edge (the menu bar
   *  switches menus); `onClose()`; `ignore` — a selector whose presses do
   *  not close it (the menu bar's titles); `parent` — a submenu's menu. */
  constructor(items, opts = {}) {
    this.opts = opts;
    this.parent = opts.parent || null;
    this.sub = null;
    this.hi = -1;
    this.el = div("dropdown" + (opts.className ? " " + opts.className : ""));
    this.el.setAttribute("role", "menu");
    this.el.tabIndex = -1;
    this.el.addEventListener("keydown", (e) => { if (this.key(e)) { e.preventDefault(); e.stopPropagation(); } });
    if (items && typeof items.then === "function") {
      this.waiting = [];
      this.el.setAttribute("aria-busy", "true");
      this.setItems([{ info: "…" }]);
      items.then((it) => this.arrived(it), () => this.arrived([]));
    } else {
      this.setItems(items);
    }
  }

  /** The rows came: show them, keep the menu in its place, replay the keys
   *  pressed while it waited. */
  arrived(items) {
    if (this.closed) return;
    const keys = this.waiting;
    this.waiting = null;
    this.el.removeAttribute("aria-busy");
    this.setItems(items);
    if (this.anchor) keepInside(this.el, true, this.anchor);
    for (const e of keys) this.key(e);
  }

  /** Build the rows for `items`. */
  setItems(items) {
    this.items = items;
    this.el.replaceChildren();
    this.el.classList.toggle("checks", items.some((it) => it && it.checked !== undefined && it.checked !== null));
    this.rows = [];
    for (const it of items) {
      if (!it || it.separator) { this.el.appendChild(div("msep")); continue; }
      if (it.info !== undefined) { this.el.appendChild(el("div", "minfo", it.info)); continue; }
      const i = this.rows.length;
      const row = div("mitem" + (it.items ? " sub" : "") + (it.disabled ? " disabled" : ""));
      const checkable = it.checked !== undefined && it.checked !== null;
      row.setAttribute("role", checkable ? "menuitemcheckbox" : "menuitem");
      if (checkable) row.setAttribute("aria-checked", String(!!it.checked));
      if (it.disabled) row.setAttribute("aria-disabled", "true");
      row.append(el("span", "mcheck", it.checked ? "✓" : ""), el("span", "mlabel", it.label),
        el("span", "mkeys", it.items ? "›" : keyLabel(it.keys || "")));
      // A move, not an enter: a menu rebuilt under a resting pointer must
      // not take the highlight from the keyboard.
      row.addEventListener("mousemove", () => { if (this.hi !== i) this.highlight(i, true); });
      row.addEventListener("mouseup", (e) => { e.stopPropagation(); this.activate(i); });
      this.rows.push({ row, it });
      this.el.appendChild(row);
    }
    if (this.hi >= this.rows.length) this.hi = -1;
  }

  root() { return this.parent ? this.parent.root() : this; }

  /** Highlight row `i`; from the pointer, a submenu row opens its submenu. */
  highlight(i, pointer) {
    this.hi = i;
    this.rows.forEach((r, k) => r.row.classList.toggle("hi", k === i));
    if (i >= 0) this.rows[i].row.scrollIntoView({ block: "nearest" });
    const it = i >= 0 ? this.rows[i].it : null;
    if (pointer && it && it.items && !it.disabled) this.openSub(i, false);
    // A submenu belongs to its row: moving off that row, by pointer or by
    // key, closes it.
    else if (this.sub && this.sub.from !== i) this.closeSub();
  }

  /** Move the highlight by `d` over the rows that can be chosen. */
  step(d) {
    const n = this.rows.length;
    for (let k = 1; k <= n; k++) {
      const i = ((this.hi < 0 && d < 0 ? 0 : this.hi) + d * k + n * k) % n;
      if (!this.rows[i].it.disabled) return this.highlight(i, false);
    }
  }

  openSub(i, withKeys) {
    if (this.sub && this.sub.from === i) { if (withKeys) this.sub.enter(); return; }
    this.closeSub();
    const r = this.rows[i].row.getBoundingClientRect();
    const sub = new Menu(this.rows[i].it.items, { parent: this, className: this.opts.className });
    sub.from = i;
    this.sub = sub;
    // Placed in the window (position: fixed) so a parent that scrolls does
    // not clip it; to the right, or to the left when there is no room.
    sub.anchor = this.root().anchor;
    sub.el.style.left = r.right + "px";
    sub.el.style.top = (r.top - 4) + "px";
    this.el.appendChild(sub.el);
    const bounds = boundsFor(sub.anchor);
    if (sub.el.getBoundingClientRect().right > bounds.right) sub.el.style.left = (r.left - sub.el.offsetWidth) + "px";
    keepInside(sub.el, true, sub.anchor);
    if (withKeys) sub.enter();
  }

  closeSub() {
    if (!this.sub) return;
    this.sub.closeSub();
    this.sub.el.remove();
    this.sub = null;
  }

  /** Give this menu the keyboard, its first row highlighted. */
  enter() {
    this.el.focus({ preventScroll: true });
    if (this.hi < 0) this.step(1);
  }

  activate(i) {
    const { it } = this.rows[i];
    if (it.disabled) return;
    if (it.items) { this.openSub(i, true); return; }
    this.root().close();
    it.run();
  }

  key(e) {
    // Waiting for the rows: Escape closes; other keys are kept for them.
    if (this.waiting) {
      if (e.key === "Escape") { this.close(); return true; }
      this.waiting.push({ key: e.key, ctrlKey: e.ctrlKey, metaKey: e.metaKey, altKey: e.altKey, shiftKey: e.shiftKey });
      return true;
    }
    const it = this.hi >= 0 ? this.rows[this.hi].it : null;
    switch (e.key) {
      case "ArrowDown": this.step(1); return true;
      case "ArrowUp": this.step(-1); return true;
      case "Home": this.hi = -1; this.step(1); return true;
      case "End": this.hi = 0; this.step(-1); return true;
      case "Enter": case " ": if (this.hi >= 0) this.activate(this.hi); return true;
      case "ArrowRight":
        if (it && it.items && !it.disabled) this.openSub(this.hi, true);
        else if (this.root().opts.onSide) this.root().opts.onSide(1);
        return true;
      case "ArrowLeft":
        if (this.parent) { this.parent.closeSub(); this.parent.el.focus({ preventScroll: true }); }
        else if (this.opts.onSide) this.opts.onSide(-1);
        return true;
      case "Escape":
        if (this.parent) { this.parent.closeSub(); this.parent.el.focus({ preventScroll: true }); }
        else this.close();
        return true;
      case "Tab": return true;
    }
    // A letter jumps to the next row that starts with it.
    if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      const c = e.key.toLowerCase(), n = this.rows.length;
      for (let k = 1; k <= n; k++) {
        const i = (this.hi + k + n) % n;
        const { it: x } = this.rows[i];
        if (!x.disabled && x.label.toLowerCase().startsWith(c)) { this.highlight(i, false); break; }
      }
      return true;
    }
    return false;
  }

  /** Show it at (x, y) in the overlay layer `layer`, inside the client. */
  openAt(x, y, layer) {
    this.layer = layer;
    this.anchor = { left: x, top: y };
    this.el.style.left = x + "px";
    this.el.style.top = y + "px";
    overlay(layer, this.el);
    keepInside(this.el, true, this.anchor);
    this.listen();
  }

  /** Show it hanging from `rect` (a title): never over it — taller than the
   *  room below, it scrolls (a press's release over it would run a row). */
  openBelow(rect, layer) {
    this.layer = layer;
    this.anchor = rect;
    this.el.style.left = rect.left + "px";
    this.el.style.top = rect.bottom + "px";
    overlay(layer, this.el);
    const room = boundsFor(rect).bottom - rect.bottom - 4;
    if (this.el.offsetHeight > room) { this.el.style.maxHeight = room + "px"; this.el.style.overflowY = "auto"; }
    keepInside(this.el, false, rect);
    this.listen();
  }

  /** Show it beside `rect`, level with it: a title in a menu bar a skin
   *  laid out as a column. */
  openBeside(rect, layer) {
    this.layer = layer;
    this.anchor = rect;
    this.el.style.left = rect.right + "px";
    this.el.style.top = rect.top + "px";
    overlay(layer, this.el);
    const b = boundsFor(rect);
    if (this.el.offsetHeight > b.height) { this.el.style.maxHeight = b.height + "px"; this.el.style.overflowY = "auto"; }
    keepInside(this.el, true, rect);
    this.listen();
  }

  listen() {
    this.el.focus({ preventScroll: true });
    this.away = (e) => {
      if (this.el.contains(e.target)) return;
      if (this.opts.ignore && e.target.closest && e.target.closest(this.opts.ignore)) return;
      this.close();
    };
    window.addEventListener("mousedown", this.away, true);
  }

  /** Close it (and its submenus); the keyboard goes back to the editor. */
  close() {
    if (this.closed) return;
    this.closed = true;
    window.removeEventListener("mousedown", this.away, true);
    if (document.querySelector(`#overlays > [data-layer="${this.layer}"]`) === this.el) overlay(this.layer, null);
    focusKeyboard();
    if (this.opts.onClose) this.opts.onClose();
  }
}

/** A context menu at (x, y); `items` may be a promise of them. */
function contextMenu(x, y, items) {
  new Menu(items, { className: "context" }).openAt(x, y, "context");
}

/** The core's menu for a place (`at`: a tab, the "+", an explorer entry —
 *  see the API's `menu.rows`) as Menu items: choosing a row runs it there,
 *  as the terminal's menu would, plugins' rows included. */
async function coreMenuItems(at) {
  let rows = [];
  try { rows = (await api.call("menu.rows", at)).rows; } catch (e) { showError(e); }
  return rows.map((r) => ({
    label: r.label, disabled: r.disabled,
    // By the row's id: if the menu changed meanwhile, a row that is gone is
    // refused rather than another one run.
    run: () => api.call("menu.choose", { ...at, row: r.id }).catch(showError),
  }));
}
