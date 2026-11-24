// Menus: the one component every menu is made of — the menu bar's menus
// and their submenus, and the context menus (the explorer's, a tab's, the
// editor's). Its rows, highlight, keyboard, submenus, placement and closing
// live here once.
//
// An item is {label, run, keys?, checked?, disabled?, items?} (`items`
// makes a submenu; `checked` true or false gives the row a check column);
// `null` is a separator. A menu is `.ctxmenu` with `.menu-item` rows
// (`.hi` highlighted, `.sub`, `.disabled`) and `hr` separators.
//
// The keyboard: Up / Down (Home / End) move, Enter or Space runs, Right
// opens a submenu (or, at the menu bar, the next menu), Left closes one (or
// the previous menu), a letter moves to the next row starting with it,
// Escape closes. While a menu is open it has the keyboard; closing gives it
// back to the editor.

/** A menu of `items`. `o`: `onSide(d)` — Left / Right past its edge;
 *  `onClose()`; `ignore` — a selector whose presses do not close it (the
 *  menu bar's titles); `parent` — a submenu's menu; `app`. */
export class Menu {
  constructor(items, o) {
    this.items = items;
    this.o = o;
    this.parent = o.parent || null;
    this.sub = null;
    this.from = -1; // a submenu: its row in the parent
    this.hi = -1;
    this.rows = [];
    this.closed = false;
    this.away = null;
    this.el = document.createElement("div");
  }

  // Build the rows and take the keyboard and pointer.
  build() {
    const el = this.el;
    el.className = "ctxmenu";
    el.tabIndex = -1;
    el.setAttribute("role", "menu");
    el.classList.toggle("checks", this.items.some((it) => it && typeof it.checked === "boolean"));
    // Pressing in a menu never takes the focus (or a selection) anywhere.
    el.addEventListener("mousedown", (e) => e.preventDefault());
    el.addEventListener("keydown", (e) => { if (this.key(e)) { e.preventDefault(); e.stopPropagation(); } });
    for (const it of this.items) {
      if (!it) { el.appendChild(document.createElement("hr")); continue; }
      const i = this.rows.length;
      const row = document.createElement("div");
      row.className = "menu-item" + (it.items ? " sub" : "") + (it.disabled ? " disabled" : "");
      row.setAttribute("role", typeof it.checked === "boolean" ? "menuitemcheckbox" : "menuitem");
      if (typeof it.checked === "boolean") row.setAttribute("aria-checked", String(it.checked));
      for (const [cls, text] of [["check", it.checked ? "✓" : ""], ["label", it.label], ["keys", it.items ? "›" : it.keys || ""]]) {
        const s = document.createElement("span");
        s.className = cls;
        s.textContent = text;
        row.appendChild(s);
      }
      // A move, not an enter: a menu opened under a resting pointer must
      // not take the highlight from the keyboard.
      row.addEventListener("mousemove", () => { if (this.hi !== i) this.highlight(i, true); });
      row.addEventListener("click", (e) => { e.stopPropagation(); this.activate(i); });
      this.rows.push({ row, it });
      el.appendChild(row);
    }
    return this;
  }

  root() { return this.parent ? this.parent.root() : this; }

  highlight(i, pointer) {
    this.hi = i;
    this.rows.forEach((r, k) => r.row.classList.toggle("hi", k === i));
    const it = i >= 0 ? this.rows[i].it : null;
    // From the pointer, a submenu row opens its submenu; moving off the row
    // closes it.
    if (pointer && it && it.items && !it.disabled) this.openSub(i, false);
    else if (this.sub && this.sub.from !== i) this.closeSub();
  }

  step(d) {
    const n = this.rows.length;
    for (let k = 1; k <= n; k++) {
      const i = (((this.hi < 0 && d < 0 ? 0 : this.hi) + d * k) % n + n) % n;
      if (!this.rows[i].it.disabled) { this.highlight(i, false); return; }
    }
  }

  openSub(i, withKeys) {
    if (this.sub && this.sub.from === i) { if (withKeys) this.sub.enter(); return; }
    this.closeSub();
    const r = this.rows[i].row.getBoundingClientRect();
    const sub = new Menu(this.rows[i].it.items, { parent: this, app: this.o.app }).build();
    sub.from = i;
    this.sub = sub;
    document.body.appendChild(sub.el);
    // To the right of its row, or to the left when there is no room.
    place(sub.el, r.right, r.top - 4, r.left);
    if (withKeys) sub.enter();
  }

  closeSub() {
    if (!this.sub) return;
    this.sub.closeSub();
    this.sub.el.remove();
    this.sub = null;
  }

  // Take the keyboard, the first row highlighted.
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
    const it = this.hi >= 0 ? this.rows[this.hi].it : null;
    switch (e.key) {
      case "ArrowDown": this.step(1); return true;
      case "ArrowUp": this.step(-1); return true;
      case "Home": this.hi = -1; this.step(1); return true;
      case "End": this.hi = 0; this.step(-1); return true;
      case "Enter": case " ": if (this.hi >= 0) this.activate(this.hi); return true;
      case "ArrowRight":
        if (it && it.items && !it.disabled) this.openSub(this.hi, true);
        else if (this.root().o.onSide) this.root().o.onSide(1);
        return true;
      case "ArrowLeft":
        if (this.parent) { this.parent.closeSub(); this.parent.el.focus({ preventScroll: true }); }
        else if (this.o.onSide) this.o.onSide(-1);
        return true;
      case "Escape":
        if (this.parent) { this.parent.closeSub(); this.parent.el.focus({ preventScroll: true }); }
        else this.close();
        return true;
      case "Tab": return true;
    }
    // A letter moves to the next row starting with it.
    if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      const c = e.key.toLowerCase(), n = this.rows.length;
      for (let k = 1; k <= n; k++) {
        const i = (this.hi + k + n) % n;
        const x = this.rows[i].it;
        if (!x.disabled && x.label.toLowerCase().startsWith(c)) { this.highlight(i, false); break; }
      }
      return true;
    }
    return false;
  }

  // Show it with its top-left corner at (x, y), kept in the window.
  openAt(x, y) {
    document.body.appendChild(this.el);
    place(this.el, x, y, x);
    this.listen();
  }

  // Show it under `rect` (a menu bar title), or beside it when the titles
  // stand in a column.
  openUnder(rect, beside) {
    document.body.appendChild(this.el);
    if (beside) place(this.el, rect.right, rect.top, rect.left);
    else place(this.el, rect.left, rect.bottom, rect.left);
    this.listen();
  }

  listen() {
    this.el.focus({ preventScroll: true });
    this.away = (e) => {
      if (this.el.contains(e.target) || (this.sub && within(this.sub, e.target))) return;
      if (this.o.ignore && e.target.closest && e.target.closest(this.o.ignore)) return;
      this.close();
    };
    window.addEventListener("mousedown", this.away, true);
    window.addEventListener("blur", this.away);
  }

  // Close it and its submenus; the keyboard goes back to the editor.
  close() {
    if (this.closed) return;
    this.closed = true;
    this.closeSub();
    window.removeEventListener("mousedown", this.away, true);
    window.removeEventListener("blur", this.away);
    this.el.remove();
    const app = this.o.app;
    if (app && (!document.activeElement || document.activeElement === document.body || document.activeElement === this.el)) app.focusPane(app.activePane);
    if (this.o.onClose) this.o.onClose();
  }
}

function within(menu, target) {
  return menu.el.contains(target) || (menu.sub ? within(menu.sub, target) : false);
}

// Put a fixed box at (x, y), kept inside the window: when it would cross
// the right edge it goes left of `altX` instead, and up when it would cross
// the bottom. Measured once, after it is in the page.
function place(box, x, y, altX) {
  box.style.left = `${x}px`;
  box.style.top = `${y}px`;
  const r = box.getBoundingClientRect();
  if (r.right > innerWidth - 4) box.style.left = `${Math.max(4, (altX === x ? innerWidth - 4 : altX) - r.width)}px`;
  if (r.bottom > innerHeight - 4) {
    const room = innerHeight - 8;
    if (r.height > room) { box.style.maxHeight = `${room}px`; box.style.overflowY = "auto"; }
    box.style.top = `${Math.max(4, innerHeight - 4 - Math.min(r.height, room))}px`;
  }
}
