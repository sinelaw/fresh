// Menus: the one component every menu is made of — the menu bar's menus
// and their submenus, and the context menus (the explorer's, a tab's, the
// editor's). Its rows, highlight, keyboard, submenus, placement and closing
// live here once.
//
// An item is a MenuItem (types/fresh.d.js), made by `action`, `actionOf`,
// `submenu` or SEPARATOR: an action's `checked` (true or false, not null)
// gives the row a check column. A menu is `.ctxmenu` with `.menu-item`
// rows (`.hi` highlighted, `.sub`, `.disabled`) and `hr` separators.
//
// The keyboard: Up / Down (Home / End) move, Enter or Space runs, Right
// opens a submenu (or, at the menu bar, the next menu), Left closes one (or
// the previous menu), a letter moves to the next row starting with it,
// Escape closes. While a menu is open it has the keyboard; closing gives it
// back to the editor.

// A row that runs `run`, with `keys` shown beside it, a check when
// `checked` is true or false, greyed out when `disabled`.
/** function actionOf<R>(label: String, keys: String | Null, checked: Boolean | Null, disabled: Boolean, run: () => R) => MenuItem */
export function actionOf(label, keys, checked, disabled, run) {
  return { kind: "action", label, keys, checked, disabled, run: () => { run(); } };
}
/** function action<R>(label: String, keys: String | Null, run: () => R) => MenuItem */
export function action(label, keys, run) { return actionOf(label, keys, null, false, run); }
/** function submenu(label: String, items: MenuItem[]) => MenuItem */
export function submenu(label, items) { return { kind: "submenu", label, items }; }
/** const SEPARATOR: MenuItem */
export const SEPARATOR = { kind: "separator" };

// A row's label, and whether it can be chosen (a separator is no row).
/** function labelOf(it: MenuItem) => String */
function labelOf(it) { return it.kind === "separator" ? "" : it.label; }
/** function enabled(it: MenuItem) => Boolean */
function enabled(it) { return it.kind === "submenu" || (it.kind === "action" && !it.disabled); }
// A row's submenu items, when it opens one.
/** function subItems(it: MenuItem) => MenuItem[] | Null */
function subItems(it) { return it.kind === "submenu" ? it.items : null; }
// What a row shows: its check, label and keys (› for a submenu).
/** function cells(it: MenuItem) => String[] */
function cells(it) {
  if (it.kind === "submenu") return ["", it.label, "›"];
  if (it.kind === "action") return [it.checked ? "✓" : "", it.label, it.keys ?? ""];
  return ["", "", ""];
}
/** function checkedOf(it: MenuItem) => Boolean | Null */
function checkedOf(it) { return it.kind === "action" ? it.checked : null; }
// The next row from `from` in direction `d` (round the ends) that can be
// chosen and that `ok` accepts; null when none is.
/** function nextRow(items: MenuItem[], from: Number | Null, d: Number, ok: (MenuItem) => Boolean) => Number | Null */
function nextRow(items, from, d, ok) {
  const n = items.length;
  const start = from === null ? (d > 0 ? -1 : n) : from;
  for (let k = 1; k <= n; k++) {
    const i = (((start + d * k) % n) + n) % n;
    if (enabled(items[i]) && ok(items[i])) return i;
  }
  return null;
}

// `f()`, when there is an `f`.
/** function callIf(f: (() => Undefined) | Null) => Undefined */
function callIf(f) { if (f !== null) f(); }
// `f(d)`, when there is an `f` (a menu's Left / Right past its edge).
/** function sideIf(f: ((Number) => Undefined) | Null, d: Number) => Undefined */
function sideIf(f, d) { if (f !== null) f(d); }

// A key's name (KeyboardEvent.key).
/** function keyName(k: String) => String */
function keyName(k) { return k; }

// Nothing highlighted, no submenu open (a menu's `at.hi`, `at.subFrom`).
/** const NO_ROW: Number | Null */
const NO_ROW = null;

// How a menu behaves: `app`; `ignore`, a selector whose presses do not
// close it (the menu bar's titles); `onSide(d)`, Left / Right past its
// edge; `onClose()`; `from`, a submenu's row in its parent.
/** type MenuSpec<A> = {
 *   app: () => A,
 *   ignore: String | Null,
 *   onSide: ((Number) => Undefined) | Null,
 *   onClose: (() => Undefined) | Null,
 *   from: Number | Null,
 * } */
/** function menuSpec<A>(app: () => A, ignore: String | Null, onSide: ((Number) => Undefined) | Null, onClose: (() => Undefined) | Null, from: Number | Null) => MenuSpec<A> */
export function menuSpec(app, ignore, onSide, onClose, from) { return { app, ignore, onSide, onClose, from }; }

// What a submenu may do to the menu it opened from: close it (the
// submenu), give it back the keyboard, close it and every menu above.
/** type MenuUp = {closeSub: () => Undefined, focus: () => Undefined, closeAll: () => Undefined} */

/** A menu of `items`, behaving as `o` (a MenuSpec) says; `up`: for a
 *  submenu, the menu it opened from (a list of at most one). Built, not
 *  shown: show it with openAt, openUnder or openBelow. */
export function makeMenu(all, o, up) {
  // The rows: every item but the separators, each with its element.
  const items = all.filter((it) => it.kind !== "separator");
  const el = document.createElement("div");
  const rowEls = [];
  /** let hi: Number | Null */
  let hi = NO_ROW; // the highlighted row
  let subs = []; // the open submenu: a list of at most one
  let closed = false;

  const closeSub = () => {
    for (const open of subs) { open.closeSub(); open.el.remove(); }
    subs = [];
  };
  // A press outside it (and its submenus, and `ignore`) closes it.
  const contains = (t) => el.contains(t) || subs.some((open) => open.contains(t));
  const away = (e) => {
    const t = e.target;
    if (contains(t)) return;
    const ignore = o.ignore;
    if (ignore !== null && t.closest(ignore) !== null) return;
    close();
  };
  // Close it and its submenus; the keyboard goes back to the editor.
  const close = () => {
    if (closed) return;
    closed = true;
    closeSub();
    window.removeEventListener("pointerdown", away);
    window.removeEventListener("blur", away);
    el.remove();
    const app = o.app();
    const focused = document.activeElement;
    if (!focused || focused === document.body || focused === el) app.focusPane(app.activePane);
    callIf(o.onClose);
  };
  // Close it and every menu it opened from.
  const closeAll = () => {
    close();
    for (const u of up) u.closeAll();
  };
  const focus = () => { el.focus(); };
  const enter = () => {
    focus();
    if (hi === null) step(1);
  };
  const openSub = (i, withKeys) => {
    if (subs.some((open) => open.from === i)) { if (withKeys) for (const open of subs) open.enter(); return; }
    closeSub();
    const its = subItems(items[i]);
    if (its === null) return;
    const r = rowEls[i].getBoundingClientRect();
    // Its Left / Right past the edge are the top menu's.
    const sub = makeMenu(its, menuSpec(o.app, null, o.onSide, null, i), [{ closeSub, focus, closeAll }]);
    subs = [sub];
    // To the right of its row, or to the left when there is no room.
    sub.putAt(r.right, r.top - 4, r.left);
    if (withKeys) sub.enter();
  };
  const highlight = (i, pointer) => {
    hi = i;
    rowEls.forEach((r, k) => { if (k === i) r.classList.add("hi"); else r.classList.remove("hi"); });
    // From the pointer, a submenu row opens its submenu; moving off the row
    // closes it.
    if (pointer && subItems(items[i]) !== null) openSub(i, false);
    else if (subs.some((open) => open.from !== i)) closeSub();
  };
  function step(d) {
    const i = nextRow(items, hi, d, () => true);
    if (i !== null) highlight(i, false);
  }
  const activate = (i) => {
    const it = items[i];
    if (it.kind === "submenu") { openSub(i, true); return; }
    if (it.kind !== "action" || it.disabled) return;
    closeAll();
    it.run();
  };
  // Back to the menu it opened from (or, at the top, `elsewhere()`).
  const back = (elsewhere) => {
    if (!up.length) { elsewhere(); return; }
    for (const u of up) { u.closeSub(); u.focus(); }
  };
  const sideways = (d) => { sideIf(o.onSide, d); };
  // A key pressed while the menu has the keyboard; true when it was the
  // menu's.
  const key = (e) => {
    const at = hi;
    switch (keyName(e.key)) {
      case "ArrowDown": step(1); return true;
      case "ArrowUp": step(-1); return true;
      case "Home": hi = NO_ROW; step(1); return true;
      case "End": hi = NO_ROW; step(-1); return true;
      case "Enter": case " ": if (at !== null) activate(at); return true;
      case "ArrowRight":
        if (at !== null && subItems(items[at]) !== null) openSub(at, true);
        else sideways(1);
        return true;
      case "ArrowLeft": back(() => { sideways(-1); }); return true;
      case "Escape": back(() => { close(); }); return true;
      case "Tab": return true;
    }
    // A letter moves to the next row starting with it.
    if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      const c = e.key.toLowerCase();
      const i = nextRow(items, at, 1, (it) => labelOf(it).toLowerCase().startsWith(c));
      if (i !== null) highlight(i, false);
      return true;
    }
    return false;
  };
  // In the page: it takes the keyboard, and a pointer press anywhere else
  // (not a mouse press, which a control may stop on its way up) closes
  // it, as does leaving the window.
  // In the page at (x, y), kept in the window (placed).
  const put = (x, y, altX) => {
    document.body.appendChild(el);
    const r = el.getBoundingClientRect();
    const at = placed(r.width, r.height, x, y, altX);
    el.style.left = `${at.left}px`;
    el.style.top = `${at.top}px`;
    const most = at.maxHeight;
    if (most !== null) { el.style.maxHeight = `${most}px`; el.style.overflowY = "auto"; }
  };
  const shown = () => {
    focus();
    window.addEventListener("pointerdown", away);
    window.addEventListener("blur", away);
  };

  el.className = "ctxmenu";
  el.tabIndex = -1;
  el.setAttribute("role", "menu");
  if (items.some((it) => checkedOf(it) !== null)) el.classList.add("checks");
  // Pressing in a menu never takes the focus (or a selection) anywhere.
  el.addEventListener("mousedown", (e) => { e.preventDefault(); });
  el.addEventListener("keydown", (e) => { if (key(e)) { e.preventDefault(); e.stopPropagation(); } });
  for (const it of all) {
    if (it.kind === "separator") { el.appendChild(document.createElement("hr")); continue; }
    const i = rowEls.length;
    const row = document.createElement("div");
    row.className = "menu-item" + (it.kind === "submenu" ? " sub" : "") + (enabled(it) ? "" : " disabled");
    const checked = checkedOf(it);
    row.setAttribute("role", checked !== null ? "menuitemcheckbox" : "menuitem");
    if (checked !== null) row.setAttribute("aria-checked", String(checked));
    const [check, label, keys] = cells(it);
    for (const [cls, text] of [["check", check], ["label", label], ["keys", keys]]) {
      const s = document.createElement("span");
      s.className = cls;
      s.textContent = text;
      row.appendChild(s);
    }
    // A move, not an enter: a menu opened under a resting pointer must
    // not take the highlight from the keyboard.
    row.addEventListener("mousemove", () => { if (hi !== i) highlight(i, true); });
    row.addEventListener("click", (e) => { e.stopPropagation(); activate(i); });
    rowEls.push(row);
    el.appendChild(row);
  }

  return {
    el,
    from: o.from,
    enter,
    close,
    closeSub,
    contains,
    // A submenu in the page at (x, y) (left of `altX` when there is no
    // room); its parent has the keyboard and the pointer.
    putAt: put,
    // Shown with its top-left corner at (x, y), kept in the window.
    openAt: (x, y) => { put(x, y, x); shown(); },
    // Shown under `rect` (a menu bar title), or beside it when the titles
    // stand in a column.
    openUnder: (rect, beside) => {
      if (beside) put(rect.right, rect.top, rect.left);
      else put(rect.left, rect.bottom, rect.left);
      shown();
    },
    // Shown dropping from `rect` (a dropdown's control): under it, from
    // its left edge, or ending at its right edge when `fromRight` or when
    // it would cross the window's.
    openBelow: (rect, fromRight) => {
      put(rect.left, rect.bottom, rect.right);
      if (fromRight) el.style.left = `${Math.max(4, rect.right - el.getBoundingClientRect().width)}px`;
      shown();
    },
  };
}

// Where a fixed box `w` × `h` goes for (x, y), kept inside the window:
// when it would cross the right edge it goes left of `altX` instead, and
// up when it would cross the bottom (no taller than the window: then it
// scrolls, `maxHeight` set).
/** type Placed = {left: Number, top: Number, maxHeight: Number | Null} */
/** function placed(w: Number, h: Number, x: Number, y: Number, altX: Number) => Placed */
function placed(w, h, x, y, altX) {
  const left = x + w > innerWidth - 4 ? Math.max(4, (altX === x ? innerWidth - 4 : altX) - w) : x;
  const room = innerHeight - 8;
  if (y + h <= innerHeight - 4) return { left, top: y, maxHeight: null };
  return { left, top: Math.max(4, innerHeight - 4 - Math.min(h, room)), maxHeight: h > room ? room : null };
}
