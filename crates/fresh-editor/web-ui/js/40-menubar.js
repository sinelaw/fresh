// The menu bar: the core's menus as data (labels, actions, keys, enabled,
// checked), drawn and driven by the client. Choosing an item runs its action
// in the core — or here, for the few actions whose UI is the client's own.

/** Actions whose screen is the client's, not the terminal's. */
const CLIENT_ACTIONS = {
  open_settings: () => openSettings(),
  toggle_file_explorer: () => toggleExplorer(),
  focus_file_explorer: () => focusExplorer(),
  toggle_menu_bar: () => {},
  open_keybinding_editor: () => openKeybindings(),
};

function runAction(action, args) {
  const local = CLIENT_ACTIONS[action];
  if (local) { local(args); return; }
  const v = activeView();
  api.send("menu.run", { action, args: args || {}, ...(v && v.id !== null ? { view: v.id } : {}) });
  focusKeyboard();
}

const menubar = {
  open: -1,
  menu: null,
  host: () => document.getElementById("menus"),
};

/** The core's menu items as the Menu component's. */
function menuItems(items) {
  return items.map((it) => {
    if (it.separator) return null;
    if (it.info !== undefined) return { info: it.info };
    return {
      label: it.label, keys: it.keys, checked: it.checked, disabled: it.enabled === false,
      items: it.items ? menuItems(it.items) : undefined,
      run: () => runAction(it.action, it.args),
    };
  });
}

function renderMenubar() {
  const host = menubar.host();
  host.replaceChildren(...core.menus.map((m, i) => {
    const b = el("button", "mtop" + (menubar.open === i ? " open" : ""), m.label);
    b.setAttribute("role", "menuitem");
    b.setAttribute("aria-haspopup", "true");
    b.setAttribute("aria-expanded", String(menubar.open === i));
    b.addEventListener("mousedown", (e) => {
      e.preventDefault();
      if (menubar.open === i) closeMenus(); else openMenu(i);
    });
    // Moving onto another title switches to its menu. A move, not an
    // enter: a pointer resting on a title must not take the menu back from
    // the keyboard.
    b.addEventListener("mousemove", () => { if (menubar.open >= 0 && menubar.open !== i) openMenu(i); });
    return b;
  }));
}

/** Open menu `i` at its title; `withKeys` highlights its first row (it
 *  was opened from the keyboard). Left/Right past its edge move to the
 *  next title's menu. */
function openMenu(i, withKeys) {
  const prev = menubar.menu;
  menubar.menu = null;
  if (prev) prev.close();
  if (i < 0 || !core.menus[i]) return;
  menubar.open = i;
  renderMenubar();
  const n = core.menus.length;
  const menu = new Menu(menuItems(core.menus[i].items), {
    ignore: "#menus",
    onSide: (d) => openMenu((i + d + n) % n, true),
    onClose: () => { if (menubar.menu === menu) { menubar.menu = null; menubar.open = -1; renderMenubar(); } },
  });
  menubar.menu = menu;
  // Under its title; beside it when a skin stacks the titles in a column.
  const host = menubar.host(), title = host.children[i].getBoundingClientRect();
  if (getComputedStyle(host).flexDirection.startsWith("column")) menu.openBeside(title, "menu");
  else menu.openBelow(title, "menu");
  if (withKeys) menu.enter();
}

function closeMenus() {
  if (menubar.menu) menubar.menu.close();
}

/** The bounds a floating thing opened at `anchor` (a rect or a point) stays
 *  in: the client, when it was opened from there — a skin's frame around
 *  the client is not a menu's to cover — else (a part a skin placed outside
 *  the client, as Winamp's detached Orchestrator window) the window. */
function boundsFor(anchor) {
  const app = document.getElementById("app").getBoundingClientRect();
  const a = anchor && { left: anchor.left ?? anchor.x, top: anchor.top ?? anchor.y };
  if (!a || (a.left >= app.left && a.left <= app.right && a.top >= app.top && a.top <= app.bottom)) return app;
  return { left: 4, top: 4, right: window.innerWidth - 4, bottom: window.innerHeight - 4,
    width: window.innerWidth - 8, height: window.innerHeight - 8 };
}

/** Move a floating menu left (and, when `vertical`, up) so it stays inside
 *  the bounds of where it was opened (`boundsFor(anchor)`). */
function keepInside(box, vertical, anchor) {
  const app = boundsFor(anchor);
  const r = box.getBoundingClientRect();
  let dx = 0, dy = 0;
  if (vertical && r.bottom > app.bottom) dy = Math.max(app.top - r.top, app.bottom - r.bottom);
  if (r.right > app.right) dx = Math.max(app.left - r.left, app.right - r.right);
  if (dy) box.style.top = (parseFloat(box.style.top) || 0) + dy + "px";
  if (dx) box.style.left = (parseFloat(box.style.left) || 0) + dx + "px";
}

/** One floating layer above the workbench, by name; `null` clears it. */
function overlay(name, node) {
  const host = document.getElementById("overlays");
  const old = host.querySelector(`:scope > [data-layer="${name}"]`);
  if (old) old.remove();
  if (!node) return;
  node.dataset.layer = name;
  host.appendChild(node);
}


// The core's menus changed (a check, an enabled state): an open menu is
// rebuilt where it is, its highlight kept.
api.on("menus", () => {
  const m = menubar.menu;
  if (!m) { renderMenubar(); return; }
  const hi = m.hi, keys = document.activeElement === m.el;
  openMenu(menubar.open, false);
  if (menubar.menu && hi >= 0) { menubar.menu.highlight(hi, false); if (keys) menubar.menu.el.focus({ preventScroll: true }); }
});
