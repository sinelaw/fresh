// Popups: completion lists, hover documentation, code actions, plugin
// lists — the core's content, placed by the client at the active view's
// caret. Keys while one is open still go to the core, which owns the
// popup's selection.

function renderPopups() {
  const host = div("popups");
  const v = activeView();
  const at = v && v.caretRect();
  for (const p of core.popups || []) {
    const box = div("popup popup-" + p.kind);
    if (p.title) box.append(el("div", "popup-title", p.title));
    const c = p.content || {};
    if (c.items) {
      const list = div("popup-list");
      list.setAttribute("role", "listbox");
      c.items.forEach((it, i) => {
        const r = div("popup-item" + (i === c.selected ? " selected" : "") + (it.disabled ? " disabled" : ""));
        r.setAttribute("role", "option");
        if (it.icon) r.append(el("span", "popup-icon", it.icon));
        r.append(el("span", "popup-text", it.text));
        if (it.detail) r.append(el("span", "popup-detail", it.detail));
        r.addEventListener("mousedown", (e) => e.preventDefault());
        r.addEventListener("click", () => {
          api.send("popup.select", { index: i });
          api.send("popup.confirm");
        });
        list.append(r);
      });
      box.append(list);
      requestAnimationFrame(() => {
        const s = list.children[c.selected];
        if (s) s.scrollIntoView({ block: "nearest" });
      });
    } else if (c.lines) {
      const body = div("popup-doc");
      for (const line of c.lines) {
        const l = div("popup-line");
        for (const s of line) {
          const sp = el("span", "", s.t);
          if (s.fg) sp.style.color = s.fg;
          if (s.bg) sp.style.background = s.bg;
          if (s.b) sp.style.fontWeight = "700";
          if (s.i) sp.style.fontStyle = "italic";
          if (s.u) sp.style.textDecoration = "underline";
          l.append(sp);
        }
        if (!line.length) l.append(document.createTextNode(" "));
        body.append(l);
      }
      box.append(body);
    }
    if (p.description) box.append(el("div", "popup-foot", p.description));
    host.append(box);
  }
  if (!host.childElementCount) { overlay("popups", null); return; }
  overlay("popups", host);
  placePopups(host, (core.popups[core.popups.length - 1] || {}).at || "cursor", at);
}

/** Put the popups where the core says the top one goes: above the status
 *  item that opened it, centred, in the bottom-right corner, or at the
 *  caret (below it, or above when there is no room). */
function placePopups(host, where, at) {
  const app = document.getElementById("app").getBoundingClientRect();
  const w = host.offsetWidth, h = host.offsetHeight;
  const clampX = (x) => Math.max(app.left + 4, Math.min(x, app.right - w - 4));
  const status = where.startsWith("status:") && document.querySelector(`#statusbar [data-anchor="${where.slice(7)}"]`);
  let x, y;
  if (status) {
    const r = status.getBoundingClientRect();
    x = clampX(r.right - w);
    y = r.top - h - 4;
  } else if (where === "bottomRight") {
    const bar = document.getElementById("statusbar").getBoundingClientRect();
    x = app.right - w - 8;
    y = bar.top - h - 8;
  } else if (where === "centered" || !at) {
    x = clampX(app.left + (app.width - w) / 2);
    y = app.top + Math.max(16, (app.height - h) / 3);
  } else {
    x = clampX(at.x);
    const below = at.y + at.h, above = at.y - h;
    y = (where === "above" || below + h > app.bottom - 8) && above > app.top + 8 ? above : below;
  }
  host.style.left = x + "px";
  host.style.top = y + "px";
}

api.on("popups", renderPopups);
api.on("view", (v) => {
  // The caret moved under an open popup: follow it.
  if (core.popups && core.popups.length) {
    const a = activeView();
    if (a && a.id === v.view) renderPopups();
  }
});
