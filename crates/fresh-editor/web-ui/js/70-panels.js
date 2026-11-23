// Plugin panels: what the core mounted (the dock, a floating panel, sidebar
// sections, a pane's panel), each a WidgetSpec drawn by the widget renderer.
// Where each goes, and how big, is the client's.

function panelEl(p) {
  const body = div("native-panel panel-body");
  body.dataset.slot = p.slot;
  // Pop-overs (a dropdown's list) opened by the last drawing of this slot.
  for (const old of document.querySelectorAll(`[data-popover-region="${p.slot}"]`)) old.remove();
  popoverRegionOwner = p.slot;
  try {
    body.appendChild(widgetEl(p.spec, {
      kind: "panel", slot: p.slot, focusKey: p.focusKey, instances: p.instances || {},
      native: true, page: false,
    }));
  } finally {
    popoverRegionOwner = null;
  }
  body.classList.toggle("has-keyboard", !!p.keyboard);
  return body;
}

/** Drawn specs by slot, so an unchanged panel is not rebuilt. */
const drawnPanels = new Map();

function changed(p) {
  const key = JSON.stringify([p.spec, p.instances, p.focusKey, p.keyboard, p.title]);
  if (drawnPanels.get(p.slot) === key) return false;
  drawnPanels.set(p.slot, key);
  return true;
}

function renderPanels() {
  const list = core.panels || [];
  const live = new Set(list.map((p) => p.slot));
  for (const s of [...drawnPanels.keys()]) if (!live.has(s)) drawnPanels.delete(s);

  // The dock: its own column at the left.
  const dock = document.getElementById("dock");
  const d = list.find((p) => p.slot === "dock");
  dock.hidden = !d;
  document.getElementById("dock-sash").hidden = !d;
  if (d && changed(d)) dock.replaceChildren(panelEl(d));
  if (!d) dock.replaceChildren();

  // Sidebar sections, under the explorer.
  const side = document.getElementById("sidebar-panels");
  const sections = list.filter((p) => p.slot.startsWith("sidebar:"));
  if (sections.some(changed) || side.childElementCount !== sections.length) {
    side.replaceChildren(...sections.map((p) => {
      const s = div("side-section");
      s.appendChild(panelEl(p));
      return s;
    }));
  }

  // A floating panel: centred over everything, or — a plugin's menu,
  // anchored where it was opened — dropped from the press that opened it,
  // with no backdrop.
  const f = list.find((p) => p.slot === "floating");
  if (!f) overlay("floating", null);
  else if (changed(f) || !document.querySelector('[data-layer="floating"]')) {
    const prev = document.querySelector('[data-layer="floating"].anchored');
    const card = div("floating-panel" + (f.anchored ? " anchored" : ""));
    if (f.title || f.closable) {
      const head = div("floating-head");
      head.append(el("span", "floating-title", f.title || ""));
      if (f.closable) {
        const x = el("button", "floating-close", "×");
        x.addEventListener("click", () => api.send("key", { key: "Escape" }));
        head.append(x);
      }
      card.append(head);
    }
    card.append(panelEl(f));
    if (f.anchored && prev) {
      // Already open (a choice in it changed it): it stays where it is.
      card.style.left = prev.style.left;
      card.style.top = prev.style.top;
      overlay("floating", card);
    } else if (f.anchored) {
      // Below what opened it, or above when there is no room below.
      const at = lastPanelPress || { x: 80, top: 80, bottom: 80 };
      card.style.left = at.x + "px";
      card.style.top = (at.bottom + 2) + "px";
      overlay("floating", card);
      const anchor = { left: at.x, top: at.top };
      const bounds = boundsFor(anchor);
      const h = card.offsetHeight;
      if (at.bottom + 2 + h > bounds.bottom && at.top - 2 - h >= bounds.top) card.style.top = (at.top - 2 - h) + "px";
      keepInside(card, true, anchor);
    } else {
      overlay("floating", scrim(card));
    }
  }

  renderPanePanels();
  syncWidgetInputFocus();
}

/** Where the last press in a panel was: an anchored panel opens there. */
let lastPanelPress = null;
document.addEventListener("mousedown", (e) => {
  const el = e.target.closest && e.target.closest(".native-panel, .w-button");
  if (!el) return;
  const r = (e.target.closest(".w-button") || e.target).getBoundingClientRect();
  lastPanelPress = { x: r.left, top: r.top, bottom: r.bottom };
}, true);
// A press outside an anchored panel closes it, as Escape does.
document.addEventListener("mousedown", (e) => {
  const card = document.querySelector('[data-layer="floating"].anchored');
  if (card && !card.contains(e.target)) api.send("key", { key: "Escape" });
}, true);

/** Panes whose content is a plugin panel show it instead of their text. */
function renderPanePanels() {
  for (const [leaf, pane] of panes) {
    const p = (core.panels || []).find((x) => x.slot === `pane:${leaf}`);
    pane.panel.hidden = !p;
    pane.view.root.hidden = !!p;
    if (!p) { pane.panel.replaceChildren(); continue; }
    if (changed(p) || !pane.panel.childElementCount) pane.panel.replaceChildren(panelEl(p));
  }
}

/** Whether a plugin panel has the editor's keyboard now. */
function panelHasKeyboard() {
  return (core.panels || []).some((p) => p.keyboard);
}

api.on("panels", renderPanels);
