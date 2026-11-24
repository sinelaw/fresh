// Theming: the client's own colours, seeded from the editor theme's so the
// text the core styles sits on the ground it was styled for.
//
// The editor theme's values go in a plain `:root {}` rule (<style
// id="editor-theme">), after the base stylesheet and before the skin's
// (js/15-skins.js), so a skin's `:root.theme-<id>` wins by the ordinary
// cascade and a property it leaves out keeps the editor theme's value.

/** The editor theme as custom properties, plus inks the skin layer derives. */
const themeVars = { theme: {}, ink: {} };

function writeThemeRule() {
  const all = { ...themeVars.theme, ...themeVars.ink };
  const body = Object.entries(all).map(([k, v]) => `${k}:${v};`).join("");
  document.getElementById("editor-theme").textContent = `:root{${body}}`;
}

api.on("theme", () => {
  const t = core.theme || {};
  const vars = {};
  const set = (name, v) => { if (v) vars[name] = v; };
  set("--bg", t.bg);
  set("--fg", t.fg);
  set("--ed-fg", t.fg);
  set("--ed-muted", t.muted);
  set("--accent", t.menuHighlightBg || t.accent);
  set("--on-accent", t.menuHighlightFg);
  set("--muted", t.muted);
  set("--selection", t.selectionBg);
  set("--menu-bg", t.menuBg);
  set("--menu-fg", t.menuFg);
  set("--popup-bg", t.popupBg);
  set("--popup-fg", t.popupFg);
  set("--border-c", t.border);
  set("--status-bg", t.statusBg);
  set("--status-fg", t.statusFg);
  // The active tab stands on the buffer's ground (--tab-active-*: --bg and
  // --ed-fg), not on the terminal's tab fill, which is a solid block there.
  set("--tab-bg", t.tabInactiveBg);
  set("--tab-fg", t.tabInactiveFg);
  set("--danger", t.error);
  set("--warning", t.warning);
  set("--thumb", t.scrollbarThumb);
  themeVars.theme = vars;
  themeVars.ink = {};
  writeThemeRule();
  document.documentElement.dataset.theme = t.name || "";
  // The skin follows the theme: which way the ground leans, and the ink on
  // any fill the skin replaced.
  skinFollowTheme();
  // Text drawn before the theme arrived used the defaults.
  for (const v of views.values()) v.repaint();
});

/** "#rrggbb" → [r, g, b], or null. */
function hexRgb(c) {
  const m = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(String(c).trim());
  return m ? [parseInt(m[1], 16), parseInt(m[2], 16), parseInt(m[3], 16)] : null;
}

/** The readable ink on a fill: white or near-black, by contrast. */
function onColor(rgb) {
  const lin = (v) => { v /= 255; return v <= 0.03928 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4; };
  const L = 0.2126 * lin(rgb[0]) + 0.7152 * lin(rgb[1]) + 0.0722 * lin(rgb[2]);
  return (1.05) / (L + 0.05) >= (L + 0.05) / 0.05 ? "#ffffff" : "#111111";
}

/** A custom property as the browser resolves it, as [r, g, b] (or null). */
function resolvedColor(prop) {
  const probe = div("skin-probe");
  probe.style.color = `var(${prop})`;
  document.body.appendChild(probe);
  const c = getComputedStyle(probe).color.trim();
  probe.remove();
  const m = /^(?:rgba?\(|color\(srgb\s+)([0-9.]+)[ ,]+([0-9.]+)[ ,]+([0-9.]+)/.exec(c);
  if (!m) return null;
  const unit = c.startsWith("color(") ? 255 : 1;
  return [m[1], m[2], m[3]].map((v) => Math.round(Math.min(255, parseFloat(v) * unit)));
}
