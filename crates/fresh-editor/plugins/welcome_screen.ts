/// <reference path="./lib/fresh.d.ts" />
import {
  button,
  col,
  raw,
  row,
  spacer,
  type StyledSegment,
  styledRow,
  toggle,
  type WidgetSpec,
  WidgetPanel,
  key as widgetKey,
} from "./lib/widgets.ts";

const editor = getEditor();

// ═════════════════════════════════════════════════════════════════════
//   WELCOME SCREEN
//
//   One short page, in three parts:
//
//   1. What's new — the latest release's changes, compiled in from
//      CHANGELOG.md at build time (`editor.releaseNotes()`), so every
//      release's page is current with no manual step. A build without
//      the changelog has no such section.
//   2. Nerd Font icons — a checkbox for `editor.nerd_font_icons`, with
//      sample glyphs beside it so the reader can tell whether their
//      terminal's font has them. It saves the setting the way Settings
//      does and takes effect on the next frame.
//   3. The essentials — the palette, opening files, the explorer,
//      settings, a terminal, help, and how to quit — each with the
//      reader's own keybinding.
//
//   Lifecycle, unchanged from the page this replaces: when Fresh
//   launches, the page opens as a tab *behind* whatever is already
//   there. It never takes the foreground on its own, never closes
//   another buffer, and never reopens because something else went away.
//   A file launch (`fresh FILE`, `$EDITOR`) does not open it. Closing it
//   is the reader's (Escape, or the tab's ×), and `Welcome` in the
//   palette brings it back. `showOnStartup` (plugin setting, and the
//   toggle at the bottom of the page) turns the startup open off.
// ═════════════════════════════════════════════════════════════════════

const C = {
  title: "syntax.keyword",
  heading: "syntax.function",
  body: "editor.fg",
  muted: "editor.line_number_fg",
  gutter: "editor.line_number_fg",
  key: "ui.help_key_fg",
  mark: "syntax.type",
  hoverFg: "ui.menu_hover_fg",
};

const LINK = { underline: true };
const HOVER_LINK = { fg: C.hoverFg, bold: true, underline: true };

/** At most this many changelog bullets before "…and N more": enough to
 *  show what a release was about. */
const MAX_NEWS = 8;

/** Rows the page needs above and around the list (title, headings, the
 *  "more" lines, and the Nerd Font checkbox with its hint), so the list
 *  can be cut to keep the checkbox on the first screen. */
const NEWS_CHROME_ROWS = 13;
/** Rows of bullets shown however short the pane. */
const MIN_NEWS_ROWS = 3;

const RELEASES_URL = "github.com/sinelaw/fresh/releases";

/** The page's text column, at most. */
const MEASURE = 76;

/** Sample Nerd Font glyphs: a folder and the Rust logo. Private-use code
 *  points — a terminal without a Nerd Font draws boxes or nothing. */
const NERD_SAMPLES = "\u{e5ff}  \u{e7a8}";

// ── State ────────────────────────────────────────────────────────────

let bufferId: number | null = null;
let panel: WidgetPanel | null = null;
let opening = false;
let paneWidth = 0;
/** The pane shape the page was last laid out for: `width,height`. */
let lastShape = "";

/** What the page last asked a setting to become, while the editor is
 *  still catching up. `saveSetting` is queued, so a repaint right after
 *  a click still reads the old value from the config snapshot; this
 *  bridges that gap and is dropped as soon as the snapshot agrees. */
const pending = new Map<string, boolean>();

function settingWithPending(path: string, live: boolean): boolean {
  if (!pending.has(path)) return live;
  const want = pending.get(path)!;
  if (want === live) {
    pending.delete(path);
    return live;
  }
  return want;
}

type CoreConfig = {
  auto_attach?: boolean;
  editor?: { nerd_font_icons?: boolean };
};

function coreConfig(): CoreConfig {
  return (editor.getConfig() ?? {}) as CoreConfig;
}

function nerdFontIcons(): boolean {
  return settingWithPending(
    "editor.nerd_font_icons",
    coreConfig().editor?.nerd_font_icons === true,
  );
}

/** Auto Attach (`auto_attach`): whether a bare `fresh` runs in the shared
 *  background daemon. On by default, so unset reads as on. */
function autoAttach(): boolean {
  return settingWithPending("auto_attach", coreConfig().auto_attach !== false);
}

editor.defineConfigBoolean("showOnStartup", {
  default: true,
  description: "Open the welcome screen when Fresh starts, as a tab behind whatever is already open.",
});

/** The page's toggle writes plugin global state, which persists across
 *  restarts; the declared config field is the fallback, so the Settings
 *  UI still owns the setting for anyone who never touches the toggle. */
function showOnStartup(): boolean {
  const override = editor.getGlobalState("showOnStartup");
  if (typeof override === "boolean") return override;
  const cfg = (editor.getPluginConfig() ?? {}) as { showOnStartup?: boolean };
  return cfg.showOnStartup !== false;
}

// ── Layout helpers ───────────────────────────────────────────────────

function line(segments: StyledSegment[]): WidgetSpec {
  return raw([styledRow(segments)]);
}

function plain(t: string, fg: string = C.body): WidgetSpec {
  return line([{ text: t, style: { fg } }]);
}

function blank(): WidgetSpec {
  return line([{ text: "" }]);
}

function heading(t: string): WidgetSpec {
  return line([{ text: t, style: { fg: C.heading, bold: true } }]);
}

function cols(s: string): number {
  try {
    return editor.stringWidth(s);
  } catch (_e) {
    return s.length;
  }
}

function wrap(text: string, width: number): string[] {
  const out: string[] = [];
  let cur = "";
  for (const w of text.split(" ")) {
    if (!cur) cur = w;
    else if (cols(cur) + 1 + cols(w) <= width) cur += " " + w;
    else {
      out.push(cur);
      cur = w;
    }
  }
  if (cur) out.push(cur);
  return out;
}

/** A paragraph wrapped to the measure, with a hanging indent. */
function para(text: string, fg: string, first = "", rest = ""): WidgetSpec[] {
  const width = Math.max(16, measure() - cols(first));
  return wrap(text, width).map((l, i) =>
    line([
      { text: i === 0 ? first : rest, style: { fg: C.gutter } },
      { text: l, style: { fg } },
    ])
  );
}

function viewportWidth(): number {
  if (paneWidth > 0) return paneWidth;
  const vp = editor.getViewport();
  return vp && vp.width > 0 ? vp.width : 100;
}

/** Rows the page has on screen. Measured from the terminal rather than
 *  `getViewport()`, which for a page panel reports the whole document;
 *  less the menu bar, tab bar and status bar. */
function viewportHeight(): number {
  const h = editor.getScreenSize().height;
  return h > 0 ? Math.max(1, h - 3) : 40;
}

function measure(): number {
  return Math.min(Math.max(20, viewportWidth() - 4), MEASURE);
}

/** Centre the page's column with the host's page-view margins (the same
 *  hint markdown compose mode uses), rather than padding every row. */
function applyComposeWidth(): void {
  if (bufferId === null) return;
  const w = measure() + 1;
  editor.setLayoutHints(bufferId, null, w < viewportWidth() ? { composeWidth: w } : {});
}

/** The reader's own binding for an action, or `null` when it is unbound. */
function keyFor(action: string, mode?: string): string | null {
  if (mode) return editor.getKeybindingLabel(action, mode);
  return editor.getKeybindingLabel(action, "normal") ??
    editor.getKeybindingLabel(action, "global");
}

function keySeg(k: string): StyledSegment {
  return { text: k, style: { fg: C.key, bold: true } };
}

// ── Sections ─────────────────────────────────────────────────────────

function title(): WidgetSpec[] {
  const notes = editor.releaseNotes();
  const t = notes
    ? editor.t("title.version", { version: notes.version })
    : editor.t("title.plain");
  return [
    blank(),
    line([{ text: t, style: { fg: C.title, bold: true } }]),
    plain(editor.t("tagline"), C.muted),
  ];
}

function whatsNew(): WidgetSpec[] {
  const notes = editor.releaseNotes();
  if (!notes || notes.items.length === 0) return [];
  // As many bullets as fit above the Nerd Font checkbox, up to MAX_NEWS.
  const budget = Math.max(MIN_NEWS_ROWS, viewportHeight() - NEWS_CHROME_ROWS);
  const bullets: WidgetSpec[][] = [];
  let used = 0;
  for (const item of notes.items.slice(0, MAX_NEWS)) {
    const rows = para(item, C.body, "  • ", "    ");
    if (bullets.length > 0 && used + rows.length > budget) break;
    bullets.push(rows);
    used += rows.length;
  }
  const more = notes.items.length - bullets.length;
  const out: WidgetSpec[] = [
    blank(),
    heading(editor.t("news.heading", { version: notes.version })),
  ];
  for (const rows of bullets) out.push(...rows);
  if (more > 0) {
    out.push(plain("    " + editor.t("news.more", { count: String(more) }), C.muted));
  }
  out.push(plain("  " + editor.t("news.full", { url: RELEASES_URL }), C.muted));
  return out;
}

function nerdFont(): WidgetSpec[] {
  return [
    blank(),
    row(
      toggle(nerdFontIcons(), editor.t("nerd.label"), { key: "nerdFontToggle" }),
      spacer(3),
      line([{ text: NERD_SAMPLES, style: { fg: C.mark } }]),
    ),
    ...para(editor.t("nerd.hint"), C.muted, "    ", "    "),
  ];
}

type Essential = { key: string; label: string; action: string; hint?: string };

function essentialsList(): Essential[] {
  return [
    { key: "ess_palette", label: editor.t("ess.palette"), action: "quick_open", hint: editor.t("ess.palette_hint") },
    { key: "ess_open", label: editor.t("ess.open"), action: "open" },
    { key: "ess_new", label: editor.t("ess.new"), action: "new" },
    { key: "ess_explorer", label: editor.t("ess.explorer"), action: "toggle_file_explorer" },
    { key: "ess_settings", label: editor.t("ess.settings"), action: "open_settings" },
    { key: "ess_terminal", label: editor.t("ess.terminal"), action: "open_terminal" },
    { key: "ess_help", label: editor.t("ess.help"), action: "show_help" },
  ];
}

function essentials(): WidgetSpec[] {
  const list = essentialsList();
  const labelCols = Math.max(...list.map((e) => cols(e.label)));
  const keys = list.map((e) => keyFor(e.action) ?? "");
  const keyCols = Math.max(...keys.map((k) => cols(k)));
  const out: WidgetSpec[] = [blank(), heading(editor.t("ess.heading"))];
  list.forEach((e, i) => {
    const k = keys[i];
    const parts: WidgetSpec[] = [
      line([{ text: "  ▸ ", style: { fg: C.mark } }]),
      button(e.label, { key: e.key, bare: true, style: LINK, hoverStyle: HOVER_LINK }),
      spacer(labelCols - cols(e.label) + 2),
      line([keySeg(k), { text: " ".repeat(keyCols - cols(k)) }]),
    ];
    // The hint goes only where there is room for it on the row.
    if (e.hint && 4 + labelCols + 2 + keyCols + 2 + cols(e.hint) <= measure()) {
      parts.push(line([{ text: "  " + e.hint, style: { fg: C.muted } }]));
    }
    out.push(row(...parts));
  });
  return out;
}

/** The keys the reader already knows, and the way out. Built from the
 *  keymap, so a rebound key is shown as it is; an unbound one drops out. */
function familiar(): WidgetSpec[] {
  const verbs: [string, string][] = [
    ["save", editor.t("keys.save")],
    ["undo", editor.t("keys.undo")],
    ["search", editor.t("keys.find")],
    ["copy", editor.t("keys.copy")],
    ["paste", editor.t("keys.paste")],
    ["quit", editor.t("keys.quit")],
  ];
  const have = verbs
    .map(([action, word]) => [keyFor(action), word] as const)
    .filter((p): p is readonly [string, string] => p[0] !== null);
  const out: WidgetSpec[] = [blank()];
  // Packed into rows that fit the measure; the reader's keys may be long.
  let segs: StyledSegment[] = [{ text: "  " }];
  let used = 2;
  have.forEach(([k, word]) => {
    const w = cols(k) + 1 + cols(word);
    const sep = used > 2 ? 3 : 0;
    if (used > 2 && used + sep + w > measure()) {
      out.push(line(segs));
      segs = [{ text: "  " }];
      used = 2;
    } else if (sep) {
      segs.push({ text: " · ", style: { fg: C.gutter } });
      used += sep;
    }
    segs.push(keySeg(k), { text: " " + word, style: { fg: C.body } });
    used += w;
  });
  if (used > 2) out.push(line(segs));
  out.push(...para(editor.t("keys.mouse"), C.muted, "  ", "  "));
  return out;
}

function footer(): WidgetSpec[] {
  const out: WidgetSpec[] = [
    blank(),
    toggle(showOnStartup(), editor.t("startup.label"), { key: "startupToggle" }),
    toggle(autoAttach(), editor.t("attach.label"), { key: "autoAttachToggle" }),
    ...para(
      editor.t(autoAttach() ? "attach.on_hint" : "attach.off_hint"),
      C.muted,
      "    ",
      "    ",
    ),
    blank(),
  ];
  const k = keyFor("welcome_close", "welcome");
  const segs: StyledSegment[] = [{ text: "  " }];
  if (k) {
    segs.push(keySeg(k), { text: " " + editor.t("close.closes") + " · ", style: { fg: C.muted } });
  }
  segs.push({ text: editor.t("close.reopen"), style: { fg: C.muted } });
  out.push(line(segs), blank());
  return out;
}

function buildSpec(): WidgetSpec {
  return col(
    ...title(),
    ...whatsNew(),
    ...nerdFont(),
    ...essentials(),
    ...familiar(),
    ...footer(),
  );
}

function render(): void {
  if (!panel) return;
  panel.set(buildSpec());
}

// ── Lifecycle ────────────────────────────────────────────────────────

/** `force` is the reader asking for the page by name (the `Welcome`
 *  command): it overrides `showOnStartup` and brings the page to the
 *  front. Startup passes `false`: the page becomes a tab behind whatever
 *  is open, and a file launch skips it entirely. */
async function openWelcome(force: boolean): Promise<void> {
  if (bufferId !== null) {
    editor.showBuffer(bufferId);
    return;
  }
  if (opening) return;
  if (!force && !showOnStartup()) return;
  if (!force && editor.fileLaunch()) return;
  opening = true;
  try {
    const res = await editor.createVirtualBuffer({
      name: "Welcome",
      mode: "welcome",
      readOnly: true,
      showLineNumbers: false,
      scrollable: false,
      showCursors: false,
      editingDisabled: true,
      highlightCurrentLine: false,
      // Startup never takes the view; only the palette brings it forward.
      background: !force,
    });
    bufferId = res.bufferId;
    // A page: one host-owned scrolling window over the whole document,
    // with focus following the caret, and nothing focused at rest.
    panel = new WidgetPanel(bufferId, undefined, {
      autoFocusFirst: false,
      page: true,
      focusFollowsCursor: true,
    });
    // Mounting the panel cleared the cursor readout; turn it back on so
    // the status bar reports where the reader is.
    editor.setBufferShowCursors(bufferId, true);
    applyComposeWidth();
    lastShape = shape();
    render();
    if (force) editor.showBuffer(bufferId);
  } catch (e) {
    editor.error(`welcome: ${e}`);
  }
  opening = false;
}

function closeWelcome(): void {
  if (bufferId === null) return;
  const id = bufferId;
  panel?.unmount();
  panel = null;
  bufferId = null;
  editor.closeBuffer(id, true);
}

registerHandler("welcome_open", () => {
  void openWelcome(true);
});
editor.registerCommand("%cmd.welcome", "%cmd.welcome_desc", "welcome_open");

registerHandler("welcomeOnReady", async () => {
  await openWelcome(false);
});

/** Only housekeeping for this page's own buffer: emptying the workspace
 *  does not reopen the page, and opening a file does not close it. */
registerHandler("welcomeOnBufferClosed", (e: { buffer_id: number }) => {
  if (bufferId !== null && e.buffer_id === bufferId) {
    panel = null;
    bufferId = null;
  }
});

/** Re-lay the page when its size changes. A page created behind a file
 *  hears no resize, so coming to the front re-measures too. */
function shape(): string {
  return `${viewportWidth()},${viewportHeight()}`;
}

function relayout(): void {
  const sh = shape();
  if (sh === lastShape) return;
  lastShape = sh;
  applyComposeWidth();
  render();
}

registerHandler("welcomeOnBufferActivated", (e: { buffer_id: number }) => {
  if (bufferId === null || e.buffer_id !== bufferId) return;
  paneWidth = 0;
  editor.setTimeout(0, "welcomeCatchUpOnShow");
});
registerHandler("welcomeCatchUpOnShow", async () => {
  if (bufferId === null || editor.getActiveBufferId() !== bufferId) return;
  paneWidth = 0;
  const sh = shape();
  if (sh === lastShape) return;
  lastShape = sh;
  applyComposeWidth();
  // The width hint must land before the panel lays out against it.
  await editor.flush();
  render();
});
registerHandler(
  "welcomeOnViewportChanged",
  (d: { buffer_id: number; width: number }) => {
    if (bufferId === null || d.buffer_id !== bufferId) return;
    if (typeof d.width === "number" && d.width > 0) paneWidth = d.width;
    relayout();
  },
);

editor.on("ready", "welcomeOnReady");
editor.on("buffer_activated", "welcomeOnBufferActivated");
editor.on("buffer_closed", "welcomeOnBufferClosed");
editor.on("viewport_changed", "welcomeOnViewportChanged");

// ── Keyboard ─────────────────────────────────────────────────────────

function dispatch(action: ReturnType<typeof widgetKey>): void {
  panel?.command(action);
}

registerHandler("welcome_tab", () => dispatch(widgetKey("Tab")));
registerHandler("welcome_shift_tab", () => dispatch(widgetKey("Shift+Tab")));
registerHandler("welcome_enter", () => dispatch(widgetKey("Enter")));
registerHandler("welcome_space", () => dispatch(widgetKey("Space")));
registerHandler("welcome_close", () => closeWelcome());

editor.defineMode(
  "welcome",
  [
    ["Tab", "welcome_tab"],
    // Shift+Tab arrives as BackTab (CSI Z), sometimes still carrying SHIFT.
    ["BackTab", "welcome_shift_tab"],
    ["S-BackTab", "welcome_shift_tab"],
    ["Return", "welcome_enter"],
    ["Space", "welcome_space"],
    ["Escape", "welcome_close"],
  ],
  true,
  // No text field on the page, so no text input: every key the mode does
  // not bind is the editor's own (Ctrl+P, F1, the arrows, selection, copy).
  false,
  true,
);

// ── Widget events ────────────────────────────────────────────────────

function saveToggle(path: string, next: boolean, on: string, off: string): void {
  pending.set(path, next);
  editor.saveSetting(path, next);
  editor.setStatus(editor.t(next ? on : off));
  render();
}

function activate(k: string, checked?: boolean): void {
  const e = essentialsList().find((x) => x.key === k);
  if (e) {
    editor.executeAction(e.action);
    return;
  }
  switch (k) {
    case "nerdFontToggle":
      // Saved the way Settings saves it, and applied now: the explorer,
      // tab icons and the theme's Nerd Font glyphs switch on the next frame.
      saveToggle(
        "editor.nerd_font_icons",
        checked ?? !nerdFontIcons(),
        "status.nerd_on",
        "status.nerd_off",
      );
      return;
    case "autoAttachToggle":
      saveToggle("auto_attach", checked ?? !autoAttach(), "status.attach_on", "status.attach_off");
      return;
    case "startupToggle": {
      const next = checked ?? !showOnStartup();
      editor.setGlobalState("showOnStartup", next);
      editor.setStatus(editor.t(next ? "status.startup_on" : "status.startup_off"));
      render();
      return;
    }
  }
}

editor.on("widget_event", (args) => {
  if (!panel || args.panel_id !== panel.id()) return;
  const k = typeof args.widget_key === "string" ? args.widget_key : "";
  if (args.event_type === "toggle") {
    const checked = (args.payload as { checked?: boolean } | undefined)?.checked;
    activate(k, typeof checked === "boolean" ? checked : undefined);
    return;
  }
  if (args.event_type === "activate") activate(k);
});

editor.debug("Welcome screen plugin loaded");
