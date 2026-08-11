/// <reference path="./lib/fresh.d.ts" />

import { git, resolveGitRepo } from "./lib/git_repo.ts";
import {
  button,
  col,
  flexSpacer,
  hintBar,
  type HintEntry,
  key as widgetKey,
  labeledSection,
  list,
  raw,
  row,
  spacer,
  type StyledSegment,
  styledRow,
  text,
  type TextPropertyEntry,
  type WidgetAction,
  WidgetPanel,
  type WidgetSpec,
} from "./lib/widgets.ts";

/**
 * Code Tour Plugin
 *
 * A JSON-driven walkthrough system that guides users through a codebase.
 * Each tour is a virtual buffer living in the shared Utility Dock, rendered
 * through the widget library: a step rail, markdown-rendered prose, clickable
 * Prev/Next/Exit buttons and a source-location bar.
 *
 * Two manifest formats load, auto-detected by content: our own
 * `.fresh-tour.json` (see `plugins/schemas/tour.schema.json`) and VS Code
 * CodeTour `.tour` files, which are converted on load. Both load commands
 * open the native file browser anchored at the workspace root with hidden
 * files shown — tour files are dotfiles (`.fresh-tour.json`, `.tours/`,
 * `.vscode/tours/`, `.github/tours/`). `fresh --cmd help tour` prints the
 * authoring guide.
 *
 * Several tours can be open at once — one dock tab each — and an unfinished
 * tour is restored on the next launch. Closing a tour forgets it.
 *
 * See `docs/internal/code-tour-dock-redesign.md`.
 */

const editor = getEditor();

// ============================================================================
// Types
// ============================================================================

interface OverlayConfig {
  type: "block" | "line";
  focus_mode: boolean;
}

interface TourStep {
  step_id: number;
  title: string;
  file_path: string;
  lines: [number, number]; // 1-indexed, inclusive
  explanation: string;
  overlay_config?: OverlayConfig;
}

interface TourManifest {
  $schema?: string;
  title: string;
  description: string;
  schema_version: "1.0";
  commit_hash?: string;
  steps: TourStep[];
}

// VS Code CodeTour (`.tour`) manifests, as written by the CodeTour
// extension (https://github.com/microsoft/codetour). Loaded read-only:
// `parseManifest` sniffs the format and converts to `TourManifest`, so
// everything downstream speaks one shape. Only the fields the conversion
// reads are declared here.
interface CodeTourPosition {
  line?: number;
  character?: number;
}

interface CodeTourStep {
  description?: string;
  file?: string;
  directory?: string;
  uri?: string;
  line?: number;
  pattern?: string;
  selection?: { start?: CodeTourPosition; end?: CodeTourPosition };
  title?: string;
}

interface CodeTourFile {
  title?: string;
  description?: string;
  ref?: string;
  steps?: CodeTourStep[];
}

/** One open tour. Keyed in `tours` by `id` (the manifest path as given). */
interface TourInstance {
  id: string;
  manifestPath: string;
  manifest: TourManifest;
  /** Dock tab label, kept so a later tour can disambiguate against it. */
  tabName: string;
  step: number;
  visited: Set<number>;
  bufferId: number;
  splitId: number;
  /** The editor pane this tour opens its steps into — see `targetSplit`.
   * Seeded with the split the tour was launched from and re-pinned to
   * wherever a step actually landed, so it survives splits being closed. */
  targetSplitId: number;
  panel: WidgetPanel;
  /** Per-tour overlay namespace, so one tour's teardown cannot clear another's
   * highlight and two tours may highlight the same buffer at once. */
  namespace: string;
  railOpen: boolean;
  /** Set when the step changed, so the next render parks focus back on the
   * prose. Reading is the common case; Tab still reaches everything else. */
  focusProse: boolean;
  dockWidth: number;
  dockHeight: number;
  /** Buffers this tour has painted overlays into, so teardown is targeted
   * instead of walking every open buffer. */
  paintedBuffers: Set<number>;
  fileMissing: boolean;
  /** `recorded at <a> · you are on <b>`, or null when the tour has no
   * `commit_hash` or the repo matches. */
  drift: string | null;
}

/** Shape persisted via `setWindowState` (see §6 of the design note). */
interface PersistedTour {
  manifestPath: string;
  step: number;
  visited: number[];
  railOpen: boolean;
}

// ============================================================================
// State
// ============================================================================

const MODE = "code-tour-panel";
const STATE_KEY = "openTours";
const DEFAULT_DOCK_WIDTH = 100;
const DEFAULT_DOCK_HEIGHT = 16;
/** Below this dock width the Steps rail folds away (§3.4). */
const RAIL_MIN_WIDTH = 110;
/** Below this the header's button cluster loses its labels. */
const COMPACT_WIDTH = 100;
/** Fraction of the dock width the Steps rail takes when shown. */
const RAIL_PCT = 26;

const tours = new Map<string, TourInstance>();
const tourByBuffer = new Map<number, string>();
const tourByPanel = new Map<number, string>();
/** Most recently touched tour — the target for the editor-context keys, which
 * fire from a split where there is no tour buffer to read. */
let lastTourId: string | null = null;

// Theme keys, not RGB literals: every tour colour restyles with the
// active theme (the host resolves ThemedStyle overlays at paint time).
const C = {
  dim: "ui.menu_disabled_fg",
  accent: "ui.help_key_fg",
};

// ============================================================================
// Small string helpers (codepoint-aware; the host measures in codepoints)
// ============================================================================

function charLen(s: string): number {
  let n = 0;
  for (const _c of s) n++;
  return n;
}

function truncate(s: string, max: number): string {
  if (max <= 0) return "";
  if (charLen(s) <= max) return s;
  if (max === 1) return "…";
  let out = "";
  let n = 0;
  for (const c of s) {
    if (n >= max - 1) break;
    out += c;
    n++;
  }
  return out + "…";
}

// ============================================================================
// Panel spec
// ============================================================================

/** Progress meter — one cell per step, capped so a long tour still fits. */
function meter(step: number, total: number): string {
  const cells = Math.min(Math.max(total, 1), 16);
  const filled = Math.min(cells, Math.max(1, Math.round(((step + 1) / total) * cells)));
  return "▰".repeat(filled) + "▱".repeat(cells - filled);
}

function currentStep(t: TourInstance): TourStep {
  return t.manifest.steps[t.step];
}

/** A step with no file at all — a converted CodeTour content step
 * (intro/outro prose, or a directory step). Prose renders as usual;
 * there is no location to show, highlight, or jump to, and no
 * "file missing" warning to raise about a file that was never named. */
function isContentStep(step: TourStep): boolean {
  return step.file_path === "";
}

function showRail(t: TourInstance): boolean {
  return t.railOpen && t.dockWidth >= RAIL_MIN_WIDTH;
}

/** The column budget the host actually renders this panel at. The host's
 * `widget_panel_width` reserves two columns of the split's text viewport
 * (gutter/scrollbar slack), so every width computation must start from
 * this — starting from `dockWidth` overshoots by up to two columns and
 * the host then ellipsis-truncates the overlong rows. */
function hostPanelWidth(t: TourInstance): number {
  return Math.max(10, t.dockWidth - 2);
}

/** Rows the body section's list may occupy.
 *
 * The panel must render *exactly* as many lines as the pane can show. One line
 * too many and the buffer becomes scrollable, at which point a click anywhere
 * that isn't a widget hit area — a section border, the padding under a short
 * step — moves the buffer cursor and scrolls the panel's own header out of
 * view. Pane height less the tab bar, then less
 * `header + 2 border rows + hints`. */
function bodyRows(t: TourInstance): number {
  return Math.max(3, t.dockHeight - 5);
}

function buildHeader(t: TourInstance): WidgetSpec {
  const total = t.manifest.steps.length;
  const compact = t.dockWidth < COMPACT_WIDTH;
  const last = t.step === total - 1;

  const titleSegments: StyledSegment[] = [
    { text: t.manifest.title, style: { bold: true } },
  ];
  if (t.drift) {
    titleSegments.push({ text: "   " + t.drift, style: { fg: C.dim, italic: true } });
  }

  const counter = compact
    ? `${t.step + 1}/${total} ${meter(t.step, total)}`
    : editor.t("panel.step_counter", {
      step: String(t.step + 1),
      total: String(total),
    }) + "  " + meter(t.step, total);

  // The step's context actions live up here (not on a bottom row of
  // their own — vertical space goes to the prose instead).
  const actions: WidgetSpec[] = isContentStep(currentStep(t))
    ? []
    : t.fileMissing
    ? [button(editor.t("btn.skip"), { key: "next", disabled: last })]
    : compact
    ? [button(editor.t("btn.jump_short"), { key: "jump" })]
    : [
      button(editor.t("btn.jump"), { key: "jump" }),
      spacer(1),
      button(editor.t("btn.rehighlight"), { key: "rehighlight" }),
    ];

  return row(
    spacer(1),
    raw([styledRow(titleSegments, {
      truncateToChars: Math.max(8, t.dockWidth - 42 - actionsWidth(t)),
    })], "title"),
    flexSpacer(),
    raw([styledRow([{ text: counter, style: { fg: C.dim } }])], "progress"),
    spacer(2),
    ...actions,
    spacer(1),
    button(compact ? "◀" : editor.t("btn.prev"), { key: "prev", disabled: t.step === 0 }),
    spacer(1),
    button(compact ? "▶" : editor.t("btn.next"), {
      key: "next",
      intent: "primary",
      disabled: last,
    }),
    spacer(1),
    button(
      compact ? (last ? "✓" : "✕") : (last ? editor.t("btn.finish") : editor.t("btn.exit")),
      { key: "exit", intent: last ? "primary" : "normal" },
    ),
    spacer(1),
  );
}

function buildRailRows(t: TourInstance, width: number): TextPropertyEntry[] {
  return t.manifest.steps.map((step, i) => {
    const marker = i === t.step ? "▸" : t.visited.has(i) ? "✓" : " ";
    const num = String(i + 1);
    const label = truncate(step.title, Math.max(4, width - charLen(num) - 4));
    return styledRow([
      { text: ` ${marker} `, style: { fg: i === t.step ? C.accent : C.dim } },
      { text: `${num}  `, style: { fg: C.dim } },
      { text: label, style: i === t.step ? { bold: true } : undefined },
    ]);
  });
}

/** The prose column's markdown source: the step explanation, prefixed
 * with the missing-file warning when the step's file is gone. The host
 * renders it through the shared markdown engine (the same one behind
 * LSP hover docs) and word-wraps at its real column budget, so the
 * plugin no longer parses or wraps anything itself. */
function proseMarkdown(t: TourInstance): string {
  const step = currentStep(t);
  let head = "";
  if (isContentStep(step)) {
    // No location line and no warning — the prose is the whole step.
  } else if (t.fileMissing) {
    head += "⚠  " + editor.t("panel.file_missing", { file: step.file_path }) + "\n\n";
    if (t.drift) head += "*" + t.drift + "*\n\n";
  } else {
    // The step's location leads the prose (its old dedicated row below
    // the body was a whole line the description can use better).
    head += editor.t("panel.location", {
      file: "`" + step.file_path + "`",
      from: String(step.lines[0]),
      to: String(step.lines[1]),
    }) + "\n\n";
  }
  const body = step.explanation.trim().length > 0
    ? step.explanation
    : editor.t("panel.no_explanation");
  return head + body;
}

function buildBody(t: TourInstance): WidgetSpec {
  const step = currentStep(t);
  const rows = bodyRows(t);
  const label = `${t.step + 1}/${t.manifest.steps.length} · ${step.title}`;
  // The prose is a markdown *document* view: read-only, rendered and
  // wrapped host-side, with a caret the arrow keys move, Shift-selection,
  // drag-to-select, and C-c copy over the rendered text.
  const prose = text({
    value: proseMarkdown(t),
    rows,
    markdown: true,
    key: "prose",
  });

  if (!showRail(t)) {
    return labeledSection({
      label: truncate(label, Math.max(8, t.dockWidth - 6)),
      key: "proseBox",
      child: prose,
    });
  }

  // The rail still sizes its labels itself: mirror the host's allocation
  // (panel rendered at `viewport.width - 2`, the rail block takes
  // `panel_width * pct / 100`, a labeled section spends 4 columns of
  // chrome, one column of slack absorbs rounding).
  const panelWidth = hostPanelWidth(t);
  const railWidth = Math.max(8, Math.floor((panelWidth * RAIL_PCT) / 100) - 5);
  const proseWidth = Math.max(
    8,
    Math.floor((panelWidth * (100 - RAIL_PCT)) / 100) - 5,
  );
  return row(
    labeledSection({
      label: editor.t("panel.steps"),
      widthPct: RAIL_PCT,
      key: "railBox",
      child: list({
        items: buildRailRows(t, railWidth),
        itemKeys: t.manifest.steps.map((_s, i) => String(i)),
        selectedIndex: t.step,
        visibleRows: rows,
        key: "stepList",
      }),
    }),
    labeledSection({
      label: truncate(label, Math.max(8, proseWidth - 2)),
      widthPct: 100 - RAIL_PCT,
      key: "proseBox",
      child: prose,
    }),
  );
}

/** Rendered width of the header's step-action buttons, including the
 * spacer between them. `[ Label ]` is the button chrome, so a label of n
 * characters occupies n + 4 columns. */
function actionsWidth(t: TourInstance): number {
  const chrome = 4;
  if (isContentStep(currentStep(t))) return 0;
  if (t.fileMissing) return charLen(editor.t("btn.skip")) + chrome;
  if (t.dockWidth < COMPACT_WIDTH) return charLen(editor.t("btn.jump_short")) + chrome;
  return (
    charLen(editor.t("btn.jump")) + chrome +
    1 +
    charLen(editor.t("btn.rehighlight")) + chrome
  );
}

function buildHints(t: TourInstance): HintEntry[] {
  const core: HintEntry[] = [
    { keys: "n", label: editor.t("hint.next") },
    { keys: "p", label: editor.t("hint.prev") },
    { keys: "⏎", label: editor.t("hint.jump") },
    { keys: "q", label: t.step === t.manifest.steps.length - 1 ? editor.t("hint.finish") : editor.t("hint.exit") },
  ];
  if (t.dockWidth < COMPACT_WIDTH) return core;
  return [
    core[0],
    core[1],
    core[2],
    { keys: "↑↓", label: editor.t("hint.scroll") },
    { keys: "Tab", label: editor.t("hint.focus") },
    { keys: "g", label: editor.t("hint.steps") },
    core[3],
  ];
}

function renderPanel(t: TourInstance): void {
  // Re-measure on every render. The dock's geometry is not final when the
  // buffer is created, and `viewport_changed` does not necessarily fire for
  // the initial layout — without this the panel keeps the default 100x16, so
  // both the rail breakpoint and the prose wrap width are wrong.
  syncDockSize(t);
  t.panel.set(
    col(
      buildHeader(t),
      buildBody(t),
      hintBar(buildHints(t)),
    ),
  );
  // `selectedIndex` on the spec is a seed the host ignores after first render,
  // so push the rail's selection explicitly on every step change.
  if (showRail(t)) t.panel.setSelectedIndex("stepList", t.step);
  if (t.focusProse) {
    t.focusProse = false;
    t.panel.setFocusKey("prose");
  }
}

// ============================================================================
// Dock geometry
// ============================================================================

/** Refresh the tour's cached dock size from the live split list. Returns true
 * when it changed, so callers can skip a re-render that would change nothing. */
function syncDockSize(t: TourInstance): boolean {
  const snap = editor.listSplits().find((s) => s.splitId === t.splitId);
  if (!snap) return false;
  // Width comes from the *text* viewport (gutter excluded); height from the
  // pane rect. `viewport.height` overreports for a dock pane — it does not
  // account for the tab bar — and one row too many makes the panel scrollable
  // (see `bodyRows`).
  const width = snap.viewport.width > 0 ? snap.viewport.width : snap.width;
  const height = snap.height > 0 ? snap.height : t.dockHeight;
  if (width === t.dockWidth && height === t.dockHeight) return false;
  t.dockWidth = Math.max(20, width);
  t.dockHeight = Math.max(6, height);
  return true;
}

// ============================================================================
// Overlays
// ============================================================================

/** Byte offsets of the first and last line of `[from, to]` (1-indexed,
 * inclusive) within `bufferId`.
 *
 * Deliberately does NOT use `getLineStartPosition` / `getLineEndPosition`:
 * those answer for the *active* buffer, and the active buffer while a tour
 * renders is the dock panel, not the source file — which is why the step
 * highlight never painted at all before.
 */
async function lineRangeBytes(
  bufferId: number,
  from: number,
  to: number,
): Promise<[number, number] | null> {
  const text = await editor.getBufferText(bufferId);
  if (typeof text !== "string") return null;
  const lines = text.split("\n");
  if (from < 1 || from > lines.length) return null;
  const last = Math.min(to, lines.length);

  let offset = 0;
  for (let i = 0; i < from - 1; i++) {
    offset += editor.utf8ByteLength(lines[i]) + 1;
  }
  const start = offset;
  for (let i = from - 1; i < last; i++) {
    offset += editor.utf8ByteLength(lines[i]) + (i < last - 1 ? 1 : 0);
  }
  return [start, offset];
}

/** Directory of the tour manifest, absolute — the base a tour's relative
 * step paths resolve against. */
function manifestDir(manifestPath: string): string {
  const norm = manifestPath.replace(/\\/g, "/");
  const abs = editor.pathIsAbsolute(norm) ? norm : editor.pathJoin(editor.getCwd(), norm);
  const idx = abs.lastIndexOf("/");
  return idx > 0 ? abs.slice(0, idx) : "/";
}

/** The workspace root a CodeTour manifest's step paths are relative to.
 *
 * CodeTour stores tours *inside* the workspace they describe — in
 * `.tours/`, `.vscode/tours/`, `.github/tours/` (arbitrarily nested), or
 * as `.vscode/main.tour` — with every step path relative to the
 * workspace root, not the manifest. Strip the well-known tour directory
 * from the manifest's own location to recover that root. Null when the
 * manifest is not under one (e.g. a root-level `.tour`, where the
 * manifest directory *is* the root and manifest-relative already works). */
function tourRootDir(manifestPath: string): string | null {
  const dir = manifestDir(manifestPath);
  const m = dir.match(/^(.*?)\/(?:\.tours|\.github\/tours|\.vscode(?:\/tours)?)(?:\/|$)/);
  if (!m) return null;
  return m[1] || "/";
}

/** A step's `file_path` as an openable path: absolute paths pass through,
 * relative ones resolve against the manifest's own directory — a tour is
 * authored next to the code it walks, and must keep working no matter
 * where the editor was started. A manifest living in a well-known
 * CodeTour directory also tries the workspace root those formats are
 * relative to. Tours written before this resolved against the working
 * directory instead, so when no other candidate names anything but the
 * cwd-relative one exists, the old meaning wins. */
function resolveStepPath(manifestPath: string, filePath: string): string {
  if (editor.pathIsAbsolute(filePath)) return filePath;
  const manifestRelative = editor.pathJoin(manifestDir(manifestPath), filePath);
  if (editor.fileExists(editor.authorityPath(manifestRelative))) return manifestRelative;
  const root = tourRootDir(manifestPath);
  if (root) {
    const rootRelative = editor.pathJoin(root, filePath);
    if (editor.fileExists(editor.authorityPath(rootRelative))) return rootRelative;
  }
  const cwdRelative = editor.pathJoin(editor.getCwd(), filePath);
  if (editor.fileExists(editor.authorityPath(cwdRelative))) return cwdRelative;
  return manifestRelative;
}

/** Resolve the buffer showing `filePath`.
 *
 * `findBufferByPath` compares `PathBuf`s for equality and buffers store
 * absolute paths, so a manifest's repo-relative `file_path` never matches it
 * directly — which is why the step highlight never painted. Try the literal
 * path, then cwd-resolved, then fall back to a normalized suffix match (buffer
 * paths are canonicalized, so a symlinked workspace root can still differ).
 * Returns 0 when the file is not open. */
function stepBufferId(filePath: string): number {
  const direct = editor.findBufferByPath(filePath);
  if (direct) return direct;
  const absolute = filePath.startsWith("/")
    ? filePath
    : editor.pathJoin(editor.getCwd(), filePath);
  const byAbsolute = editor.findBufferByPath(absolute);
  if (byAbsolute) return byAbsolute;

  const wanted = filePath.replace(/\\/g, "/").replace(/^\.\//, "");
  for (const info of editor.listBuffers()) {
    if (!info.path) continue;
    const candidate = info.path.replace(/\\/g, "/");
    if (candidate === wanted || candidate.endsWith("/" + wanted)) return info.id;
  }
  return 0;
}

function clearTourOverlays(t: TourInstance): void {
  for (const bufferId of t.paintedBuffers) {
    editor.clearNamespace(bufferId, t.namespace);
  }
  t.paintedBuffers.clear();
}

async function paintStepOverlay(t: TourInstance): Promise<void> {
  const step = currentStep(t);
  const bufferId = stepBufferId(resolveStepPath(t.manifestPath, step.file_path));
  if (!bufferId) return;

  clearTourOverlays(t);
  const range = await lineRangeBytes(bufferId, step.lines[0], step.lines[1]);
  if (!range) {
    editor.warn(
      `Tour: could not resolve lines ${step.lines[0]}-${step.lines[1]} in ${step.file_path}`,
    );
    return;
  }
  // One range-wide overlay; the host tail-fills every row it spans, so
  // each line paints a full-width band — a region marker visually
  // distinct from a text selection (which never covers the empty tail
  // of a line). The bg is a theme key so the band restyles with the
  // theme, like every other tour colour.
  editor.addOverlay(bufferId, t.namespace, range[0], range[1], {
    bg: "ui.semantic_highlight_bg",
    extendToLineEnd: true,
  });
  t.paintedBuffers.add(bufferId);
}

// ============================================================================
// Persistence (§6 of the design note)
// ============================================================================

function persist(): void {
  const open: PersistedTour[] = [];
  for (const t of tours.values()) {
    open.push({
      manifestPath: t.manifestPath,
      step: t.step,
      visited: [...t.visited],
      railOpen: t.railOpen,
    });
  }
  editor.setWindowState(STATE_KEY, open);
}

function readPersisted(): PersistedTour[] {
  const stored = editor.getWindowState(STATE_KEY);
  if (!Array.isArray(stored)) return [];
  const out: PersistedTour[] = [];
  for (const entry of stored) {
    if (!entry || typeof entry !== "object") continue;
    const e = entry as Partial<PersistedTour>;
    if (typeof e.manifestPath !== "string") continue;
    out.push({
      manifestPath: e.manifestPath,
      step: typeof e.step === "number" ? e.step : 0,
      visited: Array.isArray(e.visited) ? e.visited.filter((v) => typeof v === "number") : [],
      railOpen: e.railOpen !== false,
    });
  }
  return out;
}

// ============================================================================
// Manifest loading
// ============================================================================

function parseManifest(path: string): TourManifest | string {
  const content = editor.readFile(editor.authorityPath(path));
  if (!content) return editor.t("error.read_failed", { path });
  let parsed: unknown;
  try {
    parsed = JSON.parse(content);
  } catch (e) {
    return editor.t("error.parse_failed", { path, error: String(e) });
  }
  // Format sniff on content, not extension: a CodeTour exported as `.json`
  // still loads, and a fresh manifest renamed `.tour` still validates as one.
  const manifest = looksLikeCodeTour(parsed)
    ? convertCodeTour(parsed, path)
    : (parsed as TourManifest);
  if (manifest.schema_version !== "1.0") {
    return editor.t("error.bad_version", { version: String(manifest.schema_version) });
  }
  if (!manifest.steps || manifest.steps.length === 0) {
    return editor.t("error.no_steps");
  }
  return manifest;
}

/** A CodeTour file, not a fresh manifest? Keyed on what each format
 * *requires*: fresh steps carry `explanation`/`step_id`, CodeTour steps
 * carry `description` and never those. A fresh manifest that merely
 * forgot its `schema_version` keeps failing with `error.bad_version`
 * rather than being mis-read as a CodeTour. */
function looksLikeCodeTour(raw: unknown): raw is CodeTourFile {
  if (!raw || typeof raw !== "object") return false;
  const m = raw as Record<string, unknown>;
  if (typeof m.schema_version === "string") return false;
  if (typeof m.title !== "string" || !Array.isArray(m.steps)) return false;
  return (m.steps as unknown[]).every(
    (s) =>
      !!s && typeof s === "object" &&
      !("explanation" in (s as object)) && !("step_id" in (s as object)),
  );
}

function convertCodeTour(tour: CodeTourFile, manifestPath: string): TourManifest {
  const manifest: TourManifest = {
    title: tour.title ?? "",
    description: tour.description ?? "",
    schema_version: "1.0",
    steps: (tour.steps ?? []).map((s, i) => convertCodeTourStep(s, i, manifestPath)),
  };
  // `ref` may be a branch or tag as well as a commit; only a hash fits
  // `commit_hash` (and the drift check's prefix comparison against HEAD).
  if (tour.ref && /^[a-f0-9]{7,40}$/i.test(tour.ref)) {
    manifest.commit_hash = tour.ref.toLowerCase();
  }
  return manifest;
}

function convertCodeTourStep(
  s: CodeTourStep,
  index: number,
  manifestPath: string,
): TourStep {
  // A CodeTour step may reference no file at all (content-only steps, or
  // a `directory`/`view` step) — those become content steps here: empty
  // `file_path`, prose still rendered, nothing to highlight or jump to.
  const file = typeof s.file === "string" ? s.file : "";
  // CodeTour steps rarely carry a `title`; the extension labels them from
  // their description. Mirror that: a leading markdown heading (or first
  // line) names the step better than the file's basename ever would.
  let title = s.title;
  if (!title && typeof s.description === "string") {
    const first = s.description.split("\n").find((l) => l.trim().length > 0);
    if (first) {
      const heading = first.trim().replace(/^#+\s*/, "");
      if (heading.length > 0 && heading.length <= 60) title = heading;
    }
  }
  if (!title && file) title = file.replace(/\\/g, "/").split("/").pop();
  if (!title && typeof s.directory === "string") title = s.directory;
  let explanation = typeof s.description === "string" ? s.description : "";
  if (typeof s.directory === "string" && s.directory.length > 0) {
    explanation = "`" + s.directory + "/`\n\n" + explanation;
  }
  return {
    step_id: index + 1,
    title: title || editor.t("step.untitled", { n: String(index + 1) }),
    file_path: file,
    lines: codeTourStepLines(s, file, manifestPath),
    explanation,
  };
}

/** A CodeTour step's anchor as a fresh line range. CodeTour lines are
 * 1-indexed like ours; a bare `line` means that single line, a
 * `selection` spans its start/end lines, and a `pattern` is resolved
 * against the file's current content at load time — which is exactly
 * what makes patterns survive edits that shift line numbers. */
function codeTourStepLines(
  s: CodeTourStep,
  file: string,
  manifestPath: string,
): [number, number] {
  const start = s.selection?.start?.line;
  const end = s.selection?.end?.line;
  if (typeof start === "number" && typeof end === "number" && start >= 1) {
    const from = Math.floor(start);
    return [from, Math.max(from, Math.floor(end))];
  }
  if (typeof s.line === "number" && s.line >= 1) {
    const line = Math.floor(s.line);
    return [line, line];
  }
  if (typeof s.pattern === "string" && file) {
    const hit = findPatternLine(manifestPath, file, s.pattern);
    if (hit) return [hit, hit];
  }
  return [1, 1];
}

/** First line (1-indexed) of `file` matching `pattern`, or null when the
 * file is unreadable or the regex is invalid / matches nothing. */
function findPatternLine(
  manifestPath: string,
  file: string,
  pattern: string,
): number | null {
  const text = editor.readFile(
    editor.authorityPath(resolveStepPath(manifestPath, file)),
  );
  if (typeof text !== "string") return null;
  let re: RegExp;
  try {
    re = new RegExp(pattern);
  } catch {
    return null;
  }
  const lines = text.split("\n");
  for (let i = 0; i < lines.length; i++) {
    if (re.test(lines[i])) return i + 1;
  }
  return null;
}

/** `recorded at <a> · you are on <b>`, or null when there is nothing to say. */
async function commitDrift(manifest: TourManifest): Promise<string | null> {
  if (!manifest.commit_hash) return null;
  const repo = await resolveGitRepo(editor);
  if (!repo) return null;
  const result = await git(editor, repo, ["rev-parse", "--short", "HEAD"]);
  if (!result || result.exit_code !== 0) return null;
  const head = result.stdout.trim();
  if (head.startsWith(manifest.commit_hash) || manifest.commit_hash.startsWith(head)) return null;
  return editor.t("panel.drift", { recorded: manifest.commit_hash, current: head });
}

// ============================================================================
// Tour lifecycle
// ============================================================================

async function openTour(
  manifestPath: string,
  options: { step?: number; visited?: number[]; railOpen?: boolean; focus?: boolean } = {},
): Promise<boolean> {
  const existing = tours.get(manifestPath);
  if (existing) {
    // Already open — focus its tab rather than minting a second buffer. The
    // dock's fast path would happily create one (it runs before the panelId
    // de-duplication path), so the guard has to live here.
    editor.setSplitBuffer(existing.splitId, existing.bufferId);
    editor.focusSplit(existing.splitId);
    lastTourId = existing.id;
    return true;
  }

  const parsed = parseManifest(manifestPath);
  if (typeof parsed === "string") {
    editor.error(parsed);
    return false;
  }
  const manifest = parsed;
  const drift = await commitDrift(manifest);

  const name = tabName(manifest.title);
  // The pane the reader was in when they started the tour is where its code
  // belongs. Read before the dock panel is mounted — mounting makes the dock
  // the active split, and after that there is nothing left to remember.
  const launchedFrom = editor.getActiveSplitId();
  let bufferId = 0;
  let splitId = 0;
  try {
    const result = await editor.createVirtualBufferInSplit({
      name,
      mode: MODE,
      readOnly: true,
      entries: [],
      // `ratio` is the *first* child's share — the existing editor content.
      // 0.65 leaves the dock the remaining ~35%.
      ratio: 0.65,
      role: "utility_dock",
      showLineNumbers: false,
      showCursors: false,
      editingDisabled: true,
      // The lists own their scroll windows; a buffer scrollbar would let a
      // drag push the panel chrome off-screen.
      scrollable: false,
    });
    bufferId = result.bufferId;
    splitId = result.splitId ?? editor.getActiveSplitId();
  } catch (e) {
    editor.error(editor.t("error.open_failed", { error: String(e) }));
    return false;
  }

  const step = clampStep(options.step ?? 0, manifest.steps.length);
  const t: TourInstance = {
    id: manifestPath,
    manifestPath,
    manifest,
    tabName: name,
    step,
    visited: new Set(options.visited ?? [step]),
    bufferId,
    splitId,
    targetSplitId: launchedFrom,
    panel: new WidgetPanel(bufferId),
    namespace: `code-tour:${manifestPath}`,
    railOpen: options.railOpen !== false,
    focusProse: true,
    dockWidth: DEFAULT_DOCK_WIDTH,
    dockHeight: DEFAULT_DOCK_HEIGHT,
    paintedBuffers: new Set<number>(),
    fileMissing: false,
    drift,
  };
  t.visited.add(step);
  tours.set(t.id, t);
  tourByBuffer.set(bufferId, t.id);
  tourByPanel.set(t.panel.id(), t.id);
  lastTourId = t.id;

  t.fileMissing = !isContentStep(currentStep(t)) &&
    !editor.fileExists(
      editor.authorityPath(resolveStepPath(t.manifestPath, currentStep(t).file_path)),
    );
  renderPanel(t);
  editor.setContext("tour-active", true);
  persist();

  if (options.focus !== false) {
    await revealStep(t);
  } else {
    // A restored tour skips `revealStep`, so nothing else would re-measure
    // the dock once its geometry settles.
    await editor.delay(60);
    if (syncDockSize(t)) renderPanel(t);
  }
  return true;
}

/** Tab label. Two manifests with the same title get a disambiguating suffix so
 * the dock's tab bar stays readable. */
function tabName(title: string): string {
  // Build the suffixed form from the label rather than slicing the finished
  // name: `slice(0, -1)` drops a UTF-16 code unit, which splits a surrogate
  // pair on a title ending in an emoji (CONTRIBUTING.md §17).
  const label = editor.t("panel.tab", { title: truncate(title, 24) });
  const taken = new Set([...tours.values()].map((t) => t.tabName));
  let candidate = `*${label}*`;
  let n = 2;
  while (taken.has(candidate)) {
    candidate = `*${label} (${n})*`;
    n++;
  }
  return candidate;
}

function clampStep(step: number, total: number): number {
  if (!Number.isFinite(step)) return 0;
  return Math.min(Math.max(Math.floor(step), 0), total - 1);
}

/** The editor pane this tour opens its steps into, or `null` when the
 * workspace has none to offer.
 *
 * A tour is a reading flow: the code it is talking about belongs in one
 * predictable pane, not wherever focus happened to be when the step changed.
 * `openFile` lands in the *active* split, so stepping through a tour while
 * clicking between splits scattered its files across all of them — step 2 in
 * the left split, step 3 in the right, step 4 back in the left.
 *
 * The pane is the one the tour was launched from, but it is re-resolved on
 * every step rather than trusted: splits close, and a pane that has since
 * become a terminal or a plugin panel cannot hold the step's source. When the
 * remembered pane is no longer usable the left-most/top-most file pane takes
 * over and is remembered in its place. `t.splitId` — the tour's own dock leaf,
 * which every Utility Dock panel shares — is never a candidate, so a step's
 * file cannot land beside the panels.
 */
function targetSplit(t: TourInstance): number | null {
  const panes = editor.describeWorkspace().panes;
  const usable = (splitId: number, kind: string) =>
    kind === "file" && splitId !== t.splitId &&
    ![...tours.values()].some((other) => other.splitId === splitId);
  const remembered = panes.find((p) => p.splitId === t.targetSplitId);
  if (remembered && usable(remembered.splitId, remembered.kind)) return remembered.splitId;
  const fallback = panes.find((p) => usable(p.splitId, p.kind));
  if (!fallback) return null;
  t.targetSplitId = fallback.splitId;
  return fallback.splitId;
}

/** Open a step's file in the tour's target pane.
 *
 * Falls back to `openFile` when no pane qualifies (a workspace showing only
 * terminals and panels, say): the host's own file-open path redirects the
 * active split away from the Utility Dock, so the file still does not become
 * a tab in the dock even though the tour panel holds focus. */
function openStepFile(t: TourInstance, stepPath: string, line: number): void {
  const target = targetSplit(t);
  if (target === null) {
    editor.openFile(stepPath, line, 1);
    return;
  }
  editor.openFileInSplit(target, stepPath, line, 1);
}

/** Pin the tour to the pane its step actually landed in.
 *
 * The fallback path above lets the host choose, and even the targeted path can
 * be redirected. Reading the result back means the *next* step joins this one
 * instead of starting the hunt over — which is the whole point: one pane, all
 * the way through. Callers must `flush` first; `describeWorkspace` reads the
 * plugin-state snapshot, which still describes the pre-open layout until the
 * host has drained the queued command. */
function rememberTargetSplit(t: TourInstance, bufferId: number): void {
  const pane = editor
    .describeWorkspace()
    .panes.find((p) => p.bufferId === bufferId && p.splitId !== t.splitId);
  if (pane) t.targetSplitId = pane.splitId;
}

/** Open the step's file in the tour's target pane, centre it, and paint the
 * highlight. Never moves keyboard focus out of the panel. */
async function revealStep(t: TourInstance): Promise<void> {
  const step = currentStep(t);
  if (isContentStep(step)) {
    // Nothing to open or highlight; drop the previous step's highlight so
    // it does not linger next to prose about something else.
    t.fileMissing = false;
    clearTourOverlays(t);
    renderPanel(t);
    return;
  }
  const stepPath = resolveStepPath(t.manifestPath, step.file_path);
  t.fileMissing = !editor.fileExists(editor.authorityPath(stepPath));
  if (t.fileMissing) {
    // Drop the previous step's highlight. Leaving it up would point at code
    // belonging to a step the user has already moved past, while the panel
    // says the current step's file is missing.
    clearTourOverlays(t);
    renderPanel(t);
    return;
  }

  openStepFile(t, stepPath, step.lines[0]);
  // Hand the keyboard straight back. Both calls are FIFO commands, so
  // queueing the focus restore in the same turn leaves no window where
  // a key typed at the panel lands in the source file instead — parking
  // it after the scroll/overlay awaits below let an n/p/arrow pressed
  // during those awaits type into the code (seen as CI timeouts and a
  // literal "n" inserted into the step's file).
  editor.focusSplit(t.splitId);
  // `openFile` is a queued FIFO command — wait for the queue, not the clock.
  // A fixed 30 ms said nothing about whether the host had drained it; on a
  // loaded runner it often hadn't, `stepBufferId` below then missed the
  // buffer, and the step silently painted no highlight at all.
  await editor.flush();

  const bufferId = stepBufferId(stepPath);
  if (bufferId) {
    rememberTargetSplit(t, bufferId);
    // Centre the range — but never at the cost of its first line. The
    // host parks the passed line a third of the viewport from the top,
    // so clamp the target: a range taller than the pane keeps its top
    // (where the cursor sits) on screen instead of centring the middle
    // and scrolling the cursor away.
    const middle = Math.floor((step.lines[0] + step.lines[1]) / 2) - 1;
    const paneHeight = editor.listSplits().find((sp) => sp.bufferId === bufferId)
      ?.viewport.height ?? 24;
    const keepTopVisible = (step.lines[0] - 1) + Math.floor(paneHeight / 3);
    // Scrolls every split showing the buffer — no split id needed, which is
    // the point: we deliberately don't track which split the host chose.
    editor.scrollBufferToLine(bufferId, Math.min(middle, keepTopVisible));
    await paintStepOverlay(t);
  }
  renderPanel(t);
}

function closeTour(t: TourInstance, closeBuffer: boolean): void {
  clearTourOverlays(t);
  t.panel.unmount();
  tours.delete(t.id);
  tourByBuffer.delete(t.bufferId);
  tourByPanel.delete(t.panel.id());
  if (lastTourId === t.id) {
    lastTourId = tours.size > 0 ? [...tours.keys()][tours.size - 1] : null;
  }
  if (tours.size === 0) editor.setContext("tour-active", false);
  // Closing a tour forgets it — it must not come back on the next launch.
  persist();
  if (closeBuffer) closeTourBuffer(t).catch((e) => editor.error(`code-tour: ${e}`));
  editor.setStatus(editor.t("status.ended", { title: t.manifest.title }));
}

/** Tear down a tour's dock buffer without leaving junk in the dock.
 *
 * `closeBuffer` substitutes a *replacement* buffer — resolved from the active
 * split's focus history, so typically an ordinary source file — into every
 * split still showing the closing one. For a dock pane that means the panel is
 * replaced by a source file, turning the Utility Dock into a tab strip for
 * ordinary files, which is the one thing it must never be. */
async function closeTourBuffer(t: TourInstance): Promise<void> {
  const sibling = [...tours.values()].find((other) => other.splitId === t.splitId);
  if (sibling) {
    // Show the sibling tour first, so no split is displaying the closing
    // buffer by the time the substitution logic runs.
    editor.setSplitBuffer(t.splitId, sibling.bufferId);
    editor.closeBuffer(t.bufferId);
    editor.focusSplit(t.splitId);
    return;
  }

  editor.closeBuffer(t.bufferId);
  // No sibling tour left. If the dock now shows an ordinary file, that is the
  // substitution described above and the dock has outlived its purpose — close
  // it. A *virtual* buffer there is another plugin's panel (diagnostics,
  // search/replace), so leave the dock standing.
  //
  // `flush` first: `describeWorkspace` reads the plugin-state snapshot, which
  // still describes the pre-close layout until the host has drained the queued
  // mutation. Without it the dock always looks like it still holds the tour
  // panel and the stale file is left sitting in the dock.
  await editor.flush();
  const pane = editor
    .describeWorkspace()
    .panes.find((p) => p.splitId === t.splitId);
  if (pane && pane.kind === "file") editor.closeSplit(t.splitId);
}

async function goToStep(
  t: TourInstance,
  index: number,
  options: { focusProse?: boolean } = {},
): Promise<void> {
  const total = t.manifest.steps.length;
  if (index < 0 || index >= total) {
    editor.setStatus(
      index < 0 ? editor.t("status.at_first") : editor.t("status.at_last"),
    );
    return;
  }
  t.step = index;
  t.visited.add(index);
  // Arrow-browsing from the Steps rail keeps the rail focused so the
  // next arrow keeps browsing; everything else parks focus on the prose.
  t.focusProse = options.focusProse !== false;
  lastTourId = t.id;
  persist();
  // Paint the new step's header/rail/prose NOW — revealStep's file-open
  // chain (open, scroll, overlay, focus handback) re-renders again when
  // it lands, but the panel must not wait on it: on a loaded machine
  // that chain can take long enough that the step change looks ignored.
  renderPanel(t);
  await revealStep(t);
}

// ============================================================================
// Instance resolution
// ============================================================================

function tourForActiveBuffer(): TourInstance | null {
  const id = tourByBuffer.get(editor.getActiveBufferId());
  return id ? tours.get(id) ?? null : null;
}

/** The tour a key pressed outside the panel should drive. */
function targetTour(): TourInstance | null {
  return tourForActiveBuffer() ?? (lastTourId ? tours.get(lastTourId) ?? null : null);
}

function dispatch(action: WidgetAction): void {
  const t = targetTour();
  if (t) t.panel.command(action);
}

// ============================================================================
// Handlers — panel mode
// ============================================================================
//
// These handlers resolve their tour with `targetTour()`, not
// `tourForActiveBuffer()` alone. The mode that binds them is attached to the
// tour panel's buffer, so the key can only have been pressed *in a panel* —
// but the handler runs on the plugin thread after the fact, and the
// active-buffer snapshot it would read may be mid-flight: a step's own
// `revealStep` chain moves focus to the source file (`openFile`) and back
// (`focusSplit`), and a key dispatched around that window used to resolve to
// the source buffer, find no tour, and be dropped without a trace — `n`
// pressed right after a step landed simply did nothing. `lastTourId` is
// maintained at open and on every step change, so the fallback names the
// tour the user is driving.

registerHandler("tour_panel_next", () => {
  const t = targetTour();
  if (t) goToStep(t, t.step + 1).catch((e) => editor.error(`code-tour: ${e}`));
});

registerHandler("tour_panel_prev", () => {
  const t = targetTour();
  if (t) goToStep(t, t.step - 1).catch((e) => editor.error(`code-tour: ${e}`));
});

registerHandler("tour_panel_enter", () => dispatch(widgetKey("Enter")));
registerHandler("tour_panel_tab", () => dispatch(widgetKey("Tab")));
registerHandler("tour_panel_shift_tab", () => dispatch(widgetKey("S-Tab")));
registerHandler("tour_panel_up", () => dispatch(widgetKey("Up")));
registerHandler("tour_panel_down", () => dispatch(widgetKey("Down")));
registerHandler("tour_panel_page_up", () => dispatch(widgetKey("PageUp")));
registerHandler("tour_panel_page_down", () => dispatch(widgetKey("PageDown")));
// Selection + clipboard over the prose document (markdown text widget).
registerHandler("tour_panel_shift_up", () => dispatch(widgetKey("S-Up")));
registerHandler("tour_panel_shift_down", () => dispatch(widgetKey("S-Down")));
registerHandler("tour_panel_left", () => dispatch(widgetKey("Left")));
registerHandler("tour_panel_right", () => dispatch(widgetKey("Right")));
registerHandler("tour_panel_shift_left", () => dispatch(widgetKey("S-Left")));
registerHandler("tour_panel_shift_right", () => dispatch(widgetKey("S-Right")));
registerHandler("tour_panel_home", () => dispatch(widgetKey("Home")));
registerHandler("tour_panel_end", () => dispatch(widgetKey("End")));
registerHandler("tour_panel_copy", () => dispatch(widgetKey("C-c")));
registerHandler("tour_panel_select_all", () => dispatch(widgetKey("C-a")));

registerHandler("tour_panel_jump", () => {
  const t = targetTour();
  if (t) jumpToCode(t);
});

registerHandler("tour_panel_rehighlight", () => {
  const t = targetTour();
  if (t) revealStep(t).catch((e) => editor.error(`code-tour: ${e}`));
});

registerHandler("tour_panel_steps", () => {
  const t = targetTour();
  if (!t) return;
  if (!t.railOpen || t.dockWidth < RAIL_MIN_WIDTH) {
    t.railOpen = true;
    persist();
    renderPanel(t);
  }
  if (showRail(t)) t.panel.setFocusKey("stepList");
});

registerHandler("tour_panel_close", () => {
  const t = targetTour();
  if (t) closeTour(t, true);
});

function jumpToCode(t: TourInstance): void {
  const step = currentStep(t);
  if (isContentStep(step)) {
    editor.setStatus(editor.t("panel.content_step"));
    return;
  }
  if (t.fileMissing) {
    editor.setStatus(editor.t("panel.location_missing"));
    return;
  }
  // Deliberately no `focusSplit` back to the panel afterwards: this is the
  // explicit "put me in the code" gesture, so focus stays in the pane the
  // step opened into — the same pane every other step uses.
  openStepFile(t, resolveStepPath(t.manifestPath, step.file_path), step.lines[0]);
  editor.setStatus(
    editor.t("status.jumped", { file: step.file_path, line: String(step.lines[0]) }),
  );
}

// ============================================================================
// Workspace tour discovery
// ============================================================================

/** Well-known single-file tour locations, relative to the workspace root:
 * ours, then the CodeTour extension's. */
const KNOWN_TOUR_FILES = [
  ".fresh-tour.json",
  ".tour",
  "main.tour",
  ".vscode/main.tour",
];

/** Directories the CodeTour extension scans for tour files. */
const KNOWN_TOUR_DIRS = [".tours", ".vscode/tours", ".github/tours"];

/** How deep inside a tour directory to look — CodeTour allows arbitrary
 * nesting, but an unbounded walk of a weird workspace is worse than
 * missing a tour buried four folders down. */
const TOUR_SCAN_DEPTH = 3;

interface DiscoveredTour {
  path: string;
  title: string;
  steps: number;
}

function collectTourFiles(dir: string, depth: number, out: string[]): void {
  let entries: ReturnType<typeof editor.readDir>;
  try {
    entries = editor.readDir(editor.authorityPath(dir));
  } catch {
    return;
  }
  for (const entry of entries) {
    const p = editor.pathJoin(dir, entry.name);
    if (entry.is_dir) {
      if (depth < TOUR_SCAN_DEPTH) collectTourFiles(p, depth + 1, out);
    } else if (
      entry.is_file &&
      (entry.name.endsWith(".tour") || entry.name.endsWith(".json"))
    ) {
      out.push(p);
    }
  }
}

/** Every loadable tour in the workspace's well-known locations, either
 * format. Files that do not parse as a tour are skipped silently — a
 * `.json` in `.tours/` may simply be something else. */
function discoverTours(): DiscoveredTour[] {
  const root = editor.getCwd();
  const candidates: string[] = [];
  for (const rel of KNOWN_TOUR_FILES) {
    const p = editor.pathJoin(root, rel);
    if (editor.fileExists(editor.authorityPath(p))) candidates.push(p);
  }
  for (const rel of KNOWN_TOUR_DIRS) {
    collectTourFiles(editor.pathJoin(root, rel), 0, candidates);
  }
  const found: DiscoveredTour[] = [];
  for (const path of candidates) {
    const parsed = parseManifest(path);
    if (typeof parsed === "string") continue;
    found.push({ path, title: parsed.title, steps: parsed.steps.length });
  }
  return found;
}

/** Same file browser as `tour_load`. This deliberately does NOT
 * auto-open or suggest a discovered tour: an opinionated guess that
 * opens the wrong manifest is worse than one more keystroke in a
 * browser that starts where tour files actually live. `discoverTours`
 * stays available to scripts through the exported plugin API. */
async function tour_discover(): Promise<void> {
  await tour_load();
}
registerHandler("tour_discover", tour_discover);

// ============================================================================
// Handlers — commands
// ============================================================================

async function tour_load(): Promise<void> {
  // The editor's native Open File browser, in pick mode: Backspace-up /
  // Tab-descend navigation, resolving with the chosen path instead of
  // opening a buffer. Anchored at the workspace root with hidden files
  // forced visible — tour files are dotfiles (`.fresh-tour.json`,
  // `.tours/*.tour`), so the default anchor (the active file's
  // directory, dotfiles hidden) shows a browser with nothing pickable
  // in it.
  const path = await editor.pickFile(
    editor.t("prompt.path") + " ",
    editor.getCwd(),
    true,
  );
  if (path) await openTour(path);
}
registerHandler("tour_load", tour_load);

async function tour_next(): Promise<void> {
  const t = targetTour();
  if (t) await goToStep(t, t.step + 1);
}
registerHandler("tour_next", tour_next);

async function tour_prev(): Promise<void> {
  const t = targetTour();
  if (t) await goToStep(t, t.step - 1);
}
registerHandler("tour_prev", tour_prev);

function tour_exit(): void {
  const t = targetTour();
  if (t) closeTour(t, true);
}
registerHandler("tour_exit", tour_exit);

// ============================================================================
// Events
// ============================================================================

editor.on("widget_event", (args) => {
  const id = tourByPanel.get(args.panel_id);
  if (!id) return;
  const t = tours.get(id);
  if (!t) return;
  lastTourId = t.id;

  if (args.event_type === "activate") {
    switch (args.widget_key) {
      case "next":
        goToStep(t, t.step + 1).catch((e) => editor.error(`code-tour: ${e}`));
        return;
      case "prev":
        goToStep(t, t.step - 1).catch((e) => editor.error(`code-tour: ${e}`));
        return;
      case "exit":
        closeTour(t, true);
        return;
      case "jump":
        jumpToCode(t);
        return;
      case "rehighlight":
        revealStep(t).catch((e) => editor.error(`code-tour: ${e}`));
        return;
      case "stepList": {
        const idx = (args.payload as { index?: number } | undefined)?.index;
        if (typeof idx === "number") {
          goToStep(t, idx).catch((e) => editor.error(`code-tour: ${e}`));
        }
        return;
      }
      case "prose":
        jumpToCode(t);
        return;
    }
    return;
  }

  if (args.event_type === "select" && args.widget_key === "stepList") {
    const payload = args.payload as { index?: number; via?: string } | undefined;
    if (typeof payload?.index === "number" && payload.index !== t.step) {
      // Rail navigation — click or ↑/↓ — keeps the rail focused, so the
      // arrows right after a click keep stepping. (`n`/`p`/the buttons
      // still park focus on the prose for reading.)
      goToStep(t, payload.index, { focusProse: false })
        .catch((e) => editor.error(`code-tour: ${e}`));
    }
  }
});

editor.on("buffer_closed", (data) => {
  const id = tourByBuffer.get(data.buffer_id);
  if (!id) return;
  const t = tours.get(id);
  if (t) closeTour(t, false);
});

editor.on("viewport_changed", (data) => {
  const id = tourByBuffer.get(data.buffer_id);
  if (!id) return;
  const t = tours.get(id);
  if (!t) return;
  if (syncDockSize(t)) renderPanel(t);
});

// Restore. `ready` fires after the workspace restore, so window state is
// already loaded here; `plugins_loaded` fires before it and would see nothing.
editor.on("ready", () => {
  const stored = readPersisted();
  if (stored.length === 0) return;
  restoreTours(stored).catch((e) => editor.error(`code-tour: ${e}`));
});

async function restoreTours(stored: PersistedTour[]): Promise<void> {
  // Where the workspace restore left the user. Mounting a dock panel makes the
  // dock the active split and hands it the keyboard, so a tour left open weeks
  // ago would take focus from the file the editor just restored — the first
  // keystroke would go to a tour the user had not asked for. Restoring is not
  // opening: the tab comes back, the focus does not move.
  const focusBefore = editor.getActiveSplitId();
  const dropped: string[] = [];
  for (const entry of stored) {
    const parsed = parseManifest(entry.manifestPath);
    if (typeof parsed === "string") {
      dropped.push(entry.manifestPath);
      continue;
    }
    // `focus: false` — restoring a tour must not fight the workspace restore
    // for what the editor is showing.
    await openTour(entry.manifestPath, {
      step: entry.step,
      visited: entry.visited,
      railOpen: entry.railOpen,
      focus: false,
    });
  }
  persist();
  if (tours.size > 0) {
    // `flush` so the pending dock mounts are applied before we move focus —
    // otherwise the queued `set_active_split` from the last mount lands after
    // this and the dock takes the keyboard anyway.
    await editor.flush();
    editor.focusSplit(focusBefore);
  }
  if (dropped.length > 0) {
    editor.setStatus(
      editor.t("status.dropped", { tours: dropped.join(", ") }),
    );
  }
}

// ============================================================================
// Mode + registration
// ============================================================================

const modeBindings: [string, string][] = [
  ["n", "tour_panel_next"],
  ["Right", "tour_panel_right"],
  ["Space", "tour_panel_next"],
  ["p", "tour_panel_prev"],
  ["Left", "tour_panel_left"],
  ["Backspace", "tour_panel_prev"],
  ["Return", "tour_panel_enter"],
  ["Tab", "tour_panel_tab"],
  ["S-Tab", "tour_panel_shift_tab"],
  ["Up", "tour_panel_up"],
  ["Down", "tour_panel_down"],
  ["S-Up", "tour_panel_shift_up"],
  ["S-Down", "tour_panel_shift_down"],
  ["S-Left", "tour_panel_shift_left"],
  ["S-Right", "tour_panel_shift_right"],
  ["Home", "tour_panel_home"],
  ["End", "tour_panel_end"],
  ["C-c", "tour_panel_copy"],
  ["C-a", "tour_panel_select_all"],
  ["PageUp", "tour_panel_page_up"],
  ["PageDown", "tour_panel_page_down"],
  ["g", "tour_panel_steps"],
  ["r", "tour_panel_rehighlight"],
  ["q", "tour_panel_close"],
  ["Escape", "tour_panel_close"],
];
editor.defineMode(MODE, modeBindings, true, false);

editor.registerCommand(
  "%cmd.tour_load",
  "%cmd.tour_load_desc",
  "tour_load",
  null,
);
editor.registerCommand(
  "%cmd.tour_discover",
  "%cmd.tour_discover_desc",
  "tour_discover",
  null,
);
editor.registerCommand(
  "%cmd.tour_next",
  "%cmd.tour_next_desc",
  "tour_next",
  "tour-active",
);
editor.registerCommand(
  "%cmd.tour_prev",
  "%cmd.tour_prev_desc",
  "tour_prev",
  "tour-active",
);
editor.registerCommand(
  "%cmd.tour_exit",
  "%cmd.tour_exit_desc",
  "tour_exit",
  "tour-active",
);

// Published so a script (and therefore an agent driving the editor through
// `fresh --cmd script run`) can author a tour and open it in one go.
editor.exportPluginApi("code-tour", {
  openTour: (path: string) => openTour(path),
  nextStep: () => {
    const t = targetTour();
    return t ? goToStep(t, t.step + 1) : Promise.resolve();
  },
  prevStep: () => {
    const t = targetTour();
    return t ? goToStep(t, t.step - 1) : Promise.resolve();
  },
  closeTour: (path: string) => {
    const t = tours.get(path);
    if (t) closeTour(t, true);
    return !!t;
  },
  listTours: () =>
    [...tours.values()].map((t) => ({
      path: t.manifestPath,
      title: t.manifest.title,
      step: t.step,
      total: t.manifest.steps.length,
    })),
  discoverTours: () => discoverTours(),
});

editor.debug("Code Tour plugin loaded");
