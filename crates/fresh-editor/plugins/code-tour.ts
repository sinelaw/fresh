/// <reference path="./lib/fresh.d.ts" />

import { git, resolveGitRepo } from "./lib/git_repo.ts";
import {
  button,
  col,
  divider,
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
const RAIL_MIN_WIDTH = 130;
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

type RGB = [number, number, number];

const C = {
  dim: [120, 120, 140] as RGB,
  accent: [140, 190, 255] as RGB,
  code: [200, 170, 120] as RGB,
  rule: [80, 80, 100] as RGB,
  warn: [230, 170, 70] as RGB,
  path: [220, 160, 80] as RGB,
};

// ============================================================================
// Small string helpers (codepoint-aware; the host measures in codepoints)
// ============================================================================

function charLen(s: string): number {
  let n = 0;
  for (const _c of s) n++;
  return n;
}

function chars(s: string): string[] {
  const out: string[] = [];
  for (const c of s) out.push(c);
  return out;
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
// Markdown → styled rows
//
// The step `explanation` is authored markdown. The old popup dumped it into a
// plain text box, which dropped the head of every wrapped line. Here it is
// parsed once per (step, width) into styled rows the List widget scrolls.
// ============================================================================

type SpanKind = "bold" | "italic" | "code";
interface Span {
  start: number;
  end: number;
  kind: SpanKind;
}

/** Strip `**bold**` / `` `code` `` markers, recording the ranges they covered
 * as character offsets into the *stripped* text. Wrapping then happens on
 * plain text and the spans are intersected with each output line. */
function parseInline(src: string): { text: string; spans: Span[] } {
  const cs = chars(src);
  let text = "";
  let len = 0;
  const spans: Span[] = [];
  let boldStart: number | null = null;
  let italicStart: number | null = null;
  let codeStart: number | null = null;

  for (let i = 0; i < cs.length; i++) {
    const c = cs[i];
    if (c === "*" && cs[i + 1] === "*" && codeStart === null) {
      if (boldStart === null) {
        boldStart = len;
      } else {
        if (len > boldStart) spans.push({ start: boldStart, end: len, kind: "bold" });
        boldStart = null;
      }
      i++;
      continue;
    }
    // Single `*` is emphasis — checked after `**` so bold still wins.
    if (c === "*" && codeStart === null) {
      if (italicStart === null) {
        italicStart = len;
      } else {
        if (len > italicStart) spans.push({ start: italicStart, end: len, kind: "italic" });
        italicStart = null;
      }
      continue;
    }
    if (c === "`") {
      if (codeStart === null) {
        codeStart = len;
      } else {
        if (len > codeStart) spans.push({ start: codeStart, end: len, kind: "code" });
        codeStart = null;
      }
      continue;
    }
    text += c;
    len++;
  }
  // Unterminated markers: the text is already emitted verbatim, so there is
  // nothing to close — dropping the dangling span is the right recovery.
  return { text, spans };
}

/** Greedy word wrap. Returns character ranges into `text`. */
function wrapRanges(text: string, width: number): Array<[number, number]> {
  if (width <= 0) return [[0, charLen(text)]];
  const cs = chars(text);
  if (cs.length === 0) return [[0, 0]];
  const out: Array<[number, number]> = [];
  let lineStart = 0;
  let lastBreak = -1;
  let i = 0;
  while (i < cs.length) {
    if (cs[i] === " ") lastBreak = i;
    if (i - lineStart >= width) {
      // Break at the last space that fits; if the word itself is longer than
      // the line, hard-break it rather than overflowing the panel.
      const brk = lastBreak > lineStart ? lastBreak : i;
      out.push([lineStart, brk]);
      lineStart = brk;
      while (lineStart < cs.length && cs[lineStart] === " ") lineStart++;
      lastBreak = -1;
      i = lineStart;
      continue;
    }
    i++;
  }
  out.push([lineStart, cs.length]);
  return out;
}

/** Build one row: a plain `indent` prefix, then `text[lo,hi)` split at span
 * boundaries so each styled run becomes its own segment. */
function spannedRow(
  indent: string,
  text: string,
  spans: Span[],
  lo: number,
  hi: number,
  base?: Partial<OverlayOptions>,
): TextPropertyEntry {
  const cs = chars(text);
  const segments: StyledSegment[] = [];
  if (indent.length > 0) segments.push({ text: indent });

  // Boundaries inside [lo, hi) where the active style changes.
  const cuts = new Set<number>([lo, hi]);
  for (const s of spans) {
    if (s.end > lo && s.start < hi) {
      cuts.add(Math.max(s.start, lo));
      cuts.add(Math.min(s.end, hi));
    }
  }
  const ordered = [...cuts].sort((a, b) => a - b);
  for (let i = 0; i + 1 < ordered.length; i++) {
    const a = ordered[i];
    const b = ordered[i + 1];
    if (b <= a) continue;
    const piece = cs.slice(a, b).join("");
    const covering = spans.find((s) => s.start <= a && s.end >= b);
    let style: Partial<OverlayOptions> | undefined = base;
    if (covering?.kind === "bold") style = { ...(base ?? {}), bold: true };
    else if (covering?.kind === "italic") style = { ...(base ?? {}), italic: true };
    else if (covering?.kind === "code") style = { ...(base ?? {}), fg: C.code };
    segments.push(style ? { text: piece, style } : { text: piece });
  }
  if (segments.length === 0) segments.push({ text: "" });
  return styledRow(segments);
}

/** Render a markdown `explanation` into rows sized for `width` columns. */
function renderExplanation(md: string, width: number): TextPropertyEntry[] {
  const rows: TextPropertyEntry[] = [];
  const w = Math.max(width, 8);
  let inFence = false;

  for (const rawLine of md.split("\n")) {
    const line = rawLine.replace(/\r$/, "");
    const fence = line.trimStart().startsWith("```");
    if (fence) {
      inFence = !inFence;
      continue; // the fence markers themselves are chrome, not content
    }
    if (inFence) {
      rows.push(styledRow([{ text: "  " + truncate(line, w - 2), style: { fg: C.code } }]));
      continue;
    }
    if (line.trim().length === 0) {
      rows.push(styledRow([{ text: "" }]));
      continue;
    }

    const heading = /^(#{1,6})\s+(.*)$/.exec(line);
    if (heading) {
      const { text, spans } = parseInline(heading[2].trim());
      for (const [lo, hi] of wrapRanges(text, w)) {
        rows.push(spannedRow("", text, spans, lo, hi, { bold: true, fg: C.accent }));
      }
      rows.push(
        styledRow([{
          text: "─".repeat(Math.min(charLen(text), w)),
          style: { fg: C.rule },
        }]),
      );
      continue;
    }

    const bullet = /^\s*[-*]\s+(.*)$/.exec(line);
    if (bullet) {
      pushWrapped(rows, bullet[1], "  • ", "    ", w);
      continue;
    }

    const ordered = /^\s*(\d+)\.\s+(.*)$/.exec(line);
    if (ordered) {
      const marker = `  ${ordered[1]}. `;
      pushWrapped(rows, ordered[2], marker, " ".repeat(charLen(marker)), w);
      continue;
    }

    pushWrapped(rows, line.trim(), "", "", w);
  }

  // Trim trailing blanks so the section does not end in dead space.
  while (rows.length > 0 && rows[rows.length - 1].segments?.[0]?.text === "") rows.pop();
  return rows;
}

/** Wrap `src` with a `first` prefix on line 1 and `cont` on the rest, so a
 * bullet's continuation lines align under its text rather than its marker. */
function pushWrapped(
  rows: TextPropertyEntry[],
  src: string,
  first: string,
  cont: string,
  width: number,
): void {
  const { text, spans } = parseInline(src);
  const inner = Math.max(width - charLen(first), 8);
  const ranges = wrapRanges(text, inner);
  ranges.forEach(([lo, hi], i) => {
    rows.push(spannedRow(i === 0 ? first : cont, text, spans, lo, hi));
  });
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

function showRail(t: TourInstance): boolean {
  return t.railOpen && t.dockWidth >= RAIL_MIN_WIDTH;
}

/** Rows the body section's list may occupy.
 *
 * The panel must render *exactly* as many lines as the pane can show. One line
 * too many and the buffer becomes scrollable, at which point a click anywhere
 * that isn't a widget hit area — a section border, the padding under a short
 * step — moves the buffer cursor and scrolls the panel's own header out of
 * view. Pane height less the tab bar, then less
 * `header + rule + 2 border rows + location + hints`. */
function bodyRows(t: TourInstance): number {
  return Math.max(3, t.dockHeight - 7);
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

  return row(
    spacer(1),
    raw([styledRow(titleSegments, { truncateToChars: Math.max(8, t.dockWidth - 40) })], "title"),
    flexSpacer(),
    raw([styledRow([{ text: counter, style: { fg: C.dim } }])], "progress"),
    spacer(2),
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

function buildProseRows(t: TourInstance, width: number): TextPropertyEntry[] {
  const step = currentStep(t);
  const rows: TextPropertyEntry[] = [];
  if (t.fileMissing) {
    rows.push(
      styledRow([{
        text: "⚠  " + editor.t("panel.file_missing", { file: step.file_path }),
        style: { fg: C.warn },
      }]),
    );
    if (t.drift) {
      rows.push(styledRow([{ text: "   " + t.drift, style: { fg: C.dim } }]));
    }
    rows.push(styledRow([{ text: "" }]));
  }
  const body = renderExplanation(step.explanation, width);
  if (body.length === 0) {
    rows.push(styledRow([{ text: editor.t("panel.no_explanation"), style: { fg: C.dim } }]));
  }
  return rows.concat(body);
}

function buildBody(t: TourInstance): WidgetSpec {
  const step = currentStep(t);
  const rows = bodyRows(t);
  const label = `${t.step + 1}/${t.manifest.steps.length} · ${step.title}`;

  if (!showRail(t)) {
    const inner = Math.max(8, t.dockWidth - 5);
    return labeledSection({
      label: truncate(label, Math.max(8, t.dockWidth - 6)),
      key: "proseBox",
      child: list({
        // A List scrolls by moving its selection, so it needs one: with the
        // default -1 the hint bar's "↑↓ scroll" does nothing.
        items: buildProseRows(t, inner),
        selectedIndex: 0,
        visibleRows: rows,
        key: "proseList",
      }),
    });
  }

  // Mirror the host's allocation exactly: a Row's Block children with an
  // explicit `widthPct` each take `panel_width * pct / 100` — the prose column
  // is NOT "whatever the rail left over". Getting this wrong by a column makes
  // the renderer truncate every wrapped line with an ellipsis, eating the word
  // it broke on. The extra column of slack absorbs rounding.
  const railWidth = Math.max(8, Math.floor((t.dockWidth * RAIL_PCT) / 100) - 4);
  const proseWidth = Math.max(
    8,
    Math.floor((t.dockWidth * (100 - RAIL_PCT)) / 100) - 5,
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
      child: list({
        items: buildProseRows(t, proseWidth),
        selectedIndex: 0,
        visibleRows: rows,
        key: "proseList",
      }),
    }),
  );
}

/** Rendered width of the location row's trailing buttons, including the
 * spacer between them. `[ Label ]` is the button chrome, so a label of n
 * characters occupies n + 4 columns. */
function actionsWidth(t: TourInstance): number {
  const chrome = 4;
  if (t.fileMissing) return charLen(editor.t("btn.skip")) + chrome;
  if (t.dockWidth < COMPACT_WIDTH) return charLen(editor.t("btn.jump_short")) + chrome;
  return (
    charLen(editor.t("btn.jump")) + chrome +
    2 +
    charLen(editor.t("btn.rehighlight")) + chrome
  );
}

function buildLocation(t: TourInstance): WidgetSpec {
  const step = currentStep(t);
  const compact = t.dockWidth < COMPACT_WIDTH;
  const location = t.fileMissing
    ? editor.t("panel.location_missing")
    : editor.t("panel.location", {
      file: step.file_path,
      from: String(step.lines[0]),
      to: String(step.lines[1]),
    });

  const actions: WidgetSpec[] = t.fileMissing
    ? [button(editor.t("btn.skip"), { key: "next", disabled: t.step === t.manifest.steps.length - 1 })]
    : compact
    ? [button(editor.t("btn.jump_short"), { key: "jump" })]
    : [
      button(editor.t("btn.jump"), { key: "jump" }),
      spacer(2),
      button(editor.t("btn.rehighlight"), { key: "rehighlight" }),
    ];

  return row(
    spacer(1),
    raw([
      styledRow([
        { text: t.fileMissing ? "⚠ " : "▸ ", style: { fg: t.fileMissing ? C.warn : C.dim } },
        { text: location, style: { fg: t.fileMissing ? C.warn : C.path } },
      ], {
        // Hard cap so this row can never wrap: whatever the buttons need,
        // plus the leading marker and the row's own margins. A wrapped
        // location row costs a line the pane does not have.
        truncateToChars: Math.max(8, t.dockWidth - actionsWidth(t) - 6),
      }),
    ], "location"),
    flexSpacer(),
    ...actions,
    spacer(1),
  );
}

function buildHints(t: TourInstance): HintEntry[] {
  const core: HintEntry[] = [
    { keys: "n/→", label: editor.t("hint.next") },
    { keys: "p/←", label: editor.t("hint.prev") },
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
      divider({ style: { fg: C.rule } }),
      buildBody(t),
      buildLocation(t),
      hintBar(buildHints(t)),
    ),
  );
  // `selectedIndex` on the spec is a seed the host ignores after first render,
  // so push the rail's selection explicitly on every step change.
  if (showRail(t)) t.panel.setSelectedIndex("stepList", t.step);
  if (t.focusProse) {
    t.focusProse = false;
    t.panel.setSelectedIndex("proseList", 0);
    t.panel.setFocusKey("proseList");
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
 * renders is the dock panel, not the source file — which is why the highlight
 * never painted before.
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
  const bufferId = stepBufferId(step.file_path);
  if (!bufferId) return;

  clearTourOverlays(t);
  const range = await lineRangeBytes(bufferId, step.lines[0], step.lines[1]);
  if (!range) {
    editor.warn(
      `Tour: could not resolve lines ${step.lines[0]}-${step.lines[1]} in ${step.file_path}`,
    );
    return;
  }
  editor.addOverlay(bufferId, t.namespace, range[0], range[1], {
    bg: [42, 74, 106],
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
  let manifest: TourManifest;
  try {
    manifest = JSON.parse(content);
  } catch (e) {
    return editor.t("error.parse_failed", { path, error: String(e) });
  }
  if (manifest.schema_version !== "1.0") {
    return editor.t("error.bad_version", { version: String(manifest.schema_version) });
  }
  if (!manifest.steps || manifest.steps.length === 0) {
    return editor.t("error.no_steps");
  }
  return manifest;
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

  t.fileMissing = !editor.fileExists(editor.authorityPath(currentStep(t).file_path));
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
  const base = `*${editor.t("panel.tab", { title: truncate(title, 24) })}*`;
  let candidate = base;
  let n = 2;
  const taken = new Set(
    [...tours.values()].map((t) => t.tabName),
  );
  while (taken.has(candidate)) {
    candidate = `${base.slice(0, -1)} (${n})*`;
    n++;
  }
  return candidate;
}

function clampStep(step: number, total: number): number {
  if (!Number.isFinite(step)) return 0;
  return Math.min(Math.max(Math.floor(step), 0), total - 1);
}

/** Open the step's file in the editor split, centre it, and paint the
 * highlight. Never moves keyboard focus out of the panel. */
async function revealStep(t: TourInstance): Promise<void> {
  const step = currentStep(t);
  t.fileMissing = !editor.fileExists(editor.authorityPath(step.file_path));
  if (t.fileMissing) {
    renderPanel(t);
    return;
  }

  // `openFile` — not `openFileInSplit`. The host's file-open path already
  // redirects the active split away from the Utility Dock, so a file never
  // becomes a tab in the dock even though the tour panel is what holds focus.
  // Naming a split explicitly would bypass that guard and let the step's
  // source land beside the panels.
  editor.openFile(step.file_path, step.lines[0], 1);
  await editor.delay(30);

  const bufferId = stepBufferId(step.file_path);
  if (bufferId) {
    const middle = Math.floor((step.lines[0] + step.lines[1]) / 2) - 1;
    // Scrolls every split showing the buffer — no split id needed, which is
    // the point: we deliberately don't track which split the host chose.
    editor.scrollBufferToLine(bufferId, middle);
    await paintStepOverlay(t);
  }
  // `openFile` focused the editor. Hand the keyboard back to the panel so
  // n / p keep stepping instead of typing into the source file.
  editor.focusSplit(t.splitId);
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

async function goToStep(t: TourInstance, index: number): Promise<void> {
  const total = t.manifest.steps.length;
  if (index < 0 || index >= total) {
    editor.setStatus(
      index < 0 ? editor.t("status.at_first") : editor.t("status.at_last"),
    );
    return;
  }
  t.step = index;
  t.visited.add(index);
  t.focusProse = true;
  lastTourId = t.id;
  persist();
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
  const t = tourForActiveBuffer();
  if (t) t.panel.command(action);
}

// ============================================================================
// Handlers — panel mode
// ============================================================================

registerHandler("tour_panel_next", () => {
  const t = tourForActiveBuffer();
  if (t) goToStep(t, t.step + 1).catch((e) => editor.error(`code-tour: ${e}`));
});

registerHandler("tour_panel_prev", () => {
  const t = tourForActiveBuffer();
  if (t) goToStep(t, t.step - 1).catch((e) => editor.error(`code-tour: ${e}`));
});

registerHandler("tour_panel_enter", () => dispatch(widgetKey("Enter")));
registerHandler("tour_panel_tab", () => dispatch(widgetKey("Tab")));
registerHandler("tour_panel_shift_tab", () => dispatch(widgetKey("S-Tab")));
registerHandler("tour_panel_up", () => dispatch(widgetKey("Up")));
registerHandler("tour_panel_down", () => dispatch(widgetKey("Down")));
registerHandler("tour_panel_page_up", () => dispatch(widgetKey("PageUp")));
registerHandler("tour_panel_page_down", () => dispatch(widgetKey("PageDown")));

registerHandler("tour_panel_jump", () => {
  const t = tourForActiveBuffer();
  if (t) jumpToCode(t);
});

registerHandler("tour_panel_rehighlight", () => {
  const t = tourForActiveBuffer();
  if (t) revealStep(t).catch((e) => editor.error(`code-tour: ${e}`));
});

registerHandler("tour_panel_steps", () => {
  const t = tourForActiveBuffer();
  if (!t) return;
  if (!t.railOpen || t.dockWidth < RAIL_MIN_WIDTH) {
    t.railOpen = true;
    persist();
    renderPanel(t);
  }
  if (showRail(t)) t.panel.setFocusKey("stepList");
});

registerHandler("tour_panel_close", () => {
  const t = tourForActiveBuffer();
  if (t) closeTour(t, true);
});

function jumpToCode(t: TourInstance): void {
  if (t.fileMissing) {
    editor.setStatus(editor.t("panel.location_missing"));
    return;
  }
  const step = currentStep(t);
  // Deliberately no `focusSplit` back to the panel afterwards: this is the
  // explicit "put me in the code" gesture, so focus stays where `openFile`
  // left it.
  editor.openFile(step.file_path, step.lines[0], 1);
  editor.setStatus(
    editor.t("status.jumped", { file: step.file_path, line: String(step.lines[0]) }),
  );
}

// ============================================================================
// Handlers — commands
// ============================================================================

async function tour_load(): Promise<void> {
  const result = await editor.prompt(editor.t("prompt.path"), ".fresh-tour.json");
  if (result) await openTour(result);
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
      case "proseList":
        jumpToCode(t);
        return;
    }
    return;
  }

  if (args.event_type === "select" && args.widget_key === "stepList") {
    const payload = args.payload as { index?: number; via?: string } | undefined;
    if (typeof payload?.index === "number" && payload.via === "click") {
      goToStep(t, payload.index).catch((e) => editor.error(`code-tour: ${e}`));
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
  ["Right", "tour_panel_next"],
  ["Space", "tour_panel_next"],
  ["p", "tour_panel_prev"],
  ["Left", "tour_panel_prev"],
  ["Backspace", "tour_panel_prev"],
  ["Return", "tour_panel_enter"],
  ["Tab", "tour_panel_tab"],
  ["S-Tab", "tour_panel_shift_tab"],
  ["Up", "tour_panel_up"],
  ["Down", "tour_panel_down"],
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
});

editor.debug("Code Tour plugin loaded");
