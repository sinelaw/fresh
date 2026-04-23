/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

// ═════════════════════════════════════════════════════════════════════
//   DASHBOARD PLUGIN
//
//   Shows a TUI dashboard with weather, git, GitHub PRs, and disk
//   stats when there's no real work open — either at startup or
//   after the user closes the last file buffer (instead of the
//   default untitled scratch).
//
//   Controlled via the standard plugin config flag
//   (`plugins.dashboard.enabled` in config.json / Settings UI).
//   When the plugin is enabled and loaded, it subscribes to the
//   editor hooks that drive the dashboard's lifecycle; when disabled
//   it is never loaded, so no buffers are created, no timers run,
//   and no network fetches fire.
//
//   A second flag `plugins.dashboard.auto-open` (default true) gates
//   only the ambient open paths (startup + last-buffer-closed). When
//   false the plugin still loads and the "Show Dashboard" command is
//   still available — it just won't appear on its own.
//
//   - Auto-centers both horizontally and vertically. Repaints when the
//     viewport changes (terminal resize, file-explorer toggle, split
//     reshape).
//   - Auto-refreshes every 5 seconds while visible.
//   - All colors are theme keys → repaints for free on theme switch.
//   - Clickable rows (repo URL, branch name, PR numbers, review-branch
//     action) route clicks through the mouse_click hook, so they work
//     in terminals that swallow OSC-8 hyperlinks. The OSC-8 `url` span
//     is still set as a fallback for terminals that do honor it.
//   - Content is pushed to the buffer via `setVirtualBufferContent`, a
//     single atomic command. Going through clearNamespace / deleteRange /
//     insertText / addOverlay would let a render frame slip in between
//     the delete and the insert — the plugin thread pushes each call as
//     an independent message onto an MPSC channel that the editor drains
//     non-blocking every tick, so a partial-batch render is possible and
//     observably flickery. `setVirtualBufferContent` ships text + all
//     inline overlays in one message, so the editor applies the whole
//     replacement before the next frame.
// ═════════════════════════════════════════════════════════════════════

type Span = {
    start: number;
    end: number;
    fg?: string;
    bg?: string;
    bold?: boolean;
    underline?: boolean;
    url?: string;
};
// Click action attached to whole rows — dispatched by the mouse_click
// handler, which looks up the clicked buffer row in Draw.rowActions.
// Since terminals that swallow OSC-8 hyperlinks are common, we can't
// rely on the `url` span alone; routing clicks through the editor
// guarantees that PR numbers / repo / custom section rows are always
// actionable. The `callback` variant lets third-party sections wire
// up arbitrary click handlers via the public `DashboardContext` API.
type ClickAction =
    | { kind: "open-url"; url: string }
    | { kind: "callback"; fn: () => void };
type Draw = {
    text: string;
    spans: Span[];
    // currentRow / currentCol are maintained by `emit()` + `newline()` so
    // click-action ranges land on the same cells the underlined text
    // actually occupies. A click at (buffer_row, buffer_col) only fires
    // if it falls inside one of the row's registered ranges — padding
    // spaces, kv labels, and the frame border inside an inner row are
    // not clickable, matching the visual affordance.
    currentRow: number;
    currentCol: number;
    rowActions: Map<number, ClickActionRange[]>;
};

/** Column range within a row that carries a click target. */
type ClickActionRange = { colStart: number; colEnd: number; action: ClickAction };
const MAX_INNER = 72; // content width excluding frame + centering pad

const C = {
    frame: "ui.popup_border_fg",
    title: "syntax.keyword",
    accent: "syntax.function",
    value: "syntax.string",
    number: "syntax.constant",
    muted: "syntax.comment",
    branch: "syntax.variable",
    ok: "ui.file_status_added_fg",
    warn: "syntax.constant",
    err: "diagnostic.error_fg",
    barFill: "syntax.function",
};

// ── Public section API ─────────────────────────────────────────────────
//
// Third-party plugins (and user init.ts) can add their own dashboard
// rows via the exported plugin API. See `editor.exportPluginApi` at
// the bottom of this file and the usage example in the init.ts
// starter template (`init_script.rs::STARTER_TEMPLATE`).

// Named palette colors exposed to section callbacks. Each maps to a
// theme key under the hood so sections follow the active theme
// without hard-coding RGB values.
export type DashboardColor =
    | "muted"
    | "accent"
    | "value"
    | "number"
    | "ok"
    | "warn"
    | "err"
    | "branch";

const COLOR_KEYS: Record<DashboardColor, string> = {
    muted: C.muted,
    accent: C.accent,
    value: C.value,
    number: C.number,
    ok: C.ok,
    warn: C.warn,
    err: C.err,
    branch: C.branch,
};

export type DashboardTextOpts = {
    color?: DashboardColor;
    bold?: boolean;
    /** OSC-8 hyperlink target; terminals that honor it render the span as a
     *  clickable link. When only `onClick` is set (no `url`), the span is
     *  still routed through Fresh's own mouse-click dispatch. */
    url?: string;
    /** Invoked when the user clicks anywhere on the row carrying this text
     *  span, regardless of whether the terminal honors OSC-8. Multiple text
     *  spans on the same row share a single row-level click target; the
     *  first `onClick` emitted wins. */
    onClick?: () => void;
};

export type DashboardContext = {
    /** Emit a label/value row like "    label     value". The label
     *  column is padded to 10 cols so multi-row sections align. */
    kv(label: string, value: string, color?: DashboardColor): void;
    /** Emit a styled text segment on the current row. No newline is
     *  added — call `newline()` when the row is finished. */
    text(s: string, opts?: DashboardTextOpts): void;
    /** End the current row. */
    newline(): void;
    /** Shortcut for a single-row error message: "    status    why". */
    error(message: string): void;
};

export type SectionRefresh = (ctx: DashboardContext) => Promise<void>;

/**
 * Public surface of the bundled `dashboard` plugin, reachable through
 * `editor.getPluginApi("dashboard")`. Third-party plugins and user
 * init.ts can contribute their own rows via `registerSection`, and can
 * tear them down again via `removeSection` / `clearAllSections`.
 */
export type DashboardApi = {
    registerSection(name: string, refresh: SectionRefresh): () => void;
    /** Remove every registered section whose name matches `name`.
     *  Returns true if at least one section was removed. */
    removeSection(name: string): boolean;
    /** Remove every registered section, including the bundled
     *  built-ins (git, disk). */
    clearAllSections(): void;
    /** Toggle the ambient auto-open behaviour for this session.
     *  Equivalent to setting `plugins.dashboard.auto-open` in the
     *  user config, but scoped to the current process. */
    setAutoOpen(enabled: boolean): void;
    /** Refresh handlers for the built-in widgets that aren't
     *  registered by default (both hit the network). Pass one to
     *  `registerSection` from init.ts to enable it. */
    builtinHandlers: {
        weather: SectionRefresh;
        github: SectionRefresh;
    };
};

declare global {
    interface FreshPluginRegistry {
        dashboard: DashboardApi;
    }
}

type RegisteredSection = {
    id: number;
    name: string;
    refresh: SectionRefresh;
    /** Rendered output from the most recent refresh. Re-used until the
     *  next refresh lands so the dashboard doesn't flash back to a
     *  "loading…" placeholder on every tick. */
    draw: Draw;
};

// ── Internal state ─────────────────────────────────────────────────────

// State survives across open/close cycles so we don't pile up dashboards.
let dashboardBufferId: number | null = null;
let fetchToken = 0; // bumped each open; late fetches from a prior open no-op.

// Set to the current dashboardBufferId once per open; the next
// viewport_changed fire starts the bringup slide and clears the flag.
// Using viewport_changed as the trigger ensures the buffer's on-screen
// Rect is already cached by the editor before animateVirtualBuffer runs.
let bringupPending: number | null = null;
let bringupAnimationId: number | null = null;

// Registered sections, in render order. Built-ins are registered at
// plugin load (see the bottom of this file); third-party plugins
// append via the exported `registerSection` API.
let nextSectionId = 1;
const registeredSections: RegisteredSection[] = [];

// Ordered list of focusable clickable targets on the currently-painted
// frame. Rebuilt every paint() from `currentRowActions`. One entry per
// row that carries at least one action — when a row holds multiple
// ranges (e.g. PR rows with both #num and title clickable), the highlight
// spans the union and the activation dispatches the first range's action
// so Enter matches the leftmost click target on that row.
type ClickTarget = {
    bufferRow: number; // absolute buffer row (after topPad)
    colStart: number; // visual col (inclusive)
    colEnd: number; // visual col (exclusive)
    action: ClickAction;
};
let clickableTargets: ClickTarget[] = [];
let focusedIndex = 0;

// ── Drawing primitives ─────────────────────────────────────────────────

function utf8Len(s: string): number {
    return editor.utf8ByteLength(s);
}

function visualWidth(s: string): number {
    // Approximation: wide (E. Asian / most emoji) = 2 cols, everything else = 1.
    let w = 0;
    for (const ch of s) {
        const cp = ch.codePointAt(0) ?? 0;
        if (cp === 0) continue;
        if (cp < 0x80) { w += 1; continue; }
        // CJK / wide ranges (coarse).
        if (
            (cp >= 0x1100 && cp <= 0x115f) ||
            (cp >= 0x2e80 && cp <= 0x303e) ||
            (cp >= 0x3041 && cp <= 0x33ff) ||
            (cp >= 0x3400 && cp <= 0x4dbf) ||
            (cp >= 0x4e00 && cp <= 0x9fff) ||
            (cp >= 0xa000 && cp <= 0xa4cf) ||
            (cp >= 0xac00 && cp <= 0xd7a3) ||
            (cp >= 0xf900 && cp <= 0xfaff) ||
            (cp >= 0xfe30 && cp <= 0xfe4f) ||
            (cp >= 0xff00 && cp <= 0xff60) ||
            (cp >= 0xffe0 && cp <= 0xffe6) ||
            (cp >= 0x1f300 && cp <= 0x1f64f) ||
            (cp >= 0x1f900 && cp <= 0x1f9ff)
        ) { w += 2; continue; }
        w += 1;
    }
    return w;
}

function pad(s: string, width: number): string {
    const missing = Math.max(0, width - visualWidth(s));
    return s + " ".repeat(missing);
}

function emit(
    d: Draw,
    s: string,
    opts?: { fg?: string; bold?: boolean; url?: string; action?: ClickAction },
) {
    if (!s) return;
    const start = utf8Len(d.text);
    d.text += s;
    const width = visualWidth(s);
    const startCol = d.currentCol;
    d.currentCol += width;
    // Anything the user can click gets underlined so it reads as a link
    // even in terminals that don't render OSC-8 hyperlinks.
    const clickable = !!(opts?.url || opts?.action);
    if (opts?.fg || opts?.bold || opts?.url || clickable) {
        d.spans.push({
            start,
            end: start + utf8Len(s),
            fg: opts?.fg,
            bold: opts?.bold,
            underline: clickable || undefined,
            url: opts?.url,
        });
    }
    if (opts?.action) {
        // Record a column-scoped range so the click handler can match
        // clicks only on the text cells the underline actually covers,
        // not on padding or kv labels sharing the row.
        const ranges = d.rowActions.get(d.currentRow) ?? [];
        ranges.push({ colStart: startCol, colEnd: startCol + width, action: opts.action });
        d.rowActions.set(d.currentRow, ranges);
    }
}

function newline(d: Draw) {
    d.text += "\n";
    d.currentRow++;
    d.currentCol = 0;
}

function emptyDraw(): Draw {
    return { text: "", spans: [], currentRow: 0, currentCol: 0, rowActions: new Map() };
}

// ── Sections (sentinel / placeholder factories) ────────────────────────

// Produce a one-line status row: "    status    text". Used for the
// initial "loading…" placeholder and for top-level error messages
// that replace a whole section's body.
function statusRowDraw(text: string, fg: string): Draw {
    const d = emptyDraw();
    const label = pad("status", 10);
    emit(d, "    " + label, { fg: C.muted });
    emit(d, text, { fg });
    newline(d);
    return d;
}

function loadingDraw(): Draw {
    return statusRowDraw("loading…", C.muted);
}

// ── Section registry + DashboardContext factory ────────────────────────

// Build a DashboardContext that accumulates drawing operations into a
// fresh Draw. After the caller's refresh callback resolves, the Draw
// is stashed on the section entry and the dashboard repaints with it.
function makeContext(): { ctx: DashboardContext; draw: Draw } {
    const d = emptyDraw();
    const ctx: DashboardContext = {
        kv(label, value, color) {
            const fg = color ? COLOR_KEYS[color] : C.value;
            emit(d, "    " + pad(label, 10), { fg: C.muted });
            emit(d, value, { fg });
            newline(d);
        },
        text(s, opts) {
            const action: ClickAction | undefined = opts?.onClick
                ? { kind: "callback", fn: opts.onClick }
                : opts?.url
                    ? { kind: "open-url", url: opts.url }
                    : undefined;
            emit(d, s, {
                fg: opts?.color ? COLOR_KEYS[opts.color] : undefined,
                bold: opts?.bold,
                url: opts?.url,
                action,
            });
        },
        newline() {
            newline(d);
        },
        error(message) {
            const label = pad("status", 10);
            emit(d, "    " + label, { fg: C.muted });
            emit(d, message, { fg: C.err });
            newline(d);
        },
    };
    return { ctx, draw: d };
}

// Register a dashboard section. `refresh` is invoked each tick (every
// 5s while the dashboard is visible) and on every open. Returns a
// function that unregisters the section when called — e.g. for
// plugins that want to remove their section on disable.
function registerSection(name: string, refresh: SectionRefresh): () => void {
    const id = nextSectionId++;
    const entry: RegisteredSection = {
        id,
        name,
        refresh,
        draw: loadingDraw(),
    };
    registeredSections.push(entry);
    // Kick an immediate refresh so the initial frame isn't "loading…"
    // for any longer than the callback actually takes.
    void refreshSection(entry, fetchToken);
    paint();
    return () => {
        const idx = registeredSections.findIndex((s) => s.id === id);
        if (idx >= 0) {
            registeredSections.splice(idx, 1);
            paint();
        }
    };
}

// Remove every registered section whose name matches `name`. Returns
// true if at least one section was removed. Names are compared verbatim
// — no case folding or trimming — matching the name passed to
// `registerSection`. In-flight refreshes for removed sections resolve
// onto detached entry objects and no longer influence what's rendered.
function removeSection(name: string): boolean {
    let removed = false;
    for (let i = registeredSections.length - 1; i >= 0; i--) {
        if (registeredSections[i].name === name) {
            registeredSections.splice(i, 1);
            removed = true;
        }
    }
    if (removed) paint();
    return removed;
}

// Clear every registered section, including the bundled built-ins.
// The dashboard frame is still drawn — only the section body between
// header and footer is empty until something registers a new section.
function clearAllSections(): void {
    if (registeredSections.length === 0) return;
    registeredSections.length = 0;
    paint();
}

async function refreshSection(entry: RegisteredSection, myToken: number) {
    const { ctx, draw } = makeContext();
    try {
        await entry.refresh(ctx);
    } catch (e) {
        // A thrown error becomes a one-line error row so a buggy
        // third-party section can't blank the whole dashboard.
        const { ctx: fallbackCtx, draw: fallbackDraw } = makeContext();
        fallbackCtx.error(`failed — ${String(e).slice(0, 60)}`);
        if (myToken !== fetchToken) return;
        entry.draw = fallbackDraw;
        paint();
        return;
    }
    if (myToken !== fetchToken) return;
    entry.draw = draw;
    paint();
}

// ── Frame + section renderer ───────────────────────────────────────────

function clockNow(): string {
    const d = new Date();
    const hh = String(d.getHours()).padStart(2, "0");
    const mm = String(d.getMinutes()).padStart(2, "0");
    const ss = String(d.getSeconds()).padStart(2, "0");
    return `${hh}:${mm}:${ss}`;
}

function frameWidth(viewportW: number): { inner: number; leftPad: number } {
    const usable = Math.max(40, viewportW - 4);
    const inner = Math.min(MAX_INNER, usable - 2); // subtract 2 for frame edges
    const total = inner + 2;
    const leftPad = Math.max(0, Math.floor((viewportW - total) / 2));
    return { inner, leftPad };
}

function renderFrame(inner: number, leftPad: number): Draw {
    const d: Draw = emptyDraw();
    const lp = " ".repeat(leftPad);

    const titleText = "FRESH";
    const stamp = clockNow();
    const titleSegment = ` ${titleText} `;
    const stampSegment = ` ${stamp} `;
    // Top frame: ╭── FRESH ────…──── HH:MM:SS ──╮
    //
    // `inner` is the column count between the two corner glyphs. The top
    // row emits, between ╭ and ╮:
    //   "──" (2) + titleSegment (7) + dashRun (fillLen) + stampSegment (10) + "──" (2)
    // so fillLen = inner - visualWidth(titleSegment) - visualWidth(stampSegment) - 4.
    const fillLen =
        inner - visualWidth(titleSegment) - visualWidth(stampSegment) - 4;
    const dashRun = "─".repeat(Math.max(1, fillLen));

    // top
    emit(d, lp, undefined);
    emit(d, "╭──", { fg: C.frame });
    emit(d, titleSegment, { fg: C.title, bold: true });
    emit(d, dashRun, { fg: C.frame });
    emit(d, stampSegment, { fg: C.muted });
    emit(d, "──╮", { fg: C.frame });
    newline(d);

    // blank row
    emit(d, lp, undefined);
    emit(d, "│", { fg: C.frame });
    emit(d, " ".repeat(inner), undefined);
    emit(d, "│", { fg: C.frame });
    newline(d);

    const sectionHeader = (name: string) => {
        // Format: │ ▎  NAME ...
        // Dropped per-section icons: their widths (☀ ⎇ ⚡ ◆) disagree with
        // unicode-width depending on font/emoji-presentation, which
        // silently misaligned the right frame edge.
        const prefix = " ▎  ";
        emit(d, lp, undefined);
        emit(d, "│", { fg: C.frame });
        emit(d, prefix, { fg: C.accent, bold: true });
        emit(d, name, { fg: C.title, bold: true });
        const consumed = visualWidth(prefix) + visualWidth(name);
        emit(d, " ".repeat(Math.max(0, inner - consumed)), undefined);
        emit(d, "│", { fg: C.frame });
        newline(d);
    };

    const row = (
        body: { text: string; spans: Span[] },
        ranges?: ClickActionRange[],
    ) => {
        // Wraps a single logical row of section body in the frame.
        emit(d, lp, undefined);
        emit(d, "│", { fg: C.frame });
        // body is already one line (no embedded newlines) — renderSection
        // slices multi-line section output before calling row().
        const line = body.text;
        const used = visualWidth(line);
        const startInDoc = utf8Len(d.text);
        // Content starts in the outer draw at this visual column —
        // section-body ranges are offset by it below so clicks hit
        // the same cells the text actually lives in.
        const contentStartCol = d.currentCol;
        d.text += line;
        d.currentCol += used;
        for (const sp of body.spans) {
            if (sp.start < utf8Len(line)) {
                d.spans.push({
                    start: startInDoc + sp.start,
                    end: startInDoc + Math.min(sp.end, utf8Len(line)),
                    fg: sp.fg,
                    bold: sp.bold,
                    underline: sp.underline,
                    url: sp.url,
                });
            }
        }
        if (ranges && ranges.length > 0) {
            const shifted = ranges.map((r) => ({
                colStart: r.colStart + contentStartCol,
                colEnd: r.colEnd + contentStartCol,
                action: r.action,
            }));
            const existing = d.rowActions.get(d.currentRow) ?? [];
            d.rowActions.set(d.currentRow, existing.concat(shifted));
        }
        emit(d, " ".repeat(Math.max(0, inner - used)), undefined);
        emit(d, "│", { fg: C.frame });
        newline(d);
    };

    const spacerRow = () => {
        emit(d, lp, undefined);
        emit(d, "│", { fg: C.frame });
        emit(d, " ".repeat(inner), undefined);
        emit(d, "│", { fg: C.frame });
        newline(d);
    };

    const renderSection = (name: string, body: Draw) => {
        sectionHeader(name);
        const bodyLines = body.text.split("\n");
        let cursor = 0;
        for (let lineIdx = 0; lineIdx < bodyLines.length; lineIdx++) {
            const ln = bodyLines[lineIdx];
            if (ln.length === 0 && cursor + ln.length + 1 >= body.text.length) break;
            // Slice the body's spans that fall inside this line's byte range.
            const lineStart = cursor;
            const lineEnd = cursor + utf8Len(ln);
            const sliced: Span[] = body.spans
                .filter((sp) => sp.start >= lineStart && sp.end <= lineEnd + 1)
                .map((sp) => ({
                    start: sp.start - lineStart,
                    end: sp.end - lineStart,
                    fg: sp.fg,
                    bold: sp.bold,
                    underline: sp.underline,
                    url: sp.url,
                }));
            // Propagate the section-level row action (keyed by section-body
            // line index) onto the outer frame row we're about to write.
            row({ text: ln, spans: sliced }, body.rowActions.get(lineIdx));
            cursor = lineEnd + 1;
        }
        spacerRow();
    };

    for (const entry of registeredSections) {
        renderSection(entry.name.toUpperCase(), entry.draw);
    }

    // bottom
    emit(d, lp, undefined);
    emit(d, "╰" + "─".repeat(inner) + "╯", { fg: C.frame });
    newline(d);

    return d;
}

// ── Paint the buffer ───────────────────────────────────────────────────

// Convert the byte-indexed Draw model produced by renderFrame into per-line
// TextPropertyEntry[] with inlineOverlays. Spans are expected to stay within
// a single line (the renderer never emits a newline inside a styled span)
// but we clip defensively so a stray cross-line span doesn't misindex.
function drawToEntries(d: Draw): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    const lines = d.text.split("\n");
    let lineByteStart = 0;
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const isLast = i === lines.length - 1;
        if (isLast && line.length === 0) break; // trailing empty after final \n
        const lineBytes = utf8Len(line);
        const lineByteEnd = lineByteStart + lineBytes;
        const ios: InlineOverlay[] = [];
        for (const sp of d.spans) {
            if (sp.end <= lineByteStart) continue;
            if (sp.start >= lineByteEnd) continue;
            const s = Math.max(sp.start, lineByteStart) - lineByteStart;
            const e = Math.min(sp.end, lineByteEnd) - lineByteStart;
            if (e <= s) continue;
            const style: Partial<OverlayOptions> = {};
            if (sp.fg) style.fg = sp.fg;
            if (sp.bold) style.bold = true;
            if (sp.underline) style.underline = true;
            if (sp.url) style.url = sp.url;
            ios.push({ start: s, end: e, style });
        }
        entries.push({
            text: line + (isLast ? "" : "\n"),
            inlineOverlays: ios.length > 0 ? ios : undefined,
        });
        lineByteStart = lineByteEnd + 1; // account for the \n byte we split on
    }
    return entries;
}

// Track the last viewport dims we painted for, so repeat viewport_changed
// events (e.g. scroll fires one every time) don't trigger redundant paints.
let lastPaintedW = -1;
let lastPaintedH = -1;

// Row-index → click-action ranges map, keyed by absolute buffer row
// (after adding topPad). Each range is a `[colStart, colEnd)` pair so
// the click handler can gate on buffer_col too. Rebuilt every paint.
let currentRowActions: Map<number, ClickActionRange[]> = new Map();

// Map a visual column (counted from the start of the line) to a UTF-8
// byte offset inside `text`. Used to translate the visual col ranges
// stored in ClickTarget into the byte-offset range an InlineOverlay
// needs. Walks chars with visualWidth/utf8Len so frame glyphs like
// `│` (3 bytes, 1 col) and future wide chars stay aligned.
function visualColToByteOffset(text: string, visualCol: number): number {
    if (visualCol <= 0) return 0;
    let col = 0;
    let bytes = 0;
    for (const ch of text) {
        if (col >= visualCol) return bytes;
        col += visualWidth(ch);
        bytes += utf8Len(ch);
    }
    return bytes;
}

function paint(dims?: { width: number; height: number }) {
    if (dashboardBufferId === null) return;
    const bufferId = dashboardBufferId;
    // Prefer explicit dims (from a viewport_changed event, which ships
    // the just-resized width/height before the state snapshot catches
    // up) and fall back to the snapshot. Without this, toggling the
    // file explorer repaints against the stale pre-toggle width, so
    // the frame stays anchored at the old position for one tick.
    const vp = dims ?? editor.getViewport();
    const width = vp?.width ?? 100;
    const height = vp?.height ?? 24;
    const { inner, leftPad } = frameWidth(width);
    const drawn = renderFrame(inner, leftPad);

    // Count newlines in the rendered frame to vertically center it. Pad
    // above with blank lines; there's no need to pad below since the
    // virtual buffer's empty trailing rows already render as blank.
    let frameHeight = 0;
    for (let i = 0; i < drawn.text.length; i++) {
        if (drawn.text.charCodeAt(i) === 10) frameHeight++;
    }
    const topPad = Math.max(0, Math.floor((height - frameHeight) / 2));

    const entries: TextPropertyEntry[] = [];
    for (let i = 0; i < topPad; i++) entries.push({ text: "\n" });
    for (const e of drawToEntries(drawn)) entries.push(e);

    // Translate frame-relative row actions to absolute buffer rows by
    // shifting by the vertical padding we just prepended. Columns are
    // absolute already (the frame renderer placed them via currentCol).
    const abs: Map<number, ClickActionRange[]> = new Map();
    for (const [row, ranges] of drawn.rowActions) {
        abs.set(row + topPad, ranges);
    }
    currentRowActions = abs;

    // Rebuild the ordered focus targets in visual row order. A row with
    // multiple ranges collapses into a single target whose highlight
    // spans the union of its ranges; Enter dispatches the first range's
    // action, matching the leftmost click target on that row.
    const targets: ClickTarget[] = [];
    const sortedRows = [...abs.keys()].sort((a, b) => a - b);
    for (const row of sortedRows) {
        const ranges = abs.get(row)!;
        if (ranges.length === 0) continue;
        let minCol = ranges[0].colStart;
        let maxCol = ranges[0].colEnd;
        for (const r of ranges) {
            if (r.colStart < minCol) minCol = r.colStart;
            if (r.colEnd > maxCol) maxCol = r.colEnd;
        }
        targets.push({
            bufferRow: row,
            colStart: minCol,
            colEnd: maxCol,
            action: ranges[0].action,
        });
    }
    clickableTargets = targets;
    if (targets.length === 0) {
        focusedIndex = 0;
    } else if (focusedIndex < 0 || focusedIndex >= targets.length) {
        focusedIndex =
            ((focusedIndex % targets.length) + targets.length) % targets.length;
    }

    // Paint the focus highlight by mutating the entry for the focused
    // row: translate its visual col range into a byte range and push an
    // inline overlay on top of whatever foreground/underline spans the
    // frame renderer already added. Using `editor.selection_bg` keeps
    // the highlight theme-aware — it follows theme switches for free
    // and matches other selection-style highlights elsewhere in the UI.
    if (targets.length > 0) {
        const focus = targets[focusedIndex];
        const entry = entries[focus.bufferRow];
        if (entry) {
            const lineText = entry.text.endsWith("\n")
                ? entry.text.slice(0, -1)
                : entry.text;
            const byteStart = visualColToByteOffset(lineText, focus.colStart);
            const byteEnd = visualColToByteOffset(lineText, focus.colEnd);
            if (byteEnd > byteStart) {
                const overlays: InlineOverlay[] = entry.inlineOverlays
                    ? [...entry.inlineOverlays]
                    : [];
                overlays.push({
                    start: byteStart,
                    end: byteEnd,
                    style: { bg: "editor.selection_bg" },
                });
                entry.inlineOverlays = overlays;
            }
        }
    }

    editor.setVirtualBufferContent(bufferId, entries);
    lastPaintedW = width;
    lastPaintedH = height;
}

// Open a URL in the user's browser via the platform's "open" helper.
// Fires both xdg-open (Linux) and open (macOS) — only one exists per
// platform; the other exits immediately with ENOENT and causes no
// user-visible effect. Fire-and-forget: we don't await.
function openUrl(url: string) {
    // spawnProcess returns a ProcessHandle; we intentionally discard it.
    // The process runs to completion on its own; failures are silent.
    editor.spawnProcess("xdg-open", [url]);
    editor.spawnProcess("open", [url]);
}

function dispatchClickAction(action: ClickAction) {
    switch (action.kind) {
        case "open-url":
            openUrl(action.url);
            return;
        case "callback":
            try {
                action.fn();
            } catch (e) {
                editor.debug(`dashboard click handler threw: ${String(e)}`);
            }
            return;
    }
}

// ── Data fetchers ──────────────────────────────────────────────────────

async function run(
    cmd: string,
    args: string[],
    cwd: string,
    timeoutMs: number,
): Promise<{ stdout: string; stderr: string; ok: boolean }> {
    const handle = editor.spawnProcess(cmd, args, cwd);
    const timeout = editor.delay(timeoutMs).then(() => "__timeout__");
    const res = await Promise.race([(async () => await handle)(), timeout]);
    if (res === "__timeout__") {
        await handle.kill();
        return { stdout: "", stderr: "timed out", ok: false };
    }
    const r = res as SpawnResult;
    return { stdout: r.stdout ?? "", stderr: r.stderr ?? "", ok: r.exit_code === 0 };
}

const trim = (s: string) => s.replace(/\s+$/, "");

// Truncate to at most `maxCols` visual columns. Adds an ellipsis when
// the string is shortened. Uses the same visualWidth estimator as the
// frame renderer so the result fits exactly.
function truncate(s: string, maxCols: number): string {
    if (visualWidth(s) <= maxCols) return s;
    let out = "";
    let w = 0;
    for (const ch of s) {
        const cw = visualWidth(ch);
        if (w + cw > Math.max(0, maxCols - 1)) break;
        out += ch;
        w += cw;
    }
    return out + "…";
}

// Max room for a `kv` value cell inside a standard row. The `    ` + 10-
// col padded key consume 14 cols, so the value must fit in inner - 14.
// With MAX_INNER = 72, that's 58 cols in the default case.
const VALUE_MAX = MAX_INNER - 14;

function bar(pct: number, width: number): string {
    const filled = Math.max(0, Math.min(width, Math.round((pct / 100) * width)));
    return "━".repeat(filled) + "╌".repeat(width - filled);
}

// wttr.in's j1 response shape — only the fields we consume.
type WttrHour = {
    time?: string; // "0", "300", …, "2100"
    tempC?: string;
    FeelsLikeC?: string;
    windspeedKmph?: string;
    humidity?: string;
    weatherDesc?: { value?: string }[];
};
type WttrDay = {
    date?: string;
    maxtempC?: string;
    mintempC?: string;
    hourly?: WttrHour[];
};
type WttrCurrent = {
    temp_C?: string;
    FeelsLikeC?: string;
    windspeedKmph?: string;
    humidity?: string;
    weatherDesc?: { value?: string }[];
};
type WttrJ1 = {
    current_condition?: WttrCurrent[];
    weather?: WttrDay[];
};

// Find the hourly entry whose `time` is closest to `targetHour` (0–23).
// wttr.in returns 3-hour samples encoded as "0", "300", "600", …, so a
// requested 17 (5pm) snaps to the 1800 sample. We round to the nearest
// (rather than floor) to avoid showing "morning" weather for 17:00 just
// because 1500 is numerically smaller.
function hourlyAt(hours: WttrHour[], targetHour: number): WttrHour | null {
    let best: WttrHour | null = null;
    let bestDelta = Infinity;
    for (const h of hours) {
        const raw = h.time ?? "";
        const hh = Math.round((Number(raw) || 0) / 100);
        const delta = Math.abs(hh - targetHour);
        if (delta < bestDelta) {
            best = h;
            bestDelta = delta;
        }
    }
    return best;
}

function formatCurrent(c: WttrCurrent | undefined): string | null {
    if (!c) return null;
    const cond = c.weatherDesc?.[0]?.value ?? "";
    const temp = c.temp_C ? `${c.temp_C}°C` : "";
    const feels = c.FeelsLikeC && c.FeelsLikeC !== c.temp_C
        ? `feels ${c.FeelsLikeC}°C` : "";
    const wind = c.windspeedKmph ? `${c.windspeedKmph} km/h` : "";
    const hum = c.humidity ? `${c.humidity}%` : "";
    const s = [cond, temp, feels, wind, hum]
        .filter((x) => x.length > 0)
        .join(" · ");
    return s ? truncate(s, VALUE_MAX) : null;
}

function formatHour(h: WttrHour | null): string | null {
    if (!h) return null;
    const cond = h.weatherDesc?.[0]?.value ?? "";
    const temp = h.tempC ? `${h.tempC}°C` : "";
    const feels = h.FeelsLikeC && h.FeelsLikeC !== h.tempC
        ? `feels ${h.FeelsLikeC}°C` : "";
    const s = [cond, temp, feels]
        .filter((x) => x.length > 0)
        .join(" · ");
    return s ? truncate(s, VALUE_MAX) : null;
}

function formatDaySummary(day: WttrDay | undefined): string | null {
    if (!day) return null;
    const midday = hourlyAt(day.hourly ?? [], 12);
    const cond = midday?.weatherDesc?.[0]?.value ?? "";
    const range = day.mintempC && day.maxtempC
        ? `${day.mintempC}°..${day.maxtempC}°C`
        : "";
    const s = [range, cond].filter((x) => x.length > 0).join(" · ");
    return s ? truncate(s, VALUE_MAX) : null;
}

const weatherRefresh: SectionRefresh = async (ctx) => {
    let stdout: string;
    let ok: boolean;
    try {
        // j1 = full JSON payload (current + 3-day forecast, 3-hour samples).
        // Larger than the old %-format but gets us everything in one call.
        const res = await run(
            "curl",
            ["-fsS", "--max-time", "5", "https://wttr.in/?format=j1"],
            "",
            6000,
        );
        stdout = res.stdout;
        ok = res.ok;
    } catch {
        ctx.error("fetch failed");
        return;
    }
    if (!ok || !stdout.trim()) {
        ctx.error("offline");
        return;
    }
    let parsed: WttrJ1;
    try {
        parsed = JSON.parse(stdout) as WttrJ1;
    } catch {
        ctx.error("malformed response");
        return;
    }
    const now = formatCurrent(parsed.current_condition?.[0]);
    // "5pm": nearest 3-hour sample in today's forecast. wttr.in
    // buckets at 0/3/6/…/21, so 17:00 picks the 18:00 bucket.
    const todayHours = parsed.weather?.[0]?.hourly ?? [];
    const evening = formatHour(hourlyAt(todayHours, 17));
    const tomorrow = formatDaySummary(parsed.weather?.[1]);
    ctx.kv("now", now ?? "–", now ? "accent" : "muted");
    if (evening) ctx.kv("5pm", evening, "value");
    if (tomorrow) ctx.kv("tomorrow", tomorrow, "value");
};

function normalizeRepoUrl(raw: string): string | null {
    const s = trim(raw);
    if (!s) return null;
    // git@github.com:owner/repo(.git)? -> https://github.com/owner/repo
    const sshMatch = s.match(/^git@([^:]+):(.+?)(\.git)?$/);
    if (sshMatch) return `https://${sshMatch[1]}/${sshMatch[2]}`;
    // https://github.com/owner/repo(.git)? -> stripped
    const httpsMatch = s.match(/^(https?:\/\/[^/]+\/.+?)(\.git)?$/);
    if (httpsMatch) return httpsMatch[1];
    return s;
}

// Extract "owner/repo" (GitHub "nwo" — name-with-owner) from a git
// remote URL. Returns null for non-GitHub hosts or unparseable URLs so
// fetchGithub can surface a clear reason instead of firing off a query
// against the wrong repo.
function parseGithubNwo(raw: string): string | null {
    const s = trim(raw);
    if (!s) return null;
    // git@github.com:owner/repo(.git)?
    const ssh = s.match(/^git@github\.com:([^/]+)\/([^/]+?)(\.git)?$/i);
    if (ssh) return `${ssh[1]}/${ssh[2]}`;
    // https://github.com/owner/repo(.git)? (allow optional user:token@ prefix)
    const https = s.match(
        /^https?:\/\/(?:[^@/]*@)?github\.com\/([^/]+)\/([^/]+?)(\.git)?\/?$/i,
    );
    if (https) return `${https[1]}/${https[2]}`;
    return null;
}

// Parse the two numbers produced by `git rev-list --left-right --count`
// ("<ahead> <behind>"). Returns null on malformed output.
function parseLeftRight(stdout: string): { ahead: number; behind: number } | null {
    const parts = trim(stdout).split(/\s+/);
    const a = Number(parts[0]);
    const b = Number(parts[1]);
    if (isNaN(a) || isNaN(b)) return null;
    return { ahead: a, behind: b };
}

const gitRefresh: SectionRefresh = async (ctx) => {
    const cwd = editor.getCwd();
    let branch;
    let status;
    let ahead;
    let remote;
    try {
        [branch, status, ahead, remote] = await Promise.all([
            run("git", ["rev-parse", "--abbrev-ref", "HEAD"], cwd, 3000),
            run("git", ["status", "--porcelain"], cwd, 3000),
            run("git", ["rev-list", "--left-right", "--count", "HEAD...@{u}"], cwd, 3000),
            run("git", ["remote", "get-url", "origin"], cwd, 3000),
        ]);
    } catch {
        ctx.error("git failed");
        return;
    }
    if (!branch.ok) {
        ctx.error("not a git repo");
        return;
    }
    const modified = status.stdout
        .split("\n")
        .filter((l) => l.trim().length > 0).length;
    let trackStr = "no upstream";
    let trackColor: DashboardColor = "muted";
    if (ahead.ok) {
        const ab = parseLeftRight(ahead.stdout);
        if (ab) {
            trackStr = `↑ ${ab.ahead}   ↓ ${ab.behind}`;
            trackColor = ab.ahead > 0 || ab.behind > 0 ? "accent" : "ok";
        }
    }
    const repoUrl = remote.ok ? normalizeRepoUrl(remote.stdout) : null;
    const branchName = trim(branch.stdout);

    // "vs master" row: commits ahead/behind of master, or main as a
    // fallback for repos that use it as the default branch. Skipped
    // when the current branch IS master/main (self-comparison is 0/0
    // and not interesting), or when neither ref exists.
    let vsBase: { base: string; ahead: number; behind: number } | null = null;
    if (branchName !== "master" && branchName !== "main") {
        for (const base of ["origin/master", "origin/main", "master", "main"]) {
            const r = await run(
                "git",
                ["rev-list", "--left-right", "--count", `HEAD...${base}`],
                cwd,
                3000,
            );
            if (r.ok) {
                const ab = parseLeftRight(r.stdout);
                if (ab) {
                    vsBase = { base: base.replace(/^origin\//, ""), ...ab };
                    break;
                }
            }
        }
    }

    // branch — whole row routes clicks to the branch page.
    const branchBranchUrl = repoUrl
        ? `${repoUrl}/tree/${encodeURIComponent(branchName)}`
        : undefined;
    ctx.text("    " + pad("branch", 10), { color: "muted" });
    ctx.text(branchName, {
        color: "branch",
        url: branchBranchUrl,
        onClick: branchBranchUrl
            ? () => openUrl(branchBranchUrl)
            : undefined,
    });
    ctx.newline();

    // remote URL — displayed in full with scheme so that terminals
    // that auto-detect URLs (but ignore OSC-8) still recognize it.
    // The whole row is also click-routable via the mouse_click hook.
    if (repoUrl) {
        ctx.text("    " + pad("repo", 10), { color: "muted" });
        ctx.text(repoUrl, {
            color: "accent",
            url: repoUrl,
            onClick: () => openUrl(repoUrl),
        });
        ctx.newline();
    }

    ctx.kv("tracking", trackStr, trackColor);
    if (vsBase) {
        const label = `vs ${vsBase.base}`;
        const str = `↑ ${vsBase.ahead}   ↓ ${vsBase.behind}`;
        const color: DashboardColor =
            vsBase.ahead > 0 || vsBase.behind > 0 ? "accent" : "ok";
        ctx.kv(label, str, color);
    }
    ctx.kv(
        "changes",
        `${modified} file${modified === 1 ? "" : "s"}`,
        modified > 0 ? "warn" : "muted",
    );

    // Clickable "review branch" action. Triggers the audit_mode
    // plugin's `start_review_branch` handler via the plugin-action
    // bridge — executeAction falls through to Action::PluginAction
    // for any name that's not a built-in, and the plugin manager
    // dispatches that to the registered handler by name.
    ctx.text("    " + pad("review", 10), { color: "muted" });
    ctx.text("▶ review branch", {
        color: "accent",
        bold: true,
        onClick: () => editor.executeAction("start_review_branch"),
    });
    ctx.newline();
};

// PR row types — module-level so the last-good state can reference them.
type GhRollup = { state?: string } | null;
type GhCommit = { statusCheckRollup?: GhRollup };
type GhCommitNode = { commit?: GhCommit };
type GhThread = { isResolved?: boolean; comments?: { totalCount?: number } };
type GhPR = {
    number?: number;
    title?: string;
    state?: string;
    repository?: { nameWithOwner?: string };
    commits?: { nodes?: GhCommitNode[] };
    reviewThreads?: { nodes?: GhThread[] };
};

// Last-known-good GitHub state, preserved across refresh failures so
// the panel doesn't jump between "data" and "error". `prs === null`
// means we've never successfully fetched — in that case an error
// replaces the section wholesale. Once we have PRs, a later failure
// only adds a one-line banner at the top.
let githubLastPrs: GhPR[] | null = null;
let githubLastError: string | null = null;

// Column widths for PR rows. `num` covers `#` + up to 7 digits so PR
// numbers in large repos (e.g. the node/k8s ranges) don't overflow and
// push the state column out of alignment.
const PR_COL_NUM = 8;
const PR_COL_STATE = 5;
const PR_COL_CHECK = 2;
const PR_COL_CMTS = 6;

function renderPrRows(ctx: DashboardContext, prs: GhPR[]) {
    if (prs.length === 0) {
        ctx.kv("PRs", "no open PRs", "muted");
        return;
    }
    ctx.kv("PRs", `${prs.length} open`, "number");
    for (const pr of prs) {
        const state = (pr.state ?? "").toUpperCase();
        const stateTag =
            state === "OPEN"
                ? "open"
                : state === "MERGED"
                    ? "mrgd"
                    : state === "CLOSED"
                        ? "clsd"
                        : "???";
        const stateColor: DashboardColor =
            state === "OPEN"
                ? "ok"
                : state === "MERGED"
                    ? "accent"
                    : "muted";

        const rollup = pr.commits?.nodes?.[0]?.commit?.statusCheckRollup?.state ?? null;
        const checkGlyph =
            rollup === "SUCCESS"
                ? "✓"
                : rollup === "FAILURE" || rollup === "ERROR"
                    ? "✗"
                    : rollup === "PENDING" || rollup === "EXPECTED"
                        ? "◌"
                        : "–";
        const checkColor: DashboardColor =
            rollup === "SUCCESS"
                ? "ok"
                : rollup === "FAILURE" || rollup === "ERROR"
                    ? "err"
                    : rollup === "PENDING" || rollup === "EXPECTED"
                        ? "warn"
                        : "muted";

        const threads = pr.reviewThreads?.nodes ?? [];
        const openCmts = threads
            .filter((t) => t.isResolved === false)
            .reduce((acc, t) => acc + (t.comments?.totalCount ?? 0), 0);

        const num = `#${pr.number ?? "?"}`;
        const title = (pr.title ?? "").slice(0, 44);
        const repoName = pr.repository?.nameWithOwner ?? "";
        const prUrl =
            repoName && pr.number
                ? `https://github.com/${repoName}/pull/${pr.number}`
                : undefined;

        // Whole PR row routes clicks to prUrl (set once on the first
        // action-bearing emit — subsequent emits on the same row would
        // overwrite with the same value). Emit the number text and
        // its padding in two separate spans so the underline (added
        // to clickable spans by `emit`) lands on the "#1234" text
        // only — trailing padding spaces stay plain.
        const onClickPr = prUrl ? () => openUrl(prUrl) : undefined;
        const numPad = " ".repeat(
            Math.max(0, PR_COL_NUM - visualWidth(num)),
        );
        ctx.text("    ");
        ctx.text(num, {
            color: "number",
            url: prUrl,
            onClick: onClickPr,
        });
        if (numPad) ctx.text(numPad);
        ctx.text(pad(stateTag, PR_COL_STATE), {
            color: stateColor,
            bold: true,
        });
        ctx.text(" ");
        ctx.text(pad(checkGlyph, PR_COL_CHECK), {
            color: checkColor,
            bold: true,
        });
        const cmtCell =
            openCmts > 0
                ? pad(`${openCmts} cmt`, PR_COL_CMTS)
                : pad("", PR_COL_CMTS);
        ctx.text(cmtCell, { color: openCmts > 0 ? "warn" : "muted" });
        ctx.text(" ");
        ctx.text(title, { color: "value", url: prUrl });
        ctx.newline();
    }
}

function drawGithubState(ctx: DashboardContext) {
    // Stale-data banner: when we have previously-good PRs AND the
    // latest refresh failed, show both. Keeps the rest of the
    // section anchored — no row-count jumps between ticks.
    if (githubLastError && githubLastPrs !== null) {
        ctx.text("    " + pad("update", 10), { color: "muted" });
        ctx.text(`failed — ${githubLastError}`, { color: "err" });
        ctx.newline();
        renderPrRows(ctx, githubLastPrs);
        return;
    }
    if (githubLastPrs !== null) {
        renderPrRows(ctx, githubLastPrs);
        return;
    }
    if (githubLastError) {
        ctx.error(githubLastError);
        return;
    }
    ctx.kv("status", "loading…", "muted");
}

// Detect the GitHub owner/repo for the working directory. Returns
// either the nwo ("owner/repo") or a human-readable reason we couldn't
// determine one — fetchGithub renders that reason in the UI instead of
// fetching PRs against the wrong repo.
async function detectGithubNwo(
    cwd: string,
): Promise<{ nwo: string } | { err: string }> {
    const inside = await run(
        "git",
        ["rev-parse", "--is-inside-work-tree"],
        cwd,
        3000,
    );
    if (!inside.ok) return { err: "not a git repo" };
    const remote = await run("git", ["remote", "get-url", "origin"], cwd, 3000);
    if (!remote.ok) return { err: "no git remote" };
    const nwo = parseGithubNwo(remote.stdout);
    if (!nwo) return { err: "not a github repo" };
    return { nwo };
}

const githubRefresh: SectionRefresh = async (ctx) => {
    const cwd = editor.getCwd();
    const detected = await detectGithubNwo(cwd);
    if ("err" in detected) {
        githubLastPrs = null;
        githubLastError = detected.err;
        drawGithubState(ctx);
        return;
    }
    const nwo = detected.nwo;
    // Recent PRs in THIS repo. One GraphQL round-trip fetches state
    // (OPEN / MERGED / CLOSED), combined check status from the tip
    // commit's rollup, and the list of review threads so we can count
    // *unresolved* comment threads per PR. `$owner`/`$name` are passed
    // as variables so the nwo is not interpolated into the query
    // string.
    const [owner, name] = nwo.split("/");
    const query = `
        query($owner: String!, $name: String!) {
            repository(owner: $owner, name: $name) {
                pullRequests(first: 6, states: [OPEN], orderBy: {field: UPDATED_AT, direction: DESC}) {
                    nodes {
                        number
                        title
                        state
                        repository { nameWithOwner }
                        commits(last: 1) {
                            nodes {
                                commit {
                                    statusCheckRollup { state }
                                }
                            }
                        }
                        reviewThreads(first: 50) {
                            nodes {
                                isResolved
                                comments { totalCount }
                            }
                        }
                    }
                }
            }
        }
    `;
    let failure: string | null = null;
    try {
        const res = await run(
            "gh",
            [
                "api",
                "graphql",
                "-f",
                `query=${query}`,
                "-F",
                `owner=${owner}`,
                "-F",
                `name=${name}`,
            ],
            "",
            7000,
        );
        if (!res.ok) {
            const stderr = res.stderr.toLowerCase();
            failure =
                stderr.includes("not found") || stderr.includes("no such file")
                    ? "gh not installed"
                    : stderr.includes("auth")
                        ? "gh not authenticated"
                        : trim(res.stderr).split("\n")[0]?.slice(0, 40) || "gh failed";
        } else {
            try {
                const parsed = JSON.parse(res.stdout);
                const prs: GhPR[] =
                    (
                        parsed as {
                            data?: {
                                repository?: { pullRequests?: { nodes?: GhPR[] } };
                            };
                        }
                    )?.data?.repository?.pullRequests?.nodes ?? [];
                githubLastPrs = prs;
                githubLastError = null;
            } catch {
                failure = "malformed response";
            }
        }
    } catch {
        failure = "gh failed";
    }
    if (failure !== null) githubLastError = failure;
    drawGithubState(ctx);
};

const diskRefresh: SectionRefresh = async (ctx) => {
    const mounts = ["/", editor.getEnv("HOME") ?? "/home"];
    const seen = new Set<string>();
    const rows: { mount: string; pct: number; used: string; size: string }[] = [];
    try {
        for (const m of mounts) {
            const { stdout, ok } = await run("df", ["-hP", m], "", 3000);
            if (!ok) continue;
            const lns = stdout.split("\n").filter((l) => l.length > 0);
            if (lns.length < 2) continue;
            const cols = lns[1].split(/\s+/);
            if (cols.length < 6) continue;
            const mount = cols[5];
            if (seen.has(mount)) continue;
            seen.add(mount);
            rows.push({
                mount,
                pct: Number(cols[4].replace("%", "")) || 0,
                used: cols[2],
                size: cols[1],
            });
        }
    } catch {
        ctx.error("df failed");
        return;
    }
    if (rows.length === 0) {
        ctx.error("df failed");
        return;
    }
    for (const row of rows) {
        const color: DashboardColor =
            row.pct >= 90 ? "err" : row.pct >= 75 ? "warn" : "ok";
        ctx.text("    " + pad(row.mount, 10), { color: "muted" });
        ctx.text(bar(row.pct, 18), { color, bold: true });
        ctx.text("  " + String(row.pct).padStart(3) + "%", { color });
        ctx.text(`   ${row.used} / ${row.size}`, { color: "muted" });
        ctx.newline();
    }
};

// ── Lifecycle ──────────────────────────────────────────────────────────

// Fire-and-forget: refresh every 5s while the dashboard remains the
// active dashboard. Each tick bumps `fetchToken` and re-kicks every
// registered section's refresh callback; in-flight refreshes from a
// previous tick become no-ops the moment their token stops matching.
// Loop exits when the dashboard buffer is closed (dashboardBufferId
// becomes null).
async function refreshLoop(myBufferId: number) {
    while (dashboardBufferId === myBufferId) {
        await editor.delay(5000);
        if (dashboardBufferId !== myBufferId) return;
        paint(); // refresh clock even if fetches lag
        fetchToken++;
        const tok = fetchToken;
        for (const entry of registeredSections) {
            void refreshSection(entry, tok);
        }
    }
}

// Set for the duration of an in-flight openDashboard call. The
// createVirtualBuffer round-trip is async, so a second openDashboard
// invocation (e.g. from the `ready` hook firing while enable()'s own
// call is still awaiting) would otherwise see dashboardBufferId === null
// and create a second Dashboard tab.
let dashboardOpening = false;

// Kick section refreshes and start the periodic refresh loop. Used
// both by the ambient auto-open path and by the command-palette
// "Show Dashboard" handler — the parts that differ (whether to bail
// on real files, whether to sweep untitled scratch) live in the
// callers.
function bootstrapDashboard(bufferId: number) {
    // Reset section draws to "loading…" and kick a fresh refresh for
    // each registered section. Token guards against late resolvers
    // from a prior open clobbering the new one.
    //
    // GitHub's section callback reuses the last-good PR snapshot (if
    // any) on its first call post-open so a re-opened dashboard can
    // draw real data on the first frame while the refresh round-trip
    // is still in flight. Refresh failures surface via the in-panel
    // stale-data banner.
    fetchToken++;
    const myToken = fetchToken;
    for (const entry of registeredSections) {
        entry.draw = loadingDraw();
        void refreshSection(entry, myToken);
    }
    paint();

    // Kick off the 5-second refresh loop. It stops itself when the
    // dashboard is closed.
    refreshLoop(bufferId);
}

async function openDashboard() {
    if (dashboardBufferId !== null) return; // already open
    if (dashboardOpening) return; // another openDashboard is mid-await
    dashboardOpening = true;

    const res = await editor.createVirtualBuffer({
        name: "Dashboard",
        mode: "dashboard",
        readOnly: true,
        showLineNumbers: false,
        showCursors: false,
        editingDisabled: true,
    });
    dashboardBufferId = res.bufferId;
    dashboardOpening = false;
    focusedIndex = 0;

    // Re-check: while we were awaiting createVirtualBuffer, a real
    // file may have landed — e.g. a CLI file from `fresh my_file`
    // that was queued before our `ready` handler ran, or a file the
    // user opened from the explorer. If so, quietly close the buffer
    // we just created instead of showing it and stealing focus.
    const realFilesNow = editor.listBuffers().filter(
        (b) =>
            !b.is_virtual &&
            b.path &&
            b.path.length > 0 &&
            b.id !== dashboardBufferId,
    );
    if (realFilesNow.length > 0) {
        editor.closeBuffer(dashboardBufferId);
        dashboardBufferId = null;
        return;
    }
    editor.showBuffer(dashboardBufferId);

    // Close any untitled scratch left over from the last-tab-closed event
    // or the initial launch — the dashboard should own the split.
    for (const b of editor.listBuffers()) {
        if (
            !b.is_virtual &&
            (!b.path || b.path.length === 0) &&
            b.id !== dashboardBufferId
        ) {
            editor.closeBuffer(b.id);
        }
    }

    // Bringup animation: slide the whole dashboard up from the bottom.
    // Runs once per open. The first render after showBuffer is what
    // populates the virtual buffer's screen rect, so defer the start
    // by one frame to give the layout cache a chance to update.
    bringupPending = dashboardBufferId;

    bootstrapDashboard(dashboardBufferId);
}

// Command-palette handler: show the dashboard if it isn't open, or
// bring it to the front of the current split if it is. Unlike the
// ambient open path, this never closes real files or untitled scratch
// — if the user has a file open and types "Show Dashboard", the
// dashboard opens alongside it rather than replacing it.
async function dashboardShowOrFocus() {
    if (dashboardBufferId !== null) {
        editor.showBuffer(dashboardBufferId);
        return;
    }
    if (dashboardOpening) return;
    dashboardOpening = true;
    const res = await editor.createVirtualBuffer({
        name: "Dashboard",
        mode: "dashboard",
        readOnly: true,
        showLineNumbers: false,
        showCursors: false,
        editingDisabled: true,
    });
    dashboardBufferId = res.bufferId;
    dashboardOpening = false;
    focusedIndex = 0;
    editor.showBuffer(dashboardBufferId);
    bootstrapDashboard(dashboardBufferId);
}
registerHandler("dashboardShowOrFocus", dashboardShowOrFocus);

// Auto-open resolution: the session override (set via the exported
// plugin API from init.ts) wins over the user config. We read from
// getUserConfig (raw file) rather than getConfig because unknown
// fields are dropped when the Config struct reserializes. Default
// is true.
let autoOpenOverride: boolean | null = null;

function autoOpenEnabled(): boolean {
    if (autoOpenOverride !== null) return autoOpenOverride;
    const cfg = editor.getUserConfig() as Record<string, unknown> | null;
    const plugins = cfg?.plugins as Record<string, unknown> | undefined;
    const dashboard = plugins?.dashboard as Record<string, unknown> | undefined;
    return dashboard?.["auto-open"] !== false;
}

function shouldShowDashboard(): boolean {
    if (dashboardBufferId !== null) return false;
    if (!autoOpenEnabled()) return false;
    const all = editor.listBuffers();
    const realFiles = all.filter(
        (b) => !b.is_virtual && b.path && b.path.length > 0,
    );
    return realFiles.length === 0;
}

// ── Editor event handlers ─────────────────────────────────────────────

// Named handlers are registered up-front — the plugin runtime requires
// handlers to exist before `editor.on(...)` subscribes to them. The
// subscription itself happens at the bottom of the file, once all the
// handlers below exist.
registerHandler("dashboardOnReady", async () => {
    if (shouldShowDashboard()) await openDashboard();
});
registerHandler(
    "dashboardOnBufferClosed",
    async (e: { buffer_id: number }) => {
        // If the dashboard itself was closed, clear our handle so we'll
        // re-open on the next "last tab closed" event.
        if (dashboardBufferId !== null && e.buffer_id === dashboardBufferId) {
            if (bringupAnimationId !== null) {
                editor.cancelAnimation(bringupAnimationId);
                bringupAnimationId = null;
            }
            bringupPending = null;
            dashboardBufferId = null;
            return;
        }
        if (shouldShowDashboard()) await openDashboard();
    },
);
// Step aside when a real file opens. This covers two flows:
//   1. `fresh my_file` at the command line: the file is queued before
//      the event loop starts, so the dashboard's `ready` handler can
//      race ahead and open on top of the pending file. When the file
//      eventually lands here, we close the dashboard so the user's
//      file is the visible tab.
//   2. Opening a file from the explorer / command palette while the
//      dashboard is the current tab. Same reasoning: the user asked
//      for a real buffer, so the dashboard shouldn't linger in front.
registerHandler(
    "dashboardOnAfterFileOpen",
    (_e: { buffer_id: number; path: string }) => {
        if (dashboardBufferId === null) return;
        if (bringupAnimationId !== null) {
            editor.cancelAnimation(bringupAnimationId);
            bringupAnimationId = null;
        }
        bringupPending = null;
        editor.closeBuffer(dashboardBufferId);
        dashboardBufferId = null;
    },
);
// viewport_changed fires whenever a split's dimensions change, which
// covers terminal resize *and* file-explorer toggle (opening the explorer
// shrinks the dashboard split's width; closing it grows it back). We
// dedupe against the last-painted dims so scroll-only events (which also
// fire this hook) don't cause gratuitous repaints.
registerHandler(
    "dashboardOnViewportChanged",
    (data: { buffer_id: number; width: number; height: number }) => {
        if (dashboardBufferId === null) return;
        if (data.buffer_id !== dashboardBufferId) return;
        // First viewport_changed after open: trigger the slide-in over
        // the dashboard's now-known Rect and forget the pending flag so
        // subsequent resize events don't re-animate.
        if (bringupPending !== null && bringupPending === dashboardBufferId) {
            bringupPending = null;
            bringupAnimationId = editor.animateVirtualBuffer(dashboardBufferId, {
                kind: "slideIn",
                from: "bottom",
                durationMs: 520,
                delayMs: 0,
            });
        }
        if (data.width === lastPaintedW && data.height === lastPaintedH) return;
        // Pass the fresh dims through so we center against the new
        // split width on this very tick — the getViewport() snapshot
        // is only updated on the next render pass.
        paint({ width: data.width, height: data.height });
    },
);
// Keyboard navigation. The dashboard buffer is `showCursors: false` +
// `editingDisabled: true`, so there's no native cursor to drive
// selection — we track focus ourselves via `focusedIndex` and repaint
// to move the highlight. Wraparound in both directions so the user
// can't walk off either end of the clickable list.
function moveFocus(delta: number) {
    if (clickableTargets.length === 0) return;
    focusedIndex =
        (focusedIndex + delta + clickableTargets.length) %
        clickableTargets.length;
    paint();
}
registerHandler("dashboardFocusNext", () => moveFocus(1));
registerHandler("dashboardFocusPrev", () => moveFocus(-1));
registerHandler("dashboardActivate", () => {
    if (clickableTargets.length === 0) return;
    const target = clickableTargets[focusedIndex];
    if (!target) return;
    dispatchClickAction(target.action);
});

// Mode bindings mirror the standard "list with selectable rows"
// idiom: Tab / Down / j step forward, BackTab / Up / k step back,
// Return activates. `inheritNormalBindings: false` because every
// useful key on a read-only, no-cursor buffer is either bound above
// or intentionally inert (we don't want j/k falling through to cursor
// movement commands that would silently do nothing here).
editor.defineMode(
    "dashboard",
    [
        ["Tab", "dashboardFocusNext"],
        ["Down", "dashboardFocusNext"],
        ["j", "dashboardFocusNext"],
        ["BackTab", "dashboardFocusPrev"],
        ["Up", "dashboardFocusPrev"],
        ["k", "dashboardFocusPrev"],
        ["Return", "dashboardActivate"],
    ],
    true, // read-only
    false, // allow_text_input
    false, // don't inherit Normal bindings — no cursor to move
);

// Dispatch clicks on rows that carry an action. We don't trust the
// terminal to honor OSC-8 hyperlinks on the `url` span — many strip
// them silently — so every clickable element also registers a
// row-based ClickAction and we route the click ourselves.
registerHandler(
    "dashboardOnMouseClick",
    (data: {
        column: number;
        row: number;
        button: string;
        modifiers: string;
        content_x: number;
        content_y: number;
        buffer_id: number | null;
        buffer_row: number | null;
        buffer_col: number | null;
    }) => {
        if (data.button !== "left") return;
        if (dashboardBufferId === null) return;
        if (data.buffer_id !== dashboardBufferId) return;
        if (data.buffer_row === null) return;
        const ranges = currentRowActions.get(data.buffer_row);
        if (!ranges) return;
        // Gate on the visual column within the buffer's content area.
        // `buffer_col` is in UTF-8 bytes and doesn't line up with the
        // frame's multi-byte characters (`│`, `╭` etc.), so we derive
        // the visual column from `column - content_x` — the screen
        // column relative to the buffer panel's left edge. Our
        // registered ranges are stored in visual cells and reset to 0
        // at each newline, so the two units match. A click outside
        // every registered range is a no-op: whitespace, kv labels,
        // and the frame border inside an inner row stay unclickable,
        // matching the underline-as-affordance contract.
        const visualCol = data.column - data.content_x;
        const match = ranges.find(
            (r) => visualCol >= r.colStart && visualCol < r.colEnd,
        );
        if (!match) return;
        dispatchClickAction(match.action);
    },
);

// Register the built-in sections. They use the same public
// `DashboardContext` API that third-party plugins consume, so any
// change to the context contract surfaces here first.
//
// `weather` and `github` are opt-in — they hit the network on every
// refresh, so we only register `git` and `disk` by default. Users
// wire the others up from init.ts via the exported plugin API; see
// the init.ts starter template for a ready-to-paste example.
registerSection("git", gitRefresh);
registerSection("disk", diskRefresh);

// Expose the section-management entry points to other plugins and to
// user init.ts. `registerSection(name, refresh)` adds a section and
// returns an unregister callback; `removeSection(name)` tears sections
// down by name; `clearAllSections()` removes every section, built-ins
// included. The refresh callback receives a `DashboardContext` with
// `kv`, `text`, `newline`, and `error` primitives — see the init.ts
// starter template for an end-to-end example.
editor.exportPluginApi("dashboard", {
    registerSection(name: string, refresh: SectionRefresh): () => void {
        if (typeof name !== "string" || name.length === 0) {
            throw new Error("dashboard.registerSection: name must be a non-empty string");
        }
        if (typeof refresh !== "function") {
            throw new Error("dashboard.registerSection: refresh must be a function");
        }
        return registerSection(name, refresh);
    },
    removeSection(name: string): boolean {
        if (typeof name !== "string" || name.length === 0) {
            throw new Error("dashboard.removeSection: name must be a non-empty string");
        }
        return removeSection(name);
    },
    clearAllSections(): void {
        clearAllSections();
    },
    setAutoOpen(enabled: boolean): void {
        autoOpenOverride = !!enabled;
    },
    builtinHandlers: {
        weather: weatherRefresh,
        github: githubRefresh,
    },
});

// Subscribe to the hooks that drive the dashboard. Reaching this code
// means the plugin has been loaded, which only happens when
// `plugins.dashboard.enabled` is true in the resolved config — so the
// standard settings UI is the single enable/disable surface.
//
// If the plugin loads mid-session (user toggles it on in Settings),
// the `ready` hook has already fired, so we also run an immediate
// check. At startup the `listBuffers().length > 0` guard keeps us
// dormant until the workspace has actually restored: plugins load
// before restore, and opening a buffer here would race with the
// restore and leave a stray Dashboard tab even when real files exist.
editor.on("ready", "dashboardOnReady");
editor.on("buffer_closed", "dashboardOnBufferClosed");
editor.on("viewport_changed", "dashboardOnViewportChanged");
editor.on("mouse_click", "dashboardOnMouseClick");
editor.on("after_file_open", "dashboardOnAfterFileOpen");

// Command-palette entry. No-op when the dashboard is already the
// focused tab (showBuffer on a visible buffer is a cheap re-focus).
editor.registerCommand(
    "Show Dashboard",
    "Open the dashboard, or bring it to the front if it's already open",
    "dashboardShowOrFocus",
);

if (editor.listBuffers().length > 0 && shouldShowDashboard()) {
    openDashboard();
}
