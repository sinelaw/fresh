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
//   Opt-in. The plugin loads but stays dormant until the user enables
//   it from `~/.config/fresh/init.ts`:
//
//       const dash = editor.getPluginApi("dashboard");
//       if (dash) dash.enable();
//
//   `disable()` reverses it, closing the buffer and unsubscribing all
//   hooks. Until `enable()` is called, the plugin doesn't create any
//   buffers, subscribe to any events, or run the refresh loop.
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
    url?: string;
};
// Click action attached to whole rows — dispatched by the mouse_click
// handler, which looks up the clicked buffer row in Draw.rowActions.
// Since terminals that swallow OSC-8 hyperlinks are common, we can't
// rely on the `url` span alone; routing clicks through the editor
// guarantees PR numbers / repo / review-branch are always actionable.
type ClickAction =
    | { kind: "open-url"; url: string }
    | { kind: "review-branch" };
type Draw = {
    text: string;
    spans: Span[];
    // currentRow tracks how many newlines we've emitted; row-tagged
    // entries register their action against this row so the click
    // handler can route a click at buffer_row back to the action.
    currentRow: number;
    rowActions: Map<number, ClickAction>;
};
type Section = { draw: (d: Draw) => void };

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

// State survives across open/close cycles so we don't pile up dashboards.
let dashboardBufferId: number | null = null;
let sections: Record<"weather" | "git" | "github" | "disk", Section> = {
    weather: loading(),
    git: loading(),
    github: loading(),
    disk: loading(),
};
let fetchToken = 0; // bumped each open; late fetches from a prior open no-op.

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
    if (opts?.fg || opts?.bold || opts?.url) {
        d.spans.push({
            start,
            end: start + utf8Len(s),
            fg: opts.fg,
            bold: opts.bold,
            url: opts.url,
        });
    }
    if (opts?.action) {
        // Anchor to the row this emit landed on — subsequent emits on the
        // same row would overwrite this if they set a different action,
        // which is fine because a single row has one logical click target.
        d.rowActions.set(d.currentRow, opts.action);
    }
}

function newline(d: Draw) {
    d.text += "\n";
    d.currentRow++;
}

function emptyDraw(): Draw {
    return { text: "", spans: [], currentRow: 0, rowActions: new Map() };
}

// ── Sections (sentinel / placeholder factories) ────────────────────────

function loading(): Section {
    return {
        draw: (d) => {
            const label = pad("status", 10);
            emit(d, "    " + label, { fg: C.muted });
            emit(d, "loading…", { fg: C.muted });
            newline(d);
        },
    };
}

function errorSection(why: string): Section {
    return {
        draw: (d) => {
            const label = pad("status", 10);
            emit(d, "    " + label, { fg: C.muted });
            emit(d, why, { fg: C.err });
            newline(d);
        },
    };
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
        action?: ClickAction,
    ) => {
        // Wraps a single logical row of section body in the frame.
        emit(d, lp, undefined);
        emit(d, "│", { fg: C.frame });
        // body is already one line (no embedded newlines) — renderSection
        // slices multi-line section output before calling row().
        const line = body.text;
        const used = visualWidth(line);
        const startInDoc = utf8Len(d.text);
        d.text += line;
        for (const sp of body.spans) {
            if (sp.start < utf8Len(line)) {
                d.spans.push({
                    start: startInDoc + sp.start,
                    end: startInDoc + Math.min(sp.end, utf8Len(line)),
                    fg: sp.fg,
                    bold: sp.bold,
                    url: sp.url,
                });
            }
        }
        emit(d, " ".repeat(Math.max(0, inner - used)), undefined);
        emit(d, "│", { fg: C.frame });
        if (action) d.rowActions.set(d.currentRow, action);
        newline(d);
    };

    const spacerRow = () => {
        emit(d, lp, undefined);
        emit(d, "│", { fg: C.frame });
        emit(d, " ".repeat(inner), undefined);
        emit(d, "│", { fg: C.frame });
        newline(d);
    };

    const renderSection = (name: string, s: Section) => {
        sectionHeader(name);
        // Let section draw into a detached Draw, then split into rows.
        const body: Draw = emptyDraw();
        s.draw(body);
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
                    url: sp.url,
                }));
            // Propagate the section-level row action (keyed by section-body
            // line index) onto the outer frame row we're about to write.
            row({ text: ln, spans: sliced }, body.rowActions.get(lineIdx));
            cursor = lineEnd + 1;
        }
        spacerRow();
    };

    renderSection("WEATHER", sections.weather);
    renderSection("GIT", sections.git);
    renderSection("GITHUB", sections.github);
    renderSection("DISK", sections.disk);

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

// Row-index → click action map, keyed by absolute buffer row (after
// adding topPad). Rebuilt every paint; read by the mouse_click handler.
let currentRowActions: Map<number, ClickAction> = new Map();

function paint() {
    if (dashboardBufferId === null) return;
    const bufferId = dashboardBufferId;
    const vp = editor.getViewport();
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
    // shifting by the vertical padding we just prepended.
    const abs: Map<number, ClickAction> = new Map();
    for (const [row, action] of drawn.rowActions) {
        abs.set(row + topPad, action);
    }
    currentRowActions = abs;

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
        case "review-branch":
            // `start_review_diff` is registered by the audit_mode plugin.
            // executeAction falls through to Action::PluginAction for any
            // name that isn't a built-in, which the plugin manager then
            // dispatches by handler name.
            editor.executeAction("start_review_diff");
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

function kv(d: Draw, key: string, val: string, valColor: string = C.value) {
    emit(d, "    " + pad(key, 10), { fg: C.muted });
    emit(d, val, { fg: valColor });
    newline(d);
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

async function fetchWeather(myToken: number) {
    try {
        // j1 = full JSON payload (current + 3-day forecast, 3-hour samples).
        // Larger than the old %-format but gets us everything in one call.
        const { stdout, ok } = await run(
            "curl",
            ["-fsS", "--max-time", "5", "https://wttr.in/?format=j1"],
            "",
            6000,
        );
        if (myToken !== fetchToken) return;
        if (!ok || !stdout.trim()) {
            sections.weather = errorSection("offline");
        } else {
            let parsed: WttrJ1;
            try {
                parsed = JSON.parse(stdout) as WttrJ1;
            } catch {
                sections.weather = errorSection("malformed response");
                paint();
                return;
            }
            const now = formatCurrent(parsed.current_condition?.[0]);
            // "5pm": nearest 3-hour sample in today's forecast. wttr.in
            // buckets at 0/3/6/…/21, so 17:00 picks the 18:00 bucket.
            const todayHours = parsed.weather?.[0]?.hourly ?? [];
            const evening = formatHour(hourlyAt(todayHours, 17));
            const tomorrow = formatDaySummary(parsed.weather?.[1]);
            sections.weather = {
                draw: (d) => {
                    kv(d, "now", now ?? "–", now ? C.accent : C.muted);
                    if (evening) kv(d, "5pm", evening, C.value);
                    if (tomorrow) kv(d, "tomorrow", tomorrow, C.value);
                },
            };
        }
    } catch {
        sections.weather = errorSection("fetch failed");
    }
    paint();
}

// Shared across fetchGit and fetchGithub so PRs can link into the same
// repo without re-invoking git.
let currentRepoUrl: string | null = null; // e.g. https://github.com/owner/repo

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

// Parse the two numbers produced by `git rev-list --left-right --count`
// ("<ahead> <behind>"). Returns null on malformed output.
function parseLeftRight(stdout: string): { ahead: number; behind: number } | null {
    const parts = trim(stdout).split(/\s+/);
    const a = Number(parts[0]);
    const b = Number(parts[1]);
    if (isNaN(a) || isNaN(b)) return null;
    return { ahead: a, behind: b };
}

async function fetchGit(myToken: number) {
    const cwd = editor.getCwd();
    try {
        const [branch, status, ahead, remote] = await Promise.all([
            run("git", ["rev-parse", "--abbrev-ref", "HEAD"], cwd, 3000),
            run("git", ["status", "--porcelain"], cwd, 3000),
            run("git", ["rev-list", "--left-right", "--count", "HEAD...@{u}"], cwd, 3000),
            run("git", ["remote", "get-url", "origin"], cwd, 3000),
        ]);
        if (myToken !== fetchToken) return;
        if (!branch.ok) {
            sections.git = errorSection("not a git repo");
        } else {
            const modified = status.stdout
                .split("\n")
                .filter((l) => l.trim().length > 0).length;
            let trackStr = "no upstream";
            let trackColor = C.muted;
            if (ahead.ok) {
                const ab = parseLeftRight(ahead.stdout);
                if (ab) {
                    trackStr = `↑ ${ab.ahead}   ↓ ${ab.behind}`;
                    trackColor = ab.ahead > 0 || ab.behind > 0 ? C.accent : C.ok;
                }
            }
            const repoUrl = remote.ok ? normalizeRepoUrl(remote.stdout) : null;
            currentRepoUrl = repoUrl;
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
            if (myToken !== fetchToken) return;

            sections.git = {
                draw: (d) => {
                    // branch — whole row routes clicks to the branch page
                    const branchBranchUrl = repoUrl
                        ? `${repoUrl}/tree/${encodeURIComponent(branchName)}`
                        : undefined;
                    emit(d, "    " + pad("branch", 10), { fg: C.muted });
                    emit(d, branchName, {
                        fg: C.branch,
                        url: branchBranchUrl,
                        action: branchBranchUrl
                            ? { kind: "open-url", url: branchBranchUrl }
                            : undefined,
                    });
                    newline(d);

                    // remote URL — displayed in full with scheme so that
                    // terminals that auto-detect URLs (but ignore OSC-8)
                    // still recognize it. The whole row is also click-
                    // routable via the mouse_click hook.
                    if (repoUrl) {
                        emit(d, "    " + pad("repo", 10), { fg: C.muted });
                        emit(d, repoUrl, {
                            fg: C.accent,
                            url: repoUrl,
                            action: { kind: "open-url", url: repoUrl },
                        });
                        newline(d);
                    }

                    kv(d, "tracking", trackStr, trackColor);
                    if (vsBase) {
                        const label = `vs ${vsBase.base}`;
                        const str = `↑ ${vsBase.ahead}   ↓ ${vsBase.behind}`;
                        const color =
                            vsBase.ahead > 0 || vsBase.behind > 0 ? C.accent : C.ok;
                        kv(d, label, str, color);
                    }
                    kv(
                        d,
                        "changes",
                        `${modified} file${modified === 1 ? "" : "s"}`,
                        modified > 0 ? C.warn : C.muted,
                    );

                    // Clickable "review branch" action. Triggers the
                    // audit_mode plugin's `start_review_diff` handler via
                    // the plugin-action bridge — executeAction falls
                    // through to Action::PluginAction for any name that's
                    // not a built-in, and the plugin manager dispatches
                    // that to the registered handler by name.
                    emit(d, "    " + pad("review", 10), { fg: C.muted });
                    emit(d, "▶ review branch", {
                        fg: C.accent,
                        bold: true,
                        action: { kind: "review-branch" },
                    });
                    newline(d);
                },
            };
        }
    } catch {
        sections.git = errorSection("git failed");
    }
    paint();
}

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

function renderPrRows(d: Draw, prs: GhPR[]) {
    if (prs.length === 0) {
        kv(d, "PRs", "no recent PRs by you", C.muted);
        return;
    }
    kv(d, "PRs", `${prs.length} by you`, C.number);
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
        const stateColor =
            state === "OPEN"
                ? C.ok
                : state === "MERGED"
                    ? C.accent
                    : state === "CLOSED"
                        ? C.muted
                        : C.muted;

        const rollup = pr.commits?.nodes?.[0]?.commit?.statusCheckRollup?.state ?? null;
        const checkGlyph =
            rollup === "SUCCESS"
                ? "✓"
                : rollup === "FAILURE" || rollup === "ERROR"
                    ? "✗"
                    : rollup === "PENDING" || rollup === "EXPECTED"
                        ? "◌"
                        : "–";
        const checkColor =
            rollup === "SUCCESS"
                ? C.ok
                : rollup === "FAILURE" || rollup === "ERROR"
                    ? C.err
                    : rollup === "PENDING" || rollup === "EXPECTED"
                        ? C.warn
                        : C.muted;

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
        // overwrite with the same value).
        const prAction: ClickAction | undefined = prUrl
            ? { kind: "open-url", url: prUrl }
            : undefined;
        emit(d, "    ", undefined);
        emit(d, pad(num, 6), { fg: C.number, url: prUrl, action: prAction });
        emit(d, pad(stateTag, 5), { fg: stateColor, bold: true });
        emit(d, " ", undefined);
        emit(d, checkGlyph + " ", { fg: checkColor, bold: true });
        const cmtCell = openCmts > 0 ? pad(`${openCmts} cmt`, 6) : pad("", 6);
        emit(d, cmtCell, { fg: openCmts > 0 ? C.warn : C.muted });
        emit(d, " ", undefined);
        emit(d, title, { fg: C.value, url: prUrl });
        newline(d);
    }
}

function buildGithubSection(): Section {
    return {
        draw: (d) => {
            // Stale-data banner: when we have previously-good PRs AND the
            // latest refresh failed, show both. Keeps the rest of the
            // section anchored — no row-count jumps between ticks.
            if (githubLastError && githubLastPrs !== null) {
                emit(d, "    " + pad("update", 10), { fg: C.muted });
                emit(d, `failed — ${githubLastError}`, { fg: C.err });
                newline(d);
                renderPrRows(d, githubLastPrs);
                return;
            }
            if (githubLastPrs !== null) {
                renderPrRows(d, githubLastPrs);
                return;
            }
            if (githubLastError) {
                emit(d, "    " + pad("status", 10), { fg: C.muted });
                emit(d, githubLastError, { fg: C.err });
                newline(d);
                return;
            }
            // First run, nothing yet.
            emit(d, "    " + pad("status", 10), { fg: C.muted });
            emit(d, "loading…", { fg: C.muted });
            newline(d);
        },
    };
}

async function fetchGithub(myToken: number) {
    // Recent PRs authored by the current user. One GraphQL round-trip
    // fetches state (OPEN / MERGED / CLOSED), combined check status
    // from the tip commit's rollup, and the list of review threads so
    // we can count *unresolved* comment threads per PR.
    const query = `
        query {
            viewer {
                pullRequests(first: 6, orderBy: {field: UPDATED_AT, direction: DESC}) {
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
            ["api", "graphql", "-f", `query=${query}`],
            "",
            7000,
        );
        if (myToken !== fetchToken) return;
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
                    (parsed as { data?: { viewer?: { pullRequests?: { nodes?: GhPR[] } } } })
                        ?.data?.viewer?.pullRequests?.nodes ?? [];
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
    sections.github = buildGithubSection();
    paint();
}

async function fetchDisk(myToken: number) {
    try {
        const mounts = ["/", editor.getEnv("HOME") ?? "/home"];
        const seen = new Set<string>();
        const rows: { mount: string; pct: number; used: string; size: string }[] = [];
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
        if (myToken !== fetchToken) return;
        if (rows.length === 0) {
            sections.disk = errorSection("df failed");
        } else {
            sections.disk = {
                draw: (d) => {
                    for (const row of rows) {
                        const fg = row.pct >= 90 ? C.err : row.pct >= 75 ? C.warn : C.ok;
                        emit(d, "    " + pad(row.mount, 10), { fg: C.muted });
                        emit(d, bar(row.pct, 18), { fg, bold: true });
                        emit(d, "  " + String(row.pct).padStart(3) + "%", { fg });
                        emit(d, `   ${row.used} / ${row.size}`, { fg: C.muted });
                        newline(d);
                    }
                },
            };
        }
    } catch {
        sections.disk = errorSection("df failed");
    }
    paint();
}

// ── Lifecycle ──────────────────────────────────────────────────────────

// Fire-and-forget: refresh every 5s while the dashboard remains the
// active dashboard. Each tick bumps `fetchToken` and re-kicks all four
// fetchers; in-flight fetches from a previous tick become no-ops the
// moment their token stops matching. Loop exits when the dashboard
// buffer is closed (dashboardBufferId becomes null).
async function refreshLoop(myBufferId: number) {
    while (dashboardBufferId === myBufferId) {
        await editor.delay(5000);
        if (dashboardBufferId !== myBufferId) return;
        paint(); // refresh clock even if fetches lag
        fetchToken++;
        const tok = fetchToken;
        fetchWeather(tok);
        fetchGit(tok);
        fetchGithub(tok);
        fetchDisk(tok);
    }
}

// Set for the duration of an in-flight openDashboard call. The
// createVirtualBuffer round-trip is async, so a second openDashboard
// invocation (e.g. from the `ready` hook firing while enable()'s own
// call is still awaiting) would otherwise see dashboardBufferId === null
// and create a second Dashboard tab.
let dashboardOpening = false;

async function openDashboard() {
    if (dashboardBufferId !== null) return; // already open
    if (dashboardOpening) return; // another openDashboard is mid-await
    dashboardOpening = true;

    const res = await editor.createVirtualBuffer({
        name: "Dashboard",
        readOnly: true,
        showLineNumbers: false,
        showCursors: false,
        editingDisabled: true,
    });
    dashboardBufferId = res.bufferId;
    dashboardOpening = false;
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

    // Reset section state and kick new fetches. Token guards against late
    // resolvers from a prior open clobbering the new one.
    //
    // GitHub reuses the last-good PR snapshot (if any) so a re-opened
    // dashboard can draw real data on the first frame while the refresh
    // round-trip is still in flight. A refresh failure later on will
    // surface via the in-panel stale-data banner.
    fetchToken++;
    const myToken = fetchToken;
    sections = {
        weather: loading(),
        git: loading(),
        github: githubLastPrs !== null ? buildGithubSection() : loading(),
        disk: loading(),
    };
    paint();

    fetchWeather(myToken);
    fetchGit(myToken);
    fetchGithub(myToken);
    fetchDisk(myToken);

    // Kick off the 5-second refresh loop. It stops itself when the
    // dashboard is closed.
    refreshLoop(dashboardBufferId);
}

function shouldShowDashboard(): boolean {
    if (dashboardBufferId !== null) return false;
    const all = editor.listBuffers();
    const realFiles = all.filter(
        (b) => !b.is_virtual && b.path && b.path.length > 0,
    );
    return realFiles.length === 0;
}

// ── Opt-in via init.ts ─────────────────────────────────────────────────

// Named handlers are registered up-front — the plugin runtime requires
// handlers to exist before `editor.on(...)` subscribes to them — but we
// don't subscribe to any editor events until init.ts calls `enable()`
// on the exported plugin API. A user who never opts in pays only the
// plugin-load cost; no buffers are created, no timers run, no network
// fetches fire.
registerHandler("dashboardOnReady", async () => {
    if (shouldShowDashboard()) await openDashboard();
});
registerHandler(
    "dashboardOnBufferClosed",
    async (e: { buffer_id: number }) => {
        // If the dashboard itself was closed, clear our handle so we'll
        // re-open on the next "last tab closed" event.
        if (dashboardBufferId !== null && e.buffer_id === dashboardBufferId) {
            dashboardBufferId = null;
            return;
        }
        if (shouldShowDashboard()) await openDashboard();
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
        if (data.width === lastPaintedW && data.height === lastPaintedH) return;
        paint();
    },
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
        buffer_id: number | null;
        buffer_row: number | null;
        buffer_col: number | null;
    }) => {
        if (data.button !== "left") return;
        if (dashboardBufferId === null) return;
        if (data.buffer_id !== dashboardBufferId) return;
        if (data.buffer_row === null) return;
        const action = currentRowActions.get(data.buffer_row);
        if (!action) return;
        dispatchClickAction(action);
    },
);

let dashboardEnabled = false;

function enableDashboard() {
    if (dashboardEnabled) return;
    dashboardEnabled = true;
    editor.on("ready", "dashboardOnReady");
    editor.on("buffer_closed", "dashboardOnBufferClosed");
    editor.on("viewport_changed", "dashboardOnViewportChanged");
    editor.on("mouse_click", "dashboardOnMouseClick");
    // If init.ts enables us at runtime (or re-enables after a disable()),
    // the `ready` hook may have already fired, so trigger an immediate
    // check. Startup path: init.ts runs before workspace restore and
    // before `ready`, so `shouldShowDashboard()` here will see zero
    // buffers and the dashboard would open prematurely — defer to the
    // `ready` handler in that case. We key off whether plugins are fully
    // initialized by inspecting the buffer list: during early startup
    // there are no buffers at all (listBuffers() returns []), whereas at
    // runtime there's always at least the current or scratch buffer.
    if (editor.listBuffers().length > 0 && shouldShowDashboard()) {
        openDashboard();
    }
}

function disableDashboard() {
    if (!dashboardEnabled) return;
    dashboardEnabled = false;
    editor.off("ready", "dashboardOnReady");
    editor.off("buffer_closed", "dashboardOnBufferClosed");
    editor.off("viewport_changed", "dashboardOnViewportChanged");
    editor.off("mouse_click", "dashboardOnMouseClick");
    if (dashboardBufferId !== null) {
        editor.closeBuffer(dashboardBufferId);
        dashboardBufferId = null;
    }
}

// Expose enable/disable so init.ts can opt in:
//
//   // ~/.config/fresh/init.ts
//   const dash = editor.getPluginApi("dashboard");
//   if (dash) dash.enable();
//
// `plugins_loaded` isn't needed here — init.ts runs after every
// registry plugin, so getPluginApi("dashboard") is already available
// at top level.
editor.exportPluginApi("dashboard", {
    enable: enableDashboard,
    disable: disableDashboard,
});
