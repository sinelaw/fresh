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
//   - Auto-centers inside the viewport. Repaints on terminal resize.
//   - Auto-refreshes every 5 seconds while visible.
//   - Closes the file explorer to get the full viewport.
//   - All colors are theme keys → repaints for free on theme switch.
//   - All color/state spans can carry URLs (OSC-8 hyperlinks).
// ═════════════════════════════════════════════════════════════════════

type Span = {
    start: number;
    end: number;
    fg?: string;
    bg?: string;
    bold?: boolean;
    url?: string;
};
type Draw = { text: string; spans: Span[] };
type Section = { draw: (d: Draw) => void };

const NS = "dashboard";
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
    opts?: { fg?: string; bold?: boolean; url?: string },
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
}

function newline(d: Draw) {
    d.text += "\n";
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
    const d: Draw = { text: "", spans: [] };
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

    const row = (body: Draw) => {
        // Wraps a single logical row of section body in the frame.
        emit(d, lp, undefined);
        emit(d, "│", { fg: C.frame });
        // Splice the section body (which may have its own spans) into d.
        const bodyLines = body.text.split("\n");
        const bodyBytesPerLine: number[] = [];
        {
            let cursor = 0;
            for (const ln of bodyLines) {
                bodyBytesPerLine.push(cursor);
                cursor += utf8Len(ln) + 1;
            }
        }
        // This function expects body already formatted for one line.
        const line = bodyLines[0] ?? "";
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
        const body: Draw = { text: "", spans: [] };
        s.draw(body);
        const bodyLines = body.text.split("\n");
        let cursor = 0;
        for (const ln of bodyLines) {
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
            row({ text: ln, spans: sliced });
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

function paint() {
    if (dashboardBufferId === null) return;
    const bufferId = dashboardBufferId;
    const vp = editor.getViewport();
    const width = vp?.width ?? 100;
    const { inner, leftPad } = frameWidth(width);
    const drawn = renderFrame(inner, leftPad);

    editor.clearNamespace(bufferId, NS);
    const info = editor.getBufferInfo(bufferId);
    const existing = info?.length ?? 0;
    if (existing > 0) editor.deleteRange(bufferId, 0, existing);
    editor.insertText(bufferId, 0, drawn.text);
    for (const sp of drawn.spans) {
        const opts: Record<string, unknown> = {};
        if (sp.fg) opts.fg = sp.fg;
        if (sp.bold) opts.bold = true;
        if (sp.url) opts.url = sp.url;
        if (Object.keys(opts).length === 0) continue;
        editor.addOverlay(bufferId, NS, sp.start, sp.end, opts);
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

async function fetchWeather(myToken: number) {
    try {
        const { stdout, ok } = await run(
            "curl",
            [
                "-fsS",
                "--max-time",
                "5",
                // Drop %l — we don't display location.
                "https://wttr.in/?format=%C|%t|%f|%w|%h",
            ],
            "",
            6000,
        );
        if (myToken !== fetchToken) return;
        if (!ok || !stdout.trim()) {
            sections.weather = errorSection("offline");
        } else {
            const parts = trim(stdout).split("|").map((s) => s.trim().replace(/\s+/g, " "));
            const [cond, temp, feels, wind, hum] = [
                parts[0] ?? "",
                parts[1] ?? "",
                parts[2] ?? "",
                parts[3] ?? "",
                parts[4] ?? "",
            ];
            // Skip the "feels like" field when it matches the real temp —
            // common on wttr.in output and just adds noise.
            const feelsPart = feels && feels !== temp ? `feels ${feels}` : "";
            const summary = truncate(
                [cond, temp, feelsPart, wind, hum]
                    .filter((s) => s.length > 0)
                    .join(" · "),
                VALUE_MAX,
            );
            sections.weather = {
                draw: (d) => kv(d, "now", summary, C.accent),
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
                const parts = trim(ahead.stdout).split(/\s+/);
                const a = Number(parts[0]);
                const b = Number(parts[1]);
                if (!isNaN(a) && !isNaN(b)) {
                    trackStr = `↑ ${a}   ↓ ${b}`;
                    trackColor = a > 0 || b > 0 ? C.accent : C.ok;
                }
            }
            const repoUrl = remote.ok ? normalizeRepoUrl(remote.stdout) : null;
            currentRepoUrl = repoUrl;
            const branchName = trim(branch.stdout);
            sections.git = {
                draw: (d) => {
                    // branch — clickable, links to branch page on host
                    emit(d, "    " + pad("branch", 10), { fg: C.muted });
                    emit(d, branchName, {
                        fg: C.branch,
                        url: repoUrl ? `${repoUrl}/tree/${encodeURIComponent(branchName)}` : undefined,
                    });
                    newline(d);

                    // remote URL — clickable
                    if (repoUrl) {
                        emit(d, "    " + pad("repo", 10), { fg: C.muted });
                        emit(d, repoUrl.replace(/^https?:\/\//, ""), {
                            fg: C.accent,
                            url: repoUrl,
                        });
                        newline(d);
                    }

                    kv(d, "tracking", trackStr, trackColor);
                    kv(
                        d,
                        "changes",
                        `${modified} file${modified === 1 ? "" : "s"}`,
                        modified > 0 ? C.warn : C.muted,
                    );
                },
            };
        }
    } catch {
        sections.git = errorSection("git failed");
    }
    paint();
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
            const why =
                stderr.includes("not found") || stderr.includes("no such file")
                    ? "gh not installed"
                    : stderr.includes("auth")
                        ? "gh not authenticated"
                        : trim(res.stderr).split("\n")[0]?.slice(0, 40) || "gh failed";
            sections.github = errorSection(why);
            paint();
            return;
        }
        let parsed: unknown;
        try {
            parsed = JSON.parse(res.stdout);
        } catch {
            sections.github = errorSection("malformed response");
            paint();
            return;
        }
        type Rollup = { state?: string } | null;
        type Commit = { statusCheckRollup?: Rollup };
        type CommitNode = { commit?: Commit };
        type Thread = { isResolved?: boolean; comments?: { totalCount?: number } };
        type PR = {
            number?: number;
            title?: string;
            state?: string;
            repository?: { nameWithOwner?: string };
            commits?: { nodes?: CommitNode[] };
            reviewThreads?: { nodes?: Thread[] };
        };
        const prs: PR[] =
            (parsed as { data?: { viewer?: { pullRequests?: { nodes?: PR[] } } } })?.data
                ?.viewer?.pullRequests?.nodes ?? [];
        if (prs.length === 0) {
            sections.github = {
                draw: (d) => kv(d, "PRs", "no recent PRs by you", C.muted),
            };
            paint();
            return;
        }

        sections.github = {
            draw: (d) => {
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

                    const rollup =
                        pr.commits?.nodes?.[0]?.commit?.statusCheckRollup?.state ?? null;
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

                    emit(d, "    ", undefined);
                    emit(d, pad(num, 6), { fg: C.number, url: prUrl });
                    emit(d, pad(stateTag, 5), { fg: stateColor, bold: true });
                    emit(d, " ", undefined);
                    emit(d, checkGlyph + " ", { fg: checkColor, bold: true });
                    const cmtCell =
                        openCmts > 0
                            ? pad(`${openCmts} cmt`, 6)
                            : pad("", 6);
                    emit(d, cmtCell, {
                        fg: openCmts > 0 ? C.warn : C.muted,
                    });
                    emit(d, " ", undefined);
                    emit(d, title, { fg: C.value, url: prUrl });
                    newline(d);
                }
            },
        };
    } catch {
        sections.github = errorSection("gh failed");
    }
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

async function openDashboard() {
    if (dashboardBufferId !== null) return; // already open

    const res = await editor.createVirtualBuffer({
        name: "Dashboard",
        readOnly: true,
        showLineNumbers: false,
        showCursors: false,
        editingDisabled: true,
    });
    dashboardBufferId = res.bufferId;
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

    // Close the file explorer so the dashboard has the full viewport.
    // No direct "close" action exists — ToggleFileExplorer will close
    // it if open; we only toggle once, so a closed explorer wouldn't
    // re-open spuriously on repeated dashboard-opens.
    editor.executeAction("ToggleFileExplorer");

    // Reset section state and kick new fetches. Token guards against late
    // resolvers from a prior open clobbering the new one.
    fetchToken++;
    const myToken = fetchToken;
    sections = {
        weather: loading(),
        git: loading(),
        github: loading(),
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

// Closures aren't (yet) typed for editor.on — register named handlers
// via the documented `registerHandler` + string-based `on` pattern.
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
registerHandler("dashboardOnResize", () => {
    if (dashboardBufferId !== null) paint();
});

editor.on("ready", "dashboardOnReady");
editor.on("buffer_closed", "dashboardOnBufferClosed");
editor.on("resize", "dashboardOnResize");
