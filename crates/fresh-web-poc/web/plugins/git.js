// Git, through the workspace host's own `git` (so it is the remote's git
// for a remote workspace):
//   * the branch in the status bar, refreshed when HEAD changes;
//   * a gutter of added / modified / deleted lines, from a diff of the
//     buffer against HEAD done here (so it follows unsaved edits);
//   * next / previous change;
//   * blame, as text at the end of each line (lines edited since the file
//     was saved show as not committed);
//   * the log in a panel; a commit's diff in a buffer of its own;
//   * the diff of the current file, unsaved edits included.

import { diffLines, mapLines, unifiedDiff } from "../js/diff.js";
import * as fresh from "../js/api.js";
import { found } from "../js/api.js";
import * as ui from "../js/ui.js";

// `path` from `root`, when it is under it.
/** function relative(root: String, path: String | Null) => String | Null */
function relative(root, path) {
  return path !== null && path.startsWith(`${root}/`) ? path.slice(root.length + 1) : null;
}

// A line's blame, at the row's end.
/** const BLAME: Style */
const BLAME = { fg: "editor.line_number_fg", bg: null, bold: false, italic: true, underline: false, strike: false, mono: false, extendToLineEnd: false };

// A line's blame (git blame --line-porcelain): its commit, its line in the
// file on disk, and who, when and why.
/** type Blamed = {hash: String, line: Number, author: String, time: String, summary: String} */

// One porcelain entry's lines (a header, then "key value" lines).
/** function blameEntry(lines: String[]) => Blamed | Null */
function blameEntry(lines) {
  const head = /^([0-9a-f]{40}) \d+ (\d+)/.exec(lines.length ? lines[0] : "");
  if (!head) return null;
  const field = (k) => { const l = lines.find((x) => x.startsWith(`${k} `)); return l ? l.slice(k.length + 1) : ""; };
  const at = Number(field("author-time"));
  return {
    hash: head[1], line: Number(head[2]) - 1, author: field("author"),
    time: at ? new Date(at * 1000).toISOString().slice(0, 10) : "", summary: field("summary"),
  };
}

// Each entry ends with its line's text, tab first.
/** function parseBlame(text: String) => Blamed[] */
function parseBlame(text) {
  /** const out: Blamed[] */
  const out = [];
  /** let entry: String[] */
  let entry = [];
  for (const l of text.split("\n")) {
    if (!l.startsWith("\t")) { entry.push(l); continue; }
    const e = blameEntry(entry);
    if (e) out.push(e);
    entry = [];
  }
  return out;
}

// A file as it is at HEAD: its lines, not in HEAD, or not known (git
// could not say: why).
/** type Baseline =
 *   | {kind: "lines", lines: String[]}
 *   | {kind: "untracked"}
 *   | {kind: "unknown", why: String}
 */
/** const UNTRACKED: Baseline */
const UNTRACKED = { kind: "untracked" };
/** function baseLines(lines: String[]) => Baseline */
function baseLines(lines) { return { kind: "lines", lines }; }
/** function unknownBase(why: String) => Baseline */
function unknownBase(why) { return { kind: "unknown", why }; }

export function activate(app) {
  const root = fresh.root(app);
  const rel = (p) => relative(root, p);
  // git in the workspace (a restricted workspace refuses it: the command
  // that asked says so).
  const git = (args) => fresh.runProcess(app, "git", args, { cwd: root });
  /** const baselines: Map<String, Baseline> */
  const baselines = new Map(); // path -> the file at HEAD, once known
  /** const told: Set<String> */
  const told = new Set(); // paths whose baseline git could not give, said once

  // ---------------------------------------------------------------- branch

  const branch = fresh.statusItem(app, "git-branch");
  const refreshBranch = async () => {
    baselines.clear(); // HEAD moved: every baseline may have
    const r = await git(["rev-parse", "--abbrev-ref", "HEAD"]);
    branch.set(r.code === 0 ? `⎇ ${r.stdout.trim()}` : "", { title: "Git branch — click for the log", onClick: () => { fresh.run(app, "git.log"); } });
  };
  // HEAD changes on a checkout; the branch's ref file on a commit.
  // Stops watching the current branch's ref file (none on a detached HEAD).
  /** let unwatchRef: (() => Undefined) | Null */
  let unwatchRef = null;
  const watchRefs = async () => {
    const ref = await git(["symbolic-ref", "-q", "HEAD"]);
    const before = unwatchRef;
    unwatchRef = null;
    if (before) before();
    if (ref.code === 0) {
      const p = await git(["rev-parse", "--path-format=absolute", "--git-path", ref.stdout.trim()]);
      if (p.code === 0) unwatchRef = await fresh.watch(app, p.stdout.trim(), () => { refreshBranch(); const b = fresh.activeBuffer(app); if (b) schedule(b); });
    }
  };
  const start = async () => {
    await refreshBranch();
    // (Absolute: in a linked worktree HEAD is under the main repository's
    // .git, not this folder.)
    const head = await git(["rev-parse", "--path-format=absolute", "--git-path", "HEAD"]);
    if (head.code === 0) fresh.watch(app, head.stdout.trim(), async () => { await refreshBranch(); await watchRefs(); });
    await watchRefs();
    const b = fresh.activeBuffer(app);
    if (b) schedule(b);
  };
  fresh.on(app, "app.ready", () => { if (fresh.trusted(app)) start(); });
  fresh.on(app, "workspace.trusted", start);

  // ---------------------------------------------------------------- gutter

  // A whole buffer's lines, without the empty one after a final newline.
  /** const held: (HeldLines) => String[] */
  const held = (L) => (L.lines[L.lines.length - 1] === "" ? L.lines.slice(0, -1) : L.lines);
  // The file at HEAD: its lines, not in HEAD, or git could not say (not
  // kept: asked again next time).
  /** const baseline: (String) => Promise<Baseline> */
  const baseline = async (path) => {
    for (const known of found(baselines.get(path))) return known;
    if (!fresh.trusted(app)) return unknownBase("the workspace is restricted");
    const there = await git(["ls-tree", "--name-only", "HEAD", "--", `./${rel(path)}`]);
    if (there.code !== 0) return unknownBase(there.stderr.trim());
    /** let b: Baseline */
    let b = UNTRACKED;
    if (there.stdout.trim() !== "") {
      const r = await git(["show", `HEAD:./${rel(path)}`]);
      if (r.code !== 0) return unknownBase(r.stderr.trim());
      b = baseLines(r.stdout.replace(/\n$/, "").split("\n"));
    }
    baselines.set(path, b);
    return b;
  };

  const timers = new Map();
  const changes = new Map(); // buffer id -> changed window line indices
  /** const markGutter: (BufferHandle) => Promise<Undefined> */
  const markGutter = async (b) => {
    if (b.isVirtual || !b.whole() || rel(b.path()) === null) return;
    const base = await baseline(b.path());
    const L = b.lines();
    const d = b.decorations("gutter");
    d.clear();
    if (base.kind === "unknown" && !told.has(b.path())) { told.add(b.path()); fresh.message(app, `git: no changes shown for ${rel(b.path())}: ${base.why}`); }
    if (base.kind !== "lines" || L === null) { b.render(); return; }
    const edits = diffLines(base.lines, held(L));
    if (!edits) { b.render(); return; }
    // An inserted line where a line was also deleted is a modified line.
    const added = new Set(), deletedAt = new Set();
    for (const e of edits) (e.kind === "-" ? deletedAt : added).add(e.b);
    for (const i of added) {
      const mod = deletedAt.has(i);
      d.gutter(L.starts[i], { glyph: "▎", color: mod ? "git.modified" : "git.added", title: mod ? "modified" : "added" });
    }
    for (const i of deletedAt) if (!added.has(i) && i < L.starts.length) d.gutter(L.starts[i], { glyph: "▁", color: "git.deleted", title: "lines deleted above" });
    changes.set(b.id, [...new Set([...added, ...deletedAt])].sort((x, y) => x - y));
    b.render();
  };
  /** const schedule: (BufferHandle) => Undefined */
  const schedule = (b) => {
    const before = timers.get(b.id);
    if (before !== undefined) clearTimeout(before);
    timers.set(b.id, setTimeout(() => { markGutter(b); }, 300));
  };
  fresh.onBuffer(app, "buffer.activated", (buffer) => schedule(buffer));
  fresh.onBuffer(app, "buffer.changed", (buffer) => schedule(buffer));
  fresh.onBuffer(app, "buffer.saved", (buffer) => schedule(buffer));

  /** const step: (Number) => Undefined */
  const step = (dir) => {
    const v = fresh.activeView(app);
    if (v === null) return;
    const b = v.buffer;
    const L = b.lines();
    const list = changes.get(b.id) ?? [];
    if (L === null || !list.length) { fresh.message(app, "No changes"); return; }
    const cur = L.starts.findIndex((s, k) => v.cursor() >= s && (k + 1 >= L.starts.length || v.cursor() < L.starts[k + 1]));
    const next = dir > 0 ? list.find((i) => i > cur) ?? list[0] : [...list].reverse().find((i) => i < cur) ?? list[list.length - 1];
    v.setCursor(L.starts[next], { center: true });
  };

  // ---------------------------------------------------------------- blame

  const blamed = new Set();
  const blame = async () => {
    const b = fresh.activeBuffer(app);
    if (b === null) return;
    const file = rel(b.path());
    if (file === null) return;
    const d = b.decorations("blame");
    if (blamed.has(b.id)) { blamed.delete(b.id); d.clear(); b.render(); return; }
    const r = await git(["blame", "--line-porcelain", "--", file]);
    if (r.code !== 0) { fresh.message(app, r.stderr.trim() || "Not tracked by git"); return; }
    const L = b.lines();
    if (L === null || !b.whole()) { fresh.message(app, "Blame needs the whole file"); return; }
    // Blame describes the file on disk; the buffer's lines map onto it.
    const disk = (await fresh.read(app, b.path())).replace(/\n$/, "").split("\n");
    const map = mapLines(disk, held(L));
    const info = parseBlame(r.stdout);
    /** const byLine: Map<Number, Blamed> */
    const byLine = new Map();
    for (const x of info) byLine.set(x.line, x);
    d.clear();
    held(L).forEach((text, i) => {
      const x = map ? byLine.get(map[i]) : byLine.get(i);
      const note = x === undefined || /^0+$/.test(x.hash) ? " (not committed)" : ` ${x.author}, ${x.time} · ${x.summary}`;
      d.inline(L.starts[i] + fresh.byteLen(text), note, { style: BLAME, side: "before", edge: true, when: "always" });
    });
    blamed.add(b.id);
    b.render();
  };

  // ---------------------------------------------------------------- log and diffs

  const showDiff = (name, text) => fresh.createVirtual(app, {
    name, entries: [{ text, props: {}, style: null }],
    mode: null, readOnly: true, language: "diff", keep: false, place: "tab", lineNumbers: false,
  });

  // The log of the workspace, or of one file (`path`).
  /** const log: (String | Null) => Promise<Undefined> */
  const log = async (path) => {
    const of = path === null ? null : rel(path);
    const format = ["log", "-n", "500", "--format=%H%x1f%h%x1f%an%x1f%ar%x1f%s"];
    const r = await git(of ? [...format, "--", of] : format);
    if (r.code !== 0) { fresh.message(app, r.stderr.trim() || "git log failed"); return; }
    const items = r.stdout.trim().split("\n").filter(Boolean).map((l) => { const [hash, short, author, when, subject] = l.split("\x1f"); return { hash, short, author, when, subject }; });
    const logPanel = fresh.panel(app, { id: "git-log", title: of ? `Log: ${of}` : "Git Log", place: "bottom", onClose: null });
    logPanel.render(ui.list({
      ...ui.listProps("log", items, (c) => ui.row(ui.BOX, [ui.text({ ...ui.TEXT, color: ["ui.accent_fg"], mono: true }, c.short), ui.text(ui.TEXT, c.subject), ui.spacer(), ui.text({ ...ui.TEXT, dim: true }, `${c.author}, ${c.when}`)])),
      onActivate: async (c) => {
        const s = await git(["show", "--stat", "-p", "--format=commit %H%nAuthor: %an <%ae>%nDate:   %ad%n%n    %s%n%n%b", c.hash]);
        showDiff(`${c.short} ${c.subject}`.slice(0, 40), s.stdout);
      },
    }));
    logPanel.focus("log");
  };

  // The buffer as it is against HEAD, so unsaved edits show too.
  /** const diffFile: () => Promise<Undefined> */
  const diffFile = async () => {
    const b = fresh.activeBuffer(app);
    if (b === null) return;
    const L = b.lines();
    const file = rel(b.path());
    if (file === null || L === null) return;
    if (!b.whole()) { fresh.message(app, "Diff needs the whole file"); return; }
    const base = await baseline(b.path());
    if (base.kind === "untracked") { fresh.message(app, "Not in HEAD"); return; }
    if (base.kind === "unknown") { fresh.message(app, `git: ${base.why}`); return; }
    const text = unifiedDiff(base.lines, held(L), `a/${file}`, `b/${file}`);
    if (text === null) { fresh.message(app, "Too many changes to diff here"); return; }
    if (!text) { fresh.message(app, "No changes against HEAD"); return; }
    showDiff(`diff ${b.name()}`, text);
  };

  // A command, and its key ("" for none).
  /** const reg: (String, String, () => Undefined, String) => Undefined */
  const reg = (id, title, run, key) => { fresh.register(app, id, { title, hidden: false, run }); if (key) fresh.bindKey(app, key, id); };
  reg("git.log", "Git: Log", () => { log(null); }, "C-S-g");
  reg("git.logFile", "Git: Log of Current File", () => { const b = fresh.activeBuffer(app); log(b === null ? null : b.path()); }, "");
  reg("git.blame", "Git: Toggle Blame", () => { blame(); }, "");
  reg("git.diffFile", "Git: Diff Current File", () => { diffFile(); }, "");
  reg("git.nextChange", "Git: Next Change", () => { step(1); }, "M-F5");
  reg("git.prevChange", "Git: Previous Change", () => { step(-1); }, "M-S-F5");
  reg("git.refresh", "Git: Refresh", () => { baselines.clear(); const b = fresh.activeBuffer(app); if (b) markGutter(b); refreshBranch(); }, "");
}
