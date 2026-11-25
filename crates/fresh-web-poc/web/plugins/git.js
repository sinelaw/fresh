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
import { h } from "../js/ui.js";

// A line's blame, at the row's end.
/** const BLAME: Style */
const BLAME = { fg: "editor.line_number_fg", bg: null, bold: false, italic: true, underline: false, strike: false, mono: false, extendToLineEnd: false };

export function activate(app) {
  const root = fresh.root(app);
  const rel = (p) => (p && p.startsWith(root + "/") ? p.slice(root.length + 1) : null);
  // A restricted workspace runs no processes: git answers as a failure.
  const git = (args) => fresh.trusted(app)
    ? fresh.runProcess(app, "git", args, { cwd: root })
    : Promise.resolve({ code: 1, stdout: "", stderr: "The workspace is restricted: trust it to use git" });
  const baselines = new Map(); // path -> lines at HEAD (null: not tracked)

  // ---------------------------------------------------------------- branch

  const branch = fresh.statusItem(app, "git-branch");
  const refreshBranch = async () => {
    baselines.clear(); // HEAD moved: every baseline may have
    const r = await git(["rev-parse", "--abbrev-ref", "HEAD"]);
    branch.set(r.code === 0 ? `⎇ ${r.stdout.trim()}` : "", { title: "Git branch — click for the log", onClick: () => fresh.run(app, "git.log") });
  };
  // HEAD changes on a checkout; the branch's ref file on a commit.
  let refWatch = null;
  const watchRefs = async () => {
    const ref = await git(["symbolic-ref", "-q", "HEAD"]);
    if (refWatch) { refWatch(); refWatch = null; }
    if (ref.code === 0) {
      const p = await git(["rev-parse", "--git-path", ref.stdout.trim()]);
      if (p.code === 0) refWatch = await fresh.watch(app, `${root}/${p.stdout.trim()}`, () => { refreshBranch(); const b = fresh.activeBuffer(app); if (b) schedule(b); });
    }
  };
  const start = async () => {
    await refreshBranch();
    const head = await git(["rev-parse", "--git-path", "HEAD"]);
    if (head.code === 0) fresh.watch(app, `${root}/${head.stdout.trim()}`, async () => { await refreshBranch(); await watchRefs(); });
    await watchRefs();
    const b = fresh.activeBuffer(app);
    if (b) schedule(b);
  };
  fresh.on(app, "app.ready", () => { if (fresh.trusted(app)) start(); });
  fresh.on(app, "workspace.trusted", start);

  // ---------------------------------------------------------------- gutter

  // A whole buffer's lines, without the empty one after a final newline.
  const held = (L) => (L.lines[L.lines.length - 1] === "" ? L.lines.slice(0, -1) : L.lines);
  const baseline = async (path) => {
    if (!baselines.has(path)) {
      const r = await git(["show", `HEAD:./${rel(path)}`]);
      baselines.set(path, r.code === 0 ? r.stdout.replace(/\n$/, "").split("\n") : null);
    }
    return baselines.get(path);
  };

  const timers = new Map();
  const changes = new Map(); // buffer id -> changed window line indices
  const markGutter = async (b) => {
    if (!b || b.isVirtual || !b.whole || !rel(b.path)) return;
    const base = await baseline(b.path);
    const L = b.lines();
    const d = b.decorations("gutter");
    d.clear();
    if (!base || !L) { b.render(); return; }
    const edits = diffLines(base, held(L));
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
  const schedule = (b) => {
    clearTimeout(timers.get(b.id));
    timers.set(b.id, setTimeout(() => markGutter(b), 300));
  };
  fresh.onBuffer(app, "buffer.activated", (buffer) => schedule(buffer));
  fresh.onBuffer(app, "buffer.changed", (buffer) => schedule(buffer));
  fresh.onBuffer(app, "buffer.saved", (buffer) => schedule(buffer));

  const step = (dir) => {
    const v = fresh.activeView(app);
    const b = v?.buffer;
    const L = b?.lines();
    const list = b && changes.get(b.id);
    if (!L || !list || !list.length) return fresh.message(app, "No changes");
    const cur = L.starts.findIndex((s, k) => v.cursor >= s && (k + 1 >= L.starts.length || v.cursor < L.starts[k + 1]));
    const next = dir > 0 ? list.find((i) => i > cur) ?? list[0] : [...list].reverse().find((i) => i < cur) ?? list[list.length - 1];
    v.setCursor(L.starts[next], { center: true });
  };

  // ---------------------------------------------------------------- blame

  const blamed = new Set();
  const blame = async () => {
    const b = fresh.activeBuffer(app);
    if (!b || !rel(b.path)) return;
    const d = b.decorations("blame");
    if (blamed.has(b.id)) { blamed.delete(b.id); d.clear(); b.render(); return; }
    const r = await git(["blame", "--line-porcelain", "--", rel(b.path)]);
    if (r.code !== 0) return fresh.message(app, r.stderr.trim() || "Not tracked by git");
    const L = b.lines();
    if (!L || !b.whole) return fresh.message(app, "Blame needs the whole file");
    // Blame describes the file on disk; the buffer's lines map onto it.
    const disk = (await fresh.read(app, b.path)).replace(/\n$/, "").split("\n");
    const map = mapLines(disk, held(L));
    const info = [];
    let cur = null;
    for (const line of r.stdout.split("\n")) {
      const head = /^([0-9a-f]{40}) \d+ (\d+)/.exec(line);
      if (head) { cur = { hash: head[1], line: +head[2] - 1, author: "", time: "", summary: "" }; continue; }
      if (!cur) continue;
      if (line.startsWith("author ")) cur.author = line.slice(7);
      else if (line.startsWith("author-time ")) cur.time = new Date(+line.slice(12) * 1000).toISOString().slice(0, 10);
      else if (line.startsWith("summary ")) cur.summary = line.slice(8);
      else if (line.startsWith("\t")) { info.push(cur); cur = null; }
    }
    const byLine = new Map(info.map((x) => [x.line, x]));
    d.clear();
    held(L).forEach((text, i) => {
      const x = map ? byLine.get(map[i]) : byLine.get(i);
      const committed = x && !/^0+$/.test(x.hash);
      d.inline(L.starts[i] + fresh.byteLen(text), committed ? ` ${x.author}, ${x.time} · ${x.summary}` : " (not committed)", { style: BLAME, side: "before", edge: true, when: "always" });
    });
    blamed.add(b.id);
    b.render();
  };

  // ---------------------------------------------------------------- log and diffs

  const showDiff = (name, text) => fresh.createVirtual(app, {
    name, entries: [{ text, props: {}, style: null }],
    mode: null, readOnly: true, language: "diff", keep: false, place: "tab", lineNumbers: false,
  });

  let logPanel = null;
  const log = async (path) => {
    const args = ["log", "-n", "500", "--format=%H%x1f%h%x1f%an%x1f%ar%x1f%s"];
    if (path) args.push("--", rel(path));
    const r = await git(args);
    if (r.code !== 0) return fresh.message(app, r.stderr.trim() || "git log failed");
    const items = r.stdout.trim().split("\n").filter(Boolean).map((l) => { const [hash, short, author, when, subject] = l.split("\x1f"); return { hash, short, author, when, subject }; });
    logPanel = fresh.panel(app, { id: "git-log", title: path ? `Log: ${rel(path)}` : "Git Log", place: "bottom", onClose: null });
    logPanel.render(h("list", {
      key: "log",
      items,
      render: (c) => h("row", {}, [h("text", { color: "ui.accent_fg", mono: true }, [c.short]), h("text", {}, [c.subject]), h("spacer", {}), h("text", { dim: true }, [`${c.author}, ${c.when}`])]),
      onActivate: async (c) => {
        const s = await git(["show", "--stat", "-p", "--format=commit %H%nAuthor: %an <%ae>%nDate:   %ad%n%n    %s%n%n%b", c.hash]);
        showDiff(`${c.short} ${c.subject}`.slice(0, 40), s.stdout);
      },
    }));
    logPanel.focus("log");
  };

  // The buffer as it is against HEAD, so unsaved edits show too.
  const diffFile = async () => {
    const b = fresh.activeBuffer(app);
    const L = b?.lines();
    if (!b || !rel(b.path) || !L) return;
    if (!b.whole) return fresh.message(app, "Diff needs the whole file");
    const base = await baseline(b.path);
    if (!base) return fresh.message(app, "Not in HEAD");
    const text = unifiedDiff(base, held(L), `a/${rel(b.path)}`, `b/${rel(b.path)}`);
    if (text === null) return fresh.message(app, "Too many changes to diff here");
    if (!text) return fresh.message(app, "No changes against HEAD");
    showDiff(`diff ${b.name}`, text);
  };

  // A command, and its key ("" for none).
  const reg = (id, title, run, key) => { fresh.register(app, id, { title, hidden: false, run }); if (key) fresh.bindKey(app, key, id); };
  reg("git.log", "Git: Log", () => log(null), "C-S-g");
  reg("git.logFile", "Git: Log of Current File", () => log(fresh.activeBuffer(app)?.path), "");
  reg("git.blame", "Git: Toggle Blame", blame, "");
  reg("git.diffFile", "Git: Diff Current File", diffFile, "");
  reg("git.nextChange", "Git: Next Change", () => step(1), "M-F5");
  reg("git.prevChange", "Git: Previous Change", () => step(-1), "M-S-F5");
  reg("git.refresh", "Git: Refresh", () => { baselines.clear(); const b = fresh.activeBuffer(app); if (b) markGutter(b); refreshBranch(); }, "");
}
