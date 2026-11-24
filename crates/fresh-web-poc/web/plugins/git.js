// Git, through the workspace host's own `git` (so it is the remote's git
// for a remote workspace):
//   * the branch in the status bar, refreshed when HEAD changes;
//   * a gutter of added / modified / deleted lines, from a diff of the
//     buffer against HEAD done here (so it follows unsaved edits);
//   * next / previous change;
//   * blame, as text at the end of each line;
//   * the log in a panel; a commit's diff in a buffer of its own;
//   * the diff of the current file.

import { diffLines } from "../js/diff.js";

export function activate(fresh) {
  const { h } = fresh;
  const root = fresh.workspace.root;
  const rel = (p) => (p && p.startsWith(root + "/") ? p.slice(root.length + 1) : null);
  const git = (args) => fresh.host.run("git", args, { cwd: root });
  const baselines = new Map(); // path -> lines at HEAD (null: not tracked)

  // ---------------------------------------------------------------- branch

  const branch = fresh.ui.status("git-branch");
  const refreshBranch = async () => {
    baselines.clear(); // HEAD moved: every baseline may have
    const r = await git(["rev-parse", "--abbrev-ref", "HEAD"]);
    branch.set(r.code === 0 ? `⎇ ${r.stdout.trim()}` : "", { title: "Git branch — click for the log", onClick: () => fresh.commands.run("git.log") });
  };
  // HEAD changes on a checkout; the branch's ref file on a commit.
  let refWatch = null;
  const watchRefs = async () => {
    const ref = await git(["symbolic-ref", "-q", "HEAD"]);
    if (refWatch) { refWatch(); refWatch = null; }
    if (ref.code === 0) {
      const p = await git(["rev-parse", "--git-path", ref.stdout.trim()]);
      if (p.code === 0) refWatch = await fresh.host.watch(`${root}/${p.stdout.trim()}`, () => { refreshBranch(); const b = fresh.buffers.active(); if (b) schedule(b); });
    }
  };
  fresh.events.on("app.ready", async () => {
    await refreshBranch();
    const head = await git(["rev-parse", "--git-path", "HEAD"]);
    if (head.code === 0) fresh.host.watch(`${root}/${head.stdout.trim()}`, async () => { await refreshBranch(); await watchRefs(); });
    await watchRefs();
  });

  // ---------------------------------------------------------------- gutter

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
    const cur = L.lines[L.lines.length - 1] === "" ? L.lines.slice(0, -1) : L.lines;
    const edits = diffLines(base, cur);
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
  fresh.events.on("buffer.activated", ({ buffer }) => schedule(buffer));
  fresh.events.on("buffer.changed", ({ buffer }) => schedule(buffer));
  fresh.events.on("buffer.saved", ({ buffer }) => schedule(buffer));

  const step = (dir) => {
    const v = fresh.views.active();
    const b = v?.buffer;
    const L = b?.lines();
    const list = b && changes.get(b.id);
    if (!L || !list || !list.length) return fresh.ui.message("No changes");
    const cur = L.starts.findIndex((s, k) => v.cursor >= s && (k + 1 >= L.starts.length || v.cursor < L.starts[k + 1]));
    const next = dir > 0 ? list.find((i) => i > cur) ?? list[0] : [...list].reverse().find((i) => i < cur) ?? list[list.length - 1];
    v.setCursor(L.starts[next], { center: true });
  };

  // ---------------------------------------------------------------- blame

  const blamed = new Set();
  const blame = async () => {
    const b = fresh.buffers.active();
    if (!b || !rel(b.path)) return;
    const d = b.decorations("blame");
    if (blamed.has(b.id)) { blamed.delete(b.id); d.clear(); b.render(); return; }
    const r = await git(["blame", "--line-porcelain", "--", rel(b.path)]);
    if (r.code !== 0) return fresh.ui.message(r.stderr.trim() || "Not tracked by git");
    const L = b.lines();
    const info = [];
    let cur = null;
    for (const line of r.stdout.split("\n")) {
      const head = /^([0-9a-f]{40}) \d+ (\d+)/.exec(line);
      if (head) { cur = { hash: head[1], line: +head[2] - 1 }; continue; }
      if (!cur) continue;
      if (line.startsWith("author ")) cur.author = line.slice(7);
      else if (line.startsWith("author-time ")) cur.time = new Date(+line.slice(12) * 1000).toISOString().slice(0, 10);
      else if (line.startsWith("summary ")) cur.summary = line.slice(8);
      else if (line.startsWith("\t")) { info.push(cur); cur = null; }
    }
    d.clear();
    for (const x of info) {
      const i = x.line - (L.firstLine ?? 0);
      if (i < 0 || i >= L.starts.length) continue;
      const committed = !/^0+$/.test(x.hash);
      d.inline(L.starts[i] + fresh.text.byteLen(L.lines[i]), committed ? ` ${x.author}, ${x.time} · ${x.summary}` : " (not committed)", { edge: true, style: { fg: "editor.line_number_fg", italic: true } });
    }
    blamed.add(b.id);
    b.render();
  };

  // ---------------------------------------------------------------- log and diffs

  const showDiff = (name, text) => fresh.buffers.createVirtual({ name, entries: [{ text }], language: "diff", place: "tab", lineNumbers: false });

  let logPanel = null;
  const log = async (path) => {
    const args = ["log", "-n", "500", "--format=%H%x1f%h%x1f%an%x1f%ar%x1f%s"];
    if (path) args.push("--", rel(path));
    const r = await git(args);
    if (r.code !== 0) return fresh.ui.message(r.stderr.trim() || "git log failed");
    const items = r.stdout.trim().split("\n").filter(Boolean).map((l) => { const [hash, short, author, when, subject] = l.split("\x1f"); return { hash, short, author, when, subject }; });
    logPanel = fresh.ui.panel({ id: "git-log", title: path ? `Log: ${rel(path)}` : "Git Log", place: "bottom" });
    logPanel.render(h("list", {
      key: "log",
      items,
      render: (c) => h("row", {}, h("text", { color: "#d7ba7d", mono: true }, c.short), h("text", {}, c.subject), h("spacer", {}), h("text", { dim: true }, `${c.author}, ${c.when}`)),
      onActivate: async (c) => {
        const s = await git(["show", "--stat", "-p", "--format=commit %H%nAuthor: %an <%ae>%nDate:   %ad%n%n    %s%n%n%b", c.hash]);
        showDiff(`${c.short} ${c.subject}`.slice(0, 40), s.stdout);
      },
    }));
    logPanel.focus("log");
  };

  const diffFile = async () => {
    const b = fresh.buffers.active();
    if (!b || !rel(b.path)) return;
    const r = await git(["diff", "HEAD", "--", rel(b.path)]);
    if (!r.stdout.trim()) return fresh.ui.message("No changes against HEAD (saved file)");
    showDiff(`diff ${b.name}`, r.stdout);
  };

  const reg = (id, title, run, key) => { fresh.commands.register(id, { title, run }); if (key) fresh.keys.bind(key, id); };
  reg("git.log", "Git: Log", () => log(null), "C-S-g");
  reg("git.logFile", "Git: Log of Current File", () => log(fresh.buffers.active()?.path));
  reg("git.blame", "Git: Toggle Blame", blame);
  reg("git.diffFile", "Git: Diff Current File", diffFile);
  reg("git.nextChange", "Git: Next Change", () => step(1), "M-F5");
  reg("git.prevChange", "Git: Previous Change", () => step(-1), "S-M-F5");
  reg("git.refresh", "Git: Refresh", () => { baselines.clear(); const b = fresh.buffers.active(); if (b) markGutter(b); refreshBranch(); });
}
